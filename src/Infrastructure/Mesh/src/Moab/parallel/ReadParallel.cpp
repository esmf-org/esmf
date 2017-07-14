#include "ReadParallel.hpp"
#include "moab/Core.hpp"
#include "moab/ProcConfig.hpp"
#include "moab/FileOptions.hpp"
#include "moab/Error.hpp"
#include "moab/ReaderWriterSet.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/ParallelComm.hpp"
#include "MBParallelConventions.h"

#include <iostream>
#include <iomanip>
#include <iterator>
#include <sstream>
#include <algorithm>
#include <assert.h>

namespace moab {

const bool debug = false;

const char *ReadParallel::ParallelActionsNames[] = {
    "PARALLEL READ",
    "PARALLEL READ PART",
    "PARALLEL BROADCAST", 
    "PARALLEL DELETE NONLOCAL",
    "PARALLEL CHECK_GIDS_SERIAL",
    "PARALLEL GET_FILESET_ENTS",
    "PARALLEL RESOLVE_SHARED_ENTS",
    "PARALLEL EXCHANGE_GHOSTS",
    "PARALLEL RESOLVE_SHARED_SETS",
    "PARALLEL_AUGMENT_SETS_WITH_GHOSTS",
    "PARALLEL PRINT_PARALLEL"
};

const char* ReadParallel::parallelOptsNames[] = { "NONE",
                                                  "BCAST",
                                                  "BCAST_DELETE",
                                                  "READ_DELETE",
                                                  "READ_PART",
                                                  "",
                                                  0 };

ReadParallel::ReadParallel(Interface* impl,
                           ParallelComm *pc) 
  : mbImpl(impl), myPcomm(pc), myDebug("ReadPara", std::cerr)
{
  if (!myPcomm) {
    myPcomm = ParallelComm::get_pcomm(mbImpl, 0);
    if (NULL == myPcomm)
      myPcomm = new ParallelComm(mbImpl, MPI_COMM_WORLD);
  }
  myDebug.set_rank(myPcomm->proc_config().proc_rank());
  if (debug) // For backwards compatibility, enable all debug output if constant is true
    myDebug.set_verbosity(10);

  impl->query_interface(mError);
}

ErrorCode ReadParallel::load_file(const char **file_names,
                                  const int num_files,
                                  const EntityHandle* file_set,
                                  const FileOptions &opts,
                                  const ReaderIface::SubsetList* subset_list,
                                  const Tag* file_id_tag)
{
  int tmpval;
  if (MB_SUCCESS == opts.get_int_option("DEBUG_PIO", 1, tmpval)) {
    myDebug.set_verbosity(tmpval);
    myPcomm->set_debug_verbosity(tmpval);
  }
  myDebug.tprint(1, "Setting up...\n");

  // Get parallel settings
  int parallel_mode;
  ErrorCode result = opts.match_option("PARALLEL", parallelOptsNames, parallel_mode);
  if (MB_FAILURE == result) {
    MB_SET_ERR(MB_FAILURE, "Unexpected value for 'PARALLEL' option");
  }
  else if (MB_ENTITY_NOT_FOUND == result) {
    parallel_mode = 0;
  }

  // Get partition setting
  bool distrib;
  std::string partition_tag_name;
  result = opts.get_option("PARTITION", partition_tag_name);
  if (MB_ENTITY_NOT_FOUND == result) {
    distrib = false;
    partition_tag_name = "";
  }
  else {
    distrib = true;
    if (partition_tag_name.empty()) 
      partition_tag_name = PARALLEL_PARTITION_TAG_NAME;

    // Also get deprecated PARTITION_DISTRIBUTE option
    // so that higher-level code doesn't return an error
    // due to an unrecognized option
    opts.get_null_option("PARTITION_DISTRIBUTE");
  }

  // Get partition tag value(s), if any, and whether they're to be
  // distributed or assigned
  std::vector<int> partition_tag_vals;
  opts.get_ints_option("PARTITION_VAL", partition_tag_vals);

  // See if we need to report times
  bool cputime = false;
  result = opts.get_null_option("CPUTIME");
  if (MB_SUCCESS == result)
    cputime = true;

  // See if we need to report times
  bool print_parallel = false;
  result = opts.get_null_option("PRINT_PARALLEL");
  if (MB_SUCCESS == result)
    print_parallel = true;

  // Get ghosting options
  std::string ghost_str;
  int bridge_dim, ghost_dim = -1, num_layers, addl_ents = 0;
  result = opts.get_str_option("PARALLEL_GHOSTS", ghost_str);
  if (MB_TYPE_OUT_OF_RANGE == result) {
    ghost_dim = 3;
    bridge_dim = 0;
    num_layers = 1;
  }
  else if (MB_SUCCESS == result) {
    int num_fields = sscanf(ghost_str.c_str(), "%d.%d.%d.%d", &ghost_dim, &bridge_dim, &num_layers, &addl_ents);
    if (3 > num_fields) {
      MB_SET_ERR(MB_FAILURE, "Didn't read 3 fields from PARALLEL_GHOSTS string");
    }
  }

  // Get resolve_shared_ents option
  std::string shared_str;
  int resolve_dim = -2, shared_dim = -1;
  result = opts.get_str_option("PARALLEL_RESOLVE_SHARED_ENTS", shared_str);
  if (MB_TYPE_OUT_OF_RANGE == result) {
    resolve_dim = -1;
    shared_dim = -1;
  }
  else if (MB_SUCCESS == result) {
    int num_fields = sscanf(shared_str.c_str(), "%d.%d", &resolve_dim, &shared_dim);
    if (2 != num_fields) {
      MB_SET_ERR(MB_FAILURE, "Didn't read 2 fields from PARALLEL_RESOLVE_SHARED_ENTS string");
    }
  }

  // Get skip augmenting with ghosts option
  bool skip_augment = false;
  result = opts.get_null_option("SKIP_AUGMENT_WITH_GHOSTS");
  if (MB_SUCCESS == result)
    skip_augment = true;

  // Get MPI IO processor rank
  int reader_rank;
  result = opts.get_int_option("MPI_IO_RANK", reader_rank);
  if (MB_ENTITY_NOT_FOUND == result)
    reader_rank = 0;
  else if (MB_SUCCESS != result) {
    MB_SET_ERR(MB_FAILURE, "Unexpected value for 'MPI_IO_RANK' option");
  }

  // Now that we've parsed all the parallel options, make an instruction queue
  std::vector<int> pa_vec;
  bool is_reader = (reader_rank == (int) myPcomm->proc_config().proc_rank());

  bool partition_by_rank = false;
  if (MB_SUCCESS == opts.get_null_option("PARTITION_BY_RANK")) {
    partition_by_rank = true;
    if (!partition_tag_vals.empty()) {
      MB_SET_ERR(MB_FAILURE, "Cannot specify both PARTITION_VALS and PARTITION_BY_RANK");
    }
  }

  double factor_seq;
  if (MB_SUCCESS == opts.get_real_option( "PARALLEL_SEQUENCE_FACTOR", factor_seq ) )
  {
    if (factor_seq<1.)
      MB_SET_ERR(MB_FAILURE, "cannot have sequence factor less than 1.");
    mbImpl->set_sequence_multiplier(factor_seq);
  }
  switch (parallel_mode) {
    case POPT_BCAST:
        myDebug.print(1, "Read mode is BCAST\n");
        if (is_reader) {
          pa_vec.push_back(PA_READ);
          pa_vec.push_back(PA_CHECK_GIDS_SERIAL);
          pa_vec.push_back(PA_GET_FILESET_ENTS);
        }
        pa_vec.push_back(PA_BROADCAST);
        if (!is_reader)
          pa_vec.push_back(PA_GET_FILESET_ENTS);
        break;

    case POPT_BCAST_DELETE:
        myDebug.print(1, "Read mode is BCAST_DELETE\n");
        if (is_reader) {
          pa_vec.push_back(PA_READ);
          pa_vec.push_back(PA_CHECK_GIDS_SERIAL);
          pa_vec.push_back(PA_GET_FILESET_ENTS);
        }
        pa_vec.push_back(PA_BROADCAST);
        if (!is_reader)
          pa_vec.push_back(PA_GET_FILESET_ENTS);
        pa_vec.push_back(PA_DELETE_NONLOCAL);
        break;

    case POPT_DEFAULT:
    case POPT_READ_DELETE:
        myDebug.print(1, "Read mode is READ_DELETE\n");
        pa_vec.push_back(PA_READ);
        pa_vec.push_back(PA_CHECK_GIDS_SERIAL);
        pa_vec.push_back(PA_GET_FILESET_ENTS);
        pa_vec.push_back(PA_DELETE_NONLOCAL);
        break;

    case POPT_READ_PART:
        myDebug.print(1, "Read mode is READ_PART\n");
        pa_vec.push_back(PA_READ_PART);
        break;

    default:
        MB_SET_ERR(MB_FAILURE, "Unexpected parallel read mode");
  }

  if (-2 != resolve_dim)
    pa_vec.push_back(PA_RESOLVE_SHARED_ENTS);

  if (-1 != ghost_dim)
    pa_vec.push_back(PA_EXCHANGE_GHOSTS);

  if (-2 != resolve_dim) {
    pa_vec.push_back(PA_RESOLVE_SHARED_SETS);
    if (-1 != ghost_dim && !skip_augment)
      pa_vec.push_back(PA_AUGMENT_SETS_WITH_GHOSTS);
  }

  if (print_parallel)
    pa_vec.push_back(PA_PRINT_PARALLEL);

  result = load_file(file_names, num_files, file_set, parallel_mode, 
                     partition_tag_name,
                     partition_tag_vals, distrib,
                     partition_by_rank, pa_vec, opts,
                     subset_list, file_id_tag,
                     reader_rank, cputime,
                     resolve_dim, shared_dim,
                     ghost_dim, bridge_dim, num_layers, addl_ents);MB_CHK_ERR(result);

  if (parallel_mode == POPT_BCAST_DELETE && !is_reader)
    opts.mark_all_seen();

  return MB_SUCCESS;
}
    
ErrorCode ReadParallel::load_file(const char **file_names,
                                  const int num_files,
                                  const EntityHandle* file_set_ptr,
                                  int /*parallel_mode*/, 
                                  std::string &partition_tag_name,
                                  std::vector<int> &partition_tag_vals,
                                  bool distrib,
                                  bool partition_by_rank,
                                  std::vector<int> &pa_vec,
                                  const FileOptions &opts,
                                  const ReaderIface::SubsetList* subset_list,
                                  const Tag* file_id_tag,
                                  const int reader_rank,
                                  const bool cputime,
                                  const int resolve_dim,
                                  const int shared_dim,
                                  const int ghost_dim,
                                  const int bridge_dim,
                                  const int num_layers,
                                  const int addl_ents) 
{
  ErrorCode result = MB_SUCCESS;
  if (myPcomm == NULL)
    myPcomm = new ParallelComm(mbImpl, MPI_COMM_WORLD);

  Range entities; 
  Tag file_set_tag = 0;
  int other_sets = 0;
  ReaderWriterSet::iterator iter;
  Range other_file_sets, file_sets;
  Core *impl = dynamic_cast<Core*>(mbImpl);

  std::vector<double> act_times(pa_vec.size()+1);
  std::vector<int>::iterator vit;
  int i, j;
  act_times[0] = MPI_Wtime();

  // Make a new set for the parallel read
  EntityHandle file_set;
  if (!file_set_ptr || !(*file_set_ptr)) {
    result = mbImpl->create_meshset(MESHSET_SET, file_set);MB_CHK_SET_ERR(result, "Trouble creating file set");
  }
  else
    file_set = *file_set_ptr;

  bool i_read = false;
  Tag id_tag = 0;
  bool use_id_tag = false;
  Range ents;

  for (i = 1, vit = pa_vec.begin(); vit != pa_vec.end(); ++vit, i++) {
    ErrorCode tmp_result = MB_SUCCESS;
    switch (*vit) {
//==================
      case PA_READ:
          i_read = true;

          for (j = 0; j < num_files; j++) {
            myDebug.tprintf(1, "Reading file: \"%s\"\n", file_names[j]);

            EntityHandle new_file_set;
            result = mbImpl->create_meshset(MESHSET_SET, new_file_set);MB_CHK_SET_ERR(result, "Trouble creating file set");
            tmp_result = impl->serial_load_file(file_names[j],
                                                &new_file_set,
                                                opts,
                                                subset_list,
                                                file_id_tag);
            if (MB_SUCCESS != tmp_result)
              break;

            // Put the contents of each file set for the reader into the
            // file set for the parallel read
            assert(0 != new_file_set);
            Range all_ents;
            tmp_result = mbImpl->get_entities_by_handle(new_file_set, all_ents);
            if (MB_SUCCESS != tmp_result)
              break;
            all_ents.insert(new_file_set);
            tmp_result = mbImpl->add_entities(file_set, all_ents);
            if (MB_SUCCESS != tmp_result)
              break;
          }
          if (MB_SUCCESS != tmp_result)
            break;

          // Mark the file set for this parallel reader
          tmp_result = mbImpl->tag_get_handle("__file_set", 1, MB_TYPE_INTEGER,
                                              file_set_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
          if (MB_SUCCESS != tmp_result)
            break;

          tmp_result = mbImpl->tag_set_data(file_set_tag, &file_set, 1, 
                                            &other_sets);
          break;

//==================
      case PA_READ_PART: {
          myDebug.tprintf(1, "Reading file: \"%s\"\n", file_names[0]);

          i_read = true;
          if (num_files != 1) {
            MB_SET_ERR(MB_NOT_IMPLEMENTED, "Multiple file read not supported for READ_PART");
          }

          // If we're going to resolve shared entities, then we need
          // to ask the file reader to populate a tag with unique ids
          // (typically file ids/indices/whatever.)
          if (std::find(pa_vec.begin(), pa_vec.end(), PA_RESOLVE_SHARED_ENTS)
              != pa_vec.end()) {
            use_id_tag = true;
            if (!file_id_tag) {
              // This tag is really used for resolving shared entities with crystal router
              // In the end, this is an identifier that gets converted to long
              // In hdf5 file reader, we also convert from hdf5 file id type to long
              tmp_result = mbImpl->tag_get_handle("", sizeof(long), MB_TYPE_OPAQUE, id_tag, MB_TAG_DENSE | MB_TAG_CREAT);
              if (MB_SUCCESS != tmp_result)
                break;
              file_id_tag = &id_tag;
            }
          }

          ReaderIface::IDTag parts = {partition_tag_name.c_str(), 0, 0};
          ReaderIface::SubsetList sl;
          sl.num_parts = 0;
          int rank = myPcomm->rank();
          if (partition_by_rank) {
            assert(partition_tag_vals.empty());
            parts.tag_values = &rank;
            parts.num_tag_values = 1;
          }
          else {
            sl.num_parts = myPcomm->size();
            sl.part_number = myPcomm->rank();
            if (!partition_tag_vals.empty()) {
              parts.tag_values = &partition_tag_vals[0];
              parts.num_tag_values = partition_tag_vals.size();
            }
          }
          std::vector<ReaderIface::IDTag> subset;
          if (subset_list) {
            std::vector<ReaderIface::IDTag> tmplist(subset_list->tag_list,
                           subset_list->tag_list + subset_list->tag_list_length);
            tmplist.push_back( parts );
            subset.swap(tmplist);
            sl.tag_list = &subset[0];
            sl.tag_list_length = subset.size();
          }
          else {
            sl.tag_list = &parts;
            sl.tag_list_length = 1;
          }
          tmp_result = impl->serial_load_file(*file_names, &file_set, opts, &sl, file_id_tag);
          if (MB_SUCCESS != tmp_result)
            break;

          if (!partition_tag_name.empty()) {
            Tag part_tag;
            tmp_result = impl->tag_get_handle(partition_tag_name.c_str(), 1, MB_TYPE_INTEGER, part_tag);
            if (MB_SUCCESS != tmp_result)
              break;

            tmp_result = impl->get_entities_by_type_and_tag(file_set, MBENTITYSET, &part_tag,
                                                            0, 1, myPcomm->partition_sets());
          }
        }
          break;

//==================
      case PA_GET_FILESET_ENTS:
          myDebug.tprint(1, "Getting fileset entities.\n");

          // Get entities in the file set, and add actual file set to it;
          // mark the file set to make sure any receiving procs know which it is
          tmp_result = mbImpl->get_entities_by_handle(file_set, entities);
          if (MB_SUCCESS != tmp_result) {
            entities.clear();
            break;
          }

          // Add actual file set to entities too
          entities.insert(file_set);
          break;

//==================
      case PA_BROADCAST:
          // Do the actual broadcast; if single-processor, ignore error
          myDebug.tprint(1, "Broadcasting mesh.\n");

          if (myPcomm->proc_config().proc_size() > 1) {
            tmp_result = myPcomm->broadcast_entities(reader_rank, entities);
            if (MB_SUCCESS != tmp_result)
              break;
          }

          if (debug) {
            std::cerr << "Bcast done; entities:" << std::endl;
            mbImpl->list_entities(0, 0);
          }

          // Add the received entities to this fileset if I wasn't the reader
          if (!i_read && MB_SUCCESS == tmp_result)
            tmp_result = mbImpl->add_entities(file_set, entities);

          break;

//==================
      case PA_DELETE_NONLOCAL:
          myDebug.tprint(1, "Deleting nonlocal entities.\n");

          tmp_result = delete_nonlocal_entities(partition_tag_name, 
                                                partition_tag_vals, 
                                                distrib,
                                                file_set);
          if (debug) {
            std::cerr << "Delete nonlocal done; entities:" << std::endl;
            mbImpl->list_entities(0, 0);
          }

          if (MB_SUCCESS == tmp_result) 
            tmp_result = create_partition_sets(partition_tag_name, file_set);

          break;

//==================
      case PA_CHECK_GIDS_SERIAL:
          myDebug.tprint(1, "Checking global IDs.\n");

          tmp_result = myPcomm->check_global_ids(file_set, 0, 1, true, false);
          break;

//==================
      case PA_RESOLVE_SHARED_ENTS:
          myDebug.tprint(1, "Resolving shared entities.\n");

          if (1 == myPcomm->size())
            tmp_result = MB_SUCCESS;
          else
            tmp_result = myPcomm->resolve_shared_ents(file_set, resolve_dim, shared_dim,
                                                      use_id_tag ? file_id_tag : 0);
          if (MB_SUCCESS != tmp_result)
            break;

#ifndef NDEBUG            
          // check number of owned vertices through pcomm's public interface
          tmp_result = mbImpl->get_entities_by_type(0, MBVERTEX, ents);
          if (MB_SUCCESS == tmp_result)
            tmp_result = myPcomm->filter_pstatus(ents, PSTATUS_NOT_OWNED, PSTATUS_NOT);
          if (MB_SUCCESS == tmp_result)
            myDebug.tprintf(1, "Proc %u reports %lu owned vertices.\n", myPcomm->proc_config().proc_rank(),
                            ents.size());
#endif
          break;

//==================
      case PA_EXCHANGE_GHOSTS:
          myDebug.tprint(1, "Exchanging ghost entities.\n");

          tmp_result = myPcomm->exchange_ghost_cells(ghost_dim, bridge_dim, 
                                                     num_layers, addl_ents, true, true, &file_set);
          break;

//==================
      case PA_RESOLVE_SHARED_SETS:
          myDebug.tprint(1, "Resolving shared sets.\n");

          if (1 == myPcomm->size())
            tmp_result = MB_SUCCESS;
          else
            tmp_result = myPcomm->resolve_shared_sets(file_set, use_id_tag ? file_id_tag : 0);
          break;

//==================
      case PA_AUGMENT_SETS_WITH_GHOSTS:
          myDebug.tprint(1, "Augmenting sets with ghost entities.\n");

          if (1 == myPcomm->size())
            tmp_result = MB_SUCCESS;
          else
            tmp_result = myPcomm->augment_default_sets_with_ghosts(file_set);
          break;
//==================
      case PA_PRINT_PARALLEL:
          myDebug.tprint(1, "Printing parallel information.\n");

          tmp_result = myPcomm->list_entities(0, -1);
          break;

//==================
      default:
          MB_SET_ERR(MB_FAILURE, "Unexpected parallel action");
    } // switch (*vit)

    if (MB_SUCCESS != tmp_result) {
        MB_SET_ERR(MB_FAILURE, "Failed in step " << ParallelActionsNames[*vit]);
    }

    if (cputime)
      act_times[i] = MPI_Wtime();
  } // for (i = 1, vit = pa_vec.begin(); vit != pa_vec.end(); ++vit, i++)

  if (use_id_tag) {
    result = mbImpl->tag_delete(id_tag);MB_CHK_SET_ERR(result, "Trouble deleting id tag");
  }

  if (cputime) {
    for (i = pa_vec.size(); i > 0; i--)
      act_times[i] -= act_times[i - 1];

    // Replace initial time with overall time
    act_times[0] = MPI_Wtime() - act_times[0];
    // Get the maximum over all procs
    if (0 != myPcomm->proc_config().proc_rank()) {
      MPI_Reduce(&act_times[0], 0, pa_vec.size() + 1, MPI_DOUBLE, MPI_MAX,
                 0, myPcomm->proc_config().proc_comm());
    }
    else {
#if (MPI_VERSION >= 2)
      MPI_Reduce(MPI_IN_PLACE, &act_times[0], pa_vec.size() + 1, MPI_DOUBLE,
                 MPI_MAX, 0, myPcomm->proc_config().proc_comm());
#else
      // Note, extra comm-size allocation is required
      std::vector<double> act_times_tmp(pa_vec.size() + 1);
      MPI_Reduce(&act_times[0], &act_times_tmp[0], pa_vec.size() + 1, MPI_DOUBLE,
                  MPI_MAX, 0, myPcomm->proc_config().proc_comm());
      act_times = act_times_tmp; // extra copy here too
#endif
      std::cout << "Parallel Read times: " << std::endl;
      for (i = 1, vit = pa_vec.begin(); vit != pa_vec.end(); ++vit, i++)
        std::cout << "  " << act_times[i] << " "
                  << ParallelActionsNames[*vit] << std::endl;
      std::cout << "  " << act_times[0] << " PARALLEL TOTAL" << std::endl;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadParallel::delete_nonlocal_entities(std::string &ptag_name,
                                                 std::vector<int> &ptag_vals,
                                                 bool distribute,
                                                 EntityHandle file_set)
{
  Range partition_sets;

  Tag ptag;
  ErrorCode result = mbImpl->tag_get_handle(ptag_name.c_str(), 1, MB_TYPE_INTEGER, ptag);MB_CHK_SET_ERR(result, "Failed getting tag handle in delete_nonlocal_entities");

  result = mbImpl->get_entities_by_type_and_tag(file_set, MBENTITYSET,
                                                &ptag, NULL, 1,
                                                myPcomm->partition_sets());MB_CHK_SET_ERR(result, "Failed to get sets with partition-type tag");

  int proc_sz = myPcomm->proc_config().proc_size();
  int proc_rk = myPcomm->proc_config().proc_rank();

  if (!ptag_vals.empty()) {
    // Values input, get sets with those values
    Range tmp_sets;
    std::vector<int> tag_vals(myPcomm->partition_sets().size());
    result = mbImpl->tag_get_data(ptag, myPcomm->partition_sets(), &tag_vals[0]);MB_CHK_SET_ERR(result, "Failed to get tag data for partition vals tag");
    for (std::vector<int>::iterator pit = tag_vals.begin();
         pit != tag_vals.end(); ++pit) {
      std::vector<int>::iterator pit2 = std::find(ptag_vals.begin(),
                                                  ptag_vals.end(), *pit);
      if (pit2 != ptag_vals.end()) 
        tmp_sets.insert(myPcomm->partition_sets()[pit - tag_vals.begin()]);
    }

    myPcomm->partition_sets().swap(tmp_sets);
  }

  if (distribute) {
    // For now, require that number of partition sets be greater
    // than number of procs
    if (myPcomm->partition_sets().size() < (unsigned int) proc_sz) {
      MB_SET_ERR(MB_FAILURE, "Too few parts; P = " << proc_rk << ", tag = " << ptag << ", # sets = " << myPcomm->partition_sets().size());
    }

    Range tmp_sets;
    // Distribute the partition sets
    unsigned int num_sets = myPcomm->partition_sets().size() / proc_sz;
    unsigned int num_leftover = myPcomm->partition_sets().size() % proc_sz;
    int begin_set = 0;
    if (proc_rk < (int) num_leftover) {
      num_sets++;
      begin_set = num_sets * proc_rk;
    }
    else
      begin_set = proc_rk * num_sets + num_leftover;

    for (unsigned int i = 0; i < num_sets; i++)
      tmp_sets.insert(myPcomm->partition_sets()[begin_set + i]);

    myPcomm->partition_sets().swap(tmp_sets);
  }

  myDebug.print(1, "My partition sets: ", myPcomm->partition_sets());

  result = delete_nonlocal_entities(file_set);MB_CHK_ERR(result);

  return MB_SUCCESS;
}

ErrorCode ReadParallel::create_partition_sets(std::string &ptag_name,
                                              EntityHandle file_set)
{
  int proc_rk = myPcomm->proc_config().proc_rank();
  ErrorCode result = MB_SUCCESS;

  Tag ptag;

  // Tag the partition sets with a standard tag name
  if (ptag_name.empty())
    ptag_name = PARALLEL_PARTITION_TAG_NAME;
  bool tag_created = false;
  result = mbImpl->tag_get_handle(ptag_name.c_str(), 1, MB_TYPE_INTEGER,
                                  ptag, MB_TAG_SPARSE | MB_TAG_CREAT, 0,
                                  &tag_created);MB_CHK_SET_ERR(result, "Trouble getting PARALLEL_PARTITION tag");

  if (!tag_created) {
    // This tag already exists; better check to see that tagged sets
    // agree with this partition
    Range tagged_sets;
    int *proc_rk_ptr = &proc_rk;
    result = mbImpl->get_entities_by_type_and_tag(file_set, MBENTITYSET, &ptag,
                                                  (const void* const*)&proc_rk_ptr, 1,
                                                  tagged_sets);MB_CHK_SET_ERR(result, "Trouble getting tagged sets");
    if (!tagged_sets.empty() && tagged_sets != myPcomm->partition_sets()) {
      result = mbImpl->tag_delete_data(ptag, tagged_sets);MB_CHK_SET_ERR(result, "Trouble deleting data of PARALLEL_PARTITION tag");
    }
    else if (tagged_sets == myPcomm->partition_sets())
      return MB_SUCCESS;
  }

  // If we get here, we need to assign the tag
  std::vector<int> values(myPcomm->partition_sets().size());
  for (unsigned int i = 0; i < myPcomm->partition_sets().size(); i++)
    values[i] = proc_rk;
  result = mbImpl->tag_set_data(ptag, myPcomm->partition_sets(), &values[0]);MB_CHK_SET_ERR(result, "Trouble setting data to PARALLEL_PARTITION tag");

  return MB_SUCCESS;
}

ErrorCode ReadParallel::delete_nonlocal_entities(EntityHandle file_set) 
{
  // Get partition entities and ents related to/used by those
  // get ents in the partition
  ReadUtilIface *read_iface;
  mbImpl->query_interface(read_iface);
  Range partition_ents, all_sets;

  myDebug.tprint(2, "Gathering related entities.\n");

  ErrorCode result = read_iface->gather_related_ents(myPcomm->partition_sets(), partition_ents,
                                           &file_set);MB_CHK_SET_ERR(result, "Failure gathering related entities");

  // Get pre-existing entities
  Range file_ents;
  result = mbImpl->get_entities_by_handle(file_set, file_ents);MB_CHK_SET_ERR(result, "Couldn't get pre-existing entities");

  if (0 == myPcomm->proc_config().proc_rank()) {
    myDebug.print( 2, "File entities: ", file_ents );
  }

  // Get deletable entities by subtracting partition ents from file ents
  Range deletable_ents = subtract( file_ents, partition_ents);

  // Cache deletable vs. keepable sets
  Range deletable_sets = deletable_ents.subset_by_type(MBENTITYSET);
  Range keepable_sets = subtract(file_ents.subset_by_type(MBENTITYSET), deletable_sets);

  myDebug.tprint(2, "Removing deletable entities from keepable sets.\n");

  // Remove deletable ents from all keepable sets
  for (Range::iterator rit = keepable_sets.begin();
       rit != keepable_sets.end(); ++rit) {
    result = mbImpl->remove_entities(*rit, deletable_ents);MB_CHK_SET_ERR(result, "Failure removing deletable entities");
  }
  result = mbImpl->remove_entities(file_set, deletable_ents);MB_CHK_SET_ERR(result, "Failure removing deletable entities");

  myDebug.tprint(2, "Deleting deletable entities.\n");

  if (0 == myPcomm->proc_config().proc_rank()) {
    myDebug.print(2, "Deletable sets: ", deletable_sets);
  }

  // Delete sets, then ents
  if (!deletable_sets.empty()) {
    result = mbImpl->delete_entities(deletable_sets);MB_CHK_SET_ERR(result, "Failure deleting sets in delete_nonlocal_entities");
  }

  deletable_ents -= deletable_sets;

  if (0 == myPcomm->proc_config().proc_rank()) {
    myDebug.print(2, "Deletable entities: ", deletable_ents);
  }

  if (!deletable_ents.empty()) {
    result = mbImpl->delete_entities(deletable_ents);MB_CHK_SET_ERR(result, "Failure deleting entities in delete_nonlocal_entities");
  }

  return MB_SUCCESS;
}

} // namespace moab
