/** MOAB Damsel file reader 
 * For description of the Damsel-MOAB data model mapping, see WriteDamsel.cpp.
 *
 */

#include "moab/ParallelComm.hpp"

#include "ReadDamsel.hpp"

#include "assert.h"
#include "moab/Interface.hpp"
#include "moab/Core.hpp"
#include "moab/Range.hpp"
#include "moab/Error.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "MBTagConventions.hpp"
#include "EntitySequence.hpp"
#include "Internals.hpp"
#include "DenseTag.hpp"

namespace moab {

ReaderIface* ReadDamsel::factory(Interface* iface)
{
  return new ReadDamsel(iface);
}

ReadDamsel::ReadDamsel(Interface *impl)
  : mbImpl(impl), readMeshIface(NULL), nativeParallel(false), myPcomm(NULL), mGlobalIdTag(0),
    dU()
{
  init();
}

ReadDamsel::~ReadDamsel()
{
  if (readMeshIface)
    mbImpl->release_interface(readMeshIface);

  DMSLlib_finalize(dU.dmslLib);
}

ErrorCode ReadDamsel::init()
{
  mbImpl->query_interface(readMeshIface);
  assert(readMeshIface);

  return MB_SUCCESS;
}

ErrorCode ReadDamsel::parse_options(const FileOptions &opts,
                                    bool &parallel)
{
  // Handle parallel options
  bool use_mpio = (MB_SUCCESS == opts.get_null_option("USE_MPIO"));
  ErrorCode rval = opts.match_option("PARALLEL", "READ_PART");
  parallel = (rval != MB_ENTITY_NOT_FOUND);
  nativeParallel = (rval == MB_SUCCESS);
  if (use_mpio && !parallel) {
    MB_SET_ERR(MB_NOT_IMPLEMENTED, "'USE_MPIO' option specified w/out 'PARALLEL' option");
  }

  return MB_SUCCESS;
}

// ASSUMPTIONS:
// Partition collection is a *flat* collection of handles for entities and other collections that
// will be represented on a part

ErrorCode ReadDamsel::load_file(const char* filename,
                                const EntityHandle* file_set,
                                const FileOptions& opts,
                                const ReaderIface::SubsetList* subset_list,
                                const Tag* file_id_tag)
{
  ErrorCode rval;

  rval = parse_options(opts, nativeParallel);
  if (MB_SUCCESS != rval)
    return rval;

  // Initialize damsel
  dU.dmslLib = DMSLlib_init();

  // Create a damsel model
  dU.dmslModel = DMSLmodel_create(sizeof(EntityHandle) == 8 ? DAMSEL_HANDLE_TYPE_HANDLE64 :
                                  DAMSEL_HANDLE_TYPE_HANDLE32);

  // Model attach - need model id from make model, filename
#ifdef MOAB_HAVE_MPI
  MPI_Comm comm = MPI_COMM_WORLD;
  if (nativeParallel) {
    comm = myPcomm->proc_config().proc_comm();
  }
#endif

  damsel_err_t err;
  err = DMSLmodel_attach(dU.dmslModel, filename, comm, NULL);CHK_DMSL_ERR(err, "DMSLmodel_attach failed");
  err = DMSLmodel_populate(dU.dmslModel);CHK_DMSL_ERR(err, "DMSLmodel_populate failed");

  // STEP 0: GET COLLECTION, TAG, ENTITY INFOS FOR GLOBAL MODEL
  int num_containers = 0, num_tag_infos = 0, num_ent_infos = 0;
  DMSLmodel_get_tuple_count(dU.dmslModel, &num_containers, &num_tag_infos);
  num_ent_infos = DMSLmodel_get_entity_count(dU.dmslModel);
  int num_coll_infos = DMSLmodel_get_collection_count(dU.dmslModel);CHK_DMSL_ERR(err, "DMSLmodel_get_collection_count failed");
  if (-1 == num_containers || -1 == num_tag_infos || -1 == num_ent_infos) 
    MB_SET_ERR(MB_FAILURE, "Bad count for containers/tags/ents");

  std::vector<damsel_entity_buf_type> ent_infos(num_ent_infos);
  std::vector<damsel_collection_buf_type> coll_infos(num_coll_infos);
  std::vector<damsel_tag_buf_type> tag_infos(num_tag_infos);
  std::vector<damsel_container_buf_type> cont_infos(num_containers);
  err = DMSLmodel_get_entity_infos(dU.dmslModel, &ent_infos[0]);CHK_DMSL_ERR(err, "Failure getting entity infos");
  err = DMSLmodel_get_collection_infos(dU.dmslModel, &coll_infos[0]);CHK_DMSL_ERR(err, "Failure getting collection infos");
  err = DMSLmodel_get_tag_infos(dU.dmslModel, &tag_infos[0]);CHK_DMSL_ERR(err, "Failure getting tag infos");
  err = DMSLmodel_get_container_infos(dU.dmslModel, &cont_infos[0]);CHK_DMSL_ERR(err, "Failure getting container infos");

  // Create MOAB-side tags for all damsel tags except pre-defined ones
  rval = process_tags(tag_infos);MB_CHK_SET_ERR(rval, "Error processing tags");

/*
  if (nativeParallel) {
    // STEP 1: GET COLLECTION(S) REPRESENTING PARTITION:
    // Input: tag name, optionally value;
    // Output: container with file-side handles of collections satisfying those criteria
    // - Get all handles/values for tag
    // - Select handles matching criteria for tag value (will be collection handles)
    std::string partn_tag_name("PARALLEL_PARTITION");
    damsel_handle partn_tag = DMSLselect_tag_by_name(dU.dmslModel, partn_tag_name.c_str());
    // Get all the parts with that tag regardless of value
    damsel_container part_handles = DMSLselect_handles_with_values(dU.dmslModel, partn_tag);

    // STEP 2: GET HANDLES FOR TAGS WE NEED FOR THIS READER:
    // - "SET_CHARACTERISTIC"
    damsel_handle setchar_tag = DMSLselect_tag_by_name(dU.dmslModel, "SET_CHARACTERISTIC");
    // - "PARENT_LIST"
    //damsel_handle plist_tag = DMSLselect_tag_by_name(dU.dmslModel, "PARENT_LIST");
    // - "CHILD_LIST"
    //damsel_handle clist_tag = DMSLselect_tag_by_name(dU.dmslModel, "CHILD_LIST");

    // STEP 3: GET VALUES FOR "SET_CHARACTERISTIC" TAG ON PARTITION COLLECTIONS,
    //         GET VECTOR- OR SET-TYPE FLAGS FOR PARTITION COLLECTIONS
    // (gives tracking flag for moab)
    int num_handles = DMSLcontainer_count(part_handles);
    std::vector<unsigned> char_tagvals(num_handles);
    // Map the set chars
    err = DMSLmodel_map_tag(&char_tagvals[0], part_handles, &setchar_tag);CHK_DMSL_ERR(err, "Problem calling DMSLmodel_map_tag");

    // Execute the transfer
    err = DMSLmodel_transfer_sync(dU.dmslModel, DAMSEL_TRANSFER_TYPE_READ);CHK_DMSL_ERR(err, "Problem calling DMSLmodel_transfer_sync");

    // STEP 4: READ/PROCESS PARTITION COLLECTION(S)
    // Decide the handles I am responsible using round-robin for now
    // - GET TYPE, CONTENTS OF COLLECTION CONTENTS CONTAINER
    // - Allocate moab-side container (using count from container)
    // - MAP storage TO CONTAINER 
    // - EXECUTE
    // ==> have list of all handles (entities + collections) represented on this proc

    int tmp_num = num_handles / proc_size, extra = num_handles % proc_size;
    if (extra) tmp_num++;
    int my_num_handles = tmp_num;
    if (proc_rank >= extra) my_num_handles--;
    int first_ind = std::min(proc_rank, extra) * tmp_num +
        std::max(proc_rank - extra, 0) * (tmp_num - 1);

    // - Create moab entity sets for partition collection(s)
    EntityHandle start_handle;
    rval = readMeshIface->create_entity_sets(my_num_handles, &char_tagvals[first_ind], 0, start_handle);MB_CHK_SET_ERR(rval, "Problem creating entity sets");
  }
  else {
*/
    // Initialize just by entity; each call to process_ent_info will:
    // a. Create moab-side representation to read into
    // b. Map those handles to damsel handles
    // c. Map coords / connectivity storage to damsel equivalent
    // d. For each tag, map moab storage to damsel storage
    std::vector<damsel_entity_buf_type>::iterator eiit;

    // Process verts info first
    for (eiit =  ent_infos.begin(); eiit != ent_infos.end(); ++eiit) {
      if ((*eiit).entity_type == DAMSEL_ENTITY_TYPE_VERTEX) {
        rval = process_ent_info(*eiit);MB_CHK_ERR(rval);
      }
    }

    for (eiit =  ent_infos.begin(); eiit != ent_infos.end(); ++eiit) {
      if ((*eiit).entity_type != DAMSEL_ENTITY_TYPE_VERTEX) {
        rval = process_ent_info(*eiit);MB_CHK_ERR(rval);
      }
    }

/*
  }

  // Process collections
  rval = process_coll_infos(coll_infos);MB_CHK_ERR(rval);

  // STEP 5: Process into list of local info structs, each represents file-side struct and
  // portion of that struct
  // ASSUMPTION: Each local info struct represents single entity type & # vertices or collection

  // STEP 6: For each local info struct:

  // STEP 6b: READ CONTAINER INTO LOCAL BUFFER
  // STEP 6c: Create app representation of entities/vertices/collection, and damsel container for them,
  //    and MAP APP HANDLE CONTAINER TO DAMSEL CONTAINER
  // STEP 6d: Process vertices/entities/collection
  //    6d1: If vertices, continue
  //    6d2: If entities:
  //    - MAP LOCAL CONNECTIVITY REP'N TO DAMSEL (might be tag, don't know yet)
  //    6d3: If collection:
  //    - (everything in STEP 4 for this collection except for EXECUTE)
  //    - might need to filter out things not represented on this rank
  //    6d4: If sparse tag:
  //    - Create app-side representation of sparse tag
  //    - READ CONTAINER OF MODEL HANDLES INTO LOCAL BUFFER
  //    - Allocate app-side storage for tag values
  //    - MAP APP STORAGE TO MODEL TAG + (implicit?) CONTAINER

  // STEP 6e: Process dense tags for the local info struct; for each dense tag:
  //   - Get app tag handle for model tag handle
  //   - Get app storage for app tag handle + container
  //   - MAP APP STORAGE TO MODEL TAG + CONTAINER

  // STEP 7: EXECUTE
  //   - Assign all mapped data
  //   - Translate all damsel handles to app handles
  // uninit damsel

*/

  return MB_SUCCESS;
}

ErrorCode ReadDamsel::read_tag_values(const char* file_name,
                                      const char* tag_name,
                                      const FileOptions& opts,
                                      std::vector<int>& tag_values_out,
                                      const SubsetList* subset_list)
{
  return MB_FAILURE;
}

ErrorCode ReadDamsel::process_tags(std::vector<damsel_tag_buf_type> &tag_infos)
{
  Tag tagh;
  ErrorCode rval = MB_SUCCESS, tmp_rval;
  for (std::vector<damsel_tag_buf_type>::iterator tit = tag_infos.begin(); tit != tag_infos.end(); ++tit) {
    if (DamselUtil::dtom_data_type[(*tit).tag_datatype] == MB_TYPE_OPAQUE) {
      std::cout << "Damsel reader encountered opaque tag." << std::endl;
      continue;
    }

    tmp_rval = mbImpl->tag_get_handle((*tit).name, 1,
                                      DamselUtil::dtom_data_type[(*tit).tag_datatype],
                                      tagh, MB_TAG_CREAT | MB_TAG_DENSE);
    if (MB_SUCCESS != tmp_rval)
      rval = tmp_rval;
    else {
      dU.tagMap.push_back(DamselUtil::tinfo(tagh, 0, MB_TAG_DENSE));
      // Also store predefined tags specially...
      if (!strncmp((*tit).name, "mbdmsl_", 7)) {
        // Predefined tag name, store the handle
        if (!strcmp((*tit).name, "mbdmsl_XCOORDS"))
          dU.xcoordsTag = dU.tagMap.back();
        else if (!strcmp((*tit).name, "mbdmsl_YCOORDS")) {
          dU.ycoordsTag = dU.tagMap.back();
        }
        else if (!strcmp((*tit).name, "mbdmsl_ZCOORDS")) {
          dU.zcoordsTag = dU.tagMap.back();
        }
        else if (!strcmp((*tit).name, "mbdmsl_COLL_FLAGS")) {
          dU.collFlagsTag = dU.tagMap.back();
        }
        else if (!strcmp((*tit).name, "mbdmsl_PARENTS")) {
          dU.parentsTag = dU.tagMap.back();
        }
        else if (!strcmp((*tit).name, "mbdmsl_CHILDREN")) {
          dU.childrenTag = dU.tagMap.back();
        }
        else {
          rval = MB_FAILURE;
          continue;
        }
      }
    }
  }

  return rval;
}

ErrorCode ReadDamsel::process_ent_info(const damsel_entity_buf_type &einfo)
{
  // Create this chunk of entities
  EntityHandle *connect, start_handle;
  ErrorCode rval;
  damsel_err_t err;
  damsel_container app_cont;
  Range these_ents;

  // Check that there's only one contiguous run of file-side handles, fail if there isn't
#ifndef NDEBUG
  Range fside_handles;
  rval = DamselUtil::container_to_range(dU.dmslModel, const_cast<damsel_container&>(einfo.entity_container),
                                        fside_handles);
  if (MB_SUCCESS != rval || fside_handles.size() != einfo.count ||
      fside_handles.psize() != 1) 
    return MB_FAILURE;
#endif

  if (einfo.entity_type != DAMSEL_ENTITY_TYPE_VERTEX) {
    // Create the moab entities
    rval = readMeshIface->get_element_connect(einfo.count, einfo.vertices_per_entity,
                                              DamselUtil::dtom_entity_type[einfo.entity_type],
                                              0, start_handle, connect);MB_CHK_ERR(rval);
    these_ents.insert(start_handle, start_handle + einfo.count - 1);

    // Create an app-side sequence and map to file-side container
    app_cont = DMSLcontainer_create_sequence(dU.dmslModel, einfo.count, start_handle, 1);
    err = DMSLmodel_map_handles(app_cont, einfo.entity_container);CHK_DMSL_ERR(err, "Error returned mapping entity handles");

    // Map connectivity
    assert(DMSLcontainer_count(einfo.vertex_container) == (int)(einfo.vertices_per_entity*einfo.count));
    rval = get_contents(dU.dmslModel, einfo.vertex_container, connect);MB_CHK_SET_ERR(rval, "Error returned mapping connectivity");
  }
  else {
    // Get the number of coordinate arrays
    int num_ctags = 0;
    damsel_handle xcoord_dtag = DMSLselect_tag_by_name(dU.dmslModel, "mbdmsl_XCOORDS");
    if (xcoord_dtag)
      num_ctags++;
    damsel_handle ycoord_dtag = DMSLselect_tag_by_name(dU.dmslModel, "mbdmsl_YCOORDS");
    if (ycoord_dtag)
      num_ctags++;
    damsel_handle zcoord_dtag = DMSLselect_tag_by_name(dU.dmslModel, "mbdmsl_ZCOORDS");
    if (zcoord_dtag)
      num_ctags++;

    // Should have one vertex per entity
    assert(einfo.vertices_per_entity == 1);
    std::vector<double*> coord_arrays;
    rval = readMeshIface->get_node_coords(num_ctags, einfo.count, 0, start_handle, coord_arrays);MB_CHK_ERR(rval);

    these_ents.insert(start_handle, start_handle + einfo.count - 1);

    // Create an app-side sequence and map to file-side container
    app_cont = DMSLcontainer_create_sequence(dU.dmslModel, start_handle, einfo.count, 1);
    err = DMSLmodel_map_handles(app_cont, einfo.entity_container);CHK_DMSL_ERR(err, "Trouble mapping entity handles");

    // Map the coords storage
    if (xcoord_dtag != 0) {
      err = DMSLmodel_map_tag(coord_arrays[0], app_cont, (damsel_handle_ptr)&dU.xcoordsTag.mTagh);CHK_DMSL_ERR(err, "Trouble mapping x coordinate tag");
    }
    if (ycoord_dtag != 0) {
      err = DMSLmodel_map_tag(coord_arrays[1], app_cont, (damsel_handle_ptr)&dU.ycoordsTag.mTagh);CHK_DMSL_ERR(err, "Trouble mapping y coordinate tag");
    }
    if (zcoord_dtag != 0) {
      err = DMSLmodel_map_tag(coord_arrays[2], app_cont, (damsel_handle_ptr)&dU.zcoordsTag.mTagh);CHK_DMSL_ERR(err, "Trouble mapping z coordinate tag");
    }
  }

  // Save mapping from moab entity to einfo
  dmHandleRMap.insert(DMSLcontainer_handle_at_position(einfo.entity_container, 0), start_handle, einfo.count);

  rval = process_entity_tags(einfo.tag_count, einfo.tag_handle_container, app_cont, these_ents);

  return rval;
}

ErrorCode ReadDamsel::process_entity_tags(int count, damsel_container tag_container,
                                          damsel_container app_cont, Range &these_ents)
{
  // Process tags on these entities
  ErrorCode rval = MB_SUCCESS;
  for (int i = 0; i < count; i++) {
    damsel_handle dtagh = DMSLcontainer_handle_at_position(tag_container, i);

    // Don't do conventional tags here
    std::vector<DamselUtil::tinfo>::iterator vit =
        std::find_if(dU.tagMap.begin(), dU.tagMap.end(), DamselUtil::DtagP<DamselUtil::tinfo>(dtagh));

    if ((*vit).tagType == MB_TAG_ANY)
      continue;
    else if (vit == dU.tagMap.end())
      MB_SET_ERR(MB_FAILURE, "Failed to find tag");

    Tag tagh = (*vit).mTagh;
    assert(tagh);
    void *tag_data;
    int ecount = these_ents.size();
    rval = mbImpl->tag_iterate(tagh, these_ents.begin(), these_ents.end(), ecount, tag_data);MB_CHK_SET_ERR(rval, "Problem getting tag iterator");
    assert(ecount == (int)these_ents.size());
    damsel_err_t err = DMSLmodel_map_tag(tag_data, app_cont, (damsel_handle_ptr)&tagh);CHK_DMSL_ERR(err, "Problem calling DMSLmodel_map_tag");
  }

  return rval;
}

ErrorCode ReadDamsel::get_contents(damsel_model m, damsel_container c, Range &ents) {
  EntityHandle eh;
  if (DMSLcontainer_get_type(c) == DAMSEL_HANDLE_CONTAINER_TYPE_SEQUENCE) {
    damsel_handle start;
    size_t count, stride;
    damsel_err_t err = DMSLcontainer_sequence_get_contents(m, c, &start, &count, &stride);CHK_DMSL_ERR(err, "DMSLcontainer_sequence_get_contents");
    if (stride == 1) {
      while (count) {
        // Get start in rangemap
        RangeMap<damsel_handle, EntityHandle, 0>::iterator beg = dmHandleRMap.lower_bound(start);
        if (beg == dmHandleRMap.end())
          return MB_SUCCESS;
        unsigned long diff = std::max((*beg).begin-start, (damsel_handle)0);
        unsigned long num = std::min(count - diff, (size_t)(*beg).count);
        ents.insert((*beg).begin + diff, (*beg).begin + diff + num - 1);
        count -= (diff + num);
        ++beg;
      }
    }
    else {
      for (int i = count - 1; i >= 0; i--) {
        if (dmHandleRMap.find(start + i, eh))
          ents.insert(eh);
      }
    }
  }
  else if (DMSLcontainer_get_type(c) == DAMSEL_HANDLE_CONTAINER_TYPE_VECTOR) {
    damsel_handle *handle_ptr;
    size_t count;
    damsel_err_t err = DMSLcontainer_vector_get_contents(m, c, &handle_ptr, &count);CHK_DMSL_ERR(err, "Trouble getting vector contents");
    for (int i = count - 1; i >= 0; i--) {
      if (dmHandleRMap.find(handle_ptr[i], eh))
        ents.insert(eh);
    }
  }
  else if (DMSLcontainer_get_type(c) == DAMSEL_HANDLE_CONTAINER_TYPE_TREE) {
    damsel_handle_ptr node_ptr = NULL;
    damsel_container cont = NULL;
    while (DMSLcontainer_tree_get_contents(m, c, &node_ptr, &cont) == DMSL_OK &&
           cont) {
      ErrorCode rval = get_contents(m, c, ents);
      if (MB_SUCCESS != rval)
        return rval;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadDamsel::get_contents(damsel_model m, damsel_container c, EntityHandle *ents) {
  EntityHandle eh;
  int ind = 0;

  if (DMSLcontainer_get_type(c) == DAMSEL_HANDLE_CONTAINER_TYPE_SEQUENCE) {
    damsel_handle start;
    size_t count, stride;
    damsel_err_t err = DMSLcontainer_sequence_get_contents(m, c, &start, &count, &stride);CHK_DMSL_ERR(err, "Problem calling DMSLcontainer_sequence_get_contents");
    if (stride == 1) {
      while (count) {
        // Get start in rangemap
        RangeMap<damsel_handle, EntityHandle, 0>::iterator beg = dmHandleRMap.lower_bound(start);
        if (beg == dmHandleRMap.end())
          return MB_SUCCESS;
        unsigned int diff = std::max((*beg).begin - start, (damsel_handle)0);
        unsigned int num = std::min(count - diff, (size_t)(*beg).count);
        for (EntityHandle hdl = (*beg).begin + diff; hdl <= (int)(*beg).begin + diff + num - 1; hdl++)
          ents[ind++] = hdl;
        count -= (diff + num);
        ++beg;
      }
    }
    else {
      for (int i = count - 1; i >= 0; i--) {
        if (dmHandleRMap.find(start + i, eh))
          ents[i] = eh;
      }
    }
  }
  else if (DMSLcontainer_get_type(c) == DAMSEL_HANDLE_CONTAINER_TYPE_VECTOR) {
    damsel_handle_ptr handle_ptr;
    size_t count;
    damsel_err_t err = DMSLcontainer_vector_get_contents(m, c, &handle_ptr, &count);CHK_DMSL_ERR(err, "Failed to get vector contents");
    for (int i = count - 1; i >= 0; i--) {
      if (dmHandleRMap.find(handle_ptr[i], eh))
        ents[i] = eh;
    }
  }
  else if (DMSLcontainer_get_type(c) == DAMSEL_HANDLE_CONTAINER_TYPE_TREE) {
    damsel_handle_ptr node_ptr = NULL;
    damsel_container cont = NULL;
    while (DMSLcontainer_tree_get_contents(m, c, &node_ptr, &cont) == DMSL_OK &&
           cont) {
      ErrorCode rval = get_contents(m, cont, ents);
      if (MB_SUCCESS != rval)
        return rval;
      unsigned int num = DMSLcontainer_count(cont);
      ents += num;
    }
  }

  return MB_SUCCESS;
}

/*
ErrorCode ReadDamsel::process_coll_infos(std::vector<damsel_collection_buf_type> &coll_infos) 
{
  ErrorCode rval = MB_SUCCESS, tmp_rval;
  EntityHandle seth;
  std::vector<subrange> handle_subranges;
  for (std::vector<damsel_collection_buf_type>::iterator cit = coll_infos.begin(); cit != coll_infos.end(); ++cit) {
    // Make the set
    tmp_rval = mbImpl->create_meshset(((*cit).type ==  DAMSEL_HANDLE_COLLECTION_TYPE_SET ? MESHSET_SET : MESHSET_ORDERED),
                                      seth);
    if (MB_SUCCESS != tmp_rval)
      rval = tmp_rval;
    // Make datastructures to pass things to process_entity_tags
    Range tmp_range(seth, seth);
    damsel_container ch = DMSLcontainer_create_sequence(dU.dmslModel, seth, 1, 1);

    // Get the tags on this set
    tmp_rval = process_entity_tags((*cit).tag_count, (*cit).tag_handle_container, ch, tmp_range);
    if (MB_SUCCESS != tmp_rval)
      rval = tmp_rval;

    // Process the set contents
    if ((*cit).type == DAMSEL_HANDLE_COLLECTION_TYPE_SET) {
      Range ents;
      tmp_rval = get_contents(dU.dmslModel, (*cit).contents, ents);
      if (MB_SUCCESS != tmp_rval)
        rval = tmp_rval;
      else if (!ents.empty()) {
        tmp_rval = mbImpl->add_entities(seth, ents);
        if (MB_SUCCESS != tmp_rval)
          rval = tmp_rval;
      }
    }
    else {
      std::vector<EntityHandle> ents(DMSLcontainer_count((*cit).contents));
      tmp_rval = get_contents(dU.dmslModel, (*cit).contents, &ents[0]);
      if (MB_SUCCESS != tmp_rval)
        rval = tmp_rval;
      else if (!ents.empty()) {
        tmp_rval = mbImpl->add_entities(seth, &ents[0], ents.size());
        if (MB_SUCCESS != tmp_rval)
          rval = tmp_rval;
      }
    }

    // Get the file handle for this collection, and map it to moab's set handle
    damsel_handle collh = (damsel_handle)(*((*cit).collection_handle));
    if (handle_subranges.empty() || seth != (*handle_subranges.rbegin()).seth + 1 ||
        collh != (*handle_subranges.rbegin()).collh + 1) {
      handle_subranges.push_back(subrange(collh, seth, 1));
    }
    else (*handle_subranges.rbegin()).count++;
  }

  for (std::vector<subrange>::iterator vit = handle_subranges.begin(); vit != handle_subranges.end(); ++vit)
    dmHandleRMap.insert((*vit).collh, (*vit).seth, (*vit).count);

  return rval;
}
*/

}
