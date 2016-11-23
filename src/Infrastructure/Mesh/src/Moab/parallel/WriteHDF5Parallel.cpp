#undef DEBUG
#undef TIME_DEBUG

#include <stdio.h>
#include <stdarg.h>

#include <stdio.h>
#include <time.h>

#include <stdlib.h>
#include <string.h>

#include <vector>
#include <set>
#include <map>
#include <utility>
#include <iostream>
#include <sstream>

#include "moab/Interface.hpp"
#include "Internals.hpp"
#include "MBTagConventions.hpp"
#include "MBParallelConventions.h"
#include "moab/ParallelComm.hpp"
#include "moab/CN.hpp"
#include "moab/Range.hpp"
#include "moab/CpuTimer.hpp"

#include "WriteHDF5Parallel.hpp"

#ifndef MOAB_HAVE_HDF5
  #error Attempt to compile WriteHDF5Parallel with HDF5 support disabled
#endif

#include <H5Tpublic.h>
#include <H5Ppublic.h>
#include <H5FDmpi.h>
#include <H5FDmpio.h>

#include "mhdf.h"

#include "IODebugTrack.hpp"
#include "moab/FileOptions.hpp"

namespace {
  template<bool Condition> struct STATIC_ASSERTION;
  template<> struct STATIC_ASSERTION<true> {};
}

#define PP_CAT_(a, b) a ## b
#define PP_CAT(a, b) PP_CAT_(a, b)
#define STATIC_ASSERT(Condition) \
  enum { PP_CAT(dummy, __LINE__) = sizeof(::STATIC_ASSERTION<(bool)(Condition)>) }

namespace moab {

// Need an MPI type that we can put handles in
STATIC_ASSERT(sizeof(unsigned long) >= sizeof(EntityHandle));

// Need an MPI type that we can put file IDs in
STATIC_ASSERT(sizeof(unsigned long) >= sizeof(WriteHDF5::wid_t));

// This function doesn't do anything useful. It's just a nice
// place to set a break point to determine why the reader fails.
static inline ErrorCode error(ErrorCode rval)
{
  return rval;
}

const char* mpi_err_str(int errorcode) {
  static char buffer[2048];
  int len = sizeof(buffer);
  MPI_Error_string(errorcode, buffer, &len);
  buffer[std::min((size_t)len, sizeof(buffer) - 1)] = '\0';
  return buffer;
}

#define MPI_FAILURE_MSG(A) "MPI Failure at " __FILE__ ":%d : (Code %d) %s\n", \
                           __LINE__, (int)(A), mpi_err_str((A))

#define CHECK_MPI(A) \
  do { \
    if (MPI_SUCCESS != (A)) { \
      MB_SET_ERR_CONT("MPI Failure : (Code " << (int)(A) << ") " << mpi_err_str((A))); \
      dbgOut.printf(1, MPI_FAILURE_MSG((A))); \
      return error(MB_FAILURE); \
    } \
  } while (false)

#define MB_FAILURE_MSG(A) "MOAB_Failure at " __FILE__ ":%d : %s (%d)\n", \
                          __LINE__, ErrorCodeStr[(A)], (int)(A)

#define CHECK_MB(A) \
  do { \
    if (MB_SUCCESS != (A)) { \
      MB_SET_ERR_CONT("MOAB Failure : " << ErrorCodeStr[(A)]); \
      dbgOut.printf(1, MB_FAILURE_MSG((A))); \
      return error(A); \
    } \
  } while (false)

#define HDF_FAILURE_MSG(A) "MHDF Failure at " __FILE__ ":%d : %s\n", \
                           __LINE__, mhdf_message(&(A))

#define CHECK_HDF(A) \
  do { \
    if (mhdf_isError(&(A))) { \
      MB_SET_ERR_CONT("MHDF Failure : " << mhdf_message(&(A))); \
      dbgOut.printf(1, HDF_FAILURE_MSG((A))); \
      return error(MB_FAILURE); \
    } \
  } while (false)

#define CHECK_HDFN(A) \
  do { \
    if (mhdf_isError(&(A))) { \
      MB_SET_ERR_CONT("MHDF Failure : " << mhdf_message(&(A))); \
      return error(MB_FAILURE); \
    } \
  } while (false)

#ifdef VALGRIND
  #include <valgrind/memcheck.h>
#else
  #ifndef VALGRIND_CHECK_MEM_IS_DEFINED
    #define VALGRIND_CHECK_MEM_IS_DEFINED(a, b) ((void)0)
  #endif
  #ifndef VALGRIND_CHECK_MEM_IS_ADDRESSABLE
    #define VALGRIND_CHECK_MEM_IS_ADDRESSABLE(a, b) ((void)0)
  #endif
  #ifndef VALGRIND_MAKE_MEM_UNDEFINED
    #define VALGRIND_MAKE_MEM_UNDEFINED(a, b) ((void)0)
  #endif
#endif

template <typename T> inline 
void VALGRIND_MAKE_VEC_UNDEFINED(std::vector<T>& v) {
  if (v.size()) {}
    (void)VALGRIND_MAKE_MEM_UNDEFINED(&v[0], v.size() * sizeof(T));
}

#ifndef NDEBUG
  #define START_SERIAL \
     for (unsigned _x = 0; _x < myPcomm->proc_config().proc_size(); ++_x) { \
       MPI_Barrier(myPcomm->proc_config().proc_comm()); \
       if (_x != myPcomm->proc_config().proc_rank()) \
         continue
  #define END_SERIAL \
     } \
     MPI_Barrier(myPcomm->proc_config().proc_comm())
#else
  #define START_SERIAL
  #define END_SERIAL
#endif

#ifdef NDEBUG
  #undef assert
  #define assert
#else
  #undef assert
  #define assert(A) \
    if (!(A)) \
      do_assert(__FILE__, __LINE__, #A)
   static void do_assert(const char* file, int line, const char* condstr)
   {
     int rank;
     MPI_Comm_rank(MPI_COMM_WORLD, &rank);
     fprintf(stderr, "[%d] Assert(%s) failed at %s:%d\n", rank, condstr, file, line);
     fflush(stderr);
     abort();
   }
#endif

static int my_Gatherv(void* sendbuf,
                      int sendcount,
                      MPI_Datatype sendtype,
                      std::vector<unsigned char>& recvbuf,
                      std::vector<int>& recvcounts,
                      int root,
                      MPI_Comm comm)
{
  int nproc, rank, bytes, err;
  MPI_Comm_size(comm, &nproc);
  MPI_Comm_rank(comm, &rank);
  MPI_Type_size(sendtype, &bytes);

  recvcounts.resize(rank == root ? nproc : 0);
  err = MPI_Gather(&sendcount, 1, MPI_INT, &recvcounts[0], 1, MPI_INT, root, comm);
  if (MPI_SUCCESS != err)
    return err;

  std::vector<int> disp(recvcounts.size());
  if (root == rank) {
    disp[0] = 0;
    for (int i = 1; i < nproc; ++i)
      disp[i] = disp[i - 1] + recvcounts[i - 1];
    recvbuf.resize(bytes * (disp.back() + recvcounts.back()));
  }

  return MPI_Gatherv(sendbuf, sendcount, sendtype,
                     &recvbuf[0], &recvcounts[0], &disp[0],
                     sendtype, root, comm);
}

static void print_type_sets(Interface* iFace, DebugOutput* str, Range& sets)
{
  const unsigned VB = 2;
  if (str->get_verbosity() < VB)
    return;

  Tag gid, did, bid, sid, nid;
  iFace->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gid);
  iFace->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, did);
  iFace->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER, bid);
  iFace->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER, nid);
  iFace->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER, sid);
  Range typesets[10];
  const char* typenames[] = {"Block ", "Sideset ", "NodeSet", "Vertex", "Curve", "Surface", "Volume", "Body", "Other"};
  for (Range::iterator riter = sets.begin(); riter != sets.end(); ++riter) {
    unsigned dim, id ; //, oldsize;
    if (MB_SUCCESS == iFace->tag_get_data(bid, &*riter, 1, &id))
      dim = 0;
    else if (MB_SUCCESS == iFace->tag_get_data(sid, &*riter, 1, &id))
      dim = 1;
    else if (MB_SUCCESS == iFace->tag_get_data(nid, &*riter, 1, &id))
      dim = 2;
    else if (MB_SUCCESS == iFace->tag_get_data(did, &*riter, 1, &dim)) {
      id = 0;
      iFace->tag_get_data(gid, &*riter, 1, &id);
      dim += 3;
    }
    else {
      id = *riter;
      dim = 9;
    }

    //oldsize = typesets[dim].size();
    typesets[dim].insert(id);
    //assert(typesets[dim].size() - oldsize == 1);
  }
  for (int ii = 0; ii < 9; ++ii) {
    char tmp[64];
    sprintf(tmp, "%s (%lu) ", typenames[ii], (unsigned long)typesets[ii].size());
    str->print(VB, tmp, typesets[ii]);
  }
  str->printf(VB, "Total: %lu\n", (unsigned long)sets.size());
}

#define debug_barrier() debug_barrier_line(__LINE__)

void WriteHDF5Parallel::debug_barrier_line(int lineno)
{
  const unsigned threshold = 2;
  static unsigned long count = 0;
  if (dbgOut.get_verbosity() >= threshold && myPcomm) {
    dbgOut.printf(threshold, "*********** Debug Barrier %lu (@%d)***********\n", ++count, lineno);
    MPI_Barrier(myPcomm->proc_config().proc_comm());
  }
}

WriterIface* WriteHDF5Parallel::factory(Interface* iface)
{
  return new WriteHDF5Parallel(iface);
}

WriteHDF5Parallel::WriteHDF5Parallel(Interface* iface)
  : WriteHDF5(iface), myPcomm(NULL), pcommAllocated(false), hslabOp(H5S_SELECT_OR)
{
}

WriteHDF5Parallel::~WriteHDF5Parallel()
{
  if (pcommAllocated && myPcomm) 
    delete myPcomm;
}

// The parent WriteHDF5 class has ExportSet structs that are
// populated with the entities to be written, grouped by type
// (and for elements, connectivity length).  This function:
//  o determines which entities are to be written by a remote processor
//  o removes those entities from the ExportSet structs in WriteMesh
//  o passes them back in a Range
ErrorCode WriteHDF5Parallel::gather_interface_meshes(Range& nonowned)
{
  ErrorCode result;

  //START_SERIAL;
  dbgOut.print(3, "Pre-interface mesh:\n");
  dbgOut.print(3, nodeSet.range);
  for (std::list<ExportSet>::iterator eiter = exportList.begin();
           eiter != exportList.end(); ++eiter)
    dbgOut.print(3, eiter->range);
  dbgOut.print(3, setSet.range);

  // Move handles of non-owned entities from lists of entities
  // that this processor will write to the 'nonowned' list.

  nonowned.clear();
  result = myPcomm->filter_pstatus(nodeSet.range, PSTATUS_NOT_OWNED, PSTATUS_AND, -1, &nonowned);
  if (MB_SUCCESS != result)
    return error(result);
  nodeSet.range = subtract(nodeSet.range, nonowned);

  for (std::list<ExportSet>::iterator eiter = exportList.begin();
       eiter != exportList.end(); ++eiter) {
    Range tmpset;
    result = myPcomm->filter_pstatus(eiter->range, PSTATUS_NOT_OWNED, PSTATUS_AND, -1, &tmpset);
    if (MB_SUCCESS != result)
      return error(result);
    eiter->range = subtract(eiter->range, tmpset);
    nonowned.merge(tmpset);
  }

  dbgOut.print(3, "Post-interface mesh:\n");
  dbgOut.print(3, nodeSet.range);
  for (std::list<ExportSet>::iterator eiter = exportList.begin();
       eiter != exportList.end(); ++eiter)
    dbgOut.print(3, eiter->range);
  dbgOut.print(3, setSet.range);

  //END_SERIAL;

  return MB_SUCCESS;
}

ErrorCode WriteHDF5Parallel::parallel_create_file(const char* filename,
                                                  bool overwrite,
                                                  const std::vector<std::string>& qa_records,
                                                  const FileOptions& opts,
                                                  const Tag* user_tag_list,
                                                  int user_tag_count,
                                                  int dimension,
                                                  double* times)
{
  ErrorCode rval;
  mhdf_Status status;

  int pcomm_no = 0;
  opts.get_int_option("PARALLEL_COMM", pcomm_no);

  myPcomm = ParallelComm::get_pcomm(iFace, pcomm_no);
  if (0 == myPcomm) {
    myPcomm = new ParallelComm(iFace, MPI_COMM_WORLD);
    pcommAllocated = true;
  }

  MPI_Info info = MPI_INFO_NULL;
  std::string cb_size;
  rval = opts.get_str_option("CB_BUFFER_SIZE", cb_size);
  if (MB_SUCCESS == rval) {
    MPI_Info_create (&info);
    MPI_Info_set (info, const_cast<char*>("cb_buffer_size"), const_cast<char*>(cb_size.c_str()));
  }

  dbgOut.set_rank(myPcomm->proc_config().proc_rank());
  dbgOut.limit_output_to_first_N_procs(32);
  Range nonlocal;
  debug_barrier();
  dbgOut.tprint(1, "Gathering interface meshes\n");
  rval = gather_interface_meshes(nonlocal);
  if (MB_SUCCESS != rval)
    return error(rval);

  /**************** Get tag names for sets likely to be shared ***********/
  //debug_barrier();
  //dbgOut.tprint(1, "Getting shared entity sets\n");
  //rval = get_sharedset_tags();
  //if (MB_SUCCESS != rval) return error(rval);

  /**************** Create actual file and write meta info ***************/

  debug_barrier();
  if (myPcomm->proc_config().proc_rank() == 0) {
    dbgOut.tprintf(1, "Creating file: %s\n", filename);

    // Create the file
    const char* type_names[MBMAXTYPE];
    memset(type_names, 0, MBMAXTYPE * sizeof(char*));
    for (EntityType i = MBEDGE; i < MBENTITYSET; ++i)
      type_names[i] = CN::EntityTypeName(i);

    dbgOut.tprint(1, "call mhdf_createFile\n");
    filePtr = mhdf_createFile(filename, overwrite, type_names, MBMAXTYPE, id_type, &status);
    if (!filePtr) {
      MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
    }

    dbgOut.tprint(1, "call write_qa\n");
    rval = write_qa(qa_records);
    if (MB_SUCCESS != rval)
      return error(rval);
  }

  /**************** Create node coordinate table ***************/
  CpuTimer timer;
  debug_barrier();
  dbgOut.tprint(1, "creating node table\n");
  topState.start("creating node table");
  rval = create_node_table(dimension);
  topState.end(rval);
  if (MB_SUCCESS != rval)
    return error(rval);
  if (times)
    times[CREATE_NODE_TIME] = timer.time_elapsed();

  /**************** Create element tables ***************/

  debug_barrier();
  dbgOut.tprint(1, "negotiating element types\n");
  topState.start("negotiating element types");
  rval = negotiate_type_list();
  topState.end(rval);
  if (MB_SUCCESS != rval)
    return error(rval);
  if (times)
    times[NEGOTIATE_TYPES_TIME] = timer.time_elapsed();
  dbgOut.tprint(1, "creating element table\n");
  topState.start("creating element tables");
  rval = create_element_tables();
  topState.end(rval);
  if (MB_SUCCESS != rval)
    return error(rval);
  if (times)
    times[CREATE_ELEM_TIME] = timer.time_elapsed();

  /*************** Exchange file IDs *****************/

  debug_barrier();
  dbgOut.tprint(1, "communicating file ids\n");
  topState.start("communicating file ids");
  rval = exchange_file_ids(nonlocal);
  topState.end(rval);
  if (MB_SUCCESS != rval)
    return error(rval);
  if (times)
    times[FILEID_EXCHANGE_TIME] = timer.time_elapsed();

  /**************** Create meshset tables *********************/

  debug_barrier();
  dbgOut.tprint(1, "creating meshset table\n");
  topState.start("creating meshset tables");
  rval = create_meshset_tables(times);
  topState.end(rval);
  if (MB_SUCCESS != rval)
    return error(rval);
  if (times)
    times[CREATE_SET_TIME] = timer.time_elapsed();

  /**************** Create adjacency tables *********************/

  debug_barrier();
  dbgOut.tprint(1, "creating adjacency table\n");
  topState.start("creating adjacency tables");
  rval = create_adjacency_tables();
  topState.end(rval);
  if (MB_SUCCESS != rval)
    return error(rval);
  if (times)
    times[CREATE_ADJ_TIME] = timer.time_elapsed();

  /**************** Create tag data *********************/

  debug_barrier();
  dbgOut.tprint(1, "creating tag tables\n");
  topState.start("creating tag tables");
  rval = gather_tags(user_tag_list, user_tag_count);
  if (MB_SUCCESS != rval)
    return error(rval);
  rval = create_tag_tables();
  topState.end(rval);
  if (MB_SUCCESS != rval)
    return error(rval);
  if (times)
    times[CREATE_TAG_TIME] = timer.time_elapsed();

  /************** Close serial file and reopen parallel *****************/

  if (0 == myPcomm->proc_config().proc_rank())
    mhdf_closeFile(filePtr, &status);

  MPI_Barrier(myPcomm->proc_config().proc_comm());
  dbgOut.tprint(1, "(re)opening file in parallel mode\n");
  unsigned long junk;
  hid_t hdf_opt = H5Pcreate(H5P_FILE_ACCESS);
  H5Pset_fapl_mpio(hdf_opt, myPcomm->proc_config().proc_comm(), info);
  filePtr = mhdf_openFileWithOpt(filename, 1, &junk, id_type, hdf_opt, &status);
  H5Pclose(hdf_opt);
  if (!filePtr) {
    MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
  }

  if (collectiveIO) {
    dbgOut.print(1, "USING COLLECTIVE IO\n");
    writeProp = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(writeProp, H5FD_MPIO_COLLECTIVE);
  }

  /* Test if we can use H5S_APPEND when selecting hyperslabs */
  if (MB_SUCCESS != opts.get_null_option("HYPERSLAB_OR") &&
     (MB_SUCCESS == opts.get_null_option("HYPERSLAB_APPEND")
      || HDF5_can_append_hyperslabs())) {
    dbgOut.print(1, "HDF5 library supports H5Sselect_hyperlsab with H5S_SELECT_APPEND\n");
    hslabOp = H5S_SELECT_APPEND;
  }

  dbgOut.tprint(1, "Exiting parallel_create_file\n");
  return MB_SUCCESS;
}

class TagNameCompare {
  Interface* iFace;
  std::string name1, name2;
public:
  TagNameCompare(Interface* iface) : iFace(iface) {}
  bool operator() (const WriteHDF5::TagDesc& t1,
                   const WriteHDF5::TagDesc& t2);
};

bool TagNameCompare::operator() (const WriteHDF5::TagDesc& t1,
                                 const WriteHDF5::TagDesc& t2)
{
  iFace->tag_get_name(t1.tag_id, name1);
  iFace->tag_get_name(t2.tag_id, name2);
  return name1 < name2;
}

struct serial_tag_data {
  TagType storage;
  DataType type;
  int size;
  int name_len;
  int def_val_len;
  char name[sizeof(unsigned long)];

  static size_t pad(size_t len) {
    if (len % sizeof(unsigned long))
      return len + sizeof(unsigned long) - len % sizeof(unsigned long);
    else
      return len;
  }

  static size_t def_val_bytes(int def_val_len, DataType type) {
    switch (type) {
      case MB_TYPE_BIT:     return def_val_len ? 1 : 0;
      case MB_TYPE_OPAQUE:  return def_val_len;
      case MB_TYPE_INTEGER: return def_val_len * sizeof(int);
      case MB_TYPE_DOUBLE:  return def_val_len * sizeof(double);
      case MB_TYPE_HANDLE:  return def_val_len * sizeof(EntityHandle);
    }
    return 0;
  }

  static size_t len(int name_len, int def_val_len, DataType type) {
    return sizeof(serial_tag_data) + pad(name_len + def_val_bytes(def_val_len, type)) - sizeof(unsigned long);
  }
  size_t len() const { return len(name_len, def_val_len, type); }
  void* default_value() { return def_val_len ? name + name_len : 0; }
  const void* default_value() const { return const_cast<serial_tag_data*>(this)->default_value(); }
  void set_default_value(const void* val) { memcpy(default_value(), val, def_val_bytes(def_val_len, type)); }
};

ErrorCode WriteHDF5Parallel::append_serial_tag_data(std::vector<unsigned char>& buffer,
                                                    const WriteHDF5::TagDesc& tag)
{
  ErrorCode rval;

  std::string name;
  rval = iFace->tag_get_name(tag.tag_id, name);
  if (MB_SUCCESS != rval)
    return error(rval);

  // Get name length, including space for null char
  size_t name_len = name.size() + 1;
  if (name_len == 1)
    return MB_SUCCESS; // Skip tags with no name

  DataType data_type;
  rval = iFace->tag_get_data_type(tag.tag_id, data_type);
  if (MB_SUCCESS != rval) return error(rval);

  // Get default value
  int def_val_len;
  const void* def_val;
  if (MB_SUCCESS != iFace->tag_get_default_value(tag.tag_id, def_val, def_val_len)) {
    def_val_len = 0;
    def_val = 0;
  }

  // Allocate struct within buffer
  size_t init_size = buffer.size();
  buffer.resize(init_size + serial_tag_data::len(name_len, def_val_len, data_type));
  serial_tag_data* ptr = reinterpret_cast<serial_tag_data*>(&buffer[init_size]);

  // Populate struct
  rval = iFace->tag_get_type(tag.tag_id, ptr->storage);
  if (MB_SUCCESS != rval)
    return error(rval);
  ptr->type = data_type;
  rval = iFace->tag_get_length(tag.tag_id, ptr->size);
  if (MB_VARIABLE_DATA_LENGTH == rval)
    ptr->size = MB_VARIABLE_LENGTH;
  else if (MB_SUCCESS != rval)
    return error(rval);
  ptr->name_len = name_len;
  Range range;
  memset(ptr->name, 0, ptr->name_len);
  memcpy(ptr->name, name.data(), name.size());
  ptr->def_val_len = def_val_len;
  ptr->set_default_value(def_val);

  return MB_SUCCESS;
}

ErrorCode WriteHDF5Parallel::check_serial_tag_data(const std::vector<unsigned char>& buffer,
                                                   std::vector<TagDesc*>* missing,
                                                   std::vector<TagDesc*>* newlist)
{
  ErrorCode rval;

  // Use 'write_sparse' field as a 'visited' mark
  std::list<TagDesc>::iterator tag_iter;
  if (missing)
    for (tag_iter = tagList.begin(); tag_iter != tagList.end(); ++tag_iter)
      tag_iter->write_sparse = true;

  // Use a set as a temporary for what will ultimately go in
  // newlist because we need to pass back newlist in the order
  // of the tagList member.
  std::set<TagDesc*> newset;

  // Iterate over data from, updating the local list of tags.
  // Be careful to keep tagList sorted such that in the end all
  // procs have the same list in the same order.
  std::vector<unsigned char>::const_iterator diter = buffer.begin();
  tag_iter = tagList.begin();
  while (diter < buffer.end()) {
    // Get struct from buffer
    const serial_tag_data* ptr = reinterpret_cast<const serial_tag_data*>(&*diter);

    // Find local struct for tag
    std::string name(ptr->name);
    std::string n;
    iFace->tag_get_name(tag_iter->tag_id, n); // Second time we've called, so shouldn't fail
    if (n > name) {
      tag_iter = tagList.begin(); // New proc, start search from beginning
    }
    iFace->tag_get_name(tag_iter->tag_id, n);
    while (n < name) {
      ++tag_iter;
      if (tag_iter == tagList.end())
        break;
      iFace->tag_get_name(tag_iter->tag_id, n);
    }
    if (tag_iter == tagList.end() || n != name) { // New tag
      TagDesc newtag;

      if (ptr->size == MB_VARIABLE_LENGTH) 
        rval = iFace->tag_get_handle(name.c_str(), ptr->def_val_len, ptr->type, newtag.tag_id, MB_TAG_VARLEN|MB_TAG_CREAT|ptr->storage, ptr->default_value());
      else
        rval = iFace->tag_get_handle(name.c_str(), ptr->size, ptr->type, newtag.tag_id, MB_TAG_CREAT|ptr->storage, ptr->default_value());
      if (MB_SUCCESS != rval)
        return error(rval);

      newtag.sparse_offset = 0;
      newtag.var_data_offset = 0;
      newtag.write_sparse = false;
      newtag.max_num_ents = 0;
      newtag.max_num_vals = 0;

      tag_iter = tagList.insert(tag_iter, newtag);
      if (newlist)
        newset.insert(&*tag_iter);
    }
    else { // Check that tag is as expected
      DataType type;
      iFace->tag_get_data_type(tag_iter->tag_id, type);
      if (type != ptr->type) {
        MB_SET_ERR(MB_FAILURE, "Processes have inconsistent data type for tag \"" << name << "\"");
      }
      int size;
      iFace->tag_get_length(tag_iter->tag_id, size);
      if (size != ptr->size) {
        MB_SET_ERR(MB_FAILURE, "Processes have inconsistent size for tag \"" <<  name << "\"");
      }
      tag_iter->write_sparse = false;
    }

    // Step to next variable-length struct.
    diter += ptr->len();
  }

  // Now pass back any local tags that weren't in the buffer
  if (missing) {
    for (tag_iter = tagList.begin(); tag_iter != tagList.end(); ++tag_iter) {
      if (tag_iter->write_sparse) {
        tag_iter->write_sparse = false;
        missing->push_back(&*tag_iter);
      }
    }
  }

  // Be careful to populate newlist in the same, sorted, order as tagList
  if (newlist) {
    for (tag_iter = tagList.begin(); tag_iter != tagList.end(); ++tag_iter) 
      if (newset.find(&*tag_iter) != newset.end())
        newlist->push_back(&*tag_iter);
  }

  return MB_SUCCESS;
}

static void set_bit(int position, unsigned char* bytes)
{
  int byte = position / 8;
  int bit = position % 8;
  bytes[byte] |= (((unsigned char)1) << bit);
}

static bool get_bit(int position, const unsigned char* bytes)
{
  int byte = position / 8;
  int bit = position % 8;
  return 0 != (bytes[byte] & (((unsigned char)1) << bit));
}

ErrorCode WriteHDF5Parallel::create_tag_tables()
{
  std::list<TagDesc>::iterator tag_iter;
  ErrorCode rval;
  int err;
  const int rank = myPcomm->proc_config().proc_rank();
  const MPI_Comm comm = myPcomm->proc_config().proc_comm();

  subState.start("negotiating tag list");

  dbgOut.tprint(1, "communicating tag metadata\n");

  dbgOut.printf(2, "Exchanging tag data for %d tags.\n", (int)tagList.size());

  // Sort tagList contents in alphabetical order by tag name
  tagList.sort(TagNameCompare(iFace));

  // Negotiate total list of tags to write

  // Build concatenated list of all tag data
  std::vector<unsigned char> tag_buffer;
  for (tag_iter = tagList.begin(); tag_iter != tagList.end(); ++tag_iter) {
    rval = append_serial_tag_data(tag_buffer, *tag_iter);CHECK_MB(rval);
  }

  // Broadcast list from root to all other procs
  unsigned long size = tag_buffer.size();
  err = MPI_Bcast(&size, 1, MPI_UNSIGNED_LONG, 0, comm);CHECK_MPI(err);
  tag_buffer.resize(size);
  err = MPI_Bcast(&tag_buffer[0], size, MPI_UNSIGNED_CHAR, 0, comm);CHECK_MPI(err);

  // Update local tag list
  std::vector<TagDesc*> missing;
  rval = check_serial_tag_data(tag_buffer, &missing, 0);CHECK_MB(rval);

  // Check if we're done (0->done, 1->more, 2+->error)
  int code, lcode = (MB_SUCCESS != rval) ? rval + 2 : missing.empty() ? 0 : 1;
  err = MPI_Allreduce(&lcode, &code, 1, MPI_INT, MPI_MAX, comm);CHECK_MPI(err);
  if (code > 1) {
    MB_SET_ERR_CONT("Inconsistent tag definitions between procs");
    return error((ErrorCode)(code - 2));
  }

  // If not done...
  if (code) {
    dbgOut.print(1, "Not all procs had same tag definitions, negotiating...\n");

    // Get tags defined on this proc but not on root proc
    tag_buffer.clear();
    for (size_t i = 0; i < missing.size(); ++i) {
      rval = append_serial_tag_data(tag_buffer, *missing[i]);CHECK_MB(rval);
    }

    // Gather extra tag definitions on root processor
    std::vector<int> junk; // don't care how many from each proc
    assert(rank || tag_buffer.empty()); // must be empty on root
    err = my_Gatherv(&tag_buffer[0], tag_buffer.size(),
                     MPI_UNSIGNED_CHAR, tag_buffer, junk, 0, comm);CHECK_MPI(err);

    // Process serialized tag descriptions on root, and
    rval = MB_SUCCESS;
    if (0 == rank) {
      // Process serialized tag descriptions on root, and
      std::vector<TagDesc*> newlist;
      rval = check_serial_tag_data(tag_buffer, 0, &newlist);
      tag_buffer.clear();
      // re-serialize a unique list of new tag definitions
      for (size_t i = 0; MB_SUCCESS == rval && i != newlist.size(); ++i) {
        rval = append_serial_tag_data(tag_buffer, *newlist[i]);CHECK_MB(rval);
      }
    }

    // Broadcast any new tag definitions from root to other procs
    long this_size = tag_buffer.size();
    if (MB_SUCCESS != rval)
      this_size = -rval;
    err = MPI_Bcast(&this_size, 1, MPI_LONG, 0, comm);CHECK_MPI(err);
    if (this_size < 0) {
      MB_SET_ERR_CONT("Inconsistent tag definitions between procs");
      return error((ErrorCode) - this_size);
    }
    tag_buffer.resize(this_size);
    err = MPI_Bcast(&tag_buffer[0], this_size, MPI_UNSIGNED_CHAR, 0, comm);CHECK_MPI(err);

    // Process new tag definitions
    rval = check_serial_tag_data(tag_buffer, 0, 0);CHECK_MB(rval);
  }

  subState.end();
  subState.start("negotiate which element/tag combinations are dense");

  // Figure out for which tag/element combinations we can
  // write dense tag data.

  // Construct a table of bits,
  // where each row of the table corresponds to a tag
  // and each column to an element group.

  // Two extra, because first is nodes and last is sets.
  // (n+7)/8 is ceil(n/8)
  const int bytes_per_tag = (exportList.size() + 9) / 8;
  std::vector<unsigned char> data(bytes_per_tag * tagList.size(), 0);
  std::vector<unsigned char> recv(data.size(), 0);
  unsigned char* iter = &data[0];
  if (writeTagDense && !data.empty()) {
    for (tag_iter = tagList.begin(); tag_iter != tagList.end();
         ++tag_iter, iter += bytes_per_tag) {

      Range tagged;
      rval = get_sparse_tagged_entities(*tag_iter, tagged);CHECK_MB(rval);

      int s;
      if (MB_VARIABLE_DATA_LENGTH == iFace->tag_get_length(tag_iter->tag_id, s))
        continue;

      std::string n;
      iFace->tag_get_name(tag_iter->tag_id, n); // Second time we've called, so shouldn't fail

      // Check if we want to write this tag in dense format even if not
      // all of the entities have a tag value.  The criterion of this
      // is that the tag be dense, have a default value, and have at
      // least 2/3 of the entities tagged.
      bool prefer_dense = false;
      TagType type;
      rval = iFace->tag_get_type(tag_iter->tag_id, type);CHECK_MB(rval);
      if (MB_TAG_DENSE == type) {
        const void* defval = 0;
        rval = iFace->tag_get_default_value(tag_iter->tag_id, defval, s);
        if (MB_SUCCESS == rval)
          prefer_dense = true;
      }

      int i = 0;
      if (check_dense_format_tag(nodeSet, tagged, prefer_dense)) {
        set_bit(i, iter);
        dbgOut.printf(2, "Can write dense data for \"%s\"/Nodes\n", n.c_str());
      }
      std::list<ExportSet>::const_iterator ex_iter = exportList.begin();
      for (++i; ex_iter != exportList.end(); ++i, ++ex_iter) {
        // when writing in parallel, on some partitions, some of these element ranges might be empty
        // so do not turn this tag as sparse, just because of that, leave it dense, if we prefer dense
        if ( (prefer_dense && ex_iter->range.empty() ) ||
            check_dense_format_tag(*ex_iter, tagged, prefer_dense)) {
          set_bit(i, iter);
          dbgOut.printf(2, "Can write dense data for \"%s\"/%s\n", n.c_str(),
            ex_iter->name());
        }
      }
      if (check_dense_format_tag(setSet, tagged, prefer_dense)) {
        set_bit(i, iter);
        dbgOut.printf(2, "Can write dense data for \"%s\"/Sets\n", n.c_str());
      }
    }

    // Do bit-wise AND of list over all processors (only write dense format
    // if all proccesses want dense format for this group of entities).
    err = MPI_Allreduce(&data[0], &recv[0], data.size(), MPI_UNSIGNED_CHAR,
                        MPI_BAND, myPcomm->proc_config().proc_comm());CHECK_MPI(err);
  } // if (writeTagDense)

  // Store initial counts for sparse-formatted tag data.
  // The total number of values to send and receive will be the number of
  // tags plus the number of var-len tags because we need to negotiate
  // offsets into two different tables for the var-len tags.
  std::vector<long> counts;

  // Record dense tag/element combinations
  iter = &recv[0];
  const unsigned char* iter2 = &data[0];
  for (tag_iter = tagList.begin(); tag_iter != tagList.end();
       ++tag_iter, iter += bytes_per_tag, iter2 += bytes_per_tag) {

    Range tagged;
    rval = get_sparse_tagged_entities(*tag_iter, tagged);CHECK_MB(rval);

    std::string n;
    iFace->tag_get_name(tag_iter->tag_id, n); // Second time we've called, so shouldn't fail

    int i = 0;
    if (get_bit(i, iter)) {
      assert(get_bit(i, iter2));
      tag_iter->dense_list.push_back(nodeSet);
      tagged -= nodeSet.range;
      dbgOut.printf(2, "Will write dense data for \"%s\"/Nodes\n", n.c_str());
    }
    std::list<ExportSet>::const_iterator ex_iter = exportList.begin();
    for (++i; ex_iter != exportList.end(); ++i, ++ex_iter) {
      if (get_bit(i, iter)) {
        assert(get_bit(i, iter2));
        tag_iter->dense_list.push_back(*ex_iter);
        dbgOut.printf(2, "WIll write dense data for \"%s\"/%s\n", n.c_str(),
          ex_iter->name());
        tagged -= ex_iter->range;
      }
    }
    if (get_bit(i, iter)) {
      assert(get_bit(i, iter2));
      tag_iter->dense_list.push_back(setSet);
      dbgOut.printf(2, "Will write dense data for \"%s\"/Sets\n", n.c_str());
      tagged -= setSet.range;
    }

    counts.push_back(tagged.size());

    int s;
    if (MB_VARIABLE_DATA_LENGTH == iFace->tag_get_length(tag_iter->tag_id, s)) {
      unsigned long data_len;
      rval = get_tag_data_length(*tag_iter, tagged, data_len);CHECK_MB(rval);
      counts.push_back(data_len);
    }
  }

  subState.end();
  subState.start("Negotiate offsets for sparse tag info");

  std::vector<long> offsets(counts.size()), maxima(counts.size()), totals(counts.size());
  rval = create_dataset(counts.size(), &counts[0], &offsets[0], &maxima[0], &totals[0]);CHECK_MB(rval);

  // Copy values into local structs and if root then create tables
  size_t idx = 0;
  for (tag_iter = tagList.begin(); tag_iter != tagList.end(); ++tag_iter, ++idx) {
    assert(idx < counts.size());
    tag_iter->sparse_offset = offsets[idx];
    tag_iter->max_num_ents = maxima[idx];
    tag_iter->write_sparse = (0 != totals[idx]);
    int s;
    if (MB_VARIABLE_DATA_LENGTH == iFace->tag_get_length(tag_iter->tag_id, s)) {
      ++idx;
      assert(idx < counts.size());
      tag_iter->var_data_offset = offsets[idx];
      tag_iter->max_num_vals = maxima[idx];
    }
    else {
      tag_iter->var_data_offset = 0;
      tag_iter->max_num_vals = 0;
    }
  }

  subState.end();

  // Create tag tables on root process
  if (0 == myPcomm->proc_config().proc_rank()) {
    size_t iidx = 0;
    for (tag_iter = tagList.begin(); tag_iter != tagList.end(); ++tag_iter, ++iidx) {
      assert(iidx < totals.size());
      unsigned long num_ents = totals[iidx];
      unsigned long num_val = 0;
      int s;
      if (MB_VARIABLE_DATA_LENGTH == iFace->tag_get_length(tag_iter->tag_id, s)) {
        ++iidx;
        assert(iidx < totals.size());
        num_val = totals[iidx];
      }
      dbgOut.printf(2, "Writing tag description for tag 0x%lx with %lu values\n",
                       (unsigned long)tag_iter->tag_id, num_val ? num_val : num_ents);

      rval = create_tag(*tag_iter, num_ents, num_val);
      if (MB_SUCCESS != rval)
        return error(rval);
    }
  }

  if (dbgOut.get_verbosity() > 1) {
    dbgOut.printf(2, "Tags: %12s %8s %8s %8s %8s %8s\n", "Name", "Count", "Offset", "Var Off", "Max Ent", "Handle");

    for (tag_iter = tagList.begin(); tag_iter != tagList.end(); ++tag_iter) {
      std::string name;
      iFace->tag_get_name(tag_iter->tag_id, name);
      size_t this_size;
      get_num_sparse_tagged_entities(*tag_iter, this_size);
      dbgOut.printf(2, "%18s %8lu %8lu %8lu %8lu 0x%7lx\n", name.c_str(),
        (unsigned long)this_size,
        (unsigned long)tag_iter->sparse_offset,
        (unsigned long)tag_iter->var_data_offset,
        (unsigned long)tag_iter->max_num_ents,
        (unsigned long)tag_iter->tag_id);
    }
  }

  return MB_SUCCESS;
}

struct DatasetVals {
  long start_id;
  long max_count;
  long total;
};
STATIC_ASSERT(sizeof(DatasetVals) == 3 * sizeof(long));

ErrorCode WriteHDF5Parallel::create_dataset(int num_datasets,
                                            const long* num_owned,
                                            long* offsets_out,
                                            long* max_proc_entities,
                                            long* total_entities,
                                            const DataSetCreator& creator,
                                            ExportSet* groups[],
                                            wid_t* first_ids_out)
{
  int result;
  ErrorCode rval;
  const unsigned rank  = myPcomm->proc_config().proc_rank();
  const unsigned nproc = myPcomm->proc_config().proc_size();
  const MPI_Comm comm  = myPcomm->proc_config().proc_comm();

  // Gather entity counts for each processor on root
  std::vector<long> counts(rank ? 0 : nproc * num_datasets);
  (void)VALGRIND_CHECK_MEM_IS_DEFINED(&num_owned, sizeof(long));
  result = MPI_Gather(const_cast<long*>(num_owned), num_datasets, MPI_LONG, &counts[0], num_datasets, MPI_LONG, 0, comm);CHECK_MPI(result);

  // Create node data in file
  DatasetVals zero_val = {0, 0, 0};
  std::vector<DatasetVals> cumulative(num_datasets, zero_val);
  if (rank == 0) {
    for (unsigned i = 0; i < nproc; i++) {
      const long* proc_data = &counts[i * num_datasets];
      for (int index = 0; index < num_datasets; ++index) {
        cumulative[index].total += proc_data[index];
        if (proc_data[index] > cumulative[index].max_count)
          cumulative[index].max_count = proc_data[index];
      }
    }

    for (int index = 0; index < num_datasets; ++index) {
      if (cumulative[index].total) {
        rval = creator(this,
                       cumulative[index].total,
                       groups ? groups[index] : 0,
                       cumulative[index].start_id);CHECK_MB(rval);
      }
      else {
        cumulative[index].start_id = -1;
      }
    }
  }

  // Send id offset to every proc
  result = MPI_Bcast(&cumulative[0], 3 * num_datasets, MPI_LONG, 0, comm);CHECK_MPI(result);
  for (int index = 0; index < num_datasets; ++index) {
    if (first_ids_out)
      first_ids_out[index] = (wid_t)cumulative[index].start_id;
    max_proc_entities[index] = cumulative[index].max_count;
    total_entities[index] = cumulative[index].total;
  }

  // Convert array of per-process counts to per-process offsets
  if (rank == 0) {
    // Initialize prev_size with data sizes for root process
    std::vector<long> prev_size(counts.begin(), counts.begin() + num_datasets);
    // Root process gets offset zero
    std::fill(counts.begin(), counts.begin() + num_datasets, 0L);
    // For each proc other than this one (root)
    for (unsigned i = 1; i < nproc; ++i) {
      // Get pointer to offsets for previous process in list
      long* prev_data = &counts[(i - 1) * num_datasets];
      // Get pointer to offsets for this process in list
      long* proc_data = &counts[i * num_datasets];
      // For each data set
      for (int j = 0; j < num_datasets; ++j) {
        // Get size of data in dataset from process i
        long mysize = proc_data[j];
        // Offset for process i is offset of previous process plus
        // number of values previous process will write
        proc_data[j] = prev_data[j] + prev_size[j];
        // Store my size, as it is no longer available in 'counts'
        prev_size[j] = mysize;
      }
    }
  }

  // Send each proc it's offset in the table
  if (rank == 0) {
    (void)VALGRIND_CHECK_MEM_IS_DEFINED(&counts[0], num_datasets*nproc*sizeof(long));
  }
  result = MPI_Scatter(&counts[0], num_datasets, MPI_LONG, offsets_out, num_datasets, MPI_LONG, 0, comm);CHECK_MPI(result);

  return MB_SUCCESS;
}

ErrorCode WriteHDF5Parallel::create_node_table(int dimension)
{
  nodeSet.num_nodes = dimension; // Put it here so NodeSetCreator can access it
  struct NodeSetCreator : public DataSetCreator {
    ErrorCode operator()(WriteHDF5* file, long count, const ExportSet* group, long& start_id) const
    { 
      mhdf_Status status;
      hid_t handle = mhdf_createNodeCoords(file->file_ptr(), group->num_nodes, count, &start_id, &status);CHECK_HDFN(status);
      mhdf_closeData(file->file_ptr(), handle, &status);CHECK_HDFN(status);
      return MB_SUCCESS;
    }
  };

  const long count = nodeSet.range.size();
  ExportSet* array[] = { &nodeSet };
  ErrorCode rval = create_dataset(1,
                                  &count,
                                  &nodeSet.offset,
                                  &nodeSet.max_num_ents,
                                  &nodeSet.total_num_ents,
                                  NodeSetCreator(),
                                  array,
                                  &nodeSet.first_id);CHECK_MB(rval);
  return assign_ids(nodeSet.range, nodeSet.first_id + nodeSet.offset);
}

struct elemtype {
  int mbtype;
  int numnode;

  elemtype(int vals[2]) : mbtype(vals[0]), numnode(vals[1]) {}
  elemtype(int t, int n) : mbtype(t), numnode(n) {}

  bool operator==(const elemtype& other) const
  {
    return mbtype == other.mbtype &&
            (mbtype == MBENTITYSET ||
             numnode == other.numnode);
  }
  bool operator<(const elemtype& other) const
  {
    if (mbtype > other.mbtype)
      return false;

    return mbtype < other.mbtype ||
           (mbtype != MBENTITYSET &&
            numnode < other.numnode);
  }
  bool operator!=(const elemtype& other) const
    { return !this->operator==(other); }
};

ErrorCode WriteHDF5Parallel::negotiate_type_list()
{
  int result;
  const MPI_Comm comm = myPcomm->proc_config().proc_comm();

  exportList.sort();
  int num_types = exportList.size();

  // Get list of types on this processor
  typedef std::vector< std::pair<int, int> > typelist;
  typelist my_types(num_types);
  (void)VALGRIND_MAKE_VEC_UNDEFINED(my_types);
  typelist::iterator viter = my_types.begin();
  for (std::list<ExportSet>::iterator eiter = exportList.begin();
       eiter != exportList.end(); ++eiter) {
    viter->first = eiter->type;
    viter->second = eiter->num_nodes; 
    ++viter;
  }

  dbgOut.print(2, "Local Element Types:\n");
  for (viter = my_types.begin(); viter != my_types.end(); ++viter) {
    int type = viter->first;
    int count = viter->second;
    dbgOut.printf(2, "  %s : %d\n", CN::EntityTypeName((EntityType)type), count);
  }

  // Broadcast number of types from root to all nodes
  int num_types0 = num_types;
  result = MPI_Bcast(&num_types0, 1, MPI_INT, 0, comm);CHECK_MPI(result);
  // Broadcast type list from root to all nodes
  typelist root_types(num_types0);
  if (0 == myPcomm->proc_config().proc_rank())
    root_types = my_types;
  result = MPI_Bcast(&root_types[0], 2 * num_types0, MPI_INT, 0, comm);CHECK_MPI(result);

  // Build local list of any types that root did not know about
  typelist non_root_types;
  viter = root_types.begin();
   for (typelist::iterator iter = my_types.begin(); iter != my_types.end(); ++iter) {
    if (viter == root_types.end() || *viter != *iter)
      non_root_types.push_back(*iter);
    else
      ++viter;
  }

  // Determine if any process had types not defined on the root
  int non_root_count = non_root_types.size();
  int not_done;
  result = MPI_Allreduce(&non_root_count, &not_done, 1, MPI_INT, MPI_LOR, comm);CHECK_MPI(result);
  if (not_done) {
    // Get number of types each processor has that root does not
    std::vector<int> counts(myPcomm->proc_config().proc_size());
    int two_count = 2*non_root_count;
    result = MPI_Gather(&two_count, 1, MPI_INT, &counts[0], 1, MPI_INT, 0, comm);CHECK_MPI(result);

    // Get list of types from each processor
    std::vector<int> displs(myPcomm->proc_config().proc_size() + 1);
    (void)VALGRIND_MAKE_VEC_UNDEFINED(displs);
    displs[0] = 0;
    for (unsigned long i = 1; i <= myPcomm->proc_config().proc_size(); ++i)
      displs[i] = displs[i - 1] + counts[i - 1];
    int total = displs[myPcomm->proc_config().proc_size()];
    typelist alltypes(total/2);
    (void)VALGRIND_MAKE_VEC_UNDEFINED(alltypes);
    (void)VALGRIND_CHECK_MEM_IS_DEFINED(&non_root_types[0], non_root_types.size()*sizeof(int));
    result = MPI_Gatherv(&non_root_types[0], 2 * non_root_count, MPI_INT,
                         &alltypes[0], &counts[0], &displs[0], MPI_INT, 0, comm);CHECK_MPI(result);

    // Merge type lists.
    // Prefer O(n) insertions with O(ln n) search time because
    // we expect data from a potentially large number of processes,
    // but with a small total number of element types.
    if (0 == myPcomm->proc_config().proc_rank()) {
      for (viter = alltypes.begin(); viter != alltypes.end(); ++viter) {
        typelist::iterator titer = std::lower_bound(my_types.begin(), my_types.end(), *viter);
        if (titer == my_types.end() || *titer != *viter)
          my_types.insert(titer, *viter);
      }

      dbgOut.print(2, "Global Element Types:\n");
      for (viter = my_types.begin(); viter != my_types.end(); ++viter)
        dbgOut.printf(2, "  %s : %d\n", CN::EntityTypeName((EntityType)viter->first), viter->second);
    }

    // Send total number of types to each processor
    total = my_types.size();
    result = MPI_Bcast(&total, 1, MPI_INT, 0, comm);CHECK_MPI(result);

    // Send list of types to each processor
    my_types.resize(total);
    result = MPI_Bcast(&my_types[0], 2 * total, MPI_INT, 0, comm);CHECK_MPI(result);
  }
  else {
    // Special case: if root had types but some subset of procs did not
    // have those types, but there are no types that the root doesn't
    // know about then we still need to update processes that are missing
    // types.
    my_types.swap(root_types);
  }

  // Insert missing types into exportList, with an empty
  // range of entities to export.
  std::list<ExportSet>::iterator ex_iter = exportList.begin();
  for (viter = my_types.begin(); viter != my_types.end(); ++viter) {
    while (ex_iter != exportList.end() && *ex_iter < *viter)
      ++ex_iter;

    if (ex_iter == exportList.end() || !(*ex_iter == *viter)) {
      ExportSet insert;
      insert.type = (EntityType)viter->first;
      insert.num_nodes = viter->second;
      insert.first_id = 0;
      insert.offset = 0;
      insert.adj_offset = 0;
      ex_iter = exportList.insert(ex_iter, insert);
    }
  }

  return MB_SUCCESS;
}

ErrorCode WriteHDF5Parallel::create_element_tables()
{
  struct ElemSetCreator : public DataSetCreator {
    ErrorCode operator()(WriteHDF5* file, long size, const ExportSet* ex, long& start_id) const
      { return file->create_elem_table(*ex, size, start_id); }
  };

  const int numtypes = exportList.size();
  std::vector<ExportSet*> groups(numtypes);
  std::vector<long> counts(numtypes), offsets(numtypes), max_ents(numtypes), total_ents(numtypes);
  std::vector<wid_t> start_ids(numtypes);

  size_t idx = 0;
  std::list<ExportSet>::iterator ex_iter;
  for (ex_iter = exportList.begin(); ex_iter != exportList.end(); ++ex_iter, ++idx) {
    groups[idx] = &*ex_iter;
    counts[idx] = ex_iter->range.size();
  }
  ErrorCode rval = create_dataset(numtypes,
                                  &counts[0],
                                  &offsets[0],
                                  &max_ents[0],
                                  &total_ents[0],
                                  ElemSetCreator(),
                                  &groups[0],
                                  &start_ids[0]);CHECK_MB(rval);

  for (idx = 0, ex_iter = exportList.begin(); ex_iter != exportList.end(); ++ex_iter, ++idx) {
    ex_iter->first_id = start_ids[idx];
    ex_iter->offset = offsets[idx];
    ex_iter->max_num_ents = max_ents[idx];
    ex_iter->total_num_ents = total_ents[idx];
    rval = assign_ids(ex_iter->range, ex_iter->first_id + ex_iter->offset);CHECK_MB(rval);
  }

  return MB_SUCCESS;
}

ErrorCode WriteHDF5Parallel::create_adjacency_tables()
{
  struct AdjSetCreator : public DataSetCreator {
    ErrorCode operator()(WriteHDF5* file, long size, const ExportSet* ex, long& start_id) const
    {
      mhdf_Status status;
      hid_t handle = mhdf_createAdjacency(file->file_ptr(),
                                          ex->name(),
                                          size,
                                          &status);CHECK_HDFN(status);
      mhdf_closeData(file->file_ptr(), handle, &status);CHECK_HDFN(status);
      start_id = -1;
      return MB_SUCCESS;
    }
  };

  std::vector<ExportSet*> groups;
#ifdef WRITE_NODE_ADJACENCIES
  groups.push_back(&nodeSet);
#endif
  for (std::list<ExportSet>::iterator ex_iter = exportList.begin();
       ex_iter != exportList.end(); ++ex_iter)
    groups.push_back(&*ex_iter);

  ErrorCode rval;
  const int numtypes = groups.size();
  std::vector<long> counts(numtypes);
  std::vector<long> offsets(numtypes);
  std::vector<long> max_ents(numtypes);
  std::vector<long> totals(numtypes);
  for (int i = 0; i < numtypes; ++i) {
    wid_t count;
    rval = count_adjacencies(groups[i]->range, count);CHECK_MB(rval);
    counts[i] = count;
  }

  rval = create_dataset(numtypes,
                        &counts[0],
                        &offsets[0],
                        &max_ents[0],
                        &totals[0],
                        AdjSetCreator(),
                        &groups[0]);CHECK_MB(rval);

  // Cppcheck warning (false positive): variable groups is assigned a value that is never used
  for (int i = 0; i < numtypes; ++i) {
    groups[i]->max_num_adjs = max_ents[i];
    groups[i]->adj_offset = offsets[i];
  }
  return MB_SUCCESS;
}

const unsigned SSVB = 3;

void WriteHDF5Parallel::print_set_sharing_data(const Range& range, const char* label, Tag idt)
{
  dbgOut.printf(SSVB, "set\tid\towner\t%-*s\tfid\tshared\n",(int)(sizeof(EntityHandle)*2),"handle");
  for (Range::iterator it = range.begin(); it != range.end(); ++it) {
    int id;
    iFace->tag_get_data(idt, &*it, 1, &id);
    EntityHandle handle = 0;
    unsigned owner = 0;
    wid_t file_id = 0;
    myPcomm->get_entityset_owner(*it, owner, &handle);
    if (!idMap.find(*it, file_id))
      file_id = 0;
    dbgOut.printf(SSVB, "%s\t%d\t%u\t%lx\t%lu\t", label, id, owner, (unsigned long)handle, (unsigned long)file_id);
    std::vector<unsigned> procs;
    myPcomm->get_entityset_procs(*it, procs);
    if (procs.empty())
      dbgOut.print(SSVB, "<none>\n");
    else {
      for (unsigned i = 0; i < procs.size() - 1; ++i)
        dbgOut.printf(SSVB, "%u,", procs[i]);
      dbgOut.printf(SSVB, "%u\n", procs.back());
    }
  }
}

void WriteHDF5Parallel::print_shared_sets()
{
  const char* tag_names[][2] = { { MATERIAL_SET_TAG_NAME, "block" },
                                 { DIRICHLET_SET_TAG_NAME, "nodeset" },
                                 { NEUMANN_SET_TAG_NAME, "sideset" },
                                 { 0, 0 } };

  for (int i = 0; tag_names[i][0]; ++i) {
    Tag tag;
    if (MB_SUCCESS != iFace->tag_get_handle(tag_names[i][0], 1, MB_TYPE_INTEGER, tag))
      continue;

    Range tagged;
    iFace->get_entities_by_type_and_tag(0, MBENTITYSET, &tag, 0, 1, tagged);
    print_set_sharing_data(tagged, tag_names[i][1], tag);
  }

  Tag geom, id;
  if (MB_SUCCESS != iFace->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geom))
    return;
  if (MB_SUCCESS != iFace->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, id))
    return;

  const char* geom_names[] = { "vertex", "curve", "surface", "volume" };
  for (int d = 0; d <= 3; ++d) {
    Range tagged;
    const void* vals[] = { &d };
    iFace->get_entities_by_type_and_tag(0, MBENTITYSET, &geom, vals, 1, tagged);
    print_set_sharing_data(tagged, geom_names[d], id);
  }
}

ErrorCode WriteHDF5Parallel::communicate_shared_set_ids(const Range& owned,
                                                        const Range& remote)
{
  ErrorCode rval;
  int mperr;
  const int TAG = 0xDEADF00;
  //const unsigned rank = myPcomm->proc_config().proc_rank();
  const MPI_Comm comm = myPcomm->proc_config().proc_comm();

  dbgOut.tprint(1, "COMMUNICATING SHARED SET IDS\n");
  dbgOut.print(6, "Owned, shared sets: ", owned);

  // Post receive buffers for all procs for which we share sets

  std::vector<unsigned> procs;
  rval = myPcomm->get_entityset_owners(procs);CHECK_MB(rval);
  std::vector<unsigned>::iterator it = std::find(procs.begin(), procs.end(),
                                            myPcomm->proc_config().proc_rank());
  if (it != procs.end())
    procs.erase(it);

  std::vector<MPI_Request> recv_req(procs.size(), MPI_REQUEST_NULL);
  std::vector< std::vector<unsigned long> > recv_buf(procs.size());

  size_t recv_count = 0;
  for (size_t i = 0; i < procs.size(); ++i) {
    Range tmp;
    rval = myPcomm->get_owned_sets(procs[i], tmp);CHECK_MB(rval);
    size_t count = intersect(tmp, remote).size(); // Necessary because we might not be writing all of the database
    if (count) {
      dbgOut.printf(6, "Sets owned by proc %u (remote handles): ", procs[i]);
      if (dbgOut.get_verbosity() >= 6) {
        Range remote_handles;
        tmp = intersect(tmp, remote);
        for (Range::iterator j = tmp.begin(); j != tmp.end(); ++j) {
          unsigned r;
          EntityHandle h;
          myPcomm->get_entityset_owner(*j, r, &h);
          assert(r == procs[i]);
          remote_handles.insert(h);
        }
        dbgOut.print(6, remote_handles);
      }
      recv_count++;
      recv_buf[i].resize(2*count + 1);
      dbgOut.printf(5, "Posting receive buffer of size %lu for proc %u (%lu of %lu owned sets)\n",
                      (unsigned long)recv_buf[i].size(), procs[i], count, tmp.size());
      mperr = MPI_Irecv(&recv_buf[i][0], recv_buf[i].size(), MPI_UNSIGNED_LONG,
                        procs[i], TAG, comm, &recv_req[i]);CHECK_MPI(mperr);
    }
  }

  // Send set ids to all procs with which we share them

  // First build per-process lists of sets for which we need to send data
  std::map<unsigned, Range> send_sets;
  std::vector<unsigned> set_procs;
  for (Range::reverse_iterator i = owned.rbegin(); i != owned.rend(); ++i) {
    set_procs.clear();
    rval = myPcomm->get_entityset_procs(*i, set_procs);CHECK_MB(rval);
    for (size_t j = 0; j < set_procs.size(); ++j)
      if (set_procs[j] != myPcomm->proc_config().proc_rank())
        send_sets[set_procs[j]].insert(*i);
  }
  assert(send_sets.find(myPcomm->proc_config().proc_rank()) == send_sets.end());

  // Now send the data
  std::vector< std::vector<unsigned long> > send_buf(send_sets.size());
  std::vector< MPI_Request > send_req(send_sets.size());
  std::map<unsigned, Range>::iterator si = send_sets.begin();
  for (size_t i = 0; si != send_sets.end(); ++si, ++i) {
    dbgOut.printf(6, "Sending data for shared sets to proc %u: ", si->first);
    dbgOut.print(6, si->second);

    send_buf[i].reserve(2*si->second.size() + 1);
    send_buf[i].push_back(si->second.size());
    for (Range::iterator j = si->second.begin(); j != si->second.end(); ++j) {
      send_buf[i].push_back(*j);
      send_buf[i].push_back(idMap.find(*j));
    }
    dbgOut.printf(5, "Sending buffer of size %lu to proc %u (%lu of %lu owned sets)\n",
                     (unsigned long)send_buf[i].size(), si->first, si->second.size(), owned.size());
    mperr = MPI_Isend(&send_buf[i][0], send_buf[i].size(), MPI_UNSIGNED_LONG,
                      si->first, TAG, comm, &send_req[i]);
  }

  // Process received data
  MPI_Status status;
  int idx;
  while (recv_count--) {
    mperr = MPI_Waitany(recv_req.size(), &recv_req[0], &idx, &status);CHECK_MPI(mperr);

    assert((unsigned)status.MPI_SOURCE == procs[idx]);
    assert(2*recv_buf[idx].front() + 1 == recv_buf[idx].size());
    const size_t n = std::min<size_t>(recv_buf[idx].front(), (recv_buf[idx].size() - 1) / 2);
    dbgOut.printf(5, "Received buffer of size %lu from proc %d\n",
                     (unsigned long)(2*n + 1), (int)status.MPI_SOURCE);

    for (size_t i = 0; i < n; ++i) {
      EntityHandle handle = 0;
      rval = myPcomm->get_entityset_local_handle(procs[idx], recv_buf[idx][2*i + 1], handle);CHECK_MB(rval);
      assert(handle != 0);
      if (!idMap.insert(handle, recv_buf[idx][2*i + 2], 1).second)
        error(MB_FAILURE); // Conflicting IDs??????
    }

    recv_req[idx] = MPI_REQUEST_NULL;
  }
  assert(MPI_SUCCESS == MPI_Waitany(recv_req.size(), &recv_req[0], &idx, &status)
      && MPI_UNDEFINED == idx); // Check that we got them all

  // Wait for all sends to complete before we release send
  // buffers (implicitly releases when we return from this function)

  std::vector<MPI_Status> stats(send_req.size());
  mperr = MPI_Waitall(send_req.size(), &send_req[0], &stats[0]);CHECK_MPI(mperr);

  if (dbgOut.get_verbosity() >= SSVB)
    print_shared_sets();

  return MB_SUCCESS;  
}

//void get_global_ids(Interface* iFace, const unsigned long* ptr,
//                    size_t len, unsigned flags,
//                    std::vector<int>& ids)
//{
//  Tag idtag;
//  iFace->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, idtag);
//  for (size_t i = 0; i < len; ++i) {
//    if (flags & MESHSET_ORDERED) {
//      int tmp;
//      iFace->tag_get_data(idtag, ptr + i, 1, &tmp);
//      ids.push_back(tmp);
//      continue;
//    }
//
//    EntityHandle s = ptr[i];
//    EntityHandle e = ptr[++i];
//    for (; s <= e; ++s) {
//      int tmp;
//      iFace->tag_get_data(idtag, &s, 1, &tmp);
//      ids.push_back(tmp);
//    }
//  }
//}

ErrorCode WriteHDF5Parallel::pack_set(Range::const_iterator it,
                                      unsigned long* buffer,
                                      size_t buffer_size)
{
  ErrorCode rval;
  const EntityHandle* ptr;
  int len;
  unsigned char flags;
  std::vector<wid_t> tmp;
  size_t newlen;

  // Buffer must always contain at least flags and desired sizes
  assert(buffer_size >= 4);
  buffer_size -= 4;

  Range::const_iterator nd = it; ++nd;
  rval = writeUtil->get_entity_list_pointers(it, nd, &ptr, WriteUtilIface::CONTENTS, &len, &flags);CHECK_MB(rval);

//Tag mattag;
//iFace->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER, mattag);
//int block;
//if (MB_SUCCESS != iFace->tag_get_data(mattag, &*it, 1, &block))
//  block = 0;
//
//if (block) {
//  std::vector<int> ids;
//  get_global_ids(iFace, ptr, len, flags, ids);
//}

  if (len && !(flags & MESHSET_ORDERED)) {
    tmp.clear();
    bool blocked = false;
    assert(0 == len % 2);
    rval = range_to_blocked_list(ptr, len / 2, tmp, blocked);CHECK_MB(rval);
    if (blocked)
      flags |= mhdf_SET_RANGE_BIT;
  }
  else {
    tmp.resize(len);
    rval = vector_to_id_list(ptr, len, &tmp[0], newlen, true);CHECK_MB(rval);
    tmp.resize(newlen);
  }

  buffer[0] = flags;
  buffer[1] = tmp.size();
  if (tmp.size() <= buffer_size)
    std::copy(tmp.begin(), tmp.end(), buffer + 4);

  rval = writeUtil->get_entity_list_pointers(it, nd, &ptr, WriteUtilIface::CHILDREN, &len);CHECK_MB(rval);
  tmp.resize(len);
  rval = vector_to_id_list(ptr, len, &tmp[0], newlen, true);
  tmp.resize(newlen);
  buffer[2] = tmp.size();
  if (tmp.size() <= buffer_size - buffer[1])
    std::copy(tmp.begin(), tmp.end(), buffer + 4 + buffer[1]);

  rval = writeUtil->get_entity_list_pointers(it, nd, &ptr, WriteUtilIface::PARENTS, &len);CHECK_MB(rval);
  tmp.resize(len);
  rval = vector_to_id_list(ptr, len, &tmp[0], newlen, true);
  tmp.resize(newlen);
  buffer[3] = tmp.size();
  if (tmp.size() <= buffer_size - buffer[1] - buffer[2])
    std::copy(tmp.begin(), tmp.end(), buffer + 4 + buffer[1] + buffer[2]);

  return MB_SUCCESS;
}

template<typename TYPE>
static void convert_to_ranged_ids(const TYPE* buffer,
                                  size_t len,
                                  std::vector<WriteHDF5::wid_t>& result)
{
  if (!len) {
    result.clear();
    return;
  }

  result.resize(len * 2);
  Range tmp;
  for (size_t i = 0; i < len; i++)
    tmp.insert((EntityHandle)buffer[i]);
  result.resize(tmp.psize() * 2);
  int j = 0;
  for (Range::const_pair_iterator pit = tmp.const_pair_begin();
      pit!=tmp.const_pair_end(); ++pit, j++) {
    result[2*j] = pit->first;
    result[2*j + 1] = pit->second-pit->first + 1;
  }
}

static void merge_ranged_ids(const unsigned long* range_list,
                             size_t len,
                             std::vector<WriteHDF5::wid_t>& result)
{
  typedef WriteHDF5::wid_t wid_t;
  assert(0 == len%2);
  assert(0 == result.size()%2);
  STATIC_ASSERT(sizeof(std::pair<wid_t, wid_t>) == 2 * sizeof(wid_t));

  result.insert(result.end(), range_list, range_list + len);
  size_t plen = result.size() / 2;
  Range tmp;
  for (size_t i = 0; i < plen; i++) {
    EntityHandle starth = (EntityHandle)result[2 * i];
    EntityHandle endh = (EntityHandle)result[2 * i] + (wid_t)result[2*i + 1] - 1; // id + count - 1
    tmp.insert(starth, endh);
  }
  // Now convert back to std::vector<WriteHDF5::wid_t>, compressed range format
  result.resize(tmp.psize() * 2);
  int j = 0;
  for (Range::const_pair_iterator pit = tmp.const_pair_begin();
      pit != tmp.const_pair_end(); ++pit, j++) {
    result[2*j] = pit->first;
    result[2*j + 1] = pit->second-pit->first + 1;
  }
}

static void merge_vector_ids(const unsigned long* list,
                             size_t len,
                             std::vector<WriteHDF5::wid_t>& result)
{
  result.insert(result.end(), list, list + len);
}

ErrorCode WriteHDF5Parallel::unpack_set(EntityHandle set,
                                        const unsigned long* buffer,
                                        size_t buffer_size)
{
  // Use local variables for readability
  assert(buffer_size >= 4);
  assert(buffer[1] + buffer[2] + buffer[3] <= buffer_size);
  const unsigned long flags = buffer[0];
  unsigned long num_content = buffer[1];
  const unsigned long num_child  = buffer[2];
  const unsigned long num_parent = buffer[3];
  const unsigned long* contents = buffer + 4;
  const unsigned long* children = contents + num_content;
  const unsigned long* parents  = children + num_child;

  SpecialSetData* data = find_set_data(set);
  assert(NULL != data);
  if (NULL == data)
    return MB_FAILURE;

//Tag mattag;
//iFace->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER, mattag);
//int block;
//if (MB_SUCCESS != iFace->tag_get_data(mattag, &set, 1, &block))
//  block = 0;

  // If either the current data or the new data is in ranged format,
  // then change the other to ranged format if it isn't already
  // in both cases when they differ, the data will end up "compressed range"
  std::vector<wid_t> tmp;
  if ((flags & mhdf_SET_RANGE_BIT) != (data->setFlags & mhdf_SET_RANGE_BIT)) {
    if (flags & mhdf_SET_RANGE_BIT) {
      tmp = data->contentIds;
      convert_to_ranged_ids(&tmp[0], tmp.size(), data->contentIds);
      data->setFlags |= mhdf_SET_RANGE_BIT;
    }
    else {
      tmp.clear();
      convert_to_ranged_ids(contents, num_content, tmp);
      num_content = tmp.size();
      if (sizeof(wid_t) < sizeof(long)) {
        size_t old_size = tmp.size();
        tmp.resize(sizeof(long) * old_size / sizeof(wid_t));
        unsigned long* array = reinterpret_cast<unsigned long*>(&tmp[0]);
        for (long i = ((long)old_size) - 1; i >= 0; --i)
          array[i] = tmp[i];
        contents = array;
      }
      else if (sizeof(wid_t) > sizeof(long)) {
        unsigned long* array = reinterpret_cast<unsigned long*>(&tmp[0]);
        std::copy(tmp.begin(), tmp.end(), array);
      }
      contents = reinterpret_cast<unsigned long*>(&tmp[0]);
    }
  }

  if (data->setFlags & mhdf_SET_RANGE_BIT)
    merge_ranged_ids(contents, num_content, data->contentIds);
  else
    merge_vector_ids(contents, num_content, data->contentIds);

  merge_vector_ids(children, num_child,  data->childIds);
  merge_vector_ids(parents,  num_parent, data->parentIds);
  return MB_SUCCESS;
}

ErrorCode WriteHDF5Parallel::communicate_shared_set_data(const Range& owned,
                                                         const Range& remote)
{
  ErrorCode rval;
  int mperr;
  const unsigned rank = myPcomm->proc_config().proc_rank();
  const MPI_Comm comm = myPcomm->proc_config().proc_comm();

  dbgOut.tprintf(1, "COMMUNICATING SHARED SET DATA (%lu owned & %lu remote)\n",
                    (unsigned long)owned.size(), (unsigned long)remote.size());

  // Calculate the total number of messages to be in transit (send and receive)
  size_t nummess = 0;
  std::vector<unsigned> procs;;
  Range shared(owned);
  shared.merge(remote);
  for (Range::iterator i = shared.begin(); i != shared.end(); ++i) {
    procs.clear();
    rval = myPcomm->get_entityset_procs(*i, procs);CHECK_MB(rval);
    nummess += procs.size(); 
  }

  // Choose a receive buffer size. We need 4*sizeof(long) minimum,
  // but that is almost useless so use 16*sizeof(long) as the minimum
  // instead. Choose an upper limit such that we don't exceed 32 MB
  // of allocated memory (unless we absolutely must to meet the minimum.)
  // Also, don't initially choose buffers larger than 128*sizeof(long).
  const size_t MAX_BUFFER_MEM = 32 * 1024 * 1024 / sizeof(long);
  //const size_t INIT_BUFFER_SIZE = 128;
  const size_t INIT_BUFFER_SIZE = 1024;
  const size_t MIN_BUFFER_SIZE = 16;
  size_t init_buff_size = INIT_BUFFER_SIZE;
  if (init_buff_size * nummess > MAX_BUFFER_MEM)
    init_buff_size = MAX_BUFFER_MEM / nummess;
  if (init_buff_size < MIN_BUFFER_SIZE)
    init_buff_size = MIN_BUFFER_SIZE;

  dbgOut.printf(2, "Using buffer size of %lu for an expected message count of %lu\n",
                   (unsigned long)init_buff_size, (unsigned long)nummess);

  // Count number of recvs
  size_t numrecv = 0;
  for (Range::iterator i = owned.begin(); i != owned.end(); ++i) {
    procs.clear();
    rval = myPcomm->get_entityset_procs(*i, procs);CHECK_MB(rval);
    numrecv += procs.size();
    if (std::find(procs.begin(), procs.end(), rank) != procs.end())
      --numrecv;
  }

  // Post receive buffers for all owned sets for all sharing procs
  std::vector<MPI_Request> recv_req(numrecv, MPI_REQUEST_NULL);
  std::vector<MPI_Request> lrecv_req(numrecv, MPI_REQUEST_NULL);

  std::vector< std::vector<unsigned long> > recv_buf(numrecv, std::vector<unsigned long>(init_buff_size));
  int idx = 0;
  for (Range::iterator i = owned.begin(); i != owned.end(); ++i) {
    procs.clear();
    rval = myPcomm->get_entityset_procs(*i, procs);CHECK_MB(rval);
    for (size_t j = 0; j < procs.size(); ++j) {
      if (procs[j] == rank)
        continue;
      int tag = ID_FROM_HANDLE(*i);
      if (*i != CREATE_HANDLE(MBENTITYSET, tag)) {
#ifndef NDEBUG
        abort();
#endif
        CHECK_MB(MB_FAILURE);
      }
      dbgOut.printf(5, "Posting buffer to receive set %d from proc %u\n", tag, procs[j]);
      mperr = MPI_Irecv(&recv_buf[idx][0], init_buff_size, MPI_UNSIGNED_LONG,
                        procs[j], tag, comm, &recv_req[idx]);CHECK_MPI(mperr);
      ++idx;
    }
  }
  assert((size_t)idx == numrecv);

  // Now send set data for all remote sets that I know about
  std::vector<MPI_Request> send_req(remote.size());
  std::vector< std::vector<unsigned long> > send_buf(remote.size());
  idx = 0;
  for (Range::iterator i = remote.begin(); i != remote.end(); ++i, ++idx) {
    send_buf[idx].resize(init_buff_size);
    rval = pack_set(i, &send_buf[idx][0], init_buff_size);CHECK_MB(rval);
    EntityHandle remote_handle;
    unsigned owner;
    rval = myPcomm->get_entityset_owner(*i, owner, &remote_handle);CHECK_MB(rval);

    int tag = ID_FROM_HANDLE(remote_handle);
    assert(remote_handle == CREATE_HANDLE(MBENTITYSET, tag));
    dbgOut.printf(5, "Sending %lu values for set %d to proc %u\n",
                     send_buf[idx][1] + send_buf[idx][2] + send_buf[idx][3] + 4, tag, owner);
    mperr = MPI_Isend(&send_buf[idx][0], init_buff_size, MPI_UNSIGNED_LONG,
                      owner, tag, comm, &send_req[idx]);CHECK_MPI(mperr);
  }

//Tag mattag;
//iFace->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER, mattag);

  // Now initialize local data for managing contents of owned, shared sets
  assert(specialSets.empty());
  specialSets.clear();
  specialSets.reserve(owned.size());
  for (Range::iterator i = owned.begin(); i != owned.end(); ++i) {
    //int block;
    //if (MB_SUCCESS != iFace->tag_get_data(mattag, &*i, 1, &block))
    //  block = 0;
    //std::vector<int> ids;

    SpecialSetData data;
    data.setHandle = *i;
    rval = iFace->get_meshset_options(*i, data.setFlags);CHECK_MB(rval);
    specialSets.push_back(data);
    std::vector<EntityHandle> list;
    if (data.setFlags & MESHSET_ORDERED) {
      list.clear();
      rval = iFace->get_entities_by_handle(*i, list);CHECK_MB(rval);
      rval = vector_to_id_list(list, specialSets.back().contentIds, true);CHECK_MB(rval);
      //if (block)
      //  get_global_ids(iFace, &list[0], list.size(), MESHSET_ORDERED, ids);
    }
    else {
      Range range;
      rval = iFace->get_entities_by_handle(*i, range);CHECK_MB(rval);
      bool ranged;
      rval = range_to_blocked_list(range, specialSets.back().contentIds, ranged);
      if (ranged)
       specialSets.back().setFlags |= mhdf_SET_RANGE_BIT;
    //if (block) {
    //  std::vector<EntityHandle> tmp;
    //  for (Range::const_pair_iterator pi = range.const_pair_begin(); pi != range.const_pair_end(); ++pi) {
    //    tmp.push_back(pi->first);
    //    tmp.push_back(pi->second);
    //  }
    //  get_global_ids(iFace, &tmp[0], tmp.size(), ranged ? 0 : MESHSET_ORDERED, ids);
    //}
    }

    list.clear();
    rval = iFace->get_parent_meshsets(*i, list);CHECK_MB(rval);
    rval = vector_to_id_list(list, specialSets.back().parentIds, true);CHECK_MB(rval);
    rval = iFace->get_child_meshsets(*i, list);CHECK_MB(rval);
    rval = vector_to_id_list(list, specialSets.back().childIds, true);CHECK_MB(rval);
  }

  // Process received buffers, repost larger buffers where necessary
  size_t remaining = numrecv;
  numrecv = 0;
  while (remaining--) {
    std::vector<unsigned long> dead;
    MPI_Status status;
    mperr = MPI_Waitany(recv_req.size(), &recv_req[0], &idx, &status);CHECK_MPI(mperr);
    EntityHandle handle = CREATE_HANDLE(MBENTITYSET, status.MPI_TAG);
    std::vector<unsigned long>& buff = recv_buf[idx];
    size_t size = buff[1] + buff[2] + buff[3] + 4;
    dbgOut.printf(5, "Received %lu values for set %d from proc %d\n",
                     (unsigned long)size, status.MPI_TAG, status.MPI_SOURCE);
    if (size <= init_buff_size) {
      rval = unpack_set(handle, &buff[0], init_buff_size);CHECK_MB(rval);
      dead.swap(buff); // Release memory
    }
    else {
      // Data was too big for init_buff_size
      // repost with larger buffer
      buff.resize(size);
      dbgOut.printf(5, "Re-Posting buffer to receive set %d from proc %d with size %lu\n",
                       status.MPI_TAG, status.MPI_SOURCE, (unsigned long)size);
      mperr = MPI_Irecv(&buff[0], size, MPI_UNSIGNED_LONG, status.MPI_SOURCE,
                        status.MPI_TAG, comm, &lrecv_req[idx]);CHECK_MPI(mperr);
      ++numrecv;
    } 
    recv_req[idx] = MPI_REQUEST_NULL;
  }

  // Wait for sends to complete
  MPI_Waitall(send_req.size(), &send_req[0], MPI_STATUSES_IGNORE);

  // Re-send sets that didn't fit initial buffer size
  idx = 0;
  for (Range::iterator i = remote.begin(); i != remote.end(); ++i, ++idx) {
    std::vector<unsigned long>& buff = send_buf[idx];
    size_t size = buff[1] + buff[2] + buff[3] + 4;
    if (size <= init_buff_size)
      continue;

    buff.resize(size);
    rval = pack_set(i, &buff[0], size);CHECK_MB(rval);
    EntityHandle remote_handle;
    unsigned owner;
    rval = myPcomm->get_entityset_owner(*i, owner, &remote_handle);CHECK_MB(rval);

    int tag = ID_FROM_HANDLE(remote_handle);
    assert(remote_handle == CREATE_HANDLE(MBENTITYSET, tag));
    dbgOut.printf(5, "Sending %lu values for set %d to proc %u\n",
                     (unsigned long)size, tag, owner);
    mperr = MPI_Isend(&buff[0], size, MPI_UNSIGNED_LONG,
                      owner, tag, comm, &send_req[idx]);CHECK_MPI(mperr);
  }

  // Process received buffers
  remaining = numrecv;
  while (remaining--) {
    std::vector<unsigned long> dead;
    MPI_Status status;
    mperr = MPI_Waitany(lrecv_req.size(), &lrecv_req[0], &idx, &status);CHECK_MPI(mperr);
    EntityHandle handle = CREATE_HANDLE(MBENTITYSET, status.MPI_TAG);
    std::vector<unsigned long>& buff = recv_buf[idx];
    dbgOut.printf(5, "Received %lu values for set %d from proc %d\n",
                     4 + buff[1] + buff[2] + buff[3], status.MPI_TAG, status.MPI_SOURCE);
    rval = unpack_set(handle, &buff[0], buff.size());CHECK_MB(rval);
    dead.swap(buff); // Release memory

    lrecv_req[idx] = MPI_REQUEST_NULL;
  }

  // Wait for sends to complete
  MPI_Waitall(send_req.size(), &send_req[0], MPI_STATUSES_IGNORE);

  return MB_SUCCESS;
}

ErrorCode WriteHDF5Parallel::create_meshset_tables(double* times)
{
  Range::const_iterator riter;
  const unsigned rank = myPcomm->proc_config().proc_rank();

  START_SERIAL;
  print_type_sets(iFace, &dbgOut, setSet.range);
  END_SERIAL;
  CpuTimer timer;

  // Remove remote sets from setSets
  Range shared, owned, remote;
  ErrorCode rval = myPcomm->get_shared_sets(shared);CHECK_MB(rval);
  shared = intersect(shared, setSet.range);
  rval = myPcomm->get_owned_sets(rank, owned);CHECK_MB(rval);
  owned = intersect(owned, setSet.range);
  remote = subtract(shared, owned);
  setSet.range = subtract(setSet.range, remote);

  // Create set meta table
  struct SetDescCreator : public DataSetCreator {
    ErrorCode operator()(WriteHDF5* writer, long size, const ExportSet*, long& start_id) const
    { return writer->create_set_meta(size, start_id); }
  };
  long count = setSet.range.size();
  rval = create_dataset(1, &count,
                        &setSet.offset,
                        &setSet.max_num_ents,
                        &setSet.total_num_ents,
                        SetDescCreator(), NULL,
                        &setSet.first_id);CHECK_MB(rval);
  writeSets = setSet.max_num_ents > 0;

  rval = assign_ids(setSet.range, setSet.first_id + setSet.offset);CHECK_MB(rval);
  if (times)
    times[SET_OFFSET_TIME] = timer.time_elapsed();

  // Exchange file IDS for sets between all procs
  rval = communicate_shared_set_ids(owned, remote);CHECK_MB(rval);
  if (times)
    times[SHARED_SET_IDS] = timer.time_elapsed();

  // Communicate remote set contents, children, etc.
  rval = communicate_shared_set_data(owned, remote);CHECK_MB(rval);
  if (times)
    times[SHARED_SET_CONTENTS] = timer.time_elapsed();

  // Communicate counts for owned sets
  long data_counts[3]; // { #contents, #children, #parents }
  rval = count_set_size(setSet.range, data_counts[0], data_counts[1], data_counts[2]);CHECK_MB(rval);
  if (times)
    times[SET_OFFSET_TIME] += timer.time_elapsed();

  long offsets[3], max_counts[3], totals[3];
  rval = create_dataset(3, data_counts, offsets, max_counts, totals);CHECK_MB(rval);

  // Create the datasets
  if (0 == myPcomm->proc_config().proc_rank()) {
    rval = create_set_tables(totals[0], totals[1], totals[2]);CHECK_MB(rval);
  }

  // Store communicated global data
  setContentsOffset = offsets[0];
  setChildrenOffset = offsets[1];
  setParentsOffset = offsets[2];
  maxNumSetContents = max_counts[0];
  maxNumSetChildren = max_counts[1];
  maxNumSetParents  = max_counts[2];
  writeSetContents = totals[0] > 0;
  writeSetChildren = totals[1] > 0;
  writeSetParents  = totals[2] > 0;

  dbgOut.printf(2, "set contents: %ld local, %ld global, offset = %ld\n",
    data_counts[0], totals[0], offsets[0]);
  dbgOut.printf(2, "set children: %ld local, %ld global, offset = %ld\n",
    data_counts[1], totals[1], offsets[1]);
  dbgOut.printf(2, "set parents: %ld local, %ld global, offset = %ld\n",
    data_counts[2], totals[2], offsets[2]);

  return MB_SUCCESS;
}

void WriteHDF5Parallel::remove_remote_entities(EntityHandle relative,
                                               Range& range)
{
  Range result;
  result.merge(intersect(range, nodeSet.range));
  result.merge(intersect(range, setSet.range));
  for (std::list<ExportSet>::iterator eiter = exportList.begin();
       eiter != exportList.end(); ++eiter)
    result.merge(intersect(range, eiter->range));

  //result.merge(intersect(range, myParallelSets));
  Range sets;
  int junk;
  sets.merge(Range::lower_bound(range.begin(), range.end(), CREATE_HANDLE(MBENTITYSET, 0, junk)), range.end());
  remove_remote_sets(relative, sets);
  result.merge(sets);
  range.swap(result);
}

void WriteHDF5Parallel::remove_remote_sets(EntityHandle /* relative */,
                                           Range& range)
{
  Range result(intersect(range, setSet.range));
  // Store the non-intersecting entities separately if needed
  // Range remaining(subtract(range, result));
  range.swap(result);
}

void WriteHDF5Parallel::remove_remote_entities(EntityHandle relative,
                                               std::vector<EntityHandle>& vect)
{
  Range intrsct;
  for (std::vector<EntityHandle>::const_iterator iter = vect.begin();
       iter != vect.end(); ++iter)
    intrsct.insert(*iter);
  remove_remote_entities(relative, intrsct);

  unsigned int read, write;
  for (read = write = 0; read < vect.size(); ++read) {
    if (intrsct.find(vect[read]) != intrsct.end()) {
      if (read != write)
        vect[write] = vect[read];
      ++write;
    }
  }
  if (write != vect.size())
    vect.resize(write);
}

void WriteHDF5Parallel::remove_remote_sets(EntityHandle relative,
                                           std::vector<EntityHandle>& vect)
{
  Range intrsct;
  for (std::vector<EntityHandle>::const_iterator iter = vect.begin();
       iter != vect.end(); ++iter)
    intrsct.insert(*iter);
  remove_remote_sets(relative, intrsct);

  unsigned int read, write;
  for (read = write = 0; read < vect.size(); ++read) {
    if (intrsct.find(vect[read]) != intrsct.end()) {
      if (read != write)
        vect[write] = vect[read];
      ++write;
    }
  }
  if (write != vect.size())
    vect.resize(write);
}

ErrorCode WriteHDF5Parallel::exchange_file_ids(const Range& nonlocal)
{
  ErrorCode rval;

  // For each entity owned on the interface, write its file id to
  // a tag. The sets of entities to be written should already contain
  // only owned entities so by intersecting with them we not only
  // filter by entities to be written, but also restrict to entities
  // owned by the proc

  // Get list of interface entities
  Range imesh, tmp;
  for (std::list<ExportSet>::reverse_iterator i = exportList.rbegin();
       i != exportList.rend(); ++i) {
    tmp.clear();
    rval = myPcomm->filter_pstatus(i->range, PSTATUS_SHARED, PSTATUS_AND, -1, &tmp);
    if (MB_SUCCESS != rval)
      return error(rval);
    imesh.merge(tmp);
  }
  tmp.clear();
  rval = myPcomm->filter_pstatus(nodeSet.range, PSTATUS_SHARED, PSTATUS_AND, -1, &tmp);
  if (MB_SUCCESS != rval)
    return error(rval);
  imesh.merge(tmp);

  // Create tag to store file IDs
  EntityHandle default_val = 0;
  Tag file_id_tag = 0;
  rval = iFace->tag_get_handle("__hdf5_ll_fileid",
                               1, MB_TYPE_HANDLE,
                               file_id_tag,
                               MB_TAG_DENSE | MB_TAG_CREAT,
                               &default_val);
  if (MB_SUCCESS != rval)
    return error(rval);

  // Copy file IDs into tag
  std::vector<EntityHandle> file_id_vect(imesh.size());
  Range::const_iterator i;
  std::vector<EntityHandle>::iterator j = file_id_vect.begin();
  for (i = imesh.begin(); i != imesh.end(); ++i, ++j) {
    *j = idMap.find(*i);
    if (!*j) {
      iFace->tag_delete(file_id_tag);
      return error(MB_FAILURE);
    }
  }
  rval = iFace->tag_set_data(file_id_tag, imesh, &file_id_vect[0]);
  if (MB_SUCCESS != rval) {
    iFace->tag_delete(file_id_tag);
    return error(rval);
  }

  // Do communication
  rval = myPcomm->exchange_tags(file_id_tag, imesh);
  if (MB_SUCCESS != rval) {
    iFace->tag_delete(file_id_tag);
    return error(rval);
  }

  // Copy file IDs from tag into idMap for remote entities
  file_id_vect.resize(nonlocal.size());
  rval = iFace->tag_get_data(file_id_tag, nonlocal, &file_id_vect[0]);
  if (MB_SUCCESS != rval) {
    iFace->tag_delete(file_id_tag);
    return error(rval);
  }

  j = file_id_vect.begin();
  for (i = nonlocal.begin(); i != nonlocal.end(); ++i, ++j) {
    if (*j == 0) {
       int owner = -1;
       myPcomm->get_owner(*i, owner);
       const char* name = CN::EntityTypeName(TYPE_FROM_HANDLE(*i));
       int id = ID_FROM_HANDLE(*i);
       MB_SET_ERR_CONT("Process " << myPcomm->proc_config().proc_rank() << " did not receive valid id handle for shared " << name << " " << id << " owned by process " << owner);
       dbgOut.printf(1, "Did not receive valid remote id for "
                                "shared %s %d owned by process %d",
                                name, id, owner);
       iFace->tag_delete(file_id_tag);
       return error(MB_FAILURE);
    }
    else {
      if (!idMap.insert(*i, *j, 1).second) {
        iFace->tag_delete(file_id_tag);
        return error(MB_FAILURE);
      }
    }
  }

#ifndef NDEBUG
  // Check that writer is correct with regards to which entities
  // that it owns by verifying that the file ids that we thought
  // we were sending where not received instead
  file_id_vect.resize(imesh.size());
  rval = iFace->tag_get_data(file_id_tag, imesh, &file_id_vect[0]);
  if (MB_SUCCESS != rval) {
    iFace->tag_delete(file_id_tag);
    return error(rval);
  }
  int invalid_count = 0;
  j = file_id_vect.begin();
  for (i = imesh.begin(); i != imesh.end(); ++i, ++j) {
    EntityHandle h = idMap.find(*i);
    if (*j != h) {
      ++invalid_count;
      dbgOut.printf(1, "Conflicting ownership for %s %ld\n",
        CN::EntityTypeName(TYPE_FROM_HANDLE(*i)),
        (long)ID_FROM_HANDLE(*i));
    }
  }
  if (invalid_count) {
    iFace->tag_delete(file_id_tag);
    MB_SET_ERR(MB_FAILURE, invalid_count << " entities with conflicting ownership found by process " << myPcomm->proc_config().proc_rank() << ". This will result in duplicate entities written to file");
  }
#endif

  return iFace->tag_delete(file_id_tag);
}

void WriteHDF5Parallel::print_times(const double* times) const
{
  if (!myPcomm) {
    WriteHDF5::print_times(times);
  }
  else {
    double recv[NUM_TIMES];
    MPI_Reduce((void*)times, recv, NUM_TIMES, MPI_DOUBLE, MPI_MAX, 0, myPcomm->proc_config().proc_comm());
    if (0 == myPcomm->proc_config().proc_rank())
      WriteHDF5::print_times(recv);
  }
}

} // namespace moab
