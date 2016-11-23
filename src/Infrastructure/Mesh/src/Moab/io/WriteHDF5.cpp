/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

//-------------------------------------------------------------------------
// Filename      : WriteHDF5.cpp
//
// Purpose       : TSTT HDF5 Writer 
//
// Special Notes : WriteSLAC used as template for this
//
// Creator       : Jason Kraftcheck
//
// Creation Date : 04/01/04
//-------------------------------------------------------------------------

#include <assert.h>
#if defined(_MSC_VER)
  typedef int id_t;
#elif defined(__MINGW32__)
  #include <sys/time.h>
#endif
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <limits>
#include <cstdio>
#include <iostream>
#include "WriteHDF5.hpp"
#include <H5Tpublic.h>
#include <H5Ppublic.h>
#include <H5Epublic.h>
#include "moab/Interface.hpp"
#include "Internals.hpp"
#include "MBTagConventions.hpp"
#include "moab/CN.hpp"
#include "moab/FileOptions.hpp"
#include "moab/CpuTimer.hpp"
#include "IODebugTrack.hpp"
#include "mhdf.h"

#ifndef MOAB_HAVE_HDF5
#error Attempt to compile WriteHDF5 with HDF5 support disabled
#endif

/* Access HDF5 file handle for debugging
#include <H5Fpublic.h>
struct file { uint32_t magic; hid_t handle; };
*/
#undef DEBUG

#undef BLOCKED_COORD_IO

#ifdef DEBUG
/*
# include <H5Epublic.h>
  extern "C" herr_t hdf_error_handler(void*)
  {
    H5Eprint(stderr);
    assert(0);
  }
*/
# define myassert(A) assert(A)
#else
# define myassert(A)
#endif

#ifdef MOAB_HAVE_VALGRIND
#  include <valgrind/memcheck.h>
#else
#  ifndef VALGRIND_CHECK_MEM_IS_DEFINED
#    define VALGRIND_CHECK_MEM_IS_DEFINED(a, b) ((void)0)
#  endif
#  ifndef VALGRIND_CHECK_MEM_IS_ADDRESSABLE
#    define VALGRIND_CHECK_MEM_IS_ADDRESSABLE(a, b) ((void)0)
#  endif
#  ifndef VALGRIND_MAKE_MEM_UNDEFINED
#    define VALGRIND_MAKE_MEM_UNDEFINED(a, b) ((void)0)
#  endif
#endif

namespace moab {

template <typename T> inline 
void VALGRIND_MAKE_VEC_UNDEFINED(std::vector<T>& v) {
  (void)VALGRIND_MAKE_MEM_UNDEFINED(&v[0], v.size() * sizeof(T));
}

#define WRITE_HDF5_BUFFER_SIZE (40 * 1024 * 1024)

static hid_t get_id_type()
{
  if (8 == sizeof(WriteHDF5::wid_t)) {
    if (8 == sizeof(long))
      return H5T_NATIVE_ULONG;
    else
      return H5T_NATIVE_UINT64;
  }
  else if (4 == sizeof(WriteHDF5::wid_t)) {
    if (4 == sizeof(int))
      return H5T_NATIVE_UINT;
    else
      return H5T_NATIVE_UINT32;
  }
  else {
    assert(0);
    return (hid_t) - 1;
  }
}

// This is the HDF5 type used to store file IDs
const hid_t WriteHDF5::id_type = get_id_type();

// This function doesn't do anything useful. It's just a nice
// place to set a break point to determine why the writer fails.
static inline ErrorCode error(ErrorCode rval)
{
  return rval;
}

// Call \c error function during HDF5 library errors to make
// it easier to trap such errors in the debugger. This function
// gets registered with the HDF5 library as a callback. It
// works the same as the default (H5Eprint), except that it
// also calls the \c error function as a no-op.
#if defined(H5E_auto_t_vers) && H5E_auto_t_vers > 1
static herr_t handle_hdf5_error(hid_t stack, void* data)
{
  WriteHDF5::HDF5ErrorHandler* h = reinterpret_cast<WriteHDF5::HDF5ErrorHandler*>(data);
  herr_t result = 0;
  if (h->func)
    result = (*h->func)(stack, h->data);
  error(MB_FAILURE);
  return result;
}
#else
static herr_t handle_hdf5_error(void* data)
{
  WriteHDF5::HDF5ErrorHandler* h = reinterpret_cast<WriteHDF5::HDF5ErrorHandler*>(data);
  herr_t result = 0;
  if (h->func)
    result = (*h->func)(h->data);
  error(MB_FAILURE);
  return result;
}
#endif

// Some macros to handle error checking. The
// CHK_MHDF__ERR* macros check the value of an mhdf_Status
// object. The CHK_MB_ERR_* check the value of an ErrorCode.
// The *_0 macros accept no other arguments. The *_1
// macros accept a single hdf5 handle to close on error.
// The *_2 macros accept an array of two hdf5 handles to
// close on error. The _*2C macros accept one hdf5 handle
// to close on error and a bool and an hdf5 handle where
// the latter handle is conditionally closed depending on
// the value of the bool. All macros contain a "return"
// statement.
#define CHK_MHDF_ERR_0(A) \
do { \
  if (mhdf_isError(&(A))) { \
    MB_SET_ERR_CONT(mhdf_message(&(A))); \
    myassert(0); \
    return error(MB_FAILURE); \
  } \
} while (false)

#define CHK_MHDF_ERR_1(A, B) \
do { \
  if (mhdf_isError(&(A))) { \
    MB_SET_ERR_CONT(mhdf_message(&(A))); \
    myassert(0); \
    mhdf_closeData(filePtr, (B), &(A)); \
    return error(MB_FAILURE); \
  } \
} while (false)

#define CHK_MHDF_ERR_2(A, B) \
do { \
  if (mhdf_isError(&(A))) { \
    MB_SET_ERR_CONT(mhdf_message(&(A))); \
    myassert(0); \
    mhdf_closeData(filePtr, (B)[0], &(A)); \
    mhdf_closeData(filePtr, (B)[1], &(A)); \
    return error(MB_FAILURE); \
  } \
} while (false)

#define CHK_MHDF_ERR_3(A, B) \
do { \
  if (mhdf_isError(&(A))) { \
    MB_SET_ERR_CONT(mhdf_message(&(A))); \
    myassert(0); \
    mhdf_closeData(filePtr, (B)[0], &(A)); \
    mhdf_closeData(filePtr, (B)[1], &(A)); \
    mhdf_closeData(filePtr, (B)[2], &(A)); \
    return error(MB_FAILURE); \
  } \
} while (false)

#define CHK_MHDF_ERR_2C(A, B, C, D) \
do { \
  if (mhdf_isError(&(A))) { \
    MB_SET_ERR_CONT(mhdf_message(&(A))); \
    myassert(0); \
    mhdf_closeData(filePtr, (B), &(A)); \
    if (C) mhdf_closeData(filePtr, (D), &(A)); \
    return error(MB_FAILURE); \
  } \
} while (false)

#define CHK_MB_ERR_0(A) \
do { \
  if (MB_SUCCESS != (A)) { \
    MB_CHK_ERR_CONT((A)); \
    return error(A); \
  } \
} while (false)

#define CHK_MB_ERR_1(A, B, C) \
do { \
  if (MB_SUCCESS != (A)) { \
    MB_CHK_ERR_CONT((A)); \
    mhdf_closeData(filePtr, (B), &(C)); \
    myassert(0); \
    return error(A); \
  } \
} while (false)

#define CHK_MB_ERR_2(A, B, C) \
do { \
  if (MB_SUCCESS != (A)) { \
    MB_CHK_ERR_CONT((A)); \
    mhdf_closeData(filePtr, (B)[0], &(C)); \
    mhdf_closeData(filePtr, (B)[1], &(C)); \
    write_finished(); \
    myassert(0); \
    return error(A); \
  } \
} while (false)

#define CHK_MB_ERR_3(A, B, C) \
do { \
  if (MB_SUCCESS != (A)) { \
    MB_CHK_ERR_CONT((A)); \
    mhdf_closeData(filePtr, (B)[0], &(C)); \
    mhdf_closeData(filePtr, (B)[1], &(C)); \
    mhdf_closeData(filePtr, (B)[2], &(C)); \
    write_finished(); \
    myassert(0); \
    return error(A); \
  } \
} while (false)

#define CHK_MB_ERR_2C(A, B, C, D, E) \
do { \
  if (MB_SUCCESS != (A)) { \
    MB_CHK_ERR_CONT((A)); \
    mhdf_closeData(filePtr, (B), &(E)); \
    if (C) \
      mhdf_closeData(filePtr, (D), &(E)); \
    write_finished(); \
    myassert(0); \
    return error(A); \
  } \
} while (false)

#define debug_barrier() debug_barrier_line(__LINE__)
void WriteHDF5::debug_barrier_line(int)
{
}

class CheckOpenWriteHDF5Handles
{
  int fileline;
  mhdf_FileHandle handle;
  int enter_count;

public:
  CheckOpenWriteHDF5Handles(mhdf_FileHandle file, int line)
    : fileline(line), handle(file),
      enter_count(mhdf_countOpenHandles(file))
  {}

  ~CheckOpenWriteHDF5Handles()
  {
    int new_count = mhdf_countOpenHandles(handle);
    if (new_count != enter_count) {
      std::cout << "Leaked HDF5 object handle in function at "
                << __FILE__ << ":" << fileline << std::endl
                << "Open at entrance: " << enter_count << std::endl
                << "Open at exit:     " << new_count << std::endl;
    }
  }
};

MPEState WriteHDF5::topState;
MPEState WriteHDF5::subState;

#ifdef NDEBUG
#define CHECK_OPEN_HANDLES
#else
#define CHECK_OPEN_HANDLES \
  CheckOpenWriteHDF5Handles check_open_handles_(filePtr, __LINE__)
#endif

bool WriteHDF5::convert_handle_tag(const EntityHandle* source,
                                   EntityHandle* dest, size_t count) const
{
  bool some_valid = false;
  for (size_t i = 0; i < count; ++i) {
    if (!source[i])
      dest[i] = 0;
    else {
      dest[i] = idMap.find(source[i]);
      if (dest[i])
        some_valid = true;
    }
  }

  return some_valid;
}

bool WriteHDF5::convert_handle_tag(EntityHandle* data, size_t count) const
{
  assert(sizeof(EntityHandle) == sizeof(wid_t));
  return convert_handle_tag(data, data, count);
}

ErrorCode WriteHDF5::assign_ids(const Range& entities, wid_t id)
{
  Range::const_pair_iterator pi;
  for (pi = entities.const_pair_begin(); pi != entities.const_pair_end(); ++pi) {
    const EntityHandle n = pi->second - pi->first + 1;
    dbgOut.printf(3, "Assigning %s %lu to %lu to file IDs [%lu,%lu]\n",
                  CN::EntityTypeName(TYPE_FROM_HANDLE(pi->first)),
                  (unsigned long)(ID_FROM_HANDLE(pi->first)),
                  (unsigned long)(ID_FROM_HANDLE(pi->first) + n - 1),
                  (unsigned long)id,
                  (unsigned long)(id + n - 1));
    if (!idMap.insert(pi->first, id, n).second)
      return error(MB_FAILURE);
    id += n;
  }

  return MB_SUCCESS;
}

const char* WriteHDF5::ExportSet::name() const
{
  static char buffer[128];
  switch (type) {
    case MBVERTEX:
      return mhdf_node_type_handle();
    case MBENTITYSET:
      return mhdf_set_type_handle();
    default:
      sprintf(buffer, "%s%d", CN::EntityTypeName(type), num_nodes);
      return buffer;
  }
}

WriterIface* WriteHDF5::factory(Interface* iface)
{
  return new WriteHDF5(iface);
}

WriteHDF5::WriteHDF5(Interface* iface)
  : bufferSize(WRITE_HDF5_BUFFER_SIZE),
    dataBuffer(0),
    iFace(iface),
    writeUtil(0),
    filePtr(0),
    setContentsOffset(0),
    setChildrenOffset(0),
    setParentsOffset(0),
    maxNumSetContents(0),
    maxNumSetChildren(0),
    maxNumSetParents(0),
    writeSets(false),
    writeSetContents(false),
    writeSetChildren(false),
    writeSetParents(false),
    parallelWrite(false),
    collectiveIO(false),
    writeTagDense(false),
    writeProp(H5P_DEFAULT),
    dbgOut("H5M", stderr),
    debugTrack(false)
{
}

ErrorCode WriteHDF5::init()
{
  ErrorCode rval;

  if (writeUtil) // init has already been called
    return MB_SUCCESS;
/*
#ifdef DEBUG
  H5Eset_auto(&hdf_error_handler, writeUtil); // HDF5 callback for errors
#endif
*/
  // For known tag types, store the corresponding HDF5 in which
  // the tag data is to be written in the file.
  //register_known_tag_types(iFace);

  // Get the util interface
  rval = iFace->query_interface(writeUtil);CHK_MB_ERR_0(rval);

  idMap.clear();

#if defined(H5Eget_auto_vers) && H5Eget_auto_vers > 1
  herr_t err = H5Eget_auto(H5E_DEFAULT, &errorHandler.func, &errorHandler.data);
#else
  herr_t err = H5Eget_auto(&errorHandler.func, &errorHandler.data);
#endif
  if (err < 0) {
    errorHandler.func = 0;
    errorHandler.data = 0;
  }
  else {
#if defined(H5Eset_auto_vers) && H5Eset_auto_vers > 1
    err = H5Eset_auto(H5E_DEFAULT, &handle_hdf5_error, &errorHandler);
#else
    err = H5Eset_auto(&handle_hdf5_error, &errorHandler);
#endif
    if (err < 0) {
      errorHandler.func = 0;
      errorHandler.data = 0;
    }
  }

  if (!topState.valid())
    topState = MPEState("WriteHDF5", "yellow");
  if (!subState.valid())
    subState = MPEState("WriteHDF5 subevent", "cyan");

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_finished()
{
  // Release memory allocated in lists
  exportList.clear();
  nodeSet.range.clear();
  setSet.range.clear();
  tagList.clear();
  idMap.clear();

  HDF5ErrorHandler handler;
#if defined(H5Eget_auto_vers) && H5Eget_auto_vers > 1
  herr_t err = H5Eget_auto(H5E_DEFAULT, &handler.func, &handler.data);
#else
  herr_t err = H5Eget_auto(&handler.func, &handler.data);
#endif
  if (err >= 0 && handler.func == &handle_hdf5_error) {
    assert(handler.data == &errorHandler);
#if defined(H5Eget_auto_vers) && H5Eget_auto_vers > 1
    H5Eset_auto(H5E_DEFAULT, errorHandler.func, errorHandler.data);
#else
    H5Eset_auto(errorHandler.func, errorHandler.data);
#endif
  }

  return MB_SUCCESS;
}

WriteHDF5::~WriteHDF5()
{
  if (!writeUtil) // init() failed.
    return;

  iFace->release_interface(writeUtil);
}

ErrorCode WriteHDF5::write_file(const char* filename,
                                bool overwrite,
                                const FileOptions& opts,
                                const EntityHandle* set_array,
                                const int num_sets,
                                const std::vector<std::string>& qa_records,
                                const Tag* tag_list,
                                int num_tags,
                                int user_dimension)
{
  mhdf_Status status;

  parallelWrite = false;
  collectiveIO = false;

  // Enable debug output
  int tmpval = 0;
  if (MB_SUCCESS == opts.get_int_option("DEBUG_IO", 1, tmpval))
    dbgOut.set_verbosity(tmpval);

  //writeTagDense = (MB_SUCCESS == opts.get_null_option("DENSE_TAGS"));
  writeTagDense = true; 

  // Enable some extra checks for reads.  Note: amongst other things this
  // will print errors if the entire file is not read, so if doing a
  // partial read that is not a parallel read, this should be disabled.
  debugTrack = (MB_SUCCESS == opts.get_null_option("DEBUG_BINIO"));

  bufferSize = WRITE_HDF5_BUFFER_SIZE;
  int buf_size;
  ErrorCode rval = opts.get_int_option("BUFFER_SIZE", buf_size);
  if (MB_SUCCESS == rval && buf_size >= 24)
    bufferSize = buf_size;

  // Allocate internal buffer to use when gathering data to write.
  dataBuffer = (char*)malloc(bufferSize);
  if (!dataBuffer)
    return error(MB_MEMORY_ALLOCATION_FAILED);

  // Clear filePtr so we know if it is open upon failure
  filePtr = 0;

  // Do actual write.
  writeProp = H5P_DEFAULT;
  ErrorCode result = write_file_impl(filename, overwrite, opts,
                                     set_array, num_sets,
                                     qa_records,
                                     tag_list, num_tags,
                                     user_dimension);
  // Close writeProp if it was opened
  if (writeProp != H5P_DEFAULT)
    H5Pclose(writeProp);

  // Free memory buffer
  free(dataBuffer);
  dataBuffer = 0;

  // Close file
  bool created_file = false;
  if (filePtr) {
    created_file = true;
    mhdf_closeFile(filePtr, &status);
    filePtr = 0;
    if (mhdf_isError(&status)) {
      MB_SET_ERR_CONT(mhdf_message(&status));
      if (MB_SUCCESS == result)
        result = MB_FAILURE;
    }
  }

  // Release other resources
  if (MB_SUCCESS == result)
    result = write_finished();
  else
    write_finished();

  // If write failed, remove file unless KEEP option was specified
  if (MB_SUCCESS != result && created_file &&
      MB_ENTITY_NOT_FOUND == opts.get_null_option("KEEP"))
    remove(filename);

  return result;
}

ErrorCode WriteHDF5::write_file_impl(const char* filename,
                                     bool overwrite,
                                     const FileOptions& opts,
                                     const EntityHandle* set_array,
                                     const int num_sets,
                                     const std::vector<std::string>& qa_records,
                                     const Tag* tag_list,
                                     int num_tags,
                                     int user_dimension)
{
  ErrorCode result;
  std::list<TagDesc>::const_iterator t_itor;
  std::list<ExportSet>::iterator ex_itor;
  EntityHandle elem_count, max_id;
  double times[NUM_TIMES] = {0};

  if (MB_SUCCESS != init())
    return error(MB_FAILURE);

  // See if we need to report times
  bool cputime = false;
  result = opts.get_null_option("CPUTIME");
  if (MB_SUCCESS == result)
    cputime = true;

  CpuTimer timer;

  dbgOut.tprint(1, "Gathering Mesh\n");
  topState.start("gathering mesh");

  // Gather mesh to export
  exportList.clear();
  if (0 == num_sets || (1 == num_sets && set_array[0] == 0)) {
    result = gather_all_mesh();topState.end(result);CHK_MB_ERR_0(result);
  }
  else {
    std::vector<EntityHandle> passed_export_list(set_array, set_array + num_sets);
    result = gather_mesh_info(passed_export_list);topState.end(result);CHK_MB_ERR_0(result);
  }

  times[GATHER_TIME] = timer.time_elapsed();

  //if (nodeSet.range.size() == 0)
  //  return error(MB_ENTITY_NOT_FOUND);

  dbgOut.tprint(1, "Checking ID space\n");

  // Make sure ID space is sufficient
  elem_count = nodeSet.range.size() + setSet.range.size();
  for (ex_itor = exportList.begin(); ex_itor != exportList.end(); ++ex_itor)
    elem_count += ex_itor->range.size();
  max_id = (EntityHandle)1 << (8*sizeof(wid_t) - 1);
  if (elem_count > max_id) {
    MB_SET_ERR_CONT("ID space insufficient for mesh size");
    return error(result);
  }

  dbgOut.tprint(1, "Creating File\n");

  // Figure out the dimension in which to write the mesh.
  int mesh_dim;
  result = iFace->get_dimension(mesh_dim);CHK_MB_ERR_0(result);

  if (user_dimension < 1)
    user_dimension = mesh_dim;
  user_dimension = user_dimension > mesh_dim ? mesh_dim : user_dimension;

  // Create the file layout, including all tables (zero-ed) and
  // all structure and meta information.
  const char* optnames[] = {"WRITE_PART", "FORMAT", 0};
  int junk;
  parallelWrite = (MB_SUCCESS == opts.match_option("PARALLEL", optnames, junk));
  if (parallelWrite) {
    // Just store Boolean value based on string option here.
    // parallel_create_file will set writeProp accordingly.
    //collectiveIO = (MB_SUCCESS == opts.get_null_option("COLLECTIVE"));
    //dbgOut.printf(2, "'COLLECTIVE' option = %s\n", collectiveIO ? "YES" : "NO");
    // Do this all the time, as it appears to be much faster than indep in some cases
    collectiveIO = true;
    result = parallel_create_file(filename, overwrite, qa_records, opts, tag_list, num_tags, user_dimension, times);
  }
  else {
    result = serial_create_file(filename, overwrite, qa_records, tag_list, num_tags, user_dimension);
  }
  if (MB_SUCCESS != result)
    return error(result);

  times[CREATE_TIME] = timer.time_elapsed();

  dbgOut.tprint(1, "Writing Nodes.\n");
  // Write node coordinates
  if (!nodeSet.range.empty() || parallelWrite) {
    topState.start("writing coords");
    result = write_nodes();
    topState.end(result);
    if (MB_SUCCESS != result)
      return error(result);
  }

  times[COORD_TIME] = timer.time_elapsed();

  dbgOut.tprint(1, "Writing connectivity.\n");

  // Write element connectivity
  for (ex_itor = exportList.begin(); ex_itor != exportList.end(); ++ex_itor) {
    topState.start("writing connectivity for ", ex_itor->name());
    result = write_elems(*ex_itor);
    topState.end(result);
    if (MB_SUCCESS != result)
      return error(result);
  }
  times[CONN_TIME] = timer.time_elapsed();

  dbgOut.tprint(1, "Writing sets.\n");

  // Write meshsets
  result = write_sets(times);
  if (MB_SUCCESS != result)
    return error(result);
  debug_barrier();

  times[SET_TIME] = timer.time_elapsed();
  dbgOut.tprint(1, "Writing adjacencies.\n");

  // Write adjacencies
  // Tim says don't save node adjacencies!
#ifdef MB_H5M_WRITE_NODE_ADJACENCIES
  result = write_adjacencies(nodeSet);
  if (MB_SUCCESS != result)
    return error(result);
#endif
  for (ex_itor = exportList.begin(); ex_itor != exportList.end(); ++ex_itor) {
    topState.start("writing adjacencies for ", ex_itor->name());
    result = write_adjacencies(*ex_itor);
    topState.end(result);
    if (MB_SUCCESS != result)
      return error(result);
  }
  times[ADJ_TIME] = timer.time_elapsed();

  dbgOut.tprint(1, "Writing tags.\n");

  // Write tags
  for (t_itor = tagList.begin(); t_itor != tagList.end(); ++t_itor) {
    std::string name;
    iFace->tag_get_name(t_itor->tag_id, name);
    topState.start("writing tag: ", name.c_str());
    result = write_tag(*t_itor, times);
    topState.end(result);
    if (MB_SUCCESS != result)
      return error(result);
  }
  times[TAG_TIME] = timer.time_elapsed();
  
  times[TOTAL_TIME] = timer.time_since_birth();

  if (cputime) {
    print_times(times);
  }

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::initialize_mesh(const Range ranges[5])
{
  ErrorCode rval;

  if (!ranges[0].all_of_type(MBVERTEX))
    return error(MB_FAILURE);
  nodeSet.range = ranges[0];
  nodeSet.type = MBVERTEX;
  nodeSet.num_nodes = 1;
  nodeSet.max_num_ents = nodeSet.max_num_adjs = 0;

  if (!ranges[4].all_of_type(MBENTITYSET))
    return error(MB_FAILURE);
  setSet.range = ranges[4];
  setSet.type = MBENTITYSET;
  setSet.num_nodes = 0;
  setSet.max_num_ents = setSet.max_num_adjs = 0;
  maxNumSetContents = maxNumSetChildren = maxNumSetParents = 0;

  exportList.clear();
  std::vector<Range> bins(1024); // Sort entities by connectivity length
                                 // Resize is expensive due to Range copy, so start big
  for (EntityType type = MBEDGE; type < MBENTITYSET; ++type) {
    ExportSet set;
    set.max_num_ents = set.max_num_adjs = 0;
    const int dim = CN::Dimension(type);

    // Group entities by connectivity length
    bins.clear();
    assert(dim >= 0 && dim <= 4);
    std::pair<Range::const_iterator, Range::const_iterator> p = ranges[dim].equal_range(type);
    Range::const_iterator i = p.first;
    while (i != p.second) {
      Range::const_iterator first = i;
      EntityHandle const* conn;
      int len, firstlen;

      // Dummy storage vector for structured mesh "get_connectivity" function
      std::vector<EntityHandle> storage;

      rval = iFace->get_connectivity(*i, conn, firstlen, false, &storage);
      if (MB_SUCCESS != rval)
        return error(rval);

      for (++i; i != p.second; ++i) {
        rval = iFace->get_connectivity(*i, conn, len, false, &storage);
        if (MB_SUCCESS != rval)
          return error(rval);

        if (len != firstlen)
          break;
      }

      if (firstlen >= (int)bins.size())
        bins.resize(firstlen + 1);
      bins[firstlen].merge(first, i);
    }
    // Create ExportSet for each group
    for (std::vector<Range>::iterator j = bins.begin(); j != bins.end(); ++j) {
      if (j->empty())
        continue;

      set.range.clear();
      set.type = type;
      set.num_nodes = j - bins.begin();
      exportList.push_back(set);
      exportList.back().range.swap(*j);
    }
  }

  return MB_SUCCESS;
}

// Gather the mesh to be written from a list of owning meshsets.
ErrorCode WriteHDF5::gather_mesh_info(const std::vector<EntityHandle>& export_sets)
{
  ErrorCode rval;

  int dim;
  Range range;     // Temporary storage
  Range ranges[5]; // Lists of entities to export, grouped by dimension

  // Gather list of all related sets
  std::vector<EntityHandle> stack(export_sets);
  std::copy(export_sets.begin(), export_sets.end(), stack.begin());
  std::vector<EntityHandle> set_children;
  while (!stack.empty()) {
    EntityHandle meshset = stack.back(); stack.pop_back();
    ranges[4].insert(meshset);

    // Get contained sets
    range.clear();
    rval = iFace->get_entities_by_type(meshset, MBENTITYSET, range);CHK_MB_ERR_0(rval);
    for (Range::iterator ritor = range.begin(); ritor != range.end(); ++ritor) {
      if (ranges[4].find(*ritor) == ranges[4].end())
        stack.push_back(*ritor);
    }

    // Get child sets
    set_children.clear();
    rval = iFace->get_child_meshsets(meshset, set_children, 1);CHK_MB_ERR_0(rval);
    for (std::vector<EntityHandle>::iterator vitor = set_children.begin();
         vitor != set_children.end(); ++vitor) {
      if (ranges[4].find(*vitor) == ranges[4].end())
        stack.push_back(*vitor);
    }
  }

  // Gather list of all mesh entities from list of sets,
  // grouped by dimension.
  for (Range::iterator setitor = ranges[4].begin();
       setitor != ranges[4].end(); ++setitor) {
    for (dim = 0; dim < 4; ++dim) {
      range.clear();
      rval = iFace->get_entities_by_dimension(*setitor, dim, range, false);CHK_MB_ERR_0(rval);

      ranges[dim].merge(range);
    }
  }

  // For each list of elements, append adjacent children and
  // nodes to lists.
  for (dim = 3; dim > 0; --dim) {
    for (int cdim = 1; cdim < dim; ++cdim) {
      range.clear();
      rval = iFace->get_adjacencies(ranges[dim], cdim, false, range);CHK_MB_ERR_0(rval);
      ranges[cdim].merge(range);
    }
    range.clear();
    rval = writeUtil->gather_nodes_from_elements(ranges[dim], 0, range);CHK_MB_ERR_0(rval);
    ranges[0].merge(range);
  }

  return initialize_mesh(ranges);
}

// Gather all the mesh and related information to be written.
ErrorCode WriteHDF5::gather_all_mesh()
{
  ErrorCode rval;
  Range ranges[5];

  rval = iFace->get_entities_by_type(0, MBVERTEX, ranges[0]);
  if (MB_SUCCESS != rval)
    return error(rval);

  rval = iFace->get_entities_by_dimension(0, 1, ranges[1]);
  if (MB_SUCCESS != rval)
    return error(rval);

  rval = iFace->get_entities_by_dimension(0, 2, ranges[2]);
  if (MB_SUCCESS != rval)
    return error(rval);

  rval = iFace->get_entities_by_dimension(0, 3, ranges[3]);
  if (MB_SUCCESS != rval)
    return error(rval);

  rval = iFace->get_entities_by_type(0, MBENTITYSET, ranges[4]);
  if (MB_SUCCESS != rval)
    return error(rval);

  return initialize_mesh(ranges);
}

ErrorCode WriteHDF5::write_nodes()
{
  mhdf_Status status;
  int dim, mesh_dim;
  ErrorCode rval;
  hid_t node_table;
  long first_id, num_nodes;

  if (!nodeSet.total_num_ents)
    return MB_SUCCESS; // No nodes!

  CHECK_OPEN_HANDLES;

  rval = iFace->get_dimension(mesh_dim);CHK_MB_ERR_0(rval);

  debug_barrier();
  dbgOut.print(3, "Opening Node Coords\n");
  node_table = mhdf_openNodeCoords(filePtr, &num_nodes, &dim, &first_id, &status);CHK_MHDF_ERR_0(status);
  IODebugTrack track(debugTrack, "nodes", num_nodes);

  double* buffer = (double*)dataBuffer;
#ifdef BLOCKED_COORD_IO
  int chunk_size = bufferSize / sizeof(double);
#else
  int chunk_size = bufferSize / (3*sizeof(double));
#endif

  long remaining = nodeSet.range.size();
  long num_writes = (remaining + chunk_size - 1) / chunk_size;
  if (nodeSet.max_num_ents) {
    assert(nodeSet.max_num_ents >= remaining);
    num_writes = (nodeSet.max_num_ents + chunk_size - 1) / chunk_size;
  }
  long remaining_writes = num_writes;

  long offset = nodeSet.offset;
  Range::const_iterator iter = nodeSet.range.begin();
  dbgOut.printf(3, "Writing %ld nodes in %ld blocks of %d\n", remaining, (remaining + chunk_size - 1) / chunk_size, chunk_size);
  while (remaining) {
    (void)VALGRIND_MAKE_MEM_UNDEFINED(dataBuffer, bufferSize);
    long count = chunk_size < remaining ? chunk_size : remaining;
    remaining -= count;
    Range::const_iterator end = iter;
    end += count;

#ifdef BLOCKED_COORD_IO
    for (int d = 0; d < dim; d++) {
      if (d < mesh_dim) {
        rval = writeUtil->get_node_coords(d, iter, end, count, buffer);CHK_MB_ERR_1(rval, node_table, status);
      }
      else
        memset(buffer, 0, count * sizeof(double));

      dbgOut.printf(3, " writing %c node chunk %ld of %ld, %ld values at %ld\n",
                    (char)('X' + d), num_writes - remaining_writes + 1, num_writes, count, offset);
      mhdf_writeNodeCoordWithOpt(node_table, offset, count, d, buffer, writeProp, &status);CHK_MHDF_ERR_1(status, node_table);
    }
#else
    rval = writeUtil->get_node_coords(-1, iter, end, 3*count, buffer);CHK_MB_ERR_1(rval, node_table, status);
    dbgOut.printf(3, " writing node chunk %ld of %ld, %ld values at %ld\n",
                  num_writes - remaining_writes + 1, num_writes, count, offset);
    mhdf_writeNodeCoordsWithOpt(node_table, offset, count, buffer, writeProp, &status);CHK_MHDF_ERR_1(status, node_table);
#endif
    track.record_io(offset, count);

    iter = end;
    offset += count;
    --remaining_writes;
  }

  // Do empty writes if necessary for parallel collective IO
  if (collectiveIO) {
    while (remaining_writes--) {
      assert(writeProp != H5P_DEFAULT);
#ifdef BLOCKED_COORD_IO   
      for (int d = 0; d < dim; ++d) {
        dbgOut.printf(3, " writing (empty) %c node chunk %ld of %ld.\n",
                      (char)('X' + d), num_writes - remaining_writes, num_writes);
        mhdf_writeNodeCoordWithOpt(node_table, offset, 0, d, 0, writeProp, &status);CHK_MHDF_ERR_1(status, node_table);
      }
#else
      dbgOut.printf(3, " writing (empty) node chunk %ld of %ld.\n",
                    num_writes - remaining_writes, num_writes);
      mhdf_writeNodeCoordsWithOpt(node_table, offset, 0, 0, writeProp, &status);CHK_MHDF_ERR_1(status, node_table);
#endif
    }
  }

  mhdf_closeData(filePtr, node_table, &status);CHK_MHDF_ERR_0(status);

  track.all_reduce();
  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_elems(ExportSet& elems)
{
  mhdf_Status status;
  ErrorCode rval;
  long first_id;
  int nodes_per_elem;
  long table_size;

  CHECK_OPEN_HANDLES;

  debug_barrier();
  dbgOut.printf(2, "Writing %lu elements of type %s%d\n",
                (unsigned long)elems.range.size(),
                CN::EntityTypeName(elems.type), elems.num_nodes);
  dbgOut.print(3, "Writing elements", elems.range);

  hid_t elem_table = mhdf_openConnectivity(filePtr,
                                           elems.name(),
                                           &nodes_per_elem,
                                           &table_size,
                                           &first_id,
                                           &status);CHK_MHDF_ERR_0(status);
  IODebugTrack track(debugTrack, elems.name() && strlen(elems.name())
    ? elems.name() : "<ANONYMOUS ELEM SET?>", table_size);

  assert((unsigned long)first_id <= elems.first_id);
  assert((unsigned long)table_size >= elems.offset + elems.range.size());

  EntityHandle* buffer = (EntityHandle*)dataBuffer;
  int chunk_size = bufferSize / (elems.num_nodes * sizeof(wid_t));
  long offset = elems.offset;
  long remaining = elems.range.size();
  long num_writes = (remaining + chunk_size - 1) / chunk_size;
  if (elems.max_num_ents) {
    assert(elems.max_num_ents >= remaining);
    num_writes = (elems.max_num_ents + chunk_size - 1) / chunk_size;
  }
  long remaining_writes = num_writes;
  Range::iterator iter = elems.range.begin();

  while (remaining) {
    (void)VALGRIND_MAKE_MEM_UNDEFINED(dataBuffer, bufferSize);
    long count = chunk_size < remaining ? chunk_size : remaining;
    remaining -= count;

    Range::iterator next = iter;
    next += count;
    rval = writeUtil->get_element_connect(iter, next, elems.num_nodes,
                                          count * elems.num_nodes, buffer);CHK_MB_ERR_1(rval, elem_table, status);
    iter = next;

    for (long i = 0; i < count*nodes_per_elem; ++i) {
      buffer[i] = idMap.find(buffer[i]);
      if (0 == buffer[i]) {
        MB_SET_ERR_CONT("Invalid " << elems.name() << " element connectivity. Write Aborted");
        mhdf_closeData(filePtr, elem_table, &status);
        return error(MB_FAILURE);
      }
    }

    dbgOut.printf(3, " writing node connectivity %ld of %ld, %ld values at %ld\n",
                  num_writes - remaining_writes + 1, num_writes, count, offset);
    track.record_io(offset, count);
    mhdf_writeConnectivityWithOpt(elem_table, offset, count,
                                  id_type, buffer, writeProp, &status);CHK_MHDF_ERR_1(status, elem_table);

    offset += count;
    --remaining_writes;
  }

  // Do empty writes if necessary for parallel collective IO
  if (collectiveIO) {
    while (remaining_writes--) {
      assert(writeProp != H5P_DEFAULT);
      dbgOut.printf(3, " writing (empty) connectivity chunk %ld of %ld.\n",
                    num_writes - remaining_writes + 1, num_writes);
      mhdf_writeConnectivityWithOpt(elem_table, offset, 0, id_type, 0, writeProp, &status);CHK_MHDF_ERR_1(status, elem_table);
    }
  }

  mhdf_closeData(filePtr, elem_table, &status);CHK_MHDF_ERR_0(status);

  track.all_reduce();
  return MB_SUCCESS;
}

ErrorCode WriteHDF5::get_set_info(EntityHandle set,
                                  long& num_entities,
                                  long& num_children,
                                  long& num_parents,
                                  unsigned long& flags)
{
  ErrorCode rval;
  int i;
  unsigned int u;

  rval = iFace->get_number_entities_by_handle(set, i, false);CHK_MB_ERR_0(rval);
  num_entities = i;

  rval = iFace->num_child_meshsets(set, &i);CHK_MB_ERR_0(rval);
  num_children = i;

  rval = iFace->num_parent_meshsets(set, &i);CHK_MB_ERR_0(rval);
  num_parents = i;

  rval = iFace->get_meshset_options(set, u);CHK_MB_ERR_0(rval);
  flags = u;

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_set_data(const WriteUtilIface::EntityListType which_data,
                                    const hid_t handle,
                                    IODebugTrack& track,
                                    Range* ranged,
                                    Range* null_stripped,
                                    std::vector<long>* set_sizes)
{
  // ranged must be non-null for CONTENTS and null for anything else
  assert((which_data == WriteUtilIface::CONTENTS) == (0 != ranged));
  ErrorCode rval;
  mhdf_Status status;

  debug_barrier();

  // Function pointer type used to write set data
  void (*write_func)(hid_t, long, long, hid_t, const void*, hid_t, mhdf_Status*);
  long max_vals; // Max over all procs of number of values to write to data set
  long offset;   // Offset in HDF5 dataset at which to write next block of data
  switch (which_data) {
    case WriteUtilIface::CONTENTS:
      assert(ranged != 0 && null_stripped != 0 && set_sizes != 0);
      write_func = &mhdf_writeSetDataWithOpt;
      max_vals = maxNumSetContents;
      offset = setContentsOffset;
      dbgOut.print(2, "Writing set contents\n");
      break;
    case WriteUtilIface::CHILDREN:
      assert(!ranged && !null_stripped && !set_sizes);
      write_func = &mhdf_writeSetParentsChildrenWithOpt;
      max_vals = maxNumSetChildren;
      offset = setChildrenOffset;
      dbgOut.print(2, "Writing set child lists\n");
      break;
    case WriteUtilIface::PARENTS:
      assert(!ranged && !null_stripped && !set_sizes);
      write_func = &mhdf_writeSetParentsChildrenWithOpt;
      max_vals = maxNumSetParents;
      offset = setParentsOffset;
      dbgOut.print(2, "Writing set parent lists\n");
      break;
    default:
      assert(false);
      return MB_FAILURE;
  }
  //assert(max_vals > 0); // Should have skipped this function otherwise

  // buffer to use for IO
  wid_t* buffer = reinterpret_cast<wid_t*>(dataBuffer);
  // number of handles that will fit in the buffer
  const size_t buffer_size = bufferSize / sizeof(EntityHandle);
  // the total number of write calls that must be made, including no-ops for collective io
  const size_t num_total_writes = (max_vals + buffer_size-1)/buffer_size;

  std::vector<SpecialSetData>::iterator si = specialSets.begin();

  std::vector<wid_t> remaining; // data left over from prev iteration because it didn't fit in buffer
  size_t remaining_offset = 0; // avoid erasing from front of 'remaining'
  const EntityHandle* remaining_ptr = 0; // remaining for non-ranged data
  size_t remaining_count = 0;
  const wid_t* special_rem_ptr = 0;
  Range::const_iterator i = setSet.range.begin(), j, rhint, nshint;
  if (ranged) rhint = ranged->begin();
  if (null_stripped) nshint = null_stripped->begin();
  for (size_t w = 0; w < num_total_writes; ++w) {
    if (i == setSet.range.end() && !remaining.empty() && !remaining_ptr) { 
      // If here, then we've written everything but we need to
      // make more write calls because we're doing collective IO
      // in parallel
      (*write_func)(handle, 0, 0, id_type, 0, writeProp, &status);CHK_MHDF_ERR_0(status);
      continue;
    }

    // If we had some left-over data from a range-compacted set
    // from the last iteration, add it to the buffer now
    size_t count = 0;
    if (!remaining.empty()) {
      count = remaining.size() - remaining_offset;
      if (count > buffer_size) {
        memcpy(buffer, &remaining[remaining_offset], buffer_size*sizeof(wid_t));
        count = buffer_size;
        remaining_offset += buffer_size;
      }
      else {
        memcpy(buffer, &remaining[remaining_offset], count*sizeof(wid_t));
        remaining_offset = 0;
        remaining.clear();
      }
    }
    // If we had some left-over data from a non-range-compacted set
    // from the last iteration, add it to the buffer now
    else if (remaining_ptr) {
      if (remaining_count > buffer_size) {
        rval = vector_to_id_list(remaining_ptr, buffer, buffer_size);CHK_MB_ERR_0(rval);
        count = buffer_size;
        remaining_ptr += count;
        remaining_count -= count;
      }
      else {
        rval = vector_to_id_list(remaining_ptr, buffer, remaining_count);CHK_MB_ERR_0(rval);
        count = remaining_count;
        remaining_ptr = 0;
        remaining_count = 0;
      }
    }
    // If we had some left-over data from a "special" (i.e. parallel shared)
    // set.
    else if (special_rem_ptr) {
      if (remaining_count > buffer_size) {
        memcpy(buffer, special_rem_ptr, buffer_size*sizeof(wid_t));
        count = buffer_size;
        special_rem_ptr += count;
        remaining_count -= count;
      }
      else {
        memcpy(buffer, special_rem_ptr, remaining_count*sizeof(wid_t));
        count = remaining_count;
        special_rem_ptr = 0;
        remaining_count = 0;
      }
    }

    // While there is both space remaining in the buffer and
    // more sets to write, append more set data to buffer.

    while (count < buffer_size && i != setSet.range.end()) {
      // Special case for "special" (i.e. parallel shared) sets:
      // we already have the data in a vector, just copy it.
      if (si != specialSets.end() && si->setHandle == *i) {
        std::vector<wid_t>& list =
          (which_data == WriteUtilIface::CONTENTS) ? si->contentIds :
          (which_data == WriteUtilIface::PARENTS) ? si->parentIds  :
                                                    si->childIds   ;
        size_t append = list.size();
        if (count + list.size() > buffer_size) {
          append = buffer_size - count;
          special_rem_ptr = &list[append];
          remaining_count = list.size() - append;
        }
        memcpy(buffer+count, &list[0], append*sizeof(wid_t));
        ++i;
        ++si;
        count += append;
        continue;
      }

      j = i;
      ++i;
      const EntityHandle* ptr;
      int len;
      unsigned char flags;
      rval = writeUtil->get_entity_list_pointers(j, i, &ptr, which_data, &len, &flags);
      if (MB_SUCCESS != rval)
        return rval;
      if (which_data == WriteUtilIface::CONTENTS && !(flags & MESHSET_ORDERED)) {
        bool compacted;
        remaining.clear();
        if (len == 0)
          compacted = false;
        else {
          assert(!(len % 2));
          rval = range_to_blocked_list(ptr, len / 2, remaining, compacted);
          if (MB_SUCCESS != rval) return rval;
        }
        if (compacted) {
          rhint = ranged->insert(rhint, *j);
          set_sizes->push_back(remaining.size());
        }
        else if (remaining.size() != (unsigned)len) {
          nshint = null_stripped->insert(nshint, *j);
          set_sizes->push_back(remaining.size());
        }

        if (count + remaining.size() <= buffer_size) {
          if (!remaining.empty())
            memcpy(buffer + count, &remaining[0], sizeof(wid_t)*remaining.size());
          count += remaining.size();
          remaining.clear();
          remaining_offset = 0;
        }
        else {
          remaining_offset = buffer_size - count;
          memcpy(buffer + count, &remaining[0], sizeof(wid_t)*remaining_offset);
          count += remaining_offset;
        }
      }
      else {
        if (count + len > buffer_size) {
          size_t append = buffer_size - count;
          remaining_ptr = ptr + append;
          remaining_count = len - append;
          len = append;
        }

        rval = vector_to_id_list(ptr, buffer + count, len);
        count += len;
      }
    }

    // Write the buffer.
    (*write_func)(handle, offset, count, id_type, buffer, writeProp, &status);CHK_MHDF_ERR_0(status);
    track.record_io(offset, count);
    offset += count;
  }

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_sets(double* times)
{
  mhdf_Status status;
  ErrorCode rval;
  long first_id, size;
  hid_t table;
  CpuTimer timer;

  CHECK_OPEN_HANDLES;
  /* If no sets, just return success */
  if (!writeSets)
    return MB_SUCCESS;

  debug_barrier();
  dbgOut.printf(2, "Writing %lu non-shared sets\n", (unsigned long)setSet.range.size());
  dbgOut.print(3, "Non-shared sets", setSet.range);

  /* Write set parents */
  if (writeSetParents) {
    topState.start("writing parent lists for local sets");
    table = mhdf_openSetParents(filePtr, &size, &status);CHK_MHDF_ERR_0(status);
    IODebugTrack track(debugTrack, "SetParents", size);

    rval = write_set_data(WriteUtilIface::PARENTS, table, track);topState.end(rval);CHK_MB_ERR_1(rval, table, status);

    mhdf_closeData(filePtr, table, &status);CHK_MHDF_ERR_0(status);

    times[SET_PARENT] = timer.time_elapsed();
    track.all_reduce();
  }

  /* Write set children */
  if (writeSetChildren) {
    topState.start("writing child lists for local sets");
    table = mhdf_openSetChildren(filePtr, &size, &status);CHK_MHDF_ERR_0(status);
    IODebugTrack track(debugTrack, "SetChildren", size);

    rval = write_set_data(WriteUtilIface::CHILDREN, table, track);topState.end(rval);CHK_MB_ERR_1(rval, table, status);

    mhdf_closeData(filePtr, table, &status);CHK_MHDF_ERR_0(status);

    times[SET_CHILD] = timer.time_elapsed();
    track.all_reduce();
  }

  /* Write set contents */
  Range ranged_sets, null_stripped_sets;
  std::vector<long> set_sizes;
  if (writeSetContents) {
    topState.start("writing content lists for local sets");
    table = mhdf_openSetData(filePtr, &size, &status);CHK_MHDF_ERR_0(status);
    IODebugTrack track(debugTrack, "SetContents", size);

    rval = write_set_data(WriteUtilIface::CONTENTS, table, track,
                          &ranged_sets, &null_stripped_sets, &set_sizes);topState.end(rval);CHK_MB_ERR_1(rval, table, status);

    mhdf_closeData(filePtr, table, &status);CHK_MHDF_ERR_0(status);

    times[SET_CONTENT] = timer.time_elapsed();
    track.all_reduce();
  }
  assert(ranged_sets.size() + null_stripped_sets.size() == set_sizes.size());

  /* Write set description table */

  debug_barrier();
  topState.start("writing descriptions of local sets");
  dbgOut.printf(2, "Writing %lu non-shared sets\n", (unsigned long)setSet.range.size());
  dbgOut.print(3, "Non-shared sets", setSet.range);

  /* Open the table */
  table = mhdf_openSetMeta(filePtr, &size, &first_id, &status);CHK_MHDF_ERR_0(status);
  IODebugTrack track_meta(debugTrack, "SetMeta", size);

  /* Some debug stuff */
  debug_barrier();
  dbgOut.printf(2, "Writing %lu non-shared sets\n", (unsigned long)setSet.range.size());
  dbgOut.print(3, "Non-shared sets", setSet.range);

  /* Counts and buffers and such */
  mhdf_index_t* const buffer = reinterpret_cast<mhdf_index_t*>(dataBuffer);
  const size_t buffer_size = bufferSize / (4*sizeof(mhdf_index_t));
  const size_t num_local_writes = (setSet.range.size() + buffer_size - 1) / buffer_size;
  const size_t num_global_writes = (setSet.max_num_ents + buffer_size - 1) / buffer_size;
  assert(num_local_writes <= num_global_writes);
  assert(num_global_writes > 0);

  /* data about sets for which number of handles written is
   * not the same as the number of handles in the set
   * (range-compacted or null handles stripped out)
   */
  Range::const_iterator i = setSet.range.begin();
  Range::const_iterator r = ranged_sets.begin();
  Range::const_iterator s = null_stripped_sets.begin();
  std::vector<mhdf_index_t>::const_iterator n = set_sizes.begin();
  assert(ranged_sets.size() + null_stripped_sets.size() == set_sizes.size());

  /* We write the end index for each list, rather than the count */
  mhdf_index_t prev_contents_end = setContentsOffset - 1;
  mhdf_index_t prev_children_end = setChildrenOffset - 1;
  mhdf_index_t prev_parents_end = setParentsOffset - 1;

  /* While there is more data to write */
  size_t offset = setSet.offset;
  std::vector<SpecialSetData>::const_iterator si = specialSets.begin();
  for (size_t w = 0; w < num_local_writes; ++w) {
    // Get a buffer full of data
    size_t count = 0;
    while (count < buffer_size && i != setSet.range.end()) {
      // Get set properties
      long num_ent, num_child, num_parent;
      unsigned long flags;
      if (si != specialSets.end() && si->setHandle == *i) {
        flags = si->setFlags;
        num_ent = si->contentIds.size();
        num_child = si->childIds.size();
        num_parent = si->parentIds.size();
        ++si;
        if (r != ranged_sets.end() && *i == *r) {
          assert(flags & mhdf_SET_RANGE_BIT);
          ++r;
          ++n;
        }
        else if (s != null_stripped_sets.end() && *i == *s) {
           ++s;
          ++n;
        }
      }
      else {
        assert(si == specialSets.end() || si->setHandle > *i);

        // Get set properties
        rval = get_set_info(*i, num_ent, num_child, num_parent, flags);CHK_MB_ERR_1(rval, table, status);

        // Check if size is something other than num handles in set
        if (r != ranged_sets.end() && *i == *r) {
          num_ent = *n;
          ++r;
          ++n;
          flags |= mhdf_SET_RANGE_BIT;
        }
        else if (s != null_stripped_sets.end() && *i == *s) {
          num_ent = *n;
          ++s;
          ++n;
        }
      }

      // Put data in buffer
      mhdf_index_t* local = buffer + 4*count;
      prev_contents_end += num_ent;
      prev_children_end += num_child;
      prev_parents_end += num_parent;
      local[0] = prev_contents_end;
      local[1] = prev_children_end;
      local[2] = prev_parents_end;
      local[3] = flags;

      // Iterate
      ++count;
      ++i;
    }

    // Write the data
    mhdf_writeSetMetaWithOpt(table, offset, count, MHDF_INDEX_TYPE, buffer, writeProp, &status);CHK_MHDF_ERR_1(status, table);
    track_meta.record_io(offset, count);
    offset += count;
  }
  assert(r == ranged_sets.end());
  assert(s == null_stripped_sets.end());
  assert(n == set_sizes.end());

  /* If doing parallel write with collective IO, do null write
   * calls because other procs aren't done yet and write calls
   * are collective */
  for (size_t w = num_local_writes; w != num_global_writes; ++w) {
    mhdf_writeSetMetaWithOpt(table, 0, 0, MHDF_INDEX_TYPE, 0, writeProp, &status);CHK_MHDF_ERR_1(status, table);
  }

  topState.end();
  mhdf_closeData(filePtr, table, &status);CHK_MHDF_ERR_0(status);

  times[SET_META] = timer.time_elapsed();
  track_meta.all_reduce();

  return MB_SUCCESS;
}

template <class HandleRangeIter> inline
size_t count_num_handles(HandleRangeIter iter, HandleRangeIter end)
{
  size_t result = 0;
  for ( ; iter != end; ++iter)
    result += iter->second - iter->first + 1;

  return result;
}

template <class HandleRangeIter> inline
ErrorCode range_to_id_list_templ(HandleRangeIter begin,
                                 HandleRangeIter end,
                                 const RangeMap<EntityHandle, WriteHDF5::wid_t>& idMap,
                                 WriteHDF5::wid_t* array)
{
  ErrorCode rval = MB_SUCCESS;
  RangeMap<EntityHandle, WriteHDF5::wid_t>::iterator ri = idMap.begin();
  WriteHDF5::wid_t* i = array;
  for (HandleRangeIter pi = begin; pi != end; ++pi) {
    EntityHandle h = pi->first;
    while (h <= pi->second) {
      ri = idMap.lower_bound(ri, idMap.end(), h);
      if (ri == idMap.end() || ri->begin > h) {
        rval = MB_ENTITY_NOT_FOUND;
        *i = 0; 
        ++i;
        ++h;
        continue;
      }

      WriteHDF5::wid_t n = pi->second - h + 1;
      if (n > ri->count)
        n = ri->count;

      WriteHDF5::wid_t id = ri->value + (h - ri->begin);
      for (WriteHDF5::wid_t j = 0; j < n; ++i, ++j)
        *i = id + j;
      h += n;
    }
  }

  assert(i == array + count_num_handles(begin, end));
  return rval;
}

template <class HandleRangeIter> inline
ErrorCode range_to_blocked_list_templ(HandleRangeIter begin,
                                      HandleRangeIter end,
                                      const RangeMap<EntityHandle, WriteHDF5::wid_t>& idMap,
                                      std::vector<WriteHDF5::wid_t>& output_id_list,
                                      bool& ranged_list)
{
  output_id_list.clear();
  if (begin == end) {
    ranged_list = false;
    return MB_SUCCESS;
  }

  // First try ranged format, but give up if we reach the
  // non-range format size.
  RangeMap<EntityHandle, WriteHDF5::wid_t>::iterator ri = idMap.begin();

  const size_t num_handles = count_num_handles(begin, end);
  // If we end up with more than this many range blocks, then
  // we're better off just writing the set as a simple list
  size_t pairs_remaining = num_handles / 2;
  for (HandleRangeIter pi = begin; pi != end; ++pi) {
    EntityHandle h = pi->first;
    while (h <= pi->second) {
      ri = idMap.lower_bound(ri, idMap.end(), h);
      if (ri == idMap.end() || ri->begin > h) {
        ++h;
        continue;
      }

      WriteHDF5::wid_t n = pi->second - pi->first + 1;
      if (n > ri->count)
        n = ri->count;

      // See if we can append it to the previous range
      WriteHDF5::wid_t id = ri->value + (h - ri->begin);
      if (!output_id_list.empty() &&
          output_id_list[output_id_list.size()-2] + output_id_list.back() == id) {
        output_id_list.back() += n;
      }

      // If we ran out of space, (or set is empty) just do list format
      else if (!pairs_remaining) {
        ranged_list = false;
        output_id_list.resize(num_handles);
        range_to_id_list_templ(begin, end, idMap, &output_id_list[0]);
        output_id_list.erase(std::remove(output_id_list.begin(),
                                         output_id_list.end(),
                                         0u),
                             output_id_list.end());
        return MB_SUCCESS;
      }

      //
      else {
        --pairs_remaining;
        output_id_list.push_back(id);
        output_id_list.push_back(n);
      }
      h += n;
    }
  }

  ranged_list = true;
  return MB_SUCCESS;
}

ErrorCode WriteHDF5::range_to_blocked_list(const Range& input_range,
                                           std::vector<wid_t>& output_id_list,
                                           bool& ranged_list)
{
  return range_to_blocked_list_templ(input_range.const_pair_begin(),
                                     input_range.const_pair_end(),
                                     idMap, output_id_list, ranged_list);
}

ErrorCode WriteHDF5::range_to_blocked_list(const EntityHandle* array,
                                           size_t num_input_ranges,
                                           std::vector<wid_t>& output_id_list,
                                           bool& ranged_list)
{
  // We assume this in the cast on the following line
  typedef std::pair<EntityHandle, EntityHandle> mtype;
  assert(sizeof(mtype) == 2*sizeof(EntityHandle));
  const mtype* arr = reinterpret_cast<const mtype*>(array);
  return range_to_blocked_list_templ(arr, arr + num_input_ranges,
                                     idMap, output_id_list, ranged_list);
}

ErrorCode WriteHDF5::range_to_id_list(const Range& range,
                                      wid_t* array)
{
  return range_to_id_list_templ(range.const_pair_begin(),
                                range.const_pair_end(),
                                idMap, array);
}

ErrorCode WriteHDF5::vector_to_id_list(const EntityHandle* input,
                                       size_t input_len,
                                       wid_t* output,
                                       size_t& output_len,
                                       bool remove_zeros)
{
  const EntityHandle* i_iter = input;
  const EntityHandle* i_end = input + input_len;
  wid_t* o_iter = output;
  for ( ; i_iter != i_end; ++i_iter) {
    wid_t id = idMap.find(*i_iter);
    if (!remove_zeros || id != 0) {
      *o_iter = id;
      ++o_iter;
    }
  }
  output_len = o_iter - output;

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::vector_to_id_list(const std::vector<EntityHandle>& input,
                                       std::vector<wid_t>& output,
                                       bool remove_zeros)
{
  output.resize(input.size());
  size_t output_size = 0;
  ErrorCode rval = vector_to_id_list(&input[0], input.size(),
                                     &output[0], output_size,
                                     remove_zeros);
  output.resize(output_size);
  return rval;
}

ErrorCode WriteHDF5::vector_to_id_list(const EntityHandle* input,
                                       wid_t* output,
                                       size_t count)
{
  size_t output_len;
  return vector_to_id_list(input, count, output, output_len, false);
}

inline ErrorCode WriteHDF5::get_adjacencies(EntityHandle entity,
                                            std::vector<wid_t>& adj)
{
  const EntityHandle* adj_array;
  int num_adj;
  ErrorCode rval = writeUtil->get_adjacencies(entity, adj_array, num_adj);
  if (MB_SUCCESS != rval)
    return error(rval);

  size_t j = 0;
  adj.resize(num_adj);
  for (int i = 0; i < num_adj; ++i) 
    if (wid_t id = idMap.find(adj_array[i]))
      adj[j++] = id;
  adj.resize(j);

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_adjacencies(const ExportSet& elements)
{
  ErrorCode rval;
  mhdf_Status status;
  Range::const_iterator iter;
  const Range::const_iterator end = elements.range.end();
  std::vector<wid_t> adj_list;

  CHECK_OPEN_HANDLES;

  debug_barrier();

  /* Count Adjacencies */
  long count = 0;
  //for (iter = elements.range.begin(); iter != end; ++iter) {
  //  adj_list.clear();
  //  rval = get_adjacencies(*iter, adj_list);CHK_MB_ERR_0(rval);
  //
  //  if (adj_list.size() > 0)
  //    count += adj_list.size() + 2;
  //}

  //if (count == 0)
  //  return MB_SUCCESS;

  long offset = elements.adj_offset;
  if (elements.max_num_adjs == 0)
    return MB_SUCCESS;

  /* Create data list */
  hid_t table = mhdf_openAdjacency(filePtr, elements.name(), &count, &status);CHK_MHDF_ERR_0(status);
  IODebugTrack track(debugTrack, "Adjacencies", count);

  /* Write data */
  wid_t* buffer = (wid_t*)dataBuffer;
  long chunk_size = bufferSize / sizeof(wid_t);
  long num_writes = (elements.max_num_adjs + chunk_size - 1) / chunk_size;
  (void)VALGRIND_MAKE_MEM_UNDEFINED(dataBuffer, bufferSize);
  count = 0;
  for (iter = elements.range.begin(); iter != end; ++iter) {
    adj_list.clear();
    rval = get_adjacencies(*iter, adj_list);CHK_MB_ERR_1(rval, table, status);
    if (adj_list.size() == 0)
      continue;

    // If buffer is full, flush it
    if (count + adj_list.size() + 2 > (unsigned long)chunk_size) {
      dbgOut.print(3, " writing adjacency chunk.\n");
      track.record_io(offset, count);
      mhdf_writeAdjacencyWithOpt(table, offset, count, id_type, buffer, writeProp, &status);CHK_MHDF_ERR_1(status, table);
      (void)VALGRIND_MAKE_MEM_UNDEFINED(dataBuffer, bufferSize);

      offset += count;
      count = 0;
    }

    buffer[count++] = idMap.find(*iter);
    buffer[count++] = adj_list.size();

    assert (adj_list.size() + 2 < (unsigned long)chunk_size);
    memcpy(buffer + count, &adj_list[0], adj_list.size() * sizeof(wid_t));
    count += adj_list.size();
  }

  if (count) {
    dbgOut.print(2, " writing final adjacency chunk.\n");
    mhdf_writeAdjacencyWithOpt(table, offset, count, id_type, buffer, writeProp, &status);CHK_MHDF_ERR_1(status, table);

    offset += count;
    count = 0;
    --num_writes;
  }

  // Do empty writes if necessary for parallel collective IO
  if (collectiveIO) {
    while (num_writes > 0) {
      --num_writes;
      assert(writeProp != H5P_DEFAULT);
      dbgOut.print(2, " writing empty adjacency chunk.\n");
      mhdf_writeAdjacencyWithOpt(table, offset, 0, id_type, 0, writeProp, &status);CHK_MHDF_ERR_1(status, table);
    }
  }

  mhdf_closeData(filePtr, table, &status);CHK_MHDF_ERR_0(status);

  track.all_reduce();
  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_tag(const TagDesc& tag_data,
                               double* times)
{
  std::string name;
  ErrorCode rval = iFace->tag_get_name(tag_data.tag_id, name);
  if (MB_SUCCESS != rval)
    return error(rval);

  CHECK_OPEN_HANDLES;
  debug_barrier();
  dbgOut.tprintf(1, "Writing tag: \"%s\"\n", name.c_str());

  int moab_size, elem_size, array_len;
  DataType moab_type;
  mhdf_TagDataType mhdf_type;
  hid_t hdf5_type;
  rval = get_tag_size(tag_data.tag_id, moab_type, moab_size, elem_size,
                      array_len, mhdf_type, hdf5_type);
  if (MB_SUCCESS != rval)
    return error(rval);

  CpuTimer timer;
  if (array_len == MB_VARIABLE_LENGTH && tag_data.write_sparse) {
    dbgOut.printf(2, "Writing sparse data for var-len tag: \"%s\"\n", name.c_str());
    rval = write_var_len_tag(tag_data, name, moab_type, hdf5_type, elem_size);
    times[VARLEN_TAG_TIME] += timer.time_elapsed();
  }
  else {
    int data_len = elem_size;
    if (moab_type != MB_TYPE_BIT)
      data_len *= array_len;
    if (tag_data.write_sparse) {
      dbgOut.printf(2, "Writing sparse data for tag: \"%s\"\n", name.c_str());
      rval = write_sparse_tag(tag_data, name, moab_type, hdf5_type, data_len);
      times[SPARSE_TAG_TIME] += timer.time_elapsed();
    }
    for (size_t i = 0; MB_SUCCESS == rval && i < tag_data.dense_list.size(); ++i) {
      const ExportSet* set = find(tag_data.dense_list[i]);
      assert(0 != set);
      debug_barrier();
      dbgOut.printf(2, "Writing dense data for tag: \"%s\" on group \"%s\"\n", name.c_str(), set->name());
      subState.start("writing dense data for tag: ", (name + ":" + set->name()).c_str());
      rval = write_dense_tag(tag_data, *set, name, moab_type, hdf5_type, data_len);
      subState.end(rval);
    }
    times[DENSE_TAG_TIME] += timer.time_elapsed();
  }

  H5Tclose(hdf5_type);
  return MB_SUCCESS == rval ? MB_SUCCESS : error(rval);
}

ErrorCode WriteHDF5::write_sparse_ids(const TagDesc& tag_data,
                                      const Range& range,
                                      hid_t id_table,
                                      size_t table_size,
                                      const char* name)
{
  ErrorCode rval;
  mhdf_Status status;

  CHECK_OPEN_HANDLES;

  std::string tname(name ? name : "<UNKNOWN TAG?>");
  tname += " - Ids";
  IODebugTrack track(debugTrack, tname, table_size);

  // Set up data buffer for writing IDs
  size_t chunk_size = bufferSize / sizeof(wid_t);
  wid_t* id_buffer = (wid_t*)dataBuffer;

  // Write IDs of tagged entities.
  long remaining = range.size();
  long offset = tag_data.sparse_offset;
  long num_writes = (remaining + chunk_size - 1) / chunk_size;
  if (tag_data.max_num_ents) {
    assert(tag_data.max_num_ents >= (unsigned long)remaining);
    num_writes = (tag_data.max_num_ents + chunk_size - 1) / chunk_size;
  }
  Range::const_iterator iter = range.begin();
  while (remaining) {
    (void)VALGRIND_MAKE_MEM_UNDEFINED(dataBuffer, bufferSize);

    // Write "chunk_size" blocks of data
    long count = (unsigned long)remaining > chunk_size ? chunk_size : remaining;
    remaining -= count;
    Range::const_iterator stop = iter;
    stop += count;
    Range tmp;;
    tmp.merge(iter, stop);
    iter = stop;
    assert(tmp.size() == (unsigned)count);

    rval = range_to_id_list(tmp, id_buffer);CHK_MB_ERR_0(rval);

    // Write the data
    dbgOut.print(3, " writing sparse tag entity chunk.\n");
    track.record_io(offset, count);
    mhdf_writeSparseTagEntitiesWithOpt(id_table, offset, count, id_type,
                                       id_buffer, writeProp, &status);CHK_MHDF_ERR_0(status);

    offset += count;
    --num_writes;
  } // while (remaining)

  // Do empty writes if necessary for parallel collective IO
  if (collectiveIO) {
    while (num_writes--) {
      assert(writeProp != H5P_DEFAULT);
      dbgOut.print(3, " writing empty sparse tag entity chunk.\n");
      mhdf_writeSparseTagEntitiesWithOpt(id_table, offset, 0, id_type,
                                         0, writeProp, &status);CHK_MHDF_ERR_0(status);
    }
  }

  track.all_reduce();
  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_sparse_tag(const TagDesc& tag_data,
                                      const std::string& name,
                                      DataType mb_data_type,
                                      hid_t value_type,
                                      int value_type_size)
{
  ErrorCode rval;
  mhdf_Status status;
  hid_t tables[3];
  long table_size, data_size;

  CHECK_OPEN_HANDLES;

  // Get entities for which to write tag values
  Range range;
  rval = get_sparse_tagged_entities(tag_data, range);

  // Open tables to write info
  mhdf_openSparseTagData(filePtr,
                         name.c_str(),
                         &table_size,
                         &data_size,
                         tables,
                         &status);CHK_MHDF_ERR_0(status);
  assert(range.size() + tag_data.sparse_offset <= (unsigned long)table_size);
  // Fixed-length tag
  assert(table_size == data_size);

  // Write IDs for tagged entities
  subState.start("writing sparse ids for tag: ", name.c_str());
  rval = write_sparse_ids(tag_data, range, tables[0], table_size, name.c_str());subState.end(rval);CHK_MB_ERR_2(rval, tables, status);
  mhdf_closeData(filePtr, tables[0], &status);CHK_MHDF_ERR_1(status, tables[1]);

  // Set up data buffer for writing tag values
  IODebugTrack track(debugTrack, name + " Data", data_size);
  subState.start("writing sparse values for tag: ", name.c_str());
  rval = write_tag_values(tag_data.tag_id,
                          tables[1],
                          tag_data.sparse_offset,
                          range,
                          mb_data_type,
                          value_type,
                          value_type_size,
                          tag_data.max_num_ents,
                          track);subState.end(rval);CHK_MB_ERR_0(rval);
  mhdf_closeData(filePtr, tables[1], &status);CHK_MHDF_ERR_0(status);

  track.all_reduce();
  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_var_len_indices(const TagDesc& tag_data,
                                           const Range& range,
                                           hid_t idx_table,
                                           size_t table_size,
                                           int /*type_size*/,
                                           const char* name)
{
  ErrorCode rval;
  mhdf_Status status;

  CHECK_OPEN_HANDLES;

  std::string tname(name ? name : "<UNKNOWN TAG?>");
  tname += " - End Indices";
  IODebugTrack track(debugTrack, tname, table_size);

  // Set up data buffer for writing indices
  size_t chunk_size = bufferSize / (std::max(sizeof(void*), sizeof(long)) + sizeof(int));
  mhdf_index_t* idx_buffer = (mhdf_index_t*)dataBuffer;
  const void** junk = (const void**)dataBuffer;
  int* size_buffer = (int*)(dataBuffer + chunk_size*std::max(sizeof(void*), sizeof(mhdf_index_t)));

  // Write IDs of tagged entities.
  long data_offset = tag_data.var_data_offset - 1; // Offset at which to write data buffer
  size_t remaining = range.size();
  size_t offset = tag_data.sparse_offset;
  size_t num_writes = (remaining + chunk_size - 1) / chunk_size;
  if (tag_data.max_num_ents) {
    assert(tag_data.max_num_ents >= (unsigned long)remaining);
    num_writes = (tag_data.max_num_ents + chunk_size - 1) / chunk_size;
  }
  Range::const_iterator iter = range.begin();
  while (remaining) {
    (void)VALGRIND_MAKE_MEM_UNDEFINED(dataBuffer, bufferSize);

    // Write "chunk_size" blocks of data
    size_t count = remaining > chunk_size ? chunk_size : remaining;
    remaining -= count;
    Range::const_iterator stop = iter;
    stop += count;
    Range tmp;
    tmp.merge(iter, stop);
    iter = stop;
    assert(tmp.size() == (unsigned)count);

    rval = iFace->tag_get_by_ptr(tag_data.tag_id, tmp, junk, size_buffer);CHK_MB_ERR_0(rval);

    // Calculate end indices
    dbgOut.print(3, " writing var-len tag offset chunk.\n");
    track.record_io(offset, count);
    for (size_t i = 0; i < count; ++i) {
      data_offset += size_buffer[i];
      idx_buffer[i] = data_offset;
    }

    // Write
    mhdf_writeSparseTagIndicesWithOpt(idx_table, offset, count, MHDF_INDEX_TYPE,
                                      idx_buffer, writeProp, &status);CHK_MHDF_ERR_0(status);

    offset += count;
    --num_writes;
  } // while (remaining)

  // Do empty writes if necessary for parallel collective IO
  if (collectiveIO) {
    while (num_writes--) {
      assert(writeProp != H5P_DEFAULT);
      dbgOut.print(3, " writing empty sparse tag entity chunk.\n");
      mhdf_writeSparseTagIndicesWithOpt(idx_table, offset, 0, id_type,
                                        0, writeProp, &status);CHK_MHDF_ERR_0(status);
    }
  }

  track.all_reduce();
  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_var_len_data(const TagDesc& tag_data,
                                        const Range& range,
                                        hid_t table,
                                        size_t table_size,
                                        bool handle_tag,
                                        hid_t hdf_type,
                                        int type_size,
                                        const char* name)
{
  ErrorCode rval;
  mhdf_Status status;

  CHECK_OPEN_HANDLES;
  assert(!handle_tag || sizeof(EntityHandle) == type_size);

  std::string tname(name ? name : "<UNKNOWN TAG?>");
  tname += " - Values";
  IODebugTrack track(debugTrack, tname, table_size);

  const size_t buffer_size = bufferSize / type_size;

  size_t num_writes = (table_size + buffer_size - 1) / buffer_size;
  if (collectiveIO) {
    assert(tag_data.max_num_vals > 0);
    num_writes = (tag_data.max_num_vals + buffer_size - 1) / buffer_size;
  }

  unsigned char* buffer = (unsigned char*)dataBuffer;
  const void* prev_data = 0; // Data left over from prev iteration
  size_t prev_len = 0;
  Range::const_iterator iter = range.begin();
  long offset = tag_data.var_data_offset;
  while (prev_data || iter != range.end()) {
    size_t count = 0;
    if (prev_data) {
      size_t len;
      const void* ptr = prev_data;
      if (prev_len <= buffer_size) {
        len = prev_len;
        prev_data = 0;
        prev_len = 0;
      }
      else {
        len = buffer_size;
        prev_data = ((const char*)prev_data) + buffer_size*type_size;
        prev_len -= buffer_size;
      }

      if (handle_tag)
        convert_handle_tag((const EntityHandle*)ptr, (EntityHandle*)buffer, len);
      else
        memcpy(buffer, ptr, len * type_size);
    }

    for ( ; count < buffer_size && iter != range.end(); ++iter) {
      int len;
      const void* ptr;
      rval = iFace->tag_get_by_ptr(tag_data.tag_id, &*iter, 1, &ptr, &len);CHK_MB_ERR_0(rval);
      int bytes = len * type_size;
      if (len + count > buffer_size) {
        prev_len = len + count - buffer_size;
        prev_data = ((const char*)ptr) + prev_len*type_size;
        len = buffer_size - count;
      }

      if (handle_tag)
        convert_handle_tag((const EntityHandle*)ptr, ((EntityHandle*)buffer) + count, len);
      else
        memcpy(buffer + count*type_size, ptr, bytes);
      count += len;
    }

    track.record_io(offset, count);
    mhdf_writeTagValuesWithOpt(table, offset, count, hdf_type, buffer, writeProp, &status);CHK_MHDF_ERR_0(status);
    --num_writes;
  }

  // Do empty writes if necessary for parallel collective IO
  if (collectiveIO) {
    while (num_writes--) {
      assert(writeProp != H5P_DEFAULT);
      dbgOut.print(3, " writing empty var-len tag data chunk.\n");
      mhdf_writeTagValuesWithOpt(table, 0, 0, hdf_type, 0, writeProp, &status);CHK_MHDF_ERR_0(status);
    }
  }

  track.all_reduce();
  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_var_len_tag(const TagDesc& tag_data,
                                       const std::string& name,
                                       DataType mb_data_type,
                                       hid_t hdf_type,
                                       int type_size)
{
  ErrorCode rval;
  mhdf_Status status;
  hid_t tables[3];
  long table_size;
  long data_table_size;

  CHECK_OPEN_HANDLES;

  // Get entities for which to write tag values
  Range range;
  rval = get_sparse_tagged_entities(tag_data, range);

  // Open tables to write info
  mhdf_openSparseTagData(filePtr,
                         name.c_str(),
                         &table_size,
                         &data_table_size,
                         tables,
                         &status);CHK_MHDF_ERR_0(status);
  assert(range.size() + tag_data.sparse_offset <= (unsigned long)table_size);

  // Write IDs for tagged entities
  subState.start("writing ids for var-len tag: ", name.c_str());
  rval = write_sparse_ids(tag_data, range, tables[0], table_size, name.c_str());subState.end(rval);CHK_MB_ERR_2(rval, tables, status);
  mhdf_closeData(filePtr, tables[0], &status);CHK_MHDF_ERR_2(status, tables + 1);

  // Write offsets for tagged entities
  subState.start("writing indices for var-len tag: ", name.c_str());
  rval = write_var_len_indices(tag_data, range, tables[2], table_size, type_size, name.c_str());subState.end(rval);CHK_MB_ERR_1(rval, tables[1], status);
  mhdf_closeData(filePtr, tables[2], &status);CHK_MHDF_ERR_1(status, tables[1]);

  // Write the actual tag data
  subState.start("writing values for var-len tag: ", name.c_str());
  rval = write_var_len_data(tag_data, range, tables[1], data_table_size,
                            mb_data_type == MB_TYPE_HANDLE,
                            hdf_type, type_size, name.c_str());subState.end(rval);CHK_MB_ERR_0(rval);
  mhdf_closeData(filePtr, tables[1], &status);CHK_MHDF_ERR_0(status);

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_dense_tag(const TagDesc& tag_data,
                                     const ExportSet& elem_data,
                                     const std::string& name,
                                     DataType mb_data_type,
                                     hid_t value_type,
                                     int value_type_size)
{
  CHECK_OPEN_HANDLES;

  // Open tables to write info
  mhdf_Status status;
  long table_size;
  hid_t table = mhdf_openDenseTagData(filePtr,
                                      name.c_str(),
                                      elem_data.name(),
                                      &table_size,
                                      &status);CHK_MHDF_ERR_0(status);
  assert(elem_data.range.size() + elem_data.offset <= (unsigned long)table_size);

  IODebugTrack track(debugTrack, name + " " + elem_data.name() + " Data", table_size);
  ErrorCode rval = write_tag_values(tag_data.tag_id,
                                    table,
                                    elem_data.offset,
                                    elem_data.range,
                                    mb_data_type,
                                    value_type,
                                    value_type_size,
                                    elem_data.max_num_ents,
                                    track);CHK_MB_ERR_0(rval);
  mhdf_closeData(filePtr, table, &status);CHK_MHDF_ERR_0(status);

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_tag_values(Tag tag_id,
                                      hid_t data_table,
                                      unsigned long offset_in,
                                      const Range& range_in,
                                      DataType mb_data_type,
                                      hid_t value_type,
                                      int value_type_size,
                                      unsigned long max_num_ents,
                                      IODebugTrack& track)
{
  mhdf_Status status;

  CHECK_OPEN_HANDLES;

  // Set up data buffer for writing tag values
  size_t chunk_size = bufferSize / value_type_size;
  assert(chunk_size > 0);
  char* tag_buffer = (char*)dataBuffer;

  // Write the tag values
  size_t remaining = range_in.size();
  size_t offset = offset_in;
  Range::const_iterator iter = range_in.begin();
  long num_writes = (remaining + chunk_size - 1) / chunk_size;
  if (max_num_ents) {
    assert(max_num_ents >= remaining);
    num_writes = (max_num_ents + chunk_size - 1) / chunk_size;
  }
  while (remaining) {
    (void)VALGRIND_MAKE_MEM_UNDEFINED(dataBuffer, bufferSize);

    // Write "chunk_size" blocks of data
    long count = (unsigned long)remaining > chunk_size ? chunk_size : remaining;
    remaining -= count;
    memset(tag_buffer, 0, count * value_type_size);
    Range::const_iterator stop = iter;
    stop += count;
    Range range;
    range.merge(iter, stop);
    iter = stop;
    assert(range.size() == (unsigned)count);

    ErrorCode rval = iFace->tag_get_data(tag_id, range, tag_buffer);CHK_MB_ERR_0(rval);

    // Convert EntityHandles to file ids
    if (mb_data_type == MB_TYPE_HANDLE)
      convert_handle_tag(reinterpret_cast<EntityHandle*>(tag_buffer),
                         count * value_type_size / sizeof(EntityHandle));

    // Write the data
    dbgOut.print(2, " writing tag value chunk.\n");
    track.record_io(offset, count);
    assert(value_type > 0);
    mhdf_writeTagValuesWithOpt(data_table, offset, count,
                               value_type, tag_buffer, writeProp, &status);CHK_MHDF_ERR_0(status);

    offset += count;
    --num_writes;
  } // while (remaining)

  // Do empty writes if necessary for parallel collective IO
  if (collectiveIO) {
    while (num_writes--) {
      assert(writeProp != H5P_DEFAULT);
      dbgOut.print(2, " writing empty tag value chunk.\n");
      assert(value_type > 0);
      mhdf_writeTagValuesWithOpt(data_table, offset, 0,
                                 value_type, 0, writeProp, &status);CHK_MHDF_ERR_0(status);
    }
  }

  track.all_reduce();
  return MB_SUCCESS;
}

ErrorCode WriteHDF5::write_qa(const std::vector<std::string>& list)
{
  const char* app = "MOAB";
  const char* vers = MOAB_VERSION;
  char date_str[64];
  char time_str[64];

  CHECK_OPEN_HANDLES;

  std::vector<const char*> strs(list.size() ? list.size() : 4);
  if (list.size() == 0) {
    time_t t = time(NULL);
    tm* lt = localtime(&t);
#ifdef WIN32
    strftime(date_str, sizeof(date_str), "%m/%d/%y", lt); //VS 2008 does not support %D
    strftime(time_str, sizeof(time_str), "%H:%M:%S", lt); //VS 2008 does not support %T
#else
    strftime(date_str, sizeof(date_str), "%D", lt);
    strftime(time_str, sizeof(time_str), "%T", lt);
#endif

    strs[0] = app;
    strs[1] = vers;
    strs[2] = date_str;
    strs[3] = time_str;
  }
  else {
    for (unsigned int i = 0; i < list.size(); ++i)
      strs[i] = list[i].c_str();
  }

  mhdf_Status status;
  dbgOut.print(2, " writing QA history.\n");
  mhdf_writeHistory(filePtr, &strs[0], strs.size(), &status);CHK_MHDF_ERR_0(status);

  return MB_SUCCESS;
}

/*
ErrorCode WriteHDF5::register_known_tag_types(Interface* iface)
{
  hid_t int4, double16;
  hsize_t dim[1];
  int error = 0;
  ErrorCode rval;

  dim[0] = 4;
  int4 = H5Tarray_create(H5T_NATIVE_INT, 1, dim, NULL);

  dim[0] = 16;
  double16 = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, dim, NULL);

  if (int4 < 0 || double16 < 0)
    error = 1;

  struct { const char* name; hid_t type; } list[] = {
    { GLOBAL_ID_TAG_NAME, H5T_NATIVE_INT } ,
    { MATERIAL_SET_TAG_NAME, H5T_NATIVE_INT },
    { DIRICHLET_SET_TAG_NAME, H5T_NATIVE_INT },
    { NEUMANN_SET_TAG_NAME, H5T_NATIVE_INT },
    { HAS_MID_NODES_TAG_NAME, int4 },
    { GEOM_DIMENSION_TAG_NAME, H5T_NATIVE_INT },
    { MESH_TRANSFORM_TAG_NAME, double16 },
    { 0, 0 } };

  for (int i = 0; list[i].name; ++i) {
    if (list[i].type < 1) {
      ++error;
      continue;
    }

    Tag handle;

    std::string name("__hdf5_tag_type_");
    name += list[i].name;

    rval = iface->tag_get_handle(name.c_str(), handle);
    if (MB_TAG_NOT_FOUND == rval) {
      rval = iface->tag_create(name.c_str(), sizeof(hid_t), MB_TAG_SPARSE, handle, NULL);
      if (MB_SUCCESS != rval) {
        ++error;
        continue;
      }

      hid_t copy_id = H5Tcopy(list[i].type);
      const EntityHandle mesh = 0;
      rval = iface->tag_set_data(handle, &mesh, 1, &copy_id);
      if (MB_SUCCESS != rval) {
        ++error;
        continue;
      }
    }
  }

  H5Tclose(int4);
  H5Tclose(double16);
  return error ? MB_FAILURE : MB_SUCCESS;
}
*/

ErrorCode WriteHDF5::gather_tags(const Tag* user_tag_list, int num_tags)
{
  ErrorCode result;
  std::vector<Tag> tag_list;
  std::vector<Tag>::iterator t_itor;
  Range range;

  // Get list of Tags to write
  result = writeUtil->get_tag_list(tag_list, user_tag_list, num_tags);CHK_MB_ERR_0(result);

  // Get list of tags
  for (t_itor = tag_list.begin(); t_itor != tag_list.end(); ++t_itor) {
    // Add tag to export list
    TagDesc tag_data; tag_data.write_sparse = false;
    tag_data.tag_id = *t_itor;
    tag_data.sparse_offset = 0;
    tag_data.var_data_offset = 0;
    tag_data.max_num_ents = 0;
    tag_data.max_num_vals = 0;
    tagList.push_back(tag_data);
  }

  return MB_SUCCESS;
}

// If we support parallel, then this function will have been
// overridden with an alternate version in WriteHDF5Parallel
// that supports parallel I/O.  If we're here
// then MOAB was not built with support for parallel HDF5 I/O.
ErrorCode WriteHDF5::parallel_create_file(const char* /* filename */,
                                          bool /* overwrite */,
                                          const std::vector<std::string>& /* qa_records */,
                                          const FileOptions& /* opts */,
                                          const Tag* /* tag_list */,
                                          int /* num_tags */,
                                          int /* dimension */,
                                          double* /* times */)
{
  MB_SET_ERR(MB_NOT_IMPLEMENTED, "WriteHDF5 does not support parallel writing");
}

ErrorCode WriteHDF5::serial_create_file(const char* filename,
                                        bool overwrite,
                                        const std::vector<std::string>& qa_records,
                                        const Tag* user_tag_list,
                                        int num_user_tags,
                                        int dimension)
{
  long first_id;
  mhdf_Status status;
  hid_t handle;
  std::list<ExportSet>::iterator ex_itor;
  ErrorCode rval;

  topState.start("creating file");

  const char* type_names[MBMAXTYPE];
  memset(type_names, 0, MBMAXTYPE * sizeof(char*));
  for (EntityType i = MBEDGE; i < MBENTITYSET; ++i)
    type_names[i] = CN::EntityTypeName(i);

  // Create the file
  filePtr = mhdf_createFile(filename, overwrite, type_names, MBMAXTYPE, id_type, &status);CHK_MHDF_ERR_0(status);
  assert(!!filePtr);

  rval = write_qa(qa_records);CHK_MB_ERR_0(rval);

  // Create node table
  if (nodeSet.range.size()) {
    nodeSet.total_num_ents = nodeSet.range.size();
    handle = mhdf_createNodeCoords(filePtr, dimension, nodeSet.total_num_ents,
                                   &first_id, &status);CHK_MHDF_ERR_0(status);
    mhdf_closeData(filePtr, handle, &status);CHK_MHDF_ERR_0(status);
    nodeSet.first_id = (wid_t)first_id;
    rval = assign_ids(nodeSet.range, nodeSet.first_id);CHK_MB_ERR_0(rval);
  }
  else {
    nodeSet.first_id = std::numeric_limits<wid_t>::max();
  }
  nodeSet.offset = 0;

  // Create element tables
  for (ex_itor = exportList.begin(); ex_itor != exportList.end(); ++ex_itor) {
    ex_itor->total_num_ents = ex_itor->range.size();
    rval = create_elem_table(*ex_itor, ex_itor->total_num_ents, first_id);CHK_MB_ERR_0(rval);

    ex_itor->first_id = (wid_t)first_id;
    ex_itor->offset = 0;
    rval = assign_ids(ex_itor->range, ex_itor->first_id);CHK_MB_ERR_0(rval);
  }
  // Create set tables
  writeSets = !setSet.range.empty();
  if (writeSets) {
    long contents_len, children_len, parents_len;

    setSet.total_num_ents = setSet.range.size();
    setSet.max_num_ents = setSet.total_num_ents;
    rval = create_set_meta(setSet.total_num_ents, first_id);CHK_MB_ERR_0(rval);

    setSet.first_id = (wid_t) first_id;
    rval = assign_ids(setSet.range, setSet.first_id);CHK_MB_ERR_0(rval);

    rval = count_set_size(setSet.range, contents_len, children_len,
                          parents_len);CHK_MB_ERR_0(rval);

    rval = create_set_tables(contents_len, children_len, parents_len);CHK_MB_ERR_0(rval);

    setSet.offset = 0;
    setContentsOffset = 0;
    setChildrenOffset = 0;
    setParentsOffset = 0;
    writeSetContents = !!contents_len;
    writeSetChildren = !!children_len;
    writeSetParents = !!parents_len;

    maxNumSetContents = contents_len;
    maxNumSetChildren = children_len;
    maxNumSetParents = parents_len;
  } // if (!setSet.range.empty())

  // Create adjacency table after set table, because sets do not have yet an id
  // some entities are adjacent to sets (exodus?)
  // Create node adjacency table
  wid_t num_adjacencies;
#ifdef MB_H5M_WRITE_NODE_ADJACENCIES
  rval = count_adjacencies(nodeSet.range, num_adjacencies);CHK_MB_ERR_0(rval);
  nodeSet.adj_offset = 0;
  nodeSet.max_num_adjs = num_adjacencies;
  if (num_adjacencies > 0) {
    handle = mhdf_createAdjacency(filePtr,
                                  mhdf_node_type_handle(),
                                  num_adjacencies,
                                  &status);CHK_MHDF_ERR_0(status);
    mhdf_closeData(filePtr, handle, &status);
  }
#endif

  // Create element adjacency tables
  for (ex_itor = exportList.begin(); ex_itor != exportList.end(); ++ex_itor) {
    rval = count_adjacencies(ex_itor->range, num_adjacencies);CHK_MB_ERR_0(rval);

    ex_itor->adj_offset = 0;
    ex_itor->max_num_adjs = num_adjacencies;
    if (num_adjacencies > 0) {
      handle = mhdf_createAdjacency(filePtr,
                                    ex_itor->name(),
                                    num_adjacencies,
                                    &status);CHK_MHDF_ERR_0(status);
      mhdf_closeData(filePtr, handle, &status);
    }
  }

  dbgOut.tprint(1, "Gathering Tags\n");

  rval = gather_tags(user_tag_list, num_user_tags);CHK_MB_ERR_0(rval);

  // Create the tags and tag data tables
  std::list<TagDesc>::iterator tag_iter = tagList.begin();
  for ( ; tag_iter != tagList.end(); ++tag_iter) {
    // As we haven't yet added any ExportSets for which to write
    // dense tag data to the TagDesc struct pointed to by
    // tag_iter, this call will initially return all tagged entities
    // in the set of entities to be written.
    Range range;
    rval = get_sparse_tagged_entities(*tag_iter, range);CHK_MB_ERR_0(rval);

    int s;
    bool var_len = (MB_VARIABLE_DATA_LENGTH == iFace->tag_get_length(tag_iter->tag_id, s));

      // Determine which ExportSets we want to write dense
      // data for. We never write dense data for variable-length
      // tag data.
    if (!var_len && writeTagDense) {
      // Check if we want to write this tag in dense format even if not
      // all of the entities have a tag value.  The criterion of this
      // is that the tag be dense, have a default value, and have at
      // least 2/3 of the entities tagged.
      bool prefer_dense = false;
      TagType type;
      rval = iFace->tag_get_type(tag_iter->tag_id, type);CHK_MB_ERR_0(rval);
      if (MB_TAG_DENSE == type) {
        const void* defval = 0;
        rval = iFace->tag_get_default_value(tag_iter->tag_id, defval, s);
        if (MB_SUCCESS == rval)
          prefer_dense = true;
      }

      if (check_dense_format_tag(nodeSet, range, prefer_dense)) {
        range -= nodeSet.range;
        tag_iter->dense_list.push_back(nodeSet);
      }

      std::list<ExportSet>::const_iterator ex = exportList.begin();
      for ( ; ex != exportList.end(); ++ex) {
        if (check_dense_format_tag(*ex, range, prefer_dense)) {
          range -= ex->range;
          tag_iter->dense_list.push_back(*ex);
        }
      }

      if (check_dense_format_tag(setSet, range, prefer_dense)) {
        range -= setSet.range;
        tag_iter->dense_list.push_back(setSet);
      }
    }

    tag_iter->write_sparse = !range.empty();

    unsigned long var_len_total = 0;
    if (var_len) {
      rval = get_tag_data_length(*tag_iter, range, var_len_total);CHK_MB_ERR_0(rval);
    }

    rval = create_tag(*tag_iter, range.size(), var_len_total);CHK_MB_ERR_0(rval);
  } // for (tags)

  topState.end();
  return MB_SUCCESS;
}

bool WriteHDF5::check_dense_format_tag(const ExportSet& ents,
                                       const Range& all_tagged,
                                       bool prefer_dense)
{
  // If there are no tagged entities, then don't write anything
  if (ents.range.empty())
    return false;

  // If all of the entities are tagged, then write in dense format
  if (all_tagged.contains(ents.range))
    return true;

  // Unless asked for more lenient choice of dense format, return false
  if (!prefer_dense)
    return false;

  // If we're being lenient about choosing dense format, then
  // return true if at least 2/3 of the entities are tagged.
  Range xsect = intersect(setSet.range, all_tagged);
  if (3*xsect.size() >= 2*setSet.range.size())
    return true;

  return false;
}

ErrorCode WriteHDF5::count_adjacencies(const Range& set, wid_t& result)
{
  ErrorCode rval;
  std::vector<wid_t> adj_list;
  Range::const_iterator iter = set.begin();
  const Range::const_iterator end = set.end();
  result = 0;
  for ( ; iter != end; ++iter) {
    adj_list.clear();
    rval = get_adjacencies(*iter, adj_list);CHK_MB_ERR_0(rval);

    if (adj_list.size() > 0)
      result += 2 + adj_list.size();
  }

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::create_elem_table(const ExportSet& block,
                                       long num_entities,
                                       long& first_id_out)
{
  mhdf_Status status;
  hid_t handle;

  CHECK_OPEN_HANDLES;

  mhdf_addElement(filePtr, block.name(), block.type, &status);CHK_MHDF_ERR_0(status);

  handle = mhdf_createConnectivity(filePtr,
                                   block.name(),
                                   block.num_nodes,
                                   num_entities,
                                   &first_id_out,
                                   &status);CHK_MHDF_ERR_0(status);
  mhdf_closeData(filePtr, handle, &status);CHK_MHDF_ERR_0(status);

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::count_set_size(const Range& sets,
                                    long& contents_length_out,
                                    long& children_length_out,
                                    long& parents_length_out)
{
  ErrorCode rval;
  Range set_contents;
  long contents_length_set, children_length_set, parents_length_set;
  unsigned long flags;
  std::vector<wid_t> set_contents_ids;
  std::vector<SpecialSetData>::const_iterator si = specialSets.begin();

  contents_length_out = 0;
  children_length_out = 0;
  parents_length_out = 0;

  for (Range::const_iterator iter = sets.begin(); iter != sets.end(); ++iter) {
    while (si != specialSets.end() && si->setHandle < *iter)
      ++si;

    if (si != specialSets.end() && si->setHandle == *iter) {
      contents_length_out += si->contentIds.size();
      children_length_out += si->childIds.size();
      parents_length_out += si->parentIds.size();
      ++si;
      continue;
    }

    rval = get_set_info(*iter, contents_length_set, children_length_set,
                        parents_length_set, flags);CHK_MB_ERR_0(rval);

    // Check if can and should compress as ranges
    if (!(flags & MESHSET_ORDERED) && contents_length_set) {
      set_contents.clear();
      rval = iFace->get_entities_by_handle(*iter, set_contents, false);CHK_MB_ERR_0(rval);

      bool blocked_list;
      rval = range_to_blocked_list(set_contents, set_contents_ids, blocked_list);CHK_MB_ERR_0(rval);

      if (blocked_list) {
        assert(set_contents_ids.size() % 2 == 0);
        contents_length_set = set_contents_ids.size();
      }
    }

    contents_length_out += contents_length_set;
    children_length_out += children_length_set;
    parents_length_out += parents_length_set;
  }

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::create_set_meta(long num_sets, long& first_id_out)
{
  hid_t handle;
  mhdf_Status status;

  CHECK_OPEN_HANDLES;

  handle = mhdf_createSetMeta(filePtr, num_sets, &first_id_out, &status);CHK_MHDF_ERR_0(status);
  mhdf_closeData(filePtr, handle, &status);

  return MB_SUCCESS;
}

WriteHDF5::SpecialSetData* WriteHDF5::find_set_data(EntityHandle h)
{
  SpecialSetData tmp;
  tmp.setHandle = h;
  std::vector<SpecialSetData>::iterator i;
  i = std::lower_bound(specialSets.begin(), specialSets.end(), tmp, SpecSetLess());
  return (i == specialSets.end() || i->setHandle != h) ? 0 : &*i;
}

ErrorCode WriteHDF5::create_set_tables(long num_set_contents,
                                       long num_set_children,
                                       long num_set_parents)
{
  hid_t handle;
  mhdf_Status status;

  CHECK_OPEN_HANDLES;

  if (num_set_contents > 0) {
    handle = mhdf_createSetData(filePtr, num_set_contents, &status);CHK_MHDF_ERR_0(status);
    mhdf_closeData(filePtr, handle, &status);
  }

  if (num_set_children > 0) {
    handle = mhdf_createSetChildren(filePtr, num_set_children, &status);CHK_MHDF_ERR_0(status);
    mhdf_closeData(filePtr, handle, &status);
  }

  if (num_set_parents > 0) {
    handle = mhdf_createSetParents(filePtr, num_set_parents, &status);CHK_MHDF_ERR_0(status);
    mhdf_closeData(filePtr, handle, &status);
  }

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::get_tag_size(Tag tag,
                                  DataType& moab_type,
                                  int& num_bytes,
                                  int& type_size,
                                  int& array_length,
                                  mhdf_TagDataType& file_type,
                                  hid_t& hdf_type)
{
  ErrorCode rval;
  Tag type_handle;
  std::string tag_name, tag_type_name;

  CHECK_OPEN_HANDLES;

  // We return NULL for hdf_type if it can be determined from
  // the file_type.  The only case where it is non-zero is
  // if the user specified a specific type via a mesh tag.
  hdf_type = (hid_t)0;
  bool close_hdf_type = false;

  rval = iFace->tag_get_data_type(tag, moab_type);CHK_MB_ERR_0(rval);
  rval = iFace->tag_get_length(tag, array_length);
  if (MB_VARIABLE_DATA_LENGTH == rval) {
    array_length = MB_VARIABLE_LENGTH;
  }
  else if (MB_SUCCESS != rval)
    return error(rval);
  rval = iFace->tag_get_bytes(tag, num_bytes);
  if (MB_VARIABLE_DATA_LENGTH == rval)
    num_bytes = MB_VARIABLE_LENGTH;
  else if (MB_SUCCESS != rval)
    return error(rval);

  switch (moab_type) {
    case MB_TYPE_INTEGER:
      type_size = sizeof(int);
      file_type = mhdf_INTEGER;
      hdf_type = H5T_NATIVE_INT;
      close_hdf_type = false;
      break;
    case MB_TYPE_DOUBLE:
      type_size = sizeof(double);
      file_type = mhdf_FLOAT;
      hdf_type = H5T_NATIVE_DOUBLE;
      close_hdf_type = false;
      break;
    case MB_TYPE_BIT:
      type_size = sizeof(bool);
      file_type = mhdf_BITFIELD;
      assert(array_length <= 8);
      hdf_type = H5Tcopy(H5T_NATIVE_B8);
      H5Tset_precision(hdf_type, array_length);
      close_hdf_type = true;
      break;
    case MB_TYPE_HANDLE:
      type_size = sizeof(EntityHandle);
      file_type = mhdf_ENTITY_ID;
      hdf_type = id_type;
      close_hdf_type = false;
      break;
    case MB_TYPE_OPAQUE:
      file_type = mhdf_OPAQUE;
      rval = iFace->tag_get_name(tag, tag_name);CHK_MB_ERR_0(rval);
      tag_type_name = "__hdf5_tag_type_";
      tag_type_name += tag_name;
      rval = iFace->tag_get_handle(tag_type_name.c_str(), 0, MB_TYPE_OPAQUE, type_handle, MB_TAG_ANY);
      if (MB_TAG_NOT_FOUND == rval) {
        if (num_bytes == MB_VARIABLE_LENGTH)
          type_size = 1;
        else
          type_size = num_bytes;
        hdf_type = H5Tcreate(H5T_OPAQUE, type_size);
        close_hdf_type = true;
      }
      else if (MB_SUCCESS == rval) {
        int hsize;
        rval = iFace->tag_get_bytes(type_handle, hsize);
        if (hsize != sizeof(hid_t))
          return error(MB_FAILURE);

        const EntityHandle root = 0;
        rval = iFace->tag_get_data(type_handle, &root, 1, &hdf_type);
        if (rval != MB_SUCCESS)
          return error(rval);

        type_size = H5Tget_size(hdf_type);
        if (type_size != num_bytes)
          return error(MB_FAILURE);

        close_hdf_type = false;
      }
      else
        return error(rval);
      num_bytes = array_length;
      array_length = (num_bytes == MB_VARIABLE_LENGTH) ? MB_VARIABLE_LENGTH : 1;
      break;
    default:
      break;
  }

  assert(num_bytes == MB_VARIABLE_LENGTH ||
         (moab_type == MB_TYPE_BIT && num_bytes == 1) ||
         array_length * type_size == num_bytes);

  if (num_bytes == MB_VARIABLE_LENGTH) {
    array_length = MB_VARIABLE_LENGTH;
    if (!close_hdf_type) {
      hdf_type = H5Tcopy(hdf_type);
      //close_hdf_type = true;
    }
  }
  else if (array_length > 1 && moab_type != MB_TYPE_BIT) {
    hsize_t len = array_length;
#if defined(H5Tarray_create_vers) && (H5Tarray_create_vers > 1)
    hid_t temp_id = H5Tarray_create2(hdf_type, 1, &len);
#else
    hid_t temp_id = H5Tarray_create(hdf_type, 1, &len, NULL);
#endif
    if (close_hdf_type)
      H5Tclose(hdf_type);
    hdf_type = temp_id;
  }
  else if (!close_hdf_type) {
    hdf_type = H5Tcopy(hdf_type);
    //close_hdf_type = true;
  }

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::get_tag_data_length(const TagDesc& tag_info,
                                         const Range& range,
                                         unsigned long& result)
{
  ErrorCode rval;
  result = 0;

  // Split buffer into two pieces, one for pointers and one for sizes
  size_t step, remaining;
  step = bufferSize / (sizeof(int) + sizeof(void*));
  const void** ptr_buffer = reinterpret_cast<const void**>(dataBuffer);
  int* size_buffer = reinterpret_cast<int*>(ptr_buffer + step); 
  Range subrange;
  Range::const_iterator iter = range.begin();
  for (remaining = range.size(); remaining >= step; remaining -= step) {
    // Get subset of range containing 'count' entities
    Range::const_iterator end = iter; end += step;
    subrange.clear();
    subrange.merge(iter, end);
    iter = end;
    // Get tag sizes for entities
    rval = iFace->tag_get_by_ptr(tag_info.tag_id, subrange, ptr_buffer, size_buffer);
    if (MB_SUCCESS != rval)
      return error(rval);
    // Sum lengths
    for (size_t i = 0; i < step; ++i)
      result += size_buffer[i];
  }
  // Process remaining
  subrange.clear();
  subrange.merge(iter, range.end());
  assert(subrange.size() == remaining);
  rval = iFace->tag_get_by_ptr(tag_info.tag_id, subrange, ptr_buffer, size_buffer);
  if (MB_SUCCESS != rval)
    return error(rval);
  for (size_t i = 0; i < remaining; ++i)
    result += size_buffer[i];

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::create_tag(const TagDesc& tag_data,
                                unsigned long num_sparse_entities,
                                unsigned long data_table_size)
{
  TagType mb_storage;
  DataType mb_type;
  mhdf_TagDataType mhdf_type;
  int tag_bytes, type_size, num_vals, storage;
  hid_t hdf_type = (hid_t)0;
  hid_t handles[3];
  std::string tag_name;
  ErrorCode rval;
  mhdf_Status status;

  CHECK_OPEN_HANDLES;

  // Get tag properties
  rval = iFace->tag_get_type(tag_data.tag_id, mb_storage);CHK_MB_ERR_0(rval);
  switch (mb_storage) {
    case MB_TAG_DENSE:
      storage = mhdf_DENSE_TYPE;
      break;
    case MB_TAG_SPARSE:
      storage = mhdf_SPARSE_TYPE;
      break;
    case MB_TAG_BIT:
      storage = mhdf_BIT_TYPE;
      break;
    case MB_TAG_MESH:
      storage = mhdf_MESH_TYPE;
      break;
    default:
      return error(MB_FAILURE);
  }
  rval = iFace->tag_get_name(tag_data.tag_id, tag_name);CHK_MB_ERR_0(rval);
  rval = get_tag_size(tag_data.tag_id, mb_type, tag_bytes, type_size, num_vals,
                      mhdf_type, hdf_type);CHK_MB_ERR_0(rval);

  // Get default value
  const void *def_value, *mesh_value;
  int def_val_len, mesh_val_len;
  rval = iFace->tag_get_default_value(tag_data.tag_id, def_value, def_val_len);
  if (MB_ENTITY_NOT_FOUND == rval) {
    def_value = 0;
    def_val_len = 0;
  }
  else if (MB_SUCCESS != rval) {
    H5Tclose(hdf_type);
    return error(rval);
  }

  // Get mesh value
  unsigned char byte;
  const EntityHandle root = 0;
  if (mb_storage == MB_TAG_BIT) {
    rval = iFace->tag_get_data(tag_data.tag_id, &root, 1, &byte);
    mesh_value = &byte;
    mesh_val_len = 1;
  }
  else {
    rval = iFace->tag_get_by_ptr(tag_data.tag_id, &root, 1, &mesh_value, &mesh_val_len);
  }
  if (MB_TAG_NOT_FOUND == rval) {
    mesh_value = 0;
    mesh_val_len = 0;
  }
  else if (MB_SUCCESS != rval) {
    H5Tclose(hdf_type);
    return error(rval);
  }

  // For handle-type tags, need to convert from handles to file ids
  if (MB_TYPE_HANDLE == mb_type) {
     // Make sure there's room in the buffer for both
    assert((def_val_len + mesh_val_len) * sizeof(long) < (size_t)bufferSize);

    // Convert default value
    if (def_value) {
      memcpy(dataBuffer, def_value, def_val_len*sizeof(EntityHandle));
      convert_handle_tag(reinterpret_cast<EntityHandle*>(dataBuffer), def_val_len);
      def_value = dataBuffer;
    }

    // Convert mesh value
    if (mesh_value) {
      EntityHandle* ptr = reinterpret_cast<EntityHandle*>(dataBuffer) + def_val_len;
      memcpy(ptr, mesh_value, mesh_val_len*sizeof(EntityHandle));
      if (convert_handle_tag(ptr, mesh_val_len))
        mesh_value = ptr;
      else
        mesh_value = 0;
    }
  }

  if (MB_VARIABLE_LENGTH != tag_bytes) {
    // Write the tag description to the file
    mhdf_createTag(filePtr,
                   tag_name.c_str(),
                   mhdf_type,
                   num_vals,
                   storage,
                   def_value,
                   mesh_value,
                   hdf_type,
                   mb_type == MB_TYPE_HANDLE ? id_type : 0,
                   &status);CHK_MHDF_ERR_0(status);
    H5Tclose(hdf_type);

    // Create empty table for tag data
    if (num_sparse_entities) {
      mhdf_createSparseTagData(filePtr,
                               tag_name.c_str(),
                               num_sparse_entities,
                               handles,
                               &status);CHK_MHDF_ERR_0(status);
      mhdf_closeData(filePtr, handles[0], &status);
      mhdf_closeData(filePtr, handles[1], &status);
    }

    for (size_t i = 0; i < tag_data.dense_list.size(); ++i) {
      const ExportSet* ex = find(tag_data.dense_list[i]);
      assert(0 != ex);
      handles[0] = mhdf_createDenseTagData(filePtr,
                                           tag_name.c_str(),
                                           ex->name(),
                                           ex->total_num_ents,
                                           &status);CHK_MHDF_ERR_0(status);
      mhdf_closeData(filePtr, handles[0], &status);
    }
  }
  else {
    mhdf_createVarLenTag(filePtr,
                         tag_name.c_str(),
                         mhdf_type,
                         storage,
                         def_value, def_val_len,
                         mesh_value, mesh_val_len,
                         hdf_type, mb_type == MB_TYPE_HANDLE ? id_type : 0,
                         &status);CHK_MHDF_ERR_0(status);
    H5Tclose(hdf_type);

    // Create empty table for tag data
    if (num_sparse_entities) {
      mhdf_createVarLenTagData(filePtr,
                               tag_name.c_str(),
                               num_sparse_entities,
                               data_table_size,
                               handles,
                               &status);CHK_MHDF_ERR_0(status);
      mhdf_closeData(filePtr, handles[0], &status);
      mhdf_closeData(filePtr, handles[1], &status);
      mhdf_closeData(filePtr, handles[2], &status);
    }
  }

  return MB_SUCCESS;
}

ErrorCode WriteHDF5::get_num_sparse_tagged_entities(const TagDesc& tag,
                                                    size_t& count)
{
  Range tmp;
  ErrorCode rval = get_sparse_tagged_entities(tag, tmp);
  count = tmp.size();
  return rval;
}

ErrorCode WriteHDF5::get_sparse_tagged_entities(const TagDesc& tag,
                                                Range& results)
{
  results.clear();
  if (!tag.have_dense(setSet))
    results.merge(setSet.range);
  std::list<ExportSet>::reverse_iterator e;
  for (e = exportList.rbegin(); e != exportList.rend(); ++e) {
    if (!tag.have_dense(*e))
      results.merge(e->range);
  }
  if (!tag.have_dense(nodeSet))
    results.merge(nodeSet.range);
  if (results.empty())
    return MB_SUCCESS;

  return iFace->get_entities_by_type_and_tag(0, MBMAXTYPE,
                                             &tag.tag_id, 0, 1,
                                             results, Interface::INTERSECT);
}

void WriteHDF5::get_write_entities(Range& range)
{
  range.clear();
  range.merge(setSet.range);
  std::list<ExportSet>::reverse_iterator e;
  for (e = exportList.rbegin(); e != exportList.rend(); ++e)
    range.merge(e->range);
  range.merge(nodeSet.range);
}

void WriteHDF5::print_id_map() const
{
  print_id_map(std::cout, "");
}

void WriteHDF5::print_id_map(std::ostream& s, const char* pfx) const
{
  RangeMap<EntityHandle, wid_t>::const_iterator i;
  for (i = idMap.begin(); i != idMap.end(); ++i) {
    const char* n1 = CN::EntityTypeName(TYPE_FROM_HANDLE(i->begin));
    EntityID id = ID_FROM_HANDLE(i->begin);
    if (1 == i->count) {
      s << pfx << n1 << " " << id << " -> " << i->value << std::endl;
    }
    else {
      const char* n2 = CN::EntityTypeName(TYPE_FROM_HANDLE(i->begin + i->count - 1));
      if (n1 == n2) {
        s << pfx << n1 << " " << id << "-" << id + i->count - 1
          << " -> " << i->value << "-" << i->value + i->count - 1 << std::endl;
      }
      else {
        s << pfx << n1 << " " << id << "-" 
          << n1 << " " << ID_FROM_HANDLE(i->begin + i->count - 1)
          << " -> " << i->value << "-" << i->value + i->count - 1 << std::endl;
      }
    }
  }
}

void WriteHDF5::print_times(const double* t) const
{
  std::cout << "WriteHDF5:           " << t[TOTAL_TIME] << std::endl
            << "  gather mesh:       " << t[GATHER_TIME] << std::endl
            << "  create file:       " << t[CREATE_TIME] << std::endl
            << "    create nodes:    " << t[CREATE_NODE_TIME] << std::endl
            << "    negotiate types: " << t[NEGOTIATE_TYPES_TIME] << std::endl
            << "    create elem:     " << t[CREATE_ELEM_TIME] << std::endl
            << "    file id exch:    " << t[FILEID_EXCHANGE_TIME] << std::endl
            << "    create adj:      " << t[CREATE_ADJ_TIME] << std::endl
            << "    create set:      " << t[CREATE_SET_TIME] << std::endl
            << "      shared ids:    " << t[SHARED_SET_IDS] << std::endl
            << "      shared data:   " << t[SHARED_SET_CONTENTS] << std::endl
            << "      set offsets:   " << t[SET_OFFSET_TIME] << std::endl
            << "    create tags:     " << t[CREATE_TAG_TIME] << std::endl
            << "  coordinates:       " << t[COORD_TIME] << std::endl
            << "  connectivity:      " << t[CONN_TIME] << std::endl
            << "  sets:              " << t[SET_TIME] << std::endl
            << "    set descrip:     " << t[SET_META] << std::endl
            << "    set content:     " << t[SET_CONTENT] << std::endl
            << "    set parent:      " << t[SET_PARENT] << std::endl
            << "    set child:       " << t[SET_CHILD] << std::endl
            << "  adjacencies:       " << t[ADJ_TIME] << std::endl
            << "  tags:              " << t[TAG_TIME] << std::endl
            << "    dense data:      " << t[DENSE_TAG_TIME] << std::endl
            << "    sparse data:     " << t[SPARSE_TAG_TIME] << std::endl
            << "    var-len data:    " << t[VARLEN_TAG_TIME] << std::endl;
}

} // namespace moab
