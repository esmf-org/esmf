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
// Filename      : ReadHDF5.cpp
//
// Purpose       : HDF5 Writer 
//
// Creator       : Jason Kraftcheck
//
// Creation Date : 04/18/04
//-------------------------------------------------------------------------

#include <assert.h>
#include "moab/MOABConfig.h"
/* Include our MPI header before any HDF5 because otherwise
   it will get included indirectly by HDF5 */
#ifdef MOAB_HAVE_MPI
#  include "moab_mpi.h"
#  include "moab/ParallelComm.hpp"
#endif 
#include <H5Tpublic.h>
#include <H5Ppublic.h>
#include <H5Epublic.h>
#include "moab/Interface.hpp"
#include "Internals.hpp"
#include "MBTagConventions.hpp"
#include "ReadHDF5.hpp"
#include "moab/CN.hpp"
#include "moab/FileOptions.hpp"
#include "moab/CpuTimer.hpp"
#ifdef MOAB_HAVE_HDF5_PARALLEL
#include <H5FDmpi.h>
#include <H5FDmpio.h>
#endif
//#include "WriteHDF5.hpp"

#include <stdlib.h>
#include <string.h>
#include <limits>
#include <functional>
#include <iostream>

#include "IODebugTrack.hpp"
#include "ReadHDF5Dataset.hpp"
#include "ReadHDF5VarLen.hpp"
#include "moab_mpe.h"

namespace moab {

/* If true, coordinates are read in blocked format (all X values before
 * Y values before Z values.) If undefined, then all coordinates for a
 * given vertex are read at the same time.
 */
const bool DEFAULT_BLOCKED_COORDINATE_IO = false;

/* If true, file is opened first by root node only to read summary,
 * file is the closed and the summary is broadcast to all nodes, after
 * which all nodes open file in parallel to read data. If undefined,
 * file is opened once in parallel and all nodes read summary data.
 */
const bool DEFAULT_BCAST_SUMMARY = true;

/* If true and all processors are to read the same block of data,
 * read it on one and broadcast to others rather than using collective
 * io
 */
const bool DEFAULT_BCAST_DUPLICATE_READS = true;

#define READ_HDF5_BUFFER_SIZE (128 * 1024 * 1024)

#define assert_range(PTR, CNT) \
  assert((PTR) >= (void*)dataBuffer); assert(((PTR) + (CNT)) <= (void*)(dataBuffer + bufferSize));

// Call \c error function during HDF5 library errors to make
// it easier to trap such errors in the debugger. This function
// gets registered with the HDF5 library as a callback. It
// works the same as the default (H5Eprint), except that it
// also calls the \c error function as a no-op.
#if defined(H5E_auto_t_vers) && H5E_auto_t_vers > 1
static herr_t handle_hdf5_error(hid_t stack, void* data)
{
  ReadHDF5::HDF5ErrorHandler* h = reinterpret_cast<ReadHDF5::HDF5ErrorHandler*>(data);
  herr_t result = 0;
  if (h->func)
    result = (*h->func)(stack, h->data);
  MB_CHK_ERR_CONT(MB_FAILURE);
  return result;
}
#else
static herr_t handle_hdf5_error(void* data)
{
  ReadHDF5::HDF5ErrorHandler* h = reinterpret_cast<ReadHDF5::HDF5ErrorHandler*>(data);
  herr_t result = 0;
  if (h->func)
    result = (*h->func)(h->data);
  MB_CHK_ERR_CONT(MB_FAILURE);
  return result;
}
#endif

static void copy_sorted_file_ids(const EntityHandle* sorted_ids,
                                 long num_ids,
                                 Range& results)
{
  Range::iterator hint = results.begin();
  long i = 0;
  while (i < num_ids) {
    EntityHandle start = sorted_ids[i];
    for (++i; i < num_ids && sorted_ids[i] == 1 + sorted_ids[i - 1]; ++i);
    hint = results.insert(hint, start, sorted_ids[i - 1]);
  }
}

static void intersect(const mhdf_EntDesc& group, const Range& range, Range& result)
{
  Range::const_iterator s, e;
  s = Range::lower_bound(range.begin(), range.end(), group.start_id);
  e = Range::lower_bound(s, range.end(), group.start_id + group.count);
  result.merge(s, e);
}

#define debug_barrier() debug_barrier_line(__LINE__)
void ReadHDF5::debug_barrier_line(int lineno)
{
#ifdef MOAB_HAVE_MPI
  if (mpiComm) {
    const unsigned threshold = 2;
    static unsigned long count = 0;
    if (dbgOut.get_verbosity() >= threshold) {
      dbgOut.printf(threshold, "*********** Debug Barrier %lu (@%d)***********\n", ++count, lineno);
      MPI_Barrier(*mpiComm);
    }
  }
#else
  if (lineno) {}
#endif
}

class CheckOpenReadHDF5Handles
{
  int fileline;
  mhdf_FileHandle handle;
  int enter_count;
public:
  CheckOpenReadHDF5Handles(mhdf_FileHandle file, int line)
    : fileline(line), handle(file),
      enter_count(mhdf_countOpenHandles(file))
  {}
  ~CheckOpenReadHDF5Handles()
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

#ifdef NDEBUG
#define CHECK_OPEN_HANDLES
#else
#define CHECK_OPEN_HANDLES \
  CheckOpenReadHDF5Handles check_open_handles_(filePtr, __LINE__)
#endif

ReaderIface* ReadHDF5::factory(Interface* iface)
{
  return new ReadHDF5(iface);
}

ReadHDF5::ReadHDF5(Interface* iface)
  : bufferSize(READ_HDF5_BUFFER_SIZE),
    dataBuffer(NULL),
    iFace(iface),
    filePtr(0),
    fileInfo(NULL),
    readUtil(NULL),
    handleType(0),
    indepIO(H5P_DEFAULT),
    collIO(H5P_DEFAULT),
    myPcomm(NULL),
    debugTrack(false),
    dbgOut(stderr),
    nativeParallel(false),
    mpiComm(NULL),    
    blockedCoordinateIO(DEFAULT_BLOCKED_COORDINATE_IO),
    bcastSummary(DEFAULT_BCAST_SUMMARY),
    bcastDuplicateReads(DEFAULT_BCAST_DUPLICATE_READS),
    setMeta(0),
    timer(NULL),
    cputime(false)
{
}

ErrorCode ReadHDF5::init()
{
  ErrorCode rval;

  if (readUtil) 
    return MB_SUCCESS;

  indepIO = collIO = H5P_DEFAULT;
  //WriteHDF5::register_known_tag_types(iFace);

  handleType = H5Tcopy(H5T_NATIVE_ULONG);
  if (handleType < 0)
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  if (H5Tset_size(handleType, sizeof(EntityHandle)) < 0) {
    H5Tclose(handleType);
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  rval = iFace->query_interface(readUtil);
  if (MB_SUCCESS != rval) {
    H5Tclose(handleType);
    MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  idMap.clear();
  fileInfo = 0;
  debugTrack = false;
  myPcomm = 0;

  return MB_SUCCESS;
}

ReadHDF5::~ReadHDF5()
{
  if (!readUtil) // init() failed.
    return;

  delete [] setMeta;
  setMeta = 0;
  iFace->release_interface(readUtil);
  H5Tclose(handleType);
}

ErrorCode ReadHDF5::set_up_read(const char* filename,
                                const FileOptions& opts)
{
  ErrorCode rval;
  mhdf_Status status;
  indepIO = collIO = H5P_DEFAULT;
  mpiComm = 0;

  if (MB_SUCCESS != init())
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

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

  // Set up debug output
  int tmpval;
  if (MB_SUCCESS == opts.get_int_option("DEBUG_IO", 1, tmpval)) {
    dbgOut.set_verbosity(tmpval);
    dbgOut.set_prefix("H5M ");
  }
  dbgOut.limit_output_to_first_N_procs(32);

  // Enable some extra checks for reads. Note: amongst other things this
  // will print errors if the entire file is not read, so if doing a
  // partial read that is not a parallel read, this should be disabled.
  debugTrack = (MB_SUCCESS == opts.get_null_option("DEBUG_BINIO"));

  opts.get_toggle_option("BLOCKED_COORDINATE_IO", DEFAULT_BLOCKED_COORDINATE_IO, blockedCoordinateIO);
  opts.get_toggle_option("BCAST_SUMMARY",         DEFAULT_BCAST_SUMMARY,         bcastSummary);
  opts.get_toggle_option("BCAST_DUPLICATE_READS", DEFAULT_BCAST_DUPLICATE_READS, bcastDuplicateReads);

  // Handle parallel options
  bool use_mpio = (MB_SUCCESS == opts.get_null_option("USE_MPIO"));
  rval = opts.match_option("PARALLEL", "READ_PART");
  bool parallel = (rval != MB_ENTITY_NOT_FOUND);
  nativeParallel = (rval == MB_SUCCESS);
  if (use_mpio && !parallel) {
    MB_SET_ERR(MB_NOT_IMPLEMENTED, "'USE_MPIO' option specified w/out 'PARALLEL' option");
  }

  // This option is intended for testing purposes only, and thus
  // is not documented anywhere.  Decreasing the buffer size can
  // expose bugs that would otherwise only be seen when reading
  // very large files.
  rval = opts.get_int_option("BUFFER_SIZE", bufferSize);
  if (MB_SUCCESS != rval) {
    bufferSize = READ_HDF5_BUFFER_SIZE;
  }
  else if (bufferSize < (int)std::max(sizeof(EntityHandle), sizeof(void*))) {
    MB_CHK_ERR(MB_INVALID_SIZE);
  }

  dataBuffer = (char*)malloc(bufferSize);
  if (!dataBuffer)
    MB_CHK_ERR(MB_MEMORY_ALLOCATION_FAILED);

  if (use_mpio || nativeParallel) {

#ifndef MOAB_HAVE_HDF5_PARALLEL
    free(dataBuffer);
    dataBuffer = NULL;
    MB_SET_ERR(MB_NOT_IMPLEMENTED, "MOAB not configured with parallel HDF5 support");
#else
    MPI_Info info = MPI_INFO_NULL;
    std::string cb_size;
    rval = opts.get_str_option("CB_BUFFER_SIZE", cb_size);
    if (MB_SUCCESS == rval) {
      MPI_Info_create (&info);
      MPI_Info_set (info, const_cast<char*>("cb_buffer_size"), const_cast<char*>(cb_size.c_str()));
    }

    int pcomm_no = 0;
    rval = opts.get_int_option("PARALLEL_COMM", pcomm_no);
    if (rval == MB_TYPE_OUT_OF_RANGE) {
      MB_SET_ERR(rval, "Invalid value for PARALLEL_COMM option");
    }
    myPcomm = ParallelComm::get_pcomm(iFace, pcomm_no);
    if (0 == myPcomm) {
      myPcomm = new ParallelComm(iFace, MPI_COMM_WORLD);
    }
    const int rank = myPcomm->proc_config().proc_rank();
    dbgOut.set_rank(rank);
    dbgOut.limit_output_to_first_N_procs(32);
    mpiComm = new MPI_Comm(myPcomm->proc_config().proc_comm());

#ifndef H5_MPI_COMPLEX_DERIVED_DATATYPE_WORKS 
    dbgOut.print(1, "H5_MPI_COMPLEX_DERIVED_DATATYPE_WORKS is not defined\n");
#endif

    // Open the file in serial on root to read summary
    dbgOut.tprint(1, "Getting file summary\n");
    fileInfo = 0;

    hid_t file_prop;
    if (bcastSummary) {
      unsigned long size = 0;
      if (rank == 0) {
        file_prop = H5Pcreate(H5P_FILE_ACCESS);
        err = H5Pset_fapl_mpio(file_prop, MPI_COMM_SELF, MPI_INFO_NULL);
        assert(file_prop >= 0);
        assert(err >= 0);
        filePtr = mhdf_openFileWithOpt(filename, 0, NULL, handleType, file_prop, &status);
        H5Pclose(file_prop);

        if (filePtr) {
          fileInfo = mhdf_getFileSummary(filePtr, handleType, &status, 0); // no extra set info
          if (!is_error(status)) {
            size = fileInfo->total_size;
            fileInfo->offset = (unsigned char*)fileInfo;
          }
        }
        mhdf_closeFile(filePtr, &status);
        if (fileInfo && mhdf_isError(&status)) {
          free(fileInfo);
          fileInfo = NULL;
        }
      }

      dbgOut.tprint(1, "Communicating file summary\n");
      int mpi_err = MPI_Bcast(&size, 1, MPI_UNSIGNED_LONG, 0, myPcomm->proc_config().proc_comm());
      if (mpi_err || !size)
        return MB_FAILURE;

      if (rank != 0) 
        fileInfo = reinterpret_cast<mhdf_FileDesc*>(malloc(size));

      MPI_Bcast(fileInfo, size, MPI_BYTE, 0, myPcomm->proc_config().proc_comm());

      if (rank != 0)
        mhdf_fixFileDesc(fileInfo, reinterpret_cast<mhdf_FileDesc*>(fileInfo->offset));
    }

    file_prop = H5Pcreate(H5P_FILE_ACCESS);
    err = H5Pset_fapl_mpio(file_prop, myPcomm->proc_config().proc_comm(), info);
    assert(file_prop >= 0);
    assert(err >= 0);

    collIO = H5Pcreate(H5P_DATASET_XFER);
    assert(collIO > 0);
    err = H5Pset_dxpl_mpio(collIO, H5FD_MPIO_COLLECTIVE);
    assert(err >= 0);
    indepIO = nativeParallel ? H5P_DEFAULT : collIO;

    // Re-open file in parallel
    dbgOut.tprintf(1, "Opening \"%s\" for parallel IO\n", filename);
    filePtr = mhdf_openFileWithOpt(filename, 0, NULL, handleType, file_prop, &status);

    H5Pclose(file_prop);
    if (!filePtr) {
      free(dataBuffer);
      dataBuffer = NULL;
      H5Pclose(indepIO);
      if (collIO != indepIO)
        H5Pclose(collIO);
      collIO = indepIO = H5P_DEFAULT;
      MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
    }

    if (!bcastSummary) {
      fileInfo = mhdf_getFileSummary(filePtr, handleType, &status, 0);
      if (is_error(status)) {
        free(dataBuffer);
        dataBuffer = NULL;
        mhdf_closeFile(filePtr, &status);
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
      }
    }
#endif // HDF5_PARALLEL
  }
  else {
    // Open the file
    filePtr = mhdf_openFile(filename, 0, NULL, handleType, &status);
    if (!filePtr) {
      free(dataBuffer);
      dataBuffer = NULL;
      MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
    }

    // Get file info
    fileInfo = mhdf_getFileSummary(filePtr, handleType, &status, 0);
    if (is_error(status)) {
      free(dataBuffer);
      dataBuffer = NULL;
      mhdf_closeFile(filePtr, &status);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
  }

  ReadHDF5Dataset::default_hyperslab_selection_limit();
  int hslimit;
  rval = opts.get_int_option("HYPERSLAB_SELECT_LIMIT", hslimit);
  if (MB_SUCCESS == rval && hslimit > 0)
    ReadHDF5Dataset::set_hyperslab_selection_limit(hslimit);
  else
    ReadHDF5Dataset::default_hyperslab_selection_limit();
  if (MB_SUCCESS != opts.get_null_option("HYPERSLAB_OR") &&
     (MB_SUCCESS == opts.get_null_option("HYPERSLAB_APPEND")
      || HDF5_can_append_hyperslabs())) {
    ReadHDF5Dataset::append_hyperslabs();
    if (MB_SUCCESS != opts.get_int_option("HYPERSLAB_SELECT_LIMIT", hslimit))
      ReadHDF5Dataset::set_hyperslab_selection_limit(std::numeric_limits<int>::max());
    dbgOut.print(1, "Using H5S_APPEND for hyperslab selection\n");
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::clean_up_read(const FileOptions&)
{
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

  free(dataBuffer);
  dataBuffer = NULL;
  free(fileInfo);
  fileInfo = NULL;
  delete mpiComm;
  mpiComm = 0;

  if (indepIO != H5P_DEFAULT)
    H5Pclose(indepIO);
  if (collIO != indepIO)
    H5Pclose(collIO);
  collIO = indepIO = H5P_DEFAULT;

  delete [] setMeta;
  setMeta = 0;

  mhdf_Status status;
  mhdf_closeFile(filePtr, &status);
  filePtr = 0;
  return is_error(status) ? MB_FAILURE : MB_SUCCESS;
}

ErrorCode ReadHDF5::load_file(const char* filename,
                              const EntityHandle* file_set,
                              const FileOptions& opts,
                              const ReaderIface::SubsetList* subset_list,
                              const Tag* file_id_tag)
{
  ErrorCode rval;

  rval = set_up_read(filename, opts);
  if (MB_SUCCESS != rval) {
    clean_up_read(opts);
    return rval;
  }
  // See if we need to report times

  rval = opts.get_null_option("CPUTIME");
  if (MB_SUCCESS == rval)
  {
    cputime = true;
    timer = new CpuTimer;
    for (int i=0; i<NUM_TIMES; i++)
      _times[i]=0;
  }

  // We read the entire set description table regardless of partial
  // or complete reads or serial vs parallel reads
  rval = read_all_set_meta();

  if (cputime)
    _times[SET_META_TIME]=timer->time_elapsed();
  if (subset_list && MB_SUCCESS == rval)
    rval = load_file_partial(subset_list->tag_list,
                             subset_list->tag_list_length,
                             subset_list->num_parts,
                             subset_list->part_number,
                             opts);
  else
    rval = load_file_impl(opts);

  if (MB_SUCCESS == rval && file_id_tag) {
    dbgOut.tprint(1, "Storing file IDs in tag\n");
    rval = store_file_ids(*file_id_tag);
  }
  ErrorCode rval3 = opts.get_null_option("STORE_SETS_FILEIDS");
  if (MB_SUCCESS == rval3)
  {
    rval = store_sets_file_ids();
    if (MB_SUCCESS != rval) return rval;
  }

  if (cputime)
    _times[STORE_FILE_IDS_TIME]=timer->time_elapsed();

  if (MB_SUCCESS == rval && 0 != file_set) {
    dbgOut.tprint(1, "Reading QA records\n");
    rval = read_qa(*file_set);
  }

  if (cputime)
    _times[READ_QA_TIME]=timer->time_elapsed();
  dbgOut.tprint(1, "Cleaning up\n");
  ErrorCode rval2 = clean_up_read(opts);
  if (rval == MB_SUCCESS && rval2 != MB_SUCCESS)
    rval = rval2;

  if (MB_SUCCESS == rval)
    dbgOut.tprint(1, "Read finished.\n");
  else {
    std::string msg;
    iFace->get_last_error(msg);
    dbgOut.tprintf(1, "READ FAILED (ERROR CODE %s): %s\n", ErrorCodeStr[rval], msg.c_str());
  }

  if (cputime) {
    _times[TOTAL_TIME] = timer->time_since_birth();
    print_times();
    delete timer;
  }
  if (H5P_DEFAULT != collIO)
    H5Pclose(collIO);
  if (H5P_DEFAULT != indepIO)
    H5Pclose(indepIO);
  collIO = indepIO = H5P_DEFAULT;

  return rval;
}

ErrorCode ReadHDF5::load_file_impl(const FileOptions&)
{
  ErrorCode rval;
  mhdf_Status status;
  int i;

  CHECK_OPEN_HANDLES;

  dbgOut.tprint(1, "Reading all nodes...\n");
  Range ids;
  if (fileInfo->nodes.count) {
    ids.insert(fileInfo->nodes.start_id,
               fileInfo->nodes.start_id + fileInfo->nodes.count - 1);
    rval = read_nodes(ids);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  dbgOut.tprint(1, "Reading all element connectivity...\n");
  std::vector<int> polyhedra; // Need to do these last so that faces are loaded
  for (i = 0; i < fileInfo->num_elem_desc; ++i) {
    if (CN::EntityTypeFromName(fileInfo->elems[i].type) == MBPOLYHEDRON) {
      polyhedra.push_back(i);
      continue;
    }

    rval = read_elems(i);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }
  for (std::vector<int>::iterator it = polyhedra.begin();
       it != polyhedra.end(); ++it) {
    rval = read_elems(*it);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  dbgOut.tprint(1, "Reading all sets...\n");
  ids.clear();
  if (fileInfo->sets.count) {
    ids.insert(fileInfo->sets.start_id,
               fileInfo->sets.start_id + fileInfo->sets.count - 1);
    rval = read_sets(ids);
    if (rval != MB_SUCCESS) {
      MB_SET_ERR(rval, "ReadHDF5 Failure");
    }
  }

  dbgOut.tprint(1, "Reading all adjacencies...\n");
  for (i = 0; i < fileInfo->num_elem_desc; ++i) {
    if (!fileInfo->elems[i].have_adj)
      continue;

    long table_len;
    hid_t table = mhdf_openAdjacency(filePtr,
                                     fileInfo->elems[i].handle,
                                     &table_len,
                                     &status);
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

    rval = read_adjacencies(table, table_len);
    mhdf_closeData(filePtr, table, &status);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  dbgOut.tprint(1, "Reading all tags...\n");
  for (i = 0; i < fileInfo->num_tag_desc; ++i) {
    rval = read_tag(i);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  dbgOut.tprint(1, "Core read finished.  Cleaning up...\n");
  return MB_SUCCESS;
}

ErrorCode ReadHDF5::find_int_tag(const char* name, int& index)
{
  for (index = 0; index < fileInfo->num_tag_desc; ++index)
    if (!strcmp(name, fileInfo->tags[index].name))
      break;

  if (index == fileInfo->num_tag_desc) {
    MB_SET_ERR(MB_TAG_NOT_FOUND, "File does not contain subset tag '" << name << "'");
  }

  if (fileInfo->tags[index].type != mhdf_INTEGER ||
      fileInfo->tags[index].size != 1) {
    MB_SET_ERR(MB_TAG_NOT_FOUND, "Tag ' " << name << "' does not contain single integer value");
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::get_subset_ids(const ReaderIface::IDTag* subset_list,
                                   int subset_list_length,
                                   Range& file_ids)
{
  ErrorCode rval;

  for (int i = 0; i < subset_list_length; ++i) {
    int tag_index;
    rval = find_int_tag(subset_list[i].tag_name, tag_index);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");

    Range tmp_file_ids;
    if (!subset_list[i].num_tag_values) {
      rval = get_tagged_entities(tag_index, tmp_file_ids);
    }
    else {
      std::vector<int> ids(subset_list[i].tag_values,
                           subset_list[i].tag_values + subset_list[i].num_tag_values);
      std::sort(ids.begin(), ids.end());
      rval = search_tag_values(tag_index, ids, tmp_file_ids);
      if (MB_SUCCESS != rval)
        MB_SET_ERR(rval, "ReadHDF5 Failure");
    }

    if (tmp_file_ids.empty())
      MB_CHK_ERR(MB_ENTITY_NOT_FOUND);

    if (i == 0) 
      file_ids.swap(tmp_file_ids);
    else 
      file_ids = intersect(tmp_file_ids, file_ids);
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::get_partition(Range& tmp_file_ids, int num_parts, int part_number)
{
  CHECK_OPEN_HANDLES;

   // Check that the tag only identified sets
   if ((unsigned long)fileInfo->sets.start_id > tmp_file_ids.front()) {
     dbgOut.print(2, "Ignoring non-set entities with partition set tag\n");
     tmp_file_ids.erase(tmp_file_ids.begin(),
                        tmp_file_ids.lower_bound(
                        (EntityHandle)fileInfo->sets.start_id));
   }
   unsigned long set_end = (unsigned long)fileInfo->sets.start_id + fileInfo->sets.count;
   if (tmp_file_ids.back() >= set_end) {
     dbgOut.print(2, "Ignoring non-set entities with partition set tag\n");
     tmp_file_ids.erase(tmp_file_ids.upper_bound((EntityHandle)set_end),
                        tmp_file_ids.end());
   }

  Range::iterator s = tmp_file_ids.begin();
  size_t num_per_proc = tmp_file_ids.size() / num_parts;
  size_t num_extra = tmp_file_ids.size() % num_parts;
  Range::iterator e;
  if (part_number < (long)num_extra) {
    s += (num_per_proc + 1) * part_number;
    e = s;
    e += (num_per_proc + 1);
  }
  else {
    s += num_per_proc * part_number + num_extra;
    e = s;
    e += num_per_proc;
  }
  tmp_file_ids.erase(e, tmp_file_ids.end());
  tmp_file_ids.erase(tmp_file_ids.begin(), s);

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::load_file_partial(const ReaderIface::IDTag* subset_list,
                                      int subset_list_length,
                                      int num_parts,
                                      int part_number,
                                      const FileOptions& opts)
{
  mhdf_Status status;

  static MPEState mpe_event("ReadHDF5", "yellow");

  mpe_event.start("gather parts");

  CHECK_OPEN_HANDLES;

  for (int i = 0; i < subset_list_length; ++i) {
    dbgOut.printf(2, "Select by \"%s\" with num_tag_values = %d\n",
                  subset_list[i].tag_name, subset_list[i].num_tag_values);
    if (subset_list[i].num_tag_values) {
      assert(0 != subset_list[i].tag_values);
      dbgOut.printf(2, "  \"%s\" values = { %d",
        subset_list[i].tag_name, subset_list[i].tag_values[0]);
      for (int j = 1; j < subset_list[i].num_tag_values; ++j)
        dbgOut.printf(2, ", %d", subset_list[i].tag_values[j]);
      dbgOut.printf(2, " }\n");
    }
  }
  if (num_parts) 
    dbgOut.printf(2, "Partition with num_parts = %d and part_number = %d\n",
                  num_parts, part_number);

  dbgOut.tprint(1, "RETRIEVING TAGGED ENTITIES\n");

  Range file_ids;
  ErrorCode rval = get_subset_ids(subset_list, subset_list_length, file_ids);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  if (cputime)
    _times[SUBSET_IDS_TIME] = timer->time_elapsed();

  if (num_parts) {
    if (num_parts>(int)file_ids.size())
    {
      MB_SET_ERR(MB_FAILURE, "Only " << file_ids.size() << " parts to distribute to " << num_parts << " processes.");
    }
    rval = get_partition(file_ids, num_parts, part_number);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  if (cputime)
    _times[GET_PARTITION_TIME] = timer->time_elapsed();

  dbgOut.print_ints(4, "Set file IDs for partial read: ", file_ids);
  mpe_event.end();
  mpe_event.start("gather related sets");
  dbgOut.tprint(1, "GATHERING ADDITIONAL ENTITIES\n");

  enum RecusiveSetMode {RSM_NONE, RSM_SETS, RSM_CONTENTS};
  const char* const set_opts[] = {"NONE", "SETS", "CONTENTS", NULL};
  int child_mode;
  rval = opts.match_option("CHILDREN", set_opts, child_mode);
  if (MB_ENTITY_NOT_FOUND == rval)
    child_mode = RSM_CONTENTS;
  else if (MB_SUCCESS != rval) {
    MB_SET_ERR(rval, "Invalid value for 'CHILDREN' option");
  }
  int content_mode;
  rval = opts.match_option("SETS", set_opts, content_mode);
  if (MB_ENTITY_NOT_FOUND == rval)
    content_mode = RSM_CONTENTS;
  else if (MB_SUCCESS != rval) {
    MB_SET_ERR(rval, "Invalid value for 'SETS' option");
  }

  // If we want the contents of contained/child sets,
  // search for them now (before gathering the non-set contents
  // of the sets.)
  Range sets;
  intersect(fileInfo->sets, file_ids, sets);
  if (content_mode == RSM_CONTENTS || child_mode == RSM_CONTENTS) {
    dbgOut.tprint(1, "  doing read_set_ids_recursive\n");
    rval = read_set_ids_recursive(sets, content_mode == RSM_CONTENTS, child_mode == RSM_CONTENTS);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  if (cputime)
    _times[GET_SET_IDS_TIME] = timer->time_elapsed();
  debug_barrier();

  // Get elements and vertices contained in sets
  dbgOut.tprint(1, "  doing get_set_contents\n");
  rval = get_set_contents(sets, file_ids);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  if (cputime)
    _times[GET_SET_CONTENTS_TIME] = timer->time_elapsed();

  dbgOut.print_ints(5, "File IDs for partial read: ", file_ids);
  debug_barrier();
  mpe_event.end();
  dbgOut.tprint(1, "GATHERING NODE IDS\n");

  // Figure out the maximum dimension of entity to be read
  int max_dim = 0;
  for (int i = 0; i < fileInfo->num_elem_desc; ++i) {
    EntityType type = CN::EntityTypeFromName(fileInfo->elems[i].type);
    if (type <= MBVERTEX || type >= MBENTITYSET) {
      assert(false); // For debug code die for unknown element types
      continue; // For release code, skip unknown element types
    }
    int dim = CN::Dimension(type);
    if (dim > max_dim) {
      Range subset;
      intersect(fileInfo->elems[i].desc, file_ids, subset);
      if (!subset.empty())
        max_dim = dim;
    }
  }
#ifdef MOAB_HAVE_MPI
  if (nativeParallel) {
    int send = max_dim;
    MPI_Allreduce(&send, &max_dim, 1, MPI_INT, MPI_MAX, *mpiComm);
  }
#endif

  // If input contained any polyhedra, then need to get faces
  // of the polyhedra before the next loop because we need to
  // read said faces in that loop.
  for (int i = 0; i < fileInfo->num_elem_desc; ++i) {
    EntityType type = CN::EntityTypeFromName(fileInfo->elems[i].type);
    if (type != MBPOLYHEDRON)
      continue;

    debug_barrier();
    dbgOut.print(2, "    Getting polyhedra faces\n");
    mpe_event.start("reading connectivity for ", fileInfo->elems[i].handle);

    Range polyhedra;
    intersect(fileInfo->elems[i].desc, file_ids, polyhedra);
    rval = read_elems(i, polyhedra, &file_ids);
    mpe_event.end(rval);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  if (cputime)
    _times[GET_POLYHEDRA_TIME] = timer->time_elapsed();
  // Get node file ids for all elements
  Range nodes;
  intersect(fileInfo->nodes, file_ids, nodes);
  for (int i = 0; i < fileInfo->num_elem_desc; ++i) {
    EntityType type = CN::EntityTypeFromName(fileInfo->elems[i].type);
    if (type <= MBVERTEX || type >= MBENTITYSET) {
      assert(false); // For debug code die for unknown element types
      continue; // For release code, skip unknown element types
    }
    if (MBPOLYHEDRON == type)
      continue;

    debug_barrier();
    dbgOut.printf(2, "    Getting element node IDs for: %s\n", fileInfo->elems[i].handle);

    Range subset;
    intersect(fileInfo->elems[i].desc, file_ids, subset);
    mpe_event.start("reading connectivity for ", fileInfo->elems[i].handle);

    // If dimension is max_dim, then we can create the elements now
    // so we don't have to read the table again later (connectivity
    // will be fixed up after nodes are created when update_connectivity())
    // is called.  For elements of a smaller dimension, we just build
    // the node ID range now because a) we'll have to read the whole
    // connectivity table again later, and b) we don't want to worry
    // about accidentally creating multiple copies of the same element.
    if (CN::Dimension(type) == max_dim)
      rval = read_elems(i, subset, &nodes);
    else
      rval = read_elems(i, subset, nodes);
    mpe_event.end(rval);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }
  if (cputime)
    _times[GET_ELEMENTS_TIME] = timer->time_elapsed();
  debug_barrier();
  mpe_event.start("read coords");
  dbgOut.tprintf(1, "READING NODE COORDINATES (%lu nodes in %lu selects)\n",
                     (unsigned long)nodes.size(), (unsigned long)nodes.psize());

  // Read node coordinates and create vertices in MOAB
  // NOTE: This populates the RangeMap with node file ids,
  //       which is expected by read_node_adj_elems.
  rval = read_nodes(nodes);
  mpe_event.end(rval);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  if (cputime)
    _times[GET_NODES_TIME] = timer->time_elapsed();

  debug_barrier();
  dbgOut.tprint(1, "READING ELEMENTS\n");

  // Decide if we need to read additional elements
  enum SideMode {SM_EXPLICIT, SM_NODES, SM_SIDES};
  int side_mode;
  const char* const options[] = {"EXPLICIT", "NODES", "SIDES", 0};
  rval = opts.match_option("ELEMENTS", options, side_mode);
  if (MB_ENTITY_NOT_FOUND == rval) {
    // If only nodes were specified, then default to "NODES", otherwise
    // default to "SIDES".
    if (0 == max_dim)
      side_mode = SM_NODES;
    else
      side_mode = SM_SIDES;
  }
  else if (MB_SUCCESS != rval) {
    MB_SET_ERR(rval, "Invalid value for 'ELEMENTS' option");
  }

  if (side_mode == SM_SIDES /*ELEMENTS=SIDES*/ && max_dim == 0 /*node-based*/) {
    // Read elements until we find something. Once we find something,
    // read only elements of the same dimension. NOTE: loop termination
    // criterion changes on both sides (max_dim can be changed in loop
    // body).
    for (int dim = 3; dim >= max_dim; --dim) {
      for (int i = 0; i < fileInfo->num_elem_desc; ++i) {
        EntityType type = CN::EntityTypeFromName(fileInfo->elems[i].type);
        if (CN::Dimension(type) == dim) {
          debug_barrier();
          dbgOut.tprintf(2, "    Reading node-adjacent elements for: %s\n", fileInfo->elems[i].handle);
          mpe_event.start("reading connectivity for ", fileInfo->elems[i].handle);
          Range ents;
          rval = read_node_adj_elems(fileInfo->elems[i]);
          mpe_event.end(rval);
          if (MB_SUCCESS != rval)
            MB_SET_ERR(rval, "ReadHDF5 Failure");
          if (!ents.empty())
            max_dim = 3;
        }
      }
    }
  }

  if (cputime)
    _times[GET_NODEADJ_TIME] = timer->time_elapsed();
  Range side_entities;
  if (side_mode != SM_EXPLICIT /*ELEMENTS=NODES || ELEMENTS=SIDES*/) {
    if (0 == max_dim)
      max_dim = 4;
    // Now read any additional elements for which we've already read all
    // of the nodes.
    for (int dim = max_dim - 1; dim > 0; --dim) {
      for (int i = 0; i < fileInfo->num_elem_desc; ++i) {
        EntityType type = CN::EntityTypeFromName(fileInfo->elems[i].type);
        if (CN::Dimension(type) == dim) {
          debug_barrier();
          dbgOut.tprintf(2, "    Reading node-adjacent elements for: %s\n", fileInfo->elems[i].handle);
          mpe_event.start("reading connectivity for ", fileInfo->elems[i].handle);
          rval = read_node_adj_elems(fileInfo->elems[i], &side_entities);
          mpe_event.end(rval);
          if (MB_SUCCESS != rval)
            MB_SET_ERR(rval, "ReadHDF5 Failure");
        }
      }
    }
  }

  // We need to do this here for polyhedra to be handled correctly.
  // We have to wait until the faces are read in the above code block,
  // but need to create the connectivity before doing update_connectivity,
  // which might otherwise delete polyhedra faces.
  if (cputime)
   _times[GET_SIDEELEM_TIME] = timer->time_elapsed();

  debug_barrier();
  dbgOut.tprint(1, "UPDATING CONNECTIVITY ARRAYS FOR READ ELEMENTS\n");
  mpe_event.start("updating connectivity for elements read before vertices");
  rval = update_connectivity();
  mpe_event.end();
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  if (cputime)
   _times[UPDATECONN_TIME] = timer->time_elapsed();

  dbgOut.tprint(1, "READING ADJACENCIES\n");
  for (int i = 0; i < fileInfo->num_elem_desc; ++i) {
    if (fileInfo->elems[i].have_adj  /*&&
        idMap.intersects(fileInfo->elems[i].desc.start_id, fileInfo->elems[i].desc.count) */) {
      mpe_event.start("reading adjacencies for ", fileInfo->elems[i].handle);
      long len;
      hid_t th = mhdf_openAdjacency(filePtr, fileInfo->elems[i].handle, &len, &status);
      if (is_error(status))
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

      rval = read_adjacencies(th, len);
      mhdf_closeData(filePtr, th, &status);
      mpe_event.end(rval);
      if (MB_SUCCESS != rval)
        MB_SET_ERR(rval, "ReadHDF5 Failure");
    }
  }

  if (cputime)
   _times[ADJACENCY_TIME] = timer->time_elapsed();

  // If doing ELEMENTS=SIDES then we need to delete any entities
  // that we read that aren't actually sides (e.g. an interior face
  // that connects two disjoint portions of the part). Both
  // update_connectivity and reading of any explicit adjacencies must
  // happen before this.
  if (side_mode == SM_SIDES) {
    debug_barrier();
    mpe_event.start("cleaning up non-side lower-dim elements");
    dbgOut.tprint(1, "CHECKING FOR AND DELETING NON-SIDE ELEMENTS\n");
    rval = delete_non_side_elements(side_entities);
    mpe_event.end(rval);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  if (cputime)
   _times[DELETE_NON_SIDEELEM_TIME] = timer->time_elapsed();

  debug_barrier();
  dbgOut.tprint(1, "READING SETS\n");

  // If reading contained/child sets but not their contents then find
  // them now. If we were also reading their contents we would
  // have found them already.
  if (content_mode == RSM_SETS || child_mode == RSM_SETS) {
    dbgOut.tprint(1, "  doing read_set_ids_recursive\n");
    mpe_event.start("finding recursively contained sets");
    rval = read_set_ids_recursive(sets, content_mode == RSM_SETS, child_mode == RSM_SETS);
    mpe_event.end(rval);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  if (cputime)
   _times[READ_SET_IDS_RECURS_TIME] = timer->time_elapsed();

  dbgOut.tprint(1, "  doing find_sets_containing\n");
  mpe_event.start("finding sets containing any read entities");

  // Decide whether to read set-containing parents
  bool read_set_containing_parents = true;
  std::string tmp_opt;
  rval = opts.get_option("NO_SET_CONTAINING_PARENTS", tmp_opt);
  if (MB_SUCCESS == rval)
    read_set_containing_parents = false;

  // Append file IDs of sets containing any of the nodes or elements
  // we've read up to this point.
  rval = find_sets_containing(sets, read_set_containing_parents);
  mpe_event.end(rval);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  if (cputime)
     _times[FIND_SETS_CONTAINING_TIME] = timer->time_elapsed();

  // Now actually read all set data and instantiate sets in MOAB.
  // Get any contained sets out of file_ids.
  mpe_event.start("reading set contents/parents/children");
  EntityHandle first_set = fileInfo->sets.start_id;
  sets.merge(file_ids.lower_bound(first_set),
             file_ids.lower_bound(first_set + fileInfo->sets.count));
  dbgOut.tprint(1, "  doing read_sets\n");
  rval = read_sets(sets);
  mpe_event.end(rval);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  if (cputime)
   _times[READ_SETS_TIME] = timer->time_elapsed();

  dbgOut.tprint(1, "READING TAGS\n");

  for (int i = 0; i < fileInfo->num_tag_desc; ++i) {
    mpe_event.start("reading tag: ", fileInfo->tags[i].name);
    rval = read_tag(i);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  if (cputime)
   _times[READ_TAGS_TIME] = timer->time_elapsed();

  dbgOut.tprint(1, "PARTIAL READ COMPLETE.\n");

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::search_tag_values(int tag_index,
                                      const std::vector<int>& sorted_values,
                                      Range& file_ids,
                                      bool sets_only)
{
  ErrorCode rval;
  mhdf_Status status;
  std::vector<EntityHandle>::iterator iter;
  const mhdf_TagDesc& tag = fileInfo->tags[tag_index];
  long size;
  long start_id;

  CHECK_OPEN_HANDLES;

  debug_barrier();

  // Do dense data

  hid_t table;
  const char* name;
  std::vector<EntityHandle> indices;
  // These are probably in order of dimension, so iterate
  // in reverse order to make Range insertions more efficient.
  std::vector<int> grp_indices(tag.dense_elem_indices, tag.dense_elem_indices + tag.num_dense_indices);
  for (std::vector<int>::reverse_iterator i = grp_indices.rbegin(); i != grp_indices.rend(); ++i) {
    int idx = *i;
    if (idx == -2) {
      name = mhdf_set_type_handle();
      start_id = fileInfo->sets.start_id;
    }
    else if (sets_only) {
      continue;
    }
    else if (idx == -1) {
      name = mhdf_node_type_handle();
     start_id = fileInfo->nodes.start_id;
    }
    else {
      if (idx < 0 || idx >= fileInfo->num_elem_desc) 
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
      name = fileInfo->elems[idx].handle;
      start_id = fileInfo->elems[idx].desc.start_id;
    }
    table = mhdf_openDenseTagData(filePtr, tag.name, name, &size, &status);
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    rval = search_tag_values(table, size, sorted_values, indices);
    mhdf_closeData(filePtr, table, &status);
    if (MB_SUCCESS != rval || is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    // Convert from table indices to file IDs and add to result list
    std::sort(indices.begin(), indices.end(), std::greater<EntityHandle>());
    std::transform(indices.begin(), indices.end(), range_inserter(file_ids),
                   std::bind1st(std::plus<long>(), start_id));
    indices.clear();
  }

  if (!tag.have_sparse)
    return MB_SUCCESS;

  // Do sparse data

  hid_t tables[2];
  long junk; // Redundant value for non-variable-length tags
  mhdf_openSparseTagData(filePtr, tag.name, &size, &junk, tables, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  rval = search_tag_values(tables[1], size, sorted_values, indices);
  mhdf_closeData(filePtr, tables[1], &status);
  if (MB_SUCCESS != rval || is_error(status)) {
    mhdf_closeData(filePtr, tables[0], &status);
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }
  // Convert to ranges
  std::sort(indices.begin(), indices.end());
  std::vector<EntityHandle> ranges;
  iter = indices.begin();
  while (iter != indices.end()) {
    ranges.push_back(*iter);
    EntityHandle last = *iter;
    for (++iter; iter != indices.end() && (last + 1) == *iter; ++iter, ++last);
    ranges.push_back(last);
  }
  // Read file ids
  iter = ranges.begin();
  unsigned long offset = 0;
  while (iter != ranges.end()) {
    long begin = *iter; ++iter;
    long end   = *iter; ++iter;
    mhdf_readSparseTagEntitiesWithOpt(tables[0], begin, end - begin + 1,
                                      handleType, &indices[offset], indepIO, &status);
    if (is_error(status)) {
      mhdf_closeData(filePtr, tables[0], &status);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
    offset += end - begin + 1;
  }
  mhdf_closeData(filePtr, tables[0], &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  assert(offset == indices.size());
  std::sort(indices.begin(), indices.end());

  if (sets_only) {
    iter = std::lower_bound(indices.begin(), indices.end(),
              (EntityHandle)(fileInfo->sets.start_id + fileInfo->sets.count));
    indices.erase(iter, indices.end());
    iter = std::lower_bound(indices.begin(), indices.end(),
                            fileInfo->sets.start_id);
    indices.erase(indices.begin(), iter);
  }
  copy_sorted_file_ids(&indices[0], indices.size(), file_ids);

  return MB_SUCCESS;  
}

ErrorCode ReadHDF5::get_tagged_entities(int tag_index, Range& file_ids)
{
  const mhdf_TagDesc& tag = fileInfo->tags[tag_index];

  CHECK_OPEN_HANDLES;

  // Do dense data
  Range::iterator hint = file_ids.begin();
  for (int i = 0; i < tag.num_dense_indices; ++i) {
    int idx = tag.dense_elem_indices[i];
    mhdf_EntDesc* ents;
    if (idx == -2)
      ents = &fileInfo->sets;
    else if (idx == -1) 
      ents = &fileInfo->nodes;
    else {
      if (idx < 0 || idx >= fileInfo->num_elem_desc) 
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
      ents = &(fileInfo->elems[idx].desc);
    }

    EntityHandle h = (EntityHandle)ents->start_id;
    hint = file_ids.insert(hint, h, h + ents->count - 1);
  }

  if (!tag.have_sparse)
    return MB_SUCCESS;

  // Do sparse data

  mhdf_Status status;
  hid_t tables[2]; 
  long size, junk; 
  mhdf_openSparseTagData(filePtr, tag.name, &size, &junk, tables, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  mhdf_closeData(filePtr, tables[1], &status);
  if (is_error(status)) {
    mhdf_closeData(filePtr, tables[0], &status);
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  hid_t file_type = H5Dget_type(tables[0]);
  if (file_type < 0) 
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  hint = file_ids.begin();
  EntityHandle* buffer = reinterpret_cast<EntityHandle*>(dataBuffer);
  const long buffer_size = bufferSize / std::max(sizeof(EntityHandle), H5Tget_size(file_type));
  long remaining = size, offset = 0;
  while (remaining) {
    long count = std::min(buffer_size, remaining);
    assert_range(buffer, count);
    mhdf_readSparseTagEntitiesWithOpt(*tables, offset, count,
                                      file_type, buffer, collIO, &status);
    if (is_error(status)) {
      H5Tclose(file_type);
      mhdf_closeData(filePtr, *tables, &status);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
    H5Tconvert(file_type, handleType, count, buffer, NULL, H5P_DEFAULT);

    std::sort(buffer, buffer + count);
    for (long i = 0; i < count; ++i)
      hint = file_ids.insert(hint, buffer[i], buffer[i]);

    remaining -= count;
    offset += count;
  }

  H5Tclose(file_type);
  mhdf_closeData(filePtr, *tables, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  return MB_SUCCESS;  
}

ErrorCode ReadHDF5::search_tag_values(hid_t tag_table,
                                      unsigned long table_size,
                                      const std::vector<int>& sorted_values,
                                      std::vector<EntityHandle>& value_indices)
{
  debug_barrier();

  CHECK_OPEN_HANDLES;

  mhdf_Status status;
  size_t chunk_size = bufferSize / sizeof(int);
  int * buffer = reinterpret_cast<int*>(dataBuffer);
  size_t remaining = table_size, offset = 0;
  while (remaining) {
    // Get a block of tag values
    size_t count = std::min(chunk_size, remaining);
    assert_range(buffer, count);
    mhdf_readTagValuesWithOpt(tag_table, offset, count, H5T_NATIVE_INT, buffer, collIO, &status);
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

    // Search tag values
    for (size_t i = 0; i < count; ++i)
      if (std::binary_search(sorted_values.begin(), sorted_values.end(), (int)buffer[i]))
        value_indices.push_back(i + offset);

    offset += count;
    remaining -= count;
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_nodes(const Range& node_file_ids)
{
  ErrorCode rval;
  mhdf_Status status;
  const int dim = fileInfo->nodes.vals_per_ent;
  Range range;

  CHECK_OPEN_HANDLES;

  if (node_file_ids.empty())
    return MB_SUCCESS;

  int cdim;
  rval = iFace->get_dimension(cdim);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  if (cdim < dim) {
    rval = iFace->set_dimension(dim);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  hid_t data_id = mhdf_openNodeCoordsSimple(filePtr, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  EntityHandle handle;
  std::vector<double*> arrays(dim);
  const size_t num_nodes = node_file_ids.size();
  rval = readUtil->get_node_coords(dim, (int)num_nodes, 0, handle, arrays);
  if (MB_SUCCESS != rval) {
    mhdf_closeData(filePtr, data_id, &status);
    MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  if (blockedCoordinateIO) {
    try {
      for (int d = 0; d < dim; ++d) {
        ReadHDF5Dataset reader("blocked coords", data_id, nativeParallel, mpiComm, false);
        reader.set_column(d);
        reader.set_file_ids(node_file_ids, fileInfo->nodes.start_id, num_nodes, H5T_NATIVE_DOUBLE);
        dbgOut.printf(3, "Reading %lu chunks for coordinate dimension %d\n", reader.get_read_count(), d);
        // Should normally only have one read call, unless sparse nature
        // of file_ids caused reader to do something strange
        size_t count, offset = 0;
        int nn = 0;
        while (!reader.done()) {
          dbgOut.printf(3, "Reading chunk %d for dimension %d\n", ++nn, d);
          reader.read(arrays[d] + offset, count);
          offset += count;
        }
        if (offset != num_nodes) {
          mhdf_closeData(filePtr, data_id, &status);
          assert(false);
          return MB_FAILURE;
        }
      }
    }
    catch (ReadHDF5Dataset::Exception) {
      mhdf_closeData(filePtr, data_id, &status);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
  }
  else { // !blockedCoordinateIO
    double* buffer = (double*)dataBuffer;
    long chunk_size = bufferSize / (3 * sizeof(double));
    long coffset = 0;
    int nn = 0;
    try {
      ReadHDF5Dataset reader("interleaved coords", data_id, nativeParallel, mpiComm, false);
      reader.set_file_ids(node_file_ids, fileInfo->nodes.start_id, chunk_size, H5T_NATIVE_DOUBLE);
      dbgOut.printf(3, "Reading %lu chunks for coordinate coordinates\n", reader.get_read_count());
      while (!reader.done()) {
        dbgOut.tprintf(3, "Reading chunk %d of node coords\n", ++nn);

        size_t count;
        reader.read(buffer, count);

        for (size_t i = 0; i < count; ++i)
          for (int d = 0; d < dim; ++d)
            arrays[d][coffset + i] = buffer[dim*i + d];
        coffset += count;
      }
    }
    catch (ReadHDF5Dataset::Exception) {
      mhdf_closeData(filePtr, data_id, &status);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
  }

  dbgOut.print(3, "Closing node coordinate table\n");
  mhdf_closeData(filePtr, data_id, &status);
  for (int d = dim; d < cdim; ++d)
    memset(arrays[d], 0, num_nodes * sizeof(double));

  dbgOut.printf(3, "Updating ID to handle map for %lu nodes\n", (unsigned long)node_file_ids.size());
  return insert_in_id_map(node_file_ids, handle);
}

ErrorCode ReadHDF5::read_elems(int i)
{
  Range ids;
  ids.insert(fileInfo->elems[i].desc.start_id,
             fileInfo->elems[i].desc.start_id + fileInfo->elems[i].desc.count - 1);
  return read_elems(i, ids);
}

ErrorCode ReadHDF5::read_elems(int i, const Range& file_ids, Range* node_ids)
{
  if (fileInfo->elems[i].desc.vals_per_ent < 0) {
    if (node_ids != 0) // Not implemented for version 3 format of poly data
      MB_CHK_ERR(MB_TYPE_OUT_OF_RANGE);
    return read_poly(fileInfo->elems[i], file_ids);
  }
  else
    return read_elems(fileInfo->elems[i], file_ids, node_ids);
}

ErrorCode ReadHDF5::read_elems(const mhdf_ElemDesc& elems, const Range& file_ids, Range* node_ids)
{
  CHECK_OPEN_HANDLES;

  debug_barrier();
  dbgOut.tprintf(1, "READING %s CONNECTIVITY (%lu elems in %lu selects)\n",
                 elems.handle, (unsigned long)file_ids.size(), (unsigned long)file_ids.psize());

  ErrorCode rval = MB_SUCCESS;
  mhdf_Status status;

  EntityType type = CN::EntityTypeFromName(elems.type);
  if (type == MBMAXTYPE) {
    MB_SET_ERR(MB_FAILURE, "Unknown element type: \"" << elems.type << "\"");
  }

  const int nodes_per_elem = elems.desc.vals_per_ent;
  const size_t count = file_ids.size();
  hid_t data_id = mhdf_openConnectivitySimple(filePtr, elems.handle, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  EntityHandle handle;
  EntityHandle* array = 0;
  if (count > 0)
    rval = readUtil->get_element_connect(count, nodes_per_elem, type,
                                         0, handle, array);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  try {
    EntityHandle* buffer = reinterpret_cast<EntityHandle*>(dataBuffer);
    const size_t buffer_size = bufferSize / (sizeof(EntityHandle) * nodes_per_elem);
    ReadHDF5Dataset reader(elems.handle, data_id, nativeParallel, mpiComm);
    reader.set_file_ids(file_ids, elems.desc.start_id, buffer_size, handleType);
    dbgOut.printf(3, "Reading connectivity in %lu chunks for element group \"%s\"\n",
                  reader.get_read_count(), elems.handle);
    EntityHandle* iter = array;
    int nn = 0;
    while (!reader.done()) {
      dbgOut.printf(3, "Reading chunk %d for \"%s\"\n", ++nn, elems.handle);

      size_t num_read;
      reader.read(buffer, num_read);
      iter = std::copy(buffer, buffer + num_read*nodes_per_elem, iter);

      if (node_ids) {
        std::sort(buffer, buffer + num_read*nodes_per_elem);
        num_read = std::unique(buffer, buffer + num_read*nodes_per_elem) - buffer;
        copy_sorted_file_ids(buffer, num_read, *node_ids);
      }
    }
    assert(iter - array == (ptrdiff_t)count * nodes_per_elem);
  }
  catch (ReadHDF5Dataset::Exception) {
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  if (!node_ids) {
    rval = convert_id_to_handle(array, count * nodes_per_elem);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");

    rval = readUtil->update_adjacencies(handle, count, nodes_per_elem, array);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }
  else {
    IDConnectivity t;
    t.handle = handle;
    t.count = count;
    t.nodes_per_elem = nodes_per_elem;
    t.array = array;
    idConnectivityList.push_back(t);
  }

  return insert_in_id_map(file_ids, handle);
}

ErrorCode ReadHDF5::update_connectivity()
{
  ErrorCode rval;
  std::vector<IDConnectivity>::iterator i;
  for (i = idConnectivityList.begin(); i != idConnectivityList.end(); ++i) {
    rval = convert_id_to_handle(i->array, i->count * i->nodes_per_elem);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");

    rval = readUtil->update_adjacencies(i->handle, i->count, i->nodes_per_elem, i->array);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }
  idConnectivityList.clear();

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_node_adj_elems(const mhdf_ElemDesc& group, Range* handles_out)
{
  mhdf_Status status;
  ErrorCode rval;

  CHECK_OPEN_HANDLES;

  hid_t table = mhdf_openConnectivitySimple(filePtr, group.handle, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  rval = read_node_adj_elems(group, table, handles_out);

  mhdf_closeData(filePtr, table, &status);
  if (MB_SUCCESS == rval && is_error(status))
    MB_SET_ERR_RET_VAL("ReadHDF5 Failure", MB_FAILURE);

  return rval;
}

ErrorCode ReadHDF5::read_node_adj_elems(const mhdf_ElemDesc& group,
                                        hid_t table_handle,
                                        Range* handles_out)
{
  CHECK_OPEN_HANDLES;

  debug_barrier();

  mhdf_Status status;
  ErrorCode rval;
  IODebugTrack debug_track(debugTrack, std::string(group.handle));

  // Copy data to local variables (makes other code clearer)
  const int node_per_elem = group.desc.vals_per_ent;
  long start_id = group.desc.start_id;
  long remaining = group.desc.count;
  const EntityType type = CN::EntityTypeFromName(group.type);

  // Figure out how many elements we can read in each pass
  long* const buffer = reinterpret_cast<long*>(dataBuffer);
  const long buffer_size = bufferSize / (node_per_elem * sizeof(buffer[0]));
  // Read all element connectivity in buffer_size blocks
  long offset = 0;
  dbgOut.printf(3, "Reading node-adjacent elements from \"%s\" in %ld chunks\n",
                group.handle, (remaining + buffer_size - 1) / buffer_size);
  int nn = 0;
  Range::iterator hint;
  if (handles_out)
    hint = handles_out->begin();
  while (remaining) {
    dbgOut.printf(3, "Reading chunk %d of connectivity data for \"%s\"\n", ++nn, group.handle);

    // Read a block of connectivity data
    const long count = std::min(remaining, buffer_size);
    debug_track.record_io(offset, count);
    assert_range(buffer, count * node_per_elem);
    mhdf_readConnectivityWithOpt(table_handle, offset, count, H5T_NATIVE_LONG, buffer, collIO, &status);
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    offset += count;
    remaining -= count;

    // Count the number of elements in the block that we want,
    // zero connectivity for other elements
    long num_elem = 0;
    long* iter = buffer;
    for (long i = 0; i < count; ++i) {
      for (int j = 0; j < node_per_elem; ++j) {
        iter[j] = (long)idMap.find(iter[j]);
        if (!iter[j]) {
          iter[0] = 0;
          break;
        }
      }
      if (iter[0])
        ++num_elem;
      iter += node_per_elem;
    }

    if (!num_elem) {
      start_id += count;
      continue;
    }

    // Create elements
    EntityHandle handle;
    EntityHandle* array;
    rval = readUtil->get_element_connect((int)num_elem,
                                         node_per_elem,
                                         type,
                                         0,
                                         handle,
                                         array);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");

    // Copy all non-zero connectivity values
    iter = buffer;
    EntityHandle* iter2 = array;
    EntityHandle h = handle;
    for (long i = 0; i < count; ++i) {
      if (!*iter) {
        iter += node_per_elem;
        continue;
      }
      if (!idMap.insert(start_id + i, h++, 1).second)
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

      long* const end = iter + node_per_elem;
      for ( ; iter != end; ++iter, ++iter2)
        *iter2 = (EntityHandle)*iter;
    }
    assert(iter2 - array == num_elem * node_per_elem);
    start_id += count;

    rval = readUtil->update_adjacencies(handle, num_elem, node_per_elem, array);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
    if (handles_out)
      hint = handles_out->insert(hint, handle, handle + num_elem - 1);
   }

  debug_track.all_reduce();
  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_elems(int i, const Range& elems_in, Range& nodes)
{
  CHECK_OPEN_HANDLES;

  debug_barrier();
  dbgOut.tprintf(1, "READING %s CONNECTIVITY (%lu elems in %lu selects)\n", fileInfo->elems[i].handle,
                    (unsigned long)elems_in.size(), (unsigned long)elems_in.psize());

  EntityHandle* const buffer = reinterpret_cast<EntityHandle*>(dataBuffer);
  const int node_per_elem = fileInfo->elems[i].desc.vals_per_ent;
  const size_t buffer_size = bufferSize / (node_per_elem * sizeof(EntityHandle));

  if (elems_in.empty())
    return MB_SUCCESS;

  assert((long)elems_in.front() >= fileInfo->elems[i].desc.start_id);
  assert((long)elems_in.back() - fileInfo->elems[i].desc.start_id < fileInfo->elems[i].desc.count);

  // We don't support version 3 style poly element data
  if (fileInfo->elems[i].desc.vals_per_ent <= 0)
    MB_CHK_ERR(MB_TYPE_OUT_OF_RANGE);

  mhdf_Status status;
  hid_t table = mhdf_openConnectivitySimple(filePtr, fileInfo->elems[i].handle, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  try {
    ReadHDF5Dataset reader(fileInfo->elems[i].handle, table, nativeParallel, mpiComm);
    reader.set_file_ids(elems_in, fileInfo->elems[i].desc.start_id,
                        buffer_size, handleType);
    dbgOut.printf(3, "Reading node list in %lu chunks for \"%s\"\n", reader.get_read_count(), fileInfo->elems[i].handle);
    int nn = 0;
    while (!reader.done()) {
      dbgOut.printf(3, "Reading chunk %d of \"%s\" connectivity\n", ++nn, fileInfo->elems[i].handle);
      size_t num_read;
      reader.read(buffer, num_read);
      std::sort(buffer, buffer + num_read*node_per_elem);
      num_read = std::unique(buffer, buffer + num_read*node_per_elem) - buffer;
      copy_sorted_file_ids(buffer, num_read, nodes);
    }
  }
  catch (ReadHDF5Dataset::Exception) {
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_poly(const mhdf_ElemDesc& elems, const Range& file_ids)
{
  class PolyReader : public ReadHDF5VarLen {
    private:
      const EntityType type;
      ReadHDF5* readHDF5;
    public:
    PolyReader(EntityType elem_type, void* buffer, size_t buffer_size,
               ReadHDF5* owner, DebugOutput& dbg)
               : ReadHDF5VarLen(dbg, buffer, buffer_size),
                 type(elem_type), readHDF5(owner)
               {}
    virtual ~PolyReader() {}
    ErrorCode store_data(EntityHandle file_id, void* data, long len, bool)
    {
      size_t valid;
      EntityHandle* conn = reinterpret_cast<EntityHandle*>(data);
      readHDF5->convert_id_to_handle(conn, len, valid);
      if (valid != (size_t)len)
        MB_CHK_ERR(MB_ENTITY_NOT_FOUND);
      EntityHandle handle;
      ErrorCode rval = readHDF5->moab()->create_element(type, conn, len, handle);
      if (MB_SUCCESS != rval)
        MB_SET_ERR(rval, "ReadHDF5 Failure");

      rval = readHDF5->insert_in_id_map(file_id, handle);
      return rval;
    }
  };

  CHECK_OPEN_HANDLES;

  debug_barrier();

  EntityType type = CN::EntityTypeFromName(elems.type);
  if (type == MBMAXTYPE) {
    MB_SET_ERR(MB_FAILURE, "Unknown element type: \"" << elems.type << "\"");
  }

  hid_t handles[2];
  mhdf_Status status;
  long num_poly, num_conn, first_id;
  mhdf_openPolyConnectivity(filePtr, elems.handle, &num_poly, &num_conn, &first_id,
                            handles, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  std::string nm(elems.handle);
  ReadHDF5Dataset offset_reader((nm + " offsets").c_str(), handles[0], nativeParallel, mpiComm, true);
  ReadHDF5Dataset connect_reader((nm + " data").c_str(), handles[1], nativeParallel, mpiComm, true);

  PolyReader tool(type, dataBuffer, bufferSize, this, dbgOut);
  return tool.read(offset_reader, connect_reader, file_ids, first_id, handleType);
}

ErrorCode ReadHDF5::delete_non_side_elements(const Range& side_ents)
{
  ErrorCode rval;

  // Build list of entities that we need to find the sides of
  Range explicit_ents;
  Range::iterator hint = explicit_ents.begin();
  for (IDMap::iterator i = idMap.begin(); i != idMap.end(); ++i) {
    EntityHandle start = i->value;
    EntityHandle end = i->value + i->count - 1;
    EntityType type = TYPE_FROM_HANDLE(start);
    assert(type == TYPE_FROM_HANDLE(end)); // Otherwise handle space entirely full!!
    if (type != MBVERTEX && type != MBENTITYSET)
      hint = explicit_ents.insert(hint, start, end);
  }
  explicit_ents = subtract(explicit_ents, side_ents);

  // Figure out which entities we want to delete
  Range dead_ents(side_ents);
  Range::iterator ds, de, es;
  ds = dead_ents.lower_bound(CN::TypeDimensionMap[1].first);
  de = dead_ents.lower_bound(CN::TypeDimensionMap[2].first, ds);
  if (ds != de) {
    // Get subset of explicit ents of dimension greater than 1
    es = explicit_ents.lower_bound(CN::TypeDimensionMap[2].first);
    Range subset, adj;
    subset.insert(es, explicit_ents.end());
    rval = iFace->get_adjacencies(subset, 1, false, adj, Interface::UNION);
    if (MB_SUCCESS != rval)
      return rval;
    dead_ents = subtract(dead_ents, adj);
  }
  ds = dead_ents.lower_bound(CN::TypeDimensionMap[2].first);
  de = dead_ents.lower_bound(CN::TypeDimensionMap[3].first, ds);
  assert(de == dead_ents.end());
  if (ds != de) {
    // Get subset of explicit ents of dimension 3
    es = explicit_ents.lower_bound(CN::TypeDimensionMap[3].first);
    Range subset, adj;
    subset.insert(es, explicit_ents.end());
    rval = iFace->get_adjacencies(subset, 2, false, adj, Interface::UNION);
    if (MB_SUCCESS != rval)
      return rval;
    dead_ents = subtract(dead_ents, adj);
  }

  // Now delete anything remaining in dead_ents
  dbgOut.printf(2, "Deleting %lu elements\n", (unsigned long)dead_ents.size());
  dbgOut.print(4, "\tDead entities: ", dead_ents);
  rval = iFace->delete_entities(dead_ents);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  // Remove dead entities from ID map
  while (!dead_ents.empty()) {
    EntityHandle start = dead_ents.front();
    EntityID count = dead_ents.const_pair_begin()->second - start + 1;
    IDMap::iterator rit;
    for (rit = idMap.begin(); rit != idMap.end(); ++rit) 
      if (rit->value <= start && (EntityID)(start - rit->value) < rit->count)
        break;
    if (rit == idMap.end())
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

    EntityID offset = start - rit->value;
    EntityID avail = rit->count - offset;
    if (avail < count)
      count = avail;

    dead_ents.erase(dead_ents.begin(), dead_ents.begin() + count);
    idMap.erase(rit->begin + offset, count);
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_sets(const Range& file_ids)
{
  CHECK_OPEN_HANDLES;

  debug_barrier();

  mhdf_Status status;
  ErrorCode rval;

  const size_t num_sets = fileInfo->sets.count;
  if (!num_sets) // If no sets at all!
    return MB_SUCCESS;

  // Create sets
  std::vector<unsigned> flags(file_ids.size());
  Range::iterator si = file_ids.begin();
  for (size_t i = 0; i < flags.size(); ++i, ++si) 
    flags[i] = setMeta[*si - fileInfo->sets.start_id][3] & ~(long)mhdf_SET_RANGE_BIT;
  EntityHandle start_handle;
  rval = readUtil->create_entity_sets(flags.size(), &flags[0], 0, start_handle);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");
  rval = insert_in_id_map(file_ids, start_handle);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  // Read contents
  if (fileInfo->have_set_contents) {
    long len = 0;
    hid_t handle = mhdf_openSetData(filePtr, &len, &status);
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

    ReadHDF5Dataset dat("set contents", handle, nativeParallel, mpiComm, true);
    rval = read_set_data(file_ids, start_handle, dat, CONTENT);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  // Read set child lists
  if (fileInfo->have_set_children) {
    long len = 0;
    hid_t handle = mhdf_openSetChildren(filePtr, &len, &status);
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

    ReadHDF5Dataset dat("set children", handle, nativeParallel, mpiComm, true);
    rval = read_set_data(file_ids, start_handle, dat, CHILD);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  // Read set parent lists
  if (fileInfo->have_set_parents) {
    long len = 0;
    hid_t handle = mhdf_openSetParents(filePtr, &len, &status);
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

    ReadHDF5Dataset dat("set parents", handle, nativeParallel, mpiComm, true);
    rval = read_set_data(file_ids, start_handle, dat, PARENT);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_all_set_meta()
{
  CHECK_OPEN_HANDLES;

  assert(!setMeta);
  const long num_sets = fileInfo->sets.count;
  if (!num_sets)
    return MB_SUCCESS;

  mhdf_Status status;
  hid_t handle = mhdf_openSetMetaSimple(filePtr, &status);
  if (is_error(status)) {
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  // Allocate extra space if we need it for data conversion
  hid_t meta_type = H5Dget_type(handle);
  size_t size = H5Tget_size(meta_type);
  if (size > sizeof(long)) 
    setMeta = new long[(num_sets * size + (sizeof(long)-1)) / sizeof(long)][4];
   else
    setMeta = new long[num_sets][4];

  // Set some parameters based on whether or not each proc reads the
  // table or only the root reads it and bcasts it to the others
  int rank = 0;
  bool bcast = false;
  hid_t ioprop = H5P_DEFAULT;
#ifdef MOAB_HAVE_MPI
  MPI_Comm comm = 0;
  if (nativeParallel) {
    rank = myPcomm->proc_config().proc_rank();
    comm = myPcomm->proc_config().proc_comm();
    bcast = bcastDuplicateReads;
    if (!bcast)
      ioprop = collIO;
  }
#endif

  if (!bcast || 0 == rank) {
    mhdf_readSetMetaWithOpt(handle, 0, num_sets, meta_type, setMeta, ioprop, &status);
    if (is_error(status)) {
      H5Tclose(meta_type);
      mhdf_closeData(filePtr, handle, &status);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }

     H5Tconvert(meta_type, H5T_NATIVE_LONG, num_sets*4, setMeta, 0, H5P_DEFAULT);
  }
  mhdf_closeData(filePtr, handle, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  H5Tclose(meta_type);

  if (bcast) {
#ifdef MOAB_HAVE_MPI
    int ierr = MPI_Bcast(setMeta, num_sets*4, MPI_LONG, 0, comm);
    if (MPI_SUCCESS != ierr)
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
#else
    assert(rank == 0); // If not MPI, then only one proc
#endif
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_set_ids_recursive(Range& sets_in_out,
                                           bool contained_sets,
                                           bool child_sets)
{
  CHECK_OPEN_HANDLES;
  mhdf_Status status;

  if (!fileInfo->have_set_children)
    child_sets = false;
  if (!fileInfo->have_set_contents)
    contained_sets = false;
  if (!child_sets && !contained_sets)
    return MB_SUCCESS;

  // Open data tables
  if (fileInfo->sets.count == 0) {
    assert(sets_in_out.empty());
    return MB_SUCCESS;
  }

  if (!contained_sets && !child_sets)
    return MB_SUCCESS;

  ReadHDF5Dataset cont("set contents", false, mpiComm);
  ReadHDF5Dataset child("set children", false, mpiComm);

  if (contained_sets) {
    long content_len = 0;
    hid_t content_handle = mhdf_openSetData(filePtr, &content_len, &status);
    if (is_error(status))
       MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    try {
      cont.init(content_handle, true);
    }
    catch (ReadHDF5Dataset::Exception) {
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
  }

  if (child_sets) {
    long child_len = 0;
    hid_t child_handle = mhdf_openSetChildren(filePtr, &child_len, &status);
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    try {
      child.init(child_handle, true);
    }
    catch (ReadHDF5Dataset::Exception) {
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
  }

  ErrorCode rval = MB_SUCCESS;
  Range children, new_children(sets_in_out);
  int iteration_count = 0;
  do {
    ++iteration_count;
    dbgOut.tprintf(2, "Iteration %d of read_set_ids_recursive\n", iteration_count);
    children.clear();
    if (child_sets) {
      rval = read_set_data(new_children, 0, child, CHILD, &children);
      if (MB_SUCCESS != rval)
        break;
    }
    if (contained_sets) {
      rval = read_set_data(new_children, 0, cont, CONTENT, &children);
      // Remove any non-set values
      Range::iterator it = children.lower_bound(fileInfo->sets.start_id);
      children.erase(children.begin(), it);
      it = children.lower_bound(fileInfo->sets.start_id + fileInfo->sets.count);
      children.erase(it, children.end());
      if (MB_SUCCESS != rval)
        break;
    }
    new_children = subtract(children, sets_in_out);
    dbgOut.print_ints(2, "Adding additional contained/child sets", new_children);
    sets_in_out.merge(new_children);
  } while (!new_children.empty());

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::find_sets_containing(Range& sets_out,
                                         bool read_set_containing_parents)
{
  ErrorCode rval;
  mhdf_Status status;

  CHECK_OPEN_HANDLES;

  if (!fileInfo->have_set_contents)
    return MB_SUCCESS;
  assert(fileInfo->sets.count);

  // Open data tables
  long content_len = 0;
  hid_t content_handle = mhdf_openSetData(filePtr, &content_len, &status);
  if (is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  hid_t data_type = H5Dget_type(content_handle);

  rval = find_sets_containing(content_handle, data_type, content_len,
                              read_set_containing_parents, sets_out);

  H5Tclose(data_type);

  mhdf_closeData(filePtr, content_handle, &status);
  if (MB_SUCCESS == rval && is_error(status))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  return rval;
}

static bool set_map_intersect(bool ranged,
                              const long* contents,
                              int content_len,
                              const RangeMap<long, EntityHandle>& id_map)
{
  if (ranged) {
    if (!content_len || id_map.empty())
      return false;

    const long* j = contents;
    const long* const end = contents + content_len;
    assert(content_len % 2 == 0);
    while (j != end) {
      long start = *(j++);
      long count = *(j++);
      if (id_map.intersects(start, count))
        return true;
    }
  }
  else {
    const long* const end = contents + content_len;
    for (const long* i = contents; i != end; ++i)
      if (id_map.exists(*i))
        return true;
  }

  return false;
}

struct SetContOffComp {
  bool operator()(const long a1[4], const long a2[4])
    { return a1[ReadHDF5::CONTENT] < a2[0]; }
};

ErrorCode ReadHDF5::find_sets_containing(hid_t contents_handle,
                                         hid_t content_type,
                                         long contents_len,
                                         bool read_set_containing_parents,
                                         Range& file_ids)
{
  CHECK_OPEN_HANDLES;

  // Scan all set contents data

  const size_t content_size = H5Tget_size(content_type);
  const long num_sets = fileInfo->sets.count;
  dbgOut.printf(2, "Searching contents of %ld\n", num_sets);
  mhdf_Status status;

  int rank = 0;
  bool bcast = false;
#ifdef MOAB_HAVE_MPI
  MPI_Comm comm = 0;
  if (nativeParallel) {
    rank = myPcomm->proc_config().proc_rank();
    comm = myPcomm->proc_config().proc_comm();
    bcast = bcastDuplicateReads;
  }
#endif

  // Check offsets so that we don't read past end of table or
  // walk off end of array.
  long prev = -1;
  for (long  i = 0; i < num_sets; ++i) {
    if (setMeta[i][CONTENT] < prev) {
      std::cerr << "Invalid data in set contents offsets at position "
                << i << ": index " << setMeta[i][CONTENT] 
                << " is less than previous index " << prev << std::endl;
      std::cerr.flush();
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
    prev = setMeta[i][CONTENT];
  }
  if (setMeta[num_sets - 1][CONTENT] >= contents_len) {
    std::cerr << "Maximum set content index " << setMeta[num_sets - 1][CONTENT]
              << " exceeds contents table length of " << contents_len
              << std::endl;
    std::cerr.flush();
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  // Set up buffer for reading set contents
  long* const content_buffer = (long*)dataBuffer;
  const long content_len = bufferSize / std::max(content_size, sizeof(long));

  // Scan set table
  Range::iterator hint = file_ids.begin();
  Range tmp_range;
  long prev_idx = -1;
  int mm = 0;
  long sets_offset = 0;
  long temp_content[4];
  while (sets_offset < num_sets) {
    temp_content[0] = content_len + prev_idx;
    long sets_count = std::lower_bound(setMeta + sets_offset,
                                       setMeta + num_sets,
                                       temp_content,
                                       SetContOffComp())
                                       - setMeta - sets_offset;
    assert(sets_count >= 0 && sets_offset + sets_count <= num_sets);
    if (!sets_count) { // Contents of single set don't fit in buffer
      long content_remaining = setMeta[sets_offset][CONTENT] - prev_idx;
      long content_offset = prev_idx + 1;
      while (content_remaining) {
        long content_count = content_len < content_remaining ?
                             2*(content_len / 2) : content_remaining;
        assert_range(content_buffer, content_count);
        dbgOut.printf(3, "Reading chunk %d (%ld values) from set contents table\n", ++mm, content_count);
        if (!bcast || 0 == rank) {
          if (!bcast)
            mhdf_readSetDataWithOpt(contents_handle, content_offset,
                                    content_count, content_type,
                                    content_buffer, collIO, &status);
          else
            mhdf_readSetData(contents_handle, content_offset,
                             content_count, content_type,
                             content_buffer, &status);
          if (is_error(status))
            MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

          H5Tconvert(content_type, H5T_NATIVE_LONG, content_count, content_buffer, 0, H5P_DEFAULT);
        }
        if (bcast) {
          #ifdef MOAB_HAVE_MPI
            int ierr = MPI_Bcast(content_buffer, content_count, MPI_LONG, 0, comm);
            if (MPI_SUCCESS != ierr)
              MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
          #else
            assert(rank == 0); // If not MPI, then only one proc
          #endif
        }

        if (read_set_containing_parents) {
          tmp_range.clear();
          if (setMeta[sets_offset][3] & mhdf_SET_RANGE_BIT) tmp_range.insert(*content_buffer, *(content_buffer + 1));
          else std::copy(content_buffer, content_buffer + content_count, range_inserter(tmp_range));
          tmp_range = intersect(tmp_range, file_ids);
        }

        if (!tmp_range.empty() ||
            set_map_intersect(setMeta[sets_offset][3] & mhdf_SET_RANGE_BIT,
                              content_buffer, content_count, idMap)) {
          long id = fileInfo->sets.start_id + sets_offset;
          hint = file_ids.insert(hint, id, id);
          if (!nativeParallel) // Don't stop if doing READ_PART because we need to read collectively
            break;
        }
        content_remaining -= content_count;
        content_offset += content_count;
      }
      prev_idx = setMeta[sets_offset][CONTENT];
      sets_count = 1;
    }
    else if (long read_num = setMeta[sets_offset + sets_count - 1][CONTENT] - prev_idx) {
      assert(sets_count > 0);
      assert_range(content_buffer, read_num);
      dbgOut.printf(3, "Reading chunk %d (%ld values) from set contents table\n", ++mm, read_num);
      if (!bcast || 0 == rank) {
        if (!bcast)
          mhdf_readSetDataWithOpt(contents_handle, prev_idx + 1, read_num,
                                  content_type, content_buffer, collIO, &status);
        else
          mhdf_readSetData(contents_handle, prev_idx + 1, read_num,
                           content_type, content_buffer, &status);
        if (is_error(status))
          MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

        H5Tconvert(content_type, H5T_NATIVE_LONG, read_num, content_buffer, 0, H5P_DEFAULT);
      }
      if (bcast) {
        #ifdef MOAB_HAVE_MPI
          int ierr = MPI_Bcast(content_buffer, read_num, MPI_LONG, 0, comm);
          if (MPI_SUCCESS != ierr)
            MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
        #else
          assert(rank == 0); // If not MPI, then only one proc
        #endif
      }

      long* buff_iter = content_buffer;
      for (long i = 0; i < sets_count; ++i) {
        long set_size = setMeta[i + sets_offset][CONTENT] - prev_idx;
        prev_idx += set_size;

        // Check whether contents include set already being loaded
        if (read_set_containing_parents) {
          tmp_range.clear();
          if (setMeta[sets_offset + i][3] & mhdf_SET_RANGE_BIT)
          {
            // put in tmp_range the contents on the set
            // file_ids contain at this points only other sets
            const long* j = buff_iter;
            const long* const end = buff_iter + set_size;
            assert(set_size % 2 == 0);
            while (j != end) {
              long start = *(j++);
              long count = *(j++);
              tmp_range.insert(start, start+count-1);
            }
          }
          else
            std::copy(buff_iter, buff_iter + set_size, range_inserter(tmp_range));
          tmp_range = intersect(tmp_range, file_ids);
        }

        if (!tmp_range.empty() ||
            set_map_intersect(setMeta[sets_offset + i][3] & mhdf_SET_RANGE_BIT,
                              buff_iter, set_size, idMap)) {
          long id = fileInfo->sets.start_id + sets_offset + i;
          hint = file_ids.insert(hint, id, id);
        }
        buff_iter += set_size;
      }
    }

    sets_offset += sets_count;
  }

  return MB_SUCCESS;
}

static Range::iterator copy_set_contents(Range::iterator hint,
                                         int ranged,
                                         EntityHandle* contents,
                                         long length,
                                         Range& results)
{
  if (ranged) {
    assert(length % 2 == 0);
    for (long i = 0; i < length; i += 2)
      hint = results.insert(hint, contents[i], contents[i] + contents[i + 1] - 1);
  }
  else {
    std::sort(contents, contents+length);
    for (long i = 0; i < length; ++i)
      hint = results.insert(hint, contents[i]);
  }
  return hint;
}

ErrorCode ReadHDF5::read_set_data(const Range& set_file_ids,
                                  EntityHandle start_handle,
                                  ReadHDF5Dataset& data,
                                  SetMode mode,
                                  Range* file_ids_out)
{
  ErrorCode rval;
  Range::const_pair_iterator pi;
  Range::iterator out_hint;
  if (file_ids_out)
    out_hint = file_ids_out->begin();

  // Construct range of offsets into data table at which to read
  // Note: all offsets are incremented by TWEAK because Range cannot
  // store zeros.
  const long TWEAK = 1;
  Range data_offsets;
  Range::iterator hint = data_offsets.begin();
  pi = set_file_ids.const_pair_begin();
  if ((long)pi->first == fileInfo->sets.start_id) {
    long second = pi->second - fileInfo->sets.start_id;
    if  (setMeta[second][mode] >= 0)
      hint = data_offsets.insert(hint, TWEAK, setMeta[second][mode] + TWEAK);
    ++pi;
  }
  for ( ; pi != set_file_ids.const_pair_end(); ++pi) {
    long first = pi->first - fileInfo->sets.start_id;
    long second = pi->second - fileInfo->sets.start_id;
    long idx1 = setMeta[first - 1][mode] + 1;
    long idx2 = setMeta[second][mode];
    if (idx2 >= idx1)
      hint = data_offsets.insert(hint, idx1 + TWEAK, idx2 + TWEAK);
  }
  try {
    data.set_file_ids(data_offsets, TWEAK, bufferSize / sizeof(EntityHandle), handleType);
  }
  catch (ReadHDF5Dataset::Exception) {
    return MB_FAILURE;
  }

  // We need to increment this for each processed set because
  // the sets were created in the order of the ids in file_ids.
  EntityHandle h = start_handle;

  const long ranged_flag = (mode == CONTENT) ? mhdf_SET_RANGE_BIT : 0;

  std::vector<EntityHandle> partial; // For when we read only part of the contents of a set/entity
  Range::const_iterator fileid_iter = set_file_ids.begin();
  EntityHandle* buffer = reinterpret_cast<EntityHandle*>(dataBuffer);
  size_t count, offset;

  int nn = 0;
  while (!data.done()) {
    dbgOut.printf(3, "Reading chunk %d of %s\n", ++nn, data.get_debug_desc());
    try {
      data.read(buffer, count);
    }
    catch (ReadHDF5Dataset::Exception) {
      return MB_FAILURE;
    }

    // Assert not appropriate here - I might have treated all my file ids, but maybe
    // another proc hasn't; for me, count will be zero, so I won't do anything, but
    // I still need to go through the motions to make the read work

    // Handle 'special' case where we read some, but not all
    // of the data for an entity during the last iteration.
    offset = 0;
    if (!partial.empty()) { // Didn't read all of previous entity
      assert(fileid_iter != set_file_ids.end());
      size_t num_prev = partial.size();
      size_t idx = *fileid_iter - fileInfo->sets.start_id;
      size_t len = idx ? setMeta[idx][mode] - setMeta[idx - 1][mode] : setMeta[idx][mode] + 1;
      offset = len - num_prev;
      if (offset > count) { // Still don't have all
        partial.insert(partial.end(), buffer, buffer + count);
        continue;
      }

      partial.insert(partial.end(), buffer, buffer + offset);
      if (file_ids_out) {
        out_hint = copy_set_contents(out_hint, setMeta[idx][3] & ranged_flag,
                                     &partial[0], partial.size(), *file_ids_out);
      }
      else {
        switch (mode) {
          size_t valid;
          case CONTENT:
            if (setMeta[idx][3] & ranged_flag) {
              if (len % 2) 
                MB_CHK_ERR(MB_INDEX_OUT_OF_RANGE);
              Range range;
              convert_range_to_handle(&partial[0], len / 2, range);
              rval = moab()->add_entities(h, range);
            }
            else {
              convert_id_to_handle(&partial[0], len, valid);
              rval = moab()->add_entities(h, &partial[0], valid);
            }
            break;
          case CHILD:
            convert_id_to_handle(&partial[0], len, valid);
            rval = moab()->add_child_meshsets(h, &partial[0], valid);
            break;
          case PARENT:
            convert_id_to_handle(&partial[0], len, valid);
            rval = moab()->add_parent_meshsets(h, &partial[0], valid);
            break;
          default:
            break;
        }
        if (MB_SUCCESS != rval)
          MB_SET_ERR(rval, "ReadHDF5 Failure");
      }

      ++fileid_iter;
      ++h;
      partial.clear();
    }

    // Process contents for all entities for which we
    // have read the complete list
    while (offset < count) {
      assert(fileid_iter != set_file_ids.end());
      size_t idx = *fileid_iter - fileInfo->sets.start_id;
      size_t len = idx ? setMeta[idx][mode] - setMeta[idx - 1][mode] : setMeta[idx][mode] + 1;
       // If we did not read all of the final entity,
       // store what we did read to be processed in the
       // next iteration
      if (offset + len > count) {
        partial.insert(partial.end(), buffer + offset, buffer + count);
        break;
      }

      if (file_ids_out) {
        out_hint = copy_set_contents(out_hint, setMeta[idx][3] & ranged_flag,
                                     buffer + offset, len, *file_ids_out);
      }
      else {
        switch (mode) {
          size_t valid;
          case CONTENT:
            if (setMeta[idx][3] & ranged_flag) {
              if (len % 2) 
                MB_CHK_ERR(MB_INDEX_OUT_OF_RANGE);
              Range range;
              convert_range_to_handle(buffer + offset, len / 2, range);
              rval = moab()->add_entities(h, range);
            }
            else {
              convert_id_to_handle(buffer + offset, len, valid);
              rval = moab()->add_entities(h, buffer + offset, valid);
            }
            break;
          case CHILD:
            convert_id_to_handle(buffer + offset, len, valid);
            rval = moab()->add_child_meshsets(h, buffer + offset, valid);
            break;
          case PARENT:
            convert_id_to_handle(buffer + offset, len, valid);
            rval = moab()->add_parent_meshsets(h, buffer + offset, valid);
            break;
          default:
            break;
        }
        if (MB_SUCCESS != rval)
          MB_SET_ERR(rval, "ReadHDF5 Failure");
      }

      ++fileid_iter;
      ++h;
      offset += len;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::get_set_contents(const Range& sets, Range& file_ids)
{
  CHECK_OPEN_HANDLES;

  if (!fileInfo->have_set_contents)
    return MB_SUCCESS;
  dbgOut.tprint(2, "Reading set contained file IDs\n");
  try {
    mhdf_Status status;
    long content_len;
    hid_t contents = mhdf_openSetData(filePtr, &content_len, &status);
    if (is_error(status)) 
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    ReadHDF5Dataset data("set contents", contents, nativeParallel, mpiComm, true);

    return read_set_data(sets, 0, data, CONTENT, &file_ids);
  }
  catch (ReadHDF5Dataset::Exception) {
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }
}

ErrorCode ReadHDF5::read_adjacencies(hid_t table, long table_len)
{
  CHECK_OPEN_HANDLES;

  ErrorCode rval;
  mhdf_Status status;

  debug_barrier();

  hid_t read_type = H5Dget_type(table);
  if (read_type < 0) 
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  const bool convert = !H5Tequal(read_type, handleType);

  EntityHandle* buffer = (EntityHandle*)dataBuffer;
  size_t chunk_size = bufferSize / H5Tget_size(read_type);
  size_t remaining = table_len;
  size_t left_over = 0;
  size_t offset = 0;
  dbgOut.printf(3, "Reading adjacency list in %lu chunks\n",
    (unsigned long)(remaining + chunk_size - 1) / chunk_size);
  int nn = 0;
  while (remaining) {
    dbgOut.printf(3, "Reading chunk %d of adjacency list\n", ++nn);

    size_t count = std::min(chunk_size, remaining);
    count -= left_over;
    remaining -= count;

    assert_range(buffer + left_over, count);
    mhdf_readAdjacencyWithOpt(table, offset, count, read_type, buffer + left_over,
                              collIO, &status);
    if (is_error(status))
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    
    if (convert) {
      herr_t err = H5Tconvert(read_type, handleType, count, buffer + left_over, 0, H5P_DEFAULT);
      if (err < 0)
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }

    EntityHandle* iter = buffer;
    EntityHandle* end = buffer + count + left_over;
    while (end - iter >= 3) {
      EntityHandle h = idMap.find(*iter++);
      EntityHandle count2 = *iter++;
      if (!h) {
        iter += count2;
        continue;
      }

      if (count2 < 1)
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

      if (end < count2 + iter) {
        iter -= 2;
        break;
      }

      size_t valid;
      convert_id_to_handle(iter, count2, valid, idMap);
      rval = iFace->add_adjacencies(h, iter, valid, false);
      if (MB_SUCCESS != rval)
        MB_SET_ERR(rval, "ReadHDF5 Failure");

      iter += count2;
    }

    left_over = end - iter;
    assert_range((char*)buffer, left_over);
    assert_range((char*)iter, left_over);
    memmove(buffer, iter, left_over);
  }

  assert(!left_over); // Unexpected truncation of data

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_tag(int tag_index)
{
  CHECK_OPEN_HANDLES;

  dbgOut.tprintf(2, "Reading tag \"%s\"\n", fileInfo->tags[tag_index].name);

  debug_barrier();

  ErrorCode rval;
  mhdf_Status status;
  Tag tag = 0;
  hid_t read_type = -1;
  bool table_type;
  rval = create_tag(fileInfo->tags[tag_index], tag, read_type);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  if (fileInfo->tags[tag_index].have_sparse) {
    hid_t handles[3];
    long num_ent, num_val;
    mhdf_openSparseTagData(filePtr,
                           fileInfo->tags[tag_index].name,
                           &num_ent, &num_val,
                           handles, &status);
    if (is_error(status)) {
      if (read_type) H5Tclose(read_type);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }

    table_type = false;
    if (read_type == 0) {
      read_type = H5Dget_type(handles[1]);
      if (read_type == 0) {
        mhdf_closeData(filePtr, handles[0], &status);
        mhdf_closeData(filePtr, handles[0], &status);
        if (fileInfo->tags[tag_index].size <= 0)
          mhdf_closeData(filePtr, handles[2], &status);
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
      }
      table_type = true;
    }

    if (fileInfo->tags[tag_index].size > 0) {
      dbgOut.printf(2, "Reading sparse data for tag \"%s\"\n", fileInfo->tags[tag_index].name);
      rval = read_sparse_tag(tag, read_type, handles[0], handles[1], num_ent);
    }
    else {
      dbgOut.printf(2, "Reading var-len sparse data for tag \"%s\"\n", fileInfo->tags[tag_index].name);
      rval = read_var_len_tag(tag, read_type, handles[0], handles[1], handles[2], num_ent, num_val);
    }

    if (table_type) {
      H5Tclose(read_type);
      read_type = 0;
    }

    mhdf_closeData(filePtr, handles[0], &status);
    if (MB_SUCCESS == rval && is_error(status))
      rval = MB_FAILURE;
    mhdf_closeData(filePtr, handles[1], &status);
    if (MB_SUCCESS == rval && is_error(status))
      rval = MB_FAILURE;
    if (fileInfo->tags[tag_index].size <= 0) {
      mhdf_closeData(filePtr, handles[2], &status);
      if (MB_SUCCESS == rval && is_error(status))
        rval = MB_FAILURE;
    }
    if (MB_SUCCESS != rval) {
      if (read_type) H5Tclose(read_type);
      MB_SET_ERR(rval, "ReadHDF5 Failure");
    }
  }

  for (int j = 0; j < fileInfo->tags[tag_index].num_dense_indices; ++j) {
    long count;
    const char* name = 0;
    mhdf_EntDesc* desc;
    int elem_idx = fileInfo->tags[tag_index].dense_elem_indices[j];
    if (elem_idx == -2) {
      desc = &fileInfo->sets;
      name = mhdf_set_type_handle();
    }
    else if (elem_idx == -1) {
      desc = &fileInfo->nodes;
      name = mhdf_node_type_handle();
    }
    else if (elem_idx >= 0 && elem_idx < fileInfo->num_elem_desc) {
      desc = &fileInfo->elems[elem_idx].desc;
      name = fileInfo->elems[elem_idx].handle;
    }
    else {
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }

    dbgOut.printf(2, "Read dense data block for tag \"%s\" on \"%s\"\n", fileInfo->tags[tag_index].name, name);

    hid_t handle = mhdf_openDenseTagData(filePtr,
                                         fileInfo->tags[tag_index].name,
                                         name,
                                         &count, &status);
    if (is_error(status)) {
      rval = MB_FAILURE; // rval = error(MB_FAILURE);
      break;
    }

    if (count > desc->count) {
      mhdf_closeData(filePtr, handle, &status);
      MB_SET_ERR(MB_FAILURE, "Invalid data length for dense tag data: " << name << "/" << fileInfo->tags[tag_index].name);
    }

    table_type = false;
    if (read_type == 0) {
      read_type = H5Dget_type(handle);
      if (read_type == 0) {
        mhdf_closeData(filePtr, handle, &status);
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
      }
      table_type = true;
    }

    rval = read_dense_tag(tag, name, read_type, handle, desc->start_id, count);

    if (table_type) {
      H5Tclose(read_type);
      read_type = 0;
    }

    mhdf_closeData(filePtr, handle, &status);
    if (MB_SUCCESS != rval)
      break;
    if (is_error(status)) {
      rval = MB_FAILURE;
      break;
    }
  }

  if (read_type) 
    H5Tclose(read_type);
  return rval;
}

ErrorCode ReadHDF5::create_tag(const mhdf_TagDesc& info,
                               Tag& handle,
                               hid_t& hdf_type)
{
  CHECK_OPEN_HANDLES;

  ErrorCode rval;
  mhdf_Status status;
  TagType storage;
  DataType mb_type;
  bool re_read_default = false;

  switch (info.storage) {
    case mhdf_DENSE_TYPE:
      storage = MB_TAG_DENSE;
      break;
    case mhdf_SPARSE_TYPE:
      storage = MB_TAG_SPARSE;
      break;
    case mhdf_BIT_TYPE:
      storage = MB_TAG_BIT;
      break;
    case mhdf_MESH_TYPE:
      storage = MB_TAG_MESH;
      break;
    default:
      MB_SET_ERR(MB_FAILURE, "Invalid storage type for tag '" << info.name << "': " << info.storage);
  }

  // Type-specific stuff
  if (info.type == mhdf_BITFIELD) {
    if (info.size < 1 || info.size > 8) {
      MB_SET_ERR(MB_FAILURE, "Invalid bit tag: class is MB_TAG_BIT, num bits = " << info.size);
    }
    hdf_type = H5Tcopy(H5T_NATIVE_B8);
    mb_type = MB_TYPE_BIT;
    if (hdf_type < 0)
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }
  else if (info.type == mhdf_OPAQUE) {
    mb_type = MB_TYPE_OPAQUE;

    // Check for user-provided type
    Tag type_handle;
    std::string tag_type_name = "__hdf5_tag_type_";
    tag_type_name += info.name;
    rval = iFace->tag_get_handle(tag_type_name.c_str(), sizeof(hid_t), MB_TYPE_OPAQUE, type_handle);
    if (MB_SUCCESS == rval) {
      EntityHandle root = 0;
      rval = iFace->tag_get_data(type_handle, &root, 1, &hdf_type);
      if (MB_SUCCESS != rval)
        MB_SET_ERR(rval, "ReadHDF5 Failure");
      hdf_type = H5Tcopy(hdf_type);
      re_read_default = true;
    }
    else if (MB_TAG_NOT_FOUND == rval) {
      hdf_type = 0;
    }
    else
      MB_SET_ERR(rval, "ReadHDF5 Failure");

    if (hdf_type < 0)
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }
  else {
    switch (info.type) {
      case mhdf_INTEGER:
        hdf_type = H5T_NATIVE_INT;
        mb_type = MB_TYPE_INTEGER;
        break;
      case mhdf_FLOAT:
        hdf_type = H5T_NATIVE_DOUBLE;
        mb_type = MB_TYPE_DOUBLE;
        break;
      case mhdf_BOOLEAN:
        hdf_type = H5T_NATIVE_UINT;
        mb_type = MB_TYPE_INTEGER;
        break;
      case mhdf_ENTITY_ID:
        hdf_type = handleType;
        mb_type = MB_TYPE_HANDLE;
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }

    if (info.size > 1) { // Array
        hsize_t tmpsize = info.size;
#if defined(H5Tarray_create_vers) && H5Tarray_create_vers > 1
        hdf_type = H5Tarray_create2(hdf_type, 1, &tmpsize);
#else
        hdf_type = H5Tarray_create(hdf_type, 1, &tmpsize, NULL);
#endif
    }
    else {
      hdf_type = H5Tcopy(hdf_type);
    }
    if (hdf_type < 0)
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  // If default or global/mesh value in file, read it.
  if (info.default_value || info.global_value) {
    if (re_read_default) {
      mhdf_getTagValues(filePtr, info.name, hdf_type, info.default_value, info.global_value, &status);
      if (mhdf_isError(&status)) {
        if (hdf_type)
          H5Tclose(hdf_type);
        MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
      }
    }

    if (MB_TYPE_HANDLE == mb_type) {
      if (info.default_value) {
        rval = convert_id_to_handle((EntityHandle*)info.default_value, info.default_value_size);
        if (MB_SUCCESS != rval) {
          if (hdf_type)
            H5Tclose(hdf_type);
          MB_SET_ERR(rval, "ReadHDF5 Failure");
        }
      }
      if (info.global_value) {
        rval = convert_id_to_handle((EntityHandle*)info.global_value, info.global_value_size);
        if (MB_SUCCESS != rval) {
          if (hdf_type)
            H5Tclose(hdf_type);
          MB_SET_ERR(rval, "ReadHDF5 Failure");
        }
      }
    }
  }

  // Get tag handle, creating if necessary
  if (info.size < 0) 
    rval = iFace->tag_get_handle(info.name, info.default_value_size,
                                 mb_type, handle, storage | MB_TAG_CREAT | MB_TAG_VARLEN | MB_TAG_DFTOK,
                                 info.default_value);
  else
    rval = iFace->tag_get_handle(info.name, info.size, mb_type, handle,
                                 storage | MB_TAG_CREAT | MB_TAG_DFTOK, info.default_value);
  if (MB_SUCCESS != rval) {
    if (hdf_type)
      H5Tclose(hdf_type);
    MB_SET_ERR(MB_FAILURE, "Tag type in file does not match type in database for \"" << info.name << "\"");
  }

  if (info.global_value) {
    EntityHandle root = 0;
    if (info.size > 0) { // Fixed-length tag
      rval = iFace->tag_set_data(handle, &root, 1, info.global_value);
    }
    else {
      int tag_size = info.global_value_size;
      rval = iFace->tag_set_by_ptr(handle, &root, 1, &info.global_value, &tag_size);
    }
    if (MB_SUCCESS != rval) {
      if (hdf_type)
        H5Tclose(hdf_type);
      MB_SET_ERR(rval, "ReadHDF5 Failure");
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_dense_tag(Tag tag_handle,
                                   const char* ent_name,
                                   hid_t hdf_read_type,
                                   hid_t data,
                                   long start_id,
                                   long num_values)
{
  CHECK_OPEN_HANDLES;

  ErrorCode rval;
  DataType mb_type;

  rval = iFace->tag_get_data_type(tag_handle, mb_type);
  if (MB_SUCCESS != rval) 
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  int read_size;
  rval = iFace->tag_get_bytes(tag_handle, read_size);
  if (MB_SUCCESS != rval) // Wrong function for variable-length tags
    MB_SET_ERR(rval, "ReadHDF5 Failure");
  //if (MB_TYPE_BIT == mb_type) 
    //read_size = (read_size + 7) / 8; // Convert bits to bytes, plus 7 for ceiling

  if (hdf_read_type) { // If not opaque
    hsize_t hdf_size = H5Tget_size(hdf_read_type);
    if (hdf_size != (hsize_t)read_size)
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  // Get actual entities read from file
  Range file_ids, handles;
  Range::iterator f_ins = file_ids.begin(), h_ins = handles.begin();
  IDMap::iterator l, u;
  l = idMap.lower_bound(start_id);
  u = idMap.lower_bound(start_id + num_values - 1);
  if (l != idMap.end() && start_id + num_values > l->begin) {
    if (l == u) {
      size_t beg = std::max(start_id, l->begin);
      size_t end = std::min(start_id + num_values, u->begin + u->count) - 1;
      f_ins = file_ids.insert(f_ins, beg, end);
      h_ins = handles.insert(h_ins, l->value + (beg - l->begin),
                                    l->value + (end - l->begin));
    }
    else {
      size_t beg = std::max(start_id, l->begin);
      f_ins = file_ids.insert(f_ins, beg, l->begin + l->count - 1);
      h_ins = handles.insert(h_ins, l->value + (beg - l->begin), l->value + l->count - 1);
      for (++l; l != u; ++l) {
        f_ins = file_ids.insert(f_ins, l->begin, l->begin + l->count - 1);
        h_ins = handles.insert(h_ins, l->value, l->value + l->count - 1);
      }
      if (u != idMap.end() && u->begin < start_id + num_values) {
        size_t end = std::min(start_id + num_values, u->begin + u->count - 1);
        f_ins = file_ids.insert(f_ins, u->begin, end);
        h_ins = handles.insert(h_ins, u->value, u->value + end - u->begin);
      }
    }
  }

  // Given that all of the entities for this dense tag data should
  // have been created as a single contiguous block, the resulting
  // MOAB handle range should be contiguous.
  // THE ABOVE IS NOT NECESSARILY TRUE. SOMETIMES LOWER-DIMENSION
  // ENTS ARE READ AND THEN DELETED FOR PARTIAL READS.
  //assert(handles.empty() || handles.size() == (handles.back() - handles.front() + 1));

  std::string tn("<error>");
  iFace->tag_get_name(tag_handle, tn);
  tn += " data for ";
  tn += ent_name;
  try {
    h_ins = handles.begin();
    ReadHDF5Dataset reader(tn.c_str(), data, nativeParallel, mpiComm, false);
    long buffer_size = bufferSize / read_size;
    reader.set_file_ids(file_ids, start_id, buffer_size, hdf_read_type);
    dbgOut.printf(3, "Reading dense data for tag \"%s\" and group \"%s\" in %lu chunks\n",
                      tn.c_str(), ent_name, reader.get_read_count());
    int nn = 0;
    while (!reader.done()) {
      dbgOut.printf(3, "Reading chunk %d of \"%s\" data\n", ++nn, tn.c_str());

      size_t count;
      reader.read(dataBuffer, count);

      if (MB_TYPE_HANDLE == mb_type) {
        rval = convert_id_to_handle((EntityHandle*)dataBuffer, count * read_size / sizeof(EntityHandle));
        if (MB_SUCCESS != rval)
          MB_SET_ERR(rval, "ReadHDF5 Failure");
      }

      Range ents;
      Range::iterator end = h_ins;
      end += count;
      ents.insert(h_ins, end);
      h_ins = end;

      rval = iFace->tag_set_data(tag_handle, ents, dataBuffer);
      if (MB_SUCCESS != rval) {
        dbgOut.printf(1, "Internal error setting data for tag \"%s\"\n", tn.c_str());
        MB_SET_ERR(rval, "ReadHDF5 Failure");
      }
    }
  }
  catch (ReadHDF5Dataset::Exception) {
    dbgOut.printf(1, "Internal error reading dense data for tag \"%s\"\n", tn.c_str());
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  return MB_SUCCESS;
}

// Read entire ID table and for those file IDs corresponding
// to entities that we have read from the file add both the
// offset into the offset range and the handle into the handle
// range. If handles are not ordered, switch to using a vector.
ErrorCode ReadHDF5::read_sparse_tag_indices(const char* name,
                                            hid_t id_table,
                                            EntityHandle start_offset, // Can't put zero in a Range
                                            Range& offset_range,
                                            Range& handle_range,
                                            std::vector<EntityHandle>& handle_vect)
{
  CHECK_OPEN_HANDLES;

  offset_range.clear();
  handle_range.clear();
  handle_vect.clear();

  ErrorCode rval;
  Range::iterator handle_hint = handle_range.begin();
  Range::iterator offset_hint = offset_range.begin();

  EntityHandle* idbuf = (EntityHandle*)dataBuffer;
  size_t idbuf_size = bufferSize / sizeof(EntityHandle);

  std::string tn(name);
  tn += " indices";

  assert(start_offset > 0); // Can't put zero in a Range
  try {
    ReadHDF5Dataset id_reader(tn.c_str(), id_table, nativeParallel, mpiComm, false);
    id_reader.set_all_file_ids(idbuf_size, handleType);
    size_t offset = start_offset;
    dbgOut.printf(3, "Reading file ids for sparse tag \"%s\" in %lu chunks\n", name, id_reader.get_read_count());
    int nn = 0;
    while (!id_reader.done()) {\
      dbgOut.printf(3, "Reading chunk %d of \"%s\" IDs\n", ++nn, name);
      size_t count;
      id_reader.read(idbuf, count);

      rval = convert_id_to_handle(idbuf, count);
      if (MB_SUCCESS != rval)
        MB_SET_ERR(rval, "ReadHDF5 Failure");

      // idbuf will now contain zero-valued handles for those
      // tag values that correspond to entities we are not reading
      // from the file.
      for (size_t i = 0; i < count; ++i) {
        if (idbuf[i]) {
          offset_hint = offset_range.insert(offset_hint, offset + i);
          if (!handle_vect.empty()) {
            handle_vect.push_back(idbuf[i]);
          }
          else if (handle_range.empty() || idbuf[i] > handle_range.back()) {
            handle_hint = handle_range.insert(handle_hint, idbuf[i]);
          }
          else {
            handle_vect.resize(handle_range.size());
            std::copy(handle_range.begin(), handle_range.end(), handle_vect.begin());
            handle_range.clear();
            handle_vect.push_back(idbuf[i]);
            dbgOut.print(2, "Switching to unordered list for tag handle list\n");
          }
        }
      }

      offset += count;
    }
  }
  catch (ReadHDF5Dataset::Exception) {
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_sparse_tag(Tag tag_handle,
                                    hid_t hdf_read_type,
                                    hid_t id_table,
                                    hid_t value_table,
                                    long /*num_values*/)
{
  CHECK_OPEN_HANDLES;

  // Read entire ID table and for those file IDs corresponding
  // to entities that we have read from the file add both the
  // offset into the offset range and the handle into the handle
  // range.  If handles are not ordered, switch to using a vector.
  const EntityHandle base_offset = 1; // Can't put zero in a Range
  std::vector<EntityHandle> handle_vect;
  Range handle_range, offset_range;
  std::string tn("<error>");
  iFace->tag_get_name(tag_handle, tn);
  ErrorCode rval = read_sparse_tag_indices(tn.c_str(),
                                           id_table, base_offset,
                                           offset_range, handle_range,
                                           handle_vect);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  DataType mbtype;
  rval = iFace->tag_get_data_type(tag_handle, mbtype);
  if (MB_SUCCESS != rval) 
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  int read_size;
  rval = iFace->tag_get_bytes(tag_handle, read_size);
  if (MB_SUCCESS != rval) // Wrong function for variable-length tags
    MB_SET_ERR(rval, "ReadHDF5 Failure");
  //if (MB_TYPE_BIT == mbtype) 
    //read_size = (read_size + 7) / 8; // Convert bits to bytes, plus 7 for ceiling

  if (hdf_read_type) { // If not opaque
    hsize_t hdf_size = H5Tget_size(hdf_read_type);
    if (hdf_size != (hsize_t)read_size) 
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  const int handles_per_tag = read_size / sizeof(EntityHandle);

  // Now read data values
  size_t chunk_size = bufferSize / read_size;
  try {
    ReadHDF5Dataset val_reader((tn + " values").c_str(), value_table, nativeParallel, mpiComm, false);
    val_reader.set_file_ids(offset_range, base_offset, chunk_size, hdf_read_type);
    dbgOut.printf(3, "Reading sparse values for tag \"%s\" in %lu chunks\n", tn.c_str(), val_reader.get_read_count());
    int nn = 0;
    size_t offset = 0;
    while (!val_reader.done()) {
      dbgOut.printf(3, "Reading chunk %d of \"%s\" values\n", ++nn, tn.c_str());
      size_t count;
      val_reader.read(dataBuffer, count);
      if (MB_TYPE_HANDLE == mbtype) {
        rval = convert_id_to_handle((EntityHandle*)dataBuffer, count*handles_per_tag);
        if (MB_SUCCESS != rval)
          MB_SET_ERR(rval, "ReadHDF5 Failure");
      }

      if (!handle_vect.empty()) {
        rval = iFace->tag_set_data(tag_handle, &handle_vect[offset], count, dataBuffer);
        offset += count;
      }
      else {
        Range r;
        r.merge(handle_range.begin(), handle_range.begin() + count);
        handle_range.erase(handle_range.begin(), handle_range.begin() + count);
        rval = iFace->tag_set_data(tag_handle, r, dataBuffer);
      }
      if (MB_SUCCESS != rval)
        MB_SET_ERR(rval, "ReadHDF5 Failure");
    }
  }
  catch (ReadHDF5Dataset::Exception) {
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_var_len_tag(Tag tag_handle,
                                     hid_t hdf_read_type,
                                     hid_t ent_table,
                                     hid_t val_table,
                                     hid_t off_table,
                                     long /*num_entities*/,
                                     long /*num_values*/)
{
  CHECK_OPEN_HANDLES;

  ErrorCode rval;
  DataType mbtype;

  rval = iFace->tag_get_data_type(tag_handle, mbtype);
  if (MB_SUCCESS != rval) 
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  // Can't do variable-length bit tags
  if (MB_TYPE_BIT == mbtype)
    MB_CHK_ERR(MB_VARIABLE_DATA_LENGTH);

  // If here, MOAB tag must be variable-length
  int mbsize;
  if (MB_VARIABLE_DATA_LENGTH != iFace->tag_get_bytes(tag_handle, mbsize)) {
    assert(false);
    MB_CHK_ERR(MB_VARIABLE_DATA_LENGTH);
  }

  int read_size;
  if (hdf_read_type) {
    hsize_t hdf_size = H5Tget_size(hdf_read_type);
    if (hdf_size < 1)
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    read_size = hdf_size;
  }
  else {
    // Opaque
    read_size = 1;
  }

  std::string tn("<error>");
  iFace->tag_get_name(tag_handle, tn);

  // Read entire ID table and for those file IDs corresponding
  // to entities that we have read from the file add both the
  // offset into the offset range and the handle into the handle
  // range. If handles are not ordered, switch to using a vector.
  const EntityHandle base_offset = 1; // Can't put zero in a Range
  std::vector<EntityHandle> handle_vect;
  Range handle_range, offset_range;
  rval = read_sparse_tag_indices(tn.c_str(),
                                 ent_table, base_offset,
                                 offset_range, handle_range,
                                 handle_vect);

  // This code only works if the id_table is an ordered list.
  // This assumption was also true for the previous iteration
  // of this code, but wasn't checked. MOAB's file writer
  // always writes an ordered list for id_table.
  if (!handle_vect.empty()) {
    MB_SET_ERR(MB_FAILURE, "Unordered file ids for variable length tag not supported");
  }

  class VTReader : public ReadHDF5VarLen {
      Tag tagHandle;
      bool isHandle;
      size_t readSize;
      ReadHDF5* readHDF5;
    public:
      ErrorCode store_data(EntityHandle file_id, void* data, long count, bool)
      {
        ErrorCode rval1;
        if (isHandle) {
          assert(readSize == sizeof(EntityHandle));
          rval1 = readHDF5->convert_id_to_handle((EntityHandle*)data, count);MB_CHK_ERR(rval1);
        }
        int n = count;
        return readHDF5->moab()->tag_set_by_ptr(tagHandle, &file_id, 1, &data, &n);
      }
      VTReader(DebugOutput& debug_output, void* buffer, size_t buffer_size,
               Tag tag, bool is_handle_tag, size_t read_size1, ReadHDF5* owner)
        : ReadHDF5VarLen(debug_output, buffer, buffer_size),
          tagHandle(tag),
          isHandle(is_handle_tag),
          readSize(read_size1),
          readHDF5(owner)
      {}
  };

  VTReader tool(dbgOut, dataBuffer, bufferSize, tag_handle,
                 MB_TYPE_HANDLE == mbtype, read_size, this);
  try {
    // Read offsets into value table.
    std::vector<unsigned> counts;
    Range offsets;
    ReadHDF5Dataset off_reader((tn + " offsets").c_str(), off_table, nativeParallel, mpiComm, false);
    rval = tool.read_offsets(off_reader, offset_range, base_offset,
                             base_offset, offsets, counts);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");

    // Read tag values
    Range empty;
    ReadHDF5Dataset val_reader((tn + " values").c_str(), val_table, nativeParallel, mpiComm, false);
    rval = tool.read_data(val_reader, offsets, base_offset, hdf_read_type,
                          handle_range, counts, empty);
    if (MB_SUCCESS != rval)
      MB_SET_ERR(rval, "ReadHDF5 Failure");
  }
  catch (ReadHDF5Dataset::Exception) {
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::convert_id_to_handle(EntityHandle* array,
                                         size_t size)
{
  convert_id_to_handle(array, size, idMap);
  return MB_SUCCESS;
}

void ReadHDF5::convert_id_to_handle(EntityHandle* array,
                                    size_t size,
                                    const RangeMap<long, EntityHandle>& id_map)
{
  for (EntityHandle* const end = array + size; array != end; ++array)
    *array = id_map.find(*array);
}

void ReadHDF5::convert_id_to_handle(EntityHandle* array,
                                    size_t size, size_t& new_size,
                                    const RangeMap<long, EntityHandle>& id_map)
{
  RangeMap<long, EntityHandle>::const_iterator it;
  new_size = 0;
  for (size_t i = 0; i < size; ++i) {
    it = id_map.lower_bound(array[i]);
    if (it != id_map.end() && it->begin <= (long)array[i])
      array[new_size++] = it->value + (array[i] - it->begin);
  }
}

void ReadHDF5::convert_range_to_handle(const EntityHandle* ranges,
                                       size_t num_ranges,
                                       const RangeMap<long, EntityHandle>& id_map,
                                       Range& merge)
{
  RangeMap<long, EntityHandle>::iterator it = id_map.begin();
  Range::iterator hint = merge.begin();
  for (size_t i = 0; i < num_ranges; ++i) {
    long id = ranges[2*i];
    const long end = id + ranges[2*i + 1];
    // We assume that 'ranges' is sorted, but check just in case it isn't.
    if (it == id_map.end() || it->begin > id)
      it = id_map.begin();
    it = id_map.lower_bound(it, id_map.end(), id);
    if (it == id_map.end())
      continue;
    if (id < it->begin)
      id = it->begin;
    while (id < end) {
      if (id < it->begin) id = it->begin;
      const long off = id - it->begin;
      long count = std::min(it->count - off,  end - id);
      // It is possible that this new subrange is starting after the end
      // It will result in negative count, which does not make sense
      // We are done with this range, go to the next one
      if (count <= 0)
        break;
      hint = merge.insert(hint, it->value + off, it->value + off + count - 1);
      id += count;
      if (id < end) {
        if (++it == id_map.end())
          break;
        if (it->begin > end)
          break;
      }
    }
  }
}

ErrorCode ReadHDF5::convert_range_to_handle(const EntityHandle* array,
                                            size_t num_ranges,
                                            Range& range)
{
  convert_range_to_handle(array, num_ranges, idMap, range);
  return MB_SUCCESS;
}

ErrorCode ReadHDF5::insert_in_id_map(const Range& file_ids,
                                     EntityHandle start_id)
{
  IDMap tmp_map;
  bool merge = !idMap.empty() && !file_ids.empty() && idMap.back().begin > (long)file_ids.front();
  IDMap& map = merge ? tmp_map : idMap;
  Range::const_pair_iterator p;
  for (p = file_ids.const_pair_begin(); p != file_ids.const_pair_end(); ++p) {
    size_t count = p->second - p->first + 1;
    if (!map.insert(p->first, start_id, count).second)
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    start_id += count;
  }
  if (merge && !idMap.merge(tmp_map))
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::insert_in_id_map(long file_id,
                                     EntityHandle handle)
{
  if (!idMap.insert(file_id, handle, 1).second)
    MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_qa(EntityHandle)
{
  CHECK_OPEN_HANDLES;

  mhdf_Status status;
  //std::vector<std::string> qa_list;

  int qa_len;
  char** qa = mhdf_readHistory(filePtr, &qa_len, &status);
  if (mhdf_isError(&status)) {
    MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
  }
  //qa_list.resize(qa_len);
  for (int i = 0; i < qa_len; i++) {
    //qa_list[i] = qa[i];
    free(qa[i]);
  }
  free(qa);

  /** FIX ME - how to put QA list on set?? */

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::store_file_ids(Tag tag)
{
  CHECK_OPEN_HANDLES;

  //typedef int tag_type;
  typedef long tag_type;
  // change it to be able to read much bigger files (long is 64 bits ...)

  tag_type* buffer = reinterpret_cast<tag_type*>(dataBuffer);
  const long buffer_size = bufferSize / sizeof(tag_type);
  for (IDMap::iterator i = idMap.begin(); i != idMap.end(); ++i) {
    IDMap::Range range = *i;

    // Make sure the values will fit in the tag type
    IDMap::key_type rv = range.begin + (range.count - 1);
    tag_type tv = (tag_type)rv;
    if ((IDMap::key_type)tv != rv) {
      assert(false);
      return MB_INDEX_OUT_OF_RANGE;
    }

    while (range.count) {
      long count = buffer_size < range.count ? buffer_size : range.count;

      Range handles;
      handles.insert(range.value, range.value + count - 1);
      range.value += count;
      range.count -= count;
      for (long j = 0; j < count; ++j) 
        buffer[j] = (tag_type)range.begin++;

      ErrorCode rval = iFace->tag_set_data(tag, handles, buffer);
      if (MB_SUCCESS != rval)
        return rval;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::store_sets_file_ids()
{
  CHECK_OPEN_HANDLES;

  // create a tag that will not be saved, but it will be
  // used by visit plugin to match the sets and their file ids
  // it is the same type as the tag defined in ReadParallelcpp, for file id
  Tag setFileIdTag;
  long default_val=0;
  ErrorCode rval = iFace->tag_get_handle("__FILE_ID_FOR_SETS", sizeof(long), MB_TYPE_OPAQUE, setFileIdTag,
      (MB_TAG_DENSE | MB_TAG_CREAT),  &default_val);

  if (MB_SUCCESS != rval || 0==setFileIdTag)
    return rval;
  //typedef int tag_type;
  typedef long tag_type;
  // change it to be able to read much bigger files (long is 64 bits ...)

  tag_type* buffer = reinterpret_cast<tag_type*>(dataBuffer);
  const long buffer_size = bufferSize / sizeof(tag_type);
  for (IDMap::iterator i = idMap.begin(); i != idMap.end(); ++i) {
    IDMap::Range range = *i;
    EntityType htype = iFace->type_from_handle(range.value);
    if (MBENTITYSET!=htype)
      continue;
    // work only with entity sets
    // Make sure the values will fit in the tag type
    IDMap::key_type rv = range.begin + (range.count - 1);
    tag_type tv = (tag_type)rv;
    if ((IDMap::key_type)tv != rv) {
      assert(false);
      return MB_INDEX_OUT_OF_RANGE;
    }

    while (range.count) {
      long count = buffer_size < range.count ? buffer_size : range.count;

      Range handles;
      handles.insert(range.value, range.value + count - 1);
      range.value += count;
      range.count -= count;
      for (long j = 0; j < count; ++j)
        buffer[j] = (tag_type)range.begin++;

      rval = iFace->tag_set_data(setFileIdTag, handles, buffer);
      if (MB_SUCCESS != rval)
        return rval;
    }
  }
  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_tag_values(const char* file_name,
                                    const char* tag_name,
                                    const FileOptions& opts,
                                    std::vector<int>& tag_values_out,
                                    const SubsetList* subset_list)
{
  ErrorCode rval;

  rval = set_up_read(file_name, opts);
  if (MB_SUCCESS != rval)
    MB_SET_ERR(rval, "ReadHDF5 Failure");

  int tag_index;
  rval = find_int_tag(tag_name, tag_index);
  if (MB_SUCCESS != rval) {
    clean_up_read(opts);
    MB_SET_ERR(rval, "ReadHDF5 Failure");
  }

  if (subset_list) {
    Range file_ids;
    rval = get_subset_ids(subset_list->tag_list, subset_list->tag_list_length, file_ids);
    if (MB_SUCCESS != rval) {
      clean_up_read(opts);
      MB_SET_ERR(rval, "ReadHDF5 Failure");
    }

    rval = read_tag_values_partial(tag_index, file_ids, tag_values_out);
    if (MB_SUCCESS != rval) {
      clean_up_read(opts);
      MB_SET_ERR(rval, "ReadHDF5 Failure");
    }
  }
  else {
    rval = read_tag_values_all(tag_index, tag_values_out);
    if (MB_SUCCESS != rval) {
      clean_up_read(opts);
      MB_SET_ERR(rval, "ReadHDF5 Failure");
    }
  }

  return clean_up_read(opts);
}

ErrorCode ReadHDF5::read_tag_values_partial(int tag_index,
                                            const Range& file_ids,
                                            std::vector<int>& tag_values)
{
  CHECK_OPEN_HANDLES;

  mhdf_Status status;
  const mhdf_TagDesc& tag = fileInfo->tags[tag_index];
  long num_ent, num_val;
  size_t count;
  std::string tn(tag.name);

  // Read sparse values
  if (tag.have_sparse) {
    hid_t handles[3];
    mhdf_openSparseTagData(filePtr, tag.name, &num_ent, &num_val, handles, &status);
    if (mhdf_isError(&status)) {
      MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
    }

    try {
      // Read all entity handles and fill 'offsets' with ranges of
      // offsets into the data table for entities that we want.
      Range offsets;
      long* buffer = reinterpret_cast<long*>(dataBuffer);
      const long buffer_size = bufferSize/sizeof(long);
      ReadHDF5Dataset ids((tn + " ids").c_str(), handles[0], nativeParallel, mpiComm);
      ids.set_all_file_ids(buffer_size, H5T_NATIVE_LONG);
      size_t offset = 0;
      dbgOut.printf(3, "Reading sparse IDs for tag \"%s\" in %lu chunks\n",
                    tag.name, ids.get_read_count());
      int nn = 0;
      while (!ids.done()) {
        dbgOut.printf(3, "Reading chunk %d of IDs for \"%s\"\n", ++nn, tag.name);
        ids.read(buffer, count);

        std::sort(buffer, buffer + count);
        Range::iterator ins = offsets.begin();
        Range::const_iterator i = file_ids.begin();
        for (size_t j = 0; j < count; ++j) {
          while (i != file_ids.end() && (long)*i < buffer[j])
            ++i;
          if (i == file_ids.end())
            break;
          if ((long)*i == buffer[j]) {
            ins = offsets.insert(ins, j + offset, j + offset);
          }
        }

        offset += count;
      }

      tag_values.clear();
      tag_values.reserve(offsets.size());
      const size_t data_buffer_size = bufferSize / sizeof(int);
      int* data_buffer = reinterpret_cast<int*>(dataBuffer);
      ReadHDF5Dataset vals((tn + " sparse vals").c_str(), handles[1], nativeParallel, mpiComm);
      vals.set_file_ids(offsets, 0, data_buffer_size, H5T_NATIVE_INT);
      dbgOut.printf(3, "Reading sparse values for tag \"%s\" in %lu chunks\n",
                    tag.name, vals.get_read_count());
      nn = 0;
      // Should normally only have one read call, unless sparse nature
      // of file_ids caused reader to do something strange
      while (!vals.done()) {
        dbgOut.printf(3, "Reading chunk %d of values for \"%s\"\n", ++nn, tag.name);
        vals.read(data_buffer, count);
        tag_values.insert(tag_values.end(), data_buffer, data_buffer + count);
      }
    }
    catch (ReadHDF5Dataset::Exception) {
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
  }

  std::sort(tag_values.begin(), tag_values.end());
  tag_values.erase(std::unique(tag_values.begin(), tag_values.end()), tag_values.end());

  // Read dense values
  std::vector<int> prev_data, curr_data;
  for (int i = 0; i < tag.num_dense_indices; ++i) {
    int grp = tag.dense_elem_indices[i];
    const char* gname = 0;
    mhdf_EntDesc* desc = 0;
    if (grp == -1) {
      gname = mhdf_node_type_handle();
      desc = &fileInfo->nodes;
    }
    else if (grp == -2) {
      gname = mhdf_set_type_handle();
      desc = &fileInfo->sets;
    }
    else {
      assert(grp >= 0 && grp < fileInfo->num_elem_desc);
      gname = fileInfo->elems[grp].handle;
      desc = &fileInfo->elems[grp].desc;
    }

    Range::iterator s = file_ids.lower_bound((EntityHandle)(desc->start_id));
    Range::iterator e = Range::lower_bound(s, file_ids.end(),
                                           (EntityHandle)(desc->start_id) + desc->count);
    Range subset;
    subset.merge(s, e);

    hid_t handle = mhdf_openDenseTagData(filePtr, tag.name, gname, &num_val, &status);
    if (mhdf_isError(&status)) {
      MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
    }

    try {
      curr_data.clear();
      tag_values.reserve(subset.size());
      const size_t data_buffer_size = bufferSize/sizeof(int);
      int* data_buffer = reinterpret_cast<int*>(dataBuffer);

      ReadHDF5Dataset reader((tn + " dense vals").c_str(), handle, nativeParallel, mpiComm);
      reader.set_file_ids(subset, desc->start_id, data_buffer_size, H5T_NATIVE_INT);
      dbgOut.printf(3, "Reading dense data for tag \"%s\" and group \"%s\" in %lu chunks\n",
                    tag.name, fileInfo->elems[grp].handle, reader.get_read_count());
      int nn = 0;
      // Should normally only have one read call, unless sparse nature
      // of file_ids caused reader to do something strange
      while (!reader.done()) {
        dbgOut.printf(3, "Reading chunk %d of \"%s\"/\"%s\"\n", ++nn, tag.name, fileInfo->elems[grp].handle);
        reader.read(data_buffer, count);
        curr_data.insert(curr_data.end(), data_buffer, data_buffer + count);
      }
    }
    catch (ReadHDF5Dataset::Exception) {
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }

    std::sort(curr_data.begin(), curr_data.end());
    curr_data.erase(std::unique(curr_data.begin(), curr_data.end()), curr_data.end());
    prev_data.clear();
    tag_values.swap(prev_data);
    std::set_union(prev_data.begin(), prev_data.end(),
                   curr_data.begin(), curr_data.end(),
                   std::back_inserter(tag_values));
  }

  return MB_SUCCESS;
}

ErrorCode ReadHDF5::read_tag_values_all(int tag_index,
                                        std::vector<int>& tag_values)
{
  CHECK_OPEN_HANDLES;

  mhdf_Status status;
  const mhdf_TagDesc& tag = fileInfo->tags[tag_index];
  long junk, num_val;

  // Read sparse values
  if (tag.have_sparse) {
    hid_t handles[3];
    mhdf_openSparseTagData(filePtr, tag.name, &junk, &num_val, handles, &status);
    if (mhdf_isError(&status)) {
      MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
    }

    mhdf_closeData(filePtr, handles[0], &status);
    if (mhdf_isError(&status)) {
      MB_SET_ERR_CONT(mhdf_message(&status));
      mhdf_closeData(filePtr, handles[1], &status);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }

    hid_t file_type = H5Dget_type(handles[1]);
    tag_values.resize(num_val);
    mhdf_readTagValuesWithOpt(handles[1], 0, num_val, file_type,
                              &tag_values[0], collIO, &status);
    if (mhdf_isError(&status)) {
      MB_SET_ERR_CONT(mhdf_message(&status));
      H5Tclose(file_type);
      mhdf_closeData(filePtr, handles[1], &status);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }
    H5Tconvert(file_type, H5T_NATIVE_INT, num_val, &tag_values[0], 0, H5P_DEFAULT);
    H5Tclose(file_type);

    mhdf_closeData(filePtr, handles[1], &status);
    if (mhdf_isError(&status)) {
      MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
    }
  }

  std::sort(tag_values.begin(), tag_values.end());
  tag_values.erase(std::unique(tag_values.begin(), tag_values.end()), tag_values.end());

  // Read dense values
  std::vector<int> prev_data, curr_data;
  for (int i = 0; i < tag.num_dense_indices; ++i) {
    int grp = tag.dense_elem_indices[i];
    const char* gname = 0;
    if (grp == -1)
      gname = mhdf_node_type_handle();
    else if (grp == -2)
      gname = mhdf_set_type_handle();
    else
      gname = fileInfo->elems[grp].handle;
    hid_t handle = mhdf_openDenseTagData(filePtr, tag.name, gname, &num_val, &status);
    if (mhdf_isError(&status)) {
      MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
    }

    hid_t file_type = H5Dget_type(handle);
    curr_data.resize(num_val);
    mhdf_readTagValuesWithOpt(handle, 0, num_val, file_type, &curr_data[0], collIO, &status);
    if (mhdf_isError(&status)) {
      MB_SET_ERR_CONT(mhdf_message(&status));
      H5Tclose(file_type);
      mhdf_closeData(filePtr, handle, &status);
      MB_SET_ERR(MB_FAILURE, "ReadHDF5 Failure");
    }

    H5Tconvert(file_type, H5T_NATIVE_INT, num_val, &curr_data[0], 0, H5P_DEFAULT);
    H5Tclose(file_type);
    mhdf_closeData(filePtr, handle, &status);
    if (mhdf_isError(&status)) {
      MB_SET_ERR(MB_FAILURE, mhdf_message(&status));
    }

    std::sort(curr_data.begin(), curr_data.end());
    curr_data.erase(std::unique(curr_data.begin(), curr_data.end()), curr_data.end());

    prev_data.clear();
    tag_values.swap(prev_data);
    std::set_union(prev_data.begin(), prev_data.end(),
                   curr_data.begin(), curr_data.end(),
                   std::back_inserter(tag_values));
  }

  return MB_SUCCESS;
}
void ReadHDF5::print_times()
{
#ifdef MOAB_HAVE_MPI
  if (!myPcomm) {
    double recv[NUM_TIMES];
    MPI_Reduce((void*)_times, recv, NUM_TIMES, MPI_DOUBLE, MPI_MAX, 0, myPcomm->proc_config().proc_comm());
    for (int i=0; i<NUM_TIMES; i++)
      _times[i]=recv[i]; // just get the max from all of them
  }
  if (0==myPcomm->proc_config().proc_rank() )
  {
#endif

    std::cout << "ReadHDF5:             " << _times[TOTAL_TIME] << std::endl
              << "  get set meta        " << _times[SET_META_TIME] << std::endl
              << "  partial subsets     " << _times[SUBSET_IDS_TIME] << std::endl
              << "  partition time      " << _times[GET_PARTITION_TIME] << std::endl
              << "  get set ids         " << _times[GET_SET_IDS_TIME] << std::endl
              << "  set contents        " << _times[GET_SET_CONTENTS_TIME] << std::endl
              << "  polyhedra           " << _times[GET_POLYHEDRA_TIME] << std::endl
              << "  elements            " << _times[GET_ELEMENTS_TIME] << std::endl
              << "  nodes               " << _times[GET_NODES_TIME] << std::endl
              << "  node adjacency      " << _times[GET_NODEADJ_TIME] << std::endl
              << "  side elements       " << _times[GET_SIDEELEM_TIME] << std::endl
              << "  update connectivity " << _times[UPDATECONN_TIME] << std::endl
              << "  adjacency           " << _times[ADJACENCY_TIME] << std::endl
              << "  delete non_adj      "  << _times[DELETE_NON_SIDEELEM_TIME] << std::endl
              << "  recursive sets      " << _times[READ_SET_IDS_RECURS_TIME] << std::endl
              << "  find contain_sets   " << _times[FIND_SETS_CONTAINING_TIME] << std::endl
              << "  read sets           " << _times[READ_SETS_TIME] << std::endl
              << "  read tags           " << _times[READ_TAGS_TIME] << std::endl
              << "  store file ids      " << _times[STORE_FILE_IDS_TIME] << std::endl
              << "  read qa records     " << _times[READ_QA_TIME] << std::endl;

#ifdef MOAB_HAVE_MPI
  }
#endif
}

} // namespace moab
