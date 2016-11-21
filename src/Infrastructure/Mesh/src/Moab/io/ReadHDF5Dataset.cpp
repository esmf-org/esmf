/** \file   ReadHDF5Dataset.cpp
 *  \author Jason Kraftcheck 
 *  \date   2010-07-09
 */

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <iostream>
#include "moab/MOABConfig.h"
#include "ReadHDF5Dataset.hpp"

#include "moab_mpe.h"

#include <H5Dpublic.h>
#include <H5Tpublic.h>
#include <H5Ppublic.h>
#include <H5Spublic.h>
#ifdef MOAB_HAVE_HDF5_PARALLEL
#  include <H5FDmpi.h>
#  include <H5FDmpio.h>
#endif

#define HDF5_16API (H5_VERS_MAJOR < 2 && H5_VERS_MINOR < 8)

namespace moab {

// Selection of hyperslabs appears to be superlinear.  Don't try to select
// more than a few thousand at a time or things start to get real slow.
const size_t DEFAULT_HYPERSLAB_SELECTION_LIMIT = 200;
size_t ReadHDF5Dataset::hyperslabSelectionLimit = DEFAULT_HYPERSLAB_SELECTION_LIMIT;
void ReadHDF5Dataset::default_hyperslab_selection_limit()
  { hyperslabSelectionLimit = DEFAULT_HYPERSLAB_SELECTION_LIMIT; }

H5S_seloper_t ReadHDF5Dataset::hyperslabSelectOp = H5S_SELECT_OR;

#ifdef MOAB_HAVE_LIBMPE
static std::pair<int,int> allocate_mpe_state( const char* name, const char* color )
{
  std::pair<int,int> result;
  result.first = MPE_Log_get_event_number();
  result.second = MPE_Log_get_event_number();
  MPE_Describe_state( result.first, result.second, name, color );
  return result;
}
#else
static std::pair<int,int> allocate_mpe_state( const char* , const char* )
{
  return std::pair<int,int> ();
}
#endif

bool ReadHDF5Dataset::haveMPEEvents = false;
std::pair<int,int> ReadHDF5Dataset::mpeReadEvent;
std::pair<int,int> ReadHDF5Dataset::mpeReduceEvent;

ReadHDF5Dataset::ReadHDF5Dataset( const char* debug_desc,
                                  bool parallel,
                                  const Comm* communicator )
  : closeDataSet(false),
    dataSet( -1 ),
    dataSpace( -1 ),
    dataType( -1 ),
    fileType(-1),
    ioProp(H5P_DEFAULT),
    dataSpaceRank(0),
    rowsInTable(0),
    doConversion(false),
    nativeParallel(parallel),
    readCount(0),
    bufferSize(0),
    mpiComm(communicator),
    mpeDesc( debug_desc )
{
  if (!haveMPEEvents) {
    haveMPEEvents = true;
    mpeReadEvent   = allocate_mpe_state( "ReadHDF5Dataset::read", "yellow" );
    mpeReduceEvent = allocate_mpe_state( "ReadHDF5Dataset::all_reduce", "yellow" );
  }
  
#ifndef MOAB_HAVE_HDF5_PARALLEL
  if (nativeParallel) 
    throw Exception(__LINE__);
#else
  if (nativeParallel && !mpiComm)
    throw Exception(__LINE__);
  
  if (mpiComm) {
    ioProp = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(ioProp, H5FD_MPIO_COLLECTIVE);
  }
#endif
}    

ReadHDF5Dataset::ReadHDF5Dataset( const char* debug_desc,
                                  hid_t data_set_handle,
                                  bool parallel,
                                  const Comm* communicator,
                                  bool close_data_set )
  : closeDataSet(close_data_set),
    dataSet( data_set_handle ),
    dataSpace(-1),
    dataType( -1 ),
    fileType(-1),
    ioProp(H5P_DEFAULT),
    dataSpaceRank(0),
    rowsInTable(0),
    doConversion(false),
    nativeParallel(parallel),
    readCount(0),
    bufferSize(0),
    mpiComm(communicator),
    mpeDesc( debug_desc )
{ 
  if (!haveMPEEvents) {
    haveMPEEvents = true;
    mpeReadEvent   = allocate_mpe_state( "ReadHDF5Dataset::read", "yellow" );
    mpeReduceEvent = allocate_mpe_state( "ReadHDF5Dataset::all_reduce", "yellow" );
  }

  init( data_set_handle, close_data_set );
  
#ifndef MOAB_HAVE_HDF5_PARALLEL
  if (nativeParallel) 
    throw Exception(__LINE__);
#else
  if (nativeParallel && !mpiComm)
    throw Exception(__LINE__);
  
  if (mpiComm) {
    ioProp = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_dxpl_mpio(ioProp, H5FD_MPIO_COLLECTIVE);
  }
#endif
}

void ReadHDF5Dataset::init( hid_t data_set_handle, bool close_data_set )
{
  closeDataSet = close_data_set;
  dataSet = data_set_handle;
  
  fileType = H5Dget_type( data_set_handle );
  if (fileType < 0)
    throw Exception(__LINE__);

  dataSpace = H5Dget_space( dataSet );
  if (dataSpace < 0)
    throw Exception(__LINE__);
  
  dataSpaceRank = H5Sget_simple_extent_dims( dataSpace, dataSetCount, dataSetOffset );
  if (dataSpaceRank < 0) 
    throw Exception(__LINE__);
  rowsInTable = dataSetCount[0];
  
  for (int i = 0; i < dataSpaceRank; ++i)
    dataSetOffset[i] = 0;

  currOffset = rangeEnd = internalRange.end();
}

unsigned ReadHDF5Dataset::columns() const
{
  if (dataSpaceRank == 1)
    return 1;
  else if (dataSpaceRank == 2)
    return dataSetCount[1];
  
  throw Exception(__LINE__);
}

void ReadHDF5Dataset::set_column( unsigned column )
{
  if (dataSpaceRank != 2 || column >= dataSetCount[1])
    throw Exception(__LINE__);
  dataSetCount[1] = 1;
  dataSetOffset[1] = column;
}

Range::const_iterator ReadHDF5Dataset::next_end( Range::const_iterator iter )
{
  size_t slabs_remaining = hyperslabSelectionLimit;
  size_t avail = bufferSize;
  while (iter != rangeEnd && slabs_remaining) {
    size_t count = *(iter.end_of_block()) - *iter + 1;
    if (count >= avail) {
      iter += avail;
      break;
    }

    avail -= count;
    iter += count;
    --slabs_remaining;
  }
  return iter;
}


void ReadHDF5Dataset::set_file_ids( const Range& file_ids, 
                                    EntityHandle start_id,
                                    hsize_t row_count,
                                    hid_t data_type )
{
  startID = start_id;
  currOffset = file_ids.begin();
  rangeEnd = file_ids.end();
  readCount = 0;
  bufferSize = row_count;
  
  // if a) user specified buffer size and b) we're doing a true
  // parallel partial read and c) we're doing collective I/O, then
  // we need to know the maximum number of reads that will be done.
#ifdef MOAB_HAVE_HDF5_PARALLEL
  if (nativeParallel) {
    Range::const_iterator iter = currOffset;
    while (iter != rangeEnd) {
      ++readCount;
      iter = next_end( iter );
    }
    
    MPE_Log_event(mpeReduceEvent.first, (int)readCount, mpeDesc.c_str());
    unsigned long recv = readCount, send = readCount;
    MPI_Allreduce( &send, &recv, 1, MPI_UNSIGNED_LONG, MPI_MAX, *mpiComm );
    readCount = recv;
    MPE_Log_event(mpeReduceEvent.second, (int)readCount, mpeDesc.c_str());
  }
#endif

  dataType = data_type;
  htri_t equal = H5Tequal( fileType, dataType );
  if (equal < 0)
    throw Exception(__LINE__);
  doConversion = !equal;

    // We always read in the format of the file to avoid stupind HDF5
    // library behavior when reading in parallel.  We call H5Tconvert
    // ourselves to do the data conversion.  If the type we're reading
    // from the file is larger than the type we want in memory, then
    // we need to reduce num_rows so that we can read the larger type
    // from the file into the passed buffer mean to accomodate num_rows
    // of values of the smaller in-memory type.
  if (doConversion) {
    size_t mem_size, file_size;
    mem_size = H5Tget_size( dataType );
    file_size = H5Tget_size( fileType );
    if (file_size > mem_size)
      bufferSize = bufferSize * mem_size / file_size;
  }
}

void ReadHDF5Dataset::set_all_file_ids( hsize_t row_count, hid_t data_type )
{
  internalRange.clear();
  internalRange.insert( (EntityHandle)1, (EntityHandle)(rowsInTable) );
  set_file_ids( internalRange, 1, row_count, data_type );
}

ReadHDF5Dataset::~ReadHDF5Dataset() 
{
  if (fileType >= 0)
    H5Tclose( fileType );
  if (dataSpace >= 0)
    H5Sclose( dataSpace );
  if (closeDataSet && dataSet >= 0)
    H5Dclose( dataSet );
  dataSpace = dataSet = -1;
  if (ioProp != H5P_DEFAULT)
    H5Pclose( ioProp );
}

void ReadHDF5Dataset::read( void* buffer,
                            size_t& rows_read )
{
  herr_t err;
  rows_read = 0;

  MPE_Log_event(mpeReadEvent.first, (int)readCount, mpeDesc.c_str());
  if (currOffset != rangeEnd) {

      // Build H5S hyperslab selection describing the portions of the
      // data set to read
    H5S_seloper_t sop = H5S_SELECT_SET;
    Range::iterator new_end = next_end( currOffset );
    while (currOffset != new_end) {
      size_t count = *(currOffset.end_of_block()) - *currOffset + 1;
      if (new_end != rangeEnd && *currOffset + count > *new_end) {
        count = *new_end - *currOffset;
      }
      rows_read += count;

      dataSetOffset[0] = *currOffset - startID;
      dataSetCount[0] = count;
      err = H5Sselect_hyperslab( dataSpace, sop, dataSetOffset, NULL, dataSetCount, 0 );
      if (err < 0)
        throw Exception(__LINE__);
      sop = hyperslabSelectOp; // subsequent calls to select_hyperslab append

      currOffset += count;
    }

      // Create a data space describing the memory in which to read the data
    dataSetCount[0] = rows_read;
    hid_t mem_id = H5Screate_simple( dataSpaceRank, dataSetCount, NULL );
    if (mem_id < 0)
      throw Exception(__LINE__);

      // Do the actual read
    err = H5Dread( dataSet, fileType, mem_id, dataSpace, ioProp, buffer );
    H5Sclose( mem_id );
    if (err < 0)
      throw Exception(__LINE__);
      
    if (readCount)
      --readCount;
  
    if (doConversion) {
      err = H5Tconvert( fileType, dataType, rows_read*columns(), buffer, 0, H5P_DEFAULT);
      if (err < 0)
        throw Exception(__LINE__);
    }  
  }
  else if (readCount) {
    null_read();
    --readCount;
  }
  MPE_Log_event(mpeReadEvent.second, (int)readCount, mpeDesc.c_str());
}

void ReadHDF5Dataset::null_read()
{
  herr_t err;
  err = H5Sselect_none( dataSpace );
  if (err < 0)
    throw Exception(__LINE__);
  
//#if HDF5_16API
  hsize_t one = 1;
  hid_t mem_id = H5Screate_simple( 1, &one, NULL );
  if (mem_id < 0)
    throw Exception(__LINE__);
  err = H5Sselect_none( mem_id );
  if (err < 0) {
    H5Sclose(mem_id);
    throw Exception(__LINE__);
  }
//#else
//  hid_t mem_id = H5Screate(H5S_NULL);
//  if (mem_id < 0)
//    throw Exception(__LINE__);
//#endif

  err = H5Dread( dataSet, fileType, mem_id, dataSpace, ioProp, 0 );
  H5Sclose( mem_id );
  if (err < 0)
    throw Exception(__LINE__);
}

} // namespace moab
