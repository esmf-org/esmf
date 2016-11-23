/** \file   ReadHDF5VarLen.cpp
 *  \author Jason Kraftcheck 
 *  \date   2010-09-04
 */

#include "ReadHDF5VarLen.hpp"
#include "ReadHDF5Dataset.hpp"
#include "H5Tpublic.h"
#include <assert.h>

namespace moab {

bool ReadHDF5VarLen::is_ranged( EntityHandle file_id,
                                Range::const_iterator& ranged_iter,
                                Range::const_iterator range_end )
{
  if (ranged_iter == range_end)
    return false;
  
  assert( file_id <= *ranged_iter );
  if (*ranged_iter != file_id) 
    return false;
  
  ++ranged_iter;
  return true;
}

ErrorCode ReadHDF5VarLen::read_data( 
                                ReadHDF5Dataset& data_set,
                                const Range& offsets,
                                EntityHandle start_offset,
                                hid_t data_type,
                                const Range& file_ids,
                                const std::vector<unsigned>& vals_per_ent,
                                const Range& ranged_file_ids )
{
  ErrorCode rval;
  const size_t value_size = H5Tget_size( data_type );
  const size_t buffer_size = bufferSize / value_size;
  unsigned char* const data_buffer = reinterpret_cast<unsigned char*>(dataBuffer);
  std::vector<unsigned char> partial; // for when we read only part of the contents of a set/entity
  Range::const_iterator fileid_iter = file_ids.begin();
  Range::const_iterator ranged_iter = ranged_file_ids.begin();
  std::vector<unsigned>::const_iterator count_iter = vals_per_ent.begin();
  size_t count, offset;
  bool ranged;
  int nn = 0;
  
  assert( file_ids.size() == vals_per_ent.size() );
  
  try {
    data_set.set_file_ids( offsets, start_offset, buffer_size, data_type );
  }
  catch (ReadHDF5Dataset::Exception ) {
    return MB_FAILURE;
  }
  
  dbgOut.printf( 3, "Reading %s in %lu chunks\n", data_set.get_debug_desc(), data_set.get_read_count() );
  
  while (!data_set.done()) {
    dbgOut.printf( 3, "Reading chunk %d of %s\n", ++nn, data_set.get_debug_desc() );
    try { 
      data_set.read( data_buffer, count );
    }
    catch (ReadHDF5Dataset::Exception ) {
      return MB_FAILURE;
    }
    
    assert( 0 == count || fileid_iter != file_ids.end() );
    
      // Handle 'special' case where we read some, but not all
      // of the data for an entity during the last iteration.
    offset = 0;
    if (!partial.empty()) { // didn't read all of previous entity
      assert( fileid_iter != file_ids.end() );
      assert( 0 == (partial.size() % value_size) );
      size_t num_prev = partial.size() / value_size;
      offset = *count_iter - num_prev;
      if (offset > count) { // still don't have all
        partial.insert( partial.end(), data_buffer, data_buffer+count*value_size );
        continue;
      }
      
      partial.insert( partial.end(), data_buffer, data_buffer+offset*value_size );
      
      ranged = is_ranged( *fileid_iter, ranged_iter, ranged_file_ids.end() );
      assert(partial.size() == *count_iter * value_size );
      rval = store_data( *fileid_iter, &partial[0], *count_iter, ranged );
      if (MB_SUCCESS != rval)
        return rval;
      
      ++count_iter;
      ++fileid_iter;
      partial.clear();
    }
    
      // Process contents for all entities for which we 
      // have read the complete list
    while (count_iter != vals_per_ent.end() && offset + *count_iter <= count) {
      assert( fileid_iter != file_ids.end() );
      ranged = is_ranged( *fileid_iter, ranged_iter, ranged_file_ids.end() );
      rval = store_data( *fileid_iter, data_buffer + offset*value_size, *count_iter, ranged );
      if (MB_SUCCESS != rval)
        return rval;
      
      offset += *count_iter;
      ++count_iter;
      ++fileid_iter;
    }
    
      // If we did not read all of the final entity,
      // store what we did read to be processed in the
      // next iteration
    if (offset < count) {
      assert(partial.empty());
      partial.insert( partial.end(), 
                      data_buffer + offset*value_size, 
                      data_buffer + count*value_size );
    }
  }
  // NOTE: If the last set is empty, we will not process it here
  // assert(fileid_iter == file_ids.end());
#ifndef NDEBUG
  for (;fileid_iter != file_ids.end(); ++fileid_iter) 
    assert(0 == *count_iter++);
#endif
  return MB_SUCCESS;
}
/*
ErrorCode ReadHDF5VarLen::read_offsets( ReadHDF5Dataset& data_set,
                                        const Range& file_ids,
                                        EntityHandle start_file_id,
                                        unsigned num_columns,
                                        const unsigned indices[],
                                        EntityHandle nudge,
                                        Range offsets_out[],
                                        std::vector<unsigned> counts_out[],
                                        Range* ranged_file_ids = 0 )
{
  const int local_index = 1;

    // sanity check
  const unsigned max_cols = ranged_file_ids ? data_set.columns() - 1 : data_set.columns()
  for (unsigned i = 0; i < num_columns; ++i) {
    assert(indices[i] >= max_cols);
    if (indices[i] >= max_cols)    
      return MB_FAILURE;
 }
  
    // Use hints to make sure insertion into ranges is O(1)
  std::vector<Range::iterator> hints;
  if (ranged_file_ids) {
    hints.resize( num_colums + 1 );
    hints.back() = ranged_file_ids->begin();
  }
  else {
    hints.resize( num_columns );
  }
  for (unsigned i = 0; i < num_columns; ++i)
    offsets_out[i].clear();
    counts_out[i].clear();
    counts_out[i].reserve( file_ids.size() );
    hints[i] = offsets_out[i].begin();
  }

    // If we only need one colunm from a multi-column data set,
    // then read only that column.
  if (num_columns == 1 && data_set.columns() > 1 && !ranged_file_ids) {
    data_set.set_column( indices[0] );
    indices = &local_index;
  }
  else if (ranged_file_ids && data_set.columns() > 1 && 0 == num_columns) {
    data_set.set_column( data_set.columns() - 1 );
  }
    // NOTE: do not move this above the previous block.  
    //       The previous block changes the resutls of data_set.columns()!
  const size_t table_columns = data_set.columns();

    // Calculate which rows we need to read from the offsets table
  Range rows;
  Range::iterator hint = rows.begin();
  Range::const_pair_iterator pair = file_ids.const_pair_begin();
    // special case if reading first entity in dataset, because
    // there is no previous end value.
  if (pair != file_ids.const_pair_end() && pair->first == start_file_id) 
    hint = rows.insert( nudge, pair->second - start_file_id + nudge );
  while (pair != file_ids.const_pair_end()) {
    hint = rows.insert( hint,
                        pair->first + nudge - 1 - start_file_id, 
                        pair->second + nudge - start_file_id );
    ++pair;
  }
    
    // set up read of offsets dataset
  hsize_t buffer_size = bufferSize / (sizeof(hssize_t) * data_set.columns());
  hssize_t* buffer = reinterpret_cast<hssize_t*>(dataBuffer);
  data_set.set_file_ids( rows, nudge, buffer_size, H5T_NATIVE_HSSIZE );
  std::vector<hssize_t> prev_end;
    // If we're reading the first row of the table, then the 
    // previous end is implicitly -1.
  if (!file_ids.empty() && file_ids.front() == start_file_id) 
    prev_end.resize(num_columns,-1);
  
    // read offset table
  size_t count, offset;
  Range::const_iterator fiter = file_ids.begin();
  while (!data_set.done()) {
    try {
      data_set.read( buffer, count );
    }
    catch (ReadHDF5Dataset::Exception e) {
      return MB_FAILURE;
    }
    if (!count) // might have been NULL read for collectve IO
      continue;
    
      // If the previous end values were read in the previous iteration,
      // then they're stored in prev_end.  
    size_t offset = 0;
    if (!prev_end.empty()) {
       for (unsigned i = 0; i < num_columns; ++i) {
        counts_out[i].push_back( buffer[indices[i]] - prev_end[i] );
        hints[i] = offsets_out[i].insert( hints[i],
                                          prev_end[i] + 1 + nudge,
                                          buffer[indices[i]] + nudge );
      }
      if (ranged_file_ids && (buffer[table_columns-1] & mhdf_SET_RANGE_BIT))
        hints.back() = ranged_file_ids->insert( hints.back(), *fiter );
      ++fiter;
      offset = 1;
      prev_end.clear();
    }

    while (offset < count) {
      assert(fiter != file_ids.end());
        // whenever we get to a gap between blocks we need to 
        // advance one step because we read an extra end id 
        // preceeding teah block
      if (fiter == fiter.start_of_block()) {
        if (offset == count-1) 
          break;
        ++offset;
      }
      
      for (unsigned i = 0; i < num_columns; ++i) {
        size_t s = buffer[(offset-1)*table_columns+indices[i]] + 1;
        size_t e = buffer[ offset   *table_columns+indices[i]];
        counts_out.push_back( e - s + 1 );
        hints[i] = offsets_out.insert( hints[i], s, e );
      }
      if (ranged_file_ids && (buffer[offset*table_columns+table_columns-1] & mhdf_SET_RANGE_BIT))
        hints.back() = ranged_file_ids->insert( hints.back(), *fiter );
      
      ++fiter;
      ++offset;
    }
    
      // If we did not end on the boundary between two blocks,
      // then we need to save the end indices for the final entry
      // for use in the next iteration.  Similarly, if we ended
      // with extra values that were read with the express intention
      // of getting the previus end values for a block, we need to
      // save them.  This case only arises if we hit the break in
      // the above loop.
    if (fiter != fiter.start_of_block() || offset < count) {
      assert(prev_end.empty());
      if (offset == count) {
        --offset;
        assert(fiter != fiter.start_of_block());
      }
      else {
        assert(offset+1 == count);
        assert(fiter == fiter.start_of_block());
      }
      for (unsigned i = 0; i < num_columns; ++i) 
        prev_end.push_back(buffer[offset*table_columns+indices[i]]);
    }
  }
  assert(prev_end.empty());
  assert(fiter == file_ids.end());
  
  return MB_SUCCESS;
}
*/
ErrorCode ReadHDF5VarLen::read_offsets( ReadHDF5Dataset& data_set,
                                        const Range& file_ids,
                                        EntityHandle start_file_id,
                                        EntityHandle nudge,
                                        Range& offsets_out,
                                        std::vector<unsigned>& counts_out )
{
  
    // Use hints to make sure insertion into ranges is O(1)
  offsets_out.clear();
  counts_out.clear();
  counts_out.reserve( file_ids.size() );
  Range::iterator hint;

    // Calculate which rows we need to read from the offsets table
  Range rows;
  hint = rows.begin();
  Range::const_pair_iterator pair = file_ids.const_pair_begin();
    // special case if reading first entity in dataset, because
    // there is no previous end value.
  if (pair != file_ids.const_pair_end() && pair->first == start_file_id) {
    hint = rows.insert( nudge, pair->second - start_file_id + nudge );
    ++pair;
  }
  while (pair != file_ids.const_pair_end()) {
    hint = rows.insert( hint,
                        pair->first  - start_file_id + nudge - 1, 
                        pair->second - start_file_id + nudge );
    ++pair;
  }
    
    // set up read of offsets dataset
  hsize_t buffer_size = bufferSize / sizeof(hssize_t);
  hssize_t* buffer = reinterpret_cast<hssize_t*>(dataBuffer);
  data_set.set_file_ids( rows, nudge, buffer_size, H5T_NATIVE_HSSIZE );
  hssize_t prev_end;
  bool have_prev_end = false;
    // If we're reading the first row of the table, then the 
    // previous end is implicitly -1.
  if (!file_ids.empty() && file_ids.front() == start_file_id)  {
    prev_end = -1;
    have_prev_end = true;
  }
  
  dbgOut.printf( 3, "Reading %s in %lu chunks\n", data_set.get_debug_desc(), data_set.get_read_count() );
  
    // read offset table
  size_t count, offset;
  Range::const_iterator fiter = file_ids.begin();
  hint = offsets_out.begin();
  int nn = 0;
  while (!data_set.done()) {
    dbgOut.printf( 3, "Reading chunk %d of %s\n", ++nn, data_set.get_debug_desc() );
    try {
      data_set.read( buffer, count );
    }
    catch (ReadHDF5Dataset::Exception ) {
      return MB_FAILURE;
    }
    if (!count) // might have been NULL read for collectve IO
      continue;
    
      // If the previous end values were read in the previous iteration,
      // then they're stored in prev_end.  
    offset = 0;
    if (have_prev_end) {
      counts_out.push_back( buffer[0] - prev_end );
      hint = offsets_out.insert( hint,
                                 prev_end + 1 + nudge,
                                 buffer[0] + nudge );
      ++fiter;
      offset = 1;
      have_prev_end = false;
    }

    while (offset < count) {
      assert(fiter != file_ids.end());
        // whenever we get to a gap between blocks we need to 
        // advance one step because we read an extra end id 
        // preceeding teah block
      if (fiter == fiter.start_of_block()) {
        if (offset == count-1) 
          break;
        ++offset;
      }
      
      size_t s = buffer[offset-1] + 1;
      size_t e = buffer[offset];
      counts_out.push_back( e - s + 1 );
      hint = offsets_out.insert( hint, s + nudge, e + nudge );
      
      ++fiter;
      ++offset;
    }
    
      // If we did not end on the boundary between two blocks,
      // then we need to save the end indices for the final entry
      // for use in the next iteration.  Similarly, if we ended
      // with extra values that were read with the express intention
      // of getting the previus end values for a block, we need to
      // save them.  This case only arises if we hit the break in
      // the above loop.
    if (fiter != fiter.start_of_block() || offset < count) {
      assert(!have_prev_end);
      if (offset == count) {
        --offset;
        assert(fiter != fiter.start_of_block());
      }
      else {
        assert(offset+1 == count);
        assert(fiter == fiter.start_of_block());
      }
      have_prev_end = true;
      prev_end = buffer[offset];
    }
  }
  assert(!have_prev_end);
  assert(fiter == file_ids.end());
  
  return MB_SUCCESS;
}

} // namespace moab
