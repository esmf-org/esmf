/*
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

/**\file RangeSeqIntersectIter.cpp
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2006-08-11
 */

#include "RangeSeqIntersectIter.hpp"
#include "SequenceManager.hpp"
#include "EntitySequence.hpp"
#include <assert.h>

namespace moab {

ErrorCode RangeSeqIntersectIter::init( Range::const_iterator start,
                                           Range::const_iterator end )
{
  mSequence = 0;
  rangeIter = start;

    // special case : nothing to iterate over
  if (start == end) {
    mStartHandle = mEndHandle = mLastHandle = 0;
    return MB_FAILURE;
  }

    // normal case
  mStartHandle = *start;
  --end;
  mLastHandle = *end;
  mEndHandle = (*rangeIter).second;
  if (mEndHandle > mLastHandle)
    mEndHandle = mLastHandle;

#if MB_RANGE_SEQ_INTERSECT_ITER_STATS
  ErrorCode result = update_entity_sequence();
  update_stats(mEndHandle - mStartHandle + 1);
  return result;
#else
  return update_entity_sequence();
#endif
}
  

ErrorCode RangeSeqIntersectIter::step()
{
    // If at end, return MB_FAILURE
  if (is_at_end())
    return MB_FAILURE; 
    // If the last block was at the end of the rangeIter pair,
    // then advance the iterator and set the next block
  else if (mEndHandle == (*rangeIter).second) {
    ++rangeIter;
    mStartHandle = (*rangeIter).first;
  }
    // Otherwise start with next entity in the pair
  else {
    mStartHandle = mEndHandle + 1;
  }
    // Always take the remaining entities in the rangeIter pair.
    // will trim up the end of the range in update_entity_sequence().
  mEndHandle = (*rangeIter).second;
  if (mEndHandle > mLastHandle)
    mEndHandle = mLastHandle;
  
    // Now trim up the range (decrease mEndHandle) as necessary
    // for the corresponding EntitySquence
#if MB_RANGE_SEQ_INTERSECT_ITER_STATS
  ErrorCode result = update_entity_sequence();
  update_stats(mEndHandle - mStartHandle + 1);
  return result;
#else
  return update_entity_sequence();
#endif
}

ErrorCode RangeSeqIntersectIter::update_entity_sequence()
{
    // mStartHandle to mEndHandle is a subset of the Range.
    // Update sequence data as necessary and trim that subset
    // (reduce mEndHandle) for the current EntitySequence.
  
    // Need to update the sequence pointer?
  if (!mSequence || mStartHandle > mSequence->end_handle()) {
  
      // Check that the mStartHandle is valid
    if (TYPE_FROM_HANDLE(mStartHandle) >= MBMAXTYPE)
      return MB_TYPE_OUT_OF_RANGE;

    if (MB_SUCCESS != mSequenceManager->find( mStartHandle, mSequence ))
      return find_invalid_range();
  }
    
    // if mEndHandle is past end of sequence or block of used
    // handles within sequence, shorten it.
  if(mEndHandle > mSequence->end_handle())
    mEndHandle = mSequence->end_handle();
  
  return MB_SUCCESS;
}
 
ErrorCode RangeSeqIntersectIter::find_invalid_range()
{
  assert(!mSequence);

    // no more entities in current range
  if (mStartHandle == mEndHandle)
    return MB_ENTITY_NOT_FOUND;
    
    // Find the next EntitySequence
  EntityType type = TYPE_FROM_HANDLE(mStartHandle);
  const TypeSequenceManager& map = mSequenceManager->entity_map( type );
  TypeSequenceManager::const_iterator iter = map.upper_bound( mStartHandle );
    // If no next sequence of the same type
  if (iter == map.end()) {
      // If end type not the same as start type, split on type
    if (type != TYPE_FROM_HANDLE( mEndHandle )) {
      int junk;
      mEndHandle = CREATE_HANDLE( type, MB_END_ID, junk );
    }
  }
    // otherwise invalid range ends at min(mEndHandle, sequence start handle - 1)
  else if ((*iter)->start_handle() <= mEndHandle) {
    mEndHandle = (*iter)->start_handle()-1;
  }
  
  return MB_ENTITY_NOT_FOUND;
}
        
#if MB_RANGE_SEQ_INTERSECT_ITER_STATS
double RangeSeqIntersectIter::doubleNumCalls = 0;
double RangeSeqIntersectIter::doubleEntCount = 0;
unsigned long RangeSeqIntersectIter::intNumCalls = 0;
unsigned long RangeSeqIntersectIter::intEntCount = 0;

void RangeSeqIntersectIter::update_stats( unsigned long num_ents )
{
  if (std::numeric_limits<unsigned long>::max() == intNumCalls) {
    doubleNumCalls += intNumCalls;
    intNumCalls = 0;
  }
  ++intNumCalls;
  
  if (std::numeric_limits<unsigned long>::max() - intEntCount > num_ents) {
    doubleNumCalls += intEntCount;
    intEntCount = num_ents;
  }
  else {
    intEntCount += num_ents;
  }
}
#endif
  
} // namespace moab

