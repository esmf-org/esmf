/*
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

/**\file RangeSeqIntersectIter.hpp
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2006-08-11
 *\date 2007-11-06
 */

#ifndef MB_RANGE_SEQ_INTERSECT_ITER_HPP
#define MB_RANGE_SEQ_INTERSECT_ITER_HPP

#include "moab/Types.hpp"
#include "moab/Range.hpp"

namespace moab {

class SequenceManager;
class EntitySequence;

#define MB_RANGE_SEQ_INTERSECT_ITER_STATS 0

/** \brief Iterate over the blocks of EntityHandles in an Range that
 *         are in the same EntitySequence.
 *
 * Iterate over an Range, returning blocks of entities that are the 
 * largest ranges of contiguous handles that meet one of the following
 * conditions:
 *  - All are valid handles belonging to the same EntitySequence
 *  - All are invalid handles of the same EntityType
 *
 * The return type from init() or step() indicates whether or not the
 * current range contains valid entities.  If the handles are either
 * all valid or all holes in an EntitySequence, that sequence can
 * be obtained with get_sequence().  get_sequence() will return NULL if
 * there is no corresponding EntitySequence for the block of handles.
 *
 * This class keeps a data related to the 'current' EntitySequence and
 * references to the Range pasesd to init().  Changing either of these
 * while an instance of this class is in use would be bad.
 */
class RangeSeqIntersectIter {
public:
  
  RangeSeqIntersectIter( SequenceManager* sequences )
    : mSequenceManager( sequences ),
      mSequence( 0 ),
      mStartHandle( 0 ),
      mEndHandle( 0 ),
      mLastHandle( 0 )
    { }
    
    /** Initialize iterator to first valid subset 
     *\return - MB_SUCCESS : initial position of iterator is valid
     *        - MB_ENITITY_NOT_FOUND : range contains invalid handle -- can step past by calling again
     *        - MB_FAILURE : No entities (start == end)
     */
  ErrorCode init( Range::const_iterator start, Range::const_iterator end );
  
    /** Step iterator to next range.  
     *\return - MB_SUCCESS : there is another range, and iter has been changed to it
     *        - MB_ENITITY_NOT_FOUND : range contains invalid handle -- can step past by calling again
     *        - MB_FAILURE : at end.
     */
  ErrorCode step();
  
    /**\brief Check if next call to step() will return MB_FAILURE.
     *
     * Check if the iterator cannot be advanced any further.
     * If this method returns true, then the *next* call to step()
     * will return MB_FAILURE.
     */
  bool is_at_end() const
    { return mEndHandle == mLastHandle; }
  
    /** Get the EntitySequence for the current block.
     *  May be NULL for invaild handles.
     */
  EntitySequence* get_sequence() const
    { return mSequence; }
  
    /** Get first handle in block */
  EntityHandle get_start_handle() const
    { return mStartHandle; }
  
    /** Get last handle in block */
  EntityHandle get_end_handle() const
    { return mEndHandle; }

#if MB_RANGE_SEQ_INTERSECT_ITER_STATS
  static double fragmentation() 
    { return (doubleNumCalls + intNumCalls) / (doubleEntCount + intEntCount); }
#endif
    
private:

  /** Update entity sequence data (mSequence and freeIndex) for current
   *  mStartHandle.  If mEndHandle is past end of sequence, trim it.
   *  Called by step() and init().  step() handles iterating over the pairs 
   *  in the Range.  This method handles iterating over the set of 
   *  EntitySequences that intersect the pair.
   */
  ErrorCode update_entity_sequence();
  
  /** Handle error case where we encountered an EntityHandle w/out
   *  a corresponding EntitySequence.  Trim mEndHandle such that it
   *  is before the next valid EntityHandle of the same type.
   *\return Always returns MB_ENTITY_NOT_FOUND
   */
  ErrorCode find_invalid_range();

  SequenceManager* mSequenceManager;       //!< THE EntitySequenceManager
  EntitySequence* mSequence;               //!< EntitySequence corresponding to current location
  Range::const_pair_iterator rangeIter;  //!< Current position in Range.
  EntityHandle mStartHandle, mEndHandle; //!< Subset of current EntitySequence
  EntityHandle mLastHandle;              //!< The last of the list of all handles in the Range

#if MB_RANGE_SEQ_INTERSECT_ITER_STATS
  static double doubleNumCalls, doubleEntCount;
  static unsigned long intNumCalls, intEntCount;
  static void update_stats( unsigned long num_ents);
#endif
};
  
} // namespace moab

#endif


