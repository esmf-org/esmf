#ifndef TYPE_SEQUENCE_MANAGER_HPP
#define TYPE_SEQUENCE_MANAGER_HPP

#include "EntitySequence.hpp"
#include "moab/Range.hpp"

#include <set>
#include <vector>

namespace moab {

class Error;

/**\brief Maintain data structures organizing EntitySequence instances
 *
 * EntitySequenceManager is a composition of instances of TypeSequenceManager,
 * one instance for each EntityType.  The TypeSequenceManager provides
 * organization, owership, and querying of EntitySequences for a specific
 * EntityType.
 */
class TypeSequenceManager
{
public:
  /**\brief Comparision function used in std::set
   *
   * Define less-than comparison for EntitySequence pointers as a comparison
   * of the entity handles in the pointed-to EntitySequences.
   */
  template<class T>
  class SequenceCompare {
    public: inline bool operator()( const T* a, const T* b ) const
      { return a->end_handle() < b->start_handle(); }
  };
  /**\brief Dummy EntitySequence for use in querying set container */
  class DummySequence : public EntitySequence {
    public:
      DummySequence( EntityHandle start )
        : EntitySequence( start ) {}
  
      EntitySequence* split( EntityHandle ) 
        { return 0; }
      SequenceData* create_data_subset( EntityHandle, EntityHandle ) const
        { return 0; }
      
      void get_const_memory_use( unsigned long& a, unsigned long& b) const
        { a = b = 0; }
      unsigned long get_per_entity_memory_use( EntityHandle, EntityHandle) const
        { return 0; }
  };

  /**\brief Type of container for organizing EntitySequence instances */
  typedef std::set<EntitySequence*,SequenceCompare<EntitySequence> > set_type;
  /**\brief Iterator for set_type */
  typedef set_type::iterator iterator;
  /**\brief Iterator for set_type */
  typedef set_type::const_iterator const_iterator;
  /**\brief Type of container for organizing SequenceData instaces */
  typedef std::set<SequenceData*,SequenceCompare<SequenceData> > data_set_type;
  /**\brief iterator type for data_set_type */
  typedef data_set_type::iterator data_iterator;

  struct SequenceDataPtr {
    private:
    friend class TypeSequenceManager;
    TypeSequenceManager::iterator firstSequence;
  };
private:
  mutable EntitySequence* lastReferenced;//!< Last accessed EntitySequence - Null only if no sequences
  set_type sequenceSet;          //!< Set of all managed EntitySequence instances
  data_set_type availableList;   //!< SequenceData containing unused entries

  iterator erase( iterator i );  //!< Remove a sequence
  
  iterator split_sequence( iterator i, EntityHandle h ); //!< split a sequence

  void append_memory_use( EntityHandle first,
                          EntityHandle last,
                          const SequenceData* data,
                          unsigned long long& entity_storage,
                          unsigned long long& total_storage ) const;

    // check if sequence at passed iterator should be merged with
    // the subsequent sequence, and if so merge them retaining i.
  ErrorCode check_merge_next( iterator i );
    // check if sequence at passed iterator should be merged with
    // the previous sequence, and if so merge them retaining i.
  ErrorCode check_merge_prev( iterator i );
    // common code for check_merge_next and check_merge_prev
  ErrorCode merge_internal( iterator keep, iterator dead );

#ifndef NDEBUG
  bool check_valid_data( const EntitySequence* seq ) const;
#endif

public:

  /**\brief Add an entity sequence
   *
   * Take ownership of passed EntitySequence, and update relevant
   * data structures.  Sequence may not overlap with any existing
   * sequence.  
   *
   * NOTE:  Sequence may be merged with other, existing sequences.
   *        This function will always ensure that the passed 
   *        EntitySequence* is the remaining one, but the passed
   *        sequence may have modified start and end handles.
   */
  ErrorCode insert_sequence( EntitySequence* seq_ptr );
  
  /**\brief Remove an entity sequence.
   *
   * Give up ownership of specified EntitySequence, and remove it
   * from all internal data structures.  Passes back bool flag to
   * notify caller that ownership of the correspoding SequenceData
   * is also relinquished because the specified EntitySequence is
   * the last one referencing it.  
   */
  ErrorCode remove_sequence( const EntitySequence* seq_ptr,
                               bool& is_last_user_of_sequence_data );
                               
                               
  /**\brief Replace sequence or subset of sequence
   *
   * Replace one sequence or a subset of one sequence with 
   * another sequence.  With fail if a) the existing
   * sequence is not a subset of an existing sequence or
   * b) existing sequence shares a SequenceData with the 
   * passed sequence.
   *
   * This method is provided for use when changing the
   * number of nodes in elements. 
   */
  ErrorCode replace_subsequence( EntitySequence* seq_ptr,
                                 const int* tag_sizes,
                                 int num_tag_sizes );
  
  TypeSequenceManager() : lastReferenced(0) {}
  
  ~TypeSequenceManager();

    /**\brief Start of EntitySequence set */
  const_iterator begin() const { return sequenceSet.begin(); }
  iterator begin() { return sequenceSet.begin(); }

    /**\brief End of EntitySequence set */
  const_iterator   end() const { return sequenceSet.end();   }
  iterator   end() { return sequenceSet.end();   }

    /**\brief Return EntitySequence for specified handle.
     *
     *  Return EntitySequence for specified handle, or if
     *  no such sequence, the next one.  Returns end() if
     *  all sequences have ranges less than specified handle.
     */
  const_iterator lower_bound( EntityHandle h ) const 
    { 
      DummySequence f(h);
      return sequenceSet.lower_bound( &f ); 
    }
  iterator lower_bound( EntityHandle h )
    { 
      DummySequence f(h);
      return sequenceSet.lower_bound( &f ); 
    }

    /**\brief Return EntitySequence after specified handle.
     *
     *  Return EntitySequence with smallest start handle 
     *  that is greater than input handle.  Returns end() if
     *  all sequences have start handles less than specified 
     *  handle.
     */
  const_iterator upper_bound( EntityHandle h ) const 
    { 
      DummySequence f(h);
      return sequenceSet.upper_bound( &f ); 
    }

    /**\brief Get EntitySequence for handle. 
     *\return EntitySequence for handle, or NULL if no such sequence.
     */
  inline EntitySequence* find( EntityHandle h ) const;
  inline EntitySequence* find( EntityHandle h );
  inline ErrorCode find( EntityHandle h, EntitySequence*& );
  inline ErrorCode find( EntityHandle h, const EntitySequence*& ) const;
  inline const EntitySequence* get_last_accessed() const;
  
    /**\brief Get handles for all entities in all sequences. */
  inline void get_entities( Range& entities_out ) const;
  
    /**\brief Get handles for all entities in all sequences. */
  inline void get_entities( std::vector<EntityHandle>& entities_out ) const;
  
    /**\brief Get number of entities represented by all sequences. */
  inline EntityID get_number_entities() const;
  
  ErrorCode check_valid_handles( Error* error_handler, 
                                 EntityHandle first, 
                                 EntityHandle last ) const;
  
    /**\brief Remove entities 
     *
     * Update EntitySequence data as necessary to "delete" the
     * specified entities (e.g. split sequences, delete sequences,
     * free SequenceData instances, etc.)
     */
  ErrorCode erase( Error* error_handler, EntityHandle first, EntityHandle last );
  ErrorCode erase( Error* error_handler, EntityHandle entity );
  
  /**\brief Test if this instance contains no sequences */
  bool empty() const
    { return 0 == lastReferenced; }
  
  /**\brief Allocate a handle in an existing entity sequence
   *
   * Find an existing entity sequence to which a new handle can
   * be prepended or appended.  The 'append_out' flag indicates
   * to the caller that the new handle should be appended to the
   * returned sequence if true, and prepended if false.  
   *
   * If no appropriate EntitySequence is available, NULL will
   * be returned.  The caller will typically then want to use
   * find_free_sequence() to find appropriate values for the
   * creation of a new EntitySequence.
   */
  iterator find_free_handle( EntityHandle min_start_handle,
                             EntityHandle max_end_handle,
                             bool& append_out,
                             int values_per_ent = 0 );
  
    /**\brief Find block of free handles
     *
     * Find block of free handles, such that block does not
     * overlap any existing EntitySequence. 
     *\return First handle of block, or zero if no block found.
     */
  EntityHandle find_free_block( EntityID num_entities, 
                                  EntityHandle min_start_handle,
                                  EntityHandle max_end_handle );
  
    /**\brief Find block of free handles
     *
     * Find block of free handles, such that block a) does not
     * overlap any existing EntitySequence and b) is either 
     * entirely within one existing SequenceData or does not 
     * overlap any SequenceData.
     *\param num_entities      Size of handle block.
     *\param min_start_handle  Block may not contain any handle less than this.
     *\param max_end_handle    Block may not contain any handle greater than this.
     *\param sequence_data_out If block is within an unused portion of an 
     *                         existing SequenceData, a pointer to that 
     *                         SequenceData.  NULL otherwise.
     *\return values_per_ent   Matched against EntitySequence::values_per_entity.
     *                         An existing SequenceData will not be returned if
     *                         the existing EntitySequences using have a different
     *                         value than the passed one.
     */
  EntityHandle find_free_sequence( EntityID num_entities, 
                                     EntityHandle min_start_handle,
                                     EntityHandle max_end_handle,
                                     SequenceData*& sequence_data_out,
                                     EntityID &sequence_data_size,
                                     int values_per_ent = 0 );

    /**\brief Check if block of handles is free.
     *
     * Check if block of handles is free and can be allocated
     * as a single EntitySequence.  If the block of handles
     * is contained within an unused portion of a SequenceData,
     * the SequenceData is returned.
     */
  bool is_free_sequence( EntityHandle start_handle, 
                         EntityID num_entities,
                         SequenceData*& sequence_data_out,
                         int values_per_ent = 0 );
  
    /**\brief Check if specific handle is free for allocation
     *
     * Check if a specific handle is not currently allocated
     * and can be allocated with the passed value of values_per_ent.
     * For example, the handle may not be allocated, but it may
     * fall within an existing SequenceData.  In that case, it
     * must be possible to store the speciified values_per_ent
     * in the existing SequenceData.
     *
     * There are four possible return 'states' from this function:
     *
     * - handle is not available or cannot be allocated with specified
     *   values_per_ent.  Returned error code is MB_ALREADY_ALLOCATED.
     *
     * - handle can be appended or prepended to an existing sequence.
     *   seq_ptr_out is set to the sequence the handle should be added to.
     *
     * - handle cannot be appended to an existing sequence but falls
     *   within an existing SequenceData. The caller is expected
     *   to create a new sequence referencing that SequenceData. seq_ptr_out
     *   is NULL and data_ptr_out is set to existing SequenceData.
     *
     * - handle does not correspond to any existing sequence or data.
     *   The caller is expected to create a new sequence and SequenceData.
     *   Both seq_ptr_out and data_ptr_out are set to NULL.  
     *   block_start and block_end are set to start and end handles of the
     *   largest sequence that can be allocated and contain the input handle.
     *
     *\param handle The handle the caller wishes to allocate as a new entity
     *\param seq_ptr_out Output: pointer to sequence to append or prepend to
     *              to allocate handle.  end() if no such sequence.
     *\param data_ptr_out Output: Pointer to existing SequenceData containing input
     *              handle, or NULL if no such SequenceData.
     *\param block_start Output: Smallest possible start handle for new sequence.
     *\param block_end   Output: Largest possible end handle for new sequence.
     */
  ErrorCode is_free_handle( EntityHandle handle,
                              iterator& seq_ptr_out,
                              SequenceData*& data_ptr_out,
                              EntityHandle& block_start,
                              EntityHandle& block_end,
                              int values_per_ent = 0 );  

  EntityHandle last_free_handle( EntityHandle after_this ) const;

    /**\brief Notify that sequence was prepended to
     *
     * Notify of sequence modifications so we can check if
     * sequence needs to be merged.
     */
  ErrorCode notify_prepended( iterator seq );
  
    /**\brief Notify that sequence was appended to
     *
     * Notify of sequence modifications so we can check if
     * sequence needs to be merged.
     */
  ErrorCode notify_appended( iterator seq );
    
  void get_memory_use( unsigned long long& total_entity_storage,
                       unsigned long long& total_storage ) const;
  
  void get_memory_use( EntityHandle start, EntityHandle end,
                       unsigned long long& total_entity_storage,
                       unsigned long long& total_amortized_storage ) const;
                       
  unsigned long get_sequence_count() const
    { return sequenceSet.size(); }
    
    /**\brief Get used size of SequenceData
     *
     * Get the sum of the size of all EntitySequences referencing
     * a SequenceData.  Used for memory use calculations.
     */
  EntityID get_occupied_size( const SequenceData* ) const;
};

inline EntitySequence* TypeSequenceManager::find( EntityHandle h ) const
{
  if (!lastReferenced) // only null if empty
    return 0;
  else if (h >= lastReferenced->start_handle() && h <= lastReferenced->end_handle())
    return lastReferenced;
  else {
    DummySequence seq(h);
    const_iterator i = sequenceSet.find( &seq );
    return i == end() ? 0 : (lastReferenced = *i);
  }
}   
inline EntitySequence* TypeSequenceManager::find( EntityHandle h )
{
  if (!lastReferenced) // only null if empty
    return 0;
  else if (h >= lastReferenced->start_handle() && h <= lastReferenced->end_handle())
    return lastReferenced;
  else {
    DummySequence seq(h);
    iterator i = sequenceSet.find( &seq );
    return i == end() ? 0 : (lastReferenced = *i);
  }
}   

inline ErrorCode TypeSequenceManager::find( EntityHandle h, EntitySequence*& seq )
{
  if (!lastReferenced) { // only null if empty
    seq = 0;
    return MB_ENTITY_NOT_FOUND;
  }
  else if (h >= lastReferenced->start_handle() && h <= lastReferenced->end_handle()) {
    seq = lastReferenced;
    return MB_SUCCESS;
  }
  else {
    DummySequence ds(h);
    iterator i = sequenceSet.lower_bound( &ds );
    if (i == end() || (*i)->start_handle() > h ) {
      seq = 0;
      return MB_ENTITY_NOT_FOUND;
    }
    else {
      seq = lastReferenced = *i;
      return MB_SUCCESS;
    }
  }
}   

inline ErrorCode TypeSequenceManager::find( EntityHandle h, const EntitySequence*& seq ) const
{
  if (!lastReferenced) { // only null if empty
    seq = 0;
    return MB_ENTITY_NOT_FOUND;
  }
  else if (h >= lastReferenced->start_handle() && h <= lastReferenced->end_handle()) {
    seq = lastReferenced;
    return MB_SUCCESS;
  }
  else {
    DummySequence ds(h);
    const_iterator i = sequenceSet.lower_bound( &ds );
    if (i == end() || (*i)->start_handle() > h ) {
      seq = 0;
      return MB_ENTITY_NOT_FOUND;
    }
    else {
      seq = lastReferenced = *i;
      return MB_SUCCESS;
    }
  }
}   

inline const EntitySequence* TypeSequenceManager::get_last_accessed() const
  { return lastReferenced; /* only NULL if TypeSequenceManager is empty */ }

inline void TypeSequenceManager::get_entities( Range& entities_out ) const
{
  Range::iterator in = entities_out.begin();
  for (const_iterator i = begin(); i != end(); ++i)
    in = entities_out.insert( in, (*i)->start_handle(), (*i)->end_handle() );
}

inline void TypeSequenceManager::get_entities( std::vector<EntityHandle>& entities_out ) const
{
  for (const_iterator i = begin(); i != end(); ++i)
    for (EntityHandle j = (*i)->start_handle(); j <= (*i)->end_handle(); ++j)
      entities_out.push_back( j );
}

inline EntityID TypeSequenceManager::get_number_entities() const
{
  EntityID count = 0;
  for (const_iterator i = begin(); i != end(); ++i)
    count += (*i)->size();
  return count;
}

} // namespace moab

#endif
