/** \file   DenseTag.hpp
 *  \author Jason Kraftcheck 
 *  \date   2010-12-14
 */

#ifndef DENSE_TAG_HPP
#define DENSE_TAG_HPP

#include "TagInfo.hpp"

namespace moab {

class EntitySequence;
    
/**\brief Dense storage of fixed-length tag data
 *
 * Implement normal dense tag.
 */
class DenseTag : public TagInfo 
{
private:
  int mySequenceArray; //!< Array index in SequenceManager used to store tag data.
                        
  unsigned char* meshValue;

  DenseTag( int array_index,
            const char * name, 
            int size, 
            DataType type, 
            const void * default_value );

public:
  static 
  DenseTag* create_tag( SequenceManager* seqman,
                        Error* error,
                        const char* name,
                        int bytes,
                        DataType type,
                        const void* default_value );
  
  virtual ~DenseTag();

  virtual TagType get_storage_type() const;

  /**\brief Remove/clear tag data for all entities
   *
   * Remove tag values from entities.
   *
   *\param delete_pending  If true, then release any global
   *          data associated with the tag in preparation for deleting
   *          the tag itself.
   *
   *\Note Invalidates tag if \c tag_delete_pending is true.  The only 
   *        valid method that can be invoked that is is the destructor.
   *
   *\param seqman    Pointer to mesh entity database
   */
  virtual ErrorCode release_all_data( SequenceManager* seqman, 
                                      Error* error_handler, 
                                      bool delete_pending );
  

  /**\brief Get tag value for passed entities
   * 
   * Get tag values for specified entities.
   *
   *\Note Will fail for variable-length data.
   *\param seqman Pointer to mesh entity database
   *\param entities Entity handles for which to retrieve tag data
   *\param num_entities Length of \c entities array
   *\param data Pointer to memory in which to store consecutive tag values,
   *            one for each passed entity.
   */
  virtual
  ErrorCode get_data( const SequenceManager* seqman,
                      Error* error_handler, 
                      const EntityHandle* entities,
                      size_t num_entities,
                      void* data ) const;
  
  /**\brief Get tag value for passed entities
   * 
   * Get tag values for specified entities.
   *
   *\Note Will fail for variable-length data.
   *\param seqman Pointer to mesh entity database
   *\param entities Entity handles for which to retrieve tag data
   *\param data Pointer to memory in which to store consecutive tag values,
   *            one for each passed entity.
   */
  virtual
  ErrorCode get_data( const SequenceManager* seqman,
                      Error* error_handler, 
                      const Range& entities,
                      void* data ) const;
                      
  /**\brief Get tag value for passed entities
   * 
   * Get tag values for specified entities.
   *
   *\param seqman    Pointer to mesh entity database
   *\param entities  Entity handles for which to retrieve tag data
   *\param num_entities Length of \c entities array
   *\param data_ptrs Array of pointers to tag values, one pointer
   *                 for each passed entity.
   *\param data_lengths One value for each entity specifying the
   *                length of the tag value for the corresponding
   *                entity. 
   */
  virtual
  ErrorCode get_data( const SequenceManager* seqman,
                      Error* error_handler, 
                      const EntityHandle* entities,
                      size_t num_entities,
                      const void** data_ptrs,
                      int* data_lengths ) const ;
                      
                      
  /**\brief Get tag value for passed entities
   * 
   * Get tag values for specified entities.
   *
   *\param seqman    Pointer to mesh entity database
   *\param entities  Entity handles for which to retrieve tag data
   *\param data_ptrs Array of pointers to tag values, one pointer
   *                 for each passed entity.
   *\param data_lengths One value for each entity specifying the
   *                length of the tag value for the corresponding
   *                entity. 
   */
  virtual
  ErrorCode get_data( const SequenceManager* seqman,
                      Error* error_handler, 
                      const Range& entities,
                      const void** data_ptrs,
                      int* data_lengths ) const;
  
  /**\brief Set tag value for passed entities
   * 
   * Store tag data or update stored tag values
   *\Note Will fail for variable-length data.
   *\param seqman Pointer to mesh entity database
   *\param entities Entity handles for which to store tag data
   *\param num_entities Length of \c entities array
   *\param data Pointer to memory holding consecutive tag values,
   *            one for each passed entity.
   */
  virtual
  ErrorCode set_data( SequenceManager* seqman,
                      Error* error_handler, 
                      const EntityHandle* entities,
                      size_t num_entities,
                      const void* data );
  
  /**\brief Set tag value for passed entities
   * 
   * Store tag data or update stored tag values
   *\Note Will fail for variable-length data.
   *\param seqman Pointer to mesh entity database
   *\param entities Entity handles for which to store tag data
   *\param data Pointer to memory holding consecutive tag values,
   *            one for each passed entity.
   */
  virtual
  ErrorCode set_data( SequenceManager* seqman,
                      Error* error_handler, 
                      const Range& entities,
                      const void* data );
                      
  /**\brief Set tag value for passed entities
   * 
   * Store tag data or update stored tag values
   *
   *\param seqman    Pointer to mesh entity database
   *\param entities  Entity handles for which to store tag data
   *\param num_entities Length of \c entities array
   *\param data_ptrs Array of pointers to tag values, one pointer
   *                 for each passed entity.
   *\param data_lengths One value for each entity specifying the
   *                length of the tag value for the corresponding
   *                entity.  Array is required for variable-length
   *                tags and is ignored for fixed-length tags.
   */
  virtual
  ErrorCode set_data( SequenceManager* seqman,
                      Error* error_handler, 
                      const EntityHandle* entities,
                      size_t num_entities,
                      void const* const* data_ptrs,
                      const int* data_lengths );
                      
                      
  /**\brief Set tag value for passed entities
   * 
   * Store tag data or update stored tag values
   *
   *\param seqman    Pointer to mesh entity database
   *\param entities  Entity handles for which to store tag data
   *\param data_ptrs Array of pointers to tag values, one pointer
   *                 for each passed entity.
   *\param data_lengths One value for each entity specifying the
   *                length of the tag value for the corresponding
   *                entity.  Array is required for variable-length
   *                tags and is ignored for fixed-length tags.
   */
  virtual
  ErrorCode set_data( SequenceManager* seqman,
                      Error* error_handler, 
                      const Range& entities,
                      void const* const* data_ptrs,
                      const int* data_lengths );
                      
  /**\brief Set tag value for passed entities
   * 
   * Store tag data or update stored tag values.
   *
   *\param seqman    Pointer to mesh entity database
   *\param entities  Entity handles for which to store tag data
   *\param num_entities Length of \c entities array
   *\param value_ptr Pointer to a single tag value which is to be
   *                 stored for each of the passed entities.
   *\param value_len Length of tag value in bytes.  Ignored for
   *                 fixed-length tags.  Required for variable-
   *                 length tags.
   */
  virtual
  ErrorCode clear_data( SequenceManager* seqman,
                        Error* error_handler, 
                        const EntityHandle* entities,
                        size_t num_entities,
                        const void* value_ptr,
                        int value_len = 0 );
                      
  /**\brief Set tag value for passed entities
   * 
   * Store tag data or update stored tag values.
   *
   *\param seqman    Pointer to mesh entity database
   *\param entities  Entity handles for which to store tag data
   *\param value_ptr Pointer to a single tag value which is to be
   *                 stored for each of the passed entities.
   *\param value_len Length of tag value in bytes.  Ignored for
   *                 fixed-length tags.  Required for variable-
   *                 length tags.
   */
  virtual
  ErrorCode clear_data( SequenceManager* seqman,
                        Error* error_handler, 
                        const Range& entities,
                        const void* value_ptr,
                        int value_len = 0 );

  /**\brief Remove/clear tag data for entities
   *
   * Remove tag values from entities.
   *
   *\param seqman    Pointer to mesh entity database
   *\param entities  Entity handles for which to store tag data
   *\param num_entities Length of \c entities array
   */
  virtual ErrorCode remove_data( SequenceManager* seqman,
                                 Error* error_handler, 
                                 const EntityHandle* entities,
                                 size_t num_entities );

  /**\brief Remove/clear tag data for entities
   *
   * Remove tag values from entities.
   *
   *\param seqman    Pointer to mesh entity database
   *\param entities  Entity handles for which to store tag data
   */
  virtual ErrorCode remove_data( SequenceManager* seqman,
                                 Error* error_handler, 
                                 const Range& entities );

  /**\brief Access tag data via direct pointer into contiguous blocks
   *
   * Iteratively obtain direct access to contiguous blocks of tag
   * storage.  This function cannot be used with bit tags because
   * of the compressed bit storage.  This function cannot be used
   * with variable length tags because it does not provide a mechanism
   * to determine the length of the value for each entity.  This
   * function may be used with sparse tags, but if it is used, it
   * will return data for a single entity at a time.  
   *
   *\param iter        As input, the first entity for which to return
   *                   data.  As output, one past the last entity for
   *                   which data was returned.
   *\param end         One past the last entity for which data is desired
   *\param data_ptr    Output: pointer to tag storage.
   *  
   *\Note If this function is called for entities for which no tag value
   *      has been set, but for which a default value exists, it will 
   *      force the allocation of explicit storage for each such entity
   *      even though MOAB would normally not explicitly store tag values
   *      for such entities.
   */
  virtual
  ErrorCode tag_iterate( SequenceManager* seqman,
                         Error* error_handler, 
                         Range::iterator& iter,
                         const Range::iterator& end,
                         void*& data_ptr,
                         bool allocate = true);

  /**\brief Get all tagged entities
   *
   * Get the list of entities for which the a tag value has been set,
   * or a close approximation if the tag storage scheme cannot 
   * accurately determine exactly which entities have explicit values.
   *
   *\param seqman   Pointer to entity storage database
   *\param output_entities Results *appended* to this range
   *\param type     Optional entity type.  If specified, search is
   *                limited to entities of specified type.
   *\param intersect Opotional intersect list.  If specified,
   *                search is restricted to entities in this list.
   */
  virtual
  ErrorCode get_tagged_entities( const SequenceManager* seqman,
                                 Range& output_entities,
                                 EntityType type = MBMAXTYPE,
                                 const Range* intersect = 0 ) const;

  /**\brief Count all tagged entities
   *
   * Count the entities for which the a tag value has been set,
   * or a close approximation if the tag storage scheme cannot 
   * accurately determine exactly which entities have explicit values.
   *
   *\param seqman   Pointer to entity storage database
   *\param output_count This is *incremented* for each detected entity.
   *\param type     Optional entity type.  If specified, search is
   *                limited to entities of specified type.
   *\param intersect Opotional intersect list.  If specified,
   *                search is restricted to entities in this list.
   */
  virtual
  ErrorCode num_tagged_entities( const SequenceManager* seqman,
                                 size_t& output_count,
                                 EntityType type = MBMAXTYPE,
                                 const Range* intersect = 0 ) const;
  
  /**\brief Get all tagged entities with tag value
   *
   * Get the list of entities which have the specified tag value.
   *
   *\param seqman   Pointer to entity storage database
   *\param output_entities Results *appended* to this range
   *\param value    Pointer to tag value
   *\param value_bytes Size of tag value in bytes.
   *\param type     Optional entity type.  If specified, search is
   *                limited to entities of specified type.
   *\param intersect_entities Opotional intersect list.  If specified,
   *                search is restricted to entities in this list.
   */
  virtual
  ErrorCode find_entities_with_value( const SequenceManager* seqman,
                                      Error* error_handler, 
                                      Range& output_entities,
                                      const void* value,
                                      int value_bytes = 0,
                                      EntityType type = MBMAXTYPE,
                                      const Range* intersect_entities = 0 ) const;
  
  /**\brief Check if entity is tagged */
  virtual bool is_tagged( const SequenceManager*, EntityHandle h ) const;
   
  /**\brief Get memory use for tag data.
   *
   */
  virtual
  ErrorCode get_memory_use( const SequenceManager* seqman,
                            unsigned long& total,
                            unsigned long& per_entity ) const;

  /**\brief Get read-only tag storage
   *
   *\param seqman  Sequence manager
   *\param error   Error object through which to report errors
   *\param h       First entity for which to return storage.
   *\param ptr     Pointer to dag storage.  This pointer will be set
   *               to null and \c MB_SUCCESS will be returned if the handle
   *               is valid but no tag storage has been allocated.
   *\param count   Number of consecutive entities for which tag storage
   *               is returned.  This value will be valid even if null
   *               is returned for \c ptr and indicates the number of
   *               consecutive entities for which no tag storage has been
   *               allocated.
   *\return        MB_SUCCESS if handle is valid (regardless of whether
   *               or not any tag storage is allocated). 
   */
  ErrorCode get_array( const SequenceManager* seqman, 
                       Error* error,
                       EntityHandle h, 
                       const unsigned char* const& ptr,
                       size_t& count ) const;
  
  /**\brief Get read-only tag storage for entire sequence
   *
   *\param seq     Sequence 
   *\param ptr     Pointer to dag storage.  This pointer will be set
   *               to null and \c MB_SUCCESS will be returned if no tag storage has been allocated.
   *\return        MB_SUCCESS if sequence is valid (regardless of whether
   *               or not any tag storage is allocated). 
   */
  ErrorCode get_array( const EntitySequence* seq, 
                       const unsigned char* const& ptr) const;
  
  /**\brief Get non-read-only tag storage for entire sequence
   *
   *\param seq     Sequence 
   *\param ptr     Pointer to dag storage.  This pointer will be set
   *               to null and \c MB_SUCCESS will be returned if no tag storage has been allocated.
   *\return        MB_SUCCESS if sequence is valid (regardless of whether
   *               or not any tag storage is allocated). 
   */
  ErrorCode get_array( const EntitySequence* seq, 
                       const unsigned char* & ptr) const;
  
private:
  
  DenseTag( const DenseTag& );
  DenseTag& operator=( const DenseTag& );

  /**\brief Get or allocated tag storage
   *
   *\param h       First entity for which to return storage.
   *\param ptr     Pointer to dag storage.  This pointer will be set
   *               to null and \c MB_SUCCESS will be returned if the handle
   *               is valid but no tag storage has been allocated.
   *\param count   Number of consecutive entities for which tag storage
   *               is returned.  This value will be valid even if null
   *               is returned for \c ptr and indicates the number of
   *               consecutive entities for which no tag storage has been
   *               allocated.
   *\param allocate If true storage will be allocated and initialized
   *               if it has not already been allocated.
   *\return        MB_SUCCESS if handle is valid (regardless of whether
   *               or not any tag storage is allocated). 
   */
  inline
  ErrorCode get_array( SequenceManager* seqman, 
                       Error* error,
                       EntityHandle h, 
                       unsigned char*& ptr,
                       size_t& count,
                       bool allocate );

  /**\brief Get tag storage
   *
   *\param h       First entity for which to return storage.
   *\param ptr     Pointer to dag storage.  This pointer will be set
   *               to null and \c MB_SUCCESS will be returned if the handle
   *               is valid but no tag storage has been allocated.
   *\param count   Number of consecutive entities for which tag storage
   *               is returned.  This value will be valid even if null
   *               is returned for \c ptr and indicates the number of
   *               consecutive entities for which no tag storage has been
   *               allocated.
   *\return        MB_SUCCESS if handle is valid (regardless of whether
   *               or not any tag storage is allocated). 
   */
  inline
  ErrorCode get_array( const SequenceManager* seqman, 
                       Error* error,
                       EntityHandle h, 
                       const unsigned char*& ptr,
                       size_t& count ) const;
                       
  /**\brief Common implementation of public clear_data and remove_data */
  ErrorCode clear_data( bool allocate,
                        SequenceManager* seqman,
                        Error* error,
                        const EntityHandle* entities,
                        size_t num_entities,
                        const void* value_ptr );
  
  /**\brief Common implementation of public clear_data and remove_data */
  ErrorCode clear_data( bool allocate,
                        SequenceManager* seqman,
                        Error* error,
                        const Range& entities,
                        const void* value_ptr );
};



} // namespace moab

#endif // DENSE_TAG_HPP
