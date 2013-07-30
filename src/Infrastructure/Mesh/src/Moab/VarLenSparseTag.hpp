/**
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


#ifndef VAR_LEN_SPARSE_TAG_HPP
#define VAR_LEN_SPARSE_TAG_HPP

#ifndef IS_BUILDING_MB
#error "VarLenSparseTag.hpp isn't supposed to be included into an application"
#endif

#ifdef WIN32
#pragma warning(disable : 4786)
#endif


#define STRINGIFY_(X) #X
#define STRINGIFY(X) STRINGIFY_(X)
#ifdef HAVE_UNORDERED_MAP
# include STRINGIFY(HAVE_UNORDERED_MAP)
#else
# include <map>
#endif
#include <vector>

#include "TagInfo.hpp"
#include "VarLenTag.hpp"
#include <stdlib.h>

namespace moab {

//! Sparse tag variable-length data
class VarLenSparseTag : public TagInfo
{
public:
  
  VarLenSparseTag( const char* name,
                   DataType type,
                   const void* default_value,
                   int default_value_bytes );
  
  ~VarLenSparseTag();

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

  //! get number of entities
  unsigned long get_number_entities()
    { return mData.size(); }


  //! map of entity id and tag data
#ifdef HAVE_UNORDERED_MAP
  typedef UNORDERED_MAP_NS::unordered_map<EntityHandle,VarLenTag> MapType;
#else
  typedef std::map<EntityHandle,VarLenTag> MapType;
#endif

private:
  
  VarLenSparseTag( const VarLenSparseTag& );
  VarLenSparseTag& operator=( const VarLenSparseTag& );

  //! get the variable-length data for an entity id
  inline
  ErrorCode get_data_ptr( Error* error_handler,
                          EntityHandle entity_handle, 
                          const void*& data,
                          int& size) const;

  MapType mData;
};

} // namespace moab

#endif // VAR_LEN_SPARSE_TAG_HPP




