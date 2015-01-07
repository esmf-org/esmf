#ifndef BIT_TAG_HPP
#define BIT_TAG_HPP

#include "TagInfo.hpp"
#include "Internals.hpp"
#include <algorithm>
#include <vector>
#include <assert.h>

namespace moab {

class BitPage;

/**\brief data for a single bit tag */
class BitTag : public TagInfo
{
  private:
  
  BitTag( const char* name,
          int size,
          const void* default_value )
    : TagInfo( name, size, MB_TYPE_BIT, default_value, default_value ? 1 : 0 )
    {}
  
  public: 
  
  static BitTag* create_tag( const char* name,
                             int size,
                             const void* default_value = 0 );
  
  virtual ~BitTag();

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
  ErrorCode get_memory_use( const SequenceManager* seqman,
                            unsigned long& total,
                            unsigned long& per_entity ) const;

    /**\brief Get entities for which an explicit tag of the specified value is stored */
  ErrorCode get_entities_with_bits( EntityType type, 
                                      Range& entities,
                                      unsigned char bits ) const;

    /**\brief Get entities for which an explicit tag of the specified value is stored */
  ErrorCode get_entities_with_bits( const Range &range,
                                      EntityType type, 
                                      Range& entities,
                                      unsigned char bits ) const;
                              
  enum { Ln2PageSize = 12,              //!< Constant: log2(PageSize)
         PageSize = (1u << Ln2PageSize) //!< Constant: Bytes per BitPage (power of 2)
      };

  private:
  
  BitTag( const BitTag& );
  BitTag& operator=( const BitTag& );
  ErrorCode reserve( unsigned bits );
  
  inline unsigned char default_val() const {
    if (get_default_value())
      return *reinterpret_cast<const unsigned char*>(get_default_value());
    else
      return 0;
  }
  
  std::vector<BitPage*> pageList[MBMAXTYPE]; //!< Array of BitPage instances storing actual data.
  unsigned int requestedBitsPerEntity; //!< user-requested bits per entity
  unsigned int storedBitsPerEntity;    //!< allocated bits per entity (power of 2)
  unsigned int pageShift;              //!< log2( ents_per_page() )
  
  /**\brief Get indices from handle 
   *
   *\param type   Output: entity type
   *\param page   Output: index into pageList[type]
   *\param offset Output: index into pageList[type][page]
   */
  void unpack( EntityHandle h, EntityType& type, size_t& page, int& offset ) const
    { 
      type = TYPE_FROM_HANDLE(h);
      h = ID_FROM_HANDLE(h);
      page = ((size_t)h) >> pageShift;   // h / ents_per_page()
      offset = h & ((1u<<pageShift)-1u); // h % ends_per_page()
    }
    
  /**\brief Get the number of tag values that are stored in each BitPage */
  int ents_per_page() const { return 8*PageSize/storedBitsPerEntity; }
  
  template <class Container> inline 
  void get_tagged( EntityType type,
                   Container& entities ) const;
  template <class Container> inline 
  void get_tagged( Range::const_iterator begin,
                   Range::const_iterator end,
                   Container& entities ) const;
  template <class Container> inline 
  void get_tagged( Container& entities,
                   EntityType type,
                   const Range* intersect ) const;
};
  
} // namespace moab

#endif
