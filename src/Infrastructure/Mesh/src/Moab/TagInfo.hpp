#ifndef TAG_INFO_HPP
#define TAG_INFO_HPP

#include "moab/Range.hpp"
#include <string>

namespace moab {

class SequenceManager;
class Range;
class Error;

// ! stores information about a tag
class TagInfo
{
public:

  //! constructor
  TagInfo() : mDefaultValue(NULL),
              mMeshValue(NULL),
              mDefaultValueSize(0),
              mMeshValueSize(0),
              mDataSize(0), 
              dataType(MB_TYPE_OPAQUE)
              {}

  //! constructor that takes all parameters
  TagInfo( const char * name, 
           int size, 
           DataType type, 
           const void * default_value,
           int default_value_size);
  
  virtual ~TagInfo();

  /**\brief Remove/clear tag data for all entities
   *
   * Remove tag values from entities.
   *
   *\param tag_delete_pending  If true, then release any global
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
                                      bool tag_delete_pending ) = 0;
  
  //! set the name of the tag
  void set_name( const std::string& name) { mTagName = name; }

  //! get the name of the tag
  const std::string& get_name() const { return mTagName; }

    //! get length of default value
  int get_default_value_size() const { return mDefaultValueSize; }

    //! get the default data
  const void *get_default_value() const { return mDefaultValue; }
  
    //! compare the passed value to the default value.
    //! returns false if no default value.
  bool equals_default_value( const void* data, int size = -1 ) const;
  
  inline DataType get_data_type() const     { return dataType; }

  //! get the size of the data in bytes
  int get_size() const { return mDataSize; }
  
  //! Check if variable-length tag
  bool variable_length() const { return get_size() == MB_VARIABLE_LENGTH; }

  static int size_from_data_type( DataType t );
  
    // Check that all lengths are valid multiples of the type size.
    // Returns true if all lengths are valid, false otherwise.
  bool check_valid_sizes( const int* sizes, int num_sizes ) const;

    /**\return MB_VARIABLE_LENGTH_DATA If variable_length() && lengths is NULL
     *         MB_INVALID_SIZE         If variable_length() && lengths is not
     *                                 NULL && any size is not a multiple of
     *                                 type size.
     *         MB_INVALID_SIZE         If !variable_length() && lengths is not
     *                                 NULL && any size is not the tag size.
     *         MB_SUCCESS              Otherwise.
     */
  ErrorCode validate_lengths( Error* error_handler,
                              const int* lengths, 
                              size_t num_lengths ) const;

  virtual
  TagType get_storage_type() const = 0;

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
                      void* data ) const = 0;
  
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
                      void* data ) const = 0;
                      
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
                      int* data_lengths ) const = 0;
                      
                      
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
                      int* data_lengths ) const = 0;
  
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
                      const void* data ) = 0;
  
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
                      const void* data ) = 0;
                      
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
                      const int* data_lengths ) = 0;
                      
                      
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
                      const int* data_lengths ) = 0;
                      
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
                        int value_len = 0 ) = 0;
                      
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
                        int value_len = 0 ) = 0;

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
                                 size_t num_entities ) = 0;

  /**\brief Remove/clear tag data for entities
   *
   * Remove tag values from entities.
   *
   *\param seqman    Pointer to mesh entity database
   *\param entities  Entity handles for which to store tag data
   */
  virtual ErrorCode remove_data( SequenceManager* seqman,
                                 Error* error_handler,
                                 const Range& entities ) = 0;

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
   *\param allocate    If true, space for this tag will be allocated, if not it wont
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
                         bool allocate = true) = 0;

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
   *\param intersect Optional intersect list.  If specified,
   *                search is restricted to entities in this list.
   */
  virtual
  ErrorCode get_tagged_entities( const SequenceManager* seqman,
                                 Range& output_entities,
                                 EntityType type = MBMAXTYPE,
                                 const Range* intersect = 0 ) const = 0;

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
   *\param intersect Optional intersect list.  If specified,
   *                search is restricted to entities in this list.
   */
  virtual
  ErrorCode num_tagged_entities( const SequenceManager* seqman,
                                 size_t& output_count,
                                 EntityType type = MBMAXTYPE,
                                 const Range* intersect = 0 ) const = 0;
  
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
   *\param intersect_entities Optional intersect list.  If specified,
   *                search is restricted to entities in this list.
   */
  virtual
  ErrorCode find_entities_with_value( const SequenceManager* seqman,
                                      Error* error_handler,
                                      Range& output_entities,
                                      const void* value,
                                      int value_bytes = 0,
                                      EntityType type = MBMAXTYPE,
                                      const Range* intersect_entities = 0 ) const = 0;
  
  /**\brief Check if entity is tagged */
  virtual
  bool is_tagged( const SequenceManager* seqman, EntityHandle entity ) const = 0;
  
  /**\brief Get memory use for tag data.
   *
   */
  virtual
  ErrorCode get_memory_use( const SequenceManager* seqman,
                            unsigned long& total,
                            unsigned long& per_entity ) const = 0;

protected:    

  unsigned long get_memory_use() const
  {
    return get_default_value_size() + get_name().size();
  }

private:
  
  TagInfo( const TagInfo& copy );
  
  TagInfo& operator=( const TagInfo& copy );

  //! stores the default data, if any
  void* mDefaultValue;
  
  //! store the mesh value, if any
  void* mMeshValue;
  
  //! Size of mDefaultValue and mMeshValue, in bytes
  //! NOTE: These sizes differ from mDataSize in two cases:
  //!    a) Variable-length tags
  //!    b) Bit tags (where mDataSize is bits, not bytes.)
  int mDefaultValueSize, mMeshValueSize;

  //! stores the size of the data for this tag
  int mDataSize;
  
  //! type of tag data
  DataType dataType;

  //! stores the tag name
  std::string mTagName;
};

} // namespace moab

#endif // TAG_INFO_HPP
