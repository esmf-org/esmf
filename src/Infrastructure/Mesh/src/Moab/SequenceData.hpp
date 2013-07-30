#ifndef SEQUENCE_DATA_HPP
#define SEQUENCE_DATA_HPP


#include "TypeSequenceManager.hpp"

#include <vector>
#include <stdlib.h>
#include <string.h>

namespace moab {

class SequenceData
{
public:

  typedef std::vector<EntityHandle>* AdjacencyDataType;

  /**\param num_sequence_arrays Number of data arrays needed by the EntitySequence
   * \param start               First handle in this SequenceData
   * \param end                 Last handle in this SequenceData
   */
  inline SequenceData( int num_sequence_arrays, 
                       EntityHandle start,
                       EntityHandle end );
  
  virtual ~SequenceData();
  
  /**\return first handle in this sequence data */
  EntityHandle start_handle() const 
    { return startHandle; }
  
  /**\return last handle in this sequence data */
  EntityHandle end_handle() const
    { return endHandle; }
    
  EntityID size() const
    { return endHandle + 1 - startHandle; }
  
  /**\return ith array of EnitySequence-specific data */
  void*       get_sequence_data( int array_num )       
                { return arraySet[-1-array_num]; }
  /**\return ith array of EnitySequence-specific data */
  void const* get_sequence_data( int array_num ) const 
                { return arraySet[-1-array_num]; }
  
  /**\return array of adjacency data, or NULL if none. */
  AdjacencyDataType*       get_adjacency_data( )       
                { return reinterpret_cast<AdjacencyDataType*>(arraySet[0]); }
  /**\return array of adjacency data, or NULL if none. */
  AdjacencyDataType const* get_adjacency_data( ) const 
                { return reinterpret_cast<AdjacencyDataType const*>(arraySet[0]); }
  
  /**\return array of dense tag data, or NULL if none. */
  void*       get_tag_data( unsigned tag_num )              
                { return tag_num < numTagData  ? arraySet[tag_num+1] : 0; }
  /**\return array of dense tag data, or NULL if none. */
  void const* get_tag_data( unsigned tag_num ) const        
                { return tag_num < numTagData  ? arraySet[tag_num+1] : 0; }
  
  /**\brief Allocate array of sequence-specific data
   *
   * Allocate an array of EntitySequence-specific data.
   *\param array_num Index for which to allocate array.  
   *                 Must be in [0,num_sequence_arrays], where 
   *                 num_sequence_arrays is constructor argument.
   *\param bytes_per_ent  Bytes to allocate for each entity.
   *\param initial_val Value to initialize array with.  If non-null, must
   *                   be bytes_per_ent long.  If NULL, array will be zeroed.
   *\return The newly allocated array, or NULL if error.
   */
  void* create_sequence_data( int array_num, 
                              int bytes_per_ent,
                              const void* initial_val = 0 );
                             
  /**\brief Allocate array of sequence-specific data
   *
   * Allocate an array of EntitySequence-specific data.
   *\param array_num Index for which to allocate array.  
   *                 Must be in [0,num_sequence_arrays], where 
   *                 num_sequence_arrays is constructor argument.
   *\return The newly allocated array, or NULL if error.
   */
  void* create_custom_data( int array_num, size_t total_bytes );
  
  /**\brief Allocate array for storing adjacency data.
   *
   * Allocate array for storing adjacency data.
   *\return The newly allocated array, or NULL if already allocated.
   */
  AdjacencyDataType* allocate_adjacency_data();
  
  /**\brief Allocate array of dense tag data
   *
   * Allocate an array of dense tag data.
   *\param index        Dense tag ID for which to allocate array.
   *\param bytes_per_ent  Bytes to allocate for each entity.
   *\return The newly allocated array, or NULL if error.
   */
  void* allocate_tag_array( int index, int bytes_per_ent, const void* default_value = 0 );
  
  /**\brief Create new SequenceData that is a copy of a subset of this one
    *
    * Create a new SequenceData that is a copy of a subset of this one.
    * This function is intended for use in subdividing a SequenceData
    * for operations such as changing the number of nodes in a block of
    * elements.
    *\param start  First handle for resulting subset
    *\param end    Last handle for resulting subset
    *\param sequence_data_sizes Bytes-per-entity for sequence-specific data.
    *\NOTE Does not copy tag data.
    */
  SequenceData* subset( EntityHandle start, 
                        EntityHandle end,
                        const int* sequence_data_sizes ) const;
  
  /**\brief SequenceManager data */
  TypeSequenceManager::SequenceDataPtr seqManData;
  
  /**\brief Move tag data for a subset of this sequences to specified sequence */
  void move_tag_data( SequenceData* destination, const int* tag_sizes, int num_tag_sizes );
  
  /**\brief Free all tag data arrays */
  void release_tag_data(const int* tag_sizes, int num_tag_sizes);
  /**\brief Free specified tag data array */
  void release_tag_data( int index, int tag_size );
  
protected:

  SequenceData( const SequenceData* subset_from,
                EntityHandle start, 
                EntityHandle end,
                const int* sequence_data_sizes );

private:

  void increase_tag_count( unsigned by_this_many );

  void* create_data( int index, int bytes_per_ent, const void* initial_val = 0 );
  void copy_data_subset( int index, 
                         int size_per_ent, 
                         const void* source, 
                         size_t offset, 
                         size_t count );

  const int numSequenceData;
  unsigned numTagData;
  void** arraySet;
  EntityHandle startHandle, endHandle;
};

inline SequenceData::SequenceData( int num_sequence_arrays, 
                                   EntityHandle start,
                                   EntityHandle end )
  : numSequenceData(num_sequence_arrays),
    numTagData(0),
    startHandle(start),
    endHandle(end)
{
  const size_t sz = sizeof(void*) * (num_sequence_arrays + 1);
  void** data = (void**)malloc( sz );
  memset( data, 0, sz );
  arraySet = data + num_sequence_arrays;
}

} // namespace moab

#endif
