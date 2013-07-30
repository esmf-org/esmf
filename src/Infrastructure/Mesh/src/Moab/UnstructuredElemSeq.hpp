#ifndef UNSTRUCTURED_ELEM_SEQ_HPP
#define UNSTRUCTURED_ELEM_SEQ_HPP

#include "ElementSequence.hpp"
#include "SequenceData.hpp"

namespace moab {

class UnstructuredElemSeq : public ElementSequence
{
public:

  UnstructuredElemSeq( EntityHandle start_handle, 
                       EntityID entity_count, 
                       unsigned nodes_per_entity,
                       SequenceData* data );

  UnstructuredElemSeq( EntityHandle start_handle, 
                       EntityID entity_count, 
                       unsigned nodes_per_entity,
                       EntityID sequence_data_size);

  virtual ~UnstructuredElemSeq();

  int values_per_entity() const;
  
  virtual EntitySequence* split( EntityHandle here );
  
  SequenceData* create_data_subset( EntityHandle start, EntityHandle end ) const;
                       
  virtual ErrorCode get_connectivity( EntityHandle handle,
                                        std::vector<EntityHandle>& connect,
                                        bool topological = false ) const;
  
  virtual ErrorCode get_connectivity( EntityHandle handle,
                                        EntityHandle const*& connect,
                                        int &connect_length,
                                        bool topological = false,
                                        std::vector<EntityHandle>* storage = 0
                                       ) const;

  ErrorCode set_connectivity( EntityHandle handle,
                                EntityHandle const* connect,
                                int connect_length );
  
  EntityHandle* get_connectivity_array();
  
  ErrorCode push_front( EntityID count );
  ErrorCode push_back ( EntityID count );
  
  
  void get_const_memory_use( unsigned long& bytes_per_entity,
                             unsigned long& size_of_sequence ) const;
protected:

  inline EntityHandle const* get_array() const
  {
    return reinterpret_cast<EntityHandle const*>(data()->get_sequence_data(0))
      + nodes_per_element() * (start_handle() - data()->start_handle());
  }

  inline EntityHandle* get_array()
  {
    return reinterpret_cast<EntityHandle*>(data()->get_sequence_data(0))
      + nodes_per_element() * (start_handle() - data()->start_handle());
  }

  UnstructuredElemSeq( UnstructuredElemSeq& split_from, EntityHandle here )
    : ElementSequence( split_from, here )
   {}
};

} // namespace moab

#endif

  
