#include "UnstructuredElemSeq.hpp"
#include "SequenceData.hpp"
#include "moab/CN.hpp"

namespace moab {

UnstructuredElemSeq::UnstructuredElemSeq( EntityHandle shandle, 
                                          EntityID entity_count, 
                                          unsigned nodes_per_entity,
                                          SequenceData* dat )
  : ElementSequence( shandle, entity_count, nodes_per_entity, dat )
  {}
  

UnstructuredElemSeq::UnstructuredElemSeq( EntityHandle shandle, 
                                          EntityID entity_count, 
                                          unsigned nodes_per_entity,
                                          EntityID data_size )
  : ElementSequence( shandle, entity_count, nodes_per_entity,
                      new SequenceData( 1, shandle, shandle + data_size - 1))
{
  data()->create_sequence_data( 0, nodes_per_entity * sizeof(EntityHandle) );
}


UnstructuredElemSeq::~UnstructuredElemSeq()
{}


int UnstructuredElemSeq::values_per_entity() const
  { return nodes_per_element(); }


EntitySequence*
UnstructuredElemSeq::split( EntityHandle here )
{
  if (here <= start_handle() || here > end_handle())
    return 0;
  
  return new UnstructuredElemSeq( *this, here );
}


SequenceData*
UnstructuredElemSeq::create_data_subset( EntityHandle start, EntityHandle end ) const
{
  int esize = nodes_per_element() * sizeof(EntityHandle);
  return data()->subset(start, end, &esize );
}


void
UnstructuredElemSeq::get_const_memory_use( unsigned long& bytes_per_entity,
                                           unsigned long& size_of_sequence ) const
{
  bytes_per_entity = nodes_per_element() * sizeof(EntityHandle);
  size_of_sequence = sizeof(*this);
}

ErrorCode
UnstructuredElemSeq::get_connectivity( EntityHandle handle,
                                       std::vector<EntityHandle>& connect,
                                       bool topological ) const
{
  EntityHandle const* conn = get_array() + nodes_per_element() * (handle - start_handle());
  int len = topological ? CN::VerticesPerEntity(type()) : nodes_per_element();
  connect.reserve( connect.size() + len );
  std::copy( conn, conn+len, std::back_inserter( connect ) );
  return MB_SUCCESS;
}


ErrorCode
UnstructuredElemSeq::get_connectivity( EntityHandle handle,
                                       EntityHandle const*& conn_ptr,
                                       int& len,
                                       bool topological,
                                       std::vector<EntityHandle>* ) const
{
  conn_ptr = get_array() + nodes_per_element() * (handle - start_handle());
  len = topological ? CN::VerticesPerEntity(type()) : nodes_per_element();
  return MB_SUCCESS;
}


ErrorCode
UnstructuredElemSeq::set_connectivity( EntityHandle handle,
                                       EntityHandle const* connect,
                                       int connect_length )
{
  if ((unsigned)connect_length != nodes_per_element())
    return MB_INDEX_OUT_OF_RANGE;
  EntityHandle* conn_ptr = get_array() + nodes_per_element() * (handle - start_handle());
  std::copy( connect, connect+connect_length, conn_ptr );
  return MB_SUCCESS;
}


EntityHandle* UnstructuredElemSeq::get_connectivity_array()
  { return get_array(); }

ErrorCode UnstructuredElemSeq::push_back( EntityID count )
  { return EntitySequence::append_entities(count); }

ErrorCode UnstructuredElemSeq::push_front( EntityID count )
  { return EntitySequence::prepend_entities(count); }


} // namespace moab
