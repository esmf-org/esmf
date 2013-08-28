#include "PolyElementSeq.hpp"

namespace moab {

PolyElementSeq::~PolyElementSeq() {}
  
EntitySequence* PolyElementSeq::split( EntityHandle here )
  { return new PolyElementSeq( *this, here ); }
                       

ErrorCode
PolyElementSeq::get_connectivity( EntityHandle handle,
                                  std::vector<EntityHandle>& connect,
                                  bool ) const
{
  EntityHandle const* conn = get_array() + nodes_per_element() * (handle - start_handle());
  int len = nodes_per_element();
  connect.reserve( connect.size() + len );
  std::copy( conn, conn+len, std::back_inserter( connect ) );
  return MB_SUCCESS;
}


ErrorCode
PolyElementSeq::get_connectivity( EntityHandle handle,
                                  EntityHandle const*& conn_ptr,
                                  int& len,
                                  bool,
                                  std::vector<EntityHandle>* ) const
{
  conn_ptr = get_array() + nodes_per_element() * (handle - start_handle());
  len = nodes_per_element();
  return MB_SUCCESS;
}
  
} // namespace moab
