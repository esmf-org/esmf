#include "VertexSequence.hpp"

namespace moab {

VertexSequence::~VertexSequence() {}

EntitySequence* VertexSequence::split( EntityHandle here )
  { return new VertexSequence( *this, here ); }

SequenceData* VertexSequence::create_data_subset( EntityHandle start,
                                                  EntityHandle end ) const
{
  const int sizes[] = { sizeof(double), sizeof(double), sizeof(double) };
  return data()->subset(start, end, sizes );
}
  
ErrorCode VertexSequence::push_back( EntityID count )
  { return EntitySequence::append_entities(count); }

ErrorCode VertexSequence::push_front( EntityID count )
  { return EntitySequence::prepend_entities(count); }

void VertexSequence::get_const_memory_use( unsigned long& per_ent, unsigned long& seq ) const
{
  per_ent = 3 * sizeof(double);
  seq = sizeof(*this);
}

} // namespace moab
