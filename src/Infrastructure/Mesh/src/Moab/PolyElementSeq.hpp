#ifndef POLY_ELEMENT_SEQ_HPP
#define POLY_ELEMENT_SEQ_HPP

#include "UnstructuredElemSeq.hpp"

namespace moab {


class PolyElementSeq : public UnstructuredElemSeq
{
public:
  PolyElementSeq( EntityHandle shandle, 
                  EntityID entity_count, 
                  unsigned nodes_per_entity,
                  SequenceData* dat )
    : UnstructuredElemSeq( shandle, entity_count, nodes_per_entity, dat )
    {}

  PolyElementSeq( EntityHandle shandle, 
                  EntityID entity_count, 
                  unsigned nodes_per_entity,
                  EntityID sequence_data_size)
    : UnstructuredElemSeq( shandle, entity_count, nodes_per_entity, sequence_data_size )
    {}

  virtual ~PolyElementSeq();
  
  virtual EntitySequence* split( EntityHandle here );
                       
  virtual ErrorCode get_connectivity( EntityHandle handle,
                                        std::vector<EntityHandle>& connect,
                                        bool topological = false ) const;
  
  virtual ErrorCode get_connectivity( EntityHandle handle,
                                        EntityHandle const*& connect,
                                        int &connect_length,
                                        bool topological = false,
                                        std::vector<EntityHandle>* storage = 0
                                       ) const;

protected:

  PolyElementSeq( PolyElementSeq& split_from, EntityHandle here )
    : UnstructuredElemSeq( split_from, here )
   {}
};
  
} // namespace moab

#endif
