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

#include "StructuredElementSeq.hpp"
#include "ScdVertexData.hpp"
#include "ScdElementData.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/CN.hpp"
#include "Internals.hpp"

namespace moab {

StructuredElementSeq::StructuredElementSeq(EntityHandle shandle,
                             const int imin, const int jmin, const int kmin,
                                           const int imax, const int jmax, const int kmax,
                                           int *is_per) 
  : ElementSequence( shandle, 
                     ScdElementData::calc_num_entities( shandle,
                                                        imax-imin,
                                                        jmax-jmin,
                                                        kmax-kmin,
                                                        is_per),
                     CN::VerticesPerEntity(TYPE_FROM_HANDLE(shandle)),
                     new ScdElementData( shandle, 
                                        imin, jmin, kmin,
                                         imax, jmax, kmax, is_per) )
{
}

StructuredElementSeq::~StructuredElementSeq() 
{
}

ErrorCode StructuredElementSeq::get_connectivity( 
                                        EntityHandle handle,
                                        std::vector<EntityHandle>& connect,
                                        bool /*topological*/ ) const
{
  int i, j, k;
  ErrorCode rval = get_params( handle, i, j, k );
  if (MB_SUCCESS == rval)
    rval = get_params_connectivity( i, j, k, connect );
  return rval;
}

ErrorCode StructuredElementSeq::get_connectivity( 
                                        EntityHandle handle,
                                        EntityHandle const*& connect,
                                        int &connect_length,
                                        bool topo,
                                        std::vector<EntityHandle>* storage
                                        ) const
{
  if (!storage) {
    connect = 0;
    connect_length = 0;
    return MB_STRUCTURED_MESH;
  }
  
  storage->clear();
  ErrorCode rval = get_connectivity( handle, *storage, topo );
  connect = &(*storage)[0];
  connect_length = storage->size();
  return rval;
}

ErrorCode StructuredElementSeq::set_connectivity( 
                                        EntityHandle,
                                        EntityHandle const*,
                                        int )
{
  return MB_STRUCTURED_MESH;
}

EntityHandle* StructuredElementSeq::get_connectivity_array()
  { return 0; }

int StructuredElementSeq::values_per_entity() const
  { return -1; } // never reuse freed handles for structured elements 

EntitySequence* StructuredElementSeq::split( EntityHandle here )
  { return new StructuredElementSeq( *this, here ); }

SequenceData* StructuredElementSeq::create_data_subset( EntityHandle, EntityHandle ) const
  { return 0; }

void StructuredElementSeq::get_const_memory_use( unsigned long& bytes_per_entity,
                                                 unsigned long& sequence_size ) const
{
  sequence_size = sizeof(*this);
  bytes_per_entity = sdata()->get_memory_use() / sdata()->size();
}

} // namespace moab
