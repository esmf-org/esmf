/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2008 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#include "SweptElementSeq.hpp"
#include "SweptVertexData.hpp"
#include "SweptElementData.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/CN.hpp"
#include "Internals.hpp"

namespace moab {

SweptElementSeq::SweptElementSeq(EntityHandle shandle,
                                 const int imin, const int jmin, const int kmin,
				 const int imax, const int jmax, const int kmax,
                                 const int* Cq ) 
  : ElementSequence( shandle, 
                     ScdElementData::calc_num_entities( shandle,
                                                        imax-imin,
                                                        jmax-jmin,
                                                        kmax-kmin ),
                     CN::VerticesPerEntity(TYPE_FROM_HANDLE(shandle)),
                     new SweptElementData( shandle, 
					   imin, jmin, kmin,
					   imax, jmax, kmax,
					   Cq ) )
{
}

SweptElementSeq::~SweptElementSeq() 
{
}

ErrorCode SweptElementSeq::get_connectivity( 
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

ErrorCode SweptElementSeq::get_connectivity( 
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
    return MB_NOT_IMPLEMENTED;
  }
  
  storage->clear();
  ErrorCode rval = get_connectivity( handle, *storage, topo );
  connect = &(*storage)[0];
  connect_length = storage->size();
  return rval;
}

ErrorCode SweptElementSeq::set_connectivity( 
                                        EntityHandle,
                                        EntityHandle const*,
                                        int )
{
  return MB_NOT_IMPLEMENTED;
}

EntityHandle* SweptElementSeq::get_connectivity_array()
  { return 0; }

int SweptElementSeq::values_per_entity() const
  { return -1; } // never reuse freed handles for swept elements 

EntitySequence* SweptElementSeq::split( EntityHandle here )
  { return new SweptElementSeq( *this, here ); }

SequenceData* SweptElementSeq::create_data_subset( EntityHandle, EntityHandle ) const
  { return 0; }

void SweptElementSeq::get_const_memory_use( unsigned long& bytes_per_entity,
                                                 unsigned long& sequence_size ) const
{
  sequence_size = sizeof(*this);
  bytes_per_entity = sdata()->get_memory_use() / sdata()->size();
}

} // namespace moab
