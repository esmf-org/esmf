/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#include "SweptElementData.hpp"
#include "SweptVertexData.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/CN.hpp"
#include "Internals.hpp"
#include <assert.h>

namespace moab {

EntityID SweptElementData::calc_num_entities(EntityHandle start_handle,
                                int irange, int jrange, int krange)
{
  size_t result = 1;
  switch (CN::Dimension(TYPE_FROM_HANDLE(start_handle))) {
    default: result = 0; assert( false );
      break;
    case 3: result *= krange;
    case 2: result *= jrange;
    case 1: result *= irange;
  }
  return result;
}

SweptElementData::SweptElementData(
                             EntityHandle shandle,
                             const int imin, const int jmin, const int kmin,
                             const int imax, const int jmax, const int kmax,
			     const int* /*Cq*/ ) 
    : SequenceData(0, shandle,
                   shandle + 
                   calc_num_entities( shandle, imax-imin, jmax-jmin, kmax-kmin )
                   - 1)
{
    // need to have meaningful parameters
  assert(imax >= imin && jmax >= jmin && kmax >= kmin);
    
  elementParams[0] = HomCoord(imin, jmin, kmin);
  elementParams[1] = HomCoord(imax, jmax, kmax);
  elementParams[2] = HomCoord(1, 1, 1);
  
    // assign and compute parameter stuff
  dIJK[0] = elementParams[1][0] - elementParams[0][0] + 1;
  dIJK[1] = elementParams[1][1] - elementParams[0][1] + 1;
  dIJK[2] = elementParams[1][2] - elementParams[0][2] + 1;
  dIJKm1[0] = dIJK[0] - 1;
  dIJKm1[1] = dIJK[1] - 1;
  dIJKm1[2] = dIJK[2] - 1;
}

SweptElementData::~SweptElementData() 
{
}

bool SweptElementData::boundary_complete() const
{
    // test the bounding vertex sequences to see if they fully define the
    // vertex parameter space for this rectangular block of elements

  int p;
  std::vector<VertexDataRef> minlist, maxlist;

    // pseudo code:
    // for each vertex sequence v:
  for (std::vector<VertexDataRef>::const_iterator vseq = vertexSeqRefs.begin();
       vseq != vertexSeqRefs.end(); ++vseq)
  {
    //   test min corner mincorner:
    bool mincorner = true;
    //   for each p = (i-1,j,k), (i,j-1,k), (i,j,k-1):
    for (p = 0; p < 3; p++) {

    //     for each vsequence v' != v:
      for (std::vector<VertexDataRef>::const_iterator othervseq = vertexSeqRefs.begin();
           othervseq != vertexSeqRefs.end(); ++othervseq)
      {
        if (othervseq == vseq) continue;        
    //       if v.min-p contained in v'
        if ((*othervseq).contains((*vseq).minmax[0]-HomCoord::unitv[p])) {
    //         mincorner = false
          mincorner = false;
          break;
        }
      }
      if (!mincorner) break;
    }
  
    bool maxcorner = true;
    //   for each p = (i-1,j,k), (i,j-1,k), (i,j,k-1):
    for (p = 0; p < 3; p++) {

    //     for each vsequence v' != v:
      for (std::vector<VertexDataRef>::const_iterator othervseq = vertexSeqRefs.begin();
           othervseq != vertexSeqRefs.end(); ++othervseq)
      {
        if (othervseq == vseq) continue;        
    //       if v.max+p contained in v'
        if ((*othervseq).contains((*vseq).minmax[1]+HomCoord::unitv[p])) {
    //         maxcorner = false
          maxcorner = false;
          break;
        }
      }
      if (!maxcorner) break;
    }

    //   if mincorner add to min corner list minlist
    if (mincorner) minlist.push_back(*vseq);
    //   if maxcorner add to max corner list maxlist
    if (maxcorner) maxlist.push_back(*vseq);
  }
  
    // 
    // if minlist.size = 1 & maxlist.size = 1 & minlist[0] = esequence.min &
    //         maxlist[0] = esequence.max+(1,1,1)
  if (minlist.size() == 1 && maxlist.size() == 1 &&
      minlist[0].minmax[0] == elementParams[0] && 
      maxlist[0].minmax[1] == elementParams[1])
      //   complete
    return true;
    // else

  return false;
}


SequenceData* SweptElementData::subset( EntityHandle /*start*/, 
                                      EntityHandle /*end*/,
                                      const int* /*sequence_data_sizes*/,
                                      const int* /*tag_data_sizes*/ ) const
{
  return 0;
}

unsigned long SweptElementData::get_memory_use() const
{
  return sizeof(*this) + vertexSeqRefs.capacity() * sizeof(VertexDataRef);
}

} // namespace moab
