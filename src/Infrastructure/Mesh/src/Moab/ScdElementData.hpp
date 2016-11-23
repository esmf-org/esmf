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

#ifndef SCD_ELEMENT_DATA_HPP
#define SCD_ELEMENT_DATA_HPP

//
// Class: ScdElementData
//
// Purpose: represent a rectangular element of mesh
//
// A ScdElement represents a rectangular element of mesh, including both vertices and
// elements, and the parametric space used to address that element.  Vertex data,
// i.e. coordinates, may not be stored directly in the element, but the element returns
// information about the vertex handles of vertices in the element.  Vertex and element
// handles associated with the element are each contiguous.

#include "SequenceData.hpp"
#include "moab/HomXform.hpp"
#include "moab/CN.hpp"
#include "ScdVertexData.hpp"
#include "Internals.hpp"
#include "moab/Range.hpp"

#include <vector>
#include <algorithm>

namespace moab {

class ScdElementData : public SequenceData
{

  //! structure to hold references to bounding vertex blocks
class VertexDataRef
{
private:
  HomCoord minmax[2];
  HomXform xform, invXform;
  ScdVertexData *srcSeq;
public:
  friend class ScdElementData;
  
  VertexDataRef(const HomCoord &min, const HomCoord &max,
                const HomXform &tmp_xform, ScdVertexData *this_seq);
    
  bool contains(const HomCoord &coords) const;
};

private:

    //! parameter min/max/stride for vertices, in homogeneous coords ijkh; elements
    //! are one less than max
  HomCoord boxParams[3];

    //! difference between max and min params plus one (i.e. # VERTICES in
    //! each parametric direction)
  int dIJK[3];
  
    //! difference between max and min params (i.e. # ELEMENTS in
    //! each parametric direction)
  int dIJKm1[3];

    //! whether scd element rectangle is periodic in i and possibly j
  int isPeriodic[2];
  
    //! bare constructor, so compiler doesn't create one for me
  ScdElementData();

    //! list of bounding vertex blocks
  std::vector<VertexDataRef> vertexSeqRefs;

public:

    //! constructor
  ScdElementData( EntityHandle start_handle,
                  const int imin, const int jmin, const int kmin,
                  const int imax, const int jmax, const int kmax,
                  int *is_periodic);
  
  virtual ~ScdElementData();
  
    //! get handle of vertex at homogeneous coords
  inline EntityHandle get_vertex(const HomCoord &coords) const;
  inline EntityHandle get_vertex(int i, int j, int k) const
    { return get_vertex(HomCoord(i,j,k)); }
  
    //! get handle of element at i, j, k
  EntityHandle get_element(const int i, const int j, const int k) const;
  
    //! get min params for this element
  const HomCoord &min_params() const;

    //! get max params for this element
  const HomCoord &max_params() const;
  
    //! get the number of vertices in each direction, inclusive
  void param_extents(int &di, int &dj, int &dk) const;

    //! given a handle, get the corresponding parameters
  ErrorCode get_params(const EntityHandle ehandle,
                          int &i, int &j, int &k) const;

    //! return whether rectangle is periodic in i
  inline int is_periodic_i() const {return isPeriodic[0];};
  
    //! return whether rectangle is periodic in j
  inline int is_periodic_j() const {return isPeriodic[1];};

  inline void is_periodic(int is_p[2]) const 
      {is_p[0] = isPeriodic[0]; is_p[1] = isPeriodic[1];}
  
    //! convenience functions for parameter extents
  int i_min() const {return (boxParams[0].hom_coord())[0];}
  int j_min() const {return (boxParams[0].hom_coord())[1];}
  int k_min() const {return (boxParams[0].hom_coord())[2];}
  int i_max() const {return (boxParams[1].hom_coord())[0];}
  int j_max() const {return (boxParams[1].hom_coord())[1];}
  int k_max() const {return (boxParams[1].hom_coord())[2];}

    //! test the bounding vertex sequences and determine whether they fully
    //! define the vertices covering this element block's parameter space
  bool boundary_complete() const;

    //! test whether this sequence contains these parameters
  inline bool contains(const HomCoord &coords) const;

    //! test whether *vertex parameterization* in this sequence contains these parameters
  inline bool contains_vertex(const HomCoord &coords) const;

    //! get connectivity of an entity given entity's parameters
  inline ErrorCode get_params_connectivity(const int i, const int j, const int k,
                                       std::vector<EntityHandle>& connectivity) const;
  
    //! add a vertex seq ref to this element sequence;
    //! if bb_input is true, bounding box (in eseq-local coords) of vseq being added 
    //! is input in bb_min and bb_max (allows partial sharing of vseq rather than the whole
    //! vseq); if it's false, the whole vseq is referenced and the eseq-local coordinates
    //! is computed from the transformed bounding box of the vseq
  ErrorCode add_vsequence(ScdVertexData *vseq, 
                             const HomCoord &p1, const HomCoord &q1,
                             const HomCoord &p2, const HomCoord &q2,
                             const HomCoord &p3, const HomCoord &q3,
                             bool bb_input = false,
                             const HomCoord &bb_min = HomCoord::unitv[0],
                             const HomCoord &bb_max = HomCoord::unitv[0]);


  SequenceData* subset( EntityHandle start, 
                        EntityHandle end,
                        const int* sequence_data_sizes,
                        const int* tag_data_sizes ) const;
  
  static EntityID calc_num_entities( EntityHandle start_handle,
                                       int irange,
                                       int jrange,
                                     int krange,
                                     int *is_periodic = NULL);

  unsigned long get_memory_use() const;
};

inline EntityHandle ScdElementData::get_element(const int i, const int j, const int k) const
{
  return start_handle() + (i-i_min()) + (j-j_min())*dIJKm1[0] + (k-k_min())*dIJKm1[0]*dIJKm1[1];
}

inline const HomCoord &ScdElementData::min_params() const
{
  return boxParams[0];
}

inline const HomCoord &ScdElementData::max_params() const
{
  return boxParams[1];
}

  //! get the number of vertices in each direction, inclusive
inline void ScdElementData::param_extents(int &di, int &dj, int &dk) const
{
  di = dIJK[0];
  dj = dIJK[1];
  dk = dIJK[2];
}

inline ErrorCode ScdElementData::get_params(const EntityHandle ehandle,
                                              int &i, int &j, int &k) const
{
  if (TYPE_FROM_HANDLE(ehandle) != TYPE_FROM_HANDLE(start_handle())) return MB_FAILURE;

  int hdiff = ehandle - start_handle();

    // use double ?: test below because on some platforms, both sides of the : are
    // evaluated, and if dIJKm1[1] is zero, that'll generate a divide-by-zero
  k = (dIJKm1[1] > 0 ? hdiff / (dIJKm1[1] > 0 ? dIJKm1[0]*dIJKm1[1] : 1) : 0);
  j = (hdiff - (k*dIJKm1[0]*dIJKm1[1])) / dIJKm1[0];
  i = hdiff % dIJKm1[0];

  k += boxParams[0].k();
  j += boxParams[0].j();
  i += boxParams[0].i();

  return (ehandle >= start_handle() &&
          ehandle < start_handle()+size() &&
          i >= i_min() && i <= i_max() &&
          j >= j_min() && j <= j_max() &&
          k >= k_min() && k <= k_max()) ? MB_SUCCESS : MB_FAILURE;
}

inline bool ScdElementData::contains(const HomCoord &temp) const 
{
    // upper bound is < instead of <= because element params max is one less
    // than vertex params max, except in case of 2d or 1d sequence
  return ((dIJKm1[0] && temp.i() >= boxParams[0].i() && temp.i() < boxParams[0].i()+dIJKm1[0]) &&
          ((!dIJKm1[1] && temp.j() == boxParams[1].j()) || 
           (dIJKm1[1] && temp.j() >= boxParams[0].j() && temp.j() < boxParams[0].j()+dIJKm1[1])) &&
          ((!dIJKm1[2] && temp.k() == boxParams[1].k()) || 
           (dIJKm1[2] && temp.k() >= boxParams[0].k() && temp.k() < boxParams[0].k()+dIJKm1[2])));
}
  
inline bool ScdElementData::contains_vertex(const HomCoord &temp) const 
{
    // upper bound is < instead of <= because element params max is one less
    // than vertex params max, except in case of 2d or 1d sequence
  return ((dIJK[0] && temp.i() >= boxParams[0].i() && temp.i() < boxParams[0].i()+dIJK[0]) &&
          ((!dIJK[1] && temp.j() == boxParams[1].j()) || 
           (dIJK[1] && temp.j() >= boxParams[0].j() && temp.j() < boxParams[0].j()+dIJK[1])) &&
          ((!dIJK[2] && temp.k() == boxParams[1].k()) || 
           (dIJK[2] && temp.k() >= boxParams[0].k() && temp.k() < boxParams[0].k()+dIJK[2])));
}
  
inline bool ScdElementData::VertexDataRef::contains(const HomCoord &coords) const 
{
  return (minmax[0] <= coords && minmax[1] >= coords);
}

inline ScdElementData::VertexDataRef::VertexDataRef(const HomCoord &this_min, const HomCoord &this_max,
                                    const HomXform &tmp_xform, ScdVertexData *this_seq)
    : xform(tmp_xform), invXform(tmp_xform.inverse()), srcSeq(this_seq)
{
  minmax[0] = HomCoord(this_min);
  minmax[1] = HomCoord(this_max); 
}

inline EntityHandle ScdElementData::get_vertex(const HomCoord &coords) const
{
  assert(boundary_complete());
   for (std::vector<VertexDataRef>::const_iterator it = vertexSeqRefs.begin();
        it != vertexSeqRefs.end(); ++it) {
     if ((*it).minmax[0] <= coords && (*it).minmax[1] >= coords) {
         // first get the vertex block-local parameters
       HomCoord local_coords = coords / (*it).xform;

       assert((*it).srcSeq->contains(local_coords));

      // now get the vertex handle for those coords
       return (*it).srcSeq->get_vertex(local_coords);
     }
   }
   
     // got here, it's an error
   return 0;
}

inline ErrorCode ScdElementData::add_vsequence(ScdVertexData *vseq, 
                                                 const HomCoord &p1, const HomCoord &q1,
                                                 const HomCoord &p2, const HomCoord &q2, 
                                                 const HomCoord &p3, const HomCoord &q3,
                                                 bool bb_input,
                                                 const HomCoord &bb_min,
                                                 const HomCoord &bb_max)
{
    // compute the transform given the vseq-local parameters and the mapping to
    // this element sequence's parameters passed in minmax
  HomXform M;
  M.three_pt_xform(p1, q1, p2, q2, p3, q3);
  
    // min and max in element seq's parameter system may not be same as those in 
    // vseq's system, so need to take min/max

  HomCoord minmax[2];
  if (bb_input) {
    minmax[0] = bb_min;
    minmax[1] = bb_max;
  }
  else {
    minmax[0] = vseq->min_params() * M;
    minmax[1] = vseq->max_params() * M;
  }
  
    // check against other vseq's to make sure they don't overlap
  for (std::vector<VertexDataRef>::const_iterator vsit = vertexSeqRefs.begin();
       vsit != vertexSeqRefs.end(); ++vsit)
    if ((*vsit).contains(minmax[0]) || (*vsit).contains(minmax[1])) 
      return MB_FAILURE;
    
  HomCoord tmp_min(std::min(minmax[0].i(), minmax[1].i()), 
                   std::min(minmax[0].j(), minmax[1].j()), 
                   std::min(minmax[0].k(), minmax[1].k()));
  HomCoord tmp_max(std::max(minmax[0].i(), minmax[1].i()), 
                   std::max(minmax[0].j(), minmax[1].j()), 
                   std::max(minmax[0].k(), minmax[1].k()));

  
    // set up a new vertex sequence reference
  VertexDataRef tmp_seq_ref(tmp_min, tmp_max, M, vseq);

    // add to the list
  vertexSeqRefs.push_back(tmp_seq_ref);
  
  return MB_SUCCESS;
}

inline ErrorCode ScdElementData::get_params_connectivity(const int i, const int j, const int k,
                                                           std::vector<EntityHandle>& connectivity) const
{
  if (contains(HomCoord(i, j, k)) == false) return MB_FAILURE;
  
  int ip1 = (isPeriodic[0] ? (i+1)%dIJKm1[0] : i+1),
      jp1 = (isPeriodic[1] ? (j+1)%dIJKm1[1] : j+1);
  
  connectivity.push_back(get_vertex(i, j, k));
  connectivity.push_back(get_vertex(ip1, j, k));
  if (CN::Dimension(TYPE_FROM_HANDLE(start_handle())) < 2) return MB_SUCCESS;
  connectivity.push_back(get_vertex(ip1, jp1, k));
  connectivity.push_back(get_vertex(i, jp1, k));
  if (CN::Dimension(TYPE_FROM_HANDLE(start_handle())) < 3) return MB_SUCCESS;
  connectivity.push_back(get_vertex(i, j, k+1));
  connectivity.push_back(get_vertex(ip1, j, k+1));
  connectivity.push_back(get_vertex(ip1, jp1, k+1));
  connectivity.push_back(get_vertex(i, jp1, k+1));
  return MB_SUCCESS;
}
  
} // namespace moab

#endif
