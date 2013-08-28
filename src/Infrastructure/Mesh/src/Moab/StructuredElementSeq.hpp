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

#ifndef STRUCTURED_ELEMENT_SEQUENCE
#define STRUCTURED_ELEMENT_SEQUENCE

//
// Class: StructuredElementSeq
//
// Purpose: represent a rectangular element of mesh
//
// A ScdElement represents a rectangular element of mesh, including both vertices and
// elements, and the parametric space used to address that element.  Vertex data,
// i.e. coordinates, may not be stored directly in the element, but the element returns
// information about the vertex handles of vertices in the element.  Vertex and element
// handles associated with the element are each contiguous.

#include "ElementSequence.hpp"
#include "ScdElementData.hpp"

namespace moab {

class StructuredElementSeq : public ElementSequence
{
public:

    //! constructor
  StructuredElementSeq(
                EntityHandle start_handle,
                const int imin, const int jmin, const int kmin,
                const int imax, const int jmax, const int kmax,
                int *is_periodic = NULL);
  
  virtual ~StructuredElementSeq();

  ScdElementData* sdata() 
    { return reinterpret_cast<ScdElementData*>(data()); }
  ScdElementData const* sdata() const 
    { return reinterpret_cast<const ScdElementData*>(data()); }

    //! get handle of vertex at i, j, k
  EntityHandle get_vertex(const int i, const int j, const int k) const
    { return get_vertex( HomCoord(i,j,k) ); }
  
    //! get handle of vertex at homogeneous coords
  inline EntityHandle get_vertex(const HomCoord &coords) const
    { return sdata()->get_vertex(coords); }
  
    //! get handle of element at i, j, k
  EntityHandle get_element(const int i, const int j, const int k) const
    { return sdata()->get_element( i, j, k ); }
  
    //! get handle of element at homogeneous coords
  EntityHandle get_element(const HomCoord &coords) const
    { return sdata()->get_element( coords.i(), coords.j(), coords.k() ); }
  
    //! get min params for this element
  const HomCoord &min_params() const
    { return sdata()->min_params(); }
  void min_params(HomCoord &coords) const
    { coords = min_params(); }
  void min_params(int &i, int &j, int &k) const
    { i = min_params().i(); j = min_params().j(); k = min_params().k(); }

    //! get max params for this element
  const HomCoord &max_params() const
    { return sdata()->max_params(); }
  void max_params(HomCoord &coords) const
    { coords = max_params(); }
  void max_params(int &i, int &j, int &k) const
    { i = max_params().i(); j = max_params().j(); k = max_params().k(); }
  
    //! get the number of vertices in each direction, inclusive
  void param_extents(int &di, int &dj, int &dk) const
    { sdata()->param_extents( di, dj, dk ); }

    //! given a handle, get the corresponding parameters
  ErrorCode get_params(const EntityHandle ehandle,
                          int &i, int &j, int &k) const
    { return sdata()->get_params( ehandle, i, j, k ); }
  
    //! convenience functions for parameter extents
  int i_min() const {return min_params().i();}
  int j_min() const {return min_params().j();}
  int k_min() const {return min_params().k();}
  int i_max() const {return max_params().i();}
  int j_max() const {return max_params().j();}
  int k_max() const {return max_params().k();}

    //! test the bounding vertex sequences and determine whether they fully
    //! define the vertices covering this element block's parameter space
  inline bool boundary_complete() const
    { return sdata()->boundary_complete(); }

    //! test whether this sequence contains these parameters
  bool contains(const int i, const int j, const int k) const
    { return sdata()->contains(HomCoord(i,j,k)); }
  inline bool contains(const HomCoord &coords) const
    { return sdata()->contains(coords); }

    //! get connectivity of an entity given entity's parameters
  ErrorCode get_params_connectivity(const int i, const int j, const int k,
                                std::vector<EntityHandle>& connectivity) const
    { return sdata()->get_params_connectivity( i, j, k, connectivity ); }
  
    //! Return whether box is periodic in i
    /** Return whether box is periodic in i
     * \return True if box is periodic in i direction
     */
  int is_periodic_i() const {return sdata()->is_periodic_i();};
  
    //! Return whether box is periodic in j
    /** Return whether box is periodic in j
     * \return True if box is periodic in j direction
     */
  int is_periodic_j() const {return sdata()->is_periodic_j();};
  
    //! Return whether box is periodic in i and j
    /** Return whether box is periodic in i and j
     * \param is_periodic_ij Non-zero if periodic in i [0] or j [1]
     */
  void is_periodic(int is_periodic_ij[2]) const {sdata()->is_periodic(is_periodic_ij);};
  
  
    /***************** Methods from ElementSequence *****************/

  virtual ErrorCode get_connectivity( EntityHandle handle,
                                        std::vector<EntityHandle>& connect,
                                        bool topological = false ) const;
  
  virtual ErrorCode get_connectivity( EntityHandle handle,
                                        EntityHandle const*& connect,
                                        int &connect_length,
                                        bool topological = false,
                                        std::vector<EntityHandle>* storage = 0
                                       ) const;

  virtual ErrorCode set_connectivity( EntityHandle handle,
                                        EntityHandle const* connect,
                                        int connect_length );
  
  virtual EntityHandle* get_connectivity_array();
  
  
    /***************** Methods from EntitySequence *****************/

    /* Replace the ElementSequence implementation of this method with
     * one that always returns zero, because we cannot re-use handles
     * that are within a ScdElementData
     */
  virtual int values_per_entity() const;

  virtual EntitySequence* split( EntityHandle here );

  virtual SequenceData* create_data_subset( EntityHandle start_handle,
                                            EntityHandle end_handle ) const;

  virtual void get_const_memory_use( unsigned long& bytes_per_entity,
                                     unsigned long& size_of_sequence ) const;

protected:
  StructuredElementSeq( StructuredElementSeq& split_from, EntityHandle here )
    : ElementSequence( split_from, here )
    {}
};

} // namespace moab

#endif
