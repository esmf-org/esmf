// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
// The contents of this file were migrated from the legacy mesh file 
// ESMCI_Mapping.h in June of 2018.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#ifndef ESMCI_MBMesh_Mapping_h
#define ESMCI_MBMesh_Mapping_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_Types.h"

namespace ESMCI{

  // Type used to specify method used to do spherical (3,2) mapping
  // eventually could be broadened to specify other types
  enum MB_MAP_TYPE {MB_MAP_TYPE_CART_APPROX=0, MB_MAP_TYPE_GREAT_CIRCLE};
  extern MB_MAP_TYPE mb_sph_map_type;

  extern bool is_map_sph;

/**
 * Mapping subsystem.  Provides the mapping from a parametric element
 * space to physical coordinates.  Provides jacobian based utilities.
 * 
 * The system is templated on the numerical type to allow for sensitivies
 * wrt. parametric coordinates and mapping data.
 */

/**
 * Basic trait type to determine the numerical types used in the
 * mapping.
 */
template<typename MDATA_TYPE=double, typename PCOORD_TYPE=double>
struct MBTraits {
typedef MDATA_TYPE mdata_type;
typedef PCOORD_TYPE pcoord_type;
};

template <typename MPTRAITS>
class MBMapping;

/**
 * Parts of the mapping which do not depend on the templated numerical
 * types.
 */
class MBMappingBase {
public:
  virtual ~MBMappingBase() {}

  /**
   *  Is the point in the cell? 
   *sdim = spatial dim, pdim = parametric dim
   * mdata = mapping data (vertex coords)
   * point = point to test
   * pcoord = parametric coordinates returned.
   */
  virtual bool is_in_cell(const double *mdata,
                          const double *point,
                          double *pcoord,
                          double *dist = NULL) const = 0;

  /**
   * Dimension of the range of the mapping.
   */
  virtual UInt spatial_dim() const = 0;
  
  /**
   * Dimension of the domain of the mapping.
   */
  virtual UInt parametric_dim() const = 0;

  virtual const std::string &mname() const = 0;

  virtual MBMapping<MBTraits<> > *operator()(MBTraits<>) const = 0;

};

/**
 * Full mapping abstract class.
 */
template<typename MPTRAITS = MBTraits<> >
class MBMapping : public MBMappingBase {
public:
  virtual ~MBMapping() {}

  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  /**
   *  Apply the mapping to the given parametric points(npts,pdim)
   * return results(npts,sdim).
   * mdata = mapping data
   */
  virtual void forward(const unsigned int npts,
      const mdata_type mdata[],
      const pcoord_type points[],
      typename mbmesh_richest_type<mdata_type,pcoord_type>::value results[]) const = 0;

  /**
   * Find the inverse of the jacobian at the given parametric points.
   * If the domain dimension is less than the range, supplement the
   * columns of the jacobian with a basis for the co-space of the
   * tangent(s), i.e. in 2->3 mapping, the last column will be the normal.
   * Return the inverse of this jacobian.
   * 
   * result(sdim,sdim)
   */
  virtual void jac_inv(const mdata_type mdata[],
      const pcoord_type pcoord[],
      typename mbmesh_richest_type<mdata_type,pcoord_type>::value result[]) const = 0;
  /**
   * Return a normal of the mapping at the given point.  Only applies
   * for mappings where dim(domain) < dim(range).
   */
  virtual void normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename mbmesh_richest_type<mdata_type,pcoord_type>::value result[]) const {
    throw("No normal for non manifold map");
  }

};

/**
 * Implement a mapping type with a shapefunction object
 * Specialize the case when spatial dim == parametric dim
 */
 
template<class SFUNC_TYPE, typename MPTRAITS, int SPATIAL_DIM=SFUNC_TYPE::pdim, int PARAMETRIC_DIM=SPATIAL_DIM>
class MB_POLY_Mapping : public MBMapping<MPTRAITS> {
private:
  // Hide constructor to enforce singleton pattern
  MB_POLY_Mapping(const std::string &_name) :name(_name) {}
public:
  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  static MB_POLY_Mapping *classInstance;
  static MB_POLY_Mapping *instance();
  static const unsigned int sdim = SPATIAL_DIM;
  static const unsigned int pdim = PARAMETRIC_DIM;

  /**
   * See the abstract mapping class for documentation of these functions.
   */
  bool is_in_cell(const double *mdata,
      const double *point,
      double *pcoord,
      double *dist = NULL) const;

  void forward(const unsigned int npts,
      const mdata_type mdata[],
      const pcoord_type points[],
      typename mbmesh_richest_type<mdata_type,pcoord_type>::value results[]) const;

  void jac_inv(const mdata_type mdata[],
      const pcoord_type pcoord[],
      typename mbmesh_richest_type<mdata_type,pcoord_type>::value result[]) const;
                     
  template <typename OMPTRAITS>
  MB_POLY_Mapping<SFUNC_TYPE,OMPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM> *trade() const {
    return MB_POLY_Mapping<SFUNC_TYPE,OMPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::instance();
  }
  
  const std::string name;
  const std::string &mname() const { return name; }

  UInt spatial_dim() const { return SPATIAL_DIM;}
  UInt parametric_dim() const { return PARAMETRIC_DIM;}

  MBMapping<MBTraits<> > *operator()(MBTraits<>) const { return MB_POLY_Mapping<SFUNC_TYPE, MBTraits<>, sdim, pdim>::instance(); }
};

/**
 * Implement a mapping type with a shapefunction object
 * Specialize the case when spatial dim == 3, parametric = 2.
 */
template<class SFUNC_TYPE,typename MPTRAITS>
class MB_POLY_Mapping<SFUNC_TYPE, MPTRAITS, 3,2> : public MBMapping<MPTRAITS> {
private:
  // Hide constructor to enforce singleton pattern
  MB_POLY_Mapping(const std::string &_name) :name(_name) {}
public:
  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  static MB_POLY_Mapping *classInstance;
  static MB_POLY_Mapping *instance();
  static const unsigned int sdim = 3;
  static const unsigned int pdim = 2;

  bool is_in_cell(const double *mdata,
      const double *point,
      double *pcoord,
      double *dist = NULL) const;

  void forward(const unsigned int npts,
      const mdata_type mdata[],
      const pcoord_type points[],
      typename mbmesh_richest_type<mdata_type,pcoord_type>::value results[]) const;

  void normal(UInt npts, const mdata_type mdata[],
      const pcoord_type pcoord[],
      typename mbmesh_richest_type<mdata_type,pcoord_type>::value result[]) const;
  
  void jac_inv(const mdata_type mdata[],
      const pcoord_type pcoord[],
      typename mbmesh_richest_type<mdata_type,pcoord_type>::value result[]) const;
  
  template <typename OMPTRAITS>
  MB_POLY_Mapping<SFUNC_TYPE,OMPTRAITS,3,2> *trade() const {
    return MB_POLY_Mapping<SFUNC_TYPE,OMPTRAITS,3,2>::instance();
  }
  
  UInt spatial_dim() const { return 3;}
  UInt parametric_dim() const {return 2;}

  const std::string name;
  const std::string &mname() const { return name; }

  MBMapping<MBTraits<> > *operator()(MBTraits<>) const { return MB_POLY_Mapping<SFUNC_TYPE, MBTraits<>, sdim, pdim>::instance(); }
};

template <int dim,typename mdata_type>
void MB_POLY_Mapping_jacobian_invert(const mdata_type jac_in[], mdata_type jac_inv[]);

// Factory
struct MBTopo2Map {
MBMappingBase *operator()(const std::string &toponame);
};


///////////////////////////////// OLD //////////////////////////////

class MBElemMap {
private:

  bool tri_is_in(const double pcoord[], double *dist);
  bool quad_is_in(const double pcoord[], double *dist);

public:

  bool cartesian_eval(const double *mdata, const double *point, int num_pnts, double *pcoord, double *dist);
  bool spherical_eval(const double *mdata, const double *point, int num_pnts, double *pcoord, double *dist);
};

} // namespace

#endif
#endif
