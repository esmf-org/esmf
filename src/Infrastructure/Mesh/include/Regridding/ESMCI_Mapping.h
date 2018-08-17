// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Mapping_h
#define ESMCI_Mapping_h

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Regridding/ESMCI_ShapeFunc.h>

#include <string>

namespace ESMCI {


  // Type used to specify method used to do spherical (3,2) mapping
  // eventually could be broadened to specify other types
  enum MAP_TYPE {MAP_TYPE_CART_APPROX=0, MAP_TYPE_GREAT_CIRCLE};
  extern MAP_TYPE sph_map_type;

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
struct MPTraits {
typedef MDATA_TYPE mdata_type;
typedef PCOORD_TYPE pcoord_type;
};

template <typename MPTRAITS>
class Mapping;

/**
 * Parts of the mapping which do not depend on the templated numerical
 * types.
 */
class MappingBase {
public:
  virtual ~MappingBase() {}

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

  /**
   * Routines to switch between trait types that preserve
   * the underlying mapping type.
   */
  virtual Mapping<MPTraits<> > *operator()(MPTraits<>) const = 0;
  virtual Mapping<MPTraits<double,fad_type> > *operator()(MPTraits<double,fad_type>) const = 0;
  virtual Mapping<MPTraits<fad_type,double> > *operator()(MPTraits<fad_type,double>) const = 0;
  virtual Mapping<MPTraits<fad_type,fad_type> > *operator()(MPTraits<fad_type,fad_type>) const = 0;

};

/**
 * Full mapping abstract class.
 */
template<typename MPTRAITS = MPTraits<> >
class Mapping : public MappingBase {
public:
  virtual ~Mapping() {}

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
               typename richest_type<mdata_type,pcoord_type>::value results[]) const = 0;

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
                     typename richest_type<mdata_type,pcoord_type>::value result[]) const = 0;

  virtual const Mapping<MPTRAITS> *side_mapping(UInt side_num) const {
    Throw() << "Side mapping not implemented for this map";
  }

  /**
   * Return a normal of the mapping at the given point.  Only applies
   * for mappings where dim(domain) < dim(range).
   */
  virtual void normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename richest_type<mdata_type,pcoord_type>::value result[]) const {
    throw("No mormal for non manifold map");
  }

  /**
   * Same as above, but normalize the normal to length 1
   */
  void unit_normal(UInt npts, const mdata_type mdata[],
                const pcoord_type pcoord[],
                      typename richest_type<mdata_type,pcoord_type>::value result[]) const
 {
    UInt sdim = this->spatial_dim();

    // get normals
    this->normal(npts, mdata, pcoord, result);

    // normalize
    for (UInt p = 0; p < npts; p++) {
      typename richest_type<mdata_type,pcoord_type>::value rnorm_i = 0;
      for (UInt i = 0; i < sdim; i++) rnorm_i += result[p*sdim+i]*result[p*sdim+i];
      rnorm_i = 1.0/std::sqrt(rnorm_i);
      for (UInt i = 0; i < sdim; i++) result[p*sdim+i] *= rnorm_i;
    }
 }
  
  /**
   * Jacobian weights for integration.  Absolute value is applied.
   * result[npts]
   */
  virtual void Jx(UInt npts, const mdata_type mdata[], const pcoord_type pcoord[],
               typename richest_type<mdata_type,pcoord_type>::value result[]) const = 0;

};




/**
 * Implement a mapping type with a shapefunction object
 * Specialize the case when spatial dim == parametric dim
 */
template<class SFUNC_TYPE, typename MPTRAITS, int SPATIAL_DIM=SFUNC_TYPE::pdim, int PARAMETRIC_DIM=SPATIAL_DIM>
class POLY_Mapping : public Mapping<MPTRAITS> {
private:
  // Hide constructor to enforce singleton pattern
  POLY_Mapping(const std::string &_name) :name(_name) {}
public:
  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  static POLY_Mapping *classInstance;
  static POLY_Mapping *instance();
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
               typename richest_type<mdata_type,pcoord_type>::value results[]) const;

  void jac_inv(const mdata_type mdata[],
               const pcoord_type pcoord[],
                     typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  const Mapping<MPTRAITS> *side_mapping(UInt side_num) const;

  void Jx(UInt npts, const mdata_type mdata[], const pcoord_type pcoord[], 
             typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  template <typename OMPTRAITS>
  POLY_Mapping<SFUNC_TYPE,OMPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM> *trade() const {
    return POLY_Mapping<SFUNC_TYPE,OMPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::instance();
  }

  const std::string name;
  const std::string &mname() const { return name; }

  UInt spatial_dim() const { return SPATIAL_DIM;}
  UInt parametric_dim() const { return PARAMETRIC_DIM;}

  Mapping<MPTraits<> > *operator()(MPTraits<>) const { return POLY_Mapping<SFUNC_TYPE, MPTraits<>, sdim, pdim>::instance(); }
  Mapping<MPTraits<double,fad_type> > *operator()(MPTraits<double,fad_type>) const 
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<double, fad_type>, sdim, pdim>::instance(); }
  Mapping<MPTraits<fad_type,double> > *operator()(MPTraits<fad_type,double>) const
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<fad_type,double>, sdim, pdim>::instance(); }
  Mapping<MPTraits<fad_type,fad_type> > *operator()(MPTraits<fad_type,fad_type>) const
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<fad_type,fad_type>, sdim, pdim>::instance(); }
};

/**
 *  Implement a mapping type with a shapefunction object
 *  Specialize the case when spatial dim 3, parametric = 1.
 */
template<class SFUNC_TYPE,typename MPTRAITS>
class POLY_Mapping<SFUNC_TYPE, MPTRAITS, 3,1> : public Mapping<MPTRAITS> {
private:
  // Hide constructor to enforce singleton pattern
  POLY_Mapping(const std::string &_name) : name(_name) {}
public:
  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  static POLY_Mapping *classInstance;
  static POLY_Mapping *instance();
  static const unsigned int sdim = 3;
  static const unsigned int pdim = 1;

  bool is_in_cell(const double *mdata,
                  const double *point,
                        double *pcoord,
                        double *dist = NULL) const;

  void forward(const unsigned int npts,
               const mdata_type mdata[],
               const pcoord_type points[],
               typename richest_type<mdata_type,pcoord_type>::value results[]) const;

  void jac_inv(const mdata_type mdata[],
               const pcoord_type pcoord[],
                     typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename richest_type<mdata_type,pcoord_type>::value result[]) const;
  
  void unit_normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void Jx(UInt npts, const mdata_type mdata[], const pcoord_type pcoord[], 
            typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  
  
  template <typename OMPTRAITS>
  POLY_Mapping<SFUNC_TYPE,OMPTRAITS,3,1> *trade() const {
    return POLY_Mapping<SFUNC_TYPE,OMPTRAITS,3,1>::instance();
  }
  UInt spatial_dim() const { return 3;}
  UInt parametric_dim() const {return 1;}

  Mapping<MPTraits<> > *operator()(MPTraits<>) const { return POLY_Mapping<SFUNC_TYPE, MPTraits<>, sdim, pdim>::instance(); }
  Mapping<MPTraits<double,fad_type> > *operator()(MPTraits<double,fad_type>) const 
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<double, fad_type>, sdim, pdim>::instance(); }
  Mapping<MPTraits<fad_type,double> > *operator()(MPTraits<fad_type,double>) const
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<fad_type,double>, sdim, pdim>::instance(); }
  Mapping<MPTraits<fad_type,fad_type> > *operator()(MPTraits<fad_type,fad_type>) const
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<fad_type,fad_type>, sdim, pdim>::instance(); }

  const std::string name;
  const std::string &mname() const { return name; }
};

/**
 * Implement a mapping type with a shapefunction object
 * Specialize the case when spatial dim == 3, parametric = 2.
 */
template<class SFUNC_TYPE,typename MPTRAITS>
class POLY_Mapping<SFUNC_TYPE, MPTRAITS, 3,2> : public Mapping<MPTRAITS> {
private:
  // Hide constructor to enforce singleton pattern
  POLY_Mapping(const std::string &_name) :name(_name) {}
public:
  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  static POLY_Mapping *classInstance;
  static POLY_Mapping *instance();
  static const unsigned int sdim = 3;
  static const unsigned int pdim = 2;

  bool is_in_cell(const double *mdata,
                  const double *point,
                        double *pcoord,
                        double *dist = NULL) const;

  void forward(const unsigned int npts,
               const mdata_type mdata[],
               const pcoord_type points[],
               typename richest_type<mdata_type,pcoord_type>::value results[]) const;

  void jac_inv(const mdata_type mdata[],
               const pcoord_type pcoord[],
                     typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename richest_type<mdata_type,pcoord_type>::value result[]) const;
  
  void unit_normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void Jx(UInt npts, const mdata_type mdata[], const pcoord_type pcoord[], 
              typename richest_type<mdata_type,pcoord_type>::value  result[]) const;

  template <typename OMPTRAITS>
  POLY_Mapping<SFUNC_TYPE,OMPTRAITS,3,2> *trade() const {
    return POLY_Mapping<SFUNC_TYPE,OMPTRAITS,3,2>::instance();
  }
  UInt spatial_dim() const { return 3;}
  UInt parametric_dim() const {return 2;}

  Mapping<MPTraits<> > *operator()(MPTraits<>) const { return POLY_Mapping<SFUNC_TYPE, MPTraits<>, sdim, pdim>::instance(); }
  Mapping<MPTraits<double,fad_type> > *operator()(MPTraits<double,fad_type>) const 
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<double, fad_type>, sdim, pdim>::instance(); }
  Mapping<MPTraits<fad_type,double> > *operator()(MPTraits<fad_type,double>) const
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<fad_type,double>, sdim, pdim>::instance(); }
  Mapping<MPTraits<fad_type,fad_type> > *operator()(MPTraits<fad_type,fad_type>) const
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<fad_type,fad_type>, sdim, pdim>::instance(); }

  const std::string name;
  const std::string &mname() const { return name; }
};

/**
 * Implement a mapping type with a shapefunction object
 * Specialize the case when spatial dim == 2, parametric = 1.
 */
template<class SFUNC_TYPE,typename MPTRAITS>
class POLY_Mapping<SFUNC_TYPE, MPTRAITS, 2,1> : public Mapping<MPTRAITS> {
private:
  // Hide constructor to enforce singleton pattern
  POLY_Mapping(const std::string &_name) :name(_name) {}
public:
  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  static POLY_Mapping *classInstance;
  static POLY_Mapping *instance();
  static const unsigned int sdim = 2;
  static const unsigned int pdim = 1;

  bool is_in_cell(const double *mdata,
                  const double *point,
                        double *pcoord,
                        double *dist = NULL) const;

  void forward(const unsigned int npts,
               const mdata_type mdata[],
               const pcoord_type points[],
               typename richest_type<mdata_type,pcoord_type>::value results[]) const;

  void jac_inv(const mdata_type mdata[],
               const pcoord_type pcoord[],
                     typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename richest_type<mdata_type,pcoord_type>::value result[]) const;
  
  void unit_normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void Jx(UInt npts, const mdata_type mdata[], const pcoord_type pcoord[], 
              typename richest_type<mdata_type,pcoord_type>::value  result[]) const;

  template <typename OMPTRAITS>
  POLY_Mapping<SFUNC_TYPE,OMPTRAITS,2,1> *trade() const {
    return POLY_Mapping<SFUNC_TYPE,OMPTRAITS,2,1>::instance();
  }
  UInt spatial_dim() const { return 2;}
  UInt parametric_dim() const {return 1;}

  Mapping<MPTraits<> > *operator()(MPTraits<>) const { return POLY_Mapping<SFUNC_TYPE, MPTraits<>, sdim, pdim>::instance(); }
  Mapping<MPTraits<double,fad_type> > *operator()(MPTraits<double,fad_type>) const 
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<double, fad_type>, sdim, pdim>::instance(); }
  Mapping<MPTraits<fad_type,double> > *operator()(MPTraits<fad_type,double>) const
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<fad_type,double>, sdim, pdim>::instance(); }
  Mapping<MPTraits<fad_type,fad_type> > *operator()(MPTraits<fad_type,fad_type>) const
    { return POLY_Mapping<SFUNC_TYPE, MPTraits<fad_type,fad_type>, sdim, pdim>::instance(); }

  const std::string name;
  const std::string &mname() const { return name; }
};

template <int dim,typename mdata_type>
void POLY_Mapping_jacobian_invert(const mdata_type jac_in[], mdata_type jac_inv[]);

template <int dim,typename mdata_type>
mdata_type POLY_Mapping_determinant(const mdata_type jac_in[]);

// Factory
struct Topo2Map {
MappingBase *operator()(const std::string &toponame);
};

} //namespace


#endif
