// $Id: ESMC_Mapping.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Mapping_h
#define ESMC_Mapping_h

#include <ESMC_MeshTypes.h>
#include <ESMC_MeshObjTopo.h>

#include <ESMC_ShapeFunc.h>

#include <string>

namespace ESMCI {
namespace MESH {

// For sensitivities wrt mapp coords.
template<typename MDATA_TYPE=double, typename PCOORD_TYPE=double>
struct MPTraits {
typedef MDATA_TYPE mdata_type;
typedef PCOORD_TYPE pcoord_type;
};

template <typename MPTRAITS>
class Mapping;

class MappingBase {
public:
  virtual ~MappingBase() {}

  // Is the point in the cell? 
  // sdim = spatial dim, pdim = parametric dim
  // mdata = mapping data (vertex coords)
  // point = point to test
  // pcoord = parametric coordinates returned.
  virtual bool is_in_cell(const double *mdata,
                  const double *point,
                        double *pcoord,
                        double *dist = NULL) const = 0;

  virtual UInt spatial_dim() const = 0;
  virtual UInt parametric_dim() const = 0;

  virtual const std::string &mname() const = 0;

  virtual Mapping<MPTraits<> > *operator()(MPTraits<>) const = 0;
  virtual Mapping<MPTraits<double,fad_type> > *operator()(MPTraits<double,fad_type>) const = 0;
  virtual Mapping<MPTraits<fad_type,double> > *operator()(MPTraits<fad_type,double>) const = 0;
  virtual Mapping<MPTraits<fad_type,fad_type> > *operator()(MPTraits<fad_type,fad_type>) const = 0;

};

template<typename MPTRAITS = MPTraits<> >
class Mapping : public MappingBase {
public:
  virtual ~Mapping() {}

  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  // Apply the mapping to the given parametric points(npts,pdim)
  // return results(npts,sdim).
  // mdata = mapping data
  virtual void forward(const unsigned int npts,
               const mdata_type mdata[],
               const pcoord_type points[],
               typename richest_type<mdata_type,pcoord_type>::value results[]) const = 0;

  // Find the inverse of the jacobian at the given
  // parametric points.
  // result(sdim,sdim)
  virtual void jac_inv(const mdata_type mdata[],
               const pcoord_type pcoord[],
                     typename richest_type<mdata_type,pcoord_type>::value result[]) const = 0;

  virtual void normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename richest_type<mdata_type,pcoord_type>::value result[]) const {
    throw("No mormal for non manifold map");
  }

  // Jacobian weights for integration.  Absolute value is applied.
  // result[npts]
  virtual void Jx(UInt npts, const mdata_type mdata[], const pcoord_type pcoord[],
               typename richest_type<mdata_type,pcoord_type>::value result[]) const = 0;

};




// Implement a mapping type with a shapefunction object
// Specialize the case when spatial dim == parametric dim
template<class SFUNC_TYPE, typename MPTRAITS, int SPATIAL_DIM=SFUNC_TYPE::pdim, int PARAMETRIC_DIM=SPATIAL_DIM>
class POLY_Mapping : public Mapping<MPTRAITS> {
private:
  // Hide constructor to enforce singleton pattern
  POLY_Mapping() {}
public:
  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  static POLY_Mapping *classInstance;
  static POLY_Mapping *instance();
  static const unsigned int sdim = SPATIAL_DIM;
  static const unsigned int pdim = PARAMETRIC_DIM;
  // Is the point in the cell? 
  // sdim = spatial dim, pdim = parametric dim
  // mdata = mapping data (vertex coords)
  // point = point to test
  // pcoord = parametric coordinates returned.
  bool is_in_cell(const double *mdata,
                  const double *point,
                        double *pcoord,
                        double *dist = NULL) const;

  // Apply the mapping to the given parametric points(npts,pdim)
  // return results(npts,sdim).
  // mdata = mapping data
  void forward(const unsigned int npts,
               const mdata_type mdata[],
               const pcoord_type points[],
               typename richest_type<mdata_type,pcoord_type>::value results[]) const;
  // Find the inverse of the jacobian at the given
  // parametric points.
  // result(sdim,sdim)
  void jac_inv(const mdata_type mdata[],
               const pcoord_type pcoord[],
                     typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void Jx(UInt npts, const mdata_type mdata[], const pcoord_type pcoord[], 
             typename richest_type<mdata_type,pcoord_type>::value result[]) const;
  // Give an identical POLY_Mapping, but with the given traits
  template <typename OMPTRAITS>
  POLY_Mapping<SFUNC_TYPE,OMPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM> *trade() const {
    return POLY_Mapping<SFUNC_TYPE,OMPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::instance();
  }

  static const std::string name;
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

// Implement a mapping type with a shapefunction object
// Specialize the case when spatial dim == parametric dim-2
template<class SFUNC_TYPE,typename MPTRAITS>
class POLY_Mapping<SFUNC_TYPE, MPTRAITS, 3,1> : public Mapping<MPTRAITS> {
private:
  // Hide constructor to enforce singleton pattern
  POLY_Mapping() {}
public:
  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  static POLY_Mapping *classInstance;
  static POLY_Mapping *instance();
  static const unsigned int sdim = 3;
  static const unsigned int pdim = 1;
  // Is the point in the cell? 
  // sdim = spatial dim, pdim = parametric dim
  // mdata = mapping data (vertex coords)
  // point = point to test
  // pcoord = parametric coordinates returned.
  bool is_in_cell(const double *mdata,
                  const double *point,
                        double *pcoord,
                        double *dist = NULL) const;

  // Apply the mapping to the given parametric points(npts,pdim)
  // return results(npts,sdim).
  // mdata = mapping data
  void forward(const unsigned int npts,
               const mdata_type mdata[],
               const pcoord_type points[],
               typename richest_type<mdata_type,pcoord_type>::value results[]) const;

  // Find the inverse of the jacobian at the given
  // parametric points.
  // result(sdim,sdim)
  void jac_inv(const mdata_type mdata[],
               const pcoord_type pcoord[],
                     typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void normal(UInt npts, const mdata_type mdata[],
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

  static const std::string name;
  const std::string &mname() const { return name; }
};

// Implement a mapping type with a shapefunction object
// Specialize the case when spatial dim == parametric dim-1
template<class SFUNC_TYPE,typename MPTRAITS>
class POLY_Mapping<SFUNC_TYPE, MPTRAITS, 3,2> : public Mapping<MPTRAITS> {
private:
  // Hide constructor to enforce singleton pattern
  POLY_Mapping() {}
public:
  typedef typename MPTRAITS::mdata_type mdata_type;
  typedef typename MPTRAITS::pcoord_type pcoord_type;

  static POLY_Mapping *classInstance;
  static POLY_Mapping *instance();
  static const unsigned int sdim = 3;
  static const unsigned int pdim = 2;
  // Is the point in the cell? 
  // sdim = spatial dim, pdim = parametric dim
  // mdata = mapping data (vertex coords)
  // point = point to test
  // pcoord = parametric coordinates returned.
  bool is_in_cell(const double *mdata,
                  const double *point,
                        double *pcoord,
                        double *dist = NULL) const;

  // Apply the mapping to the given parametric points(npts,pdim)
  // return results(npts,sdim).
  // mdata = mapping data
  void forward(const unsigned int npts,
               const mdata_type mdata[],
               const pcoord_type points[],
               typename richest_type<mdata_type,pcoord_type>::value results[]) const;

  // Find the inverse of the jacobian at the given
  // parametric points.
  // result(sdim,sdim)
  void jac_inv(const mdata_type mdata[],
               const pcoord_type pcoord[],
                     typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void normal(UInt npts, const mdata_type mdata[],
              const pcoord_type pcoord[],
                    typename richest_type<mdata_type,pcoord_type>::value result[]) const;

  void Jx(UInt npts, const mdata_type mdata[], const pcoord_type pcoord[], 
              typename richest_type<mdata_type,pcoord_type>::value  result[]) const;

  // Give an identical POLY_Mapping, but with the given traits
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

  static const std::string name;
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
} //namespace


#endif
