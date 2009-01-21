// $Id: ESMC_MasterElement.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MasterElement_h
#define ESMC_MasterElement_h

#include <ESMC_ShapeFunc.h>
#include <ESMC_Mapping.h>
#include <ESMC_MeshTypes.h>
#include <ESMC_Quadrature.h>

#include <ESMC_MeshTypes.h>

#include <string>

namespace ESMCI {
namespace MESH {

// A type to determine the various types in a master element
template<typename FIELD_TYPE=double, typename MDATA_TYPE=double>
struct METraits {
typedef FIELD_TYPE field_type;
typedef MDATA_TYPE mdata_type;
};


// Convert master element to mapping traits.
template<typename METRAITS>
struct ME2MPTraits {
typedef MPTraits<typename METRAITS::mdata_type,double> value;
};

template <typename METRAITS>
class MasterElement;
// Topology and integration order
template <typename METRAITS=METraits<>,UInt q=2>
struct Topo2ME {
MasterElement<METRAITS> *operator()(const std::string &tname);
};

class MasterElementBase {
protected:
  MasterElementBase(const std::string &_name);
  virtual ~MasterElementBase() {}
public:

  virtual UInt num_functions() const = 0;

  virtual UInt IntgOrder() const = 0; // suggested order of integration

  // Return the master element for a side (may be different by side; for instance prisms)
  virtual MasterElementBase *side_element(UInt side) = 0;

  // Return a tuple (DOF_OBJTYPE, ordinal, index, polarity)
  // nval corresponds to the context and gather field "fieldname_dof#nvalset"
  virtual const int *GetDofDescription(UInt dof) const = 0;

  // We partition fields by how much data to store per topological object.  This partitions
  // the topo objs into sets by how many indices on the object.  For a given dof, this
  // functions returns how many total dofs live on the topological object.
  int GetDofValSet(UInt dof) const;

  // Declare the interpolation points.  To interpolate to coefficient values for this
  // ME, one must provide values of the function to interpolate at the given points.
  // Interpolation points are provided in physical coordinates.
  // Note, if the me is_nodal, interpolation is accomplished merely by providing
  // values at the nodes.
  virtual UInt NumInterpPoints() const = 0;
  // result(ninterp*sdim)
  virtual void InterpPoints(const MappingBase *mapping,
                const double mdata[], // mapping data
                double result[]) const = 0;

  // For now don't include the mapping.  Assume the interpolation can be done in parametric space
  // vals(ndof,fdim)
  // res(ndof,fdim)
  virtual void Interpolate(const double vals[], double res[]) const = 0;


  // convert an me to different scalar types
  virtual MasterElement<METraits<> > *operator()(METraits<>) const = 0;
  virtual MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const = 0;
  virtual MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const = 0;
  virtual MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const = 0;

  // True if dofs are all on nodes, else false
  virtual bool is_nodal() const = 0;

  const std::string name;
  protected:
  std::vector<int> dofvalset; // dof layout

};

template<class METRAITS = METraits<> >
class MasterElement : public MasterElementBase {
public:
  MasterElement(const std::string &_name) : MasterElementBase(_name)
  {}

  virtual ~MasterElement() {}

  typedef METRAITS traits_type;
  typedef typename METRAITS::field_type field_type;
  typedef typename METRAITS::mdata_type mdata_type;

  // Query for all the shape function values (npts) at the parametric points
  // pcoord(npts, pdim) and return in result(npts, nfunc)
  virtual void shape_function(UInt npts, const double pcoord[],
          double result[]) const = 0;

  // Get physical gradients.  mdata = mapping data
  // result(npts, nfunc, pdim)
  virtual void shape_grads(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[], const double pcoord[],
             mdata_type result[]) const = 0;

  // Interpolate function to point
  virtual void function_values(UInt npts, UInt fdim, const double pcoord[],
             const field_type fdata[], field_type results[]) const {
    interpolate_point(npts, fdim, pcoord, fdata, results);
  }
  // Get physical gradients.  mdata = mapping data
  // fdata(ndofs,fdim)
  // result(fdim, sdim)
  virtual void function_grads(UInt npts, UInt fdim, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
           const mdata_type mdata[], const double pcoord[],
           const field_type fdata[],
           typename richest_type<mdata_type,field_type>::value result[]) const = 0;

  // Compute curl of a vector field.
  // mdata = mapping
  // pcoord (coordinates to evaluate at)
  // fdata(ndofs, sdim)
  // result(sdim)
  virtual void function_curl(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
             const double pcoord[], const field_type fdata[],
             typename richest_type<mdata_type,field_type>::value result[]) const = 0;

  // Interpolate a field
  // pcoord(npts,pdim)
  // fdata(npts,fdim)
  // results(npts,fdim)
  virtual void interpolate_point(UInt npts, UInt fdim, const double pcoord[],
             const field_type fdata[], field_type results[]) const = 0;

  // For now, don't template this, as it doesn't return anything (really)
  virtual bool is_in_cell(const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const double *mdata,
                  const double *point,
                        double *pcoord,
                        double *dist = NULL) const = 0;

  // Calculate the normal for a point
  // result(sdim)
  // User must decide whether this normal is inward or outward
  virtual void unit_normal(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
                  const mdata_type mdata[], const double pcoord[],
                  mdata_type result[]) const = 0;

  virtual void normal(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
                  const mdata_type mdata[], const double pcoord[],
                  mdata_type result[]) const = 0;

  // Get tangent at intg points (line elements)
  //virtual void Itangent(const double mdata[], double result[]);

  virtual void JxW(const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
            const intgRule *intg,
            mdata_type result[]) const = 0;

  // Return a tuple (DOF_OBJTYPE, ordinal, index, nval_set)
  // nval corresponds to the context and gather field "fieldname_dof#nvalset"
  virtual const int *GetDofDescription(UInt dof) const = 0;


  // convert an me to different scalar types
  virtual MasterElement<METraits<> > *operator()(METraits<>) const = 0;
  virtual MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const = 0;
  virtual MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const = 0;
  virtual MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const = 0;

};


// An implementation based on generic programming.  Piece together the Mapping,
// shapefunctions and quadrature rule based on template pieces.
template<class SHAPE_FUNC, class METRAITS = METraits<> >
class MasterElementImpl : public MasterElement<METRAITS> {
private:
  MasterElementImpl();
  ~MasterElementImpl();
public:
  typedef typename METRAITS::field_type field_type;
  typedef typename METRAITS::mdata_type mdata_type;

  UInt num_functions() const { return ndofs; }

  virtual UInt IntgOrder() const { return SHAPE_FUNC::iorder; }

  static MasterElementImpl *classInstance;
  static MasterElementImpl *instance();

  static const UInt pdim = SHAPE_FUNC::pdim;
  static const UInt ndofs = SHAPE_FUNC::ndofs;

  typedef SHAPE_FUNC shape_func_type;

  // Query for all the shape function values (npts) at the parametric points
  // pcoord(npts, pdim) and return in result(npts, nfunc)
  void shape_function(UInt npts, const double pcoord[], double result[]) const {
    SHAPE_FUNC::shape(npts, pcoord, result);
  }

  // Get physical gradients.  mdata = mapping data
  // result(npts, nfunc, sdim)
  void shape_grads(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
          const double pcoord[], mdata_type result[]) const;

  // Get physical gradients.  mdata = mapping data
  // result(npts,fdim, sdim)
  void function_grads(UInt npts, UInt fdim, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
              const mdata_type mdata[], const double pcoord[],
              const field_type fdata[], 
              typename richest_type<mdata_type,field_type>::value result[]) const;

  // Curl of a vector function
  // result(npts,sdim)
  void function_curl(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, 
             const mdata_type mdata[], const double pcoord[],
             const field_type fdata[], 
             typename richest_type<mdata_type,field_type>::value result[]) const;

  // Interpolate a field
  // pcoord(npts,pdim)
  // fdata(ndofs,fdim)
  // results(npts,fdim)
  void interpolate_point(
               UInt npts, UInt fdim, const double pcoord[],
                const field_type fdata[], field_type results[]) const
  {
    std::vector<double> svals(npts*ndofs);
    SHAPE_FUNC::shape(npts, pcoord, &svals[0]);
  
    // contract
    for (UInt j = 0; j < npts; j++) {
     for (UInt f = 0; f < fdim; f++) {
       results[j*fdim + f] = 0;
       for (UInt n = 0; n < ndofs; n++) {
          results[j*fdim + f] += svals[j*ndofs+n]*fdata[n*fdim + f];
        }
      }
    }
  }
  
  bool is_in_cell(const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
                  const double *mdata,
                  const double *point,
                        double *pcoord,
                        double *dist = NULL) const {
    return mapping->is_in_cell(mdata, point, pcoord, dist);
  }

  // unit normal (surface elements)
  // result(npts,sdim)
  void unit_normal(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
           const mdata_type mdata[], const double pcoord[],
           mdata_type result[]) const;

  void normal(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
           const mdata_type mdata[], const double pcoord[],
           mdata_type result[]) const;

  // Get tangent at intg points (line elements)
  //void Itangent(const double mdata[], double result[]);

  // Return the master element for a side (may be different by side; for instance prisms)
  MasterElement<METRAITS> *side_element(UInt side);

  void JxW(const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
           const intgRule *irule, mdata_type result[]) const;

  const int *GetDofDescription(UInt dof) const;

  UInt NumInterpPoints() const { return SHAPE_FUNC::NumInterp; }
  // result(ninterp*sdim)
  void InterpPoints(const MappingBase *mapping,
                const double mdata[], // mapping data
                double result[]) const;
  void Interpolate(const double vals[], double res[]) const;

  MasterElement<METraits<> > *operator()(METraits<>) const;
  MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const;
  MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const;
  MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const;

  bool is_nodal() const { return SHAPE_FUNC::is_nodal(); }

  private:

};

void compute_imprint(const MasterElementBase &me, std::vector<int> &res);

} //namespace
} //namespace

#endif
