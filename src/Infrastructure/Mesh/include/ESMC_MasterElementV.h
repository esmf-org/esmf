// $Id: ESMC_MasterElementV.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MasterElementV_h
#define ESMC_MasterElementV_h

#include <ESMC_MasterElement.h>
#include <map>
#include <string>

// Virtual master elements.  A master element implementation using inheritence.
namespace ESMCI {
namespace MESH {

// An implementation using pointers and inheritence for shape_funcs, mapping,
template<class METRAITS = METraits<> >
class MasterElementV : public MasterElement<METRAITS> {
private:
  MasterElementV(const ShapeFunc *shape);
  ~MasterElementV();
  const ShapeFunc *m_shape;
  static std::map<std::string, MasterElementV*> meVMap;
public:
  // tables these by name
  static MasterElementV *instance(const ShapeFunc *shape);
  friend class MEFamilyHier;
  typedef typename METRAITS::field_type field_type;
  typedef typename METRAITS::mdata_type mdata_type;

  UInt num_functions() const { return m_shape->NumFunctions(); }

  UInt IntgOrder() const { return m_shape->IntgOrder(); }


  // Query for all the shape function values (npts) at the parametric points
  // pcoord(npts, pdim) and return in result(npts, nfunc)
  void shape_function(UInt npts, const double pcoord[], double result[]) const {
    m_shape->shape(npts, pcoord, result);
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
    UInt ndofs = m_shape->NumFunctions();
    std::vector<double> svals(npts*ndofs);
    m_shape->shape(npts, pcoord, &svals[0]);
  
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

  UInt NumInterpPoints() const { return m_shape->NumInterp(); }
  // result(ninterp*sdim)
  void InterpPoints(const MappingBase *mapping,
                const double mdata[], // mapping data
                double result[]) const;

  void Interpolate(const double vals[], double res[]) const;

  MasterElement<METraits<> > *operator()(METraits<>) const;
  MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const;
  MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const;
  MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const;

  bool is_nodal() const { return m_shape->is_nodal(); }

  private:
  // Since we don't use instances in this implementation, cache
  // the other types of me to avoid proliferation of instances.
  MasterElement<METraits<> > *me1;
  MasterElement<METraits<fad_type, double> > *me2;
  MasterElement<METraits<double, fad_type> > *me3;
  MasterElement<METraits<fad_type, fad_type> > *me4;

};


} // namespace
} // namespace

#endif
