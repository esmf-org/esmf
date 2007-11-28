// $Id: ESMC_MasterElementV.h,v 1.3 2007/11/28 16:23:22 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_MasterElementV_h
#define ESMC_MasterElementV_h

#include <mesh/ESMC_MasterElement.h>
#include <map>
#include <string>

/**
 * We implement the master element interface by using inheritance.
 * This system uses a virtual ShapeFunction class, which, once
 * implemented, can be extended to a full master element by the
 * routines herein.
 */
namespace ESMC {

/**
 * Master elements implemented by using a virtual shape function
 * object.
 * @ingroup mesystem
 */
template<class METRAITS = METraits<> >
class MasterElementV : public MasterElement<METRAITS> {
private:
  MasterElementV(const ShapeFunc *shape);
  ~MasterElementV();
  
  /**
   * Pointer to the shape function object used by this ME.
   */
  const ShapeFunc *m_shape;
  static std::map<std::string, MasterElementV*> meVMap;
public:
  
  /**
   * Singleton constructor.  The objects are stashed by
   * the name of the shape function, so calling this again
   * with the same name will return a pre-cached object.
   */ 
  static MasterElementV *instance(const ShapeFunc *shape);
  
  typedef typename METRAITS::field_type field_type;
  typedef typename METRAITS::mdata_type mdata_type;
  
  /**
   * For documentation of the following functions, see the MasterElement
   * base class (this is called comment reuse).
   */

  UInt num_functions() const { return m_shape->NumFunctions(); }

  UInt IntgOrder() const { return m_shape->IntgOrder(); }

  void shape_function(UInt npts, const double pcoord[], double result[]) const {
    m_shape->shape(npts, pcoord, result);
  }

  void param_shape_grads(UInt npts, const double pcoord[], double result[]) const {
    m_shape->shape_grads(npts, pcoord, result);
  }
  
  void shape_grads(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
          const double pcoord[], const double param_shape_grads[], mdata_type result[]) const;

  void function_grads(UInt npts, UInt fdim, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping,
              const mdata_type mdata[], const double pcoord[],
              const field_type fdata[], 
              typename richest_type<mdata_type,field_type>::value result[],
              mdata_type shape_grads[]) const;

  void function_curl(UInt npts, const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, 
             const mdata_type mdata[], const double pcoord[],
             const field_type fdata[], 
             typename richest_type<mdata_type,field_type>::value result[],
             mdata_type shape_grads[]) const;
             

  void function_values(
               UInt npts, UInt fdim, const double pcoord[],
                const field_type fdata[], field_type results[]) const;

  void function_values(
               UInt npts, UInt fdim, const double pcoord[],
               const field_type fdata[], field_type results[],
               double shape_vals[]) const;
 
  MasterElement<METRAITS> *side_element(UInt side) const;

  void JxW(const Mapping<typename ME2MPTraits<METRAITS>::value > *mapping, const mdata_type mdata[],
           const intgRule *irule, mdata_type result[]) const;

  const int *GetDofDescription(UInt dof) const;

  UInt NumInterpPoints() const { return m_shape->NumInterp(); }

  void InterpPoints(const MappingBase *mapping,
                const double mdata[], // mapping data
                double result[]) const;

  void Interpolate(UInt fdim, const double vals[], double res[]) const;

  MasterElement<METraits<> > *operator()(METraits<>) const;
  MasterElement<METraits<double,fad_type> > *operator()(METraits<double,fad_type>) const;
  MasterElement<METraits<fad_type,double> > *operator()(METraits<fad_type,double>) const;
  MasterElement<METraits<fad_type,fad_type> > *operator()(METraits<fad_type,fad_type>) const;

  bool is_nodal() const { return m_shape->is_nodal(); }
  
  UInt orientation() const { return m_shape->orientation(); }

  private:

};


} // namespace

#endif
