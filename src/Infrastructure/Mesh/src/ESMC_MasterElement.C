// $Id: ESMC_MasterElement.C,v 1.5 2007/11/28 16:28:02 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <mesh/ESMC_MasterElement.h>

#include <mesh/ESMC_Quadrature.h>

#include <mesh/ESMC_Exception.h>
#include <sacado/Sacado.hpp>

#include <cmath>
#include <map>
#include <iterator>

namespace ESMC {

typedef std::map<UInt, std::vector<int> > DofIdMapType;

void compute_imprint(const MasterElementBase &me, std::vector<int> &res) {

  DofIdMapType dt;

  res.resize(me.num_functions());

  // Loop dofs, inserting
  DofIdMapType::iterator di;
  for (UInt i = 0; i < me.num_functions(); ) {
    const int *dd = me.GetDofDescription(i);

    UInt nval = 1;
    // Count number of dofs with this obj. Assumes list is sorted by (DOF_TYPE, ordinal)
    const int *ddn = dd;
    for (UInt j = 1; i+j < me.num_functions() && ddn[0] == dd[0] && ddn[1] == dd[1]; j++) {
      ddn = me.GetDofDescription(i+j);
      if (ddn[0] != dd[0] || ddn[1] != dd[1]) break;
      nval++;
    }
 
    for (UInt j = 0; j < nval; j++) {
      dd = me.GetDofDescription(i+j);
      res[i+j] = nval;
    }

    i += nval; // move to next dof
  } // for i

}

MasterElementBase::MasterElementBase(const std::string &_name) : name(_name) {
}

int MasterElementBase::GetDofValSet(UInt dof) const {
  return dofvalset[dof];
}

// ********* A template implementation *******
// Powerful in terms of speed, but inflexible in that the shape func class has to 
// be compile time known.
template<class SHAPE_FUNC, class METRAITS>
MasterElementImpl<SHAPE_FUNC,METRAITS>* MasterElementImpl<SHAPE_FUNC,METRAITS>::classInstance = NULL;

template<class SHAPE_FUNC, class METRAITS>
MasterElementImpl<SHAPE_FUNC,METRAITS> *MasterElementImpl<SHAPE_FUNC,METRAITS>::instance()
{
  if (classInstance == NULL)
    classInstance = new MasterElementImpl<SHAPE_FUNC,METRAITS>();
  return classInstance;
}

template <class SHAPE_FUNC, class METRAITS>
MasterElementImpl<SHAPE_FUNC,METRAITS>::MasterElementImpl() :
MasterElement<METRAITS>("SHAPE:<" + SHAPE_FUNC::name + ">")
{
  compute_imprint(*this, this->dofvalset);
}

template <class SHAPE_FUNC, class METRAITS>
MasterElementImpl<SHAPE_FUNC,METRAITS>::~MasterElementImpl()
{
}

template <class SHAPE_FUNC, class METRAITS>
MasterElement<METraits<> > *MasterElementImpl<SHAPE_FUNC,METRAITS>::operator()(METraits<>) const {
  return MasterElementImpl<SHAPE_FUNC,METraits<> >::instance();
}

template <class SHAPE_FUNC, class METRAITS>
MasterElement<METraits<double,fad_type> > *MasterElementImpl<SHAPE_FUNC,METRAITS>::operator()(METraits<double,fad_type>) const {
  return MasterElementImpl<SHAPE_FUNC,METraits<double,fad_type> >::instance();
}

template <class SHAPE_FUNC, class METRAITS>
MasterElement<METraits<fad_type,double> > *MasterElementImpl<SHAPE_FUNC,METRAITS>::operator()(METraits<fad_type,double>) const {
  return MasterElementImpl<SHAPE_FUNC,METraits<fad_type,double> >::instance();
}

template <class SHAPE_FUNC, class METRAITS>
MasterElement<METraits<fad_type,fad_type> > *MasterElementImpl<SHAPE_FUNC,METRAITS>::operator()(METraits<fad_type,fad_type>) const {
  return MasterElementImpl<SHAPE_FUNC,METraits<fad_type,fad_type> >::instance();
}

template <class SHAPE_FUNC, class METRAITS>
void MasterElementImpl<SHAPE_FUNC,METRAITS>::JxW(
  const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping,
  const mdata_type mdata[],
  const intgRule *intg, mdata_type result[]) const
{
  std::vector<mdata_type> Jx(intg->npoints());
  mapping->Jx(intg->npoints(), mdata, intg->locations(), &Jx[0]);

  for (UInt i = 0; i < intg->npoints(); i++) result[i] = intg->weights()[i]*Jx[i];
}

template <class SHAPE_FUNC, class METRAITS>
void MasterElementImpl<SHAPE_FUNC,METRAITS>::shape_grads(UInt npts,
      const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping, 
      const mdata_type mdata[], const double pcoord[], 
      const double param_shape_grads[], mdata_type result[]) const 
{
  // First get the mapping derivatives
  UInt sdim = mapping->spatial_dim();
  std::vector<mdata_type> jac_inv(sdim*sdim);
  for (UInt j = 0; j < npts; j++) {

    // Mapping inv jac
    mapping->jac_inv(mdata, &pcoord[j*pdim], &jac_inv[0]);

    // Now contract
    for (UInt nd = 0; nd < ndofs; nd++) {
      for (UInt i = 0; i < sdim; i++) {
        result[(j*ndofs + nd)*sdim + i] = 0.0;
        // Only do to pdim, not sdim, because assume zero deriv w/rspt d coord
        for (UInt k = 0; k < pdim; k++) {
          //result[(j*ndofs + nd)*sdim + i] += sgrads[nd][k]*jac_inv[k*sdim+i];
          result[(j*ndofs + nd)*sdim + i] += param_shape_grads[(j*ndofs + nd)*pdim+k]*jac_inv[k*sdim+i];
        }
      }
    } // nd
  } // npts
}

template <class SHAPE_FUNC, class METRAITS>
void MasterElementImpl<SHAPE_FUNC,METRAITS>::function_grads(UInt npts, UInt fdim,
   const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping,
    const mdata_type mdata[], const double pcoord[], const field_type fdata[], 
   typename richest_type<mdata_type,field_type>::value result[],
   mdata_type shape_grads[]) const 
{
  UInt sdim = mapping->spatial_dim();

  // Now contract
  for (UInt p = 0; p < npts; p++) {
    for (UInt fd = 0; fd < fdim; fd++) {
      for (UInt d = 0; d < sdim; d++) {
        result[(p*fdim + fd)*sdim + d] = 0;
        for (UInt n = 0; n < ndofs; n++) {
          result[(p*fdim+fd)*sdim+d] += fdata[n*fdim +fd]*shape_grads[(p*ndofs+n)*sdim+d];
        }
      }
    }
  }
}

template <class SHAPE_FUNC, class METRAITS>
void MasterElementImpl<SHAPE_FUNC,METRAITS>::function_curl(UInt npts,
  const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping,
  const mdata_type mdata[], const double pcoord[], const field_type fdata[], 
  typename richest_type<mdata_type,field_type>::value result[],
  mdata_type shape_grads[]) const 
{
  // This is a dumb implementation, since it calculates way to many grads.  TODO optimize.
  UInt sdim = mapping->spatial_dim();
  std::vector<typename richest_type<mdata_type,field_type>::value> fgrads(npts*sdim*sdim);

  if (sdim != 3) throw("function_curl not yet implemented for sdim != 3");

  // First get all gradients of function
  function_grads(npts, sdim, mapping, mdata, pcoord, fdata, &fgrads[0], &shape_grads[0]);

  // Curl is {dyf3-dzf2, dzf1-dxf3,dxf2-dyf1} 
  for (UInt p = 0; p < npts; p++) {
    result[p*sdim+0] = fgrads[(p*sdim+2)*sdim+1] - fgrads[(p*sdim+1)*sdim+2];
    result[p*sdim+1] = fgrads[(p*sdim+0)*sdim+2] - fgrads[(p*sdim+2)*sdim+0];
    result[p*sdim+2] = fgrads[(p*sdim+1)*sdim+0] - fgrads[(p*sdim+0)*sdim+1];
  }
}

template <class SHAPE_FUNC, class METRAITS>
void MasterElementImpl<SHAPE_FUNC,METRAITS>::function_values(
               UInt npts, UInt fdim, const double pcoord[],
                const field_type fdata[], field_type results[]) const
{
  std::vector<double> svals(npts*ndofs);
  SHAPE_FUNC::shape(npts, pcoord, &svals[0]);

  function_values(npts, fdim, pcoord, fdata, results, &svals[0]);

}

template <class SHAPE_FUNC, class METRAITS>
void MasterElementImpl<SHAPE_FUNC,METRAITS>::function_values(
               UInt npts, UInt fdim, const double pcoord[],
               const field_type fdata[], field_type results[],
               double shape_vals[]) const
{
  // contract
  for (UInt j = 0; j < npts; j++) {
   for (UInt f = 0; f < fdim; f++) {
     results[j*fdim + f] = 0;
     for (UInt n = 0; n < ndofs; n++) {
        results[j*fdim + f] += shape_vals[j*ndofs+n]*fdata[n*fdim + f];
      }
    }
  }
}

template <class SHAPE_FUNC, class METRAITS>
const int * MasterElementImpl<SHAPE_FUNC,METRAITS>::GetDofDescription(UInt dof) const {
  return &SHAPE_FUNC::dof_description[dof][0];
}

template <class SHAPE_FUNC, class METRAITS>
void MasterElementImpl<SHAPE_FUNC,METRAITS>::InterpPoints(
  const MappingBase *mapping,
  const double mdata[], double result[]) const
{
  // For now, assume these are all nodal, so we should be using a nodal approach.
  Throw() << "ME:" << this->name << " is a template version, most likely nodal, so use nodes" <<
        " for interplation";
}

template <class SHAPE_FUNC, class METRAITS>
void MasterElementImpl<SHAPE_FUNC,METRAITS>::Interpolate(UInt fdim, const double vals[], double res[]) const {
  Throw() << "ME:" << this->name << " is a template version, most likely nodal, so use nodes" <<
        " for interplation";
}

// Side element specializations
template<class SHAPE_FUNC, class METRAITS>
MasterElement<METRAITS> *MasterElementImpl<SHAPE_FUNC, METRAITS>::side_element(UInt side) const {
  Throw() << "Could not find side element, side=" << side << "ME=" << this->name << std::endl;
}

template<>
MasterElement<METraits<> > *MasterElementImpl<quad_shape_func,METraits<> >::side_element(UInt side) const {
  return MasterElementImpl<bar_shape_func,METraits<> >::instance();
}

template<>
MasterElement<METraits<> > *MasterElementImpl<hex_shape_func,METraits<> >::side_element(UInt side) const {
  return MasterElementImpl<quad_shape_func,METraits<> >::instance();
}

template<>
MasterElement<METraits<> > *MasterElementImpl<tri_shape_func,METraits<> >::side_element(UInt side) const {
  return MasterElementImpl<bar_shape_func,METraits<> >::instance();
}

template<>
MasterElement<METraits<> > *MasterElementImpl<tet_shape_func,METraits<> >::side_element(UInt side) const {
  return MasterElementImpl<tri_shape_func,METraits<> >::instance();
}

#ifdef NOT
// TODO: see if these can be partial specifications covering more cases.
// QUAD sides
template<>
MasterElement<METraits<> > *MasterElementImpl<quad_shape_func,POLY_Mapping<quad_shape_func>::value>, quadq<1>,METraits<>  >::side_element(UInt side) {
  return MasterElementImpl<bar_shape_func,POLY_Mapping<bar_shape_func,ME2MPTraits<METraits<> > >, barq<1>,METraits<>  >::instance();
}
template<>
MasterElement<METraits<> > *MasterElementImpl<quad_shape_func,POLY_Mapping<quad_shape_func>, quadq<2>,METraits<>  >::side_element(UInt side) {
  return MasterElementImpl<bar_shape_func,POLY_Mapping<bar_shape_func,ME2MPTraits<METraits<> > >, barq<2>,METraits<>  >::instance();
}
template<>
MasterElement<METraits<> > *MasterElementImpl<quad_shape_func,POLY_Mapping<quad_shape_func,ME2MPTraits<METraits<> >, quadq<3>,METraits<>  >::side_element(UInt side) {
  return MasterElementImpl<bar_shape_func,POLY_Mapping<bar_shape_func>, barq<3>,METraits<>  >::instance();
}

// SHELL sides
template<>
MasterElement<METraits<> > *MasterElementImpl<quad_shape_func,POLY_Mapping<quad_shape_func,3,2>, quadq<1>,METraits<>  >::side_element(UInt side) {
  return MasterElementImpl<bar_shape_func,POLY_Mapping<bar_shape_func,3,1>, barq<1>,METraits<>  >::instance();
}
template<>
MasterElement<METraits<> > *MasterElementImpl<quad_shape_func,POLY_Mapping<quad_shape_func,3,2>, quadq<2>,METraits<>  >::side_element(UInt side) {
  return MasterElementImpl<bar_shape_func,POLY_Mapping<bar_shape_func,3,1>, barq<2>,METraits<>  >::instance();
}
template<>
MasterElement<METraits<> > *MasterElementImpl<quad_shape_func,POLY_Mapping<quad_shape_func,3,2>, quadq<3>,METraits<>  >::side_element(UInt side) {
  return MasterElementImpl<bar_shape_func,POLY_Mapping<bar_shape_func,3,1>, barq<3>,METraits<>  >::instance();
}
#endif



// Also provides Explicit Instantiations!!
template<typename METRAITS,UInt q>
MasterElement<METRAITS> *Topo2ME<METRAITS,q>::operator()(const std::string &name) {

  // _L selects the higher order mapping, the low order field

  if (name == "QUAD" || name == "QUAD4") {
      return MasterElementImpl<quad_shape_func, METRAITS >::instance();
  }
  if (name == "QUAD9") {
      return MasterElementImpl<quad9_shape_func, METRAITS >::instance();
  } else if (name == "TRI" || name == "TRI3") {
      return MasterElementImpl<tri_shape_func, METRAITS >::instance();
  }
  else if (name == "SHELL3" || name == "SHELL3_L") {
      return MasterElementImpl<tri_shape_func, METRAITS >::instance();
  }
  else if (name == "SHELL" || name == "SHELL4" || name == "SHELL_L" || name == "SHELL4_L") {
      return MasterElementImpl<quad_shape_func, METRAITS >::instance();
  }
  else if (name == "SHELL9_L") {
      return MasterElementImpl<quad_shape_func, METRAITS >::instance();
  }
  else if (name == "SHELL9") {
      return MasterElementImpl<quad9_shape_func, METRAITS >::instance();
  }
  else if (name == "HEX" || name == "HEX_L" || name == "HEX8") {
    return MasterElementImpl<hex_shape_func, METRAITS >::instance();
  }
  else if (name == "TETRA" || name == "TETRA4" || name == "TETRA_L") {
//TODO create the correct integration rule here!!!
    return MasterElementImpl<tet_shape_func, METRAITS >::instance();
  }
  else if (name == "BAR2_3D") {
    return MasterElementImpl<bar_shape_func, METRAITS >::instance();
  }

  // Simply for the purpose of instantiation
  MasterElementImpl<dg0_shape_func<1>, METRAITS>::instance();
  MasterElementImpl<dg0_shape_func<2>, METRAITS>::instance();
  MasterElementImpl<dg0_shape_func<3>, METRAITS>::instance();

  Throw() << "Cant find master element, topo=" << name << ", integration=" << q;

}


template struct Topo2ME<METraits<>,1>;
template struct Topo2ME<METraits<>,2>;
template struct Topo2ME<METraits<>,3>;
template struct Topo2ME<METraits<>,4>;

typedef METraits<fad_type, double> fad_metraits1; // field sens, no map
typedef METraits<fad_type, fad_type> fad_metraits2; // field and map sens
typedef METraits<double, fad_type> fad_metraits3; // no field sens, map sens
template struct Topo2ME<fad_metraits1,1>;
template struct Topo2ME<fad_metraits1,2>;
template struct Topo2ME<fad_metraits1,3>;
template struct Topo2ME<fad_metraits1,4>;
template struct Topo2ME<fad_metraits2,1>;
template struct Topo2ME<fad_metraits2,2>;
template struct Topo2ME<fad_metraits2,3>;
template struct Topo2ME<fad_metraits2,4>;
template struct Topo2ME<fad_metraits3,1>;
template struct Topo2ME<fad_metraits3,2>;
template struct Topo2ME<fad_metraits3,3>;
template struct Topo2ME<fad_metraits3,4>;


template class  MasterElementImpl<dg0_shape_func<2>, METraits<> >;
template class  MasterElementImpl<dg0_shape_func<2>, fad_metraits1 >;
template class  MasterElementImpl<dg0_shape_func<2>, fad_metraits2>;
template class  MasterElementImpl<dg0_shape_func<2>, fad_metraits3>;

template class  MasterElementImpl<dg0_shape_func<3>, METraits<> >;
template class  MasterElementImpl<dg0_shape_func<3>, fad_metraits1 >;
template class  MasterElementImpl<dg0_shape_func<3>, fad_metraits2>;
template class  MasterElementImpl<dg0_shape_func<3>, fad_metraits3>;

} // namespace
