// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Legacy/ESMCI_MasterElement.h>
#include <Mesh/include/Legacy/ESMCI_SFuncAdaptor.h>

#include <Mesh/include/Legacy/ESMCI_Quadrature.h>

#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/sacado/Sacado_No_Kokkos.hpp>

#include <cmath>
#include <map>
#include <iterator>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

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

/*------------------------------------------------------------------*/
// Virtual master element implementation
/*------------------------------------------------------------------*/

template <class METRAITS>
std::map<std::string, MasterElementV<METRAITS>*> &get_meVMap() {
  static std::map<std::string, MasterElementV<METRAITS>*>  meVMap;

  return meVMap;
}

template <class METRAITS>
MasterElementV<METRAITS> *MasterElementV<METRAITS>::instance(const ShapeFunc *shape) {
  typename std::map<std::string, MasterElementV<METRAITS>*>::iterator mi =
    get_meVMap<METRAITS>().find(shape->name());
  MasterElementV *me;
  if (mi == get_meVMap<METRAITS>().end()) {
    me = new MasterElementV(shape);
    get_meVMap<METRAITS>()[shape->name()] = me;
  } else me = mi->second;
  
  return me;
}

template <class METRAITS>
MasterElementV<METRAITS>::MasterElementV(
                                   const ShapeFunc *shape) :
MasterElement<METRAITS>(" SHAPE:<" + shape->name() + ">"),
m_shape(shape)
{
  compute_imprint(*this, this->dofvalset);

  /*
std::cout << "ME:" << this->name << ", imprint:" << std::endl;
for (UInt i = 0; i < num_functions(); i++) {
  const int *dd = GetDofDescription(i);
  std::cout << " (" << dd[0] << ", " << dd[1] << ", " << dd[2] << ") valset:" << this->GetDofValSet(i) << std::endl;
}
*/

}

template <class METRAITS>
MasterElementV<METRAITS>::~MasterElementV()
{
}

template <class METRAITS>
MasterElement<METraits<> > *MasterElementV<METRAITS>::operator()(METraits<>) const {
  return MasterElementV<METraits<> >::instance(m_shape);
}

template <class METRAITS>
MasterElement<METraits<fad_type,double> > *MasterElementV<METRAITS>::operator()(METraits<fad_type,double>) const {
  return MasterElementV<METraits<fad_type,double> >::instance(m_shape);
}

template <class METRAITS>
MasterElement<METraits<double,fad_type> > *MasterElementV<METRAITS>::operator()(METraits<double,fad_type>) const {
  return MasterElementV<METraits<double,fad_type> >::instance(m_shape);
}

template <class METRAITS>
MasterElement<METraits<fad_type,fad_type> > *MasterElementV<METRAITS>::operator()(METraits<fad_type,fad_type>) const {
  return MasterElementV<METraits<fad_type,fad_type> >::instance(m_shape);
}

template <class METRAITS>
void MasterElementV<METRAITS>::JxW(
  const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping,
  const mdata_type mdata[],
  const intgRule *intg, mdata_type result[]) const
{
  std::vector<mdata_type> Jx(intg->npoints());
  mapping->Jx(intg->npoints(), mdata, intg->locations(), &Jx[0]);

  for (UInt i = 0; i < intg->npoints(); i++) result[i] = intg->weights()[i]*Jx[i];
}
template <class METRAITS>
void MasterElementV<METRAITS>::shape_grads(UInt npts,
      const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping, 
      const mdata_type mdata[], const double pcoord[], const double param_shape_grads[],
      mdata_type result[]) const 
{
  // First get the mapping derivatives
  UInt ndofs = m_shape->NumFunctions();
  UInt pdim = mapping->parametric_dim();
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
          result[(j*ndofs + nd)*sdim + i] += param_shape_grads[(j*ndofs+nd)*pdim+k]*jac_inv[k*sdim+i];
        }
      }
    } // nd
  } // npts
}

template <class METRAITS>
void MasterElementV<METRAITS>::function_grads(UInt npts, UInt fdim,
   const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping,
    const mdata_type mdata[], const double pcoord[], const field_type fdata[], 
   typename richest_type<mdata_type,field_type>::value result[],
   mdata_type shape_grads[]) const 
{
  UInt sdim = mapping->spatial_dim();
  UInt ndofs = m_shape->NumFunctions();

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

template <class METRAITS>
void MasterElementV<METRAITS>::function_curl(UInt npts,
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
  function_grads(npts, sdim, mapping, mdata, pcoord, fdata, &fgrads[0], shape_grads);

  // Curl is {dyf3-dzf2, dzf1-dxf3,dxf2-dyf1} 
  for (UInt p = 0; p < npts; p++) {
    result[p*sdim+0] = fgrads[(p*sdim+2)*sdim+1] - fgrads[(p*sdim+1)*sdim+2];
    result[p*sdim+1] = fgrads[(p*sdim+0)*sdim+2] - fgrads[(p*sdim+2)*sdim+0];
    result[p*sdim+2] = fgrads[(p*sdim+1)*sdim+0] - fgrads[(p*sdim+0)*sdim+1];
  }
}

template <class METRAITS>
void MasterElementV<METRAITS>::function_values(
               UInt npts, UInt fdim, const double pcoord[],
                const field_type fdata[], field_type results[]) const
{
  UInt ndofs = m_shape->NumFunctions();
  std::vector<double> svals(npts*ndofs);
  m_shape->shape(npts, pcoord, &svals[0]);

  function_values(npts, fdim, pcoord, fdata, results, &svals[0]);

}

template <class METRAITS>
void MasterElementV<METRAITS>::function_values(
               UInt npts, UInt fdim, const double pcoord[],
               const field_type fdata[], field_type results[],
               double shape_vals[]) const
{
  UInt ndofs = m_shape->NumFunctions();

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

template <class METRAITS>
const int * MasterElementV<METRAITS>::GetDofDescription(UInt dof) const {
  return &m_shape->DofDescriptionTable()[dof*4];
}

template <class METRAITS>
void MasterElementV<METRAITS>::InterpPoints(
  const MappingBase *mapping,
  const double mdata[], double result[]) const
{
  // Get the parametric points:
  UInt np = m_shape->NumInterp();
  Mapping<> *mp = (*mapping)(MPTraits<>());
  const double *pcoord = m_shape->InterpPoints();
  mp->forward(np, mdata, pcoord, result);
}

template <class METRAITS>
const double *MasterElementV<METRAITS>::InterpPoints() const
{
  return m_shape->InterpPoints();
}

template <class METRAITS>
void MasterElementV<METRAITS>::Interpolate(UInt fdim, const field_type vals[], field_type res[]) const
{
  UInt nip = m_shape->NumInterp();
  UInt nfunc = num_functions();
  
  std::vector<field_type> ires(nfunc);
  
  for (UInt d = 0; d < fdim; d++) {
    m_shape->Interpolate(&vals[d*nip], &ires[0]);
    
    for (UInt n = 0; n < nfunc; ++n) {
      res[n*fdim+d] = ires[n];
    }
  }

}

// Side element specializations
template<class METRAITS>
MasterElement<METRAITS> *MasterElementV<METRAITS>::side_element(UInt side) const {

  return MasterElementV<METRAITS>::instance(m_shape->side_shape(side));

}

template class MasterElementV<METraits<> >;
template class MasterElementV<METraits<fad_type,double> >;
template class MasterElementV<METraits<double,fad_type> >;
template class MasterElementV<METraits<fad_type,fad_type> >;


// Topo2ME implementation
template<typename METRAITS>
MasterElement<METRAITS> *Topo2ME<METRAITS>::operator()(const std::string &name) {

  // _L selects the higher order mapping, the low order field

  if (name == "QUAD" || name == "QUAD4" || name == "QUAD_L" || name == "QUAD_3D_L") {
      return MasterElementV<METRAITS>::instance(SFuncAdaptor<quad_shape_func>::instance());
  }
  if (name == "QUAD9") {
      return MasterElementV<METRAITS>::instance(SFuncAdaptor<quad9_shape_func>::instance());
  } else if (name == "TRI" || name == "TRI3" || name == "TRI_L" || name == "TRI3_3D" || name == "TRI3_L") { // Added missing "TRI3_L" 
      return MasterElementV<METRAITS>::instance(SFuncAdaptor<tri_shape_func>::instance());
  }
  else if (name == "SHELL3" || name == "SHELL3_L") {
      return MasterElementV<METRAITS>::instance(SFuncAdaptor<tri_shape_func>::instance());
  }
  else if (name == "SHELL" || name == "SHELL4" ||
           name == "SHELL_L" || name == "SHELL4_L" || name == "QUAD_3D") {
      return MasterElementV<METRAITS>::instance(SFuncAdaptor<quad_shape_func>::instance());
  }
  else if (name == "SHELL9_L") {
      return MasterElementV<METRAITS>::instance(SFuncAdaptor<quad_shape_func>::instance());
  }
  else if (name == "SHELL9") {
      return MasterElementV<METRAITS>::instance(SFuncAdaptor<quad9_shape_func>::instance());
  }
  else if (name == "HEX" || name == "HEX_L" || name == "HEX8") {
      return MasterElementV<METRAITS>::instance(SFuncAdaptor<hex_shape_func>::instance());
  }
  else if (name == "TETRA" || name == "TETRA4" || name == "TETRA_L") {
//TODO create the correct integration rule here!!!
    return MasterElementV<METRAITS>::instance(SFuncAdaptor<tet_shape_func>::instance());
  }
  else if (name == "BAR2_3D") {
    return MasterElementV<METRAITS>::instance(SFuncAdaptor<bar_shape_func>::instance());
  }

  MasterElementV<METRAITS>::instance(SFuncAdaptor<dg0_shape_func<1> >::instance());
  MasterElementV<METRAITS>::instance(SFuncAdaptor<dg0_shape_func<2> >::instance());
  MasterElementV<METRAITS>::instance(SFuncAdaptor<dg0_shape_func<3> >::instance());


  Throw() << "Cant find master element, topo=" << name;

}


template struct Topo2ME<METraits<> >;

typedef METraits<fad_type, double> fad_metraits1; // field sens, no map
typedef METraits<fad_type, fad_type> fad_metraits2; // field and map sens
typedef METraits<double, fad_type> fad_metraits3; // no field sens, map sens
template struct Topo2ME<fad_metraits1>;
template struct Topo2ME<fad_metraits2>;
template struct Topo2ME<fad_metraits3>;

} // namespace
