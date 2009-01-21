// $Id: ESMC_MasterElementV.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_MasterElementV.h>

#include <iostream>
#include <iterator>

namespace ESMCI {
namespace MESH {


template <class METRAITS>
std::map<std::string, MasterElementV<METRAITS>*>  MasterElementV<METRAITS>::meVMap;

template <class METRAITS>
MasterElementV<METRAITS> *MasterElementV<METRAITS>::instance(const ShapeFunc *shape) {
  typename std::map<std::string, MasterElementV<METRAITS>*>::iterator mi =
    meVMap.find(shape->name());
  MasterElementV *me;
  if (mi == meVMap.end()) {
    me = new MasterElementV(shape);
    meVMap[shape->name()] = me;
  } else me = mi->second;
  
  return me;
}

template <class METRAITS>
MasterElementV<METRAITS>::MasterElementV(
                                   const ShapeFunc *shape) :
MasterElement<METRAITS>(" SHAPE:<" + shape->name() + ">"),
m_shape(shape),
me1(NULL),
me2(NULL),
me3(NULL),
me4(NULL)
{
  compute_imprint(*this, this->dofvalset);

std::cout << "ME:" << this->name << ", imprint:" << std::endl;
for (UInt i = 0; i < num_functions(); i++) {
  const int *dd = GetDofDescription(i);
  std::cout << " (" << dd[0] << ", " << dd[1] << ", " << dd[2] << ") valset:" << this->GetDofValSet(i) << std::endl;
}

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
      const mdata_type mdata[], const double pcoord[], mdata_type result[]) const 
{
  // First get the mapping derivatives
  UInt ndofs = m_shape->NumFunctions();
  UInt pdim = mapping->parametric_dim();
  std::vector<double> sgrads(ndofs*pdim);
  UInt sdim = mapping->spatial_dim();
  std::vector<mdata_type> jac_inv(sdim*sdim);
  for (UInt j = 0; j < npts; j++) {
    // Get shape function derivs
    m_shape->shape_grads(1, &pcoord[j*pdim], &sgrads[0]);

    // Mapping inv jac
    mapping->jac_inv(mdata, &pcoord[j*pdim], &jac_inv[0]);

    // Now contract
    for (UInt nd = 0; nd < ndofs; nd++) {
      for (UInt i = 0; i < sdim; i++) {
        result[(j*ndofs + nd)*sdim + i] = 0.0;
        // Only do to pdim, not sdim, because assume zero deriv w/rspt d coord
        for (UInt k = 0; k < pdim; k++) {
          result[(j*ndofs + nd)*sdim + i] += sgrads[nd*pdim+k]*jac_inv[k*sdim+i];
        }
      }
    } // nd
  } // npts
}

template <class METRAITS>
void MasterElementV<METRAITS>::function_grads(UInt npts, UInt fdim,
   const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping,
    const mdata_type mdata[], const double pcoord[], const field_type fdata[], 
   typename richest_type<mdata_type,field_type>::value result[]) const 
{
  UInt sdim = mapping->spatial_dim();
  UInt ndofs = m_shape->NumFunctions();
  std::vector<mdata_type> sgrads(npts*ndofs*sdim);

  this->shape_grads(npts, mapping, mdata, pcoord, &sgrads[0]);

  // Now contract
  for (UInt p = 0; p < npts; p++) {
    for (UInt fd = 0; fd < fdim; fd++) {
      for (UInt d = 0; d < sdim; d++) {
        result[(p*fdim + fd)*sdim + d] = 0;
        for (UInt n = 0; n < ndofs; n++) {
          result[(p*fdim+fd)*sdim+d] += fdata[n*fdim +fd]*sgrads[(p*ndofs+n)*sdim+d];
        }
      }
    }
  }
}

template <class METRAITS>
void MasterElementV<METRAITS>::function_curl(UInt npts,
  const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping,
  const mdata_type mdata[], const double pcoord[], const field_type fdata[], 
  typename richest_type<mdata_type,field_type>::value result[]) const 
{
  // This is a dumb implementation, since it calculates way to many grads.  TODO optimize.
  UInt sdim = mapping->spatial_dim();
  std::vector<typename richest_type<mdata_type,field_type>::value> fgrads(npts*sdim*sdim);

  if (sdim != 3) throw("function_curl not yet implemented for sdim != 3");

  // First get all gradients of function
  function_grads(npts, sdim, mapping, mdata, pcoord, fdata, &fgrads[0]);

  // Curl is {dyf3-dzf2, dzf1-dxf3,dxf2-dyf1} 
  for (UInt p = 0; p < npts; p++) {
    result[p*sdim+0] = fgrads[(p*sdim+2)*sdim+1] - fgrads[(p*sdim+1)*sdim+2];
    result[p*sdim+1] = fgrads[(p*sdim+0)*sdim+2] - fgrads[(p*sdim+2)*sdim+0];
    result[p*sdim+2] = fgrads[(p*sdim+1)*sdim+0] - fgrads[(p*sdim+0)*sdim+1];
  }
}

template <class METRAITS>
void MasterElementV<METRAITS>::normal(UInt npts, 
  const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping,
  const mdata_type mdata[], const double pcoord[], mdata_type result[]) const 
{
  // get normals
  mapping->normal(npts, mdata, pcoord, result);

}

template <class METRAITS>
void MasterElementV<METRAITS>::unit_normal(UInt npts, 
  const Mapping<typename ME2MPTraits<METRAITS>::value> *mapping,
  const mdata_type mdata[], const double pcoord[], mdata_type result[]) const 
{
  UInt sdim = mapping->spatial_dim();

  // get normals
  mapping->normal(npts, mdata, pcoord, result);

  // normalize
  for (UInt p = 0; p < npts; p++) {
    mdata_type rnorm_i = 0;
    for (UInt i = 0; i < sdim; i++) rnorm_i += result[p*sdim+i]*result[p*sdim+i];
    rnorm_i = 1.0/std::sqrt(rnorm_i);
    for (UInt i = 0; i < sdim; i++) result[p*sdim+i] *= rnorm_i;
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
void MasterElementV<METRAITS>::Interpolate(const double vals[], double res[]) const
{
  m_shape->Interpolate(vals, res);
}

// Side element specializations
template<class METRAITS>
MasterElement<METRAITS> *MasterElementV<METRAITS>::side_element(UInt side) {
  Throw() << "Could not find side element, side=" << side << "ME=" << this->name << std::endl;
}

template class MasterElementV<METraits<> >;
template class MasterElementV<METraits<fad_type,double> >;
template class MasterElementV<METraits<double,fad_type> >;
template class MasterElementV<METraits<fad_type,fad_type> >;

} // namespace 
} // namespace 
