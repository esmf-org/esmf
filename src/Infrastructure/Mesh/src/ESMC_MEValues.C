// $Id: ESMC_MEValues.C,v 1.1.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MEValues.h>
#include <ESMC_Exception.h>
#include <ESMC_MeshUtils.h>
#include <ESMC_MeshField.h>

#include <cstddef> // NULL
#include <vector>

namespace ESMCI {
namespace MESH {

template<typename METRAITS,typename CMETRAITS, typename FIELD,typename CFIELD>
MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::MEValues(const MEFamily &_fmef, const CFIELD &_cf) :
cur_ker(NULL),
fmef(_fmef),
cmef(_cf.GetMEFamily()),
topo(NULL),
meptr(NULL),
cmeptr(NULL),
update_flag(0),
sf(),
sfg(),
jxw(),
nqpoints(0),
cur_elem(NULL),
sdim(0),
cf(&_cf),
irule(NULL),
mapping(NULL)
{
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::Setup(const MeshObj &obj, UInt uflag,
      const intgRule *ir, const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{
  Setup(*obj.GetKernel(), uflag, ir, map);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::Setup(const Kernel &ker, UInt uflag,
      const intgRule *ir, const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{
  cur_ker = &ker;
  meptr = fmef.getME(cur_ker->GetTopo()->name, METRAITS());
  cmeptr = GetME(*cf, *cur_ker)(CMETRAITS());
  update_flag = uflag;
  irule = ir;
  if (map) {
    mapping = map;
  } else mapping = cur_ker->GetMapping()(typename ME2MPTraits<METRAITS>::value());

  ThrowRequire(irule || uflag == 0);

  nqpoints = irule ? irule->npoints() : 0;
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::ReInit(const MeshObj &elem) {
  sdim = GetMeshObjMesh(elem).spatial_dim();
  cur_elem = &elem;


  if (update_flag & MEV::update_sf) {
    sf.clear(); sf.resize(nqpoints*meptr->num_functions());
    meptr->shape_function(nqpoints, irule->locations(), &sf[0]);
  }
  std::vector<mdata_type> mdata; // possible reuse
  if (update_flag & MEV::update_sfg) {
    mdata.resize(cmeptr->num_functions()*sdim);
    GatherElemData(*cmeptr, *cf, elem, &mdata[0]);
                                  
    sfg.clear(); sfg.resize(nqpoints*sdim*meptr->num_functions());
    meptr->shape_grads(nqpoints, mapping, &mdata[0], 
               irule->locations(), &sfg[0]);
  }
  if (update_flag & MEV::update_jxw) {
    if (!(update_flag & MEV::update_sfg)) {
      mdata.resize(cmeptr->num_functions()*sdim);
      GatherElemData(*cmeptr, *cf, elem, &mdata[0]);
    } // else already filled in above
    
    jxw.resize(nqpoints);
    meptr->JxW(mapping, &mdata[0], irule, &jxw[0]);
  }
}

// TODO: make this query the master element as to where field coeffs live.
template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetFunctionValues(const FIELD &field, field_type values[]) {
  std::vector<field_type> fdata(field.dim()*meptr->num_functions());

  // Gather step.  Nodal for now.
  GatherElemData(*meptr, field, *cur_elem, &fdata[0]);

  meptr->function_values(nqpoints, field.dim(), irule->locations(), &fdata[0], &values[0]);

}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetCoordinateValues(mdata_type values[]) {
  std::vector<mdata_type> mdata; 
  mdata.resize(cmeptr->num_functions()*sdim);
  GatherElemData(*cmeptr, *cf, *cur_elem, &mdata[0]);
  cmeptr->function_values(nqpoints, cf->dim(), irule->locations(), &mdata[0], &values[0]);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetInterpolationPoints(double values[]) {
  Throw() << "this function only implemented for standard double,double case:";
}

template<>
void MEValues<METraits<>, METraits<>,MEField<>,MEField<> >::GetInterpolationPoints(double values[]) {
  std::vector<double> mdata; 
  mdata.resize(cmeptr->num_functions()*sdim);
  GatherElemData(*cmeptr, *cf, *cur_elem, &mdata[0]);
  meptr->InterpPoints(mapping, &mdata[0], values);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::Interpolate(const FIELD &f, const double fvals[]) {
  Throw() << "Interpolate only operatoes on METraits<double,double>";
}

template<>
void MEValues<METraits<>,METraits<>,MEField<>,MEField<> >::Interpolate(const MEField<> &f, const double fvals[]) {
  std::vector<double> mcoef(meptr->num_functions()*f.dim());
  meptr->Interpolate(fvals, &mcoef[0]);
  // Put data onto mesh
  ScatterElemData(*meptr, f, *cur_elem, &mcoef[0]);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetFunctionGrads(const FIELD &field, field_type grads[]) {
  std::vector<mdata_type> mdata; 
  std::vector<field_type> fdata; 
  mdata.resize(cmeptr->num_functions()*sdim);
  GatherElemData(*cmeptr, *cf, *cur_elem, &mdata[0]);
  fdata.resize(meptr->num_functions()*field.dim());
  GatherElemData(*meptr, field, *cur_elem, &fdata[0]);
  meptr->function_grads(nqpoints, field.dim(), mapping, &mdata[0], irule->locations(), &fdata[0], &grads[0]);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetFunctionCurls(const FIELD &field, field_type curls[]) {
  if (field.dim() != sdim)
    Throw() << "For curl field dim=" << field.dim() <<  "must be sdim=" << sdim;
  std::vector<mdata_type> mdata; 
  std::vector<field_type> fdata; 
  mdata.resize(cmeptr->num_functions()*sdim);
  GatherElemData(*cmeptr, *cf, *cur_elem, &mdata[0]);
  fdata.resize(meptr->num_functions()*field.dim());
  GatherElemData(*meptr, field, *cur_elem, &fdata[0]);
  meptr->function_curl(nqpoints, mapping, &mdata[0], irule->locations(), &fdata[0], &curls[0]);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetUnitNormals(mdata_type unit_normals[]) {
  std::vector<mdata_type> mdata; 
  mdata.resize(cmeptr->num_functions()*sdim);
  GatherElemData(*cmeptr, *cf, *cur_elem, &mdata[0]);
  meptr->unit_normal(nqpoints, mapping, &mdata[0], irule->locations(), &unit_normals[0]);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetNormals(mdata_type normals[]) {
  std::vector<mdata_type> mdata; 
  mdata.resize(cmeptr->num_functions()*sdim);
  GatherElemData(*cmeptr, *cf, *cur_elem, &mdata[0]);
  meptr->normal(nqpoints, mapping, &mdata[0], irule->locations(), &normals[0]);
}



template class MEValues<METraits<>, METraits<>, MEField<> >;
template class MEValues<METraits<fad_type>, METraits<>, MEField<SField> >;


} // namespace
} // namespace
