// $Id: ESMC_MEValues.C,v 1.2 2007/11/28 16:28:02 dneckels Exp $
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
#include <mesh/ESMC_MEValues.h>
#include <mesh/ESMC_Exception.h>
#include <mesh/ESMC_MeshUtils.h>
#include <mesh/ESMC_MeshField.h>
#include <mesh/ESMC_ParEnv.h>

#include <cstddef> // NULL
#include <vector>
#include <algorithm>
#include <functional>
#include <numeric>

namespace ESMC {

template<typename METRAITS,typename CMETRAITS, typename FIELD,typename CFIELD>
MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::MEValues(const MEFamily &_fmef, const CFIELD *_cf) :
cur_ker(NULL),
fmef(_fmef),
cmef(_cf ? &_cf->GetMEFamily() : static_cast<const MEFamily*>(0)),
topo(NULL),
meptr(NULL),
cmeptr(NULL),
update_flag(0),
sf(),
sfg(),
jxw(),
cur_elem(NULL),
sdim(0),
cf(_cf),
irule(NULL),
mapping(NULL),
num_irules(0),
cur_rule(0),
cur_sf_offset(0),
cur_sfg_offset(0),
cur_psfg_offset(0),
cur_jxw_offset(0),
total_nqpoints(0)
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
  std::vector<const intgRule*> irules;
  if (ir) irules.push_back(ir);

  Setup(ker, uflag, irules, map);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::Setup(const Kernel &ker, UInt uflag,
      std::vector<const intgRule*> irules,
      const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{

  topo = ker.GetTopo();
  sdim = topo->spatial_dim;
  cur_ker = &ker;
  meptr = fmef.getME(cur_ker->GetTopo()->name, METRAITS());
  if (cf) cmeptr = GetME(*cf, *cur_ker)(CMETRAITS());
  update_flag = uflag;

  irule_list.clear();
  std::copy(irules.begin(), irules.end(), std::back_inserter(irule_list));

  num_irules = irule_list.size();

  total_nqpoints = 0;
  for (UInt q = 0; q < num_irules; q++)
    total_nqpoints += irule_list[q]->npoints();

  if (map) {
    mapping = map;
  } else mapping = cur_ker->GetMapping()(typename ME2MPTraits<METRAITS>::value());

  ThrowRequire(num_irules > 0 || uflag == 0);

  // Shape functions depend only on the integration rule, not
  // the element, so we can calculate them once here, for the whole kernel.
  
  if (update_flag & MEV::update_sf) {
    sf.clear(); sf.resize(total_nqpoints*meptr->num_functions());
    UInt off = 0;
    for (UInt q = 0; q < num_irules; q++) {
      meptr->shape_function(irule_list[q]->npoints(),
                irule_list[q]->locations(), &sf[off]);
      off += irule_list[q]->npoints()*meptr->num_functions();
    }
/*
Par::Out() << "Shape functions:" << std::endl;
std::copy(sf.begin(), sf.end(), std::ostream_iterator<double>(Par::Out(), " "));
Par::Out() << std::endl;
*/
  }
  
  if (update_flag & MEV::update_sfg) {
    param_sfg.resize(total_nqpoints*meptr->num_functions()*topo->parametric_dim);
    UInt off = 0;
    for (UInt q = 0; q < num_irules; q++) {
      meptr->param_shape_grads(irule_list[q]->npoints(),
           irule_list[q]->locations(), &param_sfg[off]);

      off += irule_list[q]->npoints()*meptr->num_functions()*topo->parametric_dim;
    }

    sfg.clear(); sfg.resize(total_nqpoints*sdim*meptr->num_functions());
  }

  if (update_flag & MEV::update_jxw) {
    // Can only resize the buffer.  Must be computed element by element.
    jxw.resize(total_nqpoints);
  }

  irule = 0;
  cur_rule = 0;
  cur_sf_offset = cur_sfg_offset = cur_psfg_offset = cur_jxw_offset = 0;
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::ReInit(const MeshObj &elem, UInt rule_num) {

  ThrowRequire(num_irules == 0 || rule_num < num_irules);

  sdim = GetMeshObjMesh(elem).spatial_dim();
  
  // Update offsets
  if (num_irules > 0) {
    cur_rule = rule_num;
    cur_sf_offset = cur_sfg_offset = cur_psfg_offset = cur_jxw_offset = 0;
    for (UInt q = 0; q < rule_num; q++) {
      cur_sf_offset +=  irule_list[q]->npoints()*meptr->num_functions();
      cur_sfg_offset +=  irule_list[q]->npoints()*meptr->num_functions()*sdim;
      cur_psfg_offset +=  irule_list[q]->npoints()*meptr->num_functions()*topo->parametric_dim;
      cur_jxw_offset +=  irule_list[q]->npoints();
    }

    irule = irule_list[rule_num];
  }

  cur_elem = &elem;

  std::vector<mdata_type> mdata; // possible reuse

  // Note: shape functions were computed above, in Setup.

  if (update_flag & MEV::update_sfg) {
    ThrowRequire(cmeptr);
    mdata.resize(cmeptr->num_functions()*sdim);
    GatherElemData<CMETRAITS,CFIELD>(*cmeptr, *cf, elem, &mdata[0]);
                                  
    meptr->shape_grads(irule->npoints(), mapping, &mdata[0], 
               irule->locations(),
               &param_sfg[cur_psfg_offset], &sfg[cur_sfg_offset]);
  }
 
  if (update_flag & MEV::update_jxw) {
    compute_jxw(elem, mdata);
  }

}

// Standard element implementation
template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::compute_jxw(const MeshObj &elem, std::vector<mdata_type> &mdata) {

  if (!(update_flag & MEV::update_sfg)) {

    ThrowRequire(cmeptr);
    mdata.resize(cmeptr->num_functions()*sdim);
    GatherElemData<CMETRAITS,CFIELD>(*cmeptr, *cf, elem, &mdata[0]);

  } // else already filled in above
  
  ThrowAssert(jxw.size() == irule->npoints());
  cmeptr->JxW(mapping, &mdata[0], irule, &jxw[cur_jxw_offset]);

}

// TODO: make this query the master element as to where field coeffs live.
template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetFunctionValues(const FIELD &field, field_type values[]) {

  if (meptr->orientation() == MasterElementBase::ME_DG) {

    GetFunctionValues(field, (field_type*)field.data(*cur_elem), values);

  } else {
    std::vector<field_type> fdata(field.dim()*meptr->num_functions());

    // Gather step.  Nodal for now.
    GatherElemData<METRAITS,FIELD>(*meptr, field, *cur_elem, &fdata[0]);

    GetFunctionValues(field, &fdata[0], values);

  }

}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetElemData(const FIELD &field, field_type values[]) {

  if (meptr->orientation() == MasterElementBase::ME_DG) {

    ThrowRequire(update_flag & MEV::update_sf);

    field_type *dt = field.data(*cur_elem);
    std::copy(dt, dt+meptr->num_functions()*field.dim(), values);
    
    
  } else {
    
    // Gather step.
    GatherElemData<METRAITS,FIELD>(*meptr, field, *cur_elem, values);

  }
  
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetCoordinateValues(mdata_type values[]) {
  
  std::vector<mdata_type> mdata; 
  
  mdata.resize(cmeptr->num_functions()*sdim);
  
  GatherElemData<CMETRAITS,CFIELD>(*cmeptr, *cf, *cur_elem, &mdata[0]);
  
  cmeptr->function_values(irule->npoints(), cf->dim(), irule->locations(), &mdata[0], &values[0]);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetInterpolationPoints(double values[]) {
  Throw() << "this function only implemented for standard double,double case:";
}

template<>
void MEValues<METraits<>, METraits<>,MEField<>,MEField<> >::GetInterpolationPoints(double values[]) {
 
  std::vector<double> mdata; 
  
  mdata.resize(cmeptr->num_functions()*sdim);
  
  GatherElemData<>(*cmeptr, *cf, *cur_elem, &mdata[0]);
  
  meptr->InterpPoints(mapping, &mdata[0], values);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::Interpolate(const FIELD &f, const double fvals[]) {
  Throw() << "Interpolate only operatoes on METraits<double,double>";
}

template<>
void MEValues<METraits<>,METraits<>,MEField<>,MEField<> >::Interpolate(const MEField<> &f, const double fvals[]) {
  
  if (meptr->orientation() == MasterElementBase::ME_DG) {
    
    meptr->Interpolate(f.dim(), fvals, f.data(*cur_elem));
    
  } else {
    
    std::vector<double> mcoef(meptr->num_functions()*f.dim());
    
    meptr->Interpolate(f.dim(), fvals, &mcoef[0]);
    
    // Put data onto mesh
    ScatterElemData(*meptr, f, *cur_elem, &mcoef[0]);
  }
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetFunctionGrads(const FIELD &field, field_type grads[]) {
  std::vector<field_type> fdata; 

  fdata.resize(meptr->num_functions()*field.dim());
  GatherElemData<METRAITS,FIELD>(*meptr, field, *cur_elem, &fdata[0]);

  GetFunctionGrads(field, &fdata[0], grads);

}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetFunctionCurls(const FIELD &field, field_type curls[]) {
  if (field.dim() != sdim)
    Throw() << "For curl field dim=" << field.dim() <<  "must be sdim=" << sdim;
  std::vector<mdata_type> mdata; 
  std::vector<field_type> fdata; 

  mdata.resize(cmeptr->num_functions()*sdim);
  GatherElemData<CMETRAITS,CFIELD>(*cmeptr, *cf, *cur_elem, &mdata[0]);

  fdata.resize(meptr->num_functions()*field.dim());
  GatherElemData<METRAITS,FIELD>(*meptr, field, *cur_elem, &fdata[0]);

  ThrowRequire(update_flag & MEV::update_sfg);

  meptr->function_curl(irule->npoints(), mapping, &mdata[0],
             irule->locations(), &fdata[0], &curls[0], &sfg[cur_sfg_offset]);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetUnitNormals(mdata_type unit_normals[]) {
  std::vector<mdata_type> mdata; 
  mdata.resize(cmeptr->num_functions()*sdim);
  GatherElemData<CMETRAITS,CFIELD>(*cmeptr, *cf, *cur_elem, &mdata[0]);
  mapping->unit_normal(irule->npoints(), &mdata[0], irule->locations(), &unit_normals[0]);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::GetNormals(mdata_type normals[]) {
  std::vector<mdata_type> mdata; 
  mdata.resize(cmeptr->num_functions()*sdim);
  GatherElemData<CMETRAITS,CFIELD>(*cmeptr, *cf, *cur_elem, &mdata[0]);
  mapping->normal(irule->npoints(), &mdata[0], irule->locations(), &normals[0]);
}



template class MEValues<METraits<>, METraits<>, MEField<> >;
template class MEValues<METraits<fad_type>, METraits<>, MEField<SField> >;


/*---------------------------------------------------------*/
// MESideValues class
/*---------------------------------------------------------*/
template<typename METRAITS,typename CMETRAITS, typename FIELD,typename CFIELD>
MESideValues<METRAITS,CMETRAITS,FIELD,CFIELD>::MESideValues(const MEFamily &_fmef, const CFIELD *_cf) :
  MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>(_fmef, _cf),
  side_cmeptr(0),
  side_mapping(0)
{
}

template<typename METRAITS,typename CMETRAITS, typename FIELD,typename CFIELD>
MESideValues<METRAITS,CMETRAITS,FIELD,CFIELD>::~MESideValues()
{
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MESideValues<METRAITS,CMETRAITS,FIELD,CFIELD>::Setup(const MeshObj &obj, UInt uflag,
      const intgRule *ir, const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{
  this->Setup(*obj.GetKernel(), uflag, ir, map);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MESideValues<METRAITS,CMETRAITS,FIELD,CFIELD>::Setup(const Kernel &ker, UInt uflag,
      const intgRule *ir, const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{

  // Setup the side integration rules;
  const sideIntgFactory *sintg = sideIntgFactory::instance(ker.GetTopo()->name, ir);

  std::vector<const intgRule*> irules;

  for (UInt s = 0; s < ker.GetTopo()->num_sides; s++)
    irules.push_back(sintg->GetSideRule(s));

  // Call the element setup
  MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::Setup(ker, uflag, irules, map);

}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MESideValues<METRAITS,CMETRAITS,FIELD,CFIELD>::ReInit(const MeshObj &elem, UInt side_num) {

  ThrowRequire(side_num < this->topo->num_sides);

  // Get side cmeptr, side_mapping
  ThrowRequire(this->cmeptr);
  side_cmeptr = this->cmeptr->side_element(side_num)->operator()(CMETRAITS());

  ThrowRequire(this->mapping);
  side_mapping = this->mapping->side_mapping(side_num);

  cur_side = side_num;

  MEValues<METRAITS,CMETRAITS,FIELD,CFIELD>::ReInit(elem, side_num);
}

template<typename METRAITS,typename CMETRAITS,typename FIELD,typename CFIELD>
void MESideValues<METRAITS,CMETRAITS,FIELD,CFIELD>::compute_jxw(const MeshObj &elem, std::vector<typename METRAITS::mdata_type> &mdata) {

  mdata.resize(side_cmeptr->num_functions()*this->sdim);
  GatherSideData<CMETRAITS,CFIELD>(*side_cmeptr, *this->cf, elem, cur_side, &mdata[0]);

Par::Out() << "mdata_jxw: cur_jxw_off=" << this->cur_jxw_offset <<std::endl;
std::copy(mdata.begin(), mdata.end(), std::ostream_iterator<typename METRAITS::mdata_type>(Par::Out(), " "));
Par::Out() << std::endl;
  
  side_cmeptr->JxW(side_mapping, &mdata[0], this->irule, &this->jxw[this->cur_jxw_offset]);

}

template class MESideValues<METraits<>, METraits<>, MEField<> >;
template class MESideValues<METraits<fad_type>, METraits<>, MEField<SField> >;


} // namespace
