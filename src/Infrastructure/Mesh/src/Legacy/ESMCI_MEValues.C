// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Legacy/ESMCI_MEValues.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Regridding/ESMCI_ShapeFunc.h>

#include <cstddef> // NULL
#include <vector>
#include <algorithm>
#include <functional>
#include <numeric>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

UInt mev_round_to_dword(UInt size) {
  UInt dwsz = sizeof(void*)*4;
  UInt rm = size % dwsz;
  return rm ? size + (dwsz - rm) : size;
}

template<typename METRAITS, typename FIELD>
MEValues<METRAITS,FIELD>::MEValues(const MEFamily &_fmef, const MEField<> *_cf) :
cur_ker(NULL),
fmef(_fmef),
cmef(_cf ? &_cf->GetMEFamily() : static_cast<const MEFamily*>(0)),
topo(NULL),
update_flag(0),
sf(0),
sfg(0),
jxw(0),
mdata(0),
cur_elem(NULL),
sdim(0),
cf(_cf),
mapping(NULL),
bmg(0),
buf(0)
{
}

template<typename METRAITS,typename FIELD>
MEValues<METRAITS,FIELD>::~MEValues() {
  delete [] buf;
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::Setup(const MeshObj &obj, UInt uflag,
      const intgRule *ir, const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{
  Setup(*obj.GetKernel(), uflag, ir, map);
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::Setup(const Kernel &ker, UInt uflag,
      const intgRule *ir, const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{
  std::vector<const intgRule*> irules;
  if (ir) irules.push_back(ir);

  Setup(ker, uflag, irules, map);
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::Setup(const Kernel &ker, UInt uflag,
      std::vector<const intgRule*> irules,
      const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{

  std::size_t buf_size = 0;

  const MasterElement<METRAITS> *lmeptr = 0;
  // Master element pointer for coordinate field
  const MasterElement<METraits<> > *lcmeptr = 0;


  topo = ker.GetTopo();
  sdim = topo->spatial_dim;
  cur_ker = &ker;
  lmeptr = fmef.getME(cur_ker->GetTopo()->name, METRAITS());
  if (cf) lcmeptr = GetME(*cf, *cur_ker)(METraits<>());
  update_flag = uflag;

  irule_list.clear();
  std::copy(irules.begin(), irules.end(), std::back_inserter(irule_list));


  UInt ltotal_nqpoints = 0;
  for (UInt q = 0; q < irule_list.size(); q++)
    ltotal_nqpoints += irule_list[q]->npoints();

  if (map) {
    mapping = map;
  } else mapping = cur_ker->GetMapping()(typename ME2MPTraits<METRAITS>::value());

  if ((update_flag &MEV::update_sfg) || (update_flag & MEV::update_jxw)) {
    ThrowRequire(cf);
    update_flag |= MEV::update_map;
  }

  // First a quick calculation to figure out sizes
  buf_size += mev_round_to_dword((UInt)(sizeof(buf_manage)));

  if (update_flag & MEV::update_sf) {
    buf_size += mev_round_to_dword((UInt)(ltotal_nqpoints*lmeptr->num_functions()*sizeof(double)));
  }
  if (update_flag & MEV::update_sfg) {
    // param_sfg
    buf_size += mev_round_to_dword((UInt)(ltotal_nqpoints*lmeptr->num_functions()*topo->parametric_dim*sizeof(double)));
    // sfg
    buf_size += mev_round_to_dword((UInt)(ltotal_nqpoints*sdim*lmeptr->num_functions()*sizeof(double)));
  }
  if (update_flag & MEV::update_jxw) {
    buf_size += mev_round_to_dword((UInt)(ltotal_nqpoints*sizeof(double)));
  }
  if (update_flag & MEV::update_map) {
    buf_size += mev_round_to_dword((UInt)(lcmeptr->num_functions()*sdim*sizeof(double)));
  }

  // Allocate the buffer
  if (buf) delete [] buf;

  buf = new UChar[buf_size + 1];

  // Size and place the buffers
  // First a quick calculation to figure out sizes
  buf_size = 0;

  bmg = reinterpret_cast<buf_manage*>(&buf[buf_size]);
  buf_size += mev_round_to_dword((UInt)(sizeof(buf_manage)));

  if (update_flag & MEV::update_sf) {
    sf = reinterpret_cast<double*>(&buf[buf_size]);
    buf_size += mev_round_to_dword((UInt)(ltotal_nqpoints*lmeptr->num_functions()*sizeof(double)));
  }
  if (update_flag & MEV::update_sfg) {
    // param_sfg
    param_sfg = reinterpret_cast<double*>(&buf[buf_size]);
    buf_size += mev_round_to_dword((UInt)(ltotal_nqpoints*lmeptr->num_functions()*topo->parametric_dim*sizeof(double)));
    // sfg
    sfg = reinterpret_cast<double*>(&buf[buf_size]);
    buf_size += mev_round_to_dword((UInt)(ltotal_nqpoints*sdim*lmeptr->num_functions()*sizeof(double)));
  }
  if (update_flag & MEV::update_jxw) {
    jxw = reinterpret_cast<double*>(&buf[buf_size]);
    buf_size += mev_round_to_dword((UInt)(ltotal_nqpoints*sizeof(double)));
  }
  if (update_flag & MEV::update_map) {
    mdata = reinterpret_cast<double*>(&buf[buf_size]);
    buf_size += mev_round_to_dword((UInt)(lcmeptr->num_functions()*sdim*sizeof(double)));
  }


  bmg->num_irules = irule_list.size();
  bmg->total_nqpoints = ltotal_nqpoints;
  bmg->meptr = lmeptr;
  bmg->cmeptr = lcmeptr;

  // Shape functions depend only on the integration rule, not
  // the element, so we can calculate them once here, for the whole kernel.
  
  if (update_flag & MEV::update_sf) {
    UInt off = 0;
    for (UInt q = 0; q < bmg->num_irules; q++) {
      lmeptr->shape_function(irule_list[q]->npoints(),
                irule_list[q]->locations(), &sf[off]);
      off += irule_list[q]->npoints()*lmeptr->num_functions();
    }
/*
Par::Out() << "Shape functions:" << std::endl;
std::copy(sf.begin(), sf.end(), std::ostream_iterator<double>(Par::Out(), " "));
Par::Out() << std::endl;
*/
  }
  
  if (update_flag & MEV::update_sfg) {
    UInt off = 0;
    for (UInt q = 0; q < bmg->num_irules; q++) {
      lmeptr->param_shape_grads(irule_list[q]->npoints(),
           irule_list[q]->locations(), &param_sfg[off]);

      off += irule_list[q]->npoints()*lmeptr->num_functions()*topo->parametric_dim;
    }

  }

  if (update_flag & MEV::update_jxw) {
  }

  if (update_flag & MEV::update_map) {
  }

  bmg->irule = bmg->num_irules > 0 ? irule_list[0] : 0;
  bmg->cur_rule = 0;
  bmg->cur_sf_offset = bmg->cur_sfg_offset = bmg->cur_psfg_offset = bmg->cur_jxw_offset = 0;
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::ReInit(const MeshObj &elem, UInt rule_num) {

  ThrowRequire(bmg->num_irules == 0 || rule_num < bmg->num_irules);

  sdim = GetMeshObjMesh(elem).spatial_dim();
  
  // Update offsets
  if (bmg->num_irules > 0) {
    bmg->cur_rule = rule_num;
    bmg->cur_sf_offset = bmg->cur_sfg_offset = bmg->cur_psfg_offset = bmg->cur_jxw_offset = 0;
    for (UInt q = 0; q < rule_num; q++) {
      bmg->cur_sf_offset +=  irule_list[q]->npoints()*bmg->meptr->num_functions();
      bmg->cur_sfg_offset +=  irule_list[q]->npoints()*bmg->meptr->num_functions()*sdim;
      bmg->cur_psfg_offset +=  irule_list[q]->npoints()*bmg->meptr->num_functions()*topo->parametric_dim;
      bmg->cur_jxw_offset +=  irule_list[q]->npoints();
    }

    bmg->irule = irule_list[rule_num];
  }

  cur_elem = &elem;

  if (update_flag & MEV::update_map) {
    GatherElemData<>(*bmg->cmeptr, *cf, elem, &mdata[0]);
  }

  // Note: shape functions were computed above, in Setup.

  if (update_flag & MEV::update_sfg) {
    ThrowRequire(bmg->cmeptr);
                                  
    bmg->meptr->shape_grads(bmg->irule->npoints(), mapping, &mdata[0], 
               bmg->irule->locations(),
               &param_sfg[bmg->cur_psfg_offset], &sfg[bmg->cur_sfg_offset]);
  }
 
  if (update_flag & MEV::update_jxw) {
    compute_jxw(elem, mdata);
  }

}

// Standard element implementation
template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::compute_jxw(const MeshObj &elem, const double mdata[]) {

  bmg->cmeptr->JxW(mapping, &mdata[0], bmg->irule, &jxw[bmg->cur_jxw_offset]);

}

// TODO: make this query the master element as to where field coeffs live.
template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::GetFunctionValues(const FIELD &field, field_type values[]) {

  if (bmg->meptr->orientation() == ShapeFunc::ME_DG) {

    GetFunctionValues(field, (field_type*)field.data(*cur_elem), values);

  } else {
    std::vector<field_type> fdata(field.dim()*bmg->meptr->num_functions());

    // Gather step.  Nodal for now.
    GatherElemData<METRAITS,FIELD>(*bmg->meptr, field, *cur_elem, &fdata[0]);

    GetFunctionValues(field, &fdata[0], values);

  }

}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::GetElemData(const FIELD &field, field_type values[]) {

  if (bmg->meptr->orientation() == ShapeFunc::ME_DG) {

    ThrowRequire(update_flag & MEV::update_sf);

    field_type *dt = field.data(*cur_elem);
    std::copy(dt, dt+bmg->meptr->num_functions()*field.dim(), values);
    
  } else {
    
    // Gather step.
    GatherElemData<METRAITS,FIELD>(*bmg->meptr, field, *cur_elem, values);

  }
  
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::GetCoordinateValues(double values[]) {

  ThrowRequire(update_flag & MEV::update_map);
  
  bmg->cmeptr->function_values(bmg->irule->npoints(), cf->dim(), bmg->irule->locations(), &mdata[0], &values[0]);
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::GetInterpolationPoints(double values[]) {
  Throw() << "this function only implemented for standard double,double case:";
}

template<>
void MEValues<METraits<>,MEField<> >::GetInterpolationPoints(double values[]) {

  ThrowRequire(update_flag & MEV::update_map);
 
  bmg->meptr->InterpPoints(mapping, &mdata[0], values);
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::Interpolate(const FIELD &f, const double fvals[]) {
  Throw() << "Interpolate only operatoes on METraits<double,double>";
}

template<>
void MEValues<METraits<>,MEField<> >::Interpolate(const MEField<> &f, const double fvals[]) {
  
  if (bmg->meptr->orientation() == ShapeFunc::ME_DG) {
    
    bmg->meptr->Interpolate(f.dim(), fvals, f.data(*cur_elem));
    
  } else {
    
    std::vector<double> mcoef(bmg->meptr->num_functions()*f.dim());
    
    bmg->meptr->Interpolate(f.dim(), fvals, &mcoef[0]);
    
    // Put data onto mesh
    ScatterElemData(*bmg->meptr, f, *cur_elem, &mcoef[0]);
  }
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::GetFunctionGrads(const FIELD &field, field_type grads[]) {
  std::vector<field_type> fdata; 

  fdata.resize(bmg->meptr->num_functions()*field.dim());
  GatherElemData<METRAITS,FIELD>(*bmg->meptr, field, *cur_elem, &fdata[0]);

  GetFunctionGrads(field, &fdata[0], grads);

}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::GetFunctionCurls(const FIELD &field, field_type curls[]) {
  if (field.dim() != sdim)
    Throw() << "For curl field dim=" << field.dim() <<  "must be sdim=" << sdim;
  std::vector<field_type> fdata; 

  fdata.resize(bmg->meptr->num_functions()*field.dim());
  GatherElemData<METRAITS,FIELD>(*bmg->meptr, field, *cur_elem, &fdata[0]);

  ThrowRequire(update_flag & MEV::update_sfg);

  bmg->meptr->function_curl(bmg->irule->npoints(), mapping, &mdata[0],
             bmg->irule->locations(), &fdata[0], &curls[0], &sfg[bmg->cur_sfg_offset]);
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::GetUnitNormals(double unit_normals[]) {
  ThrowRequire(update_flag & MEV::update_map);
  mapping->unit_normal(bmg->irule->npoints(), &mdata[0], bmg->irule->locations(), &unit_normals[0]);
}

template<typename METRAITS,typename FIELD>
void MEValues<METRAITS,FIELD>::GetNormals(double normals[]) {
  ThrowRequire(update_flag & MEV::update_map);
  mapping->normal(bmg->irule->npoints(), &mdata[0], bmg->irule->locations(), &normals[0]);
}



template class MEValues<METraits<>, MEField<> >;
template class MEValues<METraits<fad_type>, MEField<SField> >;


/*---------------------------------------------------------*/
// MESideValues class
/*---------------------------------------------------------*/
template<typename METRAITS, typename FIELD>
MESideValues<METRAITS,FIELD>::MESideValues(const MEFamily &_fmef, const MEField<> *_cf) :
  MEValues<METRAITS,FIELD>(_fmef, _cf),
  side_cmeptr(0),
  side_mapping(0)
{
}

template<typename METRAITS, typename FIELD>
MESideValues<METRAITS,FIELD>::~MESideValues()
{
}

template<typename METRAITS,typename FIELD>
void MESideValues<METRAITS,FIELD>::Setup(const MeshObj &obj, UInt uflag,
      const intgRule *ir, const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{
  this->Setup(*obj.GetKernel(), uflag, ir, map);
}

template<typename METRAITS,typename FIELD>
void MESideValues<METRAITS,FIELD>::Setup(const Kernel &ker, UInt uflag,
      const intgRule *ir, const Mapping<typename ME2MPTraits<METRAITS>::value> *map) 
{

  // Setup the side integration rules;
  const sideIntgFactory *sintg = sideIntgFactory::instance(ker.GetTopo()->name, ir);

  std::vector<const intgRule*> irules;

  for (UInt s = 0; s < ker.GetTopo()->num_sides; s++)
    irules.push_back(sintg->GetSideRule(s));

  // Call the element setup
  MEValues<METRAITS,FIELD>::Setup(ker, uflag, irules, map);

}

template<typename METRAITS,typename FIELD>
void MESideValues<METRAITS,FIELD>::ReInit(const MeshObj &elem, UInt side_num) {

  ThrowRequire(side_num < this->topo->num_sides);

  // Get side cmeptr, side_mapping
  ThrowRequire(this->bmg->cmeptr);
  side_cmeptr = this->bmg->cmeptr->side_element(side_num)->operator()(METraits<>());

  ThrowRequire(this->mapping);
  side_mapping = this->mapping->side_mapping(side_num);

  cur_side = side_num;

  MEValues<METRAITS,FIELD>::ReInit(elem, side_num);
}

template<typename METRAITS,typename FIELD>
void MESideValues<METRAITS,FIELD>::compute_jxw(const MeshObj &elem, const double mdata[]) {

  std::vector<double> lmdata; // different mdata, side data

  lmdata.resize(side_cmeptr->num_functions()*this->sdim);

  GatherSideData<>(*side_cmeptr, *this->cf, elem, cur_side, &lmdata[0]);

/*
Par::Out() << "mdata_jxw: cur_jxw_off=" << this->cur_jxw_offset <<std::endl;
std::copy(lmdata.begin(), lmdata.end(), std::ostream_iterator<typename METRAITS::double>(Par::Out(), " "));
Par::Out() << std::endl;
*/
  
  side_cmeptr->JxW(side_mapping, &lmdata[0], this->bmg->irule, &this->jxw[this->bmg->cur_jxw_offset]);

}

template class MESideValues<METraits<>, MEField<> >;
template class MESideValues<METraits<fad_type>,MEField<SField> >;


} // namespace
