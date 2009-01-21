// $Id: ESMC_Interp.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_Interp.h>
#include <ESMC_Exception.h>
#include <ESMC_Search.h>
#include <ESMC_ParEnv.h>
#include <ESMC_MEValues.h>
#include <ESMC_PatchRecovery.h>

namespace ESMCI {
namespace MESH {
  
/*
 * Basic routines to interpolate data.
 */
   
/*
 * Patch interpolation, where destination and source fields are nodal.
 */
void patch_serial_transfer(MEField<> &src_coord_field, UInt _nfields, MEField<>* const* _sfields, _field* const *_dfields, int *iflag, SearchResult &sres) {
   Trace __trace("patch_serial_transfer(MEField<> &src_coord_field, UInt _nfields, MEField<>* const* _sfields, _field* const *_dfields, int *iflag, SearchResult &sres)");
   
    
  const int pdeg = 2; // TODO: deduce this.  For instance, deg source + 1 is reasonable
    
  if (_nfields == 0) return;

  std::vector<MEField<>* > fields;
  std::vector<_field* > dfields;
  UInt nrhs = 0;
  
  for (UInt i = 0; i < _nfields; i++) {
  
    // Only process interp_patch  
    if (iflag[i] != Interp::INTERP_PATCH) continue;
    
    ThrowRequire(_sfields[i]->is_nodal());
  
    fields.push_back(_sfields[i]); dfields.push_back(_dfields[i]);
    
    ThrowRequire(_sfields[i]->dim() == _dfields[i]->dim());

    nrhs += _sfields[i]->dim(); // how many rhs for recovery
  }

  // Create the  recovery field
  SearchResult::iterator sb = sres.begin(), se = sres.end();

  for (; sb != se; sb++) {
    Search_result &sres = **sb;
    // Trick:  Gather the data from the source field so we may call interpolate point
    const MeshObj &elem = *(*sb)->elem;
//std::cout << "Transfer: elem:" << elem.get_id() << std::endl;

    ElemPatch<> epatch;

    epatch.CreateElemPatch(pdeg, ElemPatch<>::GAUSS_PATCH,
                           elem,
                           src_coord_field,
                           fields.size(),
                           &fields[0],
                           700000
                            );

    // Gather parametric coords into an array.
    UInt pdim = GetMeshObjTopo(elem)->parametric_dim;
    UInt npts = sres.nodes.size(); // number of points to interpolate
    std::vector<double> pc(pdim*npts);
    
    for (UInt np = 0; np < npts; np++) {
    
      for (UInt pd = 0; pd < pdim; pd++) {
    
        pc[np*pdim+pd] = sres.nodes[np].pcoord[pd];
    
      }
    
    }

    std::vector<double> result(nrhs*npts);
    epatch.Eval(npts, &pc[0], &result[0]);

    // Now copy data into fields
    for (UInt np = 0; np < npts; np++) {
    
      const MeshObj &snode = *sres.nodes[np].node;
      UInt cur_field = 0;
    
      for (UInt i = 0; i < dfields.size(); i++) {
    
        const _field &dfield = *dfields[i];
    
        if (dfield.OnObj(snode)) {
          UInt fdim = dfield.dim();
          double *data = dfield.data(snode);
    
          for (UInt f = 0; f < fdim; f++) {
            data[f] = result[np*nrhs+(cur_field+f)];
          }

        }
        cur_field++;

      } // for fi

    } // for np

  } // for searchresult
  
}
  
void point_serial_transfer(UInt num_fields, MEField<> *const *sfields, _field *const *dfields, int *iflag, SearchResult &sres) {
  Trace __trace("point_serial_transfer(UInt num_fields, MEField<> *const *sfields, _field *const *dfields, int *iflag, SearchResult &sres)");
  
  
  if (num_fields == 0) return;

  SearchResult::iterator sb = sres.begin(), se = sres.end();

  for (; sb != se; sb++) {

    Search_result &sres = **sb;

    // Trick:  Gather the data from the source field so we may call interpolate point
    const MeshObj &elem = *(*sb)->elem;

    UInt pdim = GetMeshObjTopo(elem)->parametric_dim;

    // Inner loop through fields
    for (UInt i = 0; i < num_fields; i++) {

      if (iflag[i] != Interp::INTERP_STD) continue;

      MEField<> &sfield = *sfields[i];
      _field &dfield = *dfields[i];

      MEValues<> mev(sfield.GetMEFamily(), sfield);

      if (dfield.dim() != sfield.dim())
        Throw() << "dest and source fields have incompatible dimensions";

     // Load Parametric coords
     UInt npts = sres.nodes.size(); // number of points to interpolate
     std::vector<double> pcoord(pdim*npts);
     for (UInt np = 0; np < npts; np++) {
       for (UInt pd = 0; pd < pdim; pd++)
         pcoord[np*pdim+pd] = sres.nodes[np].pcoord[pd];
     }

     arbq pintg(pdim, npts, &pcoord[0]);
     mev.Setup(elem, 0, &pintg);
     mev.ReInit(elem);

     std::vector<double> ires(npts*dfield.dim());
     mev.GetFunctionValues(sfield, &ires[0]);

     // Copy data to nodes
     for (UInt n = 0; n < npts; n++) {
       const MeshObj &node = *sres.nodes[n].node;
       for (UInt d = 0; d < dfield.dim(); d++)
         ((double*)dfield.data(node))[d] = ires[n*dfield.dim()+d];
     }

    } // for fields

  } // for searchresult
}
 
static GeomRend::DstConfig get_dst_config(Mesh &dest, const std::vector<Interp::FieldPair> &fpairs) {
  
  // Determine the rendezvous destination configuration.  Use Field 0 for the info.  
  // All other fields must have compatability with the first.
  
  ThrowRequire(fpairs.size() > 0);
  
  MEField<> &repF = *fpairs[0].second;
  
  // Figure out the type of object to gather values for (i.e. nodes, interp, etc...)
  UInt otype;
  if (repF.is_elemental()) {
    otype = MeshObj::ELEMENT;
  } else if (repF.is_nodal()) {
    otype = MeshObj::NODE;
  } else {
    otype = MeshObj::NODE | MeshObj::INTERP;
  }
  
  // Loop fieldpairs.  If any have INTERP_PATCH, collect nieghbors
  bool nbor = false;
  for (UInt i = 0; i < fpairs.size() && nbor == false; i++) {
    if (fpairs[i].idata == Interp::INTERP_PATCH) nbor = true;
  }
  
  return GeomRend::DstConfig(repF.ObjType(), otype, repF.GetContext(), nbor);
}
  
Interp::Interp(Mesh &src, Mesh &dest, const std::vector<FieldPair> &_fpairs) :
sres(),
grend(src, dest, get_dst_config(dest, _fpairs)),
fpairs(_fpairs),
is_parallel(Par::Size() > 1),
srcF(),
dstF(),
has_std(false),
has_patch(false),
srcmesh(src),
dstmesh(dest)
{
  // Different paths for parallel/serial
  UInt search_obj_type = grend.GetDstObjType();
  
     
  for (UInt i = 0; i < fpairs.size(); i++) {
    srcF.push_back(fpairs[i].first);
    dstF.push_back(fpairs[i].second);
    dstf.push_back(fpairs[i].second->GetInterp());
    iflag.push_back(fpairs[i].idata);
  }
  
  if (is_parallel) {
   
    // Form the parallel rendezvous meshes/specs
    grend.Build(srcF.size(), &srcF[0], dstF.size(), &dstF[0]);
    
    Search(grend.GetSrcRend(), grend.GetDstRend(), grend.GetDstObjType(), sres);
    
  } else {
    // Serial track.  Meshes already in geometric rendezvous.  (Perhaps get
    // the subset of the mesh for interpolating??)
     Search(src, dest, search_obj_type, sres);
  }
  
  // Update has_[std/patch] flags
  for (UInt j = 0; j < fpairs.size(); j++) {
    if (fpairs[j].idata == Interp::INTERP_STD) has_std = true;
    if (fpairs[j].idata == Interp::INTERP_PATCH) has_patch = true;
  }
  
}
  
Interp::~Interp() {
}

void Interp::operator()() {
  Trace __trace("Interp::operator()()");  
  
  if (is_parallel) transfer_parallel(); else transfer_serial();
  
}

void Interp::transfer_serial() {
  Trace __trace("Interp::transfer_serial()");
  
  // Standard interpolation
  if (fpairs[0].second->is_nodal() && has_std) {

    point_serial_transfer(srcF.size(), &(*srcF.begin()), &(*dstf.begin()), &iflag[0], sres);
  
  } else {
  
    Throw() << "Non nodal serial not yet implemented";
  
  }
  
  // Patch interpolation
  if (has_patch) {
    
    patch_serial_transfer(*srcmesh.GetCoordField(), srcF.size(), &(*srcF.begin()), &(*dstf.begin()), &iflag[0], sres);
  
  } else {
  
    Throw() << "Non nodal serial not yet implemented";
  
  }
  
}

void Interp::transfer_parallel() {
  Trace __trace("Interp::transfer_parallel()");
  
  // Send source data to rendezvous decomp
  const std::vector<MEField<> *> &src_rend_Fields = grend.GetSrcRendFields();
  
  ThrowRequire(src_rend_Fields.size() == fpairs.size());
  
  grend.GetSrcComm().SendFields(src_rend_Fields.size(), &(*srcF.begin()), &(*src_rend_Fields.begin()));
  
  // Perform the interpolation
  const std::vector<_field*> &dst_rend_fields = grend.GetDstRendfields();
  
  ThrowRequire(dst_rend_fields.size() == src_rend_Fields.size());
  
  if (has_std) point_serial_transfer(dst_rend_fields.size(), &(*src_rend_Fields.begin()), &(*dst_rend_fields.begin()), &iflag[0], sres);
  if (has_patch) patch_serial_transfer(*grend.GetSrcRend().GetCoordField(), src_rend_Fields.size(), &(*src_rend_Fields.begin()), &(*dst_rend_fields.begin()), &iflag[0], sres);
  
  // Retrieve the interpolated data
  CommRel &dst_node_rel = grend.GetDstComm().GetCommRel(MeshObj::NODE);

  // Send the data back (comm has been transposed in GeomRend::Build)
  ThrowRequire(dstf.size() == dst_rend_fields.size());
  dst_node_rel.send_fields(dstf.size(), &(*dst_rend_fields.begin()), &(*dstf.begin()));
  
  // TODO:Process interpolation (if needed).  This means, for instance, collecting the values 
  // received above and asking the master element to manufacture coefficients.
  
}

  
} // namespace
} // namespace
