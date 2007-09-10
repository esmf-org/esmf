// $Id: ESMC_Interp.C,v 1.1 2007/09/10 17:38:29 dneckels Exp $
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
#include <ESMC_Interp.h>
#include <ESMC_Exception.h>
#include <ESMC_Search.h>
#include <ESMC_ParEnv.h>
#include <ESMC_MEValues.h>

namespace ESMCI {
namespace MESH {
  
/*
 * Basic routines to interpolate data.
 */
 
/*
 * The very simplest case; a nodal finite element (just needs values at the nodes)
 * on one processor (no rendezvous meshes).
 */
void nodal_serial_transfer(Interp::FieldPair *fields_begin, Interp::FieldPair *fields_end, SearchResult &sres) {
  
  // Verify serial processor condition.
  ThrowRequire(Par::Size() == 1);
  
  if (fields_begin == fields_end) return;

  SearchResult::iterator sb = sres.begin(), se = sres.end();

  for (; sb != se; sb++) {

    Search_result &sres = **sb;

    // Trick:  Gather the data from the source field so we may call interpolate point
    const MeshObj &elem = *(*sb)->elem;

    UInt pdim = GetMeshObjTopo(elem)->parametric_dim;

    // Inner loop through fields
    Interp::FieldPair *fi = fields_begin, *fe = fields_end;
    for (; fi != fe; ++fi) {

      MEField<> &sfield = *fi->first;
      MEField<> &dfield = *fi->second;

      // Verify is_nodal precondition.
      ThrowRequire(dfield.is_nodal());

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
  
void point_serial_transfer(UInt num_fields, MEField<> *const *sfields, _field *const *dfields, SearchResult &sres) {
  
  if (num_fields == 0) return;

  SearchResult::iterator sb = sres.begin(), se = sres.end();

  for (; sb != se; sb++) {

    Search_result &sres = **sb;

    // Trick:  Gather the data from the source field so we may call interpolate point
    const MeshObj &elem = *(*sb)->elem;

    UInt pdim = GetMeshObjTopo(elem)->parametric_dim;

    // Inner loop through fields
    for (UInt i = 0; i < num_fields; i++) {

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
  
  return GeomRend::DstConfig(repF.ObjType(), otype, repF.GetContext());
}
  
Interp::Interp(Mesh &src, Mesh &dest, const std::vector<FieldPair> &_fpairs) :
sres(),
grend(src, dest, get_dst_config(dest, _fpairs)),
fpairs(_fpairs),
is_parallel(Par::Size() > 1),
srcF(),
dstF()
{
  // Different paths for parallel/serial
  UInt search_obj_type = grend.GetDstObjType();
  
  if (is_parallel) {
   
    for (UInt i = 0; i < fpairs.size(); i++) {
      srcF.push_back(fpairs[i].first);
      dstF.push_back(fpairs[i].second);
      dstf.push_back(fpairs[i].second->GetInterp());
    }
    
    // Form the parallel rendezvous meshes/specs
    grend.Build(srcF.size(), &srcF[0], dstF.size(), &dstF[0]);
    
    Search(grend.GetSrcRend(), grend.GetDstRend(), grend.GetDstObjType(), sres);
    
  } else {
    // Serial track.  Meshes already in geometric rendezvous.  (Perhaps get
    // the subset of the mesh for interpolating??)
     Search(src, dest, search_obj_type, sres);
  }
}
  
Interp::~Interp() {
}

void Interp::operator()() {
  
  if (is_parallel) transfer_parallel(); else transfer_serial();
  
}

void Interp::transfer_serial() {
  
  if (fpairs[0].second->is_nodal()) {
    
    nodal_serial_transfer(&(*fpairs.begin()), &(*fpairs.end()), sres);
  
  } else {
  
    Throw() << "Non nodal serial not yet implemented";
  
  }
  
}

void Interp::transfer_parallel() {
  
  // Send source data to rendezvous decomp
  const std::vector<MEField<> *> &src_rend_Fields = grend.GetSrcRendFields();
  
  ThrowRequire(src_rend_Fields.size() == fpairs.size());
  
  grend.GetSrcComm().SendFields(src_rend_Fields.size(), &(*srcF.begin()), &(*src_rend_Fields.begin()));
  
  // Perform the interpolation
  const std::vector<_field*> &dst_rend_fields = grend.GetDstRendfields();
  
  ThrowRequire(dst_rend_fields.size() == src_rend_Fields.size());
  
  point_serial_transfer(dst_rend_fields.size(), &(*src_rend_Fields.begin()), &(*dst_rend_fields.begin()), sres);
  
  // Retrieve the interpolated data
  CommRel &dst_node_rel = grend.GetDstComm().GetCommRel(MeshObj::NODE);

  // Send the data back (comm has been transposed)
  ThrowRequire(dstf.size() == dst_rend_fields.size());
  dst_node_rel.send_fields(dstf.size(), &(*dst_rend_fields.begin()), &(*dstf.begin()));
  
  // Process interpolation (if needed)
  
}
  
} // namespace
} // namespace
