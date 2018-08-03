// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Interp_h
#define ESMCI_Interp_h

#include <Mesh/include/Legacy/ESMCI_MEField.h>
#include <Mesh/include/Regridding/ESMCI_Search.h>
#include <Mesh/include/Legacy/ESMCI_GeomRendezvous.h>
#include <Mesh/include/Legacy/ESMCI_Migrator.h>
#include <Mesh/include/Legacy/ESMCI_SparseMsg.h>
#include <Mesh/include/Regridding/ESMCI_WMat.h>
#include <Mesh/include/Regridding/ESMCI_Mapping.h>
#include <Mesh/src/Zoltan/zoltan.h>
#include "PointList/include/ESMCI_PointList.h"

#include <vector>
#include <ostream>
#include <map>

namespace ESMCI {
  
class CommRel;
class MeshObj;

/*
 * Provides interpolation for serial and parallel meshes.  For 
 * serial the interpolatin is a simple search/application.  For parallel
 * we use the Rendezvous algorithm of Stewart et al.  The interface
 * is the same for both cases.
 */

class Mesh;
class Geom;

// Store/Migrate interp weights
class IWeights : public WMat {

public:

    IWeights();
  
  IWeights(const IWeights &);
  
  IWeights &operator=(const IWeights &);
    
  /*
   * Gather the tangent vectors for the mesh.  Template class TVECT supports
   * tv()(const MeshObj &node, const MEField<> &coords, double U[], double V[]) 
   * to return the
   * tangent vectors.  The vectors are returned as (U V), a 3x2 matrix, unless
     * transpose=true, where they we be a 2x3 matrix, (U V)^T.
   */ 
  template <typename TVECT>
  void GatherTangentVectors(const Mesh &mesh, TVECT tv, bool transpose = false); 
  
  /*
   * Change a 3d->3d vector matrix into a matrix mapping u,v -> u,v
   * coordinates.
   */
  void ChangeCoords(const IWeights &src_uv, const IWeights &dst_uc);
  
  /*
   * Remove any rows that have a mask value < 1 (if present).  Also remove any rows assigned to
   * a node that is not locally owned.
   */
  void Prune(const Mesh &mesh, const MEField<> *mask=0);

};

// Tangent vector support
template <typename TVECT>
void IWeights::GatherTangentVectors(const Mesh &mesh, TVECT tv, bool transpose) {
  
  clear();
  
  Mesh::const_iterator ni = mesh.node_begin(), ne = mesh.node_end();
  
  const MEField<> &coords = *mesh.GetCoordField();
  
  for (; ni != ne; ++ni) {
    
    const MeshObj &node = *ni;
    
    double U[3], V[3];
    
    tv(node, coords, U, V);

     if (!transpose) {
      
      for (UInt r = 0; r < 3; r++) {
        Entry row(node.get_id(), r);
        std::vector<Entry> col(2);
        col[0] = Entry(node.get_id(), 0, U[r]);
        col[1] = Entry(node.get_id(), 1, V[r]);
        InsertRow(row, col);
      } // r
      
    } else {
      for (UInt r = 0; r < 2; r++) {
            Entry row(node.get_id(), r);
            std::vector<Entry> col(3);
            double *val = r == 0 ? U : V;
            col[0] = Entry(node.get_id(), 0, val[0]);
            col[1] = Entry(node.get_id(), 1, val[1]);
            col[2] = Entry(node.get_id(), 2, val[2]);
            InsertRow(row, col);
          } // r
    }
  } // ni
    
}

/*
 * Hold the necessary state to perform an interpolation (either parallel
 * or serial).  The interpolated fields must have a certain uniform nature.
 * In particular, they must all be defined on the same part of the meshes,
 * and the destination fields must all be either nodal or interpolatory,
 * but not mixed.
 * Note: if INTERP_PATCH is used, it is up to the caller to make sure they
 * have ghosting enabled on the mesh.  It will also be their responsibility
 * to make sure the data is halo'ed to the ghosts before the Interp() operation.
 */ 

class Interp {
public:

  enum {INTERP_STD = 0, INTERP_PATCH, INTERP_CONSERVE, INTERP_NEAREST_SRC_TO_DST, INTERP_NEAREST_DST_TO_SRC, INTERP_CONSERVE_2ND, 
        INTERP_NEAREST_IDAVG};
  
  struct FieldPair {
  FieldPair(MEField<> *_sF, MEField<> *_dF, UChar _idata=INTERP_STD, UChar _patch_order=2) :
    first(_sF), second(_dF), idata(_idata), patch_order(_patch_order) {
  }
  MEField<> *first, *second;
  UChar idata; // interpolation specification.
  UChar patch_order;
   };
  
  /* 
   * Build the interpolation object.  The MEFields must be compatible in the
   * sense that they are all element based, or node based, etc...
   */
  Interp(Mesh *src, PointList *srcplist, Mesh *dest, PointList *destplist, Mesh *midmesh, bool freeze_dst_, int imethod,
         bool set_dst_status, WMat &dst_status,
         MAP_TYPE mtype=MAP_TYPE_CART_APPROX, 
         int unmappedaction=ESMCI_UNMAPPEDACTION_ERROR, int num_src_pnts=1, 
         ESMC_R8 dist_exponent=2.0);

  ~Interp();
  
  // Actually process the interpolation
  void operator()();
  
  // Form a matrix of the interpolant for the fpair_num field
  void operator()(int fpair_num, IWeights &iw, bool set_dst_status, WMat &dst_status);


  // L2 conservative interpolation - generate conservative interpolation weights
  // this routine will not work unless iwts is defined as a MEField<> on both meshes
  void interpL2csrvM(const IWeights &, IWeights *, 
                     MEField<> const * const, MEField<> const * const);
  
  void release_zz() { if(zz) Zoltan_Destroy(&zz); zz = 0; }
  Zoltan_Struct * get_zz()  { return zz; }
  SearchResult & get_sres() { return sres; }
  GeomRend & get_grend()    { return grend; }
  
  private:

  // interpolation type
  void mat_transfer_serial(int fpair_num, IWeights &iw, IWeights &src_frac, IWeights &dst_frac, 
                           bool set_dst_status, WMat &dst_status); 
  void mat_transfer_parallel(int fpair_num, IWeights &, IWeights &, IWeights &, 
                           bool set_dst_status, WMat &dst_status); 

  // L2 conservative interpolation matrix generation parallel?
  void interpL2csrvM_serial(const IWeights &, IWeights *, MEField<> const * const, MEField<> const * const);
  void interpL2csrvM_parallel(IWeights &, IWeights *, MEField<> const * const, MEField<> const * const);

  SearchResult sres;
  GeomRend grend;
  bool is_parallel;
  std::vector<MEField<>*> srcF;
  std::vector<MEField<>*> dstF;
  std::vector<_field*> dstf; // interp fields
  std::vector<int> iflag;
  bool has_std; // true if a standard interpolation exists
  bool has_patch; // true if a patch interp exists
  bool has_cnsrv; // true if a conserve interp exists
  bool has_nearest_src_to_dst; // true if a nearest interp exists
  bool has_nearest_dst_to_src; // true if a nearest interp exists
  bool has_nearest_idavg; // true if a nearest idavg interp exists
  int num_src_pnts;
  ESMC_R8 dist_exponent;
  Mesh *srcmesh;
  PointList *srcpointlist;
  Mesh *dstmesh;
  PointList *dstpointlist;
  Mesh *midmesh;
  Zoltan_Struct * zz;
  int interp_method;
};

} // namespace

#endif /*ESMC_INTERP_H_*/
