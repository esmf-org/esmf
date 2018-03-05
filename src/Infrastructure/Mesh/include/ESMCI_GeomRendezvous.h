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
#ifndef ESMCI_GeomRendezvous_h
#define ESMCI_GeomRendezvous_h

#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_Context.h>
#include <Mesh/include/ESMCI_CommReg.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MEField.h>
#include "PointList/include/ESMCI_PointList.h"

#include <Mesh/src/Zoltan/zoltan.h>
namespace ESMCI {

class BBox;
class _field;
        
class GeomRend {
public:
/*
 * Configurendezvous configuration for the source grid object.
 * The source mesh interpolation region is created by looping through
 * the iter_obj_type over the region specified by Context.
 * Within these areas, all subordinate objects of obj_type are added
 * to the geometric rendezvous.
 */
  struct DstConfig {
  DstConfig(UInt _iter_otype, UInt _otype, const Context &_ctxt, bool _neighbors = false, bool _all_overlap_dst=false,  double _gtol = 1e-6) :
    iter_obj_type(_iter_otype), obj_type(_otype), ctxt(_ctxt), neighbors(_neighbors), all_overlap_dst(_all_overlap_dst), geom_tol(_gtol) {}
    UInt iter_obj_type; // Object to iterate when building intersection
    UInt obj_type; // One of node, node + interp, interp
    Context ctxt; // Context to match when iterating
    bool neighbors; // true = send neigbors with elements (for patch methods)
    bool all_overlap_dst; // construct the destination, so that every destination cell that overlaps a source cell ends up on the same proc
    double geom_tol;
  };

  GeomRend(Mesh *srcmesh, PointList *_srcplist,
           Mesh *dstmesh, PointList *_dstplist,
           const DstConfig &config, bool freeze_src_=false, bool on_sph=false);
  ~GeomRend();

  /*
   * Build the geometric Rendezvous.
   * We must send the fields to be registered on both the source and
   * destination fields here, since a grid commit will ensue to assure
   * that coordinates exist.
   * The destination field will be registered as in accordance with the
   * DstConfig object; if the 'conserv' type case, the full MEField will
   * be registered else only the interp _field will be registered.
   */
  void Build(UInt nsrcF, MEField<> **srcF, UInt ndstF, MEField<> **dstF, Zoltan_Struct **zzp, bool free_zz);
  void Build_Merge(UInt nsrcF, MEField<> **srcF, UInt ndstF, MEField<> **dstF, Zoltan_Struct **zzp);

  struct ZoltanUD {
  ZoltanUD(UInt _sdim, MEField<> *_coord_src, MEField<> *_coord_dst,PointList *_src_pointlist, PointList *_dst_pointlist, bool _iter_is_obj) :
      coord_src(_coord_src),
      coord_dst(_coord_dst),
        src_pointlist(_src_pointlist),
        dst_pointlist(_dst_pointlist),
        sdim(_sdim),
        iter_is_obj(_iter_is_obj) {}
    std::vector<MeshObj*> srcObj;
    std::vector<MeshObj*> dstObj;
    MEField<> *coord_src;
    MEField<> *coord_dst;
    PointList *src_pointlist;
    PointList *dst_pointlist;
    UInt sdim;
    bool iter_is_obj;
  };
        
  Mesh &GetSrcRend() { return srcmesh_rend; }

  Mesh &GetDstRend() { return dstmesh_rend; }

  PointList &GetDstPlistRend() { return *dstplist_rend; }
  PointList &GetSrcPlistRend() { return *srcplist_rend; }

  CommReg &GetSrcComm() { return srcComm; }
  CommReg &GetDstComm() { return dstComm; }

  UInt GetDstObjType() const { return dcfg.obj_type; }

  const std::vector<MEField<>*> &GetSrcRendFields() { return src_rend_Fields; }
  const std::vector<MEField<>*> &GetDstRendFields() { return dst_rend_Fields; }
  const std::vector<_field*> &GetDstRendfields() { return dst_rend_fields; }

private:
  GeomRend(const GeomRend &rhs);
  GeomRend &operator=(const GeomRend &rhs);

  // Routines

  // Build up the set of destination points and the coordinate box.
  void build_dest(double cmin[], double cmax[], ZoltanUD &zud);
  void build_dest_plist(double cmin[], double cmax[], PointList *dstpointlist);

  // Build the source elements
  void build_src(const BBox &dstBound, ZoltanUD &zud);

  // Zoltan parameters
  void set_zolt_param(Zoltan_Struct *zz);

  void build_src_mig(Zoltan_Struct *zz, ZoltanUD &zud);

  void build_dst_mig_all_overlap(ZoltanUD &zud);

  void build_src_mig_plist(ZoltanUD &zud, int numExport,
                           ZOLTAN_ID_PTR exportGids,
                           int *exportProcs, int numImport, ZOLTAN_ID_PTR importGids);


  void build_dst_mig(Zoltan_Struct *zz, ZoltanUD &zud, int numExport,
                ZOLTAN_ID_PTR exportLids, ZOLTAN_ID_PTR exportGids, int *exportProcs);

  void build_dst_mig_plist(ZoltanUD &zud, int numExport,
                           ZOLTAN_ID_PTR exportGids,
                           int *exportProcs, int numImport, ZOLTAN_ID_PTR importGids);

  void prep_meshes();

  void migrate_meshes();

  void dst_migrate_meshes();

  void src_migrate_meshes();

  // Data
  Mesh *srcmesh;
  PointList *srcplist;
  Mesh *dstmesh;
  PointList *dstplist;

  Mesh srcmesh_rend;
  Mesh dstmesh_rend;

  PointList *srcplist_rend;
  PointList *dstplist_rend;

  DstConfig dcfg;

  // The communication Registers from mesh to rendezvous mesh.
  CommReg srcComm;
  CommReg srcNbrComm;
  CommReg dstComm;
  bool built;
  UInt sdim;
  bool iter_is_obj;
  bool freeze_src;  // true if src mesh will not be migrated to create src rendezvous mesh

  // Treat as on a spherical surface (probably because we're using great circle edges)
  bool on_sph;

  /*
   * Store the fields on the rendezvous meshes that line up with those
   * sent into Build.  Depending on the type of transfer, the destination
   * mesh may use _field of MEField<>.
   */
  std::vector<MEField<>*> src_rend_Fields;
  std::vector<_field*> dst_rend_fields;
  std::vector<MEField<>*> dst_rend_Fields;
};
        
} // namespace

#endif
