// $Id: ESMC_GeomRendezvous.h,v 1.3.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times
#ifndef ESMC_GeomRendezvous_h
#define ESMC_GeomRendezvous_h

#include <ESMC_MeshTypes.h>
#include <ESMC_Context.h>
#include <ESMC_CommReg.h>
#include <ESMC_Mesh.h>
#include <ESMC_MEField.h>

#include <Mesh/src/Zoltan/zoltan.h>

namespace ESMCI {
namespace MESH {

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
         DstConfig(UInt _iter_otype, UInt _otype, const Context &_ctxt, bool _neighbors = false, double _gtol = 1e-6) :
           iter_obj_type(_iter_otype), obj_type(_otype), ctxt(_ctxt), neighbors(_neighbors), geom_tol(_gtol) {}
         UInt iter_obj_type; // Object to iterate when building intersection 
         UInt obj_type; // One of node, node + interp, interp
         Context ctxt; // Context to match when iterating 
         bool neighbors; // true = send neigbors with elements (for patch methods)
         double geom_tol;
  };

  GeomRend(Mesh &srcmesh, Mesh &dstmesh, const DstConfig &config);
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
  void Build(UInt nsrcF, MEField<> **srcF, UInt ndstF, MEField<> **dstF);

  struct ZoltanUD {
    ZoltanUD(UInt _sdim, MEField<> *_coord_src, MEField<> *_coord_dst) :
      coord_src(_coord_src),
      coord_dst(_coord_dst),
      sdim(_sdim) {}
    std::vector<MeshObj*> srcObj;
    std::vector<MeshObj*> dstObj;
    MEField<> *coord_src;
    MEField<> *coord_dst;
    UInt sdim;
  };
	
  Mesh &GetSrcRend() { return srcmesh_rend; }
  
  Mesh &GetDstRend() { return dstmesh_rend; } 
  
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

  // Build the source elements
  void build_src(const BBox &dstBound, ZoltanUD &zud);

  // Zoltan parameters
  void set_zolt_param(Zoltan_Struct *zz);

  void build_src_mig(Zoltan_Struct *zz, ZoltanUD &zud);

  void build_dst_mig(Zoltan_Struct *zz, ZoltanUD &zud, int numExport,
                ZOLTAN_ID_PTR exportLids, ZOLTAN_ID_PTR exportGids, int *exportProcs);
                
  void prep_meshes();
  
  void migrate_meshes();

  // Data
  Mesh &srcmesh;
  Mesh &dstmesh;

  Mesh srcmesh_rend;
  Mesh dstmesh_rend;
  
  DstConfig dcfg;
  
  // The communication Registers from mesh to rendezvous mesh.
  CommReg srcComm;
  CommReg dstComm;
  bool built;
  UInt sdim;
  bool iter_is_obj;
  
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
} // namespace

#endif
