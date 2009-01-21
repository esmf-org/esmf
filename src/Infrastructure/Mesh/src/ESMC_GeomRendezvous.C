// $Id: ESMC_GeomRendezvous.C,v 1.3.2.2 2009/01/21 21:25:22 cdeluca Exp $
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
#include <ESMC_GeomRendezvous.h>
#include <ESMC_Exception.h>
#include <ESMC_BBox.h>
#include <ESMC_MeshUtils.h>
#include <ESMC_ParEnv.h>
#include <ESMC_MeshRead.h>
#include <ESMC_MeshObjConn.h>

#include <Mesh/src/Zoltan/zoltan.h>

#include <limits>

#define GEOM_DEBUG

namespace ESMCI {
namespace MESH {

/*-----------------------------------------------------------------------------------*/
// Zoltan callbacks
/*-----------------------------------------------------------------------------------*/
typedef std::vector<MeshObj*> MeshObjVect;
static int GetNumAssignedObj(void *user, int *err) {
  GeomRend::ZoltanUD &udata = *(static_cast<GeomRend::ZoltanUD*>(user));
  *err = 0;
  return udata.srcObj.size() + udata.dstObj.size();
}

static void GetObjList(void *user, int numGlobalIds, int numLids, ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids,
          int wgt_dim, float *obj_wghts, int *err) 
{
  GeomRend::ZoltanUD &udata = *(static_cast<GeomRend::ZoltanUD*>(user));

  UInt i = 0;
  MeshObjVect::iterator ni = udata.srcObj.begin(), ne = udata.srcObj.end();
  for (; ni != ne; ++ni) {
    gids[2*i] = 0;
    gids[2*i + 1] = (*ni)->get_id();
    lids[i] = i;
    i++;
  }

  ni = udata.dstObj.begin(), ne = udata.dstObj.end();
  for (; ni != ne; ++ni) {
    gids[2*i] = 1;
    gids[2*i + 1] = (*ni)->get_id();
    lids[i] = i;
    i++;
  }

  *err = 0;
}

static int GetNumGeom(void *user, int *err) {
  GeomRend::ZoltanUD &udata = *(static_cast<GeomRend::ZoltanUD*>(user));
  *err = 0;

  return udata.sdim;
}

static void GetObject(void *user, int numGlobalIds, int numLids, int numObjs,
  ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids, int numDim, double *pts, int *err) 
{
  GeomRend::ZoltanUD &udata = *(static_cast<GeomRend::ZoltanUD*>(user));
  MEField<> *coord_src = udata.coord_src;
  MEField<> *coord_dst = udata.coord_dst;
  *err = 0;

  UInt list1_size = udata.srcObj.size();
  for (UInt i = 0; i < (UInt) numObjs; i++) {
    UInt mesh = gids[2*i]; // 0 = src, 1 = dst
    UInt idx = lids[i];

    std::vector<double> ndata(numDim);
    double *c;
    if (mesh == 0) {
      MeshObj *elemp = udata.srcObj[idx];
      elemCentroid(*coord_src, *elemp, &ndata[0]);
      c = &ndata[0];
      // Average node coords
    } else if (mesh == 1) {
      UInt sidx = idx - list1_size; // local indices start from first list, continue into second
      MeshObj *nodep = udata.dstObj[sidx];
      c = coord_dst->data(*nodep);
    } else throw("GetObject, unknown mesh from global id");

    for (UInt d = 0; d < (UInt) numDim; d++) pts[i*numDim + d] = c[d];
  }
}

/*-----------------------------------------------------------------------------------*/
// GeomRend functions
/*-----------------------------------------------------------------------------------*/

GeomRend::GeomRend(Mesh &_srcmesh, Mesh &_dstmesh, const DstConfig &cfg) :
srcmesh(_srcmesh),
dstmesh(_dstmesh),
dcfg(cfg),
srcComm(),
dstComm(),
built(false),
sdim(_srcmesh.spatial_dim()),
iter_is_obj(cfg.iter_obj_type == cfg.obj_type)
{
  ThrowRequire(_srcmesh.spatial_dim() == _dstmesh.spatial_dim());
  ThrowRequire(dcfg.iter_obj_type == MeshObj::ELEMENT ||
               dcfg.iter_obj_type == srcmesh.side_type());

  // For now only allow the 'conservative case for elements (since sides have no relations otherwise)
  ThrowRequire(!iter_is_obj || dcfg.obj_type == MeshObj::ELEMENT);
}

GeomRend::~GeomRend() {
}

void GeomRend::build_dest(double cmin[], double cmax[], ZoltanUD &zud) {
  Trace __trace("GeomRend::build_dest(double cmin[], double cmax[], ZoltanUD &zud)");

  // Init cmin, cmax
  std::fill(cmin, cmin+sdim, std::numeric_limits<double>::max());
  std::fill(cmax, cmax+sdim, -std::numeric_limits<double>::max());

  MEField<> &coord = *dstmesh.GetCoordField();

  // Loop the kernels
  KernelList::iterator ki = dstmesh.set_begin(), ke = dstmesh.set_end();
  for (; ki != ke; ++ki) {
    Kernel &ker = *ki;
    // select all objs of this type and not the newly created ones (which have bogus ids)
    if (ker.type() == dcfg.iter_obj_type && ker.GetContext().any(dcfg.ctxt)) {

      Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
      for (; oi !=oe; ++oi) {

        MeshObj &obj = *oi;

        // If obj_type == iter_obj_type, just add the object and get a bounding box.
        // Otherwise we must loop the USES and add.
        if (iter_is_obj) {

          zud.dstObj.push_back(&obj);

          BBox obox(coord, obj, 0.25);

          for (UInt d = 0; d < sdim; d++) {
            if (obox.getMin()[d] < cmin[d]) cmin[d] = obox.getMin()[d];
            if (obox.getMax()[d] > cmax[d]) cmax[d] = obox.getMax()[d];
          }

        } else {
          MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();

          for (; ri != re; ++ri) {

            if (ri->type == MeshObj::USES && ri->obj->get_type() & dcfg.obj_type) {

              std::vector<MeshObj*>::iterator lb =
                std::lower_bound(zud.dstObj.begin(), zud.dstObj.end(), ri->obj);

              // If not there, add
              if (lb == zud.dstObj.end() || *lb != ri->obj) {

                zud.dstObj.insert(lb, ri->obj);

                double *c = coord.data(*ri->obj);

                for (UInt d = 0; d < sdim; d++) {
                  if (c[d] < cmin[d]) cmin[d] = c[d];
                  if (c[d] > cmax[d]) cmax[d] = c[d];
                }
              }

            }

          } // for ri

        } // traverse USES

      } // for oi

    } // correct type of kernel

  } // kernels
  
}

void GeomRend::build_src(const BBox &dstBound, ZoltanUD &zud) {
  Trace __trace("GeomRend::build_src(const BBox &dstBound, ZoltanUD &zud)");

  MEField<> &coord = *srcmesh.GetCoordField();
  // Add all elements that intersect the bounding box of the dst

  Mesh::iterator ei = srcmesh.elem_begin(), ee = srcmesh.elem_end();
  for (; ei != ee; ++ei) {

    MeshObj &elem = *ei;

    BBox ebox(coord, elem, 0.25);

    if (BBoxIntersect(ebox, dstBound, dcfg.geom_tol)) 
      zud.srcObj.push_back(&elem);

  } // for ei

}

void GeomRend::set_zolt_param(Zoltan_Struct *zz) {
  Trace __trace("GeomRend::set_zolt_param(Zoltan_Struct *zz)");

  Zoltan_Set_Param(zz, "DEBUG_LEVEL", "0");
  Zoltan_Set_Param(zz, "LB_METHOD", "RCB");
  Zoltan_Set_Param(zz, "NUM_GID_ENTRIES", "2"); // Two integers for an id
  Zoltan_Set_Param(zz, "NUM_LID_ENTRIES", "1");
  Zoltan_Set_Param(zz, "RETURN_LISTS", "ALL");
  //Zoltan_Set_Param(zz, "RCB_RECTILINEAR_BLOCKS", "1");
  Zoltan_Set_Param(zz, "AVERAGE_CUTS", "1");

   
  // RCB
  //Zoltan_Set_Param(zz, "RCB_LOCK_DIRECTIONS", "1");
  Zoltan_Set_Param(zz, "KEEP_CUTS", "1");
  Zoltan_Set_Param(zz, "RCB_OUTPUT_LEVEL", "0");

}

static void rcb_isect(Zoltan_Struct *zz, MEField<> &coord, std::vector<MeshObj*> &objlist,
        std::vector<CommRel::CommNode> &mignode, double geom_tol, UInt sdim) {
  Trace __trace("rcb_isect(Zoltan_Struct *zz, MEField<> &coord, std::vector<MeshObj*> &objlist, std::vector<CommRel::CommNode> &res)");

  UInt csize = Par::Size();

  // We have already narrowed the source mesh down to those cells
  // which intersect the bbox of the dest, so loop back through these and
  // figure out where these cells go.
  std::vector<MeshObj*>::iterator si = objlist.begin(), se = objlist.end();
  int numprocs;
  std::vector<int> procs(csize);

  for (; si != se; ++si) {

    MeshObj &elem = **si;

    BBox ebox(coord, elem, geom_tol);

    // Insersect with the cuts
    Zoltan_LB_Box_Assign(zz, ebox.getMin()[0],
                             ebox.getMin()[1],
                             sdim > 2 ? ebox.getMin()[2] : 0,
                             ebox.getMax()[0],
                             ebox.getMax()[1],
                             sdim > 2 ? ebox.getMax()[2] : 0,
                             &procs[0],
                             &numprocs);

    // Add to comm
    for (UInt i = 0; i < (UInt) numprocs; i++) {
      CommRel::CommNode cnode(&elem, procs[i]);

      std::vector<CommRel::CommNode>::iterator lb =
        std::lower_bound(mignode.begin(), mignode.end(), cnode);

      // Add if not already there
      if (lb == mignode.end() || *lb != cnode)
        mignode.insert(lb, cnode);

    } // for nproc
  } // for si
}

static void add_neighbors(CommRel &src_mig) {
  
  // To do this, create a temporary dependents spec
  CommRel ndep;
  
  src_mig.dependants(ndep, MeshObj::NODE);
  
  ndep.sort_domain();
  
  // Loop these nodes; make sure that ANY element attached is going
  CommRel::MapType::iterator di = ndep.domain_begin(), de = ndep.domain_end();
  
  for (; di != de;) {
    
    MeshObj *node = di->obj;
    
    // Get elements attached to node:
    std::vector<MeshObj*> on(1);
    on[0] = node;
    std::vector<MeshObj*> elem;       
    
    MeshObjConn::common_objs(on.begin(),on.end(), MeshObj::USED_BY, MeshObj::ELEMENT, elem);
    
    // Loop occurences of this node
    while (di != de && di->obj == node) {
      
      UInt proc = di->processor;
      
      CommRel::CommNode cnode(node, proc);
      
      CommRel::MapType::iterator lb = 
        std::lower_bound(src_mig.domain_begin(), src_mig.domain_end(), cnode);
        
      if (lb == src_mig.domain_end() || *lb != cnode) {
       // std::cout << "Inserting neighbor:" << cnode.obj->get_id() << std::endl;
        src_mig.domain_insert(lb, cnode);
      }
      
      ++di;
    }
    
  } // for di
  
}

void GeomRend::build_src_mig(Zoltan_Struct *zz, ZoltanUD &zud) {
  Trace __trace("GeomRend::build_src_mig(Zoltan_Struct *zz, ZoltanUD &zud)");

  MEField<> &coord = *srcmesh.GetCoordField();

  std::vector<CommRel::CommNode> mignode;

  rcb_isect(zz, coord, zud.srcObj, mignode, dcfg.geom_tol, sdim);

  // Add our result to the migspec
  CommRel &src_migration = srcComm.GetCommRel(MeshObj::ELEMENT);
  src_migration.Init("src_migration", srcmesh, srcmesh_rend, false);
  src_migration.add_domain(mignode);

  // Add the neighbors to the spec, if necessary
  if (dcfg.neighbors)
    add_neighbors(src_migration);

#ifdef GEOM_DEBUG
Par::Out() << "** source mig:" << std::endl;
src_migration.Print(Par::Out());
#endif

  // Now form the lower order commspecs
  src_migration.dependants(srcComm.GetCommRel(MeshObj::NODE), MeshObj::NODE);
  src_migration.dependants(srcComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
  src_migration.dependants(srcComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);

}

void GeomRend::build_dst_mig(Zoltan_Struct *zz, ZoltanUD &zud, int numExport,
     ZOLTAN_ID_PTR exportLids, ZOLTAN_ID_PTR exportGids, int *exportProcs)
{
  Trace __trace("GeomRend::build_dst_mig(Zoltan_Struct *zz, ZoltanUD &zud, int numExport, ZOLTAN_ID_PTR exportLids, ZOLTAN_ID_PTR exportGids, int *exportProcs)");

  UInt rank = Par::Rank();

  // In the case that the destination is composed of the iter type, we must send also
  // intersect the destination with the RCB.
  // Otherwise, we just forward the points as implied by the exportLids.  For points on the
  // current proc, we just send anyway since the communication object is set up to avoid 
  // comm in this case.

  if (iter_is_obj) {
    /*--------------------------------------------------------------*/
    // Element/side case. Intersect
    /*--------------------------------------------------------------*/

    MEField<> &coord = *dstmesh.GetCoordField();

    std::vector<CommRel::CommNode> mignode;

    CommRel &dst_migration = dstComm.GetCommRel(dcfg.obj_type);
    dst_migration.Init("dst_migration", dstmesh, dstmesh_rend, false);
    rcb_isect(zz, coord, zud.dstObj, mignode, dcfg.geom_tol, sdim);
    
    // Now flush out the comm with lower hierarchy
    dst_migration.dependants(dstComm.GetCommRel(MeshObj::NODE), MeshObj::NODE); 
    dst_migration.dependants(dstComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
    dst_migration.dependants(dstComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);

  } else {

    /*--------------------------------------------------------------*/
    // Node/interp point case.  Just forward.
    // We have to add the objects that are NOT moving, since these
    // will still need to be constructed locally.
    /*--------------------------------------------------------------*/

    std::vector<CommRel::CommNode> mignode;
    int es_size = zud.srcObj.size();
    int nd_size = zud.dstObj.size();

    // Mark those that have been added to move
    std::vector<int> moving(nd_size, 0);

    for (UInt i = 0; i < (UInt) numExport; i++) {
      if (exportGids[2*i] == 1) { // a node
        moving[exportLids[i]-es_size] = 1;
      }
    }

    // Add in all the local objects that are not moving
    for (UInt i = 0; i < nd_size; i++) {
      if (moving[i] == 0) {
        CommRel::CommNode cn(zud.dstObj[i], rank);
        mignode.push_back(cn);
      }
    }

    // Fill This with the migrating elements
    for (UInt i = 0; i < (UInt) numExport; i++) {
      if (exportGids[2*i] == 1) { // a node
        MeshObj &obj = *zud.dstObj[exportLids[i]-es_size];
        CommRel::CommNode cn(&obj, exportProcs[i]);
        mignode.push_back(cn);
      }
    }
    
    // Now build the comm from mignode
    CommRel &dst_migration = dstComm.GetCommRel(MeshObj::NODE);
    dst_migration.Init("dst_migration", dstmesh, dstmesh_rend, false);
    dst_migration.add_domain(mignode);
    
  } // node/interp case
}

void GeomRend::prep_meshes() {
  Trace __trace("GeomRend::prep_meshes()");
  
  // Source Mesh //
  // Mesh dims
  srcmesh_rend.set_spatial_dimension(srcmesh.spatial_dim());
  srcmesh_rend.set_parametric_dimension(srcmesh.parametric_dim());
  
  // Contexts
  srcmesh_rend.AssumeContexts(srcmesh);
  
  // Coordfield
  MEField<> &scoord = *srcmesh.GetCoordField();
  srcmesh_rend.RegisterField("coordinates", scoord.GetMEFamily(),
                   MeshObj::ELEMENT, scoord.GetContext(), scoord.dim());
  
  
  // Destination Mesh //
  
  // Mesh dims
  dstmesh_rend.set_spatial_dimension(dstmesh.spatial_dim());
  dstmesh_rend.set_parametric_dimension(dstmesh.parametric_dim());
  
  // Contexts
  dstmesh_rend.AssumeContexts(dstmesh); 
  
  // Coordfield.  If dstmesh if 'conserv', register coordinates as usual.  If it
  // is nodal, we cannot do this, since we have no elements to host the MEField.
  // Instead, just register the low-level field and operate with that.
  if (iter_is_obj) {
    MEField<> &dcoord = *dstmesh.GetCoordField();
    dstmesh_rend.RegisterField("coordinates", dcoord.GetMEFamily(), MeshObj::ELEMENT,
                        dcoord.GetContext(), dcoord.dim());
  } else {
    
     MEField<> &dcoord = *dstmesh.GetCoordField();
    _field *cf = dcoord.GetNodalfield();
    
    dstmesh_rend.Registerfield(cf->name(), cf->GetAttr(), dcoord.FType(), cf->dim());
    
  }
  
}

void GeomRend::migrate_meshes() { 
  Trace __trace("GeomRend::migrate_meshes()");
  
  // First the sourcemesh
  srcComm.GetCommRel(MeshObj::NODE).build_range(true);
  srcComm.GetCommRel(MeshObj::EDGE).build_range(true);
  srcComm.GetCommRel(MeshObj::FACE).build_range(true);
  srcComm.GetCommRel(MeshObj::ELEMENT).build_range(true);
  
  MEField<> *sc = srcmesh.GetCoordField();
  MEField<> *sc_r = srcmesh_rend.GetCoordField();
  
  srcmesh_rend.Commit();
  
  srcComm.SendFields(1, &sc, &sc_r);
  
  
  // And now the destination
  dstComm.GetCommRel(MeshObj::NODE).build_range(true);
  
  // Only for the 'conserv' case
  if (iter_is_obj) {
    dstComm.GetCommRel(MeshObj::EDGE).build_range(true);
    dstComm.GetCommRel(MeshObj::FACE).build_range(true);
    dstComm.GetCommRel(MeshObj::ELEMENT).build_range(true);
  }
  
  MEField<> *dc = dstmesh.GetCoordField();

  
  dstmesh_rend.Commit();
  
  if (iter_is_obj) {
    
    MEField<> *dc_r = dstmesh_rend.GetCoordField();
    dstComm.SendFields(1, &dc, &dc_r);
    
  } else {
    
    _field *dcf = dc->GetNodalfield();
    _field *dc_rf = dstmesh_rend.Getfield("coordinates_1");
    ThrowRequire(dc_rf);
    
    CommRel &dst_node = dstComm.GetCommRel(MeshObj::NODE);
    Trace __trace("dst_node pre send fields->");
    dst_node.send_fields(1, &dcf, &dc_rf);
        Trace __trace1("dst_node post send fields->");
          
  }  
}

void GeomRend::Build(UInt nsrcF, MEField<> **srcF, UInt ndstF, MEField<> **dstF) {
  Trace __trace("GeomRend::Build()");

  ThrowRequire(built == false);
  built = true;
  
  // Should not use neighbors flag unless source has ghosting enabled
  ThrowRequire(!dcfg.neighbors || srcmesh.HasGhost());
	
  ZoltanUD zud(sdim, srcmesh.GetCoordField(), dstmesh.GetCoordField());

  // Gather the destination points.  Also get a min/max
  double cmin[3], cmax[3];

  build_dest(cmin, cmax, zud);

  BBox dstBound = BBoxParUnion(BBox(sdim, cmin, cmax));

  build_src(dstBound, zud);

  float ver;
  int rc = Zoltan_Initialize(0, NULL, &ver);

  struct Zoltan_Struct *zz;
  int rank = Par::Rank(); 
  int csize = Par::Size(); 

  zz = Zoltan_Create(MPI_COMM_WORLD);

  // Zoltan Parameters
  set_zolt_param(zz);



  // Local vars needed by zoltan
  int changes;
  int numGidEntries;
  int numLidEntries;
  int numImport;
  ZOLTAN_ID_PTR importGlobalids;
  ZOLTAN_ID_PTR importLocalids;
  int *importProcs;
  int *importToPart;
  int numExport;
  ZOLTAN_ID_PTR exportGlobalids;
  ZOLTAN_ID_PTR exportLocalids;
  int *exportProcs;
  int *exportToPart;

  // Set the mesh description callbacks
  Zoltan_Set_Num_Obj_Fn(zz, GetNumAssignedObj, (void*) &zud);
  Zoltan_Set_Obj_List_Fn(zz, GetObjList, (void*) &zud);
  Zoltan_Set_Num_Geom_Fn(zz, GetNumGeom, (void*) &zud);
  Zoltan_Set_Geom_Multi_Fn(zz, GetObject, (void*) &zud);


  // Call zoltan
  rc = Zoltan_LB_Partition(zz, &changes, &numGidEntries, &numLidEntries,
    &numImport, &importGlobalids, &importLocalids, &importProcs, &importToPart,
    &numExport, &exportGlobalids, &exportLocalids, &exportProcs, &exportToPart);

  // Build up the source migration comm using the RCB cuts
  build_src_mig(zz, zud);

  // Build the destination migration
  build_dst_mig(zz, zud, numExport, exportLocalids, exportGlobalids, exportProcs);

  // Ok.  Done with zud lists, so free them in the intests of memory
  std::vector<MeshObj*>().swap(zud.srcObj);
  std::vector<MeshObj*>().swap(zud.dstObj);
  
  // Set up the rendezvous mesh metadata
  prep_meshes();
  
  // Register the necessary fields (could put this in a function, but passing the args
  // would be a pain)
  for (UInt i = 0; i < nsrcF; i++) {
    MEField<> *fptr = srcF[i];
      src_rend_Fields.push_back(srcmesh_rend.RegisterField(fptr->name(), fptr->GetMEFamily(),
                   fptr->ObjType(), fptr->GetContext(), fptr->dim(), true, false, fptr->FType()));
  }
  // Slightly different for destination; use interp field if not 'conserv'
  for (UInt i = 0; i < ndstF; i++) {
    MEField<> *fptr = dstF[i];
    if (iter_is_obj) {
      dst_rend_Fields.push_back(dstmesh_rend.RegisterField(fptr->name(), fptr->GetMEFamily(),
                   fptr->ObjType(), fptr->GetContext(), fptr->dim(), true, false, fptr->FType()));
    } else {
      _field *ifptr = fptr->GetInterp();
      ThrowRequire(ifptr);
      dst_rend_fields.push_back(dstmesh_rend.Registerfield(ifptr->name(), ifptr->GetAttr(), fptr->FType(), fptr->dim()));
    }
  }


  // Now migrate the meshes.
  migrate_meshes();
  
  WriteMesh(srcmesh_rend, "srcrend");


  // Now, IMPORTANT: We transpose the destination comm since this is how is will be
  // used until destruction.
  dstComm.Transpose();

  // Release zoltan memory
  Zoltan_LB_Free_Part(&importGlobalids, &importLocalids,
                      &importProcs, &importToPart);
  Zoltan_LB_Free_Part(&exportGlobalids, &exportLocalids,
                      &exportProcs, &exportToPart);


  Zoltan_Destroy(&zz);

}

} // namespace
} // namespace
