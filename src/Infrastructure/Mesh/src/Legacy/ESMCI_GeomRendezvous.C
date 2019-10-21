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
#include <Mesh/include/Legacy/ESMCI_GeomRendezvous.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_BBox.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_MeshRead.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjConn.h>
#include <Mesh/include/Legacy/ESMCI_MeshVTK.h>

#include <Mesh/src/Zoltan/zoltan.h>

#include <limits>

// #define ESMF_REGRID_DEBUG_MAP_ELEM1 836800
// #define ESMF_REGRID_DEBUG_MAP_ELEM2 836801
// #define ESMF_REGRID_DEBUG_MAP_NODE 4323801
// #define ESMF_REGRID_DEBUG_MAP_ANY

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

/*-----------------------------------------------------------------------------------*/
// Zoltan callbacks
/*-----------------------------------------------------------------------------------*/
typedef std::vector<MeshObj*> MeshObjVect;
static int GetNumAssignedObj(void *user, int *err) {
  GeomRend::ZoltanUD &udata = *(static_cast<GeomRend::ZoltanUD*>(user));
  *err = 0;

  int num=0;
  if (udata.src_pointlist == NULL)
    num+=udata.srcObj.size();
  else
    num+=udata.src_pointlist->get_curr_num_pts();

  if (udata.dst_pointlist == NULL)
    num+=udata.dstObj.size();
  else
    num+=udata.dst_pointlist->get_curr_num_pts();

  return num;
}

static void GetObjList(void *user, int numGlobalIds, int numLids, ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids,
          int wgt_dim, float *obj_wghts, int *err)
{
  GeomRend::ZoltanUD &udata = *(static_cast<GeomRend::ZoltanUD*>(user));

  MeshObjVect::iterator ni,ne;
  UInt i = 0;

  if (udata.src_pointlist == NULL) {
    ni = udata.srcObj.begin(), ne = udata.srcObj.end();
    for (; ni != ne; ++ni) {
      gids[2*i] = 0;
      gids[2*i + 1] = (*ni)->get_id();
      lids[i] = i;
      i++;
    }
  } else {
    int pl_size=udata.src_pointlist->get_curr_num_pts();
    for (int loc=0; loc<pl_size; loc++) {
      gids[2*i] = 0;
      gids[2*i + 1] = loc;  //udata.src_pointlist->get_id(loc);
      lids[i] = i;

      i++;
    }
  }


  if (udata.dst_pointlist == NULL) {
    ni = udata.dstObj.begin(), ne = udata.dstObj.end();
    for (; ni != ne; ++ni) {
      gids[2*i] = 1;
      gids[2*i + 1] = (*ni)->get_id();
      lids[i] = i;
      i++;
    }
  } else {
    int pl_size=udata.dst_pointlist->get_curr_num_pts();

    for (int loc=0; loc<pl_size; loc++) {
      gids[2*i] = 1;
      gids[2*i + 1] = loc;  //udata.dst_pointlist->get_id(loc);

      lids[i] = i;

      i++;
    }
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

  UInt list1_size;
  if (udata.src_pointlist == NULL)
    list1_size = udata.srcObj.size();
  else
    list1_size = udata.src_pointlist->get_curr_num_pts();


  for (UInt i = 0; i < (UInt) numObjs; i++) {
    UInt mesh = gids[2*i]; // 0 = src, 1 = dst
    UInt idx = lids[i];

    std::vector<double> ndata(numDim);
    const double *c;

    if (mesh == 0) {
      if (udata.src_pointlist == NULL) {
        MeshObj *elemp = udata.srcObj[idx];
        elemCentroid(*coord_src, *elemp, &ndata[0]);
        c = &ndata[0];
        // Average node coords
      } else {
        int loc = gids[2*i+1];
        c = udata.src_pointlist->get_coord_ptr(loc);
      }
    } else if (mesh == 1) {
      UInt sidx = idx - list1_size; // local indices start from first list, continue into second

      if (udata.dst_pointlist == NULL) {

        MeshObj *nodep = udata.dstObj[sidx];

        // Get destination coord depending on object type
        if (udata.iter_is_obj) {
          elemCentroid(*coord_dst, *nodep, &ndata[0]);
          c = &ndata[0];
        } else {
          c = coord_dst->data(*nodep);
        }
      } else {
        int loc = gids[2*i+1];
        c = udata.dst_pointlist->get_coord_ptr(loc);
      }

    } else throw("GetObject, unknown mesh from global id");
    for (UInt d = 0; d < (UInt) numDim; d++) pts[i*numDim + d] = c[d];
  }

}

/*-----------------------------------------------------------------------------------*/
// GeomRend functions
/*-----------------------------------------------------------------------------------*/


  GeomRend::GeomRend(Mesh *_srcmesh, PointList *_srcplist,
                     Mesh *_dstmesh, PointList *_dstplist,
                     const DstConfig &cfg, bool freeze_src_, bool _on_sph) :
                     srcmesh(_srcmesh),
                     srcplist(_srcplist),
                     dstmesh(_dstmesh),
                     dstplist(_dstplist),
                     dcfg(cfg),
                     srcComm(),
                     srcNbrComm(),
                     dstComm(),
                     built(false),
                     sdim(),
                     iter_is_obj(cfg.iter_obj_type == cfg.obj_type),
                     freeze_src(freeze_src_),
                     srcplist_rend(NULL),
                     dstplist_rend(NULL),
                     on_sph(_on_sph),
                     status(GEOMREND_STATUS_UNINIT)
{

  if (_srcplist != NULL) {
    sdim = _srcplist->get_coord_dim();
  } else {
    sdim = _srcmesh->spatial_dim();
  }

  int dst_dim;
  if (_dstplist != NULL) {
    dst_dim = _dstplist->get_coord_dim();
  } else {
    dst_dim = _dstmesh->spatial_dim();
  }


  ThrowRequire(sdim == dst_dim);
  ThrowRequire(dcfg.iter_obj_type == MeshObj::ELEMENT ||
               dcfg.iter_obj_type == MeshObj::NODE);


  // For now only allow the 'conservative case for elements (since sides have no relations otherwise)
  ThrowRequire(!iter_is_obj || dcfg.obj_type == MeshObj::ELEMENT);
}

GeomRend::~GeomRend() {
  if (srcplist_rend != NULL)
    delete srcplist_rend;
  if (dstplist_rend != NULL)
    delete dstplist_rend;
}

void GeomRend::build_dest(double cmin[], double cmax[], ZoltanUD &zud) {
  Trace __trace("GeomRend::build_dest(double cmin[], double cmax[], ZoltanUD &zud)");

  // Init cmin, cmax
  std::fill(cmin, cmin+sdim, std::numeric_limits<double>::max());
  std::fill(cmax, cmax+sdim, -std::numeric_limits<double>::max());

  MEField<> &coord = *dstmesh->GetCoordField();

  // Loop the kernels
  KernelList::iterator ki = dstmesh->set_begin(), ke = dstmesh->set_end();
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

            if (ri->type == MeshObj::USES && (ri->obj->get_type() & dcfg.obj_type)) {

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

void GeomRend::build_dest_plist(double cmin[], double cmax[], PointList *dstpointlist) {
  Trace __trace("GeomRend::build_dest_plist(double cmin[], double cmax[], Pointlist *dstpointlist)");

  // Init cmin, cmax
  std::fill(cmin, cmin+sdim, std::numeric_limits<double>::max());
  std::fill(cmax, cmax+sdim, -std::numeric_limits<double>::max());

  int pointlist_size = dstpointlist->get_curr_num_pts();
  const double *c;
  for (int i=0; i<pointlist_size; i++) {
    c=dstpointlist->get_coord_ptr(i);

    for (UInt d = 0; d < sdim; d++) {
      if (c[d] < cmin[d]) cmin[d] = c[d];
      if (c[d] > cmax[d]) cmax[d] = c[d];
    }
  }
}

void GeomRend::build_src(const BBox &dstBound, ZoltanUD &zud) {
  Trace __trace("GeomRend::build_src(const BBox &dstBound, ZoltanUD &zud)");

  MEField<> &coord = *srcmesh->GetCoordField();
  // Add all elements that intersect the bounding box of the dst

  Mesh::iterator ei = srcmesh->elem_begin(), ee = srcmesh->elem_end();
  for (; ei != ee; ++ei) {

    MeshObj &elem = *ei;

    BBox ebox(coord, elem, 0.25, on_sph);

    if (BBoxIntersect(ebox, dstBound, dcfg.geom_tol)) {
      zud.srcObj.push_back(&elem);
    }

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
                      std::vector<CommRel::CommNode> &mignode, double geom_tol, UInt sdim, bool on_sph=false) {
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

    int id;
#ifdef ESMF_REGRID_DEBUG_MAP_ANY
    id = elem.get_id();
#endif
#ifdef ESMF_REGRID_DEBUG_MAP_ELEM1
    id = elem.get_id();
#endif
#ifdef ESMF_REGRID_DEBUG_MAP_ELEM2
    id = elem.get_id();
#endif

    BBox ebox(coord, elem, geom_tol, on_sph);

    // Insersect with the cuts
    Zoltan_LB_Box_Assign(zz, ebox.getMin()[0]-geom_tol,
                             ebox.getMin()[1]-geom_tol,
                             (sdim > 2 ? ebox.getMin()[2] : 0) - geom_tol,
                             ebox.getMax()[0]+geom_tol,
                             ebox.getMax()[1]+geom_tol,
                             (sdim > 2 ? ebox.getMax()[2] : 0) + geom_tol,
                             &procs[0],
                             &numprocs);

// need to find object id and put into an if statement
#ifdef ESMF_REGRID_DEBUG_MAP_ANY
    printf("%d# Elem %d send to procs [", Par::Rank(), id);
#endif
#ifdef ESMF_REGRID_DEBUG_MAP_ELEM1
    if (id==ESMF_REGRID_DEBUG_MAP_ELEM1)
      printf("%d# Elem %d send to procs [", Par::Rank(), id);
#endif
#ifdef ESMF_REGRID_DEBUG_MAP_ELEM2
    if (id==ESMF_REGRID_DEBUG_MAP_ELEM2)
      printf("%d# Elem %d send to procs [", Par::Rank(), id);
#endif

    // Add to comm
    for (UInt i = 0; i < (UInt) numprocs; i++) {
      CommRel::CommNode cnode(&elem, procs[i]);

      std::vector<CommRel::CommNode>::iterator lb =
        std::lower_bound(mignode.begin(), mignode.end(), cnode);

      // Add if not already there
      if (lb == mignode.end() || *lb != cnode)
        mignode.insert(lb, cnode);
#ifdef ESMF_REGRID_DEBUG_MAP_ANY
    printf("%d, ", i);
#endif
#ifdef ESMF_REGRID_DEBUG_MAP_ELEM1
    if (id==ESMF_REGRID_DEBUG_MAP_ELEM1)
      printf("%d, ", i);
#endif
#ifdef ESMF_REGRID_DEBUG_MAP_ELEM2
    if (id==ESMF_REGRID_DEBUG_MAP_ELEM2)
      printf("%d, ", i);
#endif
    } // for nproc
#ifdef ESMF_REGRID_DEBUG_MAP_ANY
    printf("]\n");
#endif
#ifdef ESMF_REGRID_DEBUG_MAP_ELEM1
    if (id==ESMF_REGRID_DEBUG_MAP_ELEM1)
      printf("]\n");
#endif
#ifdef ESMF_REGRID_DEBUG_MAP_ELEM2
    if (id==ESMF_REGRID_DEBUG_MAP_ELEM2)
      printf("]\n");
#endif
  } // for si
}

  static void add_neighbors(CommRel &src_mig, std::vector<CommRel::CommNode> &mignode_nbr) {

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

      for (UInt e = 0; e < elem.size(); e++) {

        CommRel::CommNode cnode(elem[e], proc);

        CommRel::MapType::iterator lb =
          std::lower_bound(src_mig.domain_begin(), src_mig.domain_end(), cnode);

        // It's not in the mesh src migration
        if (lb == src_mig.domain_end() || *lb != cnode) {

          // See if it's in the mignode_nbr list
          std::vector<CommRel::CommNode>::iterator lb_nbr =
            std::lower_bound(mignode_nbr.begin(), mignode_nbr.end(), cnode);

          // Add if not already there
          if (lb_nbr == mignode_nbr.end() || *lb_nbr != cnode) {
            mignode_nbr.insert(lb_nbr, cnode);
          }
        }
      }
      ++di;
    }
  } // for di
}

void GeomRend::build_src_mig(Zoltan_Struct *zz, ZoltanUD &zud) {
  Trace __trace("GeomRend::build_src_mig(Zoltan_Struct *zz, ZoltanUD &zud)");

  MEField<> &coord = *srcmesh->GetCoordField();

  std::vector<CommRel::CommNode> mignode;

  rcb_isect(zz, coord, zud.srcObj, mignode, dcfg.geom_tol, sdim, on_sph);

  // Add our result to the migspec
  CommRel &src_migration = srcComm.GetCommRel(MeshObj::ELEMENT);
  src_migration.Init("src_migration", *srcmesh, srcmesh_rend, false);
  src_migration.add_domain(mignode);

#if 0
  // DEBUG OUTPUT
  for (int i=0; i<mignode.size(); i++) {
      if (mignode[i].obj->get_id() == 137) {
        printf("%d# SMN elem_id=%d to proc=%d\n",Par::Rank(),mignode[i].obj->get_id(),mignode[i].processor);
      }
  }
#endif


#ifdef GEOM_DEBUG
Par::Out() << "** source mig:" << std::endl;
src_migration.Print(Par::Out());
#endif

  // If necessary, communicate the neighbors
  CommRel &src_nbr_migration = srcNbrComm.GetCommRel(MeshObj::ELEMENT);
  src_nbr_migration.Init("src_nbr_migration", *srcmesh, srcmesh_rend, false);
  if (dcfg.neighbors) {
    // nbr mig nodes
    std::vector<CommRel::CommNode> mignode_nbr;

    // Get neighbor elements of src_migration
    add_neighbors(src_migration, mignode_nbr);

    // Add neighbor elements that are migrating
    src_nbr_migration.add_domain(mignode_nbr);

    // Form the lower order commspecs on the neighbors
    src_nbr_migration.dependants(srcNbrComm.GetCommRel(MeshObj::NODE), MeshObj::NODE);
    src_nbr_migration.dependants(srcNbrComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
    src_nbr_migration.dependants(srcNbrComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);
  }

  // Now form the lower order commspecs
  src_migration.dependants(srcComm.GetCommRel(MeshObj::NODE), MeshObj::NODE);
  src_migration.dependants(srcComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
  src_migration.dependants(srcComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);
}


void GeomRend::build_src_mig_plist(ZoltanUD &zud, int numExport,
                                   ZOLTAN_ID_PTR exportGids,
                                   int *exportProcs, int numImport, ZOLTAN_ID_PTR importGids)
{
  Trace __trace("GeomRend::build_src_mig_plist(ZoltanUD &zud, int numExport, ZOLTAN_ID_PTR exportGids, int *exportProcs, int numImport, ZOLTAN_ID_PTR importGids)");

  int num_procs=Par::Size();
  int myrank=Par::Rank();

  SparseMsg comm;

  std::vector<int> mymoving(srcplist->get_curr_num_pts(),0);

  std::vector<int> proc_counts;
  proc_counts.resize(num_procs,0);
  std::vector< std::vector<int> > idx_list;
  idx_list.resize(num_procs);
  int num_snd_pts=0;
  for (int i=0; i<numExport; i++) {
    if (exportGids[i*2]==0) {
      num_snd_pts++;
      proc_counts[exportProcs[i]]++;
      idx_list[exportProcs[i]].push_back(exportGids[i*2+1]);
      mymoving[exportGids[i*2+1]]=1;
    }
  }

  int snd_size=(sdim+1)*sizeof(double);
  int num_snd_procs=0;
  std::vector<int> snd_pets;
  std::vector<int> snd_sizes;
  std::vector<int> snd_counts;
  std::vector< std::vector<int> > idx_list2;
  for (int i=0; i<num_procs; i++) {
    if (proc_counts[i] > 0) {
      num_snd_procs++;
      snd_pets.push_back(i);
      snd_sizes.push_back(proc_counts[i]*snd_size);
      snd_counts.push_back(proc_counts[i]);
      idx_list2.push_back(idx_list[i]);
    }
  }

  // Setup pattern and sizes
  if (num_snd_procs >0) {
    comm.setPattern(num_snd_procs, (const UInt *)&(snd_pets[0]));
    comm.setSizes((UInt *)&(snd_sizes[0]));
  } else {
    comm.setPattern(0, (const UInt *)NULL);
    comm.setSizes((UInt *)NULL);
  }

  // Reset buffers
  comm.resetBuffers();

  // Pack points into buffers
  for (int i=0; i< num_snd_procs; i++) {
    SparseMsg:: buffer *b=comm.getSendBuffer(snd_pets[i]);
    for (int j=0; j<snd_counts[i]; j++) {


      // Get index of node
      int loc=idx_list2[i][j];

      const double *pnt = zud.src_pointlist->get_coord_ptr(loc);
      int this_id = zud.src_pointlist->get_id(loc);

      // pack buf
      double buf[4]; // 4 is biggest this should be (i.e. 3D+id)
      buf[0]=pnt[0];
      buf[1]=pnt[1];
      if (sdim < 3)
        buf[2]=this_id;  //passing an int through a double...inefficient but ok?
      else {
        buf[2]=pnt[2];
        buf[3]=this_id;  //passing an int through a double...inefficient but ok?
      }

      // Push buf onto send struct
      b->push((const UChar *)buf, (UInt)snd_size);
    }
  }

  // Communicate point information
  comm.communicate();

  //create pointlist_rend
  int num_rcv_pts=0;
  for (int i=0; i<numImport; i++) {
    if (importGids[i*2]==0) {
      num_rcv_pts++;
    }
  }


  int plist_rend_size=srcplist->get_curr_num_pts() - num_snd_pts + num_rcv_pts;

  // Create source rendezvous point list (create outside of if, so will work even if 0-sized)
  srcplist_rend = new ESMCI::PointList(plist_rend_size,sdim);

  if (plist_rend_size > 0) {

    // srcplist_rend = new ESMCI::PointList(plist_rend_size,sdim);

    int orig_srcpointlist_size = srcplist->get_curr_num_pts();
    for (int i=0; i<orig_srcpointlist_size; i++) {
      if (mymoving[i] == 0) {
        int temp_id=srcplist->get_id(i);
        double *temp_pnt = (double *)srcplist->get_coord_ptr(i);
        srcplist_rend->add(temp_id,temp_pnt);
      }
    }

    int ip=0;
    for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);

      // Unpack everything from this processor
      while (!b->empty()) {
        double buf[4]; // 4 is biggest this should be (i.e. 3D+dist)

        /* XMRKX */
        b->pop((UChar *)buf, (UInt)snd_size);
        //      printf(" [%f %f %f], ",pnt[0],pnt[1],pnt[2]);

        // Unpack buf
        double pnt[3]={0.0,0.0,0.0};
        int loc_id;
        
        pnt[0]=buf[0];
        pnt[1]=buf[1];
        if (sdim < 3) {
          loc_id=(int)buf[2];
        } else {
          pnt[2]=buf[2];
          loc_id=(int)buf[3];
        }

        srcplist_rend->add(loc_id,pnt);

      }

      ip++;
    }
  }

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

    MEField<> &coord = *dstmesh->GetCoordField();

    std::vector<CommRel::CommNode> mignode;

    rcb_isect(zz, coord, zud.dstObj, mignode, dcfg.geom_tol, sdim, on_sph);

    // Add results to the migspec
    CommRel &dst_migration = dstComm.GetCommRel(dcfg.obj_type);
    //    CommRel &dst_migration = dstComm.GetCommRel(MeshObj::ELEMENT);
    dst_migration.Init("dst_migration", *dstmesh, dstmesh_rend, false);
    dst_migration.add_domain(mignode); // BOB added

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
    int es_size;
    es_size = zud.srcObj.size();

    int nd_size;
    nd_size = zud.dstObj.size();

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
    dst_migration.Init("dst_migration", *dstmesh, dstmesh_rend, false);
    dst_migration.add_domain(mignode);

  } // node/interp case
}

void GeomRend::build_dst_mig_plist(ZoltanUD &zud, int numExport,
                                   ZOLTAN_ID_PTR exportGids,
                                   int *exportProcs, int numImport, ZOLTAN_ID_PTR importGids)
{
  Trace __trace("GeomRend::build_dst_mig_plist(ZoltanUD &zud, int numExport, ZOLTAN_ID_PTR exportGids, int *exportProcs, int numImport, ZOLTAN_ID_PTR importGids)");

  int num_procs=Par::Size();
  int myrank=Par::Rank();

  SparseMsg comm;

  std::vector<int> mymoving(dstplist->get_curr_num_pts(),0);

  std::vector<int> proc_counts;
  proc_counts.resize(num_procs,0);
  std::vector< std::vector<int> > idx_list;
  idx_list.resize(num_procs);
  int num_snd_pts=0;
  for (int i=0; i<numExport; i++) {
    if (exportGids[i*2]==1) {
      num_snd_pts++;
      proc_counts[exportProcs[i]]++;
      idx_list[exportProcs[i]].push_back(exportGids[i*2+1]);
      mymoving[exportGids[i*2+1]]=1;
#ifdef ESMF_REGRID_DEBUG_MAP_ANY
      printf("%d# LOOP1: Node %d send by [%d]\n", Par::Rank(), exportGids[i*2+1], exportProcs[i]);
#endif
    }
  }

  int snd_size=(sdim+1)*sizeof(double);
  int num_snd_procs=0;
  std::vector<int> snd_pets;
  std::vector<int> snd_sizes;
  std::vector<int> snd_counts;
  std::vector< std::vector<int> > idx_list2;
  for (int i=0; i<num_procs; i++) {
    if (proc_counts[i] > 0) {
      num_snd_procs++;
      snd_pets.push_back(i);
      snd_sizes.push_back(proc_counts[i]*snd_size);
      snd_counts.push_back(proc_counts[i]);
      idx_list2.push_back(idx_list[i]);
#ifdef ESMF_REGRID_DEBUG_MAP_ANY
      printf("%d# LOOP2: Proc %d to send nodes [", Par::Rank(), i);
      for (int j = 0 ; j < idx_list[i].size(); ++j)
        printf("%d, ", idx_list[i].at(j));
      printf("]\n");
#endif
    }
  }


  // Setup pattern and sizes
  if (num_snd_procs >0) {
    comm.setPattern(num_snd_procs, (const UInt *)&(snd_pets[0]));
    comm.setSizes((UInt *)&(snd_sizes[0]));
  } else {
    comm.setPattern(0, (const UInt *)NULL);
    comm.setSizes((UInt *)NULL);
  }

  // Reset buffers
  comm.resetBuffers();

  // Pack points into buffers
  for (int i=0; i< num_snd_procs; i++) {
    SparseMsg:: buffer *b=comm.getSendBuffer(snd_pets[i]);
    for (int j=0; j<snd_counts[i]; j++) {


      // Get index of node
      int loc=idx_list2[i][j];

      const double *pnt = zud.dst_pointlist->get_coord_ptr(loc);
      int this_id = zud.dst_pointlist->get_id(loc);

      // pack buf
      double buf[4]; // 4 is biggest this should be (i.e. 3D+id)
      buf[0]=pnt[0];
      buf[1]=pnt[1];
      if (sdim < 3)
        buf[2]=this_id;  //passing an int through a double...inefficient but ok?
      else {
        buf[2]=pnt[2];
        buf[3]=this_id;  //passing an int through a double...inefficient but ok?
      }

      // Push buf onto send struct
      b->push((const UChar *)buf, (UInt)snd_size);
    }
  }

  // Communicate point information
  comm.communicate();

  //create pointlist_rend
  int num_rcv_pts=0;
  for (int i=0; i<numImport; i++) {
    if (importGids[i*2]==1) {
      num_rcv_pts++;
    }
  }

  int plist_rend_size=dstplist->get_curr_num_pts() - num_snd_pts + num_rcv_pts;

  // Create destination rendezvous point list (create outside of if, so will work even if 0-sized)
  dstplist_rend = new ESMCI::PointList(plist_rend_size,sdim);

  if (plist_rend_size >= 0) {

    // dstplist_rend = new ESMCI::PointList(plist_rend_size,sdim);

    int orig_dstpointlist_size = dstplist->get_curr_num_pts();
    for (int i=0; i<orig_dstpointlist_size; i++) {
      if (mymoving[i] == 0) {
        int temp_id=dstplist->get_id(i);
        double *temp_pnt = (double *)dstplist->get_coord_ptr(i);
        dstplist_rend->add(temp_id,temp_pnt);
      }
    }

    int ip=0;
    for (std::vector<UInt>::iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);

      // Unpack everything from this processor
      while (!b->empty()) {
        double buf[4]; // 4 is biggest this should be (i.e. 3D+dist)

        /* XMRKX */
        b->pop((UChar *)buf, (UInt)snd_size);
        //      printf(" [%f %f %f], ",pnt[0],pnt[1],pnt[2]);

        // Unpack buf
        double pnt[3]={0.0,0.0,0.0};
        int loc_id;
        
        pnt[0]=buf[0];
        pnt[1]=buf[1];
        if (sdim < 3) {
          loc_id=(int)buf[2];
        } else {
          pnt[2]=buf[2];
          loc_id=(int)buf[3];
        }

        dstplist_rend->add(loc_id,pnt);

      }

      ip++;
    }
  }

}

void GeomRend::prep_meshes() {
  Trace __trace("GeomRend::prep_meshes()");

  if (srcplist == NULL) {
  // Source Mesh //
  // Mesh dims
  if(!freeze_src){
    srcmesh_rend.set_spatial_dimension(srcmesh->spatial_dim());
    srcmesh_rend.set_parametric_dimension(srcmesh->parametric_dim());

    // Contexts
    srcmesh_rend.AssumeContexts(*srcmesh);

    // Coordfield
    MEField<> &scoord = *srcmesh->GetCoordField();
    srcmesh_rend.RegisterField("coordinates", scoord.GetMEFamily(),
                     MeshObj::ELEMENT, scoord.GetContext(), scoord.dim());

    MEField<> *smask = srcmesh->GetField("mask");
    if (smask != NULL) {
      srcmesh_rend.RegisterField("mask", smask->GetMEFamily(),
       MeshObj::ELEMENT, smask->GetContext(), smask->dim());
    }


    MEField<> *src_elem_mask = srcmesh->GetField("elem_mask");
    if (src_elem_mask != NULL) {
      srcmesh_rend.RegisterField("elem_mask", src_elem_mask->GetMEFamily(),
       MeshObj::ELEMENT, src_elem_mask->GetContext(), src_elem_mask->dim());
    }


    MEField<> *src_elem_area = srcmesh->GetField("elem_area");
    if (src_elem_area != NULL) {
      srcmesh_rend.RegisterField("elem_area", src_elem_area->GetMEFamily(),
       MeshObj::ELEMENT, src_elem_area->GetContext(), src_elem_area->dim());
    }

    MEField<> *src_elem_frac2 = srcmesh->GetField("elem_frac2");
    if (src_elem_frac2 != NULL) {
      srcmesh_rend.RegisterField("elem_frac2", src_elem_frac2->GetMEFamily(),
       MeshObj::ELEMENT, src_elem_frac2->GetContext(), src_elem_frac2->dim());
    }

  }
  }

  if (dstplist == NULL) {

  // Destination Mesh //

  // Mesh dims
  dstmesh_rend.set_spatial_dimension(dstmesh->spatial_dim());
  dstmesh_rend.set_parametric_dimension(dstmesh->parametric_dim());

  // Contexts
  dstmesh_rend.AssumeContexts(*dstmesh);

  // Coordfield.  If dstmesh if 'conserv', register coordinates as usual.  If it
  // is nodal, we cannot do this, since we have no elements to host the MEField.
  // Instead, just register the low-level field and operate with that.
  if (iter_is_obj) {
    MEField<> &dcoord = *dstmesh->GetCoordField();
    dstmesh_rend.RegisterField("coordinates", dcoord.GetMEFamily(), MeshObj::ELEMENT,
                               dcoord.GetContext(), dcoord.dim());

    MEField<> *dst_elem_mask = dstmesh->GetField("elem_mask");
    if (dst_elem_mask != NULL) {
      dstmesh_rend.RegisterField("elem_mask", dst_elem_mask->GetMEFamily(),
                                 MeshObj::ELEMENT, dst_elem_mask->GetContext(), dst_elem_mask->dim());
    }


    MEField<> *dst_elem_area = dstmesh->GetField("elem_area");
    if (dst_elem_area != NULL) {
      dstmesh_rend.RegisterField("elem_area", dst_elem_area->GetMEFamily(),
                                 MeshObj::ELEMENT, dst_elem_area->GetContext(), dst_elem_area->dim());
    }

    MEField<> *dst_elem_frac2 = dstmesh->GetField("elem_frac2");
    if (dst_elem_frac2 != NULL) {
      dstmesh_rend.RegisterField("elem_frac2", dst_elem_frac2->GetMEFamily(),
                                 MeshObj::ELEMENT, dst_elem_frac2->GetContext(), dst_elem_frac2->dim());
    }

  } else {


     MEField<> &dcoord = *dstmesh->GetCoordField();
    _field *cf = dcoord.GetNodalfield();

    dstmesh_rend.Registerfield(cf->name(), cf->GetAttr(), dcoord.FType(), cf->dim());

    // Do masks, if they exist
    MEField<> *dmptr = dstmesh->GetField("mask");

    if (dmptr != NULL) {
      _field *mf = dmptr->GetNodalfield();

      dstmesh_rend.Registerfield(mf->name(), mf->GetAttr(), dmptr->FType(), mf->dim());
    }
  }
  }

}

void GeomRend::src_migrate_meshes() {
  Trace __trace("GeomRend::migrate_meshes()");

  if (srcplist == NULL) {
  if(!freeze_src){
    // The actual mesh
    srcComm.GetCommRel(MeshObj::NODE).build_range();
    srcComm.GetCommRel(MeshObj::EDGE).build_range();
    srcComm.GetCommRel(MeshObj::FACE).build_range();
    srcComm.GetCommRel(MeshObj::ELEMENT).build_range();


    // Now the neighbors if necessary
    if (dcfg.neighbors) {
      srcNbrComm.GetCommRel(MeshObj::NODE).build_range(true);
      srcNbrComm.GetCommRel(MeshObj::EDGE).build_range(true);
      srcNbrComm.GetCommRel(MeshObj::FACE).build_range(true);
      srcNbrComm.GetCommRel(MeshObj::ELEMENT).build_range(true);
    }

    // Finalize the mesh
    srcmesh_rend.Commit();

    // Now send the fields
    int num_snd=0;
    MEField<> *snd[5],*rcv[5];

    MEField<> *sc = srcmesh->GetCoordField();
    MEField<> *sc_r = srcmesh_rend.GetCoordField();

    // load coordinate fields
    snd[num_snd]=sc;
    rcv[num_snd]=sc_r;
    num_snd++;

    // Do masks if necessary
    MEField<> *sm = srcmesh->GetField("mask");
    if (sm != NULL) {
      MEField<> *sm_r = srcmesh_rend.GetField("mask");

      // load mask fields
      snd[num_snd]=sm;
      rcv[num_snd]=sm_r;
      num_snd++;
    }

    // Do elem masks if necessary
    MEField<> *sem = srcmesh->GetField("elem_mask");
    if (sem != NULL) {
      MEField<> *sem_r = srcmesh_rend.GetField("elem_mask");

      // load mask fields
      snd[num_snd]=sem;
      rcv[num_snd]=sem_r;
      num_snd++;
    }

    // Do elem masks if necessary
    MEField<> *sea = srcmesh->GetField("elem_area");
    if (sea != NULL) {
      MEField<> *sea_r = srcmesh_rend.GetField("elem_area");

      // load mask fields
      snd[num_snd]=sea;
      rcv[num_snd]=sea_r;
      num_snd++;
    }

    // Do elem creeped frac if necessary
    MEField<> *sef = srcmesh->GetField("elem_frac2");
    if (sef != NULL) {
      MEField<> *sef_r = srcmesh_rend.GetField("elem_frac2");

      // load mask fields
      snd[num_snd]=sef;
      rcv[num_snd]=sef_r;
      num_snd++;
    }

    // For the actual mesh
    srcComm.SendFields(num_snd, snd, rcv);

    // For the neighbors
    if (dcfg.neighbors) {
      srcNbrComm.SendFields(num_snd, snd, rcv);
    }
  }
  }
}


void GeomRend::dst_migrate_meshes() {
  Trace __trace("GeomRend::migrate_meshes()");

  if (dstplist == NULL) {

  // And now the destination
  dstComm.GetCommRel(MeshObj::NODE).build_range();

  // Only for the 'conserv' case
  if (iter_is_obj) {
    dstComm.GetCommRel(MeshObj::EDGE).build_range();
    dstComm.GetCommRel(MeshObj::FACE).build_range();
    dstComm.GetCommRel(MeshObj::ELEMENT).build_range();
  }


  dstmesh_rend.Commit();

  if (iter_is_obj) {
    int num_snd=0;
    MEField<> *snd[5],*rcv[5];

    MEField<> *dc = dstmesh->GetCoordField();
    MEField<> *dc_r = dstmesh_rend.GetCoordField();

    // load coordinate fields
    snd[num_snd]=dc;
    rcv[num_snd]=dc_r;
    num_snd++;

    // Do masks if necessary
    MEField<> *dm = dstmesh->GetField("mask");
    if (dm != NULL) {
      MEField<> *dm_r = dstmesh_rend.GetField("mask");

      // load mask fields
      snd[num_snd]=dm;
      rcv[num_snd]=dm_r;
      num_snd++;
    }

    // Do elem masks if necessary
    MEField<> *dem = dstmesh->GetField("elem_mask");
    if (dem != NULL) {
      MEField<> *dem_r = dstmesh_rend.GetField("elem_mask");

      // load mask fields
      snd[num_snd]=dem;
      rcv[num_snd]=dem_r;
      num_snd++;
    }

    // Do elem area if necessary
    MEField<> *dea = dstmesh->GetField("elem_area");
    if (dea != NULL) {
      MEField<> *dea_r = dstmesh_rend.GetField("elem_area");

      // load mask fields
      snd[num_snd]=dea;
      rcv[num_snd]=dea_r;
      num_snd++;
    }

    // Do elem creeped frac if necessary
    MEField<> *def = dstmesh->GetField("elem_frac2");
    if (def != NULL) {
      MEField<> *def_r = dstmesh_rend.GetField("elem_frac2");

      // load mask fields
      snd[num_snd]=def;
      rcv[num_snd]=def_r;
      num_snd++;
    }

     dstComm.SendFields(num_snd, snd, rcv);

  } else {
    int num_snd=0;
    _field *snd[2],*rcv[2];

    MEField<> *dc = dstmesh->GetCoordField();
    MEField<> *dm = dstmesh->GetField("mask");

    _field *dcf = dc->GetNodalfield();
    _field *dc_rf = dstmesh_rend.Getfield("coordinates_1");
    ThrowRequire(dc_rf);

    // load coordinate fields
    snd[num_snd]=dcf;
    rcv[num_snd]=dc_rf;
    num_snd++;

    if (dm != NULL) {
      _field *dmf = dm->GetNodalfield();
      _field *dm_rf = dstmesh_rend.Getfield("mask_1");
      ThrowRequire(dm_rf);

      // load mask fields
      snd[num_snd]=dmf;
      rcv[num_snd]=dm_rf;
      num_snd++;
    }

    CommRel &dst_node = dstComm.GetCommRel(MeshObj::NODE);
    Trace __trace("dst_node pre send fields->");
    dst_node.send_fields(num_snd, snd, rcv);
        Trace __trace1("dst_node post send fields->");

  }
  }
}

// do both src and dst migration with one call
void GeomRend::migrate_meshes() {
  Trace __trace("GeomRend::migrate_meshes()");

  src_migrate_meshes();
  dst_migrate_meshes();
}



void mesh_isect(const MEField<> & scoord, const BBox & srcBBox, const MEField<> & dcoord, std::vector<MeshObj *> objlist,
  std::vector<CommRel::CommNode> & mignode, double tol, int sdim){

  // all gather all srcBBox on all pets
  // construct local buffer
  int commsize = Par::Size();
  int commrank = Par::Rank();
  char * sendbuff = new char [sizeof(BBox)];
  char * recvbuff = new char [sizeof(BBox)*commsize];
  std::memcpy((void *)sendbuff, (void *)&srcBBox, sizeof(srcBBox));
  MPI_Allgather(sendbuff, sizeof(BBox), MPI_BYTE, recvbuff, sizeof(BBox), MPI_BYTE, Par::Comm());
  std::vector<BBox> srcBBoxList(commsize);
  for(int i = 0; i < commsize; i ++)
    std::memcpy((void *)&(srcBBoxList[i]), (void *)(recvbuff+i*sizeof(BBox)), sizeof(BBox));
  delete[] sendbuff, recvbuff;

  // check if any dstOBjList intersects with the srcBBox, calculate proclist and add to mignode list
  std::vector<MeshObj*>::iterator si = objlist.begin(), se = objlist.end();
  int numprocs = 0;
  std::vector<int> procs(commsize);

  for (; si != se; ++si) {
    MeshObj &elem = **si;
    BBox ebox(dcoord, elem, 0.25);
    // Insersect with each of the src bounding box
    for(int i = 0; i < commsize; i ++)
      if (BBoxIntersect(ebox, srcBBoxList[i], tol))
        procs[numprocs++] = i;

    // Add to comm
    for (UInt i = 0; i < (UInt) numprocs; i++) {
      CommRel::CommNode cnode(&elem, procs[i]);
      std::vector<CommRel::CommNode>::iterator lb =
        std::lower_bound(mignode.begin(), mignode.end(), cnode);
      // Add if not already there
      if (lb == mignode.end() || *lb != cnode)
        mignode.insert(lb, cnode);

    } // for nproc

    numprocs = 0;
  } // for si
}

void GeomRend::Build_Merge(UInt nsrcF, MEField<> **srcF, UInt ndstF, MEField<> **dstF, struct Zoltan_Struct **zzp) {
  // Should not use neighbors flag unless source has ghosting enabled
  ThrowRequire(!dcfg.neighbors || srcmesh->HasGhost());
        
  MEField<> &scoord = *srcmesh->GetCoordField();
  BBox srcBBox(scoord, *srcmesh);
  BBox gsrcBBox = BBoxParUnion(srcBBox);

  MEField<> &dcoord = *dstmesh->GetCoordField();
  // Because src mesh is not allowed to migrate,
  // Add all dst mesh elements that intersect the global bounding box of the src
  // to a separate list for zoltan to compute dst proc
  std::vector<MeshObj *> dstObjList;
  Mesh::iterator ei = dstmesh->elem_begin(), ee = dstmesh->elem_end();
  for (; ei != ee; ++ei) {
    MeshObj &elem = *ei;
    BBox ebox(dcoord, elem, 0.25);
    if (BBoxIntersect(ebox, gsrcBBox, dcfg.geom_tol))
      dstObjList.push_back(&elem);
  } // for ei

  // Build the destination migration
  std::vector<CommRel::CommNode> mignode;
  mesh_isect(scoord, srcBBox, dcoord, dstObjList, mignode, dcfg.geom_tol, sdim);
  // Add results to the migspec
  CommRel &dst_migration = dstComm.GetCommRel(dcfg.obj_type);
  dst_migration.Init("dst_migration", *dstmesh, dstmesh_rend, false);
  dst_migration.add_domain(mignode);
  // Now flush out the comm with lower hierarchy
  dst_migration.dependants(dstComm.GetCommRel(MeshObj::NODE), MeshObj::NODE);
  dst_migration.dependants(dstComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
  dst_migration.dependants(dstComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);

  // Ok.  Done with zud lists, so free them in the interests of memory
  mignode.clear();
  dstObjList.clear();

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
      _field *ifptr = fptr->is_nodal() ? fptr->GetNodalfield() : fptr->GetInterp();
      ThrowRequire(ifptr);
      dst_rend_fields.push_back(dstmesh_rend.Registerfield(ifptr->name(), ifptr->GetAttr(), fptr->FType(), fptr->dim()));
    }
  }


  // Now migrate the meshes.
  migrate_meshes();

  //char str[64]; memset(str, 0, 64);
  //sprintf(str, "srcrend.vtk.%d", Par::Rank());
  //WriteVTKMesh(dstmesh_rend, str);

  // Now, IMPORTANT: We transpose the destination comm since this is how is will be
  // used until destruction.
  dstComm.Transpose();

}

// build a destination migration based on srcmesh_rend
// the object is to have every element that intersects with a source elem
// on the same proc
void GeomRend::build_dst_mig_all_overlap(ZoltanUD &zud) {
        
  // Get srcmesh_rend coordinate field
  MEField<> &scoord = *(srcmesh_rend.GetCoordField());

  // get local and global bounding boxes of srcmesh_rend
  BBox srcBBox(scoord, srcmesh_rend);

  // Get coord field of destination mesh
  MEField<> &dcoord = *(dstmesh->GetCoordField());

  // Build the destination migration
  std::vector<CommRel::CommNode> mignode;
  mesh_isect(scoord, srcBBox, dcoord, zud.dstObj, mignode, dcfg.geom_tol, sdim);

  // Add results to the migspec
  CommRel &dst_migration = dstComm.GetCommRel(dcfg.obj_type);
  dst_migration.Init("dst_migration", *dstmesh, dstmesh_rend, false);
  dst_migration.add_domain(mignode);

  // Now flesh out the comm with lower hierarchy
  dst_migration.dependants(dstComm.GetCommRel(MeshObj::NODE), MeshObj::NODE);
  dst_migration.dependants(dstComm.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
  dst_migration.dependants(dstComm.GetCommRel(MeshObj::FACE), MeshObj::FACE);

}



void GeomRend::Build(UInt nsrcF, MEField<> **srcF, UInt ndstF, MEField<> **dstF, struct Zoltan_Struct **zzp, bool free_zz) {
  Trace __trace("GeomRend::Build()");


  ThrowRequire(built == false);
  built = true;

  if(freeze_src) {
    Build_Merge(nsrcF, srcF, ndstF, dstF, zzp);
    status=GEOMREND_STATUS_COMPLETE;
    return;
  }

  MEField<> *src_coordField_ptr, *dst_coordField_ptr;
  if (srcmesh != NULL)
    src_coordField_ptr = srcmesh->GetCoordField();
  else
    src_coordField_ptr = NULL;

  if (dstmesh != NULL)
    dst_coordField_ptr = dstmesh->GetCoordField();
  else
    dst_coordField_ptr = NULL;


  ZoltanUD zud(sdim, src_coordField_ptr, dst_coordField_ptr, srcplist, dstplist, iter_is_obj);

  // Gather the destination points.  Also get a min/max
  int num_dst_local=0;
  double cmin[3], cmax[3];
  if (dstplist != NULL)  {
    build_dest_plist(cmin, cmax, dstplist);
    num_dst_local=dstplist->get_curr_num_pts(); 
  } else {
    build_dest(cmin, cmax, zud);
    num_dst_local=zud.dstObj.size();
  }

  BBox dstBound = BBoxParUnion(BBox(sdim, cmin, cmax));

  // Get src mesh (within dst bounding box)
  int num_src_local=0;
  if (srcplist == NULL) {
    build_src(dstBound, zud);
    num_src_local = zud.srcObj.size();
  } else {
    num_src_local=srcplist->get_curr_num_pts(); 
  }

  // Compute global sums
  int local[2];
  int global_tot[2];
  local[0]=num_dst_local;
  local[1]=num_src_local;
  MPI_Allreduce(local,global_tot,2,MPI_INT,MPI_SUM,Par::Comm());

  // Leave if there are no destination points
  if (global_tot[0] == 0) {
    status=GEOMREND_STATUS_NO_DST;
    std::vector<MeshObj*>().swap(zud.dstObj);
    std::vector<MeshObj*>().swap(zud.srcObj);
    return;
  } else if (global_tot[1] == 0) {
    status=GEOMREND_STATUS_DST_BUT_NO_SRC;
    std::vector<MeshObj*>().swap(zud.dstObj);
    std::vector<MeshObj*>().swap(zud.srcObj);
    return;
  }


  float ver;
  int rc = Zoltan_Initialize(0, NULL, &ver);

  int rank = Par::Rank();
  int csize = Par::Size();


  struct Zoltan_Struct * zz = Zoltan_Create(Par::Comm());
  *zzp = zz;

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

  //for (int xx=0; xx<numImport; xx++) {
  //for (int xx=0; xx<numExport; xx++) {

  // Set up the rendezvous mesh metadata
  // for both src and dst
  prep_meshes();

  // Build up the source migration comm using the RCB cuts
  if (zud.src_pointlist == NULL)
    build_src_mig(zz, zud);
  else {
    build_src_mig_plist(zud, numExport, exportGlobalids, exportProcs, numImport, importGlobalids);
  }

  // Done with src zud lists, so free them in the intests of memory
  std::vector<MeshObj*>().swap(zud.srcObj);


  // Register the necessary fields (could put this in a function, but passing the args
  // would be a pain)
  for (UInt i = 0; i < nsrcF; i++) {
    MEField<> *fptr = srcF[i];
      src_rend_Fields.push_back(srcmesh_rend.RegisterField(fptr->name(), fptr->GetMEFamily(),
                   fptr->ObjType(), fptr->GetContext(), fptr->dim(), true, false, fptr->FType()));
  }

  // Now migrate the src mesh.
  src_migrate_meshes();


  // Build the destination migration
  if (dcfg.all_overlap_dst) {
    // Here we need all the destination cells that overlap with each source cell on the same proc
    build_dst_mig_all_overlap(zud);
  } else {
    if (zud.dst_pointlist == NULL)
      build_dst_mig(zz, zud, numExport, exportLocalids, exportGlobalids, exportProcs);
    else
      build_dst_mig_plist(zud, numExport, exportGlobalids, exportProcs, numImport, importGlobalids);
  }


  // Slightly different for destination; use interp field if not 'conserv'
  for (UInt i = 0; i < ndstF; i++) {
    MEField<> *fptr = dstF[i];
    if (iter_is_obj) {
      dst_rend_Fields.push_back(dstmesh_rend.RegisterField(fptr->name(), fptr->GetMEFamily(),
                   fptr->ObjType(), fptr->GetContext(), fptr->dim(), true, false, fptr->FType()));
    } else {
      _field *ifptr = fptr->is_nodal() ? fptr->GetNodalfield() : fptr->GetInterp();
      ThrowRequire(ifptr);
      dst_rend_fields.push_back(dstmesh_rend.Registerfield(ifptr->name(), ifptr->GetAttr(), fptr->FType(), fptr->dim()));
    }
  }

  // Ok.  Done with dst zud lists, so free them in the intests of memory
  std::vector<MeshObj*>().swap(zud.dstObj);

  // Now migrate the dst mesh.
  dst_migrate_meshes();


  //WriteMesh(srcmesh_rend, "srcrend");

  // Now, IMPORTANT: We transpose the destination comm since this is how is will be
  // used until destruction.
  if (dstplist == NULL)
    dstComm.Transpose();

  // Release zoltan memory
  Zoltan_LB_Free_Part(&importGlobalids, &importLocalids,
                      &importProcs, &importToPart);
  Zoltan_LB_Free_Part(&exportGlobalids, &exportLocalids,
                      &exportProcs, &exportToPart);

  if(free_zz){
    Zoltan_Destroy(&zz);
  }

  // Set status before leaving
  status=GEOMREND_STATUS_COMPLETE;
}

} // namespace ESMCI
