// $Id: ESMC_Zoltan.C,v 1.4 2007/08/24 19:15:45 dneckels Exp $
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
#include <ESMC_Zoltan.h>


#include <ESMC_Mesh.h>
#include <ESMC_BBox.h>
#include <ESMC_MeshUtils.h>
#include <ESMC_CommRel.h>
#include <ESMC_Transfer.h>
#include <ESMC_MeshObjConn.h>
#include <ESMC_MeshSkin.h>

#include <Zoltan/zoltan.h>

#include <mpi.h>

#include <ESMC_MeshObjTopo.h>
#include <ESMC_MeshRead.h>

#include <ESMC_Search.h>
#include <ESMC_ParEnv.h>

#include <sstream>
#include <iomanip>

#include <map>

namespace ESMCI {
namespace MESH {

typedef std::vector<MeshObj*> MeshObjVect;

struct zoltan_rend_data {
  Mesh *srcMesh;
  Mesh *dstMesh;
  BBox *mesh_isect;
  MeshObjVect elemSrc;
  MeshObjVect nodesDst;
};


static void build_lists(BBox &isect, Mesh &src, Mesh &dst, MeshObjVect &eSrc, MeshObjVect &nDst) {
  MEField<> *coord_field = src.GetCoordField();

  // If the bounding box of an element intersects with the interpolation box,
  // send all nodes.  It is not enough to check if the nodes are in the box,
  // since the box may straddle the destination mesh.
  Mesh::iterator ei = src.elem_begin(), ee = src.elem_end();
  for (; ei != ee; ++ei) {
    MeshObj &elem = *ei;
    BBox bound(*coord_field, elem);

    if (!BBoxIntersect(isect, bound, 1e-2)) continue;
    eSrc.push_back(&elem);
  }

  Mesh::iterator ni = dst.node_begin(), ne = dst.node_end();
  coord_field = dst.GetCoordField();
 
  for (; ni != ne; ++ni) {
    MeshObj &node = *ni;
    if (!GetAttr(node).is_locally_owned()) continue;
    //if (owner->data(node)[0] != rank) continue;  TODO: shouldn't send these.  should ghost later
    double *c = coord_field->data(node);

    // See if the node is in the intersecting box, and add to list if so.
    // This is slightly dumb, since we probably want to interpolate every
    // node in the dst mesh, but maybe we only want to interpolate on, say a surface
    // or something like this.  If so, BBox will be set up ahead of time
    // and this test. 
    if (BBoxPointIn(isect, c, 1e-2)) {
      nDst.push_back(&node);
    }
  }
}


// Zoltan Mesh Functions

static int GetNumAssignedObj(void *user, int *err) {
  zoltan_rend_data &udata = *(static_cast<zoltan_rend_data*>(user));
  *err = 0;
  return udata.elemSrc.size() + udata.nodesDst.size();
}

static void GetObjList(void *user, int numGlobalIds, int numLids, ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids,
          int wgt_dim, float *obj_wghts, int *err) 
{
  zoltan_rend_data &udata = *(static_cast<zoltan_rend_data*>(user));

  UInt i = 0;
  MeshObjVect::iterator ni = udata.elemSrc.begin(), ne = udata.elemSrc.end();
  for (; ni != ne; ++ni) {
    gids[2*i] = 0;
    gids[2*i + 1] = (*ni)->get_id();
    lids[i] = i;
    i++;
  }

  ni = udata.nodesDst.begin(), ne = udata.nodesDst.end();
  for (; ni != ne; ++ni) {
    gids[2*i] = 1;
    gids[2*i + 1] = (*ni)->get_id();
    lids[i] = i;
    i++;
  }

  *err = 0;
}

static int GetNumGeom(void *user, int *err) {
  zoltan_rend_data &udata = *(static_cast<zoltan_rend_data*>(user));
  *err = 0;

  return udata.srcMesh->spatial_dim();
}

static void GetObject(void *user, int numGlobalIds, int numLids, int numObjs,
  ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids, int numDim, double *pts, int *err) 
{
  zoltan_rend_data &udata = *(static_cast<zoltan_rend_data*>(user));
  MEField<> *coord_field_src = udata.srcMesh->GetCoordField();
  MEField<> *coord_field_dst = udata.dstMesh->GetCoordField();
  *err = 0;

  UInt list1_size = udata.elemSrc.size();
  for (UInt i = 0; i < (UInt) numObjs; i++) {
    UInt mesh = gids[2*i]; // 0 = src, 1 = dst
    UInt idx = lids[i];

    std::vector<double> ndata(numDim);
    double *c;
    if (mesh == 0) {
      MeshObj *elemp = udata.elemSrc[idx];
      elemCentroid(*coord_field_src, *elemp, &ndata[0]);
      c = &ndata[0];
      // Average node coords
    } else if (mesh == 1) {
      UInt sidx = idx - list1_size; // local indices start from first list, continue into second
      MeshObj *nodep = udata.nodesDst[sidx];
      c = coord_field_dst->data(*nodep);
    } else throw("GetObject, unknown mesh from global id");

    for (UInt d = 0; d < (UInt) numDim; d++) pts[i*numDim + d] = c[d];
  }
}



void ZoltanRendezvous(Mesh &srcMesh, Mesh &dstMesh, Mesh &srcR, Mesh &dstR,
        UInt num_fields, MEField<> **sfields, MEField<> **dfields,
        const UInt zinterp[])
{
  std::vector<MEField<> *> sfields_uniq;
  std::copy(&sfields[0], &sfields[num_fields], std::back_inserter(sfields_uniq));
  std::sort(sfields_uniq.begin(), sfields_uniq.end());
  sfields_uniq.erase(std::unique(sfields_uniq.begin(), sfields_uniq.end()), sfields_uniq.end());

  float ver;
  int rc = Zoltan_Initialize(0, NULL, &ver);

  struct Zoltan_Struct *zz;
  int rank = Par::Rank(); 
  int csize = Par::Size(); 

  double seconds = 0, nseconds = 0;
  MPI_Barrier(MPI_COMM_WORLD);
  seconds = MPI_Wtime();

  zz = Zoltan_Create(MPI_COMM_WORLD);

  Zoltan_Set_Param(zz, "DEBUG_LEVEL", "0");
  Zoltan_Set_Param(zz, "LB_METHOD", "RCB");
  Zoltan_Set_Param(zz, "NUM_GID_ENTRIES", "2"); // Two integers for an id
  Zoltan_Set_Param(zz, "NUM_LID_ENTRIES", "1");
  Zoltan_Set_Param(zz, "RETURN_LISTS", "ALL");
  Zoltan_Set_Param(zz, "RCB_RECTILINEAR_BLOCKS", "1");
  Zoltan_Set_Param(zz, "AVERAGE_CUTS", "1");

   
  // RCB
  Zoltan_Set_Param(zz, "RCB_LOCK_DIRECTIONS", "1");
  Zoltan_Set_Param(zz, "KEEP_CUTS", "0");
  Zoltan_Set_Param(zz, "RCB_OUTPUT_LEVEL", "0");
  //Zoltan_Set_Param(zz, "RCB_RECTILINEAR_BLOCKS", "1");

  int changes;
  int numGidEntries;
  int numLidEntries;
  int numImport;
  ZOLTAN_ID_PTR importGlobalGids;
  ZOLTAN_ID_PTR importLocalGids;
  int *importProcs;
  int *importToPart;
  int numExport;
  ZOLTAN_ID_PTR exportGlobalGids;
  ZOLTAN_ID_PTR exportLocalGids;
  int *exportProcs;
  int *exportToPart;

  MEField<> *src_coord = srcMesh.GetCoordField();
  MEField<> *dst_coord = dstMesh.GetCoordField();

  UInt sdim = srcMesh.spatial_dim();

  BBox *mesh_isect;

  {
    BBox b1(*src_coord, srcMesh);
    BBox b2(*dst_coord, dstMesh);

    BBox *a1, *a2;

    // Get global bounding boxes
    a1 = BBoxParUnion(b1);
    a2 = BBoxParUnion(b2);

    mesh_isect = BBoxIntersection(*a1, *a2);

    delete a1; delete a2;
  }


std::cout << "P:" << rank << " mesh_isec=" << *mesh_isect << std::endl;


  zoltan_rend_data zud;
  // Build the two lists of nodes that are in the box
  build_lists(*mesh_isect, srcMesh, dstMesh, zud.elemSrc, zud.nodesDst);

std::cout << "P:" << rank << " nodesSRc:" << zud.elemSrc.size() << ", nodesDst:" << zud.nodesDst.size() << std::endl;
std::cout << "P:" << rank << " srcmeshnodes:" << srcMesh.num_nodes() << ", dstmeshnodes:" << dstMesh.num_nodes() << std::endl;

  zud.srcMesh = &srcMesh; zud.dstMesh = &dstMesh;


  // Callback parameters
  Zoltan_Set_Num_Obj_Fn(zz, GetNumAssignedObj, (void*) &zud);
  Zoltan_Set_Obj_List_Fn(zz, GetObjList, (void*) &zud);
  Zoltan_Set_Num_Geom_Fn(zz, GetNumGeom, (void*) &zud);
  Zoltan_Set_Geom_Multi_Fn(zz, GetObject, (void*) &zud);


  rc = Zoltan_LB_Partition(zz, &changes, &numGidEntries, &numLidEntries,
    &numImport, &importGlobalGids, &importLocalGids, &importProcs, &importToPart,
    &numExport, &exportGlobalGids, &exportLocalGids, &exportProcs, &exportToPart);

std::cout << "Zoltan rc=" << rc << std::endl;

std::cout << "P:" << rank << ", numIMp:" << numImport << ", numExport:" << numExport << std::endl;

  // *** Migrate the source mesh to the new decomp
  
  // Create a symmetric CommSpec
  std::vector<CommRel::CommNode> srcObj;
  UInt es_size = zud.elemSrc.size();
  std::vector<int> moving(es_size, 0);


  /*
  { // temporary namespace

  std::vector<int> added(es_size, 0); // keep track of who has been added



  // set a 1 if this local element is moving
  for (UInt i = 0; i < (UInt) numExport; i++) {
    if (exportGlobalGids[2*i] == 0) { // an element
      moving[exportLocalGids[i]] = 1;
    }
  }

  // Add in all the local objects that are not moving
  for (UInt i = 0; i < es_size; i++) {
    if (moving[i] == 0) {
      CommRel::CommNode cn(zud.elemSrc[i], rank);
      srcObj.push_back(cn);
      added[i] = 1;
    }
  }
    

  // Fill This with the migrating elements
  for (UInt i = 0; i < (UInt) numExport; i++) {
    if (exportGlobalGids[2*i] == 0) { // an element
      const MeshObj &elem = *zud.elemSrc[exportLocalGids[i]];
      CommRel::CommNode cn(&elem, exportProcs[i]);
      srcObj.push_back(cn);
      added[exportLocalGids[i]] = 1;
    }

  }
*/

  // One more step here: TODO: add the boundary elements to the RCB decomp
  // Loop src blocks again, and add them to dstObj for
  // all procs they intersect
  bool send_neighbors = false;
  for (UInt i = 0; i < num_fields; i++) {
    if (zinterp[i] == ZOLT_PATCH) {
      send_neighbors = true;
      break;
    }
  }

  int numprocs;
  std::vector<int> procs(csize);
  for (UInt i = 0; i < es_size; i++) {
    //if (added[i]) continue; // only check those not yet added
    MeshObj &elem = *zud.elemSrc[i];
    BBox bound(*src_coord, elem);
    Zoltan_LB_Box_Assign(zz, bound.getMin()[0],
                             bound.getMin()[1],
                             sdim > 2 ? bound.getMin()[2] : 0,
                             bound.getMax()[0],
                             bound.getMax()[1],
                             sdim > 2 ? bound.getMax()[2] : 0,
                             &procs[0],
                             &numprocs);

    std::set<MeshObj*> nbors;
    if (send_neighbors) {
      const MeshObjTopo *topo = GetMeshObjTopo(elem);
      for (UInt f = 0; f < topo->num_sides; f++) {
        MeshObj *el = MeshObjConn::opposite_element(elem, f);
        if (el) nbors.insert(el);
      }
    }
    for (UInt p = 0; p < (UInt) numprocs; p++) {
      srcObj.push_back(CommRel::CommNode(&elem, procs[p]));
      if (send_neighbors) {
        std::set<MeshObj*>::iterator si = nbors.begin(), se = nbors.end();
        for (; si != se; ++si) {
          srcObj.push_back(CommRel::CommNode(*si, procs[p]));
        }
      } // if sn
    } // p
                             
  }

  // unique srcObj
  std::sort(srcObj.begin(), srcObj.end());
  srcObj.erase(std::unique(srcObj.begin(), srcObj.end()), srcObj.end());



  srcR.set_parametric_dimension(srcMesh.parametric_dim());
  srcR.set_spatial_dimension(srcMesh.spatial_dim());
  srcR.AssumeContexts(srcMesh); // same set structure
  CommReg sRcr;
  CommRel &src_migration = sRcr.GetCommRel(MeshObj::ELEMENT);
  src_migration.Init("src_migration", srcMesh, srcR, false);
  src_migration.add_domain(srcObj);
  
  std::vector<CommRel::CommNode>().swap(srcObj);
  //src_migration->Print();

  
  CommRel &src_node_migration = src_migration.dependants(sRcr.GetCommRel(MeshObj::NODE), MeshObj::NODE);
  CommRel &src_edge_migration = src_migration.dependants(sRcr.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
  src_node_migration.build_range(true);
  src_edge_migration.build_range(true);
  src_migration.build_range(true);

  // Commit srcmesh
  srcR.linearize_data_index();
  //ResolveParSkin(srcR);

  // At this point we can send over fields (for instance coords)
  //srcR.Print();


  // **** Migrate the destination nodes
  // Create a symmetric CommSpec
  std::vector<CommRel::CommNode> dstObj;

  // set a 1 if this local element is moving
  UInt nd_size = zud.nodesDst.size();
  moving.resize(nd_size);
  for (UInt i = 0; i < nd_size; i++) moving[i] = 0;
  for (UInt i = 0; i < (UInt) numExport; i++) {
    if (exportGlobalGids[2*i] == 1) { // a node
      moving[exportLocalGids[i]-es_size] = 1;
    }
  }

  // Add in all the local objects that are not moving
  for (UInt i = 0; i < nd_size; i++) {
    if (moving[i] == 0) {
      CommRel::CommNode cn(zud.nodesDst[i], rank);
      dstObj.push_back(cn);
    }
  }
    

  // Fill This with the migrating elements
  for (UInt i = 0; i < (UInt) numExport; i++) {
    if (exportGlobalGids[2*i] == 1) { // a node
      MeshObj &node = *zud.nodesDst[exportLocalGids[i]-es_size];
      CommRel::CommNode cn(&node, exportProcs[i]);
      dstObj.push_back(cn);
    }

  }


  dstR.AssumeContexts(dstMesh); // same set structure
  CommReg dRcr;
  CommRel *dst_migration1 = new CommRel("dst_migration", dstMesh, dstR);
  dst_migration1->add_domain(dstObj);
  CommRel &dst_elem = dst_migration1->ancestors(dRcr.GetCommRel(MeshObj::ELEMENT));
  CommRel &dst_node = dst_elem.dependants(dRcr.GetCommRel(MeshObj::NODE), MeshObj::NODE);
  CommRel &dst_edge = dst_elem.dependants(dRcr.GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
  std::vector<CommRel::CommNode>().swap(dstObj); // clear
  dst_node.build_range(true);
  dst_edge.build_range(true);
  dst_elem.build_range(true);

  delete dst_migration1;

  dstR.set_parametric_dimension(dstMesh.parametric_dim());
  dstR.set_spatial_dimension(dstMesh.spatial_dim());
  dstR.linearize_data_index();

  std::cout << "Zolt:Finished sending source mesh" << std::endl;


/*
  Field<NodalField> *s = Field<NodalField>::get_field(srcMesh, "source");
  if (!s) throw("where is s??");
  Field<NodalField> *st = new Field<NodalField>(srcR, "source");
st->set_output_status(true);
  src_node_migration->send_fields(1,s);
*/
  // Create the fields on the rendezvous
  std::vector<MEField<>*> st(num_fields,NULL);
  std::vector<MEField<>*> st_uniq(num_fields,NULL);
  std::vector<MEField<>*> dt(num_fields,NULL);
  UInt nsfields = 0;
  for (UInt i = 0; i < num_fields; i++) {
    // source field may be going to several fields (several methods)
    if (!(st[i] = srcR.GetField(sfields[i]->name()))) {

       st[i] =  srcR.RegisterField(sfields[i]->name(), sfields[i]->GetMEFamily(), MeshObj::ELEMENT, 
                        sfields[i]->GetContext(), sfields[i]->dim(), true);
Par::Out() << "Registered:" << sfields[i]->name() << " on srcR, context:" << sfields[i]->GetContext() << std::endl;
       st_uniq[nsfields++] = st[i];
    }
    
    dt[i] =  dstR.RegisterField(dfields[i]->name(), dfields[i]->GetMEFamily(), MeshObj::ELEMENT, 
                        dfields[i]->GetContext(), dfields[i]->dim(), true);
  }

  // Create and send the coordinate field For destination
  MEField<> *dstRcoord = dstR.RegisterField("coordinates", dst_coord->GetMEFamily(), MeshObj::ELEMENT, 
                        dst_coord->GetContext(), dst_coord->dim());
  dstR.Commit();
  dRcr.SendFields(1, &dst_coord, &dstRcoord);

  // Create and send the coordinate field for source
  MEField<> *srcRcoord = srcR.RegisterField("coordinates", src_coord->GetMEFamily(), MeshObj::ELEMENT, 
                        src_coord->GetContext(), src_coord->dim());
  srcR.Commit();
  // Send over source fields
  sRcr.SendFields(1, &src_coord, &srcRcoord);
  std::vector<MEField<>*> rfuniq;
  srcR.MatchFields(sfields_uniq.size(), &sfields_uniq[0], rfuniq);
  sRcr.SendFields(sfields_uniq.size(), &sfields_uniq[0], &rfuniq[0]);


  MPI_Barrier(MPI_COMM_WORLD);
  nseconds = MPI_Wtime();
  if (rank == 0) std:: cout << "TIMING: Zoltan:" << (nseconds-seconds) << std::endl;
  seconds = nseconds;

  // Now do the (local) search
  SearchResult sres;
  Search(srcR, dstR, sres);
  //PrintSearchResult(sres);

  MPI_Barrier(MPI_COMM_WORLD);
  nseconds = MPI_Wtime();
  if (rank == 0) std:: cout << "TIMING: Search:" << (nseconds-seconds) << std::endl;
  seconds = nseconds;
/*
  Field<NodalField> *d = Field<NodalField>::get_field(dstMesh, "dest");
  if (!d) throw("where is d??");
  Field<NodalField> *dt = new Field<NodalField>(dstR, "dest");
*/

  // Transfer field to dest R
  std::vector<TransferPair> tp(num_fields);
  std::vector<TransferPair> tp1(num_fields);
  UInt nbilin=0, npatch = 0;
  for (UInt i = 0; i < num_fields; i++) {
    if (zinterp[i] == ZOLT_BILIN) {
      tp[nbilin].fields.first = st[i];
      tp[nbilin].fields.second = dt[i];
      nbilin++;
    } else {
      tp1[npatch].fields.first = st[i];
      tp1[npatch].fields.second = dt[i];
      npatch++;
    }
  }
  if (nbilin > 0) {
    Transfer(&tp[0], &tp[0] + nbilin, sres);
  }
  if (npatch > 0) {
    RTransfer(2, &tp1[0], &tp1[0] + npatch, sres, *srcRcoord, true);
  }

  // Now send home
  dRcr.Transpose();
  dRcr.SendFields(num_fields, &dt[0], &dfields[0]);

/*
  // And, lastly, halo
  for (UInt i = 0; i < csize; i++) {
  MPI_Barrier(MPI_COMM_WORLD);
if (rank == i) {
std::cout << "Sym node, rank=" << i << std::endl;
  srcMesh.GetSymNodeRel().Print();
}
}
  if (!srcMesh.GetSymNodeRel().is_sym_sane()) {
    std::cout << "P:" << rank << ", sym node spec not sane" << std::endl;
  }
*/

  dstMesh.HaloFields(num_fields, dfields);

  MPI_Barrier(MPI_COMM_WORLD);
  nseconds = MPI_Wtime();
  if (rank == 0) std:: cout << "TIMING: Matrix:" << (nseconds-seconds) << std::endl;
  seconds = nseconds;




  // Write the rendezvous for fun
  srcR.set_filename("rend");
  
  WriteMesh(srcR, srcR.filename());

  WriteMesh(dstR, "drend");
  // Delete commspecs

  // Clean up

  Zoltan_LB_Free_Part(&importGlobalGids, &importLocalGids,
                      &importProcs, &importToPart);
  Zoltan_LB_Free_Part(&exportGlobalGids, &exportLocalGids,
                      &exportProcs, &exportToPart);


  delete mesh_isect;

  Zoltan_Destroy(&zz);
}

} // namespace
} // namespace
