// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/src/Zoltan/zoltan.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Redist.h>

#include <limits>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>

#include <ESMCI_VM.h>
#include "ESMCI_LogErr.h"

#include "MBTagConventions.hpp"
#include "ESMCI_BBox.h"
#include "ESMCI_PointList.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
          
using namespace ESMCI;

struct ZData {
  // Could maybe store this more efficiently using a range. However,
  // since we are comparing with Mesh and that does it this way, and could
  // also do it more efficiently by using iterators, do it this way for now. 
  std::vector<EntityHandle> src_elems;
  std::vector<point*> dst_points;

  MBMesh *srcmesh;
  PointList *dstpl;

  int sdim;
};

static int GetNumAssignedObj(void *user, int *err) {
  ZData &udata = *(static_cast<ZData *>(user));
  *err = 0;

  int num=0;
  num += udata.dst_points.size();
  num += udata.src_elems.size();

  return num;
}
 
static void GetObjList(void *user, int numGlobalIds, int numLids, ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids,
          int wgt_dim, float *obj_wghts, int *err) 
{
  ZData &udata = *(static_cast<ZData *>(user));
  std::vector<EntityHandle>::iterator ni,ne;
  UInt i = 0;

  // Put src into lists
  ni = udata.src_elems.begin();
  ne = udata.src_elems.end();    
  for (; ni != ne; ++ni) {
    int elem_gid;
    MBMesh_get_gid(udata.srcmesh, *ni, &elem_gid);
    gids[2*i] = 0;  // 0 -> src elems
    gids[2*i + 1] = elem_gid;
    lids[i] = i;
    i++;
  }

  // Put dst into lists
  std::vector<point*>::iterator mi, me;
  mi = udata.dst_points.begin();
  me = udata.dst_points.end();
  for (; mi != me; ++mi) {
    point *p = &(**mi); // RLO: so weird..
    int node_gid = p->id;
    gids[2*i] = 1;  // 1 -> src elems
    gids[2*i + 1] = node_gid;
    lids[i] = i;
    i++;
  }

  // Return success
  *err = 0;
}

static int GetNumGeom(void *user, int *err) {
  ZData &udata = *(static_cast<ZData*>(user));
  *err = 0;

  return udata.sdim;
}


static void GetObject(void *user, int numGlobalIds, int numLids, int numObjs,
  ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids, int numDim, double *pts, int *err) 
{
  ZData &udata = *(static_cast<ZData*>(user));

  // Get size of source list
  UInt list1_size;
  list1_size = udata.src_elems.size();

  // Loop through 
  for (UInt i = 0; i < (UInt) numObjs; i++) {
    UInt src_or_dst = gids[2*i]; // 0 = src, 1 = dst
    UInt idx = lids[i];

    std::vector<double> ndata(numDim);
    const double *c;

    // Get centroid depending on source or destination
    if (src_or_dst == 0) {
      EntityHandle src_eh = udata.src_elems[idx];
      MBMesh_get_elem_centroid(udata.srcmesh, src_eh, &ndata[0]);
      c = &ndata[0];
    } else if (src_or_dst == 1) {
      UInt sidx = idx - list1_size; // local indices start from first list, continue into second
      point *dst_point= udata.dst_points[sidx];
      c = dst_point->coords;
    } else throw("GetObject, unknown mesh from global id");

    // Copy coords into list
    for (UInt d = 0; d < (UInt) numDim; d++) pts[i*numDim + d] = c[d];
  }

  // Return success
  *err = 0;
}

static void assign_elems_to_procs(MBMesh *mesh, std::vector<EntityHandle> *elems, double geom_tol, UInt sdim, Zoltan_Struct *zz, std::vector<EH_Comm_Pair> *elem_to_proc_list) {
  Trace __trace("assign_elems_to_procs()");
#undef  ESMC_METHOD
#define ESMC_METHOD "assign_elems_to_procs()"

  // Get number of Pets
  int localrc;
  int petCount = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Allocate list of procs where each elem should be assigned
  int numprocs;
  std::vector<int> procs(petCount);

  // Loop through objects assigning them to procs
  std::vector<EntityHandle>::iterator si = elems->begin(), se = elems->end();
  for (; si != se; ++si) {
    EntityHandle &elem = *si;

    // Get bounding box for elem
    MBMesh_BBox ebox(mesh, elem, geom_tol);

    // Assingn elems to procs using zoltan struct
    Zoltan_LB_Box_Assign(zz, ebox.getMin()[0]-geom_tol,
                             ebox.getMin()[1]-geom_tol,
                             (sdim > 2 ? ebox.getMin()[2] : 0) - geom_tol,
                             ebox.getMax()[0]+geom_tol,
                             ebox.getMax()[1]+geom_tol,
                             (sdim > 2 ? ebox.getMax()[2] : 0) + geom_tol,
                             &procs[0],
                             &numprocs);

    // Add to pattern
    for (int i = 0; i < numprocs; i++) {
      EH_Comm_Pair ecp(elem, procs[i]);

      std::vector<EH_Comm_Pair>::iterator lb =
        std::lower_bound(elem_to_proc_list->begin(), elem_to_proc_list->end(), ecp);

      // Add if not already there
      if (lb == elem_to_proc_list->end() || *lb != ecp)
        elem_to_proc_list->insert(lb, ecp);
    } // for nproc
  } // for si
}

static void assign_points_to_procs(PointList *pl,
                                   std::vector<point*> *points,
                                   double geom_tol, UInt sdim,
                                   Zoltan_Struct *zz,
                                   std::vector<PL_Comm_Pair> *point_to_proc_list) {
  Trace __trace("assign_points_to_procs()");
#undef  ESMC_METHOD
#define ESMC_METHOD "assign_points_to_procs()"

  // Get number of Pets
  int localrc;
  int petCount = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Allocate list of procs where each elem should be assigned
  int numprocs;
  std::vector<int> procs(petCount);

  // Loop through objects assigning them to procs
  for (unsigned int i = 0; i < points->size(); ++i) {

    point *p = points->at(i);

    // Assign elems to procs using zoltan struct
    Zoltan_LB_Box_Assign(zz, p->coords[0]-geom_tol,
                         p->coords[1]-geom_tol,
                         (sdim > 2 ? p->coords[2] : 0) - geom_tol,
                         p->coords[0]+geom_tol,
                         p->coords[1]+geom_tol,
                         (sdim > 2 ? p->coords[2] : 0) + geom_tol,
                         &procs[0],
                         &numprocs);

    // Add to pattern
    for (int j = 0; j < numprocs; j++) {
      PL_Comm_Pair pcp(i, procs[j]);

      std::vector<PL_Comm_Pair>::iterator lb =
        std::lower_bound(point_to_proc_list->begin(),
                         point_to_proc_list->end(), pcp);

      // Add if not already there
      if (lb == point_to_proc_list->end() || *lb != pcp)
        point_to_proc_list->insert(lb, pcp);
    } // for nproc
  } // for si
}

// TODO: this should be a member function of a PointList, or PointList should
//       just use a vector in the first place
// Get a vector of points in a pointlist
void get_vector_of_points(PointList *pl, std::vector<point*> *points) {
#undef  ESMC_METHOD
#define ESMC_METHOD "get_vector_of_points()"

  for (int i=0; i<pl->get_curr_num_pts(); ++i) points->push_back(pl->get_point(i));

}

// Get a vector of elems in a mesh that intersect with a bounding box
void get_vector_of_elems_that_overlap_box(MBMesh *mesh, BBox &bbox, std::vector<EntityHandle> *elems, double geom_tol) {
#undef  ESMC_METHOD
#define ESMC_METHOD "get_vector_of_elems_that_overlap_box()"
  int merr, localrc;

  // Get MOAB meshe
  Interface *mesh_moab=mesh->mesh;

  // Get a range containing all elements
  Range all_elem;
  merr=mesh_moab->get_entities_by_dimension(0,mesh->pdim,all_elem);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
   }     

  // Put elems that overlap into box
  for(Range::iterator it=all_elem.begin(); it !=all_elem.end(); it++) {
    const EntityHandle eh=*it;
    
    // Get bbox of element
    MBMesh_BBox elem_bbox(mesh, eh, 0.25);

    if (Mixed_BBoxIntersect(elem_bbox, bbox, geom_tol)) {
      elems->push_back(eh);
    }
  }
}


void calc_rendez_comm_pattern(MBMesh *srcmesh, PointList *dstpl,
                              std::vector<EH_Comm_Pair> *src_elem_to_proc_list,
                              std::vector<PL_Comm_Pair> *dst_point_to_proc_list) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_rendez_comm_pattern()"
  int rc, localrc;

  // Get mpi_comm for this VM
  MPI_Comm mpi_comm = VM::getCurrent(&localrc)->getMpi_c();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception


  // Init Zoltan
  float ver;
  rc = Zoltan_Initialize(0, NULL, &ver);

  // Create Zoltan struct
  struct Zoltan_Struct * zz = Zoltan_Create(mpi_comm);

  // Set Zoltan parameters
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

  // Set user data for zoltan call backs
  ZData zd;

  // Set sptial dimension
  zd.sdim=srcmesh->sdim;

  // Set meshes in struct
  zd.srcmesh=srcmesh;
  zd.dstpl=dstpl;

  // Set list of destination points
  get_vector_of_points(dstpl, &(zd.dst_points));

  // Gather the destination points
  int sdim = dstpl->get_coord_dim();
  double cmin[sdim], cmax[sdim];
  build_pl_bbox(cmin, cmax, dstpl);

  // Get global bounding box of dst mesh
  BBox dst_bbox = BBoxParUnion(BBox(sdim, cmin, cmax));

  //// Set list of source elements (only those that overlap BBox of dstpl)
  get_vector_of_elems_that_overlap_box(srcmesh, dst_bbox, &(zd.src_elems), 1e-6);

#if 0
  printf("%d# src_elems.size=%d\n",Par::Rank(),zd.src_elems.size());
  printf("%d# dst_elems.size=%d\n",Par::Rank(),zd.dst_elems.size());
  printf("%d# global_dst_bbox min=[%f,%f] max=[%f,%f]\n",Par::Rank(),
         global_dst_bbox.getMin()[0],global_dst_bbox.getMin()[1],
         global_dst_bbox.getMax()[0],global_dst_bbox.getMax()[1]);
#endif

  // Set the mesh description callbacks
  Zoltan_Set_Num_Obj_Fn(zz, GetNumAssignedObj, (void*) &zd);
  Zoltan_Set_Obj_List_Fn(zz, GetObjList, (void*) &zd);
  Zoltan_Set_Num_Geom_Fn(zz, GetNumGeom, (void*) &zd);
  Zoltan_Set_Geom_Multi_Fn(zz, GetObject, (void*) &zd);

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

  // Call zoltan
  rc = Zoltan_LB_Partition(zz, &changes, &numGidEntries, &numLidEntries,
    &numImport, &importGlobalids, &importLocalids, &importProcs, &importToPart,
    &numExport, &exportGlobalids, &exportLocalids, &exportProcs, &exportToPart);

  // Calc. where source elems are to go
  assign_elems_to_procs(srcmesh, &(zd.src_elems), 1e-6, srcmesh->sdim,
                                   zz, src_elem_to_proc_list);

  // Calc. where destintation points are to go
  assign_points_to_procs(dstpl, &(zd.dst_points), 1e-6, dstpl->get_coord_dim(),
                         zz, dst_point_to_proc_list);

  // Free Zoltan struct
  Zoltan_Destroy(&zz);
}


// This creates src and dst rendezvous meshes where the overlap is based on elements and points
void create_rendez_mbmesh_etop(MBMesh *srcmesh, PointList *dstpl, MBMesh **_srcmesh_rendez, PointList **_dstpl_rendez) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_rendez_mbmesh_etop()"

  // Get Parallel Information
  int localrc;
  int petCount = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Compute communication pattern to build rendezvous meshes
  std::vector<EH_Comm_Pair> src_elem_to_proc_list;
  std::vector<PL_Comm_Pair> dst_point_to_proc_list;
  calc_rendez_comm_pattern(srcmesh, dstpl, &src_elem_to_proc_list, &dst_point_to_proc_list);

/*
  // Debug print of src list
  for (int i=0; i<src_elem_to_proc_list.size(); i++) {
    EntityHandle eh=src_elem_to_proc_list[i].eh;
    int proc=src_elem_to_proc_list[i].proc;

    int gid=-55;
    MBMesh_get_gid(srcmesh, eh, &gid);

    printf("%d# src gid=%d to proc=%d \n",Par::Rank(),gid,proc);
  }

  printf("%d# ======== \n", Par::Rank());

  // Debug print of dst list
  for (int i=0; i<dst_elem_to_proc_list.size(); i++) {
    EntityHandle eh=dst_elem_to_proc_list[i].eh;
    int proc=dst_elem_to_proc_list[i].proc;

    int gid=-77;
    MBMesh_get_gid(dstmesh, eh, &gid);

    printf("%d# dst gid=%d to proc=%d \n",Par::Rank(),gid,proc);
  }
*/

  // Create src rend mesh
  create_mbmesh_redist_elem(srcmesh, &src_elem_to_proc_list, _srcmesh_rendez);

  // Create dst rend mesh
  create_pointlist_redist_point(dstpl, &dst_point_to_proc_list, _dstpl_rendez);
}

#endif // ESMF_MOAB

