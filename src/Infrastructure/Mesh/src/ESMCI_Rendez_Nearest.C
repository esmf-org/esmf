// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/src/Zoltan/zoltan.h>

#include <Mesh/include/ESMCI_MBMesh_Redist.h>
#include "Mesh/include/ESMCI_MBMesh_BBox.h"

#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>

#include <ESMCI_VM.h>
#include "ESMCI_LogErr.h"

#include "ESMCI_PointList.h"

#include <limits>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
          
using namespace ESMCI;

struct ZData {
  PointList *srcpl;
  PointList *dstpl;

  int sdim;
};

static int GetNumAssignedObj(void *user, int *err) {
  ZData &udata = *(static_cast<ZData *>(user));
  *err = 0;

  int num=0;
  num += udata.srcpl->get_curr_num_pts();
  num += udata.dstpl->get_curr_num_pts();

  return num;
}
 
static void GetObjList(void *user, int numGlobalIds, int numLids, ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids,
          int wgt_dim, float *obj_wghts, int *err) 
{
#undef  ESMC_METHOD
#define ESMC_METHOD "GetObjList()"
  Trace __trace("GetObjList()");

  ZData &udata = *(static_cast<ZData *>(user));
  UInt i = 0;

  // Put src into lists
  int pl_size=udata.srcpl->get_curr_num_pts();
  for (int loc=0; loc<pl_size; loc++) {
    gids[2*i] = 0;
    gids[2*i + 1] = loc;  //udata.src_pointlist->get_id(loc);
    lids[i] = i;
    i++;
  }

  // Put dst into lists
  pl_size=udata.dstpl->get_curr_num_pts();
  for (int loc=0; loc<pl_size; loc++) {
    gids[2*i] = 1;
    gids[2*i + 1] = loc;  //udata.src_pointlist->get_id(loc);
    lids[i] = i;
    i++;
  }

  // Return success
  *err = 0;
}

static int GetNumGeom(void *user, int *err) {
#undef  ESMC_METHOD
#define ESMC_METHOD "GetNumGeom()"
  Trace __trace("GetNumGeom()");

  ZData &udata = *(static_cast<ZData*>(user));
  *err = 0;

  return udata.sdim;
}


static void GetObject(void *user, int numGlobalIds, int numLids, int numObjs,
  ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids, int numDim, double *pts, int *err) 
{
#undef  ESMC_METHOD
#define ESMC_METHOD "GetObject()"
  Trace __trace("GetObject()");
  
  ZData &udata = *(static_cast<ZData*>(user));

  // Get size of source list
  UInt list1_size;
  list1_size = udata.srcpl->get_curr_num_pts();

  // Loop through 
  for (UInt i = 0; i < (UInt) numObjs; i++) {
    UInt src_or_dst = gids[2*i]; // 0 = src, 1 = dst
    UInt idx = lids[i];

    std::vector<double> ndata(numDim);
    const double *c;

    // Get centroid depending on source or destination
    if (src_or_dst == 0) {
      int loc = gids[2*i+1];
      c = udata.srcpl->get_coord_ptr(loc);
    } else if (src_or_dst == 1) {
      int loc = gids[2*i+1];
      c = udata.dstpl->get_coord_ptr(loc);
    } else throw("GetObject, unknown mesh from global id");

    // Copy coords into list
    for (UInt d = 0; d < (UInt) numDim; d++) pts[i*numDim + d] = c[d];
  }

  // Return success
  *err = 0;
}

static void assign_points_to_procs(PointList *pl, int numExport, ZOLTAN_ID_PTR exportGids,
                                   ZOLTAN_ID_PTR exportLids,
                                   int *exportProcs, int src_or_dst, int list1_size,
                                   std::vector<PL_Comm_Pair> *point_to_proc_list) {
#undef  ESMC_METHOD
#define ESMC_METHOD "assign_points_to_procs()"
  Trace __trace("assign_points_to_procs()");

  // Loop through objects assigning them to procs
  for (unsigned int i = 0; i < numExport; ++i) {
    if (exportGids[i*2]==src_or_dst) {
      PL_Comm_Pair pcp(exportLids[i]-list1_size, exportProcs[i]);

      std::vector<PL_Comm_Pair>::iterator lb =
        std::lower_bound(point_to_proc_list->begin(),
                         point_to_proc_list->end(), pcp);

      // Add if not already there
      if (lb == point_to_proc_list->end() || *lb != pcp)
        point_to_proc_list->insert(lb, pcp);

#ifdef ESMF_REGRID_DEBUG_MAP_ANY
      printf("%d# Node %d[%d] will be sent by proc [%d]\n", Par::Rank(), exportGids[i*2+1], exportLids[i]-list1_size, exportProcs[i]);
#endif
    }
  }
}

void calc_rendez_comm_pattern(PointList *srcpl, PointList *dstpl, 
                        std::vector<PL_Comm_Pair> *src_point_to_proc_list,
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
  zd.sdim=srcpl->get_coord_dim();

  // Set meshes in struct
  zd.srcpl=srcpl;
  zd.dstpl=dstpl;

  // Gather the destination points
  int sdim = dstpl->get_coord_dim();
  double cmin[sdim], cmax[sdim];
  build_pl_bbox(cmin, cmax, dstpl);

  // Get global bounding box of dst mesh
  MBMesh_BBox dst_bbox = MBMesh_BBoxParUnion(MBMesh_BBox(sdim, cmin, cmax));

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

  // Calc. where source points are to go
  assign_points_to_procs(srcpl, numExport, exportGlobalids, exportLocalids, 
                         exportProcs, 0, 0, src_point_to_proc_list);

  // Calc. where destintation points are to go
  assign_points_to_procs(dstpl, numExport, exportGlobalids, exportLocalids, 
                         exportProcs, 1, zd.srcpl->get_curr_num_pts(), 
                         dst_point_to_proc_list);

  // Free Zoltan struct
  Zoltan_Destroy(&zz);
}


// This creates src and dst rendezvous pointlists where the overlap is based on ??
void create_rendez_nearest(PointList *srcpl, PointList *dstpl, 
    PointList **_srcpl_rendez, PointList **_dstpl_rendez) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_rendez_nearest()"

  // Get Parallel Information
  int localrc;
  int petCount = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;

  // Compute communication pattern to build rendezvous pointlists
  std::vector<PL_Comm_Pair> src_point_to_proc_list;
  std::vector<PL_Comm_Pair> dst_point_to_proc_list;
  calc_rendez_comm_pattern(srcpl, dstpl, 
                           &src_point_to_proc_list, &dst_point_to_proc_list);

  // Create src rend pointlist
  create_pointlist_redist_point(srcpl, &src_point_to_proc_list, _srcpl_rendez);

  // Create dst rend pointlist
  create_pointlist_redist_point(dstpl, &dst_point_to_proc_list, _dstpl_rendez);
}

#endif // ESMF_MOAB

