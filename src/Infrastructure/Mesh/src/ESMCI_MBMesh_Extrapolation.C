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
#define ESMC_FILENAME "ESMCI_MBMesh_Extrapolation.C"
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMC_Util.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_Extrapolation.h"
#include "Mesh/include/ESMCI_RegridConstants.h"
#include "Mesh/include/ESMCI_MBMesh_Regrid_Glue.h"
#include "Mesh/include/ESMCI_MBMesh_Util.h"

using namespace ESMCI;

static void create_pointlist_of_points_not_in_wmat(PointList *pointlist, WMat &wts, PointList **_missing_points) {

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Count number of missing points
  int num_missing=0;
  int curr_num_pts = pointlist->get_curr_num_pts();
  for (int i=0; i<curr_num_pts; i++) {

    // get node id
    int id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }

    // If the current weight is not equal to the node id, then we must have passed it, so add it to the list
    if (wi->first.id != id) {
      num_missing++;
    }
  }

  // Create Pointlist
  PointList *missing_points = new ESMCI::PointList(num_missing, pointlist->get_coord_dim());

  // Add missing points to PointList
  wi =wts.begin_row();
  for (int i=0; i<curr_num_pts; i++) {

    // get node id
    int id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }

    // If the current weight is not equal to the node id, then we must have passed it, so add it to the list
    if (wi->first.id != id) {
      missing_points->add(id, pointlist->get_coord_ptr(i));
    }
  }

  // Output
  *_missing_points=missing_points;
}

void replace_mapped_with_mapped_extrap(WMat &status) {

  WMat::WeightMap::iterator wi = status.begin_row(), we = status.end_row();
  for (; wi != we; ++wi) {

    // Get row and column info
    const WMat::Entry &row = wi->first;
    std::vector<WMat::Entry> &col = wi->second;

    // Loop column
    for (int i = 0; i < col.size(); ++i) {
      WMat::Entry &entry = col[i];

      if (entry.id == ESMC_REGRID_STATUS_MAPPED) entry.id=ESMC_REGRID_STATUS_EXTRAP_MAPPED;
    }
  }
}


void MBMesh_Extrapolate(MBMesh *srcmesh, PointList *srcpointlist, 
                        MBMesh *dstmesh, PointList *dstpointlist,
                        WMat &wts,
                        // int *mtype, // RLO not sure if this is needed
                        UInt *pole_constraint_id, // Only valid when srcmesh exists
                        int *extrapMethod,
                        int *extrapNumSrcPnts,
                        ESMC_R8 *extrapDistExponent,
                        int *extrapNumLevels,
                        int *extrapNumInputLevels, 
                        bool set_dst_status, WMat &dst_status,
                        int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_Extrapolate()"

  int localrc;

  // Error out for extrap methods not handled right now
  if (*extrapMethod == ESMC_EXTRAPMETHOD_CREEP) {
    Throw() << "Creep fill extrapolation is currently not available when using MOAB internal mesh representation.";
  } else if (*extrapMethod == ESMC_EXTRAPMETHOD_CREEP_NRST_D) {
    Throw() << "Creep fill nearest destination extrapolation is currently not available when using MOAB internal mesh representation.";
  } else if (*extrapMethod == ESMC_EXTRAPMETHOD_NEAREST_D) {
    Throw() << "Nearest mapped destination extrapolation is currently not available when using MOAB internal mesh representation.";
  }

  // printf("MBMesh_Extrapolate: extrapMethod=%d\n", *extrapMethod);

  // if (extrapMethod == ESMC_EXTRAPMETHOD_CREEP) {
  // 
  //   // Set info for calling into interp
  //   IWeights extrap_wts;
  //   WMat extrap_dst_status;
  //   std::vector<int> valid_gids;
  // 
  //   // Get list of valid gids
  //   _get_dst_gids_in_wmat(wts, valid_gids);
  // 
  //   // Get creep fill weights
  //   CreepFill(*dstmesh, valid_gids, extrapNumLevels, extrapNumInputLevels,
  //             extrap_wts, set_dst_status, extrap_dst_status);
  // 
  //   // Merge weights into regrid weights
  //   // NOTE: this is for creeping in the destination, if we 
  //   // do it in the source, then we will have to do something different here
  //    dst_merge_creep_wts_into_regrid_wts(*dstmesh, extrap_wts, wts);
  // 
  //   return;
  // }

  // Construct pointlist for srcmesh
  // We need a pointlist to be able to use nearest neighbor
  // TODO: Change nearest neighor weight calc method, so it can
  //       optionally run on a mesh to avoid this step and make things more efficient
  PointList *srcpointlist_extrap=NULL;
  if (srcmesh != NULL) {
    // ESMCI::InterArray<int> *mask_values = ?
    // TODO: after pole extrapolation is added will need to pass in the
    //       pole_constraint_id to disallow artifical pole points in the pl
    srcpointlist_extrap = MBMesh_to_PointList(srcmesh, ESMC_MESHLOC_NODE,
                                              NULL, &localrc);
  } else if (srcpointlist != NULL) {
     srcpointlist_extrap=srcpointlist;
  } else {
     Throw() << "No source geometry object.";
  }
   
  // DEBUG
  //srcpointlist_extrap->WriteVTK("src_pl");
  
  // Construct a new point list which just contains destination points not in the weight matrix
  PointList *missing_points=NULL;
  if (dstmesh != NULL) {
     Throw() << "Conservative methods not supported in extrapolation, because extrapolation would cause the methods to no longer be conservative.";
  } else if (dstpointlist != NULL) {
     create_pointlist_of_points_not_in_wmat(dstpointlist, wts, &missing_points);
  } else {
     Throw() << "No destination geometry object.";
  }
  
  // // DEBUG
  // //missing_points->WriteVTK("missing_pl");
  
  // Translate extrap method to regrid method
  int regridMethod;
  if (*extrapMethod == ESMC_EXTRAPMETHOD_NEAREST_STOD) {
     regridMethod=ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST;
  } else if (*extrapMethod == ESMC_EXTRAPMETHOD_NEAREST_IDAVG) {
     regridMethod=ESMC_REGRID_METHOD_NEAREST_IDAVG;
  } else {
     Throw() << "unrecognized extrapolation method";
  }
   
  // Set info for calling into interp
  WMat extrap_wts;
  WMat extrap_dst_status;

  calc_nearest_regrid_wgts(srcpointlist_extrap, missing_points, extrap_wts,
                           set_dst_status, extrap_dst_status, 
                           &regridMethod, extrapNumSrcPnts,
                           extrapDistExponent);
  // Merge extrap weights into regridding matrix
  wts.MergeDisjoint(extrap_wts);
  
  // If status was requested merge that too
  if (set_dst_status) {
     // Change from ...MAPPED to ...MAPPED_EXTRAP
     replace_mapped_with_mapped_extrap(extrap_dst_status);
  
     // Replace old status with extrap ones
     dst_status.MergeReplace(extrap_dst_status);
  }
  
  // Cleanup
  if (missing_points != NULL) delete missing_points;
  // TODO: RLO: this causes a hang.. is it now a memory leak?
  // if (srcpointlist_extrap != NULL) delete srcpointlist_extrap;

}

#undef  ESMC_METHOD

#endif // ESMF_MOAB
