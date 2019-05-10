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

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_SearchNearest.h>
#include <Mesh/include/ESMCI_Rendez_Nearest.h>

#include <Mesh/include/ESMCI_MBMesh_BBox.h>

#include <Mesh/include/Legacy/ESMCI_ParEnv.h>

#include "ESMCI_PointList.h"

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>
#include <map>

#include <ESMCI_VM.h>
#include <ESMCI_LogErr.h>

using std::vector;
using std::iterator;


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


using namespace ESMCI;

void calc_nearest_mat(PointList *srcpl, PointList *dstpl, 
                      SearchNearestResultList &sres, WMat &iw) {
  Trace __trace("calc_nearest_mat()");


  if (srcpl->get_coord_dim() != dstpl->get_coord_dim()) {
    Throw() << "src and dst mesh must have the same spatial dimension for nearest regridding";
  }


  // Temporary empty col with negatives so unset values
  // can be detected if they sneak through
  WMat::Entry col_empty(-1, 0, -1.0, 0);

  // Loop through search results
  SearchNearestResultList::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    Search_nearest_result &sr = **sb;

    // Set col info
    WMat::Entry col(sr.src_gid, 0, 1.0, 0);

    // Set row info (i.e. the destination id associated with the above weight)
    WMat::Entry row(sr.dst_gid, 0, 0.0, 0);


    // Put weights into weight matrix
    // Need merge version in nearest src to dest case where there may be more than 1 src,dst pair with the same dst.
    iw.InsertRowMergeSingle(row, col);

  } // for searchresult
}

void calc_nearest_regrid_wgts(PointList *srcpl, PointList *dstpl, 
                              WMat &wts, bool set_dst_status, 
                              WMat &dst_status) {
#undef ESMC_METHOD
#define ESMC_METHOD "calc_nearest_regrid_wgts()"

  // Get Parallel Information
  int localrc;
  int petCount = VM::getCurrent(&localrc)->getPetCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Set meshes to use for regrid weight calculations
  PointList *srcpl_regrid=srcpl;
  PointList *dstpl_regrid=dstpl;

#ifdef DEBUG_POINTLIST
  {printf("%d# calc_nearest_regrid_wgts: src_pl(%d) [", Par::Rank(), srcpl->get_curr_num_pts());
  for (int p = 0; p < srcpl->get_curr_num_pts(); ++p) {
    const int *id = srcpl->get_id_ptr(p);
    double coords[3];
    srcpl->get_coord(p, &coords[0]);
     printf("%d [%f,%f,%f], ", srcpl->get_id_ptr(p), coords[0], coords[1], coords[2]);
  }
  printf("]\n");}
  {printf("%d# calc_nearest_regrid_wgts: dst_pl(%d) [", Par::Rank(), dstpl->get_curr_num_pts());
  for (int p = 0; p < dstpl->get_curr_num_pts(); ++p) {
    const int *id = dstpl->get_id_ptr(p);
    double coords[3];
    dstpl->get_coord(p, &coords[0]);
     printf("%d [%f,%f,%f], ", dstpl->get_id_ptr(p), coords[0], coords[1], coords[2]);
  }
  printf("]\n");}
#endif
 
  // If parallel then generate rendezvous pointlists
  PointList *srcpl_rend=NULL;
  PointList *dstpl_rend=NULL;
  if (petCount > 1) {

    // Create rendez pointlists
    create_rendez_nearest(srcpl, dstpl, &srcpl_rend, &dstpl_rend);

    // Use rendezvous pointlists
    srcpl_regrid=srcpl_rend;
    dstpl_regrid=dstpl_rend;
  }

  // Do search
  SearchNearestResultList result;
  SearchNearestSrcToDst(*srcpl_regrid, *dstpl_regrid, 
                        ESMCI_UNMAPPEDACTION_IGNORE, result,
                        set_dst_status, dst_status);

  // Calculate the weight matrix
  calc_nearest_mat(srcpl_regrid, dstpl_regrid, result, wts);

  // If parallel then migrate weights back to decompostion of original mesh
  if (petCount > 1) {
    wts.Migrate(*dstpl);
  }

  // If parallel then get rid of rendezvous meshes.
  if (petCount > 1) {
    if (srcpl_rend != NULL) delete srcpl_rend;
    if (dstpl_rend != NULL) delete dstpl_rend;
  }
}

#endif // ESMF_MOAB
