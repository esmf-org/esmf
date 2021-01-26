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

#include <Mesh/include/ESMCI_RegridConstants.h>
#include <Mesh/include/ESMCI_Rendez_Nearest.h>
#include <Mesh/include/ESMCI_Search_Nearest.h>

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
                      SearchNearestResultList &sres, WMat &iw,
                      int regridMethod = 3) {
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

    // swap ids for dst_to_src
    int sid = sr.src_gid;
    int did = sr.dst_gid;
    if (regridMethod == ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC) {
      sid = sr.dst_gid;
      did = sr.src_gid;
    }

    // Set col info
    WMat::Entry col(sid, 0, 1.0, 0);

    // Set row info (i.e. the destination id associated with the above weight)
    WMat::Entry row(did, 0, 0.0, 0);


    // Put weights into weight matrix
    // Need merge version in nearest src to dest case where there may be more than 1 src,dst pair with the same dst.
    iw.InsertRowMergeSingle(row, col);

  } // for searchresult
}

void calc_nearest_npts_mat(PointList *srcpointlist, PointList *dstpointlist, 
                            ESMC_R8 dist_exponent, 
                            SearchNearestResultList &sres, WMat &iw) {
  Trace __trace("calc_nearest_npts_mat()");

  // Make sure dimensions match
  if (srcpointlist->get_coord_dim() != dstpointlist->get_coord_dim()) {
    Throw() << "src and dst must have the same spatial dimension for nearest regridding";
  }

  // Get dimension
  int sdim=srcpointlist->get_coord_dim();

  // Convert to double
  // (Just in case ESMC_R8 is different)
  double dist_exponent_dbl = (double)dist_exponent;

  // Temporary empty col with negatives so unset values
  // can be detected if they sneak through
  WMat::Entry col_empty(-1, 0, -1.0, 0);

  // Put this outside loop, so it doesn't keep allocating memory every time
  std::vector<WMat::Entry> cols;

  // Loop through search results
  SearchNearestResultList::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    Search_nearest_result &sr = **sb;

    // Get dst location in dstpointlist
    int dst_loc=sr.dst_gid;

    // Get destination id
    int dst_id=dstpointlist->get_id(dst_loc);

    // Get dst point coords
    const double *coord=dstpointlist->get_coord_ptr(dst_loc);
    double dst_pnt[3];
    dst_pnt[0] = coord[0];
    dst_pnt[1] = coord[1];
    dst_pnt[2] = (sdim == 3 ? coord[2] : 0.0);

    // Set row info (i.e. the destination id)
    WMat::Entry row(dst_id, 0, 0.0, 0);

    // Clear and then Reserve to potential size
    cols.clear();
    cols.reserve(sr.nodes.size());

    // See if there are any 0.0 dist
    bool no_zero_dist=true;
    for (int i=0; i<sr.nodes.size(); i++) {

      // Get coordinates of src point
      double coord[3];
      coord[0] = sr.nodes[i].pcoord[0];
      coord[1] = sr.nodes[i].pcoord[1];
      coord[2] = (sdim == 3 ? sr.nodes[i].pcoord[2] : 0.0);

      // Calculate distance
      double dist=sqrt((dst_pnt[0]-coord[0])*(dst_pnt[0]-coord[0])+
                       (dst_pnt[1]-coord[1])*(dst_pnt[1]-coord[1])+
                       (dst_pnt[2]-coord[2])*(dst_pnt[2]-coord[2]));

      // There is a 0.0 dist, so record that fact and leave
      if (dist == 0.0) {
        no_zero_dist=false;
        break;
      }
    }

    // Loop calculating weights
    double tot=0.0;
    if (no_zero_dist) {
      for (int i=0; i<sr.nodes.size(); i++) {

        // Get coordinates of src point
        double coord[3];
        coord[0] = sr.nodes[i].pcoord[0];
        coord[1] = sr.nodes[i].pcoord[1];
        coord[2] = (sdim == 3 ? sr.nodes[i].pcoord[2] : 0.0);

        // Calculate distance
        double dist=sqrt((dst_pnt[0]-coord[0])*(dst_pnt[0]-coord[0])+
                         (dst_pnt[1]-coord[1])*(dst_pnt[1]-coord[1])+
                         (dst_pnt[2]-coord[2])*(dst_pnt[2]-coord[2]));

        // This shouldn't happen, so complain
        if (dist == 0.0) {
          Throw() << " zero distance in part of weight calc that's for nonzero.";
        }

        // 1 over dist raised to a power
        double inv_dist=1.0/pow(dist,dist_exponent_dbl);

        // Sum total weights
        tot += inv_dist;

        // Set col entry info
        // NOTE: dst_gid actually contains src_gid
        WMat::Entry col_entry(sr.nodes[i].dst_gid, 0, inv_dist, 0);

        // Push into
        cols.push_back(col_entry);
      }
    } else {
      // There are 0.0 dist, so just count those
      for (int i=0; i<sr.nodes.size(); i++) {

        // Get coordinates of src point
        double coord[3];
        coord[0] = sr.nodes[i].pcoord[0];
        coord[1] = sr.nodes[i].pcoord[1];
        coord[2] = (sdim == 3 ? sr.nodes[i].pcoord[2] : 0.0);

        // Calculate distance
        double dist=sqrt((dst_pnt[0]-coord[0])*(dst_pnt[0]-coord[0])+
                         (dst_pnt[1]-coord[1])*(dst_pnt[1]-coord[1])+
                         (dst_pnt[2]-coord[2])*(dst_pnt[2]-coord[2]));

        // This is 0.0, so just add that
        if (dist == 0.0) {

          // Set col entry info using 1.0 as weight
          // NOTE: dst_gid actually contains src_gid
          WMat::Entry col_entry(sr.nodes[i].dst_gid, 0, 1.0, 0);

          // Sum total weights
          tot += 1.0;

          // Push into
          cols.push_back(col_entry);
        }
      }
    }

    // Loop dividing by tot
    for (int i=0; i<cols.size(); i++) {
      cols[i].value=cols[i].value/tot;
    }

#if 0
    // DEBUG
    //   if ((dst_id==7050) || (dst_id==6878) || (dst_id==6880)) {
    if ((dst_id==3737)) {
      printf("wgt calc: dst_id=%d ::",dst_id);
      for (int i=0; i<cols.size(); i++) {
        printf(" %d %g, \n",cols[i].id,cols[i].value);
      }
      printf("\n");
    }
#endif

    // Put weights into weight matrix
    iw.InsertRow(row, cols);

  } // for searchresult
}


void calc_nearest_regrid_wgts(PointList *srcpl, PointList *dstpl, 
                              WMat &wts, bool set_dst_status, 
                              WMat &dst_status, int *regridMethod, 
                              int *extrapNumSrcPnts, 
                              ESMC_R8 *extrapDistExponent) {
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
  if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST) {
    if (petCount == 1) {
      SearchNearestSrcToDst(*srcpl_regrid, *dstpl_regrid, 
                            ESMCI_UNMAPPEDACTION_IGNORE, result,
                            set_dst_status, dst_status);
    } else { 
      ParSearchNearestSrcToDst(*srcpl_regrid, *dstpl_regrid, 
                               ESMCI_UNMAPPEDACTION_IGNORE, result,
                               set_dst_status, dst_status);
    }
  } else if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_IDAVG) {
    if (petCount == 1) {
      SearchNearestSrcToDstNPnts(*srcpl_regrid, *dstpl_regrid, 
                                 *extrapNumSrcPnts,
                                 ESMCI_UNMAPPEDACTION_IGNORE, result,
                                 set_dst_status, dst_status);
    } else { 
      ParSearchNearestSrcToDstNPnts(*srcpl_regrid, *dstpl_regrid, 
                                    *extrapNumSrcPnts,
                                    ESMCI_UNMAPPEDACTION_IGNORE, result,
                                    set_dst_status, dst_status);
    }
  }



  // Calculate the weight matrix
  if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST) {
    calc_nearest_mat(srcpl_regrid, dstpl_regrid, result, wts, *regridMethod);
  } else if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_IDAVG) {
    calc_nearest_npts_mat(srcpl_regrid, dstpl_regrid, *extrapDistExponent, 
                          result, wts);
  } else if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC) {
    calc_nearest_mat(srcpl_regrid, dstpl_regrid, result, wts, *regridMethod);
  } else Throw() << "This regrid method is not currently supported.";


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
