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

#define ESMC_FILENAME "ESMCI_MBMesh_Regrid_Glue.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"
#include "ESMCI_GridToMesh.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"
#include "ESMCI_TraceRegion.h"

#include "Mesh/include/Legacy/ESMCI_Exception.h"
// #include "Mesh/include/Regridding/ESMCI_Interp.h"
// #include "Mesh/include/Regridding/ESMCI_ExtrapolationPoleLGC.h"

#include "Mesh/include/ESMCI_MathUtil.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_Regrid_Glue.h"
#include "Mesh/include/ESMCI_RegridConstants.h"
#include "Mesh/include/ESMCI_MBMesh_Glue.h"
#include "Mesh/include/ESMCI_MBMesh_Bilinear.h"
#include "Mesh/include/ESMCI_MBMesh_Conserve.h"
#include "Mesh/include/ESMCI_MBMesh_Extrapolation.h"

#include <iostream>
#include <vector>
#include <map>

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
//
//EOP
//-------------------------------------------------------------------------

using namespace std;
using namespace ESMCI;

int calc_regrid_wgts(MBMesh *srcmbmp, MBMesh *dstmbmp, 
                     PointList *srcpl, PointList *dstpl, IWeights &wts,
                     int *regridConserve, int *regridMethod,
                     int *regridPoleType, int *regridPoleNPnts,
                     int *regridScheme,
                     int *map_type, 
                     int *extrapMethod,
                     int *extrapNumSrcPnts,
                     ESMC_R8 *extrapDistExponent,
                     int *extrapNumLevels,
                     int *extrapNumInputLevels, 
                     int *unmappedaction,
                     bool set_dst_status, WMat &dst_status);
// prototypes from below
static bool all_mbmesh_node_ids_in_wmat(PointList *pointlist, WMat &wts, int *missing_id);
static bool all_mbmesh_elem_ids_in_wmat(MBMesh *mesh, WMat &wts, int *missing_id);
static void get_mbmesh_node_ids_not_in_wmat(PointList *pointlist, WMat &wts, std::vector<int> *missing_ids);
static void get_mbmesh_elem_ids_not_in_wmat(MBMesh *mesh, WMat &wts, std::vector<int> *missing_ids);
static void mbcopy_rs_from_WMat_to_Array(WMat *wmat, ESMCI::Array *array);
static void mbcopy_cnsv_rs_from_WMat_to_Array(WMat *wmat, ESMCI::Array *array);

// external C functions
extern "C" void FTN_X(c_esmc_arraysmmstoreind4)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle,
    ESMC_TypeKind_Flag *typekind, void *factorList, int *factorListCount,
    ESMCI::InterArray<int> *factorIndexList, ESMC_Logical *ignoreUnmatched,
    int *srcTermProcessing, int *pipelineDepth, int *rc);


void MBMesh_regrid_create(void **meshsrcpp, ESMCI::Array **arraysrcpp, 
                          ESMCI::PointList **plsrcpp,
                          void **meshdstpp, ESMCI::Array **arraydstpp, 
                          ESMCI::PointList **pldstpp,
                          int *regridMethod,
                          int *map_type,
                          int *norm_type,
                          int *regridPoleType, int *regridPoleNPnts,
                          int *regridScheme,
                          int *extrapMethod,
                          int *extrapNumSrcPnts,
                          ESMC_R8 *extrapDistExponent,
                          int *extrapNumLevels,
                          int *extrapNumInputLevels, 
                          int *unmappedaction, int *_ignoreDegenerate,
                          int *srcTermProcessing, int *pipelineDepth,
                          ESMCI::RouteHandle **rh, int *has_rh, int *has_iw,
                          int *nentries, ESMCI::TempWeights **tweights,
                          int *has_udl, int *_num_udl, ESMCI::TempUDL **_tudl,
                          int *_has_statusArray, ESMCI::Array **_statusArray,
                          int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_regrid_create()"
  Trace __trace("MBMesh_regrid_create()");

  // Get Moab Mesh wrapper
  MBMesh *mbmsrcp=*((MBMesh **)meshsrcpp);
  MBMesh *mbmdstp=*((MBMesh **)meshdstpp);

// #define MOAB_REGRID
#ifdef MOAB_REGRID
  printf("~~~~~~~~~~~~~~~~MOAB REGRIDDING~~~~~~~~~~~~~~~~~~~~\n");
#endif

  // Access Arrays
  ESMCI::Array &srcarray = **arraysrcpp;
  ESMCI::Array &dstarray = **arraydstpp;

  // Access PointLists
  ESMCI::PointList *srcpl = *plsrcpp;
  ESMCI::PointList *dstpl = *pldstpp;

  int has_statusArray=*_has_statusArray;
  ESMCI::Array *statusArray=*_statusArray;

  // Old Regrid conserve turned off for now
  int regridConserve=ESMC_REGRID_CONSERVE_OFF;

#define PROGRESSLOG_off
#define MEMLOG_off

#ifdef PROGRESSLOG_on
  ESMC_LogDefault.Write("c_esmc_regrid_create(): Just entered routine.", ESMC_LOGMSG_INFO);
#endif

#ifdef MEMLOG_on
  VM::logMemInfo(std::string("RegridCreate1.0"));
#endif

  try {

    // transalate ignoreDegenerate to C++ bool
    bool ignoreDegenerate=false;
    if (*_ignoreDegenerate) ignoreDegenerate=true;
    else ignoreDegenerate=false;


    //// Precheck Meshes for errors
    bool concave=false;
    bool clockwise=false;
    bool degenerate=false;

#if 0
    // Check source mesh elements
    if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
      // Check cells for conservative
//BOB      cnsrv_check_for_mesh_errors(srcmesh, ignoreDegenerate, &concave, &clockwise, &degenerate);
    } else {
#if 0
      // STILL NEED TO FINISH THIS
      // Check cell nodes for non-conservative
      noncnsrv_check_for_mesh_errors(srcmesh, ignoreDegenerate, &concave, &clockwise, &degenerate);
#endif
    }


    // Concave
    if (concave) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "- Src contains a concave cell", ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // Clockwise
    if (clockwise) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "- Src contains a cell whose corners are clockwise", ESMC_CONTEXT,
        &localrc)) throw localrc;
    }

    // Degenerate
    if (degenerate) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "- Src contains a cell that has corners close enough that the cell "
        "collapses to a line or point", ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // Only check dst mesh elements for conservative because for others just nodes are used and it doesn't
    // matter what the cell looks like
    if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
      // Check mesh elements
//BOB      cnsrv_check_for_mesh_errors(dstmesh, ignoreDegenerate, &concave, &clockwise, &degenerate);

       // Concave
      if (concave) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "- Dst contains a concave cell", ESMC_CONTEXT, &localrc)) throw localrc;
      }

      // Clockwise
      if (clockwise) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "- Dst contains a cell whose corners are clockwise", ESMC_CONTEXT,
          &localrc)) throw localrc;
      }

      // Degenerate
      if (degenerate) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "- Dst contains a cell which has corners close enough that the cell "
          "collapses to a line or point", ESMC_CONTEXT, &localrc)) throw localrc;
      }
    }
#endif

#ifdef PROGRESSLOG_on
    ESMC_LogDefault.Write("c_esmc_regrid_create(): Entering weight generation.", ESMC_LOGMSG_INFO);
#endif

#ifdef MEMLOG_on
  VM::logMemInfo(std::string("RegridCreate2.0"));
#endif

    // Compute Weights matrix
    IWeights wts;
    // Turn off unmapped action checking in regrid because it's local to a proc, and can therefore
    // return false positives for multiproc cases, instead check below after gathering weights to a proc.
    int temp_unmappedaction=ESMCI_UNMAPPEDACTION_IGNORE;

    // Setup Destination status
    bool set_dst_status=false;
    if (has_statusArray) {
      set_dst_status=true;
    }
    WMat dst_status;

#ifdef ESMF_PROFILE_MESH_WEIGHTGEN_MBMESH
    int localrc;
    ESMCI_REGION_ENTER("MOAB Mesh Weight Generation", localrc)
    VM::logMemInfo(std::string("before MOAB Mesh Weight Generation"));
#endif

    // need to pass the pole_constraint_id for extrapolation
    //   |--> after pole handling implemented, which is after gridtombmesh
    if(!calc_regrid_wgts(mbmsrcp, mbmdstp, srcpl, dstpl, wts, 
                         &regridConserve, regridMethod,
                         regridPoleType, regridPoleNPnts,
                         regridScheme, map_type, 
                         extrapMethod,
                         extrapNumSrcPnts,
                         extrapDistExponent,
                         extrapNumLevels,
                         extrapNumInputLevels, 
                         &temp_unmappedaction,
                         set_dst_status, dst_status))
    Throw() << "Online regridding error" << std::endl;

#ifdef ESMF_PROFILE_MESH_WEIGHTGEN_MBMESH
    VM::logMemInfo(std::string("after MOAB Mesh Weight Generation"));
    ESMCI_REGION_EXIT("MOAB Mesh Weight Generation", localrc)
#endif

// #define OUTPUT_WEIGHTS
#ifdef OUTPUT_WEIGHTS
  cout << endl << "Weight Matrix" << endl;
  // print out weights
  WMat::WeightMap::iterator mb = wts.begin_row(), me = wts.end_row();
  for(; mb != me; ++mb) {
    WMat::Entry col = mb->first;
    vector<WMat::Entry> row = mb->second;

    cout << "[" << col.id << "," << "-" << "," << col.value << ","
         << col.src_id << "] - ";

    vector<WMat::Entry>::iterator vb = row.begin(), ve = row.end();
    for(; vb != ve; ++vb) {
      WMat::Entry rv = *vb;
      cout << "[" << rv.id << "," << "-" << "," << rv.value << ","
           << rv.src_id << "] ";
    }
    cout << endl;
  }
  cout << endl;

  // void *mbptr = (void *) mbmsrcp;
  // int len = 12; char fname[len];
  // sprintf(fname, "mesh_%d", Par::Rank());
  // MBMesh_write(&mbptr, fname, rc, len);


#endif




#ifdef PROGRESSLOG_on
    ESMC_LogDefault.Write("c_esmc_regrid_create(): Done with weight generation... check unmapped dest,", ESMC_LOGMSG_INFO);
#endif

#ifdef MEMLOG_on
   VM::logMemInfo(std::string("RegridCreate3.0"));
#endif

    // If requested get list of unmapped destination points
    std::vector<int> unmappedDstList;
    if (*has_udl) {
      if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
        get_mbmesh_elem_ids_not_in_wmat(mbmdstp, wts, &unmappedDstList);
      } else if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC) {
        // CURRENTLY DOESN'T WORK!!!
#if 0
        get_mbmesh_node_ids_not_in_wmat(mbmsrcp, wts, &unmappedDstList);
#endif
      } else { // Non-conservative
        get_mbmesh_node_ids_not_in_wmat(dstpl, wts, &unmappedDstList);
      }
    }
#ifdef PROGRESSLOG_on
    ESMC_LogDefault.Write("c_esmc_regrid_create(): More unmapped points checking.", ESMC_LOGMSG_INFO);
#endif

    // If user is worried about unmapped points then check that
    // here, because we have all the dest objects and weights
    // gathered onto the same proc.
    if (*unmappedaction==ESMCI_UNMAPPEDACTION_ERROR) {
      if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
        int missing_id;
        if (!all_mbmesh_elem_ids_in_wmat(mbmdstp, wts, &missing_id)) {
          int localrc;
          char msg[1024];
          sprintf(msg,"- There exist destination cells (e.g. id=%d) which don't overlap with any "
            "source cell",missing_id);
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP, msg,
             ESMC_CONTEXT, &localrc)) throw localrc;
        }
      } else if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC) {
        // CURRENTLY DOESN'T WORK!!!
#if 0
        if (!all_mbmesh_node_ids_in_wmat(mbmsrcp, wts)) {
          int localrc;
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "- There exist source points which can't be mapped to any "
            "destination point", ESMC_CONTEXT, &localrc)) throw localrc;
        }
#endif
      } else { // bilinear, patch, ...
        int missing_id;
        if (!all_mbmesh_node_ids_in_wmat(dstpl, wts, &missing_id)) {
          int localrc;
          char msg[1024];
          sprintf(msg,"- There exist destination points (e.g. id=%d) which can't be mapped to any "
            "source cell",missing_id);
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP, msg,
             ESMC_CONTEXT, &localrc)) throw localrc;
        }
      }
    }
 #ifdef PROGRESSLOG_on
    ESMC_LogDefault.Write("c_esmc_regrid_create(): Prepare for ArraySMMStore().", ESMC_LOGMSG_INFO);
#endif

#ifdef MEMLOG_on
  VM::logMemInfo(std::string("RegridCreate4.0"));
#endif

    /////// We have the weights, now set up the sparsemm object /////

    // Firstly, the index list
    std::pair<UInt,UInt> iisize = wts.count_matrix_entries();
    int num_entries = iisize.first;
    int *iientries = new int[2*iisize.first];
    int larg[2] = {2, static_cast<int>(iisize.first)};
    // Gather the list
    ESMCI::InterArray<int> ii(iientries, 2, larg);
    ESMCI::InterArray<int> *iiptr = &ii;

    double *factors = new double[iisize.first];

    // Translate weights to sparse matrix representation
    if (*regridMethod != ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC) {
      UInt i = 0;
      WMat::WeightMap::iterator wi = wts.begin_row(), we = wts.end_row();
      for (; wi != we; ++wi) {
        const WMat::Entry &w = wi->first;

        std::vector<WMat::Entry> &wcol = wi->second;

        // Construct factor index list
        for (UInt j = 0; j < wcol.size(); ++j) {
          UInt twoi = 2*i;
          const WMat::Entry &wc = wcol[j];

          // Construct factor list entry
          iientries[twoi+1] = w.id;  iientries[twoi] = wc.id;
          factors[i] = wc.value;

#define ESMF_REGRID_DEBUG_OUTPUT_WTS_ALL_off
#ifdef ESMF_REGRID_DEBUG_OUTPUT_WTS_ALL
          printf("d_id=%d  s_id=%d w=%20.17E \n",w.id,wc.id,wc.value);
#endif
#ifdef ESMF_REGRID_DEBUG_OUTPUT_WTS_SID
          if (wc.id==ESMF_REGRID_DEBUG_OUTPUT_WTS_SID) {
             printf("d_id=%d  s_id=%d w=%20.17E \n",w.id,wc.id,wc.value);
          }
#endif
 #ifdef ESMF_REGRID_DEBUG_OUTPUT_WTS_DID
          if (w.id==ESMF_REGRID_DEBUG_OUTPUT_WTS_DID) {
             printf("d_id=%d  s_id=%d w=%20.17E \n",w.id,wc.id,wc.value);
          }
#endif

          i++;
        } // for j
       } // for wi

    } else {
      UInt i = 0;
      WMat::WeightMap::iterator wi = wts.begin_row(), we = wts.end_row();
      for (; wi != we; ++wi) {
        const WMat::Entry &w = wi->first;

        std::vector<WMat::Entry> &wcol = wi->second;

        // Construct factor index list
        for (UInt j = 0; j < wcol.size(); ++j) {
          UInt twoi = 2*i;
          const WMat::Entry &wc = wcol[j];

          // Construct factor list entry
          // INVERT SRC and DST ID
          iientries[twoi+1] = wc.id;  iientries[twoi] = w.id;
          factors[i] = wc.value;

          //printf("d_id=%d  s_id=%d w=%f \n",w.id,wc.id,wc.value);

          i++;
        } // for j
      } // for wi
    }


//////////////////////////////////////////////////////////////////////////////////////
    // int *iientries2 = new int[2*iisize.first];
    // double *factors2 = new double[iisize.first];
    //
    // std::memcpy(factors2, factors, sizeof(double)*num_entries);
    // std::memcpy(iientries2, iientries, sizeof(int)*2*num_entries);
//////////////////////////////////////////////////////////////////////////////////////


#if 0
    ///// If conservative, translate split element weights to non-split //////
    if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
      if (mbmsrcp.is_split) translate_split_src_elems_in_wts(mbmsrcp, num_entries, iientries);
      if (mbmdstp.is_split) translate_split_dst_elems_in_wts(mbmdstp, num_entries, iientries, factors);
    }


    ///// If conservative then modify weights according to norm type //////
    if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
      if (*norm_type==ESMC_NORMTYPE_FRACAREA) change_wts_to_be_fracarea(mbmdstp, num_entries, iientries, factors);
    }
#endif


    // Copy status info from WMat to Array
    if (has_statusArray) {
      if ((*regridMethod==ESMC_REGRID_METHOD_CONSERVE) ||
          (*regridMethod==ESMC_REGRID_METHOD_CONSERVE_2ND)) {
        mbcopy_cnsv_rs_from_WMat_to_Array(&dst_status, statusArray);
      } else {
        mbcopy_rs_from_WMat_to_Array(&dst_status, statusArray);
      }
    }

#ifdef PROGRESSLOG_on
    char msgString[1024];
    sprintf(msgString, "c_esmc_regrid_create(): num_entries=%d.", num_entries);
    ESMC_LogDefault.Write(msgString, ESMC_LOGMSG_INFO);
    ESMC_LogDefault.Write("c_esmc_regrid_create(): Entering ArraySMMStore().", ESMC_LOGMSG_INFO);
#endif

#ifdef MEMLOG_on
  VM::logMemInfo(std::string("RegridCreate5.0"));
#endif

    // delete wts; // local garbage collection

#ifdef MEMLOG_on
    VM::logMemInfo(std::string("RegridCreate5.1"));
#endif

#ifdef C_SIDE_REGRID_FREED_MESH
    // enabling this freature currently breaks several tests
    delete mbmsrcp;
    delete mbmdstp;
    //TODO: also drop PointList objects here if possible to reduce Store() memory footrint
#endif

#ifdef MEMLOG_on
    VM::logMemInfo(std::string("RegridCreate5.2"));
#endif

#ifdef ESMF_PROFILE_MESH_SMMSTORE_MBMESH
    ESMCI_REGION_ENTER("MOAB Mesh ArraySMMStore", localrc)
    VM::logMemInfo(std::string("before MOAB Mesh ArraySMMStore"));
#endif

    // Build the ArraySMM
    if (*has_rh != 0) {
      int localrc;
      enum ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_R8;
      ESMC_Logical ignoreUnmatched = ESMF_FALSE;
       FTN_X(c_esmc_arraysmmstoreind4)(arraysrcpp, arraydstpp, rh, &tk, factors,
            &num_entries, iiptr, &ignoreUnmatched, srcTermProcessing,
            pipelineDepth, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
    }

#ifdef ESMF_PROFILE_MESH_SMMSTORE_MBMESH
    VM::logMemInfo(std::string("after MOAB Mesh ArraySMMStore"));
    ESMCI_REGION_EXIT("MOAB Mesh ArraySMMStore", localrc)
#endif

// #define DUMP_FACTORS_AFTER_SMM
#ifdef DUMP_FACTORS_AFTER_SMM
    for (int i = 0; i < num_entries; ++i) {
      printf("regridglue: factorIndexList[%d, %d] factorList[%f]\n", iientries[2*i], iientries[2*i+1], factors[i]);
    }
#endif

#ifdef PROGRESSLOG_on
    ESMC_LogDefault.Write("c_esmc_regrid_create(): Returned from ArraySMMStore().", ESMC_LOGMSG_INFO);
#endif

#ifdef MEMLOG_on
  VM::logMemInfo(std::string("RegridCreate6.0"));
#endif

    *nentries = num_entries;
    // Clean up.  If has_iw, then we will use the arrays to
    // fill out the users pointers.  These will be deleted following a copy.
    if (*has_iw == 0) {
      delete [] factors;
      delete [] iientries;
      *nentries = 0;
    } else {
      // Save off the weights so the F90 caller can allocate arrays and
      // copy the values.
      if (num_entries>0) {
        *tweights = new ESMCI::TempWeights;
        (*tweights)->nentries = num_entries;
        (*tweights)->factors = factors;
        (*tweights)->iientries = iientries;
      } else {
        // No weights, so don't allocate structure
        // Make sure copying method below takes this into account
        *tweights = NULL;
      }
    }

    // Setup structure to transfer unmappedDstList
    *_num_udl=0;
    *_tudl=NULL;
    if (*has_udl) {
      ESMCI::TempUDL *tudl = new ESMCI::TempUDL;

      // Get number of unmapped points
      int num_udl=unmappedDstList.size();

      // Allocate and fill udl list in struct
      tudl->udl = NULL;
       if (num_udl > 0) {
         tudl->udl = new int[num_udl];
         for (int i=0; i<num_udl; i++) {
           tudl->udl[i]=unmappedDstList[i];
         }
       }

       // Output information
       *_num_udl=num_udl;
        *_tudl=tudl;
    }


  } catch(std::exception &x) {
    // catch Mesh exception return code
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                                  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  } catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

#ifdef PROGRESSLOG_on
  ESMC_LogDefault.Write("c_esmc_regrid_create(): Final return.", ESMC_LOGMSG_INFO);
#endif

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}


// Get the list of ids in the mesh, but not in the wts
// (i.e. if mesh is the dest. mesh, the unmapped points)
static void get_mbmesh_node_ids_not_in_wmat(PointList *pointlist, WMat &wts, std::vector<int> *missing_ids) {

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  wi=wts.begin_row();
  int id;
  int curr_num_pts = pointlist->get_curr_num_pts();
  // Loop checking that all nodes have weights
  for (int i=0; i<curr_num_pts; i++) {

    // Skip non local nodes
    // ??

    // get node id
    id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }

    // If teh current weight is not equal to the node id, then we must have passed it, so add it to the list
    if (wi->first.id != id) {
      missing_ids->push_back(id);
    }
  }

}


// Get the list of ids in the mesh, but not in the wts
// (i.e. if mesh is the dest. mesh, the unmapped points)
static void get_mbmesh_elem_ids_not_in_wmat(MBMesh *mbmesh, WMat &wts, std::vector<int> *missing_ids) {

  int merr = 0;

  // Get Parallel Information
  int localrc;
  int localpet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // get verts entities, by type
  Range elems;
  merr = mbmesh->mesh->get_entities_by_dimension(0, mbmesh->pdim, elems);
  if (merr != MB_SUCCESS) Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  for (Range::iterator it = elems.begin(); it != elems.end(); it++) {
    // node owned : owner_tag
    int owner;
    merr=mbmesh->mesh->tag_get_data(mbmesh->owner_tag, &(*it), 1, &owner);
    if (merr != MB_SUCCESS) Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];

    if (owner == localpet) continue;

    // node mask : node_mask_val_tag
    int masked;
    merr=mbmesh->mesh->tag_get_data(mbmesh->elem_mask_tag, &(*it), 1, &masked);
    if (merr != MB_SUCCESS) Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    if (masked) continue;

    // id : gid_tag
    int elem_id;
    merr=mbmesh->mesh->tag_get_data(mbmesh->gid_tag,  &(*it), 1, &elem_id);
    if (merr != MB_SUCCESS)
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id <elem_id)) wi++;

    // If the current weight is not equal to the elem id, then we must have passed it, so add it to the list
    if (wi->first.id != elem_id) {
      missing_ids->push_back(elem_id);
    }

  }
}

bool all_mbmesh_node_ids_in_wmat(PointList *pointlist, WMat &wts, int *missing_id){

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  wi=wts.begin_row();
  int id;
  int curr_num_pts = pointlist->get_curr_num_pts();

  for (int i=0; i<curr_num_pts; i++) {

    // Skip non local nodes
    // ??

    // get node id
    id = pointlist->get_id(i);

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id < id)) {
      wi++;
    }
    // printf("PET %d pointlist point %d, node id %d start %d end %d\n", Par::Rank(), i, id, wt_id, wi->first.id);

    // If we're at the end of the weights then exit saying we don't have
    // all of them
    if (wi==we) {
      *missing_id=id;
      char msg[1024];
      sprintf(msg,"Destination id=%d NOT found in weight matrix.",id);
      ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
      return false;
    }

    // If we're not equal to the node id then we must have passed it
    if (wi->first.id != id) {
      *missing_id=id;
      char msg[1024];
      sprintf(msg,"Destination id=%d NOT found in weight matrix.",id);
      ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
      return false;
    }

  }


  // Still here, so must have found them all
  return true;

}

bool all_mbmesh_elem_ids_in_wmat(MBMesh *mbmesh, WMat &wts, int *missing_id) {

  int merr = 0;

  // Get Parallel Information
  int localrc;
  int localpet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // get verts entities, by type
  Range elems;
  merr = mbmesh->mesh->get_entities_by_dimension(0, mbmesh->pdim, elems);
  if (merr != MB_SUCCESS) Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  for (Range::iterator it = elems.begin(); it != elems.end(); it++) {
    // node owned : owner_tag
    int owner;
    merr=mbmesh->mesh->tag_get_data(mbmesh->owner_tag, &(*it), 1, &owner);
    if (merr != MB_SUCCESS) Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];

    if (owner == localpet) continue;

    // node mask : node_mask_val_tag
    int masked;
    merr=mbmesh->mesh->tag_get_data(mbmesh->elem_mask_tag, &(*it), 1, &masked);
    if (merr != MB_SUCCESS) Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    if (masked) continue;

    // id : gid_tag
    int elem_id;
    merr=mbmesh->mesh->tag_get_data(mbmesh->gid_tag, &(*it), 1, &elem_id);
    if (merr != MB_SUCCESS)
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id <elem_id)) wi++;

    // If we're at the end of the weights then exit saying we don't have
    // all of them
    if (wi==we) {
      *missing_id=elem_id;
      char msg[1024];
      sprintf(msg,"Destination id=%d NOT found in weight matrix.",elem_id);
      ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
      return false;
    }

    // If we're not equal to the elem id then we must have passed it
    if (wi->first.id != elem_id) {
      *missing_id=elem_id;
      char msg[1024];
      sprintf(msg,"Destination id=%d NOT found in weight matrix.",elem_id);
      ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
      return false;
    }
  }

  // Still here, so must have found them all
  return true;
}

void mbcopy_rs_from_WMat_to_Array(WMat *wmat, ESMCI::Array *array) {

  // Look at ESMCI_Grid.C getGlobalID() for how to get sequence ids

  // Get Distgrid
  DistGrid *distgrid=array->getDistGrid();

  // Get reduced dimCount
  int redDimCount=array->getRank()-array->getTensorCount();

  // Only support up to a certian dim in this subroutine
#define CWTOA_MAXDIM 4
  if (redDimCount > CWTOA_MAXDIM) {
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        " Regridding only supports info Fields (e.g. dstStatusField) of dimensions 4 and below ",
                                     ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Get computational bound arrays
  const int *clb_array=array->getComputationalLBound();
  const int *cub_array=array->getComputationalUBound();

  // Get exclusive bound arrays
  const int *elb_array=array->getExclusiveLBound();

  // Get number of localDEs
  int localDECount=distgrid->getDELayout()->getLocalDeCount();

  // Loop over localDes
  for (int lDE=0; lDE < localDECount; lDE++) {
    // Only support up to 4D for regridding right now
    int lbnd[CWTOA_MAXDIM]={0,0,0,0};
    int ubnd[CWTOA_MAXDIM]={0,0,0,0};
    int elbnd[CWTOA_MAXDIM]={0,0,0,0};
    int ind[CWTOA_MAXDIM]={0,0,0,0};
    int ind_m_elbnd[CWTOA_MAXDIM]={0,0,0,0};


    // Get localArray corresponding to this localDE
    LocalArray *localArray=array->getLocalarrayList()[lDE];

    // Get computational bounds of this DE
    const int *clb=clb_array+lDE*redDimCount;
    const int *cub=cub_array+lDE*redDimCount;

    // Get exclusive lower bounds of this DE
    const int *elb=elb_array+lDE*redDimCount;

    // Copy DE Bounds
    // NOTE: anything beyond array dimensions will be 0 & 0 so those loops won't iterate
    for (int i=0; i<redDimCount; i++) {
      lbnd[i]=clb[i];
      ubnd[i]=cub[i];
      elbnd[i]=elb[i];
    }


#if 0
    // dump whole matrix for debugging
    int j=0;
    WMat::WeightMap::iterator wi = wmat->begin_row(), we = wmat->end_row();
    for (; wi != we; ++wi) {
        const WMat::Entry &w = wi->first;
        std::vector<WMat::Entry> &wcol = wi->second;

        printf("%d col_size=%d\n",j,wcol.size());

        const WMat::Entry &wc = wcol[0];

        printf("%d dst_id=%d rs=%d\n",j,w.id,wc.id);

        j++;
      }
#endif

    // Loop indices in DE
    int pos=0;
    for (int i3=lbnd[3]; i3<=ubnd[3]; i3++) {
      ind[3]=i3;
      ind_m_elbnd[3]=i3-elbnd[3];
    for (int i2=lbnd[2]; i2<=ubnd[2]; i2++) {
      ind[2]=i2;
      ind_m_elbnd[2]=i2-elbnd[2];
    for (int i1=lbnd[1]; i1<=ubnd[1]; i1++) {
      ind[1]=i1;
      ind_m_elbnd[1]=i1-elbnd[1];
     for (int i0=lbnd[0]; i0<=ubnd[0]; i0++) {
      ind[0]=i0;
      ind_m_elbnd[0]=i0-elbnd[0];

      // Get sequence index of this point
      int localrc;
      int seq_ind;
      std::vector<int> seq_indV;
      localrc=distgrid->getSequenceIndexLocalDe(lDE,ind_m_elbnd,seq_indV);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
 /* XMRKX */

      if (seq_indV.size() > 0)
        seq_ind = seq_indV[0];
      else
        seq_ind = -1; // invalidate

      // If it's not in the WMat, then it's been masked out, so init. to masked
      ESMC_I4 regrid_status=ESMC_REGRID_STATUS_DST_MASKED;

      // Get regrid_status from WMat
      WMat::WeightMap::iterator wi = wmat->lower_bound_id_row(seq_ind);

      // If it's not found in the matrix, then just leave at init value
      if (wi != wmat->weights.end()) {

        // Get information about this entry in the matrix
        const WMat::Entry &w = wi->first;
        std::vector<WMat::Entry> &wcol = wi->second;

        // Make sure this entry has the correct id
        // (If it's not found in the matrix, then just leave at init value)
        if (w.id == seq_ind) {

          // Make sure there is only one answer
          if (wcol.size() != 1) {
            int localrc;
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                             " more than one entry found for sequence id",
                                             ESMC_CONTEXT, &localrc)) throw localrc;
          }

          // Get the one column entry
          const WMat::Entry &wc = wcol[0];

          // Get the colum id
          regrid_status=wc.id;
        }
      }

      // printf("%d# %d si=%d rs=%d\n",Par::Rank(),pos,seq_ind,regrid_status);
      pos++;

      // Set regrid_status in local Array
      localArray->setData(ind, regrid_status);
    }
    }
    }
    }
  }
}


void mbcopy_cnsv_rs_from_WMat_to_Array(WMat *wmat, ESMCI::Array *array) {
  // A small tolerence to take care of rounding effects
#define ZERO_TOL 1.0E-14

  // Look at ESMCI_Grid.C getGlobalID() for how to get sequence ids

  // Get Distgrid
  DistGrid *distgrid=array->getDistGrid();

  // Get reduced dimCount
  int redDimCount=array->getRank()-array->getTensorCount();

  // Only support up to a certian dim in this subroutine
#define CWTOA_MAXDIM 4
  if (redDimCount > CWTOA_MAXDIM) {
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        " Regridding only supports info Fields (e.g. dstStatusField) of dimensions 4 and below ",
                                     ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Get computational bound arrays
  const int *clb_array=array->getComputationalLBound();
  const int *cub_array=array->getComputationalUBound();

  // Get exclusive bound arrays
  const int *elb_array=array->getExclusiveLBound();

  // Get number of localDEs
  int localDECount=distgrid->getDELayout()->getLocalDeCount();

  // Loop over localDes
  for (int lDE=0; lDE < localDECount; lDE++) {
    // Only support up to 4D for regridding right now
    int lbnd[CWTOA_MAXDIM]={0,0,0,0};
    int ubnd[CWTOA_MAXDIM]={0,0,0,0};
    int elbnd[CWTOA_MAXDIM]={0,0,0,0};
    int ind[CWTOA_MAXDIM]={0,0,0,0};
    int ind_m_elbnd[CWTOA_MAXDIM]={0,0,0,0};


    // Get localArray corresponding to this localDE
    LocalArray *localArray=array->getLocalarrayList()[lDE];

    // Get computational bounds of this DE
    const int *clb=clb_array+lDE*redDimCount;
    const int *cub=cub_array+lDE*redDimCount;

    // Get exclusive lower bounds of this DE
    const int *elb=elb_array+lDE*redDimCount;

    // Copy DE Bounds
    // NOTE: anything beyond array dimensions will be 0 & 0 so those loops won't iterate
    for (int i=0; i<redDimCount; i++) {
      lbnd[i]=clb[i];
      ubnd[i]=cub[i];
      elbnd[i]=elb[i];
    }


#if 0
    // dump whole matrix for debugging
    int j=0;
    WMat::WeightMap::iterator wi = wmat->begin_row(), we = wmat->end_row();
    for (; wi != we; ++wi) {
        const WMat::Entry &w = wi->first;
        std::vector<WMat::Entry> &wcol = wi->second;

        printf("%d col_size=%d\n",j,wcol.size());

        const WMat::Entry &wc = wcol[0];

        printf("%d dst_id=%d rs=%d\n",j,w.id,wc.id);

        j++;
      }
#endif

    // Loop indices in DE
    int pos=0;
    for (int i3=lbnd[3]; i3<=ubnd[3]; i3++) {
      ind[3]=i3;
      ind_m_elbnd[3]=i3-elbnd[3];
    for (int i2=lbnd[2]; i2<=ubnd[2]; i2++) {
      ind[2]=i2;
      ind_m_elbnd[2]=i2-elbnd[2];
    for (int i1=lbnd[1]; i1<=ubnd[1]; i1++) {
      ind[1]=i1;
      ind_m_elbnd[1]=i1-elbnd[1];
     for (int i0=lbnd[0]; i0<=ubnd[0]; i0++) {
      ind[0]=i0;
      ind_m_elbnd[0]=i0-elbnd[0];

      // Get sequence index of this point
      int localrc;
      int seq_ind;
      std::vector<int> seq_indV;
      localrc=distgrid->getSequenceIndexLocalDe(lDE,ind_m_elbnd,seq_indV);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception

      if (seq_indV.size() > 0)
        seq_ind = seq_indV[0];
      else
        seq_ind = -1; // invalidate

      // If it's not in the WMat, then it's been masked out, so init. to masked
      ESMC_I4 regrid_status=ESMC_REGRID_STATUS_OUTSIDE;

      // Get regrid_status from WMat
      WMat::WeightMap::iterator wi = wmat->lower_bound_id_row(seq_ind);

      // If it's not found in the matrix, then just leave at init value
      if (wi != wmat->weights.end()) {

        // Get information about this entry in the matrix
        const WMat::Entry &w = wi->first;
        std::vector<WMat::Entry> &wcol = wi->second;

        // Make sure this entry has the correct id
        // (If it's not found in the matrix, then just leave at init value)
        if (w.id == seq_ind) {

          // If there are no entries then it's unmapped, so leave at unmapped and continue
          if (wcol.size() < 1) continue;

          // If this isn't a masked dst, then process
          if (wcol[0].idx != ESMC_REGRID_STATUS_DST_MASKED) {

            // Loop through processing
            regrid_status=0; // set to 0, so we can put in different statuses
            double tot_frac_used=0.0;
            for (UInt j = 0; j < wcol.size(); ++j) {
              const WMat::Entry &wc = wcol[j];

              //printf("dst_id=%d  type=%d src_id=%d frac=%g\n",w.id,wc.idx,wc.id,wc.value);

              // Set the flag for this type
              regrid_status |= (ESMC_I4)wc.idx;

              // Add in the fraction of the cell that's this type
              tot_frac_used += wc.value;
            }

            // If there's any left in the cell, then include an unmapped portion
            if ((1.0-tot_frac_used) > ZERO_TOL) regrid_status |= ESMC_REGRID_STATUS_OUTSIDE;
          } else {
            regrid_status=ESMC_REGRID_STATUS_DST_MASKED;
          }
        }
      }

      // printf("%d# %d si=%d rs=%d\n",Par::Rank(),pos,seq_ind,regrid_status);
      pos++;

      // Set regrid_status in local Array
      localArray->setData(ind, regrid_status);
    }
    }
    }
    }

  }
#undef ZERO_TOL
#undef cWtoA_MAXDIM
}


int calc_regrid_wgts(MBMesh *srcmbmp, MBMesh *dstmbmp,
                     PointList *srcpl, PointList *dstpl,
                     IWeights &wts,
                     int *regridConserve, int *regridMethod,
                     int *regridPoleType, int *regridPoleNPnts,
                     int *regridScheme,
                     int *map_type, 
                     int *extrapMethod,
                     int *extrapNumSrcPnts,
                     ESMC_R8 *extrapDistExponent,
                     int *extrapNumLevels,
                     int *extrapNumInputLevels, 
                     int *unmappedaction,
                     bool set_dst_status, WMat &dst_status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_regrid_wgts()"
  Trace __trace("calc_regrid_wgts()");

  int localrc;

  // Branch to different subroutines based on method
  if (*regridMethod == ESMC_REGRID_METHOD_CONSERVE) {
    calc_cnsrv_regrid_wgts(srcmbmp, dstmbmp, wts);
  } else if (*regridMethod == ESMC_REGRID_METHOD_BILINEAR) {
    calc_bilinear_regrid_wgts(srcmbmp, dstpl, wts, map_type,
                              set_dst_status, dst_status);
  } else if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST) {
    calc_nearest_regrid_wgts(srcpl, dstpl, wts,
                             set_dst_status, dst_status, 
                             regridMethod, extrapNumSrcPnts,
                             extrapDistExponent);
  } else if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_IDAVG) {
    calc_nearest_regrid_wgts(srcpl, dstpl, wts, 
                             set_dst_status, dst_status, 
                             regridMethod, extrapNumSrcPnts, 
                             extrapDistExponent);
  } else if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC) {
    calc_nearest_regrid_wgts(dstpl, srcpl, wts,
                             set_dst_status, dst_status,
                             regridMethod, extrapNumSrcPnts,
                             extrapDistExponent);
  } else {
    Throw() << "This regrid method is not currently supported.";
  }

  // Do extrapolation if the user has requested it
  if (*extrapMethod != ESMC_EXTRAPMETHOD_NONE) {
    MBMesh_Extrapolate(srcmbmp, srcpl, dstmbmp, dstpl,
                       wts, 
                       // map_type, // RLO: not sure if this is needed
                       NULL, // pole_constraint_id only used with src_mesh
                       extrapMethod, extrapNumSrcPnts, extrapDistExponent,
                       extrapNumLevels, extrapNumInputLevels,
                       set_dst_status, dst_status, &localrc);
  }

  return 1;

}

#undef  ESMC_METHOD

#endif // ESMF_MOAB
