// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Mesh_Regrid_Glue.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
 // INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"
#include "ESMCI_GridToMesh.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/ESMCI_MeshRead.h"
#include "Mesh/include/ESMCI_MeshRegrid.h"
#include "Mesh/include/ESMCI_Exception.h"
#include "Mesh/include/ESMCI_Integrate.h"
#include "Mesh/include/ESMCI_Interp.h"
#include "Mesh/include/ESMCI_Extrapolation.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/ESMCI_Phedra.h"
#include "Mesh/include/ESMCI_Mesh_Regrid_Glue.h"
#include "Mesh/include/ESMCI_MeshMerge.h"

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


using namespace ESMCI;


 

// prototypes from below
static bool all_mesh_node_ids_in_wmat(PointList *pointlist, WMat &wts, int *missing_id);
static bool all_mesh_elem_ids_in_wmat(Mesh *mesh, WMat &wts, int *missing_id);
static bool any_cells_in_mesh_degenerate(Mesh *mesh);
static void get_mesh_node_ids_not_in_wmat(PointList *pointlist, WMat &wts, std::vector<int> *missing_ids);
static void get_mesh_elem_ids_not_in_wmat(Mesh *mesh, WMat &wts, std::vector<int> *missing_ids);
static void translate_split_src_elems_in_wts(Mesh *srcmesh, int num_entries,
                                      int *iientries);
static void translate_split_dst_elems_in_wts(Mesh *dstmesh, int num_entries,
                                      int *iientries, double *factors);
static void change_wts_to_be_fracarea(Mesh *mesh, int num_entries,
                               int *iientries, double *factors);


// external C functions
 extern "C" void FTN_X(c_esmc_arraysmmstore)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle,
    ESMC_TypeKind_Flag *typekind, void *factorList, int *factorListCount,
    ESMCI::InterfaceInt *factorIndexList, ESMC_Logical *ignoreUnmatched,
    int *srcTermProcessing, int *pipelineDepth, int *rc);

void CpMeshDataToArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray);
void CpMeshElemDataToArray(Grid &grid, int staggerloc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray);
void PutElemAreaIntoArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array);



void ESMCI_regrid_create(ESMCI::VM **vmpp,
                     Mesh **meshsrcpp, ESMCI::Array **arraysrcpp, ESMCI::PointList **plsrcpp,
                     Mesh **meshdstpp, ESMCI::Array **arraydstpp, ESMCI::PointList **pldstpp,
                     int *regridMethod, 
                      int *map_type,
                     int *norm_type,
                     int *regridPoleType, int *regridPoleNPnts,  
                     int *regridScheme, 
                     int *unmappedaction, int *_ignoreDegenerate,
                     int *srcTermProcessing, int *pipelineDepth, 
                     ESMCI::RouteHandle **rh, int *has_rh, int *has_iw,
                     int *nentries, ESMCI::TempWeights **tweights,
                     int *has_udl, int *_num_udl, ESMCI::TempUDL **_tudl, 
                     int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_create()" 
  Trace __trace(" FTN_X(regrid_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridsrcpp, ESMCI::Grid **griddstcpp, int*rc");


  ESMCI::VM *vm = *vmpp;
  ESMCI::Array &srcarray = **arraysrcpp;
  ESMCI::Array &dstarray = **arraydstpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  Mesh *srcmesh = *meshsrcpp;


  Mesh *dstmesh = *meshdstpp;
  PointList *dstpointlist = *pldstpp;
  PointList *srcpointlist = *plsrcpp;

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
    if (*_ignoreDegenerate == 1) ignoreDegenerate=true;

    //// Precheck Meshes for errors
    bool degenerate=false;
 
    // If not ignoring, check for degenerate elements
    if (!ignoreDegenerate) {
      // Check source mesh elements 
      if ((*regridMethod==ESMC_REGRID_METHOD_CONSERVE) ||
          (*regridMethod==ESMC_REGRID_METHOD_BILINEAR) ||
          (*regridMethod==ESMC_REGRID_METHOD_PATCH)) {
        degenerate=any_cells_in_mesh_degenerate(srcmesh);
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
        degenerate=any_cells_in_mesh_degenerate(dstmesh);

        // Degenerate
        if (degenerate) {
          int localrc;
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "- Dst contains a cell which has corners close enough that the cell "
        "collapses to a line or point", ESMC_CONTEXT, &localrc)) throw localrc;
        }
      }
    }

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


    // to do NEARESTDTOS just do NEARESTSTOD and invert results
    if (*regridMethod != ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC) { 

      if(!online_regrid(srcmesh, srcpointlist, dstmesh, dstpointlist, wts, &regridConserve, 
			regridMethod, regridPoleType, regridPoleNPnts, 
                        regridScheme, map_type, &temp_unmappedaction)) {
        Throw() << "Online regridding error" << std::endl;
      }




    } else {
      int tempRegridMethod=ESMC_REGRID_METHOD_NEAREST_SRC_TO_DST;

      if(!online_regrid(dstmesh, dstpointlist, srcmesh, srcpointlist, wts, &regridConserve, 
			&tempRegridMethod, regridPoleType, regridPoleNPnts, 
                        regridScheme, map_type, &temp_unmappedaction))
        Throw() << "Online regridding error" << std::endl;
    }
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
        get_mesh_elem_ids_not_in_wmat(dstmesh, wts, &unmappedDstList);
      } else if (*regridMethod == ESMC_REGRID_METHOD_NEAREST_DST_TO_SRC) { 
        // CURRENTLY DOESN'T WORK!!!
#if 0
        get_mesh_node_ids_not_in_wmat(srcmesh, wts, &unmappedDstList);
#endif

      } else { // Non-conservative
        get_mesh_node_ids_not_in_wmat(dstpointlist, wts, &unmappedDstList);
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
        if (!all_mesh_elem_ids_in_wmat(dstmesh, wts, &missing_id)) {
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
        if (!all_mesh_node_ids_in_wmat(srcmesh, wts)) {
          int localrc;
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "- There exist source points which can't be mapped to any "
            "destination point", ESMC_CONTEXT, &localrc)) throw localrc;
        }
#endif

      } else { // bilinear, patch, ...
        int missing_id;

	if (!all_mesh_node_ids_in_wmat(dstpointlist, wts, &missing_id)) {
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
    int larg[2] = {2, iisize.first};
    // Gather the list
    ESMCI::InterfaceInt ii(iientries, 2, larg);
    ESMCI::InterfaceInt *iiptr = &ii;

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
          
          // printf("d_id=%d  s_id=%d w=%f \n",w.id,wc.id,wc.value);
          
          i++;
        } // for j
      } // for wi
    }

    ///// If conservative, translate split element weights to non-split //////
    if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
      if (srcmesh->is_split) translate_split_src_elems_in_wts(srcmesh, num_entries, iientries);
      if (dstmesh->is_split) translate_split_dst_elems_in_wts(dstmesh, num_entries, iientries, factors);
    }


    ///// If conservative then modify weights according to norm type //////
    if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
      if (*norm_type==ESMC_NORM_TYPE_FRACAREA) change_wts_to_be_fracarea(dstmesh, num_entries, iientries, factors);
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

    // Build the ArraySMM
    if (*has_rh != 0) {
      int localrc;
      enum ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_R8;
      ESMC_Logical ignoreUnmatched = ESMF_FALSE;
      FTN_X(c_esmc_arraysmmstore)(arraysrcpp, arraydstpp, rh, &tk, factors,
            &num_entries, iiptr, &ignoreUnmatched, srcTermProcessing, 
            pipelineDepth, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
    }

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

void ESMCI_regrid_getiwts(ESMCI::VM **vmpp, Grid **gridpp,
                   Mesh **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *regridScheme, int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getiwts()" 
  Trace __trace(" FTN_X(regrid_getiwts)()");
  ESMCI::VM *vm = *vmpp;
  ESMCI::Array &array = **arraypp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  Mesh &mesh = **meshpp;
  Grid &grid = **gridpp;

  try {

    // Get the integration weights
    MEField<> *iwts = mesh.GetField("iwts");
    if (!iwts) Throw() << "Could not find integration weights field on this mesh"
                             <<std::endl; 

    if(!get_iwts(mesh, iwts, regridScheme))
      Throw() << "Online regridding error" << std::endl;
    
    CpMeshDataToArray(grid, *staggerLoc, mesh, array, iwts);
 
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
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}


void ESMCI_regrid_getarea(Grid **gridpp,
                   Mesh **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *regridScheme, int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getarea()" 
  Trace __trace(" FTN_X(regrid_getarea)()");
  ESMCI::Array &array = **arraypp;

  Mesh &mesh = **meshpp;
  Grid &grid = **gridpp;

  try {

    PutElemAreaIntoArray(grid, *staggerLoc, mesh, array);
 
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
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}



// Assumes array is center stagger loc
void ESMCI_regrid_getfrac(Grid **gridpp,
                   Mesh **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getfrac()" 
  Trace __trace(" FTN_X(regrid_getfrac)()");

  ESMCI::Array &array = **arraypp;
  Mesh &mesh = **meshpp;
  Grid &grid = **gridpp;

  try {


    // Get the integration weights
    MEField<> *frac = mesh.GetField("elem_frac");
    if (!frac) Throw() << "Could not find elem_frac field on this mesh"
                             <<std::endl; 

    CpMeshElemDataToArray(grid, *staggerLoc, mesh, array, frac);
 
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
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}


// Get the list of ids in the mesh, but not in the wts 
// (i.e. if mesh is the dest. mesh, the unmapped points)
static void get_mesh_node_ids_not_in_wmat(PointList *pointlist, WMat &wts, std::vector<int> *missing_ids) {

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
static void get_mesh_elem_ids_not_in_wmat(Mesh *mesh, WMat &wts, std::vector<int> *missing_ids) {

  // Get mask Field
  MEField<> *mptr = mesh->GetField("elem_mask");

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Get mesh node iterator that goes through in order of id
  Mesh::MeshObjIDMap::const_iterator ei=mesh->map_begin(MeshObj::ELEMENT), ee=mesh->map_end(MeshObj::ELEMENT);

  // Loop checking that all nodes have weights
  for (; ei != ee; ++ei) {
    const MeshObj &elem=*ei;

    // Skip non local nodes
    if (!GetAttr(elem).is_locally_owned()) continue;

    // Skip masked elements
    if (mptr != NULL) {
      double *m=mptr->data(elem);
      if (*m > 0.5) continue;
    }

    // get node id
    int elem_id=elem.get_id();

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id <elem_id)) {
      wi++;
    }

    // If teh current weight is not equal to the node id, then we must have passed it, so add it to the list
    if (wi->first.id != elem_id) {
      missing_ids->push_back(elem_id);
    }
  }
}

 

bool all_mesh_node_ids_in_wmat(PointList *pointlist, WMat &wts, int *missing_id) {

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

bool all_mesh_elem_ids_in_wmat(Mesh *mesh, WMat &wts, int *_missing_id) {

  // Get mask Field
  MEField<> *mptr = mesh->GetField("elem_mask");

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Get mesh node iterator that goes through in order of id
  Mesh::MeshObjIDMap::const_iterator ei=mesh->map_begin(MeshObj::ELEMENT), ee=mesh->map_end(MeshObj::ELEMENT);

  // Loop checking that all elems have weights
  bool missing=false;
  int missing_id=-1;
  for (; ei != ee; ++ei) {
    const MeshObj &elem=*ei;

    // Skip non local elems
    if (!GetAttr(elem).is_locally_owned()) continue;

    // Skip masked elements
    if (mptr != NULL) {
      double *m=mptr->data(elem);
      if (*m > 0.5) continue;
    }

    // get elem id
    int elem_id=elem.get_id();

    // If we've entered the range of split elements then just quit.
    // (Elements are in order by id here and all the split elems are at top.
    // If we've reached this point then part of the split elem mapped
    // (the part with the orig_id), so no use checking the rest.)
    if (mesh->is_split && (elem_id > mesh->max_non_split_id)) break;

    // Advance weights until not less than elem id
    while ((wi != we) && (wi->first.id <elem_id)) {
      wi++;
    }

    // If we're at the end of the weights then exit saying we don't have 
    // all of them
    if (wi==we) {
      missing=true;
      missing_id=elem_id;
      break;
    }

    // If we're not equal to the elem id then we must have passed it
    if (wi->first.id != elem_id) {
      missing=true;
      missing_id=elem_id;
      break;
    }
  }

  // If not missing leave
  if (!missing) return true;

  // If the mesh contains split elements, we could have 
  // a false missing element, so only leave if we're not split
  if (missing && !mesh->is_split) {
    *_missing_id=missing_id;
    char msg[1024];
    sprintf(msg,"Destination id=%d NOT found in weight matrix.",missing_id);
    ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
    return false;
  }

  //
  // We should only reach this point if we are split and have missing elements //
  //

  // Count the number of entries in the list
  int num_dst_ids=0;
  for (wi = wts.begin_row(); wi != we; ++wi) {
    num_dst_ids++;
  }

  // If there are no dst ids, then leave
  if (num_dst_ids <= 0) return true;

  // Allocate space for ids
  UInt *dst_ids = NULL;
  if (num_dst_ids>0) dst_ids=new UInt[num_dst_ids];

  // Loop through weights generating a list of destination ids
  int pos=0;
  for (wi = wts.begin_row(); wi != we; ++wi) {
    const WMat::Entry &w = wi->first;

    // Get original id
    UInt orig_id;
    std::map<UInt,UInt>::iterator soi =  mesh->split_to_orig_id.find(w.id);
    if (soi == mesh->split_to_orig_id.end()) {
      orig_id=w.id;
    } else {
      orig_id=soi->second;
    }

    // Put in array
    dst_ids[pos]=orig_id;

    // Next position
    pos++;
  }

  // Sort list
  std::sort(dst_ids,dst_ids+num_dst_ids);

  // Loop checking that all elems have weights with new sorted list
  missing=false;
  missing_id=-1;
  pos=0;
  for (ei=mesh->map_begin(MeshObj::ELEMENT); ei != ee; ++ei) {
    const MeshObj &elem=*ei;

    // Skip non local elems
    if (!GetAttr(elem).is_locally_owned()) continue;

    // Skip masked elements
    if (mptr != NULL) {
      double *m=mptr->data(elem);
      if (*m > 0.5) continue;
    }

    // get elem id
    int elem_id=elem.get_id();

    // If we've entered the range of split elements then just quit.
    // (Elements are in order by id here and all the split elems are at top.
    // If we've reached this point then part of the split elem mapped
    // (the part with the orig_id), so no use checking the rest.)
    if (mesh->is_split && (elem_id > mesh->max_non_split_id)) break;

    // Advance weights until not less than elem id
    while ((pos<num_dst_ids) && (dst_ids[pos] < elem_id)) {
      pos++;
    }

    // If we're at the end of the weights then exit saying we don't have 
    // all of them
    if (pos >= num_dst_ids) {
      missing=true;
      missing_id=elem_id;
      break;
    }

    // If we're not equal to the elem id then we must have passed it
    if (dst_ids[pos] != elem_id) {
      missing=true;
      missing_id=elem_id;
      break;
    }
  }

  // deallocate list
  if (dst_ids !=NULL) delete [] dst_ids;

  // If the mesh contains split elements, we could have 
  // a false missing element, so only leave if we're not split
  if (missing) {
    *_missing_id=missing_id;
    char msg[1024];
    sprintf(msg,"Destination id=%d NOT found in weight matrix.",missing_id);
    ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
    return false;
  }

  // Still here, so must have found them all
  return true;
}




// OLD VERSION, BUT NOW WE SUPPORT clockwise and concave
#undef  ESMC_METHOD
#define ESMC_METHOD "any_cells_in_mesh_degenerate(Mesh &mesh)" 
static bool any_cells_in_mesh_degenerate(Mesh *meshp) {
  
  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];

  int num_poly_nodes_orig;
  double poly_coords_orig[MAX_NUM_POLY_COORDS];
   
  // Translate to mesh
  Mesh &mesh=*meshp;

  // Get coord field
  MEField<> *cfield = mesh.GetCoordField();

  // Get mask Field
  MEField<> *mptr = mesh.GetField("elem_mask");  

  // Get dimensions
  int sdim=mesh.spatial_dim();
  int pdim=mesh.parametric_dim();
     
  // Compute area depending on dimensions
  if (pdim==2) {
    if (sdim==2) {
      MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        // Get the element
        const MeshObj &elem = *ei; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;
 
        // Skip masked elements
        if (mptr != NULL) {
          double *m=mptr->data(elem);
          if (*m > 0.5) continue;
        }
        
        // Init. Degenerate
        bool is_degenerate=false;

        // Get the coords
        get_elem_coords(&elem, cfield, 2, MAX_NUM_POLY_NODES_2D, &num_poly_nodes, poly_coords);

        // Save original coords
        std::copy(poly_coords,poly_coords+2*num_poly_nodes,poly_coords_orig);
        num_poly_nodes_orig=num_poly_nodes;
        
        // Get rid of 0 len edges
        remove_0len_edges2D(&num_poly_nodes, poly_coords);

         // If less than 3 nodes then is degenerate
        if (num_poly_nodes <3) is_degenerate=true;

        // If is smashed quad then is degenerate
        if (is_smashed_quad2D(num_poly_nodes, poly_coords)) is_degenerate=true;

        // Check if degenerate
        if (is_degenerate) {
             char msg[1024];
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~ Degenerate Element Detected ~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);
            sprintf(msg,"  degenerate elem. id=%ld",elem.get_id());
             ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  ",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  degenerate elem. coords ",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  --------------------------------------------------------- ",ESMC_LOGMSG_ERROR);
            for(int i=0; i< num_poly_nodes_orig; i++) {
              double *pnt=poly_coords_orig+2*i;
              
              sprintf(msg,"    %d  (%f,  %f) ",i,pnt[0],pnt[1]);
              ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            }
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);

            return true;
        }     
      }
    } else if (sdim==3) {
      MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        // Get the element
        const MeshObj &elem = *ei; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Skip masked elements
        if (mptr != NULL) {
          double *m=mptr->data(elem);
          if (*m > 0.5) continue;
        }


        // Init. Degenerate
        bool is_degenerate=false;

        // Get the coords
        get_elem_coords(&elem, cfield, 3, MAX_NUM_POLY_NODES_3D, &num_poly_nodes, poly_coords);

         // Save original coords
        std::copy(poly_coords,poly_coords+3*num_poly_nodes,poly_coords_orig);
        num_poly_nodes_orig=num_poly_nodes;


        // Get rid of 0 len edges
        remove_0len_edges3D(&num_poly_nodes, poly_coords);


        // If less than 3 nodes then is degenerate
        if (num_poly_nodes <3) is_degenerate=true;


         // If is smashed quad then is degenerate
        if (is_smashed_quad3D(num_poly_nodes, poly_coords)) is_degenerate=true;


        // Check if degenerate
        if (is_degenerate) {
            char msg[1024];
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~~~~ Degenerate Element Detected ~~~~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);
            sprintf(msg,"  degenerate elem. id=%ld",elem.get_id());
            ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  ",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  degenerate elem. coords (lon [-180 to 180], lat [-90 to 90]) (x,y,z)",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  ----------------------------------------------------------------- ",ESMC_LOGMSG_ERROR);
            for(int i=0; i< num_poly_nodes_orig; i++) {
              double *pnt=poly_coords_orig+3*i;
              
              double lon, lat, r;
               convert_cart_to_sph_deg(pnt[0], pnt[1], pnt[2],
                                       &lon, &lat, &r);

              sprintf(msg,"    %d  (%f,  %f)  (%f, %f, %f)",i,lon,lat,pnt[0],pnt[1],pnt[2]);
              ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            }
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);

            return true;
        }
      }
    }
  }

  // TODO: Check to see if 3D elements are in correct order.

  // output no degenerate elems found
  return false;
}

// OLD VERSION, BUT NOW WE SUPPORT clockwise and concave
#undef  ESMC_METHOD
#define ESMC_METHOD "cnsrv_check_for_mesh_errors()" 
static void cnsrv_check_for_mesh_errors(Mesh &mesh, bool ignore_degenerate, bool *concave, bool *clockwise, bool *degenerate) {
  
  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];

  int num_poly_nodes_orig;
  double poly_coords_orig[MAX_NUM_POLY_COORDS];
   
  // Init variables
  *concave=false;
  *clockwise=false;
  *degenerate=false;

  // Get coord field
  MEField<> *cfield = mesh.GetCoordField();

  // Get mask Field
  MEField<> *mptr = mesh.GetField("elem_mask");  

  // Get dimensions
  int sdim=mesh.spatial_dim();
   int pdim=mesh.parametric_dim();
     
  // Compute area depending on dimensions
  if (pdim==2) {
    if (sdim==2) {
      MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        // Get the element
        const MeshObj &elem = *ei; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;
 
        // Skip masked elements
        if (mptr != NULL) {
          double *m=mptr->data(elem);
          if (*m > 0.5) continue;
        }
        
        // Init. Degenerate
        bool is_degenerate=false;

        // Get the coords
        get_elem_coords(&elem, cfield, 2, MAX_NUM_POLY_NODES_2D, &num_poly_nodes, poly_coords);

        // Save original coords
        std::copy(poly_coords,poly_coords+2*num_poly_nodes,poly_coords_orig);
        num_poly_nodes_orig=num_poly_nodes;
        
        // Get rid of 0 len edges
        remove_0len_edges2D(&num_poly_nodes, poly_coords);

         // If less than 3 nodes then is degenerate
        if (num_poly_nodes <3) is_degenerate=true;

        // If is smashed quad then is degenerate
        if (is_smashed_quad2D(num_poly_nodes, poly_coords)) is_degenerate=true;

        // Check if degenerate
        if (is_degenerate) {
          if (ignore_degenerate) {
            continue;
          } else {
             char msg[1024];
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~ Degenerate Element Detected ~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);
            sprintf(msg,"  degenerate elem. id=%ld",elem.get_id());
             ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  ",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  degenerate elem. coords ",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  --------------------------------------------------------- ",ESMC_LOGMSG_ERROR);
            for(int i=0; i< num_poly_nodes_orig; i++) {
              double *pnt=poly_coords_orig+2*i;
              
              sprintf(msg,"    %d  (%f,  %f) ",i,pnt[0],pnt[1]);
              ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            }
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);

            *degenerate=true;
            return;
          }
        }
     
        //// WE CAN NOW HANDLE CONCAVE CELLS, SO DON'T CHECK THIS ////
#if 0
        // Get elem rotation
        bool left_turn;
        bool right_turn;
        rot_2D_2D_cart(num_poly_nodes, poly_coords, &left_turn, &right_turn);
        
        // Look for errors
        if (right_turn) {
          if (left_turn) { 
            char msg[1024];
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~ Concave Element Detected ~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);
            sprintf(msg,"  concave elem. id=%ld",elem.get_id());
            ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  ",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  concave elem. coords ",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  --------------------------------------------------------- ",ESMC_LOGMSG_ERROR);
            for(int i=0; i< num_poly_nodes_orig; i++) {
               double *pnt=poly_coords_orig+2*i;
              
              sprintf(msg,"    %d  (%f,  %f) ",i,pnt[0],pnt[1]);
               ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            }
             ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);

            *concave=true;
            return;
          }
        }
#endif
      }
    } else if (sdim==3) {
      MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        // Get the element
        const MeshObj &elem = *ei; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Skip masked elements
        if (mptr != NULL) {
          double *m=mptr->data(elem);
          if (*m > 0.5) continue;
        }


        // Init. Degenerate
        bool is_degenerate=false;

        // Get the coords
        get_elem_coords(&elem, cfield, 3, MAX_NUM_POLY_NODES_3D, &num_poly_nodes, poly_coords);

         // Save original coords
        std::copy(poly_coords,poly_coords+3*num_poly_nodes,poly_coords_orig);
        num_poly_nodes_orig=num_poly_nodes;


        // Get rid of 0 len edges
        remove_0len_edges3D(&num_poly_nodes, poly_coords);


        // If less than 3 nodes then is degenerate
        if (num_poly_nodes <3) is_degenerate=true;


         // If is smashed quad then is degenerate
        if (is_smashed_quad3D(num_poly_nodes, poly_coords)) is_degenerate=true;


        // Check if degenerate
        if (is_degenerate) {
           if (ignore_degenerate) {
            continue;
          } else {
            char msg[1024];
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~~~~ Degenerate Element Detected ~~~~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);
            sprintf(msg,"  degenerate elem. id=%ld",elem.get_id());
            ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  ",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  degenerate elem. coords (lon [-180 to 180], lat [-90 to 90]) (x,y,z)",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  ----------------------------------------------------------------- ",ESMC_LOGMSG_ERROR);
            for(int i=0; i< num_poly_nodes_orig; i++) {
              double *pnt=poly_coords_orig+3*i;
              
              double lon, lat, r;
               convert_cart_to_sph_deg(pnt[0], pnt[1], pnt[2],
                                       &lon, &lat, &r);

              sprintf(msg,"    %d  (%f,  %f)  (%f, %f, %f)",i,lon,lat,pnt[0],pnt[1],pnt[2]);
              ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            }
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);

            *degenerate=true;
            return;
          }
        } 

        //// WE CAN NOW HANDLE CONCAVE CELLS, SO DON'T CHECK THIS ////
#if 0
        // Get elem rotation
        bool left_turn;
        bool right_turn;
        rot_2D_3D_sph(num_poly_nodes, poly_coords, &left_turn, &right_turn);
        
        // Look for errors
        if (right_turn) {
          if (left_turn) { 
            char msg[1024];
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~~~~ Concave Element Detected ~~~~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);
            sprintf(msg,"  concave elem. id=%ld",elem.get_id());
            ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  ",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  concave elem. coords (lon [-180 to 180], lat [-90 to 90]) (x,y,z)",ESMC_LOGMSG_ERROR);
            ESMC_LogDefault.Write("  ----------------------------------------------------------------- ",ESMC_LOGMSG_ERROR);
            for(int i=0; i< num_poly_nodes_orig; i++) {
              double *pnt=poly_coords_orig+3*i;
               
              double lon, lat, r;
              convert_cart_to_sph_deg(pnt[0], pnt[1], pnt[2],
                                      &lon, &lat, &r);

              sprintf(msg,"    %d  (%f,  %f)  (%f, %f, %f)",i,lon,lat,pnt[0],pnt[1],pnt[2]);
               ESMC_LogDefault.Write(msg,ESMC_LOGMSG_ERROR);
            }
            ESMC_LogDefault.Write("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",ESMC_LOGMSG_ERROR);

            *concave=true;
            return;
          } 
        }
#endif
      }
    }
  }


  // TODO: Check to see if 3D elements are in correct order.

}

#if 0
static void noncnsrv_check_for_mesh_errors(Mesh *mesh, bool ignore_degenerate, bool *concave, bool *clockwise, bool *degenerate) {
  
  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];
  
  // Init variables
  *concave=false;
  *clockwise=false;
  *degenerate=false;

  // Get coord field
  MEField<> *cfield = mesh->GetCoordField();

  // Get mask Field
  MEField<> *mptr = mesh->GetField("mask");  

  // Get dimensions
  int sdim=mesh->spatial_dim();
  int pdim=mesh->parametric_dim();
    
  // Compute area depending on dimensions
  if (pdim==2) {
    if (sdim==2) {
      MeshDB::const_iterator ei = mesh->elem_begin(), ee = mesh->elem_end();
      for (; ei != ee; ++ei) {
        // Get the element
        const MeshObj &elem = *ei; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;

       
        /// NEED TO CHANGE THIS TO WORK WITH ELEMENT NODES (OR MAKE A FUNCTION?)
        // Skip masked elements
        if (mptr != NULL) {
          double *m=mptr->data(elem);
          if (*m > 0.5) continue;
        }
        
        // Get the coords
        get_elem_coords(&elem, cfield, 2, MAX_NUM_POLY_NODES_2D, &num_poly_nodes, poly_coords);
        
        // Get rid of 0 len edges
        remove_0len_edges2D(&num_poly_nodes, poly_coords);

        // Check if degenerate
        if (num_poly_nodes <3) {
          if (ignore_degenerate) {
            continue;
          } else {
            *degenerate=true;
            return;
          }
        }
        
        // Get elem rotation
        bool left_turn;
        bool right_turn;
        rot_2D_2D_cart(num_poly_nodes, poly_coords, &left_turn, &right_turn);
        
        // Look for errors
        if (right_turn) {
          if (left_turn) { 
            *concave=true;
            return;
          } else {
            *clockwise=true;
            return;
          }
        }
      }
    } else if (sdim==3) {
      MeshDB::const_iterator ei = mesh->elem_begin(), ee = mesh->elem_end();
      for (; ei != ee; ++ei) {
        // Get the element
        const MeshObj &elem = *ei; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;

       /// NEED TO CHANGE THIS TO WORK WITH ELEMENT NODES (OR MAKE A FUNCTION?)        
        // Skip masked elements
        if (mptr != NULL) {
          double *m=mptr->data(elem);
          if (*m > 0.5) continue;
        }

        // Get the coords
        get_elem_coords(&elem, cfield, 3, MAX_NUM_POLY_NODES_3D, &num_poly_nodes, poly_coords);

        // Get rid of 0 len edges
        remove_0len_edges3D(&num_poly_nodes, poly_coords);

        // Check if degenerate
        if (num_poly_nodes <3) {
          if (ignore_degenerate) {
            continue;
          } else {
            *degenerate=true;
            return;
          }
        } 
       
        // Get elem rotation
        bool left_turn;
        bool right_turn;
        rot_2D_3D_sph(num_poly_nodes, poly_coords, &left_turn, &right_turn);
        
        // Look for errors
        if (right_turn) {
          if (left_turn) { 
            *concave=true;
            return;
          } else {
            *clockwise=true;
            printf(" clockwise element=%d\n",elem.get_id());
            printf(" num nodes=%d\n",num_poly_nodes);
            write_3D_poly_woid_to_vtk("clockwise", num_poly_nodes, poly_coords); 
           return;
          }
        }
      }
    }
  }

  // TODO: Check to see if 3D elements are in correct order.

}

#endif

static void translate_split_src_elems_in_wts(Mesh *srcmesh, int num_entries,
                                      int *iientries) {


  // Get a list of split ids that we own
  UInt num_gids=0;
  UInt *gids_split=NULL;
  UInt *gids_orig=NULL;

  // Get number of split points
  num_gids=srcmesh->split_to_orig_id.size();

  // Allocate space
  if (num_gids>0) {
    gids_split= new UInt[num_gids];
    gids_orig= new UInt[num_gids];
    
    // Loop and get split-orig id pairs
    std::map<UInt,UInt>::iterator mi=srcmesh->split_to_orig_id.begin();
    std::map<UInt,UInt>::iterator me=srcmesh->split_to_orig_id.end();
    
    int pos=0;
    for ( ; mi != me; mi++) {
      gids_split[pos]=mi->first;
      gids_orig[pos]=mi->second;
      pos++;
    }
    
    //    for (int i=0; i<num_gids; i++) {
    //  printf("%d# s=%d o=%d\n",Par::Rank(),gids_split[i],gids_orig[i]);
    //}
  }
  
  // Put into DDir
  DDir<> id_map_dir;
  id_map_dir.Create(num_gids,gids_split,gids_orig);

  // Clean up 
  if (num_gids>0) {
    if (gids_split!= NULL) delete [] gids_split;
    if (gids_orig != NULL) delete [] gids_orig;
  }


  // Gather list of spit src ids
  //// TODO: Maybe use a std::set instead to reduce the amount of communication??
  std::vector<UInt> src_split_gids;
  std::vector<int> src_split_gids_idx;

  // Loop through weights modifying split dst elements
  for (int i=0; i<num_entries; i++) {

      // Get src id 
     UInt src_id=iientries[2*i];

     // If a split id then add to list
     if (src_id > srcmesh->max_non_split_id) {
       src_split_gids.push_back(src_id);
       src_split_gids_idx.push_back(2*i);
     }      

  }

  // Do remote lookup to translate
  UInt num_src_split_gids=src_split_gids.size();
  UInt *src_split_gids_proc=NULL;
  UInt *src_split_gids_orig=NULL;

  if (num_src_split_gids > 0) {
    src_split_gids_proc = new UInt[num_src_split_gids];
    src_split_gids_orig = new UInt[num_src_split_gids];    
  }
  
  // Get mapping of split ids to original ids
  id_map_dir.RemoteGID(num_src_split_gids, &src_split_gids[0], src_split_gids_proc, src_split_gids_orig);

  // Loop setting new ids
  for (int i=0; i<num_src_split_gids; i++) {
    iientries[src_split_gids_idx[i]]=src_split_gids_orig[i];
  }
  
  // Clean up
  if (num_src_split_gids > 0) {
    if (src_split_gids_proc != NULL) delete [] src_split_gids_proc;
    if (src_split_gids_orig != NULL) delete [] src_split_gids_orig;
  }
}



static void translate_split_dst_elems_in_wts(Mesh *dstmesh, int num_entries,
                                      int *iientries, double *factors) {

  // Loop through weights modifying split dst elements
  for (int i=0; i<num_entries; i++) {
    int dst_id=iientries[2*i+1];

    // See if the element is part of a larger polygon
    std::map<UInt,double>::iterator mi =  dstmesh->split_id_to_frac.find(dst_id);

    // It is part of a larger polygon, so process
    if (mi != dstmesh->split_id_to_frac.end()) {

      // Modify weight by fraction of orig polygon
      factors[i] *= mi->second;        

      // See if the id needs to be translated, if so then translate
      std::map<UInt,UInt>::iterator soi =  dstmesh->split_to_orig_id.find(dst_id);
      if (soi != dstmesh->split_to_orig_id.end()) {
        iientries[2*i+1]=soi->second;
      }
    }
  }

}

static void change_wts_to_be_fracarea(Mesh *mesh, int num_entries,
                               int *iientries, double *factors) {


  // Get frac field
  MEField<> *elem_frac = mesh->GetField("elem_frac");
  if (!elem_frac) Throw() << "Trying to get elem_frac when it doesn't exist";

  // Process differently if mesh is split
  if (!mesh->is_split) {
    // Loop through weights dividing by dst_fraction
    for (int i=0; i<num_entries; i++) {
      int dst_id=iientries[2*i+1];
      
      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh->map_find(MeshObj::ELEMENT, dst_id);
      if (mi == mesh->map_end(MeshObj::ELEMENT)) {
        Throw() << " destination id not found in destination mesh.";
      }
      
      // Get the element
      const MeshObj &elem = *mi; 
      
      // Get frac data
      double *f=elem_frac->data(elem);
      double frac=*f;
      
      // If not 0.0 divide
      if (frac != 0.0) {
        factors[i] = factors[i]/frac;
      }
    }
  } else {
    // get map of elem ids to fraction
    std::map<int,double> id_to_frac;

    // Iterate through elements constucting id_to_frac list
    Mesh::iterator ei = mesh->elem_begin(), ee = mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
      
      // Don't do non-local elements
      if (!GetAttr(elem).is_locally_owned()) continue;
        
      // Get the element id
      int eid=elem.get_id();
        
      // Get frac data
      double *f=elem_frac->data(elem);
      double frac=*f;

      // See if the element is part of a larger polygon
      std::map<UInt,double>::iterator mi =  mesh->split_id_to_frac.find(eid);
        
      // Not part of something larger, so just stick in map
      if (mi == mesh->split_id_to_frac.end()) {
        id_to_frac[eid] = frac;
        continue;
      } 
        
      // It is part of original poly, so modify by fraction 
      frac *= mi->second;

      // Translate id if necessary
      int orig_id;
      std::map<UInt,UInt>::iterator soi =  mesh->split_to_orig_id.find(eid);
      if (soi == mesh->split_to_orig_id.end()) {
        orig_id=eid;
      } else {
        orig_id=soi->second;
      }

      // either put this in or add it depending if id is in map
      std::map<int,double>::iterator ifi =  id_to_frac.find(orig_id);
      if (ifi == id_to_frac.end()) {
        id_to_frac[orig_id]=frac;
      } else {
        ifi->second += frac;
      }
    }

    // Loop through weights dividing by dst_fraction
    for (int i=0; i<num_entries; i++) {
      int dst_id=iientries[2*i+1];
      
      //  Find the corresponding fraction
      std::map<int,double>::iterator ifi =  id_to_frac.find(dst_id);
      if (ifi == id_to_frac.end()) {
        Throw() << " destination id not found in id_to_frac map.";
      }

      // Get fraction
      double frac=ifi->second;

      // If not 0.0 divide
      if (frac != 0.0) {
        factors[i] = factors[i]/frac;
      }
    }
  }
}

  // Only works for scalar data right now, but would be pretty easy to add more dimensions 
void CpMeshDataToArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray) {
#undef  ESMC_METHOD
#define ESMC_METHOD "CpMeshDataToArray()" 
  Trace __trace("CpMeshDataToArray()");

 int localrc;
 int rc;

  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception

 bool is_sphere = grid.isSphere();

 // Loop nodes of the grid.  Here we loop all nodes, both owned and not.
   ESMCI::GridIter *gni=new ESMCI::GridIter(&grid,staggerLoc,true);

   // loop through all nodes in the Grid
   for(gni->toBeg(); !gni->isDone(); gni->adv()) {   
     if(!gni->isLocal()) continue;

       // get the global id of this Grid node
       int gid=gni->getGlobalID(); 

       //  Find the corresponding Mesh node
       Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::NODE, gid);
       if (mi == mesh.map_end(MeshObj::NODE)) {
	 Throw() << "Grid entry not in mesh";
       }

       // Get the node
	const MeshObj &node = *mi; 

       // Get the data 
	double *data = dataToArray->data(node);

       // Put it into the Array
      gni->setArrayData(&array, *data);
   }


   // delete Grid Iters
   delete gni;

}
#undef  ESMC_METHOD


  // Assumes array is on center staggerloc of grid
  void CpMeshElemDataToArray(Grid &grid, int staggerloc, ESMCI::Mesh &mesh, ESMCI::Array &array, MEField<> *dataToArray) {
#undef  ESMC_METHOD
#define ESMC_METHOD "CpMeshElemDataToArray()" 
  Trace __trace("CpMeshElemDataToArray()");

 int localrc;
 int rc;


  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception


    // Loop elemets of the grid.  Here we loop all elements, both owned and not.
    ESMCI::GridCellIter *gci=new ESMCI::GridCellIter(&grid,staggerloc);
    
    // loop through all nodes in the Grid
    for(gci->toBeg(); !gci->isDone(); gci->adv()) {   
      
      // get the global id of this Grid node
      int gid=gci->getGlobalID(); 
      
      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, gid);
      if (mi == mesh.map_end(MeshObj::ELEMENT)) {
	Throw() << "Grid entry not in mesh";
      }
      
      // Get the element
      const MeshObj &elem = *mi; 
      
      // Only put it in if it's locally owned
      if (!GetAttr(elem).is_locally_owned()) continue;


       // Get the data 
	double *data = dataToArray->data(elem);

        // DEBUG:  printf("G2M %d %f \n",gid,*data);

       // Put it into the Array
      gci->setArrayData(&array, *data);
   }

   // delete Grid Iters
   delete gci;
}


  void PutElemAreaIntoArray(Grid &grid, int staggerLoc, ESMCI::Mesh &mesh, ESMCI::Array &array) {
#undef  ESMC_METHOD
#define ESMC_METHOD "CpMeshElemDataToArray()" 
    Trace __trace("CpMeshElemDataToArray()");
    
    int localrc;
    int rc;

#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
    
    int num_poly_nodes;
    double poly_coords[MAX_NUM_POLY_COORDS];
    double tmp_coords[MAX_NUM_POLY_COORDS];


    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception


    // Setup interator to Loop elemets of the grid.  Here we loop all elements, both owned and not.
    ESMCI::GridCellIter *gci=new ESMCI::GridCellIter(&grid,staggerLoc);


    // If an area field exists use that instead
    // TODO: replace this with something that doesn't require building a mesh first
    MEField<> *area_field = mesh.GetField("elem_area");
    if (area_field) {
    
      // loop through all nodes in the Grid
      for(gci->toBeg(); !gci->isDone(); gci->adv()) {   
      
        // get the global id of this Grid node
        int gid=gci->getGlobalID(); 
      
        //  Find the corresponding Mesh element
        Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, gid);
        if (mi == mesh.map_end(MeshObj::ELEMENT)) {
          Throw() << "Grid entry not in mesh";
        }
      
        // Get the element
        const MeshObj &elem = *mi; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Get area from field
        double *area=area_field->data(elem);
      
        // Put it into the Array
        gci->setArrayData(&array, *area);
      }

      return;
    }


    ////// Otherwise calculate areas..... 
    
    // Get coord field
    MEField<> *cfield = mesh.GetCoordField();

    // Get dimensions
    int sdim=mesh.spatial_dim();
    int pdim=mesh.parametric_dim();
    
    // loop through all nodes in the Grid
    for(gci->toBeg(); !gci->isDone(); gci->adv()) {   
      
      // get the global id of this Grid node
      int gid=gci->getGlobalID(); 
      
      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, gid);
      if (mi == mesh.map_end(MeshObj::ELEMENT)) {
	Throw() << "Grid entry not in mesh";
      }
      
      // Get the element
      const MeshObj &elem = *mi; 
      
      // Only put it in if it's locally owned
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Get area depending on dimensions
      double area;
      
      if (pdim==2) {
	if (sdim==2) {
          get_elem_coords_2D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_2D, tmp_coords, &num_poly_nodes, poly_coords);
	  remove_0len_edges2D(&num_poly_nodes, poly_coords);
          area=area_of_flat_2D_polygon(num_poly_nodes, poly_coords);
	} else if (sdim==3) {
          get_elem_coords_3D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_3D, tmp_coords, &num_poly_nodes, poly_coords);
	  remove_0len_edges3D(&num_poly_nodes, poly_coords);
	  area=great_circle_area(num_poly_nodes, poly_coords);
	}
      } else if (pdim==3) {
	if (sdim==3) {
          Phedra tmp_phedra=create_phedra_from_elem(&elem, cfield);
          area=tmp_phedra.calc_volume(); 
        } else {
          Throw() << "Meshes with parametric dimension == 3, but spatial dim != 3 not supported for computing areas";
        }
      } else {
	Throw() << "Meshes with parametric dimension != 2 or 3 not supported for computing areas";
      }

      
       // Put it into the Array
      gci->setArrayData(&array, area);
   }

   // delete Grid Iters
   delete gci;

}


// mesh set fraction
void ESMCI_meshsetfraction(Mesh **meshpp, double * fraction, 
                   int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshsetfraction()" 
  Trace __trace(" FTN(meshsetfraction) ");

  Mesh &mesh = **meshpp;
 
  try {
    MeshSetFraction(mesh, *fraction);

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
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }
  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

// Assumes array is center stagger loc
void ESMCI_xgrid_getfrac(Grid **gridpp,
                   Mesh **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getfrac()" 
  Trace __trace(" FTN_X(regrid_getfrac)()");

  ESMCI::Array &array = **arraypp;
  Mesh &mesh = **meshpp;
  Grid &grid = **gridpp;

  try {


    // Get the integration weights
    MEField<> *frac = mesh.GetField("elem_frac");
    if (!frac) Throw() << "Could not find elem_frac field on this mesh"
                             <<std::endl; 

    CpMeshElemDataToArray(grid, *staggerLoc, mesh, array, frac);
 
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
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

////////////////////////////////////////////////////////////////////////////
// Assumes array is center stagger loc
void ESMCI_xgrid_getfrac2(Grid **gridpp,
                   Mesh **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getfrac2()" 
  Trace __trace(" FTN_X(regrid_getfrac2)()");

  ESMCI::Array &array = **arraypp;
  Mesh &mesh = **meshpp;
  Grid &grid = **gridpp;

  try {


    // Get the integration weights
    MEField<> *frac = mesh.GetField("elem_frac2");
    if (!frac) Throw() << "Could not find elem_frac2 field on this mesh"
                             <<std::endl; 

    CpMeshElemDataToArray(grid, *staggerLoc, mesh, array, frac);
 
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
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

#undef  ESMC_METHOD
