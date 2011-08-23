// $Id: ESMCI_Regrid_F.C,v 1.63.2.1 2011/08/23 21:44:40 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Regrid_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
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

#include <iostream>

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
//
//EOP
//-------------------------------------------------------------------------


using namespace ESMCI;

namespace ESMCI {
  struct TempWeights {
    int nentries;
    int *iientries;
    double *factors;
  };
}


// prototypes from below
bool all_mesh_node_ids_in_wmat(Mesh &mesh, WMat &wts);
bool all_mesh_elem_ids_in_wmat(Mesh &mesh, WMat &wts);
void check_for_concave_or_clkwise(Mesh &mesh, bool *concave, bool *clockwise);

// external C functions
extern "C" void FTN(c_esmc_arraysmmstore)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle,
    ESMC_TypeKind *typekind, void *factorList, int *factorListCount,
    ESMCI::InterfaceInt **factorIndexList, int *rc);

extern "C" void FTN(c_esmc_regrid_create)(ESMCI::VM **vmpp,
                   Mesh **meshsrcpp, ESMCI::Array **arraysrcpp,
                   Mesh **meshdstpp, ESMCI::Array **arraydstpp,
		   int *regridMethod, 
                   int *regridPoleType, int *regridPoleNPnts,  
                   int *regridScheme, int *unmappedaction,
                   ESMCI::RouteHandle **rh, int *has_rh, int *has_iw,
                   int *nentries, ESMCI::TempWeights **tweights,
                   int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_create()" 
  Trace __trace(" FTN(regrid_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridsrcpp, ESMCI::Grid **griddstcpp, int*rc");
  ESMCI::VM *vm = *vmpp;
  ESMCI::Array &srcarray = **arraysrcpp;
  ESMCI::Array &dstarray = **arraydstpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  Mesh &srcmesh = **meshsrcpp;
  Mesh &dstmesh = **meshdstpp;
 
  // Old Regrid conserve turned off for now
  int regridConserve=ESMC_REGRID_CONSERVE_OFF;

  try {

#if 0
    bool concave;
    bool clockwise;

    // Check mesh elements 
    check_for_concave_or_clkwise(srcmesh, &concave, &clockwise);
    
    // Concave
    if (concave) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
         "- Src mesh contains a concave element", &localrc)) throw localrc;
    }

    // Clockwise
    if (clockwise) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
       "- Src mesh contains an element whos nodes are clockwise", &localrc)) throw localrc;
    }

    // Only check dst mesh elements for conservative because for others just nodes are used
    if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
      // Check mesh elements 
      check_for_concave_or_clkwise(dstmesh, &concave, &clockwise);
      
      // Concave
      if (concave) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                "- Dst mesh contains a concave element", &localrc)) throw localrc;
      }
      
      // Clockwise
      if (clockwise) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                "- Dst mesh contains an element whos nodes are clockwise", &localrc)) throw localrc;
      }
    }
#endif

    // Compute Weights matrix
    IWeights wts;
    // Turn off unmapped action checking in regrid because it's local to a proc, and can therefore
    // return false positives for multiproc cases, instead check below after gathering weights to a proc. 
    int temp_unmappedaction=ESMC_UNMAPPEDACTION_IGNORE;

    if(!online_regrid(srcmesh, dstmesh, wts, &regridConserve, regridMethod,
                      regridPoleType, regridPoleNPnts, 
                      regridScheme, &temp_unmappedaction))
      Throw() << "Online regridding error" << std::endl;

    // If user is worried about unmapped points then check that
    // here, because we have all the dest objects and weights
    // gathered onto the same proc.
    if (*unmappedaction==ESMC_UNMAPPEDACTION_ERROR) {
      if (*regridMethod==ESMC_REGRID_METHOD_CONSERVE) {
        if (!all_mesh_elem_ids_in_wmat(dstmesh, wts)) {
          int localrc;
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "- There exist destination cells which don't overlap with any source cell", &localrc)) throw localrc;
        }
      } else { // bilinear, patch, ...
        if (!all_mesh_node_ids_in_wmat(dstmesh, wts)) {
          int localrc;
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                   "- There exist destination points which can't be mapped to any source cell", &localrc)) throw localrc;
        }
      }
    }

    // We have the weights, now set up the sparsemm object

    // Firstly, the index list
    std::pair<UInt,UInt> iisize = wts.count_matrix_entries();
    int num_entries = iisize.first;
    int *iientries = new int[2*iisize.first]; 
    int larg[2] = {2, iisize.first};
    // Gather the list
    ESMCI::InterfaceInt ii(iientries, 2, larg);
    ESMCI::InterfaceInt *iiptr = &ii;

    double *factors = new double[iisize.first];

    // Translate weights to sparse matrix representatio
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

	i++;
      } // for j
    } // for wi
    
/*
Par::Out() << "Matrix entries" << std::endl;
for (UInt n = 0; n < num_entries; ++n) {
  Par::Out() << std::setw(5) << iientries[2*n] << std::setw(5) << iientries[2*n+1] << std::setw(15) << factors[n] << std::endl;
}
wts.Print(Par::Out());
*/

    // Build the ArraySMM
    if (*has_rh != 0) {
      int localrc;
      enum ESMC_TypeKind tk = ESMC_TYPEKIND_R8;
      FTN(c_esmc_arraysmmstore)(arraysrcpp, arraydstpp, rh, &tk, factors,
                 &num_entries, &iiptr, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	throw localrc;  // bail out with exception
    }

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
    
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  } catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN(c_esmc_regrid_getiwts)(ESMCI::VM **vmpp, Grid **gridpp,
                   Mesh **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *regridScheme, int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getiwts()" 
  Trace __trace(" FTN(regrid_getiwts)()");
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  } catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}


extern "C" void FTN(c_esmc_regrid_getarea)(Grid **gridpp,
                   Mesh **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *regridScheme, int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getarea()" 
  Trace __trace(" FTN(regrid_getarea)()");
  ESMCI::Array &array = **arraypp;

  Mesh &mesh = **meshpp;
  Grid &grid = **gridpp;

  try {

    PutElemAreaIntoArray(grid, *staggerLoc, mesh, array);
 
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  } catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}



// Assumes array is center stagger loc
extern "C" void FTN(c_esmc_regrid_getfrac)(Grid **gridpp,
                   Mesh **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getfrac()" 
  Trace __trace(" FTN(regrid_getfrac)()");

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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  } catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}


// Copy the weights stored in the temporary tw into the fortran arrays.  Also,
// delete the temp weights.
extern "C" void FTN(c_esmc_copy_tempweights)(ESMCI::TempWeights **_tw, int *ii, double *w) {

  // See if the TempWeights structure is allocated, if not then just leave
  if (*_tw==NULL) return;

  // Copy Weights
  ESMCI::TempWeights &tw = (**_tw);

  for (int i = 0; i < tw.nentries; ++i) {
    int two_i = i << 1;

    // Reverse order of indices
    //ii[i] = tw.iientries[two_i+0];
    //ii[tw.nentries+i] = tw.iientries[two_i+1];
    ii[two_i+0] = tw.iientries[two_i+0];
    ii[two_i+1] = tw.iientries[two_i+1];

    w[i] = tw.factors[i];
  }

  if (tw.factors != NULL) delete [] tw.factors;
  if (tw.iientries != NULL) delete [] tw.iientries;

  delete *_tw;

}

bool all_mesh_node_ids_in_wmat(Mesh &mesh, WMat &wts) {


  // Get mask Field
  MEField<> *mptr = mesh.GetField("mask");

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Get mesh node iterator that goes through in order of id
  Mesh::MeshObjIDMap::const_iterator ni=mesh.map_begin(MeshObj::NODE), ne=mesh.map_end(MeshObj::NODE);

  // Loop checking that all nodes have weights
  for (; ni != ne; ++ni) {
    const MeshObj &node=*ni;

    // Skip non local nodes
    if (!GetAttr(node).is_locally_owned()) continue;

    // Skip masked elements
    if (mptr != NULL) {
      double *m=mptr->data(node);
      if (*m > 0.5) continue;
    }

    // get node id
    int node_id=node.get_id();

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than node id
    while ((wi != we) && (wi->first.id <node_id)) {
      wi++;
    }

    // If we're at the end of the weights then exit saying we don't have 
    // all of them
    if (wi==we) return false;

    // If we're not equal to the node id then we must have passed it
    if (wi->first.id != node_id) return false;

  }

  // Still here, so must have found them all
  return true;

}



bool all_mesh_elem_ids_in_wmat(Mesh &mesh, WMat &wts) {


  // Get mask Field
  MEField<> *mptr = mesh.GetField("elem_mask");

  // Get weight iterators
  WMat::WeightMap::iterator wi =wts.begin_row(),we = wts.end_row();

  // Get mesh node iterator that goes through in order of id
  Mesh::MeshObjIDMap::const_iterator ei=mesh.map_begin(MeshObj::ELEMENT), ee=mesh.map_end(MeshObj::ELEMENT);

  // Loop checking that all elems have weights
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

    // get weight id
    int wt_id=wi->first.id;

    // Advance weights until not less than elem id
    while ((wi != we) && (wi->first.id <elem_id)) {
      wi++;
    }

    // If we're at the end of the weights then exit saying we don't have 
    // all of them
    if (wi==we) return false;

    // If we're not equal to the elem id then we must have passed it
    if (wi->first.id != elem_id) return false;

  }

  // Still here, so must have found them all
  return true;
}


void check_for_concave_or_clkwise(Mesh &mesh, bool *concave, bool *clockwise) {
  
  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];
  
  // Init variables
  *concave=false;
  *clockwise=false;

  // Get coord field
  MEField<> *cfield = mesh.GetCoordField();
  
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
        
        // Get the coords
        get_elem_coords(&elem, cfield, 2, MAX_NUM_POLY_NODES_2D, &num_poly_nodes, poly_coords);
        
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
      MeshDB::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        // Get the element
        const MeshObj &elem = *ei; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Get the coords
        get_elem_coords(&elem, cfield, 3, MAX_NUM_POLY_NODES_3D, &num_poly_nodes, poly_coords);
        
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
            return;
          }
        }
      }
    }
  }

  // TODO: Check to see if 3D elements are in correct order.

}



#undef  ESMC_METHOD
