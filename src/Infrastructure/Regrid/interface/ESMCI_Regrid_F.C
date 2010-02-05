// $Id: ESMCI_Regrid_F.C,v 1.40.2.1 2010/02/05 19:59:55 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_Grid.h"
#include "GridUtil/include/ESMC_GridToMesh.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MeshRead.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_Extrapolation.h>

#include <iostream>

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
//
//EOP
//-------------------------------------------------------------------------


using namespace ESMCI;

enum {ESMF_REGRID_SCHEME_FULL3D = 0, ESMF_REGRID_SCHEME_NATIVE = 1};
enum {ESMF_REGRID_METHOD_BILINEAR = 0, ESMF_REGRID_METHOD_PATCH = 1};
enum {ESMF_REGRID_MASSCONSERVE_OFF = 0, ESMF_REGRID_MASSCONSERVE_ON = 1};

namespace ESMCI {
  struct TempWeights {
    int nentries;
    int *iientries;
    double *factors;
  };
}



extern "C" void FTN(c_esmc_arraysmmstore)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle,
    ESMC_TypeKind *typekind, void *factorList, int *factorListCount,
    ESMCI::InterfaceInt **factorIndexList, int *rc);

extern "C" void FTN(c_esmc_regrid_create)(ESMCI::VM **vmpp,
                   Mesh **meshsrcpp, ESMCI::Array **arraysrcpp, int *srcstaggerLoc,
                   Mesh **meshdstpp, ESMCI::Array **arraydstpp, int *dststaggerLoc,
		   int *regridMethod, int *regridMassConserve, 
                   int *regridScheme, int *unmappedaction,
                   ESMCI::RouteHandle **rh, int *has_rh, int *has_iw,
                   int *nentries, ESMCI::TempWeights **tweights,
                             int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_create()" 
  Trace __trace(" FTN(regrid_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridsrcpp, int *srcstaggerLoc, ESMCI::Grid **griddstcpp, int *dststaggerLoc, int*rc");
  ESMCI::VM *vm = *vmpp;
  ESMCI::Array &srcarray = **arraysrcpp;
  ESMCI::Array &dstarray = **arraydstpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  Mesh &srcmesh = **meshsrcpp;
  Mesh &dstmesh = **meshdstpp;

  try {

/*
    if (*regridScheme == ESMF_REGRID_SCHEME_FULL3D) {
      srcgrid.setSphere();
      dstgrid.setSphere();
      //std::cout << "Regrid setting sphere!!" << std::endl;
    }
*/

    // If a spherical grid, mesh the poles, if they exist
    IWeights pole_constraints;
    if (*regridScheme == ESMF_REGRID_SCHEME_FULL3D) {
      UInt constraint_id = srcmesh.DefineContext("pole_constraints");

      // Why 7?  Ask Bob.
      for (UInt i = 1; i <= 7; ++i) {
        MeshAddPole(srcmesh, i, constraint_id, pole_constraints);
      }
    }

    // Get coordinate fields
    MEField<> &scoord = *srcmesh.GetCoordField();
    MEField<> &dcoord = *dstmesh.GetCoordField();


    // Create a layer of ghost elements since the patch method needs
    // a larger stencil.
    if (*regridMethod == ESMF_REGRID_METHOD_PATCH) {
      int num_snd=0;
      MEField<> *snd[2],*rcv[2];

      // Load coord field
      MEField<> *psc = &scoord;
      snd[num_snd]=psc;
      rcv[num_snd]=psc;
      num_snd++;

      // Load mask field
      MEField<> *psm = srcmesh.GetField("mask");
      if (psm != NULL) {
	snd[num_snd]=psm;
	rcv[num_snd]=psm;
	num_snd++;
      }

      srcmesh.CreateGhost();
      srcmesh.GhostComm().SendFields(num_snd, snd, rcv);
    }

/*
    WriteMesh(srcmesh, "src_grid");
    WriteMesh(dstmesh, "dst_grid");
*/

    // Now we have the meshes, so go ahead and calc bilinear weights.
    // Since bilin needs a field, just do coords.
    // bilinear is bilinear.
    std::vector<Interp::FieldPair> fpairs;
    
    switch (*regridMethod) {

      case ESMF_REGRID_METHOD_BILINEAR:
        fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_STD));
      break;

      case ESMF_REGRID_METHOD_PATCH:
        fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_PATCH));
      break;

      default:
        Throw() << "Regrid method:" << *regridMethod << " is not implemented";
    }

    Interp interp(srcmesh, dstmesh, fpairs, *unmappedaction);

    IWeights wts;
 
    interp(0, wts);

    // Remove pole constraint by condensing out
    if (*regridScheme == ESMF_REGRID_SCHEME_FULL3D) {

      // Get the pole matrix on the right processors
      wts.GatherToCol(pole_constraints);

      // Take pole constraint out of matrix
      wts.AssimilateConstraints(pole_constraints);

    }

    // Remove non-locally owned weights (assuming destination mesh decomposition)
    wts.Prune(dstmesh, 0);

    //wts.Print(Par::Out());

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

    // Gather the data
    WMat::WeightMap::iterator wi = wts.begin_row(), we = wts.end_row();

    UInt i = 0;
    for (; wi != we; ++wi) {
      const WMat::Entry &w = wi->first;


      std::vector<WMat::Entry> &wcol = wi->second;

      for (UInt j = 0; j < wcol.size(); ++j) {

        UInt twoi = 2*i;

        const WMat::Entry &wc = wcol[j];

        // I expected things to be (dst,src), but they are (src_id,dst_id)
        //iientries[twoi] = w.id;  iientries[twoi+1] = wc.id;
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
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
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
      *tweights = new ESMCI::TempWeights;
      (*tweights)->nentries = num_entries;
      (*tweights)->factors = factors;
      (*tweights)->iientries = iientries;
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN(c_esmc_regrid_create_mesh)(ESMCI::VM **vmpp,
                   Mesh **gridsrcpp, ESMCI::Array **arraysrcpp, int *srcstaggerLoc,
                   Mesh **griddstpp, ESMCI::Array **arraydstpp, int *dststaggerLoc,
                   int *regridMethod, int *regridScheme,
                   ESMCI::RouteHandle **rh, int *has_rh, int *has_iw,
                   int *nentries, ESMCI::TempWeights **tweights,
                             int*rc) {
  Trace __trace(" FTN(regrid_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridsrcpp, int *srcstaggerLoc, ESMCI::Grid **griddstcpp, int *dststaggerLoc, int*rc");
  ESMCI::VM *vm = *vmpp;
  Mesh &srcmesh = **gridsrcpp;
  Mesh &dstmesh = **griddstpp;
  ESMCI::Array &srcarray = **arraysrcpp;
  ESMCI::Array &dstarray = **arraydstpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  try {

    // If a spherical grid, mesh the poles, if they exist
    IWeights pole_constraints;
    if (*regridScheme == ESMF_REGRID_SCHEME_FULL3D) {

      UInt constraint_id = srcmesh.DefineContext("pole_constraints");

      // Why 7?  Ask Bob.
      for (UInt i = 1; i <= 7; ++i) {
        MeshAddPole(srcmesh, i, constraint_id, pole_constraints);
      }

    }


    MEField<> &scoord = *srcmesh.GetCoordField();
    MEField<> &dcoord = *dstmesh.GetCoordField();


    // Create a layer of ghost elements since the patch method needs
    // a larger stencil.
    if (*regridMethod == ESMF_REGRID_METHOD_PATCH) {
      MEField<> *psc = &scoord;
      srcmesh.CreateGhost();
      srcmesh.GhostComm().SendFields(1, &psc, &psc);
    }



/*
    WriteMesh(srcmesh, "src_grid");
    WriteMesh(dstmesh, "dst_grid");
*/

    // Now we have the meshes, so go ahead and calc bilinear weights.
    // Since bilin needs a field, just do coords.
    // bilinear is bilinear.
    std::vector<Interp::FieldPair> fpairs;
    
    switch (*regridMethod) {

      case ESMF_REGRID_METHOD_BILINEAR:
        fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_STD));
      break;

      case ESMF_REGRID_METHOD_PATCH:
        fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_PATCH));
      break;

      default:
        Throw() << "Regrid method:" << *regridMethod << " is not implemented";
    }

    Interp interp(srcmesh, dstmesh, fpairs);

    IWeights wts;
 
    interp(0, wts);

    // Remove pole constraint by condensing out
    if (*regridScheme == ESMF_REGRID_SCHEME_FULL3D) {

      // Get the pole matrix on the right processors
      wts.GatherToCol(pole_constraints);

      // Take pole constraint out of matrix
      wts.AssimilateConstraints(pole_constraints);

    }

    // Remove non-locally owned weights (assuming destination mesh decomposition)
    wts.Prune(dstmesh, 0);

    //wts.Print(Par::Out());

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

    // Gather the data
    WMat::WeightMap::iterator wi = wts.begin_row(), we = wts.end_row();

    UInt i = 0;
    for (; wi != we; ++wi) {
      const WMat::Entry &w = wi->first;


      std::vector<WMat::Entry> &wcol = wi->second;

      for (UInt j = 0; j < wcol.size(); ++j) {

        UInt twoi = 2*i;

        const WMat::Entry &wc = wcol[j];

        // I expected things to be (dst,src), but they are (src_id,dst_id)
        //iientries[twoi] = w.id;  iientries[twoi+1] = wc.id;
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
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
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
      *tweights = new ESMCI::TempWeights;
      (*tweights)->nentries = num_entries;
      (*tweights)->factors = factors;
      (*tweights)->iientries = iientries;
    }
    
  }
  catch(std::exception &x) {
    std::cout << "Error!!! Exception, P:" << localPet << ", <" << x.what() << ">" << std::endl;
    *rc = ESMF_FAILURE;
    return;
  }
  catch(...) {
    std::cout << "Error, unknown exception" << std::endl;
    *rc = ESMF_FAILURE;
    return;
  }

  *rc = ESMF_SUCCESS;

}

// Copy the weights stored in the temporary tw into the fortran arrays.  Also,
// delete the temp weights.
extern "C" void FTN(c_esmc_copy_tempweights)(ESMCI::TempWeights **_tw, int *ii, double *w) {

  ESMCI::TempWeights &tw = (**_tw);

  for (int i = 0; i < tw.nentries; ++i) {

    int two_i = i << 1;

    ii[i] = tw.iientries[two_i+0];
    ii[tw.nentries+i] = tw.iientries[two_i+1];
    w[i] = tw.factors[i];

  }

  delete [] tw.factors;
  delete [] tw.iientries;

  delete *_tw;

}

#undef  ESMC_METHOD
