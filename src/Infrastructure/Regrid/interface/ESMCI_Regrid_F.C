// $Id: ESMCI_Regrid_F.C,v 1.7 2008/04/21 21:40:34 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
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
#include "ESMC_VM.h"
#include "ESMCI_DistGrid.h"
#include "ESMC_RHandle.h"
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_Grid.h"
#include "GridUtil/include/ESMC_GridToMesh.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"
#include "ESMC_F90Interface.h"
#include <Mesh/include/ESMC_Mesh.h>
#include <Mesh/include/ESMC_MeshRead.h>
#include <Mesh/include/ESMC_Exception.h>
#include <Mesh/include/ESMC_Interp.h>

#include <iostream>

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
//
//EOP
//-------------------------------------------------------------------------


using namespace ESMC;

enum {ESMF_REGRID_SCHEME_FULL3D = 0, ESMF_REGRID_SCHEME_NATIVE = 1};



extern "C" void FTN(c_esmc_arraysparsematmulstore)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMC_RouteHandle **routehandle,
    ESMC_TypeKind *typekind, void *factorList, int *factorListCount,
    ESMCI::InterfaceInt **factorIndexList, int *rc);

extern "C" void FTN(c_esmc_regrid_create)(ESMCI::VM **vmpp,
                   ESMCI::Grid **gridsrcpp, ESMCI::Array **arraysrcpp, int *srcstaggerLoc,
                   ESMCI::Grid **griddstpp, ESMCI::Array **arraydstpp, int *dststaggerLoc,
                   int *regridMethod, int *regridScheme,
                   ESMC_RouteHandle **rh,
                             int*rc) {
  Trace __trace(" FTN(regrid_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridsrcpp, int *srcstaggerLoc, ESMCI::Grid **griddstcpp, int *dststaggerLoc, int*rc");
  ESMCI::VM *vm = *vmpp;
  ESMCI::Grid &srcgrid = **gridsrcpp;
  ESMCI::Grid &dstgrid = **griddstpp;
  ESMCI::Array &srcarray = **arraysrcpp;
  ESMCI::Array &dstarray = **arraydstpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  Mesh srcmesh;
  Mesh dstmesh;

  try {


    if (*regridScheme == ESMF_REGRID_SCHEME_FULL3D) {
      srcgrid.setSphere();
      dstgrid.setSphere();
      std::cout << "Regrid setting sphere!!" << std::endl;
    }

    ESMCI::GridToMesh(srcgrid, *srcstaggerLoc, srcmesh, std::vector<ESMCI::Array*>());
    ESMCI::GridToMesh(dstgrid, *dststaggerLoc, dstmesh, std::vector<ESMCI::Array*>());

/*
    WriteMesh(srcmesh, "src_grid");
    WriteMesh(dstmesh, "dst_grid");
*/

    // Now we have the meshes, so go ahead and calc bilinear weights.
    // Since bilin needs a field, just do coords.
    // bilinear is bilinear.
    MEField<> &scoord = *srcmesh.GetCoordField();
    MEField<> &dcoord = *dstmesh.GetCoordField();
    std::vector<Interp::FieldPair> fpairs;
    fpairs.push_back(Interp::FieldPair(&scoord, &dcoord, Interp::INTERP_STD));

    Interp interp(srcmesh, dstmesh, fpairs);

    IWeights wts;
 
    interp(0, wts);

    // Remove non-locally owned weights (assuming destination mesh decomposition)
    wts.Prune(dstmesh, 0);

    wts.Print(Par::Out());

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

Par::Out() << "Matrix entries" << std::endl;
for (UInt n = 0; n < num_entries; ++n) {
  Par::Out() << std::setw(5) << iientries[2*n] << std::setw(5) << iientries[2*n+1] << std::setw(15) << factors[n] << std::endl;
}

    // Build the ArraySMM
    int localrc;
    enum ESMC_TypeKind tk = ESMC_TYPEKIND_R8;
    FTN(c_esmc_arraysparsematmulstore)(arraysrcpp, arraydstpp, rh, &tk, factors,
               &num_entries, &iiptr, &localrc);

    if (localrc != ESMF_SUCCESS) Throw() << "arraysparsematmulstore failed";



    // Clean up
    delete [] factors;
    delete [] iientries;
    
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
