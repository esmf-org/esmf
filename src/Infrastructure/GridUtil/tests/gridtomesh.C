//-----------------------------------------------------------------------------
// $Id: gridtomesh.C,v 1.5 2008/04/05 03:38:29 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// Testing of DistDir object
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
#include "ESMC_GridToMesh.h"
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"
#include <Mesh/include/ESMC_Mesh.h>
#include <Mesh/include/ESMC_MeshRead.h>
#include <Mesh/include/ESMC_Exception.h>

#include <iostream>


extern "C" void FTN(gridtomesh_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridpp, int *staggerLoc, int*rc) {
  ESMC::Trace __trace("FTN(gridtomesh_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridpp, int *staggerLoc, int*rc)");
  ESMCI::VM *vm = *vmpp;
  ESMCI::Grid &grid = **gridpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  ESMC::Mesh mesh;

  try {

    ESMCI::GridToMesh(grid, *staggerLoc, mesh, std::vector<ESMCI::Array*>());

    WriteMesh(mesh, "bobs_grid");

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

