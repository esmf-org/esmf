//-----------------------------------------------------------------------------
// $Id: gridtomesh.C,v 1.13.2.1 2010/02/05 19:57:53 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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

#include "ESMC_Conf.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MeshRead.h>
#include <Mesh/include/ESMCI_Exception.h>

#include <iostream>


extern "C" void FTN(gridtomesh_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridpp, int *staggerLoc, int*rc) {
  ESMCI::Trace __trace("FTN(gridtomesh_test)(ESMCI::VM **vmpp, ESMCI::Grid **gridpp, int *staggerLoc, int*rc)");
  ESMCI::VM *vm = *vmpp;
  ESMCI::Grid &grid = **gridpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  ESMCI::Mesh mesh;

  try {

    ESMCI::GridToMesh(grid, *staggerLoc, mesh, std::vector<ESMCI::Array*>(),NULL);

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

