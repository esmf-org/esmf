// $Id: ESMCI_GridUtil_F.C,v 1.10 2008/07/21 23:25:51 theurich Exp $
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

#include <iostream>

#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMCI_VM.h"
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
#include <Mesh/include/ESMC_ParEnv.h>


//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
//
//EOP
//-------------------------------------------------------------------------


using namespace ESMC;


extern "C" void FTN(c_esmc_meshio)(ESMCI::VM **vmpp, ESMCI::Grid **gridpp, int *staggerLoc, int *num_arrays,
                    char*name, int *rc,
                             ESMCI::Array **arraypp1,
                             ESMCI::Array **arraypp2,
                             ESMCI::Array **arraypp3,
                             ESMCI::Array **arraypp4,
                             ESMCI::Array **arraypp5,
                             ESMCI::Array **arraypp6,
                             int *spherical,
                             int nlen
                             ) {
  ESMCI::VM *vm = *vmpp;
  ESMCI::Grid &grid = **gridpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  std::vector<ESMCI::Array*> arrays;

  // How to do this?? Any way I can think of is ugly:
  ESMCI::Array **ar[] = {
    arraypp1,
    arraypp2,
    arraypp3,
    arraypp4,
    arraypp5,
    arraypp6
  };


  if (*spherical != 0) grid.setSphere();

  for (UInt i = 0; i < *num_arrays; ++i)
    arrays.push_back(*ar[i]);

  Mesh mesh;

  try {

    ESMCI::GridToMesh(grid, *staggerLoc, mesh, arrays);

    char *meshname = ESMC_F90toCstring(name, nlen);

    WriteMesh(mesh, meshname);

    delete [] meshname;

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
