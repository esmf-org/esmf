// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <cmath>

// ESMF header
#include "ESMC.h"

// Other ESMF headers
#include "ESMCI_Pgon.h"

// ESMF Test header
#include "ESMC_Test.h"


using std::abs;

//==============================================================================
//BOP
// !PROGRAM: ESMC_MeshUTest - Check ESMC_Mesh functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  bool correct;

  int num_elem, num_node;
  ESMC_Mesh mesh;
   int pdim=2;
  int sdim=2;

  int localPet, petCount;
  ESMC_VM vm;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL, (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_LogSet(true);

  
  ////// Create a 2D Cart Pgon ///////

  //NEX_UTest
  strcpy(name, "Create a 2D Cartesian Pgon");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  // Try catch to catch errors
  try { 
  
    // Create new Pgon triangle object
    Pgon<GEOM_CART2D> tri;
    tri.push_back_pnt(0.0,0.0);
    tri.push_back_pnt(1.0,0.0);
    tri.push_back_pnt(0.0,1.0);

    // DEBUG OUTPUT: write to file
    tri.write_to_vtk("tri2DCart");
    
  } catch(...) {
    // Change to error if we detect an error
    rc=ESMF_FAILURE;
  }
  
  // Output test info
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  ////// Create a 2D Sph Pgon ///////

  //NEX_UTest
  strcpy(name, "Create a 2D Spherical Pgon");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  // Try catch to catch errors
  try { 
  
    // Create new Pgon triangle object
    Pgon<GEOM_SPH2D3D> tri;
    tri.push_back_pnt(0.0,0.0,0.0);
    tri.push_back_pnt(1.0,0.0,0.0);
    tri.push_back_pnt(0.0,1.0,0.0);

    // DEBUG OUTPUT: write to file
    tri.write_to_vtk("tri2DSph");
    
  } catch(...) {
    // Change to error if we detect an error
    rc=ESMF_FAILURE;
  }
  
  // Output test info
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
