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
#include "ESMCI_DInfo.h"

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


  ///// THINK ABOUT IF IT MAKES SENSE TO HAVE A FILE PER NEW CLASS TO TEST OR IF IT WOULD BE
  ///// BETTER TO JUST HAVE ONE FILE (E.G. ESMCI_MeshInternalUTest.C TO TEST INTERNAL CLASSES)??
  ///// MAYBE IT MAKES SENSE TO HAVE A MIX DEPENDING ON THE COMPLEXITY OF THE CLASS TO TEST????

  
  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL, (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_LogSet(true);

  
  //NEX_UTest
  strcpy(name, "Create a small DInfo<int,double> object and test commit().");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  // Try catch to catch errors
  try {

    // Create object
    DInfo<int,double> di_test;

    // Add some data depending on PET
    if (localPet == 0) {
      di_test.add(11,0.11);
      di_test.add(12,0.12);
    } else if (localPet == 1) {
      di_test.add(18,0.18);            
      di_test.add(8,0.8);
    } else if (localPet == 2) {
      di_test.add(6,0.6);            
      di_test.add(3,0.3);
    } else if (localPet == 3) {
      di_test.add(2,0.2);
      di_test.add(1,0.1);
    } else {
      
    }

    // Commit
    di_test.commit();

    // Output
    di_test.print_searchable();
    
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
