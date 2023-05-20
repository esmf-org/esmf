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
#include "ESMCI_LogErr.h"

// ESMF Test header
#include "ESMC_Test.h"

using std::abs;

#define ESMC_METHOD "DInfo Test Code"

// Macro for catch with all the options
#define CATCH_FOR_TESTING(rc) \
  catch(std::exception &x) { \
    if (x.what()) { \
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, \
                                          x.what(), ESMC_CONTEXT,&rc); \
    } else { \
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, \
                                          "UNKNOWN", ESMC_CONTEXT,&rc); \
    }  \
  }catch(int localrc){  \
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,&rc); \
  } catch(...){ \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, \
      "- Caught unknown exception", ESMC_CONTEXT, &rc); \
  }


//==============================================================================
//BOP
// !PROGRAM: ESMC_MeshUTest - Check ESMC_Mesh functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void) {

  char name[1024];
  char failMsg[1024];
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

  
  // Create object to be used for tests below
  DInfo<int,double> di_test;

  
  //NEX_UTest
  strcpy(name, "Create a small DInfo<int,double> object and test commit().");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  // Try-catch to catch errors
  try {

    // Add some data depending on PET
    if (petCount == 1) {
        di_test.add(11,0.11);
        di_test.add(12,0.12);
        di_test.add(18,0.18);            
        di_test.add(8,0.8);
        di_test.add(6,0.6);            
        di_test.add(3,0.3);
        di_test.add(2,0.2);
        di_test.add(1,0.1);
    } else if (petCount == 4) {
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
      } 
    } else {
      Throw() <<"This test only works when run on 1 or 4 PETs.";
    }

    // Commit
    di_test.commit();

    // Debug Output
    //di_test.print_searchable();
    
  }
  CATCH_FOR_TESTING(rc);

  // Output test info
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //NEX_UTest
  strcpy(name, "Test search() of all items in DInfo() object.");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  //// NOTE: Using the Dinfo object from above /////

  // Try-catch to catch errors
  try {
    
    // Make search info depending on number of PETs
    int num_search=0;
    int search_ids[20]; // size bigger than needed below
    double search_info[20]; // size bigger than needed below
    double search_info_check[20]; // size bigger than needed below
    
    if (petCount == 1) {
      search_ids[0]=1; search_info_check[0]=0.1;
      search_ids[1]=12; search_info_check[1]=0.12;
      search_ids[2]=18; search_info_check[2]=0.18;
      search_ids[3]=8; search_info_check[3]=0.8;
      search_ids[4]=6; search_info_check[4]=0.6;
      search_ids[5]=3; search_info_check[5]=0.3;
      search_ids[6]=2; search_info_check[6]=0.2;
      search_ids[7]=11; search_info_check[7]=0.11;
      num_search=8;
    } else if (petCount == 4) {
      if (localPet == 0) {
        search_ids[0]=6; search_info_check[0]=0.6;
        search_ids[1]=3; search_info_check[1]=0.3;
        num_search=2;
      } else if (localPet == 1) {
        search_ids[0]=18; search_info_check[0]=0.18;
        search_ids[1]=8; search_info_check[1]=0.8;
        num_search=2;
      } else if (localPet == 2) {
        search_ids[0]=2; search_info_check[0]=0.2;
        search_ids[1]=1; search_info_check[1]=0.1;
        num_search=2;
      } else if (localPet == 3) {
        search_ids[0]=11; search_info_check[0]=0.11;
        search_ids[1]=12; search_info_check[1]=0.12;
        num_search=2;
      } 
    } else {
      Throw() <<"This test only works when run on 1 or 4 PETs.";
    }

    // Search 
    di_test.search(num_search, search_ids, true, -100.0, search_info);
    
    // Check answers for accuracy
    for (auto i=0; i<num_search; i++) {
      if (search_info[i] != search_info_check[i]) {
        printf("%f != %f\n",search_info[i],search_info_check[i]);
        rc=ESMF_FAILURE; // Change to error if we detect an error
        break;
      }
    }
  }
  CATCH_FOR_TESTING(rc);
  
  // Output test info
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //NEX_UTest
  strcpy(name, "Test search() without error of an item that isn't there and also of 0 length searches.");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  //// NOTE: Using the Dinfo object from above /////

  // Try-catch to catch errors
  try {
    
    // Make search info depending on number of PETs
    int num_search=0;
    int search_ids[20]; // size bigger than needed below
    double search_info[20]; // size bigger than needed below
    double search_info_check[20]; // size bigger than needed below
    
    if (petCount == 1) {
      search_ids[0]=17; search_info_check[0]=-100.0; // set check to bad info value used in search below
      num_search=1;
    } else if (petCount == 4) {
      if (localPet == 0) {
        num_search=0;
      } else if (localPet == 1) {
        num_search=0;
      } else if (localPet == 2) {
        search_ids[0]=17; search_info_check[0]=-100.0; // set check to bad info value used in search below
        num_search=1;
      } else if (localPet == 3) {
        num_search=0;
      } 
    } else {
      Throw() <<"This test only works when run on 1 or 4 PETs.";
    }

    // Search, but accept not found ids
    di_test.search(num_search, search_ids, false, -100.0, search_info);
    
    // Check answers for accuracy
    for (auto i=0; i<num_search; i++) {
      if (search_info[i] != search_info_check[i]) {
        printf("%f != %f\n",search_info[i],search_info_check[i]);
        rc=ESMF_FAILURE; // Change to error if we detect an error
        break;
      }
    }
  }
  CATCH_FOR_TESTING(rc);
  
  // Output test info
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  /// TAKE THIS OUT BECAUSE IT HANGS FOR THE USUAL REASON WITH A FAILURE ON ONE PET
  /// HOWEVER, LEAVING IN CASE WE NEED TO CHECK THAT THE ERROR CHECK IS STILL WORKING
#if 0   
  //NEX_OFF_UTest
  strcpy(name, "Test search() of an item that isn't there with error.");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  //// NOTE: Using the Dinfo object from above /////

  // Try-catch to catch errors
  try {
    
    // Make search info depending on number of PETs
    int num_search=0;
    int search_ids[20]; // size bigger than needed below
    double search_info[20]; // size bigger than needed below
    double search_info_check[20]; // size bigger than needed below
    
    if (petCount == 1) {
      search_ids[0]=17; search_info_check[0]=-100.0; // set check to bad info value used in search below
      num_search=1;
    } else if (petCount == 4) {
      if (localPet == 0) {
        num_search=0;
      } else if (localPet == 1) {
        num_search=0;
      } else if (localPet == 2) {
        search_ids[0]=17; search_info_check[0]=-100.0; // set check to bad info value used in search below
        num_search=1;
      } else if (localPet == 3) {
        num_search=0;
      } 
    } else {
      Throw() <<"This test only works when run on 1 or 4 PETs.";
    }

    // Search, but don't accept not founds ids. This should result in an error
    di_test.search(num_search, search_ids, true, -100.0, search_info);
    
  }
  CATCH_FOR_TESTING(rc);
  
  // Output test info
  ESMC_Test((rc != ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif
  
  //NEX_UTest
  strcpy(name, "Test clear()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  // Try-catch to catch errors
  try {

    // Reset Dinfo object to empty-ish, but leave memory
    di_test.clear();
    
  }
  CATCH_FOR_TESTING(rc);

  // Output test info
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //NEX_UTest
  strcpy(name, "Create a small DInfo<int,double> object with repeated ids with different info.");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  // Try-catch to catch errors
  try {

    // Add some data depending on PET
    if (petCount == 1) {
        di_test.add(11,0.11);
        di_test.add(11,0.77); // Repeated id, but different info than above
        di_test.add(12,0.12);
        di_test.add(18,0.18);            
        di_test.add(8,0.8);
        di_test.add(6,0.6);            
        di_test.add(3,0.3);
        di_test.add(2,0.2);
        di_test.add(1,0.1);
    } else if (petCount == 4) {
      if (localPet == 0) {
        di_test.add(11,0.11);
        di_test.add(12,0.12);
      } else if (localPet == 1) {
        di_test.add(18,0.18);            
        di_test.add(11,0.77); // Repeated id, but different info than above
        di_test.add(8,0.8);
      } else if (localPet == 2) {
        di_test.add(6,0.6);            
        di_test.add(3,0.3);
      } else if (localPet == 3) {
        di_test.add(2,0.2);
        di_test.add(1,0.1);
      } 
    } else {
      Throw() <<"This test only works when run on 1 or 4 PETs.";
    }

    // Commit
    di_test.commit();

    // Debug Output
    //di_test.print_searchable();
    
  }
  CATCH_FOR_TESTING(rc);

  // Output test info
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  /// TAKE THIS OUT BECAUSE IT HANGS FOR THE USUAL REASON WITH A FAILURE ON ONE PET
  /// HOWEVER, LEAVING IN CASE WE NEED TO CHECK THAT THE ERROR CHECK IS STILL WORKING
#if 0
  //NEX_OFF_UTest
  strcpy(name, "Test search of object with repeated ids with search method that doesn't allow that.");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Init to sucess
  rc=ESMF_SUCCESS;

  //// NOTE: Using the Dinfo object from above /////

  // Try-catch to catch errors
  try {
    
    // Make search info depending on number of PETs
    int num_search=0;
    int search_ids[20]; // size bigger than needed below
    double search_info[20]; // size bigger than needed below
    double search_info_check[20]; // size bigger than needed below
    
    if (petCount == 1) {
      search_ids[0]=8; search_info_check[0]=0.8;
      num_search=1;
    } else if (petCount == 4) {
      if (localPet == 0) {
        num_search=0;
      } else if (localPet == 1) {
        num_search=0;
      } else if (localPet == 2) {
        search_ids[0]=8; search_info_check[0]=0.8; 
        num_search=1;
      } else if (localPet == 3) {
        num_search=0;
      } 
    } else {
      Throw() <<"This test only works when run on 1 or 4 PETs.";
    }

    // Search, but don't accept not founds ids. This should result in an error
    di_test.search(num_search, search_ids, false, -100.0, search_info);
    
  }
  CATCH_FOR_TESTING(rc);
  
  // Output test info
  ESMC_Test((rc != ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif


  
  //// When you've added search that allows repeated ids add that here /////
  

 
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
