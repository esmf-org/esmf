// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <cstring>
#include <cstdio>
#include <vector>
using namespace std;

#if defined ESMF_MOAB

#include "moab/Core.hpp"
#include "moab/ScdInterface.hpp"
using namespace moab;
#endif

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_MeshMOABUTest - Check for MOAB functionality
//
// !DESCRIPTION: This set of unit tests is based on the MOAB
// structuredmesh example at:
//
//  http://ftp.mcs.anl.gov/pub/fathom/moab-docs/structuredmesh_8cpp_source.html
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;

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

  //----------------------------------------------------------------------------
  // Instantiate MOAB and get the structured mesh interface
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Moab scdInterface access test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB
  Interface *mb = new Core();
  ScdInterface *scdiface;
  ErrorCode rval = mb->query_interface(scdiface); // get a ScdInterface through
                                                  // moab instance
  rc = (rval == MB_SUCCESS)?ESMF_SUCCESS:ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  // Create the mesh
  //----------------------------------------------------------------------------

  // Creates a IxJxK structured mesh, which includes I*J*K vertices
  // and (I-1)*(J-1)*(K-1) hexes.
  int I=5;
  int J=5;
  int K=8;

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create structured mesh");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB

  ScdBox *box;
  rval = scdiface->construct_box(HomCoord(0, 0, 0), HomCoord(I-1, J-1, K-1),
                                 NULL, 0,
                                 box);
  rc = (rval == MB_SUCCESS)?ESMF_SUCCESS:ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Access vertices test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB

  Range verts;
  rval = mb->get_entities_by_dimension(0, 0, verts);
  rc = (rval == MB_SUCCESS)?ESMF_SUCCESS:ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Check vertices test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB

  rc = ((int)verts.size() == I*J*K)?ESMF_SUCCESS:ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Access hexes test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB

  Range hexes;
  rval = mb->get_entities_by_dimension(0, 3, hexes);
  rc = (rval == MB_SUCCESS)?ESMF_SUCCESS:ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Check hexes test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB

  rc = ((int)hexes.size() == (I-1)*(J-1)*(K-1))?ESMF_SUCCESS:ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Check verts test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB

  rc = ((int)verts.size() == I*J*K)?ESMF_SUCCESS:ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  // Loop over elements in 3 nested loops over i, j, k; for each (i,j,k)
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Loop over elements test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  rc = ESMF_SUCCESS;
#if defined ESMF_MOAB

  std::vector<double> coords(8*3);
  std::vector<EntityHandle> connect;
  for (int k = 0; k < K-1; k++) {
    for (int j = 0; j < J-1; j++) {
      for (int i = 0; i < I-1; i++) {
          // 3a. Get the element corresponding to (i,j,k)
        EntityHandle ehandle = box->get_element(i, j, k);
        if (0 == ehandle) {
          rc = ESMF_FAILURE;
          break;
        }
          // 3b. Get the connectivity of the element
        rval = mb->get_connectivity(&ehandle, 1, connect); // get the connectivity, in canonical order
        if (MB_SUCCESS != rval) {
          rc = ESMF_FAILURE;
          break;
        }
          // 3c. Get the coordinates of the vertices comprising that element
        rval = mb->get_coords(&connect[0], connect.size(), &coords[0]); // get the coordinates of those vertices
        if (MB_SUCCESS != rval) {
          rc = ESMF_FAILURE;
          break;
        }
      }
    }
  }
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);


  //----------------------------------------------------------------------------
  // Release resources tests
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Release interface test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB

  rval = mb->release_interface(scdiface);
  rc = (rval == MB_SUCCESS)?ESMF_SUCCESS:ESMF_FAILURE;
#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy MOAB instance test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMF_SUCCESS;
#if defined ESMF_MOAB

  try {
    delete mb;
  }
  catch (int ex) {
    rc = ESMF_RC_MEM_DEALLOCATE;
  };
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

