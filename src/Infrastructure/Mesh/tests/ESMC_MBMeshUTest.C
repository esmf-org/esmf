//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif
#include <mpi.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#if defined ESMF_MOAB
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMC_MBTGen.C"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>
#include <map>
#include <functional>
#include <algorithm> //find_if

struct FindPair {
    FindPair (std::string first, std::string second)
    : m_first_value(first), m_second_value(second) {}

    std::string m_first_value;
    std::string m_second_value;
    bool operator()
        ( const std::pair<std::string, std::string> &p ) {
            return (p.first == m_first_value && p.second == m_second_value);
    }
};

int main(int argc, char *argv[]) {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshUTest::main()"

  std::string failMsg = "FAIL";
  int result = 0;
  int rc, localrc;
  int localPet, petCount;
  ESMC_VM vm;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);

  // these are bound to MBT in constructor, must match!
  std::vector<std::string> test_apis;
    test_apis.push_back("createget");
    test_apis.push_back("dual");
    test_apis.push_back("redist_elem");
    test_apis.push_back("redist_node");
    test_apis.push_back("redist_elno");
    test_apis.push_back("serialize");
    test_apis.push_back("to_pointlist_elem");
    test_apis.push_back("to_pointlist_node");
    test_apis.push_back("write_vtk");
    // test_apis.push_back("mbtypes");

  // these are bound to MBTGen in constructor, must match!
  std::vector<std::string> test_meshes;
    test_meshes.push_back("quad_2d_cart");
    test_meshes.push_back("quad_2d_sph");
    test_meshes.push_back("tri_2d_cart");
    test_meshes.push_back("tri_2d_sph");
    test_meshes.push_back("hex_3d_cart");
    test_meshes.push_back("hex_3d_sph");
    test_meshes.push_back("mix_2d_cart");
    test_meshes.push_back("mix_2d_sph");
    test_meshes.push_back("ngon_2d_cart");
    test_meshes.push_back("ngon_2d_sph");

  // skip the following tests
  std::vector<std::pair<std::string, std::string>> skip_test = {\
    // dual not implemented in 3d
    {"dual", "hex_3d_cart"},
    {"dual", "hex_3d_sph"},
    // ESMCI_MBMesh_Redist.C, line:2336:Could not find a suitable processor for this element
    {"redist_node", "tri_2d_cart"},
    {"redist_node", "tri_2d_sph"},
  };

#if defined ESMF_MOAB
  MBTGen *generate = new MBTGen();
#endif

  for (const auto api: test_apis) {
    for (const auto mesh: test_meshes) {    
      rc = ESMF_FAILURE;
      
      std::string name = "MBMesh - " + api + " - " + mesh;

      auto skip_itr = std::find_if(skip_test.begin(), skip_test.end(), 
                                   FindPair(api, mesh));

      // don't run cases that hang
      if (skip_itr != skip_test.end()) {
        name = "SKIP - " + name;
        rc = ESMF_SUCCESS;
      } else {
#if defined ESMF_MOAB
        try {
          MBT *test = generate->mesh_map[mesh](localrc);
          
          test->name = name;
          // test->verbosity = 3;
          // test->tol = 1.e-15;
          // test->print();

          if (localrc == ESMF_SUCCESS) localrc = test->build();
          if (localrc == ESMF_SUCCESS) rc = test->function_map[api]();
          
          delete test;
        }
        CATCH_MBT_FAIL(&rc)
#else
        rc = ESMF_SUCCESS;
#endif
      }

      ESMC_Test(rc==ESMF_SUCCESS, name.c_str(), failMsg.c_str(), 
                &result, __FILE__, __LINE__, 0);
    }
  }

#if defined ESMF_MOAB
  delete generate;
#endif

  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  10
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  20
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  30
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  40
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  50
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  60
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  70
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  80
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest


  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}



