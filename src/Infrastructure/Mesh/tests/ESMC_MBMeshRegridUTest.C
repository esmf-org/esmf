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
#define ESMC_METHOD "MBMeshRegidUTest::main()"

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
    test_apis.push_back("regrid_rendezvous_center");
    test_apis.push_back("regrid_rendezvous_corner");
    test_apis.push_back("regrid_search_center");
    test_apis.push_back("regrid_search_corner");
    test_apis.push_back("regrid_bilinear_center");
    test_apis.push_back("regrid_bilinear_corner");
    // test_apis.push_back("regrid_conserve_center");
    // test_apis.push_back("regrid_conserve_corner");
    // test_apis.push_back("regrid_conserve_2nd_center");
    // test_apis.push_back("regrid_conserve_2nd_corner");
    // test_apis.push_back("regrid_nearest_d2s_center");
    // test_apis.push_back("regrid_nearest_d2s_corner");
    // test_apis.push_back("regrid_nearest_s2d_center");
    // test_apis.push_back("regrid_nearest_s2d_corner");
    // test_apis.push_back("regrid_patch_center");
    // test_apis.push_back("regrid_patch_corner");

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
    // test_meshes.push_back("ngon_2d_cart");
    // test_meshes.push_back("ngon_2d_sph");

  // skip the following tests
  std::vector<std::pair<std::string, std::string>> skip_test = {\
    // regrid_conserve_2nd doesn't work in 3d
    {"regrid_conserve_2nd", "hex_3d_cart"},
    {"regrid_conserve_2nd", "hex_3d_sph"}
  };

#if defined ESMF_MOAB
  MBTGen *generate = new MBTGen();
#endif

  for (const auto api: test_apis) {
    for (const auto mesh: test_meshes) {    
      rc = ESMF_FAILURE;
      
      auto skip_itr = std::find_if(skip_test.begin(), skip_test.end(), 
                                   FindPair(api, mesh));

      // don't run cases that hang
      if (skip_itr != skip_test.end()) {
        rc = ESMF_SUCCESS;
      } else {
#if defined ESMF_MOAB
        try {
          MBT *test = generate->mesh_map[mesh](localrc);
          
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

      std::string name = "MBMesh - " + api + " - " + mesh;
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
  //NEX_UTest  // 10
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  // 20
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  // 30
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  // 40
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



