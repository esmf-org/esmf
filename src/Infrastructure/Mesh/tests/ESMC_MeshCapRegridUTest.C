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

void combine(const std::string &api, const std::string &mesh, 
             const std::string &nativeormb){
    int localrc;
    int rc = ESMF_FAILURE;
    
    std::string failMsg = "FAIL";
    int result = 0;

#if defined ESMF_MOAB
    MBTGen *generate = new MBTGen();
#endif

    int nvmb = 1;
    if (nativeormb == "Native") nvmb = 0;
    
    std::string name = nativeormb + " Mesh - " + api + " - " + mesh;

#if defined ESMF_MOAB
      try {
        MBT *test = generate->mesh_map[mesh](localrc);
        
        test->name = name;
        test->nativeormb = nvmb;
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
    
#if defined ESMF_MOAB
    delete generate;
#endif

    ESMC_Test(rc==ESMF_SUCCESS, name.c_str(), failMsg.c_str(), 
              &result, __FILE__, __LINE__, 0);
}

int main(int argc, char *argv[]) {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshRegidUTest::main()"

  std::string failMsg = "FAIL";
  int result = 0;
  int rc, localrc;
  int localPet, petCount;
  ESMC_VM vm;

  bool mbmesh = true;
  bool native = true; 

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);

  // these are bound to MBT in constructor, must match!
  std::vector<std::string> test_apis_mbmesh;
    test_apis_mbmesh.push_back("regrid_bilinear_center");
    test_apis_mbmesh.push_back("regrid_bilinear_corner");
    test_apis_mbmesh.push_back("regrid_conserve_center");
    // test_apis_mbmesh.push_back("regrid_conserve_2nd_center");
    test_apis_mbmesh.push_back("regrid_nearest_d2s");
    test_apis_mbmesh.push_back("regrid_nearest_s2d");
    // test_apis_mbmesh.push_back("regrid_patch_center");
    // test_apis_mbmesh.push_back("regrid_patch_corner");

  // these are bound to MBTGen in constructor, must match!
  std::vector<std::string> test_meshes_mbmesh;
    test_meshes_mbmesh.push_back("quad_2d_cart");
    test_meshes_mbmesh.push_back("quad_2d_sph_deg");
    test_meshes_mbmesh.push_back("quad_2d_sph_rad");
    test_meshes_mbmesh.push_back("tri_2d_cart");
    test_meshes_mbmesh.push_back("tri_2d_sph_deg");
    test_meshes_mbmesh.push_back("tri_2d_sph_rad");
    test_meshes_mbmesh.push_back("hex_3d_cart");
    test_meshes_mbmesh.push_back("hex_3d_sph_deg");
    test_meshes_mbmesh.push_back("hex_3d_sph_rad");
    test_meshes_mbmesh.push_back("mix_2d_cart");
    test_meshes_mbmesh.push_back("mix_2d_sph_deg");
    test_meshes_mbmesh.push_back("mix_2d_sph_rad");
    test_meshes_mbmesh.push_back("ngon_2d_cart");
    test_meshes_mbmesh.push_back("ngon_2d_sph_deg");
    test_meshes_mbmesh.push_back("ngon_2d_sph_rad");

  // skip the following tests
  std::vector<std::pair<std::string, std::string>> skip_test_mbmesh = {\
    // regrid_conserve_2nd doesn't work in 3d
    // {"regrid_conserve_2nd", "hex_3d_cart"},
    // {"regrid_conserve_2nd", "hex_3d_sph"},
  };

  // these are bound to MBT in constructor, must match!
  std::vector<std::string> test_apis_native;
    test_apis_native.push_back("regrid_bilinear_center");
    test_apis_native.push_back("regrid_bilinear_corner");
    test_apis_native.push_back("regrid_conserve_center");
    test_apis_native.push_back("regrid_conserve_2nd_center");
    test_apis_native.push_back("regrid_nearest_d2s");
    test_apis_native.push_back("regrid_nearest_s2d");
    test_apis_native.push_back("regrid_patch_center");
    test_apis_native.push_back("regrid_patch_corner");

  // these are bound to MBTGen in constructor, must match!
  std::vector<std::string> test_meshes_native;
    test_meshes_native.push_back("quad_2d_cart");
    test_meshes_native.push_back("quad_2d_sph_deg");
    test_meshes_native.push_back("quad_2d_sph_rad");
    test_meshes_native.push_back("tri_2d_cart");
    test_meshes_native.push_back("tri_2d_sph_deg");
    test_meshes_native.push_back("tri_2d_sph_rad");
    test_meshes_native.push_back("hex_3d_cart");
    test_meshes_native.push_back("hex_3d_sph_deg");
    test_meshes_native.push_back("hex_3d_sph_rad");
    test_meshes_native.push_back("mix_2d_cart");
    test_meshes_native.push_back("mix_2d_sph_deg");
    test_meshes_native.push_back("mix_2d_sph_rad");
    test_meshes_native.push_back("ngon_2d_cart");
    test_meshes_native.push_back("ngon_2d_sph_deg");
    test_meshes_native.push_back("ngon_2d_sph_rad");

  // skip the following tests
  std::vector<std::pair<std::string, std::string>> skip_test_native = {\
    // node coord counts on PET 1 are off
    {"regrid_patch_center", "hex_3d_cart"},
    {"regrid_patch_corner", "hex_3d_cart"},
    // regrid_conserve_2nd doesn't work in 3d
    // {"regrid_conserve_2nd", "hex_3d_cart"},
    // {"regrid_conserve_2nd", "hex_3d_sph"},
  };

  // // combinatorial regrid options to explore
  // // these are bound to MBTGen in constructor, must match!
  // std::vector<std::string> test_regrid_maptype_native;
  //   test_regrid_maptype_native.push_back("MB_MAP_TYPE_CART_APPROX");
  //   test_regrid_maptype_native.push_back("MB_MAP_TYPE_GREAT_CIRCLE");
  // 
  // // these are bound to MBTGen in constructor, must match!
  // std::vector<std::string> test_regrid_normtype_native;
  //   test_regrid_normtype_native.push_back("DSTAREA");
  //   test_regrid_normtype_native.push_back("FRACAREA");
  // 
  // // the following require special handling
  // // these are bound to MBTGen in constructor, must match!
  // std::vector<std::string> test_regrid_poletype_native;
  //   test_regrid_poletype_native.push_back("NONE");
  //   test_regrid_poletype_native.push_back("ALL");
  //   test_regrid_poletype_native.push_back("NPNT");
  //   test_regrid_poletype_native.push_back("TEETH");
  // 
  // // these are bound to MBTGen in constructor, must match!
  // std::vector<std::string> test_regrid_extrapmethod_native;
  //   test_regrid_extrapmethod_native.push_back("NONE");
  //   test_regrid_extrapmethod_native.push_back("NEAREST_STOD");
  //   test_regrid_extrapmethod_native.push_back("NEAREST_IDAVG");
  //   test_regrid_extrapmethod_native.push_back("NEAREST_D");
  //   test_regrid_extrapmethod_native.push_back("CREEP");
  //   test_regrid_extrapmethod_native.push_back("CREEP_NRST_D");
  // 
  // // these are bound to MBTGen in constructor, must match!
  // std::vector<std::string> test_regrid_unmappedaction_native;
  //   test_regrid_unmappedaction_native.push_back("ERROR");
  //   test_regrid_unmappedaction_native.push_back("IGNORE");
  // 
  // // these are bound to MBTGen in constructor, must match!
  // std::vector<std::string> test_regrid_ignoredegenerate_native;
  //   test_regrid_ignoredegenerate_native.push_back("False");
  //   test_regrid_ignoredegenerate_native.push_back("True");
  
  if (mbmesh) {
    for (const auto api: test_apis_mbmesh) {
      for (const auto mesh: test_meshes_mbmesh) {    
        
        // don't run cases that hang
        auto skip_itr = std::find_if(skip_test_mbmesh.begin(), 
                                     skip_test_mbmesh.end(), 
                                     FindPair(api, mesh));
  
        // don't run cases that hang
        if (skip_itr != skip_test_mbmesh.end()) {
          continue;
        } else {
          combine(api, mesh, "MBMesh");
        }
      }
    }
  }

  if (native) {
    for (const auto api: test_apis_native) {
      for (const auto mesh: test_meshes_native) {
        // don't run cases that hang
        auto skip_itr = std::find_if(skip_test_native.begin(), 
                                     skip_test_native.end(), 
                                     FindPair(api, mesh));
  
        if (skip_itr != skip_test_native.end()) {
          continue;
        } else {
          combine(api, mesh, "Native");
        }
      }
    }
  }

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
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  // 50
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  // 60
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  // 70
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest 
  //NEX_disable_UTest
  //NEX_disable_UTest // 80
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest 
  //NEX_disable_UTest
  //NEX_disable_UTest // 90
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest // 100
  //NEX_disable_UTest




  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}



