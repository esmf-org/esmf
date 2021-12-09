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
#include "ESMC_MCTGen.C"
#endif

#include "ESMCI_MeshCap.h"
#include "ESMCI_Mesh.h"

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>
#include <map>
#include <functional>
#include <algorithm> //find_if

struct FindPair {
    FindPair (std::string first, std::string second) \
    : m_first_value(first), m_second_value(second) {}

    std::string m_first_value;
    std::string m_second_value;
    bool operator() (const std::pair<std::string, std::string> &p) {
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
    MCTGen *generate = new MCTGen();
#endif

    int nvmb = 1;
    if (nativeormb == "Native") nvmb = 0;
    
    std::string name = nativeormb + " Mesh - " + api + " - " + mesh;

#if defined ESMF_MOAB
      try {
        MCT *test = generate->mesh_map[mesh](localrc);
        
        test->name = name;
        test->nativeormb = nvmb;
        // test->verbosity = 3;
        // test->tol = 1.e-15;
        // test->print();
        
        if (localrc == ESMF_SUCCESS) localrc = test->build();
        if (localrc == ESMF_SUCCESS) rc = test->function_map[api]();
        
        delete test;
      }
      CATCH_MCT_FAIL(&rc)
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
#define ESMC_METHOD "MBMeshUTest::main()"

  int rc;
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

  // these are bound to MCT in constructor, must match!
  std::vector<std::string> test_apis_native;
    test_apis_native.push_back("createget");
    test_apis_native.push_back("dual");
    test_apis_native.push_back("redist_elem");
    test_apis_native.push_back("redist_node");
    test_apis_native.push_back("redist_elno");
    test_apis_native.push_back("serialize");
    test_apis_native.push_back("to_pointlist_elem");
    test_apis_native.push_back("to_pointlist_node");
    test_apis_native.push_back("write_vtk");

  // these are bound to MCTGen in constructor, must match!
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
    // ngons currently return the wrong counts
    // test_meshes_native.push_back("ngon_2d_cart");
    // test_meshes_native.push_back("ngon_2d_sph");

  // these are bound to MCT in constructor, must match!
  std::vector<std::string> test_apis_mbmesh;
    test_apis_mbmesh.push_back("createget");
    test_apis_mbmesh.push_back("dual");
    test_apis_mbmesh.push_back("redist_elem");
    test_apis_mbmesh.push_back("redist_node");
    test_apis_mbmesh.push_back("redist_elno");
    test_apis_mbmesh.push_back("serialize");
    test_apis_mbmesh.push_back("to_pointlist_elem");
    test_apis_mbmesh.push_back("to_pointlist_node");
    test_apis_mbmesh.push_back("write_vtk");

  // these are bound to MCTGen in constructor, must match!
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
    // dual not implemented in 3d
    {"dual", "hex_3d_cart"},
    {"dual", "hex_3d_sph_deg"},
    {"dual", "hex_3d_sph_rad"},
    // ESMCI_MBMesh_Redist.C, line:2336:Could not find a suitable processor for this element
    {"redist_node", "tri_2d_cart"},
    {"redist_node", "tri_2d_sph_deg"},
    {"redist_node", "tri_2d_sph_rad"},

  };

  std::vector<std::pair<std::string, std::string>> skip_test_native = {\
    // dual not implemented in 3d
    // Creation of a dual mesh isn't supported for Meshes of parametric dim greater than 3.
    {"dual", "hex_3d_cart"},
    {"dual", "hex_3d_sph_deg"},
    {"dual", "hex_3d_sph_rad"},
  };


  if (mbmesh) {
    for (const auto api: test_apis_mbmesh) {
      for (const auto mesh: test_meshes_mbmesh) {
        // don't run cases that hang
        auto skip_itr = std::find_if(skip_test_mbmesh.begin(),   skip_test_mbmesh.end(), 
                                         FindPair(api, mesh));
  
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
        auto skip_itr = std::find_if(skip_test_native.begin(),   skip_test_native.end(), 
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
  //NEX_UTest  90
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  100
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  110
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  120
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  130
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  140
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  150
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  160
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  170
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  180
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  190
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  200
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  210
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  220
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest  230
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  240
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  250
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  260
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  270
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  280
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest  290
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest
  //NEX_disable_UTest


  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}



