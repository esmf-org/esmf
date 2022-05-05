//==============================================================================
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
#include <ctime>

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
    
    int localPet, petCount;
    ESMC_VM vm;
    vm=ESMC_VMGetGlobal(&rc);
    rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                  (int *)NULL, (int *)NULL);

    std::array<char, 64> buffer;
    buffer.fill(0);
    time_t rawtime;
    time(&rawtime);
    const auto timeinfo = localtime(&rawtime);
    strftime(buffer.data(), sizeof(buffer), "%d-%m-%Y %H-%M-%S", timeinfo);
    std::string timeStr(buffer.data());
    
    std::string name = timeStr + " - " + "PET " + std::to_string(localPet) + " - " +
                       nativeormb + " Mesh - " + api + " - " + mesh;

#if defined ESMF_MOAB
      try {
        std::shared_ptr<MCT> test = generate->mesh_map[mesh](localrc);
        
        test->name = name;
        test->nativeormb = nvmb;
        // test->verbosity = 3;
        // test->tol = 1.e-15;
        // test->print();
        
        if (localrc == ESMF_SUCCESS) localrc = test->build();
        if (localrc == ESMF_SUCCESS) rc = test->function_map[api]();
        
        // test is a shared_ptr so no need to delete
        // delete test;
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
#define ESMC_METHOD "MeshCapUTest::main()"

  int rc;
  int localPet, petCount;
  ESMC_VM vm;

  std::string failMsg = "FAIL";
  // break this up so it doesn't count as an additional test
  std::string nex_test_tag = "//NEX_";
  nex_test_tag.append("UTest\n");
  int result = 0;

  //----------------------------------------------------------------------------
  // Start the test with an alternate name for log and stdout files
  ESMC_TestStart("ESMC_MeshCapGenUTest.C", __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);

  // output stream to write test tag to the generated source file
  std::ofstream tagfile;
  tagfile.open("ESMC_MeshCapGenUTest.C", std::ios_base::app);

  // this is an easy way to comment a single line to toggle mbmesh/native
  bool mbmesh = false;
  mbmesh = true;
  bool native = false; 
  native = true; 

  // these are bound to MCT in constructor, must match!
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
    test_meshes_native.push_back("periodic_2d_sph_deg");
    test_meshes_native.push_back("periodic_2d_sph_rad");
    // cannot get info from ngons
    // test_meshes_native.push_back("ngon_2d_cart");
    // test_meshes_native.push_back("ngon_2d_sph_deg");
    // test_meshes_native.push_back("ngon_2d_sph_rad");

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
    test_meshes_mbmesh.push_back("periodic_2d_sph_deg");
    test_meshes_mbmesh.push_back("periodic_2d_sph_rad");

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
    for (const auto api: test_apis) {
      for (const auto mesh: test_meshes_mbmesh) {
        // don't run cases that hang
        auto skip_itr = std::find_if(skip_test_mbmesh.begin(),   skip_test_mbmesh.end(), 
                                         FindPair(api, mesh));
  
        if (skip_itr != skip_test_mbmesh.end()) {
          continue;
        } else {
          combine(api, mesh, "MBMesh");
          // only print one tag per test (so only on root)
          if (localPet == 0) tagfile << nex_test_tag;
        }
      }
    }
  }

  if (native) {
    for (const auto api: test_apis) {
      for (const auto mesh: test_meshes_native) {
        // don't run cases that hang
        auto skip_itr = std::find_if(skip_test_native.begin(),   skip_test_native.end(), 
                                         FindPair(api, mesh));
    
        if (skip_itr != skip_test_native.end()) {
          continue;
        } else {
          combine(api, mesh, "Native");
          // only print one tag per test (so only on root)
          if (localPet == 0) tagfile << nex_test_tag;
        }
      }
    }
  }

  tagfile.close();

  //----------------------------------------------------------------------------
  ESMC_TestEnd("ESMC_MeshCapGenUTest.C", __LINE__, 0);

  return 0;
}
