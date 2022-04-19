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
        MCT *test = generate->mesh_map[mesh](localrc);

        test->name = name;
        test->nativeormb = nvmb;
        test->verbosity = 3;
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
  // mbmesh = true;
  bool native = false;
  native = true;

  // these are bound to MCT in constructor, must match!
  std::vector<std::string> test_apis;
    // test_apis.push_back("createget");
    // dual not working after ngon connectivity changes
    test_apis.push_back("dual");
    // test_apis.push_back("redist_elem");
    // test_apis.push_back("redist_node");
    // test_apis.push_back("redist_elno");
    // test_apis.push_back("serialize");
    // test_apis.push_back("to_pointlist_elem");
    // test_apis.push_back("to_pointlist_node");
    // test_apis.push_back("write_vtk");

  // these are bound to MCTGen in constructor, must match!
  std::vector<std::string> test_meshes;
    test_meshes.push_back("quad_2d_cart");
    // test_meshes.push_back("quad_2d_sph_deg");
    // test_meshes.push_back("quad_2d_sph_rad");
    // test_meshes.push_back("tri_2d_cart");
    // test_meshes.push_back("tri_2d_sph_deg");
    // test_meshes.push_back("tri_2d_sph_rad");
    // test_meshes.push_back("hex_3d_cart");
    // test_meshes.push_back("hex_3d_sph_deg");
    // test_meshes.push_back("hex_3d_sph_rad");
    // test_meshes.push_back("mix_2d_cart");
    // test_meshes.push_back("mix_2d_sph_deg");
    // test_meshes.push_back("mix_2d_sph_rad");
    // test_meshes.push_back("periodic_2d_sph_deg");
    // test_meshes.push_back("periodic_2d_sph_rad");
    // test_meshes.push_back("ngon_2d_cart");
    // test_meshes.push_back("ngon_2d_sph_deg");
    // test_meshes.push_back("ngon_2d_sph_rad");
    // test_meshes.push_back("ngon_quad_2d_cart");
    // test_meshes.push_back("ngon_quad_2d_sph_deg");
    // test_meshes.push_back("ngon_quad_2d_sph_rad");

  std::vector<std::pair<std::string, std::string>> skip_test_common = {\
    // dual meshes of ngons not supported
    {"dual", "ngon_2d_cart"},
    {"dual", "ngon_2d_sph_deg"},
    {"dual", "ngon_2d_sph_rad"},
    {"dual", "ngon_quad_2d_cart"},
    {"dual", "ngon_quad_2d_sph_deg"},
    {"dual", "ngon_quad_2d_sph_rad"},
    // dual not implemented in 3d
    {"dual", "hex_3d_cart"},
    {"dual", "hex_3d_sph_deg"},
    {"dual", "hex_3d_sph_rad"},  };

  std::vector<std::pair<std::string, std::string>> skip_test_mbmesh = {\
    // dual mix is giving segv in parllel for meshes with triangles
    // is_split is being set in MBMesh_detect_split_elems somehow
    {"dual", "mix_2d_cart"},
    {"dual", "mix_2d_sph_deg"},
    {"dual", "mix_2d_sph_rad"},
    // {"dual", "tri_2d_cart"},
    {"dual", "tri_2d_sph_deg"},
    {"dual", "tri_2d_sph_rad"},
    // ESMCI_MBMesh_Redist.C, line:2336:Could not find a suitable processor for this element
    {"redist_node", "tri_2d_cart"},
    {"redist_node", "tri_2d_sph_deg"},
    {"redist_node", "tri_2d_sph_rad"},
    // redist_elem failing for ngons after ngon connectivity changes
    {"redist_elem", "ngon_2d_cart"},
    {"redist_elem", "ngon_2d_sph_deg"},
    {"redist_elem", "ngon_2d_sph_rad"},
    {"redist_elem", "ngon_quad_2d_cart"},
    {"redist_elem", "ngon_quad_2d_sph_deg"},
    {"redist_elem", "ngon_quad_2d_sph_rad"},

  };

  std::vector<std::pair<std::string, std::string>> skip_test_native = {\
  };


  if (mbmesh) {
    for (const auto api: test_apis) {
      for (const auto mesh: test_meshes) {
        auto skip_itr_common = std::find_if(skip_test_common.begin(),
                                            skip_test_common.end(),
                                            FindPair(api, mesh));

        auto skip_itr_mbmesh = std::find_if(skip_test_mbmesh.begin(),
                                            skip_test_mbmesh.end(),
                                            FindPair(api, mesh));

        if ((skip_itr_common != skip_test_common.end()) ||
            (skip_itr_mbmesh != skip_test_mbmesh.end())) {
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
      for (const auto mesh: test_meshes) {
        auto skip_itr_common = std::find_if(skip_test_common.begin(),
                                            skip_test_common.end(),
                                            FindPair(api, mesh));

        auto skip_itr_native = std::find_if(skip_test_native.begin(),
                                            skip_test_native.end(),
                                            FindPair(api, mesh));

        if ((skip_itr_common != skip_test_common.end()) ||
            (skip_itr_native != skip_test_native.end())) {
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
