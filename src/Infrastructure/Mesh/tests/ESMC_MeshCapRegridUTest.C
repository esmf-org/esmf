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

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>
#include <map>
#include <functional>
#include <algorithm> //find_if
#include <fstream>
#include <ctime>

struct FindAnyPair {
    FindAnyPair (std::string a, std::string b, std::string c, std::string d, 
              std::string e, std::string f, std::string g, std::string h)
    : a_val(a), b_val(b), c_val(c), d_val(d), 
      e_val(e), f_val(f), g_val(g), h_val(h) {}

    std::string a_val;
    std::string b_val;
    std::string c_val;
    std::string d_val;
    std::string e_val;
    std::string f_val;
    std::string g_val;
    std::string h_val;
    bool operator()
        ( const std::pair<std::string, std::string> &p ) {
            return (((p.first == a_val) || (p.first == b_val) || 
                     (p.first == c_val) || (p.first == d_val) || 
                     (p.first == e_val) || (p.first == f_val) || 
                     (p.first == g_val) || (p.first == h_val)) && 
                    ((p.second == a_val) || (p.second == b_val) || 
                    (p.second == c_val) || (p.second == d_val) || 
                    (p.second == e_val) || (p.second == f_val) || 
                    (p.second == g_val) || (p.second == h_val)));
    }
};

void combine(const std::string &api, const std::string &mesh, 
             const std::string &nativeormb,
             const std::string &maptype,
             const std::string &normtype,
             const std::string &poletype,
             const std::string &extrapmethod,
             const std::string &unmappedaction,
             const std::string &ignoredegenerate){
    int localrc;
    int rc = ESMF_FAILURE;
    
    std::string failMsg = "FAIL";
    std::string dash = " - ";
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
    
    std::string name = timeStr + dash + "PET " + std::to_string(localPet) + dash +
                       nativeormb + " Mesh - " + api + dash + mesh +
                       dash + maptype + dash + normtype + dash + poletype +
                       dash + extrapmethod + dash + unmappedaction + 
                       dash + ignoredegenerate;

#if defined ESMF_MOAB
      try {
        std::shared_ptr<MCT> test = generate->mesh_map[mesh](localrc);

        test->name = name;
        test->nativeormb = nvmb;
        // test->verbosity = 3;
        // test->tol = 1.e-15;
        // test->print();
        
        // convert parameters to integers
        int mt = test->MapType[maptype];
        int nt = test->NormType[normtype];
        int pt = test->PoleType[poletype];
        int em = test->ExtrapMethod[extrapmethod];
        int ua = test->UnmappedAction[unmappedaction];
        int id = test->IgnoreDegenerate[ignoredegenerate];
        
        if (localrc == ESMF_SUCCESS) localrc = test->build();
        if (localrc == ESMF_SUCCESS) rc = test->regrid_map[api](mt, nt, pt,
                                                                em, ua, id);

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
#define ESMC_METHOD "MBMeshRegidUTest::main()"

  int rc, localrc;
  int localPet, petCount;
  ESMC_VM vm;

  std::string failMsg = "FAIL";
  // break this up so it doesn't count as an additional test
  std::string nex_test_tag = "//NEX_";
  nex_test_tag.append("UTest\n");
  int result = 0;

  //----------------------------------------------------------------------------
  // Start the test with an alternate name for log and stdout files
  ESMC_TestStart("ESMC_MeshCapRegridGenUTest.C", __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);

  // output stream to write test tag to the generated source file
  std::ofstream tagfile;
  tagfile.open("ESMC_MeshCapRegridGenUTest.C", std::ios_base::app);

  // this is an easy way to comment a single line to toggle mbmesh/native
  bool mbmesh = false;
  mbmesh = true;
  bool native = false; 
  native = true; 

  std::vector<std::string> test_apis_mbmesh;
    test_apis_mbmesh.push_back("bilinear");
    test_apis_mbmesh.push_back("conservative");
    test_apis_mbmesh.push_back("nearest_d2s");
    test_apis_mbmesh.push_back("nearest_s2d");
    // Not yet available with MBMesh
    // test_apis_mbmesh.push_back("conservative_2nd");
    // test_apis_mbmesh.push_back("patch");

  std::vector<std::string> test_apis_native;
    test_apis_native.push_back("bilinear");
    test_apis_native.push_back("conservative");
    test_apis_native.push_back("conservative_2nd");
    test_apis_native.push_back("nearest_d2s");
    test_apis_native.push_back("nearest_s2d");
    test_apis_native.push_back("patch");

  // these are bound to MCTGen in constructor, must match!
  std::vector<std::string> test_meshes;
    test_meshes.push_back("quad_2d_cart");
    test_meshes.push_back("quad_2d_sph_deg");
    test_meshes.push_back("quad_2d_sph_rad");
    test_meshes.push_back("tri_2d_cart");
    test_meshes.push_back("tri_2d_sph_deg");
    test_meshes.push_back("tri_2d_sph_rad");
    test_meshes.push_back("hex_3d_cart");
    test_meshes.push_back("hex_3d_sph_deg");
    test_meshes.push_back("hex_3d_sph_rad");
    test_meshes.push_back("mix_2d_cart");
    test_meshes.push_back("mix_2d_sph_deg");
    test_meshes.push_back("mix_2d_sph_rad");
    test_meshes.push_back("ngon_2d_cart");
    test_meshes.push_back("ngon_2d_sph_deg");
    test_meshes.push_back("ngon_2d_sph_rad");
    test_meshes.push_back("periodic_2d_sph_deg");
    test_meshes.push_back("periodic_2d_sph_rad");

  // method dependent, and mesh dependent 
  std::vector<std::string> test_regrid_maptype;
    test_regrid_maptype.push_back("MAP_CARTAPPROX");
    test_regrid_maptype.push_back("MAP_GREATCIRCLE");
    
  // only conservative methods, all meshes
  std::vector<std::string> test_regrid_normtype;
    test_regrid_normtype.push_back("NORM_DSTAREA");
    test_regrid_normtype.push_back("NORM_FRACAREA");
  
  // all methods, but only for periodic grids
  std::vector<std::string> test_regrid_poletype;
    test_regrid_poletype.push_back("POLE_NONE");
    test_regrid_poletype.push_back("POLE_ALL");
    test_regrid_poletype.push_back("POLE_NPNT");
    test_regrid_poletype.push_back("POLE_TEETH");
  
  // all methods except nn, but only for meshes with destination larger than source
  std::vector<std::string> test_regrid_extrapmethod;
    test_regrid_extrapmethod.push_back("EXTRAP_NONE");
    test_regrid_extrapmethod.push_back("EXTRAP_NEAREST_STOD");
    test_regrid_extrapmethod.push_back("EXTRAP_NEAREST_IDAVG");
    test_regrid_extrapmethod.push_back("EXTRAP_NEAREST_D");
    test_regrid_extrapmethod.push_back("EXTRAP_CREEP");
    test_regrid_extrapmethod.push_back("EXTRAP_CREEP_NRST_D");
  
  // all methods except nn, but only for meshes with destination larger than source
  std::vector<std::string> test_regrid_unmappedaction;
    test_regrid_unmappedaction.push_back("UNMAPPED_THROWERROR");
    test_regrid_unmappedaction.push_back("UNMAPPED_IGNORE");
  
  // all methods except nn, but only meshes with degenerate, clockwise or concave elements
  std::vector<std::string> test_regrid_ignoredegenerate;
    test_regrid_ignoredegenerate.push_back("DONOT_IGNORE_DEGENERATE");
    test_regrid_ignoredegenerate.push_back("IGNORE_DEGENERATE");
  
  
  
  std::vector<std::pair<std::string, std::string>> skip_test_mbmesh = {\
    // conservative not supported with ngons
    {"conservative", "ngon_2d_cart"},
    {"conservative", "ngon_2d_sph_deg"},
    {"conservative", "ngon_2d_sph_rad"},
    // extrapolation methods not supported (why do EXTRAP_NEAREST_STOD, EXTRAP_NEAREST_IDAVG work?)
    {"bilinear", "EXTRAP_NEAREST_D"},
    {"bilinear", "EXTRAP_CREEP"},
    {"bilinear", "EXTRAP_CREEP_NRST_D"},
    {"patch", "EXTRAP_NEAREST_D"},
    {"patch", "EXTRAP_CREEP"},
    {"patch", "EXTRAP_CREEP_NRST_D"},
  };

  std::vector<std::pair<std::string, std::string>> skip_test_native = {\
    // meshes with ngons return incorrect element count with native
    {"bilinear", "ngon_2d_cart"},
    {"bilinear", "ngon_2d_sph_deg"},
    {"bilinear", "ngon_2d_sph_rad"},
    {"patch", "ngon_2d_cart"},
    {"patch", "ngon_2d_sph_deg"},
    {"patch", "ngon_2d_sph_rad"},
    {"conservative", "ngon_2d_cart"},
    {"conservative", "ngon_2d_sph_deg"},
    {"conservative", "ngon_2d_sph_rad"},
    {"conservative_2nd", "ngon_2d_cart"},
    {"conservative_2nd", "ngon_2d_sph_deg"},
    {"conservative_2nd", "ngon_2d_sph_rad"},
    // Conservative not supported on 3D spherical meshes
    {"conservative", "hex_3d_sph_deg"},
    {"conservative", "hex_3d_sph_rad"},
  };

  std::vector<std::pair<std::string, std::string>> skip_test_common = {\
    // patch not supported in 3d
    {"patch", "hex_3d_cart"},
    {"patch", "hex_3d_sph_deg"},
    {"patch", "hex_3d_sph_rad"},
    // conservative_2nd not supported in 3d
    {"conservative_2nd", "hex_3d_cart"},
    {"conservative_2nd", "hex_3d_sph_deg"},
    {"conservative_2nd", "hex_3d_sph_rad"},
    // creep fill extrapolation not supported in 3D
    {"hex_3d_cart", "EXTRAP_CREEP"},
    {"hex_3d_sph_deg", "EXTRAP_CREEP"},
    {"hex_3d_sph_rad", "EXTRAP_CREEP"},
    {"hex_3d_cart", "EXTRAP_CREEP_NRST_D"},
    {"hex_3d_sph_deg", "EXTRAP_CREEP_NRST_D"},
    {"hex_3d_sph_rad", "EXTRAP_CREEP_NRST_D"},
    // conservative is not supported with extrapolation
    {"conservative", "EXTRAP_NEAREST_STOD"},
    {"conservative", "EXTRAP_NEAREST_IDAVG"},
    {"conservative", "EXTRAP_NEAREST_D"},
    {"conservative", "EXTRAP_CREEP"},
    {"conservative", "EXTRAP_CREEP_NRST_D"},
    {"conservative_2nd", "EXTRAP_NEAREST_STOD"},
    {"conservative_2nd", "EXTRAP_NEAREST_IDAVG"},
    {"conservative_2nd", "EXTRAP_NEAREST_D"},
    {"conservative_2nd", "EXTRAP_CREEP"},
    {"conservative_2nd", "EXTRAP_CREEP_NRST_D"},
    // the following are exceptions due to design, do not represent limitations per se
    // conservative only works with great circles
    {"conservative", "MAP_CARTAPPROX"},
    {"conservative_2nd", "MAP_CARTAPPROX"},
    // cartesian only works with straight lines
    {"quad_2d_cart", "MAP_GREATCIRCLE"},
    {"tri_2d_cart", "MAP_GREATCIRCLE"},
    {"hex_3d_cart", "MAP_GREATCIRCLE"},
    {"mix_2d_cart", "MAP_GREATCIRCLE"},
    {"ngon_2d_cart", "MAP_GREATCIRCLE"},
    // nearest doesn't touch on any of the regrid options, only allow defaults
    {"nearest_d2s", "MAP_GREATCIRCLE"},
    {"nearest_d2s", "POLE_ALL"},
    {"nearest_d2s", "POLE_NPNT"},
    {"nearest_d2s", "POLE_TEETH"},
    {"nearest_d2s", "EXTRAP_NEAREST_STOD"},
    {"nearest_d2s", "EXTRAP_NEAREST_IDAVG"},
    {"nearest_d2s", "EXTRAP_NEAREST_D"},
    {"nearest_d2s", "EXTRAP_CREEP"},
    {"nearest_d2s", "EXTRAP_CREEP_NRST_D"},
    {"nearest_d2s", "UNMAPPED_IGNORE"},
    {"nearest_d2s", "IGNORE_DEGENERATE"},
    {"nearest_s2d", "MAP_GREATCIRCLE"},
    {"nearest_s2d", "POLE_ALL"},
    {"nearest_s2d", "POLE_NPNT"},
    {"nearest_s2d", "POLE_TEETH"},
    {"nearest_s2d", "EXTRAP_NEAREST_STOD"},
    {"nearest_s2d", "EXTRAP_NEAREST_IDAVG"},
    {"nearest_s2d", "EXTRAP_NEAREST_D"},
    {"nearest_s2d", "EXTRAP_CREEP"},
    {"nearest_s2d", "EXTRAP_CREEP_NRST_D"},
    {"nearest_s2d", "UNMAPPED_IGNORE"},
    {"nearest_s2d", "IGNORE_DEGENERATE"},
    // only run pole options with periodic meshes
    {"quad_2d_cart", "POLE_ALL"},
    {"quad_2d_sph_deg", "POLE_ALL"},
    {"quad_2d_sph_rad", "POLE_ALL"},
    {"tri_2d_cart", "POLE_ALL"},
    {"tri_2d_sph_deg", "POLE_ALL"},
    {"tri_2d_sph_rad", "POLE_ALL"},
    {"hex_3d_cart", "POLE_ALL"},
    {"hex_3d_sph_deg", "POLE_ALL"},
    {"hex_3d_sph_rad", "POLE_ALL"},
    {"mix_2d_cart", "POLE_ALL"},
    {"mix_2d_sph_deg", "POLE_ALL"},
    {"mix_2d_sph_rad", "POLE_ALL"},
    {"ngon_2d_cart", "POLE_ALL"},
    {"ngon_2d_sph_deg", "POLE_ALL"},
    {"ngon_2d_sph_rad", "POLE_ALL"},
    {"quad_2d_cart", "POLE_NPNT"},
    {"quad_2d_sph_deg", "POLE_NPNT"},
    {"quad_2d_sph_rad", "POLE_NPNT"},
    {"tri_2d_cart", "POLE_NPNT"},
    {"tri_2d_sph_deg", "POLE_NPNT"},
    {"tri_2d_sph_rad", "POLE_NPNT"},
    {"hex_3d_cart", "POLE_NPNT"},
    {"hex_3d_sph_deg", "POLE_NPNT"},
    {"hex_3d_sph_rad", "POLE_NPNT"},
    {"mix_2d_cart", "POLE_NPNT"},
    {"mix_2d_sph_deg", "POLE_NPNT"},
    {"mix_2d_sph_rad", "POLE_NPNT"},
    {"ngon_2d_cart", "POLE_NPNT"},
    {"ngon_2d_sph_deg", "POLE_NPNT"},
    {"ngon_2d_sph_rad", "POLE_NPNT"},
    {"quad_2d_cart", "POLE_TEETH"},
    {"quad_2d_sph_deg", "POLE_TEETH"},
    {"quad_2d_sph_rad", "POLE_TEETH"},
    {"tri_2d_cart", "POLE_TEETH"},
    {"tri_2d_sph_deg", "POLE_TEETH"},
    {"tri_2d_sph_rad", "POLE_TEETH"},
    {"hex_3d_cart", "POLE_TEETH"},
    {"hex_3d_sph_deg", "POLE_TEETH"},
    {"hex_3d_sph_rad", "POLE_TEETH"},
    {"mix_2d_cart", "POLE_TEETH"},
    {"mix_2d_sph_deg", "POLE_TEETH"},
    {"mix_2d_sph_rad", "POLE_TEETH"},
    {"ngon_2d_cart", "POLE_TEETH"},
    {"ngon_2d_sph_deg", "POLE_TEETH"},
    {"ngon_2d_sph_rad", "POLE_TEETH"},
  };


  if (mbmesh) {
    for (const auto api: test_apis_mbmesh) {
      for (const auto mesh: test_meshes) {
        for (const auto mt: test_regrid_maptype) {
          for (const auto nt: test_regrid_normtype) {
            for (const auto pt: test_regrid_poletype) {
              for (const auto em: test_regrid_extrapmethod) {
                for (const auto ua: test_regrid_unmappedaction) {
                  for (const auto id: test_regrid_ignoredegenerate) {
                    auto skip_itr_common = std::find_if(skip_test_common.begin(), 
                                                 skip_test_common.end(), 
                                                 FindAnyPair(api, mesh,
                                                          mt, nt, pt,
                                                          em, ua, id));
                    
                    auto skip_itr_mbmesh = std::find_if(skip_test_mbmesh.begin(), 
                                                 skip_test_mbmesh.end(), 
                                                 FindAnyPair(api, mesh,
                                                          mt, nt, pt,
                                                          em, ua, id));
                    
                    if ((skip_itr_common != skip_test_common.end()) ||
                        (skip_itr_mbmesh != skip_test_mbmesh.end())) {
                      continue;
                    } else {
                      combine(api, mesh, "MBMesh", mt, nt, pt, em, ua, id);
                      // only print one tag per test (so only on root)
                      if (localPet == 0) tagfile << nex_test_tag;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  if (native) {
    for (const auto api: test_apis_native) {
      for (const auto mesh: test_meshes) {
        for (const auto mt: test_regrid_maptype) {
          for (const auto nt: test_regrid_normtype) {
            for (const auto pt: test_regrid_poletype) {
              for (const auto em: test_regrid_extrapmethod) {
                for (const auto ua: test_regrid_unmappedaction) {
                  for (const auto id: test_regrid_ignoredegenerate) {
                    auto skip_itr_common = std::find_if(skip_test_common.begin(), 
                                                 skip_test_common.end(), 
                                                 FindAnyPair(api, mesh,
                                                          mt, nt, pt,
                                                          em, ua, id));

                    auto skip_itr_native = std::find_if(skip_test_native.begin(), 
                                                 skip_test_native.end(), 
                                                 FindAnyPair(api, mesh,
                                                          mt, nt, pt,
                                                          em, ua, id));
                    
                    if ((skip_itr_common != skip_test_common.end()) ||
                        (skip_itr_native != skip_test_native.end())) {
                      continue;
                    } else {
                      combine(api, mesh, "Native", mt, nt, pt, em, ua, id);
                      // only print one tag per test (so only on root)
                      if (localPet == 0) tagfile << nex_test_tag;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  tagfile.close();

  //----------------------------------------------------------------------------
  ESMC_TestEnd("ESMC_MeshCapRegridGenUTest.C", __LINE__, 0);

  return 0;
}
