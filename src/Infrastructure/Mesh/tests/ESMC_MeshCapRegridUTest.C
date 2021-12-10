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

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>
#include <map>
#include <functional>
#include <algorithm> //find_if

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
    
    std::string name = nativeormb + " Mesh - " + api + dash + mesh +
                       dash + maptype + dash + normtype + dash + poletype +
                       dash + extrapmethod + dash + unmappedaction + 
                       dash + ignoredegenerate;

#if defined ESMF_MOAB
      try {
        MCT *test = generate->mesh_map[mesh](localrc);
        
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
#define ESMC_METHOD "MBMeshRegidUTest::main()"

  std::string failMsg = "FAIL";
  // break this up so it doesn't count as an additional test
  std::string nex_test_tag = "//NEX_";
  nex_test_tag.append("UTest\n");
  int result = 0;
  int rc, localrc;
  int localPet, petCount;
  ESMC_VM vm;

  bool mbmesh = true;
  bool native = true; 

  //----------------------------------------------------------------------------
  ESMC_TestStart("ESMC_MeshCapRegridProxyUTest.C", __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);

  std::ofstream tagfile;
  tagfile.open("ESMC_MeshCapRegridProxyUTest.C", std::ios_base::app);

  std::vector<std::string> test_apis_mbmesh;
    test_apis_mbmesh.push_back("bilinear_center");
    test_apis_mbmesh.push_back("bilinear_corner");
    test_apis_mbmesh.push_back("conserve_center");
    // Not yet available with MBMesh
    // test_apis_mbmesh.push_back("conserve_2nd_center");
    test_apis_mbmesh.push_back("nearest_d2s");
    test_apis_mbmesh.push_back("nearest_s2d");
    // Not yet available with MBMesh
    // test_apis_mbmesh.push_back("patch_center");
    // test_apis_mbmesh.push_back("patch_corner");

  std::vector<std::string> test_apis_native;
    test_apis_native.push_back("bilinear_center");
    test_apis_native.push_back("bilinear_corner");
    test_apis_native.push_back("conserve_center");
    test_apis_native.push_back("conserve_2nd_center");
    test_apis_native.push_back("nearest_d2s");
    test_apis_native.push_back("nearest_s2d");
    test_apis_native.push_back("patch_center");
    test_apis_native.push_back("patch_corner");

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

  std::vector<std::pair<std::string, std::string>> skip_test_mbmesh = {\
    // conserve doesn't work with ngons
    {"conserve_center", "ngon_2d_cart"},
    {"conserve_center", "ngon_2d_sph_deg"},
    {"conserve_center", "ngon_2d_sph_rad"},
    // // conserve_2nd doesn't work in 3d, but not yet enabled for mbmesh
    // {"conserve_2nd", "hex_3d_cart"},
    // {"conserve_2nd", "hex_3d_sph_deg"},
    // {"conserve_2nd", "hex_3d_sph_rad"},
  };

  std::vector<std::pair<std::string, std::string>> skip_test_native = {\
    // node coord counts on PET 1 are off with native
    {"patch_center", "hex_3d_cart"},
    {"patch_corner", "hex_3d_cart"},
    // conserve_2nd doesn't work in 3d
    {"conserve_2nd_center", "hex_3d_cart"},
    {"conserve_2nd_center", "hex_3d_sph_deg"},
    {"conserve_2nd_center", "hex_3d_sph_rad"},
    // meshes with ngons return incorrect element count with native
    {"bilinear_center", "ngon_2d_cart"},
    {"bilinear_center", "ngon_2d_sph_deg"},
    {"bilinear_center", "ngon_2d_sph_rad"},
    {"bilinear_corner", "ngon_2d_cart"},
    {"bilinear_corner", "ngon_2d_sph_deg"},
    {"bilinear_corner", "ngon_2d_sph_rad"},
    {"conserve_center", "ngon_2d_cart"},
    {"conserve_center", "ngon_2d_sph_deg"},
    {"conserve_center", "ngon_2d_sph_rad"},
    {"conserve_2nd_center", "ngon_2d_cart"},
    {"conserve_2nd_center", "ngon_2d_sph_deg"},
    {"conserve_2nd_center", "ngon_2d_sph_rad"},
    {"patch_center", "ngon_2d_cart"},
    {"patch_center", "ngon_2d_sph_deg"},
    {"patch_center", "ngon_2d_sph_rad"},
    {"patch_corner", "ngon_2d_cart"},
    {"patch_corner", "ngon_2d_sph_deg"},
    {"patch_corner", "ngon_2d_sph_rad"},
  };

  std::vector<std::pair<std::string, std::string>> skip_test_common = {\
    // the following are exceptions due to design, do not represent limitations per se
    // conservative only works with great circles
    {"conserve_center", "MAP_CARTAPPROX"},
    {"conserve_2nd_center", "MAP_CARTAPPROX"},
    // cartesian only works with straight lines
    {"quad_2d_cart", "MAP_GREATCIRCLE"},
    {"tri_2d_cart", "MAP_GREATCIRCLE"},
    {"hex_3d_cart", "MAP_GREATCIRCLE"},
    {"mix_2d_cart", "MAP_GREATCIRCLE"},
    {"ngon_2d_cart", "MAP_GREATCIRCLE"},
    // nearest doesn't touch on any of the regrid options, only allow defaults
    {"nearest_d2s", "MAP_GREATCIRCLE"},
    {"nearest_s2d", "MAP_GREATCIRCLE"},
    {"nearest_d2s", "POLE_ALL"},
    {"nearest_s2d", "POLE_ALL"},
    {"nearest_d2s", "POLE_NPNT"},
    {"nearest_s2d", "POLE_NPNT"},
    {"nearest_d2s", "POLE_TEETH"},
    {"nearest_s2d", "POLE_TEETH"},
    {"nearest_d2s", "EXTRAP_NEAREST_STOD"},
    {"nearest_s2d", "EXTRAP_NEAREST_STOD"},
    {"nearest_d2s", "EXTRAP_NEAREST_IDAVG"},
    {"nearest_s2d", "EXTRAP_NEAREST_IDAVG"},
    {"nearest_d2s", "EXTRAP_NEAREST_D"},
    {"nearest_s2d", "EXTRAP_NEAREST_D"},
    {"nearest_d2s", "EXTRAP_CREEP"},
    {"nearest_s2d", "EXTRAP_CREEP"},
    {"nearest_d2s", "EXTRAP_CREEP_NRST_D"},
    {"nearest_s2d", "EXTRAP_CREEP_NRST_D"},
    {"nearest_d2s", "UNMAPPED_IGNORE"},
    {"nearest_s2d", "UNMAPPED_IGNORE"},
    {"nearest_d2s", "IGNORE_DEGENERATE"},
    {"nearest_s2d", "IGNORE_DEGENERATE"},
  };



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
    // test_regrid_poletype.push_back("POLE_ALL");
    // test_regrid_poletype.push_back("POLE_NPNT");
    // test_regrid_poletype.push_back("POLE_TEETH");
  
  // all methods except nn, but only for meshes with destination larger than source
  std::vector<std::string> test_regrid_extrapmethod;
    test_regrid_extrapmethod.push_back("EXTRAP_NONE");
    // test_regrid_extrapmethod.push_back("EXTRAP_NEAREST_STOD");
    // test_regrid_extrapmethod.push_back("EXTRAP_NEAREST_IDAVG");
    // test_regrid_extrapmethod.push_back("EXTRAP_NEAREST_D");
    // test_regrid_extrapmethod.push_back("EXTRAP_CREEP");
    // test_regrid_extrapmethod.push_back("EXTRAP_CREEP_NRST_D");
  
  // all methods except nn, but only for meshes with destination larger than source
  std::vector<std::string> test_regrid_unmappedaction;
    // test_regrid_unmappedaction.push_back("UNMAPPED_THROWERROR");
    test_regrid_unmappedaction.push_back("UNMAPPED_IGNORE");
  
  // all methods except nn, but only meshes with degenerate, clockwise or concave elements
  std::vector<std::string> test_regrid_ignoredegenerate;
    test_regrid_ignoredegenerate.push_back("DONOT_IGNORE_DEGENERATE");
    test_regrid_ignoredegenerate.push_back("IGNORE_DEGENERATE");
  
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
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}



