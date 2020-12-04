//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
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
#include "ESMC_MBMeshTestGen.C"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>
#include <map>
// #include <functional>

#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

int main(int argc, char *argv[]) {

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

  // these are bound to MBMeshTest in constructor, must match!
  std::vector<std::string> test_functions;
    test_functions.push_back("get");
    test_functions.push_back("elem_redist");
    test_functions.push_back("node_redist");
    test_functions.push_back("elno_redist");
    // test_functions.push_back("to_pointlist");
    test_functions.push_back("write_vtk");

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

  std::map<std::string, std::function<MBMeshTest*(int&)>>  mesh_map =
    {{"quad_2d_cart", quad_2d_cart},
    {"quad_2d_sph", quad_2d_sph},
    {"tri_2d_cart", tri_2d_cart},
    {"tri_2d_sph", tri_2d_sph},
    {"hex_3d_cart", hex_3d_cart},
    {"hex_3d_sph", hex_3d_sph},
    {"mix_2d_cart", mix_2d_cart},
    {"mix_2d_sph", mix_2d_sph},
    {"ngon_2d_cart", ngon_2d_cart},
    {"ngon_2d_sph", ngon_2d_sph}
    };


  for (const auto api: test_functions) {
    for (const auto mesh: test_meshes) {    
      rc = ESMF_FAILURE;
      MBMeshTest *test = mesh_map[mesh](localrc);
      
      // test->verbosity = 0;
      // test->tol = 1.e-15;
      
      if (localrc == ESMF_SUCCESS) localrc = test->build();
      if (localrc == ESMF_SUCCESS) rc = test->function_map[api]();

      std::string name = "MBMesh - " + api + " - " + mesh;
      ESMC_Test(rc==ESMF_SUCCESS, name.c_str(), failMsg.c_str(), 
                &result, __FILE__, __LINE__, 0);

      delete test;
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

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}



