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
#include "ESMCI_MBMesh_Dual.h"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>
#include <random>
// #include <functional>

#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

// test base class ideas
// - almost equal function
// - vector comparison with IWeights

template<class T>
typename std::enable_if<!std::numeric_limits<T>::is_integer, bool>::type
    almost_equal(T x, T y, int ulp=15)
{
    // the machine epsilon has to be scaled to the magnitude of the values used
    // and multiplied by the desired precision in ULPs (units in the last place)
    return std::fabs(x-y) <= std::numeric_limits<T>::epsilon() * std::fabs(x+y) * ulp
        // unless the result is subnormal
        || std::fabs(x-y) < std::numeric_limits<T>::min();
}

// use when pointer *rc is returned
#define CATCH_MBMESHTEST_RETURN_NULL(rc) \
  catch(int localrc){ \
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return NULL; \
  } catch(std::string errstr){ \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, errstr, ESMC_CONTEXT, rc); \
    return NULL; \
  } catch (ESMCI::esmc_error &exc) { \
    ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return NULL; \
  } catch (std::exception &exc) { \
    if (exc.what()) { \
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, exc.what(), ESMC_CONTEXT, rc); \
    } else { \
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, "Unknown exception", ESMC_CONTEXT, rc); \
    } \
    return NULL; \
  } catch(...) { \
    std::string msg; \
    msg = "Unknown exception"; \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL, msg, ESMC_CONTEXT, rc); \
    return NULL; }

// for MBMesh routines, when integer is returned
#define CATCH_MBMESHTEST_RETURN_RC(rc) \
  catch(int localrc){ \
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return (*rc); \
  } catch(std::string errstr){ \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, errstr, ESMC_CONTEXT, rc); \
    return (*rc); \
  } catch (ESMCI::esmc_error &exc) { \
    ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return (*rc); \
  } catch (std::exception &exc) { \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return (*rc); \
  } catch(...) { \
    std::string msg; \
    msg = "Unknown exception"; \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL, msg, ESMC_CONTEXT, rc); \
    return (*rc); }


class MBMeshTest {
  public:
    int verbosity = 0;
    double tol = 1.e-15;
    
    MBMesh *mesh= nullptr;
    MBMesh *target = nullptr;
    std::string name = "Mesh";

    // integer values
    int pdim;
    int sdim;
    int orig_sdim;
    ESMC_CoordSys_Flag coord_sys;
    int num_node;
    int num_elem;
    int num_elem_conn;

    // present variables
    int node_mask_present;
    int elem_area_present;
    int elem_coord_present;
    int elem_mask_present;

    // node information
    std::vector<int> nodeId;
    std::vector<int> nodeMask;
    std::vector<int> nodeOwner;
    std::vector<double> nodeCoord;

    // element information
    std::vector<int> elemConn;
    std::vector<int> elemId;
    std::vector<int> elemMask;
    std::vector<int> elemType;
    std::vector<double> elemArea;
    std::vector<double> elemCoord;

    // output variables
    std::string pass = "Pass - ";
    std::string fail = "Fail - ";

    // for redist
    int num_elem_gids;
    int num_node_gids;
    std::vector<int> elem_gids;
    std::vector<int> node_gids;

    int redist_num_node;
    int redist_num_elem;
    int redist_num_elem_conn;
    std::vector<int> redist_nodeId;
    std::vector<int> redist_nodeMask;
    std::vector<int> redist_nodeOwner;
    std::vector<double> redist_nodeCoord;
    std::vector<int> redist_elemConn;
    std::vector<int> redist_elemId;
    std::vector<int> redist_elemMask;
    std::vector<int> redist_elemType;
    std::vector<double> redist_elemArea;
    std::vector<double> redist_elemCoord;

    std::vector<int> redist_nodeId_in;
    std::vector<int> redist_elemId_in;

    std::map<std::string, std::function<int(void)>>  function_map;

    MBMeshTest(int _pdim, int _sdim, ESMC_CoordSys_Flag _coord_sys, int _num_node, int _num_elem, int _num_elem_conn, int _redist_num_node, int _redist_num_elem, int _redist_num_elem_conn) {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest()"
      try {
        mesh = new MBMesh();

        pdim = _pdim;
        sdim = _sdim;
        orig_sdim = _sdim;
        num_node = _num_node;
        num_elem = _num_elem;
        num_elem_conn = _num_elem_conn;
        redist_num_node = _redist_num_node;
        redist_num_elem = _redist_num_elem;
        redist_num_elem_conn = _redist_num_elem_conn;
        coord_sys = _coord_sys;
        
        nodeId.reserve(_num_node);
        nodeCoord.reserve(_num_node*_sdim);
        nodeOwner.reserve(_num_node);
        elemId.reserve(_num_elem);
        elemType.reserve(_num_elem);
        elemConn.reserve(_num_elem_conn);
        elemCoord.reserve(_num_elem*_sdim);
        
        nodeMask.reserve(_num_node);
        elemMask.reserve(_num_elem);
        elemArea.reserve(_num_elem);

        // generate random numbers to fill mask and area vectors
        std::uniform_int_distribution<int> distribution_int(0, 42);
        std::uniform_real_distribution<double> distribution_real(0.0f, 42.0f);
        std::mt19937 engine; // Mersenne twister MT19937
        auto generator_int = std::bind(distribution_int, engine);
        auto generator_real = std::bind(distribution_real, engine);

        std::generate_n(nodeMask.begin(), _num_node, generator_int);
        std::generate_n(elemMask.begin(), _num_elem, generator_int);
        std::generate_n(elemArea.begin(), _num_elem, generator_real);

        redist_num_node = _redist_num_node;
        redist_num_elem = _redist_num_elem;
        redist_num_elem_conn = _redist_num_elem_conn;
        if (_redist_num_node > 0) {
          redist_nodeId.reserve(_redist_num_node);
          redist_nodeCoord.reserve(_redist_num_node*_sdim);
          redist_nodeOwner.reserve(_redist_num_node);
          redist_elemId.reserve(_redist_num_elem);
          redist_elemType.reserve(_redist_num_elem);
          redist_elemConn.reserve(_redist_num_elem_conn);
          redist_elemCoord.reserve(_redist_num_elem*_sdim);
          
          redist_nodeMask.reserve(_redist_num_node);
          redist_elemMask.reserve(_redist_num_elem);
          redist_elemArea.reserve(_redist_num_elem);

          std::generate_n(redist_nodeMask.begin(), _redist_num_node, generator_int);
          std::generate_n(redist_elemMask.begin(), _redist_num_elem, generator_int);
          std::generate_n(redist_elemArea.begin(), _redist_num_elem, generator_real);
        }
        
        function_map["get"] = std::bind(&MBMeshTest::get, this);
        function_map["dual"] = std::bind(&MBMeshTest::dual, this);
        function_map["elem_redist"] = std::bind(&MBMeshTest::elem_redist, this);
        function_map["node_redist"] = std::bind(&MBMeshTest::node_redist, this);
        function_map["elno_redist"] = std::bind(&MBMeshTest::elno_redist, this);
        function_map["to_pointlist"] = std::bind(&MBMeshTest::to_pointlist, this);
        function_map["write_vtk"] = std::bind(&MBMeshTest::write_vtk, this);

      }
      CATCH_MBMESH_RETHROW
    }

    ~MBMeshTest(){
      if (mesh) delete mesh;
      if (target) delete target;
    }
    
    int build() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::build()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {

        // this->print();
        node_mask_present = true;
        elem_area_present = true;
        elem_coord_present = true;
        elem_mask_present = true;

        InterArray<int> *iin = new InterArray<int>(nodeMask.data(),num_node);
        InterArray<int> *iie = new InterArray<int>(elemMask.data(),num_elem);

        int localrc;
        MBMesh_create(&mesh, &pdim, &sdim, &coord_sys, &localrc);
        ESMC_CHECK_THROW(localrc);

        MBMesh_addnodes(&mesh, &num_node, nodeId.data(), nodeCoord.data(), 
                        nodeOwner.data(), iin, &coord_sys, &orig_sdim, &localrc);
        ESMC_CHECK_THROW(localrc);

        int regridconserve = 0;
        MBMesh_addelements(&mesh, &num_elem, elemId.data(), elemType.data(), iie,
                          &elem_area_present, elemArea.data(),
                          &elem_coord_present, elemCoord.data(),
                          &num_elem_conn, elemConn.data(),
                          &regridconserve,
                          &coord_sys, &orig_sdim, &localrc);
        ESMC_CHECK_THROW(localrc);

        delete iin;
        delete iie;

      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int dual() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::dual()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;

        // skip redist if run on one processor
        if (redist_num_node == 0)
          return ESMF_SUCCESS;
          
        int ne = redist_elemId.size();

        MBMeshDual(mesh, &target, &localrc);
        ESMC_CHECK_THROW(localrc);

        test_redist_info(false, false);
      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int elem_redist() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::elem_redist()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;

        // skip redist if run on one processor
        if (redist_num_node == 0)
          return ESMF_SUCCESS;
          
        int ne = redist_elemId.size();

        MBMesh_createredistelems(&mesh,
                                 &ne, redist_elemId.data(), &target, &localrc);
        ESMC_CHECK_THROW(localrc);

        test_redist_info(true, false);
      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int node_redist() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::node_redist()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        // skip redist if run on one processor
        if (redist_num_node == 0)
          return ESMF_SUCCESS;
          
        int nn = redist_nodeId.size();
        
        MBMesh_createredistnodes(&mesh, &nn, redist_nodeId.data(), 
                                 &target, &localrc);
        ESMC_CHECK_THROW(localrc);

        test_redist_info(false, true);
      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int elno_redist() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::elno_redist()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        // skip redist if run on one processor
        if (redist_num_node == 0)
          return ESMF_SUCCESS;
          
        int nn = redist_nodeId.size();
        int ne = redist_elemId.size();
        
        MBMesh_createredist(&mesh, &nn, redist_nodeId.data(), 
                            &ne, redist_elemId.data(), &target, &localrc);
        ESMC_CHECK_THROW(localrc);

        test_redist_info(true, true);
      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int to_pointlist() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::to_pointlist()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        Throw () << "to_pointlist test not yet implemented.";
      }
      CATCH_MBMESH_RETHROW
    }


    int write_vtk() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::write_vtk()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        int len = name.length();
        char fname[len];
        sprintf(fname, "%s_%d", name.c_str(), Par::Rank());
        MBMesh_write(&mesh, fname, &localrc, len);
        ESMC_CHECK_THROW(localrc);
      }
      CATCH_MBMESH_RETHROW
    }

    int print() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::print()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        std::cout << "MBMeshTest Print:"<< std::endl;
        std::cout << "pdim" << pdim << std::endl;
        std::cout << "sdim"<< sdim << std::endl;
        std::cout << "orig_sdim"<< orig_sdim << std::endl;
        std::cout << "num_elem"<< num_elem << std::endl;
        std::cout << "num_node"<< num_node << std::endl;
        std::cout << "num_elem_conn"<< num_elem_conn << std::endl;
        std::cout << "coord_sys"<< coord_sys << std::endl;
        std::cout << "nodeId"<< std::endl;
        for (const auto i: nodeId)
          std::cout << i << ' ';
        std::cout << std::endl << "nodeMask"<< std::endl;
        for (const auto i: nodeMask)
          std::cout << i << ' ';
        std::cout << std::endl << "nodeOwner"<< std::endl;
        for (const auto i: nodeOwner)
          std::cout << i << ' ';
        std::cout << std::endl << "nodeCoord"<< std::endl;
        for (const auto i: nodeCoord)
          std::cout << i << ' ';
        std::cout << std::endl << "elemConn"<< std::endl;
        for (const auto i: elemConn)
          std::cout << i << ' ';
        std::cout << std::endl << "elemId"<< std::endl;
        for (const auto i: elemId)
          std::cout << i << ' ';
        std::cout << std::endl << "elemMask"<< std::endl;
        for (const auto i: elemMask)
          std::cout << i << ' ';
        std::cout << std::endl << "elemType"<< std::endl;
        for (const auto i: elemType)
          std::cout << i << ' ';
        std::cout << std::endl << "elemArea"<< std::endl;
        for (const auto i: elemArea)
          std::cout << i << ' ';
        std::cout << std::endl << "elemCoord"<< std::endl;
        for (const auto i: elemCoord)
          std::cout << i << ' ';
        std::cout << std::endl;

        // std::cout << "num_elem_gids"<< std::endl;
        // std::cout << "num_node_gids"<< std::endl;
        // std::cout << "elem_gids"<< std::endl;
        // for (const auto i: elem_gids)
        //   std::cout << i << ' ';
        // std::cout << std::endl << "node_gids"<< std::endl;
        // for (const auto i: node_gids)
        //   std::cout << i << ' ';
      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }


    int get(){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::test_get_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      bool fail_print, print;
      int localrc;

      std::string test;

      // VM info
      int localPet = VM::getCurrent(&localrc)->getLocalPet();
      ESMC_CHECK_THROW(localrc);

      // get dimensions
      int local_pdim, local_sdim;
      MBMesh_GetDimensions(mesh, &local_sdim, &local_pdim, &localrc);
      ESMC_CHECK_THROW(localrc);

      if (local_pdim != pdim) {
        std::cout << localPet << "# " << "pdim = " << local_pdim 
                  << " (correct = " << pdim << ")" << std::endl;
        correct = false;
      }

      if (local_sdim != sdim) {
        std::cout << localPet << "# " << "sdim = " << local_sdim 
                  << " (correct = " << sdim << ")" << std::endl;
        correct = false;
      }

      if (verbosity >= 2) {
        std::cout << localPet << "# " << "pdim = " << pdim
                  << " sdim = " << sdim << std::endl;
      }

      int nodeCount;
      MBMesh_GetNodeCount(mesh, &nodeCount, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      int elemCount;
      MBMesh_GetElemCount(mesh, &elemCount, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      int elemConnCount;
      MBMesh_GetElemConnCount(mesh, &elemConnCount, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      if (nodeCount != num_node) {
        std::cout << localPet << "# " << "nodeCount = " << nodeCount
                  << " (correct = " << num_node << ")" << std::endl;
        correct = false;
      }
    
      if (elemCount != num_elem) {
        std::cout << localPet << "# " << "elemCount = " << elemCount
                  << " (correct = " << num_elem << ")" << std::endl;
        correct = false;
      }
    
      if (elemConnCount != num_elem_conn) {
        std::cout << localPet << "# " << "elemConnCount = " << elemConnCount
                  << " (correct = " << num_elem_conn << ")" << std::endl;
        correct = false;
      }
      

      if (verbosity >= 2) {
        std::cout << localPet << "# " << "nodeCount = " << nodeCount
                  << " elemCount = " << elemCount << std::endl;
      }

      int elemMaskIsPresent, elemAreaIsPresent, elemCoordIsPresent;
      MBMesh_GetElemInfoPresence(mesh, &elemMaskIsPresent, &elemAreaIsPresent, &elemCoordIsPresent, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      if (elemMaskIsPresent!=elem_mask_present) {
        std::cout << localPet << "# " << "elemMaskIsPresent = " << elemMaskIsPresent
                  << " (correct = " << elem_mask_present << ")" << std::endl;
        correct = false;
      } 
      if (elemAreaIsPresent!=elem_area_present) {
        std::cout << localPet << "# " << "elemAreaIsPresent = " << elemAreaIsPresent
                  << " (correct = " << elem_area_present << ")" << std::endl;
        correct = false;
      } 
      if (elemCoordIsPresent!=elem_coord_present) {
        std::cout << localPet << "# " << "elemCoordIsPresent = " << elemCoordIsPresent
                  << " (correct = " << elem_coord_present << ")" << std::endl;
        correct = false;
      }
      
      if (verbosity >= 2) {
        std::cout << localPet << "# " << "elemMaskIsPresent = " << elemMaskIsPresent
                  << std::endl;
        std::cout << localPet << "# " << "elemAreaIsPresent = " << elemAreaIsPresent
                  << std::endl;
        std::cout << localPet << "# " << "elemCoordIsPresent = " << elemCoordIsPresent
                  << std::endl;
      }

      int nodeMaskIsPresent;
      MBMesh_GetNodeInfoPresence(mesh, &nodeMaskIsPresent, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      if (nodeMaskIsPresent!=node_mask_present) {
        std::cout << localPet << "# " << "nodeMaskIsPresent = " << nodeMaskIsPresent
                  << " (correct = " << node_mask_present << ")" << std::endl;
        correct = false;
      }
    
      if (verbosity >= 2) {
        std::cout << localPet << "# " << "nodeMaskIsPresent = " << nodeMaskIsPresent
                  << std::endl;
      }

      localrc = test_get_node_info();
      ESMC_CHECK_THROW(localrc);

      localrc = test_get_elem_info();
      ESMC_CHECK_THROW(localrc);

      localrc = test_get_elem_conn_info();
      ESMC_CHECK_THROW(localrc);

      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }

    int test_get_node_info(){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::test_get_node_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      bool fail_print, print;
      int localrc;

      std::string test;

      // VM info
      int localPet = VM::getCurrent(&localrc)->getLocalPet();
      ESMC_CHECK_THROW(localrc);

      // get dimensions
      int local_pdim, local_sdim;
      MBMesh_GetDimensions(mesh, &local_sdim, &local_pdim, &localrc);
      ESMC_CHECK_THROW(localrc);

      // /////////////////// node create info ////////////////////////////
    
      int nodeIds[num_node];
      InterArray<int> *nii = new InterArray<int>(nodeIds,num_node);
      double nodeCoords[num_node*orig_sdim];
      InterArray<double> *nci = new InterArray<double>(nodeCoords,num_node*orig_sdim);
      int nodeMask[num_node];
      InterArray<int> *nmi = new InterArray<int>(nodeMask,num_node);
      int nodeOwners[num_node];
      InterArray<int> *noi = new InterArray<int>(nodeOwners,num_node);
      
      MBMesh_GetNodeCreateInfo(mesh, nii, NULL, NULL, NULL, &localrc);
      ESMC_CHECK_THROW(localrc);

      test = "nodeId";
      fail_print = false;
      for (int i=0; i<nii->extent[0]; ++i) {
        print = false;
        if (nii->array[i] != nodeId[i]) {
          correct = false;
          print = true;
          fail_print = true;
        }
        if (print && verbosity >= 3)
          std::cout << localPet << "# " << "node_id[" << i << "] = " 
                    << nii->array[i] << " (correct = " << nodeId[i] << ")"
                    << std::endl;
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      MBMesh_GetNodeCreateInfo(mesh, NULL, nci, NULL, NULL, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      test = "nodeCoord";
      fail_print = false;
      for (int i=0; i<nci->extent[0]; ++i) {
        print = false;
        if (!almost_equal(nci->array[i], nodeCoord[i])) {
          correct = false;
          print = true;
          fail_print = true;
        }
        if (print && verbosity >= 3)
          std::cout << localPet << "# " << "node_coord[" << i << "] = " 
                    << std::setprecision(16) << nci->array[i] << " (correct = " 
                    << std::setprecision(16) << nodeCoord[i] << ")"
                    << std::endl;
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      MBMesh_GetNodeCreateInfo(mesh, NULL, NULL, noi, NULL, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      test = "nodeOwner";
      fail_print = false;
      for (int i=0; i<noi->extent[0]; ++i) {
        print = false;
        if (noi->array[i] != nodeOwner[i]) {
          correct = false;
          print = true;
          fail_print = true;
        }
        if (print && verbosity >= 3)
          std::cout << localPet << "# " << "node_owner[" << i << "] = " 
                    << noi->array[i] << " (correct = " << nodeOwner[i] << ")"
                    << std::endl;
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      if (node_mask_present) {
        MBMesh_GetNodeCreateInfo(mesh, NULL, NULL, NULL, nmi, &localrc);
        ESMC_CHECK_THROW(localrc);
      
        test = "nodeMask";
        fail_print = false;
        for (int i=0; i<nmi->extent[0]; ++i) {
          print = false;
          if (nmi->array[i] != nodeMask[i]) {
            correct = false;
            print = true;
            fail_print = true;
          }
          if (print && verbosity >= 3)
            std::cout << localPet << "# " << "node_mask[" << i << "] = " 
                      << nmi->array[i] << " (correct = " << nodeMask[i] << ")"
                      << std::endl;
        }
        if (verbosity >= 1) {
          if (!fail_print) std::cout<< pass << test << std::endl;
          else std::cout << fail << test << std::endl;
        }
      }

      delete nii, nci, noi;
      if (node_mask_present) delete nmi;

      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }

    int test_get_elem_info(){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::test_get_elem_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      bool fail_print, print;
      int localrc;

      std::string test;

      // VM info
      int localPet = VM::getCurrent(&localrc)->getLocalPet();
      ESMC_CHECK_THROW(localrc);

      // get dimensions
      int local_pdim, local_sdim;
      MBMesh_GetDimensions(mesh, &local_sdim, &local_pdim, &localrc);
      ESMC_CHECK_THROW(localrc);

      /////////////////// elem create info ////////////////////////////
    
      int elemIds[num_elem];
      InterArray<int> *eii = new InterArray<int>(elemIds,num_elem);
      int elemTypes[num_elem];
      InterArray<int> *eti = new InterArray<int>(elemTypes,num_elem);
      int elemMask[num_elem];
      InterArray<int> *emi = new InterArray<int>(elemMask,num_elem);
      double elemArea[num_elem];
      InterArray<double> *eai = new InterArray<double>(elemArea,num_elem);
      double elemCoord[num_elem*orig_sdim];
      InterArray<double> *eci = new InterArray<double>(elemCoord,num_elem*orig_sdim);
    
      MBMesh_GetElemCreateInfo(mesh, eii, NULL, NULL, NULL, NULL, NULL, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      test = "ElemId";
      fail_print = false;
      for (int i=0; i<eii->extent[0]; ++i) {
        print = false;
        if (eii->array[i] != elemId[i]) {
          correct = false;
          print = true;
          fail_print = true;
        }
        if (print && verbosity >= 3)
          std::cout << localPet << "# " << "elem_ids[" << i << "] = " 
                    << eii->array[i] << " (correct = " << elemId[i] << ")"
                    << std::endl;
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      MBMesh_GetElemCreateInfo(mesh, NULL, eti, NULL, NULL, NULL, NULL, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      test = "ElemType";
      fail_print = false;
      for (int i=0; i<eti->extent[0]; ++i) {
        print = false;
        if (eti->array[i] != elemType[i]) {
          correct = false;
          print = true;
          fail_print = true;
        }
        if (print && verbosity >= 3)
          std::cout << localPet << "# " << "elem_type[" << i << "] = " 
                    << eti->array[i] << " (correct = " << elemType[i] << ")"
                    << std::endl;
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      if (elem_area_present) {
        MBMesh_GetElemCreateInfo(mesh, NULL, NULL, NULL, NULL, eai, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
      
        test = "ElemArea";
        fail_print = false;
        for (int i=0; i<eai->extent[0]; ++i) {
          print = false;
          if (!almost_equal(eai->array[i], elemArea[i])) {
            correct = false;
            print = true;
            fail_print = true;
          }
          if (print && verbosity >= 3)
            std::cout << localPet << "# " << "elem_area[" << i << "] = " 
                      << std::setprecision(16) << eai->array[i] << " (correct = " 
                      << std::setprecision(16) << elemArea[i] << ")"
                      << std::endl;
        }
        if (verbosity >= 1) {
          if (!fail_print) std::cout<< pass << test << std::endl;
          else std::cout << fail << test << std::endl;
        }
      }
    
      if (elem_coord_present) {
        MBMesh_GetElemCreateInfo(mesh, NULL, NULL, NULL, NULL, NULL, eci, &localrc);
        ESMC_CHECK_THROW(localrc);
      
        test = "ElemCoord";
        fail_print = false;
        for (int i=0; i<eci->extent[0]; ++i) {
          print = false;
          if (!almost_equal(eci->array[i], elemCoord[i])) {
            correct = false;
            print = true;
            fail_print = true;
          }
        if (print && verbosity >= 3)
          std::cout << localPet << "# " << "elem_coord[" << i << "] = " 
                    << std::setprecision(16) << eci->array[i] << " (correct = " 
                    << std::setprecision(16) << elemCoord[i] << ")"
                    << std::endl;
        }
        if (verbosity >= 1) {
          if (!fail_print) std::cout<< pass << test << std::endl;
          else std::cout << fail << test << std::endl;
        }
      }
    
      if (elem_mask_present) {
        MBMesh_GetElemCreateInfo(mesh, NULL, NULL, NULL, emi, NULL, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
      
        test = "ElemMask";
        fail_print = false;
        for (int i=0; i<emi->extent[0]; ++i) {
          print = false;
          if (emi->array[i] != elemMask[i]) {
            correct = false;
            print = true;
            fail_print = true;
          }
          if (print && verbosity >= 3)
            std::cout << localPet << "# " << "elem_mask[" << i << "] = " 
                      << emi->array[i] << " (correct = " << elemMask[i] << ")"
                      << std::endl;
        }
        if (verbosity >= 1) {
          if (!fail_print) std::cout<< pass << test << std::endl;
          else std::cout << fail << test << std::endl;
        }
      }

      delete eii, eti;
      if (elem_area_present) delete eai;
      if (elem_coord_present) delete eci;
      if (elem_mask_present) delete emi;
    
      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }


    int test_get_elem_conn_info(){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::test_get_elem_conn_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      bool fail_print, print;
      int localrc;

      std::string test;

      // VM info
      int localPet = VM::getCurrent(&localrc)->getLocalPet();
      ESMC_CHECK_THROW(localrc);

      // get dimensions
      int local_pdim, local_sdim;
      MBMesh_GetDimensions(mesh, &local_sdim, &local_pdim, &localrc);
      ESMC_CHECK_THROW(localrc);

      /////////////////// elem create info ////////////////////////////
    
      int elemConn[num_elem_conn];
      InterArray<int> *ecni = new InterArray<int>(elemConn,num_elem_conn);
    
      MBMesh_GetElemCreateInfo(mesh, NULL, NULL, ecni, NULL, NULL, NULL, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      test = "ElemConn";
      fail_print = false;
      for (int i=0; i<ecni->extent[0]; ++i) {
        print = false;
        if (ecni->array[i] != elemConn[i]) {
          correct = false;
          print = true;
          fail_print = true;
        }
        if (print && verbosity >= 3)
          std::cout << localPet << "# " << "elem_connectivity[" << i << "] = " 
                    << ecni->array[i] << " (correct = " << elemConn[i] << ")"
                    << std::endl;
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    

      delete ecni;
    
      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }

    int test_redist_info(bool element, bool node){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::test_redist_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      bool fail_print, print;
      int localrc; 

      // VM info
      int localPet = VM::getCurrent(&localrc)->getLocalPet();
      ESMC_CHECK_THROW(localrc);

      // get dimensions
      int local_pdim, local_sdim;
      MBMesh_GetDimensions(target, &local_sdim, &local_pdim, &localrc);
      ESMC_CHECK_THROW(localrc);

      if (local_pdim != pdim) {
        std::cout << localPet << "# " << "pdim = " << local_pdim 
                  << " (correct = " << pdim << ")" << std::endl;
        correct = false;
      }

      if (local_sdim != sdim) {
        std::cout << localPet << "# " << "sdim = " << local_sdim 
                  << " (correct = " << sdim << ")" << std::endl;
        correct = false;
      }

      if (verbosity >= 2) {
        std::cout << localPet << "# " << "pdim = " << pdim
                  << " sdim = " << sdim << std::endl;
      }


      /////////////////// node create info ////////////////////////////
      if (node) {
        int nn = redist_nodeId.size();
        MBMesh_checknodelist(&target, &nn, redist_nodeId.data(), &localrc);
        ESMC_CHECK_THROW(localrc);
      }

      /////////////////// elem create info ////////////////////////////
      if (element) {
        int ne = redist_elemId.size();
        MBMesh_checkelemlist(&target, &ne, redist_elemId.data(), &localrc);
        ESMC_CHECK_THROW(localrc);
      }

      int elemMaskIsPresent, elemAreaIsPresent, elemCoordIsPresent;
      MBMesh_GetElemInfoPresence(target, &elemMaskIsPresent, &elemAreaIsPresent, &elemCoordIsPresent, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      if (elemMaskIsPresent!=elem_mask_present) {
        std::cout << localPet << "# " << "elemMaskIsPresent = " << elemMaskIsPresent
                  << " (correct = " << elem_mask_present << ")" << std::endl;
        correct = false;
      } 
      if (elemAreaIsPresent!=elem_area_present) {
        std::cout << localPet << "# " << "elemAreaIsPresent = " << elemAreaIsPresent
                  << " (correct = " << elem_area_present << ")" << std::endl;
        correct = false;
      } 
      if (elemCoordIsPresent!=elem_coord_present) {
        std::cout << localPet << "# " << "elemCoordIsPresent = " << elemCoordIsPresent
                  << " (correct = " << elem_coord_present << ")" << std::endl;
        correct = false;
      }
      
      if (verbosity >= 2) {
        std::cout << localPet << "# " << "elemMaskIsPresent = " << elemMaskIsPresent
                  << std::endl;
        std::cout << localPet << "# " << "elemAreaIsPresent = " << elemAreaIsPresent
                  << std::endl;
        std::cout << localPet << "# " << "elemCoordIsPresent = " << elemCoordIsPresent
                  << std::endl;
      }

      int nodeMaskIsPresent;
      MBMesh_GetNodeInfoPresence(target, &nodeMaskIsPresent, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      if (nodeMaskIsPresent!=node_mask_present) {
        std::cout << localPet << "# " << "nodeMaskIsPresent = " << nodeMaskIsPresent
                  << " (correct = " << node_mask_present << ")" << std::endl;
        correct = false;
      }
    
      if (verbosity >= 2) {
        std::cout << localPet << "# " << "nodeMaskIsPresent = " << nodeMaskIsPresent
                  << std::endl;
      }

      // RLO: I think we can only reliably and consistently check owned info
      // /////////////////// counts ////////////////////////////
      // int nodeCount;
      // MBMesh_GetNodeCount(target,&nodeCount, &localrc);
      // ESMC_CHECK_THROW(localrc);
      // 
      // int elemCount;
      // MBMesh_GetElemCount(target,&elemCount, &localrc);
      // ESMC_CHECK_THROW(localrc);
      // 
      // int elemConnCount;
      // MBMesh_GetElemConnCount(target,&elemConnCount, &localrc);
      // ESMC_CHECK_THROW(localrc);
    
      // if (verbosity >= 2) {
      //   std::cout << localPet << "# " << "nodeCount = " << nodeCount << "(" << num_node << ")"
      //             << " elemCount = " << elemCount << "(" << num_elem << ")" << std::endl;
      // }
      // 
      // if (nodeCount == num_node) {
      //   localrc = test_get_node_info_redist();
      //   ESMC_CHECK_THROW(localrc);
      // }
      // 
      // if (elemCount == num_elem) {
      //   localrc = test_get_elem_info_redist();
      //   ESMC_CHECK_THROW(localrc);
      // }
      // 
      // if (elemConnCount == num_elem_conn) {
      //   localrc = test_get_elem_conn_info_redist();
      //   ESMC_CHECK_THROW(localrc);
      // }

      }
      CATCH_MBMESHTEST_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }
};

