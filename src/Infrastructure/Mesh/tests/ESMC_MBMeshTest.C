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
// #include "ESMCI_MBMesh_Util.h"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>

#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

// test base class ideas
// - almost equal function
// - vector comparison with IWeights
// - 

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

#define MBMESHTEST_THROW_ERROR(msg) {\
    ESMCI::esmc_error local_macro_error("", ESMC_RC_MOAB_ERROR, msg); \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, local_macro_error.what(), ESMC_CONTEXT, nullptr); \
    throw(local_macro_error);}

#define MBMESHTEST_CHECK_RC_THROW(localrc) \
  if (localrc != ESMF_SUCCESS) {\
    ESMCI::esmc_error local_macro_error("", localrc, ""); \
    if (ESMC_LogDefault.MsgFoundError(localrc, local_macro_error.what(), ESMC_CONTEXT, NULL)) \
      throw(local_macro_error);}

#define MBMESHTEST_CATCH_RETHROW \
  catch (std::exception &exc) {\
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, exc.what(), ESMC_CONTEXT, nullptr)) \
      throw(exc); \
  } catch(...) {\
    std::string msg;\
    msg = "Unknown exception";\
    ESMCI::esmc_error local_macro_error("ESMC_RC_MOAB_ERROR", ESMC_RC_MOAB_ERROR, msg); \
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, msg, ESMC_CONTEXT, nullptr)) \
      throw(local_macro_error); }

// for MBMesh routines, when pointer to object is returned
#define MBMESHTEST_CATCH_RETURN_NULL(rc) \
  catch (std::exception &exc) { \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return NULL; \
  } catch(...) { \
    std::string msg; \
    msg = "Unknown exception"; \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL, msg, ESMC_CONTEXT, rc); \
    return NULL; }

// for MBMesh routines, when integer is returned
#define MBMESHTEST_CATCH_RETURN_RC(rc) \
  catch (std::exception &exc) { \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return ESMF_FAILURE; \
  } catch(...) { \
    std::string msg; \
    msg = "Unknown exception"; \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL, msg, ESMC_CONTEXT, rc); \
    return ESMF_FAILURE; }


class MBMeshTest {
  public:
    MBMesh *mesh;
    // pointer to MBMesh for calls to glue layer routines that still use void*
    void *meshp;
    void *meshp_redist;

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
    bool do_redist;

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


    MBMeshTest(int _pdim, int _sdim, ESMC_CoordSys_Flag _coord_sys, int _num_node, int _num_elem, int _num_elem_conn, bool redist=false, int _redist_num_node=0, int _redist_num_elem=0, int _redist_num_elem_conn=0) {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest()"

      try {
        mesh = new MBMesh();
        meshp = static_cast<void *> (mesh);

        pdim = _pdim;
        sdim = _sdim;
        orig_sdim = _sdim;
        num_node = _num_node;
        num_elem = _num_elem;
        num_elem_conn = _num_elem_conn;
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

        std::generate(nodeMask.begin(), nodeMask.end(), std::rand);
        std::generate(elemMask.begin(), elemMask.end(), std::rand);
        std::generate(elemArea.begin(), elemArea.end(), std::rand);

        do_redist = redist;
        if (redist) {
          redist_num_node = _redist_num_node;
          redist_num_elem = _redist_num_elem;
          redist_num_elem_conn = _redist_num_elem_conn;
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
        }

      } MBMESHTEST_CATCH_RETHROW
    }

    MBMeshTest() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest(default)"

      try {
        mesh = new MBMesh();
        meshp = static_cast<void *> (mesh);

      } MBMESHTEST_CATCH_RETHROW
    }

    int build_mbmesh() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::build_mbmesh()"
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
        MBMesh_create(&meshp, &pdim, &sdim, &coord_sys, &localrc);
        MBMESHTEST_CHECK_RC_THROW(localrc)

        MBMesh_addnodes(&meshp, &num_node, nodeId.data(), nodeCoord.data(), 
                        nodeOwner.data(), iin, &coord_sys, &orig_sdim, &localrc);
        MBMESHTEST_CHECK_RC_THROW(localrc)

        int regridconserve = 0;
        MBMesh_addelements(&meshp, &num_elem, elemId.data(), elemType.data(), iie,
                          &elem_area_present, elemArea.data(),
                          &elem_coord_present, elemCoord.data(),
                          &num_elem_conn, elemConn.data(),
                          &regridconserve,
                          &coord_sys, &orig_sdim, &localrc);
        MBMESHTEST_CHECK_RC_THROW(localrc)

        delete iin;
        delete iie;

      } MBMESHTEST_CATCH_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int mbtRedist(MBMeshTest *mbt) {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::mbtRedist"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {

        if (!do_redist)
          MBMESHTEST_THROW_ERROR("Cannot run redist with 1 core.")
        
        // first copy the information from mbt
        mbt->pdim = pdim;
        mbt->sdim = sdim;
        mbt->orig_sdim = orig_sdim;
        mbt->coord_sys = coord_sys;

        mbt->num_node = redist_num_node;
        mbt->num_elem = redist_num_elem;
        mbt->num_elem_conn = redist_num_elem_conn;
        
        mbt->node_mask_present = node_mask_present;
        mbt->elem_area_present = elem_area_present;
        mbt->elem_coord_present = elem_coord_present;
        mbt->elem_mask_present = elem_mask_present;

        mbt->nodeId.assign(redist_nodeId.begin(), redist_nodeId.end());
        mbt->nodeCoord.assign(redist_nodeCoord.begin(), redist_nodeCoord.end());
        mbt->nodeOwner.assign(redist_nodeOwner.begin(), redist_nodeOwner.end());
        mbt->elemId.assign(redist_elemId.begin(), redist_elemId.end());
        mbt->elemType.assign(redist_elemType.begin(), redist_elemType.end());
        mbt->elemConn.assign(redist_elemConn.begin(), redist_elemConn.end());
        mbt->elemCoord.assign(redist_elemCoord.begin(), redist_elemCoord.end());
        mbt->nodeMask.assign(redist_nodeMask.begin(), redist_nodeMask.end());
        mbt->elemMask.assign(redist_elemMask.begin(), redist_elemMask.end());
        mbt->elemArea.assign(redist_elemArea.begin(), redist_elemArea.end());
        
        // store original meshp in meshp_redist for build_mbmesh_redist
        mbt->meshp_redist = meshp;

      } MBMESHTEST_CATCH_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int build_mbmesh_redist() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::build_mbmesh_redist()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {

        int localrc;
        // MBMesh_createredistnodes(&meshp_redist, num_node, nodeId.data(),
        //                          &meshp, &localrc);
        MBMesh_createredistelems(&meshp_redist, &num_elem, elemId.data(),
                                 &meshp, &localrc);
        // MBMesh_createredist(&meshp_redist, num_node, nodeId.data(), &num_elem, 
        //                     elemId.data(), &meshp, &localrc);
        MBMESHTEST_CHECK_RC_THROW(localrc)

      } MBMESHTEST_CATCH_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
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
        std::cout << "nodeMask"<< std::endl;
        for (const auto i: nodeMask)
          std::cout << i << ' ';
        std::cout << "nodeOwner"<< std::endl;
        for (const auto i: nodeOwner)
          std::cout << i << ' ';
        std::cout << "nodeCoord"<< std::endl;
        for (const auto i: nodeCoord)
          std::cout << i << ' ';
        std::cout << "elemConn"<< std::endl;
        for (const auto i: elemConn)
          std::cout << i << ' ';
        std::cout << "elemId"<< std::endl;
        for (const auto i: elemId)
          std::cout << i << ' ';
        std::cout << "elemMask"<< std::endl;
        for (const auto i: elemMask)
          std::cout << i << ' ';
        std::cout << "elemType"<< std::endl;
        for (const auto i: elemType)
          std::cout << i << ' ';
        std::cout << "elemArea"<< std::endl;
        for (const auto i: elemArea)
          std::cout << i << ' ';
        std::cout << "elemCoord"<< std::endl;
        for (const auto i: elemCoord)
          std::cout << i << ' ';

        if (do_redist) {
          std::cout << "num_elem_gids"<< std::endl;
          std::cout << "num_node_gids"<< std::endl;
          std::cout << "elem_gids"<< std::endl;
          for (const auto i: elem_gids)
            std::cout << i << ' ';
          std::cout << "node_gids"<< std::endl;
          for (const auto i: node_gids)
            std::cout << i << ' ';
        }

      } MBMESHTEST_CATCH_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;

    }


    int test_get_info(int verbosity = 0, double tol = 1.e-15){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::test_get_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      bool fail_print, print;
      int localrc;

      std::string test;

      // get dimensions

      int nodeCount;
      MBMesh_GetNodeCount(meshp, &nodeCount, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
      int elemCount;
      MBMesh_GetElemCount(meshp, &elemCount, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
      int elemConnCount;
      MBMesh_GetElemConnCount(meshp, &elemConnCount, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
      if (nodeCount != num_node) {
        printf("nodeCount = %d (correct = %d)\n", nodeCount, num_node);
        correct = false;
      }
    
      if (elemCount != num_elem) {
        printf("elemCount = %d (correct = %d)\n", elemCount, num_elem);
        correct = false;
      }
    
      if (elemConnCount != num_elem_conn) {
        printf("elemConnCount = %d (correct = %d)\n", elemConnCount, num_elem_conn);
        correct = false;
      }
      

      if (verbosity >= 2) {
        printf("nodeCount = %d\n", nodeCount);
        printf("elemCount = %d\n", elemCount);
      }

      int elemMaskIsPresent, elemAreaIsPresent, elemCoordIsPresent;
      MBMesh_GetElemInfoPresence(meshp, &elemMaskIsPresent, &elemAreaIsPresent, &elemCoordIsPresent, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
      if (elemMaskIsPresent!=elem_mask_present) {
        printf("elemMaskIsPresent = %d (correct = %d)\n", elemMaskIsPresent, elem_mask_present);
        correct = false;
      } 
      if (elemAreaIsPresent!=elem_area_present) {
        printf("elemAreaIsPresent = %d (correct = %d)\n", elemAreaIsPresent, elem_area_present);
        correct = false;
      } 
      if (elemCoordIsPresent!=elem_coord_present) {
        printf("elemCoordIsPresent = %d (correct = %d)\n", elemCoordIsPresent, elem_coord_present);
        correct = false;
      }
      
      if (verbosity >= 2) {
        printf("elemMaskIsPresent = %d\n", elemMaskIsPresent);
        printf("elemAreaIsPresent = %d\n", elemAreaIsPresent);
        printf("elemCoordIsPresent = %d\n", elemCoordIsPresent);
      }

      int nodeMaskIsPresent;
      MBMesh_GetNodeInfoPresence(meshp, &nodeMaskIsPresent, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
      if (nodeMaskIsPresent!=node_mask_present) {
        printf("nodeMaskIsPresent = %d (correct = %d)\n", nodeMaskIsPresent, node_mask_present);
        correct = false;
      }
    
      // /////////////////// node create info ////////////////////////////
    
      int nodeIds[num_node];
      InterArray<int> *nii = new InterArray<int>(nodeIds,num_node);
      double nodeCoords[num_node*orig_sdim];
      InterArray<double> *nci = new InterArray<double>(nodeCoords,num_node*orig_sdim);
      int nodeMask[num_node];
      InterArray<int> *nmi = new InterArray<int>(nodeMask,num_node);
      int nodeOwners[num_node];
      InterArray<int> *noi = new InterArray<int>(nodeOwners,num_node);
      
      MBMesh_GetNodeCreateInfo(meshp, nii, NULL, NULL, NULL, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)

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
          printf("node_id[%d] = %d (correct = %d)\n", i, nii->array[i], nodeId[i]);
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      MBMesh_GetNodeCreateInfo(meshp, NULL, nci, NULL, NULL, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
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
          printf("node_coord[%d] = %.16f (correct = %.16f)\n", i, nci->array[i], nodeCoord[i]);
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      MBMesh_GetNodeCreateInfo(meshp, NULL, NULL, noi, NULL, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
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
          printf("node_owner[%d] = %d (correct = %d)\n", i, noi->array[i], nodeOwner[i]);
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      if (node_mask_present) {
        MBMesh_GetNodeCreateInfo(meshp, NULL, NULL, NULL, nmi, &localrc);
        MBMESHTEST_CHECK_RC_THROW(localrc)
      
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
            printf("node_mask[%d] = %d (correct = %d)\n", i, nmi->array[i],   nodeMask[i]);
        }
        if (verbosity >= 1) {
          if (!fail_print) std::cout<< pass << test << std::endl;
          else std::cout << fail << test << std::endl;
        }
      }
      
      /////////////////// elem create info ////////////////////////////
    
      int elemIds[num_elem];
      InterArray<int> *eii = new InterArray<int>(elemIds,num_elem);
      int elemTypes[num_elem];
      InterArray<int> *eti = new InterArray<int>(elemTypes,num_elem);
      int elemConn[num_elem_conn];
      InterArray<int> *ecni = new InterArray<int>(elemConn,num_elem_conn);
      int elemMask[num_elem];
      InterArray<int> *emi = new InterArray<int>(elemMask,num_elem);
      double elemArea[num_elem];
      InterArray<double> *eai = new InterArray<double>(elemArea,num_elem);
      double elemCoord[num_elem*orig_sdim];
      InterArray<double> *eci = new InterArray<double>(elemCoord,num_elem*orig_sdim);
    
      MBMesh_GetElemCreateInfo(meshp, eii, NULL, NULL, NULL, NULL, NULL, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
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
          printf("elem_ids[%d] = %d (correct = %d)\n", i, eii->array[i], elemId[i]);
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      MBMesh_GetElemCreateInfo(meshp, NULL, eti, NULL, NULL, NULL, NULL, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
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
          printf("elem_type[%d] = %d (correct = %d)\n", i, eti->array[i], elemType[i]);
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      MBMesh_GetElemCreateInfo(meshp, NULL, NULL, ecni, NULL, NULL, NULL, &localrc);
      MBMESHTEST_CHECK_RC_THROW(localrc)
    
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
          printf("elem_connectivity[%d] = %d (correct = %d)\n", i, ecni->array[i], elemConn[i]);
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      if (elem_area_present) {
        MBMesh_GetElemCreateInfo(meshp, NULL, NULL, NULL, NULL, eai, NULL, &localrc);
        MBMESHTEST_CHECK_RC_THROW(localrc)
      
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
            printf("elem_area[%d] = %.16f (correct = %.16f)\n", i, eai->array[i], elemArea[i]);
        }
        if (verbosity >= 1) {
          if (!fail_print) std::cout<< pass << test << std::endl;
          else std::cout << fail << test << std::endl;
        }
      }
    
      if (elem_coord_present) {
        MBMesh_GetElemCreateInfo(meshp, NULL, NULL, NULL, NULL, NULL, eci, &localrc);
        MBMESHTEST_CHECK_RC_THROW(localrc)
      
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
          printf("elem_coords[%d] = %.16f (correct = %.16f)\n", i, eci->array[i], elemCoord[i]);
        }
        if (verbosity >= 1) {
          if (!fail_print) std::cout<< pass << test << std::endl;
          else std::cout << fail << test << std::endl;
        }
      }
    
      if (elem_mask_present) {
        MBMesh_GetElemCreateInfo(meshp, NULL, NULL, NULL, emi, NULL, NULL, &localrc);
        MBMESHTEST_CHECK_RC_THROW(localrc)
      
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
            printf("elem_mask[%d] = %d (correct = %d)\n", i, emi->array[i], elemMask[i]);
        }
        if (verbosity >= 1) {
          if (!fail_print) std::cout<< pass << test << std::endl;
          else std::cout << fail << test << std::endl;
        }
      }
    
      delete nii, nci, noi;
      if (node_mask_present) delete nmi;
    
      delete eii, eti, ecni;
      if (elem_area_present) delete eai;
      if (elem_coord_present) delete eci;
      if (elem_mask_present) delete emi;
    
      } MBMESHTEST_CATCH_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }
};

