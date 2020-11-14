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
#include "ESMCI_MBMesh_Mapping.h"
#include "ESMCI_MBMesh_Util.h"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>

#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

static double UNINITVAL = -42;

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

#define ESMC_CHECK_RC_THROW(actual_rc) \
  if (actual_rc != ESMF_SUCCESS) {\
    ESMCI::esmc_error local_macro_error("", actual_rc, ""); \
    if (ESMC_LogDefault.MsgFoundError(actual_rc, local_macro_error.what(), ESMC_CONTEXT, nullptr)) \
      throw(local_macro_error);}

#define ESMC_CATCH_MBMESH_TEST_RETHROW \
  catch (std::exception &exc) {\
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, exc.what(), ESMC_CONTEXT, nullptr)) \
      throw(exc); \
  } catch(...) {\
    std::string msg;\
    msg = "Unknown exception";\
    ESMCI::esmc_error local_macro_error("ESMC_RC_MOAB_ERROR", ESMC_RC_MOAB_ERROR, msg); \
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, msg, ESMC_CONTEXT, nullptr)) \
      throw(local_macro_error); }

// for MBMesh routines, when pointer *rc is returned
#define ESMC_CATCH_MBMESH_TEST_RETURN(rc) \
  catch (std::exception &exc) { \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return NULL; \
  } catch(...) { \
    std::string msg; \
    msg = "Unknown exception"; \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL, msg, ESMC_CONTEXT, rc); \
    return NULL; }


class MBMeshTest {
  public:
    MBMesh *mesh;
    // pointer to MBMesh for calls to glue layer routines that still use void*
    void *meshp;

    // integer values
    int pdim;
    int sdim;
    int orig_sdim;
    int num_elem;
    int num_node;
    int num_elem_conn;
    ESMC_CoordSys_Flag coord_sys;

    // present variables
    int node_mask_present;
    int elem_area_present;
    int elem_coord_present;
    int elem_mask_present;

    // node information
    int *nodeId = NULL;
    int *nodeMask = NULL;
    int *nodeOwner = NULL;
    double *nodeCoord = NULL;

    // element information
    int *elemConn = NULL;
    int *elemId = NULL;
    int *elemMask = NULL;
    int *elemType = NULL;
    double *elemArea = NULL;
    double *elemCoord = NULL;

    // output variables
    std::string pass = "Pass - ";
    std::string fail = "Fail - ";


    MBMeshTest(int _pdim, int _sdim, int _num_elem, int _num_node, ESMC_CoordSys_Flag _coord_sys) {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest()"

      try {
        pdim = _pdim;
        sdim = _sdim;
        orig_sdim = _sdim;
        // orig_sdim comes from coordsys and pdim
        num_elem = _num_elem;
        num_node = _num_node;
        coord_sys = _coord_sys;
        
        node_mask_present = false;
        elem_area_present = false;
        elem_coord_present = false;
        elem_mask_present = false;
  
        // allocate the MBMesh object
        mesh = new MBMesh();
        meshp = static_cast<void *> (mesh);
  
        int localrc;
        MBMesh_create(&meshp, &pdim, &sdim, &coord_sys, &localrc);
        ESMC_CHECK_RC_THROW(localrc)

      } ESMC_CATCH_MBMESH_TEST_RETHROW
    }

    ~MBMeshTest(){
      delete [] nodeId;
      delete [] nodeOwner;
      delete [] nodeCoord;
      if (node_mask_present) delete [] nodeMask;
      // 
      delete [] elemConn;
      delete [] elemId;
      delete [] elemType;
      if (elem_area_present) delete [] elemArea;
      if (elem_coord_present) delete [] elemCoord;
      if (elem_mask_present) delete [] elemMask;
    }

    void add_nodes(int *node_id, int *node_owner, 
                   double *node_coord, int *node_mask=NULL) {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::add_nodes()"

      try {
        nodeId = new int[num_node];
        std::copy(node_id, node_id+num_node, nodeId);
        nodeOwner = new int[num_node];
        std::copy(node_owner, node_owner+num_node, nodeOwner);
        nodeCoord = new double[num_node*pdim];
        std::copy(node_coord, node_coord+num_node*pdim, nodeCoord);
  
        InterArray<int> *iin = NULL;
        if (node_mask != NULL) {
          nodeMask = new int[num_node];
          std::copy(node_mask, node_mask+num_node, nodeMask);
          node_mask_present = true;
          iin = new InterArray<int>(nodeMask,num_node);
        }
  
        int localrc;
        MBMesh_addnodes(&meshp, &num_node, nodeId, nodeCoord, nodeOwner, iin,
                          &coord_sys, &orig_sdim, &localrc);
        ESMC_CHECK_RC_THROW(localrc)
  
        if (node_mask_present) delete iin;
    
      } ESMC_CATCH_MBMESH_TEST_RETHROW
    }

    void add_elems(int numelemconn, int *elem_conn, int *elem_id, 
                   int *elem_type, int *elem_mask = NULL,
                   double *elem_area = NULL, double *elem_coord = NULL,
                   int *regridconserve = 0) {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMeshTest::add_elems()"

      try {
        num_elem_conn = numelemconn;
  
        elemConn = new int[numelemconn];
        std::copy(elem_conn, elem_conn+num_elem_conn, elemConn);
        elemId = new int[num_elem];
        std::copy(elem_id, elem_id+num_elem, elemId);
        elemType = new int[num_elem];
        std::copy(elem_type, elem_type+num_elem, elemType);
        
        InterArray<int> *iie = NULL;
        if (elem_mask != NULL) {
          elemMask = new int[num_elem];
          std::copy(elem_mask, elem_mask+num_elem, elemMask);
          elem_mask_present = true;
          iie = new InterArray<int>(elemMask,num_elem);
        }
  
        if (elem_area != NULL) {
          elemArea = new double[num_elem];
          std::copy(elem_area, elem_area+num_elem, elemArea);
          elem_area_present = true;
        }
  
        if (elem_coord != NULL) {
          elemCoord = new double[num_elem*pdim];
          std::copy(elem_coord, elem_coord+num_elem*pdim, elemCoord);
          elem_coord_present = true;
        }
  
        int localrc;
        MBMesh_addelements(&meshp, &num_elem, elemId, elemType, iie,
                          &elem_area_present, elemArea,
                          &elem_coord_present, elemCoord,
                          &numelemconn, elemConn,
                          regridconserve,
                          &coord_sys, &orig_sdim, &localrc);
        ESMC_CHECK_RC_THROW(localrc)
  
        if (elem_mask_present) delete iie;

      } ESMC_CATCH_MBMESH_TEST_RETHROW
    }

    int test_get_info(double tol = 1.e-15, int verbosity = 0){
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
      ESMC_CHECK_RC_THROW(localrc)
    
      int elemCount;
      MBMesh_GetElemCount(meshp, &elemCount, &localrc);
      ESMC_CHECK_RC_THROW(localrc)
    
      int elemConnCount;
      MBMesh_GetElemConnCount(meshp, &elemConnCount, &localrc);
      ESMC_CHECK_RC_THROW(localrc)
    
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
      ESMC_CHECK_RC_THROW(localrc)
    
      if (elemMaskIsPresent!=elem_mask_present) {
        printf("elemAreaIsPresent = %d (correct = %d)\n", elemAreaIsPresent, elem_area_present);
        correct = false;
      } 
      if (elemAreaIsPresent!=elem_area_present) {
        printf("elemCoordIsPresent = %d (correct = %d)\n", elemCoordIsPresent, elem_coord_present);
        correct = false;
      } 
      if (elemCoordIsPresent!=elem_coord_present) {
        printf("elemMaskIsPresent = %d (correct = %d)\n", elemMaskIsPresent, elem_mask_present);
        correct = false;
      }
      
      if (verbosity >= 2) {
        printf("elemMaskIsPresent = %d\n", elemMaskIsPresent);
        printf("elemAreaIsPresent = %d\n", elemAreaIsPresent);
        printf("elemCoordIsPresent = %d\n", elemCoordIsPresent);
      }

      int nodeMaskIsPresent;
      MBMesh_GetNodeInfoPresence(meshp, &nodeMaskIsPresent, &localrc);
      ESMC_CHECK_RC_THROW(localrc)
    
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
      ESMC_CHECK_RC_THROW(localrc)

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
      ESMC_CHECK_RC_THROW(localrc)
    
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
      ESMC_CHECK_RC_THROW(localrc)
    
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
        ESMC_CHECK_RC_THROW(localrc)
      
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
      ESMC_CHECK_RC_THROW(localrc)
    
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
      ESMC_CHECK_RC_THROW(localrc)
    
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
      ESMC_CHECK_RC_THROW(localrc)
    
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
        ESMC_CHECK_RC_THROW(localrc)
      
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
        ESMC_CHECK_RC_THROW(localrc)
      
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
        ESMC_CHECK_RC_THROW(localrc)
      
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
    
      } ESMC_CATCH_MBMESH_TEST_RETURN(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }
};

