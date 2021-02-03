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
#include "ESMCI_MBMesh_Bilinear.h"
#include "ESMCI_MBMesh_Dual.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMCI_MBMesh_Mapping.h"
#include "ESMCI_MBMesh_Patch.h"
#include "ESMCI_MBMesh_Rendez_EtoP.h"
#include "ESMCI_MBMesh_Search_EtoP.h"
#include "ESMCI_MBMesh_Util.h"
#include "ESMCI_PointList.h"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>
#include <random>
#include <regex>
// #include <functional>

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
#define CATCH_MBT_RETURN_NULL(rc) \
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
#define CATCH_MBT_RETURN_RC(rc) \
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

// for MBMesh routines, when integer is returned
#define CATCH_MBT_FAIL(rc) \
  catch(int localrc){ \
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
  } catch(std::string errstr){ \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, errstr, ESMC_CONTEXT, rc); \
  } catch (ESMCI::esmc_error &exc) { \
    ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
  } catch (std::exception &exc) { \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
  } catch(...) { \
    std::string msg; \
    msg = "Unknown exception"; \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL, msg, ESMC_CONTEXT, rc);}


class MBT {
  public:
    int verbosity = 0;
    double tol = 1.e-15;
    
    int np = 0;
    int localPet = -1;
    
    MBMesh *mesh= nullptr;
    MBMesh *target = nullptr;
    PointList *pl = nullptr;
    std::string name = "MBMesh";

    // buffer that needs to be deleted
    char *serialize_buffer = nullptr;
    
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
    std::vector<int> elemId;
    std::vector<int> elemMask;
    std::vector<int> elemType;
    std::vector<double> elemArea;
    std::vector<double> elemCoord;
    std::vector<int> elemConn;

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

    MBT(int _pdim, int _sdim, ESMC_CoordSys_Flag _coord_sys, int _num_node, int _num_elem, int _num_elem_conn, int _redist_num_node, int _redist_num_elem, int _redist_num_elem_conn) {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT()"
      try {
        mesh = new MBMesh();

        // Get parallel information
        int localrc;
        localrc=ESMC_VMGet(ESMC_VMGetGlobal(&localrc), &localPet, &np, 
                      (int *)NULL, (MPI_Comm *)NULL, (int *)NULL, (int *)NULL);
        ESMC_CHECK_THROW(localrc);

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
        
        function_map["createget"] = std::bind(&MBT::createget, this);
        function_map["mbtypes"] = std::bind(&MBT::mbtypes, this);
        function_map["dual"] = std::bind(&MBT::dual, this);
        function_map["redist_elem"] = std::bind(&MBT::redist_elem, this);
        function_map["redist_node"] = std::bind(&MBT::redist_node, this);
        function_map["redist_elno"] = std::bind(&MBT::redist_elno, this);
        function_map["regrid_rendezvous_center"] = std::bind(&MBT::regrid_rendezvous_center, this);
        function_map["regrid_rendezvous_corner"] = std::bind(&MBT::regrid_rendezvous_corner, this);
        function_map["regrid_search_center"] = std::bind(&MBT::regrid_search_center, this);
        function_map["regrid_search_corner"] = std::bind(&MBT::regrid_search_corner, this);
        function_map["regrid_bilinear_center"] = std::bind(&MBT::regrid_bilinear_center, this);
        function_map["regrid_bilinear_corner"] = std::bind(&MBT::regrid_bilinear_corner, this);
        // function_map["regrid_conserve_center"] = std::bind(&MBT::regrid_conserve_center, this);
        // function_map["regrid_conserve_corner"] = std::bind(&MBT::regrid_conserve_corner, this);
        // function_map["regrid_conserve_2nd_center"] = std::bind(&MBT::regrid_conserve_2nd_center, this);
        // function_map["regrid_conserve_2nd_corner"] = std::bind(&MBT::regrid_conserve_2nd_corner, this);
        // function_map["regrid_nearest_d2s_center"] = std::bind(&MBT::regrid_nearest_d2s_center, this);
        // function_map["regrid_nearest_d2s_corner"] = std::bind(&MBT::regrid_nearest_d2s_corner, this);
        // function_map["regrid_nearest_s2d_center"] = std::bind(&MBT::regrid_nearest_s2d_center, this);
        // function_map["regrid_nearest_s2d_corner"] = std::bind(&MBT::regrid_nearest_s2d_corner, this);
        function_map["regrid_patch_center"] = std::bind(&MBT::regrid_patch_center, this);
        function_map["regrid_patch_corner"] = std::bind(&MBT::regrid_patch_corner, this);
        function_map["serialize"] = std::bind(&MBT::serialize, this);
        function_map["to_pointlist_elem"] = std::bind(&MBT::to_pointlist_elem, this);
        function_map["to_pointlist_node"] = std::bind(&MBT::to_pointlist_node, this);
        function_map["write_vtk"] = std::bind(&MBT::write_vtk, this);

      }
      CATCH_MBMESH_RETHROW
    }

    ~MBT(){
      if (mesh) delete mesh;
      if (target) delete target;
      if (pl) delete pl;
      if (serialize_buffer) delete serialize_buffer;
    }
    
    int build() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::build()"
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
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int createget(){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::createget()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int mbtypes(){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::mbtypes()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        Range elems;
        int merr=mesh->mesh->get_entities_by_dimension(0,mesh->pdim,elems);
        ESMC_CHECK_THROW(merr)
        
        for (Range::iterator it=elems.begin(); it !=elems.end(); it++) {
          const EntityHandle elem=*it;
          int type = mesh->mesh->type_from_handle(elem);
          std::cout << "type = " << type << std::endl;
        }


      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int dual() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::dual()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;

        // skip if run on one processor
        if (np == 1) {
          name = "SKIP - " + name;
          return ESMF_SUCCESS;
        }
          
        int ne = redist_elemId.size();

        MBMeshDual(mesh, &target, &localrc);
        ESMC_CHECK_THROW(localrc);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // verify dual info on target
        // this is tricky will require vtk output for empirical discovery
        // localrc = test_dual_info(target);
        ESMC_CHECK_THROW(localrc);
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int redist_elem() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::redist_elem()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;

        // skip if run on one processor
        if (np == 1) {
          name = "SKIP - " + name;
          return ESMF_SUCCESS;
        }
          
        int ne = redist_elemId.size();

        MBMesh_createredistelems(&mesh,
                                 &ne, redist_elemId.data(), &target, &localrc);
        ESMC_CHECK_THROW(localrc);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // verify redist info on target
        localrc = test_redist_info(target, true, false);
        ESMC_CHECK_THROW(localrc);
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int redist_node() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::redist_node()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        // skip if run on one processor
        if (np == 1) {
          name = "SKIP - " + name;
          return ESMF_SUCCESS;
        }
          
        int nn = redist_nodeId.size();
        
        MBMesh_createredistnodes(&mesh, &nn, redist_nodeId.data(), 
                                 &target, &localrc);
        ESMC_CHECK_THROW(localrc);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // verify redist info on target
        localrc = test_redist_info(target, false, true);
        ESMC_CHECK_THROW(localrc);
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int redist_elno() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::redist_elno()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        // skip if run on one processor
        if (np == 1) {
          name = "SKIP - " + name;
          return ESMF_SUCCESS;
        }
          
        int nn = redist_nodeId.size();
        int ne = redist_elemId.size();
        
        MBMesh_createredist(&mesh, &nn, redist_nodeId.data(), 
                            &ne, redist_elemId.data(), &target, &localrc);
        ESMC_CHECK_THROW(localrc);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // verify redist info on target
        rc = test_redist_info(target, true, true);
        ESMC_CHECK_THROW(localrc);
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int regrid_rendezvous_center() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::regrid_rendezvous_center()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        // skip if run on one processor
        if (np == 1) {
          name = "SKIP - " + name;
          return ESMF_SUCCESS;
        }
        
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_ELEMENT, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
        
        // Cartesian?
        int map_type = MB_MAP_TYPE_GREAT_CIRCLE;
        if (coord_sys == ESMC_COORDSYS_CART)
          map_type = MB_MAP_TYPE_CART_APPROX;
        
        MBMesh *mesh_rend=NULL;
        PointList *pl_rend=NULL;
        create_rendez_mbmesh_etop(mesh, pl, &mesh_rend, &pl_rend, &map_type);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify mesh_rend and pls
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int regrid_rendezvous_corner() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::regrid_rendezvous_corner()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        // skip if run on one processor
        if (np == 1) {
          name = "SKIP - " + name;
          return ESMF_SUCCESS;
        }
        
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_NODE, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
        
        // Cartesian?
        int map_type = MB_MAP_TYPE_GREAT_CIRCLE;
        if (coord_sys == ESMC_COORDSYS_CART)
          map_type = MB_MAP_TYPE_CART_APPROX;
        
        MBMesh *mesh_rend=NULL;
        PointList *pl_rend=NULL;
        create_rendez_mbmesh_etop(mesh, pl, &mesh_rend, &pl_rend, &map_type);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify mesh_rend and pls
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int regrid_search_center() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::regrid_search_center()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        // skip if run on one processor
        if (np == 1) {
          name = "SKIP - " + name;
          return ESMF_SUCCESS;
        }
        
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_ELEMENT, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
        
        // Cartesian?
        int map_type = MB_MAP_TYPE_GREAT_CIRCLE;
        if (coord_sys == ESMC_COORDSYS_CART)
          map_type = MB_MAP_TYPE_CART_APPROX;
        
        WMat dst_status;

        // search between mesh and pointlist
        MBMesh_Search_EToP_Result_List sr;
        MBMesh_Search_EToP(mesh, pl, ESMCI_UNMAPPEDACTION_IGNORE, &map_type,
                     10E-8, sr, false, dst_status, NULL, NULL);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify sr and dst_status and pl
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int regrid_search_corner() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::regrid_search_corner()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        // skip if run on one processor
        if (np == 1) {
          name = "SKIP - " + name;
          return ESMF_SUCCESS;
        }
        
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_NODE, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
        
        // Cartesian?
        int map_type = MB_MAP_TYPE_GREAT_CIRCLE;
        if (coord_sys == ESMC_COORDSYS_CART)
          map_type = MB_MAP_TYPE_CART_APPROX;
        
        WMat dst_status;

        // search between mesh and pointlist
        MBMesh_Search_EToP_Result_List sr;
        MBMesh_Search_EToP(mesh, pl, ESMCI_UNMAPPEDACTION_IGNORE, &map_type,
                     10E-8, sr, false, dst_status, NULL, NULL);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify sr and dst_status and pl
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int regrid_bilinear_center() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::regrid_bilinear_center()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_ELEMENT, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
        
        // Cartesian?
        int map_type = MB_MAP_TYPE_GREAT_CIRCLE;
        if (coord_sys == ESMC_COORDSYS_CART)
          map_type = MB_MAP_TYPE_CART_APPROX;
        
        // calculate bilinear weights between mesh and pointlist
        IWeights wts, dst_status;
        calc_bilinear_regrid_wgts(mesh, pl, wts, &map_type, true, dst_status);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify the wts and dst_status
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int regrid_bilinear_corner() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::regrid_bilinear_corner()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_NODE, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
        
        // Cartesian?
        int map_type = MB_MAP_TYPE_GREAT_CIRCLE;
        if (coord_sys == ESMC_COORDSYS_CART)
          map_type = MB_MAP_TYPE_CART_APPROX;
        
        // calculate bilinear weights between mesh and pointlist
        IWeights wts, dst_status;
        calc_bilinear_regrid_wgts(mesh, pl, wts, &map_type, true, dst_status);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify the wts and dst_status
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int regrid_patch_center() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::regrid_patch_center()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_ELEMENT, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
        
        // Cartesian?
        int map_type = MB_MAP_TYPE_GREAT_CIRCLE;
        if (coord_sys == ESMC_COORDSYS_CART)
          map_type = MB_MAP_TYPE_CART_APPROX;
        
        // calculate bilinear weights between mesh and pointlist
        IWeights wts, dst_status;
        calc_patch_regrid_wgts(mesh, pl, wts, &map_type, true, dst_status);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify the wts and dst_status
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int regrid_patch_corner() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::regrid_patch_corner()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_NODE, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);
        
        // Cartesian?
        int map_type = MB_MAP_TYPE_GREAT_CIRCLE;
        if (coord_sys == ESMC_COORDSYS_CART)
          map_type = MB_MAP_TYPE_CART_APPROX;
        
        // calculate bilinear weights between mesh and pointlist
        IWeights wts, dst_status;
        calc_patch_regrid_wgts(mesh, pl, wts, &map_type, true, dst_status);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify the wts and dst_status
      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int serialize() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::serialize()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        // serialization buffer
        int len = 100*sizeof(int);
        serialize_buffer = new char[len];
        int length = len; 
        int offset = 0;
        ESMC_InquireFlag inquireflag = ESMF_NOINQUIRE;
        ESMCI_FortranStrLenArg buffer_l = len;

        // serialize, deserialize, verify
        MBMesh_serialize(&mesh, serialize_buffer, &length, &offset, 
                         &inquireflag, &localrc, buffer_l);
        ESMC_CHECK_THROW(localrc);
        
        offset = 0;
        // deserialize
        MBMesh_deserialize(&target, serialize_buffer, &offset, &localrc, buffer_l);
        ESMC_CHECK_THROW(localrc);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);
        
        // TODO: verify the deserialized target mesh
        // can't get counts because ents not finalized, because no actual info
        // localrc = test_get_info(target);
        ESMC_CHECK_THROW(localrc);
      }
      CATCH_MBMESH_RETHROW

      rc = ESMF_SUCCESS;
      return rc;
    }

    int to_pointlist_elem() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::to_pointlist_elem()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_ELEMENT, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify the pl
      }
      CATCH_MBMESH_RETHROW

      rc = ESMF_SUCCESS;
      return rc;
    }

    int to_pointlist_node() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::to_pointlist_node()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        PointList *pl;
        pl = MBMesh_to_PointList(mesh, ESMC_MESHLOC_NODE, NULL, &localrc);
        ESMC_CHECK_THROW(localrc);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify the pl
      }
      CATCH_MBMESH_RETHROW

      rc = ESMF_SUCCESS;
      return rc;
    }

    int write_vtk() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::write_vtk()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
       
/* 
        // regex to split on space and dash
        std::regex regex{R"([\s-]+)"};
        // iterator over substrings
        std::sregex_token_iterator it{name.begin(), name.end(), regex, -1};
        // vector of substrings, {} indicates end of range
        std::vector<std::string> name_sub{it, {}};
        std::string test = name_sub.back();
        // for (const auto i : name_sub)
        //   std::cout << i << ", ";
        // std::cout << std::endl;
        // std::cout << test << std::endl;
*/
        // intel 19.0.5 on cheyenne uses gcc 4.8.5 for linking, which does not
        // yet have a functional regex, uncomment above when fixed.
        std::string test = name;
      
        // create test file name using localPet
        int len = test.length();
        char fname[len];
        sprintf(fname, "%s_%d", test.c_str(), localPet);
        
        MBMesh_write(&mesh, fname, &localrc, len);
        ESMC_CHECK_THROW(localrc);

        // verify the original mesh is still valid
        localrc = test_get_info(mesh);
        ESMC_CHECK_THROW(localrc);

        // TODO: verify the write (with a read?)
      }
      CATCH_MBMESH_RETHROW

      rc = ESMF_SUCCESS;
      return rc;
    }

    int print() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::print()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        std::cout << "MBT Print:"<< std::endl;
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
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int test_get_counts(MBMesh *mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::test_get_counts()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      int localrc;

      // get dimensions
      int local_pdim, local_sdim;
      MBMesh_GetDimensions(mesh, &local_sdim, &local_pdim, &localrc);
      ESMC_CHECK_THROW(localrc);

      if (local_pdim != pdim) {
        std::cout << localPet << "# " << name  << " - "
                  << "pdim = " << local_pdim 
                  << " (correct = " << pdim << ")" << std::endl;
        correct = false;
      }

      if (local_sdim != orig_sdim) {
        std::cout << localPet << "# " << name  << " - "
                  << "sdim = " << local_sdim 
                  << " (correct = " << orig_sdim << ")" << std::endl;
        correct = false;
      }

      if (verbosity >= 2) {
        std::cout << localPet << "# " << name  << " - "
                  << "pdim = " << pdim
                  << " sdim = " << sdim << std::endl;
      }

      int nodeCount;
      MBMesh_GetNodeCount(mesh, &nodeCount, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      if (nodeCount != num_node) {
        std::cout << localPet << "# " << name  << " - "
                  << "nodeCount = " << nodeCount
                  << " (correct = " << num_node << ")" << std::endl;
        correct = false;
      }
    
      int elemCount;
      MBMesh_GetElemCount(mesh, &elemCount, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      if (elemCount != num_elem) {
        std::cout << localPet << "# " << name  << " - "
                  << "elemCount = " << elemCount
                  << " (correct = " << num_elem << ")" << std::endl;
        correct = false;
      }
    
      // NOTE: bypass for now, remove when elemConn from ngons fixed
      if (name.find("ngon") == std::string::npos) {
        int elemConnCount;
        MBMesh_GetElemConnCount(mesh, &elemConnCount, &localrc);
        ESMC_CHECK_THROW(localrc);
      
        if (elemConnCount != num_elem_conn) {
          std::cout << localPet << "# " << name  << " - "
                    << "elemConnCount = " << elemConnCount
                    << " (correct = " << num_elem_conn << ")" << std::endl;
          correct = false;
        }
      }

      if (verbosity >= 2) {
        std::cout << localPet << "# " << name  << " - "
                  << "nodeCount = " << nodeCount
                  << " elemCount = " << elemCount << std::endl;
      }

      }
      CATCH_MBT_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }

    int test_get_presence(MBMesh *mesh, bool check_elem = true, bool check_node = true){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::test_get_presence()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
        int localrc;

        if (check_elem) {
          int elemMaskIsPresent, elemAreaIsPresent, elemCoordIsPresent;
          MBMesh_GetElemInfoPresence(mesh, &elemMaskIsPresent, 
                                     NULL, NULL, &localrc);
          ESMC_CHECK_THROW(localrc);
    
          if (elemMaskIsPresent!=elem_mask_present) {
            std::cout << localPet << "# " << name  << " - "
                      << "elemMaskIsPresent = " << elemMaskIsPresent
                      << " (correct = " << elem_mask_present << ")" << std::endl;
            correct = false;
          } 

          MBMesh_GetElemInfoPresence(mesh, NULL, &elemAreaIsPresent, 
                                     NULL, &localrc);
          ESMC_CHECK_THROW(localrc);
    
          if (elemAreaIsPresent!=elem_area_present) {
            std::cout << localPet << "# " << name  << " - "
                      << "elemAreaIsPresent = " << elemAreaIsPresent
                      << " (correct = " << elem_area_present << ")" << std::endl;
            correct = false;
          } 

          MBMesh_GetElemInfoPresence(mesh, NULL, NULL, 
                                     &elemCoordIsPresent, &localrc);
          ESMC_CHECK_THROW(localrc);
    
          if (elemCoordIsPresent!=elem_coord_present) {
            std::cout << localPet << "# " << name  << " - "
                      << "elemCoordIsPresent = " << elemCoordIsPresent
                      << " (correct = " << elem_coord_present << ")" << std::endl;
            correct = false;
          }
          
          if (verbosity >= 2) {
            std::cout << localPet << "# " << name  << " - "
                      << "elemMaskIsPresent = " << elemMaskIsPresent
                      << std::endl;
            std::cout << localPet << "# " << name  << " - "
                      << "elemAreaIsPresent = " << elemAreaIsPresent
                      << std::endl;
            std::cout << localPet << "# " << name  << " - "
                      << "elemCoordIsPresent = " << elemCoordIsPresent
                      << std::endl;
          }
        }
        
        if (check_node) {
          int nodeMaskIsPresent;
          MBMesh_GetNodeInfoPresence(mesh, &nodeMaskIsPresent, &localrc);
          ESMC_CHECK_THROW(localrc);
    
          if (nodeMaskIsPresent!=node_mask_present) {
            std::cout << localPet << "# " << name  << " - "
                      << "nodeMaskIsPresent = " << nodeMaskIsPresent
                      << " (correct = " << node_mask_present << ")" << std::endl;
            correct = false;
          }
    
          if (verbosity >= 2) {
            std::cout << localPet << "# " << name  << " - "
                      << "nodeMaskIsPresent = " << nodeMaskIsPresent
                      << std::endl;
          }
        }
      }
      CATCH_MBT_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }

    int test_get_node_info(MBMesh *mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::test_get_node_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      bool fail_print, print;
      int localrc;

      std::string test;

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
          std::cout << localPet << "# " << name  << " - "
                    << "node_id[" << i << "] = " 
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
          std::cout << localPet << "# " << name  << " - "
                    << "node_coord[" << i << "] = " 
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
          std::cout << localPet << "# " << name  << " - "
                    << "node_owner[" << i << "] = " 
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
            std::cout << localPet << "# " << name  << " - "
                      << "node_mask[" << i << "] = " 
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
      CATCH_MBT_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }

    int test_get_elem_info(MBMesh *mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::test_get_elem_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      bool fail_print, print;
      int localrc;

      std::string test;

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
          std::cout << localPet << "# " << name  << " - "
                    << "elem_ids[" << i << "] = " 
                    << eii->array[i] << " (correct = " << elemId[i] << ")"
                    << std::endl;
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    
      MBMesh_GetElemCreateInfo(mesh, NULL, eti, NULL, NULL, NULL, NULL, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      // NOTE: bypass for now, remove when elemType from ngons fixed
      if (name.find("ngon") == std::string::npos) {
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
            std::cout << localPet << "# " << name  << " - "
                      << "elem_type[" << i << "] = " 
                      << eti->array[i] << " (correct = " << elemType[i] << ")"
                      << std::endl;
        }
        if (verbosity >= 1) {
          if (!fail_print) std::cout<< pass << test << std::endl;
          else std::cout << fail << test << std::endl;
        }
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
            std::cout << localPet << "# " << name  << " - "
                      << "elem_area[" << i << "] = " 
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
          std::cout << localPet << name  << " - "
                    << "# " << "elem_coord[" << i << "] = " 
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
            std::cout << localPet << "# " << name  << " - "
                      << "elem_mask[" << i << "] = " 
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
      CATCH_MBT_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }


    int test_get_elem_conn_info(MBMesh *mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::test_get_elem_conn_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;
      bool correct = true;

      try {
      bool fail_print, print;
      int localrc;

      std::string test;

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
          std::cout << localPet << "# " << name  << " - "
                    << "elem_connectivity[" << i << "] = " 
                    << ecni->array[i] << " (correct = " << elemConn[i] << ")"
                    << std::endl;
      }
      if (verbosity >= 1) {
        if (!fail_print) std::cout<< pass << test << std::endl;
        else std::cout << fail << test << std::endl;
      }
    

      delete ecni;
    
      }
      CATCH_MBT_RETURN_RC(&rc)

      if(correct == true) rc = ESMF_SUCCESS;
      return rc;
    }

    int test_get_info(MBMesh *mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::test_get_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc;
        
        localrc = test_get_counts(mesh);
        ESMC_CHECK_THROW(localrc);
        
        localrc = test_get_presence(mesh);
        ESMC_CHECK_THROW(localrc);
        
        localrc = test_get_node_info(mesh);
        ESMC_CHECK_THROW(localrc);
        
        localrc = test_get_elem_info(mesh);
        ESMC_CHECK_THROW(localrc);
        
        // NOTE: bypass for now, remove when elemConn from ngons fixed
        if (name.find("ngon") == std::string::npos) {
          localrc = test_get_elem_conn_info(mesh);
          ESMC_CHECK_THROW(localrc);
        }

      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int transfer_dual_info(){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::transfer_dual_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc; 

        num_node = num_elem;
        num_elem = num_node;
        // num_elem_conn = ;

        nodeId.clear();
        nodeMask.clear();
        nodeOwner.clear();
        nodeCoord.clear();
        elemId.clear();
        elemMask.clear();
        elemType.clear();
        elemArea.clear();
        elemCoord.clear();
        elemConn.clear();

        nodeId = elemId;
        nodeMask = elemMask;
        // nodeOwner = ;
        nodeCoord = elemCoord;
        elemId = nodeId;
        elemMask = nodeMask;
        // elemType = ;
        // elemArea = ;
        elemCoord = nodeCoord;
        // elemConn = ;

      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }


    int test_dual_info(MBMesh *mesh){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::test_dual_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc; 

        localrc = transfer_dual_info();
        ESMC_CHECK_THROW(localrc);
        
        localrc = test_get_counts(target);
        ESMC_CHECK_THROW(localrc);
        
        // dual does not yet have elem masking
        bool check_node = true;
        bool check_elem = false;
        localrc = test_get_presence(target, check_elem, check_node);
        ESMC_CHECK_THROW(localrc);
        
        // RLO: not sure how to do this for the dual..
        
        // localrc = test_get_node_info(mesh);
        // ESMC_CHECK_THROW(localrc);
        // 
        // localrc = test_get_elem_info(mesh);
        // ESMC_CHECK_THROW(localrc);
        // 
        // localrc = test_get_elem_conn_info(mesh);
        // ESMC_CHECK_THROW(localrc);

      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }
    
    int transfer_redist_info(){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::transfer_redist_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc; 

        num_node = redist_num_node;
        num_elem = redist_num_elem;
        num_elem_conn = redist_num_elem_conn;

        nodeId.clear();
        nodeMask.clear();
        nodeOwner.clear();
        nodeCoord.clear();
        elemId.clear();
        elemMask.clear();
        elemType.clear();
        elemArea.clear();
        elemCoord.clear();
        elemConn.clear();

        nodeId = redist_nodeId;
        nodeMask = redist_nodeMask;
        nodeOwner = redist_nodeOwner;
        nodeCoord = redist_nodeCoord;
        elemId = redist_elemId;
        elemMask = redist_elemMask;
        elemType = redist_elemType;
        elemArea = redist_elemArea;
        elemCoord = redist_elemCoord;
        elemConn = redist_elemConn;

      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }

    int test_redist_info(MBMesh *mesh, bool check_elem, bool check_node){
#undef ESMC_METHOD
#define ESMC_METHOD "MBT::test_redist_info()"
      // RETURN: rc : pass(0) fail(>0)
      int rc = ESMF_FAILURE;

      try {
        int localrc; 

        // transfer redist info to from test "_redist" variables
        localrc = transfer_redist_info();
        ESMC_CHECK_THROW(localrc);

        // TODO: redist can't reassemble split element (fix with ngons)
        // NOTE: bypass for now, remove when elemType from ngons fixed
        if (name.find("ngon") == std::string::npos) {
          // in the node redist case there are more elements created
          // really need to subclass MBT to allow different specs
          if (!(check_node and !check_elem))
            localrc = test_get_counts(target);
        }
        ESMC_CHECK_THROW(localrc);
        
      /////////////////// node create info ////////////////////////////
      if (check_node) {
        int nn = redist_nodeId_in.size();
        MBMesh_checknodelist(&target, &nn, redist_nodeId_in.data(), &localrc);
        ESMC_CHECK_THROW(localrc);
      }

      /////////////////// elem create info ////////////////////////////
      if (check_elem) {
        int ne = redist_elemId_in.size();
        MBMesh_checkelemlist(&target, &ne, redist_elemId_in.data(), &localrc);
        ESMC_CHECK_THROW(localrc);
      }

        // check_node and check_elem default to true
        localrc = test_get_presence(target);
        ESMC_CHECK_THROW(localrc);
        
        // localrc = test_get_node_info(mesh);
        // ESMC_CHECK_THROW(localrc);
        // 
        // localrc = test_get_elem_info(mesh);
        // ESMC_CHECK_THROW(localrc);
        // 
        // localrc = test_get_elem_conn_info(mesh);
        // ESMC_CHECK_THROW(localrc);

      /////////////////// counts ////////////////////////////
      int nodeCount;
      MBMesh_GetNodeCount(target,&nodeCount, &localrc);
      ESMC_CHECK_THROW(localrc);
      
      int elemCount;
      MBMesh_GetElemCount(target,&elemCount, &localrc);
      ESMC_CHECK_THROW(localrc);
      
      int elemConnCount;
      MBMesh_GetElemConnCount(target,&elemConnCount, &localrc);
      ESMC_CHECK_THROW(localrc);
    
      if (verbosity >= 2) {
        std::cout << localPet << "# " << "nodeCount = " << nodeCount << "(" << num_node << ")"
                  << " elemCount = " << elemCount << "(" << num_elem << ")" << std::endl;
      }
      
      if (nodeCount == num_node) {
        // localrc = test_get_node_info_redist(target);
        ESMC_CHECK_THROW(localrc);
      }
      
      if (elemCount == num_elem) {
        // localrc = test_get_elem_info_redist(target);
        ESMC_CHECK_THROW(localrc);
      }
      
      if (elemConnCount == num_elem_conn) {
        // localrc = test_get_elem_conn_info_redist(target);
        ESMC_CHECK_THROW(localrc);
      }

      }
      CATCH_MBT_RETURN_RC(&rc)

      rc = ESMF_SUCCESS;
      return rc;
    }
};

