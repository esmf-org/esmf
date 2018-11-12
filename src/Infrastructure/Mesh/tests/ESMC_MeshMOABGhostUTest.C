// $Id$
//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#if defined ESMF_MOAB

#include "ESMCI_MBMesh_Glue.h"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Dual.h"

#include "MBTagConventions.hpp"
#include "moab/Core.hpp"
#include "moab/ParallelComm.hpp"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>


using namespace std;

//==============================================================================
//BOP
// !PROGRAM: ESMC_MeshMOABGhostUTest - Check for MOAB ghost element functionality
//
// !DESCRIPTION: 
////
//EOP
//-----------------------------------------------------------------------------

MBMesh* create_mesh_quad_9_parallel_dual2(ESMC_CoordSys_Flag coordsys, int &rc) {

  //
  //   3.0   13 ------ 14 ------ 15  ------------------ 16
  //         |         |         |                      |
  //         |         |         |                      |
  //         |    8    |    9    |              10      |
  //         |         |         |                      |
  //         |         |         |                      |
  //   2.0  [9] ----- [10] ---- [11] -----------------  12
  //
  //       1.0       1.5       2.0     2.0             3.0
  //
  //                PET 2                      PET 3
  //
  //
  //   2.0   9 ------- 10 ------ 11     [11] ----------- 12
  //         |         |         |       |               |
  //         |    5    |    6    |       |       7       |
  //         |         |         |       |               |
  //   1.5   5 ------- 6 ------- 7      [7] -----------  8
  //         |         |         |       |               |
  //         |    1    |    2    |       |       4       |
  //         |         |         |       |               |
  //   1.0   1 ------- 2 ------- 3      [3] ------------ 4
  //
  //         1.0       1.5     2.0      2.0             3.0
  //
  //                PET 0                      PET 1
  //
  //               Node Id labels at corners
  //              Element Id labels in centers


  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  if (petCount != 2) {
    Throw() << "Test function must be run with 2 processors";
    return NULL;
  }

  // Mesh variables
  int pdim=2;
  int sdim=2;
  int num_elem = 9;
  int num_node = 16;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;
  double *elemCoord;

  int areapresent = 0;
  int coordspresent = 1;
  int numelemconn = 0;
  int regridconserve = 0;

  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (4*num_elem * sizeof (int));
  elemCoord = (double *) malloc (2*num_elem * sizeof (double));

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);
  MBMesh_create(&meshp, &pdim, &sdim, &coordsys, &rc);

  if (localPet == 0){
    num_node = 9;
    num_elem = 4;

    nodeId[0]=1;
    nodeId[1]=2;
    nodeId[2]=3;
    nodeId[3]=5;
    nodeId[4]=6;
    nodeId[5]=7;
    nodeId[6]=9;
    nodeId[7]=10;
    nodeId[8]=11;

    nodeCoord[0]=1.0;nodeCoord[1]=1.0;
    nodeCoord[2]=1.5;nodeCoord[3]=1.0;
    nodeCoord[4]=2.0;nodeCoord[5]=1.0;
    nodeCoord[6]=1.0;nodeCoord[7]=1.5;
    nodeCoord[8]=1.5;nodeCoord[9]=1.5;
    nodeCoord[10]=2.0;nodeCoord[11]=1.5;
    nodeCoord[12]=1.0;nodeCoord[13]=2.0;
    nodeCoord[14]=1.5;nodeCoord[15]=2.0;
    nodeCoord[16]=2.0;nodeCoord[17]=2.0;

    nodeOwner[0]=0;
    nodeOwner[1]=0;
    nodeOwner[2]=0;
    nodeOwner[3]=0;
    nodeOwner[4]=0;
    nodeOwner[5]=0;
    nodeOwner[6]=0;
    nodeOwner[7]=0;
    nodeOwner[8]=0;

    elemId[0]=1;
    elemId[1]=2;
    elemId[2]=5;
    elemId[3]=6;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_QUAD;
    elemType[2]=ESMC_MESHELEMTYPE_QUAD;
    elemType[3]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
    elemConn[4]=2;elemConn[5]=3;elemConn[6]=6;elemConn[7]=5;
    elemConn[8]=4;elemConn[9]=5;elemConn[10]=8;elemConn[11]=7;
    elemConn[12]=5;elemConn[13]=6;elemConn[14]=9;elemConn[15]=8;

    elemCoord[0]=1.25;elemCoord[1]=1.25;
    elemCoord[2]=1.75;elemCoord[3]=1.25;
    elemCoord[4]=1.25;elemCoord[5]=1.75;
    elemCoord[6]=1.75;elemCoord[7]=1.75;

    numelemconn = 4*num_elem;
  }
  else if (localPet == 1) {
    num_node = 12;
    num_elem = 5;

    nodeId[0]=3;
    nodeId[1]=4;
    nodeId[2]=7;
    nodeId[3]=8;
    nodeId[4]=9;
    nodeId[5]=10;
    nodeId[6]=11;
    nodeId[7]=12;
    nodeId[8]=13;
    nodeId[9]=14;
    nodeId[10]=15;
    nodeId[11]=16;

    nodeCoord[0]=2.0;nodeCoord[1]=1.0;
    nodeCoord[2]=3.0;nodeCoord[3]=1.0;
    nodeCoord[4]=2.0;nodeCoord[5]=1.5;
    nodeCoord[6]=3.0;nodeCoord[7]=1.5;
    nodeCoord[8]=1.0;nodeCoord[9]=2.0;
    nodeCoord[10]=1.5;nodeCoord[11]=2.0;
    nodeCoord[12]=2.0;nodeCoord[13]=2.0;
    nodeCoord[14]=3.0;nodeCoord[15]=2.0;
    nodeCoord[16]=1.0;nodeCoord[17]=3.0;
    nodeCoord[18]=1.5;nodeCoord[19]=3.0;
    nodeCoord[20]=2.0;nodeCoord[21]=3.0;
    nodeCoord[22]=3.0;nodeCoord[23]=3.0;

    nodeOwner[0]=0;
    nodeOwner[1]=1;
    nodeOwner[2]=0;
    nodeOwner[3]=1;
    nodeOwner[4]=0;
    nodeOwner[5]=0;
    nodeOwner[6]=0;
    nodeOwner[7]=1;
    nodeOwner[8]=1;
    nodeOwner[9]=1;
    nodeOwner[10]=1;
    nodeOwner[11]=1;


    elemId[0]=4;
    elemId[1]=7;
    elemId[2]=8;
    elemId[3]=9;
    elemId[4]=10;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_QUAD;
    elemType[2]=ESMC_MESHELEMTYPE_QUAD;
    elemType[3]=ESMC_MESHELEMTYPE_QUAD;
    elemType[4]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;
    elemConn[4]=3;elemConn[5]=4;elemConn[6]=8;elemConn[7]=7;
    elemConn[8]=5;elemConn[9]=6;elemConn[10]=10;elemConn[11]=9;
    elemConn[12]=6;elemConn[13]=7;elemConn[14]=11;elemConn[15]=10;
    elemConn[16]=7;elemConn[17]=8;elemConn[18]=12;elemConn[19]=11;

    elemCoord[0]=2.5;elemCoord[1]=1.25;
    elemCoord[2]=2.5;elemCoord[3]=1.75;
    elemCoord[4]=1.25;elemCoord[5]=2.5;
    elemCoord[6]=1.75;elemCoord[7]=2.5;
    elemCoord[8]=2.5;elemCoord[9]=2.5;

    numelemconn = 4*num_elem;
  }

  MBMesh_addnodes(&meshp, &num_node, nodeId, nodeCoord, nodeOwner, NULL,
                  &coordsys, &sdim, &rc);

  MBMesh_addelements(&meshp, &num_elem, elemId, elemType, NULL,
                     &areapresent, NULL,
                     &coordspresent, elemCoord,
                     &numelemconn, elemConn,
                     &regridconserve,
                     &coordsys, &sdim, &rc);

  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return static_cast<MBMesh *>(meshp);
}

MBMesh* create_mesh_ph_parallel(ESMC_CoordSys_Flag coordsys, int &rc) {

//
//  2.5        8        10 --------11
//          /     \   /            |
//  2.1   7         9              12
//        |         |      5       /
//        |    4    |            /
//        |         |          /
//  1.0   4 ------- 5 ------- 6
//        |         |  \   3  |
//        |    1    |    \    |
//        |         |  2   \  |
// -0.1   1 ------- 2 ------- 3
//
//      -0.1       1.0       2.1   2.5 
// 
//        Node Id labels at corners
//       Element Id labels in centers

  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  if (petCount != 4) {
    Throw() << "Test function must be run with 4 processors";
    return NULL;
  }

  // Mesh variables
  int pdim=2;
  int sdim=2;
  int num_elem = 5;
  int num_node = 12;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;
  double *elemCoord;

  int areapresent = 0;
  int coordspresent = 1;
  int numelemconn = 0;
  int regridconserve = 0;

  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (4*num_elem * sizeof (int));
  elemCoord = (double *) malloc (2*num_elem * sizeof (double));

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);
  MBMesh_create(&meshp, &pdim, &sdim, &coordsys, &rc);

  if (localPet == 0){
    num_node = 4;
    num_elem = 1;

    nodeId[0]=1;
    nodeId[1]=2;
    nodeId[2]=4;
    nodeId[3]=5;

    nodeCoord[0]=-0.1;nodeCoord[1]=-0.1;
    nodeCoord[2]=1.0;nodeCoord[3]=-0.1;
    nodeCoord[4]=-0.1;nodeCoord[5]=1.0;
    nodeCoord[6]=1.0;nodeCoord[7]=1.0;

    nodeOwner[0]=0;
    nodeOwner[1]=0;
    nodeOwner[2]=0;
    nodeOwner[3]=0;

    elemId[0]=1;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;

    elemCoord[0]=0.45;elemCoord[1]=0.45;

    numelemconn = 4*num_elem;
  } else if (localPet == 1) {
    num_node = 4;
    num_elem = 2;

    nodeId[0]=2;
    nodeId[1]=3;
    nodeId[2]=5;
    nodeId[3]=6;

    nodeCoord[0]=1.0;nodeCoord[1]=-0.1;
    nodeCoord[2]=2.1;nodeCoord[3]=-0.1;
    nodeCoord[4]=1.0;nodeCoord[5]=1.0;
    nodeCoord[6]=2.1;nodeCoord[7]=1.0;

    nodeOwner[0]=0;
    nodeOwner[1]=1;
    nodeOwner[2]=0;
    nodeOwner[3]=1;

    elemId[0]=2;
    elemId[1]=3;

    elemType[0]=ESMC_MESHELEMTYPE_TRI;
    elemType[1]=ESMC_MESHELEMTYPE_TRI;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=3;
    elemConn[3]=2;elemConn[4]=4;elemConn[5]=3;

    elemCoord[0]=1.37;elemCoord[1]=0.27;
    elemCoord[2]=1.73;elemCoord[3]=0.63;

    numelemconn = 3*num_elem;
  } else if (localPet == 2) {
    num_node = 5;
    num_elem = 1;

    nodeId[0]=4;
    nodeId[1]=5;
    nodeId[2]=7;
    nodeId[3]=8;
    nodeId[4]=9;

    nodeCoord[0]=-0.1;nodeCoord[1]=1.0;
    nodeCoord[2]=1.0;nodeCoord[3]=1.0;
    nodeCoord[4]=-0.1;nodeCoord[5]=2.1;
    nodeCoord[6]=0.5;nodeCoord[7]=2.5;
    nodeCoord[8]=1.0;nodeCoord[9]=2.1;

    nodeOwner[0]=0;
    nodeOwner[1]=0;
    nodeOwner[2]=2;
    nodeOwner[3]=2;
    nodeOwner[4]=2;

    elemId[0]=4;

    elemType[0]=5;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;elemConn[4]=3;

    elemCoord[0]=0.46;elemCoord[1]=1.74;

    numelemconn = 5*num_elem;
  } else if (localPet == 3) {
    num_node = 6;
    num_elem = 1;

    nodeId[0]=5;
    nodeId[1]=6;
    nodeId[2]=9;
    nodeId[3]=10;
    nodeId[4]=11;
    nodeId[5]=12;

    nodeCoord[0]=1.0;nodeCoord[1]=1.0;
    nodeCoord[2]=2.1;nodeCoord[3]=1.0;
    nodeCoord[4]=1.0;nodeCoord[5]=2.1;
    nodeCoord[6]=1.5;nodeCoord[7]=2.5;
    nodeCoord[8]=2.5;nodeCoord[9]=2.5;
    nodeCoord[10]=2.5;nodeCoord[11]=2.1;

    nodeOwner[0]=0;
    nodeOwner[1]=1;
    nodeOwner[2]=2;
    nodeOwner[3]=3;
    nodeOwner[4]=3;
    nodeOwner[5]=3;

    elemId[0]=5;

    elemType[0]=6;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=6;
    elemConn[3]=5;elemConn[4]=4;elemConn[5]=3;

    elemCoord[0]=1.76;elemCoord[1]=1.87;

    numelemconn = 6*num_elem;
  }

  MBMesh_addnodes(&meshp, &num_node, nodeId, nodeCoord, nodeOwner, NULL,
                  &coordsys, &sdim, &rc);

  MBMesh_addelements(&meshp, &num_elem, elemId, elemType, NULL,
                     &areapresent, NULL,
                     &coordspresent, elemCoord,
                     &numelemconn, elemConn,
                     &regridconserve,
                     &coordsys, &sdim, &rc);

  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return static_cast<MBMesh *>(meshp);
}



int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;

  int localPet, petCount;
  ESMC_VM vm;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  MPI_Comm mpi_comm;
  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, &mpi_comm, (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_LogSet(true);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Moab Ghost element test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB
  MBMesh *mbmesh = NULL;
  mbmesh = create_mesh_quad_9_parallel_dual2(ESMC_COORDSYS_CART, rc);
  // mbmesh = create_mesh_ph_parallel(ESMC_COORDSYS_CART, rc);
  if (!mbmesh) rc = ESMC_RC_PTR_NULL;

#define DEBUG_EXCHANGE_TAGS
#ifdef DEBUG_EXCHANGE_TAGS
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshMOABGhostUTest"
  Interface *mb = mbmesh->mesh;
  ParallelComm* pcomm = new ParallelComm(mb, mpi_comm);
  
  Range range_ent;
  int merr; int pdim = mbmesh->pdim;
  merr=mb->get_entities_by_dimension(0,pdim,range_ent);
  MBMESH_CHECK_ERR(merr, rc);

  merr = pcomm->resolve_shared_ents(0, range_ent, pdim, 1);
  // pcomm->resolve_shared_ents(0, mbmesh->pdim, mbmesh->pdim-1);
  MBMESH_CHECK_ERR(merr, rc);
  
  void *mbptr = (void *) mbmesh;
  int len = 12; char fname[len];
  sprintf(fname, "mesh_%d", localPet);
  MBMesh_write(&mbptr, fname, &rc, len);

  
  merr = pcomm->exchange_ghost_cells(pdim, // int ghost_dim
                                     0, // int bridge_dim
                                     1, // int num_layers
                                     0, // int addl_ents
                                     true);// bool store_remote_handles
  MBMESH_CHECK_ERR(merr, rc);

  vector<Tag> tags;
  tags.push_back(mbmesh->gid_tag);
  tags.push_back(mbmesh->orig_pos_tag);
  tags.push_back(mbmesh->owner_tag);
  if (mbmesh->has_node_orig_coords) tags.push_back(mbmesh->node_orig_coords_tag);
  if (mbmesh->has_node_mask) {
    tags.push_back(mbmesh->node_mask_tag);
    tags.push_back(mbmesh->node_mask_val_tag);
  }
  if (mbmesh->has_elem_frac) tags.push_back(mbmesh->elem_frac_tag);
  if (mbmesh->has_elem_mask) {
    tags.push_back(mbmesh->elem_mask_tag);
    tags.push_back(mbmesh->elem_mask_val_tag);
  }
  if (mbmesh->has_elem_area) tags.push_back(mbmesh->elem_area_tag);
  if (mbmesh->has_elem_coords) tags.push_back(mbmesh->elem_coords_tag);
  if (mbmesh->has_elem_orig_coords) tags.push_back(mbmesh->elem_orig_coords_tag);  
   
  pcomm->set_debug_verbosity(4);

  merr = pcomm->exchange_tags(tags, tags, range_ent);
  MBMESH_CHECK_ERR(merr, rc);
  
  std::ostringstream ent_str;
  ent_str << "mesh_ghost." <<pcomm->rank() << ".vtk";
  mbmesh->mesh->write_mesh(ent_str.str().c_str());
    
#else

  MBMesh *mesh_dual = NULL;
  MBMeshDual(mbmesh, &mesh_dual, &rc);
  if (!mesh_dual) rc = ESMC_RC_PTR_NULL;

  void *mbptr = (void *) mbmesh;
  int len = 12; char fname[len];
  sprintf(fname, "mesh_%d", localPet);
  MBMesh_write(&mbptr, fname, &rc, len);
  
  void *mbptrd = (void *) mesh_dual;
  int lend = 17; char fnamed[lend];
  sprintf(fnamed, "mesh_dual_%d", localPet);
  MBMesh_write(&mbptrd, fnamed, &rc, lend);
#endif

#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

