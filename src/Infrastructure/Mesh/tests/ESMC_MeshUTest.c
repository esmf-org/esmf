// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_MeshUTest - Check ESMC_Mesh functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc, localrc;
  bool correct;
  ESMC_Mesh mesh;

  
  int numNodes;
  int *nodeIds;
  double *nodeCoords;
  int *nodeOwners;

  int numElems;
  int *elemIds;
  int *elemTypes;
  int *elemConn;
  int localPet, petCount;
  
  ESMC_VM vm;


  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // Init to success
  rc=ESMF_SUCCESS;
  
  // Get parallel information
  vm=ESMC_VMGetGlobal(&localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  localrc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL, (int *)NULL, (int *)NULL);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Test creating a 3D Mesh in C (Support issue #440)
  strcpy(name, "Test creating a 3D Mesh in C");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  // Create Mesh
  enum ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_CART;
  mesh = ESMC_MeshCreate(3,3,&local_coordSys,&localrc);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;
  
  // Set up 3D Mesh information  
  if (petCount == 1) {
    // Set number of nodes
    numNodes=18;

    // Allocate and fill the node id array.
    nodeIds = (int *) malloc (numNodes * sizeof (int));
    nodeIds[0]=1; nodeIds[1]=2; nodeIds[2]=3;
    nodeIds[3]=4; nodeIds[4]=5; nodeIds[5]=6;
    nodeIds[6]=7; nodeIds[7]=8; nodeIds[8]=9;
    nodeIds[9]=10; nodeIds[10]=11; nodeIds[11]=12;
    nodeIds[12]=13; nodeIds[13]=14; nodeIds[14]=15;
    nodeIds[15]=16; nodeIds[16]=17; nodeIds[17]=18;
    
    // Allocate and fill node coordinate array.
    nodeCoords = (double *) malloc (3*numNodes * sizeof (double));
    nodeCoords[0]=0.0; nodeCoords[1]=0.0; nodeCoords[2]=0.0; // Node 1
    nodeCoords[3]=1.0; nodeCoords[4]=0.0; nodeCoords[5]=0.0; // Node 2
    nodeCoords[6]=2.0; nodeCoords[7]=0.0; nodeCoords[8]=0.0; // Node 3
    nodeCoords[9]=0.0; nodeCoords[10]=1.0; nodeCoords[11]=0.0; // Node 4
    nodeCoords[12]=1.0; nodeCoords[13]=1.0; nodeCoords[14]=0.0; // Node 5
    nodeCoords[15]=2.0; nodeCoords[16]=1.0; nodeCoords[17]=0.0; // Node 6
    nodeCoords[18]=0.0; nodeCoords[19]=2.0; nodeCoords[20]=0.0; // Node 7
    nodeCoords[21]=1.0; nodeCoords[22]=2.0; nodeCoords[23]=0.0; // Node 8
    nodeCoords[24]=2.0; nodeCoords[25]=2.0; nodeCoords[26]=0.0; // Node 9
    nodeCoords[27]=0.0; nodeCoords[28]=0.0; nodeCoords[29]=1.0; // Node 10
    nodeCoords[30]=1.0; nodeCoords[31]=0.0; nodeCoords[32]=1.0; // Node 11
    nodeCoords[33]=2.0; nodeCoords[34]=0.0; nodeCoords[35]=1.0; // Node 12
    nodeCoords[36]=0.0; nodeCoords[37]=1.0; nodeCoords[38]=1.0; // Node 13
    nodeCoords[39]=1.0; nodeCoords[40]=1.0; nodeCoords[41]=1.0; // Node 14
    nodeCoords[42]=2.0; nodeCoords[43]=1.0; nodeCoords[44]=1.0; // Node 15
    nodeCoords[45]=0.0; nodeCoords[46]=2.0; nodeCoords[47]=1.0; // Node 16
    nodeCoords[48]=1.0; nodeCoords[49]=2.0; nodeCoords[50]=1.0; // Node 17
    nodeCoords[51]=2.0; nodeCoords[52]=2.0; nodeCoords[53]=1.0; // Node 18

    // Allocate and fill the node owner array.
    // Since this Mesh is all on PET 0, it's just set to all 0.
    nodeOwners = (int *) malloc (numNodes * sizeof (int));
    for (int i=0; i<numNodes; i++) {
      nodeOwners[i]=0; // Everything on PET 0
    }

    // Set the number of elements
    numElems=4;
    
    // Allocate and fill the element id array.
    elemIds = (int *) malloc (numElems * sizeof (int));
    elemIds[0]=1; elemIds[1]=2; elemIds[2]=3; elemIds[3]=4;
       
    // Allocate and fill the element topology type array.
    elemTypes = (int *) malloc (numElems * sizeof (int));
    elemTypes[0]=ESMC_MESHELEMTYPE_HEX; elemTypes[1]=ESMC_MESHELEMTYPE_HEX;
    elemTypes[2]=ESMC_MESHELEMTYPE_HEX; elemTypes[3]=ESMC_MESHELEMTYPE_HEX;



                 
    // Allocate and fill the element connection type array.
    // Note that entries in this array refer to the 
    // positions in the nodeIds, etc. arrays and that
    // the order and number of entries for each element
    // reflects that given in the Mesh options 
    // section for the corresponding entry
    // in the elemTypes array.
    elemConn = (int *) malloc (8 * numElems * sizeof (int));

    // Elem 1
    elemConn[0]=1; elemConn[1]=2; elemConn[2]=5; elemConn[3]=4;
    elemConn[4]=10; elemConn[5]=11; elemConn[6]=14; elemConn[7]=13;

    // Elem 2
    elemConn[8]=2; elemConn[9]=3; elemConn[10]=6; elemConn[11]=5;
    elemConn[12]=11; elemConn[13]=12; elemConn[14]=15; elemConn[15]=14;

    // Elem 3
    elemConn[16]=4; elemConn[17]=5; elemConn[18]=8; elemConn[19]=7;
    elemConn[20]=13; elemConn[21]=14; elemConn[22]=17; elemConn[23]=16;

    // Elem 4
    elemConn[24]=5; elemConn[25]=6; elemConn[26]=9; elemConn[27]=8;
    elemConn[28]=14; elemConn[29]=15; elemConn[30]=18; elemConn[31]=17;
    
    //     elemConn=(/1,2,5,4,10,11,14,13, &  ! elem id 1
    //            2,3,6,5,11,12,15,14, &  ! elem id 2
    //            4,5,8,7,13,14,17,16,   &  ! elem id 3
    //            5,6,9,8,14,15,18,17/)  ! elem id 4



  } else if (petCount == 4) {
    // Setup mesh data depending on PET
    if (localPet == 0) { // This part only for PET 0
      
      // Set number of nodes
      numNodes=8;

      // Allocate and fill the node id array.
      nodeIds = (int *) malloc (numNodes * sizeof (int));
      nodeIds[0]=1; nodeIds[1]=2; nodeIds[2]=4;
      nodeIds[3]=5; nodeIds[4]=10; nodeIds[5]=11;
      nodeIds[6]=13; nodeIds[7]=14;

      // Allocate and fill node coordinate array.
      nodeCoords = (double *) malloc (3*numNodes * sizeof (double));
      nodeCoords[0]=0.0; nodeCoords[1]=0.0; nodeCoords[2]=0.0; // Node 1
      nodeCoords[3]=1.0; nodeCoords[4]=0.0; nodeCoords[5]=0.0; // Node 2
      nodeCoords[6]=0.0; nodeCoords[7]=1.0; nodeCoords[8]=0.0; // Node 4
      nodeCoords[9]=1.0; nodeCoords[10]=1.0; nodeCoords[11]=0.0; // Node 5
      nodeCoords[12]=0.0; nodeCoords[13]=0.0; nodeCoords[14]=1.0; // Node 10
      nodeCoords[15]=1.0; nodeCoords[16]=0.0; nodeCoords[17]=1.0; // Node 11
      nodeCoords[18]=0.0; nodeCoords[19]=1.0; nodeCoords[20]=1.0; // Node 13
      nodeCoords[21]=1.0; nodeCoords[22]=1.0; nodeCoords[23]=1.0; // Node 14

      // Allocate and fill the node owner array.
      // Since this part is all on PET 0, it's just set to all 0.
      nodeOwners = (int *) malloc (numNodes * sizeof (int));
      for (int i=0; i<numNodes; i++) {
        nodeOwners[i]=0; // Everything on PET 0
      }
      
      // Set the number of each type of element, plus the total number.
      numElems=1;

      // Allocate and fill the element id array.
      elemIds = (int *) malloc (numElems * sizeof (int));
      elemIds[0]=1;

      // Allocate and fill the element topology type array.
      elemTypes = (int *) malloc (numElems * sizeof (int));
      elemTypes[0]=ESMC_MESHELEMTYPE_HEX;
      
      // Allocate and fill the element connection type array.
      // Note that entry are local indices
      elemConn = (int *) malloc (8 * numElems * sizeof (int));

      // Elem 1
      elemConn[0]=1; elemConn[1]=2; elemConn[2]=4; elemConn[3]=3;
      elemConn[4]=5; elemConn[5]=6; elemConn[6]=8; elemConn[7]=7;
      

      } else if (localPet == 1) { // This part only for PET 1

      // Set number of nodes
      numNodes=8;

      // Allocate and fill the node id array.
      nodeIds = (int *) malloc (numNodes * sizeof (int));
      nodeIds[0]=2; nodeIds[1]=3; nodeIds[2]=5;
      nodeIds[3]=6; nodeIds[4]=11; nodeIds[5]=12;
      nodeIds[6]=14; nodeIds[7]=15;


      // Allocate and fill node coordinate array.
      nodeCoords = (double *) malloc (3*numNodes * sizeof (double));
      nodeCoords[0]=1.0; nodeCoords[1]=0.0; nodeCoords[2]=0.0; // Node 2
      nodeCoords[3]=2.0; nodeCoords[4]=0.0; nodeCoords[5]=0.0; // Node 3
      nodeCoords[6]=1.0; nodeCoords[7]=1.0; nodeCoords[8]=0.0; // Node 5
      nodeCoords[9]=2.0; nodeCoords[10]=1.0; nodeCoords[11]=0.0; // Node 6
      nodeCoords[12]=1.0; nodeCoords[13]=0.0; nodeCoords[14]=1.0; // Node 11
      nodeCoords[15]=2.0; nodeCoords[16]=0.0; nodeCoords[17]=1.0; // Node 12
      nodeCoords[18]=1.0; nodeCoords[19]=1.0; nodeCoords[20]=1.0; // Node 14
      nodeCoords[21]=2.0; nodeCoords[22]=1.0; nodeCoords[23]=1.0; // Node 15

      // Allocate and fill the node owner array.
      nodeOwners = (int *) malloc (numNodes * sizeof (int));
      nodeOwners[0]=0; // Node 2
      nodeOwners[1]=1;  // Node 3
      nodeOwners[2]=0;  // Node 5
      nodeOwners[3]=1;  // Node 6
      nodeOwners[4]=0;  // Node 11
      nodeOwners[5]=1;  // Node 12 
      nodeOwners[6]=0;  // Node 14
      nodeOwners[7]=1;  // Node 15       

      // Set the number of each type of element, plus the total number.
      numElems=1;

      // Allocate and fill the element id array.
      elemIds = (int *) malloc (numElems * sizeof (int));
      elemIds[0]=2;

      // Allocate and fill the element topology type array.
      elemTypes = (int *) malloc (numElems * sizeof (int));
      elemTypes[0]=ESMC_MESHELEMTYPE_HEX;
      
      // Allocate and fill the element connection type array.
      // Note that entry are local indices
      elemConn = (int *) malloc (8 * numElems * sizeof (int));

      // Elem 2
      elemConn[0]=1; elemConn[1]=2; elemConn[2]=4; elemConn[3]=3;
      elemConn[4]=5; elemConn[5]=6; elemConn[6]=8; elemConn[7]=7;

      /// STOPPED HERE ///            

    }  else if (localPet == 2) { // This part only for PET 2

      // Set number of nodes
      numNodes=8;
      
      // Allocate and fill the node id array.
      nodeIds = (int *) malloc (numNodes * sizeof (int));
      nodeIds[0]=4; nodeIds[1]=5; nodeIds[2]=7;
      nodeIds[3]=8; nodeIds[4]=13; nodeIds[5]=14;
      nodeIds[6]=16; nodeIds[7]=17;

      // Allocate and fill node coordinate array.
      nodeCoords = (double *) malloc (3*numNodes * sizeof (double));
      nodeCoords[0]=0.0; nodeCoords[1]=1.0; nodeCoords[2]=0.0; // Node 4
      nodeCoords[3]=1.0; nodeCoords[4]=1.0; nodeCoords[5]=0.0; // Node 5
      nodeCoords[6]=0.0; nodeCoords[7]=2.0; nodeCoords[8]=0.0; // Node 7
      nodeCoords[9]=1.0; nodeCoords[10]=2.0; nodeCoords[11]=0.0; // Node 8
      nodeCoords[12]=0.0; nodeCoords[13]=1.0; nodeCoords[14]=1.0; // Node 13
      nodeCoords[15]=1.0; nodeCoords[16]=1.0; nodeCoords[17]=1.0; // Node 14
      nodeCoords[18]=0.0; nodeCoords[19]=2.0; nodeCoords[20]=1.0; // Node 16
      nodeCoords[21]=1.0; nodeCoords[22]=2.0; nodeCoords[23]=1.0; // Node 17

      // Allocate and fill the node owner array.
      nodeOwners = (int *) malloc (numNodes * sizeof (int));
      nodeOwners[0]=0; // Node 4
      nodeOwners[1]=0;  // Node 5
      nodeOwners[2]=2;  // Node 7
      nodeOwners[3]=2;  // Node 8
      nodeOwners[4]=0;  // Node 13
      nodeOwners[5]=0;  // Node 14 
      nodeOwners[6]=2;  // Node 16
      nodeOwners[7]=2;  // Node 17       

      // Set the number of each type of element, plus the total number.
      numElems=1;

      // Allocate and fill the element id array.
      elemIds = (int *) malloc (numElems * sizeof (int));
      elemIds[0]=3;

      // Allocate and fill the element topology type array.
      elemTypes = (int *) malloc (numElems * sizeof (int));
      elemTypes[0]=ESMC_MESHELEMTYPE_HEX;

      // Allocate and fill the element connection type array.
      // Note that entry are local indices
      elemConn = (int *) malloc (8 * numElems * sizeof (int));

      // Elem 3
      elemConn[0]=1; elemConn[1]=2; elemConn[2]=4; elemConn[3]=3;
      elemConn[4]=5; elemConn[5]=6; elemConn[6]=8; elemConn[7]=7;

    } else if (localPet == 3) { // This part only for PET 3

      // Set number of nodes
      numNodes=8;
      
      // Allocate and fill the node id array.
      nodeIds = (int *) malloc (numNodes * sizeof (int));
      nodeIds[0]=5; nodeIds[1]=6; nodeIds[2]=8;
      nodeIds[3]=9; nodeIds[4]=14; nodeIds[5]=15;
      nodeIds[6]=17; nodeIds[7]=18;

      // Allocate and fill node coordinate array.
      nodeCoords = (double *) malloc (3*numNodes * sizeof (double));
      nodeCoords[0]=1.0; nodeCoords[1]=1.0; nodeCoords[2]=0.0; // Node 5
      nodeCoords[3]=2.0; nodeCoords[4]=1.0; nodeCoords[5]=0.0; // Node 6
      nodeCoords[6]=1.0; nodeCoords[7]=2.0; nodeCoords[8]=0.0; // Node 8
      nodeCoords[9]=2.0; nodeCoords[10]=2.0; nodeCoords[11]=0.0; // Node 9
      nodeCoords[12]=1.0; nodeCoords[13]=1.0; nodeCoords[14]=1.0; // Node 14
      nodeCoords[15]=2.0; nodeCoords[16]=1.0; nodeCoords[17]=1.0; // Node 15
      nodeCoords[18]=1.0; nodeCoords[19]=2.0; nodeCoords[20]=1.0; // Node 17
      nodeCoords[21]=2.0; nodeCoords[22]=2.0; nodeCoords[23]=1.0; // Node 18

      // Allocate and fill the node owner array.
      nodeOwners = (int *) malloc (numNodes * sizeof (int));
      nodeOwners[0]=0;  // Node 5
      nodeOwners[1]=1;  // Node 6
      nodeOwners[2]=2;  // Node 8
      nodeOwners[3]=3;  // Node 9
      nodeOwners[4]=0;  // Node 14
      nodeOwners[5]=1;  // Node 15
      nodeOwners[6]=2;  // Node 17
      nodeOwners[7]=3;  // Node 18       

      // Set the number of each type of element, plus the total number.
      numElems=1;

      // Allocate and fill the element id array.
      elemIds = (int *) malloc (numElems * sizeof (int));
      elemIds[0]=4;

      // Allocate and fill the element topology type array.
      elemTypes = (int *) malloc (numElems * sizeof (int));
      elemTypes[0]=ESMC_MESHELEMTYPE_HEX;

      // Allocate and fill the element connection type array.
      // Note that entry are local indices
      elemConn = (int *) malloc (8 * numElems * sizeof (int));

      // Elem 3
      elemConn[0]=1; elemConn[1]=2; elemConn[2]=4; elemConn[3]=3;
      elemConn[4]=5; elemConn[5]=6; elemConn[6]=8; elemConn[7]=7;
          
    }
  }

  // Add nodes to mesh                                                     
  localrc = ESMC_MeshAddNodes(mesh, numNodes, nodeIds, nodeCoords, nodeOwners);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Add elements and finish the mesh   
  localrc = ESMC_MeshAddElements(mesh, numElems, elemIds, elemTypes, elemConn, NULL, NULL, NULL);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;

  // Get rid of the mesh
  localrc = ESMC_MeshDestroy(&mesh);
  if (localrc != ESMF_SUCCESS) rc=ESMF_FAILURE;
  
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  // Free arrays used to create Mesh
  free(nodeIds);
  free(nodeCoords);
  free(nodeOwners);

  free(elemIds);
  free(elemTypes);
  free(elemConn);

  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
