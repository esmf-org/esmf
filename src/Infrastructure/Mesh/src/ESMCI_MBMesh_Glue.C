// $Id$
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

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <string>
#include <ostream>
#include <iterator>
#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_Glue.h"
#include "Mesh/include/ESMCI_MBMesh_Redist.h"
#include "Mesh/include/ESMCI_MBMesh_Util.h"


#include "MBTagConventions.hpp"
#include "moab/ParallelComm.hpp"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

// #define DEBUG_MASK
// #define DEBUG_OUTPUT
// #define DEBUG_NODE_COORDS
// #define DEBUG_ELEM_COORDS
// #define DEBUG_OWNED


extern "C" void FTN_X(f_esmf_getmeshdistgrid)(int*, int*, int*, int*);

void MBMesh_create(MBMesh **mbmpp, int *pdim, int *sdim,
                   ESMC_CoordSys_Flag *coordSys, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_create()"
  try {
    int localrc, merr;
    
    // Init output
    *mbmpp=NULL;
    
    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    
    //  Error check input //
    if (*pdim > *sdim) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                       " Parametric dimension can't be greater than spatial dimension",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
    
    if ((*pdim < 2) || (*pdim >3)) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                       " Parametric dimension can't be greater than 3D or less than 2D",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
    
    if ((*sdim < 2) || (*sdim >3)) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                       " Spatial dimension can't be greater than 3D or less than 2D",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }

    
    // Create new Mesh
    *mbmpp = new MBMesh(*pdim, *sdim, *coordSys);
    
   
  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  
} // meshcreate


void MBMesh_addnodes(MBMesh **mbmpp, int *_num_nodes, int *nodeId,
                     double *nodeCoord, int *nodeOwner, InterArray<int> *nodeMaskII,
                     ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                     int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_addnodes()"
  try {
    int localrc, merr;
    
    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);


    //// Setup some handy info ////
    
    // Get Moab Mesh wrapper
    ThrowRequire(mbmpp != NULL); 
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    // Get number of nodes
    int num_nodes=*_num_nodes;

    // Get petCount 
    int petCount = VM::getCurrent(&localrc)->getPetCount();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    //// Error check input ////

    // Check node owners
    for (int n = 0; n < num_nodes; ++n) {
      if ((nodeOwner[n]<0) || (nodeOwner[n]>petCount-1)) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                         " bad nodeOwner value ", ESMC_CONTEXT,&localrc)) throw localrc;
      }
    }

    // If present, check mask information
    if (present(nodeMaskII)) { // if masks exist
      // Error checking
      if ((nodeMaskII)->dimCount !=1) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
                                         " nodeMask array must be 1D ", ESMC_CONTEXT,  &localrc)) throw localrc;
      }
      
      if ((nodeMaskII)->extent[0] != num_nodes) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
                                         " nodeMask array must be the same size as the nodeIds array ", ESMC_CONTEXT, &localrc)) throw localrc;
      }
    }


    //// Add nodes ////

    // Create a new set of nodes with basic info
    Range added_nodes;
    mbmp->add_nodes(num_nodes,     
                    nodeCoord,
                    nodeId,          
                    NULL,  // Just use orig_pos starting from 0      
                    nodeOwner,
                    added_nodes); 
    

    // Done creating nodes, so finalize
    mbmp->finalize_nodes();

    //// Setup and fill other node fields ////

    // Setup node mask information
    if (present(nodeMaskII)) { // if masks exist
      
      // Turn on node masks
      mbmp->setup_node_mask();
      
      // Set values in node mask value
      mbmp->set_node_mask_val(added_nodes, nodeMaskII->array);
    }


    //// Debug output //// 
#ifdef ESMF_MBMESH_DEBUG_CREATE_OUTPUT_NODES
    mbmp->debug_output_nodes();
#endif


  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
   if (rc!=NULL) *rc = ESMF_SUCCESS;
}


void MBMesh_addelements(MBMesh **mbmpp,
                        int *_num_elems, int *elemId, 
                        int *elemType, InterArray<int> *_elemMaskII ,
                        int *_areaPresent, double *elemArea,
                        int *_coordsPresent, double *elemCoords,
                        int *_num_elemConn, int *elemConn, int *regridConserve,
                        ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                        int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_addelements()"
  try {

    // Error return vars
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    //// Setup some handy info ////
    ESMCI_MESHCREATE_TRACE_ENTER("MBMesh addelems setup");

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get MBMesh object
    ThrowRequire(mbmpp != NULL); 
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    // Number of elements being created
    int num_elems=*_num_elems;

    // Total Size of connection list
    int num_elemConn=*_num_elemConn;

    // Element mask array
    InterArray<int> *elemMaskII=_elemMaskII;
    int *elemMask=NULL;
    if (present(elemMaskII)) elemMask=elemMaskII->array;

    // Presence of element areas
    int areaPresent=*_areaPresent;

    // Presence of element coords
    int elemCoordsPresent=*_coordsPresent;

    // Coordinate system of mesh
    ESMC_CoordSys_Flag coordSys=mbmp->coordsys;

    // Original spatial dimension of mesh
    int orig_sdim = mbmp->orig_sdim;

    // Get parametric dimension of mesh
    int pdim=mbmp->pdim;

    // Get spatial dimension of mesh
    int sdim=mbmp->pdim;

    // Get number of orig nodes
    int num_nodes = mbmp->num_orig_node();

    //// Error check input ////

    // Make sure that the arrays that should be there when there
    // are elems (num_elems > 0), are there
    if (num_elems > 0) {
      ThrowRequire(elemId != NULL);
      ThrowRequire(elemConn != NULL);
      ThrowRequire(elemType != NULL);
      if (areaPresent) ThrowRequire(elemArea != NULL);
      if (elemCoordsPresent) ThrowRequire(elemCoords != NULL);
    }

    // Check element type
    // (Don't check when pdim=2, because any number of sides is allowed)
    if (pdim==3) {
      for (int i=0; i< num_elems; i++) {
        if ((elemType[i] != 10) && (elemType[i] != 12)) {
          int localrc;
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
             "- for a mesh with parametric dimension 3 element types must be either tetrahedron or hexahedron ",
                                           ESMC_CONTEXT, &localrc)) throw localrc;
         }
      }
    }

    // Check size of connectivity list
    int expected_conn_size=0;
    if (pdim==2) {
      for (int i=0; i< num_elems; i++) {
        expected_conn_size += elemType[i];
      }
    } else if (pdim==3) {
      for (int i=0; i< num_elems; i++) {
        if (elemType[i]==10) expected_conn_size += 4;
        else if (elemType[i]==12) expected_conn_size += 8;
      }
    }

    if (expected_conn_size != num_elemConn) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- element connectivity list doesn't contain the right number of entries ",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }


    // Check to make sure that there are no nodes without a home
    // TODO: Maybe this could be done better by just looping through nodes and
    //       making sure each one appears in elemConn[] 

    // Block so node_used goes away
    { 
      std::vector<int> node_used;
      node_used.resize(num_nodes, 0);
      
      // Error check elemConn array
      int c = 0;
      for (int e=0; e<num_elems; ++e) {
        int nnodes = MBMesh_ElemType2NumNodes(mbmp->pdim,
                                       elemType[e]);
        
        for (int n=0; n<nnodes; ++n) {
          
          // Get 0-based node index
          int node_index=elemConn[c]-1;
          
          // Check elemConn
          if (node_index < 0) {
            int localrc;
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                             "- elemConn entries should not be less than 1 ",
                                             ESMC_CONTEXT, &localrc)) throw localrc;
          }
          
          if (node_index > num_nodes-1) {
            int localrc;
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                             "- elemConn entries should not be greater than number of nodes on processor ",
                                             ESMC_CONTEXT, &localrc)) throw localrc;
          }
          
          // Mark as used
          node_used[node_index]=1;
          
          // Advance to next
          c++;
        }
      } // for e
      
      // Make sure every node used
      bool every_node_used=true;
      for (int i=0; i<num_nodes; i++) {
        if (node_used[i] == 0) {
          every_node_used=false;
          break;
        }
      }
      
      if (!every_node_used) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                         "- there are nodes on this PET that were not used in the element connectivity list ",
                                         ESMC_CONTEXT, &localrc)) throw localrc;
      }
      
    } // Block so node_used goes away


    // Check element mask info
    if (present(elemMaskII)) { // if masks exist
      // Error checking
      if (elemMaskII->dimCount !=1) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- elementMask array must be 1D ", ESMC_CONTEXT,  &localrc)) throw localrc;
      }

      if (elemMaskII->extent[0] != num_elems) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- elementMask array must be the same size as elementIds array ", ESMC_CONTEXT, &localrc)) throw localrc;
      }
     }

     ESMCI_MESHCREATE_TRACE_EXIT("MBMesh addelems setup");

     /// Add orig_pos array to track the original position of elems in input lists
 
     // Allocate memory for orig_pos
     // orig_pos is the pointer that can be switched to different memory later
     // orig_pos_alloc always points to this memory for later deleting     
     int *orig_pos_alloc=NULL;
     orig_pos_alloc=new int[num_elems]; 
     int *orig_pos=orig_pos_alloc; 

     // Fill orig_pos 
     for (int i=0; i<num_elems; i++) {
       orig_pos[i]=i;
     }

     //// See if there are any split elements, if so generate split information ////

     // Check for split elems
     bool is_split=false;
     bool is_split_local=false;     
     MBMesh_detect_split_elems(pdim, num_elems, elemType, elemConn, 
                         is_split_local, is_split);

     // Set mesh split status
     mbmp->is_split=is_split;

     // If there are split elems on any pet, then get info
     int num_elems_wsplit=0;
     int *elemConn_wsplit=NULL;
     int *elemType_wsplit=NULL;
     int *elemId_wsplit=NULL;
     double *elemArea_wsplit=NULL;
     double *elemCoords_wsplit=NULL;
     int *elemMask_wsplit=NULL;
     int *orig_pos_wsplit=NULL;

     if (is_split) {

       // If there are local split elems, allocate and fill node coords
       double *nodeCoords=NULL;
       if (is_split_local) {

         // Get vector of orig_nodes
         std::vector<EntityHandle> orig_nodes;
         mbmp->get_sorted_orig_nodes(orig_nodes);
         
         // Allocate space for node coords
         if (orig_nodes.size() > 0) {
           
           // 3 because in MOAB 3 coords are stored per vert
           nodeCoords=new double[3*orig_nodes.size()];
           
           // Get coords from MOAB
           merr=mbmp->mesh->get_coords(&orig_nodes[0], orig_nodes.size(), nodeCoords);
           ESMC_CHECK_MOAB_THROW(merr);      
         }
       }
              
       // Generate split info 
       MBMesh_generate_info_for_split_elems(// In
                                            is_split_local,pdim, orig_sdim, sdim, 
                                            num_elems, elemId, elemType, orig_pos, elemMask,
                                            areaPresent, elemArea, elemCoordsPresent, elemCoords,
                                            num_elemConn, elemConn, nodeCoords, 
                                            
                                            // Out
                                            mbmp->max_non_split_id, mbmp->split_to_orig_id,  mbmp->split_id_to_frac,
                                            num_elems_wsplit, elemId_wsplit, elemType_wsplit, orig_pos_wsplit, 
                                            elemMask_wsplit, elemArea_wsplit, elemCoords_wsplit, elemConn_wsplit);

       // If there was a local split elem, then use the split info for creating elems below. 
       if (is_split_local) {
         num_elems=num_elems_wsplit;
         elemConn=elemConn_wsplit;
         elemType=elemType_wsplit;
         orig_pos=orig_pos_wsplit,
         elemId=elemId_wsplit;
         elemMask=elemMask_wsplit;
         if (areaPresent) elemArea=elemArea_wsplit;
         if (elemCoordsPresent) elemCoords=elemCoords_wsplit;
       }

       // Get rid of nodeCoords
       if (is_split_local) {
         delete [] nodeCoords;
       }
     } // if (is_split)
     

    //// Add elems and elem fields ////
    ESMCI_MESHCREATE_TRACE_ENTER("MBMesh addelems add");
    
    // Turn on element masking 
    if (present(elemMaskII)) { 
      mbmp->setup_elem_mask();
    }
    
    // Turn on element areas
    if (areaPresent) { 
      mbmp->setup_elem_area();
    }
    
    // Turn on element coordinates
    if (elemCoordsPresent) { 
      mbmp->setup_elem_coords();
    }

    // Add elements in groups by type, also set optional fields in elements      
    MBMesh_add_elems_in_groups_by_type(mbmp, localPet, 
                                       num_elems, elemId, elemType, orig_pos, elemMask, NULL,
                                       areaPresent, elemArea, elemCoordsPresent, elemCoords,
                                       elemConn);

    ESMCI_MESHCREATE_TRACE_EXIT("MBMesh addelems add");
    
    // Done creating elems, so finalize
    mbmp->finalize_elems();

    //// Setup parallel sharing ///
    // DON'T DO, BECAUSE IT ISN'T NEEDED ALL THE TIME
    // mbmp->setup_parallel();

    // Debug output of all elems on this processor
#ifdef ESMF_MBMESH_DEBUG_CREATE_OUTPUT_ELEMS
    mbmp->debug_output_elems();
#endif

  // Get rid of extra memory for split elements
  if (is_split_local) {
    delete [] elemConn_wsplit;
    delete [] elemType_wsplit;
    delete [] elemId_wsplit;
    delete [] orig_pos_wsplit;
    if (present(elemMaskII)) delete [] elemMask_wsplit;
    if (areaPresent) delete [] elemArea_wsplit;
    if (elemCoordsPresent) delete [] elemCoords_wsplit;
  }

  // Get rid of orig_pos
  delete [] orig_pos_alloc;

  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}



void MBMesh_turnonnodemask(MBMesh **mbmpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_turnonnodemask()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // If no mask values then leave
    if (!present(maskValuesArg)) {
      // Set return code
      if (rc!=NULL) *rc = ESMF_SUCCESS;

      // Leave
      return;
    }

    // Get mask values
    int numMaskValues=(maskValuesArg)->extent[0];
    int *ptrMaskValues=&((maskValuesArg)->array[0]);

    // If has masks
    if (mbmp->has_node_mask) {

      // Get a range containing all nodes
      Range range_node;
      merr=moab_mesh->get_entities_by_dimension(0,0,range_node);
      ESMC_CHECK_MOAB_THROW(merr);

      // Loop through elements setting values
      for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
        const EntityHandle node=*it;

        // Get mask value
        int mv;
        merr=moab_mesh->tag_get_data(mbmp->node_mask_val_tag, &node, 1, &mv);
        ESMC_CHECK_MOAB_THROW(merr);

        // See if mv matches any mask values
        int masked=0;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];
          if (mv==mvi) {
            masked=1;
            break;
          }
        }

        // Set global id
        merr=moab_mesh->tag_set_data(mbmp->node_mask_tag, &node, 1, &masked);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

// Turn OFF masking
 void MBMesh_turnoffnodemask(MBMesh **mbmpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_turnoffnodemask()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // If has masks
    if (mbmp->has_node_mask) {
      // Get a range containing all nodes
      Range range_node;
      merr=moab_mesh->get_entities_by_dimension(0,0,range_node);
      ESMC_CHECK_MOAB_THROW(merr);

      // Loop through elements setting values
      for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
        const EntityHandle node=*it;

        // unset masked value
        int masked=0;
        merr=moab_mesh->tag_set_data(mbmp->node_mask_tag, &node, 1, &masked);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

void MBMesh_turnonelemmask(MBMesh **mbmpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_turnonelemmask()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // If no mask values then leave
    if (!present(maskValuesArg)) {
      // Set return code
      if (rc!=NULL) *rc = ESMF_SUCCESS;

      // Leave
      return;
    }


    // Get mask values
    int numMaskValues=(maskValuesArg)->extent[0];
    int *ptrMaskValues=&((maskValuesArg)->array[0]);

    // If has masks
    if (mbmp->has_elem_mask) {

      // Get a range containing all elements
      Range range_elem;
      merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
      ESMC_CHECK_MOAB_THROW(merr);

      // Loop through elements setting values
      for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
        const EntityHandle elem=*it;

        // Get mask value
        int mv;
        merr=moab_mesh->tag_get_data(mbmp->elem_mask_val_tag, &elem, 1, &mv);
        if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        }

        // See if mv matches any mask values
        int masked=0;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];
          if (mv==mvi) {
            masked=1;
            break;
          }
        }

        // Set global id
        merr=moab_mesh->tag_set_data(mbmp->elem_mask_tag, &elem, 1, &masked);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

// Turn OFF masking
void MBMesh_turnoffelemmask(MBMesh **mbmpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_turnoffelemmask()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // If has masks
    if (mbmp->has_elem_mask) {
      // Get a range containing all elements
      Range range_elem;
      merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
      ESMC_CHECK_MOAB_THROW(merr);

      // Loop through elements setting values
      for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
        const EntityHandle elem=*it;

        // unset masked value
        int masked=0;
        merr=moab_mesh->tag_set_data(mbmp->elem_mask_tag, &elem, 1, &masked);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}



void MBMesh_destroy(MBMesh **mbmpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_destroy()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    // Get rid of MBMesh
    delete mbmp;

    // Set to null
    *mbmpp=NULL;

  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}



void MBMesh_write(MBMesh **mbmpp, char *fname, int *rc,
    ESMCI_FortranStrLenArg nlen) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_write()"
  try {

    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get petCount
    int petCount = VM::getCurrent(&localrc)->getPetCount();
    ESMC_CHECK_PASSTHRU_THROW(localrc);


    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Make c format string
    char *filename = ESMC_F90toCstring(fname, nlen);

    // Add vtk
#define FILENAME_W_VTK_MAX 1024
    char filename_w_vtk[FILENAME_W_VTK_MAX];
    if (strlen(filename)+4 > FILENAME_W_VTK_MAX) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
                                     " filename too long ", ESMC_CONTEXT, rc);
      return;
    }
#undef FILENAME_W_VTK_MAX

    // Write file name (add pet info if > 1 PET)
    if (petCount > 1) {
      sprintf(filename_w_vtk,"%s.%d.%d.vtk",filename,petCount,localPet);
    } else {
      sprintf(filename_w_vtk,"%s.vtk",filename);
    }

    // Call into MOAB
    int merr=moab_mesh->write_file(filename_w_vtk,NULL,NULL);
    ESMC_CHECK_MOAB_THROW(merr);

    // get rid of c format string
    delete [] filename;

  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}


void MBMesh_createnodedistgrid(MBMesh **mbmpp, int *ngrid, int *num_lnodes, int *rc) {

#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createnodedistgrid()"

  // Declare id vectors
  std::vector<int> ngids;

  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get a range containing all nodes
    Range range_node;
    merr=mbmp->mesh->get_entities_by_dimension(0,0,range_node);
    ESMC_CHECK_MOAB_THROW(merr);

    for(Range::const_iterator it=range_node.begin(); it !=range_node.end(); it++) {
      const EntityHandle *node=&(*it);

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mbmp->owner_tag, node, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // If owned by this processor, put in list
      if (owner==localPet) {
        // Get gid
        int gid;
        merr=moab_mesh->tag_get_data(mbmp->gid_tag, node, 1, &gid);
        ESMC_CHECK_MOAB_THROW(merr);

        // Stick in list
        ngids.push_back(gid);
      }
    }

  } 
  CATCH_MBMESH_RETURN(rc);

  // Create the distgrids
  {
    int nsize = *num_lnodes = ngids.size();
    int rc1;

    int *indices = (nsize==0)?NULL:&ngids[0];

    FTN_X(f_esmf_getmeshdistgrid)(ngrid, &nsize, indices, &rc1);

    if (ESMC_LogDefault.MsgFoundError(rc1,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
  }

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

/**
 * Sort elements by the order in which they were originally declared
 * (which is stored by get_data_index)
 * Don't include split elements
 */

// DO THIS BETTER, HAVE A FIELD THAT CONTAINS THE POSITION IN THE FINAL ARRAY AND -1 FOR ANYTHING NOT LOCAL OR SPLIT
void getElemGIDS(MBMesh *mbmp, std::vector<int> &egids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "getElemGIDS()"
  try {
    int localrc, merr;

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get a range containing all elements
    Range range_elem;
    merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through elements putting into list
    std::vector<std::pair<int,int> > pos_and_gids;
    for(Range::const_iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
      const EntityHandle *elemp=(&*it);

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mbmp->owner_tag, elemp, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // If owned by this processor, put in list
      if (owner==localPet) {
        // Get gid
        int gid;
        merr=moab_mesh->tag_get_data(mbmp->gid_tag, elemp, 1, &gid);
        ESMC_CHECK_MOAB_THROW(merr);

        // Don't do split elements
        if (mbmp->is_split && gid > mbmp->max_non_split_id) continue;

        // Get orig_pos
        int orig_pos;
        merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, elemp, 1, &orig_pos);
        ESMC_CHECK_MOAB_THROW(merr);

        // Stick in list
        pos_and_gids.push_back(std::make_pair(orig_pos,gid));
      }
    }

    // Put in order by original pos
    std::sort(pos_and_gids.begin(), pos_and_gids.end());

    // Fill array of element gids
    egids.clear();
    for (int i = 0; i<pos_and_gids.size(); ++i) {
      egids.push_back(pos_and_gids[i].second);

      // printf("pos=%d egids=%d\n",pos_and_gids[i].first,pos_and_gids[i].second);

    }
  }
  CATCH_MBMESH_RETHROW
}


void MBMesh_createelemdistgrid(MBMesh **mbmpp, int *egrid, int *num_lelems, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createelemdistgrid()"

  // Declare id vectors
  std::vector<int> egids;

  try {

    int localrc;
    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    // Get list of elem gids in order
    getElemGIDS(mbmp, egids);

  } 
  CATCH_MBMESH_RETURN(rc);

  {
    int esize = *num_lelems = egids.size();
    int rc1;

    int *indices = (esize==0)?NULL:&egids[0];

    FTN_X(f_esmf_getmeshdistgrid)(egrid, &esize, indices, &rc1);

    if(ESMC_LogDefault.MsgFoundError(rc1,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
  }

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}


// DO THIS BETTER, HAVE A FIELD THAT CONTAINS THE POSITION IN THE FINAL ARRAY AND -1 FOR ANYTHING NOT LOCAL OR SPLIT
void getElems(MBMesh **mbmpp, std::vector<EntityHandle> &ehs) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_getElems()"
  try {
    // Get localPet
    int localrc, merr;
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get a range containing all elements
    Range range_elem;
    merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through elements putting into list
    std::vector<std::pair<int,EntityHandle> > pos_and_elems;
    for(Range::const_iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
      const EntityHandle *elemp=(&*it);
      EntityHandle elem=*it;

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mbmp->owner_tag, elemp, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // If owned by this processor, put in list
      if (owner==localPet) {
        // Get gid
        int gid;
        merr=moab_mesh->tag_get_data(mbmp->gid_tag, elemp, 1, &gid);
        ESMC_CHECK_MOAB_THROW(merr);

        // Get orig_pos
        int orig_pos;
        merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, elemp, 1, &orig_pos);
        ESMC_CHECK_MOAB_THROW(merr);

        // Stick in list
        pos_and_elems.push_back(std::make_pair(orig_pos,elem));
      }
    }

    // Put in order by original pos
    std::sort(pos_and_elems.begin(), pos_and_elems.end());

    // Fill array of element gids
    ehs.clear();
    for (int i = 0; i<pos_and_elems.size(); ++i) {
      ehs.push_back(pos_and_elems[i].second);

      // printf("pos=%d egids=%d\n",pos_and_gids[i].first,pos_and_gids[i].second);

    }
  } 
  CATCH_MBMESH_RETHROW
}


void MBMesh_getlocalelemcoords(MBMesh **mbmpp, double *ecoords,
                               int *_orig_sdim, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_getlocalelemcoords()"
  try {
    int localrc,merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

      // Get Moab Mesh wrapper
      MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

      // Make sure that there are element coords
      if (!mbmp->has_elem_coords) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                         "- this mesh doesn't contain element coordinates.",
                                         ESMC_CONTEXT, &localrc)) throw localrc;
      }

      //Get MOAB Mesh
      Interface *moab_mesh=mbmp->mesh;

      // Get original spatial dim
      int orig_sdim=*_orig_sdim;

      // Declare id vector
      std::vector<EntityHandle> ehs;

      // Get local elems in correct order
      getElems(mbmpp, ehs);

      // Loop through elements and put coordss into array
      if (mbmp->has_elem_orig_coords) {
        for (int i=0; i<ehs.size(); i++) {
          // Get element gid
          EntityHandle elem=ehs[i];

          // Get orig_pos
          merr=moab_mesh->tag_get_data(mbmp->elem_orig_coords_tag,
                                       &elem, 1, ecoords+orig_sdim*i);
          ESMC_CHECK_MOAB_THROW(merr);
        }
      } else {
        for (int i=0; i<ehs.size(); i++) {
          // Get element gid
          EntityHandle elem=ehs[i];

          // Get orig_pos
          merr=moab_mesh->tag_get_data(mbmp->elem_coords_tag,
                                       &elem, 1, ecoords+orig_sdim*i);
          ESMC_CHECK_MOAB_THROW(merr);
        }
      }
    } 
    CATCH_MBMESH_RETURN(rc);

    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;
}


void MBMesh_getarea(MBMesh **mbmpp, int *num_elem, double *elem_areas, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_getarea()"

  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];
  double tmp_coords[MAX_NUM_POLY_COORDS];

  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Declare id vector
    std::vector<EntityHandle> ehs;

    // get elem ids
    // TODO: IN FUTURE MAYBE JUST USE DATA INDEX DIRECTY, ALTHOUGH THEN HAVE TO STICK
    //       SPLIT ELEMENTS AT END
    getElems(mbmpp, ehs);


    // If there are no elements then leave
    if (ehs.empty()) {
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return;
    }

    // Check size
    if (*num_elem != ehs.size()) {
      Throw() << "Number of elements doesn't match size of input array for areas";
    }


    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get dimensions
    int sdim=mbmp->sdim;
    int pdim=mbmp->pdim;


    // Put areas into array
    if (mbmp->has_elem_area) {

      merr=moab_mesh->tag_get_data(mbmp->elem_area_tag, &ehs[0], ehs.size(), elem_areas);
      ESMC_CHECK_MOAB_THROW(merr);

    } else {
      for (int i=0; i<ehs.size(); i++) {

        // Get element
        EntityHandle elem=ehs[i];

        // Compute area depending on dimensions
        double area;
        if (pdim==2) {
          if (sdim==2) {
            MBMesh_get_elem_coords(mbmp, elem, MAX_NUM_POLY_NODES_2D, &num_poly_nodes, poly_coords);
            remove_0len_edges2D(&num_poly_nodes, poly_coords);
            area=area_of_flat_2D_polygon(num_poly_nodes, poly_coords);
          } else if (sdim==3) {
            MBMesh_get_elem_coords(mbmp, elem, MAX_NUM_POLY_NODES_3D, &num_poly_nodes, poly_coords);
            //get_elem_coords_3D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_3D, tmp_coords, &num_poly_nodes, poly_coords);
            remove_0len_edges3D(&num_poly_nodes, poly_coords);
            area=great_circle_area(num_poly_nodes, poly_coords);
          }
        } else if (pdim==3) {
          if (sdim==3) {
            Throw() << "Meshes with parametric dimension == 3 and spatial dimension = 3 not supported in MOAB";
            // Phedra tmp_phedra=create_phedra_from_elem(&elem, cfield);
            //area=tmp_phedra.calc_volume();
          } else {
            Throw() << "Meshes with parametric dimension == 3, but spatial dim != 3 not supported for computing areas";
          }
        } else {
          Throw() << "Meshes with parametric dimension != 2 or 3 not supported for computing areas";
        }

        // Put area into area array
        elem_areas[i]=area;
      }
    }



#if 0
    // Add in the split elements
    if (mesh.is_split) {
      std::map<int,int> id_to_index;
      for (int i=0; i<egids.size(); i++) {
        id_to_index[egids[i]]=i;
      }

      // Iterate through split elements adding in area
      Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;

        // Don't do non-local elements
        if (!GetAttr(elem).is_locally_owned()) continue;

        // Get the element id
        int eid=elem.get_id();

        // Skip non-split elements
        if (!(eid > mesh.max_non_split_id)) continue;

        // Compute area depending on dimensions
        double area;
        if (pdim==2) {
          if (sdim==2) {
            get_elem_coords_2D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_2D, tmp_coords, &num_poly_nodes, poly_coords);
            remove_0len_edges2D(&num_poly_nodes, poly_coords);
            area=area_of_flat_2D_polygon(num_poly_nodes, poly_coords);
          } else if (sdim==3) {
            get_elem_coords_3D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_3D, tmp_coords, &num_poly_nodes, poly_coords);
            remove_0len_edges3D(&num_poly_nodes, poly_coords);
            area=great_circle_area(num_poly_nodes, poly_coords);
          }
        } else if (pdim==3) {
          if (sdim==3) {
            Phedra tmp_phedra=create_phedra_from_elem(&elem, cfield);
            area=tmp_phedra.calc_volume();
          } else {
            Throw() << "Meshes with parametric dimension == 3, but spatial dim != 3 not supported for computing areas";
          }
        } else {
          Throw() << "Meshes with parametric dimension != 2 or 3 not supported for computing areas";
        }

        // Get original id
        int orig_id=mesh.split_to_orig_id[eid];

        // Get index
        int index=id_to_index[orig_id];

        // Add area to what's already there
        elem_areas[index] += area;
      }
    }
#endif

  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;


  // Declare polygon information
#undef  MAX_NUM_POLY_COORDS
#undef  MAX_NUM_POLY_NODES_2D
#undef  MAX_NUM_POLY_NODES_3D

}

// DO THIS BETTER, HAVE A FIELD THAT CONTAINS THE POSITION IN THE FINAL ARRAY AND -1 FOR ANYTHING NOT LOCAL OR SPLIT
void getNodes(MBMesh **mbmpp, std::vector<EntityHandle> &nodes) {
#undef  ESMC_METHOD
#define ESMC_METHOD "getNodes()"
  try {
    int localrc, merr;

    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get a range containing all nodes
    Range range_node;
    merr=moab_mesh->get_entities_by_dimension(0,0,range_node);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through nodes putting into list
    std::vector<std::pair<int,EntityHandle> > pos_and_nodes;
    for(Range::const_iterator it=range_node.begin(); it !=range_node.end(); it++) {
      const EntityHandle *nodep=(&*it);
      EntityHandle node=*it;

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mbmp->owner_tag, nodep, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // If owned by this processor, put in list
      if (owner==localPet) {
        // Get gid
        int gid;
        merr=moab_mesh->tag_get_data(mbmp->gid_tag, nodep, 1, &gid);
        ESMC_CHECK_MOAB_THROW(merr);

        // Get orig_pos
        int orig_pos;
        merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, nodep, 1, &orig_pos);
        ESMC_CHECK_MOAB_THROW(merr);

        // Stick in list
        pos_and_nodes.push_back(std::make_pair(orig_pos,node));
      }
    }

    // Put in order by original pos
    std::sort(pos_and_nodes.begin(), pos_and_nodes.end());

    // Fill array of node entities
    nodes.clear();
    for (int i = 0; i<pos_and_nodes.size(); ++i) {
      nodes.push_back(pos_and_nodes[i].second);

      // printf("pos=%d ngids=%d\n",pos_and_gids[i].first,pos_and_gids[i].second);

    }
  } 
  CATCH_MBMESH_RETHROW
}

void MBMesh_getlocalcoords(MBMesh **mbmpp, double *ncoords, int *_orig_sdim, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_getlocalcoords()"
  try {
    int localrc, merr;

    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get dimensions
    int sdim=mbmp->sdim;
    int pdim=mbmp->pdim;

    // Get original spatial dim
    int orig_sdim=*_orig_sdim;

    // Declare id vector
    std::vector<EntityHandle> nodes;

    // Get local nodes in correct order
    getNodes(mbmpp, nodes);

    // Loop through nodes and put coords into array
    if (mbmp->has_node_orig_coords) {
      for (int i=0; i<nodes.size(); i++) {
        // Get node
        EntityHandle node=nodes[i];

        // Get coords
        merr=moab_mesh->tag_get_data(mbmp->node_orig_coords_tag,
                                     &node, 1, ncoords+orig_sdim*i);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    } else {
      for (int i=0; i<nodes.size(); i++) {
        // Get node
        EntityHandle node=nodes[i];

        // Get coords
        merr=moab_mesh->get_coords(&node, 1, ncoords+orig_sdim*i);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

// Set data in Array based on typekind
template <class TYPE>
void MBMesh_set_Array_data(LocalArray *localArray, int index, ESMC_TypeKind_Flag typekind, TYPE data) {

  if (typekind == ESMC_TYPEKIND_I4) {
    ESMC_I4 data_i4=static_cast<ESMC_I4>(data);
    localArray->setData(&index, data_i4);
  } else if (typekind == ESMC_TYPEKIND_R4) {
    ESMC_R4 data_r4=static_cast<ESMC_R4>(data);
    localArray->setData(&index, data_r4);
  } else if (typekind == ESMC_TYPEKIND_R8) {
    ESMC_R8 data_r8=static_cast<ESMC_R8>(data);
    localArray->setData(&index, data_r8);
  } else {
    int localrc;
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                  " unsupported typekind in Array.",
                                  ESMC_CONTEXT, &localrc);
    throw localrc;
  }
}

template void MBMesh_set_Array_data(LocalArray *localArray, int index, ESMC_TypeKind_Flag typekind, double data);

template void MBMesh_set_Array_data(LocalArray *localArray, int index, ESMC_TypeKind_Flag typekind, int data);



void MBMesh_geteleminfointoarray(MBMesh *vmbmp,
                                 ESMCI::DistGrid *elemDistgrid, 
                                 int numElemArrays,
                                 int *infoTypeElemArrays, 
                                 ESMCI::Array **elemArrays, 
                                 int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_geteleminfointoarray()"

// Must match with ESMF_MeshGet()
#define INFO_TYPE_ELEM_ARRAYS_MASK 1
#define INFO_TYPE_ELEM_ARRAYS_AREA 2
#define INFO_TYPE_ELEM_ARRAYS_MAX  2

  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Make sure that incoming Array distgrids match element distgrid
    for (int i=0; i<numElemArrays; i++) {

      // Get match
      DistGridMatch_Flag matchflag=DistGrid::match(elemArrays[i]->getDistGrid(),elemDistgrid,&localrc);
      ESMC_CHECK_PASSTHRU_THROW(localrc);

      // Complain if it doesn't match sufficiently
      if ((matchflag != DISTGRIDMATCH_EXACT) && (matchflag != DISTGRIDMATCH_ALIAS)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                      " DistGrid in element information Array doesn't match Mesh element DistGrid.",
                                      ESMC_CONTEXT, &localrc);
        throw localrc;
      }
    }

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*>(vmbmp);

    // Get the fields, Arrays for the various types of info
    ESMCI::Array *elem_mask_Array=NULL;
    ESMCI::Array *elem_area_Array=NULL;

    for (int i=0; i<numElemArrays; i++) {
      if (infoTypeElemArrays[i] == INFO_TYPE_ELEM_ARRAYS_MASK) { 
        if (!mbmp->has_elem_mask) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                        " mesh doesn't contain element mask information.",
                                        ESMC_CONTEXT, &localrc);
          throw localrc;
        }

        // Get array pointer
        elem_mask_Array=elemArrays[i];

        // Complain if the array has more than rank 1
        if (elem_mask_Array->getRank() != 1) Throw() << "this call currently can't handle Array rank != 1";
      }

      if (infoTypeElemArrays[i] == INFO_TYPE_ELEM_ARRAYS_AREA) { 
        if (!mbmp->has_elem_area) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                        " mesh doesn't contain element area information.",
                                        ESMC_CONTEXT, &localrc);
          throw localrc;
        }

        // Get array pointer
        elem_area_Array=elemArrays[i];

        // Complain if the array has more than rank 1                                 
        if (elem_area_Array->getRank() != 1) Throw() << "this call currently can't handle Array rank != 1";

        // Complain if the Mesh is split 
        if (mbmp->is_split) Throw() << "this call currently can't handle a mesh containing elems with > 4 corners";
      }
    }


    // Get elems
    Range elems;
    mbmp->get_all_elems(elems);

    // TODO: MAYBE HAVE THE LIST OF LOCAL elemhandles in order in the mbmesh, that way you could
    //     probably just read/write them quickly in a chunk using one of the MOAB subroutines.
    //     However, until then, this is a good way to do it.

    // Loop and set up gid_to_elem_map
    std::map<int,EntityHandle> gid_to_elem_map;
    for (Range::iterator it=elems.begin(); it !=elems.end(); it++) {
      EntityHandle elem=*it;
      
      // Get node global id
      int gid=mbmp->get_gid(elem);
      
      // Add to map
      gid_to_elem_map[gid]=elem;
    }

    // Get LocalDeCount
    int localDECount=elemDistgrid->getDELayout()->getLocalDeCount();

    // Loop filling local DEs
    for (int lDE=0; lDE<localDECount; lDE++) {

      // Get sequence indices
      std::vector<int> seqIndexList;
      elemDistgrid->fillSeqIndexList(seqIndexList, lDE, 1);

      // Get mask if needed
      if (elem_mask_Array) {
        // Get the array info
        LocalArray *localArray=(elem_mask_Array->getLocalarrayList())[lDE];

        // Get localDE lower bound
        int lbound=(elem_mask_Array->getComputationalLBound())[lDE]; // (assumes array rank is 1)

        // Typekind
        ESMC_TypeKind_Flag typekind=elem_mask_Array->getTypekind();

        // Loop seqIndices
        for (int i=0; i<seqIndexList.size(); i++) {
          int si=seqIndexList[i];

          // Get elem with si as gid
          std::map<int,EntityHandle>::iterator mi =  gid_to_elem_map.find(si);
          
          // If it doesn't exist, then go to next
          if (mi == gid_to_elem_map.end()) {
            Throw() << "element with that id not found in mesh";
          }
          
          // Get elem 
          EntityHandle elem=mi->second;

          // Get elem mask value
          int mask_val=mbmp->get_elem_mask_val(elem);
     
          // Location in array
          int index=i+lbound;
          
          // Convert and set data
          MBMesh_set_Array_data(localArray, index, typekind, mask_val);
        }
      }

      // Get area if needed
      if (elem_area_Array) {
        // Get the array info
        LocalArray *localArray=(elem_area_Array->getLocalarrayList())[lDE];

        // Get localDE lower bound
        int lbound=(elem_area_Array->getComputationalLBound())[lDE]; // (assumes array rank is 1)
        // Typekind
        ESMC_TypeKind_Flag typekind=elem_area_Array->getTypekind();

        // Loop seqIndices
        for (int i=0; i<seqIndexList.size(); i++) {
          int si=seqIndexList[i];

          // Get elem with si as gid
          std::map<int,EntityHandle>::iterator mi =  gid_to_elem_map.find(si);
          
          // If it doesn't exist, then go to next
          if (mi == gid_to_elem_map.end()) {
            Throw() << "element with that id not found in mesh";
          }
          
          // Get elem 
          EntityHandle elem=mi->second;

          // Get elem area
          double area= mbmp->get_elem_area(elem);
     
          // Location in array
          int index=i+lbound;
          
          // Convert and set data
          MBMesh_set_Array_data(localArray, index, typekind, area);
        }
      }
    }

  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

#undef INFO_TYPE_ELEM_ARRAYS_MASK 
#undef INFO_TYPE_ELEM_ARRAYS_AREA 
#undef INFO_TYPE_ELEM_ARRAYS_MAX  
}


void MBMesh_serialize(MBMesh **mbmpp, char *buffer, int *length, 
                      int *offset, ESMC_InquireFlag *inquireflag, int *rc,
                      ESMCI_FortranStrLenArg buffer_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_serialize()"
  try {

    int localrc;
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    int *ip;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *mesh=mbmp->mesh;

    // Calc Size
    int size = 0;

    // TODO: verify length > vars.
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < size) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add Mesh object", ESMC_CONTEXT, rc);
         return;
      }
    }

    // Save integers
    ip= (int *)(buffer + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = mbmp->sdim;
      size++;
      *ip++ = mbmp->pdim;
      size++;
      *ip++ = mbmp->orig_sdim;
      size++;
      *ip++ = mbmp->coordsys;
      size++;
      *ip++ = mbmp->has_node_orig_coords;
      size++;
      *ip++ = mbmp->has_node_mask;
      size++;
      *ip++ = mbmp->has_elem_coords;
      size++;
      *ip++ = mbmp->has_elem_orig_coords;
      size++;
      *ip++ = mbmp->has_elem_mask;
      size++;
      *ip++ = mbmp->has_elem_area;
      size++;
      *ip++ = mbmp->has_elem_frac;
      size++;
      *ip++ = mbmp->is_split;
      size++;
      *ip++ = mbmp->max_non_split_id;
      size++;
    }

    // Adjust offset
    *offset += size * sizeof(int);

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

void MBMesh_deserialize(MBMesh **mbmpp, char *buffer, int *offset, int *rc,
                        ESMCI_FortranStrLenArg buffer_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_deserialize()"
  try {

    int localrc;
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    int *ip;
    int localsize = 0;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get pointer
    ip= (int *)(buffer + *offset);

    // Get values
    int sdim=*ip++;
    localsize++;
    int pdim=*ip++;
    localsize++;
    int orig_sdim=*ip++;
    localsize++;
    ESMC_CoordSys_Flag coordsys=static_cast<ESMC_CoordSys_Flag> (*ip++);
    localsize++;
    bool has_node_orig_coords=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_node_mask=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_coords=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_orig_coords=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_mask=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_area=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_frac=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool is_split=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    int max_non_split_id=static_cast<bool>(*ip++);
    localsize++;

    // Adjust offset
    *offset += localsize*sizeof(int);

    MBMesh_create(mbmpp, &pdim, &orig_sdim, &coordsys, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      return;

    MBMesh *mesh=reinterpret_cast<MBMesh*> (*mbmpp);
    mesh->has_node_orig_coords = has_node_orig_coords;
    mesh->has_node_mask = has_node_mask;
    mesh->has_elem_frac = has_elem_frac;
    mesh->has_elem_mask = has_elem_mask;
    mesh->has_elem_area = has_elem_area;
    mesh->has_elem_coords = has_elem_coords;
    mesh->has_elem_orig_coords = has_elem_orig_coords;
    mesh->is_split = is_split;
    mesh->max_non_split_id = max_non_split_id;

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}


/// REDIST ///

void MBMesh_createredistelems(MBMesh **src_meshpp, int *num_elem_gids, int *elem_gids,
                              MBMesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createredistelems()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    if ((*src_meshpp)->is_split) {
      // If split mesh expand ids
      int num_elem_gids_ws;
      int *elem_gids_ws=NULL;
      std::map<int,int> split_to_orig_id;

#undef debug_printelemgids
#ifdef debug_printelemgids
      for (int i=0; i<*num_elem_gids; ++i)
        printf("%d# elem gids %d\n", localPet, elem_gids[i]);
#endif

      mbmesh_expand_split_elem_ids(*src_meshpp, *num_elem_gids, elem_gids, 
                                   &num_elem_gids_ws, &elem_gids_ws, split_to_orig_id);

      mbmesh_redist_elem(*src_meshpp, &num_elem_gids_ws, elem_gids_ws, output_meshpp);

      (*output_meshpp)->is_split=(*src_meshpp)->is_split;
      (*output_meshpp)->max_non_split_id=(*src_meshpp)->max_non_split_id;
      (*output_meshpp)->split_to_orig_id=split_to_orig_id;

      // calculate split_id_to_frac map from other info
      mbmesh_calc_split_id_to_frac(*output_meshpp);

      // free split gids
      if (elem_gids_ws !=NULL) delete [] elem_gids_ws;
    } else {
      mbmesh_redist_elem(*src_meshpp, num_elem_gids, elem_gids, output_meshpp);
      (*output_meshpp)->is_split=(*src_meshpp)->is_split;
    }

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

void MBMesh_createredistnodes(MBMesh **src_meshpp, int *num_node_gids, int *node_gids,
                              MBMesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createredistnodes()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    mbmesh_redist_node(*src_meshpp, num_node_gids, node_gids, output_meshpp);

    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh split id postprocessing");
    // split element handling
    (*output_meshpp)->is_split=(*src_meshpp)->is_split;

    if ((*output_meshpp)->is_split) {
      mbmesh_set_split_orig_id_map(*src_meshpp, *output_meshpp);
      (*output_meshpp)->max_non_split_id=(*src_meshpp)->max_non_split_id;
      // RLO: not sure we need this if above is used
      // (*output_meshpp)->split_to_orig_id=(*src_meshpp)->split_to_orig_id;
    }
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh split id postprocessing");

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

void MBMesh_createredist(MBMesh **src_meshpp, int *num_node_gids, int *node_gids,
                         int *num_elem_gids, int *elem_gids,  
                         MBMesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createredist()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    if ((*src_meshpp)->is_split) {
      // If split mesh expand ids
      int num_elem_gids_ws;
      int *elem_gids_ws=NULL;
      std::map<int,int> split_to_orig_id;

      mbmesh_expand_split_elem_ids((*src_meshpp), *num_elem_gids, elem_gids, 
                                   &num_elem_gids_ws, &elem_gids_ws, split_to_orig_id);

      mbmesh_redist((*src_meshpp), num_node_gids, node_gids, 
                    &num_elem_gids_ws, elem_gids_ws, output_meshpp);
 
      (*output_meshpp)->is_split=(*src_meshpp)->is_split;
      (*output_meshpp)->max_non_split_id=(*src_meshpp)->max_non_split_id;
      (*output_meshpp)->split_to_orig_id=split_to_orig_id;

      // calculate split_id_to_frac map from other info
      mbmesh_calc_split_id_to_frac(*output_meshpp);

      // Free split gids
      if (elem_gids_ws !=NULL) delete [] elem_gids_ws;
    } else {
      mbmesh_redist((*src_meshpp), num_node_gids, node_gids, 
                    num_elem_gids, elem_gids, output_meshpp);
      (*output_meshpp)->is_split=(*src_meshpp)->is_split;
    }

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}


// This method verifies that nodes in node_gids array are the same as the local nodes in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of nodes in both cases are the same and that every
// entry in node_gids is contained in meshpp
void MBMesh_checknodelist(MBMesh **meshpp, int *_num_node_gids, int *node_gids,
                          int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_checknodelist()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // For convenience deref number
    int num_node_gids=*_num_node_gids;

    // Loop through counting local nodes
    Range nodes;
    merr=(*meshpp)->mesh->get_entities_by_dimension(0,0,nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    // list of the Mesh node gids
    std::vector<UInt> local_gids, local_owners;
    local_gids.reserve(nodes.size());
    local_owners.reserve(nodes.size());

    int num_local_nodes=0;
    Range::const_iterator si = nodes.begin(), se = nodes.end();
    for (; si != se; ++si) {
      const EntityHandle node = *si;

      int node_owner;
      merr=(*meshpp)->mesh->tag_get_data((*meshpp)->owner_tag, &node, 1, &node_owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // only consider local nodes
      if (node_owner == localPet) { 
        ++num_local_nodes;

        int gid;
        MBMesh_get_gid((*meshpp), node, &gid);
        local_gids.push_back(gid);

        local_owners.push_back(node_owner);
#undef print_nodeowners
#ifdef print_nodeowners
        printf("%d# checknodes - node %d owner %d\n", localPet, gid, elem_owner);
#endif
      }
    }

    // See if number of local nodes is the same
    if (num_node_gids != num_local_nodes) {
      Throw() << "Number of local nodes in mesh (" << num_local_nodes <<
                 ") different that number in list (" << num_node_gids << ")";
    }


    // Loop making sure nodes are all here
    for (int i=0; i<num_node_gids; i++) {
      std::vector<UInt>::const_iterator ni = std::find(local_gids.begin(), local_gids.end(), node_gids[i]);

      if (ni == local_gids.end()) {
        Throw() << "Node "<<node_gids[i] << " not found in Mesh.";
      }

      if (local_owners[ni-local_gids.begin()] != localPet) {
        Throw() << "Node "<<node_gids[i] << " in Mesh, but not local." 
                << local_owners[ni-local_gids.begin()] << "  " << localPet;
      }
    }

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}


// This method verifies that elems in elem_gids array are the same as the local elems in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of elems in both cases are the same and that every
// entry in elem_gids is contained in meshpp
void MBMesh_checkelemlist(MBMesh **meshpp, int *_num_elem_gids, int *elem_gids,
                          int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_checkelemlist()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // For convenience deref number
    int num_elem_gids=*_num_elem_gids;

    // Loop through counting local elems
    Range elems;
    merr=(*meshpp)->mesh->get_entities_by_dimension(0, (*meshpp)->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    // list of the Mesh elem gids
    std::vector<UInt> local_gids, local_owners;
    local_gids.reserve(elems.size());
    local_owners.reserve(elems.size());

    int num_local_elems=0;
    Range::const_iterator si = elems.begin(), se = elems.end();
    for (; si != se; ++si) {
      const EntityHandle elem = *si;

      // Get element id
      int elem_id;
      MBMesh_get_gid(*meshpp, elem, &elem_id);

      // Don't do split elements
     if ((*meshpp)->is_split && (elem_id > (*meshpp)->max_non_split_id)) continue;

      int elem_owner;
      merr=(*meshpp)->mesh->tag_get_data((*meshpp)->owner_tag, &elem, 1, &elem_owner);
      ESMC_CHECK_MOAB_THROW(merr);

      if (elem_owner == localPet) {
        num_local_elems++;

        int gid;
        MBMesh_get_gid(*meshpp, elem, &gid);
        local_gids.push_back(gid);

        local_owners.push_back(elem_owner);
#undef print_elemowners
#ifdef print_elemowners
        printf("%d# checkelems - elem %d owner %d\n", localPet, gid, elem_owner);
#endif
      }
    }

    // See if number of local elems is the same
    if (num_elem_gids != num_local_elems) {
      Throw() << "Number of local elems in mesh (" << num_local_elems
              << ") different that number in list (" << num_elem_gids << ")";
    }

    // Loop making sure elems are all here
    for (int i=0; i<num_elem_gids; i++) {
      std::vector<UInt>::const_iterator ni = std::find(local_gids.begin(), local_gids.end(), elem_gids[i]);

      if (ni == local_gids.end()) {
        Throw() << "Node "<<elem_gids[i]<<" not found in Mesh.";
      }

      if (local_owners[ni-local_gids.begin()] != localPet) {
        Throw() << "Elem "<<elem_gids[i]<<" in Mesh, but not local." 
                << local_owners[ni-local_gids.begin()] << "  " << localPet;
      }
    }


#undef debug_printentities
#ifdef debug_printentities
    {
    Range nodes;
    merr=(*meshpp)->mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    Range::const_iterator ni = nodes.begin(), ne = nodes.end();
    for (; ni != ne; ++ni) {
      const EntityHandle ent = *ni;
      int gid;
      MBMesh_get_gid(*meshpp, ent, &gid);
      int owner;
      merr=(*meshpp)->mesh->tag_get_data((*meshpp)->owner_tag, &ent, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      printf("%d# checkelems - node %d owner %d\n", localPet, gid, owner);
    }

    Range elems;
    merr=(*meshpp)->mesh->get_entities_by_dimension(0, (*meshpp)->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    Range::const_iterator si = elems.begin(), se = elems.end();
    for (; si != se; ++si) {
      const EntityHandle ent = *si;
      int gid;
      MBMesh_get_gid((*meshpp), ent, &gid);
      int owner;
      merr=(*meshpp)->mesh->tag_get_data((*meshpp)->owner_tag, &ent, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      printf("%d# checkelems - elem %d owner %d\n", localPet, gid, owner);
    }
    }
#endif

  }
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

void MBMesh_FitOnVM(MBMesh **meshpp, VM **new_vm, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_MeshFitOnVM()"
  try {
    int localrc, merr;

    // set up Par
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *curr_vm = VM::getCurrent(&localrc);
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get current VM size
    int curr_vm_size=curr_vm->getPetCount();

    // Get current VM rank
    int curr_vm_rank=curr_vm->getLocalPet();

    // Describe mapping of current PET
    int new_vm_rank=-1; // if there is no pet, set to -1
    if ((ESMC_NOT_PRESENT_FILTER(new_vm) != ESMC_NULL_POINTER) && *new_vm) {
      new_vm_rank=(*new_vm)->getLocalPet();
    }

    // Allocate array
    int *rank_map=new int[curr_vm_size];

    // Create array mapping from current vm to input vm
    localrc=curr_vm->allgather(&new_vm_rank,rank_map,sizeof(int));
    ESMC_CHECK_PASSTHRU_THROW(localrc);

#if 0
    // debug output
    for (int p=0; p<curr_vm_size; p++) {
      printf("%d# %d to %d\n",curr_vm_rank,p,rank_map[p]);
    }
#endif

    // Change proc numbers in mesh
    (*meshpp)->map_proc_numbers(curr_vm_size, rank_map);

    // Free map
    delete [] rank_map;

  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
} // ESMCI_MeshFitOnVM

void MBMesh_GetDimensions(MBMesh *meshp, int *sdim, int *pdim, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetDimensions()"
  try {
    if (sdim)
      *sdim = meshp->orig_sdim;

    if (pdim)
      *pdim = meshp->pdim;
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetCentroid(MBMesh *meshp, int *num_elem, double *elem_centroid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetCentroid()"
  try {
    // pass as ESMCI::InterArray<int> *elemCentroid, use extent[0] over num_elem

    char msg[256];
    if (*num_elem != meshp->num_orig_elem()) {
      Throw () << "elemCentroid array must be of size " << meshp->num_elem();
    } else if (meshp->coordsys != ESMC_COORDSYS_CART) {
      Throw () << "Cannot yet return centroids for spherical coordinates.";
    }

    meshp->get_elem_centroids(elem_centroid);

  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetNodeCount(MBMesh *meshp, int *nodeCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetNodeCount()"
  try {
    *nodeCount = meshp->num_orig_node();
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetElemCount(MBMesh *meshp, int *elemCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemCount()"
  try {
    *elemCount = meshp->num_orig_elem();
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetElemConnCount(MBMesh *meshp, int *elemConnCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemConnCount()"
  try {
    *elemConnCount = meshp->num_orig_elem_conn();
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetElemInfoPresence(MBMesh *meshp, 
                                int *elemMaskIsPresent,
                                int *elemAreaIsPresent,
                                int *elemCoordsIsPresent,
                                int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemInfoPresence()"
  try {
    // Check if element mask is present
    if (elemMaskIsPresent) {
      *elemMaskIsPresent=0;
      if (meshp->has_elem_mask) *elemMaskIsPresent=1;
    }
  
    // Check if element area is present
    if (elemAreaIsPresent) {
      *elemAreaIsPresent=0;
      if (meshp->has_elem_area) *elemAreaIsPresent=1;
    }

    // Check if element coords are present
    if (elemCoordsIsPresent) {
      *elemCoordsIsPresent=0;
      if (meshp->has_elem_coords) *elemCoordsIsPresent=1;
    }
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}


void MBMesh_GetElemCreateInfo(MBMesh *meshp,
                              ESMCI::InterArray<int> *elemIds,
                              ESMCI::InterArray<int> *elemTypes,
                              ESMCI::InterArray<int> *elemConn,
                              ESMCI::InterArray<int> *elemMask,
                              ESMCI::InterArray<ESMC_R8> *elemArea,
                              ESMCI::InterArray<ESMC_R8> *elemCoords, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemCreateInfo()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Doesn't work with split meshes right now
    if (meshp->is_split)
      if (elemConn) 
        Throw () << "Can't get elem connection count from mesh containing >4 elements.";
    
    ////// Get some handy information //////
    int num_elems = meshp->num_orig_elem();
    int orig_sdim = meshp->orig_sdim;

    ////// Error check input arrays //////

    if (present(elemIds)) {
      if (elemIds->dimCount !=1)
        Throw () << "elemIds array must be 1D";
      if (elemIds->extent[0] != num_elems)
        Throw () << "elemIds array must be of size elemCount";
    }

    if (present(elemTypes)) {
      if (elemTypes->dimCount !=1)
        Throw () << "elemTypes array must be 1D";
      if (elemTypes->extent[0] != num_elems)
        Throw () << "elemTypes array must be of size elemCount";
    }

    if (present(elemConn)) {
      // this is to allow calling for anything other than elemConn
      // num_elem_conn can be moved back to top after elemconn from ngons fixed
      int num_elem_conn = meshp->num_elem_conn();
      if (elemConn->dimCount !=1)
        Throw () << "elemConn array must be 1D";
      if (elemConn->extent[0] != num_elem_conn)
        Throw () << "elemConn array must be of size elemConnCount";
    }

    if (present(elemMask)) {
      if (!meshp->has_elem_mask)
        Throw () << "Element mask not present.";
      if (elemMask->dimCount !=1)
        Throw () << "elemMask array must be 1D";
      if (elemMask->extent[0] != num_elems)
        Throw () << "elemMask array must be of size elemCount";
    }

    if (present(elemArea)) {
      if (!meshp->has_elem_area)
        Throw () << "Element areas not present.";
      if (elemArea->dimCount !=1)
        Throw () << "elemArea array must be 1D";
      if (elemArea->extent[0] != num_elems)
        Throw () << "elemArea array must be of size elemCount";
    }

    if (present(elemCoords)) {
      if (!meshp->has_elem_coords)
        Throw () << "Element coords not present.";
      if (elemCoords->dimCount !=1)
        Throw () << "elemCoords array must be 1D";
      if (elemCoords->extent[0] != num_elems*orig_sdim)
        Throw () << "elemCoords array must be of size elemCount*sdim";
    }

    // Fill info in arrays 
    std::vector<EntityHandle> orig_elems;
    meshp->get_sorted_orig_elems(orig_elems);

    // If it was passed in, fill elementIds array
    if (present(elemIds)) {
      int *elemIds_array=elemIds->array;
      meshp->get_gid(orig_elems, elemIds_array);
    }

    // If it was passed in, fill elementTypes array
    if (present(elemTypes)) {
      int *elemTypes_array=elemTypes->array;
      meshp->get_elem_types(orig_elems, elemTypes_array);
    }

    // If it was passed in, fill elementIds array
    if (present(elemConn)) {
      // this is to allow calling for anything other than elemConn
      // num_elem_conn can be moved back to top after elemconn from ngons fixed
      int num_elem_conn = meshp->num_elem_conn();
      int *elemConn_array=elemConn->array;
      meshp->get_elem_connectivity(orig_elems, elemConn_array);
    }

    // If it was passed in, fill elementMask array
    if (present(elemMask)) {
      int *elemMask_array=elemMask->array;
      meshp->get_elem_mask_val(orig_elems, elemMask_array);
    }

    // If it was passed in, fill elementArea array
    if (present(elemArea)) {
      ESMC_R8 *elemArea_array=elemArea->array;
      meshp->get_elem_area(orig_elems, elemArea_array);
    }

    // If it was passed in, fill elemCoords array
    if (present(elemCoords)) {
      ESMC_R8 *elemCoords_array=elemCoords->array;
      meshp->get_elem_orig_coords(orig_elems, elemCoords_array);
    }

  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}


void MBMesh_SetElemCreateInfo(MBMesh *meshp,
                              ESMCI::InterArray<int> *elemMask,
                              ESMCI::InterArray<ESMC_R8> *elemArea,
                              int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemCreateInfo()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Doesn't work with split meshes right now
    if (meshp->is_split)
      Throw () << "Can't set elem info for a mesh containing >4 elements.";
    
    ////// Get some handy information //////
    int num_elems=meshp->num_orig_elem();
    int orig_sdim=meshp->orig_sdim;

    ////// Error check input arrays //////

    if (present(elemMask)) {
      if (!meshp->has_elem_mask)
        Throw () << "Element mask not present.";
      if (elemMask->dimCount !=1)
        Throw () << "elemMask array must be 1D";
      if (elemMask->extent[0] != num_elems)
        Throw () << "elemMask array must be of size elemCount";
    }

    if (present(elemArea)) {
      if (!meshp->has_elem_area)
        Throw () << "Element areas not present.";
      if (elemArea->dimCount !=1)
        Throw () << "elemArea array must be 1D";
      if (elemArea->extent[0] != num_elems)
        Throw () << "elemArea array must be of size elemCount";
    }


    // Fill info in arrays 
    std::vector<EntityHandle> orig_elems;
    meshp->get_sorted_orig_elems(orig_elems);

    // If it was passed in, fill elementMask array
    if (present(elemMask)) {
      int *elemMask_array=elemMask->array;
      meshp->set_elem_mask_val(orig_elems, elemMask_array);
    }

    // If it was passed in, fill elementArea array
    if (present(elemArea)) {
      ESMC_R8 *elemArea_array=elemArea->array;
      meshp->set_elem_area(orig_elems, elemArea_array);
    }
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetNodeInfoPresence(MBMesh *meshp, 
                                int *nodeMaskIsPresent,
                                int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetNodeInfoPresence()"
  try {
    *nodeMaskIsPresent=0;
    if (meshp->has_node_mask) *nodeMaskIsPresent=1;
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetNodeCreateInfo(MBMesh *meshp,
                              ESMCI::InterArray<int> *nodeIds,
                              ESMCI::InterArray<ESMC_R8> *nodeCoords,
                              ESMCI::InterArray<int> *nodeOwners,
                              ESMCI::InterArray<int> *nodeMask,
                              int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_MeshGetNodeCreateInfo()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    int num_nodes=meshp->num_orig_node();
    int orig_sdim=meshp->orig_sdim;

    // Error check input arrays

    // If nodeIds array exists, error check
    if (present(nodeIds)) {
      if (nodeIds->dimCount !=1)
        Throw () << "nodeIds array must be 1D";
      if (nodeIds->extent[0] != num_nodes)
        Throw () << "nodeIds array must be of size nodeCount";
    }

    // If nodeIds array exists, error check
    if (present(nodeCoords)) {
      if (nodeCoords->dimCount !=1)
        Throw () << "nodeCoords array must be 1D";
      if (nodeCoords->extent[0] != orig_sdim*num_nodes)
        Throw () << "nodeCoords array must be of size nodeCount*sdim";
    }

    // If nodeOwners array exists, error check
    if (present(nodeOwners)) {
      if (nodeOwners->dimCount !=1)
        Throw () << "nodeOwners array must be 1D";
      if (nodeOwners->extent[0] != num_nodes)
        Throw () << "nodeOwners array must be of size nodeCount";
    }

    // If nodeMask array exists, error check
    if (present(nodeMask)) {
      if (!meshp->has_node_mask)
        Throw () << "Node mask not present.";
      if (nodeMask->dimCount !=1)
        Throw () << "nodeMask array must be 1D";
      if (nodeMask->extent[0] != num_nodes)
        Throw () << "nodeMask array must be of size nodeCount";
    }

    // Fill info in arrays 
    std::vector<EntityHandle> orig_nodes;
    meshp->get_sorted_orig_nodes(orig_nodes);

    // If it was passed in, fill nodeIds array
    if (present(nodeIds)) {
      int *nodeIds_array=nodeIds->array;
      meshp->get_gid(orig_nodes, nodeIds_array); 
    }

    // If it was passed in, fill nodeCoords array
    if (present(nodeCoords)) {
      double *nodeCoords_array=nodeCoords->array;
      meshp->get_node_orig_coords(orig_nodes, nodeCoords_array); 
    }

    // If it was passed in, fill nodeOwners array
    if (present(nodeOwners)) {
      int *nodeOwners_array=nodeOwners->array;
      meshp->get_owners(orig_nodes, nodeOwners_array); 
    }

    // If it was passed in, fill nodeMask array
    if (present(nodeMask)) {
      int *nodeMask_array=nodeMask->array;
      meshp->get_node_mask_val(orig_nodes, nodeMask_array); 
    }

  }
  CATCH_MBMESH_RETURN(rc);

  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_getelemfrac(MBMesh *mbmesh, int *_num_elem, double *elem_fracs, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_getelemfrac()"
  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
    }

    // Dereference number of elements
    int num_elem=*_num_elem;

    // Get elems in creation order
    std::vector<EntityHandle> orig_elems;    
    mbmesh->get_sorted_orig_elems(orig_elems);

    // Check size
    if (num_elem != orig_elems.size()) {
      Throw() << "Number of elements in Mesh doesn't match size of input array for element fractions.";
    }

    // Get fracs (merging split elems if necessary)
    mbmesh->get_elem_frac(true,orig_elems,elem_fracs);

  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}


// Create a list of elems in the same order as gids/seqInd in Array
// TODO: Change to not be elem specific (i.e. change names to be entity or some such)
void MBMesh_create_list_of_elems_in_Array(ESMCI::Array *array, 
                                          std::map<int,EntityHandle> &gid_to_elem_map,
                                          std::vector<EntityHandle> &elems_in_Array) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_create_list_of_elems_in_Array()"

  try {

    // Get Distgrid
    ESMCI::DistGrid *distGrid=array->getDistGrid();

    // Get localDECount
    int localDECount=distGrid->getDELayout()->getLocalDeCount();

    // Loop over DEs
    for (auto lDE=0; lDE < localDECount; lDE++) {

      // iterator through Array Elements
      ArrayElement arrayElement(array, lDE, true, false, false);
      while(arrayElement.isWithin()){
        SeqIndex<ESMC_I4> seqIndex;  // invalidated by default constructor
        if (arrayElement.hasValidSeqIndex()){
          // seqIndex is well defined for this arrayElement
          seqIndex = arrayElement.getSequenceIndex<ESMC_I4>();

          // Get seqIndex/Mesh global id
          int gid=seqIndex.decompSeqIndex;;

          // Add associated EntityHandle to output
          std::map<int,EntityHandle>::iterator gtei = gid_to_elem_map.find(gid);

          // Complain if gid not found
          if (gtei == gid_to_elem_map.end()) {
            Throw() << "Global id in Array unexpectedly not found in Mesh.";
          } 

          // Add entity to output vector
          elems_in_Array.push_back(gtei->second);
        }

        // next element
        arrayElement.next();  
      } // multi dim index loop

    } // Loop over local DEs

  }
  CATCH_MBMESH_RETHROW;
}

// Copy data into memory in Array
// NEEDS TO GO THROUGH ARRAY IN SAME ORDER AS ABOVE METHOD!!!!!!!!!!!!
// TODO: eventually template this so that it'll support other types
void MBMesh_put_data_into_Array(ESMCI::Array *array, ESMC_R8 *data) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_put_data_into_Array()"

  try {

    // Get Distgrid
    ESMCI::DistGrid *distGrid=array->getDistGrid();

    // Get typekind of Array
    ESMC_TypeKind_Flag typekind=array->getTypekind();

    //  Only support ESMC_R8 right now
    if (typekind != ESMC_TYPEKIND_R8) {
      Throw() <<"Only fraction Fields of typekind=ESMF_TYPEKIND_R8 are supported right now.";
    }

    // Get localDECount
    int localDECount=distGrid->getDELayout()->getLocalDeCount();

    // Loop over DEs
    int data_pos=0;
    for (auto lDE=0; lDE < localDECount; lDE++) {

      // Get base address for local DE memory
      ESMC_R8 *lDEBaseAddr = (ESMC_R8 *)(array->getLarrayBaseAddrList())[lDE];

      // iterator through Array Elements
      ArrayElement arrayElement(array, lDE, true, false, false);
      while(arrayElement.isWithin()){
        if (arrayElement.hasValidSeqIndex()){

          // Get linear index of this element
          long unsigned int linearIndex = arrayElement.getLinearIndex();

          // Get pointer to data
          ESMC_R8 *arrayData=lDEBaseAddr+linearIndex;

          // Copy Data
          *arrayData=data[data_pos];

          // Advance to next data  pos
          data_pos++;
        }

        // next element
        arrayElement.next();  
      } // multi dim index loop

    } // Loop over local DEs

  }
  CATCH_MBMESH_RETHROW;
}

// Copy element fractions from an MBMesh (mbmesh) into an Array (array)
void MBMesh_get_elem_frac_into_Array(MBMesh *mbmesh, ESMCI::Array *array, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_get_elem_frac_into_Array()"
  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
    }

    // Error Checks
    ThrowRequire(mbmesh != NULL);
    ThrowRequire(array != NULL);

    // Get all elems
    Range elems;
    mbmesh->get_all_elems(elems);

    // Loop and set up gid_to_elem_map
    std::map<int,EntityHandle> gid_to_elem_map;
    for (Range::iterator it=elems.begin(); it !=elems.end(); it++) {
      EntityHandle elem=*it;
      
      // Get node global id
      int gid=mbmesh->get_gid(elem);
      
      // Add to map
      gid_to_elem_map[gid]=elem;
    }


    // Use gid_to_elem map to make ordered vector of elems in Array
    std::vector<EntityHandle> elems_in_Array;    
    MBMesh_create_list_of_elems_in_Array(array, gid_to_elem_map, elems_in_Array);

    // Allocate elem fractions array
    double *elem_fracs=new double[elems_in_Array.size()];

    // Get fracs (merging split elems if necessary)
    mbmesh->get_elem_frac(true,elems_in_Array,elem_fracs);

    // Copy into Array memory
    MBMesh_put_data_into_Array(array, elem_fracs);    

    // Get rid of elem_fracs
    delete [] elem_fracs;

  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

#endif // ESMF_MOAB
