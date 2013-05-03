// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Mesh_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include <string>
#include <ostream>
#include <iterator>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Mesh.h"
#include "ESMCI_MeshRead.h"
#include "ESMCI_MeshRegrid.h" //only for the conservative flag in add_elements
#include "ESMCI_MeshVTK.h"
#include "ESMCI_ParEnv.h"
#include "ESMCI_MeshUtils.h"
#include "ESMCI_GlobalIds.h"
#include "ESMCI_VM.h"
#include "ESMCI_FindPnts.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/ESMCI_Phedra.h"
#include "Mesh/include/ESMCI_XGridUtil.h"
#include "Mesh/include/ESMCI_MeshMerge.h"
#include "Mesh/include/ESMCI_MeshRedist.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;


//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Mesh} class functions.
//
//EOP
//-------------------------------------------------------------------------


/*----------------------------------------------------------------------------
 *  Low level helper functions: translate from F90 to C++.
 *----------------------------------------------------------------------------*/


extern "C" void FTN_X(c_esmc_meshinit)(char *logfile, int *use_log,
   ESMCI_FortranStrLenArg nlen) {

  char *lname = ESMC_F90toCstring(logfile, nlen);

  Par::Init(lname, (*use_log == 1 ? true : false));

  delete [] lname;

}


extern "C" void FTN_X(c_esmc_meshcreate)(Mesh **meshpp,
                         int *pdim, int *sdim, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshcreate()"



   try {

  // Initialize the parallel environment for mesh (if not already done)
{
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
}

     // Some error checking of input
    if (*pdim > *sdim) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- Parametric dimension can't be greater than spatial dimension",
         ESMC_CONTEXT, &localrc)) throw localrc;
    }

    if ((*pdim < 2) || (*pdim >3)) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- Parametric dimension can't be greater than 3D or less than 2D",
         ESMC_CONTEXT, &localrc)) throw localrc;
    }

    if ((*sdim < 2) || (*sdim >3)) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- Spatial dimension can't be greater than 3D or less than 2D",
        ESMC_CONTEXT, &localrc)) throw localrc;
    }

    *meshpp = new Mesh();

    (*meshpp)->set_parametric_dimension(*pdim);
    (*meshpp)->set_spatial_dimension(*sdim);

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT,rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT,rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

} // meshcreate

extern "C" void FTN_X(c_esmc_meshaddnodes)(Mesh **meshpp, int *num_nodes, int *nodeId, 
                                           double *nodeCoord, int *nodeOwner, InterfaceInt **nodeMaskII,
                                           int *rc) 
{
   try {
    Mesh *meshp = *meshpp;
    ThrowRequire(meshp);
    Mesh &mesh = *meshp;

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }

    // Get petCount for error checking
    int localrc;
    int petCount = VM::getCurrent(&localrc)->getPetCount();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // Check node owners
    for (int n = 0; n < *num_nodes; ++n) {
      if ((nodeOwner[n]<0) || (nodeOwner[n]>petCount-1)) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- Bad nodeOwner value ", ESMC_CONTEXT,&localrc)) throw localrc;
      }
    }


    // Create new nodes
    for (int n = 0; n < *num_nodes; ++n) {

      MeshObj *node = new MeshObj(MeshObj::NODE, nodeId[n], n);

      node->set_owner(nodeOwner[n]);

      mesh.add_node(node, 0);

    }

    // Register the nodal coordinate field.
    IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", mesh.spatial_dim());

    // Handle node masking
    IOField<NodalField> *node_mask_val;
    IOField<NodalField> *node_mask;

    bool has_node_mask=false;
    if (*nodeMaskII != NULL) { // if masks exist
    // Error checking
    if ((*nodeMaskII)->dimCount !=1) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- nodeMask array must be 1D ", ESMC_CONTEXT,  &localrc)) throw localrc;
    }

    if ((*nodeMaskII)->extent[0] != *num_nodes) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
                                       "- nodeMask array must be the same size as the nodeIds array ", ESMC_CONTEXT, &localrc)) throw localrc;
    }
    
    // Register mask fields on mesh
    node_mask_val = mesh.RegisterNodalField(mesh, "node_mask_val", 1);
    node_mask = mesh.RegisterNodalField(mesh, "mask", 1);


    // Record the fact that it has masks
    has_node_mask=true;   
  } 
    

    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();

    UInt sdim = mesh.spatial_dim();

    // Loop and add coords and mask
    if (has_node_mask) {
      int *maskArray=(*nodeMaskII)->array;
      int nm=0;
      for (UInt nc = 0; ni != ne; ++ni) {
        
        MeshObj &node = *ni;
      
        // Coords
        double *coord = node_coord->data(node);
        for (UInt c = 0; c < sdim; ++c)
          coord[c] = nodeCoord[nc+c];
        nc += sdim;   

        // Mask
        double *mask = node_mask_val->data(node);
        *mask=maskArray[nm];
        nm++;

      }
    } else {
      for (UInt nc = 0; ni != ne; ++ni) {
        
        MeshObj &node = *ni;
        
        double *coord = node_coord->data(node);
        for (UInt c = 0; c < sdim; ++c)
          coord[c] = nodeCoord[nc+c];        
        nc += sdim;   
      }      
    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

} 

extern "C" void FTN_X(c_esmc_meshwrite)(Mesh **meshpp, char *fname, int *rc,
    ESMCI_FortranStrLenArg nlen) {

  try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }

    char *filename = ESMC_F90toCstring(fname, nlen);

    WriteMesh(**meshpp, filename);

    delete [] filename;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT,rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;


}

extern "C" void FTN_X(c_esmc_meshaddelements)(Mesh **meshpp, 
                                              int *_num_elems, int *elemId, int *elemType, InterfaceInt **elemMaskII ,
                                              int *areaPresent, double *elemArea, 
                                              int *_num_elemConn, int *elemConn, int *regridConserve, int *rc) 
{
   try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }


    // Set some convient variables
    Mesh *meshp = *meshpp;
    ThrowRequire(meshp);

    Mesh &mesh = *meshp;

    int num_elems=*_num_elems;

    int num_elemConn=*_num_elemConn;


    // Get parametric dimension
    int parametric_dim=mesh.parametric_dim();

    // Error check input
    //// Check element type
    for (int i=0; i< num_elems; i++) {
      if (parametric_dim==2) {
        if ((elemType[i] != 5) && (elemType[i] != 9)) {
	  int localrc;
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 2 element types must be either triangles or quadrilaterals ",
           ESMC_CONTEXT, &localrc)) throw localrc;
        }
      } else if (parametric_dim==3) {
        if ((elemType[i] != 10) && (elemType[i] != 12)) {
	  int localrc;
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 3 element types must be either tetrahedron or hexahedron ",
           ESMC_CONTEXT, &localrc)) throw localrc;
        }

      }
    }

    //// Check size of connectivity list
    int expected_conn_size=0;
    for (int i=0; i< num_elems; i++) {
      if (elemType[i]==5) expected_conn_size += 3;   
      else if (elemType[i]==9) expected_conn_size += 4;   
      else if (elemType[i]==10) expected_conn_size += 4;   
      else if (elemType[i]==12) expected_conn_size += 8;   
    }
    if (expected_conn_size != num_elemConn) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
       "- element connectivity list doesn't contain the right number of entries ",
       ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // Get number of nodes
    int num_nodes = mesh.num_nodes();

    // Allocate array to make sure that there are no local nodes without a home
    std::vector<int> node_used;
    node_used.resize(num_nodes, 0);


    // We must first store all nodes in a flat array since element
    // connectivity will index into this array.
    std::vector<MeshObj*> all_nodes;

    all_nodes.resize(num_nodes, static_cast<MeshObj*>(0));

    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();

    for (; ni != ne; ++ni) {

      int seq = ni->get_data_index();

      if (seq >= num_nodes){
	int localrc;
	if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
            "- node index is larger or equal to num_nodes", 
            ESMC_CONTEXT, &localrc))  throw localrc;
         }
      
      all_nodes[seq] = &*ni;

    }

    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;

    for (int e = 0; e < num_elems; ++e) {

    // Get/deduce the element topology
    const MeshObjTopo *topo = Vtk2Topo(mesh.spatial_dim(), elemType[e]);

    int nnodes = topo->num_nodes;

    std::vector<MeshObj*> nconnect(nnodes, static_cast<MeshObj*>(0));

      // The object
      long eid = elemId[e];
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, eid, e);

      for (int n = 0; n < nnodes; ++n) {
      
        // Get 0-based node index
        int node_index=elemConn[cur_conn]-1;

        // Check elemConn
        if ((node_index < 0) || (node_index > num_nodes-1)) {
	  int localrc;
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
	   "- elemConn entries should not be greater than number of nodes on processor ",
           ESMC_CONTEXT, &localrc)) throw localrc;
	}

        // Setup connectivity list
        nconnect[n] = all_nodes[node_index];

        // Mark as used
        node_used[node_index]=1;

        // Advance to next
        cur_conn++;
      }

      mesh.add_element(elem, nconnect, topo->number, topo);

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

  // Register the frac field


  if (*regridConserve == ESMC_REGRID_CONSERVE_ON) {
    Context ctxt; ctxt.flip();
     MEField<> *elem_frac = mesh.RegisterField("elem_frac",
                        MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  }


  // Handle element masking
  bool has_elem_mask=false;
  if (*elemMaskII != NULL) { // if masks exist
    // Error checking
    if ((*elemMaskII)->dimCount !=1) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- elementMask array must be 1D ", ESMC_CONTEXT,  &localrc)) throw localrc;
    }

    if ((*elemMaskII)->extent[0] != num_elems) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- elementMask array must be the same size as elementIds array ", ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // Context for new fields
    Context ctxt; ctxt.flip();

    // Add element mask values field
    MEField<> *elem_mask_val = mesh.RegisterField("elem_mask_val",
                         MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    // Add element mask field
    MEField<> *elem_mask = mesh.RegisterField("elem_mask",
                         MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    // Record the fact that it has masks
    has_elem_mask=true;   
  } 


  // Handle element area
  bool has_elem_area=false;
  if (*areaPresent == 1) { // if areas exist
    // Context for new fields
    Context ctxt; ctxt.flip();

    // Add element mask field
    MEField<> *elem_area = mesh.RegisterField("elem_area",
                         MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    // Record the fact that it has masks
    has_elem_area=true;   
  } 



  // Perhaps commit will be a separate call, but for now commit the mesh here.
  mesh.build_sym_comm_rel(MeshObj::NODE);
  mesh.Commit();
  
  
  // Set Mask values
  if (has_elem_mask) {
    // Get Fields
    MEField<> *elem_mask_val=mesh.GetField("elem_mask_val"); 
    MEField<> *elem_mask=mesh.GetField("elem_mask"); 
    
    // Get mask value array
    int *elemMask=(*elemMaskII)->array;

    // Loop through elements setting values
    // Here we depend on the fact that data index for elements
    // is set as the position in the local array above
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Set mask value to input array
      double *mv=elem_mask_val->data(elem);
      int data_index = elem.get_data_index();
      *mv=(double)elemMask[data_index];

        // Init mask to 0.0
      double *m=elem_mask->data(elem);
      *m=0.0;      
    }
  }


  // Set area values
  if (has_elem_area) {
    // Get Fields
    MEField<> *elem_area=mesh.GetField("elem_area"); 
    
    // Loop through elements setting values
    // Here we depend on the fact that data index for elements
    // is set as the position in the local array above
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Set mask value to input array
      double *av=elem_area->data(elem);
      int data_index = elem.get_data_index();
      *av=(double)elemArea[data_index];
    }
  }



  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

} 


/**
 * Routines for reading in a test VTK mesh to fortran arrays (for testing the array interface)
 */
extern "C" void FTN_X(c_esmc_meshvtkheader)(char *filename, int *num_elem, int *num_node, int *conn_size, int *rc,
    ESMCI_FortranStrLenArg nlen) {

  try {
 
{
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
}

    char *fname = ESMC_F90toCstring(filename, nlen);

    std::string fnames(fname);
    int rank = Par::Rank();
    int psize = Par::Size();

    std::string newname;

    std::string extension = ".vtk";

  // If csize = 1, read fbase.g
     if (psize > 1) {
       std::ostringstream newname_str;
       int ndec = numDecimal(psize);
       newname_str << fname << "." << psize << ".";
       newname_str << std::setw(ndec) << std::setfill('0') << rank;
       newname = newname_str.str() + extension;
     } else newname = fname + extension;

    ReadVTKMeshHeader(newname, *num_elem, *num_node, *conn_size);

    delete [] fname;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN_X(c_esmc_meshvtkbody)(char *filename, int *nodeId, double *nodeCoord,
                    int *nodeOwner, int *elemId, int *elemType, int *elemConn, int *rc,
    ESMCI_FortranStrLenArg nlen) {

  try {

{
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
}


    char *fname = ESMC_F90toCstring(filename, nlen);

    std::string fnames(fname);
    int rank = Par::Rank();
    int psize = Par::Size();

    std::string newname;

    std::string extension = ".vtk";

  // If csize = 1, read fbase.g
     if (psize > 1) {
       std::ostringstream newname_str;
       int ndec = numDecimal(psize);
       newname_str << fname << "." << psize << ".";
       newname_str << std::setw(ndec) << std::setfill('0') << rank;
       newname = newname_str.str() + extension;
     } else newname = fname + extension;

    ReadVTKMeshBody(newname, nodeId, nodeCoord, nodeOwner, elemId, elemType, elemConn);

    delete [] fname;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN_X(c_esmc_meshdestroy)(Mesh **meshpp, int *rc) {

  try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }

    Mesh *meshp = *meshpp;
    
    delete meshp;
    
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN_X(c_esmc_meshfreememory)(Mesh **meshpp, int *rc) {

  try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }

    Mesh *meshp = *meshpp;

    delete meshp;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN_X(f_esmf_getmeshdistgrid)(int*, int*, int*, int*);


/**
 * Sort nodes by the order in which they were originally declared
 * (which is stored by get_data_index)
 */
void getNodeGIDS(Mesh &mesh, std::vector<int> &ngid) {

  UInt nnodes = mesh.num_nodes();

  Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();

  std::vector<std::pair<int,int> > gids;

  for (; ni != ne; ++ni) {

    MeshObj &node = *ni;

    if (!GetAttr(node).is_locally_owned()) continue;

    int idx = node.get_data_index();

    gids.push_back(std::make_pair(idx, node.get_id()));

  }

  std::sort(gids.begin(), gids.end());

  ngid.clear();
  for (UInt i = 0; i < gids.size(); ++i) ngid.push_back(gids[i].second);

}


/**
 * Sort nodes by the order in which they were originally declared
 * (which is stored by get_data_index)
 */
void getElemGIDS(Mesh &mesh, std::vector<int> &egid) {

  UInt nelems = mesh.num_elems();

  Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();

  std::vector<std::pair<int,int> > gids;

  for (; ei != ee; ++ei) {

    MeshObj &elem = *ei;

    if (!GetAttr(elem).is_locally_owned()) continue;

    int idx = elem.get_data_index();

    gids.push_back(std::make_pair(idx, elem.get_id()));

  }

  std::sort(gids.begin(), gids.end());

  egid.clear();
  for (UInt i = 0; i < gids.size(); ++i) egid.push_back(gids[i].second);

}


extern "C" void FTN_X(c_esmc_meshget)(Mesh **meshpp, int *num_nodes, int *num_elements, int *rc){

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,rc))
   return;  // bail out with exception
    }

    Mesh *meshp = *meshpp;

    *num_nodes = meshp->num_nodes();
    *num_elements = meshp->num_elems();

    if(rc != NULL) *rc = ESMF_SUCCESS;
}
    

extern "C" void FTN_X(c_esmc_meshcreatedistgrids)(Mesh **meshpp, int *ngrid, int *egrid, int *num_lnodes, int *num_lelems, int *rc) {

  // Declare id vectors
  std::vector<int> ngids; 
  std::vector<int> egids; 
  
 
  try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }

    Mesh *meshp = *meshpp;
    
    // The nodal map.  First get the set of owned nodal ids
    {
      Context c; c.set(Attr::OWNED_ID);
      Attr ae(MeshObj::ELEMENT, c);
      
      getNodeGIDS(*meshp, ngids);
      
      //      getMeshGIDS(*meshp, ae, egids);
      getElemGIDS(*meshp, egids);
    }
    
    /*
      Par::Out() << "Node ids:(" << ngids.size() << ")" << std::endl;
      std::copy(ngids.begin(), ngids.end(), std::ostream_iterator<int>(Par::Out(), "\n"));
      Par::Out().flush();
    */
    
    /*
      Par::Out() << "Elem ids:" << std::endl;
      std::copy(egids.begin(), egids.end(), std::ostream_iterator<UInt>(Par::Out(), "\n"));
      Par::Out().flush();
    */
    
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }


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


extern "C" void FTN_X(c_esmc_meshinfoserialize)(int *intMeshFreed,
	        char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *localrc,
                ESMCI_FortranStrLenArg buffer_l){

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > vars.
    int size = sizeof(int);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < size) {         
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add a Mesh object", ESMC_CONTEXT, localrc);
         return;
      }
    }

    // Save keyCount
    ip= (int *)(buffer + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = *intMeshFreed;
    }

    // Adjust offset
    *offset += size;

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


extern "C" void FTN_X(c_esmc_meshinfodeserialize)(int *intMeshFreed, 
                                 char *buffer, int *offset, int *localrc,
                                 ESMCI_FortranStrLenArg buffer_l){

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // Get pointer
    ip= (int *)(buffer + *offset);

    // Get values
    *intMeshFreed=*ip++;

    // Adjust offset
    *offset += sizeof(int);

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


extern "C" void FTN_X(c_esmc_meshserialize)(Mesh **meshpp,
	        char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *rc,
                ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshserialize()"

   try {

  // Initialize the parallel environment for mesh (if not already done)
     {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
     }

    Mesh *meshp = *meshpp;
    ThrowRequire(meshp);
    Mesh &mesh = *meshp;
    int *ip;
    UInt *uip;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get Some Mesh info
    int numSets;
    UInt *nvalSetSizes;
    UInt *nvalSetVals;
    UInt *nvalSetObjSizes; 
    UInt *nvalSetObjVals;

    mesh.GetImprints(&numSets, &nvalSetSizes, &nvalSetVals, &nvalSetObjSizes, &nvalSetObjVals);

    // Calc Size
    int size = 3*sizeof(int)+2*numSets*sizeof(UInt);

    if (nvalSetSizes != NULL)
      for (int i=0; i<numSets; i++) {
	size +=nvalSetSizes[i]*sizeof(UInt);
      }

    if (nvalSetObjSizes != NULL)
      for (int i=0; i<numSets; i++) {
	size +=nvalSetObjSizes[i]*sizeof(UInt);
      }

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
      *ip++ = mesh.spatial_dim();
      *ip++ = mesh.parametric_dim();
      *ip++ = numSets;
    }

    // Save UInt data
    uip=(UInt *)ip;
    if (*inquireflag != ESMF_INQUIREONLY) {

      // Save set sizes and values
      if (nvalSetSizes != NULL)
        for (int i=0; i<numSets; i++) {
	  *uip++=nvalSetSizes[i];
        }

      if (nvalSetVals != 0) {
        int k=0;
        for (int i=0; i<numSets; i++) {
	  for (int j=0; j<nvalSetSizes[i]; j++) {
	    *uip++=nvalSetVals[k];
	    k++;
	  }
        }
      }

      // Save set obj sizes and values
      if (nvalSetObjSizes != NULL)
        for (int i=0; i<numSets; i++) {
	  *uip++=nvalSetObjSizes[i];
        }

      if (nvalSetObjVals != NULL) {
        int k=0;
        for (int i=0; i<numSets; i++) {
	  for (int j=0; j<nvalSetObjSizes[i]; j++) {
	    *uip++=nvalSetObjVals[k];
	    k++;
          }
	}
      }

    }

    // Adjust offset
    *offset += size;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

    return;
} 


extern "C" void FTN_X(c_esmc_meshdeserialize)(Mesh **meshpp, 
                                 char *buffer, int *offset, int *rc,
                                 ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshdeserialize()"

   try {

  // Initialize the parallel environment for mesh (if not already done)
     {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
     }

    int *ip;
    UInt *uip;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get pointer
    ip= (int *)(buffer + *offset);

    // Get values
    int spatial_dim=*ip++;
    int parametric_dim=*ip++;

    // Get Some Mesh info
    int numSets;
    std::vector<UInt> nvalSetSizes;
    std::vector<UInt> nvalSetVals;
    std::vector<UInt> nvalSetObjSizes; 
    std::vector<UInt> nvalSetObjVals;

    numSets=*ip++;

    // convert pointer
    uip=(UInt *)ip;

      // Retrieve sizes and values
      nvalSetSizes.resize(numSets,0);
      for (int i=0; i<numSets; i++) {
	nvalSetSizes[i]=*uip++;
      }

      int k=0;
      for (int i=0; i<numSets; i++) {
	nvalSetVals.resize(k+nvalSetSizes[i],0);
	for (int j=0; j<nvalSetSizes[i]; j++) {
	  nvalSetVals[k]=*uip++;
	  k++;
	}
      }

      // Save set obj sizes and value
      nvalSetObjSizes.resize(numSets,0);
      for (int i=0; i<numSets; i++) {
	nvalSetObjSizes[i]=*uip++;
      }

      k=0;
      for (int i=0; i<numSets; i++) {
	nvalSetObjVals.resize(k+nvalSetObjSizes[i],0);
	for (int j=0; j<nvalSetObjSizes[i]; j++) {
	  nvalSetObjVals[k]=*uip++;
	  k++;
	}
      }

    // Adjust offset
    *offset += 3*sizeof(int)+
      nvalSetSizes.size()*sizeof(UInt)+nvalSetVals.size()*sizeof(UInt)+
      nvalSetObjSizes.size()*sizeof(UInt)+nvalSetObjVals.size()*sizeof(UInt);


    // Create Mesh
    Mesh *meshp = new Mesh();

    // Set dimensions
    meshp->set_spatial_dimension(spatial_dim);
    meshp->set_parametric_dimension(parametric_dim);
    
    
    // Register the nodal coordinate field.
    meshp->RegisterNodalField(*meshp, "coordinates", spatial_dim);
    
    // Setup the Mesh
    //    meshp->build_sym_comm_rel(MeshObj::NODE);
    meshp->ProxyCommit(numSets,
		       nvalSetSizes, nvalSetVals,
		       nvalSetObjSizes, nvalSetObjVals);
    // Output mesh
    *meshpp=meshp;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

    return;
} 


extern "C" void FTN_X(c_esmc_meshfindpnt)(Mesh **meshpp, int *unmappedaction, int *dimPnts, int *numPnts, 
					double *pnts, int *pets, int *rc){

   try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,rc))
   return;  // bail out with exception
    }


    // Find points
    int fp_err=FindPnts(**meshpp, *unmappedaction, *dimPnts, *numPnts, pnts,
             ESMC_NOT_PRESENT_FILTER(pets), (int *)NULL);
    // Check error return
    //// Temporary solution because exceptions aren't working in FindPnts()
    if (fp_err != ESMCI_FINDPNT_SUCCESS) {
      if (fp_err == ESMCI_FINDPNT_DIM_MISMATCH) {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          " - point dimension doesn't match grid/mesh dimension",
          ESMC_CONTEXT, rc);
         return;
      } else if (fp_err == ESMCI_FINDPNT_PNT_NOT_FOUND) {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          " - some points lie outside of grid/mesh ", ESMC_CONTEXT, rc);
         return;
      } else {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
	     	  " - unknown error in findpnt", ESMC_CONTEXT, rc);
         return;
      }
    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }


    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

extern "C" void FTN_X(c_esmc_getlocalcoords)(Mesh **meshpp, double *nodeCoord, int *rc) 
{
   try {
    Mesh *meshp = *meshpp;
    ThrowRequire(meshp);
    Mesh &mesh = *meshp;

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }


    // Get some info
    int sdim=mesh.spatial_dim();
    int num_nodes = mesh.num_nodes();
    MEField<> *coords = mesh.GetCoordField();
    
    // Make a map between data index and associated node pointer
    std::vector<std::pair<int,MeshObj *> > index_to_node;
    index_to_node.reserve(num_nodes);

    // iterate through local nodes collecting indices and node pointers
    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node = *ni;
    
      if (!GetAttr(node).is_locally_owned()) continue;

      int idx = node.get_data_index();
      index_to_node.push_back(std::make_pair(idx,&node));
    }

    // Sort by data index
    std::sort(index_to_node.begin(), index_to_node.end());

    // Load coords in order of index
    int nodeCoordPos=0;
    for (UInt i = 0; i < index_to_node.size(); ++i) {
      MeshObj &node = *(index_to_node[i].second);      

      // Copy coords into output array
      double *c = coords->data(node);    
      for (int j=0; j<sdim; j++) {
	nodeCoord[nodeCoordPos]=c[j];
	nodeCoordPos++;
      } 
    } 
    

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

} 


////////////////


extern "C" void FTN_X(c_esmc_meshgetarea)(Mesh **meshpp, int *num_elem, double *elem_areas, int *rc) {

  
  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];
  double tmp_coords[MAX_NUM_POLY_COORDS];
  
  
  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
	throw localrc;  // bail out with exception
    }
    
    // Get Mesh pointer
    Mesh *meshp = *meshpp;
    
    // Get Mesh reference
    Mesh &mesh = *meshp;
    

    // Declare id vector
    std::vector<int> egids; 
    
    // get elem ids
    getElemGIDS(*meshp, egids);
    
    // If there are no elements then leave
    if (egids.empty()) {
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return;
    }

    // Check size
    if (*num_elem != egids.size()) {
      Throw() << "Number of elements doesn't match size of input array for areas";
    }
    
    
    // If an area field exists use that instead
    MEField<> *area_field = mesh.GetField("elem_area");
    if (area_field) {
      
      // Loop through elements and put areas into array
      for (int i=0; i<egids.size(); i++) {
        // Get element gid
        int elem_gid=egids[i];
        
        //  Find the corresponding Mesh element
        Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, elem_gid);
        if (mi == mesh.map_end(MeshObj::ELEMENT)) {
          Throw() << "Element not in mesh";
        }
        
        // Get the element
        const MeshObj &elem = *mi; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Get area from field
        double *area=area_field->data(elem);
        
        // Put area into area array
        elem_areas[i]=*area;
      }

      if (rc!=NULL) *rc = ESMF_SUCCESS;      
      return;
    }
    

    ////// Otherwise calculate areas..... 


    // Get coord field
    MEField<> *cfield = mesh.GetCoordField();

    // Get dimensions
    int sdim=mesh.spatial_dim();
    int pdim=mesh.parametric_dim();

    // Loop through elements and put areas into array
    for (int i=0; i<egids.size(); i++) {
      // Get element gid
      int elem_gid=egids[i];
      
      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, elem_gid);
      if (mi == mesh.map_end(MeshObj::ELEMENT)) {
	Throw() << "Element not in mesh";
      }
      
      // Get the element
      const MeshObj &elem = *mi; 
      
      // Only put it in if it's locally owned
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Compute area depending on dimensions
      double area;
      if (pdim==2) {
	if (sdim==2) {
          // get_elem_coords(&elem, cfield, 2, MAX_NUM_POLY_NODES_2D, &num_poly_nodes, poly_coords);
          get_elem_coords_2D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_2D, tmp_coords, &num_poly_nodes, poly_coords);
	  remove_0len_edges2D(&num_poly_nodes, poly_coords);
          area=area_of_flat_2D_polygon(num_poly_nodes, poly_coords);
	} else if (sdim==3) {
	  //get_elem_coords(&elem, cfield, 3, MAX_NUM_POLY_NODES_3D, &num_poly_nodes, poly_coords);
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

      // Put area into area array
      elem_areas[i]=area;
    }
        
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN_X(c_esmc_meshgetdimensions)(Mesh **meshpp, int *sdim, int *pdim, int *rc) {

  try{
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
    }
    
    // Get Mesh pointer
    Mesh *meshp = *meshpp;
    
    // Get Mesh reference
    Mesh &mesh = *meshp;

    // Get dimensions
    if(sdim) *sdim=mesh.spatial_dim();
    if(pdim) *pdim=mesh.parametric_dim();
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

extern "C" void FTN_X(c_esmc_meshgetcentroid)(Mesh **meshpp, int *num_elem, double *elem_centroid, int *rc) {

  
  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];
  double tmp_coords[MAX_NUM_POLY_COORDS];
  
  
  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
	throw localrc;  // bail out with exception
    }
    
    // Get Mesh pointer
    Mesh *meshp = *meshpp;
    
    // Get Mesh reference
    Mesh &mesh = *meshp;

    // Get dimensions
    int sdim=mesh.spatial_dim();
    int pdim=mesh.parametric_dim();
    

    // Declare id vector
    std::vector<int> egids; 
    
    // get elem ids
    getElemGIDS(*meshp, egids);
    
    // If there are no elements then leave
    if (egids.empty()) {
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return;
    }

    // Check size
    if (*num_elem != egids.size()) {
      Throw() << "Number of elements doesn't match size of input array for centroid";
    }
    
    
    // If an centroid field exists use that instead
    MEField<> *centroid_field = mesh.GetField("elem_centroid");
    if (centroid_field) {
      
      // Loop through elements and put centroids into array
      for (int i=0; i<egids.size(); i++) {
        // Get element gid
        int elem_gid=egids[i];
        
        //  Find the corresponding Mesh element
        Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, elem_gid);
        if (mi == mesh.map_end(MeshObj::ELEMENT)) {
          Throw() << "Element not in mesh";
        }
        
        // Get the element
        const MeshObj &elem = *mi; 
        
        // Only put it in if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Get centroid from field
        double *centroid=centroid_field->data(elem);
        
        // Put centroid into centroid array
        std::memcpy(elem_centroid+i*sdim, centroid, sdim*sizeof(double));
      }

      if (rc!=NULL) *rc = ESMF_SUCCESS;      
      return;
    }
    

    ////// Otherwise calculate centroids..... 


    // Get coord field
    MEField<> *cfield = mesh.GetCoordField();

    // Loop through elements and put centroids into array
    for (int i=0; i<egids.size(); i++) {
      // Get element gid
      int elem_gid=egids[i];
      
      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, elem_gid);
      if (mi == mesh.map_end(MeshObj::ELEMENT)) {
	Throw() << "Element not in mesh";
      }
      
      // Get the element
      const MeshObj &elem = *mi; 
      
      // Only put it in if it's locally owned
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Compute centroid depending on dimensions
      double *centroid;
      if (pdim==2) {
	if (sdim==2) {
          // get_elem_coords(&elem, cfield, 2, MAX_NUM_POLY_NODES_2D, &num_poly_nodes, poly_coords);
          get_elem_coords_2D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_2D, tmp_coords, &num_poly_nodes, poly_coords);
          remove_0len_edges2D(&num_poly_nodes, poly_coords);
          polygon res_poly;
          coords_to_polygon(num_poly_nodes, poly_coords, sdim, res_poly);
          std::memcpy(elem_centroid+i*sdim, res_poly.centroid(sdim).c, sdim*sizeof(double)); 
	} else if (sdim==3) {
	  //get_elem_coords(&elem, cfield, 3, MAX_NUM_POLY_NODES_3D, &num_poly_nodes, poly_coords);
          get_elem_coords_3D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_3D, tmp_coords, &num_poly_nodes, poly_coords);
          remove_0len_edges3D(&num_poly_nodes, poly_coords);
          polygon res_poly;
          coords_to_polygon(num_poly_nodes, poly_coords, sdim, res_poly);
          std::memcpy(elem_centroid+i*sdim, res_poly.centroid(sdim).c, sdim*sizeof(double)); 
	}
      } else if (pdim==3) {
	if (sdim==3) {
          Phedra tmp_phedra=create_phedra_from_elem(&elem, cfield);
          Throw() << "Meshes with parametric dimension == 3, spatial dim = 3 not supported for computing centroid yet";
        } else {
          Throw() << "Meshes with parametric dimension == 3, but spatial dim != 3 not supported for computing centroid";
        }
      } else {
	Throw() << "Meshes with parametric dimension != 2 or 3 not supported for computing centroid";
      }

    }
        
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN_X(c_esmc_meshgetfrac)(Mesh **meshpp, int *num_elem, double *elem_fracs, int *rc) {
  
  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
	throw localrc;  // bail out with exception
    }
    
    // Get Mesh pointer
    Mesh *meshp = *meshpp;
    
    // Get Mesh reference
    Mesh &mesh = *meshp;
    
    // Declare id vector
    std::vector<int> egids; 
    
    // get elem ids
    getElemGIDS(*meshp, egids);
    
    // If there are no elements then leave
    if (egids.empty()) {
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return;
    }

    // Check size
    if (*num_elem != egids.size()) {
      Throw() << "Number of elements doesn't match size of input array for areas";
    }

    // Get frac field
    MEField<> *elem_frac = mesh.GetField("elem_frac");
    if (!elem_frac) Throw() << "Getting elem_frac when it doesn't exist";

    // Loop through elements and put areas into array
    for (int i=0; i<egids.size(); i++) {
      // Get element gid
      int elem_gid=egids[i];
      
      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, elem_gid);
      if (mi == mesh.map_end(MeshObj::ELEMENT)) {
	Throw() << "Element not in mesh";
      }
      
      // Get the element
      const MeshObj &elem = *mi; 

      // Get frac data
      double *f=elem_frac->data(elem);
      
      // Only put it in if it's locally owned
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Put frac into frac array
      elem_fracs[i]=*f;
    }
        
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN_X(c_esmc_meshgetfrac2)(Mesh **meshpp, int *num_elem, double *elem_fracs, int *rc) {
  
  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
	throw localrc;  // bail out with exception
    }
    
    // Get Mesh pointer
    Mesh *meshp = *meshpp;
    
    // Get Mesh reference
    Mesh &mesh = *meshp;
    
    // Declare id vector
    std::vector<int> egids; 
    
    // get elem ids
    getElemGIDS(*meshp, egids);
    
    // If there are no elements then leave
    if (egids.empty()) {
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return;
    }

    // Check size
    if (*num_elem != egids.size()) {
      Throw() << "Number of elements doesn't match size of input array for areas";
    }

    // Get frac field
    MEField<> *elem_frac = mesh.GetField("elem_frac2");
    if (!elem_frac) Throw() << "Getting elem_frac when it doesn't exist";

    // Loop through elements and put areas into array
    for (int i=0; i<egids.size(); i++) {
      // Get element gid
      int elem_gid=egids[i];
      
      //  Find the corresponding Mesh element
      Mesh::MeshObjIDMap::iterator mi =  mesh.map_find(MeshObj::ELEMENT, elem_gid);
      if (mi == mesh.map_end(MeshObj::ELEMENT)) {
	Throw() << "Element not in mesh";
      }
      
      // Get the element
      const MeshObj &elem = *mi; 

      // Get frac data
      double *f=elem_frac->data(elem);
      
      // Only put it in if it's locally owned
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Put frac into frac array
      elem_fracs[i]=*f;
    }
        
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

// Interface to internal code to triangulate a polygon
// Input is: pnts (the polygon of size numPnts*sdim
//           td a temporary buffer of the same size as pnts
//           ti a temporary buffer of size numPnts
// Output is: tri_ind, which are the 0-based indices of the triangles 
//             making up the triangulization. tri_ind should be of size 3*(numPnts-2).
// 
extern "C" void FTN_X(c_esmc_triangulate)(int *pdim, int *sdim, int *numPnts, 
					double *pnts, double *td, int *ti, int *triInd, int *rc){
   try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,rc))
   return;  // bail out with exception
    }

  
    // do triangulation based on the dimensions of polygon
    int ret;
    if (*pdim==2) {
      if (*sdim==2) {
        ret=triangulate_poly<GEOM_CART2D>(*numPnts, pnts, td, ti, triInd);
      } else if (*sdim==3) {
        ret=triangulate_poly<GEOM_SPH2D3D>(*numPnts, pnts, td, ti, triInd);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          " - triangulate can't be used for polygons with spatial dimension not equal to 2 or 3",
          ESMC_CONTEXT, rc);
        return;
      }
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
       " - triangulate can't be used for polygons with parametric dimension not equal to 2",
       ESMC_CONTEXT, rc);
      return;
    }

    // Check return code
    if (ret != ESMCI_TP_SUCCESS) {
      if (ret == ESMCI_TP_DEGENERATE_POLY) {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          " - can't triangulate a polygon with less than 3 sides", 
          ESMC_CONTEXT, rc);
         return;
      } else if (ret == ESMCI_TP_CLOCKWISE_POLY) {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          " - clockwise polygons not supported in triangulation routine",
          ESMC_CONTEXT, rc);
         return;
      } else {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   	    " - unknown error in triangulation", ESMC_CONTEXT, rc);
         return;
      }
    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }


    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if(rc != NULL) *rc = ESMF_SUCCESS;
}


extern "C" void FTN_X(c_esmc_meshturnoncellmask)(Mesh **meshpp, ESMCI::InterfaceInt **maskValuesArg,  int *rc) {

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
	throw localrc;  // bail out with exception
    }
    
    
    // Get Mesh pointer
    Mesh *meshp = *meshpp;
    
    // Get Mesh reference
    Mesh &mesh = *meshp;
    

    // If no mask values then leave
    if (maskValuesArg == NULL) {
      // Set return code 
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      
      // Leave
      return;
    }

    // If no mask values then leave
    if (*maskValuesArg == NULL) {
      // Set return code 
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      
      // Leave
      return;
    }

    // Get mask values
    int numMaskValues=(*maskValuesArg)->extent[0];
    int *ptrMaskValues=&((*maskValuesArg)->array[0]);


    // Set Mask values
    // Get Fields
    MEField<> *elem_mask_val=mesh.GetField("elem_mask_val"); 
    MEField<> *elem_mask=mesh.GetField("elem_mask"); 
      

    if ((elem_mask_val!=NULL) && 
        (elem_mask    !=NULL)) {
         
      // Loop through elements setting values
      // Here we depend on the fact that data index for elements
      // is set as the position in the local array above
      Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;
        if (!GetAttr(elem).is_locally_owned()) continue;
        // Set mask value to input array
        double *mv=elem_mask_val->data(elem);
        
        // Round value to nearest integer
        int mi=(int)((*mv)+0.5);

        // See if gm matches any mask values
        bool mask=false;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];
          if (mi==mvi) {
            mask=true;
            break;
          }
        }
        
        // Set mask
        double *m=elem_mask->data(elem);
        
        // Set Mask based on grid mask value
        if (mask) {
          *m=1.0;
        } else {
          *m=0.0;
        }
        
      }
    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                             "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

// Turn OFF masking
extern "C" void FTN_X(c_esmc_meshturnoffcellmask)(Mesh **meshpp, int *rc) {

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
	throw localrc;  // bail out with exception
    }
    
    
    // Get Mesh pointer
    Mesh *meshp = *meshpp;
    
    // Get Mesh reference
    Mesh &mesh = *meshp;
    

    // Set Mask values
    // Get Fields
    MEField<> *elem_mask_val=mesh.GetField("elem_mask_val"); 
    MEField<> *elem_mask=mesh.GetField("elem_mask"); 
    
    
    if ((elem_mask_val!=NULL) && 
        (elem_mask    !=NULL)) {
      
      // Loop through elements setting values
      // Here we depend on the fact that data index for elements
      // is set as the position in the local array above
      Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Get mask
        double *m=elem_mask->data(elem);
        
        // Init mask to 0
        *m=0.0;
      }
    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                        "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

////////////
extern "C" void FTN_X(c_esmc_meshturnonnodemask)(Mesh **meshpp, ESMCI::InterfaceInt **maskValuesArg,  int *rc) {

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
	throw localrc;  // bail out with exception
    }
    
    
    // Get Mesh pointer
    Mesh *meshp = *meshpp;
    
    // Get Mesh reference
    Mesh &mesh = *meshp;
    

    // If no mask values then leave
    if (maskValuesArg == NULL) {
      // Set return code 
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      
      // Leave
      return;
    }

    // If no mask values then leave
    if (*maskValuesArg == NULL) {
      // Set return code 
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      
      // Leave
      return;
    }

    // Get mask values
    int numMaskValues=(*maskValuesArg)->extent[0];
    int *ptrMaskValues=&((*maskValuesArg)->array[0]);


    // Set Mask values
    // Get Fields
    MEField<> *node_mask_val=mesh.GetField("node_mask_val"); 
    MEField<> *node_mask=mesh.GetField("mask"); 
      

    if ((node_mask_val!=NULL) && 
        (node_mask    !=NULL)) {
         
      // Loop through nodes setting values
      Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
      for (; ni != ne; ++ni) {
        MeshObj &node = *ni;
        if (!GetAttr(node).is_locally_owned()) continue;
        // Set mask value to input array
        double *mv=node_mask_val->data(node);
        
        // Round value to nearest integer
        int mi=(int)((*mv)+0.5);

        // See if gm matches any mask values
        bool mask=false;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];
          if (mi==mvi) {
            mask=true;
            break;
          }
        }
        
        // Set mask
        double *m=node_mask->data(node);
        
        // Set Mask based on grid mask value
        if (mask) {
          *m=1.0;
        } else {
          *m=0.0;
        }
        
      }
    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                             "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

// Turn OFF masking
extern "C" void FTN_X(c_esmc_meshturnoffnodemask)(Mesh **meshpp, int *rc) {

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
	throw localrc;  // bail out with exception
    }
    
    
    // Get Mesh pointer
    Mesh *meshp = *meshpp;
    
    // Get Mesh reference
    Mesh &mesh = *meshp;
    

    // Set Mask values
    // Get Fields
    MEField<> *node_mask_val=mesh.GetField("node_mask_val"); 
    MEField<> *node_mask=mesh.GetField("mask"); 
    
    if ((node_mask_val!=NULL) && 
        (node_mask    !=NULL)) {
      
      // Loop through elements setting values
      // Here we depend on the fact that data index for elements
      // is set as the position in the local array above
      Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
      for (; ni != ne; ++ni) {
        MeshObj &node = *ni;
        if (!GetAttr(node).is_locally_owned()) continue;
        
        // Get mask
        double *m=node_mask->data(node);
        
        // Init mask to 0
        *m=0.0;
      }
    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                        "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

//////////// 

extern "C" void FTN_X(c_esmc_get_polygon_area)(int *spatialdim, int *nedges, 
						 double *points, double *area, int *rc) {
  if (*spatialdim == 2) {
    *area = area_of_flat_2D_polygon(*nedges, points);
  } else if (*spatialdim == 3) {
    *area = great_circle_area(*nedges, points);
  } else {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                           "- Spatial dimension > 3", ESMC_CONTEXT, rc);
    return;
  }

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

////////////////

extern "C" void FTN_X(c_esmc_meshcreatefrommeshes)(Mesh **meshapp, Mesh **meshbpp, Mesh **meshpp, 
ESMC_MeshOp_Flag * meshop, double * threshold, int *rc) {
  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }

    Mesh &mesha = **meshapp;
    Mesh &meshb = **meshbpp;
    //it's meshb clips into mesha, need to revert order before this call
    MeshCreateDiff(meshb, mesha, meshpp, *threshold);

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                           "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}



extern "C" void FTN_X(c_esmc_meshcreateredistelems)(Mesh **src_meshpp, int *num_node_gids, int *node_gids, 
                                                   int *num_elem_gids, int *elem_gids,  Mesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshcreateredist()"

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }


    // Call C++ side
    MeshRedist(*src_meshpp, *num_node_gids, node_gids, *num_elem_gids, elem_gids, output_meshpp);


  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                           "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }


  if (rc!=NULL) *rc=ESMF_SUCCESS;
}


// This method verifies that nodes in node_gids array are the same as the local nodes in meshpp, otherwise
// it returns an error (used to test MeshRedist()). 
// To do this check make sure the number of nodes in both cases are the same and that every
// entry in node_gids is contained in meshpp
extern "C" void FTN_X(c_esmc_meshchecknodelist)(Mesh **meshpp, int *_num_node_gids, int *node_gids, 
                                             int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshchecknodelist()"

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }

 /* XMRKX */

    // For convenience deref mesh 
    Mesh *meshp = *meshpp;
    
    // For convenience deref number
    int num_node_gids=*_num_node_gids;


    // Loop through counting local nodes
    int num_local_nodes=0;
    Mesh::iterator ni = meshp->node_begin(), ne = meshp->node_end();
    for (;ni != ne; ++ni) {
      const MeshObj &node = *ni;
 
      if (GetAttr(node).is_locally_owned()) {
        num_local_nodes++;
      }
    }

    // See if number of local nodes is the same
    if (num_node_gids != num_local_nodes) {
      Throw() << "Number of local nodes in mesh ("<<num_local_nodes<<
                 ") different that number in list ("<<num_node_gids<<")";
    }



    // Loop making sure nodes are all here
    for (int i=0; i<num_node_gids; i++) {
      //  Find the corresponding Mesh element
        Mesh::MeshObjIDMap::iterator mi =  meshp->map_find(MeshObj::NODE, node_gids[i]);
        if (mi == meshp->map_end(MeshObj::NODE)) {
          Throw() << "Node "<<node_gids[i]<<" not found in Mesh.";
        }
        
        // Get the element
        const MeshObj &node = *mi; 
        
        // Check if it's locally owned
        if (!GetAttr(node).is_locally_owned()) {
          Throw() << "Node "<<node_gids[i]<<" in Mesh, but not local.";
        }

    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                           "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }


  if (rc!=NULL) *rc=ESMF_SUCCESS;
}


// This method verifies that elems in elem_gids array are the same as the local elems in meshpp, otherwise
// it returns an error (used to test MeshRedist()). 
// To do this check make sure the number of elems in both cases are the same and that every
// entry in elem_gids is contained in meshpp
extern "C" void FTN_X(c_esmc_meshcheckelemlist)(Mesh **meshpp, int *_num_elem_gids, int *elem_gids, 
                                             int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshcheckelemlist()"

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }

 /* XMRKX */

    // For convenience deref mesh 
    Mesh *meshp = *meshpp;
    
    // For convenience deref number
    int num_elem_gids=*_num_elem_gids;


    // Loop through counting local elems
    int num_local_elems=0;
    Mesh::iterator ni = meshp->elem_begin(), ne = meshp->elem_end();
    for (;ni != ne; ++ni) {
      const MeshObj &elem = *ni;
 
      if (GetAttr(elem).is_locally_owned()) {
        num_local_elems++;
      }
    }

    // See if number of local elems is the same
    if (num_elem_gids != num_local_elems) {
      Throw() << "Number of local elems in mesh ("<<num_local_elems<<
                 ") different that number in list ("<<num_elem_gids<<")";
    }



    // Loop making sure elems are all here
    for (int i=0; i<num_elem_gids; i++) {
      //  Find the corresponding Mesh element
        Mesh::MeshObjIDMap::iterator mi =  meshp->map_find(MeshObj::ELEMENT, elem_gids[i]);
        if (mi == meshp->map_end(MeshObj::ELEMENT)) {
          Throw() << "Elem "<<elem_gids[i]<<" not found in Mesh.";
        }
        
        // Get the element
        const MeshObj &elem = *mi; 
        
        // Check if it's locally owned
        if (!GetAttr(elem).is_locally_owned()) {
          Throw() << "Elem "<<elem_gids[i]<<" in Mesh, but not local.";
        }

    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                            "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                           "- Caught unknown exception", ESMC_CONTEXT, rc);
    return;
  }


  if (rc!=NULL) *rc=ESMF_SUCCESS;
}
