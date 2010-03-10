// $Id: ESMCI_Mesh_F.C,v 1.33.2.2 2010/03/10 06:33:08 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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

#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMC_LogMacros.inc"             // for LogErr
#include "ESMCI_Mesh.h"
#include "ESMCI_MeshRead.h"
#include "ESMCI_MeshVTK.h"
#include "ESMCI_ParEnv.h"
#include "ESMCI_MeshUtils.h"
#include "ESMCI_GlobalIds.h"
#include "ESMCI_VM.h"
#include "ESMCI_FindPnts.h"



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


extern "C" void FTN(c_esmc_meshinit)(char *logfile, int *use_log, int nlen) {

  char *lname = ESMC_F90toCstring(logfile, nlen);

  Par::Init(lname, (*use_log == 1 ? true : false));

  delete [] lname;

}


extern "C" void FTN(c_esmc_meshcreate)(Mesh **meshpp,
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
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception
}

     // Some error checking of input
    if (*pdim > *sdim) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
		      "- Parametric dimension can't be greater than spatial dimension", &localrc)) throw localrc;
    }

    if ((*pdim < 2) || (*pdim >3)) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
		      "- Parametric dimension can't be greater than 3D or less than 2D", &localrc)) throw localrc;
    }

    if ((*sdim < 2) || (*sdim >3)) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
		      "- Spatial dimension can't be greater than 3D or less than 2D", &localrc)) throw localrc;
    }

    *meshpp = new Mesh();

    (*meshpp)->set_parametric_dimension(*pdim);
    (*meshpp)->set_spatial_dimension(*sdim);

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

} // meshcreate

extern "C" void FTN(c_esmc_meshaddnodes)(Mesh **meshpp, int *num_nodes, int *nodeId, 
               double *nodeCoord, int *nodeOwner, int *rc) 
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
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception
    }

    // Get petCount for error checking
    int localrc;
    int petCount = VM::getCurrent(&localrc)->getPetCount();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
      throw localrc;  // bail out with exception

    // Check node owners
    for (int n = 0; n < *num_nodes; ++n) {
      if ((nodeOwner[n]<0) || (nodeOwner[n]>petCount-1)) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- Bad nodeOwner value ", &localrc)) throw localrc;
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

    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();

    UInt sdim = mesh.spatial_dim();

    for (UInt nc = 0; ni != ne; ++ni) {

      MeshObj &node = *ni;

      double *coord = node_coord->data(node);

      for (UInt c = 0; c < sdim; ++c)
        coord[c] = nodeCoord[nc+c];

      nc += sdim;

    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

} 

extern "C" void FTN(c_esmc_meshwrite)(Mesh **meshpp, char *fname, int *rc, int nlen) {

  try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception
    }

    char *filename = ESMC_F90toCstring(fname, nlen);

    WriteMesh(**meshpp, filename);

    delete [] filename;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;


}

extern "C" void FTN(c_esmc_meshaddelements)(Mesh **meshpp, int *num_elems, int *elemId, 
               int *elemType, int *num_elemConn, int *elemConn, int *rc) 
{
   try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception
    }


    Mesh *meshp = *meshpp;

    ThrowRequire(meshp);

    Mesh &mesh = *meshp;


    // Get parametric dimension
    int parametric_dim=mesh.parametric_dim();

    // Error check input
    //// Check element type
    for (int i=0; i< *num_elems; i++) {
      if (parametric_dim==2) {
        if ((elemType[i] != 5) && (elemType[i] != 9)) {
	  int localrc;
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 2 element types must be either triangles or quadrilaterals ", &localrc)) throw localrc;
        }
      } else if (parametric_dim==3) {
        if ((elemType[i] != 10) && (elemType[i] != 12)) {
	  int localrc;
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 3 element types must be either tetrahedron or hexahedron ", &localrc)) throw localrc;
        }

      }
    }

    //// Check size of connectivity list
    int expected_conn_size=0;
    for (int i=0; i< *num_elems; i++) {
      if (elemType[i]==5) expected_conn_size += 3;   
      else if (elemType[i]==9) expected_conn_size += 4;   
      else if (elemType[i]==10) expected_conn_size += 4;   
      else if (elemType[i]==12) expected_conn_size += 8;   
    }
    if (expected_conn_size != *num_elemConn) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
       "- element connectivity list doesn't contain the right number of entries ", &localrc)) throw localrc;
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

      ThrowRequire(seq < num_nodes);

      all_nodes[seq] = &*ni;

    }

    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;

    for (int e = 0; e < *num_elems; ++e) {

    // Get/deduce the element topology
    //printf("mesh.spatial_dim() = %d\n", mesh.spatial_dim() );
    const MeshObjTopo *topo = Vtk2Topo(mesh.spatial_dim(), elemType[e]);

    int nnodes = topo->num_nodes;

    std::vector<MeshObj*> nconnect(nnodes, static_cast<MeshObj*>(0));

      // The object
      long eid = elemId[e];
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, eid);

      for (int n = 0; n < nnodes; ++n) {
      
        // Get 0-based node index
        int node_index=elemConn[cur_conn]-1;

        // Check elemConn
        if ((node_index < 0) || (node_index > num_nodes-1)) {
	  int localrc;
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
	   "- elemConn entries should not be greater than number of nodes on processor ", &localrc)) throw localrc;
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
	   "- there are nodes on this PET that were not used in the element connectivity list ", &localrc)) throw localrc;
    }

    // Perhaps commit will be a separate call, but for now commit the mesh here.

    mesh.build_sym_comm_rel(MeshObj::NODE);

    mesh.Commit();

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

} 


/**
 * Routines for reading in a test VTK mesh to fortran arrays (for testing the array interface)
 */
extern "C" void FTN(c_esmc_meshvtkheader)(char *filename, int *num_elem, int *num_node, int *conn_size, int *rc, int nlen) {

  try {
 
{
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
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
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN(c_esmc_meshvtkbody)(char *filename, int *nodeId, double *nodeCoord,
                    int *nodeOwner, int *elemId, int *elemType, int *elemConn, int *rc, int nlen) {

  try {

{
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
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
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN(c_esmc_meshdestroy)(Mesh **meshpp, int *rc) {

  try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception
    }

    Mesh *meshp = *meshpp;
    
    delete meshp;
    
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN(c_esmc_meshfreememory)(Mesh **meshpp, int *rc) {

  try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception
    }

    Mesh *meshp = *meshpp;

    delete meshp;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

extern "C" void FTN(f_esmf_getmeshdistgrid)(int*, int*, int*, int*);


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

extern "C" void FTN(c_esmc_meshget)(Mesh **meshpp, int *num_nodes, int *num_elements, int *rc){

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
   return;  // bail out with exception
    }

    Mesh *meshp = *meshpp;

    *num_nodes = meshp->num_nodes();
    *num_elements = meshp->num_elems();

    if(rc != NULL) *rc = ESMF_SUCCESS;
}
    

extern "C" void FTN(c_esmc_meshcreatedistgrids)(Mesh **meshpp, int *ngrid, int *egrid, int *num_lnodes, int *num_lelems, int *rc) {

  // Declare id vectors
  std::vector<int> ngids; 
  std::vector<int> egids; 
  
 
  try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception
    }

    Mesh *meshp = *meshpp;
    
    // The nodal map.  First get the set of owned nodal ids
    {
      Context c; c.set(Attr::OWNED_ID);
      Attr ae(MeshObj::ELEMENT, c);
      
      getNodeGIDS(*meshp, ngids);
      
      getMeshGIDS(*meshp, ae, egids);
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
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }


  // Create the distgrids
  {
    int nsize = *num_lnodes = ngids.size();
    int rc1;
    
    int *indices = (nsize==0)?NULL:&ngids[0];

    FTN(f_esmf_getmeshdistgrid)(ngrid, &nsize, indices, &rc1);

    if (ESMC_LogDefault.MsgFoundError(rc1,
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
  }
  {
    int esize = *num_lelems = egids.size();
    int rc1;
    
    int *indices = (esize==0)?NULL:&egids[0];

    FTN(f_esmf_getmeshdistgrid)(egrid, &esize, indices, &rc1);

    if(ESMC_LogDefault.MsgFoundError(rc1,
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}


extern "C" void FTN(c_esmc_meshinfoserialize)(int *intMeshFreed,
	        void *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *localrc){

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > vars.
    int size = sizeof(int);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < size) {         
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add a Mesh object", localrc);
         return;
      }
    }

    // Save keyCount
    ip= (int *)((char *)(buffer) + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = *intMeshFreed;
    }

    // Adjust offset
    *offset += size;

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


extern "C" void FTN(c_esmc_meshinfodeserialize)(int *intMeshFreed, 
                                 void *buffer, int *offset, int *localrc){

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // Get pointer
    ip= (int *)((char *)(buffer) + *offset);

    // Get values
    *intMeshFreed=*ip++;

    // Adjust offset
    *offset += sizeof(int);

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


extern "C" void FTN(c_esmc_meshserialize)(Mesh **meshpp,
	        void *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshserialize()"

   try {

  // Initialize the parallel environment for mesh (if not already done)
     {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
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

      for (int i=0; i<numSets; i++) {
	size +=nvalSetSizes[i]*sizeof(UInt);
      }

      for (int i=0; i<numSets; i++) {
	size +=nvalSetObjSizes[i]*sizeof(UInt);
      }

    // TODO: verify length > vars.
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < size) {         
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add Mesh object", rc);
         return;
      }
    }

    // Save integers
    ip= (int *)((char *)(buffer) + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = mesh.spatial_dim();
      *ip++ = mesh.parametric_dim();
      *ip++ = numSets;
    }

    // Save UInt data
    uip=(UInt *)ip;
    if (*inquireflag != ESMF_INQUIREONLY) {

      // Save set sizes and values
      for (int i=0; i<numSets; i++) {
	*uip++=nvalSetSizes[i];
      }

      int k=0;
      for (int i=0; i<numSets; i++) {
	for (int j=0; j<nvalSetSizes[i]; j++) {
	  *uip++=nvalSetVals[k];
	  k++;
	}
      }

      // Save set obj sizes and values
      for (int i=0; i<numSets; i++) {
	*uip++=nvalSetObjSizes[i];
      }

      k=0;
      for (int i=0; i<numSets; i++) {
	for (int j=0; j<nvalSetObjSizes[i]; j++) {
	  *uip++=nvalSetObjVals[k];
	  k++;

	}
      }

    }

    // Adjust offset
    *offset += size;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

    return;
} 


extern "C" void FTN(c_esmc_meshdeserialize)(Mesh **meshpp, 
                                 void *buffer, int *offset, int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshdeserialize()"

   try {

  // Initialize the parallel environment for mesh (if not already done)
     {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,NULL))
   throw localrc;  // bail out with exception
     }

    int *ip;
    UInt *uip;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get pointer
    ip= (int *)((char *)(buffer) + *offset);

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
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

    return;
} 


extern "C" void FTN(c_esmc_meshfindpnt)(Mesh **meshpp, int *unmappedaction, int *dimPnts, int *numPnts, 
					double *pnts, int *pets, int *rc){

   try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
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
          " - point dimension doesn't match grid/mesh dimension", rc);
         return;
      } else if (fp_err == ESMCI_FINDPNT_PNT_NOT_FOUND) {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          " - some points lie outside of grid/mesh ", rc);
         return;
      } else {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   			     	  " - unknown error in findpnt", rc);
         return;
      }
    }

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }


    return;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    return;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return;
  }

  // Set return code 
  if(rc != NULL) *rc = ESMF_SUCCESS;
}
