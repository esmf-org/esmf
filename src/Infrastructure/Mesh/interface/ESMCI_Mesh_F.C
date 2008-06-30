// $Id: ESMCI_Mesh_F.C,v 1.6 2008/06/30 22:15:11 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Mesh_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_VM.h"
#include "ESMCI_DistGrid.h"
#include "ESMC_RHandle.h"
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMC_Mesh.h"
#include "ESMC_MeshVTK.h"
#include "ESMC_ParEnv.h"
#include "ESMC_MeshUtils.h"

#include <string>

using namespace ESMC;


//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Mesh} class functions.
//
//EOP
//-------------------------------------------------------------------------
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
    int localrc;

    //Initialize return code
    localrc = ESMF_SUCCESS;

    *meshpp = new Mesh();

    (*meshpp)->set_parametric_dimension(*pdim);
    (*meshpp)->set_spatial_dimension(*sdim);

    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));


    *rc = localrc;
   } catch(...) {
     *rc = ESMF_FAILURE;
   }

} // meshcreate

extern "C" void FTN(c_esmc_meshaddnodes)(Mesh **meshpp, int *num_nodes, int *nodeId, 
               double *nodeCoord, int *nodeOwner, int *rc) 
{
//std::cout << "num_nodes:" << *num_nodes << std::endl;
   try {
    int localrc;

    //Initialize return code
    localrc = ESMF_SUCCESS;

    Mesh *meshp = *meshpp;

    for (int n = 0; n < *num_nodes; ++n) {

      MeshObj *node = new MeshObj(MeshObj::NODE, nodeId[n], n);

      node->set_owner(nodeOwner[n]);

      meshp->add_node(node, 0);



    }

meshp->Print(Par::Out());

    *rc = localrc;
   } catch(...) {
     *rc = ESMF_FAILURE;
   }

} 

extern "C" void FTN(c_esmc_meshaddelements)(Mesh **meshpp, int *num_elems, int *elemId, 
               int *elemType, int *elemConn, int *rc) 
{
   try {
    int localrc;

    //Initialize return code
    localrc = ESMF_SUCCESS;

    Mesh *meshp = *meshpp;

    ThrowRequire(meshp);

    Mesh &mesh = *meshp;

    // We must first store all nodes in a flat array since element
    // connectivity will index into this array.
    std::vector<MeshObj*> all_nodes;

    int num_nodes = mesh.num_nodes();

    all_nodes.resize(num_nodes, static_cast<MeshObj*>(0));

    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();

    for (; ni != ne; ++ni) {

      int seq = ni->get_data_index();

      ThrowRequire(seq < num_nodes);

      all_nodes[seq] = &*ni;

    }


    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;

    std::vector<MeshObj*> nconnect(27, static_cast<MeshObj*>(0)); // 27 = quad hex, max nodes

    for (int e = 0; e < *num_elems; ++e) {

      // The object
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, elemId[e]);

      // Connect to the relevant nodes.
      int nnodes = elemConn[cur_conn++];

      for (int n = 0; n < nnodes; ++n) {
      
        ThrowRequire(elemConn[cur_conn] <= num_nodes);
        nconnect[n] = all_nodes[cur_conn++];

      }

      // Get/deduce the element topology
      
      const MeshObjTopo *topo = Vtk2Topo(mesh.spatial_dim(), elemType[e]);

      mesh.add_element(elem, nconnect, 1, topo);


    } // for e

mesh.Print(Par::Out());

    *rc = localrc;
   } catch(...) {
     *rc = ESMF_FAILURE;
   }

} 


/**
 * Routines for reading in a test VTK mesh to fortran arrays (for testing the array interface)
 */
extern "C" void FTN(c_esmc_meshvtkheader)(char *filename, int *num_elem, int *num_node, int *conn_size, int *rc, int nlen) {


  *rc = ESMF_SUCCESS;
  try {

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

  } catch(...) {

     *rc = ESMF_FAILURE;

  }

}

extern "C" void FTN(c_esmc_meshvtkbody)(char *filename, int *nodeId, double *nodeCoord,
                    int *nodeOwner, int *elemId, int *elemType, int *elemConn, int *rc, int nlen) {

  *rc = ESMF_SUCCESS;
  try {

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

  } catch(...) {

     *rc = ESMF_FAILURE;

  }

}




