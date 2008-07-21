// $Id: ESMCI_Mesh_F.C,v 1.10 2008/07/21 23:25:51 theurich Exp $
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
#include "ESMCI_VM.h"
#include "ESMCI_DistGrid.h"
#include "ESMC_RHandle.h"
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMC_Mesh.h"
#include "ESMC_MeshRead.h"
#include "ESMC_MeshVTK.h"
#include "ESMC_ParEnv.h"
#include "ESMC_MeshUtils.h"
#include "ESMC_GlobalIds.h"

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
    ThrowRequire(meshp);
    Mesh &mesh = *meshp;

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

      nc += 3;  // nodeCoord is stride 3 whether mesh is or not.

    }

//meshp->Print(Par::Out());

    *rc = localrc;
   } catch(...) {
     *rc = ESMF_FAILURE;
   }

} 

extern "C" void FTN(c_esmc_meshwrite)(Mesh **meshpp, char *fname, int *rc, int nlen) {

  char *filename = ESMC_F90toCstring(fname, nlen);

  try {

    *rc = ESMF_SUCCESS;
    WriteMesh(**meshpp, filename);

  } catch(...) {
    *rc = ESMF_FAILURE;
  }

  delete [] filename;

}

extern "C" void FTN(c_esmc_meshaddelements)(Mesh **meshpp, int *num_elems, int *has_eids, int *elemId, 
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


    std::vector<long> new_ids;
    if (!has_eids) {
      // Manufacture unique global element ids.
      std::vector<long> current_ids;
      new_ids.resize(*num_elems+1, 0); // add 1 so that new_ids is not empty

      GlobalIds(current_ids, new_ids);
    }

    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;

    for (int e = 0; e < *num_elems; ++e) {

    // Get/deduce the element topology
    const MeshObjTopo *topo = Vtk2Topo(mesh.spatial_dim(), elemType[e]);

    int nnodes = topo->num_nodes;

    std::vector<MeshObj*> nconnect(nnodes, static_cast<MeshObj*>(0));

      // The object
      long eid = has_eids ? elemId[e] : new_ids[e];
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, eid);

      for (int n = 0; n < nnodes; ++n) {
      
        ThrowRequire(elemConn[cur_conn] <= num_nodes);
        nconnect[n] = all_nodes[elemConn[cur_conn++]-1];

      }

      mesh.add_element(elem, nconnect, topo->number, topo);


    } // for e

    // Perhaps commit will be a separate call, but for now commit the mesh here.
    mesh.Commit();
//mesh.Print(Par::Out());

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




