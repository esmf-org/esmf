// $Id: ESMCI_Mesh_F.C,v 1.5 2008/06/30 17:05:11 dneckels Exp $
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

  ESMC::Par::Init(lname, (*use_log == 1 ? true : false));

  delete [] lname;

}


extern "C" void FTN(c_esmc_meshcreate)(ESMC::Mesh **meshpp,
                         int *pdim, int *sdim, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshcreate()"

   try {
    int localrc;

    //Initialize return code
    localrc = ESMF_SUCCESS;

    *meshpp = new ESMC::Mesh();

    (*meshpp)->set_parametric_dimension(*pdim);
    (*meshpp)->set_spatial_dimension(*sdim);

    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));


    *rc = localrc;
   } catch(...) {
     *rc = ESMF_FAILURE;
   }

} // meshcreate

extern "C" void FTN(c_esmc_meshaddnodes)(ESMC::Mesh **meshpp, int *num_nodes, int *nodeId, 
               double *nodeCoord, int *nodeOwner, int *rc) 
{
//std::cout << "num_nodes:" << *num_nodes << std::endl;
   try {
    int localrc;

    //Initialize return code
    localrc = ESMF_SUCCESS;

    ESMC::Mesh *meshp = *meshpp;

    for (int n = 0; n < *num_nodes; ++n) {

      ESMC::MeshObj *node = new ESMC::MeshObj(ESMC::MeshObj::NODE, nodeId[n], n);

      node->set_owner(nodeOwner[n]);

      meshp->add_node(node, 0);


//meshp->Print(ESMC::Par::Out());

    }

    *rc = localrc;
   } catch(...) {
     *rc = ESMF_FAILURE;
   }

} 

extern "C" void FTN(c_esmc_meshaddelements)(ESMC::Mesh **meshpp, int *num_elems, int *elementId, 
               int *elementType, int *elementConn, int *rc) 
{
   try {
    int localrc;

    //Initialize return code
    localrc = ESMF_SUCCESS;

    ESMC::Mesh *meshp = *meshpp;

    std::vector<ESMC::MeshObj*> nconnect;

    int cur_conn = 0;
    for (int n = 0; n < *num_elems; ++n) {

      // The object
      ESMC::MeshObj *elem = new ESMC::MeshObj(ESMC::MeshObj::ELEMENT, elementId[n]);

      // Connect to the relevant nodes.
      int nnodes = elementConn[cur_conn++];

      


    }

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
    int rank = ESMC::Par::Rank();
    int psize = ESMC::Par::Size();

    std::string newname;

    std::string extension = ".vtk";

  // If csize = 1, read fbase.g
     if (psize > 1) {
       std::ostringstream newname_str;
       int ndec = ESMC::numDecimal(psize);
       newname_str << fname << "." << psize << ".";
       newname_str << std::setw(ndec) << std::setfill('0') << rank;
       newname = newname_str.str() + extension;
     } else newname = fname + extension;

    ESMC::ReadVTKMeshHeader(newname, *num_elem, *num_node, *conn_size);

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
    int rank = ESMC::Par::Rank();
    int psize = ESMC::Par::Size();

    std::string newname;

    std::string extension = ".vtk";

  // If csize = 1, read fbase.g
     if (psize > 1) {
       std::ostringstream newname_str;
       int ndec = ESMC::numDecimal(psize);
       newname_str << fname << "." << psize << ".";
       newname_str << std::setw(ndec) << std::setfill('0') << rank;
       newname = newname_str.str() + extension;
     } else newname = fname + extension;

    ESMC::ReadVTKMeshBody(newname, nodeId, nodeCoord, nodeOwner, elemId, elemType, elemConn);

    delete [] fname;

  } catch(...) {

     *rc = ESMF_FAILURE;

  }

}




