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
//------------------------------------------------------------------------------
// INCLUDES    
//------------------------------------------------------------------------------
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_F90Interface.h"

#include "ESMCI_Exception.h"
#include <Mesh/include/ESMCI_MeshCXX.h>
#include "ESMCI_MeshRead.h"
#include "ESMCI_MeshVTK.h"
#include "ESMCI_ParEnv.h"
#include "ESMCI_MeshUtils.h"

namespace ESMCI {

MeshCXX::MeshCXX() : Mesh(){
}
MeshCXX::~MeshCXX(){
}
MeshCXX* MeshCXX::create( int *pdim, int *sdim, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MeshCXX::create()"

   MeshCXX* meshp;
   try {
    int localrc;

    //Initialize return code
    localrc = ESMF_SUCCESS;

    if (*pdim > *sdim) throw;

    meshp = new MeshCXX();

    (meshp)->set_parametric_dimension(*pdim);
    (meshp)->set_spatial_dimension(*sdim);

    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));

    
    *rc = localrc;
   } catch(...) {
     *rc = ESMF_FAILURE;
   }
   return meshp;
} // MeshCXX::create
 

int MeshCXX::addElements(int *numElems, int *elemId, 
                         int *elemType, int *elemConn){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::addElements()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;
   
   try{
      Mesh &mesh = *this;
    // We must first store all nodes in a flat array since element
    // connectivity will index into this array.
    std::vector<MeshObj*> all_nodes;
    
    int num_nodes = mesh.num_nodes();

    all_nodes.resize(num_nodes, static_cast<MeshObj*>(0));

    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();

    for (; ni != ne; ++ni) {

      int seq = ni->get_data_index();

      if (seq >= num_nodes){
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
          "- seq is larger or equal to num_nodes", &localrc);
      return localrc;
    }

      all_nodes[seq] = &*ni;

    }

    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;

    for (int e = 0; e < *numElems; ++e) {

    // Get/deduce the element topology
    const MeshObjTopo *topo = Vtk2Topo(mesh.spatial_dim(), elemType[e]);

    int nnodes = topo->num_nodes;


    std::vector<MeshObj*> nconnect(nnodes, static_cast<MeshObj*>(0));

      // The object
      long eid = elemId[e];
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, eid);

      for (int n = 0; n < nnodes; ++n) {

        if (num_nodes < elemConn[cur_conn]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
                 "- num_nodes < elemConn[cur_conn]", &localrc);
          return localrc;
        }


        nconnect[n] = all_nodes[elemConn[cur_conn++]-1];
  //    nconnect[n] = all_nodes[elemConn[cur_conn++]];

      }

      mesh.add_element(elem, nconnect, topo->number, topo);

    
    } // for e

    // Perhaps commit will be a separate call, but for now commit the mesh here.

    mesh.build_sym_comm_rel(MeshObj::NODE);
    
    mesh.Commit();
  mesh.Print(Par::Out());

    localrc = ESMF_SUCCESS;
   } catch(...) {
    localrc = ESMF_FAILURE;
   }  

   return localrc;
} // MeshCXX::addElements


int MeshCXX::addNodes(int *numNodes, int *nodeId, double *nodeCoord,
                      int *nodeOwner){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::addNodes()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   try{

      Mesh &mesh = *this;
       for (int n = 0; n < *numNodes; ++n) {

      MeshObj *node = new MeshObj(MeshObj::NODE, nodeId[n], n);

      node->set_owner(nodeOwner[n]);
//Par::Out() << "node:" << node->get_id() << " has owner:" << nodeOwner[n] << std::endl;

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

//meshp->Print(Par::Out());

     localrc=ESMF_SUCCESS;
   } catch(...) {
     localrc = ESMF_FAILURE;
   }

   return localrc;
} // MeshCXX::addNodes

int MeshCXX::createDistGrids(int *numLNodes, int *numLElems){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::createDistGrids()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   // Local variables
   int* ngrids;
   int* egrids;

   return localrc;
} // MeshCXX::createDistGrids


int MeshCXX::destroy(){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::destroy()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;
  
   return localrc;
} // MeshCXX::destroy


int MeshCXX::freeMemory(){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::freeMemory()"
// Free Mesh memory, but leave the rest of MeshCXX members intact.

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   return localrc;
} // MeshCXX::freeMemory


int MeshVTKHeader(char *fname, int *numElem, int *numNode, int *connSize){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshVTKHeader()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

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

    ReadVTKMeshHeader(newname, *numElem, *numNode, *connSize);

    localrc = ESMF_SUCCESS;

   return localrc;
} // MeshVTKHeader

int MeshVTKBody(char *fname, int *nodeId, double *nodeCoord, int *nodeOwner, 
                int *elemId, int *elemType, int *elemConn){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshVTKBody()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   try{
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

    localrc = ESMF_SUCCESS;

  } catch(...) {

    localrc = ESMF_FAILURE;

  }

  return localrc;

} //MeshVTKBody

} // namespace ESMCI
