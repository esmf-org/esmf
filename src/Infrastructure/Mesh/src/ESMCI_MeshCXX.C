// $Id: ESMCI_MeshCXX.C,v 1.18 2011/04/28 18:53:40 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
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
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_F90Interface.h"

#include "ESMCI_Exception.h"
#include <Mesh/include/ESMCI_MeshCXX.h>
#include "ESMCI_MeshRead.h"
#include "ESMCI_MeshVTK.h"
#include "ESMCI_ParEnv.h"
#include "ESMCI_MeshUtils.h"
#include "ESMCI_VM.h"

using std::cerr;
using std::endl;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_MeshCXX.C,v 1.18 2011/04/28 18:53:40 rokuingh Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

MeshCXX::MeshCXX() {
  meshFreed = 1;
}

MeshCXX::~MeshCXX(){
  cerr << "MeshCXX::~MeshCXX(): destructor entered" << endl;
  if (!isMeshFreed()) {
    cerr << "MeshCXX::~MeshCXX(): meshPointer = " << meshPointer << endl;
    delete meshPointer;
  }
}



MeshCXX* MeshCXX::create( int pdim, int sdim, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MeshCXX::create()"

   MeshCXX* meshCXXp;
   Mesh* meshp;
   try {

     // Initialize the parallel environment for mesh (if not already done)
     {
       int localrc;
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	 throw localrc;  // bail out with exception
     }

     // Some error checking of input
    if (pdim > sdim) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- Parametric dimension can't be greater than spatial dimension", rc)) return (MeshCXX *)NULL;
    }

    if ((pdim < 2) || (pdim >3)) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
	 "- Parametric dimension can't be greater than 3D or less than 2D", rc)) return (MeshCXX *)NULL;
    }

    if ((sdim < 2) || (sdim >3)) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
	 "- Spatial dimension can't be greater than 3D or less than 2D", rc)) return (MeshCXX *)NULL;
    }

    meshCXXp = new MeshCXX();
    meshp = new Mesh();

    (meshp)->set_parametric_dimension(pdim);
    (meshp)->set_spatial_dimension(sdim);

    (meshCXXp)->meshPointer = meshp;

    cerr << "MeshCXX::create(): meshp = " << meshp;
    cerr << ".  Setting meshFreed to 0."  << endl;
    meshCXXp->meshFreed = 0;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", rc);
    }

    return (MeshCXX *)NULL;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc);
    return (MeshCXX *)NULL;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", rc);
    return (MeshCXX *)NULL;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

   return meshCXXp;
} // MeshCXX::create

  int MeshCXX::destroy(MeshCXX **meshpp) {
    int localrc;

    if (meshpp==NULL || *meshpp == NULL) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Mesh", &localrc);
      return localrc;
    }

    // call MeshCXX destructor
    delete *meshpp; 
    
    // Set to NULL
    *meshpp=(MeshCXX*)NULL;

    try{
      // Initialize the parallel environment for mesh (if not already done)
      {
	ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
	if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	  throw localrc;  // bail out with exception
      }
      
      
    } catch(std::exception &x) {
      // catch Mesh exception return code 
      if (x.what()) {
	ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				      x.what(), &localrc);
	return localrc;
      } else {
	ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				      "UNKNOWN",  &localrc);
	return localrc;
      }
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
      return localrc;
    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				    "- Caught unknown exception", &localrc);
      return localrc;
    }
    
    // Return SUCCESS
    return ESMF_SUCCESS;
  }
  
int MeshCXX::addElements(int numElems, int *elemId, 
                         int *elemType, int *elemConn){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::addElements()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;
   
   try{
     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	 throw localrc;  // bail out with exception
     }
     
      Mesh &mesh = *meshPointer;


    // Get parametric dimension
    int parametric_dim=mesh.parametric_dim();

    // Error check input
    //// Check element type
    for (int i=0; i< numElems; i++) {
      if (parametric_dim==2) {
        if ((elemType[i] != 5) && (elemType[i] != 9)) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 2 element types must be either triangles or quadrilaterals ", &localrc)) throw localrc;
        }
      } else if (parametric_dim==3) {
        if ((elemType[i] != 10) && (elemType[i] != 12)) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 3 element types must be either tetrahedron or hexahedron ", &localrc)) throw localrc;
        }

      }
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
          "- seq is larger or equal to num_nodes", &localrc);
      return localrc;
    }

      all_nodes[seq] = &*ni;

    }

    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;

    for (int e = 0; e < numElems; ++e) {

    // Get/deduce the element topology
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
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
	   "- there are nodes on this PET that were not used in the element connectivity list ", &localrc)) throw localrc;
    }

    // Perhaps commit will be a separate call, but for now commit the mesh here.
    mesh.build_sym_comm_rel(MeshObj::NODE);
    
    mesh.Commit();

#ifdef ESMF_PARLOG
  mesh.Print(Par::Out());
#endif

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), &localrc);
      return localrc;
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN",  &localrc);
      return localrc;
    }
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return localrc;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", &localrc);
    return localrc;
  }

  // Return SUCCESS
  return ESMF_SUCCESS;

} // MeshCXX::addElements


int MeshCXX::addNodes(int numNodes, int *nodeId, double *nodeCoord,
                      int *nodeOwner){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::addNodes()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   try{
     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	 throw localrc;  // bail out with exception
     }


     // Get petCount for error checking
     int petCount = VM::getCurrent(&localrc)->getPetCount();
     if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
       throw localrc;  // bail out with exception
     
     // Check node owners
     for (int n = 0; n < numNodes; ++n) {
       if ((nodeOwner[n]<0) || (nodeOwner[n]>petCount-1)) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
	  "- Bad nodeOwner value ", &localrc)) throw localrc;
       }
     }
     
     Mesh &mesh = *meshPointer;
     for (int n = 0; n < numNodes; ++n) {
       
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

   } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), &localrc);
      return localrc;
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN",  &localrc);
      return localrc;
    }
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return localrc;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", &localrc);
    return localrc;
  }

  // Return SUCCESS
  return ESMF_SUCCESS;

} // MeshCXX::addNodes

extern "C" void FTN(f_esmf_getmeshdistgrid)(int*, int*, int*, int*);


/**
 * Sort nodes by the order in which they were originally declared
 * (which is stored by get_data_index)
 */

std::vector<int> MeshCXX::getNodeGIDS(){

  std::vector<int> ngid;

  UInt nnodes = meshPointer->num_nodes();

  Mesh::iterator ni = meshPointer->node_begin(), ne = meshPointer->node_end();

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

  return ngid;

} // getNodeGIDS



int MeshCXX::createDistGrids(int *ngrid, int *egrid, int *numLNodes, 
     int *numLElems){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::createDistGrids()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

  // The nodal map.  First get the set of owned nodal ids
   try {
     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	 throw localrc;  // bail out with exception
     }
     

     std::vector<int> ngids;
     std::vector<int> egids;
     {
       Context c; c.set(Attr::OWNED_ID);
       Attr ae(MeshObj::ELEMENT, c);
       
       ngids = getNodeGIDS();
       
       //  getMeshGIDS(*this, ae, egids);
       getMeshGIDS(*meshPointer, ae, egids);
     }
     
     
     // Create the distgrids
     {
       int nsize = *numLNodes = ngids.size();
       int rc1;
       
       FTN(f_esmf_getmeshdistgrid)(ngrid, &nsize, &ngids[0], &rc1);
       
       ESMC_LogDefault.MsgFoundError(rc1,
				     ESMCI_ERR_PASSTHRU,
				     ESMC_NOT_PRESENT_FILTER(&localrc));
       
     }
     {
       int esize = *numLElems = egids.size();
       int rc1;
       FTN(f_esmf_getmeshdistgrid)(egrid, &esize, &egids[0], &rc1);
       
       ESMC_LogDefault.MsgFoundError(rc1,
				     ESMCI_ERR_PASSTHRU,
				     ESMC_NOT_PRESENT_FILTER(&localrc));
       
     }
     
   } catch(std::exception &x) {
     // catch Mesh exception return code 
     if (x.what()) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     x.what(), &localrc);
       return localrc;
     } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     "UNKNOWN",  &localrc);
       return localrc;
     }
   }catch(int localrc){
     // catch standard ESMF return code
     ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
     return localrc;
   } catch(...){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				   "- Caught unknown exception", &localrc);
     return localrc;
  }
   
  // Return SUCCESS
  return ESMF_SUCCESS;

} // MeshCXX::createDistGrids



int MeshCXX::freeMemory(){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::freeMemory()"
// Free Mesh memory, but leave the rest of MeshCXX members intact.

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   try {
     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	 throw localrc;  // bail out with exception
     }
     
     cerr << "MeshCXX::freeMemory(): meshPointer = " << meshPointer;
     cerr << ".  Setting meshFreed to 1." << endl;
     delete meshPointer;
     meshFreed=1;
          
   } catch(std::exception &x) {
     // catch Mesh exception return code 
     if (x.what()) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     x.what(), &localrc);
       return localrc;
     } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     "UNKNOWN",  &localrc);
       return localrc;
     }
   }catch(int localrc){
     // catch standard ESMF return code
     ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
     return localrc;
   } catch(...){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				   "- Caught unknown exception", &localrc);
     return localrc;
  }
   
   // Return SUCCESS
   return ESMF_SUCCESS;
   
} // MeshCXX::freeMemory
  
  
int MeshVTKHeader(const char *fname, int *numElem, int *numNode, int *connSize){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshVTKHeader()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   try{
     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	 throw localrc;  // bail out with exception
     }

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
   } catch(std::exception &x) {
     // catch Mesh exception return code 
     if (x.what()) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     x.what(), &localrc);
       return localrc;
     } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     "UNKNOWN",  &localrc);
       return localrc;
     }
   }catch(int localrc){
     // catch standard ESMF return code
     ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
     return localrc;
   } catch(...){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				   "- Caught unknown exception", &localrc);
     return localrc;
   }
   
   // Return SUCCESS
   return ESMF_SUCCESS;

} // MeshVTKHeader

int MeshVTKBody(const char *fname, int *nodeId, double *nodeCoord, int *nodeOwner, 
                int *elemId, int *elemType, int *elemConn){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshVTKBody()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   try{
     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	 throw localrc;  // bail out with exception
     }

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

   } catch(std::exception &x) {
     // catch Mesh exception return code 
     if (x.what()) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     x.what(), &localrc);
       return localrc;
     } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     "UNKNOWN",  &localrc);
       return localrc;
     }
   }catch(int localrc){
     // catch standard ESMF return code
     ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
     return localrc;
   } catch(...){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				   "- Caught unknown exception", &localrc);
     return localrc;
   }
   
   // Return SUCCESS
   return ESMF_SUCCESS;

} //MeshVTKBody

int MeshCXX::isMeshFreed(){
  cerr << "MeshCXX::isMeshFreed(): meshFreed = " << meshFreed << endl;
  return meshFreed;
}


int MeshCXX::numNodes(){
   return numLNodes;
}


int MeshCXX::numElements(){
   return numLElements;
}

int MeshCXX::meshWrite(const char* fileName){

  int localrc;
  //Initialize localrc; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  try{
     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,NULL))
	 throw localrc;  // bail out with exception
     }

    WriteMesh(*meshPointer, fileName);
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				    x.what(), &localrc);
      return localrc;
    } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
         "UNKNOWN",  &localrc);
       return localrc;
    }
  }catch(int localrc){
     // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &localrc);
    return localrc;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", &localrc);
    return localrc;
  }
   
   // Return SUCCESS
   return ESMF_SUCCESS;

} //meshWrite

} // namespace ESMCI
