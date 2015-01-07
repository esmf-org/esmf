// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2015, University Corporation for Atmospheric Research,
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
#include "ESMCI_LogErr.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_Exception.h"
#include "Mesh/include/ESMCI_MeshCXX.h"
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
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

MeshCXX::MeshCXX() {
  meshFreed = 1;
  level=MeshCXXLevel_Empty;
}

MeshCXX::~MeshCXX(){
  //cerr << "MeshCXX::~MeshCXX(): destructor entered" << endl;
  if (!isMeshFreed()) {
    //cerr << "MeshCXX::~MeshCXX(): meshPointer = " << meshPointer << endl;
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
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
         ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
     }

     // Some error checking of input
    if (pdim > sdim) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- Parametric dimension can't be greater than spatial dimension",
         ESMC_CONTEXT, rc)) return (MeshCXX *)NULL;
    }

    if ((pdim < 2) || (pdim >3)) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
	 "- Parametric dimension can't be greater than 3D or less than 2D",
         ESMC_CONTEXT, rc)) return (MeshCXX *)NULL;
    }

    if ((sdim < 2) || (sdim >3)) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
	 "- Spatial dimension can't be greater than 3D or less than 2D",
         ESMC_CONTEXT, rc)) return (MeshCXX *)NULL;
    }

    meshCXXp = new MeshCXX();
    meshp = new Mesh();

    (meshp)->set_parametric_dimension(pdim);
    (meshp)->set_spatial_dimension(sdim);

    (meshCXXp)->meshPointer = meshp;

    // Set the number of nodes and elements
    ThrowAssert(meshp->num_elems() == 0);
    meshCXXp->numLElements = meshp->num_elems();
    ThrowAssert(meshp->num_nodes() == 0);
    meshCXXp->numLNodes = meshp->num_nodes();

    // Init to bad value
    meshCXXp->numOwnedNodes=-1;
    meshCXXp->numOwnedElements=-1;


    //cerr << "MeshCXX::create(): meshp = " << meshp;
    //cerr << ".  Setting meshFreed to 0."  << endl;
    meshCXXp->meshFreed = 0;

    // Set create level
    meshCXXp->level=MeshCXXLevel_Created;

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return (MeshCXX *)NULL;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return (MeshCXX *)NULL;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return (MeshCXX *)NULL;
  }

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

   return meshCXXp;
} // MeshCXX::create


  // Calculate the number of local elements disregarding split elements
  int calc_num_local_elems(Mesh &mesh) {

    int num;
    if (mesh.is_split) {
      num=0;
      Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        
        MeshObj &elem = *ei;
        
        // Don't do split elements
        if (elem.get_id() > mesh.max_non_split_id) continue; 
        
        num++;
      }
    } else {
      num=mesh.num_elems();
    }

    return num;
}


#undef  ESMC_METHOD
#define ESMC_METHOD "MeshCXX::createFromFile()"
MeshCXX* MeshCXX::createFromFile(const char *filename, int fileTypeFlag, 
				 int *convertToDual,
				 int *addUserArea,
				 const char *meshname,
				 int *addMask,
				 const char *varname,
				 int *rc) {
   MeshCXX* meshCXXp;
   Mesh* meshp;
   try {

     // Initialize the parallel environment for mesh (if not already done)
     {
       int localrc;
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
         ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
     }

    int localrc;
    meshp = ESMCI::Mesh::createfromfile(filename, fileTypeFlag,
					convertToDual, addUserArea, meshname, 
					addMask, varname, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception


  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, rc);
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, rc);
    }

    return (MeshCXX *)NULL;
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return (MeshCXX *)NULL;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, rc);
    return (MeshCXX *)NULL;
  }
   meshCXXp = new MeshCXX();
   (meshCXXp)->meshPointer = meshp;

   // Set the number of nodes and elements
   meshCXXp->numLElements = calc_num_local_elems(*meshp);

   meshCXXp->numLNodes = meshp->num_nodes();

   meshCXXp->meshFreed = 0;

   // Mark Mesh as finshed
   meshCXXp->level=MeshCXXLevel_Finished;


   // Create distgrids and number of owned nodes
   // TODO: I THINK THAT HOW THE DISTGRIDS ARE
   //       CREATED AND PASSED AROUND IS WRONG, 
   //       BUT I DON'T THINK THEY ARE ACTUALLY USED 
   //       THROUGH C/PYTHON RIGHT NOW, SO I'll WAIT
   //       AND FIX THE WHOLE THING WHEN I UNIFY THE MESH
   // (THE PREVIOUS VERSION DIDN'T SET DISTGRIDS EITHER)
   int tmpNDistgrid;
   int tmpEDistgrid;
   meshCXXp->createDistGrids(&tmpNDistgrid,
                             &tmpEDistgrid,
                             &meshCXXp->numOwnedNodes,
                             &meshCXXp->numOwnedElements);


   // NOW DONE ABOVE
#if 0
   // Calc and set the number of owned nodes
   int num_owned_nodes=0;
   Mesh::iterator ni2 = meshp->node_begin(), ne2 = meshp->node_end();
   for (ni2; ni2 != ne2; ++ni2) {
     MeshObj &node = *ni2;
     if (!GetAttr(node).is_locally_owned()) continue;
     num_owned_nodes++;
   }
   meshCXXp->numOwnedNodes=num_owned_nodes;

   // Calc and set the number of owned elements
   int num_owned_elems=0;
   Mesh::iterator ei = meshp->elem_begin(), ee = meshp->elem_end();
   for (; ei != ee; ++ei) {
     MeshObj &elem = *ei;
     if (!GetAttr(elem).is_locally_owned()) continue;
     num_owned_elems++;
   }   
   meshCXXp->numOwnedElements=num_owned_elems;
#endif

  // Set return code 
  if (rc!=NULL) *rc = ESMF_SUCCESS;

   return meshCXXp;
} // MeshCXX::createFromFile
  

  int MeshCXX::destroy(MeshCXX **meshpp) {
    int localrc;

    if (meshpp==NULL || *meshpp == NULL) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Mesh", ESMC_CONTEXT, &localrc);
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
	if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
      }
      
      
    } catch(std::exception &x) {
      // catch Mesh exception return code 
      if (x.what()) {
	ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				      x.what(), ESMC_CONTEXT, &localrc);
	return localrc;
      } else {
	ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				      "UNKNOWN", ESMC_CONTEXT, &localrc);
	return localrc;
      }
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc);
      return localrc;
    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught unknown exception", ESMC_CONTEXT, &localrc);
      return localrc;
    }
    
    // Return SUCCESS
    return ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "MeshCXX::getLocalCoords()"
  void MeshCXX::getLocalCoords(double *nodeCoord, int *num_nodes, int *num_dims, int *rc) {
    Mesh &mesh = *meshPointer;

    int sdim;

    // Get coords pointer and spatial dimension depending on existence
    // of original coordinates field.
    MEField<> *coords = mesh.GetField("orig_coordinates");
    if (coords) {
      sdim = 2;
    } else {
      coords = mesh.GetCoordField();
      sdim=mesh.spatial_dim();
    }
    *num_nodes = this->numOwnedNodes;
    *num_dims = sdim;

    // Make a map between data index and associated node pointer
    std::vector<std::pair<int,MeshObj *> > index_to_node;
    index_to_node.reserve(*num_nodes);
    
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

    // Set return code 
    if (rc!=NULL) *rc = ESMF_SUCCESS;
}

// TODO: most of this routine is duplicated in ESMCI_Mesh_F.C - should be merged  
int MeshCXX::addElements(int numElems, int *elemId, 
                         int *elemType, int *elemConn, 
                         int *elemMask, double *elemArea){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::addElements()"

   int localrc;
   //Initialize localrc; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;
   
   try{
     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
         ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
     }
     
      Mesh &mesh = *meshPointer;

     // Make sure that we're at the correct level to do this
     if (level >= MeshCXXLevel_Finished) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- Can't add elements twice to a Mesh. ", ESMC_CONTEXT, &localrc))
           throw localrc;
     }

     if (level < MeshCXXLevel_NodesAdded) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- Need to add nodes before adding elements. ", ESMC_CONTEXT,
           &localrc)) throw localrc;
     }



    // Get parametric dimension
    int parametric_dim=mesh.parametric_dim();

    // Error check input
    //// Check element type
    for (int i=0; i< numElems; i++) {
      if (parametric_dim==2) {
        if ((elemType[i] != 5) && (elemType[i] != 9)) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 2 element types must be "
           "either triangles or quadrilaterals ", ESMC_CONTEXT, &localrc))
           throw localrc;
        }
      } else if (parametric_dim==3) {
        if ((elemType[i] != 10) && (elemType[i] != 12)) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 3 element types must be "
           "either tetrahedron or hexahedron ", ESMC_CONTEXT, &localrc))
           throw localrc;
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
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "- seq is larger or equal to num_nodes", ESMC_CONTEXT, &localrc);
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
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, eid, e);

      for (int n = 0; n < nnodes; ++n) {

        // Get 0-based node index
        int node_index=elemConn[cur_conn]-1;

        // Check elemConn
        if ((node_index < 0) || (node_index > num_nodes-1)) {
	  int localrc;
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- bad elementConn value",
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
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- there are nodes on this PET that were not used in the element "
        "connectivity list ", ESMC_CONTEXT, &localrc)) throw localrc;
    }

    // numElems is input data from AddElements
    // numLElements is what is returned to C interface
    // what is mesh.num_elems?  petlocal?  should it = numElems?
    ThrowAssert(mesh.num_elems() == numElems);
    numLElements = numElems;

    // required to add frac fields to the mesh for conservative regridding
    Context ctxt; ctxt.flip();
    MEField<> *elem_frac = mesh.RegisterField("elem_frac",
                        MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

  // Handle element masking
  bool has_elem_mask=false;
  if (elemMask != NULL) { // if masks exist

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
  if (elemArea != NULL) { // if areas exist
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


    // Mark Mesh as finshed
    level=MeshCXXLevel_Finished;

    // Calc and set the number of owned nodes
    int num_owned_nodes=0;
    // NEW MESH interator because the one above has probably been trashed by
    // changes in the commit, etc. 
    Mesh::iterator ni2 = mesh.node_begin(), ne2 = mesh.node_end();
    for (ni2; ni2 != ne2; ++ni2) {
      MeshObj &node = *ni2;
   
      if (!GetAttr(node).is_locally_owned()) continue;
      
      num_owned_nodes++;
    }
    numOwnedNodes=num_owned_nodes;

    //    printf(" num_owned_nodes=%d num_nodes=%d\n",num_owned_nodes, mesh.num_nodes());


    // Calc and set the number of owned elements
    int num_owned_elems=0;
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
   
      if (!GetAttr(elem).is_locally_owned()) continue;
      
      num_owned_elems++;
    }   
    numOwnedElements=num_owned_elems;




#ifdef ESMF_PARLOG
  mesh.Print(Par::Out());
#endif

  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, &localrc);
      return localrc;
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, &localrc);
      return localrc;
    }
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &localrc);
    return localrc;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, &localrc);
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
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
         ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
     }


     // Make sure that we're at the correct level to do this
     if (level >= MeshCXXLevel_NodesAdded) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- Can't add nodes twice to a Mesh. ", ESMC_CONTEXT, &localrc))
           throw localrc;
     }

     if (level < MeshCXXLevel_Created) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- Mesh must be created before adding nodes. ", ESMC_CONTEXT,
           &localrc)) throw localrc;
     }


     // Get petCount for error checking
     int petCount = VM::getCurrent(&localrc)->getPetCount();
     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
       ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
     
     // Check node owners
     for (int n = 0; n < numNodes; ++n) {
       if ((nodeOwner[n]<0) || (nodeOwner[n]>petCount-1)) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
	  "- Bad nodeOwner value ", ESMC_CONTEXT, &localrc)) throw localrc;
       }
     }
     
     Mesh &mesh = *meshPointer;
     for (int n = 0; n < numNodes; ++n) {
//fprintf(stderr, "ESMCI - adding node %d\n", nodeId[n]);
       
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

//fprintf(stderr, "ESMCI - node coord [%f,%f] \n", nodeCoord[nc+0], nodeCoord[nc+1]);       
      nc += sdim;

    }

    // numNodes is input data from AddNodes
    // numLNodes is what is returned to C interface
    // what is mesh.num_nodes?  petlocal?  should it = numNodes?
    ThrowAssert(mesh.num_nodes() == numNodes);
    numLNodes = numNodes;

    // Mark Mesh as having nodes added
    level=MeshCXXLevel_NodesAdded;

   } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  x.what(), ESMC_CONTEXT, &localrc);
      return localrc;
    } else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
   					  "UNKNOWN", ESMC_CONTEXT, &localrc);
      return localrc;
    }
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &localrc);
    return localrc;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, &localrc);
    return localrc;
  }

  // Return SUCCESS
  return ESMF_SUCCESS;

} // MeshCXX::addNodes

extern "C" void FTN_X(f_esmf_getmeshdistgrid)(int*, int*, int*, int*);


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



/**
 * Sort elements by the order in which they were originally declared
 * (which is stored by get_data_index)
 * Don't include split elements
 */
void getElemGIDS(Mesh &mesh, std::vector<int> &egid) {

  UInt nelems = mesh.num_elems();

  Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();

  std::vector<std::pair<int,int> > gids;

  for (; ei != ee; ++ei) {

    MeshObj &elem = *ei;

    if (!GetAttr(elem).is_locally_owned()) continue;

    // Don't do split elements
    if (mesh.is_split && elem.get_id() > mesh.max_non_split_id) continue; 

    int idx = elem.get_data_index();

    gids.push_back(std::make_pair(idx, elem.get_id()));

  }

  std::sort(gids.begin(), gids.end());

  egid.clear();
  for (UInt i = 0; i < gids.size(); ++i) egid.push_back(gids[i].second);

}



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
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
         ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
     }
     

     if (level < MeshCXXLevel_Finished) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- Mesh must be finished before making distgrids on it. ", 
           ESMC_CONTEXT, &localrc)) throw localrc;
     }


     std::vector<int> ngids;
     std::vector<int> egids;
     {
       
       ngids = getNodeGIDS();
       
       // getMeshGIDS(*this, ae, egids);
       // getMeshGIDS(*meshPointer, ae, egids);
       getElemGIDS(*meshPointer, egids);
     }
     
     
     // Create the distgrids
     {
       int nsize = *numLNodes = ngids.size();
       int rc1;
       
       FTN_X(f_esmf_getmeshdistgrid)(ngrid, &nsize, &ngids[0], &rc1);
       
       ESMC_LogDefault.MsgFoundError(rc1,
				     ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
				     ESMC_NOT_PRESENT_FILTER(&localrc));
       
     }
     {
       int esize = *numLElems = egids.size();
       int rc1;
       FTN_X(f_esmf_getmeshdistgrid)(egrid, &esize, &egids[0], &rc1);
       
       ESMC_LogDefault.MsgFoundError(rc1,
				     ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
				     ESMC_NOT_PRESENT_FILTER(&localrc));
       
     }
     
   } catch(std::exception &x) {
     // catch Mesh exception return code 
     if (x.what()) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     x.what(), ESMC_CONTEXT, &localrc);
       return localrc;
     } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     "UNKNOWN", ESMC_CONTEXT, &localrc);
       return localrc;
     }
   }catch(int localrc){
     // catch standard ESMF return code
     ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc);
     return localrc;
   } catch(...){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught unknown exception", ESMC_CONTEXT, &localrc);
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
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
         ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
     }


     if (level < MeshCXXLevel_Finished) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- Mesh must be finished before you can free it's memory. ",
           ESMC_CONTEXT, &localrc)) throw localrc;
     }
     
     //cerr << "MeshCXX::freeMemory(): meshPointer = " << meshPointer;
     //cerr << ".  Setting meshFreed to 1." << endl;
     delete meshPointer;
     meshFreed=1;
          
   } catch(std::exception &x) {
     // catch Mesh exception return code 
     if (x.what()) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     x.what(), ESMC_CONTEXT, &localrc);
       return localrc;
     } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     "UNKNOWN", ESMC_CONTEXT, &localrc);
       return localrc;
     }
   }catch(int localrc){
     // catch standard ESMF return code
     ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc);
     return localrc;
   } catch(...){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught unknown exception", ESMC_CONTEXT, &localrc);
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
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
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
				     x.what(), ESMC_CONTEXT, &localrc);
       return localrc;
     } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     "UNKNOWN",  ESMC_CONTEXT, &localrc);
       return localrc;
     }
   }catch(int localrc){
     // catch standard ESMF return code
     ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, 
        ESMC_CONTEXT, &localrc);
     return localrc;
   } catch(...){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught unknown exception", ESMC_CONTEXT, &localrc);
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
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
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
				     x.what(), ESMC_CONTEXT, &localrc);
       return localrc;
     } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				     "UNKNOWN", ESMC_CONTEXT, &localrc);
       return localrc;
     }
   }catch(int localrc){
     // catch standard ESMF return code
     ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &localrc);
     return localrc;
   } catch(...){
     ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, &localrc);
     return localrc;
   }
   
   // Return SUCCESS
   return ESMF_SUCCESS;

} //MeshVTKBody

int MeshCXX::isMeshFreed(){
  //cerr << "MeshCXX::isMeshFreed(): meshFreed = " << meshFreed << endl;
  return meshFreed;
}


#if 0
int MeshCXX::numLocalNodes(){
   return numLNodes;
}


int MeshCXX::numLocalElements(){
   return numLElements;
}


int MeshCXX::numOwnedNodes(){
   return numOwnedNodes;
}


int MeshCXX::numOwnedElements(){
   return numOwnedElements;
}
#endif


int MeshCXX::meshWrite(const char* fileName){

  int localrc;
  //Initialize localrc; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  try{
     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
     }


     if (level < MeshCXXLevel_Finished) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- Mesh must be finished before you can write it out.", ESMC_CONTEXT,
            &localrc)) throw localrc;
     }


    WriteMesh(*meshPointer, fileName);
  } catch(std::exception &x) {
    // catch Mesh exception return code 
    if (x.what()) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
				    x.what(), ESMC_CONTEXT, &localrc);
      return localrc;
    } else {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
         "UNKNOWN", ESMC_CONTEXT, &localrc);
       return localrc;
    }
  }catch(int localrc){
     // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &localrc);
    return localrc;
  } catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught unknown exception", ESMC_CONTEXT, &localrc);
    return localrc;
  }
   
   // Return SUCCESS
   return ESMF_SUCCESS;

} //meshWrite

} // namespace ESMCI
