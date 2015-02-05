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
#include "Mesh/include/ESMCI_MathUtil.h"

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



  // Calculate the number of owned elements disregarding split elements
  int calc_num_owned_elems(Mesh &mesh) {
    
    int num=0;
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      
      MeshObj &elem = *ei;
      
      // Only do owned elements      
      if (!GetAttr(elem).is_locally_owned()) continue;
      
      // Don't do split elements
      if (mesh.is_split && elem.get_id() > mesh.max_non_split_id) continue;   
      
      num++;
    }
    
    return num;
  }



#undef  ESMC_METHOD
#define ESMC_METHOD "MeshCXX::createFromFile()"
MeshCXX* MeshCXX::createFromFile(const char *filename, int fileTypeFlag, 
				 int *convertToDual,
				 int *addUserArea,
				 const char *meshname,
				 int *maskFlag,
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
					maskFlag, varname, &localrc);
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

#define NGONS
#ifdef NGONS

// Get the element topology

// RIGHT NOW EXACT COPY OF VERSION IN ESMCI_Mesh_F.C

const MeshObjTopo *ElemType2TopoCXX(int pdim, int sdim, int etype) {

  if (pdim==2) {
    if (sdim==2) {
      if (etype==3) {
        return GetTopo("TRI3");
      } else if (etype==4) {
        return GetTopo("QUAD");
      } else {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
            "- for a mesh with parametric dimension 2 topo types must be either triangles or quadrilaterals ",
                                         ESMC_CONTEXT, &localrc)) throw localrc;
      }
    } else if (sdim==3) {
      if (etype==3) {
        return GetTopo("SHELL3");
      } else if (etype==4) {
        return GetTopo("SHELL");
      } else {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
            "- for a mesh with parametric dimension 2 topo types must be either triangles or quadrilaterals ",
                                         ESMC_CONTEXT, &localrc)) throw localrc;
      }
    }
  } else if (pdim==3) {
    return Vtk2Topo(sdim, etype);
  }
}


// Get the number of nodes from the element type
// Get the element topology

// RIGHT NOW EXACT COPY OF VERSION IN ESMCI_Mesh_F.C

int ElemType2NumNodesCXX(int pdim, int sdim, int etype) {
  if (pdim==2) {
    return etype;
  } else if (pdim==3) {
    if (etype==10) return 4;
    else if (etype==12) return 8;
    else {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
       "- for a mesh with parametric dimension 3 element types must be either tetrahedrons or hexahedrons",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
  }
}

// triangulate > 4 sided
// sdim = spatial dim
// num_p = number of points in poly
// p     = poly coords size=num_p*sdim
// td    = temporary buffer size=num_p*sdim
// ti    = temporary integer buffer size = num_p
// tri_ind = output array  size = 3*(nump-2)
// tri_frac = fraction each triangle is of whole poly size=(num_p-2)

// RIGHT NOW EXACT COPY OF VERSION IN ESMCI_Mesh_F.C

void triangulateCXX(int sdim, int num_p, double *p, double *td, int *ti, int *tri_ind, 
                 double *tri_frac) {
          int localrc;
          

          // Call into triagulation routines
          int ret;
          if (sdim==2) {
            ret=triangulate_poly<GEOM_CART2D>(num_p, p, td,
                                              ti, tri_ind);
          } else if (sdim==3) {
            ret=triangulate_poly<GEOM_SPH2D3D>(num_p, p, td, 
                                               ti, tri_ind);
          } else {
            if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                          " - triangulate can't be used for polygons with spatial dimension not equal to 2 or 3",
                                              ESMC_CONTEXT, &localrc)) throw localrc;
          }

          // Check return code
          if (ret != ESMCI_TP_SUCCESS) {
            if (ret == ESMCI_TP_DEGENERATE_POLY) {
              if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                   " - can't triangulate a polygon with less than 3 sides", 
                                                ESMC_CONTEXT, &localrc)) throw localrc;
            } else if (ret == ESMCI_TP_CLOCKWISE_POLY) {
              if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                   " - clockwise polygons not supported in triangulation routine",
                                                ESMC_CONTEXT, &localrc)) throw localrc;
            } else {
              if (ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                                " - unknown error in triangulation", ESMC_CONTEXT, &localrc)) throw localrc;
            }
          }


          // Calculate triangule areas
          double tot_area=0.0;
          int ti_pos=0;
          for (int i=0; i<num_p-2; i++) {
            // Copy triangle coordinates into td
            int td_pos=0;
            for (int j=0; j<3; j++) {
              double *pnt=p+sdim*tri_ind[ti_pos+j];
              for (int k=0; k<sdim; k++) {
                td[td_pos]=pnt[k];
                td_pos++;
              }
            }

            // compute area of triangle
            double tri_area;
            if (sdim == 2) {
              tri_area = area_of_flat_2D_polygon(3, td);
            } else if (sdim == 3) {
              tri_area = great_circle_area(3, td);
            } // Other sdim caught above

            // Save areas to use for computing fractions
            tri_frac[i]=tri_area;
            
            // compute total
            tot_area += tri_area;

            // Advance to next triangle
            ti_pos +=3;
          }

          // Calculate triangle fractions
          for (int i=0; i<num_p-2; i++) {
            if (tot_area >0.0) tri_frac[i]=tri_frac[i]/tot_area;
            else tri_frac[i]=0.0;
          }

    return;
}




// TODO: most of this routine is duplicated in ESMCI_Mesh_F.C - should be merged  
int MeshCXX::addElements(int num_elems, int *elemId, 
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
    for (int i=0; i< num_elems; i++) {
      if (parametric_dim==2) {
      // DONT DO THE CHECK BECAUSE WE NOW SUPPORT 
      // ANY NUMBER OF CORNERS WITH PDIM=2
#if 0
        if ((elemType[i] != 5) && (elemType[i] != 9)) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 2 element types must be "
           "either triangles or quadrilaterals ", ESMC_CONTEXT, &localrc))
           throw localrc;
        }
#endif
      } else if (parametric_dim==3) {
        if ((elemType[i] != 10) && (elemType[i] != 12)) {
	  if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
           "- for a mesh with parametric dimension 3 element types must be "
           "either tetrahedron or hexahedron ", ESMC_CONTEXT, &localrc))
           throw localrc;
        }

      }
    }




    //// Check size of connectivity list
    int expected_conn_size=0;
    if (parametric_dim==2) {
      for (int i=0; i< num_elems; i++) {
        expected_conn_size += elemType[i];
      }
    } else if (parametric_dim==3) {
      for (int i=0; i< num_elems; i++) {
        if (elemType[i]==10) expected_conn_size += 4;   
        else if (elemType[i]==12) expected_conn_size += 8;   
      }
    }

    // num_elemConn doesn't come in, so can't check, instead assign. 
#if 0      
    if (expected_conn_size != num_elemConn) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- element connectivity list doesn't contain the right number of entries ",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
#else
    int num_elemConn=expected_conn_size;   
#endif


    // Count the number of extra elements we need for splitting
    int num_extra_elem=0;
    int max_num_conn=0;
    if (parametric_dim==2) {
      for (int e = 0; e < num_elems; ++e) {
        if (elemType[e] >4) {
          num_extra_elem += (elemType[e]-3); // Original elem + # sides-2
        }

        if (elemType[e] > max_num_conn) max_num_conn=elemType[e];
      }

      int tot_num_extra_elem=0;
      MPI_Allreduce(&num_extra_elem,&tot_num_extra_elem,1,MPI_INT,MPI_SUM,Par::Comm());

      // If there's num_extra_elem than it's a split mesh
      if (tot_num_extra_elem>0) {
        mesh.is_split=true;
      } else {
        mesh.is_split=false;
      }
    } else {
      mesh.is_split=false;
    }

    // Compute the extra element ranges
    int beg_extra_ids=0;
    if (mesh.is_split) {      
      // get maximum local elem id      
      int max_id=0;
      for (int e = 0; e < num_elems; ++e) {
        if (elemId[e] > max_id) {
          max_id=elemId[e];
        }
      }

      // Calc global max id
      int global_max_id=0;
      MPI_Allreduce(&max_id,&global_max_id,1,MPI_INT,MPI_MAX,Par::Comm());
      
      // Set maximum of non-split ids
      mesh.max_non_split_id=global_max_id;

      // Calc our range of extra elem ids
      beg_extra_ids=0;
      MPI_Scan(&num_extra_elem,&beg_extra_ids,1,MPI_INT,MPI_SUM,Par::Comm());

      // Remove this processor's number from the sum to get the beginning
      beg_extra_ids=beg_extra_ids-num_extra_elem;

      // Start 1 up from max
      beg_extra_ids=beg_extra_ids+global_max_id+1;

      // printf("%d# beg_extra_ids=%d end=%d\n",Par::Rank(),beg_extra_ids,beg_extra_ids+num_extra_elem-1);
    }

    // Get number of nodes
    int num_nodes = mesh.num_nodes();

    // Allocate array to make sure that there are no local nodes without a home
    std::vector<int> node_used;
    node_used.resize(num_nodes, 0);

    // Error check elemConn array
    int c = 0;
    for (int e = 0; e < num_elems; ++e) {
      int nnodes = ElemType2NumNodesCXX(mesh.parametric_dim(),
                                     mesh.spatial_dim(), 
                                     elemType[e]);

      for (int n = 0; n < nnodes; ++n) {

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

    // Generate connectivity list with split elements
    // TODO: MAYBE EVENTUALLY PUT EXTRA SPLIT ONES AT END
    int num_elems_wsplit=0;
    int *elemConn_wsplit=NULL;
    int *elemType_wsplit=NULL;
    int *elemId_wsplit=NULL;
    double *elemArea_wsplit=NULL;
    int *elemMask_wsplit=NULL;


 /* XMRKX */

    if (mesh.is_split) {
      // New number of elements
      num_elems_wsplit=num_elems+num_extra_elem;

      // Allocate arrays to hold split lists
      elemConn_wsplit=new int[num_elemConn+3*num_extra_elem];
      elemType_wsplit=new int[num_elems_wsplit];
      elemId_wsplit=new int[num_elems_wsplit];
      if (elemArea != NULL) elemArea_wsplit=new double[num_elems_wsplit];
      if (elemMask != NULL) elemMask_wsplit=new int[num_elems_wsplit];

      // Allocate some temporary variables for splitting
      double *polyCoords=new double[3*max_num_conn];
      double *polyDblBuf=new double[3*max_num_conn];
      int    *polyIntBuf=new int[max_num_conn];
      int    *triInd=new int[3*(max_num_conn-2)];
      double *triFrac=new double[max_num_conn-2];

      // new id counter
      int curr_extra_id=beg_extra_ids;

      // Get some useful information
      int sdim = mesh.spatial_dim(); 

      // Loop through elems generating split elems if necessary
      int conn_pos = 0;
      int split_conn_pos = 0;
      int split_elem_pos = 0;
      for (int e = 0; e < num_elems; ++e) {
        
        // More than 4 side, split
        if (elemType[e]>4) {

          // Get coordinates
          int crd_pos=0;
          for (int i=0; i<elemType[e]; i++) {
            MeshObj *node=all_nodes[elemConn[conn_pos+i]-1];
            double *crd=mesh.node_coord->data(*node);
            
            for (int j=0; j<sdim; j++) {
              polyCoords[crd_pos]=crd[j];
              crd_pos++;
            }

            // printf("id=%d coord=%f %f \n",elemId[e],polyCoords[crd_pos-2],polyCoords[crd_pos-1]);
          }

          // Triangulate polygon
          triangulateCXX(sdim, elemType[e], polyCoords, polyDblBuf, polyIntBuf, 
                      triInd, triFrac); 
          

          // Create split element list
          int tI_pos=0;
          for (int i=0; i<elemType[e]-2; i++) {
            // First id is same, others are from new ids
            if (i==0) {
              elemId_wsplit[split_elem_pos]=elemId[e];
              mesh.split_id_to_frac[elemId[e]]=triFrac[i];
            } else {
              elemId_wsplit[split_elem_pos]=curr_extra_id;
              mesh.split_to_orig_id[curr_extra_id]=elemId[e]; // Store map of split to original id
              mesh.split_id_to_frac[curr_extra_id]=triFrac[i];
              curr_extra_id++;
            }

            // Type is triangle
            elemType_wsplit[split_elem_pos]=3; 

            // Set area to fraction of original area
            if (elemArea != NULL) elemArea_wsplit[split_elem_pos]=elemArea[e]*triFrac[i];

            // Set mask (if it exists)
            if (elemMask != NULL) elemMask_wsplit[split_elem_pos]=elemMask[e];

            // Next split element
            split_elem_pos++;

            // Set triangle corners based on triInd
            elemConn_wsplit[split_conn_pos]=elemConn[conn_pos+triInd[tI_pos]];
            elemConn_wsplit[split_conn_pos+1]=elemConn[conn_pos+triInd[tI_pos+1]];
            elemConn_wsplit[split_conn_pos+2]=elemConn[conn_pos+triInd[tI_pos+2]];

            // printf("%d eid=%d seid=%d %d %d %d %f\n",i,elemId[e],elemId_wsplit[split_elem_pos-1],elemConn_wsplit[split_conn_pos],elemConn_wsplit[split_conn_pos+1],elemConn_wsplit[split_conn_pos+2],triFrac[i]);
            split_conn_pos +=3;
            tI_pos +=3;

          }

          // Advance to next elemConn position 
          conn_pos +=elemType[e];

        } else { // just copy
          elemId_wsplit[split_elem_pos]=elemId[e];
          elemType_wsplit[split_elem_pos]=elemType[e];
          if (elemArea != NULL) elemArea_wsplit[split_elem_pos]=elemArea[e];
          if (elemMask != NULL) elemMask_wsplit[split_elem_pos]=elemMask[e];
          split_elem_pos++;
          for (int i=0; i<elemType[e]; i++) {
            elemConn_wsplit[split_conn_pos]=elemConn[conn_pos];
            split_conn_pos++;
            conn_pos++;
          }
        }
      }
      
      
      // Allocate some temporary variables for splitting
      delete [] polyCoords;
      delete [] polyDblBuf;
      delete [] polyIntBuf;
      delete [] triInd;
      delete [] triFrac;

      // Use the new split list for the connection lists below
      num_elems=num_elems_wsplit;
      elemConn=elemConn_wsplit;
      elemType=elemType_wsplit;
      elemId=elemId_wsplit;
      if (elemArea != NULL) elemArea=elemArea_wsplit;
      if (elemMask != NULL) elemMask=elemMask_wsplit;
    }   

    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;
    for (int e = 0; e < num_elems; ++e) {

      // Get/deduce the element topology
      const MeshObjTopo *topo = ElemType2TopoCXX(mesh.parametric_dim(),
                                                 mesh.spatial_dim(), 
                                                 elemType[e]);

    int nnodes = topo->num_nodes;

    std::vector<MeshObj*> nconnect(nnodes, static_cast<MeshObj*>(0));

      // The object
      long eid = elemId[e];
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, eid, e);

      for (int n = 0; n < nnodes; ++n) {
      
        // Get 0-based node index
        int node_index=elemConn[cur_conn]-1;

        // Setup connectivity list
        nconnect[n] = all_nodes[node_index];

        // Advance to next
        cur_conn++;
      }

      mesh.add_element(elem, nconnect, topo->number, topo);

    } // for e


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

   // Set the local number of nodes and elements
   numLNodes = mesh.num_nodes();
   numLElements = calc_num_local_elems(mesh);

    // Calc and set the number of owned nodes
    int num_owned_nodes=0;
    // NEW MESH iterator because the one above has probably been trashed by
    // changes in the commit, etc. 
    Mesh::iterator ni2 = mesh.node_begin(), ne2 = mesh.node_end();
    for (ni2; ni2 != ne2; ++ni2) {
      MeshObj &node = *ni2;
   
      if (!GetAttr(node).is_locally_owned()) continue;
      
      num_owned_nodes++;
    }
    numOwnedNodes=num_owned_nodes;

    // Calc and set the number of owned elements
    numOwnedElements=calc_num_owned_elems(mesh);

    // Mark Mesh as finshed
    level=MeshCXXLevel_Finished;

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

#else
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
#endif

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

    // Need this for split elements, put on Mesh for now
    mesh.node_coord=node_coord;

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
