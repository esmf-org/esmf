// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
#define ESMC_FILENAME "./src/Infrastructure/Mesh/src/ESMCI_MeshCXX.C"
//==============================================================================
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_VM.h"

#include "Mesh/include/Legacy/ESMCI_Exception.h"
#include "Mesh/include/ESMCI_MeshCXX.h"
#include "Mesh/include/Legacy/ESMCI_MeshRead.h"
#include "Mesh/include/Legacy/ESMCI_MeshVTK.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"
#include "Mesh/include/Legacy/ESMCI_MeshUtils.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/ESMCI_Mesh_Glue.h"

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



  MeshCXX* MeshCXX::create( int pdim, int sdim,
                            ESMC_CoordSys_Flag coordSys, int *rc){
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

    // Create meshCXX
    meshCXXp = new MeshCXX();

     // Set dimensions
    meshCXXp->spatialDim=sdim;
    meshCXXp->parametricDim=pdim;

    // Set CoordSys
    meshCXXp->coordSys=coordSys;

    // Create internal mesh
    meshp = new Mesh();

    // Set internal mesh in meshCXX
    (meshCXXp)->meshPointer = meshp;

    // Calculate internal Mesh dimensions
    // Get cartesian dimension
    int cart_sdim;
    int localrc;
    localrc=ESMCI_CoordSys_CalcCartDim(coordSys, sdim, &cart_sdim);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // set internal mesh dimensions
    (meshp)->set_parametric_dimension(pdim);
    (meshp)->set_spatial_dimension(cart_sdim);
    (meshp)->orig_spatial_dim=sdim;

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
   int parametricDim;
   int spatialDim;
   ESMC_CoordSys_Flag coordSys;

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
                                        maskFlag, varname,
                                        &parametricDim, &spatialDim,
                                        &coordSys,
                                        &localrc);
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

   // Set dimensions
   meshCXXp->spatialDim=spatialDim;
   meshCXXp->parametricDim=parametricDim;

   // Set CoordSys
   meshCXXp->coordSys=coordSys;

   // Set the number of nodes and elements
   meshCXXp->numLElements = calc_num_local_elems(*meshp);

   meshCXXp->numLNodes = meshp->num_nodes();

   meshCXXp->meshFreed = 0;

   // Mark Mesh as finshed
   meshCXXp->level=MeshCXXLevel_Finished;

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

     // Only do owned
     if (!GetAttr(elem).is_locally_owned()) continue;

     // Don't do split elements
     if (meshp->is_split && (elem.get_id() > meshp->max_non_split_id)) continue;

     num_owned_elems++;
   }
   meshCXXp->numOwnedElements=num_owned_elems;


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
#define ESMC_METHOD "MeshCXX::getConnectivity()"

  void MeshCXX::getConnectivity(double *connCoord, int *nodesPerElem, int *rc) {
    int localrc;
    try {
      ESMCI_getconnectivity(&meshPointer, connCoord, nodesPerElem,
                                &spatialDim, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) throw localrc;  // bail out with exception

    } catch(std::exception &x) {
      // catch Mesh exception return code
      if (x.what()) {
        localrc = ESMC_RC_INTNRL_BAD;
            ESMC_LogDefault.MsgFoundError(localrc,
                                      x.what(), ESMC_CONTEXT, rc);
            if (rc!=NULL) *rc = localrc;
            return;
      } else {
        localrc = ESMC_RC_INTNRL_BAD;
            ESMC_LogDefault.MsgFoundError(localrc,
                                      "UNKNOWN", ESMC_CONTEXT, rc);
            if (rc!=NULL) *rc = localrc;
            return;
      }
    } catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      if (rc!=NULL) *rc = localrc;
      return;
    } catch(...){
      localrc = ESMC_RC_INTNRL_BAD;
      ESMC_LogDefault.MsgFoundError(localrc,
        "- Caught unknown exception", ESMC_CONTEXT, rc);
      if (rc!=NULL) *rc = localrc;
      return;
    }

    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "MeshCXX::getLocalElemCoords()"
  void MeshCXX::getLocalElemCoords(double *ecoords, int *num_elems, int *num_dims, int *rc) {
    int localrc;
    try {
      ESMCI_getlocalelemcoords(&meshPointer, ecoords, &spatialDim, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) throw localrc;  // bail out with exception

    } catch(std::exception &x) {
      // catch Mesh exception return code
      if (x.what()) {
        localrc = ESMC_RC_INTNRL_BAD;
            ESMC_LogDefault.MsgFoundError(localrc,
                                      x.what(), ESMC_CONTEXT, rc);
            if (rc!=NULL) *rc = localrc;
            return;
      } else {
        localrc = ESMC_RC_INTNRL_BAD;
            ESMC_LogDefault.MsgFoundError(localrc,
                                      "UNKNOWN", ESMC_CONTEXT, rc);
            if (rc!=NULL) *rc = localrc;
            return;
      }
    } catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      if (rc!=NULL) *rc = localrc;
      return;
    } catch(...){
      localrc = ESMC_RC_INTNRL_BAD;
      ESMC_LogDefault.MsgFoundError(localrc,
        "- Caught unknown exception", ESMC_CONTEXT, rc);
      if (rc!=NULL) *rc = localrc;
      return;
    }

    *num_elems = this->numOwnedElements;
    *num_dims = spatialDim;

    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;
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
              " - there was a problem with triangulation (e.g. repeated points, clockwise poly, etc.)",
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

int MeshCXX::addElements(int num_elems, int *elemId,
                         int *elemType, int *elemConn,
                         int *elemMask, double *elemArea,
                         double *elemCoords){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MeshCXX::addElements()"

  //Initialize localrc; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;

  // Set flags for presence of elemArea and elemCoords arguments
  int areaPresent = (elemArea != NULL);
  int coordsPresent = (elemCoords != NULL);

  // Get parametric dimension
  int parametric_dim=parametricDim;
  int spatial_dim=spatialDim;

  // Check size of connectivity list
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
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                     "- element connectivity list doesn't contain the right number of entries ",
                                     ESMC_CONTEXT, &localrc)) throw localrc;
  }
#else
  int num_elemConn=expected_conn_size;
#endif

  int regridConserve = 1; // Set this to 1 to force the frac field to be
                          // added (required for conservative regridding.

  InterArray<int> *elemMaskII = NULL;
  if (elemMask) {
    int extent[1];
    extent[0] = num_elems;
    elemMaskII = new InterArray<int>(elemMask, 1, extent);
  }
  ESMCI_meshaddelements(&meshPointer, &num_elems, elemId, elemType, elemMaskII,
                        &areaPresent, elemArea, &coordsPresent, elemCoords,
                        &num_elemConn, elemConn, &regridConserve,
                        &coordSys, &spatial_dim, &localrc);

  // Set the local number of nodes and elements
  numLNodes = meshPointer->num_nodes();
  numLElements = calc_num_local_elems(*meshPointer);

 // Calc and set the number of owned nodes
  int num_owned_nodes=0;
  // NEW MESH iterator because the one above has probably been trashed by
  // changes in the commit, etc.
  Mesh::iterator ni2 = meshPointer->node_begin(), ne2 = meshPointer->node_end();
  for (ni2; ni2 != ne2; ++ni2) {
    MeshObj &node = *ni2;

    if (!GetAttr(node).is_locally_owned()) continue;

    num_owned_nodes++;
  }
  numOwnedNodes=num_owned_nodes;

  // Calc and set the number of owned elements
  numOwnedElements=calc_num_owned_elems(*meshPointer);

  // Mark Mesh as finshed
  level=MeshCXXLevel_Finished;

  return localrc;
}
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

     // Call into Mesh glue to add nodes
     ESMCI_meshaddnodes(&meshPointer, &numNodes, nodeId,
                        nodeCoord, nodeOwner, (InterArray<int> *)NULL,
                        &coordSys, &spatialDim,
                        &localrc);
       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
              ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception



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

     // Set some stuff needed by MeshCXX
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
