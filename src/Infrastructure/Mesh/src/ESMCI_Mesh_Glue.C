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
//==============================================================================

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
#include "ESMCI_CoordSys.h"
#include "ESMCI_FindPnts.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/ESMCI_Phedra.h"
#include "Mesh/include/ESMCI_XGridUtil.h"
#include "Mesh/include/ESMCI_MeshMerge.h"
#include "Mesh/include/ESMCI_MeshRedist.h"
#include "Mesh/include/ESMCI_MeshDual.h"
#include "Mesh/include/ESMCI_Mesh_Glue.h"
//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;


void ESMCI_meshcreate(Mesh **meshpp,
                      int *pdim, int *sdim,
                      ESMC_CoordSys_Flag *coordSys, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcreate()"


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

    // Get cartesian dimension
    int cart_sdim;
    int localrc;
    localrc=ESMCI_CoordSys_CalcCartDim(*coordSys, *sdim, &cart_sdim);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // Set dimensions
    (*meshpp)->set_parametric_dimension(*pdim);
    (*meshpp)->set_spatial_dimension(cart_sdim);

    // Save original dimension
    (*meshpp)->orig_spatial_dim=*sdim;

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

void ESMCI_meshaddnodes(Mesh **meshpp, int *num_nodes, int *nodeId,
                                           double *nodeCoord, int *nodeOwner, InterArray<int> *nodeMaskII,
                                           ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                                           int *rc)
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshaddnodes()"
{
   try {
    Mesh *meshp = *meshpp;
    ThrowRequire(meshp);
    Mesh &mesh = *meshp;

    ESMC_CoordSys_Flag coordSys=*_coordSys;
    int sdim = mesh.spatial_dim(); // spatial dim of mesh (after conversion to Cartesian)
    int orig_sdim = *_orig_sdim;   // original sdim (before conversion to Cartesian)


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
    IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", sdim);

    // Need this for split elements, put on Mesh for now
    mesh.node_coord=node_coord;

    // If not cartesian then keep original coordinate field
    IOField<NodalField> *node_orig_coord;
    if (coordSys != ESMC_COORDSYS_CART) {
      node_orig_coord = mesh.RegisterNodalField(mesh, "orig_coordinates", orig_sdim);
    }

    // Handle node masking
    IOField<NodalField> *node_mask_val;
    IOField<NodalField> *node_mask;

    bool has_node_mask=false;
    if (present(nodeMaskII)) { // if masks exist
    // Error checking
    if ((nodeMaskII)->dimCount !=1) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- nodeMask array must be 1D ", ESMC_CONTEXT,  &localrc)) throw localrc;
    }

    if ((nodeMaskII)->extent[0] != *num_nodes) {
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


#if 0
    // Loop and add coords and mask
    if (has_node_mask) {
      int *maskArray=(nodeMaskII)->array;
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
#endif

    // Loop and add coords
    if (coordSys == ESMC_COORDSYS_CART) {
      Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();

      int nc=0;
      for (; ni != ne; ++ni) {
        MeshObj &node = *ni;

        double *coord = node_coord->data(node);
        for (UInt c = 0; c < sdim; ++c)
          coord[c] = nodeCoord[nc+c];
        nc += sdim;
      }
    } else {
      Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();

      int nc=0;
      for (; ni != ne; ++ni) {
        MeshObj &node = *ni;

        // Save original coordinates
        double *orig_coord = node_orig_coord->data(node);
        for (UInt c = 0; c<orig_sdim; ++c)
          orig_coord[c] = nodeCoord[nc+c];
        nc += orig_sdim;

        // Convert and save Cartesian coordinates
        double *coord = node_coord->data(node);
        ESMCI_CoordSys_ConvertToCart(coordSys, orig_sdim,
                                     orig_coord, coord);
      }
    }


    // Loop and add mask
    if (has_node_mask) {
      int *maskArray=(nodeMaskII)->array;
      Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();

      int nm=0;
      for (; ni != ne; ++ni) {
        MeshObj &node = *ni;

        // Mask
        double *mask = node_mask_val->data(node);
        *mask=maskArray[nm];
        nm++;
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

void ESMCI_meshwrite(Mesh **meshpp, char *fname, int *rc,
    ESMCI_FortranStrLenArg nlen) {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshwrite()"

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

// Get the element topology
const MeshObjTopo *ElemType2Topo(int pdim, int sdim, int etype) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ElemType2Topo()"

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
static int ElemType2NumNodes(int pdim, int sdim, int etype) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ElemType2NumNodes()"

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
static void triangulate(int sdim, int num_p, double *p, double *td, int *ti, int *tri_ind,
                 double *tri_frac) {
#undef  ESMC_METHOD
#define ESMC_METHOD "triangulate()"

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



// triangulate > 4 sided
// sdim = spatial dim
// num_p = number of points in poly
// p     = poly coords size=num_p*sdim
// oeid  = id of original element for debug output
// td    = temporary buffer size=num_p*sdim
// ti    = temporary integer buffer size = num_p
// tri_ind = output array  size = 3*(nump-2)
// tri_area = area of each sub-triangle is of whole poly size=(num_p-2)
static void triangulate_warea(int sdim, int num_p, double *p, int oeid,
                              double *td, int *ti, int *tri_ind,
                              double *tri_area) {
#undef  ESMC_METHOD
#define ESMC_METHOD "triangulate_warea()"

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
              char msg[1024];
              sprintf(msg," - there was a problem (e.g. repeated points, clockwise poly, etc.) with the triangulation of the element with id=%d ",oeid);
              if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP, msg,
                                              ESMC_CONTEXT, &localrc)) throw localrc;
            } else {
              if (ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                                " - unknown error in triangulation", ESMC_CONTEXT, &localrc)) throw localrc;
            }
          }


          // Calculate triangule areas
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
            double area;
            if (sdim == 2) {
              area = area_of_flat_2D_polygon(3, td);
            } else if (sdim == 3) {
              area = great_circle_area(3, td);
            } // Other sdim caught above

            // Save areas to use for computing fractions
            tri_area[i]=area;

            // Advance to next triangle
            ti_pos +=3;
          }

    return;
}



void ESMCI_meshaddelements(Mesh **meshpp,
                                              int *_num_elems, int *elemId, int *elemType, InterArray<int> *_elemMaskII ,
                                              int *_areaPresent, double *elemArea,
                                              int *_elemCoordsPresent, double *elemCoords,
                                              int *_num_elemConn, int *elemConn, int *regridConserve,
                                              ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                                              int *rc)
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshaddelements()"

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

    // local rc code
    int localrc;

    // Set some convient variables
     Mesh *meshp = *meshpp;
    ThrowRequire(meshp);

    Mesh &mesh = *meshp;

    int num_elems=*_num_elems;

    int num_elemConn=*_num_elemConn;

    InterArray<int> *elemMaskII=_elemMaskII;

    int areaPresent=*_areaPresent;

    int elemCoordsPresent=*_elemCoordsPresent;

    ESMC_CoordSys_Flag coordSys=*_coordSys;
    int orig_sdim = *_orig_sdim;   // original sdim (before conversion to Cartesian)

    // Get parametric dimension
    int parametric_dim=mesh.parametric_dim();

    // Error check input
    //// Check element type
    ////(DON'T NEED TO CHECK PDIM==2, BECAUSE WE NOW SUPPORT
    //// ANY NUMBER OF CORNERS WITH PDIM=2)
    if (parametric_dim==3) {
      for (int i=0; i< num_elems; i++) {
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

    if (expected_conn_size != num_elemConn) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- element connectivity list doesn't contain the right number of entries ",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }


    // Error check size of elements
    if (parametric_dim==2) {
      int conn_pos=0;
      for (int e = 0; e < num_elems; ++e) {

        // Loop here through each set of connection looking for polybreaks to
        // figure out the size of each sub-elem
        int elem_size=0;
        for (int i=0; i<elemType[e]; i++) {

          // Advance size of element, or start a new one
          if (elemConn[conn_pos] != MESH_POLYBREAK_IND) {
            elem_size++;
          } else {
            // Complain if element is less than a triangle
            if (elem_size < 3) {
              int localrc;
              if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
               " element connectivity list contains elems with less than 3 sides.",
                                               ESMC_CONTEXT, &localrc)) throw localrc;
            }
            elem_size=0;
          }

          // next connection
          conn_pos++;
        }

        // Complain if element is less than a triangle
        if (elem_size < 3) {
          int localrc;
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
             " element connectivity list contains elems with less than 3 sides.",
                                           ESMC_CONTEXT, &localrc)) throw localrc;
        }
      }
     }


    // Variable indicating if any of the elements on this PET are split
    bool is_split_local=false;

    // Count the number of extra elements we need for splitting
    int num_extra_elem=0;
    int max_num_conn=0;
    int max_num_elemtris=0;
     if (parametric_dim==2) {
      int conn_pos=0;
      for (int e = 0; e < num_elems; ++e) {

        // Only count split elements
        if (elemType[e] > 4) {

          // Loop here through each set of connection looking for polybreaks to
          // figure out the size of each sub-elem
          int subelem_size=0;
          int num_elemtris=0;
          for (int i=0; i<elemType[e]; i++) {

            // Advance size of element, or start a new one
            if (elemConn[conn_pos] != MESH_POLYBREAK_IND) {
              subelem_size++;
            } else {
              // record this elem
              num_extra_elem += (subelem_size-2); // num tri = # sides-2
              num_elemtris += (subelem_size-2); // num tri = # sides-2
              if (subelem_size > max_num_conn) max_num_conn=subelem_size;
              subelem_size=0;
            }

            // next connection
            conn_pos++;
          }

          // record this elem
          num_extra_elem += (subelem_size-3); // num tri = # sides-2 - 1 (for orig elem)
          num_elemtris += (subelem_size-2); // num tri = # sides-2 (count orig elem)
          if (num_elemtris > max_num_elemtris) max_num_elemtris=num_elemtris;
          if (subelem_size > max_num_conn) max_num_conn=subelem_size;
        } else {
          conn_pos += elemType[e];
        }
      }

      // mark if mesh on this particular PET is split
      if (num_extra_elem > 0) is_split_local=true;

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

#if 0
    // Don't currently support split elements with element coords
    if (mesh.is_split && elemCoordsPresent) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- ESMF doesn't currently support both element coords and polygons with >4 sides in the same Mesh.",
            ESMC_CONTEXT, &localrc)) throw localrc;
    }
#endif

     // Get number of nodes
    int num_nodes = mesh.num_nodes();

    // Allocate array to make sure that there are no local nodes without a home
    std::vector<int> node_used;
    node_used.resize(num_nodes, 0);

    // Error check elemConn array
    int c = 0;
    for (int e = 0; e < num_elems; ++e) {
      int nnodes = ElemType2NumNodes(mesh.parametric_dim(),
                                     mesh.spatial_dim(),
                                     elemType[e]);

      for (int n = 0; n < nnodes; ++n) {

        // Get 0-based node index
        int node_index=elemConn[c]-1;

         // Skip the polybreak indicator
        if (node_index+1 == MESH_POLYBREAK_IND) {
          c++;
          continue;
        }

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
        c++ ;
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

    // If they exist, error check mask
    if (present(elemMaskII)) { // if masks exist
      // Error checking
      if (elemMaskII->dimCount !=1) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- elementMask array must be 1D ", ESMC_CONTEXT,  &localrc)) throw localrc;
       }

      if (elemMaskII->extent[0] != num_elems) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- elementMask array must be the same size as elementIds array ", ESMC_CONTEXT, &localrc)) throw localrc;
      }
    }


    // Before setting up connectivity store all nodes in a flat array since elemConn
    // will index into this array.
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


    // Generate connectivity list with split elements
    // TODO: MAYBE EVENTUALLY PUT EXTRA SPLIT ONES AT END
    int num_elems_wsplit=0;
    int *elemConn_wsplit=NULL;
    int *elemType_wsplit=NULL;
    int *elemId_wsplit=NULL;
    double *elemArea_wsplit=NULL;
    double *elemCoords_wsplit=NULL;
    int *elemMaskIIArray_wsplit=NULL;
    InterArray<int> *elemMaskII_wsplit=NULL;

    // If the piece of mesh on this proc is split, then generate the split elems
    if (is_split_local) {

      // New number of elements
       num_elems_wsplit=num_elems+num_extra_elem;

      // Allocate arrays to hold split lists
      elemConn_wsplit=new int[num_elemConn+3*num_extra_elem];
      elemType_wsplit=new int[num_elems_wsplit];
      elemId_wsplit=new int[num_elems_wsplit];
      if (areaPresent==1) elemArea_wsplit=new double[num_elems_wsplit];
      if (elemCoordsPresent==1) elemCoords_wsplit=new double[orig_sdim*num_elems_wsplit];

      //// Setup for split mask
      int *elemMaskIIArray=NULL;
      if (present(elemMaskII)) {

        // Get mask value array
        elemMaskIIArray=elemMaskII->array;

        int extent[1];
        elemMaskIIArray_wsplit=new int[num_elems_wsplit];

        extent[0]=num_elems_wsplit;
        elemMaskII_wsplit=new InterArray<int>(elemMaskIIArray_wsplit,1,extent);
      }

      // Allocate some temporary variables for splitting
      double *subelem_coords=new double[3*max_num_conn];
      double *subelem_dbl_buf=new double[3*max_num_conn];
      int    *subelem_int_buf=new int[max_num_conn];
      int    *subelem_tri_ind=new int[3*(max_num_conn-2)];
       double *subelem_tri_area=new double[max_num_conn-2];

      double *elemtris_area=new double[max_num_elemtris];
      int *elemtris_split_elem_pos=new int[max_num_elemtris];

      // new id counter
      int curr_extra_id=beg_extra_ids;

      // Get some useful information
      int sdim = mesh.spatial_dim();

      // Loop through elems generating split elems if necessary
      int conn_pos = 0;
      int split_conn_pos = 0;
      int split_elem_pos = 0;
      for (int e = 0; e < num_elems; ++e) {

        // More than 4 sides, split
        if (elemType[e]>4) {

          // Init for frac calc
          int    num_elemtris=0;
          double tot_elemtris_area=0.0;

          // Loop while we're still in this element
          bool first_elem=true;
          int end_of_elem=conn_pos+elemType[e];
          while (conn_pos < end_of_elem) {

            // Skip poly breaks
            if (elemConn[conn_pos] == MESH_POLYBREAK_IND) conn_pos++;

            // Find sub-elements (may be only one)
            int subelem_size=0;
            for (int i=conn_pos; i<end_of_elem; i++) {
              if (elemConn[i] == MESH_POLYBREAK_IND) break;
              subelem_size++;
            }

            //   printf("id=%d subelem_size=%d\n",elemId[e],subelem_size);


          // Get coordinates
          int crd_pos=0;
          for (int i=0; i<subelem_size; i++) {
            MeshObj *node=all_nodes[elemConn[conn_pos+i]-1];
            double *crd=mesh.node_coord->data(*node);

            for (int j=0; j<sdim; j++) {
              subelem_coords[crd_pos]=crd[j];
              crd_pos++;
             }

            // printf("id=%d coord=%f %f \n",elemId[e],subelem_coords[crd_pos-2],subelem_coords[crd_pos-1]);
          }

          // Triangulate polygon
          triangulate_warea(sdim, subelem_size, subelem_coords, elemId[e],
                            subelem_dbl_buf, subelem_int_buf,
                            subelem_tri_ind, subelem_tri_area);

          // Create split element list
          int tI_pos=0;
           for (int i=0; i<subelem_size-2; i++) {
             // First id is same, others are from new ids
             if (first_elem) {
               elemId_wsplit[split_elem_pos]=elemId[e];
               first_elem=false;
             } else {
               elemId_wsplit[split_elem_pos]=curr_extra_id;
               mesh.split_to_orig_id[curr_extra_id]=elemId[e]; // Store map of split to original id
               curr_extra_id++;
             }

            // Type is triangle
            elemType_wsplit[split_elem_pos]=3;

            // Set mask (if it exists)
             if (elemMaskIIArray !=NULL) elemMaskIIArray_wsplit[split_elem_pos]=elemMaskIIArray[e];

             // Set element coords. (if it exists)
             if (elemCoordsPresent==1) {
               double *elem_pnt=elemCoords+orig_sdim*e;
               double *elem_pnt_wsplit=elemCoords_wsplit+orig_sdim*split_elem_pos;
               for (int j=0; j<orig_sdim; j++) {
                 elem_pnt_wsplit[j]=elem_pnt[j];
               }
             }

             // Set triangle corners based on subelem_tri_ind
            elemConn_wsplit[split_conn_pos]=elemConn[conn_pos+subelem_tri_ind[tI_pos]];
            elemConn_wsplit[split_conn_pos+1]=elemConn[conn_pos+subelem_tri_ind[tI_pos+1]];
            elemConn_wsplit[split_conn_pos+2]=elemConn[conn_pos+subelem_tri_ind[tI_pos+2]];

            // Acumulate over sub-elems in one element
            elemtris_split_elem_pos[num_elemtris]=split_elem_pos;
            elemtris_area[num_elemtris]=subelem_tri_area[i];
            tot_elemtris_area += elemtris_area[num_elemtris];
            num_elemtris++;

            // printf("%d eid=%d seid=%d %d %d %d %f\n",i,elemId[e],elemId_wsplit[split_elem_pos-1],elemConn_wsplit[split_conn_pos],elemConn_wsplit[split_conn_pos+1],elemConn_wsplit[split_conn_pos+2],triFrac[i]);

            // Advance
            split_elem_pos++;
            split_conn_pos +=3;
            tI_pos +=3;
           }

           // Advance to next elemConn position
          conn_pos +=subelem_size;

           } // end of loop through sub elems

           // Loop over elem setting fracs
           if (tot_elemtris_area > 0.0) {
             for (int i=0; i<num_elemtris; i++) {
               double frac=elemtris_area[i]/tot_elemtris_area;
               int sep=elemtris_split_elem_pos[i];

               // Add frac to mesh split information
               mesh.split_id_to_frac[elemId_wsplit[sep]]=frac;

               // Set area to fraction of original area
               if (areaPresent==1) elemArea_wsplit[sep]=elemArea[e]*frac;
             }
           } else {
             for (int i=0; i<num_elemtris; i++) {
               double frac=elemtris_area[i]/tot_elemtris_area;
               int sep=elemtris_split_elem_pos[i];

               // Add frac to mesh split information
               mesh.split_id_to_frac[elemId_wsplit[sep]]=0.0;

               // Set area to fraction of original area
               if (areaPresent==1) elemArea_wsplit[sep]=0.0;
             }
           }

        } else { // just copy
          elemId_wsplit[split_elem_pos]=elemId[e];
          elemType_wsplit[split_elem_pos]=elemType[e];
          if (areaPresent==1) elemArea_wsplit[split_elem_pos]=elemArea[e];
          if (elemCoordsPresent==1) {
            double *elem_pnt=elemCoords+orig_sdim*e;
            double *elem_pnt_wsplit=elemCoords_wsplit+orig_sdim*split_elem_pos;
            for (int j=0; j<orig_sdim; j++) {
              elem_pnt_wsplit[j]=elem_pnt[j];
            }
          }
          if (elemMaskIIArray !=NULL) elemMaskIIArray_wsplit[split_elem_pos]=elemMaskIIArray[e];
          split_elem_pos++;
          for (int i=0; i<elemType[e]; i++) {
            elemConn_wsplit[split_conn_pos]=elemConn[conn_pos];
            split_conn_pos++;
            conn_pos++;
          }
        }
      }

      // Allocate some temporary variables for splitting
      delete [] subelem_coords;
      delete [] subelem_dbl_buf;
      delete [] subelem_int_buf;
      delete [] subelem_tri_ind;
      delete [] subelem_tri_area;
      delete [] elemtris_area;
      delete [] elemtris_split_elem_pos;

      // Use the new split list for the connection lists below
      num_elems=num_elems_wsplit;
      elemConn=elemConn_wsplit;
      elemType=elemType_wsplit;
      elemId=elemId_wsplit;
      if (areaPresent==1) elemArea=elemArea_wsplit;
      if (elemCoordsPresent==1) elemCoords=elemCoords_wsplit;

      if (present(elemMaskII)) {
        elemMaskII=elemMaskII_wsplit;
      }
    }


    // Now loop the elements and add them to the mesh.
    int cur_conn = 0;
    for (int e = 0; e < num_elems; ++e) {

      // Get/deduce the element topology
      const MeshObjTopo *topo = ElemType2Topo(mesh.parametric_dim(),
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

        // printf("elem id=%d type=%d node_id[%d]=%d\n",eid,elemType[e],n,nconnect[n]->get_id());


        // Advance to next
        cur_conn++;
      }

      mesh.add_element(elem, nconnect, topo->number, topo);

    } // for e


  // Register the frac field
  if (*regridConserve == ESMC_REGRID_CONSERVE_ON) {
    Context ctxt; ctxt.flip();
     MEField<> *elem_frac = mesh.RegisterField("elem_frac",
                        MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
     MEField<> *elem_frac2 = mesh.RegisterField("elem_frac2",
                        MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  }


  // Handle element masking
  bool has_elem_mask=false;
  if (present(elemMaskII)) { // if masks exist
    // ERROR CHECKED ABOVE

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
  if (areaPresent == 1) { // if areas exist

    // Context for new fields
    Context ctxt; ctxt.flip();

    // Add element mask field
    MEField<> *elem_area = mesh.RegisterField("elem_area",
                         MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

    // Record the fact that it has masks
    has_elem_area=true;
  }


   // Handle element coords
  bool has_elem_coords=false;
  if (elemCoordsPresent == 1) { // if coords exist

    // Context for new fields
    Context ctxt; ctxt.flip();

    // Add element coords field
    MEField<> *elem_coords = mesh.RegisterField("elem_coordinates",
                         MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, mesh.spatial_dim(), true);

    // If not cartesian then add original coordinates field
    if (coordSys != ESMC_COORDSYS_CART) {
      MEField<> *elem_orig_coords = mesh.RegisterField("elem_orig_coordinates",
                         MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, orig_sdim, true);
    }


    // Record the fact that it has masks
    has_elem_coords=true;
  }


  // Perhaps commit will be a separate call, but for now commit the mesh here.
  mesh.build_sym_comm_rel(MeshObj::NODE);
  mesh.Commit();

  // Set Frac values
  if (*regridConserve == ESMC_REGRID_CONSERVE_ON) {
    // Get Fields
    MEField<> *elem_frac2=mesh.GetField("elem_frac2");

    Mesh::const_iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    int count = 0;
    for (; ei != ee; ++ei) {
      const MeshObj &elem = *ei;
      double *f=elem_frac2->data(elem);
      *f=1.0;
      count ++;
    }
    //printf("number of elements: %d\n ", count);
  }


  // Set Mask values
  if (has_elem_mask) {
    // Get Fields
    MEField<> *elem_mask_val=mesh.GetField("elem_mask_val");
    MEField<> *elem_mask=mesh.GetField("elem_mask");

    // Get mask value array
    int *elemMask=elemMaskII->array;

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


  // Set coordinate values
  if (has_elem_coords) {

    if (coordSys == ESMC_COORDSYS_CART) {
      // Get Field
       MEField<> *elem_coords=mesh.GetField("elem_coordinates");

      // Get some useful information
      int sdim = mesh.spatial_dim();

      // Loop through elements setting values
      // Here we depend on the fact that data index for elements
      // is set as the position in the local array above
      Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
         MeshObj &elem = *ei;
        if (!GetAttr(elem).is_locally_owned()) continue;

        // Set coordinate value to input array
        double *coords=elem_coords->data(elem);
        int data_index = sdim*elem.get_data_index();
        for (int i = 0; i < sdim; ++i) {
          coords[i] = elemCoords[data_index+i];
          // printf("AE %d %f \n",elem.get_id(),coords[i]);

        }
        // printf("eid=%d coords=%f %f %f %f ind=%d\n",elem.get_id(),coords[0],coords[1],elemCoords[data_index+0],elemCoords[data_index+1],data_index);
      }
    } else {
      // Get Fields
      MEField<> *elem_coords=mesh.GetField("elem_coordinates");
      MEField<> *elem_orig_coords=mesh.GetField("elem_orig_coordinates");

      // Get some useful information
      int sdim = mesh.spatial_dim();

      // Loop through elements setting values
      // Here we depend on the fact that data index for elements
      // is set as the position in the local array above
      Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;
        if (!GetAttr(elem).is_locally_owned()) continue;

        // Save original coordinates
        double *orig_coords=elem_orig_coords->data(elem);
        int data_index = orig_sdim*elem.get_data_index();
        for (int i = 0; i < orig_sdim; ++i) {
          orig_coords[i] = elemCoords[data_index+i];
        }

        // Convert and save Cartesian coordinates
        double *coords = elem_coords->data(elem);
        ESMCI_CoordSys_ConvertToCart(coordSys, orig_sdim,
                                      orig_coords, coords);

        // printf("eid=%d coords=%f %f %f %f ind=%d\n",elem.get_id(),coords[0],coords[1],elemCoords[data_index+0],elemCoords[data_index+1],data_index);
      }
    }
  }


  // Get rid of extra memory for split elements
  if (is_split_local) {
    if (elemConn_wsplit != NULL) delete [] elemConn_wsplit;
    if (elemType_wsplit != NULL) delete [] elemType_wsplit;
    if (elemId_wsplit != NULL) delete [] elemId_wsplit;
    if (areaPresent==1) {
      if (elemArea_wsplit != NULL) delete [] elemArea_wsplit;
    }
    if (elemCoordsPresent==1) {
      if (elemCoords_wsplit != NULL) delete [] elemCoords_wsplit;
    }

    //// Setup for split mask
    if (present(elemMaskII)) {
      if (elemMaskIIArray_wsplit != NULL) delete [] elemMaskIIArray_wsplit;
      if (elemMaskII_wsplit != NULL) delete elemMaskII_wsplit;
    }
  }

#if 0
  // Time loops
  {
   /* XMRKX */
    double beg_tm=MPI_Wtime();

    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {

    }

    double end_tm=MPI_Wtime();

    printf("time to loop through elems=%f\n",end_tm-beg_tm);

  }
#endif


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
void ESMCI_meshvtkheader(char *filename, int *num_elem, int *num_node, int *conn_size, int *rc,
    ESMCI_FortranStrLenArg nlen) {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshvtkheader()"


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

void ESMCI_meshvtkbody(char *filename, int *nodeId, double *nodeCoord,
                    int *nodeOwner, int *elemId, int *elemType, int *elemConn, int *rc,
    ESMCI_FortranStrLenArg nlen) {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshvtkbody()"

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

void ESMCI_meshdestroy(Mesh **meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshdestroy()"

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

void ESMCI_meshfreememory(Mesh **meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshfreememory()"


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

    // Set to NULL
    *meshpp=NULL;

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
#undef  ESMC_METHOD
#define ESMC_METHOD "getNodeGIDS()"


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
 * Sort elements by the order in which they were originally declared
 * (which is stored by get_data_index)
 * Don't include split elements
 */
void getElemGIDS(Mesh &mesh, std::vector<int> &egid) {
#undef  ESMC_METHOD
#define ESMC_METHOD "getElemGIDS()"


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


void ESMCI_meshget(Mesh **meshpp, int *num_nodes, int *num_elements, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshget()"


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



void ESMCI_meshcreatenodedistgrid(Mesh **meshpp, int *ngrid, int *num_lnodes, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcreatenodedistgrid()"


  // Declare id vectors
  std::vector<int> ngids;


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

    // Get the set of owned node ids
    {
      getNodeGIDS(*meshp, ngids);
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

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}


void ESMCI_meshcreateelemdistgrid(Mesh **meshpp, int *egrid, int *num_lelems, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcreateelemdistgrid()"

  // Declare id vectors
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

    // Get the set of owned elem ids
    {
      getElemGIDS(*meshp, egids);
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


void ESMCI_meshinfoserialize(int *intMeshFreed,
                             int *spatialDim, int *parametricDim,
                             char *buffer, int *length, int *offset,
                             ESMC_InquireFlag *inquireflag, int *localrc,
                             ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshinfoserialize()"

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > vars.
    int size = 3*sizeof(int);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < size) {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add a Mesh object", ESMC_CONTEXT, localrc);
         return;
      }
    }

    // Save meshfreed
    ip= (int *)(buffer + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = *intMeshFreed;
      *ip++ = *spatialDim;
      *ip++ = *parametricDim;
    }

     // Adjust offset
    *offset += size;

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
}


void ESMCI_meshinfodeserialize(int *intMeshFreed,
                               int *spatialDim, int *parametricDim,
                               char *buffer, int *offset, int *localrc,
                               ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshinfodeserialize()"

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // Get pointer
    ip= (int *)(buffer + *offset);

    // Get values
    *intMeshFreed=*ip++;
    *spatialDim=*ip++;
    *parametricDim=*ip++;

    // Adjust offset
    *offset += 3*sizeof(int);

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
}


void ESMCI_meshserialize(Mesh **meshpp,
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *rc,
                ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshserialize()"

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

    // Record which Fields are present
#define ESMF_RECONCILE_MESH_NUM_FIELDS 11
    int fields_present[ESMF_RECONCILE_MESH_NUM_FIELDS];

    // zero out fields present list
    for (int i=0; i<ESMF_RECONCILE_MESH_NUM_FIELDS; i++) {
      fields_present[i]=0;
    }

    // Record list
    if (mesh.GetCoordField() != NULL) fields_present[0]=1;
    if (mesh.GetField("mask") != NULL) fields_present[1]=1;
    if (mesh.GetField("node_mask_val") != NULL) fields_present[2]=1;
    if (mesh.GetField("elem_mask") != NULL) fields_present[3]=1;
    if (mesh.GetField("elem_mask_val") != NULL) fields_present[4]=1;
    if (mesh.GetField("elem_area") != NULL) fields_present[5]=1;
    if (mesh.GetField("elem_frac2") != NULL) fields_present[6]=1;
    if (mesh.GetField("elem_frac") != NULL) fields_present[7]=1;
    if (mesh.GetField("orig_coordinates") != NULL) fields_present[8]=1;
    if (mesh.GetField("elem_coordinates") != NULL) fields_present[9]=1;
    if (mesh.GetField("elem_orig_coordinates") != NULL) fields_present[10]=1;


    // DEBUG OUTPUT
    // for (int i=0; i<ESMF_RECONCILE_MESH_NUM_FIELDS; i++) {
    //  printf("%d# S: %d fields_present=%d\n",Par::Rank(),i,fields_present[i]);
    //}


    // Calc Size
    int size = 6*sizeof(int)+
               ESMF_RECONCILE_MESH_NUM_FIELDS*sizeof(int)+
               2*numSets*sizeof(UInt);


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
      *ip++ = mesh.orig_spatial_dim;
      *ip++ = numSets;

      for (int i=0; i<ESMF_RECONCILE_MESH_NUM_FIELDS; i++) {
        *ip++=fields_present[i];
      }

      if (mesh.is_split) *ip++ = 1;
      else *ip++ = 0;
      *ip++ = mesh.max_non_split_id;
    }


    // Save UInt data
    uip=(UInt *)ip;
    if (*inquireflag != ESMF_INQUIREONLY) {

      // Save set sizes and values
      if (nvalSetSizes != NULL) {
        for (int i=0; i<numSets; i++) {
          *uip++=nvalSetSizes[i];
        }
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
      if (nvalSetObjSizes != NULL) {
        for (int i=0; i<numSets; i++) {
          *uip++=nvalSetObjSizes[i];
        }
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


void ESMCI_meshdeserialize(Mesh **meshpp,
                                 char *buffer, int *offset, int *rc,
                                 ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshdeserialize()"

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
    int orig_spatial_dim=*ip++;


    // Get Some Mesh info
    int numSets;
    std::vector<UInt> nvalSetSizes;
    std::vector<UInt> nvalSetVals;
    std::vector<UInt> nvalSetObjSizes;
    std::vector<UInt> nvalSetObjVals;

    numSets=*ip++;

    // Decode which Fields are present
    int fields_present[ESMF_RECONCILE_MESH_NUM_FIELDS]; // ESMF_RECONCILE_MESH_NUM_FIELDS DEFINED ABOVE

    for (int i=0; i<ESMF_RECONCILE_MESH_NUM_FIELDS; i++) {
      fields_present[i]=*ip++;
    }

    // Stuff for split meshes
    int is_split=*ip++;
    int max_non_split_id=*ip++;


    // convert pointer
    uip=(UInt *)ip;

      // Retrieve sizes and values
      nvalSetSizes.resize(numSets,0);
      for (int i=0; i<numSets; i++) {
        //      nvalSetSizes[i]=*uip++; THIS SEEMS TO CONFUSE PGI OPTIMIZER ?
        nvalSetSizes[i]=*uip;
        uip++;
      }

      int k=0;
      for (int i=0; i<numSets; i++) {
        nvalSetVals.resize(k+nvalSetSizes[i],0);
        for (int j=0; j<nvalSetSizes[i]; j++) {
          //      nvalSetVals[k]=*uip++; THIS SEEMS TO CONFUSE PGI OPTIMIZER ?
          nvalSetVals[k]=*uip;
          uip++;
          k++;
        }
      }

      // Save set obj sizes and value
      nvalSetObjSizes.resize(numSets,0);
      for (int i=0; i<numSets; i++) {
        //      nvalSetObjSizes[i]=*uip++; THIS SEEMS TO CONFUSE PGI OPTIMIZER ?
        nvalSetObjSizes[i]=*uip;
        uip++;
      }

      k=0;
      for (int i=0; i<numSets; i++) {
        nvalSetObjVals.resize(k+nvalSetObjSizes[i],0);
        for (int j=0; j<nvalSetObjSizes[i]; j++) {
          // nvalSetObjVals[k]=*uip++; THIS SEEMS TO CONFUSE PGI OPTIMIZER ?
          nvalSetObjVals[k]=*uip;
          uip++;
          k++;
        }
      }

    // Adjust offset
      *offset += 6*sizeof(int)+ESMF_RECONCILE_MESH_NUM_FIELDS*sizeof(int)+
      nvalSetSizes.size()*sizeof(UInt)+nvalSetVals.size()*sizeof(UInt)+
      nvalSetObjSizes.size()*sizeof(UInt)+nvalSetObjVals.size()*sizeof(UInt);


    // Create Mesh
    Mesh *meshp = new Mesh();

    // Set dimensions
    meshp->set_spatial_dimension(spatial_dim);
    meshp->set_parametric_dimension(parametric_dim);
    meshp->orig_spatial_dim=orig_spatial_dim;

    // Stuff for split meshes
    if (is_split==1) meshp->is_split=true;
    else  meshp->is_split=false;

    meshp->max_non_split_id=max_non_split_id;

    //  printf(" is_split=%d mnsi=%d\n",meshp->is_split,meshp->max_non_split_id);


    // Register fields
    Context ctxt; ctxt.flip(); // Needed below for element registration
    if (fields_present[0]) meshp->RegisterNodalField(*meshp, "coordinates", spatial_dim);
    if (fields_present[1]) meshp->RegisterNodalField(*meshp, "mask", 1);
    if (fields_present[2]) meshp->RegisterNodalField(*meshp, "node_mask_val", 1);
    if (fields_present[3]) meshp->RegisterField("elem_mask",MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    if (fields_present[4]) meshp->RegisterField("elem_mask_val", MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    if (fields_present[5]) meshp->RegisterField("elem_area", MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    if (fields_present[6]) meshp->RegisterField("elem_frac2", MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    if (fields_present[7]) meshp->RegisterField("elem_frac", MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
    if (fields_present[8]) meshp->RegisterNodalField(*meshp, "orig_coordinates", orig_spatial_dim);
    if (fields_present[9]) meshp->RegisterField("elem_coordinates", MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, spatial_dim, true);
    if (fields_present[10]) meshp->RegisterField("elem_orig_coordinates", MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, orig_spatial_dim, true);

    // DEBUG OUTPUT
    // for (int i=0; i<ESMF_RECONCILE_MESH_NUM_FIELDS; i++) {
    //  printf("%d# D: %d fields_present=%d\n",Par::Rank(),i,fields_present[i]);
    //}
#undef ESMF_RECONCILE_MESH_NUM_FIELDS


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


void ESMCI_meshfindpnt(Mesh **meshpp, int *unmappedaction, int *dimPnts, int *numPnts,
                                        double *pnts, int *pets, int *rc){

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshfindpnt()"

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

void ESMCI_getlocalelemcoords(Mesh **meshpp, double *ecoords, int *_orig_sdim, int *rc)
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_getlocalelemcoords()"

{
    int localrc;
    try {
        Mesh *meshp = *meshpp;
        ThrowRequire(meshp);
        Mesh &mesh = *meshp;

        // Initialize the parallel environment for mesh (if not already done)
        ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
        if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
            throw localrc;

        // Get some info
        int num_elems = mesh.num_elems();

        // Get coords pointer and spatial dimension depending on existence
        // of elem_coordinates field.
        MEField<> *elem_coords = mesh.GetField("elem_orig_coordinates");
        if (!elem_coords) {
            elem_coords = mesh.GetField("elem_coordinates");
        }

        // Make a map between data index and associated node pointer
        std::vector<std::pair<int,MeshObj *> > index_to_elem;
        index_to_elem.reserve(num_elems);

        // iterate through local nodes collecting indices and node pointers
        Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
            MeshObj &elem = *ei;

            // See if locally owned
           if (!GetAttr(elem).is_locally_owned()) continue;

           // If it's a split element, then skip
           if (mesh.is_split && elem.get_id() > mesh.max_non_split_id) continue;

            int idx = elem.get_data_index();
            index_to_elem.push_back(std::make_pair(idx,&elem));
        }

        // Sort by data index
        std::sort(index_to_elem.begin(), index_to_elem.end());

        if (elem_coords) {
            int sdim=*_orig_sdim;
            int elemCoordPos = 0;
            //Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
            //for (; ei != ee; ++ei) {
            for (UInt i = 0; i < index_to_elem.size(); ++i) {
                //MeshObj &elem = *ei;
                //if (!GetAttr(elem).is_locally_owned()) continue;

                MeshObj &elem = *(index_to_elem[i].second);

                // Set coordinate value to input array
                double *coords=elem_coords->data(elem);
                //printf("\nESMCI:getlocalelemcoords, coords = [");
                for (int j = 0; j < sdim; ++j) {
                    ecoords[elemCoordPos++] = coords[j];
                    //printf("GEC %d %f \n ",elem.get_id(),coords[j]);
                }
                //printf("]\n");
            }
        } else {
            if (ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                " - elem_coordinates not found in Mesh",
                ESMC_CONTEXT, &localrc)) throw localrc;
        }
    } catch(std::exception &x) {
        // catch Mesh exception return code
        if (x.what()) {
            localrc = ESMC_RC_INTNRL_BAD;
            ESMC_LogDefault.MsgFoundError(localrc, x.what(), ESMC_CONTEXT, rc);
    } else {
        localrc = ESMC_RC_INTNRL_BAD;
        ESMC_LogDefault.MsgFoundError(localrc, "UNKNOWN", ESMC_CONTEXT, rc);
    }
    if (rc!=NULL) *rc = localrc;
    return;
    } catch(int localrc) {
        // catch standard ESMF return code
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
        if (rc!=NULL) *rc = localrc;
        return;
    } catch(...) {
        localrc = ESMC_RC_INTNRL_BAD;
        ESMC_LogDefault.MsgFoundError(localrc,
            "- Caught unknown exception", ESMC_CONTEXT, rc);
        if (rc!=NULL) *rc = localrc;
        return;
    }
    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void ESMCI_getlocalcoords(Mesh **meshpp, double *nodeCoord, int *_orig_sdim, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_getlocalcoords()"

    int localrc;
    try {
        Mesh *meshp = *meshpp;
        ThrowRequire(meshp);
        Mesh &mesh = *meshp;

        // Initialize the parallel environment for mesh (if not already done)
        ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
        if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
            throw localrc;

         // Get some info
        int num_nodes = mesh.num_nodes();

        // Choose which coords field and dimension to use
        // try orig_coordinates first, if it doesn't exist
        // then go with coordinates
        MEField<> *coords=mesh.GetField("orig_coordinates");
        int sdim=*_orig_sdim;
        if (!coords) {
            coords = mesh.GetCoordField();
            sdim=mesh.spatial_dim();
        }

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
    } catch(int localrc) {
        // catch standard ESMF return code
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
        return;
    } catch(...) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            "- Caught unknown exception", ESMC_CONTEXT, rc);
        return;
    }

    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;

}

////////////////


void ESMCI_getconnectivity(Mesh **meshpp, double *connCoord, int *nodesPerElem,
                                   int *_orig_sdim, int *rc)
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_getlconnectivity()"
{
    int localrc;
    try {
        Mesh *meshp = *meshpp;
        ThrowRequire(meshp);
        Mesh &mesh = *meshp;

        // Initialize the parallel environment for mesh (if not already done)
        ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
        if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
            throw localrc;

        // Get some info
        int sdim=*_orig_sdim;
        int num_elems = mesh.num_elems();
        int num_nodes = mesh.num_nodes();

        // Choose which coords field and dimension to use
        // try orig_coordinates first, if it doesn't exist
        // then go with coordinates
        MEField<> *coords=mesh.GetField("orig_coordinates");
        if (!coords) {
            coords = mesh.GetCoordField();
            sdim=mesh.spatial_dim();
        }

        // Make a map between data index and associated elem pointer
        std::vector<std::pair<int,MeshObj *> > index_to_elem;
        index_to_elem.reserve(num_elems);

        // iterate through local nodes collecting indices and node pointers
        Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
            MeshObj &elem = *ei;

            if (!GetAttr(elem).is_locally_owned()) continue;

            int idx = elem.get_data_index();
            index_to_elem.push_back(std::make_pair(idx,&elem));
        }

        // Sort by data index
        std::sort(index_to_elem.begin(), index_to_elem.end());

        // iterate through local elements
        int npePos = 0, cpPos = 0;
        for (UInt i = 0; i < index_to_elem.size(); ++i) {
            MeshObj &elem = *(index_to_elem[i].second);

            // Get number of nodes in element
            const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(elem);

            // iterate through local nodes collecting coordinates and counting
                        for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){

                                const MeshObj &node = *(elem.Relations[s].obj);

                                // NOTE: we are iterating all nodes on this element to put
                                //       together a connectivity list of node coordinates
                                //if (!GetAttr(node).is_locally_owned()) continue;

                                double *c=coords->data(node);
                for (int j = 0; j < sdim; ++j)
                        connCoord[cpPos++] = c[j];

                        // NOTE: must implement sort if we want IDs
                //nodeIDs[s]=node.get_id();
                        }
                        // NOTE: must implement sort if we want IDs
                        //elemIDs[npePos] = elem.get_id();

                        nodesPerElem[npePos++] = topo->num_nodes;
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
    } catch(int localrc) {
        // catch standard ESMF return code
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
        return;
    } catch(...) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            "- Caught unknown exception", ESMC_CONTEXT, rc);
        return;
    }

    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;

}

////////////////


void ESMCI_meshgetarea(Mesh **meshpp, int *num_elem, double *elem_areas, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshgetarea()"

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
    // TODO: IN FUTURE MAYBE JUST USE DATA INDEX DIRECTY, ALTHOUGH THEN HAVE TO STICK
    //       SPLIT ELEMENTS AT END
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

      // Add in the split elements
      if (mesh.is_split) {
        std::map<int,int> id_to_index;
        for (int i=0; i<egids.size(); i++) {
          id_to_index[egids[i]]=i;
        }

        // Iterate through split elements adding in area
        Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
        for (; ei != ee; ++ei) {
          MeshObj &elem = *ei;

          // Don't do non-local elements
          if (!GetAttr(elem).is_locally_owned()) continue;

          // Get the element id
          int eid=elem.get_id();

          // Skip non-split elements
          if (!(eid > mesh.max_non_split_id)) continue;

          // Get area from field
          double *area=area_field->data(elem);

          // Get original id
          int orig_id=mesh.split_to_orig_id[eid];

          // Get index
          int index=id_to_index[orig_id];

          // Add area to what's already there
          elem_areas[index] += *area;
        }
      }

      // Return success
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return;
      }

 /* XMRKX */

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


    // Add in the split elements
    if (mesh.is_split) {
      std::map<int,int> id_to_index;
      for (int i=0; i<egids.size(); i++) {
        id_to_index[egids[i]]=i;
      }

      // Iterate through split elements adding in area
      Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;

        // Don't do non-local elements
        if (!GetAttr(elem).is_locally_owned()) continue;

        // Get the element id
        int eid=elem.get_id();

        // Skip non-split elements
        if (!(eid > mesh.max_non_split_id)) continue;

        // Compute area depending on dimensions
        double area;
        if (pdim==2) {
          if (sdim==2) {
            get_elem_coords_2D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_2D, tmp_coords, &num_poly_nodes, poly_coords);
            remove_0len_edges2D(&num_poly_nodes, poly_coords);
            area=area_of_flat_2D_polygon(num_poly_nodes, poly_coords);
          } else if (sdim==3) {
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

        // Get original id
        int orig_id=mesh.split_to_orig_id[eid];

        // Get index
        int index=id_to_index[orig_id];

        // Add area to what's already there
        elem_areas[index] += area;
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


  // Declare polygon information
#undef  MAX_NUM_POLY_COORDS
#undef  MAX_NUM_POLY_NODES_2D
#undef  MAX_NUM_POLY_NODES_3D
}

void ESMCI_meshgetdimensions(Mesh **meshpp, int *sdim, int *pdim, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshgetdimensions()"
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

void ESMCI_meshgetcentroid(Mesh **meshpp, int *num_elem, double *elem_centroid, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshgetcentroid()"

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

void ESMCI_meshgetfrac(Mesh **meshpp, int *_num_elem, double *elem_fracs, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshgetfrac()"
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

    // Dereference number of elements
    int num_elem=*_num_elem;

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
    if (num_elem != egids.size()) {
      Throw() << "Number of elements doesn't match size of input array for areas";
    }

    // Get frac field
    MEField<> *elem_frac = mesh.GetField("elem_frac");
    if (!elem_frac) Throw() << "Getting elem_frac when it doesn't exist";

    // Loop through elements and put areas into array
    if (!mesh.is_split) {
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
    } else {
      // get map of elem ids to index
      std::map<int,int> id_to_index;
      for (int i=0; i<egids.size(); i++) {
        id_to_index[egids[i]]=i;
      }

      // Zero out array
      for (int i=0; i<num_elem; i++) {
        elem_fracs[i]=0.0;
      }

      // Iterate through split elements adding in frac
      Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;

        // Don't do non-local elements
        if (!GetAttr(elem).is_locally_owned()) continue;

        // Get the element id
        UInt eid=elem.get_id();

        // Get frac data
        double *f=elem_frac->data(elem);
        double frac=*f;

        // See if the element is part of a larger polygon
        std::map<UInt,double>::iterator mi =  mesh.split_id_to_frac.find(eid);

        // Not part of something larger, so just stick in array
        if (mi == mesh.split_id_to_frac.end()) {
          int index=id_to_index[eid];
          elem_fracs[index] = frac;
          continue;
        }

        // It is part of original poly, so modify by fraction
        frac *= mi->second;

        // Translate id if necessary
        int orig_id;
        std::map<UInt,UInt>::iterator soi =  mesh.split_to_orig_id.find(eid);
        if (soi == mesh.split_to_orig_id.end()) {
          orig_id=eid;
        } else {
          orig_id=soi->second;
        }

        // Get index
        int index=id_to_index[orig_id];

        // Add modified frac to what's already there
        elem_fracs[index] += frac;
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

void ESMCI_meshgetfrac2(Mesh **meshpp, int *num_elem, double *elem_fracs, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshgetfrac2()"
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
void ESMCI_triangulate(int *pdim, int *sdim, int *numPnts,
                                        double *pnts, double *td, int *ti, int *triInd, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_triangulate()"
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
          " - there was a problem with triangulation (e.g. repeated points, clockwise poly, etc.)",
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


void ESMCI_meshturnoncellmask(Mesh **meshpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshturnoncellmask()"

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
    if (!present(maskValuesArg)) {
      // Set return code
      if (rc!=NULL) *rc = ESMF_SUCCESS;

      // Leave
      return;
    }

    // If no mask values then leave
    if (!present(maskValuesArg)) {
      // Set return code
      if (rc!=NULL) *rc = ESMF_SUCCESS;

      // Leave
      return;
    }

    // Get mask values
    int numMaskValues=(maskValuesArg)->extent[0];
    int *ptrMaskValues=&((maskValuesArg)->array[0]);


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

        // Don't just do local, because masking needs to be symmetrical
        // between local and not otherwise results change as the number
        // of PETs changes
        // if (!GetAttr(elem).is_locally_owned()) continue;

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
void ESMCI_meshturnoffcellmask(Mesh **meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshturnoffcellmask()"

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

        // Don't just do local, because masking needs to be symmetrical
        // between local and not otherwise results change as the number
        // of PETs changes
        //if (!GetAttr(elem).is_locally_owned()) continue;

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
void ESMCI_meshturnonnodemask(Mesh **meshpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshturnonnodemask()"
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
    if (!present(maskValuesArg)) {
      // Set return code
      if (rc!=NULL) *rc = ESMF_SUCCESS;

      // Leave
      return;
    }

    // If no mask values then leave
    if (!present(maskValuesArg)) {
      // Set return code
      if (rc!=NULL) *rc = ESMF_SUCCESS;

      // Leave
      return;
    }

    // Get mask values
    int numMaskValues=(maskValuesArg)->extent[0];
    int *ptrMaskValues=&((maskValuesArg)->array[0]);


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

        // Don't just do local, because masking needs to be symmetrical
        // between local and not otherwise results change as the number
        // of PETs changes
        //if (!GetAttr(node).is_locally_owned()) continue;

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
void ESMCI_meshturnoffnodemask(Mesh **meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshturnoffcellmask()"
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

        // Don't just do local, because masking needs to be symmetrical
        // between local and not otherwise results change as the number
        // of PETs changes
        //if (!GetAttr(node).is_locally_owned()) continue;

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

void ESMCI_get_polygon_area(int *spatialdim, int *nedges,
                                                 double *points, double *area, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_get_polygon_area()"

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

void ESMCI_meshcreatefrommeshes(Mesh **meshapp, Mesh **meshbpp, Mesh **meshpp,
ESMC_MeshOp_Flag * meshop, double * threshold, int *rc) {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcreatefrommeshes()"

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




void expand_split_elem_ids(Mesh *mesh, int num_elem_gids, int *elem_gids, int *_num_elem_gids_ws, int **_elem_gids_ws, std::map<UInt,UInt> &split_to_orig_id) {

#undef  ESMC_METHOD
#define ESMC_METHOD "expand_split_elem_ids()"

  // Copy input array to UInt
  UInt *elem_gids_u=NULL;
  if (num_elem_gids>0) {
    elem_gids_u= new UInt[num_elem_gids];

    for (int i=0; i<num_elem_gids; i++) {
      elem_gids_u[i]=elem_gids[i];
    }
  }


  // Get number of elements
  int num_gids=mesh->num_elems();

  // Get list of split and orig element gids
  UInt *gids_split=NULL;
  UInt *gids_orig=NULL;
  if (num_gids>0) {

    // Allocate space
    gids_split= new UInt[num_gids];
    gids_orig= new UInt[num_gids];

    // Loop through list putting into arrays
    int pos=0;
    Mesh::iterator ei = mesh->elem_begin(), ee = mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;

      // Only do local
      if (!GetAttr(elem).is_locally_owned()) continue;

      // Get element id
      UInt split_eid=elem.get_id();

      // See if this is a split id
      std::map<UInt,UInt>::iterator soi=mesh->split_to_orig_id.find(split_eid);

      // If this is a split set it to the original, otherwise just set it to the elem id
      UInt orig_eid;
      if (soi==mesh->split_to_orig_id.end()) {
        orig_eid=split_eid;
      } else {
        orig_eid=soi->second;
      }

      // Put into arrays
      gids_orig[pos]=orig_eid;
      gids_split[pos]=split_eid;

      // Next
      pos++;
    }
  }

  // Put into a DDir
  DDir<> id_map_dir;
  id_map_dir.Create(num_gids,gids_orig,gids_split);

  // Clean up
  if (num_gids>0) {
    if (gids_split!= NULL) delete [] gids_split;
    if (gids_orig != NULL) delete [] gids_orig;
  }

  // Do a look up of the input ids
  std::vector<DDir<>::dentry> lookups;
  id_map_dir.RemoteGID(num_elem_gids, elem_gids_u, lookups);

  // Don't need anymore so clean up
  if (num_elem_gids>0) {
    if (elem_gids_u != NULL) delete [] elem_gids_u;
  }

  // Loop through lookups and generate new list
  int num_elem_gids_ws=lookups.size();
  int *elem_gids_ws=NULL;
  if (num_elem_gids_ws>0) {
    elem_gids_ws= new int[num_elem_gids_ws];

    for (int i=0; i<lookups.size(); i++) {
      elem_gids_ws[i]=lookups[i].origin_lid;

      // If split put into map
      if (lookups[i].gid != lookups[i].origin_lid) {
        split_to_orig_id[lookups[i].origin_lid]=lookups[i].gid;
      }
    }
  }

  // Output
  *_num_elem_gids_ws=num_elem_gids_ws;
  *_elem_gids_ws=elem_gids_ws;
}

void calc_split_id_to_frac(Mesh *mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_split_id_to_frac()"

  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];
  double tmp_coords[MAX_NUM_POLY_COORDS];


  // Get useful info
  int sdim=mesh->spatial_dim();
  MEField<> *cfield = mesh->GetCoordField();

  // Setup map to hold total areas
  std::map<UInt,double> orig_id_to_area;

  // Loop gathering split,orig id pairs
  std::vector<UInt> split_ids;
  std::vector<UInt> orig_ids;

  std::map<UInt,UInt>::iterator mi=mesh->split_to_orig_id.begin();
  std::map<UInt,UInt>::iterator me=mesh->split_to_orig_id.end();
  for ( ; mi != me; mi++) {
    // Get split element id
    split_ids.push_back(mi->first);

    // Get original element id
    orig_ids.push_back(mi->second);

    // Get uniqued list of original ids and set to zero
    orig_id_to_area[mi->second]=0.0;
  }

  // also add orig_id to orig_id pairs that are part of each split polygon
  std::map<UInt,double>::iterator otai=orig_id_to_area.begin();
  std::map<UInt,double>::iterator otae=orig_id_to_area.end();
  for ( ; otai != otae; otai++) {
    // Get split element id
    split_ids.push_back(otai->first);

    // Get original element id
    orig_ids.push_back(otai->first);
  }


  // Loop split triangles set their area and sum into original polygon area
  for (int i=0; i<split_ids.size(); i++) {

    // Get split element id
    UInt s_id=split_ids[i];

    // Get original element id
    UInt o_id=orig_ids[i];


    //  Find the corresponding Mesh element
    Mesh::MeshObjIDMap::iterator ei = mesh->map_find(MeshObj::ELEMENT, s_id);
    if (ei == mesh->map_end(MeshObj::ELEMENT)) {
      Throw() << "Element not in mesh";
    }

    // Get the element
    const MeshObj &elem = *ei;

    // Compute area depending on dimensions
    double area;
    if (sdim==2) {
      get_elem_coords_2D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_2D, tmp_coords, &num_poly_nodes, poly_coords);
      remove_0len_edges2D(&num_poly_nodes, poly_coords);
      area=area_of_flat_2D_polygon(num_poly_nodes, poly_coords);
    } else if (sdim==3) {
      get_elem_coords_3D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_3D, tmp_coords, &num_poly_nodes, poly_coords);
      remove_0len_edges3D(&num_poly_nodes, poly_coords);
      area=great_circle_area(num_poly_nodes, poly_coords);
    }

    // Set area in split_id_to_frac
    mesh->split_id_to_frac[s_id]=area;

    // Sum to original polygon area
    //  Find the corresponding orig id
    std::map<UInt,double>::iterator oi = orig_id_to_area.find(o_id);
    // If not in map yet then complain
    if (oi == orig_id_to_area.end()) {
      Throw() << "orig id no in map!!! \n";
    }

    // Add to original polygon total
    oi->second += area;
  }


  // Loop again dividing to get fractional area
  for (int i=0; i<split_ids.size(); i++) {

    // Get split element id
    UInt s_id=split_ids[i];

    // Get original element id
    UInt o_id=orig_ids[i];

    // Find total area
    std::map<UInt,double>::iterator oi = orig_id_to_area.find(o_id);
    if (oi == orig_id_to_area.end()) {
      Throw() << "Origianl id not found in map! \n";
    }
    double total_area=oi->second;


    // Find entry to split id
    std::map<UInt,double>::iterator si = mesh->split_id_to_frac.find(s_id);
    if (si == mesh->split_id_to_frac.end()) {
      Throw() << "Split id not found in map! \n";
    }

    // Divide to get fraction
    si->second=si->second/total_area;

  }
 /* XMRKX */

#undef  MAX_NUM_POLY_COORDS
#undef  MAX_NUM_POLY_NODES_2D
#undef  MAX_NUM_POLY_NODES_3D
}


void ESMCI_meshcreateredistelems(Mesh **src_meshpp, int *num_elem_gids, int *elem_gids,
                                                    Mesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcreateredistelems()"

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }

    // Dereference
    Mesh *src_mesh=*src_meshpp;

    // if not split mesh, then just do the usual thing
    if (!src_mesh->is_split) {
      MeshRedistElem(src_mesh, *num_elem_gids, elem_gids, output_meshpp);
    } else {
      // If split mesh expand ids
      int num_elem_gids_ws;
      int *elem_gids_ws=NULL;
      std::map<UInt,UInt> split_to_orig_id;
      expand_split_elem_ids(src_mesh,*num_elem_gids,elem_gids,&num_elem_gids_ws,&elem_gids_ws,split_to_orig_id);

      // Call into redist with expanded ids
      MeshRedistElem(src_mesh, num_elem_gids_ws, elem_gids_ws, output_meshpp);

      // dereference output mesh
      Mesh *output_mesh=*output_meshpp;

      // if split mesh add info
      output_mesh->is_split=src_mesh->is_split;
      output_mesh->max_non_split_id=src_mesh->max_non_split_id;
      output_mesh->split_to_orig_id=split_to_orig_id;

      // calculate split_id_to_frac map from other info
      calc_split_id_to_frac(output_mesh);

#if 0
      // DEBUG OUTPUT
    // Loop and get split-orig id pairs
    std::map<UInt,UInt>::iterator mi=output_mesh->split_to_orig_id.begin();
    std::map<UInt,UInt>::iterator me=output_mesh->split_to_orig_id.end();

    for ( ; mi != me; mi++) {
      printf("%d# split=%d orig=%d\n",Par::Rank(),mi->first,mi->second);
    }
#endif


#if 0

    {
      // DEBUG OUTPUT
    // Loop and get split-frac id pairs
    std::map<UInt,double>::iterator si=src_mesh->split_id_to_frac.begin();
    std::map<UInt,double>::iterator se=src_mesh->split_id_to_frac.end();

    for ( ; si != se; si++) {
      printf("%d# S: split=%d frac=%f\n",Par::Rank(),si->first,si->second);
    }
    }

    {
      // DEBUG OUTPUT
    // Loop and get split-frac id pairs
    std::map<UInt,double>::iterator si=output_mesh->split_id_to_frac.begin();
    std::map<UInt,double>::iterator se=output_mesh->split_id_to_frac.end();

    for ( ; si != se; si++) {
      printf("%d# O: split=%d frac=%f\n",Par::Rank(),si->first,si->second);
    }
    }
#endif
    /* XMRKX */

      // Free split gids
      if (elem_gids_ws !=NULL) delete [] elem_gids_ws;
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



void ESMCI_meshcreateredistnodes(Mesh **src_meshpp, int *num_node_gids, int *node_gids,
                                                    Mesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcreateredistelems()"

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }

    // Dereference
    Mesh *src_mesh=*src_meshpp;

    // if not split mesh, then just do the usual thing
    if (!src_mesh->is_split) {
      MeshRedistNode(src_mesh, *num_node_gids, node_gids, output_meshpp);
    } else {

      // Redist nodes
      // NOTE: internally sets: is_split and split_to_orig_id map
      //       split_id_to_frac is set below
      MeshRedistNode(src_mesh, *num_node_gids, node_gids, output_meshpp);

      // dereference output mesh
      Mesh *output_mesh=*output_meshpp;

      // if split mesh add info
      // output_mesh->is_split=src_mesh->is_split; // SET INSIDE MeshRedistNode()
      output_mesh->max_non_split_id=src_mesh->max_non_split_id;
      // output_mesh->split_to_orig_id=split_to_orig_id; // SET INSIDE MeshRedistNode()

      // calculate split_id_to_frac map from other info
      calc_split_id_to_frac(output_mesh);

#if 0
      // DEBUG OUTPUT
    // Loop and get split-orig id pairs
    std::map<UInt,UInt>::iterator mi=output_mesh->split_to_orig_id.begin();
    std::map<UInt,UInt>::iterator me=output_mesh->split_to_orig_id.end();

    for ( ; mi != me; mi++) {
      printf("%d# split=%d orig=%d\n",Par::Rank(),mi->first,mi->second);
    }
#endif


#if 0

    {
      // DEBUG OUTPUT
    // Loop and get split-frac id pairs
    std::map<UInt,double>::iterator si=src_mesh->split_id_to_frac.begin();
    std::map<UInt,double>::iterator se=src_mesh->split_id_to_frac.end();

    for ( ; si != se; si++) {
      printf("%d# S: split=%d frac=%f\n",Par::Rank(),si->first,si->second);
    }
    }

    {
      // DEBUG OUTPUT
    // Loop and get split-frac id pairs
    std::map<UInt,double>::iterator si=output_mesh->split_id_to_frac.begin();
    std::map<UInt,double>::iterator se=output_mesh->split_id_to_frac.end();

    for ( ; si != se; si++) {
      printf("%d# O: split=%d frac=%f\n",Par::Rank(),si->first,si->second);
    }
    }

#endif
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



void ESMCI_meshcreateredist(Mesh **src_meshpp, int *num_node_gids, int *node_gids,
                                               int *num_elem_gids, int *elem_gids,  Mesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcreateredist()"

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }


    // Dereference
    Mesh *src_mesh=*src_meshpp;

    // if not split mesh, then just do the usual thing
    if (!src_mesh->is_split) {
      MeshRedist(src_mesh, *num_node_gids, node_gids, *num_elem_gids, elem_gids, output_meshpp);
    } else {
      // If split mesh expand ids
      int num_elem_gids_ws;
      int *elem_gids_ws=NULL;
      std::map<UInt,UInt> split_to_orig_id;
      expand_split_elem_ids(src_mesh,*num_elem_gids,elem_gids,&num_elem_gids_ws,&elem_gids_ws,split_to_orig_id);

      // Call into redist with expanded ids
      MeshRedist(src_mesh, *num_node_gids, node_gids, num_elem_gids_ws, elem_gids_ws, output_meshpp);

      // dereference output mesh
      Mesh *output_mesh=*output_meshpp;

      // if split mesh add info
      output_mesh->is_split=src_mesh->is_split;
      output_mesh->max_non_split_id=src_mesh->max_non_split_id;
      output_mesh->split_to_orig_id=split_to_orig_id;

      // calculate split_id_to_frac map from other info
      calc_split_id_to_frac(output_mesh);

      // Free split gids
      if (elem_gids_ws !=NULL) delete [] elem_gids_ws;
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



// This method verifies that nodes in node_gids array are the same as the local nodes in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of nodes in both cases are the same and that every
// entry in node_gids is contained in meshpp
void ESMCI_meshchecknodelist(Mesh **meshpp, int *_num_node_gids, int *node_gids,
                                             int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshchecknodelist()"

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
void ESMCI_meshcheckelemlist(Mesh **meshpp, int *_num_elem_gids, int *elem_gids,
                                             int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcheckelemlist()"

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }


    // For convenience deref mesh
    Mesh *meshp = *meshpp;

    // For convenience deref number
    int num_elem_gids=*_num_elem_gids;


    // Loop through counting local elems
    int num_local_elems=0;
    Mesh::iterator ni = meshp->elem_begin(), ne = meshp->elem_end();
    for (;ni != ne; ++ni) {
      const MeshObj &elem = *ni;

      // Don't do split elements
      if (meshp->is_split && (elem.get_id() > meshp->max_non_split_id)) continue;

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

// Interface to internal code to convert coords from spherical in degrees to Cartesian
// Input is: lon, lat - spherical coordinates in degrees
// Output is: x,y,z - Cartesian coordinates
//
void ESMCI_sphdeg_to_cart(double *lon, double *lat,
                                             double *x, double *y, double *z, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_sphdeg_to_cart()"

   try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,rc))
   return;  // bail out with exception
    }

    // TODO: should I change this to take in coords in array format?

    // temp arrays
    double sph_coords[2];
    double cart_coords[3];

    // Put sph coords into temporary array
    sph_coords[0]=*lon;
    sph_coords[1]=*lat;

    // Convert coords using standard function
    ESMCI_CoordSys_ConvertToCart(ESMC_COORDSYS_SPH_DEG, 2,
                                 sph_coords, cart_coords);

    // Get cart coords from temporary array
   *x=cart_coords[0];
   *y=cart_coords[1];
   *z=cart_coords[2];

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


// This method sets the pole values so a 2D Mesh from a SCRIP grid can still be used in regrid with poles
void ESMCI_meshsetpoles(Mesh **meshpp, int *_pole_val, int *_min_pole_gid, int *_max_pole_gid,
                                             int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshsetpoles()"

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }

    // For convenience deref mesh
    Mesh *meshp = *meshpp;

    // For convenience deref numbers
    int pole_val=*_pole_val;
    int min_pole_gid=*_min_pole_gid;
    int max_pole_gid=*_max_pole_gid;

    // printf("pole_val=%d min_pole=%d max_pole=%d\n",pole_val,min_pole_gid,max_pole_gid);

    // Loop through gids and change associated nodes to have the given pole value
    for (int gid=min_pole_gid; gid<=max_pole_gid; gid++) {

      //  Find the corresponding Mesh node
      Mesh::MeshObjIDMap::iterator mi =  meshp->map_find(MeshObj::NODE, gid);

      // If not in the local mesh, then loop to next
      if (mi == meshp->map_end(MeshObj::NODE)) continue;

      // Get the element
      MeshObj &node = *mi;

      // Create new attr with the old type, new nodeset, and old context
      const Context &old_ctxt = GetMeshObjContext(node);
      Attr attr(MeshObj::NODE, pole_val, old_ctxt,
                old_ctxt.is_set(Attr::SHARED_ID),old_ctxt.is_set(Attr::OWNED_ID),
                old_ctxt.is_set(Attr::ACTIVE_ID),old_ctxt.is_set(Attr::GENESIS_ID));
      meshp->update_obj(&node, attr);

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


void ESMCI_meshcreatedual(Mesh **src_meshpp, Mesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcreatedual()"

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception
    }

    // Call C++ side
    MeshDual(*src_meshpp, output_meshpp);

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

void ESMCI_MeshFitOnVM(Mesh **meshpp,
                       VM **new_vm,
                       int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_MeshFitOnVM()"
  int localrc;

   try {

     // Initialize the parallel environment for mesh (if not already done)
     {
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
         throw localrc;  // bail out with exception
     }

     // Get Pointer to Mesh
     ThrowRequire(meshpp);
     Mesh *mesh = *meshpp;

     // Get current VM
     VM *curr_vm=VM::getCurrent(&localrc);
     if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
       throw localrc;  // bail out with exception

     // Get current VM size
     int curr_vm_size=curr_vm->getPetCount();

     // Get current VM rank
     int curr_vm_rank=curr_vm->getLocalPet();

     // Describe mapping of current PET
     int new_vm_rank=-1; // if there is no pet, set to -1
     if (ESMC_NOT_PRESENT_FILTER(new_vm) != ESMC_NULL_POINTER) {
       new_vm_rank=(*new_vm)->getLocalPet();
     }

     // Allocate array
     int *rank_map=new int[curr_vm_size];

     // Create array mapping from current vm to input vm
     localrc=curr_vm->allgather(&new_vm_rank,rank_map,sizeof(int));
     if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
       throw localrc;  // bail out with exception

#if 0
     // debug output
     for (int p=0; p<curr_vm_size; p++) {
       printf("%d# %d to %d\n",curr_vm_rank,p,rank_map[p]);
     }
#endif


     // Loop through nodes changing owners to owners in new VM
     MeshDB::iterator ni = mesh->node_begin_all(), ne = mesh->node_end_all();
     for (; ni != ne; ++ni) {
       MeshObj &node=*ni;

       // Get original owner
       UInt orig_owner=node.get_owner();

       // Error check owner
       if ((orig_owner < 0) || (orig_owner > curr_vm_size-1)) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_OUTOFRANGE,
                                          " mesh node owner rank outside current vm",
                                          ESMC_CONTEXT, &localrc)) throw localrc;
       }

       // map to new owner rank in new vm
       int new_owner=rank_map[orig_owner];

       // Make sure that the new one is ok
       if (new_owner < 0) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_OUTOFRANGE,
                                          " mesh node owner outside of new vm",
                                          ESMC_CONTEXT, &localrc)) throw localrc;
       }

       // Set new owner
       node.set_owner((UInt)new_owner);
     }


     // Loop through elems changing owners to owners in new VM
     MeshDB::iterator ei = mesh->elem_begin_all(), ee = mesh->elem_end_all();
     for (; ei != ee; ++ei) {
       MeshObj &elem=*ei;

       // Get original owner
       UInt orig_owner=elem.get_owner();

       // Error check owner
       if ((orig_owner < 0) || (orig_owner > curr_vm_size-1)) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_OUTOFRANGE,
                                          " mesh element owner rank outside current vm",
                                          ESMC_CONTEXT, &localrc)) throw localrc;
       }

       // map to new owner rank in new vm
       int new_owner=rank_map[orig_owner];

       // Make sure that the new one is ok
       if (new_owner < 0) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_OUTOFRANGE,
                                          " mesh element owner outside of new vm",
                                          ESMC_CONTEXT, &localrc)) throw localrc;
       }

       // Set new owner
       elem.set_owner((UInt)new_owner);
     }


    // Free map
    delete [] rank_map;

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


void ESMCI_meshcreate_easy_elems(Mesh **meshpp,
                                 int *pdim_ptr, int *sdim_ptr,
                                 int *num_elems_ptr,
                                 InterArray<int> *elemIdsII,
                                 int *elemTypes,
                                 InterArray<int> *elemMaskII,
                                 int *num_elemCorners_ptr,
                                 double *elemCornerCoords,
                                 int *has_elemArea,
                                 double *elemArea,
                                 int *has_elemCoords,
                                 double *elemCoords,
                                 ESMC_CoordSys_Flag *coordSys, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_meshcreate_easy_elems()"

  int localrc;

   try {
     // Initialize the parallel environment for mesh (if not already done)
     ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
     if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
       throw localrc;  // bail out with exception

     // Setup some useful variables
     int pdim=*pdim_ptr;
     int sdim=*sdim_ptr;
     int num_elems=*num_elems_ptr;
     int num_elemCorners=*num_elemCorners_ptr;


     // Create Mesh using internal mesh create method
     ESMCI_meshcreate(meshpp,
                      pdim_ptr, sdim_ptr,
                      coordSys, &localrc);
     if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
       throw localrc;  // bail out with exception


     ////// Create nodes //////

     // Compute number of nodes
     int num_nodes=num_elemCorners; // one node per corner

     // Allocate node id array
     int *node_ids=NULL;
     if (num_nodes > 0) {
       node_ids=new int[num_nodes];
     }

     // Allocate node owners array
     int *node_owners=NULL;
     if (num_nodes > 0) {
       node_owners=new int[num_nodes];
     }

     // For now the node coord array is the same as the corner coord array
     double *node_coords=NULL;
     node_coords=elemCornerCoords;

     // Calc our range of node ids
     int beg_node_ids=0;
     MPI_Scan(&num_nodes,&beg_node_ids,1,MPI_INT,MPI_SUM,Par::Comm());

     // Remove this processors number from the sum to get the beginning on
     // this processor
     beg_node_ids=beg_node_ids-num_nodes;

     // start at 1
     beg_node_ids=beg_node_ids+1;

     // Fill node arrays
     for (int i=0; i<num_nodes; i++) {

       // Start ids at 1, then number sequentially
       node_ids[i]=beg_node_ids+i;

       // Set all owners to the current processor
       node_owners[i]=Par::Rank();
     }

     // Create nodes using internal mesh add nodes method
     ESMCI_meshaddnodes(meshpp, &num_nodes, node_ids,
                        node_coords, node_owners, NULL,
                        coordSys, &sdim, &localrc);
     if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
       throw localrc;  // bail out with exception

     // Deallocate node info
     if (node_ids != NULL) delete [] node_ids;
     if (node_owners != NULL) delete [] node_owners;


     ////// Create elements //////

     // Allocate element id array
     int *elem_ids=NULL;
     if (num_elems > 0) {
       elem_ids=new int[num_elems];
     }

     // printf("%d# num_nodes=%d num_elems=%d \n",Par::Rank(),num_nodes,num_elems);

     // Fill element id array depending on presence of input array
     if (present(elemIdsII)) {
       // Use user provided elem ids

       // Error checking
       if (elemIdsII->dimCount !=1) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
              " elemIds array must be 1D ", ESMC_CONTEXT,  &localrc)) throw localrc;
       }
       if (elemIdsII->extent[0] != num_elems) {
         if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
         "- elementIds array must be the same size as the elementTypes array ", ESMC_CONTEXT, &localrc)) throw localrc;
       }

       // fill from array from passed in values
       for (int i=0; i<num_elems; i++) {
         elem_ids[i]=elemIdsII->array[i];
       }
     } else {
       // Set elems ids internally //

       // Calc our range of elem ids
       int beg_elem_ids=0;
       MPI_Scan(&num_elems,&beg_elem_ids,1,MPI_INT,MPI_SUM,Par::Comm());

       // Remove this processors number from the sum to get the beginning on
       // this processor
       beg_elem_ids=beg_elem_ids-num_elems;

       // start at 1
       beg_elem_ids=beg_elem_ids+1;

       // Set local elem ids based on beginning
       for (int i=0; i<num_elems; i++) {
         // Start ids at 1, then number sequentially
         elem_ids[i]=beg_elem_ids+i;
       }
     }

     // The number of element connections is the same as the number of corners
     int num_elem_conns=num_elemCorners;

     // Allocate element connection array
     int *elem_conns=NULL;
     if (num_elem_conns > 0) {
       elem_conns=new int[num_elem_conns];
     }

     // Fill element connection array
     for (int i=0; i<num_elem_conns; i++) {
       // Since each corner has it's own node, the connection array is just
       // in sequence starting with 1 (connection array is 1-based)
       elem_conns[i]=i+1;
     }

     // Set regrid conserve flag
     int regridConserve = ESMC_REGRID_CONSERVE_ON;

     // Create elements using internal mesh add elements method
     ESMCI_meshaddelements(meshpp,
                           &num_elems, elem_ids, elemTypes, elemMaskII ,
                           has_elemArea, elemArea,
                           has_elemCoords, elemCoords,
                           &num_elem_conns, elem_conns, &regridConserve,
                           coordSys, &sdim,
                           &localrc);
     if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
       throw localrc;  // bail out with exception

     // Deallocate element info
     if (elem_ids != NULL) delete [] elem_ids;
     if (elem_conns != NULL) delete [] elem_conns;

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

} // meshcreate_easy_elems



