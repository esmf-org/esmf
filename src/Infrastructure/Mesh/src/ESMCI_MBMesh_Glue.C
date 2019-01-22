// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <string>
#include <ostream>
#include <iterator>
#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"

#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_Util.h"
#include "Mesh/include/ESMCI_MBMesh_Glue.h"

#include "MBTagConventions.hpp"
#include "moab/ParallelComm.hpp"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;

// #define DEBUG_MASK
// #define DEBUG_OUTPUT
// #define DEBUG_NODE_COORDS
// #define DEBUG_ELEM_COORDS



void MBMesh_create(void **mbmpp,
                      int *pdim, int *sdim,
                      ESMC_CoordSys_Flag *coordSys, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_create()"

  // Init output
  *mbmpp=NULL;


  // Initialize the parallel environment for mesh (if not already done)
#if 0
     {
       int localrc;
       int rc;
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
          throw localrc;  // bail out with exception
      }
#endif


      // Some error checking of input
     if (*pdim > *sdim) {
       if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- Parametric dimension can't be greater than spatial dimension",
                                        ESMC_CONTEXT, rc)) return;
     }

     if ((*pdim < 2) || (*pdim >3)) {
       if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- Parametric dimension can't be greater than 3D or less than 2D",
                                        ESMC_CONTEXT, rc)) return;
     }

    if ((*sdim < 2) || (*sdim >3)) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- Spatial dimension can't be greater than 3D or less than 2D",
                                       ESMC_CONTEXT, rc)) return;
    }


    // Moab error
    int merr;

    // New Mesh
    MBMesh *mbmp = new MBMesh();

    // Get cartesian dimension
    int cart_sdim;
    int localrc;
    localrc=ESMCI_CoordSys_CalcCartDim(*coordSys, *sdim, &cart_sdim);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,rc))
      return;

    // Create MOAB Mesh
    Interface *moab_mesh=new Core();

    // Default value
    int int_def_val = 0;
    double dbl_def_val[3] = {0.0, 0.0, 0.0};

     // Setup global id tag
    int_def_val=0;
    merr=moab_mesh->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, mbmp->gid_tag, MB_TAG_DENSE, &int_def_val);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
    }

    // Setup orig_pos tag
    int_def_val=-1;
    merr=moab_mesh->tag_get_handle("orig_pos", 1, MB_TYPE_INTEGER, mbmp->orig_pos_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
    }

    // Setup owner tag
    int_def_val=-1;
    merr=moab_mesh->tag_get_handle("owner", 1, MB_TYPE_INTEGER, mbmp->owner_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
    }

    // Setup node_orig_coord tag
    mbmp->has_node_orig_coords=false;
    if (*coordSys != ESMC_COORDSYS_CART) {
      dbl_def_val[0]=dbl_def_val[1]=dbl_def_val[2]=-1.0;
      merr=moab_mesh->tag_get_handle("node_orig_coords", *sdim, MB_TYPE_DOUBLE, mbmp->node_orig_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
      }
      mbmp->has_node_orig_coords=true;
    }

    // Set Moab Mesh
    mbmp->mesh=moab_mesh;

    // Set dimensions
    mbmp->pdim=*pdim;
    mbmp->sdim=cart_sdim;
    mbmp->orig_sdim=*sdim;

    // Output mesh
    *mbmpp=(void *)mbmp;

    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;

} // meshcreate


void MBMesh_addnodes(void **mbmpp, int *num_nodes, int *nodeId,
                     double *nodeCoord, int *nodeOwner, InterArray<int> *nodeMaskII,
                     ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                       int *rc)
{

  // Should we do exception handling in here, since MOAB doesn't???
   try {

     // Get Moab Mesh wrapper
     MBMesh *mbmp=*((MBMesh **)mbmpp);

     //Get MOAB Mesh
     Interface *moab_mesh=mbmp->mesh;

     // Get some handy variables
     ESMC_CoordSys_Flag coordSys=*_coordSys;
     int sdim = mbmp->sdim; // spatial dim of mesh (after conversion to Cartesian)
     int orig_sdim = *_orig_sdim;   // original sdim (before conversion to Cartesian)

     // Moab error
     int merr;

#if 0
     // Initialize the parallel environment for mesh (if not already done)
     {
       int localrc;
       int rc;
       ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
       if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
         throw localrc;  // bail out with exception
     }
#endif

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

    // Number of verts
    int num_verts=*num_nodes;
    mbmp->num_verts=num_verts;

    // Allocate temp storage for verts
    EntityHandle *verts=new EntityHandle[num_verts];
    mbmp->verts=verts; // Put temporarily in structure for use in element creation

     // Create new nodes
    for (int n = 0; n < num_verts; ++n) {
      double cart_coords[3];

      // Init to 0.0 incase less than 3D
      cart_coords[0]=0.0; cart_coords[1]=0.0; cart_coords[2]=0.0;

      // Convert to cartesian
      ESMCI_CoordSys_ConvertToCart(coordSys, orig_sdim,
                                   nodeCoord+orig_sdim*n, cart_coords);
#ifdef DEBUG_NODE_COORDS
      printf("%d# node %d [%f, %f] - [%f,%f,%f]\n", localPet, nodeId[n], nodeCoord[orig_sdim*n+0], nodeCoord[orig_sdim*n+1], cart_coords[0],cart_coords[1],cart_coords[2]);
#endif
      // Add vertex
      merr=moab_mesh->create_vertex(cart_coords,verts[n]);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
          moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }
    }

    // Set Ids
    merr=moab_mesh->tag_set_data(mbmp->gid_tag, verts, num_verts, nodeId);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    // Set Owners
    merr=moab_mesh->tag_set_data(mbmp->owner_tag, verts, num_verts, nodeOwner);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }


    // Set orig_pos
    for (int n = 0; n < num_verts; ++n) {
      merr=moab_mesh->tag_set_data(mbmp->orig_pos_tag, verts+n, 1, &n);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }
    }

    // Set original coords
    if (mbmp->has_node_orig_coords) {
      // Set orinal coords
      merr=moab_mesh->tag_set_data(mbmp->node_orig_coords_tag, verts, num_verts, nodeCoord);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }
    }

    // Set mask information
    mbmp->has_node_mask=false;
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

      // Setup node mask tag
      int int_def_val=0; // So things are by default not masked
      merr=moab_mesh->tag_get_handle("node_mask", 1, MB_TYPE_INTEGER, mbmp->node_mask_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
      }

      // Setup node mask value tag
      int_def_val=0; // So things are by default not masked
      merr=moab_mesh->tag_get_handle("node_mask_val", 1, MB_TYPE_INTEGER, mbmp->node_mask_val_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
      }
      // Set values in node mask value
      merr=moab_mesh->tag_set_data(mbmp->node_mask_val_tag, verts, num_verts, nodeMaskII->array);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Record the fact that it has masks
      mbmp->has_node_mask=true;

#ifdef DEBUG_MASK
      {
        int localrc = 0;
        int merr = 0;

        int node_mask[num_verts];
        if (mbmp->has_node_mask) { 
          Range nodes;
          merr=mbmp->mesh->get_entities_by_dimension(0, 0, nodes);
          if (merr != MB_SUCCESS) throw (ESMC_RC_MOAB_ERROR);
          merr=mbmp->mesh->tag_get_data(mbmp->node_mask_val_tag, nodes, &node_mask);
          if (merr != MB_SUCCESS)
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
              moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        }

        printf("%d# has_node_mask = %s [", Par::Rank(), mbmp->has_node_mask ? "true" : "false");
        for (int i = 0; i < num_verts; ++i)
          printf("%d, ", node_mask[i]);
        printf("]\n");
      }
#endif

    }

#ifdef DEBUG_NODE_COORDS
  {
    
    
    Range nodes;
    merr=mbmp->mesh->get_entities_by_dimension(0, 0, nodes);
    MBMESH_CHECK_ERR(merr, localrc);
    
    for(Range::iterator it=nodes.begin(); it !=nodes.end(); it++) {
      const EntityHandle *node=&(*it);
    
      int nid;
      merr=mbmp->mesh->tag_get_data(mbmp->gid_tag, node, 1, &nid);
      MBMESH_CHECK_ERR(merr, localrc);
      printf("MBMesh_addnodes node %d, [",nid);
    
      double c[3];
      merr = mbmp->mesh->get_coords(node, 1, c);
      MBMESH_CHECK_ERR(merr, localrc);
    
      for (int i=0; i<mbmp->sdim; ++i)
        printf("%f, ", c[i]);
      printf("]\n");
    }
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


//Get the entity type from parametric dimension and ESMF etype
EntityType get_entity_type(int pdim, int etype) {
  if (pdim==2) {
    if (etype==3) return MBTRI;
    else if (etype==4) return MBQUAD;
    else {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                       "- unrecognized ESMF element type",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
  } else if (pdim==3) {
    if (etype==10) return MBTET;
    else if (etype==12) return MBHEX;
    else {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                       "- unrecognized ESMF element type",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
  }
}


// TODO: Put this someplace to share with other GLUE code
// Get the number of nodes from the element type
 // Get the element topology
int ElemType2NumNodes(int pdim, int sdim, int etype) {
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

// TODO: Put this someplace to share with other GLUE code
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


void MBMesh_addelements(void **mbmpp,
                        int *_num_elems, int *elemId, 
                        int *elemType, InterArray<int> *_elemMaskII ,
                        int *_areaPresent, double *elemArea,
                        int *_coordsPresent, double *elemCoords,
                        int *_num_elemConn, int *elemConn, int *regridConserve,
                        ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                        int *rc)
{

  /* XMRKX */

   try {

#if 0

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }
#endif

    // local rc code
    int localrc;

    // Do this for now instead of initing mesh parallel stuff
    // TODO: MAYBE EVENTUALLY PUT THIS INTO MBMesh???
    MPI_Comm mpi_comm;
    {
    int localrc;
    int rc;
      mpi_comm=VM::getCurrent(&localrc)->getMpi_c();
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
    }


    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // MOAB error return
    int merr;

     // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    int num_elems=*_num_elems;

    int num_elemConn=*_num_elemConn;

    InterArray<int> *elemMaskII=_elemMaskII;

    int areaPresent=*_areaPresent;

    int elemCoordsPresent=*_coordsPresent;

    ESMC_CoordSys_Flag coordSys=*_coordSys;
    int orig_sdim = *_orig_sdim;   // original sdim (before conversion to Cartesian)

    // Get parametric dimension
    int parametric_dim=mbmp->pdim;

    // Error check input
    //// Check element type
     if (parametric_dim==2) {
      // DONT DO THE CHECK BECAUSE WE NOW SUPPORT
      // ANY NUMBER OF CORNERS WITH PDIM=2
#if 0
      for (int i=0; i< num_elems; i++) {
        if ((elemType[i] != 5) && (elemType[i] != 9)) {
          int localrc;
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
            "- for a mesh with parametric dimension 2 element types must be either triangles or quadrilaterals ",
                                           ESMC_CONTEXT, &localrc)) throw localrc;
          }
      }
#endif
    } else if (parametric_dim==3) {
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

    // Register element tags
    int     int_def_val=-1.0;
    double  dbl_def_val[3]= {0.0, 0.0, 0.0};
    double  dbl_def_val_one= 0.0;

    //// Register the frac field
    mbmp->has_elem_frac=false;
    if (*regridConserve == ESMC_REGRID_CONSERVE_ON) {

      merr=moab_mesh->tag_get_handle("elem_frac", 1, MB_TYPE_DOUBLE, mbmp->elem_frac_tag, MB_TAG_EXCL|MB_TAG_DENSE, &dbl_def_val_one);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
      }

      mbmp->has_elem_frac=true;
    }

    // Handle element masking
    mbmp->has_elem_mask=false;
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

      // Setup elem mask tag
      int_def_val=0; // So things are by default not masked
      merr=moab_mesh->tag_get_handle("elem_mask", 1, MB_TYPE_INTEGER, mbmp->elem_mask_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
      }

      // Setup elem mask value tag
      int_def_val=0; // So things are by default not masked
      merr=moab_mesh->tag_get_handle("elem_mask_val", 1, MB_TYPE_INTEGER, mbmp->elem_mask_val_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
      }

      // Record the fact that it has masks
      mbmp->has_elem_mask=true;
    }

    // Handle element area
    mbmp->has_elem_area=false;
    if (areaPresent == 1) { // if areas exist

      merr=moab_mesh->tag_get_handle("elem_area", 1, MB_TYPE_DOUBLE, mbmp->elem_area_tag, MB_TAG_EXCL|MB_TAG_DENSE, &dbl_def_val_one);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
      }

      // Record the fact that it has masks
      mbmp->has_elem_area=true;
    }

    // Handle element coords
    mbmp->has_elem_coords=false;
    mbmp->has_elem_orig_coords=false;
    if (elemCoordsPresent == 1) { // if coords exist

      // Add element coords field
      merr=moab_mesh->tag_get_handle("elem_coords", mbmp->sdim, MB_TYPE_DOUBLE, mbmp->elem_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
       if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
      }

      // If not cartesian then add original coordinates field
      if (coordSys != ESMC_COORDSYS_CART) {
        merr=moab_mesh->tag_get_handle("elem_orig_coords", orig_sdim, MB_TYPE_DOUBLE, mbmp->elem_orig_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
        if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                           moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
        }

        // Record the fact that it has original elem coords
        mbmp->has_elem_orig_coords=true;
      }


      // Record the fact that it has elem coords
      mbmp->has_elem_coords=true;
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
      MPI_Allreduce(&num_extra_elem,&tot_num_extra_elem,1,MPI_INT,MPI_SUM,mpi_comm);

      // If there's num_extra_elem than it's a split mesh
      if (tot_num_extra_elem>0) {
        mbmp->is_split=true;
       } else {
        mbmp->is_split=false;
      }
    } else {
      mbmp->is_split=false;
    }

    // Compute the extra element ranges
    int beg_extra_ids=0;
    if (mbmp->is_split) {
      // get maximum local elem id
       int max_id=0;
      for (int e = 0; e < num_elems; ++e) {
        if (elemId[e] > max_id) {
          max_id=elemId[e];
         }
      }

      // Calc global max id
      int global_max_id=0;
      MPI_Allreduce(&max_id,&global_max_id,1,MPI_INT,MPI_MAX,mpi_comm);

      // Set maximum of non-split ids
      mbmp->max_non_split_id=global_max_id;

      // Calc our range of extra elem ids
      beg_extra_ids=0;
      MPI_Scan(&num_extra_elem,&beg_extra_ids,1,MPI_INT,MPI_SUM,mpi_comm);

      // Remove this processor's number from the sum to get the beginning
       beg_extra_ids=beg_extra_ids-num_extra_elem;

      // Start 1 up from max
      beg_extra_ids=beg_extra_ids+global_max_id+1;

      // printf("%d# beg_extra_ids=%d end=%d\n",Par::Rank(),beg_extra_ids,beg_extra_ids+num_extra_elem-1);
    }

#if 0
    // Don't currently support split elements with element coords
    if (mbmp->is_split && elemCoordsPresent) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- ESMF doesn't currently support both element coords and polygons with >4 sides in the same Mesh.",
             ESMC_CONTEXT, &localrc)) throw localrc;
    }
#endif

    // Get number of verts
    int num_verts = mbmp->num_verts;

    // Allocate array to make sure that there are no local nodes without a home
    std::vector<int> node_used;
    node_used.resize(num_verts, 0);


    // Error check elemConn array
    int c = 0;
    for (int e = 0; e < num_elems; ++e) {
      int nnodes = ElemType2NumNodes(mbmp->pdim,
                                     mbmp->sdim,
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

        if (node_index > num_verts-1) {
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
    for (int i=0; i<num_verts; i++) {
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
      int sdim = mbmp->sdim;

      // Loop through elems generating split elems if necessary
      int conn_pos = 0;
       int split_conn_pos = 0;
      int split_elem_pos = 0;
      for (int e = 0; e < num_elems; ++e) {

        // More than 4 side, split
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
          for (int i=0; i<elemType[e]; i++) {
            double coords[3];

            EntityHandle *vert=mbmp->verts+elemConn[conn_pos+i]-1;
            merr=moab_mesh->get_coords(vert,1,coords);
            if (merr != MB_SUCCESS) {
              if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
            }

            for (int j=0; j<sdim; j++) {
              subelem_coords[crd_pos]=coords[j];
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
               mbmp->split_to_orig_id[curr_extra_id]=elemId[e]; // Store map of split to original id
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
               mbmp->split_id_to_frac[elemId_wsplit[sep]]=frac;

               // Set area to fraction of original area
               if (areaPresent==1) elemArea_wsplit[sep]=elemArea[e]*frac;
             }
           } else {
             for (int i=0; i<num_elemtris; i++) {
               double frac=elemtris_area[i]/tot_elemtris_area;
               int sep=elemtris_split_elem_pos[i];

               // Add frac to mesh split information
               mbmp->split_id_to_frac[elemId_wsplit[sep]]=0.0;

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


      // Get number of nodes in element
      int num_elem_verts=ElemType2NumNodes(mbmp->pdim,
                                           mbmp->sdim,
                                           elemType[e]);

      // Define the maximum number of verts
#define MAX_ELEM_VERTS 20
       if (num_elem_verts >MAX_ELEM_VERTS) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- element contains more nodes than are currently supported ",
                                         ESMC_CONTEXT, &localrc)) throw localrc;
      }

      // Connectivity array
      EntityHandle elem_verts[MAX_ELEM_VERTS];
#undef MAX_ELEM_VERTS

      // Fill the connectivity array
      for (int n = 0; n < num_elem_verts; ++n) {

        // Get 0-based vert index
        int vert_index=elemConn[cur_conn]-1;

        // Setup connectivity list
        elem_verts[n] = mbmp->verts[vert_index];

        // Advance to next
        cur_conn++;
      }

      // Get number of nodes in element
      EntityType etype=get_entity_type(mbmp->pdim,
                                       elemType[e]);

      EntityHandle new_elem;
      merr=moab_mesh->create_element(etype,elem_verts,num_elem_verts,new_elem);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
          moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

       // Set global id
      merr=moab_mesh->tag_set_data(mbmp->gid_tag, &new_elem, 1, elemId+e);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Set Position
      merr=moab_mesh->tag_set_data(mbmp->orig_pos_tag, &new_elem, 1, &e);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Set Owner to the current pet
      merr=moab_mesh->tag_set_data(mbmp->owner_tag, &new_elem, 1, &localPet);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }


      // Set elem mask value
      if (mbmp->has_elem_mask) {
        merr=moab_mesh->tag_set_data(mbmp->elem_mask_val_tag, &new_elem, 1,
                                     elemMaskII->array+e);
        if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                          moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        }
      }

      // Set elem area
      if (mbmp->has_elem_area) {
        merr=moab_mesh->tag_set_data(mbmp->elem_area_tag, &new_elem, 1,
                                     elemArea+e);
        if (merr != MB_SUCCESS) {
           if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                          moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        }
      }

      // Set elem coord
      if (mbmp->has_elem_coords) {
        // Depending on coordSys save coordinates
        if (coordSys == ESMC_COORDSYS_CART) {
          merr=moab_mesh->tag_set_data(mbmp->elem_coords_tag, &new_elem, 1,
                                       elemCoords+mbmp->sdim*e);
          if (merr != MB_SUCCESS) {
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
          }
        } else {
          // Save original coords
          merr=moab_mesh->tag_set_data(mbmp->elem_orig_coords_tag, &new_elem, 1,
                                       elemCoords+orig_sdim*e);
          if (merr != MB_SUCCESS) {
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
          }

          // Convert to Cartesian coords
          double cart_coords[3]; // 7 is the maximum dimension of ESMF grids

          // Init to 0.0 incase less than 3D
          cart_coords[0]=0.0; cart_coords[1]=0.0; cart_coords[2]=0.0;

          ESMCI_CoordSys_ConvertToCart(coordSys, orig_sdim,
                                       elemCoords+orig_sdim*e, cart_coords);
#ifdef DEBUG_ELEM_COORDS
      printf("%d# elem %d [%f, %f] - [%f,%f,%f]\n", localPet, elemId[e], elemCoords[orig_sdim*e+0], elemCoords[orig_sdim*e+1], cart_coords[0],cart_coords[1],cart_coords[2]);
#endif
          // Save Cartesian coords
          merr=moab_mesh->tag_set_data(mbmp->elem_coords_tag, &new_elem, 1,
                                       cart_coords);
          if (merr != MB_SUCCESS) {
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
          }

        }
      }


#if 0
      // Set elem coords in the current elem
      merr=moab_mesh->tag_set_data(mbmp->elem_coords_tag, &new_elem, 1, elem_coords);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }
#endif

    } // for e

    // Set number of local elems
    mbmp->num_elems=num_elems;

    //// Setup parallel sharing ///

    // setup parallel comm, destroyed in MBMesh destructor
    ParallelComm *pcomm= new ParallelComm(moab_mesh, mpi_comm);
  
    Range elems;
    merr=mbmp->mesh->get_entities_by_dimension(0, mbmp->pdim, elems);
    MBMESH_CHECK_ERR(merr, localrc);
    
    // Resolve object sharing like in Mesh->Commit()
    // merr = pcomm->resolve_shared_ents(0, mbmp->pdim, mbmp->pdim-1);
    merr = pcomm->resolve_shared_ents(0, elems, mbmp->pdim, mbmp->pdim-1);
    MBMESH_CHECK_ERR(merr, localrc);


#ifdef DEBUG_ELEM_COORDS
  {
    Range elems;
    merr=mbmp->mesh->get_entities_by_dimension(0, mbmp->pdim, elems);
    MBMESH_CHECK_ERR(merr, localrc);
    
    for(Range::iterator it=elems.begin(); it !=elems.end(); it++) {
      const EntityHandle *elem=&(*it);
    
      int eid;
      merr=mbmp->mesh->tag_get_data(mbmp->gid_tag, elem, 1, &eid);
      MBMESH_CHECK_ERR(merr, localrc);
      printf("getelems elem %d, [",eid);
    
      double c[3];
      merr=mbmp->mesh->tag_get_data(mbmp->elem_coords_tag, elem, 1, c);
      MBMESH_CHECK_ERR(merr, localrc);
    
      for (int i=0; i<mbmp->sdim; ++i)
        printf("%f, ", c[i]);
      printf("]\n");
    }
  }
#endif

#ifdef DEBUG_MASK
      {
        int localrc = 0;
        int merr = 0;

        int node_mask[num_verts];
        if (mbmp->has_node_mask) { 
          Range nodes;
          merr=mbmp->mesh->get_entities_by_dimension(0, 0, nodes);
          if (merr != MB_SUCCESS) throw (ESMC_RC_MOAB_ERROR);
          merr=mbmp->mesh->tag_get_data(mbmp->node_mask_val_tag, nodes, &node_mask);
          if (merr != MB_SUCCESS)
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
              moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        }

        printf("%d# has_node_mask = %s [", Par::Rank(), mbmp->has_node_mask ? "true" : "false");
        for (int i = 0; i < num_verts; ++i)
          printf("%d, ", node_mask[i]);
        printf("]\n");
      }
#endif

 #if 0
  // Time loops
  {
   /* XMRKX */
    double beg_tm=MPI_Wtime();

    // Get a range containing all elements
    Range all_elem;
    merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,all_elem);

    // Put into vector
    for(Range::iterator it=all_elem.begin(); it !=all_elem.end(); it++) {
      const EntityHandle eh=*it;

    }

    double end_tm=MPI_Wtime();

    printf("MOAB time to loop through elems=%f\n",end_tm-beg_tm);

  }
#endif

#if 0

  // Perhaps commit will be a separate call, but for now commit the mesh here.
  mesh.build_sym_comm_rel(MeshObj::NODE);
  mesh.Commit();


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



 /* XMRKX */
  }
#endif

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

void MBMesh_turnonnodemask(void **mbmpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {

  int merr, localrc;

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
    }

    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;


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

    // If has masks
    if (mbmp->has_node_mask) {

      // Get a range containing all nodes
      Range range_node;
      merr=moab_mesh->get_entities_by_dimension(0,0,range_node);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Loop through elements setting values
      for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
        const EntityHandle node=*it;

        // Get mask value
        int mv;
        merr=moab_mesh->tag_get_data(mbmp->node_mask_val_tag, &node, 1, &mv);
        if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        }

        // See if mv matches any mask values
        int masked=0;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];
          if (mv==mvi) {
            masked=1;
            break;
          }
        }

        // Set global id
        merr=moab_mesh->tag_set_data(mbmp->node_mask_tag, &node, 1, &masked);
         if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
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
 void MBMesh_turnoffnodemask(void **mbmpp, int *rc) {

  int merr, localrc;

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
    }

    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // If has masks
    if (mbmp->has_node_mask) {
      // Get a range containing all nodes
      Range range_node;
      merr=moab_mesh->get_entities_by_dimension(0,0,range_node);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Loop through elements setting values
      for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
        const EntityHandle node=*it;

        // unset masked value
        int masked=0;
        merr=moab_mesh->tag_set_data(mbmp->node_mask_tag, &node, 1, &masked);
        if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
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

void MBMesh_turnonelemmask(void **mbmpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {

  int merr, localrc;

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
    }

    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;


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

    // If has masks
    if (mbmp->has_elem_mask) {

      // Get a range containing all elements
      Range range_elem;
      merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Loop through elements setting values
      for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
        const EntityHandle elem=*it;

        // Get mask value
        int mv;
        merr=moab_mesh->tag_get_data(mbmp->elem_mask_val_tag, &elem, 1, &mv);
        if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        }

        // See if mv matches any mask values
        int masked=0;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];
          if (mv==mvi) {
            masked=1;
            break;
          }
        }

        // Set global id
        merr=moab_mesh->tag_set_data(mbmp->elem_mask_tag, &elem, 1, &masked);
         if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
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
 void MBMesh_turnoffelemmask(void **mbmpp, int *rc) {

  int merr, localrc;

  try {

    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
    }

    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // If has masks
    if (mbmp->has_elem_mask) {
      // Get a range containing all elements
      Range range_elem;
      merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Loop through elements setting values
      for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
        const EntityHandle elem=*it;

        // unset masked value
        int masked=0;
        merr=moab_mesh->tag_set_data(mbmp->elem_mask_tag, &elem, 1, &masked);
        if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                    moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
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



void MBMesh_destroy(void **mbmpp, int *rc) {

  try {

  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }

    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    // get the indexed pcomm object from the interface
    ParallelComm *pcomm = ParallelComm::get_pcomm(mbmp->mesh, 0);

    delete pcomm;

    // Get rid of MBMesh
    delete mbmp;

    // Set to null
    *mbmpp=NULL;

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



void MBMesh_write(void **mbmpp, char *fname, int *rc,
    ESMCI_FortranStrLenArg nlen) {

 #if 0
  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }
#endif

    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Make c format string
    char *filename = ESMC_F90toCstring(fname, nlen);

 /* XMRKX */
    // Add vtk
#define FILENAME_W_VTK_MAX 1024
    char filename_w_vtk[FILENAME_W_VTK_MAX];
    if (strlen(filename)+4 > FILENAME_W_VTK_MAX) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
                                     " filename too long ", ESMC_CONTEXT, rc);
      return;
    }
#undef FILENAME_W_VTK_MAX
    sprintf(filename_w_vtk,"%s.vtk",filename);

    // Call into MOAB
    int merr=moab_mesh->write_file(filename_w_vtk,NULL,NULL);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT, rc)) return;
    }

    // get rid of c format string
    delete [] filename;

    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;


 }


extern "C" void FTN_X(f_esmf_getmeshdistgrid)(int*, int*, int*, int*);


void MBMesh_createnodedistgrid(void **mbmpp, int *ngrid, int *num_lnodes, int *rc) {

#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createnodedistgrid()"

  // Declare id vectors
  std::vector<int> ngids;

  try {

    // Get localPet
    int localrc;
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      throw localrc;  // bail out with exception

    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // MOAB error
    int merr;

    // Get a range containing all nodes
    Range range_node;
    merr=mbmp->mesh->get_entities_by_dimension(0,0,range_node);
    MBMESH_CHECK_ERR(merr, localrc);

    for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
      const EntityHandle *node=&(*it);

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mbmp->owner_tag, node, 1, &owner);
      MBMESH_CHECK_ERR(merr, localrc);

      // If owned by this processor, put in list
      if (owner==localPet) {
        // Get gid
        int gid;
        merr=moab_mesh->tag_get_data(mbmp->gid_tag, node, 1, &gid);
        MBMESH_CHECK_ERR(merr, localrc);

        // Stick in list
        ngids.push_back(gid);
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

 /* XMRKX */
/**
 * Sort elements by the order in which they were originally declared
 * (which is stored by get_data_index)
 * Don't include split elements
 */

// DO THIS BETTER, HAVE A FIELD THAT CONTAINS THE POSITION IN THE FINAL ARRAY AND -1 FOR ANYTHING NOT LOCAL OR SPLIT
void getElemGIDS(MBMesh *mbmp, std::vector<int> &egids) {

  // Get localPet
  int localrc;
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;

  // Get a range containing all elements
  Range range_elem;
  merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Loop through elements putting into list
  std::vector<std::pair<int,int> > pos_and_gids;
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle *elemp=(&*it);

    // Get owner
    int owner;
    merr=moab_mesh->tag_get_data(mbmp->owner_tag, elemp, 1, &owner);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    // If owned by this processor, put in list
    if (owner==localPet) {
      // Get gid
      int gid;
      merr=moab_mesh->tag_get_data(mbmp->gid_tag, elemp, 1, &gid);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Don't do split elements
      if (mbmp->is_split && gid > mbmp->max_non_split_id) continue;

      // Get orig_pos
      int orig_pos;
      merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, elemp, 1, &orig_pos);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Stick in list
      pos_and_gids.push_back(std::make_pair(orig_pos,gid));
    }
  }

  // Put in order by original pos
  std::sort(pos_and_gids.begin(), pos_and_gids.end());

  // Fill array of element gids
  egids.clear();
  for (int i = 0; i<pos_and_gids.size(); ++i) {
    egids.push_back(pos_and_gids[i].second);

    // printf("pos=%d egids=%d\n",pos_and_gids[i].first,pos_and_gids[i].second);

  }

}


void MBMesh_createelemdistgrid(void **mbmpp, int *egrid, int *num_lelems, int *rc) {

  // Declare id vectors
  std::vector<int> egids;

  try {

#if 0
  // Initialize the parallel environment for mesh (if not already done)
    {
 int localrc;
 int rc;
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
 if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
   throw localrc;  // bail out with exception
    }
#endif

    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    // Get list of elem gids in order
    getElemGIDS(mbmp, egids);

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


// DO THIS BETTER, HAVE A FIELD THAT CONTAINS THE POSITION IN THE FINAL ARRAY AND -1 FOR ANYTHING NOT LOCAL OR SPLIT
void getElems(void **mbmpp, std::vector<EntityHandle> &ehs) {

  // Get localPet
  int localrc;
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Get Moab Mesh wrapper
  MBMesh *mbmp=*((MBMesh **)mbmpp);

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;


  // Get a range containing all elements
  Range range_elem;
  merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Loop through elements putting into list
  std::vector<std::pair<int,EntityHandle> > pos_and_elems;
  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle *elemp=(&*it);
    EntityHandle elem=*it;

    // Get owner
    int owner;
    merr=moab_mesh->tag_get_data(mbmp->owner_tag, elemp, 1, &owner);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    // If owned by this processor, put in list
    if (owner==localPet) {
      // Get gid
      int gid;
      merr=moab_mesh->tag_get_data(mbmp->gid_tag, elemp, 1, &gid);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Get orig_pos
      int orig_pos;
      merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, elemp, 1, &orig_pos);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Stick in list
      pos_and_elems.push_back(std::make_pair(orig_pos,elem));
    }
  }

  // Put in order by original pos
  std::sort(pos_and_elems.begin(), pos_and_elems.end());

  // Fill array of element gids
  ehs.clear();
  for (int i = 0; i<pos_and_elems.size(); ++i) {
    ehs.push_back(pos_and_elems[i].second);

    // printf("pos=%d egids=%d\n",pos_and_gids[i].first,pos_and_gids[i].second);

  }
}


void MBMesh_getlocalelemcoords(void **mbmpp, double *ecoords,
                               int *_orig_sdim, int *rc)
{
  int localrc,merr;
    try {

      // Get Moab Mesh wrapper
      MBMesh *mbmp=*((MBMesh **)mbmpp);

      // Make sure that there are element coords
      if (!mbmp->has_elem_coords) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                         "- this mesh doesn't contain element coordinates.",
                                         ESMC_CONTEXT, &localrc)) throw localrc;
      }

      //Get MOAB Mesh
      Interface *moab_mesh=mbmp->mesh;

      // Get original spatial dim
      int orig_sdim=*_orig_sdim;

      // Declare id vector
      std::vector<EntityHandle> ehs;

      // Get local elems in correct order
      getElems(mbmpp, ehs);

      // Loop through elements and put coordss into array
      if (mbmp->has_elem_orig_coords) {
        for (int i=0; i<ehs.size(); i++) {
          // Get element gid
          EntityHandle elem=ehs[i];

          // Get orig_pos
          merr=moab_mesh->tag_get_data(mbmp->elem_orig_coords_tag,
                                       &elem, 1, ecoords+orig_sdim*i);
          if (merr != MB_SUCCESS) {
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                             moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
          }
        }
      } else {
        for (int i=0; i<ehs.size(); i++) {
          // Get element gid
          EntityHandle elem=ehs[i];

          // Get orig_pos
          merr=moab_mesh->tag_get_data(mbmp->elem_coords_tag,
                                       &elem, 1, ecoords+orig_sdim*i);
          if (merr != MB_SUCCESS) {
            if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                             moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
          }
        }
      }

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


void MBMesh_getarea(void **mbmpp, int *num_elem, double *elem_areas, int *rc) {

  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];
  double tmp_coords[MAX_NUM_POLY_COORDS];


  try {

#if 0
    // Initialize the parallel environment for mesh (if not already done)
    {
      int localrc;
      ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
      if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
        throw localrc;  // bail out with exception
    }
#endif

    // Declare id vector
    std::vector<EntityHandle> ehs;

    // get elem ids
    // TODO: IN FUTURE MAYBE JUST USE DATA INDEX DIRECTY, ALTHOUGH THEN HAVE TO STICK
    //       SPLIT ELEMENTS AT END
    getElems(mbmpp, ehs);


    // If there are no elements then leave
    if (ehs.empty()) {
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return;
    }

    // Check size
    if (*num_elem != ehs.size()) {
      Throw() << "Number of elements doesn't match size of input array for areas";
    }


    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // MOAB error
    int merr;


    // Get dimensions
    int sdim=mbmp->sdim;
    int pdim=mbmp->pdim;


    // Put areas into array
    if (mbmp->has_elem_area) {

      merr=moab_mesh->tag_get_data(mbmp->elem_area_tag, &ehs[0], ehs.size(), elem_areas);
      if (merr != MB_SUCCESS) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

    } else {
      for (int i=0; i<ehs.size(); i++) {

        // Get element
        EntityHandle elem=ehs[i];

        // Compute area depending on dimensions
        double area;
        if (pdim==2) {
          if (sdim==2) {
            MBMesh_get_elem_coords(mbmp, elem, MAX_NUM_POLY_NODES_2D, &num_poly_nodes, poly_coords);
            remove_0len_edges2D(&num_poly_nodes, poly_coords);
            area=area_of_flat_2D_polygon(num_poly_nodes, poly_coords);
          } else if (sdim==3) {
            MBMesh_get_elem_coords(mbmp, elem, MAX_NUM_POLY_NODES_3D, &num_poly_nodes, poly_coords);
            //get_elem_coords_3D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_3D, tmp_coords, &num_poly_nodes, poly_coords);
            remove_0len_edges3D(&num_poly_nodes, poly_coords);
            area=great_circle_area(num_poly_nodes, poly_coords);
          }
        } else if (pdim==3) {
          if (sdim==3) {
            Throw() << "Meshes with parametric dimension == 3 and spatial dimension = 3 not supported in MOAB";
            // Phedra tmp_phedra=create_phedra_from_elem(&elem, cfield);
            //area=tmp_phedra.calc_volume();
          } else {
            Throw() << "Meshes with parametric dimension == 3, but spatial dim != 3 not supported for computing areas";
          }
        } else {
          Throw() << "Meshes with parametric dimension != 2 or 3 not supported for computing areas";
        }

        // Put area into area array
        elem_areas[i]=area;
      }
    }



#if 0
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


  // Declare polygon information
#undef  MAX_NUM_POLY_COORDS
#undef  MAX_NUM_POLY_NODES_2D
#undef  MAX_NUM_POLY_NODES_3D

}

// DO THIS BETTER, HAVE A FIELD THAT CONTAINS THE POSITION IN THE FINAL ARRAY AND -1 FOR ANYTHING NOT LOCAL OR SPLIT
void getNodes(void **mbmpp, std::vector<EntityHandle> &nodes) {

  // Get localPet
  int localrc;
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Get Moab Mesh wrapper
  MBMesh *mbmp=*((MBMesh **)mbmpp);

  //Get MOAB Mesh
  Interface *moab_mesh=mbmp->mesh;

  // MOAB error
  int merr;


  // Get a range containing all nodes
  Range range_node;
  merr=moab_mesh->get_entities_by_dimension(0,0,range_node);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Loop through nodes putting into list
  std::vector<std::pair<int,EntityHandle> > pos_and_nodes;
  for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
    const EntityHandle *nodep=(&*it);
    EntityHandle node=*it;

    // Get owner
    int owner;
    merr=moab_mesh->tag_get_data(mbmp->owner_tag, nodep, 1, &owner);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

    // If owned by this processor, put in list
    if (owner==localPet) {
      // Get gid
      int gid;
      merr=moab_mesh->tag_get_data(mbmp->gid_tag, nodep, 1, &gid);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Get orig_pos
      int orig_pos;
      merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, nodep, 1, &orig_pos);
      if (merr != MB_SUCCESS) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                         moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }

      // Stick in list
      pos_and_nodes.push_back(std::make_pair(orig_pos,node));
    }
  }

  // Put in order by original pos
  std::sort(pos_and_nodes.begin(), pos_and_nodes.end());

  // Fill array of node entities
  nodes.clear();
  for (int i = 0; i<pos_and_nodes.size(); ++i) {
    nodes.push_back(pos_and_nodes[i].second);

    // printf("pos=%d ngids=%d\n",pos_and_gids[i].first,pos_and_gids[i].second);

  }
}

void MBMesh_getlocalcoords(void **mbmpp, double *ncoords, int *_orig_sdim, int *rc)
{
  try {
    // Get Moab Mesh wrapper
    MBMesh *mbmp=*((MBMesh **)mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // MOAB error
    int merr;
    int localrc;

    // Get dimensions
    int sdim=mbmp->sdim;
    int pdim=mbmp->pdim;


    // Get original spatial dim
    int orig_sdim=*_orig_sdim;

    // Declare id vector
    std::vector<EntityHandle> nodes;

    // Get local nodes in correct order
    getNodes(mbmpp, nodes);

    // Loop through nodes and put coords into array
    if (mbmp->has_node_orig_coords) {
      for (int i=0; i<nodes.size(); i++) {
        // Get node
        EntityHandle node=nodes[i];

        // Get coords
        merr=moab_mesh->tag_get_data(mbmp->node_orig_coords_tag,
                                     &node, 1, ncoords+orig_sdim*i);
        if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        }
      }
    } else {
      for (int i=0; i<nodes.size(); i++) {
        // Get node
        EntityHandle node=nodes[i];

        // Get coords
        merr=moab_mesh->get_coords(&node, 1, ncoords+orig_sdim*i);
        if (merr != MB_SUCCESS) {
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                           moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
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




 /* XMRKX */
#if 0

/**
 * Routines for reading in a test VTK mesh to fortran arrays (for testing the array interface)
 */
void ESMCI_meshvtkheader(char *filename, int *num_elem, int *num_node, int *conn_size, int *rc,
    ESMCI_FortranStrLenArg nlen) {

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


void ESMCI_meshget(Mesh **meshpp, int *num_nodes, int *num_elements, int *rc){

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


void ESMCI_meshcreateelemdistgrid(Mesh **meshpp, int *egrid, int *num_lelems, int *rc) {

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
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *localrc,
                ESMCI_FortranStrLenArg buffer_l){

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > vars.
    int size = sizeof(int);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < size) {
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add a Mesh object", ESMC_CONTEXT, localrc);
         return;
      }
    }

    // Save keyCount
    ip= (int *)(buffer + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = *intMeshFreed;
    }

    // Adjust offset
    *offset += size;

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
}


void ESMCI_meshinfodeserialize(int *intMeshFreed,
                                 char *buffer, int *offset, int *localrc,
                                 ESMCI_FortranStrLenArg buffer_l){

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // Get pointer
    ip= (int *)(buffer + *offset);

    // Get values
    *intMeshFreed=*ip++;

    // Adjust offset
    *offset += sizeof(int);

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
}


void ESMCI_meshserialize(Mesh **meshpp,
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *rc,
                ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshserialize()"

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
#define ESMF_RECONCILE_MESH_NUM_FIELDS 8
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

    // DEBUG OUTPUT
    // for (int i=0; i<ESMF_RECONCILE_MESH_NUM_FIELDS; i++) {
    //  printf("%d# S: %d fields_present=%d\n",Par::Rank(),i,fields_present[i]);
    //}


    // Calc Size
    int size = 5*sizeof(int)+
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
      if (nvalSetSizes != NULL)
        for (int i=0; i<numSets; i++) {
          *uip++=nvalSetSizes[i];
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
      if (nvalSetObjSizes != NULL)
        for (int i=0; i<numSets; i++) {
          *uip++=nvalSetObjSizes[i];
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
#define ESMC_METHOD "c_esmc_meshdeserialize()"

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
      *offset += 5*sizeof(int)+ESMF_RECONCILE_MESH_NUM_FIELDS*sizeof(int)+
      nvalSetSizes.size()*sizeof(UInt)+nvalSetVals.size()*sizeof(UInt)+
      nvalSetObjSizes.size()*sizeof(UInt)+nvalSetObjVals.size()*sizeof(UInt);


    // Create Mesh
    Mesh *meshp = new Mesh();

    // Set dimensions
    meshp->set_spatial_dimension(spatial_dim);
    meshp->set_parametric_dimension(parametric_dim);

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

////////////////


void ESMCI_meshgetarea(Mesh **meshpp, int *num_elem, double *elem_areas, int *rc) {


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
          " - clockwise polygons not supported in triangulation routine",
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

#if 0
void ESMCI_meshturnoncellmask(Mesh **meshpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {

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

#endif

////////////
void ESMCI_get_polygon_area(int *spatialdim, int *nedges,
                                                 double *points, double *area, int *rc) {
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
#define ESMC_METHOD "c_esmc_meshcreateredistelems()"

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
#define ESMC_METHOD "c_esmc_meshcreateredistelems()"

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
#define ESMC_METHOD "c_esmc_meshcreateredist()"

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
#define ESMC_METHOD "c_esmc_meshchecknodelist()"

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
#define ESMC_METHOD "c_esmc_meshcheckelemlist()"

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
#define ESMC_METHOD "c_esmc_meshchecknodelist()"

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
#define ESMC_METHOD "c_esmc_meshcreatedual()"

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


#endif

#endif // ESMF_MOAB
