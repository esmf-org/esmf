// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
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

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_Glue.h"
#include "Mesh/include/ESMCI_MBMesh_Redist.h"
#include "Mesh/include/ESMCI_MBMesh_Util.h"


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
// #define DEBUG_OWNED


extern "C" void FTN_X(f_esmf_getmeshdistgrid)(int*, int*, int*, int*);

void MBMesh_create(MBMesh **mbmpp, int *pdim, int *sdim,
                   ESMC_CoordSys_Flag *coordSys, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_create()"
  try {
    int localrc, merr;
    
    // Init output
    *mbmpp=NULL;
    
    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    
    //  Error check input //
    if (*pdim > *sdim) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                       " Parametric dimension can't be greater than spatial dimension",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
    
    if ((*pdim < 2) || (*pdim >3)) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                       " Parametric dimension can't be greater than 3D or less than 2D",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
    
    if ((*sdim < 2) || (*sdim >3)) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                       " Spatial dimension can't be greater than 3D or less than 2D",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }

    
    // Create new Mesh
    *mbmpp = new MBMesh(*pdim, *sdim, *coordSys);
    
   
  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  
} // meshcreate


void MBMesh_addnodes(MBMesh **mbmpp, int *_num_nodes, int *nodeId,
                     double *nodeCoord, int *nodeOwner, InterArray<int> *nodeMaskII,
                     ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                     int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_addnodes()"
  try {
    int localrc, merr;
    
    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);


    //// Setup some handy info ////
    
    // Get Moab Mesh wrapper
    ThrowRequire(mbmpp != NULL); 
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    // Get number of nodes
    int num_nodes=*_num_nodes;

    // Get petCount 
    int petCount = VM::getCurrent(&localrc)->getPetCount();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    //// Error check input ////

    // Check node owners
    for (int n = 0; n < num_nodes; ++n) {
      if ((nodeOwner[n]<0) || (nodeOwner[n]>petCount-1)) {
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                         "- Bad nodeOwner value ", ESMC_CONTEXT,&localrc)) throw localrc;
      }
    }

    // If present, check mask information
    if (present(nodeMaskII)) { // if masks exist
      // Error checking
      if ((nodeMaskII)->dimCount !=1) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
                                         "- nodeMask array must be 1D ", ESMC_CONTEXT,  &localrc)) throw localrc;
      }
      
      if ((nodeMaskII)->extent[0] != num_nodes) {
        int localrc;
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
                                         "- nodeMask array must be the same size as the nodeIds array ", ESMC_CONTEXT, &localrc)) throw localrc;
      }
    }


    //// Add nodes ////

    // Create a new set of nodes with basic info
    Range added_nodes;
    mbmp->add_nodes(num_nodes,     
                    nodeCoord,
                    nodeId,          
                    NULL,  // Just use orig_pos starting from 0      
                    nodeOwner,
                    added_nodes); 
    

    // Done creating nodes, so finalize
    mbmp->finalize_nodes();

    //// Setup and fill other node fields ////

    // Setup node mask information
    if (present(nodeMaskII)) { // if masks exist
      
      // Turn on node masks
      mbmp->setup_node_mask();
      
      // Set values in node mask value
      mbmp->set_node_mask_val(added_nodes, nodeMaskII->array);
    }


    //// Debug output //// 
#ifdef ESMF_MBMESH_DEBUG_CREATE_OUTPUT_NODES
    mbmp->debug_output_nodes();
#endif


  } 
  CATCH_MBMESH_RETURN(rc);

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
                                       " unrecognized ESMF element type",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
  } else if (pdim==3) {
    if (etype==10) return MBTET;
    else if (etype==12) return MBHEX;
    else {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                       " unrecognized ESMF element type",
                                       ESMC_CONTEXT, &localrc)) throw localrc;
    }
  }
}


// TODO: Put this someplace to share with other GLUE code
// Get the number of nodes from the element type
 // Get the element topology
int ElemType2NumNodes(int pdim, int etype) {
  if (pdim==2) {
    return etype;
  } else if (pdim==3) {
    if (etype==10) return 4;
    else if (etype==12) return 8;
    else {
       int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
       " for a mesh with parametric dimension 3 element types must be either tetrahedrons or hexahedrons",
                                        ESMC_CONTEXT, &localrc)) throw localrc;
     }
   }
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
}


// This routine takes in a some  element creation information, and sees if any elements need to be split
void _detect_split_elems(
                         // In
                         int pdim, 
                         int num_elems,                                           
                         int *elemType, 
                         int *elemConn, 
                         
                         // Out
                         bool &is_split_local,
                         bool &is_split
                         ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "_detect_split_elems()"
  try {
    int localrc, merr;

    // Error Check Input //
    if (num_elems > 0) {
      ThrowRequire(elemType != NULL);
      ThrowRequire(elemConn != NULL);
    }
    
    // Get current mpi communicator
    MPI_Comm mpi_comm;
    mpi_comm=VM::getCurrent(&localrc)->getMpi_c();
    ESMC_CHECK_THROW(localrc);
    
    // Init is_split output
    is_split=false;
    is_split_local=false;
    
    // Count the number of extra elements we need for splitting
    int num_extra_elem=0;
    if (pdim==2) {
      int conn_pos=0;
      for (int e= 0; e<num_elems; ++e) {
        
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
              subelem_size=0;
            }
            
            // next connection
            conn_pos++;
          }
          
          // record this elem
          num_extra_elem += (subelem_size-3); // num tri = # sides-2 - 1 (for orig elem)
        } else {
          conn_pos += elemType[e];
        }

      }
      
      // mark if mesh on this particular PET is split
      if (num_extra_elem > 0) is_split_local=true;
    
      // Get the total number of extra split elems across all PETs  
      int tot_num_extra_elem=0;
      MPI_Allreduce(&num_extra_elem,&tot_num_extra_elem,1,MPI_INT,MPI_SUM,mpi_comm);
      
      // If there's extra elems on any PET, than it's a split mesh
      if (tot_num_extra_elem>0) {
        is_split=true;
      } else {
        is_split=false;
      }
    }
  }
  CATCH_MBMESH_RETHROW
}


// This routine should only be run when split elems have been detected by the above. 
// It takes in a set of element creation information, and outputs the new split element creation information
void _generate_info_for_split_elems(
                                    // In
                                    bool is_split_local,
                                    int pdim, 
                                    int orig_sdim, 
                                    int sdim,
                                    int num_elems,
                                    int *elemId,
                                    int *elemType, int *elemMask,
                                    int areaPresent, double *elemArea,
                                    int elemCoordsPresent, double *elemCoords,
                                    int num_elemConn,
                                    int *elemConn,
                                    double *nodeCoords, // node coords in orig. node order

                                    // Out
                                    int  &max_non_split_id,
                                    std::map<int,int> &split_to_orig_id, 
                                    std::map<int,double> &split_id_to_frac,
                                    int  &num_elems_wsplit,
                                    int *&elemId_wsplit,
                                    int *&elemType_wsplit,
                                    int *&elemMask_wsplit,
                                    double *&elemArea_wsplit,
                                    double *&elemCoords_wsplit,
                                    int *&elemConn_wsplit
                                    ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "_generate_info_for_split_elems()"
  try {
    ESMCI_MESHCREATE_TRACE_ENTER("MBMesh addelems split elem handling"); 

    int localrc, merr;

    //// Error Check Input ////

    // If there are elems, then these shouldn't be NULL
    if (num_elems > 0) {
      ThrowRequire(elemId != NULL);
      ThrowRequire(elemType != NULL);
      ThrowRequire(elemConn != NULL);
    }

    // Check below closer to where it's used
    // if ((num_elems > 0) && is_split_local)  {
    //    ThrowRequire(nodeCoords != NULL);
    // }

    // If area is present, shouldn't be NULL
    if (areaPresent) ThrowRequire(elemArea != NULL);
    if (elemCoordsPresent) ThrowRequire(elemCoords != NULL);

    

    // Get current mpi communicator
    MPI_Comm mpi_comm;
    mpi_comm=VM::getCurrent(&localrc)->getMpi_c();
    ESMC_CHECK_THROW(localrc);
    
    
    // Count the number of extra elements we need for splitting
    int num_extra_elem=0;
    int max_num_conn=0;
    int max_num_elemtris=0;
    int conn_pos=0;
    for (int e=0; e<num_elems; ++e) {
        
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
    
    // Compute the extra element ranges
    int beg_extra_ids=0;
    // get maximum local elem id
    int max_id=0;
    for (int e=0; e<num_elems; ++e) {
      if (elemId[e] > max_id) {
        max_id=elemId[e];
      }
    }

      
    // Calc global max id
    int global_max_id=0;
    MPI_Allreduce(&max_id,&global_max_id,1,MPI_INT,MPI_MAX,mpi_comm);
    
    // Set maximum of non-split ids
    max_non_split_id=global_max_id;
    
    // Calc our range of extra elem ids
    beg_extra_ids=0;
    MPI_Scan(&num_extra_elem,&beg_extra_ids,1,MPI_INT,MPI_SUM,mpi_comm);
    
    // Remove this processor's number from the sum to get the beginning
    beg_extra_ids=beg_extra_ids-num_extra_elem;
    
    // Start 1 up from max
    beg_extra_ids=beg_extra_ids+global_max_id+1;
    
    // printf("%d# beg_extra_ids=%d end=%d\n",Par::Rank(),beg_extra_ids,beg_extra_ids+num_extra_elem-1);
    
    ESMCI_MESHCREATE_TRACE_EXIT("MBMesh addelems split elem handling");
    
    
    ESMCI_MESHCREATE_TRACE_ENTER("MBMesh addelems connectivity");
    
    // If there are split elems on this processor, then generate new split lists
    if (is_split_local) {

      // New number of elements
      num_elems_wsplit=num_elems+num_extra_elem;
      
      // Allocate arrays to hold split lists
      ThrowRequire((num_elemConn+3*num_extra_elem) >= 0);
      elemConn_wsplit=new int[num_elemConn+3*num_extra_elem];
      ThrowRequire(num_elems_wsplit >= 0);
      elemType_wsplit=new int[num_elems_wsplit];
      elemId_wsplit=new int[num_elems_wsplit];
      if (elemMask != NULL) elemMask_wsplit=new int[num_elems_wsplit];
      if (areaPresent) elemArea_wsplit=new double[num_elems_wsplit];
      if (elemCoordsPresent) elemCoords_wsplit=new double[orig_sdim*num_elems_wsplit];
          
      // Allocate some temporary variables for splitting
      ThrowRequire(max_num_conn >= 0);
      double *subelem_coords=new double[3*max_num_conn];
      double *subelem_dbl_buf=new double[3*max_num_conn];
      int    *subelem_int_buf=new int[max_num_conn];

      ThrowRequire(max_num_elemtris >= 0);
      double *elemtris_area=new double[max_num_elemtris];
      int *elemtris_split_elem_pos=new int[max_num_elemtris];

      // Allocate other temp. variables, but make sure they aren't of size <0. 
      ThrowRequire((max_num_conn-2) >= 0);
      int *subelem_tri_ind=new int[3*(max_num_conn-2)];
      double *subelem_tri_area=new double[max_num_conn-2];
      
      // Check that nodeCoords is not NULL, because it's used below if num_elems>0
      if (num_elems > 0)  {
        ThrowRequire(nodeCoords != NULL);
      }
      
      // new id counter
      int curr_extra_id=beg_extra_ids;
      
      // Loop through elems generating split elems if necessary
      int conn_pos = 0;
      int split_conn_pos = 0;
      int split_elem_pos = 0;
      for (int e=0; e<num_elems; ++e) {
        
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
              
            // Get corner coordinates
            int crd_pos=0;
            for (int i=0; i<elemType[e]; i++) {
              int node_index=elemConn[conn_pos+i]-1; // -1 because elemConn 1-based, but C indexing 0-based
              double *corner_coords=nodeCoords+3*node_index; // 3 is used because nodeCoords are always 3 doubles in MOAB
              
              for (int j=0; j<sdim; j++) {
                subelem_coords[crd_pos]=corner_coords[j];
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
                split_to_orig_id[curr_extra_id]=elemId[e]; // Store map of split to original id
                curr_extra_id++;
              }
              
              // Type is triangle
              elemType_wsplit[split_elem_pos]=3;
              
              // Set mask (if it exists)
              if (elemMask != NULL) elemMask_wsplit[split_elem_pos]=elemMask[e];
              
              // Set element coords. (if it exists)
              if (elemCoordsPresent) {
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
                 split_id_to_frac[elemId_wsplit[sep]]=frac;

                 // Set area to fraction of original area
                 if (areaPresent) elemArea_wsplit[sep]=elemArea[e]*frac;
               }
             } else {
               for (int i=0; i<num_elemtris; i++) {
                 double frac=elemtris_area[i]/tot_elemtris_area;
                 int sep=elemtris_split_elem_pos[i];

                 // Add frac to mesh split information
                 split_id_to_frac[elemId_wsplit[sep]]=0.0;

                 // Set area to fraction of original area
                 if (areaPresent) elemArea_wsplit[sep]=0.0;
               }
             }

           } else { // just copy
            elemId_wsplit[split_elem_pos]=elemId[e];
            elemType_wsplit[split_elem_pos]=elemType[e];
            if (areaPresent) elemArea_wsplit[split_elem_pos]=elemArea[e];
            if (elemCoordsPresent) {
              double *elem_pnt=elemCoords+orig_sdim*e;
              double *elem_pnt_wsplit=elemCoords_wsplit+orig_sdim*split_elem_pos;
              for (int j=0; j<orig_sdim; j++) {
                elem_pnt_wsplit[j]=elem_pnt[j];
              }
            }
            if (elemMask != NULL) elemMask_wsplit[split_elem_pos]=elemMask[e];
            split_elem_pos++;
            for (int i=0; i<elemType[e]; i++) {
              elemConn_wsplit[split_conn_pos]=elemConn[conn_pos];
              split_conn_pos++;
              conn_pos++;
            }
        }
      }

      // Deallocate temporary variables for splitting
      delete [] subelem_coords;
      delete [] subelem_dbl_buf;
      delete [] subelem_int_buf;
      delete [] subelem_tri_ind;
      delete [] subelem_tri_area;
      delete [] elemtris_area;
      delete [] elemtris_split_elem_pos;
    }

    ESMCI_MESHCREATE_TRACE_EXIT("MBMesh addelems connectivity");
  }
  CATCH_MBMESH_RETHROW
}



// This method handles adding elements where the entire set is one tyoe
// This method takes a set of element info and adds the contained elements to MOAB in groups
// (Adding in groups by type seems to be more efficient in MOAB)
void _add_elems_all_one_type(MBMesh *mbmp, int localPet, 
                             int curr_elem_type,
                             int num_elems,   
                             int *elemId, 
                             int *elemMask,
                             int areaPresent, double *elemArea,
                             int elemCoordsPresent, double *elemCoords,
                             int *elemConn                              
                             ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "_add_elems_all_one_type()"
  try {
    int localrc, merr;

    // If number of elems is 0, then there's nothing to do, so just leave
    if (num_elems <= 0) return;

    // Check input
    ThrowRequire(elemId != NULL);
    ThrowRequire(elemConn != NULL);
    if (areaPresent == 1) ThrowRequire(elemArea != NULL);
    if (elemCoordsPresent == 1) ThrowRequire(elemCoords != NULL);

    // Get number of nodes_per_elem and MOAB entity type
    int num_nodes_per_elem=0;
    EntityType etype;
    num_nodes_per_elem=ElemType2NumNodes(mbmp->pdim, curr_elem_type);  
    etype=get_entity_type(mbmp->pdim, curr_elem_type);
    
     
    // Allocate connection list
    ThrowRequire(num_nodes_per_elem*num_elems >= 0);
    EntityHandle *node_conn= new EntityHandle[num_nodes_per_elem*num_elems]; 

    // Get vector of orig_nodes
    std::vector<EntityHandle> orig_nodes;
    mbmp->get_sorted_orig_nodes(orig_nodes);

    // Loop elements and construct connection list
    int conn_pos=0;
    for (int e = 0; e < num_elems; ++e) {
      
      // Loop over this element
      for (int n = 0; n < num_nodes_per_elem; ++n) {
        
        // Get 0-based vert index
        int vert_index=elemConn[conn_pos]-1;
        
        // Setup connectivity list
        node_conn[conn_pos] = orig_nodes.at(vert_index);
        
        // Advance to next
        conn_pos++;
      }        
    }


    // Allocate orig_pos list
    ThrowRequire(num_elems >= 0);
    int *orig_pos=new int[num_elems];
    
    // Fill orig_pos list
    for (int i=0; i<num_elems; i++) {
      orig_pos[i]=i;
    }
    

    // Allocate owner list
    ThrowRequire(num_elems >= 0);
    int *owners=new int[num_elems];

    // Fill owner list
    for (int i=0; i<num_elems; i++) {
      owners[i]=localPet;
    }
    
    
    // Create elems
    Range added_elems;
    mbmp->add_elems(num_elems, 
                    etype, num_nodes_per_elem,
                    node_conn, // List of nodes that make up each elem (of size num_new_elems*nodes_per_elem)
                    elemId,      // global ids for each elem (of size num_new_elems)
                    orig_pos,  // original position for each elem (of size num_new_elems)
                    owners,     // owner for each elem (of size num_new_elems)
                    added_elems);


    //// Fill optional fields ////

    // Mask
    if ((elemMask != NULL) && mbmp->has_elem_mask) {
      mbmp->set_elem_mask_val(added_elems, elemMask);
    }

    // Elem area
    if (areaPresent && mbmp->has_elem_area) {
      mbmp->set_elem_area(added_elems, elemArea);
    }


    // Elem coords
    if (elemCoordsPresent && mbmp->has_elem_coords) {
      mbmp->set_elem_coords(added_elems, elemCoords);
    }
      

    // Deallocate temp. arrays
    delete [] node_conn;
    delete [] orig_pos;
    delete [] owners;
  }
  CATCH_MBMESH_RETHROW
}


// This method handles adding elements where the element informatiomn contains
// elements of multiple types. It goes through and gathers the elements of a particular
// type and adds that set
// (Adding in groups by type seems to be more efficient in MOAB)
void _add_elems_multiple_types(MBMesh *mbmp, int localPet, 
                               int curr_elem_type,
                               int num_elems,   
                               int *elemId, 
                               int *elemType, 
                               int *elemMask,
                               int areaPresent, double *elemArea,
                               int elemCoordsPresent, double *elemCoords,
                               int *elemConn,
                               int elem_info_buff_size, int *elem_id_buff, int *elem_owner_buff, int *elem_orig_pos_buff, 
                               int tmp_buff_size, char *tmp_buff
                               ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "_add_elems_multiple_types()"
  try {
    int localrc, merr;
   
    // If number of elems is 0, then there's nothing to do, so just leave
    if (num_elems <= 0) return;

    // Check input
    // (If num_elems > 0 then all of the below should exist)
    ThrowRequire(elemId != NULL);
    ThrowRequire(elemConn != NULL);
    ThrowRequire(elemType != NULL);
    if (areaPresent) ThrowRequire(elemArea != NULL);
    if (elemCoordsPresent) ThrowRequire(elemCoords != NULL);
    ThrowRequire(elem_id_buff != NULL);
    ThrowRequire(elem_owner_buff != NULL);
    ThrowRequire(elem_orig_pos_buff != NULL);
    ThrowRequire(tmp_buff != NULL);


    // Get number of nodes_per_elem and MOAB entity type
    int num_nodes_per_elem=0;
    EntityType etype;
    num_nodes_per_elem=ElemType2NumNodes(mbmp->pdim,
                                         curr_elem_type);  
    etype=get_entity_type(mbmp->pdim,curr_elem_type);
    
     
    // Get number of elements of this type
    int num_elems_of_type=0;
    for (int e=0; e<num_elems; e++) {
      if (elemType[e] == curr_elem_type) num_elems_of_type++;
    }


    // Make sure buffers are big enough for elem info.
    if (elem_info_buff_size < num_elems_of_type*sizeof(int)) Throw() << "elem info. buffers too small.";

    // Make sure buffer is big enough for connection list
    if (tmp_buff_size < num_nodes_per_elem*num_elems_of_type*sizeof(EntityHandle)) Throw() << "tmp_buff too small to hold node connections.";

    // Use temporary buffer for connection list
    EntityHandle *node_conn=(EntityHandle *)tmp_buff;
      
    // Get vector of orig_nodes
    std::vector<EntityHandle> orig_nodes;
    mbmp->get_sorted_orig_nodes(orig_nodes);

    // Loop elements and fill informatiton
    int pos=0;
    int node_conn_pos=0;
    int elemConn_pos=0;
    for (int e=0; e<num_elems; ++e) {
      
      // Only do if the correct type
      if (elemType[e] == curr_elem_type) {

        // Fill id
        elem_id_buff[pos]=elemId[e];
        
        // Fill Owner
        elem_owner_buff[pos]=localPet;

        // Fill orig pos
        elem_orig_pos_buff[pos]=e;

        // Advance to next pos
        pos++;

        // Fill connections
        for (int n = 0; n < num_nodes_per_elem; ++n) {
          
          // Get 0-based vert index
          int vert_index=elemConn[elemConn_pos]-1;
          
          // Advance to next connection pos
          elemConn_pos++;

          // Setup connectivity list
          node_conn[node_conn_pos] = orig_nodes.at(vert_index);
          
          // Advance to next connection pos
          node_conn_pos++;
        }
      } else {

        // Even if we aren't an elem of the current type 
        // Still need to Advance elemConn_pos so that we
        // move through that array
        
        // Get number of node connections for this elem type
        int tmp_num_nodes_per_elem=ElemType2NumNodes(mbmp->pdim, elemType[e]);  

        // Advance to next elem connection pos
        elemConn_pos += tmp_num_nodes_per_elem;
      } 
    }


    
    // Create elems
    Range added_elems;
    mbmp->add_elems(num_elems_of_type,
                    etype, num_nodes_per_elem,
                    node_conn, // List of nodes that make up each elem (of size num_new_elems*nodes_per_elem)
                    elem_id_buff,      // global ids for each elem (of size num_new_elems)
                    elem_orig_pos_buff,  // original position for each elem (of size num_new_elems)
                    elem_owner_buff,     // owner for each elem (of size num_new_elems)
                    added_elems);



    //// Fill optional fields ////

    // Elem mask
    if ((elemMask != NULL) && mbmp->has_elem_mask) {

      // Check size of buffer and set
      if (tmp_buff_size < num_elems_of_type*sizeof(int)) Throw() << "tmp_buff too small to hold elem mask information.";
      int *elem_mask_buff=(int *)tmp_buff;
      
      // copy info into buffer
      int pos=0;
      for (int e=0; e<num_elems; ++e) {
      
        // Only do if the correct type
        if (elemType[e] == curr_elem_type) {

          // Fill mask
          elem_mask_buff[pos]=elemMask[e];
        
          // Advance to next pos
          pos++;
        }
      }

      // Set info in element field
      mbmp->set_elem_mask_val(added_elems, elem_mask_buff);
    }

    // Elem area
    if (areaPresent && mbmp->has_elem_area) {

      // Check size of buffer and set
      if (tmp_buff_size < num_elems_of_type*sizeof(double)) Throw() << "tmp_buff too small to hold elem area information.";
      double *elem_area_buff=(double *)tmp_buff;
      
      // copy info into buffer
      int pos=0;
      for (int e=0; e<num_elems; ++e) {
      
        // Only do if the correct type
        if (elemType[e] == curr_elem_type) {

          // Fill area
          elem_area_buff[pos]=elemArea[e];
        
          // Advance to next pos
          pos++;
        }
      }

      // Set info in element field
      mbmp->set_elem_area(added_elems, elem_area_buff);
    }


    // Elem coords
    if (elemCoordsPresent && mbmp->has_elem_coords) {

      // Check size of buffer and set
      if (tmp_buff_size < mbmp->orig_sdim*num_elems_of_type*sizeof(double)) Throw() << "tmp_buff too small to hold elem area information.";
      double *elem_coords_buff=(double *)tmp_buff;
      
      // copy info into buffer
      int buff_pos=0;
      int elemCoords_pos=0;

      for (int e = 0; e < num_elems; ++e) {
        // Only copy if the correct type
        if (elemType[e] == curr_elem_type) {
          // Copy Coords
          for (int d=0; d<mbmp->orig_sdim; d++) {
            elem_coords_buff[buff_pos]=elemCoords[elemCoords_pos];
            buff_pos++;
            elemCoords_pos++;
          } 
        } else { // Advance elemCoords_pos to move through array
          elemCoords_pos += mbmp->orig_sdim;
        }
      }
      // Set info in element field
      mbmp->set_elem_coords(added_elems, elem_coords_buff);
    }
  }
  CATCH_MBMESH_RETHROW
}



// This method takes a set of connection info and adds the contained elements to MOAB in groups
// (Adding in groups by type seems to be more efficient in MOAB)
void _add_elems_in_groups_by_type(MBMesh *mbmp, int localPet, 
                                  int num_elems,
                                  int *elemId, 
                                  int *elemType, int *elemMask,
                                  int areaPresent, double *elemArea,
                                  int elemCoordsPresent, double *elemCoords,
                                  int *elemConn                              
                                  ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "_add_elems_in_groups_by_type()"
  try {
    int localrc, merr;
   
    // If number of elems is 0, then there's nothing to do, so just leave
    if (num_elems <= 0) return;

      // Check input
    // (If num_elems > 0 then all of the below should exist)
    ThrowRequire(elemId != NULL);
    ThrowRequire(elemConn != NULL);
    ThrowRequire(elemType != NULL);
    if (areaPresent) ThrowRequire(elemArea != NULL);
    if (elemCoordsPresent) ThrowRequire(elemCoords != NULL);


    //// Figure out the list of elem types that are used, and how many of each ////

    // Right now there is a very small number of elem types with a very small range, so
    // this is an efficent way to do this, if the range ever becomes larger this might
    // have to change

// Variables for how many of each type, etc. 
#define MAX_ELEM_TYPE 12
#define ELEM_TYPE_ARRAY_SIZE MAX_ELEM_TYPE+1   // NEED +1 BECAUSE C++ arrays are 0-based
    int num_of_each_elem_type[ELEM_TYPE_ARRAY_SIZE];
    int num_elem_types;
    int elem_types[ELEM_TYPE_ARRAY_SIZE];
    int num_per_type[ELEM_TYPE_ARRAY_SIZE];

    // Int array to all zeros
    for (int i=0; i<ELEM_TYPE_ARRAY_SIZE; i++) {
      num_of_each_elem_type[i]=0;
    }

    // Figure out the number of each element type
    for (int e=0; e<num_elems; e++) {
      int etype=elemType[e];
      if (etype > MAX_ELEM_TYPE) Throw() << "Unrecognized element type.";
      num_of_each_elem_type[etype]++;
    }

    // Collapse to just non-zeros
    num_elem_types=0;
    for (int i=0; i<ELEM_TYPE_ARRAY_SIZE; i++) {
      if (num_of_each_elem_type[i] > 0) {
        elem_types[num_elem_types]=i;
        num_per_type[num_elem_types]=num_of_each_elem_type[i];
        num_elem_types++;
      }
    }

#undef MAX_ELEM_TYPE
#undef ELEM_TYPE_ARRAY_SIZE


    // If there is just one element type used, then do that and leave
    if (num_elem_types == 1) {

      _add_elems_all_one_type(mbmp, localPet, 
                              elem_types[0],
                              num_elems,   
                              elemId, 
                              elemMask,
                              areaPresent, elemArea,
                              elemCoordsPresent, elemCoords,
                              elemConn);
        return;
    }

    // Across the different elem types find the max size of connection array and max number of elems 
    int max_num_elems_per_type=0;
    int max_num_conn_per_type=0;
    for (int i=0; i<num_elem_types; i++) {
      if (num_per_type[i] > max_num_elems_per_type) max_num_elems_per_type=num_per_type[i];

      int num_conn=num_per_type[i]*ElemType2NumNodes(mbmp->pdim,elem_types[i]);  
      if (num_conn > max_num_conn_per_type) max_num_conn_per_type=num_conn;
    }  


    // Allocate max space buffers to hold creation information for int types
    ThrowRequire(max_num_elems_per_type > 0); // (If num_elems >0, then this should be >0)
    int elem_info_buff_size=max_num_elems_per_type*sizeof(int);
    int *elem_ids_buff=new int[max_num_elems_per_type];
    int *elem_owner_buff=new int[max_num_elems_per_type];
    int *elem_orig_pos_buff=new int[max_num_elems_per_type];

    
    // Allocate a buffer to use to hold node_conn, and optional fields
    int tmp_buff_size=max_num_conn_per_type*sizeof(EntityHandle); // max size of node connections
    if (elemMask != NULL) {
      int max_size_of_elemMask=max_num_elems_per_type*sizeof(int);
      if (max_size_of_elemMask > tmp_buff_size) tmp_buff_size=max_size_of_elemMask;
    } 
    if (areaPresent) {
      int max_size_of_elemArea=max_num_elems_per_type*sizeof(double);
      if (max_size_of_elemArea > tmp_buff_size) tmp_buff_size=max_size_of_elemArea;
    } 
    if (elemCoordsPresent) {
      int max_size_of_elemCoords=mbmp->orig_sdim*max_num_elems_per_type*sizeof(double);
      if (max_size_of_elemCoords > tmp_buff_size) tmp_buff_size=max_size_of_elemCoords;
    } 
    ThrowRequire(tmp_buff_size > 0); // (If num_elems >0, then this should be >0)
    char *tmp_buff=new char[tmp_buff_size];
    

    // Loop through each type
    for (int t=0; t<num_elem_types; t++) {

      _add_elems_multiple_types(mbmp, localPet, elem_types[t],
                                num_elems, elemId, elemType,  
                                elemMask, 
                                areaPresent, elemArea,
                                elemCoordsPresent, elemCoords,
                                elemConn,
                                elem_info_buff_size, elem_ids_buff, elem_owner_buff, elem_orig_pos_buff, 
                                tmp_buff_size, tmp_buff);
                               
    }

    // Deallocate buffers
    delete [] elem_ids_buff;
    delete [] elem_owner_buff;
    delete [] elem_orig_pos_buff;
    delete [] tmp_buff;
  }
  CATCH_MBMESH_RETHROW
}


void MBMesh_addelements(MBMesh **mbmpp,
                        int *_num_elems, int *elemId, 
                        int *elemType, InterArray<int> *_elemMaskII ,
                        int *_areaPresent, double *elemArea,
                        int *_coordsPresent, double *elemCoords,
                        int *_num_elemConn, int *elemConn, int *regridConserve,
                        ESMC_CoordSys_Flag *_coordSys, int *_orig_sdim,
                        int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_addelements()"
  try {

    // Error return vars
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    //// Setup some handy info ////
    ESMCI_MESHCREATE_TRACE_ENTER("MBMesh addelems setup");

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get MBMesh object
    ThrowRequire(mbmpp != NULL); 
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    // Number of elements being created
    int num_elems=*_num_elems;

    // Total Size of connection list
    int num_elemConn=*_num_elemConn;

    // Element mask array
    InterArray<int> *elemMaskII=_elemMaskII;
    int *elemMask=NULL;
    if (present(elemMaskII)) elemMask=elemMaskII->array;

    // Presence of element areas
    int areaPresent=*_areaPresent;

    // Presence of element coords
    int elemCoordsPresent=*_coordsPresent;

    // Coordinate system of mesh
    ESMC_CoordSys_Flag coordSys=mbmp->coordsys;

    // Original spatial dimension of mesh
    int orig_sdim = mbmp->orig_sdim;

    // Get parametric dimension of mesh
    int pdim=mbmp->pdim;

    // Get spatial dimension of mesh
    int sdim=mbmp->pdim;

    // Get number of orig nodes
    int num_nodes = mbmp->num_orig_node();

    //// Error check input ////

    // Make sure that the arrays that should be there when there
    // are elems (num_elems > 0), are there
    if (num_elems > 0) {
      ThrowRequire(elemId != NULL);
      ThrowRequire(elemConn != NULL);
      ThrowRequire(elemType != NULL);
      if (areaPresent) ThrowRequire(elemArea != NULL);
      if (elemCoordsPresent) ThrowRequire(elemCoords != NULL);
    }

    // Check element type
    // (Don't check when pdim=2, because any number of sides is allowed)
    if (pdim==3) {
      for (int i=0; i< num_elems; i++) {
        if ((elemType[i] != 10) && (elemType[i] != 12)) {
          int localrc;
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
             "- for a mesh with parametric dimension 3 element types must be either tetrahedron or hexahedron ",
                                           ESMC_CONTEXT, &localrc)) throw localrc;
         }
      }
    }

    // Check size of connectivity list
    int expected_conn_size=0;
    if (pdim==2) {
      for (int i=0; i< num_elems; i++) {
        expected_conn_size += elemType[i];
      }
    } else if (pdim==3) {
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


    // Check to make sure that there are no nodes without a home
    // TODO: Maybe this could be done better by just looping through nodes and
    //       making sure each one appears in elemConn[] 

    // Block so node_used goes away
    { 
      std::vector<int> node_used;
      node_used.resize(num_nodes, 0);
      
      // Error check elemConn array
      int c = 0;
      for (int e=0; e<num_elems; ++e) {
        int nnodes = ElemType2NumNodes(mbmp->pdim,
                                       elemType[e]);
        
        for (int n=0; n<nnodes; ++n) {
          
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
      
    } // Block so node_used goes away


    // Check element mask info
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

     ESMCI_MESHCREATE_TRACE_EXIT("MBMesh addelems setup");


     //// See if there are any split elements, if so generate split information ////

     // Check for split elems
     bool is_split=false;
     bool is_split_local=false;     
     _detect_split_elems(pdim, num_elems, elemType, elemConn, 
                         is_split_local, is_split);

     // Set mesh split status
     mbmp->is_split=is_split;

     // If there are split elems on any pet, then get info
     int num_elems_wsplit=0;
     int *elemConn_wsplit=NULL;
     int *elemType_wsplit=NULL;
     int *elemId_wsplit=NULL;
     double *elemArea_wsplit=NULL;
     double *elemCoords_wsplit=NULL;
     int *elemMask_wsplit=NULL;

     if (is_split) {

       // If there are local split elems, allocate and fill node coords
       double *nodeCoords=NULL;
       if (is_split_local) {

         // Get vector of orig_nodes
         std::vector<EntityHandle> orig_nodes;
         mbmp->get_sorted_orig_nodes(orig_nodes);
         
         // Allocate space for node coords
         if (orig_nodes.size() > 0) {
           
           // 3 because in MOAB 3 coords are stored per vert
           nodeCoords=new double[3*orig_nodes.size()];
           
           // Get coords from MOAB
           merr=mbmp->mesh->get_coords(&orig_nodes[0], orig_nodes.size(), nodeCoords);
           ESMC_CHECK_MOAB_THROW(merr);      
         }
       }
              
       // Generate split info 
       _generate_info_for_split_elems(// In
                                      is_split_local,pdim, orig_sdim, sdim, 
                                      num_elems, elemId, elemType, elemMask, 
                                      areaPresent, elemArea, elemCoordsPresent, elemCoords,
                                      num_elemConn, elemConn, nodeCoords, 
                                 
                                      // Out
                                      mbmp->max_non_split_id, mbmp->split_to_orig_id,  mbmp->split_id_to_frac,
                                      num_elems_wsplit, elemId_wsplit, elemType_wsplit, elemMask_wsplit, 
                                      elemArea_wsplit, elemCoords_wsplit, elemConn_wsplit);

       // If there was a local split elem, then use the split info for creating elems below. 
       if (is_split_local) {
         num_elems=num_elems_wsplit;
         elemConn=elemConn_wsplit;
         elemType=elemType_wsplit;
         elemId=elemId_wsplit;
         elemMask=elemMask_wsplit;
         if (areaPresent) elemArea=elemArea_wsplit;
         if (elemCoordsPresent) elemCoords=elemCoords_wsplit;
       }

       // Get rid of nodeCoords
       if (is_split_local) {
         delete [] nodeCoords;
       }
     } // if (is_split)
     

    //// Add elems and elem fields ////
    ESMCI_MESHCREATE_TRACE_ENTER("MBMesh addelems add");
    
    // Turn on element masking 
    if (present(elemMaskII)) { 
      mbmp->setup_elem_mask();
    }
    
    // Turn on element areas
    if (areaPresent) { 
      mbmp->setup_elem_area();
    }
    
    // Turn on element coordinates
    if (elemCoordsPresent) { 
      mbmp->setup_elem_coords();
    }

    // Add elements in groups by type, also set optional fields in elements      
    _add_elems_in_groups_by_type(mbmp, localPet, 
                                 num_elems, elemId, elemType, elemMask,
                                 areaPresent, elemArea, elemCoordsPresent, elemCoords,
                                 elemConn);

    ESMCI_MESHCREATE_TRACE_EXIT("MBMesh addelems add");
    
    // Done creating elems, so finalize
    mbmp->finalize_elems();

    //// Setup parallel sharing ///
    // DON'T DO, BECAUSE IT ISN'T NEEDED ALL THE TIME
    // mbmp->setup_parallel();

    // Debug output of all elems on this processor
#ifdef ESMF_MBMESH_DEBUG_CREATE_OUTPUT_ELEMS
    mbmp->debug_output_elems();
#endif

  // Get rid of extra memory for split elements
  if (is_split_local) {
    delete [] elemConn_wsplit;
    delete [] elemType_wsplit;
    delete [] elemId_wsplit;
    if (present(elemMaskII)) delete [] elemMask_wsplit;
    if (areaPresent) delete [] elemArea_wsplit;
    if (elemCoordsPresent) delete [] elemCoords_wsplit;
  }

  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}



void MBMesh_turnonnodemask(MBMesh **mbmpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_turnonnodemask()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

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
      ESMC_CHECK_MOAB_THROW(merr);

      // Loop through elements setting values
      for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
        const EntityHandle node=*it;

        // Get mask value
        int mv;
        merr=moab_mesh->tag_get_data(mbmp->node_mask_val_tag, &node, 1, &mv);
        ESMC_CHECK_MOAB_THROW(merr);

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
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

// Turn OFF masking
 void MBMesh_turnoffnodemask(MBMesh **mbmpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_turnoffnodemask()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // If has masks
    if (mbmp->has_node_mask) {
      // Get a range containing all nodes
      Range range_node;
      merr=moab_mesh->get_entities_by_dimension(0,0,range_node);
      ESMC_CHECK_MOAB_THROW(merr);

      // Loop through elements setting values
      for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
        const EntityHandle node=*it;

        // unset masked value
        int masked=0;
        merr=moab_mesh->tag_set_data(mbmp->node_mask_tag, &node, 1, &masked);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

void MBMesh_turnonelemmask(MBMesh **mbmpp, ESMCI::InterArray<int> *maskValuesArg,  int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_turnonelemmask()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

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
      ESMC_CHECK_MOAB_THROW(merr);

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
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

}

// Turn OFF masking
void MBMesh_turnoffelemmask(MBMesh **mbmpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_turnoffelemmask()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // If has masks
    if (mbmp->has_elem_mask) {
      // Get a range containing all elements
      Range range_elem;
      merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
      ESMC_CHECK_MOAB_THROW(merr);

      // Loop through elements setting values
      for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
        const EntityHandle elem=*it;

        // unset masked value
        int masked=0;
        merr=moab_mesh->tag_set_data(mbmp->elem_mask_tag, &elem, 1, &masked);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}



void MBMesh_destroy(MBMesh **mbmpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_destroy()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    // Get rid of MBMesh
    delete mbmp;

    // Set to null
    *mbmpp=NULL;

  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}



void MBMesh_write(MBMesh **mbmpp, char *fname, int *rc,
    ESMCI_FortranStrLenArg nlen) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_write()"
  try {

    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get petCount
    int petCount = VM::getCurrent(&localrc)->getPetCount();
    ESMC_CHECK_PASSTHRU_THROW(localrc);


    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Make c format string
    char *filename = ESMC_F90toCstring(fname, nlen);

    // Add vtk
#define FILENAME_W_VTK_MAX 1024
    char filename_w_vtk[FILENAME_W_VTK_MAX];
    if (strlen(filename)+4 > FILENAME_W_VTK_MAX) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
                                     " filename too long ", ESMC_CONTEXT, rc);
      return;
    }
#undef FILENAME_W_VTK_MAX

    // Write file name (add pet info if > 1 PET)
    if (petCount > 1) {
      sprintf(filename_w_vtk,"%s.%d.%d.vtk",filename,petCount,localPet);
    } else {
      sprintf(filename_w_vtk,"%s.vtk",filename);
    }

    // Call into MOAB
    int merr=moab_mesh->write_file(filename_w_vtk,NULL,NULL);
    ESMC_CHECK_MOAB_THROW(merr);

    // get rid of c format string
    delete [] filename;

  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}


void MBMesh_createnodedistgrid(MBMesh **mbmpp, int *ngrid, int *num_lnodes, int *rc) {

#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createnodedistgrid()"

  // Declare id vectors
  std::vector<int> ngids;

  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get a range containing all nodes
    Range range_node;
    merr=mbmp->mesh->get_entities_by_dimension(0,0,range_node);
    ESMC_CHECK_MOAB_THROW(merr);

    for(Range::const_iterator it=range_node.begin(); it !=range_node.end(); it++) {
      const EntityHandle *node=&(*it);

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mbmp->owner_tag, node, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // If owned by this processor, put in list
      if (owner==localPet) {
        // Get gid
        int gid;
        merr=moab_mesh->tag_get_data(mbmp->gid_tag, node, 1, &gid);
        ESMC_CHECK_MOAB_THROW(merr);

        // Stick in list
        ngids.push_back(gid);
      }
    }

  } 
  CATCH_MBMESH_RETURN(rc);

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

/**
 * Sort elements by the order in which they were originally declared
 * (which is stored by get_data_index)
 * Don't include split elements
 */

// DO THIS BETTER, HAVE A FIELD THAT CONTAINS THE POSITION IN THE FINAL ARRAY AND -1 FOR ANYTHING NOT LOCAL OR SPLIT
void getElemGIDS(MBMesh *mbmp, std::vector<int> &egids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "getElemGIDS()"
  try {
    int localrc, merr;

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get a range containing all elements
    Range range_elem;
    merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through elements putting into list
    std::vector<std::pair<int,int> > pos_and_gids;
    for(Range::const_iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
      const EntityHandle *elemp=(&*it);

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mbmp->owner_tag, elemp, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // If owned by this processor, put in list
      if (owner==localPet) {
        // Get gid
        int gid;
        merr=moab_mesh->tag_get_data(mbmp->gid_tag, elemp, 1, &gid);
        ESMC_CHECK_MOAB_THROW(merr);

        // Don't do split elements
        if (mbmp->is_split && gid > mbmp->max_non_split_id) continue;

        // Get orig_pos
        int orig_pos;
        merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, elemp, 1, &orig_pos);
        ESMC_CHECK_MOAB_THROW(merr);

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
  CATCH_MBMESH_RETHROW
}


void MBMesh_createelemdistgrid(MBMesh **mbmpp, int *egrid, int *num_lelems, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createelemdistgrid()"

  // Declare id vectors
  std::vector<int> egids;

  try {

    int localrc;
    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    // Get list of elem gids in order
    getElemGIDS(mbmp, egids);

  } 
  CATCH_MBMESH_RETURN(rc);

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
void getElems(MBMesh **mbmpp, std::vector<EntityHandle> &ehs) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_getElems()"
  try {
    // Get localPet
    int localrc, merr;
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get a range containing all elements
    Range range_elem;
    merr=moab_mesh->get_entities_by_dimension(0,mbmp->pdim,range_elem);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through elements putting into list
    std::vector<std::pair<int,EntityHandle> > pos_and_elems;
    for(Range::const_iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
      const EntityHandle *elemp=(&*it);
      EntityHandle elem=*it;

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mbmp->owner_tag, elemp, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // If owned by this processor, put in list
      if (owner==localPet) {
        // Get gid
        int gid;
        merr=moab_mesh->tag_get_data(mbmp->gid_tag, elemp, 1, &gid);
        ESMC_CHECK_MOAB_THROW(merr);

        // Get orig_pos
        int orig_pos;
        merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, elemp, 1, &orig_pos);
        ESMC_CHECK_MOAB_THROW(merr);

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
  CATCH_MBMESH_RETHROW
}


void MBMesh_getlocalelemcoords(MBMesh **mbmpp, double *ecoords,
                               int *_orig_sdim, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_getlocalelemcoords()"
  try {
    int localrc,merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

      // Get Moab Mesh wrapper
      MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

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
          ESMC_CHECK_MOAB_THROW(merr);
        }
      } else {
        for (int i=0; i<ehs.size(); i++) {
          // Get element gid
          EntityHandle elem=ehs[i];

          // Get orig_pos
          merr=moab_mesh->tag_get_data(mbmp->elem_coords_tag,
                                       &elem, 1, ecoords+orig_sdim*i);
          ESMC_CHECK_MOAB_THROW(merr);
        }
      }
    } 
    CATCH_MBMESH_RETURN(rc);

    // Set return code
    if (rc!=NULL) *rc = ESMF_SUCCESS;
}


void MBMesh_getarea(MBMesh **mbmpp, int *num_elem, double *elem_areas, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_getarea()"

  // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
  int num_poly_nodes;
  double poly_coords[MAX_NUM_POLY_COORDS];
  double tmp_coords[MAX_NUM_POLY_COORDS];

  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

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
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get dimensions
    int sdim=mbmp->sdim;
    int pdim=mbmp->pdim;


    // Put areas into array
    if (mbmp->has_elem_area) {

      merr=moab_mesh->tag_get_data(mbmp->elem_area_tag, &ehs[0], ehs.size(), elem_areas);
      ESMC_CHECK_MOAB_THROW(merr);

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

  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;


  // Declare polygon information
#undef  MAX_NUM_POLY_COORDS
#undef  MAX_NUM_POLY_NODES_2D
#undef  MAX_NUM_POLY_NODES_3D

}

// DO THIS BETTER, HAVE A FIELD THAT CONTAINS THE POSITION IN THE FINAL ARRAY AND -1 FOR ANYTHING NOT LOCAL OR SPLIT
void getNodes(MBMesh **mbmpp, std::vector<EntityHandle> &nodes) {
#undef  ESMC_METHOD
#define ESMC_METHOD "getNodes()"
  try {
    int localrc, merr;

    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

    // Get a range containing all nodes
    Range range_node;
    merr=moab_mesh->get_entities_by_dimension(0,0,range_node);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through nodes putting into list
    std::vector<std::pair<int,EntityHandle> > pos_and_nodes;
    for(Range::const_iterator it=range_node.begin(); it !=range_node.end(); it++) {
      const EntityHandle *nodep=(&*it);
      EntityHandle node=*it;

      // Get owner
      int owner;
      merr=moab_mesh->tag_get_data(mbmp->owner_tag, nodep, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // If owned by this processor, put in list
      if (owner==localPet) {
        // Get gid
        int gid;
        merr=moab_mesh->tag_get_data(mbmp->gid_tag, nodep, 1, &gid);
        ESMC_CHECK_MOAB_THROW(merr);

        // Get orig_pos
        int orig_pos;
        merr=moab_mesh->tag_get_data(mbmp->orig_pos_tag, nodep, 1, &orig_pos);
        ESMC_CHECK_MOAB_THROW(merr);

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
  CATCH_MBMESH_RETHROW
}

void MBMesh_getlocalcoords(MBMesh **mbmpp, double *ncoords, int *_orig_sdim, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_getlocalcoords()"
  try {
    int localrc, merr;

    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *moab_mesh=mbmp->mesh;

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
        ESMC_CHECK_MOAB_THROW(merr);
      }
    } else {
      for (int i=0; i<nodes.size(); i++) {
        // Get node
        EntityHandle node=nodes[i];

        // Get coords
        merr=moab_mesh->get_coords(&node, 1, ncoords+orig_sdim*i);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

// Set data in Array based on typekind
template <class TYPE>
void MBMesh_set_Array_data(LocalArray *localArray, int index, ESMC_TypeKind_Flag typekind, TYPE data) {

  if (typekind == ESMC_TYPEKIND_I4) {
    ESMC_I4 data_i4=static_cast<ESMC_I4>(data);
    localArray->setData(&index, data_i4);
  } else if (typekind == ESMC_TYPEKIND_R4) {
    ESMC_R4 data_r4=static_cast<ESMC_R4>(data);
    localArray->setData(&index, data_r4);
  } else if (typekind == ESMC_TYPEKIND_R8) {
    ESMC_R8 data_r8=static_cast<ESMC_R8>(data);
    localArray->setData(&index, data_r8);
  } else {
    int localrc;
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                  " unsupported typekind in Array.",
                                  ESMC_CONTEXT, &localrc);
    throw localrc;
  }
}

template void MBMesh_set_Array_data(LocalArray *localArray, int index, ESMC_TypeKind_Flag typekind, double data);

template void MBMesh_set_Array_data(LocalArray *localArray, int index, ESMC_TypeKind_Flag typekind, int data);



void MBMesh_geteleminfointoarray(MBMesh *vmbmp,
                                 ESMCI::DistGrid *elemDistgrid, 
                                 int numElemArrays,
                                 int *infoTypeElemArrays, 
                                 ESMCI::Array **elemArrays, 
                                 int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_geteleminfointoarray()"

// Must match with ESMF_MeshGet()
#define INFO_TYPE_ELEM_ARRAYS_MASK 1
#define INFO_TYPE_ELEM_ARRAYS_AREA 2
#define INFO_TYPE_ELEM_ARRAYS_MAX  2

  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Make sure that incoming Array distgrids match element distgrid
    for (int i=0; i<numElemArrays; i++) {

      // Get match
      DistGridMatch_Flag matchflag=DistGrid::match(elemArrays[i]->getDistGrid(),elemDistgrid,&localrc);
      ESMC_CHECK_PASSTHRU_THROW(localrc);

      // Complain if it doesn't match sufficiently
      if ((matchflag != DISTGRIDMATCH_EXACT) && (matchflag != DISTGRIDMATCH_ALIAS)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                      " DistGrid in element information Array doesn't match Mesh element DistGrid.",
                                      ESMC_CONTEXT, &localrc);
        throw localrc;
      }
    }

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*>(vmbmp);

    // Get the fields, Arrays for the various types of info
    ESMCI::Array *elem_mask_Array=NULL;
    ESMCI::Array *elem_area_Array=NULL;

    for (int i=0; i<numElemArrays; i++) {
      if (infoTypeElemArrays[i] == INFO_TYPE_ELEM_ARRAYS_MASK) { 
        if (!mbmp->has_elem_mask) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                        " mesh doesn't contain element mask information.",
                                        ESMC_CONTEXT, &localrc);
          throw localrc;
        }

        // Get array pointer
        elem_mask_Array=elemArrays[i];

        // Complain if the array has more than rank 1
        if (elem_mask_Array->getRank() != 1) Throw() << "this call currently can't handle Array rank != 1";
      }

      if (infoTypeElemArrays[i] == INFO_TYPE_ELEM_ARRAYS_AREA) { 
        if (!mbmp->has_elem_area) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                                        " mesh doesn't contain element area information.",
                                        ESMC_CONTEXT, &localrc);
          throw localrc;
        }

        // Get array pointer
        elem_area_Array=elemArrays[i];

        // Complain if the array has more than rank 1                                 
        if (elem_area_Array->getRank() != 1) Throw() << "this call currently can't handle Array rank != 1";

        // Complain if the Mesh is split 
        if (mbmp->is_split) Throw() << "this call currently can't handle a mesh containing elems with > 4 corners";
      }
    }


    // Get elems
    Range elems;
    mbmp->get_all_elems(elems);

    // TODO: MAYBE HAVE THE LIST OF LOCAL elemhandles in order in the mbmesh, that way you could
    //     probably just read/write them quickly in a chunk using one of the MOAB subroutines.
    //     However, until then, this is a good way to do it.

    // Loop and set up gid_to_elem_map
    std::map<int,EntityHandle> gid_to_elem_map;
    for (Range::iterator it=elems.begin(); it !=elems.end(); it++) {
      EntityHandle elem=*it;
      
      // Get node global id
      int gid=mbmp->get_gid(elem);
      
      // Add to map
      gid_to_elem_map[gid]=elem;
    }

    // Get LocalDeCount
    int localDECount=elemDistgrid->getDELayout()->getLocalDeCount();

    // Loop filling local DEs
    for (int lDE=0; lDE<localDECount; lDE++) {

      // Get sequence indices
      std::vector<int> seqIndexList;
      elemDistgrid->fillSeqIndexList(seqIndexList, lDE, 1);

      // Get mask if needed
      if (elem_mask_Array) {
        // Get the array info
        LocalArray *localArray=(elem_mask_Array->getLocalarrayList())[lDE];

        // Get localDE lower bound
        int lbound=(elem_mask_Array->getComputationalLBound())[lDE]; // (assumes array rank is 1)

        // Typekind
        ESMC_TypeKind_Flag typekind=elem_mask_Array->getTypekind();

        // Loop seqIndices
        for (int i=0; i<seqIndexList.size(); i++) {
          int si=seqIndexList[i];

          // Get elem with si as gid
          std::map<int,EntityHandle>::iterator mi =  gid_to_elem_map.find(si);
          
          // If it doesn't exist, then go to next
          if (mi == gid_to_elem_map.end()) {
            Throw() << "element with that id not found in mesh";
          }
          
          // Get elem 
          EntityHandle elem=mi->second;

          // Get elem mask value
          int mask_val=mbmp->get_elem_mask_val(elem);
     
          // Location in array
          int index=i+lbound;
          
          // Convert and set data
          MBMesh_set_Array_data(localArray, index, typekind, mask_val);
        }
      }

      // Get area if needed
      if (elem_area_Array) {
        // Get the array info
        LocalArray *localArray=(elem_area_Array->getLocalarrayList())[lDE];

        // Get localDE lower bound
        int lbound=(elem_area_Array->getComputationalLBound())[lDE]; // (assumes array rank is 1)
        // Typekind
        ESMC_TypeKind_Flag typekind=elem_area_Array->getTypekind();

        // Loop seqIndices
        for (int i=0; i<seqIndexList.size(); i++) {
          int si=seqIndexList[i];

          // Get elem with si as gid
          std::map<int,EntityHandle>::iterator mi =  gid_to_elem_map.find(si);
          
          // If it doesn't exist, then go to next
          if (mi == gid_to_elem_map.end()) {
            Throw() << "element with that id not found in mesh";
          }
          
          // Get elem 
          EntityHandle elem=mi->second;

          // Get elem area
          double area= mbmp->get_elem_area(elem);
     
          // Location in array
          int index=i+lbound;
          
          // Convert and set data
          MBMesh_set_Array_data(localArray, index, typekind, area);
        }
      }
    }

  } 
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;

#undef INFO_TYPE_ELEM_ARRAYS_MASK 
#undef INFO_TYPE_ELEM_ARRAYS_AREA 
#undef INFO_TYPE_ELEM_ARRAYS_MAX  
}


void MBMesh_serialize(MBMesh **mbmpp, char *buffer, int *length, 
                      int *offset, ESMC_InquireFlag *inquireflag, int *rc,
                      ESMCI_FortranStrLenArg buffer_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_serialize()"
  try {

    int localrc;
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    int *ip;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get Moab Mesh wrapper
    MBMesh *mbmp=reinterpret_cast<MBMesh*> (*mbmpp);

    //Get MOAB Mesh
    Interface *mesh=mbmp->mesh;

    // Calc Size
    int size = 0;

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
      *ip++ = mbmp->sdim;
      size++;
      *ip++ = mbmp->pdim;
      size++;
      *ip++ = mbmp->orig_sdim;
      size++;
      *ip++ = mbmp->coordsys;
      size++;
      *ip++ = mbmp->has_node_orig_coords;
      size++;
      *ip++ = mbmp->has_node_mask;
      size++;
      *ip++ = mbmp->has_elem_coords;
      size++;
      *ip++ = mbmp->has_elem_orig_coords;
      size++;
      *ip++ = mbmp->has_elem_mask;
      size++;
      *ip++ = mbmp->has_elem_area;
      size++;
      *ip++ = mbmp->has_elem_frac;
      size++;
      *ip++ = mbmp->is_split;
      size++;
      *ip++ = mbmp->max_non_split_id;
      size++;
    }

    // Adjust offset
    *offset += size * sizeof(int);

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

void MBMesh_deserialize(MBMesh **mbmpp, char *buffer, int *offset, int *rc,
                        ESMCI_FortranStrLenArg buffer_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_deserialize()"
  try {

    int localrc;
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    int *ip;
    int localsize = 0;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get pointer
    ip= (int *)(buffer + *offset);

    // Get values
    int sdim=*ip++;
    localsize++;
    int pdim=*ip++;
    localsize++;
    int orig_sdim=*ip++;
    localsize++;
    ESMC_CoordSys_Flag coordsys=static_cast<ESMC_CoordSys_Flag> (*ip++);
    localsize++;
    bool has_node_orig_coords=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_node_mask=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_coords=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_orig_coords=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_mask=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_area=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool has_elem_frac=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    bool is_split=static_cast<bool>(*ip);
    static_cast<int>(*ip++);
    localsize++;
    int max_non_split_id=static_cast<bool>(*ip++);
    localsize++;

    // Adjust offset
    *offset += localsize*sizeof(int);

    MBMesh_create(mbmpp, &pdim, &sdim, &coordsys, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
      return;

    MBMesh *mesh=reinterpret_cast<MBMesh*> (*mbmpp);
    mesh->has_node_orig_coords = has_node_orig_coords;
    mesh->has_node_mask = has_node_mask;
    mesh->has_elem_frac = has_elem_frac;
    mesh->has_elem_mask = has_elem_mask;
    mesh->has_elem_area = has_elem_area;
    mesh->has_elem_coords = has_elem_coords;
    mesh->has_elem_orig_coords = has_elem_orig_coords;
    mesh->is_split = is_split;
    mesh->max_non_split_id = max_non_split_id;

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}


/// REDIST ///

void MBMesh_createredistelems(MBMesh **src_meshpp, int *num_elem_gids, int *elem_gids,
                              MBMesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createredistelems()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    if ((*src_meshpp)->is_split) {
      // If split mesh expand ids
      int num_elem_gids_ws;
      int *elem_gids_ws=NULL;
      std::map<int,int> split_to_orig_id;

#undef debug_printelemgids
#ifdef debug_printelemgids
      for (int i=0; i<*num_elem_gids; ++i)
        printf("%d# elem gids %d\n", localPet, elem_gids[i]);
#endif

      mbmesh_expand_split_elem_ids(*src_meshpp, *num_elem_gids, elem_gids, 
                                   &num_elem_gids_ws, &elem_gids_ws, split_to_orig_id);

      mbmesh_redist_elem(*src_meshpp, &num_elem_gids_ws, elem_gids_ws, output_meshpp);

      (*output_meshpp)->is_split=(*src_meshpp)->is_split;
      (*output_meshpp)->max_non_split_id=(*src_meshpp)->max_non_split_id;
      (*output_meshpp)->split_to_orig_id=split_to_orig_id;

      // calculate split_id_to_frac map from other info
      mbmesh_calc_split_id_to_frac(*output_meshpp);

      // free split gids
      if (elem_gids_ws !=NULL) delete [] elem_gids_ws;
    } else {
      mbmesh_redist_elem(*src_meshpp, num_elem_gids, elem_gids, output_meshpp);
      (*output_meshpp)->is_split=(*src_meshpp)->is_split;
    }

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

void MBMesh_createredistnodes(MBMesh **src_meshpp, int *num_node_gids, int *node_gids,
                              MBMesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createredistnodes()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    mbmesh_redist_node(*src_meshpp, num_node_gids, node_gids, output_meshpp);

    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh split id postprocessing");
    // split element handling
    (*output_meshpp)->is_split=(*src_meshpp)->is_split;

    if ((*output_meshpp)->is_split) {
      mbmesh_set_split_orig_id_map(*src_meshpp, *output_meshpp);
      (*output_meshpp)->max_non_split_id=(*src_meshpp)->max_non_split_id;
      // RLO: not sure we need this if above is used
      // (*output_meshpp)->split_to_orig_id=(*src_meshpp)->split_to_orig_id;
    }
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh split id postprocessing");

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

void MBMesh_createredist(MBMesh **src_meshpp, int *num_node_gids, int *node_gids,
                         int *num_elem_gids, int *elem_gids,  
                         MBMesh **output_meshpp, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_createredist()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    if ((*src_meshpp)->is_split) {
      // If split mesh expand ids
      int num_elem_gids_ws;
      int *elem_gids_ws=NULL;
      std::map<int,int> split_to_orig_id;

      mbmesh_expand_split_elem_ids((*src_meshpp), *num_elem_gids, elem_gids, 
                                   &num_elem_gids_ws, &elem_gids_ws, split_to_orig_id);

      mbmesh_redist((*src_meshpp), num_node_gids, node_gids, 
                    &num_elem_gids_ws, elem_gids_ws, output_meshpp);
 
      (*output_meshpp)->is_split=(*src_meshpp)->is_split;
      (*output_meshpp)->max_non_split_id=(*src_meshpp)->max_non_split_id;
      (*output_meshpp)->split_to_orig_id=split_to_orig_id;

      // calculate split_id_to_frac map from other info
      mbmesh_calc_split_id_to_frac(*output_meshpp);

      // Free split gids
      if (elem_gids_ws !=NULL) delete [] elem_gids_ws;
    } else {
      mbmesh_redist((*src_meshpp), num_node_gids, node_gids, 
                    num_elem_gids, elem_gids, output_meshpp);
      (*output_meshpp)->is_split=(*src_meshpp)->is_split;
    }

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}


// This method verifies that nodes in node_gids array are the same as the local nodes in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of nodes in both cases are the same and that every
// entry in node_gids is contained in meshpp
void MBMesh_checknodelist(MBMesh **meshpp, int *_num_node_gids, int *node_gids,
                          int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_checknodelist()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // For convenience deref number
    int num_node_gids=*_num_node_gids;

    // Loop through counting local nodes
    Range nodes;
    merr=(*meshpp)->mesh->get_entities_by_dimension(0,0,nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    // list of the Mesh node gids
    std::vector<UInt> local_gids, local_owners;
    local_gids.reserve(nodes.size());
    local_owners.reserve(nodes.size());

    int num_local_nodes=0;
    Range::const_iterator si = nodes.begin(), se = nodes.end();
    for (; si != se; ++si) {
      const EntityHandle node = *si;

      int node_owner;
      merr=(*meshpp)->mesh->tag_get_data((*meshpp)->owner_tag, &node, 1, &node_owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // only consider local nodes
      if (node_owner == localPet) { 
        ++num_local_nodes;

        int gid;
        MBMesh_get_gid((*meshpp), node, &gid);
        local_gids.push_back(gid);

        local_owners.push_back(node_owner);
#undef print_nodeowners
#ifdef print_nodeowners
        printf("%d# checknodes - node %d owner %d\n", localPet, gid, elem_owner);
#endif
      }
    }

    // See if number of local nodes is the same
    if (num_node_gids != num_local_nodes) {
      Throw() << "Number of local nodes in mesh (" << num_local_nodes <<
                 ") different that number in list (" << num_node_gids << ")";
    }


    // Loop making sure nodes are all here
    for (int i=0; i<num_node_gids; i++) {
      std::vector<UInt>::const_iterator ni = std::find(local_gids.begin(), local_gids.end(), node_gids[i]);

      if (ni == local_gids.end()) {
        Throw() << "Node "<<node_gids[i] << " not found in Mesh.";
      }

      if (local_owners[ni-local_gids.begin()] != localPet) {
        Throw() << "Node "<<node_gids[i] << " in Mesh, but not local." 
                << local_owners[ni-local_gids.begin()] << "  " << localPet;
      }
    }

  } 
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}


// This method verifies that elems in elem_gids array are the same as the local elems in meshpp, otherwise
// it returns an error (used to test MeshRedist()).
// To do this check make sure the number of elems in both cases are the same and that every
// entry in elem_gids is contained in meshpp
void MBMesh_checkelemlist(MBMesh **meshpp, int *_num_elem_gids, int *elem_gids,
                          int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_checkelemlist()"
  try {
    int localrc, merr;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // For convenience deref number
    int num_elem_gids=*_num_elem_gids;

    // Loop through counting local elems
    Range elems;
    merr=(*meshpp)->mesh->get_entities_by_dimension(0, (*meshpp)->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    // list of the Mesh elem gids
    std::vector<UInt> local_gids, local_owners;
    local_gids.reserve(elems.size());
    local_owners.reserve(elems.size());

    int num_local_elems=0;
    Range::const_iterator si = elems.begin(), se = elems.end();
    for (; si != se; ++si) {
      const EntityHandle elem = *si;

      // Get element id
      int elem_id;
      MBMesh_get_gid(*meshpp, elem, &elem_id);

      // Don't do split elements
     if ((*meshpp)->is_split && (elem_id > (*meshpp)->max_non_split_id)) continue;

      int elem_owner;
      merr=(*meshpp)->mesh->tag_get_data((*meshpp)->owner_tag, &elem, 1, &elem_owner);
      ESMC_CHECK_MOAB_THROW(merr);

      if (elem_owner == localPet) {
        num_local_elems++;

        int gid;
        MBMesh_get_gid(*meshpp, elem, &gid);
        local_gids.push_back(gid);

        local_owners.push_back(elem_owner);
#undef print_elemowners
#ifdef print_elemowners
        printf("%d# checkelems - elem %d owner %d\n", localPet, gid, elem_owner);
#endif
      }
    }

    // See if number of local elems is the same
    if (num_elem_gids != num_local_elems) {
      Throw() << "Number of local elems in mesh (" << num_local_elems
              << ") different that number in list (" << num_elem_gids << ")";
    }

    // Loop making sure elems are all here
    for (int i=0; i<num_elem_gids; i++) {
      std::vector<UInt>::const_iterator ni = std::find(local_gids.begin(), local_gids.end(), elem_gids[i]);

      if (ni == local_gids.end()) {
        Throw() << "Node "<<elem_gids[i]<<" not found in Mesh.";
      }

      if (local_owners[ni-local_gids.begin()] != localPet) {
        Throw() << "Elem "<<elem_gids[i]<<" in Mesh, but not local." 
                << local_owners[ni-local_gids.begin()] << "  " << localPet;
      }
    }


#undef debug_printentities
#ifdef debug_printentities
    {
    Range nodes;
    merr=(*meshpp)->mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    Range::const_iterator ni = nodes.begin(), ne = nodes.end();
    for (; ni != ne; ++ni) {
      const EntityHandle ent = *ni;
      int gid;
      MBMesh_get_gid(*meshpp, ent, &gid);
      int owner;
      merr=(*meshpp)->mesh->tag_get_data((*meshpp)->owner_tag, &ent, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      printf("%d# checkelems - node %d owner %d\n", localPet, gid, owner);
    }

    Range elems;
    merr=(*meshpp)->mesh->get_entities_by_dimension(0, (*meshpp)->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    Range::const_iterator si = elems.begin(), se = elems.end();
    for (; si != se; ++si) {
      const EntityHandle ent = *si;
      int gid;
      MBMesh_get_gid((*meshpp), ent, &gid);
      int owner;
      merr=(*meshpp)->mesh->tag_get_data((*meshpp)->owner_tag, &ent, 1, &owner);
      ESMC_CHECK_MOAB_THROW(merr);

      printf("%d# checkelems - elem %d owner %d\n", localPet, gid, owner);
    }
    }
#endif

  }
  CATCH_MBMESH_RETURN(rc);

  if (rc!=NULL) *rc=ESMF_SUCCESS;
}

void MBMesh_FitOnVM(MBMesh **meshpp, VM **new_vm, int *rc)
{
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_MeshFitOnVM()"
  try {
    int localrc, merr;

    // set up Par
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *curr_vm = VM::getCurrent(&localrc);
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Get current VM size
    int curr_vm_size=curr_vm->getPetCount();

    // Get current VM rank
    int curr_vm_rank=curr_vm->getLocalPet();

    // Describe mapping of current PET
    int new_vm_rank=-1; // if there is no pet, set to -1
    if ((ESMC_NOT_PRESENT_FILTER(new_vm) != ESMC_NULL_POINTER) && *new_vm) {
      new_vm_rank=(*new_vm)->getLocalPet();
    }

    // Allocate array
    int *rank_map=new int[curr_vm_size];

    // Create array mapping from current vm to input vm
    localrc=curr_vm->allgather(&new_vm_rank,rank_map,sizeof(int));
    ESMC_CHECK_PASSTHRU_THROW(localrc);

#if 0
    // debug output
    for (int p=0; p<curr_vm_size; p++) {
      printf("%d# %d to %d\n",curr_vm_rank,p,rank_map[p]);
    }
#endif

    // Change proc numbers in mesh
    (*meshpp)->map_proc_numbers(curr_vm_size, rank_map);

    // Free map
    delete [] rank_map;

  }
  CATCH_MBMESH_RETURN(rc);

  // Set return code
  if (rc!=NULL) *rc = ESMF_SUCCESS;
} // ESMCI_MeshFitOnVM

void MBMesh_GetDimensions(MBMesh *meshp, int *sdim, int *pdim, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetDimensions()"
  try {
    *pdim = meshp->pdim;
    *sdim = meshp->orig_sdim;
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetCentroid(MBMesh *meshp, int *num_elem, double *elem_centroid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetCentroid()"
  try {
    // pass as ESMCI::InterArray<int> *elemCentroid, use extent[0] over num_elem

    char msg[256];
    if (*num_elem != meshp->num_elem()) {
      Throw () << "elemCentroid array must be of size " << meshp->num_elem();
    } else if (meshp->coordsys != ESMC_COORDSYS_CART) {
      Throw () << "Cannot yet return centroids for spherical coordinates.";
    }

    meshp->get_elem_centroids(elem_centroid);

  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetNodeCount(MBMesh *meshp, int *nodeCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetNodeCount()"
  try {
    *nodeCount = meshp->num_orig_node();
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetElemCount(MBMesh *meshp, int *elemCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemCount()"
  try {
    *elemCount = meshp->num_orig_elem();
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetElemConnCount(MBMesh *meshp, int *elemConnCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemConnCount()"
  try {
    *elemConnCount = meshp->num_orig_elem_conn();
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetElemInfoPresence(MBMesh *meshp, 
                                int *elemMaskIsPresent,
                                int *elemAreaIsPresent,
                                int *elemCoordsIsPresent,
                                int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemInfoPresence()"
  try {
    // Check if element mask is present
    *elemMaskIsPresent=0;
    if (meshp->has_elem_mask) *elemMaskIsPresent=1;
  
    // Check if element area is present
    *elemAreaIsPresent=0;
    if (meshp->has_elem_area) *elemAreaIsPresent=1;
  
    // Check if element coords are present
    *elemCoordsIsPresent=0;
    if (meshp->has_elem_coords) *elemCoordsIsPresent=1;
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}


void MBMesh_GetElemCreateInfo(MBMesh *meshp,
                              ESMCI::InterArray<int> *elemIds,
                              ESMCI::InterArray<int> *elemTypes,
                              ESMCI::InterArray<int> *elemConn,
                              ESMCI::InterArray<int> *elemMask,
                              ESMCI::InterArray<ESMC_R8> *elemArea,
                              ESMCI::InterArray<ESMC_R8> *elemCoords, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemCreateInfo()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Doesn't work with split meshes right now
    if (meshp->is_split)
      Throw () << "Can't get elem connection count from mesh containing >4 elements.";
    
    ////// Get some handy information //////
    int num_elems = meshp->num_elem();
    int orig_sdim = meshp->orig_sdim;
    int num_elem_conn = meshp->num_elem_conn();

    ////// Error check input arrays //////

    if (present(elemIds)) {
      if (elemIds->dimCount !=1)
        Throw () << "elemIds array must be 1D";
      if (elemIds->extent[0] != num_elems)
        Throw () << "elemIds array must be of size elemCount";
    }

    if (present(elemTypes)) {
      if (elemTypes->dimCount !=1)
        Throw () << "elemTypes array must be 1D";
      if (elemTypes->extent[0] != num_elems)
        Throw () << "elemTypes array must be of size elemCount";
    }

    if (present(elemConn)) {
      if (elemConn->dimCount !=1)
        Throw () << "elemConn array must be 1D";
      if (elemConn->extent[0] != num_elem_conn)
        Throw () << "elemConn array must be of size elemConnCount";
    }

    if (present(elemMask)) {
      if (!meshp->has_elem_mask)
        Throw () << "Element mask not present.";
      if (elemMask->dimCount !=1)
        Throw () << "elemMask array must be 1D";
      if (elemMask->extent[0] != num_elems)
        Throw () << "elemMask array must be of size elemCount";
    }

    if (present(elemArea)) {
      if (!meshp->has_elem_area)
        Throw () << "Element areas not present.";
      if (elemArea->dimCount !=1)
        Throw () << "elemArea array must be 1D";
      if (elemArea->extent[0] != num_elems)
        Throw () << "elemArea array must be of size elemCount";
    }

    if (present(elemCoords)) {
      if (!meshp->has_elem_coords)
        Throw () << "Element coords not present.";
      if (elemCoords->dimCount !=1)
        Throw () << "elemCoords array must be 1D";
      if (elemCoords->extent[0] != num_elems*orig_sdim)
        Throw () << "elemCoords array must be of size elemCount*sdim";
    }

    // Fill info in arrays 
    std::vector<EntityHandle> orig_elems;
    meshp->get_sorted_orig_elems(orig_elems);

    // If it was passed in, fill elementIds array
    if (present(elemIds)) {
      int *elemIds_array=elemIds->array;
      meshp->get_gid(orig_elems, elemIds_array);
    }

    // If it was passed in, fill elementTypes array
    if (present(elemTypes)) {
      int *elemTypes_array=elemTypes->array;
      meshp->get_elem_types(orig_elems, elemTypes_array);
    }

    // If it was passed in, fill elementIds array
    if (present(elemConn)) {
      int *elemConn_array=elemConn->array;
      meshp->get_elem_connectivity(orig_elems, elemConn_array);
    }

    // If it was passed in, fill elementMask array
    if (present(elemMask)) {
      int *elemMask_array=elemMask->array;
      meshp->get_elem_mask_val(orig_elems, elemMask_array);
    }

    // If it was passed in, fill elementArea array
    if (present(elemArea)) {
      ESMC_R8 *elemArea_array=elemArea->array;
      meshp->get_elem_area(orig_elems, elemArea_array);
    }

    // If it was passed in, fill elemCoords array
    if (present(elemCoords)) {
      ESMC_R8 *elemCoords_array=elemCoords->array;
      meshp->get_elem_orig_coords(orig_elems, elemCoords_array);
    }

  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}


void MBMesh_SetElemCreateInfo(MBMesh *meshp,
                              ESMCI::InterArray<int> *elemMask,
                              ESMCI::InterArray<ESMC_R8> *elemArea,
                              int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetElemCreateInfo()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    // Doesn't work with split meshes right now
    if (meshp->is_split)
      Throw () << "Can't set elem info for a mesh containing >4 elements.";
    
    ////// Get some handy information //////
    int num_elems=meshp->num_orig_elem();
    int orig_sdim=meshp->orig_sdim;

    ////// Error check input arrays //////

    if (present(elemMask)) {
      if (!meshp->has_elem_mask)
        Throw () << "Element mask not present.";
      if (elemMask->dimCount !=1)
        Throw () << "elemMask array must be 1D";
      if (elemMask->extent[0] != num_elems)
        Throw () << "elemMask array must be of size elemCount";
    }

    if (present(elemArea)) {
      if (!meshp->has_elem_area)
        Throw () << "Element areas not present.";
      if (elemArea->dimCount !=1)
        Throw () << "elemArea array must be 1D";
      if (elemArea->extent[0] != num_elems)
        Throw () << "elemArea array must be of size elemCount";
    }


    // Fill info in arrays 
    std::vector<EntityHandle> orig_elems;
    meshp->get_sorted_orig_elems(orig_elems);

    // If it was passed in, fill elementMask array
    if (present(elemMask)) {
      int *elemMask_array=elemMask->array;
      meshp->set_elem_mask_val(orig_elems, elemMask_array);
    }

    // If it was passed in, fill elementArea array
    if (present(elemArea)) {
      ESMC_R8 *elemArea_array=elemArea->array;
      meshp->set_elem_area(orig_elems, elemArea_array);
    }
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetNodeInfoPresence(MBMesh *meshp, 
                                int *nodeMaskIsPresent,
                                int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_GetNodeInfoPresence()"
  try {
    *nodeMaskIsPresent=0;
    if (meshp->has_node_mask) *nodeMaskIsPresent=1;
  }
  CATCH_MBMESH_RETURN(rc);
  
  if(rc != NULL) *rc = ESMF_SUCCESS;
}

void MBMesh_GetNodeCreateInfo(MBMesh *meshp,
                              ESMCI::InterArray<int> *nodeIds,
                              ESMCI::InterArray<ESMC_R8> *nodeCoords,
                              ESMCI::InterArray<int> *nodeOwners,
                              ESMCI::InterArray<int> *nodeMask,
                              int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_MeshGetNodeCreateInfo()"
  try {
    int localrc;

    // Initialize the parallel environment for mesh (if not already done)
    ESMCI::Par::Init("MESHLOG", false, VM::getCurrent(&localrc)->getMpi_c());
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_PASSTHRU_THROW(localrc);

    int num_nodes=meshp->num_orig_node();
    int orig_sdim=meshp->orig_sdim;

    // Error check input arrays

    // If nodeIds array exists, error check
    if (present(nodeIds)) {
      if (nodeIds->dimCount !=1)
        Throw () << "nodeIds array must be 1D";
      if (nodeIds->extent[0] != num_nodes)
        Throw () << "nodeIds array must be of size nodeCount";
    }

    // If nodeIds array exists, error check
    if (present(nodeCoords)) {
      if (nodeCoords->dimCount !=1)
        Throw () << "nodeCoords array must be 1D";
      if (nodeCoords->extent[0] != orig_sdim*num_nodes)
        Throw () << "nodeCoords array must be of size nodeCount*sdim";
    }

    // If nodeOwners array exists, error check
    if (present(nodeOwners)) {
      if (nodeOwners->dimCount !=1)
        Throw () << "nodeOwners array must be 1D";
      if (nodeOwners->extent[0] != num_nodes)
        Throw () << "nodeOwners array must be of size nodeCount";
    }

    // If nodeMask array exists, error check
    if (present(nodeMask)) {
      if (!meshp->has_node_mask)
        Throw () << "Node mask not present.";
      if (nodeMask->dimCount !=1)
        Throw () << "nodeMask array must be 1D";
      if (nodeMask->extent[0] != num_nodes)
        Throw () << "nodeMask array must be of size nodeCount";
    }

    // Fill info in arrays 
    std::vector<EntityHandle> orig_nodes;
    meshp->get_sorted_orig_nodes(orig_nodes);

    // If it was passed in, fill nodeIds array
    if (present(nodeIds)) {
      int *nodeIds_array=nodeIds->array;
      meshp->get_gid(orig_nodes, nodeIds_array); 
    }

    // If it was passed in, fill nodeCoords array
    if (present(nodeCoords)) {
      double *nodeCoords_array=nodeCoords->array;
      meshp->get_node_orig_coords(orig_nodes, nodeCoords_array); 
    }

    // If it was passed in, fill nodeOwners array
    if (present(nodeOwners)) {
      int *nodeOwners_array=nodeOwners->array;
      meshp->get_owners(orig_nodes, nodeOwners_array); 
    }

    // If it was passed in, fill nodeMask array
    if (present(nodeMask)) {
      int *nodeMask_array=nodeMask->array;
      meshp->get_node_mask_val(orig_nodes, nodeMask_array); 
    }

  }
  CATCH_MBMESH_RETURN(rc);

  if(rc != NULL) *rc = ESMF_SUCCESS;
}


#endif // ESMF_MOAB
