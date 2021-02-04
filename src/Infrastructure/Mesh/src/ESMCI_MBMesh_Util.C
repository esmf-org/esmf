// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/Regridding/ESMCI_WMat.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/ESMCI_MBMesh_BBox.h>
#include <Mesh/include/ESMCI_MBMesh_Search.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>

#include "ESMCI_TraceMacros.h"  // for profiling

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>

#include <ESMCI_VM.h>

// #define DEBUG_POINTLIST

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


using namespace ESMCI;

// expects pcoords to have 3 elements
// expects pcoords in domain [-1,1] and translates to [0,1]
// useful for translating pcoords from MOAB to ESMF domain
void translate(double *pcoords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "translate"

  pcoords[0] = (pcoords[0]+1)/2;
  pcoords[1] = (pcoords[1]+1)/2;
  pcoords[2] = (pcoords[2]+1)/2;

}


// Get ids
void MBMesh_get_gid(MBMesh *mbmp, EntityHandle eh, int *gid) {
  int merr;

  merr=mbmp->mesh->tag_get_data(mbmp->gid_tag, &eh, 1, gid);
  if (merr != MB_SUCCESS) {
    Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
  }
}

// Get the coordinates of all the corners of an element
void MBMesh_get_elem_coords(MBMesh *mbmp, EntityHandle elem, int max_num_nodes, int *num_nodes, double *coords) {

  // MOAB Error
  int merr;

  // Coordinate dimension
  int sdim=mbmp->sdim;

  // Get Verts in element
  int num_verts;
  const EntityHandle *verts;
  merr=mbmp->mesh->get_connectivity(elem,verts,num_verts);
  if (merr != MB_SUCCESS) {
    Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
  }


  // make sure that we're not bigger than max size
  if (num_verts > max_num_nodes) {
    Throw() << "Element exceeds maximum poly size";
  }

  // Loop over verts
  int k=0;
  for(int i=0; i<num_verts; i++) {
    // Get vert coords
    double c[3];
    merr=mbmp->mesh->get_coords(verts+i,1,c);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Load coords
    for (int j=0; j<sdim; j++) {
      coords[k]=c[j];
      k++;
    }
  }

  // Get number of nodes
  *num_nodes=num_verts;
}


// Get coords, but flip so always counter clockwise
// Also gets rid of degenerate edges
// This version only works for elements of parametric_dimension = 2 and spatial_dim=3
void MBMesh_get_elem_coords_3D_ccw(MBMesh *mbmp, EntityHandle elem,
                                   int max_num_nodes, double *tmp_coords,
                                   int *num_nodes, double *coords) {
    int num_tmp_nodes;

    // Get element coords
    MBMesh_get_elem_coords(mbmp, elem, max_num_nodes, &num_tmp_nodes, tmp_coords);

    // Remove degenerate edges
    remove_0len_edges3D(&num_tmp_nodes, tmp_coords);

    // Check if degenerate
    // (if there's less than 3 no notion of CCW or CW)
    if (num_tmp_nodes <3) {
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
      *num_nodes=num_tmp_nodes;

      return;
    }

    // Get elem rotation
    bool left_turn;
    bool right_turn;
    rot_2D_3D_sph(num_tmp_nodes, tmp_coords, &left_turn, &right_turn);

    // Copy to output array swapping if necessary
    if (left_turn) {
      // Don't Swap
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
    } else {
      // Swap
      int j=0; int k=3*(num_tmp_nodes-1);
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[k];
        coords[j+1]=tmp_coords[k+1];
        coords[j+2]=tmp_coords[k+2];
        j+=3; k-=3;
      }
    }


  // Output num nodes
  *num_nodes=num_tmp_nodes;
}

// Compute centroid by averaging points
// centroid - the output centroid. Needs to be allocated to the spatial dimension of the mesh.
void MBMesh_get_elem_centroid(MBMesh *mbmp, EntityHandle elem, double *centroid) {

  // MOAB Error
  int merr;

  // Coordinate dimension
  int sdim=mbmp->sdim;

  // Get Verts in element
  int num_verts;
  const EntityHandle *verts;
  merr=mbmp->mesh->get_connectivity(elem,verts,num_verts);
  if (merr != MB_SUCCESS) {
    Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
  }

  // Init centroid to 0.0
  for (int d=0; d<sdim; d++) {
    centroid[d]=0.0;
  }

  // Loop over verts
  for(int i=0; i<num_verts; i++) {

    // Get vert coords
    double c[3];
    merr=mbmp->mesh->get_coords(verts+i,1,c);
    if (merr != MB_SUCCESS) {
      Throw() <<"MOAB ERROR: "<<moab::ErrorCodeStr[merr];
    }

    // Add coords
    for (int d=0; d<sdim; d++) {
      centroid[d] += c[d];
    }
  }

  // Divide by number of points
  double inv_num=1.0/((double)num_verts);
  for (int d=0; d<sdim; d++) {
    centroid[d] *= inv_num;
  }
}

// Get elem gids owned by this processor and sorted in order of their original position
void MBMesh_get_local_elem_gids(MBMesh *mbmp, std::vector<UInt> &egids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_get_local_elem_gids()"

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
    egids.push_back((UInt)pos_and_gids[i].second);

    // printf("pos=%d egids=%d\n",pos_and_gids[i].first,pos_and_gids[i].second);

  }
}


// This method converts a Mesh to a PointList
ESMCI::PointList *MBMesh_to_PointList(MBMesh *mesh, ESMC_MeshLoc_Flag meshLoc, ESMCI::InterArray<int> *maskValuesArg, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMeshToPointList()"

  ESMCI::PointList *plp = NULL;

  int localrc;
  Tag src_mask_val;

  // Get localPet
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Initialize the parallel environment for mesh (if not already done)
  ESMCI::Par::Init("MESHLOG", false /* use log */,VM::getCurrent(&localrc)->getMpi_c());
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  //Get MOAB Mesh
  Interface *moab_mesh=mesh->mesh;

  // MOAB error
  int merr;
  Range range_ent;

  if (meshLoc == ESMC_MESHLOC_NODE) {

    // if (!mesh->has_node_orig_coords) {
    //   int localrc;
    //   if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
    //      "- mesh node coordinates unavailable",
    //      ESMC_CONTEXT, &localrc)) throw localrc;
    // }

    src_mask_val = mesh->node_mask_val_tag;

    merr=moab_mesh->get_entities_by_dimension(0,0,range_ent);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

  } else if (meshLoc == ESMC_MESHLOC_ELEMENT) {

    //need check here to see that elem coordinates are set
    if (!mesh->has_elem_coords) {
      int localrc;
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
         "- mesh element coordinates unavailable",
         ESMC_CONTEXT, &localrc)) throw localrc;
    }

    src_mask_val = mesh->elem_mask_val_tag;

    merr=moab_mesh->get_entities_by_dimension(0,mesh->pdim,range_ent);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }

  } else {
    //unknown meshLoc
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
       "- illegal value specified for mesh location",
       ESMC_CONTEXT, &localrc)) throw localrc;
  }

  int numMaskValues;
  int *ptrMaskValues;

  if (present(maskValuesArg)) {
    numMaskValues=maskValuesArg->extent[0];
    ptrMaskValues=&(maskValuesArg->array[0]);
  } else {
    numMaskValues=0;
    ptrMaskValues = NULL;
  }

  int num_local_pts=0;
  for(Range::iterator it=range_ent.begin(); it !=range_ent.end(); it++) {
    const EntityHandle *ent=(&*it);

    // Get owner
    int owner;
    merr=moab_mesh->tag_get_data(mesh->owner_tag, ent, 1, &owner);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }
    if (owner == localPet) {
      if (present(maskValuesArg)) {
        int mv;
        // get the mask values out of the moab mesh
        merr=moab_mesh->tag_get_data(src_mask_val, ent, 1, &mv);
        if (merr != MB_SUCCESS)
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
            moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
        // Only put objects in if they're not masked
        bool mask=false;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];

          if (mv == mvi) {
            mask=true;
            break;
          }
        }
        if (mask) continue;
      }
      
      num_local_pts++;
    }
  }

  // Create PointList
  plp = new PointList(num_local_pts, mesh->sdim);

  // Loop through adding local nodes
  for(Range::iterator it=range_ent.begin(); it !=range_ent.end(); it++) {
    const EntityHandle *ent=(&*it);

    // Get the owner again..
    int owner;
    merr=moab_mesh->tag_get_data(mesh->owner_tag, ent, 1, &owner);
    if (merr != MB_SUCCESS)
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
        moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;

    // if this node is owned, add it to the pointlist
    double c[3];
    //vector<EntityHandle> nodev;
    if (owner == localPet) {

      if (present(maskValuesArg)) {
        // get the mask values out of the moab mesh
        int mv;
        merr=moab_mesh->tag_get_data(src_mask_val, ent, 1, &mv);
        if (merr != MB_SUCCESS)
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
            moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;

        // Only put objects in if they're not masked
        bool mask=false;
        for (int i=0; i<numMaskValues; i++) {
          int mvi=ptrMaskValues[i];
          if (mv == mvi) {
            mask=true;
            break;
          }
        }
        if (mask) continue;
      }
      
      if (meshLoc == ESMC_MESHLOC_NODE) {
        merr = moab_mesh->get_coords(ent, 1, c);
        if (merr != MB_SUCCESS)
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
            moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      } else if (meshLoc == ESMC_MESHLOC_ELEMENT) {
        merr=moab_mesh->tag_get_data(mesh->elem_coords_tag, ent, 1, c);
        if (merr != MB_SUCCESS)
          if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
            moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      }
      
      int id;
      merr=moab_mesh->tag_get_data(mesh->gid_tag, ent, 1, &id);
      if (merr != MB_SUCCESS)
        if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
          moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
      // printf("PET%d: id = %d, c = [%f, %f, %f]\n", localPet, id, c[0], c[1], c[2]);
      
      plp->add(id, c);
    }
  }

  // sort the pointlist
  plp->sort_by_id();
  
#ifdef DEBUG_POINTLIST
  {printf("%d# MB2P POINTLIST(%d) [", localPet, plp->get_curr_num_pts());
  for (int p = 0; p < plp->get_curr_num_pts(); ++p) {
    const int *id = plp->get_id_ptr(p);
    printf("%d, ", plp->get_id_ptr(p));
  }
  printf("]\n");}
#endif

  if (rc!=NULL) *rc=ESMF_SUCCESS;
  return plp;

}


// Get the number of nodes from the ESMF element type
 // Get the element topology
int MBMesh_ElemType2NumNodes(int pdim, int etype) {
  if (pdim==2) {
    return etype;
  } else if (pdim==3) {
    if (etype==10) return 4;
    else if (etype==12) return 8;
    else {
      Throw () << " unrecognized ESMF element type: "<<etype;
     }
   }
}

// Go from number of corner nodes to esmf_etype
int MBMesh_num_nodes_to_esmf_etype(int pdim, int num_corner_nodes) {
  if (pdim==2) {
    if (num_corner_nodes==3) return 3;
    else if (num_corner_nodes==4) return 4;
    else {
      Throw() << " unsupported number of corner nodes for 2D elements: "<<num_corner_nodes; 
    }
  } else if (pdim==3) {
    if (num_corner_nodes==4) return 10;
    else if (num_corner_nodes==8) return 12;
    else {
      Throw() << " unsupported number of corner nodes for 3D elements: "<<num_corner_nodes; 
    }
  }
}



// This method takes a set of info and adds the contained nodes to the mbmp mesh in a group
// (Adding in groups seems to be more efficient in MOAB)
void MBMesh_add_nodes_in_a_group(MBMesh *mbmp,   // Mesh to add elems to
                                 int num_nodes, // the number of new nodes to add
                                 int *node_ids,   // id for each new node
                                 double *node_coords, // node original coords
                                 int *node_orig_pos, // orig pos of each new node (if NULL just order starting from 0)
                                 int *node_owners, // owner for each new node
                                                   // TODO: allow NULL for above
                                 int *node_mask_vals, // optional elem mask value (if NULL ignored)
                                 int *node_masks // optional elem mask (if NULL ignored)
                                 ) {


    // Create a new set of nodes with basic info
    Range added_nodes;
    mbmp->add_nodes(num_nodes,     
                    node_coords,
                    node_ids,          
                    node_orig_pos,
                    node_owners,
                    added_nodes); 
    
    // If present set node mask values
    if (node_mask_vals != NULL) {
      mbmp->set_node_mask_val(added_nodes, node_mask_vals);
    }

    // If present set node mask
    if (node_masks != NULL) {
      mbmp->set_node_mask(added_nodes, node_masks);
    }
}


// Wrapper for the above function that takes vectors instead of arrays
void MBMesh_add_nodes_in_a_group(MBMesh *mbmp,   // Mesh to add elems to
                                 std::vector<int> &node_ids,   // id for each new node
                                 std::vector<double> &node_coords, // node original coords
                                 std::vector<int> &node_orig_pos, // orig pos of each new node (if NULL just order starting from 0)
                                 std::vector<int> &node_owners, // owner for each new node
                                 std::vector<int> &node_mask_vals, // optional elem mask value (if NULL ignored)
                                 std::vector<int> &node_masks // optional elem mask (if NULL ignored)
                                 ) {


  // Number of nodes to create
  int num_nodes=node_ids.size();
  
  // Get pointer to node_ids data
  int *node_ids_ptr=node_ids.data();

  // Get pointer to node_coords data
  if (node_coords.size() != num_nodes*mbmp->orig_sdim) {
    Throw() << " node_coords must have the size of node_ids * the user facing (orig.) spatial dim";
  }
  double *node_coords_ptr=node_coords.data();

  // Get pointer to node orig pos data
  // (If it's not there, then make it NULL, otherwise make sure the size
  //  is correct)
  int *node_orig_pos_ptr; 
  if (node_orig_pos.empty()) {
    node_orig_pos_ptr=NULL;
  } else {  
    if (node_orig_pos.size() != num_nodes) {
      Throw() << " node_orig_pos must have same size as node_ids";
    }
    node_orig_pos_ptr=node_orig_pos.data();
  }

  // Get pointer to node owners data
  if (node_owners.size() != num_nodes) {
    Throw() << " node_owners must have the same size as nodes_ids";
  }
  int *node_owners_ptr=node_owners.data();

  // Get pointer to node_mask_vals data
  // (If it's not there, then make it NULL, otherwise make sure the size
  //  is correct)
  int *node_mask_vals_ptr; 
  if (node_mask_vals.empty()) {
    node_mask_vals_ptr=NULL;
  } else {  
    if (node_mask_vals.size() != num_nodes) {
      Throw() << " node_mask_vals must have same size as node_ids";
    }
    node_mask_vals_ptr=node_mask_vals.data();
  }

  // Get pointer to node_masks data
  // (If it's not there, then make it NULL, otherwise make sure the size
  //  is correct)
  int *node_masks_ptr; 
  if (node_masks.empty()) {
    node_masks_ptr=NULL;
  } else {  
    if (node_masks.size() != num_nodes) {
      Throw() << " node_masks must have same size as node_ids";
    }
    node_masks_ptr=node_masks.data();
  }

  // Call into pointer version
  MBMesh_add_nodes_in_a_group(mbmp,
                              num_nodes,
                              node_ids_ptr, 
                              node_coords_ptr,
                              node_orig_pos_ptr,
                              node_owners_ptr,
                              node_mask_vals_ptr,
                              node_masks_ptr);
}

// This routine takes in a some  element creation information, and sees if any elements need to be split
void MBMesh_detect_split_elems(
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
            if (elemConn[conn_pos] != MBMESH_POLYBREAK_IND) {
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


// This routine should only be run when split elems have been detected by the above. 
// It takes in a set of element creation information, and outputs the new split element creation information
void MBMesh_generate_info_for_split_elems(
                                    // In
                                    bool is_split_local,
                                    int pdim, 
                                    int orig_sdim, 
                                    int sdim,
                                    int num_elems,
                                    int *elemId,
                                    int *elemType, 
                                    int *orig_pos, 
                                    int *elemMask,
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
                                    int *&orig_pos_wsplit,
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
      ThrowRequire(orig_pos != NULL);
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
          if (elemConn[conn_pos] != MBMESH_POLYBREAK_IND) {
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
      orig_pos_wsplit=new int[num_elems_wsplit];
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
              if (elemConn[conn_pos] == MBMESH_POLYBREAK_IND) conn_pos++;
              
              // Find sub-elements (may be only one)
              int subelem_size=0;
              for (int i=conn_pos; i<end_of_elem; i++) {
                if (elemConn[i] == MBMESH_POLYBREAK_IND) break;
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
                orig_pos_wsplit[split_elem_pos]=orig_pos[e];
                first_elem=false;
              } else {
                elemId_wsplit[split_elem_pos]=curr_extra_id;
                orig_pos_wsplit[split_elem_pos]=MBMesh::ORIG_POS_SPLITELEM;
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
            orig_pos_wsplit[split_elem_pos]=orig_pos[e];
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


// Get MBMesh entity type from parametric dimension and esmf entity type
EntityType MBMesh_get_entity_type(int pdim, int esmf_etype) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_get_entity_type()"
  try {
    if (pdim==2) {
      if (esmf_etype==3) return MBTRI;
      else if (esmf_etype==4) return MBQUAD;
      else {
        Throw () << " unrecognized ESMF element type ="<<esmf_etype;
      }
    } else if (pdim==3) {
      if (esmf_etype==10) return MBTET;
      else if (esmf_etype==12) return MBHEX;
      else {
        Throw () << " unrecognized ESMF element type ="<<esmf_etype;
      }
    }
  }
  CATCH_MBMESH_RETHROW
}


// This method handles adding elements where the entire set is one tyoe
// This method takes a set of element info and adds the contained elements to MOAB in groups
// (Adding in groups by type seems to be more efficient in MOAB)
void _add_elems_all_one_type(MBMesh *mbmp, int all_elems_owner, 
                             int curr_elem_type,
                             int num_elems,   
                             int *elemId, 
                             int *orig_pos, 
                             int *elemMaskVal,
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
    num_nodes_per_elem=MBMesh_ElemType2NumNodes(mbmp->pdim, curr_elem_type);  
    etype=MBMesh_get_entity_type(mbmp->pdim, curr_elem_type);
    
     
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

    
    // Allocate owner list
    ThrowRequire(num_elems >= 0);
    int *owners=new int[num_elems];

    // Fill owner list
    for (int i=0; i<num_elems; i++) {
      owners[i]=all_elems_owner;
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

    // Mask Val
    if ((elemMaskVal != NULL) && mbmp->has_elem_mask) {
      mbmp->set_elem_mask_val(added_elems, elemMaskVal);
    }


    // Mask
    if ((elemMask != NULL) && mbmp->has_elem_mask) {
      mbmp->set_elem_mask(added_elems, elemMask);
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
    delete [] owners;
  }
  CATCH_MBMESH_RETHROW
}


// This method handles adding elements where the element informatiomn contains
// elements of multiple types. It goes through and gathers the elements of a particular
// type and adds that set
// (Adding in groups by type seems to be more efficient in MOAB)
void _add_elems_multiple_types(MBMesh *mbmp, int all_elems_owner, 
                               int curr_elem_type,
                               int num_elems,   
                               int *elemId, 
                               int *elemType, 
                               int *orig_pos, 
                               int *elemMaskVal,
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
    ThrowRequire(orig_pos != NULL);
    if (areaPresent) ThrowRequire(elemArea != NULL);
    if (elemCoordsPresent) ThrowRequire(elemCoords != NULL);
    ThrowRequire(elem_id_buff != NULL);
    ThrowRequire(elem_owner_buff != NULL);
    ThrowRequire(elem_orig_pos_buff != NULL);
    ThrowRequire(tmp_buff != NULL);


    // Get number of nodes_per_elem and MOAB entity type
    int num_nodes_per_elem=0;
    EntityType etype;
    num_nodes_per_elem=MBMesh_ElemType2NumNodes(mbmp->pdim,
                                         curr_elem_type);  
    etype=MBMesh_get_entity_type(mbmp->pdim,curr_elem_type);
    
     
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
        elem_owner_buff[pos]=all_elems_owner;

        // Fill orig pos
        elem_orig_pos_buff[pos]=orig_pos[e];

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
        int tmp_num_nodes_per_elem=MBMesh_ElemType2NumNodes(mbmp->pdim, elemType[e]);  

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
    if ((elemMaskVal != NULL) && mbmp->has_elem_mask) {

      // Check size of buffer and set
      if (tmp_buff_size < num_elems_of_type*sizeof(int)) Throw() << "tmp_buff too small to hold elem mask val information.";
      int *elem_mask_val_buff=(int *)tmp_buff;
      
      // copy info into buffer
      int pos=0;
      for (int e=0; e<num_elems; ++e) {
      
        // Only do if the correct type
        if (elemType[e] == curr_elem_type) {

          // Fill mask
          elem_mask_val_buff[pos]=elemMaskVal[e];
        
          // Advance to next pos
          pos++;
        }
      }

      // Set info in element field
      mbmp->set_elem_mask_val(added_elems, elem_mask_val_buff);
    }

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
      mbmp->set_elem_mask(added_elems, elem_mask_buff);
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
void MBMesh_add_elems_in_groups_by_type(MBMesh *mbmp,   // Mesh to add elems to
                                        int all_elems_owner, // all new elems owner will be set to this
                                        int num_elems, // the number of new elems to add
                                        int *elemId,   // list of ids for each new elem
                                        int *elemType, // The ESMF elemType (i.e. the same as num corners for 2D)
                                                       // TODO: change elemType to be num_corner_nodes ???
                                        int *orig_pos, // orig pos of new elems in input list
                                                       // TODO: allow NULL for above
                                        int *elemMaskVal, // optional elem mask value (if NULL ignored)
                                        int *elemMask, // optional elem mask (if NULL ignored)
                                        int areaPresent, double *elemArea,
                                        int elemCoordsPresent, double *elemCoords,
                                        int *elemConn  // 1-based elem connections into local nodes (matches with node orig_pos+1)
                                                       // TODO: Change this so internally all 0-based
                                        ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh_add_elems_in_groups_by_type()"
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

      _add_elems_all_one_type(mbmp, all_elems_owner, 
                              elem_types[0],
                              num_elems,   
                              elemId, 
                              orig_pos, 
                              elemMaskVal,
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

      int num_conn=num_per_type[i]*MBMesh_ElemType2NumNodes(mbmp->pdim,elem_types[i]);  
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
    if (elemMaskVal != NULL) {
      int max_size_of_elemMaskVal=max_num_elems_per_type*sizeof(int);
      if (max_size_of_elemMaskVal > tmp_buff_size) tmp_buff_size=max_size_of_elemMaskVal;
    } 
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

      _add_elems_multiple_types(mbmp, all_elems_owner, elem_types[t],
                                num_elems, elemId, elemType, orig_pos, 
                                elemMaskVal, 
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

// Vector version of the above
void MBMesh_add_elems_in_groups_by_type(MBMesh *mbmp, 
                                        int all_elems_owner, // all new elems owner will be set to this
                                        std::vector<int> &elem_ids,
                                        std::vector<int> &elem_types,
                                        std::vector<int> &elem_orig_pos,
                                        std::vector<int> &elem_mask_vals,
                                        std::vector<int> &elem_masks,
                                        std::vector<double> &elem_areas,
                                        std::vector<double> &elem_coords,
                                        std::vector<int> &elem_conns
                                        ) {

  // Number of elements to create
  int num_elems=elem_ids.size();
  
  // Get pointer to elem ids data
  int *elem_ids_ptr=elem_ids.data();

  // Get pointer to elem types data
  if (elem_types.size() != num_elems) {
    Throw() << " elem_types must have same size as elem_ids";
  }
  int *elem_types_ptr=elem_types.data();

  // Get pointer to elem orig_pos data
  if (elem_orig_pos.size() != num_elems) {
    Throw() << " elem_orig_pos must have same size as elem_ids";
  }
  int *elem_orig_pos_ptr=elem_orig_pos.data();

  // Get pointer to elem mask value data
  // (If it's not there, then make it NULL, otherwise make sure the size
  //  is correct)
  int *elem_mask_vals_ptr; 
  if (elem_mask_vals.empty()) {
    elem_mask_vals_ptr=NULL;
  } else {  
    if (elem_mask_vals.size() != num_elems) {
      Throw() << " elem_mask_vals must have same size as elem_ids";
    }
    elem_mask_vals_ptr=elem_mask_vals.data();
  }

  // Get pointer to elem mask data
  // (If it's not there, then make it NULL, otherwise make sure the size
  //  is correct)
  int *elem_masks_ptr; 
  if (elem_masks.empty()) {
    elem_masks_ptr=NULL;
  } else {  
    if (elem_masks.size() != num_elems) {
      Throw() << " elem_masks must have same size as elem_ids";
    }
    elem_masks_ptr=elem_masks.data();
  }


  // Get pointer to elem area data
  // (If it's not there, then make it NULL, otherwise make sure the size
  //  is correct)
  double *elem_areas_ptr; 
  int areaPresent;
  if (elem_areas.empty()) {
    elem_areas_ptr=NULL;
    areaPresent=0;
  } else {  
    if (elem_areas.size() != num_elems) {
      Throw() << " elem_areas must have same size as elem_ids";
    }
    elem_areas_ptr=elem_areas.data();
    areaPresent=1;
  }

  // Get pointer to elem coord data
  // (If it's not there, then make it NULL, otherwise make sure the size
  //  is correct)
  double *elem_coords_ptr; 
  int elemCoordsPresent;
  if (elem_coords.empty()) {
    elem_coords_ptr=NULL;
    elemCoordsPresent=0;
  } else {  
    if (elem_coords.size() != num_elems*mbmp->orig_sdim) {
      Throw() << " elem_coords must have same size as elem_ids";
    }
    elem_coords_ptr=elem_coords.data();
    elemCoordsPresent=1;
  }

  // It's not easy to figure out the size of elem_conn, so 
  // just get pointer for now.
  int *elem_conns_ptr=elem_conns.data();

  // Call into non-vector version
  MBMesh_add_elems_in_groups_by_type(mbmp, 
                                     all_elems_owner,
                                     num_elems,
                                     elem_ids_ptr, 
                                     elem_types_ptr, 
                                     elem_orig_pos_ptr,
                                     elem_mask_vals_ptr,
                                     elem_masks_ptr,
                                     areaPresent, elem_areas_ptr,
                                     elemCoordsPresent, elem_coords_ptr,
                                     elem_conns_ptr);

}




#endif // ESMF_MOAB
