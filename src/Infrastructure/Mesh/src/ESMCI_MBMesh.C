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

#include <limits>
#include <string>
#include <ostream>
#include <iterator>
#include <vector>

#include "ESMCI_Macros.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"

#include "Mesh/include/ESMCI_MBMesh.h"
#include "Mesh/include/ESMCI_MBMesh_Glue.h"

#include "MBTagConventions.hpp"
#include "moab/ParallelComm.hpp"
#include "MBParallelConventions.h"

#include "VM/include/ESMC_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


using namespace ESMCI;
using namespace std;

// #define DEBUG_WRITE_MESH
// #define DEBUG_MOAB_GHOST_EXCHANGE


// NOTES (until I find a better place for them):
//   + MBMesh should be responsible for both coordinate forms (cart and original)
//      it should know about what coordinate system the mesh is in and be able to hand
//      out either type. If we have the mesh responsbile for doing coordinate conversion, 
//      then we know it's always happening the same way.
//  
// TODO:
//    + Think about orig position, it seems like we 
//      could make it much more useful for indexing into output arrays.
//    + We might need a status flag eventually for things like
//      non-active, pole, border, or ghostcells
//      Maybe make bad_id and bad_proc MBMesh parameters? 
//    + add an add_nodes() method that takes in lists and makes nodes in a group.
//      we can replace a lot of the functionality in MBMesh_addnodes() with that. 



#define ESMF
#define ESMC_METHOD "MBMesh::func()"

// Empty mesh
// TODO: Get rid of this once the more complete creation interfaces are used everywhere??
MBMesh::MBMesh() : sdim(0),pdim(0),mesh(NULL),verts(NULL) {

} 

// From inputs
// _pdim - parametric dimension
// _orig_sdim - the original spatial dimension (before converting to Cart 3D)
// _coordSys  - the coordinate system of the mesh
MBMesh::MBMesh(int _pdim, int _orig_sdim, ESMC_CoordSys_Flag _coordsys): 
  // TODO: Figure out if there's an empty tag to init tags to
  pdim(_pdim),
  sdim(0), 
  orig_sdim(_orig_sdim),
  coordsys(_coordsys), 
  mesh(NULL),
  num_verts(0),
  verts(NULL),
  num_elems(0),
  has_node_orig_coords(false),
  has_node_mask(false),
  has_elem_frac(false),
  has_elem_mask(false),
  has_elem_area(false),
  has_elem_coords(false),
  has_elem_orig_coords(false), 
  is_split(false),
  max_non_split_id(0) {

  // Moab error
  int merr;

  // Get cartesian dimension
  int cart_sdim;
  int localrc;
  localrc=ESMCI_CoordSys_CalcCartDim(coordsys, orig_sdim, &cart_sdim);
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL)) throw localrc;


  // Set cartesian spatial dim in struct
  sdim=cart_sdim;
  
  // Create MOAB Mesh
  mesh=new Core();

  // Default value
  int int_def_val=0;

  // Setup global id tag
  int_def_val=0;
  merr=mesh->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gid_tag, MB_TAG_DENSE, &int_def_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }
  
  // Setup orig_pos tag
  int_def_val=-1;
  merr=mesh->tag_get_handle("orig_pos", 1, MB_TYPE_INTEGER, orig_pos_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }
  
  // Setup owner tag
  int_def_val=-1;
  merr=mesh->tag_get_handle("owner", 1, MB_TYPE_INTEGER, owner_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }
  
  // Setup node_orig_coord tag
  has_node_orig_coords=false;
  if (coordsys != ESMC_COORDSYS_CART) {
    double dbl_def_val[3]={-1.0, -1.0, -1.0};
    merr=mesh->tag_get_handle("node_orig_coords", orig_sdim, MB_TYPE_DOUBLE, node_orig_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
    }
    has_node_orig_coords=true;
  }

  // Setup elem frac
  double dbl_def_val_1=1.0;
  merr=mesh->tag_get_handle("elem_frac", 1, MB_TYPE_DOUBLE, elem_frac_tag, MB_TAG_EXCL|MB_TAG_DENSE, &dbl_def_val_1);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }
  has_elem_frac=true;

} 


// The coords variable here is in the original representation, not converted to cart. 
EntityHandle MBMesh::add_node(double *orig_coords, int gid, int orig_pos, int owner) {

  // Error return codes
  int localrc;
  int merr;


  // Convert coords (if necessary)
  double cart_coords[3]={0.0,0.0,0.0};

  // Convert to cartesian
  ESMCI_CoordSys_ConvertToCart(coordsys, orig_sdim,
                               orig_coords, cart_coords);

  // Add vertex
  EntityHandle new_vert;
  merr=mesh->create_vertex(cart_coords,new_vert);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Set original coords (if necessary) 
  if (has_node_orig_coords) {
    // Set original coords
    merr=mesh->tag_set_data(node_orig_coords_tag, &new_vert, 1, orig_coords);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }
  }


  // Set Id
  merr=mesh->tag_set_data(gid_tag, &new_vert, 1, &gid);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Set original position
  merr=mesh->tag_set_data(orig_pos_tag, &new_vert, 1, &orig_pos);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  
  // Set Owner
  merr=mesh->tag_set_data(owner_tag, &new_vert, 1, &owner);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }
  
  // Increment number of verts
  num_verts++;

  // Output new vertex/node
  return new_vert;
}


// Get a Range of all nodes on this processor
void MBMesh::get_all_nodes(Range &all_nodes) {

  int merr=mesh->get_entities_by_dimension(0, 0, all_nodes);
  if (merr != MB_SUCCESS) {
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }
}

// Get a Range of all elems on this processor
void MBMesh::get_all_elems(Range &all_elems) {

  int merr=mesh->get_entities_by_dimension(0, pdim, all_elems);
  if (merr != MB_SUCCESS) {
    int localrc;
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }
}


void MBMesh::set_owner(EntityHandle eh, int owner) {

  // Error return codes
  int localrc;
  int merr;
  
  // Get Owner
  merr=mesh->tag_set_data(owner_tag, &eh, 1, &owner);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }  
}


int MBMesh::get_owner(EntityHandle eh) {

  // Error return codes
  int localrc;
  int merr;
  
  // Get Owner
  int owner;
  merr=mesh->tag_get_data(owner_tag, &eh, 1, &owner);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // return owner
  return owner;
}

int MBMesh::get_orig_pos(EntityHandle eh) {

  // Error return codes
  int localrc;
  int merr;
  
  // Get Owner
  int orig_pos;
  merr=mesh->tag_get_data(orig_pos_tag, &eh, 1, &orig_pos);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // return owner
  return orig_pos;
}


int MBMesh::get_gid(EntityHandle eh) {

  // Error return codes
  int localrc;
  int merr;
  
  // Get gid
  int gid;
  merr=mesh->tag_get_data(gid_tag, &eh, 1, &gid);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // return owner
  return gid;
}



// The coords variable here is in the original representation, not converted to cart. 
EntityHandle MBMesh::add_elem(EntityType elem_type, int num_nodes, EntityHandle *nodes, 
                              int gid, int orig_pos, int owner) {

  // Error return codes
  int localrc;
  int merr;

  // Add element
  EntityHandle new_elem;
  merr=mesh->create_element(elem_type, nodes, num_nodes, new_elem);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Set Id
  merr=mesh->tag_set_data(gid_tag, &new_elem, 1, &gid);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Set original position
  merr=mesh->tag_set_data(orig_pos_tag, &new_elem, 1, &orig_pos);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  
  // Set Owner
  merr=mesh->tag_set_data(owner_tag, &new_elem, 1, &owner);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }
  
  // Increment number of verts
  num_elems++;

  // Output new vertex/node
  return new_elem;
}


void MBMesh::setup_node_mask() {

  int merr,localrc;

  // Setup node mask tag
  int int_def_val=0; // So things are by default not masked
  merr=mesh->tag_get_handle("node_mask", 1, MB_TYPE_INTEGER, node_mask_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }
  
  // Setup node mask value tag
  int_def_val=0; // So things are by default not masked
  merr=mesh->tag_get_handle("node_mask_val", 1, MB_TYPE_INTEGER, node_mask_val_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Turn on masking
  has_node_mask=true;
}


void MBMesh::set_node_mask_val(EntityHandle eh, int mask_val) {

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, then leave
  // TODO: should this be an error instead?
  if (!has_node_mask) return;

  // Get Owner
  merr=mesh->tag_set_data(node_mask_val_tag, &eh, 1, &mask_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }  
}


void MBMesh::set_node_coords(EntityHandle eh, double *orig_coords) {

  // Error return codes
  int localrc;
  int merr;
  
  // Set original coords if present
  if (has_node_orig_coords) {
    merr=mesh->tag_set_data(node_orig_coords_tag, &eh, 1, orig_coords);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }  
  }

  // Convert and set vertex coords
  double cart_coords[3]={0.0,0.0,0.0};

  // Convert original coords to cartesian
  ESMCI_CoordSys_ConvertToCart(coordsys, orig_sdim,
                               orig_coords, cart_coords);

  // Set Vertex
  merr=mesh->set_coords(&eh,1,cart_coords);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

}


void MBMesh::setup_elem_mask() {

  int merr,localrc;

  // Setup elem mask tag
  int int_def_val=0; // So things are by default not masked
  merr=mesh->tag_get_handle("elem_mask", 1, MB_TYPE_INTEGER, elem_mask_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }
  
  // Setup elem mask value tag
  int_def_val=0; // So things are by default not masked
  merr=mesh->tag_get_handle("elem_mask_val", 1, MB_TYPE_INTEGER, elem_mask_val_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // Turn on masking
  has_elem_mask=true;
}

void MBMesh::set_elem_mask_val(EntityHandle eh, int mask_val) {

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, then leave
  // TODO: should this be an error instead?
  if (!has_elem_mask) return;

  // Get Owner
  merr=mesh->tag_set_data(elem_mask_val_tag, &eh, 1, &mask_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }  
}

int MBMesh::get_elem_mask_val(EntityHandle eh) {

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, then error
  if (!has_elem_mask) Throw() << "Element mask value not present in mesh.";

  // Get mask vale
  int mask_val;
  merr=mesh->tag_get_data(elem_mask_val_tag, &eh, 1, &mask_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }  

  // Output information
  return mask_val;
}

int *MBMesh::get_elem_mask_array(const Range &elems) {

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, then error
  if (!has_elem_mask) Throw() << "Element mask value not present in mesh.";

  // Get mask vale
  int *elem_mask;
  merr=mesh->tag_get_data(elem_mask_val_tag, elems, elem_mask);
  MBMESH_CHECK_ERR(merr, localrc) 

  // Output information
  return elem_mask;
}


void MBMesh::setup_elem_area() {

  int merr,localrc;

  // Setup elem area tag
  double dbl_def_val=0.0; 
  merr=mesh->tag_get_handle("elem_area", 1, MB_TYPE_DOUBLE, elem_area_tag, MB_TAG_EXCL|MB_TAG_DENSE, &dbl_def_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }
  
  // Turn on elem areas
  has_elem_area=true;
}

void MBMesh::set_elem_area(EntityHandle eh, double area) {

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, then leave
  // TODO: should this be an error instead?
  if (!has_elem_area) return;

  // Get Owner
  merr=mesh->tag_set_data(elem_area_tag, &eh, 1, &area);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }  
}

double MBMesh::get_elem_area(EntityHandle eh) {

  // Error return codes
  int localrc;
  int merr;
  
  // If no area, then error
  if (!has_elem_area) Throw() << "Element area not present in mesh.";

  // Get Owner
  double area;
  merr=mesh->tag_get_data(elem_area_tag, &eh, 1, &area);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }  

  // Output area
  return area;
}


void MBMesh::setup_elem_coords() {

  int merr,localrc;

  // Add element coords field
  double  dbl_def_val[3]= {0.0, 0.0, 0.0};
  merr=mesh->tag_get_handle("elem_coords", sdim, MB_TYPE_DOUBLE, elem_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
  }

  // If not cartesian then add original coordinates field
  if (coordsys != ESMC_COORDSYS_CART) {
    merr=mesh->tag_get_handle("elem_orig_coords", orig_sdim, MB_TYPE_DOUBLE, elem_orig_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
    }
    
    // Record the fact that it has original elem coords
    has_elem_orig_coords=true;
  }

  // Turn on elem coords
  has_elem_coords=true;
}


void MBMesh::set_elem_coords(EntityHandle eh, double *orig_coords) {

  // Error return codes
  int localrc;
  int merr;
  
  // Leave if no elem coords
  if (!has_elem_coords) return;

  // Set original coords if present
  if (has_elem_orig_coords) {
    merr=mesh->tag_set_data(elem_orig_coords_tag, &eh, 1, orig_coords);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
    }  
  }

  // Convert and set vertex coords
  double cart_coords[3]={0.0,0.0,0.0};

  // Convert original coords to cartesian
  ESMCI_CoordSys_ConvertToCart(coordsys, orig_sdim,
                               orig_coords, cart_coords);

  // Set cart coords
  // Set original coords if present
  merr=mesh->tag_set_data(elem_coords_tag, &eh, 1, cart_coords);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }  
 
}

// Setup MBMesh to operate in parallel by resolving shared ents, etc. 
void MBMesh::setup_parallel() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMesh::setup_parallel()"

  int merr,localrc;

  // Get MPI Communicator of current VM
  MPI_Comm mpi_comm=VM::getCurrent(&localrc)->getMpi_c();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // setup parallel comm, destroyed in MBMesh destructor   
  ParallelComm *pcomm= new ParallelComm(mesh, mpi_comm);

  // Get list of elements
  Range elems;
  merr=mesh->get_entities_by_dimension(0, pdim, elems);
  MBMESH_CHECK_ERR(merr, localrc);
    
  // Resolve object sharing 
  merr = pcomm->resolve_shared_ents(0, elems, pdim, pdim-1);
  MBMESH_CHECK_ERR(merr, localrc);

}

// Update parallel
// If the mesh has changed, call this to update the parallel connections
// between pieces of the mesh. (E.g. for haloing, etc...) 
// (This hasn't been tested, but it should be what has to happen.
//  TODO: Test!)
void MBMesh::update_parallel() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMesh::update_parallel()"

  int merr,localrc;

  // Get the indexed pcomm object from the interface                                                                
  // index 0 should return the one created inside MBMesh_addelements                                                
  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);

  pcomm->set_debug_verbosity(0);

  // strange errors with edges (in moab edges are processor boundaries) showed up with redistribution
  // the best solution we found with the moab team was to remove edges after redist and prior to resolve_shared_ents
  Range edges;
  merr=mesh->get_entities_by_dimension(0, 1, edges);
  MBMESH_CHECK_ERR(merr, localrc);
  pcomm->delete_entities(edges);

  // Get current list of elements
  Range elems;
  merr=mesh->get_entities_by_dimension(0, pdim, elems);
  MBMESH_CHECK_ERR(merr, localrc);
    
  // Resolve object sharing 
  merr = pcomm->resolve_shared_ents(0, elems, pdim, pdim-1);
  MBMESH_CHECK_ERR(merr, localrc);

}

// Do halo communication for all appropriate tags on nodes
// TODO: Right now this is a work around because MOAB doesn't let you specify which 
//       is the owned version of a shared entity (it decides itself). So we use reduce min
//       and set the non-owned versions to the max. 
//       do_internal_coords - if true also halo internal vertex coords. This is more expensive, because a new tag needs
//                   to be added and removed, so if not supplied it's default off. 
void MBMesh::halo_comm_nodes_all_tags(bool do_internal_coords) {
  int merr, localrc;

  // Get localPet
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Get the indexed pcomm object from the interface                                                                
  // index 0 should return the one created inside MBMesh_addelements                                                
  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);

  // Do node halo comm
  vector<Tag> node_tags;  
  // DON'T UPDATE THIS IS LOCAL INFO node_tags.push_back(this->gid_tag);
  // DON'T UPDATE THIS IS LOCAL INFO node_tags.push_back(this->orig_pos_tag);
  // DON'T UPDATE THIS IS LOCAL INFO node_tags.push_back(this->owner_tag);
  if (this->has_node_orig_coords) node_tags.push_back(this->node_orig_coords_tag);
  if (this->has_node_mask) {
    node_tags.push_back(this->node_mask_tag);
    node_tags.push_back(this->node_mask_val_tag);
  }


  // Get shared nodes
  Range shared_nodes;
  merr=pcomm->get_shared_entities(-1, shared_nodes,0);
  MBMESH_CHECK_ERR(merr, localrc);

  // Loop setting unowned data to max
  for (Range::iterator it=shared_nodes.begin(); it != shared_nodes.end(); it++) {
    EntityHandle node=*it;
    
    // Only reset if not local
    if (this->get_owner(node) != localPet) {

      // ONLY FOR TESTING
      //int max_orig_pos=std::numeric_limits<int>::max();
      //merr=mesh->tag_set_data(orig_pos_tag, &node, 1, &max_orig_pos);
      //MBMESH_CHECK_ERR(merr, localrc);
      
      // Set orig_coords
      if (this->has_node_orig_coords) {
        double max_orig_coords[3]={std::numeric_limits<double>::max(),
                               std::numeric_limits<double>::max(),
                               std::numeric_limits<double>::max()};
        merr=mesh->tag_set_data(node_orig_coords_tag, &node, 1, max_orig_coords);
        MBMESH_CHECK_ERR(merr, localrc);
      }

      // Set mask
      if (this->has_node_mask) {
        int max_mask=std::numeric_limits<int>::max();
        merr=mesh->tag_set_data(node_mask_tag, &node, 1, &max_mask);
        MBMESH_CHECK_ERR(merr, localrc);

        merr=mesh->tag_set_data(node_mask_val_tag, &node, 1, &max_mask);
        MBMESH_CHECK_ERR(merr, localrc);
      }
    }
  }

  // Add coords if asked
  Tag tmp_node_coords_tag;
  if (do_internal_coords) {

    // Add tmp coords tag
    double dbl_def_val[3]={-1.0, -1.0, -1.0};
    merr=mesh->tag_get_handle("tmp_node_coords", sdim, MB_TYPE_DOUBLE, tmp_node_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
    if (merr != MB_SUCCESS) {
      if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                       moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc;
    }
   
    // Add tag to list
    node_tags.push_back(tmp_node_coords_tag);

    // Loop setting unowned data to max
    for (Range::iterator it=shared_nodes.begin(); it != shared_nodes.end(); it++) {
      EntityHandle node=*it;

      // If local set from vertex coords
      if (this->get_owner(node) == localPet) {

        // get vertex coords
        double coords[3]={0.0,0.0,0.0};
        merr=mesh->get_coords(&node,1,coords);
        MBMESH_CHECK_ERR(merr, localrc);

        // set vertex coords in tmp tag
        merr=mesh->tag_set_data(tmp_node_coords_tag, &node, 1, coords);
        MBMESH_CHECK_ERR(merr, localrc);

      } else { // Not local set as max
        double max_coords[3]={std::numeric_limits<double>::max(),
                              std::numeric_limits<double>::max(),
                              std::numeric_limits<double>::max()};
        merr=mesh->tag_set_data(tmp_node_coords_tag, &node, 1, max_coords);
        MBMESH_CHECK_ERR(merr, localrc);
      }
    }
  }

  // Now do a halo by doing a reduce
  merr = pcomm->reduce_tags(node_tags, node_tags, MPI_MIN, shared_nodes);
  MBMESH_CHECK_ERR(merr, localrc);

  // Copy tmp coords back to vertex
  if (do_internal_coords) {
   
    // Loop setting unowned vertex to tmp tag
    for (Range::iterator it=shared_nodes.begin(); it != shared_nodes.end(); it++) {
      EntityHandle node=*it;

      // If not local set vertex coords from tmp tag
      if (this->get_owner(node) != localPet) {

        // get vertex coords from tmp tag
        double coords[3]={0.0,0.0,0.0};
        merr=mesh->tag_get_data(tmp_node_coords_tag, &node, 1, coords);
        MBMESH_CHECK_ERR(merr, localrc);

        // Put into vertex
        merr=mesh->set_coords(&node,1,coords);
        MBMESH_CHECK_ERR(merr, localrc);
      }
    }

    // TODO: REMOVE tmp_node_coords_tag HERE!!!!!
  }


}

// Do halo communication for all appropriate tags on elems
// TODO: Right now this is a work around because MOAB doesn't let you specify which 
//       is the owned version of a shared entity (it decides itself). So we use reduce min
//       and set the non-owned versions to the max. 
void MBMesh::halo_comm_elems_all_tags() {
  int merr, localrc;

  // Get localPet
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // Get the indexed pcomm object from the interface                                                                
  // index 0 should return the one created inside MBMesh_addelements                                                
  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);

  // Do elem halo comm
  vector<Tag> elem_tags;  
  // DON'T UPDATE THIS IS LOCAL INFO elem_tags.push_back(this->gid_tag);
  // DON'T UPDATE THIS IS LOCAL INFO elem_tags.push_back(this->orig_pos_tag);
  // DON'T UPDATE THIS IS LOCAL INFO elem_tags.push_back(this->owner_tag);
  if (this->has_elem_frac) elem_tags.push_back(this->elem_frac_tag);
  if (this->has_elem_mask) {
    elem_tags.push_back(this->elem_mask_tag);
    elem_tags.push_back(this->elem_mask_val_tag);
  }
  if (this->has_elem_area) elem_tags.push_back(this->elem_area_tag);
  if (this->has_elem_coords) elem_tags.push_back(this->elem_coords_tag);
  if (this->has_elem_orig_coords) elem_tags.push_back(this->elem_orig_coords_tag);


  // Get shared elems
  Range shared_elems;
  merr=pcomm->get_shared_entities(-1, shared_elems,pdim);
  MBMESH_CHECK_ERR(merr, localrc);

  // Loop setting unowned data to max
  for (Range::iterator it=shared_elems.begin(); it != shared_elems.end(); it++) {
    EntityHandle elem=*it;
    
    // Only reset if not local
    if (this->get_owner(elem) != localPet) {

      // ONLY FOR TESTING
      // int max_orig_pos=std::numeric_limits<int>::max();
      //merr=mesh->tag_set_data(orig_pos_tag, &node, 1, &max_orig_pos);
      //MBMESH_CHECK_ERR(merr, localrc);

      // Set elem_frac
      if (this->has_elem_frac) {
        double max_elem_frac=std::numeric_limits<double>::max();
        merr=mesh->tag_set_data(elem_frac_tag, &elem, 1, &max_elem_frac);
        MBMESH_CHECK_ERR(merr, localrc);
      }

      // Set elem_area
      if (this->has_elem_area) {
        double max_elem_area=std::numeric_limits<double>::max();
        merr=mesh->tag_set_data(elem_area_tag, &elem, 1, &max_elem_area);
        MBMESH_CHECK_ERR(merr, localrc);
      }

      
      // Set elem_orig_coords
      if (this->has_elem_orig_coords) {
        double max_orig_coords[3]={std::numeric_limits<double>::max(),
                               std::numeric_limits<double>::max(),
                               std::numeric_limits<double>::max()};
        merr=mesh->tag_set_data(elem_orig_coords_tag, &elem, 1, max_orig_coords);
        MBMESH_CHECK_ERR(merr, localrc);
      }

      // Set elem_coords
      if (this->has_elem_coords) {
        double max_coords[3]={std::numeric_limits<double>::max(),
                              std::numeric_limits<double>::max(),
                              std::numeric_limits<double>::max()};
        merr=mesh->tag_set_data(elem_coords_tag, &elem, 1, max_coords);
        MBMESH_CHECK_ERR(merr, localrc);
      }


      // Set mask
      if (this->has_elem_mask) {
        int max_mask=std::numeric_limits<int>::max();
        merr=mesh->tag_set_data(elem_mask_tag, &elem, 1, &max_mask);
        MBMESH_CHECK_ERR(merr, localrc);

        merr=mesh->tag_set_data(elem_mask_val_tag, &elem, 1, &max_mask);
        MBMESH_CHECK_ERR(merr, localrc);
      }
    }
  }

  // Now do a halo by doing a reduce
  merr = pcomm->reduce_tags(elem_tags, elem_tags, MPI_MIN, shared_elems);
  MBMESH_CHECK_ERR(merr, localrc);

#if 0
  Range elems;
  merr=this->mesh->get_entities_by_dimension(0, this->pdim, elems);
  MBMESH_CHECK_ERR(merr, localrc);

  merr = pcomm->exchange_tags(elem_tags, elem_tags, elems);
  MBMESH_CHECK_ERR(merr, localrc);
#endif

}



MBMesh::~MBMesh() {
  
  // Get rid of MOAB mesh
  if (mesh != NULL) delete mesh;

  // Get rid of list of verts
  if (verts != NULL) delete [] verts;
  
} 


/* XMRKX */
void MBMesh::debug_output_nodes() {
  int merr,localrc;

  // Get localPet
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // get the indexed pcomm object from the interface
  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);

  // Get range of nodes
  Range nodes;
  merr=mesh->get_entities_by_dimension(0, 0, nodes);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Loop over nodes outputting information
  for (Range::const_iterator it=nodes.begin(); it != nodes.end(); it++) {
    EntityHandle node=*it;

    // Output info
    cout<<localPet<<"# NODE:";
    
    // Output global id
    int gid=this->get_gid(node);
    cout<<" gid="<<gid;

    // output node owner
    int owner=this->get_owner(node);
    cout<<" owner="<<owner;

    // FOR DEBUGGING 
    // output moab owner
    //int moab_owner;
    //pcomm->get_owner(node,moab_owner);
    //cout<<" moab_owner="<<moab_owner;
    
    // get node orig pos
    int orig_pos=this->get_orig_pos(node);
    cout<<" orig_pos="<<orig_pos;

    // Output coords
    double coords[3];
    merr=mesh->get_coords(&node,1,coords);
    MBMESH_CHECK_ERR(merr, localrc);
    cout<<" coords=["<<coords[0]<<" "<<coords[1]<<" "<<coords[2]<<"]";


    //end line
    cout<<"\n";
  }
}

void MBMesh::debug_output_elems() {
  int merr,localrc;

  // Get localPet
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // get the indexed pcomm object from the interface
  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);

  // Get range of elems
  Range elems;
  merr=mesh->get_entities_by_dimension(0, pdim, elems);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Loop over nodes outputting information
  for (Range::const_iterator it=elems.begin(); it != elems.end(); it++) {
    EntityHandle elem=*it;

    // Output info
    cout<<localPet<<"# ELEM:";
    
    // Output global id
    int gid=this->get_gid(elem);
    cout<<" gid="<<gid;

    // output node owner
    int owner=this->get_owner(elem);
    cout<<" owner="<<owner;

    // FOR DEBUGGING 
    // output moab owner
    //int moab_owner;
    //pcomm->get_owner(node,moab_owner);
    //cout<<" moab_owner="<<moab_owner;
    
    // get node orig pos
    int orig_pos=this->get_orig_pos(elem);
    cout<<" orig_pos="<<orig_pos;

    //end line
    cout<<"\n";
  }
}

// DEPRECATED 
// TODO: Get rid of verts array
// Call after all nodes have been added to setup verts array
void MBMesh::setup_verts_array() {
  int merr, localrc;

  // Allocate storage for verts
  verts=new EntityHandle[num_verts];
  
  // Get range of nodes
  Range nodes;
  merr=mesh->get_entities_by_dimension(0, 0, nodes);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Complain if sizes don't match
  if (num_verts != nodes.size()) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,
                                     "num_verts and node.size() don't match as they should",
                                     ESMC_CONTEXT,&localrc)) throw localrc;
  }


  // Loop over nodes filling array
  int i=0;
  for (Range::const_iterator it=nodes.begin(); it != nodes.end(); it++) {
    EntityHandle node=*it;

    verts[i]=node;
    i++;
  }

  // Sort so ordered by orig pos
  // I'M NOT DOING THIS BECAUSE I DON'T THINK THAT 
  // IT'S NECESSARY AND THIS SUBROUTINE SHOULD GO AWAY SOON, BUT
  // LEAVING THIS NOTE JUST IN CASE...

}



void MBMesh::CreateGhost() {

  int merr, localrc;

  // Do this for now instead of initiating mesh parallel stuff
  // TODO: MAYBE EVENTUALLY PUT THIS INTO MBMesh???
  ESMC_VM vm;
  vm = ESMC_VMGetCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  int localPet, petCount;
  MPI_Comm mpi_comm;
  localrc = ESMC_VMGet(vm, &localPet, &petCount, NULL, &mpi_comm, NULL, NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // set up the MOAB parallel environment
  ParallelComm *pcomm= new ParallelComm(mesh, mpi_comm);

  // Get list of elements
  Range elems;
  merr=mesh->get_entities_by_dimension(0, pdim, elems);
  MBMESH_CHECK_ERR(merr, localrc);
    
  // Resolve object sharing 
  merr = pcomm->resolve_shared_ents(0, elems, pdim, pdim-1);
  MBMESH_CHECK_ERR(merr, localrc);


#ifdef DEBUG_MOAB_GHOST_EXCHANGE
  int nprocs = pcomm->size();
  int rank = pcomm->rank();

  Range shared_ents;
  // Get entities shared with all other processors
  merr = pcomm->get_shared_entities(-1, shared_ents);
  MBMESH_CHECK_ERR(merr, localrc);
  
  // Filter shared entities with not not_owned, which means owned
  Range owned_entities;
  merr = pcomm->filter_pstatus(shared_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &owned_entities);
  MBMESH_CHECK_ERR(merr, localrc);
    
  unsigned int nums[4] = {0}; // to store the owned entities per dimension
  for (int i = 0; i < 4; i++)
    // nums[i] = (nt)shared_ents.num_of_dimension(i);
    nums[i] = (int)owned_entities.num_of_dimension(i);
    
  vector<int> rbuf(nprocs*4, 0);
  MPI_Gather(nums, 4, MPI_INT, &rbuf[0], 4, MPI_INT, 0, mpi_comm);
  // Print the stats gathered:
  if (0 == rank) {
    for (int i = 0; i < nprocs; i++)
      cout << " Shared, owned entities on proc " << i << ": " << rbuf[4*i] << " verts, " <<
          rbuf[4*i + 1] << " edges, " << rbuf[4*i + 2] << " faces, " << rbuf[4*i + 3] << " elements" << endl;
  }
#endif

  // Now exchange 1 layer of ghost elements, using vertices as bridge
  merr = pcomm->exchange_ghost_cells(this->pdim, // int ghost_dim
                                     0, // int bridge_dim
                                     1, // int num_layers
                                     0, // int addl_ents
                                     true);// bool store_remote_handles
  MBMESH_CHECK_ERR(merr, localrc);

  vector<Tag> node_tags;
  vector<Tag> elem_tags;
  
  node_tags.push_back(this->gid_tag);
  node_tags.push_back(this->orig_pos_tag);
  node_tags.push_back(this->owner_tag);
  if (this->has_node_orig_coords) node_tags.push_back(this->node_orig_coords_tag);
  if (this->has_node_mask) {
    node_tags.push_back(this->node_mask_tag);
    node_tags.push_back(this->node_mask_val_tag);
  }
  
  elem_tags.push_back(this->gid_tag);
  elem_tags.push_back(this->orig_pos_tag);
  elem_tags.push_back(this->owner_tag);
  if (this->has_elem_frac) elem_tags.push_back(this->elem_frac_tag);
  if (this->has_elem_mask) {
    elem_tags.push_back(this->elem_mask_tag);
    elem_tags.push_back(this->elem_mask_val_tag);
  }
  if (this->has_elem_area) elem_tags.push_back(this->elem_area_tag);
  if (this->has_elem_coords) elem_tags.push_back(this->elem_coords_tag);
  if (this->has_elem_orig_coords) elem_tags.push_back(this->elem_orig_coords_tag);
  
  // pcomm->set_debug_verbosity(4);

  Range nodes;
  merr=this->mesh->get_entities_by_dimension(0, 0, nodes);
  MBMESH_CHECK_ERR(merr, localrc);

  merr = pcomm->exchange_tags(node_tags, node_tags, nodes);
  MBMESH_CHECK_ERR(merr, localrc);

  merr = pcomm->exchange_tags(elem_tags, elem_tags, elems);
  MBMESH_CHECK_ERR(merr, localrc);

#ifdef DEBUG_WRITE_MESH
  {void *mbptr = (void *) this;
  int rc;
  int len = 12; char fname[len];
  sprintf(fname, "meshdual_%d", localPet);
  MBMesh_write(&mbptr, fname, &rc, len);}
#endif

#ifdef DEBUG_MOAB_GHOST_EXCHANGE
  // Repeat the reports, after ghost exchange
  shared_ents.clear();
  owned_entities.clear();
  merr = pcomm->get_shared_entities(-1, shared_ents);
  MBMESH_CHECK_ERR(merr, localrc);
  
  merr = pcomm->filter_pstatus(shared_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &owned_entities);
  MBMESH_CHECK_ERR(merr, localrc);
  
  // Find out how many shared entities of each dimension are owned on this processor
  for (int i = 0; i < 4; i++)
    // nums[i] = (int)shared_ents.num_of_dimension(i);
    nums[i] = (int)owned_entities.num_of_dimension(i);
  
  // Gather the statistics on processor 0
  MPI_Gather(nums, 4, MPI_INT, &rbuf[0], 4, MPI_INT, 0, mpi_comm);
  if (0 == rank) {
    cout << " \n\n After exchanging one ghost layer: \n";
    for (int i = 0; i < nprocs; i++) {
      cout << " Shared, owned entities on proc " << i << ": " << rbuf[4*i] << " verts, " <<
          rbuf[4*i + 1] << " edges, " << rbuf[4*i + 2] << " faces, " << rbuf[4*i + 3] << " elements" << endl;
    }
  }
#endif

  // Get the indexed pcomm object from the interface
  delete pcomm;

}

// Change the proc numbers in a mesh to correspond to a different set. This isn't to 
// merge procs into one another, but to map them to different number. E.g. if they 
// are being switched to a different VM or MPI_COMM.
void MBMesh::map_proc_numbers(int num_procs, int *proc_map) {
  int merr,localrc;

  // Get range of nodes
  Range nodes;
  merr=mesh->get_entities_by_dimension(0, 0, nodes);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], 
                                     ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Loop through nodes changing owners to owners in new VM
  for (Range::const_iterator it=nodes.begin(); it != nodes.end(); it++) {
    EntityHandle node=*it;

    int orig_owner=this->get_owner(node);
    if ((orig_owner < 0) || (orig_owner > num_procs-1)) {
      Throw()<<" mesh node owner rank outside current vm";
    }

    // map to new owner rank in new vm
    int new_owner=proc_map[orig_owner];
    // Make sure that the new one is ok
    if (new_owner < 0) {
      Throw()<<" mesh node owner outside of new vm";
    }

    // Set new owner
    this->set_owner(node, new_owner);
  }

  // Get range of elems
  Range elems;
  merr=mesh->get_entities_by_dimension(0, pdim, elems);
  if (merr != MB_SUCCESS) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR,
                                     moab::ErrorCodeStr[merr], ESMC_CONTEXT,&localrc)) throw localrc;
  }

  // Loop through elems changing owners to owners in new VM
  for (Range::const_iterator it=elems.begin(); it != elems.end(); it++) {
    EntityHandle elem=*it;

    int orig_owner=this->get_owner(elem);
    // Error check owner
    if ((orig_owner < 0) || (orig_owner > num_procs-1)) {
      Throw()<<" mesh element owner rank outside current vm";
    }

    // map to new owner rank in new vm
    int new_owner=proc_map[orig_owner];
    // Make sure that the new one is ok
    if (new_owner < 0) {
       Throw()<<" mesh element owner outside of new vm";
    }
    // Set new owner
    this->set_owner(elem, new_owner);
  }

  // NOTE: not sure it this needs to be done for the mbmesh
  // // Change CommReg
  // CommReg::map_proc_numbers(num_procs, proc_map);
  // // Remove ghosting, because it would be wrong now that procs have changed.
  // RemoveGhost();
}


#endif // ESMF_MOAB



