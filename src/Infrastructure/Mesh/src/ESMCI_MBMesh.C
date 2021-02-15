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
#include "moab/ReadUtilIface.hpp"
#include "moab/Util.hpp"


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


int MBMesh::M2EType(int key, int range) {
  return
    (range == 0) ? throw "Key not present":
    (MOAB2ESMFElemType[range - 1].first == key) ? 
      MOAB2ESMFElemType[range - 1].second:
    M2EType(key, range - 1);
};

int MBMesh::M2ETypeT(int value, int range) {
  return
    (range == 0) ? throw "Value not present":
    (MOAB2ESMFElemType[range - 1].second == value) ? 
      MOAB2ESMFElemType[range - 1].first:
    M2ETypeT(value, range - 1);
};

int MBMesh::E2MType(int key, int range) {
  return
    (range == 0) ? throw "Key not present":
    (ESMF2MOABElemType[range - 1].first == key) ? 
      ESMF2MOABElemType[range - 1].second:
    E2MType(key, range - 1);
};

int MBMesh::E2MTypeT(int value, int range) {
  return
    (range == 0) ? throw "Value not present":
    (ESMF2MOABElemType[range - 1].second == value) ? 
      ESMF2MOABElemType[range - 1].first:
    E2MTypeT(value, range - 1);
};


// Empty mesh
// EVENTUALLY MAKE THIS PRIVATE TO ENCOURAGE THE USE OF THE CONSTRUCTOR
// THAT SETS EVEYTHING UP CORRECTLY
MBMesh::MBMesh(): 
  pdim(0),
  sdim(0), 
  orig_sdim(0),
  coordsys(ESMC_COORDSYS_UNINIT), 
  mesh(NULL),
  nodes_finalized(false),
  elems_finalized(false),
  has_node_orig_coords(false),
  has_node_mask(false),
  has_elem_coords(false),
  has_elem_orig_coords(false), 
  has_elem_mask(false),
  has_elem_area(false),
  has_elem_frac(false),
  is_split(false),
  max_non_split_id(0),
  has_ghost(false) {

    _num_node = 0;
    _num_elem = 0;
    _num_elem_conn = 0;
    _num_orig_node = 0;
    _num_orig_elem = 0;
    _num_orig_elem_conn = 0;
    _num_owned_node = 0;
    _num_owned_elem = 0;
    _num_owned_elem_conn = 0;
} 

// From inputs
// _pdim - parametric dimension
// _orig_sdim - the original spatial dimension (before converting to Cart 3D)
// _coordSys  - the coordinate system of the mesh
MBMesh::MBMesh(int _pdim, int _orig_sdim, ESMC_CoordSys_Flag _coordsys): 
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh()"
  // TODO: Figure out if there's an empty tag to init tags to
  pdim(_pdim),
  sdim(0), 
  orig_sdim(_orig_sdim),
  coordsys(_coordsys), 
  mesh(NULL),
  nodes_finalized(false),
  elems_finalized(false),
  has_node_orig_coords(false),
  has_node_mask(false),
  has_elem_coords(false),
  has_elem_orig_coords(false), 
  has_elem_mask(false),
  has_elem_area(false),
  has_elem_frac(false),
  is_split(false),
  max_non_split_id(0),
  has_ghost(false) {


  // Moab error
  int merr;

  // Init
  _num_node = 0;
  _num_elem = 0;
  _num_elem_conn = 0;
  _num_orig_node = 0;
  _num_orig_elem = 0;
  _num_orig_elem_conn = 0;
  _num_owned_node = 0;
  _num_owned_elem = 0;
  _num_owned_elem_conn = 0;

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
  ESMC_CHECK_MOAB_THROW(merr);
  
  // Setup orig_pos tag
  int_def_val=ORIG_POS_AFTERCREATE; // Default to AFTERCREATE, so that if objects
                                    // are created later after the original mesh creation
                                    // we know. 
  merr=mesh->tag_get_handle("orig_pos", 1, MB_TYPE_INTEGER, orig_pos_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  ESMC_CHECK_MOAB_THROW(merr);
  
  // Setup owner tag
  int_def_val=-1;
  merr=mesh->tag_get_handle("owner", 1, MB_TYPE_INTEGER, owner_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  ESMC_CHECK_MOAB_THROW(merr);

  
  // Setup node_orig_coord tag
  has_node_orig_coords=false;
  if (coordsys != ESMC_COORDSYS_CART) {
    double dbl_def_val[3]={-1.0, -1.0, -1.0};
    merr=mesh->tag_get_handle("node_orig_coords", orig_sdim, MB_TYPE_DOUBLE, node_orig_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
    ESMC_CHECK_MOAB_THROW(merr);
    has_node_orig_coords=true;
  }

  // Setup elem frac
  double dbl_def_val_1=1.0;
  merr=mesh->tag_get_handle("elem_frac", 1, MB_TYPE_DOUBLE, elem_frac_tag, MB_TAG_EXCL|MB_TAG_DENSE, &dbl_def_val_1);
  ESMC_CHECK_MOAB_THROW(merr);
  has_elem_frac=true;

} 

// The coords variable here is in the original representation, not converted to cart. 
EntityHandle MBMesh::add_node(double *orig_coords, int gid, int orig_pos, int owner) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::add_node()"

  // Error return codes
  int localrc, merr;


  // Convert coords (if necessary)
  double cart_coords[3]={0.0,0.0,0.0};

  // Convert to cartesian
  ESMCI_CoordSys_ConvertToCart(coordsys, orig_sdim,
                               orig_coords, cart_coords);

  // Add vertex
  EntityHandle new_vert;
  merr=mesh->create_vertex(cart_coords,new_vert);
  ESMC_CHECK_MOAB_THROW(merr);


  // Set original coords (if necessary) 
  if (has_node_orig_coords) {
    // Set original coords
    merr=mesh->tag_set_data(node_orig_coords_tag, &new_vert, 1, orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  }


  // Set Id
  merr=mesh->tag_set_data(gid_tag, &new_vert, 1, &gid);
  ESMC_CHECK_MOAB_THROW(merr);


  // Set original position
  merr=mesh->tag_set_data(orig_pos_tag, &new_vert, 1, &orig_pos);
  ESMC_CHECK_MOAB_THROW(merr);

  
  // Set Owner
  merr=mesh->tag_set_data(owner_tag, &new_vert, 1, &owner);
  ESMC_CHECK_MOAB_THROW(merr);
  
  // Output new vertex/node
  return new_vert;
}


// Create a bunch of new nodes at once 
// (This seems more memory efficient in MOAB)
// TODO: have the added_nodes range come out the return??
void MBMesh::add_nodes(int num_new_nodes,       // Number of nodes
                       double *orig_coords, // For each node it's orig_coords
                       int *gids,           // For each node it's gid
                       int *orig_pos,       // For each node it's orig_pos, if NULL just do in order
                       int *owners,         // For each node it's owner
                       Range &added_nodes) {        
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::add_nodes()"

  // Error return codes
  int localrc, merr;

  // If there are no nodes, just leave because there's nothing to do
  if (num_new_nodes <= 0) return;

  // Check input
  ThrowRequire(orig_coords != NULL); 
  ThrowRequire(gids != NULL); 
  // orig_pos can be NULL
  ThrowRequire(owners != NULL); 

  // Allocate coordinate space
  double *cart_coords3D=NULL;
  cart_coords3D=new double[3*num_new_nodes];

  // Convert coords to 3D cartesian
  double *node_orig_coords=orig_coords;
  double *node_cart_coords=cart_coords3D;
  for (int n=0; n<num_new_nodes; n++) {

    // Set 3rd dim to 0 in case this is 2D cart
    // and that dimension isn't being set
    node_cart_coords[2]=0.0;

    // Convert to cartesian
    ESMCI_CoordSys_ConvertToCart(coordsys, orig_sdim,
                                 node_orig_coords,
                                 node_cart_coords);

    // Advance to next spot in arrays
    node_orig_coords+=orig_sdim;
    node_cart_coords+=3;
  }


  // Add vertices
  merr=mesh->create_vertices(cart_coords3D, num_new_nodes, added_nodes);
  ESMC_CHECK_MOAB_THROW(merr);


  // Get rid of coordinate memory
  if (cart_coords3D != NULL) delete [] cart_coords3D;


  // Set original coords (if necessary) 
  if (has_node_orig_coords) {
    // Set original coords
    merr=mesh->tag_set_data(node_orig_coords_tag, added_nodes, orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  }


  // Set Ids
  merr=mesh->tag_set_data(gid_tag, added_nodes, gids);
  ESMC_CHECK_MOAB_THROW(merr);


  // Set original positions
  // IF NULL, just order from 1, otherwise use input
  if (orig_pos == NULL) {

    // Set in order starting from 0
    int pos=0;
    for(Range::const_iterator it=added_nodes.begin(); it !=added_nodes.end(); it++) {
      const EntityHandle *node=&(*it);
    
      merr=mesh->tag_set_data(orig_pos_tag, node, 1, &pos);
      ESMC_CHECK_MOAB_THROW(merr);

      // Next pos
      pos++;
    }

  } else { // Use input
    merr=mesh->tag_set_data(orig_pos_tag, added_nodes, orig_pos);
    ESMC_CHECK_MOAB_THROW(merr);
  }
  
  // Set Owners
  merr=mesh->tag_set_data(owner_tag, added_nodes, owners);
  ESMC_CHECK_MOAB_THROW(merr);  
}


void MBMesh::setup_node_mask() {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::setup_node_mask()"

  int localrc, merr;

  // Setup node mask tag
  int int_def_val=0; // So things are by default not masked
  merr=mesh->tag_get_handle("node_mask", 1, MB_TYPE_INTEGER, node_mask_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  ESMC_CHECK_MOAB_THROW(merr);

  
  // Setup node mask value tag
  int_def_val=0; // So things are by default not masked
  merr=mesh->tag_get_handle("node_mask_val", 1, MB_TYPE_INTEGER, node_mask_val_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  ESMC_CHECK_MOAB_THROW(merr);

  // Turn on masking
  has_node_mask=true;
}


// Get a Range of all nodes on this processor
void MBMesh::get_all_nodes(Range &all_nodes) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_all_nodes()"
  try {
    int merr=mesh->get_entities_by_dimension(0, 0, all_nodes);
    ESMC_CHECK_MOAB_THROW(merr);
  }
  CATCH_MBMESH_RETHROW
}

// Get a Range of all elems on this processor
void MBMesh::get_all_elems(Range &all_elems) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_all_elems()"
  try {
    int merr=mesh->get_entities_by_dimension(0, pdim, all_elems);
    ESMC_CHECK_MOAB_THROW(merr);
  }
  CATCH_MBMESH_RETHROW
}

// Get a vector of owned nodes on this processor
void MBMesh::get_all_nodes(std::vector<EntityHandle> &all_nodes) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_all_nodes()"
  try {
    int merr=mesh->get_entities_by_dimension(0, 0, all_nodes);
    ESMC_CHECK_MOAB_THROW(merr);
  }
  CATCH_MBMESH_RETHROW
}

// Get a vector of owned elements on this processor
void MBMesh::get_all_elems(std::vector<EntityHandle> &all_elems) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_all_elems()"
  try {
    int merr=mesh->get_entities_by_dimension(0, pdim, all_elems);
    ESMC_CHECK_MOAB_THROW(merr);
  }
  CATCH_MBMESH_RETHROW
}

// Get a vector of owned nodes on this processor
void MBMesh::get_owned_nodes(std::vector<EntityHandle> &owned_nodes) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_owned_nodes()"
  try {
    int localrc;
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    std::vector<EntityHandle> all_nodes;
    get_all_nodes(all_nodes);

    std::vector<EntityHandle>::const_iterator ni = all_nodes.begin();
    std::vector<EntityHandle>::const_iterator ne = all_nodes.end();

    for (ni; ni != ne; ++ni) {
      int owner = get_owner(*ni);
      if (owner == localPet) owned_nodes.push_back(*ni);
    }
  }
  CATCH_MBMESH_RETHROW
}

// Get a vector of owned elements on this processor
void MBMesh::get_owned_elems(std::vector<EntityHandle> &owned_elems) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_owned_elems()"
  try {
    int localrc;
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    std::vector<EntityHandle> all_elems;
    get_all_elems(all_elems);
    std::vector<EntityHandle>::const_iterator ei = all_elems.begin();
    std::vector<EntityHandle>::const_iterator ee = all_elems.end();

    for (ei; ei != ee; ++ei) {
      int owner = get_owner(*ei);
      if (owner == localPet) owned_elems.push_back(*ei);
    }
  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_sorted_orig_nodes(std::vector<EntityHandle> &orig_nodes) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_sorted_orig_nodes()"
  try {
    int localrc;
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    int merr;
  
    // compute original nodes
    Range all_nodes;
    get_all_nodes(all_nodes);
  
    // Setup list to hold pairs
    std::vector<std::pair<int,EntityHandle> > pos_and_nodes;
    pos_and_nodes.reserve(all_nodes.size());
  
    // Loop through nodes putting into list
    for(Range::const_iterator it=all_nodes.begin(); it !=all_nodes.end();   it++) {
      EntityHandle node=*it;
  
      // Get orig_pos
      int orig_pos;
      merr=mesh->tag_get_data(orig_pos_tag, &node, 1, &orig_pos);
      ESMC_CHECK_MOAB_THROW(merr);
  
      // Skip ones with orig_pos<0 (they are not from original creation)
      if (orig_pos < ORIG_POS_MIN_OUTPUT) continue;
  
      // Stick in list
      pos_and_nodes.push_back(std::make_pair(orig_pos,node));
    }
  
    // Put in order by original pos
    std::sort(pos_and_nodes.begin(), pos_and_nodes.end());
  
    // Setup orig_nodes list
    orig_nodes.clear();
    orig_nodes.reserve(pos_and_nodes.size());
  
    // Fill array of node entities
    for (int i = 0; i<pos_and_nodes.size(); ++i) {
      orig_nodes.push_back(pos_and_nodes[i].second);
    }
  
  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_sorted_orig_elems(std::vector<EntityHandle> &orig_elems) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_sorted_orig_elems()"
  try {
    int localrc;
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    int merr;

    // compute original nodes
    Range all_elems;
    get_all_elems(all_elems);

    // Setup list to hold pairs
    std::vector<std::pair<int,EntityHandle> > pos_and_elems;
    pos_and_elems.reserve(all_elems.size());

    // Loop through elems putting into list
    for(Range::const_iterator it=all_elems.begin(); it !=all_elems.end(); it++) {
      EntityHandle elem=*it;

      // Get orig_pos
      int orig_pos;
      merr=mesh->tag_get_data(orig_pos_tag, &elem, 1, &orig_pos);
      ESMC_CHECK_MOAB_THROW(merr);

      // Skip ones with orig_pos<0 (they are not from original creation)
      if (orig_pos < ORIG_POS_MIN_OUTPUT) continue;

      // Stick in list
      pos_and_elems.push_back(std::make_pair(orig_pos,elem));
    }

    // Put in order by original pos
    std::sort(pos_and_elems.begin(), pos_and_elems.end());

    // Setup orig_elems list
    orig_elems.clear();
    orig_elems.reserve(pos_and_elems.size());

    // Fill array of elem entities
    for (int i = 0; i<pos_and_elems.size(); ++i) {
      orig_elems.push_back(pos_and_elems[i].second);
    }

  }
  CATCH_MBMESH_RETHROW
}

int MBMesh::get_num_elem_conn(std::vector<EntityHandle> elems){
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::num_elem_conn()"
  int elemConnCount=0;
  try {
    int merr;
    // Doesn't work with split meshes right now
    if (this->is_split)
      Throw () << "Can't get elem connection count from mesh containing >4 elements.";

    std::vector<EntityHandle>::const_iterator ei = elems.begin();
    std::vector<EntityHandle>::const_iterator ee = elems.end();

    // Loop summing number of nodes per element
    for (ei; ei != ee; ++ei) {
      EntityHandle elem=*ei;

      // Get topology of element
      std::vector<EntityHandle> nodes_on_elem;
      merr=mesh->get_connectivity(&elem, 1, nodes_on_elem);
      ESMC_CHECK_MOAB_THROW(merr)

      // Add number of nodes for this elem to connection count
      elemConnCount += nodes_on_elem.size();
    }
  }
  CATCH_MBMESH_RETHROW
  
  return elemConnCount;
}
void MBMesh::get_elem_areas(double *elem_area) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_areas()"
  try {
    int merr;
  
    if (!has_elem_area) Throw () << "Element areas not present.";

    Range elems;
    merr=mesh->get_entities_by_dimension(0, pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr)
  
    merr=mesh->tag_get_data(elem_area_tag, elems, elem_area);
    ESMC_CHECK_MOAB_THROW(merr)

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_elem_coords(double *elem_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_coords()"
  try {
    int merr;
  
    Range elems;
    merr=mesh->get_entities_by_dimension(0, pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr)

    if (coordsys != ESMC_COORDSYS_CART) {
      if (!has_elem_orig_coords)
        Throw () << "Element original coords not present.";
  
      merr=mesh->tag_get_data(elem_orig_coords_tag, elems, elem_coords);
      ESMC_CHECK_MOAB_THROW(merr)
    } else {
      if (!has_elem_coords)
        Throw () << "Element coords not present.";

      merr=mesh->tag_get_data(elem_coords_tag, elems, elem_coords);
      ESMC_CHECK_MOAB_THROW(merr)
    }

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_elem_ids(int *elem_ids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_ids()"
  try {
    int merr;

    Range elems;
    merr=mesh->get_entities_by_dimension(0, pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr)

    merr=mesh->tag_get_data(gid_tag, elems, elem_ids);
    ESMC_CHECK_MOAB_THROW(merr)

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_elem_mask(int *elem_mask) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_mask()"
  try {
    int merr;

    if (!has_elem_mask) Throw () << "Element mask not present.";

    Range elems;
    merr=mesh->get_entities_by_dimension(0, pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr)

    merr=mesh->tag_get_data(elem_mask_val_tag, elems, elem_mask);
    ESMC_CHECK_MOAB_THROW(merr)

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_elem_types(int *elem_types) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_types()"
  try {
    int merr;

    std::vector<EntityHandle> elems;
    get_owned_elems(elems);
    ESMC_CHECK_MOAB_THROW(merr)

    std::vector<EntityHandle>::const_iterator it = elems.begin();
    int i = 0;
    for (it; it != elems.end(); it++) {
      EntityHandle elem=*it;
  
      // Get topology of element
      int type = mesh->type_from_handle(elem);
      ESMC_CHECK_MOAB_THROW(merr)

      elem_types[i] = M2EType(type);

      // // NOTE: old style
      // Range nodes_on_elem;
      // merr=mesh->get_connectivity(&elem, 1, nodes_on_elem);
      // if (pdim == 2) {
      //   if (nodes_on_elem.size() == 3) elem_types[i] = ESMC_MESHELEMTYPE_TRI;
      //   else if (nodes_on_elem.size() == 4) elem_types[i] = ESMC_MESHELEMTYPE_QUAD;
      //   else
      //     Throw () << "Element type not recognized.";
      // } else if (pdim == 3) {
      //   if (nodes_on_elem.size() == 4) elem_types[i] = ESMC_MESHELEMTYPE_TETRA;
      //   else if (nodes_on_elem.size() == 8) elem_types[i] = ESMC_MESHELEMTYPE_HEX;       
      //   else
      //     Throw () << "Element type not recognized.";
      // } else
      //   Throw () << "Parameteric dimension not recognized.";
      i++;
    }

  }
  CATCH_MBMESH_RETHROW
}


void MBMesh::get_elem_connectivity(int *elem_conn) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_connectivity()"
  try {
    int localrc, merr;

    // get node ids in a vector, which we will need to search later
    Range nodes;
    merr=mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr)
    int *node_ids = new int[nodes.size()];
    get_node_ids(node_ids);
    std::vector<int> nodeids(node_ids, node_ids + nodes.size());
    delete [] node_ids;

    // now iterate through the elements
    Range elems;
    merr=mesh->get_entities_by_dimension(0, pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr)
    
    int elemConnCountTemp = 0;
    for (Range::const_iterator it=elems.begin(); it != elems.end(); it++) {
      EntityHandle elem=*it;

      // Get topology of element (ordered)
      vector<EntityHandle> nodes_on_elem;
      merr=mesh->get_connectivity(&elem, 1, nodes_on_elem);
      ESMC_CHECK_MOAB_THROW(merr)

      int nid;
      // add connectivity to output array
      for (int i=0; i<nodes_on_elem.size(); ++i) {
        // get the node id
        merr=mesh->tag_get_data(gid_tag, &nodes_on_elem.at(i), 1, &nid);
        ESMC_CHECK_MOAB_THROW(merr)

        std::vector<int>::iterator itr = std::find(nodeids.begin(), nodeids.end(), nid);
        // add 1 for Fortran indexing
        elem_conn[elemConnCountTemp+i] = std::distance(nodeids.begin(), itr) +1;
      }

      // Add number of nodes for this elem to connection count
      elemConnCountTemp += nodes_on_elem.size();
    }

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_elem_types(std::vector<EntityHandle> const &elems, int *elem_types) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_types()"
  try {
    int merr;
  
    int i = 0;
    for (std::vector<EntityHandle>::const_iterator it=elems.begin(); it != elems.end(); it++) {
      EntityHandle elem=*it;
  
      // Get topology of element
      Range nodes_on_elem;
      merr=mesh->get_connectivity(&elem, 1, nodes_on_elem);
      ESMC_CHECK_MOAB_THROW(merr)

      elem_types[i] = 0;
      if (pdim == 2) {
        if (nodes_on_elem.size() == 3) elem_types[i] = ESMC_MESHELEMTYPE_TRI;
        else if (nodes_on_elem.size() == 4) elem_types[i] = ESMC_MESHELEMTYPE_QUAD;
        else
          Throw () << "Element type not recognized.";
      } else if (pdim == 3) {
        if (nodes_on_elem.size() == 4) elem_types[i] = ESMC_MESHELEMTYPE_TETRA;
        else if (nodes_on_elem.size() == 8) elem_types[i] = ESMC_MESHELEMTYPE_HEX;       
        else
          Throw () << "Element type not recognized.";
      } else
        Throw () << "Parameteric dimension not recognized.";
      i++;
    }

  }
  CATCH_MBMESH_RETHROW
}


void MBMesh::get_elem_connectivity(std::vector<EntityHandle> const &elems, int *elem_conn) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_connectivity()"
  try {
    int localrc, merr;

    // get node ids in a vector, which we will need to search later
    // Range nodes;
    // merr=mesh->get_entities_by_dimension(0, 0, nodes);
    // ESMC_CHECK_MOAB_THROW(merr)
    // int *node_ids = new int[nodes.size()];
    // get_node_ids(node_ids);
    // std::vector<int> nodeids(node_ids, node_ids + nodes.size());
    // delete [] node_ids;
    std::vector<int> nodeids(num_node(), -1);
    std::vector<EntityHandle> orig_nodes;
    get_sorted_orig_nodes(orig_nodes);
    get_gid(orig_nodes, nodeids.data());

    // now iterate through the elements
    int elemConnCountTemp = 0;
    for (std::vector<EntityHandle>::const_iterator it=elems.begin(); it != elems.end(); it++) {
      EntityHandle elem=*it;

      // Get topology of element (ordered)
      vector<EntityHandle> nodes_on_elem;
      merr=mesh->get_connectivity(&elem, 1, nodes_on_elem);
      ESMC_CHECK_MOAB_THROW(merr)

      int nid;
      // add connectivity to output array
      for (int i=0; i<nodes_on_elem.size(); ++i) {
        // get the node id
        merr=mesh->tag_get_data(gid_tag, &nodes_on_elem.at(i), 1, &nid);
        ESMC_CHECK_MOAB_THROW(merr)

        std::vector<int>::iterator itr = std::find(nodeids.begin(), nodeids.end(), nid);
        // add 1 for Fortran indexing
        elem_conn[elemConnCountTemp+i] = std::distance(nodeids.begin(), itr) +1;
      }

      // Add number of nodes for this elem to connection count
      elemConnCountTemp += nodes_on_elem.size();
    }

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_elem_centroids(double *elem_centroids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_centroids()"
  try {
    int merr;
  
    Throw () << "Elem centroids are not yet available.";
  
    // mbutil = Util::Util();
    
    // auto i = 0;
    // for (Range::const_iterator it=elems.begin(); it != elems.end(); it++) {
    //   EntityHandle elem=*it;
    // 
    //   double coord[3];
    //   mesh->centroid(this, elem, coord);
    //   ESMC_CHECK_MOAB_THROW(merr)
    // 
    //   elem_centroids[i*sdim] = coord[0];
    //   elem_centroids[i*sdim+1] = coord[1];
    //   if (sdim == 3)
    //     elem_centroids[i*sdim+2] = coord[2];
    // }

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_node_coords(double *node_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_coords()"
  try {
    int merr;
    
    Range nodes;
    merr=mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr)

    if (coordsys != ESMC_COORDSYS_CART) {
      if (!has_node_orig_coords)
        Throw () << "Node original coords not present.";
  
      merr=mesh->tag_get_data(node_orig_coords_tag, nodes, node_coords);
      ESMC_CHECK_MOAB_THROW(merr)
    } else {
      double *node_coords3d = new double[3*nodes.size()];
      // double *node_coords3d = new double;
      merr=mesh->get_coords(nodes, node_coords3d);
      ESMC_CHECK_MOAB_THROW(merr)
      
      // compact 3d coords to 2d
      for (int i=0; i<nodes.size(); ++i) {
        node_coords[i*pdim] = node_coords3d[i*3];
        node_coords[i*pdim+1] = node_coords3d[i*3+1];
        if (pdim == 3)
          node_coords[i*pdim+2] = node_coords3d[i*3+2];
      }
      
      delete [] node_coords3d;
    }

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_node_ids(int *node_ids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_ids()"
  try {
    int merr;
    
    Range nodes;
    merr=mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr)

    merr=mesh->tag_get_data(gid_tag, nodes, node_ids);
    ESMC_CHECK_MOAB_THROW(merr)

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_node_mask(int *node_mask) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_mask()"
  try {
    int merr;

    if (!has_node_mask) Throw () << "Node mask not present.";

    Range nodes;
    merr=mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr)

    merr=mesh->tag_get_data(node_mask_val_tag, nodes, node_mask);
    ESMC_CHECK_MOAB_THROW(merr)
  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::get_node_owners(int *node_owners) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_owners()"
  try {
    int merr;

    Range nodes;
    merr=mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr)

    merr=mesh->tag_get_data(owner_tag, nodes, node_owners);
    ESMC_CHECK_MOAB_THROW(merr)

  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::set_node_mask_val(EntityHandle eh, int mask_val) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_node_mask_val()"

  // Error return codes
  int localrc;
  int merr;

  // If no masking, complain
  if (!has_node_mask) Throw() << "node mask value not present in mesh.";
  
  // Set data
  merr=mesh->tag_set_data(node_mask_val_tag, &eh, 1, &mask_val);
  ESMC_CHECK_MOAB_THROW(merr);
}

void MBMesh::set_node_mask_val(Range nodes, int *mask_vals) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_node_mask_val()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_node_mask) Throw() << "node mask value not present in mesh.";

  // Set data in MOAB
  merr=mesh->tag_set_data(node_mask_val_tag, nodes, mask_vals);
  ESMC_CHECK_MOAB_THROW(merr);

}

void MBMesh::set_node_mask_val(std::vector<EntityHandle> const &nodes, int *mask_vals) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_node_mask_val()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_node_mask) Throw() << "node mask value not present in mesh.";

  // Set data in MOAB
  if (nodes.size() > 0) {
    merr=mesh->tag_set_data(node_mask_val_tag, &nodes[0], nodes.size(), mask_vals);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}

void MBMesh::set_node_mask(EntityHandle eh, int mask) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_node_mask()"

  // Error return codes
  int localrc;
  int merr;

  // If no masking, complain
  if (!has_node_mask) Throw() << "node mask not present in mesh.";
  
  // Set data
  merr=mesh->tag_set_data(node_mask_tag, &eh, 1, &mask);
  ESMC_CHECK_MOAB_THROW(merr);
}

void MBMesh::set_node_mask(Range nodes, int *masks) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_node_mask()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_node_mask) Throw() << "node mask not present in mesh.";

  // Set data in MOAB
  merr=mesh->tag_set_data(node_mask_tag, nodes, masks);
  ESMC_CHECK_MOAB_THROW(merr);

}

void MBMesh::set_node_mask(std::vector<EntityHandle> const &nodes, int *masks) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_node_mask()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_node_mask) Throw() << "node mask not present in mesh.";

  // Set data in MOAB
  if (nodes.size() > 0) {
    merr=mesh->tag_set_data(node_mask_tag, &nodes[0], nodes.size(), masks);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}




int MBMesh::get_node_mask(EntityHandle node) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_mask()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, then error
  if (!has_node_mask) Throw() << "node mask not present in mesh.";

  // Get mask
  int mask;
  merr=mesh->tag_get_data(node_mask_tag, &node, 1, &mask);
  ESMC_CHECK_MOAB_THROW(merr);

  // Output information
  return mask;
}

int MBMesh::get_node_mask_val(EntityHandle node) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_mask_val()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, then error
  if (!has_node_mask) Throw() << "node mask value not present in mesh.";

  // Get mask val
  int mask_val;
  merr=mesh->tag_get_data(node_mask_val_tag, &node, 1, &mask_val);
  ESMC_CHECK_MOAB_THROW(merr);

  // Output information
  return mask_val;
}

void MBMesh::get_node_mask_val(std::vector<EntityHandle> const &nodes, int *mask_vals) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_mask_val()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_node_mask) Throw() << "node mask value not present in mesh.";

  // Set data in MOAB
  if (nodes.size() > 0) {
    merr=mesh->tag_get_data(node_mask_val_tag, &nodes[0], nodes.size(), mask_vals);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}



void MBMesh::set_node_coords(EntityHandle eh, double *orig_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_node_coords()"

  // Error return codes
  int localrc;
  int merr;
  
  // Set original coords if present
  if (has_node_orig_coords) {
    merr=mesh->tag_set_data(node_orig_coords_tag, &eh, 1, orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  }

  // Convert and set vertex coords
  double cart_coords[3]={0.0,0.0,0.0};

  // Convert original coords to cartesian
  ESMCI_CoordSys_ConvertToCart(coordsys, orig_sdim,
                               orig_coords, cart_coords);

  // Set Vertex
  merr=mesh->set_coords(&eh,1,cart_coords);
  ESMC_CHECK_MOAB_THROW(merr);
}

// Get the internal Cartesian coords for the node
// coords needs to be of size sdim 
void MBMesh::get_node_cart_coords(EntityHandle node, double *coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_cart_coords()"

  // Error return codes
  int merr;

  // Get the coords
  double tmp_coords[3]={0.0,0.0,0.0};
  merr=mesh->get_coords(&node,1,tmp_coords);
  ESMC_CHECK_MOAB_THROW(merr);

  // Copy
  coords[0]=tmp_coords[0];
  coords[1]=tmp_coords[1];
  if (sdim > 2) coords[2]=tmp_coords[2];
}


// Get the original coords for the node, if there are no orig coords then 
// just uses coords (because those would be the original ones). 
// orig_coords needs to be of size orig_sdim
void MBMesh::get_node_orig_coords(EntityHandle node, double *orig_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_orig_coords()"

  // Error return codes
  int merr;

  // If has orig coords then get those
  if (has_node_orig_coords) {
    merr=mesh->tag_get_data(node_orig_coords_tag, &node, 1, orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  } else {
    // Get coords from MOAB
    double coords[3]={0.0,0.0,0.0};
    merr=mesh->get_coords(&node,1,coords);
    ESMC_CHECK_MOAB_THROW(merr);

    // Copy into buffer of size orig_sdim
    orig_coords[0]=coords[0];
    orig_coords[1]=coords[1];
    if (orig_sdim > 2) orig_coords[2]=coords[2];
  }
}

// Get the original coords for the node, if there are no orig coords then 
// just uses coords (because those would be the original ones). 
// orig_coords needs to be of size orig_sdim*nodes.size()
void MBMesh::get_node_orig_coords(std::vector<EntityHandle> const &nodes, double *orig_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_node_orig_coords()"

  // Error return codes
  int merr;

  // If nothing in vector then leave
  if (nodes.size() <= 0) return;

  // If has orig coords then get those
  if (has_node_orig_coords) {
    merr=mesh->tag_get_data(node_orig_coords_tag, &nodes[0], nodes.size(), orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  } else {
    int pos=0;
    for (int i=0; i<nodes.size(); i++) {

      // Get coords from MOAB
      double coords[3]={0.0,0.0,0.0};
      merr=mesh->get_coords(&nodes[i],1,coords);
      ESMC_CHECK_MOAB_THROW(merr);
      
      // Copy into buffer of size orig_sdim
      orig_coords[pos]=coords[0];
      orig_coords[pos+1]=coords[1];
      if (orig_sdim > 2) orig_coords[pos+2]=coords[2];
      
      // advance pos
      pos += orig_sdim;
    }
  }
}



// The coords variable here is in the original representation, not converted to cart. 
EntityHandle MBMesh::add_elem(EntityType elem_type, int num_nodes, EntityHandle *nodes, 
                              int gid, int orig_pos, int owner) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::add_elem()"

  // Error return codes
  int localrc;
  int merr;

  // Add element
  EntityHandle new_elem;
  merr=mesh->create_element(elem_type, nodes, num_nodes, new_elem);
  ESMC_CHECK_MOAB_THROW(merr);


  // Set Id
  merr=mesh->tag_set_data(gid_tag, &new_elem, 1, &gid);
  ESMC_CHECK_MOAB_THROW(merr);

  // Set original position
  merr=mesh->tag_set_data(orig_pos_tag, &new_elem, 1, &orig_pos);
  ESMC_CHECK_MOAB_THROW(merr);

  // Set Owner
  merr=mesh->tag_set_data(owner_tag, &new_elem, 1, &owner);
  ESMC_CHECK_MOAB_THROW(merr);

  // Output new vertex/node
  return new_elem;
}

// Add a set of elements of the same type
// returns range of newly added elems
void MBMesh::add_elems(int num_new_elems,  // The number of elems to add
                       EntityType elem_type, int nodes_per_elem, // The type and number of nodes in each elem
                       EntityHandle *nodes, // List of nodes that make up each elem (of size num_new_elems*nodes_per_elem)
                       int *gids,      // global ids for each elem (of size num_new_elems)
                       int *orig_pos,  // original position for each elem (of size num_new_elems), if NULL just do in order
                       int *owners,     // owner for each elem (of size num_new_elems)
                       Range &added_elems) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::add_elems()"

  // Error return codes
  int localrc;
  int merr;

  // If there are no new elems, just leave because there's nothing to do
  if (num_new_elems <= 0) return;

  // Check input
  ThrowRequire(nodes != NULL); 
  ThrowRequire(gids != NULL); 
  // orig_pos can be NULL
  ThrowRequire(owners != NULL); 


  // Get reader interface
  ReadUtilIface* iface;
  merr = mesh->query_interface(iface);
  ESMC_CHECK_MOAB_THROW(merr);


  // Add elements
  EntityHandle start_new_elems_eh;
  EntityHandle* conn;
  merr=iface->get_element_connect(num_new_elems,
                             nodes_per_elem,
                             elem_type,
                             0,
                             start_new_elems_eh,
                             conn);

  // Fill connections with the appropriate nodes
  int num_new_nodes=num_new_elems*nodes_per_elem;
  for (int n=0; n<num_new_nodes; n++) {
    conn[n]=nodes[n];
  }

  // Do update conn
  merr = iface->update_adjacencies(start_new_elems_eh, num_new_elems, nodes_per_elem, conn);
  ESMC_CHECK_MOAB_THROW(merr);


  // Create range of newly added elems
  added_elems.clear();
  added_elems.insert(start_new_elems_eh,start_new_elems_eh+num_new_elems-1);

  // Set Ids
  merr=mesh->tag_set_data(gid_tag, added_elems, gids);
  ESMC_CHECK_MOAB_THROW(merr);

  // Set original positions
  // IF NULL, just order from 1, otherwise use input
  if (orig_pos == NULL) {

    // Set in order starting from 0
    int pos=0;
    for(Range::const_iterator it=added_elems.begin(); it !=added_elems.end(); it++) {
      const EntityHandle elem=(*it);
    
      merr=mesh->tag_set_data(orig_pos_tag, &elem, 1, &pos);
      ESMC_CHECK_MOAB_THROW(merr);

      // Next pos
      pos++;
    }

  } else { // Use input
    merr=mesh->tag_set_data(orig_pos_tag, added_elems, orig_pos);
    ESMC_CHECK_MOAB_THROW(merr);
  }
  
  // Set Owners
  merr=mesh->tag_set_data(owner_tag, added_elems, owners);
  ESMC_CHECK_MOAB_THROW(merr);
}



// Get the internal Cartesian coords for the elem
// coords needs to be of size sdim 
void MBMesh::get_elem_cart_coords(EntityHandle elem, double *coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_cart_coords()"

  // If no coords, complain
  if (!has_elem_coords) Throw() << "element coords not present in mesh.";

  // Error return codes
  int merr=mesh->tag_get_data(elem_coords_tag, &elem, 1, coords);
  ESMC_CHECK_MOAB_THROW(merr);
}


// Get the original coords for the elem, if there are no orig coords then 
// just uses coords (because those would be the original ones). 
// orig_coords needs to be of size orig_sdim
void MBMesh::get_elem_orig_coords(EntityHandle elem, double *orig_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_orig_coords()"

  // Error return codes
  int merr;

  // If no coords, complain
  if (!has_elem_coords) Throw() << "element coords not present in mesh.";

  // If has orig coords then get those
  if (has_elem_orig_coords) {
    merr=mesh->tag_get_data(elem_orig_coords_tag, &elem, 1, orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  } else {
    merr=mesh->tag_get_data(elem_coords_tag, &elem, 1, orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  } 
}


// Get the original coords for a vector of elems, if there are no orig coords then 
// just uses coords (because those would be the original ones). 
// orig_coords needs to be of size orig_sdim*nodes.size()
void MBMesh::get_elem_orig_coords(std::vector<EntityHandle> const &elems, double *orig_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_orig_coords()"

  // Error return codes
  int merr;

  // If nothing in list, just leave
  if (elems.size() <= 0) return;

  // If no coords, complain
  if (!has_elem_coords) Throw() << "element coords not present in mesh.";

  // If has orig coords then get those
  if (has_elem_orig_coords) {
    merr=mesh->tag_get_data(elem_orig_coords_tag, &elems[0], elems.size(), orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  } else {
    merr=mesh->tag_get_data(elem_coords_tag, &elems[0], elems.size(), orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  } 
}


// Get an element's corner nodes
// It looks like MOAB just returns a pointer into the actual connectivity storage, so 
// corner_nodes shouldn't be deallocated. 
void MBMesh::get_elem_corner_nodes(EntityHandle elem, int &num_corner_nodes, const EntityHandle *&corner_nodes) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_corner_nodes()"

  // Error return code
  int merr;
  
  // Get nodes in element
  merr=mesh->get_connectivity(elem,corner_nodes,num_corner_nodes,true);
  ESMC_CHECK_MOAB_THROW(merr);
}

void MBMesh::setup_elem_mask() {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::setup_elem_mask()"

  int localrc, merr;

  // Setup elem mask tag
  int int_def_val=0; // So things are by default not masked
  merr=mesh->tag_get_handle("elem_mask", 1, MB_TYPE_INTEGER, elem_mask_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  ESMC_CHECK_MOAB_THROW(merr);

  
  // Setup elem mask value tag
  int_def_val=0; // So things are by default not masked
  merr=mesh->tag_get_handle("elem_mask_val", 1, MB_TYPE_INTEGER, elem_mask_val_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  ESMC_CHECK_MOAB_THROW(merr);


  // Turn on masking
  has_elem_mask=true;
}


void MBMesh::set_elem_mask(EntityHandle eh, int mask) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_mask()"

  // Error return codes
  int localrc;
  int merr;

  // If no masking, complain
  if (!has_elem_mask) Throw() << "elem mask not present in mesh.";
  
  // Set data
  merr=mesh->tag_set_data(elem_mask_tag, &eh, 1, &mask);
  ESMC_CHECK_MOAB_THROW(merr);
}

void MBMesh::set_elem_mask(Range elems, int *masks) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_mask()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_elem_mask) Throw() << "element mask not present in mesh.";

  // Set data in MOAB
  merr=mesh->tag_set_data(elem_mask_tag, elems, masks);
  ESMC_CHECK_MOAB_THROW(merr);

}



void MBMesh::set_elem_mask(std::vector<EntityHandle> const &elems, int *masks) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_mask()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_elem_mask) Throw() << "elem mask not present in mesh.";

  // Set data in MOAB
  if (elems.size() > 0) {
    merr=mesh->tag_set_data(elem_mask_tag, &elems[0], elems.size(), masks);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}



int MBMesh::get_elem_mask(EntityHandle eh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_mask()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, then error
  if (!has_elem_mask) Throw() << "element mask not present in mesh.";

  // Get mask
  int mask;
  merr=mesh->tag_get_data(elem_mask_tag, &eh, 1, &mask);
  ESMC_CHECK_MOAB_THROW(merr);

  // Output information
  return mask;
}


void MBMesh::set_elem_mask_val(EntityHandle eh, int mask_val) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_mask_val()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_elem_mask) Throw() << "element mask value not present in mesh.";

  // Set data in MOAB
  merr=mesh->tag_set_data(elem_mask_val_tag, &eh, 1, &mask_val);
  ESMC_CHECK_MOAB_THROW(merr);
}



void MBMesh::set_elem_mask_val(Range elems, int *mask_vals) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_mask_val()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_elem_mask) Throw() << "element mask value not present in mesh.";

  // Set data in MOAB
  merr=mesh->tag_set_data(elem_mask_val_tag, elems, mask_vals);
  ESMC_CHECK_MOAB_THROW(merr);

}



void MBMesh::set_elem_mask_val(std::vector<EntityHandle> const &elems, int *mask_vals) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_mask_val()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_elem_mask) Throw() << "elem mask value not present in mesh.";

  // Set data in MOAB
  if (elems.size() > 0) {
    merr=mesh->tag_set_data(elem_mask_val_tag, &elems[0], elems.size(), mask_vals);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}



void MBMesh::get_elem_mask_val(std::vector<EntityHandle> const &elems, int *mask_vals) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_mask_val()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, complain
  if (!has_elem_mask) Throw() << "elem mask value not present in mesh.";

  // Set data in MOAB
  if (elems.size() > 0) {
    merr=mesh->tag_get_data(elem_mask_val_tag, &elems[0], elems.size(), mask_vals);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}



int MBMesh::get_elem_mask_val(EntityHandle eh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_mask_val()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no masking, then error
  if (!has_elem_mask) Throw() << "element mask value not present in mesh.";

  // Get mask vale
  int mask_val;
  merr=mesh->tag_get_data(elem_mask_val_tag, &eh, 1, &mask_val);
  ESMC_CHECK_MOAB_THROW(merr);

  // Output information
  return mask_val;
}


void MBMesh::setup_elem_area() {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::setup_elem_area()"

  int localrc, merr;

  // Setup elem area tag
  double dbl_def_val=0.0; 
  merr=mesh->tag_get_handle("elem_area", 1, MB_TYPE_DOUBLE, elem_area_tag, MB_TAG_EXCL|MB_TAG_DENSE, &dbl_def_val);
  ESMC_CHECK_MOAB_THROW(merr);
  
  // Turn on elem areas
  has_elem_area=true;
}

void MBMesh::set_elem_area(EntityHandle eh, double area) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_area()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no area, complain
  if (!has_elem_area) Throw() << "element area not present in mesh.";

  // Get Owner
  merr=mesh->tag_set_data(elem_area_tag, &eh, 1, &area);
  ESMC_CHECK_MOAB_THROW(merr);

}

void MBMesh::set_elem_area(Range elems, double *area) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_area()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no area, complain
  if (!has_elem_area) Throw() << "element area not present in mesh.";

  // Get Owner
  merr=mesh->tag_set_data(elem_area_tag, elems, area);
  ESMC_CHECK_MOAB_THROW(merr);
}


void MBMesh::set_elem_area(std::vector<EntityHandle> const &elems, double *areas) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_area()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no area, complain
  if (!has_elem_area) Throw() << "elem areas not present in mesh.";

  // Set data in MOAB
  if (elems.size() > 0) {
    merr=mesh->tag_set_data(elem_area_tag, &elems[0], elems.size(), areas);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}


// Add merge_split flag
void MBMesh::get_elem_area(std::vector<EntityHandle> const &elems, double *areas) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_area()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no area, complain
  if (!has_elem_area) Throw() << "elem areas not present in mesh.";

  // Set data in MOAB
  if (elems.size() > 0) {
    merr=mesh->tag_get_data(elem_area_tag, &elems[0], elems.size(), areas);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}

// Add merge_split flag??
double MBMesh::get_elem_area(EntityHandle eh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_area()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no area, then error
  if (!has_elem_area) Throw() << "element area not present in mesh.";

  // Get Owner
  double area;
  merr=mesh->tag_get_data(elem_area_tag, &eh, 1, &area);
  ESMC_CHECK_MOAB_THROW(merr);


  // Output area
  return area;
}

// Get element fraction
// NOTE: This method does not merge split elems, it just gets the data out of MOAB
// TODO: Add merge_split flag??
double MBMesh::get_elem_frac(EntityHandle eh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_frac()"
  
  // If no frac, then error
  if (!has_elem_frac) Throw() << "element frac not present in mesh.";

  // Get data from MOAB
  double frac;
  int merr=mesh->tag_get_data(elem_frac_tag, &eh, 1, &frac);
  ESMC_CHECK_MOAB_THROW(merr);

  // Output data
  return frac;
}


// Get elem fractions. 
// merge_split - if true merge together values for split elems, otherwise just copy out data.
// elems - vector of elems for which to get fraction data
// fracs - place to put fraction data. Needs to be at least of size = sizeof(double)*elems.size()
// WARNING: if merging, this won't handle repeated elements in the elems list
void MBMesh::get_elem_frac(bool merge_split, std::vector<EntityHandle> const &elems, double *fracs) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_elem_frac()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no fracs, complain
  if (!has_elem_frac) Throw() << "element fracs not present in mesh.";


  // If requested and if they exist, merge split elems 
  if (merge_split && is_split) {

      // Make map of elem ids to index
      std::map<int,int> id_to_index;
      for (auto i=0; i<elems.size(); i++) {
        // Get elem gid
        int elem_gid=get_gid(elems[i]);

        // Insert into map
        std::pair<std::map<int,int>::iterator,bool> ret;
        ret=id_to_index.insert(std::pair<int,int>(elem_gid,i));

        // Complain if it's already there
        if (!ret.second) {
          Throw() <<"multiple entries with the same global id in the input list currently not supported when merging fractions.";
        } 
      }

      // Zero out array
      for (auto i=0; i<elems.size(); i++) {
        fracs[i]=0.0;
      }


      // Iterate through split elements adding in frac
      Range all_elems;
      get_all_elems(all_elems);
      for(Range::const_iterator it=all_elems.begin(); it !=all_elems.end(); it++) {
        EntityHandle elem=*it;

        // Get the element id
        int elem_gid=get_gid(elem);

        // Translate id if necessary
        int orig_gid;
        std::map<int,int>::iterator soi =  split_to_orig_id.find(elem_gid);
        if (soi == split_to_orig_id.end()) {
          orig_gid=elem_gid;
        } else {
          orig_gid=soi->second;
        }

        // Get index in input array
        int index;
        std::map<int,int>::iterator itoi=id_to_index.find(orig_gid);
        if (itoi == id_to_index.end()) { 
          continue; // Not in input array, so skip
        } else {
          index=itoi->second; // In input, so get index
        }

        // Get frac data in elem
        double frac=get_elem_frac(elem);

        // Get fraction of larger polygon
        std::map<int,double>::iterator mi=split_id_to_frac.find(elem_gid);

        // Not part of something larger, so just stick in array
        if (mi == split_id_to_frac.end()) {
          fracs[index] = frac;
          continue;
        }

        // It is part of larger original poly, so modify by fraction
        frac *= mi->second;

        // Add modified frac to what's already in array
        fracs[index] += frac;
      }

  } else { // Otherwise just get data from MOAB.
    if (elems.size() > 0) {
      merr=mesh->tag_get_data(elem_frac_tag, &elems[0], elems.size(), fracs);
      ESMC_CHECK_MOAB_THROW(merr);
    }
  }
}



void MBMesh::setup_elem_coords() {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::setup_elem_coords()"

  int localrc, merr;

  // Add element coords field
  double  dbl_def_val[3]= {0.0, 0.0, 0.0};
  merr=mesh->tag_get_handle("elem_coords", sdim, MB_TYPE_DOUBLE, elem_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
  ESMC_CHECK_MOAB_THROW(merr);

  // If not cartesian then add original coordinates field
  if (coordsys != ESMC_COORDSYS_CART) {
    merr=mesh->tag_get_handle("elem_orig_coords", orig_sdim, MB_TYPE_DOUBLE, elem_orig_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
    ESMC_CHECK_MOAB_THROW(merr);

    // Record the fact that it has original elem coords
    has_elem_orig_coords=true;
  }

  // Turn on elem coords
  has_elem_coords=true;
}


void MBMesh::set_elem_coords(EntityHandle eh, double *orig_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_coords()"
  // Error return codes
  int localrc;
  int merr;
  
  // If no coords, complain
  if (!has_elem_coords) Throw() << "Element coords not present in mesh.";


  // Set original coords if present
  if (has_elem_orig_coords) {
    merr=mesh->tag_set_data(elem_orig_coords_tag, &eh, 1, orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  }

  // Convert and set vertex coords
  double cart_coords[3]={0.0,0.0,0.0};

  // Convert original coords to cartesian
  ESMCI_CoordSys_ConvertToCart(coordsys, orig_sdim,
                               orig_coords, cart_coords);

  // Set cart coords
  // Set original coords if present
  merr=mesh->tag_set_data(elem_coords_tag, &eh, 1, cart_coords);
  ESMC_CHECK_MOAB_THROW(merr);
 
}


void MBMesh::set_elem_coords(Range elems, double *orig_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_elem_coords()"

  // Error return codes
  int localrc;
  int merr;
  
  // If no coords, complain
  if (!has_elem_coords) Throw() << "element coords not present in mesh.";

  // Set original coords if present
  if (has_elem_orig_coords) {
    merr=mesh->tag_set_data(elem_orig_coords_tag, elems, orig_coords);
    ESMC_CHECK_MOAB_THROW(merr);
  }
  
  // Loop converting and setting each cart coord. individually 
  // (This is more memory efficient, but maybe a bit less time efficient.)
  double *elem_orig_coords=orig_coords;
  for(Range::const_iterator it=elems.begin(); it !=elems.end(); it++) {
    const EntityHandle elem=*it;
    
    // Convert and set coords
    double cart_coords[3]={0.0,0.0,0.0};
    
    // Convert original coords to cartesian
    ESMCI_CoordSys_ConvertToCart(coordsys, orig_sdim,
                                 elem_orig_coords, cart_coords);
    
    // Set cart coords
    merr=mesh->tag_set_data(elem_coords_tag, &elem, 1, cart_coords);
    ESMC_CHECK_MOAB_THROW(merr);
    
    // Move to next set of coordinates
    elem_orig_coords += orig_sdim;
  }


}



void MBMesh::set_owner(EntityHandle eh, int owner) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::set_owner()"

  // Error return codes
  int localrc;
  int merr;
  
  // Get Owner
  merr=mesh->tag_set_data(owner_tag, &eh, 1, &owner);
  ESMC_CHECK_MOAB_THROW(merr);
}


int MBMesh::get_owner(EntityHandle eh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_owner()"

  // Error return codes
  int localrc;
  int merr;
  
  // Get Owner
  int owner;
  merr=mesh->tag_get_data(owner_tag, &eh, 1, &owner);
  ESMC_CHECK_MOAB_THROW(merr);

  // return owner
  return owner;
}

void MBMesh::get_owners(std::vector<EntityHandle> const &objs, int *owners) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_owners()"

  // Error return codes
  int localrc;
  int merr;
  
  // Set data in MOAB
  if (objs.size() > 0) {
    merr=mesh->tag_get_data(owner_tag, &objs[0], objs.size(), owners);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}


int MBMesh::get_orig_pos(EntityHandle eh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_orig_pos()"

  // Error return codes
  int localrc;
  int merr;
  
  // Get Owner
  int orig_pos;
  merr=mesh->tag_get_data(orig_pos_tag, &eh, 1, &orig_pos);
  ESMC_CHECK_MOAB_THROW(merr);

  // return owner
  return orig_pos;
}

void MBMesh::get_orig_pos(std::vector<EntityHandle> const &objs, int *orig_pos) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_orig_pos()"

  // Error return codes
  int localrc;
  int merr;
  
  // Set data in MOAB
  if (objs.size() > 0) {
    merr=mesh->tag_get_data(orig_pos_tag, &objs[0], objs.size(), orig_pos);
    ESMC_CHECK_MOAB_THROW(merr);
  }
}



int MBMesh::get_gid(EntityHandle eh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_gid()"

  // Error return codes
  int localrc;
  int merr;
  
  // Get gid
  int gid;
  merr=mesh->tag_get_data(gid_tag, &eh, 1, &gid);
  ESMC_CHECK_MOAB_THROW(merr);

  // return owner
  return gid;
}

void MBMesh::get_gid(std::vector<EntityHandle> const &objs, int *gids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::get_gid()"

  // Error return codes
  int localrc;
  int merr;
  
  // Set data in MOAB
  if (objs.size() > 0) {
    merr=mesh->tag_get_data(gid_tag, &objs[0], objs.size(), gids);
    ESMC_CHECK_MOAB_THROW(merr);

  }
}




// Setup MBMesh to operate in parallel by resolving shared ents, etc. 
void MBMesh::setup_parallel() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMesh::setup_parallel()"

  int localrc, merr;

  // Get MPI Communicator of current VM
  MPI_Comm mpi_comm=VM::getCurrent(&localrc)->getMpi_c();
  if (ESMC_LogDefault.MsgFoundError(localrc,ESMCI_ERR_PASSTHRU,ESMC_CONTEXT,NULL))
    throw localrc;  // bail out with exception

  // setup parallel comm, destroyed in MBMesh destructor   
  ParallelComm *pcomm= new ParallelComm(mesh, mpi_comm);

  // Get list of elements
  Range elems;
  merr=mesh->get_entities_by_dimension(0, pdim, elems);
  ESMC_CHECK_MOAB_THROW(merr);
    
  // Resolve object sharing 
  merr = pcomm->resolve_shared_ents(0, elems, pdim, pdim-1);
  ESMC_CHECK_MOAB_THROW(merr);

}

// Update parallel
// If the mesh has changed, call this to update the parallel connections
// between pieces of the mesh. (E.g. for haloing, etc...) 
// (This hasn't been tested, but it should be what has to happen.
//  TODO: Test!)
void MBMesh::update_parallel() {
#undef ESMC_METHOD
#define ESMC_METHOD "MBMesh::update_parallel()"

  int merr;

  // Get the indexed pcomm object from the interface
  // index 0 should return the one created inside MBMesh_addelements
  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);

  pcomm->set_debug_verbosity(0);

  // strange errors with edges (in moab edges are processor boundaries) showed up with redistribution
  // the best solution we found with the moab team was to remove edges after redist and prior to resolve_shared_ents
  Range edges;
  merr=mesh->get_entities_by_dimension(0, 1, edges);
  ESMC_CHECK_MOAB_THROW(merr);
  pcomm->delete_entities(edges);

  // Get current list of elements
  Range elems;
  merr=mesh->get_entities_by_dimension(0, pdim, elems);
  ESMC_CHECK_MOAB_THROW(merr);
    
  // Resolve object sharing 
  merr = pcomm->resolve_shared_ents(0, elems, pdim, pdim-1);
  ESMC_CHECK_MOAB_THROW(merr);

}

// Do halo communication for all appropriate tags on nodes
// TODO: Right now this is a work around because MOAB doesn't let you specify which 
//       is the owned version of a shared entity (it decides itself). So we use reduce min
//       and set the non-owned versions to the max. 
//       do_internal_coords - if true also halo internal vertex coords. This is more expensive, because a new tag needs
//                   to be added and removed, so if not supplied it's default off. 
void MBMesh::halo_comm_nodes_all_tags(bool do_internal_coords) {
  int localrc, merr;
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::halo_comm_nodes_all_tags()"

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
  ESMC_CHECK_MOAB_THROW(merr);

  // Loop setting unowned data to max
  for (Range::iterator it=shared_nodes.begin(); it != shared_nodes.end(); it++) {
    EntityHandle node=*it;
    
    // Only reset if not local
    if (this->get_owner(node) != localPet) {

      // ONLY FOR TESTING
      //int max_orig_pos=std::numeric_limits<int>::max();
      //merr=mesh->tag_set_data(orig_pos_tag, &node, 1, &max_orig_pos);
      //ESMC_CHECK_MOAB_THROW(merr);
      
      // Set orig_coords
      if (this->has_node_orig_coords) {
        double max_orig_coords[3]={std::numeric_limits<double>::max(),
                               std::numeric_limits<double>::max(),
                               std::numeric_limits<double>::max()};
        merr=mesh->tag_set_data(node_orig_coords_tag, &node, 1, max_orig_coords);
        ESMC_CHECK_MOAB_THROW(merr);
      }

      // Set mask
      if (this->has_node_mask) {
        int max_mask=std::numeric_limits<int>::max();
        merr=mesh->tag_set_data(node_mask_tag, &node, 1, &max_mask);
        ESMC_CHECK_MOAB_THROW(merr);

        merr=mesh->tag_set_data(node_mask_val_tag, &node, 1, &max_mask);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  }

  // Add coords if asked
  Tag tmp_node_coords_tag;
  if (do_internal_coords) {

    // Add tmp coords tag
    double dbl_def_val[3]={-1.0, -1.0, -1.0};
    merr=mesh->tag_get_handle("tmp_node_coords", sdim, MB_TYPE_DOUBLE, tmp_node_coords_tag, MB_TAG_EXCL|MB_TAG_DENSE, dbl_def_val);
    ESMC_CHECK_MOAB_THROW(merr);
   
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
        ESMC_CHECK_MOAB_THROW(merr);

        // set vertex coords in tmp tag
        merr=mesh->tag_set_data(tmp_node_coords_tag, &node, 1, coords);
        ESMC_CHECK_MOAB_THROW(merr);

      } else { // Not local set as max
        double max_coords[3]={std::numeric_limits<double>::max(),
                              std::numeric_limits<double>::max(),
                              std::numeric_limits<double>::max()};
        merr=mesh->tag_set_data(tmp_node_coords_tag, &node, 1, max_coords);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  }

  // Now do a halo by doing a reduce
  merr = pcomm->reduce_tags(node_tags, node_tags, MPI_MIN, shared_nodes);
  ESMC_CHECK_MOAB_THROW(merr);

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
        ESMC_CHECK_MOAB_THROW(merr);

        // Put into vertex
        merr=mesh->set_coords(&node,1,coords);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }

    // TODO: REMOVE tmp_node_coords_tag HERE!!!!!
  }


}

// Do halo communication for all appropriate tags on elems
// TODO: Right now this is a work around because MOAB doesn't let you specify
//       which is the owned version of a shared entity (it decides itself).
//       So we use reduce min and set the non-owned versions to the max. 
void MBMesh::halo_comm_elems_all_tags() {
  int localrc, merr;
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::halo_comm_elems_all_tags()"

  // Get localPet
  int localPet = VM::getCurrent(&localrc)->getLocalPet();
  ESMC_CHECK_THROW(localrc);

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
  ESMC_CHECK_MOAB_THROW(merr);

  // Loop setting unowned data to max
  for (Range::iterator it=shared_elems.begin(); it != shared_elems.end(); it++) {
    EntityHandle elem=*it;
    
    // Only reset if not local
    if (this->get_owner(elem) != localPet) {

      // ONLY FOR TESTING
      // int max_orig_pos=std::numeric_limits<int>::max();
      //merr=mesh->tag_set_data(orig_pos_tag, &node, 1, &max_orig_pos);
      //ESMC_CHECK_MOAB_THROW(merr);

      // Set elem_frac
      if (this->has_elem_frac) {
        double max_elem_frac=std::numeric_limits<double>::max();
        merr=mesh->tag_set_data(elem_frac_tag, &elem, 1, &max_elem_frac);
        ESMC_CHECK_MOAB_THROW(merr);
      }

      // Set elem_area
      if (this->has_elem_area) {
        double max_elem_area=std::numeric_limits<double>::max();
        merr=mesh->tag_set_data(elem_area_tag, &elem, 1, &max_elem_area);
        ESMC_CHECK_MOAB_THROW(merr);
      }

      
      // Set elem_orig_coords
      if (this->has_elem_orig_coords) {
        double max_orig_coords[3]={std::numeric_limits<double>::max(),
                               std::numeric_limits<double>::max(),
                               std::numeric_limits<double>::max()};
        merr=mesh->tag_set_data(elem_orig_coords_tag, &elem, 1, max_orig_coords);
        ESMC_CHECK_MOAB_THROW(merr);
      }

      // Set elem_coords
      if (this->has_elem_coords) {
        double max_coords[3]={std::numeric_limits<double>::max(),
                              std::numeric_limits<double>::max(),
                              std::numeric_limits<double>::max()};
        merr=mesh->tag_set_data(elem_coords_tag, &elem, 1, max_coords);
        ESMC_CHECK_MOAB_THROW(merr);
      }


      // Set mask
      if (this->has_elem_mask) {
        int max_mask=std::numeric_limits<int>::max();
        merr=mesh->tag_set_data(elem_mask_tag, &elem, 1, &max_mask);
        ESMC_CHECK_MOAB_THROW(merr);

        merr=mesh->tag_set_data(elem_mask_val_tag, &elem, 1, &max_mask);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  }

  // Now do a halo by doing a reduce
  merr = pcomm->reduce_tags(elem_tags, elem_tags, MPI_MIN, shared_elems);
  ESMC_CHECK_MOAB_THROW(merr);

#if 0
  Range elems;
  merr=this->mesh->get_entities_by_dimension(0, this->pdim, elems);
  ESMC_CHECK_MOAB_THROW(merr);

  merr = pcomm->exchange_tags(elem_tags, elem_tags, elems);
  ESMC_CHECK_MOAB_THROW(merr);
#endif

}



MBMesh::~MBMesh() {

  // MOAB::pcomm
  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);
  // MOAB::get_comm returns NULL if it hasn't been created
  if (pcomm) delete pcomm;

  // Deallocate the MOAB::Interface
  if (mesh != NULL) delete mesh;
} 


void MBMesh::debug_output_nodes() {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::debug_output_nodes()"
  try {
    int localrc, merr;

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // get the indexed pcomm object from the interface
    // ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);

    // Get range of nodes
    Range nodes;
    this->get_all_nodes(nodes);

    // Output info
    cout<<"\n";
    cout<<localPet<<"# NODE OVERALL INFO: total num="<<nodes.size();
    cout<<" has_orig_coords="<<has_node_orig_coords;
    cout<<" has_node_mask="<<has_node_mask;
    cout<<"\n";


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
      this->get_node_cart_coords(node,coords);
      cout<<" coords=["<<coords[0]<<" "<<coords[1];
      if (sdim > 2) cout<<" "<<coords[2];
      cout<<"]";

      // Orig coords
      if (has_node_orig_coords) {
        double orig_coords[3];
        this->get_node_orig_coords(node, orig_coords);
        cout<<" orig_coords=["<<coords[0]<<" "<<coords[1];
        if (orig_sdim > 2) cout<<" "<<coords[2];
        cout<<"]";
      }

      // Orig coords
      if (has_node_mask) {
        int mask_val=this->get_node_mask_val(node);
        cout<<" mask_val="<<mask_val;
      }

      //end line
      cout<<"\n";
    }
  }
  CATCH_MBMESH_RETHROW
}

void MBMesh::debug_output_elems() {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::debug_output_elems()"
  try {
    int localrc, merr;

    // Get localPet
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // get the indexed pcomm object from the interface
    //  ParallelComm *pcomm = ParallelComm::get_pcomm(this->mesh, 0);

    // Output info
    cout<<"\n";
    cout<<localPet<<"# ELEM OVERALL INFO: num="<<num_elem();
    cout<<" has_elem_coords="<<has_elem_coords;
    cout<<" has_elem_orig_coords="<<has_elem_orig_coords;
    cout<<" has_elem_mask="<<has_elem_mask;
    cout<<" has_elem_area="<<has_elem_area;
    cout<<"\n";


    // Get range of elems
    Range elems;
    merr=mesh->get_entities_by_dimension(0, pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);


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
      
      // get elem orig pos
      int orig_pos=this->get_orig_pos(elem);
      cout<<" orig_pos="<<orig_pos;

      // Output coords
      if (has_elem_coords) {
        double coords[3];
        this->get_elem_cart_coords(elem,coords);
        cout<<" coords=["<<coords[0]<<" "<<coords[1];
        if (sdim > 2) cout<<" "<<coords[2];
        cout<<"]";
      }

      // Orig coords
      if (has_elem_orig_coords) {
        double orig_coords[3];
        this->get_elem_orig_coords(elem, orig_coords);
        cout<<" orig_coords=["<<orig_coords[0]<<" "<<orig_coords[1];
        if (orig_sdim > 2) cout<<" "<<orig_coords[2];
        cout<<"]";
      }

      // Mask value
      if (has_elem_mask) {
        int mask_val=this->get_elem_mask_val(elem);
        cout<<" mask_val="<<mask_val;
      }

      // Mask value
      if (has_elem_area) {
        int area=this->get_elem_area(elem);
        cout<<" area="<<area;
      }


      //end line
      cout<<"\n";
    }
    cout<<"\n";
  }
  CATCH_MBMESH_RETHROW
}

// Call after all nodes have been added to setup some internal stuff
void MBMesh::finalize_nodes() {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::finalize_nodes()"
  try {
    //RLO: we had considered separate calls for the counts, however it
    //     saves nothing in time, and very little in overall memory
  
    vector<EntityHandle> all_nodes;
    get_all_nodes(all_nodes);  

    vector<EntityHandle> orig_nodes;
    get_sorted_orig_nodes(orig_nodes);
    
    vector<EntityHandle> owned_nodes;
    get_owned_nodes(owned_nodes);
  
    _num_node = all_nodes.size();
    _num_orig_node = orig_nodes.size();
    _num_owned_node = owned_nodes.size();

    // Mark nodes as finalized, so things can be used
    nodes_finalized=true;

  }
  CATCH_MBMESH_RETHROW
}


// Call after all elems have been added to setup some internal stuff
void MBMesh::finalize_elems() {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::finalize_elems()"
  try {
    //RLO: we had considered separate calls for the counts, however it
    //     saves nothing in time, and very little in overall memory, plus
    //     the vectors are useful for getting the connectivity counts

    vector<EntityHandle> all_elems;
    get_all_elems(all_elems);

    vector<EntityHandle> orig_elems;
    get_sorted_orig_elems(orig_elems);

    vector<EntityHandle> owned_elems;
    get_owned_elems(owned_elems);

    _num_elem = all_elems.size();
    _num_owned_elem = owned_elems.size();
    _num_orig_elem = orig_elems.size();
    
    // NOTE: cannot get connectivity of ngons
    if (!this->is_split) {
      _num_elem_conn = get_num_elem_conn(all_elems);
      _num_owned_elem_conn = get_num_elem_conn(owned_elems);
      _num_orig_elem_conn = get_num_elem_conn(orig_elems);
    }

    // Mark elems as finalized, so things can be used
    elems_finalized=true;
  }
  CATCH_MBMESH_RETHROW
}


void MBMesh::CreateGhost() {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::CreateGhost()"
  try {
    int localrc, merr;

    // If already ghosted, leave
    if (has_ghost) return;

    // Get VM info
    ESMC_VM vm;
    vm = ESMC_VMGetCurrent(&localrc);
    ESMC_CHECK_THROW(localrc);

    int localPet, petCount;
    MPI_Comm mpi_comm;
    localrc = ESMC_VMGet(vm, &localPet, &petCount, NULL, &mpi_comm, NULL, NULL);
    ESMC_CHECK_THROW(localrc);

    // set up the MOAB parallel environment
    ParallelComm *pcomm= new ParallelComm(mesh, mpi_comm);

    // Get list of elements
    Range elems;
    merr=mesh->get_entities_by_dimension(0, pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);
      
    // Resolve object sharing 
    merr = pcomm->resolve_shared_ents(0, elems, pdim, pdim-1);
    ESMC_CHECK_MOAB_THROW(merr);

    // Now exchange 1 layer of ghost elements, using vertices as bridge
    merr = pcomm->exchange_ghost_cells(this->pdim, // int ghost_dim
                                       0, // int bridge_dim
                                       1, // int num_layers
                                       0, // int addl_ents
                                       true);// bool store_remote_handles
    ESMC_CHECK_MOAB_THROW(merr);

    vector<Tag> node_tags;
    vector<Tag> elem_tags;
    
    node_tags.push_back(this->gid_tag);
    // Don't comm. orig_pos because it's different each PET 
    // node_tags.push_back(this->orig_pos_tag); 
    node_tags.push_back(this->owner_tag);
    if (this->has_node_orig_coords) node_tags.push_back(this->node_orig_coords_tag);
    if (this->has_node_mask) {
      node_tags.push_back(this->node_mask_tag);
      node_tags.push_back(this->node_mask_val_tag);
    }
    
    elem_tags.push_back(this->gid_tag);
    // Don't comm. orig_pos because it's different each PET 
    // elem_tags.push_back(this->orig_pos_tag);
    elem_tags.push_back(this->owner_tag);
    if (this->has_elem_frac) elem_tags.push_back(this->elem_frac_tag);
    if (this->has_elem_mask) {
      elem_tags.push_back(this->elem_mask_tag);
      elem_tags.push_back(this->elem_mask_val_tag);
    }
    if (this->has_elem_area) elem_tags.push_back(this->elem_area_tag);
    if (this->has_elem_coords) elem_tags.push_back(this->elem_coords_tag);
    if (this->has_elem_orig_coords) elem_tags.push_back(this->elem_orig_coords_tag);
    
    Range nodes;
    merr=this->mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    merr = pcomm->exchange_tags(node_tags, node_tags, nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    merr = pcomm->exchange_tags(elem_tags, elem_tags, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    // Get rid of the pcomm object now that we're done with it
    delete pcomm;

    // Mark as ghosted
    has_ghost=true;

  }
  CATCH_MBMESH_RETHROW
}

// Called after a Redist
// Change the proc numbers in a mesh to correspond to a different set. This isn't to 
// merge procs into one another, but to map them to different number. E.g. if they 
// are being switched to a different VM or MPI_COMM.
void MBMesh::map_proc_numbers(int num_procs, int *proc_map) {
#undef  ESMC_METHOD
#define ESMC_METHOD "MBMesh::map_proc_numbers()"
  try {
    int localrc, merr;

    // Get range of nodes
    Range nodes;
    merr=mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through nodes changing owners to owners in new VM
    for (Range::const_iterator it=nodes.begin(); it != nodes.end(); it++) {
      EntityHandle node=*it;

      int orig_owner=this->get_owner(node);
      if ((orig_owner < 0) || (orig_owner > num_procs-1)) {
        Throw() << " mesh node owner rank outside current vm";
      }

      // map to new owner rank in new vm
      int new_owner=proc_map[orig_owner];
      // Make sure that the new one is ok
      if (new_owner < 0) {
        Throw() << " mesh node owner outside of new vm";
      }

      // Set new owner
      this->set_owner(node, new_owner);
    }

    // Get range of elems
    Range elems;
    merr=mesh->get_entities_by_dimension(0, pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through elems changing owners to owners in new VM
    for (Range::const_iterator it=elems.begin(); it != elems.end(); it++) {
      EntityHandle elem=*it;

      int orig_owner=this->get_owner(elem);
      // Error check owner
      if ((orig_owner < 0) || (orig_owner > num_procs-1)) {
        Throw() << " mesh element owner rank outside current vm";
      }

      // map to new owner rank in new vm
      int new_owner=proc_map[orig_owner];
      // Make sure that the new one is ok
      if (new_owner < 0) {
         Throw() << " mesh element owner outside of new vm";
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
  CATCH_MBMESH_RETHROW
}


#endif // ESMF_MOAB



