// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Mesh_Regrid_Glue.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
 // INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"
#include "ESMCI_GridToMesh.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/Legacy/ESMCI_MeshRead.h"
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h"
#include "Mesh/include/Legacy/ESMCI_Exception.h"
#include "Mesh/include/Regridding/ESMCI_Integrate.h"
#include "Mesh/include/Regridding/ESMCI_Interp.h"
#include "Mesh/include/Regridding/ESMCI_ExtrapolationPoleLGC.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/ESMCI_MathUtil.h"
#include "Mesh/include/Legacy/ESMCI_Phedra.h"
#include "Mesh/include/ESMCI_Mesh_Regrid_Glue.h"
#include "Mesh/include/Legacy/ESMCI_MeshMerge.h"
#include "Mesh/include/ESMCI_Mesh_GToM_Glue.h"
#include "Mesh/include/ESMCI_DInfo.h"


#include <iostream>
#include <vector>
#include <map>

  //------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
//
 //EOP
//-------------------------------------------------------------------------


using namespace ESMCI;



// Get information about vector dimensions for vectorRegrid capability
void get_vec_dims_for_vectorRegrid(ESMCI::Array &array, int &num_vec_dims, int *vec_dims_undist_seqind) {

  // Get undisributed dimension sizes
  const int *undistLBound=array.getUndistLBound();
  const int *undistUBound=array.getUndistUBound();

  // We error checked earlier that there is only one undist bound, but double check here that these aren't NULL
  ThrowRequire(undistLBound != NULL);
  ThrowRequire(undistUBound != NULL);

  // Calculate size of vector dims
  num_vec_dims=undistUBound[0]-undistLBound[0]+1;

  // Fill undistibuted seqinds for vector dims
  // (This is easy now, but will be harder as the number of undist. dims gets larger than 1.)
  for (int i=0; i<num_vec_dims; i++) {
    vec_dims_undist_seqind[i]=i+1;
  }
}


// Temporary class for holding coordinates
class Coord {


  // Data
  double c[3];

public:

  Coord() {
    c[0]=0.0;
    c[1]=0.0;
    c[2]=0.0;
  }
  
  
  Coord(double x, double y, double z) {
    c[0]=x;
    c[1]=y;
    c[2]=z;
  }
  
  // Less 
  bool operator<(const Coord &rhs) const {
    if (c[0] != rhs.c[0]) {
      return c[0] < rhs.c[0];
    } else if (c[1] != rhs.c[1]) {
      return c[1] < rhs.c[1];
    }
    return c[2] < rhs.c[2];
  }

  // Do we need this? 
  bool operator==(const Coord &rhs) const {
    return (c[0] == rhs.c[0] && c[1] == rhs.c[1] && c[2] == rhs.c[2]);
  }

  // Fill _coords with coordinate values.  _coords must be at least size 3.
  void get_coords(double *_coords) {
    _coords[0]=c[0];
    _coords[1]=c[1];
    _coords[2]=c[2];
  }
  
};



// Class that lets you search for coords by Id
class CoordFromId {

  class CoordFromIdEntry{
  public:
    int id;
    Coord coord;

    // Constructors
    CoordFromIdEntry(int _id, double x, double y, double z): id(_id), coord(x,y,z) {}
    CoordFromIdEntry(int _id, Coord _coord): id(_id), coord(_coord) {}
    

    // Less 
    bool operator<(const CoordFromIdEntry &rhs) const {
      if (id != rhs.id) {
        return id < rhs.id;
      }
      return coord < rhs.coord;
    }

    // Do we need this? 
    bool operator==(const CoordFromIdEntry &rhs) const {
      return (id == rhs.id && coord == rhs.coord);
    }
    
  };

  // A less function object for CoordFromIdEntry that only cares about id.
  // Used below in lower_bound search by just id.
  class CoordFromIdEntry_just_id_less : public std::binary_function<CoordFromIdEntry, CoordFromIdEntry, bool> {
  public:
    CoordFromIdEntry_just_id_less() {}
    bool operator()(const CoordFromIdEntry  &l, const CoordFromIdEntry &r) {
      return l.id < r.id;
    }
  };
  
  
  
  // Data
  bool is_searchable;
  std::vector<CoordFromIdEntry> searchable; // After committing this will contain a sorted list for searching


  
 
public:
  
  // Create empty
  CoordFromId(): is_searchable(false) {
    
  }

  
  // Add points to query structure from Mesh
  void add(Mesh *mesh, MeshObj::id_type obj_type);

  // Add points to query structure from PointList
  void add(PointList *pl);

  // Make searchable
  void make_searchable();

  // This method makes the structure searchable for a particular set
  // of possibly non-local ids. In this case, for convenience, the ids
  // are passed in in the form of the weight matrix index list. sord
  // give whether the ids from the source (sord=0) or destination (sord=1) should
  // be used.
  void make_searchable(int num_entries, int *iientries, int sord);
  
  // Search
  // Returns true if id has been found. In that case fills coords_out with coords of
  // point for that id, coords_out must be of size >=3.
  bool search(int search_id, double *coords_out) {

    // Find id in searchable list
    std::vector<CoordFromIdEntry>::iterator ei; // break into two lines, so easier to read
    ei = std::lower_bound(searchable.begin(),
                          searchable.end(),
                          CoordFromIdEntry(search_id, 0.0, 0.0, 0.0),
                          CoordFromIdEntry_just_id_less()); 
    

    // If within list
    if (ei != searchable.end()) {
      CoordFromIdEntry &lb_cfie = *ei;
      
      // If the ids match, then we've found an answer
      if (lb_cfie.id == search_id) {
        
        // Get coords from entry
        lb_cfie.coord.get_coords(coords_out);
        
        // Report success
        return true;
      } 
    }   
    
    // Didn't find id, so report that
    return false;
  }
  
};




void CoordFromId::add(Mesh *mesh, MeshObj::id_type obj_type) {

  // If already searchable, don't allow more to be added
  if (is_searchable) {
    Throw() << "CoordFromId object has already been made searchable, more points can't be added.";
  }
  
  // Get spatial dimension
  int sdim=mesh->spatial_dim();
  
  // Add based on obj_type
  if (obj_type == MeshObj::NODE) {

    // Reserve to the correct size
    searchable.reserve(mesh->num_nodes());
    
    // Get coordinate data
    MEField<> *node_coords=mesh->GetField("coordinates");
    ThrowRequire(node_coords != NULL);
    
    // Add
    if (sdim == 2) { 
      Mesh::iterator ni = mesh->node_begin(), ne = mesh->node_end();
      for (; ni != ne; ++ni) {
        MeshObj &node = *ni;
        
        // Skip if not local
        if (!GetAttr(node).is_locally_owned()) continue;
        
        // Get id
        int id=node.get_id();
        
        // Get pointer to coords
        double *c = node_coords->data(node);
        
        // Add to list
        searchable.push_back(CoordFromIdEntry(id,c[0],c[1],0.0));      
      }
    } else if (sdim == 3) { 
      Mesh::iterator ni = mesh->node_begin(), ne = mesh->node_end();
      for (; ni != ne; ++ni) {
        MeshObj &node = *ni;
        
        // Skip if not local
        if (!GetAttr(node).is_locally_owned()) continue;
        
        // Get id
        int id=node.get_id();
        
        // Get pointer to coords
        double *c = node_coords->data(node);
        
        // Add to list
        searchable.push_back(CoordFromIdEntry(id,c[0],c[1],c[2]));      
      }
    } else {
      Throw() << "Meshes of spatial dim= "<<sdim<<" not supported in vector regridding.";
    }

  } else if (obj_type == MeshObj::ELEMENT) {

    // Reserve to the correct size
    searchable.reserve(mesh->num_elems());
    
    // Get element coordinate data
    MEField<> *elem_coords=mesh->GetField("elem_coordinates");
    if (elem_coords == NULL) Throw() << "Vector regridding not supported on Mesh elements when the elements don't have center coordinates.";
    
    // Add based on spatial dim
    if (sdim == 2) { 
      Mesh::iterator ei = mesh->elem_begin(), ee = mesh->elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;
        
        // Skip if not local
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Get id
        int id=elem.get_id();
        
        // Get pointer to coords
        double *c = elem_coords->data(elem);
        
        // Add to list
        searchable.push_back(CoordFromIdEntry(id,c[0],c[1],0.0));      
      }
    } else if (sdim == 3) { 
      Mesh::iterator ei = mesh->elem_begin(), ee = mesh->elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;
        
        // Skip if not local
        if (!GetAttr(elem).is_locally_owned()) continue;
        
        // Get id
        int id=elem.get_id();
        
        // Get pointer to coords
        double *c = elem_coords->data(elem);
        
        // Add to list
        searchable.push_back(CoordFromIdEntry(id,c[0],c[1],c[2]));      
      }
    } else {
      Throw() << "Meshes of spatial dim= "<<sdim<<" not supported in vector regridding.";
    } 
  } else {
    Throw() << "Unrecognized Mesh object type.";
  }

}

void CoordFromId::add(PointList *pl) {

  // If already searchable, don't allow more to be added
  if (is_searchable) {
    Throw() << "CoordFromId object has already been made searchable, more points can't be added.";
  }

  
  // Get spatial (coordinate) dim
  int sdim=pl->get_coord_dim();

  // Reserve to the correct size
  searchable.reserve(pl->get_curr_num_pts());
  
  // Add points based on spatial dim
  if (sdim == 2) { 
    // Loop adding ids and points
    int num_pts = pl->get_curr_num_pts();
    for (int i=0; i<num_pts; i++) {
      
      // Get point id
      int id = pl->get_id(i);
      
      // Get point coords
      const double *c = pl->get_coord_ptr(i);   
      
      // Add to list
      searchable.push_back(CoordFromIdEntry(id,c[0],c[1],0.0));          
    }
  } else if (sdim == 3) { 
    // Loop adding ids and points
    int num_pts = pl->get_curr_num_pts();
    for (int i=0; i<num_pts; i++) {
      
      // Get point id
      int id = pl->get_id(i);
      
      // Get point coords
      const double *c = pl->get_coord_ptr(i);   
      
      // Add to list
      searchable.push_back(CoordFromIdEntry(id,c[0],c[1],c[2]));          
    }    
  } else {
    Throw() << "Geometries of spatial dim= "<<sdim<<" not supported in vector regridding.";
  }

}

 void CoordFromId::make_searchable() {

  // If already done, then leave
  if (is_searchable) return;
  
  // Sort to make quickly searchable
  std::sort(searchable.begin(),searchable.end());
  
  // Mark as searchable
  is_searchable=true;
}


// This method makes the structure searchable for a particular set
// of possibly non-local ids. In this case, for convenience, the ids
// are passed in in the form of the weight matrix index list. sord
// give whether the ids from the source (sord=0) or destination (sord=1) should
// be used.
void CoordFromId::make_searchable(int num_entries, int *iientries, int sord) {

  // If already done, then leave
  if (is_searchable) return;

  // Error check sord
  if ((sord < 0) || (sord > 1)) {
    Throw() << "Value= "<<sord<<" is an unsupported value in source or destination indicator.";
  }

  
  //// Get unique set of ids that will be searched for on this PET}

  // Declare id storage
  std::vector<int> search_ids;

  // Reserve a reasonable size
  search_ids.reserve(num_entries);
  
  // Get list of ids on this PET
  int pos=sord;
  for (int i=0; i< num_entries; i++) {
    search_ids.push_back(iientries[pos]);
    pos += 2; // Advance to next pair of entries
  }

  // Sort to allow unique to work
  std::sort(search_ids.begin(),search_ids.end());

  // Unique ids to make search and comm more efficient
  search_ids.erase(std::unique(search_ids.begin(), search_ids.end()), search_ids.end());


  //// Set up class for distributed search

  // Allocate array where the results will go
  Coord *results = new Coord[search_ids.size()];    
    
  // Declare distributed search structure
  {
    DInfo<int,Coord> di_search;
    
    // Reserve space
    di_search.reserve(searchable.size());
    
    // Add search items
    for (CoordFromIdEntry cfie : searchable) {
      di_search.add(cfie.id,cfie.coord);
    }
    
    // Commit to make searchable
    di_search.commit();
    

    //// Do distributed search on local set of ids
    
    // Declare not found info value
    Coord bad_value(0.0,0.0,0.0);
    
    // Do search
    // (Because of true will return error if id not found)
    if (search_ids.empty()) {
      di_search.search(0, NULL, true, bad_value, results);
    } else {
      di_search.search(search_ids.size(), search_ids.data(), true, bad_value, results);
    }
    
  } // End of block to get rid of search struct
  
  
  //// Use distributed results to reconstruct search vector

  // Empty current search vector
  searchable.clear();
  
  // Add new distributed objects to search vector
  for (auto i=0; i<search_ids.size(); i++) {
    searchable.push_back(CoordFromIdEntry(search_ids[i],results[i]));
  }

  // Now that we don't need them anymore, get rid of results
  delete [] results;

  
  //// Make new searchable vector searchable
  
  // Sort to make quickly searchable
  std::sort(searchable.begin(),searchable.end());
  
  // Mark as searchable
  is_searchable=true;
}


// Compute n_vec,e_vec unit basis vectors from 3D Cart.
// All vector args should at least be of size 3
void _calc_basis_vec(double *cart_coords, double *n_vec, double *e_vec) {
  
  // Convert 3D Cart to lon, lat, etc.in radians
  double lon, lat, rad;
  convert_cart_to_sph_rad(cart_coords[0], cart_coords[1], cart_coords[2],
                          &lon, &lat, &rad);

  // Calculate north vec
  n_vec[0]=-sin(lat)*cos(lon);
  n_vec[1]=-sin(lat)*sin(lon);
  n_vec[2]= cos(lat);

  // Normalize
  double n_vec_len=MU_LEN_VEC3D(n_vec);
  if (n_vec_len != 0.0) {
    double div_len=1.0/n_vec_len;
    MU_MULT_BY_SCALAR_VEC3D(n_vec,n_vec,div_len);
  }


  // Calculate east vec
  e_vec[0]=-sin(lon);
  e_vec[1]= cos(lon);
  e_vec[2]= 0.0;

  // Normalize
  double e_vec_len=MU_LEN_VEC3D(e_vec);
  if (e_vec_len != 0.0) {
    double div_len=1.0/e_vec_len;
    MU_MULT_BY_SCALAR_VEC3D(e_vec,e_vec,div_len);
  }
}


// src_coords, dst_coords must be at least of size 3
// vec_wgts must be at least of size 4
// This assumes that the first vector component (1) is east and the second (2) is north
void _calc_2D_vec_weights(double *src_coords, double *dst_coords, double *vec_wgts) {

  // Get src basis vectors
  double src_n_vec[3];
  double src_e_vec[3];
  _calc_basis_vec(src_coords, src_n_vec, src_e_vec);


  // Get dst basis vectors
  double dst_n_vec[3];
  double dst_e_vec[3];
  _calc_basis_vec(dst_coords, dst_n_vec, dst_e_vec);
  
  // Calc. vector weights
  vec_wgts[0] = src_e_vec[0]*dst_e_vec[0]+src_e_vec[1]*dst_e_vec[1]+src_e_vec[2]*dst_e_vec[2]; // src 1 to  dst 1
  vec_wgts[1] = src_n_vec[0]*dst_e_vec[0]+src_n_vec[1]*dst_e_vec[1]+src_n_vec[2]*dst_e_vec[2]; // src 2 to  dst 1
  vec_wgts[2] = src_e_vec[0]*dst_n_vec[0]+src_e_vec[1]*dst_n_vec[1]+src_e_vec[2]*dst_n_vec[2]; // src 1 to  dst 2
  vec_wgts[3] = src_n_vec[0]*dst_n_vec[0]+src_n_vec[1]*dst_n_vec[1]+src_n_vec[2]*dst_n_vec[2]; // src 2 to  dst 2
}



///////////////////////////////////////////////////////////

// Create a vector sparse matrix from a regular one

void create_vector_sparse_mat_from_reg_sparse_mat(int num_entries, int *iientries, double *factors,
                                                          int num_vec_dims, int *src_vec_dims_undist_seqind, int *dst_vec_dims_undist_seqind,
                                                          Mesh *src_mesh, PointList *src_pl,
                                                          Mesh *dst_mesh, PointList *dst_pl,
                                                          int &num_entries_vec, int *&iientries_vec, double *&factors_vec) {

  // Set up coordinate query for source
  CoordFromId srcCoordFromId;
  if (src_pl != NULL) {
    srcCoordFromId.add(src_pl);
  } else if (src_mesh != NULL) {
    srcCoordFromId.add(src_mesh, MeshObj::NODE); // TODO: Make this ELEMENT for conservative
  } else {
    Throw() <<"No available geometry object to get coords from.";
  }

  // Make searchable
  srcCoordFromId.make_searchable(num_entries, iientries, 0);
  
  
  // Set up coordinate query for destination
  CoordFromId dstCoordFromId;
  if (dst_pl != NULL) {
    dstCoordFromId.add(dst_pl);
  } else if (dst_mesh != NULL) {
    dstCoordFromId.add(dst_mesh, MeshObj::NODE); // TODO: Make this ELEMENT for conservative
  } else {
    Throw() <<"No available geometry object to get coords from.";
  }

  // Make searchable
  dstCoordFromId.make_searchable(); 
  
  
  // Size of a vector matrix compared to non-vector
  int vec_factor=num_vec_dims*num_vec_dims;
  
  // Allocate new vector matrix
  num_entries_vec=vec_factor*num_entries;
  iientries_vec = new int[4*num_entries_vec]; 
  factors_vec= new double[num_entries_vec];

  // Loop calculating vector entries from regular ones
  int factor_pos=0;
  int iientries_pos=0;
  int factor_vec_pos=0;
  int iientries_vec_pos=0;
  for (auto i=0; i<num_entries; i++) {

    // Get factor
    double factor = factors[factor_pos];
    
    // Get src id
    int src_id=iientries[iientries_pos];

    // Get dst id
    int dst_id=iientries[iientries_pos+1];

    // Get src coords
    double src_coords[3];
    if (!srcCoordFromId.search(src_id, src_coords)) {
      Throw()<<"src id="<<src_id<<" not found in coordinate search.";
    }

    // printf("id=%d src_coords=%f %f %f\n",src_id,src_coords[0],src_coords[1],src_coords[2]);

    
    // Get dst coords, complain if not there
    double dst_coords[3];
    if (!dstCoordFromId.search(dst_id, dst_coords)) {
      Throw()<<"dst id="<<dst_id<<" not found in coordinate search.";
    }
    

    // Add new vector weights
    if (num_vec_dims == 2) {

      // Use coords to calculate new matrix entries based on vec_dim
      double vec_wgts[4];
      _calc_2D_vec_weights(src_coords, dst_coords, vec_wgts);
      
      // Fill in new matrix entries
      iientries_vec[iientries_vec_pos] = src_id;
      iientries_vec[iientries_vec_pos+1] = 1;
      iientries_vec[iientries_vec_pos+2] = dst_id;
      iientries_vec[iientries_vec_pos+3] = 1;
      factors_vec[factor_vec_pos]=factor*vec_wgts[0];

      iientries_vec[iientries_vec_pos+4] = src_id;
      iientries_vec[iientries_vec_pos+5] = 2;
      iientries_vec[iientries_vec_pos+6] = dst_id;
      iientries_vec[iientries_vec_pos+7] = 1;
      factors_vec[factor_vec_pos+1]=factor*vec_wgts[1];

      iientries_vec[iientries_vec_pos+8] = src_id;
      iientries_vec[iientries_vec_pos+9] = 1;
      iientries_vec[iientries_vec_pos+10] = dst_id;
      iientries_vec[iientries_vec_pos+11] = 2;
      factors_vec[factor_vec_pos+2]=factor*vec_wgts[2];

      iientries_vec[iientries_vec_pos+12] = src_id;
      iientries_vec[iientries_vec_pos+13] = 2;
      iientries_vec[iientries_vec_pos+14] = dst_id;
      iientries_vec[iientries_vec_pos+15] = 2;
      factors_vec[factor_vec_pos+3]=factor*vec_wgts[3];      
      
    } else {
      Throw() << "Fields with a vector dim of "<<num_vec_dims<<" are currently not supported in vector regridding.";      
    }
       
    // Advance position in regular matrix    
    factor_pos += 1;
    iientries_pos += 2;

    // Advance position in new vector matrix    
    factor_vec_pos += vec_factor;
    iientries_vec_pos += 4*vec_factor;
  }

  
}


