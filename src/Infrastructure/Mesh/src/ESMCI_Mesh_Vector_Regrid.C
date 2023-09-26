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
#include "Mesh/include/Regridding/ESMCI_MeshRegrid.h"
#include "Mesh/include/Legacy/ESMCI_Exception.h"
#include "Mesh/include/Regridding/ESMCI_Interp.h"
#include "Mesh/include/ESMCI_MathUtil.h"
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

///////////////////////////// Outward facing interface //////////////////////////////////////////

// Get information about vector dimensions for the vector regrid capability
// INPUTS:
// + array - the ESMF Array from the Field which contains the vector data
// OUTPUTS:
// + num_vec_dims - the number of components in the vector dimensions
// + vec_dims_undist_seqind - the tensor seqinds of the vector components. HOWEVER, since we only
//                            support one ungridded dim right now, currently this is just the first few tensor dims.
//                            Needs to be of size at least num_vec_dims.
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




//////////// File local classes and subroutines used by create_vector_sparse_mat_from_reg_sparse_mat() ///////////

// Class for holding coordinates to make other classes below cleaner
class Coord {
  // Data
  double c[2];  // Holds lon, lat in radians

public:

  Coord() {
    c[0]=0.0;
    c[1]=0.0;
  }
    
  Coord(double lon, double lat) {
    c[0]=lon;
    c[1]=lat;
  }
  
  // Less 
  bool operator<(const Coord &rhs) const {
    if (c[0] != rhs.c[0]) {
      return c[0] < rhs.c[0];
    }
    return c[1] < rhs.c[1];
  }

  // Do we need this? 
  bool operator==(const Coord &rhs) const {
    return (c[0] == rhs.c[0] && c[1] == rhs.c[1]);
  }

  // Fill _coords with coordinate values.  _coords must be at least size 3.
  void get_coords(double *_coords) {
    _coords[0]=c[0];
    _coords[1]=c[1];
  }
  
};


// Class that lets you search for coords by Id. It used DInfo<> to allow the search to be parallel
class CoordFromId {

  class CoordFromIdEntry{
  public:
    int id;
    Coord coord;

    // Constructors
    CoordFromIdEntry(int _id, double lon, double lat): id(_id), coord(lon,lat) {}
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
                          CoordFromIdEntry(search_id, 0.0, 0.0),
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
  int orig_sdim=mesh->spatial_dim();
  if (orig_sdim < 2) Throw() << "Vector regridding only supported with meshes with >=2 coordinate dims.";

  
  // Add based on obj_type
  if (obj_type == MeshObj::NODE) {

    // Reserve to the correct size
    searchable.reserve(mesh->num_nodes());
    
    // Get coordinate data
    MEField<> *node_coords=mesh->GetField("orig_coordinates");
    if (!node_coords) Throw() << "Vector regridding not supported without original coords.";
    
    // Add coordinates, but only the first two (just need lon,lat)
    Mesh::iterator ni = mesh->node_begin(), ne = mesh->node_end();
    for (; ni != ne; ++ni) {
      MeshObj &node = *ni;
      
      // Skip if not local
      if (!GetAttr(node).is_locally_owned()) continue;
      
      // Get id
      int id=node.get_id();
      
      // Get pointer to coords
      double *c = node_coords->data(node);

      // If degrees, convert
      if (mesh->coordsys == ESMC_COORDSYS_SPH_DEG) {
        c[0] *= ESMC_CoordSys_Deg2Rad;
        c[1] *= ESMC_CoordSys_Deg2Rad;
      }
      
      // Add to list
      searchable.push_back(CoordFromIdEntry(id,c[0],c[1]));      
    }

  } else if (obj_type == MeshObj::ELEMENT) {

    // Reserve to the correct size
    searchable.reserve(mesh->num_elems());
    
    // Get element coordinate data
    MEField<> *elem_coords=mesh->GetField("elem_orig_coordinates");
    if (elem_coords == NULL) Throw() << "Vector regridding not supported on Mesh elements when the elements don't have original center coordinates.";
    
    // Add coordinates, but only the first two (just need lon,lat)
    Mesh::iterator ei = mesh->elem_begin(), ee = mesh->elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
        
      // Skip if not local
      if (!GetAttr(elem).is_locally_owned()) continue;
      
      // Get id
      int id=elem.get_id();
      
      // Get pointer to coords
      double *c = elem_coords->data(elem);

      // If degrees, convert
      if (mesh->coordsys == ESMC_COORDSYS_SPH_DEG) {
        c[0] *= ESMC_CoordSys_Deg2Rad;
        c[1] *= ESMC_CoordSys_Deg2Rad;
      }
      
      // Add to list
      searchable.push_back(CoordFromIdEntry(id,c[0],c[1]));      
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

  printf("Pl->hasOrigCoords=%d\n",pl->hasOrigCoords());

  
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
      searchable.push_back(CoordFromIdEntry(id,c[0],c[1]));          
    }
  } else if (sdim == 3) { 
    // Loop adding ids and points
    int num_pts = pl->get_curr_num_pts();
    for (int i=0; i<num_pts; i++) {
      
      // Get point id
      int id = pl->get_id(i);
      
      // Get point coords
      const double *c = pl->get_coord_ptr(i);   

      // Convert 3D Cart to lon, lat, etc.in radians
      double lon, lat, rad;
      convert_cart_to_sph_rad(c[0], c[1], c[2],
                              &lon, &lat, &rad);

      
      // Add to list
      //searchable.push_back(CoordFromIdEntry(id,lon,lat));
      double oc[3];
      pl->get_orig_coord(i,oc);   

      oc[0] *= ESMC_CoordSys_Deg2Rad;
      oc[1] *= ESMC_CoordSys_Deg2Rad;
      
      //      printf("id=%d converted coords=%f %f  orig coords=%f %f\n",id,lon,lat,oc[0],oc[1]);

      searchable.push_back(CoordFromIdEntry(id,oc[0],oc[1]));
      
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
    Coord bad_value(0.0,0.0);
    
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
// sph_coords - is of size 2 containing lon and lat
// n_vec - north vec is of size 3
// e_vec - east vec is of size 3
static void _calc_basis_vec(double *sph_coords, double *n_vec, double *e_vec) {

  // Copy to single variables for clarity
  double lon=sph_coords[0];
  double lat=sph_coords[1];
  
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


// src_coords, dst_coords must be at least of size 2
// vec_wgts must be at least of size 4
// This assumes that the first vector component (1) is east and the second (2) is north
static void _calc_2D_vec_weights(double *src_coords, double *dst_coords, double *vec_wgts) {

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



///////////////////////////// Outward facing interface //////////////////////////////////////////

// This interface transforms a standard weight matrix into one that works on vectors. The
// vectors are mapped into 3D Cartesian space, regridded using the matrix, and then mapped
// back to vectors on the sphere. Currently the basis vectors used to map to and from 3D Cart.
// are standard north and east calculated by ESMF. TODO: Add the ability for a user to set their own basis vectors
//
// INPUTS:
// + num_entries - the size of the input sparse matrix
// + iientries   - the indices of the input sparse matrix of size 2*num_entries
// + factors     - the weights of hte input sparse matrix of size num_entries
// + num_vec_dims - the number of vector components
// + src_vec_dims_undist_seqind - src vector component tensor dims (of size num_vec_dims)
// + dst_vec_dims_undist_seqind,- dst vector component tensor dims (of size num_vec_dims)
// + src_mesh - if not NULL and src_pl is NULL, then describes the src geometry
// + src_pl  -  if not NULL, then describes the src geometry
// + dst_mesh - if not NULL and src_pl is NULL, then describes the dst geometry
// + dst_pl  -  if not NULL, then describes the dst geometry
// OUTPUTS:
// + num_entries_vec - the size of the new vector sparse matrix
// + iientries_vec   - the indices of the new vector sparse matrix allocated internally, but of size 4*num_entries_vec
// + factors_vec     - the weigths of the new vector sparse matrix allocated internally, but of size num_entries_vec

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
    double src_coords[2];
    if (!srcCoordFromId.search(src_id, src_coords)) {
      Throw()<<"src id="<<src_id<<" not found in coordinate search.";
    }

    // printf("id=%d src_coords=%f %f %f\n",src_id,src_coords[0],src_coords[1],src_coords[2]);

    
    // Get dst coords, complain if not there
    double dst_coords[2];
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
      iientries_vec[iientries_vec_pos+1] = src_vec_dims_undist_seqind[0]; 
      iientries_vec[iientries_vec_pos+2] = dst_id;
      iientries_vec[iientries_vec_pos+3] = dst_vec_dims_undist_seqind[0]; 
      factors_vec[factor_vec_pos]=factor*vec_wgts[0];

      iientries_vec[iientries_vec_pos+4] = src_id;
      iientries_vec[iientries_vec_pos+5] = src_vec_dims_undist_seqind[1];
      iientries_vec[iientries_vec_pos+6] = dst_id;
      iientries_vec[iientries_vec_pos+7] = dst_vec_dims_undist_seqind[0];
      factors_vec[factor_vec_pos+1]=factor*vec_wgts[1];

      iientries_vec[iientries_vec_pos+8] = src_id;
      iientries_vec[iientries_vec_pos+9] = src_vec_dims_undist_seqind[0]; 
      iientries_vec[iientries_vec_pos+10] = dst_id;
      iientries_vec[iientries_vec_pos+11] = dst_vec_dims_undist_seqind[1];
      factors_vec[factor_vec_pos+2]=factor*vec_wgts[2];

      iientries_vec[iientries_vec_pos+12] = src_id;
      iientries_vec[iientries_vec_pos+13] = src_vec_dims_undist_seqind[1];
      iientries_vec[iientries_vec_pos+14] = dst_id;
      iientries_vec[iientries_vec_pos+15] = dst_vec_dims_undist_seqind[1];
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


