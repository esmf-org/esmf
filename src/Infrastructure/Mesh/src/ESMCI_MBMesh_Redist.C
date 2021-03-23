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

#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>
#include <Mesh/include/ESMCI_MBMesh_Redist.h>
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_SparseMsg.h>

#include <Mesh/include/ESMCI_MBMesh_Glue.h>

#include <limits>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>
#include <set>
#include <map>

#include <ESMCI_VM.h>
#include "ESMCI_LogErr.h"

#include "ESMCI_TraceMacros.h"  // for profiling

#include "MBTagConventions.hpp"
#include "moab/ParallelComm.hpp"
#include "moab/MeshTopoUtil.hpp"

// #define ESMF_REGRID_DEBUG_MAP_ANY

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------
          
namespace ESMCI {

///////////////////////// helper functions /////////////////////////////

  void create_mbmesh_copy_metadata(MBMesh *src_mesh, 
                                 MBMesh **_out_mesh);

  void create_mbmesh_redist_elem_move_nodes(MBMesh *src_mesh, 
                                            std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                                            MBMesh *out_mesh);
  void create_mbmesh_redist_elem_move_elems(MBMesh *src_mesh,
                                            std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                                            MBMesh *out_mesh);
  void create_pointlist_redist_move_points(PointList *src_pl,
                                          std::vector<PL_Comm_Pair> *point_to_proc_list,
                                          PointList **out_pl);

int calc_size_vert_comm(MBMesh *src_mesh);
void pack_node_comm(MBMesh *src_mesh, EntityHandle vert, char *buff);
void unpack_gid_node_comm(MBMesh *out_mesh, char *buff, int *gid);
void unpack_node_comm(MBMesh *out_mesh, char *buff,
                      std::vector<int> &node_ids,
                      std::vector<double> &node_coords,
                      std::vector<int> &node_mask_vals,
                      std::vector<int> &node_masks);
int calc_size_elem_comm(MBMesh *src_mesh, EntityHandle eh);
void pack_elem_comm(MBMesh *src_mesh, EntityHandle elem, char *buff);
int calc_size_from_buff_elem_comm(MBMesh *out_mesh, char *buff);
void unpack_gid_elem_comm(MBMesh *out_mesh, char *buff, int *_gid);
void unpack_elem_comm(MBMesh *out_mesh, char *buff, std::map<int,int> &node_gid_to_pos, 
                      std::vector<int> &elem_ids, std::vector<int> &elem_types, std::vector<int> &elem_conns,
                      std::vector<int> &elem_mask_vals, std::vector<int> &elem_masks, std::vector<double> &elem_areas, std::vector<double> &elem_coords);


// resetting owners after a redist
void mbmesh_set_elem_owners(MBMesh *mesh, DDir<> edir);
void mbmesh_set_elem_owners_wo_list(MBMesh *mesh);
void mbmesh_set_node_owners(MBMesh *mesh, DDir<> ndir);
void mbmesh_set_node_owners_wo_list(MBMesh *mesh);

// resetting orig_pos after a redist
void mbmesh_set_elem_orig_pos(MBMesh *mesh, int num_elem_gids, int *elem_gids);
void mbmesh_set_elem_orig_pos_wo_list(MBMesh *mesh);
void mbmesh_set_node_orig_pos(MBMesh *output_mesh, int num_node_gids, int *node_gids);
void mbmesh_set_node_orig_pos_wo_list(MBMesh *mesh);

// split element handling
void mbmesh_invert_split_to_orig_id_map(MBMesh *mesh, 
  std::multimap<int, EntityHandle> &orig_id_to_split_elem);
void mbmesh_add_other_split_elems(MBMesh *mesh, const int &gid, const int &proc,
                      std::multimap<int, EntityHandle> orig_id_to_split_elem,
                      std::vector<EH_Comm_Pair> &elem_to_proc_list);

// local helper functions

void mbmesh_initialize_edir(MBMesh *mesh, int *num_elem_gids, int *elem_gids, std::vector<UInt> &src_elem_gids_proc, DDir<> &edir);
void mbmesh_initialize_ndir(MBMesh *mesh, int *num_node_gids, int *node_gids, std::vector<UInt> &src_node_gids_proc, DDir<> &ndir);
void mbmesh_initialize_ddir(MBMesh *mesh, const Range &ents, int *num_gids, int *gids, std::vector<UInt> &src_gids_proc, DDir<> &ddir);
void mbmesh_initialize_elem_to_proc_list(MBMesh *mesh, const std::vector<UInt> &src_gids_proc, std::multimap<int, EntityHandle> &orig_id_to_split_elem, 
std::vector<EH_Comm_Pair> &elem_to_proc_list);
void mbmesh_expand_elem_to_proc_list(MBMesh *mesh, const std::vector<UInt> &src_gids_proc, std::multimap<int, EntityHandle> &orig_id_to_split_elem, 
std::vector<EH_Comm_Pair> &elem_to_proc_list);
void mbmesh_handle_unassigned_elements(MBMesh *mesh, std::multimap<int, EntityHandle> &orig_id_to_split_elem, 
std::vector<EH_Comm_Pair> &elem_to_proc_list);

#define MAX_VERT_COMM_SIZE 100
#define MAX_ELEM_COMM_SIZE 100

///////////////////////// helper functions /////////////////////////////

// This creates a copy of a mesh redisted according to the elem distribution described in elem_to_proc_list
// NOTE: this func. currently does not set vert owners or orig_ind, because they are not necessary for 
// rend. meshes. Evntually add this capability (with a flag to toggle it on and off).
void create_mbmesh_redist_elem(MBMesh *src_mesh, 
                               std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                               MBMesh **_out_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_elem()"
  try {
    // Get Parallel Information
    int localrc;
    MPI_Comm mpi_comm;
    mpi_comm=VM::getCurrent(&localrc)->getMpi_c();
    ESMC_CHECK_THROW(localrc);
  
    int num_proc = VM::getCurrent(&localrc)->getPetCount();
    ESMC_CHECK_THROW(localrc);
  
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

#ifdef DEBUG
    void *mbptr = (void *) src_mesh;
    int len = 12; char fname[len];
    sprintf(fname, "srcmesh_%dm", localPet);
    MBMesh_write(&mbptr, fname, &localrc, len);
#endif

#ifdef DEBUG
    // Debug print of elem to proc lcist
    for (int i=0; i<elem_to_proc_list->size(); i++) {
      EntityHandle eh=(*elem_to_proc_list)[i].eh;
      int proc=(*elem_to_proc_list)[i].proc;

      int gid=-55;
      MBMesh_get_gid(src_mesh, eh, &gid);

      printf("%d# elem gid=%d to proc=%d \n",localPet,gid,proc);
    }

    printf("%d# ======\n",localPet);
#endif

    // First func that creates copy of Mesh with same metadata
    MBMesh *out_mesh;
    create_mbmesh_copy_metadata(src_mesh, &out_mesh);

    // Redist nodes to new mesh
    std::map<int,int> node_gid_to_pos; 
    ESMCI_RENDEZVOUS_TRACE_ENTER("MBMesh rendezvous redist elements move verts")
    create_mbmesh_redist_elem_move_nodes(src_mesh, elem_to_proc_list, out_mesh);
    ESMCI_RENDEZVOUS_TRACE_EXIT("MBMesh rendezvous redist elements move verts")

    // Redist elems to new mesh
    ESMCI_RENDEZVOUS_TRACE_ENTER("MBMesh rendezvous redist elements move elems")
    create_mbmesh_redist_elem_move_elems(src_mesh, elem_to_proc_list, out_mesh);
    ESMCI_RENDEZVOUS_TRACE_EXIT("MBMesh rendezvous redist elements move elems")

    // Output new mesh
    *_out_mesh=out_mesh;

    }
    CATCH_MBMESH_RETHROW
}

void create_pointlist_redist_point(PointList *src_pl,
                                   std::vector<PL_Comm_Pair> *point_to_proc_list,
                                   PointList **_out_pl) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_point()"
  try {
    // Get Parallel Information
    int localrc;
    int num_proc = VM::getCurrent(&localrc)->getPetCount();
    ESMC_CHECK_THROW(localrc);

    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // Redist points to new pointlist
    create_pointlist_redist_move_points(src_pl, point_to_proc_list, _out_pl);
  }
  CATCH_MBMESH_RETHROW
}



//// TODO: Eventually move these to someplace global, so they can be used everywhere
//// that way as things change they only need to be updated in one place


// Create a new mbmesh that is a copy of src_mesh's metadata, 
// but not verts, elems, etc.
void create_mbmesh_copy_metadata(MBMesh *src_mesh, 
                                 MBMesh **_out_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_copy_metadata()"
  try {
    int merr;
    
    // New Mesh    
    MBMesh *out_mesh = new MBMesh(src_mesh->pdim,
                                  src_mesh->orig_sdim,
                                  src_mesh->coordsys);
        
    // Copy which fields are set up
    if (src_mesh->has_node_mask) {
      out_mesh->setup_node_mask();
    }

    if (src_mesh->has_elem_coords) {
      out_mesh->setup_elem_coords();
    }

    if (src_mesh->has_elem_mask) {
      out_mesh->setup_elem_mask();
    }

    if (src_mesh->has_elem_area) {
      out_mesh->setup_elem_area();
    }

    // Do output
    *_out_mesh=out_mesh;
  }
  CATCH_MBMESH_RETHROW
}


// Redist verts to new mesh
//  - Loop figuring out which verts go to where, use set to unique to each proc, pack and send
//  - on other side unpack and create map of gid to vert, use this when unpacking and creating elems
void create_mbmesh_redist_elem_move_nodes(MBMesh *src_mesh, 
                                            std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                                            MBMesh *out_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_elem_move_nodes()"
  try {
    int localrc, merr;

    // Get Parallel Information
    int num_proc = VM::getCurrent(&localrc)->getPetCount();
    ESMC_CHECK_THROW(localrc);
    
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);
    
    ESMCI_RENDEZVOUS_TRACE_ENTER("MBMesh rendezvous redist elements move verts setup comm pattern")

    //// Get unique list of verts going to each proc ////
    // allocate set list
    std::set<int> *set_of_gids_per_proc=NULL;
    set_of_gids_per_proc=new std::set<int>[num_proc];
    
    //// Also create a gid to vert map
    std::map<int,EntityHandle> gid_to_vert;
    
    // Loop adding elem gids to sets
    for (int i=0; i<elem_to_proc_list->size(); i++) {
       EntityHandle eh=(*elem_to_proc_list)[i].eh;
      int proc=(*elem_to_proc_list)[i].proc;
      
      // Get verts
      int num_verts;
      const EntityHandle *verts;
      merr=src_mesh->mesh->get_connectivity(eh,verts,num_verts); // NEED TO PASS IN corners_only = true???
      ESMC_CHECK_MOAB_THROW(merr);
   
      // Loop verts getting gids and inserting into sets
      for (int v=0; v<num_verts; v++) {
        int gid;
        
        // Get gid of vert
        MBMesh_get_gid(src_mesh, verts[v], &gid);
        
        // Insert into set
        set_of_gids_per_proc[proc].insert(gid);

        // Insert into map
        gid_to_vert[gid]=verts[v];
      }   
    }
    
    // Count number of non-empty sets
    int num_nonempty=0;
    for (int p=0; p<num_proc; p++) {
      if (!set_of_gids_per_proc[p].empty()) num_nonempty++;
    }


    // Allocate pattern for sparse communication
    UInt *nonempty_pets=NULL;
    UInt *nonempty_sizes=NULL;
    if (num_nonempty > 0) {
      nonempty_pets=new  UInt[num_nonempty];
      nonempty_sizes=new UInt[num_nonempty];
    }

    // Fill communication pattern arrays
     int size_per_vert=calc_size_vert_comm(src_mesh); // constant per vert
    int j=0;
    for (int p=0; p<num_proc; p++) {
      if (!set_of_gids_per_proc[p].empty()) {
        nonempty_pets[j]=(UInt) p;
        nonempty_sizes[j]=(UInt)(set_of_gids_per_proc[p].size()*size_per_vert);
        j++;
       }
    }

    // Create communication structure
    SparseMsg comm;

    // Set patterns and sizes
    comm.setPattern(num_nonempty, nonempty_pets);
    comm.setSizes(nonempty_sizes);

    // Deallocate arrays for communication pattern
    if (num_nonempty > 0) {
      if (nonempty_pets != NULL) delete [] nonempty_pets;
      if (nonempty_sizes != NULL) delete [] nonempty_sizes;
    }

    // Reset buffers
    comm.resetBuffers();

    // Loop through sets, packing buffers
    for (int p=0; p<num_proc; p++) {
      if (!set_of_gids_per_proc[p].empty()) {

        // Get buffer
          SparseMsg:: buffer *b=comm.getSendBuffer(p);

        // Get iterators to set
        std::set<int>::const_iterator si=set_of_gids_per_proc[p].begin();
        std::set<int>::const_iterator se=set_of_gids_per_proc[p].end();

        // Loop through set
        for (; si != se; ++si) {
          int gid=*si;
          // printf("%d# vert gid=%d to proc=%d \n",localPet,gid,p);

          // Get entity handle from gid
          std::map<int,EntityHandle>::const_iterator gehi =  gid_to_vert.find(gid);
          if (gehi == gid_to_vert.end()) {
            Throw() << " Gid not found in map!";
          }

          // Get EntityHandle
          EntityHandle vert=gehi->second;

          // Pack vert to send
          char buff[MAX_VERT_COMM_SIZE];
          pack_node_comm(src_mesh, vert, buff);

          // Put into send buffer
          b->push((UChar *)buff, (UInt)size_per_vert);
        }
      }
    }

    // Deallocate list of sets
    if (set_of_gids_per_proc != NULL) delete [] set_of_gids_per_proc;

    ESMCI_RENDEZVOUS_TRACE_EXIT("MBMesh rendezvous redist elements move verts setup comm pattern")

    ESMCI_RENDEZVOUS_TRACE_ENTER("MBMesh rendezvous redist elements move verts communication")
    // Communicate verts
    comm.communicate();
    ESMCI_RENDEZVOUS_TRACE_EXIT("MBMesh rendezvous redist elements move verts communication");

    // Count max number of nodes for use in reserving space in vectors
    int max_num_nodes = 0;
    for (std::vector<UInt>::const_iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);
      
      while (!b->empty()) {
        char buff[MAX_VERT_COMM_SIZE];
        
        // Get one verts info out of buffer
        b->pop((UChar *)buff, (UInt)size_per_vert);
        
        // Count node  
        max_num_nodes++;
      }
    }

    // Reset buffers to allow them to be used again below
    // for the actual unpacking
    comm.resetBuffers();


    // Vectors to hold node information as it's being unpacked
    std::vector<int> node_ids;
    std::vector<double> node_coords;
    std::vector<int> node_orig_pos;
    std::vector<int> node_mask_vals;
    std::vector<int> node_masks;

    // Reserve space in vectors to reduce reallocs
    node_ids.reserve(max_num_nodes);
    node_coords.reserve(out_mesh->orig_sdim*max_num_nodes);
    node_orig_pos.reserve(max_num_nodes);
    
    if (out_mesh->has_node_mask) {
      node_mask_vals.reserve(max_num_nodes);
      node_masks.reserve(max_num_nodes);
    }


    ESMCI_RENDEZVOUS_TRACE_ENTER("MBMesh rendezvous redist elements move verts create verts");
    // Go through received buffers and create verts
    { // Block so set new_node_gids goes away when done

      // Track new nodes so we don't create more than one with same gid
      std::set<int> new_node_gids;  

      // Go through received buffers and unpack new node info
      int pos=0;
      for (std::vector<UInt>::const_iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
        UInt proc = *p;
        SparseMsg::buffer *b = comm.getRecvBuffer(proc);
        
        while (!b->empty()) {
          char buff[MAX_VERT_COMM_SIZE];
          
          // Get one verts info out of buffer
          b->pop((UChar *)buff, (UInt)size_per_vert);
          
          // Unpack gid
          int gid;
          unpack_gid_node_comm(out_mesh, buff, &gid);
          
          // Check to see if the node already exists, if not then unpack and add
          std::set<int>::const_iterator nngi =  new_node_gids.find(gid);
          if (nngi == new_node_gids.end()) {

            // Unpack node
            unpack_node_comm(out_mesh, buff, 
                             node_ids, 
                             node_coords,
                             node_mask_vals,
                             node_masks);

            // Set a default orig_pos to imply an ordering that will be used
            // in elem creation. It's fine if this changes later as long
            // as it doesn't change when it's needed for elem creation. 
            node_orig_pos.push_back(pos);
            
            // next pos
            pos++;

            // Add to set
            new_node_gids.insert(gid);          
          }
        }
      }

    } // Block so set new_node_gids goes away when done


    // Create node_owners and set default (-1)
    std::vector<int> node_owners(node_ids.size(),-1);


    // Create nodes all at once
    MBMesh_add_nodes_in_a_group(out_mesh, 
                                node_ids,
                                node_coords,
                                node_orig_pos,
                                node_owners,
                                node_mask_vals,
                                node_masks);

    // Done creating nodes, so finalize
    out_mesh->finalize_nodes();

    ESMCI_RENDEZVOUS_TRACE_EXIT("MBMesh rendezvous redist elements move verts create verts");
  }
  CATCH_MBMESH_RETHROW
}

// Redist elems to new mesh
void create_mbmesh_redist_elem_move_elems(MBMesh *src_mesh, 
                                            std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                                            MBMesh *out_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_elem_move_elems()"
  try {
    int localrc, merr;

    // Get Parallel Information
    int num_proc = VM::getCurrent(&localrc)->getPetCount();
    ESMC_CHECK_THROW(localrc);
     
    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    ESMCI_RENDEZVOUS_TRACE_ENTER("MBMesh rendezvous redist elements move elems setup comm pattern")

    // Allocate size array
    UInt *size_per_proc=NULL;
    size_per_proc=new UInt[num_proc];

    // Init to zero
    for (int i=0; i<num_proc; i++) {
      size_per_proc[i]=0;
    }

    // Calc. Size going to each proc
    for (int i=0; i<elem_to_proc_list->size(); i++) {
      EntityHandle eh=(*elem_to_proc_list)[i].eh;
      int proc=(*elem_to_proc_list)[i].proc;

      size_per_proc[proc] += calc_size_elem_comm(src_mesh, eh);
    }

#if 0
    for (int i=0; i<num_proc; i++) {
      printf("%d# size going to %d = %d\n",localPet,i,size_per_proc[i]);
    }
#endif
    
    // Count number of non-empty procs
    int num_nonempty=0;
    for (int p=0; p<num_proc; p++) {
      if (size_per_proc[p]>0) num_nonempty++;
    }


    // Allocate pattern for sparse communication
    UInt *nonempty_pets=NULL;
    UInt *nonempty_sizes=NULL;
    if (num_nonempty > 0) {
      nonempty_pets=new  UInt[num_nonempty];
      nonempty_sizes=new UInt[num_nonempty];
     }

    // Fill communication pattern arrays
    int j=0;
    for (int p=0; p<num_proc; p++) {
      if (size_per_proc[p]>0) {
        nonempty_pets[j]=(UInt) p;
        nonempty_sizes[j]=(UInt)(size_per_proc[p]);
        j++;
       }
    }
    
    // Deallocate size_per_proc
    if (size_per_proc != NULL) delete [] size_per_proc;

    // Create communication structure
    SparseMsg comm;

    // Set patterns and sizes
    comm.setPattern(num_nonempty, nonempty_pets);
    comm.setSizes(nonempty_sizes);

    // Deallocate arrays for communication pattern
    if (num_nonempty > 0) {
      if (nonempty_pets != NULL) delete [] nonempty_pets;
      if (nonempty_sizes != NULL) delete [] nonempty_sizes;
    }

    // Reset buffers
    comm.resetBuffers();

    // Loop through elem distribution list packing buffers
    for (int i=0; i<elem_to_proc_list->size(); i++) {
      EntityHandle elem=(*elem_to_proc_list)[i].eh;
      int proc=(*elem_to_proc_list)[i].proc;

      // Get buffer
      SparseMsg:: buffer *b=comm.getSendBuffer(proc);

      // Pack vert to send
      char buff[MAX_ELEM_COMM_SIZE];
      pack_elem_comm(src_mesh, elem, buff);

      // Calc size
      int elem_comm_size=calc_size_elem_comm(src_mesh, elem);

      // Put into send buffer
      b->push((UChar *)buff, (UInt)elem_comm_size);
    }
    ESMCI_RENDEZVOUS_TRACE_EXIT("MBMesh rendezvous redist elements move elems setup comm pattern")

    ESMCI_RENDEZVOUS_TRACE_ENTER("MBMesh rendezvous redist elements move elems communication")
    // Communicate elems
    comm.communicate();
    ESMCI_RENDEZVOUS_TRACE_EXIT("MBMesh rendezvous redist elements move elems communication");

    ESMCI_RENDEZVOUS_TRACE_ENTER("MBMesh rendezvous redist elements move elems create elems");

    // Count max number of elems for use in reserving space in vectors
    int max_num_elems=0;
    for (std::vector<UInt>::const_iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);
        
      while (!b->empty()) {
        char buff[MAX_ELEM_COMM_SIZE];
        
        // Look at buffer to figure out what size we need to pop
        int elem_size=calc_size_from_buff_elem_comm(out_mesh, (char *)(b->get_current()));
        
        // Get one elem's info out of buffer
        b->pop((UChar *)buff, (UInt)elem_size);
          
        // Count elems
        max_num_elems++;
      }
    }

    // Reset buffers to allow them to be used again below
    // for the actual unpacking
    comm.resetBuffers();


    // Vectors in which to store element creation info
    std::vector<int> elem_ids;
    std::vector<int> elem_types;
    std::vector<int> elem_orig_pos;
    std::vector<int> elem_masks;
    std::vector<int> elem_mask_vals;
    std::vector<double> elem_areas;
    std::vector<double> elem_coords;
    std::vector<int> elem_conns;

    // Reserve space in vectors to reduce reallocs
    elem_ids.reserve(max_num_elems);
    elem_types.reserve(max_num_elems);
    elem_orig_pos.reserve(max_num_elems);

    if (out_mesh->has_elem_mask) {
      elem_masks.reserve(max_num_elems);
      elem_mask_vals.reserve(max_num_elems);
    }

    if (out_mesh->has_elem_area) {
      elem_areas.reserve(max_num_elems);
    } 

    if (out_mesh->has_elem_coords) {
      elem_coords.reserve(out_mesh->orig_sdim*max_num_elems);
    }

    if (out_mesh->pdim == 2) {
      elem_conns.reserve(4*max_num_elems); // Quad max for 2D
    } else {
      elem_conns.reserve(8*max_num_elems); // Hex max for 3D 
    }

    // Get map of node gids to pos in array to use in elem connection generation
    // (block so orig_nodes vector goes away)
    std::map<int,int> node_gid_to_pos; 
    { 
      // Get sorted list of orig_nodes
      std::vector<EntityHandle> orig_nodes;
      out_mesh->get_sorted_orig_nodes(orig_nodes);
      
      // Loop through list mapping gid to pos in list
      for (auto i=0; i<orig_nodes.size(); i++) {
        int gid=out_mesh->get_gid(orig_nodes[i]);
        node_gid_to_pos[gid]=i;
      }
    }

    // Go through received buffers and create elems
    // Block so new_elems_gids set goes away when done
    {
      // Track new elems so we don't create more than one with same gid
      std::set<int> new_elem_gids; 
      
      // Go through received buffers and unpack new elem info
      for (std::vector<UInt>::const_iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
        UInt proc = *p;
        SparseMsg::buffer *b = comm.getRecvBuffer(proc);
        
        while (!b->empty()) {
          char buff[MAX_ELEM_COMM_SIZE];
          
          // Look at buffer to figure out what size we need to pop
          int elem_size=calc_size_from_buff_elem_comm(out_mesh, (char *)(b->get_current()));
          
          // Get one elem's info out of buffer
          b->pop((UChar *)buff, (UInt)elem_size);
          
          // Unpack gid
          int gid;
          unpack_gid_elem_comm(out_mesh, buff, &gid);
          
          // Check to see if the elem already exists, if not then unpack and add
          std::set<int>::const_iterator negi =  new_elem_gids.find(gid);
          if (negi == new_elem_gids.end()) {
            
            // Unpack elem info
            unpack_elem_comm(out_mesh, buff, node_gid_to_pos, 
                             elem_ids, elem_types, elem_conns, 
                             elem_mask_vals, elem_masks, 
                             elem_areas, elem_coords);
            
            // Add to set
            new_elem_gids.insert(gid);          
          }
        }
      }

    }

    // Fill elem_orig_pos with default value
    // TODO: change the MBMesh_add_elems_.. call to allow orig_pos to be optional
    for (auto i=0; i<elem_ids.size(); i++) {
      elem_orig_pos.push_back(-3);
    }

    // Create new elems from info in lists
    MBMesh_add_elems_in_groups_by_type(out_mesh,
                                       -1, // Set owner to default, 
                                       elem_ids,
                                       elem_types,
                                       elem_orig_pos,
                                       elem_mask_vals,
                                       elem_masks,
                                       elem_areas,
                                       elem_coords,
                                       elem_conns);

    // Done creating elems, so finalize
    out_mesh->finalize_elems();

    ESMCI_RENDEZVOUS_TRACE_EXIT("MBMesh rendezvous redist elements move elems create elems")
  }
  CATCH_MBMESH_RETHROW
}

// Redist elems to new mesh
void create_pointlist_redist_move_points(PointList *pl,
                                         std::vector<PL_Comm_Pair> *point_to_proc_list,
                                         PointList **out_pl) {
#undef  ESMC_METHOD
#define ESMC_METHOD "create_mbmesh_redist_pointlist_move_points()"
  try {
    int localrc, merr;

    // Get Parallel Information
    int petCount = VM::getCurrent(&localrc)->getPetCount();
    ESMC_CHECK_THROW(localrc);

    int localPet = VM::getCurrent(&localrc)->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    SparseMsg comm;

    std::vector<int> mymoving(pl->get_curr_num_pts(),0);

    // construct a per proc list of location in pl to be sent to procs
    //      also mark which points are moving
    std::vector<int> proc_counts;
    proc_counts.resize(petCount,0);
    std::vector< std::vector<int> > idx_list; // TODO: optimize sparse vector
    idx_list.resize(petCount);

    int num_snd_pts = 0;
    std::vector<PL_Comm_Pair>::const_iterator pb = point_to_proc_list->begin();
    std::vector<PL_Comm_Pair>::const_iterator pe = point_to_proc_list->end();
    for (pb; pb !=  pe; ++pb) {
        num_snd_pts++;
        proc_counts[pb->proc]++;
        idx_list[pb->proc].push_back(pb->loc);
        mymoving[pb->loc] = 1;
#ifdef ESMF_REGRID_DEBUG_MAP_ANY
        printf("%d# LOOP1: Node %d send by [%d]\n", Par::Rank(), pb->loc, pb->proc);
#endif
    }

    // construct lists used to set up the communication pattern
    int sdim = pl->get_coord_dim();
    int snd_size=(sdim+1)*sizeof(double); // buffer item size
    int num_snd_procs=0;
    std::vector<int> snd_pets;
    std::vector<int> snd_sizes;
    std::vector<int> snd_counts;
    std::vector< std::vector<int> > idx_list2;
    for (int i=0; i<petCount; i++) {
      if (proc_counts[i] > 0) {
        num_snd_procs++;
        snd_pets.push_back(i);
        snd_sizes.push_back(proc_counts[i]*snd_size);
        snd_counts.push_back(proc_counts[i]);
        idx_list2.push_back(idx_list[i]); // dense version of idx_list
#ifdef ESMF_REGRID_DEBUG_MAP_ANY
      printf("%d# LOOP2: Proc %d to send nodes [", Par::Rank(), i);
      for (int j = 0 ; j < idx_list[i].size(); ++j)
        printf("%d, ", idx_list[i].at(j));
      printf("]\n");
#endif
      }
    }

    // Setup pattern and sizes
    if (num_snd_procs >0) {
      comm.setPattern(num_snd_procs, (const UInt *)&(snd_pets[0]));
      comm.setSizes((UInt *)&(snd_sizes[0]));
    } else {
      comm.setPattern(0, (const UInt *)NULL);
      comm.setSizes((UInt *)NULL);
    }

    // Reset buffers
    comm.resetBuffers();

    // Pack points into buffers
    for (int i=0; i< num_snd_procs; i++) {
      SparseMsg:: buffer *b=comm.getSendBuffer(snd_pets[i]);
      for (int j=0; j<snd_counts[i]; j++) {


        // Get index of node
        int loc=idx_list2[i][j];

        // coordinates and gid of point
        const double *pnt = pl->get_coord_ptr(loc);
        int this_id = pl->get_id(loc);

        // pack buf
        double buf[4]; // 4 is biggest this should be (i.e. 3D+id)
        buf[0]=pnt[0];
        buf[1]=pnt[1];
        if (sdim < 3)
          buf[2]=this_id;  //passing an int through a double...inefficient but ok?
        else {
          buf[2]=pnt[2];
          buf[3]=this_id;  //passing an int through a double...inefficient but ok?
        }

        // Push buf onto send struct
        b->push((const UChar *)buf, (UInt)snd_size);
      }
    }

    // Communicate point information
    comm.communicate();

    int num_pts = 0;
    for (std::vector<UInt>::const_iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
      UInt proc = *p;
      SparseMsg::buffer *b = comm.getRecvBuffer(proc);

      // Unpack everything from this processor
      while (!b->empty()) {
        double buf[4]; // 4 is biggest this should be (i.e. 3D+dist)

        b->pop((UChar *)buf, (UInt)snd_size);
        num_pts++;
      }
    }
    comm.resetBuffers();

    int pl_rend_size=pl->get_curr_num_pts() - num_snd_pts + num_pts;
    // int pl_rend_size=num_pts;
#ifdef DEBUG
    printf("PET %d - create_pointlist_redist_move_points: pl_rend_size (%d) = pl->size (%d)\n", localPet, pl_rend_size, pl->get_curr_num_pts());
#endif

    // Create source rendezvous point list, start with points that are not being sent to other processors
    PointList *pl_rend;
    pl_rend = new ESMCI::PointList(pl_rend_size,sdim);

    if (pl_rend_size >= 0) {

      int orig_dstpointlist_size = pl->get_curr_num_pts();
      for (int i=0; i<orig_dstpointlist_size; i++) {
        if (mymoving[i] == 0) {
          int temp_id=pl->get_id(i);
          double *temp_pnt = (double *)pl->get_coord_ptr(i);
          pl_rend->add(temp_id,temp_pnt);
        }
      }

      int ip=0;
      for (std::vector<UInt>::const_iterator p = comm.inProc_begin(); p != comm.inProc_end(); ++p) {
        UInt proc = *p;
        SparseMsg::buffer *b = comm.getRecvBuffer(proc);

        // Unpack everything from this processor
        while (!b->empty()) {
          double buf[4]; // 4 is biggest this should be (i.e. 3D+dist)

          b->pop((UChar *)buf, (UInt)snd_size);
          //      printf(" [%f %f %f], ",pnt[0],pnt[1],pnt[2]);


          // Unpack buf
          double pnt[3]={0.0,0.0,0.0};
          int loc_id;

          pnt[0]=buf[0];
          pnt[1]=buf[1];
          if (sdim < 3) {
            loc_id=(int)buf[2];
          } else {
            pnt[2]=buf[2];
            loc_id=(int)buf[3];
          }

          pl_rend->add(loc_id,pnt);
        }

        ip++;
      }
    }
    // pointer magic
    *out_pl = pl_rend;
  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_redist_elem(MBMesh *mesh, int *num_elem_gids, int *elem_gids, MBMesh **out_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_redist_elem()"
  try {

    // vector of element processor pairs
    std::vector<EH_Comm_Pair> elem_to_proc_list;
    std::vector<UInt> src_elem_gids_proc;
    DDir<> edir;

    // for split element handling
    std::multimap<int, EntityHandle> orig_id_to_split_elem;
    mbmesh_invert_split_to_orig_id_map(mesh, orig_id_to_split_elem);

    // find the processor owners of each element
    mbmesh_initialize_edir(mesh, num_elem_gids, elem_gids, src_elem_gids_proc, edir);

    // initialize the elem_to_proc_list
    mbmesh_initialize_elem_to_proc_list(mesh, src_elem_gids_proc, orig_id_to_split_elem, elem_to_proc_list);

    // move the elements
    create_mbmesh_redist_elem(mesh, &elem_to_proc_list, out_mesh);

    // reset the owners
    mbmesh_set_elem_owners(*out_mesh, edir);
    mbmesh_set_node_owners_wo_list(*out_mesh);

    // reset the orig_pos
    mbmesh_set_elem_orig_pos(*out_mesh, *num_elem_gids, elem_gids);
    mbmesh_set_node_orig_pos_wo_list(*out_mesh);

    // Finalize nodes and elems
    (*out_mesh)->finalize_nodes();
    (*out_mesh)->finalize_elems();
  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_redist_node(MBMesh *mesh, int *num_node_gids, int *node_gids, MBMesh **out_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_redist_node()"
  try {

    // vector of element processor pairs
    std::vector<EH_Comm_Pair> elem_to_proc_list;
    std::vector<UInt> src_node_gids_proc;
    DDir<> ndir;

    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh split id preprocessing");
    // for split element handling
    std::multimap<int, EntityHandle> orig_id_to_split_elem;
    mbmesh_invert_split_to_orig_id_map(mesh, orig_id_to_split_elem);
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh split id preprocessing");

    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh ddir initialization");
    // find the processor owners of each node
    mbmesh_initialize_ndir(mesh, num_node_gids, node_gids, src_node_gids_proc, ndir);
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh ddir initialization");

    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh ddir processing");
    // expand the elem_to_proc_list to include nodal distgrid information
    mbmesh_expand_elem_to_proc_list(mesh, src_node_gids_proc, orig_id_to_split_elem, elem_to_proc_list);

    // loop through elements looking for ones that haven't been assigned
    //   assign them to the processor of one of their neighbors if possible
    mbmesh_handle_unassigned_elements(mesh, orig_id_to_split_elem, elem_to_proc_list);
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh ddir processing");

    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh element communication");
    // move the elements
    create_mbmesh_redist_elem(mesh, &elem_to_proc_list, out_mesh);
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh element communication");

    ESMCI_MESHREDIST_TRACE_ENTER("MBMesh post processing");
    // reset the owners
    mbmesh_set_node_owners(*out_mesh, ndir);
    mbmesh_set_elem_owners_wo_list(*out_mesh);

    // reset the orig_pos
    mbmesh_set_node_orig_pos(*out_mesh, *num_node_gids, node_gids);
    mbmesh_set_elem_orig_pos_wo_list(*out_mesh);
    ESMCI_MESHREDIST_TRACE_EXIT("MBMesh post processing");

    // Finalize nodes and elems
    (*out_mesh)->finalize_nodes();
    (*out_mesh)->finalize_elems();
  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_redist(MBMesh *mesh, int *num_node_gids, int *node_gids, int *num_elem_gids, int *elem_gids, MBMesh **out_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_redist()"
  try {

    // vector of element processor pairs
    std::vector<EH_Comm_Pair> elem_to_proc_list;
    std::vector<UInt> src_elem_gids_proc;
    std::vector<UInt> src_node_gids_proc;
    DDir<> edir;
    DDir<> ndir;

    // for split element handling
    std::multimap<int, EntityHandle> orig_id_to_split_elem;
    mbmesh_invert_split_to_orig_id_map(mesh, orig_id_to_split_elem);

    // find the processor owners of each element
    mbmesh_initialize_edir(mesh, num_elem_gids, elem_gids, src_elem_gids_proc, edir);

    // find the processor owners of each node
    mbmesh_initialize_ndir(mesh, num_node_gids, node_gids, src_node_gids_proc, ndir);

    // Initialize the elem_to_proc_list from the elemental distgrid information
    mbmesh_initialize_elem_to_proc_list(mesh, src_elem_gids_proc, orig_id_to_split_elem, elem_to_proc_list);

    // expand the elem_to_proc_list to include nodal distgrid information
    mbmesh_expand_elem_to_proc_list(mesh, src_node_gids_proc, orig_id_to_split_elem, elem_to_proc_list);

    // loop through elements looking for ones that haven't been assigned
    //   assign them to the processor of one of their neighbors if possible
    mbmesh_handle_unassigned_elements(mesh, orig_id_to_split_elem, elem_to_proc_list);

    // move the elements
    create_mbmesh_redist_elem(mesh, &elem_to_proc_list, out_mesh);

    // reset the owners
    mbmesh_set_node_owners(*out_mesh, ndir);
    mbmesh_set_elem_owners(*out_mesh, edir);

    // reset the orig_pos
    mbmesh_set_node_orig_pos(*out_mesh, *num_node_gids, node_gids);
    mbmesh_set_elem_orig_pos(*out_mesh, *num_elem_gids, elem_gids);

    // Finalize nodes and elems
    (*out_mesh)->finalize_nodes();
    (*out_mesh)->finalize_elems();
  }
  CATCH_MBMESH_RETHROW
}


//////////////////////////////////////  HELPER ROUTINES  ///////////////////////////////////////////////

int calc_size_vert_comm(MBMesh *src_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_size_vert_comm()"    
  try {
    int size=0;
    // Gid
    size += sizeof(int);

    //// These will make no sense on a new proc, so don't send them
    //  // orig_pos
    //  size += sizeof(int);
     //
    //  // owner
    //  size += sizeof(int);

    // node coords
    // (Only pack orig. version of coords, because the others will be generated from those.) 
    size += sizeof(double)*src_mesh->orig_sdim;

    // node mask info
    if (src_mesh->has_node_mask) {
      size += sizeof(int); // node_mask
      size += sizeof(int); // node_mask_val
    }
    // output size
    return size;
  }
  CATCH_MBMESH_RETHROW
}


void pack_node_comm(MBMesh *src_mesh, EntityHandle node, char *buff) {
#undef  ESMC_METHOD
#define ESMC_METHOD "pack_node_comm()"
  try {
    // MOAB Error RC
    int merr;

    // Offset
    int off=0;

    // Pack gid
    int gid=src_mesh->get_gid(node);
    *((int *)(buff+off))=gid;
    off +=sizeof(int);
    

    ///// Don't send orig_pos and owner, since they will make no sense on
    ///// a new processor

    // Pack coords
    // (Only pack orig. version of coords, because the others will be generated from those.) 
    double noc[3];
    src_mesh->get_node_orig_coords(node, noc);
    
    int orig_sdim=src_mesh->orig_sdim;
    for (int i=0; i<orig_sdim; ++i) {
      *((double *)(buff+off))=noc[i];
      off +=sizeof(double);
    }

    if (src_mesh->has_node_mask) {
      // Pack mask
      int masked=src_mesh->get_node_mask(node);
      *((int *)(buff+off))=masked;
      off +=sizeof(int);

      // Pack number of mask val
      int mask_val=src_mesh->get_node_mask_val(node);
      *((int *)(buff+off))=mask_val;
      off +=sizeof(int);
    }
  }
  CATCH_MBMESH_RETHROW
}

void unpack_gid_node_comm(MBMesh *out_mesh, char *buff, int *gid) {
  // Unpack gid
  *gid=*((int *)(buff));
}


void unpack_node_comm(MBMesh *out_mesh, char *buff,
                      std::vector<int> &node_ids,
                      std::vector<double> &node_coords,
                      std::vector<int> &node_mask_vals,
                      std::vector<int> &node_masks) {
#undef  ESMC_METHOD
#define ESMC_METHOD "unpack_node_comm()"
  try {
    int localrc, merr;

    // Offset
    int off=0;

    // Unpack gid
    int gid;
    gid=*((int *)(buff+off));
    off +=sizeof(int);
    node_ids.push_back(gid);

    ///// Don't recv orig_pos and owner, since they will make no sense on
    ///// a new processor

    // Unpack coords
    // (Only orig. version of coords, because the others will be generated from those.) 
    int orig_sdim=out_mesh->orig_sdim;
    for (int i=0; i<orig_sdim; i++) {
      double crd=*((double *)(buff+off));
      off +=sizeof(double);
      node_coords.push_back(crd);
    }

    if (out_mesh->has_node_mask) {
      int masked;
      masked=*((int *)(buff+off));
      off +=sizeof(int);
      node_masks.push_back(masked);

      int mask_val;
      mask_val=*((int *)(buff+off));
      off +=sizeof(int);
      node_mask_vals.push_back(mask_val);
    }
  }
  CATCH_MBMESH_RETHROW
}

#if 0
  // OLD ONE


void unpack_node_comm(MBMesh *out_mesh, char *buff,  EntityHandle *_new_vert) {
#undef  ESMC_METHOD
#define ESMC_METHOD "unpack_node_comm()"
  try {
    int localrc, merr;

    // Offset
    int off=0;

    // Unpack gid
    int gid;
    gid=*((int *)(buff+off));
    off +=sizeof(int);
    

    ///// Don't recv orig_pos and owner, since they will make no sense on
    ///// a new processor

    // Unpack coords
    double coords[3]={0.0,0.0,0.0};
    int sdim=out_mesh->sdim;
    for (int i=0; i<sdim; i++) {
      coords[i]=*((double *)(buff+off));
      off +=sizeof(double);
    }

    //    printf("RECV: gid=%d coords=[%f %f]\n",gid,c[0],c[1]);

    // Create new vertex
    // NEED TO SWITDH THESE TO USE ACCESSOR METHODS EVENTUALLY, BUT 
    // I WANT TO THINK A BIT ABOUT HOW TO STRUCTURE THE NEW ONES THAT WILL 
    // BE NECESSARY
    EntityHandle new_vert;
    merr=out_mesh->mesh->create_vertex(coords, new_vert);
    ESMC_CHECK_MOAB_THROW(merr);

    // Set gid
    merr=out_mesh->mesh->tag_set_data(out_mesh->gid_tag, &new_vert, 1, &gid);
    ESMC_CHECK_MOAB_THROW(merr);

    if (out_mesh->has_node_orig_coords) {
      double noc[3]={0.0,0.0,0.0};
      for (int i=0; i<out_mesh->sdim; ++i) {
        noc[i]=*((double *)(buff+off));
        off +=sizeof(double);
      }
      
      merr=out_mesh->mesh->tag_set_data(out_mesh->node_orig_coords_tag, &new_vert, 1, noc);
      ESMC_CHECK_MOAB_THROW(merr);
    }

    if (out_mesh->has_node_mask) {
      int masked;
      masked=*((int *)(buff+off));
      off +=sizeof(int);
      out_mesh->set_node_mask(new_vert, masked);

      int mask_val;
      mask_val=*((int *)(buff+off));
      off +=sizeof(int);
      out_mesh->set_node_mask_val(new_vert, mask_val);
    }

    // Output vertex
    *_new_vert=new_vert;
  }
  CATCH_MBMESH_RETHROW
}
#endif

int calc_size_elem_comm(MBMesh *src_mesh, EntityHandle eh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_size_elem_comm()"
  try {
    int merr;
    int size=0;

    // Gid
    size += sizeof(int);

    ///// DON'T PACK THESE BECAUSE THEY WON'T MAKE SENSE ON THE NEW PROC
    // // orig_pos
    // size += sizeof(int);
    //
    // // owner
    // size += sizeof(int);

    // number of verts
    size += sizeof(int);

    // num verts
    int num_verts;
    const EntityHandle *verts;
    // TODO: do we need to pass in corners_only = true?
    merr=src_mesh->mesh->get_connectivity(eh,verts,num_verts);
    ESMC_CHECK_MOAB_THROW(merr);

    // number of nodes
    size += num_verts*sizeof(int);

    // ADD OTHER THINGS HERE AS ADDED TO ELEM

    // elem coords
    // (Only pack orig. version of coords, because the others will be generated from those.) 
    if (src_mesh->has_elem_coords) {
      size += sizeof(double)*src_mesh->orig_sdim;
    }

    // elem mask
    if (src_mesh->has_elem_mask) {
      size += sizeof(int); // elem mask
      size += sizeof(int); // elem mask_val
    }

    // elem area
    if (src_mesh->has_elem_area) {
      size += sizeof(double);
    }

    // output size
    return size;
  }
  CATCH_MBMESH_RETHROW
}

void pack_elem_comm(MBMesh *src_mesh, EntityHandle elem, char *buff) {
#undef  ESMC_METHOD
#define ESMC_METHOD "pack_elem_comm()"
  try {
    int merr;

    // Offset
    int off=0;
 
    // Pack gid
    int gid=src_mesh->get_gid(elem);
    *((int *)(buff+off))=gid;
    off +=sizeof(int);
    
    ///// Don't send orig_pos and owner, since they will make no sense on
    ///// a new processor

     // Get number of verts and vert list 
    int num_verts;
    const EntityHandle *verts;
    src_mesh->get_elem_corner_nodes(elem, num_verts, verts);

    // Pack number of verts
    *((int *)(buff+off))=num_verts;
    off +=sizeof(int);

    // Pack node data
    for (int v=0; v<num_verts; v++) {

      // Get gid of vert
      int vert_gid=src_mesh->get_gid(verts[v]);

      // Pack into buffer
      *((int *)(buff+off))=vert_gid;
      off +=sizeof(int);
    }

    // Pack elem coord data
    // (Only pack orig. version of coords, because the others will be generated from those.) 
    if (src_mesh->has_elem_coords) {
      // Get orig. coords
      double eoc[3];
      src_mesh->get_elem_orig_coords(elem, eoc);
      
      // Pack
      for (int i=0; i<src_mesh->orig_sdim; ++i) {
        *((double *)(buff+off))=eoc[i];
        off +=sizeof(double);
      }
    }

    // (Only pack mask field (not mask_val), since that's the only one needed for rend)
    if (src_mesh->has_elem_mask) {
      // Elem mask
      int masked=src_mesh->get_elem_mask(elem);      
      *((int *)(buff+off))=masked;
      off +=sizeof(int);

      // Elem mask val
      int mask_val=src_mesh->get_elem_mask_val(elem);      
      *((int *)(buff+off))=mask_val;
      off +=sizeof(int);
    }

    if (src_mesh->has_elem_area) {
      double area=src_mesh->get_elem_area(elem);
      *((double *)(buff+off))=area;
      off +=sizeof(double);
    }

  }
  CATCH_MBMESH_RETHROW
}

int calc_size_from_buff_elem_comm(MBMesh *out_mesh, char *buff) {
#undef  ESMC_METHOD
#define ESMC_METHOD "calc_size_from_buff_elem_comm()"
  try {
    int size=0;

    // Offset
    int off=0;

    // Size of gid, plus offset past gid
    size += sizeof(int);
    off +=sizeof(int);
    
    // Get number of verts
    int num_verts= *((int *)(buff+off));

    // Size to hold number of verts
    size +=sizeof(int);

    // Size to hold an integer gid for each vert
    size += num_verts*sizeof(int);

    // elem coords
    // (Only pack orig. version of coords, because the others will be generated from those.)     
    if (out_mesh->has_elem_coords) {
      size += sizeof(double)*out_mesh->orig_sdim;
    }

    if (out_mesh->has_elem_mask) {
      size += sizeof(int); // elem mask
      size += sizeof(int); // elem mask_val
    }

    if (out_mesh->has_elem_area) {
      size += sizeof(double);
    }

    // return size
    return size;
  }
  CATCH_MBMESH_RETHROW
}

void unpack_gid_elem_comm(MBMesh *out_mesh, char *buff, int *_gid) {
  // Unpack gid
  *_gid=*((int *)(buff));
}

void unpack_elem_comm(MBMesh *out_mesh, char *buff, std::map<int,int> &node_gid_to_pos, 
                      std::vector<int> &elem_ids, std::vector<int> &elem_types, std::vector<int> &elem_conns,
                      std::vector<int> &elem_mask_vals, std::vector<int> &elem_masks, std::vector<double> &elem_areas, std::vector<double> &elem_coords) {
#undef  ESMC_METHOD
#define ESMC_METHOD "unpack_elem_comm()"

  try {
    int localrc, merr;

    // Offset
    int off=0;

    // Unpack gid
    int gid;
    gid=*((int *)(buff+off));
    off +=sizeof(int);
    elem_ids.push_back(gid);

    ///// Don't send orig_pos and owner, since they will make no sense on
    ///// a new processor

    // Unpack num_nodes
    int num_nodes;
    num_nodes=*((int *)(buff+off));
    off +=sizeof(int);
    int esmf_etype=MBMesh_num_nodes_to_esmf_etype(out_mesh->pdim, num_nodes);
    elem_types.push_back(esmf_etype);

    // Loop unpacking each node gid and converting to pos
    for(int n=0; n<num_nodes; n++) {
      int node_gid=*((int *)(buff+off));
      off +=sizeof(int);

      // Get pos from gid
      std::map<int,int>::const_iterator ngtpi =  node_gid_to_pos.find(node_gid);
      if (ngtpi == node_gid_to_pos.end()) {
        Throw() << "node gid not found in map!";
       }

      // Add to connection list (make 1-based)
      elem_conns.push_back(ngtpi->second+1);
    }

    // Elem cart coords
    // (Only orig. version of coords were packed, because the others will be generated from those.)
    if (out_mesh->has_elem_coords) {

      // Unpack coord and add to list
      for (int i=0; i<out_mesh->orig_sdim; ++i) {
        double crd=*((double *)(buff+off));
        off +=sizeof(double);
 
        elem_coords.push_back(crd);
      }
    }

    // Elem mask
    if (out_mesh->has_elem_mask) {
      // Unpack/set elem mask
      int masked=*((int *)(buff+off));
      off +=sizeof(int);

      // Add to elem mask list
      elem_masks.push_back(masked);

      // Unpack/set elem mask val
      int mask_val=*((int *)(buff+off));
      off +=sizeof(int);

      // Add to elem mask val. list
      elem_mask_vals.push_back(mask_val);
    }

    // Elem area
    if (out_mesh->has_elem_area) {
      // Unpack/set elem area
      double area=*((double *)(buff+off));
      off +=sizeof(double);

      // Add to elem mask val. list
      elem_areas.push_back(area);
    }
  }
  CATCH_MBMESH_RETHROW
}


#if 0 

  // OLD VERSION OF ABOVE
void unpack_elem_comm(MBMesh *out_mesh, char *buff, std::map<int,EntityHandle> *out_gid_to_vert, EntityHandle *_new_elem) {
#undef  ESMC_METHOD
#define ESMC_METHOD "unpack_elem_comm()"

  try {
    int localrc, merr;

    // Offset
    int off=0;

    // Unpack gid
    int gid;
    gid=*((int *)(buff+off));
    off +=sizeof(int);

    ///// Don't send orig_pos and owner, since they will make no sense on
    ///// a new processor

    // Unpack num_verts
    int num_verts;;
    num_verts=*((int *)(buff+off));
    off +=sizeof(int);

    // Define the maximum number of verts and error check
#define MAX_ELEM_VERTS 20
    if (num_verts >MAX_ELEM_VERTS) {
      Throw () << "- element contains more nodes than are currently supported ";
    }

    // Connectivity array
    EntityHandle verts[MAX_ELEM_VERTS];
#undef MAX_ELEM_VERTS

    // Loop unpacking each vert gid and creating list of verts
    for(int v=0; v<num_verts; v++) {
      int vert_gid=*((int *)(buff+off));
      off +=sizeof(int);

      // Get vert handle from gid
      std::map<int,EntityHandle>::const_iterator ogtvi =  out_gid_to_vert->find(vert_gid);
      if (ogtvi == out_gid_to_vert->end()) {
        Throw() << "vertex gid not found in map!";
       }

      // Get vert EntityHandle
      verts[v]=ogtvi->second;
    }

    // Get entity type of element
    EntityType etype=MBMesh_get_entity_type(out_mesh->pdim, num_verts);

    // Create new element
    // NEED TO SWITDH THESE TO USE ACCESSOR METHODS EVENTUALLY, BUT 
    // I WANT TO THINK A BIT ABOUT HOW TO STRUCTURE THE NEW METHODS THAT WILL 
    // BE NECESSARY
    EntityHandle new_elem;
    merr=out_mesh->mesh->create_element(etype,verts,num_verts,new_elem);
    ESMC_CHECK_MOAB_THROW(merr); 

    // Set global id
    merr=out_mesh->mesh->tag_set_data(out_mesh->gid_tag, &new_elem, 1, &gid);
    ESMC_CHECK_MOAB_THROW(merr);

    if (out_mesh->has_elem_coords) {
      double ec[3]={0.0,0.0,0.0};
      for (int i=0; i<out_mesh->sdim; ++i) {
        ec[i]=*((double *)(buff+off));
        off +=sizeof(double);
      }

      merr=out_mesh->mesh->tag_set_data(out_mesh->elem_coords_tag, &new_elem, 1, ec);
      ESMC_CHECK_MOAB_THROW(merr);
    }

    if (out_mesh->has_elem_orig_coords) {
      double eoc[3]={0.0,0.0,0.0};
      for (int i=0; i<out_mesh->orig_sdim; ++i) {
        eoc[i]=*((double *)(buff+off));
        off +=sizeof(double);
      }

      merr=out_mesh->mesh->tag_set_data(out_mesh->elem_orig_coords_tag, &new_elem, 1, eoc);
      ESMC_CHECK_MOAB_THROW(merr);
    }

    if (out_mesh->has_elem_mask) {
      // Unpack/set elem mask
      int masked=*((int *)(buff+off));
      off +=sizeof(int);
      out_mesh->set_elem_mask(new_elem, masked);      

      // Unpack/set elem mask val
      int mask_val=*((int *)(buff+off));
      off +=sizeof(int);
      out_mesh->set_elem_mask_val(new_elem, mask_val);      
    }

    if (out_mesh->has_elem_area) {
      // Unpack/set elem area
      double area=*((double *)(buff+off));
      off +=sizeof(double);
      out_mesh->set_elem_area(new_elem, area);      
    }

    // Output elem
    *_new_elem=new_elem;
  }
  CATCH_MBMESH_RETHROW
}
#endif

void mbmesh_invert_split_to_orig_id_map(MBMesh *mesh, 
  std::multimap<int, EntityHandle> &orig_id_to_split_elem) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_invert_split_to_orig_id_map()"
  try {

    int localrc, merr;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    Range elems;

    merr=mesh->mesh->get_entities_by_dimension(0, mesh->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through elems
    Range::const_iterator ei = elems.begin(), ee = elems.end();
    for (; ei != ee; ++ei) {
      const EntityHandle elem = *ei;

      // Get element id
      int gid;
      MBMesh_get_gid(mesh, elem, &gid);

      // See if this is a split id
      std::map<int,double>::const_iterator sitf=mesh->split_id_to_frac.find(gid);
      if (sitf != mesh->split_id_to_frac.end()) {
        // Translate split id to original
        int orig_id;
        std::map<int,int>::const_iterator soi = mesh->split_to_orig_id.find(gid);
        if (soi == mesh->split_to_orig_id.end()) {
          orig_id=gid;
        } else {
          orig_id=soi->second;
        }

        // Add to multimap
        orig_id_to_split_elem.insert(std::pair<int,EntityHandle>(orig_id,elem));
      }
    }
  }
  CATCH_MBMESH_RETHROW
}

// For a split mesh add the other parts of a split element
void mbmesh_add_other_split_elems(MBMesh *mesh, const int &gid, const int &proc,
                      std::multimap<int, EntityHandle> orig_id_to_split_elem,
                      std::vector<EH_Comm_Pair> &elem_to_proc_list) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_add_other_split_elems()"
  try {

    int localrc, merr;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // If this is a split element
    std::map<int,double>::const_iterator sitf =  mesh->split_id_to_frac.find(gid);
    if (sitf != mesh->split_id_to_frac.end()) {
      // Translate split id to original
      int orig_id;
      std::map<int,int>::const_iterator soi = mesh->split_to_orig_id.find(gid);
      if (soi == mesh->split_to_orig_id.end()) {
        orig_id=gid;
      } else {
        orig_id=soi->second;
      }

      // Loop through and add other elements from original
      std::pair <std::multimap<int,EntityHandle>::const_iterator, std::multimap<int,EntityHandle>::const_iterator> ret;
      ret=orig_id_to_split_elem.equal_range(orig_id);
      for (std::multimap<int,EntityHandle>::const_iterator it=ret.first; it!=ret.second; ++it) {
        // Split elem
        const EntityHandle split_elem=it->second;

        int id;
        MBMesh_get_gid(mesh, split_elem, &id);

        // Only add if not the one that's been added before
        if (id != gid) {

          // add to elem_to_proc_list if not already present
          EH_Comm_Pair ecp(split_elem, id, proc);
          std::vector<EH_Comm_Pair>::const_iterator ehf = find(elem_to_proc_list.begin(), elem_to_proc_list.end(), ecp);
          if (ehf == elem_to_proc_list.end())
            elem_to_proc_list.push_back(ecp);
        }
      }
    }
  }
  CATCH_MBMESH_RETHROW
}

// Set the split_orig_id_map in a redisted mesh from the src mesh
void mbmesh_set_split_orig_id_map(MBMesh *src_mesh, MBMesh *output_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_set_split_orig_id_map()"
  try {

    int localrc, merr;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    Range elems, elems_out;
    UInt *gids_split=NULL;
    UInt *gids_orig=NULL;
    DDir<> id_map_dir;

    merr=src_mesh->mesh->get_entities_by_dimension(0, src_mesh->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    // Get number of elements
    int num_src_gids=elems.size();
    
    // Get list of split and orig element gids
    if (num_src_gids>0) {
    
      // Allocate space
      gids_split=new UInt[num_src_gids];
      gids_orig=new UInt[num_src_gids];
    
      // Loop through list putting into arrays
      int pos=0;
    
      // Loop through elems
      Range::const_iterator ei = elems.begin(), ee = elems.end();
      for (; ei != ee; ++ei) {
        const EntityHandle elem = *ei;

        int owner;
        merr=src_mesh->mesh->tag_get_data(src_mesh->owner_tag, &elem, 1, &owner);
        ESMC_CHECK_MOAB_THROW(merr);

        // only consider local nodes
        if (owner != localPet) continue;

        // Get element id
        int split_eid;
        MBMesh_get_gid(src_mesh, elem, &split_eid);

        // See if this is a split id
        std::map<int,int>::const_iterator soi=src_mesh->split_to_orig_id.find(split_eid);
    
        // If this is a split set it to the original, otherwise just set it to the elem id
        UInt orig_eid;
        if (soi==src_mesh->split_to_orig_id.end()) {
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
    id_map_dir.Create(num_src_gids,gids_orig,gids_split);
    
    // Clean up
    if (num_src_gids>0) {
      if (gids_split!= NULL) delete [] gids_split;
      if (gids_orig != NULL) delete [] gids_orig;
    }


    // output_mesh
    merr=output_mesh->mesh->get_entities_by_dimension(0, output_mesh->pdim, elems_out);
    ESMC_CHECK_MOAB_THROW(merr);

    int num_out_gids=elems_out.size();
    
    // Copy input array to UInt
    UInt *elem_gids_u=NULL;
    if (num_out_gids>0) {
      elem_gids_u= new UInt[num_out_gids];
    }
    
    // Loop through and collect output_mesh element ids
    int om_pos=0;
    // Loop through elems
    Range::const_iterator ei = elems_out.begin(), ee = elems_out.end();
    for (; ei != ee; ++ei) {
      const EntityHandle elem = *ei;

      // Get element id
      int eid;
      MBMesh_get_gid(output_mesh, elem, &eid);

      elem_gids_u[om_pos]=eid;
      om_pos++;
    }
    
    // Do a look up of the input ids
    std::vector<DDir<>::dentry> lookups;
    id_map_dir.RemoteGID(num_out_gids, elem_gids_u, lookups);
    
    // Don't need anymore so clean up
    if (num_out_gids>0) {
      if (elem_gids_u != NULL) delete [] elem_gids_u;
    }
    
    // Loop through lookups and generate new list
    for (int i=0; i<lookups.size(); i++) {
    
      // If split put into map
      if (lookups[i].gid != lookups[i].origin_lid) {
        output_mesh->split_to_orig_id[lookups[i].origin_lid]=lookups[i].gid;
      }
    }
  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_expand_split_elem_ids(MBMesh *mesh, int num_elem_gids, int *elem_gids, int *_num_elem_gids_ws, int **_elem_gids_ws, std::map<int,int> &split_to_orig_id) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_expand_split_elem_ids()"
  try {
    int localrc, merr;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

#undef debug_printelemgids
#ifdef debug_printelemgids
    for (int i=0; i<num_elem_gids; ++i)
      printf("%d# elem gids %d\n", localPet, elem_gids[i]);
#endif

    // OPTIMIZATION
    // Copy input array to UInt
    UInt *elem_gids_u=NULL;
    if (num_elem_gids>0) {
      elem_gids_u= new UInt[num_elem_gids];

      for (int i=0; i<num_elem_gids; i++) {
        elem_gids_u[i]=elem_gids[i];
      }
    }

    // Get number of elements
    Range elems;
    merr=mesh->mesh->get_entities_by_dimension(0, mesh->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);
    
    int num_gids=elems.size();

    // Get list of split and orig element gids
    UInt *gids_split=NULL;
    UInt *gids_orig=NULL;
    if (num_gids>0) {

      // Allocate space
      gids_split= new UInt[num_gids];
      gids_orig= new UInt[num_gids];

      // Loop through list putting into arrays
      int pos=0;
      Range::const_iterator si = elems.begin(), se = elems.end();
      for (; si != se; ++si) {
        const EntityHandle elem = *si;

        int elem_owner;
        merr=mesh->mesh->tag_get_data(mesh->owner_tag, &elem, 1, &elem_owner);
        ESMC_CHECK_MOAB_THROW(merr);

        // Only do local
        if (elem_owner != localPet) continue;

        // Get element id
        int split_eid;
        MBMesh_get_gid(mesh, elem, &split_eid);

        // See if this is a split id
        std::map<int,int>::const_iterator soi=mesh->split_to_orig_id.find(split_eid);

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

        ++pos;
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

#ifdef debug_printelemgids
    for (int i=0; i<num_elem_gids_ws; ++i)
      printf("%d# elem gids_ws %d\n", localPet, elem_gids_ws[i]);
#endif

    // Output
    *_num_elem_gids_ws=num_elem_gids_ws;
    *_elem_gids_ws=elem_gids_ws;
  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_calc_split_id_to_frac(MBMesh *mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_calc_split_id_to_frac()"
  try {
    int localrc, merr;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // Declare polygon information
#define  MAX_NUM_POLY_COORDS  60
#define  MAX_NUM_POLY_NODES_2D  30  // MAX_NUM_POLY_COORDS/2
#define  MAX_NUM_POLY_NODES_3D  20  // MAX_NUM_POLY_COORDS/3
    int num_poly_nodes;
    double poly_coords[MAX_NUM_POLY_COORDS];

    // Get useful info
    int sdim=mesh->sdim;

    // Setup map to hold total areas
    std::map<UInt,double> orig_id_to_area;

    // Loop gathering split,orig id pairs
    std::vector<UInt> split_ids;
    std::vector<UInt> orig_ids;

    std::map<int,int>::const_iterator mi=mesh->split_to_orig_id.begin();
    std::map<int,int>::const_iterator me=mesh->split_to_orig_id.end();
    for ( ; mi != me; mi++) {
      // Get split element id
      split_ids.push_back(mi->first);

      // Get original element id
      orig_ids.push_back(mi->second);

      // Get uniqued list of original ids and set to zero
      orig_id_to_area[mi->second]=0.0;
    }

    // also add orig_id to orig_id pairs that are part of each split polygon
    std::map<UInt,double>::const_iterator otai=orig_id_to_area.begin();
    std::map<UInt,double>::const_iterator otae=orig_id_to_area.end();
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

      Range elems;
      merr=mesh->mesh->get_entities_by_dimension(0, mesh->pdim, elems);
      ESMC_CHECK_MOAB_THROW(merr);

      Range::const_iterator si = elems.begin(), se = elems.end();
      for (; si != se; ++si) {
        const EntityHandle elem = *si;
        int elem_id;
        MBMesh_get_gid(mesh, elem, &elem_id);

        if (s_id == elem_id) break;
      }
      if (si == se)
        Throw() << "Element not in mesh";

      // Get the element
      const EntityHandle &elem = *si;

      // Compute area depending on dimensions
      double area;
      if (sdim==2) {
        MBMesh_get_elem_coords(mesh, elem, MAX_NUM_POLY_NODES_2D, &num_poly_nodes, poly_coords);
        // get_elem_coords_2D_ccw(&elem, cfield, MAX_NUM_POLY_NODES_2D, tmp_coords, &num_poly_nodes, poly_coords);
        remove_0len_edges2D(&num_poly_nodes, poly_coords);
        area=area_of_flat_2D_polygon(num_poly_nodes, poly_coords);
      } else if (sdim==3) {
        MBMesh_get_elem_coords(mesh, elem, MAX_NUM_POLY_NODES_3D, &num_poly_nodes, poly_coords);
        // MBMesh_get_elem_coords_3D_ccw(mesh, elem, MAX_NUM_POLY_NODES_3D, tmp_coords, &num_poly_nodes, poly_coords);
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
      std::map<UInt,double>::const_iterator oi = orig_id_to_area.find(o_id);
      if (oi == orig_id_to_area.end()) {
        Throw() << "Original id not found in map! \n";
      }
      double total_area=oi->second;


      // Find entry to split id
      std::map<int,double>::iterator si = mesh->split_id_to_frac.find(s_id);
      if (si == mesh->split_id_to_frac.end()) {
        Throw() << "Split id not found in map! \n";
      }

      // Divide to get fraction
      si->second=si->second/total_area;
    }
  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_initialize_edir(MBMesh *mesh, int *num_elem_gids, int *elem_gids, std::vector<UInt> &src_elem_gids_proc, DDir<> &edir) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_initialize_edir()"
  try {
    Range elems;

    // get mesh elements and gids
    int merr=mesh->mesh->get_entities_by_dimension(0,mesh->pdim,elems);
    ESMC_CHECK_MOAB_THROW(merr);
    
    mbmesh_initialize_ddir(mesh, elems, num_elem_gids, elem_gids, src_elem_gids_proc, edir);

#undef debug_printedir
#ifdef debug_printedir
    int localrc;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // print the vectors
    printf("%d# src_elem_gids_proc [%d] = [", localPet, src_elem_gids_proc.size());
    for (int i=0; i<src_elem_gids_proc.size(); ++i) {
      printf("%d, ", src_elem_gids_proc[i]);
    }
    printf("]\n");
#endif

  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_initialize_ndir(MBMesh *mesh, int *num_node_gids, int *node_gids, std::vector<UInt> &src_node_gids_proc, DDir<> &ndir) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_initialize_ndir()"
  try {
    Range nodes;

    int merr=mesh->mesh->get_entities_by_dimension(0,0,nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    mbmesh_initialize_ddir(mesh, nodes, num_node_gids, node_gids, src_node_gids_proc, ndir);

#undef debug_printndir
#ifdef debug_printndir
    int localrc;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // print the vectors
    printf("%d# src_node_gids_proc [%d] = [", localPet, src_node_gids_proc.size());
    for (int i=0; i<src_node_gids_proc.size(); ++i) {
      printf("%d, ", src_node_gids_proc[i]);
    }
    printf("]\n");
#endif

  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_initialize_ddir(MBMesh *mesh, const Range &ents, int *num_gids, int *gids, std::vector<UInt> &src_gids_proc, DDir<> &ddir) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_initialize_ddir()"
  try {
    int localrc, merr;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    // distributed directory to determine element destinations
    std::vector<UInt> e_lids(*num_gids, 0);
    std::vector<UInt> e_gids(*num_gids, 0);
    
    // load the distributed directory
    for (int i=0; i<*num_gids; i++) {
      e_lids[i]=i;
      if (gids[i]>=0) {
        e_gids[i]=gids[i];
      } else {
        e_gids[i]=0;
      }
    }

    if (*num_gids) {
      ddir.Create(*num_gids, &e_gids[0], &e_lids[0]);
    } else {
      ddir.Create(0, (UInt*) NULL, (UInt *)NULL);
    }

    // list of the Mesh elem gids
    std::vector<UInt> src_gids;
    src_gids.reserve(ents.size());

    // Loop through objects getting ids
    Range::const_iterator si = ents.begin(), se = ents.end();
    for (; si != se; ++si) {
      const EntityHandle ent = *si;
      int gid;
      MBMesh_get_gid(mesh, ent, &gid);
      src_gids.push_back(gid);
    }

    // vectors of element destinations
    UInt num_src_gids=src_gids.size();
    std::vector<UInt> src_gids_lids(num_src_gids, 0);
    src_gids_proc.assign(num_src_gids, 0);


    // Get element destinations
    if (num_src_gids) {
      ddir.RemoteGID(num_src_gids, &src_gids[0], &src_gids_proc[0], &src_gids_lids[0]);
    } else {
      ddir.RemoteGID(0, (UInt *)NULL, (UInt *)NULL, (UInt *)NULL);
    }

#undef debug_printddir
#ifdef debug_printddir
    printf("%d# num_gids     %d\n", localPet, *num_gids);
    printf("%d# num_src_gids %d\n", localPet, num_src_gids);

    // print the vectors
    printf("%d# src_gids [%d]      = [", localPet, src_gids.size());
    for (int i=0; i<src_gids.size(); ++i) {
      printf("%d, ", src_gids[i]);
    }
    printf("]\n");
    printf("%d# src_gids_proc [%d] = [", localPet, src_gids_proc.size());
    for (int i=0; i<src_gids_proc.size(); ++i) {
      printf("%d, ", src_gids_proc[i]);
    }
    printf("]\n");
    printf("%d# src_gids_lids [%d] = [", localPet, src_gids_lids.size());
    for (int i=0; i<src_gids_lids.size(); ++i) {
      printf("%d, ", src_gids_lids[i]);
    }
    printf("]\n");
#endif

  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_initialize_elem_to_proc_list(MBMesh *mesh, const std::vector<UInt> &src_gids_proc, std::multimap<int, EntityHandle> &orig_id_to_split_elem, std::vector<EH_Comm_Pair> &elem_to_proc_list) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_initialize_elem_to_proc_list()"
  try {
    int localrc, merr;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    Range elems;
    merr=mesh->mesh->get_entities_by_dimension(0, mesh->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    Range::const_iterator si = elems.begin(), se = elems.end();
    int i = 0;
    for (; si != se; ++si) {
      const EntityHandle elem = *si;
      int elem_id;
      MBMesh_get_gid(mesh, elem, &elem_id);

      EH_Comm_Pair ecp(elem, elem_id, src_gids_proc[i]);
      std::vector<EH_Comm_Pair>::const_iterator ehf = find(elem_to_proc_list.begin(), elem_to_proc_list.end(), ecp);
      if (ehf == elem_to_proc_list.end())
        elem_to_proc_list.push_back(ecp);
      if (mesh->is_split)
        mbmesh_add_other_split_elems(mesh, elem_id, src_gids_proc[i],
          orig_id_to_split_elem, elem_to_proc_list);
      ++i;
    } // for si

#undef debug_printelemtoproclist
#ifdef debug_printelemtoproclist
    std::vector<EH_Comm_Pair>::const_iterator ehi = elem_to_proc_list.begin(), ehe = elem_to_proc_list.end();
    for (; ehi != ehe; ++ehi) {
      int id;
      MBMesh_get_gid(mesh, ehi->eh, &id);
      printf("%d# elem_to_proc_list - elem %d proc %d\n", localPet, id, ehi->proc);
    }
#endif

  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_expand_elem_to_proc_list(MBMesh *mesh, const std::vector<UInt> &src_gids_proc, std::multimap<int, EntityHandle> &orig_id_to_split_elem, std::vector<EH_Comm_Pair> &elem_to_proc_list) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_expand_elem_to_proc_list()"
  // expand the elem_to_proc_list to include nodal distgrid information
  try {
    int localrc, merr;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    Range nodes;
    merr=mesh->mesh->get_entities_by_dimension(0,0,nodes);
    ESMC_CHECK_MOAB_THROW(merr);
  
    Range::const_iterator si = nodes.begin(), se = nodes.end();
    int i = 0;
    for (; si != se; ++si) {
      const EntityHandle node = *si;
      int node_owner;
      merr=mesh->mesh->tag_get_data(mesh->owner_tag, &node, 1, &node_owner);
      ESMC_CHECK_MOAB_THROW(merr);

      // only consider local nodes
      if (node_owner != localPet) { ++i; continue;}

      // Loop elements attached to this node
      Range elems_on_node;
      merr=mesh->mesh->get_adjacencies(&node, 1, mesh->pdim, false, elems_on_node);
      ESMC_CHECK_MOAB_THROW(merr);

      Range::const_iterator eni = elems_on_node.begin(), ene = elems_on_node.end();
      for (; eni != ene; ++eni) {
        const EntityHandle elem_on_node = *eni;
        int elem_owner;
        merr=mesh->mesh->tag_get_data(mesh->owner_tag, &elem_on_node, 1, &elem_owner);
        ESMC_CHECK_MOAB_THROW(merr);

        // only consider local elements
        if (elem_owner != localPet) continue;

        int elem_id;
        MBMesh_get_gid(mesh, elem_on_node, &elem_id);

        // add to elem_to_proc_list if not already present
        EH_Comm_Pair ecp(elem_on_node, elem_id, src_gids_proc[i]);
        std::vector<EH_Comm_Pair>::const_iterator ehf = find(elem_to_proc_list.begin(), elem_to_proc_list.end(), ecp);
        if (ehf == elem_to_proc_list.end())
          elem_to_proc_list.push_back(ecp);
          // add the split elements
          if (mesh->is_split)
            mbmesh_add_other_split_elems(mesh, elem_id, src_gids_proc[i],
              orig_id_to_split_elem, elem_to_proc_list);
        else
          break;
      } // for eni
    ++i;
    } // for si

#undef debug_printnodelems
#ifdef debug_printnodelems
    merr=mesh->mesh->get_entities_by_dimension(0,0,nodes);
    ESMC_CHECK_MOAB_THROW(merr);
    
    Range::const_iterator si = nodes.begin(), se = nodes.end();
    for (; si != se; ++si) {
      const EntityHandle node = *si;
      int node_owner;
      merr=mesh->mesh->tag_get_data(mesh->owner_tag, &node, 1, &node_owner);
      ESMC_CHECK_MOAB_THROW(merr);

      if (node_owner != localPet) { ++i; continue;}

      int node_id;
      MBMesh_get_gid(mesh, node, &node_id);
      printf("%d# Node %d connects to Elements ", localPet, node_id);

      Range elems_on_node;
      merr=mesh->mesh->get_adjacencies(&node, 1, mesh->pdim, false, elems_on_node);
      ESMC_CHECK_MOAB_THROW(merr);

      Range::const_iterator eni = elems_on_node.begin(), ene = elems_on_node.end();
      for (; eni != ene; ++eni) {
        const EntityHandle elem_on_node = *eni;
        int elem_owner;
        merr=mesh->mesh->tag_get_data(mesh->owner_tag, &elem_on_node, 1, &elem_owner);
        ESMC_CHECK_MOAB_THROW(merr);

        if (elem_owner != localPet) continue;

        int elem_id;
        MBMesh_get_gid(mesh, elem_on_node, &elem_id);

        printf("%d, ", elem_id);
      }
      printf("\n");
    }
#endif

#undef debug_printelemtoproclist
#ifdef debug_printelemtoproclist
    std::vector<EH_Comm_Pair>::const_iterator ehi = elem_to_proc_list.begin(), ehe = elem_to_proc_list.end();
    for (; ehi != ehe; ++ehi) {
      int id;
      MBMesh_get_gid(mesh, ehi->eh, &id);
      printf("%d# elem_to_proc_list - elem %d proc %d\n", localPet, id, ehi->proc);
    }
#endif
  }
  CATCH_MBMESH_RETHROW
}

void mbmesh_handle_unassigned_elements(MBMesh *mesh, std::multimap<int, EntityHandle> &orig_id_to_split_elem, std::vector<EH_Comm_Pair> &elem_to_proc_list) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_handle_unassigned_elements()"
  // loop through elements looking for ones that haven't been assigned
  //   assign them to the processor of one of their neighbors if possible
  try {
    int localrc, merr;
    VM *vm = VM::getCurrent(&localrc);
    int petCount = vm->getPetCount();
    int localPet = vm->getLocalPet();
    ESMC_CHECK_THROW(localrc);

    bool found;
    int annointed_proc;
    Range elems;

    merr=mesh->mesh->get_entities_by_dimension(0, mesh->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    // Loop through elems
    Range::const_iterator ei = elems.begin(), ee = elems.end();
    for (; ei != ee; ++ei) {
      const EntityHandle elem = *ei;
      int elem_id;
      MBMesh_get_gid(mesh, elem, &elem_id);

#undef debug_missingelems
#ifdef debug_missingelems
printf("%d# elem %d\n", localPet, elem_id);
#endif
      // RLO: this loop resulted in O(n^2) behavior
      // // Loop through elem_to_proc_list
      // std::vector<EH_Comm_Pair>::const_iterator ehi = elem_to_proc_list.begin(), ehe = elem_to_proc_list.end();
      // for (; ehi != ehe; ++ehi) {
      //   int idtocheck = -1;
      //   MBMesh_get_gid(mesh, ehi->eh, &idtocheck);
      //   // if elem is in elem_to_proc_list it is already assigned
      //   if (elem_id == idtocheck) {
      //     break;
      //   }
      // }
      // 
      // // if elem is not in elem_to_proc_list, need to assign to a processor
      // if (ehi == ehe) {

      auto ehi = std::find_if(elem_to_proc_list.begin(), elem_to_proc_list.end(), 
                             [&elem_id](const EH_Comm_Pair& obj) {return obj.getID() == elem_id;});
      // if elem is already in elem_to_proc_list then we are good to go
      if  (ehi != elem_to_proc_list.end()) continue;
      else {

#ifdef debug_missingelems
printf("%d#   elem %d missing\n", localPet, elem_id);
#endif

        // find a neighboring element that is already assigned to a processor
        Range adj_elems;
        // WARNING: the following MOAB call only return the same element, no neighbors
        // merr=mesh->mesh->get_adjacencies(&elem, 1, mesh->pdim, false, adj_elems, Interface::UNION);
        MeshTopoUtil mt = MeshTopoUtil(mesh->mesh);
        merr=mt.get_bridge_adjacencies(elem, 0, mesh->pdim, adj_elems);
        ESMC_CHECK_MOAB_THROW(merr);
#ifdef debug_missingelems
printf("%d# looping %d adj_elems\n", localPet, adj_elems.size());
#endif
        // loop through the neighboring elements to see if any are already assigned to a processor
        found = false;
        annointed_proc = 0;
        Range::const_iterator nei = adj_elems.begin(), nee = adj_elems.end();
        for (; nei != nee; ++nei) {
          // if a processor has already been found, assign this element to it and break
          if (found) {

#ifdef debug_missingelems
printf("%d#   FOUND - setting elem comm pair (%d,%d)\n", localPet, elem_id, annointed_proc);
#endif

            EH_Comm_Pair ecp(elem, elem_id, annointed_proc);
            std::vector<EH_Comm_Pair>::const_iterator ehf = find(elem_to_proc_list.begin(), elem_to_proc_list.end(), ecp);
            if (ehf == elem_to_proc_list.end())
              elem_to_proc_list.push_back(ecp);

            // if split mesh, add split elements
            if (mesh->is_split)
              mbmesh_add_other_split_elems(mesh, elem_id, annointed_proc, 
                orig_id_to_split_elem, elem_to_proc_list);
            break;
          // if a processor has not yet been found, continue looking
          } else {

            const EntityHandle adj_elem = *nei;
            int adj_elem_id;
            MBMesh_get_gid(mesh, adj_elem, &adj_elem_id);

#ifdef debug_missingelems
printf("%d#   adj_elem_id %d\n", localPet, adj_elem_id);
#endif

            // Find a match in elem_to_proc_list for adj_elem_id
            auto ehi2 = std::find_if(elem_to_proc_list.begin(), elem_to_proc_list.end(), 
                                    [&adj_elem_id](const EH_Comm_Pair& obj) {return obj.getID() == adj_elem_id;});
            if  (ehi2 == elem_to_proc_list.end()) {
              Throw () << "Could not find a suitable processor for this element";
            } else {
              annointed_proc = ehi2->proc;
              found = true;
            }

// RLO: this loop resulted in O(n^2) behavior
//             // Loop through elem_to_proc_list looking for match to adj_elem_id
//             std::vector<EH_Comm_Pair>::const_iterator ehi2 = elem_to_proc_list.begin(), ehe2 = elem_to_proc_list.end();
//             for (; ehi2 != ehe2; ++ehi2) {
//               int idtocheck2 = -1;
//               MBMesh_get_gid(mesh, ehi2->eh, &idtocheck2);
//               // if adj_elem_id matches something in elem_to_proc_list, we have found a processor
// #ifdef debug_missingelems
// printf("%d#     checking adj_elem_id %d idtocheck2 %d\n", localPet, adj_elem_id, idtocheck2);
// #endif
// 
//               if (adj_elem_id == idtocheck2) {
// #ifdef debug_missingelems
// printf("%d#     match! adj_elem_id %d idtocheck2 %d\n", localPet, adj_elem_id, idtocheck2);
// #endif
// 
//                 annointed_proc = ehi2->proc;
//                 found = true;
//                 break;
//               }
//             } // ehi2
          } // else if not found
        } // nei
        // RLO: not sure if we should throw if no processor found or just assign to zero..
        if (nei == nee) {
          Throw () "Could not find a suitable processor for this element";
          // annointed_proc = 0;
          // EH_Comm_Pair ecp(elem, elem_id, 0);
          // std::vector<EH_Comm_Pair>::const_iterator ehf = find(elem_to_proc_list.begin(), elem_to_proc_list.end(), ecp);
          // if (ehf == elem_to_proc_list.end())
          //   elem_to_proc_list.push_back(ecp);
        }
      } // ehi == ehe
    } // ei
#undef debug_printelemtoproclist
#ifdef debug_printelemtoproclist
    std::vector<EH_Comm_Pair>::const_iterator ehi = elem_to_proc_list.begin(), ehe = elem_to_proc_list.end();
    for (; ehi != ehe; ++ehi) {
      int id;
      MBMesh_get_gid(mesh, ehi->eh, &id);
      printf("%d# elem_to_proc_list - elem %d proc %d\n", localPet, id, ehi->proc);
    }
#endif
  }
  CATCH_MBMESH_RETHROW
}

// Assign node owners in output_mesh using ndir
void mbmesh_set_node_owners(MBMesh *mesh, DDir<> ndir) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_set_node_owners()"
  try {
    int localrc, merr;

    // Get a list of the Mesh nodes with gids
    Range nodes;
    std::vector<UInt> gids;

    // get mesh nodes and gids
    merr=mesh->mesh->get_entities_by_dimension(0,0,nodes);
    ESMC_CHECK_MOAB_THROW(merr);
  
    gids.reserve(nodes.size());

    // Loop through objects getting ids
    Range::const_iterator si = nodes.begin(), se = nodes.end();
    for (; si != se; ++si) {
      const EntityHandle node = *si;
      int gid;
      MBMesh_get_gid(mesh, node, &gid);
      gids.push_back(gid);
    }

    // Get number of gids
    UInt num_src_gids=gids.size();
    std::vector<UInt> src_gids_proc(num_src_gids, 0);
    std::vector<UInt> src_gids_lids(num_src_gids, 0);

    // Get where each node is to go
    if (num_src_gids) {
      ndir.RemoteGID(num_src_gids, &gids[0], &src_gids_proc[0], &src_gids_lids[0]);
    } else {
      ndir.RemoteGID(0, (UInt *)NULL, (UInt *)NULL, (UInt *)NULL);
    }

    // Loop setting owner
    int nodeowners[nodes.size()];
    for (int i = 0; i < num_src_gids; ++i) {
      nodeowners[i] = src_gids_proc[i];

#undef print_nodeowners
#ifdef print_nodeowners
VM *vm = VM::getCurrent(&localrc);
int localPet = vm->getLocalPet();
printf("%d# node %d owner %d\n", localPet, gids[i], src_gids_proc[i]);
#endif
    }

    // Set Owners
    merr=mesh->mesh->tag_set_data(mesh->owner_tag, nodes, nodeowners);
    ESMC_CHECK_MOAB_THROW(merr);

  }
  CATCH_MBMESH_RETHROW
}


// Assign node owners in mesh without ndir
void mbmesh_set_node_owners_wo_list(MBMesh *mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_set_node_owners_wo_list()"
  try {
    int localrc, merr;

    // Get a list of the Mesh nodes with gids
    Range nodes;
    merr=mesh->mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    std::vector<UInt> gids;
    std::vector<UInt> lids; // Actually the number of associated elements
    std::vector<UInt> owner; // The owner proc
    owner.resize(nodes.size());

    Range::const_iterator si = nodes.begin(), se = nodes.end();
    for (; si != se; ++si) {
      const EntityHandle node = *si;
      int gid;
      MBMesh_get_gid(mesh, node, &gid);
      gids.push_back(gid);

      // Count the number of associated elems
      Range elems_on_node;
      merr=mesh->mesh->get_adjacencies(&node, 1, mesh->pdim, false, elems_on_node);
      ESMC_CHECK_MOAB_THROW(merr);

#undef debug__nodeowners
#ifdef debug__nodeowners
      VM *vm = VM::getCurrent(&localrc);
      int localPet = vm->getLocalPet();

      int node_owner;
      merr=mesh->mesh->tag_get_data(mesh->owner_tag, &node, 1, &node_owner);
      ESMC_CHECK_MOAB_THROW(merr);

      Range::const_iterator eoni = elems_on_node.begin(), eone =      elems_on_node.end();
      for (; eoni != eone; ++eoni) {
        int egid;
        const EntityHandle elem = *eoni;
        MBMesh_get_gid(mesh, elem, &egid);
        printf("%d# node %d owner %d adjacent element %d\n", localPet, gid,       node_owner, egid);
      }
#endif

      // Set the number of associated elements as the lids
      lids.push_back(elems_on_node.size());
    }

    // Create a distributed directory with the above information
    DDir<> dir;

    if (gids.size ()) {
      dir.Create(gids.size(), &gids[0], &lids[0]);
    } else {
      dir.Create(0, (UInt*) NULL, 0);
    }

#undef debug__gids
#ifdef debug__gids
    for (int i=0; i<gids.size(); ++i)
      printf("%d# gid %d lid %d\n", localPet, gids[i], lids[i]);
#endif

    std::vector<DDir<>::dentry> lookups;
    if (gids.size())
      dir.RemoteGID(gids.size(), &gids[0], lookups);
    else
      dir.RemoteGID(0, (UInt *) NULL, lookups);


    // Loop through the results.
    int curr_pos=0;
    UInt curr_gid=0;
    UInt curr_lid_best=0;
    UInt curr_proc_best=0;
    bool first_time=true;
    std::vector<DDir<>::dentry>::const_iterator ri = lookups.begin(), re = lookups.end();
    for (; ri != re; ++ri) {
      DDir<>::dentry dent = *ri;

      // Get info for this entry gid
      UInt gid=dent.gid;
      UInt lid=dent.origin_lid;
      UInt proc=dent.origin_proc;

#undef debug_lookupissue
#ifdef debug_lookupissue
      // Print out
      printf("%d# gid=%d lid=%d orig_proc=%d \n",Par::Rank(),gid,lid,proc);
#endif

      // first time
      if (first_time) {
        // If this doesn't match throw error
        if (gids[curr_pos] != gid) {
          printf("Error: first time gid[curr_pos]=%d  gid=%d\n",gids[curr_pos],gid);
  
          Throw() << " Error: gid "<<gid<<" missing from search list!";
        }
  
        // Set intial values
        curr_gid=gids[curr_pos];
        curr_lid_best=lid;
        curr_proc_best=proc;
  
        first_time=false;
      }
  
      // See if we're still looking at the same gid, if not move to   next
      if (curr_gid != gid) {
        // Set owner of gid before moving on
        owner[curr_pos]=curr_proc_best;
  
        // Move to next gid
        curr_pos++;
  
        // If this doesn't match throw error
        if (gids[curr_pos] != gid) {
          printf("Error: gid[curr_pos]=%dgid=%d\n",gids[curr_pos],gid);
  
          Throw() << " Error: gid "<<gid<<" missing from search list!";
        }
  
        // Get info
        curr_gid=gids[curr_pos];
        curr_lid_best=lid;
        curr_proc_best=proc;
      } else {
        // Still the same gid so see if the proc is better
        if (lid > curr_lid_best) {
          curr_lid_best=lid;
          curr_proc_best=proc;
        } else if (lid == curr_lid_best) {
          // Same lid, so chose the lowest proc
          if (proc < curr_proc_best) {
            curr_lid_best=lid;
            curr_proc_best=proc;
          }
        }
      }
    } // ri


    // Set owner of last gid before moving on
    // (could use gids.size() in if here also, but
    //  owner.size seemed clearer...)
    if (owner.size()) {
      owner[curr_pos]=curr_proc_best;
    }

    // printf("Last curr_pos=%d gids.size()=%d\n",curr_pos,gids.size());

    int nodeowners[owner.size()];
    for (int i = 0; i < owner.size(); ++i)
      nodeowners[i] = owner[i];

    // Set owners
    merr=mesh->mesh->tag_set_data(mesh->owner_tag, nodes, nodeowners);
    ESMC_CHECK_MOAB_THROW(merr);

  }
  CATCH_MBMESH_RETHROW
}


// Assign elem owners in output_mesh using edir
void mbmesh_set_elem_owners(MBMesh *mesh, DDir<> edir) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_set_elem_owners()"
  try {
    int localrc, merr;

    // Get a list of the Mesh nodes with gids
    Range elems;
    std::vector<UInt> gids;
    std::vector<EntityHandle> elemvec;

    // get mesh nodes and gids
    merr=mesh->mesh->get_entities_by_dimension(0, mesh->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);
  
    gids.reserve(elems.size());
    elemvec.reserve(elems.size());

    // Loop through objects getting ids
    Range::const_iterator si = elems.begin(), se = elems.end();
    for (; si != se; ++si) {
      const EntityHandle elem = *si;
      int gid;
      MBMesh_get_gid(mesh, elem, &gid);
      gids.push_back(gid);
      elemvec.push_back(elem);
    }

    // Get number of gids
    UInt num_src_gids=gids.size();
    std::vector<UInt> src_gids_proc(num_src_gids, 0);
    std::vector<UInt> src_gids_lids(num_src_gids, 0);

    // Get where each element is to go
    if (num_src_gids) {
      edir.RemoteGID(num_src_gids, &gids[0], &src_gids_proc[0], &src_gids_lids[0]);
    } else {
      edir.RemoteGID(0, (UInt *)NULL, (UInt *)NULL, (UInt *)NULL);
    }

    // Loop setting owner
    for (int i = 0; i < num_src_gids; ++i) {
      // Set Owners
      merr=mesh->mesh->tag_set_data(mesh->owner_tag, &elemvec[i], 1, &src_gids_proc[i]);
      ESMC_CHECK_MOAB_THROW(merr);
#undef print_elemowners
#ifdef print_elemowners
      VM *vm = VM::getCurrent(&localrc);
      int localPet = vm->getLocalPet();
      printf("%d# elem %d owner %d\n", localPet, gids[i], src_gids_proc[i]);
#endif
    }

  }
  CATCH_MBMESH_RETHROW
}


// Assign elem owners in output_mesh without edir
// WARNING: needs is_split set in output_mesh, and if is_split==true then needs
//          split_to_orig_id map to be correct in output_mesh.
void mbmesh_set_elem_owners_wo_list(MBMesh *mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_set_elem_owners_wo_list()"
  try {
    int localrc, merr;

    VM *vm = VM::getCurrent(&localrc);
    int localPet = vm->getLocalPet();
    if (localrc != ESMF_SUCCESS)
      Throw () << "Could not retrieve VM information.";

    // Count number of split and non-split elements
    int num_non_split=0;
    int num_split=0;

    Range elems;
    merr=mesh->mesh->get_entities_by_dimension(0, mesh->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    if (mesh->is_split) {
      // Loop through elems
      Range::const_iterator ei = elems.begin(), ee = elems.end();
      for (; ei != ee; ++ei) {
        const EntityHandle elem = *ei;

        // Get element id
        int eid;
        MBMesh_get_gid(mesh, elem, &eid);

        // If this is an elem created as the result of a split, then skip
        std::map<int,int>::const_iterator soi = mesh->split_to_orig_id.find(eid);
        if (soi == mesh->split_to_orig_id.end()) {
          num_non_split++;
        } else {
          num_split++;
        }
      }
    } else {
      num_non_split=elems.size();
    }


    // Get a list of the Mesh nodes with gids
    std::vector<UInt> gids;
    gids.resize(num_non_split,0);
    std::vector<UInt> lids; // Actually the number of associated elements
    lids.resize(num_non_split,0);
    std::vector<UInt> owner; // The owner proc
    owner.resize(num_non_split,0);
    std::vector<EntityHandle> elements;
    elements.resize(num_non_split);

    int i=0;
    Range::const_iterator ei = elems.begin(), ee = elems.end();
    for (; ei != ee; ++ei) {
      const EntityHandle elem = *ei;

      // Get element id
      int eid;
      MBMesh_get_gid(mesh, elem, &eid);

      // If this is an elem created as the result of a split, then skip
      if (mesh->is_split) {
        std::map<int,int>::const_iterator soi = mesh->split_to_orig_id.find(eid);
        if (soi != mesh->split_to_orig_id.end()) {
          continue;
        }
      }

      // Set GID
      gids[i]=eid;

      // Set elem
      elements[i]=elem;

      // Count the number of local nodes associated with elem
      int num_loc_nodes=0;

      // Loop elements attached to this node
      Range nodes_on_elem;
      merr=mesh->mesh->get_adjacencies(&elem, 1, 0, false, nodes_on_elem);
      ESMC_CHECK_MOAB_THROW(merr);

      Range::const_iterator nei = nodes_on_elem.begin(), nee = nodes_on_elem.end();
      for (; nei != nee; ++nei) {
        const EntityHandle node_on_elem = *nei;
        int node_owner;
        merr=mesh->mesh->tag_get_data(mesh->owner_tag, &node_on_elem, 1, &node_owner);
        ESMC_CHECK_MOAB_THROW(merr);

        // only consider local elements
        if (node_owner == localPet) ++num_loc_nodes;
      }

      // Set the number of associated local nodes as the lids
      lids[i]=num_loc_nodes;

      // Next thing in list
      ++i;
    }

    // Create a distributed directory with the above information
    DDir<> dir;

    if (gids.size ()) {
      dir.Create(gids.size(), &gids[0], &lids[0]);
    } else {
      dir.Create(0, (UInt*) NULL, 0);
    }


    // Lookup elem gids
    std::vector<DDir<>::dentry> lookups;
    if (gids.size())
      dir.RemoteGID(gids.size(), &gids[0], lookups);
    else
      dir.RemoteGID(0, (UInt *) NULL, lookups);


    // Loop through the results.
    int curr_pos=0;
    UInt curr_gid=0;
    UInt curr_lid_best=0;
    UInt curr_proc_best=0;
    bool first_time=true;
    std::vector<DDir<>::dentry>::const_iterator ri = lookups.begin(), re = lookups.end();
    for (; ri != re; ++ri) {
      DDir<>::dentry dent = *ri;

      // Get info for this entry gid
      UInt gid=dent.gid;
      UInt lid=dent.origin_lid;
      UInt proc=dent.origin_proc;

      // first time
      if (first_time) {
         // If this doesn't match throw error
         if (gids[curr_pos] != gid) {
           printf("Error: first time gid[curr_pos]=%d gid=%d\n",gids[curr_pos],gid);

           Throw() << " Error: gid "<<gid<<" missing from search list!";
         }

         // Set intial values
         curr_gid=gids[curr_pos];
         curr_lid_best=lid;
         curr_proc_best=proc;

         first_time=false;
      }


      // See if we're still looking at the same gid, if not move to  next
      if (curr_gid != gid) {
         // Set owner of gid before moving on
         owner[curr_pos]=curr_proc_best;

         // Move to next gid
         curr_pos++;

         // If this doesn't match throw error
         if (gids[curr_pos] != gid) {
           printf("Error: gid[curr_pos]=%d gid=%d\n",gids[curr_pos],gid);

           Throw() << " Error: gid "<<gid<<" missing from search list!";
         }

         // Get info
         curr_gid=gids[curr_pos];
         curr_lid_best=lid;
         curr_proc_best=proc;
      } else {
         // Still the same gid so see if the proc is better
         if (lid > curr_lid_best) {
           curr_lid_best=lid;
           curr_proc_best=proc;
         } else if (lid == curr_lid_best) {
           // Same lid, so chose the lowest proc
           if (proc < curr_proc_best) {
             curr_lid_best=lid;
             curr_proc_best=proc;
           }
         }
      }

       // Print out
       //  printf("%d# gid=%d lid=%d orig_proc=%d \n",Par::Rank(),gid,lid,proc);

    } // ri


    // Set owner of last gid before moving on
    // (could use gids.size() in if here also, but
    //  owner.size seemed clearer...)
    if (owner.size()) {
     owner[curr_pos]=curr_proc_best;
    }

    // printf("Last curr_pos=%d gids.size()=%d\n",curr_pos,gids.size());


    // Loop setting owner and OWNER_ID
    for (int i=0; i<gids.size(); i++) {
      const EntityHandle elem=elements[i];

      merr=mesh->mesh->tag_set_data(mesh->owner_tag, &elem, 1, &owner[i]);
      ESMC_CHECK_MOAB_THROW(merr);
    }


    // Do split elems
    if (mesh->is_split) {
      // Create list of split elems
      std::vector<EntityHandle> split_elems;
      split_elems.resize(num_split);
      // Fill list of split elems
      int pos=0;
      Range elems;
      merr=mesh->mesh->get_entities_by_dimension(0, mesh->pdim, elems);
      ESMC_CHECK_MOAB_THROW(merr);

      Range::const_iterator ei = elems.begin(), ee = elems.end();
      for (; ei != ee; ++ei) {
        const EntityHandle elem = *ei;
        // Get element id
        int eid;
        MBMesh_get_gid(mesh, elem, &eid); 
        // If this is an elem created as the result of a split, then skip
        std::map<int,int>::const_iterator soi = mesh->split_to_orig_id.find(eid);
        if (soi == mesh->split_to_orig_id.end()) {
          continue;
        } 
        // Set elem
        split_elems[pos]=elem; 
        // Next in list
        pos++;
      }

      // Loop setting owner and OWNER_ID
      for (int i=0; i<split_elems.size(); i++) {
        const EntityHandle elem =split_elems[i];

        // Get element id
        int eid;
        MBMesh_get_gid(mesh, elem, &eid); 

        // Get orig id
        std::map<int,int>::const_iterator soi = mesh->split_to_orig_id.find(eid);
        if (soi == mesh->split_to_orig_id.end()) {
          Throw() << " split element not found in split_to_orig_id map";
        }
        int orig_id=soi->second;

        // Get original element
        // TODO: there should be a more elegant way to do this...
        Range::const_iterator ei = elems.begin(), ee = elems.end();
        for (; ei != ee; ++ei) {
          const EntityHandle orig_elem = *ei;
          // Get element id
          int eid;
          MBMesh_get_gid(mesh, orig_elem, &eid);

          if (orig_id == eid) break;
        }

        // Get original element
        if (ei == elems.end()) {
          Throw() << " elem id not found in element map";
        }

        const EntityHandle orig_elem=*ei;

        // Split element owner is original elements owner
        int orig_owner;
        merr=mesh->mesh->tag_get_data(mesh->owner_tag, &orig_elem, 1, &orig_owner);
        ESMC_CHECK_MOAB_THROW(merr);

        // Set owner
        merr=mesh->mesh->tag_set_data(mesh->owner_tag, &elem, 1, &orig_owner);
        ESMC_CHECK_MOAB_THROW(merr);
      }
    }
  }
  CATCH_MBMESH_RETHROW
}

// Assign node orig_pos in output_mesh using ndir
void mbmesh_set_node_orig_pos(MBMesh *output_mesh, int num_node_gids, int *node_gids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_set_node_orig_pos()"
  try {
    int localrc, merr;

    // for using a std::map<UInt, EntityHandle>
    typedef std::pair<UInt, EntityHandle> IDEntityPair;

    // Get a list of the Mesh nodes with gids
    Range nodes;

    // get mesh nodes and gids
    merr=output_mesh->mesh->get_entities_by_dimension(0,0,nodes);
    ESMC_CHECK_MOAB_THROW(merr);
  
    std::map<UInt, EntityHandle> gids_to_nodes;

    // Loop through objects getting ids
    Range::const_iterator si = nodes.begin(), se = nodes.end();
    for (; si != se; ++si) {
      const EntityHandle node = *si;
      int gid;
      MBMesh_get_gid(output_mesh, node, &gid);
      gids_to_nodes.insert(IDEntityPair(gid,node));
    }

    // Loop setting orig_pos_tag
    for (int i = 0; i < num_node_gids; ++i) {
      std::map<UInt, EntityHandle>::const_iterator it = gids_to_nodes.find(node_gids[i]);

      if (it != gids_to_nodes.end()) {
        merr=output_mesh->mesh->tag_set_data(output_mesh->orig_pos_tag,
                                             &it->second, 1, &i);
        ESMC_CHECK_MOAB_THROW(merr);
      }

#undef print_nodeorigpos
#ifdef print_nodeorigpos
      VM *vm = VM::getCurrent(&localrc);
      int localPet = vm->getLocalPet();
      printf("%d# node %d gid %d orig_pos %d\n", localPet, gids_to_nodes->first, gids_to_nodes->second(), i);
#endif
    }
  }
  CATCH_MBMESH_RETHROW
}


// Assign node orig_pos in mesh
void mbmesh_set_node_orig_pos_wo_list(MBMesh *output_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_set_node_orig_pos_wo_list()"
  try {
    int localrc, merr;

    // Get a list of the Mesh nodes with gids
    Range nodes;
    merr=output_mesh->mesh->get_entities_by_dimension(0, 0, nodes);
    ESMC_CHECK_MOAB_THROW(merr);

    int index=0;
    Range::const_iterator si = nodes.begin(), se = nodes.end();
    for (; si != se; ++si) {
      const EntityHandle node = *si;
      merr=output_mesh->mesh->tag_set_data(output_mesh->orig_pos_tag, &node, 1, &index);
      ESMC_CHECK_MOAB_THROW(merr);
      ++index;
    }
  }
  CATCH_MBMESH_RETHROW
}

// Assign elem orig_pos in output_mesh
void mbmesh_set_elem_orig_pos(MBMesh *output_mesh, int num_elem_gids, int *elem_gids) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_set_elem_orig_pos()"
  try {
    int localrc, merr;

    // for using a std::map<UInt, EntityHandle>
    typedef std::pair<UInt, EntityHandle> IDEntityPair;

    // Get a list of the Mesh nodes with gids
    Range elems;

    // get mesh elems and gids
    merr=output_mesh->mesh->get_entities_by_dimension(0, output_mesh->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    std::map<UInt, EntityHandle> gids_to_elems;

    // Loop through objects getting ids
    Range::const_iterator si = elems.begin(), se = elems.end();
    for (; si != se; ++si) {
      const EntityHandle elem = *si;
      int gid;
      MBMesh_get_gid(output_mesh, elem, &gid);
      gids_to_elems.insert(IDEntityPair(gid,elem));
    }

    // Loop setting orig_pos_tag
    for (int i = 0; i < num_elem_gids; ++i) {
      std::map<UInt, EntityHandle>::const_iterator it = gids_to_elems.find(elem_gids[i]);

      if (it != gids_to_elems.end()) {
        merr=output_mesh->mesh->tag_set_data(output_mesh->orig_pos_tag,
                                             &it->second, 1, &i);
        ESMC_CHECK_MOAB_THROW(merr);
      }

#undef print_elemorigpos
#ifdef print_elemorigpos
      VM *vm = VM::getCurrent(&localrc);
      int localPet = vm->getLocalPet();
      printf("%d# elem %d gid %d orig_pos %d\n", localPet, gids_to_elems->first, gids_to_elems->second(), i);
#endif
    }
  }
  CATCH_MBMESH_RETHROW
}

// Assign elem orig_pos in output_mesh
void mbmesh_set_elem_orig_pos_wo_list(MBMesh *output_mesh) {
#undef  ESMC_METHOD
#define ESMC_METHOD "mbmesh_set_elem_orig_pos_wo_list()"
  try {
    int localrc, merr;

    Range elems;
    merr=output_mesh->mesh->get_entities_by_dimension(0, output_mesh->pdim, elems);
    ESMC_CHECK_MOAB_THROW(merr);

    int index=0;
    Range::const_iterator ei = elems.begin(), ee = elems.end();
    for (; ei != ee; ++ei) {
      const EntityHandle elem = *ei;
      merr=output_mesh->mesh->tag_set_data(output_mesh->orig_pos_tag, 
                                           &elem, 1, &index);
      ESMC_CHECK_MOAB_THROW(merr);
      ++index;
    }
  }
  CATCH_MBMESH_RETHROW
}

} // ESMCI namespace


#endif // ESMF_MOAB
