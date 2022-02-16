// $Id$
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MBMesh_SplitMerge_h
#define ESMCI_MBMesh_SplitMerge_h

#include <Mesh/include/ESMCI_MBMesh.h>


using namespace ESMCI;

  // structure holding original and split elems
#undef  ESMC_METHOD
#define ESMC_METHOD "MB_OSE()"
  struct MB_OSE{
    const MBMesh *mesh;
    const EntityHandle *orig_elem;
    const EntityHandle *split_elem;
    int orig_id;
    int split_id;
    
    int orig_pos;

    MB_OSE() : mesh(NULL), orig_elem(NULL), split_elem(NULL) {};
    MB_OSE(const MBMesh *_mesh, 
           const EntityHandle *_orig_elem, 
           const EntityHandle *_split_elem) {
      mesh = _mesh;
      orig_elem = _orig_elem;
      split_elem = _split_elem;
      
      int merr;
      merr=mesh->mesh->tag_get_data(mesh->orig_pos_tag, _orig_elem, 1, &orig_pos);
      merr=mesh->mesh->tag_get_data(mesh->gid_tag, _orig_elem, 1, &orig_id);
      merr=mesh->mesh->tag_get_data(mesh->gid_tag, _split_elem, 1, &split_id);
    };

    bool operator<(const MB_OSE &rhs) const {
      int merr;
      int rhs_orig_pos;
      int rhs_orig_id;
      int rhs_split_id;
      merr=mesh->mesh->tag_get_data(mesh->orig_pos_tag, rhs.orig_elem, 1, &rhs_orig_pos);
      merr=mesh->mesh->tag_get_data(mesh->gid_tag, rhs.orig_elem, 1, &rhs_orig_id);
      merr=mesh->mesh->tag_get_data(mesh->gid_tag, rhs.split_elem, 1, &rhs_split_id);
      if ((orig_elem == NULL) || (split_elem == NULL))
        Throw() << "Members shouldn't be NULL.";
      if ((rhs.orig_elem == NULL) || (rhs.split_elem == NULL))
        Throw() << "Members shouldn't be NULL.";
      if (orig_pos != rhs_orig_pos)
        return (orig_pos < rhs_orig_pos);
      if (orig_id != rhs_orig_id)
        return (orig_id < rhs_orig_id);
        
      return (split_id < rhs_split_id);
    }

  };

  void mbmesh_get_elem_merged_connlist(const MBMesh &mesh, 
                                std::vector<MB_OSE>::iterator beg_elem_range, 
                                std::vector<MB_OSE>::iterator end_elem_range, 
                                std::vector<int> &used, 
                                std::vector<int> &elem_merged_nids,
                                bool debug=false);
                                          
  // Create a connection list for a mesh that has the original >4 sided connections
  void mbmesh_get_mesh_merged_connlist(const MBMesh &mesh, 
                                std::vector<int> &num_merged_nids, 
                                std::vector<int> &merged_nids,
                                bool debug=false);
  
#endif
