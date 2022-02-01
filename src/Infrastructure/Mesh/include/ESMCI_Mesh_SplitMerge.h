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
#ifndef ESMCI_Mesh_SplitMerge_h
#define ESMCI_Mesh_SplitMerge_h

#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>


using namespace ESMCI;

  // structure holding original and split elems
  struct OSE{
    const MeshObj *orig_elem;
    const MeshObj *split_elem;

    OSE() : orig_elem(NULL), split_elem(NULL) {};
    OSE(const MeshObj *_orig_elem, const MeshObj *_split_elem) :
       orig_elem(_orig_elem), split_elem(_split_elem) {};

    bool operator<(const OSE &rhs) const { 
      if ((orig_elem == NULL) || (split_elem == NULL)) Throw() << "Members shouldn't be NULL.";
      if ((rhs.orig_elem == NULL) || (rhs.split_elem == NULL)) Throw() << "Members shouldn't be NULL.";
      if (orig_elem->get_data_index() != rhs.orig_elem->get_data_index()) return (orig_elem->get_data_index() < rhs.orig_elem->get_data_index());
      if (orig_elem->get_id() != rhs.orig_elem->get_id()) return (orig_elem->get_id() < rhs.orig_elem->get_id());
      return (split_elem->get_id() < rhs.split_elem->get_id());
    }

  };

  void get_elem_merged_connlist(const Mesh &mesh, 
                                std::vector<OSE>::iterator beg_elem_range, 
                                std::vector<OSE>::iterator end_elem_range, 
                                std::vector<int> &used, 
                                std::vector<int> &elem_merged_nids);
                                          
  // Create a connection list for a mesh that has the original >4 sided connections
  void get_mesh_merged_connlist(const Mesh &mesh, 
                                std::vector<int> &num_merged_nids, 
                                std::vector<int> &merged_nids);
  
#endif
