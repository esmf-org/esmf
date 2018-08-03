// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MBMesh_h
#define ESMCI_MBMesh_h


#if defined ESMF_MOAB
#include "moab/Core.hpp"
using namespace moab;
#endif

#include <map>

namespace ESMCI {

  class MBMesh {
#if defined ESMF_MOAB

  public:
    int sdim, pdim; // dimensions MAYBE I SHOULD NAME THESE MORE SIMILAR TO WHAT IN OTHER MESH
    int orig_sdim;
    Interface *mesh; // Moab mesh  MAYBE I SHOULD NAME ThIS SOMETHING ELSE????

    int num_verts;
    EntityHandle *verts; // Temporary storage for element create

    int num_elems; // number of elems on this processor

    // Tags
    Tag gid_tag;
    Tag orig_pos_tag;
    Tag owner_tag;

    bool has_node_orig_coords;
    Tag node_orig_coords_tag;

    bool has_node_mask;
    Tag node_mask_tag;
    Tag node_mask_val_tag;

    bool has_elem_frac;
    Tag  elem_frac_tag;

    bool has_elem_mask;
    Tag elem_mask_tag;
    Tag elem_mask_val_tag;

    bool has_elem_area;
    Tag  elem_area_tag;

    bool has_elem_coords;
    Tag  elem_coords_tag;

    bool has_elem_orig_coords;
    Tag  elem_orig_coords_tag;

    // Split stuff
    bool is_split;
    int max_non_split_id;
    std::map<int,int> split_to_orig_id;
    std::map<int,double> split_id_to_frac;

#endif

    MBMesh();

    ~MBMesh();

  };

} // namespace 

#endif
