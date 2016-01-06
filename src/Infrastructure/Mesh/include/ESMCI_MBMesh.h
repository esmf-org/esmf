// $Id$
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MBMesh_h
#define ESMCI_MBMesh_h

// Take out if MOAB isn't being used
#ifdef ESMF_MOAB

#include "Mesh/src/Moab/moab/Core.hpp"
using namespace moab;


#include <map>

namespace ESMCI {

  class MBMesh {
#ifdef ESMF_MOAB

  public:
    int sdim, pdim; // dimensions MAYBE I SHOULD NAME THESE MORE SIMILAR TO WHAT IN OTHER MESH
    Interface *mesh; // Moab mesh  MAYBE I SHOULD NAME ThIS SOMETHING ELSE????

    int num_verts;

    EntityHandle *verts; // Temporary storage for element create

    // Tags
    Tag gid_tag;
    Tag orig_pos_tag;
    Tag owner_tag;

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

#endif // ESMF_MOAB
#endif
