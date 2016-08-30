// $Id: ESMCI_Search.h,v 1.13 2012/11/13 22:22:41 oehmke Exp $
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MBMesh_Redist_h
#define ESMCI_MBMesh_Redist_h

// Take out if MOAB isn't being used
#ifdef ESMF_MOAB

#include <Mesh/include/ESMCI_MBMesh.h>

#include <vector>

namespace ESMCI {

// Eventually move this to wherever you have the code to redist a mesh
struct EH_Comm_Pair {
  EntityHandle eh;
  int proc;

  EH_Comm_Pair(EntityHandle _eh, int _proc) : eh(_eh), proc(_proc) {}

  // Sort as obj <, proc
  bool operator< (const EH_Comm_Pair &other) const {
    return eh != other.eh ?
       eh < other.eh 
     : proc < other.proc;
  }

  bool operator==(const EH_Comm_Pair &other) const {
    if (eh != other.eh) return false;
    if (proc != other.proc) return false;
    return true;
  }

  bool operator!=(const EH_Comm_Pair &rhs) const {
    return !(*this == rhs);
  }
};


// This creates src and dst rendezvous meshes where the overlap is based on elements
void create_mbmesh_redist_elem(MBMesh *src_mesh, 
                               std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                               MBMesh **_out_mesh);


} //namespace

#endif // ESMF_MOAB

#endif
