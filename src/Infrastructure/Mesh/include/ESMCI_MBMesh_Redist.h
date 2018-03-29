// $Id: ESMCI_Search.h,v 1.13 2012/11/13 22:22:41 oehmke Exp $
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
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
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_MBMesh.h>

#include "ESMCI_PointList.h"

#include <vector>

namespace ESMCI {

struct point;

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

struct PL_Comm_Pair {
  int loc;
  int proc;

  PL_Comm_Pair(int _loc, int _proc) : loc(_loc), proc(_proc) {}

  // Sort as obj <, proc
  bool operator< (const PL_Comm_Pair &other) const {
    return loc != other.loc ?
       loc < other.loc
     : proc < other.proc;
  }

  bool operator==(const PL_Comm_Pair &other) const {
    if (loc != other.loc) return false;
    if (proc != other.proc) return false;
    return true;
  }

  bool operator!=(const PL_Comm_Pair &rhs) const {
    return !(*this == rhs);
  }
};

// This creates src and dst rendezvous meshes where the overlap is based on elements
void create_mbmesh_redist_elem(MBMesh *src_mesh, 
                               std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                               MBMesh **_out_mesh);

void create_pointlist_redist_point(PointList *src_pl,
                                   std::vector<PL_Comm_Pair> *point_to_proc_list,
                                   PointList **_out_pl);


} //namespace

#endif // ESMF_MOAB

#endif
