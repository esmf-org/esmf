// $Id: ESMCI_Search.h,v 1.13 2012/11/13 22:22:41 oehmke Exp $
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
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

#include "Mesh/include/ESMCI_MBMesh.h"

#include "Mesh/include/Legacy/ESMCI_DDir.h"

#include "ESMCI_PointList.h"

#include <vector>

namespace ESMCI {

struct point;

// Eventually move this to wherever you have the code to redist a mesh
struct Proc_Elem_Pair {
  int proc;
  std::vector<int> elem_gids;

  Proc_Elem_Pair(int _proc, int *_num_elem_gids, int *_elem_gids) {
    proc = _proc;
    elem_gids.reserve(*_num_elem_gids);
    for(int i=0; i<*_num_elem_gids; ++i) elem_gids.push_back(_elem_gids[i]);
  }

  // // Sort as obj <, proc
  // bool operator< (const EH_Comm_Pair &other) const {
  //   return eh != other.eh ?
  //      eh < other.eh 
  //    : proc < other.proc;
  // }
  // 
  // bool operator==(const EH_Comm_Pair &other) const {
  //   if (eh != other.eh) return false;
  //   if (proc != other.proc) return false;
  //   return true;
  // }
  // 
  // bool operator!=(const EH_Comm_Pair &rhs) const {
  //   return !(*this == rhs);
  // }
};

struct EH_Comm_Pair {
  EntityHandle eh;
  int id;
  int proc;

  EH_Comm_Pair(EntityHandle _eh, int _id, int _proc) : eh(_eh), id(_id), proc(_proc) {}

  // Sort as obj <, proc
  bool operator< (const EH_Comm_Pair &other) const {
    // replace with c++11
    return std::tie(eh, id, proc) < std::tie(other.eh, other.id, other.proc);
    // return eh != other.eh ?
    //    eh < other.eh 
    //  : proc < other.proc;
  }

  bool operator==(const EH_Comm_Pair &other) const {
    if (eh != other.eh) return false;
    if (id != other.id) return false;
    if (proc != other.proc) return false;
    return true;
  }

  bool operator!=(const EH_Comm_Pair &rhs) const {
    return !(*this == rhs);
  }

  inline int getID() const {return id;}
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

// individual entry points from the glue layer to the mesh cap
void mbmesh_redist_elem(MBMesh *src_mesh, int *num_elem_gids, int *elem_gids, MBMesh **out_mesh);
void mbmesh_redist_node(MBMesh *src_mesh, int *num_node_gids, int *node_gids, MBMesh **out_mesh);
void mbmesh_redist(MBMesh *src_mesh, int *num_node_gids, int *node_gids, int *num_elem_gids, int *elem_gids, MBMesh **out_mesh);

// This creates src and dst rendezvous meshes where the overlap is based on elements
void create_mbmesh_redist_elem(MBMesh *src_mesh, 
                               std::vector<EH_Comm_Pair> *elem_to_proc_list, 
                               MBMesh **_out_mesh);

void create_pointlist_redist_point(PointList *src_pl,
                                   std::vector<PL_Comm_Pair> *point_to_proc_list,
                                   PointList **_out_pl);

// split element handling that needs to be called from the glue layer
void mbmesh_expand_split_elem_ids(MBMesh *mesh, int num_elem_gids, int *elem_gids, int *_num_elem_gids_ws, int **_elem_gids_ws, std::map<int,int> &split_to_orig_id);
void mbmesh_set_split_orig_id_map(MBMesh *src_mesh, MBMesh *output_mesh);
void mbmesh_calc_split_id_to_frac(MBMesh *mesh);

} //namespace

#endif // ESMF_MOAB

#endif
