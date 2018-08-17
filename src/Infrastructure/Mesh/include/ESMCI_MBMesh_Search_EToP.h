// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#ifndef ESMCI_MBMesh_Search_EToP_h
#define ESMCI_MBMesh_Search_EToP_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "Mesh/include/ESMCI_MBMesh.h"

#include "ESMCI_PointList.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

using namespace ESMCI;


struct etop_sr {
  const point *node;
  int dst_gid;
  double pcoord[3];  // parametric coord of node in elem
};

struct MBMesh_Search_EToP_Result {

  EntityHandle src_elem;
  std::vector<etop_sr> dst_nodes;
  double dist;

  bool operator<(const MBMesh_Search_EToP_Result &rhs) const {
    return src_elem < rhs.src_elem;
  }
  bool operator==(const MBMesh_Search_EToP_Result &rhs) const {
    return src_elem == rhs.src_elem;
  }
  bool operator!=(const MBMesh_Search_EToP_Result &rhs) const {
    return !(*this == rhs);
   }
};
typedef std::vector<MBMesh_Search_EToP_Result*> MBMesh_Search_EToP_Result_List;


void MBMesh_Search_EToP(MBMesh *mbmAp,
                        PointList *mbmBp, int unmappedactionB,
                        int *map_type, double stol, 
                        MBMesh_Search_EToP_Result_List &result,
                        bool set_dst_status, WMat &dst_status,
                        std::vector<int> *revised_dst_loc, OTree *box_in);

#endif
#endif
