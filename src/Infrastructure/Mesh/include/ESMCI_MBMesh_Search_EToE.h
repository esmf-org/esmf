// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_MBMesh_Search_EToE_h
#define ESMCI_MBMesh_Search_EToE_h

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include "Mesh/include/ESMCI_MBMesh.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
// static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

using namespace ESMCI;


struct MBMesh_Search_EToE_Result {

  EntityHandle src_elem;
  std::vector<EntityHandle> dst_elems;

  // AN IDEA would be to do some kind of inheritence instead of having nodes and elems in the same struct

  bool operator<(const MBMesh_Search_EToE_Result &rhs) const {
    return src_elem < rhs.src_elem;
  }
  bool operator==(const MBMesh_Search_EToE_Result &rhs) const {
    return src_elem == rhs.src_elem;
  }
  bool operator!=(const MBMesh_Search_EToE_Result &rhs) const {
    return !(*this == rhs);
   }
};
typedef std::vector<MBMesh_Search_EToE_Result*> MBMesh_Search_EToE_Result_List;


void MBMesh_Search_EToE(MBMesh *mbmAp, int unmappedactionA, MBMesh *mbmBp, int unmappedactionB, 
                        double stol, MBMesh_Search_EToE_Result_List &result);

#endif
#endif
