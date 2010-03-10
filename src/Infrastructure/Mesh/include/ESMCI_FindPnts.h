//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_FindPnts_h
#define ESMCI_FindPnts_h


#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshObjTopo.h>
#include <Mesh/include/ESMCI_Mapping.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_OTree.h>
#include <Mesh/include/ESMCI_SearchFlags.h>
#include <Mesh/include/ESMCI_Exception.h> 
#include <algorithm>
#include <iterator>

#include <ostream>

#include <set>

#include <limits>
#include <vector>


namespace ESMCI {

  // List of error returns because Throw doesn't seem to work right now
  #define ESMCI_FINDPNT_SUCCESS       0
  #define ESMCI_FINDPNT_DIM_MISMATCH  1
  #define ESMCI_FINDPNT_PNT_NOT_FOUND 2
  #define ESMCI_FINDPNT_UNKNOWN       3

  int FindPnts(const Mesh &mesh, int unmappedaction, int dim_pnts, int num_pnts, double *pnts, int *procs, int *gids);

} //namespace

#endif
