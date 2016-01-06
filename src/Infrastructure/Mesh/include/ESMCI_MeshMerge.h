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
#ifndef ESMCI_MeshMerge_h
#define ESMCI_MeshMerge_h


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

  void MeshMerge(Mesh &mesh1, Mesh &mesh2, Mesh **outMesh);
  void MeshSetFraction(Mesh & mesh, double fraction);
  void MeshCreateDiff(Mesh &mesh1, Mesh &mesh2, Mesh **outMesh, double threshold);

} //namespace

#endif
