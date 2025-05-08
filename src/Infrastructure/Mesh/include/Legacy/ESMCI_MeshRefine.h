// $Id$
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshRefine_h
#define ESMCI_MeshRefine_h

#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/Legacy/ESMCI_MEField.h>
#include <Mesh/include/Legacy/ESMCI_RefineTopo.h>

namespace ESMCI {

// Refine the object on the local processor (may require synchronization
// of face/edge refinement across processor boundaries).
void RefineMeshObjLocal(MeshObj &obj, const RefineTopo &rtopo);

// Unrefine a mesh object.  Remove any children that no longer have a parent.
void UnrefineMeshObjLocal(MeshObj &obj);

} // namespace

#endif
