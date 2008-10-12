//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_MeshRefine_h
#define ESMC_MeshRefine_h

#include <Mesh/include/ESMC_MeshObj.h>
#include <Mesh/include/ESMC_MEField.h>
#include <Mesh/include/ESMC_RefineTopo.h>

namespace ESMCI {

// Refine the object on the local processor (may require synchronization
// of face/edge refinement across processor boundaries).
void RefineMeshObjLocal(MeshObj &obj, const RefineTopo &rtopo);

// Unrefine a mesh object.  Remove any children that no longer have a parent.
void UnrefineMeshObjLocal(MeshObj &obj);

} // namespace

#endif
