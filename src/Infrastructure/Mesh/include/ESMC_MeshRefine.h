// $Id: ESMC_MeshRefine.h,v 1.2 2007/11/28 16:23:22 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_MeshRefine_h
#define ESMC_MeshRefine_h

#include <mesh/ESMC_MeshObj.h>
#include <mesh/ESMC_MEField.h>
#include <mesh/ESMC_RefineTopo.h>

namespace ESMC {

// Refine the object on the local processor (may require synchronization
// of face/edge refinement across processor boundaries).
void RefineMeshObjLocal(MeshObj &obj, const RefineTopo &rtopo);

// Unrefine a mesh object.  Remove any children that no longer have a parent.
void UnrefineMeshObjLocal(MeshObj &obj);

// elems is the list of (newly) refine elements
void ProlongNodeCoords(std::vector<MeshObj*> &elems, MEField<> &coord);

} // namespace

#endif
