// $Id: ESMC_MeshRefine.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_MeshRefine_h
#define ESMC_MeshRefine_h

#include <ESMC_MeshObj.h>
#include <ESMC_MEField.h>
#include <ESMC_RefineTopo.h>

namespace ESMCI {
namespace MESH {

// Refine the object on the local processor (may require synchronization
// of face/edge refinement across processor boundaries).
void RefineMeshObjLocal(MeshObj &obj, const RefineTopo &rtopo);

// Unrefine a mesh object.  Remove any children that no longer have a parent.
void UnrefineMeshObjLocal(MeshObj &obj);

// elems is the list of (newly) refine elements
void ProlongNodeCoords(std::vector<MeshObj*> &elems, MEField<> &coord);

} // namespace
} // namespace

#endif
