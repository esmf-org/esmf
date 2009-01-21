// $Id: ESMC_MeshSkin.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MeshSkin_h
#define ESMC_MeshSkin_h

#include "ESMC_Mesh.h"

namespace ESMCI {
namespace MESH {

// Add sidesets for exterior blocks and for interior block boundaries
// (Locally) mark nodes as boundary nodes.  If add_fadces=true, create the faces.
void Skin(Mesh &mesh);

// For a parallel mesh, resolve sides on a parallel boundary, i.e. delete sides
// that have been created on a parallel boundary because the local proc thinks this
// is an exposed boundary.  Also, deimprint the exposedboundary from the corresponding
// nodes.
void ResolveParSkin(Mesh &mesh);

} //namespace
} //namespace

#endif
