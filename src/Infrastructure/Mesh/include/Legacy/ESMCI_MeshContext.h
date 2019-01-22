// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshContext_h
#define ESMCI_MeshContext_h

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>

// Manage the number of context bits.
// Both Mesh and context include this file to determine the
// size of the context object.

namespace ESMCI {

const UInt NUM_CONTEXT_CHARS = 4;

} // namespace

#endif
