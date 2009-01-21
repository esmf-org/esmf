// $Id: ESMC_MeshGen.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MeshGen_h
#define ESMC_MeshGen_h

#include <ostream>

namespace ESMCI {
namespace MESH {

class Mesh;
class MeshObjTopo;

// Generate a hyper cube on proc 0
void HyperCube(Mesh &mesh, const MeshObjTopo *topo);

} // namespace
} // namespace

#endif
