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
#ifndef ESMCI_MeshCXX_h
#define ESMCI_MeshCXX_h

#include "ESMCI_Mesh.h"

namespace ESMCI {
class MeshCXX : public Mesh {

public:
MeshCXX();
~MeshCXX();

MeshCXX* create(int*, int*, int*);
int addElements(int*, int*, int*, int*);
int addNodes(int*, int*, double*, int*);
int createDistGrids(int*, int*);
int destroy();
int freeMemory();


private:
int isMeshFreed();
int* nodalDistGrid;
int* elementDistGrid;
int numLNodes;
int num_LElements;
int meshFreed;
};

} // namespace 

#endif
