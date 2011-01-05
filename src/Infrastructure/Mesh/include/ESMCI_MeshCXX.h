// $Id: ESMCI_MeshCXX.h,v 1.13 2011/01/05 20:05:44 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
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
 int MeshVTKHeader(const char*, int*, int*, int*);
 int MeshVTKBody(const char*, int*, double*, int*, int*, int*, int*);

  class MeshCXX  {

    public:
    MeshCXX();
    ~MeshCXX();

    static MeshCXX* create(int, int, int*);
    static int destroy(MeshCXX **);

    int addElements(int, int*, int*, int*);
    int addNodes(int, int*, double*, int*);
    std::vector<int> getNodeGIDS();
    int createDistGrids(int*, int*, int*, int*);
    int meshWrite(const char*);
    int destroy();
    int freeMemory();
    int numNodes();
    int numElements();

    friend int MeshVTKHeader(const char*, int*, int*, int*);
    friend int MeshVTKBody(const char*, int*, double*, int*, int*, int*, int*);

    private:
    Mesh* meshPointer;
    int isMeshFreed();
    int* nodalDistGrid;
    int* elementDistGrid;
    int numLNodes;
    int numLElements;
    int meshFreed;
  };

} // namespace 

#endif
