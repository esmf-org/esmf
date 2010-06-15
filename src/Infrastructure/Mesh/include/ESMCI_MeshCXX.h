// $Id: ESMCI_MeshCXX.h,v 1.11 2010/06/15 23:10:16 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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

    static MeshCXX* create(int*, int*, int*);
    int addElements(int*, int*, int*, int*);
    int addNodes(int*, int*, double*, int*);
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
