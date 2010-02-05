// $Id: ESMCI_MeshCXX.h,v 1.8.2.1 2010/02/05 19:59:28 svasquez Exp $
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
 int MeshVTKHeader(char*, int*, int*, int*);
 int MeshVTKBody(char*, int*, double*, int*, int*, int*, int*);

  class MeshCXX  {

    public:
    MeshCXX();
    ~MeshCXX();

    static MeshCXX* create(int*, int*, int*);
    int addElements(int*, int*, int*, int*);
    int addNodes(int*, int*, double*, int*);
    std::vector<int> getNodeGIDS();
    int createDistGrids(int*, int*, int*, int*);
    int meshWrite(char*);
    int destroy();
    int freeMemory();
    int numNodes();
    int numElements();

    friend int MeshVTKHeader(char*, int*, int*, int*);
    friend int MeshVTKBody(char*, int*, double*, int*, int*, int*, int*);

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
