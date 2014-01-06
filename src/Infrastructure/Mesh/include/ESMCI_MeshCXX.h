// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research, 
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
    static MeshCXX* createFromFile(char *, int, int *, int *, 
				   char *, int *, char *, int *);
    void getLocalCoords(double *, int *, int *, int *);
    static int destroy(MeshCXX **);

    int addElements(int, int*, int*, int*, int*, double *);
    int addNodes(int, int*, double*, int*);
    std::vector<int> getNodeGIDS();
    int createDistGrids(int*, int*, int*, int*);
    int meshWrite(const char*);
    int destroy();
    int freeMemory();

    friend int MeshVTKHeader(const char*, int*, int*, int*);
    friend int MeshVTKBody(const char*, int*, double*, int*, int*, int*, int*);

    private:
    enum MeshCXXLevel {MeshCXXLevel_Empty=0, 
                      MeshCXXLevel_Created, 
                      MeshCXXLevel_NodesAdded, 
                      MeshCXXLevel_Finished};

    Mesh* meshPointer;
    int isMeshFreed();
    int* nodalDistGrid;
    int* elementDistGrid;
    int numLNodes;
    int numLElements;
    int numOwnedNodes;
    int numOwnedElements;
    int meshFreed;
    MeshCXXLevel level;

  public:
    int isNodesAdded() {return (level>=MeshCXXLevel_NodesAdded);}
    int isElemsAdded() {return (level>=MeshCXXLevel_Finished);}
    int isMeshFinished() {return (level>=MeshCXXLevel_Finished);}
    int getNumLocalNodes() {return numLNodes;}
    int getNumLocalElements() {return numLElements;}
    int getNumOwnedNodes() {return numOwnedNodes;}
    int getNumOwnedElements() {return numOwnedElements;}
  };

} // namespace 

#endif
