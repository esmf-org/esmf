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
#ifndef ESMCI_MeshVTK_h
#define ESMCI_MeshVTK_h

#include <Mesh/include/ESMCI_MeshTypes.h>

#include <string>

namespace ESMCI {

class Mesh;
class MeshObjTopo;

/**
 * Write the mesh as a VTK file.
 */
void WriteVTKMesh(const Mesh &mesh, const std::string &filename);

/**
 * Read and create the mesh from a VTK file.
 */
void ReadVTKMesh(Mesh &mesh, const std::string &filename);

/**
 * Find the number of elements and nodes in the mesh.  This is
 * useful if creating a Fortran mesh, since one can then allocate the
 * correct size arrays to read the full mesh.
 */
void ReadVTKMeshHeader(const std::string &filename, int &num_elems, int &num_nodes, int &conn_size);

/**
 * Read the mesh into a series of arrays.  Used to read in a mesh for testing the fortran api.
 */
void ReadVTKMeshBody(const std::string &filename, int *nodeId, double *nodeCoord, int *nodeOwner,
                     int *elemId, int *elemType, int *elemConn);

/**
 * Convert the vtk type to a mesh obj topo.
 */
const MeshObjTopo *Vtk2Topo(UInt sdim, UInt vtk_type);

} // namespace

#endif
