// $Id$
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshExodus_h
#define ESMCI_MeshExodus_h

#include <string>

namespace ESMCI {

class Mesh;
void LoadExMesh(Mesh &mesh, const std::string &filename, int nstep = 1);

// Write the mesh to file
void WriteExMesh(const Mesh &mesh, const std::string &filename, int nstep = 1, double tstep = 0.0);

// Write data at a timestep into file (tstep = float)
// If file does not exist, it is created
void WriteExMeshTimeStep(int nstep, double tstep, const Mesh &mesh, const std::string &filename);

} // namespace

#endif
