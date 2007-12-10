//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_MeshVTK_h
#define ESMC_MeshVTK_h

#include <Mesh/include/ESMC_MeshTypes.h>

#include <string>

namespace ESMC {

class Mesh;

void WriteVTKMesh(const Mesh &mesh, const std::string &filename);

void ReadVTKMesh(Mesh &mesh, const std::string &filename);

} // namespace

#endif
