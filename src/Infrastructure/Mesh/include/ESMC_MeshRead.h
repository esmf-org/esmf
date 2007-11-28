// $Id: ESMC_MeshRead.h,v 1.4 2007/11/28 16:23:22 dneckels Exp $
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
#ifndef ESMC_MeshRead
#define ESMC_MeshRead

#include <mpi.h>

#include <mesh/ESMC_Mesh.h>

namespace ESMC {

enum { ESMC_FILE_EXODUS = 0, ESMC_FILE_VTK=1 };

// Read the mesh in parallel (append the correct strings to the local filename)
// Create the shared CommRel.
void ReadMesh(Mesh &mesh, const std::string &fbase, bool skin=true, int file_type=ESMC_FILE_EXODUS);

void WriteMesh(const Mesh &mesh, const std::string &fbase, int step = 1, double tstep=0.0, int file_type=ESMC_FILE_EXODUS);

void WriteMeshTimeStep(const Mesh &mesh, const std::string &fbase, int step = 1, double tstep = 0.0);

} // namespace

#endif
