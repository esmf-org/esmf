// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshRead
#define ESMCI_MeshRead

#include <mpi.h>

#include <Mesh/include/ESMCI_Mesh.h>
#include "ESMCI_Array.h"

namespace ESMCI {

enum { ESMC_FILE_EXODUS = 0, ESMC_FILE_VTK=1 };

// Read the mesh in parallel (append the correct strings to the local filename)
// Create the shared CommRel.
void ReadMesh(Mesh &mesh, const std::string &fbase, bool skin=true, int file_type=ESMC_FILE_VTK);

 void WriteMesh(const Mesh &mesh, const std::string &fbase, 
                int num_nodeArrays=0, ESMCI::Array **nodeArrays=NULL, 
                int num_elemArrays=0, ESMCI::Array **elemArrays=NULL, 
                int step = 1, double tstep=0.0, int file_type=ESMC_FILE_VTK);

void WriteMeshTimeStep(const Mesh &mesh, const std::string &fbase, int step = 1, double tstep = 0.0);

} // namespace

#endif
