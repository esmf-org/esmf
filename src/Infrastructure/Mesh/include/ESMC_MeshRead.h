// $Id: ESMC_MeshRead.h,v 1.3.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_MeshRead
#define ESMC_MeshRead

#include <mpi.h>

#include <ESMC_Mesh.h>

namespace ESMCI {
namespace MESH {

enum { ESMC_FILE_EXODUS = 0, ESMC_FILE_VTK=1 };

// Read the mesh in parallel (append the correct strings to the local filename)
// Create the shared CommRel.
void ReadMesh(Mesh &mesh, const std::string &fbase, bool skin=true, int file_type=ESMC_FILE_VTK);

void WriteMesh(const Mesh &mesh, const std::string &fbase, int step = 1, double tstep=0.0, int file_type=ESMC_FILE_VTK);

void WriteMeshTimeStep(const Mesh &mesh, const std::string &fbase, int step = 1, double tstep = 0.0);

} // namespace
} // namespace

#endif
