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
#ifndef ESMC_MeshPartition_h
#define ESMC_MeshPartition_h

#include <ESMC_Mesh.h>
#include <ESMC_MeshTypes.h>
#include <ESMC_MeshField.h>

// Partitions a mesh and saves the output as a nemesis file
namespace ESMC {

// Partition using metis and save the results.
// ep = element partition field, np = nodal partition field
void MeshMetisPartition(const Mesh &mesh, UInt npart, const MEField<> &ep, const MEField<> &np);

// Save a partition gotten from wherever
void SavePartition(const Mesh &mesh, UInt npart, const MEField<> &ep, const MEField<> &np);

// Concat the given meshes
void MeshConcat(Mesh &mesh, std::vector<Mesh*> &srcmesh);

} //namespace

#endif
