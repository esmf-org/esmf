// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MESHPARTITION_H
#define ESMCI_MESHPARTITION_H

#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>

// Partitions a mesh and saves the output as a nemesis file
namespace ESMCI {

// Partition using metis and save the results.
// ep = element partition field, np = nodal partition field
void MeshMetisPartition(const Mesh &mesh, UInt npart, const MEField<> &ep, const MEField<> &np);

// Save a partition gotten from wherever
void SavePartition(const Mesh &mesh, UInt npart, const MEField<> &ep, const MEField<> &np);

// Concat the given meshes
void MeshConcat(Mesh &mesh, std::vector<Mesh*> &srcmesh);

} //namespace

#endif
