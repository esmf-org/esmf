// $Id: ESMC_MeshPartition.h,v 1.2 2007/11/28 16:23:22 dneckels Exp $
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

#include <mesh/ESMC_Mesh.h>
#include <mesh/ESMC_MeshTypes.h>
#include <mesh/ESMC_MeshField.h>

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
