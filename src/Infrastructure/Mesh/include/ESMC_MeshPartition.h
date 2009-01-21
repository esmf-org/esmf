// $Id: ESMC_MeshPartition.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MeshPartition_h
#define ESMC_MeshPartition_h

#include <ESMC_Mesh.h>
#include <ESMC_MeshTypes.h>
#include <ESMC_MeshField.h>

// Partitions a mesh and saves the output as a nemesis file
namespace ESMCI {
namespace MESH {

// Partition using metis and save the results.
// ep = element partition field, np = nodal partition field
void MeshMetisPartition(const Mesh &mesh, UInt npart, const MEField<> &ep, const MEField<> &np);

// Save a partition gotten from wherever
void SavePartition(const Mesh &mesh, UInt npart, const MEField<> &ep, const MEField<> &np);

// Concat the given meshes
void MeshConcat(Mesh &mesh, std::vector<Mesh*> &srcmesh);

} //namespace
} //namespace

#endif
