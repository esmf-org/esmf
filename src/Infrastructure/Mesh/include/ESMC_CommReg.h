// $Id: ESMC_CommReg.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_CommReg_h
#define ESMC_CommReg_h

#include <ESMC_CommRel.h>
#include <ESMC_MEField.h>

#include <iostream>


namespace ESMCI {
namespace MESH {

/**
 * A class to represent a connection between two meshes (possible the same mesh).
 * Aggregates the CommRel for different types of objects in the topological hierarchy.
 */
class CommReg {
public:
CommReg();
CommReg(const std::string &_name, Mesh &domainMesh, Mesh &rangeMesh);
~CommReg();

CommRel &GetCommRel(UInt obj_type);
const CommRel &GetCommRel(UInt obj_type) const;

void Transpose();

// Send the field(s) down the pipeline.  Sends the sub _fields
// using the necessary spec.
void SendFields(UInt nfields, MEField<> *const *sfields, MEField<> *const *rfields);

void HaloFields(UInt nfields, MEField<> **sfields);

/**
 * To each shared object, perform a global reduction
*/
template<typename VTYPE>
void SwapOp(UInt nfields, MEField<> **fields, int op);

/** Synchronize attributes in parallel. */
void SyncAttributes();

// Verify the symmetric comms (if they are symmetric);
bool VerifySymComm();

void CommPrint(std::ostream &);

void clear();

private:
CommRel node_rel;
CommRel edge_rel;
CommRel face_rel;
CommRel elem_rel;
const MeshDB *dom;
const MeshDB *ran;
};

} // namespace
} // namespace

#endif
