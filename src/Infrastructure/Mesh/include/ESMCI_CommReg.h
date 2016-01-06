// $Id$
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_CommReg_h
#define ESMCI_CommReg_h

#include <Mesh/include/ESMCI_CommRel.h>
#include <Mesh/include/ESMCI_MEField.h>

#include <iostream>


namespace ESMCI {

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

/*
 * Synchronize attributes for shared objects.
 * This consists of or'ing attributes together, except
 * for the following bits:
 *   Attr::SHARED_ID
 *   Attr::OWNED_ID
 *   Attr::ACTIVE_ID
 *   Attr::GENESIS_ID
 * 
 * Consistency checks are enfoced for these bits,
 * meaning they should be fixed before calling sync.
 */
void SyncAttributes();

/*
 * Same as @func{SyncAttributes}, but
 * only for the object types that are or'ed into
 * the argument.
 */
void SyncAttributes(UInt obj_type);

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

#endif
