// $Id: ESMC_Mesh.h,v 1.1 2007/08/07 17:47:56 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
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

#ifndef ESMC_Mesh_h
#define ESMC_Mesh_h

#include <ESMC_MeshDB.h>
#include <ESMC_CommRel.h>
#include <ESMC_MeshFieldReg.h>
#include <ESMC_CommReg.h>
#include <ESMC_DDir.h>
#include <mpi.h>


namespace ESMCI {
namespace MESH {

/**
 * Basic parallel mesh operations.  Aggregates the serial meshes,
 * the list of fields, and the parallel communiation relations
 */
class Mesh : public MeshDB, public FieldReg, public CommReg {
public:
friend void WriteExMesh(const MeshDB &mesh, const std::string &filename,int,double);
friend void WriteExMeshTimeStep(int,double,const MeshDB&,const std::string&);
friend void LoadExMesh(MeshDB &mesh, const std::string &filename,int);
friend void ReadMesh(Mesh&,const std::string&, bool,int);
Mesh();
~Mesh();

/**
 * Sets the mesh in 'stone'.  Builds the field data storage
 * and allocates space for fields.  All fields must be registered
 * prior to calling this function
 */
void Commit();

bool IsParallel() const { return true;}

/**
 * Objects are deleted on the local, serial mesh.  This function performs
 * a parallel resolution to see if the objects should be deleted, or if
 * they need to be kept for ghosting purposes
 */
void ResolvePendingDelete();

/**
 * Objects are created locally.  This function tries to see if the same object
 * has been created on neighboring processors, and resolves such creation.  Also,
 * if the object needs to be ghosted on a neighbor, this is done.  Global unique
 * identifiers are selected.
 */
void ResolvePendingCreate();

const CommRel &GetSymNodeRel() const;

// Build ScratchGhost
struct scratch_data {
  CommRel *node_ghost;
  CommRel *elem_ghost;
};
void CreateGhost();
void RemoveGhost();

// Create the sym rel
void build_sym_comm_rel(UInt obj_type);

IOField<NodalField> *RegisterNodalField(const std::string &name, UInt dim=1) {
  return FieldReg::RegisterNodalField(*this, name, dim);
}
IOField<ElementField> *RegisterElementField(const std::string &name, UInt dim=1) {
  return FieldReg::RegisterElementField(*this, name, dim);
}

private:
void assign_new_ids();
scratch_data *sghost;
bool committed;
};

} // namespace 
} // namespace 

#endif
