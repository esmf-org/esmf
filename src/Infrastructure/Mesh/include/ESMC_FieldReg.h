// $Id: ESMC_FieldReg.h,v 1.4.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_FieldReg_h
#define ESMC_FieldReg_h

#include <ESMC_Attr.h>
#include <ESMC_MEField.h>
#include <ESMC_MeshDB.h>
#include <ESMC_MEFamily.h>
#include <ESMC_Meshfield.h>
#include <map>
#include <ESMC_MeshDB.h>
#include <ESMC_Iterator.h>
#include <ESMC_IOField.h>

namespace ESMCI {
namespace MESH {

class Kernel;

/**
 * A collector for fields, i.e. the Field Registrar.  A field (with a particular name)
 * may be registered multiple times, with multiple attrs. 
*/
class FieldReg {
public:
FieldReg();
~FieldReg();

// Create a field on a block (elem block or sideset)
MEField<> *RegisterField(const std::string &name,
       const MEFamily &mef, // Imprint pattern, number dofs, etc
       UInt obj_type, // ELEMENT, FACE ??
       const Context &ctxt, // set 
       UInt dim,
       bool out = false,        // whether to output
       bool interp = false,     // whether to create a field for interpolation
       const _fieldTypeBase &ftype = _fieldType<double>::instance()
       );


MEField<> *GetField(const std::string &fname) const;

_field *Getfield(const std::string &fname) const;

// Create a list of fields with the same names, but from this mesh
void MatchFields(UInt nfields, MEField<> **fds, std::vector<MEField<>*> &res) const;

MEField<> *GetCoordField() const;

// Create the fields that were temporarily store as nodal/element fields from Exodus file.
// Call before commit.
void CreateDBFields();

// Load the data from nodal/element to regular fields.
void PopulateDBFields(MeshDB &mesh);

// Free the db fields.
void ReleaseDBFields();

// Commit the registry
void Commit(MeshDB &);

UInt NumFields() const { return Fields.size();}
MEFieldBase **ListOfFields() { return &Fields[0]; }

typedef std::map<std::string, MEField<>*> FMapType;
typedef Map_Ptr_Adapt_iterator<FMapType, FMapType::const_iterator, MEField<>, const MEField<>&, const MEField<> *> MEField_const_iterator;
typedef Map_Ptr_Adapt_iterator<FMapType, FMapType::iterator, MEField<>, MEField<>&, MEField<>*> MEField_iterator;

MEField_const_iterator Field_begin() const { return fmap.begin();}
MEField_const_iterator Field_end() const { return fmap.end();}

MEField_iterator Field_begin() { return fmap.begin();}
MEField_iterator Field_end() { return fmap.end();}

UInt Numfields() const { return fields.size(); }
_field **ListOffields() { return &fields[0]; }

_field *Registerfield(const std::string &name, const Attr &attr, const _fieldTypeBase &_ftype, UInt dim);

// Register low-level iofields.  TODO: this interface is currently contorted by
// having to pass the mesh as an arg.  This is, in reality, a dependency issue.  
// Needs to be resolved.
IOField<NodalField> *RegisterNodalField(const MeshDB &mesh, const std::string &name, UInt dim=1);
IOField<ElementField> *RegisterElementField(const MeshDB &mesh, const std::string &name, UInt dim=1);

protected:
// Register a bootstrap type field;
friend void LoadExMesh(Mesh &mesh, const std::string &filename, int nstep);

private:

bool is_committed;
FMapType fmap;
typedef std::map<std::string, _field*> fMapType;
fMapType _fmap;
UInt nfields;
std::vector<_field*> fields;
std::vector<MEFieldBase*> Fields;
std::vector<IOField<NodalField>*> ndfields;
std::vector<IOField<ElementField>*> efields;
};

} // namespace 
} // namespace 

#endif
