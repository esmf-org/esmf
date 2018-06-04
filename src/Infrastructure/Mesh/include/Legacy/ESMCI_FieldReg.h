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
#ifndef ESMCI_FieldReg_h
#define ESMCI_FieldReg_h

#include <Mesh/include/Legacy/ESMCI_Attr.h>
#include <Mesh/include/Legacy/ESMCI_MEField.h>
#include <Mesh/include/Legacy/ESMCI_MeshDB.h>
#include <Mesh/include/Legacy/ESMCI_MEFamily.h>
#include <Mesh/include/Legacy/ESMCI_MeshllField.h>
#include <map>
#include <Mesh/include/Legacy/ESMCI_MeshDB.h>
#include <Mesh/include/Legacy/ESMCI_Iterator.h>
#include <Mesh/include/Legacy/ESMCI_IOField.h>

/**
 * @defgroup field
 *
 * The field subsystem contains methods that allow the storage of data on
 * the mesh database.
 * @ingroup mesh
 */
namespace ESMCI {

class Kernel;

/**
 * A collector for fields, i.e. the Field Registrar.  A field (with a particular name)
 * may be registered multiple times, with multiple attrs.
 * @ingroup field
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


 void ProxyCommit(MeshDB &mesh,
                  int numSetsArg,
                  std::vector<UInt> nvalSetSizesArg, std::vector<UInt> nvalSetValsArg,
                  std::vector<UInt> nvalSetObjSizesArg, std::vector<UInt> nvalSetObjValsArg);

#if 0
 void GetImprints(
                  int *numSetsArg,
                  std::vector<UInt> nvalSetSizesArg, std::vector<UInt> nvalSetValsArg,
                  std::vector<UInt> nvalSetObjSizesArg, std::vector<UInt> nvalSetObjValsArg);
#endif

void GetImprints(
                 int *numSetsArg,
                 UInt **nvalSetSizesArg, UInt **nvalSetValsArg,
                 UInt **nvalSetObjSizesArg, UInt **nvalSetObjValsArg);

UInt NumFields() const { return Fields.size();}
MEFieldBase **ListOfFields() {
// Note that if Fields is empty, most STLs seem to return a NULL pointer
// when returning &Fields[0].  However the Microsoft STL aborts the
// program.  So we need an explicit check here.
  if (Fields.size() == 0) {
//    std::cerr << "Mesh::ListOfFields: Warning: Fields.size = 0.  Returning NULL" << std::endl;
    return NULL;
  }
  return &Fields[0];
}

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



 int numSets;
 std::vector<UInt> nvalSetSizes;
 std::vector<UInt> nvalSetVals;
 std::vector<UInt> nvalSetObjSizes;
 std::vector<UInt> nvalSetObjVals;


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

#endif
