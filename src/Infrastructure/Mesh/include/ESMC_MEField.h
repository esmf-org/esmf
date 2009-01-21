// $Id: ESMC_MEField.h,v 1.2.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MEField_h
#define ESMC_MEField_h

#include <ESMC_Meshfield.h>
#include <ESMC_MEFamily.h>

namespace ESMCI {
namespace MESH {

// ******** New style general fields *********
class MEFamily;

// Template the lower level field so that it may be replaced by a sensitivity field.
class MEFieldBase {
protected:
MEFieldBase(const std::string &name, const MEFamily &mef, UInt obj_type, const Context &ctxt,
             UInt dim, bool out, bool interp, const _fieldTypeBase &_ftype);
~MEFieldBase();
public:

friend class FieldReg;
UInt GetType() const { return obj_type; }
const Context &GetContext() const { return my_ctxt;}
const MEFamily &GetMEFamily() const { return mef; }
const std::string &name() const { return fname;}

// Vector dimension of field.
UInt dim() const { return fdim;}

UInt ObjType() const { return obj_type; }

bool is_nodal() const { return mef.is_nodal(); }
bool is_elemental() const { return mef.is_elemental(); }
bool has_interp() const { return interp; }

UInt GetOrdinal() const { return ordinal; }
void SetOutput(bool val) { output = val;}

bool Output() const { return output;}

const _fieldTypeBase &FType() const { return ftype; }

protected:
std::string fname;
const MEFamily &mef;
UInt obj_type;
const Context my_ctxt;
UInt fdim;
bool output;
bool interp;
const _fieldTypeBase &ftype;
UInt ordinal;
};


template <typename _FIELD=_field>
class MEField : public MEFieldBase {
public:
typedef _FIELD field_type;
friend class FieldReg;
MEField(const std::string &name, const MEFamily &mef, UInt obj_type, const Context &ctxt,
             UInt dim, bool out, bool interp, const _fieldTypeBase &_ftype);
~MEField();

void Addfield(_FIELD *f, UInt nval);

void SetInterp(_FIELD *f) { ThrowRequire(!interpfield); interpfield = f;}

_FIELD *GetInterp() { return interpfield; }

// Return number of low level fields
UInt Numfields() const;

// Get field for a specified number of dofs
_FIELD *Getfield(UInt nval) const;

// Return a list of all fields.  Pushes back on
// whatever is already in the list;
void Getfields(std::vector<_FIELD*> &res) const;

// ********* Some tools to make this act as a nodal field (if it is) ******
// Return the nodal field 
_FIELD *GetNodalfield() const {
  ThrowAssert(mef.is_nodal() && Numfields() == 1);
  return primaryfield;
}
_FIELD *GetElementfield() const {
  ThrowAssert(mef.is_elemental() && Numfields() == 1);
  return primaryfield;
}
// Shortcut for above
_FIELD &operator()() const {
  ThrowAssert((mef.is_nodal() || mef.is_elemental()) && Numfields() == 1);
  return *primaryfield;
}

// Return data of nodal field.
_fieldValuePtr data(const MeshObj &obj) const {
  return this->operator()().data(obj);
}

private:
std::vector<UInt> fidx_table;
std::vector<_FIELD*> fields;
_FIELD *primaryfield; // if nodal or elemental
_FIELD *interpfield;
};

} // namespace
} // namespace


#endif
