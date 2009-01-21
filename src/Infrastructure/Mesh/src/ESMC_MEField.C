// $Id: ESMC_MEField.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_MEField.h>
#include <ESMC_MeshField.h>


namespace ESMCI {
namespace MESH {

// ********** New style fields ************
// MEFieldBase (no templates)
MEFieldBase::MEFieldBase(const std::string &_name, const MEFamily &_mef,
    UInt _obj_type, const Context &_ctxt, UInt dim, bool out, bool _interp, const _fieldTypeBase &_ftype) :
fname(_name),
mef(_mef),
obj_type(_obj_type),
my_ctxt(_ctxt),
fdim(dim),
output(out),
interp(_interp),
ftype(_ftype),
ordinal(0)
{
}

MEFieldBase::~MEFieldBase() {
}

// ****** MEField ********
template<typename _FIELD>
MEField<_FIELD>::MEField(const std::string &_name, const MEFamily &_mef,
    UInt _obj_type, const Context &_ctxt, UInt dim, bool out, bool interp, const _fieldTypeBase &_ftype) :
MEFieldBase(_name, _mef, _obj_type, _ctxt, dim, out, interp, _ftype),
fidx_table(),
fields(),
primaryfield(NULL),
interpfield(NULL)
{
}

template<typename _FIELD>
MEField<_FIELD>::~MEField() {
}

template<typename _FIELD>
void MEField<_FIELD>::Addfield(_FIELD *f, UInt nval) {
  // Store as a hash of fields by nval;
  std::vector<UInt>::iterator i = 
    std::lower_bound(fidx_table.begin(), fidx_table.end(), nval);

  // Error if already there
  if (i == fidx_table.end() || *i != nval) {
    i = fidx_table.insert(i, nval);
  } else Throw() << "Field:" << fname << ", already added nval:" << nval;

  // Insert field at same spot.
  UInt pos = std::distance(fidx_table.begin(), i);
  typename std::vector<_FIELD*>::iterator fi = fields.begin() + pos;
  UInt cursize = fields.size();
  if (cursize == 0) primaryfield = f;
  // if nval is larger, resize.  Keeps old fields, assigns null elsewhere
  ThrowAssert(fields.size() <= pos);
  fields.insert(fi, f);
}

template<typename _FIELD>
UInt MEField<_FIELD>::Numfields() const {
  return fidx_table.size();
}

template<typename _FIELD>
_FIELD *MEField<_FIELD>::Getfield(UInt nval) const {
  std::vector<UInt>::const_iterator i = 
    std::lower_bound(fidx_table.begin(), fidx_table.end(), nval);
  // Error if not there
  if (i == fidx_table.end() || *i != nval) {
    Throw() << "Field:" << fname << ", could not get nval:" << nval;
  }

  UInt pos = std::distance(fidx_table.begin(), i);
  return *(fields.begin() + pos);
};

template<typename _FIELD>
void MEField<_FIELD>::Getfields(std::vector<_FIELD*> &res) const {
  for (UInt i = 0; i < fields.size(); i++) {
    if (fields[i]) res.push_back(fields[i]);
  }
}

template class MEField<_field>;
template class MEField<SField>;

} // namespace
} // namespace
