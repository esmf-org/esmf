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
#ifndef ESMCI_MEField_h
#define ESMCI_MEField_h

#include <Mesh/include/Legacy/ESMCI_MeshllField.h>
#include <Mesh/include/Legacy/ESMCI_MEFamily.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>

#include <vector>
#include <set>

namespace ESMCI {

class MEFamily;

/**
 * The MEField hierarchy is a high level interface to the field system 
 * which manipulates and creates lower level fields as needed to represent
 * the finite element that registers the class.  The hierarchy is templated
 * on the low level field type so that a 'sensitivity' type field may be
 * substituted for a real field, providing an MEField interface that accrues
 * sensitivities to the underlying field coefficients.  
 * This base is all of functionality that does not depend on the template type.
 * @ingroup field
 */
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

//protected:
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

/**
 * A finite element field, which creates the low level fields needed 
 * to store the data for a given finite element.
 * @ingroup field
 */
template <typename _FIELD=_field>
class MEField : public MEFieldBase {
public:
typedef _FIELD field_type;
friend class FieldReg;
friend class MEField<SField>;
friend class MEField<_field>;
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
MEField(const MEField &rhs);
MEField &operator=(const MEField &rhs);
public: // Since 
std::vector<UInt> fidx_table;
std::vector<_FIELD*> fields;
_FIELD *primaryfield; // if nodal or elemental
_FIELD *interpfield;
};

/**
 * Specialization for a sensitivity field.
 * 
 * This class provides an MEField<> interface to objects that use such a field.
 * However, each degree of freedom is a fad_type variable, so that sensitivities
 * may be tracked wrt these dofs.
 * 
 * To call:
 * First, construct by giving a real MEField<> that you wish to replace.  The class
 * creates the lower level SFields, which mimic _fields at this point.
 * 
 * You do not want to use this object for the whole mesh, since the sensitivity matrix
 * would then be, in effect, dense.  Instead, you use this on either an element (
 * for element assembly), or on a group of elements (patch recovery or DG assembly).
 * 
 * When coming to a new set of elements, first call AssignElements.  This counts the
 * dofs you need to supply as fad variables, so you then call ReInit with these
 * fad variables.  Under the hood, this is the point where the object initializes
 * the SField with its objects and fads.
 * 
 * You must manage the fad variables, i.e. assigned the fad.diff(#,#).  In this way,
 * you can create several MEFields and utilize them as fads.
 * 
 * @ingroup field
 */
template <>
class MEField<SField> : public MEFieldBase {

public:
typedef SField field_type;
friend class FieldReg;

/* Construct from a regular MEField<>.  Creates all necessary
 * subfields, etc...
 */
MEField(MEField<_field> &rhs);

~MEField();

/**
 * Tell field the elements to setup on.  This function will count
 * the number of dofs you need to supply and return this.
 */

template<typename obj_iterator>
UInt AssignElements(obj_iterator ebegin, obj_iterator eend) {
  
  cur_elems.clear();
  std::copy(ebegin, eend, std::back_inserter(cur_elems));
  
  return do_assign_elements(cur_elems);
}

UInt AssignElement(MeshObj &elem);

/**
 * Sets up the fad variables on the low level fields.  This also sets the values
 * of the fad variables to the values on the field.
 */
void ReInit(fad_type *dof_buffer);

// Return number of low level fields
UInt Numfields() const;

// Get field for a specified number of dofs
SField *Getfield(UInt nval) const;

// ********* Some tools to make this act as a nodal field (if it is) ******
// Return the nodal field 
SField *GetNodalfield() const {
  ThrowAssert(mef.is_nodal() && Numfields() == 1);
  return primaryfield;
}
SField *GetElementfield() const {
  ThrowAssert(mef.is_elemental() && Numfields() == 1);
  return primaryfield;
}
// Shortcut for above
SField &operator()() const {
  ThrowAssert((mef.is_nodal() || mef.is_elemental()) && Numfields() == 1);
  return *primaryfield;
}

// Return data of nodal field.
_fieldValuePtr data(const MeshObj &obj) const {
  return this->operator()().data(obj);
}

/**
 * Iterate the dofs as MEField has laid of the fad_variable.  For
 * each entry, call:
 * dact(obj, nvalset, n)
 * where 0 <= n < nvalset*fdim.
 * It is up to the user to decide how to interpret n (nvalset,fdim) or (fdim, nvalset)
 */
template <typename dof_action>
void dof_iterator(dof_action &dact);

// Call fad.diff(#, total_dofs) for the doflist.  Number these in order
// with the master element coeffs, with (fdim, nfunc)
// Start indexing at offset
// Only works for the one element case.
void set_diff(UInt offset, UInt total_dofs);

private:
  
// Return the index into SField vector of the nval field.
UInt get_field_index(UInt nval) const;

UInt do_assign_elements(std::vector<MeshObj*> &elems);

std::vector<UInt> fidx_table;
std::vector<SField*> fields;
std::vector<std::set<MeshObj*> > field_objs;
SField *primaryfield; // if nodal or elemental
std::vector<MeshObj*> cur_elems;
MEField<_field> &f;
fad_type *dof_buffer;
};

template <typename dof_action>
void MEField<SField>::dof_iterator(dof_action &dact) {
  // Assign the dof buffer to each SField
  
  for (UInt i = 0; i < fields.size(); i++) {
    
    std::set<MeshObj*> &fs = field_objs[i];
    
    UInt nvalset = fidx_table[i];
    
    SField &sf = *fields[i];
   
    std::set<MeshObj*>::iterator si = fs.begin(), se = fs.end();
    
    for (; si != se; ++si) {
      
      UInt lim = nvalset*f.dim();
      
      for (UInt n = 0; n < lim; ++n) {
        
        dact(*si, nvalset, n);
        
      } // dofs on object
      
    } // objects
      
  } // fields
}

} // namespace


#endif
