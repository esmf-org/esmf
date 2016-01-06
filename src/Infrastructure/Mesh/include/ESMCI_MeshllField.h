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
#ifndef ESMCI_MeshllField_h
#define ESMCI_MeshllField_h

#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_Attr.h>
#include <Mesh/include/ESMCI_List.h>

#include <typeinfo>
#include <cstddef>
#include <string>


/**
 * @defgroup BaseField
 * 
 * A subsystem that implements a fixed size field on a given set of
 * mesh objects (described by an Attr).
 * 
 * @ingroup field
 */

namespace ESMCI {

class _field;

/**
 * The basic field storage object for a set of mesh objects that
 * all have the same fields present.  All fields for a small set of
 * objects of homogenous attribute are store in this object in an
 * array (actually a table) of values.
 * @ingroup BaseField
*/
class _fieldStore : public ListNode<_fieldStore> {
public:

// Number of objects to store
static const UInt NOBJS = 100;

// We can store at max 254 objects in table due to using
// a UChar as the next free index.
enum { TABLE_FULL = 255 };

// Pass the fields that live on the mesh and the context for
// the Kernel this store services.
_fieldStore();
~_fieldStore();

// Note: one should send the ENTIRE registrar of fields to EVERY
// store.  The store figures out whether or not the field will live
// here, but the offset table expects all fields.
void Create(UInt nfields, _field **fields, const Attr &attr);

// Copy the data for an object into this store.  The object is
// still pointing to the old store.  idx is the index in this store
// to place the object.
void CopyIn(UInt nfields, _field **fields, MeshObj &obj, UInt idx);

/*
 * Zero out a slot for an object.
 */
void Zero(UInt nfields, _field **fields, UInt idx);

bool Full() const { return next_free == TABLE_FULL; }
bool empty() const { return num_objs == 0; }

/*
 * Free a data slot referenced by idx.
 */
void remove(UInt idx); 

/*
 * Insert an object (return slot number) into the next free (should check
 * to make sure not full first)
 */
UInt insert();

UChar *Offset(UInt fnum) const { ThrowAssert(fnum < nfield && is_committed); return offsets[fnum];}

UChar *GetData() const { return data;}

/*
 * Return number of objects in store.
 */
UInt NumObjs() const { return num_objs; }

/* 
 * Return ration of num slots used / total num
 */
double FillRatio() const;

private:
// offset table for fields into storage
UChar **offsets;
UInt offsize;
UChar next_free;
UInt free_stride; // stride of first field
UChar *data;
bool is_committed;
UInt nfield;
UInt num_objs;
};

/**
 * A class to help encapsulate the C++ type a given field is.  This
 * class helps provide type safety in debug mode.
 * @ingroup BaseField
*/
class _fieldTypeBase {
public:
  _fieldTypeBase() {}
  _fieldTypeBase(const _fieldTypeBase &rhs) {}
  virtual ~_fieldTypeBase() {}
  virtual std::size_t size() const = 0;
  virtual const std::type_info &type() const = 0;
};
/**
 * Provide an easy template style interface to the base
 * @ingroup BaseField
 */
template<typename SCALAR>
class _fieldType : public _fieldTypeBase {
public:
  ~_fieldType();
  void non_virtual_func();
  std::size_t size() const;
  const std::type_info &type() const { return ti;}
  static const _fieldTypeBase &instance();
private:
  _fieldType(const _fieldType &rhs) : ti(rhs.ti) {}
  static _fieldTypeBase *classInstance;
  _fieldType();
  const std::type_info &ti;
};

/**
 * Object that facilitates casting a block of memory to a type pointer.
 * Provides for type checking in debug mode, and fast casting in optimized.
 * @ingroup BaseField
*/
class _fieldValuePtr {
public:

#ifdef NDEBUG
_fieldValuePtr(void *dptr, const std::type_info &) : data(dptr) {}
_fieldValuePtr(const _fieldValuePtr &rhs) : data(rhs.data) {}

// Constructors from  specific types
_fieldValuePtr(UChar *dptr) : data(dptr){}
_fieldValuePtr(char *dptr) : data(dptr){}
_fieldValuePtr(int *dptr) : data(dptr) {}
_fieldValuePtr(long *dptr) : data(dptr) {}
_fieldValuePtr(float *dptr) : data(dptr) {}
_fieldValuePtr(double *dptr) : data(dptr) {}
_fieldValuePtr(fad_type *dptr) : data(dptr) {}
#else
_fieldValuePtr(void *dptr, const std::type_info &_ti) : data(dptr), ti(_ti) {}
_fieldValuePtr(const _fieldValuePtr &rhs) : data(rhs.data), ti(rhs.ti) {}

// Constructors from  specific types
_fieldValuePtr(UChar *dptr) : data(dptr), ti(typeid(UChar)) {}
_fieldValuePtr(char *dptr) : data(dptr), ti(typeid(char)) {}
_fieldValuePtr(int *dptr) : data(dptr), ti(typeid(int)) {}
_fieldValuePtr(long *dptr) : data(dptr), ti(typeid(long)) {}
_fieldValuePtr(float *dptr) : data(dptr), ti(typeid(float)) {}
_fieldValuePtr(double *dptr) : data(dptr), ti(typeid(double)) {}
_fieldValuePtr(fad_type *dptr) : data(dptr), ti(typeid(fad_type)) {}
#endif

// Cast to pointer
operator UChar*() const { ThrowAssert(ti == typeid(UChar)); return static_cast<UChar*>(data); }
operator char*() const { ThrowAssert(ti == typeid(char)); return static_cast<char*>(data); }
operator int*() const { ThrowAssert(ti == typeid(int)); return static_cast<int*>(data); }
operator long*() const { ThrowAssert(ti == typeid(long)); return static_cast<long*>(data); }
operator float*() const { ThrowAssert(ti == typeid(float)); return static_cast<float*>(data); }
operator double*() const { ThrowAssert(ti == typeid(double)); return static_cast<double*>(data); }
operator fad_type*() const { ThrowAssert(ti == typeid(fad_type)); return static_cast<fad_type*>(data); }

// Cast to reference
operator UChar&() const { ThrowAssert(ti == typeid(UChar)); return *static_cast<UChar*>(data); }
operator char&() const { ThrowAssert(ti == typeid(char)); return *static_cast<char*>(data); }
operator int&() const { ThrowAssert(ti == typeid(int)); return *static_cast<int*>(data); }
operator long&() const { ThrowAssert(ti == typeid(long)); return *static_cast<long*>(data); }
operator float&() const { ThrowAssert(ti == typeid(float)); return *static_cast<float*>(data); }
operator double&() const { ThrowAssert(ti == typeid(double)); return *static_cast<double*>(data); }
operator fad_type&() const { ThrowAssert(ti == typeid(fad_type)); return *static_cast<fad_type*>(data); }

private:
void *data;
#ifdef NDEBUG
#else
const std::type_info &ti;
#endif
};

/**
 * Basic class representing a BaseField that lives on some set of
 * mesh objects.
 * This field lives on the union of subsets described by a context value.
 * i.e. if the context is 1000111, the field will life on any kernel that
 * has any one of these bits set.
 * @ingroup BaseField
*/
class _field {
public:
_field(const std::string &name, const Attr &attr, const _fieldTypeBase &_ftype, UInt dim); 
const std::string &name() const { return fname;}
int GetOrdinal() { return ordinal; }
const Attr &GetAttr() const { return my_attr;}
UInt dim() const { return fdim; }

_fieldValuePtr data(const MeshObj &obj) const {
  const std::pair<_fieldStore*,UInt> &st = obj.GetStore();
  UChar *offset = st.first->Offset(ordinal);
  return offset ? 
      _fieldValuePtr(
      &offset[st.second*stride], ti)
       : _fieldValuePtr(NULL, ti);
}

UInt Stride() const { return stride;}

// Return true if the field is on this object, else false
bool OnObj(const MeshObj &obj) const;

void set_ordinal(UInt ord) { ordinal = ord;}

const std::type_info &tinfo() const { return ti;}
private:
std::string fname;
Attr my_attr;
UInt fdim;
int ordinal; // ordinal in field registry
UInt stride; // fdim*sizeof(T) --save to avoid recomputing
const std::type_info &ti;
};



} // namespace


#endif
