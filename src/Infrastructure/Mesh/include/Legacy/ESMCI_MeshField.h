// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshField_h
#define ESMCI_MeshField_h

#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/Legacy/ESMCI_MEFamily.h>
#include <Mesh/include/Legacy/ESMCI_Context.h>
#include <Mesh/include/Legacy/ESMCI_MeshllField.h>

#include <string>
#include <map>

namespace ESMCI {

/**
 * A field that allows one to provide an object that acts on
 * a mesh object to return a value.  The provided class ACTION
 * must provide the dim and data functions, and a copy constructor.
 * @ingroup field
*/
template <typename ACTION>
class ActField {
public:
  ActField(const std::string &_name) :
  fname(_name) {}
  
  const std::string &name() { return fname;}
  UInt dim() { return a.dim(); }
  
  // Careful here: object returns a pointer to a's internal data, so
  // make sure a manages storage for the data
  typename ACTION::real_type *data(const MeshObj &obj) { return a.data(obj); }

  bool OnObj(const MeshObj &obj) { return a.OnObj(obj); }
private:
std::string fname;
ACTION a;
};

// Act like a field that returns owner
struct OwnerAction {
  typedef UInt real_type;
  UInt dim() { return 1; }
  real_type *data(const MeshObj &obj) {
    return const_cast<MeshObj&>(obj).get_owner_ptr();
  }
  bool OnObj(const MeshObj &) { return true;}
};

// Act like a field that returns data index
struct DataIndexAction {
  typedef MeshObj::DataIndexType real_type;
  UInt dim() { return 1; }
  real_type *data(const MeshObj &obj) {
    return const_cast<MeshObj&>(obj).get_data_index_ptr();
  }
  bool OnObj(const MeshObj &) { return true;}
};



/**
 * A field that can be used for sensitivities, replacing a Low Level Field.
 * @ingroup BaseField
*/
class SField {
public:
  typedef fad_type real_type;
  SField(const _field &field);
  ~SField();

  // clear the data.
  void Reset();
  // builds the fad array.  Also calls diff to set up ldof numbering
  template <typename obj_iter>
  void SetData(obj_iter obj_begin, obj_iter obj_end, fad_type *base);
  UInt dim() const { return fdim;}
  real_type *data(const MeshObj &obj) const;
  real_type *FadBase() { return fdata;}
  UInt NumFadObj() { return nobj;}
  MeshObj* Obj(UInt i) { ThrowRequire(i < nobj); return objs[i];}
  const std::string &name() const { return fname;}
  private:
  const _field &field;
  const std::string fname;
  UInt fdim;
  // Takes data index to local index.
  typedef std::map<const MeshObj*, UInt> DMapType;
  DMapType dmap;
  fad_type* fdata;
  UInt nobj;
  std::vector<MeshObj*> objs;
};

template<typename obj_iter>
void SField::SetData(obj_iter obj_begin, obj_iter obj_end,
      fad_type *base) {
  fdata = base;
  // First, how many objects?
  // Loop Objects.  Set variables and build index map.
  UInt i = 0;
  nobj = 0;
  for (obj_iter oi = obj_begin; oi != obj_end; ++oi) {
    nobj++;
    objs.push_back(*oi);
    const double *fd = field.data(**oi);
    //dmap[(*oi)->get_id()] = i; // access map
    dmap[*oi] = i; // access map
    for (UInt d = 0; d < fdim; d++) {
      // Initialize the fad vars;
      fdata[i] = fd[d];
      i++;
    }
  }
  
}

} // namespace

#endif
