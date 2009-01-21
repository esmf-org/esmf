// $Id: ESMC_MeshField.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MeshField_h
#define ESMC_MeshField_h

#include <ESMC_MeshTypes.h>
#include <ESMC_MeshObj.h>
#include <ESMC_MEFamily.h>
#include <ESMC_Context.h>
#include <ESMC_Meshfield.h>

#include <string>
#include <map>

namespace ESMCI {
namespace MESH {

/**
 * A field that allows one to provide an object that acts on
 * a mesh object to return a value.  The provided class ACTION
 * must provide the dim and data functions, and a copy constructor.
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

/**
 * A field that can be used for sensitivities.
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
  UInt NumFad() { return fdim*nobj;}
  const std::string &name() const { return fname;}
  private:
  const _field &field;
  const std::string fname;
  UInt fdim;
  // Takes data index to local index.
  typedef std::map<UInt, UInt> DMapType;
  DMapType dmap;
  fad_type* fdata;
  UInt nobj;
};

template<typename obj_iter>
void SField::SetData(obj_iter obj_begin, obj_iter obj_end,
      fad_type *base) {
  fdata = base;
  nobj = std::distance(obj_begin, obj_end);
  // First, how many objects?
  // Loop Objects.  Set variables and build index map.
  UInt i = 0;
  for (obj_iter oi = obj_begin; oi != obj_end; ++oi) {
    const double *fd = field.data(**oi);
    dmap[(*oi)->get_data_index()] = i; // access map
    for (UInt d = 0; d < fdim; d++) {
      // Initialize the fad vars;
      fdata[i] = fd[d];
      i++;
    }
  }
  
}

} // namespace
} // namespace

#endif
