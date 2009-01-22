// $Id: ESMC_Meshfield.C,v 1.2.2.1 2009/01/22 04:52:40 theurich Exp $
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
#include <ESMC_Meshfield.h>


namespace ESMCI {
namespace MESH {


// ******** type holders *********

template<typename SCALAR>
_fieldTypeBase *_fieldType<SCALAR>::classInstance = NULL;

template<typename SCALAR>
const _fieldTypeBase &_fieldType<SCALAR>::instance() {
  if (classInstance == NULL) {
    classInstance = new _fieldType();
  }
  return *classInstance;
}

template<typename SCALAR>
_fieldType<SCALAR>::_fieldType() :
_fieldTypeBase(),
ti(typeid(SCALAR))
{
}

template<typename SCALAR>
_fieldType<SCALAR>::~_fieldType() {
}

template<typename SCALAR>
void _fieldType<SCALAR>::non_virtual_func() {
}

template<typename SCALAR>
std::size_t _fieldType<SCALAR>::size() const {
  return sizeof(SCALAR);
}

template class _fieldType<char>;
template class _fieldType<long>;
template class _fieldType<int>;
template class _fieldType<UChar>;
template class _fieldType<float>;
template class _fieldType<double>;


// ******* low level fields *********
static UInt round_to_dword(UInt size) {
  UInt dwsz = sizeof(void*)*4;
  UInt rm = size % dwsz;
  return rm ? size + (dwsz - rm) : size;
}


_field::_field(const std::string &name, const Attr &attr, const _fieldTypeBase &_ftype, UInt dim) :
fname(name),
my_attr(attr),
fdim(dim),
ordinal(-1),
stride(0),
ti(_ftype.type())
{
  stride = fdim*_ftype.size();
}

bool _field::OnObj(const MeshObj &obj) const {
  // Quickest check:  Just look at the offset
  const std::pair<_fieldStore*,UInt> &st = obj.GetStore();
  UChar *offset = st.first->Offset(ordinal);
  return offset != NULL;
}


// ******* Field storage **********

_fieldStore::_fieldStore() :
offsets(NULL),
offsize(0),
next_free(0),
free_stride(0),
data(NULL),
is_committed(false),
nfield(0)
{
  if (NOBJS > 254) Throw() << "Max NOBJS=254, due to using UCHAR for next free";
}

_fieldStore::~_fieldStore() {
  delete [] data;
}

void _fieldStore::Create(UInt nfields, _field **fields, const Attr &attr) {

  nfield = nfields;

  // Loop fields; if field lives here, add storage.
  std::size_t bufSize = 0;

  // store offsets at beginning of data block
  offsize = round_to_dword(sizeof(UChar*)*nfields);
  bufSize += offsize;

  // first a sizing pass
  int first_field = -1;
  int fieldsOnKer = 0;
  for (UInt f = 0; f < nfields; f++) {
    // Make sure field[0] is at least as big as short,
    // since we will use it to store next free table
    _field &fd = *fields[f];
    if (fd.GetAttr().any(attr)) {
      fieldsOnKer++;
      if (first_field < 0) first_field = f;
      bufSize += round_to_dword(NOBJS*fd.Stride());
    }
  }

  // make sure we allocate at least enough data
  // to store the next_free list
  UInt min_size = round_to_dword(offsize+sizeof(UChar)*NOBJS);
  bufSize = bufSize < min_size ? min_size : bufSize;

  // allocate
  data = new UChar[bufSize];
  UChar *end = data + bufSize;
  std::fill(data, end, (UChar) 0);
  offsets = reinterpret_cast<UChar**>(data);

  bufSize = offsize; // use as an offset counter
  for (UInt f = 0; f < nfields; f++) {
    _field &fd = *fields[f];
    offsets[f] = &data[bufSize];
    if (fd.GetAttr().any(attr)) {
      bufSize += round_to_dword(NOBJS*fd.Stride());
    } else 
      offsets[f] = NULL;
  }

  free_stride = (fieldsOnKer == 0 ? sizeof(UChar) : fields[first_field]->Stride());
/*
if (fieldsOnKer) std::cout << "field first:" << fields[first_field]->name() << " stride:" << fields[first_field]->Stride() << std::endl;
std::cout << "free_stride def=" << free_stride << std::endl;
*/
  // set up next free table
  for (UChar i = 0; i < NOBJS-1; i++) {
    data[offsize+i*free_stride] = i+1;
  }
  data[offsize+(NOBJS-1)*free_stride] = TABLE_FULL; // end of free

  is_committed = true;

}

void _fieldStore::remove(UInt idx) {
  data[offsize+idx*free_stride] = next_free;
  next_free = idx;
}

UInt _fieldStore::insert() {
  ThrowRequire(is_committed);
  if (next_free == TABLE_FULL)
    Throw() << "Inserting into full freeStore!!";
  UInt idx = next_free;
//std::cout << "next_free=" << next_free << " fs:" << free_stride << std::endl;
  next_free = data[offsize+next_free*free_stride];

  return idx;
}

void _fieldStore::CopyIn(UInt nfields, _field **fields, MeshObj &obj, UInt idx) {
  for (UInt f = 0; f < nfields; f++) {
    UChar *newloc = Offset(f);
    if (newloc == NULL) continue;
    const UInt stride = fields[f]->Stride();
    newloc += idx*stride; // move to correct object
    UChar *oldloc = obj.GetStore().first->Offset(f);
    int oldidx = obj.GetStore().second;
    if (oldloc == NULL) {
      // zero out data
      std::fill(newloc, newloc + stride, 0);
      continue;
    }
    oldloc += oldidx*stride;
    std::copy(oldloc, oldloc + stride, newloc);
  }
}

void _fieldStore::Zero(UInt nfields, _field **fields, UInt idx) {
  for (UInt f = 0; f < nfields; f++) {
    UChar *newloc = Offset(f);
    if (newloc == NULL) continue;
    const UInt stride = fields[f]->Stride();
    newloc += idx*stride;
    std::fill(newloc, newloc + stride, 0);
  }
}

double _fieldStore::FillRatio() const {

  // Traverse the free list to see how many slots left;
  UInt left = 0;
  
  UChar next = next_free;
  while (next != TABLE_FULL) {
    next = data[offsize+next*free_stride];
    left++;
  }
  return (double) (NOBJS - left) / NOBJS;
}

} // namespace
} // namespace
