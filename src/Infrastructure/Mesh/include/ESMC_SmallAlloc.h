// $Id: ESMC_SmallAlloc.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_SmallAlloc_h
#define ESMC_SmallAlloc_h

#include <ESMC_MeshTypes.h>
#include <ESMC_List.h>

#include <cstddef>


// A class to efficiently store a large collection of uniformly sized
// small objects.

namespace ESMCI {
namespace MESH {

class Chunk : public ListNode<Chunk> {
public:
  Chunk(std::size_t blockSize, UInt blocks);
  ~Chunk();
  void *Allocate(std::size_t blockSize);
  void Deallocate(void *p, std::size_t blockSize);
  bool Full() const { return next_free == -1;}
  // Is the pointer in this Chunk?
  bool In(void *p) const;
private:
  UChar *_data; 
  UChar *_end_data;
  int next_free;
  std::size_t bsize;
  std::size_t o_bsize; // original blocksize
  UChar blocks;
};
 

typedef List<Chunk> ChunkList;

// A pool for a given object type
template <typename ObjType>
class ObjPool {
public:
static ObjPool *instance();
// Hardcode for all object types.  IF we want to make this object
// specific, just make it a static templated variable and specialize the
// cases.
static const UInt NOBJS_CHUNK = 500; // number objs per chunk

void *Allocate(std::size_t blockSize);

// Most expensive part:  Requires a search through the
// list of chunks to bracket the pointer.
void Deallocate(void *p, std::size_t blockSize);

private:
ObjPool();
~ObjPool();
ObjPool(const ObjPool &rhs);
ObjPool &operator=(const ObjPool &rhs);
static ObjPool *classInstance;
ChunkList chunks;
};

// To use the above, derive your object from this class, or just
// implement the operator new/delete below for your class.  This can
// be beneficial, since it avoids the instantiation of a vtable (below).
template <typename ObjType>
class SmallObject {
public:
  static void *operator new(std::size_t);
  static void operator delete(void *p, std::size_t);
  virtual ~SmallObject();
};

} // namespace
} // namespace

#endif
