// $Id: ESMC_SmallAlloc.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_SmallAlloc.h>

#include <ESMC_Exception.h>

namespace ESMCI {
namespace MESH {



// ******* Chunk *******

Chunk::Chunk(std::size_t blockSize, UInt _blocks) :
_data(NULL),
_end_data(NULL),
next_free(-1),
bsize(blockSize),
o_bsize(blockSize),
blocks(_blocks)
{

  //bsize = round_to_word(blockSize);
  
  // Make sure blockSize is at least int so that we can store
  // the free table there
  ThrowRequire(bsize >= sizeof(int));
  _data = new UChar[bsize*blocks];
  _end_data = &_data[bsize*blocks];

  // Loop through, setting up the next free table
  for (UInt i = 0; i < (UInt) (blocks-1); i++) {
    int *dptr = reinterpret_cast<int*>(&_data[i*bsize]);
    dptr[0] = i+1;
  }
  int *dptr = reinterpret_cast<int*>(&_data[(blocks-1)*bsize]);
  dptr[0] = -1; // end of free
  next_free = 0;
}

void *Chunk::Allocate(std::size_t blockSize) {
  ThrowRequire(blockSize == o_bsize);
  if (next_free == -1) return 0; // no room in chunk

  int *iptr = reinterpret_cast<int*>(&_data[next_free*bsize]);
  
  ThrowRequire(iptr >= reinterpret_cast<int*>(_data) &&
               iptr < reinterpret_cast<int*>(_end_data));
  next_free = *iptr;
  return static_cast<void*>(iptr);
}

bool Chunk::In(void *p) const {
  UChar *dp = static_cast<UChar*>(p);
  return (dp >= _data && dp < _end_data);
}

void Chunk::Deallocate(void *p, std::size_t blockSize) {
  ThrowRequire(blockSize == o_bsize);
  UChar *cptr = static_cast<UChar*>(p);
  // Verify alignment
  int oset = cptr-_data;
  ThrowRequire((oset % bsize) == 0);
  ThrowRequire(cptr >= _data && cptr < _end_data);

  // Zero the memory
  UChar *eptr = cptr + bsize;
  while (cptr != eptr) *cptr++ = 0;
  

  int *dptr = static_cast<int*>(p);
  dptr[0] = next_free; // point this block to next free
  next_free = oset / bsize; // next free points here
}

Chunk::~Chunk() {
  delete [] _data;
}


// ******* ObjPool *******

template <typename ObjType>
ObjPool<ObjType> *ObjPool<ObjType>::classInstance = NULL;

template <typename ObjType>
ObjPool<ObjType> *ObjPool<ObjType>::instance() {
  if (classInstance == NULL) {
    classInstance = new ObjPool();
  }
  return classInstance;
}

template<typename ObjType>
ObjPool<ObjType>::ObjPool() {
  // Start with a single chunk
  Chunk *ch = new Chunk(sizeof(ObjType), NOBJS_CHUNK);
  chunks.push_back(*ch);
}

template<typename ObjType>
ObjPool<ObjType>::~ObjPool() {
  ChunkList::iterator ci = chunks.begin(), ce = chunks.end(), cn;
  for (; ci != ce; ) {
    cn = ci; cn++;
    delete &*ci;
    ci = cn;
  }
}

template<typename ObjType>
void *ObjPool<ObjType>::Allocate(std::size_t blockSize) {
  ThrowRequire(blockSize == sizeof(ObjType));
  // Strategy: if a block is free, it is at front of list.
  {
    Chunk &ch = *chunks.begin();
   
    if (ch.Full()) {
      // Move chunk to end of list.  This allows any chunks with
      // space to 'bubble up' to the top of list.
      chunks.erase(ch);
      chunks.push_back(ch);
      // If there is space, it will be at front.  IF not, create
      // a new chunk.
      if (chunks.begin()->Full()) {
        // List must be full, so add a new block to front of list
        Chunk *nch = new Chunk(sizeof(ObjType), NOBJS_CHUNK);
        chunks.push_front(*nch);
      }
    }
  }
  // Now we must have space!!
  Chunk &ch = *chunks.begin();
  void *mem = ch.Allocate(blockSize);
  ThrowRequire(mem);

  // A last detail:  If we have filled the store, we must move
  // it to the back in case a delete pushes a free store in front of
  // us.  In this case, when it fills we will 'hide' the free blocks behind us.
  if (ch.Full()) {
    // Move store to end.  This allows possible nonfull stores to bubble up.
    //  If next store is full, then create a new store.
    chunks.erase(ch);
    chunks.push_back(ch);
  }

  return mem;
}

template<typename ObjType>
void ObjPool<ObjType>::Deallocate(void *p, std::size_t blockSize) {
  // We first must find which chunk this object is in
  ChunkList::iterator ci = chunks.begin(), ce = chunks.end();
  for (; ci != ce && !ci->In(p); ++ci) ;
  ThrowRequire(ci != ce);

  ci->Deallocate(p, blockSize);

  // Now put this chunk at beginning of list
  chunks.erase(*ci);
  chunks.push_front(*ci);
}


// ********* SmallObject **********
template<typename ObjType>
void *SmallObject<ObjType>::operator new(std::size_t size) {
  return ObjPool<ObjType>::instance()->Allocate(size);
}

template<typename ObjType>
void SmallObject<ObjType>::operator delete(void *p, std::size_t size) {
  ObjPool<ObjType>::instance()->Deallocate(p, size);
}

template<typename ObjType>
SmallObject<ObjType>::~SmallObject() {
}

} // namespace 
} // namespace 



// *********** Type instantiations **********

#include <ESMC_MeshObj.h>


namespace ESMCI {
namespace MESH {


template class ObjPool<MeshObj>;
template class SmallObject<MeshObj>;

} // namespace
} // namespace

