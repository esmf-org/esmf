// $Id: ESMC_Context.C,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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
#include <ESMC_Context.h>
#include <ESMC_Exception.h>

#include <iostream>
#include <iomanip>


namespace ESMCI {
namespace MESH {

// Just in case someday a byte is not 8??
#define CSIZE 8

Context::Context() {
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) bits[i] = 0;
}

Context::Context(const Context &rhs) {
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) bits[i] = rhs.bits[i];
}

Context &Context::operator=(const Context &rhs) {
  if (this == &rhs) return *this;
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) bits[i] = rhs.bits[i];
  return *this;
}

bool Context::is_set(UInt bit) const {
  UInt byte = bit/CSIZE;
  UInt pos = bit % CSIZE;
  if (byte >= NUM_CONTEXT_CHARS)
    Throw() << "Context is_set:bit=" << bit << " too large!!"  
               << " Yields byte=" << byte << ", pos=" << pos;;
  return bits[byte] & ((0x01 << (CSIZE-1)) >> pos);
}

bool Context::operator==(const Context &rhs) const {
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) {
    if (bits[i] != rhs.bits[i]) return false;
  }
  return true;
}

Context &Context::operator|=(const Context &rhs) {
  if (this == &rhs) return *this;
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) bits[i] |= rhs.bits[i];
  return *this;
}

Context &Context::flip() {
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) {
    bits[i] = ~bits[i];
  }
  return *this;
}

bool Context::any(const Context &rhs) const {
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) {
    if (bits[i] & rhs.bits[i]) return true;
  }

  return false;
}

bool Context::operator<(const Context &rhs) const {
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) {
    if (bits[i] != rhs.bits[i]) {
      return bits[i] > rhs.bits[i];  // bits to left come first
    }
  }
  return false; // (they are equal)
}

UInt Context::nbits() const {
  return NUM_CONTEXT_CHARS*CSIZE;
}

UInt Context::NumBits() {
  return NUM_CONTEXT_CHARS*CSIZE;
}

void Context::set(UInt bit) {
  UInt byte = bit/CSIZE;
  UInt pos = bit % CSIZE;
  if (byte >= NUM_CONTEXT_CHARS)
    Throw() << "Context set:bit=" << bit << " too large!!"  
               << " Yields byte=" << byte << ", pos=" << pos;;
  bits[byte] |= ((0x01 << (CSIZE-1)) >> pos);
//std::cout << "set bit=" << bit << ", context=" << *this << std::endl;
}

void Context::clear(UInt bit) {
  UInt byte = bit/CSIZE;
  UInt pos = bit % CSIZE;
  if (byte >= NUM_CONTEXT_CHARS)
    Throw() << "Context set:bit=" << bit << " too large!!"  
               << " Yields byte=" << byte << ", pos=" << pos;;
  bits[byte] &= ~((0x01 << (CSIZE-1)) >> pos);
//std::cout << "set bit=" << bit << ", context=" << *this << std::endl;
}

void Context::clear() {
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) bits[i] = 0;
}

bool Context::subset(const Context &rhs) const {
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) {
    // If the union of the two sets is the rhs set,
    // then left is a subset.
    if ((bits[i] | rhs.bits[i]) != rhs.bits[i])
      return false;
  }
  return true;
}

std::ostream &operator<<(std::ostream &os, const Context &ctxt) {
  for (UInt i = 0; i < NUM_CONTEXT_CHARS; i++) {
    UChar c = ctxt.bits[i];
    UChar t = (0x01 << (CSIZE - 1));
    for (UInt j = 0; j < CSIZE; j++) {
      if (c & t) os << "1"; else os << "0";
//os << "t=" << (int) t << ", c=" << std::hex << (int) c << std::endl;
      t >>= 1;
    }
  }
  return os;
}

} // namespace
} // namespace
