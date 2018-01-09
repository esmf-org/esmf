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
#ifndef ESMCI_Context_h
#define ESMCI_Context_h

#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_MeshContext.h>

#include <ostream>

namespace ESMCI {

template <typename> class SparsePack;
template <typename> class SparseUnpack;

/**
 * A bit set class.  Each bit represents a subset of mesh objects.
 * If a given bit is set, then the object with this context is
 * in the given set.
*/
class Context {
public:
Context();
Context(const Context &rhs);
Context &operator=(const Context &rhs);

bool operator<(const Context &rhs) const;
bool operator==(const Context &rhs) const;
bool operator!=(const Context &rhs) const {
  return !(*this == rhs);
}
// Set the give bit to true
void set(UInt bit);

Context &operator|=(const Context &rhs);

// Clear the given bit
void clear(UInt bit);

// clear all bits
void clear();

// True if bit set
bool is_set(UInt bit) const;

// How many bits in the context??
UInt nbits() const;

// return true if any bits in common
bool any(const Context &rhs) const;

// reverse bits
Context &flip();

// Return true if every bit set in this is also set in rhs.
bool subset(const Context &rhs) const;

static UInt NumBits();
private:
  UChar bits[NUM_CONTEXT_CHARS];
  friend std::ostream &operator<<(std::ostream &os, const Context &ctxt);
  friend class SparsePack<Context>;
  friend class SparseUnpack<Context>;
};

std::ostream &operator<<(std::ostream &os, const Context &ctxt);

} // namespace

#endif
