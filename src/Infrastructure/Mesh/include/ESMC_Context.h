// $Id: ESMC_Context.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_Context_h
#define ESMC_Context_h

#include <ESMC_MeshTypes.h>
#include <ESMC_MeshContext.h>

#include <ostream>

namespace ESMCI {
namespace MESH {

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
} // namespace

#endif
