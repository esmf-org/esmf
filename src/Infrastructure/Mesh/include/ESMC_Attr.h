// $Id: ESMC_Attr.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_Attr_h
#define ESMC_Attr_h

#include <ESMC_Context.h>
#include <ESMC_MeshObj.h>

#include <ostream>

namespace ESMCI {
namespace MESH {

template <typename> class SparsePack;
template <typename> class SparseUnpack;

/**
 * Basic key for different types of mesh objects.  The attr holds a context,
 * the object type, and other attributes.
 */
class Attr {
public:
  enum {
       ACTIVE_ID = 0,
       OWNED_ID,
       SHARED_ID,
       EXPOSED_BOUNDARY_ID,
       PENDING_CREATE_ID,
       PENDING_DELETE_ID,
       GENESIS_ID,
       REFINED_ID,
       CONSTRAINED_ID
     };
  static char *reservedContextNames[];
  static const UInt numReservedContexts = 9;


  Attr() {
     // default these.  Note, the default iterators
     type = MeshObj::ANY;
     context.set(ACTIVE_ID);
  }
  Attr(UInt _type, const Context &_context) :
    type(_type), globalKey(0), context(_context)
  {
  }
  
  Attr(UInt _type, UInt _globalKey, const Context &_context = Context(),
    bool _shared = false, bool _locally_owned = true, bool _active = true, bool _genesis = true) :
    type(_type), globalKey(_globalKey), context(_context)
   {
     if (_shared) context.set(SHARED_ID);
     if (_locally_owned) context.set(OWNED_ID);
     if (_active) context.set(ACTIVE_ID);
     if (_genesis) context.set(GENESIS_ID);
   }
  Attr(const Attr &rhs) :
    type(rhs.type), globalKey(rhs.globalKey), context(rhs.context)
  {}
  Attr &operator=(const Attr &rhs) {
    if (this == &rhs) return *this;
    type = rhs.type;
    globalKey = rhs.globalKey;
    context = rhs.context;
    return *this;
  }

  // Convenience constructor for new context
  Attr(const Attr &rhs, const Context &ctxt) :
    type(rhs.type), globalKey(rhs.globalKey), context(ctxt)
  {}

  // true < false
  bool bool_comp(bool l, bool r) const {
    return l == r ? false :
      l;
  }

  bool operator<(const Attr &rhs) const {
    if (type != rhs.type) return type < rhs.type;
    if (globalKey != rhs.globalKey) return globalKey < rhs.globalKey;
    return context < rhs.context;
  }
  bool operator==(const Attr &rhs) const {
    return (type == rhs.type && globalKey == rhs.globalKey
          && context == rhs.context
       );
  }
  bool operator!=(const Attr &rhs) const {
    return !(*this == rhs);
  }

  // True if this attr is a "subset" of the rhs attr,
  // which means the MeshObjType is a of this matches
  // at least one of the rhs types, and that
  // the context is a subset of the other.
  // globalKey is NOT used in this comparison, since it
  // only exists to keep the mesh sorted by the original block
  // sideset and nodesets.
  bool subset(const Attr &rhs) const;

  // return true if the two attrs share a common meshobj type
  // any share any bit.
  bool any(const Attr &rhs) const;
  // operator= (use standard)

  UInt get_type() const { return type;}
  UInt get_key() const { return globalKey;}
  Context &get_context() { return context;}
  const Context &get_context() const { return context;}
  bool is_shared() const { return context.is_set(SHARED_ID); }
  bool is_locally_owned() const { return context.is_set(OWNED_ID);}
  friend std::ostream &operator<<(std::ostream &os, const Attr &attr);

private:
  UInt type; // MeshObjType
  UInt globalKey; // global block/sideset/nodeset #
  Context context;

  friend class SparsePack<Attr>;
  friend class SparseUnpack<Attr>;

};

std::ostream &operator<<(std::ostream &os, const Attr &attr);


} // namespace
} // namespace

#endif
