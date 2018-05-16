// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Attr_h
#define ESMCI_Attr_h

#include <Mesh/include/Legacy/ESMCI_Context.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>

#include <ostream>

namespace ESMCI {

template <typename> class SparsePack;
template <typename> class SparseUnpack;

/**
 * Basic relational key for different types of mesh objects.  The attr holds a context,
 * the object type, and other attributes.
 * @ingroup meshdatabase
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
  static const char *reservedContextNames[];
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

  UInt GetType() const { return type;}
  UInt GetBlock() const { return globalKey;}

  Context &GetContext() { return context;}
  const Context &GetContext() const { return context;}
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

#endif
