// $Id: ESMC_MeshObjPack.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
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

#ifndef ESMC_MeshObjPack_h
#define ESMC_MeshObjPack_h

#include <ESMC_MeshObj.h>
#include <ESMC_CommRel.h>
#include <ESMC_Kernel.h>
#include <ESMC_Context.h>
#include <ESMC_SparseMsg.h>
#include <ESMC_MeshDB.h>

#include <cstddef>

namespace ESMCI {
namespace MESH {

// Pack a meshobject, or determine its packing size
UInt MeshObjPackSize(MeshObj &obj);

// If ghosting = true, then the object attr will be modified so it is sent
// as shared an not locally owned, else attr is copied directly.
void MeshObjPack(SparseMsg::buffer &b, MeshObj &obj, bool ghosting = false);

// Unpack the object, find the relations, etc..., insert into subrosters, and so on.
// News the object as well (better encapsulation of MeshObjData)
void MeshObjUnpack(MeshDB &mesh, SparseMsg::buffer &b, MeshObj *&obj);


// *** Context ***
template<>
class SparsePack<Context> {
public:
  explicit SparsePack(SparseMsg::buffer &b, const Context &c) {
    b.push((UChar*)&c.bits[0], size());
  }
  static UInt size() {
    return NUM_CONTEXT_CHARS;
  }
};

template<>
class SparseUnpack<Context> {
public:
  explicit SparseUnpack(SparseMsg::buffer &b, Context &c) {
    b.pop((UChar*)&c.bits[0], SparsePack<Context>::size());
  }
};

// *** Relation ***
template<>
class SparsePack<MeshObj::Relation> {
public:
  SparsePack(SparseMsg::buffer &b, const MeshObj::Relation &r) {
    SparsePack<MeshObj::id_type>(b, r.obj->get_id());
    SparsePack<MeshObj::Relation::ordinal_type>(b, r.ordinal);
    SparsePack<MeshObj::Relation::polarity_type>(b, r.polarity);
    SparsePack<MeshObj::Relation::type_type>(b, r.type);
    SparsePack<MeshObj::Relation::rotation_type>(b, r.rotation);
    SparsePack<UInt>(b, r.obj->get_type());
  }
  static UInt size() {
    return
      SparsePack<MeshObj::id_type>::size() + // obj id
      SparsePack<MeshObj::Relation::ordinal_type>::size() + 
      SparsePack<MeshObj::Relation::polarity_type>::size() + 
      SparsePack<MeshObj::Relation::type_type>::size() + 
      SparsePack<MeshObj::Relation::rotation_type>::size() +
      SparsePack<UInt>::size(); // object type (must have to look up object)
      ;
  }
};

template<>
class SparseUnpack<MeshObj::Relation> {
public:
  // unpack obj_id into variable, since we can't get the object.
  SparseUnpack(SparseMsg::buffer &b, MeshObj::Relation &r, MeshObj::id_type &obj_id, UInt &obj_type) {
    r.obj = NULL;
    SparseUnpack<MeshObj::id_type>(b, obj_id);
    SparseUnpack<MeshObj::Relation::ordinal_type>(b, r.ordinal);
    SparseUnpack<MeshObj::Relation::polarity_type>(b, r.polarity);
    SparseUnpack<MeshObj::Relation::type_type>(b, r.type);
    SparseUnpack<MeshObj::Relation::rotation_type>(b, r.rotation);
    SparseUnpack<UInt>(b, obj_type);
  }
};

// *** Attr ***
template<>
class SparsePack<Attr> {
public:
  SparsePack(SparseMsg::buffer &b, const Attr &a, bool ghosting = false) {
    // If ghosting, then object will be shared and not locally owned.
    SparsePack<UInt>(b, a.get_type()); // type
    SparsePack<UInt>(b, a.get_key()); // blockKey
    SparsePack<Context>(b, a.get_context()); // context
  }
  static UInt size() {
    return
         SparsePack<UInt>::size() + // type
         SparsePack<UInt>::size() + // blockKey
         SparsePack<Context>::size(); // context
  }
};

template<>
class SparseUnpack<Attr> {
public:
  SparseUnpack(SparseMsg::buffer &b, Attr &a) {
    SparseUnpack<UInt>(b, a.type); // type
    SparseUnpack<UInt>(b, a.globalKey); // blockKey
    SparseUnpack<Context>(b, a.context); // context
  }
};

} // namespace
} // namespace

#endif
