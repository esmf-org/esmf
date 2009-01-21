// $Id: ESMC_MeshObjPack.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshObjPack.h>
#include <ESMC_ParEnv.h>
#include <ESMC_MeshObjConn.h>


namespace ESMCI {
namespace MESH {

UInt MeshObjPackSize(MeshObj &obj) {
  
  UInt sz = 0;
  // Obj id;
  sz += SparsePack<MeshObj::id_type>::size();

  // Obj topo
  sz += SparsePack<UInt>::size();

  // Obj owner
  sz += SparsePack<UInt>::size();

  // Obj Attr
  sz += SparsePack<Attr>::size();

  // Size of relations
  sz += SparsePack<UInt>::size();

  // Relations 
  MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();
  for (; ri != re; ++ri) {
    sz += SparsePack<MeshObj::Relation>::size();
  }

  return sz;
}

void MeshObjPack(SparseMsg::buffer &b, MeshObj &obj, bool ghosting) {

  // Obj id;
  SparsePack<MeshObj::id_type>(b, obj.get_id());
//Par::Out() << "Packingn id=" << obj.get_id() << std::endl;

  // Obj topo
  const MeshObjTopo *topo = GetMeshObjTopo(obj);
  UInt tnum = topo ? topo->number : 0;
  SparsePack<UInt>(b, tnum);
//Par::Out() << "Packingn tnum=" << tnum << std::endl;

  // Pack owner
  UInt owner = obj.get_owner();
  SparsePack<UInt>(b, owner);

  // Obj Attr
  SparsePack<Attr>(b, GetAttr(obj));

  // Pack Relations size
  MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();
  UInt nrel = obj.Relations.size();
  SparsePack<UInt>(b, nrel);

  // Pack Relations size
  ri = obj.Relations.begin();
  for (; ri != re; ++ri) {
    SparsePack<MeshObj::Relation>(b, *ri);
  }

}

void MeshObjUnpack(MeshDB &mesh, SparseMsg::buffer &b, MeshObj *&obj) {
  Trace __trace("MeshObjUnpack(MeshDB &mesh, SparseMsg::buffer &b, MeshObj *&obj)");

  // Obj id;
  MeshObj::id_type id;
  SparseUnpack<MeshObj::id_type>(b, id);
//Par::Out() << "Unpack id=" << id << std::endl;

  // Obj topo
  UInt tnum;
  SparseUnpack<UInt>(b, tnum);
//Par::Out() << "Unpack tnum=" << tnum << std::endl;
  const MeshObjTopo *topo = GetTopo(tnum);

  // Obj owner
  UInt owner;
  SparseUnpack<UInt>(b, owner);

  Attr attr;
  // Obj Attr
  SparseUnpack<Attr>(b, attr);

  // First, See if the object is already there:
  bool obj_exists = false;
  MeshDB::MeshObjIDMap::iterator mi =
       mesh.map_find(attr.get_type(), id);

  if (mi != mesh.map_end(attr.get_type())) {
    obj_exists = true;
    // TODO: check that everything is the same
  }

  // We now have enough info to create the meshobj
  if (!obj_exists) {
    obj = new MeshObj(attr.get_type(), id, 0);
    obj->set_owner(owner);
  } else obj = &*mi;

  // Relations
  // unpack all relations
  UInt nrel;
  SparseUnpack<UInt>(b, nrel);

  // If the object exists, just unpack the buffer; add any (potentially)
  // new relations.
  if (obj_exists) {

    // May need to update owner.  Any diagnostic check???
/*
Par::Out() << "obj " << MeshObjTypeString(obj->get_type()) << " " << obj->get_id() << " old owner:"
    << obj->get_owner() << " -> " << owner << std::endl;
*/
    obj->set_owner(owner);

    for (UInt n = 0; n < nrel; n++) {
      MeshObj::Relation r;
      MeshObj::id_type obj_id;
      UInt obj_type;
      SparseUnpack<MeshObj::Relation>(b, r, obj_id, obj_type);
      MeshObjRelationList::iterator ri =
        MeshObjConn::find_relation(*obj, obj_type, r.ordinal, r.type);
 
      // Might be a similar relation with to different obj (think USED_BY, ord 1)
      if (ri != obj->Relations.end() && ri->obj->get_id() == obj_id) {
      } else {
        // Relation doesn't exist, so try to add it.
        MeshDB::MeshObjIDMap::iterator mi =
           mesh.map_find(obj_type, obj_id);
        if (mi != mesh.map_end(obj_type)) {
          // Got the object!
          MeshObj &robj = *mi;
          r.obj = &robj;
          AddMeshObjRelation(*obj, r);
          // Add the converse relation to other object
          if ((r.type = MeshObjRelationConverse(r.type)) != 0) {
            r.obj = obj;
            AddMeshObjRelation(robj, r);
          }
        } // else obj not there, so leave well enough as well is.
      }
    }

  } else {
    obj->Relations.reserve(nrel);
    for (UInt n = 0; n < nrel; n++) {
      obj->Relations.push_back(MeshObj::Relation());
      MeshObj::Relation &r = obj->Relations.back();
      MeshObj::id_type obj_id;
      UInt obj_type;
      SparseUnpack<MeshObj::Relation>(b, r, obj_id, obj_type);
      // Now go to mesh and recruit the obj_id fail if not found.
      MeshDB::MeshObjIDMap::iterator mi =
         mesh.map_find(obj_type, obj_id);
      if (mi == mesh.map_end(obj_type)) {
        obj->Relations.pop_back();
#ifdef NOT
        // TODO: figure out a strategy of when to/not to forgive this
        //if (obj_type != (UInt) mesh.side_type())
        if (1)
          Throw() << "P:" << Par::Rank() << " mesh =" << mesh.filename() << " unpack MeshObj:id=" << id << ",attr=" << attr << ", could not find object (type,id)="
            << MeshObjTypeString(obj_type) << ", " << obj_id << ", ordinal:" << r.ordinal  
            << ", Meshside type=" << MeshObjTypeString(mesh.side_type())
            << std::endl;
#endif
      }
      // Else drop the object into the relation
      r.obj = &*mi;
    }
  
    // Now add object to Mesh
//Par::Out() << "Unpacked object:" << MeshObjTypeString(obj->get_type()) << ", id=" << obj->get_id() << std::endl;
    mesh.add_object(obj, attr, topo);
  } // if !exists

  // Make the locally owned consistent.
  {
    const Attr &oattr = GetAttr(*obj);
    const Context &ctxt = GetMeshObjContext(*obj);
    Context newctxt(ctxt);
    if (obj->get_owner() == Par::Rank())
      newctxt.set(Attr::OWNED_ID); 
    else
      newctxt.clear(Attr::OWNED_ID);
    if (newctxt != ctxt) {
      Attr attr(oattr, newctxt);
      mesh.update_obj(obj, attr);
    }
  }

}

} // namespace DVD
} // namespace DVD
