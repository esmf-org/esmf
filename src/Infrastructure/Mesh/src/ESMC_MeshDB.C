// $Id: ESMC_MeshDB.C,v 1.3.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshDB.h>

#include <ESMC_MeshObjTopo.h>
#include <ESMC_Exception.h>
#include <ESMC_MeshObjConn.h>
#include <ESMC_Mapping.h>
#include <ESMC_Quadrature.h>
#include <ESMC_MEField.h>
#include <ESMC_ParEnv.h>

#include <iterator>
#include <iostream>
#include <ostream>
#include <algorithm>

namespace ESMCI {
namespace MESH {

//#define DEL_DEBUG

// Some various sorts
bool MeshObjPtrSort(const MeshObj *lhs, const MeshObj*rhs) {
  return lhs->get_id() < rhs->get_id();
}

MeshDB::MeshDB() :
contexts(),
curContext(0),
setRoster(),
node_map(),
element_map(),
edge_map(),
face_map(),
spatial_dimension(0),
parametric_dimension(0),
max_mapping_data(0),
fname("NONE"),
committed(false),
use_sides(false),
Fields()
{
  // Set up context(s)
  for (UInt i = 0; i < Attr::numReservedContexts; i++) {
    DefineContext(Attr::reservedContextNames[i]);
  }
}


MeshDB::~MeshDB() {

 // First clear the maps, i.e. take objects out of maps
 node_map.clear();
 edge_map.clear();
 face_map.clear();
 element_map.clear();

 MeshDB::iterator oi = obj_begin(), oe = obj_end();

 while(oi != oe) {
   MeshDB::iterator on = oi;
   oi++;
   MeshObj *obj = &*on;
   delete obj;
 }

 KernelList::iterator si = set_begin(), se = set_end();

 while(si != se) {
   KernelList::iterator sn = si;
   si++;
   Kernel *st = &*sn;
   delete st;
 }

/*  
  {
    KernelList::iterator i = set_begin();
    if (i != set_end())
    do {
    
      // Kill the objects in this roster
      {
        MeshObjList::iterator j = i->objects.begin();
        if (j != i->objects.end())
        do {
          delete &*j;
          j = i->objects.begin();
        } while (j != i->objects.begin());
      }
      delete &*i;
      i = set_begin();
    } while (i != set_end());
  }
*/

}

void MeshDB::UseSides(bool val) {
  ThrowRequire(!committed);
  use_sides = val;
}

void MeshDB::Print(std::ostream &os, bool summary_only) const {

  if (!summary_only) {
    MeshDB::const_iterator oi = obj_begin_all(), oe = obj_end_all();
    for (; oi != oe; ++oi) {
      const MeshObj &obj = (*oi);
      os << obj;
      const MeshObjTopo *topo = GetMeshObjTopo(obj);
      os << "\tTopo:" << (topo ? topo->name : "NONE") << std::endl;
      //obj.printdata();
    }
  }

  os << "Contexts:";
  ContextMapType::const_iterator ci = contexts.begin(), ce = contexts.end();

  for (; ci != ce; ++ci) {
    os << "\t" << ci->first << "->" << ci->second << std::endl;
  }

  os << "Set map:" << std::endl;
  for (KernelList::const_iterator mi = set_begin(); mi != set_end(); mi++) {
    const Kernel *md = &(*mi);
    const MeshObjTopo *topo = md->topo;
    os << "Attr:" << md->GetAttr() << ", count:" << md->Count() << ", topo=" <<
        (topo ? topo->name : "NONE") << std::endl;
    //md->PrintStoreInfo();
    }
}


class md_attr_comp : public std::binary_function<Kernel,Attr,bool> {
public:
  md_attr_comp() {}
  bool operator()(const Kernel &l, const Attr &r) {
    return l.GetAttr() < r;
  }
};

void MeshDB::push_back_sorted(MeshObj &obj, const Attr &attr, const MeshObjTopo *topo) {
 
  //int ikey = obj.get_int(key);
  KernelList::iterator lb = std::lower_bound(setRoster.begin(), setRoster.end(), attr, md_attr_comp());
  Kernel *md;
 
  if (lb == setRoster.end() ||
      attr != lb->GetAttr()) {
    md = new Kernel(*this, attr);
    md->topo = topo;
    if (topo) {
      md->mapping = Topo2Map()(topo->name);
      md->irule = Topo2Intg()(2, topo->name); // default intg rule. Can swap order
    }
    setRoster.insert(lb, *md);

    // Commit the new kernel, if mesh is committed
    if (committed) md->Commit(Fields.size(), &Fields[0], fields.size(), &fields[0]);

//std::cout << "New Kernel, attr=" << attr << std::endl;
  } else md = &(*lb);

  // Objects with the same attr should share the topo.
  if (md->topo != topo) 
    Throw() << "New Kernel, topos don't match" <<
        (md->topo ? md->topo->name : "NULL") << 
        (topo ? topo->name : "NULL") << std::endl;

  // Add the object;
  md->AssimilateObject(obj);
}

void MeshDB::update_obj(MeshObj::id_type id, MeshObj *obj, const Attr &attr) {
  Trace __trace("MeshDB::update_obj(MeshObj::id_type id, MeshObj *obj, const Attr &attr)");
  // Reset the objidmap entry
  if (obj->get_id() != id) {
    MeshObjIDMap &map = get_map(obj->get_type());
    map.erase(obj);
    map.insert(*obj);
  }
  update_obj(obj, attr);
}

void MeshDB::update_obj(MeshObj *obj, const Attr &attr) {
  Trace __trace("MeshDB::update_obj(MeshObj *obj, const Attr &attr)");
  const Attr &oattr = GetAttr(*obj);
  if (attr == oattr) return; // nothing to do
  // Make sure object is not changing type.
  if (attr.get_type() != oattr.get_type())
    Throw() << "update_object, attr type changing:" << attr << " to " << oattr << std::endl;

  // Take object out of old list.
  Kernel *ker = obj->GetKernel();
  const MeshObjTopo *topo = GetMeshObjTopo(*obj);
  ker->erase(*obj); // doesn't erase data yet!

  // Reinsert the object in its new location.
  push_back_sorted(*obj, attr, topo);

}

void MeshDB::add_object(MeshObj *obj, const Attr &attr, const MeshObjTopo *topo) {
  Trace __trace("MeshDB::add_object(MeshObj *obj, const Attr &attr, const MeshObjTopo *topo)");
  // handle each case separately
  if (obj->get_type() != MeshObj::NODE && !topo) 
    Throw() << "add_object, required nonnull topo for" << MeshObjTypeString(obj->get_type());

  // Add reciprical relations
  {
    // Add used by for nodes, sides, edges
    MeshObjRelationList::iterator ri = obj->Relations.begin(),
      re = obj->Relations.end();
    for (; ri != re; ++ri) {
      UInt rec = MeshObjRelationConverse(ri->type);
      if (rec != 0) {
        // Add back relation to object
        MeshObj &sobj = *ri->obj;
        MeshObj::Relation rel = *ri; // start here (mean to copy)
        rel.obj = obj;
        rel.ordinal = ri->ordinal; // mirror ordinal
        rel.type = rec;
        AddMeshObjRelation(sobj, rel); 
      } 
    }
  }

  push_back_sorted(*obj, attr, topo);
  get_map(obj->get_type()).insert(*obj);
}

void MeshDB::add_element(MeshObj*elem, std::vector<MeshObj*> &nodevect, const Attr &attr,
  const MeshObjTopo *topo) {
  Trace __trace("MeshDB::add_element(MeshObj*elem, std::vector<MeshObj*> &nodevect, const Attr &attr, const MeshObjTopo *topo)");
  element_connect(*elem, nodevect);
  push_back_sorted(*elem, attr, topo);
  element_map.insert(*elem);

  elem->set_owner(Par::Rank()); // elems not shared
  //element_map[elem->get_id()] = elem;
}

void MeshDB::add_element(MeshObj*elem, std::vector<MeshObj*> &nodevect, UInt block,
               const MeshObjTopo *topo) {
  Attr attr(MeshObj::ELEMENT, block);
  add_element(elem, nodevect, attr, topo);
}

void MeshDB::add_node(MeshObj *node, const Attr &attr) {
  push_back_sorted(*node, attr,NULL);
  //node_map[node->get_id()] = node;
  node_map.insert(*node);
}

void MeshDB::add_node(MeshObj *node, UInt nodeSet)
{
   TraceBack __trace("MeshDB::add_node(MeshObj *node, UInt nodeSet)");
   Attr attr(MeshObj::NODE, nodeSet);
/*  Test setting up some contexts
static int cnt = 0;
Context c;
c.set((cnt++ / 100) % c.nbits());
Attr attr(MeshObj::NODE, nodeSet,c);
*/
   
   add_node(node, attr);
}

void MeshDB::add_node_local(MeshObj *node,
                    const MeshObjTopo *topo, // supporting topo
                    UInt ordinal,    // ordinal within supporting topo
                    std::vector<MeshObj*> &support,
                    const Attr &attr)
{
  TraceBack __trace("MeshDB::add_node_local(MeshObj *node, const MeshObjTopo *topo, // supporting topo UInt ordinal,    // ordinal within supporting topo std::vector<MeshObj*> &support, const Attr &attr)");

  ThrowRequire(ordinal >= topo->num_nodes); // only for child nodes

  push_back_sorted(*node, attr,NULL);
  //node_map[node->get_id()] = node;
  node_map.insert(*node);

  // Now connect up to relevant elements
  std::vector<MeshObj*> elems;
  // Get the list of elements that use these nodes
  MeshObjConn::common_objs(support.begin(), support.end(),
             MeshObj::USED_BY, MeshObj::ELEMENT, elems);

  ThrowRequire(elems.size() > 0);
//std::cout << "num element for edge:" << elems.size() << std::endl;
  std::vector<int> ordinals; ordinals.resize(elems.size());
  std::vector<int> polaritys; polaritys.resize(elems.size());
  std::vector<int> rotations; rotations.resize(elems.size());

  if (topo->parametric_dim == 1) {
    MeshObjConn::edge_info(support.begin(), support.end(), elems.begin(), elems.end(), &ordinals[0], &polaritys[0]);
  } else if (topo->parametric_dim == 2) {
    MeshObjConn::face_info(support.begin(), support.end(), elems.begin(), elems.end(), &ordinals[0], &polaritys[0], &rotations[0]);
  } else Throw() << "Parametric dim wrong for topo:" << topo->name;

  // We add node to these objects:
  for (UInt e = 0; e < elems.size(); e++) {
    MeshObj &elem = *elems[e];

    const MeshObjTopo *elem_topo = GetMeshObjTopo(elem);

    const int *side_nodes = (topo->parametric_dim == 1 ? elem_topo->get_edge_nodes(ordinals[e]) : elem_topo->get_side_nodes(ordinals[e]));
  
    MeshObj::Relation r;
    r.obj = node; 
    r.ordinal = side_nodes[ordinal];
    r.type = MeshObj::CHILD;
    AddMeshObjRelation(elem, r);
    r.obj = &elem;
    r.type = MeshObj::PARENT;
    AddMeshObjRelation(*node, r);
  }
}

void MeshDB::element_connect(MeshObj &element, std::vector<MeshObj*> &nodevect) {
  for (unsigned int i = 0; i < nodevect.size(); i++) {
    // push node onto element
    MeshObj::Relation rel;
    rel.obj = nodevect[i];
    rel.ordinal = i;
    rel.polarity = true;
    rel.type = MeshObj::USES;
    AddMeshObjRelation(element, rel);
    
    // and element onto node
   rel.obj = &element; rel.ordinal = i;
   rel.type = MeshObj::USED_BY;
   AddMeshObjRelation(*nodevect[i], rel);
  }
}

void MeshDB::add_edge_local(MeshObj &edge, MeshObj &element, int ordinal, UInt edgeset,
              const MeshObjTopo *topo, const Context &ctxt) {
  Trace __trace("MeshDB::add_edge_local(MeshObj &edge, MeshObj &element, int ordinal, UInt edgeset, const MeshObjTopo *topo, const Context &ctxt)");
  add_edge(edge, element, ordinal, edgeset, topo, true, ctxt);
}

void MeshDB::add_edge(MeshObj &edge, MeshObj &element, int ordinal, UInt edgeset,
              const MeshObjTopo *topo, bool local, const Context &ctxt) {

  // First connect the element to face;
  struct MeshObj::Relation rel;
  rel.obj = &edge;
  rel.ordinal = ordinal;
  rel.polarity = true;
  rel.rotation = 0;
  rel.type = MeshObj::USES;
  AddMeshObjRelation(element, rel);

  // Now the face to element
  rel.obj = &element;
  rel.type = MeshObj::USED_BY;
  AddMeshObjRelation(edge, rel);

  Attr attr(MeshObj::EDGE, edgeset);
  if (local) {
    // if local, then not part of the original mesh
    attr.get_context().set(Attr::PENDING_CREATE_ID);
    attr.get_context().clear(Attr::GENESIS_ID);
    attr.get_context() |= ctxt;
  }
  push_back_sorted(edge, attr, topo);

  edge_map.insert(edge);

  // Now to connect (potentially) the other elements

  std::vector<MeshObj*> enodes;
  MeshObjConn::get_obj_nodes(edge, enodes);
 
  ThrowAssert(enodes.size() > 0);
  std::vector<MeshObj*> elems;
  // Get the list of elements that use these nodes
  MeshObjConn::common_objs(enodes.begin(), enodes.end(),
             MeshObj::USED_BY, MeshObj::ELEMENT, elems);
//std::cout << "num element for edge:" << elems.size() << std::endl;
  std::vector<int> ordinals; ordinals.resize(elems.size());
  std::vector<int> polaritys; polaritys.resize(elems.size());

  MeshObjConn::edge_info(enodes.begin(), enodes.end(), elems.begin(), elems.end(), &ordinals[0], &polaritys[0]);

  for (UInt el = 0; el < elems.size(); el++) {
    MeshObj  &elem = *elems[el];
    if (elem == element) {
      ThrowRequire(ordinals[el] == ordinal);
      ThrowRequire(polaritys[el] == 1);
    } else {
      // Relation from edge to element
      rel.obj = &elem;
      rel.ordinal = ordinals[el];
      rel.polarity = polaritys[el];
      rel.rotation = 0; // no edge rotation
      rel.type = MeshObj::USED_BY;
      AddMeshObjRelation(edge, rel);

      // Relation from elem to edge
      rel.obj = &edge;
      rel.ordinal = ordinals[el];
      rel.polarity = polaritys[el];
      rel.rotation = 0; // no edge rotation
      rel.type = MeshObj::USES;
      AddMeshObjRelation(elem, rel);
    }
  }

}

void MeshDB::add_side_local(MeshObj &side, MeshObj &element, int ordinal, UInt sideset,
              const MeshObjTopo *topo, const Context &ctxt) {
  Trace __trace("MeshDB::add_side_local(MeshObj &side, MeshObj &element, int ordinal, UInt sideset, const MeshObjTopo *topo, const Context &ctxt)");
  add_side(side, element, ordinal, sideset, topo, true, ctxt);
}

void MeshDB::add_side(MeshObj &side, MeshObj &element, int ordinal, UInt sideset,
              const MeshObjTopo *topo, bool local, const Context &ctxt) {
  Trace __trace("MeshDB::add_side(MeshObj &side, MeshObj &element, int ordinal, UInt sideset, const MeshObjTopo *topo, bool local, const Context &ctxt)");

  // First connect the element to face;
  struct MeshObj::Relation rel;
  rel.obj = &side;
  rel.ordinal = ordinal;
  rel.polarity = true;
  rel.rotation = 0;
  rel.type = MeshObj::USES;
  AddMeshObjRelation(element, rel);

  // Now the face to element
  rel.obj = &element;
  rel.type = MeshObj::USED_BY;
  AddMeshObjRelation(side, rel);

  Attr attr(side_type(), sideset);
  if (local) {
    // if local, then not part of the original mesh
    attr.get_context().set(Attr::PENDING_CREATE_ID);
    attr.get_context().clear(Attr::GENESIS_ID);
    attr.get_context() |= ctxt;
  }
  push_back_sorted(side, attr, topo);

  GetMeshObjTopo(element)->parametric_dim == 3 ?
          face_map.insert(side) : edge_map.insert(side);

  // Now to connect (potentially) the other side of this object

  int ord, pol, rot;
  MeshObj *el = MeshObjConn::opposite_element(element, ordinal, ord, pol, rot);


  if (el ==NULL) {
//std::cout << "el:" << element.get_id() << "has no neighbor face:" << std::endl;
  } else {
    
    // Connect side to face
    rel.obj = el;
    rel.ordinal = ord;
    rel.polarity = pol;
    rel.rotation = rot;
    rel.type = MeshObj::USED_BY;
    AddMeshObjRelation(side, rel);

    // Connect elem to side
    rel.obj = &side;
    rel.ordinal = ord;
    rel.polarity = pol;
    rel.rotation = rot;
    rel.type = MeshObj::USES;
    AddMeshObjRelation(*el, rel);
if (side.get_id() == -1) {
  Par::Out() << "add side -1: to elem:" << element << ", opposite:" << *el << std::endl;
Par::Out() << "pol, rot=" << pol << ", " << rot << std::endl;
}
  }

}

UInt MeshDB::num_elems(UInt blk) const {
  UInt num = 0;
  KernelList::const_iterator mi = set_begin(), me = set_end();
  for (; mi != me; ++mi) {
    if (mi->type() == MeshObj::ELEMENT
        && mi->key() == blk && mi->is_active())
      num += mi->Count();
  }

  return num;
}

UInt MeshDB::num_sides(UInt blk) const {
  UInt num = 0;
  KernelList::const_iterator mi = setRoster.begin(), me = setRoster.end();
  for (; mi != me; ++mi) {
    if (mi->type() == (UInt) side_type()
        && mi->key() == blk && mi->is_active())
      num += mi->Count();
  }

  return num;
}

UInt MeshDB::num_nodes(UInt blk) const {
  UInt num = 0;
  KernelList::const_iterator mi = setRoster.begin(), me = setRoster.end();
  for (; mi != me; ++mi) {
    if (mi->type() == (UInt) MeshObj::NODE
        && mi->key() == blk && mi->is_active())
      num += mi->Count();
  }

  return num;
}

UInt MeshDB::num_objs(UInt obj_type) const {
  UInt num = 0;
  KernelList::const_iterator mi = setRoster.begin(), me = setRoster.end();
  for (; mi != me; ++mi) {
    if (mi->type() == obj_type && mi->is_active())
      num += mi->Count();
  }

  return num;
}

UInt MeshDB::num_globalKeys(UInt obj_type) const {
  std::set<UInt> keys;
  KernelList::const_iterator mi = setRoster.begin(), me = setRoster.end();
  for (; mi != me; ++mi) {
    // Zero is reserved for non exodus types
    if (mi->type() == obj_type && mi->key() != 0)
      keys.insert(mi->key());
  }

  return keys.size();
}

MeshDB::MeshObjIDMap &MeshDB::get_map(UInt type) {
  return const_cast<MeshDB::MeshObjIDMap&>(static_cast<const MeshDB*>(this)->get_map(type));
}
const MeshDB::MeshObjIDMap &MeshDB::get_map(UInt type) const {
  switch (type) {
    case MeshObj::NODE:
      return node_map;

    case MeshObj::EDGE:
      return edge_map;

    case MeshObj::FACE:
      return face_map;

    case MeshObj::ELEMENT:
      return element_map;


    default:
      Throw() << "Bad meshobjtype in get_map = " << MeshObjTypeString(type);
  }
}

MeshDB::MeshObjIDMap::const_iterator MeshDB::map_begin(UInt type) const {
  return get_map(type).begin();
}

MeshDB::MeshObjIDMap::const_iterator MeshDB::map_end(UInt type) const {
  return get_map(type).end();
}

MeshDB::MeshObjIDMap::const_iterator MeshDB::map_find(UInt type, MeshObj::id_type id) const {
  return get_map(type).find(id);
}

MeshDB::MeshObjIDMap::iterator MeshDB::map_find(UInt type, MeshObj::id_type id) {
  return get_map(type).find(id);
}

MeshObj::id_type MeshDB::get_min_id(UInt obj_type) const {
  const MeshObjIDMap &the_map = get_map(obj_type);

  if (the_map.empty()) return 1;

  return the_map.begin()->get_id();
}

MeshObj::id_type MeshDB::get_max_id(UInt obj_type) const {
  const MeshObjIDMap &the_map = get_map(obj_type);

  if (the_map.empty()) return 1;

  MeshObjIDMap::const_iterator me = the_map.end();
  me--;
  return me->get_id();
}

MeshObj::id_type MeshDB::get_new_local_id(UInt obj_type) const {
  // It doesn't matter these are max/min, because all numbers
  // are temporary: there will be a global resolution that assignes
  // numbers that don't march off to +- inf
  MeshObj::id_type new_id;
    new_id = get_min_id(obj_type) - 1;
    new_id = (new_id >= 0 ? -1 : new_id);

  return new_id;
}


// for ACTIVE objects only!!!
MeshDB::iterator MeshDB::obj_begin(UInt obj_type) {
  KernelList::iterator ms = setRoster.begin();
  const KernelList::iterator me = setRoster.end();
  
  for (; ms != me; ++ms) {
    if (ms->type() == obj_type)
      return MeshDB::iterator(ms, me);
  }

  //Throw() << "obj begin, did not find " << obj_type << " beginning";
  return obj_end();
}

MeshDB::const_iterator MeshDB::obj_begin(UInt obj_type) const {
  KernelList::const_iterator ms = setRoster.begin();
  const KernelList::const_iterator me = setRoster.end();
  
  for (; ms != me; ++ms) {
    if (ms->type() == obj_type)
      return MeshDB::const_iterator(ms, me);
  }

  //Throw() << "obj begin, did not find " << obj_type << " beginning";
  return obj_end();
}


MeshDB::const_iterator MeshDB::obj_end(UInt obj_type) const {
  const KernelList::const_iterator me = setRoster.end();
  KernelList::const_iterator ms = obj_begin(obj_type).mesh_set();
  
  while (ms != me && ms->type() == obj_type) ++ms;

  return const_iterator(ms, me);
}

MeshDB::iterator MeshDB::obj_end(UInt obj_type) {
  const KernelList::iterator me = setRoster.end();
  KernelList::iterator ms = obj_begin(obj_type).mesh_set();
  
  while (ms != me && ms->type() == obj_type) ++ms;

  return iterator(ms, me);
}

// for ACTIVE and INACTIVE objects !!!
MeshDB::iterator MeshDB::obj_begin_all(UInt obj_type) {
  KernelList::iterator ms = setRoster.begin();
  const KernelList::iterator me = setRoster.end();
  
  for (; ms != me; ++ms) {
    if (ms->type() == obj_type)
      return MeshDB::iterator(ms, me, Attr(obj_type, Context()));
  }

  //Throw() << "obj begin, did not find " << obj_type << " beginning";
  return obj_end();
}

MeshDB::const_iterator MeshDB::obj_begin_all(UInt obj_type) const {
  KernelList::const_iterator ms = setRoster.begin();
  const KernelList::const_iterator me = setRoster.end();
  
  for (; ms != me; ++ms) {
    if (ms->type() == obj_type)
      return MeshDB::const_iterator(ms, me, Attr(obj_type, Context()));
  }

  //Throw() << "obj begin, did not find " << obj_type << " beginning";
  return obj_end();
}


MeshDB::const_iterator MeshDB::obj_end_all(UInt obj_type) const {
  const KernelList::const_iterator me = setRoster.end();
  KernelList::const_iterator ms = obj_begin(obj_type).mesh_set();
  
  while (ms != me && ms->type() == obj_type) ++ms;

  return const_iterator(ms, me, Attr(obj_type, Context()));
}

MeshDB::iterator MeshDB::obj_end_all(UInt obj_type) {
  const KernelList::iterator me = setRoster.end();
  KernelList::iterator ms = obj_begin(obj_type).mesh_set();
  
  while (ms != me && ms->type() == obj_type) ++ms;

  return iterator(ms, me, Attr(obj_type, Context()));
}

UInt MeshDB::DefineContext(const std::string &cname) {
  if (curContext >= Context::NumBits()) 
    Throw() << "curContext=" << curContext << ", which is the maximum"
               << " number of contexts allowed." << cname
               << " cannot be registered!";
  std::pair<ContextMapType::iterator,bool> is =
    contexts.insert(std::make_pair(cname,curContext));

  // If already there, just return the id
  if (is.second == false)
    return is.first->second;

  return curContext++;
}

UInt MeshDB::GetContext(const std::string &cname) {
  Trace __trace("MeshDB::GetContext(const std::string &cname)");

  ContextMapType::iterator is =
    contexts.find(cname);

  if (is == contexts.end())
    Throw() << "Could not get context id for " << cname;
  // If already there, just return the id
  return is->second;
}

// by default, we really shouldn't be creating 
// local objects (check IsParallel).  But loop through
// and make sure.
void MeshDB::ResolvePendingDelete(int obj_type) {

  // A two stage process.  First, isolate self from mesh by
  // removing all relations to/from object.
  // Second, delete objects.

  // Loop sets.  Isolate relations.
  KernelList::iterator mi = set_begin(), me = set_end(), mn;
  for (; mi != me; ) {
    mn = mi; mn++;
    if ((mi->type() & obj_type) && mi->GetContext().is_set(Attr::PENDING_DELETE_ID)) {
//std::cout << "Deleting set:" << mi->GetAttr() << std::endl;
      // Loop objects, deleting
      List<MeshObj>::iterator ni = mi->objects.begin(), ne = mi->objects.end(), nn;
      for (; ni != ne;) {
        nn = ni; nn++;
        // Relations: remove any relations pointing
        // to this object, either from below or above
        // in the hierarchy.
        MeshObjConn::remove_forward_relations(*ni);
        MeshObjConn::remove_back_relations(*ni);

        ni = nn;
      }
    } // if pd
    mi = mn;
  } // mi

  // Actually delete objects
  mi = set_begin();
  for (; mi != me; ) {
    mn = mi; mn++;
    if ((mi->type() & obj_type) &&  mi->GetContext().is_set(Attr::PENDING_DELETE_ID)) {
//std::cout << "Deleting set:" << mi->GetAttr() << std::endl;
      // Loop objects, deleting
      List<MeshObj>::iterator ni = mi->objects.begin(), ne = mi->objects.end(), nn;
      for (; ni != ne;) {
        nn = ni; nn++;
        // Remove self from map
        MeshObjIDMap::iterator ti(&*ni);
        get_map(ni->get_type()).erase(ti);

#ifdef DEL_DEBUG
Par::Out() << "MeshDB delete " << MeshObjTypeString(ni->get_type()) << " " << ni->get_id() << std::endl;
#endif
        mi->erase(*ni);  // PENDING DELETE HAS NO DATA
        delete &*ni;
        ni = nn;
      }
    } // if pd
    mi = mn;
  } // mi
  remove_unused_kernels();
}
void MeshDB::ResolvePendingCreate() {

  UInt csize = Par::Size();

  // Loop sets.  Take 
  KernelList::iterator mi = set_begin(), me = set_end(), mn;
  for (; mi != me; ) {
    mn = mi; mn++;
    if (mi->GetContext().is_set(Attr::PENDING_CREATE_ID)) {
      List<MeshObj>::iterator ni = mi->objects.begin(), ne = mi->objects.end(), nn;

      for (; ni != ne; ) {

        if (ni->get_owner() >= csize) {
          Throw() << "Object:" << *ni << " has invalid owner id";
        }

        nn = ni; nn++; // in case we delete ourselves
        MeshObj &node = *ni;
        const Attr &oattr = GetAttr(node);
        const Context &ctxt = GetMeshObjContext(node);
        Context newctxt(ctxt);
        newctxt.clear(Attr::PENDING_CREATE_ID);
        if (newctxt == ctxt) continue; // no need to update.
        Attr attr(oattr, newctxt);
        update_obj(&node, attr);
        ni = nn;
      } // ni
    } // is pending
    mi = mn;
  } // mi
  remove_unused_kernels();
}

void MeshDB::remove_unused_nodes() {
  MeshDB::iterator ni = node_begin_all(), ne = node_end_all(), nn;

  UInt ndeleted = 0;
  for (; ni != ne; ) {
    nn = ni; nn++; // in case we delete ourselves
    MeshObj &node = *ni;
    if (node.Relations.size() == 0) {
      const Attr &oattr = GetAttr(node);
      const Context &ctxt = GetMeshObjContext(node);
      Context newctxt(ctxt);
      newctxt.set(Attr::PENDING_DELETE_ID);
      if (newctxt != ctxt) {
        ndeleted++;
        Attr attr(oattr, newctxt);
        update_obj(&node, attr);
      }
    }
    ni = nn;
  } // for ni

  //std::cout << "ndeleted=" << ndeleted << std::endl;

  ResolvePendingDelete();
}

void MeshDB::linearize_data_index() {

  for (UInt o = 0; o < NumMeshObjTypes; o++) {
    MeshDB::iterator ni = obj_begin_all(MeshObjTypes[o]), ne = obj_end_all(MeshObjTypes[o]);
    UInt i = 0;
    for (; ni != ne; ++ni) {
      ni->data_index = i;
      i++;
    }
  }
}

void MeshDB::Commit(UInt _nFields, MEFieldBase **_Fields, UInt _nfields, _field **_fields) {
  Trace __trace("MeshDB::Commit(UInt _nFields, MEFieldBase **_Fields)");

  if (committed)
    Throw() << "MeshDB already committed!!";

  for (UInt o = 0; o < NumMeshObjTypes; o++) {
    //UInt i = 0;
    MeshDB::iterator ni = obj_begin_all(MeshObjTypes[o]), ne = obj_end_all(MeshObjTypes[o]);
    for (; ni != ne; ++ni) {
      MeshObjRelationList(ni->Relations).swap(ni->Relations);
      //ni->data_index = i;
      //i++;
    }
  }

  // Now commit the kernels
  KernelList::iterator ki = set_begin(), ke = set_end();

  for (; ki != ke; ++ki) {
    ki->Commit(_nFields, _Fields, _nfields, _fields);
  }

  remove_unused_kernels();

  // Save fields in case we need to commit new kernels
  std::copy(_Fields, _Fields + _nFields, std::back_inserter(Fields));
  
  std::copy(_fields, _fields + _nfields, std::back_inserter(fields));

  committed = true;
}

void MeshDB::AssumeContexts(const MeshDB &rhs) {
  contexts = rhs.contexts;
  curContext = rhs.curContext;
  // simple enough
}

// In some ways, this is simpler than creating an arbitraty side, since we
// know the object is always shared across processors.  Assumption: the mesh has
// already been skinned, which tells us when an edge/face is on a proc boundry
// as opposed to an exterior.
void MeshDB::CreateAllSides() {
  Trace __trace("MeshDB::CreateAllSides()");
Par::Out() << "CreateAllSides" << std::endl;

  // Loop elements: create all faces, edges
  MeshDB::iterator eit = elem_begin(), eet = elem_end();
  for (; eit != eet; ++eit) {
    MeshObj &elem = *eit;
    // Loop sides.  If side not there, create
    const MeshObjTopo *topo = GetMeshObjTopo(elem);
    // Edges
    if (side_type() != MeshObj::EDGE) {
      for (int e = 0; e < topo->num_edges; e++) {
        MeshObjRelationList::iterator ei = MeshObjConn::find_relation(elem, MeshObj::EDGE, e);
        if (ei != elem.Relations.end()) continue;
        MeshObj *nedge = new MeshObj(MeshObj::EDGE, get_new_local_id(MeshObj::EDGE));
        const MeshObjTopo *etopo = topo->edge_topo(e);
        add_edge_local(*nedge, elem, e, 0, etopo);
      }
    }
    for (UInt s = 0; s < topo->num_sides; s++) {
      MeshObjRelationList::iterator si = MeshObjConn::find_relation(elem, side_type(), s);
      // If there, fine; move on.
      if (si != elem.Relations.end()) continue;
      MeshObj *nside = new MeshObj(side_type(), get_new_local_id(side_type()));
      const MeshObjTopo *stopo = topo->side_topo(s);
      add_side_local(*nside, elem, s, 0, stopo);
    } // sides
  } // elements
}

void MeshDB::remove_unused_kernels() {
  KernelList::iterator ki = set_begin(), ke = set_end(), kn;
  for (; ki != ke;) {
    kn = ki; ++kn;
    if (ki->NumObjects() == 0) delete &*ki;
    ki = kn;
  }
}

} // namespace
} // namespace
