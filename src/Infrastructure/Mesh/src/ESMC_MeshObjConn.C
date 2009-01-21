// $Id: ESMC_MeshObjConn.C,v 1.4.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshObjConn.h>
#include <ESMC_MeshObj.h>
#include <ESMC_MeshObjTopo.h>
#include <ESMC_Kernel.h>
#include <ESMC_ParEnv.h>
#include <ESMC_Mesh.h>

#include <mpi.h>

#include <set>

namespace ESMCI {
namespace MESH {

namespace MeshObjConn {

MeshObj *opposite_element(const MeshObj &obj, const int side_ordinal) {
  int ord, pol, rot;
  return opposite_element(obj, side_ordinal, ord, pol, rot);
}

MeshObj *opposite_element(const MeshObj &obj, const int side_ordinal, int &ordinal,
             int &polarity, int &rotation) {
  const MeshObjTopo *topo = GetMeshObjTopo(obj);
  const int * const side_nodes = topo->get_side_nodes(side_ordinal);
  int num_side_nodes = topo->get_num_side_nodes();

  ordinal = polarity = rotation = -1;  // set a default value for vars
//std::cout << "elem:" << obj << " side:" << side_ordinal << ":" << std::endl;

  std::vector<MeshObj*> in_objs;
  std::vector<MeshObj*> out_objs;

  for (int i = 0; i < num_side_nodes; i++) {
    MeshObj *node = obj.Relations[side_nodes[i]].obj;
    in_objs.push_back(node);
  }

  common_objs(in_objs.begin(), in_objs.end(), MeshObj::USED_BY, MeshObj::ELEMENT, out_objs);

  if (out_objs.size() == 1) {
    if (out_objs[0] != &obj)
      Throw() << "opposite elem, unexpected single element:" << *out_objs[0] << std::endl;
    return NULL;
  }

  if (out_objs.size() != 2) {
      std::vector<MeshObj*>::iterator si = out_objs.begin(),
         se = out_objs.end();
      std::cout << "Error, Expected two elements." << std::endl;
      std::cout << "this=" << obj.get_id() << ", side=" << side_ordinal << std::endl;
      for (; si != se; ++si) {
        std::cout << "Elem:" << (*si)->get_id() << std::endl;
        std::cout << "\ttopo:" << GetMeshObjTopo(**si)->name << std::endl;
        (*si)->printrelations(std::cout);
      }
      throw("opposite.  Expected two elements");
  }

  int pols[2];
  int rots[2];
  int ords[2];
  if (topo->parametric_dim == 3) {
    face_info(in_objs.begin(), in_objs.end(), out_objs.begin(), out_objs.end(),
      ords, pols, rots);
  } else {
    edge_info(in_objs.begin(), in_objs.end(), out_objs.begin(), out_objs.end(),
      ords, pols);
  }

  int idx = out_objs[0] == &obj ? 1: 0;
  ThrowRequire(out_objs[1-idx] == &obj);

  if (topo->parametric_dim == 3) {
    ordinal = ords[idx];
    rotation = rots[idx];
    polarity = pols[idx];
  } else {
    rotation = 0;
    ordinal = ords[idx];
    polarity = pols[idx];
  }

  return out_objs[idx];

}

// ***** Search by type, ordinal, rel_type ********
struct find_rel0_data {
  find_rel0_data(UInt ot, int ord, int rtype) : objtype(ot), ordinal(ord), rel_type(rtype) {}
  UInt objtype;
  int ordinal;
  int rel_type;
};

class find_rel0_less : public std::binary_function<MeshObj::Relation,find_rel0_data,bool> {
public:
  find_rel0_less() {}
  bool operator()(const MeshObj::Relation &l, const find_rel0_data &r) const {
    if (l.obj->get_type() != r.objtype) return l.obj->get_type() < r.objtype;
    if (l.type != r.rel_type) return l.type < r.rel_type;
    return l.ordinal < r.ordinal;
  }
};

MeshObjRelationList::const_iterator find_relation(const MeshObj &obj, UInt objtype, int ordinal, int rel_type) {
  MeshObjRelationList::iterator rel = find_relation(const_cast<MeshObj&>(obj), objtype, ordinal, rel_type);
  return rel;
}

MeshObjRelationList::iterator find_relation(MeshObj &obj, UInt objtype, int ordinal, int rel_type) {
  MeshObjRelationList::iterator rel = obj.Relations.begin(), rele = obj.Relations.end(), ri;
  
  ri = std::lower_bound(rel, rele, find_rel0_data(objtype, ordinal, rel_type), find_rel0_less());
  if (ri == rele || (objtype != ri->obj->get_type() || ri->ordinal != ordinal || ri->type != rel_type)) return rele;
  return ri;
}

// ***** Search by type, ordinal ********
struct find_rel1_data {
  find_rel1_data(UInt ot, int ord) : objtype(ot), ordinal(ord) {}
  UInt objtype;
  int ordinal;
};

class find_rel1_less : public std::binary_function<MeshObj::Relation,find_rel1_data,bool> {
public:
  find_rel1_less() {}
  bool operator()(const MeshObj::Relation &l, const find_rel1_data &r) const {
    if (l.obj->get_type() != r.objtype) return l.obj->get_type() < r.objtype;
    return l.ordinal < r.ordinal;
  }
};

MeshObjRelationList::const_iterator find_relation(const MeshObj &obj, UInt objtype, int ordinal) {
  MeshObjRelationList::iterator rel = find_relation(const_cast<MeshObj&>(obj), objtype, ordinal);
  return rel;
}

MeshObjRelationList::iterator find_relation(MeshObj &obj, UInt objtype, int ordinal) {
  MeshObjRelationList::iterator rel = obj.Relations.begin(), rele = obj.Relations.end(), ri;
  
  ri = std::lower_bound(rel, rele, find_rel1_data(objtype, ordinal), find_rel1_less());
  if (ri == rele || (objtype != ri->obj->get_type() || ri->ordinal != ordinal)) return rele;
  return ri;
}

MeshObjRelationList::const_iterator find_relation(const MeshObj &obj, UInt objtype) {
  MeshObjRelationList::iterator rel = find_relation(const_cast<MeshObj&>(obj), objtype);
  return rel;
}

// ***** Search by type ********
class find_rel2_less : public std::binary_function<MeshObj::Relation,UInt,bool> {
public:
  find_rel2_less() {}
  bool operator()(const MeshObj::Relation &l, UInt r) const {
    return l.obj->get_type() < r;
  }
};
MeshObjRelationList::iterator find_relation(MeshObj &obj, UInt objtype) {
  MeshObjRelationList::iterator rel = obj.Relations.begin(), rele = obj.Relations.end(), ri;

  ri = std::lower_bound(rel, rele, objtype, find_rel2_less());
  if (ri == rele || objtype != ri->obj->get_type()) return rele;
  return ri;
}

void PatchNodes(const MeshObj &elem, std::set<const MeshObj*> &nodes) {

 std::set<const MeshObj*>().swap(nodes);

 std::set<MeshObj*> elems;
  // Get the patch of elements
  elems.clear();
  UInt npe = GetMeshObjTopo(elem)->num_nodes;
  for (UInt n = 0; n < npe; n++) {
    const MeshObj &node = *elem.Relations[n].obj;
    MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
  
    // Stack elements in
    while (el != node.Relations.end() && el->obj->get_type() == MeshObj::ELEMENT){
      elems.insert(el->obj);
      el++;
    }
  
    std::set<MeshObj*>::iterator ei = elems.begin(), ee = elems.end();
    for (; ei != ee; ++ei) {
      const MeshObj &selem = **ei;
      UInt npe = GetMeshObjTopo(selem)->num_nodes;
      // Loop the nodes as described by the rme
      for (UInt i = 0; i < npe; i++) {
        const MeshObj &snode = *selem.Relations[i].obj;
        nodes.insert(&snode);
      }
    }

  } // for n

}

template<typename obj_iter>
void common_objs(obj_iter in_obj_begin, obj_iter in_obj_end, 
                 UInt rel_type, 
                 UInt out_obj_type, 
                 std::vector<MeshObj*> &out_obj)
{

  std::set<MeshObj*> objs[2];
  std::set<MeshObj*> current;

  if (in_obj_begin == in_obj_end) return;

  out_obj.clear();

//#define DEBUG_CMN
#ifdef DEBUG_CMN
std::cout << "In objs:" << std::endl;
for (UInt i = 0; i < in_obj.size(); i++) {
  std::cout << *in_obj[i] << std::endl;
}
#endif

  MeshObj *obj = *(in_obj_begin++);
#ifdef DEBUG_CMN
std::cout << "obj:" << *obj << ", " << std::endl;
#endif
  MeshObjRelationList::iterator mi = find_relation(*obj, out_obj_type), me = obj->Relations.end();
  for (; (mi != me) && (UInt) mi->obj->get_type() == out_obj_type &&
          mi->type & rel_type; ++mi) {
    objs[0].insert((*mi).obj);
#ifdef DEBUG_CMN
std::cout << "Inserting object:" << *mi->obj;
#endif
  }
 


  // Intersect with remaining
  int last = 0, next = 1;
  while (in_obj_begin != in_obj_end) {
    obj = *(in_obj_begin++);
#ifdef DEBUG_CMN
std::cout << "obj:" << *obj << ", " << std::endl;
#endif
    current.clear();
    objs[next].clear();
    for (MeshObjRelationList::iterator mi = find_relation(*obj, out_obj_type);
       mi != obj->Relations.end() && (UInt) (*mi).obj->get_type() == out_obj_type &&
       mi->type & rel_type; mi++) {
      current.insert((*mi).obj);
#ifdef DEBUG_CMN
std::cout << "Inserting object:" << *mi->obj;
#endif
//std::cout << "\tinsert el:" << (*mi).obj->get_id() << std::endl;
    }
    std::set_intersection(current.begin(), current.end(),
                          objs[last].begin(), objs[last].end(),
                          std::inserter(objs[next], objs[next].begin()));

    if (objs[next].size() == 0) {
      return;
    }
 
    last = 1 - last;
    next = 1 - next;
  }

  
  std::set<MeshObj*>::iterator si = objs[last].begin(), se = objs[last].end();
  for (; si != se; ++si) {
    out_obj.push_back(*si);
  }
  
}

bool get_obj_nodes(const MeshObj &obj, std::vector<MeshObj*> &nodes, bool must_find) {
  Trace __trace("get_obj_nodes(const MeshObj &obj, std::vector<MeshObj*> &nodes, bool must_find)");

  nodes.clear();
  if (obj.get_type() == MeshObj::ELEMENT) {
    const MeshObjTopo *topo = GetMeshObjTopo(obj);
    UInt npe = topo->num_nodes;
    for (UInt i = 0; i < npe; i++) 
      nodes.push_back(obj.Relations[i].obj);
      return true;
  } else if (obj.get_type() == MeshObj::NODE) {
    UInt ord; const MeshObjTopo *topo;
    get_node_support(obj, topo, ord, nodes);
    return true;
  }

  const MeshObjTopo *otopo = GetMeshObjTopo(obj);
  ThrowRequire(otopo);
  // Otherwise we must use an element and a topo to get the nodes we need
  MeshObjRelationList::const_iterator el = MeshObjConn::find_relation(obj, MeshObj::ELEMENT);
  if (el == obj.Relations.end()) {
    if (must_find) {
      std::cout << "No elements attached to object!";
      std::cout << "obj:" << obj;
      Throw();
    } else return false;
  }
  const MeshObj &elem = *el->obj;
  const MeshObjTopo *topo = GetMeshObjTopo(elem);
  UInt ord = el->ordinal;
  const int *perm = otopo->perm_table(el->rotation, el->polarity);
  if (obj.get_type() == (UInt) GetMeshObjMesh(obj).side_type()) {
    const int * const side_nodes = topo->get_side_nodes(ord);
    for (UInt n = 0; n < topo->get_num_side_nodes(); n++) {
      nodes.push_back(elem.Relations[side_nodes[perm[n]]].obj);
    }
  } else if (obj.get_type() == MeshObj::EDGE) {
    const int * const edge_nodes = topo->get_edge_nodes(ord);
    UInt nnodes = topo->get_num_edge_nodes();
    nodes.resize(nnodes, 0);
    for (UInt n = 0; n < nnodes; n++) {
      MeshObj *op = elem.Relations[edge_nodes[perm[n]]].obj;
      nodes[n] = op;
    }
  } else
    Throw() << "get nodes for this combo not implemented:obj:" << obj << ", mesh side type:" <<
          MeshObjTypeString(GetMeshObjMesh(obj).side_type()) << " obj.get_type()=" <<
          MeshObjTypeString(obj.get_type());

  return true;
}

void get_shared_procs(const MeshObj &obj, const CommRel &node_sym_spec, std::vector<UInt> &procs, bool obj_in_comm)
{
  Trace __trace("get_shared_procs(const MeshObj &obj, const CommRel &node_sym_spec, std::vector<UInt> &procs)");

  procs.clear();

  std::vector<MeshObj*> nodes;

  if (!obj_in_comm) {

  // First try to get support for the object itself
  if (!get_obj_nodes(obj, nodes, false)) {
    // Failed, so try a parent (sharing is same)
    MeshObj::Relation r;
    r.obj = const_cast<MeshObj*>(&obj); r.ordinal = 0; r.type = MeshObj::PARENT;
    MeshObjRelationList::const_iterator lb = std::lower_bound(obj.Relations.begin(), obj.Relations.end(), r);
    
    // For interior objects, there may not be parents, so the object may be toast.
    if (lb == obj.Relations.end() ||
                  !(lb->obj->get_type() == obj.get_type() && lb->type == MeshObj::PARENT)) return;

    // Get node support of this parent object
    get_obj_nodes(*lb->obj, nodes);
  }

  } else nodes.push_back(const_cast<MeshObj*>(&obj));

  if (nodes.size() == 0) return;

  // If any nodes are marked as unshared, then the object
  // isn't shared.
  for (UInt i = 0; i < nodes.size(); i++) {
    if (!GetAttr(*nodes[i]).is_shared()) return;
  }

  // Get the list of procs
  std::vector<MeshObj*>::iterator ni = nodes.begin(), ne = nodes.end();


  CommRel::MapType::const_iterator ci = node_sym_spec.domain_begin(), ce = node_sym_spec.domain_end(), cl;
  
  // Get the list of object shared by first node.
  cl = std::lower_bound(ci, ce, CommRel::CommNode(*ni,0));
  for (; cl != ce && cl->obj == *ni; cl++) {
    // There is no entry for this proc, so push on
    procs.push_back(cl->processor);
  }

  // Now loop the rest of the nodes, intersecting their procs with mine
  for (++ni; ni != nodes.end() && !procs.empty(); ++ni) {
    cl = std::lower_bound(ci, ce, CommRel::CommNode(*ni,0));

    // we don't add anyone less that 
    for (UInt i = 0; i < procs.size(); ) {
      // Move forward till we go to or past proc[j]
      for (; (cl != ce && cl->obj == *ni) && cl->processor < procs[i]; cl++);
       if ((cl != ce && cl->obj == *ni) && cl->processor == procs[i]) {
         // We hit the proc, so we keep it
         i++;
       } else {
         // We went past the proc, so erase it
         procs.erase(procs.begin()+i);
       }
    }
  }
  
}

void get_node_sharing(MeshObj &node, const CommRel &node_sym_spec, std::vector<UInt> &procs) {
  procs.clear();
  CommRel::MapType::const_iterator ci = node_sym_spec.domain_begin(), ce = node_sym_spec.domain_end(), cl;
  
  // Get the list of object shared by first node.
  cl = std::lower_bound(ci, ce, CommRel::CommNode(&node,0));
  for (; cl != ce && cl->obj == &node; cl++) {
    // There is no entry for this proc, so push on
    procs.push_back(cl->processor);
  }
}

template<typename obj_iter>
void edge_info(obj_iter node_begin, obj_iter node_end,
               obj_iter elem_begin, obj_iter elem_end,
               int *ordinal, // out
               int *polarity, // out
               bool must_find
               )
{
  UInt nnode = std::distance(node_begin, node_end);

  UInt el = 0;
  obj_iter ei = elem_begin;
  for (; ei != elem_end; ++ei, el++) {
    const MeshObj &elem = **ei;
    const MeshObjTopo *eltopo = GetMeshObjTopo(elem);
    // Now Loop sides, trying to find a match
    bool found = false;
    for (UInt e = 0; !found && e < (UInt) eltopo->num_edges; e++) {
      const MeshObjTopo *etopo = eltopo->edge_topo(e);
      ThrowRequire(etopo && etopo->num_nodes == nnode);
      UInt n = 0;
      for (n = 0; n < etopo->num_nodes; n++) {
        MeshObj &enode = *elem.Relations[eltopo->get_edge_nodes(e)[n]].obj;
        obj_iter nfi =
          std::find(node_begin, node_end, &enode);
        if (nfi == node_end) break; // not the right edge
      }
      found = (n == etopo->num_nodes);
      if (found) {
        ordinal[el] = e;
        polarity[el] = (elem.Relations[eltopo->get_edge_nodes(e)[0]].obj == *node_begin ?
                  1 : 0);
//std::cout << "Edge is (ord, pol)=" << e << ", " << polarity[el] << std::endl;
      }
    } // edges
    if (must_find && !found) Throw() << "Edge not found in element";
    if (!found) ordinal[el] = -1;
  } // elems
}

template void edge_info<>(MeshObj **node_begin,
               MeshObj** node_end,
               MeshObj** elem_begin,
               MeshObj** elem_end,
               int *ordinal, // out
               int *polarity, // out
               bool
               );

template <typename obj_iter>
void face_info(obj_iter node_begin, obj_iter node_end,
               obj_iter elem_begin, obj_iter elem_end,
               int *ordinal, // out
               int *polarity, // out
               int *rotation, // out
               bool must_find
               )
{
#ifndef NDEBUG
  UInt nelem = std::distance(elem_begin, elem_end);
#endif
  ThrowAssert(nelem <= 2 && nelem > 0); // how could there be more than two elems on a face?
  UInt nnode = std::distance(node_begin, node_end);

  UInt el = 0;
  obj_iter ei = elem_begin;
  for (; ei != elem_end; ++ei, el++) {
    const MeshObj &elem = **ei;
    const MeshObjTopo *eltopo = GetMeshObjTopo(elem);
    ThrowRequire(eltopo->parametric_dim == 3); // So we can use sides below and mean face
    // Now Loop sides, trying to find a match
    bool found = false;
    rotation[el] = 0;
    int rotation1 = 0;
    for (UInt e = 0; !found && e < (UInt) eltopo->num_sides; e++) {
      const MeshObjTopo *etopo = eltopo->side_topo(e);
      ThrowRequire(etopo && etopo->num_nodes == nnode);
      UInt n = 0;
      for (n = 0; n < etopo->num_nodes; n++) {
        MeshObj &enode = *elem.Relations[eltopo->get_side_nodes(e)[n]].obj;
        obj_iter nfi =
          std::find(node_begin, node_end, &enode);
        if (nfi == node_end) break; // not the right face

        // Gather some info to figure out orientation.  Save how
        // the first and second nodes of this face relate to the given
        // node ordering.
        if (n == 0) rotation[el] = std::distance(node_begin, nfi);
        if (n == 1) rotation1 = std::distance(node_begin, nfi);
      }
      found = (n == etopo->num_nodes);
      if (found) {
        ordinal[el] = e;
        // Ok: polarity and orientation are more difficult than for edges: 
        polarity[el] = ((UInt) rotation1 == ((rotation[el] + 1) % etopo->num_nodes)) ? 1 : 0; 
        // Now that we know the polarity, we can investigate the true rotation.
        // Use the fact that (i-r) % n = other index of i for true polarity,
        // or i->(n-1)-i, (i+r) % n = other index for reverse polarity
        int i = 0;
        for (; (UInt) i < nnode; i++) {
          int oidx = (polarity[el] == 1 ? ((etopo->num_nodes-i) % etopo->num_nodes): ((etopo->num_nodes - 1- 0 + i) % etopo->num_nodes));
          if (elem.Relations[eltopo->get_side_nodes(e)[oidx]].obj == *node_begin) {
            rotation[el] = i; break;
          }
        }
        // Should stop before hitting nnode
        if ((UInt) i == nnode) {
          std::cout << "Error, could not settle roation/polatiry: in nodes:";
          for (obj_iter ni = node_begin; ni != node_end; ni++) std::cout << (*ni)->get_id() << " ";
          std::cout << std::endl;
          std::cout << "node found:";
          for (int i = 0; (UInt) i < etopo->num_nodes; i++) {
          int oidx = (polarity[el] == 1 ? ((etopo->num_nodes-i) % etopo->num_nodes): ((etopo->num_nodes - 1- 0 + i) % etopo->num_nodes));
            std::cout << elem.Relations[eltopo->get_side_nodes(e)[i]].obj->get_id() << " (oidx=" << oidx <<") ";
          }
          std::cout << std::endl;
          std::cout << "deduced polarity=" << polarity[el] << ", nnode=" << nnode << std::endl;
          Throw();
        }
 
        

/* verify the permutation tables
const int *perm = etopo->perm_table(rotation[el], polarity[el]);
std::cout << "Verify(rot,pol):" << rotation[el] << ", " << polarity[el] << " topo:" << etopo->name << std::endl;
for (n = 0; n < etopo->num_nodes; n++) {
  MeshObj &enode = *elem.Relations[eltopo->get_side_nodes(e)[perm[n]]].obj;
  if (&enode != *(node_begin + n)) {
    std::cout << "Nodes in:";
    for (obj_iter ni = node_begin; ni != node_end; ++ni) {
      std::cout << (*ni)->get_id() << " ";
    }
    std::cout << "Nodes on side:";
    for (UInt nn = 0; nn < etopo->num_nodes; nn++) {
      MeshObj &Enode = *elem.Relations[eltopo->get_side_nodes(e)[nn]].obj;
      std::cout << Enode.get_id() << ", perm:" << perm[nn]<< " ";
    }
    std::cout << std::endl;
std::cout << "(rot,pol):" << rotation[el] << ", " << polarity[el] << std::endl;
    Throw() << "Node perm table didnt match!!";
  }
  
}
*/

      }
    } // faces
    if (!found) ordinal[el] = -1;
    if (!found && must_find) {
      std::cout << "Face was not associated with element!!";
      std::cout << "element:" << elem << std::endl;
      std::cout << "Nodes:";
      for (obj_iter ni = node_begin; ni != node_end; ++ni)
         std::cout << (*ni)->get_id() << " ";
      std::cout << std::endl;
      Throw();
    }
  } // elems
}

template void face_info<>(MeshObj** node_begin,
               MeshObj** node_end,
               MeshObj** elem_begin,
               MeshObj** elem_end,
               int *ordinal, // out
               int *polarity, // out
               int *rotation, // out
               bool
               );

template
void common_objs<>(MeshObj** in_obj_begin, MeshObj** in_obj_end, 
                 UInt rel_type, 
                 UInt out_obj_type, 
                 std::vector<MeshObj*> &out_obj);


void remove_back_relations(MeshObj &obj) {
  MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();

  // Loop my relations.  If I find have PARENT or USED_BY, I must remove
  // myself from that objects relations.
  for (; ri != re; ++ri) {

    // See if the object has a uses pointing at me, if so, remove
    MeshObj::Relation r = *ri;
    r.obj = &obj;
    r.ordinal = ri->ordinal;
    if (ri->type == MeshObj::USED_BY) {
      r.type = MeshObj::USES;
      MeshObjRelationList::iterator li =
        std::lower_bound(ri->obj->Relations.begin(), ri->obj->Relations.end(), r);
      if (li != ri->obj->Relations.end() && li->obj == &obj && li->ordinal == r.ordinal && li->type == r.type) {
        // remove 
        ri->obj->Relations.erase(li);
      }
    } else if (ri->type == MeshObj::PARENT) {

      // If object has me as child,remove relation.
      r.type = MeshObj::CHILD;
      MeshObjRelationList::iterator li = std::lower_bound(ri->obj->Relations.begin(), ri->obj->Relations.end(), r);
      if (li != ri->obj->Relations.end() && li->obj == &obj && li->ordinal == r.ordinal && li->type == r.type) {
        // remove 
        ri->obj->Relations.erase(li);
      }
    }
  } // for ri

}

void remove_forward_relations(MeshObj &obj, bool child_only) {
  Trace __trace("remove_forward_relations(MeshObj &obj, bool child_only)");

  MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();

  // Loop my relations.  If I find have CHILD or USES, I must remove
  // myself from that objects relations.
  for (; ri != re; ++ri) {

//std::cout << " (investg " << MeshObjTypeString(ri->obj->get_type()) << " " << ri->obj->get_id() << ") ";
    // See if the object has a used_by pointing at me, if so, remove
    bool found = false;
    MeshObj::Relation r = *ri;
    r.obj = &obj;
    r.ordinal = ri->ordinal;

    if (ri->type == MeshObj::USES && !child_only) {
      r.type = MeshObj::USED_BY;
   
      MeshObjRelationList::iterator li =
        std::lower_bound(ri->obj->Relations.begin(), ri->obj->Relations.end(), r);
      // Found possible objects, but there could be multiple USED_BY of same ordinal, type, obj type
      while (!found && li != ri->obj->Relations.end() && li->type == r.type && li->ordinal == r.ordinal) {
        if (li->obj->get_id() == obj.get_id()) found = true; else li++;
      }
      if (found) {
        ri->obj->Relations.erase(li);
      } else Throw() << "Obj: " << obj << "uses but is not used by:\n" << *ri->obj << std::endl;

    } else if (ri->type == MeshObj::CHILD) {

/*
if (ri->obj->get_type() == MeshObj::NODE && ri->obj->get_id() == 893) {
  Par::Out() << "node 893 being examined: I am:" << obj << ", node is:" << *ri->obj << std::endl;
}
*/

      // If object has me as parent,remove relation.
      r.type = MeshObj::PARENT;
      found = false;
      MeshObjRelationList::iterator li =
                 std::lower_bound(ri->obj->Relations.begin(), ri->obj->Relations.end(), r);
      while (!found && li != ri->obj->Relations.end() && li->type == r.type && li->ordinal == r.ordinal) {
        if (li->obj->get_id() == obj.get_id()) found = true; else li++;
      }
//if (ri->obj->get_type() == MeshObj::NODE && ri->obj->get_id() == 893) 
      if (found) {
    //    std::cout << " (zap " << MeshObjTypeString(ri->obj->get_type()) << " " << ri->obj->get_id() << ") ";
        ri->obj->Relations.erase(li);
      } else Throw() << "Obj: " << obj << "CHILD of but is not PARENTed by:\n" << *ri->obj << std::endl;
  //Par::Out() << "\tfound=" << found << ", obj after erase:" << *ri->obj << std::endl;
    }
  } // for ri

}

bool verify_parent_child_relations(MeshObj &obj) {

  // For every element I have as a child, make sure they have me as
  // a parent:

  MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();

  bool ok = true;
  for (; ri !=re; ++ri) {
    if (ri->type != MeshObj::CHILD) continue;
    
    MeshObj &cobj = *ri->obj;
    bool found = false;
    MeshObj::Relation r = *ri;
    r.obj = &obj;
    r.ordinal = ri->ordinal;
    r.type = MeshObj::PARENT;
   
    MeshObjRelationList::iterator li =
        std::lower_bound(ri->obj->Relations.begin(), ri->obj->Relations.end(), r);
      // Found possible objects, but there could be multiple USED_BY of same ordinal, type, obj type
    while (!found && li != ri->obj->Relations.end() && li->type == r.type && li->ordinal == r.ordinal) {
      if (li->obj->get_id() == obj.get_id()) found = true; else li++;
    }
    if (!found) {
      ok = false;
      std::cout << "/---------------------------------------------------/" << std::endl;
      std::cout << "Obj:" << obj << " has no backward parent rel from:\n" << cobj;
      std::cout << "/---------------------------------------------------/" << std::endl;
    }
  }
  return ok;
}

bool obj_used(MeshObj &obj) {
  
  MeshObjRelationList::iterator ui = obj.Relations.begin(), ue = obj.Relations.end();
   
  bool is_used = false;
  for (;!is_used && ui != ue; ++ui) {
    if (ui->type == MeshObj::USED_BY) {

    // If object is not on pending delete, we are used
    if (!GetMeshObjContext(*ui->obj).is_set(Attr::PENDING_DELETE_ID))
      is_used = true;
/*
        // Look for the back relation
      MeshObjRelationList::iterator uui = MeshObjConn::find_relation(
                       *ui->obj, obj.get_type(), ui->ordinal, MeshObj::USES);
      if (uui != ui->obj->Relations.end()) {
        // See if object is active
        is_used = true;
      }
        ThrowRequire(is_used); // if USED_BY, then must have USED
*/

//std::cout << " ub " << MeshObjTypeString(ui->obj->get_type()) << ", " << ui->obj->get_id() << " ";

    }
  }
  return is_used;
}

void get_node_support(const MeshObj &node, // in
                       const MeshObjTopo *&topo, // out
                       UInt &ordinal, // out
                       std::vector<MeshObj*> &nodes)
{
  Trace __trace("get_node_support(const MeshObj &node, const MeshObjTopo *&topo, UInt &ordinal, std::vector<MeshObj*> &nodes)");

  // Firstly, get a parent element for the node:
  MeshObjRelationList::const_iterator pi = find_relation(node, MeshObj::ELEMENT);

  nodes.clear();

  bool found = false;
  for (; !found && pi != node.Relations.end() && pi->obj->get_type() == MeshObj::ELEMENT; ++pi) {
    if (pi->type == MeshObj::PARENT) found = true;
  }
  if (!found) {
    std::cout << "Node:" << node;
    Throw() << "Error, get_node_support, no parent!!";
  }
  pi--;

  MeshObj &elem = *pi->obj;

  const MeshObjTopo *etopo = GetMeshObjTopo (elem);

  found = false;

  // Loop edges first, to see if node is on an edge
  for (UInt e = 0; !found && e < (UInt) etopo->num_edges; e++) {
    const MeshObjTopo *edge_topo = etopo->edge_topo(e);
    const int *edge_nodes = etopo->get_edge_nodes(e);

    int n = edge_topo->num_nodes;
    for (; !found && n < edge_topo->num_child_nodes; n++) {
      found = edge_nodes[n] == pi->ordinal;
    }

    if (found) {

      ordinal = n - 1;
     
      nodes.reserve(edge_topo->num_nodes);

      for (UInt n = 0; n < edge_topo->num_nodes; n++) {

        nodes.push_back(elem.Relations[edge_nodes[n]].obj);
        
      }

        topo = edge_topo;

    }

  } // edges

  if (etopo->parametric_dim >= 3) {
    // Loop faces first, to see if node is on a face
    for (UInt f = 0; !found && f < etopo->num_sides; f++) {
      const MeshObjTopo *face_topo = etopo->side_topo(f);
      const int *face_nodes = etopo->get_side_nodes(f);
  
      int n = face_topo->num_nodes;
      for (; !found && n < face_topo->num_child_nodes; n++) {
        found = face_nodes[n] == pi->ordinal;
      }
  
      if (found) {
  
        ordinal = n - 1;
       
        nodes.reserve(face_topo->num_nodes);
  
        for (UInt n = 0; n < face_topo->num_nodes; n++) {
  
          nodes.push_back(elem.Relations[face_nodes[n]].obj);
          
        }
  
          topo = face_topo;

      }
  
    } // faces
  } // dim >= 3

  if (!found) {

    // Must be the element
    nodes.clear(); nodes.reserve(etopo->num_nodes);

    ordinal = pi->ordinal;

    for (UInt n = 0; n < etopo->num_nodes; n++)

      nodes.push_back(elem.Relations[n].obj);

      topo = etopo;
 
  } // element

}

int get_ordinal(MeshObj &obj, MeshObj &rel_obj) {
  MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();
  for (; ri != re; ++ri) {
    if (ri->obj == &rel_obj) return ri->ordinal;
  }
  return -1;
}

bool verify_edge_relations(Mesh &mesh) {
  Trace __trace("verify_edge_relations(Mesh &mesh)");

  Mesh::iterator ei = mesh.obj_begin_all(MeshObj::EDGE), ee = mesh.obj_end_all(MeshObj::EDGE);

  bool ok = true;

  for (; ei != ee; ++ei) {
    MeshObj &edge = *ei;
//Par::Out() << "Check edge:" << edge.get_id() << std::endl;
  
    MeshObjRelationList::iterator el = edge.Relations.begin(), ele = edge.Relations.end();
   
    int nelem = 0;
    std::vector<MeshObj*> nodes;
    MeshObj *oelem = NULL;
    while (el != ele) {

      // Advance to end or next element USED_BY
      while (el != ele && !(el->obj->get_type() == MeshObj::ELEMENT && el->type == MeshObj::USED_BY)) ++el;
 
      if (el == ele) break;

       MeshObj &elem = *el->obj; if (!oelem) oelem = &elem;
//Par::Out() << "\tCheck elem:" << elem.get_id() << ", relation:" << std::endl;
       nelem++;

       const MeshObjTopo *topo = GetMeshObjTopo(elem);
       const int *edge_nodes = topo->get_edge_nodes(el->ordinal);
       const MeshObjTopo *etopo = topo->edge_topo(el->ordinal);
       const int *perm = etopo->perm_table(el->rotation,el->polarity);
       ThrowRequire(perm);

       if (nodes.size() == 0) {
         // Build node list
         for (UInt n = 0; n < etopo->num_nodes; n++) {
           nodes.push_back(elem.Relations[edge_nodes[perm[n]]].obj);
         }
       } else {
         // Check nodes
         for (UInt n = 0; n < etopo->num_nodes; n++) {
           if (nodes[n] != elem.Relations[edge_nodes[perm[n]]].obj) {
             ok = false;
             Par::Out() << "Error, edge nodes don't match:" << edge;

             Par::Out() << "First elem:";
             for (UInt i = 0; i < nodes.size(); i++)
               Par::Out() << " " << nodes[i]->get_id();

             Par::Out() << ", this elem:"; 
             for (UInt j = 0; j < etopo->num_nodes; j++)
               Par::Out() << " " << elem.Relations[edge_nodes[perm[j]]].obj->get_id();

             Par::Out() << std::endl;

             Par::Out() << "First elem detail:" << *oelem << std::endl;
             Par::Out() << "This elem detail:" << elem << std::endl;
             Throw() << "edge verify relation err!!";
           }
         }
//if (ok) Par::Out() << "\t elem ok" << std::endl;
       }

       ++el;
     } // while relations
     if (nelem == 0) {
       // Better be a ghosted edge
       if (!GetMeshObjContext(edge).is_set(Attr::SHARED_ID))
         Throw() << "Edge:" << edge << " has no elem!!";
     }
   
     nodes.clear();
  } // for edges

  return ok;
}

bool verify_face_relations(Mesh &mesh) {
  Trace __trace("verify_face_relations(Mesh &mesh)");

  Mesh::iterator ei = mesh.obj_begin_all(MeshObj::FACE), ee = mesh.obj_end_all(MeshObj::FACE);

  bool ok = true;

  for (; ei != ee; ++ei) {
    MeshObj &face = *ei;
//Par::Out() << "Check face:" << face.get_id() << std::endl;
  
    MeshObjRelationList::iterator el = face.Relations.begin(), ele = face.Relations.end();
   
    int nelem = 0;
    std::vector<MeshObj*> nodes;
    while (el != ele) {

      // Advance to end or next element USED_BY
      while (el != ele && !(el->obj->get_type() == MeshObj::ELEMENT && el->type == MeshObj::USED_BY)) ++el;
 
      if (el == ele) break;

       MeshObj &elem = *el->obj;
//Par::Out() << "\tCheck elem:" << elem.get_id() << ", relation:" << std::endl;
       nelem++;

       const MeshObjTopo *topo = GetMeshObjTopo(elem);
       const int *side_nodes = topo->get_side_nodes(el->ordinal);
       const MeshObjTopo *etopo = topo->side_topo(el->ordinal);
       const int *perm = etopo->perm_table(el->rotation,el->polarity);
       ThrowRequire(perm);

       if (nodes.size() == 0) {
         // Build node list
         for (UInt n = 0; n < etopo->num_nodes; n++) {
           nodes.push_back(elem.Relations[side_nodes[perm[n]]].obj);
         }
       } else {
         // Check nodes
         for (UInt n = 0; n < etopo->num_nodes; n++) {
           if (nodes[n] != elem.Relations[side_nodes[perm[n]]].obj) {
             ok = false;
             Par::Out() << "Error, face nodes don't match:" << face;

             Par::Out() << "First elem:";
             for (UInt i = 0; i < nodes.size(); i++)
               Par::Out() << " " << nodes[i]->get_id();

             Par::Out() << ", this elem:"; 
             for (UInt j = 0; j < etopo->num_nodes; j++)
               Par::Out() << " " << elem.Relations[side_nodes[perm[j]]].obj->get_id();

             Par::Out() << std::endl;
             Throw() << "Face verify relation err!!";
           }
         }
//if (ok) Par::Out() << "\t elem ok" << std::endl;
       }

       ++el;
     } // while relations
     if (nelem == 0) {
       // Better be a ghosted edge
       if (!GetMeshObjContext(face).is_set(Attr::SHARED_ID))
         Throw() << "Face:" << face << " has no elem!!";
     }
   
     nodes.clear();
  } // for edges

  return ok;
}

} // namespace 
} // namespace 
} // namespace 
