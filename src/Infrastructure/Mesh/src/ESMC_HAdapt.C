// $Id: ESMC_HAdapt.C,v 1.6.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_HAdapt.h>
#include <ESMC_Mesh.h>
#include <ESMC_Attr.h>
#include <ESMC_MeshObjConn.h>
#include <ESMC_MeshRefine.h>
#include <ESMC_RefineTopo.h>
#include <ESMC_Mesh.h>
#include <ESMC_Exception.h>
#include <ESMC_MEField.h>
#include <ESMC_ParEnv.h>

namespace ESMCI {
namespace MESH {

HAdapt::HAdapt(Mesh &_mesh) :
mesh(_mesh)
{

  // Register the node_marker and element marker fields
  Context ctxt; ctxt.flip();

  {
    node_marker = mesh.RegisterField("_node_hadapt_marker", MEFamilyStd::instance(),
                                     MeshObj::ELEMENT, ctxt, 1, false, false, _fieldType<char>::instance());
  }
  {
    elem_marker = mesh.RegisterField("_hadapt_marker", MEFamilyDG0::instance(),
                                     MeshObj::ELEMENT, ctxt, 1, false, false, _fieldType<char>::instance());
  }
}

void HAdapt::RefineUniformly(bool keep_parents) {
  // Loop mesh.  Mark all elements
  Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
  for (; ei != ee; ++ei) {
     MarkElement(*ei, ELEM_REFINE);
  }

  MarkerResolution(); 

  RefineMesh();

  RefinementResolution();

  // If not to keep parents, mark all inactive objects to delete
  if (!keep_parents) {
    KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
    for (; ki != ke; ++ki) {
      Kernel &ker = *ki;
      // select all objs of this type and not the newly created ones (which have bogus ids)
      if (!ker.GetContext().is_set(Attr::PENDING_DELETE_ID) 
          && !ker.GetContext().is_set(Attr::ACTIVE_ID)) {

        Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end(), on;
        for (; oi !=oe;) {

          // Object may be deleted from list, so get next in advance.
          on = oi; ++on;

          // Mark object to delete
          MeshObj &obj = *oi;
          const Context &ctxt = GetMeshObjContext(obj);
          Context newctxt(ctxt);
          newctxt.set(Attr::PENDING_DELETE_ID);
          Attr attr(GetAttr(obj), newctxt);
          mesh.update_obj(&obj, attr);

          oi = on;

        } // for oi
      } // if a candidate

    } // for kernel

    mesh.ResolvePendingDelete();

    // Mark all objects with the genesis bit
    ki = mesh.set_begin(); ke = mesh.set_end();
    for (; ki != ke; ++ki) {
      Kernel &ker = *ki;
      Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end(), on;
      for (; oi !=oe;) {

        // Object may be deleted from list, so get next in advance.
        on = oi; ++on;

        // Mark object to delete
        MeshObj &obj = *oi;
        const Context &ctxt = GetMeshObjContext(obj);
        Context newctxt(ctxt);
        newctxt.set(Attr::GENESIS_ID);
        if (newctxt != ctxt) {
          Attr attr(GetAttr(obj), newctxt);
          mesh.update_obj(&obj, attr);
        }

        oi = on;

      } // for oi

    } // for kernel

  } // !keep_parents

  mesh.remove_unused_kernels();
}

void HAdapt::ZeroMarker() const {

  Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
  for (; ei != ee; ++ei) {

    char *marker = elem_marker->data(*ei);
    ThrowAssert(marker);
    
     *marker = 0;
  }

}

void HAdapt::MarkElement(const MeshObj &elem, int mark) const {

  ThrowRequire(elem.get_type() == MeshObj::ELEMENT);

  char *marker = elem_marker->data(elem);
  ThrowAssert(marker);

  if (mark == ELEM_UNREFINE) {

    *marker = ELEM_REQUEST_UNREFINE;

  } else if (mark == ELEM_REFINE) {

    *marker = ELEM_REFINE;

  } else Throw() << "Unknown mark:" << mark;
}

static bool is_hanging(const MeshObj &node) {
  // Traverse all parent elements
  MeshObjRelationList::const_iterator ei = MeshObjConn::find_relation(node, MeshObj::ELEMENT);

  for (; ei != node.Relations.end() && ei->obj->get_type() == MeshObj::ELEMENT; ++ei) {
    if (ei->type == MeshObj::PARENT) {
      if (GetMeshObjContext(*ei->obj).is_set(Attr::ACTIVE_ID)) {
//Par::Out() << "Node " << node.get_id() << " hangs from parent:" << ei->obj->get_id() << std::endl;
        return true;
      }
    }
  }

  return false;
}

void HAdapt::RefinementResolution() const {
  Trace __trace("HAdapt::RefinementResolution() const");

//Par::Out() << "Refine res, mesh:" << std::endl;
//mesh.Print(Par::Out());

  // Determine hanging nodes.  A node is 'hanging' if it is a child node
  // for an active element.  Simple, eh?
  Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end(), nn;
  for (; ni != ne; ) {
    nn = ni; ++nn;
    if (is_hanging(*ni)) {
      // Mark constrained
      const Context &ctxt = GetMeshObjContext(*ni);
      Context newctxt(ctxt);
      newctxt.set(Attr::CONSTRAINED_ID);

      if (newctxt != ctxt) {
        Attr attr(GetAttr(*ni), newctxt);
        mesh.update_obj(&*ni, attr);
      }
    } else {
      // Mark unconstrained
      const Context &ctxt = GetMeshObjContext(*ni);
      Context newctxt(ctxt);
      newctxt.clear(Attr::CONSTRAINED_ID);

      if (newctxt != ctxt) {
        Attr attr(GetAttr(*ni), newctxt);
        mesh.update_obj(&*ni, attr);
      }
    }
    ni = nn;
  } // for nodes

  // Another fix:  During unrefinement/delete resolution, we tried
  // to figure out refine/active status.  However, this is not possible
  // until deleteion is resolve.  Rule: if a FACE or EDGE has its children
  // it is refined and inactive, else unrefined and active
  Mesh::iterator oi = mesh.obj_begin_all(), oe = mesh.obj_end_all(), on;
  for (; oi != oe;) {
    on = oi; ++on;
    if (oi->get_type() == MeshObj::EDGE || oi->get_type() == MeshObj::FACE) {

      MeshObj &obj = *oi;
      const Context &ctxt = GetMeshObjContext(obj);
      Context newctxt(ctxt);

      // Children?
      MeshObjRelationList::iterator ci = MeshObjConn::find_relation(obj, obj.get_type(), 0, MeshObj::CHILD);
      if (ci != obj.Relations.end()) {
        newctxt.set(Attr::REFINED_ID);
        newctxt.clear(Attr::ACTIVE_ID);
        if (newctxt != ctxt) {
          Attr attr(GetAttr(obj), newctxt);
          mesh.update_obj(&obj, attr);
        }
      } else {
        newctxt.clear(Attr::REFINED_ID);
        newctxt.set(Attr::ACTIVE_ID);
        if (newctxt != ctxt) {
          Attr attr(GetAttr(obj), newctxt);
          mesh.update_obj(&obj, attr);
        }

      }

    }

    oi = on;
  }

  // Parallel sync of attributes.
  mesh.SyncAttributes();

  // Make sure coordinate fields are assigned good values
  {
    MEField<> *cf = mesh.GetCoordField();
    mesh.HaloFields(1, &cf);
  }

}

static void set_used_nodes(MeshObj &elem, MEField<> *node_marker) {
  MeshObjRelationList::iterator ri = elem.Relations.begin();

  while (ri != elem.Relations.end() &&
            ri->obj->get_type() == MeshObj::NODE && ri->type == MeshObj::USES)
  {
    char *nmark = node_marker->data(*ri->obj);
    ThrowAssert(nmark);
    *nmark = 1;
    ++ri;
  }
}

static int mark_cnode_parents(MeshObj &cnode, MEField<> *node_marker, MEField<> *elem_marker) {

  char *nmark = node_marker->data(cnode);
  ThrowAssert(nmark);
  
  if (*nmark <= 0) return 0; // don't need to mark parents.

//Par::Out() << "Checking cnode" << std::endl;

  int nmarked = 0;

  MeshObjRelationList::iterator ri = cnode.Relations.begin();

  while (ri != cnode.Relations.end() &&
            ri->obj->get_type() == MeshObj::ELEMENT)
  {

    if (ri->type == MeshObj::PARENT) {

      if (GetMeshObjContext(*ri->obj).is_set(Attr::ACTIVE_ID)) {
 // Par::Out() << "found active parent" << std::endl;
        char *emark = elem_marker->data(*ri->obj);
        ThrowAssert(emark);
        if (*emark != HAdapt::ELEM_REFINE) {
          *emark = HAdapt::ELEM_REFINE;
          nmarked++; 
        }
      }
    }
    ++ri;
  }
  return nmarked;
}

void HAdapt::resolve_refinement(std::vector<MeshObj*> &refine_list) {
  Trace __trace("HAdapt::resolve_refinement(std::vector<MeshObj*> &refine_list)");

  int nproc = Par::Size();

  // Every element marked for refinement WILL refine.  We must propogate
  // the 2-1 rule so that we don't create cascading hanging nodes.

  // Zero node marker field
  {
    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
    for (; ni != ne; ++ni) {
      char *nmark = node_marker->data(*ni);
      ThrowAssert(nmark);
      *nmark = 0;
    }
  }

  // Iterate until mesh doesn't change
  bool done = 0;
  int total_marked;
  int total_marked_g;
  while(!done) {
    total_marked = total_marked_g = 0;
    // Now, set all nodes that are USED by a refinement element to 1;
    {
      Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
      for (; ei != ee; ++ei) {
        MeshObj &elem = *ei;
      
        char *emark = elem_marker->data(elem);
        ThrowAssert(emark);
  
        if (*emark == ELEM_REFINE) set_used_nodes(elem, node_marker);
      }
    }
  
    // Now, for parallel, swap add the field.  After this operation, all
    // nodes that are used by an element refining will have values > 0
    mesh.SwapOp<char>(1, &node_marker, CommRel::OP_SUM);
  
    // We complete the 2-1 rule as follows: any hanging node with val > 0 must
    // force any active parent to refine.
    Context c; c.set(Attr::CONSTRAINED_ID);
    Attr a(MeshObj::NODE, c);
    Mesh::iterator csi(mesh.set_begin(), mesh.set_end(), a), cse(mesh.set_end(), mesh.set_end());
  
    for (; csi != cse; ++csi) {
      MeshObj &cnode = *csi;
  //std::cout << "Constrained node:" << cnode.get_id() << std::endl;
      total_marked += mark_cnode_parents(cnode, node_marker, elem_marker);
    }


    if (nproc > 1) {
      MPI_Allreduce(&total_marked, &total_marked_g, 1, MPI_INT, MPI_SUM, Par::Comm());
    } else {
      total_marked_g = total_marked;
    }


#ifdef REF_DEBUG
Par::Out() << "total_marked=" << total_marked_g << std::endl;
#endif
    if (total_marked_g == 0) done = true;
  } // 2-1 iteration
  
  // Now build the refinement list
  {
    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) {
      MeshObj &elem = *ei;
    
      char *emark = elem_marker->data(elem);
      ThrowAssert(emark);

      if (*emark == ELEM_REFINE) refine_list.push_back(&elem);
    }
  }


}

void HAdapt::resolve_unrefinement(std::vector<MeshObj*> &unrefine_list) {
  Trace __trace("HAdapt::resolve_unrefinement(std::vector<MeshObj*> &unrefine_list)");

  // Firstly, enforce the following basic rules; an element can unrefine only if:
  //  -1) elem not refined
  //   0) element has a parent
  //   1) all sibilings are marked for unrefinement
  //   2) no sibling is refined.

  // Store all potential unrefiners.
  std::vector<MeshObj*> may_unrefine_list;

  Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
  for (; ei != ee; ++ei) {
  
    MeshObj &elem = *ei;

    // refined?
    if (GetMeshObjContext(elem).is_set(Attr::REFINED_ID)) continue;

    char *emark = elem_marker->data(elem);
    ThrowAssert(emark);

    if (*emark != ELEM_REQUEST_UNREFINE) continue;

    // Get siblings
    {
      std::vector<MeshObj*> siblings;
  
      // A way to get parent.
      MeshObj::Relation r;
      r.obj = &elem; r.ordinal = 0; r.type = MeshObj::PARENT;
      MeshObjRelationList::const_iterator lb = std::lower_bound(elem.Relations.begin(), elem.Relations.end(), r);
  
      // For interior objects, there may not be parents, so the object may be toast.
      if (lb == elem.Relations.end() ||
                    !(lb->obj->get_type() == elem.get_type() && lb->type == MeshObj::PARENT)) {
        // Enforce 0) Object has no parent, can't unrefine
        *emark = 0;
        continue;
      }

      MeshObj &parent = *lb->obj;

      // When children are created, all are created, so get 0th one.
      MeshObjRelationList::iterator ci = MeshObjConn::find_relation(parent, parent.get_type(), 0, MeshObj::CHILD);
      for (; ci != parent.Relations.end() && ci->obj->get_type() == parent.get_type() && ci->type == MeshObj::CHILD; ++ci) {
        if (ci->obj != &elem)
          siblings.push_back(ci->obj);
      }

      // Now check 1, 2
      bool ok_refine = true;
      for (UInt c = 0; ok_refine && c < siblings.size(); c++) {
        MeshObj &sib = *siblings[c];
        char *semark = elem_marker->data(sib);
        ThrowAssert(semark);

        // If 1 or 2, mark all siblings to not unrefine
        if (*semark != ELEM_REQUEST_UNREFINE || GetMeshObjContext(sib).is_set(Attr::REFINED_ID)) {
          for (UInt j = 0; j < siblings.size(); j++) {
            char *rsemark = elem_marker->data(*siblings[j]);
            *rsemark = 0;
            ThrowAssert(rsemark);
          }
          ok_refine = false;
        }
        
      } // for c

      if (ok_refine) {
#ifdef REF_DEBUG
Par::Out() << "Element " << elem.get_id() << " and siblings ok to unrefine, check 1" << std::endl;
#endif
        // Mark everyone as actual REFINE, so they aren't investigated again.
        *emark = ELEM_UNREFINE;
        for (UInt c = 0;  c < siblings.size(); c++) {
          MeshObj &sib = *siblings[c];
          char *semark = elem_marker->data(sib);
          ThrowAssert(semark);
          *semark = ELEM_UNREFINE;
        }

        // Save parent as a potential unrefiner
        may_unrefine_list.push_back(&parent);

      } // ok to unrefine
      else {
#ifdef REF_DEBUG
 Par::Out() << "Element " << elem.get_id() << " and siblings NOT ok to unrefine, check 1" << std::endl;
#endif
      }

    } // siblings

  } // for e .


  // Now, the harder part II.  Make sure unrefinement doesn't mess up the 2-1 rule.
  // Strategy: slightly more strict than necessary, but simple to implement:
  // Parent USES of hnodes can't become future hnodes (could cascade).
  // Also, USES of refining elements can't become future hnodes

  // Zero node marker field
  {
    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
    for (; ni != ne; ++ni) {
      char *nmark = node_marker->data(*ni);
      ThrowAssert(nmark);
      *nmark = 0;
    }
  }

  // Loop hnodes, mark parent USES as no HNODE
  const char NO_FUTURE_HNODE = 1;

  // We complete the 2-1 rule as follows: any hanging node with val > 0 must
  // force any active parent to refine.
  Context c; c.set(Attr::CONSTRAINED_ID);
  Attr a(MeshObj::NODE, c);
  Mesh::iterator csi(mesh.set_begin(), mesh.set_end(), a), cse(mesh.set_end(), mesh.set_end());

  for (; csi != cse; ++csi) {
    MeshObj &cnode = *csi;
    
    // Find parents
    MeshObjRelationList::iterator pi = cnode.Relations.begin(), pe = cnode.Relations.end();
    for (; pi != pe; ++pi) {
      if (pi->obj->get_type() == MeshObj::ELEMENT &&
          pi->type == MeshObj::PARENT) {

        MeshObj &parent = *pi->obj;

        // Loop USES nodes
        MeshObjRelationList::iterator pni = parent.Relations.begin();
        for (; pni != parent.Relations.end() &&
               pni->obj->get_type() == MeshObj::NODE &&
               pni->type == MeshObj::USES;
               ++pni)
        {
          MeshObj &node = *pni->obj;
          char *nmark = node_marker->data(node);
          ThrowAssert(nmark);
          *nmark = NO_FUTURE_HNODE;
        }
      } // parent
    } // parents
  } // cnodes

  // Now do the same to USES nodes for refining elements
  ei = mesh.elem_begin();
  for (; ei != ee; ++ei) {
  
    MeshObj &elem = *ei;

    char *emark = elem_marker->data(elem);
    ThrowAssert(emark);

    if (*emark == ELEM_REFINE) {
      // Loop USES nodes
      MeshObjRelationList::iterator pni = elem.Relations.begin();
      for (; pni != elem.Relations.end() &&
             pni->obj->get_type() == MeshObj::NODE &&
             pni->type == MeshObj::USES;
             ++pni)
      {
        MeshObj &node = *pni->obj;
        char *nmark = node_marker->data(node);
        ThrowAssert(nmark);
        *nmark = NO_FUTURE_HNODE;
      }
    } // refining

  } // for ei


  // Union marker in parallel
  mesh.SwapOp<char>(1, &node_marker, CommRel::OP_SUM);

  // Loop potential unrefiners; decide fates
  for (UInt u = 0; u < may_unrefine_list.size(); u++) {
    
    MeshObj &elem = *may_unrefine_list[u];

    // If a child node with the NO_FUTURE, then can't do it!
    MeshObjRelationList::iterator cni =
       elem.Relations.begin() + GetMeshObjTopo(elem)->num_nodes;

    bool ok_unrefine = true;
    for (; ok_unrefine && cni != elem.Relations.end() &&
           cni->obj->get_type() == MeshObj::NODE &&
           cni->type == MeshObj::CHILD;  ++cni) 
    {
      MeshObj &node = *cni->obj;
      char *nmark = node_marker->data(node);
      ThrowAssert(nmark);

      if (*nmark >= NO_FUTURE_HNODE) ok_unrefine = false;

    }


    // Mark true if can unrefine.  Avoid duplicates by inserting sorted.
    if (ok_unrefine) {
#ifdef REF_DEBUG
Par::Out() << "Elem:" << elem.get_id() << " passed check 2 unrefine" << std::endl;
#endif
      std::vector<MeshObj*>::iterator lb =
        std::lower_bound(unrefine_list.begin(), unrefine_list.end(), &elem);
      if (lb == unrefine_list.end() || *lb != &elem)
        unrefine_list.insert(lb, &elem);
    }
#ifdef REF_DEBUG
else {
Par::Out() << "Elem:" << elem.get_id() << " FAILED check 2 unrefine" << std::endl;
}
#endif


  } // may unrefine list

}

void HAdapt::MarkerResolution() {

  // Clear lists
  std::vector<MeshObj*>().swap(refine_list);
  std::vector<MeshObj*>().swap(unrefine_list);

  // Propogate 2-1 rule for refinement requests
  resolve_refinement(refine_list);

  // And now, resolve unrefinement requests
  resolve_unrefinement(unrefine_list);
}


void HAdapt::RefineMesh() {
  Trace __trace("HAdapt::RefineMesh()");
  
  ThrowRequire(!mesh.HasGhost()); // Cannot refine when ghosts present

  std::vector<MeshObj*>::iterator ri = refine_list.begin(), re = refine_list.end();

  for (; ri != re; ++ri) {
    RefineMeshObjLocal(**ri,  *GetHomoRefineTopo(GetMeshObjTopo(**ri)->name));
  }

  mesh.ResolvePendingCreate();

  MEField<> &coord = *mesh.GetCoordField();
  ProlongNodeCoords(refine_list, coord);

}

void HAdapt::UnrefineMesh() {
  Trace __trace("HAdpat::UnrefineMesh()");
  
  ThrowRequire(!mesh.HasGhost()); // Cannot refine when ghosts present

  std::vector<MeshObj*>::iterator ri = unrefine_list.begin(), re = unrefine_list.end();

  for (; ri != re; ++ri) {
#ifdef REF_DEBUG
Par::Out() << "UNREFINING:" << (*ri)->get_id() << std::endl;
#endif
    UnrefineMeshObjLocal(**ri);
  }

  mesh.ResolvePendingDelete();
}

} // namespace
} // namespace
