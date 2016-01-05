// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_HAdapt.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_Attr.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_MeshRefine.h>
#include <Mesh/include/ESMCI_RefineTopo.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MEField.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_MeshUtils.h>

#include <map>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

HAdapt::HAdapt(Mesh &_mesh) :
mesh(_mesh)
{

  // Register the node_marker and element marker fields
  Context ctxt; ctxt.flip();

  {
    node_marker = mesh.RegisterField("_node_hadapt_marker", MEFamilyStd::instance(),
                                     MeshObj::ELEMENT, ctxt, 1, 
                                     false, // output 
                                     false, // interp
                                     _fieldType<char>::instance());
  }
  {
    elem_marker = mesh.RegisterField("_hadapt_marker", MEFamilyDG0::instance(),
                                     MeshObj::ELEMENT, ctxt, 1, 
                                     false, // no output
                                     false, // no interp
                                     _fieldType<char>::instance());
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

  //refinement_resolution(); --Now done in RefineMesh.

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

void HAdapt::refinement_resolution() const {
  Trace __trace("HAdapt::refinement_resolution() const");
  
  // Get these out the way 
  mesh.remove_unused_kernels();
  
//Par::Out() << "Refine res, mesh:" << std::endl;
//mesh.Print(Par::Out());

  // Determine hanging nodes.  A node is 'hanging' if it is a child node
  // for an active element.  Simple, eh?
  {
    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end(), nn;
    for (; ni != ne; ) {
      nn = ni; ++nn;
      if (is_hanging(*ni)) {
        // Mark constrained
        const Context &ctxt = GetMeshObjContext(*ni);
        
        if (!ctxt.is_set(Attr::CONSTRAINED_ID)) {
          Context newctxt(ctxt);
          newctxt.set(Attr::CONSTRAINED_ID);
  
          Attr attr(GetAttr(*ni), newctxt);
          mesh.update_obj(&*ni, attr);
        }
      } else {
        // Mark unconstrained
        const Context &ctxt = GetMeshObjContext(*ni);
  
        if (ctxt.is_set(Attr::CONSTRAINED_ID)) {
          Context newctxt(ctxt);
          newctxt.clear(Attr::CONSTRAINED_ID);
    
          Attr attr(GetAttr(*ni), newctxt);
          mesh.update_obj(&*ni, attr);
        }
        
      }
      ni = nn;
    } // for nodes
  }  
  
  // Another fix:  During unrefinement/delete resolution, we tried
  // to figure out refine/active status.  However, this is not possible
  // until deleteion is resolved.  Rule: if a FACE or EDGE has its children
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

  /*
   * Now that hanging node constraints are shared, we can 
   * update constrained status of sides.  
   * We mark a side constrained if it USES a constrained node.  This 
   * USES is, of course, indirect, since edges don't have USES
   * relations.  Hence we get the USED_BY elements, edge node tables, and
   * verify this way.
   * 
   * Since an edge may be ghosted on a processor and not have an element,
   * we or the attributes after computing; one side will have the element
   * and node.  This is the reason we must call this after the syncatt
   * above, since we nodes to be globally consistent.
   * 
   * We only update active objects; the state of inactive objects
   * is indeterminant.
   */
  {
    Mesh::iterator ei = mesh.side_begin(), ee = mesh.side_end(), en;

    for (; ei != ee; ) {
      
      en = ei; ++en;
      
      MeshObj &side = *ei;

      bool constr = false;
      
      // Loop USED_BY elements:
      MeshObjRelationList::iterator ri = side.Relations.begin(), re = side.Relations.end();
      
      for (; !constr && ri != re; ++ri) {
        
        if (ri->type == MeshObj::USED_BY && ri->obj->get_type() == MeshObj::ELEMENT) {
          
          MeshObj &elem = *ri->obj;
          
          const MeshObjTopo *etopo = GetMeshObjTopo(elem);
          
          const int *side_nodes = etopo->get_side_nodes(ri->ordinal);
          
          for (UInt sn = 0; !constr && sn != etopo->num_side_nodes; ++sn) {
           
            // Child nodes may not all be there, so we must use find_rel.
            MeshObjRelationList::iterator ni =
              MeshObjConn::find_relation(elem, MeshObj::NODE, side_nodes[sn], MeshObj::USES);
            
            if (ni != elem.Relations.end())
              constr = GetMeshObjContext(*ni->obj).is_set(Attr::CONSTRAINED_ID);
            
          } // snn
          
        } // used_by, element

      } // ri
      
      
      // And now set the correct bit for the object.
      if (constr) {

        const Context &ctxt = GetMeshObjContext(side);
        
        if (!ctxt.is_set(Attr::CONSTRAINED_ID)) {
//Par::Out() << "Setting side:" << side.get_id() << " constrained" << std::endl;
          Context newctxt(ctxt);
          newctxt.set(Attr::CONSTRAINED_ID);
          Attr attr(GetAttr(side), newctxt);
          mesh.update_obj(&side, attr);
        }
        
      } else {
        
        const Context &ctxt = GetMeshObjContext(side);
        
        if (ctxt.is_set(Attr::CONSTRAINED_ID)) {
          Context newctxt(ctxt);
          newctxt.clear(Attr::CONSTRAINED_ID);
          Attr attr(GetAttr(side), newctxt);
          mesh.update_obj(&side, attr);
        }
        
      }
      
      ei = en;
    } // ei
  
    // Finally, we must share what we have found.
    mesh.SyncAttributes(mesh.side_type());
    
  }
  
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

void HAdapt::resolve_refinement_markers(std::vector<MeshObj*> &refine_list) {
  Trace __trace("HAdapt::resolve_refinement_markers(std::vector<MeshObj*> &refine_list)");

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

void HAdapt::resolve_unrefinement_markers(std::vector<MeshObj*> &unrefine_list) {
  Trace __trace("HAdapt::resolve_unrefinement_markers(std::vector<MeshObj*> &unrefine_list)");

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
  resolve_refinement_markers(refine_list);

  // And now, resolve unrefinement requests
  resolve_unrefinement_markers(unrefine_list);
}


void HAdapt::RefineMesh() {
  Trace __trace("HAdapt::RefineMesh()");
  
  ThrowRequire(!mesh.HasGhost()); // Cannot refine when ghosts present

  std::vector<MeshObj*>::iterator ri = refine_list.begin(), re = refine_list.end();

  for (; ri != re; ++ri) {
    RefineMeshObjLocal(**ri,  *GetHomoRefineTopo(GetMeshObjTopo(**ri)));
  }

  mesh.ResolvePendingCreate();

  MEField<> &coord = *mesh.GetCoordField();
  ProlongField(refine_list, coord);

  refinement_resolution();

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

  refinement_resolution();

  // Defragment stores, in case we left holes.
  mesh.CompactData();

}

static void OldProlongField(std::vector<MeshObj*> &elems, MEField<> &coord) {

  UInt fdim = coord.dim();
  MeshObjRelationList::iterator nri;
  for (UInt i = 0; i < elems.size(); i++) {
    MeshObj &elem = *elems[i];
   
    const MeshObjTopo *topo = GetMeshObjTopo(elem);
    // Start with edges
    for (UInt e = 0; e < (UInt) topo->num_edges; e++) {
      const int *edge_nodes = topo->get_edge_nodes(e);
      ThrowRequire(topo->num_edge_child_nodes == 3); // only doing linears now
      MeshObj *node[3]; double *d[3];
      for (UInt n = 0; n < 3; n++) {
        nri = MeshObjConn::find_relation(elem, MeshObj::NODE, edge_nodes[n]);
        ThrowRequire(nri != elem.Relations.end());
        node[n] = nri->obj;
        d[n] = coord.data(*node[n]);  ThrowAssert(d[n]);
      }
      for (UInt dm = 0; dm < fdim; dm++)
        d[2][dm] = 0.5*(d[0][dm] + d[1][dm]);
        //d[2][dm] = node[2]->get_id();
    } // e

    // Faces??
    // Centroid??
    if (topo->parametric_dim == 2 && topo->num_nodes == 4) { // quad (hard code this)
      MeshObj *node[5];
      double *d[5];
      for (UInt n = 0; n < 4; n++) {
        nri = MeshObjConn::find_relation(elem, MeshObj::NODE, n);
        ThrowRequire(nri != elem.Relations.end());
        node[n] = nri->obj;
        d[n] = coord.data(*node[n]);  ThrowAssert(d[n]);
      }
      nri = MeshObjConn::find_relation(elem, MeshObj::NODE, 8);
      ThrowRequire(nri != elem.Relations.end());
      node[4] = nri->obj;
      d[4] = coord.data(*node[4]); ThrowAssert(d[4]);
  
      for (UInt dm = 0; dm < fdim; dm++) {
        d[4][dm] = 0.25*(d[0][dm]+d[1][dm]+d[2][dm]+d[3][dm]);
      }
    } else if (topo->num_nodes == 8) { // hex 
      MeshObj *node[9];
      double *d[9];
      // Face centroids first
      for (UInt f = 0; f < 6; f++) {
        const int *side_nodes = topo->get_side_nodes(f);
        for (UInt n = 0; n < 4; n++) {
          nri = MeshObjConn::find_relation(elem, MeshObj::NODE, side_nodes[n]);
          ThrowRequire(nri != elem.Relations.end());
          node[n] = nri->obj;
          d[n] = coord.data(*node[n]);  ThrowAssert(d[n]);
        }
        nri = MeshObjConn::find_relation(elem, MeshObj::NODE, side_nodes[8]);
        ThrowRequire(nri != elem.Relations.end());
        node[4] = nri->obj;
        d[4] = coord.data(*node[4]); ThrowAssert(d[4]);
    
        for (UInt dm = 0; dm < fdim; dm++) {
          d[4][dm] = 0.25*(d[0][dm]+d[1][dm]+d[2][dm]+d[3][dm]);
        }
      }

      // Now the centroid of the element
      for (UInt n = 0; n < 8; n++) {
        nri = MeshObjConn::find_relation(elem, MeshObj::NODE, n);
        ThrowRequire(nri != elem.Relations.end());
        node[n] = nri->obj;
        d[n] = coord.data(*node[n]);  ThrowAssert(d[n]);
      }
      nri = MeshObjConn::find_relation(elem, MeshObj::NODE, 20);
      ThrowRequire(nri != elem.Relations.end());
      node[8] = nri->obj; d[8] = coord.data(*nri->obj); ThrowAssert(d[8]);
      for (UInt dm = 0; dm < fdim; dm++) {
        d[8][dm] = 0.125*(d[0][dm]+d[1][dm]+d[2][dm]+d[3][dm]+d[4][dm]+d[5][dm]+d[6][dm]+d[7][dm]);
      }
    } else if (topo->parametric_dim == 3 && topo->num_nodes == 4) { // tetrahedron
      // Nothing
    }
  } // elem
}

void HAdapt::ProlongField(std::vector<MeshObj*> &elems, MEField<> &coord) {

  UInt fdim = coord.dim();

  for (UInt i = 0; i < elems.size(); i++) {
    MeshObj &elem = *elems[i];

//Par::Out() << "Prolonging element:" << elem.get_id() << std::endl;
    MasterElementBase &meb = GetME(coord, elem);
    MasterElement<> &me = dynamic_cast<MasterElement<>&>(meb);
   
    const MeshObjTopo *topo = GetMeshObjTopo(elem);
    ThrowRequire(topo);
    const RefineTopo *rtopo = GetHomoRefineTopo(topo);
    ThrowRequire(rtopo);

    const RefDual *rdual = RefDual::instance(rtopo, &meb);

    // Gather parent coef
    UInt nfunc = me.num_functions();
    std::vector<double> pcoef(nfunc*fdim), ccoef(nfunc*fdim);

    GatherElemData<>(me, coord, elem, &pcoef[0]);

    for (UInt c = 0; c < rtopo->NumChild(); c++) {

      MeshObjRelationList::iterator cri =
          MeshObjConn::find_relation(elem, MeshObj::ELEMENT, c, MeshObj::CHILD);
 
      ThrowRequire(cri != elem.Relations.end());

      MeshObj &child = *cri->obj;

      rdual->apply_prolongation(fdim, c, &pcoef[0], &ccoef[0]);

      ScatterElemData(me, coord, child, &ccoef[0]);

    }

  }


}

/*---------------------------------------------------------------------------*/
// Refinement dual
/*---------------------------------------------------------------------------*/

RefDual::ref_map &get_ref_dual_map() {
  static RefDual::ref_map rd_map;

  return rd_map;
}

const RefDual *RefDual::instance(const RefineTopo *topo, const MasterElementBase *me) {

  ref_map &rd_map = get_ref_dual_map();

  ref_map::iterator ri = rd_map.find(std::make_pair(topo,me));

  RefDual *rd = 0;

  if (ri == rd_map.end()) {

    rd = new RefDual(topo, me);

    rd_map.insert(std::make_pair(std::make_pair(topo,me), rd));

  } else rd = ri->second;

  return rd;
}

RefDual::RefDual(const RefineTopo *_rtopo, const MasterElementBase *_me) :
nfunc(0),
prolong_matrices()
{
  build_matrices(_rtopo, _me);
}

void RefDual::build_matrices(const RefineTopo *_rtopo, const MasterElementBase *_me)
{

 // TODO: this function assumes child has same me as parent.  This may not
 // be true....

 // Build the prolongation matrices 

 const MasterElement<> &me = dynamic_cast<const MasterElement<>&>(*_me);
 const MasterElement<METraits<fad_type,double> > &fme = *me(METraits<fad_type,double>());

 // How to build the prolongation:
 // 1) set up mapping to child parametric coords from parent
 // 2) set parent coef fads to sensitive
 // 3) map interpolation points to child, extract matrix
 // 4) Apply interpolation on child, extract matrix
 // 5) compose matrices of step 3,4

 // First get parametric coordinates of interpolation points of child in parent space
 const MeshObjTopo *ptopo1 = _rtopo->GetParentTopo();
 ThrowRequire(ptopo1);


 // We with to use the mapping to compute parametric coords of the child interp
 // points in parent space, so we want to avoid going out to a higher dimension.
 const MeshObjTopo *ptopo = FlattenTopo(*ptopo1);

 UInt pdim = ptopo->parametric_dim; 
 ThrowRequire(ptopo->parametric_dim == ptopo->spatial_dim);

 UInt nip = me.NumInterpPoints();
 nfunc = me.num_functions();

 // Mapping for parametric coordinate space transforms
 Mapping<> *map = dynamic_cast<Mapping<>*>(Topo2Map()(ptopo->name));
 ThrowRequire(map);

Par::Out() << "pdim=" << pdim << ", nip=" << nip << ", nfunc=" << nfunc << std::endl;
Par::Out() << "ptopo->num_nodes=" << ptopo->num_nodes << std::endl;
 const double *ipoints = me.InterpPoints();

 std::vector<double> child_ipoints(nip*pdim);
 std::vector<double> cmdata(ptopo->num_nodes*pdim);

 prolong_matrices.resize(_rtopo->NumChild());
 for (UInt c = 0; c < _rtopo->NumChild(); c++) {

   const UInt *child_nodes = _rtopo->ChildNode(c);
   const double *node_coord = ptopo->node_coord();

   for (UInt n = 0; n < ptopo->num_nodes; n++) {
     for (UInt p = 0; p < pdim; p++) {
       cmdata[n*pdim+p] = node_coord[pdim*child_nodes[n]+p];
     }
   }

   map->forward(nip, &cmdata[0], ipoints, &child_ipoints[0]);
#ifdef ESMF_PARLOG
std::copy(child_ipoints.begin(), child_ipoints.end(), std::ostream_iterator<double>(Par::Out(), " "));
#endif
Par::Out() << "\n end child ipoints" << std::endl;

   // Now do an interpolation from parent to these points.  Record sensitivities.
   std::vector<fad_type> pfdata(nfunc, 0), cfval(nip,0);

   for (UInt i = 0; i < nfunc; i++) {
     pfdata[i] = 1.0; pfdata[i].diff(i, nfunc);
   }

   fme.function_values(nip, 1, &child_ipoints[0], &pfdata[0], &cfval[0]);

   // We now know how to take parent coef to values at child interp points.
   // Now we must see how to get child coeff from such values.
   std::vector<fad_type> ccoef(nfunc);

   fme.Interpolate(1, &cfval[0], &ccoef[0]);

/*
std::copy(ccoef.begin(), ccoef.end(), std::ostream_iterator<fad_type>(Par::Out(), " ")); 
Par::Out() << std::endl;
*/

   std::vector<double> &cmatrix = prolong_matrices[c];
   cmatrix.resize(nfunc*nfunc);

Par::Out() << "topo:" << ptopo->name << ", child:" << c << std::endl;
   for (UInt i = 0; i < nfunc; i++) {
     const double *dx = &(ccoef[i].fastAccessDx(0));
     for (UInt j = 0; j < nfunc; j++) {
       cmatrix[i*nfunc+j] = dx[j];
Par::Out() << cmatrix[i*nfunc+j] << " ";
     }
Par::Out() << std::endl;
   }

 } // child

}

void RefDual::apply_prolongation(UInt fdim, UInt child_num, const double parent_mcoef[], double child_mcoef[]) const {

  ThrowRequire(child_num < prolong_matrices.size());
  const std::vector<double> &cmatrix = prolong_matrices[child_num];

  for (UInt f = 0; f < fdim; f++) {
    for (UInt i = 0; i < nfunc; i++) {
      UInt ixfdim = i*fdim;
      child_mcoef[ixfdim+f] = 0;
      for (UInt j = 0; j < nfunc; j++) {
        child_mcoef[ixfdim+f] += cmatrix[i*nfunc+j]*parent_mcoef[j*fdim+f];
      }
    }
  }

}


} // namespace
