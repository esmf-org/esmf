// $Id: ESMC_MeshRefine.C,v 1.2.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshRefine.h>
#include <ESMC_Kernel.h>
#include <ESMC_Attr.h>
#include <ESMC_MeshObjConn.h>
#include <ESMC_Exception.h>
#include <ESMC_ParEnv.h>

#include <functional>
#include <algorithm>
#include <iterator>

//#define REF_DEBUG

namespace ESMCI {
namespace MESH {

// Same as below, but instead of having the side object, we only
// have the nodes that represent it.
// Returns true if it found anything beyond the obvious USES nodes, else false.
static bool retrieve_nodes(std::vector<MeshObj*> &snodes, const MeshObjTopo *topo, std::vector<MeshObj*> &nodes) {
  // Now, if any other nodes are to be found, it is because a peer (edge across or face across)
   // has refined.  So search for this:
#ifdef REF_DEBUG
Par::Out() << " ( ";
for (UInt i = 0; i < topo->num_nodes; i++)
 Par::Out() << snodes[i]->get_id() << " ";
Par::Out() << ") ";
#endif

   std::vector<MeshObj*> elems;
   MeshObjConn::common_objs(snodes.begin(), snodes.begin() + topo->num_nodes,
                            MeshObj::USED_BY, MeshObj::ELEMENT, elems);

   bool found = false;
   for (UInt i = 0; !found && i < elems.size(); i++) {
//Par::Out() << " e" << elems[i]->get_id() <<  " ";
     if (!GetMeshObjContext(*elems[i]).is_set(Attr::REFINED_ID)) continue; // not a help
//Par::Out() << "r ";
     // Found a refined peer!  Great! recruit the nodes.  The only hard
     // part is to deal with the rotation/orientation to see how the
     // peer nodes relate to me.
     found = true; // stop loop
     MeshObj &elem = *elems[i];

     // Get side info for this guy
     int ordinal;
     int polarity;
     int rotation = 0;
     if (topo->parametric_dim == 2) {
       MeshObjConn::face_info(&snodes[0], &snodes[topo->num_nodes], &elems[i], &elems[i+1], &ordinal, &polarity, &rotation);
     } else {
       ThrowRequire(topo->parametric_dim == 1);
       MeshObjConn::edge_info(&snodes[0], &snodes[topo->num_nodes], &elems[i], &elems[i+1], &ordinal, &polarity);
     }

     // Get the side numbering array for the other object;
     const int *perm = topo->perm_table(rotation, polarity);
//Par::Out() << "rot,pol,ord=" << rotation << "," << polarity << "," << ordinal << " ";
     const MeshObjTopo *otopo = GetMeshObjTopo(elem);
     
     // Get the side node table for other element, this side
     const int *oside_nodes = topo->parametric_dim == 2 ? otopo->get_side_nodes(ordinal) : otopo->get_edge_nodes(ordinal);
     for (UInt n = topo->num_nodes; n < (UInt) topo->num_child_nodes; n++) {
       MeshObjRelationList::iterator nri = MeshObjConn::find_relation(elem, MeshObj::NODE, oside_nodes[perm[n]]);
//Par::Out() << "n=" << n << " perm[n]=" << perm[n] << " ";
//Par::Out() << "looking for " << oside_nodes[perm[n]] << " ";
       ThrowRequire(nri != elem.Relations.end()); // since refine, the better be there!
       nodes[n] = nri->obj;
//Par::Out() << nodes[n]->get_id() << " ";
     }
     

   } // for elems

   return found;
}

static void retrieve_nodes(MeshObj &obj, const MeshObjTopo *topo, std::vector<MeshObj*> &nodes) {

  // Low hanging fruit first, i.e. the USES nodes
  if (obj.get_type() == MeshObj::ELEMENT) {
    for (UInt i = 0; i < topo->num_nodes; i++) {
      nodes[i] = obj.Relations[i].obj;
//Par::Out() << nodes[i]->get_id() << " ";
    }
    // Now possibly child nodes:
    for (UInt i = topo->num_nodes; i < (UInt) topo->num_child_nodes; i++) {
      MeshObjRelationList::iterator ni = MeshObjConn::find_relation(obj, MeshObj::NODE, i, MeshObj::CHILD);
      if (ni != obj.Relations.end()) {
        nodes[i] = ni->obj;
//Par::Out() << nodes[i]->get_id() << " ";
      }
    }
  } else Throw() << "Did not expect this branch!!";
  // Now have base nodes in place.  Look for any child nodes

  // If nodes exist, they will be on the side or edge of an element.  Hence, if we are an element,
  // call recursively for each side.  We must also drill down to the edge level, because, for instance,
  // the side may be an exposed boundary, in which case there is no element across to mine a node
  // from; only the element across from the edge can provide it!!
  if (obj.get_type() == MeshObj::ELEMENT) {
    // We have to assume that a side object doesn't exist, since
    // it may node.  Use nodes instead.
    for (UInt s = 0; s < topo->num_sides; s++) {
      const MeshObjTopo *stopo = topo->side_topo(s);
      const int *side_nodes = topo->get_side_nodes(s);
      std::vector<MeshObj*> snodes(stopo->num_child_nodes);;
      for (UInt nsn = 0; nsn < stopo->num_nodes; nsn++) snodes[nsn] = 
         nodes[side_nodes[nsn]]; 

#ifdef REF_DEBUG
Par::Out() << "r_sn (" << topo->name <<")";
#endif
      if (retrieve_nodes(snodes, stopo, snodes)) {
        // found some child nodes; load these into the element array (any beyond the child)
        for (UInt nsn = stopo->num_nodes; nsn < (UInt) stopo->num_child_nodes; nsn++) {
          nodes[side_nodes[nsn]] = snodes[nsn];
#ifdef REF_DEBUG
Par::Out() << "<" << side_nodes[nsn] << "> " <<nodes[side_nodes[nsn]]->get_id() << " ";
#endif
        }
      } // else nothing found
    } // s

    // And now down to edges.  Do a quick check just in case we have
    // already found everything:
    if (GetMeshObjMesh(obj).parametric_dim() == 3) {
      bool need_edges = false;
      for (UInt i = topo->num_nodes; !need_edges && i < topo->num_child_nodes; i++)
        if (nodes[i] == NULL) need_edges = true;
  
      if (need_edges) {
        for (UInt s = 0; s < topo->num_edges; s++) {
          const MeshObjTopo *etopo = topo->edge_topo(s);
          const int *edge_nodes = topo->get_edge_nodes(s);
          std::vector<MeshObj*> enodes(etopo->num_child_nodes);;
          for (UInt nsn = 0; nsn < etopo->num_nodes; nsn++) enodes[nsn] = 
             nodes[edge_nodes[nsn]]; 
    
          if (retrieve_nodes(enodes, etopo, enodes)) {
            // found some child nodes; load these into the element array (any beyond the child)
            for (UInt nsn = etopo->num_nodes; nsn < (UInt) etopo->num_child_nodes; nsn++) {
              nodes[edge_nodes[nsn]] = enodes[nsn];
            }
          } // else nothing found
        } // s
      }
    } // dim 3

  } else {
#ifdef REF_DEBUG
Par::Out() << "r_n ";
#endif
    Throw() << "Did not expect THIS branch!!";
  }

  // Make sure all nodes are active: nodes may be inactive if they are here only as ghosts
  for (UInt i = 0; i < nodes.size(); i++) {
    MeshObj *node = nodes[i];
    if (!node || GetAttr(*node).get_context().is_set(Attr::ACTIVE_ID)) continue;
    const Attr &oattr = GetAttr(*node);
    const Context &ctxt = GetMeshObjContext(*node);
    Context newctxt(ctxt);
    newctxt.set(Attr::ACTIVE_ID); // TODO: maybe this should still be active?? Other side might use it.
    Attr attr(oattr, newctxt);
    GetMeshObjMesh(*node).update_obj(node, attr);
  }

}

/*------------------------------------------------------------*/
// ImprintExterior: Imprint exterior boundary on  
//   nodes that are on the boundary.
/*------------------------------------------------------------*/
void ImprintExterior(MeshObj &obj) {
  const MeshObjTopo *topo = GetMeshObjTopo(obj);
  MeshDB &mesh = GetMeshObjMesh(obj);

  for (UInt s = 0; s < topo->num_sides; s++) {
    const int *side_nodes = topo->get_side_nodes(s);
    bool exterior = true;
    for (UInt i = 0; exterior && i < (UInt) topo->num_side_nodes; i++) {
      MeshObjRelationList::iterator nri = MeshObjConn::find_relation(obj, 
                       MeshObj::NODE, side_nodes[i]);
      ThrowRequire(nri != obj.Relations.end());
      if (!nri->obj->GetKernel()->is_exposed()) {
        exterior = false;
      }
    } // side nodes

    // Mark child nodes, if appropriate
    if (exterior) {
      for (UInt i = topo->num_side_nodes; i < (UInt) topo->num_side_child_nodes; i++) {
        MeshObjRelationList::iterator nri = MeshObjConn::find_relation(obj, 
                       MeshObj::NODE, side_nodes[i], MeshObj::CHILD);
        ThrowRequire(nri != obj.Relations.end());
        MeshObj &node = *nri->obj;
        const Attr &oattr = GetAttr(node);
        const Context &ctxt = GetMeshObjContext(node);
        Context newctxt(ctxt);
        newctxt.set(Attr::EXPOSED_BOUNDARY_ID);
        if (newctxt != ctxt) { // context could have been imprinted by neighbor
          Attr attr(oattr, newctxt);
          mesh.update_obj(&node, attr);
        }
      }
    }
  } // sides
}


static void create_interior_objs(MeshObj &obj, const RefineTopo &rtopo, UInt obj_type) {
  Trace __trace("create_interior_objs(MeshObj &obj, const RefineTopo &rtopo, UInt obj_type)");

  ThrowRequire(obj.get_type() == MeshObj::ELEMENT);

  MeshDB &mesh = GetMeshObjMesh(obj);

  // Loop children
  for (UInt c = 0; c < rtopo.NumChild(); c++) {
    MeshObjRelationList::iterator cr = MeshObjConn::find_relation(obj,
                   MeshObj::ELEMENT, c, MeshObj::CHILD);
    ThrowRequire(cr != obj.Relations.end());

    const MeshObjTopo *ctopo = rtopo.ChildTopo(c);
    ThrowRequire(ctopo);

    // Loop objtype
    for (UInt o = 0; o < (obj_type == MeshObj::FACE ? ctopo->num_sides : ctopo->num_edges); o++) {
      // Does the object exist?
      MeshObjRelationList::iterator coi = MeshObjConn::find_relation(*cr->obj,
               obj_type, o, MeshObj::USES);

      if (coi != cr->obj->Relations.end()) continue; // object exists
//Par::Out() << "Creating obj:" << MeshObjTypeString(obj_type) << ", child:" << c << ", ord:" << o << std::endl;
        Context newctxt;
        newctxt.set(Attr::PENDING_CREATE_ID);
        newctxt.set(Attr::ACTIVE_ID);

        const MeshObjTopo *cotopo = (obj_type == MeshObj::FACE ? ctopo->side_topo(o) : ctopo->edge_topo(o));

        MeshObj *newobj = new MeshObj(obj_type, mesh.get_new_local_id(obj_type));

        // Use add_side/edge_local, since these functions connect up to any
        // neighboring objects.  In this way, if the face we create can be
        // used by another child, the connections will be automatically made.
        if (obj_type == (UInt) mesh.side_type()) {
          mesh.add_side_local(*newobj, *cr->obj, o, 0, cotopo, newctxt);
        } else {
          ThrowRequire(mesh.parametric_dim() == 3 && obj_type == MeshObj::EDGE);
          mesh.add_edge_local(*newobj, *cr->obj, o, 0, cotopo, newctxt);
        }
    }
  } // children
}

static void create_interior_edges(MeshObj &obj, const RefineTopo &rtopo) {
  create_interior_objs(obj, rtopo, MeshObj::EDGE);
}

static void create_interior_faces(MeshObj &obj, const RefineTopo &rtopo) {
  create_interior_objs(obj, rtopo, MeshObj::FACE);
}

/*------------------------------------------------------------*/
// RefineMeshObjLocal: refine a mesh object on the
//   local processor.
/*------------------------------------------------------------*/

void RefineMeshObjLocal(MeshObj &obj, const RefineTopo &rtopo) {
  Trace __trace("RefineMeshObjLocal(MeshObj &obj, const RefineTopo &rtopo)");
#ifdef REF_DEBUG
Par::Out() << "Refining object:" << MeshObjTypeString(obj.get_type()) << ":" << obj.get_id() << std::endl;
#endif

  if (GetMeshObjContext(obj).is_set(Attr::REFINED_ID)) {

#ifdef REF_DEBUG
Par::Out() << MeshObjTypeString(obj.get_type()) << " already refined, children" << std::endl;
MeshObjRelationList::iterator cri = obj.Relations.begin(), cre = obj.Relations.end();
for (; cri != cre; ++cri) {
if (cri->obj->get_type() == obj.get_type() && cri->type == MeshObj::CHILD) 
  Par::Out() << " " << cri->obj->get_id() ;
}
Par::Out() << std::endl;
#endif

    return; // throw error ? maybe not.
  }

  MeshDB &mesh = GetMeshObjMesh(obj);

  /*--------------------------------------------------------------*/
  // First refine any edges.
  /*--------------------------------------------------------------*/
  const MeshObjTopo *topo = GetMeshObjTopo(obj);
  if (obj.get_type() == MeshObj::ELEMENT && topo->parametric_dim >= 2) {

    for (UInt e = 0; e < (UInt) topo->num_edges; e++) {

      MeshObjRelationList::iterator ei = MeshObjConn::find_relation(obj, MeshObj::EDGE, e, MeshObj::USES);

      // If no edge, then don't refine!
      if (ei != obj.Relations.end()) {
        RefineMeshObjLocal(*ei->obj, *rtopo.EdgeRTopo(e));
      } else {
        ThrowRequire(!mesh.AllSides());
      }
    } // edge
  } // if pdim >= 2

  /*--------------------------------------------------------------*/
  // And now refine faces
  /*--------------------------------------------------------------*/
  if (obj.get_type() == MeshObj::ELEMENT && topo->parametric_dim == 3) {
    for (UInt f = 0; f < topo->num_sides; f++) {
      MeshObjRelationList::iterator fi = MeshObjConn::find_relation(obj, MeshObj::FACE, f, MeshObj::USES);
      // If no face, then don't refine!
      if (fi != obj.Relations.end()) {
#ifdef REF_DEBUG
Par::Out() << "\t";
#endif
        RefineMeshObjLocal(*fi->obj, *rtopo.FaceRTopo(f));
      } else {
        ThrowRequire(!mesh.AllSides());
      }
    } // edge
  } // if pdim >= 2

  /*--------------------------------------------------------------*/
  // And now the object itself.
  // Start by searching the mesh to find all the nodes (including child) for the object.
  /*--------------------------------------------------------------*/
 
  std::vector<MeshObj*> nodes(topo->num_child_nodes, (MeshObj*)NULL);
  
#ifdef REF_DEBUG
Par::Out() << "\tretrieving nodes...";
#endif
  if (obj.get_type() == MeshObj::ELEMENT)
    retrieve_nodes(obj, topo, nodes);

#ifdef REF_DEBUG
if (obj.get_type() == MeshObj::ELEMENT) {
Par::Out() << std::endl;
  for (UInt i = 0; i < nodes.size(); i++) {
    if (nodes[i]) { 
      Par::Out() << "\t" << nodes[i]->get_id() << std::endl;
    } else {
      Par::Out() << "\tNULL" << std::endl;
    }
}
}
Par::Out() << std::endl;
#endif

  /*-------------------------------------------------------------------*/
  // Create nodes that don't exist and connect to parents.  Only for
  // elements, however.
  /*--------------------------------------------------------------*/
  if (obj.get_type() == MeshObj::ELEMENT) {
    for (UInt i = topo->num_nodes; i < (UInt) topo->num_child_nodes; i++) {
      if (nodes[i] == NULL) {  
        MeshObj *newnode = new MeshObj(MeshObj::NODE, mesh.get_new_local_id(MeshObj::NODE));
#ifdef REF_DEBUG
Par::Out() << "CRE NODE:" << newnode->get_id() << std::endl;
#endif
        Context newctxt; 
        newctxt.set(Attr::PENDING_CREATE_ID);
        newctxt.set(Attr::ACTIVE_ID);
        newctxt.clear(Attr::GENESIS_ID);
        Attr attr(MeshObj::NODE, newctxt);
        mesh.add_node(newnode, attr);
        nodes[i] = newnode;
  
      }
  
      // If an elememt, add nodes as children (duplicates are ignored)
      if (obj.get_type() == MeshObj::ELEMENT) {
        MeshObj::Relation r;
        r.obj = nodes[i];
        r.ordinal = i;
        r.type = MeshObj::CHILD;
        AddMeshObjRelation(obj, r);
        r.obj = &obj;
        r.ordinal = i;
        r.type = MeshObj::PARENT;
        AddMeshObjRelation(*nodes[i], r);
      }
  
    } // i nodes
  } // element

  /*--------------------------------------------------------------*/
  // We now have all the nodes we need to create the children.  Create
  // children and connect to parents.
  /*--------------------------------------------------------------*/
  std::vector<MeshObj*> children; children.reserve(rtopo.NumChild());
  {
    // We copy the object parents context, since it will contain the contexts necessary
    // to imprint the object
    Context newctxt(GetMeshObjContext(obj)); // imprints any usage sets
    newctxt.set(Attr::PENDING_CREATE_ID);
    newctxt.clear(Attr::GENESIS_ID);
    newctxt.set(Attr::ACTIVE_ID);
    Attr attr(GetAttr(obj), newctxt); 

    for (UInt c = 0; c < rtopo.NumChild(); c++) {
      const MeshObjTopo *ctopo = rtopo.ChildTopo(c);
      const UInt *child_node = rtopo.ChildNode(c);
      MeshObj *child = new MeshObj(obj.get_type(), mesh.get_new_local_id(obj.get_type()), -1, Par::Rank());
      children.push_back(child);
  
#ifdef REF_DEBUG
Par::Out() << "\t\tchild:" << child->get_id() << " attaching to " << obj.get_id() << std::endl;
#endif
      // Connect to parent
      MeshObj::Relation r;
      r.obj = child;
      r.ordinal = c;
      r.type = MeshObj::CHILD;
      AddMeshObjRelation(obj, r);
      r.obj = &obj;
      r.type = MeshObj::PARENT;
      AddMeshObjRelation(*child, r);

      // For an element, connect up the child nodes (faces/edges do not have child relations)
      if (obj.get_type() == MeshObj::ELEMENT) {
  
        // Set up relations
        // nodes:
#ifdef REF_DEBUG
Par::Out() << "\tattaching nodes:(ctopo=" << ctopo->name << ")";
#endif
        for (UInt n = 0; n < ctopo->num_nodes; n++) {
#ifdef REF_DEBUG
Par::Out() << nodes[child_node[n]]->get_id() << " ";
#endif
          MeshObj::Relation r;
          r.obj = nodes[child_node[n]];
          r.ordinal = n;
          r.type = MeshObj::USES;
          AddMeshObjRelation(*child, r);
        }
#ifdef REF_DEBUG
Par::Out() << std::endl;
#endif
        
      } // element
  
      // Add the child object to the mesh.
      {
        mesh.add_object(child, attr, ctopo);

        // There may be child nodes out there for the child element to gather:  This
        // could happen, for instance, if the current element is refining due to 2-1 propogation,
        // but the propogater already refined.
        if (obj.get_type() == MeshObj::ELEMENT) {
          std::vector<MeshObj*> childnodes(ctopo->num_child_nodes, static_cast<MeshObj*>(0));
          retrieve_nodes(*child, ctopo, childnodes);
          for (UInt i = ctopo->num_nodes; i < (UInt) ctopo->num_child_nodes; i++) {
            if (childnodes[i] != NULL) {
              MeshObj::Relation r;
              r.obj = childnodes[i];
              r.ordinal = i;
              r.type = MeshObj::CHILD;
              AddMeshObjRelation(*child, r);
              r.obj = child;
              r.type = MeshObj::PARENT;
              AddMeshObjRelation(*childnodes[i], r);
            }
          }
        }

        // Imprint the child
        child->GetKernel()->Imprint(*child);
      }
    } // for child
  } // create children
  
  /*------------------------------------------------------------------------*/
  // For elements, we must do several things:
  // 0) if use_sides, create faces/edges not present.
  // 1) Connect children of faces to the correct child elements
  // 2) Add nodes that have been created on edges/faces to any surrounding
  //    elements as chilren.
  // We do this first for faces, then for edges.
  /*------------------------------------------------------------------------*/
  if (obj.get_type() == MeshObj::ELEMENT) {

    if (topo->parametric_dim == 3) {
      // Loop all faces of the element
      for (UInt pf = 0; pf < topo->num_sides; pf++) {
        bool has_face = false;
        MeshObjRelationList::iterator pfi = MeshObjConn::find_relation(obj, MeshObj::FACE, pf, MeshObj::USES);
        has_face = (pfi != obj.Relations.end());

        // If use all sides, then all sides must be present.
        ThrowRequire(!mesh.AllSides() || has_face);

        // Parent face topo/ refinement topo
        const MeshObjTopo *pftopo = topo->side_topo(pf);
        const RefineTopo *pfrtopo = rtopo.FaceRTopo(pf);
        ThrowRequire(pftopo && pfrtopo);

        const int *side_nodes = topo->get_side_nodes(pf);
        const int *perm = has_face ? pftopo->perm_table(pfi->rotation, pfi->polarity) :
                       pftopo->perm_table(0, 1) /* standard */;

//Par::Out() << "face no:" << pf << ", id=" << pfi->obj->get_id() << std::endl;
        // Loop children of face
        for (UInt pfc = 0; pfc < pfrtopo->NumChild(); pfc++) {
          MeshObjRelationList::iterator pfci;
          if (has_face) {
            pfci = MeshObjConn::find_relation(*pfi->obj, MeshObj::FACE, pfc, MeshObj::CHILD);
            if (pfci == pfi->obj->Relations.end()) {
              std::cerr << "P:" << Par::Rank() << *pfi->obj;
              Throw() << "Expected face children!!";
            }
          }

          const MeshObjTopo *pfctopo = pfrtopo->ChildTopo(pfc); ThrowRequire(pfctopo);

          // Gather nodes of the child face
          std::vector<MeshObj*> pfcnodes;
          const UInt *child_nodes = pfrtopo->ChildNode(pfc);
          pfcnodes.reserve(pfctopo->num_nodes);

          for (UInt pfcn = 0; pfcn < pfctopo->num_nodes; pfcn++) {
            MeshObjRelationList::iterator pfcni = MeshObjConn::find_relation(obj,
                   MeshObj::NODE, side_nodes[perm[child_nodes[pfcn]]]);
            ThrowRequire(pfcni != obj.Relations.end());
            pfcnodes.push_back(pfcni->obj);
          }
  
          /*----------------------------------------------------------*/
          // Goal 1) Add face children as children faces
          // Okay.  Now have the nodes for face child.  We see if these match to any of the children
          if (has_face) {
            int ordinal, polarity, rotation;
            bool found_child = false;
            for (UInt c = 0; !found_child && c < rtopo.NumChild(); c++) {
              MeshObjConn::face_info(&pfcnodes[0], &pfcnodes[pfctopo->num_nodes], &children[c], &children[c+1],
                             &ordinal, &polarity, &rotation, false);
              if (ordinal >= 0) { // found it
                MeshObj::Relation r;
                r.obj = pfci->obj; // the subface
                r.ordinal = ordinal;
                r.polarity = polarity;
                r.rotation = rotation;
                r.type = MeshObj::USES;
                AddMeshObjRelation(*children[c], r); // add face to child
                r.obj = children[c];
                r.type = MeshObj::USED_BY;
                AddMeshObjRelation(*pfci->obj, r); // add child to face
                found_child = true;
              }
            } // children
            if (!found_child) Throw() << "Child face:" << pfc << " of face:" << pf << " did not find host child element!";
          } // has face
        } // pfc // children of face

        /*----------------------------------------------------------*/
        // Goal 2) connect up children to neighbors
        // Okay: Now for the parent edge check if we have nodes to provide. 
        std::vector<MeshObj*> pfnodes(pftopo->num_nodes);
        for (UInt i = 0; i < pftopo->num_nodes; i++) {
          pfnodes[i] = nodes[side_nodes[perm[i]]];
        }
        std::vector<MeshObj*> neighbors;
        MeshObjConn::common_objs(pfnodes.begin(), pfnodes.end(), MeshObj::USED_BY, MeshObj::ELEMENT, neighbors);
        if (neighbors.size() > 0) {
          std::vector<int> ordinals(neighbors.size());
          std::vector<int> polaritys(neighbors.size());
          std::vector<int> rotations(neighbors.size());
          MeshObjConn::face_info(pfnodes.begin(), pfnodes.end(), neighbors.begin(), neighbors.end(),
                          &ordinals[0], &polaritys[0], &rotations[0]);
          // loop neighbors, add nodes as necessary
          for (UInt nb = 0; nb < neighbors.size(); nb++) {
            if (neighbors[nb] == &obj) continue; // self
            MeshObj &nobj = *neighbors[nb];
#ifdef REF_DEBUG
Par::Out() << "should add to? " << nobj.get_id() << std::endl;
#endif
            const MeshObjTopo *ntopo = GetMeshObjTopo(nobj);
            const int *nside_nodes = ntopo->get_side_nodes(ordinals[nb]);
            const int *nperm = pftopo->perm_table(rotations[nb], polaritys[nb]);
            for (UInt i = pftopo->num_nodes; i < (UInt) pftopo->num_child_nodes; i++) {
              MeshObjRelationList::iterator nbcni = MeshObjConn::find_relation(nobj, MeshObj::NODE, nside_nodes[nperm[i]]);
              if (nbcni != nobj.Relations.end()) {
                // If it is there, it better be the same as what I have in that slot
                ThrowRequire(nbcni->obj == nodes[side_nodes[perm[i]]]);
              } else { // add it
                MeshObj::Relation r;
                r.obj = nodes[side_nodes[perm[i]]];
                r.ordinal = nside_nodes[nperm[i]];
                r.type = MeshObj::CHILD;
                AddMeshObjRelation(nobj, r); // add edge to child
                r.obj = &nobj;
                r.type = MeshObj::PARENT;
                AddMeshObjRelation(*nodes[side_nodes[perm[i]]], r); // add child to edge
              }
            } // child nodes
          } // neighbors
        } // if neighbors

      } // pf // faces (loop all faces)

      // Now, if AllSides, every child should have every face.  However, if the faces
      // were interior, they will not have been created, so they must be created now.
      if (mesh.AllSides())
        create_interior_faces(obj, rtopo);

    } // if topo dim == 3

    // Edges
    if (topo->parametric_dim >= 2) {
      // Loop all edges of the element
      for (UInt pf = 0; pf < (UInt) topo->num_edges; pf++) {
        bool has_edge = false;
        MeshObjRelationList::iterator pfi = MeshObjConn::find_relation(obj, MeshObj::EDGE, pf, MeshObj::USES);
        has_edge =  (pfi != obj.Relations.end());

        // If use all sides, then all sides must be present.
        ThrowRequire(!mesh.AllSides() || has_edge);

        // Loop children of edge
        const MeshObjTopo *pftopo = topo->edge_topo(pf);
        const RefineTopo *pfrtopo = rtopo.EdgeRTopo(pf);
        const int *edge_nodes = topo->get_edge_nodes(pf);
        const int *perm = has_edge ? pftopo->perm_table(pfi->rotation, pfi->polarity) : 
                      pftopo->perm_table(0,1);
        ThrowRequire(pftopo && pfrtopo);

        /*------------------------------------------------------------*/
        // Loop child edges
        for (UInt pfc = 0; pfc < pfrtopo->NumChild(); pfc++) {
          MeshObjRelationList::iterator pfci;
          if (has_edge) {
            pfci = MeshObjConn::find_relation(*pfi->obj, MeshObj::EDGE, pfc, MeshObj::CHILD);
            if (pfci == pfi->obj->Relations.end()) {
              std::cerr << "P:" << Par::Rank() << *pfi->obj;
              Throw() << "Expected edge children!!";
            }
          }

          const MeshObjTopo *pfctopo = pfrtopo->ChildTopo(pfc); ThrowRequire(pfctopo);
      
          // Gather nodes
          std::vector<MeshObj*> pfcnodes;
          const UInt *child_nodes = pfrtopo->ChildNode(pfc);
          pfcnodes.reserve(pfctopo->num_nodes);
          for (UInt pfcn = 0; pfcn < pfctopo->num_nodes; pfcn++) {
//Par::Out() << "edge:" << pf << ", node:" << pfcn << " at parent node:" << edge_nodes[child_nodes[pfcn]] << std::endl;
            MeshObjRelationList::iterator pfcni = MeshObjConn::find_relation(obj,
                   MeshObj::NODE, edge_nodes[perm[child_nodes[pfcn]]]);
//Par::Out() << " found: " << pfcni->obj->get_id() << std::endl;
            ThrowRequire(pfcni != obj.Relations.end());
            pfcnodes.push_back(pfcni->obj);
          }
  
          /*----------------------------------------------------------*/
          // Goal 1) Add edge children as children edges. 
          // Okay.  Now have the nodes for edge child.  We see if these match to any of the children
          if (has_edge) {
            int ordinal, polarity;
            bool found_child = false;
            for (UInt c = 0; !found_child && c < rtopo.NumChild(); c++) {
              MeshObjConn::edge_info(&pfcnodes[0], &pfcnodes[pfctopo->num_nodes], &children[c], &children[c+1],
                             &ordinal, &polarity, false);
              if (ordinal >= 0) { // found it
                MeshObj::Relation r;
                r.obj = pfci->obj; // the subface
                r.ordinal = ordinal;
                r.polarity = polarity;
                r.type = MeshObj::USES;
                AddMeshObjRelation(*children[c], r); // add edge to child
                r.obj = children[c];
                r.type = MeshObj::USED_BY;
                AddMeshObjRelation(*pfci->obj, r); // add child to edge
                found_child = true;
              }
            } // children
            if (!found_child) Throw() << "Child edge:" << pfc << " of edge:" << pf << " did not find host child element!";
            } // if has edge

        } // pfc // children of edge


        /*----------------------------------------------------------*/
        // Goal 2) connect up children to neighbors
        // Okay: Now for the parent edge check if we have nodes to provide. 
        std::vector<MeshObj*> pfnodes(pftopo->num_nodes);
        for (UInt i = 0; i < pftopo->num_nodes; i++) {
          pfnodes[i] = nodes[edge_nodes[perm[i]]];
        }
#ifdef REF_DEBUG
dPar::Out() << "n1:" << pfnodes[0]->get_id() << ", " << pfnodes[1]->get_id() << std::endl;
#endif
        std::vector<MeshObj*> neighbors;
        MeshObjConn::common_objs(pfnodes.begin(), pfnodes.end(), MeshObj::USED_BY, MeshObj::ELEMENT, neighbors);
        if (neighbors.size() > 0) {
          std::vector<int> ordinals(neighbors.size());
          std::vector<int> polaritys(neighbors.size());
          MeshObjConn::edge_info(pfnodes.begin(), pfnodes.end(), neighbors.begin(), neighbors.end(),
                          &ordinals[0], &polaritys[0]);
          // loop neighbors, add nodes as necessary
          for (UInt nb = 0; nb < neighbors.size(); nb++) {
            if (neighbors[nb] == &obj) continue; // self
            MeshObj &nobj = *neighbors[nb];
#ifdef REF_DEBUG
Par::Out() << "edge:should add to? " << nobj.get_id() << " ";
#endif
            const MeshObjTopo *ntopo = GetMeshObjTopo(nobj);
            const int *nedge_nodes = ntopo->get_edge_nodes(ordinals[nb]);
            const int *nperm = pftopo->perm_table(0, polaritys[nb]);
            for (UInt i = pftopo->num_nodes; i < (UInt) pftopo->num_child_nodes; i++) {
              MeshObjRelationList::iterator nbcni = MeshObjConn::find_relation(nobj, MeshObj::NODE, nedge_nodes[nperm[i]], MeshObj::CHILD);
              if (nbcni != nobj.Relations.end()) {
                // If it is there, it better be the same as what I have in that slot
                if (nbcni->obj != nodes[edge_nodes[perm[i]]]) {
                  std::cerr << "Expected node:" << *nbcni->obj << " of element:" << nobj 
                      << ", edge:ord=" << ordinals[nb] << ", pol=" << polaritys[nb] <<
                      " to be:" << *nodes[edge_nodes[perm[i]]] << " ord=" << edge_nodes[perm[i]] <<
                      " of elem:" << obj;
                  Throw() << "Node not what expected:";
                }
              } else { // add it
#ifdef REF_DEBUG
Par::Out() << " yes, " << nodes[edge_nodes[perm[i]]]->get_id() << " ";
#endif
                MeshObj::Relation r;
                r.obj = nodes[edge_nodes[perm[i]]];
                r.ordinal = nedge_nodes[nperm[i]];
                r.type = MeshObj::CHILD;
                AddMeshObjRelation(nobj, r); // add edge to child
                r.obj = &nobj;
                r.type = MeshObj::PARENT;
                AddMeshObjRelation(*nodes[edge_nodes[perm[i]]], r); // add child to edge
              }
            } // child nodes
#ifdef REF_DEBUG
Par::Out() << std::endl;
#endif
          } // neighbors
        } // if neighbors

      } // pf // edge
      // Now, if AllSides, every child should have every edge.  However, if the edge
      // were interior, they will not have been created, so they must be created now.
      if (mesh.AllSides())
        create_interior_edges(obj, rtopo);
    } // pdim >= 2
  } // element

  // Imprint nodes with exterior boundary, if they are on a face that is exterior
  if (obj.get_type() == MeshObj::ELEMENT) ImprintExterior(obj);

  // Mark object refined, and remove from active roster.
  const Attr &oattr = GetAttr(obj);
  const Context &ctxt = GetMeshObjContext(obj);
  Context newctxt(ctxt);
  newctxt.clear(Attr::ACTIVE_ID); // TODO: maybe this should still be active?? Other side might use it.
  newctxt.set(Attr::REFINED_ID);
  ThrowRequire(newctxt != ctxt); // can't be refining an inactive refined obj!
  Attr attr(oattr, newctxt);
  mesh.update_obj(&obj, attr);
  
  
}

/*--------------------------------------------------------------*/
// Prolong the node coordinates.
/*--------------------------------------------------------------*/
void ProlongNodeCoords(std::vector<MeshObj*> &elems, MEField<> &coord) {

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

bool can_unrefine_investigate(MeshObj &obj) {
  // For an element, we can always unrefine:
  if (obj.get_type() == MeshObj::ELEMENT) return true;

  // For a face/edge, we must make sure that someone
  // across from us is still not using one of our children.
  MeshObjRelationList::iterator cri = obj.Relations.begin(), cre = obj.Relations.end();

  for (; cri != cre; ++cri) {
    if (cri->obj->get_type() != obj.get_type() || cri->type != MeshObj::CHILD) continue; 

    if (MeshObjConn::obj_used(*cri->obj)) return false;

  } // children

  return true;
}

void save_face_edge_lists(const std::vector<MeshObj*> &children, std::vector<MeshObj*> &face_list, std::vector<MeshObj*> &edge_list) {
  for (UInt c = 0; c < children.size(); c++) {
    MeshObj &cobj = *children[c];
    MeshObjRelationList::iterator cri = cobj.Relations.begin(), cre = cobj.Relations.end();
    for (; cri != cre; ++cri) {
      if (cri->type == MeshObj::USES) {
        if (cri->obj->get_type() == MeshObj::FACE) {
          std::vector<MeshObj*>::iterator lb = std::lower_bound(face_list.begin(), face_list.end(), cri->obj);
          if (lb == face_list.end() || *lb != cri->obj)
            face_list.insert(lb, cri->obj);
        } else if (cri->obj->get_type() == MeshObj::EDGE) {
          std::vector<MeshObj*>::iterator lb = std::lower_bound(edge_list.begin(), edge_list.end(), cri->obj);
          if (lb == edge_list.end() || *lb != cri->obj)
            edge_list.insert(lb, cri->obj);
        }
      }
    }
  }
}

void prune_face_edge_lists(MeshDB &mesh, std::vector<MeshObj*> &face_list, std::vector<MeshObj*> &edge_list) {
  for (UInt f = 0; f < face_list.size(); f++) {
    MeshObj &face = *face_list[f];
    if (!MeshObjConn::obj_used(face)) {
      const Attr &oattr = GetAttr(face);
      const Context &ctxt = GetMeshObjContext(face);
      Context newctxt(ctxt);
      newctxt.set(Attr::PENDING_DELETE_ID);
      if (newctxt != ctxt) {
        Attr attr(oattr, newctxt);
        mesh.update_obj(&face, attr);
      }
    }
  }
  for (UInt e = 0; e < edge_list.size(); e++) {
    MeshObj &edge = *edge_list[e];
    if (!MeshObjConn::obj_used(edge)) {
      const Attr &oattr = GetAttr(edge);
      const Context &ctxt = GetMeshObjContext(edge);
      Context newctxt(ctxt);
      newctxt.set(Attr::PENDING_DELETE_ID);
      if (newctxt != ctxt) {
        Attr attr(oattr, newctxt);
        mesh.update_obj(&edge, attr);
      }
    }
  }
}

/*---------------------------------------------------------------*/
// Unrefine a mesh object.  Unrefine the children of an element first,
// then unrefine faces, edges.
/*---------------------------------------------------------------*/
void UnrefineMeshObjLocal(MeshObj &obj) {

  // Ignore if object not refined.
  if (!GetMeshObjContext(obj).is_set(Attr::REFINED_ID)) return;

//Par::Out() << "Unrefining " << MeshObjTypeString(obj.get_type()) << ", " << obj.get_id() << std::endl;
  const MeshObjTopo *topo = GetMeshObjTopo(obj);
  MeshDB &mesh = GetMeshObjMesh(obj);

  // Remove Children nodes and of this same type.  If someone still USES the object, then it
  // can not be removed (think faces/edges with refined elements across.
  // Now, since we only unrefine element, face, edge, and the only possible
  // USES of these are by elements, we just check if there is an element that
  // uses the object, in which case we cant remove it.  This works out since
  // elements are unrefined before their faces, hence removing the children USES
  // of the faces/edges.

  // Loop children.  Should have children 0-n, so find first, they will
  // all be just after.

  /*------------------------------------------------------------------------------------*/
  // first find out if we CAN unrefine.  The criterion is that all children (of this type)
  // must be unused.
  /*------------------------------------------------------------------------------------*/
  bool can_unrefine = can_unrefine_investigate(obj);

//Par::Out() << " can unrefine=" << can_unrefine << std::endl;
  if (!can_unrefine) return;


  /*------------------------------------------------------------------------------------*/
  // We ARE refining, so cut children off from mesh and put them on pending delete.
  /*------------------------------------------------------------------------------------*/
  {
    MeshObjRelationList::iterator cri = obj.Relations.begin(), cre = obj.Relations.end();
    std::vector<MeshObj*> children;
    for (; cri != cre; ++cri) {
      if (cri->obj->get_type() != obj.get_type() || cri->type != MeshObj::CHILD)
                continue; 
      children.push_back(cri->obj);
     }

    /*------------------------------------------------------------------------------------*/
    //  If we are the element, save a list of all faces and edges.  In this way we
    //  detect interior edges/faces that are not children of anyone, but that the children
    //  will abandon.
    /*------------------------------------------------------------------------------------*/
    std::vector<MeshObj*> face_list;
    std::vector<MeshObj*> edge_list;
    if (obj.get_type() == MeshObj::ELEMENT) {
      save_face_edge_lists(children, face_list, edge_list);
    }

    // use list, since this operation can invalidate the iterators above
    for (UInt c = 0; c < children.size(); c++) {

      // And now push on pendingdelete
      const Attr &oattr = GetAttr(*children[c]);
      const Context &ctxt = GetMeshObjContext(*children[c]);
      Context newctxt(ctxt);
      newctxt.set(Attr::PENDING_DELETE_ID);
      if (newctxt != ctxt) {
        Attr attr(oattr, newctxt);
        mesh.update_obj(children[c], attr);
      }
    } // for c

    // Zap any interior face/edges
    prune_face_edge_lists(mesh, face_list, edge_list);

  }

  /*------------------------------------------------------------------------------------*/
  // IF we are an element, now we can investigate the nodes, since the children
  // don't point to them any more.  For any nodes that aren't used, remove them.
  /*------------------------------------------------------------------------------------*/
  if (obj.get_type() == MeshObj::ELEMENT) {
    MeshObjRelationList::iterator cri = obj.Relations.begin(), cre = obj.Relations.end();
    std::vector<MeshObj*> children;
    for (; cri != cre; ++cri) {
      if (cri->obj->get_type() != MeshObj::NODE || cri->type != MeshObj::CHILD)
                continue; 
      if (!MeshObjConn::obj_used(*cri->obj))
        children.push_back(cri->obj);
    }

    // use list, since this operation can invalidate the iterators above
    for (UInt c = 0; c < children.size(); c++) {

//Par::Out() << " remove child:" << MeshObjTypeString(children[c]->get_type()) << ", " << children[c]->get_id() << std::endl;

      // And now push on pendingdelete
      const Attr &oattr = GetAttr(*children[c]);
      const Context &ctxt = GetMeshObjContext(*children[c]);
      Context newctxt(ctxt);
      newctxt.set(Attr::PENDING_DELETE_ID);
      if (newctxt != ctxt) {
        Attr attr(oattr, newctxt);
        mesh.update_obj(children[c], attr);
      }
    } // for c
  }

  // Now, if element, unrefine all faces, edges
  if (obj.get_type() == MeshObj::ELEMENT) {

    // faces
    if (topo->parametric_dim == 3) {
      for (UInt f = 0; f < topo->num_sides; f++) {
        MeshObjRelationList::iterator fi = MeshObjConn::find_relation(obj, MeshObj::FACE, f, MeshObj::USES);
        // If no face, then don't refine!
        if (fi != obj.Relations.end()) {
 // Par::Out() << "\t";
          UnrefineMeshObjLocal(*fi->obj);
        }
      } // edge
    } // if pdim >= 2
  
    if (topo->parametric_dim >= 2) {
      for (UInt e = 0; e < (UInt) topo->num_edges; e++) {
        MeshObjRelationList::iterator ei = MeshObjConn::find_relation(obj, MeshObj::EDGE, e, MeshObj::USES);
        // If no edge, then don't refine!
        if (ei != obj.Relations.end()) {
//  Par::Out() << "\t";
          UnrefineMeshObjLocal(*ei->obj);
        }
      } // edge
    } // if pdim >= 2
  } // if element


  // Put object back on the active roster
if (obj.get_id() == 738 && obj.get_type() == MeshObj::EDGE) {
Par::Out() << "Unrefine edge 738!!" << std::endl;
}
  const Attr &oattr = GetAttr(obj);
  const Context &ctxt = GetMeshObjContext(obj);
  Context newctxt(ctxt);
  newctxt.set(Attr::ACTIVE_ID); 
  newctxt.clear(Attr::REFINED_ID);
  ThrowRequire(newctxt != ctxt); 
  Attr attr(oattr, newctxt);
  mesh.update_obj(&obj, attr);
} 

} // namespace
} // namespace
