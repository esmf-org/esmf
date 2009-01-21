// $Id: ESMC_Mesh.C,v 1.5.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_Mesh.h>
#include <ESMC_MeshField.h>

#include <ESMC_MeshObjConn.h>
#include <ESMC_MeshObjPack.h>
#include <ESMC_MeshSkin.h>
#include <ESMC_SparseMsg.h>
#include <ESMC_ParEnv.h>
#include <ESMC_GlobalIds.h>
#include <bitset>

//#define CRE_DEBUG

namespace ESMCI {
namespace MESH {

// *** Mesh implementation ***

Mesh::Mesh() : MeshDB(), FieldReg(), CommReg(),
sghost(NULL),
committed(false)
{

   GetCommRel(MeshObj::NODE).Init("node_sym", *this, *this, true);
   GetCommRel(MeshObj::EDGE).Init("edge_sym", *this, *this, true);
   GetCommRel(MeshObj::FACE).Init("face_sym", *this, *this, true);
   GetCommRel(MeshObj::ELEMENT).Init("elem_sym", *this, *this, true);
}

Mesh::~Mesh() {
  if (sghost)
    RemoveGhost();
}

void Mesh::assign_new_ids() {
  Trace __trace("assign_new_ids()");
  for (UInt tp = 0; tp < NumMeshObjTypes; tp++) {
    UInt obj_type = MeshObjTypes[tp];
    std::vector<MeshObj::id_type> used_ids;
    std::vector<MeshObj::id_type> needed_ids;
    UInt needed_id_count = 0;

    KernelList::iterator ki = set_begin(), ke = set_end();
    for (; ki != ke; ++ki) {
      Kernel &ker = *ki;
      // select all objs of this type and not the newly created ones (which have bogus ids)
      if (ker.type() == obj_type) {
        if (!ker.GetContext().is_set(Attr::PENDING_CREATE_ID)) {
          Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
          for (; oi !=oe; ++oi) {
            used_ids.push_back(oi->get_id());
          }
        } else {
          needed_id_count += ker.NumObjects();
        }
      } // correct type
    } // kernels

    // We now have all the ids for this object type
    needed_ids.resize(needed_id_count);
    GlobalIds(used_ids, needed_ids);

    // Lets update the objects with the new ids.
    UInt i = 0;
    ki = set_begin();
    for (; ki != ke; ++ki) {
      Kernel &ker = *ki;
      // select all objs of this type and not the newly created ones (which have bogus ids)
      if (ker.type() == obj_type) {
        if (ker.GetContext().is_set(Attr::PENDING_CREATE_ID)) {
          Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
          for (; oi !=oe; ++oi) {
            MeshObj::id_type newid = needed_ids[i++];
            // Take object from map
            MeshObj &obj = *oi;
//Par::Out() << MeshObjTypeString(obj_type) << " " << obj.get_id() << " -> " << newid << std::endl;
            MeshObjIDMap &omap = get_map(obj_type);
            omap.erase(&obj);
            obj.key = newid;
            std::pair<MeshObjIDMap::iterator,bool> iu =
              omap.insert(obj);
            if (!iu.second)
              Throw() << "Error inserting. Key already exists!!! Obj:" << obj << std::endl;

            // clear pendinig bit wit base class, below
            
          } // for oi
        } // Pending create
      } // object kernel type
    } // kernels
  } // otype
}

void build_shared_comm(Mesh &mesh, UInt obj_type, std::vector<CommRel::CommNode> &ecom, UInt ctxt_id) {
  ecom.clear();
  // get the shared objs
  KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
  for (; ki != ke; ++ki) {
    Kernel &ker = *ki;
    // select all objs of this type and not the newly created ones (which have bogus ids)
    if (ker.type() == obj_type) {
      if (ker.GetContext().is_set(ctxt_id)) {
        Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
        for (; oi !=oe; ++oi) {
          std::vector<UInt> procs;
          // Use SymNodeRel for sharing of ALL types!
          MeshObjConn::get_shared_procs(*oi, mesh.GetSymNodeRel(), procs);
           
          // If the object is shared, but no procs come back, make a last try by
          // consulting the shared comm
          if (procs.size() == 0 && GetMeshObjContext(*oi).is_set(Attr::SHARED_ID)) {
            CommRel &crel = mesh.GetCommRel(obj_type);
            CommRel::MapType::iterator lb = 
              std::lower_bound(crel.domain_begin(), crel.domain_end(), CommRel::CommNode(&*oi, 0));

            while(lb != crel.domain_end() && lb->obj == &*oi) {
              procs.push_back(lb->processor);
              lb++;
            }
          }

          for (UInt i = 0; i < procs.size(); i++) {
            ecom.push_back(CommRel::CommNode(&*oi, procs[i]));
          }
        }
      } // pending create
    } // correct type
  } // kernels

  // Sort by id, proc
  std::sort(ecom.begin(), ecom.end());
}

void set_owner_to_self(Mesh &mesh) {
  KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
  for (; ki != ke; ++ki) {
    Kernel &ker = *ki;
    // select all objs of this type and not the newly created ones (which have bogus ids)
    if (ker.GetContext().is_set(Attr::PENDING_CREATE_ID)) {
      Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end(), on;
      for (; oi !=oe;) {
        on = oi; ++on;
        oi->set_owner(Par::Rank());

        // Update to locally owned, if needed
        const Context &ctxt = GetMeshObjContext(*oi);
        Context newctxt(ctxt);
        newctxt.set(Attr::OWNED_ID);
        if (newctxt != ctxt) {
          Attr attr(GetAttr(*oi), newctxt);
          mesh.update_obj(&*oi, attr);
        }

        oi = on;

      }
    } // pending create
  } // kernels
}

/*----------------------------------------------------------------*/
// Resolve mesh object creation across parallel interfaces 
/*----------------------------------------------------------------*/
void Mesh::ResolvePendingCreate() {
  TraceBack __trace("Mesh::ResolvePendingCreate()");
//Par::Out() << "PendingCreate resolution:" << std::endl;

  UInt csize = Par::Size();
  // Step 1: Get a global id for each new object.  Global ids may be redundant here,
  // since an object might be shared among several procs.  We resolve this later.

  assign_new_ids();


  // Set owner to be this processor (may or may not stand)
  set_owner_to_self(*this);

   // ************************ Step 2: object resolution ********************************
  // Step 2:  Now we find out if the objects are shared.  Amongst other things, assign an
  // owning processor.  If the object is shared, add to the sym comm spec and and resolve the global
  // id to be that of the owner.
  for (UInt tp = 0; csize > 1 && tp < NumMeshObjTypes; tp++) 
  {
    
    UInt obj_type = MeshObjTypes[tp];

    // For edges, we process the full and then child cases separately
    // to avoid problems with the interior edge situation.
    UInt subcases = obj_type == MeshObj::EDGE ? 2 : 1;
  for (UInt scase = 0; scase < subcases; scase++) {

#ifdef CRE_DEBUG
Par::Out() << "Resolve object type:" << MeshObjTypeString(obj_type) << std::endl;
#endif

    // This comm is used for the intial query of object pairing
    CommRel orel("object_resolution", *this, false);

    // Those objects that find mates on other processors go into this comm, which,
    // after symetrization, are added to symetric comm.
    CommRel orel1("final_object_resolution", *this, false);

    {
      std::vector<CommRel::CommNode> ecnodes;

      /*---------------------------------------*/
      // Get the shared objects and the procs they
      // are shared with.
      build_shared_comm(*this, obj_type, ecnodes, Attr::PENDING_CREATE_ID);
  
      // Add these objects to the comm rel.
      orel.add_domain(ecnodes);
  
    }

    // At this point, orel is the commspec for all shared objects of this type.

    // We will create the resolution spec objs
    std::vector<CommRel::CommNode> final_sym_objs;

    /*--------------------------------------------------------*/
    // Broadcast the object to all sharing processors 
    {
      SparseMsg msg;
 
      CommRel::MapType::iterator di = orel.domain_begin(), de = orel.domain_end();
      std::vector<UInt> to_proc;
      std::vector<UInt> send_sizes_all(csize, 0);

      const MeshObjTopo *etopo = NULL;

      // ** Sizing Loop **
      for (; di != de; ++di) {
        MeshObj *obj = di->obj;
        UInt proc = di->processor;


        if (obj_type != MeshObj::NODE) {

          std::vector<MeshObj*> parents;

          // If the object is a child, send the node support of its parent rather
          // than self. Else send node support of self
          MeshObjConn::common_objs(&obj, &obj + 1, MeshObj::PARENT, obj_type, parents);

          if (parents.size() > 0) {
   
            // Don't send the child edges out until we have processed parents.
            if (obj_type == MeshObj::EDGE && scase == 0) continue;  
            etopo = GetMeshObjTopo(*parents[0]);
          }
          else {
            // Already sent these out in step 0, so no need now.
            if (obj_type == MeshObj::EDGE && scase == 1) continue;  
            etopo = GetMeshObjTopo(*obj);
          }

          ThrowRequire(etopo);
        } else {
          std::vector<MeshObj*> nodes; UInt ord;
          MeshObjConn::get_node_support(*obj, etopo, ord, nodes);
        }

        std::vector<UInt>::iterator lb = std::lower_bound(to_proc.begin(), to_proc.end(), proc);
        if (lb == to_proc.end() || *lb != proc)
          to_proc.insert(lb, proc);

        // Send id
        send_sizes_all[proc] += SparsePack<MeshObj::id_type>::size();

        // Send attr
        send_sizes_all[proc] += SparsePack<Attr>::size();

        // Send topo number
        send_sizes_all[proc] += SparsePack<MeshObjTopo::global_identifier>::size();

        // Send the ordinal in parent topo, or < 0 if not a child
        send_sizes_all[proc] += SparsePack<int>::size();

        for (UInt n = 0; n < etopo->num_nodes; n++) {
          // Send id of node
          send_sizes_all[proc] += SparsePack<MeshObj::id_type>::size();
        }
      } // for di
 
      // Set the communication patter, collect sizes
      UInt nsend = to_proc.size();
      msg.setPattern(nsend, nsend == 0 ? NULL : &to_proc[0]);

      std::vector<UInt> to_sizes(nsend,0);
      for (UInt i = 0; i < nsend; i++) 
        to_sizes[i] = send_sizes_all[to_proc[i]];

      msg.setSizes(nsend == 0 ? NULL : &to_sizes[0]);

      // ** Now pack message **
      for (di = orel.domain_begin(); di != de; ++di) {
        MeshObj *obj = di->obj;
//Par::Out() << "pack, Hit obj:" << obj->get_id() << std::endl;
        // Smallest processor is first, so we own the object if we are < first
//Par::Out() << "pack I own the object, sending out:";
        UInt proc = di->processor;
//Par::Out() << "pck proc " << proc;
        SparseMsg::buffer &b = *msg.getSendBuffer(proc);


        // Now get nodes
        int ord;
        std::vector<MeshObj*> enodes;
        if (obj_type != MeshObj::NODE) {

          std::vector<MeshObj*> parents;

          // If the object is a child, send the node support of its parent rather
          // than self. Else send node support of self
          MeshObjConn::common_objs(&obj, &obj + 1, MeshObj::PARENT, obj_type, parents);

          if (parents.size() > 0) {

            if (obj_type == MeshObj::EDGE && scase == 0) continue;  

            MeshObjConn::get_obj_nodes(*parents[0], enodes);

            ord = MeshObjConn::get_ordinal(*obj, *parents[0]);
            ThrowRequire(ord >= 0);
          }
          else {
            if (obj_type == MeshObj::EDGE && scase == 1) continue;  
            MeshObjConn::get_obj_nodes(*obj, enodes);
            ord = -1;
          }

          ThrowRequire(etopo->num_nodes == enodes.size());

        } else {
          UInt tord;
          MeshObjConn::get_node_support(*obj, etopo, tord, enodes);

          ord = tord;
        }

        // Send id
        SparsePack<MeshObj::id_type>(b, obj->get_id());

        // Send Attr
        SparsePack<Attr>(b, GetAttr(*obj));

        // Send topo number
        SparsePack<MeshObjTopo::global_identifier>(b, etopo->number);

        // Pack ordinal
        SparsePack<int>(b, ord);

        for (UInt n = 0; n < etopo->num_nodes; n++) {
          // Send id of supporting nodes
          SparsePack<MeshObj::id_type>(b, enodes[n]->get_id());
        }
      } // pack objs

      if (!msg.filled())
        Throw() << "Message not filled in object resoltuon, obj type:" << MeshObjTypeString(obj_type);

      // ** Send Message **
      msg.communicate();

      // ** Unpack **
     for (UInt *p = msg.inProc_begin(); p != msg.inProc_end(); ++p) {
       UInt proc = *p;
       SparseMsg::buffer &b = *msg.getRecvBuffer(proc);
  
       // Unpack everything from this processor
       while (!b.empty()) {

         // Unpack id
         MeshObj::id_type obj_id;
         SparseUnpack<MeshObj::id_type>(b, obj_id);

         // Unpack Attr
         Attr a;
         SparseUnpack<Attr>(b, a);

         // Unpack topo id
         MeshObjTopo::global_identifier tid;
         SparseUnpack<MeshObjTopo::global_identifier>(b, tid);

         const MeshObjTopo *etopo = GetTopo(tid);
         ThrowRequire(etopo);

         int obj_ord;
         SparseUnpack<int>(b, obj_ord);

#ifdef CRE_DEBUG
 Par::Out() << "Got id:" << obj_id << " from proc:" << proc << ", ord=" << obj_ord << std::endl;
#endif

//  Par::Out() << ", topo=" << etopo->name << std::endl;
         std::vector<MeshObj::id_type> nids(etopo->num_nodes,0);
         std::vector<MeshObj*> ends(etopo->num_nodes);

         for (UInt n = 0; n < etopo->num_nodes; n++) {

           SparseUnpack<MeshObj::id_type>(b, nids[n]);
           Mesh::MeshObjIDMap::iterator mi = get_map(MeshObj::NODE).find(nids[n]);

           if (mi == get_map(MeshObj::NODE).end())
             Throw() << "Did not find node " << nids[n] << ", though it is shared";
           ends[n] = &*mi;
         }
#ifdef CRE_DEBUG
 Par::Out() << "Sent nodes=";
for (UInt i = 0; i < ends.size(); i++) Par::Out() << "nid:" << ends[i]->get_id() << " ";
Par::Out() << std::endl;
#endif
         // Now search for the object.  Start by getting the elems that use these nodes.
         std::vector<MeshObj*> elems;
         MeshObjConn::common_objs(ends.begin(), ends.end(), MeshObj::USED_BY, MeshObj::ELEMENT, elems);

         bool interior_edge = false;
         if (elems.size() == 0) {
           // *** This definitely could happen, if the face or edge is on an 
           // exterior, but the neighbor processor envelops the boundary of the
           // element.
#ifdef CRE_DEBUG
Par::Out() << MeshObjTypeString(obj_type) << " " << obj_id << " not found on other proc!" << std::endl;
 Par::Out() << "Sent nodes=";
for (UInt i = 0; i < ends.size(); i++) Par::Out() << "nid:" << ends[i]->get_id() << " ";
Par::Out() << std::endl;
#endif

           // There is one other strange case where this can happen: the interior
           // edges on a 3d face have no parents, so they may come across with the
           // nodes they span, which might be child nodes of elements not on this
           // processor.  In that case, we will not find an element.  Hence, such
           // edges are not shared until the element across refines.
           if (obj_type == MeshObj::EDGE) {
             MeshObjConn::common_objs(ends.begin(), ends.end(),
                   MeshObj::USED_BY | MeshObj::PARENT, MeshObj::ELEMENT, elems);
#ifdef CRE_DEBUG
if (elems.size() > 0)
  Par::Out() << "Turns out this must be an interior edge, elems > 0" << std::endl;
#endif

             if (elems.size() > 0) interior_edge = true;
           }

           if (!interior_edge) continue;
         }

         ThrowRequire(elems.size() > 0);
         // Get info about object (using the orientation implied by this ordering);
         std::vector<int> ordinals; ordinals.resize(elems.size());
         std::vector<int> polaritys; polaritys.resize(elems.size());
         std::vector<int> rotations; rotations.resize(elems.size());
    
         // Our first bifurcation:  If edges, edge_info, else face_info; nodes may be either
         if (obj_type == MeshObj::NODE) {
           // Must determine the type of topology that was sent:

           if (etopo->parametric_dim == 1) {

             MeshObjConn::edge_info(ends.begin(), ends.end(), elems.begin(), elems.end(), &ordinals[0], &polaritys[0]);

           } else {

             ThrowRequire(etopo->parametric_dim == 2 && spatial_dim() == 3);
             MeshObjConn::face_info(ends.begin(), ends.end(), elems.begin(), elems.end(), &ordinals[0], &polaritys[0], &rotations[0]);

           }

         } else if (!interior_edge && obj_type == MeshObj::EDGE) {

           MeshObjConn::edge_info(ends.begin(), ends.end(), elems.begin(), elems.end(), &ordinals[0], &polaritys[0]);

         } else if (obj_type == MeshObj::FACE) {

           MeshObjConn::face_info(ends.begin(), ends.end(), elems.begin(), elems.end(), &ordinals[0], &polaritys[0], &rotations[0]);

         }

         MeshObj *eobj = NULL;

         // Now try to find a matching object.  Nodes/edges/faces differ
         // If the object is not found, since the object are synchronized, this is the first time
         // any processor has created the object.
         //
         // To assign an owner, we set the owner to be the lowest proc that has created the object
         // locally.  This will guarantee that a halo will update, for instance, node coords, etc with
         // valid data (not data from a proc that is only ghosting the object).
         //
         // So we check; if proc < owner, let proc take over ownership of the object, and use proc's id.

         if (obj_type == MeshObj::NODE) {
           // Use entry 0 to search for object
           MeshObj &elem = *elems[0];

      
           const MeshObjTopo *elem_topo = GetMeshObjTopo(elem);
 
           const int *side_nodes = (etopo->parametric_dim == 1 ?
                         elem_topo->get_edge_nodes(ordinals[0]) : elem_topo->get_side_nodes(ordinals[0]));

           MeshObjRelationList::iterator geri = MeshObjConn::find_relation(elem, MeshObj::NODE, side_nodes[obj_ord], MeshObj::CHILD);

           // Create the node if not there
           if (geri == elem.Relations.end()) {
#ifdef CRE_DEBUG
Par::Out() << "Did not find node on other side, creating" << std::endl; // TODO: take out of sym spec
#endif

             MeshObj *newnode = new MeshObj(MeshObj::NODE, get_new_local_id(MeshObj::NODE));

             Context ctxt(a.get_context());
             //ctxt.clear(Attr::ACTIVE_ID); // make child nodes without USES inactive

             // This function connects the node to the appropriate local element(s)
             add_node_local(newnode, etopo, obj_ord, ends, Attr(a, ctxt));

             // set owner to ridiculously large number so that it is assigned to proc, below.
             newnode->set_owner(std::numeric_limits<UInt>::max());

             eobj = newnode;
           } else eobj = geri->obj; // else assign to object found

//Par::Out() << "Found node on other side, id=" << eobj->get_id() << std::endl;

         } else if (!interior_edge) { // not NODE

           // Finally; we've heard all about the object, now finally get a handle to it:
           MeshObjRelationList::iterator geri = MeshObjConn::find_relation(**elems.begin(), obj_type, ordinals[0]);


           // Two cases now:  1) We are bartering with a parent object, in which case we must look for or
           // create the child, 2) We are transacting with an actual object, and we need to look for that
           // object.
           if (obj_ord < 0) {
             if (geri == (*elems.begin())->Relations.end()) {
  
#ifdef CRE_DEBUG
  Par::Out() << "Did not find " << MeshObjTypeString(obj_type) << " on other side, creating" << std::endl; // TODO: take out of sym spec
#endif
  
               MeshObj *newside = new MeshObj(obj_type, get_new_local_id(obj_type));

               Context ctxt(a.get_context());
   
               // Add object to mesh; depends on object type.
               if (obj_type == MeshObj::EDGE) {
   
                  add_edge_local(*newside, *elems[0], ordinals[0], a.get_key(), etopo, ctxt);
  
                } else if (obj_type == MeshObj::FACE) {
  
                  add_side_local(*newside, *elems[0], ordinals[0], a.get_key(), etopo, ctxt);
  
                } else Throw() << "Unknown obj type in side create resolution";
  
               // set owner to ridiculously large number so that it is assigned to proc, below.
               newside->set_owner(std::numeric_limits<UInt>::max());
  
             } else {

                eobj = geri->obj; // assign to found object

                // We have found the local object.  There is one strange situation:  For 3d interior
                // edges, it is possible that the edge was resident from a previous refinement on this
                // processor.  If so, it will not have been shared to the other processor, and
                // that processor will be the only one requesting a create this round.  It will assume
                // that it is the lowest processor and I will retain ownership, causing a global inconsistency.
                // We break this by letting the other proc own the object.
                if (obj_type == MeshObj::EDGE && !GetAttr(*eobj).get_context().is_set(Attr::PENDING_CREATE_ID)) {
#ifdef CRE_DEBUG
Par::Out() << "local obj " << eobj->get_id() << " not pending create, relinquishing ownership" << std::endl;
#endif
                  eobj->set_owner(std::numeric_limits<UInt>::max());

                  // MUST remove object from local symmetric comm spec, since it will get a new number and
                  // end up in a different place.
                  CommRel &edge_rel = GetCommRel(obj_type);
                  std::vector<MeshObj*> cobj; cobj.push_back(eobj);
                  edge_rel.remove_domain(cobj);
                }
             }

           } else { // obj_ord >= 0, geri points to parent
          
             // If geri = relations.end, something has gone wrong.  We MUST have the parent on this
             // proc!!
             ThrowRequire(geri != elems[0]->Relations.end());

             // See if a child of my ordinal exists
             MeshObjRelationList::iterator cri = MeshObjConn::find_relation(*geri->obj, obj_type, obj_ord, MeshObj::CHILD);

             // if object is there, we are done
             if (cri != geri->obj->Relations.end()) {
               eobj = cri->obj;
             } else {
               // We must create the object.  Also, set the refined bit and take off active for parent

               MeshObj &par = *geri->obj;

               // Create object.  Only relations are to parent:
               MeshObj *newside = new MeshObj(obj_type, get_new_local_id(obj_type));

               MeshObj::Relation r;
               r.obj = newside;
               r.ordinal = obj_ord;
               r.type = MeshObj::CHILD;
               AddMeshObjRelation(par, r);
               r.obj = &par;
               r.type = MeshObj::PARENT;
               AddMeshObjRelation(*newside, r);
               add_object(newside, a, etopo);

               eobj = newside;

               // set owner to ridiculously large number so that it is assigned to proc, below.
               newside->set_owner(std::numeric_limits<UInt>::max());

               // Update parent
               const Attr &oattr = GetAttr(par);
               const Context &ctxt = GetMeshObjContext(par);
               Context newctxt(ctxt);
               newctxt.clear(Attr::ACTIVE_ID);
               newctxt.set(Attr::REFINED_ID);
               if (newctxt != ctxt) { // First child may set this.
                 Attr attr(oattr, newctxt);
                 update_obj(&par, attr);
               }
             }
           }


/* Verify that the edge found DOES match the nodes given.
 Par::Out() << "Obj id=" << obj_id << " locally is  " << eobj->get_id() << ", sent nodes=";
for (UInt i = 0; i < ends.size(); i++) Par::Out() << "nid:" << ends[i]->get_id() << " ";
Par::Out() << std::endl;
{
std::vector<MeshObj*> tnodes;
MeshObjConn::get_obj_nodes(*eobj, tnodes);
Par::Out() << ", and nodes local:";
for (UInt i = 0; i < tnodes.size(); i++) Par::Out() << "nid:" << tnodes[i]->get_id() << " ";
Par::Out() << std::endl;
}
*/
         } else if (interior_edge) {
           // Don't Create the interior edge.  
           continue;
         }

         /*-----------------------------------------------------------------------*/
         // Invariant:  No matter what came before, now eobj points to the shared
         // object.  It now remains to resolve ownership and id 

         // If proc < owner, then assign the node to proc and assign proc's gid
#ifdef CRE_DEBUG
Par::Out() << "\tlocal obj:" << eobj->get_id() << std::endl;
#endif
         if (proc < eobj->get_owner()) {
#ifdef CRE_DEBUG
Par::Out() << "Assigning owner " << proc << ", old owner:" << eobj->get_owner() << ", to obj:" << eobj->get_id() << " -> " << obj_id << std::endl;
#endif

           // First look the object up in final_sym_objs.  If there, delete it, since
           // the owner info and id are about to change.
           CommRel::CommNode tnode(eobj, eobj->get_owner());
           std::vector<CommRel::CommNode>::iterator lb = std::lower_bound(final_sym_objs.begin(),
                      final_sym_objs.end(), tnode);

           if (lb != final_sym_objs.end() && *lb == tnode) final_sym_objs.erase(lb);

           // Change the owner and the id.
           eobj->set_owner(proc);

           // Update to locally owned, if needed
           const Context &ctxt = GetMeshObjContext(*eobj);
           Context newctxt(ctxt);
           // By definition, proc != self.
           newctxt.clear(Attr::OWNED_ID);
           if (newctxt != ctxt) {
             Attr attr(GetAttr(*eobj), newctxt);
             update_obj(eobj, attr);
           }

           // Update the map with the new id
           MeshObjIDMap &emap = get_map(obj_type);
           emap.erase(eobj);
           eobj->key = obj_id;
           std::pair<MeshObjIDMap::iterator,bool> iu =
              emap.insert(*eobj);
           if (!iu.second)
                Throw() << "Error inserting. Key already exists!!! Obj:" << *eobj << std::endl;

           // insert in sorted order (new id/proc will place us in a new spot) to the
           // sym spec resolver.
           tnode.processor = proc;
           lb = std::lower_bound(final_sym_objs.begin(),
                      final_sym_objs.end(), tnode);

           final_sym_objs.insert(lb, tnode);
            

           /*-------------------------------------------------------------------------*/
           // Now update all the polarities so they match the owner.  We do not 
           // worry about polarities if the object is a child because either
           // a) the object already existed, which means it has a parent here, which
           // means the child object inherits its orientation from the parent (which must
           // have been reconciled, or b) the object has no element here, in which case
           // it inherits the parent orientation.  Maybe, anyway...
           bool is_flipped = false;
           if (obj_type != MeshObj::NODE && obj_ord < 0) {
#ifdef CRE_DEBUG
Par::Out() << "\treset orientations, obj:" << eobj->get_id() << std::endl;
#endif
             for (UInt i = 0; i < elems.size(); i++) {
               MeshObj &elem = *elems[i];
               MeshObjRelationList::iterator ri = MeshObjConn::find_relation(elem, obj_type, ordinals[i]);
               ThrowRequire(ri != elem.Relations.end());
               MeshObj::Relation &rel = *ri;
               if (rel.polarity != polaritys[i]) is_flipped = true;
               rel.polarity = polaritys[i];
               rel.rotation = obj_type == MeshObj::FACE ? rotations[i] : 0;
               ThrowRequire(rel.obj == eobj);
               // And the back relation
               MeshObjRelationList::iterator eri = MeshObjConn::find_relation(*eobj, MeshObj::ELEMENT, ordinals[i]);
               ThrowRequire(eri != eobj->Relations.end());
               // I may be the edge of the same ordinal for multiple elements, fine the right one:
               while (eri != eobj->Relations.end() &&
                      eri->obj->get_type() == MeshObj::ELEMENT &&
                      eri->obj->get_id() != elem.get_id()) eri++;
               ThrowRequire(eri->obj == &elem);
               MeshObj::Relation &erel = *eri;
               erel.polarity = polaritys[i];
               erel.rotation = obj_type == MeshObj::FACE ? rotations[i] : 0;
             }

             // Does the object have children?  Only should be the case if a once interior
             // edge.
             MeshObjRelationList::iterator eci = MeshObjConn::find_relation(*eobj, obj_type, 0, MeshObj::CHILD);
             if (eci != eobj->Relations.end()) {
#ifdef CRE_DEBUG
Par::Out() << MeshObjTypeString(obj_type) << " " << eobj->get_id() << " has children.  is_flipped= " << is_flipped << "!!" << std::endl;
#endif
               ThrowRequire(obj_type == MeshObj::EDGE);
               if (is_flipped) {

                 // Now the annoying process of flipping the children.  For the edge,
                 // it is actually somewhat simple:  We switch edges, and then reverse
                 // the polarities.
  
  
                 /*-----------------------------------*/
                 // Reset the eobj->child pointers
                 MeshObj::Relation r0 = *eci;
                 eobj->Relations.erase(eci);
            
                 MeshObjRelationList::iterator eci1 = MeshObjConn::find_relation(*eobj, obj_type, 1, MeshObj::CHILD);
                 ThrowRequire(eci1 != eobj->Relations.end());
                 MeshObj::Relation r1 = *eci1;
                 eobj->Relations.erase(eci1);

                 // Reset
                 r0.ordinal = 1; 
                 r1.ordinal = 0;
                 AddMeshObjRelation(*eobj, r0);
                 AddMeshObjRelation(*eobj, r1);

                 /*-----------------------------------*/
                 // Reset the child->eobj pointers
                 MeshObjRelationList::iterator pi = MeshObjConn::find_relation(*r0.obj, obj_type, 0, MeshObj::PARENT);
                 ThrowRequire(pi != r0.obj->Relations.end());
                 pi->ordinal = 1;
                 pi = MeshObjConn::find_relation(*r1.obj, obj_type, 1, MeshObj::PARENT);
                 ThrowRequire(pi != r1.obj->Relations.end());
                 pi->ordinal = 0;

                 // And now flip the polarity for an objects using these two edges.
                 std::vector<MeshObj*> edge_elem;
                 
                 for (UInt i = 0; i < r0.obj->Relations.size(); i++) {
                   MeshObj::Relation &rel = r0.obj->Relations[i];
                   if (rel.type == MeshObj::USED_BY) {
                     rel.polarity = !rel.polarity;
                     MeshObjRelationList::iterator br = MeshObjConn::find_relation(*rel.obj, obj_type, rel.ordinal, MeshObj::USES);
                     ThrowRequire(br != rel.obj->Relations.end());
                     br->polarity = !br->polarity;
                   }
                 }
                 for (UInt i = 0; i < r1.obj->Relations.size(); i++) {
                   MeshObj::Relation &rel = r1.obj->Relations[i];
                   if (rel.type == MeshObj::USED_BY) {
                     rel.polarity = !rel.polarity;
                     MeshObjRelationList::iterator br = MeshObjConn::find_relation(*rel.obj, obj_type, rel.ordinal, MeshObj::USES);
                     ThrowRequire(br != rel.obj->Relations.end());
                     br->polarity = !br->polarity;
                   }
                 }

               } // flipped
               
             } // has children to re-orient
           }
         } // if proc < eobj->owner

       } // b ! empty
     } // unpack

     if (!msg.empty())
        Throw() << "Message buffer not emptied in edge resolution!!";

    } // send msg

    /*-------------------------------------------------------------------*/
    // Now that we have new shared objects, we need to rebuild the sym
    // commspec for this object type.  We use the resolution spec 
    // to build a symetric spec, then update object attributes to reflect
    // their sharing, and finally add these objects into the symmetric spec.
    /*-------------------------------------------------------------------*/

    // Symetrize the comm spec, so everyone knows any objects that are
    // shared.
    orel1.BuildFromOwner(*this, final_sym_objs);

    // Loop through comm and make sure all objects are marked as shared
    CommRel::MapType::iterator dbi = orel1.domain_begin(), dbe = orel1.domain_end();
    for (; dbi != dbe; ++dbi) {
      MeshObj &obj = *dbi->obj;
      const Attr &oattr = GetAttr(obj);
      const Context &ctxt = GetMeshObjContext(obj);
      Context newctxt(ctxt);
      newctxt.set(Attr::SHARED_ID);
      if (obj.get_owner() == Par::Rank())
        newctxt.set(Attr::OWNED_ID);
      else
        newctxt.clear(Attr::OWNED_ID);
      if (newctxt == ctxt) continue; // no need to update.
      Attr attr(oattr, newctxt);
      update_obj(&obj, attr);
    }

//orel1.Print(Par::Out());
   
    // And, lastly, add all these objects to the symmetric edge comm.
    GetCommRel(obj_type).Append(orel1);
#ifdef CRE_DEBUG
Par::Out() << "Sym comm:" << std::endl;
GetCommRel(obj_type).Print(Par::Out());
#endif

  } // subcases
  } // For object type

  // We assume all objects are taken care of.  Let base class remove pendingcreate bit
  MeshDB::ResolvePendingCreate();

#ifdef CRE_DEBUG
  MeshObjConn::verify_edge_relations(*this);
  MeshObjConn::verify_face_relations(*this);
#endif

}

void Mesh::ResolvePendingDelete() {
  Trace __trace("Mesh::ResolvePendingDelete()");
//  std::cout << "Warning!! ResolvePendingDelete not yet implemented in pmesh!" << std::endl;

  /*-------------------------------------------------------------------*/
  // There are difficulties here, but not as bad as with create.
  // Essentially, any element that is a child must find out whether
  // they are ghosted on a neighboring processor.  If so, we go inactive,
  // but we do not delete ourselves yet.
  UInt csize = Par::Size();

  for (int tp = NumMeshObjTypes - 1; csize > 1 && tp >= 0; tp--) 
  {
    UInt obj_type = MeshObjTypes[tp];
    std::vector<CommRel::CommNode> ecnodes;

    std::vector<MeshObj*> to_delete_list;

    /*---------------------------------------*/
    // Get the shared objects and the procs they
    // are shared with.
    build_shared_comm(*this, obj_type, ecnodes, Attr::PENDING_DELETE_ID);

/*
Par::Out() << "Investigating delete for " << MeshObjTypeString(obj_type) <<std::endl;
for (UInt i = 0; i < ecnodes.size(); i++)
Par::Out() << ecnodes[i].obj->get_id() << ", proc:" << ecnodes[i].processor << std::endl;
Par::Out() << std::endl;
*/

    // For the object to be deleted, we need it to be either on PENDING_DELETE
    // or unused on all other processors.  Otherwise, we just put object on the 
    // we keep it, and we reconnect the parent relations.


    // Here we collect some data structures that need global scope (beyond each message)
    std::vector<UInt> to_proc;
    std::vector<UInt> send_sizes_all(csize, 0);
    std::vector<UInt> to_sizes;
    std::map<UInt, std::vector<int> > reply;  // proc -> info about objects from proc.

    // Store the votes in a map.  Maybe a better way...
    typedef std::map<MeshObj::id_type, int> Vote_Map_Type;
    Vote_Map_Type vote_map;

    // Keep list of edges to take off spec because another processor is
    // removing an interior edge.
    std::vector<MeshObj*> int_edges;


    // Keep each message in a local scope, since the SparseMsg size can be quite large.
    {

      enum {DEL_NORMAL=0, DEL_FORCE=1};

      // Send out a query.
      SparseMsg msg;

      std::vector<CommRel::CommNode>::iterator di = ecnodes.begin(), de = ecnodes.end();

      // ** Sizing Loop **
      for (; di != de; ++di) {
        UInt proc = di->processor;
        std::vector<UInt>::iterator lb = std::lower_bound(to_proc.begin(), to_proc.end(), proc);
        if (lb == to_proc.end() || *lb != proc)
          to_proc.insert(lb, proc);

        // Send our id (much easier than creation time!!!)
        send_sizes_all[proc] += SparsePack<MeshObj::id_type>::size();

        // Send info about the deletion
        send_sizes_all[proc] += SparsePack<int>::size();
      }

      // Set the communication pattern, collect sizes
      UInt nsend = to_proc.size();
      msg.setPattern(nsend, nsend == 0 ? NULL : &to_proc[0]);

      to_sizes.resize(nsend, 0);
      for (UInt i = 0; i < nsend; i++)
        to_sizes[i] = send_sizes_all[to_proc[i]];

      msg.setSizes(nsend == 0 ? NULL : &to_sizes[0]);

      // ** Now pack message **
      for (di = ecnodes.begin(); di != de; ++di) {
        MeshObj *obj = di->obj;

        UInt proc = di->processor;
//Par::Out() << "pck proc " << proc;
        SparseMsg::buffer &b = *msg.getSendBuffer(proc);

        // Pack id
        SparsePack<MeshObj::id_type>(b, obj->get_id());

        // Send delete info
        // If the object is an interior edge about to die, let other proces know so they can take it off
        // their commspec's
        int del_stat = (obj_type == MeshObj::EDGE && obj->Relations.size() == 0) ? DEL_FORCE : DEL_NORMAL;
        SparsePack<int>(b, del_stat);

      } // pack

      if (!msg.filled())
        Throw() << "Message not filled in object resoltuon, obj type:" << MeshObjTypeString(obj_type);

      // ** Send Message **
      msg.communicate();

      // Set up reply while unpacking.  TODO:the reply is simply the transpose
      // of the send.  It would be nice to use this fact...
      std::fill(send_sizes_all.begin(), send_sizes_all.end(), 0);
      to_proc.clear();
      to_sizes.clear();


      // ** Unpack **
      for (UInt *p = msg.inProc_begin(); p != msg.inProc_end(); ++p) {
        UInt proc = *p;
         SparseMsg::buffer &b = *msg.getRecvBuffer(proc);

       std::vector<int> &proc_reply = (reply.insert(std::make_pair(proc, std::vector<int>()))).first->second;

       // Should be this many messages incoming, hence same outgoing.
       proc_reply.reserve(b.msg_size() / SparsePack<MeshObj::id_type>::size());

       while (!b.empty()) {
         MeshObj::id_type obj_id;

         // Unpack id
         SparseUnpack<MeshObj::id_type>(b, obj_id);

         int del_stat;
         SparseUnpack<int>(b, del_stat);


#ifdef DEL_DEBUG
Par::Out() << "request about " << obj_id << " from proc:" << proc << ", del_stat=" << del_stat << std::endl;
#endif

         // Set up reply.  Will send back 0 = not ok, 1 = (ok to delete)
         send_sizes_all[proc] += SparsePack<int>::size();
         std::vector<UInt>::iterator lb = std::lower_bound(to_proc.begin(), to_proc.end(), proc);
         if (lb == to_proc.end() || *lb != proc)
           to_proc.insert(lb, proc);


         // Investigate: is it ok to delete the object from my perspective:
         // 1) If object is on my pendingdelete, great!
         // 2) If object is not present on this processor (think interior edge)
         // 3) Object is only ghosted (no used_by, only parent/child relations)
        
         // Find object:
         MeshObjIDMap &omap = get_map(obj_type);
         MeshObjIDMap::iterator mi = omap.find(obj_id);
         

         // Case 2:
         if (mi == omap.end()) {
           proc_reply.push_back(1); // Either an interior edge or object on side of element surrounded by
                                    // a neighboring processor
/*
           if (obj_type != MeshObj::EDGE) {
             Throw() << "P:" << Par::Rank() << ", Obj not edge!!, " << MeshObjTypeString(obj_type) << " id=" << obj_id;
           }
*/
#ifdef DEL_DEBUG
Par::Out() << "\tok to delete: object not on this proc" << std::endl;
#endif
         } else if (GetAttr(*mi).get_context().is_set(Attr::PENDING_DELETE_ID)) {
           // Case 1:
           proc_reply.push_back(1); // have at it
#ifdef DEL_DEBUG
Par::Out() << "\tok to delete: on my pending delete" << std::endl;
#endif
         } else {

           // If interior edge is going away on another proc, take off comm spec
           if (del_stat == DEL_FORCE) {
             std::vector<MeshObj*>::iterator lb =
                std::lower_bound(int_edges.begin(), int_edges.end(), &*mi);
             if (lb == int_edges.end() || *lb != &*mi) 
               int_edges.insert(lb, &*mi);
           }

           // Look into case 3.
           MeshObj &other_obj = *mi;
           MeshObjRelationList::iterator ri = other_obj.Relations.begin(), re = other_obj.Relations.end();
           bool used_by_found = false;
           for (; !used_by_found && ri != re; ++ri) {
             if (ri->type == MeshObj::USED_BY) used_by_found = true;
           }

           if (used_by_found) {
             proc_reply.push_back(0); // no way, I still use the object
#ifdef DEL_DEBUG
Par::Out() << "\t not ok to delete!! used_by relations here" << std::endl;
#endif
           } else {
             proc_reply.push_back(1); // ok, object must be ghosted
#ifdef DEL_DEBUG
Par::Out() << "\tok to delete: no used_by relations here" << std::endl;
#endif
           }
        }

       }
    
      } // unpack

      if (!msg.empty())
        Throw() << "Message buffer not emptied in edge resolution!!";
    } // Message

    { // reply message
      SparseMsg rmsg;

      // Set the communication pattern, collect sizes
      UInt nsend = to_proc.size();
      rmsg.setPattern(nsend, nsend == 0 ? NULL : &to_proc[0]);

      to_sizes.resize(nsend, 0);
      for (UInt i = 0; i < nsend; i++)
        to_sizes[i] = send_sizes_all[to_proc[i]];

      rmsg.setSizes(nsend == 0 ? NULL : &to_sizes[0]);

      // Pack message
      for (UInt i = 0; i < to_proc.size(); i++) {
        UInt proc = to_proc[i];
        std::map<UInt, std::vector<int> >::iterator pmi = reply.find(to_proc[i]);
        ThrowRequire(pmi != reply.end());
        const std::vector<int> &proc_reply = pmi->second;

        SparseMsg::buffer &b = *rmsg.getSendBuffer(proc);

        for (UInt n = 0; n < proc_reply.size(); n++) {
          SparsePack<int>(b, proc_reply[n]);
        }
        
      } // pack

      if (!rmsg.filled()) {
        Throw() << "Message not filled in delete response!!";
      }

      // garbage collect reply map
      std::map<UInt,std::vector<int> >().swap(reply);

      // Set up ecnodes to parse the response.  Sort it by processor (stable sort).
      // In this way, the messages will come in in the same order they were sent out
      // AND we can unpack by processor.
      std::stable_sort(ecnodes.begin(), ecnodes.end(), CommRel::range_sort());

      // Communicate
      rmsg.communicate();

      // Unpack the responses.
      for (UInt *p = rmsg.inProc_begin(); p != rmsg.inProc_end(); ++p) {
        UInt proc = *p;
         SparseMsg::buffer &b = *rmsg.getRecvBuffer(proc);

        // Find correct location in ecnodes
        std::vector<CommRel::CommNode>::iterator clb =
          std::lower_bound(ecnodes.begin(), ecnodes.end(), CommRel::CommNode(NULL, proc), CommRel::range_sort());

        if (clb == ecnodes.end() || clb->processor != proc)
          Throw() << "Confusion in ecnodes, unpacking delete response!!";

        while(clb != ecnodes.end() && clb->processor == proc) {

          MeshObj &obj = *clb->obj; 

          int vote;
          SparseUnpack<int>(b, vote);
 
          // Try to insert the vote (in case object hasn't yet been inserted)
          std::pair<Vote_Map_Type::iterator,bool> ins_it = vote_map.insert(std::make_pair(obj.get_id(), vote));

#ifdef DEL_DEBUG
Par::Out() << "Vote map insert:" << MeshObjTypeString(obj_type) << " " << obj.get_id() << " from proc:" << proc << ", vote=" << vote << std::endl;
#endif
    
          // If object already exists, vote: all objects must allow delete or it doesn't happen.
          if (ins_it.second == false) {
            ins_it.first->second = (ins_it.first->second == 1 && vote == 1 ? 1 : 0); // all or nothing
          }
           
          ++clb;
        } // objects for this proc

      } // unpack delete responses

      if (!rmsg.empty())
        Throw() << "Reply delete message not emptied!!";
    } // reply message

    // Now, finally, we must send out the results of the vote, since processors
    // not deleting an object need to know if they should delete the object.


    // go back to an 'object-centric' view.  Will sort by id, proc
    std::sort(ecnodes.begin(), ecnodes.end());
    {
      SparseMsg vmsg;

      std::fill(send_sizes_all.begin(), send_sizes_all.end(), 0);
      to_proc.clear();
      to_sizes.clear();

      // Sizing loop: for each object to be deleted, we will send the id.
      std::vector<CommRel::CommNode>::iterator di = ecnodes.begin(), de = ecnodes.end();
      for (; di != de;) {
        MeshObj &obj = *di->obj;

        Vote_Map_Type::iterator vmi = vote_map.find(obj.get_id());

        if (vmi == vote_map.end()) {
          Throw() << "Did not find vote map obj:" << obj.get_id() << " in map:";
        }

        int vote = vmi->second; 

        // If vote says delete item, forward a delete message to all
        // procs involved.
        do {
          if (vote == 1) {

            UInt proc = di->processor;
            send_sizes_all[proc] += SparsePack<MeshObj::id_type>::size();
            
            // add processor
            std::vector<UInt>::iterator lb = std::lower_bound(to_proc.begin(), to_proc.end(), proc);
            if (lb == to_proc.end() || *lb != proc)
               to_proc.insert(lb, proc);
          }
        } while ((++di) != ecnodes.end() && di->obj == &obj);

      } // di

     // Set the communication pattern, collect sizes
      UInt nsend = to_proc.size();
      vmsg.setPattern(nsend, nsend == 0 ? NULL : &to_proc[0]);

      to_sizes.resize(nsend, 0);
      for (UInt i = 0; i < nsend; i++)
        to_sizes[i] = send_sizes_all[to_proc[i]];

      vmsg.setSizes(nsend == 0 ? NULL : &to_sizes[0]);

      // Packing loop
      di = ecnodes.begin();
      for (; di != de;) {
        MeshObj &obj = *di->obj;

        Vote_Map_Type::iterator vmi = vote_map.find(obj.get_id());
        ThrowRequire(vmi != vote_map.end());

        int vote = vmi->second; 

        // If vote says delete item, forward a delete message to all
        // procs involved.
        do {
          if (vote == 1) {

            UInt proc = di->processor;

            // Send id of obj to be deleted.
            SparseMsg::buffer &b = *vmsg.getSendBuffer(proc);
            SparsePack<MeshObj::id_type>(b, obj.get_id());
            
          }
        } while ((++di) != ecnodes.end() && di->obj == &obj);

      } // di

      if (!vmsg.filled())
        Throw() << "Vote message not filled!";

      // Communicate
      vmsg.communicate();

      // Unpack:
      MeshObjIDMap &omap = get_map(obj_type);
      for (UInt *p = vmsg.inProc_begin(); p != vmsg.inProc_end(); ++p) {
        UInt proc = *p;
        SparseMsg::buffer &b = *vmsg.getRecvBuffer(proc);

        while (!b.empty()) {
          MeshObj::id_type obj_id;
       
          SparseUnpack<MeshObj::id_type>(b, obj_id);


          MeshObjIDMap::iterator mi = omap.find(obj_id);
          if (mi == omap.end()) {
            //ThrowRequire(obj_type == MeshObj::EDGE);
            continue;
          }

          MeshObj &dobj = *mi;


#ifdef DEL_DEBUG
Par::Out() << "Deleting object: " << MeshObjTypeString(obj_type) << " " << obj_id << std::endl;
#endif

          const Context &ctxt = GetMeshObjContext(dobj);
          Context newctxt(ctxt);
          newctxt.set(Attr::PENDING_DELETE_ID);

          if (newctxt != ctxt) {
            Attr attr(GetAttr(dobj), newctxt);
            update_obj(&dobj, attr);
          }

          // Save a list of deleted objects to remove from comm rel
          if (GetAttr(dobj).is_shared()) {
            std::vector<MeshObj*>::iterator tdl = 
                std::lower_bound(to_delete_list.begin(), to_delete_list.end(), &dobj);
            if (tdl == to_delete_list.end() || *tdl != &dobj)
              to_delete_list.insert(tdl, &dobj);
          }

        } // !b.empty

      } // unpack

      if (!vmsg.empty())
        Throw() << "Vote message not emptied!!";

    } // vote message

    // We have now placed on pending delete all requests from others.
    // We also need to put our originating requests on pending delete.
    Vote_Map_Type::iterator vmi = vote_map.begin(), vme = vote_map.end();
    MeshObjIDMap &omap = get_map(obj_type);
    for (; vmi != vme; ++vmi) {

      MeshObjIDMap::iterator mi = omap.find(vmi->first);
      ThrowRequire(mi != omap.end());
      MeshObj &dobj = *mi;

      // Delete all of my requests that either 
      //    a) pass the vote test
      //    b) are an interior edge that has been marooned, i.e. no relations
      if (vmi->second == 1 || (obj_type == MeshObj::EDGE && dobj.Relations.size() == 0)) {

#ifdef DEL_DEBUG
Par::Out() << "Deleting object: " << MeshObjTypeString(obj_type) << " " << vmi->first << std::endl;
#endif

        const Context &ctxt = GetMeshObjContext(dobj);
        Context newctxt(ctxt);
        newctxt.set(Attr::PENDING_DELETE_ID);
  
        if (newctxt != ctxt) {
          Attr attr(GetAttr(dobj), newctxt);
          update_obj(&dobj, attr);
        }
        // Save a list of deleted objects to remove from comm rel
        if (GetAttr(dobj).is_shared()) {
          std::vector<MeshObj*>::iterator tdl = 
              std::lower_bound(to_delete_list.begin(), to_delete_list.end(), &dobj);
          if (tdl == to_delete_list.end() || *tdl != &dobj)
            to_delete_list.insert(tdl, &dobj);
        }

      } else { // vote != 1, take off pending delete.
        const Context &ctxt = GetMeshObjContext(dobj);
        Context newctxt(ctxt);
        newctxt.clear(Attr::PENDING_DELETE_ID);

        if (newctxt != ctxt) {
          Attr attr(GetAttr(dobj), newctxt);
          update_obj(&dobj, attr);
        }
      }

    }

#ifdef DEL_DEBUG
Par::Out() << "Objects to delete:" << std::endl;
for (UInt i = 0; i < to_delete_list.size(); i++) 
  Par::Out() << "\t" << MeshObjTypeString(to_delete_list[i]->get_type()) << " " << to_delete_list[i]->get_id() << std::endl;

Par::Out() << "commrel before delete:" << std::endl;
    GetCommRel(obj_type).Print(Par::Out());
#endif

    // Remove from commrel
    GetCommRel(obj_type).remove_domain(to_delete_list);
    GetCommRel(obj_type).remove_domain(int_edges);

// Verify objects were deleted:
#ifdef DEL_DEBUG
{
CommRel &nrel = GetCommRel(obj_type);
for (UInt i = 0; i < to_delete_list.size(); i++)  {
  CommRel::MapType::iterator lb = std::lower_bound(nrel.domain_begin(), nrel.domain_end(), CommRel::CommNode(to_delete_list[i], 0));
  if (lb != nrel.domain_end() && lb->obj == to_delete_list[i])
    Throw() << "Did not delete object:" << MeshObjTypeString(to_delete_list[i]->get_type()) << " " << to_delete_list[i]->get_id();
}
}
#endif

#ifdef DEL_DEBUG
Par::Out() << "New commrel after delete:" << std::endl;
    GetCommRel(obj_type).Print(Par::Out());
#endif

    // Wipes out objects.
    MeshDB::ResolvePendingDelete(obj_type);

  } // object type

}

const CommRel &Mesh::GetSymNodeRel() const {
  return GetCommRel(MeshObj::NODE);
}

void Mesh::Commit() {
  Trace __trace("Mesh::Commit()");

  if (committed)
    Throw() << "Mesh is already committed!";

  // Create sides/edges, if requested
  if (use_sides)
    CreateAllSides();
  ResolvePendingCreate();

  FieldReg::CreateDBFields();
  FieldReg::Commit(*this);

  MeshDB::Commit(FieldReg::NumFields(), FieldReg::ListOfFields(), Numfields(), FieldReg::ListOffields());

  FieldReg::PopulateDBFields(*this);
  FieldReg::ReleaseDBFields();

  // Parallel stuff.
}

void Mesh::CreateGhost() {
  if (sghost) return; // must already be scratched
  sghost = new CommReg("_ghost", *this, *this);

  std::vector<CommRel::CommNode> selem;

  // Loop through the shared nodes; add all attached elements.
  KernelList::iterator ms = set_begin(), me = set_end();
  for (; ms != me; ++ms) {
    
    if (ms->type() == (UInt) MeshObj::NODE && ms->GetAttr().is_shared()) {
      
      Kernel::obj_iterator oi = ms->obj_begin(), oe = ms->obj_end();
      for (; oi != oe; ++oi) {
        
        MeshObj &node = *oi;
        std::vector<MeshObj*> elem;
        std::vector<MeshObj*> on(1);
        on[0] = &node;
        
        // Get all the elements this node is used by.
        MeshObjConn::common_objs(on.begin(),on.end(), MeshObj::USED_BY, MeshObj::ELEMENT, elem);
        
        // Find the procs this node goes to and send the element there
        std::vector<UInt> nprocs;
        MeshObjConn::get_node_sharing(node, GetSymNodeRel(), nprocs);
        for (UInt i = 0; i < nprocs.size(); i++) {
          
          // Add the elements (if not already present)
          for (UInt e = 0; e < elem.size(); e++) {
            CommRel::CommNode cnode(elem[e], nprocs[i]);
            
            std::vector<CommRel::CommNode>::iterator lb = 
              std::lower_bound(selem.begin(), selem.end(), cnode);
              
            if (lb == selem.end() || *lb != cnode)
              selem.push_back(CommRel::CommNode(elem[e], nprocs[i]));
              
          }
        
        } // for i
        
      } // for oi 
      
    } // if kernel matches
    
  } // ms

  CommRel &ecomm = sghost->GetCommRel(MeshObj::ELEMENT);
  
  ecomm.add_domain(selem);
  
  ecomm.dependants(sghost->GetCommRel(MeshObj::NODE), MeshObj::NODE);
  ecomm.dependants(sghost->GetCommRel(MeshObj::EDGE), MeshObj::EDGE);
  ecomm.dependants(sghost->GetCommRel(MeshObj::FACE), MeshObj::FACE);

  // Ship objects over;
   // First the sourcemesh
  sghost->GetCommRel(MeshObj::NODE).build_range(true);
  sghost->GetCommRel(MeshObj::EDGE).build_range(true);
  sghost->GetCommRel(MeshObj::FACE).build_range(true);
  sghost->GetCommRel(MeshObj::ELEMENT).build_range(true);


  // Send coordinates
  MEField<> *cd = GetCoordField();
  sghost->SendFields(1, &cd, &cd);

}

void Mesh::RemoveGhost() {
  if (!sghost) return; // must already be scratched
  
  sghost->GetCommRel(MeshObj::ELEMENT).delete_range();
  sghost->GetCommRel(MeshObj::FACE).delete_range();
  sghost->GetCommRel(MeshObj::EDGE).delete_range();
  sghost->GetCommRel(MeshObj::NODE).delete_range();
  
  delete sghost; sghost = 0; // does delete do this?? don't remember...
}

void Mesh::build_sym_comm_rel(UInt obj_type) {
  Trace __trace("Mesh::build_sym_comm_rel(UInt obj_type)");

   CommRel &ncom = GetCommRel(obj_type);

  // Loop the mesh; find shared nodes and add
   std::vector<CommRel::CommNode> snodes;
 
   // Ok, now the trick.  Loop through nodes, if _OWNER != rank, then node is owned
   MeshDB::iterator ni = obj_begin(obj_type), ne = obj_end(obj_type);
   if (Par::Size() > 1)
   for (; ni != ne; ++ni) {
     MeshObj &node = *ni;
     UInt oproc = node.get_owner();
     if (oproc != (UInt) Par::Rank()) {
 //Par::Out() << "proc:" << Par::Rank() << ", shared obj:" << MeshObjTypeString(obj_type) << ", id=" << node.get_id() << ", owner=" << node.get_owner() << std::endl;
       snodes.push_back(CommRel::CommNode(&node, oproc));
     }

     if (oproc >= Par::Size()) {
       Par::Out() << "Error! rank is greater than nproc:obj:" << node;
       Throw() << "Bad processor number!";
     }
   }
   ncom.BuildFromOwner(*this, snodes);
   std::vector<CommRel::CommNode>().swap(snodes);
 
   // We now know all the shared objs, so loop through and mark them.  Also,
   // update the locally_owned attr;
   CommRel::MapType::iterator sni = ncom.domain_begin(), sne = ncom.domain_end();
   std::vector<MeshObj*> psnodes;
 
   for (; sni != sne; ++sni) {
   
     psnodes.push_back(sni->obj);
   }
 
   std::sort(psnodes.begin(), psnodes.end());
   psnodes.erase(std::unique(psnodes.begin(), psnodes.end()), psnodes.end());
 
   // snodes now contains the shared node list.  Loop through and update nodes;
   std::vector<MeshObj*>::iterator spi = psnodes.begin(), spe = psnodes.end();
   for (; spi != spe; ++spi) {
     MeshObj *node = *spi;
     UInt oproc = node->get_owner();
     bool lowned = (oproc == (UInt) Par::Rank());
     const Context &ctxt = GetMeshObjContext(*node);
     Context newctxt(ctxt);
     newctxt.set(Attr::SHARED_ID);
     if (lowned)
       newctxt.set(Attr::OWNED_ID);
     else
       newctxt.clear(Attr::OWNED_ID);
     if (newctxt != ctxt) {
       Attr attr(GetAttr(*node), newctxt);
       update_obj(node, attr);
     }
  }

}

} // namespace
} // namespace
