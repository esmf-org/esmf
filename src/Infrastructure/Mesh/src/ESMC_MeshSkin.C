// $Id: ESMC_MeshSkin.C,v 1.1.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshSkin.h>
#include <ESMC_MeshDB.h>
#include <ESMC_MeshObj.h>
#include <ESMC_MeshObjTopo.h>
#include <ESMC_MeshObjConn.h>
#include <ESMC_Kernel.h>
#include <ESMC_SparseMsg.h>
#include <ESMC_ParEnv.h>

#include <iostream>
#include <fstream>

namespace ESMCI {
namespace MESH {

const UInt exposed_sideset_key = 10001;

void Skin(Mesh &mesh) {
  Trace __trace("Skin(Mesh &mesh)");
  // Strategy:
  // 1) loop every element of the mesh.  Subloop every face of the element.
  //   -if the face is already in a sideset, continue
  //   -else see if there is an element across the face
  //    - if no, create a face, put in the exterior sideset
  //    - if yes, if face is in same block, continue
  //      - otherwise, create a face, put in the interior boundary sideset

  // One must beware, since I am updating nodes and sides within an iterator
  // through elements.  However, since elements come AFTER nodes and sides,
  // I know the iterator for element will not be affected.

//std::cout << "Skinning mesh up reaal goood!" << std::endl;
  MeshDB::iterator eit = mesh.elem_begin(), eet = mesh.elem_end();
  for (; eit != eet; eit++) {
    MeshObj &elem = *eit;
//std::cout << "elem:" << elem.get_id() << std::endl;
    const MeshObjTopo *const topo = GetMeshObjTopo(elem);
    for (UInt nf = 0; nf < topo->num_sides; nf++) {
      // Is there already a side here?
      MeshObj::MeshObjType side_type = mesh.side_type();
      MeshObjRelationList::iterator rel = MeshObjConn::find_relation(elem,side_type, nf);

      if (rel != elem.Relations.end()) {
//std::cout << "face:" << nf << " has side " << rel->obj->get_id() << std::endl;
        // Check to see if an exterior face
        // See if an element on other side.
        bool exterior_face = true;
        MeshObjRelationList::iterator oel = MeshObjConn::find_relation(*rel->obj,MeshObj::ELEMENT, 0);
        while(oel != rel->obj->Relations.end() && oel->obj->get_type() == MeshObj::ELEMENT) {
          if (oel->obj != &elem) {
            exterior_face = false;
            break;
          }
          oel++;
        }
        // Mark nodes with exposed
        if (exterior_face) {
          MeshObj &side = *rel->obj;
          const Attr &oattr = GetAttr(side);
          const Context &ctxt = GetMeshObjContext(side);
          Context newctxt(ctxt);
          newctxt.set(Attr::EXPOSED_BOUNDARY_ID);
          if (newctxt != ctxt) {  // no need to update.
            Attr attr(oattr, newctxt);
            mesh.update_obj(&side, attr);
/*
Par::Out() << "Skin, updating side:" << side.get_id() << std::endl;
Par::Out() << "is pending create=" << GetMeshObjContext(side).is_set(Attr::PENDING_CREATE_ID) << std::endl;
*/
          }
          for (UInt n = 0; n < topo->get_num_side_nodes(); n++) {
            MeshObjRelationList::iterator nrel = 
              MeshObjConn::find_relation(elem, MeshObj::NODE, 
                  topo->get_side_nodes(rel->ordinal)[n]);
            MeshObj &node = *nrel->obj;
            const Attr &oattr = GetAttr(node);
            const Context &ctxt = GetMeshObjContext(node);
            Context newctxt(ctxt);
            newctxt.set(Attr::EXPOSED_BOUNDARY_ID);
            if (newctxt == ctxt) continue; // no need to update.
            Attr attr(oattr, newctxt);
            mesh.update_obj(&node, attr);
          }
        }
      } else {
        // We need to see if there is a side opposite
//std::cout << "face:" << nf << " has no side" << std::endl;
        MeshObj *el = MeshObjConn::opposite_element(elem, nf);
        if (el == NULL) {
//std::cout << "\tno opposite element.  Exterior face" << std::endl;
          // Create a face obj
          MeshObj::MeshObjType tp = topo->parametric_dim == 2 ? MeshObj::EDGE : MeshObj::FACE;
          MeshObj *side = new MeshObj(tp, mesh.get_new_local_id(tp));
          //side->add_data("sideset", 10000);
          // TODO: get side topo from parent
          const MeshObjTopo *stopo = topo->side_topo(nf);
//std::cout << "side topo:" << stopo->name << std::endl;
          //mesh.add_side_local(*side, elem, nf, 0, stopo);
          
          Context newctxt;
          newctxt.set(Attr::EXPOSED_BOUNDARY_ID); // orthis in
          mesh.add_side_local(*side, elem, nf, exposed_sideset_key, stopo, newctxt);

          // Mark all faces nodes as exterior
          for (UInt n = 0; n < topo->get_num_side_nodes(); n++) {
            MeshObjRelationList::iterator nrel = 
            MeshObjConn::find_relation(elem, MeshObj::NODE, 
                  topo->get_side_nodes(nf)[n]);
            MeshObj &node = *nrel->obj;
            const Attr &oattr = GetAttr(node);
            const Context &ctxt = GetMeshObjContext(node);
            Context newctxt(ctxt);
            newctxt.set(Attr::EXPOSED_BOUNDARY_ID);
            if (newctxt == ctxt) continue; // no need to update.
            Attr attr(oattr, newctxt);
            mesh.update_obj(&node, attr);
          }
        } else {
/* Re-activate this to create Interblock boundaries.
//std::cout << "\topposite element:" << el->get_id() << std::endl;
          //int block_id = elem.get_int("block");
          int block_id = GetAttr(elem).globalKey;
          int oblock_id = GetAttr(*el).globalKey;
          //int oblock_id = el->get_int("block");
//std::cout << "\tblk:" << block_id << ", oblk:" << oblock_id << std::endl;
          if (block_id != oblock_id) {
//std::cout << "\telement is on an interior block boundary" << std::endl;
            // Create a face obj
            MeshObj::MeshObjType tp = topo->spatial_dim == 2 ? MeshObj::EDGE : MeshObj::FACE;
            if (add_faces) {
              MeshObj *side = new MeshObj(tp, mesh.get_next_unused_face_id());
              //side->add_data("sideset", 10001);
              // TODO:Get side topo from parent
              mesh.add_side(*side, elem, nf, 10001, NULL);
            }
          } // different block
*/
        }
      } // no face
      
    } // nf
  } // for elements

  // Resolve pending create
  mesh.ResolvePendingCreate();

  // We have created some local faces, so resolve global numbering
  ResolveParSkin(mesh);

}

void ResolveParSkin(Mesh &mesh) {
  Trace __trace("ResolveParSkin(Mesh &mesh)");

  // Any face that is both shared AND exposed can be deleted, since
  // it is really an interior face.

  KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();

  for (; ki != ke; ++ki) {
    if (ki->type() == (UInt) mesh.side_type() && ki->GetContext().is_set(Attr::EXPOSED_BOUNDARY_ID)
        && ki->GetContext().is_set(Attr::SHARED_ID))
    {
       Kernel::obj_iterator oi = ki->obj_begin(), oe = ki->obj_end(), on;
       for (; oi != oe; ) {

         // Manually update next link, since we may remove object from
         // list when changing context, below
         on = oi; ++on;

         MeshObj &side = *oi;
         // Deimprint exposed boundary from nodes.
         std::vector<MeshObj*> snodes;
         MeshObjConn::get_obj_nodes(side, snodes);
  
        for (UInt nd = 0; nd < snodes.size(); nd++) {
  
          // clear exposed from node.
          MeshObj &node = *snodes[nd];
          const Attr &oattr = GetAttr(node);
          const Context &ctxt = GetMeshObjContext(node);
          Context newctxt(ctxt);
          newctxt.clear(Attr::EXPOSED_BOUNDARY_ID);
          if (newctxt == ctxt) continue; // no need to update.
          Attr attr(oattr, newctxt);
  
          mesh.update_obj(&node, attr);
  
        } // for nd
  
        // Put side on pending delete const Attr &oattr = GetAttr(side);
        const Context &ctxt = GetMeshObjContext(side);
        Context newctxt(ctxt);
        newctxt.set(Attr::PENDING_DELETE_ID);
        if (newctxt != ctxt) {
    //std::cout << "Delete object:" << side.get_id() << std::endl;
          Attr attr(GetAttr(side), newctxt);
          mesh.update_obj(&side, attr);
        }

        oi = on;
      } // objects
    } // exposed, shared kernels
  } // kernels

  // Now, we may have de-imprinted too many nodes, so go back through and imprint exposed on
  // the nodes
  ki = mesh.set_begin();
  for (; ki != ke; ++ki) {
    if (ki->type() == (UInt) mesh.side_type() && ki->GetContext().is_set(Attr::EXPOSED_BOUNDARY_ID) &&
         !ki->GetContext().is_set(Attr::PENDING_DELETE_ID))
    {
       Kernel::obj_iterator oi = ki->obj_begin(), oe = ki->obj_end();
       for (; oi != oe; ++oi) {
       MeshObj &side = *oi;
       // Deimprint exposed boundary from nodes.
       std::vector<MeshObj*> snodes;
       MeshObjConn::get_obj_nodes(side, snodes);

        for (UInt nd = 0; nd < snodes.size(); nd++) {

        // set exposed from node.
        MeshObj &node = *snodes[nd];
        const Attr &oattr = GetAttr(node);
        const Context &ctxt = GetMeshObjContext(node);
        Context newctxt(ctxt);
        newctxt.set(Attr::EXPOSED_BOUNDARY_ID);

        if (newctxt != ctxt) {
          Attr attr(oattr, newctxt);
          mesh.update_obj(&node, attr);

        }

        } // for nd
  
      } // objects
    } // exposed, shared kernels
  } // kernels


  mesh.ResolvePendingDelete();

}

} // namespace
} // namespace
