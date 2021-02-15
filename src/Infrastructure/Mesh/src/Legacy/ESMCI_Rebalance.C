// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Legacy/ESMCI_Rebalance.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjConn.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>


#include <Mesh/src/Zoltan/zoltan.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

static bool form_rebalance_comm(Mesh &mesh, CommReg &migration);

static void set_new_obj_owners(Mesh &mesh, CommReg &migration, UInt obj_type);

static void set_new_elem_owners(Mesh &mesh, CommReg &migration);

static void build_obj_migration(Mesh &mesh, CommReg &migration, UInt obj_type);

/*
 * Set everyone to !SHARED, and set owner based on get_owner.
 */
static void resolve_rebalance_ownership(Mesh &mesh) {
  
  UInt rank = Par::Rank();
  
  Mesh::iterator oi = mesh.obj_begin_all(), oe = mesh.obj_end_all(), on;
  
  for (; oi != oe; ) {
    
    on = oi; ++on;
    
    MeshObj &obj = *oi;
    const Context &ctxt = GetMeshObjContext(obj);
  
    bool lowned = rank == obj.get_owner();
    
    if (ctxt.is_set(Attr::SHARED_ID)) {
      
      // Must clear shared.  Update lowned at same time.
      
      const Attr &oattr = GetAttr(obj);

      Context newctxt(ctxt);
      newctxt.clear(Attr::SHARED_ID);
      
      if (lowned) newctxt.set(Attr::OWNED_ID);
      else newctxt.clear(Attr::OWNED_ID);
      
      Attr attr(oattr, newctxt);
      mesh.update_obj(&obj, attr);
      
    } else {
      // Object not marked shared, but see if we need to update
      // locally owned status.
      
      if (lowned != (ctxt.is_set(Attr::OWNED_ID))) {
        
        const Attr &oattr = GetAttr(obj);

        Context newctxt(ctxt);
         
         if (lowned) newctxt.set(Attr::OWNED_ID);
         else newctxt.clear(Attr::OWNED_ID);
         
         Attr attr(oattr, newctxt);
         mesh.update_obj(&obj, attr);
      }
      
    }
    
    oi = on;
  }
}

void GetRebalanceComm(Mesh &mesh, CommReg &mig) {
  form_rebalance_comm(mesh, mig);
}

bool Rebalance(Mesh &mesh, CommReg &mig) {

#ifdef REBAL_DEBUG
  Par::Out() << "Rebalance comm:" << std::endl;
  mig.CommPrint(Par::Out());
#endif

  // Assign elements and child elements to new processors
  set_new_elem_owners(mesh, mig);

  // We will form the new sym specs based upon the nodal ownership.
  // Here we set the future node owners.  From this point until the
  // mesh is finally rebalanced, these owners will be inconsistent.
  // Same for edge/face.
  // A CAUTION: we will have to fix up the shared interior edges before
  // building the spec.
  set_new_obj_owners(mesh, mig, MeshObj::NODE);
  set_new_obj_owners(mesh, mig, MeshObj::EDGE);
  set_new_obj_owners(mesh, mig, MeshObj::FACE);

  // Form the node migration spec
  build_obj_migration(mesh, mig, MeshObj::NODE);
  build_obj_migration(mesh, mig, MeshObj::EDGE);
  build_obj_migration(mesh, mig, MeshObj::FACE);

//mig.GetCommRel(MeshObj::EDGE).Print(Par::Out());
  // Do this one last since the calls above traverse what
  // we have in this spec.
  build_obj_migration(mesh, mig, MeshObj::ELEMENT);

  // In theory (at least) now all we have to do is 
  // 1) ship objects over and create on new procs
  // 2) Delete sent objects on this proc
  // 3) Fix up interior edges that might need to be deleted
  // 4) Rebuild symmetric CommRel

  // Nodes first:  Send local field data at same time.
  {
    CommRel &nrel = mig.GetCommRel(MeshObj::NODE);
    nrel.build_range();
    nrel.send_fields(mesh.Numfields(), mesh.ListOffields(), mesh.ListOffields());
  }
  // Ok, that worked, so cross fingers and try edges
  {
    CommRel &crel = mig.GetCommRel(MeshObj::EDGE);
    crel.build_range();
    crel.send_fields(mesh.Numfields(), mesh.ListOffields(), mesh.ListOffields());
  }
  // Faces ??
  {
    CommRel &crel = mig.GetCommRel(MeshObj::FACE);
    crel.build_range();
    crel.send_fields(mesh.Numfields(), mesh.ListOffields(), mesh.ListOffields());
  }
  // Elements will surely wreak havoc, but try anyway
  {
    CommRel &crel = mig.GetCommRel(MeshObj::ELEMENT);
    crel.build_range();
    crel.send_fields(mesh.Numfields(), mesh.ListOffields(), mesh.ListOffields());
  }

  // Now, delete the local objects.  Start with elements, which 
  // we KNOW will go, then start zapping lower order objects with no
  // USED_BY/CHILD left.

  mig.GetCommRel(MeshObj::ELEMENT).delete_domain();
  mig.GetCommRel(MeshObj::FACE).delete_domain();
  mig.GetCommRel(MeshObj::EDGE).delete_domain();
  mig.GetCommRel(MeshObj::NODE).delete_domain();
  
  /*
   * Now the shared and owned attributes may not be correct
   * for the mesh.  Building the comm rel's below will mark
   * the shared, so, for now, mark everyone as !SHARED,
   * and set the correct owned id.
   */
  resolve_rebalance_ownership(mesh);
 

  mesh.remove_unused_kernels();

  // Deal with the interior edge problem: We may need to delete
  // the interior edges on some processors.

  // Last step!  Rebuild symmetric comms from the ownership
  // we have given the objects.
  mesh.build_sym_comm_rel(MeshObj::NODE);
  mesh.build_sym_comm_rel(MeshObj::EDGE);
  mesh.build_sym_comm_rel(MeshObj::FACE);
  
  mesh.remove_unused_kernels();

  mesh.CompactData();

  return true;
}

/*--------------------------------------------------------*/
// Rebalance mesh
/*--------------------------------------------------------*/
bool Rebalance(Mesh &mesh) {
  Trace __trace("Rebalance(Mesh &mesh)");
  
  CommReg mig("_rebalance_migration", mesh, mesh);

  ThrowRequire(mesh.is_committed());

  if (!form_rebalance_comm(mesh, mig)) {
    //std::cout << "No rebalance!!";
    return false;
  }

  return Rebalance(mesh, mig);

}

static void add_obj_children(MeshObj &obj, std::vector<CommRel::CommNode> &cnodes, UInt P) {
  MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();
  for (; ri != re; ++ri) {
    if (ri->type == MeshObj::CHILD && ri->obj->get_type() == obj.get_type()) {
      CommRel::CommNode tnode(ri->obj, P);
      std::vector<CommRel::CommNode>::iterator lb =
        std::lower_bound(cnodes.begin(), cnodes.end(), tnode);
      if (lb == cnodes.end() || *lb != tnode) {
        cnodes.insert(lb, tnode);
//Par::Out() << "obj " << obj.get_id() << ", Adding child " << MeshObjTypeString(obj.get_type()) << " id " << ri->obj->get_id() << 
// "to proc:" << P << std::endl;
      }
    }
  }
}

static void build_obj_migration_recursive(MeshObj &obj, std::vector<CommRel::CommNode> &cnodes, UInt P, UInt obj_type) {

  ThrowRequire(obj.get_type() == MeshObj::ELEMENT);

  // Special case: if obj_type = element, then just add the element
  if (obj_type == MeshObj::ELEMENT) {
    CommRel::CommNode tnode(&obj, P);
    std::vector<CommRel::CommNode>::iterator lb =
      std::lower_bound(cnodes.begin(), cnodes.end(), tnode);
    if (lb == cnodes.end() || *lb != tnode)
      cnodes.insert(lb, tnode);
    // still recurse to children, below
  }

  // Assign all uses of the object and child (think child node of element)
  MeshObjRelationList::iterator ri = obj.Relations.begin(), re = obj.Relations.end();
  for (; ri != re; ++ri) {
    if ((ri->type == MeshObj::USES || ri->type == MeshObj::CHILD)
           && ri->obj->get_type() == obj_type) {
      CommRel::CommNode tnode(ri->obj, P);
      std::vector<CommRel::CommNode>::iterator lb =
        std::lower_bound(cnodes.begin(), cnodes.end(), tnode);
      if (lb == cnodes.end() || *lb != tnode) {
        cnodes.insert(lb, tnode);
        // Also add the objects children, since they may not have a USES
        // from an element
      }
      // Not sure why, but this must be outside
      add_obj_children(*ri->obj, cnodes, P);
    }
   
    // recurse on element children
    if (ri->type == MeshObj::CHILD && ri->obj->get_type() == MeshObj::ELEMENT)
      build_obj_migration_recursive(*ri->obj, cnodes, P, obj_type);
  }
}

static void build_obj_migration(Mesh &mesh, CommReg &migration, UInt obj_type) {

  CommRel &erel = migration.GetCommRel(MeshObj::ELEMENT);
  CommRel &crel = migration.GetCommRel(obj_type);
  std::vector<CommRel::CommNode> cnodes;

  CommRel::MapType::iterator oi = erel.domain_begin(), oe = erel.domain_end();
  for (; oi != oe; ++oi) {
    MeshObj &elem = *oi->obj;

    // Add objects from this element
    build_obj_migration_recursive(elem, cnodes, oi->processor, obj_type);
  }

  // Add objects to spec.  When appending to the element spec, this
  // call just stuffs the new elements on the end of the spec, = ok.
  crel.add_domain(cnodes);
  
}

static void recursive_elem_owner(MeshObj &elem, UInt owner) {

  // Set self owner
  elem.set_owner(owner);

  // And now all chilren.
  MeshObjRelationList::iterator ci = elem.Relations.begin(),
       ce = elem.Relations.end();

  for (; ci != ce; ++ci) {
    if (ci->type == MeshObj::CHILD &&
          ci->obj->get_type() == MeshObj::ELEMENT)
    {
      recursive_elem_owner(*ci->obj, owner);
    }
  }
}

/*--------------------------------------------------------*/
// Figure out who the future owners of the elems are.
// Simply assign the elems moving (and all children) to
// proc assigned.
/*--------------------------------------------------------*/
void set_new_elem_owners(Mesh &mesh, CommReg &mig) {
  CommRel &erel = mig.GetCommRel(MeshObj::ELEMENT);

  CommRel::MapType::iterator oi = erel.domain_begin(), 
           oe = erel.domain_end();

  for (; oi != oe; ++oi) {
    recursive_elem_owner(*oi->obj, oi->processor);
  }
}

/*--------------------------------------------------------*/
// Figure out who the future owners of the nodes are.
// We simply traverse all element attached to the node,
// and assign the node to the lowest processor number that
// will have the nodes.
/*--------------------------------------------------------*/
static void set_new_obj_owners(Mesh &mesh, CommReg &mig, UInt obj_type) {
  Trace __trace("set_new_obj_owners(Mesh &mesh, CommReg &mig, UInt obj_type)");
  // First, just assign by looping the local elements and
  // picking out the smallest proc.
  {
    Mesh::iterator nb = mesh.obj_begin_all(obj_type), ne = mesh.obj_end_all(obj_type);
    for (; nb != ne; ++nb) {
      MeshObj &node = *nb;

      // In case object is not used by an element, it will
      // pick up an owner from where it is.
      node.set_owner(std::numeric_limits<int>::max());
      
      // Loop elements
      MeshObjRelationList::iterator ei , ee = node.Relations.end();
      // Seek to elements
      ei = MeshObjConn::find_relation(node, MeshObj::ELEMENT);
      node.set_owner(std::numeric_limits<UInt>::max());
      bool found_elem = false;
      for (; ei != ee && ei->obj->get_type() == MeshObj::ELEMENT && ei->type == MeshObj::USED_BY; ++ei) {
        found_elem = true;
        const int proc = ei->obj->get_owner();
        if ((UInt) proc < node.get_owner()) node.set_owner(proc);
      }
      
    }
  } // node owners

  // Now do a parallel reduction to get the minimum
  CommRel &nrel = mesh.GetCommRel(obj_type);
  
  ActField<OwnerAction> af("ownerfield");
  ActField<OwnerAction> *afp = &af;
  nrel.swap_op<UInt,ActField<OwnerAction> >(1, &afp, CommRel::OP_MIN);

}


/*--------------------------------------------------------*/
// Zoltan based stuff

typedef std::vector<MeshObj*> MeshObjVect;

struct zoltan_user_data {
  Mesh *mesh;
  MeshObjVect gen_elem;
};

static void build_lists(Mesh &mesh, MeshObjVect &gen_elem) {

  gen_elem.clear();

  // Loop and add the genesis elements
  KernelList::iterator si = mesh.set_begin(), se = mesh.set_end();

  for (; si != se; ++si) {

    if (si->type() == MeshObj::ELEMENT && si->is_genesis()) {
      Kernel::obj_iterator oi = si->obj_begin(), oe = si->obj_end();

      for (; oi != oe; ++oi) {
        gen_elem.push_back(&*oi);
//Par::Out() << "Adding gen_elem:" << oi->get_id() << std::endl;
      }

    }
  }
}

static int num_children(MeshObj &obj) {

  if (GetMeshObjContext(obj).is_set(Attr::ACTIVE_ID))
    return 1;

  MeshObjRelationList::iterator ci = obj.Relations.begin(), ce = obj.Relations.end();

  int num = 0;

  for (; ci != ce; ++ci) {
    if (ci->type == MeshObj::CHILD && ci->obj->get_type() == obj.get_type()) {
      num += num_children(*ci->obj);
    }
  }

  return num;
}


// Zoltan Mesh Functions

static int GetNumAssignedObj(void *user, int *err) {
  zoltan_user_data &udata = *(static_cast<zoltan_user_data*>(user));
  *err = 0;
  return udata.gen_elem.size();
}

static void GetObjList(void *user, int numGlobalIds, int numLids, ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids,
          int wgt_dim, float *obj_wghts, int *err) 
{
  zoltan_user_data &udata = *(static_cast<zoltan_user_data*>(user));

  ThrowRequire(wgt_dim == 1);

  UInt i = 0;
  MeshObjVect::iterator ni = udata.gen_elem.begin(), ne = udata.gen_elem.end();
  for (; ni != ne; ++ni) {
    gids[i] = (*ni)->get_id();
    lids[i] = i;
    obj_wghts[i] = num_children(**ni);
//Par::Out() << "elem " << (*ni)->get_id() << ", weight " << obj_wghts[i] << std::endl;
    i++;
  }

  *err = 0;
}

static int GetNumGeom(void *user, int *err) {
  zoltan_user_data &udata = *(static_cast<zoltan_user_data*>(user));
  *err = 0;

  return udata.mesh->spatial_dim();
}

static void GetObject(void *user, int numGlobalIds, int numLids, int numObjs,
  ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids, int numDim, double *pts, int *err) 
{
  zoltan_user_data &udata = *(static_cast<zoltan_user_data*>(user));
  MEField<> *coord_field = udata.mesh->GetCoordField();
  *err = 0;

  for (UInt i = 0; i < (UInt) numObjs; i++) {
    UInt idx = lids[i];

    int ind = i*numDim;

    std::vector<double> ndata(numDim);
    double *c;
    MeshObj *elemp = udata.gen_elem[idx];
    elemCentroid(*coord_field, *elemp, &ndata[0]);
    c = &ndata[0];

    // Rotate the points ever so slightly to avoid a line of centers falling
    // along a coordinate axis (causes Zoltan issues).

    // Sin/cos of rotation angle.
    static const double sa = 0.0001;
    static const double ca = std::sqrt(1 - sa*sa);
    static const double t = (1.0-ca);

    // Rotate around (1,1,1)
    // http://www.fastgraph.com/makegames/3Drotation/
    static const double len = 1.0/std::sqrt(3.0);
    static const double onethird = 1.0/3.0;
    static const double m1 = onethird*t + ca;
    static const double m2 = onethird*t - len*sa;
    static const double m3 = onethird*t + len*sa;

    if (numDim == 3) {
      pts[ind]   = m1*c[0] + m2*c[1] + m3*c[2];
      pts[ind+1] = m3*c[0] + m1*c[1] + m2*c[2];
      pts[ind+2] = m2*c[0] + m3*c[1] + m1*c[2];
    } else if (numDim == 2) {
      pts[ind] = ca*c[0] - sa*c[1];
      pts[ind+1] = sa*c[0] + ca*c[1];
    }
  }
}


/*--------------------------------------------------------*/
// Rebalance the mesh.  Create the required migration and
// sym specs.
/*--------------------------------------------------------*/
static bool form_rebalance_comm(Mesh &mesh, CommReg &migration) {

  float ver;
  int rc = Zoltan_Initialize(0, NULL, &ver);

  static struct Zoltan_Struct *zz = NULL;
  int rank = Par::Rank(); 

  if (zz == NULL) {
    zz = Zoltan_Create(Par::Comm());
  
    Zoltan_Set_Param(zz, "DEBUG_LEVEL", "0");
    Zoltan_Set_Param(zz, "LB_METHOD", "RCB");
    Zoltan_Set_Param(zz, "NUM_GID_ENTRIES", "1");
    Zoltan_Set_Param(zz, "NUM_LID_ENTRIES", "1");
    Zoltan_Set_Param(zz, "RETURN_LISTS", "ALL");
    Zoltan_Set_Param(zz, "AVERAGE_CUTS", "1");
    Zoltan_Set_Param(zz, "OBJ_WEIGHT_DIM", "1");
  
     
    // RCB
    //Zoltan_Set_Param(zz, "RCB_RECTILINEAR_BLOCKS", "1");
    Zoltan_Set_Param(zz, "KEEP_CUTS", "1");
    //Zoltan_Set_Param(zz, "RCB_LOCK_DIRECTIONS", "1");
    Zoltan_Set_Param(zz, "RCB_REUSE", "1");
    Zoltan_Set_Param(zz, "RCB_OUTPUT_LEVEL", "0");
  }

  int changes;
  int numGidEntries;
  int numLidEntries;
  int numImport;
  ZOLTAN_ID_PTR importGlobalGids;
  ZOLTAN_ID_PTR importLocalGids;
  int *importProcs;
  int *importToPart;
  int numExport;
  ZOLTAN_ID_PTR exportGlobalGids;
  ZOLTAN_ID_PTR exportLocalGids;
  int *exportProcs;
  int *exportToPart;


  zoltan_user_data zud;
  // Build the two lists of nodes that are in the box
  zud.mesh = &mesh;
  build_lists(mesh, zud.gen_elem);

  // Callback parameters
  Zoltan_Set_Num_Obj_Fn(zz, GetNumAssignedObj, (void*) &zud);
  Zoltan_Set_Obj_List_Fn(zz, GetObjList, (void*) &zud);
  Zoltan_Set_Num_Geom_Fn(zz, GetNumGeom, (void*) &zud);
  Zoltan_Set_Geom_Multi_Fn(zz, GetObject, (void*) &zud);


  rc = Zoltan_LB_Partition(zz, &changes, &numGidEntries, &numLidEntries,
    &numImport, &importGlobalGids, &importLocalGids, &importProcs, &importToPart,
    &numExport, &exportGlobalGids, &exportLocalGids, &exportProcs, &exportToPart);


//std::cout << "P:" << rank << ", numIMp:" << numImport << ", numExport:" << numExport << std::endl;

  // Figure out the migration CommSpec.  Just put the genesis
  // elements in, and where they go.
  if (changes != 0 && numExport != 0) {

    CommRel &erel = migration.GetCommRel(MeshObj::ELEMENT);

    std::vector<CommRel::CommNode> enodes;

    for (UInt i = 0; i < (UInt) numExport; i++) {
      enodes.push_back(CommRel::CommNode(zud.gen_elem[exportLocalGids[i]], exportProcs[i]));
    }

    erel.add_domain(enodes);

  } // build migration comm

  Zoltan_LB_Free_Part(&importGlobalGids, &importLocalGids,
                      &importProcs, &importToPart);
  Zoltan_LB_Free_Part(&exportGlobalGids, &exportLocalGids,
                      &exportProcs, &exportToPart);

 // Zoltan_Destroy(&zz);

  return changes != 0;
}

} // namespace
