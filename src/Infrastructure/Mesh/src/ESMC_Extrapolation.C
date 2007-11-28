// $Id: ESMC_Extrapolation.C,v 1.2 2007/11/28 16:42:40 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_Extrapolation.h>

#include <ESMC_Interp.h>
#include <ESMC_Mesh.h>
#include <ESMC_ParEnv.h>

namespace ESMC {

void MeshAddNorthPole(Mesh &mesh, const Context &elem_ctxt,
                  UInt constraint_id, double z_north, IWeights &cweights)
{
  
  ThrowRequire(mesh.is_committed());
  
  // First collect all the boundary nodes;
  std::set<MeshObj*> bnodes;
  
  // Loop kernels, get objects
  // get the shared objs
  KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
  
  for (; ki != ke; ++ki) {
    
    Kernel &ker = *ki;
    
    // select all objs of this type and not the newly created ones (which have bogus ids)
    if (ker.type() == MeshObj::NODE && ker.GetContext().is_set(Attr::EXPOSED_BOUNDARY_ID)) {
      
      Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
      
      for (; oi != oe; ++oi)

        bnodes.insert(&*oi);
      
    } // kernel matches
    
  } // for ki

  
  // Now loop the nodes, get the lat/lon coords and determine if the nodes are on the 
  // northern boundary or not.
  std::set<MeshObj*>::iterator bi = bnodes.begin(), be = bnodes.end();
  
  std::set<MeshObj*> fbnodes; // keep final nodes so we can remove boundary context, later
  
  MEField<> &coords = *mesh.GetCoordField();
  
  ThrowRequire(coords.dim() == 3); 
  
  std::set<MeshObj*> elems; // find the attached elements at the same time
  
  for (; bi != be; ++bi) {
    
    MeshObj &node = **bi;
    
    double *coord = coords.data(node);
    
    if (coord[2] > z_north) {
      
      fbnodes.insert(&node);
      
//std::cout << "Found boundary north node:" << node << std::endl;
      
      // add the elements attached to the 'ring'
      MeshObjRelationList::iterator ri = node.Relations.begin(),
                re = node.Relations.end();
      
      for (; ri != re; ++ri) {
        
        if (ri->obj->get_type() == MeshObj::ELEMENT && ri->type == MeshObj::USED_BY)
          elems.insert(ri->obj);
          
      }
      
    } // a poleward node
    
  } // for potential poleward nodes
  
  // Now verify that we only have elements on a single processor.  Otherwise we would
  // need to write code here to collect these elements on a single proc.
  int has_elems = elems.size() > 0 ? 1 : 0;
  int total_procs;
  
  MPI_Reduce(&has_elems, &total_procs, 1, MPI_INT, MPI_SUM, 0, Par::Comm()); 
  
  if (Par::Rank() == 0 && total_procs != 1) {
    Throw() << "Error; either more than one or no processor"
      "has elems to mesh from: total_procs=" << total_procs << std::endl;
  }
  
  if (has_elems) {
    
    std::set<MeshObj*>().swap(bnodes); // free bnodes memory
    
    // Now triangulate the pole;
    
    // 1) Create the pole node
    MeshObj::id_type pole_id = mesh.get_max_id(MeshObj::NODE) + 1;
    MeshObj *pnode = new MeshObj(MeshObj::NODE, pole_id, 0, Par::Rank());
    
    Context ctxt;
    ctxt.set(constraint_id);
    
    Attr a(MeshObj::NODE,
        1,  // nodeset
        ctxt, // context
        false,  // shared
        true, // locally owned
        true, // active
        false);  // genesis
        
    mesh.add_node(pnode,a);
    
    /* Now add the triangles;
     * 1) Get the elements that surround the pole 'gap'.
     * 2) Get the two nodes on that element that are on the gap.
     * 3) Create a triangle that uses the two nodes and the pole.
     */ 
    
    // Loop the elements
    std::set<MeshObj*>::iterator ei = elems.begin(), ee = elems.end();
    
    const MeshObjTopo *tri_topo = GetTopo("SHELL3");
    ThrowRequire(tri_topo);
    
    for (; ei != ee; ++ei) {
      
      MeshObj &elem = **ei;
      
      // Get side nodes with poleward boundary context
      const MeshObjTopo *etopo = GetMeshObjTopo(elem);
      
      // TODO: Make work for quadratic quadrilaterals
      ThrowRequire(etopo->num_nodes == 4);
      
      int pole_side = -1;
      
      for (UInt s = 0; pole_side < 0 && s < etopo->num_sides; s++) {
        
        const int *side_nodes = etopo->get_side_nodes(s);
        
        // Check; if this is the side, all nodes on side should have the boundary context;
        bool is_pole_side = true;
        for (UInt sn = 0; is_pole_side && sn < (UInt) etopo->num_side_nodes; sn++) {
          
          is_pole_side = GetMeshObjContext(*elem.Relations[side_nodes[sn]].obj).is_set(Attr::EXPOSED_BOUNDARY_ID);
          
        }
        
        if (is_pole_side) pole_side = s; 
        
      } // for s
      
      ThrowRequire(pole_side >= 0); // need to have found a side
      
      const int *side_nodes = etopo->get_side_nodes(pole_side);
      
      // Create the triangle
      MeshObj::id_type tri_id = mesh.get_max_id(MeshObj::ELEMENT) + 1;
      MeshObj *tri = new MeshObj(MeshObj::ELEMENT, tri_id, 0, Par::Rank());
      
      std::vector<MeshObj*> tri_nodes;
  
      tri_nodes.push_back(pnode);
      tri_nodes.push_back(elem.Relations[side_nodes[0]].obj);
      tri_nodes.push_back(elem.Relations[side_nodes[1]].obj);
      
      // TODO: possibly add quadratic nodes??
  
      mesh.add_element(tri, tri_nodes, 2, tri_topo);
  
      // Download the field contexts to this object
      tri->GetKernel()->Imprint(*tri);
      
    } // for ei
   
    std::set<MeshObj*>::iterator bf_i = fbnodes.begin(), bf_e = fbnodes.end();
    
    for (; bf_i != bf_e; ++bf_i) {
      
      MeshObj &node = **bf_i;
      
      // Remove boundary context from node
      const Attr &oattr = GetAttr(node);
      const Context &ctxt = GetMeshObjContext(node);
      Context newctxt(ctxt);
      newctxt.clear(Attr::EXPOSED_BOUNDARY_ID);
      Attr attr(oattr, newctxt);
      mesh.update_obj(&node, attr);
      
    }
    
    
    // Set the coordinates of the pole. ASSUM: pole at (0,0,1)
    double *pole_coord = coords.data(*pnode);
    
    ThrowRequire(pole_coord); // better be data there!!!
    
    pole_coord[0] = pole_coord[1] = 0.0;
    
    pole_coord[2] = 1.0; 
    
    // Put together the constraint
    double val = 1.0 / fbnodes.size();
    
    // TODO: how many indices do I need to constrain?? For now 3,
    // since I am using this for a vector.
    for (UInt r = 0; r < 3; r++) {
      
      IWeights::Entry row(pnode->get_id(), r);
      
      std::vector<IWeights::Entry> col;
      
      bf_i = fbnodes.begin();
      
      for (; bf_i != fbnodes.end(); ++bf_i) {
      
        MeshObj &node = **bf_i;
        
        col.push_back(IWeights::Entry(node.get_id(), r, val));
        
      }
      
      cweights.InsertRow(row, col);
      
    } // r
    
    mesh.remove_unused_kernels();
    
  } // has elems
}


} // namespace
