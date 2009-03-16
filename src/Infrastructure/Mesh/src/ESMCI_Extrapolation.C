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
#include <Mesh/include/ESMCI_Extrapolation.h>

#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_Rebalance.h>
#include <Mesh/include/ESMCI_GlobalIds.h>

#include <set>

namespace ESMCI {

/*
void MeshAddPole(Mesh &mesh, UInt node_id, UInt constraint_id, IWeights &cweights) {


  std::set<MeshObj*> elements, nodes;

  // Loop the nodes with given node_id
  KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
  
  for (; ki != ke; ++ki) {
    
    Kernel &ker = *ki;

    bool lowned = ker.is_owned();

    if (ker.type() == MeshObj::NODE && ker.key() == node_id) {

      Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
      
      for (; oi != oe; ++oi) {

        MeshObj &node = *oi;

        Par::Out() << "found node:" << node.get_id() << ", node_id=" << node_id << std::endl;

        if (lowned) nodes.insert(&node);

        // Push all connected elements onto list
        MeshObjRelationList::iterator ei = node.Relations.begin(), ee = node.Relations.end();
        for (; ei != ee; ++ei) {

          if (ei->type == MeshObj::USED_BY && ei->obj->get_type() == MeshObj::ELEMENT) {
            elements.insert(ei->obj);
          }

        }


      } // oi

    } 

  } // ki

  // Find how many nodes globally.  If zero, we are done
  int nfound = nodes.size();
  int gnfound;

  MPI_Allreduce(&nfound, &gnfound, 1, MPI_INT, MPI_SUM, Par::Comm());

  if (gnfound == 0) return;


  UInt nelem = elements.size();


  MEField<> &coord = *mesh.GetCoordField();


  // Otherwise we mesh.

  // Step 1: Figure out the coordinates of the new node.

  UInt nsend = nodes.size();
  double ncoord[3] = {0,0,0};
  double gncoord[3] = {0,0,0};

  // Sum the local coords
  std::set<MeshObj*>::iterator si = nodes.begin(), se = nodes.end();

  for (; si != se; ++si) {

    double *c = coord.data(**si);
    ThrowRequire(c);

    ncoord[0] += c[0]; ncoord[1] += c[1]; ncoord[2] += c[2];

  }

  MPI_Allreduce(&ncoord[0], &gncoord[0], 3, MPI_DOUBLE, MPI_SUM, Par::Comm());

  gncoord[0] /= nfound;
  gncoord[1] /= nfound;
  gncoord[2] /= nfound;

  if (nsend > 0) {

    // Move coords onto the spehere
    double r = std::sqrt(gncoord[0]*gncoord[0] +
                 gncoord[1]*gncoord[1] +
                 gncoord[2]*gncoord[2]);

    gncoord[0] *= r;
    gncoord[1] *= r;
    gncoord[2] *= r;

  Par::Out() << "Have new mesh coords!!" << gncoord[0] << ", " << gncoord[1] << ", " << gncoord[2] << std::endl;

    // Create the pole node on every proc that is involved
    MeshObj::id_type pole_id = mesh.get_max_id(MeshObj::NODE) + 1;
    MeshObj *pnode = new MeshObj(MeshObj::NODE, pole_id, 0);
    
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
   
  }


}
*/

void MeshAddPole(Mesh &mesh, UInt node_id,
                  UInt constraint_id, IWeights &cweights)
{

  UInt rank = Par::Rank();

  {

  // First: Get the elements around the pole and ship them to processor zero
  std::set<MeshObj*> elements;

  // Loop the nodes with given node_id
  KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
  
  for (; ki != ke; ++ki) {
    
    Kernel &ker = *ki;

    bool lowned = ker.is_owned();

    if (ker.type() == MeshObj::NODE && ker.key() == node_id) {

      Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
      
      for (; oi != oe; ++oi) {

        MeshObj &node = *oi;

        // Push all connected elements onto list
        MeshObjRelationList::iterator ei = node.Relations.begin(), ee = node.Relations.end();
        for (; ei != ee; ++ei) {

          if (ei->type == MeshObj::USED_BY && ei->obj->get_type() == MeshObj::ELEMENT) {
            elements.insert(ei->obj);
          }

        }


      } // oi

    } 

  } // ki

  // If there are no elements, go no further
  {
    int nfound = elements.size();
    int gnfound;

    MPI_Allreduce(&nfound, &gnfound, 1, MPI_INT, MPI_SUM, Par::Comm());

    if (gnfound == 0) return;
  }

  // Form the migration comm to send elements to proc 0
  CommReg mig("_rebalance_migration", mesh, mesh);



  if (rank != 0) // don't ship elements on zero to zero (causes trouble)
  {

    CommRel &erel = mig.GetCommRel(MeshObj::ELEMENT);

    std::set<MeshObj*>::iterator ei = elements.begin(), ee = elements.end();

    std::vector<CommRel::CommNode> enodes;

    for (; ei != ee; ++ei) {

      enodes.push_back(CommRel::CommNode(*ei, 0));  

    }

    erel.add_domain(enodes);

  }

  // Now ship them back to proc zero
  Rebalance(mesh, mig);

  } // done with rebalance

  // So now to the job at hand of meshing on proc zero

  std::set<MeshObj*> elems, nodes;
  if (rank == 0) {
    // Gather all the nodes and elements

    // Loop the nodes with given node_id
    KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
  
    for (; ki != ke; ++ki) {
    
      Kernel &ker = *ki;


      if (ker.type() == MeshObj::NODE && ker.key() == node_id) {

        Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
      
        for (; oi != oe; ++oi) {

          MeshObj &node = *oi;

          nodes.insert(&node);

          // Push all connected elements onto list
          MeshObjRelationList::iterator ei = node.Relations.begin(), ee = node.Relations.end();
          for (; ei != ee; ++ei) {

            if (ei->type == MeshObj::USED_BY && ei->obj->get_type() == MeshObj::ELEMENT) {
              elems.insert(ei->obj);
            }

          }


        } // oi

      } 

    } // ki

  } // rank = 0

  // We need to get ids for the pole node and for the new triangles
  std::vector<long> new_ids;
  long pole_id;
  {
    std::vector<long> cur_ids;

    // nodes
    Mesh::iterator ni = mesh.node_begin(), ne = mesh.node_end();
    for (; ni != ne; ++ni) cur_ids.push_back(ni->get_id());

    new_ids.resize(1, 0);

    GlobalIds(cur_ids, new_ids);

    pole_id = new_ids[0];

    new_ids.clear();
    cur_ids.clear();

    Mesh::iterator ei = mesh.elem_begin(), ee = mesh.elem_end();
    for (; ei != ee; ++ei) cur_ids.push_back(ei->get_id());

    new_ids.resize(elems.size(), 0);

    GlobalIds(cur_ids, new_ids);

  }

  MEField<> &coords = *mesh.GetCoordField();

  if (rank == 0) {
    
    // 1) Create the pole node
    MeshObj *pnode = new MeshObj(MeshObj::NODE, pole_id, 0, rank);
      
    Context ctxt;
    ctxt.set(constraint_id);
      
    Attr a(MeshObj::NODE,
        0,  // nodeset
        ctxt, // context
        false,  // shared
        true, // locally owned
        true, // active
        false);  // genesis
          
    mesh.add_node(pnode,a);
      
    // Now add the triangles;
     // 1) Get the elements that surround the pole 'gap'.
     // 2) Get the two nodes on that element that are on the gap.
     // 3) Create a triangle that uses the two nodes and the pole.
       
      
    // Loop the elements
    std::set<MeshObj*>::iterator ei = elems.begin(), ee = elems.end();
      
    const MeshObjTopo *tri_topo = GetTopo("SHELL3");
    ThrowRequire(tri_topo);
      
    UInt tri_num = 0; // count ids used
    for (; ei != ee; ++ei) {
        
      MeshObj &elem = **ei;
        
      // Get side nodes with poleward boundary
      const MeshObjTopo *etopo = GetMeshObjTopo(elem);
        
      ThrowRequire(etopo->num_nodes == 4);
        
      int pole_side = -1;
        
      for (UInt s = 0; pole_side < 0 && s < etopo->num_sides; s++) {
          
        const int *side_nodes = etopo->get_side_nodes(s);
          
        // Check; if this is the side, all nodes on side should have the boundary context;
        bool is_pole_side = true;
        for (UInt sn = 0; is_pole_side && sn < (UInt) etopo->num_side_nodes; sn++) {
            
          is_pole_side = elem.Relations[side_nodes[sn]].obj->GetKernel()->key() == node_id;
            
        }
          
        if (is_pole_side) pole_side = s; 
          
      } // for s
        
      ThrowRequire(pole_side >= 0); // need to have found a side
        
      const int *side_nodes = etopo->get_side_nodes(pole_side);
        
      // Create the triangle
      ThrowRequire(tri_num < new_ids.size());
      MeshObj::id_type tri_id = new_ids[tri_num++];

      MeshObj *tri = new MeshObj(MeshObj::ELEMENT, tri_id, 0, rank);
        
      std::vector<MeshObj*> tri_nodes;

      // Order of triangles is important so normals point in correct direction    
      tri_nodes.push_back(elem.Relations[side_nodes[1]].obj);
      tri_nodes.push_back(elem.Relations[side_nodes[0]].obj);
      tri_nodes.push_back(pnode);

        
      mesh.add_element(tri, tri_nodes, 2, tri_topo);
    
      // Download the field contexts to this object
      tri->GetKernel()->Imprint(*tri);
        
    } // for ei
     
    std::set<MeshObj*>::iterator bf_i = nodes.begin(), bf_e = nodes.end();
      
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

    double new_coords[3] = {0, 0, 0};

    std::set<MeshObj*>::iterator ni = nodes.begin(), ne = nodes.end();

    for (; ni != ne; ++ni) {

      double *c = coords.data(**ni);

      new_coords[0] += c[0]; new_coords[1] += c[1]; new_coords[2] += c[2];

    }

    UInt nfound = nodes.size();

    new_coords[0] /= nfound;
    new_coords[1] /= nfound;
    new_coords[2] /= nfound;

    // Move coords onto the spehere
    double rr = 1.0/std::sqrt(new_coords[0]*new_coords[0] +
                              new_coords[1]*new_coords[1] +
                              new_coords[2]*new_coords[2]);

    new_coords[0] *= rr;
    new_coords[1] *= rr;
    new_coords[2] *= rr;

    pole_coord[0] = new_coords[0];
    pole_coord[1] = new_coords[1];
    pole_coord[2] = new_coords[2];
      
    // Put together the constraint
    double val = 1.0 / nfound;
      
        
    IWeights::Entry row(pnode->get_id(), 0);
        
    std::vector<IWeights::Entry> col;
        
    bf_i = nodes.begin();
        
    for (; bf_i != nodes.end(); ++bf_i) {
        
      MeshObj &node = **bf_i;
          
      col.push_back(IWeights::Entry(node.get_id(), 0, val));
          
    }
        
    cweights.InsertRow(row, col);
        
    mesh.remove_unused_kernels();
      
  } // rank zero

}

/*

void MeshAddNorthPole(Mesh &mesh, UInt node_id,
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
    
    // Now add the triangles;
     // 1) Get the elements that surround the pole 'gap'.
     // 2) Get the two nodes on that element that are on the gap.
     // 3) Create a triangle that uses the two nodes and the pole.
     
    
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

*/




} // namespace
