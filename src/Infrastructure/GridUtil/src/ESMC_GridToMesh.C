// $Id: ESMC_GridToMesh.C,v 1.2 2007/12/18 18:46:29 dneckels Exp $
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
#define ESMC_FILENAME "ESMC_GridToMesh.C"
//==============================================================================
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Create a mesh from a given grid.
//
//-----------------------------------------------------------------------------

// ESMF's fun include strategy requires a specific order to includes for
// compilation to successfully complete.
#include <GridUtil/include/ESMC_GridToMesh.h>
#include <Grid/include/ESMCI_Grid.h>

#include <Mesh/include/ESMC_Mesh.h>
#include <Mesh/include/ESMC_IOField.h>

namespace ESMCI {

// *** Convert a grid to a mesh.  The staggerLoc should describe
// whether the mesh is at the dual location or coincident with the
// grid itself.  I am not sure how this is going to work, though,
// since the dual of a nice multi-patch grid could be a very bad
// object, not representable by a mesh.
//
//   o------------o-------------o
//   |     :      |      :      |
//   |     :      |      :      |
//   |.....x......|......x......|
//   |     :      |      :      |
//   |     :      |      :      |
//   o------------o-------------o
//   |     :      |      :      |
//   |     :      |      :      |
//   |.....x......|......x......|
//   |     :      |      :      |
//   |     :      |      :      |
//   o------------o-------------o
//   
//   E.G. mesh o---o, dual mesh x....x
//
// I think there is work here.  
// For the moment, we should at least be able to represent the grid itself,
// and, maybe, for simple single patch grids with a periodic component,
// the dual, which is not so bad.  This will put us equivalent with
// SCRIP.  
void GridToMesh(const Grid &grid, int staggerLoc, ESMC::Mesh &mesh) {

#ifdef NOT

 try {

 
   // *** Set some meta-data ***

     // We set the topological dimension of the mesh (quad = 2, hex = 3, etc...)
   UInt pdim = grid.GetParametricDim();
   mesh.set_parametric_dimension(pdim);
  
     // In what dimension is the grid embedded?? (sphere = 3, simple rectangle = 2, etc...)
   UInt sdim = grid.GetSpatialDim();
   mesh.set_spatial_dimension(sdim);
  
   // *** Create the Nodes ***
  
     // Loop nodes of the grid.  Here we loop all nodes, both owned and not.
   Grid::iterator ni = grid.node_begin(), ne = grid.node_end();
  
     // We save the nodes in a linear list so that we can access then as such
     // for cell creation.
   std::vector<MeshObj*> nodevect;
  
   
   for (UInt i = 0; ni != ne; ++ni) {
  
     MeshObj *node = new MeshObj(MeshObj::NODE,        // node...
                                 ni->get_global_id(),  // unique global id
                                 i++                   // data index for boostrapping field data
                                 );
  
     node->set_owner(ni->get_owner());  // What processor (uniquely) owns this node?
  
     UInt nodeset = 1;   // Do we need to partition the nodes in any sets?
  
     mesh.add_node(node, nodeset);
  
   } // ni
  
   // *** Create the Cells ***
  
     // Loop the cells
   Grid::iterator ci = grid.cell_begin(), ce = grid.cell_end();
  
   for (; ci != ce; ++ci) {
  
     MeshObj *cell = new MeshObj(MeshObj::ELEMENT,     // Mesh equivalent of Cell
                                 ci->get_global_id(),  // unique global id
                                 i++                   // index for bootstrapping field data
                                 );
  
     cell->set_owner(ci->get_owner());
  
       // Presumably, for a structured grid there is only the
       // 'hypercube' topology, i.e. a quadrilateral for 2d
       // and a hexahedron for 3d
     const MeshObjTopo *ctopo = pdim == 2 ? GetTopo("QUAD") :
                                         (pdim == 3 ? GetTopo("HEX") : 0);
  
     if (!ctopo)
       Throw() << "Could not get topology for pdim=" << pdim;

     // We need to get the nodes which comprise the cell.  One way is to
     // provide the indices that correspond to the declaration order of the
     // nodes, above.  We assume this is the case.

     const int *node_indices = ci->get_node_indices();

     ThrowRequire(ci->get_num_nodes() == ctopo->num_nodes);

     std::vector<MeshObj*> nodes(ctopo->num_nodes);

     for (UInt n = 0; n < ctopo->num_nodes; ++n) {

       nodes[n] = nodevect[node_indices[n]];

     } // n

       // Who owns the cell ? Should be this proc
     cell->set_owner(ci->get_owner());

     UInt block_id = 1;  // Any reason to use different sets for cells?

     mesh.add_element(cell, nodes, block_id, ctopo);
     
   } // ci

     // Now set up the nodal coordinates
   IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", sdim);


   // I am not sure by what strategy the grid will hand over the coords.
   // Here I assume I can call a function for each node.

   MeshDB::iterator ni = mesh.node_begin(), ne = mesh.node_end();

   for (; ni != ne; ++ni) {

     double *c = node_coord->data(*ni);

     UInt idx = ni->get_data_index(); // we set this above when creating the node

     double *gc = grid.GetNodeCoords(idx);
     
     std::copy(gc, gc + sdim, c);

   } // ni


   // ** That's it.  The mesh is now in the pre-commit phase, so other
   // fields can be registered, sides/faces can be turned on, etc, or one
   // can simply call mesh.Commit() and then proceed.
   

 } // try catch block
  catch (std::exception &x) {

  // Set any ESMF error codes/messages

 } // catch
  catch(...) {

  // Set any ESMF error codes/messages

 }

#endif

}

} // namespace

