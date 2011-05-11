// $Id: ESMCI_XGridUtil.C,v 1.4 2011/05/11 17:49:02 feiliu Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MEValues.h>
#include <Mesh/include/ESMCI_PatchRecovery.h>
#include <Mesh/include/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_CommRel.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_ConserveInterp.h>
#include <Mesh/include/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_MeshVTK.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <Mesh/include/ESMCI_MeshRegrid.h>
#include <Mesh/include/ESMCI_MathUtil.h>

#include <cmath>
#include <algorithm>

#include <ESMCI_VM.h>
#include "ESMCI_Macros.h"

namespace ESMCI{

void compute_midmesh(std::vector<sintd_node *> & sintd_nodes, std::vector<sintd_cell *> & sintd_cells, int pdim, int sdim, Mesh *midmesh){

  // Debug
  //std::vector<sintd_node *>::iterator ib = sintd_nodes.begin();
  //std::vector<sintd_node *>::iterator ie = sintd_nodes.end();
  //
  //for(; ib != ie; ib++){
  //  *ib->print();
  //}

  // collect all the unique intersection points
  std::vector<sintd_node *> tmpnodes;
  std::stable_sort(sintd_nodes.begin(), sintd_nodes.end(), sintd_node_less());
  { // make sure the genesis cells in the duplicates are pointing 
    // to the retained node. The two loops take linear time.
    std::vector<sintd_node *>::iterator it = sintd_nodes.begin();
    while(it != sintd_nodes.end()){

      tmpnodes.push_back(*it);
      std::vector<sintd_node *>::iterator itt = it+1;
      while(itt != sintd_nodes.end() && **itt == **it){
        (*itt)->get_cell()->replace_node(*it);
        // delete the node pointed to; reset the pointer value
        delete (*itt); *itt = 0;
        itt++;
      }

      it = itt;
    }
  }
  sintd_nodes.clear(); sintd_nodes.resize(tmpnodes.size());
  std::copy(tmpnodes.begin(), tmpnodes.end(), sintd_nodes.begin());
  //sintd_nodes.erase(std::unique(sintd_nodes.begin(), sintd_nodes.end(), sintd_node_equal()), sintd_nodes.end());

  Mesh & meshmid = *midmesh;
  meshmid.set_parametric_dimension(pdim);
  meshmid.set_spatial_dimension(sdim);
  int rc;
  int me = VM::getCurrent(&rc)->getLocalPet();

  int offset=0;
  { //reduction to compute the global ids
    int nnodes = sintd_nodes.size();
    int lrc = MPI_Scan((void *)&nnodes, (void*)&offset, 1, MPI_INT, MPI_SUM, 
      VM::getCurrent(&rc)->getMpi_c()); 
    if(lrc != 0) Throw() << "MPI_Scan failed to reduce the local offset.\n";
    offset -= nnodes;
  }

  // Add nodes to mesh for each intersection point
  std::vector<MeshObj *> node_list;
  int num_int = sintd_nodes.size();
  node_list.resize(num_int);

  for (int i=0; i<num_int; i++) {
    // Create new node in mesh object       
    MeshObj *node = new MeshObj(MeshObj::NODE,i+offset+1,i);
    node->set_owner(me); 

    // Add to mesh    
    meshmid.add_node(node, 0);

    // Add to local list
    node_list[i]=node;

    // cross reference
    sintd_nodes[i]->set_node(node);
  }

  // Register the nodal coordinate field.
  IOField<NodalField> *node_coord = meshmid.RegisterNodalField(meshmid, "coordinates", meshmid.spatial_dim());
  
  // Add coordinates to Nodes
  for (int i=0; i<num_int; i++) {
    double *c = node_coord->data(*(node_list[i]));

    for (int d=0; d<sdim; d++) {
      c[d]=sintd_nodes[i]->operator[](d);
    }
  }

  // collect all the unique intersection cells ? shouldn't need to do this
  //std::stable_sort(sintd_cells.begin(), sintd_cells.end());
  //sintd_cells.erase(unique(sintd_cells.begin(), sintd_cells.end()), sintd_cells.end());
  // Add Elements
  offset = 0;
  { //reduction to compute the global ids
    int ncells = sintd_cells.size();
    int lrc = MPI_Scan((void *)&ncells, (void*)&offset, 1, MPI_INT, MPI_SUM, 
      VM::getCurrent(&rc)->getMpi_c()); 
    if(lrc != 0) Throw() << "MPI_Scan failed to reduce the local offset.\n";
    offset -= ncells;
  }
  int num_cells = sintd_cells.size();
  std::vector<MeshObj *> elem_list;
  elem_list.resize(num_cells);
  for (int i=0; i<num_cells; i++) {
    // dump Cells
    // sintd_cells[i]->print(me, i+offset+1, i);

    // only add those that are cells, these should all be valid cells coming out of the valid check
    int num_edges = sintd_cells[i]->num_edges();
    if(num_edges < 3) continue;
    MeshObj *cell = new MeshObj(MeshObj::ELEMENT, i+offset+1, i);    // Mesh equivalent of Cell

    // Set Owner
    cell->set_owner(me);
    std::vector<MeshObj *> nodes;
    for(int in = 0; in < num_edges; in ++)
      nodes.push_back(sintd_cells[i]->operator[](in)->get_node());

    meshmid.add_element(cell, nodes, 1, sintd_cells[i]->get_topo(sdim, pdim));
    elem_list[i] = cell;
  }

  // set up the fraction Field
  // Intersection cells should not be masked, fraction should all be 1.0
  Context ctxt; ctxt.flip();
  MEField<> *elem_frac = meshmid.RegisterField("elem_frac",
                     MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);
  // Can also attach elem_area here, an optimization
  //MEField<> *elem_area = meshmid.RegisterField("elem_area",
  //                   MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

  // Finalize mesh
  meshmid.build_sym_comm_rel(MeshObj::NODE);
  //meshmid.build_sym_comm_rel(MeshObj::ELEMENT);

  meshmid.Commit();

  elem_frac = meshmid.GetField("elem_frac");
  //elem_area = meshmid.GetField("elem_area");
  for (int i=0; i<num_cells; i++) {
    double *frac = elem_frac->data(*(elem_list[i]));
    *frac = 1.0;
    //double *area = elem_area->data(*(elem_list[i]));
    //*area = sintd_cells[i]->get_area();
  }

  //WriteVTKMesh(meshmid, "midMesh.vtk");
  //WriteVTKMesh(srcmesh, "srcMesh.vtk");
  //WriteVTKMesh(dstmesh, "dstMesh.vtk");

  // free all memory
  std::vector<sintd_node *>::iterator it1=sintd_nodes.begin();
  for(;it1 != sintd_nodes.end(); it1++)
    delete *it1;
  std::vector<sintd_cell *>::iterator it2=sintd_cells.begin();
  for(;it2 != sintd_cells.end(); it2++)
    delete *it2;

}

// Compute cells of middle mesh based on clipping results
void compute_sintd_nodes_cells(double area, int num_sintd_nodes, double * sintd_coords, int pdim, int sdim, 
  std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells, struct Zoltan_Struct * zz){

  // bubble up the nodes and cells
  // cross reference the nodes and cells
  int rc;
  int proc, part;
  int me = VM::getCurrent(&rc)->getLocalPet();
  int npet = VM::getCurrent(&rc)->getNpets();
  
  if(npet != 1){
    // determine if we want to build the nodes/cells on this proc
    int num_owned = 0;
    std::map<int,int> proc_owned;
    for(int in = 0; in < num_sintd_nodes; in ++){
      Zoltan_LB_Point_PP_Assign(zz, sintd_coords+sdim*in, &proc, &part);
      if(proc == me) num_owned ++;
      proc_owned[proc]++; 
    }
    // if none of the nodes is owned by this proc, return
    if(num_owned == 0) return;
    // partially owned
    if(num_owned > 0 && num_owned < num_sintd_nodes){
      std::vector<proc_count> proc_counts;
      std::map<int,int>::const_iterator it=proc_owned.begin(),
        ie=proc_owned.end();
      for(;it != ie; it++) proc_counts.push_back(proc_count(it->first, it->second));
      std::vector<proc_count>::iterator it1 = std::find_if(proc_counts.begin(),
        proc_counts.end(), proc_count_equal(me));
      if(it1 == proc_counts.end()) return; // this shouldn't happen
      int my_count = it1->count;
      std::sort(proc_counts.begin(), proc_counts.end(), proc_count_less());
      // I don't own the most nodes
      if(proc_counts[0].proc != me && proc_counts[0].count > my_count) return;

      // almost there...
      std::vector<sintd_node *> sorted_nodes;
      for(int in = 0; in < num_sintd_nodes; in ++){
        sintd_node * node = new sintd_node(sdim, sintd_coords+sdim*in);
        sorted_nodes.push_back(node);
      }
      std::sort(sorted_nodes.begin(), sorted_nodes.end(), sintd_node_less());
      Zoltan_LB_Point_PP_Assign(zz, sorted_nodes[0]->get_coord(), &proc, &part);
      if(proc != me){
        std::vector<sintd_node *>::iterator it=sorted_nodes.begin();
        for(;it != sorted_nodes.end(); it++)
          delete *it;
        return;
      }
    }
  }

  // Ready to create nodes etc..
  // Break up the cell into triangles if it's got more than 4 nodes
  // For some reason quad_3d can't coexist with tri3_3d
  int break_threshold = 4;
  if(pdim == 2 && sdim == 3) break_threshold = 3;
  if(num_sintd_nodes > break_threshold){

    double * coords = new double[3*sdim];
    for(int i = 0; i < sdim; i ++)
      coords[i] = sintd_coords[i];

    for(int start_node = 1; start_node < num_sintd_nodes-1; start_node ++){
      std::vector<sintd_node *> cell_nodes;
      sintd_node * root_node = new sintd_node(sdim, sintd_coords);
      cell_nodes.push_back(root_node);

      for(int i = 0; i < 2; i ++){
        sintd_node * node = new sintd_node(sdim, sintd_coords+sdim*(i+start_node));
        cell_nodes.push_back(node);
      }
      for(int i = 0; i < 2; i ++)
        for(int j = 0; j < sdim; j ++)
          coords[(i+1)*sdim+j] = cell_nodes[i+1]->operator[](j);

      double split_area;
      if(sdim == 2)
        split_area = area_of_flat_2D_polygon(3, coords);
      if(sdim == 3)
        split_area = great_circle_area(3, coords);
      if(split_area == 0.){
        // release the nodes of this cell and continue split process
        for(int i = 0; i < 3; i ++) delete cell_nodes[i];
        continue;
      }

      // Ready to add the split cell
      for(int i = 0; i < 3; i ++) sintd_nodes->push_back(cell_nodes[i]);

      // every cell keeps track of the nodes enclosing it
      sintd_cell * cell = new sintd_cell(split_area, cell_nodes);
      sintd_cells->push_back(cell);

      for(int in = 0; in < 3; in ++)
        (*(cell_nodes[in])).set_cell(cell);
    }
    delete[] coords;
      
  }else{
    // Ready to create nodes etc in one shot
    std::vector<sintd_node *> cell_nodes;
    for(int in = 0; in < num_sintd_nodes; in ++){
      sintd_node * node = new sintd_node(sdim, sintd_coords+sdim*in);
      sintd_nodes->push_back(node);
      cell_nodes.push_back(node);
    }

    // every cell keeps track of the nodes enclosing it
    sintd_cell * cell = new sintd_cell(area, cell_nodes);
    sintd_cells->push_back(cell);

    // every node associated with this genesis cell refers to it
    for(int in = 0; in < num_sintd_nodes; in ++)
      (*(cell_nodes[in])).set_cell(cell);
  }
}

// A different path for online regrid
int online_regrid_xgrid(Mesh &srcmesh, Mesh &dstmesh, Mesh * midmesh, IWeights &wts,
                  int *regridConserve, int *regridMethod, int *regridScheme,
                  int *unmappedaction) {

  // Conservative regridding
  // pole type and points, and scheme are not needed for cons. regridding
  // This is the current layer cut off subroutine
  int regridPoleType = 0;
  int regridPoleNPnts = 1;
  if (!regrid(srcmesh, dstmesh, midmesh, wts, regridMethod, regridScheme, 
            &regridPoleType, &regridPoleNPnts, unmappedaction))
    Throw() << "Regridding error" << std::endl;

  return 1;
}

} //namespace
