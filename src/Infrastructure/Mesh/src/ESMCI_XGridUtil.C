// $Id: ESMCI_XGridUtil.C,v 1.2 2011/05/04 19:04:52 feiliu Exp $
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
#include <Mesh/include/ESMCI_Search.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_MEValues.h>
#include <Mesh/include/ESMCI_PatchRecovery.h>
#include <Mesh/include/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_CommRel.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_Migrator.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_ConserveInterp.h>
#include <Mesh/include/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_MeshVTK.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include <Mesh/include/ESMCI_MeshRegrid.h>


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
  std::stable_sort(sintd_nodes.begin(), sintd_nodes.end(), sintd_node_less());
  { // make sure the genesis cells in the duplicates are pointing 
    // to the retained node. The two loops take linear time.
    std::vector<sintd_node *>::iterator it = sintd_nodes.begin();
    while(it != sintd_nodes.end()){

      std::vector<sintd_node *>::iterator itt = it+1;
      while(itt != sintd_nodes.end() && **itt == **it){
        (*itt)->get_cell()->replace_node(*it);
        itt++;
      }

      it = itt;
    }
  }
  sintd_nodes.erase(std::unique(sintd_nodes.begin(), sintd_nodes.end(), sintd_node_equal()), sintd_nodes.end());

  Mesh & meshmid = *midmesh;
  meshmid.set_parametric_dimension(pdim);
  meshmid.set_spatial_dimension(sdim);
  int rc;
  int me = VM::getCurrent(&rc)->getLocalPet();

  // Add nodes to mesh for each intersection point
  std::vector<MeshObj *> node_list;
  int num_int = sintd_nodes.size();
  node_list.resize(num_int);

  for (int i=0; i<num_int; i++) {
    // Create new node in mesh object       
    MeshObj *node = new MeshObj(MeshObj::NODE,i,i);
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
  int num_cells = sintd_cells.size();
  std::vector<MeshObj *> elem_list;
  elem_list.resize(num_cells);
  for (int i=0; i<num_cells; i++) {
    // only add those that are cells, these should all be valid cells coming out of the valid check
    int num_edges = sintd_cells[i]->num_edges();
    if(num_edges < 3) continue;
    MeshObj *cell = new MeshObj(MeshObj::ELEMENT, i, i);    // Mesh equivalent of Cell

    // Set Owner
    cell->set_owner(me);
    std::vector<MeshObj *> nodes;
    for(int in = 0; in < num_edges; in ++)
      nodes.push_back(sintd_cells[i]->operator[](in)->get_node());

    meshmid.add_element(cell, nodes, 1, sintd_cells[i]->get_topo());
    elem_list[i] = cell;
  }

  // set up the fraction Field
  // Intersection cells should not be masked, fraction should all be 1.0
  Context ctxt; ctxt.flip();
  MEField<> *elem_frac = meshmid.RegisterField("elem_frac",
                     MEFamilyDG0::instance(), MeshObj::ELEMENT, ctxt, 1, true);

  // Can also attach elem_area here, an optimization

  // Finalize mesh
  meshmid.build_sym_comm_rel(MeshObj::NODE);

  meshmid.Commit();

  elem_frac = meshmid.GetField("elem_frac");
  for (int i=0; i<num_cells; i++) {
    double *frac = elem_frac->data(*(elem_list[i]));
    *frac = 1.0;
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

void compute_sintd_nodes_cells(int num_sintd_nodes, double * sintd_coords, int sdim, 
  std::vector<sintd_node *> * sintd_nodes, std::vector<sintd_cell *> * sintd_cells){

  // bubble up the nodes and cells
  // cross reference the nodes and cells
  std::vector<sintd_node *> cell_nodes;
  for(int in = 0; in < num_sintd_nodes; in ++){
    sintd_node * node = new sintd_node(sdim, sintd_coords+sdim*in);
    sintd_nodes->push_back(node);
    cell_nodes.push_back(node);
  }

  // every cell keeps track of the nodes enclosing it
  sintd_cell * cell = new sintd_cell(cell_nodes);
  sintd_cells->push_back(cell);

  // every node associated with this genesis cell refers to it
  for(int in = 0; in < num_sintd_nodes; in ++)
    (*(cell_nodes[in])).set_cell(cell);
}

// A different path for online regrid
int online_regrid_xgrid(Mesh &srcmesh, Mesh &dstmesh, Mesh * midmesh, IWeights &wts,
                  int *regridConserve, int *regridMethod,
                  int *unmappedaction) {

  // Conservative regridding
  // pole type and points, and scheme are not needed for cons. regridding
  // This is the current layer cut off subroutine
  // Talk to Bob to streamline regrid as well to bubble up area/centroid info
  int regridPoleType = -999;
  int regridPoleNPnts = -999;
  int regridScheme = 1;
  if (!regrid(srcmesh, dstmesh, midmesh, wts, regridMethod, &regridScheme, 
            &regridPoleType, &regridPoleNPnts, unmappedaction))
    Throw() << "Regridding error" << std::endl;

  return 1;
}

} //namespace
