// $Id: ESMC_MeshGen.C,v 1.1 2007/08/20 19:46:10 dneckels Exp $
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
#include <ESMC_MeshGen.h>

#include <ESMC_Mesh.h>
#include <ESMC_MeshObj.h>
#include <ESMC_MeshObjTopo.h>
#include <ESMC_Exception.h>
#include <ESMC_ParEnv.h>
#include <ESMC_IOField.h>

namespace ESMCI {
namespace MESH {

static void gen2d(Mesh &mesh, const MeshObjTopo *topo) {
  Trace __trace("gen2d(Mesh &mesh, const MeshObjTopo *topo");

  switch (topo->num_nodes) {
    case 4:
    // quadrilateral
    {
      std::vector<MeshObj*> node(4, (MeshObj*) 0);

      mesh.set_parametric_dimension(2);
      mesh.set_spatial_dimension(2);

      if (Par::Rank() == 0) {
        // Create the nodes
        node[0] = new MeshObj(MeshObj::NODE, 1, 0);
        node[1] = new MeshObj(MeshObj::NODE, 2, 1);
        node[2] = new MeshObj(MeshObj::NODE, 3, 2);
        node[3] = new MeshObj(MeshObj::NODE, 4, 3);
  
        mesh.add_node(node[0], 1);
        mesh.add_node(node[1], 1);
        mesh.add_node(node[2], 1);
        mesh.add_node(node[3], 1);
  
        // Set as locally owned
        node[0]->set_owner(0);
        node[1]->set_owner(0);
        node[2]->set_owner(0);
        node[3]->set_owner(0);
  
        // Create the element
        MeshObj *elem = new MeshObj(MeshObj::ELEMENT, 1, 0);
  
        mesh.add_element(elem, node, 1, topo);
      }

      // Set up the coordinates to match the unit cube.
      IOField<NodalField> *node_coord = mesh.RegisterNodalField("coordinates", mesh.spatial_dim());

      if (Par::Rank() == 0) {
        double *c = node_coord->data(*node[0]);
        c[0] = 0; c[1] = 0;
        c = node_coord->data(*node[1]);
        c[0] = 1; c[1] = 0;
        c = node_coord->data(*node[2]);
        c[0] = 1; c[1] = 1;
        c = node_coord->data(*node[3]);
        c[0] = 0; c[1] = 1;
      }
    
    }
    break;

    default:
      Throw() << "2d topo " << topo->name << " not yet supported!";
  }
}

static void gen3d(Mesh &mesh, const MeshObjTopo *topo) {
  Trace __trace("gen3d(Mesh &mesh, const MeshObjTopo *topo");

  switch (topo->num_nodes) {

    default:
      Throw() << "3d topo " << topo->name << " not yet supported!";
  }
}

void HyperCube(Mesh &mesh, const MeshObjTopo *topo) {
  Trace __trace("HyperCube(Mesh &mesh, const MeshObjTopo *topo)");

  // Break into cases
  switch (topo->parametric_dim) {
    case 2:
      gen2d(mesh, topo);
    break;

    case 3:
      gen3d(mesh, topo);
    break;

    default:
      Throw() << "Unsupported dimension " << mesh.parametric_dim();
  }
}

} // namespace
} // namespace
