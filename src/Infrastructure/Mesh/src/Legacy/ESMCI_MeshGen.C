// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Legacy/ESMCI_MeshGen.h>

#include <Mesh/include/ESMCI_Mesh.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjTopo.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_IOField.h>
#include <Mesh/include/Legacy/ESMCI_MeshSkin.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

static void gen2d(Mesh &mesh, const MeshObjTopo *topo) {
  Trace __trace("gen2d(Mesh &mesh, const MeshObjTopo *topo");

  mesh.set_spatial_dimension(topo->spatial_dim);
  mesh.set_parametric_dimension(topo->parametric_dim);
  UInt sdim = mesh.spatial_dim();

  // This is not so elegant, really, but it is the most straightforward
  // approach.  Just switch on the number of nodes for the various types.
  switch (topo->num_nodes) {
    case 4:
    // quadrilateral
    {
      std::vector<MeshObj*> node(4, (MeshObj*) 0);


      if (Par::Rank() == 0) {
        // Create the nodes
        node[0] = new MeshObj(MeshObj::NODE, 1, 0);
        node[1] = new MeshObj(MeshObj::NODE, 2, 1);
        node[2] = new MeshObj(MeshObj::NODE, 3, 2);
        node[3] = new MeshObj(MeshObj::NODE, 4, 3);
  
        mesh.add_node(node[0], 0);
        mesh.add_node(node[1], 0);
        mesh.add_node(node[2], 0);
        mesh.add_node(node[3], 0);
  
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
      IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", sdim);

      if (Par::Rank() == 0) {
        double *c = node_coord->data(*node[0]);
        c[0] = 0; c[1] = 0; if (sdim == 3) c[2] = 0;
        c = node_coord->data(*node[1]);
        c[0] = 1; c[1] = 0; if (sdim == 3) c[2] = 0;
        c = node_coord->data(*node[2]);
        c[0] = 1; c[1] = 1; if (sdim == 3) c[2] = 0;
        c = node_coord->data(*node[3]);
        c[0] = 0; c[1] = 1; if (sdim == 3) c[2] = 0;
      }
    
    }
    break;

    case 3:
    // Triangle
    {
      std::vector<MeshObj*> node(4, (MeshObj*) 0);

      if (Par::Rank() == 0) {
        // Create the nodes
        node[0] = new MeshObj(MeshObj::NODE, 1, 0);
        node[1] = new MeshObj(MeshObj::NODE, 2, 1);
        node[2] = new MeshObj(MeshObj::NODE, 3, 2);
        node[3] = new MeshObj(MeshObj::NODE, 4, 3);
  
        mesh.add_node(node[0], 0);
        mesh.add_node(node[1], 0);
        mesh.add_node(node[2], 0);
        mesh.add_node(node[3], 0);
  
        // Set as locally owned
        node[0]->set_owner(0);
        node[1]->set_owner(0);
        node[2]->set_owner(0);
        node[3]->set_owner(0);
  
        // Create the elements
        std::vector<MeshObj*> tnode(3,static_cast<MeshObj*>(0));

        MeshObj *elem = new MeshObj(MeshObj::ELEMENT, 1, 0);
        MeshObj *elem1 = new MeshObj(MeshObj::ELEMENT, 2, 1);

        // First triangle
        tnode[0] = node[0]; tnode[1] = node[1]; tnode[2] = node[2];
        mesh.add_element(elem, tnode, 1, topo);

        // Second triangle
        tnode[0] = node[3]; tnode[1] = node[2]; tnode[2] = node[0];
        mesh.add_element(elem1, tnode, 1, topo);
  
      }

      // Set up the coordinates to match the unit cube.
      IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", mesh.spatial_dim());

      if (Par::Rank() == 0) {
        double *c = node_coord->data(*node[0]);
        c[0] = 0; c[1] = 0; if (sdim == 3) c[2] = 0;
        c = node_coord->data(*node[1]);
        c[0] = 1; c[1] = 0; if (sdim == 3) c[2] = 0;
        c = node_coord->data(*node[2]);
        c[0] = 1; c[1] = 1; if (sdim == 3) c[2] = 0;
        c = node_coord->data(*node[3]);
        c[0] = 0; c[1] = 1; if (sdim == 3) c[2] = 0;
      }
    
    }
    break;

    default:
      Throw() << "2d topo " << topo->name << " not yet supported!";
  }
}

static void gen3d(Mesh &mesh, const MeshObjTopo *topo) {
  Trace __trace("gen3d(Mesh &mesh, const MeshObjTopo *topo");

  mesh.set_spatial_dimension(topo->spatial_dim);
  mesh.set_parametric_dimension(topo->parametric_dim);
  UInt sdim = mesh.spatial_dim();

  switch (topo->num_nodes) {
    case 8:
    // Hexahedron
    {
      std::vector<MeshObj*> node(8, (MeshObj*) 0);

      if (Par::Rank() == 0) {
        // Create the nodes
        node[0] = new MeshObj(MeshObj::NODE, 1, 0);
        node[1] = new MeshObj(MeshObj::NODE, 2, 1);
        node[2] = new MeshObj(MeshObj::NODE, 3, 2);
        node[3] = new MeshObj(MeshObj::NODE, 4, 3);
        node[4] = new MeshObj(MeshObj::NODE, 5, 4);
        node[5] = new MeshObj(MeshObj::NODE, 6, 5);
        node[6] = new MeshObj(MeshObj::NODE, 7, 6);
        node[7] = new MeshObj(MeshObj::NODE, 8, 7);
  
        mesh.add_node(node[0], 0);
        mesh.add_node(node[1], 0);
        mesh.add_node(node[2], 0);
        mesh.add_node(node[3], 0);
        mesh.add_node(node[4], 0);
        mesh.add_node(node[5], 0);
        mesh.add_node(node[6], 0);
        mesh.add_node(node[7], 0);
  
        // Set as locally owned
        node[0]->set_owner(0);
        node[1]->set_owner(0);
        node[2]->set_owner(0);
        node[3]->set_owner(0);
        node[4]->set_owner(0);
        node[5]->set_owner(0);
        node[6]->set_owner(0);
        node[7]->set_owner(0);
  
        // Create the element
        MeshObj *elem = new MeshObj(MeshObj::ELEMENT, 1, 0);
  
        mesh.add_element(elem, node, 1, topo);
      }

      // Set up the coordinates to match the unit cube.
      IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", sdim);

      if (Par::Rank() == 0) {
        double *c = node_coord->data(*node[0]);
        c[0] = 0; c[1] = 0; c[2] = 0;
        c = node_coord->data(*node[1]);
        c[0] = 1; c[1] = 0; c[2] = 0;
        c = node_coord->data(*node[2]);
        c[0] = 1; c[1] = 1; c[2] = 0;
        c = node_coord->data(*node[3]);
        c[0] = 0; c[1] = 1; c[2] = 0;

        c = node_coord->data(*node[4]);
        c[0] = 0; c[1] = 0; c[2] = 1;
        c = node_coord->data(*node[5]);
        c[0] = 1; c[1] = 0; c[2] = 1;
        c = node_coord->data(*node[6]);
        c[0] = 1; c[1] = 1; c[2] = 1;
        c = node_coord->data(*node[7]);
        c[0] = 0; c[1] = 1; c[2] = 1;
      }
    
    }
    break;

    case 4:
    // tetrahedron
    {
      std::vector<MeshObj*> node(9, (MeshObj*) 0);

      if (Par::Rank() == 0) {
        // Create the nodes
        node[0] = new MeshObj(MeshObj::NODE, 1, 0);
        node[1] = new MeshObj(MeshObj::NODE, 2, 1);
        node[2] = new MeshObj(MeshObj::NODE, 3, 2);
        node[3] = new MeshObj(MeshObj::NODE, 4, 3);
        node[4] = new MeshObj(MeshObj::NODE, 5, 4);
        node[5] = new MeshObj(MeshObj::NODE, 6, 5);
        node[6] = new MeshObj(MeshObj::NODE, 7, 6);
        node[7] = new MeshObj(MeshObj::NODE, 8, 7);
        node[8] = new MeshObj(MeshObj::NODE, 9, 8);
  
        mesh.add_node(node[0], 0);
        mesh.add_node(node[1], 0);
        mesh.add_node(node[2], 0);
        mesh.add_node(node[3], 0);
        mesh.add_node(node[4], 0);
        mesh.add_node(node[5], 0);
        mesh.add_node(node[6], 0);
        mesh.add_node(node[7], 0);
        mesh.add_node(node[8], 0);
  
        // Set as locally owned
        node[0]->set_owner(0);
        node[1]->set_owner(0);
        node[2]->set_owner(0);
        node[3]->set_owner(0);
        node[4]->set_owner(0);
        node[5]->set_owner(0);
        node[6]->set_owner(0);
        node[7]->set_owner(0);
        node[8]->set_owner(0);
  
        // Create the elements
        std::vector<MeshObj*> tnode(4, static_cast<MeshObj*>(0));

        MeshObj *elem[12];
        for (UInt i = 0; i < 12; i++)
          elem[i] = new MeshObj(MeshObj::ELEMENT, i+1, i);

        // Connection array (one based due to export from mesher)
        int connect[12][4] = {
           {1, 2, 3, 4},
           {2, 4, 1, 5},
           {6, 4, 1, 3},
           {7, 8, 1, 5},
           {6, 8, 1, 7},
           {1, 3, 2, 8},
           {2, 1, 8, 5},
           {5, 4, 1, 9},
           {5, 7, 9, 1},
           {9, 4, 1, 6},
           {1, 7, 9, 6},
           {6, 1, 8, 3}
        };
  
        for (UInt i = 0; i < 12; i++) {
          tnode[0] = node[connect[i][0]-1];
          tnode[1] = node[connect[i][1]-1];
          tnode[2] = node[connect[i][2]-1];
          tnode[3] = node[connect[i][3]-1];
          mesh.add_element(elem[i], tnode, i+1, topo);
        }
      }

      // Set up the coordinates to match the unit cube.
      IOField<NodalField> *node_coord = mesh.RegisterNodalField(mesh, "coordinates", sdim);

      if (Par::Rank() == 0) {
        double *c = node_coord->data(*node[0]);
        c[0] = 0.5528; c[1] = 0.4196; c[2] = 0.5529;
        c = node_coord->data(*node[1]);
        c[0] = 0; c[1] = 0; c[2] = 0;
        c = node_coord->data(*node[2]);
        c[0] = 1; c[1] = 0; c[2] = 1;
        c = node_coord->data(*node[3]);
        c[0] = 0; c[1] = 0; c[2] = 1;

        c = node_coord->data(*node[4]);
        c[0] = 0; c[1] = 1; c[2] = 0;
        c = node_coord->data(*node[5]);
        c[0] = 1; c[1] = 1; c[2] = 1;
        c = node_coord->data(*node[6]);
        c[0] = 1; c[1] = 1; c[2] = 0;
        c = node_coord->data(*node[7]);
        c[0] = 1; c[1] = 0; c[2] = 0;
        c = node_coord->data(*node[8]);
        c[0] = 0; c[1] = 1; c[2] = 1;
      }
    
    }
    break;

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
  
  Skin(mesh);
}

void Cart2D(Mesh &mesh, const int X, const int Y,
                        const double xA, const double xB,
                        const double yA, const double yB) {

    ThrowRequire(X>1&&Y>1);

    const MeshObjTopo *topo = GetTopo("QUAD");
    mesh.set_spatial_dimension(topo->spatial_dim);
    mesh.set_parametric_dimension(topo->parametric_dim);

    std::vector<MeshObj*> node(X*Y, (MeshObj*) 0);
    std::vector<MeshObj*> nodes(4, (MeshObj*) 0);

    for (UInt i=0; i<X*Y; ++i) {
      node[i] = new MeshObj(MeshObj::NODE, i+1, i);
      mesh.add_node(node[i], 0);
      node[i]->set_owner(0);
    }

    int ind = 0;
    for(UInt j=0; j<Y-1; ++j) {
    for(UInt i=0; i<X-1; ++i) {
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, ind+1, ind);
      nodes[0] = node[i+1+(j*X)];
      nodes[1] = node[i+(j*X)];
      nodes[2] = node[i+((j+1)*X)];
      nodes[3] = node[i+1+((j+1)*X)];
      mesh.add_element(elem, nodes, 0, topo);
      ++ind;
      }
    }

    // Set up the coordinates to match the unit cube.
    IOField<NodalField> *node_coord = mesh.RegisterNodalField(
      mesh, "coordinates", mesh.spatial_dim());

    const double hx = std::abs(xB-xA)/double(X-1);
    const double hy = std::abs(yB-yA)/double(Y-1);
    for (UInt i=0; i<X*Y; ++i) {
      double *c = node_coord->data(*node[i]);
      c[0] = xA+double(i%X)*hx; c[1] = yA+double(std::floor(double(i)/X))*hy;
    }
}

void SphShell(Mesh &mesh, const int lat, const int lon,
                        const double latA, const double latB,
                        const double lonA, const double lonB) {

    ThrowRequire(lat>1&&lon>1);

    const MeshObjTopo *topo = GetTopo("SHELL");
    mesh.set_spatial_dimension(3);
    mesh.set_parametric_dimension(2);

    std::vector<MeshObj*> node(lat*lon, (MeshObj*) 0);
    std::vector<MeshObj*> nodes(4, (MeshObj*) 0);

    for (UInt i=0; i<lat*lon; ++i) {
      node[i] = new MeshObj(MeshObj::NODE, i+1, i);
      mesh.add_node(node[i], 0);
      node[i]->set_owner(0);
    }

    int ind = 0;
    for(UInt j=0; j<lon-1; ++j) {
    for(UInt i=0; i<lat-1; ++i) {
      MeshObj *elem = new MeshObj(MeshObj::ELEMENT, ind+1, ind);
      nodes[0] = node[i+1+(j*lat)];
      nodes[1] = node[i+(j*lat)];
      nodes[2] = node[i+((j+1)*lat)];
      nodes[3] = node[i+1+((j+1)*lat)];
      mesh.add_element(elem, nodes, 0, topo);
      ++ind;
      }
    }

    // Set up the coordinates to match the unit cube.
    IOField<NodalField> *node_coord = mesh.RegisterNodalField(
      mesh, "coordinates", mesh.spatial_dim());

    double latAt = latA;
    //double latAt = latA - 3.14159/2;
    double latBt = latB;
    //double latBt = latB - 3.14159/2;

    const double hLat = std::abs(latBt-latAt)/double(lat-1);
    const double hLon = std::abs(lonB-lonA)/double(lon-1);

    for (UInt i=0; i<lat*lon; ++i) {
      double *c = node_coord->data(*node[i]);
      double phi = latAt+double(i%lat)*hLat;
      double theta = lonA+double(std::floor(double(i)/double(lat)))*hLon;

      c[0] = std::cos(theta)*std::sin(phi);
      c[1] = std::sin(theta)*std::sin(phi);
      c[2] = std::cos(phi);
/*
std::cout<<c[0]<<"  "<<c[1]<<"  "<<c[2]
                            <<"  theta = "<<theta<<"  phi = "<<phi<<std::endl;
*/
    }
}

} // namespace
