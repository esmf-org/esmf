// $Id: ESMC_MeshObjTopo.C,v 1.3.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#include <ESMC_MeshObjTopo.h>
#include <ESMC_Exception.h>

#include <map>

namespace ESMCI {
namespace MESH {


static int hex_sides[] = {
 0, 1, 5, 4,  8, 13, 16, 12, 25,
 1, 2, 6, 5,  9, 14, 17, 13, 24,
 2, 3, 7, 6,  10, 15, 18, 14, 26,
 0, 4, 7, 3,  12, 19, 15, 11, 23,
 0, 3, 2, 1,  11, 10, 9, 8, 21, 
 4, 5, 6, 7,  16, 17, 18, 19, 22
};

static int hex_edges[] = {
 0,1,  8,
 1,2,  9,
 2,3,  10,
 3,0,  11,
 4,5,  16,
 5,6,  17,
 6,7,  18,
 7,4,  19,
 0,4,  12,
 1,5,  13,
 2,6,  14,
 3,7,  15
};

#ifdef FACE_EDGE
static int hex_face_edge[] = {
  0,  9,  4,  8,
  1, 10,  5,  9,
  2, 11,  6, 10,
  8,  7, 11,  3,
  3,  2,  1,  0,
  4,  5,  6,  7
};

static int hex_face_edge_pol[] = {
  1, 1, 0, 0,
  1, 1, 0, 0,
  1, 1, 0, 0,
  1, 0, 0, 1,
  1, 1, 1, 1
};
#endif

static int quad_sides[] = {
 0, 1,  4,
 1, 2,  5,
 2, 3,  6,
 3, 0,  7
};

static int quad_perm[] = {
// rotation 0
0, 1, 2, 3,  4, 5, 6, 7,  8, // pol=true
3, 2, 1, 0,  6, 5, 4, 7,  8, // pol=false
//rotation 1
3, 0, 1, 2,  7, 4, 5, 6,  8, // pol=true
0, 3, 2, 1,  7, 6, 5, 4,  8, // pol=false
//rotation 2
2, 3, 0, 1,  6, 7, 4, 5,  8, // pol=true
1, 0, 3, 2,  4, 7, 6, 5,  8, // pol=false
//rotation 3
1, 2, 3, 0,  5, 6, 7, 4,  8,// pol=true
2, 1, 0, 3,  5, 4, 7, 6,  8
// pol=false
};


static int tri_sides[] = {
0,1,  3,
1,2,  4,
2,0,  5
};

static int tri_perm[] = {
// rotation 0
0, 1, 2,  3, 4, 5, // pol = true
2, 1, 0,  4, 3, 5, // pol = false
// rotation 1
2, 0, 1,  5, 3, 4, // pol=true
0, 2, 1,  5, 4, 3, // pol=false
// rotation 2
1, 2, 0,  4, 5, 3, // pol=true
1, 0, 2,  3, 5, 4 // pol=false
};

static int tet_sides[] = {
 0, 1, 3,   4, 8, 7,
 1, 2, 3,   5, 9, 8,
 0, 3, 2,   7, 9, 6,
 0, 2, 1,   6, 5, 4
};

static int tet_edges[] = {
 0,1,  4,
 1,2,  5,
 2,0,  6,
 0,3,  7,
 1,3,  8,
 2,3,  9
};

#ifdef FACE_EDGE
static int tet_face_edge[] = {
  0, 4, 3,
  1, 5, 4,
  3, 5, 2, 
  2, 1, 0
};

static int tet_face_edge_pol[] = {
  1, 1, 0,
  1, 1, 0,
  1, 0, 1,
  0, 0, 0
};
#endif

static int bar_sides[] = {
 0,
 1
};

static int bar_perm[] = {
0, 1,  2,  // pol = true
1, 0,  2    // pol = false
};

// Add a homogeneous side to a topo
void AddHomoSide(MeshObjTopo *topo, std::string side_name) {
  MeshObjTopo *stopo = GetTopo(side_name);
  if (!stopo)
    Throw() << "'Topo:" << side_name << " invalid";
  topo->side_topo_list.reserve(topo->num_sides);
  for (UInt i = 0; i < topo->num_sides; i++) {
    topo->side_topo_list.push_back(stopo);
  }
}

// Add a homogeneous edge to a topo
void AddHomoEdge(MeshObjTopo *topo, std::string edge_name) {
  MeshObjTopo *etopo = GetTopo(edge_name);
  if (!etopo)
    Throw() << "'Topo:" << edge_name << " invalid";
  topo->side_topo_list.reserve(topo->num_edges);
  for (int i = 0; i < topo->num_edges; i++) {
    topo->edge_topo_list.push_back(etopo);
  }
}

MeshObjTopo *ManufactureTopo(const std::string &name) {
  MeshObjTopo *topo = NULL;
  if (name == "HEX8" || name == "HEX") {
    topo = new MeshObjTopo(name, 1, 8, 8, 6, 3,3);
    topo->side_node_map = hex_sides;
    topo->num_side_nodes = 4;
    topo->num_edge_nodes = 2;
    topo->num_edges = 12;
    topo->num_side_child_nodes = 9;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 27;
    topo->edge_node_map = hex_edges;
    AddHomoSide(topo, "QUAD_3D");
    AddHomoEdge(topo, "BAR2_3D");
    topo->ptable = NULL;
  }
  else if (name == "HEX27") {
    topo = new MeshObjTopo(name, 2, 8, 27, 6, 3,3);
    topo->side_node_map = hex_sides;
    topo->num_side_nodes = 9;
    topo->num_edge_nodes = 3;
    topo->num_edges = 12;
    // For now, don't implement child for hex27
    topo->num_side_child_nodes = 9;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 27;

    topo->edge_node_map = hex_edges;
    AddHomoSide(topo, "QUAD9_3D");
    AddHomoEdge(topo, "BAR3_3D");
    topo->ptable = NULL;
  }
  else if (name == "TETRA4" || name == "TETRA") {
    topo = new MeshObjTopo(name, 3, 4, 4, 4, 3,3);
    topo->side_node_map = tet_sides;
    topo->num_side_nodes = 3;
    topo->num_edge_nodes = 2;
    topo->num_edges = 6;
    topo->num_side_child_nodes = 6;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 10;
    topo->edge_node_map = tet_edges;
    AddHomoSide(topo, "TRI3_3D");
    AddHomoEdge(topo, "BAR2_3D");
    topo->ptable = NULL;
  }
  else if (name == "TETRA10") {
    topo = new MeshObjTopo(name, 4, 4, 10, 4, 3,3);
    topo->side_node_map = tet_sides;
    topo->num_side_nodes = 6;
    topo->num_edge_nodes = 3;
    topo->num_edges = 6;
    // No child for now, tetra10
    topo->num_side_child_nodes = 6;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 10;

    topo->edge_node_map = tet_edges;
    AddHomoSide(topo, "TRI6_3D");
    AddHomoEdge(topo, "BAR3_3D");
    topo->ptable = NULL;
  }
  else if (name == "TRI3") {
    topo = new MeshObjTopo(name, 5, 3, 3, 3, 2,2);
    topo->side_node_map = tri_sides;
    topo->num_side_nodes = 2;
    topo->num_edge_nodes = 2;
    topo->num_edges = 3;
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 6;
    topo->edge_node_map = tri_sides;
    AddHomoSide(topo, "BAR2_2D");
    AddHomoEdge(topo, "BAR2_2D");
    topo->ptable = NULL;
  }
  else if (name == "TRI3_3D") {
    topo = new MeshObjTopo(name, 6, 3, 3, 3, 3,2);
    topo->side_node_map = tri_sides;
    topo->num_side_nodes = 2;
    topo->num_edge_nodes = 2;
    topo->num_edges = 3;
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 6;
    topo->edge_node_map = tri_sides;
    AddHomoSide(topo, "BAR2_3D");
    AddHomoEdge(topo, "BAR2_3D");
    topo->ptable = tri_perm;
  }
  else if (name == "TRI6") {
    topo = new MeshObjTopo(name, 7, 3, 6, 3, 2,2);
    topo->side_node_map = tri_sides;
    topo->num_side_nodes = 3;
    topo->num_edge_nodes = 3;
    topo->num_edges = 3;

    // No child for now, tri6
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 6;

    topo->edge_node_map = tri_sides;
    AddHomoSide(topo, "BAR3_2D");
    AddHomoEdge(topo, "BAR3_2D");
    topo->ptable = NULL;
  }
  else if (name == "TRI6_3D") {
    topo = new MeshObjTopo(name, 8, 3, 6, 3, 3,2);
    topo->side_node_map = tri_sides;
    topo->num_side_nodes = 3;
    topo->num_edge_nodes = 3;
    topo->num_edges = 3;
    // No child for now, tri6
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 6;
    topo->edge_node_map = tri_sides;
    AddHomoSide(topo, "BAR3_3D");
    AddHomoEdge(topo, "BAR3_3D");
    topo->ptable = tri_perm;
  }
  else if (name == "QUAD4" || name == "QUAD") {
    topo = new MeshObjTopo(name, 9, 4, 4, 4, 2,2);
    topo->side_node_map = quad_sides;
    topo->num_side_nodes = 2;
    topo->num_edge_nodes = 2;
    topo->num_edges = 4;
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 9;
    topo->edge_node_map = quad_sides;
    AddHomoSide(topo, "BAR2_2D");
    AddHomoEdge(topo, "BAR2_2D");
    topo->ptable =quad_perm;
  }
  else if (name == "QUAD4_3D" || name == "QUAD_3D") {
    topo = new MeshObjTopo(name, 10, 4, 4, 4, 3,2);
    topo->side_node_map = quad_sides;
    topo->num_side_nodes = 2;
    topo->num_edge_nodes = 2;
    topo->num_edges = 4;
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 9;
    topo->edge_node_map = quad_sides;
    AddHomoSide(topo, "BAR2_3D");
    AddHomoEdge(topo, "BAR2_3D");
    topo->ptable = quad_perm;
  }
  else if (name == "QUAD9") {
    topo = new MeshObjTopo(name, 11, 4, 9, 4, 2,2);
    topo->side_node_map = quad_sides;
    topo->num_side_nodes = 3;
    topo->num_edge_nodes = 3;
    topo->num_edges = 4;
    // No quad9 children, for now
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 9;

    topo->edge_node_map = quad_sides;
    AddHomoSide(topo, "BAR3_2D");
    AddHomoEdge(topo, "BAR3_2D");
    topo->ptable = NULL;
  }
  else if (name == "QUAD9_3D") {
    topo = new MeshObjTopo(name, 12, 4, 9, 4, 3,2);
    topo->side_node_map = quad_sides;
    topo->num_side_nodes = 3;
    topo->num_edge_nodes = 3;
    topo->num_edges = 4;
    // No quad9 children, for now
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 9;
    topo->edge_node_map = quad_sides;
    AddHomoSide(topo, "BAR3_3D");
    AddHomoEdge(topo, "BAR3_3D");
    topo->ptable = quad_perm;
  }
  else if (name == "SHELL" || name == "SHELL4") {
    topo = new MeshObjTopo(name, 13, 4, 4, 4, 3,2);
    topo->side_node_map = quad_sides;
    topo->num_side_nodes = 2;
    topo->num_edge_nodes = 2;
    topo->num_edges = 4;
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 9;
    topo->edge_node_map = quad_sides;
    AddHomoSide(topo, "BAR2_3D");
    AddHomoEdge(topo, "BAR2_3D");
    topo->ptable = quad_perm;
  }
  else if (name == "SHELL9") {
    topo = new MeshObjTopo(name, 14, 4, 9, 4, 3,2);
    topo->side_node_map = quad_sides;
    topo->num_side_nodes = 3;
    topo->num_edge_nodes = 3;
    topo->num_edges = 4;
    /// For now, no children, shell9
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 9;
    topo->edge_node_map = quad_sides;
    AddHomoSide(topo, "BAR3_3D");
    AddHomoEdge(topo, "BAR3_3D");
    topo->ptable = quad_perm;
  }
  else if (name == "SHELL3") {
    topo = new MeshObjTopo(name, 15, 3, 3, 3, 3,2);
    topo->side_node_map = tri_sides;
    topo->num_side_nodes = 2;
    topo->num_edge_nodes = 2;
    topo->num_edges = 3;
    topo->num_side_child_nodes = 3;
    topo->num_edge_child_nodes = 3;
    topo->num_child_nodes = 6;
    topo->edge_node_map = tri_sides;
    AddHomoSide(topo, "BAR2_3D");
    AddHomoEdge(topo, "BAR2_3D");
    topo->ptable = tri_perm;
  }
  else if (name == "BAR2") {
    topo = new MeshObjTopo(name, 16, 2, 2, 2, 1,1);
    topo->side_node_map = bar_sides;
    topo->num_side_nodes = 1;
    topo->num_edge_nodes = 0;
    topo->num_edges = 0;
    topo->num_side_child_nodes = 1;
    topo->num_edge_child_nodes = 1;
    topo->num_child_nodes = 3;
    topo->edge_node_map = NULL;
    topo->ptable = bar_perm;
  }
  else if (name == "BAR2_2D") {
    topo = new MeshObjTopo(name, 17, 2, 2, 2, 2,1);
    topo->side_node_map = bar_sides;
    topo->num_side_nodes = 1;
    topo->num_edge_nodes = 0;
    topo->num_edges = 0;
    topo->num_side_child_nodes = 1;
    topo->num_edge_child_nodes = 1;
    topo->num_child_nodes = 3;
    topo->edge_node_map = NULL;
    topo->ptable = bar_perm;
  }
  else if (name == "BAR2_3D") {
    topo = new MeshObjTopo(name, 18, 2, 2, 2, 3,1);
    topo->side_node_map = bar_sides;
    topo->num_side_nodes = 1;
    topo->num_edge_nodes = 0;
    topo->num_edges = 0;
    topo->num_side_child_nodes = 1;
    topo->num_edge_child_nodes = 1;
    topo->num_child_nodes = 3;
    topo->edge_node_map = NULL;
    topo->ptable = bar_perm;
  }
  else if (name == "BAR3") {
    topo = new MeshObjTopo(name, 19, 2, 3, 2, 1,1);
    topo->side_node_map = bar_sides; // same as bar2
    topo->num_side_nodes = 1;
    topo->num_edge_nodes = 0;
    topo->num_edges = 0;
    // no bar3 child for now
    topo->num_side_child_nodes = 1;
    topo->num_edge_child_nodes = 1;
    topo->num_child_nodes = 3;
    topo->edge_node_map = NULL;
    topo->ptable = NULL;
  }
  else if (name == "BAR3_2D") {
    topo = new MeshObjTopo(name, 20, 2, 3, 2, 2,1);
    topo->side_node_map = bar_sides; // same as bar2
    topo->num_side_nodes = 1;
    topo->num_edge_nodes = 0;
    topo->num_edges = 0;
    // no bar3 child for now
    topo->num_side_child_nodes = 1;
    topo->num_edge_child_nodes = 1;
    topo->num_child_nodes = 3;
    topo->edge_node_map = NULL;
    topo->ptable = bar_perm;
  }
  else if (name == "BAR3_3D") {
    topo = new MeshObjTopo(name, 21, 2, 3, 2, 3,1);
    topo->side_node_map = bar_sides; // same as bar2
    topo->num_side_nodes = 1;
    topo->num_edge_nodes = 0;
    topo->num_edges = 0;
    // no bar3 child for now
    topo->num_side_child_nodes = 1;
    topo->num_edge_child_nodes = 1;
    topo->num_child_nodes = 3;
    topo->edge_node_map = NULL;
    topo->ptable = bar_perm;
  } else Ex() << "Illegal topo type:" << name;

  return topo;
}

MeshObjTopo *GetTopo(const std::string &name) {
  static std::map<std::string, MeshObjTopo*> static_topo_map;

  MeshObjTopo *topo;
  std::map<std::string, MeshObjTopo*>::iterator mit;
  
  mit = static_topo_map.find(name);

  if (mit == static_topo_map.end()) {
    topo = ManufactureTopo(name);
    static_topo_map[name] = topo;
  } else topo = mit->second;

  return topo;
}

MeshObjTopo *GetTopo(UInt t) {

  // This is 1 based in topo, so zero is no topo (nodes)
  static char *tmap[] = {
    "NULL",
    "HEX",
    "HEX27",
    "TETRA",
    "TETRA10",
    "TRI3",
    "TRI3_3D",
    "TRI6",
    "TRI6_3D",
    "QUAD",
    "QUAD_3D",
    "QUAD9",
    "QUAD9_3D",
    "SHELL",
    "SHELL9",
    "SHELL3",
    "BAR2",
    "BAR2_2D",
    "BAR2_3D",
    "BAR3",
    "BAR3_2D",
    "BAR3_3D"
};   

  return GetTopo(tmap[t]);
}

MeshObjTopo *LowerTopo(const MeshObjTopo &topo) {
  const std::string name = topo.name;

  if (name == "HEX27") {
    return GetTopo("HEX8");
  } else if (name == "TETRA10") {
    return GetTopo("TETRA4");
  } else if (name == "QUAD9") {
    return GetTopo("QUAD4");
  } else if (name == "TRI6") {
    return GetTopo("TRI3");
  }

  return NULL;
}

} // namespace
} // namespace
