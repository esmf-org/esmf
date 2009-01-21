// $Id: ESMC_MeshObjTopo.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_MeshObjTopo_h
#define ESMC_MeshObjTopo_h

#include <ESMC_MeshTypes.h>


#include <string>
#include <vector>

namespace ESMCI {
namespace MESH {

// Get the topo instance
class MeshObjTopo;
MeshObjTopo *GetTopo(const std::string &type);

// Get a topo from a marshalled int.
MeshObjTopo *GetTopo(UInt);
MeshObjTopo *ManufactureTopo(const std::string &type);

// Return a lower order topology; linear from quadratic
MeshObjTopo *LowerTopo(const MeshObjTopo &topo);

class MeshObjTopo {
public:
 typedef UInt global_identifier;
 friend MeshObjTopo *GetTopo(const std::string&);
 friend MeshObjTopo *ManufactureTopo(const std::string&);
 friend void AddHomoSide(MeshObjTopo *topo, std::string side_name);
 friend void AddHomoEdge(MeshObjTopo *topo, std::string edge_name);

 MeshObjTopo(const std::string &a_name,
             global_identifier a_number,
             int a_num_vertices,
             int a_num_nodes,
             int a_num_sides,
             int a_spatial_dim,
             int a_parametric_dim) :
  name(a_name), number(a_number), num_vertices(a_num_vertices), num_nodes(a_num_nodes), num_sides(a_num_sides),
  spatial_dim(a_spatial_dim), parametric_dim(a_parametric_dim) {}
 const int *get_side_nodes(int side) const {return &side_node_map[num_side_child_nodes*side];}
 const int *get_edge_nodes(int edge) const {return &edge_node_map[num_edge_child_nodes*edge];}
 const std::string name;
 const global_identifier number;  // for marshalling
 const UInt num_vertices;
 const UInt num_nodes;
 const UInt num_sides;
 const UInt spatial_dim;
 const UInt parametric_dim;
 UInt get_num_side_nodes() const {return num_side_nodes;}
 UInt get_num_edge_nodes() const {return num_edge_nodes;}
 const MeshObjTopo *side_topo(UInt ordinal) const {
   return side_topo_list[ordinal];
 }
 const MeshObjTopo *edge_topo(UInt ordinal) const {
   return edge_topo_list[ordinal];
 }
 int num_edges;
 int num_side_nodes;
 int num_side_child_nodes; 
 int num_edge_nodes;
 int num_edge_child_nodes;
 int num_child_nodes;
 const int *perm_table(UInt rotation, UInt p) const { return &ptable[(2*rotation + (1-p))*num_child_nodes];}
 private:
 std::vector<MeshObjTopo*> side_topo_list;
 std::vector<MeshObjTopo*> edge_topo_list;
 int *side_node_map;
 int *edge_node_map;
 int *ptable;
};


} // namespace
} // namespace


#endif
