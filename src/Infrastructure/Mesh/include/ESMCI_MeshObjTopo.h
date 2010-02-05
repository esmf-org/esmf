//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MeshObjTopo_h
#define ESMCI_MeshObjTopo_h

#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_Exception.h>


#include <string>
#include <vector>

namespace ESMCI {

// Get the topo instance
class MeshObjTopo;
MeshObjTopo *GetTopo(const std::string &type);

// Get a topo from a marshalled int.
MeshObjTopo *GetTopo(UInt);
MeshObjTopo *ManufactureTopo(const std::string &type);

// Return a lower order topology; linear from quadratic
MeshObjTopo *LowerTopo(const MeshObjTopo &topo);

/**
 * Switch a shell topo for its pdim=sdim version
 */
const MeshObjTopo *FlattenTopo(const MeshObjTopo &topo);

/**
 * The basic connection pattern within a mesh database.  Describes
 * how a high level topological object (face, cell) relates to other
 * objects in the mesh.
 * 
 * <ul>
 * 
 * <li> Hexahedron topology:
 * @verbatim
 *                       18
 *            7 o--------*--------o 6
 *             /|                /|
 *          19* |            17 * |
 *           /  |              /  |
 *         4/   |  16         /   |
 *         o--------*--------o 5  * 14
 *         |    *15          |    |
 *         |    |            |    |
 *         |    |            |    |
 *         |    | 3     10   |13  |  
 *      12 *    o--------*---*----o 2 
 *         |   /             |   / 
 *         |11*              |  *9  
 *         | /               | /
 *         |/                |/    
 *         o-------*---------o
 *         0       8         1
 * 
 * Nodes 20-26 are in the center and center of faces:
 * 
 * 
 *                 o22
 *                 |  o26
 *                 | /
 *        23       |/
 *         o-------o-------o24
 *                /| 20
 *               / |
 *              o  |
 *             25  o21
 *            
 * @endverbatim
 * 
 * <li> Quadratic topology:
 * @verbatim
 * 
 * Nodes (*= child node)
 *                       
 *       3    6     2
 *       o-----*-----o
 *       |           |    
 *       |           |       
 *     7 *    8*     * 5      
 *       |           |        
 *       |           |    
 *       o-----*-----o
 *       0     4     1
 *                     
 * @endverbatim
 *   
 * <li> Triangle topology:
 * @verbatim
 * Nodes:(*=child node)
 *              2
 *              o
 *             / \
 *            /   \      
 *           /     \         
 *        5 *       * 4      
 *         /         \       
 *        /           \      
 *       /             \
 *      o-------*-------o
 *      0       3       1 
 * @endverbatim
 * 
 * <li> Tetrahedron topology:
 * @verbatim
 *         
 *            o 3
 *           /|\
 *        7 * | * 9
 *         /  |6 \
 *      o o- -*- -o 2
 *         \  |8 /
 *        4 * | * 5
 *           \|/
 *            o
 *            1
 *@endverbatim
 *
 * <li> Bar topology:
 * @verbatim
 * 
 *   o-----*-----o 
 *   0     2     1
 * 
 * @endverbatim
 * </ul>           
 * 
 * Parametric coordinates and parametric coordinate mappings are
 * also included in this object.
 *
 * This object can be modified to incorporate no homogenous topologies,
 * but this will require modifying some assumptions, namely that sides
 * have the same number of nodes, edges, etc...
 * 
 * @ingroup meshdatabase
 * 
 */
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
  spatial_dim(a_spatial_dim), parametric_dim(a_parametric_dim),
  face_edge_map(0), face_edge_pol(0) {}

 /**
  * Return the map for how the side topology's nodes map to the element
  * nodes.
  */
 const int *get_side_nodes(int side) const {return &side_node_map[num_side_child_nodes*side];}

 /**
  * Return a map relating how edge nodes map to the element node numbering.
  */
 const int *get_edge_nodes(int edge) const {return &edge_node_map[num_edge_child_nodes*edge];}

 /**
  * A map from face edge to element edges.
  */
 const int *get_face_edge(int face_num) const {
  ThrowAssert(face_edge_map);
   return &face_edge_map[side_topo_list[0]->num_edges*face_num]; 
 }

 /**
  * A face's edges may not have the same orientation as the element.
  * If the polarity is the same, a 1, else 0.
  */
 const int *get_face_edge_pol(int face_num) const {
  ThrowAssert(face_edge_pol);
   return &face_edge_pol[side_topo_list[0]->num_edges*face_num]; 
 }

 const std::string name;

 /** Unique topology index for marshalling the topology */
 const global_identifier number; 

 /** Number of vertex nodes */
 const UInt num_vertices;

 /** Number of nodes >= vertex nodes, for instance, with quadratic topologies. */
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

 /**
  * Return parametric coordinates of the object's nodes.
  */
 const double *node_coord() const {
   return node_coords;
 }

 int num_edges;
 int num_side_nodes;
 int num_side_child_nodes; 
 int num_edge_nodes;
 int num_edge_child_nodes;
 int num_child_nodes;
 const int *perm_table(UInt rotation, UInt p) const { return &ptable[(2*rotation + (1-p))*num_child_nodes];}

 private:

 /** Side and edge topology lists */
 std::vector<MeshObjTopo*> side_topo_list;
 std::vector<MeshObjTopo*> edge_topo_list;
 
 /** How do side nodes (in their ordering) match element nodes? */
 int *side_node_map;

 /** How do edge nodes (in their ordering) match element nodes? */
 int *edge_node_map;

 /** How do a face's edges match element's edges? */
 int *face_edge_map;

 /** Does the face polarity match the element polarity? */
 int *face_edge_pol;

 /** Permutation tables for side topology */
 int *ptable;

 /** Array of nodal parametric coordinates */
 double *node_coords;
};


} // namespace


#endif
