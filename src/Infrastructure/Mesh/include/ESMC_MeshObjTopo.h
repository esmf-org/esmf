// $Id: ESMC_MeshObjTopo.h,v 1.3 2007/11/28 16:43:50 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_MeshObjTopo_h
#define ESMC_MeshObjTopo_h

#include <ESMC_MeshTypes.h>


#include <string>
#include <vector>

namespace ESMC {

// Get the topo instance
class MeshObjTopo;
MeshObjTopo *GetTopo(const std::string &type);

// Get a topo from a marshalled int.
MeshObjTopo *GetTopo(UInt);
MeshObjTopo *ManufactureTopo(const std::string &type);

// Return a lower order topology; linear from quadratic
MeshObjTopo *LowerTopo(const MeshObjTopo &topo);

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
 * </ul>           
 * 
 * Parametric coordinates and parametric coordinate mappings are
 * also included in this object.
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
 std::vector<MeshObjTopo*> side_topo_list;
 std::vector<MeshObjTopo*> edge_topo_list;
 int *side_node_map;
 int *edge_node_map;
 int *ptable;
 double *node_coords;
};


} // namespace


#endif
