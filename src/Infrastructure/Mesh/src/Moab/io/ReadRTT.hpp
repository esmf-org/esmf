/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 *
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 */

//-------------------------------------------------------------------------
// Filename      : ReadRTT.hpp
//
// Purpose       : RTT file reader
//
// Creator       : Andrew Davis
//
// Date          : 02/2014
//
//-------------------------------------------------------------------------

/**
 * The RTT file format is used by the Attila deterministic radiation
 * transport code. The specific mesh format can be found in Chapter 9
 * of the Attila manual. The format is defined by xml like, block/end block
 * type syntax. The implementation at the time of writing supports a subset
 * of the whole format, and even Attila does not support the entireity of
 * its own mesh format. 
 * 
 * The mesh contains several features, that as a whole allow the conversion
 * from the RTT format, to a DAGMC geometry and a Tet Mesh for tallying.
 *
 * Sides - Defines the 6 boundary condtions for top, bottom, front, back
 *         left and right, as well as internal and external.
 *---------------------------------------------------------------------
 * Faces - Logically equivalent to surfaces in DAGMC, containers for triangles, includes
 *         the definition of the sense of the faces with respect to the Cells (volumes)
 *         which bound it. 
 * 
 *         The face syntax looks like
 *
 *         1 (+)Pyrex@14
 *         
 *         This means Face (surface) 1 is used to define the insde of the Pyrex cell only
 *          
 *         75 (+)Pyrex/(-)Fuel30@25
 *
 *         This means Face (surface) 75 is used by both Cell Pyrex and Cell Fuel 30, 
 *         the + and - signs refer to the sense, i.e. the inside sense defines the Pyrex and 
 *         the outside sense defines the Fuel.
 *---------------------------------------------------------------------
 * Cells - Entityset like coillections of tetrahedra which define contiguous material properties
 *
 *        cell_flags
 *          1 REGIONS
 *            1 Pyrex
 *        end_cell_flags
 * 
 * Defines that there is 1 region called Pyrex
 *---------------------------------------------------------------------
 * Nodes - Defines the vertices for facets and tets, the syntax of which is shown below
 *
 *   100  1.8900000000E+03  0.0000000000E+00  5.0000000000E+03 100
 *
 * Defines that this is node 100, and has the coordinates 1890.0, 0.0 5000.0 cm
**---------------------------------------------------------------------
 * Side (element) - Triangles
 *
 *  1 3 874 132 154 3 6365
 *
 * Defines that this is side element 1, it has 3 nodes, 874, 132 and 154, 
 * side ID 3 and surface number 6365
 *---------------------------------------------------------------------
 * Cells (element) - Tetrahedra
 *
 *   691 4 599 556 1218 1216 2
 *
 * Defines that this is tet 691, it has 4 connections to nodes 599, 556, 
 * 1218, 1216 and belongs to cell number 2.
 *
 */

#ifndef READRTT_HPP
#define READRTT_HPP

#ifndef IS_BUILDING_MB
  #error "ReadRTT.hpp isn't supposed to be included into an application"
#endif

#include <iostream>
#include <fstream>
#include <sstream>
#include <map>
#include <vector>

#include "moab/Interface.hpp"
#include "moab/ReaderIface.hpp"
#include "FileTokenizer.hpp"
#include "moab/RangeMap.hpp"

// structure to hold sense & vol data
struct boundary {
  int sense;
  std::string name;
};

// structure to hold side data
struct side {
  int id;
  int senses[2];
  std::string names[2];
};

// structure to hold cell data
struct cell {
  int id;
  std::string name;
};

// structure to hold node data
struct node {
  int id;
  double x,y,z;
};

// strucutre to hold facet data
struct facet {
  int id;
  int connectivity[3];
  int side_id;
  int surface_number;
};

// strucutre to hold tet data
struct tet {
  int id;
  int connectivity[4];
  int material_number;
};

namespace moab {

class ReadUtilIface;
class GeomTopoTool;

class ReadRTT : public ReaderIface
{

public:
  // factory method
  static ReaderIface* factory( Interface* );

  // generic overloaded core -> load_file
  ErrorCode load_file( const char* file_name,
                       const EntityHandle* file_set,
                       const FileOptions& opts,
                       const SubsetList* subset_list = 0,
                       const Tag* file_id_tag = 0 );
  // constructor
  ReadRTT(Interface* impl = NULL);

  // destructor
  virtual ~ReadRTT();

  // implementation empty
  ErrorCode read_tag_values( const char* file_name,
                             const char* tag_name,
                             const FileOptions& opts,
                             std::vector<int>& tag_values_out,
                             const SubsetList* subset_list = 0 );

protected:

// private functions
private:
  /**
   * generates the topology of the problem from the already read input data, loops over the 2 and 3 dimension macrodata that
   * exist from the rtt file, sides = dagmc surfaces, cells = dagmc cells, creates a meshset for each surface and tags with
   * the id number, and similarly makes a meshset for dagmc cells and tags with the id number. The surfaces are added to the s
   * surface map, where the key is the surface ID number (1->N) and (cells and surfaces are added to an dimesional entity map
   * stored in the class
   *
   * @param side_data, vector of side data
   * @param cell_data, vector of vector of cell data
   * @param surface_map, reference to the surface map of data
   *
   */
  ErrorCode generate_topology(std::vector<side> side_data,
			      std::vector<cell> cell_data,
			      std::map <int,EntityHandle> &surface_map);
  /**
   * Generate parent child links to create DAGMC like structure of surface meshsets being children
   * of parent cell meshsets. By looping over the surfaces (1->N), look in the description of the 
   * cells that are shared by that surface, and then make the surface the child of the parent volume.
   * The appropriate sense data will be set later
   *
   * @param num_ents[4], array containing the number of surfaces, cells, groups etc
   * @param entity_map[4], vector of maps containing data by dimension
   * @param side_data, vector of all the side data in the problem
   * @param cell_data, vector of the cell data in the problem
   *
   */
  void generate_parent_child_links(int num_ents[4],std::vector<EntityHandle> entity_map[4],
				   std::vector<side> side_data, std::vector<cell> cell_data);
  /**
   * Sets the appropriate surface senses for each surface in the problem. By looping through all the 
   * surfaces, we determine from the side_data vector, the volume id's that are shared, then using 1 to mean
   * +ve sense and -1 to mean -ve sense wrt the volume.
   *
   * @param num_ents[4], array containing the number of surfaces, cells, groups etc
   * @param entity_map[4], vector of maps containing data by dimension
   * @param side_data, vector of all the side data in the problem
   * @param cell_data, vector of the cell data in the problem
   *
   */  
  void set_surface_senses(int num_ents[4], std::vector<EntityHandle> entity_map[4],
			  std::vector<side> side_data, std::vector<cell> cell_data);

  /**
   * creates the group data requried for dagmc, reflecting planes, material assignments etc
   * @param entity_map, vector of vector of entitiy handles for each dimension
   *
   * @returns moab::ErrorCode
   */
  ErrorCode setup_group_data(std::vector<EntityHandle> entity_map[4]);


  /**
   * create a group of a given name, mustkeep track of id
   * @param group_name, name of the group
   * @param id, integer id number
   *
   * returns the entity handle of the group
   */
  EntityHandle create_group(std::string group_name, int id);

  /**
   * Builds the full MOAB representation of the data, making vertices from coordinates, triangles from vertices
   * and tets from the same vertices. Tags appropriate to each dataset collection are applied, triangles are 
   * tagged with the surface id and side id they belong to, as well as tagging the surface with the same data. Tets
   * are similarly tagged only with the Material number
   *
   * @param node_data the node data
   * @param facet_data, the triangles in the problem
   * @param tet_data, the tets in the problem
   * @param surface_map, the map of surface meshset and id numbers 
   *
   * @return moab::ErrorCode
   */
  ErrorCode build_moab(std::vector<node> node_data,
		       std::vector<facet> facet_data,
		       std::vector<tet> tet_data,
		       std::map <int,EntityHandle> surface_map);

  /**
   * Reads the full set of side data from the file
   *
   * @param filename, the file to read all the side data from
   * @param side data, a vector containing all the read side data
   *
   * @return moab::ErrorCode
   */
  ErrorCode read_sides(const char* filename, std::vector<side> &side_data);

  /**
   * Reads the full set of cell data from the file
   *
   * @param filename, the file to read all the side data from
   * @param cell data, a vector containing all the read cell data
   *
   * @return moab::ErrorCode
   */
  ErrorCode read_cells(const char* filename, std::vector<cell> &cell_data);

  /**
   * Reads the full set of node data from the file
   *
   * @param filename, the file to read all the side data from
   * @param node data, a vector containing all the read node data
   *
   * @return moab::ErrorCode
   */
  ErrorCode read_nodes(const char* filename, std::vector<node> &node_data);
  
  /**
   * Reads the full set of facet data from the file
   *
   * @param filename, the file to read all the side data from
   * @param facet data, a vector containing all the read facet data
   *
   * @return moab::ErrorCode
   */
  ErrorCode read_facets(const char* filename, std::vector<facet> &facet_data);
  
  /**
   * Reads the full set of tet data from the file
   *
   * @param filename, the file to read all the side data from
   * @param tet data, a vector containing all the read tet data
   *
   * @return moab::ErrorCode
   */
  ErrorCode read_tets(const char* filename, std::vector<tet> &tet_data);

  /**
   * Reads a single atomic cell data string and populates a cell struct 
   *
   * @param celldata, a string of read data and 
   *
   * @return cell, the propulated cell struct
   */
  cell get_cell_data(std::string celldata);

  /**
   * Reads a single atomic side data string and populates a side struct 
   *
   * @param sidedata, a string of read data and 
   *
   * @return side, the propulated side struct
   */  
  side get_side_data(std::string sidedata);
  
  /**
   * Reads a single atomic node data string and populates a node struct 
   *
   * @param sidedata, a string of read data and 
   *
   * @return node, the propulated node struct
   */  
  node get_node_data(std::string nodedata);

  /**
   * Reads a single atomic facet data string and populates a facet struct 
   *
   * @param facetdata, a string of facet data and 
   *
   * @return facet, the propulated facet struct
   */  
  facet get_facet_data(std::string facetdata);

  /**
   * Reads a single atomic tet data string and populates a tet struct 
   *
   * @param tetdata, a string of tet data and 
   *
   * @return tet, the propulated tet struct
   */  
  tet get_tet_data(std::string tetdata);

  /**
   * Splits a string into a vector of substrings delimited by split_char
   *
   * @param string_to_split, the string that needs splitting into chunks 
   * @param split_char, the character to split the string with
   *
   * @return a vector of strings that are delimited by split_char
   */  
  std::vector<std::string> split_string(std::string string_to_split, char split_char);

  /**
   * Splits an Attila cellname and populates a boundary structure
   *
   * @param attila_cellname, string containing the boundary information
   *
   * @return a boundary object
   */ 
  boundary split_name(std::string atilla_cellname);

  /**
   * Count the number of unique surface numbers in the dataset, also get list of surface numbers
   * @param side_data, collection of all the side data in the mesh
   * @param surface_numbers, collection of surface numbers
   *
   * returns the number of surface numbers
   */
  int count_sides(std::vector<side> side_data,
		  std::vector<int> &surface_numbers);

  // Class Member variables
  private:
  // read mesh interface
  ReadUtilIface* readMeshIface;
  // Moab Interface
  Interface* MBI;
  // geom tool instance
  GeomTopoTool* myGeomTool;
  // tags used in the problem
  Tag geom_tag,id_tag,name_tag,category_tag,faceting_tol_tag;
};

} // namespace moab

#endif
