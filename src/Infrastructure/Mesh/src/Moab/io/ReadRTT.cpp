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

/**
 * \class ReadRTT
 * \brief ReadRTT based on ReadNASTRAN
 *
 * See:
 *
 * \author Andrew Davis
 */



#include "ReadRTT.hpp"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <map>
#include <assert.h>
#include <cmath>

#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "Internals.hpp" // for MB_START_ID
#include "moab/Range.hpp"
#include "moab/FileOptions.hpp"
#include "FileTokenizer.hpp"
#include "MBTagConventions.hpp"
#include "moab/CN.hpp"
#include "moab/ErrorHandler.hpp"
#include "moab/GeomTopoTool.hpp"

namespace moab {

  ReaderIface* ReadRTT::factory( Interface* iface ){
  return new ReadRTT( iface );
}

// constructor
ReadRTT::ReadRTT(Interface* impl)
  : MBI(impl),geom_tag(0), id_tag(0), name_tag(0), category_tag(0), faceting_tol_tag(0) {
    assert(NULL != impl);
    myGeomTool = new GeomTopoTool(impl);
    MBI->query_interface(readMeshIface);
    assert(NULL != readMeshIface);

    // this section copied from ReadCGM initalisation
    int negone = -1, zero = 0;
    ErrorCode rval;
    rval = MBI->tag_get_handle( GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER,
				geom_tag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);
    assert(!rval);
    rval = MBI->tag_get_handle( GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
				id_tag, MB_TAG_DENSE|MB_TAG_CREAT, &zero);
    assert(!rval);
    rval = MBI->tag_get_handle( NAME_TAG_NAME, NAME_TAG_SIZE, MB_TYPE_OPAQUE,
				name_tag, MB_TAG_SPARSE|MB_TAG_CREAT );
    assert(!rval);
    rval = MBI->tag_get_handle( CATEGORY_TAG_NAME, CATEGORY_TAG_SIZE, MB_TYPE_OPAQUE,
				category_tag, MB_TAG_SPARSE|MB_TAG_CREAT );
    assert(!rval);
    rval = MBI->tag_get_handle("FACETING_TOL", 1, MB_TYPE_DOUBLE, faceting_tol_tag,
			     MB_TAG_SPARSE|MB_TAG_CREAT, &zero );
    assert(!rval);
    #ifdef NDEBUG
      if (!rval) {}; // Line to avoid compiler warning about variable set but not used
    #endif
}

// destructor
ReadRTT::~ReadRTT() {
  if (readMeshIface) {
    MBI->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

ErrorCode ReadRTT::read_tag_values( const char*        /*file_name*/,
                                    const char*        /*tag_name*/,
                                    const FileOptions& /*opts*/,
                                    std::vector<int>&  /*tag_values_out*/,
                                    const SubsetList*  /*subset_list*/ )
{
  return MB_NOT_IMPLEMENTED;
}

// load the file as called by the Interface function
ErrorCode ReadRTT::load_file(const char                      *filename,
                             const EntityHandle            *,
                             const FileOptions             &,
                             const ReaderIface::SubsetList *subset_list,
                             const Tag*                     /*file_id_tag*/ ) {
  ErrorCode rval;

  // at this time there is no support for reading a subset of the file
  if (subset_list) {
    std::cout << "Subset reading not supported for Rtt meshes" << std::endl;
    return MB_UNSUPPORTED_OPERATION;
  }

  // test to see if file exists
  FILE *file = NULL;
  file = fopen (filename,"r");
  if(file == NULL) return MB_FILE_DOES_NOT_EXIST;
  // otherwise close the file
  fclose(file);

  // read the side_flag data
  std::vector<side> side_data;
  rval = ReadRTT::read_sides(filename,side_data);
  if(rval != MB_SUCCESS) return rval;

  // read the cell data
  std::vector<cell> cell_data;
  rval = ReadRTT::read_cells(filename,cell_data);
  if(rval != MB_SUCCESS) return rval;

  // read the node data
  std::vector<node> node_data;
  rval = ReadRTT::read_nodes(filename,node_data);
  if(rval != MB_SUCCESS) return rval;

  // read the facet data
  std::vector<facet> facet_data;
  rval = ReadRTT::read_facets(filename,facet_data);
  if(rval != MB_SUCCESS) return rval;

  // read the tetrahedra data
  std::vector<tet> tet_data;
  rval = ReadRTT::read_tets(filename,tet_data);
  if(rval != MB_SUCCESS) return rval;

  // make the map of surface number in the rttmesh to the surface meshset
  std::map <int,EntityHandle> surface_map; // corrsespondance of surface number to entity handle
  rval = ReadRTT::generate_topology(side_data,cell_data,surface_map);
  if(rval != MB_SUCCESS) return rval;

  // generate the rest of the database, triangles to surface meshsets etc
  rval = ReadRTT::build_moab(node_data,facet_data,tet_data,surface_map);
  if(rval != MB_SUCCESS) return rval;

  return MB_SUCCESS;
}

/*
 * builds the topology of the problem
 */
ErrorCode ReadRTT::generate_topology(std::vector<side> side_data,
				     std::vector<cell> cell_data,
				     std::map <int,EntityHandle> &surface_map){

  ErrorCode rval;
  std::vector<EntityHandle> entmap[4];
  int num_ents[4]; // number of entities in each dimension

  const char geom_categories[][CATEGORY_TAG_SIZE] =
    {"Vertex\0", "Curve\0", "Surface\0", "Volume\0", "Group\0"};

  std::vector<int> surface_numbers; // the surface numbers in the problem

  // corresponds to number of cad like surfaces and cad like volumes
  num_ents[2] = side_data.size();
  num_ents[3] = cell_data.size();

  // loop over surfaces & volumes
  for ( int dim = 2 ; dim <= 3 ; dim++ ) {
    for ( int i = 0 ; i != num_ents[dim] ; i++ ) {
      EntityHandle handle;
      // create a meshset for each entity surface/volume
      rval = MBI->create_meshset( dim == 1 ? MESHSET_ORDERED : MESHSET_SET, handle );
      // if failure
      if (rval != MB_SUCCESS ) return rval;

      // collect the entity handles into an
      entmap[dim].push_back(handle);

      // set the dimension tag
      rval = MBI->tag_set_data( geom_tag, &handle, 1, &dim );
      // if fail
      if (MB_SUCCESS != rval)	return rval;
      // if we are a surface
      if(dim == 2 ) {
        // tag the id onto the surface meshset
	rval = MBI->tag_set_data( id_tag, &handle, 1, &side_data[i].id );
        // inesert entity into the map
	surface_map[side_data[i].id]=handle;
      } else {
        // otherwise we set the volume tag data, loop is only 2 & 3 dim
	rval = MBI->tag_set_data( id_tag, &handle, 1, &cell_data[i].id );
      }
      // if fail
      if (MB_SUCCESS != rval)	return rval;
      // set the category tag
      rval = MBI->tag_set_data( category_tag, &handle, 1, &geom_categories[dim] );
      if (MB_SUCCESS != rval)	return rval;
    }
  }
  
  // generate parent child links
  // best to loop over the surfaces and assign them to volumes, we can then assign facets to
  // to each surface
  generate_parent_child_links(num_ents,entmap,side_data,cell_data);

  // set the surface senses
  set_surface_senses(num_ents,entmap,side_data,cell_data);

  // set the group data
  rval = setup_group_data(entmap);

  return MB_SUCCESS;
}

/*
 * builds the moab representation of the mesh
 */
ErrorCode ReadRTT::build_moab(std::vector<node> node_data,
			      std::vector<facet> facet_data,
			      std::vector<tet> tet_data,
			      std::map<int,EntityHandle> surface_map) {

  ErrorCode rval; // reusable return value
  EntityHandle file_set; // the file handle
  // create the file set
  rval = MBI->create_meshset( MESHSET_SET, file_set );
  if (MB_SUCCESS != rval) return rval;

  // create the vertices
  EntityHandle handle;
  std::vector<node>::iterator it; // iterate over the nodes
  Range mb_coords; //range of coordinates
  for ( it = node_data.begin() ; it != node_data.end() ; ++it) {
    node tmp = *it;
    double coords[3] = {tmp.x,tmp.y,tmp.z};
    rval = MBI->create_vertex(coords,handle);
    if (MB_SUCCESS != rval) return rval;
    mb_coords.insert(handle); // inesert handle into the coordinate range
  }

  // add verts to set
  rval = MBI->add_entities(file_set,mb_coords);

  // create sense tag
  Tag side_id_tag, surface_number_tag;
  //  int zero = 0;
  rval = MBI->tag_get_handle( "SIDEID_TAG", 1, MB_TYPE_INTEGER,
			      side_id_tag, MB_TAG_SPARSE|MB_TAG_CREAT);
  rval = MBI->tag_get_handle( "SURFACE_NUMBER", 1, MB_TYPE_INTEGER,
			      surface_number_tag, MB_TAG_SPARSE|MB_TAG_CREAT);

  // create the facets
  EntityHandle triangle;
  std::vector<facet>::iterator it_f;
  // range of triangles
  Range mb_tris;
  // loop over the facet data
  for ( it_f = facet_data.begin() ; it_f != facet_data.end() ; ++it_f) {
    facet tmp = *it_f;
    // get the nodes for the triangle
    EntityHandle tri_nodes[3]={mb_coords[tmp.connectivity[0]-1],
			       mb_coords[tmp.connectivity[1]-1],
			       mb_coords[tmp.connectivity[2]-1]};
    // create a triangle element
    rval = MBI->create_element(MBTRI,tri_nodes,3,triangle);
    // tag in side id on the triangle
    rval = MBI->tag_set_data(side_id_tag,&triangle,1,&tmp.side_id);
    // tag the surface number on the triangle
    rval = MBI->tag_set_data(surface_number_tag,&triangle,1,&tmp.surface_number);
    // insert vertices and triangles into the appropriate surface meshset
    EntityHandle meshset_handle = surface_map[tmp.surface_number];
    // also set surface tag
    rval = MBI->tag_set_data(side_id_tag,&meshset_handle,1,&tmp.side_id);
    rval = MBI->tag_set_data(surface_number_tag,&meshset_handle,1,&tmp.surface_number);
    // add vertices to the mesh
    rval = MBI->add_entities(meshset_handle,&(*tri_nodes),3);
    // add triangles to the meshset
    rval = MBI->add_entities(meshset_handle,&triangle,1);
    // ineter triangles into large run
    mb_tris.insert(triangle);
  }
  // add tris to set to fileset
  rval = MBI->add_entities(file_set,mb_tris);

  // create material number tag
  Tag mat_num_tag;
  //  int zero = 0;
  rval = MBI->tag_get_handle( "MATERIAL_NUMBER", 1, MB_TYPE_INTEGER,
			      mat_num_tag, MB_TAG_SPARSE|MB_TAG_CREAT);

  // create the tets
  EntityHandle tetra; // handle for a specific tet
  std::vector<tet>::iterator it_t;
  Range mb_tets;
  // loop over all tets
  for ( it_t = tet_data.begin() ; it_t != tet_data.end() ; ++it_t) {
    tet tmp = *it_t;
    // get the handles for the tet
    EntityHandle tet_nodes[4]={mb_coords[tmp.connectivity[0]-1],
			       mb_coords[tmp.connectivity[1]-1],
			       mb_coords[tmp.connectivity[2]-1],
			       mb_coords[tmp.connectivity[3]-1]};
    // create the tet
    rval = MBI->create_element(MBTET,tet_nodes,4,tetra);
    int mat_number = tmp.material_number;
    // tag the tet with the material number
    rval = MBI->tag_set_data(mat_num_tag,&tetra,1,&mat_number);
    // set the tag data
    mb_tets.insert(tetra);
  }
  // add tris to set
  rval = MBI->add_entities(file_set,mb_tets);

  return MB_SUCCESS;
}

/*
 * reads the side data from the filename pointed to
 */
ErrorCode ReadRTT::read_sides(const char* filename, std::vector<side> &side_data ){
  std::string line; // the current line being read
  std::ifstream input_file (filename); // filestream for rttfile
  // file ok?
  if ( !input_file.good() ) {
    std::cout << "Problems reading file = " << filename << std::endl;
    return MB_FAILURE;
  }
  // if it works
  if (input_file.is_open()) {
    while ( std::getline (input_file,line) ) {
      if(line.compare("  2 FACES\0") == 0) {
	// read lines until find end nodes
	while( std::getline( input_file,line)) {
	  if(line.compare("end_side_flags\0") == 0) break;
	  side data = ReadRTT::get_side_data(line);
	  side_data.push_back(data);
	}
      }
    }
    input_file.close();
  }
  if(side_data.size() == 0) return MB_FAILURE;
  return MB_SUCCESS;
}

/*
 * reads the cell data from the filename pointed to
 */
  ErrorCode ReadRTT::read_cells(const char* filename, std::vector<cell> &cell_data ){
  std::string line; // the current line being read
  std::ifstream input_file (filename); // filestream for rttfile
  // file ok?
  if ( !input_file.good() ) {
    std::cout << "Problems reading file = " << filename << std::endl;
    return MB_FAILURE;
  }
  // if it works
  if (input_file.is_open()) {
    while ( std::getline (input_file,line)) {
      if(line.compare("  1 REGIONS\0") == 0) {
	// read lines until find end nodes
	while( std::getline( input_file,line)) {
	  if(line.compare("end_cell_flags\0") == 0) break;
	  cell data = ReadRTT::get_cell_data(line);
	  cell_data.push_back(data);
	}
      }
    }
    input_file.close();
  }
  if(cell_data.size() == 0) return MB_FAILURE;
  return MB_SUCCESS;
}

/*
 * Reads the node data fromt the filename pointed to
 */
ErrorCode ReadRTT::read_nodes(const char* filename, std::vector<node> &node_data ){
  std::string line; // the current line being read
  std::ifstream input_file (filename); // filestream for rttfile
  // file ok?
  if ( !input_file.good() ) {
    std::cout << "Problems reading file = " << filename << std::endl;
    return MB_FAILURE;
  }
  
  // if it works
  if (input_file.is_open()) {
    while ( std::getline (input_file,line) ) {
      if(line.compare("nodes\0") == 0) {
	// read lines until find end nodes
	while( std::getline( input_file,line)) {
	  if(line.compare("end_nodes\0") == 0) break;
	  node data = ReadRTT::get_node_data(line);
	  node_data.push_back(data);
	}
      }
    }
    input_file.close();
  }
  if(node_data.size() == 0) return MB_FAILURE;
  return MB_SUCCESS;
}

/*
 * Reads the facet data fromt the filename pointed to
 */
ErrorCode ReadRTT::read_facets(const char* filename, std::vector<facet> &facet_data ){
  std::string line; // the current line being read
  std::ifstream input_file (filename); // filestream for rttfile
  // file ok?
  if ( !input_file.good() ) {
    std::cout << "Problems reading file = " << filename << std::endl;
      return MB_FAILURE;
  }
  
  // if it works
  if (input_file.is_open()) {
    while ( std::getline (input_file,line) ) {
      if(line.compare("sides\0") == 0) {
	// read lines until find end nodes
	while( std::getline( input_file,line)) {
	  if(line.compare("end_sides\0") == 0) break;
	  facet data = ReadRTT::get_facet_data(line);
	  facet_data.push_back(data);
        }
      }
    }
    input_file.close();
  }
  if(facet_data.size() == 0) return MB_FAILURE;
  return MB_SUCCESS;
}

/*
 * Reads the facet data fromt the filename pointed to
 */
ErrorCode ReadRTT::read_tets(const char* filename, std::vector<tet> &tet_data ){
  std::string line; // the current line being read
  std::ifstream input_file (filename); // filestream for rttfile
  // file ok?
  if ( !input_file.good() ) {
      std::cout << "Problems reading file = " << filename << std::endl;
      return MB_FAILURE;
  }
  // if it works
  if (input_file.is_open()) {
    while ( std::getline (input_file,line) ) {
      if(line.compare("cells\0") == 0)  {
	// read lines until find end nodes
	while( std::getline( input_file,line))  {
	  if(line.compare("end_cells\0") == 0) break;
	  tet data = ReadRTT::get_tet_data(line);
	  tet_data.push_back(data);
	}
      }
    }
    input_file.close();
  }
  if(tet_data.size() == 0) return MB_FAILURE;
  return MB_SUCCESS;
}

/*
 * given the string sidedata, get the id number, senses and names of the sides
 */
side ReadRTT::get_side_data(std::string sidedata) {
  side new_side;
  std::vector<std::string> tokens;
  tokens = ReadRTT::split_string(sidedata,' ');

  std::vector<std::string>::iterator it;
  // set the side id
  if(tokens.size() != 2 ) {
    MB_SET_ERR_RET_VAL("Error, too many tokens found from side_data",new_side);
  }
  // create the new side
  new_side.id = std::atoi(tokens[0].c_str());

  std::vector<std::string> cell_names = ReadRTT::split_string(tokens[1],'/');
  // get the boundary
  boundary new_bnd = ReadRTT::split_name(cell_names[0]);
  // set the surface sense and name
  new_side.senses[0]=new_bnd.sense;
  new_side.names[0]=new_bnd.name;
  //
  if (cell_names.size() > 1 ) {
    boundary bnd = ReadRTT::split_name(cell_names[1]);
    new_side.senses[1] = bnd.sense;
    new_side.names[1] = bnd.name;
  } else {
    new_side.senses[1]=0;
    new_side.names[1]="\0";
  }

  return new_side;
}

/*
 * given the string celldata, get the id number and name of each cell
 */
cell ReadRTT::get_cell_data(std::string celldata) {
  cell new_cell;
  std::vector<std::string> tokens;
  tokens = ReadRTT::split_string(celldata,' ');

  std::vector<std::string>::iterator it;
  
  // set the side id
  if(tokens.size() != 2 ) {
    MB_SET_ERR_RET_VAL("Error, too many tokens found from cell_data",new_cell);
  }
  // create the new side
  new_cell.id = std::atoi(tokens[0].c_str());
  new_cell.name = tokens[1];

  return new_cell;
}

/*
 * given the string nodedata, get the id number and coordinates of the node
 */
node ReadRTT::get_node_data(std::string nodedata) {
  node new_node;
  std::vector<std::string> tokens;
  tokens = ReadRTT::split_string(nodedata,' ');
  
  // set the side id
  if(tokens.size() != 5 ) {
    MB_SET_ERR_RET_VAL("Error, too many tokens found from get_node_data",new_node);
  }
  std::vector<std::string>::iterator it;
  new_node.id=std::atoi(tokens[0].c_str());
  new_node.x=std::atof(tokens[1].c_str());
  new_node.y=std::atof(tokens[2].c_str());
  new_node.z=std::atof(tokens[3].c_str());
  return new_node;
}

/*
 * given the string nodedata, get the id number, connectivity and sense data
 */
facet ReadRTT::get_facet_data(std::string facetdata) {
  facet new_facet;
  std::vector<std::string> tokens;
  tokens = ReadRTT::split_string(facetdata,' ');

  // set the side id
  if(tokens.size() != 7 ) {
    MB_SET_ERR_RET_VAL("Error, too many tokens found from get_facet_data",new_facet);
  }
  new_facet.id = std::atoi(tokens[0].c_str());
  new_facet.connectivity[0] = std::atoi(tokens[1].c_str());
  new_facet.connectivity[1] = std::atoi(tokens[2].c_str());
  new_facet.connectivity[2] = std::atoi(tokens[3].c_str());
  new_facet.side_id = std::atoi(tokens[4].c_str());
  new_facet.surface_number = std::atoi(tokens[5].c_str());

  return new_facet;
}

/*
 * given the string tetdata, get the id number, connectivity and mat num of the tet
 */
tet ReadRTT::get_tet_data(std::string tetdata) {
  tet new_tet;
  std::vector<std::string> tokens;
  tokens = ReadRTT::split_string(tetdata,' ');

  // set the side id
  if(tokens.size() != 7 ) {
    MB_SET_ERR_RET_VAL("Error, too many tokens found from get_tet_data",new_tet);
  }
  new_tet.id = std::atoi(tokens[0].c_str());
  new_tet.connectivity[0] = std::atoi(tokens[1].c_str());
  new_tet.connectivity[1] = std::atoi(tokens[2].c_str());
  new_tet.connectivity[2] = std::atoi(tokens[3].c_str());
  new_tet.connectivity[3] = std::atoi(tokens[4].c_str());
  new_tet.material_number = std::atoi(tokens[5].c_str());

  return new_tet;
}

/*
 * splits string into sense and name, to later facilitate the building
 * of sense data, strips off the tailing @ if it exists
 */
boundary ReadRTT::split_name(std::string atilla_cellname) {
  boundary new_boundary;
  // default initialisation
  new_boundary.sense = 0;
  new_boundary.name = "\0";
  // +ve sense
  if( atilla_cellname.find("+") != std::string::npos ) {
      new_boundary.sense = 1;
      // look for the @# we do not want it
      std::size_t found = atilla_cellname.find("@");
      if(found != std::string::npos)
     	  new_boundary.name=atilla_cellname.substr(3,found);
      else
	      new_boundary.name=atilla_cellname.substr(3,atilla_cellname.length());
  } else if ( atilla_cellname.find("-") != std::string::npos ) {
    // negative sense
    new_boundary.sense = -1;
    new_boundary.name=atilla_cellname.substr(3,atilla_cellname.length());
  }
  return new_boundary;
}

/*
 * splits a string in a vector of strings split by spaces
 */
 std::vector<std::string> ReadRTT::split_string(std::string string_to_split, char split_char) {
  std::istringstream ss( string_to_split );
  std::vector<std::string> tokens;
  while (!ss.eof()) {
    std::string x;               // here's a nice, empty string
    std::getline( ss, x, split_char);  // try to read the next field into it
    tokens.push_back(x);
  }

  // remove empty tokens
  std::vector<std::string>::iterator it;
  for ( it = tokens.begin() ; it != tokens.end() ; ) {
    std::string string = *it;
    if(string.compare("\0") == 0 ) 
      it = tokens.erase(it);
    else
      ++it;
  }
  return tokens;
}

/*
 * Generate the parent-child links bwetween the cell and surface meshsets
*/
void ReadRTT::generate_parent_child_links(int num_ents[4],std::vector<EntityHandle> entity_map[4],
					    std::vector<side> side_data, std::vector<cell> cell_data) {
  ErrorCode rval; // return value
  // loop over the number of surfaces
  for ( int i = 0 ; i < num_ents[2] ; i++ ) {
    // get the surface handle
    EntityHandle surf_handle = entity_map[2][i];
    // there are volumes that share this face
    for ( unsigned int shared = 0 ; shared <= 1 ; shared++ ) {
      std::string parent_name = side_data[i].names[shared];
      // find the @ sign
      unsigned pos = parent_name.find("@");
      parent_name = parent_name.substr(0,pos);

      // loop over tets looking for matching name
      for ( int j = 0 ; j < num_ents[3] ; j++ ) {
	// if match found
	if(cell_data[j].name.compare(parent_name) == 0) {
	  EntityHandle cell_handle = entity_map[3][j];
	  // parent
	  rval = MBI->add_parent_child(cell_handle,surf_handle);
	  if (rval != MB_SUCCESS) {
            std::cerr << "Failed to add parent child relationship" << std::endl;
	  }
	}
      }
    }
  }
  return;
}

/*
 * sets the sense of the surfaces wrt to volumes using geom topo tool
 */
void ReadRTT::set_surface_senses(int num_ents[4], std::vector<EntityHandle> entity_map[4],
				 std::vector<side> side_data, std::vector<cell> cell_data) {

  ErrorCode rval; // return value
  // loop over the number of surfaces
  for ( int i = 0 ; i < num_ents[2] ; i++ ) {
    EntityHandle surf_handle = entity_map[2][i];
    // there are 2 volumes that share this face
    for ( unsigned int shared = 0 ; shared <= 1 ; shared++ ) {
      std::string parent_name = side_data[i].names[shared];
      unsigned pos = parent_name.find("@");
      parent_name = parent_name.substr(0,pos);
      // loop over tets looking for matching name
      for ( int j = 0 ; j < num_ents[3] ; j++ ) {
	// if match found
	if(cell_data[j].name.compare(parent_name) == 0) {
	  EntityHandle cell_handle = entity_map[3][j];
	  // in rtt mesh +represents the inside and -represents outside
	  // in moab reverse is outside and forward is inside
	  if( side_data[i].senses[shared] == 1 )
	    rval = myGeomTool->set_sense(surf_handle,cell_handle,SENSE_FORWARD);
	  else if ( side_data[i].senses[shared] == -1 )
	    rval = myGeomTool->set_sense(surf_handle,cell_handle,SENSE_REVERSE);
	  else
	    rval = myGeomTool->set_sense(surf_handle,0,SENSE_REVERSE);

	  if(rval != MB_SUCCESS ) {
            std::cerr << "Failed to set sense appropriately" << std::endl;
	  }
	}
      }
    }
  }
  return;
}

/*
 * Add all entities that are to be part of the graveyard
 */
ErrorCode ReadRTT::setup_group_data( std::vector<EntityHandle> entity_map[4])
{
  ErrorCode rval; // error codes
  EntityHandle handle;
  handle = create_group("graveyard_comp",1);

  // add any volume to group graveyard, it is ignored by dag
  EntityHandle vol_handle = entity_map[3][0];
  rval = MBI->add_entities( handle, &vol_handle, 1);
  return rval;
}

/*
 * create a new group of with the name group name and id
 */
EntityHandle ReadRTT::create_group(std::string group_name, int id)
{
  ErrorCode rval;
  // category tags
  const char geom_categories[][CATEGORY_TAG_SIZE] =
    {"Vertex\0", "Curve\0", "Surface\0", "Volume\0", "Group\0"};

  EntityHandle handle;
  rval = MBI->create_meshset( MESHSET_SET, handle );
  if (MB_SUCCESS != rval)
    return rval;

  rval = MBI->tag_set_data( name_tag, &handle, 1, group_name.c_str() );
  if (MB_SUCCESS != rval)
    return MB_FAILURE;

  rval = MBI->tag_set_data( id_tag, &handle, 1, &id );
  if (MB_SUCCESS != rval)
    return MB_FAILURE;

  rval = MBI->tag_set_data( category_tag, &handle, 1, &geom_categories[4] );
  if (MB_SUCCESS != rval)
    return MB_FAILURE;

  return handle;
}

} // namespace moab
