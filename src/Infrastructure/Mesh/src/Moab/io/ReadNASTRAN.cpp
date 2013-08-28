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

#include "ReadNASTRAN.hpp"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <assert.h>
#include <cmath>

#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "Internals.hpp" // for MB_START_ID
#include "moab/Range.hpp"
#include "FileOptions.hpp"
#include "FileTokenizer.hpp"
#include "MBTagConventions.hpp"
#include "moab/CN.hpp"

namespace moab {

ReaderIface* ReadNASTRAN::factory( Interface* iface ) { 
  return new ReadNASTRAN( iface );
}

// constructor
ReadNASTRAN::ReadNASTRAN(Interface* impl)
  : MBI(impl) {
    assert(NULL != impl);
    MBI->query_interface(readMeshIface);
    assert(NULL != readMeshIface);
}

// destructor
ReadNASTRAN::~ReadNASTRAN() {
  if (readMeshIface) {
    MBI->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

ErrorCode ReadNASTRAN::read_tag_values( const char*        /*file_name*/,
                                        const char*        /*tag_name*/,
                                        const FileOptions& /*opts*/,
                                        std::vector<int>&  /*tag_values_out*/,
                                        const SubsetList*  /*subset_list*/ )
{
  return MB_NOT_IMPLEMENTED;
}

// load the file as called by the Interface function
ErrorCode ReadNASTRAN::load_file(const char                      *filename, 
                                   const EntityHandle            *, 
                                   const FileOptions             &,
                                   const ReaderIface::SubsetList *subset_list,
                                   const Tag*                     file_id_tag) {
  // at this time there is no support for reading a subset of the file
  if (subset_list) {
    readMeshIface->report_error( "Reading subset of files not supported for NASTRAN." );
    return MB_UNSUPPORTED_OPERATION;
  }

  nodeIdMap.clear();
  elemIdMap.clear();

  bool debug = false;
  if (debug) std::cout << "begin ReadNASTRAN::load_file" << std::endl;
  ErrorCode result;

  // Count the entities of each type in the file. This is used to allocate the node array. 
  int entity_count[MBMAXTYPE];
  for(int i=0; i<MBMAXTYPE; i++) entity_count[i] = 0;
 
  /* Determine the line_format of the first line. Assume that the entire file 
     has the same format. */  
  std::string line;
  std::ifstream file (filename);
  if (!getline(file,line))
    return MB_FILE_DOES_NOT_EXIST;
  line_format format;
  result = determine_line_format( line, format );
  if(MB_SUCCESS != result) return result;

  /* Count the number of each entity in the file. This allows one to allocate
     a sequential array of vertex handles. */
  while(!file.eof()) {
    // Cut the line into fields as determined by the line format.
    // Use a vector to allow for an unknown number of tokens (continue lines).
    // Continue lines are not implemented.
    std::vector<std::string> tokens;
    tokens.reserve(10); // assume 10 fields to avoid extra vector resizing
    result = tokenize_line( line, format, tokens );
    if(MB_SUCCESS != result) return result;

    // Process the tokens of the line. The first token describes the entity type.
    EntityType type;
    result = determine_entity_type( tokens.front(), type );
    if(MB_SUCCESS != result) return result;
    entity_count[type]++;
    getline(file,line);
  }      

  if(debug) {
    for(int i=0; i<MBMAXTYPE; i++) {
      std::cout << "entity_count[" << i << "]=" << entity_count[i] << std::endl;
    }
  }
  
  // Keep list of material sets
  std::vector<Range> materials;

  // Now that the number of vertices is known, create the vertices.
  EntityHandle start_vert = 0;
  std::vector<double*> coord_arrays(3);
  result = readMeshIface->get_node_coords( 3, entity_count[0], MB_START_ID,
					   start_vert, coord_arrays );
  if(MB_SUCCESS != result) return result;
  if(0 == start_vert) return MB_FAILURE; // check for NULL
  int id, vert_index = 0;
  if(debug) std::cout << "allocated coord arrays" << std::endl;

  // Read the file again to create the entities.
  file.clear();  // clear eof state from object
  file.seekg(0); // rewind file
  while (!file.eof()) {
    getline(file,line);

    // Cut the line into fields as determined by the line format.
    // Use a vector to allow for an unknown number of tokens (continue lines).
    // Continue lines are not implemented.
    std::vector<std::string> tokens;
    tokens.reserve(10); // assume 10 fields to avoid extra vector resizing
    result = tokenize_line( line, format, tokens );
    if(MB_SUCCESS != result) return result;

    // Process the tokens of the line. The first token describes the entity type.
    EntityType type;
    result = determine_entity_type( tokens.front(), type );
    if(MB_SUCCESS != result) return result;

    // Create the entity.
    if(MBVERTEX == type) {
      double* coords[3] = { coord_arrays[0] + vert_index,
                            coord_arrays[1] + vert_index,
                            coord_arrays[2] + vert_index };
      result = read_node(tokens, debug, coords, id ); 
      if(MB_SUCCESS != result) return result;
      if (!nodeIdMap.insert( id, start_vert + vert_index, 1 ).second)
        return MB_FAILURE; // duplicate IDs!
      ++vert_index;
    } else {
      result = read_element( tokens, materials, type, debug ); 
      if(MB_SUCCESS != result) return result;
    }
  }

  result = create_materials( materials );
  if(MB_SUCCESS != result) return result;

  result = assign_ids( file_id_tag );
  if(MB_SUCCESS != result) return result;  
  
  file.close();
  nodeIdMap.clear();
  elemIdMap.clear();
  return MB_SUCCESS;
}

  /* Determine the type of NASTRAN line: small field, large field, or free field.
     small field: each line has 10 fields of 8 characters
     large field: 1x8, 4x16, 1x8. Field 1 must have an asterisk following the character string
     free field: each line entry must be separated by a comma
     Implementation tries to avoid more searches than necessary. */
ErrorCode ReadNASTRAN::determine_line_format( const std::string line, 
                                                  line_format &format ) {
  std::string::size_type found_asterisk = line.find("*");
  if(std::string::npos != found_asterisk) {
      format = LARGE_FIELD;
      return MB_SUCCESS;
    } else {
      std::string::size_type found_comma = line.find(",");
      if(std::string::npos != found_comma) {
	format = FREE_FIELD;
	return MB_SUCCESS;
      } else {
	format = SMALL_FIELD;
	return MB_SUCCESS;
      }
    }
  }

  /* Tokenize the line. Continue-lines have not been implemented. */
ErrorCode ReadNASTRAN::tokenize_line(const std::string line, const line_format format, 
				       std::vector<std::string> &tokens ) { 
  size_t line_size = line.size();
  switch(format) {
  case SMALL_FIELD: {
    // Expect 10 fields of 8 characters.
    // The sample file does not have all 10 fields in each line
    const int field_length = 8;
    unsigned int n_tokens = line_size / field_length;
    for(unsigned int i=0; i<n_tokens; i++) {
      tokens.push_back( line.substr(i*field_length,field_length) );
    }
    break; 
  } case LARGE_FIELD:
    return MB_NOT_IMPLEMENTED;
  case FREE_FIELD:
    return MB_NOT_IMPLEMENTED;
  default:
    return MB_FAILURE;
  }

  return MB_SUCCESS;
}

ErrorCode ReadNASTRAN::determine_entity_type( const std::string first_token, 
                                                  EntityType &type ) {
  if     (0==first_token.compare("GRID    ")) type = MBVERTEX;
  else if(0==first_token.compare("CTETRA  ")) type = MBTET;
  else if(0==first_token.compare("CPENTA  ")) type = MBPRISM;
  else if(0==first_token.compare("CHEXA   ")) type = MBHEX;
  else return MB_NOT_IMPLEMENTED;

  return MB_SUCCESS;
}

/* Some help from Jason:
   Nastran floats must contain a decimal point, may contain
   a leading '-' and may contain an exponent.  The 'E' is optional
   when specifying an exponent.  A '-' or '+' at any location other
   than the first position indicates an exponent.  For a positive
   exponent, either a '+' or an 'E' must be specified.  For a
   negative exponent, the 'E' is option and the '-' is always specified.
   Examples for the real value 7.0 from mcs2006 quick reference guide:
   7.0  .7E1  0.7+1  .70+1  7.E+0  70.-1

   From the test file created in SC/Tetra:
   GRID           1       03.9804546.9052-15.6008-1    
   has the coordinates: ( 3.980454, 6.9052e-1, 5.6008e-1 )
   GRID      200005       04.004752-3.985-15.4955-1  
   has the coordinates: ( 4.004752, -3.985e-1, 5.4955e-1 ) */
ErrorCode ReadNASTRAN::get_real( const std::string token, double &real ) {
  std::string significand = token;
  std::string exponent = "0";

  // Cut off the first digit because a "-" could be here indicating a negative
  // number. Instead we are looking for a negative exponent.
  std::string back_token = token.substr(1);
  
  // A minus that is not the first digit is always a negative exponent
  std::string::size_type found_minus = back_token.find("-");
  if(std::string::npos != found_minus) {
    // separate the significand from the exponent at the "-"
    exponent = token.substr( found_minus+1 );
    significand = token.substr( 0, found_minus+1 );

    // if the significand has an "E", remove it
    if(std::string::npos != significand.find("E")) 
      // Assume the "E" is at the end of the significand.
      significand = significand.substr( 1, significand.size()-2 );

    // If a minus does not exist past the 1st digit, but an "E" or "+" does, then 
    // it is a positive exponent. First look for an "E",
  } else { 
    std::string::size_type found_E = token.find("E");
    if(std::string::npos != found_E) {
      significand = token.substr(0, found_E-1);
      exponent = token.substr(found_E+1);
      // if there is a "+" on the exponent, cut it off
      std::size_t found_plus = exponent.find("+");
      if(std::string::npos != found_plus) {
	exponent = exponent.substr(found_plus+1);
      }
    } else {
      // if there is a "+" on the exponent, cut it off
      std::size_t found_plus = token.find("+");
      if(std::string::npos != found_plus) {
	significand = token.substr(0, found_plus-1);
	exponent = token.substr(found_plus+1);
      }
    }
  }
    
  // now assemble the real number
  double signi = atof(significand.c_str());
  double expon = atof(exponent.c_str());

  if(HUGE_VAL==signi || HUGE_VAL==expon) return MB_FAILURE;
  real = signi * pow( 10, expon );
  return MB_SUCCESS;
}

/* It has been determined that this line is a vertex. Read the rest of
   the line and create the vertex. */
ErrorCode ReadNASTRAN::read_node( const std::vector<std::string> tokens, 
				    const bool debug, 
				    double* coords[3],
                                    int& id ) {
  // read the node's id (unique)
  ErrorCode result;
  id = atoi(tokens[1].c_str());

  // read the node's coordinate system number
  // "0" or blank refers to the basic coordinate system.
  int coord_system = atoi(tokens[2].c_str());
  if(0 != coord_system) {
    std::cerr << "ReadNASTRAN: alternative coordinate systems not implemented" 
              << std::endl;
    return MB_NOT_IMPLEMENTED;    
  }

  // read the coordinates
  for(unsigned int i=0; i<3; i++) {
    result = get_real( tokens[i+3], *coords[i] );
    if(MB_SUCCESS != result) return result;
    if(debug) std::cout << "read_node: coords[" << i << "]=" << coords[i] << std::endl;
  }

  return MB_SUCCESS;
}

/* The type of element has already been identified. Read the rest of the
   line and create the element. Assume that all of the nodes have already
   been created. */
ErrorCode ReadNASTRAN::read_element( const std::vector<std::string> tokens, 
				       std::vector<Range> &materials,
				       const EntityType element_type,
				       const bool /*debug*/ ) {

  // read the element's id (unique) and material set
  ErrorCode result;
  int id = atoi(tokens[1].c_str());    
  int material = atoi(tokens[2].c_str());
  
    // Resize materials list if necessary.  This code is somewhat complicated
    // so as to avoid copying of Ranges
  if (material >= (int)materials.size()) {
    if ((int)materials.capacity() < material)
      materials.resize( material+1 );
    else {
      std::vector<Range> new_mat( material+1 );
      for (size_t i = 0; i < materials.size(); ++i)
        new_mat[i].swap( materials[i] );
      materials.swap( new_mat );
    }
  }

  // the size of the connectivity array depends on the element type
  int n_conn = CN::VerticesPerEntity(element_type);
  EntityHandle conn_verts[27];
  assert(n_conn <= (int)(sizeof(conn_verts)/sizeof(EntityHandle)));
  
  // read the connected node ids from the file
  for(int i=0; i<n_conn; i++) {
    int n = atoi(tokens[3+i].c_str());
    conn_verts[i] = nodeIdMap.find( n );
    if (!conn_verts[i]) // invalid vertex id
      return MB_FAILURE;
  }

  // Create the element and set the global id from the NASTRAN file
  EntityHandle element;
  result = MBI->create_element( element_type, conn_verts, n_conn, element );
  if(MB_SUCCESS != result) return result;
  elemIdMap.insert( id, element, 1 );
  
  materials[material].insert( element );
  return MB_SUCCESS;
}


ErrorCode ReadNASTRAN::create_materials( const std::vector<Range>& materials )
{
  ErrorCode result;
  Tag material_tag;
  int negone = -1;
  result = MBI->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                               material_tag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);
  if(MB_SUCCESS!=result) return result;
  
  for (size_t i = 0; i < materials.size(); ++i) {
    if (materials[i].empty())
      continue;
    
      // Merge with existing or create new?  Original code effectively
      // created new by only merging with existing in current file set,
      // so do the same here. - j.kraftcheck
      
    EntityHandle handle;
    result = MBI->create_meshset( MESHSET_SET, handle );
    if (MB_SUCCESS != result)
      return result;
    
    result = MBI->add_entities( handle, materials[i] );
    if (MB_SUCCESS != result)
      return result;
    
    int id = i;
    result = MBI->tag_set_data( material_tag, &handle, 1, &id );
    if (MB_SUCCESS != result)
      return result;
  }
  
  return MB_SUCCESS;
}

ErrorCode ReadNASTRAN::assign_ids( const Tag* file_id_tag )
{

  // create tag
  ErrorCode result;
  Tag id_tag;
  int zero = 0;
  result = MBI->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                               id_tag, MB_TAG_DENSE|MB_TAG_CREAT, &zero);
  if(MB_SUCCESS!=result && MB_ALREADY_ALLOCATED!=result) return result;

  RangeMap<int,EntityHandle>::iterator i;
  std::vector<int> ids;
  for (int t = 0; t < 2; ++t) {
    RangeMap<int,EntityHandle>& fileIdMap = t ? elemIdMap : nodeIdMap;
    for (i = fileIdMap.begin(); i != fileIdMap.end(); ++i) {
      Range range( i->value, i->value + i->count - 1 );

      result = readMeshIface->assign_ids( id_tag, range, i->begin );
      if (MB_SUCCESS != result)
        return result;

      if (file_id_tag && *file_id_tag != id_tag) {
        result = readMeshIface->assign_ids( *file_id_tag, range, i->begin );
        if (MB_SUCCESS != result)
          return result;
      }
    }
  }
  
  return MB_SUCCESS;
}

} // namespace moab
