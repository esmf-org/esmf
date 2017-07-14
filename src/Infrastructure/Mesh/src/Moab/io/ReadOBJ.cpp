/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#include "ReadOBJ.hpp"
#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <map>
#include <assert.h>
#include <cmath>

#include "moab/Core.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "Internals.hpp"
#include "moab/Range.hpp"
#include "moab/CartVect.hpp"
#include "moab/FileOptions.hpp"
#include "FileTokenizer.hpp"
#include "MBTagConventions.hpp"
#include "moab/CN.hpp"
#include "moab/GeomTopoTool.hpp"

namespace moab {

ReaderIface* ReadOBJ::factory( Interface* iface )
{
  return new ReadOBJ( iface );
}

// Subset of starting tokens currently supported
const char* ReadOBJ::delimiters = " ";
const char* object_start_token = "o";
const char* group_start_token = "g";
const char* vertex_start_token = "v";
const char* face_start_token = "f";

#define OBJ_AMBIGUOUS "AMBIGUOUS"
#define OBJ_UNDEFINED "UNDEFINED"

// Name of geometric entities
const char* const geom_name[] = { "Vertex\0", "Curve\0", "Surface\0", "Volume\0"};

// Geometric Categories
const char geom_category[][CATEGORY_TAG_SIZE] = 
                  { "Vertex\0","Curve\0","Surface\0","Volume\0","Group\0"};

// Constructor
ReadOBJ::ReadOBJ(Interface* impl)
  : MBI(impl),geom_tag(0), id_tag(0), name_tag(0), category_tag(0),
    faceting_tol_tag(0), geometry_resabs_tag(0), obj_name_tag(0), sense_tag(0) 
{
  assert(NULL != impl);
  MBI->query_interface(readMeshIface);
  myGeomTool = new GeomTopoTool(impl);
  assert(NULL != readMeshIface);
  
  // Get all handles 
  int negone = -1, zero = 0;
  ErrorCode rval;
  rval = MBI->tag_get_handle( GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER,
      			geom_tag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);
  MB_CHK_ERR_RET(rval);

  rval = MBI->tag_get_handle( GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
      			id_tag, MB_TAG_DENSE|MB_TAG_CREAT, &zero);
  MB_CHK_ERR_RET(rval);

  rval = MBI->tag_get_handle( NAME_TAG_NAME, NAME_TAG_SIZE, MB_TYPE_OPAQUE,
      			name_tag, MB_TAG_SPARSE|MB_TAG_CREAT );
  MB_CHK_ERR_RET(rval);

  rval = MBI->tag_get_handle( CATEGORY_TAG_NAME, CATEGORY_TAG_SIZE, MB_TYPE_OPAQUE,
      			category_tag, MB_TAG_SPARSE|MB_TAG_CREAT );
  MB_CHK_ERR_RET(rval);
  
  rval = MBI->tag_get_handle( "OBJECT_NAME", 32, MB_TYPE_OPAQUE,
      			obj_name_tag, MB_TAG_SPARSE|MB_TAG_CREAT );
  MB_CHK_ERR_RET(rval);

  rval = MBI->tag_get_handle("FACETING_TOL", 1, MB_TYPE_DOUBLE, faceting_tol_tag,
                               MB_TAG_SPARSE|MB_TAG_CREAT );
  MB_CHK_ERR_RET(rval);

  rval = MBI->tag_get_handle("GEOMETRY_RESABS", 1, MB_TYPE_DOUBLE, 
                               geometry_resabs_tag, MB_TAG_SPARSE|MB_TAG_CREAT);
  MB_CHK_ERR_RET(rval);
    
}

// Destructor
ReadOBJ::~ReadOBJ() 
{
  if (readMeshIface) 
    {
      MBI->release_interface(readMeshIface);
      readMeshIface = 0;
    }

  delete myGeomTool;
}

ErrorCode ReadOBJ::read_tag_values( const char*        /*file_name*/,
                                    const char*        /*tag_name*/,
                                    const FileOptions& /*opts*/,
                                    std::vector<int>&  /*tag_values_out*/,
                                    const SubsetList*  /*subset_list*/ )
{
  return MB_NOT_IMPLEMENTED;
}

// Load the file as called by the Interface function
ErrorCode ReadOBJ::load_file(const char *filename, 
                             const EntityHandle *, 
                             const FileOptions &,
                             const ReaderIface::SubsetList *subset_list,
                             const Tag* /*file_id_tag*/) 
{
  ErrorCode rval;
  int ignored = 0; // Number of lines not beginning with o, v, or f
  std::string line; // The current line being read
  EntityHandle vert_meshset;
  EntityHandle curr_meshset; // Current object meshset
  std::string object_name;     
  std::vector<EntityHandle> vertex_list;
  int object_id = 0, group_id = 0;// ID number for each volume/surface
  int num_groups;

  // At this time, there is no support for reading a subset of the file
  if (subset_list) 
    {
      MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for OBJ.");
    }
 
  std::ifstream input_file(filename); // Filestream for OBJ file

  // Check that the file can be read
  if ( !input_file.good() )  
    {
      std::cout << "Problems reading file = " << filename <<  std::endl;
      return MB_FILE_DOES_NOT_EXIST;
    }
   

  // If the file can be read
  if (input_file.is_open()) 
    {

      //create meshset for global vertices
      rval = MBI->create_meshset( MESHSET_SET, vert_meshset );
      MB_CHK_SET_ERR(rval, "Failed to create global vert meshset.");
      
      while ( std::getline (input_file,line) ) 
        {
          // Skip blank lines in file
	  if (line.length() == 0 ) continue;

          // Tokenize the line
          std::vector<std::string> tokens;
          tokenize(line, tokens, delimiters);

          // Each group and object line must have a name, token size is at least 2
          // Each vertex and face line should have token size of at least 4
          if (tokens.size() < 2) continue; 

          switch (get_keyword(tokens))
            { 
              // Object line
              case object_start:         
                {
                   object_id++;
                   object_name = tokens[1]; // Get name of object

                   // Create new meshset for object
                   rval = create_new_object(object_name, object_id, curr_meshset); MB_CHK_ERR(rval);
                   break;
                }

              // Group line
              case group_start: 
                {
                  group_id++;
                  num_groups = tokens.size()-1;
                  std::string group_name = "Group";  
                  for ( int i = 0; i < num_groups; i++)
                    {
                      group_name = group_name + '_' + tokens[i+1];
                    }
               
                  // Create new meshset for group 
                  rval = create_new_group(group_name, group_id, curr_meshset); MB_CHK_ERR(rval);
                  break;             
                }

              // Vertex line 
              case vertex_start:
                {
                   // Read vertex and return EH
                   EntityHandle new_vertex_eh;
                   rval = create_new_vertex(tokens, new_vertex_eh); MB_CHK_ERR(rval);
                   
                   // Add new vertex EH to list 
                   vertex_list.push_back(new_vertex_eh);
                   
                   // Add new vertex EH to the meshset
                   MBI->add_entities( vert_meshset, &new_vertex_eh, 1);
                   MB_CHK_SET_ERR(rval, "Failed to add vertex to global meshset.");
                   break;
                }

              // Face line
              case face_start:
                {
                  // Faces in .obj file can have 2, 3, or 4 vertices. If the face has
                  // 3 vertices, the EH will be immediately added to the meshset.
                  // If 4, face is split into triangles.  Anything else is ignored.
                  EntityHandle new_face_eh;
                  
                  if ( tokens.size() == 4 )
                    {  
                      rval = create_new_face(tokens, vertex_list, new_face_eh); MB_CHK_ERR(rval);
               
                      if (rval == MB_SUCCESS)
                        {
                          // Add new face EH to the meshset
                          MBI->add_entities(curr_meshset, &new_face_eh, 1);
                        }   
                    }
                  
                  else if( tokens.size() == 5 )  
                    {
                      // Split_quad fxn will create 2 new triangles from 1 quad
                      Range new_faces_eh;
                      rval = split_quad(tokens, vertex_list, new_faces_eh); MB_CHK_ERR(rval);
                     
                      // Add new faces created by split quad to meshset 
                      if (rval == MB_SUCCESS)
                        {
                          MBI->add_entities(curr_meshset, new_faces_eh);
                        }
                    } 
               
                  else
                    {
                      std::cout << "Neither tri nor a quad: " << line <<  std::endl;
                    }
                  
                  break;            
                }
         
              case valid_unsupported:
                {
                  // First token is not recognized as a supported character
                  ++ignored;
                  break;
                }
           
              default:
                {
                  MB_SET_ERR(MB_FAILURE, "Invalid/unrecognized line");
                }
           }
        }
      
    }

  // If no object lines are read (those beginning w/ 'o'), file is not obj type
  if (object_id == 0 && group_id == 0)
    {
      MB_SET_ERR(MB_FAILURE, "This is not an obj file. ");  
    }

  std::cout << "There were " << ignored << " ignored lines in this file." << std::endl;

  input_file.close(); 
  
  return MB_SUCCESS;
}


/* The tokenize function will split an input line
 * into a vector of strings based upon the delimiter
 */
void ReadOBJ::tokenize( const std::string& str, 
                         std::vector<std::string>& tokens,
                         const char* delimiters)
{
  tokens.clear();

  std::string::size_type next_token_end, next_token_start =
                         str.find_first_not_of( delimiters, 0);

  while ( std::string::npos != next_token_start )
    {
      next_token_end = str.find_first_of( delimiters, next_token_start );
      if ( std::string::npos == next_token_end )
        {
	  tokens.push_back(str.substr(next_token_start));
          next_token_start = std::string::npos;
        }
      else
        {
          tokens.push_back( str.substr( next_token_start, next_token_end -
                                        next_token_start ) );
          next_token_start = str.find_first_not_of( delimiters, next_token_end );
        }
    }
}

keyword_type ReadOBJ::get_keyword(std::vector<std::string> tokens)
{
  std::map<std::string, keyword_type> keywords;
 
  // currently supported
  keywords["o"] = object_start; 
  keywords["g"] = group_start; 
  keywords["f"] = face_start; 
  keywords["v"] = vertex_start; 

  // not currently supported, will be ignored
  keywords["vn"] = valid_unsupported;
  keywords["vt"] = valid_unsupported;
  keywords["vp"] = valid_unsupported;
  keywords["s"] = valid_unsupported;
  keywords["mtllib"] = valid_unsupported;
  keywords["usemtl"] = valid_unsupported;
  keywords["#"] = valid_unsupported;
  keywords["cstype"] = valid_unsupported;
  keywords["deg"] = valid_unsupported;
  keywords["bmat"] = valid_unsupported;
  keywords["step"] = valid_unsupported;
  keywords["p"] = valid_unsupported;
  keywords["l"] = valid_unsupported;
  keywords["curv"] = valid_unsupported;
  keywords["curv2"] = valid_unsupported;
  keywords["surf"] = valid_unsupported;
  keywords["parm"] = valid_unsupported;
  keywords["trim"] = valid_unsupported;
  keywords["hole"] = valid_unsupported;
  keywords["scrv"] = valid_unsupported;
  keywords["sp"] = valid_unsupported;
  keywords["end"] = valid_unsupported;
  keywords["mg"] = valid_unsupported;
  keywords["bevel"] = valid_unsupported;
  keywords["c_interp"] = valid_unsupported;
  keywords["d_interp"] = valid_unsupported;
  keywords["lod"] = valid_unsupported;
  keywords["shadow_obj"] = valid_unsupported;
  keywords["trace_obj"] = valid_unsupported;
  keywords["ctech"] = valid_unsupported;
  keywords["stech"] = valid_unsupported;


  return keywords[match(tokens[0], keywords)];
}

template <typename T>
std::string ReadOBJ::match(const std::string &token,
                              std::map<std::string, T> &tokenList)
{
  // Initialize with no match and obj_undefined as return string
  std::string best_match = OBJ_UNDEFINED;

  // Search the map
  for (typename std::map<std::string, T>::iterator thisToken = tokenList.begin();
       thisToken != tokenList.end();
       ++thisToken) 
    {
      // If a perfect match break the loop (assume keyword list is unambiguous)
      if (token == (*thisToken).first) 
        {
          best_match = token;
          break;
        }
    }

  // Possible return values: OBJ_UNDEFINED, keyword from list
  return best_match;
}


/*
 * The create_new_object function starts a new meshset for each object
 * that will contain all faces that make up the object.
 */
ErrorCode ReadOBJ::create_new_object ( std::string object_name,
                                       int curr_object, 
                                       EntityHandle &object_meshset )
{
  ErrorCode rval;
  
  // Create meshset to store object
  // This is also referred to as the surface meshset
  rval = MBI->create_meshset( MESHSET_SET, object_meshset );
  MB_CHK_SET_ERR(rval,"Failed to generate object mesh set.");

  // Set surface meshset tags
  rval = MBI->tag_set_data( name_tag, &object_meshset, 1,
                            object_name.c_str());
  MB_CHK_SET_ERR(rval,"Failed to set mesh set name tag.");

  rval = MBI->tag_set_data( id_tag, &object_meshset, 1, &(curr_object));
  MB_CHK_SET_ERR(rval,"Failed to set mesh set ID tag.");

  int dim = 2;
  rval = MBI->tag_set_data( geom_tag, &object_meshset, 1, &(dim));
  MB_CHK_SET_ERR(rval,"Failed to set mesh set dim tag.");

  rval = MBI->tag_set_data( category_tag, &object_meshset, 1, geom_category[2]);
  MB_CHK_SET_ERR(rval,"Failed to set mesh set category tag.");

  /* Create volume entity set corresponding to surface
     The volume meshset will have one child--
     the meshset of the surface that bounds the object. 
   */
  EntityHandle vol_meshset;
  rval = MBI->create_meshset( MESHSET_SET, vol_meshset );
  MB_CHK_SET_ERR(rval,"Failed to create volume mesh set.");

  rval = MBI->add_parent_child( vol_meshset, object_meshset );
  MB_CHK_SET_ERR(rval,"Failed to add object mesh set as child of volume mesh set.");
  
  /* Set volume meshset tags
     The volume meshset is tagged with the same name as the surface meshset
     for each object because of the direct relation between these entities.
   */
  rval = MBI->tag_set_data( obj_name_tag, &vol_meshset, 1, object_name.c_str());
  MB_CHK_SET_ERR(rval,"Failed to set mesh set name tag.");

  rval = MBI->tag_set_data( id_tag, &vol_meshset, 1, &(curr_object));
  MB_CHK_SET_ERR(rval,"Failed to set mesh set ID tag.");

  dim = 3;
  rval = MBI->tag_set_data( geom_tag, &vol_meshset, 1, &(dim));
  MB_CHK_SET_ERR(rval,"Failed to set mesh set dim tag.");

  rval = MBI->tag_set_data( name_tag, &vol_meshset, 1, geom_name[3]);
  MB_CHK_SET_ERR(rval,"Failed to set mesh set name tag.");

  rval = MBI->tag_set_data( category_tag, &vol_meshset, 1, geom_category[3]);
  MB_CHK_SET_ERR(rval,"Failed to set mesh set category tag.");
  
  rval = myGeomTool->set_sense(object_meshset, vol_meshset, SENSE_FORWARD);
  MB_CHK_SET_ERR(rval, "Failed to set surface sense."); 
  
  return rval;
}

/*
 * The create_new_group function starts a new meshset for each group
 * that will contain all faces that make up the group
 */
ErrorCode ReadOBJ::create_new_group ( std::string group_name,
                                       int curr_group, 
                                       EntityHandle &group_meshset )
{
  ErrorCode rval;
  
  // Create meshset to store group
  rval = MBI->create_meshset( MESHSET_SET, group_meshset );
  MB_CHK_SET_ERR(rval,"Failed to generate group mesh set.");

  // Set meshset tags
  rval = MBI->tag_set_data( name_tag, &group_meshset, 1,
                            group_name.c_str());
  MB_CHK_SET_ERR(rval,"Failed to set mesh set name tag.");

  rval = MBI->tag_set_data( id_tag, &group_meshset, 1, &(curr_group));
  MB_CHK_SET_ERR(rval,"Failed to set mesh set ID tag.");


  return rval;
}


/* The create_new_vertex function converts a vector 
   of tokens (v x y z) to 
   the vertex format; a structure that has the three
   coordinates as members.
 */
ErrorCode ReadOBJ::create_new_vertex (std::vector<std::string> v_tokens,
                                      EntityHandle &vertex_eh) 
{
  ErrorCode rval;
  vertex next_vertex;

  for (int i = 1; i < 4; i++)
    next_vertex.coord[i-1] = atof(v_tokens[i].c_str());

  rval = MBI->create_vertex( next_vertex.coord, vertex_eh ); 
  MB_CHK_SET_ERR(rval,"Unbale to create vertex.");

  return rval;
}


/* The create_new_face function converts a vector
   of tokens ( f v1 v2 v3) ) to the face format; 
   a structure that has the three
   connectivity points as members.  
 */
ErrorCode ReadOBJ::create_new_face (std::vector<std::string> f_tokens,
                                       const std::vector<EntityHandle>&vertex_list,
                                       EntityHandle &face_eh) 
{
  face next_face;
  ErrorCode rval;

  for (int i = 1; i < 4; i++)
    {
      int vertex_id = atoi(f_tokens[i].c_str());

      // Some faces contain format 'vertex/texture'
      // Remove the '/texture' and add the vertex to the list
      std::size_t slash = f_tokens[i].find('/');
      if ( slash != std::string::npos )
        {
          std::string face = f_tokens[i].substr(0, slash);
          vertex_id = atoi(face.c_str());
        }

      next_face.conn[i-1] = vertex_list[vertex_id-1];
    }

  rval = MBI->create_element(MBTRI, next_face.conn, 3, face_eh);
  MB_CHK_SET_ERR(rval,"Unable to create new face.");


  return rval;
}


// The split_quad function divides a quad face into 4 tri faces.
ErrorCode ReadOBJ::split_quad(std::vector<std::string> f_tokens,
                                       std::vector<EntityHandle>&vertex_list,
                                       Range &face_eh)
{
  ErrorCode rval;
  std::vector<EntityHandle> quad_vert_eh;

  // Loop over quad connectivity getting vertex EHs 
  for (int i = 1; i < 5; i++)
    {
      int vertex_id = atoi(f_tokens[i].c_str());
      std::size_t slash = f_tokens[i].find('/');
      if ( slash != std::string::npos )
        {
          std::string face = f_tokens[i].substr(0, slash);
          vertex_id = atoi(face.c_str());
        }
 
      quad_vert_eh.push_back(vertex_list[vertex_id-1]);
    }

  // Create new tri faces
  rval = create_tri_faces( quad_vert_eh, face_eh);
  MB_CHK_SET_ERR(rval,"Failed to create triangles when splitting quad.");
  
 
  return rval;
}

ErrorCode ReadOBJ::create_tri_faces( std::vector<EntityHandle> quad_vert_eh, 
//				       EntityHandle center_vertex_eh,
				       Range &face_eh )
{
  ErrorCode rval;
  EntityHandle connectivity[3];
  EntityHandle new_face;

  connectivity[0] = quad_vert_eh[0];
  connectivity[1] = quad_vert_eh[1];
  connectivity[2] = quad_vert_eh[2];
  rval = MBI->create_element(MBTRI,connectivity,3,new_face);
  face_eh.insert(new_face);

  connectivity[0] = quad_vert_eh[2];
  connectivity[1] = quad_vert_eh[3];
  connectivity[2] = quad_vert_eh[0];
  rval = MBI->create_element(MBTRI,connectivity,3,new_face);
  face_eh.insert(new_face);

  return rval;
}


} // namespace moab
  
