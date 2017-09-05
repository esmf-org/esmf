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

//-----------------------------------------------------------------------------
// Filename : ReadOBJ.hpp
//
// Purpose  : Wavefront obj file reader
//
// Creators : Chelsea D'Angelo, Paul Wilson, Andrew Davis
//
// Date     : 02/16
//
// Owner    : Chelsea D'Angelo
//-----------------------------------------------------------------------------


/**
 * This class will read in an obj file and populate a MOAB instance with the
 * vertex and connectivity information in the file.  A specification for obj files
 * can be found here: https://en.wikipedia.org/wiki/Wavefront_.obj_file
 * This reader only supports a subset of the full file structure, namely, 
 * object names, group names, vertices, and faces.
 *
 * Overview of the supported structure:
 *
 * Heading
 * Object line: o object_name
 * Group line: g group_name1
 * Vertex lines: v x y z
 * Face lines (tri): f v1 v2 v3
 * Face lines (quad): f v1 v2 v3 v4
 *
 * Lines that begin w/ anything other than 'o ', 'g ', 'v ', and 'f ' are not
 * supported. If a valid, but unsupported line is found, it will be ignored.
 * If an invalid line is found, an error will be produced.
 * Face lines that contain 'vertex\texture\normal' are handled by ignoring the
 * texture and normal
 * 
 * 
 * A new meshset will be created for each object or group in the file.
 * Each object and group must have a name, or the line will be ignored.  
 * Groups are thought to be collections of elements with no dimension or category and
 * will therefore only be assigned name and id tags.
 * Objects are thought to be closed surfaces.  A surface meshset will be
 * created for each object.  A volume meshset that directly corresponds
 * to the surface meshset will also be created.  These will have name, id,
 * categorty, and dimension tags.  A parent-child relationship exists
 * between the volume and surface meshsets.
 * Vertices will be created and added to a global vertex meshset.
 * Triangular faces will be created and added as members of the surface meshets.
 */


#ifndef READ_OBJ_HPP
#define READ_OBJ_HPP     
                                     
#ifndef IS_BUILDING_MB                   
  #error "ReadOBJ.hpp isn't supposed to be included into an application"
#endif   

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>

#include "moab/Interface.hpp"
#include "moab/ReaderIface.hpp"
#include "FileTokenizer.hpp"
#include "moab/RangeMap.hpp"
#include "MBTagConventions.hpp"

/* struct vertex is a structure that stores coordinates
   of vertices. This is a convenience structure making 
   i/o easier.
*/
struct vertex {
  int vertex_id;
  double coord[3];
};

/* struct face is a structure that stores connectivity.
   This is a convenience structure makin i/o easier.
*/
struct face {
  int face_id;
  moab::EntityHandle conn[3];
};
namespace moab {

/* Supported obj file keywords
 */
enum keyword_type {obj_undefined = 0,
                   object_start,
                   group_start,
                   face_start,
                   vertex_start,
                   valid_unsupported};

class ReadUtilIface;
class GeomTopoTool;

class ReadOBJ : public ReaderIface
{
   
public:

  //! factory method 
  static ReaderIface* factory( Interface* );

  ErrorCode load_file( const char* file_name,
                       const EntityHandle* file_set,
                       const FileOptions& opts,
                       const SubsetList* subset_list = 0,
                       const Tag* file_id_tag = 0 );

  ErrorCode read_tag_values( const char* file_name,
                             const char* tag_name,
                             const FileOptions& opts,
                             std::vector<int>& tag_values_out,
                             const SubsetList* subset_list = 0 );
 

 
  //! Constructor
  ReadOBJ(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadOBJ();

private:
  ReadUtilIface* readMeshIface;

  //! interface instance
  Interface* MBI;

  GeomTopoTool* myGeomTool;

  Tag geom_tag,id_tag,name_tag,category_tag,faceting_tol_tag, geometry_resabs_tag, obj_name_tag, 
    sense_tag;

  /*  The keyword type function matches the first character extracted from each line to a type of line
   */ 
  keyword_type get_keyword(std::vector<std::string> tokens);
  
  /*  The match function searches a list of map keys for a match with the token
   */
  template <typename T>
  std::string match(const std::string &token, std::map<std::string, T> &tokenList);
 

  /* The tokenize function takes a string as input and splits it into
   * a vector of strings based on the delimiter
   */
  static const char* delimiters;

  void tokenize( const std::string& str, std::vector<std::string>& tokens,
                 const char* delimiters );
  
  /*
   * The create_object funtion will create a new meshset for
   * each object that contains all vertices and faces 
   */
  ErrorCode create_new_object(std::string object_name, 
                              int object_id,
                              EntityHandle &curr_obj_meshset);
  
  ErrorCode create_new_group ( std::string object_name,
                                       int curr_object, 
                                       EntityHandle &object_meshset );

  /* create_new_vertex converts tokenized string input to 
     vertex structure
   */
  ErrorCode create_new_vertex (std::vector<std::string> v_tokens,
                                      EntityHandle &vertex_eh); 

  /* create_new_face converts tokenized string input to 
   * face structure
   */
  ErrorCode create_new_face (std::vector<std::string> f_tokens,
                                       const std::vector<EntityHandle>&vertex_list,
                                       EntityHandle &face_eh); 
 

  /*
   * The split_quad function creates 1 new vertex and 4 new tri faces
   * from a quad face.  
   */

  ErrorCode split_quad(std::vector<std::string> f_tokens,
                                       std::vector<EntityHandle>&vertex_list,
                                       Range &face_eh);

 ErrorCode create_tri_faces( std::vector<EntityHandle> quad_vert_eh, 
//				       EntityHandle center_vertex_eh,
				       Range &face_eh );

};

} // namespace moab



#endif
