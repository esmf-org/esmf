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

#ifndef READ_VTK_HPP
#define READ_VTK_HPP

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"

#include <string>

namespace moab {

class ReadUtilIface;
class FileTokenizer;

class ReadVtk : public ReaderIface
{
   
public:

  static ReaderIface* factory( Interface* );

    //! load a file
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
  ReadVtk(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadVtk();

protected:

  ErrorCode allocate_vertices( long num_vtx,
                                 EntityHandle& start_handle_out,
                                 double*& x_coord_array_out,
                                 double*& y_coord_array_out,
                                 double*& z_coord_array_out );

  ErrorCode read_vertices( FileTokenizer& tokens,
                             long num_verts, 
                             EntityHandle& start_handle_out );

  ErrorCode allocate_elements( long num_elements,
                                 int vert_per_element,
                                 EntityType type,
                                 EntityHandle& start_handle_out,
                                 EntityHandle*& conn_array_out,
                                 std::vector<Range>& append_to_this );

  ErrorCode vtk_read_dataset( FileTokenizer& tokens,
                                Range& vertex_list,
                                std::vector<Range>& element_list );

  ErrorCode vtk_read_structured_points( FileTokenizer& tokens, 
                                          Range& vertex_list,
                                          std::vector<Range>& elem_list );

  ErrorCode vtk_read_structured_grid( FileTokenizer& tokens, 
                                        Range& vertex_list,
                                        std::vector<Range>& elem_list );

  ErrorCode vtk_read_rectilinear_grid( FileTokenizer& tokens, 
                                         Range& vertex_list,
                                         std::vector<Range>& elem_list );
                                         
  ErrorCode vtk_read_polydata( FileTokenizer& tokens, 
                                 Range& vertex_list,
                                 std::vector<Range>& elem_list );
                                 
  ErrorCode vtk_read_polygons( FileTokenizer& tokens,
                                 EntityHandle first_vtx, 
                                 std::vector<Range>& elem_list );

  ErrorCode vtk_read_unstructured_grid( FileTokenizer& tokens,
                                          Range& vertex_list,
                                          std::vector<Range>& elem_list  );

  ErrorCode vtk_create_structured_elems( const long* dims, 
                                           EntityHandle first_vtx,
                                           std::vector<Range>& elem_list );

  ErrorCode vtk_read_field( FileTokenizer& tokens );

  ErrorCode vtk_read_attrib_data( FileTokenizer& tokens, 
                                    std::vector<Range>& entities );

  ErrorCode vtk_read_tag_data( FileTokenizer& tokens, 
                                 int type, 
                                 size_t per_elem, 
                                 std::vector<Range>& entities,
                                 const char* name );

  ErrorCode vtk_read_scalar_attrib( FileTokenizer& tokens, 
                                      std::vector<Range>& entities,
                                      const char* name);

  ErrorCode vtk_read_color_attrib( FileTokenizer& tokens, 
                                     std::vector<Range>& entities,
                                     const char* name);

  ErrorCode vtk_read_vector_attrib( FileTokenizer& tokens, 
                                      std::vector<Range>& entities,
                                      const char* name);

  ErrorCode vtk_read_texture_attrib( FileTokenizer& tokens,
                                       std::vector<Range>& entities,
                                       const char* name);

  ErrorCode vtk_read_tensor_attrib( FileTokenizer& tokens,
                                      std::vector<Range>& entities,
                                      const char* name );

  ErrorCode vtk_read_field_attrib( FileTokenizer& tokens, 
                                     std::vector<Range>& entities,
                                     const char* name);

  ErrorCode store_file_ids( Tag tag,
                              const Range& vertices,
                              const std::vector<Range>& elements );

private:

  ReadUtilIface* readMeshIface;

  //------------member variables ------------//

    //! interface instance
  Interface* mdbImpl;

    //! A field which, if present and having a single integer for storage, should be used to partition the mesh by range. Defaults to MATERIAL_SET_TAG_NAME
  std::string mPartitionTagName;
};

} // namespace moab

#endif




