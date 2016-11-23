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


#ifndef WRITE_STL_HPP
#define WRITE_STL_HPP

#include "moab/CartVect.hpp"
#include "moab/Forward.hpp"
#include "moab/WriterIface.hpp"

#include <stdio.h>

namespace moab {

class WriteUtilIface;

/**
 * \brief ASCII and Binary Stereo Lithography File writers.
 * \author Jason Kraftcheck
 *
 * This writer will write only the MBTRI elements in the mesh.  It
 * will not decompose other 2-D elements into triangles, nor will
 * it skin the mesh or do any other high-level operation to generate
 * triangles from 3-D elements.  
 *
 * Binary files will be written with a little-endian byte order by
 * default.  The byte order can be controlled with writer options.
 */
class WriteSTL : public WriterIface
{
 
public:
  
    //! factory method forSTL writer
  static WriterIface* factory( Interface* );

   //! Constructor
  WriteSTL(Interface *impl);

   //! Destructor
  virtual ~WriteSTL();
  
    //! writes out a file
  ErrorCode write_file(const char *file_name,
                         const bool overwrite,
                         const FileOptions& opts,
                         const EntityHandle *output_list,
                         const int num_sets,
                         const std::vector<std::string>& qa_list,
                         const Tag* tag_list = NULL,
                         int num_tags = 0,
                         int export_dimension = 3);  

protected:
  
  enum ByteOrder { STL_BIG_ENDIAN, STL_LITTLE_ENDIAN, STL_UNKNOWN_BYTE_ORDER };
  
    //! Write list of triangles to an STL file.  
  ErrorCode ascii_write_triangles( FILE* file,
                                     const char header[81],
                                     const Range& triangles,
                                     int precision );
    //! Write list of triangles to an STL file.  
  ErrorCode binary_write_triangles( FILE* file,
                                      const char header[81],
                                      ByteOrder byte_order,
                                      const Range& triangles );

    //! Given an array of vertex coordinates for a triangle,
    //! pass back individual point coordinates as floats and 
    //! calculate triangle normal.
  ErrorCode get_triangle_data( const double vtx_coords[9],
                                 float v1[3],
                                 float v2[3],
                                 float v3[3],
                                 float n[3] );

  ErrorCode get_triangle_data( const double vtx_coords[9],
                               CartVect& v1,
                               CartVect& v2,
                               CartVect& v3,
                               CartVect& n);

    //! interface instance
  Interface *mbImpl;
  WriteUtilIface* mWriteIface;
  
private:

    //! Construct 80-byte, null-terminated description string from
    //! qa_list.  Unused space in header will be null-char padded.
  ErrorCode make_header( char header[81], const std::vector<std::string>& qa_list );
  
    //! Get triangles to write from input array of entity sets.  If
    //! no sets, gets all triangles.
  ErrorCode get_triangles( const EntityHandle* set_array,
                             int set_array_length,
                             Range& triangles );  
  
    //! Open a file, respecting passed overwrite value and
    //! subclass-specified value for need_binary_io().
  FILE* open_file( const char* name, bool overwrite, bool binary );
};

} // namespace moab

#endif
