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


#ifndef WRITE_VTK_HPP
#define WRITE_VTK_HPP

#include <iosfwd>

#include "moab/Forward.hpp"
#include "moab/WriterIface.hpp"

namespace moab {

class WriteUtilIface;

//class MB_DLL_EXPORT WriteVtk : public WriterIface
class WriteVtk : public WriterIface
{
 
public:

   //! Constructor
   WriteVtk(Interface *impl);

   //! Destructor
  virtual ~WriteVtk();
  
  static WriterIface* factory( Interface* );

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

private:

    //! Get entities to write, given set list passed to \ref write_file
  ErrorCode gather_mesh( const EntityHandle* set_list,
                           int num_sets, 
                           Range& nodes,
                           Range& elems );
    
    //! Write 4-line VTK file header
  ErrorCode write_header( std::ostream& stream );
  
    //! Write node coordinates
  ErrorCode write_nodes( std::ostream& stream, const Range& nodes );
  
    //! Write element connectivity
  ErrorCode write_elems( std::ostream& stream, const Range& nodes, const Range& elems );
  
    //! Write all tags on either the list of nodes or the list of elements
  ErrorCode write_tags( std::ostream& stream, bool nodes, const Range& entities,
                          const Tag* tag_list, int num_tags );
  
    //! Write the tad description for the passed tag and call the template
    //! \ref write_tag function to write the tag data.
  ErrorCode write_tag( std::ostream& stream, Tag tag, const Range& entities, const Range& tagged_entities );
  
    //! Write tag data
  template <typename T> 
  ErrorCode write_tag( std::ostream& stream, Tag tag, const Range& entities, const Range& tagged_entities,
                         const int);

  ErrorCode write_bit_tag( std::ostream& stream, Tag tag, const Range& entities, const Range& tagged_entities );
    //! Write a list of values
  template <typename T>
  void write_data( std::ostream& stream, const std::vector<T>& data, unsigned vals_per_tag );

  Interface* mbImpl;
  WriteUtilIface* writeTool;
 
  bool mStrict; // If true, do not write data that cannot fit in strict VTK file format.
  
};

} // namespace moab

#endif
