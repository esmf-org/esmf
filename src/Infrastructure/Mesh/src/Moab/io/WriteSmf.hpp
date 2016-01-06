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


#ifndef WRITE_SMF_HPP
#define WRITE_SMF_HPP

#include <iosfwd>

#include "moab/Forward.hpp"
#include "moab/WriterIface.hpp"

namespace moab {

class WriteUtilIface;

class WriteSmf : public WriterIface
{
 
public:

   //! Constructor
   WriteSmf(Interface *impl);

   //! Destructor
  virtual ~WriteSmf();
  
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
     
    Interface* mbImpl;
    WriteUtilIface* writeTool;
};

} // namespace moab

#endif
