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
 * \class ReadGmsh
 * \brief Gmsh (http://www.geuz.org/gmsh) file reader
 *
 * See: http://geuz.org/gmsh/doc/texinfo/gmsh.html#MSH-ASCII-file-format
 *
 * \author Jason Kraftcheck
 */

#include "ReadTxt.hpp"
#include "FileTokenizer.hpp" // for file tokenizer
#include "Internals.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/Range.hpp"
#include "MBTagConventions.hpp"
#include "MBParallelConventions.h"
#include "moab/CN.hpp"

#include <errno.h>
#include <string.h>
#include <map>
#include <set>

namespace moab {

ReaderIface* ReadTxt::factory( Interface* iface )
  { return new ReadTxt(iface); }

ReadTxt::ReadTxt(Interface* impl)
    : mdbImpl(impl)
{
  mdbImpl->query_interface(readMeshIface);
}

ReadTxt::~ReadTxt()
{
  if (readMeshIface) {
    mdbImpl->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

ErrorCode ReadTxt::load_file( const char* , 
                               const EntityHandle*,
                               const FileOptions& ,
                               const ReaderIface::SubsetList* ,
                               const Tag* )
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode ReadTxt::read_tag_values( const char* /* file_name */,
                                     const char* /* tag_name */,
                                     const FileOptions& /* opts */,
                                     std::vector<int>& /* tag_values_out */,
                                     const SubsetList* /* subset_list */ )
{
  return MB_NOT_IMPLEMENTED;
}


} // namespace moab
