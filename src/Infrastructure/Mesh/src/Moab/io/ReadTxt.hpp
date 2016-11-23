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



#ifndef READ_TXT_HPP
#define READ_TXT_HPP

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/Range.hpp"

namespace moab {

class ReadUtilIface;

class ReadTxt : public ReaderIface
{
   
public:

    //! factory method 
  static ReaderIface* factory( Interface* );

  ErrorCode load_file( const char* file_name,
                       const EntityHandle* file_set,
                       const FileOptions& opts,
                       const SubsetList* subset_list = 0,
                       const Tag* file_id_tag = 0 );

    //! Constructor
  ReadTxt(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadTxt();

  ErrorCode read_tag_values( const char* file_name,
                             const char* tag_name,
                             const FileOptions& opts,
                             std::vector<int>& tag_values_out,
                             const SubsetList* subset_list = 0 );
  
private:

  ReadUtilIface* readMeshIface;

    //! interface instance
  Interface* mdbImpl;
  
};

} // namespace moab

#endif
