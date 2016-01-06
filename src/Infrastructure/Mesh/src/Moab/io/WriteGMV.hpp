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

//-------------------------------------------------------------------------
// Filename      : WriteGMV.hpp
//
// Purpose       : Writer template
//
// Special Notes : 
//
// Creator       : Tim Tautges
//
// Date          : 2/04
//
//-------------------------------------------------------------------------

#ifndef WRITEGMV_HPP
#define WRITEGMV_HPP

#include "moab/Forward.hpp"
#include "moab/WriterIface.hpp"

namespace moab {

class WriteUtilIface;

//! Output Exodus File for VERDE
class WriteGMV : public WriterIface
{
 
public:

   //! Constructor
   WriteGMV(Interface *impl);

   //! Destructor
  virtual ~WriteGMV();

  static WriterIface* factory( Interface* );

  ErrorCode write_file( const char* filename,
                          const bool overwite,
                          const FileOptions& opts,
                          const EntityHandle* output_sets,
                          const int num_output_sets,
                          const std::vector<std::string>& qa_list,
                          const Tag* tag_list = NULL,
                          int num_tags = 0,
                          int requested_dimension = 3);

    //! writes out a mesh file
  ErrorCode write_file(const char *file_name,
                         const EntityHandle output_set,
                         const int user_dimension = 3,
                         const bool mesh = true,
                         const bool poly_mesh = true);
  
protected:

private:

    //! interface instance
  Interface *mbImpl;
  WriteUtilIface* mWriteIface;
  
    //! Meshset Handle for the mesh that is currently being written
  EntityHandle mCurrentMeshHandle;

  //! Cached tags for reading.  Note that all these tags are defined when the
  //! core is initialized.
  Tag mMaterialSetTag;
  Tag mDirichletSetTag;
  Tag mNeumannSetTag;
  Tag mHasMidNodesTag;
  Tag mGeomDimensionTag;
  Tag mGlobalIdTag;

  static const char *gmvTypeNames[MBMAXTYPE];
  
  ErrorCode local_write_mesh(const char *file_name,
                               const EntityHandle output_set,
                               const int user_dimension,
                               const bool mesh,
                               const bool poly_mesh);
};

} // namespace moab

#endif
