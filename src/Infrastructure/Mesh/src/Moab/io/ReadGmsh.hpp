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



#ifndef READ_GMSH_HPP
#define READ_GMSH_HPP

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/Range.hpp"

namespace moab {

class ReadUtilIface;
struct GmshElemType;

/**
 * \brief Gmsh (http://www.geuz.org/gmsh) file reader
 * \author Jason Kraftcheck
 */
class ReadGmsh : public ReaderIface
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
  ReadGmsh(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadGmsh();

private:

  ErrorCode create_elements( const GmshElemType& type,
                               const std::vector<int>& elem_ids,
                               const std::vector<int>& matl_ids,
                               const std::vector<int>& geom_ids,
                               const std::vector<int>& prtn_ids,
                               const std::vector<EntityHandle>& connectivity,
                               const Tag* file_id_tag );

  ErrorCode create_sets( EntityType element_type,
                           const Range& elements,
                           const std::vector<int>& set_ids,
                           int set_type );
  
  ErrorCode create_geometric_topology();
  

  ReadUtilIface* readMeshIface;

    //! interface instance
  Interface* mdbImpl;
  
  Tag globalId;
  Range geomSets;
};

} // namespace moab

#endif
