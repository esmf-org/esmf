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

//-------------------------------------------------------------------------
// Filename      : WriteTemplate.hpp
//
// Purpose       : ExodusII writer
//
// Special Notes : Lots of code taken from verde implementation
//
// Creator       : Corey Ernst 
//
// Date          : 8/02
//
// Owner         : Corey Ernst 
//-------------------------------------------------------------------------

#ifndef WRITETemplate_HPP
#define WRITETemplate_HPP

#ifndef IS_BUILDING_MB
#error "WriteTemplate.hpp isn't supposed to be included into an application"
#endif

#include <vector>
#include <string>

#include "moab/Forward.hpp"
#include "moab/Range.hpp"
#include "moab/ExoIIInterface.hpp"
#include "moab/WriterIface.hpp"

namespace moab {

class WriteUtilIface;

class WriteTemplate : public WriterIface
{
 
public:

   //! Constructor
   WriteTemplate(Interface *impl);

   //! Destructor
  virtual ~WriteTemplate();
  
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
  
//! struct used to hold data for each block to be output; used by
//! initialize_file to initialize the file header for increased speed
  struct MaterialSetData
  {
    int id;
    int number_elements;
    int number_nodes_per_element;
    int number_attributes;
    ExoIIElementType element_type;
    EntityType moab_type;
    Range *elements;
  };

//! struct used to hold data for each nodeset to be output; used by
//! initialize_file to initialize the file header for increased speed
  struct DirichletSetData
  {
    int id;
    int number_nodes;
    std::vector< EntityHandle > nodes;
    std::vector< double > node_dist_factors;
  
  };

//! struct used to hold data for each sideset to be output; used by
//! initialize_file to initialize the file header for increased speed
  struct NeumannSetData
  {
    int id;
    int number_elements;
    std::vector<EntityHandle> elements;
    std::vector<int> side_numbers;
    EntityHandle mesh_set_handle;
  };


protected:

    //! number of dimensions in this file
  //int number_dimensions();

    //! open a file for writing
  ErrorCode open_file(const char *filename);

  //! contains the general information about a mesh
  class MeshInfo
  {
  public:
    unsigned int num_dim;
    unsigned int num_nodes;
    unsigned int num_elements;
    unsigned int num_matsets;
    unsigned int num_dirsets;
    unsigned int num_neusets;
    Range nodes;

    MeshInfo() 
        : num_dim(0), num_nodes(0), num_elements(0), num_matsets(0), 
          num_dirsets(0), num_neusets(0)
      {}
    
  };
  
private:

    //! interface instance
  Interface *mbImpl;
  WriteUtilIface* mWriteIface;
  
    //! file name
  std::string fileName;

  //! Cached tags for reading.  Note that all these tags are defined when the
  //! core is initialized.
  Tag mMaterialSetTag;
  Tag mDirichletSetTag;
  Tag mNeumannSetTag;
  Tag mGlobalIdTag;

  Tag mEntityMark;   //used to say whether an entity will be exported

  ErrorCode gather_mesh_information(MeshInfo &mesh_info,
                                      std::vector<MaterialSetData> &matset_info,
                                      std::vector<NeumannSetData> &neuset_info,
                                      std::vector<DirichletSetData> &dirset_info,
                                      std::vector<EntityHandle> &matsets,
                                      std::vector<EntityHandle> &neusets,
                                      std::vector<EntityHandle> &dirsets);
  
  ErrorCode initialize_file(MeshInfo &mesh_info);

  ErrorCode write_nodes(const int num_nodes, const Range& nodes, 
                          const int dimension );

  ErrorCode write_matsets(MeshInfo &mesh_info, 
                            std::vector<MaterialSetData> &matset_data,
                            std::vector<NeumannSetData> &neuset_data);
  
  ErrorCode get_valid_sides(Range &elems, const int sense,
                              WriteTemplate::NeumannSetData &neuset_data);
  
  void reset_matset(std::vector<MaterialSetData> &matset_info);
  
  ErrorCode get_neuset_elems(EntityHandle neuset, int current_sense,
                               Range &forward_elems, Range &reverse_elems);
  
};

} // namespace moab

#endif
