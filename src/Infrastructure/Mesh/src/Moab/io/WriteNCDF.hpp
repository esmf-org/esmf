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
// Filename      : WriteNCDF.hpp
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

#ifndef WRITENCDF_HPP
#define WRITENCDF_HPP

#ifndef IS_BUILDING_MB
#error "WriteNCDF.hpp isn't supposed to be included into an application"
#endif

#include <vector>
#include <string>

#include "moab/Forward.hpp"
#include "moab/Range.hpp"
#include "moab/ExoIIInterface.hpp"
#include "moab/WriterIface.hpp"

namespace moab {

class WriteUtilIface;

//! struct used to hold data for each block to be output in Exodus; used by
//! initialize_exodus_file to initialize the file header for increased speed
struct MaterialSetData
{
  int id;
  int number_elements;
  int number_nodes_per_element;
  int number_attributes;
  ExoIIElementType element_type;
  Range elements;
};

//! struct used to hold data for each nodeset to be output in Exodus; used by
//! initialize_exodus_file to initialize the file header for increased speed
struct DirichletSetData
{
  int id;
  int number_nodes;
  std::vector< EntityHandle > nodes;
  std::vector< double > node_dist_factors;
  
};

//! struct used to hold data for each sideset to be output in Exodus; used by
//! initialize_exodus_file to initialize the file header for increased speed
struct NeumannSetData
{
  int id;
  int number_elements;
  std::vector<EntityHandle> elements;
  std::vector<int> side_numbers;
  EntityHandle mesh_set_handle;
  std::vector< double > ss_dist_factors;
};


//! Output Exodus File for VERDE
class WriteNCDF : public WriterIface
{
 
public:
  
  static WriterIface* factory( Interface* );

   //! Constructor
   WriteNCDF(Interface *impl);

   //! Destructor
  virtual ~WriteNCDF();

    //! writes out an ExoII file
  ErrorCode write_file(const char *exodus_file_name,
                         const bool overwrite,
                         const FileOptions& opts,
                          const EntityHandle *output_list,
                          const int num_sets,
                          const std::vector<std::string> &qa_records, 
                          const Tag* = NULL,
                          int = 0,
                          int user_dimension = 3);
  
protected:

    //! number of dimensions in this exo file
  //int number_dimensions();

    //! open an ExoII file for writing
  ErrorCode open_file(const char *file_name);

  //! contains the general information about a mesh
  struct ExodusMeshInfo
  {
    unsigned int num_dim;
    unsigned int num_nodes;
    unsigned int num_elements;
    unsigned int num_elementblocks;
    unsigned int num_polyhedra_blocks;
    std::vector<std::string> qaRecords;
    Range nodes;
    Range polyhedronFaces; // they will accumulate, like nodes
    // they will be written before any other face blocks, and before polyhedra blocks
  };
  
private:

    //! interface instance
  Interface *mdbImpl;
  WriteUtilIface* mWriteIface;
  
    //! file name
  std::string exodusFile;
  int ncFile;

    //! Meshset Handle for the mesh that is currently being read
  EntityHandle mCurrentMeshHandle;

  //! Cached tags for reading.  Note that all these tags are defined when the
  //! core is initialized.
  Tag mMaterialSetTag;
  Tag mDirichletSetTag;
  Tag mNeumannSetTag;
  Tag mHasMidNodesTag;
  Tag mGeomDimensionTag;
  Tag mDistFactorTag;
  Tag mGlobalIdTag;
  Tag mQaRecordTag;

  Tag mEntityMark;   //used to say whether an entity will be exported

  int repeat_face_blocks; // only to make paraview and visit happy

  ErrorCode gather_mesh_information(ExodusMeshInfo &mesh_info,
                                       std::vector<MaterialSetData> &block_info,
                                       std::vector<NeumannSetData> &sideset_info,
                                       std::vector<DirichletSetData> &nodeset_info,
                                       std::vector<EntityHandle> &blocks,
                                       std::vector<EntityHandle> &sidesets,
                                       std::vector<EntityHandle> &nodesets);

  ErrorCode write_header(ExodusMeshInfo& mesh_info,
                            std::vector<MaterialSetData> &block_info,
                            std::vector<NeumannSetData> &sideset_info,
                            std::vector<DirichletSetData> &nodeset_info,
                            const char *filename);

  ErrorCode initialize_exodus_file(ExodusMeshInfo &mesh_info,
                                      std::vector<MaterialSetData> &block_data,
                                      std::vector<NeumannSetData> & sideset_data,
                                      std::vector<DirichletSetData> & nodeset_data,
                                      const char* title_string,
                                      bool write_maps = true,
                                      bool write_sideset_distribution_factors = true);

  ErrorCode write_qa_string(const char *string,
                               int record_number,
                               int record_position);

  ErrorCode write_qa_records( std::vector<std::string> &qa_record_list);

  ErrorCode write_nodes(int num_nodes, Range& nodes, int dimension );

  ErrorCode write_poly_faces(ExodusMeshInfo& mesh_info);
  ErrorCode write_elementblocks(ExodusMeshInfo& mesh_info, std::vector<MaterialSetData> &block_data );

  ErrorCode write_exodus_integer_variable(const char* variable_name,
                                                      int *variable_array,
                                                      int start_position,
                                                      int number_values);

  ErrorCode write_global_node_order_map(int num_nodes, Range& nodes);
  ErrorCode write_global_element_order_map(int num_elements);
  ErrorCode write_element_order_map(int num_elements);

  ErrorCode write_BCs(std::vector<NeumannSetData> &sidesets,
                         std::vector<DirichletSetData> &nodesets);

  ErrorCode find_side_element_type( const int element_id, ExoIIElementType &type );

 /* ErrorCode assign_block_ids_to_ssets(EntityHandle ss_handle,
                                         MB_MeshSet *ss_mesh_set);
                                         */
  //! free up allocated Ranges
  void reset_block(std::vector<MaterialSetData> &block_info);

    //! recursive function; given a meshset handle, get the entities and put them
    //! on the right list, then call recursively for any contained meshsets, first
    //! checking for sense reversals
  ErrorCode get_sideset_elems(EntityHandle sideset, int current_sense,
                                 Range &forward_elems, Range &reverse_elems);
  

  ErrorCode get_valid_sides(Range &elems, ExodusMeshInfo &mesh_info, 
                               const int sense,
                               NeumannSetData &sideset_data);
  
    //! get the time and date in strings
  static void time_and_date(char* time_string, char* date_string);
};

} // namespace moab

#endif
