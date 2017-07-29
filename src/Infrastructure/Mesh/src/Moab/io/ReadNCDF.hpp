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
// Filename      : ReadNCDF.hpp
//
// Purpose       : ExodusII reader
//
// Special Notes : Lots of code taken from verde implementation
//
// Creator       : Tim Tautges & Corey Ernst
//
// Date          : 3/02
//
// Owner         : Tim Tautges & Corey Ernst
//-------------------------------------------------------------------------

#ifndef READNCDF_HPP
#define READNCDF_HPP

#ifndef IS_BUILDING_MB
//#error "ReadNCDF.hpp isn't supposed to be included into an application"
#endif

#include <vector>
#include <string>

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/ExoIIInterface.hpp"
#include "moab/Range.hpp"

namespace moab {

class ReadUtilIface;

struct ReadBlockData
{
  int blockId;
  int startExoId; 
  EntityHandle startMBId; 
  int numElements;
  bool reading_in;
  ExoIIElementType elemType;
  std::vector<EntityHandle> polys; // used only if elem type is polyhedra or polygons
   // because the order has to be maintained
};

// these are for polyhedra only
struct ReadFaceBlockData
{
  int faceBlockId;
  int startExoId;
  int numElements;
  bool reading_in;
  // ExoIIElementType elemType; should be polygons
};


//! Output Exodus File for VERDE
class ReadNCDF : public ReaderIface
{
   
public:
  
  static ReaderIface* factory( Interface* );
  
  void tokenize( const std::string& str,
                 std::vector<std::string>& tokens,
                 const char* delimiters );

    //! load an ExoII file
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
   ReadNCDF(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadNCDF();

  //update the coords for deformed mesh according to FileOptions
  ErrorCode update(const char *exodus_file_name, const FileOptions& opts,
                     const int num_blocks, const int *blocks_to_load,
                     const EntityHandle file_set );

private:

  ReadUtilIface* readMeshIface;

  bool dimension_exists(const char *attrib_name);
  
  void reset();

    //! read the header from the ExoII file
  ErrorCode read_exodus_header();
  
    //! read the nodes
  ErrorCode read_nodes(const Tag* file_id_tag);
  
  // face blocks for polyhedra
  ErrorCode read_face_blocks_headers(); // all of them?

    //! read block headers, containing info about element type, number, etc.
  ErrorCode read_block_headers(const int *blocks_to_load,
                                  const int num_blocks);
  
  // these are needed only when polyhedra are present
  ErrorCode read_polyhedra_faces();

    //! read the element blocks
  ErrorCode read_elements(const Tag* file_id_tag);
  
    //! read in the global element ids
  ErrorCode read_global_ids();

    //! read the nodesets into meshsets
  ErrorCode read_nodesets();
  
    //! read the sidesets (does nothing for now)
  ErrorCode read_sidesets();

    //! exodus file bound to this object
  int exodus_file();

    //! number of dimensions in this exo file
  int number_dimensions();

  //! map a character exodusII element type to a TSTT type & topology
  ErrorCode get_type(char *exo_element_type,
                        EntityType &elem_type);
 
  ErrorCode get_type(EntityType &elem_type,
                        std::string &exo_element_type);

  /* 
  int get_int_tag(const MB_MeshSet *this_ms,
                  const TagHandle tag_id);
 */

  //qa record stuff 
  ErrorCode read_qa_records(EntityHandle file_set);
  ErrorCode read_qa_information( std::vector<std::string> &qa_record_list);

  ErrorCode read_qa_string(char *string,
                              int record_number,
                              int record_position); 

  ErrorCode create_ss_elements( int *element_ids, int *side_list,
                                   int num_sides, int num_dist_factors,
                                   std::vector<EntityHandle> &entities_to_add,
                                   std::vector<EntityHandle> &reverse_entities,
                                   std::vector<double> &dist_factor_vector,
                                   int ss_seq_id);

  ErrorCode find_side_element_type( const int element_id, ExoIIElementType &type, 
                                       ReadBlockData &block_data, int &df_index, int side_id );
  
 /* ErrorCode assign_block_ids_to_ssets(EntityHandle ss_handle,
                                         MB_MeshSet *ss_mesh_set);
                                         */

  //! creates an element with the given connectivity
  ErrorCode create_sideset_element( const std::vector<EntityHandle>&, EntityType, EntityHandle&, int&);

  int get_number_nodes( EntityHandle handle );



  //------------member variables ------------//

    //! interface instance
  Interface* mdbImpl;
  
  int ncFile;        // netcdf/exodus file

  int CPU_WORD_SIZE;
  int IO_WORD_SIZE;

    //! int to offset vertex ids with
  int vertexOffset;

    //! file name
  std::string exodusFile;

    //! number of nodes in the current exoII file
  int numberNodes_loading;

    //! number of elements in the current exoII file
  int numberElements_loading;

    //! number of blocks in the current exoII file
  int numberElementBlocks_loading; 

  //! number of face blocks in the current exoII file (used for polyhedra)
  int numberFaceBlocks_loading;

    //! number of nodesets in the current exoII file
  int numberNodeSets_loading; 

    //! number of sidesets in the current exoII file
  int numberSideSets_loading; 

  int numberDimensions_loading;

    //! Meshset Handle for the mesh that is currently being read
  EntityHandle mCurrentMeshHandle;

  //keeps track of the exodus ids of the elements and nodes just loaded
  std::vector<char> nodesInLoadedBlocks;
  //note- vector<bool> has limited capabilities

  //vector of blocks that are loading 
  std::vector< ReadBlockData > blocksLoading;

  std::vector<EntityHandle> polyfaces; // the order is maintained with this for polyhedra

  //vector of face blocks that are loading : these are for polyhedra blocks
  std::vector< ReadFaceBlockData > faceBlocksLoading;

  //! Cached tags for reading.  Note that all these tags are defined when the
  //! core is initialized.
  Tag mMaterialSetTag;
  Tag mDirichletSetTag;
  Tag mNeumannSetTag;
  Tag mHasMidNodesTag;
  Tag mDistFactorTag;
  Tag mGlobalIdTag;
  Tag mQaRecordTag;

  int max_line_length, max_str_length;

    //! range of entities in initial mesh, before this read
  Range initRange;
};

// inline functions
inline int ReadNCDF::number_dimensions() 
{
   return numberDimensions_loading;
}

} // namespace moab

#endif




