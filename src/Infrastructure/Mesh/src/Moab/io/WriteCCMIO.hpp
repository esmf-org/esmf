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
// Filename      : WriteCCMIO.hpp
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

#ifndef WRITECCMIO_HPP
#define WRITECCMIO_HPP

#ifndef IS_BUILDING_MB
#error "WriteCCMIO.hpp isn't supposed to be included into an application"
#endif

#include <vector>
#include <string>

#include "moab/Forward.hpp"
#include "moab/Range.hpp"
#include "moab/ExoIIInterface.hpp"
#include "moab/WriterIface.hpp"
#include "ccmio.h"

namespace moab {

class WriteUtilIface;

class WriteCCMIO : public WriterIface
{
 
public:

   //! Constructor
   WriteCCMIO(Interface *impl);

   //! Destructor
  virtual ~WriteCCMIO();
  
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
  
protected:

    //! number of dimensions in this file
  //int number_dimensions();

    //! open a file for writing
  ErrorCode open_file(const char *filename, bool overwrite, CCMIOID &rootID);

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

    // material set information
  class MaterialSetData
  {
  public:
    Range elems; // elements in material set
    EntityHandle setHandle; // handle of the material set
    EntityType entityType; // entity type of these elements
    int verts_per_element; // number of vertices in each element
    int matsetId; // id of this matset, from MATERIAL_SET tag
    int materialId; // materialid, if any (from CCMIO)
    std::string setName; // name for this matset, if any
    std::string materialType; // material type for this matset, if any

    MaterialSetData() 
            : setHandle(0), entityType(MBMAXTYPE), verts_per_element(0), matsetId(-1),
              materialId(-1)
    
        {}
  };
  
    // neumann set information
  class NeumannSetData
  {
  public:
    Range elems; // elements in neumann set
    EntityHandle setHandle; // handle of the neumann set
    EntityType entityType; // entity type of these elements
    int verts_per_element; // number of vertices in each element
    int neusetId; // id of this matset, from NEUMANN_SET tag
    std::string setName; // name for this neuset, if any

    NeumannSetData() 
            : setHandle(0), entityType(MBMAXTYPE), verts_per_element(0), neusetId(-1)
        {}
  };
  
private:

    //! interface instance
  Interface *mbImpl;
  WriteUtilIface* mWriteIface;
  
    //! file name
  std::string fileName;

    //! Meshset Handle for the mesh that is currently being read
  EntityHandle mCurrentMeshHandle;

  //! Cached tags for reading.  Note that all these tags are defined when the
  //! core is initialized.
  Tag mMaterialSetTag;
  Tag mDirichletSetTag;
  Tag mNeumannSetTag;
  Tag mPartitionSetTag;
  Tag mHasMidNodesTag;
  Tag mGlobalIdTag;
  Tag mNameTag, mMaterialIdTag, mMaterialTypeTag;
  Tag mRadiationTag, mPorosityIdTag, mSpinIdTag, mGroupIdTag, mColorIdxTag,
      mProcessorIdTag, mLightMaterialTag, mFreeSurfaceMaterialTag;
  Tag mThicknessTag, mProstarRegionNumberTag, mBoundaryTypeTag, mCreatingProgramTag;

  Tag mEntityMark;   //used to say whether an entity will be exported

  int mDimension; // dimension of entities being exported

  bool mWholeMesh; // if true, whole mesh is being output

    //! gathers elements in each matset, and all the vertices used by them;
    //! marks the vertices with the mEntityMark bit flag
  ErrorCode gather_matset_info(std::vector<EntityHandle> &matsets,
                               std::vector<MaterialSetData> &matset_data,
                               Range &all_verts);
  
    //! gathers elements in each neuset
  ErrorCode gather_neuset_info(std::vector<EntityHandle> &neusets,
                               std::vector<NeumannSetData> &neuset_data);
  
  ErrorCode close_and_compress(const char *filename, CCMIOID rootID);
    
  ErrorCode initialize_file(MeshInfo &mesh_info);

    //! write vertices to file
  ErrorCode write_nodes(CCMIOID rootID, const Range& nodes, const int dimension, CCMIOID &verticesID);
  
    //! write cells and internal/boundary faces, using vgids and verts input
  ErrorCode write_cells_and_faces(CCMIOID rootID, 
                                  std::vector<WriteCCMIO::MaterialSetData> &matset_data,
                                  std::vector<WriteCCMIO::NeumannSetData> &neuset_data,
                                  Range &verts, CCMIOID &topologyID);

    //! write external faces, including connectivity and connected cells
  ErrorCode write_external_faces(CCMIOID rootID, CCMIOID topologyID, int set_num, Range &facets);
  
    // get global ids for these entities; allocates gids and passes back,
    // caller is responsible for deleting
  ErrorCode get_gids(const Range &ents, int *&gids,
                       int &minid, int &maxid);
  
  ErrorCode write_meshes(MeshInfo &mesh_info, 
                            std::vector<MaterialSetData> &matset_data,
                            std::vector<NeumannSetData> &neuset_data,
                            Range &verts,
                            const int *vgids);
  
  ErrorCode get_valid_sides(Range &elems, const int sense,
                              WriteCCMIO::NeumannSetData &neuset_data);
  
  void reset_matset(std::vector<MaterialSetData> &matset_info);
  
  ErrorCode get_neuset_elems(EntityHandle neuset, int current_sense,
                               Range &forward_elems, Range &reverse_elems);
  
  ErrorCode transform_coords(const int dimension, const int num_nodes, double *coords);

  ErrorCode write_problem_description(CCMIOID rootID, CCMIOID stateID, CCMIOID &problemID,
                                      CCMIOID processorID,
                                      std::vector<MaterialSetData> &matset_data,
                                      std::vector<NeumannSetData> &neuset_data);

    // get the material, dirichlet, neumann, and partition sets to be written,
    // either from input sets or in the whole mesh
  ErrorCode get_sets(const EntityHandle *ent_handles,
                     int num_sets,
                     std::vector<EntityHandle> &matsets,
                     std::vector<EntityHandle> &dirsets,
                     std::vector<EntityHandle> &neusets,
                     std::vector<EntityHandle> &partsets);

    //! create state and processor nodes
  ErrorCode create_ccmio_structure(CCMIOID rootID, CCMIOID &stateID,
                                   CCMIOID &processorID);

    //! write solution (tag) data
  ErrorCode write_solution_data();
  
    //! finalize processor
  ErrorCode write_processor(CCMIOID processorID, CCMIOID verticesID, CCMIOID topologyID);

    //! convert MOAB to CCMIO type
  int moab_to_ccmio_type(EntityType etype, int has_mid_nodes[]);

  ErrorCode write_int_option(const char *opt_name,
                             EntityHandle seth,
                             Tag &tag, CCMIOID &node);
  
  ErrorCode write_dbl_option(const char *opt_name,
                             EntityHandle seth,
                             Tag &tag, CCMIOID &node);

  ErrorCode write_str_option(const char *opt_name,
                             EntityHandle seth,
                             Tag &tag, CCMIOID &node,
                             const char *other_name = NULL);
};

} // namespace moab

#endif
