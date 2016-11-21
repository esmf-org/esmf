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

/*
 * Tim's Quick 'N Dirty Cub File Reader (Tqdcfr)
 *
 */

#ifndef TQDCFR
#define TQDCFR

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include "MBTagConventions.hpp"
#include "moab/Range.hpp"

#include <stdio.h>
#include <string>
#include <vector>
#include <map>

namespace moab {

class ReadUtilIface;
class FEModelHeader;
class GeomHeader;
class GroupHeader;
class BlockHeader;
class NodesetHeader;
class SidesetHeader;


class Tqdcfr : public ReaderIface
{
public:  

  void FSEEK( unsigned offset );        // set cubFile offset to specified value
  void FREADI( unsigned num_ents ); // read integers into uint_buf
  void FREADD( unsigned num_ents ); // read doubles into dbl_buf
  void FREADC( unsigned num_ents ); // read characters into char_buf
  void FREADIA( unsigned num_ents,    unsigned int* array ); // read integers
  void FREADDA( unsigned num_ents, double* array ); // read doubles
  void FREADCA( unsigned num_ents,   char* arrat ); // read bytes
  void CONVERT_TO_INTS(unsigned int num_ents); // convert uint_buf to int_buf in-place

    // class for holding the file table of contents
  class FileTOC
  {
  public:
    unsigned int fileEndian, fileSchema, numModels, modelTableOffset, 
      modelMetaDataOffset, activeFEModel;

    FileTOC();
    void print();
  };

    // 
  class FEModelHeader 
  {
  public:
    unsigned int feEndian, feSchema, feCompressFlag, feLength;

    class ArrayInfo 
    {
    public:
      unsigned numEntities, tableOffset, metaDataOffset;

      ArrayInfo();
      
      void print();
      void init(const std::vector<unsigned int>& uint_buf_in);
    };
    
    ArrayInfo geomArray, nodeArray, elementArray, groupArray, 
      blockArray, nodesetArray, sidesetArray;

    void init(const unsigned int offset, Tqdcfr* instance );
        
    void print();
  };

  class MetaDataContainer
  {
  public:
    unsigned int mdSchema, compressFlag;

    class MetaDataEntry
    {
    public:
      unsigned int mdOwner, mdDataType, mdIntValue;
      std::string mdName, mdStringValue;
      std::vector<unsigned int> mdIntArrayValue;
      double mdDblValue;
      std::vector<double> mdDblArrayValue;
      
      MetaDataEntry();

      void print();
    };

    void print();

    int get_md_entry(const unsigned int owner, const std::string &name);
    
    std::vector<MetaDataEntry> metadataEntries;
    MetaDataContainer();
  };

  class GeomHeader 
  {
  public:
    unsigned int geomID, nodeCt, nodeOffset, elemCt, elemOffset, 
      elemTypeCt, elemLength;

    int maxDim;
    
    EntityHandle setHandle;

    void print();

    static ErrorCode read_info_header(const unsigned int model_offset, 
                                        const FEModelHeader::ArrayInfo &info,
                                        Tqdcfr* instance,
                                        GeomHeader *&entity_headers);

    GeomHeader();
  };
  
  class GroupHeader 
  {
  public:
    unsigned int grpID, grpType, memCt, memOffset, memTypeCt, grpLength;

    EntityHandle setHandle;

    void print();

    static ErrorCode read_info_header(const unsigned int model_offset, 
                                 const FEModelHeader::ArrayInfo &info,
                                 Tqdcfr* instance,
                                 GroupHeader *&entity_headers);

    GroupHeader();
  };
  
  class BlockHeader 
  {
  public:
    unsigned int blockID, blockElemType, memCt, memOffset, memTypeCt, attribOrder, blockCol,
      blockMixElemType, blockPyrType, blockMat, blockLength, blockDim;

    EntityHandle setHandle;

    EntityType blockEntityType;

    int hasMidNodes[4];

    void print();

    static ErrorCode read_info_header(const double data_version,
                                        const unsigned int model_offset, 
                                        const FEModelHeader::ArrayInfo &info,
                                        Tqdcfr* instance,
                                        BlockHeader *&block_headers);

    BlockHeader();
  };
  
  class NodesetHeader 
  {
  public:
    unsigned int nsID, memCt, memOffset, memTypeCt, pointSym, nsCol, nsLength;

    EntityHandle setHandle;

    void print();

    static ErrorCode read_info_header(const unsigned int model_offset, 
                                 const FEModelHeader::ArrayInfo &info,
                                 Tqdcfr* instance,
                                 NodesetHeader *&entity_headers);

    NodesetHeader();
  };
  
  class SidesetHeader 
  {
  public:
    unsigned int ssID, memCt, memOffset, memTypeCt, numDF, ssCol, useShell, ssLength;

    EntityHandle setHandle;

    void print();

    static ErrorCode read_info_header(const unsigned int model_offset, 
                                 const FEModelHeader::ArrayInfo &info,
                                 Tqdcfr* instance,
                                 SidesetHeader *&entity_headers);

    SidesetHeader();
  };
  
    // class to hold model entry data for various kinds of models 
    // (acis, free mesh, etc.)
  class ModelEntry
  {
  public:
    ModelEntry();

    ~ModelEntry();
    
    unsigned int modelHandle, modelOffset, modelLength, modelType, modelOwner, modelPad;

    FEModelHeader feModelHeader;
    GeomHeader *feGeomH;
    GroupHeader *feGroupH;
    BlockHeader *feBlockH;
    NodesetHeader *feNodeSetH;
    SidesetHeader *feSideSetH;
    
    MetaDataContainer geomMD, nodeMD, elementMD, groupMD, blockMD, nodesetMD, sidesetMD;
    
    void print();

    void print_geom_headers(const char *prefix,
                            GeomHeader *header,
                            unsigned int num_headers);

    void print_group_headers(const char *prefix,
                             GroupHeader *header,
                             const unsigned int num_headers);

    void print_block_headers(const char *prefix,
                             BlockHeader *header,
                             const unsigned int num_headers);

    void print_nodeset_headers(const char *prefix,
                               NodesetHeader *header,
                               const unsigned int num_headers);

    void print_sideset_headers(const char *prefix,
                               SidesetHeader *header,
                               const unsigned int num_headers);
    
    ErrorCode read_header_info( Tqdcfr* instance, const double data_version);
    ErrorCode read_metadata_info(Tqdcfr *tqd);
  };

  enum {aBODY, LUMP, SHELL, FACE, LOOP, COEDGE, aEDGE, aVERTEX, ATTRIB, UNKNOWN};
  
  
  struct AcisRecord 
  {
    unsigned int rec_type;
    std::string att_string;
    bool processed;
    int first_attrib;
    int att_prev, att_next, att_ent_num;
    EntityHandle entity;
  };

  ~Tqdcfr();

  ReadUtilIface *readUtilIface;
  Interface *mdbImpl;
  FILE *cubFile;
  FileTOC fileTOC;
  std::vector<ModelEntry> modelEntries;
  MetaDataContainer modelMetaData;
  long currVHandleOffset;
  Range beforeEnts;
  long currElementIdOffset[MBMAXTYPE];
  Tag globalIdTag, cubIdTag, geomTag, uniqueIdTag, blockTag, nsTag, ssTag,
    attribVectorTag, entityNameTag, categoryTag, hasMidNodesTag;
  std::map<int, EntityHandle> uidSetMap;
  std::map<int, EntityHandle> gidSetMap[6];
  bool swapForEndianness;

  std::vector<unsigned int> uint_buf;
  int *int_buf;
  std::vector<double> dbl_buf;
  std::vector<char> char_buf;

  static ReaderIface* factory( Interface* );
  
    // read cub file
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
                               
  ErrorCode read_nodeset(const unsigned int nsindex,
                         ModelEntry *model,
                         NodesetHeader *nodeseth);
  ErrorCode read_sideset(const unsigned int ssindex,
                         const double data_version,
                         ModelEntry *model,
                         SidesetHeader *sideseth);
  ErrorCode read_block(const unsigned int blindex,
                       const double data_version,
                       ModelEntry *model,
                       BlockHeader *blockh);
  ErrorCode read_group(const unsigned int gr_index,
                         ModelEntry *model,
                         GroupHeader *grouph);
  ErrorCode read_nodes(const unsigned int gindex,
                  ModelEntry *model,
                  GeomHeader *entity);
  ErrorCode read_elements(ModelEntry *model,
                     GeomHeader *entity);
  ErrorCode read_file_header();
  ErrorCode read_model_entries();
  int find_model(const unsigned int model_type);
  ErrorCode read_meta_data(const unsigned int metadata_offset, 
                      MetaDataContainer &mc);
  ErrorCode read_md_string(std::string &name);
  
  enum {mesh, acist, acisb, facet, exodusmesh};
  EntityType type_from_cub_type(const unsigned int cub_type, const unsigned int nodes_per_elem);
  void check_contiguous(const unsigned int num_ents, int &contig, 
                        unsigned int &min_id, unsigned int &max_id);

  Tqdcfr(Interface *impl);

  ErrorCode create_set( EntityHandle& h, unsigned int flags = MESHSET_SET );

private:

  EntityHandle mFileSet; // set containing read entities.

  bool printedSeqWarning; // only print acis sequence #'s warning once
  
  bool printedElemWarning; // only print element #'s warning once
  
  ErrorCode convert_nodesets_sidesets();

  ErrorCode read_acis_records( const char* sat_file_name = 0 );
  
  ErrorCode parse_acis_attribs(const unsigned int entity_rec_num,
                          std::vector<AcisRecord> &records);
  ErrorCode interpret_acis_records(std::vector<AcisRecord> &records);

  ErrorCode reset_record(AcisRecord &this_record);
  
  ErrorCode process_record(AcisRecord &this_record);
  
  static const char geom_categories[][CATEGORY_TAG_SIZE];
  
  FILE* acisDumpFile;

    // map between cub ids and MOAB handles
  std::vector<EntityHandle> *cubMOABVertexMap;

    // enum used to identify element/entity type in groups
  enum {GROUP = 0, BODY, VOLUME, SURFACE, CURVE, VERTEX, HEX, TET, PYRAMID, QUAD, TRI, EDGE, NODE};
  static const EntityType group_type_to_mb_type[];

  enum {SPHERE_EXO=0,
        BAR, BAR2, BAR3,
        BEAM, BEAM2, BEAM3,
        TRUSS, TRUSS2, TRUSS3,
        SPRING,
        TRIthree, TRI3, TRI6, TRI7,
        TRISHELL, TRISHELL3, TRISHELL6, TRISHELL7,
        SHEL, SHELL4, SHELL8, SHELL9,
        QUADfour, QUAD4, QUAD5, QUAD8, QUAD9,
        TETRAfour, TETRA4, TETRA8, TETRA10, TETRA14,
        PYRAMIDfive, PYRAMID5, PYRAMID8, PYRAMID13, PYRAMID18,
        HEXeight, HEX8, HEX9, HEX20, HEX27, HEXSHELL, 
        INVALID_ELEMENT_TYPE};
  static const EntityType block_type_to_mb_type[];
  static const int cub_elem_num_verts[];
  static const int cub_elem_num_verts_len;

    //! mapping from mesh packet type to moab type
  static const EntityType mp_type_to_mb_type[];
  
    //! get entities with individually-specified types; if is_group is false, 
    //! increment each mem_type by 2 since they're CSOEntityType's and not group types
  ErrorCode get_entities(const unsigned int *mem_types,
                           int *id_buf, const unsigned int id_buf_size,
                           const bool is_group,
                           std::vector<EntityHandle> &entities);
  
    //! get entities specified by type and ids, append to entities
  ErrorCode get_entities(const unsigned int this_type, 
                           int *id_buf, const unsigned int id_buf_size,
                           std::vector<EntityHandle> &entities,
                           std::vector<EntityHandle> &excl_entities);
  
    //! get ref entity sets with specified type and ids
  ErrorCode get_ref_entities(const unsigned int this_type, 
                               int *id_buf, const unsigned id_buf_size,
                               std::vector<EntityHandle> &entities);
  
    //! get mesh entities with specified type and ids
  ErrorCode get_mesh_entities(const unsigned int this_type, 
                                int *id_buf, const unsigned id_buf_size,
                                std::vector<EntityHandle> &entities,
                                std::vector<EntityHandle> &excl_entities);
  
    //! process entities in a sideset according to sense flags stored in uint_buf
    //! or char_buf (depending on sense_size)
  ErrorCode process_sideset_10(const int this_type, const int num_ents,
                                 const int sense_size,
                                 std::vector<EntityHandle> &ss_entities,
                                 Tqdcfr::SidesetHeader *sideseth);

  ErrorCode process_sideset_11(std::vector<EntityHandle> &ss_entities,
                                 int num_wrts,
                                 Tqdcfr::SidesetHeader *sideseth);
  
    // put entities into the specfied set, and excluded entities into a 
    // std::vector pointed to by the "Exclude_Entities" tag on that set
  ErrorCode put_into_set(EntityHandle set_handle,
                           std::vector<EntityHandle> &entities,
                           std::vector<EntityHandle> &excl_entities);
  
    // look in metadatacontainer[set_index] for name data; if found, set name (and extra names,
    // if multiple found) on set handle
  ErrorCode get_names(MetaDataContainer &md, unsigned int set_index, EntityHandle seth);
  
};

} // namespace moab

#endif
