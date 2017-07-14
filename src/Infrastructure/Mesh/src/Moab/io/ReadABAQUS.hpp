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
// Filename      : ReadABAQUS.hpp
//
// Purpose       : ABAQUS inp file reader
//
// Special Notes : Started with NetCDF EXODUS II reader
//
// Creator       : Paul Wilson & Patrick Snouffer
//
// Date          : 08/2009
//
// Owner         : Paul Wilson
//-------------------------------------------------------------------------

/** Implementation of ABAQUS mesh hierarchy and meta-data on MOAB


This reader processes data written by the ABAQUS computer-aided engineering
front-end.  While that tool writes binary files in its own proprietary format,
it also writes an ASCII input file that is the fundamental input to the
ABAQUS solver itself.  A published syntax for this format is available from Simulia.

This reader only supports a subset of the mesh syntax necessary to support 
a basic thermal analysis of solid systems.

An ABAQUS mesh makes use of the common paradigms of building a
geometry as an "assembly" of "instances" of "parts".

A "part" is defined as a set of "nodes" and "elements" connecting
those nodes. The nodes and elements can be arranged in "node sets" and
"element sets" and specific "materials" can be assigned to "element
sets" other features of parts are not currently used by applications
and are not implemented.


Overview of supported structure

* File:
   * Heading
   * Part
      * Nodes 
      * Elements
      * Node Sets
      * Element Sets
      * Solid Sections
   * Assembly
      * Instance
        * Nodes 
        * Elements
        * Node Sets
        * Element Sets
        * Solid Sections
      * Node Sets


An "instance" is a full copy of a "part" with a linear geometric
transformation. To create a full copy:
• a duplicate set of nodes is created by copying the coordinates of
  the part nodes and applying a linear geometric transformation - the
  new coords are used to define the new nodes
• a new node set is created for each node set in the part and the set
  of nodes among the duplicates are assigned to the new node set
• a duplicate set of elements is defined by creating a new element
  with a connectivity made up of the duplicate nodes that correspond
  to the appropriate original element
• a new element set is created for each element set in the part and
  the set of elements among the duplicates are assigned to the new
  element set; the corresponding material is also assigned go the new
  element sets

It is also possible for an "instance" to contain the complete
definition of the mesh, copying nothing from the "part" (the "part"
may have no mesh defined).  It is unclear whether an "instance" can
combine mesh from the "part" definition with mesh contained only in
the "instance" definition. (Not appropriately documented in ABAUQUS
Reference Manual.)

In order to provide convenient access to the data and mesh structures
the following data model is used:

• EntitySet file_set
   • tags
       • NONE
   • members
       • all nodes of all parts and all instances
       • all elements of all parts and all instances
       • all assembly_sets
       • all part_sets
• EntitySet part_set
   • tags
       • mSetNameTag (opaque=char*)
         name of entity set
   • members
       • part nodes
       • part elements
       • part node sets
       • part element sets
• EntitySet assembly_set
   • tags
       • mSetNameTag (opaque=char*)
         name of entity set
   • members
      • instance_sets
      • instance element_sets
      • instance node_sets  
      • instance nodes
      • instance elements
• EntitySet instance_set
   • tags
       • mSetNameTag (opaque=char*)
         name of entity set
       • mPartHandleTag (handle)
         pointer to part from which this instance was generated
       • mAssemblyHandleTag (handle)
         pointer to assembly in which this instance exists
       • mInstancePIDTag (int)
         ordinal number indicating which instance of this part
       • mInstanceGIDTag (int)
         ordinal number indicating which instance in this assembly
   • members
      • instance nodes
      • instance elements
      • instance node_sets  
      • instance element_sets
• EntitySet node_set
   • tags
       • mSetNameTag (opaque=char*)
         name of entity set
       • mPartHandleTag (handle)
         pointer to part in which this node set exists
	 (only defined for node sets that are in an instance and 
          derive from a part)
       • mInstanceHandleTag (handle)
         pointer back to instance set in which this node set exists
         (NULL if this node_set is not in an instance)
       • mAssemblyHandleTag (handle)
         pointer to assembly in which this node set exists
         (NULL if this node_set is not in an assembly)
   • members
      • nodes
• EntitySet element_set
   • tags
       • mSetNameTag (opaque=char*)
         name of entity set
       • mPartHandleTag (handle)
         pointer to part in which this element set exists
	 (only defined for node sets that are in an instance and 
          derive from a part)
       • mInstanceHandleTag (handle)
         pointer back to instance set in which this element set exists
         (NULL if this node_set is not in an instance)
       • mAssemblyHandleTag (handle)
         pointer to assembly in which this element set exists
         (NULL if this node_set is not in an assembly)
       • mMatNameTag (opaque=char*)
         name of material in these elements
       • mMaterialSetTag (integer)
         material id in these elements
   • members
      • elements
• Entity node
   • tags
       • mLocalIDTag (int)
         numerical ID of node in local scope (part, instance)
       • mInstanceHandleTag (handle)
         pointer back to instance set in which this node exists
         (NULL if this node is not in an instance)
• Entity element
   • tags
       • mLocalIDTag (int)
         numerical ID of element in local scope (part, instance)
       • mInstanceHandleTag (handle)
         pointer back to instance set in which this element exists
         (NULL if this element is not in an instance)


 **/

#ifndef READABAQUS_HPP
#define READABAQUS_HPP

#ifndef IS_BUILDING_MB
  #error "ReadABAQUS.hpp isn't supposed to be included into an application"
#endif

#include <vector>
#include <map>
#include <string>
#include <iostream>
#include <fstream>

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/Range.hpp"

namespace moab {

#define ABAQUS_SET_TYPE_TAG_NAME "abaqus_set_type"
#define ABAQUS_SET_NAME_TAG_NAME "abaqus_set_name"
#define ABAQUS_SET_NAME_LENGTH   100
#define ABAQUS_LOCAL_ID_TAG_NAME "abaqus_local_id"

// Many sets should know who contains them
#define ABAQUS_INSTANCE_HANDLE_TAG_NAME "abaqus_instance_handle"
#define ABAQUS_ASSEMBLY_HANDLE_TAG_NAME "abaqus_assembly_handle"
#define ABAQUS_PART_HANDLE_TAG_NAME     "abaqus_part_handle"

// Instances should know things about themselves:
//  * which part they derive from (see ABAQUS_PART_HANDLE_TAG_NAME above)
//  * which instance of a part this is
//  * which instance of an assembly this is
#define ABAQUS_INSTANCE_PART_ID_TAG_NAME   "abaqus_instance_part_id"
#define ABAQUS_INSTANCE_GLOBAL_ID_TAG_NAME "abaqus_instance_global_id"

// Element sets have material name
// Using MOAB's general MATERIAL_SET to store material id
#define ABAQUS_MAT_NAME_TAG_NAME "abaqus_mat_name"
#define ABAQUS_MAT_NAME_LENGTH   100

#define ABQ_ASSEMBLY_SET 1
#define ABQ_PART_SET     2
#define ABQ_INSTANCE_SET 3
#define ABQ_NODE_SET     4
#define ABQ_ELEMENT_SET  5

enum abaqus_line_types {abq_undefined_line = 0,
                        abq_blank_line,
                        abq_comment_line,
                        abq_keyword_line,
                        abq_data_line,
                        abq_eof};

enum abaqus_keyword_type {abq_undefined = 0,
                          abq_unsupported,
                          abq_ambiguous,
                          abq_heading,
                          abq_part,
                          abq_end_part,
                          abq_assembly,
                          abq_end_assembly,
                          abq_node,
                          abq_element,
                          abq_nset,
                          abq_elset,
                          abq_instance,
                          abq_end_instance,
                          abq_solid_section};

enum abaqus_part_params {abq_part_undefined = 0,
                         abq_part_ambiguous,
                         abq_part_name};

enum abaqus_instance_params {abq_instance_undefined = 0,
                             abq_instance_ambiguous,
                             abq_instance_name,
                             abq_instance_part};

enum abaqus_assembly_params {abq_assembly_undefined = 0,
                             abq_assembly_ambiguous,
                             abq_assembly_name};

enum abaqus_node_params {abq_node_undefined = 0,
                         abq_node_ambiguous,
                         abq_node_nset,
                         abq_node_system};

enum abaqus_element_params {abq_element_undefined = 0,
                            abq_element_ambiguous,
                            abq_element_elset,
                            abq_element_type};

enum abaqus_element_type {abq_eletype_unsupported = 0,
                          abq_eletype_dc3d8,
                          abq_eletype_c3d8r,
                          abq_eletype_dcc3d8,
                          abq_eletype_c3d4,
                          abq_eletype_dc3d4,
                          abq_eletype_ds4};

enum abaqus_nset_params {abq_nset_undefined = 0,
                         abq_nset_ambiguous,
                         abq_nset_nset,
                         abq_nset_elset,
                         abq_nset_generate,
                         abq_nset_instance};

enum abaqus_elset_params {abq_elset_undefined = 0,
                          abq_elset_ambiguous,
                          abq_elset_elset,
                          abq_elset_generate,
                          abq_elset_instance};

enum abaqus_solid_section_params {abq_solid_section_undefined = 0,
                                  abq_solid_section_ambiguous,
                                  abq_solid_section_elset,
                                  abq_solid_section_matname};

class ReadUtilIface;

class ReadABAQUS : public ReaderIface
{
public:

  static ReaderIface* factory(Interface*);

  void tokenize(const std::string& str,
                std::vector<std::string>& tokens,
                const char* delimiters);

  //! Load an ABAQUS file
  ErrorCode load_file(const char* file_name,
                      const EntityHandle* file_set,
                      const FileOptions& opts,
                      const SubsetList* subset_list = 0,
                      const Tag* file_id_tag = 0);
  
  ErrorCode read_tag_values(const char* file_name,
                            const char* tag_name,
                            const FileOptions& opts,
                            std::vector<int>& tag_values_out,
                            const SubsetList* subset_list = 0);

  //! Constructor
  ReadABAQUS(Interface* impl = NULL);
  
  //! Destructor
  virtual ~ReadABAQUS();

private:

  void reset();

  ErrorCode read_heading(EntityHandle file_set);
  ErrorCode read_part(EntityHandle file_set);
  ErrorCode read_assembly(EntityHandle file_set);
  ErrorCode read_unsupported(EntityHandle file_set);
  ErrorCode read_node_list(EntityHandle parent_set,
                           EntityHandle assembly_set = 0);
  ErrorCode read_element_list(EntityHandle parent_set,
                              EntityHandle assembly_set = 0);
  ErrorCode read_node_set(EntityHandle parent_set,
                          EntityHandle file_set = 0,
                          EntityHandle assembly_set = 0);
  ErrorCode read_element_set(EntityHandle parent_set,
                             EntityHandle file_set = 0,
                             EntityHandle assembly_set = 0);
  ErrorCode read_solid_section(EntityHandle parent_set);
  ErrorCode read_instance(EntityHandle assembly_set,
                          EntityHandle file_set);

  ErrorCode get_elements_by_id(EntityHandle parent_set,
                               std::vector<int> element_ids_subset,
                               Range &element_range);

  ErrorCode get_nodes_by_id(EntityHandle parent_set,
                            std::vector<int> node_ids_subset,
                            Range &node_range);
    
  ErrorCode get_set_by_name(EntityHandle parent_set,
                            int ABQ_set_type,
                            const std::string &set_name,
                            EntityHandle &set_handle);

  ErrorCode get_set_elements(EntityHandle set_handle,
                             Range &element_range);

  ErrorCode get_set_elements_by_name(EntityHandle parent_set,
                                     int ABQ_set_type,
                                     const std::string &set_name,
                                     Range &element_range);

  ErrorCode get_set_nodes(EntityHandle parent_set,
                          int ABQ_set_type,
                          const std::string &set_name,
                          Range &node_range);

  ErrorCode add_entity_set(EntityHandle parent_set,
                           int ABQ_set_type,
                           const std::string &set_name,
                           EntityHandle &entity_set);

  ErrorCode create_instance_of_part(const EntityHandle file_set,
                                    const EntityHandle parent_set,
                                    const std::string &part_name,
                                    const std::string &instance_name,
                                    EntityHandle &entity_set,
                                    const std::vector<double> &translation,
                                    const std::vector<double> &rotation);

  Tag get_tag(const char* tag_name, int tag_size, TagType tag_type,
              DataType tag_data_type, const void* def_val = 0);

  void cyl2rect(std::vector<double> coord_list);

  void sph2rect(std::vector<double> coord_list);

  abaqus_line_types get_next_line_type();
  abaqus_keyword_type get_keyword();

  template <class T>
  std::string match(const std::string &token,
                    std::map<std::string,T> &tokenList);

  void stringToUpper(const std::string& toBeConverted,std::string& converted);

  void extract_keyword_parameters(const std::vector<std::string>& tokens,
                                  std::map<std::string, std::string>& params);

  //! Interface instance
  Interface* mdbImpl;

  //! Read mesh interface
  ReadUtilIface* readMeshIface;

  std::ifstream abFile; // abaqus file

  std::string readline;

  unsigned lineNo;

  //! Cached tags for reading. Note that all these tags are defined when the
  //! core is initialized.
  Tag mMaterialSetTag;
  Tag mDirichletSetTag;
  Tag mNeumannSetTag;
  Tag mHasMidNodesTag;

  Tag mSetTypeTag;
  Tag mPartHandleTag;
  Tag mInstancePIDTag;
  Tag mInstanceGIDTag;

  Tag mLocalIDTag;
  Tag mInstanceHandleTag;
  Tag mAssemblyHandleTag;

  Tag mSetNameTag;
  Tag mMatNameTag;

  abaqus_line_types next_line_type;

  std::map<EntityHandle, unsigned int> num_part_instances;
  std::map<EntityHandle, unsigned int> num_assembly_instances;
  std::map<std::string, unsigned int> matIDmap;
  unsigned mat_id;

};

} // namespace moab

#endif
