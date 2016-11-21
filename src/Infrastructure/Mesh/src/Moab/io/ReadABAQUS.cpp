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

#ifdef WIN32
#pragma warning(disable:4786)
#endif

#include "ReadABAQUS.hpp"

#include <algorithm>
#include <time.h>
#include <string>
#include <assert.h>
#include <stdio.h>
#include <cmath>

#include "moab/Range.hpp"
#include "moab/Interface.hpp"
#include "MBTagConventions.hpp"
#include "Internals.hpp"
#include "moab/ReadUtilIface.hpp"
#include "AffineXform.hpp"
// #include "abaqus_order.h"
#include "moab/FileOptions.hpp"

namespace moab {

#define ABQ_AMBIGUOUS "AMBIGUOUS"
#define ABQ_UNDEFINED "UNDEFINED"
#define DEG2RAD 0.017453292519943295769236907684886

#define MB_RETURN_IF_FAIL if (MB_SUCCESS != status) return status

ReaderIface* ReadABAQUS::factory(Interface* iface)
{
  return new ReadABAQUS(iface);
}

ReadABAQUS::ReadABAQUS(Interface* impl)
  : mdbImpl(impl), readMeshIface(NULL), lineNo(0), next_line_type(abq_undefined_line), mat_id(0)
{
  assert(impl != NULL);
  reset();

  impl->query_interface(readMeshIface);

  // Initialize in case tag_get_handle fails below
  mMaterialSetTag  = 0;
  mDirichletSetTag = 0;
  mNeumannSetTag   = 0;
  mHasMidNodesTag  = 0;

  mSetTypeTag        = 0;
  mPartHandleTag     = 0;
  mInstancePIDTag    = 0;
  mInstanceGIDTag    = 0;
  mLocalIDTag        = 0;
  mInstanceHandleTag = 0;
  mAssemblyHandleTag = 0;
  mSetNameTag        = 0;
  mMatNameTag        = 0;

  //! Get and cache predefined tag handles
  int zero = 0, negone = -1, negonearr[] = {-1, -1, -1, -1};
  mMaterialSetTag  = get_tag(MATERIAL_SET_TAG_NAME,  1, MB_TAG_SPARSE, MB_TYPE_INTEGER, &negone);
  mDirichletSetTag = get_tag(DIRICHLET_SET_TAG_NAME, 1, MB_TAG_SPARSE, MB_TYPE_INTEGER, &negone);
  mNeumannSetTag   = get_tag(NEUMANN_SET_TAG_NAME,   1, MB_TAG_SPARSE, MB_TYPE_INTEGER, &negone);
  mHasMidNodesTag  = get_tag(HAS_MID_NODES_TAG_NAME, 4, MB_TAG_SPARSE, MB_TYPE_INTEGER, negonearr);

  mSetTypeTag        = get_tag(ABAQUS_SET_TYPE_TAG_NAME,           1, MB_TAG_SPARSE, MB_TYPE_INTEGER);
  mPartHandleTag     = get_tag(ABAQUS_PART_HANDLE_TAG_NAME,        1, MB_TAG_SPARSE, MB_TYPE_HANDLE);
  mInstanceHandleTag = get_tag(ABAQUS_INSTANCE_HANDLE_TAG_NAME,    1, MB_TAG_DENSE, MB_TYPE_HANDLE);
  mAssemblyHandleTag = get_tag(ABAQUS_ASSEMBLY_HANDLE_TAG_NAME,    1, MB_TAG_DENSE, MB_TYPE_HANDLE);
  mInstancePIDTag    = get_tag(ABAQUS_INSTANCE_PART_ID_TAG_NAME,   1, MB_TAG_SPARSE, MB_TYPE_INTEGER);
  mInstanceGIDTag    = get_tag(ABAQUS_INSTANCE_GLOBAL_ID_TAG_NAME, 1, MB_TAG_SPARSE, MB_TYPE_INTEGER, &zero);
  mLocalIDTag        = get_tag(ABAQUS_LOCAL_ID_TAG_NAME,           1, MB_TAG_DENSE, MB_TYPE_INTEGER);
  mSetNameTag        = get_tag(ABAQUS_SET_NAME_TAG_NAME, ABAQUS_SET_NAME_LENGTH, MB_TAG_SPARSE, MB_TYPE_OPAQUE, 0);
  mMatNameTag        = get_tag(ABAQUS_MAT_NAME_TAG_NAME, ABAQUS_MAT_NAME_LENGTH, MB_TAG_SPARSE, MB_TYPE_OPAQUE, 0);
}

void ReadABAQUS::reset()
{
}

ReadABAQUS::~ReadABAQUS() 
{
  mdbImpl->release_interface(readMeshIface);
  if (abFile.fail())
    abFile.close();
}

/*

ErrorCode ReadABAQUS::check_file_stats()
* check for existence of file
* initialize meshsets, and offsets if necessary

*/

ErrorCode ReadABAQUS::read_tag_values(const char* /* file_name */,
                                      const char* /* tag_name */,
                                      const FileOptions& /* opts */,
                                      std::vector<int>& /* tag_values_out */,
                                      const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode ReadABAQUS::load_file(const char *abaqus_file_name,
                                const EntityHandle* file_set_ptr,
                                const FileOptions& /*opts*/,
                                const ReaderIface::SubsetList* subset_list,
                                const Tag* /*file_id_tag*/)
{
  ErrorCode status;

  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for ABAQUS data");
  }

  // Open file
  lineNo = 0;
  abFile.open(abaqus_file_name);
  if (!abFile)
    return MB_FILE_DOES_NOT_EXIST;

  bool in_unsupported = false;

  EntityHandle file_set;
  status = mdbImpl->create_meshset(MESHSET_SET, file_set);
  if (MB_SUCCESS != status)
    return status;

  next_line_type = get_next_line_type();
  while (next_line_type != abq_eof) {
    switch (next_line_type) {
      case abq_keyword_line:
        in_unsupported = false;
        switch (get_keyword()) {
          case abq_heading:
            // Read header
            status = read_heading(file_set);
            break;
          case abq_part:
            // Read parts until done
            status = read_part(file_set);
            break;
          case abq_assembly:
            // Read assembly (or assemblies?)
            status = read_assembly(file_set);
            break;
          default:
            // Skip reading other content for now
            // (e.g. material properties, loads, surface interactions, etc)
            in_unsupported = true;
            //std::cout << "Ignoring unsupported keyword: " << readline << std::endl;
        }
        MB_RETURN_IF_FAIL;
        break;
      case abq_comment_line:
        break;
      case abq_data_line:
        if (!in_unsupported) {
          MB_SET_ERR(MB_FAILURE, "Expected Keyword");
        }
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Invalid/unrecognized line");
    }

    next_line_type = get_next_line_type();
  }

  // Temporary??? delete parts
  // Get all node sets in part
  Range part_sets;
  int tag_val = ABQ_PART_SET;
  void* tag_data[] = {&tag_val};
  status = mdbImpl->get_entities_by_type_and_tag(file_set,
                                                 MBENTITYSET,
                                                 &mSetTypeTag,
                                                 tag_data, 1, part_sets);
  MB_RETURN_IF_FAIL;

  for (Range::iterator part_set = part_sets.begin();
      part_set != part_sets.end();
      ++part_set) {
    Range ent_sets;
    tag_val = ABQ_NODE_SET;
    tag_data[0] = &tag_val;

    status = mdbImpl->get_entities_by_type_and_tag(*part_set,
                                                   MBENTITYSET,
                                                   &mSetTypeTag,
                                                   tag_data, 1, ent_sets);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->delete_entities(ent_sets);
    MB_RETURN_IF_FAIL;

    tag_val = ABQ_ELEMENT_SET;
    tag_data[0] = &tag_val;

    status = mdbImpl->get_entities_by_type_and_tag(*part_set,
                                                   MBENTITYSET,
                                                   &mSetTypeTag,
                                                   tag_data, 1, ent_sets);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->delete_entities(ent_sets);
    MB_RETURN_IF_FAIL;

    Range node_list, ele_list;
    status = get_set_elements(*part_set, ele_list);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->delete_entities(ele_list);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->get_entities_by_dimension(*part_set, 0, node_list);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->delete_entities(node_list);
    MB_RETURN_IF_FAIL;
  }

  if (file_set_ptr) {
    status = mdbImpl->unite_meshset(*file_set_ptr, file_set);
    MB_RETURN_IF_FAIL;
  }

  return mdbImpl->delete_entities(&file_set, 1);
}

ErrorCode ReadABAQUS::read_heading(EntityHandle /*file_set*/)
{
  // Current line is only heading token. get next line
  next_line_type = get_next_line_type();

  // Perhaps keep first line and tag geometry with title?

  while (abq_data_line == next_line_type ||
      abq_comment_line == next_line_type)
    next_line_type = get_next_line_type();

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::read_assembly(EntityHandle file_set)
{
  ErrorCode status = MB_SUCCESS;

  std::vector<std::string> tokens;
  std::map<std::string, std::string> params;
  std::map<std::string, abaqus_assembly_params> requiredParams;
  requiredParams["NAME"] = abq_assembly_name;

  std::map<std::string, abaqus_assembly_params> allowableParams;
  allowableParams[ABQ_AMBIGUOUS] = abq_assembly_ambiguous;

  abaqus_assembly_params param;

  std::string assembly_name;

  // Tokenize last line read
  tokenize(readline, tokens, ",\n");
  extract_keyword_parameters(tokens, params);

  // Search for required parameters
  for (std::map<std::string, abaqus_assembly_params>::iterator thisParam = requiredParams.begin();
      thisParam != requiredParams.end();
      ++thisParam) {
    std::string param_key = match((*thisParam).first, params);
    param = requiredParams[param_key];
    switch (param) {
      case abq_assembly_name:
        assembly_name = params[param_key];
        params.erase(param_key);
        // std::cout << "Adding ASSEMBLY with name: " << assembly_name << std::endl; // REMOVE
        break;
      default:
        // std::cout << "Missing required ASSEMBLY parameter " << (*thisParam).first << std::endl;
        return MB_FAILURE;
    }
  }

  // Process parameters
  for (std::map<std::string, std::string>::iterator thisParam = params.begin();
      thisParam != params.end();
      ++thisParam) {
    // Look for unambiguous match with this node parameter
    param = allowableParams[match((*thisParam).first, allowableParams)];
    switch (param) {
      case abq_assembly_ambiguous:
        //std::cout << "\tIgnoring ambiguous ASSEMBLY parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
      default:
        //std::cout << "\tIgnoring unsupported ASSEMBLY parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
    }
  }

  EntityHandle assembly_set;

  status = add_entity_set(file_set, ABQ_ASSEMBLY_SET, assembly_name, assembly_set);

  next_line_type = get_next_line_type();

  bool end_assembly = false;
  bool in_unsupported = false;

  while (next_line_type != abq_eof && !end_assembly) {
    switch (next_line_type) {
      case abq_keyword_line:
        in_unsupported = false;
        switch (get_keyword()) {
          case abq_end_assembly:
            end_assembly = true;
            break;
          case abq_instance:
            status = read_instance(assembly_set, file_set);
            break;
          case abq_nset:
            status = read_node_set(assembly_set, file_set);
            break;
          default:
            in_unsupported = true;
            //std::cout << "\tIgnoring unsupported keyword in this ASSEMBLY: "
            //          << readline << std::endl;
            next_line_type = get_next_line_type();
            break;
        }
        break;
      case abq_comment_line:
        next_line_type = get_next_line_type();
        break;
      case abq_data_line:
        if (!in_unsupported) {
          //std::cout << "Internal Error: Data lines not allowed in ASSEMBLY keyword."
          //          << std::endl << readline << std::endl;
          return MB_FAILURE;
        }
        next_line_type = get_next_line_type();
        break;
      case abq_blank_line:
        //std::cout << "Error: Blank lines are not allowed." << std::endl;
        return MB_FAILURE;
      default:
        //std::cout << "Error reading ASSEMBLY " << assembly_name << std::endl;
        return MB_FAILURE;
    }
    MB_RETURN_IF_FAIL;
  }

  num_assembly_instances[assembly_set] = 0;

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::read_instance(EntityHandle assembly_set, EntityHandle file_set)
{
  ErrorCode status = MB_SUCCESS;

  std::vector<std::string> tokens;
  std::map<std::string, std::string> params;
  std::map<std::string, abaqus_instance_params> requiredParams;
  requiredParams["NAME"] = abq_instance_name;
  requiredParams["PART"] = abq_instance_part;

  std::map<std::string, abaqus_instance_params> allowableParams;
  allowableParams[ABQ_AMBIGUOUS] = abq_instance_ambiguous;

  abaqus_instance_params param;

  std::string instance_name, part_name;

  // Tokenize last line read
  tokenize(readline, tokens, ",\n");
  extract_keyword_parameters(tokens, params);

  // Search for required parameters
  for (std::map<std::string, abaqus_instance_params>::iterator thisParam = requiredParams.begin();
      thisParam != requiredParams.end();
      ++thisParam) {
    std::string param_key = match((*thisParam).first, params);
    param = requiredParams[param_key];
    switch (param) {
      case abq_instance_name:
        instance_name = params[param_key];
        params.erase(param_key);
        break;
      case abq_instance_part:
        part_name = params[param_key];
        params.erase(param_key);
        break;
      default:
        //std::cout << "Missing required INSTANCE parameter " << (*thisParam).first << std::endl;
        return MB_FAILURE;
    }
  }
  //std::cout << "\tAdding INSTANCE with name: " << instance_name << " of PART wit name: " << part_name <<  std::endl; // REMOVE

  // Process parameters
  for (std::map<std::string, std::string>::iterator thisParam = params.begin();
      thisParam != params.end();
      ++thisParam) {
    // Look for unambiguous match with this node parameter
    param = allowableParams[match((*thisParam).first, allowableParams)];
    switch (param) {
      case abq_instance_ambiguous:
        //std::cout << "\t\tIgnoring ambiguous INSTANCE parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
      default:
        //std::cout << "\t\tIgnoring unsupported INSTANCE parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
    }
  }

  next_line_type = get_next_line_type();

  bool read_translation = false;
  bool read_rotation = false;
  std::vector<double> translation(3, 0);
  std::vector<double> rotation(7, 0);
  bool end_instance = false;
  bool in_unsupported = false;

  EntityHandle instance_set;
  status = add_entity_set(assembly_set, ABQ_INSTANCE_SET, instance_name, instance_set);
  MB_RETURN_IF_FAIL;

  while (next_line_type != abq_eof && !end_instance) {
    switch (next_line_type) {
      case abq_keyword_line:
        in_unsupported = false;
        switch (get_keyword()) {
          case abq_end_instance:
            end_instance = true;
            next_line_type = get_next_line_type();
            break;
          case abq_node:
            status = read_node_list(instance_set, assembly_set);
            break;
          case abq_element:
            status = read_element_list(instance_set, assembly_set);
            break;
          case abq_nset:
            status = read_node_set(instance_set, file_set, assembly_set);
            break;
          case abq_elset:
            status = read_element_set(instance_set, file_set, assembly_set);
            break;
          case abq_solid_section:
            status = read_solid_section(instance_set);
            break;
          default:
            in_unsupported = true;
            //std::cout << "\t\tIgnoring unsupported keyword in this INSTANCE: "
            //          << readline << std::endl;
            next_line_type = get_next_line_type();
            break;
        }
        break;
      case abq_comment_line:
        next_line_type = get_next_line_type();
        break;
      case abq_data_line:
        if (!in_unsupported) {
          tokenize(readline, tokens, ", \n");
          if (!read_translation) {
            if (tokens.size() != 3) {
              MB_SET_ERR(MB_FAILURE, "Wrong number of entries on INSTANCE translation line");
            }

            for (unsigned int i = 0; i < 3; i++)
              translation[i] = atof(tokens[i].c_str());

            read_translation = true;
          }
          else if (!read_rotation) {
            if (tokens.size() != 7) {
              MB_SET_ERR(MB_FAILURE, "Wrong number of entries on INSTANCE rotation line");
            }
            for (unsigned int i = 0; i < 7; i++)
              rotation[i] = atof(tokens[i].c_str());

            read_rotation = true;
          }
          else {
            MB_SET_ERR(MB_FAILURE, "Too many data lines for this INSTANCE");
          }
        } // if (!in_unsupported)
        next_line_type = get_next_line_type();
        break;
      case abq_blank_line:
        MB_SET_ERR(MB_FAILURE, "Error: Blank lines are not allowed");
      default:
        MB_SET_ERR(MB_FAILURE, "Error reading INSTANCE");
    } // switch (next_line_type)
  } // while (next_line_type != abq_eof && !end_instance)

  status = create_instance_of_part(file_set, assembly_set, part_name,
                                   instance_name, instance_set, translation, rotation);
  MB_RETURN_IF_FAIL;

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::read_part(EntityHandle file_set)
{
  ErrorCode status = MB_SUCCESS;

  std::vector<std::string> tokens;
  std::map<std::string, std::string> params;
  std::map<std::string, abaqus_part_params> requiredParams;
  requiredParams["NAME"] = abq_part_name;

  std::map<std::string, abaqus_part_params> allowableParams;
  allowableParams[ABQ_AMBIGUOUS] = abq_part_ambiguous;

  abaqus_part_params param;

  std::string part_name;

  // Tokenize last line read
  tokenize(readline, tokens, ",\n");
  extract_keyword_parameters(tokens, params);

  // Search for required parameters
  for (std::map<std::string, abaqus_part_params>::iterator thisParam = requiredParams.begin();
      thisParam != requiredParams.end();
      ++thisParam) {
    std::string param_key = match((*thisParam).first, params);
    param = requiredParams[param_key];
    switch (param) {
      case abq_part_name:
        part_name = params[param_key];
        params.erase(param_key);
        //std::cout << "Adding PART with name: " << part_name << std::endl; // REMOVE
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Missing required PART parameter");
    }
  }

  // Process parameters
  for (std::map<std::string, std::string>::iterator thisParam = params.begin();
      thisParam != params.end();
      ++thisParam) {
    // Look for unambiguous match with this node parameter
    param = allowableParams[match((*thisParam).first, allowableParams)];
    switch (param) {
      case abq_part_ambiguous:
        //std::cout << "\tIgnoring ambiguous PART parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
      default:
        //std::cout << "\tIgnoring unsupported PART parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
    }
  }

  EntityHandle part_set;

  status = add_entity_set(file_set, ABQ_PART_SET, part_name, part_set);

  next_line_type = get_next_line_type();

  bool end_part = false;
  bool in_unsupported = false;

  while (next_line_type != abq_eof && !end_part) {
    switch (next_line_type) {
      case abq_keyword_line:
        in_unsupported = false;
        switch (get_keyword()) {
          case abq_end_part:
            end_part = true;
            break;
          case abq_node:
            status = read_node_list(part_set);
            break;
          case abq_element:
            status = read_element_list(part_set);
            break;
          case abq_nset:
            status = read_node_set(part_set);
            break;
          case abq_elset:
            status = read_element_set(part_set);
            break;
          case abq_solid_section:
            status = read_solid_section(part_set);
            break;
          default:
            in_unsupported = true;
            //std::cout << "\tIgnoring unsupported keyword in this PART: "
            //          << readline << std::endl;
            next_line_type = get_next_line_type();
            break;
        }
        MB_RETURN_IF_FAIL;
        break;
      case abq_comment_line:
        next_line_type = get_next_line_type();
        break;
      case abq_data_line:
        if (!in_unsupported) {
          MB_SET_ERR(MB_FAILURE, "Data lines not allowed in PART keyword");
        }
        next_line_type = get_next_line_type();
        break;
      case abq_blank_line:
        MB_SET_ERR(MB_FAILURE, "Blank lines are not allowed");
      default:
        MB_SET_ERR(MB_FAILURE, "Error reading PART");
    }
  }

  num_part_instances[part_set] = 0;

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::read_solid_section(EntityHandle parent_set)
{
  ErrorCode status;

  std::vector<std::string> tokens;
  std::map<std::string, std::string> params;
  std::map<std::string, abaqus_solid_section_params> requiredParams;
  requiredParams["ELSET"] = abq_solid_section_elset;
  requiredParams["MATERIAL"] = abq_solid_section_matname;

  std::map<std::string, abaqus_solid_section_params> allowableParams;
  allowableParams[ABQ_AMBIGUOUS] = abq_solid_section_ambiguous;

  abaqus_solid_section_params param;

  // Tokenize last line read
  tokenize(readline, tokens, ",\n");
  extract_keyword_parameters(tokens, params);

  std::string elset_name, mat_name;

  // Search for required parameters
  for (std::map<std::string, abaqus_solid_section_params>::iterator thisParam = requiredParams.begin();
      thisParam != requiredParams.end();
      ++thisParam) {
    std::string param_key = match((*thisParam).first, params);
    param = requiredParams[param_key];
    switch (param) {
      case abq_solid_section_elset:
        elset_name = params[param_key];
        params.erase(param_key);
        break;
      case abq_solid_section_matname:
        mat_name = params[param_key];
        params.erase(param_key);
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Missing required SOLID SECTION parameter");
    }
  }
  //std::cout << "\tAdding SOLID SECTION with to ELEMENT SET: " << elset_name << " with material: " << mat_name << std::endl; // REMOVE

  // Process parameters
  for (std::map<std::string, std::string>::iterator thisParam = params.begin();
      thisParam != params.end();
      ++thisParam) {
    // Look for unambiguous match with this node parameter
    param = allowableParams[match((*thisParam).first, allowableParams)];
    switch (param) {
      case abq_solid_section_ambiguous:
        //std::cout << "\t\tIgnoring ambiguous SOLID_SECTION parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
      default:
        //std::cout << "\t\tIgnoring unsupported SOLID_SECTION parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
    }
  }

  EntityHandle set_handle;
  status = get_set_by_name(parent_set, ABQ_ELEMENT_SET, elset_name, set_handle);
  MB_RETURN_IF_FAIL;

  status = mdbImpl->tag_set_data(mMatNameTag, &set_handle, 1, mat_name.c_str());
  MB_RETURN_IF_FAIL;

  if (0 == matIDmap[mat_name])
    matIDmap[mat_name] = ++mat_id;

  status = mdbImpl->tag_set_data(mMaterialSetTag, &set_handle, 1, &(matIDmap[mat_name]));
  MB_RETURN_IF_FAIL;

  next_line_type = get_next_line_type();

  while (next_line_type != abq_eof && next_line_type != abq_keyword_line)
    next_line_type = get_next_line_type();

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::read_element_set(EntityHandle parent_set, EntityHandle file_set, EntityHandle assembly_set)
{
  ErrorCode status;

  std::vector<std::string> tokens;
  std::map<std::string, std::string> params;
  std::map<std::string, abaqus_elset_params> requiredParams;
  requiredParams["ELSET"] = abq_elset_elset;

  std::map<std::string, abaqus_elset_params> allowableParams;
  allowableParams[ABQ_AMBIGUOUS] = abq_elset_ambiguous;
  allowableParams["GENERATE"] = abq_elset_generate;
  allowableParams["INSTANCE"] = abq_elset_instance;

  abaqus_elset_params param;

  std::string elset_name;
  bool generate_elset = false;
  std::string instance_name;
  EntityHandle element_container_set = parent_set;

  // Tokenize last line read
  tokenize(readline, tokens, ",\n");
  extract_keyword_parameters(tokens, params);

  Range element_range;

  // Search for required parameters
  for (std::map<std::string, abaqus_elset_params>::iterator thisParam = requiredParams.begin();
      thisParam != requiredParams.end();
      ++thisParam) {
    std::string param_key = match((*thisParam).first, params);
    param = requiredParams[param_key];
    switch (param) {
      case abq_elset_elset:
        elset_name = params[param_key];
        params.erase(param_key);
        //std::cout << "\tAdding ELSET with name: " << elset_name << std::endl; // REMOVE
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Missing required ELSET parameter");
    }
  }

  // Process parameters
  for (std::map<std::string, std::string>::iterator thisParam = params.begin();
      thisParam != params.end();
      ++thisParam) {
    // Look for unambiguous match with this node parameter
    param = allowableParams[match((*thisParam).first, allowableParams)];
    switch (param) {
      case abq_elset_generate:
        generate_elset = true;
        break;
      case abq_elset_instance:
        instance_name = (*thisParam).second;
        status = get_set_by_name(parent_set, ABQ_INSTANCE_SET, instance_name, element_container_set);
        MB_RETURN_IF_FAIL;
        break;
      case abq_elset_ambiguous:
        //std::cout << "\t\tIgnoring ambiguous ELSET parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
      default:
        //std::cout << "\t\tIgnoring unsupported ELSET parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
    }
  }

  std::vector<int> element_list;
  Range tmp_element_range;

  next_line_type = get_next_line_type();

  while (next_line_type != abq_eof && next_line_type != abq_keyword_line) {
    if (abq_data_line == next_line_type) {
      tokenize(readline, tokens, ", \n");
      if (generate_elset) {
        if (tokens.size() != 3) {
          MB_SET_ERR(MB_FAILURE, "Wrong number of entries on GENERATE element set data line");
        }
        int e1 = atoi(tokens[0].c_str());
        int e2 = atoi(tokens[1].c_str());
        int incr = atoi(tokens[2].c_str());
        if ((incr == 0) || (((e2 - e1) % incr) != 0)) {
          MB_SET_ERR(MB_FAILURE, "Invalid data on GENERATE element set data line");
        }
        for (int element_id = e1; element_id <= e2; element_id += incr)
          element_list.push_back(element_id);
      }
      else {
        for (unsigned int idx = 0; idx < tokens.size(); idx++) {
          if (isalpha(tokens[idx][0])) {
            tmp_element_range.clear();
            status = get_set_elements_by_name(element_container_set, ABQ_ELEMENT_SET, tokens[idx], tmp_element_range);
            MB_RETURN_IF_FAIL;

            element_range.merge(tmp_element_range);
          }
          else
            element_list.push_back(atoi(tokens[idx].c_str()));
        }
      }
    } // if (abq_data_line == next_line_type)

    next_line_type = get_next_line_type();
  } // while (next_line_type != abq_eof && next_line_type != abq_keyword_line)

  tmp_element_range.clear();
  status = get_elements_by_id(element_container_set, element_list, tmp_element_range);
  MB_RETURN_IF_FAIL;

  element_range.merge(tmp_element_range);

  EntityHandle element_set;

  status = add_entity_set(parent_set, ABQ_ELEMENT_SET, elset_name, element_set);
  MB_RETURN_IF_FAIL;

  status = mdbImpl->add_entities(element_set, element_range);
  MB_RETURN_IF_FAIL;

  // SHOULD WE EVER DO THIS???
  if (file_set) {
    status = mdbImpl->add_entities(file_set, &element_set, 1);
    MB_RETURN_IF_FAIL;
  }

  // SHOULD WE EVER DO THIS???
  if (assembly_set) {
    status = mdbImpl->add_entities(assembly_set, &element_set, 1);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->tag_set_data(mAssemblyHandleTag, &element_set, 1, &assembly_set);
    MB_RETURN_IF_FAIL;
  }

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::read_node_set(EntityHandle parent_set, EntityHandle file_set, EntityHandle assembly_set)
{
  ErrorCode status;

  std::vector<std::string> tokens;
  std::map<std::string, std::string> params;
  std::map<std::string, abaqus_nset_params> requiredParams;
  requiredParams["NSET"] = abq_nset_nset;

  std::map<std::string, abaqus_nset_params> allowableParams;
  allowableParams[ABQ_AMBIGUOUS] = abq_nset_ambiguous;
  allowableParams["ELSET"] = abq_nset_elset;
  allowableParams["GENERATE"] = abq_nset_generate;
  allowableParams["INSTANCE"] = abq_nset_instance;

  abaqus_nset_params param;

  std::string nset_name;
  bool make_from_elset = false;
  bool generate_nset = false;
  std::string elset_name, instance_name;
  EntityHandle node_container_set = parent_set;

  // Tokenize last line read
  tokenize(readline, tokens, ",\n");
  extract_keyword_parameters(tokens, params);

  Range node_range;

  // Search for required parameters
  for (std::map<std::string, abaqus_nset_params>::iterator thisParam = requiredParams.begin();
      thisParam != requiredParams.end();
      ++thisParam) {
    std::string param_key = match((*thisParam).first, params);
    param = requiredParams[param_key];
    switch (param) {
      case abq_nset_nset:
        nset_name = params[param_key];
        params.erase(param_key);
        //std::cout << "\tAdding NSET with name: " << nset_name << std::endl; // REMOVE
        break;
      default:
        MB_SET_ERR(MB_FAILURE, "Missing required NSET parameter");
    }
  }

  // Process parameters
  for (std::map<std::string, std::string>::iterator thisParam = params.begin();
      thisParam != params.end();
      ++thisParam) {
    // Look for unambiguous match with this node parameter
    param = allowableParams[match((*thisParam).first, allowableParams)];
    switch (param) {
      case abq_nset_elset:
        make_from_elset = true;
        elset_name = (*thisParam).second;
        break;
      case abq_nset_generate:
        generate_nset = true;
        break;
      case abq_nset_instance:
        instance_name = (*thisParam).second;
        status = get_set_by_name(parent_set, ABQ_INSTANCE_SET, instance_name, node_container_set);
        MB_RETURN_IF_FAIL;
        break;
      case abq_nset_ambiguous:
        //std::cout << "\t\tIgnoring ambiguous NSET parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
      default:
        //std::cout << "\t\tIgnoring unsupported NSET parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
    }
  }

  if (make_from_elset && generate_nset) {
    MB_SET_ERR(MB_FAILURE, "Incompatible NSET parameters ELSET & GENERATE");
  }

  if (make_from_elset) {
    status = get_set_nodes(parent_set, ABQ_ELEMENT_SET, elset_name, node_range);
    MB_RETURN_IF_FAIL;
  }
  else {
    std::vector<int> node_list;
    Range tmp_node_range;

    next_line_type = get_next_line_type();

    while (next_line_type != abq_eof && next_line_type != abq_keyword_line) {
      if (abq_data_line == next_line_type) {
        tokenize(readline, tokens, ", \n");
        if (generate_nset) {
          if (tokens.size() != 3) {
            MB_SET_ERR(MB_FAILURE, "Wrong number of entries on GENERATE node set data line");
          }
          int n1 = atoi(tokens[0].c_str());
          int n2 = atoi(tokens[1].c_str());
          int incr = atoi(tokens[2].c_str());
          if ((incr == 0) || (((n2 - n1) % incr) != 0)) {
            MB_SET_ERR(MB_FAILURE, "Invalid data on GENERATE node set data line");
          }
          for (int node_id = n1; node_id <= n2; node_id += incr)
            node_list.push_back(node_id);
        }
        else {
          for (unsigned int idx = 0; idx < tokens.size(); idx++) {
            if (isalpha(tokens[idx][0])) {
              tmp_node_range.clear();
              status = get_set_nodes(parent_set, ABQ_NODE_SET, tokens[idx], tmp_node_range);
              MB_RETURN_IF_FAIL;

              node_range.merge(tmp_node_range);
            }
            else
              node_list.push_back(atoi(tokens[idx].c_str()));
          }
        }
      } // if (abq_data_line == next_line_type)

      next_line_type = get_next_line_type();
    } // while (next_line_type != abq_eof && next_line_type != abq_keyword_line)

    tmp_node_range.clear();

    status = get_nodes_by_id(node_container_set, node_list, tmp_node_range);
    MB_RETURN_IF_FAIL;

    node_range.merge(tmp_node_range);
  }

  EntityHandle node_set;

  status = add_entity_set(parent_set, ABQ_NODE_SET, nset_name, node_set);
  MB_RETURN_IF_FAIL;

  status = mdbImpl->add_entities(node_set, node_range);
  MB_RETURN_IF_FAIL;

  if (file_set) {
    status = mdbImpl->add_entities(file_set, &node_set, 1);
    MB_RETURN_IF_FAIL;
  }

  if (assembly_set) {
    status = mdbImpl->add_entities(assembly_set, &node_set, 1);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->tag_set_data(mAssemblyHandleTag, &node_set, 1, &assembly_set);
    MB_RETURN_IF_FAIL;
  }

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::read_element_list(EntityHandle parent_set, EntityHandle assembly_set)
{
  ErrorCode status;

  std::vector<std::string> tokens;
  std::map<std::string, std::string> params;
  std::map<std::string, abaqus_element_params> requiredParams;
  requiredParams["TYPE"] = abq_element_type;

  std::map<std::string, abaqus_element_params> allowableParams;
  allowableParams[ABQ_AMBIGUOUS] = abq_element_ambiguous;
  allowableParams["ELSET"] = abq_element_elset;

  abaqus_element_params param;

  std::map<std::string, abaqus_element_type> elementTypes;
  std::map<abaqus_element_type, unsigned int> nodes_per_element;
  std::map<abaqus_element_type, EntityType> entityTypeMap;
  elementTypes["DC3D8"]                 = abq_eletype_dc3d8;
  nodes_per_element[abq_eletype_dc3d8]  = 8;
  entityTypeMap[abq_eletype_dc3d8]      = MBHEX;

  elementTypes["DCC3D8"]                = abq_eletype_dcc3d8;
  nodes_per_element[abq_eletype_dcc3d8] = 8;
  entityTypeMap[abq_eletype_dcc3d8]     = MBHEX;

  elementTypes["C3D4"]                  = abq_eletype_c3d4;
  nodes_per_element[abq_eletype_c3d4]   = 4;
  entityTypeMap[abq_eletype_c3d4]       = MBTET;

  elementTypes["DC3D4"]                 = abq_eletype_dc3d4;
  nodes_per_element[abq_eletype_dc3d4]  = 4;
  entityTypeMap[abq_eletype_dc3d4]      = MBTET;

  elementTypes["C3D8R"]                 = abq_eletype_c3d8r;
  nodes_per_element[abq_eletype_c3d8r]  = 8;
  entityTypeMap[abq_eletype_c3d8r]      = MBHEX;

  elementTypes["DS4"]                   = abq_eletype_ds4;
  nodes_per_element[abq_eletype_ds4]    = 4;
  entityTypeMap[abq_eletype_ds4]        = MBQUAD;

  abaqus_element_type element_type = abq_eletype_dc3d8;

  bool make_element_set = false;
  std::string element_set_name;

  // Tokenize last line read
  tokenize(readline, tokens, ",\n");
  extract_keyword_parameters(tokens, params);

  // Search for required parameters
  for (std::map<std::string, abaqus_element_params>::iterator thisParam = requiredParams.begin();
      thisParam != requiredParams.end();
      ++thisParam) {
    std::string param_key = match((*thisParam).first, params);
    param = requiredParams[param_key];
    switch (param) {
      case abq_element_type:
        element_type = elementTypes[params[param_key]];
        if (abq_eletype_unsupported == element_type) {
          MB_SET_ERR(MB_FAILURE, "MOAB doesn't currently support this element type");
        }
        //std::cout << "\tAdding ELEMENTS of type: " << params[param_key] << std::endl; // REMOVE
        params.erase(param_key);
        break;
      case abq_element_undefined:
        MB_SET_ERR(MB_FAILURE, "Missing required ELEMENT parameter");
      default:
        break;
    }
  }

  // Process parameters
  for (std::map<std::string, std::string>::iterator thisParam = params.begin();
      thisParam != params.end();
      ++thisParam) {
    // Look for unambiguous match with this node parameter
    param = allowableParams[match((*thisParam).first, allowableParams)];
    switch (param) {
      case abq_element_elset:
        make_element_set = true;
        element_set_name = (*thisParam).second;
        break;
      case abq_element_ambiguous:
        //std::cout << "\t\tIgnoring ambiguous ELEMENT parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
      default:
        //std::cout << "\t\tIgnoring unsupported ELEMENT parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
    }
  }

  std::vector<int> connect_list, element_ids;

  next_line_type = get_next_line_type();

  while (next_line_type != abq_eof && next_line_type != abq_keyword_line) {
    if (abq_data_line == next_line_type) {
      tokenize(readline, tokens, ", \n");
      if (tokens.size() < nodes_per_element[element_type] + 1) {
        MB_SET_ERR(MB_FAILURE, "Not enough data on node data line");
      }
      element_ids.push_back(atoi(tokens[0].c_str()));
      for (unsigned int i = 1; i < nodes_per_element[element_type] + 1; i++)
        connect_list.push_back(atoi(tokens[i].c_str()));
    }

    next_line_type = get_next_line_type();
  }

  int num_elements = element_ids.size();

  // Get and fill element arrays
  EntityHandle start_element = 0;
  EntityHandle *connect;

  status = readMeshIface->get_element_connect(num_elements, nodes_per_element[element_type],
                                              entityTypeMap[element_type], MB_START_ID,
                                              start_element, connect);
  MB_RETURN_IF_FAIL;
  if (0 == start_element)
    return MB_FAILURE;

  // ASSUME: elements must be defined after nodes!
  // Get list of node entity handles and node IDs
  Range node_list;
  status = mdbImpl->get_entities_by_dimension(parent_set, 0, node_list);
  MB_RETURN_IF_FAIL;

  std::vector<int> node_ids(node_list.size());
  status = mdbImpl->tag_get_data(mLocalIDTag, node_list, &node_ids[0]);
  MB_RETURN_IF_FAIL;

  std::map<int, EntityHandle> nodeIdMap;
  for (unsigned int idx = 0; idx < node_list.size(); idx++)
    nodeIdMap[node_ids[idx]] = node_list[idx];

  for (unsigned int node = 0; node < connect_list.size(); node++)
    connect[node] = nodeIdMap[connect_list[node]];

  Range element_range(start_element, start_element + num_elements - 1);

  // Add elements to file_set
  // status = mdbImpl->add_entities(file_set, element_range);
  // MB_RETURN_IF_FAIL;

  // Add elements to this parent_set
  status = mdbImpl->add_entities(parent_set, element_range);
  MB_RETURN_IF_FAIL;

  // Tag elements with their local ID's
  status = mdbImpl->tag_set_data(mLocalIDTag, element_range, &element_ids[0]);
  MB_RETURN_IF_FAIL;

  if (assembly_set) {
    status = mdbImpl->add_entities(assembly_set, element_range);
    MB_RETURN_IF_FAIL;

    std::vector<EntityHandle> tmp_assembly_handles;
    tmp_assembly_handles.assign(element_range.size(), assembly_set);
    status = mdbImpl->tag_set_data(mAssemblyHandleTag, element_range, &(tmp_assembly_handles[0]));
    MB_RETURN_IF_FAIL;
  }

  // These elements don't know their instance_set (probably not defined)

  if (make_element_set) {
    EntityHandle element_set;

    status = add_entity_set(parent_set, ABQ_ELEMENT_SET, element_set_name, element_set);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->add_entities(element_set, element_range);
    MB_RETURN_IF_FAIL;

    // This ad-hoc element set doesn't know its:
    // * part_set (probably parent_set)
    // * instance_set (probably not defined)
    // * assembly_set (probably not defined)
  }

  return MB_SUCCESS;
}
 
ErrorCode ReadABAQUS::read_node_list(EntityHandle parent_set, EntityHandle assembly_set)
{
  ErrorCode status;

  std::vector<std::string> tokens;
  std::map<std::string, std::string> params;
  std::map<std::string, abaqus_node_params> allowableParams;

  allowableParams[ABQ_AMBIGUOUS] = abq_node_ambiguous;
  allowableParams["NSET"] = abq_node_nset;
  allowableParams["SYSTEM"] = abq_node_system;

  abaqus_node_params param;

  bool make_node_set = false;
  std::string node_set_name;

  char coord_system = 'R';

  // Tokenize last line read
  tokenize(readline, tokens, ",\n");
  extract_keyword_parameters(tokens, params);

  //std::cout << "\tAdding NODES"  << std::endl; // REMOVE

  // Process parameters
  for (std::map<std::string, std::string>::iterator thisParam = params.begin();
      thisParam != params.end();
      ++thisParam) {
    // Look for unambiguous match with this node parameter
    param = allowableParams[match((*thisParam).first, allowableParams)];
    switch (param) {
      case abq_node_nset:
        make_node_set = true;
        node_set_name = (*thisParam).second;
        break;
      case abq_node_system:
        // Store coordinate system
        coord_system = (*thisParam).second[0];
        break;
      case abq_node_ambiguous:
        //std::cout << "\t\tIgnoring ambiguous NODE parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
      default:
        //std::cout << "\t\tIgnoring unsupported NODE parameter: " << (*thisParam).first
        //          << "=" << (*thisParam).second << std::endl;
        break;
    }
  }

  std::vector<double> coord_list;
  std::vector<int> node_ids;

  next_line_type = get_next_line_type();

  while (next_line_type != abq_eof && next_line_type != abq_keyword_line) {
    if (abq_data_line == next_line_type) {
      tokenize(readline, tokens, ", \n");
      if (tokens.size() < 4) {
        MB_SET_ERR(MB_FAILURE, "Not enough data on node data line");
      }
      node_ids.push_back(atoi(tokens[0].c_str()));
      for (unsigned int i = 1; i < 4; i++)
        coord_list.push_back(atof(tokens[i].c_str()));
    }

    next_line_type = get_next_line_type();
  }

  unsigned int num_nodes = node_ids.size();
  
  // Transform coordinate systems
  switch (coord_system) {
    case 'R':
      break;
    case 'C':
      cyl2rect(coord_list);
      break;
    case 'S':
      sph2rect(coord_list);
      break;
    default:
      //std::cout << "Treating undefined coordinate system: " << coord_system
      //          << " as rectangular/Cartesian." << std::endl;
      break;
  }

  // Get and fill coordinate arrays
  std::vector<double*> coord_arrays(3);
  EntityHandle start_node = 0;
  status = readMeshIface->get_node_coords(3, num_nodes, MB_START_ID,
                                          start_node, coord_arrays);
  MB_RETURN_IF_FAIL;

  if (0 == start_node)
    return MB_FAILURE;

  // Cppcheck warning (false positive): variable coord_arrays is assigned a value that is never used
  for (unsigned int idx = 0; idx < num_nodes; idx++) {
    coord_arrays[0][idx] = coord_list[idx*3];
    coord_arrays[1][idx] = coord_list[idx*3 + 1];
    coord_arrays[2][idx] = coord_list[idx*3 + 2];
  }

  Range node_range(start_node, start_node + num_nodes - 1);
  // Add nodes to file_set
  // status = mdbImpl->add_entities(file_set, node_range);
  // MB_RETURN_IF_FAIL;

  // Add nodes to this parent_set
  status = mdbImpl->add_entities(parent_set, node_range);
  MB_RETURN_IF_FAIL;

  // Tag nodes with their local ID's
  status = mdbImpl->tag_set_data(mLocalIDTag, node_range, &node_ids[0]);
  MB_RETURN_IF_FAIL;

  if (assembly_set) {
    status = mdbImpl->add_entities(assembly_set, node_range);
    MB_RETURN_IF_FAIL;

    std::vector<EntityHandle> tmp_assembly_handles;
    tmp_assembly_handles.assign(node_range.size(), assembly_set);
    status = mdbImpl->tag_set_data(mAssemblyHandleTag, node_range, &(tmp_assembly_handles[0]));
    MB_RETURN_IF_FAIL;
  }

  // These nodes don't know their instance_set (probably not defined)

  if (make_node_set) {
    EntityHandle node_set;

    status = add_entity_set(parent_set, ABQ_NODE_SET, node_set_name, node_set);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->add_entities(node_set, node_range);
    MB_RETURN_IF_FAIL;

    // This ad-hoc node set doesn't know its:
    // * part_set (probably parent_set)
    // * instance_set (probably not defined)
    // * assembly_set (probably not defined)
  }

  return MB_SUCCESS;
}

// SET CREATION & ACCESS UTILITIES

ErrorCode ReadABAQUS::get_elements_by_id(EntityHandle parent_set,
                                         std::vector<int> element_ids_subset,
                                         Range &element_range)
{
  ErrorCode status;
  Range all_elements;

  status = get_set_elements(parent_set, all_elements);
  MB_RETURN_IF_FAIL;

  std::vector<int> element_ids(all_elements.size());
  status = mdbImpl->tag_get_data(mLocalIDTag, all_elements, &element_ids[0]);
  MB_RETURN_IF_FAIL;

  std::map<int, EntityHandle> elementIdMap;
  for (unsigned int idx = 0; idx < all_elements.size(); idx++)
    elementIdMap[element_ids[idx]] = all_elements[idx];

  for (std::vector<int>::iterator element = element_ids_subset.begin();
       element != element_ids_subset.end();
       ++element)
    element_range.insert(elementIdMap[*element]);

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::get_nodes_by_id(EntityHandle parent_set,
                                      std::vector<int> node_ids_subset,
                                      Range &node_range)
{
  ErrorCode status;

  Range all_nodes;
  status = mdbImpl->get_entities_by_type(parent_set, MBVERTEX, all_nodes);
  MB_RETURN_IF_FAIL;

  std::vector<int> node_ids(all_nodes.size());
  status = mdbImpl->tag_get_data(mLocalIDTag, all_nodes, &node_ids[0]);
  MB_RETURN_IF_FAIL;

  std::map<int, EntityHandle> nodeIdMap;
  for (unsigned int idx = 0; idx < all_nodes.size(); idx++)
    nodeIdMap[node_ids[idx]] = all_nodes[idx];

  for (std::vector<int>::iterator node = node_ids_subset.begin();
      node != node_ids_subset.end();
      ++node)
    node_range.insert(nodeIdMap[*node]);

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::get_set_by_name(EntityHandle parent_set,
                                      int ABQ_set_type,
                                      const std::string &set_name,
                                      EntityHandle &set_handle)
{
  ErrorCode status;

  char this_set_name[ABAQUS_SET_NAME_LENGTH];

  set_handle = 0;

  Range sets;
  void* tag_data[] = {&ABQ_set_type};
  status = mdbImpl->get_entities_by_type_and_tag(parent_set,
                                                 MBENTITYSET,
                                                 &mSetTypeTag,
                                                 tag_data, 1, sets);MB_CHK_SET_ERR(status, "Did not find any sets of that type");

  for (Range::iterator this_set = sets.begin();
      this_set != sets.end() && 0 == set_handle;
      ++this_set) {
    std::fill(this_set_name, this_set_name + ABAQUS_SET_NAME_LENGTH, '\0');
    status = mdbImpl->tag_get_data(mSetNameTag, &(*this_set), 1, &this_set_name[0]);
    if (MB_SUCCESS != status && MB_TAG_NOT_FOUND != status)
      return status;

    if (set_name == std::string(this_set_name))
      set_handle = *this_set;
  }

  if (0 == set_handle) {
    MB_SET_ERR(MB_FAILURE, "Did not find requested set");
  }

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::get_set_elements(EntityHandle set_handle,
                                       Range &element_range)
{
  ErrorCode status;

  Range dim_ent_list;

  // Could have elements of multiple dimensions in this set???
  for (int dim = 1; dim <= 3; dim++) {
    dim_ent_list.clear();
    status = mdbImpl->get_entities_by_dimension(set_handle, dim, dim_ent_list);
    MB_RETURN_IF_FAIL;

    element_range.merge(dim_ent_list);
  }

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::get_set_elements_by_name(EntityHandle parent_set,
                                               int ABQ_set_type,
                                               const std::string &set_name,
                                               Range &element_range)
{
  ErrorCode status;
  
  EntityHandle set_handle;
  status = get_set_by_name(parent_set, ABQ_set_type, set_name, set_handle);
  MB_RETURN_IF_FAIL;
  
  status = get_set_elements(set_handle, element_range);
  MB_RETURN_IF_FAIL;

  if (element_range.size() == 0) {
    //std::cout << "No elements were found in set " << set_name << std::endl;
  }

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::get_set_nodes(EntityHandle parent_set,
                                    int ABQ_set_type,
                                    const std::string &set_name,
                                    Range &node_range)
{
  ErrorCode status;

  EntityHandle set_handle;
  status = get_set_by_name(parent_set, ABQ_set_type, set_name, set_handle);
  MB_RETURN_IF_FAIL;

  Range ent_list;
  Range dim_ent_list;
  // Could have elements of multiple dimensions in this set???
  for (int dim = 0; dim <= 3; dim++) {
    dim_ent_list.clear();
    status = mdbImpl->get_entities_by_dimension(set_handle, dim, dim_ent_list);
    MB_RETURN_IF_FAIL;

    ent_list.merge(dim_ent_list);
  }

  status = mdbImpl->get_adjacencies(ent_list, 0, false, node_range);
  MB_RETURN_IF_FAIL;

  if (node_range.size() == 0) {
    std::cout << "No nodes were found in set " << set_name << std::endl;
  }

  return MB_SUCCESS;
}

Tag ReadABAQUS::get_tag(const char* tag_name,
                        int tag_size,
                        TagType tag_type,
                        DataType tag_data_type,
                        const void* def_val)
{
  Tag retval;

  ErrorCode rval = mdbImpl->tag_get_handle(tag_name, tag_size, tag_data_type,
                                           retval, tag_type | MB_TAG_CREAT,
                                           def_val);
  assert(MB_SUCCESS == rval);
  return MB_SUCCESS == rval ? retval : 0;
}

ErrorCode ReadABAQUS::create_instance_of_part(const EntityHandle file_set,
                                              const EntityHandle assembly_set,
                                              const std::string &part_name,
                                              const std::string &/*instance_name*/,
                                              EntityHandle &instance_set,
                                              const std::vector<double> &translation,
                                              const std::vector<double> &rotation)
{
  ErrorCode status;

  EntityHandle part_set;
  status = get_set_by_name(file_set, ABQ_PART_SET, part_name, part_set);
  MB_RETURN_IF_FAIL;

  // Cross-reference
  status = mdbImpl->tag_set_data(mPartHandleTag, &instance_set, 1, &part_set);
  MB_RETURN_IF_FAIL;

  int instance_id = ++num_part_instances[part_set];
  status = mdbImpl->tag_set_data(mInstancePIDTag, &instance_set, 1, &instance_id);
  MB_RETURN_IF_FAIL;

  status = mdbImpl->tag_set_data(mAssemblyHandleTag,&instance_set, 1, &assembly_set);
  MB_RETURN_IF_FAIL;

  instance_id = ++num_assembly_instances[assembly_set];
  status = mdbImpl->tag_set_data(mInstanceGIDTag, &instance_set, 1, &instance_id);
  MB_RETURN_IF_FAIL;

  // Create maps to cross-reference the part and instance versions of each entity
  std::map<EntityHandle, EntityHandle> p2i_nodes, p2i_elements;

  // ---- NODES ---- 

  // Get all nodes and IDs
  Range part_node_list;
  status = mdbImpl->get_entities_by_dimension(part_set, 0, part_node_list);
  MB_RETURN_IF_FAIL;

  if (0 < part_node_list.size()) {
    std::vector<int> node_ids(part_node_list.size());
    status = mdbImpl->tag_get_data(mLocalIDTag, part_node_list, &node_ids[0]);
    MB_RETURN_IF_FAIL;

    //std::map<int, EntityHandle> nodeIdMap;
    //for (unsigned int idx = 0; idx < part_node_list.size(); idx++)
      //nodeIdMap[node_ids[idx]] = part_node_list[idx];

    // Create new nodes
    std::vector<double*> coord_arrays(3);
    EntityHandle start_node = 0;
    status = readMeshIface->get_node_coords(3, part_node_list.size(), MB_START_ID,
                                            start_node, coord_arrays);
    MB_RETURN_IF_FAIL;

    if (0 == start_node)
      return MB_FAILURE;

    // Copy coordinates into new coord_arrays
    status = mdbImpl->get_coords(part_node_list, coord_arrays[0], coord_arrays[1], coord_arrays[2]);

    // Rotate to new position
    double rot_axis[3];
    rot_axis[0] = rotation[3] - rotation[0];
    rot_axis[1] = rotation[4] - rotation[1];
    rot_axis[2] = rotation[5] - rotation[2];

    AffineXform rotationXform;
    if (rotation[6] != 0)
      rotationXform = AffineXform::rotation(rotation[6]*DEG2RAD, rot_axis);

    // Translate to new position
    for (unsigned int idx = 0; idx < part_node_list.size(); idx++) {
      double coords[3];

      // Transform to new location and then shift origin of rotation
      for (unsigned int dim = 0; dim < 3; dim++)
        coords[dim] = coord_arrays[dim][idx] + translation[dim] - rotation[dim];

      // Rotate around this origin
      if (rotation[6] != 0)
        rotationXform.xform_vector(coords);

      // Transform origin of rotation back
      for (unsigned int dim = 0; dim < 3; dim++)
        coord_arrays[dim][idx] = coords[dim] + rotation[dim];
    }

    Range instance_node_list(start_node, start_node + part_node_list.size() - 1);

    // (DO NOT) add nodes to file_set
    // status = mdbImpl->add_entities(file_set, instance_node_list);
    // MB_RETURN_IF_FAIL;

    // Add nodes to this instance_set
    status = mdbImpl->add_entities(instance_set, instance_node_list);
    MB_RETURN_IF_FAIL;

    // Add nodes to this assembly_set
    status = mdbImpl->add_entities(assembly_set, instance_node_list);
    MB_RETURN_IF_FAIL;

    // Tag nodes with their local ID's
    status = mdbImpl->tag_set_data(mLocalIDTag, instance_node_list, &node_ids[0]);
    MB_RETURN_IF_FAIL;

    // Create a map of old handles to new handles!!!
    for (unsigned int idx = 0; idx < part_node_list.size(); idx++)
      p2i_nodes[part_node_list[idx]] = instance_node_list[idx];
  }

  //  ---- ELEMENTS ----

  Range part_element_list;
  status = get_set_elements(part_set, part_element_list);
  MB_RETURN_IF_FAIL;

  if (0 < part_element_list.size()) {
    std::vector<int> part_element_ids(part_element_list.size());
    status = mdbImpl->tag_get_data(mLocalIDTag, part_element_list, &part_element_ids[0]);
    MB_RETURN_IF_FAIL;

    //std::map<int, EntityHandle> elementIdMap;
    //for (unsigned int idx = 0; idx < part_element_list.size(); idx++)
      //elementIdMap[part_element_ids[idx]] = part_element_list[idx];

    // Create new elements
    Range instance_element_list;
    instance_element_list.clear();

    // Cross-referencing storage and pointers/iterators
    std::vector<int> instance_element_ids;
    std::vector<int>::iterator part_element_id = part_element_ids.begin();

    for (Range::iterator part_element = part_element_list.begin();
        part_element != part_element_list.end();
        ++part_element, ++part_element_id) {
      EntityType element_type = mdbImpl->type_from_handle(*part_element);
      std::vector<EntityHandle> part_connectivity, instance_connectivity;
      EntityHandle new_element;
      status = mdbImpl->get_connectivity(&(*part_element), 1, part_connectivity);
      MB_RETURN_IF_FAIL;

      instance_connectivity.clear();
      for (std::vector<EntityHandle>::iterator connectivity_node = part_connectivity.begin();
          connectivity_node != part_connectivity.end();
          ++connectivity_node)
        instance_connectivity.push_back(p2i_nodes[*connectivity_node]);

      status = mdbImpl->create_element(element_type, &instance_connectivity[0], instance_connectivity.size(), new_element);
      MB_RETURN_IF_FAIL;

      instance_element_list.insert(new_element);
      p2i_elements[*part_element] = new_element;
      instance_element_ids.push_back(*part_element_id);
    }

    // (DO NOT) add elements to file_set
    // status = mdbImpl->add_entities(file_set, instance_element_list);
    // MB_RETURN_IF_FAIL;

    // Add elements to this instance_set
    status = mdbImpl->add_entities(instance_set, instance_element_list);
    MB_RETURN_IF_FAIL;

    // Add elements to this assembly_set
    status = mdbImpl->add_entities(assembly_set, instance_element_list);
    MB_RETURN_IF_FAIL;

    // Tag elements with their local ID's
    status = mdbImpl->tag_set_data(mLocalIDTag, instance_element_list, &(instance_element_ids[0]));
    MB_RETURN_IF_FAIL;
  }

  // ----- NODE SETS -----

  // Get all node sets in part
  Range part_node_sets;
  int tag_val = ABQ_NODE_SET;
  void* tag_data[] = {&tag_val};
  status = mdbImpl->get_entities_by_type_and_tag(part_set,
                                                 MBENTITYSET,
                                                 &mSetTypeTag,
                                                 tag_data, 1, part_node_sets);
  MB_RETURN_IF_FAIL;

  Range part_node_set_list, instance_node_set_list;
  for (Range::iterator part_node_set = part_node_sets.begin();
      part_node_set != part_node_sets.end();
      ++part_node_set) {
    char node_set_name[ABAQUS_SET_NAME_LENGTH];
    std::fill(node_set_name, node_set_name + ABAQUS_SET_NAME_LENGTH, '\0');
    status = mdbImpl->tag_get_data(mSetNameTag, &(*part_node_set), 1, &node_set_name[0]);
    if (MB_SUCCESS != status && MB_TAG_NOT_FOUND != status)
      return status;

    part_node_set_list.clear();
    status = mdbImpl->get_entities_by_dimension(*part_node_set, 0, part_node_set_list);

    instance_node_set_list.clear();
    for (Range::iterator set_node = part_node_set_list.begin();
        set_node != part_node_set_list.end();
        ++set_node)
      instance_node_set_list.insert(p2i_nodes[*set_node]);

    EntityHandle instance_node_set;

    status = add_entity_set(instance_set, ABQ_NODE_SET, node_set_name, instance_node_set);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->add_entities(instance_node_set, instance_node_set_list);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->add_entities(assembly_set, &instance_node_set, 1);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->tag_set_data(mPartHandleTag, &instance_node_set, 1, &part_set);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->tag_set_data(mAssemblyHandleTag, &instance_node_set, 1, &assembly_set);
    MB_RETURN_IF_FAIL;
  }

  // ----- ELEMENT SETS -----

  // Get all element sets in part
  Range part_element_sets;
  tag_val = ABQ_ELEMENT_SET;
  tag_data[0] = &tag_val;
  status = mdbImpl->get_entities_by_type_and_tag(part_set,
                                                 MBENTITYSET,
                                                 &mSetTypeTag,
                                                 tag_data, 1, part_element_sets);
  MB_RETURN_IF_FAIL;

  Range part_element_set_list, instance_element_set_list;
  for (Range::iterator part_element_set = part_element_sets.begin();
       part_element_set != part_element_sets.end();
       ++part_element_set) {
    char element_set_name[ABAQUS_SET_NAME_LENGTH];
    std::fill(element_set_name, element_set_name + ABAQUS_SET_NAME_LENGTH, '\0');
    status = mdbImpl->tag_get_data(mSetNameTag, &(*part_element_set), 1, &element_set_name[0]);
    if (MB_SUCCESS != status && MB_TAG_NOT_FOUND != status)
      return status;

    part_element_set_list.clear();
    status = get_set_elements(*part_element_set, part_element_set_list);

    instance_element_set_list.clear();
    for (Range::iterator set_element = part_element_set_list.begin();
        set_element != part_element_set_list.end();
        ++set_element)
      instance_element_set_list.insert(p2i_elements[*set_element]);

    EntityHandle instance_element_set;
    status = add_entity_set(instance_set, ABQ_ELEMENT_SET, element_set_name, instance_element_set);
    MB_RETURN_IF_FAIL;

    //std::cerr << instance_set << "\t" << instance_element_set << std::endl;
    status = mdbImpl->add_entities(instance_element_set, instance_element_set_list);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->add_entities(assembly_set, &instance_element_set, 1);
    MB_RETURN_IF_FAIL;

    //status = mdbImpl->add_entities(file_set, &instance_element_set, 1);
    //MB_RETURN_IF_FAIL;

    status = mdbImpl->tag_set_data(mPartHandleTag, &instance_element_set, 1, &part_set);
    MB_RETURN_IF_FAIL;

    status = mdbImpl->tag_set_data(mAssemblyHandleTag, &instance_element_set, 1, &assembly_set);
    MB_RETURN_IF_FAIL;

    char element_set_matname[ABAQUS_SET_NAME_LENGTH];
    std::fill(element_set_matname, element_set_matname + ABAQUS_SET_NAME_LENGTH, '\0');
    status = mdbImpl->tag_get_data(mMatNameTag, &(*part_element_set), 1, &element_set_matname[0]);
    if (MB_SUCCESS != status && MB_TAG_NOT_FOUND != status)
      return status;

    if (MB_TAG_NOT_FOUND != status) {
      status = mdbImpl->tag_set_data(mMatNameTag, &instance_element_set, 1, element_set_matname);
      MB_RETURN_IF_FAIL;
    }

    int element_set_mat_id;
    status = mdbImpl->tag_get_data(mMaterialSetTag,&(*part_element_set), 1, &element_set_mat_id);
    if (MB_SUCCESS != status && MB_TAG_NOT_FOUND != status)
      return status;

    if (MB_TAG_NOT_FOUND != status) {
      status = mdbImpl->tag_set_data(mMaterialSetTag, &instance_element_set, 1, &element_set_mat_id);
      MB_RETURN_IF_FAIL;
    }
  }

  // Tag everything with their instance handle
  // some nodes are assigned outside of this routine so query final list of all
  // instance nodes, elements, etc
  Range instance_entity_list;
  status = mdbImpl->get_entities_by_dimension(instance_set, 0, instance_entity_list);
  MB_RETURN_IF_FAIL;

  std::vector<EntityHandle> tmp_instance_handles;
  tmp_instance_handles.assign(instance_entity_list.size(), instance_set);
  status = mdbImpl->tag_set_data(mInstanceHandleTag, instance_entity_list, &tmp_instance_handles[0]);
  MB_RETURN_IF_FAIL;

  instance_entity_list.clear();
  status = get_set_elements(instance_set, instance_entity_list);
  MB_RETURN_IF_FAIL;

  tmp_instance_handles.clear();
  tmp_instance_handles.assign(instance_entity_list.size(), instance_set);
  status = mdbImpl->tag_set_data(mInstanceHandleTag, instance_entity_list, &tmp_instance_handles[0]);
  MB_RETURN_IF_FAIL;

  // Get all node sets in instance
  instance_entity_list.clear();
  tag_val = ABQ_NODE_SET;
  tag_data[0] = &tag_val;
  status = mdbImpl->get_entities_by_type_and_tag(instance_set,
                                                 MBENTITYSET,
                                                 &mSetTypeTag,
                                                 tag_data, 1, instance_entity_list);
  MB_RETURN_IF_FAIL;

  tmp_instance_handles.clear();
  tmp_instance_handles.assign(instance_entity_list.size(), instance_set);
  status = mdbImpl->tag_set_data(mInstanceHandleTag, instance_entity_list, &tmp_instance_handles[0]);
  MB_RETURN_IF_FAIL;

  // Get all element sets in part
  instance_entity_list.clear();
  tag_val = ABQ_ELEMENT_SET;
  tag_data[0] = &tag_val;
  status = mdbImpl->get_entities_by_type_and_tag(instance_set,
                                                 MBENTITYSET,
                                                 &mSetTypeTag,
                                                 tag_data, 1, instance_entity_list);
  MB_RETURN_IF_FAIL;

  tmp_instance_handles.clear();
  tmp_instance_handles.assign(instance_entity_list.size(), instance_set);
  status = mdbImpl->tag_set_data(mInstanceHandleTag, instance_entity_list, &tmp_instance_handles[0]);
  MB_RETURN_IF_FAIL;

  return MB_SUCCESS;
}

ErrorCode ReadABAQUS::add_entity_set(EntityHandle parent_set,
                                     int ABQ_Set_Type,
                                     const std::string &set_name,
                                     EntityHandle &entity_set)
{
  ErrorCode status;

  status = mdbImpl->create_meshset(MESHSET_SET, entity_set);
  MB_RETURN_IF_FAIL;

  status = mdbImpl->tag_set_data(mSetTypeTag, &entity_set, 1, &ABQ_Set_Type);
  MB_RETURN_IF_FAIL;

  status = mdbImpl->tag_set_data(mSetNameTag, &entity_set, 1, set_name.c_str());
  MB_RETURN_IF_FAIL;

  status = mdbImpl->add_entities(parent_set, &entity_set, 1);
  MB_RETURN_IF_FAIL;

  return MB_SUCCESS;
}

void ReadABAQUS::cyl2rect(std::vector<double> coord_list)
{
  int num_nodes = coord_list.size() / 3;
  double x, y, r, t;

  for (int node = 0; node < num_nodes; node++) {
    r = coord_list[3*node];
    t = coord_list[3*node + 1] * DEG2RAD;

    x = r * cos(t);
    y = r * sin(t);

    coord_list[3*node] = x;
    coord_list[3*node + 1] = y;
  }
}

void ReadABAQUS::sph2rect(std::vector<double> coord_list)
{
  int num_nodes = coord_list.size() / 3;
  double x, y, z, r, t, p;

  for (int node = 0; node < num_nodes; node++) {
    r = coord_list[3*node];
    t = coord_list[3*node + 1] * DEG2RAD;
    p = coord_list[3*node + 2] * DEG2RAD;

    x = r * cos(p) * cos(t);
    y = r * cos(p) * sin(t);
    z = r * sin(p);

    coord_list[3*node] = x;
    coord_list[3*node + 1] = y;
    coord_list[3*node + 2] = z;
  }
}

// PARSING RECOGNITION

abaqus_line_types ReadABAQUS::get_next_line_type()
{
  readline.clear();
  std::getline(abFile, readline);
  ++lineNo;

  if (abFile.eof())
    return abq_eof;

  std::string::size_type pos = readline.find_first_not_of(' ');

  if (std::string::npos == pos)
    return abq_blank_line;

  if ('*' == readline[pos])
    if ('*' == readline[pos + 1])
      return abq_comment_line;
    else
      return abq_keyword_line;
  else
    return abq_data_line;
}

abaqus_keyword_type ReadABAQUS::get_keyword()
{
  std::vector<std::string> tokens;
  std::map<std::string, abaqus_keyword_type> keywords;

  // Set up list of supported keywords
  // Note: any attempt to match something not in the keyword list 
  //       using the [] operator will create a new entry in the map
  //       but that entry will have value abq_undefined based on the
  //       definition of the abaqus_keyword_type enum.
  keywords[ABQ_AMBIGUOUS]   = abq_ambiguous;
  keywords["HEADING"]       = abq_heading;
  keywords["PART"]          = abq_part;
  keywords["END PART"]      = abq_end_part;
  keywords["ASSEMBLY"]      = abq_assembly;
  keywords["END ASSEMBLY"]  = abq_end_assembly;
  keywords["NODE"]          = abq_node;
  keywords["ELEMENT"]       = abq_element;
  keywords["NSET"]          = abq_nset;
  keywords["ELSET"]         = abq_elset;
  keywords["SOLID SECTION"] = abq_solid_section;
  keywords["INSTANCE"]      = abq_instance;
  keywords["END INSTANCE"]  = abq_end_instance;

  tokenize(readline, tokens, "*,\n");

  // Convert to upper case and test for unambiguous match/partial match
  stringToUpper(tokens[0], tokens[0]);
  return keywords[match(tokens[0], keywords)];
}

// PARSING UTILITY FUNCTIONS

// For a map of strings to values of type T
// search the key list of the map for an unambiguous partial match with the token
template <typename T>
std::string ReadABAQUS::match(const std::string &token,
                              std::map<std::string, T> &tokenList)
{
  // Initialize with no match and ABQ_UNDEFINED as return string
  bool found_match = false;
  std::string best_match = ABQ_UNDEFINED;

  // Search the map
  for (typename std::map<std::string, T>::iterator thisToken = tokenList.begin();
       thisToken != tokenList.end();
       ++thisToken) {
    // If a perfect match break the loop (assume keyword list is unambiguous)
    if (token == (*thisToken).first) {
      best_match = token;
      break;
    }
    else {
      int short_length = (token.length() < (*thisToken).first.length() ? token.length() : (*thisToken).first.length());
      // If the token matches the first token.length() characters of the keyword
      // consider this a match
      if (token.substr(short_length) == (*thisToken).first.substr(short_length)) {
        if (!found_match) {
          // If no match already, record match and matching keyword
          found_match = true;
          best_match = (*thisToken).first;
        }
        else
          // If match already set matching keyword to ABQ_AMBIGUOUS
          best_match = ABQ_AMBIGUOUS;
      }
    }
  }

  // Possible return values: ABQ_UNDEFINED, keyword from list, ABQ_AMBIGUOUS
  return best_match;
}

// Convert a string to upper case
void ReadABAQUS::stringToUpper(const std::string& toBeConverted, std::string& converted)
{
  converted = toBeConverted;

  for (unsigned int i = 0; i < toBeConverted.length(); i++)
    converted[i] = toupper(toBeConverted[i]);
}

// Extract key/value pairs from parameter list
void ReadABAQUS::extract_keyword_parameters(const std::vector<std::string>& tokens,
                                            std::map<std::string, std::string>& params)
{
  std::string key, value;

  // NOTE: skip first token - it is the keyword
  for (std::vector<std::string>::const_iterator token = tokens.begin() + 1;
       token != tokens.end(); ++token) {
    std::string::size_type pos = token->find('=');
    stringToUpper(token->substr(0, pos), key);
    if (std::string::npos != pos)
      value = token->substr(pos + 1);
    else
      value = "";
    pos = key.find_first_not_of(' ', 0);
    key = key.substr(pos);
    params[key] = value;
  }
}

// Tokenize a string based on a set of possible delimiters
void ReadABAQUS::tokenize(const std::string& str,
                          std::vector<std::string>& tokens,
                          const char* delimiters)
{
  tokens.clear();

  std::string::size_type pos, last = str.find_first_not_of(delimiters, 0);

  while (std::string::npos != last) {
    pos = str.find_first_of(delimiters, last);
    if (std::string::npos == pos) {
      tokens.push_back(str.substr(last));
      last = std::string::npos;
    }
    else {
      tokens.push_back(str.substr(last, pos - last));
      last = str.find_first_not_of(delimiters, pos);
    }
  }
}

} // namespace moab
