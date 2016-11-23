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

#ifndef NAMES_AND_PATHS_H
#define NAMES_AND_PATHS_H

/* Constants for misc paths and names inside the hdf file */

/* The root group of the hdf file, containing all mesh data */
#define ROOT_GROUP             "/tstt/"

/* Object containing history (ExodusII-type QA records) */
#define HISTORY_NAME           "history"
#define HISTORY_PATH           ROOT_GROUP HISTORY_NAME

/* Path to the group containing the tag meta-information and
 * sparse tag data */
#define TAG_GROUP_NAME         "tags"
#define TAG_GROUP              ROOT_GROUP TAG_GROUP_NAME "/"
#define SPARSE_ENTITY_NAME     "id_list"
#define SPARSE_VALUES_NAME     "values"
#define TAG_TYPE_NAME          "type"
#define TAG_VAR_INDICES        "var_indices"

/* Common names for data node/element/set groups */
#define DENSE_TAG_SUBGROUP     TAG_GROUP_NAME "/"
#define ADJACENCY_NAME         "adjacency"
#define CONNECTIVITY_NAME      "connectivity"     
#define POLY_INDEX_NAME        "poly_indices"
#define TYPE_ENUM_PATH         ROOT_GROUP "elemtypes"

/* Node paths */
#define NODE_GROUP_NAME        "nodes"
#define NODE_GROUP             ROOT_GROUP NODE_GROUP_NAME "/"
#define NODE_COORD_NAME        "coordinates"
#define NODE_COORD_PATH        NODE_GROUP NODE_COORD_NAME
#define NODE_TAG_GROUP         NODE_GROUP DENSE_TAG_SUBGROUP
#define NODE_ADJCY_PATH        NODE_GROUP ADJACENCY_NAME

/* MeshSet paths */
#define SET_GROUP_NAME         "sets"
#define SET_GROUP              ROOT_GROUP SET_GROUP_NAME "/"
#define SET_META_NAME          "list"
#define SET_META_PATH          SET_GROUP  SET_META_NAME
#define SET_CHILD_NAME         "children"
#define SET_CHILD_PATH         SET_GROUP  SET_CHILD_NAME
#define SET_PARENT_NAME        "parents"
#define SET_PARENT_PATH        SET_GROUP  SET_PARENT_NAME
#define SET_DATA_NAME          "contents"
#define SET_DATA_PATH          SET_GROUP  SET_DATA_NAME
#define SET_TAG_GROUP          SET_GROUP  DENSE_TAG_SUBGROUP

/* Group for all element types.  Subgroups for each type. */
#define ELEMENT_GROUP_NAME     "elements"
#define ELEMENT_GROUP          ROOT_GROUP ELEMENT_GROUP_NAME "/"

/* Names for attributes placed on objects in the hdf file */
#define TAG_DEFAULT_ATTRIB     "default"
#define TAG_GLOBAL_ATTRIB      "global"
#define TAG_TYPE_ATTRIB        "class"
#define TAG_HANDLE_TYPE_ATTRIB "is_handle"
#define TAG_VARLEN_ATTRIB      "variable_length"
#define ELEM_TYPE_ATTRIB       "element_type"
#define START_ID_ATTRIB        "start_id"
#define MAX_ID_ATTRIB          "max_id"

#endif
