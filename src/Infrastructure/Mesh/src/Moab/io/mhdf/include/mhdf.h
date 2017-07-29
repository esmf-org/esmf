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

#ifndef MHDF_H
#define MHDF_H

#include "moab/mhdf_public.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \page h5mmain H5M File Format API
 *
 *\section Intro   Introduction
 *
 * MOAB's native file format is based on the HDF5 file format.  
 * The most common file extension used for such files is .h5m.
 * A .h5m file can be identified by the top-level \c tstt group
 * in the HDF5 file.
 *
 * The API implemented by this library is a wrapper on top of the
 * underlying HDF5 library.  It provides the following features:
 * - Enforces and hides MOAB's expected file layout 
 * - Provides a slightly higher-level API
 * - Provides some backwards compatibility for file layout changes
 *
 *
 *\section Overview   H5M File Layout
 *
 * The H5M file format relies on the use of a unique entity ID space for
 * all vertices, elements, and entity sets stored in the file.  This
 * ID space is defined by the application.  IDs must be unique over all
 * entity types (a vertex and an entity set may not have the same ID.)
 * The IDs must be positive (non-zero) integer values.  
 * There are no other requirements imposed by the format on the ID space.
 *
 * Elements, with the exception of polyhedra, are defined by a list of 
 * vertex IDs.  Polyhedra are defined by a list of face IDs.  Entity sets 
 * have a list of contained entity IDs, and lists of parent and child 
 * entity set IDs.  The set contents may include any valid entity ID,
 * including other sets.  The parent and child lists are expected to
 * contain only entity IDs corresponding to other entity sets.  A zero
 * entity ID may be used in some contexts (tag data with the mhdf_ENTITY_ID
 * property) to indicate a 'null' value,
 *
 * Element types are defined by the combination of a topology identifier (e.g. 
 * hexahedral topology) and the number of nodes in the element.  
 *
 *
 *\section Root   The tstt Group
 *
 * All file data is stored in the \c tstt group in the HDF5 root group.
 * The \c tstt group may have an optional scalar integer attribute 
 * named \c max_id .  This attribute, if present, should contain the
 * value of the largest entity ID used internally to the file.  It can
 * be used to verify that the code reading the file is using an integer
 * type of sufficient size to accommodate the entity IDs.
 *
 * The \c tstt group contains four sub-groups, a datatype object, and a 
 * dataset object.  The four sub-groups are: \c nodes, \c elements,
 * \c sets, and \c tags.  The dataset is named \c history .
 *
 * The \c elemtypes datatype is an enumeration of the elem topologies
 * used in the file.  The element topologies understood by MOAB are:
 * - \c Edge
 * - \c Tri
 * - \c Quad
 * - \c Polygon
 * - \c Tet
 * - \c Pyramid
 * - \c Prism
 * - \c Knife
 * - \c Hex
 * - \c Polyhedron
 * 
 *
 *\section History   The history DataSet
 *
 * The \c history DataSet is a list of variable-length strings with
 * application-defined meaning.  
 *
 *\section Nodes   The nodes Group
 *
 *
 * The \c nodes group contains a single DataSet and an optional
 * subgroup.  The \c tags subgroup is described in the 
 * \ref Dense "section on dense tag storage".  
 *
 * The \c coordinates
 * DataSet contains the coordinates of all vertices in the mesh.
 * The DataSet should contain floating point values and have a dimensions
 * \f$ n \times d \f$, where \c n is the number of vertices and \c d
 * is the number of coordinate values for each vertex.
 *
 * The \c coordinates DataSet must have an integer attribute named \c start_id .
 * The vertices are then defined to have IDs beginning with this value
 * and increasing sequentially in the order that they are defined in the
 * \c coordinates table.
 *
 *
 *\section Elements   The elements Group
 *
 * The \c elements group contains an application-defined number of 
 * subgroups.  Each subgroup defines one or more mesh elements that
 * have the same topology and length of connectivity (number of nodes
 * for any topology other than \c Polyhedron.)  The names of the subgroups
 * are application defined.  MOAB uses a combination of the element
 * topology name and connectivity length (e.g. "Hex8".).  
 *
 * Each subgroup must have an attribute named \c element_type that 
 * contains one of the enumerated element topology values defined 
 * in the \c elemtypes datatype described in a \ref Root "previous section".
 *
 * Each subgroup contains a single DataSet named \c connectivity and an 
 * optional subgroup named \c tags.  The \c tags subgroup is described in the 
 * \ref Dense "section on dense tag storage". 
 *
 * The \c connectivity DataSet is an \f$ n \times m \f$ array of integer
 * values.  The DataSet contains one row for each of the \c n contained
 * elements, where the connectivity of each element contains \c m IDs.  For
 * all element types supported by MOAB, with the exception of polyhedra,
 * the element connectivity list is expected to contain only IDs 
 * corresponding to nodes.  
 *
 * Each element \c connectivity DataSet must have an integer attribute 
 * named \c start_id .  The elements defined in the connectivity table
 * are defined to have IDs beginning with this value and increasing
 * sequentially in the order that they are defined in the table.
 *
 *
 *\section Sets   The sets Group
 *
 * The \c sets group contains the definitions of any entity sets stored
 * in the file.  It contains 1 to 4 DataSets and the optional \c tags 
 * subgroup.  The \c contents, \c parents, and \c children data sets
 * are one dimensional arrays containing the concatenation of the
 * corresponding lists for all of the sets represented in the file.
 *
 * The \c lists DataSet is a \f$ n \times 4 \f$ table, having one
 * row of four integer values for each set.  The first three values
 * for each set are the indices into the \c contents, \c children, 
 * and \c parents DataSets, respectively, at which the \em last value
 * for set is stored.  The contents, child, and parent lists for
 * sets are stored in the corresponding datasets in the same order as
 * the sets are listed in the \c lists DataSet, such that the index of
 * the first value in one of those tables is one greater than the 
 * corresponding end index in the \em previous row of the table.  The
 * number of content entries, parents, or children for a given set can
 * be calculated as the difference between the corresponding end index
 * entry for the current set and the same entry in the previous row 
 * of the table.  If the first set in the \c lists DataSet had no parent
 * sets, then the corresponding index in the third column of the table
 * would be \c -1.  If it had one parent, the index would be \c 0.  If it
 * had two parents, the index would be \c 1, as the first parent would be
 * stored at position 0 of the \c parents DataSet and the second at position
 * 1.
 *
 * The fourth column of the \c lists DataSet is a series of bit flags
 * defining some properties of the sets.  The four bit values currently
 * defined are:
 *  - 0x1 owner
 *  - 0x2 unique
 *  - 0x4 ordered
 *  - 0x8 range compressed
 *
 * The fourth (most significant) bit indicates that, in the \c contents 
 * data set, that the contents list for the corresponding set is stored
 * using a single range compression.  Rather than storing the IDs of the
 * contained entities individually, each ID \c i is followed by a count 
 * \c n indicating that the set contains the contiguous range of IDs
 * \f$ [i, i+n-1] \f$.
 *
 * The three least significant bits specify intended properties of the
 * set and are unrelated to how the set data is stored in the file.  These
 * properties, described briefly from least significant bit to most 
 * significant are: contained entities should track set membership;
 * the set should contain each entity only once (strict set); and
 * that the order of the entries in the set should be preserved.  
 *
 * Similar to the \c nodes/coordinates and \c elements/.../connectivity
 * DataSets, the \c lists DataSet must have an integer attribute 
 * named \c start_id .  IDs are assigned to to sets in the order that
 * they occur in the \c lists table, beginning with the attribute value.
 *
 * The \c sets group may contain a subgroup names \c tags.  The \c tags 
 * subgroup is described in the \ref Dense "section on dense tag storage". 
 *
 * 
 * \section Tags   The tags Group
 *
 * The \c tags group contains a sub-group for each tag defined
 * in the file.  These sub-groups contain the definition of the
 * tag and may contain some or all of the tag values associated with
 * entities in the file.  However, it should be noted that tag values
 * may also be stored in the "dense" format as described in the 
 * \ref Dense "section on dense tag storage".
 *
 * Each sub-group of the \c tags group contains the definition for
 * a single tag.  The name of each sub-group is the name of the 
 * corresponding tag.  Non-printable characters, characters
 * prohibited in group names in the HDF5 file format, and the
 * backslash ('\') character are encoded
 * in the name string by a backslash ('\') character followed by
 * the ASCII value of the character expressed as a pair of hexadecimal
 * digits.  Thus the backslash character would be represented as \c \5C .
 * Each tag group should also contain a comment which contains the
 * unencoded tag name.
 *
 * The tag sub-group may have any or all of the following four attributes:
 * \c default, \c global, \c is_handle, and \c variable_length.  
 * The \c default attribute, if present,
 * must contain a single tag value that is to be considered the 'default'
 * value of the tag.  The \c global attribute, if present, must contain a
 * single tag value that is the value of the tag as set on the mesh instance
 * (MOAB terminology) or root set (ITAPS terminology.)  The presence of the
 * \c is_handle attribute (the value, if any, is meaningless) indicates
 * that the tag values are to be considered entity IDs.  After reading the
 * file, the reader should map any such tag values to whatever mechanism
 * it uses to reference the corresponding entities read from the file.
 * The presence of the \c variable_length attribute indicates that each 
 * tag value is a variable-length array.  The reader should rely on the
 * presence of this attribute rather than the presence of the \c var_indices
 * DataSet discussed below because the file may contain the definition of
 * a variable length tag without containing any values for that tag.  In such
 * a case, the \c var_indices DataSet will not be present.
 *
 * Each tag sub-group will contain a committed type object named \c type .
 * This type must be the type instance used by the \c global and \c default
 * attributes discussed above and any tag value data sets.  For fixed-length
 * tag data, the tag types understood by MOAB are:
 *  - opaque data
 *  - a single floating point value
 *  - a single integer value
 *  - a bit field
 *  - an array of floating point values
 *  - an array of integer values
 * Any other data types will be treated as opaque data.
 * For Variable-length tag data, MOAB expects the \c type object to be
 * one of:
 *  - opaque data
 *  - a single floating point value
 *  - a single integer value
 *
 * For fixed-length tags, the tag sub-group may contain 'sparse' formatted
 * tag data, which is comprised of two data sets: \c id_list and \c values.
 * Both data sets must be 1-dimensional arrays of the same length.  The 
 * \c id_list data set contains a list of entity IDs and the \c values 
 * data set contains a list of corresponding tag values.  The data stored in
 * the \c values table must be of type \c type.  Fixed-length tag values
 * may also be stored in the "dense" format as described in the 
 * \ref Dense "section on dense tag storage".  A mixture of both sparse-
 * and dense-formatted tag values may be present for a single tag.
 *
 * For variable-length tags the tag values, if any, are always stored
 * in the tag sub-group of the \c tags group and are represented by
 * three one-dimensional data sets: \c id_list, \c var_indices, and \c values.  
 * Similar to the fixed-length sparse-formatted tag data, the \c id_list
 * contains the IDs of the entities for which tag values are defined.
 * The \c values dataset contains the concatenation of the tag values
 * for each of the entities referenced by ID in the \c id_list table, 
 * in the order that the entities are referenced in the \c id_list table.
 * The \c var_indices table contains an index into the \c values data set
 * for each entity in \c id_list.  The index indicates the position of
 * the \em last tag value for the entity in \c values.  The index of
 * the first value is one greater than the 
 * corresponding end index for the \em entry in \c var_indices.  The
 * number of tag values for a given entity can
 * be calculated as the difference between the corresponding end index
 * entry for the current entity and the previous value in the \c var_indices
 * dataset.  
 *
 *
 * \section Dense   The tags Sub-Groups
 *
 * Data for fixed-length tags may also be stored in the \c tags sub-group
 * of the \c nodes, \c sets, and subgroups of the \c elements group.  
 * Values for given tag are stored in a dataset within the \c tags sub-group
 * that has the following properties:
 *  - The name must be the same as that of the tag definition in the main 
 *      \c tags group
 *  - The type of the data set must be the committed type object stored
 *      as \c /tstt/tags/<tagname>/type .
 *  - The data set must have the same length as the data set in the
 *    parent group with the \c start_id attribute.  
 *
 * If dense-formatted data is specified for any entity in the group, then
 * it must be specified for every entity in the group.  The table is 
 * expected to contain one value for each entity in the corresponding 
 * primary definition table (\c /tstt/nodes/coordinates , 
 * \c /tstt/elements/<name>/connectivity , or \c /tstt/sets/list), in the
 * same order as the entities in that primary definition table.
 * 
 */
 
/**
 *\defgroup mhdf MHDF API for reading/writing MOAB-format HDF5 mesh files.
 */
/*@{*/


/**
 *\defgroup mhdf_status Error handling
 */
/*@{*/

/**
 *\defgroup mhdf_group Element group handle
 */
/*@{*/


/** \brief Get an mhdf_ElemHandle object for the node data.  
 *
 * \return A special element group handle used when specifying adjacency or
 * tag data for nodes. 
 */
const char*
mhdf_node_type_handle(void);


/** \brief Return a special element group handle used to specify the set group.
 *
 *  \return A special element group handle used to specify the set group
 *  for reading/writing tag data on sets.
 */
const char*
mhdf_set_type_handle(void);

#define MHDF_INDEX_TYPE H5T_NATIVE_LONG

/** \brief Given an element type Id, get the name. 
 * Fails if buffer is not of sufficient size.
 * \param file_handle The file.
 * \param type_index The type index.  Corresponds to indices into
 *                   the element type list passed to \ref mhdf_createFile.
 * \param buffer     The buffer into which to copy the name.
 * \param buffer_size The length of <code>buffer</code>.
 * \param status     Passed back status of API call.
 */
void
mhdf_getElemName( mhdf_FileHandle file_handle,
                  unsigned int type_index,
                  char* buffer, size_t buffer_size, 
                  mhdf_Status* status );

int
mhdf_checkOpenHandles( mhdf_FileHandle handle, mhdf_Status* status );

/** \brief Common close function for all data handle types.
 *
 * Close an hid_t-type handle returned from any of the following
 * functions.  Any hid_t passed-back or returned must be closed via
 * this function to avoid resource loss.
 *
 * \param file   The file the object pointed to by the passed data
 *               handled exists int.
 * \param handle The data object to close.
 * \param status     Passed back status of API call.
 */
void
mhdf_closeData( mhdf_FileHandle file,
                hid_t handle,
                mhdf_Status* status );

/**\brief Get start ID that will be assigned to next created dataset
 *
 * Get the first_id parameter that will be returned from the next
 * call to any of mhdf_createNodeCoords, mhdf_createConnectivity, 
 * mhdf_createPolyConnectivity, or mhdf_createSetMeta
 */
void
mhdf_getNextStartId( mhdf_FileHandle file,
                     mhdf_index_t* start_id_out,
                     mhdf_Status* status );

/** \brief Write the file history as a list of strings.
 *
 * Each entry is composed of four strings:
 * application, version, date, and time.
 *
 * \param file       The file.
 * \param strings    An array of null-terminated strings.
 * \param num_strings The length of <code>strings</code>
 * \param status     Passed back status of API call.
 */
void
mhdf_writeHistory( mhdf_FileHandle file, 
                   const char** strings, 
                   int num_strings, 
                   mhdf_Status* status );

/** \brief Read the file history as a list of strings.
 *
 * Each entry is composed of four strings:
 * application, version, date, and time.
 *
 * Strings and array are allocated with <code>malloc</code>.  Caller must
 * release them by calling <code>free</code>
 *
 * \param file       The file.
 * \param num_records_out  The length of the returned array.
 * \param status     Passed back status of API call.
 * \return An array of null-terminates strings.
 */
char**
mhdf_readHistory( mhdf_FileHandle file,
                  int* num_records_out, 
                  mhdf_Status* status );
/*@}*/
/**
 *\defgroup mhdf_node Node coordinate data.
 */
/*@{*/

/* Node Coordinates */

int
mhdf_haveNodes( mhdf_FileHandle file_handle, mhdf_Status* status );
                

/** \brief Create new table for node coordinate data 
 *
 * \param file_handle  The file.
 * \param dimension    Number of coordinate values per node.
 * \param num_nodes    The number of nodes the table will contain.
 * \param first_node_id_out  Nodes are assigned IDs sequentially in the
 *             order they occur in the table, where the ID of the first
 *             node in the table is this passed-back value.
 * \param status     Passed back status of API call.
 * \return An HDF5 handle to the coordinate table.
 */
hid_t
mhdf_createNodeCoords( mhdf_FileHandle file_handle,
                       int dimension,
                       long num_nodes,
                       long* first_node_id_out,
                       mhdf_Status* status );

/** \brief Open table containing node coordinate data 
 *
 * \param file_handle       The file.
 * \param dimension_out    Number of coordinate values per node.
 * \param num_nodes_out    The number of nodes the table contains.
 * \param first_node_id_out  Nodes are assigned IDs sequentially in the
 *             order they occur in the table, where the ID of the first
 *             node in the table is this passed-back value.
 * \param status     Passed back status of API call.
 * \return An HDF5 handle to the coordinate table.
 */
hid_t
mhdf_openNodeCoords( mhdf_FileHandle file_handle,
                     long* num_nodes_out,
                     int* dimension_out,
                     long* first_node_id_out,
                     mhdf_Status* status );

hid_t
mhdf_openNodeCoordsSimple( mhdf_FileHandle file_handle, mhdf_Status* status );

/** \brief Write node coordinate data
 *
 * Write interleaved coordinate data for a block of nodes
 *
 * \param data_handle  Handle returned from <code>mhdf_createNodeCoords</code>
 *                     or <code>mhdf_openNodeCoords</code>.
 * \param offset       Table row (node index) at which to start writing.
 * \param count        Number of rows (number of nodes) to write.
 * \param coords       Interleaved node coordinate data.
 * \param status     Passed back status of API call.
 */
void
mhdf_writeNodeCoords( hid_t data_handle,
                      long offset,
                      long count,
                      const double* coords,
                      mhdf_Status* status );
void
mhdf_writeNodeCoordsWithOpt( hid_t data_handle,
                      long offset,
                      long count,
                      const double* coords,
                      hid_t write_prop,
                      mhdf_Status* status );

/** \brief Write node coordinate data
 *
 * Write a single coordinate value (e.g. the 'x' coordinate) for a 
 * block of nodes.
 *
 * \param data_handle  Handle returned from <code>mhdf_createNodeCoords</code>
 *                     or <code>mhdf_openNodeCoords</code>.
 * \param offset       Table row (node index) at which to start writing.
 * \param count        Number of rows (number of nodes) to write.
 * \param dimension    The coordinate to write (0->x, 1->y, ...)
 * \param coords       Coordinate list.
 * \param status     Passed back status of API call.
 */
void
mhdf_writeNodeCoord( hid_t data_handle,
                     long offset,
                     long count,
                     int dimension,
                     const double* coords,
                     mhdf_Status* status );
void
mhdf_writeNodeCoordWithOpt( hid_t data_handle,
                     long offset,
                     long count,
                     int dimension,
                     const double* coords,
                     hid_t write_prop,
                     mhdf_Status* status );

/** \brief Read node coordinate data
 *
 * Read interleaved coordinate data for a block of nodes
 *
 * \param data_handle  Handle returned from <code>mhdf_createNodeCoords</code>
 *                     or <code>mhdf_openNodeCoords</code>.
 * \param offset       Table row (node index) at which to start reading.
 * \param count        Number of rows (number of nodes) to read.
 * \param coordinates  Buffer in which to write node coordinate data.
 * \param status       Passed back status of API call.
 */
void
mhdf_readNodeCoords( hid_t data_handle,
                     long offset,
                     long count,
                     double* coordinates,
                     mhdf_Status* status );
void
mhdf_readNodeCoordsWithOpt( hid_t data_handle,
                     long offset,
                     long count,
                     double* coordinates,
                     hid_t read_prop,
                     mhdf_Status* status );


/** \brief Read node coordinate data
 *
 * Read a single coordinate value (e.g. the 'x' coordinate) for a 
 * block of nodes.
 *
 * \param data_handle  Handle returned from <code>mhdf_createNodeCoords</code>
 *                     or <code>mhdf_openNodeCoords</code>.
 * \param offset       Table row (node index) at which to start reading.
 * \param count        Number of rows (number of nodes) to read.
 * \param dimension    The coordinate to read (0->x, 1->y, ...)
 * \param coords       Buffer in which to write node coordinate data.
 * \param status     Passed back status of API call.
 */
void
mhdf_readNodeCoord( hid_t data_handle,
                    long offset,
                    long count,
                    int dimension,
                    double* coords,
                    mhdf_Status* status );
void
mhdf_readNodeCoordWithOpt( hid_t data_handle,
                    long offset,
                    long count,
                    int dimension,
                    double* coords,
                    hid_t read_prop,
                    mhdf_Status* status );

/*@}*/
/**
 *\defgroup mhdf_conn Element connectivity data.
 */
/*@{*/

/* Element Connectivity */

/** \brief Add a new table of element data to the file.
 *
 * Add a element group to the file. 
 * An element group is the data for a block of elements with the same 
 * TSTT type and same number of nodes in their connectivity data.
 * (e.g. all the MBHEX20 elements).  This function is also
 * used to create the groups for general polygon data and
 * general polyhedron data.  The requirement that all elements
 * have the same number of nodes in their connectivity does not
 * apply for poly(gons|hedra).
 *
 * \param file_handle  File in which to create the element type.
 * \param elem_handle  The name to use for the element data.  This
 *                     name is used as an identifier to reference the
 *                     data for this element type later.  The selected
 *                     name also appears explicitly in the file and 
 *                     therefore should be something
 *                     descriptive of the element type such as the
 *                     'base type' and number of nodes (e.g. "Hex20").
 * \param named_elem_type An index into the list of named element types
 *                     passed to \ref mhdf_createFile .
 * \param status     Passed back status of API call.
 */
void
mhdf_addElement( mhdf_FileHandle file_handle,
                 const char* elem_handle,
                 unsigned int named_elem_type,
                 mhdf_Status* status );

/** \brief Get the list of element groups in the file.
 *
 * Get the list of element groups in the file.
 * An element group is the data for a block of elements with the same 
 * TSTT type and same number of nodes in their connectivity data.
 * (e.g. all the MBHEX20 elements).  This function is also
 * used to retrieve the groups for general polygon data and
 * general polyhedron data.  The requirement that all elements
 * have the same number of nodes in their connectivity does not
 * apply for poly(gons|hedra).
 *
 * \param  file_handle   The file.
 * \param  count_out     Memory location at which to store the
 *                       length of the returned array.
 * \param  status        Passed back status of API call.
 * \return               An array of pointers to element group
 *                       names.  This array is allocated as a 
 *                       single memory block and should be freed
 *                       with <em>one</em> call to free().
 */
char**
mhdf_getElemHandles( mhdf_FileHandle file_handle,
                     unsigned int* count_out,
                     mhdf_Status* status );

/** 
 * \brief Get the element type name for a given element group handle.
 *
 * Fails if name is longer than <code>buf_len</code>.
 *
 * \param file_handle The file.
 * \param elem_handle One of the group names passed back from 
 *                    \ref mhdf_getElemHandles
 * \param buffer      A buffer to copy the name into.
 * \param buf_len     The length of <code>buffer</code>.
 * \param status      Passed back status of API call.
 */
void
mhdf_getElemTypeName( mhdf_FileHandle file_handle,
                      const char* elem_handle,
                      char* buffer, size_t buf_len,
                      mhdf_Status* status );

/** \brief Check if an element group contains polygon or polyhedron
 *
 * Check if an element group contains general polygon or polyhedrons
 * rather than typically fixed-connectivity elements.  
 *
 * \param file_handle The file.
 * \param elem_handle The element group.
 * \param status      Passed back status of API call.
 * \return Zero if normal fixed-connectivity element data.  Non-zero if
 *         poly(gon/hedron) general-connectivity data.
 */
int
mhdf_isPolyElement( mhdf_FileHandle file_handle,
                    const char* elem_handle,
                    mhdf_Status* status );

/** \brief Create connectivity table for an element group
 * 
 * Create fixed-connectivity data for an element group.
 * Do NOT use this function for poly(gon/hedron) data.
 *
 * \param file_handle  The file.
 * \param elem_handle  The element group.
 * \param num_nodes_per_elem The number of nodes in the connectivity data
 *                     for each element.
 * \param num_elements The number of elements to be written to the table.
 * \param first_elem_id_out Elements are assigned global IDs in 
 *                     sequential blocks where the block is the table in
 *                     which their connectivity data is written and the 
 *                     sequence is the sequence in which they are written
 *                     in that table.  The global ID for the first element
 *                     in this group is passed back at this address.  The
 *                     global IDs for all other elements in the table are
 *                     assigned in the sequence in which they are written
 *                     in the table.
 * \param status      Passed back status of API call.
 * \return The HDF5 handle to the connectivity data.
 */
hid_t 
mhdf_createConnectivity( mhdf_FileHandle file_handle,
                         const char* elem_handle,
                         int num_nodes_per_elem,
                         long num_elements,
                         long* first_elem_id_out,
                         mhdf_Status* status );


/** \brief Open connectivity table for an element group
 * 
 * Open fixed-connectivity data for an element group.
 * Do NOT use this function for poly(gon/hedron) data.  Use
 * <code>mhdf_isPolyElement</code> or <code>mhdf_getTsttElemType</code>
 * to check if the data is poly(gon|hedron) data before calling this
 * function to open the data.
 *
 * \param file_handle  The file.
 * \param elem_handle  The element group.
 * \param num_nodes_per_elem_out Used to pass back the number of nodes
 *                     in each element.
 * \param num_elements_out Pass back the number of elements in the table.
 * \param first_elem_id_out Elements are assigned global IDs in 
 *                     sequential blocks where the block is the table in
 *                     which their connectivity data is written and the 
 *                     sequence is the sequence in which they are written
 *                     in that table.  The global ID for the first element
 *                     in this group is passed back at this address.  The
 *                     global IDs for all other elements in the table are
 *                     assigned in the sequence in which they are written
 *                     in the table.
 * \param status      Passed back status of API call.
 * \return The HDF5 handle to the connectivity data.
 */
hid_t
mhdf_openConnectivity( mhdf_FileHandle file_handle,
                       const char* elem_handle,
                       int* num_nodes_per_elem_out,
                       long* num_elements_out,
                       long* first_elem_id_out,
                       mhdf_Status* status );

hid_t
mhdf_openConnectivitySimple( mhdf_FileHandle file_handle, 
                             const char* elem_handle,
                             mhdf_Status* status );
                             

/** \brief Write element coordinate data
 *
 * Write interleaved fixed-connectivity element data for a block of elements.
 * Note: Do not use this for polygon or polyhedron data. 
 *
 * \param data_handle  Handle returned from <code>mhdf_createConnectivity</code>
 *                     or <code>mhdf_openConnectivity</code>.
 * \param offset       Table row (element index) at which to start writing.
 * \param count        Number of rows (number of elements) to write.
 * \param hdf_integer_type The type of the integer data in node_id_list.
 *                     Typically <code>H5T_NATIVE_INT</code> or
 *                     <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                     The HDF class of this type object <em>must</em> be H5T_INTEGER
 * \param node_id_list Interleaved connectivity data specified as global node IDs.
 * \param status       Passed back status of API call.
 */
void
mhdf_writeConnectivity( hid_t data_handle,
                        long offset,
                        long count,
                        hid_t hdf_integer_type,
                        const void* node_id_list,
                        mhdf_Status* status );
void
mhdf_writeConnectivityWithOpt( hid_t data_handle,
                        long offset,
                        long count,
                        hid_t hdf_integer_type,
                        const void* node_id_list,
                        hid_t write_prop,
                        mhdf_Status* status );

/** \brief Read element coordinate data
 *
 * Read interleaved fixed-connectivity element data for a block of elements.
 * Note: Do not use this for polygon or polyhedron data. 
 *
 * \param data_handle  Handle returned from <code>mhdf_createConnectivity</code>
 *                     or <code>mhdf_openConnectivity</code>.
 * \param offset       Table row (element index) at which to start read.
 * \param count        Number of rows (number of elements) to read.
 * \param hdf_integer_type The type of the integer data in node_id_list.
 *                     Typically <code>H5T_NATIVE_INT</code> or
 *                     <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                     The HDF class of this type object <em>must</em> be H5T_INTEGER
 * \param node_id_list Pointer to memory at which to write interleaved
 *                     connectivity data specified as global node IDs.
 * \param status       Passed back status of API call.
 */
void 
mhdf_readConnectivity( hid_t data_handle,
                       long offset,
                       long count,
                       hid_t hdf_integer_type,
                       void* node_id_list,
                       mhdf_Status* status );
void 
mhdf_readConnectivityWithOpt( hid_t data_handle,
                       long offset,
                       long count,
                       hid_t hdf_integer_type,
                       void* node_id_list,
                       hid_t read_prop,
                       mhdf_Status* status );

/* Poly(gon|hedra) */


/** \brief Create a new table for polygon or polyhedron connectivity data.
 *
 * Poly (polygon or polyhedron) connectivity is stored as two lists.
 * One list is the concatenation of the the connectivity data for
 * all the polys in the group.  The other contains one value per
 * poly where that value is the index of the last entry in the
 * connectivity of the corresponding poly.  The ID
 * list for polygons contains global node IDs.  The ID list for polyhedra
 * contains the global IDs of faces (either polygons or 2D fixed-connectivity
 * elements.)
 *
 * \param file_handle  The file to write.
 * \param elem_handle  The element group.
 * \param num_poly     The total number number of polygons or polyhedra to
 *                     be written in the table.
 * \param data_list_length The total number of values to be written to the
 *                     table (the number of polys plus the sum of the number
 *                     of entities in each poly's connectivity data.)
 * \param first_id_out Elements are assigned global IDs in 
 *                     sequential blocks where the block is the table in
 *                     which their connectivity data is written and the 
 *                     sequence is the sequence in which they are written
 *                     in that table.  The global ID for the first element
 *                     in this group is passed back at this address.  The
 *                     global IDs for all other elements in the table are
 *                     assigned in the sequence in which they are written
 *                     in the table.
 * \param idx_and_id_handles_out The handles for the index list and
 *                     connectivity list, respectively.
 * \param status       Passed back status of API call.
 */
void
mhdf_createPolyConnectivity( mhdf_FileHandle file_handle,
                             const char* elem_handle,
                             long num_poly,
                             long data_list_length,
                             long* first_id_out,
                             hid_t idx_and_id_handles_out[2],
                             mhdf_Status* status );

/** \brief Open a table of polygon or polyhedron connectivity data.
 *
 * Poly (polygon or polyhedron) connectivity is stored as two lists.
 * One list is the concatenation of the the connectivity data for
 * all the polys in the group.  The other contains one value per
 * poly where that value is the index of the last entry in the
 * connectivity of the corresponding poly.  The ID
 * list for polygons contains global node IDs.  The ID list for polyhedra
 * contains the global IDs of faces (either polygons or 2D fixed-connectivity
 * elements.)
 *
 * \param file_handle  The file to write.
 * \param elem_handle  The element group.
 * \param num_poly_out The total number number of polygons or polyhedra to
 *                     be written in the table.
 * \param data_list_length_out The total number of values to be written to the
 *                     table (the number of polys plus the sum of the number
 *                     of entities in each poly's connectivity data.)
 * \param first_id_out Elements are assigned global IDs in 
 *                     sequential blocks where the block is the table in
 *                     which their connectivity data is written and the 
 *                     sequence is the sequence in which they are written
 *                     in that table.  The global ID for the first element
 *                     in this group is passed back at this address.  The
 *                     global IDs for all other elements in the table are
 *                     assigned in the sequence in which they are written
 *                     in the table.
 * \param idx_and_id_handles_out The handles for the index list and
 *                     connectivity list, respectively.
 * \param status       Passed back status of API call.
 */
void
mhdf_openPolyConnectivity( mhdf_FileHandle file_handle,
                           const char* elem_handle,
                           long* num_poly_out,
                           long* data_list_length_out,
                           long* first_id_out,
                           hid_t idx_and_id_handles_out[2],
                           mhdf_Status* status );

/** \brief Write polygon or polyhedron index data.
 *
 * Poly (polygon or polyhedron) connectivity is stored as two lists.
 * One list is the concatenation of the the connectivity data for
 * all the polys in the group.  The other contains one value per
 * poly where that value is the index of the last entry in the
 * connectivity of the corresponding poly.  The ID
 * list for polygons contains global node IDs.  The ID list for polyhedra
 * contains the global IDs of faces (either polygons or 2D fixed-connectivity
 * elements.)
 *
 * This function writes the index list.
 *
 * \param poly_handle  The handle returned from 
 *                     <code>mhdf_createPolyConnectivity</code> or
 *                     <code>mhdf_openPolyConnectivity</code>.
 * \param offset       The offset in the table at which to write.  The
 *                     offset is in terms of the integer values in the table,
 *                     not the count of polys.
 * \param count        The size of the integer list to write.
 * \param hdf_integer_type The type of the integer data in <code>id_list</code>.
 *                     Typically <code>H5T_NATIVE_INT</code> or
 *                     <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                     The HDF class of this type object <em>must</em> be H5T_INTEGER
 * \param index_list   The index list for the polys.
 * \param status       Passed back status of API call.
 */
void
mhdf_writePolyConnIndices( hid_t poly_handle,
                           long offset,
                           long count,
                           hid_t hdf_integer_type,
                           const void* index_list,
                           mhdf_Status* status );
void
mhdf_writePolyConnIndicesWithOpt( hid_t poly_handle,
                           long offset,
                           long count,
                           hid_t hdf_integer_type,
                           const void* index_list,
                           hid_t write_prop,
                           mhdf_Status* status );

/** \brief Write polygon or polyhedron connectivity data.
 *
 * Poly (polygon or polyhedron) connectivity is stored as two lists.
 * One list is the concatenation of the the connectivity data for
 * all the polys in the group.  The other contains one value per
 * poly where that value is the index of the last entry in the
 * connectivity of the corresponding poly.  The ID
 * list for polygons contains global node IDs.  The ID list for polyhedra
 * contains the global IDs of faces (either polygons or 2D fixed-connectivity
 * elements.)
 *
 * This function writes the connectivity/ID list.
 *
 * \param poly_handle  The handle returned from 
 *                     <code>mhdf_createPolyConnectivity</code> or
 *                     <code>mhdf_openPolyConnectivity</code>.
 * \param offset       The offset in the table at which to write.  The
 *                     offset is in terms of the integer values in the table,
 *                     not the count of polys.
 * \param count        The size of the integer list to write.
 * \param hdf_integer_type The type of the integer data in <code>id_list</code>.
 *                     Typically <code>H5T_NATIVE_INT</code> or
 *                     <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                     The HDF class of this type object <em>must</em> be H5T_INTEGER
 * \param id_list      The count/global ID list specifying the connectivity 
 *                     of the polys.
 * \param status       Passed back status of API call.
 */
void
mhdf_writePolyConnIDs( hid_t poly_handle,
                       long offset,
                       long count,
                       hid_t hdf_integer_type,
                       const void* id_list,
                       mhdf_Status* status );
void
mhdf_writePolyConnIDsWithOpt( hid_t poly_handle,
                       long offset,
                       long count,
                       hid_t hdf_integer_type,
                       const void* id_list,
                       hid_t write_prop,
                       mhdf_Status* status );

/** \brief Read polygon or polyhedron index data.
 *
 * Poly (polygon or polyhedron) connectivity is stored as two lists.
 * One list is the concatenation of the the connectivity data for
 * all the polys in the group.  The other contains one value per
 * poly where that value is the index of the last entry in the
 * connectivity of the corresponding poly.  The ID
 * list for polygons contains global node IDs.  The ID list for polyhedra
 * contains the global IDs of faces (either polygons or 2D fixed-connectivity
 * elements.)
 *
 * \param poly_handle  The handle returned from 
 *                     <code>mhdf_createPolyConnectivity</code> or
 *                     <code>mhdf_openPolyConnectivity</code>.
 * \param offset       The offset in the table at which to read.  The
 *                     offset is in terms of the integer values in the table,
 *                     not the count of polys.
 * \param count        The size of the integer list to read.
 * \param hdf_integer_type The type of the integer data as written into memory.
 *                     Typically <code>H5T_NATIVE_INT</code> or
 *                     <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                     The HDF class of this type object <em>must</em> be H5T_INTEGER
 * \param index_list   The memory location at which to write the indices.
 * \param status       Passed back status of API call.
 */
void 
mhdf_readPolyConnIndices( hid_t poly_handle,
                          long offset,
                          long count,
                          hid_t hdf_integer_type,
                          void* index_list,
                          mhdf_Status* status );
void 
mhdf_readPolyConnIndicesWithOpt( hid_t poly_handle,
                          long offset,
                          long count,
                          hid_t hdf_integer_type,
                          void* index_list,
                          hid_t read_prop,
                          mhdf_Status* status );

/** \brief Read polygon or polyhedron connectivity data.
 *
 * Poly (polygon or polyhedron) connectivity is stored as two lists.
 * One list is the concatenation of the the connectivity data for
 * all the polys in the group.  The other contains one value per
 * poly where that value is the index of the last entry in the
 * connectivity of the corresponding poly.  The ID
 * list for polygons contains global node IDs.  The ID list for polyhedra
 * contains the global IDs of faces (either polygons or 2D fixed-connectivity
 * elements.)
 *
 * \param poly_handle  The handle returned from 
 *                     <code>mhdf_createPolyConnectivity</code> or
 *                     <code>mhdf_openPolyConnectivity</code>.
 * \param offset       The offset in the table at which to read.  The
 *                     offset is in terms of the integer values in the table,
 *                     not the count of polys.
 * \param count        The size of the integer list to read.
 * \param hdf_integer_type The type of the integer data as written into memory.
 *                     Typically <code>H5T_NATIVE_INT</code> or
 *                     <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                     The HDF class of this type object <em>must</em> be H5T_INTEGER
 * \param id_list      The memory location at which to write the connectivity data.
 * \param status       Passed back status of API call.
 */
void 
mhdf_readPolyConnIDs( hid_t poly_handle,
                      long offset,
                      long count,
                      hid_t hdf_integer_type,
                      void* id_list,
                      mhdf_Status* status );
void 
mhdf_readPolyConnIDsWithOpt( hid_t poly_handle,
                      long offset,
                      long count,
                      hid_t hdf_integer_type,
                      void* id_list,
                      hid_t read_prop,
                      mhdf_Status* status );
/*@}*/
/**
 *\defgroup mhdf_adj Adjacency data.
 *
 * Adjacency data is formated as a sequence of integer groups where
 * the first entry in each group is the ID of the element for which
 * adjacencies are being specified, the second value is the count of
 * adjacent entities, and the remainder of the group is the list of
 * IDs of the adjacent entities.
 */
/*@{*/
                     

/** \brief Create adjacency data table for nodes, elements, polys, etc. 
 * 
 * Create file object for adjacency data for a nodes or a specified
 * element group.
 *
 * Adjacency data is formated as a sequence of integer groups where
 * the first entry in each group is the ID of the element for which
 * adjacencies are being specified, the second value is the count of
 * adjacent entities, and the remainder of the group is the list of
 * IDs of the adjacent entities.
 *
 * \param file_handle The file.
 * \param elem_handle The element group (or the result of
 *                    <code>mhdf_node_type_handle</code> for nodes) for
 *                    which the adjacency data is to be specified.
 * \param adj_list_size The total number of integer values contained
 *                    in the adjacency data for the specified element group.
 * \param status       Passed back status of API call.
 * \return The HDF5 handle to the connectivity data.
 */
hid_t
mhdf_createAdjacency( mhdf_FileHandle file_handle,
                      const char* elem_handle,
                      long adj_list_size,
                      mhdf_Status* status );

/** \brief Check if adjacency data is present in the file for the specified
 *  element group.
 *
 * \param file         The file.
 * \param elem_handle  A handle to an element group.  
 * \param status       Passed back status of API call.
 */
int
mhdf_haveAdjacency( mhdf_FileHandle file,
                    const char* elem_handle,
                    mhdf_Status* status );

/** \brief Open adjacency data table for nodes, elements, polys, etc. 
 * 
 * Open the file object containing adjacency data for a nodes or a specified
 * element group.
 *
 * Adjacency data is formated as a sequence of integer groups where
 * the first entry in each group is the ID of the element for which
 * adjacencies are being specified, the second value is the count of
 * adjacent entities, and the remainder of the group is the list of
 * IDs of the adjacent entities.
 *
 * \param file_handle The file.
 * \param elem_handle The element group (or the result of
 *                    <code>mhdf_node_type_handle</code> for nodes) for
 *                    which the adjacency data is to be specified.
 * \param adj_list_size The total number of integer values contained
 *                    in the adjacency data for the specified element group.
 * \param status       Passed back status of API call.
 * \return The HDF5 handle to the connectivity data.
 */
hid_t
mhdf_openAdjacency( mhdf_FileHandle file_handle,
                    const char* elem_handle,
                    long* adj_list_size,
                    mhdf_Status* status );

/** \brief Write node/element adjacency data
 *
 * Write adjacency data.
 *
 * Adjacency data is formated as a sequence of integer groups where
 * the first entry in each group is the ID of the element for which
 * adjacencies are being specified, the second value is the count of
 * adjacent entities, and the remainder of the group is the list of
 * IDs of the adjacent entities.
 *
 * \param data_handle  Handle returned from <code>mhdf_createAdjacency</code>
 *                     or <code>mhdf_openAdjacency</code>.
 * \param offset       List position at which to start writing.  Offset is
 *                     from the count if integer values written, NOT a count
 *                     of the number of elements for which adjacency data
 *                     is written.
 * \param count        Number of integer values to write.
 * \param hdf_integer_type The type of the integer data in <code>adj_list_data</code>.
 *                     Typically <code>H5T_NATIVE_INT</code> or
 *                     <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                     The HDF class of this type object <em>must</em> be H5T_INTEGER
 * \param adj_list_data Adjacency data to write.
 * \param status       Passed back status of API call.
 */
void
mhdf_writeAdjacency( hid_t data_handle,
                     long offset,
                     long count,
                     hid_t hdf_integer_type,
                     const void* adj_list_data,
                     mhdf_Status* status );
void
mhdf_writeAdjacencyWithOpt( hid_t data_handle,
                     long offset,
                     long count,
                     hid_t hdf_integer_type,
                     const void* adj_list_data,
                     hid_t write_prop,
                     mhdf_Status* status );

/** \brief Read node/element adjacency data
 *
 * Read adjacency data.
 *
 * Adjacency data is formated as a sequence of integer groups where
 * the first entry in each group is the ID of the element for which
 * adjacencies are being specified, the second value is the count of
 * adjacent entities, and the remainder of the group is the list of
 * IDs of the adjacent entities.
 *
 * \param data_handle  Handle returned from <code>mhdf_createAdjacency</code>
 *                     or <code>mhdf_openAdjacency</code>.
 * \param offset       List position at which to start reading.  Offset is
 *                     from the count if integer values written, NOT a count
 *                     of the number of elements for which adjacency data
 *                     is written.
 * \param count        Number of integer values to reading.
 * \param hdf_integer_type The type of the integer data in <code>adj_list_data_out</code>.
 *                     Typically <code>H5T_NATIVE_INT</code> or
 *                     <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                     The HDF class of this type object <em>must</em> be H5T_INTEGER
 * \param adj_list_data_out Pointer to memory at which to write adjacency data.
 * \param status       Passed back status of API call.
 */
void
mhdf_readAdjacency( hid_t data_handle,
                    long offset,
                    long count,
                    hid_t hdf_integer_type,
                    void* adj_list_data_out,
                    mhdf_Status* status );
void
mhdf_readAdjacencyWithOpt( hid_t data_handle,
                    long offset,
                    long count,
                    hid_t hdf_integer_type,
                    void* adj_list_data_out,
                    hid_t read_prop,
                    mhdf_Status* status );

/*@}*/

/**
 *\defgroup mhdf_set Meshset data.
 *
 * Meshset data is divided into three groups of data.  The set-list/meta-information table,
 * the set contents table and the set children table.  Each is written and read independently.
 *
 * The set list table contains one row for each set.  Each row contains four values:
 * {content list end index, child list end index, parent list end index, and flags}.  The flags 
 * value is a collection of bits with
 * values defined in \ref mhdf_set_flag .  The all the flags except \ref mhdf_SET_RANGE_BIT are
 * saved properties of the mesh data and are not relevant to the actual file in any way.  The
 * \ref mhdf_SET_RANGE_BIT flag is a toggle for how the meshset contents (not children) are saved.
 * It is an internal property of the file format and should not be passed on to the mesh database.
 * The content list end index and child list end index are the indices of the last entry for the
 * set in the contents and children tables respectively.  In the case where a set has either no
 * children or no contents, the last index of should be the same as the last index of the previous
 * set in the table, or -1 for the first set in the table.  Thus the first index is always one
 * greater than the last index of the previous set.  If the first index, calculated as one greater
 * that the last index of the previous set is greater than the last index of the current set, then
 * there are no values in the corresponding contents or children table for that set.
 *
 * The set contents table is a vector of integer global IDs that is the concatenation of the contents
 * data for all of the mesh sets.  The values are stored corresponding to the order of the sets
 * in the set list table.  Depending on the value of \ref mhdf_SET_RANGE_BIT in the flags field of
 * the set list table, the contents for a specific set may be stored in one of two formats.  If the
 * flag is set, the contents list is a list of pairs where each pair is a starting global Id and a 
 * count.  For each pair, the set contains the range of global Ids beginning at the start value. 
 * If the \ref mhdf_SET_RANGE_BIT flag is not set, the meshset contents are a simple list of global Ids.
 *
 * The meshset child table is a vector of integer global IDs.  It is a concatenation of the child
 * lists for all the mesh sets, in the order the sets occur in the meshset list table.  The values
 * are always simple lists.  The child table may never contain ranges of IDs.
 */
/*@{*/


/**
 *\defgroup mhdf_set_flag Set flag bits
 */
/*@{*/

/** \brief Make entities in set aware of owning set (MOAB-specific?)*/
#define mhdf_SET_OWNER_BIT 0x1  
/** \brief Set cannot contain duplicates */ 
#define mhdf_SET_UNIQUE_BIT 0x2  
/** \brief Set order is preserved */
#define mhdf_SET_ORDER_BIT 0x4  

/** \brief The bit specifying set storage format.
 *
 * If this bit is set, then the contents of a set (not the children) 
 * is written as set of ranges, where each range is of the form
 * {global start id, count}.  For such a range, the set contains the
 * <code>count</code> entities with sequential global IDs beginning
 * with the specified start ID.  If this bit is not set in the set flags,
 * the contents of the set are stored as a simple list of global IDs.
 */
#define mhdf_SET_RANGE_BIT 0x8

/*@}*/

/** \brief Create table holding list of meshsets and their properties.
 * 
 * The set table contains description of sets, but not contents or
 * children.  The table is a <code>n x 4</code> matrix of values.  
 * One row for each of <code>n</code> sets.  Each row contains the end index
 * for the set in the contents table, the end index for the set in the children
 * table, the end index for the set in the parents table, and the set flags, 
 * respectively. The \ref mhdf_SET_RANGE_BIT
 * bit in the flags specifies the format of the contents list for each set.
 * See a description of the \ref mhdf_SET_RANGE_BIT flag for a description
 * of the two possible data formats.  The index values in the first two columns
 * of the table are the index of the <em>last</em> value for the set in the corresponding
 * contents and children lists.  The first index is always one greater than the last index
 * for the previous set in the table.  The first index of the first set in the table is
 * implicitly zero.  A special value of -1 in the appropriate column should be used to 
 * indicate that the first set contains no contents or has no children.  For any other set,
 * if the last index for the set is the same as that of the previous set, it has no data
 * in the corresponding list. 
 *
 *\param file_handle  The file.
 *\param num_sets     The number of sets in the table.
 *\param first_set_id_out  The global ID that will be assigned to the first
 *                    set in the table.  All subsequent sets in the table
 *                    will be assigned sequential global IDs.
 * \param status       Passed back status of API call.
 *\return The handle to the set meta-data table.  
 */
hid_t
mhdf_createSetMeta( mhdf_FileHandle file_handle,
                    long num_sets,
                    long* first_set_id_out,
                    mhdf_Status* status );

/** \brief Check if file contains any sets
 *
 *\param file               The file.
 *\param have_set_data_out  If non-null set to 1 if file contains table
 *                          of set contents, zero otherwise.
 *\param have_set_child_out If non-null set to 1 if file contains table
 *                          of set children, zero otherwise.
 *\param have_set_parents_out If non-null set to 1 if file contains table
 *                          of set parents, zero otherwise.
 * \param status       Passed back status of API call.
 *\return Zero if the file does not contain any sets, one if it does.
 */
int
mhdf_haveSets( mhdf_FileHandle file,
               int* have_set_data_out,
               int* have_set_child_out,
               int* have_set_parents_out,
               mhdf_Status* status );

/** \brief Open table holding list of meshsets and their properties.
 * 
 * Open set list.  
 * See \ref mhdf_createSetMeta or \ref mhdf_set for a description of this data.
 *
 *\param file_handle  The file.
 *\param num_sets_out The number of sets in the table.
 *\param first_set_id_out  The global ID that will of the first
 *                    set in the table.  All subsequent sets in the table
 *                    have sequential global IDs.
 * \param status       Passed back status of API call.
 *\return The handle to the set meta-data table.  
 */
hid_t
mhdf_openSetMeta( mhdf_FileHandle file_handle,
                  long* num_sets_out,
                  long* first_set_id_out,
                  mhdf_Status* status );

hid_t
mhdf_openSetMetaSimple( mhdf_FileHandle file_handle, mhdf_Status* status );

/** \brief Read list of sets and meta-information about sets.
 *
 * Read set descriptions.  See \ref mhdf_createSetMeta or \ref mhdf_set 
 * for a description of this data.
 *
 *\param data_handle The handle returned from \ref mhdf_createSetMeta or
 *                   \ref mhdf_openSetMeta.
 *\param offset      The offset (set index) to begin reading at.
 *\param count       The number of rows (sets, integer triples) to read.
 *\param hdf_integer_type The type of the integer data in <code>set_desc_data</code>.
 *                   Typically <code>H5T_NATIVE_INT</code> or
 *                   <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                   The HDF class of this type object <em>must</em> be H5T_INTEGER
 * \param status       Passed back status of API call.
 *\param set_desc_data The memory location at which to write the data.
 */
void
mhdf_readSetMeta( hid_t data_handle,
                  long offset,
                  long count,
                  hid_t hdf_integer_type,
                  void* set_desc_data,  
                  mhdf_Status* status );
void
mhdf_readSetMetaWithOpt( hid_t data_handle,
                  long offset,
                  long count,
                  hid_t hdf_integer_type,
                  void* set_desc_data,  
                  hid_t read_prop,
                  mhdf_Status* status );

/** \brief Read only the flags portion of the set description table
 *
 * Read flags for each set from the set description table.
 * See \ref mhdf_createSetMeta for a description of this data.
 *\param data_handle The handle returned from mdhf_createSetMeta or mhdf_openSetMeta
 *\param offset      The offset (set index) at which to begin reading.
 *\param count       The number of values (number of sets) to read.
 *\param hdf_integer_type The integer type of the input array 'set_flag_data'.
 *\param set_flag_data Array of integers.
 *\param status      Location at which to store status of API call.
 */
void
mhdf_readSetFlags( hid_t data_handle,
                   long offset,
                   long count,
                   hid_t hdf_integer_type,
                   void* set_flag_data,
                   mhdf_Status* status );
void
mhdf_readSetFlagsWithOpt( hid_t data_handle,
                   long offset,
                   long count,
                   hid_t hdf_integer_type,
                   void* set_flag_data,
                   hid_t read_prop,
                   mhdf_Status* status );


/** \brief Read only the content end indices portion of the set description table
 *
 * For each set, read the last index of that set's data in the set
 * contents table. 
 *
 * NOTE: This is a signed value.  Any sets w/out contents that occur
 *       first in the list will have an end index of -1.
 * 
 *\param data_handle The handle returned from mdhf_createSetMeta or mhdf_openSetMeta
 *\param offset      The offset (set index) at which to begin reading.
 *\param count       The number of values (number of sets) to read.
 *\param hdf_integer_type The integer type of the input array 'set_flag_data'.
 *\param end_indices_out Array of indices.
 *\param status      Location at which to store status of API call.
 */
void
mhdf_readSetContentEndIndices( hid_t data_handle,
                               long offset,
                               long count,
                               hid_t hdf_integer_type,
                               void* end_indices_out,
                               mhdf_Status* status );
void
mhdf_readSetContentEndIndicesWithOpt( hid_t data_handle,
                               long offset,
                               long count,
                               hid_t hdf_integer_type,
                               void* end_indices_out,
                               hid_t read_prop,
                               mhdf_Status* status );

/** \brief Read only the child end indices portion of the set description table
 *
 * For each set, read the last index of that set's data in the set
 * children table. 
 *
 * NOTE: This is a signed value.  Any sets w/out contents that occur
 *       first in the list will have an end index of -1.
 * 
 *\param data_handle The handle returned from mdhf_createSetMeta or mhdf_openSetMeta
 *\param offset      The offset (set index) at which to begin reading.
 *\param count       The number of values (number of sets) to read.
 *\param hdf_integer_type The integer type of the input array 'set_flag_data'.
 *\param end_indices_out Array of indices.
 *\param status      Location at which to store status of API call.
 */
void
mhdf_readSetChildEndIndices( hid_t data_handle,
                             long offset,
                             long count,
                             hid_t hdf_integer_type,
                             void* end_indices_out,
                             mhdf_Status* status );
void
mhdf_readSetChildEndIndicesWithOpt( hid_t data_handle,
                             long offset,
                             long count,
                             hid_t hdf_integer_type,
                             void* end_indices_out,
                             hid_t read_prop,
                             mhdf_Status* status );

/** \brief Read only the parent end indices portion of the set description table
 *
 * For each set, read the last index of that set's data in the set
 * parents table. 
 *
 * NOTE: This is a signed value.  Any sets w/out contents that occur
 *       first in the list will have an end index of -1.
 * 
 *\param data_handle The handle returned from mdhf_createSetMeta or mhdf_openSetMeta
 *\param offset      The offset (set index) at which to begin reading.
 *\param count       The number of values (number of sets) to read.
 *\param hdf_integer_type The integer type of the input array 'set_flag_data'.
 *\param end_indices_out Array of indices.
 *\param status      Location at which to store status of API call.
 */
void
mhdf_readSetParentEndIndices( hid_t data_handle,
                              long offset,
                              long count,
                              hid_t hdf_integer_type,
                              void* end_indices_out,
                              mhdf_Status* status );
void
mhdf_readSetParentEndIndicesWithOpt( hid_t data_handle,
                              long offset,
                              long count,
                              hid_t hdf_integer_type,
                              void* end_indices_out,
                              hid_t read_prop,
                              mhdf_Status* status );


/** \brief Write list of sets and meta-information about sets.
 *
 * Write set descriptions.  See \ref mhdf_createSetMeta or \ref mhdf_set for a 
 * description of the data format.
 *
 *\param data_handle The handle returned from \ref mhdf_createSetMeta or
 *                   \ref mhdf_openSetMeta.
 *\param offset      The offset (set index) to begin writing at.
 *\param count       The number of rows (sets, integer triples) to write.
 *\param hdf_integer_type The type of the integer data in <code>set_desc_data</code>.
 *                   Typically <code>H5T_NATIVE_INT</code> or
 *                   <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                   The HDF class of this type object <em>must</em> be H5T_INTEGER
 *\param set_desc_data The data to write.
 * \param status       Passed back status of API call.
 */
void
mhdf_writeSetMeta( hid_t data_handle,
                   long offset,
                   long count,
                   hid_t hdf_integer_type,
                   const void* set_desc_data,  
                   mhdf_Status* status );
void
mhdf_writeSetMetaWithOpt( hid_t data_handle,
                   long offset,
                   long count,
                   hid_t hdf_integer_type,
                   const void* set_desc_data,  
                   hid_t write_prop,
                   mhdf_Status* status );

/** \brief Create file object to hold list of meshset contents. 
 *
 * Create set contents data object.
 * The format of this data is a vector of integer values which is the
 * concatenation of the contents list for all the meshsets.  The length
 * and format of the data for each set is stored in the set meta table.
 * See \ref mhdf_createSetMeta and \ref mhdf_SET_RANGE_BIT for a 
 * description of that data.
 *
 *\param file_handle The file.
 *\param data_list_size The total length (number of integer values) to
 *                   be written for all the sets.
 *\param status      Passed back status of API call.
 *\return A handle to the table.
 */
hid_t
mhdf_createSetData( mhdf_FileHandle file_handle,
                    long data_list_size,
                    mhdf_Status* status );

/** \brief Open the file object for the meshset contents. 
 *
 * Open set contents data object.
 * The format of this data is a vector of integer values which is the
 * concatenation of the contents list for all the meshsets.  The length
 * and format of the data for each set is stored in the set meta table.
 * See \ref mhdf_createSetMeta and \ref mhdf_SET_RANGE_BIT for a 
 * description of that data.
 *
 *\param file_handle        The file.
 *\param data_list_size_out The length of the table.
 *\param status             Passed back status of API call.
 *\return                   A handle to the table.
 */
hid_t
mhdf_openSetData( mhdf_FileHandle file_handle,
                  long* data_list_size_out,
                  mhdf_Status* status );

/** \brief Write set contents.
 *
 * Write data specifying entities contained in sets.
 * The format of this data is a vector of integer values which is the
 * concatenation of the contents list for all the meshsets.  The length
 * and format of the data for each set is stored in the set meta table.
 * See \ref mhdf_createSetMeta and \ref mhdf_SET_RANGE_BIT for a 
 * description of that data.
 *
 *\param set_handle   The handle returned from \ref mhdf_createSetData
 *                    or \ref mhdf_openSetData .
 *\param offset       The position at which to write into the integer vector.
 *\param count        The number of values to write into the data vector.
 *\param hdf_integer_type The type of the integer data in <code>set_data</code>.
 *                   Typically <code>H5T_NATIVE_INT</code> or
 *                   <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                   The HDF class of this type object <em>must</em> be H5T_INTEGER
 *\param set_data    The data to write.
 *\param status      Passed back status of API call.
 */
void
mhdf_writeSetData( hid_t set_handle,
                   long offset,
                   long count,
                   hid_t hdf_integer_type,
                   const void* set_data,
                   mhdf_Status* status );
void
mhdf_writeSetDataWithOpt( hid_t set_handle,
                   long offset,
                   long count,
                   hid_t hdf_integer_type,
                   const void* set_data,
                   hid_t write_prop,
                   mhdf_Status* status );

/** \brief Read set contents.
 *
 * Read data specifying entities contained in sets.
 * The format of this data is a vector of integer values which is the
 * concatenation of the contents list for all the meshsets.  The length
 * and format of the data for each set is stored in the set meta table.
 * See \ref mhdf_createSetMeta and \ref mhdf_SET_RANGE_BIT for a 
 * description of that data.
 *
 *\param set_handle   The handle returned from \ref mhdf_createSetData
 *                    or \ref mhdf_openSetData .
 *\param offset       The position at which to read from the integer vector.
 *\param count        The number of values to read from the data vector.
 *\param hdf_integer_type The type of the integer data in <code>set_data</code>.
 *                   Typically <code>H5T_NATIVE_INT</code> or
 *                   <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                   The HDF class of this type object <em>must</em> be H5T_INTEGER
 *\param set_data    A pointer to memory at which to store the read data.
 *\param status      Passed back status of API call.
 */
void
mhdf_readSetData( hid_t set_handle,
                  long offset,
                  long count,
                  hid_t hdf_integer_type,
                  void* set_data,
                  mhdf_Status* status );
void
mhdf_readSetDataWithOpt( hid_t set_handle,
                  long offset,
                  long count,
                  hid_t hdf_integer_type,
                  void* set_data,
                  hid_t read_prop,
                  mhdf_Status* status );

/** \brief Create file object for storing the set child list 
 *
 * Create a data group for the list of set children.  
 * The format of this data is the concatenation of the lists of
 * global IDs of child sets for each set.  The order of the sets and
 * the number of children for each set is contained in the set meta table.
 * (See \ref mhdf_createSetMeta ).
 *
 *\param file_handle      The file
 *\param child_list_size  The total length of the data (the sum of the 
 *                        number of children for each set.)
 *\param status           Passed back status of API call.
 *\return A handle to the data object in the file.
 */
hid_t
mhdf_createSetChildren( mhdf_FileHandle file_handle,
                        long child_list_size,
                        mhdf_Status* status );

/** \brief Open the file object containing the set child list 
 *
 * Open the data group containing the list of set children.  
 * See \ref mhdf_createSetChildren and \ref mhdf_createSetMeta for 
 * a description of this data.
 *
 *\param file_handle      The file
 *\param child_list_size  The total length of the data (the sum of the 
 *                        number of children for each set.)
 *\param status           Passed back status of API call.
 *\return A handle to the data object in the file.
 */
hid_t
mhdf_openSetChildren( mhdf_FileHandle file_handle,
                      long* child_list_size,
                      mhdf_Status* status );

/** \brief Create file object for storing the set parent list 
 *
 * Create a data group for the list of set parents.  
 * The format of this data is the concatenation of the lists of
 * global IDs of parent sets for each set.  The order of the sets and
 * the number of parents for each set is contained in the set meta table.
 * (See \ref mhdf_createSetMeta ).
 *
 *\param file_handle       The file
 *\param parent_list_size  The total length of the data (the sum of the 
 *                         number of parents for each set.)
 *\param status            Passed back status of API call.
 *\return A handle to the data object in the file.
 */
hid_t
mhdf_createSetParents( mhdf_FileHandle file_handle,
                       long parent_list_size,
                       mhdf_Status* status );

/** \brief Open the file object containing the set parent list 
 *
 * Open the data group containing the list of set parents.  
 * See \ref mhdf_createSetParents and \ref mhdf_createSetMeta for 
 * a description of this data.
 *
 *\param file_handle       The file
 *\param parent_list_size  The total length of the data (the sum of the 
 *                         number of parents for each set.)
 *\param status            Passed back status of API call.
 *\return A handle to the data object in the file.
 */
hid_t
mhdf_openSetParents( mhdf_FileHandle file_handle,
                     long* parent_list_size,
                     mhdf_Status* status );

/** \brief Write set parent/child list
 *
 * Write the list of parent or child IDs for sets.
 * See \ref mhdf_createSetChildren and \ref mhdf_createSetMeta for 
 * a description of this data.
 * 
 *\param data_handle The value returned from \ref mhdf_createSetChildren
 *                   or \ref mhdf_openSetChildren.
 *\param offset      The offset into the list of global IDs.
 *\param count       The number of global IDs to write.
 *\param hdf_integer_type The type of the integer data in <code>child_id_list</code>.
 *                   Typically <code>H5T_NATIVE_INT</code> or
 *                   <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                   The HDF class of this type object <em>must</em> be H5T_INTEGER
 *\param id_list     The data to write.
 *\param status      Passed back status of API call.
 */
void
mhdf_writeSetParentsChildren( hid_t data_handle,
                              long offset,
                              long count,
                              hid_t hdf_integer_type,
                              const void* id_list,
                              mhdf_Status* status );
void
mhdf_writeSetParentsChildrenWithOpt( hid_t data_handle,
                              long offset,
                              long count,
                              hid_t hdf_integer_type,
                              const void* id_list,
                              hid_t write_prop,
                              mhdf_Status* status );

/** \brief Read set parent/child list
 *
 * Read from the list of parent or child IDs for sets.
 * See \ref mhdf_createSetChildren and \ref mhdf_createSetMeta for 
 * a description of this data.
 * 
 *\param data_handle The value returned from \ref mhdf_createSetChildren
 *                   or \ref mhdf_openSetChildren.
 *\param offset      The offset into the list of global IDs.
 *\param count       The number of global IDs to read.
 *\param hdf_integer_type The type of the integer data in <code>child_id_list</code>.
 *                   Typically <code>H5T_NATIVE_INT</code> or
 *                   <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                   The HDF class of this type object <em>must</em> be H5T_INTEGER
 *\param id_list     Pointer to memory in which to the read data.
 *\param status      Passed back status of API call.
 */
void
mhdf_readSetParentsChildren( hid_t data_handle,
                             long offset,
                             long count,
                             hid_t hdf_integer_type,
                             void* id_list,
                             mhdf_Status* status );
void
mhdf_readSetParentsChildrenWithOpt( hid_t data_handle,
                             long offset,
                             long count,
                             hid_t hdf_integer_type,
                             void* id_list,
                             hid_t read_prop,
                             mhdf_Status* status );

/*@}*/

/**
 *\defgroup mhdf_tag Tag data.
 *
 * The data for each tag can be stored in two places/formats:  sparse and/or
 * dense.  The data may be stored in both, but there should not be redundant 
 * values for the same entity.  
 *
 * Dense tag data is stored as multiple tables of tag values, one for each
 * element group.  (Note:  special \ref mhdf_ElemHandle values are available
 * for accessing dense tag data on nodes or meshsets via the \ref mhdf_node_type_handle
 * and \ref mhdf_set_type_handle functions.)  Each dense tag table should contain
 * the same number of entries as the element connectivity table.  The tag values 
 * are associated with the corresponding element in the connectivity table.
 *
 * Sparse tag data is stored as a global table pair for each tag type.  The first
 * if the pair of tables is a list of Global IDs.  The second is the corresponding
 * tag value for each entity in the ID list.
 */
/*@{*/


/**
 *\defgroup mhdf_tag_flag Tag type values   (MOAB-specific)
 */
/*@{*/

/** \brief Was dense tag data in mesh database */
#define mhdf_DENSE_TYPE   2 
/** \brief Was sparse tag data in mesh database */
#define mhdf_SPARSE_TYPE  1 
/** \brief Was bit-field tag data in mesh database */
#define mhdf_BIT_TYPE     0 
/** \brief Unused */
#define mhdf_MESH_TYPE    3 

/*@}*/

/** \brief Make type native-endian.
 *
 * Given an atomic HDF5 data type, return the built-in type
 * that matches the class of the passed type and is the specified
 * size. 
 *
 * This function is provided to allow converting the stored tag
 * type in a file to the preferred type for it's representation 
 * in memory when reading tag values.
 *
 * This function works only for atomic types.  The returned type
 * will be a pre-defined HDF5 object and does not need to be
 * closed/released.
 *
 *\param input_type   The type to convert.
 *\param size         The desired size in bytes.
 *\param status       Passed back status of API call.
 *\return             The converted type.
 */
hid_t
mhdf_getNativeType( hid_t input_type,
                    int size,
                    mhdf_Status* status );

/** \brief Add a tag to the file
 *
 * Add a new tag to the file.  This function must be called
 * to define the tag characteristics before values for the
 * tag can be written.
 *
 *\param file_handle    The file
 *\param tag_name       The tag name
 *\param tag_type       The tag type.
 *\param size           If tag_type == mhdf_BITFIELD, the number of
 *                      bits.  If tag_type == mhdf_OPAQUE, the size
 *                      of the opaque type in bytes.  Otherwise the
 *                      length of the array of tag_type entities associated
 *                      with each mesh entity, or 1 for a scalar value.
 *\param storage        MOAB storage type (dense, sparse, etc.)
 *\param default_value  Default value for tag, or NULL if none.
 *\param global_value   Global value for tag, or NULL if none.
 *\param hdf_type       If non-zero, assumed to be a user-specified type
 *                      for opaque data.  Ignored if tag_type is not
 *                      mhdf_OPAQUE.
 *\param hdf_base_type  Ignored if hdf_type is non-zero.  If hdf_type is
 *                      zero and this type is non-zero, it is used either
 *                      as the type or as the base type for an array type for 
 *                      default_value and global_value, respectively.  Typically
 *                      used to specify the input data type for mhdf_ENTITY_ID
 *                      tags.  
 */
void
mhdf_createTag( mhdf_FileHandle file_handle,
                const char* tag_name,
                enum mhdf_TagDataType tag_type,
                int size,
                int storage,
                const void* default_value,
                const void* global_value,
                hid_t hdf_type,
                hid_t mhdf_base_type,
                mhdf_Status* status );

/**\brief Get handle to HDF5 type object for tag data */
hid_t
mhdf_getTagDataType( mhdf_FileHandle file_handle, 
                     const char* tag_name, 
                     mhdf_Status* status );


/** \brief Add variable-length tag to file
 *
 * Add a new tag to the file.  This function must be called
 * to define the tag characteristics before values for the
 * tag can be written.  Use this function if the tag values
 * are not fixed-length.
 *
 *\param file_handle    The file
 *\param tag_name       The tag name
 *\param tag_type       The tag type.
 *\param storage        MOAB storage type (dense, sparse, etc.)
 *\param default_value  Default value for tag, or NULL if none.
 *\param default_value_length Length of default value.
 *\param global_value   Global value for tag, or NULL if none.
 *\param global_value_length Length of global value.
 *\param hdf_type       If non-zero, assumed to be a user-specified type
 *                      for opaque data.  Ignored if tag_type is not
 *                      mhdf_OPAQUE.
 *\param hdf_base_type  Ignored if hdf_type is non-zero.  If hdf_type is
 *                      zero and this type is non-zero, it is used either
 *                      as the type or as the base type for an array type for 
 *                      default_value and global_value, respectively.  Typically
 *                      used to specify the input data type for mhdf_ENTITY_ID
 *                      tags.  
 */
void
mhdf_createVarLenTag( mhdf_FileHandle file_handle,
                      const char* tag_name,
                      enum mhdf_TagDataType tag_type,
                      int storage,
                      const void* default_value,
                      int default_value_length,
                      const void* global_value,
                      int global_value_length,
                      hid_t hdf_type,
                      hid_t hdf_base_type,
                      mhdf_Status* status );
                

/** \brief Get the number of tags in the file.
 *
 *\param file_handle   The file.
 *\param status        Passed back status of API call.
 *\return The number of tags.
 */
int
mhdf_getNumberTags( mhdf_FileHandle file_handle,
                    mhdf_Status* status );

/** \brief Get the name for each tag defined in the file.
 *
 *\param file_handle   The file.
 *\param num_names_out The length of the returned array of strings.
 *\param status        Passed back status of API call.
 *\return An array of null-terminated strings.  The array
 *        and each string is allocated with <code>malloc</code>.
 *        The caller should release this memory by calling 
 *        <code>free</code> for each string and the array.
 */                 
char**
mhdf_getTagNames( mhdf_FileHandle file_handle,
                  int* num_names_out,
                  mhdf_Status* status );

/** \brief Get the description of a specified tag.
 *
 * Get everything about a tag except the actual values.
 *
 *\param file_handle      The file.
 *\param tag_name         The name of the tag to retrieve the data for.
 *\param class_out        The TSTT class of the tag data.
 *\param size_out         Depends on value of class_out:
 *                        - mhdf_OPAQUE  - size of opaque data in bytes
 *                        - mhdf_BITFIELD - number of bits
 *                        - if data is fixed-length array, length of array
 *                        - if data is single value, 1
 *                        - if data is a variable-length array, -1
 *\param tstt_storage_out The value of the TSTT enum for storage (dense, sparse, etc.)
 *\param have_default_out Non-zero if file contains a default value for the tag. 
 *                        Length of default value if variable-lenth tag.
 *\param have_global_out  Non-zero if the file contains a global/mesh value for the tag.
 *\param have_sparse_out  Non-zero if the file contains a sparse data table for this tag.
 */
void
mhdf_getTagInfo( mhdf_FileHandle file_handle,
                 const char* tag_name,
                 enum mhdf_TagDataType* class_out,
                 int* size_out,
                 int* tstt_storage_out,
                 int* have_default_out,
                 int* have_global_out,
                 int* have_sparse_out,
                 mhdf_Status* status );
                 


/** \brief Get the default and global values of the tag.
 *
 *\param file_handle      The file.
 *\param tag_name         The tag name.
 *\param output_data_type The HDF5 type for the memory into which the
 *                        tag data is to be written.  If zero, then 
 *                        the value(s) will be read as opaque data.
 *\param default_value    Memory location at which to write the default
 *                        value of the tag.
 *\param global_value     If the tag has a global value, the memory location
 *                        at which to write that value.
 *\param status           Passed back status of API call.
 */
void
mhdf_getTagValues( mhdf_FileHandle file_handle,
                   const char* tag_name,
                   hid_t output_data_type,
                   void* default_value,
                   void* global_value,
                   mhdf_Status* status );

/** \brief Check if the file contains dense tag data for the specified tag and element group.
 *
 * Check if there is dense tag data for a given element type for the specified
 * tag.  
 *
 *\param file_handle  The file.
 *\param tag_name     The tag.
 *\param elem_group   The element group handle, or the return value of
 *                    \ref mhdf_node_type_handle or \ref mhdf_set_type_handle
 *                    for nodes or sets respectively.
 *\param status       Passed back status of API call.
 *\return Non-zero if file contains specified data.  Zero otherwise.
 */
int
mhdf_haveDenseTag( mhdf_FileHandle file_handle,
                   const char* tag_name,
                   const char* elem_group,
                   mhdf_Status* status );

/** \brief Create an object to hold dense tag values for a given element group.
 *
 *\param file_handle  The file.
 *\param tag_name     The tag.
 *\param elem_group   The element group handle, or the return value of
 *                    \ref mhdf_node_type_handle or \ref mhdf_set_type_handle
 *                    for nodes or sets respectively.
 *\param num_values   The number of tag values to be written.  Must be
 *                    The same as the number of elements in the group. 
 *                    Specified here to allow tag values to be written
 *                    before node coordinates, element connectivity or meshsets.
 *\param status       Passed back status of API call.
 *\return             Handle to data object in file.
 */
hid_t
mhdf_createDenseTagData( mhdf_FileHandle file_handle,
                         const char* tag_name,
                         const char* elem_group,
                         long num_values,
                         mhdf_Status* status );

/** \brief Open the object containing dense tag values for a given element group.
 *
 *\param file_handle    The file.
 *\param tag_name       The tag.
 *\param elem_group     The element group handle, or the return value of
 *                      \ref mhdf_node_type_handle or \ref mhdf_set_type_handle
 *                      for nodes or sets respectively.
 *\param num_values_out The number of tag values to be written.  Must be
 *                      The same as the number of elements in the group. 
 *\param status         Passed back status of API call.
 *\return               Handle to data object in file.
 */
hid_t
mhdf_openDenseTagData( mhdf_FileHandle file_handle,
                       const char* tag_name,
                       const char* elem_group,
                       long* num_values_out,
                       mhdf_Status* status );

/** \brief Create file objects to store sparse tag data 
 *
 * Create the file objects to store all sparse data for a given tag in.  The 
 * sparse data is stored in a pair of objects.  The first is a vector of
 * global IDs.  The second is a vector of tag values for each entity specified
 * in the list of global IDs.
 *
 *\param file_handle    The file.
 *\param tag_name       The tag.
 *\param num_values     The number of tag values to be written.
 *\param entities_and_values_out The handles to the file objects.
 *                      The first is the vector of global IDs.  The second
 *                      is the list of corresponding tag values.  
 *\param status         Passed back status of API call.
 */
void
mhdf_createSparseTagData( mhdf_FileHandle file_handle,
                          const char* tag_name,
                          long num_values,
                          hid_t entities_and_values_out[2],
                          mhdf_Status* status );

/** \brief Create file objects to store (sparse) variable-length tag data 
 *
 * Create the file objects to store all sparse data for a given tag in.  The 
 * sparse data is stored in a pair of objects.  The first is a vector of
 * global IDs.  The second is a vector of tag values for each entity specified
 * in the list of global IDs.
 *
 *\param file_handle    The file.
 *\param tag_name       The tag.
 *\param num_entities   The number of entities for which tag values are to be stored
 *\param num_values     The total number of scalar values to be written (the
 *                      total number of bytes of data for all tags for opaque
 *                      data.)
 *\param entities_and_values_out The handles to the file objects.
 *                      The first is the vector of global IDs.  The second
 *                      is the list of corresponding tag values.  The third
 *                      is the handle to the index table.
 *\param status         Passed back status of API call.
 */
void
mhdf_createVarLenTagData( mhdf_FileHandle file_handle,
                          const char* tag_name,
                          long num_entities,
                          long num_values,
                          hid_t entities_and_values_out[3],
                          mhdf_Status* status );

/** \brief Create file objects to read sparse tag data 
 *
 * Open the file objects containing all sparse data for a given tag in.  The 
 * sparse data is stored in a pair of objects.  The first is a vector of
 * global IDs.  The second is a vector of tag values for each entity specified
 * in the list of global IDs.  For variable-length tags, a third table
 * containing end offsets for each tag value is returned in the third
 * position of the output hid_t array (see mhdf_readSparseTagIndices.)
 *
 *\param file_handle    The file.
 *\param tag_name       The tag.
 *\param num_entity_out The number of entities for which there are tag values.
 *\param num_values_out The number of data values.  For fixed-length tags,
 *                      this is the same as num_entity_out.  For
 *                      variable-length tags, it is the total number of
 *                      values in the data table.
 *\param entities_and_values_out The handles to the pair of file objects.
 *                      The first is the vector of global IDs.  The second
 *                      is the list of corresponding tag values.  The third
 *                      is the handle to the index table, iff the tag is 
 *                      variable-length.  If the tag is fixed-length, this
 *                      value is not set.
 *\param status         Passed back status of API call.
 */
void
mhdf_openSparseTagData( mhdf_FileHandle file_handle,
                        const char* tag_name,
                        long* num_entity_out,
                        long* num_values_out,
                        hid_t entities_and_values_out[3],
                        mhdf_Status* status );

/** \brief Write Global ID list for sparse tag data
 *
 *\param id_handle   The first handle passed back from either
 *                   \ref mhdf_createSparseTagData or 
 *                   \ref mhdf_openSparseTagData.
 *\param offset      The offset at which to begin writing.
 *\param count       The number of global IDs to write.
 *\param hdf_integer_type The type of the integer data in <code>id_list</code>.
 *                   Typically <code>H5T_NATIVE_INT</code> or
 *                   <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                   The HDF class of this type object <em>must</em> be H5T_INTEGER
 *\param id_list     The list of global IDs to write.
 *\param status      Passed back status of API call.
 */
void
mhdf_writeSparseTagEntities( hid_t id_handle,
                             long offset,
                             long count,
                             hid_t hdf_integer_type,
                             const void* id_list,
                             mhdf_Status* status );
void
mhdf_writeSparseTagEntitiesWithOpt( hid_t id_handle,
                             long offset,
                             long count,
                             hid_t hdf_integer_type,
                             const void* id_list,
                             hid_t write_prop,
                             mhdf_Status* status );



/** \brief Write tag values
 *
 *\param value_handle  The second handle passed back from either
 *                     \ref mhdf_createSparseTagData or 
 *                     \ref mhdf_openSparseTagData; or the handle
 *                     returned by \ref mhdf_createDenseTagData or
 *                     \ref mhdf_openDenseTagData.
 *\param offset        The offset at which to begin writing.
 *\param count         The number of tag values to write.
 *\param hdf_tag_data_type The type of the data in memory.  
 *                     It must be possible for the HDF library to convert
 *                     between this type and the type the tag data is stored
 *                     as.  
 *\param tag_data      The list of tag values to write.
 *\param status        Passed back status of API call.
 */
void
mhdf_writeTagValues( hid_t value_handle,
                     long offset,
                     long count,
                     hid_t hdf_tag_data_type,
                     const void* tag_data,
                     mhdf_Status* status );
void
mhdf_writeTagValuesWithOpt( hid_t value_handle,
                     long offset,
                     long count,
                     hid_t hdf_tag_data_type,
                     const void* tag_data,
                     hid_t write_prop,
                     mhdf_Status* status );

/**\brief Write sparse tag end indices for variable-length tag data
 *
 * Write sparse tag end indices for variable-length tag data.
 * Each value in the list is the *last* index (zero-based) into the tag
 * data for the corresponding entity.
 *
 *\param tag_handle   handle to the data object to write to.
 *\param offset       The offset into the table at which to begin writting
 *\param count        The number of values to write.
 *\param hdf_integer_type  The type of the values pointed to by end_indices 
 *                    (must be an integer type).
 *\param end_indices  The values to store in the table.
 *\param status       Output: API result.
 */
void 
mhdf_writeSparseTagIndices( hid_t tag_handle,
                            long offset,
                            long count,
                            hid_t hdf_integer_type,
                            const void* end_indices,
                            mhdf_Status* status );
void 
mhdf_writeSparseTagIndicesWithOpt( hid_t tag_handle,
                            long offset,
                            long count,
                            hid_t hdf_integer_type,
                            const void* end_indices,
                            hid_t write_prop,
                            mhdf_Status* status );

/** \brief Read Global ID list for sparse tag data
 *
 *\param id_handle  The first handle passed back from either
 *                  \ref mhdf_createSparseTagData or 
 *                  \ref mhdf_openSparseTagData.
 *\param offset     The offset at which to begin reading.
 *\param count      The number of global IDs to read.
 *\param hdf_integer_type The type of the integer data in <code>id_list</code>.
 *                   Typically <code>H5T_NATIVE_INT</code> or
 *                   <code>N5T_NATIVE_LONG</code> as defined in <i>H5Tpublic.h</i>.
 *                   The HDF class of this type object <em>must</em> be H5T_INTEGER
 *\param id_list     The memory location at which to store the global IDs.
 *\param status      Passed back status of API call.
 */
void
mhdf_readSparseTagEntities( hid_t id_handle,
                            long offset,
                            long count,
                            hid_t hdf_integer_type,
                            void* id_list,
                            mhdf_Status* status );
void
mhdf_readSparseTagEntitiesWithOpt( hid_t id_handle,
                            long offset,
                            long count,
                            hid_t hdf_integer_type,
                            void* id_list,
                            hid_t read_prop,
                            mhdf_Status* status );

/** \brief Read tag values
 *
 *\param value_handle  The second handle passed back from either
 *                     \ref mhdf_createSparseTagData or 
 *                     \ref mhdf_openSparseTagData; or the handle
 *                     returned by \ref mhdf_createDenseTagData or
 *                     \ref mhdf_openDenseTagData.
 *\param offset     The offset at which to begin reading.
 *\param count      The number of tag values to read.
 *\param hdf_type   The type of the data in memory.  If this is specified,
 *                  it must be possible for the HDF library to convert
 *                  between this type and the type the tag data is stored
 *                  as.  If zero, the values will be read as opaque data.
 *\param memory     Memory location at which to store tag values.
 *\param status     Passed back status of API call.
 */
void
mhdf_readTagValues( hid_t value_handle,
                    long offset,
                    long count,
                    hid_t hdf_type,
                    void* memory,
                    mhdf_Status* status );
void
mhdf_readTagValuesWithOpt( hid_t value_handle,
                    long offset,
                    long count,
                    hid_t hdf_type,
                    void* memory,
                    hid_t read_prop,
                    mhdf_Status* status );


/**\brief Read sparse tag end indices for variable-length tag data
 *
 * Read sparse tag end indices for variable-length tag data.
 * Each value in the list is the *last* index (zero-based) into the tag
 * data for the corresponding entity.
 *
 *\param tag_handle   handle to the data object to read from.
 *\param offset       The offset into the table at which to begin reading
 *\param count        The number of values to read.
 *\param hdf_integer_type  The type of the values pointed to by end_indices 
 *                    (must be an integer type).
 *\param end_indices  Memory in which to store the data read from the table.
 *\param status       Output: API result.
 */
void 
mhdf_readSparseTagIndices( hid_t tag_handle,
                           long offset,
                           long count,
                           hid_t hdf_integer_type,
                           void* end_indices,
                           mhdf_Status* status );
void 
mhdf_readSparseTagIndicesWithOpt( hid_t tag_handle,
                           long offset,
                           long count,
                           hid_t hdf_integer_type,
                           void* end_indices,
                           hid_t read_prop,
                           mhdf_Status* status );

/*@}*/


/*@}*/


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
