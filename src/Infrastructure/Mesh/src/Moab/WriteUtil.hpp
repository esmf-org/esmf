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

#ifndef MB_WRITE_UTIL_HPP
#define MB_WRITE_UTIL_HPP

#ifndef IS_BUILDING_MB
  #error "WriteUtil.hpp isn't supposed to be included into an application"
#endif

#include "moab/WriteUtilIface.hpp"

namespace moab {

class Core;

class WriteUtil : public WriteUtilIface
{
private:
  //! Pointer to the Core
  Core* mMB;
public:

  //! Constructor takes Core pointer
  WriteUtil(Core* mdb);

  //! Destructor
  ~WriteUtil(){}

  //! Check if the specified file already exists.
  //! Returns MB_SUCCESS if file does not exist, MB_ALREADY_ALLOCATED
  //! if file does exist, or MB_FAILURE for some other error condition.
  virtual ErrorCode check_doesnt_exist(const char* file_name);

  //! Gather all entities in the mesh, or in the sets specified
  virtual ErrorCode gather_entities(Range &all_ents, /**< range in which entities are returned */
                                    const EntityHandle *ent_sets = NULL, /**< entity sets whose contents are to be gathered */
                                    const int num_sets = 0 /**< number of sets in list */);

  //! Gets arrays for coordinate data from the MB
  ErrorCode get_node_coords(const int num_arrays,
                            const int num_nodes,
                            const Range& entities,
                            Tag node_id_tag,
                            const int start_node_id,
                            std::vector<double*>& arrays);

  /** Get an array of coordinate values for nodes
   *
   * Given a range of node handles, retrieve a single or multiple coordinate
   * value(s) for each. 
   *
   * Failure conditions:
   *  - invalid entity handles (not vertices, non-existent entity, etc.)
   *  - range is empty (<code>iter == end</code>)
   *  - <code>output_array</code> is null
   *  - insufficient space in <code>output_array</code>
   *
   *\param which_array  The coordinate to retrieve (0-&gt;X, 1-&gt;Y, 2-&gt;Z, -1-&gt;all)
   *\param begin        The first node handle.
   *\param end          One past the last node handle.
   *\param output_size  The size of <code>output_array</code>.
   *\param output_array The memory in which to write the node coordinates.
   *\author Jason Kraftcheck
   */
  ErrorCode get_node_coords(const int which_array,
                            Range::const_iterator begin,
                            const Range::const_iterator& end,
                            const size_t output_size,
                            double* const output_array);

  /** Get connectivity for elements 
   *
   * Get the connectivity list for a range of elements.
   *
   *\param num_elements Number of elements for which connectivity is needed
   *\param vertices_per_elem Number of vertices to retrieve for each
   *                    element.
   *\param node_id_tag  A tag with integer values.
   *\param entities Entities being queried
   *\param element_id_tag If non-zero, elements are tagged with an id starting at start_element_id
   *\param start_element_id Starting id value for element_id_tag
   *\param add_sizes If true, writes size of connect array before connectivity in array
   */
  ErrorCode get_element_connect(const int num_elements,
                                const int verts_per_element,
                                Tag node_id_tag,
                                const Range& entities,
                                Tag element_id_tag,
                                int start_element_id,
                                int* array,
                                bool add_sizes = false);

  /** Get connectivity for elements
   *
   * Get the connectivity list for a range of elements.
   *
   * Failure cases:
   *  - Passed range is empty (<code>begin == end</code>).
   *  - <code>vertices_per_elem</code> is less than one
   *  - <code>element_array</code> is null.
   *  - The range contains invalid handles (non-existent entities,
   *      not an element, etc.)
   *  - Retrieving ID tag for an entity failed.
   *  - Insufficient space in passed array.
   *
   *\param begin        The first element handle
   *\param end          One past the last element handle
   *\param vertices_per_elem Number of vertices to retrieve for each
   *                    element. If the element has more vertices, the
   *                    element connectivity will be truncated. If
   *                    <code>vertices_per_elem</code> is greater than the
   *                    number of nodes for an element, the data will be
   *                    padded with zeros.
   *\param node_id_tag  A tag with integer values.
   *\param array_size   The length of <code>element_array</code>
   *\param element_array The memory location at which to store the
   *                    connectivity list.
   *\param add_sizes If true, writes size of connect array before connectivity in array
   *\author Jason Kraftcheck
   */
  ErrorCode get_element_connect(Range::const_iterator begin,
                                const Range::const_iterator& end,
                                const int vertices_per_elem,
                                Tag node_id_tag,
                                const size_t array_size,
                                int *const element_array,
                                bool add_sizes = false);

  /** Get connectivity for elements
   *
   * Get the connectivity list for a range of elements.
   *
   * Failure cases:
   *  - Passed range is empty (<code>begin == end</code>).
   *  - <code>vertices_per_elem</code> is less than one
   *  - <code>element_array</code> is null.
   *  - The range contains invalid handles (non-existent entities,
   *      not an element, etc.)
   *  - Insufficient space in passed array.
   *
   *\param begin        The first element handle
   *\param end          One past the last element handle
   *\param vertices_per_elem Number of vertices to retrieve for each
   *                    element. If the element has more vertices, the
   *                    element connectivity will be truncated. If
   *                    <code>vertices_per_elem</code> is greater than the
   *                    number of nodes for an element, the data will be
   *                    padded with zeros.
   *\param array_size   The length of <code>element_array</code>
   *\param element_array The memory location at which to store the
   *                    connectivity list.
   *\author Jason Kraftcheck
   */
  virtual ErrorCode get_element_connect(Range::const_iterator begin,
                                        const Range::const_iterator& end,
                                        const int vertices_per_elem,
                                        const size_t array_size,
                                        EntityHandle *const element_array);

  /** Get poly (polygon or polyhedron) connectivity size
   *\param begin  First iterator in range of poly
   *\param end    One past last in range of poly.
   *\param connectivity_size  The length of the connectivity list
   *              For the specified range of polyhedra.
   *\author Jason Kraftcheck
   */
  virtual ErrorCode get_poly_connect_size(Range::const_iterator begin,
                                          const Range::const_iterator& end,
                                          int& connectivity_size);

  /** Get poly (polygon or polyhedron) connectivity.
   *
   * This function will add as many polys as possible to the
   * passed arrays given the sizes of those arrays. It will
   * then pass back position at which it stopped and the sizes
   * of the data written to the arrays.
   *
   *\param iter               As input, the first element handle.
   *                          As output, one past the last element handle
   *                          for which data was written to the arrays.
   *\param end                The iterator at which to stop.
   *\param node_id_tag        A tag with integer values.
   *\param element_array_len  As input, length of <code>element_array</code>.
   *                          As output, the number of entries written in that
   *                          array.
   *\param element_array      The memory location at which to store the
   *                          connectivity list.
   *\param index_array_len    As input, the length of <code>index_array</code>.
   *                          As output, the number of entries written in that
   *                          array.
   *\param index_array        The memory location at which to store offsets.
   *\param index_offset       Value to offset (add to) index values. As output
   *                          the input value plus the amount of data
   *                          written to the element array. (The value you
   *                          presumably want to pass to the next call.)
   *\author Jason Kraftcheck
   */
  virtual ErrorCode get_poly_connect(Range::const_iterator& iter,
                                     const Range::const_iterator& end,
                                     const Tag node_id_tag,
                                     size_t& handle_array_len,
                                     int *const handle_array,
                                     size_t& index_array_len,
                                     int *const index_array,
                                     int& index_offset);

  //! Get a set of nodes that represent a set of elements
  ErrorCode gather_nodes_from_elements(const Range& elements,
                                       const Tag node_bit_mark_tag,
                                       Range& nodes);

  //! Assign ids to input elements starting with start_id, written to id_tag
  //! if zero, assigns to GLOBAL_ID_TAG_NAME
  ErrorCode assign_ids(Range &elements,
                       Tag id_tag,
                       const int start_id);

  /** Get explicit adjacencies
   *
   * Get explicit adjacences stored in database.
   * Does not create any explicit adjacencies or search for
   * implicit ones.
   *
   *\param entity  The entity to retrieve adjacencies for.
   *\param id_tag  The global ID tag
   *\param adj     The output list of global IDs of adjacent entities.
   */
  ErrorCode get_adjacencies(EntityHandle entity,
                            Tag id_tag,
                            std::vector<int>& adj
  );

  ErrorCode get_adjacencies(EntityHandle entity,
                            const EntityHandle*& adj_array,
                            int& num_adj);

  /**\brief Get list of tags to write.
   *
   * Get the list of tags to write to the file, possibly using
   * an optional user-specified tag list. This function consolidates
   * some common code for file writers to use to figure out what
   * tag data to write to the file. It provides the following features:
   *  o filter list based on user-specified array of tag handles
   *  o filter internal tags (those for which the name is prefixed with
   *    two underscore characters)
   *  o filter anonymous tags
   *  o optionally filter variable-length tags.
   *
   *\author Jason Kraftcheck
   *\param result_list List of tag handles for which to write data
   *\param user_tag_list Optional array of tag handles passed by user
   *                     to write to file.
   *\param include_variable_length_tags If false, return only fixed-length
   *                                    tags.
   */
  virtual ErrorCode 
  get_tag_list(std::vector<Tag>& result_list,
               const Tag* user_tag_list = 0,
               int user_tag_list_length = 0,
               bool include_variable_length_tags = true);

  /*\brief Get pointers to internal storage of entity data
   *
   * Get pointers to element connectivity or set content storage.
   *\param query_begin Start of range of entities for which to return results
   *\param query_end   End of range of entities for which to return results.
   *\param output_pointer_array Result list of pointers. Points to either
   *          element connectivity or set contents. Note: set contents
   *          may be in range-compacted format.
   *\param lengths Optional per-entity length of list. If passed, then
   *          always set, for each entity, to the number of values in the
   *          array passed back in \c output_pointer_array
   *\param relation If entity is entity set, which set data to return
   *          (contents array, parent array, or child array). If
   *          entity is an element, then CONTENTS for complete connectivity
   *          or TOPOLOGICAL for only corner vertices.
   *\param flags Optional per-entity flag values. If passed, then
   *          always set to zero for elements and set to set creation
   *          flags for entity sets.
   *\return MB_STRUCTURED_MESH if one or more input elements are stored as
   *          structured mesh and therefore do not have explicit storage.
   *        MB_TYPE_OUT_OF_RANGE if called for vertices.
   */
  virtual ErrorCode
  get_entity_list_pointers(Range::const_iterator query_begin,
                           Range::const_iterator query_end,
                           EntityHandle const* * output_pointer_array,
                           EntityListType relation = CONTENTS,
                           int* lengths = 0,
                           unsigned char* flags = 0);

  /*\brief Get pointers to internal storage of entity data
   *
   * Get pointers to element connectivity or set content storage.
   *\param entities Pointer to list of entities for which to return results
   *\param num_entities Number of entities in list
   *\param output_pointer_array Result list of pointers. Points to either
   *          element connectivity or set contents. Note: set contents
   *          may be in range-compacted format.
   *\param lengths Optional per-entity length of list. If passed, then
   *          always set, for each entity, to the number of values in the
   *          array passed back in \c output_pointer_array
   *\param relation If entity is entity set, which set data to return
   *          (contents array, parent array, or child array). If
   *          entity is an element, then CONTENTS for complete connectivity
   *          or TOPOLOGICAL for only corner vertices.
   *\param flags Optional per-entity flag values. If passed, then
   *          always set to zero for elements and set to set creation
   *          flags for entity sets.
   *\return MB_STRUCTURED_MESH if one or more input elements are stored as
   *          structured mesh and therefore do not have explicit storage.
   *        MB_TYPE_OUT_OF_RANGE if called for vertices.
   */
  virtual ErrorCode
  get_entity_list_pointers(EntityHandle const* entities,
                           int num_entities,
                           EntityHandle const* * output_pointer_array,
                           EntityListType relation = CONTENTS,
                           int* lengths = 0,
                           unsigned char* flags = 0);
};

} // namespace moab

#endif
