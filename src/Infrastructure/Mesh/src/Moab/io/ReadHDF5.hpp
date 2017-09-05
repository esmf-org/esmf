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

#ifndef READ_HDF5_HPP
#define READ_HDF5_HPP

#include <stdlib.h>
#include <list>
#include "mhdf.h"
#include "moab/Forward.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/Range.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/RangeMap.hpp"
#include "DebugOutput.hpp"
#include "HDF5Common.hpp"

#ifdef MOAB_HAVE_MPI
# include <moab_mpi.h>
#endif

namespace moab {

class ParallelComm;
class ReadHDF5Dataset;
class CpuTimer;

/**
 * \brief  Read mesh from MOAB HDF5 (.h5m) file.
 * \author Jason Kraftcheck
 * \date   18 April 2004
 */
class ReadHDF5 : public ReaderIface
{
public:

#ifdef MOAB_HAVE_MPI
  typedef MPI_Comm Comm;
#else
  typedef int Comm;
#endif

  static ReaderIface* factory(Interface*);

  ReadHDF5(Interface* iface);

  virtual ~ReadHDF5();

  /** Export specified meshsets to file
   * \param filename     The filename to export. Must end in <em>.mhdf</em>
   * \param export_sets  Array of handles to sets to export, or NULL to export all.
   * \param export_set_count Length of <code>export_sets</code> array.
   */
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
  Interface* moab() const
    { return iFace; }

  //! Store old HDF5 error handling function
  struct HDF5ErrorHandler {
    HDF5_Error_Func_Type func;
    void* data;
  };

protected:

  ErrorCode load_file_impl(const FileOptions& opts);

  ErrorCode load_file_partial(const ReaderIface::IDTag* subset_list,
                              int subset_list_length,
                              int num_parts,
                              int part_number,
                              const FileOptions& opts);

  ErrorCode read_tag_values_all(int tag_index, std::vector<int>& results);
  ErrorCode read_tag_values_partial(int tag_index, const Range& file_ids,
                                    std::vector<int>& results);

  enum ReadTimingValues {
    TOTAL_TIME = 0,
    SET_META_TIME,
    SUBSET_IDS_TIME,
    GET_PARTITION_TIME,
    GET_SET_IDS_TIME,
    GET_SET_CONTENTS_TIME,
    GET_POLYHEDRA_TIME,
    GET_ELEMENTS_TIME,
    GET_NODES_TIME,
    GET_NODEADJ_TIME,
    GET_SIDEELEM_TIME,
    UPDATECONN_TIME,
    ADJACENCY_TIME,
    DELETE_NON_SIDEELEM_TIME,
    READ_SET_IDS_RECURS_TIME,
    FIND_SETS_CONTAINING_TIME,
    READ_SETS_TIME,
    READ_TAGS_TIME,
    STORE_FILE_IDS_TIME,
    READ_QA_TIME,
    NUM_TIMES
  };


  void print_times( );

private:
  ErrorCode init();

  inline int is_error(mhdf_Status& status) {
    int i;
    if ((i = mhdf_isError(&status)))
      MB_SET_ERR_CONT(mhdf_message(&status));
    return i;
  }

  //! The size of the data buffer (<code>dataBuffer</code>).
  int bufferSize;
  //! A memory buffer to use for all I/O operations.
  char* dataBuffer;

  //! Interface pointer passed to constructor
  Interface* iFace;

  //! The file handle from the mhdf library
  mhdf_FileHandle filePtr;

  //! File summary
  mhdf_FileDesc* fileInfo;

  //! Map from File ID to MOAB handle
  typedef RangeMap<long, EntityHandle> IDMap;
  IDMap idMap;

  //! Cache pointer to read util
  ReadUtilIface* readUtil;

  //! The type of an EntityHandle
  hid_t handleType;

  //! List of connectivity arrays for which conversion from file ID
  //! to handle was deferred until later.
  struct IDConnectivity {
    EntityHandle handle; // Start_handle
    size_t count; // Num entities
    int nodes_per_elem; // Per-element connectivity length
    EntityHandle* array; // Connectivity array
  };
  //! List of connectivity arrays for which conversion from file ID
  //! to handle was deferred until later.
  std::vector<IDConnectivity> idConnectivityList;  

  //! Read/write property handle
  //! indepIO -> independent IO during true parallel read
  //! collIO  -> collective IO during true parallel read
  //! Both are H5P_DEFAULT for serial IO and collective
  //! when reading the entire file on all processors.
  hid_t indepIO, collIO;

  ParallelComm* myPcomm;

  //! Use IODebugTrack instances to verify reads.
  //! Enable with the DEBUG_OVERLAPS option.
  bool debugTrack;
  //! Debug output. Verbosity controlled with DEBUG_FORMAT option.
  DebugOutput dbgOut;
  //! Doing true parallel read (PARALLEL=READ_PART)
  bool nativeParallel;
  //! MPI_Comm value (unused if \c !nativeParallel)
  Comm* mpiComm;

  //! Flags for some behavior that can be changed through
  //! reader options
  bool blockedCoordinateIO;
  bool bcastSummary;
  bool bcastDuplicateReads;

  //! Store old HDF5 error handling function
  HDF5ErrorHandler errorHandler;

  long (*setMeta)[4];

  double _times[NUM_TIMES];
  CpuTimer * timer;
  bool cputime;
  ErrorCode read_all_set_meta();

  ErrorCode set_up_read(const char* file_name, const FileOptions& opts);
  ErrorCode clean_up_read(const FileOptions& opts);

  //! Given a list of tags and values, get the file ids for the
  //! corresponding entities in the file.
  ErrorCode get_subset_ids(const ReaderIface::IDTag* subset_list,
                           int subset_list_length,
                           Range& file_ids_out);

  /**\brief Remove all but the specified fraction of sets from the passed range
   *
   * Select a subset of the gathered set of file ids to read in based on
   * communicator size and rank.
   *\param tmp_file_ids As input: sets to be read on all procs.
   *                    As output: sets to read on this proc.
   *\param num_parts    communicator size
   *\param part_number  communicator rank
   */
  ErrorCode get_partition(Range& tmp_file_ids, int num_parts, int part_number);

  ErrorCode read_nodes(const Range& node_file_ids);

  // Read elements in fileInfo->elems[index]
  ErrorCode read_elems(int index);

  // Read subset of elements in fileInfo->elems[index]
  ErrorCode read_elems(int index, const Range& file_ids, Range* node_ids = 0);

  //! Read element connectivity.
  //!
  //!\param node_ids  If this is non-null, the union of the connectivity list
  //!                 for all elements is passed back as FILE IDS in this
  //!                 range AND the connectivity list is left in the form
  //!                 of file IDs (NOT NODE HANDLES).
  ErrorCode read_elems(const mhdf_ElemDesc& elems, const Range& file_ids, Range* node_ids = 0);

  //! Update connectivity data for all element groups for which read_elems
  //! was called with a non-null \c node_ids argument.
  ErrorCode update_connectivity();

  // Read connectivity data for a list of element file ids.
  // passing back the file IDs for the element connectivity
  // w/out actually creating any elements in MOAB.
  ErrorCode read_elems(int index, const Range& element_file_ids, Range& node_file_ids);

  // Scan all elements in group. For each element for which all vertices
  // are contained in idMap (class member), read the element. All new
  // elements are added to idMap.
  //
  // NOTE: Collective IO calls in parallel.
  ErrorCode read_node_adj_elems(const mhdf_ElemDesc& group,
                                Range* read_entities = 0);

  // Scan all elements in specified file table. For each element for
  // which all vertices are contained in idMap (class member), read the
  // element. All new elements are added to idMap.
  //
  // NOTE: Collective IO calls in parallel.
  ErrorCode read_node_adj_elems(const mhdf_ElemDesc& group,
                                hid_t connectivity_handle,
                                Range* read_entities = 0);

  //! Read poly(gons|hedra)
  ErrorCode read_poly(const mhdf_ElemDesc& elems, const Range& file_ids);
  
  //! Clean up elements that were a) read because we had read all of the
  //! nodes and b) weren't actually sides of the top-dimension elements
  //! we were trying to read.
  ErrorCode delete_non_side_elements(const Range& side_ents);

  //! Read sets
  ErrorCode read_sets(const Range& set_file_ids);

  ErrorCode read_adjacencies(hid_t adjacency_table,
                             long table_length);

  //! Create tag and read all data.
  ErrorCode read_tag(int index);

  //! Create new tag or verify type matches existing tag
  ErrorCode create_tag(const mhdf_TagDesc& info, Tag& handle, hid_t& type);

  //! Read dense tag for all entities 
  ErrorCode read_dense_tag(Tag tag_handle,
                           const char* ent_name,
                           hid_t hdf_read_type,
                           hid_t data_table,
                           long start_id,
                           long count);

  //! Read sparse tag for all entities.
  ErrorCode read_sparse_tag(Tag tag_handle,
                            hid_t hdf_read_type,
                            hid_t ent_table,
                            hid_t val_table,
                            long num_entities);

  //! Read variable-length tag for all entities.
  ErrorCode read_var_len_tag(Tag tag_handle,
                             hid_t hdf_read_type,
                             hid_t ent_table,
                             hid_t val_table,
                             hid_t off_table,
                             long num_entities,
                             long num_values);

  /**\brief Read index table for sparse tag.
   *
   * Read ID table for a sparse or variable-length tag, returning
   * the handles and offsets within the table for each file ID
   * that corresponds to an entity we've read from the file (an
   * entity that is in \c idMap).
   *
   * \param id_table     The MOAB handle for the tag
   * \param start_offset Some non-zero value because ranges (in this case
   *                     the offset_range) cannot contain zeros.
   * \param offset_range Output: The offsets in the id table for which IDs
   *                     that occur in \c idMap were found. All values
   *                     are increased by \c start_offset to avoid
   *                     putting zeros in the range.
   * \param handle_range Output: For each valid ID read from the table,
   *                     the corresponding entity handle. Note: if
   *                     the IDs did not occur in handle order, then
   *                     this will be empty. Use \c handle_vect instead.
   * \param handle_vect  Output: For each valid ID read from the table,
   *                     the corresponding entity handle. Note: if
   *                     the IDs occurred in handle order, then
   *                     this will be empty. Use \c handle_range instead.
   */
  ErrorCode read_sparse_tag_indices(const char* name,
                                    hid_t id_table,
                                    EntityHandle start_offset,
                                    Range& offset_range,
                                    Range& handle_range,
                                    std::vector<EntityHandle>& handle_vect);

  ErrorCode read_qa(EntityHandle file_set);

public:                               
  ErrorCode convert_id_to_handle(EntityHandle* in_out_array,
                                 size_t array_length);

  void convert_id_to_handle(EntityHandle* in_out_array,
                            size_t array_length,
                            size_t& array_length_out) const
    { return convert_id_to_handle(in_out_array, array_length, array_length_out, idMap); }

  ErrorCode convert_range_to_handle(const EntityHandle* ranges,
                                    size_t num_ranges,
                                    Range& merge);

  static
  void convert_id_to_handle(EntityHandle* in_out_array,
                            size_t array_length,
                            const RangeMap<long, EntityHandle>& id_map);

  static
  void convert_id_to_handle(EntityHandle* in_out_array,
                            size_t array_length,
                            size_t& array_length_out,
                            const RangeMap<long, EntityHandle>& id_map);

  static
  void convert_range_to_handle(const EntityHandle* ranges,
                               size_t num_ranges,
                               const RangeMap<long, EntityHandle>& id_map,
                               Range& merge);

  ErrorCode insert_in_id_map(const Range& file_ids,
                             EntityHandle start_id);

  ErrorCode insert_in_id_map(long file_id, EntityHandle handle);

private:

  /**\brief Search for entities with specified tag values
   * 
   *\NOTE For parallel reads, this function does collective IO.
   *
   *\param tag_index  Index into info->tags specifying which tag to search.
   *\param sorted_values  List of tag values to check for, in ascending sorted
   *                  order.
   *\param file_ids_out  File IDs for entities with specified tag values.
   */
  ErrorCode search_tag_values(int tag_index,
                              const std::vector<int>& sorted_values,
                              Range& file_ids_out,
                              bool sets_only = false);

  /**\brief Search for entities with specified tag
   * 
   *\NOTE For parallel reads, this function does collective IO.
   *
   *\param tag_index  Index into info->tags specifying which tag to search.
   *\param file_ids_out  File IDs for entities with specified tag values.
   */
  ErrorCode get_tagged_entities(int tag_index, Range& file_ids_out);
                                 
  /**\brief Search a table of tag data for a specified set of values.
   *
   * Search a table of tag values, returning the indices into the table
   * at which matches were found.
   *\NOTE For parallel reads, this function does collective IO.
   *
   *\param info       Summary of data contained in file.
   *\param tag_table     HDF5/mhdf handle for tag values
   *\param table_size    Number of values in table
   *\param sorted_values Sorted list of values to search for.
   *\param value_indices Output: Offsets into the table of data at which
   *                       matching values were found.
   */
  ErrorCode search_tag_values(hid_t tag_table,
                              unsigned long table_size,
                              const std::vector<int>& sorted_values,
                              std::vector<EntityHandle>& value_indices);

  /**\brief Get the file IDs for nodes and elements contained in sets.
   *
   * Read the contents for the specified sets and return the file IDs
   * of all nodes and elements contained within those sets.
   *\param sets       Container of file IDs designating entity sets.
   *\param file_ids   Output: File IDs of entities contained in sets.
   */
  ErrorCode get_set_contents(const Range& sets, Range& file_ids);
 
  /** Given a list of file IDs for entity sets, find all contained
   *  or child sets (at any depth) and append them to the Range
   *  of file IDs.
   */
  ErrorCode read_set_ids_recursive(Range& sets_in_out,
                                   bool containted_sets,
                                   bool child_sets);

  /** Find all sets containing one or more entities read from the file
   *  and added to idMap 
   */
  ErrorCode find_sets_containing(Range& sets_out, bool read_set_containing_parents);
 
  /**\brief Read sets from file into MOAB for partial read of file.
   *
   * Given the file IDs for entity sets (sets_in) and elements and
   * nodes (id_map), read in all sets containing any of the elements
   * or nodes and all sets that are (recursively) children of any
   * other set to be read (those in sets_in or those containing any
   * already-read element or node.)
   *\param sets_in    File IDs for sets to read (unconditionally)
   */
  ErrorCode read_sets_partial(const Range& sets_in);

  /** Find file IDs of sets containing any entities in the passed id_map */
  ErrorCode find_sets_containing(hid_t content_handle,
                                 hid_t content_type,
                                 long content_len,
                                 bool read_set_containing_parents,
                                 Range& file_ids);

public:
  enum SetMode {CONTENT = 0, CHILD = 1, PARENT = 2};

private:
  // Read the set data specified by by which_data.
  // Update set data in MOAB according to which_data,
  // unless file_ids_out is non-null. If file_ids_out is
  // non-null, then return the file IDs in the passed
  // range and do not make any changes to MOAB data
  // structures. If file_ids_out is NULL, then set_start_handle
  // is ignored.
  ErrorCode read_set_data(const Range& set_file_ids,
                          EntityHandle set_start_handle,
                          ReadHDF5Dataset& set_data_set,
                          SetMode which_data,
                          Range* file_ids_out = 0);

  /**\brief Store file IDS in tag values
   *
   * Copy file ID from IDMap for each entity read from file
   * into a tag value on the entity.
   */
  ErrorCode store_file_ids(Tag tag);

  /**\brief Store sets file IDS in a custom tag
   *
   * Copy sets file ID from IDMap for each set read from file
   *  into a custom tag on the sets
   *  needed now for the VisIt plugin, to identify sets read
   */
  ErrorCode store_sets_file_ids();

  /**\brief Find index in mhdf_FileDesc* fileInfo for specified tag name
   *
   * Given a tag name, find its index in fileInfo and verify that
   * each tag value is a single integer.
   */
  ErrorCode find_int_tag(const char* name, int& index_out);

  void debug_barrier_line(int lineno);
};

} // namespace moab

#endif
