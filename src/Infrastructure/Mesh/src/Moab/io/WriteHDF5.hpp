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


#ifndef WRITE_HDF5_HPP
#define WRITE_HDF5_HPP

#include <list>
#include "moab/MOABConfig.h"
#ifdef MOAB_HAVE_MPI // include this before HDF5 headers to avoid conflicts
#  include "moab_mpi.h"
#endif
#include "moab_mpe.h"
#include "mhdf.h"
#include "moab/Forward.hpp"
#include "moab/Range.hpp"
#include "moab/WriterIface.hpp"
#include "moab/RangeMap.hpp"
#include "moab/WriteUtilIface.hpp"
#include "DebugOutput.hpp"
#include "HDF5Common.hpp"

namespace moab {

class IODebugTrack;

/* If this define is not set, node->entity adjacencies will not be written */
#undef MB_H5M_WRITE_NODE_ADJACENCIES

/**
 * \brief  Write mesh database to MOAB's native HDF5-based file format.
 * \author Jason Kraftcheck
 * \date   01 April 2004
 */
class WriteHDF5 : public WriterIface
{

public:

  static WriterIface* factory( Interface* );

  WriteHDF5( Interface* iface );
  
  virtual ~WriteHDF5();
  
  /** Export specified meshsets to file
   * \param filename     The filename to export. 
   * \param export_sets  Array of handles to sets to export, or NULL to export all.
   * \param export_set_count Length of <code>export_sets</code> array.
   */
  ErrorCode write_file( const char* filename,
                          const bool overwrite,
                          const FileOptions& opts,
                          const EntityHandle* export_sets,
                          const int export_set_count,
                          const std::vector<std::string>& qa_records,
                          const Tag* tag_list = NULL,
                          int num_tags = 0,
                          int user_dimension = 3 );

  /** The type to use for entity IDs w/in the file.
   * 
   * NOTE:  If this is changed, the value of id_type 
   *        MUST be changed accordingly.
   */
  typedef EntityHandle wid_t; // change the name,
  //  to avoid conflicts to /usr/include/x86_64-linux-gnu/bits/types.h : id_t , which is unsigned int
  
  /** HDF5 type corresponding to type of wid_t */
  static const hid_t id_type;

  struct ExportType
  {
      //! The type of the entities in the range
    EntityType type;
      //! The number of nodes per entity - not used for nodes and sets
    int num_nodes;

    virtual ~ExportType()
    { }
    
    bool operator==(ExportType t) const
      { return t.type == type && t.num_nodes == num_nodes; }
    bool operator!=(ExportType t) const
      { return t.type != type || t.num_nodes != num_nodes; }
    bool operator<(ExportType t) const
      { return type < t.type || (type == t.type && num_nodes < t.num_nodes); }
  }; 

  //! Range of entities, grouped by type, to export 
  struct ExportSet : public ExportType
  {
    //! The range of entities.
    Range range;
    //! The first Id allocated by the mhdf library.  Entities in range have sequential IDs.
    wid_t first_id;
    //! The offset at which to begin writing this processor's data.
    //! Always zero except for parallel IO.
    long offset;
    //! Offset for adjacency data.  Always zero except for parallel IO
    long adj_offset;
    //! If doing parallel IO, largest number of entities to write
    //! for any processor (needed to do collective IO).  Zero if unused.
    long max_num_ents, max_num_adjs;
    //! The total number of entities that will be written to the file
    //! for this group.  For serial IO, this should always be range.size().
    //! For parallel IO, it will be the sum of range size over all processors.
    //! For parallel IO, this value is undefined except for on the root 
    //! processor.
    long total_num_ents;
    
    bool operator<( const ExportType& other ) const
      { return type < other.type || 
               (type == other.type && num_nodes < other.num_nodes); }
    
    bool operator<( std::pair<int,int> other ) const
      { return type < other.first || 
               (type == other.first && num_nodes < other.second); }
    
    bool operator==( const ExportType& other ) const
      { return (type == other.type && num_nodes == other.num_nodes); }
    
    bool operator==( std::pair<int,int> other ) const
      { return (type == other.first && num_nodes == other.second); }
                
    const char* name() const;
  };
  
  //! Tag to write to file.
  struct TagDesc
  {
    //! The tag handle
    Tag tag_id;
    //! The offset at which to begin writting this processor's data.
    //! Always zero except for parallel IO. 
    wid_t sparse_offset;
    //! For variable-length tags, a second offset for the tag data table,
    //! separate from the offset used for the ID and Index tables.
    //! Always zero except for parallel IO. 
    wid_t var_data_offset;
    //! Write sparse tag data (for serial, is always equal to !range.empty())
    bool write_sparse;
    //! If doing parallel IO, largest number, over all processes, of entities
    //! for which to write tag data.  Zero if unused.
    unsigned long max_num_ents;
    //! For variable-length tags during parallel IO: the largest number
    //! of tag values to be written on by any process, used to calculate
    //! the total number of collective writes that all processes must do.
    //! Zero for fixed-length tags or if not doing parallel IO.
    unsigned long max_num_vals;
    
    //! List of entity groups for which to write tag data in 
    //! dense format
    std::vector<ExportType> dense_list;
    
    bool have_dense( const ExportType& type ) const
      { return std::find(dense_list.begin(), dense_list.end(), type) != dense_list.end(); }
    
    bool operator<(const TagDesc&) const;
  };

  /** Create attributes holding the HDF5 type handle for the 
   *  type of a bunch of the default tags.
   */
  //static ErrorCode register_known_tag_types( Interface* );
  
  //! Store old HDF5 error handling function
  struct HDF5ErrorHandler {
    HDF5_Error_Func_Type func;
    void* data;
  };
  
  mhdf_FileHandle file_ptr() { return filePtr; }
  
  WriteUtilIface* write_util() { return writeUtil; }

protected:
  
  //! Store old HDF5 error handling function
  HDF5ErrorHandler errorHandler;

  /** Function to create the file.  Virtual to allow override
   *  for parallel version.
   */
  virtual ErrorCode parallel_create_file( const char* filename,
                                            bool overwrite,
                                            const std::vector<std::string>& qa_records,
                                            const FileOptions& opts,
                                            const Tag* tag_list,
                                            int num_tags,
                                            int dimension = 3,
                                            double* times = 0 );
  virtual ErrorCode write_finished();
  virtual void debug_barrier_line(int lineno);
 
  //! Gather tags
  ErrorCode gather_tags( const Tag* user_tag_list, int user_tag_list_length );

  /** Check if tag values for a given ExportSet should be written in dense format
   *
   *\param ents        ExportSet to consider
   *\param all_tagged  Range containing all the entities in ents.range for 
   *                   which an explicit tag value is stored.  Range may
   *                   also contain entities not in ents.range, but may
   *                   not contain entities in ents.range for which no tag
   *                   value is stored.
   *\param prefer_dense If true, will return true if at least 2/3 of the
   *                   entities are tagged.  This should not be passed as
   *                   true if the tag does not have a default value, as
   *                   tag values must be stored for all entities in the
   *                   ExportSet for dense-formatted data.  
   */
  bool check_dense_format_tag( const ExportSet& ents, 
                               const Range& all_tagged,
                               bool prefer_dense );

  /** Helper function for create-file
   *
   * Calculate the sum of the number of non-set adjacencies
   * of all entities in the passed range.
   */
  ErrorCode count_adjacencies( const Range& elements, wid_t& result );
  
public: // make these public so helper classes in WriteHDF5Parallel can use them

  /** Helper function for create-file
   *
   * Create zero-ed tables where element connectivity and 
   * adjacency data will be stored.
   */
  ErrorCode create_elem_table( const ExportSet& block, long num_ents, long& first_id_out );
  
  /** Helper function for create-file
   *
   * Create zero-ed table where set descriptions will be written
   */
  ErrorCode create_set_meta( long num_sets, long& first_id_out );

protected:
  
  /** Helper function for create-file
   *
   * Calculate total length of set contents and child tables.
   */
  ErrorCode count_set_size( const Range& sets,
                              long& contents_length_out,
                              long& children_length_out,
                              long& parents_length_out );

  //! Get information about a meshset
  ErrorCode get_set_info( EntityHandle set,
                            long& num_entities,
                            long& num_children,
                            long& num_parents,
                            unsigned long& flags );

  /** Helper function for create-file
   *
   * Create zero-ed tables where set data will be written.
   */
  ErrorCode create_set_tables( long contents_length,
                               long children_length,
                               long parents_length );

  //! Write exodus-type QA info
  ErrorCode write_qa( const std::vector<std::string>& list );

  //!\brief Get tagged entities for which to write tag values
  ErrorCode get_num_sparse_tagged_entities( const TagDesc& tag, size_t& count );
  //!\brief Get tagged entities for which to write tag values
  ErrorCode get_sparse_tagged_entities( const TagDesc& tag, Range& range );
  //!\brief Get entities that will be written to file
  void get_write_entities( Range& range );
  
  //! The size of the data buffer (<code>dataBuffer</code>).
  size_t bufferSize;
  //! A memory buffer to use for all I/O operations.
  char* dataBuffer;

  //! Interface pointer passed to constructor
  Interface* iFace;
  //! Cached pointer to writeUtil interface.
  WriteUtilIface* writeUtil;
  
  //! The file handle from the mhdf library
  mhdf_FileHandle filePtr;
  
  //! Map from entity handles to file IDs
  RangeMap<EntityHandle,wid_t> idMap;
  
  //! The list elements to export.
  std::list<ExportSet> exportList;
  //! The list of nodes to export
  ExportSet nodeSet;
  //! The list of sets to export
  ExportSet setSet;
  
  const ExportSet* find( ExportType type ) const {
    if (type.type == MBVERTEX)
      return &nodeSet;
    else if (type.type == MBENTITYSET)
      return &setSet;
    else {
      std::list<ExportSet>::const_iterator it;
      it = std::find( exportList.begin(), exportList.end(), type );
      return it == exportList.end() ? 0 : &*it;
    }
  }
  
  //! Offset into set contents table (zero except for parallel)
  unsigned long setContentsOffset;
  //! Offset into set children table (zero except for parallel)
  unsigned long setChildrenOffset, setParentsOffset;
  //! The largest number of values to write
  //! for any processor (needed to do collective IO). 
  long maxNumSetContents, maxNumSetChildren, maxNumSetParents;
  //! Flags idicating if set data should be written.
  //! For the normal (non-parallel) case, these values
  //! will depend only on whether or not there is any
  //! data to be written.  For parallel-meshes, opening
  //! the data table is collective so the values must
  //! depend on whether or not any processor has meshsets
  //! to be written.
  bool writeSets, writeSetContents, writeSetChildren, writeSetParents;
  
  //! Struct describing a set for which the contained and linked entity
  //! lists are something other than the local values.  Used to store
  //! data for shared sets owned by this process when writing in parallel.
  struct SpecialSetData {
    EntityHandle setHandle;
    unsigned setFlags;
    std::vector<wid_t> contentIds;
    std::vector<wid_t> childIds;
    std::vector<wid_t> parentIds;
  };
  struct SpecSetLess {
    bool operator() (const SpecialSetData& a, SpecialSetData b) const
      { return a.setHandle < b.setHandle; }
  };

  
  //! Array of special/shared sets, in order of handle value.
  std::vector<SpecialSetData> specialSets;
  const SpecialSetData* find_set_data( EntityHandle h ) const
    { return const_cast<WriteHDF5*>(this)->find_set_data(h); }
  SpecialSetData* find_set_data( EntityHandle h );
  
  //! The list of tags to export
  std::list<TagDesc> tagList;

  //! True if doing parallel write
  bool parallelWrite;
  //! True if using collective IO calls for parallel write
  bool collectiveIO;
  //! True if writing dense-formatted tag data
  bool writeTagDense;
  
  //! Property set to pass to H5Dwrite calls. 
  //! For serial, should be H5P_DEFAULTS.
  //! For parallel, may request collective IO.
  hid_t writeProp;
  
  //! Utility to log debug output
  DebugOutput dbgOut;
  
  static MPEState topState;
  static MPEState subState;
  
  //! Look for overlapping and/or missing writes
  bool debugTrack;

  void print_id_map() const;
  void print_id_map( std::ostream& str, const char* prefix = "" ) const;

  /** Helper function for create-file
   *
   * Write tag meta-info and create zero-ed table where
   * tag values will be written.
   *\param num_entities  Number of entities for which to write tag data.
   *\param var_len_total For variable-length tags, the total number of values
   *                     in the data table.
   */
  ErrorCode create_tag( const TagDesc& tag_data, 
                        unsigned long num_entities,
                        unsigned long var_len_total );
  
  /**\brief add entities to idMap */
  ErrorCode assign_ids( const Range& entities, wid_t first_id );
  
  /** Get possibly compacted list of IDs for passed entities
   *
   * For the passed range of entities, determine if IDs
   * can be compacted and write IDs to passed list.
   *
   * If the IDs are not compacted, the output list will contain
   * a simple ordered list of IDs.
   *
   * If IDs are compacted, the output list will contain 
   * {start,count} pairs.
   *
   * If the ID list is compacted, ranged_list will be 'true'.
   * Otherwise it will be 'false'.
   */
  ErrorCode range_to_blocked_list( const Range& input_range,
                                   std::vector<wid_t>& output_id_list ,
                                   bool& ranged_list );
  
  /** Get possibly compacted list of IDs for passed entities
   *
   * For the passed range of entities, determine if IDs
   * can be compacted and write IDs to passed list.
   *
   * If the IDs are not compacted, the output list will contain
   * a simple ordered list of IDs.
   *
   * If IDs are compacted, the output list will contain 
   * {start,count} pairs.
   *
   * If the ID list is compacted, ranged_list will be 'true'.
   * Otherwise it will be 'false'.
   */
  ErrorCode range_to_blocked_list( const EntityHandle* input_ranges,
                                   size_t num_input_ranges,
                                   std::vector<wid_t>& output_id_list ,
                                   bool& ranged_list );
  

  ErrorCode range_to_id_list( const Range& input_range,
                                wid_t* array );
  //! Get IDs for entities 
  ErrorCode vector_to_id_list( const std::vector<EntityHandle>& input,
                               std::vector<wid_t>& output,
                               bool remove_non_written = false );
  //! Get IDs for entities 
  ErrorCode vector_to_id_list( const EntityHandle* input,
                               wid_t* output,
                               size_t num_entities );
  //! Get IDs for entities 
  ErrorCode vector_to_id_list( const EntityHandle* input,
                               size_t input_len,
                               wid_t* output,
                               size_t& output_len,
                               bool remove_non_written );

  /** When writing tags containing EntityHandles to file, need to convert tag
   *  data from EntityHandles to file IDs.  This function does that. 
   *
   * If the handle is not valid or does not correspond to an entity that will
   * be written to the file, the file ID is set to zero.
   *\param data  The data buffer.  As input, an array of EntityHandles.  As
   *             output an array of file IDS, where the size of each integral
   *             file ID is the same as the size of EntityHandle.
   *\param count The number of handles in the buffer.
   *\return true if at least one of the handles is valid and will be written to
   *             the file or at least one of the handles is NULL (zero). false
   *             otherwise
   */
  bool convert_handle_tag( EntityHandle* data, size_t count ) const;
  bool convert_handle_tag( const EntityHandle* source,
                           EntityHandle* dest, 
                           size_t count ) const;

  /** Get IDs of adjacent entities.
   * 
   * For all entities adjacent to the passed entity, if the
   * adjacent entity is to be exported (ID is not zero), append
   * the ID to the passed list.
   */
  ErrorCode get_adjacencies( EntityHandle entity, std::vector<wid_t>& adj );
                                
  //! get sum of lengths of tag values (as number of type) for 
  //! variable length tag data.
  ErrorCode get_tag_data_length( const TagDesc& tag_info,
                                 const Range& range,
                                 unsigned long& result );
  
private:

  //! Do the actual work of write_file.  Separated from write_file
  //! for easier resource cleanup.
  ErrorCode write_file_impl( const char* filename,
                               const bool overwrite,
                               const FileOptions& opts,
                               const EntityHandle* export_sets,
                               const int export_set_count,
                               const std::vector<std::string>& qa_records,
                               const Tag* tag_list,
                               int num_tags,
                               int user_dimension = 3 );

  ErrorCode init();
  
  ErrorCode serial_create_file( const char* filename,
                                  bool overwrite,
                                  const std::vector<std::string>& qa_records,
                                  const Tag* tag_list,
                                  int num_tags,
                                  int dimension = 3 );
  
  /** Get all mesh to export from given list of sets.
   *
   * Populate exportSets, nodeSet and setSet with lists of
   * entities to write.
   *
   * \param export_sets  The list of meshsets to export
   */
  ErrorCode gather_mesh_info( const std::vector<EntityHandle>& export_sets );
  
  //! Same as gather_mesh_info, except for entire mesh
  ErrorCode gather_all_mesh( );
  
  //! Initialize internal data structures from gathered mesh
  ErrorCode initialize_mesh( const Range entities_by_dim[5] );
 
  /** Write out the nodes.
   *
   * Note: Assigns IDs to nodes.
   */
  ErrorCode write_nodes( );
  
  /** Write out element connectivity.
   *
   * Write connectivity for passed set of elements.
   *
   * Note: Assigns element IDs.
   * Note: Must do write_nodes first so node IDs get assigned.
   */
  ErrorCode write_elems( ExportSet& elemset );
  
  /** Write out meshsets
   * 
   * Write passed set of meshsets, including parent/child relations.
   *
   * Note: Must have written nodes and element connectivity
   *       so entities have assigned IDs.
   */
  ErrorCode write_sets( double* times );

  /** Write set contents/parents/children lists
   *
   *\param which_data Which set data to write (contents, parents, or children)
   *\param handle     HDF5 handle for data set in which to write data
   *\param track      Debugging tool
   *\param ranged     Will be populated with handles of sets for which
   *                  contents were written in a range-compacted format.
   *                  (mhdf_SET_RANGE_BIT).  Should be null for parents/children.
   *\param null_stripped Will be populated with handles of sets for which
   *                  invalid or null handles were stripped from the contents
   *                  list.  This is only done for unordered sets.  This argument
   *                  should be null if writing parents/children because those
   *                  lists are always ordered.
   *\param set_sizes  Will be populated with the length of the data written
   *                  for those sets for which the handles were added to
   *                  either \c ranged or \c null_stripped.  Values are
   *                  in handle order.
   */
  ErrorCode write_set_data( const WriteUtilIface::EntityListType which_data,
                            const hid_t handle,
                            IODebugTrack& track,
                            Range* ranged = 0,
                            Range* null_stripped = 0,
                            std::vector<long>* set_sizes = 0);
  
  /** Write adjacency info for passed set of elements
   *
   * Note: Must have written element connectivity so elements
   *       have IDs assigned.
   */
  ErrorCode write_adjacencies( const ExportSet& export_set );
  
  /** Write tag information and data.
   * 
   * Note: Must have already written nodes, elem connectivity and
   *       sets so that entities have IDs assigned.
   */

  //! Write tag for all entities.
  ErrorCode write_tag( const TagDesc& tag_data,
                       double* times );
                            
  //! Get element connectivity
  ErrorCode get_connectivity( Range::const_iterator begin,
                              Range::const_iterator end,
                              int nodes_per_element,
                              wid_t* id_data_out );
                                   
  //! Get size data for tag
  //!\param tag       MOAB tag ID
  //!\param moab_type Output: DataType for tag
  //!\param num_bytes Output: MOAB tag size (bits for bit tags).
  //!                         MB_VARIABLE_LENGTH for variable-length tags.
  //!\param elem_size Output: Size of of the base data type of the
  //!                         tag data (e.g. sizeof(double) if
  //!                         moab_type == MB_TYPE_DOUBLE).
  //!                         One for bit and opaque tags.
  //!\param array_size Output: The number of valeus of size elem_size
  //!                          for each tag.  Always 1 for opaque data.
  //!                          Nubmer of bits for bit tags.
  //!\param file_type Output: mhdf type enumeration
  //!\param hdf_type  Output: Handle to HDF5 type object.  Caller is
  //!                         responsible for releasing this object
  //!                         (calling H5Tclose).
  ErrorCode get_tag_size( Tag tag,
                          DataType& moab_type,
                          int& num_bytes,
                          int& elem_size,
                          int& file_size,
                          mhdf_TagDataType& file_type,
                          hid_t& hdf_type );
                            
  //! Write ID table for sparse tag
  ErrorCode write_sparse_ids( const TagDesc& tag_data, 
                              const Range& range,
                              hid_t table_handle, 
                              size_t table_size, 
                              const char* name = 0 );
  
  //! Write fixed-length tag data in sparse format
  ErrorCode write_sparse_tag( const TagDesc& tag_data,
                              const std::string& tag_name,
                              DataType tag_data_type,
                              hid_t hdf5_data_type,
                              int hdf5_type_size );

  //! Write end index data_set for a variable-length tag
  ErrorCode write_var_len_indices( const TagDesc& tag_data,
                                   const Range& range,
                                   hid_t idx_table,
                                   size_t table_size,
                                   int type_size,
                                   const char* name = 0 );
  
  //! Write tag value data_set for a variable-length tag
  ErrorCode write_var_len_data( const TagDesc& tag_data,
                                const Range& range,
                                hid_t table,
                                size_t table_size,
                                bool handle_tag,
                                hid_t hdf_type,
                                int type_size,
                                const char* name = 0 );
  
  //! Write varialbe-length tag data
  ErrorCode write_var_len_tag( const TagDesc& tag_info,
                               const std::string& tag_name,
                               DataType tag_data_type,
                               hid_t hdf5_type,
                               int hdf5_type_size );

  //! Write dense-formatted tag data
  ErrorCode write_dense_tag( const TagDesc& tag_data,
                             const ExportSet& elem_data,
                             const std::string& tag_name,
                             DataType tag_data_type,
                             hid_t hdf5_data_type,
                             int hdf5_type_size );

  //! Write data for fixed-size tag
  ErrorCode write_tag_values( Tag tag_id,
                              hid_t data_table,
                              unsigned long data_offset,
                              const Range& range,
                              DataType tag_data_type,
                              hid_t hdf5_data_type,
                              int hdf5_type_size,
                              unsigned long max_num_ents,
                              IODebugTrack& debug_track );

protected:

  enum TimingValues { 
       TOTAL_TIME = 0,
         GATHER_TIME,
         CREATE_TIME,
           CREATE_NODE_TIME,
           NEGOTIATE_TYPES_TIME,
           CREATE_ELEM_TIME,
           FILEID_EXCHANGE_TIME,
           CREATE_ADJ_TIME,
           CREATE_SET_TIME,
             SHARED_SET_IDS,
             SHARED_SET_CONTENTS,
             SET_OFFSET_TIME,
           CREATE_TAG_TIME,
         COORD_TIME,
         CONN_TIME,
         SET_TIME,
           SET_META,
           SET_CONTENT,
           SET_PARENT,
           SET_CHILD,
         ADJ_TIME,
         TAG_TIME,
           DENSE_TAG_TIME,
           SPARSE_TAG_TIME,
           VARLEN_TAG_TIME,
         NUM_TIMES };

  
  virtual void print_times( const double times[NUM_TIMES] ) const;
};

} // namespace moab

#endif
