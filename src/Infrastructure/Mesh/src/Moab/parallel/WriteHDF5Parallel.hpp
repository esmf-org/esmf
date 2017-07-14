
#ifndef WRITE_HDF5_PARALLEL_HPP
#define WRITE_HDF5_PARALLEL_HPP

#include "WriteHDF5.hpp"
#include <H5Spublic.h>
#include <map>

namespace moab {

struct RemoteSetData;
class ParallelComm;
class IODebugTrack;

/** 
 * \brief Write MOAB HDF5 file in parallel.
 * \author Jason Kraftcheck
 * \data   22 July 2004
 */
class WriteHDF5Parallel : public WriteHDF5
{
  public:

    static WriterIface* factory( Interface* );
    
      /** Consturctor
       */
    WriteHDF5Parallel( Interface* iface );

    virtual ~WriteHDF5Parallel();
    
  
  protected:
    
    virtual void debug_barrier_line(int lineno);

  
    virtual void print_times( const double* times ) const;
  
      //! Called by normal (non-parallel) writer.  Sets up
      //! necessary data for parallel write.
    virtual ErrorCode parallel_create_file( const char* filename,
                                     bool overwrite,
                                     const std::vector<std::string>& qa_records,
                                     const FileOptions& opts,
                                     const Tag* user_tag_list = 0,
                                     int user_tag_count = 0,
                                     int dimension = 3,
                                     double* times = 0);
    
      //! Figure out which mesh local mesh is duplicated on
      //! remote processors and which processor will write
      //! that mesh.
      //!\param non_local_ents Output list of entities that are not to
      //!                  be written by this processor but are
      //!                  referenced by other entities that are
      //!                  to be written.
    ErrorCode gather_interface_meshes( Range& non_local_ents );
    
      //! For entities that will be written by another 
      //! processor but are referenced by entities on this
      //! processor, get the file Ids that will be assigned
      //! to those so they can be referenced by
      //! entities to be written on this processor.
      //!\param non_local_ents List of entities that are not to
      //!                  be written by this processor but are
      //!                  referenced by other entities that are
      //!                  to be written.
    ErrorCode exchange_file_ids( const Range& non_local_ents );
    
      //! Get remote ids for shared sets
    ErrorCode communicate_shared_set_ids( const Range& owned, const Range& remote );
    
      //! Pack set data for communication.  
      //!
      //! If set_data_length is insufficient for the set data,
      //! the length entries at indices 1, 2, and 3 of set_data
      //! will be set with the necessary lengths, but no data will
      //! be written to set_data beyond that.
    ErrorCode pack_set( Range::const_iterator set,
                        unsigned long* set_data,
                        size_t set_data_length );
  
      //! Unpack set data from communication
    ErrorCode unpack_set( EntityHandle set, 
                          const unsigned long* set_data, 
                          size_t set_data_length );
                          
      //! Communicate set contents between processors such that each
      //! owner knows the contents, parents, & child lists from all
      //! processors that have a copy of the set.
    ErrorCode communicate_shared_set_data( const Range& owned, const Range& remote );
    
      //! Create the node table in the file.
    ErrorCode create_node_table( int dimension );
    
      //! Communicate with other processors to negotiate 
      //! the types of elements that will be written
      //! (the union of the types defined on each proc.)
    ErrorCode negotiate_type_list();
    
      //! Create tables to hold element connectivity
    ErrorCode create_element_tables();
    
      //! Create tables to hold element adjacencies.
    ErrorCode create_adjacency_tables();
    
      //! Create tables for mesh sets
    ErrorCode create_meshset_tables(double* times);
    
      //! Write tag descriptions and create tables to hold tag data.
    ErrorCode create_tag_tables();
   
      //! Remove any remote mesh entities from the passed range.
    void remove_remote_entities( EntityHandle relative, Range& range );
    void remove_remote_entities( EntityHandle relative, std::vector<EntityHandle>& vect );
    void remove_remote_sets( EntityHandle relative, Range& range );
    void remove_remote_sets( EntityHandle relative, std::vector<EntityHandle>& vect );
  
      //! get any existing tags which aren't excluded and add to shared set tags
    ErrorCode get_sharedset_tags();

    ErrorCode append_serial_tag_data( std::vector<unsigned char>& buffer,
                                      const WriteHDF5::TagDesc& tag );

      //! helper function for create_tag_tables
    ErrorCode check_serial_tag_data( 
                               const std::vector<unsigned char>& buffer,
                               std::vector<TagDesc*>* missing = 0,
                               std::vector<TagDesc*>* newlist = 0 );
  
      /**\brief Argument ot create_dataset */
    struct DataSetCreator {
      virtual ErrorCode operator()( WriteHDF5* writer,
                                    long data_set_size,
                                    const ExportSet* group, 
                                    long& start_id_out ) const = 0;
    };
    struct NoopDescCreator : public DataSetCreator {
      ErrorCode operator()( WriteHDF5*, long, const ExportSet*, long& start_id ) const
      { start_id = -1; return MB_SUCCESS; }
    };
    
      /**\brief Do typical communication for dataset creation
       *
       * Given the number of entities each processor intends to write,
       * do necessary communication and create dataset on root, passing
       * back misc info to each proc.
       *
       *\param creator             Functor to do actual dataset creation.  Used 
       *                           only on root process.
       *\param num_datasets        The number of datasets to create.
       *\param groups              Third argument passed to DataSetCreator.
       *                           Array of length \c num_datasets pr NULL.
       *\param num_owned_entities  The number of entities this proc will write.
       *                           Array of length \c num_datasets .
       *\param offsets_out         Output: The offset in the dataset at which 
       *                           this process should write.  
       *                           Array of length \c num_datasets .
       *\param max_proc_ents_out   Output: The maximun number of entities that 
       *                           any proc will write
       *                           Array of length \c num_datasets .
       *\param total_ents_out      Output: The size of the created dataset (sum 
       *                           of counts over all procs)
       *                           Array of length \c num_datasets .
       *\param first_ids_out       Output: The first ID of the first entity in the 
       *                           data set.  First ID for this proc's entities is 
       *                           first_id_out+offset_out
       *                           Array of length \c num_datasets or NULL.
       */
    ErrorCode create_dataset( int num_datasets,
                              const long* num_owned_entities,
                              long* offsets_out,
                              long* max_proc_ents_out,
                              long* total_ents_out,
                              const DataSetCreator& creator = NoopDescCreator(),
                              ExportSet* groups[] = 0,
                              wid_t* first_ids_out = NULL );
  
    void print_shared_sets();
    void print_set_sharing_data( const Range& range, const char* label, Tag idt );
  
  private:

      //! pcomm controlling parallel nature of mesh
    ParallelComm *myPcomm;

      //! whether this instance allocated (and dtor should delete) the pcomm
    bool pcommAllocated;
    
      //! Operation to use to append hyperslab selections
    H5S_seloper_t hslabOp;
};

} // namespace moab

#endif
