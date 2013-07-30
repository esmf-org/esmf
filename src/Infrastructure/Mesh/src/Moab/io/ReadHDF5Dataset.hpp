/** \file   ReadHDF5Dataset.hpp
 *  \author Jason Kraftcheck 
 *  \date   2010-07-09
 */

#ifndef moab_READ_HDF5DATASET_HPP
#define moab_READ_HDF5DATASET_HPP

#ifdef USE_MPI
# include <moab_mpi.h>
#endif

#include <stdlib.h> // for size_t
#include <H5Ipublic.h>
#include <H5Spublic.h>

#include "moab/Range.hpp"
#include <vector>

namespace moab {

/**\brief Utility used for reading portions of an HDF5 dataset
 *
 * Implement iterative read of table where:
 * - subset of rows to be read can be specified usign an Range of offsets
 * - each read fills as much as possible of a passed buffer 
 * - each read call reads a subsequent set of rows of the data set in an
 *   iterator-like fashion.
 *
 * NOTE: This class also implements an RAII pattern for the data set handle:
 *       It will close the data set in its destructor unless it is specified
 *       to the constructor that only a single column should be read.
 *
 * NOTE: This class will always do collective IO for parallel reads.
 */
class ReadHDF5Dataset 
{
public:

#ifdef USE_MPI
  typedef MPI_Comm Comm;
#else
  typedef int Comm;
#endif

  class Exception { public: int line_no; Exception(int l) : line_no(l) {} };
  
  /**\brief Setup to read entire table
   *\param data_set_handle The HDF5 DataSet to read.
   *\param parallel    Doing true partial-read parallel read (as opposed
   *                   to read and delete where collective IO is done for
   *                   everything because all procs read the same stuff.)
   *\param communictor If \c parallel is \c true and \c io_prop is 
   *                   \c H5FD_MPIO_COLLECTIVE, then this
   *                   must be a pointer to the MPI_Communicator value.
   *\param close_data_set_on_destruct Call \c H5Dclose on passed
   *                 \c data_set_handle in desturctor.
   *
   *\NOTE If \c parallel is \c true and \c io_prop is \c H5FD_MPIO_COLLECTIVE,
   *      then not only must \c communicator be non-null, but this call must
   *      be made collectively!

   *\NOTE Class instance will not be usable until one of either
   *      \c set_file_ids or \c set_all_file_ids is called.
   */
  ReadHDF5Dataset( const char* debug_desc,
                   hid_t data_set_handle,
                   bool parallel,
                   const Comm* communicator = 0,
                   bool close_data_set_on_destruct = true );
  
  ReadHDF5Dataset( const char* debug_desc,
                   bool parallel,
                   const Comm* communicator = 0 );
  void init( hid_t data_set_handle, bool close_data_set_on_destruct = true );
  
  bool will_close_data_set() const { return closeDataSet; }
  void close_data_set_on_destruct( bool val ) { closeDataSet = val; }
  
  ~ReadHDF5Dataset();
  
  
  /**\brief Change file ids to read from. 
   *
   *\param file_ids    List of rows to read from dataset
   *\param start_id    Rows of dataset are enumerating beginning with
   *                   this value.  Thus the offset row to be read from
   *                   dataset will be \c file_ids.begin() - \c start_id .
   *\param row_count   Read buffer size in number of table rows.
   *\param data_type       The data type of the buffer into which table values
   *                       are to be read.
   */
  void set_file_ids( const Range& file_ids, 
                     EntityHandle start_id,
                     hsize_t row_cout,
                     hid_t data_type );
  
  /**\brief Read all values in dataset (undo set_file_ids)
   *
   *\param row_count   Read buffer size in number of table rows.
   *\param data_type       The data type of the buffer into which table values
   *                       are to be read.
   */
  void set_all_file_ids( hsize_t row_count, hid_t data_type );
  
  /**\brief Return false if more data to read, true otherwise
   *
   * Test if the iterative read has reached the end.
   */
  bool done() const { return (currOffset == rangeEnd) && (readCount == 0); }
  
  /**\brief Read rows of table
   *
   * Read up to max_num_rows from data set.
   *\param buffer    Memory in which to store values read from data set
   *\param rows_read The actual number of rows read from the table.  Will
   *                 never exceed \c max_rows .
   */
  void read( void* buffer, size_t& rows_read );
  
  /**\brief Return position in \c Range of file IDs at which next read will start
   */
  Range::const_iterator next_file_id() const { return currOffset; }
  
  /**\brief Do null read operation
   *
   * Do a read call requesting no data.  This functionality is provided
   * so as to allow collective IO when not all processes need to make the
   * same number of read calls.  To prevent deadlock in this case, processes
   * that have finished their necessary read calls can call this function
   * so that all processes are calling the read method collectively.
   */
  void null_read();
  
  unsigned columns() const;
  void set_column( unsigned c );
  
  unsigned long get_read_count() const { return readCount; }
  const char* get_debug_desc() const { return mpeDesc.c_str(); }

  static void set_hyperslab_selection_limit( size_t val )
    { hyperslabSelectionLimit = val; }
  static void default_hyperslab_selection_limit();

    /** Use non-standard 'APPEND' operation for hyperslab selection */
  static void append_hyperslabs() { hyperslabSelectOp = H5S_SELECT_APPEND; }
    /** Revert to default select behavior for standard HDF5 library */
  static void or_hyperslabs() { hyperslabSelectOp = H5S_SELECT_OR; }
private:

  Range::const_iterator next_end( Range::const_iterator iter );

  Range internalRange; //!< used when reading entire dataset

  bool closeDataSet; //!< close dataset in destructor
  hsize_t dataSetOffset[64], dataSetCount[64];
  hid_t dataSet;       //!< Handle for HDF5 data set
  hid_t dataSpace;     //!< Data space for data set
  hid_t dataType;      //!< Data type client code wants for data
  hid_t fileType;      //!< Data type as stored in data set
  hid_t ioProp;        //!< Used to specify collective IO
  int dataSpaceRank;   //!< Rank of data set
  hsize_t rowsInTable; //!< Total number of rows in dataset
  bool doConversion;   //!< True if dataType != fileType
  bool nativeParallel; //!< If true then reading different data on different procs
  
  hsize_t readCount;  //!< Number of actual reads to do
  hsize_t bufferSize; //!< size of buffer passed to \c read, in number of rows
  const Comm* mpiComm;
  
  Range::const_iterator currOffset, rangeEnd;
  EntityHandle startID;
  
  static bool haveMPEEvents;
  static std::pair<int,int> mpeReadEvent;
  static std::pair<int,int> mpeReduceEvent;
  std::string mpeDesc;
  
  static size_t hyperslabSelectionLimit;
  static H5S_seloper_t hyperslabSelectOp;
}; 



} // namespace moab

#endif // moab_READ_HDF5DATASET_HPP
