#ifndef moab_IO_DEBUG_TRACK_HPP
#define moab_IO_DEBUG_TRACK_HPP

#include <list>
#include <iosfwd>
#include <string>

namespace moab {

/**\brief Tool for debugging binary IO
 *
 * Track which ranges of a table of data have been read/written,
 * watching for overlapping IO requests and ranges of unaccessed
 * data.
 *
 * Notes:  This class assumes MPI_COMM_WORLD is the communicator
 *         for parallel.
 */
class IODebugTrack {
  private:
    struct DRange {
      unsigned long begin;
      unsigned long end;
      unsigned long rank;
    };
  
    bool enableOutput;
    std::string tableName;
    std::list<DRange> dataSet;
    std::ostream& ostr;
    unsigned long maxSize;
    int mpiRank;
    bool haveMPI;
    
    void record_io( DRange data );

  public:
  
      /**\brief Constuctor requires stream to which to log errors 
       *\param table_name    Used to tag output
       *\param output_stream Stream to which to print error messages
       *\param table_size Max table size.  No limit if unspecified 
       */
    IODebugTrack( bool enable,
                  const std::string& table_name,
                  std::ostream& output_stream,
                  unsigned long table_size = 0 ) ;
  
      /**\brief Constuctor requires stream to which to log errors 
       *\param table_name    Used to tag output
       *\param table_size Max table size.  No limit if unspecified 
       */
    IODebugTrack( bool enable,
                  const std::string& table_name,
                  unsigned long table_size = 0 ) ;
    
      /**\brief Destructor prints errors about unaccessed ranges */
    ~IODebugTrack();
    
      /**\brief Notify of IO request
       *\param begin  First table row being read/written
       *\param count  Num consecutive table rows being read/written
       */
    void record_io( unsigned long begin, unsigned long count );
    
      /**\brief Push all data to root process
       *
       * Does nothing if MPI support is not enabled
       */
    void all_reduce();
};


} // namespace moab

#endif
