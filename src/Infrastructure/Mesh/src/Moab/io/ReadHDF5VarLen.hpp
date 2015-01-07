/** \file   ReadHDF5VarLen.hpp
 *  \author Jason Kraftcheck 
 *  \date   2010-09-04
 */

#ifndef moab_READ_HDF5_VAR_LEN_HPP
#define moab_READ_HDF5_VAR_LEN_HPP

#ifdef USE_MPI
#  include "moab_mpi.h"
#endif
#include "DebugOutput.hpp"
#include "moab/Range.hpp"
#include "H5Ipublic.h"

namespace moab {

class ReadHDF5Dataset;

/**\brief Read variable-length data from 1-D array dataset
 *
 * Utility class for reading variable-length data from an HDF5 dataset.
 * Used for reading set contents, set parents, set children, 
 * polygon and polyhedron connectivity, and variable-length tag
 * data.
 *
 * This is an abstract class.  The pure virtual \c store_data method
 * must be implemented to create a concrete instance.
 */
class ReadHDF5VarLen {
  protected:
    DebugOutput& dbgOut;
  private:
    void* const dataBuffer;
    const size_t bufferSize;
    
      /**\brief Test if passed file_id is value pointed to by ranged_iter,
       *        and if so, incremenet ranged_iter 
       */
    static bool is_ranged( EntityHandle file_id, 
                           Range::const_iterator& ranged_iter,
                           Range::const_iterator ranged_end );
  protected:
  
      /**\brief Store data list for a single entity
       *
       * The is the pure virtual method that must be provided.
       * It is responsible for storing the data read for a single
       * entity.
       *
       * This function will always be called in the order of the 
       * file_ids in the range passed to the \c read method.
       *
       *\param file_id  The file ID for the entity
       *\param data     A pointer to the data for the entity
       *\param num_data Number of values for the entity
       *\param ranged   For set contents, true if in ranged format.
       */
    virtual ErrorCode store_data( EntityHandle file_id,
                                  void* data,
                                  long num_data,
                                  bool ranged ) = 0;
    
  public:
      /**\brief Constructor
       *\param buffer      A temporary buffer to use during read
       *\param buffer_size Size of \c buffer, in bytes.
       */
    ReadHDF5VarLen( DebugOutput& debug_output,
                    void* buffer,
                    size_t buffer_size )
      : dbgOut( debug_output ),
        dataBuffer( buffer ),
        bufferSize( buffer_size )
    {}
    
    virtual ~ReadHDF5VarLen() {}
    
      /**\brief Do actual read of data set
       *\param data_set         The data set to read.  
       *\param file_ids         The file ids of the entities to read.
       *\param start_file_id    The file id corresponding to the first row of the dataset
       *\param data_type        The desired, in-memory data type for values
       *\param vals_per_ent     The number of values for each entity
       *\param ranged_file_ids  Those file ids for which the 'ranged'
       *                        argument to \c storedata should be passed 
       *                        as \c true.
       */
    ErrorCode read_data( ReadHDF5Dataset& data_set,
                         const Range& offsets,
                         EntityHandle start_offset,
                         hid_t data_type,
                         const Range& file_ids,
                         const std::vector<unsigned>& vals_per_ent,
                         const Range& ranged_file_ids );
    
      /**\brief Read set description table or offset vector for
       *        var-len tags or old-format poly(gon|hedra) connectivity.
       *\param data_set         The data set to read.  
       *\param file_ids         The file ids of the entities to read.
       *\param start_file_id    The file id corresponding to the first row of the dataset
       *\param num_columns      The number of columns of offsets in the dataset
       *\param indices          Array of length \c num_columns contaning the
       *                        indices of the columns to read.
       *\param nudge            Amount by which to offset values in 
       *                        \c offset_out to avoid putting zeros in
       *                        Range.  Must be greater than 0.  Probably 1.
       *\param offsets_out      An array of length \c num_columns which will
       *                        be populated with the resulting list of offsets
       *                        into the contents list calculated from reading
       *                        the offsets from the passed data set.
       *\param counts_out       An array of length \c num_columns of std::vectors,
       *                        where each vector will be filled with one value
       *                        per file ID indicating the length of the data for
       *                        the corresponding file ID.
       *\param ranged_file_ids  If non-null, the last column of the table will
       *                        be read and tested for the ranged bit.  For
       *                        all file_ids for which the range bit is set,
       *                        the file ID will be added to this list.
       */
 /*
    ErrorCode read_offsets( ReadHDF5Dataset& data_set,
                            const Range& file_ids,
                            EntityHandle start_file_id,
                            unsigned num_columns,
                            const unsigned indices[],
                            EntityHandle nudge,
                            Range offsets_out[],
                            std::vector<unsigned> counts_out[],
                            Range* ranged_file_ids = 0 );
*/
    ErrorCode read_offsets( ReadHDF5Dataset& data_set,
                            const Range& file_ids,
                            EntityHandle start_file_id,
                            EntityHandle nudge,
                            Range& offsets_out,
                            std::vector<unsigned>& counts_out );
                            
    ErrorCode read( ReadHDF5Dataset& offset_data,
                    ReadHDF5Dataset& value_data,
                    const Range& file_ids,
                    EntityHandle start_file_id,
                    hid_t data_type,
                    const Range* ranged = 0 )
    {
      ErrorCode rval;
      const EntityHandle nudge = 1;
      Range offsets;
      std::vector<unsigned> counts;
      rval = read_offsets( offset_data, file_ids, 
                           start_file_id, nudge,
                           offsets, counts );
      if (MB_SUCCESS != rval)
        return rval;
      Range empty;
      rval = read_data( value_data, 
                        offsets, nudge, data_type, 
                        file_ids, counts, ranged ? *ranged : empty );
      return rval;
    }
};

} // namespace moab

#endif // moab_READ_HDF5_VAR_LEN_HPP
