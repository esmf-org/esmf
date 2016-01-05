#ifndef READ_SMS_HPP
#define READ_SMS_HPP

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/Range.hpp"
#include <vector>

namespace moab {

class ReadUtilIface;

// Base class for binary and ASCII readers
class ReadSms : public ReaderIface
{
   
public:

    //! factory method 
  static ReaderIface* factory( Interface* );

  ErrorCode load_file( const char* file_name,
                       const EntityHandle* file_set,
                       const FileOptions& opts,
                       const SubsetList* subset_list = 0,
                       const Tag* file_id_tag = 0 );

  ErrorCode read_tag_values( const char* file_name,
                             const char* tag_name,
                             const FileOptions& opts,
                             std::vector<int>& tag_values_out,
                             const SubsetList* subset_list = 0 );
  
    //! Constructor
  ReadSms(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadSms();

private:

  ErrorCode add_entities( EntityHandle start, 
                            EntityHandle count,
                            const Tag* file_id_tag );

  ErrorCode load_file_impl( FILE* file, const Tag* file_id_tag );
  
  ErrorCode get_set(std::vector<EntityHandle> *sets,
                      int set_type, int set_id,
                      Tag set_tag,
                      EntityHandle &this_set,
                      const Tag* file_id_tag );

  ErrorCode read_parallel_info(FILE *file_ptr);

  ReadUtilIface* readMeshIface;

    //! interface instance
  Interface* mdbImpl;
  
  Tag globalId, paramCoords, geomDimension;
  
  int setId;
};

} // namespace moab

#endif
