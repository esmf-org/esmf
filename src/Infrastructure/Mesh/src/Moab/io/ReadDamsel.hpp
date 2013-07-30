//-------------------------------------------------------------------------
// Filename      : ReadDamsel.hpp
//
// Purpose       : Damsel file reader
//
// Creator       : Tim Tautges
//-------------------------------------------------------------------------

#ifndef READDAMSEL_HPP
#define READDAMSEL_HPP

#ifndef IS_BUILDING_MB
//#error "ReadDamsel.hpp isn't supposed to be included into an application"
#endif

#include <vector>
#include <map>
#include <string>

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/Range.hpp"
#include "moab/RangeMap.hpp"
#include "DebugOutput.hpp"
#include "DamselUtil.hpp"

#include "damsel.h"

namespace moab {

class ReadUtilIface;
class ParallelComm;
class Error;

class ReadDamsel : public ReaderIface
{
   
public:
  
  static ReaderIface* factory( Interface* );
  
    //! load an NC file
  ErrorCode load_file( const char* file_name,
                       const EntityHandle* file_set,
                       const FileOptions& opts,
                       const SubsetList* subset_list = 0,
                       const Tag* file_id_tag = 0 );

   //! Constructor
   ReadDamsel(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadDamsel();

  virtual ErrorCode read_tag_values( const char* file_name,
                                     const char* tag_name,
                                     const FileOptions& opts,
                                     std::vector<int>& tag_values_out,
                                     const SubsetList* subset_list = 0 );

private:

  //! get contents of the container (containing file-side handles) and translate to moab-side handles
  ErrorCode get_contents(damsel_model m, damsel_container c, Range &ents);

  //! get contents of the container (containing file-side handles) and translate to moab-side handles
  //! ents argument should point to already-allocated space
  ErrorCode get_contents(damsel_model m, damsel_container c, EntityHandle *ents);

  ErrorCode init();

  ErrorCode parse_options(const FileOptions &opts,
                          bool &parallel);
  
  ErrorCode process_tags(std::vector<damsel_tag_buf_type> &tag_infos);
  
  ErrorCode process_ent_info(const damsel_entity_buf_type &einfo);
  
  ErrorCode process_entity_tags(int count, damsel_container tag_container, 
                                damsel_container app_cont, Range &these_ents);
  
  ErrorCode process_coll_infos(std::vector<damsel_collection_buf_type> &coll_infos);

    //! convert handles in a container into handle pairs, one pair per contiguous sequence of handles in the container
  ErrorCode container_to_handle_pairs(damsel_container &cont, std::vector<damsel_handle> &handle_pairs);

    //! store MOAB handles starting from start_handle, corresponding to store handles in handle_pairs, into the
    //! entity map
  ErrorCode insert_into_map(std::vector<damsel_handle> &handle_pairs, EntityHandle start_handle);
  
  class subrange
  {
   public:
    subrange(damsel_handle ch, EntityHandle s, int c) : collh(ch), seth(s), count(c) {}
    damsel_handle collh;
    EntityHandle seth;
    int count;
  };

//------------member variables ------------//

    //! interface instance
  Interface* mbImpl;

    //! utilIface
  ReadUtilIface *readMeshIface;
  
    //! Error object used to register errors
  Error *mError;
  
    //! file name
  std::string fileName;

    //! whether this reader natively supports parallel semantics
  bool nativeParallel;

    //! parallel info
  ParallelComm *myPcomm;
  
    //! Used to track file handles
  Tag mGlobalIdTag;
  
  //! map from damsel to moab handles
  RangeMap<damsel_handle, EntityHandle, 0> dmHandleRMap;
  
    //! keep variuos damsel data
  DamselUtil dU;
};

inline const bool operator==(const damsel_err_t &lhs, const damsel_err_t &rhs) 
{
  return lhs.id == rhs.id;
}
  
} // namespace moab

#endif
