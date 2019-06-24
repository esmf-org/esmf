#ifndef READ_PARALLEL_HPP
#define READ_PARALLEL_HPP

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"
#include "DebugOutput.hpp"

#include <string>

namespace moab {

class ReadUtilIface;
class ParallelComm;
class Error;

class ReadParallel
{
   
public:

  static ReaderIface* factory( Interface* );

    //! load a file
  ErrorCode load_file(const char *file_name,
                        const EntityHandle* file_set,
                        const FileOptions &opts,
                        const ReaderIface::SubsetList* subset_list = 0,
                        const Tag* file_id_tag = 0 );
  
    //! load multiple files
  ErrorCode load_file(const char **file_names,
                        const int num_files,
                        const EntityHandle* file_set,
                        const FileOptions &opts,
                        const ReaderIface::SubsetList* subset_list = 0,
                        const Tag* file_id_tag = 0 );
  
  ErrorCode load_file(const char **file_names,
                        const int num_files,
                        const EntityHandle* file_set,
                        int parallel_mode, 
                        std::string &partition_tag_name, 
                        std::vector<int> &partition_tag_vals, 
                        bool distrib,
                        bool partition_by_rank,
                        std::vector<int> &pa_vec,
                        const FileOptions &opts,
                        const ReaderIface::SubsetList* subset_list,
                        const Tag* file_id_tag,
                        const int reader_rank,
                        const bool cputime,
                        const int resolve_dim,
                        const int shared_dim,
                        const int ghost_dim,
                        const int bridge_dim,
                      const int num_layers,
                      const int addl_ents);
    //! Constructor
  ReadParallel(Interface* impl = NULL, ParallelComm *pc = NULL);

   //! Destructor
  virtual ~ReadParallel() {}

  static const char *parallelOptsNames[];
  
  enum ParallelActions {PA_READ=0, 
                        PA_READ_PART=1,
                        PA_BROADCAST=2,
                        PA_DELETE_NONLOCAL=3,
                        PA_CHECK_GIDS_SERIAL=4,
                        PA_GET_FILESET_ENTS=5,
                        PA_RESOLVE_SHARED_ENTS=6,
                        PA_EXCHANGE_GHOSTS=7,
                        PA_RESOLVE_SHARED_SETS=8,
                        PA_AUGMENT_SETS_WITH_GHOSTS=9,
                        PA_PRINT_PARALLEL=10,
                        PA_CREATE_TRIVIAL_PARTITION=11
                       };

  static const char *ParallelActionsNames[];
  
  enum ParallelOpts { POPT_NONE=0, 
                      POPT_BCAST, 
                      POPT_BCAST_DELETE, 
                      POPT_READ_DELETE, 
                      POPT_READ_PART, 
                      POPT_DEFAULT};

    //! PUBLIC TO ALLOW TESTING
  ErrorCode delete_nonlocal_entities(std::string &ptag_name,
                                       std::vector<int> &ptag_vals,
                                       bool distribute,
                                       EntityHandle file_set);
  
  ErrorCode delete_nonlocal_entities(EntityHandle file_set);

protected:
  ErrorCode create_partition_sets( std::string &ptag_name,
                                     EntityHandle file_set );

private:

  Interface *mbImpl;

    // each reader can keep track of its own pcomm
  ParallelComm *myPcomm;
  
  DebugOutput myDebug;

  Error *mError;
};

inline ErrorCode ReadParallel::load_file(const char *file_name,
                                         const EntityHandle* file_set,
                                         const FileOptions &opts,
                                         const ReaderIface::SubsetList* subset_list,
                                         const Tag* file_id_tag )
{
  return load_file(&file_name, 1, file_set, opts, 
                   subset_list, file_id_tag);
}

} // namespace moab

#endif
