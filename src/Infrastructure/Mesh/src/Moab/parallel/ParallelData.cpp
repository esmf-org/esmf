#include "moab/ParallelData.hpp"
#include "moab/ParallelComm.hpp"
#include "MBParallelConventions.h"
#include "moab/Interface.hpp"

#include <map>

namespace moab {

    //! return partition sets; if tag_name is input, gets sets with
    //! that tag name, otherwise uses PARALLEL_PARTITION tag
ErrorCode ParallelData::get_partition_sets(Range &part_sets,
                                               const char *tag_name) 
{
  Tag part_tag = 0;
  ErrorCode result;
  
  if (NULL != tag_name) 
    result = mbImpl->tag_get_handle(tag_name, 1, MB_TYPE_INTEGER, part_tag);
  else
    result = mbImpl->tag_get_handle(PARALLEL_PARTITION_TAG_NAME, 1, MB_TYPE_INTEGER, part_tag);
    
  if (MB_SUCCESS != result) return result;
  else if (0 == part_tag) return MB_TAG_NOT_FOUND;
  
  result = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &part_tag, 
                                                NULL, 1, part_sets,
                                                Interface::UNION);
  return result;
}
  

    //! get communication interface sets and the processors with which
    //! this processor communicates; sets are sorted by processor
ErrorCode ParallelData::get_interface_sets(std::vector<EntityHandle> &iface_sets,
                                               std::vector<int> &iface_procs) 
{
#define CONTINUE {result = tmp_result; continue;}
  iface_sets.clear();
  iface_procs.clear();
  
  Tag proc_tag = 0, procs_tag = 0;
  ErrorCode result = MB_SUCCESS;
  int my_rank;
  if (parallelComm) 
    my_rank = parallelComm->proc_config().proc_rank();
  else 
    return MB_FAILURE;

  std::multimap<int, EntityHandle> iface_data;

  for (int i = 0; i < 2; i++) {
    ErrorCode tmp_result;
    
    if (0 == i)
      tmp_result = mbImpl->tag_get_handle(PARALLEL_SHARED_PROC_TAG_NAME, 
                                      1, MB_TYPE_INTEGER, proc_tag);
    else
      tmp_result = mbImpl->tag_get_handle(PARALLEL_SHARED_PROCS_TAG_NAME, 
                                      MAX_SHARING_PROCS, MB_TYPE_INTEGER, proc_tag);
    if (MB_SUCCESS != tmp_result) CONTINUE;

    int tsize;
    tmp_result = mbImpl->tag_get_length(proc_tag, tsize);
    if (0 == tsize || MB_SUCCESS != tmp_result) CONTINUE;
    
    Range proc_sets;
    tmp_result = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, 
                                                  &proc_tag, NULL, 1,
                                                  proc_sets, Interface::UNION);
    if (MB_SUCCESS != tmp_result) CONTINUE;
    
    if (proc_sets.empty()) CONTINUE;
      
    std::vector<int> proc_tags(proc_sets.size()*tsize);
    tmp_result = mbImpl->tag_get_data(procs_tag, proc_sets, &proc_tags[0]);
    if (MB_SUCCESS != tmp_result) CONTINUE;
    int k;
    Range::iterator rit;
    
    for (k = 0, rit = proc_sets.begin(); rit != proc_sets.end(); rit++, k++) {
      for (int j = 0; j < tsize; j++) {
        if (my_rank != proc_tags[2*k+j] && proc_tags[2*k+j] >= 0)
          iface_data.insert(std::pair<int,EntityHandle>(proc_tags[2*k+j], *rit));
      }
    }
  }

    // now get the results in sorted order
  std::multimap<int,EntityHandle>::iterator mit;
  for (mit = iface_data.begin(); mit != iface_data.end(); mit++)
    iface_procs.push_back((*mit).first),
      iface_sets.push_back((*mit).second);
    
  return result;
}


} // namespace moab

