
#ifndef MOAB_PARALLEL_DATA_HPP
#define MOAB_PARALLEL_DATA_HPP

#include "moab/Forward.hpp"
#include "moab/Range.hpp"

namespace moab {

class ParallelComm;

/**
 * \brief Parallel data in MOAB
 * \author Tim Tautges
 *
 *  This class implements methods to retrieve information about 
 * the parallel mesh from MOAB.  Most of this data can be retrieved
 * directly from MOAB as sets and tags; this class provides convenience
 * methods implemented on top of other MOAB functions.
 *
 */
class ParallelData
{
public:

    //! constructor; if non-null parallelcomm, that is used to
    //! determine rank, otherwise rank is taken from impl
  ParallelData(Interface *impl, ParallelComm *pcomm = NULL);

    //! return partition sets; if tag_name is input, gets sets with
    //! that tag name, otherwise uses PARALLEL_PARTITION tag
  ErrorCode get_partition_sets(Range &part_sets,
                                 const char *tag_name = NULL);

    //! get communication interface sets and the processors with which
    //! this processor communicates; sets are sorted by processor
  ErrorCode get_interface_sets(std::vector<EntityHandle> &iface_sets,
                                 std::vector<int> &iface_procs);
  

private:

    //! interface instance to which this instance corresponds
  Interface *mbImpl;

    //! ParallelComm object to which this is bound
  ParallelComm *parallelComm;
  
};

inline ParallelData::ParallelData(Interface *impl, 
                                      ParallelComm *pcomm) 
    : mbImpl(impl), parallelComm(pcomm) 
{}

}

#endif
