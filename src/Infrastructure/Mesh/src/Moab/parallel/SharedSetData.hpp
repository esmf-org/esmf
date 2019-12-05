/** \file   SharedSetData.hpp
 *  \author Jason Kraftcheck 
 *  \date   2011-06-23
 */

#ifndef moab_SHARED_SET_DATA_HPP
#define moab_SHARED_SET_DATA_HPP

#include "moab/Types.hpp"
#include "moab/RangeMap.hpp"

#define STRINGIFY_(X) #X
#define STRINGIFY(X) STRINGIFY_(X)
#ifdef MOAB_HAVE_UNORDERED_MAP
# include STRINGIFY(MOAB_HAVE_UNORDERED_MAP)
# include STRINGIFY(MOAB_HAVE_UNORDERED_SET)
#else
# include <map>
# include <set>
#endif

#include <vector>

namespace moab {

class Interface;

/**\brief ParallelComm data about shared entity sets */
class SharedSetData
{
public:
  SharedSetData(Interface& moab, int pcID,  unsigned rank);
  
  ~SharedSetData();
  
    /**\brief Get ranks of sharing procs 
     *
     * Get list of all process ranks that own at least one set that
     * is shared with this process.
     */
  ErrorCode get_owning_procs( std::vector<unsigned>& ranks_out ) const;
  
    /**\brief Get ranks of sharing procs 
     *
     * Get list of all process ranks with which this process the passed set.
     * Returns an empty list for non-shared sets.
     */
  ErrorCode get_sharing_procs( EntityHandle entity_set,
                               std::vector<unsigned>& ranks_out ) const;
  
    /**\brief Get handles for all shared sets */
  ErrorCode get_shared_sets( Range& sets_out ) const;
                               
    /**\brief Get handles for all sets shared with specified process */
  ErrorCode get_shared_sets( unsigned rank, Range& sets_out ) const;

    /**\brief Get owner and owner's handle for shared set */ 
  ErrorCode get_owner( EntityHandle set,
                       unsigned& rank_out, 
                       EntityHandle& remote_handle_out ) const;
  
    /**\brief Get owner of shared set */ 
  ErrorCode get_owner( EntityHandle set, unsigned& rank_out ) const
    { EntityHandle h; return get_owner( set, rank_out, h ); }
  
    /**\brief Get owner's handle for shared set */ 
  ErrorCode get_owner_handle( EntityHandle set, EntityHandle& handle_out ) const
    { unsigned rank; return get_owner( set, rank, handle_out); } 
  
    /**\brief Get local handle for shared set */
  ErrorCode get_local_handle( unsigned owner_rank,
                              EntityHandle remote_handle,
                              EntityHandle& local_handle_out ) const;
  
  ErrorCode set_owner( EntityHandle set, unsigned owner_rank, EntityHandle owner_handle );
  
    /**\brief set/update sharing list for a set
     *
     *\NOTE sorts \c ranks vector
     */
  ErrorCode set_sharing_procs( EntityHandle set_handle,
                               std::vector<unsigned>& ranks );

private:
  
  Interface& mb;

  /**\brief per-set tag data */
  struct SharedSetTagData 
  { 
    unsigned ownerRank; 
    EntityHandle ownerHandle; 
    const std::vector<unsigned>* sharingProcs;
  };

    /** Shared set data: opaque tag containing struct SharedSetTagData */
  Tag sharedSetTag;
 
    /** Map type for lookup of local handle given remote handle */
  typedef RangeMap<EntityHandle,EntityHandle> ProcHandleMapType;
  
  static void append_local_handles( const ProcHandleMapType& map,
                                    Range& append_to_this );


    /** Map type for lookup of ProcHandleMapType instance by rank */
#ifdef MOAB_HAVE_UNORDERED_MAP
  struct hash_vect {
      // Copied (more or less) from Boost
    template <typename T> static void hash_combine( size_t& seed, T val )
      { seed ^= MOAB_UNORDERED_MAP_NS::hash<T>().operator()(val) + 0x9e3779b9 + (seed << 6) + (seed >> 2); }
    template <typename IT> static size_t hash_range( IT it, IT last )
      { size_t seed = 0; for (; it != last; ++it) hash_combine( seed, *it ); return seed; }
    size_t operator()(const std::vector<unsigned>& v) const
      { return hash_range( v.begin(), v.end() ); }
  };

  typedef MOAB_UNORDERED_MAP_NS::unordered_map<unsigned,ProcHandleMapType> RHMap;
  typedef MOAB_UNORDERED_MAP_NS::unordered_set<std::vector<unsigned>,hash_vect> RProcMap;
#else
  struct less_vect {
    bool operator()( const std::vector<unsigned>& a, const std::vector<unsigned>& b ) const
      {
          // sort by size first
        if (a.size() != b.size())
          return a.size() < b.size();
          // if same size, sort by first non-equal value
        size_t i = 0;
        while (i != a.size() && a[i] == b[i]) ++i;
        return i != a.size() && a[i] < b[i];
      }
  };

  typedef std::map<unsigned,ProcHandleMapType> RHMap;
  typedef std::set<std::vector<unsigned>,less_vect> RProcMap;
#endif
  
    /** Map for lookup of ProcHandleMapType instance by rank */
  RHMap handleMap;

    /** Storage of sharing lists */
  RProcMap procListMap;
};


} // namespace moab

#endif // moab_SHARED_SET_DATA_HPP
