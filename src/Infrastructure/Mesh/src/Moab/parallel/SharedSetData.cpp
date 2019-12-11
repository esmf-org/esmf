/** \file   SharedSetData.cpp
 *  \author Jason Kraftcheck 
 *  \date   2011-06-23
 */

#include "moab/Interface.hpp"
#include "SharedSetData.hpp"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

namespace moab {

SharedSetData::SharedSetData(Interface& moab, int pcID,  unsigned rank)
  : mb(moab), sharedSetTag(0)
{
  SharedSetTagData zero;

  // Zero out any padding bytes in SharedSetTagData (used for memory alignment)
  // Otherwise, memcmp could lead to unexpected false negatives for comparison
  memset(&zero, 0, sizeof(SharedSetTagData));

  zero.ownerRank = rank;
  zero.ownerHandle = 0;
  zero.sharingProcs = NULL;
  // band-aid: make sure the tag is unique, to not interfere with different pcID
  // maybe a better solution is needed
  // pcID can be at most 64, it ranges from 0 to 63; problems appear at migrate mesh
  std::ostringstream sharedTagUniqueName;
  sharedTagUniqueName << "__sharedSetTag" << pcID;
  ErrorCode rval = mb.tag_get_handle( sharedTagUniqueName.str().c_str(), sizeof(SharedSetTagData), MB_TYPE_OPAQUE,
                                      sharedSetTag, MB_TAG_CREAT|MB_TAG_SPARSE, &zero );
  assert(MB_SUCCESS == rval );
  if (MB_SUCCESS != rval) {
    fprintf(stderr, "Aborted from the constructor of SharedSetData.\n");
    abort();
  }
}

SharedSetData::~SharedSetData()
{
  mb.tag_delete( sharedSetTag );
}

ErrorCode 
SharedSetData::get_owning_procs( std::vector<unsigned>& ranks_out ) const
{
  ranks_out.clear();
  ranks_out.reserve( handleMap.size() );
  for (RHMap::const_iterator i = handleMap.begin(); i != handleMap.end(); ++i)
    ranks_out.push_back( i->first );
  return MB_SUCCESS;
}

ErrorCode 
SharedSetData::get_sharing_procs( EntityHandle entity_set,
                                  std::vector<unsigned>& ranks_out ) const
{
  ErrorCode rval;
  SharedSetTagData data;
  rval = mb.tag_get_data( sharedSetTag, &entity_set, 1, &data );
  if (MB_SUCCESS != rval) return rval;
  
  ranks_out.clear();
  if (data.sharingProcs)
    ranks_out = *data.sharingProcs;
  return MB_SUCCESS;
}

ErrorCode
SharedSetData::get_shared_sets( Range& sets_out ) const
{
//  sets_out.clear();
//  return mb.get_entities_by_type_and_tag( 0, MBENTITYSET, &sharedSetTag, 1, 0, sets_out );

  sets_out.clear();
  for (RHMap::const_iterator i = handleMap.begin(); i != handleMap.end(); ++i)
    append_local_handles( i->second, sets_out );
  return MB_SUCCESS;
}


ErrorCode
SharedSetData::get_shared_sets( unsigned rank, 
                                Range& sets_out ) const
{
  sets_out.clear();
//  if (rank == myRank) {
//    return mb.get_entities_by_type_and_tag( 0, MBENTITYSET, 
//  }
//  else {
    RHMap::const_iterator i = handleMap.find( rank );
    if (i != handleMap.end())
      append_local_handles( i->second, sets_out );
    return MB_SUCCESS;
//  }
}

ErrorCode
SharedSetData::get_owner( EntityHandle entity_set,
                          unsigned& rank_out, 
                          EntityHandle& remote_handle_out ) const
{
  ErrorCode rval;
  SharedSetTagData data;
  rval = mb.tag_get_data( sharedSetTag, &entity_set, 1, &data );
  if (MB_SUCCESS != rval) return rval;
  
  if (!data.ownerHandle) { // not shared
    assert(!data.sharingProcs); // really not shared
    data.ownerHandle = entity_set;
  }
  
  rank_out = data.ownerRank;
  remote_handle_out = data.ownerHandle;
  return MB_SUCCESS;
}
  
ErrorCode
SharedSetData::get_local_handle( unsigned owner_rank,
                                 EntityHandle remote_handle,
                                 EntityHandle& local_handle ) const
{
  RHMap::const_iterator i = handleMap.find( owner_rank );
  assert(i != handleMap.end());
  if (i == handleMap.end()) {
    local_handle = ~(EntityHandle)0;
    return MB_FAILURE;
  }
  
  if (!i->second.find( remote_handle, local_handle )) {
    assert(false);
    local_handle = ~(EntityHandle)0;
    return MB_FAILURE;
  }
  
  return MB_SUCCESS;
}

ErrorCode
SharedSetData::set_owner( EntityHandle set, unsigned owner_rank, EntityHandle owner_handle )
{
  ErrorCode rval;
  SharedSetTagData data;
  rval = mb.tag_get_data( sharedSetTag, &set, 1, &data );
  if (MB_SUCCESS != rval) return rval;
  
  if (data.ownerHandle) {
    RHMap::iterator i = handleMap.find( data.ownerRank );
    if (i != handleMap.end()) {
      i->second.erase( data.ownerHandle, 1 );
    }
  }
  
  data.ownerRank = owner_rank;
  data.ownerHandle = owner_handle;
  rval = mb.tag_set_data( sharedSetTag, &set, 1, &data );
  if (MB_SUCCESS != rval) return rval;
  
  if (!handleMap[owner_rank].insert( owner_handle, set, 1 ).second) {
    assert(false);
    return MB_FAILURE;
  }
  
  return MB_SUCCESS;  
}

ErrorCode
SharedSetData::set_sharing_procs( EntityHandle entity_set,
                                  std::vector<unsigned>& ranks )
{
  std::sort( ranks.begin(), ranks.end() );
  RProcMap::iterator it = procListMap.insert( ranks ).first;
  
  ErrorCode rval;
  SharedSetTagData data;
  rval = mb.tag_get_data( sharedSetTag, &entity_set, 1, &data );
  if (MB_SUCCESS != rval) return rval;
  
  data.sharingProcs = &*it;
  rval = mb.tag_set_data( sharedSetTag, &entity_set, 1, &data );
  if (MB_SUCCESS != rval) return rval;
  
  return MB_SUCCESS;
}

void 
SharedSetData::append_local_handles( const ProcHandleMapType& map,
                                     Range& range )
{
  Range::iterator hint = range.begin();
  for (ProcHandleMapType::const_iterator i = map.begin(); i != map.end(); ++i)
    hint = range.insert( hint, i->value, i->value + i->count-1 );
}


} // namespace moab
