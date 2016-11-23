/** \file   ReorderTool.cpp
 *  \author Jason Kraftcheck 
 *  \date   2011-05-23
 */

#include "moab/ReorderTool.hpp"
#include "moab/Core.hpp"
#include "moab/Range.hpp"

#include "SequenceManager.hpp"
#include "TypeSequenceManager.hpp"
#include "EntitySequence.hpp"

#include <algorithm>
#include <numeric>
#include <set>
#include <iostream>

namespace moab {

// no-op function as a convenient spot to set a breakpoint
static inline ErrorCode error(ErrorCode val)
  { return val; }

#define CHKERR \
  if (MB_SUCCESS != rval) \
    return error(rval)

#define UNRECOVERABLE(ERRCODE) do { if (MB_SUCCESS != (ERRCODE)) { \
  error((ERRCODE)); \
  std::cerr << "Unreconverable error during mesh reorder." << std::endl \
            << "Error Code " << (ERRCODE) << " at " \
            << __FILE__ << ":" << __LINE__ << std::endl; \
  std::cerr.flush(); \
  abort(); \
} } while(false)

static ErrorCode check_tag_type( Interface* moab,
                                 Tag tag,
                                 DataType exp_type,
                                 int exp_size )
{
  ErrorCode rval;
  DataType act_type;
  int act_size;

  rval = moab->tag_get_data_type( tag, act_type );
  CHKERR;
  
  rval = moab->tag_get_bytes( tag, act_size );
  CHKERR;
  
  if (act_type != exp_type || act_size != exp_size)
    return MB_TYPE_OUT_OF_RANGE;

  return MB_SUCCESS;
}

ErrorCode ReorderTool::handle_order_from_int_tag( Tag tag,
                                                  int skip_value,
                                                  Tag& new_handles )
{
  ErrorCode rval;
 
    // check that input tag handles are what we expect
  rval = check_tag_type( mMB, tag, MB_TYPE_INTEGER, sizeof(int) );
  CHKERR;
  EntityHandle zero = 0;
  rval = mMB->tag_get_handle( 0, 1, MB_TYPE_HANDLE, new_handles,
                              MB_TAG_DENSE|MB_TAG_CREAT|MB_TAG_EXCL, &zero );
  CHKERR;

    // Can only reorder within same type/connectivity (or vertex/num_coords)
    // groupings.  Call helper function for each such grouping.  
  for (EntityType t = MBVERTEX; t < MBENTITYSET; ++t) {
      // Get list of all connectivity lengths (or vertex dimensions)
      // that exist for type t.
    TypeSequenceManager& seqs = mMB->sequence_manager()->entity_map(t);
    TypeSequenceManager::iterator i;
    std::set<int> values;
    for (i = seqs.begin(); i != seqs.end(); ++i) {
      EntitySequence* seq = *i;
        // 0 values per entity implies structured data, which
        // we cannot reorder.  Skip those.
      if (t == MBVERTEX || 0 < seq->values_per_entity())
        values.insert( seq->values_per_entity() );
    }
  
      // Call helper function for each (type,size) tuple.
    std::set<int>::iterator j;
    for (j = values.begin(); j != values.end(); ++j) {
      rval = handle_order_from_int_tag( t, *j, tag, skip_value, new_handles );
      if (MB_SUCCESS != rval) {
        mMB->tag_delete( new_handles );
        return error(rval);
      }
    }
  }
  
  return MB_SUCCESS;
}

void ReorderTool::get_entities( EntityType t, int vals_per_ent, Range& entities )
{
  Range::iterator hint = entities.begin();;
  TypeSequenceManager& seqs = mMB->sequence_manager()->entity_map(t);
  TypeSequenceManager::iterator s;
  for (s = seqs.begin(); s != seqs.end(); ++s) {
    EntitySequence* seq = *s;
    if (seq->values_per_entity() == vals_per_ent)
      hint = entities.insert( hint, seq->start_handle(), seq->end_handle() );
  }
}

ErrorCode ReorderTool::handle_order_from_int_tag( EntityType t,
                                                  int vals_per_ent,
                                                  Tag tag,
                                                  int skip_value,
                                                  Tag new_handles )
{
  ErrorCode rval;
 
    // check that input tag handles are what we expect
  rval = check_tag_type( mMB, tag, MB_TYPE_INTEGER, sizeof(int) );
  CHKERR;
  rval = check_tag_type( mMB, new_handles, MB_TYPE_HANDLE, sizeof(EntityHandle) );
  CHKERR;

    // get entities to re-order
  Range entities;
  get_entities( t, vals_per_ent, entities );

    // get entity order values
  std::vector<int> sortvals(entities.size());
  rval = mMB->tag_get_data( tag, entities, &sortvals[0] );
  CHKERR;
  
    // remove any entities for which value is skip_value
  size_t r = 0, w = 0;
  for (Range::iterator i = entities.begin(); i != entities.end(); ++r) {
    if (sortvals[r] == skip_value)
      i = entities.erase(i);
    else {
      sortvals[w++] = sortvals[r];
      ++i;
    }
  }
  sortvals.resize(w);
  
    // sort 
  std::sort( sortvals.begin(), sortvals.end() );
    // Convert to unique list and offsets for each value
    // When done, sortvals will contain unique, sortvals and
    // offsets will contain, for each unique value in sortvals,
    // the number of values that occured in the non-unique list
    // before the first instance of that value.
  std::vector<size_t> offsets;
  offsets.push_back( 0 );
  offsets.push_back( 1 );
  for (w = 0, r = 1; r < sortvals.size(); ++r) {
    if (sortvals[r] == sortvals[w]) {
      ++offsets.back();
    }
    else {
      ++w;
      sortvals[w] = sortvals[r];
      offsets.push_back(offsets.back()+1);
    }
  }
  ++w;
  assert(w+1 == offsets.size());
  sortvals.resize(w);
  
    // Tag each entity with its new handle
  for (Range::iterator i = entities.begin(); i != entities.end(); ++i) {
    int val;
    rval = mMB->tag_get_data( tag, &*i, 1, &val );
    CHKERR;
    w = std::lower_bound( sortvals.begin(), sortvals.end(), val ) - sortvals.begin();
    assert( w < sortvals.size() );
    size_t offset = offsets[w];
    ++offsets[w];
      // should maybe copy range into array to avoid possibly n^2 behavior here
    EntityHandle h = *(entities.begin() + offset);
    rval = mMB->tag_set_data( new_handles, &*i, 1, &h );
    CHKERR;
  }

  return MB_SUCCESS;  
}
    
    
    

ErrorCode
ReorderTool::handle_order_from_sets_and_adj( const Range& sets,
                                             Tag& handle_tag )
{
  ErrorCode rval;
  
  if (!sets.all_of_type(MBENTITYSET))
    return MB_TYPE_OUT_OF_RANGE;
  
  Tag order_tag;
  const int negone = -1;
  rval = mMB->tag_get_handle( 0, 1, MB_TYPE_INTEGER, order_tag,
                              MB_TAG_DENSE|MB_TAG_CREAT|MB_TAG_EXCL, &negone );
  if (MB_SUCCESS != rval) {
    mMB->tag_delete( handle_tag );
    handle_tag = 0;
    return error(rval);
  }
  
  std::vector<std::vector<EntityHandle>*> data;
  rval = int_order_from_sets_and_adj( sets, order_tag, negone, data );
  for (size_t i = 0; i < data.size(); ++i)
    delete data[i];
  if (MB_SUCCESS != rval) {
    mMB->tag_delete( order_tag );
    return error(rval);
  }
  
  rval = handle_order_from_int_tag( order_tag, negone, handle_tag );
  if (MB_SUCCESS != rval) {
    mMB->tag_delete( order_tag );
    return error(rval);
  }
  
  rval = mMB->tag_delete( order_tag );
  if (MB_SUCCESS != rval)
    return error(rval);
  
  return MB_SUCCESS;
}
  
    // Compare function to use for a map keyed on pointers to sorted vectors
struct CompSortedVect {
  bool operator()( const std::vector<EntityHandle>* v1,
                   const std::vector<EntityHandle>* v2 ) const
  {
    std::vector<EntityHandle>::const_iterator i1, i2;
    for (i1 = v1->begin(), i2 = v2->begin(); i1 != v1->end(); ++i1, ++i2) {
      if (i2 == v2->end() || *i1 > *i2)
        return false;
      else if (*i1 < *i2)
        return true;
    }
    return i2 != v2->end();
  }
};

ErrorCode ReorderTool::int_order_from_sets_and_adj( const Range& sets,
                                                    Tag order_tag,
                                                    int skip_val,
                  std::vector<std::vector<EntityHandle>*>& revMap )
{
  ErrorCode rval;

  if (!sets.all_of_type(MBENTITYSET))
    return MB_TYPE_OUT_OF_RANGE;
  
  rval = check_tag_type( mMB, order_tag, MB_TYPE_INTEGER, sizeof(int) );
  CHKERR;
  
    // Compare function to use for a map keyed on pointers to sorted vectors
  CompSortedVect lessthan;
    // Map from sorted list of handles to index
  std::map< std::vector<EntityHandle>*, int, CompSortedVect > forMap;
  std::map< std::vector<EntityHandle>*, int, CompSortedVect >::iterator fiter, fiter2;
  std::vector<EntityHandle> sharing; // tmp storage for entity
  
    // for each set
  for (Range::iterator s = sets.begin(); s != sets.end(); ++s) {

      // gather up all entities and adjacencies
    Range tmp, ents, adj[4]; // indexed by dimension
    for (int dim = 0; dim < 4; ++dim) {
      rval = mMB->get_entities_by_dimension( *s, dim, tmp );
      CHKERR;
      for (int ldim = 0; ldim < dim; ++ldim) {
        rval = mMB->get_adjacencies( tmp, ldim, false, adj[ldim], Interface::UNION );
        CHKERR;
      }
      for (int udim = dim+1; udim <= 3; ++udim) {
        rval = mMB->get_adjacencies( tmp, udim, false, adj[udim], Interface::UNION );
        CHKERR;
      }
      ents.merge(tmp);
      tmp.clear();
    }
    for (int dim = 0; dim < 4; ++dim) {
      ents.merge( adj[dim] );
      adj[dim].clear();
    }

      // process each entity
    for (Range::iterator e = ents.begin(); e != ents.end(); ++e) {
      int val;
      rval = mMB->tag_get_data( order_tag, &*e, 1, &val );
      CHKERR;
      
        // If this entity is already in one or more of the sets (either
        // directly or through adjacency) then get the existing list of
        // sets and append this set handle (we are iterating over sets
        // in sorted order, so appending should aways maintain a sorted
        // list.)
      sharing.clear();
      if (val != skip_val) {
        sharing = *revMap[val];
        assert( std::lower_bound(sharing.begin(), sharing.end(), *s) == sharing.end() );
      }
      sharing.push_back( *s );
      
        // Check if new sharing list already exists in forward map
      fiter = forMap.lower_bound( &sharing );
      if (fiter == forMap.end() || lessthan(fiter->first,&sharing)) {
          // Add new sharing list to forward and reverse maps.
        std::vector<EntityHandle>* newvec = new std::vector<EntityHandle>;
        newvec->swap(sharing);
        if ((int)revMap.size() == skip_val)
          revMap.push_back(0);
        fiter2 = forMap.insert( fiter, std::pair<std::vector<EntityHandle>*, int>(newvec,revMap.size()) );
        assert(fiter2 != fiter);
        fiter = fiter2;
        revMap.push_back( newvec );
      }
      
        // Update index on entity
      val = fiter->second;
      rval = mMB->tag_set_data( order_tag, &*e, 1, &val );
      CHKERR;
    }
  }
  
  return MB_SUCCESS;
}

ErrorCode 
ReorderTool::get_reordered_handles( Tag tag, 
                                    const Range& old_handles, 
                                    std::vector<EntityHandle>& new_handles )
{
  new_handles.resize( old_handles.size() );
  ErrorCode rval = mMB->tag_get_data( tag, old_handles, (new_handles.empty())?NULL:&new_handles[0] );
  CHKERR;
  
  Range::const_iterator it1 = old_handles.begin();
  std::vector<EntityHandle>::iterator it2 = new_handles.begin();
  for (;it1 != old_handles.end(); ++it1, ++it2) 
    if (0 == *it2)
      *it2 = *it1;
  
  return MB_SUCCESS;
}

ErrorCode ReorderTool::get_reordered_handles( Tag tag, 
                        const std::vector<EntityHandle>& old_handles,
                        std::vector<EntityHandle>& new_handles )
{
  new_handles.resize( old_handles.size() );
  return get_reordered_handles( tag, &old_handles[0], &new_handles[0], old_handles.size() );
}

ErrorCode ReorderTool::get_reordered_handles( Tag tag, 
                                        const EntityHandle* old_handles,
                                        EntityHandle* new_handles,
                                        size_t num_handles )
{
  ErrorCode rval = mMB->tag_get_data( tag, old_handles, num_handles, new_handles );
  CHKERR;
  
  for (size_t i = 0; i < num_handles; ++i)
    if (0 == new_handles[i])
      new_handles[i] = old_handles[i];
  
  return MB_SUCCESS;
}
  
ErrorCode ReorderTool::get_new_handles( Tag tag,
                                        Range& old_handles,
                                        std::vector<EntityHandle>& newhandles )
{
    // get new handles for tagged entities
  newhandles.resize(old_handles.size());
  ErrorCode rval = mMB->tag_get_data( tag, old_handles, (newhandles.empty())?NULL:&newhandles[0] );
  CHKERR;

    // remove entities that were not reordered
  Range::iterator i = old_handles.begin();
  size_t w = 0;
  for (size_t r = 0; r < newhandles.size(); ++r) {
    if (0 != newhandles[r]) {
      newhandles[w] = newhandles[r];
      ++w;
      ++i;
    }
    else {
      i = old_handles.erase(i);
    }
  }
  newhandles.resize(w);
  assert(newhandles.size() == old_handles.size());
  return MB_SUCCESS;
}

ErrorCode ReorderTool::reorder_entities( Tag new_handles )
{
  ErrorCode rval;

  rval = check_tag_type( mMB, new_handles, MB_TYPE_HANDLE, sizeof(EntityHandle) );
  CHKERR;
  EntityHandle defval;
  rval = mMB->tag_get_default_value( new_handles, &defval );
  CHKERR;
  if (0 != defval)
    return error(MB_INDEX_OUT_OF_RANGE);

    // Can only reorder within same type/connectivity (or vertex/num_coords)
    // groupings.  
  for (EntityType t = MBVERTEX; t < MBENTITYSET; ++t) {
      // Get list of all connectivity lengths (or vertex dimensions)
      // that exist for type t.
    TypeSequenceManager& seqs = mMB->sequence_manager()->entity_map(t);
    TypeSequenceManager::iterator i;
    std::set<int> values;
    for (i = seqs.begin(); i != seqs.end(); ++i) {
      EntitySequence* seq = *i;
        // 0 values per entity implies structured data, which
        // we cannot reorder.  Skip those.
      if (t == MBVERTEX || 0 < seq->values_per_entity())
        values.insert( seq->values_per_entity() );
    }
  
      // reorder primary data for each (type,size) tuple.
    std::set<int>::iterator j;
    for (j = values.begin(); j != values.end(); ++j) {
      Range entities;
      get_entities( t, *j, entities );
      std::vector<EntityHandle> handles;
      rval = get_reordered_handles( new_handles, entities, handles );
      UNRECOVERABLE(rval);
      
      if (t == MBVERTEX) {
        std::vector<double> coords( entities.size() * 3 );
        rval = mMB->get_coords( entities, &coords[0] );
        UNRECOVERABLE(rval);
        rval = mMB->set_coords( &handles[0], handles.size(), &coords[0] );
        UNRECOVERABLE(rval);
      }
      else {
        std::vector<EntityHandle> conn;
        conn.reserve( entities.size() * *j );
        std::vector<EntityHandle> old_handles;
        old_handles.resize( entities.size() );
        std::copy( entities.begin(), entities.end(), old_handles.begin() );
        rval = mMB->get_connectivity( &old_handles[0], old_handles.size(),
                                      conn, false );
        UNRECOVERABLE(rval);
        old_handles.clear();
        old_handles = conn;
        rval = get_reordered_handles( new_handles, old_handles, conn );
        UNRECOVERABLE(rval);
        for (unsigned int h = 0; h < handles.size(); ++h) {
          rval = mMB->set_connectivity( handles[h], &conn[h * *j], *j );
          UNRECOVERABLE(rval);
        }
      }
    }
  }
  
    // now update tag data
  std::vector<Tag> tag_handles;
  mMB->tag_get_tags( tag_handles );
  for (size_t i = 0; i < tag_handles.size(); ++i) {
    Tag tag = tag_handles[i];
    if (tag == new_handles) // don't mess up mapping from old to new handles
      continue; 

    for (EntityType t = MBVERTEX; t <= MBENTITYSET; ++t) {
      rval = reorder_tag_data( t, new_handles, tag );
      UNRECOVERABLE(rval);
    }
  }
  
  rval = update_set_contents( new_handles );
  UNRECOVERABLE(rval);
  
  return MB_SUCCESS;
}

ErrorCode ReorderTool::reorder_tag_data( EntityType etype, Tag new_handles, Tag tag )
{
  ErrorCode rval;
  
  int tagsize;
  DataType tagtype;
  rval = mMB->tag_get_data_type( tag, tagtype );
  if (MB_SUCCESS != rval) 
    return error(rval);
  if (MB_TYPE_BIT == tagtype)
    tagsize = 1;
  else {
    rval = mMB->tag_get_bytes( tag, tagsize );
    if (MB_VARIABLE_DATA_LENGTH == rval)
      tagsize = -1;
    else if (MB_SUCCESS != rval) 
      return error(rval);
  }

    // we don't re-order set handles, so we don't care about sets
    // unless the tag contains handles that need to be updated
  if (MBENTITYSET == etype && MB_TYPE_HANDLE != tagtype)
    return MB_SUCCESS;

    // get tagged entities
  Range old_tagged;
  rval = mMB->get_entities_by_type_and_tag( 0, etype, &tag, 0, 1, old_tagged );
  if (MB_SUCCESS != rval && old_tagged.empty()) 
    return error(rval);
  
    // remove entities that were not reordered, unless the tag
    // is handle type in which case we need to update the data
    // for all entities, regardless of reordering.
  std::vector<EntityHandle> newhandles;
  if (MB_TYPE_HANDLE == tagtype) 
    rval = get_reordered_handles( new_handles, old_tagged, newhandles );
  else
    rval = get_new_handles( new_handles, old_tagged, newhandles );
  CHKERR;

  if (old_tagged.empty())
    return MB_SUCCESS;

    // get copy of all tag data   
  std::vector<unsigned char> buffer;
  std::vector<const void*> pointers;
  std::vector<int> sizes;
    // if variable-length tag
  if (-1 == tagsize) { 
    pointers.resize(old_tagged.size());
    sizes.resize(pointers.size());
    rval = mMB->tag_get_by_ptr( tag, old_tagged, &pointers[0], &sizes[0] );
    CHKERR;
    int total = std::accumulate( sizes.begin(), sizes.end(), 0 );
    DataType dtype;
    mMB->tag_get_data_type( tag, dtype );
    int type_size;
    switch (dtype) {
      case MB_TYPE_INTEGER: type_size = sizeof(int);  break;
      case MB_TYPE_DOUBLE:  type_size = sizeof(double); break;
      case MB_TYPE_HANDLE:  type_size = sizeof(EntityHandle); break;
      case MB_TYPE_BIT:     type_size = 1; break;
      case MB_TYPE_OPAQUE:  type_size = 1; break;
      default:              return MB_TYPE_OUT_OF_RANGE;
    }
    buffer.resize( total*type_size );
    size_t off = 0;
    for (size_t j = 0; j < pointers.size(); ++j) {
      memcpy( &buffer[off], pointers[j], type_size*sizes[j] );
      pointers[j] = &buffer[off];
      off += sizes[j] * type_size;
    }
  }
    // if fixed-length tag
  else {
    buffer.resize( old_tagged.size() * tagsize );
    rval = mMB->tag_get_data( tag, old_tagged, &buffer[0] );
    CHKERR;
  }
  
    // if handle tag, update tag values for reordered handles
  if (MB_TYPE_HANDLE == tagtype) {
    assert(!(buffer.size() % sizeof(EntityHandle)));
    std::vector<unsigned char> buffer2(buffer.size());
    rval = get_reordered_handles( new_handles,
                 reinterpret_cast<const EntityHandle*>(&buffer[0]), 
                 reinterpret_cast<EntityHandle*>(&buffer2[0]), 
                 buffer.size() / sizeof(EntityHandle) );
    CHKERR;
      // if var-length tag then do not do swap because pointers[] contains pointers
      // into old buffer
    if (-1 == tagsize)
      memcpy( &buffer[0], &buffer2[0], buffer.size() );
    else
      buffer.swap(buffer2);
  }
  
    // store re-ordered tag data
  if (-1 == tagsize) {
    rval = mMB->tag_set_by_ptr( tag, &newhandles[0], newhandles.size(), &pointers[0], &sizes[0] );
    pointers.clear();
    sizes.clear();
    buffer.clear();
  }
  else {
    rval = mMB->tag_set_data( tag, &newhandles[0], newhandles.size(), &buffer[0] );
    buffer.clear();
  }
  CHKERR;
  
    // all permutations should be cyclical, but not all permuted
    // entities necessarily had tag values, so we might need to delete
    // tags for some entities
  std::sort( newhandles.begin(), newhandles.end() );
  std::vector<EntityHandle>::iterator k = newhandles.begin();
  Range::iterator i = old_tagged.begin();
  while (i != old_tagged.end()) {
    while (k != newhandles.end() && *k < *i)
      ++k;
    if (k == newhandles.end())
      break;
    
    if (*i == *k) // what old_tagged -= newhandles
      i = old_tagged.erase( i );
    else
      ++i;
  }
  
  if (!old_tagged.empty()) {
    rval = mMB->tag_delete_data( tag, old_tagged );
    CHKERR;
  }
  
  return MB_SUCCESS;
}

ErrorCode ReorderTool::update_set_contents( Tag nh_tag )
{
  Range sets;
  ErrorCode rval = mMB->get_entities_by_type( 0, MBENTITYSET, sets );
  CHKERR;
  
  std::vector<EntityHandle> old_handles, new_handles;
  for (Range::iterator i = sets.begin(); i != sets.end(); ++i) {
      // If set is un-ordered...
    unsigned opts = 0;
    mMB->get_meshset_options( *i, opts );
    if (!(opts & MESHSET_ORDERED)) {
      Range contents;
      rval = mMB->get_entities_by_handle( *i, contents );
      CHKERR;
    
      rval = get_new_handles( nh_tag, contents, new_handles );
      CHKERR;
      
      Range replace;
      std::sort( new_handles.begin(), new_handles.end() );
      Range::iterator hint = replace.begin();
      for (size_t j = 0; j < new_handles.size(); ++j)
        hint = replace.insert( hint, new_handles[j] );
      Range common = intersect(contents,replace);
      contents -= common;
      replace -= common;
      assert(contents.size() == replace.size());
      if (!contents.empty()) {
        rval = mMB->remove_entities( *i, contents );
        CHKERR;
        rval = mMB->add_entities( *i, replace );
      }
    }
    
      // If set is ordered...
    else {
        // get set contents
      old_handles.clear();
      rval = mMB->get_entities_by_handle( *i, old_handles );
      CHKERR;

        // get new handles from old contained handles
      rval = get_reordered_handles( nh_tag, old_handles, new_handles );
      CHKERR;

      rval = mMB->clear_meshset( &*i, 1 );
      CHKERR;
        
      rval = mMB->add_entities( *i, &new_handles[0], new_handles.size() );
      CHKERR;
    }
  } // for each set
  
  return MB_SUCCESS;
}

} // namespace moab
