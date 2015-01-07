#ifdef WIN32
#ifdef _DEBUG
// turn off warnings that say they debugging identifier has been truncated
// this warning comes up when using some STL containers
#pragma warning(disable : 4786)
#endif
#endif

#include "MeshSet.hpp"
#include "AEntityFactory.hpp"

namespace moab {


/*****************************************************************************************
 *                          Helper Function Declarations                                 *
 *****************************************************************************************/

/**\brief Insert into parent/child list */
static inline 
MeshSet::Count insert_in_vector( const MeshSet::Count count, 
                                   MeshSet::CompactList& list,
                                   const EntityHandle h,
                                   int &result );

/**\brief Remvoe from parent/child list */
static inline
MeshSet::Count remove_from_vector( const MeshSet::Count count, 
                                     MeshSet::CompactList& list,
                                     const EntityHandle h,
                                     int &result );


/**\brief Resize MeshSet::CompactList.  Returns pointer to storage */
static EntityHandle* resize_compact_list( MeshSet::Count& count,
                                            MeshSet::CompactList& clist,
                                            size_t new_list_size );
/**\brief Methods to insert/remove range-based data from contents list.
 *        Templatized to operate on both Range and set-based MeshSets.
 */
template <typename pair_iter_t> class range_tool
{
public:
  /** Insert range-based data into range-based MeshSet */
  inline static ErrorCode ranged_insert_entities( MeshSet::Count& count, 
                                                    MeshSet::CompactList& clist, 
                                                    pair_iter_t begin, 
                                                    pair_iter_t end, 
                                                    EntityHandle my_handle, 
                                                    AEntityFactory* adj );
  
  /** Remove range-based data from range-based MeshSet */
  inline static ErrorCode ranged_remove_entities( MeshSet::Count& count, 
                                                    MeshSet::CompactList& clist, 
                                                    pair_iter_t begin, 
                                                    pair_iter_t end, 
                                                    EntityHandle my_handle, 
                                                    AEntityFactory* adj );

  /** Insert range-based data into list-based MeshSet */
  inline static ErrorCode vector_insert_entities( MeshSet::Count& count, 
                                                    MeshSet::CompactList& clist, 
                                                    pair_iter_t begin, 
                                                    pair_iter_t end, 
                                                    EntityHandle my_handle, 
                                                    AEntityFactory* adj );
};

/** Remove Range of handles fromr vector-based MeshSet */
static ErrorCode vector_remove_range( MeshSet::Count& count, 
                                        MeshSet::CompactList& clist, 
                                        const Range& range, 
                                        EntityHandle my_handle, 
                                        AEntityFactory* adj );

/** Remove range-based MeshSet contents from vector-based MeshSet */
static ErrorCode vector_remove_ranges( MeshSet::Count& count, 
                                         MeshSet::CompactList& clist, 
                                         const EntityHandle* pair_list,
                                         size_t num_pairs,
                                         EntityHandle my_handle, 
                                         AEntityFactory* adj );

/** Remove unsorted array of handles from vector-based MeshSet */
static ErrorCode vector_remove_vector( MeshSet::Count& count, 
                                         MeshSet::CompactList& clist, 
                                         const EntityHandle* vect,
                                         size_t vect_size,
                                         EntityHandle my_handle, 
                                         AEntityFactory* adj );

/** Insert unsorted array of handles into vector-based MeshSet */
static ErrorCode vector_insert_vector( MeshSet::Count& count, 
                                         MeshSet::CompactList& clist, 
                                         const EntityHandle* vect,
                                         size_t vect_size,
                                         EntityHandle my_handle, 
                                         AEntityFactory* adj );

/** Convert unsorted array of handles into array of ranged [begin,end] pairs */
static void convert_to_ranges( const EntityHandle* vect_in, size_t vect_in_len,
                               std::vector<EntityHandle>& vect_out );


/*****************************************************************************************
 *                             Parent/Child Operations                                   *
 *****************************************************************************************/

static inline 
MeshSet::Count insert_in_vector( const MeshSet::Count count, 
                                MeshSet::CompactList& list,
                                const EntityHandle h,
                                int &result )
{
  switch (count) {
    case MeshSet::ZERO:
      list.hnd[0] = h;
      result = true;
      return MeshSet::ONE;
    case MeshSet::ONE:
      if (list.hnd[0] == h) {
        result = false;
        return MeshSet::ONE;
      }
      else {
        result = true;
        list.hnd[1] = h;
        return MeshSet::TWO;
      }
    case MeshSet::TWO:
      if (list.hnd[0] == h || list.hnd[1] == h) {
        result = false;
        return MeshSet::TWO;
      }
      else {
        EntityHandle* ptr = (EntityHandle*)malloc(3*sizeof(EntityHandle));
        ptr[0] = list.hnd[0];
        ptr[1] = list.hnd[1];
        ptr[2] = h;
        list.ptr[0] = ptr;
        list.ptr[1] = ptr + 3;
        result = true;
        return MeshSet::MANY;
      }
    case MeshSet::MANY:
      if (std::find( list.ptr[0], list.ptr[1], h ) != list.ptr[1]) {
        result = false;
      }
      else {
        int size = list.ptr[1] - list.ptr[0];
        list.ptr[0] = (EntityHandle*)realloc( list.ptr[0], (size+1)*sizeof(EntityHandle) );
        list.ptr[0][size] = h;
        list.ptr[1] = list.ptr[0] + size + 1;
        result = true;
      }
      return MeshSet::MANY;
  }

  return MeshSet::ZERO;
}

static inline
MeshSet::Count remove_from_vector( const MeshSet::Count count, 
                                  MeshSet::CompactList& list,
                                  const EntityHandle h,
                                  int &result )
{
  switch (count) {
    case MeshSet::ZERO:
      result = false;
      return MeshSet::ZERO;
    case MeshSet::ONE:
      if (h == list.hnd[0]) {
        result = true;
        return MeshSet::ZERO;
      }
      else {
        result = false;
        return MeshSet::ONE;
      }
    case MeshSet::TWO:
      if (h == list.hnd[0]) {
        list.hnd[0] = list.hnd[1];
        result = true;
        return MeshSet::ONE;
      } 
      else if (h == list.hnd[1]) {
        result = true;
        return MeshSet::ONE;
      }
      else {
        result = false;
        return MeshSet::TWO;
      }
    case MeshSet::MANY: {
      EntityHandle *i, *j, *p;
      i = std::find( list.ptr[0], list.ptr[1], h );
      if (i == list.ptr[1]) {
        result = false;
        return MeshSet::MANY;
      }
      
      result = true;
      p = list.ptr[1] - 1;
      while (i != p) {
        j = i + 1;
        *i = *j;
        i = j;
      }
      int size = p - list.ptr[0];
      if (size == 2) {
        p = list.ptr[0];
        list.hnd[0] = p[0];
        list.hnd[1] = p[1];
        free( p );
        return MeshSet::TWO;
      }
      else {
        list.ptr[0] = (EntityHandle*)realloc( list.ptr[0], size*sizeof(EntityHandle) );
        list.ptr[1] = list.ptr[0] + size;
        return MeshSet::MANY;
      }
    }
  }

  return MeshSet::ZERO;
}


int MeshSet::add_parent( EntityHandle parent )
{ 
  int result = 0;
  mParentCount = insert_in_vector( (Count)mParentCount, parentMeshSets, parent, result );
  return result;
}
int MeshSet::add_child( EntityHandle child )
{ 
  int result = 0;
  mChildCount = insert_in_vector( (Count)mChildCount, childMeshSets, child, result );
  return result;
}

int MeshSet::remove_parent( EntityHandle parent )
{ 
  int result = 0;
  mParentCount = remove_from_vector( (Count)mParentCount, parentMeshSets, parent, result );
  return result;
}
int MeshSet::remove_child( EntityHandle child )
{ 
  int result = 0;
  mChildCount = remove_from_vector( (Count)mChildCount, childMeshSets, child, result );
  return result;
}


/*****************************************************************************************
 *                          Flag Conversion Operations                                   *
 *****************************************************************************************/

ErrorCode MeshSet::convert( unsigned flg, EntityHandle my_handle, AEntityFactory* adj )
{
  ErrorCode rval = MB_SUCCESS;
  if ((mFlags & MESHSET_TRACK_OWNER) && !(flg & MESHSET_TRACK_OWNER))
    rval = remove_adjacencies( my_handle, adj );
  else if (!(mFlags & MESHSET_TRACK_OWNER) && (flg & MESHSET_TRACK_OWNER))
    rval = create_adjacencies( my_handle, adj );
  if (MB_SUCCESS != rval)
    return rval;

  if (!(mFlags & MESHSET_ORDERED) && (flg & MESHSET_ORDERED)) {
    size_t datalen;
    EntityHandle* data = get_contents(datalen);
    if (datalen) {
      std::vector<EntityHandle> list( datalen );
      memcpy( &list[0], data, datalen*sizeof(EntityHandle) );
      int num_ents = num_entities();
      Count count = (Count)mContentCount;
      data = resize_compact_list( count, contentList, num_ents );
      mContentCount = count;
      assert( list.size() % 2 == 0 );
      std::vector<EntityHandle>::iterator i = list.begin();
      while (i != list.end()) {
        EntityHandle h = *i; ++i;
        EntityHandle e = *i; ++i;
        for (; h <= e; ++h) {
          *data = h; 
          ++data;
        }
      }
    }
  }
  else if ((mFlags & MESHSET_ORDERED) && !(flg & MESHSET_ORDERED)) {
    size_t datalen;
    EntityHandle* data = get_contents(datalen);
    if (datalen) {
      std::vector<EntityHandle> ranges;
      convert_to_ranges( data, datalen, ranges );
      Count count = (Count)mContentCount;
      data = resize_compact_list( count, contentList, ranges.size() );
      mContentCount = count;
      memcpy( data, &ranges[0], ranges.size()*sizeof(EntityHandle) );
    }
  }
  
  return MB_SUCCESS;
}

ErrorCode MeshSet::create_adjacencies( EntityHandle my_handle, AEntityFactory* adj )
{
  ErrorCode rval = MB_SUCCESS;;
  size_t count;
  const EntityHandle *const ptr = get_contents( count );
  const EntityHandle *const end = ptr + count;
  if (vector_based()) {
    for (const EntityHandle* i = ptr; i != end; ++i) {
      rval = adj->add_adjacency( *i, my_handle, false );
      if (MB_SUCCESS != rval) {
        for (const EntityHandle* j = ptr; j != i; ++j) 
          adj->remove_adjacency( *j, my_handle );
        return rval;
      }
    }
  }
  else {
    assert( 0 == count % 2 );
    for (const EntityHandle* i = ptr; i != end; i += 2) {
      for (EntityHandle h = i[0]; h <= i[1]; ++h) {
        rval = adj->add_adjacency( h, my_handle, false );
        if (MB_SUCCESS != rval) {
          for (EntityHandle j = i[0]; j < h; ++j)
            adj->remove_adjacency( j, my_handle );
          for (const EntityHandle* j = ptr; j != i; j += 2)
            for (EntityHandle k = j[0]; k <= j[1]; ++k)
              adj->remove_adjacency( k, my_handle );
          return rval;
        }
      }
    }
  }
  return MB_SUCCESS;
}

ErrorCode MeshSet::remove_adjacencies( EntityHandle my_handle, AEntityFactory* adj )
{
  size_t count;
  const EntityHandle *const ptr = get_contents( count );
  const EntityHandle *const end = ptr + count;
  if (vector_based()) {
    for (const EntityHandle* i = ptr; i != end; ++i)
      adj->remove_adjacency( *i, my_handle );
  }
  else {
    assert( 0 == count % 2 );
    for (const EntityHandle* i = ptr; i != end; i += 2)
      for (EntityHandle h = i[0]; h <= i[1]; ++h)
        adj->remove_adjacency( h, my_handle );
  }
  return MB_SUCCESS;
}


/*****************************************************************************************
 *                          Contents Modifiction Methods                                 *
 *****************************************************************************************/

static EntityHandle* resize_compact_list( MeshSet::Count& count,
                                            MeshSet::CompactList& clist,
                                            size_t new_list_size )
{
  if (count <= 2) {
    if (new_list_size <= 2) {
      count = (MeshSet::Count)new_list_size;
      return clist.hnd;
    }
    else {
      EntityHandle* list = (EntityHandle*)malloc( new_list_size*sizeof(EntityHandle) );
      list[0] = clist.hnd[0];
      list[1] = clist.hnd[1];
      clist.ptr[0] = list;
      clist.ptr[1] = list + new_list_size;
      count = MeshSet::MANY;
      return list;
    }
  }
  else if (new_list_size > 2) {
    if (new_list_size > (size_t)(clist.ptr[1] - clist.ptr[0]))
      clist.ptr[0] = (EntityHandle*)realloc( clist.ptr[0], new_list_size*sizeof(EntityHandle) );
    clist.ptr[1] = clist.ptr[0] + new_list_size;
    count = MeshSet::MANY;
    return clist.ptr[0];
  }
  else {
    EntityHandle* list = clist.ptr[0];
    clist.hnd[0] = list[0];
    clist.hnd[1] = list[1];
    free(list);
    count = (MeshSet::Count)new_list_size;
    return clist.hnd;
  }
}

typedef std::pair<EntityHandle,EntityHandle> MeshSetRange;

class MeshSetRComp {
  public: bool operator()( const MeshSetRange& r, EntityHandle h )
    { return r.second < h; }
};

template <typename pair_iter_t> inline ErrorCode
range_tool<pair_iter_t>::ranged_insert_entities( MeshSet::Count& count, 
                                                 MeshSet::CompactList& clist, 
                                                 pair_iter_t begin, 
                                                 pair_iter_t end, 
                                                 EntityHandle my_handle, 
                                                 AEntityFactory* adj )
{
     //first pass:
    // 1) merge existing ranges 
    // 2) count number of new ranges that must be inserted
  EntityHandle *list_ptr;
  size_t list_size;
  if (count < MeshSet::MANY) {
    list_ptr = clist.hnd;
    list_size = count;
  }
  else {
    list_ptr = clist.ptr[0];
    list_size = clist.ptr[1] - clist.ptr[0];
  }
  
  MeshSetRange* list = reinterpret_cast<MeshSetRange*>(list_ptr);
  assert(0 == list_size % 2);
  assert(2*sizeof(EntityHandle) == sizeof(MeshSetRange));
  list_size /= 2;
  MeshSetRange *const list_end = list + list_size;
  MeshSetRange *list_read = list, *list_write = list;
  pair_iter_t i = begin;
  
    // count number of range pairs that are straight insertions
    // (don't overlap any range pair in the current set) that 
    // could not be inserted during the first pass.
  size_t insert_count = 0;
  
   // merge lists
  while(i != end) {
    // find first range that intersects the current input range
    
    // do binary search if no holes in current set contents
    if (list_read == list_write) {
        // subtract one from i->first because if it is one greater
        // then the the last value of some block, then we want that
        // block to append to.
      list_write = std::lower_bound( list_read, list_end, i->first-1, MeshSetRComp() );
      list_read = list_write;
    }
    // otherwise shift down until we find where we find a range block
    // that intersects
    else while (list_read != list_end && list_read->second + 1 < i->first) {
      *list_write = *list_read;
      ++list_write;
      ++list_read;
    }
    
      // handle any straight insertions of range blocks
    for ( ; i != end && (list_read == list_end || i->second+1 < list_read->first); ++i) {
        // If we haven't removed any range pairs, we don't have space to
        // insert here.  Defer the insertion until later.
      if (list_read == list_write) {
        ++insert_count;
      }
      else {
        if (adj) 
          for (EntityHandle j = i->first; j <= i->second; ++j)
            adj->add_adjacency( j, my_handle, false );

        list_write->first = i->first;
        list_write->second = i->second;
        ++list_write;
      }
    }
    
      // merge all the stuff that gets merged into a single range pair
      // from both the input list and the existing set data
    if (list_read != list_end) {
      MeshSetRange working = *list_read; // copy because might be the same as list_write
      ++list_read;
      
        // Check if we need to prepend to the existing block.
        // We only need to check this for the first input range because
        // after this working.first will always be the first possible handle
        // in the merged set of ranges.
      if (i != end && i->first < working.first && i->second+1 >= working.first) {
        if (adj)
          for (EntityHandle h = i->first; h < working.first; ++h)
            adj->add_adjacency( h, my_handle, false );
        working.first = i->first;
      }
      
        // Now append from the input list and the remaining set contents
        // until we've consolidated all overlapping/touching ranges.
      bool done = false;
      while (!done) {
          // does next set contents range touch working range?
        bool set_overlap = list_read != list_end && list_read->first <= working.second+1;
          // does next input range touch working range?
        bool inp_overlap = i != end && i->first <= working.second+1;
        
          // if both ranges touch...
        if (inp_overlap && set_overlap) {
            // if next set range is contained in working, skip it
          if (list_read->second <= working.second)
            ++list_read;
            // if next input range is contained in working, skip it
          else if (i->second <= working.second)
            ++i;
            // Otherwise set the working end to the smaller of the two 
            // ends: either the next set end or the next input end.
            // We want the smaller of the two because the larger might
            // intersect additional ranges in the other list.
          else if (list_read->second <= i->second) {
            working.second = list_read->second;
            ++list_read;
          }
          else {
            working.second = i->second;
            ++i;
          }
        }
          // If only the input range intersect the current working range...
        else if (inp_overlap) {
            // Would it be more efficient to just extent 'working' to 
            // the end of the current input range?  We'd end up adding
            // adjacencies for for entities that are already in the tracking
            // set and therefore already have the adjacency.
          EntityHandle last = i->second;
          if (list_read != list_end && list_read->first < last)
            last = list_read->first-1;
          else
            ++i;
          
          if (last > working.second) {
            if (adj)
              for (EntityHandle h = working.second + 1; h <= last; ++h)
                adj->add_adjacency( h, my_handle, false );

            working.second = last;
          }
        }
        else if (set_overlap) {
          if (working.second < list_read->second)
            working.second = list_read->second;
          ++list_read;
        }
        else {
          done = true;
        }
      }
      
      assert(list_write < list_read);
      *list_write = working;
      ++list_write;
    }
  }
  

    // shuffle down entries to fill holes
  if (list_read == list_write) 
    list_read = list_write = list_end;
  else while(list_read < list_end) {
    *list_write = *list_read;
    ++list_read;
    ++list_write;
  }

    // adjust allocated array size
  const size_t occupied_size = list_write - list;
  const size_t new_list_size = occupied_size + insert_count;
  list_ptr = resize_compact_list( count, clist, 2*new_list_size );
    // done?
  if (!insert_count)
    return MB_SUCCESS;
  list = reinterpret_cast<MeshSetRange*>(list_ptr);

    // Second pass: insert non-mergable range pairs
    // All range pairs in the input are either completely disjoint from
    // the ones in the mesh set and must be inserted or are entirely contained
    // within a range pair in the mesh set.
  assert( begin != end ); // can't have items to insert if given empty input list
  pair_iter_t ri = end; --ri;
  list_write = list + new_list_size - 1;
  list_read = list + occupied_size - 1;
  for ( ; list_write >= list; --list_write ) {
    if (list_read >= list) {
      while (ri->first >= list_read->first && ri->second <= list_read->second) {
        assert(ri != begin);
        --ri;
      }
    
      if (list_read->first > ri->second) {
        *list_write = *list_read;
        --list_read;
        continue;
      }
    }
    
    assert( insert_count > 0 );
    if (adj) 
      for (EntityHandle h = ri->first; h <= ri->second; ++h) 
        adj->add_adjacency( h, my_handle, false );
    list_write->first = ri->first;
    list_write->second = ri->second;

      // don't have reverse iterator, so check before decrement
      // if insert_count isn't zero, must be more in range
    if (0 == --insert_count) {
      assert( list_read == list_write-1 );
      break;
    }
    else {
      --ri;
    }
  }

  assert(!insert_count);
  return MB_SUCCESS;
}
 
template <typename pair_iter_t> inline ErrorCode
range_tool<pair_iter_t>::ranged_remove_entities( MeshSet::Count& count, 
                                                 MeshSet::CompactList& clist, 
                                                 pair_iter_t begin, 
                                                 pair_iter_t end, 
                                                 EntityHandle my_handle, 
                                                 AEntityFactory* adj )
{
    //first pass:
    // 1) remove (from) existing ranges 
    // 2) count number of ranges that must be split
  ptrdiff_t split_count = 0;
  EntityHandle *list;
  size_t list_size;
  if (count < MeshSet::MANY) {
    list = clist.hnd;
    list_size = count;
  }
  else {
    list = clist.ptr[0];
    list_size = clist.ptr[1] - clist.ptr[0];
  }

  EntityHandle* list_write = list;
  EntityHandle *const list_end = list + list_size, *list_read = list;
  pair_iter_t i = begin;
  
  while(list_read != list_end && i != end) {
    
    while (i != end && i->second < list_read[0])
      ++i;
    if (i == end)
      break;
    
      // if there are holes in the current array, shuffle blocks 
      // down until we find the next block to remove
    if (list_read != list_write) {
      while (list_read != list_end && i->second < list_read[0]) {
      	list_write[0] = list_read[0];
        list_write[1] = list_read[1];
        list_write += 2;
        list_read += 2;
      }
    }
      // otherwise do a binary search
    else {
      list_write = std::lower_bound( list_write, list_end, i->first );
      	// if in middle of range block (odd index), back up to start of block
      list_write -= (list_write - list)%2;
      list_read = list_write;
    }
    
      // if everything remaning is past end of set contents...
    if (list_read == list_end) 
      break;
      
      // skip any remove pairs that aren't in the list
    if (i->second < list_read[0]) {
      ++i;
      continue;
    }
    
      // Begin by assuming that we will keep the entire block
    list_write[0] = list_read[0];
    list_write[1] = list_read[1];
    list_read += 2;
    
    for (; i != end && i->first <= list_write[1]; ++i) {
      if (i->first <= list_write[0]) {
          // remove whole block
        if (i->second >= list_write[1]) {
          if (adj)
            for (EntityHandle h = list_write[0]; h <= list_write[1]; ++h)
              adj->remove_adjacency( h, my_handle );
          list_write -= 2;
          break;
        }
          // remove from start of block
        else if (i->second >= list_write[0]) {
          if (adj)
            for (EntityHandle h = list_write[0]; h <= i->second; ++h)
              adj->remove_adjacency( h, my_handle );
          list_write[0] = i->second + 1;
        }
      }
      else if (i->first <= list_write[1]) {
          // remove from end of block
        if (i->second >= list_write[1]) {
          if (adj)
            for (EntityHandle h = i->first; h <= list_write[1]; ++h)
              adj->remove_adjacency( h, my_handle );
          list_write[1] = i->first - 1;
          //list_write += 2;
          break;
        }
          // split block
        else {
          if (adj)
            for (EntityHandle h = i->first; h <= i->second; ++h)
              adj->remove_adjacency( h, my_handle );

          if (list_read - list_write <= 2) {
            ++split_count;
            continue;
          }
          else {
            list_write[3] = list_write[1];
            list_write[1] = i->first - 1;
            list_write[2] = i->second + 1;
            list_write += 2;
          }
        }
      }
    }
    list_write += 2;
  }

    // shuffle down entries to fill holes
  if (list_read == list_write) 
    list_read = list_write = list_end;
  else 
    while(list_read < list_end) {
      list_write[0] = list_read[0];
      list_write[1] = list_read[1];
      list_read += 2;
      list_write += 2;
    }

    // adjust allocated array size
  const size_t occupied_size = list_write - list;
  const size_t new_list_size = occupied_size + 2*split_count;
  list = resize_compact_list( count, clist, new_list_size );
    // done?
  if (!split_count)
    return MB_SUCCESS;

    // Second pass: split range pairs
    // All range pairs in the input are either already removed or
    // require one of the existing range pairs to be split
  assert( begin != end ); // can't have ranges to split if given empty input list
  pair_iter_t ri = end; --ri;
  list_write = list + new_list_size - 2;
  list_read = list + occupied_size - 2;
  for ( ; list_write >= list; list_write -= 2 ) {
    if (list_read >= list) {
      while (ri->second > list_read[1]) {
        assert(ri != begin);
        --ri;
      }
    
      if (list_read[0] > ri->second) {
        list_write[0] = list_read[0];
        list_write[1] = list_read[1];
        list_read -= 2;
        continue;
      }
    }
    
    assert( split_count > 0 );
    list_write[0] = ri->second + 1;
    list_write[1] = list_read[1];
    list_read[1] = ri->first - 1;

      // don't have reverse iterator, so check before decrement
      // if insert_count isn't zero, must be more in range
    if (0 == --split_count) {
      assert( list_read == list_write-2 );
      break;
    }
    else {
      --ri;
    }
  }

  assert(!split_count);
  return MB_SUCCESS;
}


template <typename pair_iter_t> inline ErrorCode
range_tool<pair_iter_t>::vector_insert_entities( MeshSet::Count& count, 
                                                 MeshSet::CompactList& clist, 
                                                 pair_iter_t begin, 
                                                 pair_iter_t end, 
                                                 EntityHandle my_handle, 
                                                 AEntityFactory* adj )
{
  const size_t init_size = count < MeshSet::MANY ? (int)count : clist.ptr[1] - clist.ptr[0];
  size_t add_size = 0;
  for (pair_iter_t i = begin; i != end; ++i)
    add_size += i->second - i->first + 1;
  EntityHandle* list = resize_compact_list( count, clist, init_size + add_size );
  EntityHandle* li = list + init_size;

  for (pair_iter_t i = begin; i != end; ++i) {
    for (EntityHandle h = i->first; h <= i->second; ++h) {
      if (adj)
        adj->add_adjacency( h, my_handle, false );
      *li = h;
      ++li;
    }
  }

  return MB_SUCCESS;
}

static ErrorCode vector_remove_range( MeshSet::Count& count, 
                                        MeshSet::CompactList& clist, 
                                        const Range& range, 
                                        EntityHandle my_handle, 
                                        AEntityFactory* adj )
{
  EntityHandle *list;
  size_t list_size;
  if (count < MeshSet::MANY) {
    list = clist.hnd;
    list_size = count;
  }
  else {
    list = clist.ptr[0];
    list_size = clist.ptr[1] - clist.ptr[0];
  }

  const EntityHandle * const list_end = list + list_size;
  EntityHandle* list_write = list;
  for (const EntityHandle* list_read = list; list_read != list_end; ++list_read) {
    if (range.find(*list_read) == range.end()) { // keep
      *list_write = *list_read;
      ++list_write;
    }
    else if (adj) {    
      adj->remove_adjacency( *list_read, my_handle );
    }
  }

  resize_compact_list( count, clist, list_write - list );
  return MB_SUCCESS;
}

static ErrorCode vector_remove_ranges( MeshSet::Count& count, 
                                         MeshSet::CompactList& clist, 
                                         const EntityHandle* pair_list,
                                         size_t num_pairs,
                                         EntityHandle my_handle, 
                                         AEntityFactory* adj )
{
  EntityHandle *list;
  size_t list_size;
  if (count < MeshSet::MANY) {
    list = clist.hnd;
    list_size = count;
  }
  else {
    list = clist.ptr[0];
    list_size = clist.ptr[1] - clist.ptr[0];
  }

  const EntityHandle *const list_end = list + list_size, 
                       *const input_end = pair_list + 2*num_pairs;
  EntityHandle* list_write = list;
  for (const EntityHandle* list_read = list; list_read != list_end; ++list_read) {
    const EntityHandle* ptr = std::lower_bound( pair_list, input_end, *list_read );
    if ((ptr != input_end && (*ptr == *list_read || (ptr - pair_list)%2)) && // if in delete list
        std::find(list_read+1, list_end, *list_read) == list_end) { // and is last occurance in list 
        // only remove adj if no previous occurance
      if (adj && std::find(list, list_write, *list_read) == list_write)
        adj->remove_adjacency( *list_read, my_handle );
    }
    else {
      *list_write = *list_read;
      ++list_write;
    }
  }

  resize_compact_list( count, clist, list_write - list );
  return MB_SUCCESS;
}

static ErrorCode vector_remove_vector( MeshSet::Count& count, 
                                         MeshSet::CompactList& clist, 
                                         const EntityHandle* vect,
                                         size_t vect_size,
                                         EntityHandle my_handle, 
                                         AEntityFactory* adj )
{
  EntityHandle *list;
  size_t list_size;
  if (count < MeshSet::MANY) {
    list = clist.hnd;
    list_size = count;
  }
  else {
    list = clist.ptr[0];
    list_size = clist.ptr[1] - clist.ptr[0];
  }

  const EntityHandle *const list_end = list + list_size, 
                       *const input_end = vect + vect_size;
  EntityHandle* list_write = list;
  for (const EntityHandle* list_read = list; list_read != list_end; ++list_read) {
    if (std::find(vect, input_end, *list_read) != input_end && // if in delete list
        std::find(list_read+1, list_end, *list_read) == list_end) { // and is last occurance in list 
        // only remove adj if no previous occurance?
      if (adj ) // && std::find(list, list_write, *list_read) == list_write)
        adj->remove_adjacency( *list_read, my_handle );
    }
    else {
      *list_write = *list_read;
      ++list_write;
    }
  }

  resize_compact_list( count, clist, list_write - list );
  return MB_SUCCESS;
}

static ErrorCode vector_insert_vector( MeshSet::Count& count, 
                                         MeshSet::CompactList& clist, 
                                         const EntityHandle* vect,
                                         size_t vect_size,
                                         EntityHandle my_handle, 
                                         AEntityFactory* adj )
{
  const size_t orig_size = count < MeshSet::MANY ? (int)count : clist.ptr[1] - clist.ptr[0];
  EntityHandle* list = resize_compact_list( count, clist, orig_size + vect_size );
  if (adj) 
    for (size_t i = 0; i < vect_size; ++i)
      adj->add_adjacency( vect[i], my_handle, false );
  memcpy( list+orig_size, vect, sizeof(EntityHandle)*vect_size );
  return MB_SUCCESS;
}

ErrorCode MeshSet::insert_entity_ranges( const EntityHandle* range_vect, size_t len, EntityHandle my_h, AEntityFactory* adj )
{
  typedef const std::pair<EntityHandle,EntityHandle>* pair_vect_t;
  pair_vect_t pair_vect = reinterpret_cast<pair_vect_t>(range_vect);
  MeshSet::Count count = static_cast<MeshSet::Count>(mContentCount);
  ErrorCode rval;
  if (!vector_based())
    rval = range_tool<pair_vect_t>::ranged_insert_entities( count, contentList,  pair_vect, 
                                             pair_vect + len/2, my_h, tracking() ? adj : 0 );
  else
    rval = range_tool<pair_vect_t>::vector_insert_entities( count, contentList,  pair_vect, 
                                             pair_vect + len/2, my_h, tracking() ? adj : 0 );
  mContentCount = count;
  return rval;
}

ErrorCode MeshSet::insert_entity_ranges( const Range& range, EntityHandle my_h, AEntityFactory* adj )
{
  ErrorCode rval;
  MeshSet::Count count = static_cast<MeshSet::Count>(mContentCount);
  if (!vector_based())
    rval = range_tool<Range::const_pair_iterator>::ranged_insert_entities( count, 
                             contentList, range.const_pair_begin(), range.const_pair_end(), 
                             my_h, tracking() ? adj : 0 );
  else
    rval = range_tool<Range::const_pair_iterator>::vector_insert_entities( count, 
                             contentList, range.const_pair_begin(), range.const_pair_end(), 
                             my_h, tracking() ? adj : 0 );
  mContentCount = count;
  return rval;
}

ErrorCode MeshSet::remove_entity_ranges( const EntityHandle* range_vect, size_t len, EntityHandle my_h, AEntityFactory* adj )
{
  ErrorCode rval;
  MeshSet::Count count = static_cast<MeshSet::Count>(mContentCount);
  if (vector_based()) 
    rval = vector_remove_ranges( count, contentList, range_vect, len/2, my_h, 
                                 tracking() ? adj : 0 );
  else {
    typedef const std::pair<EntityHandle,EntityHandle>* pair_vect_t;
    pair_vect_t pair_vect = reinterpret_cast<pair_vect_t>(range_vect);
    rval = range_tool<pair_vect_t>::ranged_remove_entities( count, contentList, pair_vect, 
                                           pair_vect + len/2, my_h, tracking() ? adj : 0 );
  }
  mContentCount = count;
  return rval;
}

ErrorCode MeshSet::remove_entity_ranges( const Range& range, EntityHandle my_h, AEntityFactory* adj )
{
  ErrorCode rval;
  MeshSet::Count count = static_cast<MeshSet::Count>(mContentCount);
  if (vector_based()) 
    rval = vector_remove_range( count, contentList, range, my_h, tracking() ? adj : 0 );
  else 
    rval = range_tool<Range::const_pair_iterator>::ranged_remove_entities( count, 
                         contentList, range.const_pair_begin(), range.const_pair_end(), 
                         my_h, tracking() ? adj : 0 );
  mContentCount = count;
  return rval;
}


ErrorCode MeshSet::intersect( const MeshSet* other, EntityHandle my_handle, AEntityFactory* adj )
{
  ErrorCode rval;
  if (!vector_based() && !other->vector_based()) {
    size_t other_count = 0;
    const EntityHandle* other_vect = other->get_contents( other_count );
    if (!other_count)
      return clear( my_handle, adj );
    assert(0 == other_count%2);
    
    std::vector<EntityHandle> compliment;
    compliment.reserve( other_count + 4 );
    if (*other_vect > 0) {
      compliment.push_back( 0 );
      compliment.push_back( *other_vect - 1 );
    }
    ++other_vect;
    const EntityHandle *const other_end = other_vect + other_count - 2;
    for (; other_vect < other_end; other_vect += 2) {
      compliment.push_back( other_vect[0] + 1 );
      compliment.push_back( other_vect[1] - 1 );
    }
    if (*other_vect < ~(EntityHandle)0) {
      compliment.push_back( *other_vect + 1 );
      compliment.push_back( ~(EntityHandle)0 );
    }
    
    return remove_entity_ranges( &compliment[0], compliment.size(), my_handle, adj );
  }
  else {
    Range my_ents, other_ents;
    rval = get_entities(my_ents);
    if (MB_SUCCESS != rval)
      return rval;
    rval = other->get_entities(other_ents);
    return remove_entities( moab::subtract(my_ents, other_ents), my_handle, adj );
  }
}

static void convert_to_ranges( const EntityHandle* vect_in, size_t vect_in_len,
                               std::vector<EntityHandle>& vect_out )
{
  vect_out.reserve( 2*vect_in_len );
  vect_out.resize( vect_in_len );
  std::copy( vect_in, vect_in+vect_in_len, vect_out.begin() );
  std::sort( vect_out.begin(), vect_out.end() );
  vect_out.erase( std::unique( vect_out.begin(), vect_out.end() ), vect_out.end() );

    // duplicate all entries
  vect_out.resize( vect_out.size() * 2 );
  for (long i = vect_out.size() - 1; i >= 0; --i) 
    vect_out[i] = vect_out[i/2];
   
    // compact adjacent ranges
  std::vector<EntityHandle>::iterator r = vect_out.begin(), w = vect_out.begin();
  while (r != vect_out.end()) {
    *w = *r;
    ++w; 
    ++r;
    *w = *r;
    ++r;
    
    while (r != vect_out.end() && *w + 1 == *r) {
      ++r;
      *w = *r;
      ++r;
    }
    ++w;
  }
  
    // remove extra space
  vect_out.erase( w, vect_out.end() );
}

ErrorCode MeshSet::insert_entity_vector( const EntityHandle* vect, size_t len, EntityHandle my_h, AEntityFactory* adj )
{
  MeshSet::Count count = static_cast<MeshSet::Count>(mContentCount);
  ErrorCode rval;
  if (vector_based())
    rval = vector_insert_vector( count, contentList, vect, len, my_h, tracking() ? adj : 0 );
  else {
    std::vector<EntityHandle> rangevect;
    convert_to_ranges( vect, len, rangevect );
    typedef const std::pair<EntityHandle,EntityHandle>* pair_vect_t;
    pair_vect_t pair_vect = reinterpret_cast<pair_vect_t>(&rangevect[0]);
    rval = range_tool<pair_vect_t>::ranged_insert_entities( count, contentList, pair_vect, 
                                 pair_vect + rangevect.size()/2, my_h, tracking() ? adj : 0 );
  }
  mContentCount = count;
  return rval;
}

ErrorCode MeshSet::remove_entity_vector( const EntityHandle* vect, size_t len, EntityHandle my_h, AEntityFactory* adj )
{
  MeshSet::Count count = static_cast<MeshSet::Count>(mContentCount);
  ErrorCode rval;
  if (vector_based())
    rval = vector_remove_vector( count, contentList, vect, len, my_h, tracking() ? adj : 0 );
  else {
    std::vector<EntityHandle> rangevect;
    convert_to_ranges( vect, len, rangevect );
    typedef const std::pair<EntityHandle,EntityHandle>* pair_vect_t;
    pair_vect_t pair_vect = reinterpret_cast<pair_vect_t>(&rangevect[0]);
    rval = range_tool<pair_vect_t>::ranged_remove_entities( count, contentList, pair_vect, 
                                pair_vect + rangevect.size()/2, my_h, tracking() ? adj : 0 );
  }
  mContentCount = count;
  return rval;
}



ErrorCode MeshSet::replace_entities( EntityHandle my_handle,
                                         const EntityHandle* old_entities,
                                         const EntityHandle* new_entities,
                                         size_t num_ents,
                                         AEntityFactory* adjfact )
{
  if (vector_based()) {
    ErrorCode result = MB_SUCCESS;
    size_t count;
    EntityHandle* vect = get_contents( count );
    EntityHandle* const vect_end = vect+count;
    for (size_t i = 0; i < num_ents; ++i) {
      EntityHandle* p = std::find( vect, vect_end, old_entities[i] );
      if (p == vect_end) {
        result = MB_ENTITY_NOT_FOUND;
      }
      else do {
        if (tracking()) {
          adjfact->remove_adjacency( *p, my_handle );
          adjfact->add_adjacency( new_entities[i], my_handle, false );
        }
        *p = new_entities[i];
        p = std::find( p+1, vect_end, old_entities[i] );
      } while (p != vect_end);
    }
    return result;
  }
  else {
    ErrorCode r1 = remove_entities( old_entities, num_ents, my_handle, adjfact );
    ErrorCode r2 = add_entities( new_entities, num_ents, my_handle, adjfact );
    return (MB_SUCCESS == r2) ? r1 : r2;
  }
}


/*****************************************************************************************
 *                                  Misc. Methods                                        *
 *****************************************************************************************/

unsigned long MeshSet::get_memory_use() const
{
  unsigned long result = 0;
  if (mParentCount == MANY)
    result += parentMeshSets.ptr[1] - parentMeshSets.ptr[0];
  if (mChildCount == MANY)
    result += childMeshSets.ptr[1] - childMeshSets.ptr[0];
  if (mContentCount == MANY)
    result += contentList.ptr[1] - contentList.ptr[0];
  return sizeof(EntityHandle)*result;
}
  
} // namespace moab
