#ifndef IS_BUILDING_MB
#error "SetIterator.hpp isn't supposed to be included into an application"
#endif

#include "moab/SetIterator.hpp"
#include "moab/Core.hpp"
#include "moab/WriteUtilIface.hpp"
#include "MeshSet.hpp"
#include "Internals.hpp"
#include "moab/CN.hpp"
#include "moab/Error.hpp"

namespace moab 
{
    
SetIterator::~SetIterator() 
{
  myCore->remove_set_iterator(this);
}

RangeSetIterator::RangeSetIterator(Core *core, EntityHandle eset, int chunk_sz, 
                          EntityType ent_tp, int ent_dim, bool check_valid) 
        : SetIterator(core, eset, chunk_sz, ent_tp, ent_dim, check_valid),
          iterPos(0), pairPtr(NULL), numPairs(0)
{
  if (!eset) {
      // special case for the root set, have to keep a local array
    ErrorCode rval = build_pair_vec();
    assert(MB_SUCCESS == rval);

      // empty statement to avoid warning
    (void)(rval);
  }
}

RangeSetIterator::~RangeSetIterator() 
{
  if (pairPtr) delete [] pairPtr;
  numPairs = 0;
}

ErrorCode RangeSetIterator::build_pair_vec() 
{
    // shouldn't be here unless we're iterating the root set
  assert(!entSet);
  
  Range all_ents;
  ErrorCode rval = myCore->get_entities_by_handle(0, all_ents);
  if (MB_SUCCESS != rval) return rval;

  if (pairPtr) delete [] pairPtr;
  pairPtr = new EntityHandle[2*all_ents.psize()];
  Range::const_pair_iterator pi;
  int i;
  for (pi = all_ents.const_pair_begin(), i = 0;
       pi != all_ents.const_pair_end(); pi++, i+=2) {
    pairPtr[i] = (*pi).first;
    pairPtr[i+1] = (*pi).second;
  }
  numPairs = all_ents.psize();

  return MB_SUCCESS;
}
    
ErrorCode RangeSetIterator::get_next_arr(std::vector<EntityHandle> &arr,
                                         bool &atend) 
{
  atend = false;  

  int count;
  const EntityHandle *ptr;
  WriteUtilIface *iface;
  std::vector<EntityHandle> tmp_arr;
  std::vector<EntityHandle> *tmp_ptr = &arr;
  if (checkValid) tmp_ptr = &tmp_arr;
  ErrorCode rval;
  if (!pairPtr) {
    Interface *mbImpl = dynamic_cast<Interface*>(myCore);
    rval = mbImpl->query_interface(iface);
    if (MB_SUCCESS != rval) return rval;
  
    rval = iface->get_entity_list_pointers( &entSet, 1, &ptr, WriteUtilIface::CONTENTS, &count);
    if (MB_SUCCESS != rval) return rval;
    mbImpl->release_interface(iface);
  }
  else {
    if (checkValid) {
      rval = build_pair_vec();
      if (MB_SUCCESS != rval) return rval;
    }
    ptr = pairPtr;
    count = 2*numPairs;
  }
  assert(!(count%2));
  if (!count) {
    atend = true;
    return MB_SUCCESS;
  }

  if (-1 == entDimension) rval = get_next_by_type(ptr, count, *tmp_ptr, atend);
  else rval = get_next_by_dimension(ptr, count, *tmp_ptr, atend);
  if (MB_SUCCESS != rval) return rval;
  
  if (checkValid) {
    for (std::vector<EntityHandle>::iterator vit = tmp_ptr->begin(); vit != tmp_ptr->end(); vit++) {
      if (myCore->is_valid(*vit)) arr.push_back(*vit);
    }
  }
    
  return MB_SUCCESS;
}

ErrorCode RangeSetIterator::get_next_by_type(const EntityHandle *&ptr, int count,
                                             std::vector<EntityHandle> &arr, bool &atend)
{
  unsigned int num_ret = 0;
  bool max_type = (entType == MBMAXTYPE);
  size_t idx = 0;
    // initialize to first relevant handle
  while ((int)idx < count &&
         (iterPos > ptr[idx+1] ||
          (!max_type && !iterPos && CREATE_HANDLE(entType, ID_FROM_HANDLE(iterPos)) > ptr[idx+1])))
    idx += 2;
  if ((int)idx == count || TYPE_FROM_HANDLE(ptr[idx]) > entType) {
    atend = true;
    return MB_SUCCESS;
  }
  if (!iterPos && max_type) iterPos = ptr[idx];
  else if (!iterPos && 
           TYPE_FROM_HANDLE(ptr[idx]) <= entType &&
           TYPE_FROM_HANDLE(ptr[idx+1]) >= entType) {
    iterPos = std::max(CREATE_HANDLE(entType,1), ptr[idx]);
  }
  
    // idx points to start of subrange, iterPos in that subrange
  do {
    EntityHandle next = ptr[idx+1];
    if (TYPE_FROM_HANDLE(next) != entType && !max_type) next = LAST_HANDLE(entType);
    unsigned int this_ret = chunkSize-num_ret;
    unsigned int to_end = next - iterPos + 1;
    if (to_end < this_ret) this_ret = to_end;
    std::copy(MeshSet::hdl_iter(iterPos), MeshSet::hdl_iter(iterPos + this_ret),
              std::back_inserter(arr));
    if (this_ret == to_end) {
      idx += 2;
      iterPos = ((int)idx < count ? ptr[idx] : 0);
    }
    else iterPos += this_ret;

    num_ret += this_ret;
  }
  while ((int)idx < count && num_ret < chunkSize && 
         iterPos && (max_type || TYPE_FROM_HANDLE(iterPos) == entType));

  if (!iterPos || (!max_type && TYPE_FROM_HANDLE(iterPos) != entType)) atend = true;

  return MB_SUCCESS;
}

ErrorCode RangeSetIterator::get_next_by_dimension(const EntityHandle *&ptr, int count,
                                                  std::vector<EntityHandle> &arr, bool &atend) 
{
    // iterating by dimension - type should be maxtype
  if (entType != MBMAXTYPE) {
    Error *error;
    dynamic_cast<Interface*>(myCore)->query_interface(error);
    error->set_last_error("Both dimension and type should not be set on an iterator.");
    return MB_FAILURE;
  }
    
  unsigned int num_ret = 0;
  size_t idx = 0;
    // initialize to first relevant handle
  while ((int)idx < count &&
         (iterPos > ptr[idx+1] ||
          (!iterPos && entDimension > CN::Dimension(TYPE_FROM_HANDLE(ptr[idx+1])))))
    idx += 2;
  if ((int)idx == count || CN::Dimension(TYPE_FROM_HANDLE(ptr[idx])) > entDimension) {
    atend = true;
    return MB_SUCCESS;
  }
  if (!iterPos) iterPos = ptr[idx];
  else if (CN::Dimension(TYPE_FROM_HANDLE(ptr[idx])) < entDimension)
    iterPos = CREATE_HANDLE(CN::TypeDimensionMap[entDimension].first,1);
  
    // idx points to start of subrange, iterPos in that subrange
  do {
    EntityHandle next = ptr[idx+1];
    if (CN::Dimension(TYPE_FROM_HANDLE(next)) != entDimension) 
      next = LAST_HANDLE(CN::TypeDimensionMap[entDimension].second);
    unsigned int this_ret = chunkSize-num_ret;
    unsigned int to_end = next - iterPos + 1;
    if (to_end < this_ret) this_ret = to_end;
    std::copy(MeshSet::hdl_iter(iterPos), MeshSet::hdl_iter(iterPos + this_ret),
              std::back_inserter(arr));
    if (this_ret == to_end) {
      idx += 2;
      iterPos = ((int)idx < count ? ptr[idx] : 0);
    }
    else iterPos += this_ret;

    num_ret += this_ret;
  }
  while ((int)idx < count && num_ret < chunkSize && 
         iterPos && CN::Dimension(TYPE_FROM_HANDLE(iterPos)) == entDimension);

  if (!iterPos || CN::Dimension(TYPE_FROM_HANDLE(iterPos)) != entDimension) atend = true;

  return MB_SUCCESS;
}

ErrorCode RangeSetIterator::reset() 
{
  iterPos = 0; 
  return MB_SUCCESS;
}
  
ErrorCode VectorSetIterator::get_next_arr(std::vector<EntityHandle> &arr,
                                          bool &atend)
{
  int count;
  const EntityHandle *ptr;
  WriteUtilIface *iface;
  Interface *mbImpl = dynamic_cast<Interface*>(myCore);
  ErrorCode rval = mbImpl->query_interface(iface);
  if (MB_SUCCESS != rval) return rval;
  
  rval = iface->get_entity_list_pointers( &entSet, 1, &ptr, WriteUtilIface::CONTENTS, &count);
  if (MB_SUCCESS != rval) return rval;
  mbImpl->release_interface(iface);
  
  if (!count || iterPos >= count) {
    atend = true;
    return MB_SUCCESS;
  }
  
  std::vector<EntityHandle> tmp_arr;
  std::vector<EntityHandle> *tmp_ptr = &arr;
  if (checkValid) tmp_ptr = &tmp_arr;

    // just get the next chunkSize entities, or as many as you can
  int this_ct = 0;
  while (this_ct < (int)chunkSize && iterPos < count) {
    if ((MBMAXTYPE == entType || TYPE_FROM_HANDLE(ptr[iterPos]) == entType) &&
        (-1 == entDimension || CN::Dimension(TYPE_FROM_HANDLE(ptr[iterPos])) == entDimension)) {
      arr.push_back(ptr[iterPos]);
      this_ct++;
    }
    iterPos++;
  }
  
  atend = (iterPos == count);

  if (checkValid) {
    for (std::vector<EntityHandle>::iterator vit = tmp_ptr->begin(); vit != tmp_ptr->end(); vit++) {
      if (myCore->is_valid(*vit)) arr.push_back(*vit);
    }
  }

    // step along list, adding entities
  return MB_SUCCESS;
}

ErrorCode VectorSetIterator::reset() 
{
  iterPos = 0; 
  return MB_SUCCESS;
}
  
}
