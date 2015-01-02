#include "EntitySequence.hpp"
#include "SequenceData.hpp"

namespace moab {

bool EntitySequence::using_entire_data() const 
{
  return start_handle() == data()->start_handle()
        && end_handle() == data()->  end_handle();
}

int EntitySequence::values_per_entity() const
  { return 0; }

ErrorCode EntitySequence::pop_back( EntityID count )
{
  EntityHandle new_end = endHandle - count;
  if (new_end < startHandle)
    return MB_FAILURE;
  
  endHandle = new_end;
  return MB_SUCCESS;
}

ErrorCode EntitySequence::pop_front( EntityID count )
{
  EntityHandle new_start = startHandle + count;
  if (new_start > endHandle)
    return MB_FAILURE;
  
  startHandle = new_start;
  return MB_SUCCESS;
}


ErrorCode EntitySequence::prepend_entities( EntityID count )
{
  EntityHandle new_start = startHandle - count;
  if (new_start < data()->start_handle())
    return MB_FAILURE;
  
  startHandle = new_start;
  return MB_SUCCESS;
}
 
ErrorCode EntitySequence::append_entities( EntityID count )
{
  EntityHandle new_end = endHandle + count;
  if (new_end > data()->end_handle())
    return MB_FAILURE;
  
  endHandle = new_end;
  return MB_SUCCESS;
}

ErrorCode EntitySequence::merge( EntitySequence& other )
{
  if (sequenceData != other.sequenceData)
    return MB_FAILURE;
  if (end_handle() + 1 == other.start_handle()) {
    endHandle = other.end_handle();
    other.startHandle = other.end_handle()+1;
  }
  else if (start_handle() == other.end_handle() + 1) {
    startHandle = other.start_handle();
    other.endHandle = other.start_handle()-1;
  }
  else
    return MB_FAILURE;
  return MB_SUCCESS;
}

unsigned long EntitySequence::get_per_entity_memory_use( EntityHandle,
                                                         EntityHandle ) const
  { return 0; }
  
} // namespace moab
