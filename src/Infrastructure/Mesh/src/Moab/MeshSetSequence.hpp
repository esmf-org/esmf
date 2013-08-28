/*
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

/**\file MeshSetSequence.hpp
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2007-04-30
 */

#ifndef MESH_SET_SEQUENCE_HPP
#define MESH_SET_SEQUENCE_HPP

#include "EntitySequence.hpp"
#include "MeshSet.hpp"
#include "SequenceData.hpp"

namespace moab {

class SequenceManager;

class MeshSetSequence : public EntitySequence
{
public:

  MeshSetSequence( EntityHandle start,
                   EntityID count,
                   const unsigned* flags,
                   SequenceData* data );
  
  MeshSetSequence( EntityHandle start,
                   EntityID count,
                   unsigned flags,
                   SequenceData* data );

  MeshSetSequence( EntityHandle start,
                   EntityID count,
                   const unsigned* flags,
                   EntityID sequence_size );
  
  MeshSetSequence( EntityHandle start,
                   EntityID count,
                   unsigned flags,
                   EntityID sequence_size );

  virtual ~MeshSetSequence();

  EntitySequence* split( EntityHandle here );
  
  SequenceData* create_data_subset( EntityHandle, EntityHandle ) const
    { return 0; }
  
  ErrorCode pop_back( EntityID count );
  ErrorCode pop_front( EntityID count );
  ErrorCode push_back( EntityID count, const unsigned* flags );
  ErrorCode push_front( EntityID count, const unsigned* flags );
  
  void get_const_memory_use( unsigned long& bytes_per_entity,
                             unsigned long& size_of_sequence ) const;
  unsigned long get_per_entity_memory_use( EntityHandle first,
                                           EntityHandle last ) const;


  inline MeshSet* get_set( EntityHandle h );
  inline const MeshSet* get_set( EntityHandle h ) const;
  
  ErrorCode get_entities( EntityHandle set, std::vector<EntityHandle>& entities ) const;
  ErrorCode get_entities(  SequenceManager const* seqman, EntityHandle set,                    Range& entities, bool recursive ) const;
  ErrorCode get_dimension( SequenceManager const* seqman, EntityHandle set, int dim,           std::vector<EntityHandle>& entities, bool recursive ) const;
  ErrorCode get_dimension( SequenceManager const* seqman, EntityHandle set, int dim,           Range& entities, bool recursive ) const;
  ErrorCode get_type(      SequenceManager const* seqman, EntityHandle set, EntityType type, std::vector<EntityHandle>& entities, bool recursive ) const;
  ErrorCode get_type(      SequenceManager const* seqman, EntityHandle set, EntityType type, Range& entities, bool recursive ) const;
  
  ErrorCode num_entities(  SequenceManager const* seqman, EntityHandle set,                    int& count, bool recursive ) const;
  ErrorCode num_dimension( SequenceManager const* seqman, EntityHandle set, int dim,           int& count, bool recursive ) const;
  ErrorCode num_type(      SequenceManager const* seqman, EntityHandle set, EntityType type, int& count, bool recursive ) const;

  ErrorCode get_parents       ( SequenceManager const* seqman, EntityHandle of, std::vector<EntityHandle>& parents,  int num_hops ) const;
  ErrorCode get_children      ( SequenceManager const* seqman, EntityHandle of, std::vector<EntityHandle>& children, int num_hops ) const;
  ErrorCode get_contained_sets( SequenceManager const* seqman, EntityHandle of, std::vector<EntityHandle>& contents, int num_hops ) const;
  ErrorCode num_parents       ( SequenceManager const* seqman, EntityHandle of, int& number, int num_hops ) const;
  ErrorCode num_children      ( SequenceManager const* seqman, EntityHandle of, int& number, int num_hops ) const;
  ErrorCode num_contained_sets( SequenceManager const* seqman, EntityHandle of, int& number, int num_hops ) const;
  
private:

  enum SearchType { PARENTS, CHILDREN, CONTAINED };

  MeshSetSequence( MeshSetSequence& split_from, EntityHandle split_at )
    : EntitySequence( split_from, split_at )
    {}

  void initialize( const unsigned* set_flags );
  
  ErrorCode get_parent_child_meshsets( EntityHandle meshset,
                                    SequenceManager const* set_sequences,
                                    std::vector<EntityHandle>& results,
                                    int num_hops, SearchType link_type ) const;
                                    
  static ErrorCode recursive_get_sets( EntityHandle start_set,
                            SequenceManager const* set_sequences,
                            std::vector<const MeshSet*>* sets_out = 0,
                            Range* set_handles_out = 0,
                            std::vector<EntityHandle>* set_handle_vect_out = 0 );
  static ErrorCode recursive_get_sets( EntityHandle start_set,
                            SequenceManager* set_sequences,
                            std::vector<MeshSet*>& sets_out );
  
  enum { SET_SIZE = sizeof(MeshSet) };

  inline const unsigned char* array() const
    { return reinterpret_cast<const unsigned char*>(data()->get_sequence_data(0)); }

  inline unsigned char* array()
    { return reinterpret_cast<unsigned char*>(data()->get_sequence_data(0)); }
    
  inline void allocate_set( unsigned flags, EntityID index )
  {
    unsigned char* const ptr = array() + index * SET_SIZE;
    new (ptr) MeshSet(flags);
  }
    
  inline void deallocate_set( EntityID index ) 
  {
    MeshSet* set = reinterpret_cast<MeshSet*>(array() + SET_SIZE * index );
    set->~MeshSet();
  }
};

inline MeshSet* MeshSetSequence::get_set( EntityHandle h )
{
  return reinterpret_cast<MeshSet*>(array() + SET_SIZE*(h - data()->start_handle()));
}
inline const MeshSet* MeshSetSequence::get_set( EntityHandle h ) const
{
  return reinterpret_cast<const MeshSet*>(array() + SET_SIZE*(h - data()->start_handle()));
}
  
} // namespace moab

#endif
