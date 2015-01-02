/**
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

#ifndef AENTITY_FACTORY_HPP
#define AENTITY_FACTORY_HPP

#ifndef IS_BUILDING_MB
#error "AEntityFactory.hpp isn't supposed to be included into an application"
#endif

#include "moab/Forward.hpp"
#include <vector>

namespace moab {

typedef std::vector<EntityHandle> AdjacencyVector;
class Core;

//! class AEntityFactory
class AEntityFactory 
{
public:

  //! require an Interface object in order to access tags on that interface
  AEntityFactory(Core *mdb);

  //! destructor
  ~AEntityFactory();
  
//! add an adjacency from from_ent to to_ent; if both_ways is true, add one
//! in reverse too
//! NOTE: this function is defined even though we may only be implementing
//! vertex-based up-adjacencies
  ErrorCode add_adjacency(EntityHandle from_ent,
                             EntityHandle to_ent,
                             const bool both_ways = false);

//! remove an adjacency from from the base_entity.
  ErrorCode remove_adjacency(EntityHandle base_entity,
                                EntityHandle adjacency_to_remove);

//! remove all adjacencies from from the base_entity.
  ErrorCode remove_all_adjacencies(EntityHandle base_entity,
                                     const bool delete_adj_list = false);

/**\brief Get adjacencies for a single source entity.
 *
 * Get adjacent entities.
 *
 *\param source_entity    The entity for which to retrieve the adjacencies.
 *\param target_dimension Retrieve adjacent entities of this dimension.  Must
 *                        be in the range [1,3], where 4 is used to indicated entity sets.
 *\param target_entities  Requested adjacent entities will be appended to this list.
 *\param create_if_missing If true, adjacent elements of the specified dimension will
 *                        be created if they do not already exist.  If the target dimension
 *                        is less than the dimension of the input entity and greater than zero, the 
 *                        elements will be created as required to represent the "sides" of
 *                        the source element.  If the target dimension is greater than that
 *                        of the source entity and less than 3, then sides of the specified
 *                        dimension on that are a) of greater dimension and b) adjacent to
 *                        the input entity will be created.
 * \param create_adjacency_option If create_adjacency_option is >= 0, adjacencies from 
 *                        entities of that dimension to each target_entity are created 
 *                        (this function uses AEntityFactory::get_element for each element)
 */
  ErrorCode get_elements(EntityHandle source_entity,
                            const unsigned int target_dimension,
                            std::vector<EntityHandle> &target_entities,
                            const bool create_if_missing,
                            const int create_adjacency_option = -1);

    //! get the vertices for a polyhedron (special implementation because for polyhedra
    //! connectivity array stores faces)
  ErrorCode get_polyhedron_vertices(const EntityHandle source_entity, 
                                      std::vector<EntityHandle> &target_entities);
  
//! get the meshsets that are in source_entitiy's adjacency vector
  ErrorCode get_associated_meshsets( EntityHandle source_entity, 
                                        std::vector<EntityHandle> &target_entities );

//! get the element defined by the vertices in vertex_list, of the
//! type target_type, passing back in target_entity; if create_if_missing
//! is true and no entity is found, one is created; if create_adjacency_option
//! is >= 0, adjacencies from entities of that dimension to target_entity
//! are created (only create_adjacency_option=0 is supported right now,
//! so that never creates other ancillary entities); explicitly require
//! the vertex_list_size for consistency, even though we could probably get
//! it from target_type
  ErrorCode get_element(const EntityHandle *vertex_list,
                           const int vertex_list_size,
                           const EntityType target_type,
                           EntityHandle &target_entity,
                           const bool create_if_missing,
                           const EntityHandle source_entity = 0,
                           const int create_adjacency_option = -1);

  /**\brief Get adjacent entities
   *
   *\param entity            The source entity for which to retrieve adjacent entities.
   *\param to_dimension      The adjacent entities to retrieve, specified by dimension.
   *\param create_if_missing Create adjacent entities that do not already exist.
   *\param adjacent_entities The resulting adjacent entities are appended to this 
   *                         list.
   */
  ErrorCode get_adjacencies(const EntityHandle entity,
                               const unsigned int to_dimension,
                               bool create_if_missing,
                               std::vector<EntityHandle> &adjacent_entities);

    //! return const array * for adjacencies
  ErrorCode get_adjacencies(EntityHandle entity,
                               const EntityHandle *&adjacent_entities,
                               int &num_entities) const;
                               
  ErrorCode get_adjacencies( EntityHandle entity,
                               std::vector<EntityHandle>*& adj_vec_ptr_out,
                               bool create_if_missing = false );
  
  //! returns the entities in sorted order
  ErrorCode get_adjacencies(EntityHandle entity,
                               std::vector<EntityHandle>& adjacent_entities) const;
  

  //! creates vertex to element adjacency information
  ErrorCode create_vert_elem_adjacencies();

  //! returns whether vertex to element adjacencies are being stored
  bool vert_elem_adjacencies() const { return mVertElemAdj; }

  //! calling code notifying this that an entity is getting deleted
  ErrorCode notify_delete_entity(EntityHandle entity);

  //! calling code notifying this that to update connectivity of 'entity' 
  ErrorCode notify_create_entity(const EntityHandle entity, 
                                    const EntityHandle *node_array,
                                    const int number_nodes);

  //! calling code notifying that an entity changed its connectivity
  ErrorCode notify_change_connectivity(EntityHandle entity, 
                                          const EntityHandle* old_array,
                                          const EntityHandle* new_array, int number_nodes);

    //! return true if 2 entities are explicitly adjacent
  bool explicitly_adjacent(const EntityHandle ent1,
                           const EntityHandle ent2);

    //! in preparation for merging two entities, adjust adjacencies so that
    //! entity_to_keep will be adjacent to the "right" entities after merge
    //! (also checks for potential formation of equivalent entities and 
    //! creates explicit adjacencies accordingly)
  ErrorCode merge_adjust_adjacencies(EntityHandle entity_to_keep,
                                       EntityHandle entity_to_remove);
  
  void get_memory_use( unsigned long& total_entity_storage,
                       unsigned long& total_storage );
  ErrorCode get_memory_use( const Range& entities,
                              unsigned long& total_entity_storage,
                              unsigned long& total_amortized_storage );
  
private:

  ErrorCode get_adjacency_ptr( EntityHandle, std::vector<EntityHandle>*& );
  ErrorCode get_adjacency_ptr( EntityHandle, const std::vector<EntityHandle>*& ) const;
  ErrorCode set_adjacency_ptr( EntityHandle, std::vector<EntityHandle>* );
  
  ErrorCode get_vertices( EntityHandle h,
                            const EntityHandle*& vect_out,
                            int& count_out,
                            std::vector<EntityHandle>& storage );

  //! private constructor to prevent the construction of a default one
  AEntityFactory();

  //! interface associated with this tool
  Core *thisMB;

  //! whether vertex to element adjacencies are begin done
  bool mVertElemAdj;
  
  //! compare vertex_list to the vertices in this_entity, 
  //!  and return true if they contain the same vertices
  bool entities_equivalent(const EntityHandle this_entity, 
                           const EntityHandle *vertex_list, 
                           const int vertex_list_size,
                           const EntityType target_type);

  ErrorCode get_zero_to_n_elements(EntityHandle source_entity,
                            const unsigned int target_dimension,
                            std::vector<EntityHandle> &target_entities,
                            const bool create_if_missing,
                            const int create_adjacency_option = -1);

  ErrorCode get_down_adjacency_elements(EntityHandle source_entity,
                            const unsigned int target_dimension,
                            std::vector<EntityHandle> &target_entities,
                            const bool create_if_missing,
                            const int create_adjacency_option = -1);

  ErrorCode get_down_adjacency_elements_poly(EntityHandle source_entity,
                                               const unsigned int target_dimension,
                                               std::vector<EntityHandle> &target_entities,
                                               const bool create_if_missing,
                                               const int create_adjacency_option = -1);

  ErrorCode get_up_adjacency_elements(EntityHandle source_entity,
                            const unsigned int target_dimension,
                            std::vector<EntityHandle> &target_entities,
                            const bool create_if_missing,
                            const int create_adjacency_option = -1);

    //! check for equivalent entities that may be formed when merging two entities, and
    //! create explicit adjacencies accordingly
  ErrorCode check_equiv_entities(EntityHandle entity_to_keep,
                                   EntityHandle entity_to_remove);
  
    //! create explicit adjacencies between this_ent and all adjacent entities of higher
    //! dimension
  ErrorCode create_explicit_adjs(EntityHandle this_ent);
  
};
 
} // namespace moab

#endif
