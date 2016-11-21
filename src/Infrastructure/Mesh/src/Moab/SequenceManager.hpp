#ifndef SEQUENCE_MANAGER_HPP
#define SEQUENCE_MANAGER_HPP

#include "TypeSequenceManager.hpp"
#include "TagInfo.hpp"
#include <vector>

namespace moab {

class HomCoord;
class Error;

class SequenceManager 
{
  public:
    
    SequenceManager(double default_seq_multiplier = 1.0) : sequence_multiplier(default_seq_multiplier)
    { }

    ~SequenceManager();
    
      /** Delete all contained data */
    void clear();
    
      /** Find entity sequence containing specified handle.
       *\return MB_SUCCESS or MB_ENTITY_NOT_FOUND
       */
    ErrorCode find( EntityHandle handle, EntitySequence*& sequence_out )
      { 
        return typeData[TYPE_FROM_HANDLE(handle)].find( handle, sequence_out );
      }
    
      /** Find entity sequence containing specified handle.
       *\return MB_SUCCESS or MB_ENTITY_NOT_FOUND
       */
    ErrorCode find( EntityHandle handle, const EntitySequence*& sequence_out ) const
      { 
        return typeData[TYPE_FROM_HANDLE(handle)].find( handle, sequence_out );
      }
    
      /** Get all entities of a given EntityType, return all entities
       *  if type == MBMAXTYPE */
    void get_entities( EntityType type, Range& entities_out ) const
      { 
        if (type == MBMAXTYPE)
          get_entities( entities_out );
        else
          typeData[type].get_entities( entities_out ); 
      }

    void get_entities( Range& entities_out ) const;
    
      /** Get all entities of a given EntityType, return all entities
       *  if type == MBMAXTYPE */
    void get_entities( EntityType type, std::vector<EntityHandle>& entities_out ) const
      { 
        if (type == MBMAXTYPE)
          get_entities( entities_out );
        else
          typeData[type].get_entities( entities_out ); 
      }
    
    void get_entities( std::vector<EntityHandle>& entities_out ) const;
    
      /** Count entities of a given EntityType */
    EntityID get_number_entities( EntityType type ) const
      { return type == MBMAXTYPE ? get_number_entities() : typeData[type].get_number_entities(); }
    
      /** Count entities of a given EntityType */
    EntityID get_number_entities( ) const;
      
      /** Get most recently accessed sequence for a given type */
    const EntitySequence* get_last_accessed_sequence( EntityType type ) const
      { return typeData[type].get_last_accessed(); }
    
      /**\brief Replace subset of existing sequence with new 
       *        sequence (splits existing sequence)
       *
       * Used for converting number of nodes for fixed-connectivity-length
       * elements.  Input sequence must be a non-strict subset of an existing
       * sequence.  Existing sequence will be removed, modified, or split
       * into two prevent it from overlapping the new sequence.
       */
    ErrorCode replace_subsequence( EntitySequence* new_seq );
    
      /** Check if passed entity handles are valid */
    ErrorCode check_valid_entities( Error* error_handler, 
                                    const Range& entities ) const;
    
      /** Check if passed entity handles are valid 
       *\param root_set_okay  If true, do not returnan error if the passed
       *                      array contains one or more zero-valued handles 
       */
    ErrorCode check_valid_entities( Error* error_handler,
                                    const EntityHandle entities[],
                                    size_t num_entities,
                                    bool root_set_okay = false ) const;
    
      /** Delete an entity.  Deletes sequence if only contained entity. */
    ErrorCode delete_entity( Error* error_handler, EntityHandle entity );
    
      /** Delete entities */
    ErrorCode delete_entities( Error* error_handler, const Range& entities );
    
      /** Allocate a vertex (possibly in an existing sequence) and 
       *  assign it the passed coordinate values.
       */
    ErrorCode create_vertex( const double coords[3],
                               EntityHandle& handle_out );
    
      /** Allocate a element (possibly in an existing sequence) and 
       *  assign it the passed connectivity.
       */
    ErrorCode create_element( EntityType type,
                                const EntityHandle* conn_array,
                                unsigned num_vertices,
                                EntityHandle& handle_out );
    
      /** Allocate an entity set (possibly in an existing sequence) */
    ErrorCode create_mesh_set( unsigned flags,
                                 EntityHandle& handle_out );
      /** Allocate an entity set with the specified handle. 
       *\return MB_ALREADY_ALLOCATED if handle is in use, MB_SUCCESS otherwise.
       */
    ErrorCode allocate_mesh_set( EntityHandle at_this_handle,
                                   unsigned flags );
    
      /**\brief Allocate a block of consecutive entity handles
       *
       * Allocate a block of consecutive entity handles.  Handles
       * may be appended or prepended to an existing entity sequence.
       *\param type The type of of entity for which to allocate handles
       *\param num_entities Number of entities to allocate
       *\param nodes_per_entity Number of nodes in connectivity for elements,
       *                    ignored MBVERTEX, MBPOLYGON, MBPOLYHEDRON, and
       *                    MBENTITYSET types.
       *\param start_id_hint Preferred ID portion for first handle.  
       *                    May be ignored if not available.
       *\param first_handle_out First allocated handle.  Allocated handles
       *                    are [first_handle_out, first_handle_out+num_entities-1].
       *\param sequence_out The sequence in which the entities were allocated.
       *                    NOTE: first_handle_out may not be first handle in
       *                    sequence.
       *\param sequence_size If specified, allocate this sequence size instead of DEFAULT_***_SEQUENCE_SIZE
       */
    ErrorCode create_entity_sequence( EntityType type,
                                      EntityID num_entities,
                                      int nodes_per_entity,
                                      EntityID start_id_hint,
                                      EntityHandle& first_handle_out,
                                      EntitySequence*& sequence_out,
                                      int sequence_size);
    
       /**\brief Allocate a block of consecutive mesh sets
       *
       * Allocate a block of consecutive entity handles.  Handles
       * may be appended or prepended to an existing entity sequence.
       *\param type The type of of entity for which to allocate handles
       *\param num_sets     Number of entities to allocate
       *\param start_id_hint Preferred ID portion for first handle.  
       *                    May be ignored if not available.
       *\param processor_id Processor ID to embed in handles
       *\param flags        Array of length 'num_sets' containing entity set
       *                    creating flags.
       *\param first_handle_out First allocated handle.  Allocated handles
       *                    are [first_handle_out, first_handle_out+num_entities-1].
       *\param sequence_out The sequence in which the entities were allocated.
       *                    NOTE: first_handle_out may not be first handle in
       *                    sequence.
       */
    ErrorCode create_meshset_sequence( EntityID num_sets,
                                         EntityID start_id_hint,
                                         const unsigned* flags,
                                         EntityHandle& first_handle_out,
                                         EntitySequence*& sequence_out );
    
      /**\brief Allocate a block of consecutive mesh sets
       *
       * Alternate form that creates all mesh sets with same flags.
       */
    ErrorCode create_meshset_sequence( EntityID num_sets,
                                         EntityID start_id_hint,
                                         unsigned flags,
                                         EntityHandle& first_handle_out,
                                         EntitySequence*& sequence_out );
    
      /** Create structured mesh */
    ErrorCode create_scd_sequence( int imin, int jmin, int kmin,
                                     int imax, int jmax, int kmax,
                                     EntityType type,
                                     EntityID start_id_hint,
                                     EntityHandle& first_handle_out,
                                   EntitySequence*& sequence_out,
                                   int *is_periodic = NULL);
    
      /** Create structured mesh */
    ErrorCode create_scd_sequence( const HomCoord& coord_min,
                                     const HomCoord& coord_max,
                                     EntityType type,
                                     EntityID start_id_hint,
                                     EntityHandle& first_handle_out,
                                     EntitySequence*& sequence_out,
                                   int *is_periodic = NULL);

      /** Create swept mesh */
    ErrorCode create_sweep_sequence( int imin, int jmin, int kmin,
				       int imax, int jmax, int kmax,
				       int* Cq,
				       EntityType type,
				       EntityID start_id_hint,
				       EntityHandle& first_handle_out,
				       EntitySequence*& sequence_out );
    
      /** Create swept mesh */
    ErrorCode create_sweep_sequence( const HomCoord& coord_min,
				       const HomCoord& coord_max,
				       int* Cq,
				       EntityType type,
				       EntityID start_id_hint,
				       EntityHandle& first_handle_out,
				       EntitySequence*& sequence_out );

    /** Add a structured vertex sequence to this structured element sequence;
     * see comments in ScdElementData */
  ErrorCode add_vsequence(EntitySequence *vert_seq,
                            EntitySequence *elem_seq,
                            const HomCoord &p1, const HomCoord &q1,
                            const HomCoord &p2, const HomCoord &q2,
                            const HomCoord &p3, const HomCoord &q3,
                            bool bb_input = false,
                            const HomCoord *bb_min = NULL,
                            const HomCoord *bb_max = NULL);

      /** Get data for a specific EntityType */
    TypeSequenceManager& entity_map( EntityType type )
      { return typeData[type]; }
    
      /** Get data for a specific EntityType */
    const TypeSequenceManager& entity_map( EntityType type ) const
      { return typeData[type]; }
    
    void get_memory_use( unsigned long long& total_entity_storage,
                         unsigned long long& total_storage ) const;
                         
    void get_memory_use( EntityType type,
                         unsigned long long& total_entity_storage,
                         unsigned long long& total_storage ) const;
    
    void get_memory_use( const Range& entities,
                         unsigned long long& total_entity_storage,
                         unsigned long long& total_amortized_storage ) const;
    
  
  
    /* Dense Tag Functions */
    
      /** Allocate a tag ID
       *\param tag_size The size of the tag value for each entity
       */
    ErrorCode reserve_tag_array( Error* error_handler, int tag_size, int& array_id_out );
    
      /** Release any storage assocociated with a tag ID, and optionally,
       *  release the reserved tag ID. */
    ErrorCode release_tag_array( Error* error_handler, int id, bool release_id );
    
      /**\brief Get default size of POLYGON and POLYHEDRON SequenceData */
    static EntityID default_poly_sequence_size( int entity_connectivity_length );
    
      /**\brief Size to allocate for new SquenceData 
       * THIS FUNCTION SHOULD ONLY BE CALLED WHEN ALLOCATING FROM ReadUtil IN BULK
       * (since it will allocate lesser of requested_size and default_size)
       * If sequence_size != -1, will try to allocate that, unless there isn't available
       * space
       */
    EntityID new_sequence_size( EntityHandle start_handle, 
                                EntityID requested_size,
                                int sequence_size) const;

    /** \brief Interface to control memory allocation for sequences
     * Provide a factor that controls the size of the sequence that gets allocated.
     * This is typically useful in the parallel setting when a-priori, the number of ghost entities
     * and the memory required for them within the same sequence as the owned entities are unknown.
     * The default factor is 1.0 but this can be appropriately updated at runtime so that we do not
     * have broken sequences.
     */
    double get_sequence_multiplier() const
    { return sequence_multiplier; }

    /** \brief Interface to control memory allocation for sequences
     * Provide a factor that controls the size of the sequence that gets allocated.
     * This is typically useful in the parallel setting when a-priori, the number of ghost entities
     * and the memory required for them within the same sequence as the owned entities are unknown.
     * The default factor is 1.0 but this can be appropriately updated at runtime so that we do not
     * have broken sequences.
     *
     * \param meshset User specified multiplier (should be greater than 1.0)
     */
    void set_sequence_multiplier(double factor)
    { sequence_multiplier = factor; }
  
    /**\brief Default allocation size for vertices */
  static const EntityID DEFAULT_VERTEX_SEQUENCE_SIZE;
  
    /**\brief Default allocation size for elements */
  static const EntityID DEFAULT_ELEMENT_SEQUENCE_SIZE;

    /**\brief Default allocation size for poly's */
  static const EntityID DEFAULT_POLY_SEQUENCE_SIZE;

    /**\brief Default allocation size for meshsets */
  static const EntityID DEFAULT_MESHSET_SEQUENCE_SIZE;

  private:
   
    /**\brief Utility function for allocate_mesh_set (and similar)
     *
     * Given a block of available handles, determine the non-strict
     * subset at which to create a new EntitySequence.
     */
    void trim_sequence_block( EntityHandle start_handle,
                              EntityHandle& end_handle_in_out,
                              unsigned maximum_sequence_size );
  
  
      /**\brief Get range of handles in which to create an entity sequence
       *
       * Get range of handles in whcih to place a new entity sequence.
       *\param type              The EntityType for the contents of the sequence
       *\param entity_count      The number of entities in the range
       *\param values_per_entity Vertices per element, zero for other types
       *\param start_id_hint     Preferred id of first handle
       *\param processor_rank    MPI processor ID
       *\param data_out  Output: Either NULL or an existing SequenceData
       *                         with a sufficiently large block to accomodate 
       *                         the handle range.
       *\return zero if no available handle range, start handle otherwise.
       */
    EntityHandle sequence_start_handle( EntityType type,
                                          EntityID entity_count,
                                          int values_per_entity,
                                          EntityID start_id_hint,
                                          SequenceData*& data_out,
                                          EntityID &data_size );
  
    TypeSequenceManager typeData[MBMAXTYPE];
    
    std::vector<int> tagSizes;

     /**\brief The over-allocation factor for entities in a sequence (strictly >= 1.0) */
    double sequence_multiplier;

};

} // namespace moab

#endif
