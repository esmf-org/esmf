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


#ifndef MB_READ_UTIL_HPP
#define MB_READ_UTIL_HPP

#ifndef IS_BUILDING_MB
#error "ReadUtil.hpp isn't supposed to be included into an application"
#endif

#include "moab/ReadUtilIface.hpp"

namespace moab {

class Core;
class Error;

class ReadUtil : public ReadUtilIface
{
private:
  //! pointer to the Core
  Core* mMB;
  Error* mError;
public:

  //! constructor takes Core pointer
  ReadUtil(Core* mdb, Error* error_handler);

  //! destructor
  ~ReadUtil(){}

  //! gets arrays for coordinate data from the MB
  ErrorCode get_node_coords(
      const int num_arrays,
      const int num_nodes, 
      const int preferred_start_id,
      EntityHandle& actual_start_handle, 
      std::vector<double*>& arrays,
      int sequence_size = -1);

  //! get array for connectivity data from the MB
  ErrorCode get_element_connect(
      const int num_elements, 
      const int verts_per_element,
      const EntityType mdb_type,
      const int preferred_start_id, 
      EntityHandle& actual_start_handle, 
      EntityHandle*& array,
      int sequence_size = -1
      );

    /**
     *\brief Gather entities related to those in the partition
     * Gather entities related to those in the input partition.  Related
     * means down-adjacent to, contained in, etc.
     * \param partition Entities for which to gather related entities
     * \param related_ents Related entities
     * \param all_sets If non-NULL, all sets in mesh instance are returned
     * in the pointed-to range
     */
  ErrorCode gather_related_ents(Range &partition,
                                Range &related_ents,
                                EntityHandle *file_set = NULL);
  
  ErrorCode create_entity_sets(
    EntityID num_sets,
    const unsigned* set_flags,
    EntityID preffered_start_id,
    EntityHandle& actual_start_handle
  );
 
  //! tell MB which elements have been added to the database
  ErrorCode update_adjacencies(
      const EntityHandle start_handle,
      const int number_elements,
      const int number_vertices_per_element,
      const EntityHandle* conn_array);

  //! tell MB there was an error when reading the mesh
  //! it makes sense to have this as long as Interface has a load_mesh function
  ErrorCode report_error( const std::string& error );

  ErrorCode report_error( const char* error, ... ) MB_PRINTF(1);

    //! given an ordered list of bounding entities and the sense of
    //! those entities, return an ordered list of vertices
  ErrorCode get_ordered_vertices(EntityHandle *bound_ents, 
                                   int *sense, 
                                   int num_bound,
                                   int dim,
                                   EntityHandle *bound_verts, 
                                   EntityType &etype);


  ErrorCode assign_ids( Tag id_tag, const Range& ents, int start = 0 );
  ErrorCode assign_ids( Tag id_tag, const EntityHandle* ents, 
                          size_t num_ents, int start = 0 );
};
  
} // namespace moab

#endif


