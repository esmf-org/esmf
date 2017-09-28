/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 *
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 */

// Contributed by Lorenzo Alessio Botti (SpaFEDTe)
// This implementation is mostly borrowed from the mbzoltan MOAB partitioning tool

#ifndef __metispartitioner_hpp__
#define __metispartitioner_hpp__

#include <stdlib.h>
#include "moab/PartitionerBase.hpp"
#include "metis.h"

namespace moab {

  class Interface;
  class Range;
}

using namespace moab;

  class MetisPartitioner : public PartitionerBase<idx_t>
  {

  public:
    MetisPartitioner( Interface *impl = NULL,
                          const bool use_coords = false,
                          int argc = 0, 
                          char **argv = NULL);
    
    virtual ~MetisPartitioner();

    virtual ErrorCode partition_mesh_and_geometry(const double part_geom_mesh_size,
                                                  const idx_t nparts,
                                                  const char *zmethod,
                                                  const char *other_method,
                                                  double imbal_tol,
                                                  const int part_dim = 3,
                                                  const bool write_as_sets = true,
                                                  const bool write_as_tags = false,
                                                  const int obj_weight = 0,
                                                  const int edge_weight = 0,
                                                  const bool part_surf = false,
                                                  const bool ghost = false,
                                                  const bool spherical_coords = false,
                                                  const bool print_time = false);

    virtual ErrorCode partition_mesh( const idx_t nparts,
                                      const char *method,
                                      const int part_dim = 3, 
                                      const bool write_as_sets = true,
                                      const bool write_as_tags = false,
                                      const bool partition_tagged_sets = false,
                                      const bool partition_tagged_ents = false,
                                      const char *aggregating_tag = NULL,
                                      const bool print_time=false);

    virtual ErrorCode write_partition(const idx_t nparts, Range &elems, 
                                const idx_t *assignment,
                                const bool write_as_sets,
                                const bool write_as_tags);
    
    ErrorCode write_aggregationtag_partition(const idx_t nparts, Range &elems, 
                                             const idx_t *assignment,
                                             const bool write_as_sets,
                                             const bool write_as_tags);

      // put closure of entities in the part sets too
    virtual ErrorCode include_closure();

    // virtual ErrorCode write_file(const char *filename, const char *out_file);
  
  private:

    int argcArg;
    
    char **argvArg;

    ErrorCode assemble_graph(const int dimension, 
                             std::vector<double> &coords,
                             std::vector<idx_t> &moab_ids,
                             std::vector<idx_t> &adjacencies, 
                             std::vector<idx_t> &length,
                             Range &elems);
    
    ErrorCode assemble_taggedsets_graph(const int dimension, 
                                        std::vector<double> &coords,
                                        std::vector<idx_t> &moab_ids,
                                        std::vector<idx_t> &adjacencies, 
                                        std::vector<idx_t> &length,
                                        Range &elems,
                                        const char *aggregating_tag);
    
    ErrorCode assemble_taggedents_graph(const int dimension, 
                                        std::vector<double> &coords,
                                        std::vector<idx_t> &moab_ids,
                                        std::vector<idx_t> &adjacencies, 
                                        std::vector<idx_t> &length,
                                        Range &elems,
                                        const char *aggregating_tag);
  };

// Inline functions

inline
ErrorCode MetisPartitioner::partition_mesh_and_geometry(const double ,
                                                  const idx_t nparts,
                                                  const char *zmethod,
                                                  const char *,
                                                  double ,
                                                  const int part_dim,
                                                  const bool write_as_sets,
                                                  const bool write_as_tags,
                                                  const int ,
                                                  const int ,
                                                  const bool ,
                                                  const bool ,
                                                  const bool ,
                                                  const bool print_time)
{
  // Only partition the mesh - no geometric partition available
  return partition_mesh( nparts, zmethod, part_dim, write_as_sets, write_as_tags, false, false, NULL, print_time);
}

inline
ErrorCode MetisPartitioner::include_closure()
{
  return MB_NOT_IMPLEMENTED;
}

#endif

