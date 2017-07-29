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

#include <iostream>
#include <assert.h>
#include <sstream>
#include <map>
#include <ctime>

#include "moab/MetisPartitioner.hpp"
#include "moab/Interface.hpp"
#include "Internals.hpp"
#include "moab/Range.hpp"
#include "moab/WriteUtilIface.hpp"
#include "moab/MeshTopoUtil.hpp"
#include "moab/Skinner.hpp"
#include "MBTagConventions.hpp"
#include "moab/CN.hpp"

using namespace moab;

const bool debug = false;

MetisPartitioner::MetisPartitioner( Interface *impl, 
                                    const bool use_coords,
                                    int argc, 
                                    char **argv) 
                                  : PartitionerBase(impl,use_coords), 
                                             argcArg(argc), 
                                             argvArg(argv)
{
}

MetisPartitioner::~MetisPartitioner() 
{
}

ErrorCode MetisPartitioner::partition_mesh(const idx_t nparts,
                                            const char *method,
                                            const int part_dim,
                                            const bool write_as_sets,
                                            const bool write_as_tags,
                                            const bool partition_tagged_sets,
                                            const bool partition_tagged_ents,
                                            const char *aggregating_tag,
                                            const bool print_time)
{
#ifdef MOAB_HAVE_MPI
    // should only be called in serial
  if (mbpc->proc_config().proc_size() != 1) {
    std::cout << "MetisPartitioner::partition_mesh_and_geometry must be called in serial." 
              << std::endl;
    return MB_FAILURE;
  }
#endif

  if (NULL != method && strcmp(method, "ML_RB") && strcmp(method, "ML_KWAY"))
  {
    std::cout << "ERROR: Method must be "
              << "ML_RB or ML_KWAY"
              << std::endl;
    return MB_FAILURE;
  }
  
  std::vector<double> pts; // x[0], y[0], z[0], ... from MOAB
  std::vector<idx_t> ids; // poidx_t ids from MOAB
  std::vector<idx_t> adjs, parts;
  std::vector<idx_t> length;
  Range elems;
  // Get a mesh from MOAB and diide it across processors.

  clock_t t = clock();
  
  ErrorCode result;
  if (!partition_tagged_sets && !partition_tagged_ents)
  {
    result = assemble_graph(part_dim, pts, ids, adjs, length, elems);MB_CHK_ERR(result);
  }
  else if (partition_tagged_sets) 
  {
    result = assemble_taggedsets_graph(part_dim, pts, ids, adjs, length, elems, &(*aggregating_tag));MB_CHK_ERR(result);
  }
  else if (partition_tagged_ents) 
  {
    result = assemble_taggedents_graph(part_dim, pts, ids, adjs, length, elems, &(*aggregating_tag));MB_CHK_ERR(result);
  }
  else {
    MB_SET_ERR(MB_FAILURE, "Either partition tags or sets for Metis partitoner");
  }

  if (print_time)
  {
    std::cout << " time to assemble graph: " << (clock() - t) / (double) CLOCKS_PER_SEC  << "s. \n";
    t = clock();
  }

  std::cout << "Computing partition using " << method 
            <<" method for " << nparts << " processors..." << std::endl;

  idx_t nelems = length.size()-1;
  idx_t *assign_parts;
  assign_parts = (idx_t *)malloc(sizeof(idx_t) * nelems);
  idx_t nconstraidx_ts = 1;
  idx_t edgeCut = 0;
  idx_t nOfPartitions = static_cast<idx_t>(nparts);
  idx_t metis_RESULT;

  if (strcmp(method, "ML_KWAY") == 0)
  {
    idx_t options[METIS_NOPTIONS];
    METIS_SetDefaultOptions(options);
    options[METIS_OPTION_CONTIG] = 1;  
    metis_RESULT = METIS_PartGraphKway(&nelems, &nconstraidx_ts, &length[0], &adjs[0], NULL, NULL, NULL, &nOfPartitions, NULL, NULL, options, &edgeCut, assign_parts);
  }
  else if (strcmp(method, "ML_RB") == 0)
  {
    idx_t options[METIS_NOPTIONS];
    METIS_SetDefaultOptions(options);
    options[METIS_OPTION_OBJTYPE] = METIS_OBJTYPE_CUT; // CUT 
    options[METIS_OPTION_IPTYPE] = METIS_IPTYPE_GROW; // GROW or RANDOM
    options[METIS_OPTION_CTYPE] = METIS_CTYPE_RM; // RM or SHEM
    options[METIS_OPTION_RTYPE] = METIS_RTYPE_FM; // FM
    options[METIS_OPTION_NCUTS] = 10; // Number of different partitionings to compute, then chooses the best one, default = 1
    options[METIS_OPTION_NITER] = 10;  // Number of refinements steps, default = 10
    options[METIS_OPTION_UFACTOR] = 30; // Imabalance, default = 1
    options[METIS_OPTION_DBGLVL] = METIS_DBG_INFO;
    metis_RESULT = METIS_PartGraphRecursive(&nelems, &nconstraidx_ts, &length[0], &adjs[0], NULL, NULL, NULL, &nOfPartitions, NULL, NULL, options, &edgeCut, assign_parts);
  }
  else
    MB_SET_ERR(MB_FAILURE, "Either ML_KWAY or ML_RB needs to be specified for Metis partitioner");

  if (print_time)
  {
    std::cout << " time to partition: " << (clock() - t) / (double) CLOCKS_PER_SEC  << "s. \n";
    t = clock();
  }

#ifdef MOAB_HAVE_MPI
    // assign global node ids, starting from one! TODO
  result = mbpc->assign_global_ids(0, 0, 1);MB_CHK_ERR(result);
#endif

  if (metis_RESULT != METIS_OK)
    return MB_FAILURE;
  
  // take results & write onto MOAB partition sets
  std::cout << "Saving partition information to MOAB..." << std::endl;
  {
    if (partition_tagged_sets || partition_tagged_ents) {
      result = write_aggregationtag_partition(nparts, elems, assign_parts,
                                              write_as_sets, write_as_tags);MB_CHK_ERR(result);
    }
    else {
      result = write_partition(nparts, elems, assign_parts,
                              write_as_sets, write_as_tags);MB_CHK_ERR(result);
    }
  }

  if (print_time)
  {
    std::cout << " time to write partition in memory " <<(clock() - t) / (double) CLOCKS_PER_SEC  << "s. \n";
    t = clock();
  }
  free(assign_parts);

  return MB_SUCCESS;
}

ErrorCode MetisPartitioner::assemble_taggedents_graph(const int dimension,
                                                          std::vector<double> &coords,
                                                          std::vector<idx_t> &moab_ids,
                                                          std::vector<idx_t> &adjacencies, 
                                                          std::vector<idx_t> &length,
                                                          Range &elems,
					                  const char *aggregating_tag)
{
  Tag partSetTag;
  ErrorCode result = mbImpl->tag_get_handle(aggregating_tag, 1, MB_TYPE_INTEGER, partSetTag);
  if (MB_SUCCESS != result) return result;
 
  Range allSubElems;
  result = mbImpl->get_entities_by_dimension(0, dimension, allSubElems);
  if (MB_SUCCESS != result || allSubElems.empty()) return result;
  idx_t partSet;
  std::map<idx_t, Range> aggloElems;
  for (Range::iterator rit = allSubElems.begin(); rit != allSubElems.end(); rit++) 
  {
    EntityHandle entity = *rit;
    result = mbImpl->tag_get_data(partSetTag,&entity,1,&partSet);
    if (MB_SUCCESS != result) return result;
    if (partSet >= 0)
      aggloElems[partSet].insert(entity);
  }
  // clear aggregating tag data
  TagType type;
  result = mbImpl->tag_get_type(partSetTag, type);
  if (type == MB_TAG_DENSE)
  {
    // clear tag on ents and sets
    result = mbImpl->tag_delete(partSetTag); 
    if (MB_SUCCESS != result) return result;
  }
  if (type == MB_TAG_SPARSE)
  {
    // clear tag on ents
    result = mbImpl->tag_delete_data(partSetTag, allSubElems); 
    if (MB_SUCCESS != result) return result;
    // clear tag on sets
    result = mbImpl->get_entities_by_type_and_tag(0 , MBENTITYSET, &partSetTag, 0, 1, elems);
    if (MB_SUCCESS != result) return result;
    result = mbImpl->tag_delete_data(partSetTag, elems); 
    if (MB_SUCCESS != result) return result;
    elems.clear();
  }
  result = mbImpl->tag_get_handle("PARALLEL_PARTITION", 1, MB_TYPE_INTEGER,
                                  partSetTag, MB_TAG_SPARSE|MB_TAG_CREAT); 
  if (MB_SUCCESS != result) return result;
  
  for (std::map<idx_t, Range>::iterator mit = aggloElems.begin(); mit != aggloElems.end(); mit++) 
  {
    EntityHandle new_set;
    result = mbImpl->create_meshset(MESHSET_SET, new_set);
    if (MB_SUCCESS != result) return result;
    result = mbImpl->add_entities(new_set, mit->second);
    if (MB_SUCCESS != result) return result;
    result = mbImpl->tag_set_data (partSetTag, &new_set, 1, &mit->first);
    if (MB_SUCCESS != result) return result;
  }

  result = assemble_taggedsets_graph(dimension, coords, moab_ids, adjacencies, length, elems, &(*aggregating_tag));
  return MB_SUCCESS;
}

ErrorCode MetisPartitioner::assemble_taggedsets_graph(const int dimension,
                                                          std::vector<double> &coords,
                                                          std::vector<idx_t> &moab_ids,
                                                          std::vector<idx_t> &adjacencies, 
                                                          std::vector<idx_t> &length,
                                                          Range &elems,
					                  const char *aggregating_tag)
{
  length.push_back(0);
    // assemble a graph with vertices equal to elements of specified dimension, edges
    // signified by list of other elements to which an element is connected

  // get the tagged elements 
  Tag partSetTag;
  ErrorCode result = mbImpl->tag_get_handle(aggregating_tag, 1, MB_TYPE_INTEGER, partSetTag);MB_CHK_ERR(result);
  //ErrorCode result = mbImpl->tag_get_handle("PARALLEL_PARTITION_SET", 1, MB_TYPE_INTEGER, partSetTag);MB_CHK_ERR(result);

  result = mbImpl->get_entities_by_type_and_tag(0 , MBENTITYSET, &partSetTag, 0, 1, elems);
  if (MB_SUCCESS != result || elems.empty()) return result;

  //assign globla ids to elem sets based on aggregating_tag data 
  Tag gid_tag;
  idx_t zero1 = -1;
  result = mbImpl->tag_get_handle("GLOBAL_ID_AGGLO", 1, MB_TYPE_INTEGER, gid_tag, MB_TAG_SPARSE|MB_TAG_CREAT, &zero1);MB_CHK_ERR(result);
  for (Range::iterator rit = elems.begin(); rit != elems.end(); rit++) 
  {
    idx_t partSet;
    result = mbImpl->tag_get_data(partSetTag,&(*rit),1,&partSet);MB_CHK_ERR(result);
    result = mbImpl->tag_set_data(gid_tag, &(*rit), 1, &partSet);MB_CHK_ERR(result);
  }
  // clear aggregating tag data
  TagType type;
  result = mbImpl->tag_get_type(partSetTag, type);MB_CHK_ERR(result);
  if (type == MB_TAG_DENSE)
  {
    result = mbImpl->tag_delete(partSetTag);MB_CHK_ERR(result);
  }
  if (type == MB_TAG_SPARSE)
  {
    result = mbImpl->tag_delete_data(partSetTag, elems);MB_CHK_ERR(result);
  }
  
  // assemble the graph, using Skinner to get d-1 dimensional neighbors and then idx_tersecting to get adjacencies
  std::vector<Range> skin_subFaces(elems.size());
  unsigned int i = 0;
  for (Range::iterator rit = elems.begin(); rit != elems.end(); rit++) 
  {
    Range part_ents;
    result = mbImpl->get_entities_by_handle(*rit, part_ents, false);
    if (mbImpl->dimension_from_handle(*part_ents.rbegin()) != mbImpl->dimension_from_handle(*part_ents.begin())) 
    {
      Range::iterator lower = part_ents.lower_bound(CN::TypeDimensionMap[0].first),
      upper = part_ents.upper_bound(CN::TypeDimensionMap[dimension-1].second);
      part_ents.erase(lower, upper);
    }
    Skinner skinner(mbImpl);
    result = skinner.find_skin(0, part_ents, false, skin_subFaces[i], NULL, false, true, false);MB_CHK_ERR(result);
    i++;
  }
  std::vector<EntityHandle> adjs;
  std::vector<idx_t> neighbors;
  double avg_position[3];
  idx_t moab_id;
  MeshTopoUtil mtu(mbImpl);
  for (unsigned int k = 0; k < i; k++)
  {
      // get bridge adjacencies for element k
    adjs.clear();
    for (unsigned int t = 0; t < i; t++)
    {
      if (t != k)
      {
        Range subFaces = intersect(skin_subFaces[k],skin_subFaces[t]);
        if (subFaces.size() > 0)
  	      adjs.push_back(elems[t]);
      }
    }
    if (!adjs.empty()) 
    {
      neighbors.resize(adjs.size());
      result = mbImpl->tag_get_data(gid_tag, &adjs[0], adjs.size(), &neighbors[0]);MB_CHK_ERR(result); 
    }
      // copy those idx_to adjacencies vector
    length.push_back(length.back()+(idx_t)adjs.size());
    std::copy(neighbors.begin(), neighbors.end(), std::back_inserter(adjacencies));
      // get the graph vertex id for this element
    const EntityHandle& setk = elems[k];
    result = mbImpl->tag_get_data(gid_tag, &setk, 1, &moab_id); 
    moab_ids.push_back(moab_id);
      // get average position of vertices
    Range part_ents;
    result = mbImpl->get_entities_by_handle(elems[k], part_ents, false);MB_CHK_ERR(result);
    result = mtu.get_average_position(part_ents, avg_position);MB_CHK_ERR(result);
    std::copy(avg_position, avg_position+3, std::back_inserter(coords));
  }
  for (unsigned int k = 0; k < i; k++)
  {
    for (unsigned int t = 0; t < k; t++)
    {
      Range subFaces = intersect(skin_subFaces[k],skin_subFaces[t]);
      if (subFaces.size() > 0)
        mbImpl->delete_entities(subFaces);
    }
  }

  if (debug) {
    std::cout << "Length vector: " << std::endl;
    std::copy(length.begin(), length.end(), std::ostream_iterator<idx_t>(std::cout, ", "));
    std::cout << std::endl;
    std::cout << "Adjacencies vector: " << std::endl;
    std::copy(adjacencies.begin(), adjacencies.end(), std::ostream_iterator<idx_t>(std::cout, ", "));
    std::cout << std::endl;
    std::cout << "Moab_ids vector: " << std::endl;
    std::copy(moab_ids.begin(), moab_ids.end(), std::ostream_iterator<idx_t>(std::cout, ", "));
    std::cout << std::endl;
    std::cout << "Coords vector: " << std::endl;
    std::copy(coords.begin(), coords.end(), std::ostream_iterator<double>(std::cout, ", "));
    std::cout << std::endl;
  }
  return MB_SUCCESS;
}

ErrorCode MetisPartitioner::assemble_graph(const int dimension,
                                               std::vector<double> &coords,
                                               std::vector<idx_t> &moab_ids,
                                               std::vector<idx_t> &adjacencies, 
                                               std::vector<idx_t> &length,
                                               Range &elems) 
{
  length.push_back(0);
    // assemble a graph with vertices equal to elements of specified dimension, edges
    // signified by list of other elements to which an element is connected

    // get the elements of that dimension
  ErrorCode result = mbImpl->get_entities_by_dimension(0, dimension, elems);
  if (MB_SUCCESS != result || elems.empty()) return result;
  
#ifdef MOAB_HAVE_MPI
    // assign global ids
  result = mbpc->assign_global_ids(0, dimension, 0); 
#endif

    // now assemble the graph, calling MeshTopoUtil to get bridge adjacencies through d-1 dimensional
    // neighbors
  MeshTopoUtil mtu(mbImpl);
  Range adjs;
    // can use a fixed-size array 'cuz the number of lower-dimensional neighbors is limited
    // by MBCN
  int neighbors[5*MAX_SUB_ENTITIES]; // these are global ids, they will be int

  double avg_position[3];
  int moab_id;
  
    // get the global id tag hanlde
  Tag gid;
  result = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                  gid, MB_TAG_DENSE|MB_TAG_CREAT);MB_CHK_ERR(result);
  
  for (Range::iterator rit = elems.begin(); rit != elems.end(); rit++) {

      // get bridge adjacencies
    adjs.clear();
    result = mtu.get_bridge_adjacencies(*rit, (dimension > 0 ? dimension-1 : 3), 
                                        dimension, adjs);MB_CHK_ERR(result);
    
      // get the graph vertex ids of those
    if (!adjs.empty()) {
      assert(adjs.size() < 5*MAX_SUB_ENTITIES);
      result = mbImpl->tag_get_data(gid, adjs, neighbors);MB_CHK_ERR(result);
    }

      // copy those idx_to adjacencies vector
    length.push_back(length.back()+(idx_t)adjs.size());
    // conversion made to idx_t
    std::copy(neighbors, neighbors+adjs.size(), std::back_inserter(adjacencies));

      // get average position of vertices
    result = mtu.get_average_position(*rit, avg_position);MB_CHK_ERR(result);
    
      // get the graph vertex id for this element
    result = mbImpl->tag_get_data(gid, &(*rit), 1, &moab_id);MB_CHK_ERR(result);

      // copy those idx_to coords vector
    moab_ids.push_back(moab_id); // conversion made to idx_t
    std::copy(avg_position, avg_position+3, std::back_inserter(coords));
  }

  if (debug) {
    std::cout << "Length vector: " << std::endl;
    std::copy(length.begin(), length.end(), std::ostream_iterator<idx_t>(std::cout, ", "));
    std::cout << std::endl;
    std::cout << "Adjacencies vector: " << std::endl;
    std::copy(adjacencies.begin(), adjacencies.end(), std::ostream_iterator<idx_t>(std::cout, ", "));
    std::cout << std::endl;
    std::cout << "Moab_ids vector: " << std::endl;
    std::copy(moab_ids.begin(), moab_ids.end(), std::ostream_iterator<idx_t>(std::cout, ", "));
    std::cout << std::endl;
    std::cout << "Coords vector: " << std::endl;
    std::copy(coords.begin(), coords.end(), std::ostream_iterator<double>(std::cout, ", "));
    std::cout << std::endl;
  }

  return MB_SUCCESS;
}

ErrorCode MetisPartitioner::write_aggregationtag_partition(const idx_t nparts,
                                                               Range &elems, 
                                                               const idx_t *assignment,
                                                               const bool write_as_sets,
                                                               const bool write_as_tags)
{
  ErrorCode result;

    // get the partition set tag
  Tag part_set_tag;
  result = mbImpl->tag_get_handle("PARALLEL_PARTITION", 1, MB_TYPE_INTEGER,
                                  part_set_tag, MB_TAG_SPARSE|MB_TAG_CREAT);MB_CHK_ERR(result);

    // get any sets already with this tag, and clear them
  Range tagged_sets;
  result = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &part_set_tag, NULL, 1,
                                                tagged_sets, Interface::UNION);MB_CHK_ERR(result);
  if (!tagged_sets.empty()) {
    result = mbImpl->clear_meshset(tagged_sets); 
    if (!write_as_sets) {
      result = mbImpl->tag_delete_data(part_set_tag, tagged_sets);MB_CHK_ERR(result);
    }
  }
 
  if (write_as_sets) {
      // first, create partition sets and store in vector
    partSets.clear();
  
    if (nparts > (idx_t) tagged_sets.size()) {
        // too few partition sets - create missing ones
      idx_t num_new = nparts - tagged_sets.size();
      for (idx_t i = 0; i < num_new; i++) {
        EntityHandle new_set;
        result = mbImpl->create_meshset(MESHSET_SET, new_set);MB_CHK_ERR(result);
        tagged_sets.insert(new_set);
      }
    }
    else if (nparts < (idx_t) tagged_sets.size()) {
        // too many partition sets - delete extras
      idx_t num_del = tagged_sets.size() - nparts;
      for (idx_t i = 0; i < num_del; i++) {
        EntityHandle old_set = tagged_sets.pop_back();
        result = mbImpl->delete_entities(&old_set, 1);MB_CHK_ERR(result);
      }
    }
  
      // assign partition sets to vector
    partSets.swap(tagged_sets);
  
      // write a tag to those sets denoting they're partition sets, with a value of the
      // proc number
    idx_t *dum_ids = new idx_t[nparts];
    for (idx_t i = 0; i < nparts; i++) dum_ids[i] = i;
  
    result = mbImpl->tag_set_data(part_set_tag, partSets, dum_ids);MB_CHK_ERR(result);

      // assign entities to the relevant sets
    std::vector<EntityHandle> tmp_part_sets;
    std::copy(partSets.begin(), partSets.end(), std::back_inserter(tmp_part_sets));
    Range::iterator rit;
    unsigned j=0;
    for (rit = elems.begin(); rit != elems.end(); rit++, j++) {
      result = mbImpl->add_entities(tmp_part_sets[assignment[j]], &(*rit), 1);MB_CHK_ERR(result);
    }

      // check for empty sets, warn if there are any
    Range empty_sets;
    for (rit = partSets.begin(); rit != partSets.end(); rit++) {
      int num_ents = 0;
      result = mbImpl->get_number_entities_by_handle(*rit, num_ents);
      if (MB_SUCCESS != result || !num_ents) empty_sets.insert(*rit);
    }
    if (!empty_sets.empty()) {
      std::cout << "WARNING: " << empty_sets.size() << " empty sets in partition: ";
      for (rit = empty_sets.begin(); rit != empty_sets.end(); rit++)
        std::cout << *rit << " ";
      std::cout << std::endl;
    }
  }

  if (write_as_tags) {
    Tag gid_tag;
    result = mbImpl->tag_get_handle("GLOBAL_ID_AGGLO", 1, MB_TYPE_INTEGER, gid_tag, MB_TAG_SPARSE);MB_CHK_ERR(result);
  
      // allocate idx_teger-size partitions
    unsigned int i = 0;
    idx_t gid;
    for (Range::iterator rit = elems.begin(); rit != elems.end(); rit++) 
    {
      result = mbImpl->tag_get_data(gid_tag, &(*rit), 1, &gid);
      Range part_ents;
//      std::cout<<"part ents "<<part_ents.size()<<std::endl;
      result = mbImpl->get_entities_by_handle(*rit, part_ents, false);MB_CHK_ERR(result);

      for (Range::iterator eit = part_ents.begin(); eit != part_ents.end(); eit++) 
      {
        result = mbImpl->tag_set_data(part_set_tag, &(*eit), 1, &assignment[i]);MB_CHK_ERR(result);

        result = mbImpl->tag_set_data(gid_tag, &(*eit), 1, &gid);MB_CHK_ERR(result);
      }
      i++;
    }
  }
  return MB_SUCCESS;
}

ErrorCode MetisPartitioner::write_partition(const idx_t nparts,
                                                Range &elems, 
                                                const idx_t *assignment,
                                                const bool write_as_sets,
                                                const bool write_as_tags) 
{
  ErrorCode result;

    // get the partition set tag
  Tag part_set_tag;
  idx_t dum_id = -1, i;
  result = mbImpl->tag_get_handle("PARALLEL_PARTITION", 1, MB_TYPE_INTEGER,
                                  part_set_tag, MB_TAG_SPARSE|MB_TAG_CREAT, &dum_id);MB_CHK_ERR(result);
  
    // get any sets already with this tag, and clear them
  Range tagged_sets;
  result = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &part_set_tag, NULL, 1,
                                                tagged_sets, Interface::UNION);MB_CHK_ERR(result);
  if (!tagged_sets.empty()) {
    result = mbImpl->clear_meshset(tagged_sets); 
    if (!write_as_sets) {
      result = mbImpl->tag_delete_data(part_set_tag, tagged_sets);MB_CHK_ERR(result);
    }
  }

  if (write_as_sets) {
      // first, create partition sets and store in vector
    partSets.clear();
  
    if (nparts > (int) tagged_sets.size()) {
        // too few partition sets - create missing ones
      idx_t num_new = nparts - tagged_sets.size();
      for (i = 0; i < num_new; i++) {
        EntityHandle new_set;
        result = mbImpl->create_meshset(MESHSET_SET, new_set);MB_CHK_ERR(result);
        tagged_sets.insert(new_set);
      }
    }
    else if (nparts < (idx_t) tagged_sets.size()) {
        // too many partition sets - delete extras
      idx_t num_del = tagged_sets.size() - nparts;
      for (i = 0; i < num_del; i++) {
        EntityHandle old_set = tagged_sets.pop_back();
        result = mbImpl->delete_entities(&old_set, 1);MB_CHK_ERR(result);
      }
    }
  
      // assign partition sets to vector
    partSets.swap(tagged_sets);
  
      // write a tag to those sets denoting they're partition sets, with a value of the
      // proc number
    int *dum_ids = new int[nparts]; // this remains integer
    for (i = 0; i < nparts; i++) dum_ids[i] = i;
  
    result = mbImpl->tag_set_data(part_set_tag, partSets, dum_ids); 
    delete dum_ids;

      // assign entities to the relevant sets
    std::vector<EntityHandle> tmp_part_sets;
    std::copy(partSets.begin(), partSets.end(), std::back_inserter(tmp_part_sets));
    Range::iterator rit;
    for (i = 0, rit = elems.begin(); rit != elems.end(); rit++, i++) {
      result = mbImpl->add_entities(tmp_part_sets[assignment[i]], &(*rit), 1);MB_CHK_ERR(result);
    }

      // check for empty sets, warn if there are any
    Range empty_sets;
    for (rit = partSets.begin(); rit != partSets.end(); rit++) {
      int num_ents = 0;
      result = mbImpl->get_number_entities_by_handle(*rit, num_ents);
      if (MB_SUCCESS != result || !num_ents) empty_sets.insert(*rit);
    }
    if (!empty_sets.empty()) {
      std::cout << "WARNING: " << empty_sets.size() << " empty sets in partition: ";
      for (rit = empty_sets.begin(); rit != empty_sets.end(); rit++)
        std::cout << *rit << " ";
      std::cout << std::endl;
    }
  }
  
  if (write_as_tags) {
    if (sizeof(int) != sizeof(idx_t))
    {
        // allocate idx_teger-size partitions
      // first we have to copy to int, then assign
      int * assg_int = new int [elems.size()];
      for (int k=0; k<(int)elems.size(); k++)
        assg_int [k] = assignment[k];
      result = mbImpl->tag_set_data(part_set_tag, elems, assg_int); MB_CHK_ERR(result);
      delete [] assg_int;
    }
    else
      result = mbImpl->tag_set_data(part_set_tag, elems, assignment);MB_CHK_ERR(result);
  }
  
  return MB_SUCCESS;
}

