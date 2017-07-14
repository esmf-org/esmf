/* $Id$
 *
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

/** Get a mesh from MOAB and write a Zoltan partition set back into MOAB and to
 *  a file
 *
 * 
 */

#include <iostream>
#include <assert.h>
#include <sstream>
#include <algorithm>

#include "moab/ZoltanPartitioner.hpp"
#include "moab/Interface.hpp"
#include "Internals.hpp"
#include "moab/Range.hpp"
#include "moab/WriteUtilIface.hpp"
#include "moab/MeshTopoUtil.hpp"
#include "MBTagConventions.hpp"
#include "moab/CN.hpp"

#ifdef MOAB_HAVE_CGM
#include "CGMConfig.h"
#include <limits>
#include "RefEntity.hpp"
#include "RefFace.hpp"
#include "RefEdge.hpp"
#include "RefVertex.hpp"
#include "Body.hpp"
#include "TopologyEntity.hpp"
#include "CastTo.hpp"
#include "CABodies.hpp"
#include "TDParallel.hpp"
#include "TDUniqueId.hpp"
#endif

using namespace moab;

#define RR if (MB_SUCCESS != result) return result

static double *Points=NULL;
static int *GlobalIds=NULL;
static int NumPoints=0;
static int *NumEdges=NULL;
static int *NborGlobalId=NULL;
static int *NborProcs=NULL;
static double *ObjWeights=NULL;
static double *EdgeWeights=NULL;
static int *Parts=NULL;

const bool debug = false;

ZoltanPartitioner::ZoltanPartitioner( Interface *impl,
                    const bool use_coords,
                    int argc,
                    char **argv
#ifdef MOAB_HAVE_CGM
                    , GeometryQueryTool *gqt
#endif
)
                   : PartitionerBase(impl,use_coords),
                     myZZ(NULL),
                     argcArg(argc),
                     argvArg(argv)
#ifdef MOAB_HAVE_CGM
                   , gti(gqt)
#endif
{
}

ZoltanPartitioner::~ZoltanPartitioner()
{
  if (NULL != myZZ)
    delete myZZ;
}

ErrorCode ZoltanPartitioner::balance_mesh(const char *zmethod,
                                 const char *other_method,
                                 const bool write_as_sets,
                                 const bool write_as_tags) 
{
  if (!strcmp(zmethod, "RR") && !strcmp(zmethod, "RCB") && !strcmp(zmethod, "RIB") &&
      !strcmp(zmethod, "HSFC") && !strcmp(zmethod, "Hypergraph") &&
      !strcmp(zmethod, "PHG") && !strcmp(zmethod, "PARMETIS") &&
      !strcmp(zmethod, "OCTPART")) 
  {
    std::cout << "ERROR node " << mbpc->proc_config().proc_rank() << ": Method must be "
              << "RR, RCB, RIB, HSFC, Hypergraph (PHG), PARMETIS, or OCTPART"
              << std::endl;
    return MB_FAILURE;
  }

  std::vector<double> pts; // x[0], y[0], z[0], ... from MOAB
  std::vector<int> ids; // point ids from MOAB
  std::vector<int> adjs, length;
  Range elems;

  // Get a mesh from MOAB and divide it across processors.

  ErrorCode result;

  if (mbpc->proc_config().proc_rank() == 0) {
    result = assemble_graph(3, pts, ids, adjs, length, elems); RR;
  }
  
  myNumPts = mbInitializePoints((int)ids.size(), &pts[0], &ids[0], &adjs[0],
                                &length[0]);

  // Initialize Zoltan.  This is a C call.  The simple C++ code 
  // that creates Zoltan objects does not keep track of whether 
  // Zoltan_Initialize has been called.

  float version;

  Zoltan_Initialize(argcArg, argvArg, &version); 

  // Create Zoltan object.  This calls Zoltan_Create.  
  if (NULL == myZZ) myZZ = new Zoltan(MPI_COMM_WORLD);

  if (NULL == zmethod || !strcmp(zmethod, "RCB"))
    SetRCB_Parameters();
  else if (!strcmp(zmethod, "RIB"))
    SetRIB_Parameters();
  else if (!strcmp(zmethod, "HSFC"))
    SetHSFC_Parameters();
  else if (!strcmp(zmethod, "Hypergraph") || !strcmp(zmethod, "PHG"))
    if (NULL == other_method)
      SetHypergraph_Parameters("auto");
    else
      SetHypergraph_Parameters(other_method);
  else if (!strcmp(zmethod, "PARMETIS")) {
    if (NULL == other_method)
      SetPARMETIS_Parameters("RepartGDiffusion");
    else
      SetPARMETIS_Parameters(other_method);
  }
  else if (!strcmp(zmethod, "OCTPART")) {
    if (NULL == other_method)
      SetOCTPART_Parameters("2");
    else
      SetOCTPART_Parameters(other_method);
  }
  
  // Call backs:

  myZZ->Set_Num_Obj_Fn(mbGetNumberOfAssignedObjects, NULL);
  myZZ->Set_Obj_List_Fn(mbGetObjectList, NULL);
  myZZ->Set_Num_Geom_Fn(mbGetObjectSize, NULL);
  myZZ->Set_Geom_Multi_Fn(mbGetObject, NULL);
  myZZ->Set_Num_Edges_Multi_Fn(mbGetNumberOfEdges, NULL);
  myZZ->Set_Edge_List_Multi_Fn(mbGetEdgeList, NULL);

  // Perform the load balancing partitioning

  int changes;
  int numGidEntries;
  int numLidEntries;
  int numImport;
  ZOLTAN_ID_PTR importGlobalIds;
  ZOLTAN_ID_PTR importLocalIds;
  int *importProcs;
  int *importToPart;
  int numExport;
  ZOLTAN_ID_PTR exportGlobalIds;
  ZOLTAN_ID_PTR exportLocalIds;
  int *exportProcs;
  int *exportToPart;

  int rc = myZZ->LB_Partition(changes, numGidEntries, numLidEntries, 
                            numImport, importGlobalIds, importLocalIds, 
                            importProcs, importToPart,
                            numExport, exportGlobalIds, exportLocalIds, 
                            exportProcs, exportToPart);

  rc = mbGlobalSuccess(rc);
  
  if (!rc){
    mbPrintGlobalResult("==============Result==============", 
                        myNumPts, numImport, numExport, changes);
  }
  else{
    return MB_FAILURE;
  }
  
  // take results & write onto MOAB partition sets

  int *assignment;

  mbFinalizePoints((int)ids.size(), numExport, exportLocalIds,
                   exportProcs, &assignment);

  if (mbpc->proc_config().proc_rank() == 0) {
    result = write_partition(mbpc->proc_config().proc_size(), elems, assignment,
                                       write_as_sets, write_as_tags);

    if (MB_SUCCESS != result) return result;

    free((int *) assignment);
  }

  // Free the memory allocated for lists returned by LB_Parition()

  myZZ->LB_Free_Part(&importGlobalIds, &importLocalIds, &importProcs, 
                     &importToPart);
  myZZ->LB_Free_Part(&exportGlobalIds, &exportLocalIds, &exportProcs, 
                     &exportToPart);

  // Implementation note:  A Zoltan object contains an MPI communicator.
  //   When the Zoltan object is destroyed, it uses it's MPI communicator.
  //   So it is important that the Zoltan object is destroyed before
  //   the MPI communicator is destroyed.  To ensure this, dynamically
  //   allocate the Zoltan object, so you can explicitly destroy it.
  //   If you create a Zoltan object on the stack, it's destructor will
  //   be invoked atexit, possibly after the communicator's
  //   destructor.

  return MB_SUCCESS;
}

ErrorCode  ZoltanPartitioner::repartition(std::vector<double> & x,std::vector<double>&y, std::vector<double> &z, int StartID,
    const char * zmethod, Range & localGIDs)
{
  //
  int nprocs=mbpc->proc_config().proc_size();
  int rank = mbpc->proc_config().proc_rank();
  clock_t t = clock();
  // form pts and ids, as in assemble_graph
  std::vector<double> pts; // x[0], y[0], z[0], ... from MOAB
  pts.resize(x.size()*3);
  std::vector<int> ids; // point ids from MOAB
  ids.resize(x.size());
  for (size_t i=0; i<x.size(); i++)
  {
    pts[3*i]=  x[i];
    pts[3*i+1]=y[i];
    pts[3*i+2]=z[i];
    ids[i]=StartID+(int)i;
  }
  // do not get out until done!

  Points= &pts[0];
  GlobalIds=&ids[0];
  NumPoints=(int)x.size();
  NumEdges=NULL;
  NborGlobalId=NULL;
  NborProcs=NULL;
  ObjWeights=NULL;
  EdgeWeights=NULL;
  Parts=NULL;

  float version;
  if (rank==0)
    std::cout << "Initializing zoltan..." << std::endl;

  Zoltan_Initialize(argcArg, argvArg, &version);

  // Create Zoltan object.  This calls Zoltan_Create.
  if (NULL == myZZ) myZZ = new Zoltan(MPI_COMM_WORLD);

  if (NULL == zmethod || !strcmp(zmethod, "RCB"))
    SetRCB_Parameters();
  else if (!strcmp(zmethod, "RIB"))
    SetRIB_Parameters();
  else if (!strcmp(zmethod, "HSFC"))
    SetHSFC_Parameters();

  // set # requested partitions
  char buff[10];
  sprintf(buff, "%d", nprocs);
  int retval = myZZ->Set_Param("NUM_GLOBAL_PARTITIONS", buff);
  if (ZOLTAN_OK != retval) return MB_FAILURE;

    // request all, import and export
  retval = myZZ->Set_Param("RETURN_LISTS", "ALL");
  if (ZOLTAN_OK != retval) return MB_FAILURE;


  myZZ->Set_Num_Obj_Fn(mbGetNumberOfAssignedObjects, NULL);
  myZZ->Set_Obj_List_Fn(mbGetObjectList, NULL);
  myZZ->Set_Num_Geom_Fn(mbGetObjectSize, NULL);
  myZZ->Set_Geom_Multi_Fn(mbGetObject, NULL);
  myZZ->Set_Num_Edges_Multi_Fn(mbGetNumberOfEdges, NULL);
  myZZ->Set_Edge_List_Multi_Fn(mbGetEdgeList, NULL);

  // Perform the load balancing partitioning

  int changes;
  int numGidEntries;
  int numLidEntries;
  int num_import;
  ZOLTAN_ID_PTR import_global_ids, import_local_ids;
  int * import_procs;
  int * import_to_part;
  int num_export;
  ZOLTAN_ID_PTR export_global_ids, export_local_ids;
  int *assign_procs, *assign_parts;

  if (rank ==0)
    std::cout << "Computing partition using " << (zmethod ? zmethod : "RCB") <<
      " method for " << nprocs << " processors..." << std::endl;

  retval = myZZ->LB_Partition(changes, numGidEntries, numLidEntries,
                              num_import, import_global_ids, import_local_ids, import_procs, import_to_part,
                              num_export, export_global_ids, export_local_ids,
                              assign_procs, assign_parts);
  if (ZOLTAN_OK != retval) return MB_FAILURE;

  if (rank==0)
  {
    std::cout << " time to LB_partition " << (clock() - t) / (double) CLOCKS_PER_SEC  << "s. \n";
    t = clock();
  }

  std::sort(import_global_ids, import_global_ids+num_import, std::greater<int> ());
  std::sort(export_global_ids, export_global_ids+num_export, std::greater<int> ());

  Range iniGids((EntityHandle)StartID, (EntityHandle)StartID+x.size()-1);
  Range imported, exported;
  std::copy(import_global_ids, import_global_ids+num_import, range_inserter(imported));
  std::copy(export_global_ids, export_global_ids+num_export, range_inserter(exported));
  localGIDs=subtract(iniGids, exported);
  localGIDs=unite(localGIDs, imported);
  // Free data structures allocated by Zoltan::LB_Partition
  retval = myZZ-> LB_Free_Part(&import_global_ids, &import_local_ids,
           &import_procs, &import_to_part);
  if (ZOLTAN_OK != retval) return MB_FAILURE;
  retval = myZZ-> LB_Free_Part(&export_global_ids, &export_local_ids,
           &assign_procs, &assign_parts);
  if (ZOLTAN_OK != retval) return MB_FAILURE;

  return MB_SUCCESS;
}

ErrorCode ZoltanPartitioner::partition_mesh_and_geometry(const double part_geom_mesh_size,
                                        const int nparts,
                                        const char *zmethod,
                                        const char *other_method,
                                        double imbal_tol,
                                        const int part_dim,
                                        const bool write_as_sets,
                                        const bool write_as_tags,
                                        const int obj_weight,
                                        const int edge_weight,
#ifdef MOAB_HAVE_CGM
                                        const bool part_surf,
                                        const bool ghost,
#else
                                        const bool, const bool,
#endif
                                        const bool spherical_coords,
                                        const bool print_time)
{
    // should only be called in serial
  if (mbpc->proc_config().proc_size() != 1) {
    std::cout << "ZoltanPartitioner::partition_mesh_and_geometry must be called in serial." 
              << std::endl;
    return MB_FAILURE;
  }
  clock_t t = clock();
  if (NULL != zmethod && strcmp(zmethod, "RR") && strcmp(zmethod, "RCB") && strcmp(zmethod, "RIB") &&
      strcmp(zmethod, "HSFC") && strcmp(zmethod, "Hypergraph") &&
      strcmp(zmethod, "PHG") && strcmp(zmethod, "PARMETIS") &&
      strcmp(zmethod, "OCTPART")) 
  {
    std::cout << "ERROR node " << mbpc->proc_config().proc_rank() << ": Method must be "
              << "RCB, RIB, HSFC, Hypergraph (PHG), PARMETIS, or OCTPART"
              << std::endl;
    return MB_FAILURE;
  }

  bool part_geom = false;
  if ( 0==  strcmp(zmethod, "RR") || 0== strcmp(zmethod, "RCB") || 0== strcmp(zmethod, "RIB")
      || 0==strcmp(zmethod, "HSFC") )
    part_geom=true; // so no adjacency / edges needed
  std::vector<double> pts; // x[0], y[0], z[0], ... from MOAB
  std::vector<int> ids; // point ids from MOAB
  std::vector<int> adjs, length, parts;
  std::vector<double> obj_weights, edge_weights;
  Range elems;
#ifdef MOAB_HAVE_CGM
  DLIList<RefEntity*> entities;
#endif

  // Get a mesh from MOAB and diide it across processors.

  ErrorCode result = MB_SUCCESS;
  
    // short-circuit everything if RR partition is requested
  if (!strcmp(zmethod, "RR")) {
    if (part_geom_mesh_size < 0.) {
      // get all elements
      result = mbImpl->get_entities_by_dimension(0, part_dim, elems); RR;
      if(elems.empty())
         return MB_FAILURE;
      // make a trivial assignment vector
      std::vector<int> assign_vec(elems.size());
      int num_per = elems.size() / nparts;
      int extra = elems.size() % nparts;
      if (extra) num_per++;
      int nstart = 0;
      for (int i = 0; i < nparts; i++) {
        if (i == extra) num_per--;
        std::fill(&assign_vec[nstart], &assign_vec[nstart+num_per], i);
        nstart += num_per;
      }

      result = write_partition(nparts, elems, &assign_vec[0],
                               write_as_sets, write_as_tags); RR;
    }
    else {
#ifdef MOAB_HAVE_CGM
      result = partition_round_robin(nparts); RR;
#endif
    }

    return result;
  }

  std::cout << "Assembling graph..." << std::endl;
  
  if (part_geom_mesh_size < 0.) {
    //if (!part_geom) {
       result = assemble_graph(part_dim, pts, ids, adjs, length, elems, part_geom,
           spherical_coords); RR;
  }
  else {
#ifdef MOAB_HAVE_CGM
    result = assemble_graph(part_dim, pts, ids, adjs, length, obj_weights,
                            edge_weights, parts, entities,
                            part_geom_mesh_size, nparts); RR;

    if (debug) {
      int n_ids = ids.size();
      entities.reset();
      int i_leng = 0;
      for (int i = 0; i < n_ids; i++) {
        std::cout << "graph_input_ids[" << i << "]=" << ids[i]
                  << ",obj_weights=" << obj_weights[i]
                  << ",entity_id=" << entities.get_and_step()->id()
                  << ",part=" << parts[i] << std::endl;
        for (int j = 0; j < length[i]; j++) {
          std::cout << "adjs[" << j << "]=" << adjs[i_leng] 
                    << ",edge_weights=" << edge_weights[i_leng]<< std::endl;
          i_leng++;
        }
      }
    }
#endif
  }
  if (print_time)
  {
    std::cout << " time to assemble graph: " << (clock() - t) / (double) CLOCKS_PER_SEC  << "s. \n";
    t = clock();
  }
  double* o_wgt = NULL;
  double* e_wgt = NULL;
  if (obj_weights.size() > 0) o_wgt = &obj_weights[0];
  if (edge_weights.size() > 0) e_wgt = &edge_weights[0];
    
  myNumPts = mbInitializePoints((int)ids.size(), &pts[0], &ids[0], &adjs[0],
                                &length[0], o_wgt, e_wgt, &parts[0], part_geom);
  

  // Initialize Zoltan.  This is a C call.  The simple C++ code 
  // that creates Zoltan objects does not keep track of whether 
  // Zoltan_Initialize has been called.

  if (print_time)
  {
    std::cout << " time to initialize points: " << (clock() - t) / (double) CLOCKS_PER_SEC  << "s. \n";
    t = clock();
  }
  float version;

  std::cout << "Initializing zoltan..." << std::endl;

  Zoltan_Initialize(argcArg, argvArg, &version); 

  // Create Zoltan object.  This calls Zoltan_Create.  
  if (NULL == myZZ) myZZ = new Zoltan(MPI_COMM_WORLD);

  if (NULL == zmethod || !strcmp(zmethod, "RCB"))
    SetRCB_Parameters();
  else if (!strcmp(zmethod, "RIB"))
    SetRIB_Parameters();
  else if (!strcmp(zmethod, "HSFC"))
    SetHSFC_Parameters();
  else if (!strcmp(zmethod, "Hypergraph") || !strcmp(zmethod, "PHG")) {
    if (NULL == other_method || (other_method[0]=='\0') )
      SetHypergraph_Parameters("auto");
    else
      SetHypergraph_Parameters(other_method);

    if (imbal_tol) {
      std::ostringstream str;
      str << imbal_tol;
      myZZ->Set_Param("IMBALANCE_TOL", str.str().c_str());     // no debug messages
    }
  }
  else if (!strcmp(zmethod, "PARMETIS")) {
    if (NULL == other_method)
      SetPARMETIS_Parameters("RepartGDiffusion");
    else
      SetPARMETIS_Parameters(other_method);
  }
  else if (!strcmp(zmethod, "OCTPART")) {
    if (NULL == other_method)
      SetOCTPART_Parameters("2");
    else
      SetOCTPART_Parameters(other_method);
  }
  
    // set # requested partitions
  char buff[10];
  sprintf(buff, "%d", nparts);
  int retval = myZZ->Set_Param("NUM_GLOBAL_PARTITIONS", buff);
  if (ZOLTAN_OK != retval) return MB_FAILURE;

    // request only partition assignments
  retval = myZZ->Set_Param("RETURN_LISTS", "PARTITION ASSIGNMENTS");
  if (ZOLTAN_OK != retval) return MB_FAILURE;
  
  if (obj_weight > 0) {
    std::ostringstream str;
    str << obj_weight;
    retval = myZZ->Set_Param("OBJ_WEIGHT_DIM", str.str().c_str());
    if (ZOLTAN_OK != retval) return MB_FAILURE;
  }

  if (edge_weight > 0) {
    std::ostringstream str;
    str << edge_weight;
    retval = myZZ->Set_Param("EDGE_WEIGHT_DIM", str.str().c_str());
    if (ZOLTAN_OK != retval) return MB_FAILURE;
  }

  // Call backs:

  myZZ->Set_Num_Obj_Fn(mbGetNumberOfAssignedObjects, NULL);
  myZZ->Set_Obj_List_Fn(mbGetObjectList, NULL);
  myZZ->Set_Num_Geom_Fn(mbGetObjectSize, NULL);
  myZZ->Set_Geom_Multi_Fn(mbGetObject, NULL);
  myZZ->Set_Num_Edges_Multi_Fn(mbGetNumberOfEdges, NULL);
  myZZ->Set_Edge_List_Multi_Fn(mbGetEdgeList, NULL);
  if (part_geom_mesh_size > 0.) {
    myZZ->Set_Part_Multi_Fn(mbGetPart, NULL);
  }

  // Perform the load balancing partitioning

  int changes;
  int numGidEntries;
  int numLidEntries;
  int dumnum1;
  ZOLTAN_ID_PTR dum_local, dum_global;
  int *dum1, *dum2;
  int num_assign;
  ZOLTAN_ID_PTR assign_gid, assign_lid;
  int *assign_procs, *assign_parts;

  std::cout << "Computing partition using " << (zmethod ? zmethod : "RCB") <<
      " method for " << nparts << " processors..." << std::endl;
  
  retval = myZZ->LB_Partition(changes, numGidEntries, numLidEntries, 
                              dumnum1, dum_global, dum_local, dum1, dum2,
                              num_assign, assign_gid, assign_lid,
                              assign_procs, assign_parts);
  if (ZOLTAN_OK != retval) return MB_FAILURE;
  
  if (print_time)
  {
    std::cout << " time to LB_partition " << (clock() - t) / (double) CLOCKS_PER_SEC  << "s. \n";
    t = clock();
  }
  // take results & write onto MOAB partition sets
  std::cout << "Saving partition information to MOAB..." << std::endl;
  
  if (part_geom_mesh_size < 0.) {
    //if (!part_geom) {
    result = write_partition(nparts, elems, assign_parts,
                             write_as_sets, write_as_tags);
  }
  else {
#ifdef MOAB_HAVE_CGM
    result = write_partition(nparts, entities, assign_parts,
                             obj_weights, part_surf, ghost);
#endif
  }

  if (print_time)
  {
    std::cout << " time to write partition in memory " <<(clock() - t) / (double) CLOCKS_PER_SEC  << "s. \n";
    t = clock();
  }

  if (MB_SUCCESS != result) return result;


  // Free the memory allocated for lists returned by LB_Parition()
  myZZ->LB_Free_Part(&assign_gid, &assign_lid, &assign_procs, &assign_parts);

  return MB_SUCCESS;
}

ErrorCode ZoltanPartitioner::include_closure() 
{
  ErrorCode result;
  Range ents;
  Range adjs;
  std::cout << "Adding closure..." << std::endl;
  
  for (Range::iterator rit = partSets.begin(); rit != partSets.end(); ++rit) {
    
      // get the top-dimensional entities in the part
    result = mbImpl->get_entities_by_handle(*rit, ents, true); RR;

    if (ents.empty()) continue;
    
      // get intermediate-dimensional adjs and add to set
    for (int d = mbImpl->dimension_from_handle(*ents.begin())-1; d >= 1; d--) {
      adjs.clear();
      result = mbImpl->get_adjacencies(ents, d, false, adjs, Interface::UNION); RR;
      result = mbImpl->add_entities(*rit, adjs); RR;
    }
    
      // now get vertices and add to set; only need to do for ents, not for adjs
    adjs.clear();
    result = mbImpl->get_adjacencies(ents, 0, false, adjs, Interface::UNION); RR;
    result = mbImpl->add_entities(*rit, adjs); RR;

    ents.clear();
  }
  
    // now go over non-part entity sets, looking for contained entities
  Range sets, part_ents;
  result = mbImpl->get_entities_by_type(0, MBENTITYSET, sets); RR;
  for (Range::iterator rit = sets.begin(); rit != sets.end(); ++rit) {
      // skip parts
    if (partSets.find(*rit) != partSets.end()) continue;

      // get entities in this set, recursively
    ents.clear();
    result = mbImpl->get_entities_by_handle(*rit, ents, true); RR;
    
      // now check over all parts
    for (Range::iterator rit2 = partSets.begin(); rit2 != partSets.end(); ++rit2) {
      part_ents.clear();
      result = mbImpl->get_entities_by_handle(*rit2, part_ents, false); RR;
      Range int_range = intersect(ents, part_ents);
      if (!int_range.empty()) {
          // non-empty intersection, add to part set
        result = mbImpl->add_entities(*rit2, &(*rit), 1); RR;
      }
    }
  }
  
    // finally, mark all the part sets as having the closure
  Tag closure_tag;
  result = mbImpl->tag_get_handle("INCLUDES_CLOSURE", 1, MB_TYPE_INTEGER,
                                  closure_tag, MB_TAG_SPARSE|MB_TAG_CREAT); RR;
  
  std::vector<int> closure_vals(partSets.size(), 1);
  result = mbImpl->tag_set_data(closure_tag, partSets, &closure_vals[0]); RR;
  
  return MB_SUCCESS;
}

ErrorCode ZoltanPartitioner::assemble_graph(const int dimension,
                                   std::vector<double> &coords,
                                   std::vector<int> &moab_ids,
                                   std::vector<int> &adjacencies, 
                                   std::vector<int> &length,
                                   Range &elems, bool  part_geom, bool spherical_coords)
{
    // assemble a graph with vertices equal to elements of specified dimension, edges
    // signified by list of other elements to which an element is connected

    // get the elements of that dimension
  ErrorCode result = mbImpl->get_entities_by_dimension(0, dimension, elems);
  if (MB_SUCCESS != result || elems.empty()) return result;
  
    // assign global ids
  result = mbpc->assign_global_ids(0, dimension); RR;

    // now assemble the graph, calling MeshTopoUtil to get bridge adjacencies through d-1 dimensional
    // neighbors
  MeshTopoUtil mtu(mbImpl);
  Range adjs;
    // can use a fixed-size array 'cuz the number of lower-dimensional neighbors is limited
    // by MBCN
  int neighbors[5*MAX_SUB_ENTITIES];
  double avg_position[3];
  int moab_id;
  
    // get the global id tag handle
  Tag gid;
  result = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                  gid, MB_TAG_DENSE|MB_TAG_CREAT);
  if (MB_SUCCESS != result) return result;
  
  for (Range::iterator rit = elems.begin(); rit != elems.end(); ++rit) {


    if (!part_geom)
    {
      // get bridge adjacencies
      adjs.clear();
      result = mtu.get_bridge_adjacencies(*rit, (dimension > 0 ? dimension-1 : 3),
                                          dimension, adjs); RR;

        // get the graph vertex ids of those
      if (!adjs.empty()) {
        assert(adjs.size() < 5*MAX_SUB_ENTITIES);
        result = mbImpl->tag_get_data(gid, adjs, neighbors); RR;
      }

        // copy those into adjacencies vector
      length.push_back((int)adjs.size());
      std::copy(neighbors, neighbors+adjs.size(), std::back_inserter(adjacencies));
    }

      // get average position of vertices
    result = mtu.get_average_position(*rit, avg_position); RR;
    
      // get the graph vertex id for this element
    result = mbImpl->tag_get_data(gid, &(*rit), 1, &moab_id); RR;

      // copy those into coords vector
    moab_ids.push_back(moab_id);
    // transform coordinates to spherical coordinates, if requested
    if (spherical_coords)
    {
      double R = avg_position[0]*avg_position[0]+avg_position[1]*avg_position[1]+avg_position[2]*avg_position[2];
      R = sqrt(R);
      double lat = asin(avg_position[2]/R);
      double lon=atan2(avg_position[1], avg_position[0]);
      avg_position[0]=lon;
      avg_position[1]=lat;
      avg_position[2]=R;
    }

    std::copy(avg_position, avg_position+3, std::back_inserter(coords));
  }

  if (debug) {
    std::cout << "Length vector: " << std::endl;
    std::copy(length.begin(), length.end(), std::ostream_iterator<int>(std::cout, ", "));
    std::cout << std::endl;
    std::cout << "Adjacencies vector: " << std::endl;
    std::copy(adjacencies.begin(), adjacencies.end(), std::ostream_iterator<int>(std::cout, ", "));
    std::cout << std::endl;
    std::cout << "Moab_ids vector: " << std::endl;
    std::copy(moab_ids.begin(), moab_ids.end(), std::ostream_iterator<int>(std::cout, ", "));
    std::cout << std::endl;
    std::cout << "Coords vector: " << std::endl;
    std::copy(coords.begin(), coords.end(), std::ostream_iterator<double>(std::cout, ", "));
    std::cout << std::endl;
  }

  return MB_SUCCESS;
}

#ifdef MOAB_HAVE_CGM
ErrorCode ZoltanPartitioner::assemble_graph(const int /* dimension */,
                                   std::vector<double> & /* coords */,
                                   std::vector<int> &moab_ids,
                                   std::vector<int> &adjacencies, 
                                   std::vector<int> &length,
                                   std::vector<double> &obj_weights,
                                   std::vector<double> &edge_weights,
                                   std::vector<int> &parts,
                                   DLIList<RefEntity*> &entities,
                                   const double part_geom_mesh_size,
                                   const int n_part) 
{
  // get body vertex weights
  DLIList<RefEntity*> body_list;
  gti->ref_entity_list("body", body_list, CUBIT_FALSE);
  DLIList<RefFace*> all_shared_surfs;
  int n_bodies = body_list.size();
  int body_map_index = 1;
  int surf_map_index = n_bodies + 1;
  int n_bodies_proc = n_bodies/n_part; // # of entities per processor
  int i_bodies_proc = n_bodies_proc; // entity index limit for each processor
  int proc = 0;

  body_list.reset();
  for (int i = 0; i < n_bodies; i++) {
    // assign part in round-robin
    if (i == i_bodies_proc) {
      proc++;
      if (proc < n_part) i_bodies_proc += n_bodies_proc;
      else {
        proc %= n_part;
        i_bodies_proc++;
      }
    }
    parts.push_back(proc);

    // volume mesh generation load estimation
    RefEntity *body = body_list.get_and_step();
    double vertex_w = body->measure();
    vertex_w /= part_geom_mesh_size*part_geom_mesh_size*part_geom_mesh_size;
    vertex_w = 3.8559e-5*vertex_w*log(vertex_w);

    // add child non-interface face weights
    DLIList<RefFace*> shared_surfs;
    DLIList<RefFace*> faces;
    (dynamic_cast<TopologyEntity*> (body))->ref_faces(faces);
    int n_face = faces.size();
    faces.reset();
    for (int j = 0; j < n_face; j++) {
      RefFace* face = faces.get_and_step();
      TopologyEntity *te = CAST_TO(face, TopologyEntity);
      if (te->bridge_manager()->number_of_bridges() > 1) { // shared
        shared_surfs.append(face);
      }
      else { // non-shared
        vertex_w += estimate_face_mesh_load(face, face->measure());
      }
    }

    int temp_index = body_map_index++;
    body_vertex_map[body->id()] = temp_index;
    moab_ids.push_back(temp_index); // add body as graph vertex
    obj_weights.push_back(vertex_w); // add weight for body graph vertex

    if (debug) {
      std::cout << "body=" << body->id() << ",graph_id=" << temp_index
                << ",weight=" << vertex_w << ",part=" << proc << std::endl;
    }

    // treat shared surfaces connected to this body
    int n_shared = shared_surfs.size();
    shared_surfs.reset();
    for (int k = 0; k < n_shared; k++) { // add adjacencies
      RefFace *face = shared_surfs.get_and_step();
      std::map<int, int>::iterator iter = surf_vertex_map.find(face->id());
      if (iter != surf_vertex_map.end()) {
	temp_index = (*iter).second;
      }
      else {
	temp_index = surf_map_index++;
	surf_vertex_map[face->id()] = temp_index;
        all_shared_surfs.append(face);
      }
      adjacencies.push_back(temp_index);
      double tmp_sw = estimate_face_comm_load(face, part_geom_mesh_size);
      edge_weights.push_back(tmp_sw);

      if (debug) {
        std::cout << "adjac=" << temp_index << ",weight="
                  << tmp_sw << std::endl;
      }
    }
    length.push_back(n_shared);
  }
  entities += body_list;

  // add shared surface as graph vertex
  int n_all_shared_surf = all_shared_surfs.size();
  all_shared_surfs.reset();
  for (int i = 0; i < n_all_shared_surf; i++) {
    RefEntity *face = all_shared_surfs.get_and_step();
    moab_ids.push_back(surf_vertex_map[face->id()]);
    double face_mesh_load = estimate_face_mesh_load(face,
                                                    part_geom_mesh_size);
    obj_weights.push_back(face_mesh_load);

    // set surface object graph edges
    int min_ind = -1;
    double min_load = std::numeric_limits<double>::max();;
    DLIList<Body*> parents;
    dynamic_cast<TopologyEntity*> (face)->bodies(parents);
    int n_parents = parents.size();
    parents.reset();
    for (int j = 0; j < n_parents; j++) {
      int body_gid = body_vertex_map[parents.get_and_step()->id()];
      adjacencies.push_back(body_gid); // add adjacency
      edge_weights.push_back(estimate_face_comm_load(face,
                                                     part_geom_mesh_size));
      
      if (obj_weights[body_gid - 1] < min_load) {
        min_ind = body_gid - 1;
        min_load = obj_weights[min_ind];
      }
    }
    length.push_back(n_parents);
    entities.append(dynamic_cast<RefEntity*> (face));
    obj_weights[min_ind] += face_mesh_load;
    parts.push_back(parts[min_ind]);

    if (debug) {
      std::cout << "shared_surf=" << face->id() << ",graph_id="
                << surf_vertex_map[face->id()]
                << ",weight=" << face_mesh_load
                << ",part=" << parts[min_ind] << std::endl;
    }
  }

  for (size_t i = 0; i < obj_weights.size(); i++) if (obj_weights[i] < 1.) obj_weights[i] = 1.;
  for (size_t i = 0; i < edge_weights.size(); i++) if (edge_weights[i] < 1.) edge_weights[i] = 1.;
  
  return MB_SUCCESS;
}

double ZoltanPartitioner::estimate_face_mesh_load(RefEntity* face, const double h)
{
  GeometryType type = CAST_TO(face, RefFace)->geometry_type();
  double n = face->measure()/h/h;
  double n_logn = n*log(n);

  if (type == PLANE_SURFACE_TYPE) {
    return 1.536168737505151e-4*n_logn;
  }
  else if (type == SPLINE_SURFACE_TYPE) {
    return 5.910511018383144e-4*n_logn;
  }
  else if (type == CONE_SURFACE_TYPE ||
	   type == SPHERE_SURFACE_TYPE ||
	   type == TORUS_SURFACE_TYPE) {
    return 2.352511671418708e-4*n_logn;
  }

  return 0.0;
}

double ZoltanPartitioner::estimate_face_comm_load(RefEntity* face, const double h)
{
  double peri = 0.;
#if ((CGM_MAJOR_VERSION == 14 && CGM_MINOR_VERSION > 2) || CGM_MAJOR_VERSION == 15)
  DLIList<DLIList<RefEdge*> > ref_edge_loops;
#else
  DLIList<DLIList<RefEdge*>*> ref_edge_loops;
#endif
  CAST_TO(face, RefFace)->ref_edge_loops(ref_edge_loops);
  ref_edge_loops.reset();

#if ((CGM_MAJOR_VERSION == 14 && CGM_MINOR_VERSION > 2) || CGM_MAJOR_VERSION == 15)
  for (int i = 0; i < ref_edge_loops.size(); i++) {
    DLIList<RefEdge*> eloop = ref_edge_loops.get_and_step();
    eloop.reset();
    for (int j = 0; j < eloop.size(); j++) {
      peri += eloop.get_and_step()->measure();
    }
  }
#else
  for (int i = 0; i < ref_edge_loops.size(); i++) {
    DLIList<RefEdge*>* eloop = ref_edge_loops.get_and_step();
    eloop->reset();
    for (int j = 0; j < eloop->size(); j++) {
      peri += eloop->get_and_step()->measure();
    }
  }
#endif
  
  //return 104*face->measure()/sqrt(3)/h/h + 56/3*peri/h;
  return (104*face->measure()/sqrt(3)/h/h + 56/3*peri/h)/700000.;
}

ErrorCode ZoltanPartitioner::write_partition(const int nparts,
                                    DLIList<RefEntity*> entities,
                                    const int *assignment,
                                    std::vector<double> &obj_weights,
                                    const bool part_surf,
                                    const bool ghost)
{
  ErrorCode result;

  // actuate CA_BODIES and turn on auto flag for other attributes
  CGMApp::instance()->attrib_manager()->register_attrib_type(CA_BODIES, "bodies", "BODIES",
							     CABodies_creator, CUBIT_TRUE,
							     CUBIT_TRUE, CUBIT_TRUE, CUBIT_TRUE,
							     CUBIT_TRUE, CUBIT_FALSE);
  CGMApp::instance()->attrib_manager()->auto_flag(CUBIT_TRUE);

  // set partition info to Body at first
  int n_entities = entities.size();
  entities.reset();
  for (int i = 0; i < n_entities; i++) {
    int proc = assignment[i];
    DLIList<int> shared_procs;
    RefEntity *entity = entities.get_and_step();

    if (entity->entity_type_info() == typeid(Body)) {
      shared_procs.append(proc);
      TDParallel *td_par = (TDParallel *) entity->get_TD(&TDParallel::is_parallel);
      if (td_par == NULL) td_par = new TDParallel(entity, NULL, &shared_procs);

      if (debug) {
        std::cout << "body" << entity->id() << "_is_partitioned_to_p" << proc << std::endl;
      }

      // assign to volumes, it should be removed in future
      DLIList<RefVolume*> volumes;
      (dynamic_cast<TopologyEntity*> (entity))->ref_volumes(volumes);
      int n_vol = volumes.size();
      volumes.reset();
      for (int j = 0; j < n_vol; j++) {
        RefEntity *vol = volumes.get_and_step();
        td_par = (TDParallel *) vol->get_TD(&TDParallel::is_parallel);
        if (td_par == NULL) td_par = new TDParallel(vol, NULL, &shared_procs);
      }
    }
  }

  // set partition info to shared surfaces
  entities.reset();
  for (int i = 0; i < n_entities; i++) {
    int proc = assignment[i];
    DLIList<int> shared_procs;
    RefEntity *entity = entities.get_and_step();

    if (entity->entity_type_info() == typeid(RefFace)) { // surface
      DLIList<Body*> parents;
      (dynamic_cast<TopologyEntity*> (entity))->bodies(parents);
      int n_parents = parents.size();
      if (n_parents != 2) { // check # of parents
        std::cerr << "# of parent bodies of interface surface should be 2." << std::endl;
        return MB_FAILURE;
      }
      shared_procs.append(proc); // local proc
      parents.reset();
      for (int j = 0 ; j < 2; j++) {
        RefEntity *parent = parents.get_and_step();
        TDParallel *parent_td = (TDParallel *) parent->get_TD(&TDParallel::is_parallel);
        
        if (parent_td == NULL) {
          std::cerr << "parent body should have balanced process." << std::endl;
          return MB_FAILURE;
        }
        int temp_proc = parent_td->get_charge_proc();
        if (temp_proc != proc) shared_procs.append(temp_proc); // remote proc
      }

      if (shared_procs.size() > 1) { // if it is shared by 2 processors
        int merge_id = TDUniqueId::get_unique_id(entity);
        TDParallel *td_par = (TDParallel *) entity->get_TD(&TDParallel::is_parallel);
        if (td_par == NULL) td_par = new TDParallel(entity, NULL, &shared_procs,
                                                    NULL, merge_id, 1);
        
        if (debug) {
          std::cout << "surf" << entity->id() << "_is_partitioned_to_p";
          for (int j = 0; j < shared_procs.size(); j++) {
            std::cout << "," << shared_procs[j];
          }
          std::cout << std::endl;
        }
      }
    }
  }

  // do non-shared surface partition too
  if (part_surf) {
    result = partition_surface(nparts, entities, assignment, obj_weights); RR;
  }

  // partition shared edges and vertex
  result = partition_child_entities(1, nparts, part_surf, ghost); RR;
  result = partition_child_entities(0, nparts, part_surf); RR;

  if (debug) {
    entities.reset();
    for (int i = 0; i < n_entities; i++) {
      RefEntity *entity = entities.get_and_step();
      if (entity->entity_type_info() == typeid(Body)) {
        TDParallel *td_par = (TDParallel *) entity->get_TD(&TDParallel::is_parallel);
        CubitString st = entity->entity_name();
        DLIList<int>* sp = td_par->get_shared_proc_list();
        int n_sp = sp->size();
        sp->reset();
        for (int j = 0; j < n_sp; j++) {
          std::cout << "partitioned_" << st.c_str() << ",proc=" << sp->get_and_step() << std::endl;
        }
        DLIList<int>* gp = td_par->get_ghost_proc_list();
        int n_gp = gp->size();
        sp->reset();
        for (int j = 0; j < n_gp; j++) {
          std::cout << "partitioned_" << st.c_str() << ",ghost=" << gp->get_and_step() << std::endl;
        }
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode ZoltanPartitioner::partition_surface(const int nparts,
                                      DLIList<RefEntity*> entities,
                                      const int *assignment,
                                      std::vector<double> &obj_weights)
{
  int i;
  double ave_load = 0.;
  double* loads = new double[nparts];
  for (i = 0; i < nparts; i++) loads[i] = 0.;

  int n_entities = entities.size();
  entities.reset();
  for (i = 0; i < n_entities; i++) {
    loads[assignment[i]] += obj_weights[i];
    ave_load += obj_weights[i];
  }
  ave_load /= nparts;

  if (debug) {
    for (i = 0; i < nparts; i++) {
      std::cout << "loads_before_surface_partition[" << i << "]=" << loads[i]
                << std::endl;
    }
  }
  
  int n_iter = 0;
  bool b_stop = false;
  do {
    // get max and min load processors
    int max_proc = nparts, min_proc = 0;
    double min_load = std::numeric_limits<double>::max();
    double max_load = std::numeric_limits<double>::min();
    for (i = 0; i < nparts; i++) {
      if (loads[i] < min_load) {
        min_load = loads[i];
        min_proc = i;
      }
      if (loads[i] > max_load) {
        max_load = loads[i];
        max_proc = i;
      }
    }

    double load_diff = max_load - ave_load;
    if (load_diff > ave_load/10.) {
      bool b_moved = false;
      entities.reset();
      for (i = 0; i < n_entities; i++) {
        if (b_moved) break;
        if (assignment[i] == max_proc && // find maximum load processor bodies
            entities[i]->entity_type_info() == typeid(Body)) {
          DLIList<RefFace*> faces;
          (dynamic_cast<TopologyEntity*> (entities[i]))->ref_faces(faces);
          int n_face = faces.size();
          faces.reset();
          for (int j = 0; j < n_face; j++) {
            RefFace* face = faces.get_and_step();
            TopologyEntity *te = CAST_TO(face, TopologyEntity);
            if (te->bridge_manager()->number_of_bridges() < 2) { // non-shared
              TDParallel *td_par = (TDParallel *) face->get_TD(&TDParallel::is_parallel);
              if (td_par == NULL) { // only consider unpartitioned surfaces
                double face_load = face->measure();
                if (load_diff > min_load + face_load - ave_load) {
                  loads[max_proc] -= face_load;
                  loads[min_proc] += face_load;
                  int merge_id = TDUniqueId::get_unique_id(face);
                  DLIList<int> shared_procs;
                  shared_procs.append(min_proc);
                  shared_procs.append(max_proc);
                  td_par = new TDParallel(face, NULL, &shared_procs,
                                          NULL, merge_id, 1);

                  if (debug) {
                    std::cout << "non-shared surface " << face->id()
                              << " is moved from p" << max_proc
                              << " to p" << min_proc << std::endl;
                  }
                  b_moved = true;
                  break;
                }
              }
            }
          }
        }
      }
    }
    else b_stop = true;

    n_iter++;
  } while (!b_stop && n_iter < 50);

  if (debug) {
    for (i = 0; i < nparts; i++) {
      std::cout << "loads_after_surface_partition[" << i << "]=" << loads[i]
                << std::endl;
    }
  }

  delete loads;
  return MB_SUCCESS;
}

ErrorCode ZoltanPartitioner::partition_round_robin(const int n_part)
{
  int i, j, k;
  double* loads = new double[n_part]; // estimated loads for each processor
  double* ve_loads = new double[n_part]; // estimated loads for each processor
  for (i = 0; i < n_part; i++) {
    loads[i] = 0.0;
    ve_loads[i] = 0.0;
  }
  DLIList<RefEntity*> body_entity_list;
  gti->ref_entity_list("body", body_entity_list, CUBIT_FALSE);
  int n_entity = body_entity_list.size();
  int n_entity_proc = n_entity/n_part; // # of entities per processor
  int i_entity_proc = n_entity_proc; // entity index limit for each processor
  int proc = 0;
  RefEntity* entity;
  
  // assign processors to bodies
  body_entity_list.reset();
  for (i = 0; i < n_entity; i++) {
    if (i == i_entity_proc) {
      proc++;
      if (proc < n_part)
        i_entity_proc += n_entity_proc;
      else {
        proc %= n_part;
        i_entity_proc++;
      }
    }

    // assign to bodies
    entity = body_entity_list.get_and_step();
    DLIList<int> shared_procs;
    shared_procs.append(proc);
    TDParallel *td_par = (TDParallel *) entity->get_TD(&TDParallel::is_parallel);
    if (td_par == NULL)
      td_par = new TDParallel(entity, NULL, &shared_procs);
    loads[proc] += entity->measure();

    // assign to volumes, it should be removed in future
    DLIList<RefVolume*> volumes;
    (dynamic_cast<TopologyEntity*> (entity))->ref_volumes(volumes);
    int n_vol = volumes.size();
    volumes.reset();
    for (j = 0; j < n_vol; j++) {
      RefEntity *vol = volumes.get_and_step();
      td_par = (TDParallel *) vol->get_TD(&TDParallel::is_parallel);
      if (td_par == NULL)
        td_par = new TDParallel(vol, NULL, &shared_procs);
    }

    // add local surface load
    DLIList<RefFace*> faces;
    (dynamic_cast<TopologyEntity*> (entity))->ref_faces(faces);
    int n_face = faces.size();
    faces.reset();
    for (j = 0; j < n_face; j++) {
      RefFace* face = faces.get_and_step();
      TopologyEntity *te = CAST_TO(face, TopologyEntity);
      if (te->bridge_manager()->number_of_bridges() < 2)
        loads[proc] = loads[proc] + face->measure();
    }

    // Get all child entities
    DLIList<RefEntity*> child_list;
    RefEntity::get_all_child_ref_entities(body_entity_list, child_list);
    int n_child = child_list.size();

    // assign processors to interface entities
    child_list.reset();
    for (j = 0; j < n_child; j++) {
      entity = child_list.get_and_step();
      TopologyEntity *te = CAST_TO(entity, TopologyEntity);
      
      if (te->bridge_manager()->number_of_bridges() > 1) {
        DLIList<Body*> parent_bodies;
        DLIList<int> child_shared_procs; // Shared processors of each child entity
        (dynamic_cast<TopologyEntity*> (entity))->bodies(parent_bodies);
        int n_parent = parent_bodies.size();
	
        for (k = 0; k < n_parent; k++) {
          RefEntity *parent_vol = CAST_TO(parent_bodies.get_and_step(), RefEntity);
          TDParallel *parent_td = (TDParallel *) parent_vol->get_TD(&TDParallel::is_parallel);

          if (parent_td == NULL) {
            PRINT_ERROR("parent Volume has to be partitioned.");
            delete[] loads;
            delete[] ve_loads;
            return MB_FAILURE;
          }
          child_shared_procs.append_unique(parent_td->get_charge_proc());
        }

        if (child_shared_procs.size() > 1) { // if it is interface
          td_par = (TDParallel *) entity->get_TD(&TDParallel::is_parallel);
          if (td_par == NULL) {
            int merge_id = TDUniqueId::get_unique_id(entity);
            if (entity->entity_type_info() == typeid(RefFace)) { // face
              if (child_shared_procs.size() != 2) {
                PRINT_ERROR("Error: # of shared processors of interface surface should be 2.");
                delete[] loads;
                delete[] ve_loads;
                return MB_FAILURE;
              }

              // balance interface surface loads
              if (loads[child_shared_procs[0]] > loads[child_shared_procs[1]])
                child_shared_procs.reverse();

              loads[child_shared_procs[0]] = loads[child_shared_procs[0]] + entity->measure();
              td_par = new TDParallel(entity, NULL, &child_shared_procs, NULL, merge_id, 1);
            } // face
            else if (entity->entity_type_info() == typeid(RefEdge) ||
                entity->entity_type_info() == typeid(RefVertex)) { // edge or vertex
              // balance interface surface loads
              int min_p = child_shared_procs[0];
              int n_shared_proc = child_shared_procs.size();
              for (k = 1; k < n_shared_proc; k++) {
                if (ve_loads[child_shared_procs[k]] < ve_loads[min_p])
                  min_p = child_shared_procs[k];
              }
              ve_loads[min_p] = ve_loads[min_p] + entity->measure();
              child_shared_procs.remove(min_p);
              child_shared_procs.insert_first(min_p);
              td_par = new TDParallel(entity, NULL, &child_shared_procs, NULL, merge_id, 1);
            } // edge or vertex
          } // if (td_par == NULL)
        } // if it is interface
      } // if (te->bridge_manager()->number_of_bridges() > 1)
    } // for (j = 0; j < n_child; j++)
  } // for (i = 0; i < n_entity; i++)

  delete[] loads;
  delete[] ve_loads;

  return MB_SUCCESS;
}

// partition child entities to one of parent entity shared processors
ErrorCode ZoltanPartitioner::partition_child_entities(const int dim,
                                             const int n_part,
                                             const bool part_surf,
                                             const bool ghost)
{
  DLIList<RefEntity*> entity_list;
  if (dim == 0) gti->ref_entity_list("vertex", entity_list, CUBIT_FALSE);
  else if (dim == 1) gti->ref_entity_list("curve", entity_list, CUBIT_FALSE);
  else {
    std::cerr << "Dimention should be from 0 to 1." << std::endl;
    return MB_FAILURE;
  }

  int i, j, k;
  int n_entity = entity_list.size();
  double* loads = new double[n_part];
  for (i = 0; i < n_part; i++) loads[i] = 0.;
  entity_list.reset();

  for (i = 0; i < n_entity; i++) { // for all entities
    RefEntity* entity = entity_list.get_and_step();
    TopologyEntity *te = CAST_TO(entity, TopologyEntity);

    if (!part_surf && te->bridge_manager()->number_of_bridges() < 2) continue;

    DLIList<int> shared_procs;
    DLIList<Body*> parents;
    (dynamic_cast<TopologyEntity*> (entity))->bodies(parents);
    int n_parents = parents.size();
    std::set<int> s_proc;
    parents.reset();
    
    // get shared processors from parent bodies
    for (j = 0 ; j < n_parents; j++) {
      RefEntity *parent = parents.get_and_step();
      TDParallel *parent_td = (TDParallel *) parent->get_TD(&TDParallel::is_parallel);

      if (parent_td != NULL) {
        DLIList<int>* parent_procs = parent_td->get_shared_proc_list();
        int n_shared = parent_procs->size();
        parent_procs->reset();
        for (k = 0; k < n_shared; k++) {
          int p = parent_procs->get_and_step();
          s_proc.insert(p);
        }
      }
    }

    if (part_surf) { // also get shared procs from parent surfaces
      DLIList<RefFace*> parent_faces;
      (dynamic_cast<TopologyEntity*> (entity))->ref_faces(parent_faces);
      int n_pface = parent_faces.size();
      parent_faces.reset();
      
      // get shared processors from parent faces
      for (j = 0 ; j < n_pface; j++) {
        RefEntity *parent = parent_faces.get_and_step();
        TDParallel *parent_td = (TDParallel *) parent->get_TD(&TDParallel::is_parallel);
        
        if (parent_td != NULL) {
          DLIList<int>* parent_procs = parent_td->get_shared_proc_list();
          int n_shared = parent_procs->size();
          parent_procs->reset();

          for (k = 0; k < n_shared; k++) {
            int p = parent_procs->get_and_step();
            s_proc.insert(p);
          }
        }
      }
    }
    
    // find the minimum load processor and put it
    // at the front of the shared_procs list
    if (s_proc.size() > 1) {
      int min_proc = 0;
      double min_load = std::numeric_limits<double>::max();
      std::set<int>::iterator iter = s_proc.begin();
      std::set<int>::iterator end_iter = s_proc.end();
      for (; iter != end_iter; ++iter) {
        if (loads[*iter] < min_load) {
          min_load = loads[*iter];
          min_proc = *iter;
        }
      }

      if (dim == 1) loads[min_proc] += entity->measure();
      else if (dim == 0) loads[min_proc] += 1.;
      shared_procs.append(min_proc);
      iter = s_proc.begin();
      end_iter = s_proc.end();
      for (; iter != end_iter; ++iter) {
        if (*iter != min_proc) {
          shared_procs.append(*iter);
        }
      }
      
      // add ghost geometries to shared processors for edge
      if (ghost) {
        parents.reset();
        for (j = 0; j < n_parents; j++) { // for all parent bodies
          RefEntity *parent_vol = CAST_TO(parents.get_and_step(), RefEntity);
          TDParallel *parent_td = (TDParallel *) parent_vol->get_TD(&TDParallel::is_parallel);
          int n_shared_proc = shared_procs.size();
          
          for (k = 0; k < n_shared_proc; k++) {
            parent_td->add_ghost_proc(shared_procs[k]);
          }
        }
      }
      
      // initialize tool data
      int merge_id = TDUniqueId::get_unique_id(entity);
      TDParallel *td_par = (TDParallel *) entity->get_TD(&TDParallel::is_parallel);
      if (td_par == NULL) td_par = new TDParallel(entity, NULL, &shared_procs,
                                                  NULL, merge_id, 1);
    }
  }

  delete loads;
  return MB_SUCCESS;
}
#endif

ErrorCode ZoltanPartitioner::write_partition(const int nparts,
                                    Range &elems, 
                                    const int *assignment,
                                    const bool write_as_sets,
                                    const bool write_as_tags)
{
  ErrorCode result;

    // get the partition set tag
  Tag part_set_tag;
  int dum_id = -1, i;
  result = mbImpl->tag_get_handle("PARALLEL_PARTITION", 1, MB_TYPE_INTEGER,
                                  part_set_tag, MB_TAG_SPARSE|MB_TAG_CREAT, &dum_id); RR;
  
    // get any sets already with this tag, and clear them
  Range tagged_sets;
  result = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &part_set_tag, NULL, 1,
                                                tagged_sets, Interface::UNION); RR;
  if (!tagged_sets.empty()) {
    result = mbImpl->clear_meshset(tagged_sets); RR;
    if (!write_as_sets) {
      result = mbImpl->tag_delete_data(part_set_tag, tagged_sets); RR;
    }
  }

  if (write_as_sets) {
      // first, create partition sets and store in vector
    partSets.clear();
  
    if (nparts > (int) tagged_sets.size()) {
        // too few partition sets - create missing ones
      int num_new = nparts - tagged_sets.size();
      for (i = 0; i < num_new; i++) {
        EntityHandle new_set;
        result = mbImpl->create_meshset(MESHSET_SET, new_set); RR;
        tagged_sets.insert(new_set);
      }
    }
    else if (nparts < (int) tagged_sets.size()) {
        // too many partition sets - delete extras
      int num_del = tagged_sets.size() - nparts;
      for (i = 0; i < num_del; i++) {
        EntityHandle old_set = tagged_sets.pop_back();
        result = mbImpl->delete_entities(&old_set, 1); RR;
      }
    }
    // assign partition sets to vector
    partSets.swap(tagged_sets);

      // write a tag to those sets denoting they're partition sets, with a value of the
      // proc number
    int *dum_ids = new int[nparts];
    for (i = 0; i < nparts; i++) dum_ids[i] = i;
  
    result = mbImpl->tag_set_data(part_set_tag, partSets, dum_ids); RR;
    // found out by valgrind when we run mbpart 
    delete [] dum_ids;
    dum_ids = NULL;

    // assign entities to the relevant sets
    std::vector<EntityHandle> tmp_part_sets;
    //int N = (int)elems.size();
    std::copy(partSets.begin(), partSets.end(), std::back_inserter(tmp_part_sets));
    /*Range::reverse_iterator riter;
    for (i = N-1, riter = elems.rbegin(); riter != elems.rend(); ++riter, i--) {
      int assigned_part = assignment[i];
      part_ranges[assigned_part].insert(*riter);
      //result = mbImpl->add_entities(tmp_part_sets[assignment[i]], &(*rit), 1); RR;
    }*/

    Range::iterator rit;
    for (i = 0, rit = elems.begin(); rit != elems.end(); ++rit, i++) {
      result = mbImpl->add_entities(tmp_part_sets[assignment[i]], &(*rit), 1); RR;
    }
    /*for (i=0; i<nparts; i++)
    {
      result = mbImpl->add_entities(tmp_part_sets[i], part_ranges[i]); RR;
    }
    delete [] part_ranges;*/
      // check for empty sets, warn if there are any
    Range empty_sets;
    for (rit = partSets.begin(); rit != partSets.end(); ++rit) {
      int num_ents = 0;
      result = mbImpl->get_number_entities_by_handle(*rit, num_ents);
      if (MB_SUCCESS != result || !num_ents) empty_sets.insert(*rit);
    }
    if (!empty_sets.empty()) {
      std::cout << "WARNING: " << empty_sets.size() << " empty sets in partition: ";
      for (rit = empty_sets.begin(); rit != empty_sets.end(); ++rit)
        std::cout << *rit << " ";
      std::cout << std::endl;
    }
  }
  
  if (write_as_tags) {
      // allocate integer-size partitions
    result = mbImpl->tag_set_data(part_set_tag, elems, assignment); RR;
  }
  
  return MB_SUCCESS;
}

void ZoltanPartitioner::SetRCB_Parameters()
{
  if (mbpc->proc_config().proc_rank() == 0) std::cout << "\nRecursive Coordinate Bisection" << std::endl;
  // General parameters:

  myZZ->Set_Param("DEBUG_LEVEL", "0");     // no debug messages
  myZZ->Set_Param("LB_METHOD", "RCB");     // recursive coordinate bisection

  // RCB parameters:

  myZZ->Set_Param("RCB_OUTPUT_LEVEL", "2");
  myZZ->Set_Param("KEEP_CUTS", "1");              // save decomposition
  myZZ->Set_Param("RCB_RECTILINEAR_BLOCKS", "1"); // don't split point on boundary
}

void ZoltanPartitioner::SetRIB_Parameters()
{
  if (mbpc->proc_config().proc_rank() == 0) std::cout << "\nRecursive Inertial Bisection" << std::endl;
  // General parameters:

  myZZ->Set_Param("DEBUG_LEVEL", "0");     // no debug messages
  myZZ->Set_Param("LB_METHOD", "RIB");     // Recursive Inertial Bisection

  // RIB parameters:

  myZZ->Set_Param("KEEP_CUTS", "1");              // save decomposition
  myZZ->Set_Param("AVERAGE_CUTS", "1");
}

void ZoltanPartitioner::SetHSFC_Parameters()
{
  if (mbpc->proc_config().proc_rank() == 0) std::cout << "\nHilbert Space Filling Curve" << std::endl;
  // General parameters:

  myZZ->Set_Param("DEBUG_LEVEL", "0");     // no debug messages
  myZZ->Set_Param("LB_METHOD", "HSFC");    // perform Hilbert space filling curve

  // HSFC parameters:

  myZZ->Set_Param("KEEP_CUTS", "1");              // save decomposition
}

void ZoltanPartitioner::SetHypergraph_Parameters(const char *phg_method)
{
  if (mbpc->proc_config().proc_rank() == 0) std::cout << "\nHypergraph (or PHG): " << std::endl;
  // General parameters:

  myZZ->Set_Param("DEBUG_LEVEL", "0");     // no debug messages
  myZZ->Set_Param("LB_METHOD", "Hypergraph");     // Hypergraph (or PHG)

  // Hypergraph (or PHG) parameters:
  myZZ->Set_Param("PHG_COARSEPARTITION_METHOD",phg_method);//CoarsePartitionMethod
}

void ZoltanPartitioner::SetPARMETIS_Parameters(const char *parmetis_method)
{
  if (mbpc->proc_config().proc_rank() == 0) std::cout << "\nPARMETIS: " << parmetis_method << std::endl;
  // General parameters:

  myZZ->Set_Param("DEBUG_LEVEL", "0");     // no debug messages
  myZZ->Set_Param("LB_METHOD", "PARMETIS");     // the ParMETIS library

  // PARMETIS parameters:

  myZZ->Set_Param("PARMETIS_METHOD", parmetis_method); // method in the library
}

void ZoltanPartitioner::SetOCTPART_Parameters(const char *oct_method)
{
  if (mbpc->proc_config().proc_rank() == 0) std::cout << "\nOctree Partitioning: " << oct_method
			   << std::endl;
  // General parameters:

  myZZ->Set_Param("DEBUG_LEVEL", "0");     // no debug messages
  myZZ->Set_Param("LB_METHOD", "OCTPART");     // octree partitioning

  // OCTPART parameters:

  myZZ->Set_Param("OCT_METHOD", oct_method); // the SFC to be used
  myZZ->Set_Param("OCT_OUTPUT_LEVEL", "3");
}

int ZoltanPartitioner::mbInitializePoints(int npts, double *pts, int *ids, 
                                 int *adjs, int *length,
                                 double *obj_weights, double *edge_weights,
                                 int *parts, bool part_geom)
{
  unsigned int i;
  int j;
  int *numPts, *nborProcs = NULL;
  int sum, ptsPerProc, ptsAssigned, mySize;
  MPI_Status stat;
  double *sendPts;
  int *sendIds;
  int *sendEdges = NULL;
  int *sendNborId = NULL;
  int *sendProcs;

  if (mbpc->proc_config().proc_rank() == 0) {
      /* divide pts to start */

    numPts = (int *)malloc(sizeof(int) * mbpc->proc_config().proc_size());
    ptsPerProc = npts / mbpc->proc_config().proc_size();
    ptsAssigned = 0;

    for (i = 0; i < mbpc->proc_config().proc_size() - 1; i++) {
      numPts[i] = ptsPerProc;
      ptsAssigned += ptsPerProc;
    }

    numPts[mbpc->proc_config().proc_size()-1] = npts - ptsAssigned;

    mySize = numPts[mbpc->proc_config().proc_rank()];
    sendPts = pts + (3 * numPts[0]);
    sendIds = ids + numPts[0];
    sum = 0; // possible no adjacency sent
    if (!part_geom)
    {
      sendEdges = length + numPts[0];


      for (j = 0; j < numPts[0]; j++)
        sum += length[j];

      sendNborId = adjs + sum;

      for (j = numPts[0]; j < npts; j++)
        sum += length[j];

      nborProcs = (int *)malloc(sizeof(int) * sum);
    }
    for (j = 0; j < sum; j++)
      if ((i = adjs[j] / ptsPerProc) < mbpc->proc_config().proc_size())
        nborProcs[j] = i;
      else
        nborProcs[j] = mbpc->proc_config().proc_size() - 1;

    sendProcs = nborProcs + (sendNborId - adjs);

    for (i = 1; i < mbpc->proc_config().proc_size(); i++) {
      MPI_Send(&numPts[i], 1, MPI_INT, i, 0x00, MPI_COMM_WORLD);
      MPI_Send(sendPts, 3 * numPts[i], MPI_DOUBLE, i, 0x01, MPI_COMM_WORLD);
      MPI_Send(sendIds, numPts[i], MPI_INT, i, 0x03, MPI_COMM_WORLD);
      MPI_Send(sendEdges, numPts[i], MPI_INT, i, 0x06, MPI_COMM_WORLD);
      sum = 0;

      for (j = 0; j < numPts[i]; j++)
        sum += sendEdges[j];

      MPI_Send(sendNborId, sum, MPI_INT, i, 0x07, MPI_COMM_WORLD);
      MPI_Send(sendProcs, sum, MPI_INT, i, 0x08, MPI_COMM_WORLD);
      sendPts += (3 * numPts[i]);
      sendIds += numPts[i];
      sendEdges += numPts[i];
      sendNborId += sum;
      sendProcs += sum;
    }

    free(numPts);
  }
  else {
    MPI_Recv(&mySize, 1, MPI_INT, 0, 0x00, MPI_COMM_WORLD, &stat);
    pts = (double *)malloc(sizeof(double) * 3 * mySize);
    ids = (int *)malloc(sizeof(int) * mySize);
    length = (int *)malloc(sizeof(int) * mySize);
    if (obj_weights != NULL) obj_weights = (double *)malloc(sizeof(double) * mySize);
    MPI_Recv(pts, 3 * mySize, MPI_DOUBLE, 0, 0x01, MPI_COMM_WORLD, &stat);
    MPI_Recv(ids, mySize, MPI_INT, 0, 0x03, MPI_COMM_WORLD, &stat);
    MPI_Recv(length, mySize, MPI_INT, 0, 0x06, MPI_COMM_WORLD, &stat);
    sum = 0;

    for (j = 0; j < mySize; j++)
      sum += length[j];

    adjs = (int *)malloc(sizeof(int) * sum);
    if (edge_weights != NULL) edge_weights = (double *)malloc(sizeof(double) * sum);
    nborProcs = (int *)malloc(sizeof(int) * sum);
    MPI_Recv(adjs, sum, MPI_INT, 0, 0x07, MPI_COMM_WORLD, &stat);
    MPI_Recv(nborProcs, sum, MPI_INT, 0, 0x08, MPI_COMM_WORLD, &stat);
  }     

  Points = pts;
  GlobalIds = ids;  
  NumPoints = mySize;
  NumEdges = length;
  NborGlobalId = adjs;
  NborProcs = nborProcs;
  ObjWeights = obj_weights;
  EdgeWeights = edge_weights;
  Parts = parts;

  return mySize;
}     

void ZoltanPartitioner::mbFinalizePoints(int npts, int numExport,
                                ZOLTAN_ID_PTR exportLocalIDs, int *exportProcs,
                                int **assignment)
{
  int *MyAssignment;
  int i;
  int numPts;
  MPI_Status stat;
  int *recvA;

  /* assign pts to start */

  if (mbpc->proc_config().proc_rank() == 0)
    MyAssignment = (int *)malloc(sizeof(int) * npts);
  else
    MyAssignment = (int *)malloc(sizeof(int) * NumPoints);

  for (i = 0; i < NumPoints; i++)
    MyAssignment[i] = mbpc->proc_config().proc_rank();

  for (i = 0; i < numExport; i++)
    MyAssignment[exportLocalIDs[i]] = exportProcs[i];

  if (mbpc->proc_config().proc_rank() == 0) {
      /* collect pts */
      recvA = MyAssignment + NumPoints;

    for (i = 1; i< (int) mbpc->proc_config().proc_size(); i++) {
      MPI_Recv(&numPts, 1, MPI_INT, i, 0x04, MPI_COMM_WORLD, &stat);
      MPI_Recv(recvA, numPts, MPI_INT, i, 0x05, MPI_COMM_WORLD, &stat);
      recvA += numPts;
    }

    *assignment = MyAssignment;
  }
  else {
    MPI_Send(&NumPoints, 1, MPI_INT, 0, 0x04, MPI_COMM_WORLD);
    MPI_Send(MyAssignment, NumPoints, MPI_INT, 0, 0x05, MPI_COMM_WORLD);
    free(MyAssignment);
  }
}

int ZoltanPartitioner::mbGlobalSuccess(int rc)
{
  int fail = 0;
  unsigned int i;
  int *vals = (int *)malloc(mbpc->proc_config().proc_size() * sizeof(int));

  MPI_Allgather(&rc, 1, MPI_INT, vals, 1, MPI_INT, MPI_COMM_WORLD);

  for (i = 0; i<mbpc->proc_config().proc_size(); i++) {
    if (vals[i] != ZOLTAN_OK) {
      if (0 == mbpc->proc_config().proc_rank()){
        mbShowError(vals[i], "Result on process ");
      }
      fail = 1;
    }
  }

  free(vals);
  return fail;
}

void ZoltanPartitioner::mbPrintGlobalResult(const char *s, 
                                   int begin, int import, int exp, int change)
{
  unsigned int i;
  int *v1 = (int *)malloc(4 * sizeof(int));
  int *v2 = NULL;
  int *v;

  v1[0] = begin;
  v1[1] = import;
  v1[2] = exp;
  v1[3] = change;

  if (mbpc->proc_config().proc_rank() == 0) {
    v2 = (int *)malloc(4 * mbpc->proc_config().proc_size() * sizeof(int));
  }

  MPI_Gather(v1, 4, MPI_INT, v2, 4, MPI_INT, 0, MPI_COMM_WORLD);

  if (mbpc->proc_config().proc_rank() == 0) {
    fprintf(stdout, "======%s======\n", s);
    for (i = 0, v = v2; i < mbpc->proc_config().proc_size(); i++, v += 4) {
      fprintf(stdout,"%u: originally had %d, import %d, exp %d, %s\n",
        i, v[0], v[1], v[2],
        v[3] ? "a change of partitioning" : "no change");
    }
    fprintf(stdout,"==========================================\n");
    fflush(stdout); 

    free(v2);
  }

  free(v1);
}

void ZoltanPartitioner::mbShowError(int val, const char *s)
{
  if (s)
    printf("%s ", s);

  switch (val) {
    case ZOLTAN_OK:
      printf("%d: SUCCESSFUL\n", mbpc->proc_config().proc_rank());
      break;
    case ZOLTAN_WARN:
      printf("%d: WARNING\n", mbpc->proc_config().proc_rank());
      break;
    case ZOLTAN_FATAL:
      printf("%d: FATAL ERROR\n", mbpc->proc_config().proc_rank());
      break;
    case ZOLTAN_MEMERR:
      printf("%d: MEMORY ALLOCATION ERROR\n", mbpc->proc_config().proc_rank());
      break;
    default:
      printf("%d: INVALID RETURN CODE\n", mbpc->proc_config().proc_rank());
      break;
    }
}

/**********************
** call backs
**********************/

int mbGetNumberOfAssignedObjects(void * /* userDefinedData */, int *err)
{
  *err = 0;
  return NumPoints;
}

void mbGetObjectList(void * /* userDefinedData */, int /* numGlobalIds */, int /* numLids */,
  ZOLTAN_ID_PTR gids, ZOLTAN_ID_PTR lids, int wgt_dim, float *obj_wgts,
  int *err)
{
  for (int i = 0; i < NumPoints; i++) {
    gids[i] = GlobalIds[i];
    lids[i] = i;
    if (wgt_dim > 0)
      obj_wgts[i] = ObjWeights[i];
  }

  *err = 0;
}

int mbGetObjectSize(void * /* userDefinedData */, int *err)
{
  *err = 0; 
  return 3;
}

void mbGetObject(void * /* userDefinedData */, int /* numGlobalIds */, int /* numLids */, int numObjs,
  ZOLTAN_ID_PTR /* gids */, ZOLTAN_ID_PTR lids, int numDim, double *pts, int *err)
{ 
  int i, id, id3;
  int next = 0;

  if (numDim != 3) {
    *err = 1;         
    return;
  }

  for (i = 0; i < numObjs; i++) {
    id = lids[i];

    if ((id < 0) || (id >= NumPoints)) {
      *err = 1;
      return;
    }

    id3 = lids[i] * 3;

    pts[next++] = Points[id3];
    pts[next++] = Points[id3 + 1];
    pts[next++] = Points[id3 + 2];
  }
} 

void mbGetNumberOfEdges(void * /* userDefinedData */, int /* numGlobalIds */, int /* numLids */,
			int numObjs, 
			ZOLTAN_ID_PTR /* gids */, ZOLTAN_ID_PTR lids,	int *numEdges,
			int *err)
{
  int i, id;
  int next = 0;

  for (i = 0; i < numObjs; i++) {
    id = lids[i];

    if ((id < 0) || (id >= NumPoints)) {
      *err = 1;
      return;
    }

    numEdges[next++] = NumEdges[id];
  }
}

void mbGetEdgeList(void * /* userDefinedData */, int /* numGlobalIds */, int /* numLids */,
		   int numObjs,
		   ZOLTAN_ID_PTR /* gids */, ZOLTAN_ID_PTR lids, int * /* numEdges */,
		   ZOLTAN_ID_PTR nborGlobalIds, int *nborProcs, int wgt_dim,
		   float *edge_wgts, int *err)
{
  int i, id, idSum, j;
  int next = 0;

  for (i = 0; i < numObjs; i++) {
    id = lids[i];

    if ((id < 0) || (id >= NumPoints)) {
	    *err = 1;
      return;
	  }

    idSum = 0;

    for (j = 0; j < id; j++)
      idSum += NumEdges[j];

    for (j = 0; j < NumEdges[id]; j++) {
      nborGlobalIds[next] = NborGlobalId[idSum];
      nborProcs[next] = NborProcs[idSum];
      if (wgt_dim > 0) edge_wgts[next] = EdgeWeights[idSum];
        next++;
        idSum++;
    }
  }
}

void mbGetPart(void * /* userDefinedData */, int /* numGlobalIds */, int /* numLids */,
               int numObjs, ZOLTAN_ID_PTR /* gids */, ZOLTAN_ID_PTR lids,
               int *part, int *err)
{
  int i, id;
  int next = 0;

  for (i = 0; i < numObjs; i++) {
    id = lids[i];

    if ((id < 0) || (id >= NumPoints)) {
      *err = 1;
      return;
    }

    part[next++] = Parts[id];
  }
}
