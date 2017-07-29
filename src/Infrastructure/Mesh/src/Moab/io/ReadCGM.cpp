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
#ifdef WIN32
#pragma warning(disable:4786)
#endif

#include "CGMConfig.h"
#include "GeometryQueryTool.hpp"
#include "ModelQueryEngine.hpp"
#include "RefEntityName.hpp"
#include "GMem.hpp"

#include "RefGroup.hpp"
#include "RefVolume.hpp"
#include "RefFace.hpp"
#include "RefEdge.hpp"
#include "RefVertex.hpp"

#include "SenseEntity.hpp"
#include "Surface.hpp"
#include "Curve.hpp"
#include "Body.hpp"
#include "InitCGMA.hpp"

#include "moab/Core.hpp"
#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "MBTagConventions.hpp"
#include "moab/FileOptions.hpp"

#include "moab/GeomTopoTool.hpp"

#include "CubitCompat.hpp"

#include <stdio.h>
#include <algorithm>
#include <assert.h>
#include <iostream>

#include "ReadCGM.hpp"
#include "moab/ReadUtilIface.hpp"

namespace moab {

#define GF_CUBIT_FILE_TYPE    "CUBIT"
#define GF_STEP_FILE_TYPE     "STEP"
#define GF_IGES_FILE_TYPE     "IGES"
#define GF_OCC_BREP_FILE_TYPE "OCC"
#define GF_FACET_FILE_TYPE    "FACET"

ReaderIface* ReadCGM::factory(Interface* iface)
{
  return new ReadCGM(iface);
}

ReadCGM::ReadCGM(Interface *impl)
  : geom_tag(0), id_tag(0), name_tag(0), category_tag(0), faceting_tol_tag(0),
    geometry_resabs_tag(0)
{
  assert(NULL != impl);
  mdbImpl = impl;
  myGeomTool = new GeomTopoTool(impl);
  impl->query_interface(readUtilIface);
  assert(NULL != readUtilIface);

  // initialise counters
  failed_curve_count = 0;
  failed_surface_count = 0;

  ErrorCode rval;

  // get some tag handles
  int negone = -1, zero = 0 /*, negonearr[] = {-1, -1, -1, -1}*/;
  rval = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER,
                                 geom_tag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
  assert(!rval);
  rval = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                 id_tag, MB_TAG_DENSE | MB_TAG_CREAT, &zero);
  assert(!rval);
  rval = mdbImpl->tag_get_handle(NAME_TAG_NAME, NAME_TAG_SIZE, MB_TYPE_OPAQUE,
                                 name_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
  assert(!rval);

  rval = mdbImpl->tag_get_handle(CATEGORY_TAG_NAME, CATEGORY_TAG_SIZE, MB_TYPE_OPAQUE,
                                 category_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
  assert(!rval);
  rval = mdbImpl->tag_get_handle("FACETING_TOL", 1, MB_TYPE_DOUBLE, faceting_tol_tag,
                                 MB_TAG_SPARSE | MB_TAG_CREAT);
  assert(!rval);
  rval = mdbImpl->tag_get_handle("GEOMETRY_RESABS", 1, MB_TYPE_DOUBLE, 
                                 geometry_resabs_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
  assert(!rval);
#ifdef NDEBUG
  if (!rval) {}; // Line to avoid compiler warning about variable set but not used
#endif
}

ReadCGM::~ReadCGM()
{
  mdbImpl->release_interface(readUtilIface);
  delete myGeomTool;
}

ErrorCode ReadCGM::read_tag_values(const char* /* file_name */,
                                   const char* /* tag_name */,
                                   const FileOptions& /* opts */,
                                   std::vector<int>& /* tag_values_out */,
                                   const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

// Sets options passed into ReadCGM::load_file
ErrorCode ReadCGM::set_options(const FileOptions& opts,
                               int& norm_tol,
                               double& faceting_tol,
                               double& len_tol,
                               bool& act_att,
                               bool& verbose_warnings,
			       bool& fatal_on_curves)
{
  ErrorCode rval;

  // Default Values
  int DEFAULT_NORM = 5;
  double DEFAULT_FACET_TOL = 0.001;
  double DEFAULT_LEN_TOL = 0.0;
  act_att = true;

  // Check for the options.
  if (MB_SUCCESS != opts.get_int_option("FACET_NORMAL_TOLERANCE", norm_tol))
    norm_tol = DEFAULT_NORM;

  if (MB_SUCCESS != opts.get_real_option("FACET_DISTANCE_TOLERANCE", faceting_tol))
    faceting_tol = DEFAULT_FACET_TOL;

  if (MB_SUCCESS != opts.get_real_option("MAX_FACET_EDGE_LENGTH", len_tol))
    len_tol = DEFAULT_LEN_TOL;

  if (MB_SUCCESS == opts.get_null_option("VERBOSE_CGM_WARNINGS"))
    verbose_warnings = true;

  if (MB_SUCCESS == opts.get_null_option("FATAL_ON_CURVES"))
    fatal_on_curves = true;



  const char* name = "CGM_ATTRIBS";
  const char* value = "no";
  rval = opts.match_option(name, value);
  if (MB_SUCCESS == rval)
    act_att = false; 

  return MB_SUCCESS;
}

ErrorCode ReadCGM::create_entity_sets(std::map<RefEntity*, EntityHandle> (&entmap)[5])
{
  ErrorCode rval;
  const char geom_categories[][CATEGORY_TAG_SIZE] =
              {"Vertex\0", "Curve\0", "Surface\0", "Volume\0", "Group\0"};
  const char* const names[] = {"Vertex", "Curve", "Surface", "Volume"};
  DLIList<RefEntity*> entlist;

  for (int dim = 0; dim < 4; dim++) {
    entlist.clean_out();
    GeometryQueryTool::instance()->ref_entity_list(names[dim], entlist, true);
    entlist.reset();

    for (int i = entlist.size(); i--; ) {
      RefEntity* ent = entlist.get_and_step();
      EntityHandle handle;
      // Create the new meshset
      rval = mdbImpl->create_meshset(dim == 1 ? MESHSET_ORDERED : MESHSET_SET, handle);
      if (MB_SUCCESS != rval)
        return rval;

      // Map the geom reference entity to the corresponding moab meshset
      entmap[dim][ent] = handle;

      // Create tags for the new meshset
      rval = mdbImpl->tag_set_data(geom_tag, &handle, 1, &dim);
      if (MB_SUCCESS != rval)
        return rval;

      int id = ent->id();
      rval = mdbImpl->tag_set_data(id_tag, &handle, 1, &id);
      if (MB_SUCCESS != rval)
        return rval;

      rval = mdbImpl->tag_set_data(category_tag, &handle, 1, &geom_categories[dim]);
      if (MB_SUCCESS != rval)
        return rval;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadCGM::create_topology(std::map<RefEntity*, EntityHandle> (&entitymap)[5])
{
  ErrorCode rval;
  DLIList<RefEntity*> entitylist;
  std::map<RefEntity*, EntityHandle>::iterator ci;

  for (int dim = 1; dim < 4; ++dim) {
    for (ci = entitymap[dim].begin(); ci != entitymap[dim].end(); ++ci) {
      entitylist.clean_out();
      ci->first->get_child_ref_entities(entitylist);

      entitylist.reset();
      for (int i = entitylist.size(); i--; ) {
        RefEntity* ent = entitylist.get_and_step();
        EntityHandle h = entitymap[dim - 1][ent];
        rval = mdbImpl->add_parent_child(ci->second, h);
        if (MB_SUCCESS != rval)
          return rval;
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadCGM::store_surface_senses(std::map<RefEntity*, EntityHandle>& surface_map,
                                        std::map<RefEntity*, EntityHandle>& volume_map)
{
  ErrorCode rval;
  std::map<RefEntity*, EntityHandle>::iterator ci;

  for (ci = surface_map.begin(); ci != surface_map.end(); ++ci) {
    RefFace* face = (RefFace*)(ci->first);
    BasicTopologyEntity *forward = 0, *reverse = 0;
    for (SenseEntity* cf = face->get_first_sense_entity_ptr();
         cf; cf = cf->next_on_bte()) {
      BasicTopologyEntity* vol = cf->get_parent_basic_topology_entity_ptr();
      // Allocate vol to the proper topology entity (forward or reverse)
      if (cf->get_sense() == CUBIT_UNKNOWN ||
          cf->get_sense() != face->get_surface_ptr()->bridge_sense()) {
        // Check that each surface has a sense for only one volume
        if (reverse) {
          std::cout << "Surface " << face->id() << " has reverse sense " <<
                       "with multiple volume " << reverse->id() << " and " <<
                       "volume " << vol->id() << std::endl;
          return MB_FAILURE;
        }
        reverse = vol;
      }
      if (cf->get_sense() == CUBIT_UNKNOWN ||
          cf->get_sense() == face->get_surface_ptr()->bridge_sense()) {
        // Check that each surface has a sense for only one volume
        if (forward) {
          std::cout << "Surface " << face->id() << " has forward sense " <<
                       "with multiple volume " << forward->id() << " and " <<
                       "volume " << vol->id() << std::endl;
          return MB_FAILURE;
        }
        forward = vol;
      }
    }

    if (forward) {
      rval = myGeomTool->set_sense(ci->second, volume_map[forward], SENSE_FORWARD);
      if (MB_SUCCESS != rval)
        return rval;
    }
    if (reverse) {
      rval = myGeomTool->set_sense(ci->second, volume_map[reverse], SENSE_REVERSE);
      if (MB_SUCCESS != rval)
        return rval;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadCGM::store_curve_senses(std::map<RefEntity*, EntityHandle>& curve_map,
                                      std::map<RefEntity*, EntityHandle>& surface_map)
{
  ErrorCode rval;
  std::vector<EntityHandle> ents;
  std::vector<int> senses;
  std::map<RefEntity*, EntityHandle>::iterator ci;
  for (ci = curve_map.begin(); ci != curve_map.end(); ++ci) {
    RefEdge* edge = (RefEdge*)(ci->first);
    ents.clear();
    senses.clear();
    for (SenseEntity* ce = edge->get_first_sense_entity_ptr();
         ce; ce = ce->next_on_bte()) {
      BasicTopologyEntity* fac = ce->get_parent_basic_topology_entity_ptr();
      EntityHandle face = surface_map[fac];
      if (ce->get_sense() == CUBIT_UNKNOWN ||
          ce->get_sense() != edge->get_curve_ptr()->bridge_sense()) {
        ents.push_back(face);
        senses.push_back(SENSE_REVERSE);
      }
      if (ce->get_sense() == CUBIT_UNKNOWN ||
          ce->get_sense() == edge->get_curve_ptr()->bridge_sense()) {
        ents.push_back(face);
        senses.push_back(SENSE_FORWARD);
      }
    }

    rval = myGeomTool->set_senses(ci->second, ents, senses);
    if (MB_SUCCESS != rval)
      return rval;
  }
  return MB_SUCCESS;
}

ErrorCode ReadCGM::store_groups(std::map<RefEntity*, EntityHandle> (&entitymap)[5])
{
  ErrorCode rval;

  // Create entity sets for all ref groups
  rval = create_group_entsets(entitymap[4]);
  if (rval != MB_SUCCESS)
    return rval;

  // Store group names and entities in the mesh
  rval = store_group_content(entitymap);
  if (rval != MB_SUCCESS)
    return rval;

  return MB_SUCCESS;
}

ErrorCode ReadCGM::create_group_entsets(std::map<RefEntity*, EntityHandle>& group_map)
{
  ErrorCode rval;
  const char geom_categories[][CATEGORY_TAG_SIZE] =
      {"Vertex\0", "Curve\0", "Surface\0", "Volume\0", "Group\0"};
  DLIList<RefEntity*> entitylist;
  // Create entity sets for all ref groups
  std::vector<Tag> extra_name_tags;
#if CGM_MAJOR_VERSION > 13
  DLIList<CubitString> name_list;
#else
  DLIList<CubitString*> name_list;
#endif
  entitylist.clean_out();
  // Get all entity groups from the CGM model
  GeometryQueryTool::instance()->ref_entity_list("group", entitylist);
  entitylist.reset();
  // Loop over all groups
  for (int i = entitylist.size(); i--; ) {
    // Take the next group
    RefEntity* grp = entitylist.get_and_step();
    name_list.clean_out();
// Get the names of all entities in this group from the solid model
#if CGM_MAJOR_VERSION > 13
    RefEntityName::instance()->get_refentity_name(grp, name_list);
#else
    // True argument is optional, but for large multi-names situation, it should save
    // some cpu time
    RefEntityName::instance()->get_refentity_name(grp, name_list, true);
#endif
    if (name_list.size() == 0)
      continue;
    // Set pointer to first name of the group and set the first name to name1
    name_list.reset();
#if  CGM_MAJOR_VERSION > 13
    CubitString name1 = name_list.get();
#else
    CubitString name1 = *name_list.get();
#endif
    // Create entity handle for the group
    EntityHandle h;
    rval = mdbImpl->create_meshset(MESHSET_SET, h);
    if (MB_SUCCESS != rval)
      return rval;
    // Set tag data for the group
    char namebuf[NAME_TAG_SIZE];
    memset(namebuf, '\0', NAME_TAG_SIZE);
    strncpy(namebuf, name1.c_str(), NAME_TAG_SIZE - 1);
    if (name1.length() >= (unsigned)NAME_TAG_SIZE)
      std::cout << "WARNING: group name '" << name1.c_str()
                << "' truncated to '" << namebuf << "'" << std::endl;
    rval = mdbImpl->tag_set_data(name_tag, &h, 1, namebuf);
    if (MB_SUCCESS != rval)
      return MB_FAILURE;

    int id = grp->id();
    rval = mdbImpl->tag_set_data(id_tag, &h, 1, &id);
    if (MB_SUCCESS != rval)
      return MB_FAILURE;

    rval = mdbImpl->tag_set_data(category_tag, &h, 1, &geom_categories[4]);
    if (MB_SUCCESS != rval)
      return MB_FAILURE;
    // Check for extra group names
    if (name_list.size() > 1) {
      for (int j = extra_name_tags.size(); j < name_list.size(); ++j) {
        sprintf(namebuf, "EXTRA_%s%d", NAME_TAG_NAME, j);
        Tag t;
        rval = mdbImpl->tag_get_handle(namebuf, NAME_TAG_SIZE, MB_TYPE_OPAQUE, t, MB_TAG_SPARSE | MB_TAG_CREAT);
        assert(!rval);
        extra_name_tags.push_back(t);
      }
      // Add extra group names to the group handle
      for (int j = 0; j < name_list.size(); ++j) {
#if  CGM_MAJOR_VERSION>13
        name1 = name_list.get_and_step();
#else
        name1 = *name_list.get_and_step();
#endif
        memset(namebuf, '\0', NAME_TAG_SIZE);
        strncpy(namebuf, name1.c_str(), NAME_TAG_SIZE - 1);
        if (name1.length() >= (unsigned)NAME_TAG_SIZE)
          std::cout << "WARNING: group name '" << name1.c_str()
                    << "' truncated to '" << namebuf << "'" << std::endl;
        rval = mdbImpl->tag_set_data(extra_name_tags[j], &h, 1, namebuf);
        if (MB_SUCCESS != rval)
          return MB_FAILURE;
      }
    }
    // Add the group handle
    group_map[grp] = h;
  }

  return MB_SUCCESS;
}

ErrorCode ReadCGM::store_group_content(std::map<RefEntity*, EntityHandle> (&entitymap)[5])
{
  ErrorCode rval;
  DLIList<RefEntity*> entlist;
  std::map<RefEntity*, EntityHandle>::iterator ci;
  // Store contents for each group
  entlist.reset();
  for (ci = entitymap[4].begin(); ci != entitymap[4].end(); ++ci) {
    RefGroup* grp = (RefGroup*)(ci->first);
    entlist.clean_out();
    grp->get_child_ref_entities(entlist);

    Range entities;
    while (entlist.size()) {
      RefEntity* ent = entlist.pop();
      int dim = ent->dimension();

      if (dim < 0) {
        Body* body;
        if (entitymap[4].find(ent) != entitymap[4].end()) {
          // Child is another group; examine its contents
          entities.insert(entitymap[4][ent]);
        }
        else if ((body = dynamic_cast<Body*>(ent)) != NULL) {
          // Child is a CGM Body, which presumably comprises some volumes--
          // extract volumes as if they belonged to group.
          DLIList<RefVolume*> vols;
          body->ref_volumes(vols);
          for (int vi = vols.size(); vi--; ) {
            RefVolume* vol = vols.get_and_step();
            if (entitymap[3].find(vol) != entitymap[3].end()) {
              entities.insert(entitymap[3][vol]);
            }
            else{
              std::cerr << "Warning: CGM Body has orphan RefVolume" << std::endl;
            }
          }
        }
        else {
          // Otherwise, warn user.
          std::cerr << "Warning: A dim<0 entity is being ignored by ReadCGM." << std::endl;
        }
      }
      else if (dim < 4) {
        if (entitymap[dim].find(ent) != entitymap[dim].end())
          entities.insert(entitymap[dim][ent]);
      }
    }

    if (!entities.empty()) {
      rval = mdbImpl->add_entities(ci->second, entities);
      if (MB_SUCCESS != rval)
        return MB_FAILURE;
    }
  }

  return MB_SUCCESS;
}

void ReadCGM::set_cgm_attributes(bool const act_attributes, bool const verbose)
{
  if (act_attributes) {
    CGMApp::instance()->attrib_manager()->set_all_auto_read_flags(act_attributes);
    CGMApp::instance()->attrib_manager()->set_all_auto_actuate_flags(act_attributes);
  }

  if (!verbose) {
    CGMApp::instance()->attrib_manager()->silent_flag(true);
  }
}

ErrorCode ReadCGM::create_vertices(std::map<RefEntity*, EntityHandle> &vertex_map)
{
  ErrorCode rval;
  std::map<RefEntity*, EntityHandle>::iterator ci;
  for (ci = vertex_map.begin(); ci != vertex_map.end(); ++ci) {
    CubitVector pos = dynamic_cast<RefVertex*>(ci->first)->coordinates();
    double coords[3] = {pos.x(), pos.y(), pos.z()};
    EntityHandle vh;
    rval = mdbImpl->create_vertex(coords, vh);
    if (MB_SUCCESS != rval)
      return MB_FAILURE;

    // Add the vertex to its tagged meshset
    rval = mdbImpl->add_entities(ci->second, &vh, 1);
    if (MB_SUCCESS != rval)
      return MB_FAILURE;

    // Replace the meshset handle with the vertex handle
    // This makes adding the vertex to higher dim sets easier
    ci->second = vh;
  }

  return MB_SUCCESS;
}

ErrorCode ReadCGM::create_curve_facets(std::map<RefEntity*, EntityHandle>& curve_map,
                                       std::map<RefEntity*, EntityHandle>& vertex_map,
#if CGM_MAJOR_VERSION > 12
                                       int norm_tol,
#else
                                       int /* norm_tol */,
#endif
                                       double faceting_tol,
                                       bool verbose_warn,
				       bool fatal_on_curves)
{
  ErrorCode rval;
  CubitStatus s;
  // Maximum allowable curve-endpoint proximity warnings
  // If this integer becomes negative, then abs(curve_warnings) is the
  // number of warnings that were suppressed.
  int curve_warnings = 0;

  // Map iterator
  std::map<RefEntity*, EntityHandle>::iterator ci;

  // Create geometry for all curves
  GMem data;
  for (ci = curve_map.begin(); ci != curve_map.end(); ++ci) {
    // Get the start and end points of the curve in the form of a reference edge
    RefEdge* edge = dynamic_cast<RefEdge*>(ci->first);
    // Get the edge's curve information
    Curve* curve = edge->get_curve_ptr();
    // Clean out previous curve information
    data.clean_out();
    // Facet curve according to parameters and CGM version
#if CGM_MAJOR_VERSION > 12
    s = edge->get_graphics(data, norm_tol, faceting_tol);
#else
    s = edge->get_graphics(data, faceting_tol);
#endif

    if( s != CUBIT_SUCCESS )
      {
	// if we fatal on curves
	if(fatal_on_curves)
	  {  
	    std::cout << "Failed to facet the curve " << edge->id() << std::endl;
	    return MB_FAILURE;
	  }
	// otherwise record them
	else
	  {
	    failed_curve_count++;
	    failed_curves.push_back(edge->id());
	  }
	continue;
      }

    std::vector<CubitVector> points;
    for (int i = 0; i < data.pointListCount; ++i)
      // Add Cubit vertext points to a list
      points.push_back(CubitVector(data.point_list()[i].x,
                                   data.point_list()[i].y,
                                   data.point_list()[i].z));

    // Need to reverse data?
    if (curve->bridge_sense() == CUBIT_REVERSED) 
      std::reverse(points.begin(), points.end());

    // Check for closed curve
    RefVertex *start_vtx, *end_vtx;
    start_vtx = edge->start_vertex();
    end_vtx = edge->end_vertex();

    // Special case for point curve
    if (points.size() < 2) {
      if (start_vtx != end_vtx || curve->measure() > GEOMETRY_RESABS) {
        std::cerr << "Warning: No facetting for curve " << edge->id() << std::endl;
        continue;
      }
      EntityHandle h = vertex_map[start_vtx];
      rval = mdbImpl->add_entities(ci->second, &h, 1);
      if (MB_SUCCESS != rval)
        return MB_FAILURE;
      continue;
    }
    // Check to see if the first and last interior vertices are considered to be
    // coincident by CUBIT
    const bool closed = (points.front() - points.back()).length() < GEOMETRY_RESABS;
    if (closed != (start_vtx == end_vtx)) {
      std::cerr << "Warning: topology and geometry inconsistant for possibly closed curve "
                << edge->id() << std::endl;
    }

    // Check proximity of vertices to end coordinates
    if ((start_vtx->coordinates() - points.front()).length() > GEOMETRY_RESABS ||
        (end_vtx->coordinates() - points.back()).length() > GEOMETRY_RESABS) {

      curve_warnings--;
      if (curve_warnings >= 0 || verbose_warn) {
        std::cerr << "Warning: vertices not at ends of curve " << edge->id() << std::endl;
        if (curve_warnings == 0 && !verbose_warn) {
          std::cerr << "         further instances of this warning will be suppressed..." << std::endl;
        }
      }
    }
    // Create interior points
    std::vector<EntityHandle> verts, edges;
    verts.push_back(vertex_map[start_vtx]);
    for (size_t i = 1; i < points.size() - 1; ++i) {
      double coords[] = {points[i].x(), points[i].y(), points[i].z()};
      EntityHandle h;
      // Create vertex entity
      rval = mdbImpl->create_vertex(coords, h);
      if (MB_SUCCESS != rval)
        return MB_FAILURE;
      verts.push_back(h);
    }
    verts.push_back(vertex_map[end_vtx]);

    // Create edges
    for (size_t i = 0; i < verts.size() - 1; ++i) {
      EntityHandle h;
      rval = mdbImpl->create_element(MBEDGE, &verts[i], 2, h);
      if (MB_SUCCESS != rval)
        return MB_FAILURE;
      edges.push_back(h);
    }

    // If closed, remove duplicate
    if (verts.front() == verts.back())
      verts.pop_back();
    // Add entities to the curve meshset from entitymap
    rval = mdbImpl->add_entities(ci->second, &verts[0], verts.size());
    if (MB_SUCCESS != rval)
      return MB_FAILURE;
    rval = mdbImpl->add_entities(ci->second, &edges[0], edges.size());
    if (MB_SUCCESS != rval)
      return MB_FAILURE;
  }

  if (!verbose_warn && curve_warnings < 0) {
    std::cerr << "Suppressed " << -curve_warnings
              << " 'vertices not at ends of curve' warnings." << std::endl;
    std::cerr << "To see all warnings, use reader param VERBOSE_CGM_WARNINGS." << std::endl;
  }

  return MB_SUCCESS;
}

ErrorCode ReadCGM::create_surface_facets(std::map<RefEntity*, EntityHandle>& surface_map,
                                         std::map<RefEntity*, EntityHandle>& vertex_map,
                                         int norm_tol,
                                         double facet_tol,
                                         double length_tol)
{
  ErrorCode rval;
  std::map<RefEntity*, EntityHandle>::iterator ci;
  CubitStatus s;
#if  ( (CGM_MAJOR_VERSION == 14 && CGM_MINOR_VERSION > 2) || CGM_MAJOR_VERSION == 15 )
  DLIList<TopologyEntity*> me_list;
#else
  DLIList<ModelEntity*> me_list;
#endif

  GMem data;
  // Create geometry for all surfaces
  for (ci = surface_map.begin(); ci != surface_map.end(); ++ci) {
    RefFace* face = dynamic_cast<RefFace*>(ci->first);

    data.clean_out();
    s = face->get_graphics(data, norm_tol, facet_tol, length_tol);

    if (CUBIT_SUCCESS != s)
      return MB_FAILURE;

    // Declare array of all vertex handles
    std::vector<EntityHandle> verts(data.pointListCount, 0);

    // Get list of geometric vertices in surface
    me_list.clean_out();
    ModelQueryEngine::instance()->query_model(*face, DagType::ref_vertex_type(), me_list);

    // For each geometric vertex, find a single coincident point in facets
    // Otherwise, print a warning
    for (int i = me_list.size(); i--; ) {
      // Assign geometric vertex
      RefVertex* vtx = dynamic_cast<RefVertex*>(me_list.get_and_step());
      CubitVector pos = vtx->coordinates();

      for (int j = 0; j < data.pointListCount; ++j) {
        // Assign facet vertex
        CubitVector vpos(data.point_list()[j].x,
                         data.point_list()[j].y,
                         data.point_list()[j].z);
        // Check to see if they are considered coincident
        if ((pos - vpos).length_squared() < GEOMETRY_RESABS*GEOMETRY_RESABS) {
          // If this facet vertex has already been found coincident, print warning
          if (verts[j])
            std::cerr << "Warning: Coincident vertices in surface " << face->id() << std::endl;
          // If a coincidence is found, keep track of it in the verts vector
          verts[j] = vertex_map[vtx];
          break;
        }
      }
    }

    // Now create vertices for the remaining points in the facetting
    for (int i = 0; i < data.pointListCount; ++i) {
      if (verts[i]) // If a geometric vertex
        continue;
      double coords[] = {data.point_list()[i].x,
                         data.point_list()[i].y,
                         data.point_list()[i].z};
      // Return vertex handle to verts to fill in all remaining facet
      // vertices
      rval = mdbImpl->create_vertex(coords, verts[i]);
      if (MB_SUCCESS != rval)
        return rval;
    }

    // record the failures for information
    if (data.fListCount == 0)
      {
	failed_surface_count++;
	failed_surfaces.push_back(face->id());
      }

    // Now create facets
    Range facets;
    std::vector<EntityHandle> corners;
    for (int i = 0; i < data.fListCount; i += data.facet_list()[i] + 1) {
      // Get number of facet verts
      int* facet = data.facet_list() + i;
      corners.resize(*facet);
      for (int j = 1; j <= *facet; ++j) {
        if (facet[j] >= (int)verts.size()) {
          std::cerr << "ERROR: Invalid facet data for surface " << face->id() << std::endl;
          return MB_FAILURE;
        }
        corners[j - 1] = verts[facet[j]];
      }
      EntityType type;
      if (*facet == 3)
        type = MBTRI;
      else {
        std::cerr << "Warning: non-triangle facet in surface " << face->id() << std::endl;
        std::cerr << "  entity has " << *facet << " edges" << std::endl;
        if (*facet == 4)
          type = MBQUAD;
        else
          type = MBPOLYGON;
      }

      //if (surf->bridge_sense() == CUBIT_REVERSED)
        //std::reverse(corners.begin(), corners.end());

      EntityHandle h;
      rval = mdbImpl->create_element(type, &corners[0], corners.size(), h);
      if (MB_SUCCESS != rval)
        return MB_FAILURE;

      facets.insert(h);
    }

    // Add vertices and facets to surface set
    rval = mdbImpl->add_entities(ci->second, &verts[0], verts.size());
    if (MB_SUCCESS != rval)
      return MB_FAILURE;
    rval = mdbImpl->add_entities(ci->second, facets);
    if (MB_SUCCESS != rval)
      return MB_FAILURE;
  }

  return MB_SUCCESS;
}

// Copy geometry into mesh database
ErrorCode ReadCGM::load_file(const char *cgm_file_name,
                             const EntityHandle* file_set,
                             const FileOptions& opts,
                             const ReaderIface::SubsetList* subset_list,
                             const Tag* /*file_id_tag*/)
{
  // Blocks_to_load and num_blocks are ignored.
  ErrorCode rval;

  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for CGM data");
  }

  int norm_tol;
  double faceting_tol;
  double len_tol;
  bool act_att = true;
  bool verbose_warnings = false;
  bool fatal_on_curves = false;

  rval = set_options(opts, norm_tol, faceting_tol, len_tol, act_att, verbose_warnings,fatal_on_curves);
  if (MB_SUCCESS != rval)
    return rval;

  // Always tag with the faceting_tol and geometry absolute resolution
  // If file_set is defined, use that, otherwise (file_set == NULL) tag the interface
  EntityHandle set = file_set ? *file_set : 0;
  rval = mdbImpl->tag_set_data(faceting_tol_tag, &set, 1, &faceting_tol);
  if (MB_SUCCESS != rval)
    return rval;

  rval = mdbImpl->tag_set_data(geometry_resabs_tag, &set, 1, &GEOMETRY_RESABS);
  if (MB_SUCCESS != rval)
    return rval;

  // Initialize CGM
  InitCGMA::initialize_cgma();

  // Determine CGM settings and amount of output
  set_cgm_attributes(act_att, verbose_warnings);

  CubitStatus s;

  // Get CGM file type
  const char* file_type = 0;
  file_type = get_geom_file_type(cgm_file_name);
  if (!file_type || !strcmp(file_type , "CUBIT"))
    return MB_FAILURE;

  s = CubitCompat_import_solid_model(cgm_file_name, file_type);
  if (CUBIT_SUCCESS != s) {
    MB_SET_ERR(MB_FAILURE, cgm_file_name << ": Failed to read file of type \"" << file_type << "\"");
  }

  // Create entity sets for all geometric entities
  std::map<RefEntity*, EntityHandle> entmap[5]; // One for each dim, and one for groups

  rval = create_entity_sets(entmap);
  if (rval != MB_SUCCESS)
    return rval;

  // Create topology for all geometric entities
  rval = create_topology(entmap);
  if (rval != MB_SUCCESS)
    return rval;

  // Store CoFace senses
  rval = store_surface_senses(entmap[2], entmap[3]);
  if (rval != MB_SUCCESS)
    return rval;

  // Store CoEdge senses
  rval = store_curve_senses(entmap[1], entmap[2]);
  if (rval != MB_SUCCESS)
    return rval;

  // Get group information and store it in the mesh
  rval = store_groups(entmap);
  if (rval != MB_SUCCESS)
    return rval;

  // Done with volumes and groups
  entmap[3].clear();
  entmap[4].clear();

  // Create geometry for all vertices and replace
  rval = create_vertices(entmap[0]);
  if (rval != MB_SUCCESS)
    return rval;

  // Create facets for all curves
  rval = create_curve_facets(entmap[1], entmap[0], norm_tol, faceting_tol, verbose_warnings, fatal_on_curves);
  if (rval != MB_SUCCESS)
    return rval;

  // Create facets for surfaces
  rval = create_surface_facets(entmap[2], entmap[0], norm_tol, faceting_tol, len_tol);
  if (rval != MB_SUCCESS)
    return rval;

  // print the fail information
  dump_fail_counts();
  
  return MB_SUCCESS;
}

// return the number of curves that failed to facet
int ReadCGM::get_failed_curve_count()
{
  return failed_curve_count;
}

// return the number of surfaces that failed to facet
int ReadCGM::get_failed_surface_count()
{
  return failed_surface_count;
}

void ReadCGM::dump_fail_counts()
{
  std::cout << "***** Faceting Summary Information *****" << std::endl;
  std::cout << "----- Curve Fail Information -----" << std::endl;
  std::cout << "There were " << failed_curve_count << " curves that could not be faceted." << std::endl;

  if(failed_curve_count > 0 )
    {
      std::cout << "The curves were ";
      for ( int i = 0 ; i < failed_curve_count ; i++ )
	{
	  std::cout << failed_curves[i] << " ";
	  if ( (i%10 == 0) & (i > 0) )
	    std::cout << std::endl;
	}
    }
  std::cout << std::endl;
  std::cout << "----- Facet Fail Information -----" << std::endl;
  std::cout << "There were " << failed_surface_count << " surfaces that could not be faceted." << std::endl;
  if(failed_surface_count > 0 )
    {
      std::cout << "The surfaces were ";
      for ( int i = 0 ; i < failed_surface_count ; i++ )
	{
	  std::cout << failed_surfaces[i] << " ";
	  if ( (i%10 == 0) & (i > 0) )
	    std::cout << std::endl;
	}      
    }
  std::cout << std::endl;
  std::cout << "***** End of Faceting Summary Information *****" << std::endl;
  return;
}

const char* ReadCGM::get_geom_file_type(const char* name)
{
  FILE* file;
  const char* result = 0;

  file = fopen(name, "r");
  if (file) {
    result = get_geom_fptr_type(file);
    fclose(file);
  }

  return result;
}

const char* ReadCGM::get_geom_fptr_type(FILE* file)
{
  static const char* CUBIT_NAME = GF_CUBIT_FILE_TYPE;
  static const char*  STEP_NAME = GF_STEP_FILE_TYPE;
  static const char*  IGES_NAME = GF_IGES_FILE_TYPE;
  static const char*  BREP_NAME = GF_OCC_BREP_FILE_TYPE;
  static const char* FACET_NAME = GF_FACET_FILE_TYPE;

  if (is_cubit_file(file))
    return CUBIT_NAME;
  else if (is_step_file(file))
    return STEP_NAME;
  else if (is_iges_file(file))
    return IGES_NAME;
  else if (is_occ_brep_file(file))
    return BREP_NAME;
  else if (is_facet_file(file))
    return FACET_NAME;
  else
    return NULL;
}

int ReadCGM::is_cubit_file(FILE* file)
{
  unsigned char buffer[4];
  return !fseek(file, 0, SEEK_SET) &&
         fread(buffer, 4, 1, file) &&
         !memcmp(buffer, "CUBE", 4);
}

int ReadCGM::is_step_file(FILE* file)
{
  unsigned char buffer[9];
  return !fseek(file, 0, SEEK_SET) &&
         fread(buffer, 9, 1, file) &&
         !memcmp(buffer, "ISO-10303", 9);
}

int ReadCGM::is_iges_file(FILE* file)
{
  unsigned char buffer[10];
  return !fseek(file, 72, SEEK_SET) &&
         fread(buffer, 10, 1, file) &&
         !memcmp(buffer, "S      1", 8);
}

int ReadCGM::is_occ_brep_file(FILE* file)
{
  unsigned char buffer[6];
  return !fseek(file, 0, SEEK_SET) &&
         fread(buffer, 6, 1, file) &&
         !memcmp(buffer, "DBRep_", 6);
}
int ReadCGM::is_facet_file(FILE* file)
{
  unsigned char buffer[10];
  return !fseek(file, 0, SEEK_SET) &&
         fread(buffer, 10, 1, file) &&
         !memcmp(buffer, "MESH_BASED", 10);
}

void ReadCGM::tokenize(const std::string& str,
                       std::vector<std::string>& tokens,
                       const char* delimiters)
{
  std::string::size_type last = str.find_first_not_of(delimiters, 0);
  std::string::size_type pos  = str.find_first_of(delimiters, last);
  while (std::string::npos != pos && std::string::npos != last) {
    tokens.push_back(str.substr(last, pos - last));
    last = str.find_first_not_of(delimiters, pos);
    pos  = str.find_first_of(delimiters, last);
    if (std::string::npos == pos)
      pos = str.size();
  }
}

} // namespace moab
