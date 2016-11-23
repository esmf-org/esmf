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

#include "moab/DualTool.hpp"
#include "moab/Range.hpp"
// using Core for call to check_adjacencies
#include "moab/Core.hpp"
#include "Internals.hpp"
#include "MBTagConventions.hpp"
#include "moab/Skinner.hpp"
#include "moab/Core.hpp"
#include "moab/MeshTopoUtil.hpp"
#include "AEntityFactory.hpp"
#include "moab/CN.hpp"
#include <string>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <assert.h>

#define RR if (MB_SUCCESS != result) return result
#define SWAP(a,b) {EntityHandle tmp_ent = a; a = b; b = tmp_ent;}

namespace moab {

bool debug = false;
bool debug_ap = false;

  //! tag name for dual surfaces
const char *DualTool::DUAL_SURFACE_TAG_NAME = "DUAL_SURFACE";

  //! tag name for dual curves
const char *DualTool::DUAL_CURVE_TAG_NAME = "DUAL_CURVE";

  //! tag name for dual cells
const char *DualTool::IS_DUAL_CELL_TAG_NAME = "__IS_DUAL_CELL";

  //! tag name for dual entities
const char *DualTool::DUAL_ENTITY_TAG_NAME = "__DUAL_ENTITY";

  //! tag name for extra dual entities
const char *DualTool::EXTRA_DUAL_ENTITY_TAG_NAME = "__EXTRA_DUAL_ENTITY";

  //! tag name for graphics point
const char *DualTool::DUAL_GRAPHICS_POINT_TAG_NAME = "__DUAL_GRAPHICS_POINT";

//const int DualTool::GP_SIZE = 20;

DualTool::DualTool(Interface *impl) 
    : mbImpl(impl)
{
  EntityHandle dum_handle = 0;
  ErrorCode result;
  
  result = mbImpl->tag_get_handle( DUAL_SURFACE_TAG_NAME, 
                                   1, MB_TYPE_HANDLE, 
                                   dualSurfaceTag,
                                   MB_TAG_SPARSE|MB_TAG_CREAT,
                                   &dum_handle );
  assert(MB_SUCCESS == result);
  
  result = mbImpl->tag_get_handle( DUAL_CURVE_TAG_NAME, 
                                   1, MB_TYPE_HANDLE, 
                                   dualCurveTag,
                                   MB_TAG_SPARSE|MB_TAG_CREAT,
                                   &dum_handle );
  assert(MB_SUCCESS == result);
  
  unsigned int dummy = 0;
  result = mbImpl->tag_get_handle( IS_DUAL_CELL_TAG_NAME, 
                                   1, MB_TYPE_INTEGER,
                                   isDualCellTag,
                                   MB_TAG_SPARSE|MB_TAG_CREAT, 
                                   &dummy);
  assert(MB_SUCCESS == result);

  result = mbImpl->tag_get_handle( DUAL_ENTITY_TAG_NAME, 
                                   1, MB_TYPE_HANDLE, 
                                   dualEntityTag,
                                   MB_TAG_DENSE|MB_TAG_CREAT,
                                   &dum_handle );
  assert(MB_SUCCESS == result);
  
  result = mbImpl->tag_get_handle( EXTRA_DUAL_ENTITY_TAG_NAME, 
                                   1, MB_TYPE_HANDLE, 
                                   extraDualEntityTag,
                                   MB_TAG_SPARSE|MB_TAG_CREAT,
                                   &dum_handle );
  assert(MB_SUCCESS == result);
  
  static const char dum_name[CATEGORY_TAG_SIZE] = {0};
  result = mbImpl->tag_get_handle( CATEGORY_TAG_NAME,
                                   CATEGORY_TAG_SIZE, MB_TYPE_OPAQUE,
                                   categoryTag,
                                   MB_TAG_SPARSE|MB_TAG_CREAT,
                                   dum_name );
  assert(MB_SUCCESS == result);
  
  DualTool::GraphicsPoint dum_pt(0.0, 0.0, 0.0, -1);
  result = mbImpl->tag_get_handle(DUAL_GRAPHICS_POINT_TAG_NAME, 
                                  sizeof(DualTool::GraphicsPoint), MB_TYPE_DOUBLE, 
                                  dualGraphicsPointTag, 
                                  MB_TAG_DENSE|MB_TAG_CREAT|MB_TAG_BYTES,
                                  &dum_pt);
  assert(MB_SUCCESS == result);

  int dum_int = 0;
  result = mbImpl->tag_get_handle( GLOBAL_ID_TAG_NAME, 
                                   1, MB_TYPE_INTEGER,
                                   globalIdTag,
                                   MB_TAG_SPARSE|MB_TAG_CREAT, 
                                   &dum_int);
  assert(MB_SUCCESS == result);
  if (MB_SUCCESS == result) {} // empty statement to get rid of warning.
  
  maxHexId = -1;
}

DualTool::~DualTool() 
{}

  //! construct the dual entities for the entire mesh
ErrorCode DualTool::construct_dual(EntityHandle *entities, 
                                     const int num_entities) 
{
    // allocate a dual entity for each primal entity in the mesh, starting
    // with highest dimension and working downward; do each dimension in a separate code
    // block, since they're all handled slightly differently
  
  Range regions, faces, edges, vertices;
  ErrorCode result;

  if (NULL == entities || 0 == num_entities) {
    
      // first, construct all the aentities, since they're currently needed to 
      // compute the dual
    result = mbImpl->get_entities_by_dimension(0, 0, vertices);
    if (MB_SUCCESS != result) return result;

    result = MeshTopoUtil(mbImpl).construct_aentities(vertices);
    if (MB_SUCCESS != result) return result;

      // get all edges, faces and regions now, so we don't need to filter out dual
      // entities later
  
    result = mbImpl->get_entities_by_dimension(0, 1, edges);
    if (MB_SUCCESS != result) return result;
    result = mbImpl->get_entities_by_dimension(0, 2, faces);
    if (MB_SUCCESS != result) return result;
    result = mbImpl->get_entities_by_dimension(0, 3, regions);
    if (MB_SUCCESS != result) return result;

      // get the max global id for hexes, we'll need for modification ops
    std::vector<int> gid_vec(regions.size());
    result = mbImpl->tag_get_data(globalId_tag(), regions, &gid_vec[0]);
    if (MB_SUCCESS != result) return result;
    maxHexId = -1;
    Range::iterator rit;
    unsigned int i;
    for (rit = regions.begin(), i = 0; rit != regions.end(); ++rit, i++) {
      if (gid_vec[i] > maxHexId && mbImpl->type_from_handle(*rit) == MBHEX)
        maxHexId = gid_vec[i];
    }
  }
  else {
      // get entities of various dimensions adjacent to these
    result = mbImpl->get_adjacencies(entities, num_entities, 0, true, vertices, 
                                     Interface::UNION);
    if (MB_SUCCESS != result) return result;
    result = mbImpl->get_adjacencies(entities, num_entities, 1, true, edges,
                                     Interface::UNION);
    if (MB_SUCCESS != result) return result;
    result = mbImpl->get_adjacencies(entities, num_entities, 2, true, faces,
                                     Interface::UNION);
    if (MB_SUCCESS != result) return result;
    result = mbImpl->get_adjacencies(entities, num_entities, 3, true, regions,
                                     Interface::UNION);
    if (MB_SUCCESS != result) return result;
  
  }
  
  Range dual_verts;
  result = construct_dual_vertices(regions, dual_verts);
  if (MB_SUCCESS != result || dual_verts.size() != regions.size()) return result;
  if (debug)
    std::cout << "Constructed " << dual_verts.size() << " dual vertices." << std::endl;

    // don't really need dual edges, but construct 'em anyway
  Range dual_edges;
  result = construct_dual_edges(faces, dual_edges);
  if (MB_SUCCESS != result || dual_edges.size() != faces.size()) return result;
  if (debug)
    std::cout << "Constructed " << dual_edges.size() << " dual edges." << std::endl;

    // construct dual faces
  Range dual_faces;
  result = construct_dual_faces(edges, dual_faces);
  if (MB_SUCCESS != result || dual_faces.size() != edges.size()) return result;
  if (debug)
    std::cout << "Constructed " << dual_faces.size() << " dual faces." << std::endl;

    // construct dual cells
  Range dual_cells;
  result = construct_dual_cells(vertices, dual_cells);
  if (MB_SUCCESS != result || dual_cells.size() != vertices.size()) return result;
  if (debug)
    std::cout << "Constructed " << dual_cells.size() << " dual cells." << std::endl;

  return MB_SUCCESS;
}

ErrorCode DualTool::construct_dual_vertices(const Range &all_regions,
                                              Range &dual_ents) 
{
  if (all_regions.empty()) return MB_SUCCESS;
  
    // make sure they're all regions
  assert(3 == CN::Dimension(TYPE_FROM_HANDLE(*all_regions.begin())) &&
         3 == CN::Dimension(TYPE_FROM_HANDLE(*all_regions.rbegin())));
  
  Range::const_iterator rit;
  EntityHandle dual_ent;
  ErrorCode tmp_result = MB_SUCCESS;
  ErrorCode result = MB_SUCCESS;
  
  for (rit = all_regions.begin(); rit != all_regions.end(); ++rit) {
    if (tmp_result != MB_SUCCESS) result = tmp_result;
    
    tmp_result = mbImpl->tag_get_data(dualEntity_tag(), &(*rit), 1, &dual_ent);
    if (MB_SUCCESS == tmp_result && 0 != dual_ent) {
      dual_ents.insert(dual_ent);
      continue;
    }
    else if (MB_SUCCESS != tmp_result) continue;

    tmp_result = construct_dual_vertex(*rit, dual_ent, false, true);
    if (MB_SUCCESS != tmp_result) continue;

      // save it in the list of new dual ents
    dual_ents.insert(dual_ent);
    
  }

  return result;
}

ErrorCode DualTool::construct_dual_vertex(EntityHandle entity, 
                                            EntityHandle &dual_ent, 
                                            const bool extra,
                                            const bool add_graphics_pt)
{
    // no dual entity; construct one; first need the avg coordinates
  unsigned int is_dual = 0x1;
  double avg_pos[3];
  ErrorCode result = MeshTopoUtil(mbImpl).get_average_position(entity, avg_pos);
  if (MB_SUCCESS != result) return result;
    
    // now construct the new dual entity
  result = mbImpl->create_vertex(avg_pos, dual_ent);
  if (MB_SUCCESS != result) return result;
    
    // tag it indicating it's a dual entity
  result = mbImpl->tag_set_data(isDualCell_tag(), &dual_ent, 1, &is_dual);
  if (MB_SUCCESS != result) return result;
    
    // tag the primal entity with its dual entity and vica versa
  if (extra) 
    result = mbImpl->tag_set_data(extraDualEntity_tag(), &(entity), 1, &dual_ent);
  else
    result = mbImpl->tag_set_data(dualEntity_tag(), &(entity), 1, &dual_ent);
  if (MB_SUCCESS != result) return result;
    
  result = mbImpl->tag_set_data(dualEntity_tag(), &dual_ent, 1, &(entity));
  if (MB_SUCCESS != result) return result;

  if (add_graphics_pt)
        // put a graphics point on that vertex too
    result = add_graphics_point(dual_ent, avg_pos);

  return result;
}

ErrorCode DualTool::add_graphics_point(EntityHandle entity,
                                         double *avg_pos) 
{
    // add a graphics pt, placed at the same position as the vertex
  double my_pos[3];
  ErrorCode result;
  
  if (NULL == avg_pos) {
    result = MeshTopoUtil(mbImpl).get_average_position(entity, my_pos);
    if (MB_SUCCESS != result) return result;
  }
  else
    for (int i = 0; i < 3; i++) my_pos[i] = avg_pos[i];
  
  DualTool::GraphicsPoint dum_pt(my_pos, -1);
  result = mbImpl->tag_set_data(dualGraphicsPoint_tag(), &entity, 1, 
                                &dum_pt);
  return result;
}

ErrorCode DualTool::construct_dual_edges(const Range &all_faces,
                                           Range &dual_ents) 
{
  if (all_faces.empty()) return MB_SUCCESS;
  
    // make sure they're all faces
  assert(2 == CN::Dimension(TYPE_FROM_HANDLE(*all_faces.begin())) &&
         2 == CN::Dimension(TYPE_FROM_HANDLE(*all_faces.rbegin())));
  
  Range::const_iterator rit;
  EntityHandle dual_ent;
  unsigned int is_dual = 0x1;
  ErrorCode tmp_result = MB_SUCCESS;
  ErrorCode result = MB_SUCCESS;
  
  for (rit = all_faces.begin(); rit != all_faces.end(); ++rit) {
    if (tmp_result != MB_SUCCESS) result = tmp_result;
    
    tmp_result = mbImpl->tag_get_data(dualEntity_tag(), &(*rit), 1, &dual_ent);
    if (MB_SUCCESS == tmp_result && 0 != dual_ent) {
      dual_ents.insert(dual_ent);
      continue;
    }
    
      // no dual entity; construct one; get the bounding regions
    std::vector<EntityHandle> out_ents;
    tmp_result = mbImpl->get_adjacencies(&(*rit), 1, 3, false, out_ents);
    if (MB_SUCCESS != tmp_result || out_ents.empty()) continue;

      // get the dual vertices
    std::vector<EntityHandle> dual_verts(out_ents.size());
    tmp_result = mbImpl->tag_get_data(dualEntity_tag(), &out_ents[0], out_ents.size(), 
                                      &dual_verts[0]);
    if (MB_SUCCESS != tmp_result) continue;
    assert(dual_verts.size() <= 2);
    
    double avg_pos[3];      
    bool bdy_face = (dual_verts.size() == 1 ? true : false);
    if (bdy_face) {
        // boundary face - make a dual vertex at the face center and put in list
      tmp_result = construct_dual_vertex(*rit, dual_ent, true, true);
      
        // put it on vertex list
      dual_verts.push_back(dual_ent);
    }
    
    assert(dual_verts.size() == 2);
    
      // now create the dual edge
    tmp_result = mbImpl->create_element(MBEDGE, &dual_verts[0], 2, dual_ent);
    if (MB_SUCCESS != tmp_result || 0 == dual_ent) continue;
    
      // save it in the list of new dual ents
    dual_ents.insert(dual_ent);
    
      // tag the primal entity with its dual entity and vica versa
    tmp_result = mbImpl->tag_set_data(dualEntity_tag(), &(*rit), 1, &dual_ent);
    if (MB_SUCCESS != tmp_result) continue;

    tmp_result = mbImpl->tag_set_data(dualEntity_tag(), &dual_ent, 1, &(*rit));
    if (MB_SUCCESS != tmp_result) continue;

      // tag the edge indicating it's a dual entity
    tmp_result = mbImpl->tag_set_data(isDualCell_tag(), &dual_ent, 1, &is_dual);
    if (MB_SUCCESS != tmp_result) continue;

      // add a graphics point to the edge; position depends on whether it's a
      // bdy face (mid-pt of dual edge) or not (mid-pt of primal face)
    if (bdy_face)
      tmp_result = add_graphics_point(dual_ent);
    else {
        // get the face's position
      tmp_result = MeshTopoUtil(mbImpl).get_average_position(*rit, avg_pos);
      if (MB_SUCCESS != tmp_result) continue;
      tmp_result = add_graphics_point(dual_ent, avg_pos);
    }
    if (MB_SUCCESS != tmp_result) continue;
  }

  return result;
}

ErrorCode DualTool::construct_dual_faces(const Range &all_edges,
                                           Range &dual_ents) 
{
  if (all_edges.empty()) return MB_SUCCESS;
  
    // make sure they're all edges
  assert(1 == CN::Dimension(TYPE_FROM_HANDLE(*all_edges.begin())) &&
         1 == CN::Dimension(TYPE_FROM_HANDLE(*all_edges.rbegin())));
  
  Range::const_iterator rit;
  EntityHandle dual_ent;
  unsigned int is_dual = 0x1;
  ErrorCode tmp_result = MB_SUCCESS;
  ErrorCode result = MB_SUCCESS;
  Range equiv_edges;
#define TRC if (MB_SUCCESS != tmp_result) {result = tmp_result; continue;}
  for (rit = all_edges.begin(); rit != all_edges.end(); ++rit) {
    
    tmp_result = mbImpl->tag_get_data(dualEntity_tag(), &(*rit), 1, &dual_ent);
    if (MB_SUCCESS == tmp_result && 0 != dual_ent) {
      dual_ents.insert(dual_ent);
      continue;
    }
    
      // no dual entity; construct one; get the dual vertices bounding the edge in radial order,
      // then construct the dual face
    std::vector<EntityHandle> rad_dverts;
    bool bdy_edge;
    tmp_result = get_radial_dverts(*rit, rad_dverts, bdy_edge);TRC
    if (rad_dverts.empty()) continue;
    
    tmp_result = mbImpl->create_element(MBPOLYGON, &rad_dverts[0], rad_dverts.size(), dual_ent);TRC

      // tag it indicating it's a dual entity, and tag primal/dual with dual/primal
    tmp_result = mbImpl->tag_set_data(isDualCell_tag(), &dual_ent, 1, &is_dual);TRC
    tmp_result = mbImpl->tag_set_data(dualEntity_tag(), &(*rit), 1, &dual_ent);TRC
    tmp_result = mbImpl->tag_set_data(dualEntity_tag(), &dual_ent, 1, &(*rit));TRC

      // save it in the list of new dual ents
    dual_ents.insert(dual_ent);
    
      // add a graphics point to the cell; position depends on whether it's a
      // bdy cell (mid-pt of cell's vertices) or not (mid-pt of primal edge)
    double avg_pos[3];
    tmp_result = MeshTopoUtil(mbImpl).get_average_position(*rit, avg_pos);TRC
    if (bdy_edge) {
      
        // add a new dual edge betw last 2 verts
      EntityHandle new_edge;
      tmp_result = mbImpl->create_element(MBEDGE, &rad_dverts[rad_dverts.size()-2], 
                                          2, new_edge);TRC
      tmp_result = mbImpl->tag_set_data(isDualCell_tag(), &new_edge, 1, &is_dual);TRC

        // tag the new dual edge with the primal edge as it's dual entity; primal
        // edge IS NOT likewise tagged, since it's already tagged with the 2cell
      tmp_result = mbImpl->tag_set_data(dualEntity_tag(), &new_edge, 1, &(*rit)); TRC
      
        // add a graphics pt, position is center of primal edge
      tmp_result = add_graphics_point(dual_ent);TRC
      tmp_result = add_graphics_point(new_edge, avg_pos);TRC
    }
    
    else {
        // if inside, point goes on the 2cell, at primal edge mid-pt
      tmp_result = add_graphics_point(dual_ent, avg_pos);TRC
    }

      // check to see whether we have equiv entities; if we find any, save for later fixup
    Range dum_edges, dum_poly(dual_ent, dual_ent);
    tmp_result = mbImpl->get_adjacencies(dum_poly, 1, false, dum_edges);
    if (MB_MULTIPLE_ENTITIES_FOUND == tmp_result) {
        // we do - need to add adjacencies to disambiguate; use the primal
      equiv_edges.merge(dum_edges);
    }    
  }

  if (!equiv_edges.empty()) 
    result = check_dual_equiv_edges(equiv_edges);

  return result;
}

ErrorCode DualTool::check_dual_equiv_edges(Range &dual_edges)
{
    // fix equivalent dual edges (i.e. edges whose vertices define multiple edges)
    // by explicitly adding adjacencies to containing polygons; adjacent polygons
    // found by going through primal
  ErrorCode tmp_result, result = MB_SUCCESS;

  Range all_dedges(dual_edges);
    // first, go through all dual edges and find equivalent edges (by looking for
    // up-adjacent edges on the vertices of each edge)
  for (Range::iterator rit = dual_edges.begin(); rit != dual_edges.end(); ++rit) {
    Range connect, dum_range(*rit, *rit);
    tmp_result = mbImpl->get_adjacencies(dum_range, 0, false, connect);
    if (MB_SUCCESS != tmp_result) continue;
    tmp_result = mbImpl->get_adjacencies(connect, 1, false, all_dedges, Interface::UNION);
    if (MB_SUCCESS != tmp_result) continue;
  }

    // save a copy for checking later
  Range save_all_2cells;

    // go through each edge
  while (!all_dedges.empty()) {
    EntityHandle this_edge = *all_dedges.begin();
    all_dedges.erase(all_dedges.begin());
    
    const EntityHandle *connect;
    int num_connect;
    result = mbImpl->get_connectivity(this_edge, connect, num_connect);
    if (MB_SUCCESS != result) continue;

    Range dum_edges, verts;
    verts.insert(connect[0]);
    verts.insert(connect[1]);
    tmp_result = mbImpl->get_adjacencies(verts, 1, false, dum_edges);
    if (MB_SUCCESS != tmp_result) {
      result = tmp_result;
      continue;
    }
    if (dum_edges.size() == 1) {
        // not an equiv edge - already removed from list, so just continue
      continue;
    }
    
      // ok, have an equiv entity - fix by looking through primal
      // pre-get the primal of these
    EntityHandle dedge_quad;
    tmp_result = mbImpl->tag_get_data(dualEntity_tag(), &this_edge, 1, &dedge_quad);
    if (MB_SUCCESS != tmp_result) {
      result = tmp_result;
      continue;
    }
  
    if (MBQUAD == mbImpl->type_from_handle(dedge_quad)) {
      
        // get the primal edges adj to quad
      Range dum_quad_range(dedge_quad, dedge_quad), adj_pedges;
      tmp_result = mbImpl->get_adjacencies(dum_quad_range, 1, false, adj_pedges);
      if (MB_SUCCESS != tmp_result) {
        result = tmp_result;
        continue;
      }
        // get the dual 2cells corresponding to those pedges
      std::vector<EntityHandle> dcells;
      dcells.resize(adj_pedges.size());
      tmp_result = mbImpl->tag_get_data(dualEntity_tag(), adj_pedges, &dcells[0]);
      if (MB_SUCCESS != tmp_result) {
        result = tmp_result;
        continue;
      }
        // now add explicit adjacencies from the dedge to those dcells
      std::vector<EntityHandle>::iterator vit;
      for (vit = dcells.begin(); vit != dcells.end(); ++vit) {
        save_all_2cells.insert(*vit);
        
        assert(MBPOLYGON == mbImpl->type_from_handle(*vit));
        tmp_result = mbImpl->add_adjacencies(this_edge, &(*vit), 1, false);
        if (MB_SUCCESS != tmp_result) {
          result = tmp_result;
          continue;
        }
          // check that there are really adjacencies and *vit is in them
        const EntityHandle *adjs;
        int num_adjs;
        tmp_result = reinterpret_cast<Core*>(mbImpl)->a_entity_factory()->
          get_adjacencies(this_edge, adjs, num_adjs);
        if (NULL == adjs || std::find(adjs, adjs+num_adjs, *vit) == adjs+num_adjs)
          std::cout << "Add_adjacencies failed in construct_dual_faces." << std::endl;
      }
    }
    else {
        // else, have a dual edge representing a bdy edge - tie directly to
        // dual entity if its dual entity
      EntityHandle bdy_dcell;
      tmp_result = mbImpl->tag_get_data(dualEntity_tag(), &dedge_quad, 1, &bdy_dcell); TRC
      assert(MBPOLYGON == mbImpl->type_from_handle(bdy_dcell));
      
      tmp_result = mbImpl->add_adjacencies(this_edge, &bdy_dcell, 1, false); 
      if (MB_SUCCESS != tmp_result) {
        result = tmp_result;
        continue;
      }
    }
  }
  

    // sanity check - look for adj edges again, and check for equiv entities
  for (Range::iterator vit = save_all_2cells.begin(); 
       vit != save_all_2cells.end(); ++vit) {
    Range adj_edges, dum_quad_range;
    dum_quad_range.insert(*vit);
    assert(MBPOLYGON == mbImpl->type_from_handle(*vit));
    tmp_result = mbImpl->get_adjacencies(dum_quad_range, 1, false, adj_edges);
    if (MB_MULTIPLE_ENTITIES_FOUND == tmp_result) {
      std::cout << "Multiple entities returned for polygon " << mbImpl->id_from_handle(*vit)
                << "." << std::endl;
      continue;
    }
  }
    // success!
  return result;
}

ErrorCode DualTool::construct_dual_cells(const Range &all_verts,
                                           Range &dual_ents) 
{
  if (all_verts.empty()) return MB_SUCCESS;
  
    // make sure they're all edges
  assert(0 == CN::Dimension(TYPE_FROM_HANDLE(*all_verts.begin())) &&
         0 == CN::Dimension(TYPE_FROM_HANDLE(*all_verts.rbegin())));
  
  Range::const_iterator rit;
  EntityHandle dual_ent;
  unsigned int is_dual = 0x1;
  ErrorCode tmp_result = MB_SUCCESS;
  ErrorCode result = MB_SUCCESS;
  std::vector<EntityHandle> edges, dfaces;
  
  for (rit = all_verts.begin(); rit != all_verts.end(); ++rit) {
    if (tmp_result != MB_SUCCESS) result = tmp_result;
    
    tmp_result = mbImpl->tag_get_data(dualEntity_tag(), &(*rit), 1, &dual_ent);
    if (MB_SUCCESS == tmp_result && 0 != dual_ent) {
      dual_ents.insert(dual_ent);
      continue;
    }
    
      // no dual entity; construct one; get the edges bounding the vertex
    edges.clear();
    dfaces.clear();
    tmp_result = mbImpl->get_adjacencies(&(*rit), 1, 1, false, edges);
    if (MB_SUCCESS != tmp_result) continue;

      // get the dual faces corresponding to the edges
    dfaces.resize(edges.size());
    tmp_result = mbImpl->tag_get_data(dualEntity_tag(), &edges[0], edges.size(), &dfaces[0]);
    if (MB_SUCCESS != tmp_result) continue;
    
      // create the dual cell from those faces
    tmp_result = mbImpl->create_element(MBPOLYHEDRON, &dfaces[0], dfaces.size(), dual_ent);
    if (MB_SUCCESS != tmp_result || 0 == dual_ent) continue;
    
      // save it in the list of new dual ents
    dual_ents.insert(dual_ent);
    
      // tag it indicating it's a dual entity
    tmp_result = mbImpl->tag_set_data(isDualCell_tag(), &dual_ent, 1, &is_dual);
    if (MB_SUCCESS != tmp_result) continue;

      // tag the primal entity with its dual entity and vica versa
    tmp_result = mbImpl->tag_set_data(dualEntity_tag(), &(*rit), 1, &dual_ent);
    if (MB_SUCCESS != tmp_result) continue;
    tmp_result = mbImpl->tag_set_data(dualEntity_tag(), &dual_ent, 1, &(*rit));
    if (MB_SUCCESS != tmp_result) continue;
  }

  return result;
}

  //! given an edge handle, return a list of dual vertices in radial order 
  //! around the edge
ErrorCode DualTool::get_radial_dverts(const EntityHandle edge,
                                        std::vector<EntityHandle> &rad_dverts,
                                        bool &bdy_edge) 
{
  rad_dverts.clear();
  
  std::vector<EntityHandle> rad_faces, rad_ents;
  ErrorCode result = MeshTopoUtil(mbImpl).star_entities(edge, rad_faces, bdy_edge, 0, 
                                                          &rad_ents);
  if (MB_SUCCESS != result) return result;

  if (bdy_edge) {
      // if we're a bdy edge, change the order back to what DualTool expects
    rad_ents.push_back(*rad_faces.rbegin());
    rad_ents.push_back(*rad_faces.begin());
  }
  
  rad_dverts.resize(rad_ents.size());
  for (unsigned int i = 0; i < rad_ents.size(); i++) {
    EntityHandle dual_ent;
    result = mbImpl->tag_get_data(dualEntity_tag(), &rad_ents[i], 1,
                                  &dual_ent);
    if (!bdy_edge || i < rad_ents.size()-2) rad_dverts[i] = dual_ent;
    else {
      // fix up this entry
      assert(mbImpl->type_from_handle(dual_ent) == MBEDGE);
    
        // get connectivity of that edge
      const EntityHandle *connect;
      int num_connect;
      result = mbImpl->get_connectivity(dual_ent, connect, num_connect);
      if (MB_SUCCESS != result) return result;
    
        // we want the one that's not already on the list; reuse last_face
      int last_hex = (i == rad_ents.size()-1 ? 0 : i-1);
      EntityHandle last_face = (connect[0] == rad_dverts[last_hex] ? connect[1] : connect[0]);
      rad_dverts[i] = last_face;
    }
  }

  return result;
}

  //! construct the dual entities for a hex mesh, including dual surfaces & curves
ErrorCode DualTool::construct_hex_dual(Range &entities) 
{
  std::vector<EntityHandle> evec;
  std::copy(entities.begin(), entities.end(), std::back_inserter(evec));
  return construct_hex_dual(&evec[0], evec.size());
}
  
  //! construct the dual entities for a hex mesh, including dual surfaces & curves
ErrorCode DualTool::construct_hex_dual(EntityHandle *entities,
                                         const int num_entities) 
{
    // really quite simple: 

    // construct the dual...
  ErrorCode result = construct_dual(entities, num_entities);
  if (MB_SUCCESS != result) {
    std::cerr << "Error constructing dual entities for primal entities." << std::endl;
    return result;
  }

    // now traverse to build 1d and 2d hyperplanes
  result = construct_dual_hyperplanes(1, entities, num_entities);
  if (MB_SUCCESS != result) {
    std::cerr << "Problem traversing 1d hyperplanes." << std::endl;
    return result;
  }

  result = construct_dual_hyperplanes(2, entities, num_entities);
  if (MB_SUCCESS != result) {
    std::cerr << "Problem traversing 2d hyperplanes." << std::endl;
    return result;
  }

  result = construct_hp_parent_child();
  if (MB_SUCCESS != result) {
    std::cerr << "Problem constructing parent/child relations between hyperplanes." 
              << std::endl;
    return result;
  }

    // see?  simple, just like I said
  return MB_SUCCESS;
}

  //! get the cells of the dual
ErrorCode DualTool::get_dual_entities(const int dim, 
                                        EntityHandle *entities, 
                                        const int num_entities, 
                                        Range &dual_ents) 
{
  if (0 == isDualCell_tag()) return MB_SUCCESS;
  if (0 > dim || 3 < dim) return MB_INDEX_OUT_OF_RANGE;

  unsigned int dum = 0x1;
  const void *dum_ptr = &dum;
  static EntityType dual_type[] = {MBVERTEX, MBEDGE, MBPOLYGON, MBPOLYHEDRON};

  Range dim_ents;
  
  ErrorCode result;

  if (0 == entities || 0 == num_entities) {
      // just get all the dual entities of this dimension
    result = mbImpl->get_entities_by_type_and_tag(0, dual_type[dim], 
                                                  &isDualCellTag, &dum_ptr, 1,
                                                  dual_ents);
  }
  else {
      // else look for specific dual entities
    result = mbImpl->get_adjacencies(entities, num_entities, 3-dim, false,
                                     dim_ents, Interface::UNION);
    if (MB_SUCCESS != result) return result;
    std::vector<EntityHandle> dual_ents_vec(dim_ents.size());
    result = mbImpl->tag_get_data(dualEntity_tag(), dim_ents, &dual_ents_vec[0]);
    if (MB_SUCCESS != result) return result;
    std::copy(dual_ents_vec.begin(), dual_ents_vec.end(), 
              range_inserter(dual_ents));
  }
  
  return result;
}

  //! get the faces of the dual
ErrorCode DualTool::get_dual_entities(const int dim, 
                                        EntityHandle *entities, 
                                        const int num_entities, 
                                        std::vector<EntityHandle> &dual_ents) 
{
  Range tmp_range;
  ErrorCode result = get_dual_entities(dim, entities, num_entities, tmp_range);
  if (MB_SUCCESS != result)
    return result;

    // dual_ents.insert(dual_ents.end(), tmp_range.begin(), tmp_range.end());
  dual_ents.reserve(dual_ents.size() + tmp_range.size());
  for (Range::const_iterator it = tmp_range.begin();
       it != tmp_range.end();
       ++it)
  {
    dual_ents.push_back(*it);
  }
  return MB_SUCCESS;
}

ErrorCode DualTool::get_dual_hyperplanes(const Interface *impl, const int dim, 
                                           Range &dual_ents) 
{
  if (dim != 1 && dim != 2) return MB_INDEX_OUT_OF_RANGE;

  Tag dual_tag;
  ErrorCode result;
  
  if (dim == 1)
    result = impl->tag_get_handle(DUAL_CURVE_TAG_NAME, 1, MB_TYPE_HANDLE, dual_tag );
  else 
    result = impl->tag_get_handle(DUAL_SURFACE_TAG_NAME, 1, MB_TYPE_HANDLE, dual_tag );

  if (MB_SUCCESS == result)
    result = impl->get_entities_by_type_and_tag(0, MBENTITYSET, &dual_tag, NULL, 1, dual_ents,
                                                Interface::UNION);
      
  return result;
}

ErrorCode DualTool::construct_dual_hyperplanes(const int dim, 
                                                 EntityHandle *entities, 
                                                 const int num_entities) 
{
    // this function traverses dual faces of input dimension, constructing
    // dual hyperplanes of them in sets as it goes

    // check various inputs
  int num_quads, num_hexes;
  if (
      // this function only makes sense for dim == 1 or dim == 2
    (dim != 1 && dim != 2) ||
      // should either be quads or hexes around
    mbImpl->get_number_entities_by_type(0, MBQUAD, num_quads) != MB_SUCCESS ||
    mbImpl->get_number_entities_by_type(0, MBHEX, num_hexes) != MB_SUCCESS ||
      // if we're asking for 1d dual ents, should be quads around
    (num_quads == 0 && dim == 1) ||
      // if we're asking for 2d dual ents, should be hexes around
    (num_hexes == 0 && dim == 2))
    return MB_FAILURE;
  
    // get tag name for this dimension hyperplane
  Tag gid_tag;
  int dum = 0;
  ErrorCode result = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 
                                            1, MB_TYPE_INTEGER, 
                                            gid_tag,
                                            MB_TAG_CREAT|MB_TAG_DENSE,
                                            &dum);
  if (MB_SUCCESS != result)
    return result;
    
  Tag hp_tag = (1 == dim ? dualCurve_tag() : dualSurface_tag());
  
    // two stacks: one completely untreated entities, and the other untreated 
    // entities on the current dual hyperplane
  std::vector<EntityHandle> tot_untreated;
  
    // put dual entities of this dimension on the untreated list
  result = get_dual_entities(dim, entities, num_entities, tot_untreated);
  if (MB_SUCCESS != result) 
    return result;
  
    // main part of traversal loop
  EntityHandle this_ent;
  EntityHandle this_hp;

  while (!tot_untreated.empty()) {
    if (debug && dim == 2 /*(tot_untreated.size()%report == 0)*/)
      std::cout << "Untreated list size " << tot_untreated.size() << "." << std::endl;
      
    this_ent = tot_untreated.back(); tot_untreated.pop_back();
    result = mbImpl->tag_get_data(hp_tag, &this_ent, 1, &this_hp);
    if (MB_SUCCESS != result && MB_TAG_NOT_FOUND != result) 
      return result;

      // d for this entity having a hyperplane assignment already
    else if (this_hp != 0)
      continue;

    if (1 == dim && check_1d_loop_edge(this_ent)) continue;
    
      // inner loop: traverse the hyperplane 'till we don't have any more
    result = traverse_hyperplane(hp_tag, this_hp, this_ent);
    if (MB_SUCCESS != result) {
      std::cout << "Failed to traverse hyperplane ";
      if (this_hp) std::cout << mbImpl->id_from_handle(this_hp) << "."  << std::endl;
      else std::cout << "0." << std::endl;
      return result;
    }

      // ok, now order the edges if it's a chord
    if (1 == dim) order_chord(this_hp);
  }

  return MB_SUCCESS;
}

ErrorCode DualTool::traverse_hyperplane(const Tag hp_tag, 
                                          EntityHandle &this_hp, 
                                          EntityHandle this_ent) 
{
  Range tmp_star, star, tmp_range, new_hyperplane_ents;
  std::vector<EntityHandle> hp_untreated;
  int dim = mbImpl->dimension_from_handle(this_ent);
  MeshTopoUtil mtu(mbImpl);
  this_hp = 0;
  ErrorCode result;
  
  unsigned short mark_val = 0x0;
  Tag mark_tag;
  result = mbImpl->tag_get_handle("__hyperplane_mark", 1, MB_TYPE_BIT, mark_tag,
                                  MB_TAG_CREAT|MB_TAG_BIT);
  if (MB_SUCCESS != result) 
    return result;
  mark_val = 0x1;
  
  while (0 != this_ent) {
    EntityHandle tmp_hp = get_dual_hyperplane(this_ent);
    if (0 == this_hp && 0 != tmp_hp) this_hp = tmp_hp;
    
    if (0 == tmp_hp) new_hyperplane_ents.insert(this_ent);
    
    if (debug && hp_untreated.size()%10 == 0) 
      std::cout << "Dual surface " << this_hp << ", hp_untreated list size = " 
                << hp_untreated.size() << "." << std::endl;

      // get the 2nd order adjacencies through lower dimension
    tmp_range.clear(); tmp_star.clear(); star.clear();
    result = mtu.get_bridge_adjacencies(this_ent, dim-1, dim, star); RR;

      // get the bridge adjacencies through higher dimension
    result = mtu.get_bridge_adjacencies(this_ent, dim+1, dim, tmp_star); RR;
    tmp_range = subtract( star, tmp_star);
    
    for (Range::iterator rit = tmp_range.begin(); rit != tmp_range.end(); ++rit) {
      if (new_hyperplane_ents.find(*rit) != new_hyperplane_ents.end()) continue;
      
        // check for tag first, 'cuz it's probably faster than checking adjacencies
        // assign to avoid valgrind warning
      unsigned short tmp_mark = 0x0;
      result = mbImpl->tag_get_data(mark_tag, &(*rit), 1, &tmp_mark);
      if (MB_SUCCESS == result && mark_val == tmp_mark) 
        continue;

        // if it's on the loop, it's not eligible
      if (1 == dim && check_1d_loop_edge(*rit)) continue;

        // have one on this hp; just put it on the hp_untreated list for now,
        // will get tagged and put in the hp set later
      hp_untreated.push_back(*rit);
      result = mbImpl->tag_set_data(mark_tag, &(*rit), 1, &mark_val);
      if (MB_SUCCESS != result) 
        return result;
    }

      // end of inner loop; get the next this_ent, or set to zero
    if (hp_untreated.empty()) this_ent = 0;
    else {
      this_ent = hp_untreated.back();
      hp_untreated.pop_back();
    }
  }

  if (debug_ap) {
    std::string hp_name;
    if (2 == dim) hp_name = "sheet";
    else hp_name = "chord";
    
    if (0 == this_hp) std::cout << "Constructed new " << hp_name << " with ";
    else {
      int this_id;
      result = mbImpl->tag_get_data(globalId_tag(), &this_hp, 1, &this_id); RR;
      std::cout << "Added to " << hp_name << " " << this_id << " ";
    }
    if (dim == 2) std::cout << "edges:" << std::endl;
    else std::cout << "quads:" << std::endl;
    std::vector<EntityHandle> pents(new_hyperplane_ents.size());
    result = mbImpl->tag_get_data(dualEntity_tag(), new_hyperplane_ents,
                                  &pents[0]); RR;
    for (std::vector<EntityHandle>::iterator vit = pents.begin(); 
         vit != pents.end(); ++vit) {
      if (vit != pents.begin()) std::cout << ", ";
      std::cout << mbImpl->id_from_handle(*vit);
    }
    std::cout << std::endl;
  }

  if (0 == this_hp) {
      // ok, doesn't have one; make a new hyperplane
    int new_id = -1;
    result = construct_new_hyperplane(dim, this_hp, new_id);
    if (MB_SUCCESS != result) return result;

    if (debug_ap) {
      std::cout << "New ";
      if (2 == dim) std::cout << " sheet ";
      else std::cout << " chord ";
      std::cout << new_id << " constructed." << std::endl;
    }
  }

    // set the hp_val for entities which didn't have one before
  std::vector<EntityHandle> hp_tags(new_hyperplane_ents.size());
  std::fill(hp_tags.begin(), hp_tags.end(), this_hp);
  result = mbImpl->tag_set_data(hp_tag, new_hyperplane_ents, &hp_tags[0]);
  if (MB_SUCCESS != result) 
    return result;
  result = mbImpl->add_entities(this_hp, new_hyperplane_ents);
  if (MB_SUCCESS != result) 
    return result;

    // unmark the entities by removing the tag
  result = mbImpl->tag_delete(mark_tag);
  if (MB_SUCCESS != result) 
    return result;

  return MB_SUCCESS;
}

ErrorCode DualTool::order_chord(EntityHandle chord_set) 
{
    // re-order the 1cells in the set so they are in order along the chord
    // start by finding the vertex dual to a quad
  Range verts, one_cells;
  ErrorCode result = mbImpl->get_entities_by_dimension(chord_set, 1, one_cells); 
  if (MB_SUCCESS != result || one_cells.empty()) return MB_FAILURE;
  
  result = mbImpl->get_adjacencies(one_cells, 0, false, verts, Interface::UNION);
  if (MB_SUCCESS != result || verts.empty()) return MB_FAILURE;
  
  EntityHandle last_vert = 0;
  for (Range::iterator rit = verts.begin(); rit != verts.end(); ++rit) {
    if (TYPE_FROM_HANDLE(get_dual_entity(*rit)) == MBQUAD) {
      last_vert = *rit;
      break;
    }
  }
    // if there's no vertex owned by a quad, just start with 1st one
  if (0 == last_vert) last_vert = *verts.begin();
  
    // now, skip from vertex to vertex, building a list of 1cells
  std::vector<EntityHandle> ordered_1cells;
  EntityHandle last_1cell = 0;
  Range dum1, dum2;
  const EntityHandle *connect;
  int num_connect;
  ErrorCode tmp_result = MB_SUCCESS;
  while(ordered_1cells.size() != one_cells.size()) {
    dum1 = one_cells;
    result = mbImpl->get_adjacencies(&last_vert, 1, 1, false, dum1);
    if (0 != last_1cell) dum1.erase(last_1cell);
      // assert(1 == dum1.size());
    if (0 != last_1cell && 1 != dum1.size()) {
      std::cerr << "unexpected size traversing chord." << std::endl;
      tmp_result = MB_FAILURE;
    }
      
    last_1cell = *dum1.begin();
    ordered_1cells.push_back(last_1cell);
    result = mbImpl->get_connectivity(last_1cell, connect, num_connect); RR;
    if (last_vert == connect[0]) last_vert = connect[1];
    else last_vert = connect[0];
  }
  
    // now have the 1cells in order, replace them in the set
  if (MB_SUCCESS == tmp_result) {
    result = mbImpl->remove_entities(chord_set, one_cells); RR;
    result = mbImpl->add_entities(chord_set, &ordered_1cells[0], ordered_1cells.size()); RR;
  }
  
  return MB_SUCCESS;
}

ErrorCode DualTool::construct_new_hyperplane(const int dim,
                                               EntityHandle &new_hyperplane,
                                               int &id) 
{
  ErrorCode result;
  if (1 == dim)
    result = mbImpl->create_meshset((MESHSET_ORDERED | MESHSET_TRACK_OWNER), 
                                    new_hyperplane);
  else
    result = mbImpl->create_meshset((MESHSET_SET | MESHSET_TRACK_OWNER), 
                                    new_hyperplane);
  if (MB_SUCCESS != result) return result;

  if (-1 == id) {
    Range all_hyperplanes;
    result = get_dual_hyperplanes(mbImpl, dim, all_hyperplanes); RR;
    std::vector<int> gids(all_hyperplanes.size());
    result = mbImpl->tag_get_data(globalIdTag, all_hyperplanes, (gids.empty())?NULL:&gids[0]); RR;
    for (unsigned int i = 0; i < gids.size(); i++) 
      if (gids[i] > id) id = gids[i];
    id++;
    if (0 == id) id++;
  }
    
  result = mbImpl->tag_set_data(globalId_tag(), &new_hyperplane, 1, &id); RR;
  Tag hp_tag = (1 == dim ? dualCurve_tag() : dualSurface_tag());
  result = mbImpl->tag_set_data(hp_tag, &new_hyperplane, 1, &new_hyperplane); RR;

    // assign a category name to these sets
  static const char dual_category_names[2][CATEGORY_TAG_SIZE] = 
    {"Chord\0", "Sheet\0"};
    
  result = mbImpl->tag_set_data(categoryTag, &new_hyperplane, 1, dual_category_names[dim-1]);

  return result;
}

bool DualTool::check_1d_loop_edge(EntityHandle this_ent) 
{
    // make sure it's an edge
  if (MBEDGE != mbImpl->type_from_handle(this_ent)) return false;

    // also has to be a dual entity
  unsigned int dum;
  ErrorCode result = mbImpl->tag_get_data(isDualCell_tag(), &this_ent, 1, &dum);
  if (MB_SUCCESS != result || dum != 0x1) return false;
  
  const EntityHandle *verts;
  EntityHandle vert_tags[2];
  int num_verts;
  result = mbImpl->get_connectivity(this_ent, verts, num_verts);
  if (MB_SUCCESS != result) return false;
  
  result = mbImpl->tag_get_data(dualEntity_tag(), verts, 2, vert_tags);
  if (MB_SUCCESS != result ||
      mbImpl->type_from_handle(vert_tags[0]) != MBQUAD ||
      mbImpl->type_from_handle(vert_tags[1]) != MBQUAD) return false;
  
  else return true;
}

ErrorCode DualTool::construct_hp_parent_child() 
{
  Range dual_surfs, dual_cells, dual_edges;
  ErrorCode result = this->get_dual_hyperplanes(mbImpl, 2, dual_surfs);
  if (MB_SUCCESS != result || dual_surfs.empty()) return result;
  std::vector<EntityHandle> dual_curve_sets;
  
  for (Range::iterator surf_it = dual_surfs.begin(); surf_it != dual_surfs.end();
       ++surf_it) {
      // get all the cells, edges in those cells, and chords for those edges
    dual_cells.clear();
    result = mbImpl->get_entities_by_handle(*surf_it, dual_cells);
    if (MB_SUCCESS != result) return result;
    dual_edges.clear();
    result = mbImpl->get_adjacencies(dual_cells, 1, false, dual_edges, Interface::UNION);
    if (MB_SUCCESS != result) return result;
    dual_curve_sets.resize(dual_edges.size());
    result = mbImpl->tag_get_data(dualCurve_tag(), dual_edges, &dual_curve_sets[0]);
    if (MB_SUCCESS != result) return result;

      // reuse dual_cells to get unique list of chord sets
    dual_cells.clear();
    for (unsigned int i = 0; i < dual_edges.size(); i++)
      if (dual_curve_sets[i] != 0) dual_cells.insert(dual_curve_sets[i]);
    
      // now connect up this dual surf with all the 1d ones
    for (Range::iterator rit = dual_cells.begin(); rit != dual_cells.end(); ++rit) {
      result = mbImpl->add_parent_child(*surf_it, *rit);
      if (MB_SUCCESS != result) return result;
    }
  }

  return MB_SUCCESS;
}
    
ErrorCode DualTool::get_graphics_points(EntityHandle dual_ent,
                                          std::vector<int> &npts,
                                          std::vector<GraphicsPoint> &points) 
{
    // shouldn't be a set
  assert(MBENTITYSET != mbImpl->type_from_handle(dual_ent));
  
    // get the graphics points comprising the given entity
  GraphicsPoint gp_array[DualTool::GP_SIZE];

  ErrorCode result = MB_SUCCESS;
  
    // switch based on topological dimension
  switch (mbImpl->dimension_from_handle(dual_ent)) {
    case 0:
        // just return the vertex point
      result = mbImpl->tag_get_data(dualGraphicsPoint_tag(), &dual_ent, 1,
                                    gp_array);
      if (MB_SUCCESS == result)
        points.push_back(gp_array[0]);
        
      break;

    case 1:
        // get my graphics point then those of my vertices
      const EntityHandle *connect;
      int num_connect;
      result = mbImpl->get_connectivity(dual_ent, connect, num_connect);
      if (MB_SUCCESS != result) break;
      
      result = mbImpl->tag_get_data(dualGraphicsPoint_tag(), connect, 2,
                                    gp_array);
      if (MB_SUCCESS == result) {
        points.push_back(gp_array[0]);
        points.push_back(gp_array[0]);
        points.push_back(gp_array[1]);
        result = mbImpl->tag_get_data(dualGraphicsPoint_tag(), &dual_ent, 1,
                                      gp_array);
        if (MB_SUCCESS == result) points[1] = gp_array[0];
      }

      npts.push_back(3);
      
      break;
      
    case 2:
      result = get_cell_points(dual_ent, npts, points);
      break;
  }
  
  return result;
}

ErrorCode DualTool::get_cell_points(EntityHandle dual_ent,
                                      std::vector<int> &npts,
                                      std::vector<GraphicsPoint> &points) 
{
  assert(MBPOLYGON == mbImpl->type_from_handle(dual_ent));
  
    // get the 1cells in this 2cell
  Range one_cells;
  
  Range tc_range; tc_range.insert(dual_ent);
  ErrorCode result = mbImpl->get_adjacencies(tc_range, 1, false, one_cells, 
                                               Interface::UNION); RR;

  int num_edges = one_cells.size();
  std::vector<GraphicsPoint> dum_gps(num_edges+1);
  
    // get graphics points for 0cells and for this cell
  result = mbImpl->tag_get_data(dualGraphicsPoint_tag(), one_cells,
                                &dum_gps[0]); RR;
  result = mbImpl->tag_get_data(dualGraphicsPoint_tag(), &dual_ent, 1,
                                &(dum_gps[num_edges])); RR;

  Range::iterator eit;
  const EntityHandle *connect;
  int num_connect;
  GraphicsPoint vert_gps[2];
  int i;
  for (i = 0, eit = one_cells.begin(); i < num_edges; i++, ++eit) {
      // get the vertices and the graphics points for them
    result = mbImpl->get_connectivity(*eit, connect, num_connect); RR;
    result = mbImpl->tag_get_data(dualGraphicsPoint_tag(), connect, 2, 
                                  vert_gps); RR;

      // make the 2 tris corresponding to this edge; don't worry about order
      // for now
    npts.push_back(3);
    points.push_back(dum_gps[num_edges]);
    points.push_back(vert_gps[0]);
    points.push_back(dum_gps[i]);
    
    npts.push_back(3);
    points.push_back(dum_gps[num_edges]);
    points.push_back(dum_gps[i]);
    points.push_back(vert_gps[1]);
  }

  return result;
}

ErrorCode DualTool::get_graphics_points(const Range &in_range,
                                          std::vector<GraphicsPoint> &points,
                                          const bool assign_ids,
                                          const int start_id) 
{
    // return graphics points on dual entities in in_range or in entities
    // in sets in in_range
  ErrorCode result;

    // for each dual hyperplane set:
  Range::const_iterator rit;
  
  Range two_cells, all_cells;
  for (rit = in_range.begin(); rit != in_range.end(); ++rit) {
      // for each entity:
    two_cells.clear();
    EntityType this_type = mbImpl->type_from_handle(*rit);
    if (MBENTITYSET == this_type) {
      result = mbImpl->get_entities_by_handle(*rit, two_cells); RR;
    
      std::copy(two_cells.begin(), two_cells.end(), range_inserter(all_cells));
    }
    
    else {
      two_cells.insert(*rit);
      assert(this_type == MBVERTEX || this_type == MBEDGE ||
             this_type == MBPOLYGON || this_type == MBPOLYHEDRON);
    }

    result = mbImpl->get_adjacencies(two_cells, 0, false, all_cells, 
                                     Interface::UNION); RR;
    result = mbImpl->get_adjacencies(two_cells, 1, false, all_cells, 
                                     Interface::UNION); RR;
  }
      
      // get graphics points
  points.resize(all_cells.size());
  
  result = mbImpl->tag_get_data(dualGraphicsPointTag, all_cells,
                                &points[0]); RR;

  if (assign_ids) {
    int i = start_id;
    
    for (std::vector<GraphicsPoint>::iterator vit = points.begin(); 
         vit != points.end(); ++vit)
      vit->id = i++;

    result = mbImpl->tag_set_data(dualGraphicsPoint_tag(), all_cells, 
                                  &points[0]); RR;
  }

  return result;
}

EntityHandle DualTool::next_loop_vertex(const EntityHandle last_v,
                                          const EntityHandle this_v,
                                          const EntityHandle dual_surf)
{
    // given two vertices, find the next one on the loop; if one is a dual
    // surface, then just choose either one for that surface
  assert((0 == last_v || mbImpl->type_from_handle(last_v) == MBVERTEX) &&
         mbImpl->type_from_handle(this_v) == MBVERTEX &&
         mbImpl->type_from_handle(dual_surf) == MBENTITYSET);

    // get the connected vertices
  MeshTopoUtil tpu(mbImpl);
  Range other_verts;
  ErrorCode result = tpu.get_bridge_adjacencies(this_v, 1, 0, other_verts);
  if (MB_SUCCESS != result || other_verts.empty()) return 0;
  
    //if (mbImpl->type_from_handle(last_v) == MBENTITYSET) {
      // dual surface, choose either; first get a 2cell on this surface
  Range tcells, tcells2, verts;
  result = mbImpl->get_entities_by_type(dual_surf, MBPOLYGON, tcells);
  if (MB_SUCCESS != result || tcells.empty()) return 0;

    // ok, pay attention here: first get 2cells common to dual surface and this_v
  verts.insert(this_v);
  result = mbImpl->get_adjacencies(verts, 2, false, tcells);
  if (MB_SUCCESS != result || tcells.empty()) return 0;

    // next get vertices common to both 2cells and subtract from other_verts; also
    // remove last_v if it's non-zero
  verts.clear();
  result = mbImpl->get_adjacencies(tcells, 0, false, verts);
  if (MB_SUCCESS != result || verts.empty()) return 0;
  
  Range tmp_verts = subtract( other_verts, verts);
  other_verts.swap(tmp_verts);
  if (0 != last_v) other_verts.erase(last_v);

    // now get intersection of remaining vertices and 2 2cells vertices
    // look at each one successively; should find one, maybe not on both
  tmp_verts = other_verts;
  Range tmp_faces(*tcells.begin(), *tcells.begin());
  result = mbImpl->get_adjacencies(tmp_faces, 0, false, tmp_verts);
  if (MB_SUCCESS == result && !tmp_verts.empty()) return *tmp_verts.begin();
  tmp_faces.clear();
  tmp_faces.insert(*tcells.rbegin());
  result = mbImpl->get_adjacencies(tmp_faces, 0, false, other_verts);
  if (MB_SUCCESS == result && !other_verts.empty()) return *other_verts.begin();

    // if we got here, there isn't any
  return 0;
}

EntityHandle DualTool::get_dual_hyperplane(const EntityHandle ncell) 
{
    // get the sheet or chord it's in
  std::vector<EntityHandle> adj_sets;
  ErrorCode result = mbImpl->get_adjacencies(&ncell, 1, 4, false, adj_sets);
  if (MB_SUCCESS != result) return 0;
    
  EntityHandle dum_set;
  for (std::vector<EntityHandle>::iterator vit = adj_sets.begin(); 
       vit != adj_sets.end(); ++vit) {
    if (mbImpl->tag_get_data(dualCurve_tag(), &(*vit), 1, &dum_set) != MB_TAG_NOT_FOUND ||
        mbImpl->tag_get_data(dualSurface_tag(), &(*vit), 1, &dum_set) != MB_TAG_NOT_FOUND)
      return *vit;
  }
  
  return 0;
}

    //! set the dual surface or curve for an entity
ErrorCode DualTool::set_dual_surface_or_curve(EntityHandle entity, 
                                                const EntityHandle dual_hyperplane,
                                                const int dual_entity_dimension)
{
  if (1 == dual_entity_dimension)
    mbImpl->tag_set_data(dualCurve_tag(), &entity, 1, &dual_hyperplane);
  else if (2 == dual_entity_dimension)
    mbImpl->tag_set_data(dualSurface_tag(), &entity, 1, &dual_hyperplane);
  else 
    return MB_INDEX_OUT_OF_RANGE;

  return MB_SUCCESS;
}

//! return the corresponding dual entity
EntityHandle DualTool::get_dual_entity(const EntityHandle this_ent) const
{
  EntityHandle dual_ent;
  ErrorCode result = mbImpl->tag_get_data(dualEntity_tag(), &this_ent, 1, &dual_ent);
  if (MB_SUCCESS != result || MB_TAG_NOT_FOUND == result) return 0;
  else return dual_ent;
}

//! return the corresponding dual entity
EntityHandle DualTool::get_extra_dual_entity(const EntityHandle this_ent) 
{
  EntityHandle dual_ent;
  ErrorCode result = mbImpl->tag_get_data(extraDualEntity_tag(), &this_ent, 1, &dual_ent);
  if (MB_SUCCESS != result || MB_TAG_NOT_FOUND == result) return 0;
  else return dual_ent;
}

ErrorCode DualTool::atomic_pillow(EntityHandle odedge, EntityHandle &quad1,
                                    EntityHandle &quad2) 
{
  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();

  if (debug_ap) {
    Range sets;
    Tag ms_tag;
    
    ErrorCode result = mbImpl->tag_get_handle("MATERIAL_SET", 1, MB_TYPE_INTEGER, ms_tag);
    if (MB_SUCCESS == result) {
      result = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &ms_tag, NULL,
                                                    1, sets);
      if (MB_SUCCESS == result)
        result = mbImpl->delete_entities(sets);
    }
  }
  
  std::cout << "-AP("; print_cell(odedge); std::cout << ")" << std::endl;

    // perform an atomic pillow operation around dedge

    // grab the quad before deleting the odedge
  quad1 = get_dual_entity(odedge);
  assert(0 != quad1);

    // 0. get star 2cells around odedge (before odedge changes) and 3cells around
    // those 2cells (going to delete the 2cells, therefore need to delete the 3cells
    // that depend on those too)
  MeshTopoUtil mtu(mbImpl);
  Range star_cells, tmp_cells;
  ErrorCode result = mbImpl->get_adjacencies(&odedge, 1, 2, false, star_cells); RR;
  result = mbImpl->get_adjacencies(star_cells, 3, false, tmp_cells,
                                   Interface::UNION); RR;
  star_cells.merge(tmp_cells);
  star_cells.insert(odedge);
  
    // tear down the dual entities which will be modified by the ap first
  result = delete_dual_entities(star_cells); RR;

    // now change the quad to an ap
  std::vector<EntityHandle> verts;
  result = mbImpl->get_connectivity(&quad1, 1, verts); RR;
  
    // get average position of vertices
  double coords[12], avg[3] = {0.0, 0.0, 0.0};
  result = mbImpl->get_coords(&verts[0], verts.size(), coords); RR;
  for (int i = 0; i < 4; i++) {
    avg[0] += coords[3*i]; avg[1] += coords[3*i+1]; avg[2] += coords[3*i+2];
  }
  for (int i = 0; i < 3; i++) avg[i] *= 0.25;

    // for each position, get a corresponding position 1/2 way to avg
  double new_coords[12];
  for (int i = 0; i < 4; i++) {
    new_coords[3*i] = avg[0] + .5*(coords[3*i]-avg[0]);
    new_coords[3*i+1] = avg[1] + .5*(coords[3*i+1]-avg[1]);
    new_coords[3*i+2] = avg[2] + .5*(coords[3*i+2]-avg[2]);
  }
  
    // make the 4 new vertices; store in vector long enough for hex connectivity
  for (int i = 0; i < 4; i++) {
    verts.push_back(0);
    result = mbImpl->create_vertex(&new_coords[3*i], verts[4+i]); RR;
  }

    // get the hexes connected to the quad
  Range hexes;
  result = mbImpl->get_adjacencies(&quad1, 1, 3, false, hexes); RR;
  assert(hexes.size() <= 2);
  
    // remove any explicit adjacency from the first hex, since that'll get connected
    // to the new quad; add adjacency between quad and second hex, if there is a 2nd
  result = mbImpl->remove_adjacencies(quad1, &(*hexes.begin()), 1); RR;
  if (hexes.size() == 2) {
    result = mbImpl->add_adjacencies(quad1, &(*hexes.rbegin()), 1, false); RR;
  }
  
    // create the new, inner quad, and make it explicitly adjacent to 1st hex;
    // make the connectivity of this quad same as the original one
  std::vector<EntityHandle> tmp_verts;
  std::copy(verts.begin(), verts.end(), std::back_inserter(tmp_verts));
  
  result = mbImpl->create_element(MBQUAD, &tmp_verts[0], 4, quad2); RR;
  result = mbImpl->add_adjacencies(quad2, &(*hexes.begin()), 1, false); RR;

    // reverse the connectivity of the 1st hex
  std::reverse(verts.begin(), verts.begin()+4);
  std::reverse(verts.begin()+4, verts.end());

    // now make two inner hexes; note connectivity array is flipped for the two hexes
  EntityHandle new_hexes[2];
  result = mbImpl->create_element(MBHEX, &verts[0], 8, new_hexes[0]); RR;
  result = mbImpl->create_element(MBHEX, &tmp_verts[0], 8, new_hexes[1]); RR;

    // set the global id tag on the new hexes
  int new_hex_ids[2] = {maxHexId+1, maxHexId+2};
  maxHexId += 2;
  result = mbImpl->tag_set_data(globalId_tag(), new_hexes, 2, new_hex_ids);
  if (MB_SUCCESS != result) return result;

    // by definition, quad1 is adj to new_hexes[0]
  result = mbImpl->add_adjacencies(quad1, &new_hexes[0], 1, false); RR;
  result = mbImpl->add_adjacencies(quad2, &new_hexes[1], 1, false); RR;

  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();

    // now update the dual
  result = construct_hex_dual(&new_hexes[0], 2); RR;

    // get the new dual surface, by getting one of the edges between the center
    // and outer vertex rings
  Range new_edge;
  verts[1] = verts[4];
  result = mbImpl->get_adjacencies(&verts[0], 2, 1, false, new_edge);
  if (MB_SUCCESS != result || new_edge.size() != 1) return result;
  
  return MB_SUCCESS;
}

  //! effect reverse atomic pillow operation
ErrorCode DualTool::rev_atomic_pillow(EntityHandle pillow, Range &chords) 
{
    // get the dual entities associated with elements in the pillow; go through
    // the elements instead of the pillow sheet so you get all of them, not just
    // the ones on the sheet
  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();

  std::cout << "-AP("; print_cell(pillow); std::cout << ")" << std::endl;

  Range dverts;
  ErrorCode result = get_dual_entities(pillow, NULL, NULL,
                                         &dverts, NULL, NULL);
  if (MB_SUCCESS != result) return result;
  assert(2 == dverts.size());
  
  EntityHandle hexes[2];
  result = mbImpl->tag_get_data(dualEntity_tag(), dverts, hexes); RR;
  assert(hexes[0] != 0 && hexes[1] != 0);

  std::vector<EntityHandle> dcells[4];
  Range pcells[4];
  std::copy(hexes, hexes+2, range_inserter(pcells[3]));
  std::copy(dverts.begin(), dverts.end(), std::back_inserter(dcells[0]));
  for (int dim = 0; dim <= 2; dim++) {
    result = mbImpl->get_adjacencies(hexes, 2, dim, false, pcells[dim], 
                                     Interface::UNION); RR;
    dcells[3-dim].resize(pcells[dim].size());
    result = mbImpl->tag_get_data(dualEntity_tag(), pcells[dim], &dcells[3-dim][0]); RR;
  }
  
    // delete the dual entities which are part of the original pillow
  result = mbImpl->delete_entities(&pillow, 1);
  if (MB_SUCCESS != result) return result;
  
  result = mbImpl->delete_entities(chords);
  if (MB_SUCCESS != result) return result;

  for (int i = 3; i >= 0; i--) {
    result = delete_dual_entities(&dcells[i][0], dcells[i].size()); RR;
  }

    // delete the primal entities inserted by the ap; be careful to get the right
    // faces, edges and vertices
  Range del_faces, del_edges, del_verts, tmp_faces, tmp_verts;
    // faces are the shared 5 and the 1 other one with greater handle (which
    // we know will be later in the range)
  result = mbImpl->get_adjacencies(hexes, 2, 2, false, del_faces); RR;
  assert(5 == del_faces.size());
  std::copy(pcells[2].begin(), pcells[2].end(), range_inserter(tmp_faces));
  tmp_faces = subtract( tmp_faces, del_faces);
  del_faces.insert(*tmp_faces.rbegin());
  result = mbImpl->get_adjacencies(tmp_faces, 0, false, tmp_verts); RR;
  std::copy(pcells[0].begin(), pcells[0].end(), range_inserter(del_verts));
  del_verts = subtract( del_verts, tmp_verts);
  assert(4 == del_verts.size());
  result = mbImpl->get_adjacencies(del_verts, 1, false, del_edges, Interface::UNION); RR;
  assert(8 == del_edges.size());
  
  result = mbImpl->delete_entities(hexes, 2); RR;
  result = mbImpl->delete_entities(del_faces); RR;
  result = mbImpl->delete_entities(del_edges); RR;
  result = mbImpl->delete_entities(del_verts); RR;

  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();

    // recompute the dual for the hexes on either side of the quad affected
    // by the ap removal
  Range tmp_hexes;
  result = mbImpl->get_adjacencies(tmp_verts, 3, false, tmp_hexes, Interface::UNION); RR;
  result = construct_hex_dual(tmp_hexes); RR;

  return MB_SUCCESS;
}

ErrorCode DualTool::delete_dual_entities(EntityHandle *entities,
                                           int num_entities) 
{
  Range tmp_ents;
  std::copy(entities, entities+num_entities, range_inserter(tmp_ents));
  return delete_dual_entities(tmp_ents);
}
  
ErrorCode DualTool::delete_dual_entities(Range &entities) 
{
  if (entities.empty()) return delete_whole_dual();
  
  EntityHandle null_entity = 0;
  ErrorCode result;
  Range ents_to_delete;
  
  while (!entities.empty()) {
    EntityHandle this_entity = entities.pop_back();
    
      // reset the primal's dual entity
    EntityHandle primal = get_dual_entity(this_entity);
    if (get_dual_entity(primal) == this_entity) {
      result = mbImpl->tag_set_data(dualEntity_tag(), &primal, 1, &null_entity); RR;
    }
    EntityHandle extra = get_extra_dual_entity(primal);
    if (0 != extra) {
      result = mbImpl->tag_set_data(extraDualEntity_tag(), &primal, 1, &null_entity); RR;
    }

    ents_to_delete.insert(this_entity);
    
      // check for extra dual entities
    if (mbImpl->type_from_handle(this_entity) == MBPOLYGON) {
      // for 2cell, might be a loop edge
      Range loop_edges;
      result = mbImpl->get_adjacencies(&this_entity, 1, 1, false, loop_edges);
      for (Range::iterator rit = loop_edges.begin(); rit != loop_edges.end(); ++rit)
        if (check_1d_loop_edge(*rit)) entities.insert(*rit);
    }
    else if (extra && extra != this_entity)
        // just put it on the list; primal for which we're extra has already been
        // reset to not point to extra entity
      ents_to_delete.insert(extra);
  }

    // now delete the entities (sheets and chords will be updated automatically)
  return mbImpl->delete_entities(ents_to_delete);
}

void DualTool::print_cell(EntityHandle cell) 
{
  const EntityHandle *connect;
  int num_connect;
  ErrorCode result = mbImpl->get_connectivity(cell, connect, num_connect);
  if (MB_SUCCESS != result) return;
  bool first = true;
  EntityHandle primals[20];
  std::vector<int> ids;
  
  assert(num_connect < 20);
  result = mbImpl->tag_get_data(dualEntityTag, connect, num_connect, primals);
  if (MB_SUCCESS != result) return;
  ids.resize(num_connect);
  result = mbImpl->tag_get_data(globalIdTag, primals, 
                             num_connect, &ids[0]);
  if (MB_SUCCESS != result) return;
  for (int i = 0; i < num_connect; i++) {
    if (!first) std::cout << "-";
    EntityType this_type = mbImpl->type_from_handle(primals[i]);
    if (this_type == MBHEX) std::cout << "h";
    else if (this_type == MBQUAD) std::cout << "f";
    else std::cout << "u";

    if (ids[i] != 0) std::cout << ids[i];
    else std::cout << mbImpl->id_from_handle(primals[i]);

    first = false;
  }
}

ErrorCode DualTool::face_open_collapse(EntityHandle ocl, EntityHandle ocr) 
{
  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();

  MeshTopoUtil mtu(mbImpl);

  std::cout << "OC("; print_cell(ocl); std::cout << ")-("; print_cell(ocr);
  std::cout << ")" << std::endl;

    // get the primal entities we're dealing with
  EntityHandle split_quads[2] = {0}, 
    split_edges[3] = {0}, split_nodes[2] = {0}, other_edges[6] = {0}, other_nodes[6] = {0};
  Range hexes;
  ErrorCode result = foc_get_ents(ocl, ocr, split_quads, split_edges, split_nodes,
                                    hexes, other_edges, other_nodes); RR;

    // get star entities around edges, separated into halves
  std::vector<EntityHandle> star_dp1[2], star_dp2[2];
  result = foc_get_stars(split_quads, split_edges, star_dp1, star_dp2); RR;

  if (MBQUAD != mbImpl->type_from_handle(split_quads[0]) ||
      MBQUAD != mbImpl->type_from_handle(split_quads[1]))
    return MB_TYPE_OUT_OF_RANGE;
  
  result = foc_delete_dual(split_quads, split_edges, hexes);
  if (MB_SUCCESS != result) return result;

  EntityHandle new_quads[2], new_edges[3], new_nodes[2];
  result = split_pair_nonmanifold(split_quads, split_edges, split_nodes, 
                                  star_dp1, star_dp2,
                                  other_edges, other_nodes, 
                                  new_quads, new_edges, new_nodes);
  if (MB_SUCCESS != result) return result;

    // now merge entities, the C of foc
  EntityHandle keepit, deleteit;
#define MIN(a,b) (a < b ? a : b)  
#define MAX(a,b) (a > b ? a : b)  
#define KEEP_DELETE(a,b,c,d) {c = MIN(a,b); d = MAX(a,b);}

    // find how many shared edges there were
  int num_shared_edges = (split_edges[2] ? 3 :
                          (split_edges[1] ? 2 : 1));
  
    // first the node(s)
  for (int i = 0; i < 3-num_shared_edges; i++) {
    KEEP_DELETE(other_nodes[2+2*i], other_nodes[3+2*i], keepit, deleteit);
    result = mbImpl->merge_entities(keepit, deleteit, false, true); RR;
  }
  
    // now the edges
  for (int i = 0; i < 4-num_shared_edges; i++) {
    KEEP_DELETE(other_edges[2*i], other_edges[2*i+1], keepit, deleteit);
    result = mbImpl->merge_entities(keepit, deleteit, false, true); RR;
  }

    // now the faces
  KEEP_DELETE(split_quads[0], split_quads[1], keepit, deleteit);
  result = mbImpl->merge_entities(keepit, deleteit, false, true); RR;
  
  result = mbImpl->merge_entities(new_quads[0], new_quads[1], false, true); RR;
  
  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();

    // reconstruct dual
  result = construct_hex_dual(hexes); 
  if (MB_SUCCESS != result) return result;

  return check_dual_adjs();
  
}

ErrorCode DualTool::foc_get_ents(EntityHandle ocl, 
                                   EntityHandle ocr, 
                                   EntityHandle *split_quads, 
                                   EntityHandle *split_edges, 
                                   EntityHandle *split_nodes, 
                                   Range &hexes, 
                                   EntityHandle *other_edges, 
                                   EntityHandle *other_nodes)
{
    // get the entities used for foc; ocl and ocr are dual 1-cells 
    // representing quads to be split; returned from this function:
    // quads[2] - 2 quads to be split
    // split_edges[2] - edge(s) to be split (2nd is 0 if only one)
    // split_node - node to be split, if any (otherwise 0)
    // hexes - connected hexes to split_edges
    // other_edges[0], [1] - edges in quads[0] and [1] sharing node with
    //        one end of split_edges[0]
    // other_edges[2], [3] - other end of split_edges[0] (or [1] if 2 
    //        split_edges)
    // other_edges[4], [5] - edges in quads[0], [1] opposite to split_edges[0]
    // other_nodes[0], [1] - nodes on other_edges[0], [1] not shared with 
    //        split_edges[0]
    // other_nodes[2], [3] - nodes on other_edges[2], [3] not shared with 
    //        split_edges[0] (if 2 split_edges, there's only 1 opposite node
    //        in each split quad)
    // (for diagram, see Tim's notes from 11/12/07)

  split_quads[0] = get_dual_entity(ocl);
  split_quads[1] = get_dual_entity(ocr);
  if (MBQUAD != mbImpl->type_from_handle(split_quads[0]) ||
      MBQUAD != mbImpl->type_from_handle(split_quads[1]))
    return MB_TYPE_OUT_OF_RANGE;

  Range common_edges;
  ErrorCode result = mbImpl->get_adjacencies(split_quads, 2, 1, false, 
                                               common_edges);
  if (MB_SUCCESS != result) return result;
  
  if (common_edges.empty()) return MB_FAILURE;
  for (unsigned int i = 0; i < common_edges.size(); i++) 
    split_edges[i] = common_edges[i];

  MeshTopoUtil mtu(mbImpl);

  if (common_edges.size() == 3) {
      // find other (non-shared) edges
    for (int i = 0; i < 2; i++) {
      Range tmp_edges;
      result = mbImpl->get_adjacencies(&split_quads[i], 1, 1, false, 
                                       tmp_edges);
      if (MB_SUCCESS != result) return result;
      tmp_edges = subtract( tmp_edges, common_edges);
      assert(tmp_edges.size() == 1);
      other_edges[i] = *tmp_edges.begin();
    }
    assert(other_edges[0] && other_edges[1] &&
           other_edges[0] != other_edges[1]);
    
      // arrange common edges so middle is in middle
    result = mtu.opposite_entity(split_quads[0], other_edges[0],
                                 split_edges[1]); RR;
    common_edges.erase(split_edges[1]);
    split_edges[0] = *common_edges.begin();
    split_edges[2] = *common_edges.rbegin();
    common_edges.insert(split_edges[1]);
    
      // get split nodes and other nodes
    split_nodes[0] = mtu.common_entity(split_edges[0], split_edges[1], 0);
    split_nodes[1] = mtu.common_entity(split_edges[2], split_edges[1], 0);
    other_nodes[0] = mtu.common_entity(split_edges[0], other_edges[0], 0);
    other_nodes[1] = mtu.common_entity(split_edges[2], other_edges[1], 0);

    assert(other_nodes[0] && other_nodes[1] && split_nodes[0] && split_nodes[1]);
    assert(split_edges[0] && split_edges[1] && split_edges[2] &&
           split_edges[0] != split_edges[1] && split_edges[1] != split_edges[2] &&
           split_edges[0] != split_edges[2]);
  }
  else if (common_edges.size() == 2) {
      // split node is shared by split edges
    split_nodes[0] = mtu.common_entity(split_edges[0], split_edges[1], 0);
    if (0 == split_nodes[0]) return MB_FAILURE;
      // first two other nodes are on split edges opposite split node
    result = mtu.opposite_entity(split_edges[0], split_nodes[0],
                                 other_nodes[0]); RR;
    result = mtu.opposite_entity(split_edges[1], split_nodes[0],
                                 other_nodes[1]); RR;
      // over split quads:
    for (int i = 0; i < 2; i++) {
        // 1st other edge is opposite second split edge
      result = mtu.opposite_entity(split_quads[i], 
                                   split_edges[1], other_edges[i]); RR;
        // 2nd other edge is opposite first split edge
      result = mtu.opposite_entity(split_quads[i], 
                                   split_edges[0], other_edges[2+i]); RR;
        // last other node is opposite split node on split quad
      result = mtu.opposite_entity(split_quads[i], split_nodes[0],
                                   other_nodes[2+i]); RR;
    }
  }
  else {
    const EntityHandle *connect;
    int num_connect;
    result = mbImpl->get_connectivity(split_edges[0], connect, num_connect);
    if (MB_SUCCESS != result) return result;
      // other_nodes[0], [1] are on split edge
    other_nodes[0] = connect[0];
    other_nodes[1] = connect[1];
      
      // for each of the split quads
    for (int i = 0; i < 2; i++) {
        // get the other edge on the split quad adj to node 0 on the split edge, by getting
        // edges adj to split quad and node and removing split edge; that's other_edge[i]
      Range tmp_range1, tmp_range2;
      tmp_range1.insert(connect[0]);
      tmp_range1.insert(split_quads[i]);
      result = mbImpl->get_adjacencies(tmp_range1, 1, false, tmp_range2);
      if (MB_SUCCESS != result) return result;
      tmp_range2.erase(split_edges[0]);
      assert(tmp_range2.size() == 1);
      other_edges[i] = *tmp_range2.begin();
        // get edge connected to other node on split edge & split quad; that's
        // opposite prev other_edges on the split quad; that's other_edges[4+i]
      result = mtu.opposite_entity(split_quads[i], other_edges[i],
                                   other_edges[4+i]); RR;
        // get the edge on the split quad opposite the split edge; that's other_edges[2+i]
      result = mtu.opposite_entity(split_quads[i], split_edges[0],
                                   other_edges[2+i]); RR;
        // get nodes on other side of split quad from split edge, by getting common
        // node between top/bottom edge and opposite edge
      other_nodes[2+i] = mtu.common_entity(other_edges[i], other_edges[2+i], 0);
      other_nodes[4+i] = mtu.common_entity(other_edges[4+i], other_edges[2+i], 0);
      if (0 == other_nodes[2+i] || 0 == other_nodes[4+i]) return MB_FAILURE;
    }
  }

  result = mbImpl->get_adjacencies(split_edges, common_edges.size(), 3, false, 
                                   hexes, Interface::UNION);
  if (MB_SUCCESS != result) return result;

  assert("split node not in other_nodes" &&
         other_nodes[0] != split_nodes[0] && other_nodes[0] != split_nodes[1] &&
         other_nodes[1] != split_nodes[0] && other_nodes[1] != split_nodes[1]);
  assert("each split node on an end of a split edge" &&
         mtu.common_entity(other_nodes[0], split_edges[0], 0) &&
         (((split_edges[2] && mtu.common_entity(other_nodes[1], split_edges[2], 0)) ||
           (split_edges[1] && mtu.common_entity(other_nodes[1], split_edges[1], 0)) ||
           mtu.common_entity(other_nodes[1], split_edges[0], 0))));
  assert("opposite other edges meet at an other node" &&
         (mtu.common_entity(other_edges[0], other_edges[1], 0) == other_nodes[0] ||
          (split_edges[2] && 
           mtu.common_entity(other_edges[0], other_edges[1], 0) == other_nodes[1])) &&
         (split_edges[2] || 
          (split_edges[1] && 
           mtu.common_entity(other_edges[2], other_edges[3], 0) == other_nodes[1]) ||
          mtu.common_entity(other_edges[4], other_edges[5], 0) == other_nodes[1]));
         
  return MB_SUCCESS;
}

ErrorCode DualTool::split_pair_nonmanifold(EntityHandle *split_quads,
                                             EntityHandle *split_edges,
                                             EntityHandle *split_nodes,
                                             std::vector<EntityHandle> *star_dp1,
                                             std::vector<EntityHandle> *star_dp2,
                                             EntityHandle * /*other_edges*/,
                                             EntityHandle * /*other_nodes*/,
                                             EntityHandle *new_quads,
                                             EntityHandle *new_edges,
                                             EntityHandle *new_nodes)
{

    // if there's a bdy in the star around the shared edge(s), get the quads on that
    // bdy so we know which edges to merge after the split-nonmanifold
  MeshTopoUtil mtu(mbImpl);
  ErrorCode result;

    // get which star the split faces are in, and choose the other one
  int new_side = -1;
  if (std::find(star_dp1[0].begin(), star_dp1[0].end(), split_quads[0]) !=
      star_dp1[0].end())
    new_side = 1;
  else if (std::find(star_dp1[1].begin(), star_dp1[1].end(), split_quads[0]) !=
           star_dp1[1].end())
    new_side = 0;
  assert(-1 != new_side);
  if (-1 == new_side) return MB_FAILURE;
    
    //=============== split faces

  for (int i = 0; i < 2; i++) {
      // get a hex in star_dp2[new_side] that's adj to this split quad, to tell 
      // mtu which one the new quad should go with; there should be at least one,
      // if we have any hexes connected to the split quad
    EntityHandle gowith_hex = 0;
    for (std::vector<EntityHandle>::iterator vit = star_dp2[new_side].begin();
         vit != star_dp2[new_side].end(); ++vit) {
      if (mtu.common_entity(*vit, split_quads[i], 2)) {
        gowith_hex = *vit;
        break;
      }
    }
    assert(0 != gowith_hex);
    
    // split manifold each of the split_quads, and put the results on the merge list
    result = mtu.split_entities_manifold(split_quads+i, 1, new_quads+i, NULL,
                                         (gowith_hex ? &gowith_hex : NULL)); RR;
  }

    // make ranges of faces which need to be explicitly adj to old, new
    // edge; faces come from stars and new_quads (which weren't in the stars);
    // new_quads go with side j, which does not have split quads
  Range tmp_addl_faces[2], addl_faces[2];
  for (int i = 0; i < 2; i++) {
    std::copy(star_dp1[i].begin(), star_dp1[i].end(), 
            range_inserter(tmp_addl_faces[i]));
    tmp_addl_faces[new_side].insert(new_quads[i]);
  }
#ifndef NDEBUG
  bool cond1 = ("split_quads on 1, new_quads on 0" &&
          tmp_addl_faces[0].find(split_quads[0]) == tmp_addl_faces[0].end() &&
          tmp_addl_faces[0].find(split_quads[1]) == tmp_addl_faces[0].end() &&
          tmp_addl_faces[1].find(split_quads[0]) != tmp_addl_faces[1].end() &&
          tmp_addl_faces[1].find(split_quads[1]) != tmp_addl_faces[1].end() &&
          tmp_addl_faces[0].find(new_quads[0]) != tmp_addl_faces[0].end() &&
          tmp_addl_faces[0].find(new_quads[1]) != tmp_addl_faces[0].end() &&
          tmp_addl_faces[1].find(new_quads[0]) == tmp_addl_faces[1].end() &&
                tmp_addl_faces[1].find(new_quads[1]) == tmp_addl_faces[1].end()),
      cond2 = ("split_quads on 0, new_quads on 1" &&
               tmp_addl_faces[0].find(split_quads[0]) != tmp_addl_faces[0].end() &&
               tmp_addl_faces[0].find(split_quads[1]) != tmp_addl_faces[0].end() &&
               tmp_addl_faces[1].find(split_quads[0]) == tmp_addl_faces[1].end() &&
               tmp_addl_faces[1].find(split_quads[1]) == tmp_addl_faces[1].end() &&
               tmp_addl_faces[0].find(new_quads[0]) == tmp_addl_faces[0].end() &&
               tmp_addl_faces[0].find(new_quads[1]) == tmp_addl_faces[0].end() &&
               tmp_addl_faces[1].find(new_quads[0]) != tmp_addl_faces[1].end() &&
               tmp_addl_faces[1].find(new_quads[1]) != tmp_addl_faces[1].end());
  
  assert(cond1 || cond2);
#endif

    //=============== split edge(s)
  for (int j = 0; j < 3; j++) {
    if (!split_edges[j]) break;
    
      // filter add'l faces to only those adj to split_edges[j]
    addl_faces[0] = tmp_addl_faces[0]; addl_faces[1] = tmp_addl_faces[1];
    for (int i = 0; i < 2; i++) {
      result = mbImpl->get_adjacencies(&split_edges[j], 1, 2, false, 
                                       addl_faces[i]); RR;
    }
  
      // split edge
    result = mtu.split_entity_nonmanifold(split_edges[j], addl_faces[1-new_side], 
                                          addl_faces[new_side], new_edges[j]); RR;
  }
  
    //=============== split node(s)

  for (int j = 0; j < 2; j++) {
    if (!split_nodes[j]) break;
    
      // if we're splitting multiple edges, there might be other edges that have the split
      // node; also need to know which side they're on
    Range tmp_addl_edges[2];
    result = foc_get_addl_ents(star_dp1, star_dp2, split_edges, 
                               split_nodes[j], tmp_addl_edges); RR;

      // also, we need to know which of the split/new edges go
      // with the split/new node; new edges go with side 0, split with 1
    for (int i = 0; i < 3; i++) {
      if (!split_edges[i]) break;
      tmp_addl_edges[new_side].insert(new_edges[i]);
      tmp_addl_edges[1-new_side].insert(split_edges[i]);
    }

      // same for star faces and hexes
    for (int i = 0; i < 2; i++) {
      std::copy(star_dp1[i].begin(), star_dp1[i].end(), range_inserter(tmp_addl_edges[i]));
      std::copy(star_dp2[i].begin(), star_dp2[i].end(), range_inserter(tmp_addl_edges[i]));
    }

      // finally, new quads
    for (int i = 0; i < 2; i++) tmp_addl_edges[new_side].insert(new_quads[i]);

      // filter the entities, keeping only the ones adjacent to this node
    Range addl_edges[2];
    for (int i = 0; i < 2; i++) {
      for (Range::reverse_iterator rit = tmp_addl_edges[i].rbegin(); 
           rit != tmp_addl_edges[i].rend(); ++rit) {
        if (mtu.common_entity(*rit, split_nodes[j], 0)) addl_edges[i].insert(*rit);
      }
    }
    
      // now split the node too
    result = mtu.split_entity_nonmanifold(split_nodes[j], addl_edges[1-new_side], 
                                          addl_edges[new_side], new_nodes[j]); RR;
  }
  
  return MB_SUCCESS;
}

ErrorCode DualTool::foc_get_addl_ents(std::vector<EntityHandle> *star_dp1, 
                                        std::vector<EntityHandle> * /*star_dp2*/, 
                                        EntityHandle *split_edges,
                                        EntityHandle split_node,
                                        Range *addl_ents) 
{
    // if we're splitting 2 edges, there might be other edges that have the split
    // node; also need to know which side they're on

    // algorithm: for a given star_dp1 (faces) on a side:
    // - get all edges adj to all faces -> R1
    // - get all edges adj to split_node -> R2
    // - R3 = R1 & R2 (edges connected to split_node & adj to a star face)
    // - R3 -= split_edges (take split edges off addl_ents)

  Range R2;
  MeshTopoUtil mtu(mbImpl);
  ErrorCode result = mbImpl->get_adjacencies(&split_node, 1, 1, false, R2); RR;
  Range::iterator rit;

  for (int i = 0; i < 2; i++) {
    Range R1, R3;
    result = mbImpl->get_adjacencies(&star_dp1[i][0], star_dp1[i].size(), 1, false, 
                                     R1, Interface::UNION); RR;
    R3 = intersect( R1, R2);
    for (int j = 0; j < 3; j++)
      if (split_edges[j]) R3.erase(split_edges[j]);
    addl_ents[i].merge(R3);
  }
  
  return MB_SUCCESS;
}

ErrorCode DualTool::foc_get_stars(EntityHandle *split_quads,
                                    EntityHandle *split_edges,
                                    std::vector<EntityHandle> *star_dp1,
                                    std::vector<EntityHandle> *star_dp2) 
{
  bool on_bdy = false, on_bdy_tmp;
  ErrorCode result;
  MeshTopoUtil mtu(mbImpl);
  
    // get the star around the split_edge
  std::vector<EntityHandle> qstar, hstar;
  unsigned int qpos = 0;
  
  for (int i = 0; i < 3; i++) {
    if (!split_edges[i]) break;
    
      // get the star around this split edge
    unsigned int qpos_tmp = 0;
    std::vector<EntityHandle> qstar_tmp, hstar_tmp;
    result = mtu.star_entities(split_edges[i], qstar_tmp, on_bdy_tmp, 0,
                             &hstar_tmp); RR;
      // if we're on the bdy, add a null to the hex star too
    if (on_bdy_tmp) {
      assert(hstar_tmp.size() == qstar_tmp.size()-1);
      hstar_tmp.push_back(0);
      on_bdy = true;
    }
    
      // get the position of first split quad in star
    while (qpos_tmp < qstar_tmp.size() && qstar_tmp[qpos_tmp] != split_quads[0])
      qpos_tmp++;
    if (qpos_tmp == qstar_tmp.size()) return MB_FAILURE;
  
    bool forward;
      // 1st iteration is forward by definition
    if (0 == i) forward = true;

      // need to be careful about direction on later iters
    else if (hstar[qpos] == hstar_tmp[qpos_tmp]) forward = true;
    else if (hstar[qpos] == hstar_tmp[(qpos_tmp+qstar_tmp.size()-1)%qstar_tmp.size()] &&
             hstar_tmp[qpos_tmp] == hstar[(qpos+qstar.size()-1)%qstar.size()])
      forward = false;
    else return MB_FAILURE;
    
    if (forward) {
        // 1st half of star
        // save hex right after split_quad[0] first
      star_dp2[0].push_back(hstar_tmp[qpos_tmp]);
      qpos_tmp = (qpos_tmp+1)%qstar_tmp.size();
      while (qstar_tmp[qpos_tmp] != split_quads[1]) {
        star_dp1[0].push_back(qstar_tmp[qpos_tmp]);
        star_dp2[0].push_back(hstar_tmp[qpos_tmp]);
        qpos_tmp = (qpos_tmp+1)%qstar_tmp.size();
      }
        // 2nd half of star
        // save hex right after split_quad[1] first
      star_dp2[1].push_back(hstar_tmp[qpos_tmp]);
      qpos_tmp = (qpos_tmp+1)%qstar_tmp.size();
      while (qstar_tmp[qpos_tmp] != split_quads[0]) {
        star_dp1[1].push_back(qstar_tmp[qpos_tmp]);
        star_dp2[1].push_back(hstar_tmp[qpos_tmp]);
        qpos_tmp = (qpos_tmp+1)%qstar_tmp.size();
      }
    }
    else {
        // go in reverse - take prev hex instead of current
        // one, and step in reverse

        // save hex right after split_quad[0] first
      qpos_tmp = (qpos_tmp+qstar_tmp.size()-1)%qstar_tmp.size();
      star_dp2[0].push_back(hstar_tmp[qpos_tmp]);
      while (qstar_tmp[qpos_tmp] != split_quads[1]) {
        star_dp1[0].push_back(qstar_tmp[qpos_tmp]);
        qpos_tmp = (qpos_tmp+qstar_tmp.size()-1)%qstar_tmp.size();
        star_dp2[0].push_back(hstar_tmp[qpos_tmp]);
      }
        // 2nd half of star
        // save hex right after split_quad[1] first
      qpos_tmp = (qpos_tmp+qstar_tmp.size()-1)%qstar_tmp.size();
      star_dp2[1].push_back(hstar_tmp[qpos_tmp]);
      while (qstar_tmp[qpos_tmp] != split_quads[0]) {
        star_dp1[1].push_back(qstar_tmp[qpos_tmp]);
        qpos_tmp = (qpos_tmp+qstar_tmp.size()-1)%qstar_tmp.size();
        star_dp2[1].push_back(hstar_tmp[qpos_tmp]);
      }
    }

    if (0 == i) {
      // if we're on the first iteration, save results and continue, other iters
      // get compared to this one
      qstar.swap(qstar_tmp);
      hstar.swap(hstar_tmp);
      on_bdy = on_bdy_tmp;
      qpos = qpos_tmp;
    }
  }
  
    // split quads go on list with NULLs, if any, otherwise on 2nd
  if (on_bdy) {
    if (std::find(star_dp2[0].begin(), star_dp2[0].end(), 0) != 
        star_dp2[0].end()) {
        // remove *all* the zeros
      star_dp2[0].erase(std::remove(star_dp2[0].begin(), star_dp2[0].end(), 0),
                        star_dp2[0].end());
        // put the split quads on this half
      star_dp1[0].push_back(split_quads[0]);
      star_dp1[0].push_back(split_quads[1]);
    }
    else {
      star_dp2[1].erase(std::remove(star_dp2[1].begin(), star_dp2[1].end(), 0),
                        star_dp2[1].end());
        // put the split quads on this half
      star_dp1[1].push_back(split_quads[0]);
      star_dp1[1].push_back(split_quads[1]);
    }
  }
  else {
    star_dp1[1].push_back(split_quads[0]);
    star_dp1[1].push_back(split_quads[1]);
  }

    // some error checking
  if (!(((std::find(star_dp1[0].begin(), star_dp1[0].end(), split_quads[0]) == star_dp1[0].end() &&
          std::find(star_dp1[0].begin(), star_dp1[0].end(), split_quads[1]) == star_dp1[0].end() &&
          std::find(star_dp1[1].begin(), star_dp1[1].end(), split_quads[0]) != star_dp1[1].end() &&
          std::find(star_dp1[1].begin(), star_dp1[1].end(), split_quads[1]) != star_dp1[1].end()) ||
         
         (std::find(star_dp1[1].begin(), star_dp1[1].end(), split_quads[0]) == star_dp1[1].end() &&
          std::find(star_dp1[1].begin(), star_dp1[1].end(), split_quads[1]) == star_dp1[1].end() &&
          std::find(star_dp1[0].begin(), star_dp1[0].end(), split_quads[0]) != star_dp1[0].end() &&
          std::find(star_dp1[0].begin(), star_dp1[0].end(), split_quads[1]) != star_dp1[0].end()))
        )) {
    std::cerr << "foc_get_stars: both split quads should be on the same star list half and not "
              << "on the other, failed" << std::endl;
    return MB_FAILURE;
  }
  
  if (!(std::find(star_dp2[0].begin(), star_dp2[0].end(), 0) == star_dp2[0].end() &&
        std::find(star_dp2[1].begin(), star_dp2[1].end(), 0) == star_dp2[1].end())) {
    std::cerr << "foc_get_stars: no NULLs on the hstar lists, failed";
    return MB_FAILURE;
  }
  
  return MB_SUCCESS;
}

ErrorCode DualTool::foc_delete_dual(EntityHandle *split_quads,
                                      EntityHandle *split_edges,
                                      Range &hexes) 
{
    // special delete dual procedure, because in some cases we need to delete
    // a sheet too since it'll get merged into another

    // figure out whether we'll need to delete a sheet
  EntityHandle sheet1 = get_dual_hyperplane(get_dual_entity(split_edges[0]));
  if (split_edges[1]) sheet1 = get_dual_hyperplane(get_dual_entity(split_edges[1]));
  EntityHandle chordl = get_dual_hyperplane(get_dual_entity(split_quads[0]));
  EntityHandle chordr = get_dual_hyperplane(get_dual_entity(split_quads[1]));
  assert(0 != sheet1 && 0 != chordl && 0 != chordr);
  Range parentsl, parentsr;
  ErrorCode result = mbImpl->get_parent_meshsets(chordl, parentsl);
  if (MB_SUCCESS != result) return result;
  result = mbImpl->get_parent_meshsets(chordr, parentsr);
  if (MB_SUCCESS != result) return result;
  parentsl.erase(sheet1);
  parentsr.erase(sheet1);

    // before deciding which one to delete, collect the other cells which must
    // be deleted, and all the chords/sheets they're on
  Range adj_ents, dual_ents, cells1or2;
  for (int i = 0; i < 3; i++) {
    result = mbImpl->get_adjacencies(hexes, i, false, adj_ents, Interface::UNION);
    if (MB_SUCCESS != result) return result;
  }

    // cache any adjacent hexes, for rebuilding the dual later
  result = mbImpl->get_adjacencies(adj_ents, 3, false, hexes, Interface::UNION);
  if (MB_SUCCESS != result) return result;
  
  for (Range::iterator rit = adj_ents.begin(); rit != adj_ents.end(); ++rit) {
    EntityHandle this_ent = get_dual_entity(*rit);
    dual_ents.insert(this_ent);
    int dim = mbImpl->dimension_from_handle(this_ent);
    if (1 == dim || 2 == dim) cells1or2.insert(this_ent);
  }

  Range dual_hps;
  for (Range::iterator rit = cells1or2.begin(); rit != cells1or2.end(); ++rit)
    dual_hps.insert(get_dual_hyperplane(*rit));

  result = delete_dual_entities(dual_ents);
  if (MB_SUCCESS != result) return result;

    // now decide which sheet to delete (to be merged into the other)
  EntityHandle sheet_delete = 0;
  if (is_blind(*parentsl.begin())) sheet_delete = *parentsl.begin();
  else if (is_blind(*parentsr.begin())) sheet_delete = *parentsr.begin();
  else {
      // neither is blind, take the one with fewer cells
    Range tmp_ents;
    int numl, numr;
    result = mbImpl->get_number_entities_by_handle(*parentsl.begin(), numl);
    if (MB_SUCCESS != result) return result;
    result = mbImpl->get_number_entities_by_handle(*parentsr.begin(), numr);
    if (MB_SUCCESS != result) return result;
    sheet_delete = (numl > numr ? *parentsr.begin() : *parentsl.begin());
  }
  assert(0 != sheet_delete);
  
    // after deleting cells, check for empty chords & sheets, and delete those too
  for (Range::iterator rit = dual_hps.begin(); rit != dual_hps.end(); ++rit) {
    Range tmp_ents;
    result = mbImpl->get_entities_by_handle(*rit, tmp_ents);
    if (MB_SUCCESS != result) return result;
    if (tmp_ents.empty()) {
      result = mbImpl->delete_entities(&(*rit), 1);
      if (MB_SUCCESS != result) return result;
    }
    else if (*rit == sheet_delete) {
        // delete the sheet
      result = mbImpl->delete_entities(&(*rit), 1);
      if (MB_SUCCESS != result) return result;
    }
  }

    // now just to be safe, add the hexes bridge-adjacent across vertices
    // to the hexes we already have
  Range tmp_hexes;
  MeshTopoUtil mtu(mbImpl);
  for (Range::iterator rit = hexes.begin(); rit != hexes.end(); ++rit) {
    result = mtu.get_bridge_adjacencies(*rit, 0, 3, tmp_hexes);
    if (MB_SUCCESS != result) return result;
  }
  hexes.merge(tmp_hexes);

  return MB_SUCCESS;
}

//! returns true if all vertices are dual to hexes (not faces)
bool DualTool::is_blind(const EntityHandle chord_or_sheet) 
{
    // must be an entity set
  if (TYPE_FROM_HANDLE(chord_or_sheet) != MBENTITYSET) return false;
  
    // get the vertices
  Range verts, ents;
  ErrorCode result = mbImpl->get_entities_by_handle(chord_or_sheet, ents); 
  if (MB_SUCCESS != result || ents.empty()) return false;
  
  result = mbImpl->get_adjacencies(ents, 0, false, verts, Interface::UNION);
  if (MB_SUCCESS != result || verts.empty()) return false;
  
  for (Range::iterator rit = verts.begin(); rit != verts.end(); ++rit) {
      // get dual entity for this vertex
    EntityHandle dual_ent = get_dual_entity(*rit);
    if (0 == dual_ent) continue;
    if (TYPE_FROM_HANDLE(dual_ent) == MBQUAD) return false;
  }

    // if none of the vertices' duals were quads, chord_or_sheet must be blind
  return true;
}

  //! given a 1-cell and a chord, return the neighboring vertices on the
  //! chord, in the same order as the 1-cell's vertices
ErrorCode DualTool::get_opposite_verts(const EntityHandle middle_edge, 
                                         const EntityHandle chord, 
                                         EntityHandle *verts) 
{
    // get the edges on the chord, in order, and move to middle_edge
  std::vector<EntityHandle> chord_edges;
  const EntityHandle *connect;
  int num_connect;
  
  ErrorCode result = mbImpl->get_entities_by_handle(chord, chord_edges); RR;
  std::vector<EntityHandle>::iterator vit = std::find(chord_edges.begin(), chord_edges.end(),
                                                        middle_edge);
  result = mbImpl->get_connectivity(middle_edge, connect, num_connect); RR;

  if (
      // middle_edge isn't on this chord
    vit == chord_edges.end() || 
      // chord only has 1 edge
    chord_edges.size() == 1 ||
      // middle_edge is at beginning or end and chord isn't blind
      ((vit == chord_edges.begin() || vit == chord_edges.end()-1) && !is_blind(chord))) 
    return MB_FAILURE;

  else if (chord_edges.size() == 2) {
      // easier if it's a 2-edge blind chord, just get vertices in opposite order
    verts[0] = connect[1];
    verts[1] = connect[0];
    return MB_SUCCESS;
  }
  
    // get vertices with the prev edge & subtract vertices of 1-cell
  if (vit == chord_edges.begin())
    vit = chord_edges.end() - 1;
  else --vit;
  Range dum_connect, middle_connect;
  result = mbImpl->get_connectivity(&middle_edge, 1, middle_connect); RR;
  result = mbImpl->get_connectivity(&(*vit), 1, dum_connect); RR;
  dum_connect = subtract( dum_connect, middle_connect);
  if (dum_connect.size() != 1) {
    std::cerr << "Trouble traversing chord." << std::endl;
    return MB_FAILURE;
  }

    // put in verts[0]
  verts[0] = *dum_connect.begin();

    // same with prev edge
  ++vit;
  if (vit == chord_edges.end()) vit = chord_edges.begin();
  ++vit;
  dum_connect.clear();
  result = mbImpl->get_connectivity(&(*vit), 1, dum_connect); RR;
  dum_connect = subtract( dum_connect, middle_connect);
  if (dum_connect.size() != 1) {
    std::cerr << "Trouble traversing chord." << std::endl;
    return MB_FAILURE;
  }

    // put in verts[1]
  verts[1] = *dum_connect.begin();

    // if verts[0] and 1st vertex of 1cell don't have common edge, switch verts
  MeshTopoUtil mtu(mbImpl);
  if (0 == mtu.common_entity(verts[0], connect[0], 1)) {
    EntityHandle dum_h = verts[0];
    verts[0] = verts[1];
    verts[1] = dum_h;
  }
  
  if (0 == mtu.common_entity(verts[0], connect[0], 1)) {
    std::cerr << "Trouble traversing chord." << std::endl;
    return MB_FAILURE;
  }
  
  return MB_SUCCESS;
}

ErrorCode DualTool::get_dual_entities(const EntityHandle dual_ent,
                                        Range *dcells,
                                        Range *dedges,
                                        Range *dverts,
                                        Range *dverts_loop,
                                        Range *dedges_loop)
{
  ErrorCode result = MB_SUCCESS;

  if (NULL != dcells) {
    result = mbImpl->get_entities_by_type(dual_ent, MBPOLYGON, *dcells);
    if (MB_SUCCESS != result) return result;
  }
  
  if (NULL != dedges) {
    if (NULL != dcells)
      result = mbImpl->get_adjacencies(*dcells, 1, false, *dedges, Interface::UNION);
    else 
      result = mbImpl->get_entities_by_type(dual_ent, MBEDGE, *dedges);

    if (MB_SUCCESS != result) return result;
  }

  if (NULL != dverts) {
    if (NULL != dcells)
      result = mbImpl->get_adjacencies(*dcells, 0, false, *dverts, Interface::UNION);
    else if (NULL != dedges)
      result = mbImpl->get_adjacencies(*dedges, 0, false, *dverts, Interface::UNION);
    else {
      Range all_ents;
      result = mbImpl->get_entities_by_handle(dual_ent, all_ents); RR;
      result = mbImpl->get_adjacencies(all_ents, 0, false, *dverts, Interface::UNION);
    }

    if (MB_SUCCESS != result) return result;
  }

  if (NULL != dverts_loop && NULL != dverts) {
    static std::vector<EntityHandle> dual_ents;
    dual_ents.resize(dverts->size());
    result = mbImpl->tag_get_data(dualEntity_tag(), *dverts, &dual_ents[0]);
    if (MB_SUCCESS != result) return result;
    Range::iterator rit;
    unsigned int i;
    for (rit = dverts->begin(), i = 0; rit != dverts->end(); ++rit, i++)
      if (0 != dual_ents[i] && mbImpl->type_from_handle(dual_ents[i]) == MBQUAD)
        dverts_loop->insert(*rit);
  }
  
  if (NULL != dedges_loop && NULL != dedges) {
    static std::vector<EntityHandle> dual_ents;
    dual_ents.resize(dedges->size());
    result = mbImpl->tag_get_data(dualEntity_tag(), *dedges, &dual_ents[0]);
    if (MB_SUCCESS != result) return result;
    Range::iterator rit;
    unsigned int i;
    for (rit = dedges->begin(), i = 0; rit != dedges->end(); ++rit, i++)
      if (0 != dual_ents[i] && mbImpl->type_from_handle(dual_ents[i]) == MBEDGE)
        dedges_loop->insert(*rit);
  }

  return result;
}

ErrorCode DualTool::list_entities(const EntityHandle *entities,
                                    const int num_entities) const
{
  Range temp_range;
  ErrorCode result;
  if (NULL == entities && 0 == num_entities) 
    return mbImpl->list_entities(entities, num_entities);
  
  else if (NULL == entities && 0 < num_entities) {

      // list all entities of all types
    std::cout << std::endl;
    for (EntityType this_type = MBVERTEX; this_type < MBMAXTYPE; this_type++) {
      result = mbImpl->get_entities_by_type(0, this_type, temp_range);
      if (MB_SUCCESS != result) return result;
    }
  }
  
  else {
    std::copy(entities, entities+num_entities, range_inserter(temp_range));
  }

  return list_entities(temp_range);
}
  
ErrorCode DualTool::list_entities(const Range &entities) const
{
    // now print each entity, listing the dual information first then calling Interface to do
    // the rest
  ErrorCode result = MB_SUCCESS, tmp_result;
  for (Range::const_iterator iter = entities.begin(); iter != entities.end(); ++iter) {
    EntityType this_type = TYPE_FROM_HANDLE(*iter);
    std::cout << CN::EntityTypeName(this_type) << " " << ID_FROM_HANDLE(*iter) << ":" << std::endl;

    EntityHandle dual_ent = get_dual_entity(*iter);
    if (0 != dual_ent) {
      std::cout << "Dual to " 
                << CN::EntityTypeName(mbImpl->type_from_handle(dual_ent)) << " "
                << mbImpl->id_from_handle(dual_ent) << std::endl;
    }

    if (TYPE_FROM_HANDLE(*iter) == MBENTITYSET) {
      EntityHandle chord = 0, sheet = 0;
      int id;
      result = mbImpl->tag_get_data(dualCurve_tag(), &(*iter), 1, &chord);
      if (MB_SUCCESS != result) return result;
      result = mbImpl->tag_get_data(dualSurface_tag(), &(*iter), 1, &sheet);
      if (MB_SUCCESS != result) return result;
      result = mbImpl->tag_get_data(globalId_tag(), &(*iter), 1, &id);
      if (MB_SUCCESS != result) return result;
        
      if (0 != chord)
        std::cout << "(Dual chord " << id << ")" << std::endl;
      if (0 != sheet)
        std::cout << "(Dual sheet " << id << ")" << std::endl;
    }

    tmp_result = mbImpl->list_entity(*iter);
    if (MB_SUCCESS != tmp_result) result = tmp_result;
  }

  return result;
}

ErrorCode DualTool::face_shrink(EntityHandle odedge) 
{
    // some preliminary checking
  if (mbImpl->type_from_handle(odedge) != MBEDGE) return MB_TYPE_OUT_OF_RANGE;

  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();
  
  std::cout << "FS("; print_cell(odedge); std::cout << ")" << std::endl;

  EntityHandle quads[4], hexes[2];
  std::vector<EntityHandle> connects[4], side_quads[2];

    // get the quads along the chord through the 2 hexes, and the vertices 
    // for those quads
  ErrorCode result = fs_get_quads(odedge, quads, hexes, connects);
  if (MB_SUCCESS != result) return result;
  
    // flip/rotate connect arrays so they align & are same sense
  result = fs_check_quad_sense(hexes[0], quads[0], connects);
  if (MB_SUCCESS != result) return result;

    // get the quad loops along the "side" surfaces
  result = fs_get_quad_loops(hexes, connects, side_quads);
  if (MB_SUCCESS != result) return result;
  
    // ok, done with setup; now delete dual entities affected by this operation,
    // which is all the entities adjacent to vertices of dual edge
  Range adj_verts, adj_edges, dual_ents, cells1or2;
  MeshTopoUtil mtu(mbImpl);
  result = mtu.get_bridge_adjacencies(odedge, 0, 1, adj_edges);
  if (MB_SUCCESS != result) return result;
  result = mbImpl->get_adjacencies(adj_edges, 0, false, adj_verts, 
                                   Interface::UNION);
  if (MB_SUCCESS != result) return result;
  for (int i = 1; i <= 3; i++) {
    result = mbImpl->get_adjacencies(adj_verts, i, false, dual_ents, 
                                     Interface::UNION);
    if (MB_SUCCESS != result) return result;
  }

    // before deleting dual, grab the 1- and 2-cells
  for (Range::iterator rit = dual_ents.begin(); rit != dual_ents.end(); ++rit) {
    int dim = mbImpl->dimension_from_handle(*rit);
    if (1 == dim || 2 == dim) cells1or2.insert(*rit);
  }
  Range dual_hps;
  for (Range::iterator rit = cells1or2.begin(); rit != cells1or2.end(); ++rit)
    dual_hps.insert(get_dual_hyperplane(*rit));
  
  dual_ents.insert(odedge);
  result = delete_dual_entities(dual_ents);
  if (MB_SUCCESS != result) return result;
  
    // after deleting cells, check for empty chords & sheets, and delete those too
  for (Range::iterator rit = dual_hps.begin(); rit != dual_hps.end(); ++rit) {
    Range tmp_ents;
    result = mbImpl->get_entities_by_handle(*rit, tmp_ents);
    if (MB_SUCCESS != result) return result;
    if (tmp_ents.empty()) {
      result = mbImpl->delete_entities(&(*rit), 1);
      if (MB_SUCCESS != result) return result;
    }
  }

    // remove any explicit adjacencies between side quads and hexes; don't check
    // for error, since there might not be adjacencies
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 2; j++) {
      result = mbImpl->remove_adjacencies(side_quads[j][i], &hexes[j], 1);
    }
  }
  
    // make inner ring of vertices
    // get centroid of quad2
  double q2coords[12], avg[3] = {0.0, 0.0, 0.0};
  result = mbImpl->get_coords(&connects[1][0], 4, q2coords);
  if (MB_SUCCESS != result) return result;
  for (int i = 0; i < 4; i++) {
    avg[0] += q2coords[3*i];
    avg[1] += q2coords[3*i+1];
    avg[2] += q2coords[3*i+2];
  }
  avg[0] *= .25; avg[1] *= .25; avg[2] *= .25;
    // position new vertices
  connects[3].resize(4);
  for (int i = 0; i < 4; i++) {
    q2coords[3*i] = avg[0] + .25*(q2coords[3*i]-avg[0]);
    q2coords[3*i+1] = avg[1] + .25*(q2coords[3*i+1]-avg[1]);
    q2coords[3*i+2] = avg[2] + .25*(q2coords[3*i+2]-avg[2]);
    result = mbImpl->create_vertex(&q2coords[3*i], connects[3][i]);
    if (MB_SUCCESS != result) return result;
  }
  
    // ok, now have the 4 connectivity arrays for 4 quads; construct hexes
  EntityHandle hconnect[8], new_hexes[4];
  int new_hex_ids[4];
  
  for (int i = 0; i < 4; i++) {
    int i1 = i, i2 = (i+1)%4;
    hconnect[0] = connects[0][i1];
    hconnect[1] = connects[0][i2];
    hconnect[2] = connects[3][i2];
    hconnect[3] = connects[3][i1];

    hconnect[4] = connects[1][i1];
    hconnect[5] = connects[1][i2];
    hconnect[6] = connects[2][i2];
    hconnect[7] = connects[2][i1];
    
    result = mbImpl->create_element(MBHEX, hconnect, 8, new_hexes[i]);
    if (MB_SUCCESS != result) return result;

      // test for equiv entities from the side quads, and make explicit adjacencies
      // if there are any
    for (int j = 0; j < 2; j++) {
      if (mtu.equivalent_entities(side_quads[j][i])) {
        result = mbImpl->add_adjacencies(side_quads[j][i], &new_hexes[i], 1, false);
        if (MB_SUCCESS != result) return result;
      }
    }

    new_hex_ids[i] = ++maxHexId;
  }

    // set the global id tag on the new hexes
  result = mbImpl->tag_set_data(globalId_tag(), new_hexes, 4, new_hex_ids);
  if (MB_SUCCESS != result) return result;
  
    // now fixup other two hexes; start by getting hex through quads 0, 1       
    // make this first hex switch to the other side, to make the dual look like
    // a hex push
  int tmp_ids[2];
  result = mbImpl->tag_get_data(globalId_tag(), hexes, 2, tmp_ids);
  if (MB_SUCCESS != result) return result;
  

  result = mbImpl->delete_entities(hexes, 2);
  if (MB_SUCCESS != result) return result;
  result = mbImpl->delete_entities(&quads[1], 1);
  if (MB_SUCCESS != result) return result;
  for (int i = 0; i < 4; i++) {
    hconnect[i] = connects[3][i];
    hconnect[4+i] = connects[2][i];
  }
  result = mbImpl->create_element(MBHEX, hconnect, 8, hexes[0]);
  if (MB_SUCCESS != result) return result;

  for (int i = 0; i < 4; i++) {
    hconnect[i] = connects[0][i];
    hconnect[4+i] = connects[3][i];
  }
  result = mbImpl->create_element(MBHEX, hconnect, 8, hexes[1]);
  if (MB_SUCCESS != result) return result;

    // check for and fix any explicit adjacencies on either end quad
  if (mtu.equivalent_entities(quads[0]))
      mbImpl->add_adjacencies(quads[0], &hexes[1], 1, false);
  if (mtu.equivalent_entities(quads[2]))
      mbImpl->add_adjacencies(quads[2], &hexes[0], 1, false);

    // re-set the global ids for the hexes to what they were
  result = mbImpl->tag_set_data(globalId_tag(), hexes, 2, tmp_ids);
  if (MB_SUCCESS != result) return result;
  
  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();

    // now update the dual
  Range tmph;
  result = mtu.get_bridge_adjacencies(hexes[0], 0, 3, tmph);
  if (MB_SUCCESS != result) return result;
  result = mtu.get_bridge_adjacencies(hexes[1], 0, 3, tmph);
  if (MB_SUCCESS != result) return result;
  tmph.insert(hexes[1]);
  result = construct_hex_dual(tmph);
  if (MB_SUCCESS != result) return result;
  
  return result;
}

ErrorCode DualTool:: fs_get_quad_loops(EntityHandle *hexes, 
                                         std::vector<EntityHandle> *connects, 
                                         std::vector<EntityHandle> *side_quads) 
{
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 2; j++) {
      Range adj_ents, dum_quads;
      adj_ents.insert(hexes[j]);
      adj_ents.insert(connects[j][i]);
      adj_ents.insert(connects[j][(i+1)%4]);
      adj_ents.insert(connects[j+1][i]);
      adj_ents.insert(connects[j+1][(i+1)%4]);
    
      ErrorCode result = mbImpl->get_adjacencies(adj_ents, 2, false, dum_quads);
      if (MB_SUCCESS != result) return result;
      assert(1 == dum_quads.size());
      side_quads[j].push_back(*dum_quads.begin());
    }
  }
  
  return MB_SUCCESS;
}
  
ErrorCode DualTool::fs_check_quad_sense(EntityHandle hex0,
                                          EntityHandle quad0,
                                          std::vector<EntityHandle> *connects) 
{
    // check sense of 0th quad wrt hex; since sense is out of element,
    // switch if quad is NOT reversed wrt hex
  int dum1, dum2, sense = 0;
  ErrorCode result = mbImpl->side_number(hex0, quad0, dum1, sense, dum2);
  if (MB_SUCCESS != result) return result;
  assert(0 != sense);
  if (1 == sense) {
      // just switch sense of this one; others will get switched next
    EntityHandle dum = connects[0][0];
    connects[0][0] = connects[0][2];
    connects[0][2] = dum;
  }
  
    // check sense of 1st, 2nd quads, rotate if necessary to align connect arrays
  int index0 = -1, index2 = -1, sense0 = 0, sense2 = 0;
  MeshTopoUtil mtu(mbImpl);
  for (int i = 0; i < 4; i++) {
    if (0 != mtu.common_entity(connects[0][0], connects[1][i], 1)) {
      index0 = i;
      if (0 != mtu.common_entity(connects[0][1], connects[1][(i+1)%4], 1))
        sense0 = 1;
      else if (0 != mtu.common_entity(connects[0][1], connects[1][(i+4-1)%4], 1))
        sense0 = -1;
      break;
    }
  }

  assert(index0 != -1 && sense0 != 0);
  
  if (sense0 == -1) {
    EntityHandle dumh = connects[1][0];
    connects[1][0] = connects[1][2];
    connects[1][2] = dumh;
    if (index0%2 == 0)
      index0 = (index0+2)%4;
  }

  if (index0 != 0) {
    std::vector<EntityHandle> tmpc;
    for (int i = 0; i < 4; i++)
      tmpc.push_back(connects[1][(index0+i)%4]);
    connects[1].swap(tmpc);
  }
    
  for (int i = 0; i < 4; i++) {
    if (0 != mtu.common_entity(connects[1][0], connects[2][i], 1)) {
      index2 = i;
      if (0 != mtu.common_entity(connects[1][1], connects[2][(i+1)%4], 1))
        sense2 = 1;
      else if (0 != mtu.common_entity(connects[1][1], connects[2][(i+4-1)%4], 1))
        sense2 = -1;
      break;
    }
  }

  assert(index2 != -1 && sense2 != 0);

  if (sense2 == -1) {
    EntityHandle dumh = connects[2][0];
    connects[2][0] = connects[2][2];
    connects[2][2] = dumh;
    if (index2%2 == 0)
      index2 = (index2+2)%4;
  }

  if (index2 != 0) {
    std::vector<EntityHandle> tmpc;
    for (int i = 0; i < 4; i++)
      tmpc.push_back(connects[2][(index2+i)%4]);
    connects[2].swap(tmpc);
  }
    
  return MB_SUCCESS;
}

  //! effect reverse face shrink operation
ErrorCode DualTool::rev_face_shrink(EntityHandle odedge) 
{
  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();

    // some preliminary checking
  if (mbImpl->type_from_handle(odedge) != MBEDGE) return MB_TYPE_OUT_OF_RANGE;
  
  std::cout << "-FS("; print_cell(odedge); std::cout << ")" << std::endl;

  EntityHandle quads[4], hexes[2];
  std::vector<EntityHandle> connects[4], side_quads[2];

    // get three quads (shared quad & 2 end quads), hexes, and quad
    // connects
  ErrorCode result = fs_get_quads(odedge, quads, hexes, connects);
  if (MB_SUCCESS != result) return result;
  
    // adjust sense & rotation so they're aligned, together & wrt first
    // hex
  result = fs_check_quad_sense(hexes[0], quads[0], connects);
  if (MB_SUCCESS != result) return result;

  result = fsr_get_fourth_quad(connects, side_quads);
  if (MB_SUCCESS != result) {
    std::cout << "Can't do -FS here, two hexes must be adjacent to ring of 4 hexes." 
              << std::endl;
    return result;
  }

  Range adj_ents, outer_hexes, all_adjs;

    // first get the entities connected to interior 4 verts
  for (int i = 1; i <= 3; i++) {
    result = mbImpl->get_adjacencies(&connects[1][0], 4, i, false, adj_ents,
                                     Interface::UNION);
    if (MB_SUCCESS != result) return result;
  }
  
    // next get all entities adjacent to those; these will have their dual
    // entities deleted
  for (int i = 0; i < 3; i++) {
    result = mbImpl->get_adjacencies(adj_ents, i, false, all_adjs,
                                     Interface::UNION);
    if (MB_SUCCESS != result) return result;
  }

    // get the dual entities and delete them
  Range dual_ents, dual_hps;
  for (Range::iterator rit = all_adjs.begin(); rit != all_adjs.end(); ++rit) {
    EntityHandle this_ent = get_dual_entity(*rit);
    dual_ents.insert(this_ent);
  }
    
    // before deleting dual, grab the 1- and 2-cells
  for (Range::iterator rit = dual_ents.begin(); rit != dual_ents.end(); ++rit) {
    int dim = mbImpl->dimension_from_handle(*rit);
    if (1 == dim || 2 == dim) dual_hps.insert(get_dual_hyperplane(*rit));
  }
  
  result = delete_dual_entities(dual_ents);
  if (MB_SUCCESS != result) return result;
  
    // after deleting cells, check for empty chords & sheets, and delete those too
  for (Range::iterator rit = dual_hps.begin(); rit != dual_hps.end(); ++rit) {
    Range tmp_ents;
    result = mbImpl->get_entities_by_handle(*rit, tmp_ents);
    if (MB_SUCCESS != result) return result;
    if (tmp_ents.empty()) {
      result = mbImpl->delete_entities(&(*rit), 1);
      if (MB_SUCCESS != result) return result;
    }
  }
    
    // before re-connecting two hexes, check for existing quad on 4th quad vertices; 
    // if there is a quad there, need to add explicit adjs to any adj hexes, since
    // by definition there'll be another quad on those vertices
  bool need_explicit = false;
  Range adj_quads;
  result = mbImpl->get_adjacencies(&connects[3][0], 4, 2, false, adj_quads);
  if (MB_MULTIPLE_ENTITIES_FOUND == result || !adj_quads.empty()) {
      // there's already a quad for these 4 vertices; by definition,
      // we'll be creating equivalent entities, so that original quad
      // needs explicit adj's to its bounding elements
    need_explicit = true;
    for (Range::iterator rit = adj_quads.begin(); rit != adj_quads.end(); 
         ++rit) {
      Range adj_hexes;
      result = mbImpl->get_adjacencies(&(*rit), 1, 3, false, adj_hexes); RR;
      result = mbImpl->add_adjacencies(*rit, adj_hexes, false); RR;
    }
  }
    
    // re-connect the two hexes
  std::vector<EntityHandle> new_connect;
  std::copy(connects[3].begin(), connects[3].end(), std::back_inserter(new_connect));
  std::copy(connects[2].begin(), connects[2].end(), std::back_inserter(new_connect));
  result = mbImpl->set_connectivity(hexes[0], &new_connect[0], 8);
  if (MB_SUCCESS != result) return result;

  new_connect.clear();
  std::copy(connects[0].begin(), connects[0].end(), std::back_inserter(new_connect));
  std::copy(connects[3].begin(), connects[3].end(), std::back_inserter(new_connect));
  result = mbImpl->set_connectivity(hexes[1], &new_connect[0], 8);
  if (MB_SUCCESS != result) return result;

    // test for equiv entities from the side quads, and make explicit 
    // adjacencies if there are any
  MeshTopoUtil mtu(mbImpl);
  for (int j = 0; j < 2; j++) {
    for (int i = 0; i < 4; i++) {
      if (mtu.equivalent_entities(side_quads[j][i])) {
        result = mbImpl->add_adjacencies(side_quads[j][i], &hexes[j], 1, false);
        if (MB_SUCCESS != result) return result;
      }
    }
  }
  
    // remove hexes we want to keep
  adj_ents.erase(hexes[0]);
  adj_ents.erase(hexes[1]);
  
    // delete the other interior entities
  result = mbImpl->delete_entities(adj_ents);
  if (MB_SUCCESS != result) return result;

  EntityHandle new_quad;
  result = mbImpl->create_element(MBQUAD, &connects[3][0], 4, new_quad); RR;
  if (need_explicit) {
    result = mbImpl->add_adjacencies(new_quad, hexes, 2, false); RR;
  }
  
  if (debug_ap) ((Core*)mbImpl)->check_adjacencies();

    // now update the dual
  result = construct_hex_dual(hexes, 2);
  if (MB_SUCCESS != result) return result;

  return MB_SUCCESS;
}

ErrorCode DualTool::fsr_get_fourth_quad(std::vector<EntityHandle> *connects,
                                          std::vector<EntityHandle> *side_quads) 
{
    // given the first three quad connectivities in ordered vectors, get the fourth,
    // where the fourth is really the 4 vertices originally shared by the 2 hexes
    // before the face shrink on them

    // vertex on 4th quad is in quad adj to other 3 verts
  for (int i = 0; i < 4; i++) {
    Range start_verts, tmp_verts, quads;
    for (int j = 0; j < 3; j++) start_verts.insert(connects[j][i]);
    ErrorCode result = mbImpl->get_adjacencies(start_verts, 2, false, quads);
    if (MB_SUCCESS != result) return result;
    assert(quads.size() == 1);
    result = mbImpl->get_adjacencies(&(*quads.begin()), 1, 0, false, tmp_verts); RR;
    tmp_verts = subtract( tmp_verts, start_verts);
    assert(1 == tmp_verts.size());
    connects[3].push_back(*tmp_verts.begin());
  }
    
    // now get the side quads
  for (int i = 0; i < 4; i++) {
    Range dum_ents, hexes;
    dum_ents.insert(connects[1][i]);
    dum_ents.insert(connects[1][(i+1)%4]);
    dum_ents.insert(connects[3][i]);
    ErrorCode result = mbImpl->get_adjacencies(dum_ents, 3, false, hexes);
    if (MB_SUCCESS != result) return result;
    assert(1 == hexes.size());
    
    hexes.insert(connects[0][i]);
    hexes.insert(connects[0][(i+1)%4]);
    hexes.insert(connects[3][i]);
    hexes.insert(connects[3][(i+1)%4]);
    dum_ents.clear();
    result = mbImpl->get_adjacencies(hexes, 2, false, dum_ents);
    if (MB_SUCCESS != result) return result;
    assert(dum_ents.size() == 1);
    side_quads[0].push_back(*dum_ents.begin());

    hexes.erase(connects[0][i]);
    hexes.erase(connects[0][(i+1)%4]);
    hexes.insert(connects[2][i]);
    hexes.insert(connects[2][(i+1)%4]);
    dum_ents.clear();
    result = mbImpl->get_adjacencies(hexes, 2, false, dum_ents);
    if (MB_SUCCESS != result) return result;
    side_quads[1].push_back(*dum_ents.begin());
  }

  return MB_SUCCESS;
}

ErrorCode DualTool::fs_get_quads(EntityHandle odedge, 
                                   EntityHandle *quads,
                                   EntityHandle *hexes,
                                   std::vector<EntityHandle> *connects) 
{
    // need to get the three quads along the chord
  EntityHandle chord = get_dual_hyperplane(odedge);
  if (0 == chord) return MB_FAILURE;
  
  std::vector<EntityHandle> edges;
  ErrorCode result = mbImpl->get_entities_by_handle(chord, edges);
  if (MB_FAILURE == result) return result;
  
  std::vector<EntityHandle>::iterator vit = std::find(edges.begin(), edges.end(), odedge);
    // shouldn't be first or last edge on chord
  if (vit == edges.end() || *edges.begin() == *vit || *edges.rbegin() == *vit)
    return MB_FAILURE;
  
    // get quads/connectivity for first 3 quads
  quads[0] = get_dual_entity(*(vit-1));
  quads[1] = get_dual_entity(*vit);
  quads[2] = get_dual_entity(*(vit+1));
  for (int i = 0; i < 3; i++) {
    result = mbImpl->get_connectivity(&quads[i], 1, connects[i], true);
    if (MB_SUCCESS != result) return result;
  }
  
  Range tmph;
  result = mbImpl->get_adjacencies(quads, 2, 3, false, tmph);
  if (MB_SUCCESS != result) return result;
  assert(tmph.size() == 1);
  hexes[0] = *tmph.begin();
  
  tmph.clear();
  result = mbImpl->get_adjacencies(&quads[1], 2, 3, false, tmph);
  if (MB_SUCCESS != result) return result;
  assert(tmph.size() == 1);
  hexes[1] = *tmph.begin();
  
  return MB_SUCCESS;
}

ErrorCode DualTool::delete_whole_dual() 
{
    // delete dual hyperplanes
  Range dual_surfs, dual_curves;
  ErrorCode result = this->get_dual_hyperplanes(mbImpl, 2, dual_surfs); RR;
  result = mbImpl->delete_entities(dual_surfs); RR;
  result = this->get_dual_hyperplanes(mbImpl, 1, dual_curves); RR;
  result = mbImpl->delete_entities(dual_curves); RR;
  
    // gather up all dual entities
  Range dual_ents;
  result = mbImpl->get_entities_by_type_and_tag(0, MBVERTEX, &isDualCellTag, NULL, 1, dual_ents,
                                                Interface::UNION); RR;
  result = mbImpl->get_entities_by_type_and_tag(0, MBEDGE, &isDualCellTag, NULL, 1, dual_ents,
                                                Interface::UNION); RR;
  result = mbImpl->get_entities_by_type_and_tag(0, MBPOLYGON, &isDualCellTag, NULL, 1, dual_ents,
                                                Interface::UNION); RR;
  result = mbImpl->get_entities_by_type_and_tag(0, MBPOLYHEDRON, &isDualCellTag, NULL, 1, dual_ents,
                                                Interface::UNION); RR;

    // delete them, in reverse order of dimension
  ErrorCode tmp_result;
  for (Range::reverse_iterator rit = dual_ents.rbegin(); rit != dual_ents.rend(); ++rit) {
    tmp_result = mbImpl->delete_entities(&(*rit), 1);
    if (MB_SUCCESS != tmp_result) result = tmp_result;
  }
  RR;
  
    // delete dual-related tags
  if (0 != dualSurfaceTag) {
    tmp_result = mbImpl->tag_delete(dualSurfaceTag); 
    if (MB_SUCCESS != tmp_result && MB_TAG_NOT_FOUND != tmp_result) result = tmp_result;
  }
  if (0 != dualCurveTag) {
    tmp_result = mbImpl->tag_delete(dualCurveTag); 
    if (MB_SUCCESS != tmp_result && MB_TAG_NOT_FOUND != tmp_result) result = tmp_result;
  }
  if (0 != dualEntityTag) {
    tmp_result = mbImpl->tag_delete(dualEntityTag); 
    if (MB_SUCCESS != tmp_result && MB_TAG_NOT_FOUND != tmp_result) result = tmp_result;
  }
  if (0 != extraDualEntityTag) {
    tmp_result = mbImpl->tag_delete(extraDualEntityTag); 
    if (MB_SUCCESS != tmp_result && MB_TAG_NOT_FOUND != tmp_result) result = tmp_result;
  }
  if (0 != dualGraphicsPointTag) {
    tmp_result = mbImpl->tag_delete(dualGraphicsPointTag); 
    if (MB_SUCCESS != tmp_result && MB_TAG_NOT_FOUND != tmp_result) result = tmp_result;
  }

  return MB_SUCCESS;
}

ErrorCode DualTool::check_dual_adjs() 
{
    // check primal-dual correspondence

    // get the primal entities
  Range pents[4];
  ErrorCode result = mbImpl->get_entities_by_type(0, MBHEX, pents[3]); RR;
  for (int i = 2; i >= 0; i--) {
    result = mbImpl->get_adjacencies(pents[3], 2, false, pents[2], 
                                     Interface::UNION); RR;
  }
  
    // for each primal entity of dimension pd
#define PRENT(ent) CN::EntityTypeName(TYPE_FROM_HANDLE(ent)) << " " \
        << ID_FROM_HANDLE(ent) 
  ErrorCode overall_result = MB_SUCCESS;
  for (int pd = 1; pd <= 3; pd++) {
    for (Range::iterator prit = pents[pd].begin(); prit != pents[pd].end(); ++prit) {
        // get corresponding dual entity of dimension dd = 3-pd
      EntityHandle dual_ent = get_dual_entity(*prit);
      if (0 == dual_ent) 
        std::cerr << "Problem getting dual entity for " << PRENT(*prit) << std::endl;
      
        // for each sub dimension sd = 0..pd-1
      for (int sd = 0; sd < pd; sd++) {
        Range R1, R2, R3;
          //   R1 = entities bounding primal entity of dim sd
        result = mbImpl->get_adjacencies(&(*prit), 1, sd, false, R1); RR;
        
          //   R2 = entities bounded by dual entity, of dim 3-sd
        result = mbImpl->get_adjacencies(&dual_ent, 1, 3-sd, false, R2); RR;


        if (R1.size() != R2.size()) {
          std::cerr << PRENT(*prit) << ": number of adj ents in "
                    << "primal/dual don't agree for dimension " << sd << "." << std::endl;
          overall_result = MB_FAILURE;
        }
        
          // for each entity in R1, get its dual and look for it in R2
        for (Range::iterator r1it = R1.begin(); r1it != R1.end(); ++r1it) {
          EntityHandle tmp_dual = get_dual_entity(*r1it);
          if (R2.find(tmp_dual) == R2.end()) {
            std::cerr << PRENT(*prit) << ": adj entity " << PRENT(*r1it)
                      << " isn't adjacent in dual." << std::endl;
            overall_result = MB_FAILURE;
          }
        }
          // ditto for R2
        for (Range::iterator r2it = R2.begin(); r2it != R2.end(); ++r2it) {
          EntityHandle tmp_prim = get_dual_entity(*r2it);
          if (R1.find(tmp_prim) == R1.end()) {
            std::cerr << PRENT(*prit) << ": adj entity " << PRENT(*r2it)
                      << " isn't adjacent in primal." << std::endl;
            overall_result = MB_FAILURE;
          }
        }
      }
    }
  }

  return overall_result;
}
  
} // namespace moab
