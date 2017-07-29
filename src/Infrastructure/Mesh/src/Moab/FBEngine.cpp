#include <iostream>
#include <map>

#include "moab/FBEngine.hpp"
#include "moab/Interface.hpp"
#include "moab/GeomTopoTool.hpp"
#include "moab/OrientedBoxTreeTool.hpp"

#include <stdlib.h>
#include <cstring>
#include <map>
#include <set>
#include <queue>
#include <algorithm>
#include "assert.h"

#include "SmoothCurve.hpp"
#include "SmoothFace.hpp"

// this is just to replace MBI with moab interface, which is _mbImpl in this class
#define MBI _mbImpl
#define MBERRORR(rval, STR) { if (MB_SUCCESS != rval) { \
      std::cout<<STR<<std::endl;       \
      return rval; } }

namespace moab {

// some tolerances for ray tracing and geometry intersections
// these are involved in ray tracing, at least

unsigned min_tolerace_intersections = 1000;
double tolerance = 0.01; // TODO: how is this used ????
double tolerance_segment = 0.01; // for segments intersection, points collapse, area coordinates for triangles
// it should be a relative, not an absolute value
// as this splitting operation can create small edges, it should be relatively high
// or, it should be coordinated with the decimation errors
// we really just want to preserve the integrity of the mesh, we should avoid creating small edges or angles
const bool Debug_surf_eval = false;
bool debug_splits = false;

// will compute intersection between a segment and slice of a plane
// output is the intersection point
bool intersect_segment_and_plane_slice(CartVect & from, CartVect & to,
    CartVect & p1, CartVect & p2, CartVect & , CartVect & normPlane,
    CartVect & intx_point, double & parPos)
{
  //
  // plane eq is normPlane % r + d = 0, or normPlane % r - normPlane%p1 = 0
  double dd = -normPlane % p1;
  double valFrom = normPlane % from + dd;
  double valTo = normPlane % to + dd;

  if (fabs(valFrom) < tolerance_segment) {
    intx_point = from;
    parPos = 0.;
    double proj1 = (intx_point - p1) % (p2 - p1);
    double proj2 = (intx_point - p2) % (p1 - p2);
    if (proj1 <= -tolerance_segment || proj2 <= -tolerance_segment)
      return false;
    if (debug_splits)
      std::cout << "intx : " << intx_point << "\n";
    return true;
  }
  if (fabs(valTo) < tolerance_segment) {
    intx_point = to;
    parPos = 1;
    double proj1 = (intx_point - p1) % (p2 - p1);
    double proj2 = (intx_point - p2) % (p1 - p2);
    if (proj1 <= -tolerance_segment || proj2 <= -tolerance_segment)
      return false;
    if (debug_splits)
      std::cout << "intx : " << intx_point << "\n";
    return true;
  }
  if (valFrom * valTo > 0)
    return false; // no intersection, although it could be very close
  // else, it could intersect the plane; check for the slice too.
  parPos = valFrom / (valFrom - valTo);// this is 0 for valFrom 0, 1 for valTo 0
  intx_point = from + (to - from) * parPos;
  // now check if the intx_point is indeed between p1 and p2 in the slice.
  double proj1 = (intx_point - p1) % (p2 - p1);
  double proj2 = (intx_point - p2) % (p1 - p2);
  if (proj1 <= -tolerance_segment || proj2 <= -tolerance_segment)
    return false;

  if (debug_splits)
    std::cout << "intx : " << intx_point << "\n";
  return true;
}

ErrorCode area_coordinates(Interface * mbi, EntityHandle tri, CartVect & pnt,
    double * area_coord, EntityHandle & boundary_handle)
{

  int nnodes;
  const EntityHandle * conn3;
  ErrorCode rval = mbi->get_connectivity(tri, conn3, nnodes);
  MBERRORR(rval, "Failed to get connectivity");
  assert(3 == nnodes);
  CartVect P[3];
  rval = mbi->get_coords(conn3, nnodes, (double*) &P[0]);
  MBERRORR(rval, "Failed to get coordinates");

  CartVect r0(P[0] - pnt);
  CartVect r1(P[1] - pnt);
  CartVect r2(P[2] - pnt);
  if (debug_splits)
  {
    std::cout << " nodes:" << conn3[0] << " "<<  conn3[1] << " " << conn3[2] <<"\n";
    std::cout << " distances: " << r0.length() << " " << r1.length() << " " << r2.length() << "\n";
  }
  if (r0.length() < tolerance_segment) {
    area_coord[0] = 1.;
    area_coord[1] = 0.;
    area_coord[2] = 0.;
    boundary_handle = conn3[0];
    return MB_SUCCESS;
  }
  if (r1.length() < tolerance_segment) {
    area_coord[0] = 0.;
    area_coord[1] = 1.;
    area_coord[2] = 0.;
    boundary_handle = conn3[1];
    return MB_SUCCESS;
  }
  if (r2.length() < tolerance_segment) {
    area_coord[0] = 0.;
    area_coord[1] = 0.;
    area_coord[2] = 1.;
    boundary_handle = conn3[2];
    return MB_SUCCESS;
  }

  CartVect v1(P[1] - P[0]);
  CartVect v2(P[2] - P[0]);

  double areaDouble = (v1 * v2).length();// the same for CartVect
  if (areaDouble < tolerance_segment * tolerance_segment) {
    MBERRORR(MB_FAILURE, "area of triangle too small");
  }
  area_coord[0] = (r1 * r2).length() / areaDouble;
  area_coord[1] = (r2 * r0).length() / areaDouble;
  area_coord[2] = (r0 * r1).length() / areaDouble;

  if (fabs(area_coord[0] + area_coord[1] + area_coord[2] - 1)
      > tolerance_segment) {
    MBERRORR(MB_FAILURE, "point outside triangle");
  }
  // the tolerance is used here for area coordinates (0 to 1), and in other
  // parts it is used as an absolute distance; pretty inconsistent.
  bool side0 = (area_coord[0] < tolerance_segment);
  bool side1 = (area_coord[1] < tolerance_segment);
  bool side2 = (area_coord[2] < tolerance_segment);
  if (!side0 && !side1 && !side2) 
    return MB_SUCCESS; // interior point
  // now, find out what boundary is in question
  // first, get all edges, in order
  std::vector<EntityHandle> edges;
  EntityHandle nn2[2];
  for (int i = 0; i < 3; i++) {
    nn2[0] = conn3[(i + 1) % 3];
    nn2[1] = conn3[(i + 2) % 3];
    std::vector<EntityHandle> adjacent;
    rval = mbi->get_adjacencies(nn2, 2, 1, false, adjacent,
        Interface::INTERSECT);
    MBERRORR(rval, "Failed to get edges");
    if (adjacent.size() != 1)
      MBERRORR(MB_FAILURE, "Failed to get adjacent edges");
    // should be only one edge here
    edges.push_back(adjacent[0]);
  }

  if (side0)
    boundary_handle = edges[0];
  if (side1)
    boundary_handle = edges[1];
  if (side2)
    boundary_handle = edges[2];

  return MB_SUCCESS;
}

FBEngine::FBEngine(Interface *impl, GeomTopoTool * topoTool, const bool smooth) :
  _mbImpl(impl), _my_geomTopoTool(topoTool), _t_created(false),
      _smooth(smooth), _initialized(false), _smthFace(NULL), _smthCurve(NULL)
{
  if (!_my_geomTopoTool) {
    _my_geomTopoTool = new GeomTopoTool(_mbImpl);
    _t_created = true;
  }
  // should this be part of the constructor or not?
  //Init();
}
FBEngine::~FBEngine()
{
  clean();
  _smooth = false;
}

void FBEngine::clean()
{
  if (_smooth) {
    _faces.clear();
    _edges.clear();
    int size1 = _my_gsets[1].size();
    int i = 0;
    for (i = 0; i < size1; i++)
      delete _smthCurve[i];
    delete[] _smthCurve;
    _smthCurve = NULL;
    size1 = _my_gsets[2].size();
    for (i = 0; i < size1; i++)
      delete _smthFace[i];
    delete[] _smthFace;
    _smthFace = NULL;
    //_smooth = false;
  }

  for (int j = 0; j < 5; j++)
    _my_gsets[j].clear();
  if (_t_created)
    delete _my_geomTopoTool;
  _my_geomTopoTool = NULL;
  _t_created = false;
}

ErrorCode FBEngine::Init()
{
  if (!_initialized) {
    if (!_my_geomTopoTool)
      return MB_FAILURE;

    ErrorCode rval = _my_geomTopoTool->find_geomsets(_my_gsets);
    assert(rval == MB_SUCCESS);
    if (MB_SUCCESS != rval){return rval;}
    

    rval = split_quads();
    assert (rval == MB_SUCCESS);

    rval = _my_geomTopoTool->construct_obb_trees();
    assert(rval == MB_SUCCESS);

    if (_smooth)
      rval = initializeSmoothing();
    assert(rval == MB_SUCCESS);

    _initialized = true;
  }
  return MB_SUCCESS;
}
ErrorCode FBEngine::initializeSmoothing()
{
  //
  /*ErrorCode rval = Init();
   MBERRORR(rval, "failed initialize");*/
  // first of all, we need to retrieve all the surfaces from the (root) set
  // in icesheet_test we use iGeom, but maybe that is a stretch
  // get directly the sets with geom dim 2, and from there create the SmoothFace
  Tag geom_tag, gid_tag;
  ErrorCode rval = MBI->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geom_tag);
  MBERRORR(rval, "can't get geom tag");
  rval = MBI->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gid_tag);
  MBERRORR(rval, "can't get id tag");
  int numSurfaces = _my_gsets[2].size();
  //SmoothFace ** smthFace = new SmoothFace *[numSurfaces];
  _smthFace = new SmoothFace *[numSurfaces];

  // there should also be a map from surfaces to evaluators
  //std::map<MBEntityHandle, SmoothFace*> mapSurfaces;

  int i = 0;
  Range::iterator it;
  for (it = _my_gsets[2].begin(); it != _my_gsets[2].end(); ++it, i++) {
    EntityHandle face = *it;
    _smthFace[i] = new SmoothFace(MBI, face, _my_geomTopoTool);// geom topo tool will be used for searching,
    // among other things; also for senses in edge sets...
    _faces[face] = _smthFace[i];
  }

  int numCurves = _my_gsets[1].size();//csets.size();
  //SmoothCurve ** smthCurve = new SmoothCurve *[numCurves];
  _smthCurve = new SmoothCurve *[numCurves];
  // there should also be a map from surfaces to evaluators
  //std::map<MBEntityHandle, SmoothCurve*> mapCurves;

  i = 0;
  for (it = _my_gsets[1].begin(); it != _my_gsets[1].end(); ++it, i++) {
    EntityHandle curve = *it;
    _smthCurve[i] = new SmoothCurve(MBI, curve, _my_geomTopoTool);
    _edges[curve] = _smthCurve[i];
  }

  for (i = 0; i < numSurfaces; i++) {
    _smthFace[i]->init_gradient();// this will also retrieve the triangles in each surface
    _smthFace[i]->compute_tangents_for_each_edge();// this one will consider all edges internal, so the
    // tangents are all in the direction of the edge; a little bit of waste, as we store
    // one tangent for each edge node , even though they are equal here...
    // no loops are considered
  }

  // this will be used to mark boundary edges, so for them the control points are computed earlier
  unsigned char value = 0; // default value is "not used"=0 for the tag
  // unsigned char def_data_bit = 1;// valid by default
  // rval = mb->tag_create("valid", 1, MB_TAG_BIT, validTag, &def_data_bit);
  Tag markTag;
  rval = MBI->tag_get_handle("MARKER", 1, MB_TYPE_BIT, markTag, 
                             MB_TAG_EXCL|MB_TAG_BIT, &value); // default value : 0 = not computed yet
  // each feature edge will need to have a way to retrieve at every moment the surfaces it belongs to
  // from edge sets, using the sense tag, we can get faces, and from each face, using the map, we can get
  // the SmoothFace (surface evaluator), that has everything, including the normals!!!
  assert(rval==MB_SUCCESS);

  // create the tag also for control points on the edges
  double defCtrlPoints[9] = { 0., 0., 0., 0., 0., 0., 0., 0., 0. };
  Tag edgeCtrlTag;
  rval = MBI->tag_get_handle("CONTROLEDGE", 9, MB_TYPE_DOUBLE, edgeCtrlTag, 
                             MB_TAG_DENSE|MB_TAG_CREAT, &defCtrlPoints );
  assert(rval == MB_SUCCESS);

  Tag facetCtrlTag;
  double defControls[18] = { 0. };
  rval = MBI->tag_get_handle("CONTROLFACE", 18, MB_TYPE_DOUBLE, 
                             facetCtrlTag, MB_TAG_CREAT|MB_TAG_DENSE,
                             &defControls);
  assert(rval == MB_SUCCESS);

  Tag facetEdgeCtrlTag;
  double defControls2[27] = { 0. }; // corresponding to 9 control points on edges, in order from edge 0, 1, 2 ( 1-2, 2-0, 0-1 )
  rval = MBI->tag_get_handle("CONTROLEDGEFACE", 27, MB_TYPE_DOUBLE,
                              facetEdgeCtrlTag, MB_TAG_CREAT|MB_TAG_DENSE,
                              &defControls2);
  assert(rval == MB_SUCCESS);
  // if the
  double min_dot = -1.0; // depends on _angle, but now we ignore it, for the time being
  for (i = 0; i < numCurves; i++) {
    _smthCurve[i]->compute_tangents_for_each_edge();// do we need surfaces now? or just the chains?
    // the computed edges will be marked accordingly; later one, only internal edges to surfaces are left
    _smthCurve[i]->compute_control_points_on_boundary_edges(min_dot, _faces,
        edgeCtrlTag, markTag);
  }

  // when done with boundary edges, compute the control points on all edges in the surfaces

  for (i = 0; i < numSurfaces; i++) {
    // also pass the tags for
    _smthFace[i]->compute_control_points_on_edges(min_dot, edgeCtrlTag, markTag);
  }

  // now we should be able to compute the control points for the facets

  for (i = 0; i < numSurfaces; i++) {
    // also pass the tags for edge and facet control points
    _smthFace[i]->compute_internal_control_points_on_facets(min_dot,
        facetCtrlTag, facetEdgeCtrlTag);
  }
  // we will need to compute the tangents for all edges in the model
  // they will be needed for control points for each edge
  // the boundary edges and the feature edges are more complicated
  // the boundary edges need to consider their loops, but feature edges need to consider loops and the normals
  // on each connected surface

  // some control points
  if (Debug_surf_eval)
    for (i = 0; i < numSurfaces; i++)
      _smthFace[i]->DumpModelControlPoints();

  return MB_SUCCESS;
}

// clean up the smooth tags data if created, so the files will be smaller
// if saved
// also, recompute the tags if topology is modified
void FBEngine::delete_smooth_tags()
{
  // get all tags from database that are created for smooth data, and
  // delete them; it will delete all data associated with them
  // first tags from faces, edges:
  std::vector<Tag> smoothTags;
  int size1 = (int)_my_gsets[2].size();

  for (int i=0; i<size1; i++)
  {
    // these 2 will append gradient tag and plane tag
    _smthFace[i]->append_smooth_tags(smoothTags);
  }
  // then , get other tags:
  // "TANGENTS", "MARKER", "CONTROLEDGE", "CONTROLFACE", "CONTROLEDGEFACE"
  Tag tag_handle;
  ErrorCode rval = _mbImpl->tag_get_handle( "TANGENTS", 6, MB_TYPE_DOUBLE, tag_handle );
  if (rval != MB_TAG_NOT_FOUND)
    smoothTags.push_back(tag_handle);

  rval = _mbImpl->tag_get_handle( "MARKER", 1, MB_TYPE_BIT, tag_handle );
  if (rval != MB_TAG_NOT_FOUND)
    smoothTags.push_back(tag_handle);

  rval = _mbImpl->tag_get_handle( "CONTROLEDGE", 9, MB_TYPE_DOUBLE, tag_handle );
  if (rval != MB_TAG_NOT_FOUND)
    smoothTags.push_back(tag_handle);

  rval = _mbImpl->tag_get_handle( "CONTROLFACE", 18, MB_TYPE_DOUBLE, tag_handle );
  if (rval != MB_TAG_NOT_FOUND)
    smoothTags.push_back(tag_handle);

  rval = _mbImpl->tag_get_handle( "CONTROLEDGEFACE", 27, MB_TYPE_DOUBLE, tag_handle );
  if (rval != MB_TAG_NOT_FOUND)
    smoothTags.push_back(tag_handle);

  // a lot of tags, delete them
  for (unsigned int k = 0; k<smoothTags.size(); k++ )
  {
    // could be a lot of data
    _mbImpl->tag_delete(smoothTags[k]);
  }
}
/*
#define COPY_RANGE(r, vec) {                      \
    EntityHandle *tmp_ptr = reinterpret_cast<EntityHandle*>(vec);	\
    std::copy(r.begin(), r.end(), tmp_ptr);}
*/

/*static inline void
 ProcessError(const char* desc);*/

ErrorCode FBEngine::getRootSet(EntityHandle * root_set)
{
  *root_set =  _my_geomTopoTool-> get_root_model_set();
  return MB_SUCCESS;
}

ErrorCode FBEngine::getNumEntSets(EntityHandle set, int num_hops,
    int * all_sets)
{
  ErrorCode rval = MBI->num_contained_meshsets(set, all_sets, num_hops + 1);
  return rval;
}

ErrorCode FBEngine::createEntSet(int isList, EntityHandle * pSet)
{
  ErrorCode rval;

  if (isList)
    rval = MBI->create_meshset(MESHSET_ORDERED, *pSet);
  else
    rval = MBI->create_meshset(MESHSET_SET, *pSet);

  return rval;
}

ErrorCode FBEngine::getEntities(EntityHandle set_handle, int entity_type,
    Range & gentities)
{
  int i;
  if (0 > entity_type || 4 < entity_type) {
    return MB_FAILURE;
  } else if (entity_type < 4) {// 4 means all entities
    gentities = _my_geomTopoTool->geoRanges()[entity_type];// all from root set!
  } else {
    gentities.clear();
    for (i = 0; i < 4; i++) {
      gentities.merge(_my_geomTopoTool->geoRanges()[i]);
    }
  }
  Range sets;
  // see now if they are in the set passed as input or not
  ErrorCode rval = MBI->get_entities_by_type(set_handle, MBENTITYSET, sets);
  MBERRORR(rval, "can't get sets in the initial set");
  gentities = intersect(gentities, sets);

  return MB_SUCCESS;
}

ErrorCode FBEngine::addEntArrToSet(Range entities, EntityHandle set)
{
  return MBI->add_entities(set, entities);
}

ErrorCode FBEngine::addEntSet(EntityHandle entity_set_to_add,
    EntityHandle entity_set_handle)
{
  return MBI->add_entities(entity_set_handle, &entity_set_to_add, 1);
}

ErrorCode FBEngine::getNumOfType(EntityHandle set, int ent_type, int * pNum)
{
  if (0 > ent_type || 4 < ent_type) {
    std::cout << "Invalid type\n";
    return MB_FAILURE;
  }
  // get sets of geom dimension tag from here, and intersect with the gentities from geo
  // ranges

  // get the geom dimensions sets in the set (AKA gentities)
  Range geom_sets;
  Tag geom_tag;
  ErrorCode rval = _mbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geom_tag,
                                              MB_TAG_SPARSE|MB_TAG_CREAT);
  MBERRORR(rval, "Failed to get geom tag.");
  rval = _mbImpl->get_entities_by_type_and_tag(set, MBENTITYSET, &geom_tag, NULL, 1, geom_sets,
			Interface::UNION);
  MBERRORR(rval, "Failed to get gentities from set");

  if (ent_type==4)
  {
	  *pNum=0;
	  for (int k=0; k<=3; k++)
	  {
	    Range gEntsOfTypeK = intersect(geom_sets, _my_geomTopoTool->geoRanges()[k]);
	    *pNum += (int)gEntsOfTypeK.size();
	  }
  }
  else
  {
    Range gEntsOfType = intersect(geom_sets, _my_geomTopoTool->geoRanges()[ent_type]);
    *pNum = (int)gEntsOfType.size();
  }
  // we do not really check if it is in the set or not;
  // _my_gsets[i].find(gent) != _my_gsets[i].end()
  return MB_SUCCESS;
}

ErrorCode FBEngine::getEntType(EntityHandle gent, int * type)
{
  for (int i = 0; i < 4; i++) {
    if (_my_geomTopoTool->geoRanges()[i].find(gent) != _my_geomTopoTool->geoRanges()[i].end()) {
      *type = i;
      return MB_SUCCESS;
    }
  }
  *type = -1; // failure
  return MB_FAILURE;
}
ErrorCode FBEngine::getEntBoundBox(EntityHandle gent, double* min_x,
    double* min_y, double* min_z, double* max_x, double* max_y, double* max_z)
{
  ErrorCode rval;
  int type;
  rval = getEntType(gent, &type);
  MBERRORR(rval, "Failed to get entity type.");

  if (type == 0) {
    rval = getVtxCoord(gent, min_x, min_y, min_z);
    MBERRORR(rval, "Failed to get vertex coordinates.");
    max_x = min_x;
    max_y = min_y;
    max_z = min_z;
  } else if (type == 1) {
    MBERRORR(MB_FAILURE, "iGeom_getEntBoundBox is not supported for Edge entity type.");
  } else if (type == 2 || type == 3) {

    EntityHandle root;
    CartVect center, axis[3];
    rval = _my_geomTopoTool->get_root(gent, root);
    MBERRORR(rval, "Failed to get tree root in iGeom_getEntBoundBox.");
    rval = _my_geomTopoTool->obb_tree()->box(root, center.array(),
        axis[0].array(), axis[1].array(), axis[2].array());
    MBERRORR(rval, "Failed to get closest point in iGeom_getEntBoundBox.");

    CartVect absv[3];
    for (int i = 0; i < 3; i++) {
      absv[i] = CartVect(fabs(axis[i][0]), fabs(axis[i][1]), fabs(axis[i][2]));
    }
    CartVect min, max;
    min = center - absv[0] - absv[1] - absv[2];
    max = center + absv[0] + absv[1] + absv[2];
    *min_x = min[0];
    *min_y = min[1];
    *min_z = min[2];
    *max_x = max[0];
    *max_y = max[1];
    *max_z = max[2];
  } else
    return MB_FAILURE;

  return MB_SUCCESS;
}
ErrorCode FBEngine::getEntClosestPt(EntityHandle this_gent, double near_x,
    double near_y, double near_z, double* on_x, double* on_y, double* on_z)
{
  ErrorCode rval;
  int type;
  rval = getEntType(this_gent, &type);
  MBERRORR(rval, "Failed to get entity type.");

  if (type == 0) {
    rval = getVtxCoord(this_gent, on_x, on_y, on_z);
    MBERRORR(rval, "Failed to get vertex coordinates.");
  } else if (_smooth && type == 1) {
    *on_x = near_x;
    *on_y = near_y;
    *on_z = near_z;
    SmoothCurve * smthcurve = _edges[this_gent];
    // call the new method from smooth edge
    smthcurve->move_to_curve( *on_x, *on_y, *on_z);

  } else if (type == 2 || type == 3) {
    double point[3] = { near_x, near_y, near_z };
    double point_out[3];
    EntityHandle root, facet_out;
    if (_smooth && 2 == type) {
      SmoothFace* smthFace = _faces[this_gent];
      *on_x = near_x;
      *on_y = near_y;
      *on_z = near_z;
      smthFace->move_to_surface(*on_x, *on_y, *on_z);
    } else {
      rval = _my_geomTopoTool->get_root(this_gent, root);
      MBERRORR(rval, "Failed to get tree root in iGeom_getEntClosestPt.");
      rval = _my_geomTopoTool->obb_tree()->closest_to_location(point, root,
          point_out, facet_out);
      MBERRORR(rval, "Failed to get closest point in iGeom_getEntClosestPt.");

      *on_x = point_out[0];
      *on_y = point_out[1];
      *on_z = point_out[2];
    }
  } else
    return MB_TYPE_OUT_OF_RANGE;

  return MB_SUCCESS;
}

ErrorCode FBEngine::getVtxCoord(EntityHandle vertex_handle, double * x0,
    double * y0, double * z0)
{
  int type;
  ErrorCode rval = getEntType(vertex_handle, &type);
  MBERRORR(rval, "Failed to get entity type in getVtxCoord.");

  if (type != 0) {
    MBERRORR(MB_FAILURE, "Entity is not a vertex type.");
  }

  Range entities;
  rval = MBI->get_entities_by_type(vertex_handle, MBVERTEX, entities);
  MBERRORR(rval, "can't get nodes in vertex set.");

  if (entities.size() != 1) {
    MBERRORR(MB_FAILURE, "Vertex has multiple points.");
  }
  double coords[3];
  EntityHandle node = entities[0];
  rval = MBI->get_coords(&node, 1, coords);
  MBERRORR(rval, "can't get coordinates.");
  *x0 = coords[0];
  *y0 = coords[1];
  *z0 = coords[2];

  return MB_SUCCESS;
}

ErrorCode FBEngine::gsubtract(EntityHandle entity_set_1,
    EntityHandle entity_set_2, EntityHandle result_entity_set)
{
  /*result_entity_set = subtract(entity_set_1, entity_set_2);*/
  Range ents1, ents2;
  ErrorCode rval = MBI->get_entities_by_type(entity_set_1, MBENTITYSET, ents1);
  MBERRORR(rval, "can't get entities from set 1.");

  rval = MBI->get_entities_by_type(entity_set_2, MBENTITYSET, ents2);
  MBERRORR(rval, "can't get entities from set 2.");

  ents1 = subtract(ents1, ents2);
  rval = MBI->clear_meshset(&result_entity_set, 1);
  MBERRORR(rval, "can't empty set.");

  rval = MBI->add_entities(result_entity_set, ents1);
  MBERRORR(rval, "can't add result to set.");

  return rval;
}

ErrorCode FBEngine::getEntNrmlXYZ(EntityHandle entity_handle, double x,
    double y, double z, double* nrml_i, double* nrml_j, double* nrml_k)
{
  // just do for surface and volume
  int type;
  ErrorCode rval = getEntType(entity_handle, &type);
  MBERRORR(rval, "Failed to get entity type in iGeom_getEntNrmlXYZ.");

  if (type != 2 && type != 3) {
    MBERRORR(MB_FAILURE, "Entities passed into gentityNormal must be face or volume.");
  }

  if (_smooth && 2 == type) {
    SmoothFace* smthFace = _faces[entity_handle];
    //*on_x = near_x; *on_y = near_y; *on_z = near_z;
    smthFace-> normal_at(x, y, z, *nrml_i, *nrml_j, *nrml_k);

  } else {
    // get closest location and facet
    double point[3] = { x, y, z };
    double point_out[3];
    EntityHandle root, facet_out;
    _my_geomTopoTool->get_root(entity_handle, root);
    rval = _my_geomTopoTool->obb_tree()->closest_to_location(point, root,
        point_out, facet_out);
    MBERRORR(rval , "Failed to get closest location in iGeom_getEntNrmlXYZ.");

    // get facet normal
    const EntityHandle* conn;
    int len;
    CartVect coords[3], normal;
    rval = MBI->get_connectivity(facet_out, conn, len);
    MBERRORR(rval, "Failed to get triangle connectivity in iGeom_getEntNrmlXYZ.");
    if (len != 3)
      MBERRORR(MB_FAILURE, " not a triangle, error ");

    rval = MBI->get_coords(conn, len, coords[0].array());
    MBERRORR(rval, "Failed to get triangle coordinates in iGeom_getEntNrmlXYZ.");

    coords[1] -= coords[0];
    coords[2] -= coords[0];
    normal = coords[1] * coords[2];
    normal.normalize();
    *nrml_i = normal[0];
    *nrml_j = normal[1];
    *nrml_k = normal[2];
  }
  return MB_SUCCESS;
}

ErrorCode FBEngine::getPntRayIntsct(double x, double y, double z, double dir_x,
    double dir_y, double dir_z,
    std::vector<EntityHandle> &intersect_entity_handles,
    /* int storage_order,*/
    std::vector<double> & intersect_coords, std::vector<double> & param_coords)
{
  // this is pretty cool
  // we will return only surfaces (gentities )
  //
  ErrorCode rval;

  unsigned int numfaces = _my_gsets[2].size();
  // do ray fire
  const double point[] = { x, y, z };
  const double dir[] = { dir_x, dir_y, dir_z };
  CartVect P(point);
  CartVect V(dir);

  //std::vector<double> distances;
  std::vector<EntityHandle> facets;
  //std::vector<EntityHandle> sets;
  unsigned int i;
  for (i = 0; i < numfaces; i++) {
    EntityHandle face = _my_gsets[2][i];
    EntityHandle rootForFace;
    rval = _my_geomTopoTool->get_root(face, rootForFace);
    MBERRORR(rval, "Failed to get root of face.");
    std::vector<double> distances_out;
    std::vector<EntityHandle> sets_out;
    std::vector<EntityHandle> facets_out;
    rval = _my_geomTopoTool->obb_tree()-> ray_intersect_sets(distances_out,
        sets_out, facets_out, rootForFace, tolerance,
        min_tolerace_intersections, point, dir);
    unsigned int j;
    for (j = 0; j < distances_out.size(); j++)
      param_coords.push_back(distances_out[j]);
    for (j = 0; j < sets_out.size(); j++)
      intersect_entity_handles.push_back(sets_out[j]);
    for (j = 0; j < facets_out.size(); j++)
      facets.push_back(facets_out[j]);

    MBERRORR(rval, "Failed to get ray intersections.");
  }
  // facets.size == distances.size()!!
  for (i = 0; i < param_coords.size(); i++) {
    CartVect intx = P + param_coords[i] * V;
    for (int j = 0; j < 3; j++)
      intersect_coords.push_back(intx[j]);

  }
  if (_smooth) {
    // correct the intersection point and the distance for smooth surfaces
    for (i = 0; i < intersect_entity_handles.size(); i++) {
      //EntityHandle geoSet = MBH_cast(sets[i]);
      SmoothFace* sFace = _faces[intersect_entity_handles[i]];
      // correct coordinates and distance from point
      /*moab::ErrorCode ray_intersection_correct(moab::EntityHandle facet, // (IN) the facet where the patch is defined
       moab::CartVect &pt, // (IN) shoot from
       moab::CartVect &ray, // (IN) ray direction
       moab::CartVect &eval_pt, // (INOUT) The intersection point
       double & distance, // (IN OUT) the new distance
       bool &outside);*/
      CartVect pos(&(intersect_coords[3 * i]));
      double dist = param_coords[i];
      bool outside = false;
      rval = sFace->ray_intersection_correct(facets[i], P, V, pos, dist,
          outside);
      MBERRORR(rval, "Failed to get better point on ray.");
      param_coords[i] = dist;

      for (int j = 0; j < 3; j++)
        intersect_coords[3 * i + j] = pos[j];
    }
  }
  return MB_SUCCESS;
}

ErrorCode FBEngine::getAdjacentEntities(const EntityHandle from,
    const int to_dim, Range &adjs)
{
  int this_dim = -1;
  for (int i = 0; i < 4; i++) {
    if (_my_geomTopoTool->geoRanges()[i].find(from) != _my_geomTopoTool->geoRanges()[i].end()) {
      this_dim = i;
      break;
    }
  }

  // check target dimension
  if (-1 == this_dim) {
    //ProcessError(iBase_FAILURE, "Entity not a geometry entity.");
    return MB_FAILURE;
  } else if (0 > to_dim || 3 < to_dim) {
    //ProcessError(iBase_FAILURE, "To dimension must be between 0 and 3.");
    return MB_FAILURE;
  } else if (to_dim == this_dim) {
    //ProcessError(iBase_FAILURE,
    //      "To dimension must be different from entity dimension.");
    return MB_FAILURE;
  }

  ErrorCode rval = MB_SUCCESS;
  adjs.clear();
  if (to_dim > this_dim) {
    int diffDim = to_dim-this_dim;
    rval = MBI->get_parent_meshsets(from, adjs, diffDim);
    if (MB_SUCCESS != rval) return rval;
    if (diffDim>1)
    {
      // subtract the parents that come with diffDim-1 hops
      Range extra;
      rval = MBI->get_parent_meshsets(from, extra, diffDim-1);
      if (MB_SUCCESS != rval) return rval;
      adjs = subtract(adjs, extra);
    }

  } else {
    int diffDim = this_dim - to_dim;
    rval = MBI->get_child_meshsets(from, adjs, diffDim);
    if (MB_SUCCESS != rval) return rval;
    if (diffDim > 1)
    {
      // subtract the children that come with diffDim-1 hops
      Range extra;
      rval = MBI->get_child_meshsets(from, extra, diffDim-1);
      if (MB_SUCCESS != rval) return rval;
      adjs = subtract(adjs, extra);
    }
  }

  return rval;
}

// so far, this one is
// used only for __MKModelEntityGeo tag

ErrorCode FBEngine::createTag(const char* tag_name, int tag_size, int tag_type,
    Tag & tag_handle_out)
{
  // this is copied from iMesh_MOAB.cpp; different name to not have trouble
  // with it
  // also, we do not want to depend on iMesh.h...
  // iMesh is more complicated, because of the options passed

  DataType mb_data_type_table2[] = { MB_TYPE_OPAQUE, MB_TYPE_INTEGER,
      MB_TYPE_DOUBLE, MB_TYPE_HANDLE, MB_TYPE_HANDLE };
  moab::TagType storage = MB_TAG_SPARSE;
  ErrorCode result;

  result = MBI->tag_get_handle(tag_name, tag_size,
      mb_data_type_table2[tag_type], tag_handle_out, storage | MB_TAG_EXCL);

  if (MB_SUCCESS != result) {
    std::string msg("iMesh_createTag: ");
    if (MB_ALREADY_ALLOCATED == result) {
      msg += "Tag already exists with name: \"";
      msg += tag_name;
      std::cout<<msg << "\n";
    }
    else
    {
      std::cout<< "Failed to create tag with name: " << tag_name << "\n";
      return MB_FAILURE;
    }

  }

  // end copy
  return MB_SUCCESS;
}


ErrorCode FBEngine::getArrData(const EntityHandle* entity_handles,
    int entity_handles_size, Tag tag_handle, void* tag_values_out)
{
  // responsibility of the user to have tag_values_out properly allocated
  // only some types of Tags are possible (double, int, etc)
  return MBI->tag_get_data(tag_handle, entity_handles, entity_handles_size,
      tag_values_out);
}

ErrorCode FBEngine::setArrData(const EntityHandle* entity_handles,
    int entity_handles_size, Tag tag_handle, const void* tag_values)
{
  // responsibility of the user to have tag_values_out properly allocated
  // only some types of Tags are possible (double, int, etc)
  return MBI->tag_set_data(tag_handle, entity_handles, entity_handles_size,
      tag_values);
}

ErrorCode FBEngine::getEntAdj(EntityHandle handle, int type_requested,
    Range & adjEnts)
{
  return getAdjacentEntities(handle, type_requested, adjEnts);
}

ErrorCode FBEngine::getEgFcSense(EntityHandle mbedge, EntityHandle mbface,
    int & sense_out)
{

  // this one is important, for establishing the orientation of the edges in faces
  // use senses
  std::vector<EntityHandle> faces;
  std::vector<int> senses; // 0 is forward and 1 is backward
  ErrorCode rval = _my_geomTopoTool->get_senses(mbedge, faces, senses);
  if (MB_SUCCESS != rval)
    return rval;

  for (unsigned int i = 0; i < faces.size(); i++) {
    if (faces[i] == mbface) {
      sense_out = senses[i];
      return MB_SUCCESS;
    }
  }
  return MB_FAILURE;

}
// we assume the measures array was allocated correctly
ErrorCode FBEngine::measure(const EntityHandle * moab_entities,
    int entities_size, double * measures)
{
  ErrorCode rval;
  for (int i = 0; i < entities_size; i++) {
    measures[i] = 0.;

    int type;
    EntityHandle gset = moab_entities[i];
    rval = getEntType(gset, &type);
    if (MB_SUCCESS != rval)
      return rval;
    if (type == 1) { // edge: get all edges part of the edge set
      Range entities;
      rval = MBI->get_entities_by_type(gset, MBEDGE, entities);
      if (MB_SUCCESS != rval)
        return rval;

      for (Range::iterator it = entities.begin(); it != entities.end(); ++it) {
        EntityHandle edge = *it;
        CartVect vv[2];
        const EntityHandle *conn2 = NULL;
        int num_nodes;
        rval = MBI->get_connectivity(edge, conn2, num_nodes);
        if (MB_SUCCESS != rval || num_nodes != 2)
          return MB_FAILURE;
        rval = MBI->get_coords(conn2, 2, (double *) &(vv[0][0]));
        if (MB_SUCCESS != rval)
          return rval;

        vv[0] = vv[1] - vv[0];
        measures[i] += vv[0].length();
      }
    }
    if (type == 2) { // surface
      // get triangles in surface; TODO: quads!
      Range entities;
      rval = MBI->get_entities_by_type(gset, MBTRI, entities);
      if (MB_SUCCESS != rval)
        return rval;

      for (Range::iterator it = entities.begin(); it != entities.end(); ++it) {
        EntityHandle tri = *it;
        CartVect vv[3];
        const EntityHandle *conn3 = NULL;
        int num_nodes;
        rval = MBI->get_connectivity(tri, conn3, num_nodes);
        if (MB_SUCCESS != rval || num_nodes != 3)
          return MB_FAILURE;
        rval = MBI->get_coords(conn3, 3, (double *) &(vv[0][0]));
        if (MB_SUCCESS != rval)
          return rval;

        vv[1] = vv[1] - vv[0];
        vv[2] = vv[2] - vv[0];
        vv[0] = vv[1] * vv[2];
        measures[i] += vv[0].length() / 2;// area of triangle
      }

    }
  }
  return MB_SUCCESS;
}

ErrorCode FBEngine::getEntNrmlSense(EntityHandle /*face*/, EntityHandle /*region*/,
                                    int& /*sense*/)
{
  return MB_NOT_IMPLEMENTED; // not implemented
}

ErrorCode FBEngine::getEgEvalXYZ(EntityHandle /*edge*/, double /*x*/, double /*y*/,
                                 double /*z*/, double& /*on_x*/, double& /*on_y*/, double& /*on_z*/, double& /*tngt_i*/,
                                 double& /*tngt_j*/, double& /*tngt_k*/, double& /*cvtr_i*/, double& /*cvtr_j*/,
                                 double& /*cvtr_k*/)
{
  return MB_NOT_IMPLEMENTED; // not implemented
}
ErrorCode FBEngine::getFcEvalXYZ(EntityHandle /*face*/, double /*x*/, double /*y*/,
                                 double /*z*/, double& /*on_x*/, double& /*on_y*/, double& /*on_z*/, double& /*nrml_i*/,
                                 double& /*nrml_j*/, double& /*nrml_k*/, double& /*cvtr1_i*/, double& /*cvtr1_j*/,
                                 double& /*cvtr1_k*/, double& /*cvtr2_i*/, double& /*cvtr2_j*/, double& /*cvtr2_k*/)
{
  return MB_NOT_IMPLEMENTED; // not implemented
}

ErrorCode FBEngine::getEgVtxSense(EntityHandle edge, EntityHandle vtx1,
    EntityHandle vtx2, int& sense)
{
  // need to decide first or second vertex
  // important for moab
  int type;

  EntityHandle v1, v2;
  ErrorCode rval = getEntType(vtx1, &type);
  if (MB_SUCCESS != rval || type != 0)
    return MB_FAILURE;
  // edge: get one vertex as part of the vertex set
  Range entities;
  rval = MBI->get_entities_by_type(vtx1, MBVERTEX, entities);
  if (MB_SUCCESS != rval)
    return rval;
  if (entities.size() < 1)
    return MB_FAILURE;
  v1 = entities[0]; // the first vertex
  entities.clear();
  rval = getEntType(vtx2, &type);
  if (MB_SUCCESS != rval || type != 0)
    return MB_FAILURE;
  rval = MBI->get_entities_by_type(vtx2, MBVERTEX, entities);
  if (MB_SUCCESS != rval)
    return rval;
  if (entities.size() < 1)
    return MB_FAILURE;
  v2 = entities[0]; // the first vertex
  entities.clear();
  // now get the edges, and get the first node and the last node in sequence of edges
  // the order is important...
  // these are ordered sets !!
  std::vector<EntityHandle> ents;
  rval = MBI->get_entities_by_type(edge, MBEDGE, ents);
  if (MB_SUCCESS != rval)
    return rval;
  if (ents.size() < 1)
    return MB_FAILURE;

  const EntityHandle* conn = NULL;
  int len;
  EntityHandle startNode, endNode;
  rval = MBI->get_connectivity(ents[0], conn, len);
  if (MB_SUCCESS != rval)
    return rval;
  startNode = conn[0];
  rval = MBI->get_connectivity(ents[ents.size() - 1], conn, len);
  if (MB_SUCCESS != rval)
    return rval;

  endNode = conn[1];
  sense = 1; //
  if ((startNode == endNode) && (v1 == startNode)) {
    sense = 0; // periodic
  }
  if ((startNode == v1) && (endNode == v2)) {
    sense = 1; // forward
  }
  if ((startNode == v2) && (endNode == v1)) {
    sense = -1; // reverse
  }
  return MB_SUCCESS;
}

ErrorCode FBEngine::getEntURange(EntityHandle edge, double& u_min,
    double& u_max)
{
  SmoothCurve * smoothCurve = _edges[edge];// this is a map
  // now, call smoothCurve methods
  smoothCurve -> get_param_range(u_min, u_max);
  return MB_SUCCESS;
}

ErrorCode FBEngine::getEntUtoXYZ(EntityHandle edge, double u, double& x,
    double& y, double& z)
{
  SmoothCurve * smoothCurve = _edges[edge];// this is a map
  // now, call smoothCurve methods
  smoothCurve -> position_from_u(u, x, y, z);
  return MB_SUCCESS;
}

ErrorCode FBEngine::getEntTgntU( EntityHandle edge,
                                    double u,
                                    double& i, double& j, double& k )
{
  SmoothCurve * smoothCurve = _edges[edge];// this is a map
  // now, call smoothCurve methods
  double tg[3];
  double x, y, z;
  smoothCurve -> position_from_u(u, x, y, z, tg);
  i = tg[0];
  j = tg[1];
  k = tg[2];
  return MB_SUCCESS;
}
ErrorCode FBEngine::isEntAdj(EntityHandle entity1, EntityHandle entity2,
    bool& adjacent_out)
{
  int type1, type2;
  ErrorCode rval = getEntType(entity1, &type1);
  if (MB_SUCCESS != rval)
    return rval;
  rval = getEntType(entity2, &type2);
  if (MB_SUCCESS != rval)
    return rval;

  Range adjs;
  if (type1 < type2) {
    rval = MBI->get_parent_meshsets(entity1, adjs, type2 - type1);
    if (MB_SUCCESS != rval)
      return rval;// MBERRORR("Failed to get parent meshsets in iGeom_isEntAdj.");

  } else {
    // note: if they ave the same type, they will not be adjacent, in our definition
    rval = MBI->get_child_meshsets(entity1, adjs, type1 - type2);
    if (MB_SUCCESS != rval)
      return rval;//MBERRORR("Failed to get child meshsets in iGeom_isEntAdj.");
  }

  //adjacent_out = adjs.find(entity2) != _my_gsets[type2].end();
  // hmmm, possible bug here; is this called?
  adjacent_out = adjs.find(entity2) != adjs.end();


  return MB_SUCCESS;
}

ErrorCode FBEngine::split_surface_with_direction(EntityHandle face, std::vector<double> & xyz, double * direction,
    int closed, double min_dot, EntityHandle & oNewFace )
{

  // first of all, find all intersection points (piercing in the face along the direction)
  // assume it is robust; what if it is not sufficiently robust?
  // if the polyline is open, find the intersection with the boundary edges, of the
  // polyline extruded at ends

  ErrorCode rval;

  // then find the position
  int numIniPoints = (int) xyz.size() / 3;
  if (  (closed && numIniPoints < 3)  ||  (!closed && numIniPoints<2) )
    MBERRORR(MB_FAILURE, "not enough polyline points ");
  EntityHandle rootForFace;

  rval = _my_geomTopoTool->get_root(face, rootForFace);
  MBERRORR(rval, "Failed to get root of face.");

  const double dir[] = { direction[0], direction[1], direction[2] };
  std::vector<EntityHandle> nodes; // get the nodes closest to the ray traces of interest

  // these are nodes on the boundary of original face;
  // if the cutting line is not closed, the starting - ending vertices of the
  // polygonal line must come from this list

  std::vector<CartVect> b_pos;
  std::vector<EntityHandle> boundary_nodes;
  std::vector<EntityHandle> splittingNodes;
  Range boundary_mesh_edges;
  if (!closed)
  {
    rval = boundary_nodes_on_face(face, boundary_nodes);
    MBERRORR(rval, "Failed to get boundary nodes.");
    b_pos.resize(boundary_nodes.size());
    rval = _mbImpl->get_coords(&(boundary_nodes[0]), boundary_nodes.size(), (double *)(&b_pos[0][0]));
    MBERRORR(rval, "Failed to get coordinates for boundary nodes.");
    rval = boundary_mesh_edges_on_face(face, boundary_mesh_edges);
    MBERRORR(rval, "Failed to get mesh boundary edges for face.");
  }
  //
  int i = 0;
  CartVect dirct(direction);
  dirct.normalize(); // maybe an overkill?
  for (; i < numIniPoints; i++) {

    const double point[] = { xyz[3 * i], xyz[3 * i + 1], xyz[3 * i + 2] };// or even point( &(xyz[3*i]) ); //
    CartVect p1(point);
    if (!closed && ( (0==i) || (numIniPoints-1==i) ) )
    {

      // find the intersection point between a plane and boundary mesh edges
      // this will be the closest point on the boundary of face
      /// the segment is the first or last segment in the polyline
      int i1 = i+1;
      if (i==numIniPoints-1) i1= i-1;// previous point if the last
      // the direction is from point to point1
      const double point1[] = { xyz[3 * i1], xyz[3 * i1 + 1], xyz[3 * i1 + 2] };
      CartVect p2(point1);
      CartVect normPlane=(p2-p1)*dirct;
      normPlane.normalize();
      //(roughly, from p1 to p2, perpendicular to dirct, in the "xy" plane
      // if the intx point is "outside" p1 - p2, skip if the intx point is closer to p2
      CartVect perpDir = dirct*normPlane;
      Range::iterator ite=boundary_mesh_edges.begin();
      // do a linear search for the best intersection point position (on a boundary edge)
      if (debug_splits)
      {
        std::cout << " p1:" << p1 <<"\n";
        std::cout << " p2:" << p2 <<"\n";
        std::cout << " perpDir:" << perpDir << "\n";
        std::cout<<" boundary edges size:" << boundary_mesh_edges.size() << "\n";
      }
      for ( ; ite!=boundary_mesh_edges.end(); ++ite)
      {
        EntityHandle candidateEdge = *ite;
        const EntityHandle * conn2;
        int nno;
        rval= _mbImpl->get_connectivity(candidateEdge, conn2, nno);
        MBERRORR(rval, "Failed to get conn for boundary edge");
        CartVect pts[2];
        rval = _mbImpl->get_coords(conn2, 2, &(pts[0][0]));
        MBERRORR(rval, "Failed to get coords of nodes for boundary edge");
        CartVect intx_point;
        double parPos;
        bool intersect = intersect_segment_and_plane_slice(pts[0], pts[1],
            p1, p2, dirct, normPlane, intx_point,  parPos);
        if (debug_splits)
        {
          std::cout << "   Edge:" << _mbImpl->id_from_handle(candidateEdge)<<"\n";
          std::cout << "   Node 1:" << _mbImpl->id_from_handle(conn2[0]) << pts[0] <<"\n";
          std::cout << "   Node 2:" << _mbImpl->id_from_handle(conn2[1]) << pts[1] <<"\n";
          std::cout << "    Intersect bool:" << intersect << "\n";
        }
        if (intersect)
        {
          double proj1 = (intx_point-p1)%perpDir;
          double proj2 = (intx_point-p2)%perpDir;
          if (
                ( fabs(proj1) > fabs(proj2) ) // this means it is closer to p2 than p1
              )
            continue; // basically, this means the intersection point is with a
                      //  boundary edge on the other side, closer to p2 than p1, so we skip it
          if (parPos==0)
          {
            //close to vertex 1, nothing to do
            nodes.push_back(conn2[0]);
            splittingNodes.push_back(conn2[0]);
          }
          else if (parPos ==1.)
          {
            //close to vertex 2, nothing to do
            nodes.push_back(conn2[1]);
            splittingNodes.push_back(conn2[1]);
          }
          else
          {
            // break the edge, create a new node at intersection point (will be smoothed out)
            EntityHandle newVertex;
            rval = _mbImpl->create_vertex(&(intx_point[0]), newVertex);
            MBERRORR(rval, "can't create vertex");
            nodes.push_back(newVertex);
            split_internal_edge(candidateEdge, newVertex);
            splittingNodes.push_back(newVertex);
            _brokenEdges[newVertex] = candidateEdge;
            _piercedEdges.insert(candidateEdge);
          }
          break; // break from the loop over boundary edges, we are interested in the first
                 //      split (hopefully, the only split)
        }
      }
      if (ite==boundary_mesh_edges.end())
        MBERRORR(MB_FAILURE, "Failed to find boundary intersection edge. Bail out");

    }
    else
    {
      std::vector<double> distances_out;
      std::vector<EntityHandle> sets_out;
      std::vector<EntityHandle> facets_out;
      rval = _my_geomTopoTool->obb_tree()-> ray_intersect_sets(distances_out,
          sets_out, facets_out, rootForFace, tolerance,
          min_tolerace_intersections, point, dir);
      MBERRORR(rval, "Failed to get ray intersections.");
      if (distances_out.size() < 1)
        MBERRORR(MB_FAILURE, "Failed to get one intersection point, bad direction.");

      if (distances_out.size() > 1) {
        std::cout
            << " too many intersection points. Only the first one considered\n";
      }
      std::vector<EntityHandle>::iterator pFace = std::find(sets_out.begin(), sets_out.end(), face);

      if (pFace == sets_out.end())
        MBERRORR(MB_FAILURE, "Failed to intersect given face, bad direction.");
      unsigned int index = pFace-sets_out.begin();
      // get the closest node of the triangle, and modify locally the triangle(s), so the
      // intersection point is at a new vertex, if needed
      CartVect P(point);
      CartVect Dir(dir);
      CartVect newPoint = P + distances_out[index] * Dir;
      // get the triangle coordinates

      double area_coord[3];
      EntityHandle  boundary_handle =0; // if 0, not on a boundary
      rval = area_coordinates(_mbImpl, facets_out[index], newPoint, area_coord, boundary_handle);
      MBERRORR(rval, "Failed to get area coordinates");

      if (debug_splits)
      {
        std::cout <<" int point:" << newPoint << " area coord " << area_coord[0] << " "
           <<  area_coord[1] << " " << area_coord[2] << "\n";
        std::cout << " triangle: " <<
            _mbImpl->id_from_handle(facets_out[index]) << " boundary:" <<  boundary_handle <<  "\n";
      }
      EntityType type;
      if (boundary_handle)
        type = _mbImpl->type_from_handle(boundary_handle);
      if (boundary_handle&& (type == MBVERTEX))
      {
        // nothing to do, we are happy
        nodes.push_back(boundary_handle);
      }
      else
      {
        // for an edge, we will split 2 triangles
        // for interior point, we will create 3 triangles out of one
        // create a new vertex
        EntityHandle newVertex;
        rval = _mbImpl->create_vertex(&(newPoint[0]), newVertex);
        if (boundary_handle)// this is edge
        {
          split_internal_edge(boundary_handle, newVertex);
          _piercedEdges.insert(boundary_handle);// to be removed at the end
        }
        else
          divide_triangle(facets_out[index], newVertex);

        nodes.push_back(newVertex);
      }

    }
  }
  // now, we have to find more intersection points, either interior to triangles, or on edges, or on vertices
  // use the same tolerance as before
  // starting from 2 points on 2 triangles, and having the direction, get more intersection points
  // between the plane formed by direction and those 2 points, and edges from triangulation (the triangles
  // involved will be part of the same gentity , original face ( moab set)
  //int closed = 1;// closed = 0 if the polyline is not closed

  CartVect Dir(direction);
  std::vector<EntityHandle>  chainedEdges;

  for (i = 0; i < numIniPoints - 1 + closed; i++) {
    int nextIndex = (i + 1) % numIniPoints;
    std::vector<EntityHandle> trianglesAlong;
    std::vector<CartVect> points;
      // otherwise to edges or even nodes
    std::vector<EntityHandle> entities;
    //start with initial points, intersect along the direction, find the facets
    rval = compute_intersection_points(face, nodes[i], nodes[nextIndex], Dir, points,
        entities, trianglesAlong);
    MBERRORR(rval, "can't get intersection points along a line");
    std::vector<EntityHandle> nodesAlongPolyline;
    // refactor code; move some edge creation for each 2 intersection points
    nodesAlongPolyline.push_back(entities[0]); // it is for sure a node
    int num_points = (int) points.size(); // it should be num_triangles + 1
    for (int j = 0; j < num_points-1; j++) {
      EntityHandle tri = trianglesAlong[j]; // this is happening in trianglesAlong i
      EntityHandle e1 = entities[j];
      EntityHandle e2 = entities[j + 1];
      EntityType et1 = _mbImpl->type_from_handle(e1);
      //EntityHandle vertex1 = nodesAlongPolyline[i];// irrespective of the entity type i,
      // we already have the vertex there
      EntityType et2 = _mbImpl->type_from_handle(e2);
      if (et2 == MBVERTEX) {
        nodesAlongPolyline.push_back(e2);
      }
      else // if (et2==MBEDGE)
      {
        CartVect coord_vert=points[j+1];
        EntityHandle newVertex;
        rval = _mbImpl->create_vertex((double*)&coord_vert, newVertex);
        MBERRORR(rval, "can't create vertex");
        nodesAlongPolyline.push_back(newVertex);
      }
      // if vertices, do not split anything, just get the edge for polyline
      if (et2 == MBVERTEX && et1 == MBVERTEX) {
        // nothing to do, just continue;
        continue; // continue the for loop
      }

      if (debug_splits)
      {
        std::cout <<"tri: type: " << _mbImpl->type_from_handle(tri) << " id:" <<
            _mbImpl->id_from_handle(tri) << "\n    e1:" << e1 << " id:" <<_mbImpl->id_from_handle(e1) <<
            "   e2:" << e2 << " id:" <<_mbImpl->id_from_handle(e2) <<"\n";
      }
      // here, at least one is an edge
      rval = BreakTriangle2( tri, e1, e2, nodesAlongPolyline[j], nodesAlongPolyline[j+1]);
      MBERRORR(rval, "can't break triangle 2");
      if (et2==MBEDGE)
        _piercedEdges.insert(e2);
      _piercedTriangles.insert(tri);

    }
    // nodesAlongPolyline will define a new geometric edge
    if (debug_splits)
    {
      std::cout<<"nodesAlongPolyline: " << nodesAlongPolyline.size() << "\n";
      std::cout << "points: " << num_points << "\n";
    }
    // if needed, create edges along polyline, or revert the existing ones, to
    // put them in a new edge set
    EntityHandle new_geo_edge;
    rval = create_new_gedge(nodesAlongPolyline, new_geo_edge);
    MBERRORR(rval, "can't create a new edge");
    chainedEdges.push_back(new_geo_edge);
    // end copy
  }
  // the segment between point_i and point_i+1 is in trianglesAlong_i
  // points_i is on entities_i
  // all these edges are oriented correctly
  rval = split_surface(face, chainedEdges, splittingNodes, oNewFace);
  MBERRORR(rval, "can't split surface");
  //
  rval = chain_edges(min_dot); // acos(0.8)~= 36 degrees
  MBERRORR(rval, "can't chain edges");
  return MB_SUCCESS;
}
/**
 *  this method splits along the polyline defined by points and entities
 *  the polyline will be defined with
 *  // the entities are now only nodes and edges, no triangles!!!
 *  the first and last ones are also nodes for sure
 */
ErrorCode FBEngine::split_surface(EntityHandle face,
     std::vector<EntityHandle> & chainedEdges,
     std::vector<EntityHandle> & splittingNodes, EntityHandle & newFace)
{
  // use now the chained edges to create a new face (loop or clean split)
  // use a fill to determine the new sets, up to the polyline
  // points on the polyline will be moved to the closest point location, with some constraints
  // then the sets will be reset, geometry recomputed. new vertices, new edges, etc.

  Range iniTris;
  ErrorCode rval;
  rval = _mbImpl -> get_entities_by_type(face, MBTRI, iniTris);
  MBERRORR(rval, "can't get initial triangles");

  // start from a triangle that is not in the triangles to delete
  // flood fill 

  bool closed = splittingNodes.size()==0;
  if (!closed)
  {
    //
    if (splittingNodes.size()!=2)
      MBERRORR(MB_FAILURE, "need to have exactly 2 nodes for splitting");
    // we will have to split the boundary edges
    // first, find the actual boundary, and try to split with the 2 end points (nodes)
    // get the adjacent edges, and see which one has the end nodes
    rval = split_boundary(face, splittingNodes[0]);
    MBERRORR(rval, "can't split with first node");
    rval = split_boundary(face, splittingNodes[1]);
    MBERRORR(rval, "can't split with second node)");
  }
  // we will separate triangles to delete, unaffected, new_triangles,
  //  nodesAlongPolyline, 
  Range first, second;
  rval = separate (face, chainedEdges, first, second);

  // now, we are done with the computations;
  // we need to put the new nodes on the smooth surface
  if (this->_smooth)
  {
    rval = smooth_new_intx_points(face, chainedEdges);
    MBERRORR(rval, "can't smooth new points");
  }

  // create the new set
  rval = _mbImpl->create_meshset(MESHSET_SET, newFace);
  MBERRORR(rval, "can't create a new face");

  _my_geomTopoTool->add_geo_set(newFace, 2);


  // the new face will have the first set (positive sense triangles, to the left)
  rval = _mbImpl->add_entities(newFace, first);
  MBERRORR(rval, "can't add first range triangles to new face");

  for (unsigned int j=0; j<chainedEdges.size(); j++)
  {
    EntityHandle new_geo_edge = chainedEdges[j];
    // both faces will have the edge now
    rval = _mbImpl ->add_parent_child( face, new_geo_edge);
    MBERRORR(rval, "can't add parent child relations for new edge");

    rval = _mbImpl ->add_parent_child( newFace, new_geo_edge);
    MBERRORR(rval, "can't add parent child relations for new edge");
    // add senses
    // sense newFace is 1, old face is -1
    rval = _my_geomTopoTool-> set_sense( new_geo_edge, newFace, 1);
    MBERRORR(rval, "can't set sense for new edge");

    rval = _my_geomTopoTool-> set_sense( new_geo_edge, face, -1);
    MBERRORR(rval, "can't set sense for new edge in original face");
  }

  rval = set_neumann_tags(face, newFace);
  MBERRORR(rval, "can't set NEUMANN set tags");

  // now, we should remove from the original set all tris, and put the "second" range
  rval = _mbImpl->remove_entities(face, iniTris);
  MBERRORR(rval, "can't remove original tris from initial face set");

  rval = _mbImpl->add_entities(face, second);
  MBERRORR(rval, "can't add second range to the original set");

  if (!closed)
  {
    rval = redistribute_boundary_edges_to_faces(face, newFace, chainedEdges);
    MBERRORR(rval, "fail to reset the proper boundary faces");
  }

  /*if (_smooth)
    delete_smooth_tags();// they need to be recomputed, anyway
  // this will remove the extra smooth faces and edges
  clean();*/
  // also, these nodes need to be moved to the smooth surface, sometimes before deleting the old
  // triangles
  // remove the triangles from the set, then delete triangles (also some edges need to be deleted!)
  rval=_mbImpl->delete_entities( _piercedTriangles );

  MBERRORR(rval, "can't delete triangles");
  _piercedTriangles.clear();
  // delete edges that are broke up in 2
  rval=_mbImpl->delete_entities(_piercedEdges);
  MBERRORR(rval, "can't delete edges");
  _piercedEdges.clear();

  if (debug_splits)
  {
    _mbImpl->write_file("newFace.vtk", "vtk", 0, &newFace, 1);
    _mbImpl->write_file("leftoverFace.vtk", "vtk", 0, &face, 1);
  }
  return MB_SUCCESS;
}

ErrorCode FBEngine::smooth_new_intx_points(EntityHandle face,
      std::vector<EntityHandle> & chainedEdges)
{

  // do not move nodes from the original face
  // first get all triangles, and then all nodes from those triangles

  Range tris;
  ErrorCode rval = _mbImpl->get_entities_by_type(face, MBTRI, tris);
  MBERRORR(rval, "can't get triangles");

  Range ini_nodes;
  rval = _mbImpl->get_connectivity( tris, ini_nodes);
  MBERRORR(rval, "can't get connectivities");

  SmoothFace* smthFace = _faces[face];

  // get all nodes from chained edges
  Range mesh_edges;
  for (unsigned int j= 0; j<chainedEdges.size(); j++)
  {
    // keep adding to the range of mesh edges
    rval = _mbImpl->get_entities_by_dimension(chainedEdges[j], 1, mesh_edges);
    MBERRORR(rval, "can't get mesh edges");
  }
  // nodes along polyline
  Range nodes_on_polyline;
  rval = _mbImpl->get_connectivity(mesh_edges, nodes_on_polyline, true); // corners only
  MBERRORR(rval, "can't get nodes on the polyline");

  Range new_intx_nodes = subtract(nodes_on_polyline, ini_nodes);

  std::vector<double> ini_coords;
  int num_points = (int)new_intx_nodes.size();
  ini_coords.resize(3*num_points);
  rval = _mbImpl->get_coords(new_intx_nodes, &(ini_coords[0]));
  MBERRORR(rval, "can't get coordinates");

  int i=0;
  for (Range::iterator it = new_intx_nodes.begin(); it != new_intx_nodes.end(); ++it)
  {
    /*EntityHandle node = *it;*/
    int i3=3*i;
    smthFace->move_to_surface(ini_coords[i3], ini_coords[i3+1], ini_coords[i3+2]);
    // reset the coordinates of this node
    ++i;

  }
  rval = _mbImpl->set_coords(new_intx_nodes, &(ini_coords[0]));
  MBERRORR(rval, "can't set smoothed coordinates for the new nodes");

  return MB_SUCCESS;
}
// we will use the fact that the splitting edge is oriented right now
// to the left will be new face, to the right, old face
// (to the left, positively oriented triangles)
ErrorCode FBEngine::separate (EntityHandle face,
    std::vector<EntityHandle> & chainedEdges, Range & first,  Range & second)
{
  //Range unaffectedTriangles = subtract(iniTriangles, _piercedTriangles);
  // insert in each
  // start with a new triangle, and flood to get the first range; what is left is the
  // second range
  // flood fill is considering edges adjacent to seed triangles; if there is
  //  an edge in the new_geo_edge, it is skipped; triangles in the
  // triangles to delete are not added
  // first, create all edges of the new triangles

  //
  // new face will have the new edge oriented positively
  // get mesh edges from geo edge (splitting gedge);

  Range mesh_edges;
  ErrorCode rval;
  // mesh_edges
  for(unsigned int j=0; j<chainedEdges.size(); j++)
  {
    // this will keep adding edges to the mesh_edges range
    // when we get out, the mesh_edges will be in this range, but not ordered
    rval = _mbImpl->get_entities_by_type(chainedEdges[j], MBEDGE, mesh_edges);
    MBERRORR(rval, "can't get new polyline edges");
    if (debug_splits)
    {
     std::cout << " At chained edge " << j << " " <<
         _mbImpl->id_from_handle(chainedEdges[j]) << " mesh_edges Range size:" << mesh_edges.size() << "\n";
    }
  }

  // get a positive triangle adjacent to mesh_edge[0]
  // add to first triangles to the left, second triangles to the right of the mesh_edges ;

  // create a temp tag, and when done, delete it
  // default value: 0
  // 3 to be deleted, pierced
  // 1 first set
  // 2 second set
  // create the tag also for control points on the edges
  int defVal = 0;
  Tag separateTag;
  rval = MBI->tag_get_handle("SEPARATE_TAG", 1, MB_TYPE_INTEGER, separateTag,
                               MB_TAG_DENSE|MB_TAG_CREAT, &defVal );
  MBERRORR(rval, "can't create temp tag for separation");
  // the deleted triangles will get a value 3, from start
  int delVal = 3;
  for (Range::iterator it1=this->_piercedTriangles.begin();
      it1!=_piercedTriangles.end(); ++it1)
  {
    EntityHandle trToDelete= *it1;
    rval = _mbImpl->tag_set_data(separateTag, &trToDelete, 1, &delVal );
    MBERRORR(rval, "can't set delete tag value");
  }

  // find a triangle that will be in the first range, positively oriented about the splitting edge
  EntityHandle seed1=0;
  for (Range::iterator it = mesh_edges.begin(); it!=mesh_edges.end() && !seed1; ++it)
  {
    EntityHandle meshEdge = *it;
    Range adj_tri;
    rval =  _mbImpl->get_adjacencies(&meshEdge, 1,
            2, false, adj_tri);
    MBERRORR(rval, "can't get adj_tris to mesh edge");

    for ( Range::iterator it2=adj_tri.begin(); it2!=adj_tri.end(); ++it2)
    {
      EntityHandle tr=*it2;
      if (_piercedTriangles.find(tr)!=_piercedTriangles.end())
        continue;// do not attach pierced triangles, they are not good
      int num1, sense, offset;
      rval = _mbImpl->side_number(tr, meshEdge, num1, sense, offset);
      MBERRORR(rval, "edge not adjacent");
      if (sense==1)
      {
        //firstSet.insert(tr);
        if (!seed1)
        {
          seed1=tr;
          break;
        }
      }
    }
  }

  // flood fill first set, the rest will be in second set
  // the edges from new_geo_edge will not be crossed

  // get edges of face (adjacencies)
  // also get the old boundary edges, from face; they will be edges to not cross, too
  Range bound_edges;
  rval = getAdjacentEntities(face, 1, bound_edges);
  MBERRORR(rval, "can't get boundary edges");

  // add to the do not cross edges range, all edges from initial boundary
  Range initialBoundaryEdges;
  for (Range::iterator it= bound_edges.begin(); it!=bound_edges.end(); ++it)
  {
    EntityHandle bound_edge=*it;
    rval = _mbImpl->get_entities_by_dimension(bound_edge, 1, initialBoundaryEdges);
  }

  Range doNotCrossEdges = unite(initialBoundaryEdges, mesh_edges);// add the splitting edges !

  // use a second method, with tags
  //
  std::queue<EntityHandle> queue1;
  queue1.push(seed1);
  std::vector<EntityHandle> arr1;
  while(!queue1.empty())
  {
    // start copy
    EntityHandle currentTriangle=queue1.front();
    queue1.pop();
    arr1.push_back(currentTriangle);
    // add new triangles that share an edge
    Range currentEdges;
    rval =  _mbImpl->get_adjacencies(&currentTriangle, 1,
        1, true, currentEdges, Interface::UNION);
    MBERRORR(rval, "can't get adjacencies");
    for (Range::iterator it=currentEdges.begin(); it!=currentEdges.end(); ++it)
    {
      EntityHandle frontEdge= *it;
      if ( doNotCrossEdges.find(frontEdge)==doNotCrossEdges.end())
      {
        // this is an edge that can be crossed
        Range adj_tri;
        rval =  _mbImpl->get_adjacencies(&frontEdge, 1,
                2, false, adj_tri, Interface::UNION);
        MBERRORR(rval, "can't get adj_tris");
        // if the triangle is not in first range, add it to the queue
        for (Range::iterator it2=adj_tri.begin(); it2!=adj_tri.end(); ++it2)
        {
          EntityHandle tri2=*it2;
          int val =0;
          rval = _mbImpl->tag_get_data(separateTag, &tri2, 1, &val );
          MBERRORR(rval, "can't get tag value");
          if (val)
            continue;
          // else, set it to 1
          val =1;
          rval = _mbImpl->tag_set_data(separateTag, &tri2, 1, &val );
          MBERRORR(rval, "can't get tag value");

          queue1.push(tri2);
        }
      }// end edge do not cross
    }// end while
  }

  std::sort(arr1.begin(), arr1.end());
  //Range first1;
  std::copy(arr1.rbegin(), arr1.rend(), range_inserter(first));

  //std::cout<< "\n first1.size() " << first1.size() << " first.size(): " << first.size() << "\n";
  if (debug_splits)
  {
    EntityHandle tmpSet;
    _mbImpl->create_meshset(MESHSET_SET, tmpSet);
    _mbImpl->add_entities(tmpSet, first);
    _mbImpl->write_file("dbg1.vtk", "vtk", 0, &tmpSet, 1);
  }
  // now, decide the set 2:
  // first, get all ini tris
  Range initr;
  rval = _mbImpl -> get_entities_by_type(face, MBTRI, initr);
  MBERRORR(rval, "can't get tris ");
  second = unite(initr, _newTriangles);
  Range second2 = subtract(second, _piercedTriangles);
  second = subtract(second2, first);
  _newTriangles.clear();
  if (debug_splits)
  {
    std::cout<< "\n second.size() " << second.size() << " first.size(): " << first.size() << "\n";
    // debugging code
    EntityHandle tmpSet2;
    _mbImpl->create_meshset(MESHSET_SET, tmpSet2);
    _mbImpl->add_entities(tmpSet2, second);
    _mbImpl->write_file("dbg2.vtk", "vtk", 0, &tmpSet2, 1);
  }
  /*Range intex = intersect(first, second);
  if (!intex.empty() && debug_splits)
  {
    std::cout << "error, the sets should be disjoint\n";
    for (Range::iterator it1=intex.begin(); it1!=intex.end(); ++it1)
    {
      std::cout<<_mbImpl->id_from_handle(*it1) << "\n";
    }
  }*/
  rval = _mbImpl->tag_delete(separateTag);
  MBERRORR(rval, "can't delete tag ");
  return MB_SUCCESS;
}
// if there is an edge between 2 nodes, then check it's orientation, and revert it if needed
ErrorCode  FBEngine::create_new_gedge(std::vector<EntityHandle> &nodesAlongPolyline, EntityHandle & new_geo_edge)
{

  ErrorCode rval = _mbImpl->create_meshset(MESHSET_ORDERED, new_geo_edge);
  MBERRORR(rval, "can't create geo edge");

  // now, get the edges, or create if not existing
  std::vector<EntityHandle> mesh_edges;
  for (unsigned int i=0; i<nodesAlongPolyline.size()-1; i++)
  {
    EntityHandle n1 = nodesAlongPolyline[i], n2 = nodesAlongPolyline[i+1];

    EntityHandle nn2[2];
    nn2[0] = n1;
    nn2[1] = n2;

    std::vector<EntityHandle> adjacent;
    rval = _mbImpl->get_adjacencies(nn2, 2, 1, false, adjacent,
                Interface::INTERSECT);
    // see if there is an edge between those 2 already, and if it is oriented as we like
    bool new_edge = true;
    if (adjacent.size()>=1)
    {
      // check the orientation
      const EntityHandle * conn2 = NULL;
      int nnodes = 0;
      rval = _mbImpl->get_connectivity(adjacent[0], conn2, nnodes);
      MBERRORR(rval, "can't get connectivity");
      if (conn2[0]==nn2[0] && conn2[1]==nn2[1])
      {
        // everything is fine
        mesh_edges.push_back(adjacent[0]);
        new_edge = false;// we found one that's good, no need to create a new one
      }
      else
      {
        _piercedEdges.insert(adjacent[0]);// we want to remove this one, it will be not needed
      }
    }
    if (new_edge)
    {
      // there is no edge between n1 and n2, create one
      EntityHandle mesh_edge;
      rval = _mbImpl->create_element(MBEDGE, nn2, 2, mesh_edge);
      MBERRORR(rval, "Failed to create a new edge");
      mesh_edges.push_back(mesh_edge);
    }
  }

  // add loops edges to the edge set
  rval = _mbImpl->add_entities(new_geo_edge, &mesh_edges[0], mesh_edges.size());// only one edge
  MBERRORR(rval, "can't add edges to new_geo_set");
  // check vertex sets for vertex 1 and vertex 2?
  // get all sets of dimension 0 from database, and see if our ends are here or not

  Range ends_geo_edge;
  ends_geo_edge.insert(nodesAlongPolyline[0]);
  ends_geo_edge.insert(nodesAlongPolyline[nodesAlongPolyline.size()-1]);

  for (unsigned int k = 0; k<ends_geo_edge.size(); k++ )
  {
    EntityHandle node = ends_geo_edge[k];
    EntityHandle nodeSet;
    bool found=find_vertex_set_for_node(node, nodeSet);

    if (!found)
    {
      // create a node set and add the node

      rval = _mbImpl->create_meshset(MESHSET_SET, nodeSet);
      MBERRORR(rval, "Failed to create a new vertex set");

      rval = _mbImpl->add_entities(nodeSet, &node, 1);
      MBERRORR(rval, "Failed to add the node to the set");

      rval = _my_geomTopoTool->add_geo_set(nodeSet, 0);//
      MBERRORR(rval, "Failed to commit the node set");

      if (debug_splits)
      {
        std::cout<<" create a vertex set " << _mbImpl->id_from_handle(nodeSet) << " global id:"<<
            this->_my_geomTopoTool->global_id(nodeSet) << " for node " << node <<  "\n";
      }

    }

    rval = _mbImpl ->add_parent_child( new_geo_edge, nodeSet);
    MBERRORR(rval, "Failed to add parent child relation");
  }
  // finally, put the edge in the range of edges
  rval = _my_geomTopoTool->add_geo_set(new_geo_edge, 1);MB_CHK_ERR(rval);


  return rval;
}

void FBEngine::print_debug_triangle(EntityHandle t)
{
  std::cout<< " triangle id:" << _mbImpl->id_from_handle(t) << "\n";
  const EntityHandle * conn3 = NULL;
  int nnodes = 0;
  _mbImpl->get_connectivity(t, conn3, nnodes);
  // get coords
  CartVect P[3];
  _mbImpl->get_coords(conn3, 3, (double*) &P[0]);
  std::cout <<"  nodes:" << conn3[0] << " " << conn3[1] << " " << conn3[2] << "\n";
  CartVect PP[3];
  PP[0] = P[1]-P[0];
  PP[1] = P[2]-P[1];
  PP[2] = P[0] - P[2];

  std::cout <<"  pos:" <<  P[0] << " " << P[1] << " " << P[2] << "\n";
  std::cout <<"   x,y diffs " <<  PP[0][0] <<" " << PP[0][1]  << ",  " << PP[1][0] <<" " << PP[1][1]
                   << ",  " << PP[2][0] <<" " << PP[2][1]  << "\n";
  return;
}
// actual breaking of triangles
// case 1: n2 interior to triangle
ErrorCode FBEngine::BreakTriangle(EntityHandle , EntityHandle , EntityHandle ,
    EntityHandle , EntityHandle , EntityHandle )
{
  std::cout<< "FBEngine::BreakTriangle not implemented yet\n";
  return MB_FAILURE;
}
// case 2, n1 and n2 on boundary
ErrorCode FBEngine::BreakTriangle2(EntityHandle tri, EntityHandle e1, EntityHandle e2, EntityHandle n1,
  EntityHandle n2)// nodesAlongPolyline are on entities!
{
  // we have the nodes, we just need to reconnect to form new triangles
  ErrorCode rval;
  const EntityHandle * conn3 = NULL;
  int nnodes = 0;
  rval = _mbImpl->get_connectivity(tri, conn3, nnodes);
  MBERRORR(rval, "Failed to get connectivity");
  assert(3 == nnodes);

  EntityType et1= _mbImpl->type_from_handle(e1);
  EntityType et2= _mbImpl->type_from_handle(e2);

  if (MBVERTEX == et1)
  {
    // find the vertex in conn3, and form 2 other triangles
    int index =-1;
    for (index=0; index<3; index++)
    {
      if (conn3[index]==e1)// also n1
        break;
    }
    if (index==3)
      return MB_FAILURE;
    // 2 triangles: n1, index+1, n2, and n1, n2, index+2
    EntityHandle conn[6]={ n1, conn3[(index+1)%3], n2, n1, n2, conn3[(index+2)%3]};
    EntityHandle newTriangle;
    rval = _mbImpl->create_element(MBTRI, conn, 3, newTriangle);
    MBERRORR(rval, "Failed to create a new triangle");
    _newTriangles.insert(newTriangle);
    if (debug_splits)
      print_debug_triangle(newTriangle);
    rval = _mbImpl->create_element(MBTRI, conn+3, 3, newTriangle);// the second triangle
    MBERRORR(rval, "Failed to create a new triangle");
    _newTriangles.insert(newTriangle);
    if (debug_splits)
      print_debug_triangle(newTriangle);
  }
  else if (MBVERTEX == et2)
  {
    int index =-1;
    for (index=0; index<3; index++)
    {
      if (conn3[index]==e2) // also n2
        break;
    }
    if (index==3)
      return MB_FAILURE;
    // 2 triangles: n1, index+1, n2, and n1, n2, index+2
    EntityHandle conn[6]={ n2, conn3[(index+1)%3], n1, n2, n1, conn3[(index+2)%3]};
    EntityHandle newTriangle;
    rval = _mbImpl->create_element(MBTRI, conn, 3, newTriangle);
    MBERRORR(rval, "Failed to create a new triangle");
    _newTriangles.insert(newTriangle);
    if (debug_splits)
          print_debug_triangle(newTriangle);
    rval = _mbImpl->create_element(MBTRI, conn+3, 3, newTriangle);// the second triangle
    MBERRORR(rval, "Failed to create a new triangle");
    _newTriangles.insert(newTriangle);
    if (debug_splits)
          print_debug_triangle(newTriangle);
  }
  else
  {
    // both are edges adjacent to triangle tri
    // there are several configurations possible for n1, n2, between conn3 nodes.
    int num1, num2, sense, offset;
    rval = _mbImpl->side_number(tri, e1, num1, sense, offset);
    MBERRORR(rval, "edge not adjacent");

    rval = _mbImpl->side_number(tri, e2, num2, sense, offset);
    MBERRORR(rval, "edge not adjacent");

    const EntityHandle * conn12; // connectivity for edge 1
    const EntityHandle * conn22; // connectivity for edge 2
    //int nnodes;
    rval = _mbImpl->get_connectivity(e1, conn12, nnodes);
    MBERRORR(rval, "Failed to get connectivity of edge 1");
    assert(2 == nnodes);
    rval = _mbImpl->get_connectivity(e2, conn22, nnodes);
    MBERRORR(rval, "Failed to get connectivity of edge 2");
    assert(2 == nnodes);
    // now, having the side number, work through
    if (debug_splits)
    {
      std::cout << "tri conn3:" << conn3[0] << " "<< conn3[1] <<" " << conn3[2] << "\n";
      std::cout << " edge1: conn12:" << conn12[0] << " "<< conn12[1] <<"  side: " << num1 << "\n";
      std::cout << " edge2: conn22:" << conn22[0] << " "<< conn22[1] <<"  side: " << num2 << "\n";
    }
    int unaffectedSide = 3-num1-num2;
    int i3 = (unaffectedSide+2)%3;// to 0 is 2, to 1 is 0, to 2 is 1
    // triangles will be formed with triVertexIndex , n1, n2 (in what order?)
    EntityHandle v1, v2; // to hold the 2 nodes on edges
    if (num1==i3)
    {
      v1 = n1;
      v2 = n2;
    }
    else // if (num2==i3)
    {
      v1 = n2;
      v2 = n1;
    }
    // three triangles are formed
    int i1 = (i3+1)%3;
    int i2 = (i3+2)%3;
    // we could break the surface differently
    EntityHandle conn[9]={ conn3[i3], v1, v2, v1, conn3[i1], conn3[i2],
        v2, v1,  conn3[i2]};
    EntityHandle newTriangle;
    if (debug_splits)
       std::cout << "Split 2 edges :\n";
    rval = _mbImpl->create_element(MBTRI, conn, 3, newTriangle);
    MBERRORR(rval, "Failed to create a new triangle");
    _newTriangles.insert(newTriangle);
    if (debug_splits)
          print_debug_triangle(newTriangle);
    rval = _mbImpl->create_element(MBTRI, conn+3, 3, newTriangle);// the second triangle
    MBERRORR(rval, "Failed to create a new triangle");
    _newTriangles.insert(newTriangle);
    if (debug_splits)
          print_debug_triangle(newTriangle);
    rval = _mbImpl->create_element(MBTRI, conn+6, 3, newTriangle);// the second triangle
    MBERRORR(rval, "Failed to create a new triangle");
    _newTriangles.insert(newTriangle);
    if (debug_splits)
          print_debug_triangle(newTriangle);
  }

  return MB_SUCCESS;
}

// build the list of intx points and entities from database involved
// vertices, edges, triangles
//it could be just a list of vertices (easiest case to handle after)

ErrorCode FBEngine::compute_intersection_points(EntityHandle & ,
    EntityHandle from, EntityHandle to,
    CartVect & Dir, std::vector<CartVect> & points,
    std::vector<EntityHandle> & entities, std::vector<EntityHandle> & triangles)
{
  // keep a stack of triangles to process, and do not add those already processed
  // either mark them, or maybe keep them in a local set?
  // from and to are now nodes, start from them
  CartVect p1, p2;// the position of from and to
  ErrorCode rval = _mbImpl->get_coords(&from, 1, (double *)&p1);
  MBERRORR(rval, "failed to get 'from' coordinates");
  rval = _mbImpl->get_coords(&to, 1, (double *)&p2);
  MBERRORR(rval, "failed to get 'from' coordinates");

  CartVect vect(p2 - p1);
  double dist2 = vect.length();
  if (dist2 < tolerance_segment) {
    // we are done, return
    return MB_SUCCESS;
  }
  CartVect normPlane = Dir * vect;
  normPlane.normalize();
  std::set<EntityHandle> visitedTriangles;
  CartVect currentPoint = p1;
  // push the current point if it is empty
  if (points.size() == 0) {
    points.push_back(p1);
    entities.push_back(from);// this is a node now
  }

  // these will be used in many loops
  CartVect intx = p1;// somewhere to start
  double param = -1.;

  // first intersection
  EntityHandle currentBoundary = from;// it is a node, in the beginning

  vect = p2 - currentPoint;
  while (vect.length() > 0.) {
    // advance towards "to" node, from boundary handle
    EntityType etype = _mbImpl->type_from_handle(currentBoundary);
    //if vertex, look for other triangles connected which intersect our plane (defined by p1, p2, dir)
    std::vector<EntityHandle> adj_tri;
    rval = _mbImpl->get_adjacencies(&currentBoundary, 1, 2, false, adj_tri);
    unsigned int j = 0;
    EntityHandle tri;
    for (; j < adj_tri.size(); j++) {
      tri = adj_tri[j];
      if (visitedTriangles.find(tri) != visitedTriangles.end())
        continue;// get another triangle, this one was already visited
      // check if it is one of the triangles that was pierced already
      if (_piercedTriangles.find(tri) != _piercedTriangles.end())
        continue;
      // if vertex, look for opposite edge
      // if edge, look for 2 opposite edges
      // get vertices
      int nnodes;
      const EntityHandle * conn3;
      rval = _mbImpl->get_connectivity(tri, conn3, nnodes);
      MBERRORR(rval, "Failed to get connectivity");
      // if one of the nodes is to, stop right there
      {
        if (conn3[0]==to || conn3[1]==to || conn3[2]==to)
        {
          visitedTriangles.insert(tri);
          triangles.push_back(tri);
          currentPoint = p2;
          points.push_back(p2);
          entities.push_back(to);// we are done
          break;// this is break from for loop, we still need to get out of while
          // we will get out, because vect will become 0, (p2-p2)
        }
      }
      EntityHandle nn2[2];
      if (MBVERTEX == etype) {
        nn2[0] = conn3[0];
        nn2[1] = conn3[1];
        if (nn2[0] == currentBoundary)
          nn2[0] = conn3[2];
        if (nn2[1] == currentBoundary)
          nn2[1] = conn3[2];
        // get coordinates
        CartVect Pt[2];

        rval = _mbImpl->get_coords(nn2, 2, (double*) &Pt[0]);
        MBERRORR(rval, "Failed to get coordinates");
        // see the intersection
        if (intersect_segment_and_plane_slice(Pt[0], Pt[1], currentPoint, p2,
            Dir, normPlane, intx, param)) {
          // we should stop for loop, and decide if it is edge or vertex
          if (param == 0.)
            currentBoundary = nn2[0];
          else {
            if (param == 1.)
              currentBoundary = nn2[1];
            else // param between 0 and 1, so edge
            {
              //find the edge between vertices
              std::vector<EntityHandle> edges1;
              // change the create flag to true, because that edge must exist in current triangle
              // if we want to advance; nn2 are 2 nodes in current triangle!!
              rval = _mbImpl->get_adjacencies(nn2, 2, 1, true, edges1,
                  Interface::INTERSECT);
              MBERRORR(rval, "Failed to get edges");
              if (edges1.size() != 1)
                MBERRORR(MB_FAILURE, "Failed to get adjacent edges to 2 nodes");
              currentBoundary = edges1[0];
            }
          }
          visitedTriangles.insert(tri);
          currentPoint = intx;
          points.push_back(intx);
          entities.push_back(currentBoundary);
          triangles.push_back(tri);
          if (debug_splits)
            std::cout << "vtx new tri : " << _mbImpl->id_from_handle(tri)
                << " type bdy:" << _mbImpl->type_from_handle(currentBoundary)
                << "\n";
          break; // out of for loop over triangles

        }
      } else // this is MBEDGE, we have the other triangle to try out
      {
        //first find the nodes from existing boundary
        int nnodes2;
        const EntityHandle * conn2;
        rval = _mbImpl->get_connectivity(currentBoundary, conn2, nnodes2);
        MBERRORR(rval, "Failed to get connectivity");
        int thirdIndex = -1;
        for (int tj = 0; tj < 3; tj++) {
          if ((conn3[tj] != conn2[0]) && (conn3[tj] != conn2[1])) {
            thirdIndex = tj;
            break;
          }
        }
        if (-1 == thirdIndex)
          MBERRORR(MB_FAILURE, " can't get third node");
        CartVect Pt[3];
        rval = _mbImpl->get_coords(conn3, 3, (double*) &Pt[0]);
        MBERRORR(rval, "Failed to get coords");
        int indexFirst = (thirdIndex + 1) % 3;
        int indexSecond = (thirdIndex + 2) % 3;
        int index[2] = { indexFirst, indexSecond };
        for (int k = 0; k < 2; k++) {
          nn2[0] = conn3[index[k]], nn2[1] = conn3[thirdIndex];
          if (intersect_segment_and_plane_slice(Pt[index[k]], Pt[thirdIndex],
              currentPoint, p2, Dir, normPlane, intx, param)) {
            // we should stop for loop, and decide if it is edge or vertex
            if (param == 0.)
              currentBoundary = conn3[index[k]];//it is not really possible
            else {
              if (param == 1.)
                currentBoundary = conn3[thirdIndex];
              else // param between 0 and 1, so edge is fine
              {
                //find the edge between vertices
                std::vector<EntityHandle> edges1;
                // change the create flag to true, because that edge must exist in current triangle
                     // if we want to advance; nn2 are 2 nodes in current triangle!!
                rval = _mbImpl->get_adjacencies(nn2, 2, 1, true, edges1,
                    Interface::INTERSECT);
                MBERRORR(rval, "Failed to get edges");
                if (edges1.size() != 1)
                  MBERRORR(MB_FAILURE, "Failed to get adjacent edges to 2 nodes");
                currentBoundary = edges1[0];
              }
            }
            visitedTriangles.insert(tri);
            currentPoint = intx;
            points.push_back(intx);
            entities.push_back(currentBoundary);
            triangles.push_back(tri);
            if (debug_splits)
            {
              std::cout << "edge new tri : " << _mbImpl->id_from_handle(tri)
                  << "  type bdy: " << _mbImpl->type_from_handle(
                  currentBoundary) << " id: " << _mbImpl->id_from_handle(currentBoundary) << "\n";
              _mbImpl->list_entity(currentBoundary);
            }
            break; // out of for loop over triangles

          }
          // we should not reach here
        }

      }

    }
    /*if (j==adj_tri.size())
     {
     MBERRORR(MB_FAILURE, "did not advance");
     }*/
    vect = p2 - currentPoint;

  }


  if (debug_splits)
    std::cout << "nb entities: " << entities.size() <<  " triangles:" << triangles.size() <<
     " points.size(): " << points.size() <<  "\n";

  return MB_SUCCESS;
}

ErrorCode  FBEngine::split_edge_at_point(EntityHandle edge, CartVect & point,
    EntityHandle & new_edge)
{
  //return MB_NOT_IMPLEMENTED;
  // first, we need to find the closest point on the smooth edge, then
  // split the smooth edge, then call the split_edge_at_mesh_node
  // or maybe just find the closest node??
  // first of all, we need to find a point on the smooth edge, close by
  // then split the mesh edge (if needed)
  // this would be quite a drama, as the new edge has to be inserted in
  // order for proper geo edge definition

  // first of all, find a closest point
  // usually called for
  if (debug_splits)
  {
    std::cout<<"Split edge " << _mbImpl->id_from_handle(edge) << " at point:" <<
        point << "\n";
  }
  int dim = _my_geomTopoTool->dimension(edge);
  if (dim !=1)
    return MB_FAILURE;
  if (!_smooth)
    return MB_FAILURE; // call it only for smooth option...
  // maybe we should do something for linear too

  SmoothCurve * curve = this->_edges[edge];
  EntityHandle closeNode;
  int edgeIndex;
  double u = curve-> u_from_position(point[0], point[1], point[2],
      closeNode, edgeIndex) ;
  if (0==closeNode)
  {
    // we really need to split an existing edge
    // do not do that yet
    // evaluate from u:
    /*double pos[3];
    curve->position_from_u(u,  pos[0], pos[1], pos[2] );*/
    // create a new node here, and split one edge
    // change also connectivity, create new triangles on both sides ...
    std::cout << "not found a close node,  u is: " << u << " edge index: " <<
        edgeIndex << "\n";
    return MB_FAILURE;// not implemented yet
  }
  return split_edge_at_mesh_node(edge, closeNode, new_edge);

}

ErrorCode FBEngine::split_edge_at_mesh_node(EntityHandle edge, EntityHandle node,
    EntityHandle & new_edge)
{
  // the node should be in the list of nodes

  int dim = _my_geomTopoTool->dimension(edge);
  if (dim!=1)
    return MB_FAILURE; // not an edge

  if (debug_splits)
  {
    std::cout<<"Split edge " << _mbImpl->id_from_handle(edge) << " with global id: "<<
        _my_geomTopoTool->global_id(edge) << " at node:" <<
        _mbImpl->id_from_handle(node) << "\n";
  }

  // now get the edges
  // the order is important...
  // these are ordered sets !!
  std::vector<EntityHandle> ents;
  ErrorCode rval = _mbImpl->get_entities_by_type(edge, MBEDGE, ents);
  if (MB_SUCCESS != rval)
    return rval;
  if (ents.size() < 1)
    return MB_FAILURE; // no edges

  const EntityHandle* conn = NULL;
  int len;
  // find the edge connected to the splitting node
  int num_mesh_edges = (int)ents.size();
  int index_edge;
  EntityHandle firstNode = 0;
  for (index_edge = 0; index_edge<num_mesh_edges; index_edge++)
  {
    rval = MBI->get_connectivity(ents[index_edge], conn, len);
    if (MB_SUCCESS != rval)
      return rval;
    if (index_edge == 0)
      firstNode = conn[0]; // will be used to decide vertex sets adjacencies
    if (conn[0] == node)
    {
      if (index_edge==0)
      {
        new_edge = 0; // no need to split, it is the first node
        return MB_SUCCESS; // no split
      }
      else
        return MB_FAILURE; // we should have found the node already , wrong
                           // connectivity
    }
    else if (conn[1] == node)
    {
      // we found the index of interest
      break;
    }
  }
  if (index_edge==num_mesh_edges-1)
  {
    new_edge = 0; // no need to split, it is the last node
    return MB_SUCCESS; // no split
  }

  // here, we have 0 ... index_edge edges in the first set,
  // create a vertex set and add the node to it

  if (debug_splits)
  {
    std::cout<<"Split edge with " << num_mesh_edges << " mesh edges, at index (0 based) " <<
        index_edge << "\n";
  }

  // at this moment, the node set should have been already created in new_geo_edge
  EntityHandle nodeSet; // the node set that has the node (find it!)
  bool found=find_vertex_set_for_node(node, nodeSet);

  if (!found) {
    // create a node set and add the node

    // must be an error, but create one nevertheless
    rval = _mbImpl->create_meshset(MESHSET_SET, nodeSet);
    MBERRORR(rval, "Failed to create a new vertex set");

    rval = _mbImpl->add_entities(nodeSet, &node, 1);
    MBERRORR(rval, "Failed to add the node to the set");

    rval = _my_geomTopoTool->add_geo_set(nodeSet, 0);//
    MBERRORR(rval, "Failed to commit the node set");

    if (debug_splits)
    {
      std::cout<<" create a vertex set (this should have been created before!)" << _mbImpl->id_from_handle(nodeSet) << " global id:"<<
          this->_my_geomTopoTool->global_id(nodeSet) <<  "\n";
    }
  }

  // we need to remove the remaining mesh edges from first set, and add it
  // to the second set, in order

  rval = _mbImpl->create_meshset(MESHSET_ORDERED, new_edge);
  MBERRORR(rval, "can't create geo edge");

  int remaining= num_mesh_edges - 1 - index_edge;

  // add edges to the edge set
  rval = _mbImpl->add_entities(new_edge, &ents[index_edge+1], remaining);
  MBERRORR(rval, "can't add edges to the new edge");

  // also, remove the second node set from old edge
  // remove the edges index_edge+1 and up

  rval = _mbImpl->remove_entities(edge, &ents[index_edge+1], remaining);
  MBERRORR(rval, "can't remove edges from the old edge");

  // need to find the adjacent vertex sets
  Range vertexRange;
  rval = getAdjacentEntities(edge, 0, vertexRange);

  EntityHandle secondSet;
  if (vertexRange.size() == 1)
  {
    // initially a periodic edge, OK to add the new set to both edges, and the
    // second set
    secondSet = vertexRange[0];
  }
  else
  {
    if (vertexRange.size() > 2)
      return MB_FAILURE; // something must be wrong with too many vertices
    // find first node
    int k;
    for (k=0; k<2; k++)
    {
      Range verts;
      rval = _mbImpl->get_entities_by_type(vertexRange[k], MBVERTEX, verts);

      MBERRORR(rval, "can't get vertices from vertex set");
      if (verts.size()!=1)
         MBERRORR(MB_FAILURE, " node set not defined well");
      if (firstNode == verts[0])
      {
        secondSet = vertexRange[1-k]; // the other set; it is 1 or 0
        break;
      }
    }
    if (k>=2)
    {
      // it means we didn't find the right node set
      MBERRORR(MB_FAILURE, " can't find the right vertex");
    }
    // remove the second set from the connectivity with the
    //  edge (parent-child relation)
    //remove_parent_child( EntityHandle parent,
     //                                          EntityHandle child )
    rval = _mbImpl->remove_parent_child(edge, secondSet);
    MBERRORR(rval, " can't remove the second vertex from edge");
  }
  // at this point, we just need to add to both edges the new node set (vertex)
  rval = _mbImpl->add_parent_child(edge, nodeSet);
  MBERRORR(rval, " can't add new vertex to old edge");

  rval = _mbImpl->add_parent_child(new_edge, nodeSet);
  MBERRORR(rval, " can't add new vertex to new edge");

  // one thing that I forgot: add the secondSet as a child to new edge!!!
  // (so far, the new edge has only one end vertex!)
  rval = _mbImpl->add_parent_child(new_edge, secondSet);
  MBERRORR(rval, " can't add second vertex to new edge");

// now, add the edge and vertex to geo tool

  rval = _my_geomTopoTool->add_geo_set(new_edge, 1);
  MBERRORR(rval, " can't add new edge");

  // next, get the adjacent faces to initial edge, and add them as parents
  // to the new edge

  // need to find the adjacent face sets
  Range faceRange;
  rval = getAdjacentEntities(edge, 2, faceRange);

  // these faces will be adjacent to the new edge too!
  // need to set the senses of new edge within faces

  for (Range::iterator it= faceRange.begin(); it!=faceRange.end(); ++it)
  {
    EntityHandle face = *it;
    rval = _mbImpl->add_parent_child(face, new_edge);
    MBERRORR(rval, " can't add new edge - face parent relation");
    int sense;
    rval = _my_geomTopoTool->get_sense(edge, face, sense);
    MBERRORR(rval, " can't get initial sense of edge in the adjacent face");
    // keep the same sense for the new edge within the faces
    rval = _my_geomTopoTool->set_sense(new_edge, face, sense);
    MBERRORR(rval, " can't set sense of new edge in the adjacent face");
  }

  return MB_SUCCESS;
}

ErrorCode FBEngine::split_bedge_at_new_mesh_node(EntityHandle edge, EntityHandle node, EntityHandle brokenEdge,
      EntityHandle & new_edge)
{
  // the node should be in the list of nodes

  int dim = _my_geomTopoTool->dimension(edge);
  if (dim != 1)
    return MB_FAILURE; // not an edge

  if (debug_splits) {
    std::cout << "Split edge " << _mbImpl->id_from_handle(edge)
        << " with global id: " << _my_geomTopoTool->global_id(edge)
        << " at new node:" << _mbImpl->id_from_handle(node) << "breaking mesh edge" <<
              _mbImpl->id_from_handle(brokenEdge)<< "\n";
  }

  // now get the edges
  // the order is important...
  // these are ordered sets !!
  std::vector<EntityHandle> ents;
  ErrorCode rval = _mbImpl->get_entities_by_type(edge, MBEDGE, ents);
  if (MB_SUCCESS != rval)
    return rval;
  if (ents.size() < 1)
    return MB_FAILURE; // no edges

  const EntityHandle* conn = NULL;
  int len;
  // the edge connected to the splitting node is brokenEdge
  // find the small edges it is broken into, which are connected to
  // new vertex (node) and its ends; also, if necessary, reorient these small edges
  // for proper orientation
  rval = _mbImpl->get_connectivity(brokenEdge, conn, len);
  MBERRORR(rval, "Failed to get connectivity of broken edge");

  // we already created the new edges, make sure they are oriented fine; if not, revert them
  EntityHandle conn02[]={conn[0], node}; // first node and new node
  // default, intersect
  std::vector<EntityHandle> adj_edges;
  rval = _mbImpl->get_adjacencies(conn02, 2, 1, false, adj_edges);
  if (adj_edges.size()<1 || rval !=MB_SUCCESS)
    MBERRORR(MB_FAILURE, " Can't find small split edge");

  // get this edge connectivity;
  EntityHandle firstEdge=adj_edges[0];
  const EntityHandle * connActual = NULL;
  rval = _mbImpl->get_connectivity(firstEdge, connActual, len);
  MBERRORR(rval, "Failed to get connectivity of first split edge");
  // if it is the same as conn02, we are happy
  if (conn02[0]!=connActual[0])
  {
    // reset connectivity of edge
    rval = _mbImpl->set_connectivity(firstEdge, conn02, 2);
    MBERRORR(rval, "Failed to reset connectivity of first split edge");
  }
  //  now treat the second edge
  adj_edges.clear(); //
  EntityHandle conn21[]={node, conn[1]}; //  new node and second node
  rval = _mbImpl->get_adjacencies(conn21, 2, 1, false, adj_edges);
  if (adj_edges.size()<1 || rval !=MB_SUCCESS)
    MBERRORR(MB_FAILURE, " Can't find second small split edge");

  // get this edge connectivity;
  EntityHandle secondEdge=adj_edges[0];
  rval = _mbImpl->get_connectivity(firstEdge, connActual, len);
  MBERRORR(rval, "Failed to get connectivity of first split edge");
  // if it is the same as conn21, we are happy
  if (conn21[0]!=connActual[0])
  {
    // reset connectivity of edge
    rval = _mbImpl->set_connectivity(secondEdge, conn21, 2);
    MBERRORR(rval, "Failed to reset connectivity of first split edge");
  }

  int num_mesh_edges = (int) ents.size();
  int index_edge;// this is the index of the edge that will be removed (because it is split)
  // the rest of edges will be put in order in the (remaining) edge and new edge

  for (index_edge = 0; index_edge < num_mesh_edges; index_edge++)
    if (brokenEdge==ents[index_edge])
      break;
  if (index_edge>=num_mesh_edges)
    MBERRORR(MB_FAILURE, "can't find the broken edge");

  //so the edges up to index_edge and firstEdge, will form the "old" edge
  // the edges secondEdge and from index_edge+1 to end will form the new_edge

  // here, we have 0 ... index_edge edges in the first set,
  // create a vertex set and add the node to it

  if (debug_splits) {
    std::cout << "Split edge with " << num_mesh_edges
        << " mesh edges, at index (0 based) " << index_edge << "\n";
  }

  // at this moment, the node set should have been already created in new_geo_edge
  EntityHandle nodeSet; // the node set that has the node (find it!)
  bool found = find_vertex_set_for_node(node, nodeSet);

  if (!found) {
    // create a node set and add the node

    // must be an error, but create one nevertheless
    rval = _mbImpl->create_meshset(MESHSET_SET, nodeSet);
    MBERRORR(rval, "Failed to create a new vertex set");

    rval = _mbImpl->add_entities(nodeSet, &node, 1);
    MBERRORR(rval, "Failed to add the node to the set");

    rval = _my_geomTopoTool->add_geo_set(nodeSet, 0);//
    MBERRORR(rval, "Failed to commit the node set");

    if (debug_splits) {
      std::cout
          << " create a vertex set (this should have been created before!)"
          << _mbImpl->id_from_handle(nodeSet) << " global id:"
          << this->_my_geomTopoTool->global_id(nodeSet) << "\n";
    }
  }

  // we need to remove the remaining mesh edges from first set, and add it
  // to the second set, in order

  rval = _mbImpl->create_meshset(MESHSET_ORDERED, new_edge);
  MBERRORR(rval, "can't create geo edge");

  int remaining = num_mesh_edges - 1 - index_edge;

  // add edges to the new edge set
  rval = _mbImpl->add_entities(new_edge, &secondEdge, 1); // add the second split edge to new edge
  MBERRORR(rval, "can't add second split edge to the new edge");
  // then add the rest
  rval = _mbImpl->add_entities(new_edge, &ents[index_edge + 1], remaining);
  MBERRORR(rval, "can't add edges to the new edge");

  // also, remove the second node set from old edge
  // remove the edges index_edge and up

  rval = _mbImpl->remove_entities(edge, &ents[index_edge], remaining+1);// include the
  MBERRORR(rval, "can't remove edges from the old edge");
  // add the firstEdge too
  rval = _mbImpl->add_entities(edge, &firstEdge, 1); // add the second split edge to new edge
  MBERRORR(rval, "can't add first split edge to the old edge");

  // need to find the adjacent vertex sets
  Range vertexRange;
  rval = getAdjacentEntities(edge, 0, vertexRange);

  EntityHandle secondSet;
  if (vertexRange.size() == 1) {
    // initially a periodic edge, OK to add the new set to both edges, and the
    // second set
    secondSet = vertexRange[0];
  } else {
    if (vertexRange.size() > 2)
      return MB_FAILURE; // something must be wrong with too many vertices
    // find first node
    EntityHandle firstNode;

    rval = MBI->get_connectivity(ents[0], conn, len);
    if (MB_SUCCESS != rval)
      return rval;
    firstNode = conn[0]; // this is the first node of the initial edge
                         // we will use it to identify the vertex set associated with it
    int k;
    for (k = 0; k < 2; k++) {
      Range verts;
      rval = _mbImpl->get_entities_by_type(vertexRange[k], MBVERTEX, verts);

      MBERRORR(rval, "can't get vertices from vertex set");
      if (verts.size() != 1)
        MBERRORR(MB_FAILURE, " node set not defined well");
      if (firstNode == verts[0]) {
        secondSet = vertexRange[1 - k]; // the other set; it is 1 or 0
        break;
      }
    }
    if (k >= 2) {
      // it means we didn't find the right node set
      MBERRORR(MB_FAILURE, " can't find the right vertex");
    }
    // remove the second set from the connectivity with the
    //  edge (parent-child relation)
    //remove_parent_child( EntityHandle parent,
    //                                          EntityHandle child )
    rval = _mbImpl->remove_parent_child(edge, secondSet);
    MBERRORR(rval, " can't remove the second vertex from edge");
  }
  // at this point, we just need to add to both edges the new node set (vertex)
  rval = _mbImpl->add_parent_child(edge, nodeSet);
  MBERRORR(rval, " can't add new vertex to old edge");

  rval = _mbImpl->add_parent_child(new_edge, nodeSet);
  MBERRORR(rval, " can't add new vertex to new edge");

  // one thing that I forgot: add the secondSet as a child to new edge!!!
  // (so far, the new edge has only one end vertex!)
  rval = _mbImpl->add_parent_child(new_edge, secondSet);
  MBERRORR(rval, " can't add second vertex to new edge");

  // now, add the edge and vertex to geo tool

  rval = _my_geomTopoTool->add_geo_set(new_edge, 1);
  MBERRORR(rval, " can't add new edge");

  // next, get the adjacent faces to initial edge, and add them as parents
  // to the new edge

  // need to find the adjacent face sets
  Range faceRange;
  rval = getAdjacentEntities(edge, 2, faceRange);

  // these faces will be adjacent to the new edge too!
  // need to set the senses of new edge within faces

  for (Range::iterator it = faceRange.begin(); it != faceRange.end(); ++it) {
    EntityHandle face = *it;
    rval = _mbImpl->add_parent_child(face, new_edge);
    MBERRORR(rval, " can't add new edge - face parent relation");
    int sense;
    rval = _my_geomTopoTool->get_sense(edge, face, sense);
    MBERRORR(rval, " can't get initial sense of edge in the adjacent face");
    // keep the same sense for the new edge within the faces
    rval = _my_geomTopoTool->set_sense(new_edge, face, sense);
    MBERRORR(rval, " can't set sense of new edge in the adjacent face");
  }

  return MB_SUCCESS;
}

ErrorCode FBEngine::split_boundary(EntityHandle face, EntityHandle atNode)
{
  // find the boundary edges, and split the one that we find it is a part of
  if (debug_splits)
  {
    std::cout<<"Split face " << _mbImpl->id_from_handle(face) << " at node:" <<
        _mbImpl->id_from_handle(atNode) << "\n";
  }
  Range bound_edges;
  ErrorCode rval = getAdjacentEntities(face, 1, bound_edges);MBERRORR(rval, " can't get boundary edges");
  bool brokEdge = _brokenEdges.find(atNode)!=_brokenEdges.end();

  for (Range::iterator it =bound_edges.begin(); it!=bound_edges.end(); ++it)
  {
    EntityHandle b_edge = *it;
    // get all edges in range
    Range mesh_edges;
    rval = _mbImpl->get_entities_by_dimension(b_edge, 1,
        mesh_edges);MBERRORR(rval, " can't get mesh edges");
    if (brokEdge)
    {
      EntityHandle brokenEdge = _brokenEdges[atNode];
      if (mesh_edges.find(brokenEdge)!=mesh_edges.end())
      {
        EntityHandle new_edge;
        rval = split_bedge_at_new_mesh_node(b_edge, atNode, brokenEdge, new_edge);
        return rval;
      }
    }
    else
    {
      Range nodes;
      rval = _mbImpl->get_connectivity(mesh_edges, nodes);MBERRORR(rval, " can't get nodes from mesh edges");

      if (nodes.find(atNode)!=nodes.end())
      {
        // we found our boundary edge candidate
        EntityHandle new_edge;
        rval = split_edge_at_mesh_node(b_edge, atNode, new_edge);
        return rval;
      }
    }
  }
  // if the node was not found in any "current" boundary, it broke an existing
  // boundary edge
  MBERRORR(MB_FAILURE, " we did not find an appropriate boundary edge"); ; //
}

bool FBEngine::find_vertex_set_for_node(EntityHandle iNode, EntityHandle & oVertexSet)
{
  bool found = false;
  Range vertex_sets;

  const int zero = 0;
  const void* const zero_val[] = { &zero };
  Tag geom_tag;
  ErrorCode rval = MBI->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geom_tag);
  if (MB_SUCCESS!=rval)
    return false;
  rval = _mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &geom_tag,
        zero_val, 1, vertex_sets);
  if (MB_SUCCESS!=rval)
    return false;
  // local _gsets, as we might have not updated the local lists
  // see if ends of geo edge generated is in a geo set 0 or not

  for( Range::iterator vsit=vertex_sets.begin(); vsit!=vertex_sets.end(); ++vsit)
  {
    EntityHandle vset=*vsit;
    // is the node part of this set?
    if (_mbImpl->contains_entities(vset, &iNode, 1))
    {

      found = true;
      oVertexSet = vset;
      break;
    }
  }
  return found;
}
ErrorCode FBEngine::redistribute_boundary_edges_to_faces(EntityHandle face, EntityHandle newFace,
      std::vector<EntityHandle> & chainedEdges)
{

  // so far, original boundary edges are all parent/child relations for face
  // we should get them all, and see if they are truly adjacent to face or newFace
  // decide also on the orientation/sense with respect to the triangles
  Range r1; // range in old face
  Range r2; // range of tris in new face
  ErrorCode rval = _mbImpl->get_entities_by_dimension(face, 2, r1);
  MBERRORR(rval, " can't get triangles from old face");
  rval = _mbImpl->get_entities_by_dimension(newFace, 2, r2);
  MBERRORR(rval, " can't get triangles from new face");
  // right now, all boundary edges are children of face
  // we need to get them all, and verify if they indeed are adjacent to face
  Range children;
  rval = _mbImpl->get_child_meshsets(face, children);// only direct children are of interest
  MBERRORR(rval, " can't get children sets from face");

  for (Range::iterator it = children.begin(); it!=children.end(); ++it)
  {
    EntityHandle edge=*it;
    if (std::find(chainedEdges.begin(), chainedEdges.end(), edge)!=chainedEdges.end())
      continue; // we already set this one fine
    // get one mesh edge from the edge; we have to get all of them!!
    if (_my_geomTopoTool->dimension(edge)!=1)
      continue; // not an edge
    Range mesh_edges;
    rval = _mbImpl->get_entities_by_handle(edge, mesh_edges);
    MBERRORR(rval, " can't get mesh edges from edge");
    if (mesh_edges.empty())
      MBERRORR(MB_FAILURE, " no mesh edges");
    EntityHandle mesh_edge = mesh_edges[0]; // first one is enough
    //get its triangles; see which one is in r1 or r2; it should not be in both
    Range triangles;
    rval = _mbImpl->get_adjacencies(&mesh_edge, 1, 2, false, triangles);
    MBERRORR(rval, " can't get adjacent triangles");
    Range intx1 = intersect(triangles, r1);
    Range intx2 = intersect(triangles, r2);
    if (!intx1.empty() && !intx2.empty())
      MBERRORR(MB_FAILURE, " at least one should be empty");

    if (intx2.empty())
    {
      // it means it is in the range r1; the sense should be fine, no need to reset
      // the sense should have been fine, also
      continue;
    }
    // so it must be a triangle in r2;
    EntityHandle triangle = intx2[0];// one triangle only
    // remove the edge from boundary of face, and add it to the boundary of newFace
    // remove_parent_child( EntityHandle parent,  EntityHandle child )
    rval = _mbImpl->remove_parent_child(face, edge);
    MBERRORR(rval, " can't remove parent child relation for edge");
    // add to the new face
    rval = _mbImpl->add_parent_child(newFace, edge);
    MBERRORR(rval, " can't add parent child relation for edge");

    // set some sense, based on the sense of the mesh_edge in triangle
    int num1, sense, offset;
    rval = _mbImpl->side_number(triangle, mesh_edge, num1, sense, offset);
    MBERRORR(rval, "mesh edge not adjacent to triangle");

    rval = this->_my_geomTopoTool->set_sense(edge, newFace, sense);
    MBERRORR(rval, "can't set proper sense of edge in face");

  }

  return MB_SUCCESS;
}

ErrorCode FBEngine::set_neumann_tags(EntityHandle face, EntityHandle newFace)
{
  // these are for debugging purposes only
  // check the initial tag, then
  Tag ntag;
  ErrorCode rval = _mbImpl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER, ntag);
  MBERRORR(rval, "can't get tag handle");
  // check the value for face
  int nval;
  rval = _mbImpl->tag_get_data(ntag, &face, 1, &nval);
  if (MB_SUCCESS == rval)
  {
    nval++;
  }
  else
  {
    nval = 1;
    rval = _mbImpl->tag_set_data(ntag, &face, 1, &nval);
    MBERRORR(rval, "can't set tag");
    nval = 2;
  }
  rval = _mbImpl->tag_set_data(ntag, &newFace, 1, &nval);
  MBERRORR(rval, "can't set tag");

  return MB_SUCCESS;
}

// split the quads if needed; it will create a new gtt, which will
// contain triangles instead of quads
ErrorCode FBEngine::split_quads()
{
  // first see if we have any quads in the 2d faces
  //  _my_gsets[2] is a range of surfaces (moab sets of dimension 2)
  int num_quads=0;
  for (Range::iterator it=_my_gsets[2].begin(); it!=_my_gsets[2].end(); ++it)
  {
    EntityHandle surface = *it;
    int num_q=0;
    _mbImpl->get_number_entities_by_type(surface, MBQUAD, num_q);
    num_quads+=num_q;
  }

  if (num_quads==0)
    return MB_SUCCESS; // nothing to do

  GeomTopoTool * new_gtt = NULL;
  ErrorCode rval = _my_geomTopoTool->duplicate_model(new_gtt);
  MBERRORR(rval, "can't duplicate model");
  if (this->_t_created)
    delete _my_geomTopoTool;

  _t_created = true; // this one is for sure created here, even if the original gtt was not

  // if we were using smart pointers, we would decrease the reference to the _my_geomTopoTool, at least
  _my_geomTopoTool = new_gtt;

  // replace the _my_gsets with the new ones, from the new set
  _my_geomTopoTool->find_geomsets(_my_gsets);

  // we have duplicated now the model, and the quads are part of the new _my_gsets[2]
  // we will split them now, by creating triangles along the smallest diagonal
  // maybe we will come up with a better criteria, but for the time being, it should be fine.
  // what we will do: we will remove all quads from the surface sets, and split them

  for (Range::iterator it2=_my_gsets[2].begin(); it2!=_my_gsets[2].end(); ++it2)
  {
    EntityHandle surface = *it2;
    Range quads;
    rval = _mbImpl->get_entities_by_type(surface, MBQUAD, quads);
    MBERRORR(rval, "can't get quads from the surface set");
    rval = _mbImpl->remove_entities(surface, quads);
    MBERRORR(rval, "can't remove quads from the surface set"); // they are not deleted, just removed from the set
    for (Range::iterator it=quads.begin(); it!=quads.end(); ++it)
    {
      EntityHandle quad = *it;
      int nnodes;
      const EntityHandle * conn;
      rval = _mbImpl->get_connectivity(quad, conn, nnodes);
      MBERRORR(rval, "can't get quad connectivity");
      // get all vertices position, to see which one is the shortest diagonal
      CartVect pos[4];
      rval = _mbImpl->get_coords(conn, 4, (double*) &pos[0]);
      MBERRORR(rval, "can't get node coordinates");
      bool diag1 = ( (pos[2]-pos[0]).length_squared() < (pos[3]-pos[1]).length_squared() );
      EntityHandle newTris[2];
      EntityHandle tri1[3]= { conn[0], conn[1], conn[2] };
      EntityHandle tri2[3]= { conn[0], conn[2], conn[3] };
      if (!diag1)
      {
        tri1[2] = conn[3];
        tri2[0] = conn[1];
      }
      rval = _mbImpl->create_element(MBTRI, tri1, 3, newTris[0]);
      MBERRORR(rval, "can't create triangle 1");
      rval = _mbImpl->create_element(MBTRI, tri2, 3, newTris[1]);
      MBERRORR(rval, "can't create triangle 2");
      rval = _mbImpl->add_entities(surface, newTris, 2);
      MBERRORR(rval, "can't add triangles to the set");
    }
    //
  }
  return MB_SUCCESS;
}
ErrorCode FBEngine::boundary_mesh_edges_on_face(EntityHandle face, Range & boundary_mesh_edges)
{
  // this list is used only for finding the intersecting mesh edge for starting the
  // polygonal cutting line at boundary (if !closed)
  Range bound_edges;
  ErrorCode rval = getAdjacentEntities(face, 1, bound_edges);
  MBERRORR(rval, " can't get boundary edges");
  for (Range::iterator it =bound_edges.begin(); it!=bound_edges.end(); ++it)
  {
    EntityHandle b_edge = *it;
    // get all edges in range
    //Range mesh_edges;
    rval = _mbImpl->get_entities_by_dimension(b_edge, 1,
       boundary_mesh_edges);
    MBERRORR(rval, " can't get mesh edges");
  }
  return MB_SUCCESS;
}
ErrorCode FBEngine::boundary_nodes_on_face(EntityHandle face, std::vector<EntityHandle> & boundary_nodes)
{
  // even if we repeat some nodes, it is OK
  // this list is used only for finding the closest boundary node for starting the
  // polygonal cutting line at boundary (if !closed)
  Range bound_edges;
  ErrorCode rval = getAdjacentEntities(face, 1, bound_edges);
  MBERRORR(rval, " can't get boundary edges");
  Range b_nodes;
  for (Range::iterator it =bound_edges.begin(); it!=bound_edges.end(); ++it)
  {
    EntityHandle b_edge = *it;
    // get all edges in range
    Range mesh_edges;
    rval = _mbImpl->get_entities_by_dimension(b_edge, 1,
        mesh_edges);
    MBERRORR(rval, " can't get mesh edges");
    rval = _mbImpl->get_connectivity(mesh_edges, b_nodes);
    MBERRORR(rval, " can't get nodes from mesh edges");
  }
  // create now a vector based on Range of nodes
  boundary_nodes.resize(b_nodes.size());
  std::copy(b_nodes.begin(), b_nodes.end(), boundary_nodes.begin());
  return MB_SUCCESS;
}
// used for splitting an edge
ErrorCode FBEngine::split_internal_edge(EntityHandle & edge, EntityHandle & newVertex)
{
  // split the edge, and form 4 new triangles and 2 edges
  // get 2 triangles adjacent to edge
  Range adj_tri;
  ErrorCode rval =  _mbImpl->get_adjacencies(&edge, 1,
                2, false, adj_tri);
  MBERRORR(rval, "can't get adj_tris");
  adj_tri = subtract(adj_tri, _piercedTriangles);
  if (adj_tri.size()>=3)
  {
    std::cout<< "WARNING: non manifold geometry. Are you sure?";
  }
  for (Range::iterator it=adj_tri.begin(); it!=adj_tri.end(); ++it)
  {
    EntityHandle tri = *it;
    _piercedTriangles.insert(tri);
    const EntityHandle * conn3;
    int nnodes;
    rval = _mbImpl->get_connectivity(tri, conn3, nnodes); 
    MBERRORR(rval, "can't get nodes");
    int num1, sense, offset;
    rval = _mbImpl->side_number(tri, edge, num1, sense, offset);
    MBERRORR(rval, "can't get side number");
    // after we know the side number, we can split in 2 triangles
    // node i is opposite to edge i
    int num2 = (num1+1)%3;
    int num3 = (num2+1)%3;
    // the edge from num1 to num2 is split into 2 edges
    EntityHandle t1[]={conn3[num2], conn3[num3], newVertex};
    EntityHandle t2[]={conn3[num1], newVertex, conn3[num3]};
    EntityHandle newTriangle, newTriangle2;
    rval = _mbImpl->create_element(MBTRI, t1, 3, newTriangle);
    MBERRORR(rval, "can't create triangle");
    _newTriangles.insert(newTriangle);
    rval = _mbImpl->create_element(MBTRI, t2, 3, newTriangle2);
    MBERRORR(rval, "can't create triangle");
    _newTriangles.insert(newTriangle2);
    // create edges with this, indirectly
    std::vector<EntityHandle> edges0;
    rval = _mbImpl->get_adjacencies(&newTriangle, 1, 1, true, edges0);
    MBERRORR(rval, "can't get new edges");
    edges0.clear();
    rval = _mbImpl->get_adjacencies(&newTriangle2, 1, 1, true, edges0);
    MBERRORR(rval, "can't get new edges");
    if (debug_splits)
    {
      std::cout<<"2 (out of 4) triangles formed:\n";
      _mbImpl->list_entity(newTriangle);
      print_debug_triangle(newTriangle);
      _mbImpl->list_entity(newTriangle2);
      print_debug_triangle(newTriangle2);
    }
  }
  return MB_SUCCESS;
}
  // triangle split
ErrorCode FBEngine::divide_triangle(EntityHandle triangle, EntityHandle & newVertex)
{
  // 
  _piercedTriangles.insert(triangle);
  int nnodes = 0;
  const EntityHandle * conn3 = NULL;
  ErrorCode  rval = _mbImpl->get_connectivity(triangle, conn3, nnodes); 
  MBERRORR(rval, "can't get nodes");
  EntityHandle t1[]={conn3[0], conn3[1], newVertex};
  EntityHandle t2[]={conn3[1], conn3[2], newVertex};
  EntityHandle t3[]={conn3[2], conn3[0], newVertex};
  EntityHandle newTriangle, newTriangle2, newTriangle3;
  rval = _mbImpl->create_element(MBTRI, t1, 3, newTriangle);
  MBERRORR(rval, "can't create triangle");
  _newTriangles.insert(newTriangle);
  rval = _mbImpl->create_element(MBTRI, t2, 3, newTriangle3);
  MBERRORR(rval, "can't create triangle");
  _newTriangles.insert(newTriangle3);
  rval = _mbImpl->create_element(MBTRI, t3, 3, newTriangle2);
  MBERRORR(rval, "can't create triangle");
  _newTriangles.insert(newTriangle2);

  // create all edges
  std::vector<EntityHandle> edges0;
  rval = _mbImpl->get_adjacencies(&newTriangle, 1, 1, true, edges0);
  MBERRORR(rval, "can't get new edges");
  edges0.clear();
  rval = _mbImpl->get_adjacencies(&newTriangle2, 1, 1, true, edges0);
  MBERRORR(rval, "can't get new edges");
  if (debug_splits)
  {
    std::cout<<"3 triangles formed:\n";
    _mbImpl->list_entity(newTriangle);
    print_debug_triangle(newTriangle);
    _mbImpl->list_entity(newTriangle3);
    print_debug_triangle(newTriangle3);
    _mbImpl->list_entity(newTriangle2);
    print_debug_triangle(newTriangle2);
    std::cout<<"original nodes in tri:\n";
    _mbImpl->list_entity(conn3[0]);
    _mbImpl->list_entity(conn3[1]);
    _mbImpl->list_entity(conn3[2]);
  }
  return MB_SUCCESS;
}

ErrorCode FBEngine::create_volume_with_direction(EntityHandle newFace1, EntityHandle newFace2, double * direction,
      EntityHandle & volume)
{

  // MESHSET
  // ErrorCode rval = _mbImpl->create_meshset(MESHSET_ORDERED, new_geo_edge);
  ErrorCode rval = _mbImpl->create_meshset(MESHSET_SET, volume);
  MBERRORR(rval, "can't create volume");

  int volumeMatId = 1;// just give a mat id, for debugging, mostly
  Tag matTag;
  rval = _mbImpl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER, matTag);
  MBERRORR(rval, "can't get material tag");

  rval=_mbImpl->tag_set_data(matTag, &volume, 1, &volumeMatId);
  MBERRORR(rval, "can't set material tag value on volume");

  // get the edges of those 2 faces, and get the vertices of those edges
  // in order, they should be created in the same order (?); check for that, anyway
  rval=_mbImpl->add_parent_child(volume, newFace1);
  MBERRORR(rval, "can't add first face to volume");

  rval=_mbImpl->add_parent_child(volume, newFace2);
  MBERRORR(rval, "can't add second face to volume");

  // first is bottom, so it is negatively oriented
  rval = _my_geomTopoTool->add_geo_set(volume, 3);
  MBERRORR(rval, "can't add volume to the gtt");

  // set senses
  // bottom face is negatively oriented, its normal is toward interior of the volume
  rval = _my_geomTopoTool->set_sense(newFace1, volume, -1);
  MBERRORR(rval, "can't set bottom face sense to the volume");

  // the top face is positively oriented
  rval = _my_geomTopoTool->set_sense(newFace2, volume, 1);
  MBERRORR(rval, "can't set top face sense to the volume");

  // the children should be in the same direction
  //   get the side edges of each face, and form lateral faces, along direction
  std::vector<EntityHandle> edges1;
  std::vector<EntityHandle> edges2;

  rval = _mbImpl->get_child_meshsets(newFace1, edges1); // no hops
  MBERRORR(rval, "can't get children edges or first face, bottom");

  rval = _mbImpl->get_child_meshsets(newFace2, edges2); // no hops
  MBERRORR(rval, "can't get children edges for second face, top");

  if (edges1.size()!=edges2.size())
    MBERRORR(MB_FAILURE, "wrong correspondence ");

  for (unsigned int i = 0; i < edges1.size(); ++i)
  {
    EntityHandle newLatFace;
    rval = weave_lateral_face_from_edges(edges1[i], edges2[i], direction, newLatFace);
    MBERRORR(rval, "can't weave lateral face");
    rval=_mbImpl->add_parent_child(volume, newLatFace);
    MBERRORR(rval, "can't add lateral face to volume");

    // set sense as positive
    rval = _my_geomTopoTool->set_sense(newLatFace, volume, 1);
    MBERRORR(rval, "can't set lateral face sense to the volume");
  }

  rval = set_default_neumann_tags();
  MBERRORR(rval, "can't set new neumann tags");

  return MB_SUCCESS;
}

ErrorCode  FBEngine::get_nodes_from_edge(EntityHandle gedge, std::vector<EntityHandle> & nodes)
{
  std::vector<EntityHandle> ents;
  ErrorCode rval = _mbImpl->get_entities_by_type(gedge, MBEDGE, ents);
  if (MB_SUCCESS != rval)
    return rval;
  if (ents.size() < 1)
    return MB_FAILURE;

  nodes.resize(ents.size() +1);
  const EntityHandle* conn = NULL;
  int len;
  for (unsigned int i=0; i<ents.size(); ++i)
  {
    rval = _mbImpl->get_connectivity(ents[i], conn, len);
    MBERRORR(rval, "can't get edge connectivity");
    nodes[i] = conn[0];
  }
  // the last one is conn[1]
  nodes[ents.size()] = conn[1];
  return MB_SUCCESS;
}
ErrorCode  FBEngine::weave_lateral_face_from_edges(EntityHandle bEdge, EntityHandle tEdge,  double * direction,
      EntityHandle & newLatFace)
{
  // in weird cases might need to create new vertices in the interior;
  // in most cases, it is OK

  ErrorCode rval = _mbImpl->create_meshset(MESHSET_SET, newLatFace);
  MBERRORR(rval, "can't create new lateral face");

  EntityHandle v[4]; // vertex sets
  // bot edge will be v1->v2
  // top edge will be v3->v4
  // we need to create edges from v1 to v3 and from v2 to v4
  std::vector<EntityHandle> adj;
  rval = _mbImpl->get_child_meshsets(bEdge, adj);
  MBERRORR(rval, "can't get children nodes");
  bool periodic = false;
  if (adj.size()==1)
  {
    v[0] = v[1] = adj[0];
    periodic = true;
  }
  else
  {
    v[0]=adj[0];
    v[1]=adj[1];
  }
  int senseB;
  rval = getEgVtxSense( bEdge, v[0], v[1],  senseB );
  MBERRORR(rval, "can't get bottom edge sense");
  if (-1==senseB)
  {
    v[1]=adj[0];// so , bEdge will be oriented from v[0] to v[1], and will start at nodes1, coords1..
    v[0]=adj[1];
  }
  adj.clear();
  rval = _mbImpl->get_child_meshsets(tEdge, adj);
  MBERRORR(rval, "can't get children nodes");
  if (adj.size()==1)
  {
    v[2]=v[3]=adj[0];
    if (!periodic)
      MBERRORR(MB_FAILURE, "top edge is periodic, but bottom edge is not");
  }
  else
  {
    v[2]=adj[0];
    v[3]=adj[1];
    if (periodic)
      MBERRORR(MB_FAILURE, "top edge is not periodic, but bottom edge is");
  }

  // now, using nodes on bottom edge and top edge, create triangles, oriented outwards the
  //  volume (sense positive on bottom edge)
  std::vector<EntityHandle> nodes1;
  rval = get_nodes_from_edge(bEdge, nodes1);
  MBERRORR(rval, "can't get nodes from bott edge");

  std::vector<EntityHandle> nodes2;
  rval = get_nodes_from_edge(tEdge, nodes2);
  MBERRORR(rval, "can't get nodes from top edge");

  std::vector<CartVect> coords1, coords2;
  coords1.resize(nodes1.size());
  coords2.resize(nodes2.size());

  int N1 = (int)nodes1.size();
  int N2 = (int)nodes2.size();

  rval = _mbImpl->get_coords(&(nodes1[0]), nodes1.size(), (double*) &(coords1[0]));
  MBERRORR(rval, "can't get coords of nodes from bott edge");

  rval = _mbImpl->get_coords(&(nodes2[0]), nodes2.size(), (double*) &(coords2[0]));
  MBERRORR(rval, "can't get coords of nodes from top edge");
  CartVect up(direction);

  // see if the start and end coordinates are matching, if not, reverse edge 2 nodes and coordinates
  CartVect v1 = (coords1[0]-coords2[0])*up;
  CartVect v2 = (coords1[0]-coords2[N2-1])*up;
  if (v1.length_squared()>v2.length_squared())
  {
    // we need to reverse coords2 and node2, as nodes are not above each other
    // the edges need to be found between v0 and v3, v1 and v2!
    for (unsigned int k=0 ; k<nodes2.size()/2; k++)
    {
      EntityHandle tmp=nodes2[k];
      nodes2[k] = nodes2[N2-1-k];
      nodes2[N2-1-k] = tmp;
      CartVect tv = coords2[k];
      coords2[k] = coords2[N2-1-k];
      coords2[N2-1-k]= tv;
    }
  }
  // make sure v[2] has nodes2[0], if not, reverse v[2] and v[3]
  if (!_mbImpl->contains_entities(v[2], &(nodes2[0]), 1))
  {
    //reverse v[2] and v[3], so v[2] will be above v[0]
    EntityHandle tmp = v[2];
    v[2] = v[3];
    v[3]= tmp;
  }
  // now we know that v[0]--v[3] will be vertex sets in the order we want
  EntityHandle nd[4]={nodes1[0], nodes1[N1-1], nodes2[0], nodes2[N2-1]};

  adj.clear();
  EntityHandle e1, e2;
  // find edge 1 between v[0] and v[2], and e2 between v[1] and v[3]
  rval=_mbImpl->get_parent_meshsets(v[0], adj);
  MBERRORR(rval, "can't get edges connected to vertex set 1");
  bool found = false;
  for (unsigned int j =0; j< adj.size(); j++)
  {
    EntityHandle ed=adj[j];
    Range vertices;
    rval = _mbImpl->get_child_meshsets(ed, vertices);
    if (vertices.find(v[2])!=vertices.end())
    {
      found = true;
      e1 = ed;
      break;
    }
  }
  if (!found)
  {
    // create an edge from v[0] to v[2]
    rval = _mbImpl->create_meshset(MESHSET_SET, e1);
    MBERRORR(rval, "can't create edge 1");

    rval=_mbImpl->add_parent_child(e1, v[0]);
    MBERRORR(rval, "can't add parent - child relation");

    rval=_mbImpl->add_parent_child(e1, v[2]);
    MBERRORR(rval, "can't add parent - child relation");

    EntityHandle nn2[2] = {nd[0], nd[2]};
    EntityHandle medge;
    rval = _mbImpl->create_element(MBEDGE, nn2, 2, medge);
    MBERRORR(rval, "can't create mesh edge");

    rval = _mbImpl->add_entities(e1, &medge, 1);
    MBERRORR(rval, "can't add mesh edge to geo edge");

    rval = this->_my_geomTopoTool->add_geo_set(e1, 1);
    MBERRORR(rval, "can't add edge to gtt");
  }

  // find the edge from v2 to v4 (if not, create one)
  rval=_mbImpl->get_parent_meshsets(v[1], adj);
  MBERRORR(rval, "can't get edges connected to vertex set 2");
  found = false;
  for (unsigned int i =0; i< adj.size(); i++)
  {
    EntityHandle ed=adj[i];
    Range vertices;
    rval = _mbImpl->get_child_meshsets(ed, vertices);
    if (vertices.find(v[3])!=vertices.end())
    {
      found = true;
      e2 = ed;
      break;
    }
  }
  if (!found)
  {
    // create an edge from v2 to v4
    rval = _mbImpl->create_meshset(MESHSET_SET, e2);
    MBERRORR(rval, "can't create edge 1");

    rval=_mbImpl->add_parent_child(e2, v[1]);
    MBERRORR(rval, "can't add parent - child relation");

    rval=_mbImpl->add_parent_child(e2, v[3]);
    MBERRORR(rval, "can't add parent - child relation");

    EntityHandle nn2[2] = {nd[1], nd[3]};
    EntityHandle medge;
    rval = _mbImpl->create_element(MBEDGE, nn2, 2, medge);
    MBERRORR(rval, "can't create mesh edge");

    rval = _mbImpl->add_entities(e2, &medge, 1);
    MBERRORR(rval, "can't add mesh edge to geo edge");

    rval =  _my_geomTopoTool->add_geo_set(e2, 1);
    MBERRORR(rval, "can't add edge to gtt");
  }

  // now we have the four edges, add them to the face, as children

  // add children to face
  rval=_mbImpl->add_parent_child(newLatFace, bEdge);
  MBERRORR(rval, "can't add parent - child relation");

  rval=_mbImpl->add_parent_child(newLatFace, tEdge);
  MBERRORR(rval, "can't add parent - child relation");

  rval=_mbImpl->add_parent_child(newLatFace, e1);
  MBERRORR(rval, "can't add parent - child relation");

  rval=_mbImpl->add_parent_child(newLatFace, e2);
  MBERRORR(rval, "can't add parent - child relation");

  rval =  _my_geomTopoTool->add_geo_set(newLatFace, 2);
  MBERRORR(rval, "can't add face to gtt");
  // add senses
  //
  rval = _my_geomTopoTool->set_sense(bEdge, newLatFace, 1);
  MBERRORR(rval, "can't set bottom edge sense to the lateral face");

  int Tsense;
  rval = getEgVtxSense( tEdge, v[3], v[2],  Tsense );
  MBERRORR(rval, "can't get top edge sense");
  // we need to see what sense has topEdge in face
  rval = _my_geomTopoTool->set_sense(tEdge, newLatFace, Tsense);
  MBERRORR(rval, "can't set top edge sense to the lateral face");

  rval = _my_geomTopoTool->set_sense(e1, newLatFace, -1);
  MBERRORR(rval, "can't set first vert edge sense");

  rval = _my_geomTopoTool->set_sense(e2, newLatFace, 1);
  MBERRORR(rval, "can't set second edge sense to the lateral face");
  // first, create edges along direction, for the

  int indexB=0, indexT=0;// indices of the current nodes in the weaving process
  // weaving is either up or down; the triangles are oriented positively either way
  // up is nodes1[indexB], nodes2[indexT+1], nodes2[indexT]
  // down is nodes1[indexB], nodes1[indexB+1], nodes2[indexT]
  // the decision to weave up or down is based on which one is closer to the direction normal
  /*
   *
   *     --------*------*-----------*                           ^
   *            /   .    \ .      .           ------> dir1      |  up
   *           /.         \   .  .                              |
   *     -----*------------*----*
   *
   */
  // we have to change the logic to account for cases when the curve in xy plane is not straight
  // (for example, when merging curves with a min_dot < -0.5, which means that the edges
  // could be even closed (periodic), with one vertex
  // the top and bottom curves should follow the same path in the "xy" plane (better to say
  // the plane perpendicular to the direction of weaving)
  // in this logic, the vector dir1 varies along the curve !!!
  CartVect dir1= coords1[1] - coords1[0];// we should have at least 2 nodes, N1>=2!!

  CartVect planeNormal = dir1*up;
  dir1 = up * planeNormal;
  dir1.normalize();
  // this direction will be updated constantly with the direction of last edge added
  bool weaveDown = true;

  CartVect startP = coords1[0]; // used for origin of comparisons
  while(1)
  {
    if ((indexB == N1-1) && (indexT == N2-1))
      break; // we cannot advance anymore
    if (indexB == N1-1)
    {
      weaveDown = false;
    }
    else if (indexT==N2-1)
    {
      weaveDown = true;
    }
    else
    {
      // none are at the end, we have to decide which way to go, based on which  index + 1 is closer
      double proj1 = (coords1[indexB+1]-startP)%dir1;
      double proj2 = (coords2[indexT+1]-startP)%dir1;
      if (proj1 < proj2)
        weaveDown = true;
      else
        weaveDown = false;
    }
    EntityHandle nTri[3] = { nodes1[indexB], 0, nodes2[indexT]};
    if (weaveDown)
    {
      nTri[1] = nodes1[indexB+1];
      nTri[2] = nodes2[indexT];
      indexB++;
    }
    else
    {
      nTri[1] = nodes2[indexT+1];
      indexT++;
    }
    EntityHandle triangle;
    rval = _mbImpl->create_element(MBTRI, nTri, 3, triangle);
    MBERRORR(rval, "can't create triangle");

    rval = _mbImpl->add_entities(newLatFace, &triangle, 1);
    MBERRORR(rval, "can't add triangle to face set");
    if (weaveDown)
    {
      // increase was from nodes1[indexB-1] to nodes1[indexb]
      dir1= coords1[indexB] - coords1[indexB-1];// we should have at least 2 nodes, N1>=2!!
    }
    else
    {
      dir1= coords2[indexT] - coords2[indexT-1];
    }
    dir1 = up * (dir1*up);
    dir1.normalize();

  }
  // we do not check yet if the triangles are inverted
  // if yes, we should correct them. HOW?
  // we probably need a better meshing strategy, one that can overcome really bad meshes.
  // again, these faces are not what we should use for geometry, maybe we should use the
  //  extruded quads, identified AFTER hexa are created.
  // right now, I see only a cosmetic use of these lateral faces
  // the whole idea of volume maybe is overrated
  // volume should be just quads extruded from bottom ?
  //
  return MB_SUCCESS;
}
// this will be used during creation of the volume, to set unique
// tags on all surfaces
// it is changing original tags, so do not use it if you want to preserve
// initial neumann tags
ErrorCode FBEngine::set_default_neumann_tags()
{
  // these are for debugging purposes only
  // check the initial tag, then
  Tag ntag;
  ErrorCode rval = _mbImpl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER, ntag);
  MBERRORR(rval, "can't get tag handle");
  // get all surfaces in the model now
  Range sets[5];
  rval = _my_geomTopoTool->find_geomsets(sets);
  MBERRORR(rval, "can't get geo sets");
  int nfaces = (int)sets[2].size();
  int * vals = new int [nfaces];
  for (int i=0; i<nfaces; i++)
  {
    vals[i] = i+1;
  }
  rval = _mbImpl->tag_set_data(ntag, sets[2], (void*)vals);
  MBERRORR(rval, "can't set tag values for neumann sets");

  delete [] vals;

  return MB_SUCCESS;
}
// a reverse operation for splitting an gedge at a mesh node
ErrorCode FBEngine::chain_edges(double min_dot)
{
  Range sets[5];
  ErrorCode rval;
  while (1)// break out only if no edges are chained
  {
    rval = _my_geomTopoTool->find_geomsets(sets);
    MBERRORR(rval, "can't get geo sets");
    // these gentities are "always" current, while the ones in this-> _my_gsets[0:4] are
    // the "originals" before FBEngine modifications
    int nedges=(int)sets[1].size();
    // as long as we have chainable edges, continue;
    bool chain=false;
    for (int i=0; i<nedges; i++)
    {
      EntityHandle edge=sets[1][i];
      EntityHandle next_edge;
      bool chainable=false;
      rval = chain_able_edge(edge, min_dot, next_edge, chainable);
      MBERRORR(rval, "can't determine chain-ability");
      if ( chainable )
      {
        rval = chain_two_edges(edge, next_edge);
        MBERRORR(rval, "can't chain 2 edges");
        chain = true;
        break; // interrupt for loop
      }
    }
    if (!chain)
    {
      break; // break out of while loop
    }
  }
  return MB_SUCCESS;
}

// determine if from the end of edge we can extend with another edge; return also the
//  extension edge (next_edge)
ErrorCode FBEngine::chain_able_edge(EntityHandle edge, double min_dot,
    EntityHandle & next_edge, bool & chainable)
{
  // get the end, then get all parents of end
  // see if some are the starts of
  chainable = false;
  EntityHandle v1, v2;
  ErrorCode rval = get_vert_edges(edge, v1, v2);
  MBERRORR(rval, "can't get vertices");
  if (v1==v2)
    return MB_SUCCESS;// it is periodic, can't chain it with another edge!

  // v2 is a set, get its parents, which should be edges
  Range edges;
  rval = _mbImpl->get_parent_meshsets(v2, edges);
  MBERRORR(rval, "can't get parents of vertex set");
  // get parents of current edge (faces)
  Range faces;
  rval = _mbImpl->get_parent_meshsets(edge, faces);
  MBERRORR(rval, "can't get parents of edge set");
  // get also the last edge "tangent" at the vertex
  std::vector<EntityHandle> mesh_edges;
  rval = _mbImpl->get_entities_by_type(edge, MBEDGE, mesh_edges);
  MBERRORR(rval, "can't get mesh edges from edge set");
  EntityHandle lastMeshEdge= mesh_edges[mesh_edges.size()-1];
  const EntityHandle * conn2 = NULL;
  int len = 0;
  rval = _mbImpl->get_connectivity(lastMeshEdge, conn2, len);
  MBERRORR(rval, "can't connectivity of last mesh edge");
  // get the coordinates of last edge
  if (len!=2)
    MBERRORR(MB_FAILURE, "bad number of vertices");
  CartVect P[2];
  rval = _mbImpl->get_coords(conn2, len, (double*) &P[0]);
  MBERRORR(rval, "Failed to get coordinates");

  CartVect vec1(P[1] - P[0]);
  vec1.normalize();
  for (Range::iterator edgeIter = edges.begin(); edgeIter!=edges.end(); ++edgeIter)
  {
    EntityHandle otherEdge = *edgeIter;
    if (edge==otherEdge)
      continue;
    // get faces parents of this edge
    Range faces2;
    rval = _mbImpl->get_parent_meshsets(otherEdge, faces2);
    MBERRORR(rval, "can't get parents of other edge set");
    if (faces!=faces2)
      continue;
    // now, if the first mesh edge is within given angle, we can go on
    std::vector<EntityHandle> mesh_edges2;
    rval = _mbImpl->get_entities_by_type(otherEdge, MBEDGE, mesh_edges2);
    MBERRORR(rval, "can't get mesh edges from other edge set");
    EntityHandle firstMeshEdge= mesh_edges2[0];
    const EntityHandle * conn22;
    int len2;
    rval = _mbImpl->get_connectivity(firstMeshEdge, conn22, len2);
    MBERRORR(rval, "can't connectivity of first mesh edge");
    if (len2!=2)
      MBERRORR(MB_FAILURE, "bad number of vertices");
    if (conn2[1]!=conn22[0])
      continue; // the mesh edges are not one after the other
    // get the coordinates of first edge

    //CartVect P2[2];
    rval = _mbImpl->get_coords(conn22, len, (double*) &P[0]);
    CartVect vec2(P[1] - P[0]);
    vec2.normalize();
    if (vec1%vec2 < min_dot)
      continue;
    // we found our edge, victory! we can get out
    next_edge = otherEdge;
    chainable = true;
    return MB_SUCCESS;

  }


  return MB_SUCCESS;// in general, hard to come by chain-able edges
}
ErrorCode FBEngine::chain_two_edges(EntityHandle edge, EntityHandle next_edge)
{
  // the biggest thing is to see the sense tags; or maybe not...
  // they should be correct :)
  // get the vertex sets
  EntityHandle v11, v12, v21, v22;
  ErrorCode rval = get_vert_edges( edge,  v11, v12);
  MBERRORR(rval, "can't get vert sets");
  rval = get_vert_edges( next_edge,  v21, v22);
  MBERRORR(rval, "can't get vert sets");
  assert(v12==v21);
  std::vector<EntityHandle> mesh_edges;
  rval = MBI->get_entities_by_type(next_edge, MBEDGE, mesh_edges);
  MBERRORR(rval, "can't get mesh edges");

  rval = _mbImpl->add_entities(edge, &mesh_edges[0], (int)mesh_edges.size());
  MBERRORR(rval, "can't add new mesh edges");
  // remove the child - parent relation for second vertex of first edge
  rval = _mbImpl->remove_parent_child(edge, v12);
  MBERRORR(rval, "can't remove parent - child relation between first edge and middle vertex");

  if (v22!=v11) // the edge would become periodic, do not add again the relationship
  {
    rval = _mbImpl->add_parent_child(edge, v22);
    MBERRORR(rval, "can't add second vertex to edge ");
  }
  // we can now safely eliminate next_edge
  rval = _mbImpl->remove_parent_child(next_edge, v21);
  MBERRORR(rval, "can't remove child - parent relation ");

  rval = _mbImpl->remove_parent_child(next_edge, v22);
  MBERRORR(rval, "can't remove child - parent relation ");

  // remove the next_edge relation to the faces
  Range faces;
  rval = _mbImpl->get_parent_meshsets(next_edge, faces);
  MBERRORR(rval, "can't get parent faces ");

  for (Range::iterator it=faces.begin(); it!=faces.end(); ++it)
  {
    EntityHandle ff=*it;
    rval = _mbImpl->remove_parent_child(ff, next_edge);
    MBERRORR(rval, "can't remove parent-edge rel ");
  }

  rval = _mbImpl->delete_entities(&next_edge, 1);
  MBERRORR(rval, "can't remove edge set ");

  // delete the vertex set that is idle now (v12 = v21)
  rval = _mbImpl->delete_entities(&v12, 1);
  MBERRORR(rval, "can't remove edge set ");
  return MB_SUCCESS;
}
ErrorCode FBEngine::get_vert_edges(EntityHandle edge, EntityHandle & v1, EntityHandle & v2)
{
  // need to decide first or second vertex
  // important for moab

  Range children;
  //EntityHandle v1, v2;
  ErrorCode rval = _mbImpl->get_child_meshsets(edge, children);
  MBERRORR(rval, "can't get child meshsets");
  if (children.size()==1)
  {
    // this is periodic edge, get out early
    v1 = children[0];
    v2 = v1;
    return MB_SUCCESS;
  }
  else if (children.size()>2)
    MBERRORR(MB_FAILURE, "too many vertices in one edge");
  // edge: get one vertex as part of the vertex set
  Range entities;
  rval = MBI->get_entities_by_type(children[0], MBVERTEX, entities);
  MBERRORR(rval, "can't get entities from vertex set");
  if (entities.size() < 1)
    MBERRORR(MB_FAILURE, "no mesh nodes in vertex set");
  EntityHandle node0 = entities[0]; // the first vertex
  entities.clear();

  // now get the edges, and get the first node and the last node in sequence of edges
  // the order is important...
  // these are ordered sets !!
  std::vector<EntityHandle> ents;
  rval = MBI->get_entities_by_type(edge, MBEDGE, ents);
  MBERRORR(rval, "can't get mesh edges");
  if (ents.size() < 1)
    MBERRORR(MB_FAILURE, "no mesh edges in edge set");

  const EntityHandle* conn = NULL;
  int len;
  rval = MBI->get_connectivity(ents[0], conn, len);
  MBERRORR(rval, "can't connectivity of first mesh edge");

  if (conn[0]==node0)
  {
    v1 = children[0];
    v2 = children[1];
  }
  else // the other way around, although we should check (we are paranoid)
  {
    v2 = children[0];
    v1 = children[1];
  }

  return MB_SUCCESS;
}
} // namespace moab


