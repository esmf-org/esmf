#include "moab/GeomQueryTool.hpp"

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <limits>
#include <algorithm>
#include <set>

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "moab/OrientedBoxTreeTool.hpp"

const bool debug = false;
#ifdef __DEBUG
debug = true;
#endif

namespace moab {


  /** \class GQT_IntRegCtxt
   *
   *\brief An implementation of an Intersection Registration Context for use GQT ray-firing
   *
   * This context uses a variety of tests and conditions to confirm whether or
   * not to accumulate an intersection, to ensure robustness for ray firing.
   *
   * This context only accumulates intersections that are oriented parallel to
   * the 'desiredOrient', if provided, with respect to 'geomVol', using
   * information in the in 'senseTag'.
   *
   * This context only accumulates a single intersection out of a set of
   * multiple intersections that fall in the same 'neighborhood', where a
   * 'neighborhood' is defined as facets that share edges or vertices.
   *
   * This context only accumulates piercing intersections.  This is relevant
   * for intersections that are found to be on an edge or vertex by the
   * Plucker test.  Such intersections are piercing if the ray has the same
   * orientation w.r.t. to all fecets that share that edge or vertex.
   *
   * This context tests intersections against a list of 'prevFacets' to
   * prevent a ray from crossing the same facet more than once.  The user is
   * responsible for ensuring that this list is reset when appropriate.
   *
   * This context accumulates all intersections within 'tol' of the
   * start of the ray and if the number of intersections within the
   * 'tol' of the ray start point is less than 'minTolInt', the next
   * closest intersection. If the desired result is only the closest
   * intersection, 'minTolInt' should be 0.  This function will return all
   * intersections, regardless of distance from the start of the ray, if
   * 'minTolInt' is negative.
   *
   */  

  class GQT_IntRegCtxt : public OrientedBoxTreeTool::IntRegCtxt {

  private:
    // Input
    OrientedBoxTreeTool* tool;
    const CartVect       ray_origin;
    const CartVect       ray_direction;
    const double         tol;             /* used for box.intersect_ray, radius of
                                             neighborhood for adjacent triangles,
                                             and old mode of add_intersection */
    const int            minTolInt;      
  
    // Optional Input - to screen RTIs by orientation and edge/node intersection
    const EntityHandle*  rootSet;         /* used for sphere_intersect */
    const EntityHandle*  geomVol;         /* used for determining surface sense */
    const Tag*           senseTag;        /* allows screening by triangle orientation.
                                             both geomVol and senseTag must be used together. */
    const int*           desiredOrient;   /* points to desired orientation of ray with
                                             respect to surf normal, if this feature is used.
                                             Must point to -1 (reverse) or 1 (forward).
                                             geomVol and senseTag are needed for this feature */
  
    // Optional Input - to avoid returning these as RTIs
    const std::vector<EntityHandle>* prevFacets; /* intersections on these triangles 
                                                    will not be returned */
  
    // Other Variables
    std::vector< std::vector<EntityHandle> > neighborhoods;
    std::vector<EntityHandle> neighborhood;

    void add_intersection( EntityHandle set, EntityHandle tri, double dist, OrientedBoxTreeTool::IntersectSearchWindow &search_win);
    void append_intersection( EntityHandle set, EntityHandle facet, double dist);
    void set_intersection( int len_idx, EntityHandle set, EntityHandle facet, double dist);
    void add_mode1_intersection( EntityHandle set, EntityHandle facet, double dist, OrientedBoxTreeTool::IntersectSearchWindow &search_win);
    bool edge_node_piercing_intersect(const EntityHandle                tri,
                                      const CartVect&                   ray_direction,
                                      const GeomUtil::intersection_type int_type,
                                      const std::vector<EntityHandle>&  close_tris,
                                      const std::vector<int>&           close_senses,
                                      const Interface*                  MBI,
                                      std::vector<EntityHandle>*        neighborhood_tris =0 );

    bool in_prevFacets(const EntityHandle tri);
    bool in_neighborhoods(const EntityHandle tri);
  
  public:

    GQT_IntRegCtxt(OrientedBoxTreeTool* obbtool,
                   const double ray_point[3],
                   const double ray_dir[3],
                   double tolerance,
                   int min_tolerance_intersections,
                   const EntityHandle* root_set,
                   const EntityHandle* geom_volume,
                   const Tag* sense_tag,
                   const int* desired_orient,
                   const std::vector<EntityHandle>* prev_facets)
      : tool(obbtool), ray_origin(ray_point), ray_direction(ray_dir),
        tol(tolerance), minTolInt(min_tolerance_intersections), rootSet(root_set),
        geomVol(geom_volume), senseTag(sense_tag), desiredOrient(desired_orient),
        prevFacets(prev_facets) {

    };

    virtual ErrorCode register_intersection(EntityHandle set, EntityHandle triangle, double distance, 
                                            OrientedBoxTreeTool::IntersectSearchWindow&, GeomUtil::intersection_type int_type);

    virtual ErrorCode update_orient(EntityHandle set, int* surfTriOrient);
    virtual const int* getDesiredOrient() { return desiredOrient;};
  };


  ErrorCode GQT_IntRegCtxt::update_orient(EntityHandle set, int* surfTriOrient) {

    ErrorCode rval;

    // Get desired orientation of surface wrt volume. Use this to return only 
    // exit or entrance intersections.
    if(geomVol && senseTag && desiredOrient && surfTriOrient) {
      if(1!=*desiredOrient && -1!=*desiredOrient) {
        std::cerr << "error: desired orientation must be 1 (forward) or -1 (reverse)" 
                  << std::endl;
      }
      EntityHandle vols[2];
      rval = tool->get_moab_instance()->tag_get_data( *senseTag, &set, 1, vols );
      assert(MB_SUCCESS == rval);
      if(MB_SUCCESS != rval) return rval;
      if(vols[0] == vols[1]) {
        std::cerr << "error: surface has positive and negative sense wrt same volume" 
                  << std::endl;
        return MB_FAILURE;
      }
      // surfTriOrient will be used by plucker_ray_tri_intersect to avoid
      // intersections with wrong orientation.
      if       (*geomVol == vols[0]) {
        *surfTriOrient = *desiredOrient*1;
      } else if(*geomVol == vols[1]) {
        *surfTriOrient = *desiredOrient*(-1);
      } else {
        assert(false);
        return MB_FAILURE;
      }
    }

    return MB_SUCCESS;
  }
  
  bool GQT_IntRegCtxt::in_prevFacets(const EntityHandle tri) {
    return (prevFacets &&
            ((*prevFacets).end() != find((*prevFacets).begin(), (*prevFacets).end(), tri) ) );
  }

  bool GQT_IntRegCtxt::in_neighborhoods(const EntityHandle tri) {
    bool same_neighborhood = false;
    for(unsigned i=0; i<neighborhoods.size(); ++i) {
      if( neighborhoods[i].end() != find(neighborhoods[i].begin(), 
                                         neighborhoods[i].end(), tri ) ) {
        same_neighborhood = true;
        continue;
      }
    }
    return same_neighborhood;
  }



  /**\brief Determine if a ray-edge/node intersection is glancing or piercing.
   *        This function avoids asking for upward adjacencies to prevent their
   *        creation.
   *\param tri           The intersected triangle
   *\param ray_dir       The direction of the ray
   *\param int_type      The type of intersection (EDGE0, EDGE1, NODE2, ...)
   *\param close_tris    Vector of triangles in the proximity of the intersection
   *\param close_senses  Vector of surface senses for tris in the proximity of
   *                     the intersection
   *\param neighborhood  Vector of triangles in the topological neighborhood of the intersection
   *\return              True if piercing, false otherwise.
   */
  bool GQT_IntRegCtxt::edge_node_piercing_intersect(const EntityHandle                tri,
                                                    const CartVect&                   ray_dir,
                                                    const GeomUtil::intersection_type int_type,
                                                    const std::vector<EntityHandle>&  close_tris,
                                                    const std::vector<int>&           close_senses,
                                                    const Interface*                  MBI,
                                                    std::vector<EntityHandle>*        neighborhood_tris  ) {
  
    // get the node of the triangle
    const EntityHandle* conn = NULL;
    int len = 0;
    ErrorCode rval = MBI->get_connectivity( tri, conn, len );
    if(MB_SUCCESS!=rval || 3!=len) return MB_FAILURE;
  
    // get adjacent tris (and keep their corresponding senses)
    std::vector<EntityHandle> adj_tris;
    std::vector<int>          adj_senses;

    // node intersection
    if(GeomUtil::NODE0==int_type || GeomUtil::NODE1==int_type || GeomUtil::NODE2==int_type) {
 
      // get the intersected node
      EntityHandle node;
      if     (GeomUtil::NODE0==int_type) node = conn[0];
      else if(GeomUtil::NODE1==int_type) node = conn[1];
      else                               node = conn[2];

      // get tris adjacent to node
      for(unsigned i=0; i<close_tris.size(); ++i) {
        const EntityHandle* con = NULL;
        rval = MBI->get_connectivity( close_tris[i], con, len );
        if(MB_SUCCESS!=rval || 3!=len) return MB_FAILURE;

        if(node==con[0] || node==con[1] || node==con[2]) {
          adj_tris.push_back(   close_tris[i]   );
          adj_senses.push_back( close_senses[i] );
        }
      }
      if( adj_tris.empty() ) {
        std::cerr << "error: no tris are adjacent to the node" << std::endl;
        return MB_FAILURE;
      }
      // edge intersection
    } else if(GeomUtil::EDGE0==int_type || GeomUtil::EDGE1==int_type || GeomUtil::EDGE2==int_type) {

      // get the endpoints of the edge
      EntityHandle endpts[2];
      if       (GeomUtil::EDGE0==int_type) {
        endpts[0] = conn[0];
        endpts[1] = conn[1]; 
      } else if(GeomUtil::EDGE1==int_type) {
        endpts[0] = conn[1];
        endpts[1] = conn[2];
      } else {
        endpts[0] = conn[2];
        endpts[1] = conn[0];
      }

      // get tris adjacent to edge
      for(unsigned i=0; i<close_tris.size(); ++i) {
        const EntityHandle* con = NULL;
        rval = MBI->get_connectivity( close_tris[i], con, len );
        if(MB_SUCCESS!=rval || 3!=len) return MB_FAILURE;

        // check both orientations in case close_tris are not on the same surface
        if( (endpts[0]==con[0] && endpts[1]==con[1]) ||
            (endpts[0]==con[1] && endpts[1]==con[0]) ||
            (endpts[0]==con[1] && endpts[1]==con[2]) ||
            (endpts[0]==con[2] && endpts[1]==con[1]) ||
            (endpts[0]==con[2] && endpts[1]==con[0]) ||
            (endpts[0]==con[0] && endpts[1]==con[2]) ) {
          adj_tris.push_back(   close_tris[i]   );
          adj_senses.push_back( close_senses[i] );
        }
      }   
      // In a 2-manifold each edge is adjacent to exactly 2 tris
      if(2 != adj_tris.size() ) {
        std::cerr << "error: edge of a manifold must be topologically adjacent to exactly 2 tris" 
                  << std::endl;
        MBI->list_entities( endpts, 2 );
        return true;
      }
    } else {
      std::cerr << "error: special case not an node/edge intersection" << std::endl;
      return MB_FAILURE;
    }

    // The close tris were in proximity to the intersection. The adj_tris are
    // topologically adjacent to the intersection (the neighborhood).
    if(neighborhood_tris) (*neighborhood_tris).assign( adj_tris.begin(), adj_tris.end() );

    // determine glancing/piercing
    // If a desired_orientation was used in this call to ray_intersect_sets, 
    // the plucker_ray_tri_intersect will have already used it. For a piercing
    // intersection, the normal of all tris must have the same orientation.
    int sign = 0;
    for(unsigned i=0; i<adj_tris.size(); ++i) {
      const EntityHandle* con = NULL;
      rval = MBI->get_connectivity( adj_tris[i], con, len );
      if(MB_SUCCESS!=rval || 3!=len) return MB_FAILURE;
      CartVect coords[3];
      rval = MBI->get_coords( con, len, coords[0].array() );      
      if(MB_SUCCESS != rval) return MB_FAILURE;

      // get normal of triangle
      CartVect v0 = coords[1] - coords[0];
      CartVect v1 = coords[2] - coords[0];
      CartVect norm = adj_senses[i]*(v0*v1);
      double dot_prod = norm%ray_dir;

      // if the sign has not yet been decided, choose it
      if(0==sign && 0!=dot_prod) {
        if(0<dot_prod) sign = 1;
        else           sign = -1;
      }

      // intersection is glancing if tri and ray do not point in same direction
      // for every triangle
      if(0!=sign && 0>sign*dot_prod) return false;

    }
    return true;

  }



  ErrorCode
  GQT_IntRegCtxt::register_intersection(EntityHandle set, EntityHandle t, double int_dist, 
                                        OrientedBoxTreeTool::IntersectSearchWindow &search_win,
                                        GeomUtil::intersection_type int_type)
  {
    ErrorCode rval;
  
    // Do not accept intersections if they are in the vector of previously intersected
    // facets.
    if( in_prevFacets(t) ) return MB_SUCCESS;
  
    // Do not accept intersections if they are in the neighborhood of previous
    // intersections.
    if ( in_neighborhoods(t) ) return MB_SUCCESS;
  
    neighborhood.clear();
  
    // Handle special case of edge/node intersection. Accept piercing 
    // intersections and reject glancing intersections.
    // The edge_node_intersection function needs to know surface sense wrt volume.
    // A less-robust implementation could work without sense information.
    // Would it ever be useful to accept a glancing intersection?
    if(GeomUtil::INTERIOR != int_type && rootSet && geomVol && senseTag) {
      // get triangles in the proximity of the intersection
      std::vector<EntityHandle> close_tris;
      std::vector<int> close_senses;
      rval = tool->get_close_tris(ray_origin + int_dist*ray_direction, tol, rootSet, geomVol, senseTag, 
                                  close_tris, close_senses);
    
      if(MB_SUCCESS != rval) return rval; 
    
      if (!edge_node_piercing_intersect( t, ray_direction, int_type, close_tris,
                                         close_senses, tool->get_moab_instance(), &neighborhood ))
        return MB_SUCCESS;
    
    } else {
      neighborhood.push_back( t );
    }      
  
    // NOTE: add_intersection may modify the 'neg_ray_len' and 'nonneg_ray_len'
    //       members, which will affect subsequent calls to ray_tri_intersect 
    //       in this loop.
    add_intersection( set, t, int_dist, search_win);

    return MB_SUCCESS;
  }

  void GQT_IntRegCtxt::append_intersection(EntityHandle set, EntityHandle facet, double dist) {
    intersections.push_back(dist);
    sets.push_back(set);
    facets.push_back(facet);
    neighborhoods.push_back(neighborhood);
    return;
  }

  void GQT_IntRegCtxt::set_intersection(int len_idx, EntityHandle set, EntityHandle facet, double dist) {
    intersections[len_idx] = dist;
    sets[len_idx] = set;
    facets[len_idx] = facet;
    return;
  }

  /* Mode 1: Used if neg_ray_len and nonneg_ray_len are specified
     variables used:     nonneg_ray_len, neg_ray_len
     variables not used: min_tol_int, tol
     1) keep the closest nonneg intersection and one negative intersection, if closer
  */
  void GQT_IntRegCtxt::add_mode1_intersection(EntityHandle set, EntityHandle facet, double dist,
                                              OrientedBoxTreeTool::IntersectSearchWindow &search_win) {
    if(2 != intersections.size()) {
      intersections.resize(2,0);
      sets.resize(2,0);
      facets.resize(2,0);
      // must initialize this for comparison below
      intersections[0] = -std::numeric_limits<double>::max();
    }
  
    // negative case
    if(0.0>dist) {
      set_intersection(0, set, facet, dist);
      search_win.second      = &intersections[0];
      // nonnegative case
    } else {
      set_intersection(1, set, facet, dist);
      search_win.first   = &intersections[1];
      // if the intersection is closer than the negative one, remove the negative one
      if(dist < -*(search_win.second) ) {
        set_intersection(0, 0, 0, -intersections[1]);
        search_win.second      = &intersections[0]; 
      }
    }
    //    std::cout << "add_intersection: dist = " << dist << " search_win.second=" << *search_win.second
    //          << " search_win.first=" << *search_win.first << std::endl;
    return;
  }

  void GQT_IntRegCtxt::add_intersection( EntityHandle set, EntityHandle facet, double dist,
                                         OrientedBoxTreeTool::IntersectSearchWindow &search_win ) {

    // Mode 1, detected by non-null neg_ray_len pointer
    // keep the closest nonneg intersection and one negative intersection, if closer
    if(search_win.second  && search_win.first ) {
      return add_mode1_intersection(set,facet,dist,search_win);
    }

    // ---------------------------------------------------------------------------
    /*   Mode 2: Used if neg_ray_len is not specified
         variables used:     min_tol_int, tol, search_win.first
         variables not used: neg_ray_len
         1) if(min_tol_int<0) return all intersections
         2) otherwise return all inside tolerance and unless there are >min_tol_int
         inside of tolerance, return the closest outside of tolerance */
    // Mode 2
    // If minTolInt is less than zero, return all intersections
    if (minTolInt < 0 && dist > -tol) {
      append_intersection(set,facet,dist);
      neighborhoods.push_back(neighborhood);
      return;
    }

    // Check if the 'len' pointer is pointing into the intersection
    // list.  If this is the case, then the list contains, at that
    // location, an intersection greater than the tolerance away from
    // the base point of the ray.
    int len_idx = -1;
    if (search_win.first && search_win.first >= &intersections[0] && 
        search_win.first < &intersections[0] + intersections.size())
      len_idx = search_win.first - &intersections[0];

    // If the intersection is within tol of the ray base point, we 
    // always add it to the list.
    if (dist <= tol) {
      // If the list contains an intersection outside the tolerance...
      if (len_idx >= 0) {
        // If we no longer want an intersection outside the tolerance,
        // remove it.
        if ((int)intersections.size() >= minTolInt) {
          set_intersection(len_idx,set,facet,dist);
          // From now on, we want only intersections within the tolerance,
          // so update length accordingly
          search_win.first = &tol;
        }
        // Otherwise appended to the list and update pointer
        else {
          append_intersection(set,facet,dist);
          search_win.first = &intersections[len_idx];
        }
      }
      // Otherwise just append it
      else {
        append_intersection(set,facet,dist);
        // If we have all the intersections we want, set
        // length such that we will only find further intersections
        // within the tolerance
        if ((int)intersections.size() >= minTolInt)
          search_win.first = &tol;
      }
    }
    // Otherwise the intersection is outside the tolerance
    // If we already have an intersection outside the tolerance and
    // this one is closer, replace the existing one with this one.
    else if (len_idx >= 0) {
      if (dist <= *search_win.first) {
        set_intersection(len_idx,set,facet,dist);
      }
    }
    // Otherwise if we want an intersection outside the tolerance
    // and don'thave one yet, add it.
    else if ((int)intersections.size() < minTolInt) {
      append_intersection(set,facet,dist);
      // update length.  this is currently the closest intersection, so
      // only want further intersections that are closer than this one.
      search_win.first = &intersections.back();
    }
  }


  
GeomQueryTool::GeomQueryTool(GeomTopoTool* geomtopotool, bool trace_counting,
                             double overlap_thickness, double numerical_precision){

  geomTopoTool = geomtopotool;

  senseTag = geomTopoTool->get_sense_tag();
  
  obbTreeTool = geomTopoTool->obb_tree();
  MBI = geomTopoTool->get_moab_instance();

  counting = trace_counting;
  overlapThickness = overlap_thickness;
  numericalPrecision = numerical_precision;

  // reset query counters
  n_pt_in_vol_calls = 0;
  n_ray_fire_calls = 0;
}

GeomQueryTool::~GeomQueryTool() {}

ErrorCode GeomQueryTool::initialize() {

  ErrorCode rval;

  rval = geomTopoTool->find_geomsets();
  MB_CHK_SET_ERR(rval, "Failed to find geometry sets");

  rval = geomTopoTool->setup_implicit_complement();
  MB_CHK_SET_ERR(rval , "Couldn't setup the implicit complement");

  rval = geomTopoTool->construct_obb_trees();
  MB_CHK_SET_ERR(rval, "Failed to construct OBB trees");
  
  return MB_SUCCESS;
}

void GeomQueryTool::RayHistory::reset() {
  prev_facets.clear();
}

void GeomQueryTool::RayHistory::reset_to_last_intersection() {

  if( prev_facets.size() > 1 ){
    prev_facets[0] = prev_facets.back();
    prev_facets.resize( 1 );
  }
}

void GeomQueryTool::RayHistory::rollback_last_intersection() {
  if( prev_facets.size() )
    prev_facets.pop_back();
}

bool GeomQueryTool::RayHistory::in_history(EntityHandle ent) {
  return std::find(prev_facets.begin(), prev_facets.end(), ent) != prev_facets.end();
}

void GeomQueryTool::RayHistory::add_entity(EntityHandle ent) {
  prev_facets.push_back(ent);
}

ErrorCode GeomQueryTool::ray_fire(const EntityHandle volume,
                                  const double point[3], const double dir[3],
                                  EntityHandle& next_surf, double& next_surf_dist,
                                  RayHistory* history, double user_dist_limit,
                                  int ray_orientation,
                                  OrientedBoxTreeTool::TrvStats* stats ) {

  // take some stats that are independent of nps
  if(counting) {
    ++n_ray_fire_calls;
    if(0==n_ray_fire_calls%10000000) {
      std::cout << "n_ray_fires="   << n_ray_fire_calls
                << " n_pt_in_vols=" << n_pt_in_vol_calls << std::endl;
    }
  }
  
  if (debug) {
    std::cout << "ray_fire:"
              << " xyz=" << point[0] << " " << point[1] << " " << point[2]
              << " uvw=" << dir[0] << " " << dir[1] << " " << dir[2]
              << " entity_handle=" << volume << std::endl;
  }
  
  const double huge_val = std::numeric_limits<double>::max();
  double dist_limit = huge_val;
  if( user_dist_limit > 0 )
    dist_limit = user_dist_limit;
  
  // don't recreate these every call
  std::vector<double>       dists;
  std::vector<EntityHandle> surfs;
  std::vector<EntityHandle> facets;
  
  EntityHandle root;
  ErrorCode rval = geomTopoTool->get_root(volume, root);
  MB_CHK_SET_ERR(rval, "Failed to get the obb tree root of the volume");
  
  // check behind the ray origin for intersections
  double neg_ray_len;
  if(0 == overlapThickness) {
    neg_ray_len = -numericalPrecision;
  } else {
    neg_ray_len = -overlapThickness;
  }
  
  // optionally, limit the nonneg_ray_len with the distance to next collision.
  double nonneg_ray_len = dist_limit;
  
  // the nonneg_ray_len should not be less than -neg_ray_len, or an overlap
  // may be missed due to optimization within ray_intersect_sets
  if(nonneg_ray_len < -neg_ray_len) nonneg_ray_len = -neg_ray_len;
  if (0 > nonneg_ray_len || 0 <= neg_ray_len) {
    MB_SET_ERR(MB_FAILURE, "Incorrect ray length provided");
  }
  
  // min_tolerance_intersections is passed but not used in this call
  const int min_tolerance_intersections = 0;

  // numericalPrecision is used for box.intersect_ray and find triangles in the
  // neighborhood of edge/node intersections.
  GQT_IntRegCtxt int_reg_ctxt(geomTopoTool->obb_tree(), point, dir, numericalPrecision,
                              min_tolerance_intersections, &root, &volume,
                              &senseTag, &ray_orientation,
                              history ? &(history->prev_facets) : NULL );

  OrientedBoxTreeTool::IntersectSearchWindow search_win(&nonneg_ray_len, &neg_ray_len);
  rval = geomTopoTool->obb_tree()->ray_intersect_sets( dists, surfs, facets, root, numericalPrecision,
                                                       point, dir, search_win, int_reg_ctxt, stats);
  
  MB_CHK_SET_ERR(rval, "Ray query failed");

  // If no distances are returned, the particle is lost unless the physics limit
  // is being used. If the physics limit is being used, there is no way to tell
  // if the particle is lost. To avoid ambiguity, DO NOT use the distance limit
  // unless you know lost particles do not occur.
  if( dists.empty() ) {
    next_surf = 0;
    if(debug) {
      std::cout << "          next_surf=0 dist=(undef)" << std::endl;
    }
    return MB_SUCCESS;
  }

  // Assume that a (neg, nonneg) pair of RTIs could be returned,
  // however, only one or the other may exist. dists[] may be populated, but
  // intersections are ONLY indicated by nonzero surfs[] and facets[].
  if (2 != dists.size() || 2 != facets.size()) {
    MB_SET_ERR(MB_FAILURE, "Incorrect number of facets/distances");
  }
  if ( 0.0 < dists[0] || 0.0 > dists[1] ) {
    MB_SET_ERR(MB_FAILURE, "Invalid intersection distance signs");
  }

  // If both negative and nonnegative RTIs are returned, the negative RTI must
  // closer to the origin.
  if( (0!=facets[0] && 0!=facets[1]) && (-dists[0] > dists[1]) ) {
    MB_SET_ERR(MB_FAILURE, "Invalid intersection distance values");
  }

  // If an RTI is found at negative distance, perform a PMT to see if the
  // particle is inside an overlap.
  int exit_idx = -1;
  if(0!=facets[0]) {
    // get the next volume
    std::vector<EntityHandle> vols;
    EntityHandle nx_vol;
    rval = MBI->get_parent_meshsets( surfs[0], vols );
    MB_CHK_SET_ERR(rval, "Failed to get the parent meshsets");
    if(2 != vols.size()) {
      MB_SET_ERR(MB_FAILURE, "Invaid number of parent volumes found");
    }
    if(vols.front() == volume) {
      nx_vol = vols.back();
    } else {
      nx_vol = vols.front();
    }
    // Check to see if the point is actually in the next volume.
    // The list of previous facets is used to topologically identify the
    // "on_boundary" result of the PMT. This avoids a test that uses proximity
    // (a tolerance).
    int result;
    rval = point_in_volume( nx_vol, point, result, dir, history );
    MB_CHK_SET_ERR(rval, "Point in volume query failed");
    if(1==result) exit_idx = 0;

  }

  // if the negative distance is not the exit, try the nonnegative distance
  if(-1==exit_idx && 0!=facets[1]) exit_idx = 1;

  // if the exit index is still unknown, the particle is lost
  if(-1 == exit_idx) {
    next_surf = 0;
    if (debug) {
      std::cout << "next surf hit = 0, dist = (undef)" << std::endl;
    }
    return MB_SUCCESS;
  }

  // return the intersection
  next_surf = surfs[exit_idx];
  next_surf_dist = ( 0>dists[exit_idx] ? 0 : dists[exit_idx]);

  if( history ){
    history->prev_facets.push_back( facets[exit_idx] );
  }

  if (debug) {
    if( 0 > dists[exit_idx] ){
      std::cout << "          OVERLAP track length=" << dists[exit_idx] << std::endl;
    }
    std::cout << "          next_surf = " <<  next_surf  // todo: use geomtopotool to get id by entity handle
              << ", dist = " << next_surf_dist << " new_pt=";
    for( int i = 0; i < 3; ++i ){
      std::cout << point[i]+dir[i]*next_surf_dist << " ";
    }
    std::cout << std::endl;
  }

  return MB_SUCCESS;
}

ErrorCode GeomQueryTool::point_in_volume(const EntityHandle volume,
                                         const double xyz[3],
                                         int& result,
                                         const double *uvw,
                                         const RayHistory *history) {
  // take some stats that are independent of nps
  if(counting) ++n_pt_in_vol_calls;

  // early fail for piv - see if point inside the root level obb
  // if its not even in the box dont bother doing anything else
  ErrorCode rval = point_in_box(volume,xyz,result);
  if( !result ) {
    result = 0;
    return MB_SUCCESS;
  }
  
  // get OBB Tree for volume
  EntityHandle root;
  rval = geomTopoTool->get_root(volume, root);
  MB_CHK_SET_ERR(rval, "Failed to find the volume's obb tree root");
  
  // Don't recreate these every call. These cannot be the same as the ray_fire
  // vectors because both are used simultaneously.
  std::vector<double>       dists;
  std::vector<EntityHandle> surfs;
  std::vector<EntityHandle> facets;
  std::vector<int>          dirs;

  // if uvw is not given or is full of zeros, use a random direction
  double u = 0, v = 0, w = 0;

  if( uvw ){
    u = uvw[0]; v=uvw[1], w=uvw[2];
  }

  if( u == 0 && v == 0 && w == 0 )
  {
    u = rand();
    v = rand();
    w = rand();
    const double magnitude = sqrt( u*u + v*v + w*w );
    u /= magnitude;
    v /= magnitude;
    w /= magnitude;
  }

  const double ray_direction[] = { u, v, w };

  // if overlaps, ray must be cast to infinity and all RTIs must be returned
  const double   large       = 1e15;
  double   ray_length  = large;

  // If overlaps occur, the pt is inside if traveling along the ray from the
  // origin, there are ever more exits than entrances. In lieu of implementing
  // that, all intersections to infinity are required if overlaps occur (expensive)
  int min_tolerance_intersections;
  if(0 != overlapThickness) {
    min_tolerance_intersections = -1;
  // only the first intersection is needed if overlaps do not occur (cheap)
  } else {
    min_tolerance_intersections = 1;
  }

  // Get intersection(s) of forward and reverse orientation. Do not return
  // glancing intersections or previous facets.
  GQT_IntRegCtxt int_reg_ctxt(geomTopoTool->obb_tree(), xyz, ray_direction, numericalPrecision,
                              min_tolerance_intersections, &root, &volume,
                              &senseTag, NULL,
                              history ? &(history->prev_facets) : NULL );
  
  OrientedBoxTreeTool::IntersectSearchWindow search_win(&ray_length,NULL);
  rval = geomTopoTool->obb_tree()->ray_intersect_sets( dists, surfs, facets, root, numericalPrecision,
                                                       xyz, ray_direction, search_win, int_reg_ctxt);
  MB_CHK_SET_ERR(rval, "Ray fire query failed");

  // determine orientation of all intersections
  // 1 for entering, 0 for leaving, -1 for tangent
  // Tangent intersections are not returned from ray_tri_intersect.
  dirs.resize(dists.size());
  for(unsigned i=0; i<dists.size(); ++i) {
    rval = boundary_case( volume, dirs[i], u, v, w, facets[i], surfs[i] );
    MB_CHK_SET_ERR(rval, "Failed to resolve boundary case");
  }

  // count all crossings
  if(0 != overlapThickness) {
    int sum = 0;
    for(unsigned i=0; i<dirs.size(); ++i) {
      if     ( 1==dirs[i]) sum+=1; // +1 for entering
      else if( 0==dirs[i]) sum-=1; // -1 for leaving
      else if(-1==dirs[i]) {       //  0 for tangent
        std::cout << "direction==tangent" << std::endl;
        sum+=0;
      } else {
	MB_SET_ERR(MB_FAILURE, "Error: unknown direction");
      }
    }

    // inside/outside depends on the sum
    if(0<sum)                                                result = 0; // pt is outside (for all vols)
    else if(0>sum)                                           result = 1; // pt is inside  (for all vols)
    else if ( geomTopoTool->is_implicit_complement(volume) ) result = 1; // pt is inside  (for impl_compl_vol)
    else                                                     result = 0; // pt is outside (for all other vols)

  // Only use the first crossing
  } else {
      if( dirs.empty() ) {
      result = 0; // pt is outside
    } else {
      int smallest = std::min_element( dists.begin(), dists.end() ) - dists.begin();
      if     ( 1==dirs[smallest] ) result = 0; // pt is outside
      else if( 0==dirs[smallest] ) result = 1; // pt is inside
      else if(-1==dirs[smallest] ) {
        // Should not be here because Plucker ray-triangle test does not
        // return coplanar rays as intersections.
        std::cout << "direction==tangent" << std::endl;
        result = -1;
      } else {
	MB_SET_ERR(MB_FAILURE, "Error: unknown direction");
      }
    }
  }

  if(debug)
    std::cout << "pt_in_vol: result=" << result
              << " xyz=" << xyz[0] << " " << xyz[1] << " " << xyz[2] << " uvw=" << u << " " << v << " " << w
              << " vol_id=" << volume << std::endl;  // todo: use geomtopotool to get id by entity handle

  return MB_SUCCESS;
}

/**
 *  \brief For the volume pointed to and the point wished to be tested, returns
 *   whether the point is inside or outside the bounding box of the volume.
 * inside = 0, not inside, inside = 1, inside   
 */
ErrorCode GeomQueryTool::point_in_box(EntityHandle volume, const double point[3], int &inside ) {
  double minpt[3];
  double maxpt[3];
  ErrorCode rval = geomTopoTool->get_bounding_coords(volume,minpt,maxpt);
  MB_CHK_SET_ERR(rval, "Failed to get the bounding coordinates of the volume");

  // early exits
  if ( point[0] > maxpt[0] || point[0] < minpt[0]) {
    inside = 0;
    return rval;
  }
  if ( point[1] > maxpt[1] || point[1] < minpt[1]) {
    inside = 0;
    return rval;
  }
  if ( point[2] > maxpt[2] || point[2] < minpt[2]) {
    inside = 0;
    return rval;
  }
  inside = 1;
  return rval;  
}
  
ErrorCode GeomQueryTool::test_volume_boundary(const EntityHandle volume, const EntityHandle surface,
                                              const double xyz[3], const double uvw[3], int& result,
                                              const RayHistory* history )
{
  ErrorCode rval;
  int dir;

  if( history && history->prev_facets.size() ){
    // the current facet is already available
    rval = boundary_case( volume, dir, uvw[0], uvw[1], uvw[2], history->prev_facets.back(), surface );
    MB_CHK_SET_ERR(rval, "Failed to resolve the boundary case");
  }
  else{
    // look up nearest facet

    // Get OBB Tree for surface
    EntityHandle root;
    rval = geomTopoTool->get_root(volume, root);
    MB_CHK_SET_ERR(rval, "Failed to get the volume's OBB tree root");

    // Get closest triangle on surface
    const CartVect point(xyz);
    CartVect nearest;
    EntityHandle facet_out;
    rval = geomTopoTool->obb_tree()->closest_to_location( point.array(), root, nearest.array(), facet_out );
    MB_CHK_SET_ERR(rval, "Failed to find the closest point to location");

    rval = boundary_case( volume, dir, uvw[0], uvw[1], uvw[2], facet_out, surface );
    MB_CHK_SET_ERR(rval, "Failed to resolve the boundary case");

  }

  result = dir;

  return MB_SUCCESS;

}

// use spherical area test to determine inside/outside of a polyhedron.
ErrorCode GeomQueryTool::point_in_volume_slow( EntityHandle volume, const double xyz[3], int& result )
{
  ErrorCode rval;
  Range faces;
  std::vector<EntityHandle> surfs;
  std::vector<int> senses;
  double sum = 0.0;
  const CartVect point(xyz);

  rval = MBI->get_child_meshsets( volume, surfs );
  MB_CHK_SET_ERR(rval, "Failed to get the volume's child surfaces");

  senses.resize( surfs.size() );
  rval = geomTopoTool->get_surface_senses( volume, surfs.size(), &surfs[0], &senses[0] );
  MB_CHK_SET_ERR(rval, "Failed to get the volume's surface senses");

  for (unsigned i = 0; i < surfs.size(); ++i) {
    if (!senses[i])  // skip non-manifold surfaces
      continue;

    double surf_area = 0.0, face_area;
    faces.clear();
    rval = MBI->get_entities_by_dimension( surfs[i], 2, faces );
    MB_CHK_SET_ERR(rval, "Failed to get the surface entities by dimension");

    for (Range::iterator j = faces.begin(); j != faces.end(); ++j) {
      rval = poly_solid_angle( *j, point, face_area );
      MB_CHK_SET_ERR(rval, "Failed to determin the polygon's solid angle");

      surf_area += face_area;
    }

    sum += senses[i] * surf_area;
  }

  result = fabs(sum) > 2.0*M_PI;
  return MB_SUCCESS;
}



// detemine distance to nearest surface
  ErrorCode GeomQueryTool::closest_to_location( EntityHandle volume, const double coords[3], double& result,
						EntityHandle* closest_surface)
{
  // Get OBB Tree for volume
  EntityHandle root;
  ErrorCode rval = geomTopoTool->get_root(volume, root);
  MB_CHK_SET_ERR(rval, "Failed to get the volume's obb tree root");

  // Get closest triangles in volume
  const CartVect point(coords);
  CartVect nearest;
  EntityHandle facet_out;

  rval = geomTopoTool->obb_tree()->closest_to_location( point.array(), root, nearest.array(), facet_out, closest_surface);
  MB_CHK_SET_ERR(rval, "Failed to get the closest intersection to location");
  // calculate distance between point and nearest facet
  result = (point-nearest).length();
  
  return MB_SUCCESS;

}

// calculate volume of polyhedron
ErrorCode GeomQueryTool::measure_volume( EntityHandle volume, double& result )
{
  ErrorCode rval;
  std::vector<EntityHandle> surfaces;
  result = 0.0;

   // don't try to calculate volume of implicit complement
  if (geomTopoTool->is_implicit_complement(volume)) {
    result = 1.0;
    return MB_SUCCESS;
  }

    // get surfaces from volume
  rval = MBI->get_child_meshsets( volume, surfaces );
  MB_CHK_SET_ERR(rval, "Failed to get the volume's child surfaces");

    // get surface senses
  std::vector<int> senses( surfaces.size() );
  rval = geomTopoTool->get_surface_senses( volume, surfaces.size(), &surfaces[0], &senses[0] );
  MB_CHK_SET_ERR(rval, "Failed to retrieve surface-volume sense data. Cannot calculate volume");
  
  for (unsigned i = 0; i < surfaces.size(); ++i) {
      // skip non-manifold surfaces
    if (!senses[i])
      continue;

      // get triangles in surface
    Range triangles;
    rval = MBI->get_entities_by_dimension( surfaces[i], 2, triangles );
    MB_CHK_SET_ERR(rval, "Failed to get the surface triangles");
    
    if (!triangles.all_of_type(MBTRI)) {
      std::cout << "WARNING: Surface " << surfaces[i]  // todo: use geomtopotool to get id by entity handle
                << " contains non-triangle elements. Volume calculation may be incorrect."
                << std::endl;
      triangles.clear();
      rval = MBI->get_entities_by_type( surfaces[i], MBTRI, triangles );
      MB_CHK_SET_ERR(rval, "Failed to get the surface triangles");
    }

      // calculate signed volume beneath surface (x 6.0)
    double surf_sum = 0.0;
    const EntityHandle *conn;
    int len;
    CartVect coords[3];
    for (Range::iterator j = triangles.begin(); j != triangles.end(); ++j) {
      rval = MBI->get_connectivity( *j, conn, len, true );
      MB_CHK_SET_ERR(rval, "Failed to get the connectivity of the current triangle");
      if(3 != len) {
	MB_SET_ERR(MB_FAILURE, "Incorrect connectivity length for triangle");
      }
      rval = MBI->get_coords( conn, 3, coords[0].array() );
      MB_CHK_SET_ERR(rval, "Failed to get the coordinates of the current triangle's vertices");

      coords[1] -= coords[0];
      coords[2] -= coords[0];
      surf_sum += (coords[0] % (coords[1] * coords[2]));
    }
    result += senses[i] * surf_sum;
  }

  result /= 6.0;
  return MB_SUCCESS;
}

// sum area of elements in surface
ErrorCode GeomQueryTool::measure_area( EntityHandle surface, double& result )
{
    // get triangles in surface
  Range triangles;
  ErrorCode rval = MBI->get_entities_by_dimension( surface, 2, triangles );
  MB_CHK_SET_ERR(rval, "Failed to get the surface entities");
  if (!triangles.all_of_type(MBTRI)) {
    std::cout << "WARNING: Surface " << surface  // todo: use geomtopotool to get id by entity handle
              << " contains non-triangle elements. Area calculation may be incorrect."
              << std::endl;
    triangles.clear();
    rval = MBI->get_entities_by_type( surface, MBTRI, triangles );
    MB_CHK_SET_ERR(rval, "Failed to the surface's triangle entities");
  }

    // calculate sum of area of triangles
  result = 0.0;
  const EntityHandle *conn;
  int len;
  CartVect coords[3];
  for (Range::iterator j = triangles.begin(); j != triangles.end(); ++j) {
    rval = MBI->get_connectivity( *j, conn, len, true );
    MB_CHK_SET_ERR(rval, "Failed to get the current triangle's connectivity");
    if(3 != len) {
      MB_SET_ERR(MB_FAILURE, "Incorrect connectivity length for triangle");
    }
    rval = MBI->get_coords( conn, 3, coords[0].array() );
    MB_CHK_SET_ERR(rval, "Failed to get the current triangle's vertex coordinates");

    //calculated area using cross product of triangle edges
    CartVect v1 = coords[1] - coords[0];
    CartVect v2 = coords[2] - coords[0];
    CartVect xp = v1 * v2;
    result += xp.length();
  }
  result *= 0.5;
  return MB_SUCCESS;
}


ErrorCode GeomQueryTool::get_normal(EntityHandle surf, const double in_pt[3], double angle[3], const RayHistory* history )
{
  EntityHandle root;
  ErrorCode rval = geomTopoTool->get_root(surf, root);
  MB_CHK_SET_ERR(rval, "Failed to get the surface's obb tree root");

  std::vector<EntityHandle> facets;

  // if no history or history empty, use nearby facets
  if( !history || (history->prev_facets.size() == 0) ){
    rval = geomTopoTool->obb_tree()->closest_to_location( in_pt, root, numericalPrecision, facets );
    MB_CHK_SET_ERR(rval, "Failed to get closest intersection to location");
  }
  // otherwise use most recent facet in history
  else{
    facets.push_back( history->prev_facets.back() );
  }

  CartVect coords[3], normal(0.0);
  const EntityHandle *conn;
  int len;
  for (unsigned i = 0; i < facets.size(); ++i) {
    rval = MBI->get_connectivity( facets[i], conn, len );
    MB_CHK_SET_ERR(rval, "Failed to get facet connectivity");
    if(3 != len) {
      MB_SET_ERR(MB_FAILURE, "Incorrect connectivity length for triangle");
    }
 
    rval = MBI->get_coords( conn, 3, coords[0].array() );
    MB_CHK_SET_ERR(rval, "Failed to get vertex coordinates");

    coords[1] -= coords[0];
    coords[2] -= coords[0];
    normal += coords[1] * coords[2];
  }

  normal.normalize();
  normal.get( angle );

  return MB_SUCCESS;
}


/* SECTION II (private) */

// If point is on boundary, then this function is called to
// discriminate cases in which the ray is entering or leaving.
// result= 1 -> inside volume or entering volume
// result= 0 -> outside volume or leaving volume
// result=-1 -> on boundary with null or tangent uvw
ErrorCode GeomQueryTool::boundary_case(EntityHandle volume, int& result,
                                       double u, double v, double w,
                                       EntityHandle facet,
                                       EntityHandle surface)
{
  ErrorCode rval;

  // test to see if uvw is provided
  if ( u <= 1.0 && v <= 1.0 && w <= 1.0 ) {

    const CartVect ray_vector(u, v, w);
    CartVect coords[3], normal(0.0);
    const EntityHandle *conn;
    int len, sense_out;

    rval = MBI->get_connectivity( facet, conn, len );
    MB_CHK_SET_ERR(rval, "Failed to get the triangle's connectivity");
    if(3 != len) {
      MB_SET_ERR(MB_FAILURE, "Incorrect connectivity length for triangle");
    }

    rval = MBI->get_coords( conn, 3, coords[0].array() );
    MB_CHK_SET_ERR(rval, "Failed to get vertex coordinates");

    rval = geomTopoTool->get_sense( surface, volume, sense_out );
    MB_CHK_SET_ERR(rval, "Failed to get the surface's sense with respect to it's volume");

    coords[1] -= coords[0];
    coords[2] -= coords[0];
    normal = sense_out * (coords[1] * coords[2]);

    double sense = ray_vector % normal;

    if ( sense < 0.0 ) {
      result = 1;     // inside or entering
    } else  if ( sense > 0.0 ) {
      result = 0;     // outside or leaving
    } else  if ( sense == 0.0 ) {
      result = -1;    // tangent, therefore on boundary
    } else {
      result = -1;    // failure
      MB_SET_ERR(MB_FAILURE, "Failed to resolve boundary case");
    }

  // if uvw not provided, return on_boundary.
  } else {
    result = -1;      // on boundary
    return MB_SUCCESS;

  }

  return MB_SUCCESS;
}

// point_in_volume_slow, including poly_solid_angle helper subroutine
// are adapted from "Point in Polyhedron Testing Using Spherical Polygons", Paulo Cezar
// Pinto Carvalho and Paulo Roma Cavalcanti, _Graphics Gems V_, pg. 42.  Original algorithm
// was described in "An Efficient Point In Polyhedron Algorithm", Jeff Lane, Bob Magedson,
// and Mike Rarick, _Computer Vision, Graphics, and Image Processing 26_, pg. 118-225, 1984.

// helper function for point_in_volume_slow.  calculate area of a polygon
// projected into a unit-sphere space
ErrorCode GeomQueryTool::poly_solid_angle( EntityHandle face, const CartVect& point, double& area )
{
  ErrorCode rval;

    // Get connectivity
  const EntityHandle* conn;
  int len;
  rval = MBI->get_connectivity( face, conn, len, true );
  MB_CHK_SET_ERR(rval, "Failed to get the connectivity of the polygon");

  // Allocate space to store vertices
  CartVect coords_static[4];
  std::vector<CartVect> coords_dynamic;
  CartVect* coords = coords_static;
  if ((unsigned)len > (sizeof(coords_static)/sizeof(coords_static[0]))) {
    coords_dynamic.resize(len);
    coords = &coords_dynamic[0];
  }

  // get coordinates
  rval = MBI->get_coords( conn, len, coords->array() );
  MB_CHK_SET_ERR(rval, "Failed to get the coordinates of the polygon vertices");
  
  // calculate normal
  CartVect norm(0.0), v1, v0 = coords[1] - coords[0];
  for (int i = 2; i < len; ++i) {
    v1 = coords[i] - coords[0];
    norm += v0 * v1;
    v0 = v1;
  }

  // calculate area
  double s, ang;
  area = 0.0;
  CartVect r, n1, n2, b, a = coords[len-1] - coords[0];
  for (int i = 0; i < len; ++i) {
    r = coords[i] - point;
    b = coords[(i+1)%len] - coords[i];
    n1 = a * r; // = norm1 (magnitude is important)
    n2 = r * b; // = norm2 (magnitude is important)
    s = (n1 % n2) / (n1.length() * n2.length()); // = cos(angle between norm1,norm2)
    ang = s <= -1.0 ? M_PI : s >= 1.0 ? 0.0 : acos(s); // = acos(s)
    s = (b * a) % norm; // =orientation of triangle wrt point
    area += s > 0.0 ? M_PI - ang : M_PI + ang;
    a = -b;
  }

  area -= M_PI * (len - 2);
  if ((norm % r) > 0)
    area = -area;
  return MB_SUCCESS;
}

void GeomQueryTool::set_overlap_thickness( double new_thickness ){

  if (new_thickness < 0 || new_thickness > 100) {
    std::cerr << "Invalid overlap_thickness = " << new_thickness << std::endl;
  }
  else{
    overlapThickness = new_thickness;
  }
  std::cout << "Set overlap thickness = " << overlapThickness << std::endl;

}

void GeomQueryTool::set_numerical_precision( double new_precision ){

  if ( new_precision <= 0 || new_precision > 1) {
    std::cerr << "Invalid numerical_precision = " << numericalPrecision << std::endl;
  }
  else{
    numericalPrecision = new_precision;
  }

  std::cout << "Set numerical precision = " << numericalPrecision << std::endl;

}


}
