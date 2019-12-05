#ifndef MOAB_GEOM_QUERY_TOOL_HPP
#define MOAB_GEOM_QUERY_TOOL_HPP

#include "MBTagConventions.hpp"
#include "moab/CartVect.hpp"
#include "moab/Range.hpp"
#include "moab/Core.hpp"
#include "moab/GeomUtil.hpp"
#include "moab/FileOptions.hpp"
#include "moab/EntityHandle.hpp"
#include "moab/GeomTopoTool.hpp"
#include "moab/OrientedBoxTreeTool.hpp"

#include <vector>
#include <map>
#include <string>
#include <assert.h>

namespace moab {

  /** \class GeomQueryTool
   *
   * \brief Tool for querying different aspects of geometric topology sets in MOAB
   *
   * Given the conventions established in GeomTopoTool for representing
   * geometric topology through a hierarchy of meshsets, this tool provides a
   * set of methods to query different geometric aspects of those geometric
   * topology sets including:
   * 
   *   * measures of surface area and volume
   *   * distance to a bounding surface from a point within a volume
   *   * test the inclusion of a point within a volume
   *   * find the angle of a surface at a point
   *
   * A feature of these queries is that there is some support for overlapping
   * geometries.
   * 
   */

class GeomQueryTool
{
public:

  /* \class RayHistory
   *
   * In many use cases, it is useful to track some of the history of a ray as
   * it passes through a geometry, particularly a geometry represented by
   * facets.  For example, given round-off erorr in ray-triangle tests
   * (GeomUtil::ray_tri_intersect) used as part of a test for ray-surface
   * intersection, it is possible for subsequent queries to oscillate between
   * adjacent surfaces.  This class stores information about history of a ray
   * that can be used to test for such circumstances so that they can be
   * accommodated.
   *
   */
  
  class RayHistory {

  public:
    /**
     * Clear this entire history-- logically equivalent to creating a new history,
     * but probably more efficient.
     */
    void reset();

    /**
     * Clear the history up to the most recent intersection.  This should be
     * called when a ray changes direction at the site of a surface crossing,
     * a situation that most commonly occurs at a reflecting boundary.
     */
    void reset_to_last_intersection();

    /**
     * Remove the most recent intersection.  This allows a subsequent call
     * along the same ray to return the same intersection.
     */
    void rollback_last_intersection();

    /**
     * @return the number of surface crossings currently represented by this ray history
     */
    int size() const { return prev_facets.size(); }

    /**
     * @return Boolean indicating if this entity is in the RayHistory
     */
    bool in_history(EntityHandle ent);

    /**
     * Add entity to the RayHistory
     */
    void add_entity(EntityHandle ent);
    
  private:
    std::vector<EntityHandle> prev_facets;

    friend class GeomQueryTool;

  };
  
  // Constructor
  GeomQueryTool(GeomTopoTool* geomtopotool, bool trace_counting = false,
                double overlap_thickness = 0., double numerical_precision = 0.001);

  // Destructor
  ~GeomQueryTool();

  ErrorCode initialize();

  /**\brief find the next surface crossing from a given point in a given direction
   *
   * This is the primary method to enable ray tracing through a geometry.
   * Given a volume and a ray, it determines the distance to the nearest intersection
   * with a bounding surface of that volume and returns that distance and the 
   * EntityHandle indicating on which surface that intersection occurs.
   * The caller can compute the location of the intersection by adding the
   * distance to the ray.
   *
   * When a series of calls to this function are made along the same ray (e.g. for
   * the purpose of tracking a ray through several volumes), the optional history
   * argument should be given.  The history prevents previously intersected facets
   * from being intersected again.  A single history should be used as long as a
   * ray is proceeding forward without changing direction.  This situation is
   * sometimes referred to as "streaming."
   *
   * If a ray changes direction at an intersection site, the caller should call
   * reset_to_last_intersection() on the history object before the next ray fire.
   *
   * @param volume The volume at which to fire the ray
   * @param ray_start An array of x,y,z coordinates from which to start the ray.
   * @param ray_dir An array of x,y,z coordinates indicating the direction of the ray.
   *                Must be of unit length.
   * @param next_surf Output parameter indicating the next surface intersected by the ray.
   *                If no intersection is found, will be set to 0.
   * @param next_surf_dist Output parameter indicating distance to next_surf.  If next_surf is
   *                0, this value is undefined and should not be used.
   * @param history Optional RayHistory object.  If provided, the facets in the history are
   *                assumed to not intersect with the given ray.  The facet intersected
   *                by this query will also be added to the history.
   * @param dist_limit Optional distance limit.  If provided and > 0, no intersections at a
   *                distance further than this value will be returned.
   * @param ray_orientation Optional ray orientation. If provided determines intersections
   *                along the normal provided, e.g. if -1 allows intersections back along the
   *                the ray direction, Default is 1, i.e. exit intersections
   * @param stats Optional TrvStats object used to measure performance of underlying OBB
   *              ray-firing query.  See OrientedBoxTreeTool.hpp for details.
   *
   */
  ErrorCode ray_fire(const EntityHandle volume,
                     const double ray_start[3], const double ray_dir[3],
                     EntityHandle& next_surf, double& next_surf_dist,
                     RayHistory* history = NULL, double dist_limit = 0,
                     int ray_orientation = 1,
                     OrientedBoxTreeTool::TrvStats* stats = NULL );

  /**\brief Test if a point is inside or outside a volume
   *
   * This method finds the point on the boundary of the volume that is nearest
   * the test point (x,y,z).  If that point is "close" to a surface, a boundary test
   * is performed based on the normal of the surface at that point and the
   * optional ray direction (u,v,w).
   * @param volume The volume to test
   * @param xyz The location to test for volume containment
   * @param result Set to 0 if xyz it outside volume, 1 if inside, and -1 if on boundary.
   * @param Optional direction to use for underlying ray fire query.  Used to ensure
   *        consistent results when a ray direction is known.  If NULL or {0,0,0} is
   *        given, a random direction will be used.
   * @param history Optional RayHistory object to pass to underlying ray fire query.
   *        The history is not modified by this call.
   */
  ErrorCode point_in_volume(const EntityHandle volume,
                            const double xyz[3],
                            int& result,
                            const double* uvw = NULL,
                            const RayHistory* history = NULL );

  /**\brief Robust test if a point is inside or outside a volume using unit sphere area method
   *
   * This test may be more robust that the standard point_in_volume, but is much slower.
   * It does not detect 'on boundary' situations as point_in_volume does.
   * @param volume The volume to test
   * @param xyz The location to test for volume containment
   * @param result Set to 0 if xyz it outside volume, 1 if inside.
   */
  ErrorCode point_in_volume_slow( const EntityHandle volume, const double xyz[3], int& result );

  /**\brief Test if a point is inside or outsize a volume's axis-aligned bounding box
   *
   * This is used as a fast way to determine whether a point is inside or outside of a volume
   * before performing a point_in_volume test which involves firing a ray.
   * @param volume The volume to test
   * @param point The location to test for bounding box containment
   * @param inside set to 0 if point is outside the box, 1 if inside
   */
  ErrorCode point_in_box(const EntityHandle volume, const double point[3], int &inside );

  
  /** \brief Given a ray starting at a surface of a volume, check whether the ray enters or exits the volume
   *
   * This function is most useful for rays that change directions at a surface crossing.
   * It can be used to check whether a direction change redirects the ray back into the originating
   * volume.
   *
   * @param volume The volume to test
   * @param surface A surface on volume
   * @param xyz A point location on surface
   * @param uvw A (unit) direction vector
   * @param result Set to 1 if ray is entering volume, or 0 if it is leaving
   * @param history Optional ray history object from a previous call to ray_fire.  If present and non-empty,
   *        the history is used to look up the surface facet at which the ray begins.  Absent a
   *        history, the facet nearest to xyz will be looked up.  The history should always be
   *        provided if available, as it avoids the computational expense of a nearest-facet query.
   */
  ErrorCode test_volume_boundary( const EntityHandle volume, const EntityHandle surface,
                                  const double xyz[3], const double uvw[3], int& result,
                                  const RayHistory* history = NULL );

  /**\brief Find the distance to the point on the boundary of the volume closest to the test point
   *
   * @param volume Volume to query
   * @param point Coordinates of test point
   * @param result Set to the minimum distance from point to a surface in volume
   */
  ErrorCode closest_to_location( EntityHandle volume, const double point[3], double& result,
				 EntityHandle* closest_surface = 0 );

  /** Calculate the volume contained in a 'volume' */
  ErrorCode measure_volume( EntityHandle volume, double& result );

  /** Calculate sum of area of triangles */
  ErrorCode measure_area( EntityHandle surface, double& result );


  /** Get the normal to a given surface at the point on the surface closest to a given point
   *
   * This method first identifies which facet contains this point and then
   * calculates the unit outward normal of that facet.  The facet of the
   * provided volume that is nearest the provided point is used for this
   * calculation.  The search for that facet can be circumvented by providing
   * a RayHistory, in which case the last facet of the history will be used.
   *
   * @param surf Surface on which to get normal
   * @param xyz Point on surf
   * @param angle Set to coordinates of surface normal nearest xyz
   * @param history Optional ray history from a previous call to ray_fire().
   *        If present and non-empty, return the normal
   *        of the most recently intersected facet, ignoring xyz.
   */
  ErrorCode get_normal(EntityHandle surf, const double xyz[3], double angle[3],
                       const RayHistory* history = NULL );

private:

  /**\brief determine the point membership when the point is effectively on the boundary
   *
   * Called by point_in_volume when the point is with tolerance of the boundary. Compares the
   * ray direction with the surface normal to determine a volume membership.
   */
  ErrorCode boundary_case( EntityHandle volume, int& result,
                             double u, double v, double w,
                             EntityHandle facet,
                             EntityHandle surface);

  /** get the solid angle projected by a facet on a unit sphere around a point
   *  - used by point_in_volume_slow
   */
  ErrorCode poly_solid_angle( EntityHandle face, const CartVect& point, double& area );

  /**\brief State object used in calls to ray_fire()
   *
   * Storage for the "history" of a ray.  This represents the surface facets
   * that the ray is known to have crossed, which cannot be crossed again
   * as long as the ray does not change direction.  It is intended to be used
   * with a series of consecutive calls to ray_fire(), in which a ray passes
   * over potentially many surfaces.
   */

public:

  /*
   Overlap Thickness:
   This tolerance is the maximum distance across an overlap. It should be zero
   unless the geometry has overlaps. The overlap thickness is set using the dagmc
   card. Overlaps must be small enough not to significantly affect physics.
     Performance: increasing tolerance decreases performance
     Robustness:  increasing tolerance increases robustness
     Knowledge:   user must have intuition of overlap thickness
  */
  
  /** Attempt to set a new overlap thickness tolerance, first checking for sanity */

  void set_overlap_thickness( double new_overlap_thickness );
  

  /*    
   Numerical Precision:
   This tolerance is used for obb.intersect_ray, finding neighborhood of
   adjacent triangle for edge/node intersections, and error in advancing
   geometric position of particle (x' ~= x + d*u). When determining the
   neighborhood of adjacent triangles for edge/node intersections, the facet
   based model is expected to be watertight.
     Performance: increasing tolerance decreases performance (but not very much)
     Robustness:  increasing tolerance increases robustness
     Knowledge:   user should not change this tolerance
  */
  
  /** Attempt to set a new numerical precision , first checking for sanity
   *  Use of this function is discouraged.
   */
  void set_numerical_precision( double new_precision );

  double get_numerical_precision() { return numericalPrecision; }

  double get_overlap_thickness() { return overlapThickness; }

  GeomTopoTool* gttool() { return geomTopoTool; }

  Interface* moab_instance() { return MBI; }
  
private:

  GeomTopoTool* geomTopoTool;
  Interface* MBI;
  OrientedBoxTreeTool* obbTreeTool;
  bool counting;
  long long int n_pt_in_vol_calls;
  long long int n_ray_fire_calls;
  double overlapThickness, numericalPrecision;
  Tag senseTag;

};

}

#endif
