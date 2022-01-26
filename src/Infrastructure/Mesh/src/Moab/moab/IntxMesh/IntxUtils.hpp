/*
 * IntxUtils.hpp
 *
 *  Created on: Oct 3, 2012
 */

#ifndef INTXUTILS_HPP_
#define INTXUTILS_HPP_

#include "moab/CartVect.hpp"
#include "moab/Core.hpp"

namespace moab
{

class IntxUtils
{
  public:
    // vec utilities that could be common between quads on a plane or sphere
    static inline double dist2( double* a, double* b )
    {
        double abx = b[0] - a[0], aby = b[1] - a[1];
        return sqrt( abx * abx + aby * aby );
    }

    static inline double area2D( double* a, double* b, double* c )
    {
        // (b-a)x(c-a) / 2
        return ( ( b[0] - a[0] ) * ( c[1] - a[1] ) - ( b[1] - a[1] ) * ( c[0] - a[0] ) ) / 2;
    }

    static int borderPointsOfXinY2( double* X, int nX, double* Y, int nY, double* P, int* side, double epsilon_area );

    static int SortAndRemoveDoubles2( double* P, int& nP, double epsilon );
    // the marks will show what edges of blue intersect the red

    static ErrorCode EdgeIntersections2( double* blue, int nsBlue, double* red, int nsRed, int* markb, int* markr,
                                         double* points, int& nPoints );

    // special one, for intersection between rll (constant latitude)  and cs quads
    static ErrorCode EdgeIntxRllCs( double* blue, CartVect* bluec, int* blueEdgeType, int nsBlue, double* red,
                                    CartVect* redc, int nsRed, int* markb, int* markr, int plane, double Radius,
                                    double* points, int& nPoints );

    // vec utils related to gnomonic projection on a sphere
    // vec utils
    /*
     *
     * position on a sphere of radius R
     * if plane specified, use it; if not, return the plane, and the point in the plane
     * there are 6 planes, numbered 1 to 6
     * plane 1: x=R, plane 2: y=R, 3: x=-R, 4: y=-R, 5: z=-R, 6: z=R
     *
     * projection on the plane will preserve the orientation, such that a triangle, quad pointing
     * outside the sphere will have a positive orientation in the projection plane
     * this is similar logic to
     * /cesm1_0_4/models/atm/cam/src/dynamics/homme/share/coordinate_systems_mod.F90 method:
     * function cart2face(cart3D) result(face_no)
     */
    static void decide_gnomonic_plane( const CartVect& pos, int& oPlane );

    // point on a sphere is projected on one of six planes, decided earlier

    static ErrorCode gnomonic_projection( const CartVect& pos, double R, int plane, double& c1, double& c2 );

    // given the position on plane (one out of 6), find out the position on sphere
    static ErrorCode reverse_gnomonic_projection( const double& c1, const double& c2, double R, int plane,
                                                  CartVect& pos );

    // given a mesh on the sphere, project all centers in 6 gnomonic planes, or project mesh too
    static void gnomonic_unroll( double& c1, double& c2, double R, int plane );

    // given a mesh on the sphere, project all centers in 6 gnomonic planes, or project mesh too
    static ErrorCode global_gnomonic_projection( Interface* mb, EntityHandle inSet, double R, bool centers_only,
                                                 EntityHandle& outSet );

    static void transform_coordinates( double* avg_position, int projection_type );
    /*
    *   other methods to convert from spherical coord to cartesian, and back
    *   A spherical coordinate triple is (R, lon, lat)
    *   should we store it as a CartVect? probably not ...
    *   /cesm1_0_4/models/atm/cam/src/dynamics/homme/share/coordinate_systems_mod.F90
    *
        enforce three facts:
        ! ==========================================================
        ! enforce three facts:
        !
        ! 1) lon at poles is defined to be zero
        !
        ! 2) Grid points must be separated by about .01 Meter (on earth)
        !    from pole to be considered "not the pole".
        !
        ! 3) range of lon is { 0<= lon < 2*pi }
        !
        ! 4) range of lat is from -pi/2 to pi/2; -pi/2 or pi/2 are the poles, so there the lon is 0
        !
        ! ==========================================================
    */
    struct SphereCoords
    {
        double R, lon, lat;
    };

    static SphereCoords cart_to_spherical( CartVect& );

    static CartVect spherical_to_cart( SphereCoords& );

    /*
     *  create a mesh used mainly for visualization for now, with nodes corresponding to
     *   GL points, a so-called refined mesh, with NP nodes in each direction.
     *   input: a range of quads (coarse), and  a desired order (NP is the number of points), so it
     *   is order + 1
     *
     *   output: a set with refined elements; with proper input, it should be pretty
     *   similar to a Homme mesh read with ReadNC
     */
    // ErrorCode SpectralVisuMesh(Interface * mb, Range & input, int NP, EntityHandle & outputSet,
    // double tolerance);

    /*
     * given an entity set, get all nodes and project them on a sphere with given radius
     */
    static ErrorCode ScaleToRadius( Interface* mb, EntityHandle set, double R );

    static double distance_on_great_circle( CartVect& p1, CartVect& p2 );

    // break the nonconvex quads into triangles; remove the quad from the set? yes.
    // maybe radius is not needed;
    //
    static ErrorCode enforce_convexity( Interface* mb, EntityHandle set, int rank = 0 );

    // this could be larger than PI, because of orientation; useful for non-convex polygons
    static double oriented_spherical_angle( double* A, double* B, double* C );

    // looking at quad connectivity, collapse to triangle if 2 nodes equal
    // then delete the old quad
    static ErrorCode fix_degenerate_quads( Interface* mb, EntityHandle set );

    // distance along a great circle on a sphere of radius 1
    static double distance_on_sphere( double la1, double te1, double la2, double te2 );

    /* End Analytical functions */

    /*
     * given 2 arcs AB and CD, compute the unique intersection point, if it exists
     *  in between
     */
    static ErrorCode intersect_great_circle_arcs( double* A, double* B, double* C, double* D, double R, double* E );
    /*
     * given 2 arcs AB and CD, compute the intersection points, if it exists
     *  AB is a great circle arc
     *  CD is a constant latitude arc
     */
    static ErrorCode intersect_great_circle_arc_with_clat_arc( double* A, double* B, double* C, double* D, double R,
                                                               double* E, int& np );

    // ErrorCode  set_edge_type_flag(Interface * mb, EntityHandle sf1);

    static int borderPointsOfCSinRLL( CartVect* redc, double* red2dc, int nsRed, CartVect* bluec, int nsBlue,
                                      int* blueEdgeType, double* P, int* side, double epsil );

    // used only by homme
    static ErrorCode deep_copy_set_with_quads( Interface* mb, EntityHandle source_set, EntityHandle dest_set );

    // used to 'repair' scrip-like meshes
    static ErrorCode remove_duplicate_vertices( Interface* mb, EntityHandle file_set, double merge_tol,
                                                std::vector< Tag >& tagList );
};

class IntxAreaUtils
{
  public:
    enum AreaMethod
    {
        lHuiller        = 0,
        Girard          = 1,
        GaussQuadrature = 2
    };

    IntxAreaUtils( AreaMethod p_eAreaMethod = lHuiller ) : m_eAreaMethod( p_eAreaMethod ) {}

    ~IntxAreaUtils() {}

    /*
     * utilities to compute area of a polygon on which all edges are arcs of great circles on a
     * sphere
     */
    /*
     * this will compute the spherical angle ABC, when A, B, C are on a sphere of radius R
     *  the radius will not be needed, usually, just for verification the points are indeed on that
     * sphere the center of the sphere is at origin (0,0,0) this angle can be used in Girard's
     * theorem to compute the area of a spherical polygon
     */
    double spherical_angle( double* A, double* B, double* C, double Radius );

    double area_spherical_triangle( double* A, double* B, double* C, double Radius );

    double area_spherical_polygon( double* A, int N, double Radius, int* sign = NULL );

    double area_spherical_element( Interface* mb, EntityHandle elem, double R );

    double area_on_sphere( Interface* mb, EntityHandle set, double R );

    ErrorCode positive_orientation( Interface* mb, EntityHandle set, double R );

  private:
    /* lHuiller method for computing area on a spherical triangle */
    double area_spherical_triangle_lHuiller( double* ptA, double* ptB, double* ptC, double Radius );

    /* lHuiller method for computing area on a spherical polygon */
    double area_spherical_polygon_lHuiller( double* A, int N, double Radius, int* sign = NULL );

    /* Girard method for computing area on a spherical triangle with spherical excess */
    double area_spherical_triangle_girard( double* A, double* B, double* C, double Radius );

    /* Girard method for computing area on a spherical polygon with spherical excess */
    double area_spherical_polygon_girard( double* A, int N, double Radius );

#ifdef MOAB_HAVE_TEMPESTREMAP
    /* Gauss-quadrature based integration method for computing area on a spherical triangle */
    double area_spherical_triangle_GQ( double* ptA, double* ptB, double* ptC, double Radius );

    /* Gauss-quadrature based integration method for computing area on a spherical polygon element
     */
    double area_spherical_polygon_GQ( double* A, int N, double Radius );
#endif

  private:
    /* data */
    AreaMethod m_eAreaMethod;
};

}  // namespace moab
#endif /* INTXUTILS_HPP_ */
