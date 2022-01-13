/*
 * IntxRllCssphere.hpp
 *
 */

#ifndef INTXRLLCSSPHERE_HPP_
#define INTXRLLCSSPHERE_HPP_

#include "Intx2Mesh.hpp"

namespace moab
{

class IntxRllCssphere : public moab::Intx2Mesh
{
  public:
    IntxRllCssphere( Interface* mbimpl );

    virtual ~IntxRllCssphere();

    void set_radius( double radius )
    {
        R = radius;
    }

    double setup_tgt_cell( EntityHandle tgt, int& nsTgt );

    // src cell will be always lat lon cell, so it will be a rectangle in lat-lon coors
    // it will be used for "interior" determinations of other points
    // double setup_src_cell(EntityHandle src, int & nsSrc);

    // main method to intersect meshes on a sphere

    ErrorCode computeIntersectionBetweenTgtAndSrc( EntityHandle tgt, EntityHandle src, double* P, int& nP, double& area,
                                                   int markb[MAXEDGES], int markr[MAXEDGES], int& nsSrc, int& nsTgt,
                                                   bool check_boxes_first = false );

    ErrorCode findNodes( EntityHandle tgt, int nsTgt, EntityHandle src, int nsSrc, double* iP, int nP );

  private:
    double R;            // radius of the sphere
    int plane;           // current gnomonic plane, will still be used for projection
    int srcEdgeType[4];  // at most 4
    // these could be from [-PI/2, +PI/2] and [0 to 2*PI]
};

} /* namespace moab */
#endif /* INTXRLLCSSPHERE_HPP_ */
