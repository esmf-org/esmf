/*
 * IntxRllCssphere.hpp
 *
 */

#ifndef INTXRLLCSSPHERE_HPP_
#define INTXRLLCSSPHERE_HPP_

#include "Intx2Mesh.hpp"

namespace moab {

class IntxRllCssphere: public moab::Intx2Mesh {
public:
  IntxRllCssphere(Interface * mbimpl);
  virtual ~IntxRllCssphere();

  void set_radius(double radius) { R=radius ;}

  double setup_red_cell(EntityHandle red, int & nsRed);

  // blue cell will be always lat lon cell, so it will be a rectangle in lat-lon coors
  // it will be used for "interior" determinations of other points
  //double setup_blue_cell(EntityHandle red, int & nsRed);

  // main method to intersect meshes on a sphere

  ErrorCode computeIntersectionBetweenRedAndBlue(EntityHandle red, EntityHandle blue,
          double * P, int & nP, double & area, int markb[MAXEDGES], int markr[MAXEDGES],
          int & nsBlue, int & nsRed, bool check_boxes_first=false);

  ErrorCode findNodes(EntityHandle red, int nsRed, EntityHandle blue, int nsBlue,
      double * iP, int nP);

private:
  double R; // radius of the sphere
  int plane; // current gnomonic plane, will still be used for projection
  int blueEdgeType[4]; // at most 4
  // these could be from [-PI/2, +PI/2] and [0 to 2*PI]
};

} /* namespace moab */
#endif /* INTXRLLCSSPHERE_HPP_ */
