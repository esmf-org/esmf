/*
 * Intx2MeshOnSphere.hpp
 *
 *  Created on: Oct 3, 2012
 *      Author: iulian
 */

#ifndef INTX2MESHONSPHERE_HPP_
#define INTX2MESHONSPHERE_HPP_

#include "Intx2Mesh.hpp"

namespace moab {

class Intx2MeshOnSphere: public moab::Intx2Mesh
{
public:
  Intx2MeshOnSphere(Interface * mbimpl);
  virtual ~Intx2MeshOnSphere();

  void set_radius_source_mesh(double radius) { Rsrc=radius ;}
  void set_radius_destination_mesh(double radius) { Rdest=radius ;}

  double setup_red_cell(EntityHandle red, int & nsRed);

  // main method to intersect meshes on a sphere

  ErrorCode computeIntersectionBetweenRedAndBlue(EntityHandle red, EntityHandle blue,
          double * P, int & nP, double & area, int markb[MAXEDGES], int markr[MAXEDGES],
          int & nsBlue, int & nsRed, bool check_boxes_first=false);

  ErrorCode findNodes(EntityHandle red, int nsRed, EntityHandle blue, int nsBlue,
      double * iP, int nP);

  ErrorCode update_tracer_data(EntityHandle out_set, Tag & tagElem, Tag & tagArea);
#ifdef MOAB_HAVE_MPI
  virtual ErrorCode construct_covering_set(EntityHandle & initial_distributed_set, EntityHandle & covering_set);

  virtual ErrorCode build_processor_euler_boxes(EntityHandle euler_set, Range & local_verts);
#endif

private:
  int plane; // current gnomonic plane
  double Rsrc, Rdest; // radius of the sphere


};

} /* namespace moab */
#endif /* INTX2MESHONSPHERE_HPP_ */
