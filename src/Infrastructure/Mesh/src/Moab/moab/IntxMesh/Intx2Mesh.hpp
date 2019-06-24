/*
 * Intx2Mesh.hpp
 *
 *  Created on: Oct 2, 2012
 *
 */

#ifndef INTX2MESH_HPP_
#define INTX2MESH_HPP_

#include <iostream>
#include <sstream>
#include <fstream>
#include <map>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "moab/Core.hpp"
#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/CartVect.hpp"

// #define ENABLE_DEBUG

// maximum number of edges on each convex polygon of interest
#define MAXEDGES 10
#define MAXEDGES2 20 // used for coordinates in plane
#define CORRTAGNAME "__correspondent"

namespace moab {

#define ERRORR(rval, str) \
    if (MB_SUCCESS != rval) \
           {std::cout << str << "\n"; return rval;}

#define ERRORV(rval, str) \
    if (MB_SUCCESS != rval) \
           {std::cout << str << "\n"; return ;}

#ifdef MOAB_HAVE_MPI
// forward declarations
class ParallelComm;
class TupleList;
#endif

class Intx2Mesh
{
public:
  Intx2Mesh(Interface * mbimpl);
  virtual ~Intx2Mesh();

  /*
   * computes intersection between 2 sets;
   * set 2 (mbs2) should be completely covered by set mbs1, as all elements
   * from set 2 will be decomposed in polygons ; an area verification is done, and
   * will signal elements from set 2 that are not "recovered"
   *
   * the resulting polygons are inserted in output set; total area from output set should be
   * equal to total area of elements in set 2 (check that!)
   */
  ErrorCode intersect_meshes(EntityHandle mbs1, EntityHandle mbs2,
       EntityHandle & outputSet);

  // mark could be (3 or 4, depending on type: ) no, it could go to 10
  // no, it will be MAXEDGES = 10
  // this is pure abstract, this needs to be implemented by
  // all derivations
  // the max number of intersection points could be 2*MAXEDGES
  // so P must be dimensioned to 4*MAXEDGES (2*2*MAXEDGES)
  // so, if you intersect 2 convex polygons with MAXEDGES , you will get a convex polygon
  // with 2*MAXEDGES, at most
  // will also return the number of nodes of red and blue elements
  virtual ErrorCode computeIntersectionBetweenRedAndBlue(EntityHandle red,
      EntityHandle blue, double * P, int & nP, double & area,
      int markb[MAXEDGES], int markr[MAXEDGES], int & nsidesBlue,
      int & nsidesRed, bool check_boxes_first=false)=0;

  // this is also abstract
  virtual ErrorCode findNodes(EntityHandle red, int nsRed, EntityHandle blue, int nsBlue,
      double * iP, int nP)=0;

  // this is also computing the area of the red cell in plane (gnomonic plane for sphere)
  // setting the local variables:
  // this will be done once per red cell, and once per localQueue for blue cells
  //  const EntityHandle * redConn;
  // CartVect redCoords[MAXEDGES];
  // double redCoords2D[MAXEDGES2]; // these are in plane

  virtual double setup_red_cell(EntityHandle red, int & nsRed)= 0;

  virtual ErrorCode FindMaxEdgesInSet(EntityHandle eset, int & max_edges);
  virtual ErrorCode FindMaxEdges(EntityHandle set1, EntityHandle set2); // this needs to be called before any covering communication in parallel

  virtual ErrorCode createTags();

  ErrorCode DetermineOrderedNeighbors(EntityHandle inputSet, int max_edges, Tag & neighTag);

  void set_error_tolerance(double eps) { epsilon_1=eps; epsilon_area = eps*eps/2;}

#ifdef MOAB_HAVE_MPI
  void set_parallel_comm(moab::ParallelComm* pcomm) { parcomm = pcomm; }
#endif
  //void SetEntityType (EntityType tp) { type=tp;}

  // clean some memory allocated
  void clean();

  void set_box_error(double berror)
   {box_error = berror;}

  ErrorCode create_departure_mesh_2nd_alg(EntityHandle & euler_set, EntityHandle & covering_lagr_set);

  // in this method, used in parallel, each departure elements are already created, and at their positions
  // the covering_set is output, will contain the departure cells that cover the euler set; some of these
  // departure cells might come from different processors
  // so the covering_set contains some elements from lagr_set and some elements that come from other procs
  // we need to keep track of what processors "sent" the elements so we know were to
  // send back the info about the tracers masses

  ErrorCode create_departure_mesh_3rd_alg(EntityHandle & lagr_set, EntityHandle & covering_set);

  /* in this method, used in parallel, each cell from first mesh need to be sent to the
   * tasks whose second mesh bounding boxes intersects bounding box of the cell
   * this method assumes that the bounding boxes for the second mesh were computed already in a previous method
   * (called build_processor_euler_boxes)
   *
   * the covering_set is output,  will cover the second mesh (set) from the task;
   * Some of the cells in the first mesh will be sent to multiple processors; we keep track of them using the global id
   * of the  vertices and global ids of the cells (we do not want to create multiple vertices with the
   * same ids). The global id of the cells are needed just for debugging, the cells cannot come from 2 different
   * tasks, but the vertices can
   *
   * Right now, we use crystal router, but an improvement might be to use direct communication (send_entities)
   * on parallel comm
   *
   * param initial_distributed_set (IN) : the initial distribution of the first mesh (set)
   *
   * param (OUT) : the covering set in first mesh , which completely covers the second mesh set
  */
#ifdef MOAB_HAVE_MPI

  virtual ErrorCode build_processor_euler_boxes(EntityHandle euler_set, Range & local_verts);
#endif
  void correct_polygon(EntityHandle * foundIds, int & nP);
#ifdef MOAB_HAVE_MPI
  ErrorCode correct_intersection_points_positions();
#endif
#ifdef ENABLE_DEBUG
  void enable_debug()  {dbg_1 = 1;}
  void disable_debug() {dbg_1 = 0;}
#endif
protected: // so it can be accessed in derived classes, InPlane and OnSphere
  Interface * mb;

  EntityHandle mbs1;
  EntityHandle mbs2;
  Range rs1;// range set 1 (departure set, lagrange set, blue set, manufactured set, target mesh)
  Range rs2;// range set 2 (arrival set, euler set, red set, initial set, source mesh)

  EntityHandle outSet; // will contain intersection
  Tag gid; // global id tag will be used to set the parents of the intersection cell

  // tags used in computation, advancing front
  Tag RedFlagTag; // to mark red quads already considered

  Range RedEdges; //

  // red parent and blue parent tags
  // these will be on the out mesh
  Tag redParentTag;
  Tag blueParentTag;
  Tag countTag;

  Tag blueNeighTag; // will store neighbors for navigating easily in advancing front, for first mesh (blue, target, lagrange)
  Tag redNeighTag; // will store neighbors for navigating easily in advancing front, for second mesh (red, source, euler)

  Tag neighRedEdgeTag; // will store edge borders for each red cell

  Tag orgSendProcTag; /// for coverage mesh, will store the original sender

  //EntityType type; // this will be tri, quad or MBPOLYGON...

  const EntityHandle * redConn;
  const EntityHandle * blueConn;
  CartVect redCoords[MAXEDGES];
  CartVect blueCoords[MAXEDGES];
  double redCoords2D[MAXEDGES2]; // these are in plane
  double blueCoords2D[MAXEDGES2]; // these are in plane

#ifdef ENABLE_DEBUG
  static int dbg_1;
  std::ofstream mout_1[6]; // some debug files
#endif
  // for each red edge, we keep a vector of extra nodes, coming from intersections
  // use the index in RedEdges range
  // so the extra nodes on each red edge are kept track of
  // only entity handles are in the vector, not the actual coordinates;
  // actual coordinates are retrieved every time, which could be expensive
  // maybe we should store the coordinates too, along with entity handles (more memory used, faster to retrieve)
  std::vector<std::vector<EntityHandle> *> extraNodesVec;

  double epsilon_1;
  double epsilon_area;

  std::vector<double> allBoxes;
  double box_error;
  /* \brief Local root of the kdtree */
  EntityHandle localRoot;
  Range localEnts;// this range is for local elements of interest, euler cells, or "first mesh"
  unsigned int my_rank;

#ifdef MOAB_HAVE_MPI
  ParallelComm * parcomm;
  TupleList * remote_cells; // not used anymore for communication, just a container
  TupleList * remote_cells_with_tracers; // these will be used now to update tracers on remote procs
  std::map<int, EntityHandle> globalID_to_eh;// needed for parallel, mostly
#endif
  int max_edges_1; // maximum number of edges in the lagrange set (first set, blue)
  int max_edges_2; // maximum number of edges in the euler set (second set, red)
  int counting;

};

} /* namespace moab */
#endif /* INTX2MESH_HPP_ */
