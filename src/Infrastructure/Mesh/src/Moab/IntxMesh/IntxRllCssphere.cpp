/*
 * IntxRllCssphere.cpp
 *
 *  Created on: Aug 8, 2014
 *      Author: iulian
 */

#include "moab/IntxMesh/IntxRllCssphere.hpp"
#include "moab/GeomUtil.hpp"
#include "moab/IntxMesh/IntxUtils.hpp"

namespace moab {

IntxRllCssphere::IntxRllCssphere(Interface * mbimpl):Intx2Mesh(mbimpl), R(0.0), plane(0) {
  // TODO Auto-generated constructor stub

}

IntxRllCssphere::~IntxRllCssphere() {
  // TODO Auto-generated destructor stub
}
/*
 * return also the area for robustness verification
 */
double IntxRllCssphere::setup_red_cell(EntityHandle red, int & nsRed){


  // get coordinates of the red quad, to decide the gnomonic plane
  double cellArea =0;

  int num_nodes;
  ErrorCode rval = mb->get_connectivity(red, redConn, num_nodes);

  if (MB_SUCCESS != rval )
    return 1;
  nsRed = num_nodes;
  // these edges will never be polygons, only quads or triangles

  //CartVect coords[4];
  rval = mb->get_coords(redConn, nsRed, &(redCoords[0][0]));
  if (MB_SUCCESS != rval)
    return 1;
  CartVect middle = redCoords[0];
  for (int i=1; i<nsRed; i++)
    middle += redCoords[i];
  middle = 1./nsRed * middle;

  decide_gnomonic_plane(middle, plane);// output the plane
  for (int j = 0; j < nsRed; j++)
  {
    // populate coords in the plane for intersection
    // they should be oriented correctly, positively
    int rc = gnomonic_projection(redCoords[j],  R, plane, redCoords2D[2 * j],
        redCoords2D[2 * j + 1]);
    if (rc != 0)
      return 1;
  }

  for (int j=1; j<nsRed-1; j++)
    cellArea += area2D(&redCoords2D[0], &redCoords2D[2*j], &redCoords2D[2*j+2]);

  // take red coords in order and compute area in plane
  return cellArea;
}

/* the elements are convex for sure, then do a gnomonic projection of both,
 *  compute intersection in the plane, then go back to the sphere for the points
 *  */
ErrorCode IntxRllCssphere::computeIntersectionBetweenRedAndBlue(EntityHandle red, EntityHandle blue,
    double * P, int & nP, double & area, int markb[MAXEDGES], int markr[MAXEDGES],
    int & nsBlue, int & nsRed, bool check_boxes_first)
{
  // the area will be used from now on, to see how well we fill the red cell with polygons
  // the points will be at most 40; they will describe a convex patch, after the points will be ordered and
  // collapsed (eliminate doubles)

  //CartVect bluecoords[4];
  int num_nodes=0;
  ErrorCode rval = mb->get_connectivity(blue, blueConn, num_nodes);MB_CHK_ERR(rval);

  nsBlue = num_nodes;
  rval = mb->get_coords(blueConn, nsBlue, &(blueCoords[0][0]));MB_CHK_ERR(rval);

  // determine the type of edge: const lat or not?
  // just look at the consecutive z coordinates for the edge
  for (int i=0; i<nsBlue; i++)
  {
    int nexti=(i+1)%nsBlue;
    if ( fabs(blueCoords[i][2]- blueCoords[nexti][2]) < 1.e-6 )
      blueEdgeType[i]=1;
    else
      blueEdgeType[i]=0;
  }
  area = 0.;
  nP = 0; // number of intersection points we are marking the boundary of blue!
  if (check_boxes_first)
  {
    // look at the boxes formed with vertices; if they are far away, return false early
    // make sure the red is setup already
    setup_red_cell(red, nsRed); // we do not need area here
    if (!GeomUtil::bounding_boxes_overlap(redCoords, nsRed, blueCoords, nsBlue, box_error))
      return MB_SUCCESS; // no error, but no intersection, decide early to get out
  }
#ifdef ENABLE_DEBUG
  if (dbg_1)
  {
    std::cout << "red " << mb->id_from_handle(red) << "\n";
    for (int j = 0; j < nsRed; j++)
    {
      std::cout << redCoords[j] << "\n";
    }
    std::cout << "blue " << mb->id_from_handle(blue) << "\n";
    for (int j = 0; j < nsBlue; j++)
    {
      std::cout << blueCoords[j] << "\n";
    }
    mb->list_entities(&red, 1);
    mb->list_entities(&blue, 1);
  }
#endif
  for (int j=0; j<nsBlue; j++)
  {
    rval = gnomonic_projection(blueCoords[j], R, plane, blueCoords2D[2 * j],
        blueCoords2D[2 * j + 1]);MB_CHK_ERR(rval);
  }
#ifdef ENABLE_DEBUG
  if (dbg_1)
  {
    std::cout << "gnomonic plane: " << plane << "\n";
    std::cout << " red                                blue\n";
    for (int j = 0; j < nsRed; j++)
    {
      std::cout << redCoords2D[2 * j] << " " << redCoords2D[2 * j + 1] << "\n";
    }
    for (int j = 0; j < nsBlue; j++)
    {
      std::cout << blueCoords2D[2 * j] << " " << blueCoords2D[2 * j + 1] << "\n";
    }
  }
#endif
  rval = EdgeIntxRllCs(blueCoords2D, blueCoords, blueEdgeType, nsBlue, redCoords2D, redCoords, nsRed, markb, markr,
      plane, R, P, nP);MB_CHK_ERR(rval);

  int side[MAXEDGES] = { 0 };// this refers to what side? blue or red?// more tolerant here with epsilon_area
  int extraPoints = borderPointsOfXinY2(blueCoords2D, nsBlue, redCoords2D, nsRed, &(P[2 * nP]), side, 2*epsilon_area);
  if (extraPoints >= 1)
  {
    for (int k = 0; k < nsBlue; k++)
    {
      if (side[k])
      {
        // this means that vertex k of blue is inside convex red; mark edges k-1 and k in blue,
        //   as being "intersected" by red; (even though they might not be intersected by other edges,
        //   the fact that their apex is inside, is good enough)
        markb[k] = 1;
        markb[(k + nsBlue-1) % nsBlue] = 1; // it is the previous edge, actually, but instead of doing -1, it is
        // better to do modulo +3 (modulo 4)
        // null side b for next call
        side[k]=0;
      }
    }
  }
  nP += extraPoints;

  extraPoints = borderPointsOfCSinRLL(redCoords, redCoords2D, nsRed, blueCoords, nsBlue, blueEdgeType, &(P[2 * nP]), side,
      100*epsilon_area); // we need to compare with 0 a volume from 3 vector product; // lots of round off errors at stake
  if (extraPoints >= 1)
  {
    for (int k = 0; k < nsRed; k++)
    {
      if (side[k])
      {
        // this is to mark that red edges k-1 and k are intersecting blue
        markr[k] = 1;
        markr[(k + nsRed-1) % nsRed] = 1; // it is the previous edge, actually, but instead of doing -1, it is
        // better to do modulo +3 (modulo 4)
        // null side b for next call
      }
    }
  }
  nP += extraPoints;

  // now sort and orient the points in P, such that they are forming a convex polygon
  // this will be the foundation of our new mesh
  // this works if the polygons are convex
  SortAndRemoveDoubles2(P, nP, epsilon_1); // nP should be at most 8 in the end ?
  // if there are more than 3 points, some area will be positive

  if (nP >= 3)
  {
    for (int k = 1; k < nP - 1; k++)
      area += area2D(P, &P[2 * k], &P[2 * k + 2]);
  }

  return MB_SUCCESS; // no error
}


// this method will also construct the triangles/polygons in the new mesh
// if we accept planar polygons, we just save them
// also, we could just create new vertices every time, and merge only in the end;
// could be too expensive, and the tolerance for merging could be an
// interesting topic
ErrorCode IntxRllCssphere::findNodes(EntityHandle red, int nsRed, EntityHandle blue, int nsBlue,
    double * iP, int nP)
{
  // first of all, check against red and blue vertices
  //
#ifdef ENABLE_DEBUG
  if (dbg_1)
  {
    std::cout << "red, blue, nP, P " << mb->id_from_handle(red) << " "
        << mb->id_from_handle(blue) << " " << nP << "\n";
    for (int n = 0; n < nP; n++)
      std::cout << " \t" << iP[2 * n] << "\t" << iP[2 * n + 1] << "\n";

  }
#endif

  // get the edges for the red triangle; the extra points will be on those edges, saved as
  // lists (unordered)

  // first get the list of edges adjacent to the red cell
  // use the neighRedEdgeTag
  EntityHandle adjRedEdges[MAXEDGES];
  ErrorCode rval = mb->tag_get_data(neighRedEdgeTag, &red, 1, &(adjRedEdges[0])); MB_CHK_SET_ERR(rval, "can't get edge red tag");
  // we know that we have only nsRed edges here; [nsRed, MAXEDGES) are ignored, but it is small potatoes

  // these will be in the new mesh, mbOut
  // some of them will be handles to the initial vertices from blue or red meshes (lagr or euler)

  EntityHandle * foundIds = new EntityHandle[nP];
  for (int i = 0; i < nP; i++)
  {
    double * pp = &iP[2 * i]; // iP+2*i
    // project the point back on the sphere
    CartVect pos;
    reverse_gnomonic_projection(pp[0], pp[1], R, plane, pos);
    int found = 0;
    // first, are they on vertices from red or blue?
    // priority is the red mesh (mb2?)
    int j = 0;
    EntityHandle outNode = (EntityHandle) 0;
    for (j = 0; j < nsRed && !found; j++)
    {
      //int node = redTri.v[j];
      double d2 = dist2(pp, &redCoords2D[2 * j]);
      if (d2 < epsilon_1)
      {

        foundIds[i] = redConn[j]; // no new node
        found = 1;
#ifdef ENABLE_DEBUG
        if (dbg_1)
          std::cout << "  red node j:" << j << " id:"
              << mb->id_from_handle(redConn[j]) << " 2d coords:" << redCoords2D[2 * j] << "  "
              << redCoords2D[2 * j + 1] << " d2: " << d2 << " \n";
#endif
      }
    }

    for (j = 0; j < nsBlue && !found; j++)
    {
      //int node = blueTri.v[j];
      double d2 = dist2(pp, &blueCoords2D[2 * j]);
      if (d2 < epsilon_1)
      {
        // suspect is blueConn[j] corresponding in mbOut

        foundIds[i] = blueConn[j]; // no new node
        found = 1;
#ifdef ENABLE_DEBUG
        if (dbg_1)
          std::cout << "  blue node " << j << " "
              << mb->id_from_handle(blueConn[j]) << " d2:" << d2 << " \n";
#endif
      }

    }
    if (!found)
    {
      // find the edge it belongs, first, on the red element
      //
      for (j = 0; j < nsRed; j++)
      {
        int j1 = (j + 1) % nsRed;
        double area = area2D(&redCoords2D[2 * j], &redCoords2D[2 * j1], pp);
#ifdef ENABLE_DEBUG
        if (dbg_1)
          std::cout << "   edge " << j << ": "
              << mb->id_from_handle(adjRedEdges[j]) << " " << redConn[j] << " "
              << redConn[j1] << "  area : " << area << "\n";
#endif
        if (fabs(area) < epsilon_1/2)
        {
          // found the edge; now find if there is a point in the list here
          //std::vector<EntityHandle> * expts = extraNodesMap[redEdges[j]];
          int indx = RedEdges.index(adjRedEdges[j]);
          // CID 181167 (#1 of 1): Argument cannot be negative (NEGATIVE_RETURNS)
          if (indx<0)
          {
            std::cerr<<" error in adjacent red edge: " << mb->id_from_handle(adjRedEdges[j])<< "\n";
            delete[] foundIds;
            return MB_FAILURE;
          }
          std::vector<EntityHandle> * expts = extraNodesVec[indx];
          // if the points pp is between extra points, then just give that id
          // if not, create a new point, (check the id)
          // get the coordinates of the extra points so far
          int nbExtraNodesSoFar = expts->size();
          if (nbExtraNodesSoFar>0)
          {
            CartVect * coords1 = new CartVect[nbExtraNodesSoFar];
            mb->get_coords(&(*expts)[0], nbExtraNodesSoFar, &(coords1[0][0]));
            //std::list<int>::iterator it;
            for (int k = 0; k < nbExtraNodesSoFar && !found; k++)
            {
              //int pnt = *it;
              double d2 = (pos - coords1[k]).length_squared();
              if (d2 < epsilon_1)
              {
                found = 1;
                foundIds[i] = (*expts)[k];
#ifdef ENABLE_DEBUG
                if (dbg_1)
                  std::cout << " found node:" << foundIds[i] << std::endl;
#endif
              }
            }
            delete[] coords1;
          }
          if (!found)
          {
            // create a new point in 2d (at the intersection)
            //foundIds[i] = m_num2dPoints;
            //expts.push_back(m_num2dPoints);
            // need to create a new node in mbOut
            // this will be on the edge, and it will be added to the local list
            mb->create_vertex(pos.array(), outNode);
            (*expts).push_back(outNode);
            foundIds[i] = outNode;
            found = 1;
#ifdef ENABLE_DEBUG
            if (dbg_1)
              std::cout << " new node: " << outNode << std::endl;
#endif
          }

        }
      }
    }
    if (!found)
    {
      std::cout << " red quad: ";
      for (int j1 = 0; j1 < nsRed; j1++)
      {
        std::cout << redCoords2D[2 * j1] << " " << redCoords2D[2 * j1 + 1] << "\n";
      }
      std::cout << " a point pp is not on a red quad " << *pp << " " << pp[1]
          << " red quad " << mb->id_from_handle(red) << " \n";
      delete[] foundIds;
      return MB_FAILURE;
    }
  }
#ifdef ENABLE_DEBUG
  if (dbg_1)
  {
    std::cout << " candidate polygon: nP" << nP <<  " plane: " << plane << "\n";
    for (int i1 = 0; i1 < nP; i1++)
            std::cout << iP[2 * i1] << " " << iP[2 * i1 + 1] << " " << foundIds[i1] << "\n";
  }
#endif
  // first, find out if we have nodes collapsed; shrink them
  // we may have to reduce nP
  // it is possible that some nodes are collapsed after intersection only
  // nodes will always be in order (convex intersection)
  correct_polygon(foundIds, nP);
  // now we can build the triangles, from P array, with foundIds
  // we will put them in the out set
  if (nP >= 3)
  {
    EntityHandle polyNew;
    mb->create_element(MBPOLYGON, foundIds, nP, polyNew);
    mb->add_entities(outSet, &polyNew, 1);

    // tag it with the index ids from red and blue sets
    int id = rs1.index(blue); // index starts from 0
    mb->tag_set_data(blueParentTag, &polyNew, 1, &id);
    id = rs2.index(red);
    mb->tag_set_data(redParentTag, &polyNew, 1, &id);

    counting++;
    mb->tag_set_data(countTag, &polyNew, 1, &counting);

#ifdef ENABLE_DEBUG
    if (dbg_1)
    {

      std::cout << "Counting: " << counting << "\n";
      std::cout << " polygon " << mb->id_from_handle(polyNew) << "  nodes: " << nP << " :";
      for (int i1 = 0; i1 < nP; i1++)
        std::cout << " " << mb->id_from_handle(foundIds[i1]);
      std::cout << " plane: " << plane << "\n";
      std::vector<CartVect> posi(nP);
      mb->get_coords(foundIds, nP, &(posi[0][0]));
      for (int i1 = 0; i1 < nP; i1++)
        std::cout << foundIds[i1]<< " " << posi[i1] << "\n";

      std::stringstream fff;
      fff << "file0" <<  counting<< ".vtk";
         mb->write_mesh(fff.str().c_str(), &outSet, 1);
    }
#endif
  }
  //disable_debug();
  delete[] foundIds;
  foundIds = NULL;
  return MB_SUCCESS;
}

} /* namespace moab */
