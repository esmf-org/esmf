/*
 * Intx2MeshOnSphere.cpp
 *
 *  Created on: Oct 3, 2012
 */

#include "moab/IntxMesh/Intx2MeshOnSphere.hpp"
#include "moab/IntxMesh/IntxUtils.hpp"
#include "moab/GeomUtil.hpp"
#ifdef MOAB_HAVE_MPI
#include "moab/ParallelComm.hpp"
#endif
#include "MBTagConventions.hpp"

// #define ENABLE_DEBUG

namespace moab {


Intx2MeshOnSphere::Intx2MeshOnSphere(Interface * mbimpl):Intx2Mesh(mbimpl), plane(0), Rsrc(0.0), Rdest(0.0)
{
  // TODO Auto-generated constructor stub

}

Intx2MeshOnSphere::~Intx2MeshOnSphere()
{
  // TODO Auto-generated destructor stub
}

/*
 * return also the area for robustness verification
 */
double Intx2MeshOnSphere::setup_red_cell(EntityHandle red, int & nsRed) {

  // get coordinates of the red quad, to decide the gnomonic plane
  double cellArea =0;

  int num_nodes;
  ErrorCode rval = mb->get_connectivity(red, redConn, num_nodes);MB_CHK_ERR_RET_VAL(rval,cellArea);

  nsRed = num_nodes;
  // account for possible padded polygons
  while (redConn[nsRed-2]==redConn[nsRed-1] && nsRed>3)
    nsRed--;

  //CartVect coords[4];
  rval = mb->get_coords(redConn, nsRed, &(redCoords[0][0]));MB_CHK_ERR_RET_VAL(rval,cellArea);

  CartVect middle = redCoords[0];
  for (int i=1; i<nsRed; i++)
    middle += redCoords[i];
  middle = 1./nsRed * middle;

  decide_gnomonic_plane(middle, plane);// output the plane
  for (int j = 0; j < nsRed; j++)
  {
    // populate coords in the plane for intersection
    // they should be oriented correctly, positively
    rval = gnomonic_projection(redCoords[j],  Rdest, plane, redCoords2D[2 * j],
        redCoords2D[2 * j + 1]);MB_CHK_ERR_RET_VAL(rval,cellArea);
  }

  for (int j=1; j<nsRed-1; j++)
    cellArea += area2D(&redCoords2D[0], &redCoords2D[2*j], &redCoords2D[2*j+2]);

  // take red coords in order and compute area in plane
  return cellArea;
}

/* the elements are convex for sure, then do a gnomonic projection of both,
 *  compute intersection in the plane, then go back to the sphere for the points
 *  */
ErrorCode Intx2MeshOnSphere::computeIntersectionBetweenRedAndBlue(EntityHandle red, EntityHandle blue,
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
  // account for possible padded polygons
  while (blueConn[nsBlue-2]==blueConn[nsBlue-1] && nsBlue>3)
    nsBlue--;
  rval = mb->get_coords(blueConn, nsBlue, &(blueCoords[0][0]));MB_CHK_ERR(rval);

  area = 0.;
  nP = 0; // number of intersection points we are marking the boundary of blue!
  if (check_boxes_first)
  {
    // look at the boxes formed with vertices; if they are far away, return false early
    // make sure the red is setup already
    setup_red_cell(red, nsRed); // we do not need area here
    // use here gnomonic plane (plane) to see where blue is
    bool overlap3d = GeomUtil::bounding_boxes_overlap(redCoords, nsRed, blueCoords, nsBlue, box_error);
    int planeb;
    CartVect mid3 = (blueCoords[0]+blueCoords[1]+blueCoords[2])/3;
    decide_gnomonic_plane(mid3, planeb);
    if (!overlap3d && (plane!=planeb)) // plane was set at setup_red_cell
      return MB_SUCCESS; // no error, but no intersection, decide early to get out
    // if same plane, still check for gnomonic plane in 2d
    // if no overlap in 2d, get out
    if (!overlap3d && plane==planeb) // CHECK 2D too
    {
      for (int j=0; j<nsBlue; j++)
      {
        rval = gnomonic_projection(blueCoords[j], Rsrc, plane, blueCoords2D[2 * j],
            blueCoords2D[2 * j + 1]);MB_CHK_ERR(rval);
      }
      bool overlap2d = GeomUtil::bounding_boxes_overlap_2d (blueCoords2D, nsBlue, redCoords2D, nsRed, box_error);
      if (!overlap2d)
        return MB_SUCCESS; // we are sure they are not overlapping in 2d , either
    }

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
    rval = gnomonic_projection(blueCoords[j], Rsrc, plane, blueCoords2D[2 * j],
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

  rval = EdgeIntersections2(blueCoords2D, nsBlue, redCoords2D, nsRed, markb, markr, P, nP);MB_CHK_ERR(rval);

  int side[MAXEDGES] = { 0 };// this refers to what side? blue or red?
  int extraPoints = borderPointsOfXinY2(blueCoords2D, nsBlue, redCoords2D, nsRed, &(P[2 * nP]), side, epsilon_area);
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

  extraPoints = borderPointsOfXinY2(redCoords2D, nsRed, blueCoords2D, nsBlue, &(P[2 * nP]), side, epsilon_area);
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


// this method will also construct the triangles/quads/polygons in the new mesh
// if we accept planar polygons, we just save them
// also, we could just create new vertices every time, and merge only in the end;
// could be too expensive, and the tolerance for merging could be an
// interesting topic
ErrorCode Intx2MeshOnSphere::findNodes(EntityHandle red, int nsRed, EntityHandle blue, int nsBlue,
    double * iP, int nP)
{
#ifdef ENABLE_DEBUG
  // first of all, check against red and blue vertices
  //
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
  // some of them will be handles to the initial vertices from blue or red meshes

  std::vector<EntityHandle> foundIds;
  foundIds.resize(nP);
  for (int i = 0; i < nP; i++)
  {
    double * pp = &iP[2 * i]; // iP+2*i
    // project the point back on the sphere
    CartVect pos;
    reverse_gnomonic_projection(pp[0], pp[1], Rdest, plane, pos);
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
        if (fabs(area) < epsilon_1/2) // this should be some sort of machine epsilon
        {
          // found the edge; now find if there is a point in the list here
          //std::vector<EntityHandle> * expts = extraNodesMap[redEdges[j]];
          int indx = RedEdges.index(adjRedEdges[j]);
          if (indx<0) // CID 181166 (#1 of 1): Argument cannot be negative (NEGATIVE_RETURNS)
          {
            std::cerr<<" error in adjacent red edge: " << mb->id_from_handle(adjRedEdges[j])<< "\n";
            return MB_FAILURE;
          }
          std::vector<EntityHandle> * expts = extraNodesVec[indx];
          // if the points pp is between extra points, then just give that id
          // if not, create a new point, (check the id)
          // get the coordinates of the extra points so far
          int nbExtraNodesSoFar = expts->size();
          if (nbExtraNodesSoFar>0)
          {
            std::vector<CartVect>  coords1;
            coords1.resize(nbExtraNodesSoFar);
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
          }
          if (!found)
          {
            // create a new point in 2d (at the intersection)
            //foundIds[i] = m_num2dPoints;
            //expts.push_back(m_num2dPoints);
            // need to create a new node in mbOut
            // this will be on the edge, and it will be added to the local list
            rval = mb->create_vertex(pos.array(), outNode); MB_CHK_ERR(rval);
            (*expts).push_back(outNode);
            // CID 181168; avoid leak storage error
            rval = mb->add_entities(outSet, &outNode, 1); MB_CHK_ERR(rval);
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
  correct_polygon(&foundIds[0], nP);
  // now we can build the triangles, from P array, with foundIds
  // we will put them in the out set
  if (nP >= 3)
  {
    EntityHandle polyNew;
    rval = mb->create_element(MBPOLYGON, &foundIds[0], nP, polyNew);MB_CHK_ERR(rval);
    rval = mb->add_entities(outSet, &polyNew, 1);MB_CHK_ERR(rval);

    // tag it with the global ids from red and blue elements
    int globalID;
    rval = mb->tag_get_data(gid, &blue, 1, &globalID);MB_CHK_ERR(rval);
    rval = mb->tag_set_data(blueParentTag, &polyNew, 1, &globalID);MB_CHK_ERR(rval);
    // if(!parcomm->rank()) std::cout << "Setting parent for " << mb->id_from_handle(polyNew) << " : Blue = " << globalID << ", " << mb->id_from_handle(blue) << "\t\n";
    rval = mb->tag_get_data(gid, &red, 1, &globalID);MB_CHK_ERR(rval);
    rval = mb->tag_set_data(redParentTag, &polyNew, 1, &globalID);MB_CHK_ERR(rval);
    // if(parcomm->rank()) std::cout << "Setting parent for " << mb->id_from_handle(polyNew) << " : Red = " << globalID << ", " << mb->id_from_handle(red) << "\n";

    counting++;
    rval = mb->tag_set_data(countTag, &polyNew, 1, &counting);MB_CHK_ERR(rval);
    if (orgSendProcTag)
    {
      int org_proc=-1;
      rval = mb->tag_get_data(orgSendProcTag, &blue, 1, &org_proc);MB_CHK_ERR(rval);
      rval = mb->tag_set_data(orgSendProcTag, &polyNew, 1, &org_proc);MB_CHK_ERR(rval);// yet another tag
    }

#ifdef ENABLE_DEBUG
    if (dbg_1)
    {
      std::cout << "Counting: " << counting << "\n";
      std::cout << " polygon " << mb->id_from_handle(polyNew) << "  nodes: " << nP << " :";
      for (int i1 = 0; i1 < nP; i1++)
        std::cout << " " << mb->id_from_handle(foundIds[i1]);
      std::cout << " plane: " << plane << "\n";
      std::vector<CartVect> posi(nP);
      mb->get_coords(&foundIds[0], nP, &(posi[0][0]));
      for (int i1 = 0; i1 < nP; i1++)
        std::cout << foundIds[i1]<< " " << posi[i1] << "\n";

      std::stringstream fff;
      fff << "file0" <<  counting<< ".vtk";
      rval = mb->write_mesh(fff.str().c_str(), &outSet, 1);MB_CHK_ERR(rval);
    }
#endif

  }
  // else {
  //   std::cout << "[[FAILURE]] Number of vertices in polygon is less than 3\n";
  // }
  //disable_debug();
  return MB_SUCCESS;
}

ErrorCode Intx2MeshOnSphere::update_tracer_data(EntityHandle out_set, Tag & tagElem, Tag & tagArea)
{
  EntityHandle dum = 0;

  Tag corrTag;
  ErrorCode rval = mb->tag_get_handle(CORRTAGNAME,
                                           1, MB_TYPE_HANDLE, corrTag,
                                           MB_TAG_DENSE, &dum); // it should have been created
  MB_CHK_SET_ERR(rval, "can't get correlation tag");

  // get all polygons out of out_set; then see where are they coming from
  Range polys;
  rval = mb->get_entities_by_dimension(out_set, 2, polys);MB_CHK_SET_ERR(rval, "can't get polygons out");

  // rs2 is the red range, arrival; rs1 is blue, departure;
  // there is a connection between rs1 and rs2, through the corrTag
  // corrTag is __correlation
  // basically, mb->tag_get_data(corrTag, &(redPoly), 1, &bluePoly);
  // also,  mb->tag_get_data(corrTag, &(bluePoly), 1, &redPoly);
  // we start from rs2 existing, then we have to update something

  // tagElem will have multiple tracers
  int numTracers = 0;
  rval = mb->tag_get_length(tagElem, numTracers);MB_CHK_SET_ERR(rval, "can't get number of tracers in simulation");
  if (numTracers < 1)
    MB_CHK_SET_ERR(MB_FAILURE, "no tracers data");

  std::vector<double>  currentVals(rs2.size()*numTracers);
  rval = mb->tag_get_data(tagElem, rs2, &currentVals[0]);MB_CHK_SET_ERR(rval, "can't get existing tracers values");

  // create new tuple list for tracers to other processors, from remote_cells
#ifdef MOAB_HAVE_MPI
  if (remote_cells)
  {
    int n = remote_cells->get_n();
    if (n>0) {
      remote_cells_with_tracers = new TupleList();
      remote_cells_with_tracers->initialize(2, 0, 1, numTracers, n); // tracers are in these tuples
      remote_cells_with_tracers->enableWriteAccess();
      for (int i=0; i<n; i++)
      {
        remote_cells_with_tracers->vi_wr[2*i]=remote_cells->vi_wr[2*i];
        remote_cells_with_tracers->vi_wr[2*i+1]=remote_cells->vi_wr[2*i+1];
        //    remote_cells->vr_wr[i] = 0.; will have a different tuple for communication
        remote_cells_with_tracers->vul_wr[i]=   remote_cells->vul_wr[i];// this is the corresponding red cell (arrival)
        for (int k=0; k<numTracers; k++)
          remote_cells_with_tracers->vr_wr[numTracers*i+k] = 0; // initialize tracers to be transported
        remote_cells_with_tracers->inc_n();
      }
    }
    delete remote_cells;
    remote_cells = NULL;
  }
#endif
  // for each polygon, we have 2 indices: red and blue parents
  // we need index blue to update index red?
  std::vector<double> newValues(rs2.size()*numTracers, 0.);// initialize with 0 all of them
  // area of the polygon * conc on red (old) current quantity
  // finally, divide by the area of the red
  double check_intx_area=0.;
  for (Range::iterator it= polys.begin(); it!=polys.end(); ++it)
  {
    EntityHandle poly=*it;
    int blueIndex, redIndex;
    rval =  mb->tag_get_data(blueParentTag, &poly, 1, &blueIndex);MB_CHK_SET_ERR(rval, "can't get blue tag");
    
    EntityHandle blue = rs1[blueIndex-1]; // big assumption, it should work for meshes where global id is the same
    // as element handle (ordered from 1 to number of elements); should be OK for Homme meshes
    rval =  mb->tag_get_data(redParentTag, &poly, 1, &redIndex);MB_CHK_SET_ERR(rval, "can't get red tag");
    //EntityHandle red = rs2[redIndex];
    // big assumption here, red and blue are "parallel" ;we should have an index from
    // blue to red (so a deformed blue corresponds to an arrival red)
    /// TODO: VSM: Its unclear whether we need the source or destination radius here.
    double radius = Rsrc;
    double areap = area_spherical_element(mb, poly, radius);
    check_intx_area+=areap;
    // so the departure cell at time t (blueIndex) covers a portion of a redCell
    // that quantity will be transported to the redCell at time t+dt
    // the blue corresponds to a red arrival
    EntityHandle redArr;
    rval = mb->tag_get_data(corrTag, &blue, 1, &redArr);
    if (0==redArr || MB_TAG_NOT_FOUND==rval)
    {
#ifdef MOAB_HAVE_MPI
      if (!remote_cells_with_tracers)
        MB_CHK_SET_ERR( MB_FAILURE, "no remote cells, failure\n");
      // maybe the element is remote, from another processor
      int global_id_blue;
      rval = mb->tag_get_data(gid, &blue, 1, &global_id_blue);
      MB_CHK_SET_ERR(rval, "can't get arrival red for corresponding blue gid");
      // find the
      int index_in_remote = remote_cells_with_tracers->find(1, global_id_blue);
      if (index_in_remote==-1)
        MB_CHK_SET_ERR( MB_FAILURE, "can't find the global id element in remote cells\n");
      for (int k=0; k<numTracers; k++)
        remote_cells_with_tracers->vr_wr[index_in_remote*numTracers+k] +=
            currentVals[numTracers*(redIndex-1)+k]*areap;
#endif
    }
    else if (MB_SUCCESS==rval)
    {
      int arrRedIndex = rs2.index(redArr);
      if (-1 == arrRedIndex)
        MB_CHK_SET_ERR(MB_FAILURE, "can't find the red arrival index");
      for (int k=0; k<numTracers; k++)
        newValues[numTracers*arrRedIndex+k] += currentVals[(redIndex-1)*numTracers+k]*areap;
    }

    else
      MB_CHK_SET_ERR(rval, "can't get arrival red for corresponding ");
  }
  // now, send back the remote_cells_with_tracers to the processors they came from, with the updated values for
  // the tracer mass in a cell
#ifdef MOAB_HAVE_MPI
  if (remote_cells_with_tracers)
  {
    // so this means that some cells will be sent back with tracer info to the procs they were sent from
    (parcomm->proc_config().crystal_router())->gs_transfer(1, *remote_cells_with_tracers, 0);
    // now, look at the global id, find the proper "red" cell with that index and update its mass
    //remote_cells->print("remote cells after routing");
    int n = remote_cells_with_tracers->get_n();
    for (int j=0; j<n; j++)
    {
      EntityHandle redCell = remote_cells_with_tracers->vul_rd[j];// entity handle sent back
      int arrRedIndex = rs2.index(redCell);
      if (-1 == arrRedIndex)
        MB_CHK_SET_ERR(MB_FAILURE, "can't find the red arrival index");
      for (int k=0; k<numTracers; k++)
        newValues[arrRedIndex*numTracers+k] += remote_cells_with_tracers->vr_rd[j*numTracers+k];
    }
  }
#endif /* MOAB_HAVE_MPI */
  // now divide by red area (current)
  int j=0;
  Range::iterator iter = rs2.begin();
  void * data=NULL; //used for stored area
  int count =0;
  std::vector<double> total_mass_local(numTracers, 0.);
  while (iter != rs2.end())
  {
    rval = mb->tag_iterate(tagArea, iter, rs2.end(), count, data);MB_CHK_SET_ERR(rval, "can't tag iterate");
    double * ptrArea=(double*)data;
    for (int i=0; i<count; i++, ++iter, j++, ptrArea++)
    {
      for (int k=0; k<numTracers; k++)
      {
        total_mass_local[k]+=newValues[j*numTracers+k];
        newValues[j*numTracers+k]/= (*ptrArea);
      }
    }
  }
  rval = mb->tag_set_data(tagElem, rs2, &newValues[0]);MB_CHK_SET_ERR(rval, "can't set new values tag");

#ifdef MOAB_HAVE_MPI
  std::vector<double> total_mass(numTracers,0.);
  double total_intx_area =0;
  int mpi_err = MPI_Reduce(&total_mass_local[0], &total_mass[0], numTracers, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  if (MPI_SUCCESS != mpi_err) return MB_FAILURE;
  // now reduce total area
  mpi_err = MPI_Reduce(&check_intx_area, &total_intx_area, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  if (MPI_SUCCESS != mpi_err) return MB_FAILURE;
  if (my_rank==0)
  {
    for (int k=0; k<numTracers; k++)
      std::cout <<"total mass now tracer k=" << k+1<<" "  << total_mass[k] << "\n";
    std::cout <<"check: total intersection area: (4 * M_PI * R^2): " << 4 * M_PI * Rsrc*Rsrc << " " << total_intx_area << "\n";
  }

  if (remote_cells_with_tracers)
  {
    delete remote_cells_with_tracers;
    remote_cells_with_tracers=NULL;
  }
#else
  for (int k=0; k<numTracers; k++)
        std::cout <<"total mass now tracer k=" << k+1<<" "  << total_mass_local[k] << "\n";
  std::cout <<"check: total intersection area: (4 * M_PI * R^2): " << 4 * M_PI * Rsrc*Rsrc << " " << check_intx_area << "\n";
#endif
  return MB_SUCCESS;
}

#ifdef MOAB_HAVE_MPI
ErrorCode Intx2MeshOnSphere::build_processor_euler_boxes(EntityHandle euler_set, Range & local_verts)
{
  localEnts.clear();
  ErrorCode rval = mb->get_entities_by_dimension(euler_set, 2, localEnts); MB_CHK_SET_ERR(rval, "can't get local ents");

  rval = mb->get_connectivity(localEnts, local_verts); MB_CHK_SET_ERR(rval, "can't get connectivity");
  int num_local_verts = (int) local_verts.size();

  assert(parcomm != NULL);

  // will use 6 gnomonic planes to decide boxes for each gnomonic plane
  // each gnomonic box will be 2d, min, max
  double gnom_box[24];
  for (int i=0; i<6; i++)
  {
    gnom_box[4*i  ] = gnom_box[4*i+1] =  DBL_MAX;
    gnom_box[4*i+2] = gnom_box[4*i+3] = -DBL_MAX;
  }

  // there are 6 gnomonic planes; some elements could be on the corners, and affect multiple planes
  // decide what gnomonic planes will be affected by each cell
  // some elements could appear in multiple gnomonic planes !
  std::vector<double> coords(3*num_local_verts);
  rval = mb->get_coords(local_verts, &coords[0]); MB_CHK_SET_ERR(rval, "can't get vertex coords");
  ERRORR(rval, "can't get coords of vertices ");
  // decide each local vertex to what gnomonic plane it belongs

  std::vector<int> gnplane;
  gnplane.resize(num_local_verts);
  for (int i=0; i<num_local_verts; i++)
  {
    CartVect pos(&coords[3*i]);
    int pl;
    decide_gnomonic_plane(pos, pl);
    gnplane[i] = pl;
  }

  for (Range::iterator it=localEnts.begin(); it!= localEnts.end(); it++ )
  {
    EntityHandle cell = *it;
    // get coordinates, and decide gnomonic planes for it
    int nnodes;
    const EntityHandle * conn=NULL;
    rval = mb->get_connectivity(cell, conn, nnodes); MB_CHK_SET_ERR(rval, "can't get connectivity");
    // get coordinates of vertices involved with this
    std::vector<double> elco(3*nnodes);
    std::set<int> planes;
    for (int i=0; i<nnodes; i++)
    {
      int ix = local_verts.index(conn[i]);
      planes.insert(gnplane[ix]);
      for (int j=0; j<3; j++)
      {
        elco[3*i+j] = coords[3*ix+j];
      }
    }
    // now, augment the boxes for all planes involved
    for (std::set<int>::iterator st=planes.begin(); st!=planes.end(); st++)
    {
      int pl = *st;
      for (int i=0; i<nnodes; i++)
      {
        CartVect pos(&elco[3*i]);
        double c2[2];
        gnomonic_projection(pos, Rdest, pl, c2[0], c2[1]);  // 2 coordinates
        //
        for (int k=0; k<2; k++)
        {
          double val=c2[k];
          if (val < gnom_box[4*(pl-1)+k])
            gnom_box[4*(pl-1)+k] = val; // min in k direction
          if (val > gnom_box[4*(pl-1)+ 2 + k])
            gnom_box[4*(pl-1)+ 2 + k] = val; // max in k direction
        }
      }
    }
  }

  int numprocs=parcomm->proc_config().proc_size();
  allBoxes.resize(24*numprocs); // 6 gnomonic planes , 4 doubles for each for 2d box

  my_rank = parcomm->proc_config().proc_rank() ;
  for (int k=0; k<24; k++)
    allBoxes[24*my_rank+k]=gnom_box[k];


   // now communicate to get all boxes
  int mpi_err;
#if (MPI_VERSION >= 2)
    // use "in place" option
  mpi_err = MPI_Allgather(MPI_IN_PLACE, 0, MPI_DATATYPE_NULL,
                          &allBoxes[0], 24, MPI_DOUBLE,
                          parcomm->proc_config().proc_comm());
#else
  {
    std::vector<double> allBoxes_tmp(24*parcomm->proc_config().proc_size());
    mpi_err = MPI_Allgather( &allBoxes[24*my_rank], 6, MPI_DOUBLE,
                             &allBoxes_tmp[0], 24, MPI_DOUBLE,
                             parcomm->proc_config().proc_comm());
    allBoxes = allBoxes_tmp;
  }
#endif
  if (MPI_SUCCESS != mpi_err) return MB_FAILURE;

#ifdef VERBOSE
  if (my_rank==0)
  {
    std::cout << " maximum number of vertices per cell are " << max_edges_1 << " on first mesh and "
       << max_edges_2 << " on second mesh \n";
    for (int i=0; i<numprocs; i++)
    {
      std::cout<<"task: " << i << " \n";
      for (int pl=1; pl <=6; pl++)
      {
        std::cout << "  plane " << pl<<" min: \t" << allBoxes[24*i +4*(pl-1)] << " \t" << allBoxes[24*i +4*(pl-1)+1] << "\n";
        std::cout << " \t  max: \t" << allBoxes[24*i +4*(pl-1)+2] << " \t" << allBoxes[24*i +4*(pl-1)+3] << "\n";
      }
    }
  }
#endif

  return MB_SUCCESS;
}

// this will use the bounding boxes for the (euler)/ fix  mesh that are already established
// will distribute the mesh to other procs, so that on each task, the covering set covers the local bounding box
// this means it will cover the second (local) mesh set;
// So the covering set will cover completely the second local mesh set (in intersection)
ErrorCode Intx2MeshOnSphere::construct_covering_set(EntityHandle & initial_distributed_set, EntityHandle & covering_set)
{
  assert(parcomm != NULL);
  if ( 1==parcomm->proc_config().proc_size())
  {
    covering_set = initial_distributed_set; // nothing to move around, it must be serial
    return MB_SUCCESS;
  }

  // primary element came from, in the joint communicator ; this will be forwarded by coverage mesh
  // needed for tag migrate later on
  int defaultInt=-1; // no processor, so it was not migrated from somewhere else
  ErrorCode rval = mb->tag_get_handle("orig_sending_processor", 1, MB_TYPE_INTEGER, orgSendProcTag,
      MB_TAG_DENSE | MB_TAG_CREAT, &defaultInt);MB_CHK_SET_ERR(rval, "can't create original sending processor tag");

  // mark on the coverage mesh where this element came from
  Tag sendProcTag; /// for coverage mesh, will store the sender
  rval = mb->tag_get_handle("sending_processor", 1, MB_TYPE_INTEGER, sendProcTag,
        MB_TAG_DENSE | MB_TAG_CREAT, &defaultInt);MB_CHK_SET_ERR(rval, "can't create sending processor tag");

  // this information needs to be forwarded to coverage mesh, if this mesh was already migrated from somewhere else
  Range meshCells;
  rval = mb->get_entities_by_dimension(initial_distributed_set, 2, meshCells);MB_CHK_SET_ERR(rval, "can't get cells by dimension from mesh set");

  // look at the value of orgSendProcTag for one mesh cell; if -1, no need to forward that; if !=-1,
  // we know that this mesh was migrated, we need to find out more about origin of cell
  int orig_sender =-1;
  EntityHandle oneCell= meshCells[0];
  rval = mb->tag_get_data(orgSendProcTag, &oneCell, 1, &orig_sender); MB_CHK_SET_ERR(rval, "can't get original sending processor value");

  int migrated_mesh = 0;
  if (orig_sender != -1) migrated_mesh = 1; //

  // decide if we need to transfer global DOFs info attached to each HOMME coarse cell; first we need to decide if the mesh
  // has that tag; will affect the size of the tuple list involved in the crystal routing
  int size_gdofs_tag=0;
  std::vector<int> valsDOFs;
  Tag gdsTag;
  rval = mb->tag_get_handle("GLOBAL_DOFS", gdsTag);
  if (MB_SUCCESS == rval && gdsTag)
  {
    DataType dtype;
    rval = mb->tag_get_data_type(gdsTag, dtype);
    if (MB_SUCCESS == rval && MB_TYPE_INTEGER == dtype)
    {
      // find the values on first cell
      int lenTag = 0;
      rval = mb->tag_get_length(gdsTag, lenTag);
      if (MB_SUCCESS == rval && lenTag > 0)
      {
        valsDOFs.resize(lenTag);
        rval = mb->tag_get_data(gdsTag, &oneCell, 1, &valsDOFs[0]);
        if (MB_SUCCESS == rval && valsDOFs[0]>0 )
        {
          // first value positive means we really need to transport this data during coverage
          size_gdofs_tag = lenTag;
        }
      }
    }
  }
  // if size_gdofs_tag>0, we are sure valsDOFs got resized to what we need

  // get all mesh verts1
  Range mesh_verts;
  rval = mb->get_connectivity(meshCells, mesh_verts);MB_CHK_SET_ERR(rval, "can't get  mesh vertices");
  int num_mesh_verts = (int) mesh_verts.size();

  // now see the mesh points positions; to what boxes should we send them?
  std::vector<double> coords_mesh(3 * num_mesh_verts);
  rval = mb->get_coords(mesh_verts, &coords_mesh[0]);MB_CHK_SET_ERR(rval, "can't get mesh points position");

  // decide gnomonic plane for each vertex, as in the compute boxes
  std::vector<int> gnplane;
  gnplane.resize(num_mesh_verts);
  for (int i=0; i<num_mesh_verts; i++)
  {
    CartVect pos(&coords_mesh[3*i]);
    int pl;
    decide_gnomonic_plane(pos, pl);
    gnplane[i] = pl;
  }

  std::vector<int> gids(num_mesh_verts);
  rval = mb->tag_get_data(gid, mesh_verts, &gids[0]);MB_CHK_SET_ERR(rval, "can't get vertices gids");

  // ranges to send to each processor; will hold vertices and elements (quads/ polygons)
  // will look if the box of the mesh cell covers bounding box(es) (within tolerances)
  std::map<int, Range> Rto;
  int numprocs = parcomm->proc_config().proc_size();

  for (Range::iterator eit = meshCells.begin(); eit != meshCells.end(); ++eit)
  {
    EntityHandle q = *eit;
    const EntityHandle * conn;
    int num_nodes;
    rval = mb->get_connectivity(q, conn, num_nodes);MB_CHK_SET_ERR(rval, "can't get connectivity on cell");

    // first decide what planes need to consider
    std::set<int> planes; // if this list contains more than 3 planes, we have a very bad mesh!!!
    std::vector<double> elco(3*num_nodes);
    for (int i = 0; i < num_nodes; i++)
    {
      EntityHandle v = conn[i];
      int index = mesh_verts.index(v);
      planes.insert(gnplane[index]);
      for (int j=0; j<3; j++)
      {
        elco[3*i+j] = coords_mesh[3*index+j]; // extract from coords
      }
    }
    // now loop over all planes that need to be considered for this element
    for (std::set<int>::iterator st=planes.begin(); st!=planes.end(); st++)
    {
      int pl = *st; // gnomonic plane considered
      double qmin[2] = { DBL_MAX,  DBL_MAX};
      double qmax[2] = {-DBL_MAX, -DBL_MAX};
      for (int i = 0; i < num_nodes; i++)
      {
        CartVect dp(&elco[3 * i]); // uses constructor for CartVect that takes a pointer to double
        // gnomonic projection
        double c2[2];
        gnomonic_projection(dp, Rsrc, pl, c2[0], c2[1]);  // 2 coordinates
        for (int j = 0; j < 2; j++)
        {
          if (qmin[j] > c2[j])
            qmin[j] = c2[j];
          if (qmax[j] < c2[j])
            qmax[j] = c2[j];
        }
      }
      // now decide if processor p should be interested in this cell, by looking at plane pl 2d box
      // this is one of the few size n loops;
      for (int p = 0; p < numprocs; p++) // each cell q can be sent to more than one processor
      {
        double procMin1 = allBoxes[24 * p + 4*(pl-1)  ];  // these were determined before
        //
        if (procMin1>=DBL_MAX) // the processor has no targets on this plane
          continue;
        double procMin2 = allBoxes[24 * p + 4*(pl-1)+1];
        double procMax1 = allBoxes[24 * p + 4*(pl-1)+2];
        double procMax2 = allBoxes[24 * p + 4*(pl-1)+3];
        // test overlap of 2d boxes
        if (procMin1 > qmax[0] + box_error || procMin2 > qmax[1] + box_error )
          continue; //
        if (qmin[0] > procMax1 + box_error || qmin[1] > procMax2 + box_error )
          continue;
        // good to be inserted
        Rto[p].insert(q);

      }
    }

  }

  // here, we will use crystal router to send each cell to designated tasks (mesh migration)

  // a better implementation would be to use pcomm send / recv entities; a good test case
  // pcomm send / receives uses point to point communication, not global gather / scatter

  // now, build TLv and TLq  (tuple list for vertices and cells, separately sent)
  size_t numq = 0;
  size_t numv = 0;

  // merge the list of vertices to be sent
  for (int p = 0; p < numprocs; p++)
  {
    if (p == (int) my_rank)
      continue; // do not "send" it to current task, because it is already here
    Range & range_to_P = Rto[p];
    // add the vertices to it
    if (range_to_P.empty())
      continue; // nothing to send to proc p
    Range vertsToP;
    rval = mb->get_connectivity(range_to_P, vertsToP);MB_CHK_SET_ERR(rval, "can't get connectivity");
    numq = numq + range_to_P.size();
    numv = numv + vertsToP.size();
    range_to_P.merge(vertsToP);
  }

  TupleList TLv; // send vertices with a different tuple list
  TupleList TLq;
  TLv.initialize(2, 0, 0, 3, numv); // to proc, GLOBAL ID, 3 real coordinates
  TLv.enableWriteAccess();

  // add also GLOBAL_DOFS info, if found on the mesh cell; it should be found only on HOMME cells!
  int sizeTuple = 2 + max_edges_1 + migrated_mesh + size_gdofs_tag; // max edges could be up to MAXEDGES :) for polygons
  TLq.initialize(sizeTuple, 0, 0, 0, numq); // to proc, elem GLOBAL ID, connectivity[max_edges] (global ID v), plus original sender if set (migrated mesh case)
  // we will not send the entity handle, global ID should be more than enough
  // we will not need more than 2B vertices
  // if we need more than 2B, we will need to use a different marker anyway (GLOBAL ID is not enough then)

  TLq.enableWriteAccess();
#ifdef VERBOSE
  std::cout << "from proc " << my_rank << " send " << numv << " vertices and "
      << numq << " elements\n";
#endif

  for (int to_proc = 0; to_proc < numprocs; to_proc++)
  {
    if (to_proc == (int) my_rank)
      continue;
    Range & range_to_P = Rto[to_proc];
    Range V = range_to_P.subset_by_type(MBVERTEX);

    for (Range::iterator it = V.begin(); it != V.end(); ++it)
    {
      EntityHandle v = *it;
      int index = mesh_verts.index(v);//
      assert(-1!=index);
      int n = TLv.get_n(); // current size of tuple list
      TLv.vi_wr[2 * n] = to_proc; // send to processor
      TLv.vi_wr[2 * n + 1] = gids[index]; // global id needs index in the second_mesh_verts range
      TLv.vr_wr[3 * n] = coords_mesh[3 * index]; // departure position, of the node local_verts[i]
      TLv.vr_wr[3 * n + 1] = coords_mesh[3 * index + 1];
      TLv.vr_wr[3 * n + 2] = coords_mesh[3 * index + 2];
      TLv.inc_n(); // increment tuple list size
    }
    // also, prep the 2d cells for sending ...
    Range Q = range_to_P.subset_by_dimension(2);
    for (Range::iterator it = Q.begin(); it != Q.end(); ++it)
    {
      EntityHandle q = *it; // this is a second mesh cell (or blue, lagrange set)
      int global_id;
      rval = mb->tag_get_data(gid, &q, 1, &global_id);MB_CHK_SET_ERR(rval, "can't get gid for polygon");
      int n = TLq.get_n(); // current size
      TLq.vi_wr[sizeTuple * n] = to_proc; //
      TLq.vi_wr[sizeTuple * n + 1] = global_id; // global id of element, used to identify it for debug purposes only
      const EntityHandle * conn4;
      int num_nodes; // could be up to MAXEDGES; max_edges?;
      rval = mb->get_connectivity(q, conn4, num_nodes);MB_CHK_SET_ERR(rval, "can't get connectivity for cell");
      if (num_nodes > max_edges_1) {
        mb->list_entities(&q,1);
        MB_CHK_SET_ERR(MB_FAILURE, "too many nodes in a cell (" << num_nodes << "," << max_edges_1 << ")");
      }
      for (int i = 0; i < num_nodes; i++)
      {
        EntityHandle v = conn4[i];
        int index = mesh_verts.index(v);
        assert(-1!=index);
        TLq.vi_wr[sizeTuple * n + 2 + i] = gids[index];
      }
      for (int k = num_nodes; k < max_edges_1; k++)
      {
        TLq.vi_wr[sizeTuple * n + 2 + k] = 0; // fill the rest of node ids with 0; we know that the node ids start from 1!
      }
      int currentIndexIntTuple = 2+max_edges_1;
      // is the mesh migrated before or not?
      if (migrated_mesh)
      {
        rval = mb->tag_get_data(orgSendProcTag, &q, 1, &orig_sender);MB_CHK_SET_ERR(rval, "can't get original sender for polygon, in migrate scenario");
        TLq.vi_wr[sizeTuple * n + currentIndexIntTuple] = orig_sender; // should be different than -1
        currentIndexIntTuple ++;
      }
      // GLOBAL_DOFS info, if available
      if (size_gdofs_tag)
      {
        rval = mb->tag_get_data(gdsTag, &q, 1, &valsDOFs[0]);MB_CHK_SET_ERR(rval, "can't get gdofs data in HOMME");
        for (int i=0; i<size_gdofs_tag; i++)
        {
          TLq.vi_wr[sizeTuple * n + currentIndexIntTuple + i] = valsDOFs[i]; // should be different than 0 or -1
        }
      }

      TLq.inc_n(); // increment tuple list size

    }
  } // end for loop over total number of processors

  // now we are done populating the tuples; route them to the appropriate processors
  // this does the communication magic
  (parcomm->proc_config().crystal_router())->gs_transfer(1, TLv, 0);
  (parcomm->proc_config().crystal_router())->gs_transfer(1, TLq, 0);

  // the first mesh elements are in localEnts; we do not need them at all

  // maps from global ids to new vertex and cell handles, that are added

  std::map<int, EntityHandle> globalID_to_vertex_handle;
  // we already have some vertices from second mesh set; they are already in the processor, even before receiving other
  // verts from neighbors
  // this is an inverse map from gid to vertex handle, which is local here, we do not want to duplicate vertices
  // their identifier is the global ID!! it must be unique per mesh ! (I mean, second mesh); gid gor first mesh is not needed here
  int k=0;
  for (Range::iterator vit=mesh_verts.begin(); vit!=mesh_verts.end(); ++vit, k++)
  {
    globalID_to_vertex_handle[gids[k]] = *vit;
  }
  /*std::map<int, EntityHandle> globalID_to_eh;*/ // do we need this one?
  globalID_to_eh.clear();  // we do not really need it, but we keep it for debugging mostly

  // now, look at every TLv, and see if we have to create a vertex there or not
  int n = TLv.get_n(); // the size of the points received
  for (int i = 0; i < n; i++)
  {
    int globalId = TLv.vi_rd[2 * i + 1];
    if (globalID_to_vertex_handle.find(globalId) == globalID_to_vertex_handle.end()) // we do not have locally this vertex (yet)
      // so we have to create it, and add to the inverse map
    {
      EntityHandle new_vert;
      double dp_pos[3] = { TLv.vr_wr[3 * i], TLv.vr_wr[3 * i + 1], TLv.vr_wr[3 * i + 2] };
      rval = mb->create_vertex(dp_pos, new_vert);MB_CHK_SET_ERR(rval, "can't create new vertex ");
      globalID_to_vertex_handle[globalId] = new_vert; // now add it to the map
      // set the GLOBAL ID tag on the new vertex
      rval = mb->tag_set_data(gid, &new_vert, 1, &globalId); MB_CHK_SET_ERR(rval, "can't set global ID tag on new vertex ");
    }
  }

  // now, all necessary vertices should be created
  // look in the local list of 2d cells for this proc, and add all those cells to covering set also

  Range & local = Rto[my_rank];
  Range local_q = local.subset_by_dimension(2);

  for (Range::iterator it = local_q.begin(); it != local_q.end(); ++it)
  {
    EntityHandle q = *it;// these are from lagr cells, local
    int gid_el;
    rval = mb->tag_get_data(gid, &q, 1, &gid_el);MB_CHK_SET_ERR(rval, "can't get global id of cell ");
    assert(gid_el >= 0);
    globalID_to_eh[gid_el] = q; // do we need this? yes, now we do; parent tags are now using it heavily
    rval = mb->tag_set_data(sendProcTag, &q, 1, &my_rank);MB_CHK_SET_ERR(rval, "can't set sender for cell");
  }

  // now look at all elements received through; we do not want to duplicate them
  n = TLq.get_n(); // number of elements received by this processor
  // a cell should be received from one proc only; so why are we so worried about duplicated elements?
  // a vertex can be received from multiple sources, that is fine

  for (int i = 0; i < n; i++)
  {
    int globalIdEl = TLq.vi_rd[sizeTuple * i + 1];
    // int from_proc=TLq.vi_rd[sizeTuple * i ]; // we do not need from_proc anymore

    // do we already have a quad with this global ID, represented? no way !
    //if (globalID_to_eh.find(globalIdEl) == globalID_to_eh.end())
    //{
    // construct the conn triangle , quad or polygon
    EntityHandle new_conn[MAXEDGES]; // we should use std::vector with max_edges_1
    int nnodes = -1;
    for (int j = 0; j < max_edges_1; j++)
    {
      int vgid = TLq.vi_rd[sizeTuple * i + 2 + j]; // vertex global ID
      if (vgid == 0)
        new_conn[j] = 0; // this can actually happen for polygon mesh (when we have less number of vertices than max_edges)
      else
      {
        assert(globalID_to_vertex_handle.find(vgid)!=globalID_to_vertex_handle.end());
        new_conn[j] = globalID_to_vertex_handle[vgid];
        nnodes = j + 1; // nodes are at the beginning, and are variable number
      }
    }
    EntityHandle new_element;
    //
    EntityType entType = MBQUAD;
    if (nnodes > 4)
      entType = MBPOLYGON;
    if (nnodes < 4)
      entType = MBTRI;
    rval = mb->create_element(entType, new_conn, nnodes, new_element);MB_CHK_SET_ERR(rval, "can't create new element for second mesh ");

    globalID_to_eh[globalIdEl] = new_element;
    local_q.insert(new_element);
    rval = mb->tag_set_data(gid, &new_element, 1, &globalIdEl);MB_CHK_SET_ERR(rval, "can't set gid for cell ");
    int currentIndexIntTuple = 2 + max_edges_1;
    if (migrated_mesh)
    {
      orig_sender = TLq.vi_wr[sizeTuple * i + currentIndexIntTuple];
      rval = mb->tag_set_data(orgSendProcTag, &new_element, 1, &orig_sender);MB_CHK_SET_ERR(rval, "can't set original sender for cell, in migrate scenario");
      currentIndexIntTuple ++;// add one more
    }
    // check if we need to retrieve and set GLOBAL_DOFS data
    if (size_gdofs_tag)
    {
      for (int j=0; j<size_gdofs_tag; j++)
      {
        valsDOFs[j] = TLq.vi_wr[sizeTuple * i + currentIndexIntTuple + j];
      }
      rval = mb->tag_set_data(gdsTag, &new_element, 1, &valsDOFs[0]);MB_CHK_SET_ERR(rval, "can't set GLOBAL_DOFS data on coverage mesh");
    }
    // store also the processor this coverage element came from
    int from_proc = TLq.vi_rd[sizeTuple * i];
    rval = mb->tag_set_data(sendProcTag, &new_element, 1, &from_proc);MB_CHK_SET_ERR(rval, "can't set sender for cell");
  }

  // now, create a new set, covering_set
  rval = mb->add_entities(covering_set, local_q);MB_CHK_SET_ERR(rval,  "can't add entities to new mesh set ");
  return MB_SUCCESS;
}

#endif // MOAB_HAVE_MPI
} /* namespace moab */
