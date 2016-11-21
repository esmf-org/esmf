#include "moab/LloydSmoother.hpp"
#include "moab/Skinner.hpp"
#include "moab/CN.hpp"
#include "moab/CartVect.hpp"
#include "moab/BoundBox.hpp"

#ifdef MOAB_HAVE_MPI
#include "moab/ParallelComm.hpp"
#include "MBParallelConventions.h"
#endif

#include <iostream>

namespace moab {
  
LloydSmoother::LloydSmoother(Interface *impl, ParallelComm *pc, Range &elms, Tag ctag,
                             Tag ftag, double at, double rt) 
        : mbImpl(impl), myPcomm(pc), myElems(elms), coordsTag(ctag), fixedTag(ftag), absTol(at), relTol(rt),
          reportIts(0), numIts(0), iCreatedTag(false)
{
}

LloydSmoother::~LloydSmoother() 
{
  if (iCreatedTag && fixedTag) {
    ErrorCode rval = mbImpl->tag_delete(fixedTag);MB_CHK_SET_ERR_RET(rval, "Failed to delete the fixed tag");
  }
}

ErrorCode LloydSmoother::perform_smooth() 
{
  ErrorCode rval;

  if (myElems.empty()) {
    MB_SET_ERR(MB_FAILURE, "No elements specified to Lloyd smoother");
  }
  else if (mbImpl->dimension_from_handle(*myElems.begin()) != mbImpl->dimension_from_handle(*myElems.rbegin())) {
    MB_SET_ERR(MB_FAILURE, "Elements of unequal dimension specified to Lloyd smoother");
  }    

  int dim = mbImpl->dimension_from_handle(*myElems.begin());
  
    // first figure out tolerance to use
  if (0 > absTol) {
      // no tolerance set - get one relative to bounding box around elements
    BoundBox bb;
    rval = bb.update(*mbImpl, myElems);MB_CHK_SET_ERR(rval, "Failed to compute bounding box around elements");
    absTol = relTol * bb.diagonal_length();
  }

    // initialize if we need to
  rval = initialize();MB_CHK_SET_ERR(rval, "Failed to initialize");

    // get all vertices
  Range verts;
  rval = mbImpl->get_adjacencies(myElems, 0, false, verts, Interface::UNION);MB_CHK_SET_ERR(rval, "Failed to get all vertices");
  
    // perform Lloyd relaxation:
    // 1. setup: set vertex centroids from vertex coords; filter to owned verts; get fixed tags

    // get all verts coords into tag; don't need to worry about filtering out fixed verts, 
    // we'll just be setting to their fixed coords
  std::vector<double> vcentroids(3*verts.size());
  if (!coordsTag) {
    rval = mbImpl->get_coords(verts, &vcentroids[0]);MB_CHK_SET_ERR(rval, "Failed to get vert coords");
  }
  else {
    rval = mbImpl->tag_get_data(coordsTag, verts, &vcentroids[0]);MB_CHK_SET_ERR(rval, "Failed to get vert coords tag values");
  }

  Tag centroid;
  rval = mbImpl->tag_get_handle("", 3, MB_TYPE_DOUBLE, centroid, MB_TAG_CREAT | MB_TAG_DENSE);MB_CHK_SET_ERR(rval, "Couldn't get tag handle");
  rval = mbImpl->tag_set_data(centroid, verts, &vcentroids[0]);MB_CHK_SET_ERR(rval, "Failed setting centroid tag");

  Range owned_verts, shared_owned_verts;
#ifdef MOAB_HAVE_MPI
    // filter verts down to owned ones and get fixed tag for them
  if (myPcomm && myPcomm->size() > 1) {
    rval = myPcomm->filter_pstatus(verts, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &owned_verts);MB_CHK_SET_ERR(rval, "Failed to filter on pstatus");
      // get shared owned verts, for exchanging tags
    rval = myPcomm->filter_pstatus(owned_verts, PSTATUS_SHARED, PSTATUS_AND, -1, &shared_owned_verts);MB_CHK_SET_ERR(rval, "Failed to filter for shared owned");
      // workaround: if no shared owned verts, put a non-shared one in the list, to prevent exchanging tags
      // for all shared entities
    if (shared_owned_verts.empty()) shared_owned_verts.insert(*verts.begin());
  }
  else
    owned_verts = verts;
#else
  owned_verts = verts;
#endif
  
  std::vector<unsigned char> fix_tag(owned_verts.size());
  rval = mbImpl->tag_get_data(fixedTag, owned_verts, &fix_tag[0]);MB_CHK_SET_ERR(rval, "Failed to get fixed tag");

    // now fill vcentroids array with positions of just owned vertices, since those are the ones
    // we're actually computing
  vcentroids.resize(3*owned_verts.size());
  rval = mbImpl->tag_get_data(centroid, owned_verts, &vcentroids[0]);MB_CHK_SET_ERR(rval, "Failed to get centroid tag");

    // some declarations for later iterations
  std::vector<double> fcentroids(3*myElems.size()); // fcentroids for element centroids
  std::vector<double> ctag(3*CN::MAX_NODES_PER_ELEMENT);  // temporary coordinate storage for verts bounding an element
  const EntityHandle *conn;  // const ptr & size to elem connectivity
  int nconn;
  Range::iterator eit, vit;  // for iterating over elems, verts
  int e, v;  // for indexing into centroid vectors
  std::vector<EntityHandle> adj_elems;  // used in vertex iteration
  
    // 2. while !converged
  double resid = DBL_MAX;
  numIts = 0;
  while (resid > absTol) {
    numIts++;
    resid = 0.0;
    
    // 2a. foreach elem: centroid = sum(vertex centroids)/num_verts_in_cell
    for (eit = myElems.begin(), e = 0; eit != myElems.end(); ++eit, e++) {
        // get verts for this elem
      rval = mbImpl->get_connectivity(*eit, conn, nconn);MB_CHK_SET_ERR(rval, "Failed to get connectivity");
        // get centroid tags for those verts
      rval = mbImpl->tag_get_data(centroid, conn, nconn, &ctag[0]);MB_CHK_SET_ERR(rval, "Failed to get centroid");
      fcentroids[3*e+0] = fcentroids[3*e+1] = fcentroids[3*e+2] = 0.0;
      for (v = 0; v < nconn; v++) {
        fcentroids[3*e+0] += ctag[3*v+0];
        fcentroids[3*e+1] += ctag[3*v+1];
        fcentroids[3*e+2] += ctag[3*v+2];
      }
      for (v = 0; v < 3; v++) fcentroids[3*e+v] /= nconn;
    }
    rval = mbImpl->tag_set_data(centroid, myElems, &fcentroids[0]);MB_CHK_SET_ERR(rval, "Failed to set elem centroid");

      // 2b. foreach owned vertex: 
    for (vit = owned_verts.begin(), v = 0; vit != owned_verts.end(); ++vit, v++) {
        // if !fixed
      if (fix_tag[v]) continue;
        // vertex centroid = sum(cell centroids)/ncells
      adj_elems.clear();
      rval = mbImpl->get_adjacencies(&(*vit), 1, dim, false, adj_elems);MB_CHK_SET_ERR(rval, "Failed getting adjs");
      rval = mbImpl->tag_get_data(centroid, &adj_elems[0], adj_elems.size(), &fcentroids[0]);MB_CHK_SET_ERR(rval, "Failed to get elem centroid");
      double vnew[] = {0.0, 0.0, 0.0};
      for (e = 0; e < (int)adj_elems.size(); e++) {
        vnew[0] += fcentroids[3*e+0];
        vnew[1] += fcentroids[3*e+1];
        vnew[2] += fcentroids[3*e+2];
      }
      for (e = 0; e < 3; e++) vnew[e] /= adj_elems.size();
      double delta = (CartVect(vnew)-CartVect(&vcentroids[3*v])).length();
      resid = (v ? std::max(resid, delta) : delta);
      for (e = 0; e < 3; e++) vcentroids[3*v+e] = vnew[e];
    }

      // set the centroid tag; having them only in vcentroids array isn't enough, as vertex centroids are
      // accessed randomly in loop over faces
    rval = mbImpl->tag_set_data(centroid, owned_verts, &vcentroids[0]);MB_CHK_SET_ERR(rval, "Failed to set vertex centroid");

#ifdef MOAB_HAVE_MPI
    // 2c. exchange tags on owned verts
    if (myPcomm && myPcomm->size() > 1) {
      rval = myPcomm->exchange_tags(centroid, shared_owned_verts);MB_CHK_SET_ERR(rval, "Failed to exchange tags");
    }
#endif

    if (reportIts && !(numIts%reportIts)) {
      double global_max = resid;
#ifdef MOAB_HAVE_MPI
        // global reduce for maximum delta, then report it
      if (myPcomm && myPcomm->size() > 1)
        MPI_Reduce(&resid, &global_max, 1, MPI_DOUBLE, MPI_MAX, 0, myPcomm->comm());
      if (!myPcomm || !myPcomm->rank()) 
#endif
        std::cout << "Max residual = " << global_max << std::endl;
    }

  } // end while
  
    // write the tag back onto vertex coordinates
  if (!coordsTag) {
    rval = mbImpl->set_coords(owned_verts, &vcentroids[0]);MB_CHK_SET_ERR(rval, "Failed to set vertex coords");
  }
  else {
    rval = mbImpl->tag_set_data(coordsTag, owned_verts, &vcentroids[0]);MB_CHK_SET_ERR(rval, "Failed to set vert coords tag values");
  }

  return MB_SUCCESS;
}

ErrorCode LloydSmoother::initialize()
{
  ErrorCode rval = MB_SUCCESS;
  
    // first, check for tag; if we don't have it, make one and mark skin as fixed
  if (!fixedTag) {
    unsigned char fixed = 0x0;
    rval = mbImpl->tag_get_handle("", 1, MB_TYPE_OPAQUE, fixedTag, MB_TAG_DENSE | MB_TAG_CREAT, &fixed);MB_CHK_SET_ERR(rval, "Trouble making fixed tag");
    iCreatedTag = true;
  
      // get the skin; get facets, because we might need to filter on shared entities
    Skinner skinner(mbImpl);
    Range skin, skin_verts;
    rval = skinner.find_skin(0, myElems, false, skin);MB_CHK_SET_ERR(rval, "Unable to find skin");

#ifdef MOAB_HAVE_MPI
      // need to do a little extra if we're working in parallel
    if (myPcomm) {
        // filter out ghost and interface facets
      rval = myPcomm->filter_pstatus(skin, PSTATUS_GHOST | PSTATUS_INTERFACE, PSTATUS_NOT);MB_CHK_SET_ERR(rval, "Failed to filter on shared status");
    }
#endif
      // get the vertices from those entities
    rval = mbImpl->get_adjacencies(skin, 0, false, skin_verts, Interface::UNION);MB_CHK_SET_ERR(rval, "Trouble getting vertices");
    
      // mark them fixed
    std::vector<unsigned char> marks(skin_verts.size(), 0x1);
    rval = mbImpl->tag_set_data(fixedTag, skin_verts, &marks[0]);MB_CHK_SET_ERR(rval, "Unable to set tag on skin");
  }
  
  return MB_SUCCESS;
}

}
