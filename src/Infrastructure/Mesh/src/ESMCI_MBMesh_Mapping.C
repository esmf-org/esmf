//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MBMesh_Mapping.h>
#include <Mesh/include/ESMCI_MathUtil.h>

using namespace ESMCI;

//TODO: make tolerance parameters global

bool MBElemMap::tri_is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;
  bool in=true;

  if ((pcoord[0] <-in_tol) || (pcoord[1] <-in_tol)) {
    double out_dist[2]={0.0,0.0};

     if (pcoord[0] < -in_tol) {
        out_dist[0] = -pcoord[0];
        in=false;
     } else if (pcoord[0] > 1.0+in_tol) {
        out_dist[0] = pcoord[0]-1.0;
        // no setting of in, this is just to calculate dist
     }

     if (pcoord[1] < -in_tol) {
        out_dist[1] = -pcoord[1];
        in=false;
     } else if (pcoord[1] > 1.0+in_tol) {
        out_dist[1] = pcoord[1]-1.0;
        // no setting of in, this is just to calculate dist
     }

     if (dist) *dist=std::sqrt(out_dist[0]*out_dist[0]+out_dist[1]*out_dist[1]);

  } else if ((pcoord[0] + pcoord[1]) > 1.0+in_tol) {
    if (dist) *dist = std::abs((pcoord[0] + pcoord[1]) - 1.0);
    in=false;
  }

  return in;
}

bool MBElemMap::quad_is_in(const double pcoord[], double *dist) {
  const double in_tol = 1e-10;
  bool in=true;
  double max_out[2]={0.0,0.0};

  if (pcoord[0] < -1.0*in_tol) {
    max_out[0]=-1.0*pcoord[0];
    in= false;
  } else if (pcoord[0] > 1.0+in_tol) {
    max_out[0]=pcoord[0] - 1.0;
    in= false;
  }

  if (pcoord[1] < -1.0*in_tol) {
    max_out[1]=-1.0*pcoord[1];
    in= false;
  } else if (pcoord[1] > 1.0+in_tol) {
    max_out[1]=pcoord[1] - 1.0;
    in= false;
  }

  if (dist) *dist=std::sqrt(max_out[0]*max_out[0]+max_out[1]*max_out[1]);

  return in;
}


bool MBElemMap::spherical_eval(const double *mdata,
                    const double *point,
                    int num_pnts,
                    double *pcoord,
                    double *dist)
{
  // return is_inside
  //
  // in:
  //    mdata    - coordinate values? (3d cartesian)
  //    point    - coordinates of point of interest (3d cartesian)
  //    num_pnts - number of corners
  //    pcoord   - parametric coordinates of point
  //    dist     - distance outside the cell if point is not inside

  // The maximum number of points we expect to see in a polygon in here, plus a bit extra
  int PM_MAX_PNTS_IN_POLY = 4;

  // If point actually lands on a another point then the match should be exact
  // TODO: Do this for other types of mapping also (e.g. 3D)
  if (num_pnts==3) {
    // Corner 0
    if ((mdata[0] == point[0]) && (mdata[1] == point[1]) && (mdata[2] == point[2])) {
      pcoord[0]=0.0; pcoord[1]=0.0;
      if (dist) *dist = 0.0;
      return true;
    }

    // Corner 1
    if ((mdata[3] == point[0]) && (mdata[4] == point[1]) && (mdata[5] == point[2])) {
      pcoord[0]=1.0; pcoord[1]=0.0;
      if (dist) *dist = 0.0;
      return true;
    }

    // Corner 2
    if ((mdata[6] == point[0]) && (mdata[7] == point[1]) && (mdata[8] == point[2])) {
      pcoord[0]=0.0; pcoord[1]=1.0;
      if (dist) *dist = 0.0;
      return true;
    }
  } else if (num_pnts==4) {
    // Corner 0
    if ((mdata[0] == point[0]) && (mdata[1] == point[1]) && (mdata[2] == point[2])) {
      pcoord[0]=0.0; pcoord[1]=0.0;
      if (dist) *dist = 0.0;
      return true;
    }

    // Corner 1
    if ((mdata[3] == point[0]) && (mdata[4] == point[1]) && (mdata[5] == point[2])) {
      pcoord[0]=1.0; pcoord[1]=0.0;
      if (dist) *dist = 0.0;
      return true;
    }

    // Corner 2
    if ((mdata[6] == point[0]) && (mdata[7] == point[1]) && (mdata[8] == point[2])) {
      pcoord[0]=1.0; pcoord[1]=1.0;
      if (dist) *dist = 0.0;
      return true;
    }

    // Corner 3
    if ((mdata[9] == point[0]) && (mdata[10] == point[1]) && (mdata[11] == point[2])) {
      pcoord[0]=0.0; pcoord[1]=1.0;
      if (dist) *dist = 0.0;
      return true;
    }
  } else {
    Throw() << " only polygons with 3 or 4 sides are currently supported with 2 parametric dimensions";
  }

  // translate into polygon
  // Copy into new memory, so it can be changed without altering orig.
  double pnts[3*PM_MAX_PNTS_IN_POLY];
  if (num_pnts==3) {
    num_pnts=3;
    pnts[0]=mdata[0]; pnts[1]=mdata[1]; pnts[2]=mdata[2];
    pnts[3]=mdata[3]; pnts[4]=mdata[4]; pnts[5]=mdata[5];
    pnts[6]=mdata[6]; pnts[7]=mdata[7]; pnts[8]=mdata[8];
  } else if (num_pnts==4) {
    num_pnts=4;
    pnts[0]=mdata[0]; pnts[1]=mdata[1]; pnts[2]=mdata[2];
    pnts[3]=mdata[3]; pnts[4]=mdata[4]; pnts[5]=mdata[5];
    pnts[6]=mdata[6]; pnts[7]=mdata[7]; pnts[8]=mdata[8];
    pnts[9]=mdata[9]; pnts[10]=mdata[10]; pnts[11]=mdata[11];
  } else {
    Throw() << " only polygons with 3 or 4 sides are currently supported with 2 parametric dimensions";
  }

  // Init outputs as if we haven't found anything to make false returns easier below
  if (dist) *dist = std::numeric_limits<double>::max();
  pcoord[0]=0.0; pcoord[1]=0.0;

  double p1,p2;  // Get rid of degenerate edges


  int first_removed_ind=-1;
  remove_0len_edges3D(&num_pnts, pnts, &first_removed_ind);


  // Map triangles
  if (num_pnts==3) {

    if (calc_gc_parameters_tri(point,
                               pnts+6, pnts, pnts+3,
                               &p1, &p2)) {
      return false;
    }

    // Don't need to transform tri parametric coords because tri shape func seems to use [0,1], but
    // put into pcoord
    pcoord[0]=p1;
    pcoord[1]=p2;

    // do is in
    double sdist;
    bool in_tri = tri_is_in(pcoord, &sdist);

    // Distance to tri
    if (dist) *dist=2.0*sdist;

    return in_tri;

  } else if (num_pnts==4) { // Map quads
    double p1,p2;

    // Intersect quad with line from point to center of sphere
    if (calc_gc_parameters_quad(point,
                                pnts+9, pnts, pnts+3, pnts+6,
                                &p1, &p2)) {
      return false;
    }

    pcoord[0] = p1;
    pcoord[1] = p2;

    // do is in
    double sdist;
    bool in_quad = quad_is_in(pcoord, &sdist);

    // Distance to quad
    if (dist) *dist=sdist;
    return in_quad;

  }  else {
    // This is a degenerate cell so we can't map to it.
    // Could throw an error here, but for flexiblity allow
    // these degenerate cells for now, but simply don't map to them.
    return false;
  }

  // Shouldn't be able to get here, but just in case...
  return false;
}

/*
bool MBElemMap::cartesian_eval(const double *mdata, const double *point, double *pcoord, double *dist) const
{
  // Init output
  if (dist) *dist = std::numeric_limits<double>::max();

  // Newton's method
  const double ctol = 1e-10;
  const int max_iter = 15;

  int pdim=2, sdim=2, ndof=2;

  POLY_Mapping mpstd, jacobian_invert;

  double s[pdim];
  double delta_s[pdim];
  double res[pdim];
  double jac[sdim][sdim];
  double jac_inv[sdim][sdim];
  double sgrads[SFUNC_TYPE::ndofs][sdim];
  double dnorm = 0.0, rnorm = 0.0;
  int niters = 0;

  for (unsigned int i = 0; i < pdim; i++) {
    s[i] = delta_s[i] = res[i] = 0.0;
  }

  POLY_Mapping<SFUNC_TYPE,MPTraits<>,SPATIAL_DIM,PARAMETRIC_DIM> *mpstd =
    trade<MPTraits<> >();
  bool converged = false;
  do {
    SFUNC_TYPE::shape_grads(1, s, &sgrads[0][0]);
    // Calculate residual.  Also use loop to start jacobian
    mpstd->forward(1, mdata, s, res); // F($)
    rnorm = 0.0;
    for (unsigned int i = 0; i < sdim; i++) {
      res[i] = point[i] - res[i];  // x - F(x) = -R(x)
      rnorm += res[i]*res[i];
      // Load forward jacobian
      for (unsigned int j = 0; j < sdim; j++) {
        jac[i][j] = 0.0;
        for (unsigned int k = 0; k < SFUNC_TYPE::ndofs; k++) {
          jac[i][j] += sgrads[k][j]*mdata[k*sdim+i];
        }
      }
    } // for sdim

    //  rnorm is small even when very bad.
//    if (rnorm < ctol) {
//      converged = true;
//      break;
//    }


    // So now res holds -F(x) and jac has jacobian.  We must invert the jacobian
    POLY_Mapping_jacobian_invert<sdim>(&jac[0][0], &jac_inv[0][0]);

    // delta_s = jac_inv*res
    dnorm = 0;
    for (unsigned int i = 0; i < sdim; i++) {
      delta_s[i] = 0.0;
      for (unsigned int j = 0; j < sdim; j++) {
        delta_s[i] += jac_inv[i][j]*res[j];
      }

      // snew = sold+delta
      s[i] = s[i] + delta_s[i];
      dnorm += delta_s[i]*delta_s[i];
    } // i

    if (dnorm <= ctol) converged = true;
    niters++;

    if (niters >= max_iter) break; // stop loop uncoverged.
  } while(!converged);

  for (unsigned int i = 0; i < sdim; i++)
    pcoord[i] = s[i];

  if (dist) *dist = 0.0;

  if (!converged) {
    if (dist) *dist = std::numeric_limits<double>::max();
    return false;
  }

  // check parametric bounds.
  double sdist=0.0;
  bool resu = SFUNC_TYPE::is_in(pcoord, &sdist);
  if(dist) *dist = sdist;
  return resu;
}*/

#endif
