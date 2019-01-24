//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//
// The contents of this file were migrated from the legacy mesh file 
// ESMCI_Mapping.C in June of 2018.
//
// Take out if MOAB isn't being used
#if defined ESMF_MOAB

#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MBMesh_Mapping.h>
#include <Mesh/include/ESMCI_MathUtil.h>
#include <Mesh/include/ESMCI_MBMesh_ShapeFunc.h>


namespace ESMCI {

//TODO: make tolerance parameters global

  // Type used to specify method used to do spherical (3,2) mapping
  // eventually could be broadened to specify other types
  MB_MAP_TYPE mb_sph_map_type=MB_MAP_TYPE_CART_APPROX;

template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM> *MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::classInstance = NULL;

template<class SFUNC_TYPE,typename MPTRAITS>
MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2> *MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::classInstance = NULL;

template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM> *MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::instance() {
  if (classInstance == NULL)
    classInstance = new MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>("mapdd" + std::string(SFUNC_TYPE::name));

  return classInstance;
}
template<class SFUNC_TYPE,typename MPTRAITS>
MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2> *MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::instance() {
  if (classInstance == NULL)
    classInstance = new MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>("map32," + std::string(SFUNC_TYPE::name));

  return classInstance;
}

template<class SFUNC_TYPE,typename MPTRAITS>
bool MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::is_in_cell(const double *mdata,
                                                       const double *point,
                                                       double *pcoord,
                                                       double *dist) const
{

  // The maximum number of points we expect to see in a polygon in here, plus a bit extra
#define PM_MAX_PNTS_IN_POLY 6

  // If point actually lands on a another point then the match should be exact
  // TODO: Do this for other types of mapping also (e.g. 3D)
  if (SFUNC_TYPE::ndofs==3) {
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
  } else if (SFUNC_TYPE::ndofs==4) {
    // Corner 0
    if ((mdata[0] == point[0]) && (mdata[1] == point[1]) && (mdata[2] == point[2])) {
      pcoord[0]=-1.0; pcoord[1]=-1.0;
      if (dist) *dist = 0.0;
      return true;
    }

    // Corner 1
    if ((mdata[3] == point[0]) && (mdata[4] == point[1]) && (mdata[5] == point[2])) {
      pcoord[0]=1.0; pcoord[1]=-1.0;
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
      pcoord[0]=-1.0; pcoord[1]=1.0;
      if (dist) *dist = 0.0;
      return true;
    }
  } else {
    Throw() << " only polygons with 3 or 4 sides are currently supported with 2 parametric dimensions";
  }


  // Eventually need to reorganize mapping/shape_func system
  // so that the switch to different mapping types happens
  // automatically (perhaps via a curved set of shapes as suggested
  // by Ryan).

  // translate into polygon
  // Copy into new memory, so it can be changed without altering orig. 
  int num_pnts;
  double pnts[3*PM_MAX_PNTS_IN_POLY];
  if (SFUNC_TYPE::ndofs==3) {
    num_pnts=3;
    pnts[0]=mdata[0]; pnts[1]=mdata[1]; pnts[2]=mdata[2];
    pnts[3]=mdata[3]; pnts[4]=mdata[4]; pnts[5]=mdata[5];
    pnts[6]=mdata[6]; pnts[7]=mdata[7]; pnts[8]=mdata[8];
  } else if (SFUNC_TYPE::ndofs==4) {
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


  // Map point depending on map type and shape
  if (mb_sph_map_type==MB_MAP_TYPE_CART_APPROX) {

    // See if we're degenerate
    int num_0len;
    count_0len_edges3D(num_pnts, pnts, &num_0len);

    // If degenerate than leave
    if ((SFUNC_TYPE::ndofs-num_0len) < 3){ 
      return false;
    }

    // Handle depending on what kind of shape it is
    if (SFUNC_TYPE::ndofs==3){  // Triangle
      double center[3]={0.0,0.0,0.0}; // center of sphere
      double p[2];
      double t;

      // Intersect tri with line from point to center of sphere
      if (!intersect_tri_with_line(pnts, center, point, p, &t)) {
        return false;
      }

      // Mapped to other side of sphere, so count as not found
      if (t <= 0.0) {
        return false;
      }
      
      // do is in
      double sdist;
      bool in_tri = tri_shape_func::is_in(p, &sdist);

      // Don't need to transform tri parametric coords because tri shape func seems to use [0,1], but
      // put into pcoord 
      pcoord[0]=p[0];
      pcoord[1]=p[1];
      
       // Distance to tri
      if (dist) *dist=2.0*sdist;
    
      return in_tri;
    } else if (SFUNC_TYPE::ndofs==4){  // Quad
      double center[3]={0.0,0.0,0.0}; // center of sphere
      double p[2];
      double t;
      
      // Intersect quad with line from point to center of sphere
      if (!intersect_quad_with_line(pnts, center, point, p, &t)) {
        return false;
      }

      // Mapped to other side of sphere, so count as not found
      if (t <= 0.0) {
        return false;
      } 

      // Transform quad parametric coords from [0,1] to [-1,1] for consistancy
      pcoord[0]=2*p[0]-1.0;
      pcoord[1]=2*p[1]-1.0;    

      // do is in
      double sdist;
      bool in_quad = quad_shape_func::is_in(pcoord, &sdist);
    
      // Distance to quad
      if (dist) *dist=sdist;
      return in_quad;

    } else {
      Throw() << "Only quadrilaterals and triangles are currently supported in the 2D mapping code.";
    }
  } else if (mb_sph_map_type==MB_MAP_TYPE_GREAT_CIRCLE) {
      double p1,p2; 

      // Get rid of degenerate edges
      // TODO: Get rid of this call here too!
      int first_removed_ind=-1;
      remove_0len_edges3D(&num_pnts, pnts, &first_removed_ind);

      //printf("G. CIRCLE \n");

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
        bool in_tri = tri_shape_func::is_in(pcoord, &sdist);
        
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
        
        // Transform quad parametric coords from [0,1] to [-1,1] for consistancy
        pcoord[0]=2*p1-1.0;
        pcoord[1]=2*p2-1.0;

        // do is in
        double sdist;
        bool in_quad = quad_shape_func::is_in(pcoord, &sdist);
        
        // Distance to quad
        if (dist) *dist=sdist;
        return in_quad;

      }  else {
        // This is a degenerate cell so we can't map to it. 
        // Could throw an error here, but for flexiblity allow
        // these degenerate cells for now, but simply don't map to them.
        return false;
      }
  } else {
    Throw() << "Unrecognized line type. \n";
  }
  // Shouldn't be able to get here, but just in case...
  return false;
 
#undef PM_MAX_PNTS_IN_POLY
}


template<class SFUNC_TYPE,typename MPTRAITS,int SPATIAL_DIM, int PARAMETRIC_DIM>
bool MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::is_in_cell(const double *mdata,
                                                                              const double *point,
                                                                              double *pcoord,
                                                                              double *dist) const
{

 /* XMRKX */

  // Init output
  if (dist) *dist = std::numeric_limits<double>::max();

#define SPH_MAP
#ifdef SPH_MAP
  // Is this spherical and 3D, then use Spherical mapping
  // TODO: ORGANIZE THIS BETTER
  // ALSO NEED TO MAKE SURE IS HEX.


  if (mb_sph_map_type==MB_MAP_TYPE_GREAT_CIRCLE) {
    double p[3];

    // Only support hexes in spherical right now
    if (SFUNC_TYPE::ndofs != 8) {
      Throw() << "3D spherical mapping is only currently supported for hexahedrons";
    }

    // Rough is outside precheck
    if (is_outside_hex_sph3D_xyz(mdata, point)) {
      //// Is outside, so just leave
      pcoord[0]=0.0; pcoord[1]=0.0; pcoord[2]=0.0;
      return false;
    }

    // Calculate p in hex defined by mdata
    if (!calc_p_hex_sph3D_xyz(mdata, point, p)) {
      //// Didn't converge, so just leave...
      pcoord[0]=0.0; pcoord[1]=0.0; pcoord[2]=0.0;
      return false;
    }

    //printf(" In Spherical p=%f %f %f\n",p[0],p[1],p[2]);

    // Compute pcoord from p
    pcoord[0]=2.0*p[0]-1.0;
    pcoord[1]=2.0*p[1]-1.0;
    pcoord[2]=2.0*p[2]-1.0;


  } else if (mb_sph_map_type==MB_MAP_TYPE_CART_APPROX) { // Cartesian method
#endif

  // Newton's method
  const double ctol = 1e-10;
  const int max_iter = 15;

  double s[pdim];
  double delta_s[pdim];
  double res[pdim];
  double jac[sdim][sdim];
  double jac_inv[sdim][sdim];
  double sgrads[SFUNC_TYPE::ndofs][sdim];
  double dnorm = 0.0, rnorm = 0.0;
  int niters = 0;

  for (int i = 0; i < pdim; i++) {
    s[i] = delta_s[i] = res[i] = 0.0;
  }

  MB_POLY_Mapping<SFUNC_TYPE,MBTraits<>,SPATIAL_DIM,PARAMETRIC_DIM> *mpstd =
    trade<MBTraits<> >();
  bool converged = false;
  do {
    SFUNC_TYPE::shape_grads(1, s, &sgrads[0][0]);
    // Calculate residual.  Also use loop to start jacobian
    mpstd->forward(1, mdata, s, res); // F($)
    rnorm = 0.0;
    for (int i = 0; i < sdim; i++) {
      res[i] = point[i] - res[i];  // x - F(x) = -R(x)
      rnorm += res[i]*res[i];
      // Load forward jacobian
      for (int j = 0; j < sdim; j++) {
        jac[i][j] = 0.0;
        for (int k = 0; k < SFUNC_TYPE::ndofs; k++) {
          jac[i][j] += sgrads[k][j]*mdata[k*sdim+i];
        }
      }
    } // for sdim

    /*  rnorm is small even when very bad.
    if (rnorm < ctol) {
      converged = true;
      break;
    }
    */

    // So now res holds -F(x) and jac has jacobian.  We must invert the jacobian
    MB_POLY_Mapping_jacobian_invert<sdim>(&jac[0][0], &jac_inv[0][0]);

    // delta_s = jac_inv*res
    dnorm = 0;
    for (int i = 0; i < sdim; i++) {
      delta_s[i] = 0.0;
      for (int j = 0; j < sdim; j++) {
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

  for (int i = 0; i < sdim; i++) 
    pcoord[i] = s[i];

  if (dist) *dist = 0.0;

  if (!converged) {
    if (dist) *dist = std::numeric_limits<double>::max();

    return false;
  }
#ifdef SPH_MAP
  } else {
    Throw() << "Unrecognized line type. \n";
  }
#endif

  // check parametric bounds.
  double sdist=0.0;
  bool resu = SFUNC_TYPE::is_in(pcoord, &sdist);
  if(dist) *dist = sdist;
  return resu;
}

template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
void MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::forward(const unsigned int npts,
                          const mdata_type mdata[],
                          const pcoord_type points[],
                          typename mbmesh_richest_type<mdata_type,pcoord_type>::value  results[]) const
{
  // We get the shape function values at points
  std::vector<pcoord_type> svals(npts*SFUNC_TYPE::ndofs);

  SFUNC_TYPE::shape(npts, points, &svals[0]);

/*
std::cout << "shape:";
for (int i = 0; i < npts; i++) {
for (int j = 0; j < SFUNC_TYPE::ndofs; j++) {
  std::cout << svals[i][j] << ", ";
}
std::cout << std::endl;
}
*/

  for (int j = 0; j < npts; j++) {
    for (int i = 0; i < sdim; i++) {
    results[j*sdim+i] = 0.0;
      for (int ncf = 0; ncf < SFUNC_TYPE::ndofs; ncf++) {
        results[j*sdim+i] += mdata[ncf*sdim+i]*svals[j*SFUNC_TYPE::ndofs+ncf];
      } // ncf
    } // sdim
  } // npts
}

template<class SFUNC_TYPE,typename MPTRAITS>
void MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::forward(const unsigned int npts,
                          const mdata_type mdata[],
                          const pcoord_type points[],
                          typename mbmesh_richest_type<mdata_type,pcoord_type>::value results[]) const
{
  // We get the shape function values at points
  std::vector<pcoord_type> svals(npts*SFUNC_TYPE::ndofs);

  SFUNC_TYPE::shape(npts, points, &svals[0]);

/*
std::cout << "shape:";
for (int i = 0; i < npts; i++) {
for (int j = 0; j < SFUNC_TYPE::ndofs; j++) {
  std::cout << svals[i][j] << ", ";
}
std::cout << std::endl;
}
*/

  for (int j = 0; j < npts; j++) {
    for (int i = 0; i < sdim; i++) {
    results[j*sdim+i] = 0.0;
      for (int ncf = 0; ncf < SFUNC_TYPE::ndofs; ncf++) {
        results[j*sdim+i] += mdata[ncf*sdim+i]*svals[j*SFUNC_TYPE::ndofs+ncf];
      } // ncf
    } // sdim
  } // npts
}

template<class SFUNC_TYPE,typename MPTRAITS>
void MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::normal(UInt npts, const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename mbmesh_richest_type<mdata_type,pcoord_type>::value result[]) const {

  std::vector<pcoord_type> sgrads(npts*SFUNC_TYPE::ndofs*pdim);
  // First, calculate \partial_eta F, \partial_xi F

  SFUNC_TYPE::shape_grads(npts,pcoord, &sgrads[0]);

  for (UInt p = 0; p < npts; p++) {
    typename mbmesh_richest_type<mdata_type,pcoord_type>::value Feta[sdim];
    typename mbmesh_richest_type<mdata_type,pcoord_type>::value Fxi[sdim];
  
    for (UInt i = 0; i < sdim; i++) {
      Feta[i] = 0.0; Fxi[i] = 0.0;
      for (UInt n = 0; n < SFUNC_TYPE::ndofs; n++) {
        Feta[i] += mdata[n*sdim+i]*sgrads[(p*SFUNC_TYPE::ndofs+n)*pdim+0];
        Fxi[i] += mdata[n*sdim+i]*sgrads[(p*SFUNC_TYPE::ndofs+n)*pdim+1];
      }
    }
  
  /*
std::cout << "Feta:" << Feta[0] << ", " << Feta[1] << "," << Feta[2] << std::endl;
std::cout << "Fxi:" << Fxi[0] << ", " << Fxi[1] << "," << Fxi[2] << std::endl;
*/

  // Cross product
    result[p*sdim +0] = Feta[1]*Fxi[2] -Feta[2]*Fxi[1];
    result[p*sdim+1] = Feta[2]*Fxi[0] - Feta[0]*Fxi[2];
    result[p*sdim+2] = Feta[0]*Fxi[1] - Feta[1]*Fxi[0];
  }

/*
ScalarT dot= result[0]*Feta[0]+result[1]*Feta[1]+result[2]*Feta[2];
std::cout << "dot=" << dot << std::endl;
*/
}

template<class SFUNC_TYPE,typename MPTRAITS, int SPATIAL_DIM, int PARAMETRIC_DIM>
void MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,SPATIAL_DIM,PARAMETRIC_DIM>::jac_inv(
                           const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename mbmesh_richest_type<mdata_type,pcoord_type>::value result[]) const {
  typename mbmesh_richest_type<mdata_type,pcoord_type>::value jac[sdim][sdim];
  std::vector<pcoord_type> sgrads(SFUNC_TYPE::ndofs*sdim);

  SFUNC_TYPE::shape_grads(1, pcoord, &sgrads[0]);
  for (int i = 0; i < sdim; i++) {
    for (int j = 0; j < sdim; j++) {
      jac[i][j] = 0.0;
      for (int k = 0; k < SFUNC_TYPE::ndofs; k++) {
        jac[i][j] += sgrads[k*sdim+j]*mdata[k*sdim+i];
      }
    }
  } // for sdim

  MB_POLY_Mapping_jacobian_invert<sdim>(&jac[0][0], result);

}

template<class SFUNC_TYPE,typename MPTRAITS>
void MB_POLY_Mapping<SFUNC_TYPE,MPTRAITS,3,2>::jac_inv(
                           const mdata_type mdata[],
                           const pcoord_type pcoord[],
                                 typename mbmesh_richest_type<mdata_type,pcoord_type>::value result[]) const {
  typename mbmesh_richest_type<mdata_type,pcoord_type>::value jac[sdim][sdim];
  std::vector<pcoord_type> sgrads(SFUNC_TYPE::ndofs*pdim);

  SFUNC_TYPE::shape_grads(1, pcoord, &sgrads[0]);
  for (int i = 0; i < sdim; i++) {
    for (int j = 0; j < pdim; j++) {
      jac[i][j] = 0.0;
      for (int k = 0; k < SFUNC_TYPE::ndofs; k++) {
        jac[i][j] += sgrads[k*pdim+j]*mdata[k*sdim+i];
      }
    }
  } // for sdim

  // Now put the normal in the far right column
  typename mbmesh_richest_type<mdata_type,pcoord_type>::value n[sdim];
  normal(1, mdata, pcoord, &n[0]);

  for (UInt i = 0; i < sdim; i++) jac[i][2] = n[i];

  MB_POLY_Mapping_jacobian_invert<sdim>(&jac[0][0], result);
}

// Invert a 2d jacobian.
template<>
void MB_POLY_Mapping_jacobian_invert<2>(const double jac[], double jac_inv[]) {
  double deti = 1.0/(jac[0]*jac[3] - jac[1]*jac[2]);

  jac_inv[0] = deti*jac[3];
  jac_inv[1] = -deti*jac[1];
  jac_inv[2] = -deti*jac[2];
  jac_inv[3] = deti*jac[0];
}

template<>
void MB_POLY_Mapping_jacobian_invert<3>(const double jac[], double jac_inv[]) {
  const double deti = 1.0 / ( +jac[0] * (jac[4]*jac[8] - jac[5]*jac[7])
                                -jac[1] * (jac[3]*jac[8] - jac[5]*jac[6])
                                +jac[2] * (jac[3]*jac[7] - jac[4]*jac[6]) );
  
  jac_inv[0] = (jac[4]*jac[8] - jac[5]*jac[7]) * deti;
  jac_inv[1] = (jac[2]*jac[7] - jac[1]*jac[8]) * deti;
  jac_inv[2] = (jac[1]*jac[5] - jac[2]*jac[4]) * deti;

  jac_inv[3] = (jac[5]*jac[6] - jac[3]*jac[8]) * deti;
  jac_inv[4] = (jac[0]*jac[8] - jac[2]*jac[6]) * deti;
  jac_inv[5] = (jac[2]*jac[3] - jac[0]*jac[5]) * deti;

  jac_inv[6] = (jac[3]*jac[7] - jac[4]*jac[6]) * deti;
  jac_inv[7] = (jac[1]*jac[6] - jac[0]*jac[7]) * deti;
  jac_inv[8] = (jac[0]*jac[4] - jac[1]*jac[3]) * deti;
}


// Also instantiates
MBMappingBase *MBTopo2Map::operator()(const std::string &toponame) {
  const std::string &name = toponame;

  if (name == "QUAD" || name == "QUAD4") {
    return MB_POLY_Mapping<quad_shape_func,MBTraits<> >::instance();
  } else if (name == "QUAD_3D") {
    return MB_POLY_Mapping<quad_shape_func,MBTraits<>,3,2>::instance();
  } else if (name == "TRI" || name == "TRI3") {
    return MB_POLY_Mapping<tri_shape_func,MBTraits<> >::instance();
  } else if (name == "TRI3_3D" || name == "TRI_3D") {
    return MB_POLY_Mapping<tri_shape_func,MBTraits<>,3,2>::instance();
  } else if (name == "QUAD9") {
    return MB_POLY_Mapping<quad9_shape_func,MBTraits<> >::instance();
  }
  else if (name == "SHELL" || name == "SHELL4") {
    return MB_POLY_Mapping<quad_shape_func,MBTraits<> ,3,2>::instance();
  }
  else if (name == "SHELL3") {
    return MB_POLY_Mapping<tri_shape_func,MBTraits<> ,3,2>::instance();
  }
  else if (name == "SHELL9") {
    return MB_POLY_Mapping<quad9_shape_func,MBTraits<> ,3,2>::instance();
  }
  else if (name == "HEX" || name == "HEX8") {
    return MB_POLY_Mapping<hex_shape_func,MBTraits<> >::instance();
  }
  else if (name == "TETRA" || name == "TETRA4") {
    return MB_POLY_Mapping<tet_shape_func,MBTraits<> >::instance();
  }
  else {
    std::string err = "Cant find mapping for topo:" + name + "!!!";
    throw(err.c_str());
  }
}

// TRI
template class MB_POLY_Mapping<tri_shape_func,MBTraits<> >;
template class MB_POLY_Mapping<tri_shape_func,MBTraits<>, 3,2>;

// QUAD
template class MB_POLY_Mapping<quad_shape_func,MBTraits<> >;
template class MB_POLY_Mapping<quad_shape_func,MBTraits<>, 3,2>;

// HEX
template class MB_POLY_Mapping<hex_shape_func,MBTraits<> >;

// TET
template class MB_POLY_Mapping<tet_shape_func,MBTraits<> >;


///////////////////////////////// OLD //////////////////////////////



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

bool MBElemMap::cartesian_eval(const double *mdata, const double *point, int num_pnts, double *pcoord, double *dist)
{
  // Init output
  if (dist) *dist = std::numeric_limits<double>::max();

  // Newton's method
  const double ctol = 1e-10;
  const int max_iter = 15;

  int pdim=2, sdim=2, ndof=2;

  // MB_POLY_Mapping mpstd, jacobian_invert;

  double s[pdim];
  double delta_s[pdim];
  double res[pdim];
  double jac[sdim][sdim];
  double jac_inv[sdim][sdim];
  // double sgrads[SFUNC_TYPE::ndofs][sdim];
  double sgrads[num_pnts][sdim];
  double dnorm = 0.0, rnorm = 0.0;
  int niters = 0;
  double one4th = 1./4.;

  for (int i = 0; i < pdim; i++) {
    s[i] = delta_s[i] = res[i] = 0.0;
  }

  // MB_POLY_Mapping<SFUNC_TYPE,MBTraits<>,SPATIAL_DIM,PARAMETRIC_DIM> *mpstd =
    // trade<MBTraits<> >();
  bool converged = false;
  do {
    // replace shape_grads call with if/else block below (from Mapping)
    // SFUNC_TYPE::shape_grads(1, s, &sgrads[0][0]);
    // triangle
    if (num_pnts == 3){
      // dof 1
      sgrads[0][0] = -1.0;
      sgrads[0][1] = -1.0;

      // dof 2
      sgrads[1][0] = 1.0;
      sgrads[1][1] = 0.0;

      // dof 3
      sgrads[2][0] = 0.0;
      sgrads[2][1] = 1.0;

    } else if (num_pnts == 4){
      double xi = s[0], eta = s[1];

      // dof 1
      sgrads[0][0] = -one4th*(1.0-eta);
      sgrads[0][1] = -one4th*(1.0-xi);

      // dof 2
      sgrads[1][0] = one4th*(1.0-eta);
      sgrads[1][1] = -one4th*(1.0+xi);

      // dof 3
      sgrads[2][0] = one4th*(1.0+eta);
      sgrads[2][1] = one4th*(1.0+xi);

      // dof 4
      sgrads[3][0] = -one4th*(1.0+eta);
      sgrads[3][1] = one4th*(1.0-xi);
    }
    
    // Calculate residual.  Also use loop to start jacobian
    
    // replace forward call with loops below
    // mpstd->forward(1, mdata, s, res); // F($)
    
    // ---------------------------- forward ----------------------------------
    // We get the shape function values at points
    std::vector<double> svals(num_pnts);

    if (num_pnts == 3){
        // this comes from the shape() call inside of forward()
        double xi = s[0], eta = s[1];
        svals[0] = 1.0 - xi - eta;
        svals[1] = xi;
        svals[2] = eta;
    } else if (num_pnts == 4){
        double xi = s[0], eta = s[1];
        // this comes from the shape() call inside of forward()
        svals[0] = one4th*(1.0-xi)*(1.0-eta);
        svals[1] = one4th*(1.0+xi)*(1.0-eta);
        svals[2] = one4th*(1.0+xi)*(1.0+eta);
        svals[3] = one4th*(1.0-xi)*(1.0+eta);
    }

    for (int i = 0; i < sdim; i++) {
      res[i] = 0.0;
      for (int ncf = 0; ncf < num_pnts; ncf++) {
        res[i] += mdata[ncf*sdim+i]*svals[ncf];
      } // ncf
    } // sdim

    // ---------------------------- forward ----------------------------------
    rnorm = 0.0;
    for (int i = 0; i < sdim; i++) {
      res[i] = point[i] - res[i];  // x - F(x) = -R(x)
      rnorm += res[i]*res[i];
      // Load forward jacobian
      for (int j = 0; j < sdim; j++) {
        jac[i][j] = 0.0;
        for (int k = 0; k < num_pnts; k++) {
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
    // MB_POLY_Mapping_jacobian_invert<sdim>(&jac[0][0], &jac_inv[0][0]);
    double deti = 1.0/(jac[0][0]*jac[1][1] - jac[0][1]*jac[1][0]);
    jac_inv[0][0] = deti*jac[1][1];
    jac_inv[0][1] = -deti*jac[0][1];
    jac_inv[1][0] = -deti*jac[1][0];
    jac_inv[1][1] = deti*jac[0][0];

    // delta_s = jac_inv*res
    dnorm = 0;
    for (int i = 0; i < sdim; i++) {
      delta_s[i] = 0.0;
      for (int j = 0; j < sdim; j++) {
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

  for (int i = 0; i < sdim; i++)
    pcoord[i] = s[i];

  if (dist) *dist = 0.0;

  if (!converged) {
    if (dist) *dist = std::numeric_limits<double>::max();
#ifdef DEBUG_CARTMAP
  printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEBUG ~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
  printf("Newton iteration did not converge!! dnorm = %f\n", dnorm);
  printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEBUG ~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
#endif
    return false;
  }

  bool in=true;
  if (num_pnts == 3){
    const double in_tol = 1e-10;

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

  } else if (num_pnts == 4){
    const double in_tol = 1e-10;
    double max_out[2]={0.0,0.0};

    if (pcoord[0] < -1.0-in_tol) {
      max_out[0]=-1.0 - pcoord[0];
      in= false;
    } else if (pcoord[0] > 1.0+in_tol) {
      max_out[0]=pcoord[0] - 1.0;
      in= false;
    }

    if (pcoord[1] < -1.0-in_tol) {
      max_out[1]=-1.0 - pcoord[1];
      in= false;
    } else if (pcoord[1] > 1.0+in_tol) {
      max_out[1]=pcoord[1] - 1.0;
      in= false;
    }

    if (dist) *dist=std::sqrt(max_out[0]*max_out[0]+max_out[1]*max_out[1]);

  }
  
  // pcoords should not be negative
  pcoord[0] = std::abs(pcoord[0]);
  pcoord[1] = std::abs(pcoord[1]);
  pcoord[2] = std::abs(pcoord[2]);
  
  // check parametric bounds.
  // double sdist=0.0;
  // bool resu = SFUNC_TYPE::is_in(pcoord, &sdist);
  // if(dist) *dist = sdist;
  return in;
}

} // namespace
#endif
