// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_ClumpPnts_h
#define ESMCI_ClumpPnts_h

namespace ESMCI {

////
// More ESMF-y version of call with log error and return code
//
// Inputs:
//   num_pnt - the number of input points
//   pnt_lon - the longitudes in degrees of each point (array is of size num_pnt)
//   pnt_lat - the latitudes in degrees of each point (array is of size num_pnt)
//   tol     - the tolerence with which to clump points (measured in Cartesian space on a radius=1.0 sphere)
// Outputs:
//   pnt_cl_ind   - for each point, the index into _cl_lon and _cl_lat indicating which point it's clumped with
//                   (array is of size num_pnt)
//   num_cl       - the number of points after clumping
//   cl_lon       - the longitudes in degrees of each point (array is of size num_cl)
//   cl_lat       - the latitudes in degrees of each point (array is of size num_cl)
//   max_size_cl  - the maximum number of a points in a clump
//   rc            - ESMF return code
//
/////
void ClumpPnts(int num_pnt, double *pnt_lon, double *pnt_lat, double tol, int *pnt_cl_ind,
               int *num_cl, double **cl_lon, double **cl_lat, int *max_size_cl, int *rc);





////
// More ESMF-y version of call with log error and return code
//
// Inputs:
//   num_pnt - the number of input points
//   pnt_lon - the longitudes in degrees of each point (array is of size num_pnt)
//   pnt_lat - the latitudes in degrees of each point (array is of size num_pnt)
//   tol     - the tolerence with which to clump points (measured in degrees)
// Outputs:
//   pnt_cl_ind        - for each point, the index into _cl_lon and _cl_lat indicating which point it's clumped with
//                   (array is of size num_pnt)
//   num_cl       - the number of points after clumping
//   cl_lon       - the longitudes in degrees of each clump point (array is of size num_cl)
//   cl_lat       - the latitudes in degrees of each clump point (array is of size num_cl)
//   max_size_cl  - the maximum number of a points in a clump
//   rc            - ESMF return code
//
/////
void ClumpPntsLL(int num_pnt, double *pnt_lon, double *pnt_lat, double tol, int *pnt_cl_ind,
                 int *num_cl, double **cl_lon, double **cl_lat, int *max_size_cl,
                 double start_lat, double end_lat, int *rc);




// Internal implemenation with exception based error handling
//
// Inputs:
//   num_pnt - the number of input points
//   pnt_lon - the longitudes in deg of each point (array is of size num_pnt)
//   pnt_lat - the latitudes in deg of each point (array is of size num_pnt)
//   tol     - the tolerence with which to clump points (in Cartesian space on a radius=1.0 sphere)
//   start_lat - when run in parallel, specify the range of the latitudes to be clumped
//   end_lat   - when run in parallel, specify the range of the latitudes to be clumped
// Outputs:
//   pnt_cl_ind    - for each point, the index into _cl_lon and _cl_lat indicating which point it's clumped with
//                   (array is of size num_pnt)
//   _num_cl       - the number of points after clumping
//   _cl_lon       - the longitudes in deg of each point (array is of size num_cl)
//   _cl_lat       - the latitudes in deg of each point (array is of size num_cl)
//   _max_size_cl  - the maximum number of a points in a clump
void clump_pnts(int num_pnt, double *pnt_lon, double *pnt_lat, double tol, int *pnt_cl_ind,
                int *_num_cl, double **_cl_lon, double **_cl_lat, int *_max_size_cl);



// Inputs:
//   num_pnt - the number of input points
//   pnt_lon - the longitudes in deg of each point (array is of size num_pnt)
//   pnt_lat - the latitudes in deg of each point (array is of size num_pnt)
//   tol     - the tolerence with which to clump points (in degrees)
// Outputs:
//   pnt_cl_ind        - for each point, the index into _cl_lon and _cl_lat indicating which point it's clumped with
//                   (array is of size num_pnt)
//   _num_cl       - the number of points after clumping
//   _cl_lon       - the longitudes in deg of each point (array is of size num_cl)
//   _cl_lat       - the latitudes in deg of each point (array is of size num_cl)
//   _max_size_cl  - the maximum number of a points in a clump
void clump_pnts_ll(int num_pnt, double *pnt_lon, double *pnt_lat, double tol, int *pnt_cl_ind,
                   int *_num_cl, double **_cl_lon, double **_cl_lat, int *_max_size_cl,
                   double start_lat, double end_lat);



} // namespace

#endif
