// $Id$
//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif
#include <mpi.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

// other headers
#include "ESMCI_Regrid_Nearest.h"
#include "ESMCI_PointList.h"

#include <iostream>
#include <iterator>
#include <vector>


#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

static double UNINITVAL2 = -42;

bool weights_correct_nearest(WMat &wts, std::vector<double> weights) {
  bool correct = true;
  if (weights[0] != UNINITVAL2) {
    int ind = 0;
    WMat::WeightMap::iterator mb = wts.begin_row(), me = wts.end_row();
    for(; mb != me; ++mb) {
      std::vector<WMat::Entry> row = mb->second;
      std::vector<WMat::Entry>::iterator vb = row.begin(), ve = row.end();
      for(; vb != ve; ++vb) {
        WMat::Entry rv = *vb;
        if (rv.value /= weights[ind]) correct = false;
        ++ind;
      }
    }
    if (weights.size() != ind) correct = false;
  }
  
  return correct;
}

#if defined ESMF_MOAB
bool weight_gen_nearest(PointList *pl1, PointList *pl, 
                        std::vector<double> weights,
                        int method = 3, int num_pts = 0,
                        double dist_exponent = 2) {
  bool correct = false;

  // early exit for ESMF_MOAB=OFF
  if (pl1 == NULL || pl == NULL)
    return true;
  
  // do bilinear regridding between mesh and pointlist
  WMat wt, dst_status;
  WMat &wts = wt;
  WMat &ds = dst_status;
  calc_nearest_regrid_wgts(pl1, pl, wts, true, ds, &method, &num_pts, &dist_exponent);

  // verify results
  if (weights_correct_nearest(wts, weights)) correct = true;

  // output weight matrix for debugging purposes
#define OUTPUT_WEIGHTS
#ifdef OUTPUT_WEIGHTS
  std::cout << std::endl << "Bilinear Weight Matrix" << std::endl;
  // print out weights
  WMat::WeightMap::iterator mb = wts.begin_row(), me = wts.end_row();
  for(; mb != me; ++mb) {
    WMat::Entry col = mb->first;
    std::vector<WMat::Entry> row = mb->second;

    std::cout << "[" << col.id << "," << col.idx << "," << col.value << ","
         << col.src_id << "] - ";

    std::vector<WMat::Entry>::iterator vb = row.begin(), ve = row.end();
    for(; vb != ve; ++vb) {
      WMat::Entry rv = *vb;
      std::cout << "[" << rv.id << "," << rv.idx << "," << rv.value << ","
           << rv.src_id << "] ";
    }
    std::cout << std::endl;
  }
  std::cout << std::endl;
#endif
  return correct;
}
#endif

PointList* create_pointlist(std::vector<double*> *cv, int &rc) {
  //
  //
  //  2.0   7 ------- 8 -------- 9
  //        |         |          |
  //        |         |          |
  //        |         |          |
  //  1.0   4 ------- 5 -------- 6
  //        |         |          |
  //        |         |          |
  //        |         |          |
  //  0.0   1 ------- 2 -------- 3
  //
  //       0.0       1.0        2.0
  //

  rc = ESMF_RC_NOT_IMPL;

  // clean out the cv list first in case it is being reused
  cv->clear();

  double *c = new double[2];
  c[0] = 0.0; c[1] = 0.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.0; c[1] = 0.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 0.0; c[1] = 1.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.0; c[1] = 1.0;
  cv->push_back(c);

  c = new double[2];
  c[0] = 1.0; c[1] = 0.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 2.0; c[1] = 0.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.0; c[1] = 1.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 2.0; c[1] = 1.0;
  cv->push_back(c);

  c = new double[2];
  c[0] = 0.0; c[1] = 1.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.0; c[1] = 1.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 0.0; c[1] = 2.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.0; c[1] = 2.0;
  cv->push_back(c);

  c = new double[2];
  c[0] = 1.0; c[1] = 1.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 2.0; c[1] = 1.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.0; c[1] = 2.0;
  cv->push_back(c);
  c = new double[2];
  c[0] = 2.0; c[1] = 2.0;
  cv->push_back(c);

  // 0,1,3,4
  // 1,2,4,5
  // 3,4,6,7
  // 4,5,7,8

  int np = 9;
  int dim = 2;

  double x[np];
  x[0] = 0.0;
  x[1] = 1.0;
  x[2] = 2.0;
  x[3] = 0.0;
  x[4] = 1.0;
  x[5] = 2.0;
  x[6] = 0.0;
  x[7] = 1.0;
  x[8] = 2.0;

  double y[np];
  y[0] = 0.0;
  y[1] = 0.0;
  y[2] = 0.0;
  y[3] = 1.0;
  y[4] = 1.0;
  y[5] = 1.0;
  y[6] = 2.0;
  y[7] = 2.0;
  y[8] = 2.0;

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = i+1;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_for_quad(std::vector<double*> *cv, int &rc) {
  //
  //
  //  1.5   3 ------- 4
  //        |         |
  //        |         |
  //        |         |
  //  0.5   1 ------- 2
  //
  //       0.5       1.5
  //

  rc = ESMF_RC_NOT_IMPL;

  // clean out the cv list first in case it is being reused
  cv->clear();

  double *c = new double[2];
  c[0] = 0.5; c[1] = 0.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.5; c[1] = 0.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 0.5; c[1] = 1.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.5; c[1] = 1.5;
  cv->push_back(c);

  int np = 4;
  int dim = 2;

  double x[np];
  x[0] = 0.5;
  x[1] = 1.5;
  x[2] = 0.5;
  x[3] = 1.5;

  double y[np];
  y[0] = 0.5;
  y[1] = 0.5;
  y[2] = 1.5;
  y[3] = 1.5;

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = i+1;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_par(int &rc) {
  //
  //
  //  3.0    ------- 3 -------   ---   -------   ------- 4 -------   -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  2.0    ------- 1 -------   ---   -------   ------- 2 -------   -------
  //
  //  PET0
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  3.0    -------   ------- 3 ---   -------   -------   ------- 4 -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  2.0    -------   ------- 1 ---   -------   -------   ------- 2 -------
  //
  //  PET1
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  3.0  3 -------   -------   ---   ------- 4 -------   -------   -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  2.0  1 -------   -------   ---   ------- 2 -------   -------   -------
  //
  //  PET2
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  3.0    -------   -------   --- 3 -------   -------   -------   ------- 4
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  2.0    -------   -------   --- 1 -------   -------   -------   ------- 2
  //
  //  PET3
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //

  rc = ESMF_RC_NOT_IMPL;

  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return NULL;

  if (petCount != 4) {
    Throw() << "Test function must be run with 4 processors";
    return NULL;
  }

  int np = 4;
  int dim = 2;

  double x[np];
  double y[np];

  if (localPet == 0) {
    x[0] = 1.5;
    x[1] = 2.5;
    x[2] = 1.5;
    x[3] = 2.5;

    y[0] = 2.0;
    y[1] = 2.0;
    y[2] = 3.0;
    y[3] = 3.0;
  } else if (localPet == 1) {
    x[0] = 1.75;
    x[1] = 2.75;
    x[2] = 1.75;
    x[3] = 2.75;

    y[0] = 2.0;
    y[1] = 2.0;
    y[2] = 3.0;
    y[3] = 3.0;
  } else if (localPet == 2) {
    x[0] = 1.25;
    x[1] = 2.25;
    x[2] = 1.25;
    x[3] = 2.25;

    y[0] = 2.0;
    y[1] = 2.0;
    y[2] = 3.0;
    y[3] = 3.0;
  } else if (localPet == 3) {
    x[0] = 1.95;
    x[1] = 2.95;
    x[2] = 1.95;
    x[3] = 2.95;

    y[0] = 2.0;
    y[1] = 2.0;
    y[2] = 3.0;
    y[3] = 3.0;
  }

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = localPet*petCount+i;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_par_sph(int &rc) {
  //
  //
  //  3.0    ------- 3 -------   ---   -------   ------- 4 -------   -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  2.0    ------- 1 -------   ---   -------   ------- 2 -------   -------
  //
  //  PET0
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  3.0    -------   ------- 3 ---   -------   -------   ------- 4 -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  2.0    -------   ------- 1 ---   -------   -------   ------- 2 -------
  //
  //  PET1
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  3.0  3 -------   -------   ---   ------- 4 -------   -------   -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  2.0  1 -------   -------   ---   ------- 2 -------   -------   -------
  //
  //  PET2
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  3.0    -------   -------   --- 3 -------   -------   -------   ------- 4
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  2.0    -------   -------   --- 1 -------   -------   -------   ------- 2
  //
  //  PET3
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //

  rc = ESMF_RC_NOT_IMPL;

  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return NULL;

  if (petCount != 4) {
    Throw() << "Test function must be run with 4 processors";
    return NULL;
  }

  int np = 4;
  int dim = 3;

  double x[np];
  double y[np];

  int pi2 = 3.14159/2;

  double z[np];
  z[0] = 1;
  z[1] = 1;
  z[2] = 1;
  z[3] = 1;

  if (localPet == 0) {
    x[0] = 1.5 * pi2;
    x[1] = 2.5 * pi2;
    x[2] = 1.5 * pi2;
    x[3] = 2.5 * pi2;

    y[0] = 2.0 * pi2;
    y[1] = 2.0 * pi2;
    y[2] = 3.0 * pi2;
    y[3] = 3.0 * pi2;
  } else if (localPet == 1) {
    x[0] = 1.75 * pi2;
    x[1] = 2.75 * pi2;
    x[2] = 1.75 * pi2;
    x[3] = 2.75 * pi2;

    y[0] = 2.0 * pi2;
    y[1] = 2.0 * pi2;
    y[2] = 3.0 * pi2;
    y[3] = 3.0 * pi2;
  } else if (localPet == 2) {
    x[0] = 1.25 * pi2;
    x[1] = 2.25 * pi2;
    x[2] = 1.25 * pi2;
    x[3] = 2.25 * pi2;

    y[0] = 2.0 * pi2;
    y[1] = 2.0 * pi2;
    y[2] = 3.0 * pi2;
    y[3] = 3.0 * pi2;
  } else if (localPet == 3) {
    x[0] = 1.95 * pi2;
    x[1] = 2.95 * pi2;
    x[2] = 1.95 * pi2;
    x[3] = 2.95 * pi2;

    y[0] = 2.0 * pi2;
    y[1] = 2.0 * pi2;
    y[2] = 3.0 * pi2;
    y[3] = 3.0 * pi2;
  }

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = localPet*petCount+i;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    p.coords[2] = z[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_for_quad_parallel(int &rc) {
  //
  //
  //  2.5    ------- 3 -------   ---   -------   ------- 4 -------   -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  1.5    ------- 1 -------   ---   -------   ------- 2 -------   -------
  //
  //  PET0
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  2.5    -------   ------- 3 ---   -------   -------   ------- 4 -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  1.5    -------   ------- 1 ---   -------   -------   ------- 2 -------
  //
  //  PET1
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  2.5  3 -------   -------   ---   ------- 4 -------   -------   -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  1.5  1 -------   -------   ---   ------- 2 -------   -------   -------
  //
  //  PET2
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  2.5    -------   -------   --- 3 -------   -------   -------   ------- 4
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  1.5    -------   -------   --- 1 -------   -------   -------   ------- 2
  //
  //  PET3
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //

  rc = ESMF_RC_NOT_IMPL;

  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return NULL;

  if (petCount != 4) {
    Throw() << "Test function must be run with 4 processors";
    return NULL;
  }

  int np = 4;
  int dim = 2;

  double x[np];
  double y[np];

  if (localPet == 0) {
    x[0] = 1.5;
    x[1] = 2.5;
    x[2] = 1.5;
    x[3] = 2.5;

    y[0] = 1.5;
    y[1] = 1.5;
    y[2] = 2.5;
    y[3] = 2.5;
  } else if (localPet == 1) {
    x[0] = 1.75;
    x[1] = 2.75;
    x[2] = 1.75;
    x[3] = 2.75;

    y[0] = 1.5;
    y[1] = 1.5;
    y[2] = 2.5;
    y[3] = 2.5;
  } else if (localPet == 2) {
    x[0] = 1.25;
    x[1] = 2.25;
    x[2] = 1.25;
    x[3] = 2.25;

    y[0] = 1.5;
    y[1] = 1.5;
    y[2] = 2.5;
    y[3] = 2.5;
  } else if (localPet == 3) {
    x[0] = 1.95;
    x[1] = 2.95;
    x[2] = 1.95;
    x[3] = 2.95;

    y[0] = 1.5;
    y[1] = 1.5;
    y[2] = 2.5;
    y[3] = 2.5;
  }

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = localPet*petCount+i;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_sph_for_quad_parallel(int &rc) {
  //
  //
  //  2.5    ------- 3 -------   ---   -------   ------- 4 -------   -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  1.5    ------- 1 -------   ---   -------   ------- 2 -------   -------
  //
  //  PET0
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  2.5    -------   ------- 3 ---   -------   -------   ------- 4 -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  1.5    -------   ------- 1 ---   -------   -------   ------- 2 -------
  //
  //  PET1
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  2.5  3 -------   -------   ---   ------- 4 -------   -------   -------
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  1.5  1 -------   -------   ---   ------- 2 -------   -------   -------
  //
  //  PET2
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //
  //  2.5    -------   -------   --- 3 -------   -------   -------   ------- 4
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //       |         |         |     |         |         |         |         |
  //  1.5    -------   -------   --- 1 -------   -------   -------   ------- 2
  //
  //  PET3
  //
  //     1.25       1.5       1.75  1.95       2.25     2.5      2.75      2.95
  //

  rc = ESMF_RC_NOT_IMPL;

  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return NULL;

  if (petCount != 4) {
    Throw() << "Test function must be run with 4 processors";
    return NULL;
  }

  int np = 4;
  int dim = 3;

  double x[np];
  double y[np];

  int pi2 = 3.14159/2;

  double z[np];
  z[0] = 1;
  z[1] = 1;
  z[2] = 1;
  z[3] = 1;

  if (localPet == 0) {
    x[0] = 1.5 * pi2;
    x[1] = 2.5 * pi2;
    x[2] = 1.5 * pi2;
    x[3] = 2.5 * pi2;

    y[0] = 1.5 * pi2;
    y[1] = 1.5 * pi2;
    y[2] = 2.5 * pi2;
    y[3] = 2.5 * pi2;
  } else if (localPet == 1) {
    x[0] = 1.75 * pi2;
    x[1] = 2.75 * pi2;
    x[2] = 1.75 * pi2;
    x[3] = 2.75 * pi2;

    y[0] = 1.5 * pi2;
    y[1] = 1.5 * pi2;
    y[2] = 2.5 * pi2;
    y[3] = 2.5 * pi2;
  } else if (localPet == 2) {
    x[0] = 1.25 * pi2;
    x[1] = 2.25 * pi2;
    x[2] = 1.25 * pi2;
    x[3] = 2.25 * pi2;

    y[0] = 1.5 * pi2;
    y[1] = 1.5 * pi2;
    y[2] = 2.5 * pi2;
    y[3] = 2.5 * pi2;
  } else if (localPet == 3) {
    x[0] = 1.95 * pi2;
    x[1] = 2.95 * pi2;
    x[2] = 1.95 * pi2;
    x[3] = 2.95 * pi2;

    y[0] = 1.5 * pi2;
    y[1] = 1.5 * pi2;
    y[2] = 2.5 * pi2;
    y[3] = 2.5 * pi2;
  }

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = localPet*petCount+i;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    p.coords[2] = z[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_for_quad_sph(std::vector<double*> *cv, int &rc) {
  //
  //
  // 3pi/20 3 ------- 4
  //        |         |
  //        |         |
  //        |         |
  //  pi/20 1 ------- 2
  //
  //       pi/20     3pi/20
  //

  rc = ESMF_RC_NOT_IMPL;

  double pi = 3.14159;

  // clean out the cv list first in case it is being reused
  cv->clear();

  double *c = new double[3];
  c[0] = pi/20; c[1] = pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = 3*pi/20; c[1] = pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = pi/20; c[1] = 3*pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = 3*pi/20; c[1] = 3*pi/20; c[2] = 1;
  cv->push_back(c);

  int np = 4;
  int dim = 3;

  double x[np];
  x[0] = pi/20;
  x[1] = 3*pi/20;
  x[2] = pi/20;
  x[3] = 3*pi/20;

  double y[np];
  y[0] = pi/20;
  y[1] = pi/20;
  y[2] = 3*pi/20;
  y[3] = 3*pi/20;

  double z[np];
  z[0] = 1;
  z[1] = 1;
  z[2] = 1;
  z[3] = 1;

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = i+1;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    p.coords[2] = z[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_for_tri(std::vector<double*> *cv, int &rc) {
  //
  //
  //  1.5   5 ------- 6 ------- 7 ------- 8
  //        |         |         |         |
  //        |         |         |         |
  //        |         |         |         |
  //  0.5   1 ------- 2 ------- 3 ------- 4
  //
  //       0.25      0.75      1.25     1.75

  rc = ESMF_RC_NOT_IMPL;

  // clean out the cv list first in case it is being reused
  cv->clear();

  double *c = new double[2];
  c[0] = 0.25; c[1] = 0.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 0.75; c[1] = 0.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.25; c[1] = 0.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.75; c[1] = 0.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 0.25; c[1] = 1.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 0.75; c[1] = 1.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.25; c[1] = 1.5;
  cv->push_back(c);
  c = new double[2];
  c[0] = 1.75; c[1] = 1.5;
  cv->push_back(c);

  int np = 8;
  int dim = 2;

  std::vector<double> x;
  x.reserve(np);
  x.push_back(0.25);
  x.push_back(0.75);
  x.push_back(1.25);
  x.push_back(1.75);
  x.push_back(0.25);
  x.push_back(0.75);
  x.push_back(1.25);
  x.push_back(1.75);

  std::vector<double> y;
  y.reserve(np);
  y.push_back(0.5);
  y.push_back(0.5);
  y.push_back(0.5);
  y.push_back(0.5);
  y.push_back(1.5);
  y.push_back(1.5);
  y.push_back(1.5);
  y.push_back(1.5);

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = i+1;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_for_tri_sph(std::vector<double*> *cv, int &rc) {
  //
  //
  // 3pi/20 5 ------- 6 ------- 7 ------- 8
  //        |         |         |         |
  //        |         |         |         |
  //        |         |         |         |
  //  pi/20 1 ------- 2 ------- 3 ------- 4
  //
  //       pi/40     3pi/40    5pi/40    7pi/40

  rc = ESMF_RC_NOT_IMPL;

  double pi = 3.14159;

  // clean out the cv list first in case it is being reused
  cv->clear();

  double *c = new double[3];
  c[0] = pi/40; c[1] = pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = 3*pi/40; c[1] = pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = 5*pi/40; c[1] = pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = 7*pi/40; c[1] = pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = pi/40; c[1] = 3*pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = 3*pi/40; c[1] = 3*pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = 5*pi/40; c[1] = 3*pi/20; c[2] = 1;
  cv->push_back(c);
  c = new double[3];
  c[0] = 7*pi/40; c[1] = 3*pi/20; c[2] = 1;
  cv->push_back(c);

  int np = 8;
  int dim = 3;

  std::vector<double> x;
  x.reserve(np);
  x.push_back(pi/40);
  x.push_back(3*pi/40);
  x.push_back(5*pi/40);
  x.push_back(7*pi/40);
  x.push_back(pi/40);
  x.push_back(3*pi/40);
  x.push_back(5*pi/40);
  x.push_back(1.75);

  std::vector<double> y;
  y.reserve(np);
  y.push_back(pi/20);
  y.push_back(pi/20);
  y.push_back(pi/20);
  y.push_back(pi/20);
  y.push_back(3*pi/20);
  y.push_back(3*pi/20);
  y.push_back(3*pi/20);
  y.push_back(3*pi/20);

  std::vector<double> z;
  z.reserve(np);
  z.push_back(1);
  z.push_back(1);
  z.push_back(1);
  z.push_back(1);
  z.push_back(1);
  z.push_back(1);
  z.push_back(1);
  z.push_back(1);

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = i+1;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    p.coords[2] = z[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_for_tet(std::vector<double*> *cv, int &rc) {
  //
  //
  //  1.5   3 ------- 4
  //        |         |
  //        |         |
  //        |         |
  //  0.5   1 ------- 2
  //
  //       0.5       1.5
  //

  rc = ESMF_RC_NOT_IMPL;

  // clean out the cv list first in case it is being reused
  cv->clear();

  double *c = new double[3];
  c[0] = 0.5; c[1] = 0.5; c[2] = 0.5;
  cv->push_back(c);
  c = new double[3];
  c[0] = 1.5; c[1] = 0.5; c[2] = 0.5;
  cv->push_back(c);
  c = new double[3];
  c[0] = 1.0; c[1] = 0.5; c[2] = 0.5;
  cv->push_back(c);
  c = new double[3];
  c[0] = 1.0; c[1] = 1.5; c[2] = 0.5;
  cv->push_back(c);

  int np = 4;
  int dim = 3;

  double x[np];
  x[0] = 0.5;
  x[1] = 1.5;
  x[2] = 1.0;
  x[3] = 1.0;

  double y[np];
  y[0] = 0.5;
  y[1] = 0.5;
  y[2] = 0.5;
  y[3] = 1.5;

  double z[np];
  z[0] = 0.5;
  z[1] = 0.5;
  z[2] = 0.5;
  z[3] = 0.5;

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = i+1;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    p.coords[2] = z[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}
