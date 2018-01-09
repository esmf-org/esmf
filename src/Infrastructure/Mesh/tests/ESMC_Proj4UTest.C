// $Id$
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

#include <cstring>
#include <cstdio>
using namespace std;

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

// PROJ.4 header and settings (if linked)
#if defined (ESMF_PROJ4)
#include <proj_api.h>
#define TOLERANCE 1E-4
#endif

//==============================================================================
//BOP
// !PROGRAM: ESMC_Proj4UTest - Check for PROJ.4 functionality
//
// !DESCRIPTION: This set of unit tests test the functionality of the
// PROJ.4 cartographic projections library in ESMF.  For more information
// on PROJ.4, see https://trac.osgeo.org/proj/
//    
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  int localPet, petCount;
  ESMC_VM vm;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL, (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_LogSet(true);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "PROJ.4 rotated pole transform test");
  strcpy(failMsg, "PROJ.4 not linked so this test should have passed");
#if defined (ESMF_PROJ4)
  double x_deg = -25.075000762939453;
  double y_deg = -20.075000762939453;
  double x_deg_rot = 0.0;
  double y_deg_rot = 0.0;
  double x_actual = -8.33782;
  double y_actual = 26.2072;
  double x_diff = 1e30;
  double y_diff = 1e30;
  const char *proj_src_str = "+proj=longlat +a=6370997 +b=6370997 +towgs84=0,0,0,0,0,0,0 +no_defs";
  const char *proj_dst_str = "+proj=ob_tran +o_proj=latlon +o_lon_p=-162.0 +o_lat_p=39.25 +lon_0=180 +ellps=sphere";
  
  // Convert the coordinate values to radians.
  double x_rad = x_deg * DEG_TO_RAD;
  double y_rad = y_deg * DEG_TO_RAD;

  // Initialize source and destination PROJ.4 projections.
  projPJ proj_src = pj_init_plus(proj_src_str);
  projPJ proj_dst = pj_init_plus(proj_dst_str);
  if (!proj_src) {
    strcpy(failMsg, "PROJ.4 failure to initialize source projection");
  }
  else if (!proj_dst) {
    strcpy(failMsg, "PROJ.4 failure to initialize destination projection");
  }
  else {
    // Perform PROJ.4 transformation and check accuracy of result.
    strcpy(failMsg, "PROJ.4 rotated coordinate values not within tolerance");
    int p = pj_transform(proj_src, proj_dst, 1, 1, &x_rad, &y_rad, NULL);
    x_deg_rot = x_rad *= RAD_TO_DEG;
    y_deg_rot = y_rad *= RAD_TO_DEG;
    x_diff = fabs(x_deg_rot - x_actual);
    y_diff = fabs(y_deg_rot - y_actual);
    printf("x_deg_rot = %.14lf\n", x_deg_rot);
    printf("y_deg_rot = %.14lf\n", y_deg_rot);
    printf("x_actual = %.14lf\n", x_actual);
    printf("y_actual = %.14lf\n", y_actual);
    printf("x_diff = %.14lf\n", x_diff);
    printf("y_diff = %.14lf\n", y_diff);
  }

  // Free up resources associated with source and destination 
  // PROJ.4 projections.
  if (proj_src) {
    pj_free (proj_src);
  }
  if (proj_dst) {
    pj_free (proj_dst);
  }

  // Final ESMC test check and logging.
  ESMC_Test((x_diff < TOLERANCE) && (y_diff < TOLERANCE), 
            name, failMsg, &result, __FILE__, __LINE__, 0);
#else
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
