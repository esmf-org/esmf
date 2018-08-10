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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#include "ESMCI_RegionNode.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_TraceRegionUTest
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  int userRc;
  
  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  ESMCI::RegionNode rootNode(0);
  
  uint64_t enters[] = {0, 100, 200, 300, 400, 500, 600, 700, 800, 900};
  uint64_t exits[] = {10, 145, 222, 399, 402, 550, 676, 789, 899, 934}; 

  for (int i = 0; i < 10; i++) {
    rootNode.entered(enters[i]);
    rootNode.exited(exits[i]);
  }

  //----------------------------------------------------------------------------
  //NEX_UTest
  uint64_t total = 0;
  for (int i = 0; i < 10; i++) {
    total += exits[i] - enters[i];
  }  
  strcpy(name, "Region total");
  strcpy(failMsg, "Statistic did not match");
  ESMC_Test((rootNode.getTotal() == total), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Region count");
  strcpy(failMsg, "Statistic did not match");
  ESMC_Test((rootNode.getCount() == 10), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest
  double mean = total / 10.0;
  strcpy(name, "Region mean");
  snprintf(failMsg, 80, "Mean did not match: expected %f, but got %f", mean, rootNode.getMean());
  ESMC_Test((abs(rootNode.getMean() - mean) < .0000001), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  uint64_t min = 9999999999;
  uint64_t max = 0;
  for (int i = 0; i < 10; i++) {
    uint64_t delta = exits[i] - enters[i];
    if (delta > max) max = delta;
    if (delta < min) min = delta;
  }  
  strcpy(name, "Region min/max");
  strcpy(failMsg, "Statistic did not match");
  ESMC_Test((rootNode.getMin() == min && rootNode.getMax() == max), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  double rstddev = 0.0;
  double rmean = 0.0;
  double rm2 = 0.0;
  double rvariance = 0.0;
  
  for (int i = 0; i < 10; i++) {
    double delta = (exits[i] - enters[i]) - rmean;
    rmean += delta / (i+1);
    rm2 += delta * ((exits[i] - enters[i]) - rmean);
    rvariance = rm2 / (i+1);
  }
  rstddev = sqrt(rvariance);
  
  strcpy(name, "Region standard deviation");
  snprintf(failMsg, 80, "Std dev did not match: expected %f, but got %f", rstddev, rootNode.getStdDev());
  ESMC_Test((abs(rootNode.getStdDev()-rstddev) < .0000001), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
