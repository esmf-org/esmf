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
#include <iostream>
#include <cmath>

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

static int eqltol(double a, double b) {
  //printf("abs(a-b): a=%f, b=%f,  a-b=%f, abs(a-b)=%f\n", a, b, a-b, std::abs(a-b));
  if (std::abs(a - b) < .00000001) {    
    return 1;
  }
  else {
    return 0;
  }
}

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  int userRc;
  
  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Tolerance test 1");
  strcpy(failMsg, "Tolerance off");
  ESMC_Test(!eqltol(1.01, 1.02), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Tolerance test 2");
  strcpy(failMsg, "Tolerance off");
  ESMC_Test(eqltol(5.0, 5.00000000001), name, failMsg, &result, __FILE__, __LINE__, 0);
  
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
  ESMC_Test(eqltol(rootNode.getMean(), mean), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test(eqltol(rootNode.getStdDev(), rstddev), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMCI::RegionNode nodeA(0);
  ESMCI::RegionNode nodeB(0);

  uint64_t ins[] = {0, 10, 20, 0, 10, 30, 40};
  uint64_t outs[] =  {5, 12, 30, 4, 22, 40, 48}; 

  for (int i=0; i<3; i++) {
    nodeA.entered(ins[i]); nodeA.exited(outs[i]);
  }

  for (int i=3; i<7; i++) {
    nodeB.entered(ins[i]); nodeB.exited(outs[i]);
  }
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Region mean");
  snprintf(failMsg, 80, "Region mean: expected %f, but got %f", ((5+2+10)/3.0), nodeA.getMean());
  ESMC_Test( eqltol(((5+2+10)/3.0), nodeA.getMean()), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Region mean");
  snprintf(failMsg, 80, "Region mean: expected %f, but got %f", ((4+12+10+8)/4.0), nodeB.getMean());
  ESMC_Test( eqltol(((4+12+10+8)/4.0), nodeB.getMean()), name, failMsg, &result, __FILE__, __LINE__, 0);
     
  //merge B into A, combining statistics
  nodeA.merge(nodeB);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Merge total");
  snprintf(failMsg, 80, "Merge total: expected %f, but got %f", (5+2+10+4+12+10+8), nodeA.getTotal());
  ESMC_Test((5+2+10+4+12+10+8)==nodeA.getTotal(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Merge count");
  snprintf(failMsg, 80, "Merge count: expected %f, but got %f", 7, nodeA.getCount());
  ESMC_Test(7==nodeA.getCount(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Merge min");
  snprintf(failMsg, 80, "Merge min: expected %f, but got %f", 2, nodeA.getMin());
  ESMC_Test(2==nodeA.getMin(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Merge max");
  snprintf(failMsg, 80, "Merge max: expected %f, but got %f", 12, nodeA.getMax());
  ESMC_Test(12==nodeA.getMax(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Merge mean");
  snprintf(failMsg, 80, "Merge mean: expected %f, but got %f", (5+2+10+4+12+10+8)/7.0, nodeA.getMean());
  ESMC_Test((5+2+10+4+12+10+8)/7.0==nodeA.getMean(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_IGNORE_UTest
  
  rstddev = 0.0;
  rmean = 0.0;
  rm2 = 0.0;
  rvariance = 0.0;
  
  for (int i = 0; i < 7; i++) {
    double delta = (outs[i] - ins[i]) - rmean;
    rmean += delta / (i+1);
    rm2 += delta * ((outs[i] - ins[i]) - rmean);
    rvariance = rm2 / (i+1);
  }
  rstddev = sqrt(rvariance);

  //strcpy(name, "Merge stddev");
  //snprintf(failMsg, 80, "Merge stddev: expected %f, but got %f", rstddev, nodeA.getStdDev());
  //ESMC_Test(eqltol(rstddev, nodeA.getStdDev()), name, failMsg, &result, __FILE__, __LINE__, 0);

  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
