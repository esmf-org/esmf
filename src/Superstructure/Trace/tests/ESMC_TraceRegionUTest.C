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

static int matches(ESMCI::RegionNode *rn1, ESMCI::RegionNode *rn2) {
  if (rn1 != NULL && rn2 != NULL &&
    rn1->getTotal() == rn2->getTotal() &&
    rn1->getCount() == rn2->getCount() &&
    rn1->getMin() == rn2->getMin() &&
    rn1->getMax() == rn2->getMax() &&
    rn1->getName() == rn2->getName() &&
    rn1->getStdDev() == rn2->getStdDev() &&
      rn1->getMean() == rn2->getMean()) {
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
  
  ESMCI::RegionNode rootNode;
  
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
  ESMCI::RegionNode *nodeParent = new ESMCI::RegionNode();
  ESMCI::RegionNode *nodeChild1;
  ESMCI::RegionNode *nodeChild2;
  ESMCI::RegionNode *nodeChild2a;

  nodeParent->entered(10);
  nodeChild1 = nodeParent->addChild();
  nodeChild1->setName("child1");
  nodeChild1->entered(20);  nodeChild1->exited(27);
  nodeChild1->entered(30);  nodeChild1->exited(45);
  nodeChild2 = nodeParent->addChild();
  nodeChild2->setName("child2");
  nodeChild2->entered(55);  nodeChild2->exited(67);
  nodeChild2->entered(88);  nodeChild2->exited(105);
  nodeChild2->entered(109); nodeChild2->exited(127);
  nodeChild2a = nodeChild2->addChild();
  nodeChild2a->setName("child2a");
  nodeChild2a->entered(200); nodeChild2a->exited(305);
  nodeParent->exited(333);
  
  ESMCI::RegionNode *cloneParent = new ESMCI::RegionNode(NULL, nodeParent);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Clone region node");
  snprintf(failMsg, 80, "Clone total time does not match. Expected %d but got %d\n", nodeParent->getTotal(), cloneParent->getTotal());
  ESMC_Test(nodeParent->getTotal() == cloneParent->getTotal(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Clone count time does not match. Expected %d but got %d\n", nodeParent->getCount(), cloneParent->getCount());
  ESMC_Test(nodeParent->getCount() == cloneParent->getCount(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Clone count min/max do not match.");
  ESMC_Test(nodeParent->getMin() == cloneParent->getMin() && nodeParent->getMax()==cloneParent->getMax(),
	    name, failMsg, &result, __FILE__, __LINE__, 0);

  ESMCI::RegionNode *cloneChild1, *cloneChild2, *cloneChild2a;
  cloneChild1 = cloneParent->getChild("child1");
  cloneChild2 = cloneParent->getChild("child2");

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Clone child1 not created");
  ESMC_Test(cloneChild1 != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Clone child1 did not match");
  ESMC_Test(matches(cloneChild1, nodeChild1), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Clone child2 not created");
  ESMC_Test(cloneChild2 != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Clone child2 did not match");
  ESMC_Test(matches(cloneChild2, nodeChild2), name, failMsg, &result, __FILE__, __LINE__, 0);

  cloneChild2a = cloneChild2->getChild("child2a");

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Clone child2a not created");
  ESMC_Test(cloneChild2a != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Clone child2a did not match");
  ESMC_Test(matches(cloneChild2a, nodeChild2a), name, failMsg, &result, __FILE__, __LINE__, 0);
    
  delete nodeParent;
  delete cloneParent;  
  cloneChild1 = NULL;
  cloneChild2 = NULL;
  cloneChild2a = NULL;
  
  //----------------------------------------------------------------------------
  ESMCI::RegionNode nodeA;
  ESMCI::RegionNode nodeB;

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
   
  //simulates PET0
  ESMCI::RegionNode *nodeESM1 = new ESMCI::RegionNode();
  ESMCI::RegionNode *nodeATM1;
  nodeESM1->setName("ESM");
  nodeESM1->entered(0);
  nodeATM1 = nodeESM1->addChild();
  nodeATM1->setName("ATM");
  nodeATM1->entered(20);  nodeATM1->exited(30);
  nodeATM1->entered(40);  nodeATM1->exited(50);
  nodeATM1->entered(60);  nodeATM1->exited(70);
  nodeESM1->exited(100);

  //simulates PET1
  ESMCI::RegionNode *nodeESM2 = new ESMCI::RegionNode();
  ESMCI::RegionNode *nodeATM2;
  nodeESM2->setName("ESM");
  nodeESM2->entered(0);
  nodeATM2 = nodeESM2->addChild();
  nodeATM2->setName("ATM");
  nodeATM2->entered(20);  nodeATM2->exited(30);
  nodeATM2->entered(40);  nodeATM2->exited(50);
  nodeATM2->entered(60);  nodeATM2->exited(70);
  nodeESM2->exited(100);

  //simulates PET2
  ESMCI::RegionNode *nodeESM3 = new ESMCI::RegionNode();
  ESMCI::RegionNode *nodeOCN3, *nodeOCNSUB3;
  nodeESM3->setName("ESM");
  nodeESM3->entered(0);
  nodeOCN3 = nodeESM3->addChild();
  nodeOCN3->setName("OCN");
  nodeOCN3->entered(20);
  nodeOCNSUB3 = nodeOCN3->addChild();
  nodeOCNSUB3->setName("OCNSUB");
  nodeOCNSUB3->entered(22); nodeOCNSUB3->exited(27);
  nodeOCN3->exited(30);
  nodeOCN3->entered(40);  nodeOCN3->exited(50);
  nodeOCN3->entered(60);  nodeOCN3->exited(70);
  nodeOCN3->entered(80);  nodeOCN3->exited(90);
  nodeESM3->exited(100);

  //merge statistics to nodeESM1, which will
  //represent the global statics across all PETs
  nodeESM1->merge(*nodeESM2);
  nodeESM1->merge(*nodeESM3);

  strcpy(name, "Merge tree");

  //----------------------------------------------------------------------------
  //NEX_UTest
  ESMCI::RegionNode *rnATM = nodeESM1->getChild("ATM");
  snprintf(failMsg, 80, "Merge child exists: ATM");
  ESMC_Test(rnATM != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  ESMCI::RegionNode *rnOCN = nodeESM1->getChild("OCN");
  snprintf(failMsg, 80, "Merge child exists: OCN");
  ESMC_Test(rnOCN != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  ESMCI::RegionNode *rnOCNSUB = rnOCN->getChild("OCNSUB");
  snprintf(failMsg, 80, "Merge child exists: OCNSUB");
  ESMC_Test(rnOCNSUB != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Merged ESM count");
  ESMC_Test(nodeESM1->getCount()==3, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Merged ESM total");
  ESMC_Test(nodeESM1->getTotal()==300, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Merged ATM count");
  ESMC_Test(rnATM->getCount()==6, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Merged OCN count");
  ESMC_Test(rnOCN->getCount()==4, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Merged OCNSUB total");
  ESMC_Test(rnOCNSUB->getTotal()==5, name, failMsg, &result, __FILE__, __LINE__, 0);
    
  delete nodeESM1, nodeESM2, nodeESM3;
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
