

// $Id$
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
#include "ESMCI_VM.h"
#include "ESMCI_RegionNode.h"
#include "ESMCI_RegionSummary.h"

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

static int treeMatch(ESMCI::RegionNode *rn1, ESMCI::RegionNode *rn2) {
  if (matches(rn1, rn2) == 0) {
    std::cout << "statistics match failed: " << rn1->getName() << " : " << rn2->getName();
    return 0;
  }
  if (rn1->getGlobalId() != rn2->getGlobalId()) {
    std::cout << "global id match failed: " << rn1->getName() << " : " << rn2->getName();
    return 0;
  }
  if (rn1->getParentGlobalId() != rn2->getParentGlobalId()) {
    std::cout << "parent global id match failed: " << rn1->getName() << "(" << rn1->getParentGlobalId() << ")";
    std::cout << " : " << rn2->getName() << "(" << rn2->getParentGlobalId() << ")\n";
    std::cout << "left parent null == " << (rn1->getParent() == NULL) << "\n";
    std::cout << "right parent null == " << (rn2->getParent() == NULL) << "\n";
    return 0;
  }
  if (rn1->getLocalId() != rn2->getLocalId()) return 0;
  if (rn1->getChildren().size() != rn2->getChildren().size()) {
    //std::cout << "match child size failed: " << rn1->getChildren().size() << " : " << rn2->getChildren().size();
    return 0;
  }
  for (unsigned i=0; i < rn1->getChildren().size(); i++) {
    if (treeMatch(rn1->getChildren().at(i), rn2->getChildren().at(i)) == 0) return 0;
  }
  return 1;
}


int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  
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
  nodeChild1 = nodeParent->addChild("child1");
  nodeChild1->entered(20);  nodeChild1->exited(27);
  nodeChild1->entered(30);  nodeChild1->exited(45);
  nodeChild2 = nodeParent->addChild("child2");
  nodeChild2->entered(55);  nodeChild2->exited(67);
  nodeChild2->entered(88);  nodeChild2->exited(105);
  nodeChild2->entered(109); nodeChild2->exited(127);
  nodeChild2a = nodeChild2->addChild("child2a");
  nodeChild2a->entered(200); nodeChild2a->exited(305);
  nodeParent->exited(333);
  
  ESMCI::RegionNode *cloneParent = new ESMCI::RegionNode(NULL, nodeParent);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Clone region node");
  snprintf(failMsg, 80, "Clone total time does not match. Expected %lu but got %lu\n", nodeParent->getTotal(), cloneParent->getTotal());
  ESMC_Test(nodeParent->getTotal() == cloneParent->getTotal(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Clone count time does not match. Expected %lu but got %lu\n", nodeParent->getCount(), cloneParent->getCount());
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
  snprintf(failMsg, 80, "Merge total: expected %d, but got %lu", (5+2+10+4+12+10+8), nodeA.getTotal());
  ESMC_Test((5+2+10+4+12+10+8)==nodeA.getTotal(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Merge count");
  snprintf(failMsg, 80, "Merge count: expected %d, but got %lu", 7, nodeA.getCount());
  ESMC_Test(7==nodeA.getCount(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Merge min");
  snprintf(failMsg, 80, "Merge min: expected %d, but got %lu", 2, nodeA.getMin());
  ESMC_Test(2==nodeA.getMin(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Merge max");
  snprintf(failMsg, 80, "Merge max: expected %d, but got %lu", 12, nodeA.getMax());
  ESMC_Test(12==nodeA.getMax(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Merge mean");
  snprintf(failMsg, 80, "Merge mean: expected %f, but got %f", (5+2+10+4+12+10+8)/7.0, nodeA.getMean());
  ESMC_Test((5+2+10+4+12+10+8)/7.0==nodeA.getMean(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
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
  nodeATM1 = nodeESM1->addChild("ATM");
  nodeATM1->entered(20);  nodeATM1->exited(30);
  nodeATM1->entered(40);  nodeATM1->exited(50);
  nodeATM1->entered(60);  nodeATM1->exited(70);
  nodeESM1->exited(100);

  //simulates PET1
  ESMCI::RegionNode *nodeESM2 = new ESMCI::RegionNode();
  ESMCI::RegionNode *nodeATM2;
  nodeESM2->setName("ESM");
  nodeESM2->entered(0);
  nodeATM2 = nodeESM2->addChild("ATM");
  nodeATM2->entered(20);  nodeATM2->exited(30);
  nodeATM2->entered(40);  nodeATM2->exited(50);
  nodeATM2->entered(60);  nodeATM2->exited(70);
  nodeESM2->exited(100);

  //simulates PET2
  ESMCI::RegionNode *nodeESM3 = new ESMCI::RegionNode();
  ESMCI::RegionNode *nodeOCN3, *nodeOCNSUB3;
  nodeESM3->setName("ESM");
  nodeESM3->entered(0);
  nodeOCN3 = nodeESM3->addChild("OCN");
  nodeOCN3->entered(20);
  nodeOCNSUB3 = nodeOCN3->addChild("OCNSUB");
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
  // Test region summarization
   
  //simulates PET0
  nodeESM1 = new ESMCI::RegionNode();
  nodeESM1->setName("ESM");
  nodeESM1->entered(0);
  nodeATM1 = nodeESM1->addChild("ATM");
  nodeATM1->entered(20);  nodeATM1->exited(30);
  nodeATM1->entered(40);  nodeATM1->exited(50);
  nodeATM1->entered(60);  nodeATM1->exited(70);  //total == 30
  ESMCI::RegionNode *nodeMED1 = nodeESM1->addChild("MED");
  nodeMED1->entered(71);  nodeMED1->exited(73);
  nodeMED1->entered(75);  nodeMED1->exited(78);
  nodeESM1->exited(99);

  //simulates PET1
  nodeESM2 = new ESMCI::RegionNode();
  nodeESM2->setName("ESM");
  nodeESM2->entered(0);
  nodeATM2 = nodeESM2->addChild("ATM");
  nodeATM2->entered(20);  nodeATM2->exited(22);
  nodeATM2->entered(40);  nodeATM2->exited(42);
  nodeATM2->entered(60);  nodeATM2->exited(62);  //total == 6
  ESMCI::RegionNode *nodeMED2 = nodeESM2->addChild("MED");
  nodeMED2->entered(71);  nodeMED2->exited(75);
  nodeESM2->exited(100);

  //simulates PET2
  nodeESM3 = new ESMCI::RegionNode();
  nodeESM3->setName("ESM");
  nodeESM3->entered(0);
  nodeOCN3 = nodeESM3->addChild("OCN");
  nodeOCN3->entered(20);
  nodeOCNSUB3 = nodeOCN3->addChild("OCNSUB");
  nodeOCNSUB3->entered(22); nodeOCNSUB3->exited(27);
  nodeOCN3->exited(30);
  nodeOCN3->entered(40);  nodeOCN3->exited(50);
  nodeOCN3->entered(60);  nodeOCN3->exited(70);
  nodeOCN3->entered(80);  nodeOCN3->exited(90);
  nodeESM3->exited(101);

  //summarize statistics
  ESMCI::RegionSummary *regSum = new ESMCI::RegionSummary(NULL);
  regSum->merge(*nodeESM1, 0);
  regSum->merge(*nodeESM2, 1);
  regSum->merge(*nodeESM3, 2);

  strcpy(name, "Region summary tree");

  //----------------------------------------------------------------------------
  //NEX_UTest
  ESMCI::RegionSummary *rsATM = regSum->getChild("ATM");
  snprintf(failMsg, 80, "Summary child exists: ATM");
  ESMC_Test(rsATM != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  ESMCI::RegionSummary *rsOCN = regSum->getChild("OCN");
  snprintf(failMsg, 80, "Summary child exists: OCN");
  ESMC_Test(rsOCN != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  ESMCI::RegionSummary *rsMED = regSum->getChild("MED");
  snprintf(failMsg, 80, "Summary child exists: MED");
  ESMC_Test(rsMED != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  ESMCI::RegionSummary *rsOCNSUB = rsOCN->getChild("OCNSUB");
  snprintf(failMsg, 80, "Summary child exists: OCNSUB");
  ESMC_Test(rsOCNSUB != NULL, name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ESM pet count");
  ESMC_Test(regSum->getPetCount()==3, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ESM count each");
  ESMC_Test(regSum->getCountEach()==1, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ESM count match");
  ESMC_Test(regSum->getCountsMatch(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ESM total sum");
  ESMC_Test(regSum->getTotalSum()==300, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ESM total min");
  ESMC_Test(regSum->getTotalMin()==99, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ESM total min pet");
  ESMC_Test(regSum->getTotalMinPet()==0, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ESM total max");
  ESMC_Test(regSum->getTotalMax()==101, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ESM total max pet");
  ESMC_Test(regSum->getTotalMaxPet()==2, name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ESM total mean");
  ESMC_Test(regSum->getTotalMean()==100, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summarized MED counts should NOT match");
  ESMC_Test(rsMED->getCountsMatch()==false, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summarized ATM count each: expected 3 but got %lu", rsATM->getCountEach());
  ESMC_Test(rsATM->getCountEach()==3, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summarized ATM counts should match");
  ESMC_Test(rsATM->getCountsMatch(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ATM total min: expected 6 but got %lu", rsATM->getTotalMin());
  ESMC_Test(rsATM->getTotalMin()==6, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ATM total min pet: expected 1 but got %d", rsATM->getTotalMinPet());
  ESMC_Test(rsATM->getTotalMinPet()==1, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ATM total max: expected 30 but got %lu", rsATM->getTotalMax());
  ESMC_Test(rsATM->getTotalMax()==30, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ATM total max pet: expected 0 but got %d", rsATM->getTotalMaxPet());
  ESMC_Test(rsATM->getTotalMaxPet()==0, name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ATM total mean: expected %f but got %f", ((30+6)/2.0), rsATM->getTotalMean());
  ESMC_Test(rsATM->getTotalMean()==((30+6)/2.0), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary ATM pet count: expected 2 but got %lu", rsATM->getPetCount());
  ESMC_Test(rsATM->getPetCount()==2, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summarized OCN count each: expected 4 but got %lu", rsOCN->getCountEach());
  ESMC_Test(rsOCN->getCountEach()==4, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summary OCN pet count: expected 1 but got %lu", rsOCN->getPetCount());
  ESMC_Test(rsOCN->getPetCount()==1, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Summarized OCNSUB total sum");
  ESMC_Test(rsOCNSUB->getTotalSum()==5, name, failMsg, &result, __FILE__, __LINE__, 0);
    

  delete nodeESM1, nodeESM2, nodeESM3, regSum;

   
  //----------------------------------------------------------------------------
  strcpy(name, "Serialize/deserialize single region node");
 
  ESMCI::RegionNode *ser = new ESMCI::RegionNode(NULL, 989, true);
  ser->setName("serialize_this_region_1234");
  ser->entered(0);   ser->exited(100);
  ser->entered(200); ser->exited(298);
  ser->entered(500); ser->exited(523);

  size_t bufsize = 0;
  char *sbuf = ser->serialize(&bufsize);
  
  //printf("offset after serializeLocal: %lu\n", offset);
  
  ESMCI::RegionNode *des = new ESMCI::RegionNode(sbuf, bufsize);
  free(sbuf);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize global id");
  ESMC_Test(ser->getGlobalId()==des->getGlobalId(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize local id");
  ESMC_Test(ser->getLocalId()==des->getLocalId(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize total");
  ESMC_Test(ser->getTotal()==des->getTotal(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize self time");
  ESMC_Test(ser->getSelfTime()==des->getSelfTime(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize count");
  ESMC_Test(ser->getCount()==des->getCount(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize min");
  ESMC_Test(ser->getMin()==des->getMin(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize max");
  ESMC_Test(ser->getMax()==des->getMax(), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize mean");
  ESMC_Test(ser->getMean()==des->getMean(), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize stddev");
  ESMC_Test(ser->getStdDev()==des->getStdDev(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize isUserRegion");
  ESMC_Test(ser->isUserRegion()==des->isUserRegion(), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialize name: %s, %s", ser->getName().c_str(), des->getName().c_str());
  ESMC_Test(ser->getName()==des->getName(), name, failMsg, &result, __FILE__, __LINE__, 0);

  delete ser, des;
  
  //----------------------------------------------------------------------------
  strcpy(name, "Serialize/deserialize region tree");  

  ESMCI::RegionNode *serParent = new ESMCI::RegionNode(NULL, 800, true);
  ESMCI::RegionNode *serChild1;
  ESMCI::RegionNode *serChild1a;
  ESMCI::RegionNode *serChild2;
  ESMCI::RegionNode *serChild2a;
  ESMCI::RegionNode *serChild2a1;
  ESMCI::RegionNode *serChild2a2;

  serParent->setName("serParent");
  serParent->entered(10);
  serChild1 = serParent->addChild("child1");
  serChild1->entered(20);   serChild1->exited(27);
  serChild1->entered(30);   serChild1->exited(45);
  serChild2 = serParent->addChild("child2");
  serChild2->entered(55);   serChild2->exited(67);
  serChild2->entered(88);   serChild2->exited(105);
  serChild2->entered(109);  serChild2->exited(127);
  serChild2a = serChild2->addChild("child2a");
  serChild2a->entered(200); 
  serChild2a1 = serChild2a->addChild("child2a1");
  serChild2a1->entered(201); serChild2a1->exited(204);
  serChild2a2 = serChild2a->addChild("child2a2");
  serChild2a2->entered(205); serChild2a1->exited(210);
  serChild2a->exited(305);
  serChild1->entered(310);
  serChild1a = serParent->addChild("child1a");
  serChild1a->entered(315); serChild1a->exited(413);
  serChild1->exited(900);
  serParent->exited(1000);

  size_t treeBufSize = 0;
  char *treeBuffer = serParent->serialize(&treeBufSize);

  ESMCI::RegionNode *desParent = new ESMCI::RegionNode(treeBuffer, treeBufSize);
      
  free(treeBuffer);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialized root node does not match");
  ESMC_Test(matches(desParent, serParent), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialized tree does not match");
  ESMC_Test(treeMatch(desParent, serParent), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  strcpy(name, "Merge deserialized tree");

  //merge the two back together, essentially a merge with itself
  serParent->merge(*desParent);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Unexpected deserialized merge: count");
  ESMC_Test(serParent->getCount() == 2, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Unexpected deserialized merge: total");
  ESMC_Test(serParent->getTotal() == (1000-10)*2, name, failMsg, &result, __FILE__, __LINE__, 0);

  delete serParent, desParent;


  //----------------------------------------------------------------------------
  // test sending serialized region tree over MPI
  strcpy(name, "Gather serialized tree on root PET");
  
  int localrc;
  ESMCI::VM *globalvm = ESMCI::VM::getGlobal(&localrc);
  int localPet = globalvm->getLocalPet();
  int petCount = globalvm->getPetCount();
  
  treeBufSize = 0;
  
  serParent = new ESMCI::RegionNode(NULL, 800, true);
  serParent->setName("serParent");
  serParent->entered(10);
  serChild1 = serParent->addChild("child1");
  serChild1->entered(20);   serChild1->exited(27);
  serChild1->entered(30);   serChild1->exited(45);
  serChild2 = serParent->addChild("child2");
  serChild2->entered(55);   serChild2->exited(67);
  serChild2->entered(88);   serChild2->exited(105);
  serChild2->entered(109);  serChild2->exited(127);
  serChild2a = serChild2->addChild("child2a");
  serChild2a->entered(200); 
  serChild2a1 = serChild2a->addChild("child2a1");
  serChild2a1->entered(201); serChild2a1->exited(204);
  serChild2a2 = serChild2a->addChild("child2a2");
  serChild2a2->entered(205); serChild2a1->exited(210);
  serChild2a->exited(305);
  serChild1->entered(310);
  serChild1a = serParent->addChild("child1a");
  serChild1a->entered(315); serChild1a->exited(413);
  serChild1->exited(900);
  serParent->exited(1000);

  int matched[petCount-1];
  
  if (localPet > 0) {
    
    treeBuffer = serParent->serialize(&treeBufSize);

    //std::cout << "sent buffer size: " << treeBufSize;
    
    //send size of buffer
    globalvm->send((void *) &treeBufSize, sizeof(treeBufSize), 0);
    //send buffer itself
    globalvm->send((void *) treeBuffer, treeBufSize, 0); 

    free(treeBuffer);
  }
  else if (localPet == 0) {

    for (int p=1; p<petCount; p++) {

      treeBufSize = 0;
      globalvm->recv((void *) &treeBufSize, sizeof(treeBufSize), p);
      //std::cout << "received treeBufSize = " << treeBufSize << " from pet " << p << "\n";

      treeBuffer = (char *) malloc(treeBufSize);
      memset(treeBuffer, 0, treeBufSize);
    
      globalvm->recv(treeBuffer, treeBufSize, p);

      desParent = new ESMCI::RegionNode(treeBuffer, treeBufSize);
      
      matched[p-1] = treeMatch(serParent, desParent);

      delete desParent;
      free(treeBuffer);
    }
  }

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialized tree 1 received on root PET does not match");
  ESMC_Test((petCount == 1 || localPet > 0 || matched[0]), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialized tree 2 received on root PET does not match");
  ESMC_Test((petCount == 1 || localPet > 0 || matched[1]), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
  snprintf(failMsg, 80, "Deserialized tree 3 received on root PET does not match");
  ESMC_Test((petCount == 1 || localPet > 0 || matched[2]), name, failMsg, &result, __FILE__, __LINE__, 0);

  delete serParent;
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
