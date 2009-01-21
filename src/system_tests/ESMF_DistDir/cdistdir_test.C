//-----------------------------------------------------------------------------
// $Id: cdistdir_test.C,v 1.6.2.3 2009/01/21 21:25:25 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// Testing of DistDir object
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
#include <iostream>

#include "ESMC_VM.h"

#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"     
#include <GridUtil/include/ESMC_SparseMsg.h>
#include <GridUtil/include/ESMC_Ptypes.h>
#include <GridUtil/include/ESMC_DistDir.h>
#include <GridUtil/include/ESMC_Exception.h>



using ESMC::UInt;
using ESMC::UChar;
using ESMC::Ex;


extern "C" {

void FTN(cdistdir_test)(ESMCI::VM **vmpp, int*rc) {
  ESMCI::VM *vm = *vmpp;

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  try {

  // *** Test sparse messages
  {


  // Here is the test:  ON each pet, send messages to yourself and
  // every pet with an id less than you.  The particular message that you send
  // is your localPet #, localPet # + 1 times.  Simple enough.
  ESMC::SparseMsg msg(*vm);


  std::vector<UInt> procs(localPet+1, 0);
// send to every <= me
  for (int i = localPet; i >= 0; i--) procs[i] = i;
  
  msg.setPattern(localPet+1, &procs[0]);

  std::vector<UInt> sizes(localPet+1, 0);
  // send my own pid my id number of times
  for (int i = localPet; i >= 0; i--) sizes[i] = ESMC::SparsePack<int>::size()*(localPet+1); 

  msg.setSizes(&sizes[0]);

  
  for (int i = localPet; i >= 0; i--)  {
    ESMC::SparseMsg::buffer &b = *msg.getSendBuffer(i);
    for (int j = 0; j < localPet+1; j++) 
      ESMC::SparsePack<int>(b, localPet);
  }

  if (!msg.filled()) throw Ex() << "P:" << localPet << ", Error!! message not filled" << std::endl;

  msg.communicate();

  for (UInt *p = msg.inPet_begin(); p != msg.inPet_end(); p++) {
    ESMC::SparseMsg::buffer &b = *msg.getRecvBuffer(*p);
    UInt num = b.msg_size() / ESMC::SparsePack<int>::size();
    if (num != *p+1) throw Ex() << "P:" << localPet << ", Error!!! Num=" << num << ", should have num=" << localPet+1 << std::endl;
    for (UInt i= 0; i < num; i++) {
      int val;
      ESMC::SparseUnpack<int>(b, val);
      //std::cout << "Pet:" << localPet << " got " << val << " from pet:" << *p << std::endl;
      if (val != *p) throw Ex() << "P:" << localPet << ", Error!!! val =" << val << " should be " << *p << std::endl;
    }
  }

  if (!msg.empty()) throw Ex() << "Error!! message not emptied" << std::endl;
 
  }

  // *** Test Dist dir
  // Here is the test: Distribute indices 0 to localPet*100 as such:
  // localPet p gets p, p+npet, p+2*npet, ..., p+99*6
  // After setting up the directory, query my all indices and verify that the 
  // directory is not lying.
  {
  const UInt ngid = 100;
  std::vector<UInt> my_gid(ngid);
  std::vector<UInt> my_lid(ngid);

  for (UInt i = 0; i < ngid; i++) {
    my_gid[i] = localPet + i*petCount;
    my_lid[i] = i;
  }
  ESMC::DistDir<UInt> dir(*vm, ngid, &my_gid[0], &my_lid[0]);

  //dir.Print();

  // Now request all possible indices and make sure the directory is correct
  my_gid.resize(ngid*petCount+1);
  my_lid.resize(ngid*petCount+1);
  bool *barr = new bool[petCount*ngid+1]; // std::vector<bool> is a heretic
  std::vector<UInt> oproc(ngid*petCount+1);

  const UInt ngidq = ngid*petCount +1;  // send in a bad one

  for (UInt i = 0; i < ngidq-1; i++) my_gid[i] = i;
  my_gid[ngidq-1] = ngidq; // to big, shouldn't be found

  dir.RemoteGID(ngidq, &my_gid[0], &oproc[0], &my_lid[0], barr);
  
  // Test
  for (UInt i = 0; i < ngidq-1; i++) {
    if (!barr[i]) throw Ex() << "Error!!!! P:" << localPet
                    << " gid=" << my_gid[i] << " not found in directory!" << std::endl;
    if (oproc[i] != (i%petCount)) throw Ex() << "Error!!!! P:" << localPet
                    << " gid=" << my_gid[i] << " from pet " << oproc[i] << std::endl;
    if (my_lid[i] != (i/petCount)) throw Ex() << "Error!!!! P:" << localPet
                    << " gid=" << my_gid[i] << " from pet " << oproc[i] << " bad lid=" << my_lid[i] << std::endl;

  }

  if (barr[ngidq-1] != false)  throw Ex() << "Error!!!! P:" << localPet
                    << " gid=" << my_gid[ngidq-1] << " is found.  It shouldn't have been!" << std::endl;

  std::cout << "P:" << localPet << "Test passed!!" << std::endl;

  delete [] barr;


  } // distdir test

  }
  catch(std::exception &x) {
    std::cout << "Error!!! Exception, P:" << localPet << ", <" << x.what() << ">" << std::endl;
    *rc = ESMF_FAILURE;
    return;
  }

  *rc = ESMF_SUCCESS;

}


} // extern
