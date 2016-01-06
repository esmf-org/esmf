// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times


#ifndef ESMCI_WebServForkClient_H
#define ESMCI_WebServForkClient_H

#include "ESMCI_WebServCompSvrMgr.h"
#include <string>

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServForkClient
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Fork Client members and method
// signatures (prototypes).  The companion file ESMCI\_WebServForkClient.C
// contains the full code (bodies) for the Fork Client methods.
//
// This class provides access to a job scheduler using the Fork
// interface.
// 
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServForkClient : public ESMCI_WebServCompSvrMgr
  {
  public:

     // constructor and desructor
	  ESMCI_WebServForkClient(string  hostName,
                             string  scriptDir,
                             string  scriptName);
	  ~ESMCI_WebServForkClient();

     string  submitJob(int     portNum,
                       string  registrarHost,
                       int     clientId);
     int     cancelJob(string  jobId);
     int     jobStatus(string  jobId);


  private:

     int		extractPid(string  jobId);
  };

} // end namespace

#endif 	// ESMCI_WebServForkClient_H
