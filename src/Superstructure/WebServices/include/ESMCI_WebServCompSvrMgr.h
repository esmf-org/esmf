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


#ifndef ESMCI_WebServCompSvrMgr_H
#define ESMCI_WebServCompSvrMgr_H

#include <string>
#include "ESMCI_WebServClientInfo.h"

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServCompSvrMgr
//
// !DESCRIPTION:
//
// The code in this file defines the C++ GRAM Client members and method
// signatures (prototypes).  The companion file ESMCI\_WebServCompSvrMgr.C
// contains the full code (bodies) for the GRAM Client methods.
//
// This class provides access to a job scheduler using the Globus GRAM
// interface.
// 
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServCompSvrMgr 
  {
  public:

     // constructor and desructor
	  ESMCI_WebServCompSvrMgr(string  hostName, 
                             string  scriptDir,
                             string  scriptName);
	  ~ESMCI_WebServCompSvrMgr();

     virtual string  submitJob(int     portNum,
                               string  registrarHost,
                               int     clientId) = 0;
     virtual int     cancelJob(string  jobId) = 0;
     virtual int     jobStatus(string  jobId) = 0;


  protected:

     string		theHostName;
     string		theScriptDir;
     string		theScriptName;
  };

} // end namespace

#endif 	// ESMCI_WebServCompSvrMgr_H
