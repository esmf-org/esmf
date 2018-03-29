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
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times


#ifndef ESMCI_WebServGRAMClient_H
#define ESMCI_WebServGRAMClient_H

#include "ESMCI_WebServCompSvrMgr.h"
#include <string>

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServGRAMClient
//
// !DESCRIPTION:
//
// The code in this file defines the C++ GRAM Client members and method
// signatures (prototypes).  The companion file ESMCI\_WebServGRAMClient.C
// contains the full code (bodies) for the GRAM Client methods.
//
// This class provides access to a job scheduler using the Globus GRAM
// interface.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServGRAMClient : public ESMCI_WebServCompSvrMgr
  {
  public:

     // constructor and desructor
          ESMCI_WebServGRAMClient(string  hostName,
                             string  scriptDir,
                             string  scriptName);
          ~ESMCI_WebServGRAMClient();

     string  submitJob(int     portNum,
                       string  registrarHost,
                       int     clientId);
     int     cancelJob(string  jobId);
     int     jobStatus(string  jobId);


  private:

     int     loginToServer(string  userName,
                           string  passwd);

     string             theUserName;
     string             thePassword;
  };

} // end namespace

#endif          // ESMCI_WebServGRAMClient_H
