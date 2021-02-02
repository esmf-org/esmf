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
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_WebServProcCtrlClient_H
#define ESMCI_WebServProcCtrlClient_H

#include <stdlib.h>
#include <string>
#include <vector>

//#include "ESMCI_WebServServerSocket.h"
//#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_WebServNetEsmf.h"
#include "ESMCI_WebServNetEsmfClient.h"
#include "ESMCI_WebServDataDesc.h"
#include "ESMCI_WebServDataContent.h"

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServProcCtrlClient
//
// !DESCRIPTION:
//
// The code in this file defines the C++ ProcCtrlClient members and method
// signatures (prototypes).  The companion file ESMCI\_WebServProcCtrlClient.C
// contains the full code (bodies) for the ProcCtrlClient methods.
//
// This class provides the capability to connect and communicate with a
// ESMF Component service implemented with the ESMCI_WebServComponentSvr class.
// This class is intended to be used only by a PassThruSvr service which
// is the intermediary between the client (web service) and the component
// service.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServProcCtrlClient : public ESMCI_WebServNetEsmfClient
  {
  public:

     // constructor and destructor
          ESMCI_WebServProcCtrlClient(const char*  host,
                                 int          port,
                                 const char*  userName,
                                 const char*  password);
          ~ESMCI_WebServProcCtrlClient();

     // access methods
     int  getClientId() { return theClientId; }

     // methods to send client requests to the server
     int                                                                state();
     int                                                                init();
     int                                                                run();
     int                                                                timestep(int  numTimesteps);
     int                                                                final();
     ESMCI_WebServDataDesc*      dataDesc();
     ESMCI_WebServDataContent*   outputData(double  timestamp);
     int                                                                end();
     int                                                                killServer();


  private:

     int   newClient();
          void  setClientId(int  clientId);

          char    theMsg[8192];              // the message buffer
     char    theUserName[256];  // the login name for the user on the server
     char    thePassword[256];  // the password for the user on the server
          int       theClientId;             // the id of the client on the PassThruSvr
  };

} // end namespace

#endif          // ESMCI_WebServProcCtrlClient_H
