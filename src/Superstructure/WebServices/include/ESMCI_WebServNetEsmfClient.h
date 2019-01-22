// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_WebServNetEsmfClient_H
#define ESMCI_WebServNetEsmfClient_H

#include <stdlib.h>
#include <string>
#include <vector>

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_WebServNetEsmf.h"

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServNetEsmfClient
//
// !DESCRIPTION:
//
// The code in this file defines the C++ NetEsmfClient members and method
// signatures (prototypes).  The companion file ESMCI\_WebServNetEsmfClient.C
// contains the full code (bodies) for the NetEsmfClient methods.
//
// This class provides the capability to connect and communicate with a
// ESMF Component (Grid or Coupler) service.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServNetEsmfClient
  {
  public:

          // constructor and destructor
          ESMCI_WebServNetEsmfClient(const char*  host,
                                int          port);
          ~ESMCI_WebServNetEsmfClient();

     int    getStateValue(const char*  stateStr);
     char*  getStateStr(int  id);

  protected:

     // methods to setup the connection parameters
          void setHost(const char*  host);
          void setPort(int  port);

     // low-level communications methods
          int  sendRequest(int    request,
                      int    length = 0,
                      void*  data = NULL);

          int  sendData(int    length,
                   void*  data);

          int  sendString(const char*  data);

          int  getResponse(int    request,
                      int&   length,
                      void*  data);

          int  connect();
          void disconnect();


  private:

          char*                         theHost;                        // the name of the machine that hosts the svc
          int                           thePort;                        // the port number for the service
          char                  theMsg[8192];   // the message buffer

          ESMCI_WebServClientSocket     
                                                theSocket;              // the socket connection to the service
  };

}  // end namespace


#endif          // ESMCI_WebServNetEsmfClient_H
