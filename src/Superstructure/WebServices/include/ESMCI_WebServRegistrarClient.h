// $Id: ESMCI_WebServRegistrarClient.h,v 1.3 2011/01/05 20:05:48 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_WebServRegistrarClient_H
#define ESMCI_WebServRegistrarClient_H

#include <stdlib.h>

#include "ESMCI_WebServClientSocket.h"

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServRegistrarClient
//
// !DESCRIPTION:
//
// The code in this file defines the C++ RegistrarClient members and method
// signatures (prototypes).  The companion file ESMCI\_WebServRegistrarClient.C
// contains the full code (bodies) for the RegistrarClient methods.
//
// This class provides the capability to connect and communicate with a
// Registrar service.
//
//EOPI
//-------------------------------------------------------------------------

// The default port number for the Registrar
#define REGISTRAR_PORT	45002

namespace ESMCI
{

  class ESMCI_WebServRegistrarClient 
  {
  public:

     // constructor and destructor
	  ESMCI_WebServRegistrarClient(const char*  host,
                                  int          port);
	  ~ESMCI_WebServRegistrarClient();

     // methods to setup the connection parameters
	  void setHost(const char*  host);
	  void setPort(int  port);

     // low-level methods to communicate across the socket
	  int  sendRequest(int    request,
                      int    length = 0,
                      void*  data = NULL);

	  int  getResponse(int    request,
                      int&   length,
                      void*  data);

	  int  connect();
	  void disconnect();

     // methods to send client requests to the server
	  int  registerComp(char*  name,
                       char*  desc,
                       char*  hostName,
                       char*  portNum,
                       void*  retValue);

	  int  unregisterComp(char*  name,
                         char*  hostName,
                         char*  portNum,
                         void*  retValue);

  private:

	  char*			theHost;			// the name of the machine that hosts the svc
	  int				thePort;			// the port number for the service
	  char			theMsg[8192];	// the message buffer

	  ESMCI_WebServClientSocket		
						theSocket;		// the socket connection to the service
  };

} // end namespace

#endif 	// ESMCI_WebServRegistrarClient_H
