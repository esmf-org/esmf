// $Id: ESMCI_WebServPassThruSvr.h,v 1.4 2011/01/05 20:05:48 svasquez Exp $
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

#ifndef ESMCI_WebServPassThruSvr_H
#define ESMCI_WebServPassThruSvr_H

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_WebServNetEsmf.h"
#include "ESMCI_WebServClientInfo.h"
#include "ESMCI_WebServCAMOutputFile.h"
#include <map>

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServPassThruSvr
//
// !DESCRIPTION:
//
// The code in this file defines the C++ PassThruSvr members and method
// signatures (prototypes).  The companion file ESMCI\_WebServPassThruSvr.C
// contains the full code (bodies) for the PassThruSvr methods.
//
// This class provides the basic functionality setting up Process Controller
// Service, which essentially just passes requests from its client on to an
// ESMF Component Service (implemented with the ESMCI_ComponentSvr class).
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServPassThruSvr 
  {
  public:

     // constructor and destructor
	  ESMCI_WebServPassThruSvr(int     port, 
                              string  camDir);
	  ~ESMCI_WebServPassThruSvr();

     // port number access methods
	  int  getPort()		{ return thePort; }
	  void setPort(int  port);

     // method to setup socket service loop
	  int  requestLoop();


  private:

     // methods to handle incoming requests
	  int  getNextRequest();
	  int  serviceRequest(int  request);

	  int   getRequestId(const char  request[]);
	  char* getRequestFromId(int  id);

     // process request methods
	  int   processNew();
	  int   processInit();
	  int   processRun();
	  int   processFinal();
	  int   processState();
	  int   processFiles();
	  int   processGetData();
	  int   processEnd();
	  int   processPing();

     // internal data access methods
	  int  getNextClientId();

	  int				thePort;			// the port number of the socket service

	  ESMCI_WebServServerSocket	theSocket;		// the server socket

     // list of client sessions and counter to keep track of the next
     // available client session id
	  map<int, ESMCI_WebServClientInfo*>	theClients;
	  int												theNextClientId;

     //***
     // These data members are specific to the CCSM/CAM Component Service...
     // we should probably abstract this capability
     //***
	  string				theCAMDir;			// the directory where the CAM output
                                       // files can be found
	  ESMCI_WebServCAMOutputFile*	
							theOutputFile;		// the CAM output file
  };

} // end namespace


#endif 	// ESMCI_WebServPassThruSvr_H
