// $Id: ESMCI_WebServPassThruSvr.C,v 1.9 2012/01/06 20:19:29 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//==============================================================================
//
// ESMC WebServPassThruSvr method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ PassThruSvr methods declared
// in the companion file ESMCI_WebServPassThruSvr.h.  This code
// provides the functionality needed to implement a Process Controller
// Service, which essentially just passes requests from its client on to an
// ESMF Component Service (implemented with the ESMCI_ComponentSvr class).
//
//-----------------------------------------------------------------------------

#include "ESMF_LogMacros.inc" // TODO: remove once this comes through ESMCI_LogErr.h

#include "ESMCI_WebServPassThruSvr.h"

#if !defined(ESMF_OS_MinGW)
#include <netdb.h>
#else
#include <Winsock.h>
#endif
#include <string.h>

#include "ESMCI_WebServSocketUtils.h"
#include "ESMCI_WebServCompSvrClient.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_WebServPassThruSvr.C,v 1.9 2012/01/06 20:19:29 svasquez Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI
{

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::ESMCI_WebServPassThruSvr()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::ESMCI_WebServPassThruSvr()
//
// !INTERFACE:
ESMCI_WebServPassThruSvr::ESMCI_WebServPassThruSvr(
//
//
// !ARGUMENTS:
//
  int     port,    // (in) the port number on which to setup the socket service
                   // to listen for requests
  string  camDir,	 // (in) the directory where the CAM output files can be 
                   // found
  int     svrPort  // (in) the port number of the component server to which
                   // we'll be connecting
  )
//
// !DESCRIPTION:
//    Initialize the ESMF Process Controller service with the default values 
//    as well as the specified port number and output file directory.
//
//EOPI
//-----------------------------------------------------------------------------
{
	theNextClientId = 101;

	theCAMDir = camDir;
	theOutputFile = NULL;
   theSvrPort = svrPort;

	setPort(port);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::~ESMCI_WebServPassThruSvr()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::~ESMCI_WebServPassThruSvr()
//
// !INTERFACE:
ESMCI_WebServPassThruSvr::~ESMCI_WebServPassThruSvr(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Cleanup the process controller service.  For now, all this involves is 
//    making sure the socket is disconnected.
//
//EOPI
//-----------------------------------------------------------------------------
{
	theSocket.disconnect();
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::setPort()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::setPort()
//
// !INTERFACE:
void  ESMCI_WebServPassThruSvr::setPort(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  port    // (in) number of the port on which the socket service listens
               // for requests
  )
//
// !DESCRIPTION:
//    Sets the number of the port on which the socket service listens
//    for requests.
//
//EOPI
//-----------------------------------------------------------------------------
{
	thePort = port;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::requestLoop()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::requestLoop()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::requestLoop(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Sets up a socket service for a process controller server to handle 
//    client requests.  
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServPassThruSvr::requestLoop()\n");
	int	localrc = 0;
	
   //***
   // Setup the server socket
   //***
	if (theSocket.connect(thePort) < 0)
	{
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Connection error for the server socket.",
         &localrc);

      return localrc;
	}

   //***
   // Enter into a loop that waits for a client request and processes the
   // requests as they come in.  This loop continues until the client sends
   // an exit request (this isn't currently used).
   //***
	int	request;

	do
	{
		request = getNextRequest();

      if (request == ESMF_FAILURE)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_ARG_VALUE,
            "Request ID not valid.",
            &localrc);

         return ESMF_FAILURE;
      }

		serviceRequest(request);

	} while (request != NET_ESMF_EXIT);

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::getNextRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::getNextRequest()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::getNextRequest(
//
// !RETURN VALUE:
//    int  id of the client request (defined in ESMCI_WebServNetEsmf.h);
//         ESMF_FAILURE if error
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Listens on a server socket for client requests, and as the requests
//    arrive, reads the request id from the socket and returns it.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServPassThruSvr::getNextRequest()\n");
	int	localrc = 0;

   //***
   // Wait for client requests
   //***
	if (theSocket.accept() != ESMF_SUCCESS)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "The Server socket not accepting requests.",
         &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Read the request id string from the socket
   //***
	int	n;
	char	requestStr[50];

	if (theSocket.read(n, requestStr) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read request id from socket.",
         &localrc);

      return localrc;
   }

	//printf("SERVER: request: %s\n", requestStr);

   //***
   // Convert the string to a valid request id
   //***
	return getRequestId(requestStr);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::serviceRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::serviceRequest()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::serviceRequest(
//
// !RETURN VALUE:
//    int  id of the client request (the same value that's passed in)
//
// !ARGUMENTS:
//
  int  request    // id of the client request
  )
//
// !DESCRIPTION:
//    Calls the appropriate process method based on the client request id.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServPassThruSvr::serviceRequest()\n");
	//printf("Request ID: %d\n", request);

	switch (request)
	{
	case NET_ESMF_NEW: 
		processNew();
		break;

	case NET_ESMF_INIT: 
		processInit();
		break;

	case NET_ESMF_RUN: 
		processRun();
		break;

	case NET_ESMF_FINAL: 
		processFinal();
		break;

	case NET_ESMF_STATE: 
		processState();
		break;

	case NET_ESMF_FILES: 
		processFiles();
		break;

	case NET_ESMF_DATA: 
		processGetData();
		break;

	case NET_ESMF_END: 
		processEnd();
		break;

	case NET_ESMF_PING: 
		processPing();
		break;

	case NET_ESMF_EXIT: 
		processExit();
		break;

	default:
		break;
	}

	theSocket.close();

	return request;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::getRequestId()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::getRequestId()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::getRequestId(
//
// !RETURN VALUE:
//    int  id of the request based on the specified string; ESMF_FAILURE
//         if the id cannot be found
//
// !ARGUMENTS:
//
  const char  request[] // request string for which the id is to be returned
  )
//
// !DESCRIPTION:
//    Looks up a request id based on a specified string value.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServPassThruSvr::getRequestId()\n");

	if (strcmp(request, "NEW")   == 0)	return NET_ESMF_NEW;
	if (strcmp(request, "EXIT")  == 0)	return NET_ESMF_EXIT;
	if (strcmp(request, "INIT")  == 0)	return NET_ESMF_INIT;
	if (strcmp(request, "RUN")   == 0)	return NET_ESMF_RUN;
	if (strcmp(request, "FINAL") == 0)	return NET_ESMF_FINAL;
	if (strcmp(request, "STATE") == 0)	return NET_ESMF_STATE;
	if (strcmp(request, "FILES") == 0)	return NET_ESMF_FILES;
	if (strcmp(request, "DATA")  == 0)	return NET_ESMF_DATA;
	if (strcmp(request, "END")   == 0)	return NET_ESMF_END;
	if (strcmp(request, "PING")  == 0)	return NET_ESMF_PING;

	return ESMF_FAILURE;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::getRequestFromId()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::getRequestFromId()
//
// !INTERFACE:
char*  ESMCI_WebServPassThruSvr::getRequestFromId(
//
// !RETURN VALUE:
//    char*  string value for the specified request id; the string, "UNKN",
//           if the value cannot be found
//
// !ARGUMENTS:
//
  int  id      // request id for which the string value is to be returned
  )
//
// !DESCRIPTION:
//    Looks up a request string value based on a specified request id.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServPassThruSvr::getRequestFromId()\n");

	switch (id)
	{
	case NET_ESMF_EXIT:	return (char*)"EXIT";
	case NET_ESMF_NEW:	return (char*)"NEW";
	case NET_ESMF_INIT:	return (char*)"INIT";
	case NET_ESMF_RUN:	return (char*)"RUN";
	case NET_ESMF_FINAL:	return (char*)"FINAL";
	case NET_ESMF_STATE:	return (char*)"STATE";
	case NET_ESMF_FILES:	return (char*)"FILES";
	case NET_ESMF_DATA:	return (char*)"DATA";
	case NET_ESMF_END:	return (char*)"END";
	case NET_ESMF_PING:	return (char*)"PING";
	default:					return (char*)"UNKN";
	}

	return (char*)"UNKN";
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processNew()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processNew()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processNew(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request for a new client session.  This method reads the
//    client name from the socket, generates a new client id, creates a new
//    client info object and adds it to the list of clients, and then writes
//    the new client id to the socket to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("\n\nSERVER: processing New\n");
	int	localrc = 0;

	//***
	// Read the client name
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client name from socket.",
         &localrc);

      return localrc;
   }

	//printf("Buffer: %s\n", buf);

	//***
	// Generate a new client id and add the new client to the collection 
	// of clients
	//***
	int								clientId = getNextClientId();
	ESMCI_WebServClientInfo*	newClient = new ESMCI_WebServClientInfo(clientId);
	theClients[clientId] = newClient;

	//***
	// KDS: In the future, I think I want to startup a new component server
	//      for each client.
	//***
	newClient->setServerHost("localhost");
	newClient->setServerPort(theSvrPort);

	newClient->setStatus(NET_ESMF_STAT_READY);

	//***
	// Send back the new client id
	//***
	int	netClientId = htonl(clientId);
	//printf("Network client id: %d\n", netClientId);

	if (theSocket.write(4, &netClientId) != 4)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         &localrc);

      return localrc;
   }

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processInit()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processInit()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processInit(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to initialize the component.  This method reads the
//    client id from the socket and uses it to lookup the client information.
//    It then reads the names of input files (if any) from the socket.  The
//    request and its parameters are then passed on to the component server,
//    and finally, the component status is written to the socket to complete 
//    the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("\n\nSERVER: processing Init\n");

	int	localrc = 0;
	int	status = NET_ESMF_STAT_IDLE;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

      return localrc;
   }

   int	clientId = ntohl(*((unsigned int*)buf));
	//printf("Client ID: %d\n", clientId);

	//***
	// Get the number of files (should be either 0 or 1)... if there's 1, then
	// get the filename
	//***
	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read number of files from socket.",
         &localrc);

      return localrc;
   }

   int	numFiles = ntohl(*((unsigned int*)buf));
	//printf("Num Files: %d\n", numFiles);

	char	filename[1024];

	if (numFiles > 0)
	{
		if (theSocket.read(bytesRead, buf) <= 0)
   	{
      	ESMC_LogDefault.ESMCI_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Unable to read filename from socket.",
         	&localrc);

      	return localrc;
   	}

		strcpy(filename, (char*)buf);
		//printf("Filename: %s\n", filename);
	}

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	map<int, ESMCI_WebServClientInfo*>::iterator		iter;
	ESMCI_WebServClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            &localrc);

         return localrc;
      }

      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         &localrc);

      return localrc;
	}

	clientInfo = iter->second;
	//clientInfo->print();
	status = clientInfo->status();

	//***
	// Call the component initialize.  Must be in the READY state before the
	// initialize can be called, so a call to request the current state is
	// made first.
	//***
	ESMCI_WebServCompSvrClient	client(clientInfo->serverHost().c_str(), 
                                     clientInfo->serverPort(), 
                                     clientInfo->clientId());

	status = client.state();
	clientInfo->setStatus(status);

	if (status == NET_ESMF_STAT_READY)
	{
		status = client.init();
		//clientInfo->setStatus(NET_ESMF_STAT_INITIALIZING);
		clientInfo->setStatus(status);
	}
	else
	{
		// Should return an error message
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);

	if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         &localrc);

      return localrc;
   }
	//clientInfo->print();

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processRun()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processRun()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processRun(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to run the component.  This method reads the
//    client id from the socket and uses it to lookup the client information.
//    The request and its parameters are then passed on to the component 
//    server, and finally, the component status is written to the socket 
//    to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("\n\nSERVER: processing Run\n");

	int	localrc = 0;
	int	status = NET_ESMF_STAT_IDLE;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

      return localrc;
   }

   int	clientId = ntohl(*((unsigned int*)buf));
	//printf("Client ID: %d\n", clientId);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	map<int, ESMCI_WebServClientInfo*>::iterator		iter;
	ESMCI_WebServClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            &localrc);

         return localrc;
      }

      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         &localrc);

      return localrc;
	}

	clientInfo = iter->second;
	//clientInfo->print();
	status = clientInfo->status();

	//***
	// Call the component run (must have completed the initialize phase before
	// the run could be called)
	//***
	ESMCI_WebServCompSvrClient	client(clientInfo->serverHost().c_str(), 
                                     clientInfo->serverPort(), 
                                     clientInfo->clientId());

	status = client.state();
	clientInfo->setStatus(status);

	if (status == NET_ESMF_STAT_INIT_DONE)
	{
		status = client.run();
		//clientInfo->setStatus(NET_ESMF_STAT_RUNNING);
		clientInfo->setStatus(status);
	}
	else
	{
		// Should return an error message
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component run call to determine the state)
	//***
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);

	if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         &localrc);

      return localrc;
   }

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processFinal()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processFinal()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processFinal(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to finalize the component.  This method reads the
//    client id from the socket and uses it to lookup the client information.
//    The request and its parameters are then passed on to the component 
//    server, and finally, the component status is written to the socket 
//    to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("\n\nSERVER: processing Final\n");

	int	localrc = 0;
	int	status = NET_ESMF_STAT_IDLE;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

      return localrc;
   }

   int	clientId = ntohl(*((unsigned int*)buf));
	//printf("Client ID: %d\n", clientId);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	map<int, ESMCI_WebServClientInfo*>::iterator		iter;
	ESMCI_WebServClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            &localrc);

         return localrc;
      }

      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         &localrc);

      return localrc;
	}

	clientInfo = iter->second;
	//clientInfo->print();
	status = clientInfo->status();

	//***
	// Call the component finalize (must have completed at least the initalize
   // phase, and possibly the run phase before the finalize can be called 
   // (KDS: assuming you can call finalize after calling just initialize, but
   //       not sure if that is true.)
	//***
	ESMCI_WebServCompSvrClient	client(clientInfo->serverHost().c_str(), 
                                     clientInfo->serverPort(), 
                                     clientInfo->clientId());

	status = client.state();
	clientInfo->setStatus(status);

	if ((status == NET_ESMF_STAT_RUN_DONE)  ||
	    (status == NET_ESMF_STAT_INIT_DONE))
	{
		status = client.final();
		//clientInfo->setStatus(NET_ESMF_STAT_FINALIZING);
		clientInfo->setStatus(status);
	}
	else
	{
		// Should return an error message
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component finalize call to determine the state)
	//***
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);

	if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         &localrc);

      return localrc;
   }

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processState()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processState()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processState(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to retrieve the component state.  This method
//    reads the client id from the socket and uses it to lookup the client
//    information.  The request and its parameters are then passed on to 
//    the component server, and finally, the component status is written 
//    to the socket to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("\n\nSERVER: processing State\n");

	int	localrc = 0;
	int	status = NET_ESMF_STAT_IDLE;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

      return localrc;
   }

   int	clientId = ntohl(*((unsigned int*)buf));
	//printf("Client ID: %d\n", clientId);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	map<int, ESMCI_WebServClientInfo*>::iterator		iter;
	ESMCI_WebServClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            &localrc);

         return localrc;
      }

      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         &localrc);

      return localrc;
	}

	//***
	// Pass on the state request to the component server and get the current
	// status
	//***
	clientInfo = iter->second;
	//clientInfo->print();
	status = clientInfo->status();

	ESMCI_WebServCompSvrClient	client(clientInfo->serverHost().c_str(), 
                                     clientInfo->serverPort(), 
                                     clientInfo->clientId());

	status = client.state();
	clientInfo->setStatus(status);

	//***
	// Send the current state back to the client 
	//***
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);

	if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         &localrc);

      return localrc;
   }

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processFiles()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processFiles()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processFiles(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to retrieve the export filenames.  
//    (KDS: This method is here to support the existing NetEsmfClient call,
//          but we want to use the Get Data call instead, so I'm not 
//          supporting this call anymore.  Until we remove it completely,
//          I just return zero filenames.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("\n\nSERVER: processing Files\n");

	int	localrc = 0;
	int	status = NET_ESMF_STAT_IDLE;
	int	numFiles = 0;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

      return localrc;
   }

   int	clientId = ntohl(*((unsigned int*)buf));
	//printf("Client ID: %d\n", clientId);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	map<int, ESMCI_WebServClientInfo*>::iterator		iter;
	ESMCI_WebServClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		numFiles = 0;
		unsigned int  netNumFiles = htonl(numFiles);
		if (theSocket.write(4, &netNumFiles) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write number of files to socket.",
            &localrc);

         return localrc;
      }

		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            &localrc);

         return localrc;
      }

      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         &localrc);

      return localrc;
	}

	clientInfo = iter->second;
	//clientInfo->print();

	status = clientInfo->status();

	//***
	// Write the file information back to the client
	//***
	numFiles = 0;
	char	fileInfoBuf[1024];

	unsigned int  netNumFiles = htonl(numFiles);
	if (theSocket.write(4, &netNumFiles) != 4)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write number of files to socket.",
         &localrc);

      return localrc;
   }

	//***
	// Send the current state back to the client 
	//***
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);

	if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         &localrc);

      return localrc;
   }

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processGetData()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processGetData()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processGetData(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to retrieve the export data.  This method
//    reads the client id from the socket and uses it to lookup the client
//    information.  It then reads the data parameters (variable name, time, 
//    lat and lon) from the socket and uses that information to lookup the 
//    data from a netcdf file.  The data and the component status are then 
//    written back to the socket to complete the transaction.
//
//    (KDS: This design is very specific to CCSM/CAM and is hardcoded for
//          that prototype.  This needs to be redesigned to be more generic.)
//    (KDS: Also, getting one value for a specific time/lat/lon is really
//          inefficient and not practical.  There needs to be a way to handle
//          more data values at a time.)
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("\n\nSERVER: processing GetData\n");

	int	localrc = 0;
	int	status = NET_ESMF_STAT_IDLE;
	int	numFiles = 0;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

      return localrc;
   }

   int	clientId = ntohl(*((unsigned int*)buf));
	//printf("Client ID: %d\n", clientId);

	char	varName[256];
	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read variable name from socket.",
         &localrc);

      return localrc;
   }

	strncpy(varName, (char*)buf, 255);
	//printf("Var Name: %s\n", varName);

	//***
	// These next values are read as strings and then converted to double values
	//***
	char	tempValue[256];

	// Read time
	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read time value from socket.",
         &localrc);

      return localrc;
   }
	strncpy(tempValue, (char*)buf, 255);
	//printf("Time: %s\n", tempValue);
	double	timeValue = atof(tempValue);

	// Read lat
	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read lat value from socket.",
         &localrc);

      return localrc;
   }
	strncpy(tempValue, (char*)buf, 255);
	//printf("Lat: %s\n", tempValue);
	double	latValue = atof(tempValue);

	// Read lon
	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read lon value from socket.",
         &localrc);

      return localrc;
   }
	strncpy(tempValue, (char*)buf, 255);
	//printf("Lon: %s\n", tempValue);
	double	lonValue = atof(tempValue);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	map<int, ESMCI_WebServClientInfo*>::iterator		iter;
	ESMCI_WebServClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		numFiles = 0;
		unsigned int  netNumFiles = htonl(numFiles);
		if (theSocket.write(4, &netNumFiles) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write number of files to socket.",
            &localrc);

         return localrc;
      }

		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            &localrc);

         return localrc;
      }

      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         &localrc);

      return localrc;
	}

	clientInfo = iter->second;
	//clientInfo->print();

	//***
	// If the data files (to be added to ClientInfo) have not been retrieved
	// from the component server, then get them.
	// KDS: Right now, I'm hardcoding the output filename...
	//***
	if (theOutputFile == NULL)
	{
		theOutputFile = new ESMCI_WebServCAMOutputFile(
										theCAMDir + 
										"/camrun.cam2.rh0.0000-01-02-00000.nc");

		//***
		// KDS: Make call to component server to get filenames... set status 
		//      to whatever status gets returned
		//***
		status = clientInfo->status();
	}

	//***
	// Read the data from the specified file
	//***
	double	dataValue = theOutputFile->getDataValue(varName, 
                                                    timeValue, 
                                                    latValue, 
                                                    lonValue);
	//printf("Data Value: %e\n", dataValue);

	//***
	// Write the data back to the client
	//***
	sprintf(tempValue, "%e", dataValue);
	int	valueLen = strlen(tempValue) + 1;

	if (theSocket.write(valueLen, tempValue) != valueLen)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write data value to socket.",
         &localrc);

      return localrc;
   }

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);

	if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         &localrc);

      return localrc;
   }

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processEnd()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processEnd()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processEnd(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to end a client session.  This method reads the
//    client id from the socket and uses it to lookup the client information.
//    The client information is deleted from the list of clients, and finally,
//    the component status is written to the socket to complete the
//    transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("\n\nSERVER: processing End\n");

	int	localrc = 0;
	int	status = NET_ESMF_STAT_IDLE;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

      return localrc;
   }

   int	clientId = ntohl(*((unsigned int*)buf));
	//printf("Client ID: %d\n", clientId);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	map<int, ESMCI_WebServClientInfo*>::iterator		iter;
	ESMCI_WebServClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            &localrc);

         return localrc;
      }

      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         &localrc);

      return localrc;
	}

	clientInfo = iter->second;
	//clientInfo->print();
	status = clientInfo->status();

	ESMCI_WebServCompSvrClient	client(clientInfo->serverHost().c_str(), 
                                     clientInfo->serverPort(), 
                                     clientInfo->clientId());

	status = client.end();
	clientInfo->setStatus(status);

   //***
   // Remove the client from the collection of clients
	//***
	theClients.erase(clientId);
	delete clientInfo;

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	status = NET_ESMF_STAT_DONE;
	unsigned int	netStatus = htonl(status);

	if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         &localrc);

      return localrc;
   }

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processExit()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processExit()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processExit(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to exit the service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("\n\nSERVER: processing End\n");

	int	localrc = 0;
	int	status = NET_ESMF_STAT_IDLE;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

      return localrc;
   }

   int	clientId = ntohl(*((unsigned int*)buf));
	//printf("Client ID: %d\n", clientId);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	map<int, ESMCI_WebServClientInfo*>::iterator		iter;
	ESMCI_WebServClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.ESMCI_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            &localrc);

         return localrc;
      }

      ESMC_LogDefault.ESMCI_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         &localrc);

      return localrc;
	}

	clientInfo = iter->second;
	//clientInfo->print();
	status = clientInfo->status();

	ESMCI_WebServCompSvrClient	client(clientInfo->serverHost().c_str(), 
                                     clientInfo->serverPort(), 
                                     clientInfo->clientId());

	client.killServer();

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::processPing()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::processPing()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::processPing(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to ping the service.  Doesn't actually do anything.
//
//EOPI
//-----------------------------------------------------------------------------
{
	printf("\n\nSERVER: processing Ping\n");

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServPassThruSvr::getNextClientId()"
//BOPI
// !ROUTINE:  ESMCI_WebServPassThruSvr::getNextClientId()
//
// !INTERFACE:
int  ESMCI_WebServPassThruSvr::getNextClientId(
//
// !RETURN VALUE:
//    int  the next available client identifier
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Increments the next client identifier value by one and returns the
//    new value.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	nextClientId = theNextClientId;

	++theNextClientId;

	return nextClientId;
}

} // end namespace
