// $Id: ESMCI_WebServNetEsmfClient.C,v 1.3 2010/11/10 20:16:35 ksaint Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServNetEsmfClient.C"
//==============================================================================
//
// ESMC WebServNetEsmfClient method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ NetEsmfClient methods declared
// in the companion file ESMCI_WebServNetEsmfClient.h.  This code
// provides the functionality needed to communicate with an ESMF component 
// (grid or coupler) service.
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServNetEsmfClient.h"

#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "ESMCI_WebServSocketUtils.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_WebServNetEsmfClient.C,v 1.3 2010/11/10 20:16:35 ksaint Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI
{

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::ESMCI_WebServNetEsmfClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::ESMCI_WebServNetEsmfClient()
//
// !INTERFACE:
ESMCI_WebServNetEsmfClient::ESMCI_WebServNetEsmfClient(
//
//
// !ARGUMENTS:
//
  const char*  host,   // (in) the name of the host machine running the
                       // component service
  int          port    // (in) the port number of the component service 
                       // to which this client will connect
  )
//
// !DESCRIPTION:
//    Initialize the ESMF Component client with the name of the host and port
//    where the component service is running.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//***
	// if the host isn't specified, default to the "localhost"
	//***
	theHost = NULL;

	if (host == NULL)
	{
		host = "localhost";
	}

	setHost(host);
	setPort(port);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::~ESMCI_WebServNetEsmfClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::~ESMCI_WebServNetEsmfClient()
//
// !INTERFACE:
ESMCI_WebServNetEsmfClient::~ESMCI_WebServNetEsmfClient(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Cleans up the ESMF Component client by disconnecting from the service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	theSocket.disconnect();
	delete theHost;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::setHost()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::setHost()
//
// !INTERFACE:
void  ESMCI_WebServNetEsmfClient::setHost(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  const char*  host    // (in) the name of the host machine running the
                       // component service
  )
//
// !DESCRIPTION:
//    Sets the name of the component service host machine.
//
//EOPI
//-----------------------------------------------------------------------------
{
	if (theHost)
	{
		delete theHost;
	}

	theHost = new char[strlen(host) + 1];
	strcpy(theHost, host);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::setPort()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::setPort()
//
// !INTERFACE:
void  ESMCI_WebServNetEsmfClient::setPort(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int          port    // (in) the port number of the component service 
                       // to which this client will connect
  )
//
// !DESCRIPTION:
//    Sets the component service port number.
//
//EOPI
//-----------------------------------------------------------------------------
{
	thePort = port;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::sendRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::sendRequest()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::sendRequest(
//
// !RETURN VALUE:
//    int  number of bytes written to the socket (in addition to the request 
//         msg); ESMF_FAILURE if there is an error.
//
// !ARGUMENTS:
//
  int    request,		// (in) the request identifier
  int    length,		// (in) the length of the data to send
  void*  data			// (in) the buffer containing the data to send
  )
//
// !DESCRIPTION:
//    Sends a request to the component service.  First, it sends the request
//    identifier.  Then, it sends any ancillary data.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServNetEsmfClient::sendRequest()\n");

	int		localrc = 0;
	char*		requestStr = getRequestFromId(request);

	if (strcmp(requestStr, "UNKN") == 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid request id.",
         &localrc);

		return ESMF_FAILURE;
	}

	if (theSocket.send(requestStr) <= 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error writing request id to socket.",
         &localrc);

      return ESMF_FAILURE;
	}
	//printf("\nCLIENT: req: %s\n", requestStr);

	int	bytesWritten = 0;

	if ((length > 0)  &&  (data != NULL))
	{
		if ((bytesWritten = theSocket.write(length, data)) != length)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error writing request data to socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}
	}

	return bytesWritten;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::getResponse()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::getResponse()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::getResponse(
//
// !RETURN VALUE:
//    int  number of bytes read from the socket; ESMF_FAILURE if there is
//         and error.
//
// !ARGUMENTS:
//
  int    request,		// (in) the request identifier (ignored)
  int&   length,		// (out) the length of the data placed in the buffer
  void*  data			// (out) the buffer containing the received data 
  )
//
// !DESCRIPTION:
//    Reads a request response from the component service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServNetEsmfClient::getResponse()\n");
	int	localrc = 0;

	length = 0;
	if (theSocket.read(length, data) <= 0)
	{
     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_FILE_WRITE,
        	"Error reading request response from socket.",
        	&localrc);

     	return ESMF_FAILURE;
	}

	return length;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::connect()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::connect()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::connect(
//
// !RETURN VALUE:
//   int  socket file descriptor if successful, ESMF_FAILURE otherwise.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Connects to the component service identified by the host name and port
//    number data members.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServNetEsmfClient::connect()\n");

	return theSocket.connect(theHost, thePort);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::disconnect()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::disconnect()
//
// !INTERFACE:
void  ESMCI_WebServNetEsmfClient::disconnect(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Disconnects from the component service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServNetEsmfClient::disconnect()\n");

	return theSocket.disconnect();
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::getRequestId()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::getRequestId()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::getRequestId(
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
	//printf("ESMCI_WebServNetEsmfClient::getRequestId()\n");

	if (strcmp(request, "EXIT")  == 0)	return NET_ESMF_EXIT;
	if (strcmp(request, "NEW")   == 0)	return NET_ESMF_NEW;
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
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::getRequestFromId()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::getRequestFromId()
//
// !INTERFACE:
char*  ESMCI_WebServNetEsmfClient::getRequestFromId(
//
// !RETURN VALUE:
//    char*  string value for the specified request id
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
	//printf("ESMCI_WebServNetEsmfClient::getRequestFromId()\n");

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
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::newClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::newClient()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::newClient(
//
// !RETURN VALUE:
//   int  a unique identifier for this client on the component server; 
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  const char*  clientName	// a name for the client
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to create a new
//    client session, retrieves the new client id, and then disconnect from 
//    the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	int	clientId = -1;
	int	bufSize = 0;
   char  buf[1024];

	//***
	// Connect to the component service
	//***
	if (connect() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		return ESMF_FAILURE;
	}

	//***
	// Send the "New Client" request... sending along the client name
	//***
	strcpy(buf, clientName);
	bufSize = strlen(clientName) + 1;

	int	bytesSent = 0;
	if ((bytesSent = sendRequest(NET_ESMF_NEW, bufSize, buf)) == ESMF_FAILURE)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending new client request to socket.",
         &localrc);

      return ESMF_FAILURE;
	}

	if (bytesSent == bufSize)
	{
		//***
		// Get the server response, which should be a new client id
		//***
		if (getResponse(NET_ESMF_NEW, bufSize, buf) == ESMF_FAILURE)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error reading new client response from socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		if (bufSize == 4)
		{
			clientId = ntohl(*((unsigned int*)buf));
			//printf("Client ID: %d\n", clientId);
		}
	}

	//***
	// Disconnect from the component service
	//***
	disconnect();

	return clientId;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::init()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::init()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::init(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  int  clientId		// the idenfier of the client on the component server
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to initialize the
//    component, retrieve the server status, and then disconnect from 
//    the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	//***
	// Connect to the component service
	//***
	if (connect() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		return ESMF_FAILURE;
	}

	//***
	// Send the "Initialize" request... along with the client identifier
	//***
	unsigned int	netClientId = htonl(clientId);
	int				bytesSent;

	if ((bytesSent = sendRequest(NET_ESMF_INIT, 4, &netClientId)) <= 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending initialize request to socket.",
         &localrc);

      return ESMF_FAILURE;
	}

	if (bytesSent == 4)
	{
		int				numFiles = 0;
		unsigned int	netNumFiles = htonl(numFiles);
		int				bytesWritten;

		if ((bytesWritten = theSocket.write(4, &netNumFiles)) <= 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error sending number of files to socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		//***
		// Retrieve the response... which should be the server status
		//***
		if (getResponse(NET_ESMF_INIT, bufSize, buf) <= 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error reading initialize response from socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
			//printf("Status: %d\n", status);
		}
	}

	//***
	// Disconnect from the component service
	//***
	disconnect();

	return status;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::init()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::init()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::init(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  int          clientId,	// the idenfier of the client on the component svr
  const char*  filename		// the name of a file that contains input state data
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to initialize the
//    component, retrieve the server status, and then disconnect from 
//    the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	//***
	// Connect to the component service
	//***
	if (connect() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		return ESMF_FAILURE;
	}

	//***
	// Send the "Initialize" request... along with the client identifier and
	// the import state filename
	//***
	unsigned int	netClientId = htonl(clientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_INIT, 4, &netClientId)) <= 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending initialize request to socket.",
         &localrc);

      return ESMF_FAILURE;
	}

	if (bytesSent == 4)
	{
		int				numFiles = 1;
		unsigned int	netNumFiles = htonl(numFiles);
		int				bytesWritten = 0;

		if ((bytesWritten = theSocket.write(4, &netNumFiles)) != 4)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error sending number of files to socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		int	filenameSize = strlen(filename) + 1;
		if ((bytesWritten = 
				theSocket.write(filenameSize, (void*)filename)) != filenameSize)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error sending filename to socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		//***
		// Retrieve the response... which should be the server status
		//***
		if (getResponse(NET_ESMF_INIT, bufSize, buf) <= 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error reading initialize response from socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
			//printf("Status: %d\n", status);
		}
	}

	//***
	// Disconnect from the component service
	//***
	disconnect();

	return status;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::run()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::run()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::run(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  int  clientId		// the idenfier of the client on the component server
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to run the
//    component, retrieve the server status, and then disconnect from 
//    the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	//***
	// Connect to the component service
	//***
	if (connect() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		return ESMF_FAILURE;
	}

	//***
	// Send the "Run" request... along with the client identifier
	//***
	unsigned int	netClientId = htonl(clientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_RUN, 4, &netClientId)) != 4)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending run request to socket.",
         &localrc);

      return ESMF_FAILURE;
	}

	if (bytesSent == 4)
	{
		//***
		// Retrieve the response... which should be the server status
		//***
		if (getResponse(NET_ESMF_RUN, bufSize, buf) <= 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error reading run response from socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
			//printf("Status: %d\n", status);
		}
	}

	//***
	// Disconnect from the component service
	//***
	disconnect();

	return status;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::final()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::final()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::final(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  int  clientId		// the idenfier of the client on the component server
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to finalize the
//    component, retrieve the server status, and then disconnect from 
//    the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	//***
	// Connect to the component service
	//***
	if (connect() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		return ESMF_FAILURE;
	}

	//***
	// Send the "Finalize" request... along with the client identifier
	//***
	unsigned int	netClientId = htonl(clientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_FINAL, 4, &netClientId)) != 4)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending finalize request to socket.",
         &localrc);

      return ESMF_FAILURE;
	}

	if (bytesSent == 4)
	{
		//***
		// Retrieve the response... which should be the server status
		//***
		if (getResponse(NET_ESMF_FINAL, bufSize, buf) <= 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error reading finalize response from socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
			//printf("Status: %d\n", status);
		}
	}

	//***
	// Disconnect from the component service
	//***
	disconnect();

	return status;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::state()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::state()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::state(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  int  clientId		// the idenfier of the client on the component server
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to get the current 
//    service state, retrieve the server status, and then disconnect from 
//    the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	//***
	// Connect to the component service
	//***
	if (connect() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		return ESMF_FAILURE;
	}

	//***
	// Send the "Get State" request... along with the client identifier
	//***
	unsigned int	netClientId = htonl(clientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_STATE, 4, &netClientId)) != 4)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending get state request to socket.",
         &localrc);

      return ESMF_FAILURE;
	}

	if (bytesSent == 4)
	{
		//***
		// Retrieve the response... which should be the server status
		//***
		if (getResponse(NET_ESMF_STATE, bufSize, buf) <= 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error reading get state response from socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
			// printf("Status: %d\n", status);
		}
	}

	//***
	// Disconnect from the component service
	//***
	disconnect();

	return status;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::files()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::files()
//
// !INTERFACE:
vector<string>  ESMCI_WebServNetEsmfClient::files(
//
// !RETURN VALUE:
//   vector<string>  a list of filenames that contain the export data
//
// !ARGUMENTS:
//
  int  clientId		// the idenfier of the client on the component server
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to get the export 
//    filenames, retrieve the filenames and the component status, and then 
//    disconnect from the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	vector<string>		dataFiles;

	//***
	// Connect to the component service
	//***
	if (connect() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		return dataFiles;
	}

	//***
	// Send the "Get Files" request... along with the client identifier
	//***
	unsigned int	netClientId = htonl(clientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_FILES, 4, &netClientId)) != 4)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending get files request to socket.",
         &localrc);

      return dataFiles;
	}

	if (bytesSent == 4)
	{
		//***
		// Retrieve the response... which should include the number of export
		// files, the export filenames (if any), and the component server status
		//***
		if (getResponse(NET_ESMF_FILES, bufSize, buf) <= 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error reading get files response - number of files from socket.",
         	&localrc);

      	return dataFiles;
		}

		if (bufSize == 4)
		{
			char	fileType[1024];
			char	fileName[1024];
			int	numFiles = ntohl(*((unsigned int*)buf));
			//printf("Num Files: %d\n", numFiles);

			for (int i = 0; i < numFiles; ++i)
			{
				if (getResponse(NET_ESMF_FILES, bufSize, buf) <= 0)
				{
      			ESMC_LogDefault.ESMC_LogMsgFoundError(
         			ESMC_RC_FILE_READ,
         			"Error reading get files response - file type from socket.",
         			&localrc);

      			return dataFiles;
				}
				strcpy(fileType, buf);
				//printf("File Type: %s\n", fileType);

				if (getResponse(NET_ESMF_FILES, bufSize, buf) <= 0)
				{
      			ESMC_LogDefault.ESMC_LogMsgFoundError(
         			ESMC_RC_FILE_READ,
         			"Error reading get files response - filename from socket.",
         			&localrc);

      			return dataFiles;
				}
				strcpy(fileName, buf);
				//printf("File Name: %s\n", fileName);

				dataFiles.push_back(fileName);
			}
		}

		if (getResponse(NET_ESMF_FILES, bufSize, buf) <= 0)
		{
    			ESMC_LogDefault.ESMC_LogMsgFoundError(
       			ESMC_RC_FILE_READ,
       			"Error reading get files response - status from socket.",
       			&localrc);

    			return dataFiles;
		}
		status = ntohl(*((unsigned int*)buf));
		//printf("Status: %d\n", status);
	}

	//***
	// Disconnect from the component service
	//***
	disconnect();

	return dataFiles;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::getData()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::getData()
//
// !INTERFACE:
string  ESMCI_WebServNetEsmfClient::getData(
//
// !RETURN VALUE:
//   string  a string representation of the data value for the specified
//           input parameters
//
// !ARGUMENTS:
//
  int      clientId,	// (in) the idenfier of the client on the component server
  string   varName,	// (in) the variable name of the data to retrieve
  string   time,		// (in) the timestamp of the data to retrieve
  string   lat,		// (in) the latitude of the data to retrieve
  string   lon,		// (in) the longitude of the data to retrieve
  string&  dataValue	// (out) the string representation of the data value
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to get the data value 
//    for the specified parameters, retrieve the data value, and then 
//    disconnect from the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int		localrc = 0;
	int		status = 0;
	int		bufSize = 0;
   char  	buf[1024];

	//***
	// Connect to the component service
	//***
	if (connect() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		return "";
	}

	//***
	// Send the "Get Data" request... along with the client identifier
	//***
	//printf("ESMCI_WebServNetEsmfClient::getData()\n");
	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_DATA, 4, &netClientId)) != 4)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending get data request to socket.",
         &localrc);

      return "";
	}

	if (bytesSent == 4)
	{
		int			bytesWritten = 0;

		//***
		// Send the variable name
		//***
		const char*	varNameChar = varName.c_str();
		int			nameLen = strlen(varNameChar) + 1;

		if ((bytesWritten = 
				theSocket.write(nameLen, (void*)varNameChar)) != nameLen)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error sending variable name to socket.",
         	&localrc);

      	return "";
		}

		//***
		// Send the time value
		//***
		const char*	timeChar = time.c_str();
		int			timeLen = strlen(timeChar) + 1;

		if ((bytesWritten = theSocket.write(timeLen, (void*)timeChar)) != timeLen)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error sending time value to socket.",
         	&localrc);

      	return "";
		}

		//***
		// Send the latitude value
		//***
		const char*	latChar = lat.c_str();
		int			latLen = strlen(latChar) + 1;

		if ((bytesWritten = theSocket.write(latLen, (void*)latChar)) != latLen)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error sending latitude value to socket.",
         	&localrc);

      	return "";
		}

		//***
		// Send the longitude value
		//***
		const char*	lonChar = lon.c_str();
		int			lonLen = strlen(lonChar) + 1;

		if ((bytesWritten = theSocket.write(lonLen, (void*)lonChar)) != lonLen)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error sending longitude value to socket.",
         	&localrc);

      	return "";
		}

		//***
		// Retrieve the response... which should be the data value
		//***
		if (getResponse(NET_ESMF_DATA, bufSize, buf) <= 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error reading get data response from socket.",
         	&localrc);

      	return "";
		}

		dataValue = buf;
	}

	//***
	// Disconnect from the component service
	//***
	disconnect();

	return dataValue;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::end()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::end()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::end(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  int  clientId		// the idenfier of the client on the component server
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to end the client 
//    session on the component server, retrieve the server status, and then 
//    disconnect from the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	//***
	// Connect to the component service
	//***
	if (connect() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		return ESMF_FAILURE;
	}

	//***
	// Send the "End Client" request... along with the client identifier
	//***
	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_END, 4, &netClientId)) != 4)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending end client request to socket.",
         &localrc);

      return ESMF_FAILURE;
	}

	if (bytesSent == 4)
	{
		//***
		// Retrieve the response... which should be the server status
		//***
		if (getResponse(NET_ESMF_END, bufSize, buf) <= 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error reading end client response from socket.",
         	&localrc);

      	return ESMF_FAILURE;
		}

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
			//printf("Status: %d\n", status);
		}
	}

	//***
	// Disconnect from the component service
	//***
	disconnect();

	return status;
}


} // end namespace

