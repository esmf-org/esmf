// $Id: ESMCI_WebServCompSvrClient.C,v 1.2 2010/11/02 18:36:04 ksaint Exp $
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
#define ESMC_FILENAME "ESMCI_WebServCompSvrClient.C"
//==============================================================================
//
// ESMC WebServCompSvrClient method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ CompSvrClient methods declared
// in the companion file ESMCI_WebServCompSvrClient.h.  This code
// provides the functionality needed to communicate with an ESMF grid 
// component service implemented with the ESMCI_WebServComponentSvr class.
// This class is intended to be used only by a PassThruSvr service which
// is the intermediary between the client (web service) and the component
// service.
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServCompSvrClient.h"

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

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_WebServCompSvrClient.C,v 1.2 2010/11/02 18:36:04 ksaint Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI
{

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::ESMCI_WebServCompSvrClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::ESMCI_WebServCompSvrClient()
//
// !INTERFACE:
ESMCI_WebServCompSvrClient::ESMCI_WebServCompSvrClient(
//
//
// !ARGUMENTS:
//
  const char*  host,   	// (in) the name of the host machine running the
                       	// component service
  int          port,   	// (in) the port number of the component service
                       	// to which this client will connect
  int          clientId // (in) the id of the client on the PassThruSvr
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

	//***
	// Set the data members
	//***
	setHost(host);
	setPort(port);
	setClientId(clientId);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::~ESMCI_WebServCompSvrClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::~ESMCI_WebServCompSvrClient()
//
// !INTERFACE:
ESMCI_WebServCompSvrClient::~ESMCI_WebServCompSvrClient(
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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::setHost()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::setHost()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrClient::setHost(
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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::setPort()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::setPort()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrClient::setPort(
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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::setClientId()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::setClientId()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrClient::setClientId(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  clientId	// (in) the unique id of the client on the PassThruSvr
  )
//
// !DESCRIPTION:
//    Sets the id of the client on the PassThruSvr.
//
//EOPI
//-----------------------------------------------------------------------------
{
	theClientId = clientId;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::sendRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::sendRequest()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::sendRequest(
//
// !RETURN VALUE:
//    int  number of bytes written to the socket (in addition to the request
//         msg).
//
// !ARGUMENTS:
//
  int    request,    // (in) the request identifier
  int    length,     // (in) the length of the data to send
  void*  data        // (in) the buffer containing the data to send
  )
//
// !DESCRIPTION:
//    Sends a request to the component service.  First, it sends the request
//    identifier.  Then, it sends any ancillary data.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServCompSvrClient::sendRequest()\n");

	//***
	// Get the id from the request string and send it
	//***
	char*		requestStr = getRequestFromId(request);

	theSocket.send(requestStr);
	//printf("\nCLIENT: req: %s\n", requestStr);

	//***
	// Send along the rest of the data
	//***
	int	bytesWritten = 0;

	if ((length > 0)  &&  (data != NULL))
	{
		bytesWritten = theSocket.write(length, data);
	}

	return bytesWritten;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::getResponse()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::getResponse()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::getResponse(
//
// !RETURN VALUE:
//    int  number of bytes read from the socket.
//
// !ARGUMENTS:
//
  int    request,    // (in) the request identifier (ignored)
  int&   length,     // (out) the length of the data placed in the buffer
  void*  data        // (out) the buffer containing the received data
  )
//
// !DESCRIPTION:
//    Reads a request response from the component service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServCompSvrClient::getResponse()\n");

	length = 0;
	theSocket.read(length, data);

	return length;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::connect()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::connect()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::connect(
//
// !RETURN VALUE:
//   int  socket file descriptor if successful, -1 otherwise.
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
	//printf("ESMCI_WebServCompSvrClient::connect()\n");

	return theSocket.connect(theHost, thePort);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::disconnect()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::disconnect()
//
// !INTERFACE:
void  ESMCI_WebServCompSvrClient::disconnect(
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
	//printf("ESMCI_WebServCompSvrClient::disconnect()\n");

	return theSocket.disconnect();
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::getRequestId()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::getRequestId()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::getRequestId(
//
// !RETURN VALUE:
//    int  id of the request based on the specified string
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
	//printf("ESMCI_WebServCompSvrClient::getRequestId()\n");

	if (strcmp(request, "EXIT")  == 0)	return NET_ESMF_EXIT;
	if (strcmp(request, "NEW")   == 0)	return NET_ESMF_NEW;
	if (strcmp(request, "INIT")  == 0)	return NET_ESMF_INIT;
	if (strcmp(request, "RUN")   == 0)	return NET_ESMF_RUN;
	if (strcmp(request, "FINAL") == 0)	return NET_ESMF_FINAL;
	if (strcmp(request, "STATE") == 0)	return NET_ESMF_STATE;
	if (strcmp(request, "FILES") == 0)	return NET_ESMF_FILES;
	if (strcmp(request, "END")   == 0)	return NET_ESMF_END;
	if (strcmp(request, "PING")  == 0)	return NET_ESMF_PING;

	return NET_ESMF_UNKN;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::getRequestFromId()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::getRequestFromId()
//
// !INTERFACE:
char*  ESMCI_WebServCompSvrClient::getRequestFromId(
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
	//printf("ESMCI_WebServCompSvrClient::getRequestFromId()\n");

	switch (id)
	{
	case NET_ESMF_EXIT:	return (char*)"EXIT";
	case NET_ESMF_NEW:	return (char*)"NEW";
	case NET_ESMF_INIT:	return (char*)"INIT";
	case NET_ESMF_RUN:	return (char*)"RUN";
	case NET_ESMF_FINAL:	return (char*)"FINAL";
	case NET_ESMF_STATE:	return (char*)"STATE";
	case NET_ESMF_FILES:	return (char*)"FILES";
	case NET_ESMF_END:	return (char*)"END";
	case NET_ESMF_PING:	return (char*)"PING";
	default:					return (char*)"UNKN";
	}

	return (char*)"UNKN";
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::init()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::init()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::init(
//
// !RETURN VALUE:
//   int  the current state of the component service
//
// !ARGUMENTS:
//
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
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
	if (connect() < 0)
	{
		return status;
	}

   //***
   // Send the "Initialize" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int	bytesSent = sendRequest(NET_ESMF_INIT, 4, &netClientId);

	if (bytesSent == 4)
	{
		int	numFiles = 0;
		unsigned int	netNumFiles = htonl(numFiles);
		int	bytesWritten = theSocket.write(4, &netNumFiles);

      //***
      // Retrieve the response... which should be the server status
      //***
		getResponse(NET_ESMF_INIT, bufSize, buf);

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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::init()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::init()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::init(
//
// !RETURN VALUE:
//   int  the current state of the component service
//
// !ARGUMENTS:
//
  const char*  filename    // the name of a file that contains input state data
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
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
	if (connect() < 0)
	{
		return status;
	}

   //***
   // Send the "Initialize" request... along with the client identifier and
   // the import state filename
   //***
	unsigned int	netClientId = htonl(theClientId);
	int	bytesSent = sendRequest(NET_ESMF_INIT, 4, &netClientId);

	if (bytesSent == 4)
	{
		int				numFiles = 1;
		unsigned int	netNumFiles = htonl(numFiles);

		int	bytesWritten = theSocket.write(4, &netNumFiles);
		bytesWritten = theSocket.write(strlen(filename) + 1, (void*)filename);

      //***
      // Retrieve the response... which should be the server status
      //***
		getResponse(NET_ESMF_INIT, bufSize, buf);

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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::run()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::run()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::run(
//
// !RETURN VALUE:
//   int  the current state of the component service
//
// !ARGUMENTS:
//
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
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
	if (connect() < 0)
	{
		return status;
	}

   //***
   // Send the "Run" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int	bytesSent = sendRequest(NET_ESMF_RUN, 4, &netClientId);

	if (bytesSent == 4)
	{
      //***
      // Retrieve the response... which should be the server status
      //***
		getResponse(NET_ESMF_RUN, bufSize, buf);

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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::final()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::final()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::final(
//
// !RETURN VALUE:
//   int  the current state of the component service
//
// !ARGUMENTS:
//
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
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
	if (connect() < 0)
	{
		return status;
	}

   //***
   // Send the "Finalize" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int	bytesSent = sendRequest(NET_ESMF_FINAL, 4, &netClientId);

	if (bytesSent == 4)
	{
      //***
      // Retrieve the response... which should be the server status
      //***
		getResponse(NET_ESMF_FINAL, bufSize, buf);

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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::state()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::state()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::state(
//
// !RETURN VALUE:
//   int  the current state of the component service
//
// !ARGUMENTS:
//
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
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
	if (connect() < 0)
	{
		return status;
	}

   //***
   // Send the "Get State" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int	bytesSent = sendRequest(NET_ESMF_STATE, 4, &netClientId);

	if (bytesSent == 4)
	{
      //***
      // Retrieve the response... which should be the server status
      //***
		getResponse(NET_ESMF_STATE, bufSize, buf);

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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::files()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::files()
//
// !INTERFACE:
vector<string>  ESMCI_WebServCompSvrClient::files(
//
// !RETURN VALUE:
//   vector<string>  a list of filenames that contain the export data
//
// !ARGUMENTS:
//
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
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
	vector<string>		dataFiles;

	if (connect() < 0)
	{
		return dataFiles;
	}

   //***
   // Send the "Get Files" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int	bytesSent = sendRequest(NET_ESMF_FILES, 4, &netClientId);

	if (bytesSent == 4)
	{
      //***
      // Retrieve the response... which should include the number of export
      // files, the export filenames (if any), and the component server status
      //***
		getResponse(NET_ESMF_FILES, bufSize, buf);

		if (bufSize == 4)
		{
			char	fileType[1024];
			char	fileName[1024];
			int	numFiles = ntohl(*((unsigned int*)buf));
			//printf("Num Files: %d\n", numFiles);

			for (int i = 0; i < numFiles; ++i)
			{
				getResponse(NET_ESMF_FILES, bufSize, buf);
				strcpy(fileType, buf);
				//printf("File Type: %s\n", fileType);
				getResponse(NET_ESMF_FILES, bufSize, buf);
				strcpy(fileName, buf);
				//printf("File Name: %s\n", fileName);

				dataFiles.push_back(fileName);
			}
		}

		getResponse(NET_ESMF_FILES, bufSize, buf);
		status = ntohl(*((unsigned int*)buf));
		printf("Status: %d\n", status);
	}

   //***
   // Disconnect from the component service
   //***
	disconnect();

	return dataFiles;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::end()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::end()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::end(
//
// !RETURN VALUE:
//   int  the current state of the component service
//
// !ARGUMENTS:
//
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
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
	if (connect() < 0)
	{
		return status;
	}

   //***
   // Send the "End Client" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int	bytesSent = sendRequest(NET_ESMF_END, 4, &netClientId);

	if (bytesSent == 4)
	{
      //***
      // Retrieve the response... which should be the server status
      //***
		getResponse(NET_ESMF_END, bufSize, buf);

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
			printf("Status: %d\n", status);
		}
	}

   //***
   // Disconnect from the component service
   //***
	disconnect();

	return status;
}


} // end namespace
