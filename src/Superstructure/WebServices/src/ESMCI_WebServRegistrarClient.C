// $Id: ESMCI_WebServRegistrarClient.C,v 1.2 2010/11/02 18:36:04 ksaint Exp $
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
#define ESMC_FILENAME "ESMCI_WebServRegistrarClient.C"
//==============================================================================
//
// ESMC WebServRegistrarClient method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ RegistrarClient methods declared
// in the companion file ESMCI_WebServRegistrarClient.h.  This code
// provides the functionality needed to communicate with a Registrar service.
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServRegistrarClient.h"

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
static const char *const version = "$Id: ESMCI_WebServRegistrarClient.C,v 1.2 2010/11/02 18:36:04 ksaint Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI
{


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::ESMCI_WebServRegistrarClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::ESMCI_WebServRegistrarClient()
//
// !INTERFACE:
ESMCI_WebServRegistrarClient::ESMCI_WebServRegistrarClient(
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
//    Initialize the Registrar client with the name of the host and port
//    where the Registrar service is running.
//
//EOPI
//-----------------------------------------------------------------------------
{
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
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::~ESMCI_WebServRegistrarClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::~ESMCI_WebServRegistrarClient()
//
// !INTERFACE:
ESMCI_WebServRegistrarClient::~ESMCI_WebServRegistrarClient(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Cleans up the Registrar client by disconnecting from the service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	theSocket.disconnect();
	delete theHost;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::setHost()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::setHost()
//
// !INTERFACE:
void  ESMCI_WebServRegistrarClient::setHost(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  const char*  host    // (in) the name of the host machine running the
                       // Registrar service
  )
//
// !DESCRIPTION:
//    Sets the name of the Registrar service host machine.
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
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::setPort()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::setPort()
//
// !INTERFACE:
void  ESMCI_WebServRegistrarClient::setPort(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int          port    // (in) the port number of the Registrar service
                       // to which this client will connect
  )
//
// !DESCRIPTION:
//    Sets the Registrar service port number.
//
//EOPI
//-----------------------------------------------------------------------------
{
	thePort = port;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::sendRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::sendRequest()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::sendRequest(
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
//    Sends a request to the Registrar service.  First, it sends the request
//    identifier.  Then, it sends any ancillary data.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("RegistrarClient::sendRequest()\n");

	int	bytesWritten = 0;

	if ((length > 0)  &&  (data != NULL))
	{
		bytesWritten = theSocket.write(length, data);
	}

	return bytesWritten;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::getResponse()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::getResponse()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::getResponse(
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
//    Reads a request response from the Registrar service.
//
//EOPI
//-----------------------------------------------------------------------------
{
//printf("RegistrarClient::getResponse()\n");
	length = 0;
	theSocket.read(length, data);

	return length;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::registerComp()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::registerComp()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::registerComp(
//
// !RETURN VALUE:
//    int  number of bytes read from the socket.
//
// !ARGUMENTS:
//
  char*  name,			// (in) the name of the component service to register
  char*  desc,			// (in) the description of the component service 
  char*  hostName,	// (in) the host name of the component service 
  char*  portNum,		// (in) the port number of the component service
  void*  retValue		// (out) the return value from the Registrar
  )
//
// !DESCRIPTION:
//    Registers the component service identified by the input parameters 
//    with the Registrar.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("RegistrarClient::registerComp()\n");

	theSocket.write(strlen("register") + 1, (char*)"register");
	theSocket.write(strlen(name) + 1, name);
	theSocket.write(strlen(desc) + 1, desc);
	theSocket.write(strlen(hostName) + 1, hostName);
	theSocket.write(strlen(portNum) + 1, portNum);

	int	length = 0;
	theSocket.read(length, retValue);

	return length;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::unregisterComp()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::unregisterComp()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::unregisterComp(
//
// !RETURN VALUE:
//    int  number of bytes read from the socket.
//
// !ARGUMENTS:
//
  char*  name,			// (in) the name of the component service to unregister
  char*  hostName,	// (in) the host name of the component service 
  char*  portNum,		// (in) the port number of the component service
  void*  retValue		// (out) the return value from the Registrar
  )
//
// !DESCRIPTION:
//    Unregisters the component service identified by the input parameters 
//    from the Registrar.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("RegistrarClient::unregisterComp()\n");

	theSocket.write(strlen("unregister") + 1, (char*)"unregister");
	theSocket.write(strlen(name) + 1, name);
	theSocket.write(strlen(hostName) + 1, hostName);
	theSocket.write(strlen(portNum) + 1, portNum);

	int	length = 0;
	theSocket.read(length, retValue);

	return length;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::connect()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::connect()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::connect(
//
// !RETURN VALUE:
//   int  socket file descriptor if successful, -1 otherwise.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Connects to the Registrar service identified by the host name and port
//    number data members.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("RegistrarClient::connect()\n");

	return theSocket.clientConnect(theHost, thePort);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::disconnect()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::disconnect()
//
// !INTERFACE:
void  ESMCI_WebServRegistrarClient::disconnect(
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
	//printf("RegistrarClient::disconnect()\n");

	return theSocket.disconnect();
}

} // end namespace
