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

#include <string.h>

#include "ESMCI_WebServSocketUtils.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
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
	char*		requestStr = ESMCI_WebServGetRequestFromId(request);

	if (strcmp(requestStr, "UNKN") == 0)
	{
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid request id.",
         ESMC_CONTEXT, &localrc);

		return ESMF_FAILURE;
	}

	if (theSocket.send(requestStr) <= 0)
	{
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error writing request id to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
	}
	//printf("\nCLIENT: req: %s\n", requestStr);

	int	bytesWritten = 0;

	if ((length > 0)  &&  (data != NULL))
	{
		if ((bytesWritten = theSocket.write(length, data)) != length)
		{
      	ESMC_LogDefault.MsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error writing request data to socket.",
         	ESMC_CONTEXT, &localrc);

      	return ESMF_FAILURE;
		}
	}

	return bytesWritten;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::sendData()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::sendData()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::sendData(
//
// !RETURN VALUE:
//    int  number of bytes written to the socket; ESMF_FAILURE if there is
//         an error.
//
// !ARGUMENTS:
//
  int    length,		// (in) the length of the data to send
  void*  data			// (in) the buffer containing the data to send
  )
//
// !DESCRIPTION:
//    Sends a packet of data to the service.  
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServNetEsmfClient::sendData()\n");

	int	localrc = 0;
	int	bytesWritten = 0;

	if ((length > 0)  &&  (data != NULL))
	{
		if ((bytesWritten = theSocket.write(length, data)) != length)
		{
      	ESMC_LogDefault.MsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error writing data to socket.",
         	ESMC_CONTEXT, &localrc);

      	return ESMF_FAILURE;
		}
	}

	return bytesWritten;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::sendString()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::sendString()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::sendString(
//
// !RETURN VALUE:
//    int  number of bytes written to the socket; ESMF_FAILURE if there is 
//         an error.
//
// !ARGUMENTS:
//
  const char*  data		// (in) the string containing the data to send
  )
//
// !DESCRIPTION:
//    Sends a string to the service.  
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServNetEsmfClient::sendString()\n");

	int	localrc = 0;
	int	bytesWritten = 0;

	if ((bytesWritten = theSocket.send(data)) <= 0)
	{
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error writing string to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
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
     	ESMC_LogDefault.MsgFoundError(
        	ESMC_RC_FILE_WRITE,
        	"Error reading request response from socket.",
        	ESMC_CONTEXT, &localrc);

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
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::getStateStr()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::getStateStr()
//
// !INTERFACE:
char*  ESMCI_WebServNetEsmfClient::getStateStr(
//
// !RETURN VALUE:
//    char*  string value for the specified request id
//
// !ARGUMENTS:
//
  int  state      // request id for which the string value is to be returned
  )
//
// !DESCRIPTION:
//    Looks up a state string value based on a specified state value.
//
//EOPI
//-----------------------------------------------------------------------------
{
   //printf("ESMCI_WebServNetEsmfClient::getStateStr()\n");

   switch (state)
   {
   case NET_ESMF_STAT_IDLE:          return (char*)"PENDING";
	case NET_ESMF_STAT_READY:         return (char*)"READY";
	case NET_ESMF_STAT_BUSY:          return (char*)"BUSY";
	case NET_ESMF_STAT_INITIALIZING:  return (char*)"INITIALIZING";
	case NET_ESMF_STAT_RUNNING:       return (char*)"RUNNING";
	case NET_ESMF_STAT_FINALIZING:    return (char*)"FINALIZING";
	case NET_ESMF_STAT_INIT_DONE:     return (char*)"INIT_DONE";
	case NET_ESMF_STAT_RUN_DONE:      return (char*)"RUN_DONE";
	case NET_ESMF_STAT_FINAL_DONE:    return (char*)"FINAL_DONE";
	case NET_ESMF_STAT_DONE:          return (char*)"DONE";
	case NET_ESMF_STAT_SUBMITTED:     return (char*)"SUBMITTED";
	case NET_ESMF_STAT_TIMESTEP_DONE: return (char*)"TIMESTEP_DONE";
	case NET_ESMF_STAT_ERROR:         return (char*)"ERROR";
   default:                          return (char*)"UNKN";
   }

   return (char*)"UNKN";
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfClient::getStateValue()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfClient::getStateValue()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfClient::getStateValue(
//
// !RETURN VALUE:
//    int  net esmf state based on the specified string; ESMF_FAILURE
//         if the id cannot be found
//
// !ARGUMENTS:
//
  const char*  stateStr // request string for which the id is to be returned
  )
//
// !DESCRIPTION:
//    Looks up a state value id based on a specified string value.
//
//EOPI
//-----------------------------------------------------------------------------
{
   //printf("ESMCI_WebServNetEsmfClient::getStateValue()\n");

	if (strcmp(stateStr, "PENDING") == 0)      return NET_ESMF_STAT_IDLE;
	if (strcmp(stateStr, "READY") == 0)        return NET_ESMF_STAT_READY;
	if (strcmp(stateStr, "BUSY") == 0)         return NET_ESMF_STAT_BUSY;
	if (strcmp(stateStr, "INITIALIZING") == 0) return NET_ESMF_STAT_INITIALIZING;
	if (strcmp(stateStr, "RUNNING") == 0)      return NET_ESMF_STAT_RUNNING;
	if (strcmp(stateStr, "FINALIZING") == 0)   return NET_ESMF_STAT_FINALIZING;
	if (strcmp(stateStr, "INIT_DONE") == 0)    return NET_ESMF_STAT_INIT_DONE;
	if (strcmp(stateStr, "RUN_DONE") == 0)     return NET_ESMF_STAT_RUN_DONE;
	if (strcmp(stateStr, "FINAL_DONE") == 0)   return NET_ESMF_STAT_FINAL_DONE;
	if (strcmp(stateStr, "DONE") == 0)         return NET_ESMF_STAT_DONE;
	if (strcmp(stateStr, "SUBMITTED") == 0)    return NET_ESMF_STAT_SUBMITTED;
	if (strcmp(stateStr, "TIMESTEP_DONE") == 0) 
														 return NET_ESMF_STAT_TIMESTEP_DONE;
	if (strcmp(stateStr, "ERROR") == 0)        return NET_ESMF_STAT_ERROR;

   return ESMF_FAILURE;
}


} // end namespace

