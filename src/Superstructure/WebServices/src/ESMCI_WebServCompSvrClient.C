// $Id: ESMCI_WebServCompSvrClient.C,v 1.8 2011/08/05 13:01:30 w6ws Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
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

#if !defined (ESMF_OS_MinGW)
#include <netdb.h>
#else
#include <Winsock.h>
#endif
#include <string.h>

#include "ESMCI_WebServSocketUtils.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_WebServCompSvrClient.C,v 1.8 2011/08/05 13:01:30 w6ws Exp $";
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
  ) : ESMCI_WebServNetEsmfClient(host, port)
//
// !DESCRIPTION:
//    Initialize the ESMF Component client with the name of the host and port
//    where the component service is running.
//
//EOPI
//-----------------------------------------------------------------------------
{

	//***
	// Set the data members
	//***
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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::init()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::init()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::init(
//
// !RETURN VALUE:
//   int  the current state of the component service; 
//        ESMF_FAILURE if an error occurs
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
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_INIT, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending init request to socket.",
         &localrc);

      return ESMF_FAILURE;
   }

	if (bytesSent == 4)
	{
      //***
      // Retrieve the response... which should be the server status
      //***
		if (getResponse(NET_ESMF_INIT, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.ESMC_LogMsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading init response from socket.",
            &localrc);

         return ESMF_FAILURE;
      }

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
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
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
	unsigned int	netClientId = htonl(theClientId);
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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::final()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::final()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::final(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
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
	unsigned int	netClientId = htonl(theClientId);
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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::state()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::state()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::state(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
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
	unsigned int	netClientId = htonl(theClientId);
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
            "Error reading get status response from socket.",
            &localrc);

         return ESMF_FAILURE;
      }

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
	int	localrc = 0;
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
	vector<string>		dataFiles;

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
	unsigned int	netClientId = htonl(theClientId);
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
			printf("Num Files: %d\n", numFiles);

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
				printf("File Type: %s\n", fileType);

				if (getResponse(NET_ESMF_FILES, bufSize, buf) <= 0)
      		{
         		ESMC_LogDefault.ESMC_LogMsgFoundError(
            		ESMC_RC_FILE_READ,
            		"Error reading get files response - filename from socket.",
            		&localrc);

         		return dataFiles;
      		}
				strcpy(fileName, buf);
				printf("File Name: %s\n", fileName);

				dataFiles.push_back(fileName);
			}
		}

		if (getResponse(NET_ESMF_FILES, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.ESMC_LogMsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading get data response - status from socket.",
            &localrc);

         return dataFiles;
      }
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
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
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
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

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
			printf("Status: %d\n", status);
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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::killServer()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::killServer()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::killServer(
//
// !RETURN VALUE:
//   int  ESMF_SUCCESS if the request is successfully sent;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to kill the component
//    server process.  Nothing is expected in return.
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
   // Send the "Exit" request... the client id does not need to be sent.
   //***
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_EXIT, 0, NULL)) != 0)
   {
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending exit client request to socket.",
         &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Disconnect from the component service
   //***
	disconnect();

	return ESMF_SUCCESS;
}


} // end namespace
