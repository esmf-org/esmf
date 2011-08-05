// $Id: ESMCI_WebServComponentSvr.C,v 1.11 2011/08/05 02:47:46 ksaint Exp $
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
#define ESMC_FILENAME "ESMCI_WebServComponentSvr.C"
//==============================================================================
//
// ESMC WebServComponentSvr method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ComponentSvr methods declared
// in the companion file ESMCI_WebServComponentSvr.h.  This code provides
// the functionality needed to implement an ESMF component (grid only) as
// a network-accessible service.
//
// (Note: This class is essentially a subset of the ESMCI_WebServNetEsmfServer
//        class.  It was created when setting up CCSM/CAM as a Component
//        and is used in conjunction with a "Process Controller" that is
//        implemented using the ESMCI_WebServPassThruSvr.)
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServComponentSvr.h"

#include <netdb.h>

#include "ESMCI_WebServSocketUtils.h"
#include "ESMCI_WebServRegistrarClient.h"
#include <ESMCI_IO_NetCDF.h>
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"


//***
// KDS: I think this section is going to have to move to a new file in the
//      interface directory
//***
extern "C"
{
	void FTN(f_esmf_processinit)(ESMCI::GridComp*  comp,
                                ESMCI::State*     importState,
                                ESMCI::State*     exportState,
                                ESMCI::Clock*     clock,
                                int               phase,
                                int*              rc);

	void FTN(f_esmf_processrun)(ESMCI::GridComp*  comp,
                               ESMCI::State*     importState,
                               ESMCI::State*     exportState,
                               ESMCI::Clock*     clock,
                               int               phase,
                               int*              rc);

	void FTN(f_esmf_processfinal)(ESMCI::GridComp*  comp,
                                 ESMCI::State*     importState,
                                 ESMCI::State*     exportState,
                                 ESMCI::Clock*     clock,
                                 int               phase,
                                 int*              rc);

	void*  initThreadStartup(void*);
	void*  runThreadStartup(void*);
	void*  finalThreadStartup(void*);
};

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_WebServComponentSvr.C,v 1.11 2011/08/05 02:47:46 ksaint Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI
{

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::ESMCI_WebServComponentSvr()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::ESMCI_WebServComponentSvr()
//
// !INTERFACE:
ESMCI_WebServComponentSvr::ESMCI_WebServComponentSvr(
//
//
// !ARGUMENTS:
//
  int  port    // (in) the port number on which to setup the socket service
               // to listen for requests
  )
//
// !DESCRIPTION:
//    Initialize the ESMF Component service with the default values as well
//    as the specified port number.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;

	//***
	// Initialize the status mutex
	//***
#ifndef ESMF_NO_PTHREADS
	if (pthread_mutex_init(&theStatusMutex, NULL) != 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while initializing mutex lock... behavior unknown.",
         &localrc);
	}
#endif

	//***
	// Set the data members
	//***
	setPort(port);
	setStatus(NET_ESMF_STAT_READY);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::~ESMCI_WebServComponentSvr()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::~ESMCI_WebServComponentSvr()
//
// !INTERFACE:
ESMCI_WebServComponentSvr::~ESMCI_WebServComponentSvr(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Cleanup the component service.  For now, all this involves is making
//    sure the socket is disconnected.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;

	theSocket.disconnect();

#ifndef ESMF_NO_PTHREADS
	if (pthread_mutex_destroy(&theStatusMutex) != 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while destroying mutex lock.",
         &localrc);
	}
#endif
}


/*
*****************************************************************************
**
*****************************************************************************
*/
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::setPort()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::setPort()
//
// !INTERFACE:
void  ESMCI_WebServComponentSvr::setPort(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  port    // (in) number of the port on which component service listens
               // for requests
  )
//
// !DESCRIPTION:
//    Sets the number of the port on which the component service listens
//    for requests.
//
//EOPI
//-----------------------------------------------------------------------------
{
	thePort = port;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::addOutputFilename()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::addOutputFilename()
//
// !INTERFACE:
void  ESMCI_WebServComponentSvr::addOutputFilename(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  string  filename    // the name of the output filename to add to the list
  )
//
// !DESCRIPTION:
//    Adds the specified filename to the list of output filenames.
//
//EOPI
//-----------------------------------------------------------------------------
{
	theOutputFiles.push_back(filename);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::requestLoop()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::requestLoop()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::requestLoop(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  ESMCI::GridComp*   comp,          // (in) the grid component
  ESMCI::State*      importState,   // (in) import state
  ESMCI::State*      exportState,   // (in) export state
  ESMCI::Clock*      clock,         // (in) clock
  int                phase,         // (in) phase
  ESMC_BlockingFlag  blockingFlag   // (in) blocking flag
  )
//
// !DESCRIPTION:
//    Sets up a socket service for a grid component server to handle client
//    requests.  The input parameters are all saved for later use when the
//    client makes requests of the server to initalize, run, and finalize.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServComponentSvr::grid requestLoop()\n");

	int	localrc = 0;

   //***
   // Save the input parameters... these are used later when the client
   // wants to execute the initialize, run and finalize procedures
   //***
	theGridComp     = comp;
	theImportState  = importState;
	theExportState  = exportState;
	theClock	       = clock;
	thePhase        = phase;
	theBlockingFlag = blockingFlag;
	
	//comp->print("");

   //***
   // Setup the server socket
   //***
	if (theSocket.connect(thePort) < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
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
//printf("Request ID: %d\n", request);

		if (request == ESMF_FAILURE)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
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
#define ESMC_METHOD "ESMCI_WebServComponentSvr::getNextRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::getNextRequest()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::getNextRequest(
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
	//printf("ESMCI_WebServComponentSvr::getNextRequest()\n");

	int	localrc = 0;

   //***
   // Wait for client requests
   //***
	if (theSocket.accept() < 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read request id from socket.",
         &localrc);

		//return localrc;
		return ESMF_FAILURE;
	}

	//printf("SERVER: request: %s\n", requestStr);

   //***
   // Convert the string to a valid request id and return it
   //***
	return getRequestId(requestStr);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::serviceRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::serviceRequest()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::serviceRequest(
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
	//printf("ESMCI_WebServComponentSvr::serviceRequest()\n");

	strcpy(theMsg, "OK");

	switch (request)
	{
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

	case NET_ESMF_EXIT: 
		printf("Exiting Component Server\n");
		break;

	default:
		break;
	}

	theSocket.close();

	return request;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::getRequestId()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::getRequestId()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::getRequestId(
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
	//printf("ESMCI_WebServComponentSvr::getRequestId()\n");

	if (strcmp(request, "INIT")  == 0)	return NET_ESMF_INIT;
	if (strcmp(request, "RUN")   == 0)	return NET_ESMF_RUN;
	if (strcmp(request, "FINAL") == 0)	return NET_ESMF_FINAL;
	if (strcmp(request, "STATE") == 0)	return NET_ESMF_STATE;
	if (strcmp(request, "FILES") == 0)	return NET_ESMF_FILES;
	if (strcmp(request, "DATA")  == 0)	return NET_ESMF_DATA;
	if (strcmp(request, "END")   == 0)	return NET_ESMF_END;
	if (strcmp(request, "EXIT")  == 0)	return NET_ESMF_EXIT;

	return ESMF_FAILURE;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::getRequestFromId()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::getRequestFromId()
//
// !INTERFACE:
char*  ESMCI_WebServComponentSvr::getRequestFromId(
//
// !RETURN VALUE:
//    char*  string value for the specified request id; the string, "UNKN"
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
	//printf("ESMCI_WebServComponentSvr::getRequestFromId()\n");

	switch (id)
	{
	case NET_ESMF_INIT:	return (char*)"INIT";
	case NET_ESMF_RUN:	return (char*)"RUN";
	case NET_ESMF_FINAL:	return (char*)"FINAL";
	case NET_ESMF_STATE:	return (char*)"STATE";
	case NET_ESMF_FILES:	return (char*)"FILES";
	case NET_ESMF_DATA:	return (char*)"DATA";
	case NET_ESMF_END:	return (char*)"END";
	case NET_ESMF_EXIT:	return (char*)"EXIT";
	default:					return (char*)"UNKN";
	}

	return (char*)"UNKN";
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::setStatus()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::setStatus()
//
// !INTERFACE:
void  ESMCI_WebServComponentSvr::setStatus(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  status      // new status value
  )
//
// !DESCRIPTION:
//    Sets the current status... has to lock the status mutex before setting
//    it and unlock the mutex after setting it.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;

#ifndef ESMF_NO_PTHREADS
	if (pthread_mutex_lock(&theStatusMutex) != 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while locking current status mutex lock... behavior unknown.",
         &localrc);
	}
#endif

	theCurrentStatus = status;

#ifndef ESMF_NO_PTHREADS
	if (pthread_mutex_unlock(&theStatusMutex) != 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while unlocking current status mutex lock... behavior unknown.",
         &localrc);
	}
#endif

   ESMCI::ESMCI_WebServRegistrarClient registrar("localhost", REGISTRAR_PORT);

	char	idStr[64];
	sprintf(idStr, "%d", theCurrentClientId);

   if (registrar.setStatus(idStr, registrar.getStateStr(theCurrentStatus)) == 
		ESMF_FAILURE)
   {
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_UNEXPECTED,
         "Error setting status on Registrar.",
         &localrc);
   }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::processInit()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::processInit()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::processInit(
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
//    client id from the socket and uses it to validate the client information.
//    It then reads the names of input files (if any) from the socket.  It
//    then creates a new thread which is responsible for calling the component 
//    initialization routine and writing the component status to the socket 
//    to complete the transaction.
//
//    (KDS: The whole import file stuff was not used for the CCSM/CAM project,
//          so I removed all of the file processing code (it was commented out
//          anyways), but left a placeholder if it needs to be added back in.
//
//EOPI
//-----------------------------------------------------------------------------
{
	printf("\n\nSERVER: processing Init\n");
	int	localrc = 0;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

		return localrc;
	}

   theCurrentClientId = ntohl(*((unsigned int*)buf));
	printf("SERVER: Client ID: %d\n", theCurrentClientId);

	if (theCurrentStatus == NET_ESMF_STAT_READY)
	{
		//***
		// Call the component initialize
		//***
		setStatus(NET_ESMF_STAT_INITIALIZING);

		//***
		// OK... here's where I'm gonna want to kick off a thread and do the
		// call to process initialize in the thread so that I can return
		// to the client immediately and not block the socket.  The thread
		// will be responsible for updating the status when the initialize is
		// done.
		//***
#ifndef ESMF_NO_PTHREADS
		pthread_t	thread;
		int			rc = 0;

		if ((rc = pthread_create(&thread, NULL, initThreadStartup, this)) != 0)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Error creating initialize thread.",
        		&localrc);

			return localrc;
		}
#else
		runInit();
#endif
	}

	//***
	// Send the current state back to the client 
	//***
printf("SERVER: Writing Status: %d\n", theCurrentStatus);
	unsigned int	netStatus = htonl(theCurrentStatus);
	if (theSocket.write(4, &netStatus) != 4)
	{
     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_FILE_WRITE,
        	"Unable to write status to socket.",
        	&localrc);

		return localrc;
	}

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::processRun()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::processRun()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::processRun(
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
//    client id from the socket and uses it to validate the client information.
//    It then creates a new thread which is responsible for calling the 
//    component run routine and writing the component status to the socket 
//    to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	printf("\n\nSERVER: processing Run\n");
	int	localrc = 0;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

		return localrc;
	}

   int	clientId = ntohl(*((unsigned int*)buf));
	printf("Client ID: %d\n", clientId);

	if (clientId != theCurrentClientId)
	{
		int				status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		setStatus(NET_ESMF_STAT_ERROR);

		if (theSocket.write(4, &netStatus) != 4)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Unable to write error status to socket.",
        		&localrc);

			return localrc;
		}

     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_ARG_VALUE,
        	"Invalid client id read from socket.",
        	&localrc);

		return localrc;
	}

	printf("Current Status: %d\n", theCurrentStatus);
	if (theCurrentStatus == NET_ESMF_STAT_INIT_DONE)
	{
		//***
		// Call the component run
		//***
		setStatus(NET_ESMF_STAT_RUNNING);

#ifndef ESMF_NO_PTHREADS
		pthread_t	thread;
		int			rc = 0;

		if ((rc = pthread_create(&thread, NULL, runThreadStartup, this)) != 0)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Error creating run thread.",
        		&localrc);

			return localrc;
		}
#else
		runRun();
#endif
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	unsigned int	netStatus = htonl(theCurrentStatus);
	if (theSocket.write(4, &netStatus) != 4)
	{
     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_FILE_WRITE,
        	"Unable to write status to socket.",
        	&localrc);

		return localrc;
	}

   return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::processFinal()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::processFinal()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::processFinal(
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
//    client id from the socket and uses it to validate the client information.
//    It then creates a new thread which is responsible for calling the 
//    component finalize routine and writing the component status to the 
//    socket to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	printf("\n\nSERVER: processing Final\n");
	int	localrc = 0;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

		return localrc;
	}

   int	clientId = ntohl(*((unsigned int*)buf));
	printf("Client ID: %d\n", clientId);

	if (clientId != theCurrentClientId)
	{
		int				status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		setStatus(NET_ESMF_STAT_ERROR);

		if (theSocket.write(4, &netStatus) != 4)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Unable to write error status to socket.",
        		&localrc);

			return localrc;
		}

     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_ARG_VALUE,
        	"Invalid client id read from socket.",
        	&localrc);

		return localrc;
	}

	if ((theCurrentStatus == NET_ESMF_STAT_INIT_DONE)  ||
	    (theCurrentStatus == NET_ESMF_STAT_RUN_DONE))
	{
		//***
		// Set the status to indicate that the service is busy right now
		//***
		setStatus(NET_ESMF_STAT_FINALIZING);

#ifndef ESMF_NO_PTHREADS
		pthread_t	thread;
		int			rc = 0;
		if ((rc = pthread_create(&thread, NULL, finalThreadStartup, this)) != 0)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Error creating finalize thread.",
        		&localrc);

			return localrc;
		}
#else
		runFinal();
#endif
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	unsigned int	netStatus = htonl(theCurrentStatus);
	if (theSocket.write(4, &netStatus) != 4)
	{
     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_FILE_WRITE,
        	"Unable to write status to socket.",
        	&localrc);

		return localrc;
	}

   return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::processState()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::processState()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::processState(
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
//    reads the client id from the socket (the client id is actually not used
//    right now). The component state is then written to the socket to 
//    complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	printf("\n\nSERVER: processing State\n");
	int	localrc = 0;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

		return localrc;
	}

   int	clientId = ntohl(*((unsigned int*)buf));
	printf("Client ID: %d\n", clientId);

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	printf("The Current Status: %d\n", theCurrentStatus);
	unsigned int	netStatus = htonl(theCurrentStatus);

	if (theSocket.write(4, &netStatus) != 4)
	{
     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_FILE_WRITE,
        	"Unable to write status to socket.",
        	&localrc);

		return localrc;
	}

   return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::processFiles()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::processFiles()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::processFiles(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to retrieve the export filenames.  This method
//    reads the client id from the socket and uses it to validate the client
//    information. Next, the list of export files is written out to the 
//    socket.  And finally, the component status is written to the socket 
//    to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
	printf("\n\nSERVER: processing Files\n");

	int	localrc = 0;
	int	numFiles = 0;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

		return localrc;
	}

   int	clientId = ntohl(*((unsigned int*)buf));
	printf("Client ID: %d\n", clientId);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	if (clientId != theCurrentClientId)
	{
		numFiles = 0;
		unsigned int  netNumFiles = htonl(numFiles);

		if (theSocket.write(4, &netNumFiles) != 4)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Unable to write number of files to socket.",
        		&localrc);

			return localrc;
		}

		int				status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		if (theSocket.write(4, &netStatus) != 4)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Unable to write error status to socket.",
        		&localrc);

			return localrc;
		}

     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_ARG_VALUE,
        	"Invalid client id read from socket.",
        	&localrc);

		return localrc;
	}

	//***
	// Write the file information back to the client
	//***
	if (theCurrentStatus == NET_ESMF_STAT_FINAL_DONE)
	{
		numFiles = theOutputFiles.size();
		char	fileInfoBuf[1024];

		unsigned int  netNumFiles = htonl(numFiles);
		if (theSocket.write(4, &netNumFiles) != 4)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Unable to write number of files to socket.",
        		&localrc);

			return localrc;
		}

		for (int i = 0; i < numFiles; ++i)
		{
      	// All of the files are export files
			strcpy(fileInfoBuf, "export");
			int	filenameSize = strlen(fileInfoBuf) + 1;

			if (theSocket.write(filenameSize, fileInfoBuf) != filenameSize)
			{
     			ESMC_LogDefault.ESMC_LogMsgFoundError(
        			ESMC_RC_FILE_WRITE,
        			"Unable to write filename to socket.",
        			&localrc);

				return localrc;
			}

			//strcpy(fileInfoBuf, "camrun.cam2.rh0.000-01-02-00000.nc");
			strcpy(fileInfoBuf, theOutputFiles[i].c_str());
			filenameSize = strlen(fileInfoBuf) + 1;

			if (theSocket.write(filenameSize, fileInfoBuf) != filenameSize)
			{
     			ESMC_LogDefault.ESMC_LogMsgFoundError(
        			ESMC_RC_FILE_WRITE,
        			"Unable to write filename to socket.",
        			&localrc);

				return localrc;
			}
		}
	}
	else
	{
		numFiles = 0;
		unsigned int  netNumFiles = htonl(numFiles);
		if (theSocket.write(4, &netNumFiles) != 4)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Unable to write number of files to socket.",
        		&localrc);

			return localrc;
		}
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	unsigned int	netStatus = htonl(theCurrentStatus);
	if (theSocket.write(4, &netStatus) != 4)
	{
     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_FILE_WRITE,
        	"Unable to write status to socket.",
        	&localrc);

		return localrc;
	}

   return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::processGetData()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::processGetData()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::processGetData(
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
/*
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMC_LogMsgFoundError(
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read variable name from socket.",
         &localrc);

      return localrc;
   }

	strncpy(varName, (char*)buf, 255);
	//printf("Var Name: %s\n", varName);

	/////
	// These next values are read as strings and then converted to double values
	/////
	char	tempValue[256];

	// Read time
	if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.ESMC_LogMsgFoundError(
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read lon value from socket.",
         &localrc);

      return localrc;
   }
	strncpy(tempValue, (char*)buf, 255);
	//printf("Lon: %s\n", tempValue);
	double	lonValue = atof(tempValue);

	/////
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	/////
	map<int, ESMCI_WebServClientInfo*>::iterator		iter;
	ESMCI_WebServClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		numFiles = 0;
		unsigned int  netNumFiles = htonl(numFiles);
		if (theSocket.write(4, &netNumFiles) != 4)
      {
         ESMC_LogDefault.ESMC_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write number of files to socket.",
            &localrc);

         return localrc;
      }

		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.ESMC_LogMsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            &localrc);

         return localrc;
      }

      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         &localrc);

      return localrc;
	}

	clientInfo = iter->second;
	//clientInfo->print();

	/////
	// If the data files (to be added to ClientInfo) have not been retrieved
	// from the component server, then get them.
	// KDS: Right now, I'm hardcoding the output filename...
	/////
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

	/////
	// Write the data back to the client
	/////
	sprintf(tempValue, "%e", dataValue);
	int	valueLen = strlen(tempValue) + 1;

	if (theSocket.write(valueLen, tempValue) != valueLen)
   {
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write data value to socket.",
         &localrc);

      return localrc;
   }

	/////
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	/////
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);

	if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         &localrc);

      return localrc;
   }
*/

	return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::processEnd()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::processEnd()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::processEnd(
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
//    client id from the socket and uses it to validate the client.
//    The component status is updated and written to the socket to complete 
//    the transaction.
//    KDS: I think this method is not necessary any more, since the process
//         will kill the server when it's completed.
//
//EOPI
//-----------------------------------------------------------------------------
{
	printf("\n\nSERVER: processing End\n");
	int	localrc = 0;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	if (theSocket.read(bytesRead, buf) <= 0)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         &localrc);

		return localrc;
	}

   int	clientId = ntohl(*((unsigned int*)buf));
	printf("Client ID: %d\n", clientId);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	if (clientId != theCurrentClientId)
	{
		int				status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);

		if (theSocket.write(4, &netStatus) != 4)
		{
     		ESMC_LogDefault.ESMC_LogMsgFoundError(
        		ESMC_RC_FILE_WRITE,
        		"Unable to write error status to socket.",
        		&localrc);

			return localrc;
		}

     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_ARG_VALUE,
        	"Invalid client id read from socket.",
        	&localrc);

		return localrc;
	}

	setStatus(NET_ESMF_STAT_DONE);

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	unsigned int	netStatus = htonl(theCurrentStatus);
	if (theSocket.write(4, &netStatus) != 4)
	{
     	ESMC_LogDefault.ESMC_LogMsgFoundError(
        	ESMC_RC_FILE_WRITE,
        	"Unable to write status to socket.",
        	&localrc);

		return localrc;
	}

   return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::runInit()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::runInit()
//
// !INTERFACE:
void*  ESMCI_WebServComponentSvr::runInit(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Makes the call to the grid component initialization routine.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("initializing a grid component\n");
	int	localrc = 0;

// Update status on registrar
	//***
	// Make the call to the initialization routine
	//***
	int	rc = 0;
   FTN(f_esmf_processinit)(theGridComp,
                             theImportState, 
                             theExportState, 
                             theClock, 
                             thePhase, 
                             &rc);

	//***
	// Update the status when completed
	//***
	if (rc != ESMF_SUCCESS)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         rc,
         "Error while executing initialization.",
         &localrc);

		setStatus(NET_ESMF_STAT_ERROR);
	}
	else
	{
		setStatus(NET_ESMF_STAT_INIT_DONE);
	}
// Update status on registrar
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::runRun()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::runRun()
//
// !INTERFACE:
void*  ESMCI_WebServComponentSvr::runRun(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Makes the call to the grid component run routine.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("ESMCI_WebServComponentSvr::runRun()\n");
	int	localrc = 0;

	//***
	// Make the call to the initialization routine
	//***
	int	rc = 0;
   FTN(f_esmf_processrun)(theGridComp,
                          theImportState, 
                          theExportState, 
                          theClock, 
                          thePhase, 
                          &rc);

	//***
	// Update the status when completed
	//***
	if (rc != ESMF_SUCCESS)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         rc,
         "Error while executing run.",
         &localrc);

		setStatus(NET_ESMF_STAT_ERROR);
	}
	else
	{
		setStatus(NET_ESMF_STAT_RUN_DONE);
	}
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::runFinal()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::runFinal()
//
// !INTERFACE:
void*  ESMCI_WebServComponentSvr::runFinal(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Makes the call to the grid component finalization routine.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//***
	// KDS: If you want to export the component state out to a file, this is
	//      probably the place to do it. 
	//***
	int	localrc = 0;

	//***
	// Make the call to the initialization routine
	//***
	int	rc = 0;
   FTN(f_esmf_processfinal)(theGridComp,
                            theImportState, 
                            theExportState, 
                            theClock, 
                            thePhase, 
                            &rc);

	//***
	// Update the status when completed
	//***
	if (rc != ESMF_SUCCESS)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         rc,
         "Error while executing finalization.",
         &localrc);

		setStatus(NET_ESMF_STAT_ERROR);
	}
	else
	{
		setStatus(NET_ESMF_STAT_FINAL_DONE);
	}
}


} // end namespace

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "initThreadStartup()"
//BOPI
// !ROUTINE:  initThreadStartup()
//
// !INTERFACE:
void*  initThreadStartup(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  void*  tgtObject	// the component service object
  )
//
// !DESCRIPTION:
//    Function called to run the initialization method for a component service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//***
	// Cast the target object to a component service object
	//***
	ESMCI::ESMCI_WebServComponentSvr*	
		svrObject = (ESMCI::ESMCI_WebServComponentSvr*)tgtObject;

	//***
	// Call the initialization method
	//***
	svrObject->runInit();
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "runThreadStartup()"
//BOPI
// !ROUTINE:  runThreadStartup()
//
// !INTERFACE:
void*  runThreadStartup(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  void*  tgtObject	// the component service object
  )
//
// !DESCRIPTION:
//    Function called to run the run method for a component service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//***
	// Cast the target object to a component service object
	//***
	ESMCI::ESMCI_WebServComponentSvr*	
		svrObject = (ESMCI::ESMCI_WebServComponentSvr*)tgtObject;

	//***
	// Call the run method
	//***
	svrObject->runRun();
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "finalThreadStartup()"
//BOPI
// !ROUTINE:  finalThreadStartup()
//
// !INTERFACE:
void*  finalThreadStartup(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  void*  tgtObject	// the component service object
  )
//
// !DESCRIPTION:
//    Function called to run the finalization method for a component service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//***
	// Cast the target object to a component service object
	//***
	ESMCI::ESMCI_WebServComponentSvr*	
		svrObject = (ESMCI::ESMCI_WebServComponentSvr*)tgtObject;

	//***
	// Call the finalization method
	//***
	svrObject->runFinal();
}
