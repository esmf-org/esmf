// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
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
//        implemented using the ESMCI_WebServProcCtrl.)
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServComponentSvr.h"

#include <stdint.h>

#if !defined (ESMF_OS_MinGW)
#include <arpa/inet.h>
#include <netdb.h>
#else
#include <Winsock.h>
#endif

#include "ESMCI_WebServSocketUtils.h"
#include "ESMCI_WebServRegistrarClient.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"


//***
// KDS: I think this section is going to have to move to a new file in the
//      interface directory
//***
extern "C"
{
        void FTN_X(f_esmf_processinit)(ESMCI::GridComp*  comp,
                                ESMCI::State*     importState,
                                ESMCI::State*     exportState,
                                ESMCI::Clock*     clock,
                                int               phase,
                                int*              rc);

        void FTN_X(f_esmf_processrun)(ESMCI::GridComp*  comp,
                               ESMCI::State*     importState,
                               ESMCI::State*     exportState,
                               ESMCI::Clock*     clock,
                               int               phase,
                               int*              rc);

        void FTN_X(f_esmf_processfinal)(ESMCI::GridComp*  comp,
                                 ESMCI::State*     importState,
                                 ESMCI::State*     exportState,
                                 ESMCI::Clock*     clock,
                                 int               phase,
                                 int*              rc);

        void FTN_X(f_esmf_cplcompprocessinit)(ESMCI::CplComp*  comp,
                                         ESMCI::State*    importState,
                                         ESMCI::State*    exportState,
                                         ESMCI::Clock*    clock,
                                         int              phase,
                                         int*             rc);

        void FTN_X(f_esmf_cplcompprocessrun)(ESMCI::CplComp*  comp,
                                        ESMCI::State*    importState,
                                        ESMCI::State*    exportState,
                                        ESMCI::Clock*    clock,
                                        int              phase,
                                        int*             rc);

        void FTN_X(f_esmf_cplcompprocessfinal)(ESMCI::CplComp*  comp,
                                          ESMCI::State*    importState,
                                          ESMCI::State*    exportState,
                                          ESMCI::Clock*    clock,
                                          int              phase,
                                          int*             rc);

        void*  initThreadStartup(void*);
        void*  runThreadStartup(void*);
        void*  timeStepThreadStartup(void*);
        void*  finalThreadStartup(void*);
};

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
  int  port,       // (in) the port number on which to setup the socket service
                   // to listen for requests
  int  clientId,   // (in) the id of the client for whom this service is
                   // being run
  string  registrarHost  // (in) name of host one which registrar is running
  )
//
// !DESCRIPTION:
//    Initialize the ESMF Component service with the default values as well
//    as the specified port number.
//
//EOPI
//-----------------------------------------------------------------------------
{
        int     localrc = 0;

        //***
        // Initialize the status mutex and output data mutex
        //***
#ifndef ESMF_NO_PTHREADS
        if (pthread_mutex_init(&theStatusMutex, NULL) != 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while initializing status mutex lock... behavior unknown.",
         ESMC_CONTEXT, &localrc);
        }

        if (pthread_mutex_init(&theDataMutex, NULL) != 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while initializing data mutex lock... behavior unknown.",
         ESMC_CONTEXT, &localrc);
        }
#endif

        //***
        // Set the data members
        //***
        setPort(port);
   theCurrentClientId = clientId;
printf("Setting CurrentClientId: %d\n", clientId);
   theRegistrarHost = registrarHost;
printf("Setting RegistrarHost: %s\n", registrarHost.c_str());

        setStatus(NET_ESMF_STAT_READY);

        string  varNames[] = { "temp", "wind", "precip", "solarrad", "relhumid" };
        double  latValues[] = { -90, -80, -70, -60, -50, -40, -30, -20, -10, 0,
                            10, 20, 30, 40, 50, 60, 70, 80, 90 };
        double  lonValues[] = { 0, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150,
                            165, 180, 195, 210, 225, 240, 255, 270, 285, 300,
                            315, 330, 345 };

        theOutputData = new ESMCI_WebServDataMgr(5, varNames,
                                            19, latValues,
                                            24, lonValues);
/*
        theOutputData = NULL;
*/
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
        int     localrc = 0;

        theSocket.disconnect();

#ifndef ESMF_NO_PTHREADS
        if (pthread_mutex_destroy(&theStatusMutex) != 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while destroying status mutex lock.",
         ESMC_CONTEXT, &localrc);
        }

        if (pthread_mutex_destroy(&theDataMutex) != 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while destroying data mutex lock.",
         ESMC_CONTEXT, &localrc);
        }
#endif
}


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
#define ESMC_METHOD "ESMCI_WebServComponentSvr::setOutputDesc()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::setOutputDesc()
//
// !INTERFACE:
void  ESMCI_WebServComponentSvr::setOutputDesc(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  ESMCI_WebServDataDesc*  desc  // (in) description of output data
  )
//
// !DESCRIPTION:
//    Allocates the output data structure and sets up its description.
//
//EOPI
//-----------------------------------------------------------------------------
{

        string  varNames[] = { "temp", "wind", "precip", "solarrad", "relhumid" };
        double  latValues[] = { -90, -80, -70, -60, -50, -40, -30, -20, -10, 0,
                            10, 20, 30, 40, 50, 60, 70, 80, 90 };
        double  lonValues[] = { 0, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150,
                            165, 180, 195, 210, 225, 240, 255, 270, 285, 300,
                            315, 330, 345 };

        theOutputData = new ESMCI_WebServDataMgr(5, varNames,
                                            19, latValues,
                                            24, lonValues);
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
#define ESMC_METHOD "ESMCI_WebServComponentSvr::addOutputData()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::addOutputData()
//
// !INTERFACE:
void  ESMCI_WebServComponentSvr::addOutputData(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
// TODO: Change structure to support output data format
  string  filename    // the name of the output filename to add to the list
  )
//
// !DESCRIPTION:
//    Adds the specified data information to the list of output data.
//
//EOPI
//-----------------------------------------------------------------------------
{
        // TODO: Add output data to list of output data
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
//    client makes requests of the server to initialize, run, and finalize.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("ESMCI_WebServComponentSvr::grid requestLoop()\n");

        int     localrc = 0;

   //***
   // Save the input parameters... these are used later when the client
   // wants to execute the initialize, run and finalize procedures
   //***
        theGridComp     = comp;
        theImportState  = importState;
        theExportState  = exportState;
        theClock               = clock;
        thePhase        = phase;
        theBlockingFlag = blockingFlag;
        theCompType     = ESMC_WEBSERVCOMPTYPE_GRID;
        
        //comp->print("");

   //***
   // Setup the server socket
   //***
        if (theSocket.connect(thePort) < 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Connection error for the server socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
        }

   //***
   // Enter into a loop that waits for a client request and processes the
   // requests as they come in.  This loop continues until the client sends
   // an exit request (this isn't currently used).
   //***
        int     request;

        do
        {
                request = getNextRequest();
//printf("Request ID: %d\n", request);

                if (request == ESMF_FAILURE)
                {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_ARG_VALUE,
                "Request ID not valid.",
                ESMC_CONTEXT, &localrc);

        return ESMF_FAILURE;
                }

                serviceRequest(request);

        } while (request != NET_ESMF_EXIT);

        return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::cplCompRequestLoop()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::cplCompRequestLoop()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::cplCompRequestLoop(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  ESMCI::CplComp*    comp,          // (in) the coupler component
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
//    client makes requests of the server to initialize, run, and finalize.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("ESMCI_WebServComponentSvr::grid requestLoop()\n");

        int     localrc = 0;

   //***
   // Save the input parameters... these are used later when the client
   // wants to execute the initialize, run and finalize procedures
   //***
        theCplComp      = comp;
        theImportState  = importState;
        theExportState  = exportState;
        theClock               = clock;
        thePhase        = phase;
        theBlockingFlag = blockingFlag;
        theCompType     = ESMC_WEBSERVCOMPTYPE_COUPLER;
        
        //comp->print("");

   //***
   // Setup the server socket
   //***
        if (theSocket.connect(thePort) < 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Connection error for the server socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
        }

   //***
   // Enter into a loop that waits for a client request and processes the
   // requests as they come in.  This loop continues until the client sends
   // an exit request (this isn't currently used).
   //***
        int     request;

        do
        {
                request = getNextRequest();
//printf("Request ID: %d\n", request);

                if (request == ESMF_FAILURE)
                {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_ARG_VALUE,
                "Request ID not valid.",
                ESMC_CONTEXT, &localrc);

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

        int     localrc = 0;

   //***
   // Wait for client requests
   //***
        if (theSocket.accept() < 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "The Server socket not accepting requests.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
        }

   //***
   // Read the request id string from the socket
   //***
        int     n;
        char    requestStr[50];

        if (theSocket.read(n, requestStr) <= 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read request id from socket.",
         ESMC_CONTEXT, &localrc);

                //return localrc;
                return ESMF_FAILURE;
        }

        //printf("SERVER: request: %s\n", requestStr);

   //***
   // Convert the string to a valid request id and return it
   //***
        return ESMCI_WebServGetRequestId(requestStr);
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
        //printf("Request: %d\n", request);

        strcpy(theMsg, "OK");

        switch (request)
        {
        case NET_ESMF_INIT:
                processInit();
                break;

        case NET_ESMF_RUN:
                processRun();
                break;

        case NET_ESMF_TIMESTEP:
                processTimestep();
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

        case NET_ESMF_DATA_DESC:
                processGetDataDesc();
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
        int     localrc = 0;

#ifndef ESMF_NO_PTHREADS
        if (pthread_mutex_lock(&theStatusMutex) != 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while locking current status mutex lock... behavior unknown.",
         ESMC_CONTEXT, &localrc);
        }
#endif

        theCurrentStatus = status;

#ifndef ESMF_NO_PTHREADS
        if (pthread_mutex_unlock(&theStatusMutex) != 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_OBJ_WRONG,
         "Error while unlocking current status mutex lock... behavior unknown.",
         ESMC_CONTEXT, &localrc);
        }
#endif

//   ESMCI::ESMCI_WebServRegistrarClient registrar("localhost", REGISTRAR_PORT);
   ESMCI::ESMCI_WebServRegistrarClient registrar(theRegistrarHost.c_str(),
                                                 REGISTRAR_PORT);

        char    idStr[64];
        sprintf(idStr, "%d", theCurrentClientId);

   if (registrar.setStatus(idStr, registrar.getStateStr(theCurrentStatus)) ==
                ESMF_FAILURE)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_UNEXPECTED,
         "Error setting status on Registrar.",
         ESMC_CONTEXT, &localrc);
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
        int     localrc = 0;

        //***
        // Get the client id
        //***
        int     bytesRead = 0;
        char    buf[1024];

        if (theSocket.read(bytesRead, buf) <= 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         ESMC_CONTEXT, &localrc);

                return localrc;
        }

//   theCurrentClientId = ntohl(*((unsigned int*)buf));
//      printf("SERVER: Client ID: %d\n", theCurrentClientId);

   int  clientId = ntohl(*((unsigned int*)buf));
        printf("Client ID: %d\n", clientId);

        if (clientId != theCurrentClientId)
        {
                int                             status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);

                setStatus(NET_ESMF_STAT_ERROR);

                if (theSocket.write(4, &netStatus) != 4)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write error status to socket.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }

        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_ARG_VALUE,
                "Invalid client id read from socket.",
                ESMC_CONTEXT, &localrc);

                return localrc;
        }

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
                pthread_t       thread;
                int                     rc = 0;

                if ((rc = pthread_create(&thread, NULL, initThreadStartup, this)) != 0)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Error creating initialize thread.",
                        ESMC_CONTEXT, &localrc);

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
        unsigned int    netStatus = htonl(theCurrentStatus);
        if (theSocket.write(4, &netStatus) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write status to socket.",
                ESMC_CONTEXT, &localrc);

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
        int     localrc = 0;

        //***
        // Get the client id
        //***
        int     bytesRead = 0;
        char    buf[1024];

        if (theSocket.read(bytesRead, buf) <= 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         ESMC_CONTEXT, &localrc);

                return localrc;
        }

   int  clientId = ntohl(*((unsigned int*)buf));
        printf("Client ID: %d\n", clientId);

        if (clientId != theCurrentClientId)
        {
                int                             status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);

                setStatus(NET_ESMF_STAT_ERROR);

                if (theSocket.write(4, &netStatus) != 4)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write error status to socket.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }

        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_ARG_VALUE,
                "Invalid client id read from socket.",
                ESMC_CONTEXT, &localrc);

                return localrc;
        }

        printf("Current Status: %d\n", theCurrentStatus);
        if ((theCurrentStatus == NET_ESMF_STAT_INIT_DONE) ||
            (theCurrentStatus == NET_ESMF_STAT_TIMESTEP_DONE))
        {
                //***
                // Call the component run
                //***
                setStatus(NET_ESMF_STAT_RUNNING);

#ifndef ESMF_NO_PTHREADS
                pthread_t       thread;
                int                     rc = 0;

                if ((rc = pthread_create(&thread, NULL, runThreadStartup, this)) != 0)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Error creating run thread.",
                        ESMC_CONTEXT, &localrc);

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
        unsigned int    netStatus = htonl(theCurrentStatus);
        if (theSocket.write(4, &netStatus) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write status to socket.",
                ESMC_CONTEXT, &localrc);

                return localrc;
        }

   return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::processTimestep()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::processTimestep()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::processTimestep(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to run the component for the specified number of
//    timesteps.  This method reads the client id from the socket and uses it
//    to validate the client information.  It then gets the value for the
//    number of timesteps to run from the client.
//    It creates a new thread which is responsible for calling the
//    component run routine and writing the component status to the socket
//    to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("\n\nSERVER: processing Timestep\n");
        int     localrc = 0;

        //***
        // Get the client id
        //***
        int     bytesRead = 0;
        char    buf[1024];

        if (theSocket.read(bytesRead, buf) <= 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         ESMC_CONTEXT, &localrc);

                return localrc;
        }

   int  clientId = ntohl(*((unsigned int*)buf));
        printf("Client ID: %d\n", clientId);

        if (clientId != theCurrentClientId)
        {
                int                             status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);

                setStatus(NET_ESMF_STAT_ERROR);

                if (theSocket.write(4, &netStatus) != 4)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write error status to socket.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }

        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_ARG_VALUE,
                "Invalid client id read from socket.",
                ESMC_CONTEXT, &localrc);

                return localrc;
        }

        //***
        // Read the number of timesteps to run from the client
        //***
        bytesRead = 0;

        if (theSocket.read(bytesRead, buf) <= 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read number of timesteps from socket.",
         ESMC_CONTEXT, &localrc);

                return localrc;
        }

   int  numTimesteps = ntohl(*((unsigned int*)buf));
        printf("Num Timesteps: %d\n", numTimesteps);

        //***
        // Execute the run for the specified number of timesteps.  If pthreads
        // are supported, do this step in a separate thread so as to not continue
        // blocking the socket service.
        //***
        printf("Current Status: %d\n", theCurrentStatus);
        if ((theCurrentStatus == NET_ESMF_STAT_INIT_DONE)  ||
            (theCurrentStatus == NET_ESMF_STAT_TIMESTEP_DONE))
        {
                //***
                // Call the component run
                //***
                setStatus(NET_ESMF_STAT_RUNNING);

/*
#ifndef ESMF_NO_PTHREADS
                pthread_t       thread;
                int                     rc = 0;

                if ((rc =
                        pthread_create(&thread, NULL, timeStepThreadStartup, this)) != 0)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Error creating timestep thread.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }
#else
                runTimeStep();
#endif
*/
                runTimeStep();
        }

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
        unsigned int    netStatus = htonl(theCurrentStatus);
        if (theSocket.write(4, &netStatus) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write status to socket.",
                ESMC_CONTEXT, &localrc);

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
        int     localrc = 0;

        //***
        // Get the client id
        //***
        int     bytesRead = 0;
        char    buf[1024];

        if (theSocket.read(bytesRead, buf) <= 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         ESMC_CONTEXT, &localrc);

                return localrc;
        }

   int  clientId = ntohl(*((unsigned int*)buf));
        printf("Client ID: %d\n", clientId);

        if (clientId != theCurrentClientId)
        {
                int                             status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);

                setStatus(NET_ESMF_STAT_ERROR);

                if (theSocket.write(4, &netStatus) != 4)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write error status to socket.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }

        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_ARG_VALUE,
                "Invalid client id read from socket.",
                ESMC_CONTEXT, &localrc);

                return localrc;
        }

        if ((theCurrentStatus == NET_ESMF_STAT_INIT_DONE)  ||
            (theCurrentStatus == NET_ESMF_STAT_TIMESTEP_DONE)  ||
            (theCurrentStatus == NET_ESMF_STAT_RUN_DONE))
        {
                //***
                // Set the status to indicate that the service is busy right now
                //***
                setStatus(NET_ESMF_STAT_FINALIZING);

#ifndef ESMF_NO_PTHREADS
                pthread_t       thread;
                int                     rc = 0;
                if ((rc = pthread_create(&thread, NULL, finalThreadStartup, this)) != 0)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Error creating finalize thread.",
                        ESMC_CONTEXT, &localrc);

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
        unsigned int    netStatus = htonl(theCurrentStatus);
        if (theSocket.write(4, &netStatus) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write status to socket.",
                ESMC_CONTEXT, &localrc);

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
        //printf("\n\nSERVER: processing State\n");
        int     localrc = 0;

        //***
        // Get the client id
        //***
        int     bytesRead = 0;
        char    buf[1024];

        if (theSocket.read(bytesRead, buf) <= 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         ESMC_CONTEXT, &localrc);

                return localrc;
        }

   int  clientId = ntohl(*((unsigned int*)buf));
        //printf("Client ID: %d\n", clientId);

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
        printf("The Current Status: %d\n", theCurrentStatus);
        unsigned int    netStatus = htonl(theCurrentStatus);

        if (theSocket.write(4, &netStatus) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write status to socket.",
                ESMC_CONTEXT, &localrc);

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

        int     localrc = 0;
        int     numFiles = 0;

        //***
        // Get the client id
        //***
        int     bytesRead = 0;
        char    buf[1024];

        if (theSocket.read(bytesRead, buf) <= 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         ESMC_CONTEXT, &localrc);

                return localrc;
        }

   int  clientId = ntohl(*((unsigned int*)buf));
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
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write number of files to socket.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }

                int                             status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);

                if (theSocket.write(4, &netStatus) != 4)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write error status to socket.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }

        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_ARG_VALUE,
                "Invalid client id read from socket.",
                ESMC_CONTEXT, &localrc);

                return localrc;
        }

        //***
        // Write the file information back to the client
        //***
        if (theCurrentStatus == NET_ESMF_STAT_FINAL_DONE)
        {
                numFiles = theOutputFiles.size();
                char    fileInfoBuf[1024];

                unsigned int  netNumFiles = htonl(numFiles);
                if (theSocket.write(4, &netNumFiles) != 4)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write number of files to socket.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }

                for (int i = 0; i < numFiles; ++i)
                {
        // All of the files are export files
                        strcpy(fileInfoBuf, "export");
                        int     filenameSize = strlen(fileInfoBuf) + 1;

                        if (theSocket.write(filenameSize, fileInfoBuf) != filenameSize)
                        {
                        ESMC_LogDefault.MsgFoundError(
                                ESMC_RC_FILE_WRITE,
                                "Unable to write filename to socket.",
                                ESMC_CONTEXT, &localrc);

                                return localrc;
                        }

                        //strcpy(fileInfoBuf, "camrun.cam2.rh0.000-01-02-00000.nc");
                        strcpy(fileInfoBuf, theOutputFiles[i].c_str());
                        filenameSize = strlen(fileInfoBuf) + 1;

                        if (theSocket.write(filenameSize, fileInfoBuf) != filenameSize)
                        {
                        ESMC_LogDefault.MsgFoundError(
                                ESMC_RC_FILE_WRITE,
                                "Unable to write filename to socket.",
                                ESMC_CONTEXT, &localrc);

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
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write number of files to socket.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }
        }

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
        unsigned int    netStatus = htonl(theCurrentStatus);
        if (theSocket.write(4, &netStatus) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write status to socket.",
                ESMC_CONTEXT, &localrc);

                return localrc;
        }

   return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::processGetDataDesc()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::processGetDataDesc()
//
// !INTERFACE:
int  ESMCI_WebServComponentSvr::processGetDataDesc(
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
//    lat and lon) from the socket and uses that information to read the
//    data from the socket.  The data and the component status are then
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
        printf("\n\nSERVER: processing GetDataDesc\n");

        int     localrc = 0;
        int     status = NET_ESMF_STAT_IDLE;
        int     dataSize = 0;

        //***
        // Get the client id
        //***
        int     bytesRead = 0;
        char    buf[1024];

        if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

   int  clientId = ntohl(*((unsigned int*)buf));
        printf("Client ID: %d\n", clientId);

        //***
        // Now that everything's been read off the socket, make sure that the
        // client id matches the client id for this component server.  If it
        // doesn't match, then send back an error.  Also...
        //
        // If the output data is null, then the component hasn't set it up,so
        // send back zeros for the number of variables, number of lats, and number
        // of lons.  This should be the indicator to the client that the data
        // hasn't be defined yet.
        //***
        if ((clientId != theCurrentClientId)  ||  (theOutputData == NULL))
        {
                unsigned int    numVars = htonl(0);
                unsigned int    numLats = htonl(0);
                unsigned int    numLons = htonl(0);

                if (theSocket.write(4, &numVars) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write zero num vars to socket.",
            ESMC_CONTEXT, &localrc);
      }

                if (theSocket.write(4, &numLats) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write zero num lats to socket.",
            ESMC_CONTEXT, &localrc);
      }

                if (theSocket.write(4, (unsigned int*)(&numLons)) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write zero num lons to socket.",
            ESMC_CONTEXT, &localrc);
      }
        }
        //***
        // Otherwise, send back the data description
        //***
        else
        {
                //***
                // Return the description of the variables and the grid information
                // (i.e., the variable names and the number of lat/lon values)
                //***
                int     numVars = theOutputData->getNumVars();
                unsigned int  netNumVars = htonl(numVars);
                if (theSocket.write(4, &netNumVars) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write num vars to socket.",
                ESMC_CONTEXT, &localrc);

        return localrc;
        }

                string*         varNames = theOutputData->getVarNames();

                for (int i = 0; i < numVars; ++i)
                {
                        int             valueLen = varNames[i].length() + 1;
        
                        if (theSocket.write(valueLen,
                             (void*)(varNames[i].c_str())) != valueLen)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write variable name to socket.",
                        ESMC_CONTEXT, &localrc);

                return localrc;
                }
                }

                int     numLats = theOutputData->getNumLatValues();
                unsigned int  netNumLats = htonl(numLats);
                if (theSocket.write(4, &netNumLats) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write num lats to socket.",
                ESMC_CONTEXT, &localrc);
        
        return localrc;
        }
        
                double*         latValues = theOutputData->getLatValues();
                double*         netLatValues = new double[numLats];
                int             latDataSize = sizeof(double) * numLats;
        
                for (int i = 0; i < numLats; ++i)
                {
                        netLatValues[i] = latValues[i];
                        htonll((uint64_t)(netLatValues[i]));
                }

                if (theSocket.write(latDataSize, (void*)netLatValues) != latDataSize)
        {
        ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write latitude values to socket.",
                        ESMC_CONTEXT, &localrc);
        
                return localrc;
                }
        
                int     numLons = theOutputData->getNumLonValues();
                unsigned int  netNumLons = htonl(numLons);
        
                if (theSocket.write(4, &netNumLons) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write num lons to socket.",
                ESMC_CONTEXT, &localrc);
        
        return localrc;
        }

        
                double*         lonValues = theOutputData->getLonValues();
                double*         netLonValues = new double[numLons];
                int             lonDataSize = sizeof(double) * numLons;
        
                for (int i = 0; i < numLons; ++i)
                {
                        netLonValues[i] = lonValues[i];
                        htonll((uint64_t)(netLonValues[i]));
                }

                if (theSocket.write(lonDataSize, (void*)netLonValues) != lonDataSize)
        {
        ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write longitude values to socket.",
                        ESMC_CONTEXT, &localrc);
        
                return localrc;
                }
        }

        //***
        // If the client id didn't match the current client id, then send back
        // an error status and log the error.
        //***
        if (clientId != theCurrentClientId)
        {
                status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);
                if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return localrc;
      }

      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
        }
        //***
        // Otherwise, send the current state back to the client (use the return
        // code from the component initialize call to determine the state)
        //***
        else
        {
                unsigned int    netStatus = htonl(theCurrentStatus);
                if (theSocket.write(4, &netStatus) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write status to socket.",
                ESMC_CONTEXT, &localrc);

        return localrc;
        }
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
//    lat and lon) from the socket and uses that information to read the
//    data from the socket.  The data and the component status are then
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
        printf("\n\nSERVER: processing GetData\n");

        int     localrc = 0;
        int     status = NET_ESMF_STAT_IDLE;
        int     dataSize = 0;

        //***
        // Get the client id
        //***
        int     bytesRead = 0;
        char    buf[1024];

        if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

   int  clientId = ntohl(*((unsigned int*)buf));
        printf("Client ID: %d\n", clientId);

        //***
        // Read the timestamp for the requested data
        //***
        if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read time value from socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

        printf("Bytes read: %d\n", bytesRead);

        double*         timeValue = (double*)buf;
        ntohll((uint64_t)(*timeValue));
printf("%g\n", *timeValue);

        //***
        // Now that everything's been read off the socket, make sure that the
        // client id matches the client id for this component server.  If it
        // doesn't match, then send back an error
        //***
        if (clientId != theCurrentClientId)
        {
                status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);
                if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return localrc;
      }

      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
        }

        //***
        // If the output data is null, then the component hasn't set it up,so
        // send back a data size of zero indicating that no data is currently
        // available.
        //***
        if (theOutputData == NULL)
        {
                status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);
                if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return localrc;
      }

      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Output data not defined.",
         ESMC_CONTEXT, &localrc);

      return localrc;
        }

        int             numVars   = theOutputData->getNumVars();
        string*         varNames  = theOutputData->getVarNames();
        int             numLats   = theOutputData->getNumLatValues();
        int             numLons   = theOutputData->getNumLonValues();

        ESMCI_WebServDataContent*       
                dataContent = theOutputData->getDataValues(*timeValue);

        //***
        // Write out the number of variables in the output data (this must match
        // the value returned in the get output data desc call).
        //***
        unsigned int  netNumVars = htonl(numVars);
        if (theSocket.write(4, &netNumVars) != 4)
   {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write num vars to socket.",
                ESMC_CONTEXT, &localrc);

        return localrc;
   }

        //***
        // Now, write out the variable names
        //***
        for (int i = 0; i < numVars; ++i)
        {
                int             valueLen = varNames[i].length() + 1;
        
                if (theSocket.write(valueLen, (void*)(varNames[i].c_str())) != valueLen)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write variable name to socket.",
                        ESMC_CONTEXT, &localrc);

                return localrc;
                }
        }

        //***
        // Write out the number of latitude values in the output data
        //***
        unsigned int  netNumLats = htonl(numLats);
        if (theSocket.write(4, &netNumLats) != 4)
   {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write num lats to socket.",
                ESMC_CONTEXT, &localrc);

        return localrc;
   }

        //***
        // Write out the number of longitude values in the output data
        //***
        unsigned int  netNumLons = htonl(numLons);
        if (theSocket.write(4, &netNumLons) != 4)
   {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write num lons to socket.",
                ESMC_CONTEXT, &localrc);

        return localrc;
   }

        //***
        // For each variable, write out the data
        //***
        for (int i = 0; i < numVars; ++i)
        {
                double*         dataValues = dataContent->getDataValues(varNames[i]);
                int             numValues = numLats * numLons;
                double*         netDataValues = new double[numValues];

printf("Num Values: %d\n", numValues);
                for (int j = 0; j < numValues; ++j)
                {
//printf("Data Value[%d][%d]: %g\n", i, j, dataValues[j]);
                        netDataValues[j] = dataValues[j];
                        ntohll((uint64_t)(netDataValues[j]));
                }

                int     dataSize = sizeof(double) * numValues;
                int     bytesSent = 0;

                if ((bytesSent = theSocket.write(dataSize, (void*)netDataValues)) !=
                        dataSize)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write data values to socket.",
                        ESMC_CONTEXT, &localrc);

                return localrc;
        }
        }

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
        unsigned int    netStatus = htonl(theCurrentStatus);
        if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

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
        int     localrc = 0;

        //***
        // Get the client id
        //***
        int     bytesRead = 0;
        char    buf[1024];

        if (theSocket.read(bytesRead, buf) <= 0)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client id from socket.",
         ESMC_CONTEXT, &localrc);

                return localrc;
        }

   int  clientId = ntohl(*((unsigned int*)buf));
        printf("Client ID: %d\n", clientId);

        //***
        // Now that everything's been read off the socket, lookup the client info
        // based on the client id.  If the client can't be found, then send back
        // an error
        //***
        if (clientId != theCurrentClientId)
        {
                int                             status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);

                if (theSocket.write(4, &netStatus) != 4)
                {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Unable to write error status to socket.",
                        ESMC_CONTEXT, &localrc);

                        return localrc;
                }

        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_ARG_VALUE,
                "Invalid client id read from socket.",
                ESMC_CONTEXT, &localrc);

                return localrc;
        }

        setStatus(NET_ESMF_STAT_DONE);

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
        unsigned int    netStatus = htonl(theCurrentStatus);
        if (theSocket.write(4, &netStatus) != 4)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Unable to write status to socket.",
                ESMC_CONTEXT, &localrc);

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
void  ESMCI_WebServComponentSvr::runInit(
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
        int     localrc = 0;

// Update status on registrar
        //***
        // Make the call to the initialization routine
        //***
        int     rc = 0;

        if (theCompType == ESMC_WEBSERVCOMPTYPE_GRID)
        {
        FTN_X(f_esmf_processinit)(theGridComp,
                                theImportState,
                                theExportState,
                                theClock,
                                thePhase,
                                &rc);
        }
        else
        {
      FTN_X(f_esmf_cplcompprocessinit)(theCplComp,
                                       theImportState,
                                       theExportState,
                                       theClock,
                                       thePhase,
                                       &rc);
        }

        //***
        // Update the status when completed
        //***
        if (rc != ESMF_SUCCESS)
        {
      ESMC_LogDefault.MsgFoundError(
         rc,
         "Error while executing initialization.",
         ESMC_CONTEXT, &localrc);

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
void  ESMCI_WebServComponentSvr::runRun(
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
        int     localrc = 0;

        //***
        // Make the call to the initialization routine
        //***
        int     rc = 0;

        if (theCompType == ESMC_WEBSERVCOMPTYPE_GRID)
        {
      FTN_X(f_esmf_processrun)(theGridComp,
                               theImportState,
                               theExportState,
                               theClock,
                               thePhase,
                               &rc);
        }
        else
        {
      FTN_X(f_esmf_cplcompprocessrun)(theCplComp,
                                      theImportState,
                                      theExportState,
                                      theClock,
                                      thePhase,
                                      &rc);
        }

        //***
        // Update the status when completed
        //***
        if (rc != ESMF_SUCCESS)
        {
      ESMC_LogDefault.MsgFoundError(
         rc,
         "Error while executing run.",
         ESMC_CONTEXT, &localrc);

                setStatus(NET_ESMF_STAT_ERROR);
        }
        else
        {
                setStatus(NET_ESMF_STAT_RUN_DONE);
        }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::runTimeStep()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::runTimeStep()
//
// !INTERFACE:
void  ESMCI_WebServComponentSvr::runTimeStep(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Makes the call to the grid component timestep routine.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("ESMCI_WebServComponentSvr::runTimeStep()\n");
        int     localrc = 0;

        //***
        // Make the call to the run a timestep routine
        //***
        int     rc = 0;

        if (theCompType == ESMC_WEBSERVCOMPTYPE_GRID)
        {
      FTN_X(f_esmf_processrun)(theGridComp,
                               theImportState,
                               theExportState,
                               theClock,
                               thePhase,
                               &rc);
        }
        else
        {
      FTN_X(f_esmf_cplcompprocessrun)(theCplComp,
                                      theImportState,
                                      theExportState,
                                      theClock,
                                      thePhase,
                                      &rc);
        }

        //***
        // Update the status when completed
        //***
        if (rc != ESMF_SUCCESS)
        {
      ESMC_LogDefault.MsgFoundError(
         rc,
         "Error while executing run.",
         ESMC_CONTEXT, &localrc);

                setStatus(NET_ESMF_STAT_ERROR);
        }
        else
        {
/*
                setStatus(NET_ESMF_STAT_RUN_DONE);
*/
                setStatus(NET_ESMF_STAT_TIMESTEP_DONE);
        }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServComponentSvr::runFinal()"
//BOPI
// !ROUTINE:  ESMCI_WebServComponentSvr::runFinal()
//
// !INTERFACE:
void  ESMCI_WebServComponentSvr::runFinal(
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
        int     localrc = 0;

        //***
        // Make the call to the initialization routine
        //***
        int     rc = 0;

   if (theCompType == ESMC_WEBSERVCOMPTYPE_GRID)
   {
      FTN_X(f_esmf_processfinal)(theGridComp,
                                 theImportState,
                                 theExportState,
                                 theClock,
                                 thePhase,
                                 &rc);
   }
   else
   {
      FTN_X(f_esmf_cplcompprocessfinal)(theCplComp,
                                        theImportState,
                                        theExportState,
                                        theClock,
                                        thePhase,
                                        &rc);
   }

        //***
        // Update the status when completed
        //***
        if (rc != ESMF_SUCCESS)
        {
      ESMC_LogDefault.MsgFoundError(
         rc,
         "Error while executing finalization.",
         ESMC_CONTEXT, &localrc);

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
  void*  tgtObject      // the component service object
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

   return NULL;
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
  void*  tgtObject      // the component service object
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

   return NULL;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "timeStepThreadStartup()"
//BOPI
// !ROUTINE:  timeStepThreadStartup()
//
// !INTERFACE:
void*  timeStepThreadStartup(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  void*  tgtObject      // the component service object
  )
//
// !DESCRIPTION:
//    Function called to run the timestep method for a component service.
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
        // Call the timeStep method
        //***
        svrObject->runTimeStep();

   return NULL;
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
  void*  tgtObject      // the component service object
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

   return NULL;
}
