// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServNetEsmfServer.C"
//==============================================================================
//
// ESMC WebServNetEsmfServer method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ NetEsmfServer methods declared
// in the companion file ESMCI_WebServNetEsmfServer.h.  This code
// provides the functionality needed to implement an ESMF component (grid
// or coupler) as a network-accessible service.
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServNetEsmfServer.h"

#if !defined(ESMF_OS_MinGW)
#include <netdb.h>
#else
#include <Winsock.h>
#endif
#include <fstream>

#include "ESMCI_WebServSocketUtils.h"
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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::ESMCI_WebServNetEsmfServer()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::ESMCI_WebServNetEsmfServer()
//
// !INTERFACE:
ESMCI_WebServNetEsmfServer::ESMCI_WebServNetEsmfServer(
//
//
// !ARGUMENTS:
//
  int  port             // (in) the port number on which to setup the socket service
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
        theStatus = (char*)NET_ESMF_STAT_IDLE;
        theNextClientId = 101;

        setPort(port);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::~ESMCI_WebServNetEsmfServer()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::~ESMCI_WebServNetEsmfServer()
//
// !INTERFACE:
ESMCI_WebServNetEsmfServer::~ESMCI_WebServNetEsmfServer(
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
        theSocket.disconnect();
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::setPort()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::setPort()
//
// !INTERFACE:
void  ESMCI_WebServNetEsmfServer::setPort(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  port             // (in) number of the port on which component service listens
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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::requestLoop()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::requestLoop()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::requestLoop(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  ESMCI::GridComp*   comp,                              // (in) the grid component
  ESMCI::State*      importState,       // (in) import state
  ESMCI::State*      exportState,       // (in) export state
  ESMCI::Clock*      clock,                     // (in) clock
  int                phase,                     // (in) phase
  ESMC_BlockingFlag  blockingFlag       // (in) blocking flag
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
        printf("NetEsmfServer::grid requestLoop()\n");

        int     localrc = 0;

        //***
        // Save the input parameters... these are used later when the client
        // wants to execute the initialize, run and finalize procedures
        //***
        theCompType = ESMC_COMPTYPE_GRID;

        theGridComp = comp;
        theImportState = importState;
        theExportState = exportState;
        theClock        = clock;
        thePhase = phase;
        theBlockingFlag = blockingFlag;
        
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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::requestLoop()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::requestLoop()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::requestLoop(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  ESMCI::CplComp*    comp,                              // (in) the coupler component
  ESMCI::State*      importState,       // (in) import state
  ESMCI::State*      exportState,       // (in) export state
  ESMCI::Clock*      clock,                     // (in) clock
  int                phase,                     // (in) phase
  ESMC_BlockingFlag  blockingFlag       // (in) blocking flag
  )
//
// !DESCRIPTION:
//    Sets up a socket service for a coupler component server to handle client
//    requests.  The input parameters are all saved for later use when the
//    client makes requests of the server to initialize, run, and finalize.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("NetEsmfServer::coupler requestLoop()\n");

        int     localrc = 0;

        //***
        // Save the input parameters... these are used later when the client
        // wants to execute the initialize, run and finalize procedures
        //***
        theCompType = ESMC_COMPTYPE_COUPLER;

        theCouplerComp = comp;
        theImportState = importState;
        theExportState = exportState;
        theClock        = clock;
        thePhase = phase;
        theBlockingFlag = blockingFlag;
        
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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::getNextRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::getNextRequest()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::getNextRequest(
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
        //printf("NetEsmfServer::getNextRequest()\n");

        int     localrc = 0;

        //***
        // Wait for client requests
        //***
        if (theSocket.accept() != ESMF_SUCCESS)
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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::serviceRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::serviceRequest()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::serviceRequest(
//
// !RETURN VALUE:
//    int  id of the client request (the same value that's passed in)
//
// !ARGUMENTS:
//
  int  request          // id of the client request
  )
//
// !DESCRIPTION:
//    Calls the appropriate process method based on the client request id.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("NetEsmfServer::serviceRequest()\n");
        strcpy(theMsg, "OK");

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

        case NET_ESMF_END:
                processEnd();
                break;

        case NET_ESMF_PING:
                processPing();
                break;

        default:
                break;
        }

        theSocket.close();

        return request;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::getRequestId()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::getRequestId()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::getRequestId(
//
// !RETURN VALUE:
//    int  id of the request based on the specified string; ESMF_FAILURE
//         if the id cannot be found
//
// !ARGUMENTS:
//
  const char  request[]         // request string for which the id is to be returned
  )
//
// !DESCRIPTION:
//    Looks up a request id based on a specified string value.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("NetEsmfServer::getRequestId()\n");
        if (strcmp(request, "NEW")   == 0)      return NET_ESMF_NEW;
        if (strcmp(request, "EXIT")  == 0)      return NET_ESMF_EXIT;
        if (strcmp(request, "INIT")  == 0)      return NET_ESMF_INIT;
        if (strcmp(request, "RUN")   == 0)      return NET_ESMF_RUN;
        if (strcmp(request, "FINAL") == 0)      return NET_ESMF_FINAL;
        if (strcmp(request, "STATE") == 0)      return NET_ESMF_STATE;
        if (strcmp(request, "FILES") == 0)      return NET_ESMF_FILES;
        if (strcmp(request, "END")   == 0)      return NET_ESMF_END;
        if (strcmp(request, "PING")  == 0)      return NET_ESMF_PING;

        return ESMF_FAILURE;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::getRequestFromId()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::getRequestFromId()
//
// !INTERFACE:
char*  ESMCI_WebServNetEsmfServer::getRequestFromId(
//
// !RETURN VALUE:
//    char*  string value for the specified request id; the string, "UNKN",
//           if the value cannot be found
//
// !ARGUMENTS:
//
  int  id               // request id for which the string value is to be returned
  )
//
// !DESCRIPTION:
//    Looks up a request string value based on a specified request id.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("NetEsmfServer::getRequestFromId()\n");
        switch (id)
        {
        case NET_ESMF_EXIT:     return (char*)"EXIT";
        case NET_ESMF_NEW:      return (char*)"NEW";
        case NET_ESMF_INIT:     return (char*)"INIT";
        case NET_ESMF_RUN:      return (char*)"RUN";
        case NET_ESMF_FINAL:    return (char*)"FINAL";
        case NET_ESMF_STATE:    return (char*)"STATE";
        case NET_ESMF_FILES:    return (char*)"FILES";
        case NET_ESMF_END:      return (char*)"END";
        case NET_ESMF_PING:     return (char*)"PING";
        default:                                        return (char*)"UNKN";
        }

        return (char*)"UNKN";
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::processNew()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::processNew()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::processNew(
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
        int     localrc = 0;

        //***
        // Read the client name
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

        //***
        // Generate a new client id and add the new client to the collection
        // of clients
        //***
        int                                                             clientId = getNextClientId();
        ESMCI_WebServClientInfo*        newClient = new ESMCI_WebServClientInfo(clientId);

        theClients[clientId] = newClient;
        newClient->setStatus(NET_ESMF_STAT_READY);

        //***
        // Send back the new client id
        //***
        int     netClientId = htonl(clientId);
printf("Network client id: %d\n", netClientId);

        if (theSocket.write(4, &netClientId) != 4)
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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::processInit()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::processInit()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::processInit(
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
//    It then reads the names of input files (if any) from the socket and
//    imports the input file contents into an ESMF import state object.
//    Next, the component initialization routine is called, and finally, the
//    component status is written to the socket to complete the transaction.
//
//    (KDS: The whole import file stuff is still "iffy"... it works, but the
//          file has to be locally accessible... and I currently only support
//          one input file.)
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("\n\nSERVER: processing Init\n");

        int     localrc = 0;
        int     status = NET_ESMF_STAT_IDLE;

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
        // Get the number of files (should be either 0 or 1)... if there's 1, then
        // get the filename
        //***
        if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read number of files from socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

   int  numFiles = ntohl(*((unsigned int*)buf));
        char    filename[1024];
        //printf("Num Files: %d\n", numFiles);

        if (numFiles > 0)
        {
                if (theSocket.read(bytesRead, buf) <= 0)
        {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_READ,
                "Unable to read filename from socket.",
                ESMC_CONTEXT, &localrc);

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
        map<int, ESMCI_WebServClientInfo*>::iterator            iter;
        ESMCI_WebServClientInfo*                                                                clientInfo = NULL;

        if ((iter = theClients.find(clientId)) == theClients.end())
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

        clientInfo = iter->second;
        //clientInfo->print();

        //***
        // If a filename was specified, create the import state object
   // KDS: Reading information from a file is no longer used or
   //      supported since the data is not streamed across the network
   //      socket.
        //***

        //***
        // Call the component initialize
        //***
        clientInfo->setStatus(NET_ESMF_STAT_BUSY);

        int     rc = 0;
        if (theCompType == ESMC_COMPTYPE_GRID)
        {
                //printf("initializing a grid component\n");
      FTN_X(f_esmf_processinit)(theGridComp,
                              theImportState,
                              theExportState,
                              theClock,
                              thePhase,
                              &rc);
                //printf("Return code: %d\n", rc);

                if (rc != ESMF_SUCCESS)
        {
        ESMC_LogDefault.MsgFoundError(
                rc,
                "Error while executing initialization.",
                ESMC_CONTEXT, &localrc);

        return localrc;

                        clientInfo->setStatus(NET_ESMF_STAT_ERROR);
        }
                else
                {
                        clientInfo->setStatus(NET_ESMF_STAT_INIT_DONE);
                }
        }
        else if (theCompType == ESMC_COMPTYPE_COUPLER)
        {
                //printf("initializing a coupler component\n");
                theCouplerComp->initialize(theImportState,
                                 theExportState,
                                 theClock,
                                 thePhase,
                                 &rc);

                if (rc != ESMF_SUCCESS)
        {
        ESMC_LogDefault.MsgFoundError(
                rc,
                "Error while executing initialization.",
                ESMC_CONTEXT, &localrc);

        return localrc;

                        clientInfo->setStatus(NET_ESMF_STAT_ERROR);
        }
                else
                {
                        clientInfo->setStatus(NET_ESMF_STAT_INIT_DONE);
                }
        }
        //printf("Initialize Status: %d\n", rc);

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
   // TODO: determine status from rc
        status = clientInfo->status();
        unsigned int    netStatus = htonl(status);

        if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }
        // clientInfo->print();

        return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::processRun()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::processRun()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::processRun(
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
//    Next, the component run routine is called, and finally, the
//    component status is written to the socket to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("\n\nSERVER: processing Run\n");

        int     localrc = 0;
        int     status = NET_ESMF_STAT_IDLE;

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
        // Now that everything's been read off the socket, lookup the client info
        // based on the client id.  If the client can't be found, then send back
        // an error
        //***
        map<int, ESMCI_WebServClientInfo*>::iterator            iter;
        ESMCI_WebServClientInfo*                                                                clientInfo = NULL;

        if ((iter = theClients.find(clientId)) == theClients.end())
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

        clientInfo = iter->second;
        //clientInfo->print();

        //***
        // Call the component run
        //***
        clientInfo->setStatus(NET_ESMF_STAT_BUSY);

        int     rc = 0;
        if (theCompType == ESMC_COMPTYPE_GRID)
        {
      FTN_X(f_esmf_processrun)(theGridComp,
                             theImportState,
                             theExportState,
                             theClock,
                             thePhase,
                             &rc);

                if (rc != ESMF_SUCCESS)
        {
        ESMC_LogDefault.MsgFoundError(
                rc,
                "Error while executing run.",
                ESMC_CONTEXT, &localrc);

        return localrc;

                        clientInfo->setStatus(NET_ESMF_STAT_ERROR);
        }
                else
                {
                        clientInfo->setStatus(NET_ESMF_STAT_RUN_DONE);
                }
        }
        else if (theCompType == ESMC_COMPTYPE_COUPLER)
        {
                theCouplerComp->run(theImportState,
                          theExportState,
                          theClock,
                          thePhase,
                          &rc);

                if (rc != ESMF_SUCCESS)
        {
        ESMC_LogDefault.MsgFoundError(
                rc,
                "Error while executing run.",
                ESMC_CONTEXT, &localrc);

        return localrc;

                        clientInfo->setStatus(NET_ESMF_STAT_ERROR);
        }
                else
                {
                        clientInfo->setStatus(NET_ESMF_STAT_RUN_DONE);
                }
        }

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
   // TODO: determine status from rc
        status = clientInfo->status();
        unsigned int    netStatus = htonl(status);

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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::processFinal()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::processFinal()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::processFinal(
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
//    Next, the component finalize routine is called, and then the export
//    state is written to the socket.  Finally, the component status is
//    written to the socket to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("\n\nSERVER: processing Final\n");

        int     localrc = 0;
        int     status = NET_ESMF_STAT_IDLE;

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
        // Now that everything's been read off the socket, lookup the client info
        // based on the client id.  If the client can't be found, then send back
        // an error
        //***
        map<int, ESMCI_WebServClientInfo*>::iterator            iter;
        ESMCI_WebServClientInfo*                                                                clientInfo = NULL;

        if ((iter = theClients.find(clientId)) == theClients.end())
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

        clientInfo = iter->second;
        //clientInfo->print();

        //***
        // Set the status to indicate that the service is busy right now
        //***
        clientInfo->setStatus(NET_ESMF_STAT_BUSY);

        //***
        // Write the export file out to the local server file location
   // KDS: This functionality is no longer used since the data is now
   //      streamed to a network socket.
        //***

        //***
        // Call the component finalize
        //***
        int     rc = 0;
        if (theCompType == ESMC_COMPTYPE_GRID)
        {
      FTN_X(f_esmf_processfinal)(theGridComp,
                               theImportState,
                               theExportState,
                               theClock,
                               thePhase,
                               &rc);

                if (rc != ESMF_SUCCESS)
        {
        ESMC_LogDefault.MsgFoundError(
                rc,
                "Error while executing finalization.",
                ESMC_CONTEXT, &localrc);

        return localrc;

                        clientInfo->setStatus(NET_ESMF_STAT_ERROR);
        }
                else
                {
                        clientInfo->setStatus(NET_ESMF_STAT_FINAL_DONE);
                }
        }
        else if (theCompType == ESMC_COMPTYPE_COUPLER)
        {
                theCouplerComp->finalize(theImportState,
                               theExportState,
                               theClock,
                               thePhase,
                               &rc);

                if (rc != ESMF_SUCCESS)
        {
        ESMC_LogDefault.MsgFoundError(
                rc,
                "Error while executing finalization.",
                ESMC_CONTEXT, &localrc);

        return localrc;

                        clientInfo->setStatus(NET_ESMF_STAT_ERROR);
        }
                else
                {
                        clientInfo->setStatus(NET_ESMF_STAT_FINAL_DONE);
                }
        }

        //***
        // OK, this is really hokey and needs to be changed at the earliest possible
        // moment... I'm calling the finalize method where the state object is
        // written to the file, and then copying the file to the server location.
        // What's really bad about this is that the filename is the same everytime
        // the finalize is called... What I should be doing is writing the file
        // to the server location once and not copy the file, but I don't have a way
        // of passing the filename (which should include the client id) to the
        // finalize method... so maybe I should export the state here... I don't
        // know right now... but I need to figure it out.
        //***

        // TODO: Change this to write the export state here instead of expecting
        //       the finalize method to do it...

/*
        if (access("afile.nc", R_OK) == 0)
        {
                char            localFilename[256];
                char            exportFilename[512];
                char*           webServer = "http://27thstsoftware.com:8080";
                char*           openDapPath = "opendap/data/nc";

                sprintf(localFilename, "/usr/local/share/hyrax/data/nc/afile_%d.nc",
                             clientInfo->clientId());
                sprintf(exportFilename, "%s/%s/afile_%d.nc",
                              webServer, openDapPath, clientInfo->clientId());

                copyFile("afile.nc", localFilename);
                clientInfo->setExportFilename(exportFilename);
        }
*/

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
   // TODO: determine status from rc
        status = clientInfo->status();
        unsigned int    netStatus = htonl(status);

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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::processState()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::processState()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::processState(
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
//    information. The component state is retrieved from the client
//    information and is written to the socket to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("\n\nSERVER: processing State\n");

        int     localrc = 0;
        int     status = NET_ESMF_STAT_IDLE;

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
        // Now that everything's been read off the socket, lookup the client info
        // based on the client id.  If the client can't be found, then send back
        // an error
        //***
        map<int, ESMCI_WebServClientInfo*>::iterator            iter;
        ESMCI_WebServClientInfo*                                                                clientInfo = NULL;

        if ((iter = theClients.find(clientId)) == theClients.end())
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

        clientInfo = iter->second;
        //clientInfo->print();

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
   // TODO: determine status from rc
        status = clientInfo->status();
        unsigned int    netStatus = htonl(status);

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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::processFiles()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::processFiles()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::processFiles(
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
//    reads the client id from the socket and uses it to lookup the client
//    information. Next, list of export files is retrieved from the client
//    information and is written out to the socket.  Finally, the component
//    status is written to the socket to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("\n\nSERVER: processing Files\n");

        int     localrc = 0;
        int     status = NET_ESMF_STAT_IDLE;
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
        //printf("Client ID: %d\n", clientId);

        //***
        // Now that everything's been read off the socket, lookup the client info
        // based on the client id.  If the client can't be found, then send back
        // an error
        //***
        map<int, ESMCI_WebServClientInfo*>::iterator            iter;
        ESMCI_WebServClientInfo*                                                                clientInfo = NULL;

        if ((iter = theClients.find(clientId)) == theClients.end())
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

        clientInfo = iter->second;
        //clientInfo->print();

        //***
        // Write the file information back to the client
        //***
        numFiles = 2;
        char    fileInfoBuf[1024];
        int     fileInfoSize = 0;

        unsigned int  netNumFiles = htonl(numFiles);
        if (theSocket.write(4, &netNumFiles) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write number of files to socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

        strcpy(fileInfoBuf, "import");
        fileInfoSize = strlen(fileInfoBuf) + 1;
        if (theSocket.write(fileInfoSize, fileInfoBuf) != fileInfoSize)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write file type to socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

        strcpy(fileInfoBuf, "file1.nc");
        //strcpy(fileInfoBuf, clientInfo->importFilename());
        fileInfoSize = strlen(fileInfoBuf) + 1;
        if (theSocket.write(fileInfoSize, fileInfoBuf) != fileInfoSize)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write filename to socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

        strcpy(fileInfoBuf, "export");
        fileInfoSize = strlen(fileInfoBuf) + 1;
        if (theSocket.write(fileInfoSize, fileInfoBuf) != fileInfoSize)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write file type to socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

        strcpy(fileInfoBuf, "file2.nc");
        //strcpy(fileInfoBuf, clientInfo->exportFilename());
        fileInfoSize = strlen(fileInfoBuf) + 1;
        if (theSocket.write(fileInfoSize, fileInfoBuf) != fileInfoSize)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write filename to socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
        status = clientInfo->status();
        unsigned int    netStatus = htonl(status);

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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::processEnd()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::processEnd()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::processEnd(
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

        int     localrc = 0;
        int     status = NET_ESMF_STAT_IDLE;

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
        // Now that everything's been read off the socket, lookup the client info
        // based on the client id.  If the client can't be found, then send back
        // an error
        //***
        map<int, ESMCI_WebServClientInfo*>::iterator            iter;
        ESMCI_WebServClientInfo*                                                                clientInfo = NULL;

        if ((iter = theClients.find(clientId)) == theClients.end())
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

        clientInfo = iter->second;
        //clientInfo->print();

   //***
   // Remove the client from the collection of clients
        //***
        theClients.erase(clientId);
        delete clientInfo;

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
   // TODO: determine status from rc
        status = NET_ESMF_STAT_DONE;
        unsigned int    netStatus = htonl(status);

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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::processPing()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::processPing()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::processPing(
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
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::getNextClientId()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::getNextClientId()
//
// !INTERFACE:
int  ESMCI_WebServNetEsmfServer::getNextClientId(
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
        int     nextClientId = theNextClientId;

        ++theNextClientId;

        return nextClientId;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNetEsmfServer::copyFile()"
//BOPI
// !ROUTINE:  ESMCI_WebServNetEsmfServer::copyFile()
//
// !INTERFACE:
void  ESMCI_WebServNetEsmfServer::copyFile(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  const char*  srcFilename,     // the name of the file to copy
  const char*  destFilename     // the name of the destination file
  )
//
// !DESCRIPTION:
//    Copies the source file to the destination file.
//
//EOPI
//-----------------------------------------------------------------------------
{
        fstream         fin(srcFilename, ios::in | ios::binary);
        fstream         fout(destFilename, ios::out | ios::binary);

        if ((!fin.is_open())  ||  (!fout.is_open()))
        {
                cerr << "Error copying file" << endl;
                return;
        }

        char    c;
        while (!fin.eof())
        {
                fin.get(c);
                fout.put(c);
        }
}

} // end namespace
