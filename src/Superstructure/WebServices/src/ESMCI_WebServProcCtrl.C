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
//==============================================================================
//
// ESMC WebServProcCtrl method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ProcCtrl methods declared
// in the companion file ESMCI_WebServProcCtrl.h.  This code
// provides the functionality needed to implement a Process Controller
// Service, which essentially just passes requests from its client on to an
// ESMF Component Service (implemented with the ESMCI_ComponentSvr class).
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServProcCtrl.h"

#include <stdint.h>
#include <string.h>

#if !defined (ESMF_OS_MinGW)
#include <netdb.h>
#else
#include <Winsock.h>
#endif

#include "ESMCI_WebServSocketUtils.h"
#include "ESMCI_WebServCompSvrClient.h"
#include "ESMCI_WebServRegistrarClient.h"
#include "ESMCI_WebServGRAMClient.h"
#include "ESMCI_WebServForkClient.h"
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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::ESMCI_WebServProcCtrl()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::ESMCI_WebServProcCtrl()
//
// !INTERFACE:
ESMCI_WebServProcCtrl::ESMCI_WebServProcCtrl(
//
//
// !ARGUMENTS:
//
  int              procCtrlPort,       // (in) the port number on which to
                                       //   setup the socket service to listen
                                       //   for requests
  string           registrarHost,      // (in) the host name on which the
                                       //   Registrar is running
  int              registrarPort,      // (in) the port number of the Registrar
  string           compSvrHost,        // (in) the host name of the component
                                       //   svc to which we'll be connecting
  int              compSvrStartPort,   // (in) the starting port number of pool
                                       //   of ports on which the component
                                       //   services will be listening
  int              portPoolSize,       // (in) the size of the pool of ports
  string           compSvrScriptDir,   // (in) the directory that contains the
                                       //   script that is used to startup the
                                       //   component svc
  string           compSvrScriptName,  // (in) the name of the script that is
                                       //   used to startup the component svc
  ESMC_JobMgrType  jobMgrType          // (in) the type of tool used to manage
                                       //   component svc jobs
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

        theProcCtrlPort      = procCtrlPort;
   theRegistrarHost     = registrarHost;
   theRegistrarPort     = registrarPort;
   theCompSvrHost       = compSvrHost;
   theCompSvrStartPort  = compSvrStartPort;
   thePortPoolSize      = portPoolSize;
        theCompSvrScriptDir  = compSvrScriptDir;
        theCompSvrScriptName = compSvrScriptName;
        theJobMgrType        = jobMgrType;

        //***
        // Startup a new component server for each client.
        //***
        if (theJobMgrType == ESMC_JOBMGRTYPE_GLOBUS)
        {
                theCompSvrMgr = new ESMCI_WebServGRAMClient(theCompSvrHost,
                                                  theCompSvrScriptDir,
                                                  theCompSvrScriptName);
        }
        else
        {
                theCompSvrMgr = new ESMCI_WebServForkClient(theCompSvrHost,
                                                  theCompSvrScriptDir,
                                                  theCompSvrScriptName);
        }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrl::~ESMCI_WebServProcCtrl()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::~ESMCI_WebServProcCtrl()
//
// !INTERFACE:
ESMCI_WebServProcCtrl::~ESMCI_WebServProcCtrl(
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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::requestLoop()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::requestLoop()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::requestLoop(
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
        //printf("ESMCI_WebServProcCtrl::requestLoop()\n");
        int     localrc = 0;
        
   //***
   // Setup the server socket
   //***
        if (theSocket.connect(theProcCtrlPort) < 0)
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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::getNextRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::getNextRequest()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::getNextRequest(
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
        //printf("ESMCI_WebServProcCtrl::getNextRequest()\n");
        int     localrc = 0;

   //***
   // Wait for client requests
   //***
        if (theSocket.accept() == ESMF_FAILURE)
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

        //printf("ProcCtrl: request: %s\n", requestStr);

   //***
   // Convert the string to a valid request id
   //***
        return ESMCI_WebServGetRequestId(requestStr);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrl::serviceRequest()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::serviceRequest()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::serviceRequest(
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
        //printf("ESMCI_WebServProcCtrl::serviceRequest()\n");
        //printf("Request ID: %d\n", request);

        switch (request)
        {
        case NET_ESMF_NEW:
                processNew();
                break;

        case NET_ESMF_INIT:
                processInit();
                break;

        case NET_ESMF_STATE:
                processState();
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

        case NET_ESMF_DATA_DESC:
                processGetDataDesc();
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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processNew()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processNew()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processNew(
//
// !RETURN VALUE:
//    ESMF_SUCCESS if successful; ESMF_FAILURE otherwise;
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
        printf("\n\nProcCtrl: processing New\n");
        int     localrc = 0;

        //***
        // Read the client name and password from the socket
        //***
        int     bytesRead = 0;
        char    userName[1024];
        char    password[1024];

        if (theSocket.read(bytesRead, userName) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client user name from socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if (theSocket.read(bytesRead, password) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read client user name from socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        printf("User Name: %s\n", userName);
        printf("Password: %s\n", password);

        //***
        // Generate a new client id and add the new client to the collection
        // of clients
        //***
        int                                                             clientId = getNextClientId();
        ESMCI_WebServClientInfo*        newClient = new ESMCI_WebServClientInfo(clientId);
        theClients[clientId] = newClient;

        newClient->setUserName(userName);
        newClient->setPassword(password);
        newClient->setServerHost(theCompSvrHost);
        newClient->setServerPort(theCompSvrStartPort);
printf("*** Client ID: %d\n", clientId);
printf("*** Port Num: %d\n", theCompSvrStartPort);

// KDS: TODO - Add method to get next port number

        //***
        // Register submitted job with Registrar
        // Issue here is that if using fork, server will register itself before
        // the process controller... so process controller needs to register
        // before it gets submitted, then change the status to error if there's
        // an error in the submission
        //***
        ESMCI_WebServRegistrarClient    registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);

        char    clientIdStr[64];
        sprintf(clientIdStr, "%d", clientId);

        char    portNumStr[64];
        sprintf(portNumStr, "%d", theCompSvrStartPort);

        if ((localrc = registrar.registerComp(clientIdStr,
                                         theCompSvrHost.c_str(),
                                         portNumStr)) != NET_ESMF_STAT_IDLE)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_NOT_VALID,
         "Error while registering component service with Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
        }

        //***
        // Startup a new component server for the new client.
        //***
printf("Client ID: %d\n", clientId);
printf("Port Num: %d\n", theCompSvrStartPort);
        string  jobId = theCompSvrMgr->submitJob(clientId,
                                             theRegistrarHost,
                                             theCompSvrStartPort);
        newClient->setJobId(jobId);
printf("Job ID: %s\n", jobId.c_str());

        if (jobId.empty())
        {
                newClient->setStatus(NET_ESMF_STAT_ERROR);
        }
        else
        {
                int     currentStatus = registrar.compSubmitted(clientIdStr, jobId.c_str());
                if (currentStatus == ESMF_FAILURE)
                {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_NOT_VALID,
                "Error while updating Registrar.",
                ESMC_CONTEXT, &localrc);

        return ESMF_FAILURE;
                }

                newClient->setStatus(currentStatus);
        }

        //***
        // Send back the new client id
        //***
        printf("Client id: %d\n", clientId);
        int     netClientId = htonl(clientId);

        if (theSocket.write(4, &netClientId) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processState()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processState()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processState(
//
// !RETURN VALUE:
//    ESMF_SUCCESS if successful; ESMF_FAILURE otherwise;
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to retrieve the component state.  This method
//    reads the client id from the socket and uses it to lookup the client
//    information.  The Registrar is then used to fetch the current status
//    of the component server, and finally, the component status is written
//    to the socket to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("\n\nSERVER: processing State\n");

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

      return ESMF_FAILURE;
   }

   int  clientId = ntohl(*((unsigned int*)buf));
        printf("Client ID: %d\n", clientId);

        //***
        // Now that everything's been read off the socket, lookup the client info
        // based on the client id.  If the client can't be found, then send back
        // an error
        //***
        map<int, ESMCI_WebServClientInfo*>::iterator            iter;
        ESMCI_WebServClientInfo*                                                                clientInfo = NULL;

        if ((iter = theClients.find(clientId)) == theClients.end())
        {
                //***
                // Client ID not found... send back error
                //***
                status = NET_ESMF_STAT_ERROR;
                unsigned int    netStatus = htonl(status);

                if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
      }

      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_ARG_VALUE,
         "Invalid client id read from socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
        }

        //***
        // Pass on the state request to the Registrar and get the current status
        //***
        clientInfo = iter->second;
        clientInfo->print();

        //***
        // Get the component server state from the Registrar.  If there's an
        // error getting the status from the Registrar, log the error but continue.
        // Just use the existing status in the local store.
        //***
        ESMCI_WebServRegistrarClient    registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);

        char    clientIdStr[64];
        sprintf(clientIdStr, "%d", clientId);

        status = registrar.getStatus(clientIdStr);
        if (status == ESMF_FAILURE)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_NOT_VALID,
         "Error while while getting component service status from Registrar.",
         ESMC_CONTEXT, &localrc);
        }
        else
        {
                clientInfo->setStatus(status);
        }

        //***
        // Send the current state back to the client
        //***
printf("Component Server Status: %s\n", registrar.getStateStr(status));
        unsigned int    netStatus = htonl(status);

        if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processInit()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processInit()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processInit(
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
                //***
                // Client ID not found... send back error
                //***
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
        clientInfo->print();

        //***
        // Get the component server information from the Registrar.
        //***
        char    clientIdStr[64];
        sprintf(clientIdStr, "%d", clientInfo->clientId());

        ESMCI_WebServRegistrarClient    registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);
        ESMCI_WebServCompSvrInfo                compSvrInfo;

        if ((status = registrar.getComponent(clientIdStr, &compSvrInfo)) ==
                        ESMF_FAILURE)
        {
                //***
                // Error communicating with Registrar... send back error
                //***
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

                //***
                // Then log and return
                //***
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_NOT_VALID,
                "Error while while getting component svc info from Registrar.",
                ESMC_CONTEXT, &localrc);

        return ESMF_FAILURE;
        }

        compSvrInfo.print();
        status = compSvrInfo.status();

        clientInfo->setStatus(status);

        //***
        // Call the component initialize.  Must be in the READY state before the
        // initialize can be called.
        //***
        if (status == NET_ESMF_STAT_READY)
        {
                ESMCI_WebServCompSvrClient              client(compSvrInfo.physHostName().c_str(),
                                           compSvrInfo.portNum(),
                                           compSvrInfo.clientId());

                status = client.init();

                clientInfo->setStatus(status);
        }
        else
        {
                // Handle error... probably just log because the next step will
                // return the error to the client... don't really care about the
                // calling function getting an error message... nothing will happen
                // because of it.
        }

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
        status = clientInfo->status();
printf("PROC CTRL: Returning Status: %d\n", status);
        unsigned int    netStatus = htonl(status);

        if (theSocket.write(4, &netStatus) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Unable to write status to socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }
        //clientInfo->print();

        return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processRun()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processRun()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processRun(
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
   // Get the component server information from the Registrar.
   //***
   char  clientIdStr[64];
   sprintf(clientIdStr, "%d", clientInfo->clientId());

   ESMCI_WebServRegistrarClient  registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);
   ESMCI_WebServCompSvrInfo      compSvrInfo;

   if ((status = registrar.getComponent(clientIdStr, &compSvrInfo)) ==
         ESMF_FAILURE)
   {
      //***
      // Error communicating with Registrar... send back error
      //***
      status = NET_ESMF_STAT_ERROR;
      unsigned int   netStatus = htonl(status);

      if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return localrc;
      }

      //***
      // Then log and return
      //***
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_NOT_VALID,
         "Error while while getting component svc info from Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   compSvrInfo.print();
   status = compSvrInfo.status();

        clientInfo->setStatus(status);

        //***
        // Call the component run (must have completed the initialize phase before
        // the run could be called)
        //***
        if (status == NET_ESMF_STAT_INIT_DONE)
        {
           ESMCI_WebServCompSvrClient   client(compSvrInfo.physHostName().c_str(),
                                        compSvrInfo.portNum(),
                                        compSvrInfo.clientId());

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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processTimestep()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processTimestep()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processTimestep(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to timestep the component.  This method reads the
//    client id from the socket and uses it to lookup the client information.
//    It then reads the number of timesteps parameter from the socket.
//    The request and its parameters are then passed on to the component
//    server, and finally, the component status is written to the socket
//    to complete the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("\n\nSERVER: processing Timestep\n");

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
        printf("Client ID: %d\n", clientId);

        //***
        // Get the number of timesteps
        //***
        bytesRead = 0;

        if (theSocket.read(bytesRead, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Unable to read num timesteps from socket.",
         ESMC_CONTEXT, &localrc);

      return localrc;
   }

   int  numTimesteps = ntohl(*((unsigned int*)buf));
        printf("Num Timesteps: %d\n", numTimesteps);

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
   // Get the component server information from the Registrar.
   //***
   char  clientIdStr[64];
   sprintf(clientIdStr, "%d", clientInfo->clientId());

   ESMCI_WebServRegistrarClient  registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);
   ESMCI_WebServCompSvrInfo      compSvrInfo;

   if ((status = registrar.getComponent(clientIdStr, &compSvrInfo)) ==
         ESMF_FAILURE)
   {
      //***
      // Error communicating with Registrar... send back error
      //***
      status = NET_ESMF_STAT_ERROR;
      unsigned int   netStatus = htonl(status);

      if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return localrc;
      }

      //***
      // Then log and return
      //***
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_NOT_VALID,
         "Error while while getting component svc info from Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   compSvrInfo.print();
   status = compSvrInfo.status();

   clientInfo->setStatus(status);

        //***
        // Call the component run (must have completed the initialize phase before
        // the run could be called)
        //***
        if ((status == NET_ESMF_STAT_INIT_DONE)  ||
            (status == NET_ESMF_STAT_TIMESTEP_DONE))
        {
           ESMCI_WebServCompSvrClient   client(compSvrInfo.physHostName().c_str(),
                                        compSvrInfo.portNum(),
                                        compSvrInfo.clientId());

                status = client.timestep(numTimesteps);
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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processFinal()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processFinal()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processFinal(
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
        printf("\n\nSERVER: processing Final\n");

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
        printf("Client ID: %d\n", clientId);

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
   // Get the component server information from the Registrar.
   //***
   char  clientIdStr[64];
   sprintf(clientIdStr, "%d", clientInfo->clientId());

   ESMCI_WebServRegistrarClient  registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);
   ESMCI_WebServCompSvrInfo      compSvrInfo;

   if ((status = registrar.getComponent(clientIdStr, &compSvrInfo)) ==
         ESMF_FAILURE)
   {
      //***
      // Error communicating with Registrar... send back error
      //***
      status = NET_ESMF_STAT_ERROR;
      unsigned int   netStatus = htonl(status);

      if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return localrc;
      }

      //***
      // Then log and return
      //***
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_NOT_VALID,
         "Error while while getting component svc info from Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   compSvrInfo.print();
   status = compSvrInfo.status();

   clientInfo->setStatus(status);

        //***
        // Call the component finalize (must have completed at least the initalize
   // phase, and possibly the run phase before the finalize can be called
   // (KDS: assuming you can call finalize after calling just initialize, but
   //       not sure if that is true.)
        //***
        if ((status == NET_ESMF_STAT_RUN_DONE)  ||
            (status == NET_ESMF_STAT_TIMESTEP_DONE)  ||
            (status == NET_ESMF_STAT_INIT_DONE))
        {
           ESMCI_WebServCompSvrClient   client(compSvrInfo.physHostName().c_str(),
                                        compSvrInfo.portNum(),
                                        compSvrInfo.clientId());

                status = client.final();
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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processGetDataDesc()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processGetDataDesc()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processGetDataDesc(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to retrieve the description of the output data.
//    This method reads the client id from the socket and uses it to lookup
//    the client information.  It then gets the data description from the
//    component server and writes that information back to the client.
//    Finally, the component status is written back to the socket to complete
//    the transaction.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("\n\nPROCCTRL: processing GetDataDesc\n");

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
        printf("Client ID: %d\n", clientId);

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
   // Get the component server information from the Registrar.
   //***
   char  clientIdStr[64];
   sprintf(clientIdStr, "%d", clientInfo->clientId());

   ESMCI_WebServRegistrarClient  registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);
   ESMCI_WebServCompSvrInfo      compSvrInfo;

   if ((status = registrar.getComponent(clientIdStr, &compSvrInfo)) ==
         ESMF_FAILURE)
   {
      //***
      // Error communicating with Registrar... send back error
      //***
      status = NET_ESMF_STAT_ERROR;
      unsigned int   netStatus = htonl(status);

      if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return localrc;
      }

      //***
      // Then log and return
      //***
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_NOT_VALID,
         "Error while while getting component svc info from Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   compSvrInfo.print();
   status = compSvrInfo.status();

   clientInfo->setStatus(status);


        //***
        // Get the data description from the server
        //***
        ESMCI_WebServCompSvrClient      client(compSvrInfo.physHostName().c_str(),
                                     compSvrInfo.portNum(),
                                     compSvrInfo.clientId());

        ESMCI_WebServDataDesc*  dataDesc = client.dataDesc();

        //***
   // Return the description of the variables and the grid information
   // (i.e., the variable names and the lat/lon values)
   //***
        if (dataDesc != NULL)
        {
                //dataDesc->print();

                //***
                // Write out the number of variables
                //***
      int   numVars = dataDesc->getNumVars();
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
                // Write out the variable names
                //***
      string*  varNames = dataDesc->getVarNames();

      for (int i = 0; i < numVars; ++i)
      {
         int      valueLen = varNames[i].length() + 1;

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

                //***
                // Write out the number of latitude values
                //***
      int   numLats = dataDesc->getNumLatValues();
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
                // Write out the latitude values
                //***
      double*  latValues = dataDesc->getLatValues();
      double*  netLatValues = new double[numLats];
      int      latDataSize = sizeof(double) * numLats;

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

                //***
                // Write out the number of longitude values
                //***
      int   numLons = dataDesc->getNumLonValues();
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
                // Write out the longitude values
                //***
      double*  lonValues = dataDesc->getLonValues();
      double*  netLonValues = new double[numLons];
      int      lonDataSize = sizeof(double) * numLons;

      for (int i = 0; i < numLons; ++i)
      {
         netLonValues[i] = lonValues[i];
         htonll((uint64_t)(netLonValues[i]));
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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processGetData()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processGetData()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processGetData(
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

   double*  timeValue = (double*)buf;
   ntohll((uint64_t)(*timeValue));
printf("After: %g\n", *timeValue);

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
   // Get the component server information from the Registrar.
   //***
   char  clientIdStr[64];
   sprintf(clientIdStr, "%d", clientInfo->clientId());

   ESMCI_WebServRegistrarClient  registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);
   ESMCI_WebServCompSvrInfo      compSvrInfo;

   if ((status = registrar.getComponent(clientIdStr, &compSvrInfo)) ==
         ESMF_FAILURE)
   {
      //***
      // Error communicating with Registrar... send back error
      //***
      status = NET_ESMF_STAT_ERROR;
      unsigned int   netStatus = htonl(status);

      if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return localrc;
      }

      //***
      // Then log and return
      //***
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_NOT_VALID,
         "Error while while getting component svc info from Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   compSvrInfo.print();
   status = compSvrInfo.status();

   clientInfo->setStatus(status);

        //***
        // Get the data from the server
        //***
        ESMCI_WebServCompSvrClient      client(compSvrInfo.physHostName().c_str(),
                                     compSvrInfo.portNum(),
                                     compSvrInfo.clientId());

   int      numVars = 0;
   int      numLats = 0;
   int      numLons = 0;
   string*  varNames = NULL;

        ESMCI_WebServDataContent*       outputData = client.outputData(*timeValue,
                                                             &numVars,
                                                             &varNames,
                                                             &numLats,
                                                             &numLons);

        if (outputData != NULL)
        {
                //outputData->print();
        }

   printf("Num Vars: %d\n", numVars);
   for (int i = 0; i < numVars; ++i)
   {
      printf("Var Name[%d]: %s\n", i, varNames[i].c_str());
   }
   printf("Num Lats: %d\n", numLats);
   printf("Num Lons: %d\n", numLons);

        //***
   // Return the description of the variables and the grid information
   // (i.e., the variable names and the number of lat/lon values)
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
      int      valueLen = varNames[i].length() + 1;

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
      double*  dataValues = outputData->getDataValues(varNames[i]);
      int      numValues = numLats * numLons;
      double*  netDataValues = new double[numValues];

printf("Num Values: %d\n", numValues);
      for (int j = 0; j < numValues; ++j)
      {
//printf("Data Value[%d][%d]: %g\n", i, j, dataValues[j]);
         netDataValues[j] = dataValues[j];
         ntohll((uint64_t)(netDataValues[j]));
      }

      int   dataSize = sizeof(double) * numValues;
      int   bytesSent = 0;

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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processEnd()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processEnd()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processEnd(
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
        status = clientInfo->status();

   //***
   // Get the component server information from the Registrar.
   //***
   char  clientIdStr[64];
   sprintf(clientIdStr, "%d", clientInfo->clientId());

   ESMCI_WebServRegistrarClient  registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);
   ESMCI_WebServCompSvrInfo      compSvrInfo;

   if ((status = registrar.getComponent(clientIdStr, &compSvrInfo)) ==
         ESMF_FAILURE)
   {
      //***
      // Error communicating with Registrar... send back error
      //***
      status = NET_ESMF_STAT_ERROR;
      unsigned int   netStatus = htonl(status);

      if (theSocket.write(4, &netStatus) != 4)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_WRITE,
            "Unable to write error status to socket.",
            ESMC_CONTEXT, &localrc);

         return localrc;
      }

      //***
      // Then log and return
      //***
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_NOT_VALID,
         "Error while while getting component svc info from Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   compSvrInfo.print();
   status = compSvrInfo.status();

   clientInfo->setStatus(status);

        ESMCI_WebServCompSvrClient      client(compSvrInfo.physHostName().c_str(),
                                     compSvrInfo.portNum(),
                                     compSvrInfo.clientId());

        status = client.end();
        clientInfo->setStatus(status);

   //***
   // Remove the client from the collection of clients
        //***
        theClients.erase(clientId);
        delete clientInfo;

   //***
   // Kill the server
   //**
   client.killServer();

        //***
        // Send the current state back to the client (use the return code from
        // the component initialize call to determine the state)
        //***
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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processExit()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processExit()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processExit(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or ESMF_FAILURE on failure.
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Processes the request to exit the Process Controller.  Exiting the
//    Process Controller means exiting all of the Component Services associate
//    with the process controller.  This is a very powerful operation, and
//    I should probably do some security checking before allowing it... later.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("\n\nProcCtrl: processing EXIT\n");

        int     localrc = 0;
        int     status = NET_ESMF_STAT_IDLE;


        //***
        // Go through the list of clients and for each client, connect to
        // it's component service and execute the killServer command.
        // KDS: TODO - check the server status, and if it's still "SUBMITTED", then
        //      we'll have to use the job scheduler to CANCEL the job request
        //      (since the job has not yet started to run).
        //***
        ESMCI_WebServRegistrarClient    registrar(theRegistrarHost.c_str(),
                                           theRegistrarPort);

        map<int, ESMCI_WebServClientInfo*>::iterator            iter;
        ESMCI_WebServClientInfo*                                                                clientInfo = NULL;

        for (iter = theClients.begin(); iter != theClients.end(); ++iter)
        {
                clientInfo = iter->second;
                //clientInfo->print();

                //***
                // First, get the server info from the registrar
                //***
                char    clientIdStr[64];
                sprintf(clientIdStr, "%d", clientInfo->clientId());

                ESMCI_WebServCompSvrInfo        compSvrInfo;

                if ((status = registrar.getComponent(clientIdStr, &compSvrInfo)) ==
                        ESMF_FAILURE)
                {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_NOT_VALID,
                "Error while while getting component svc info from Registrar.",
                ESMC_CONTEXT, &localrc);

        return ESMF_FAILURE;
                }

                compSvrInfo.print();
                status = compSvrInfo.status();

                switch (status)
                {
                case NET_ESMF_STAT_READY:
                case NET_ESMF_STAT_INITIALIZING:
                case NET_ESMF_STAT_INIT_DONE:
                case NET_ESMF_STAT_RUNNING:
                case NET_ESMF_STAT_RUN_DONE:
                case NET_ESMF_STAT_FINALIZING:
                case NET_ESMF_STAT_FINAL_DONE:
                        {
                                ESMCI_WebServCompSvrClient      
                                        client(compSvrInfo.physHostName().c_str(),
                      compSvrInfo.portNum(),
                      compSvrInfo.clientId());

                                client.killServer();
                        }
                        break;

                case NET_ESMF_STAT_SUBMITTED:
                        {
                                // Cancel using job scheduler
                        }
                        break;

                default:
                        break;

                } // end switch

        } // end for

        return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrl::processPing()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::processPing()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::processPing(
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
#define ESMC_METHOD "ESMCI_WebServProcCtrl::getNextClientId()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrl::getNextClientId()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrl::getNextClientId(
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

} // end namespace
