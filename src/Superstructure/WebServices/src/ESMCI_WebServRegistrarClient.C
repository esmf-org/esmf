// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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
        : ESMCI_WebServNetEsmfClient(host, port)
{
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
//    int  current state of the component service on the Registrar;
//         ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  const char*  clientId,        // (in) the id for the client who created the svc
  const char*  hostName,        // (in) the host name of the component svc scheduler
  const char*  portNum          // (in) the port number of the component svc
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
        int     localrc = 0;

        //***
        // Connect to the server
        //***
        if (connect() == ESMF_FAILURE)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error connecting to Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the request identifier
        //***
        int     dataLen = strlen("register") + 1;
        if (sendData(dataLen, (void*)"register") != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending register request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the client id
        //***
        dataLen = strlen(clientId) + 1;
        if (sendData(dataLen, (void*)clientId) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending clientId to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the host name
        //***
        dataLen = strlen(hostName) + 1;
        if (sendData(dataLen, (void*)hostName) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending service host name to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the port number
        //***
        dataLen = strlen(portNum) + 1;
        if (sendData(dataLen, (void*)portNum) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending service port number to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Get the response from the registrar
        //***
        int     length = 0;
        char    retValue[256];

        if (getResponse(0, length, retValue) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading register response from socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }
printf("Return Value: %s\n", retValue);

        //***
        // Disconnect from the Registrar
        //***
        disconnect();

        return getStateValue(retValue);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::compSubmitted()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::compSubmitted()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::compSubmitted(
//
// !RETURN VALUE:
//    int  number of bytes read from the socket;
//         ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  const char*  clientId,        // (in) the id for the client who created the service
  const char*  jobId               // (in) the id of the job associated with the
                           //      scheduling of the service
  )
//
// !DESCRIPTION:
//    Notifies the Registrar that the process controller has submitted the
//    job to the job scheduler and sets the job id returned from the job
//    scheduler.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("RegistrarClient::compSubmitted()\n");
        int     localrc = 0;

        //***
        // Connect to the server
        //***
        if (connect() == ESMF_FAILURE)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error connecting to Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the request identifier
        //***
        int     dataLen = strlen("submitted") + 1;
        if (sendData(dataLen, (void*)"submitted") != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending started request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the client id
        //***
        dataLen = strlen(clientId) + 1;
        if (sendData(dataLen, (void*)clientId) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending clientId to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the job id
        //***
        dataLen = strlen(jobId) + 1;
        if (sendData(dataLen, (void*)jobId) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending job id to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Get the response from the registrar
        //***
        int     length = 0;
        char    retValue[256];

        if (getResponse(0, length, retValue) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading register response from socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }
printf("Return Value: %s\n", retValue);

        //***
        // Disconnect from the Registrar
        //***
        disconnect();

        return getStateValue(retValue);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::compStarted()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::compStarted()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::compStarted(
//
// !RETURN VALUE:
//    int  number of bytes read from the socket;
//         ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  const char*  clientId,                // (in) the id of the client who created the svc
  const char*  compName,                // (in) the name of the component service
  const char*  compDesc,                // (in) the description of the component service
  const char*  physHostName     // (in) the name of actual host on which the
                              //      component service is running
  )
//
// !DESCRIPTION:
//    Notifies the Registrar that the component service has successfully
//    been started.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("RegistrarClient::compStarted()\n");
        int     localrc = 0;

        //***
        // Connect to the server
        //***
        if (connect() == ESMF_FAILURE)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error connecting to Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the request identifier
        //***
        int     dataLen = strlen("started") + 1;
        if (sendData(dataLen, (void*)"started") != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending started request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the client id
        //***
printf("Client ID: %s\n", clientId);
printf("Client ID Len: %lu\n", strlen(clientId));
        dataLen = strlen(clientId) + 1;
        if (sendData(dataLen, (void*)clientId) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending clientId to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the component name
        //***
        dataLen = strlen(compName) + 1;
        if (sendData(dataLen, (void*)compName) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending component name to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the component description
        //***
        dataLen = strlen(compDesc) + 1;
        if (sendData(dataLen, (void*)compDesc) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending component description to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the physical host name
        //***
        dataLen = strlen(physHostName) + 1;
        if (sendData(dataLen, (void*)physHostName) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending physical host name to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Get the response from the registrar
        //***
        int     length = 0;
        char    retValue[256];

        if (getResponse(0, length, retValue) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading register response from socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }
printf("Return Value: %s\n", retValue);

        //***
        // Disconnect from the Registrar
        //***
        disconnect();

        return getStateValue(retValue);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::getComponent()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::getComponent()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::getComponent(
//
// !RETURN VALUE:
//    int  number of bytes read from the socket;
//         ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  const char*               clientId,      // (in) the id for the client who
                                          //    created the service
  ESMCI_WebServCompSvrInfo*  compSvrInfo  // (out) structure into which the
                                          //    server information is put
  )
//
// !DESCRIPTION:
//    Notifies the Registrar that the component service has successfully
//    been started.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("RegistrarClient::getComponent()\n");
        int     localrc = 0;
        char    retValue[256];


        //***
        // Make sure compSvrInfo structure is not null
        //***
        if (compSvrInfo == NULL)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_ARG_BAD,
         "Invalid CompSvrInfo structure.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Connect to the server
        //***
        if (connect() == ESMF_FAILURE)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error connecting to Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the request identifier
        //***
        int     dataLen = strlen("get") + 1;
        if (sendData(dataLen, (void*)"get") != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending started request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the client id
        //***
        dataLen = strlen(clientId) + 1;
        if (sendData(dataLen, (void*)clientId) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending clientId to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Read number of component services that match the client id
        // (This should be a "1" if the component is found and "0" otherwise)
        //***
   char  numCompFound[64];
        int     length = 0;

        if (getResponse(0, length, numCompFound) <= 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to read client ID from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

   printf("Num Components Found: %s\n", numCompFound);

        if (strcmp(numCompFound, "1") != 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to find client with id in Registrar.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

        //***
        // Read client id
        //***
   char  retClientId[1024];
        length = 0;

        if (getResponse(0, length, retClientId) <= 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to read client ID from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

        compSvrInfo->setClientId(atoi(retClientId));
   printf("Client ID: %s\n", retClientId);

        //***
        // Read job id
        //***
   char  retJobId[1024];
        length = 0;

        if (getResponse(0, length, retJobId) <= 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to read job ID from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

        compSvrInfo->setJobId(retJobId);
   printf("Job ID: %s\n", retJobId);

        //***
        // Read host name
        //***
   char  retHostName[1024];
        length = 0;

        if (getResponse(0, length, retHostName) <= 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to read host name from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

        compSvrInfo->setHostName(retHostName);
   printf("Host Name: %s\n", retHostName);

        //***
        // Read port number
        //***
   char  retPortNum[1024];
        length = 0;

        if (getResponse(0, length, retPortNum) <= 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to read port number from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

        compSvrInfo->setPortNum(atoi(retPortNum));
   printf("Port Num: %s\n", retPortNum);

        //***
        // Read Component Name
        //***
   char  retCompName[1024];
        length = 0;

        if (getResponse(0, length, retCompName) <= 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to read component name from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

        compSvrInfo->setName(retCompName);
   printf("Comp Name: %s\n", retCompName);

        //***
        // Read Component Description
        //***
   char  retCompDesc[1024];
        length = 0;

        if (getResponse(0, length, retCompDesc) <= 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to read component description from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

        compSvrInfo->setDesc(retCompDesc);
   printf("Comp Desc: %s\n", retCompDesc);

        //***
        // Read Physical Host Name
        //***
   char  retPhysHostName[1024];
        length = 0;

        if (getResponse(0, length, retPhysHostName) <= 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to read physical host name from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

        compSvrInfo->setPhysHostName(retPhysHostName);
   printf("Physical Host Name: %s\n", retPhysHostName);

        //***
        // Read Current Status
        //***
   char  retStatus[1024];
        length = 0;

        if (getResponse(0, length, retStatus) <= 0)
   {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Unable to read status from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
   }

        compSvrInfo->setStatus(getStateValue(retStatus));
   printf("Status: %s\n", retStatus);

        //***
        // Disconnect from the Registrar
        //***
        disconnect();

        return getStateValue(retStatus);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::getStatus()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::getStatus()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::getStatus(
//
// !RETURN VALUE:
//    int  number of bytes read from the socket;
//         ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  const char*  clientId                 // (in) the id for the client who created the service
  )
//
// !DESCRIPTION:
//    Notifies the Registrar that the component service has successfully
//    been started.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("RegistrarClient::getStatus()\n");
        int     localrc = 0;
        char    retValue[256];

        //***
        // Connect to the server
        //***
        if (connect() == ESMF_FAILURE)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error connecting to Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the request identifier
        //***
        int     dataLen = strlen("getstatus") + 1;
        if (sendData(dataLen, (void*)"getstatus") != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending started request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the client id
        //***
        dataLen = strlen(clientId) + 1;
        if (sendData(dataLen, (void*)clientId) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending clientId to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Read Current Status
        //***
   char  retStatus[1024];
        int     length = 0;

        if (getResponse(0, length, retStatus) <= 0)
   {
        ESMC_LogDefault.MsgFoundError(
           ESMC_RC_FILE_READ,
           "Unable to read status from socket.",
           ESMC_CONTEXT, &localrc);

        return ESMF_FAILURE;
   }

   printf("Status: %s\n", retStatus);

        //***
        // Disconnect from the Registrar
        //***
        disconnect();

        return getStateValue(retStatus);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRegistrarClient::setStatus()"
//BOPI
// !ROUTINE:  ESMCI_WebServRegistrarClient::setStatus()
//
// !INTERFACE:
int  ESMCI_WebServRegistrarClient::setStatus(
//
// !RETURN VALUE:
//    int  number of bytes read from the socket.
//         ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  const char*  clientId,   // (in) the id of the client whose component svc is
                              //      to be unregistered
  const char*  status           // (in) the status to be set
  )
//
// !DESCRIPTION:
//    Sets the status of the component server in the Registrar.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("RegistrarClient::setStatus()\n");
        int     localrc = 0;

        //***
        // First, make sure that the status string that's being passed in is
        // a valid string...
        //***
        int     stateValue;
        if ((stateValue = getStateValue(status)) == ESMF_FAILURE)
        {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_ARG_BAD,
         "Invalid state string.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
        }

        //***
        // Connect to the server
        //***
        if (connect() == ESMF_FAILURE)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error connecting to Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the request identifier
        //***
        int     dataLen = strlen("setstatus") + 1;
        if (sendData(dataLen, (void*)"setstatus") != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending unregister request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the client id
        //***
        dataLen = strlen(clientId) + 1;
        if (sendData(dataLen, (void*)clientId) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending client ID to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the component server status
        //***
        dataLen = strlen(status) + 1;
        if (sendData(dataLen, (void*)status) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending status to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Get the response from the registrar
        //***
        int     length = 0;
        char    retValue[256];

        if (getResponse(0, length, retValue) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading unregister response from socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }
printf("Return Value: %s\n", retValue);

        //***
        // Disconnect from the Registrar
        //***
        disconnect();

        return getStateValue(retValue);
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
//         ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  const char*  clientId          // (in) the id of the client whose component service is
                   //      to be unregistered
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
        int     localrc = 0;
        char    retValue[256];

        //***
        // Connect to the server
        //***
        if (connect() == ESMF_FAILURE)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error connecting to Registrar.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the request identifier
        //***
        int     dataLen = strlen("unregister") + 1;
        if (sendData(dataLen, (void*)"unregister") != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending unregister request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Send the client id
        //***
        dataLen = strlen(clientId) + 1;
        if (sendData(dataLen, (void*)clientId) != dataLen)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending client ID to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        //***
        // Get the response from the registrar
        //***
        int     length = 0;
        if (getResponse(0, length, retValue) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading unregister response from socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }
printf("Return Value: %s\n", retValue);

        //***
        // Disconnect from the Registrar
        //***
        disconnect();

        return length;
}

} // end namespace
