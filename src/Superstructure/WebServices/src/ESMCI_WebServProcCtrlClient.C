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
#define ESMC_FILENAME "ESMCI_WebServProcCtrlClient.C"
//==============================================================================
//
// ESMC WebServProcCtrlClient method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ProcCtrlClient methods declared
// in the companion file ESMCI_WebServProcCtrlClient.h.  This code
// provides the functionality needed to communicate with an ESMF grid
// component service implemented with the ESMCI_WebServComponentSvr class.
// This class is intended to be used only by a Process Controller service which
// is the intermediary between the client (web service) and the component
// service.
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServProcCtrlClient.h"

#include <stdint.h>
#include <string.h>

#if !defined (ESMF_OS_MinGW)
#include <netdb.h>
#else
#include <Winsock.h>
#endif

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
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::ESMCI_WebServProcCtrlClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::ESMCI_WebServProcCtrlClient()
//
// !INTERFACE:
ESMCI_WebServProcCtrlClient::ESMCI_WebServProcCtrlClient(
//
//
// !ARGUMENTS:
//
  const char*  host,             // (in) the name of the host machine running the
                                 //   component service
  int          port,             // (in) the port number of the component service
                                 //   to which this client will connect
  const char*  userName, // (in) the login name for the user on the server
  const char*  password  // (in) the password for the user on the server
  ) : ESMCI_WebServNetEsmfClient(host, port)
//
// !DESCRIPTION:
//    Initialize the ESMF Component client with the name of the host and port
//    where the component service is running.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("ESMCI_WebServProcCtrlClient::ESMCI_WebServProcCtrlClient()\n");

        strcpy(theUserName, userName);
        strcpy(thePassword, password);

        setClientId(newClient());
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::~ESMCI_WebServProcCtrlClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::~ESMCI_WebServProcCtrlClient()
//
// !INTERFACE:
ESMCI_WebServProcCtrlClient::~ESMCI_WebServProcCtrlClient(
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
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::setClientId()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::setClientId()
//
// !INTERFACE:
void  ESMCI_WebServProcCtrlClient::setClientId(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int  clientId         // (in) the unique id of the client on the Process Controller
  )
//
// !DESCRIPTION:
//    Sets the id of the client on the Process Controller.
//
//EOPI
//-----------------------------------------------------------------------------
{
        theClientId = clientId;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::newClient()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::newClient()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrlClient::newClient(
//
// !RETURN VALUE:
//   int  the id of this client on the Process Controller (a number greater
//        than zero); ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Connects to the process controller, makes a request to create a new
//    client, retrieves the new client id, and then disconnects.
//
//EOPI
//-----------------------------------------------------------------------------
{
        printf("ESMCI_WebServProcCtrlClient::newClient()\n");

        int     localrc = 0;
        int     clientId = 0;
        int     bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to Process Controller socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "New Client" request...
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_NEW, 0, NULL)) != 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending new client request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if ((bytesSent = sendString(theUserName)) != (strlen(theUserName) + 1))
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending client user name to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if ((bytesSent = sendString(thePassword)) != (strlen(thePassword) + 1))
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending client password to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Retrieve the response... which should be the new client id
   //***
   if (getResponse(NET_ESMF_NEW, bufSize, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading init response from socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if (bufSize == 4)
        {
                clientId = ntohl(*((unsigned int*)buf));
                printf("Client ID: %d\n", clientId);
        }

   //***
   // Disconnect from the component service
   //***
        disconnect();

        return clientId;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::init()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::init()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrlClient::init(
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
        int     localrc = 0;
        int     status = 0;
        int     bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Initialize" request... along with the client identifier
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_INIT, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending init request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if (bytesSent == 4)
        {
      //***
      // Retrieve the response... which should be the server status
      //***
                if (getResponse(NET_ESMF_INIT, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading init response from socket.",
            ESMC_CONTEXT, &localrc);

         return ESMF_FAILURE;
      }

                if (bufSize == 4)
                {
                        status = ntohl(*((unsigned int*)buf));
printf("CLIENT: Status: %d\n", status);
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
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::run()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::run()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrlClient::run(
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
        int     localrc = 0;
        int     status = 0;
        int     bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Run" request... along with the client identifier
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_RUN, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending run request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if (bytesSent == 4)
        {
      //***
      // Retrieve the response... which should be the server status
      //***
                if (getResponse(NET_ESMF_RUN, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading run response from socket.",
            ESMC_CONTEXT, &localrc);

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
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::timestep()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::timestep()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrlClient::timestep(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  int   numTimesteps            // number of timesteps to run
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to timestep the
//    component, retrieve the server status, and then disconnect from
//    the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
        int     localrc = 0;
        int     status = 0;
        int     bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "timestep" request... along with the client identifier
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_TIMESTEP, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending run request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the number of timesteps to run parameter
   //***
   unsigned int   netNumTimesteps = htonl(numTimesteps);
   bytesSent = 0;

   if ((bytesSent = sendData(4, &netNumTimesteps)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending num timesteps to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if (bytesSent == 4)
        {
      //***
      // Retrieve the response... which should be the server status
      //***
                if (getResponse(NET_ESMF_RUN, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading timestep response from socket.",
            ESMC_CONTEXT, &localrc);

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
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::final()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::final()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrlClient::final(
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
        int     localrc = 0;
        int     status = 0;
        int     bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Finalize" request... along with the client identifier
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_FINAL, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending finalize request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if (bytesSent == 4)
        {
      //***
      // Retrieve the response... which should be the server status
      //***
                if (getResponse(NET_ESMF_FINAL, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading finalize response from socket.",
            ESMC_CONTEXT, &localrc);

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
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::dataDesc()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::dataDesc()
//
// !INTERFACE:
ESMCI_WebServDataDesc*  ESMCI_WebServProcCtrlClient::dataDesc(
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
//    the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
        int     localrc = 0;
        int     status = 0;
        int     bufSize = 0;
   char  buf[1024];

        ESMCI_WebServDataDesc*  dataDesc = NULL;

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return dataDesc;
   }

   //***
   // Send the "get data desc" request... along with the client identifier
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_DATA_DESC, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending run request to socket.",
         ESMC_CONTEXT, &localrc);

      return dataDesc;
   }

        //***
        // Get the data description from the server... which should include
        // the following:
   //   - number of variable names
   //   - the variable names (if any)
   //   - the number of lat values
   //   - the lat values
   //   - the number of lon values
   //   - the lon values
        //***

   int      numVarNames  = 0;
   string*  varNames     = NULL;
   int      numLatValues = 0;
   double*  latValues    = NULL;
   int      numLonValues = 0;
   double*  lonValues    = NULL;

   //***
   // Get Variable Names
   //***
   if (getResponse(NET_ESMF_DATA_DESC, bufSize, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading get files response - number of vars from socket.",
         ESMC_CONTEXT, &localrc);

      return dataDesc;
   }

   if (bufSize == 4)
   {
      char  varName[1024];
      int   numVars = ntohl(*((unsigned int*)buf));
      printf("Num Variables: %d\n", numVars);

      numVarNames = numVars;
      varNames = new string[numVars];

      for (int i = 0; i < numVars; ++i)
      {
         if (getResponse(NET_ESMF_DATA_DESC, bufSize, buf) <= 0)
         {
            ESMC_LogDefault.MsgFoundError(
               ESMC_RC_FILE_READ,
               "Error reading get description - var name from socket.",
               ESMC_CONTEXT, &localrc);

            return dataDesc;
         }

         strcpy(varName, buf);
         //printf("Variable Name: %s\n", varName);

         varNames[i] = varName;
      }
        }

   //***
   // Get Latitude Values
   //***
   if (getResponse(NET_ESMF_DATA_DESC, bufSize, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading get files response - number of lats from socket.",
         ESMC_CONTEXT, &localrc);

      return dataDesc;
   }

   if (bufSize == 4)
   {
      double*  lats;
      int      numLats = ntohl(*((unsigned int*)buf));
      printf("Num Latitudes: %d\n", numLats);

      numLatValues = numLats;
      latValues = new double[numLatValues];

      if (numLats > 0)
      {
         bufSize = sizeof(double) * numLats;

         if (getResponse(NET_ESMF_DATA_DESC, bufSize, buf) != bufSize)
         {
            ESMC_LogDefault.MsgFoundError(
               ESMC_RC_FILE_READ,
               "Error reading get description - lat values from socket.",
               ESMC_CONTEXT, &localrc);

            return dataDesc;
         }

         lats = (double*)buf;

         for (int i = 0; i < numLats; ++i)
         {
            ntohll((uint64_t)(lats[i]));
            //printf("Lat Value: %g\n", lats[i]);

            latValues[i] = lats[i];
         }
      }
        }

   //***
   // Get Longitude Values
   //***
   if (getResponse(NET_ESMF_DATA_DESC, bufSize, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading get files response - number of lons from socket.",
         ESMC_CONTEXT, &localrc);

      return dataDesc;
   }

   if (bufSize == 4)
        {
      double*  lons;
      int      numLons = ntohl(*((unsigned int*)buf));
      printf("Num Longitudes: %d\n", numLons);

      numLonValues = numLons;
      lonValues = new double[numLonValues];

      if (numLons > 0)
      {
         bufSize = sizeof(double) * numLons;

         if (getResponse(NET_ESMF_DATA_DESC, bufSize, buf) != bufSize)
         {
            ESMC_LogDefault.MsgFoundError(
               ESMC_RC_FILE_READ,
               "Error reading get description - lon values from socket.",
               ESMC_CONTEXT, &localrc);

            return dataDesc;
         }

         lons = (double*)buf;

         for (int i = 0; i < numLons; ++i)
         {
            ntohll((uint64_t)(lons[i]));
            //printf("Lon Value: %g\n", lons[i]);

            lonValues[i] = lons[i];
         }
      }
        }

   dataDesc = new ESMCI_WebServDataDesc(numVarNames,
                                                 varNames,
                                                 numLatValues,
                                                 latValues,
                                                 numLonValues,
                                                 lonValues);

   //***
   // Retrieve the response... which should be the server status
   //***
        if (getResponse(NET_ESMF_RUN, bufSize, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading timestep response from socket.",
          ESMC_CONTEXT, &localrc);

      return dataDesc;
   }

        if (bufSize == 4)
        {
                status = ntohl(*((unsigned int*)buf));
                //printf("Status: %d\n", status);
        }

   //***
   // Disconnect from the component service
   //***
        disconnect();

        return dataDesc;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::outputData()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::outputData()
//
// !INTERFACE:
ESMCI_WebServDataContent*  ESMCI_WebServProcCtrlClient::outputData(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  double                timestamp
  )
//
// !DESCRIPTION:
//    the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
        int     localrc = 0;
        int     status = 0;
        int     bufSize = 0;
   char  buf[10240];

        ESMCI_WebServDataContent*       outputData = NULL;

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return outputData;
   }

   //***
   // Send the "get data" request... along with the client identifier
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_DATA, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending run request to socket.",
         ESMC_CONTEXT, &localrc);

      return outputData;
   }

   //***
   // Send the timestamp value
   //***
   double   netTimestamp = timestamp;
   htonll((uint64_t)netTimestamp);
   bytesSent = 0;

   if ((bytesSent = sendData(8, (unsigned char*)(&netTimestamp))) != 8)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending timestamp to socket.",
         ESMC_CONTEXT, &localrc);

      return outputData;
   }

   //***
   // Read Number of variables in the output data
   //***
   if (getResponse(NET_ESMF_DATA, bufSize, buf) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading get data - number of vars from socket.",
         ESMC_CONTEXT, &localrc);

      return outputData;
   }

   int   numVars = ntohl(*((unsigned int*)buf));
   printf("Num Variables: %d\n", numVars);

   //***
   // Read the variable names from the socket
   //***
   string*  varNames = new string[numVars];
   char     varName[1024];

   for (int i = 0; i < numVars; ++i)
   {
      if (getResponse(NET_ESMF_DATA_DESC, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading get data - var name from socket.",
            ESMC_CONTEXT, &localrc);

         return outputData;
      }
      strcpy(varName, buf);
      printf("Variable Name: %s\n", varName);

      varNames[i] = varName;
   }

   //***
   // Read Number of lat values in the output data
   //***
   if (getResponse(NET_ESMF_DATA, bufSize, buf) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading get data - number of lats from socket.",
         ESMC_CONTEXT, &localrc);

      return outputData;
   }

   int   numLats = ntohl(*((unsigned int*)buf));
   printf("Num Lats: %d\n", numLats);

   //***
   // Read Number of lon values in the output data
   //***
   if (getResponse(NET_ESMF_DATA, bufSize, buf) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading get data - number of lons from socket.",
         ESMC_CONTEXT, &localrc);

      return outputData;
   }

   int   numLons = ntohl(*((unsigned int*)buf));
   printf("Num Lons: %d\n", numLons);

   int      dataSize = numLats * numLons;
printf("Data Size: %d\n", dataSize);

   outputData = new ESMCI_WebServDataContent(numLats, numLons);

   for (int i = 0; i < numVars; ++i)
   {
      double*  dataValues = NULL;

      if (dataSize > 0)
      {
         bufSize = sizeof(double) * dataSize;
         int   bytesRead = 0;

         if ((bytesRead = getResponse(NET_ESMF_DATA, bufSize, buf)) != bufSize)
         {
            ESMC_LogDefault.MsgFoundError(
                  ESMC_RC_FILE_READ,
                  "Error reading get output data - data values from socket.",
                  ESMC_CONTEXT, &localrc);

            return outputData;
         }

         dataValues = (double*)buf;

         for (int j = 0; j < dataSize; ++j)
         {
            ntohll((uint64_t)(dataValues[j]));
            //printf("Data Value: %g\n", dataValues[j]);
         }

         outputData->addDataValues(varNames[i], dataValues);
      }
   }

   //***
   // Retrieve the response... which should be the server status
   //***
        if (getResponse(NET_ESMF_DATA, bufSize, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading timestep response from socket.",
          ESMC_CONTEXT, &localrc);

      return outputData;
   }

        if (bufSize == 4)
        {
                status = ntohl(*((unsigned int*)buf));
                //printf("Status: %d\n", status);
        }

   //***
   // Disconnect from the component service
   //***
        disconnect();

        return outputData;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::state()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::state()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrlClient::state(
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
        int     localrc = 0;
        int     status = 0;
        int     bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Get State" request... along with the client identifier
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_STATE, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending get state request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if (bytesSent == 4)
        {
      //***
      // Retrieve the response... which should be the server status
      //***
                if (getResponse(NET_ESMF_STATE, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading get status response from socket.",
            ESMC_CONTEXT, &localrc);

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
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::end()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::end()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrlClient::end(
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
        int     localrc = 0;
        int     status = 0;
        int     bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "End Client" request... along with the client identifier
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_END, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending end client request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

        if (bytesSent == 4)
        {
      //***
      // Retrieve the response... which should be the server status
      //***
                if (getResponse(NET_ESMF_END, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading end client response from socket.",
            ESMC_CONTEXT, &localrc);

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
#define ESMC_METHOD "ESMCI_WebServProcCtrlClient::killServer()"
//BOPI
// !ROUTINE:  ESMCI_WebServProcCtrlClient::killServer()
//
// !INTERFACE:
int  ESMCI_WebServProcCtrlClient::killServer(
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
        int     localrc = 0;
        int     status = 0;
        int     bufSize = 0;
   char  buf[1024];

   //***
   // Connect to the component service
   //***
        if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Exit" request... the client id does not need to be sent.
   //***
        unsigned int    netClientId = htonl(theClientId);
        int                             bytesSent = 0;

        if ((bytesSent = sendRequest(NET_ESMF_EXIT, 4, &netClientId)) != 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending exit client request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Disconnect from the component service
   //***
        disconnect();

        return ESMF_SUCCESS;
}


} // end namespace
