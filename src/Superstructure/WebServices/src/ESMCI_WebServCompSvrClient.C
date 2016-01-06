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
// This class is intended to be used only by a Process Controller service which
// is the intermediary between the client (web service) and the component
// service.
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServCompSvrClient.h"

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
  int          clientId // (in) the id of the client on the Process Controller
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

	theOutputDataDesc = NULL;
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
  int  clientId	// (in) the unique id of the client on the Process Controller
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
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Initialize" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

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
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Run" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::timestep()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::timestep()
//
// !INTERFACE:
int  ESMCI_WebServCompSvrClient::timestep(
//
// !RETURN VALUE:
//   int  the current state of the component service;
//        ESMF_FAILURE if an error occurs
//
// !ARGUMENTS:
//
  int  numTimesteps		// The number of timesteps to run
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to run the
//    component for the specified number of timesteps, retrieve the server 
//    status, and then disconnect from the server.
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
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Timestep" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_TIMESTEP, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending timestep request to socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

	//***
	// Send the number of timesteps to run parameter
	//***
	unsigned int	netNumTimesteps = htonl(numTimesteps);
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
		if (getResponse(NET_ESMF_TIMESTEP, bufSize, buf) <= 0)
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
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Finalize" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

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
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Get State" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

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
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return dataFiles;
   }

   //***
   // Send the "Get Files" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_FILES, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending get files request to socket.",
         ESMC_CONTEXT, &localrc);

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
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading get files response - number of files from socket.",
            ESMC_CONTEXT, &localrc);

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
         		ESMC_LogDefault.MsgFoundError(
            		ESMC_RC_FILE_READ,
            		"Error reading get files response - file type from socket.",
            		ESMC_CONTEXT, &localrc);

         		return dataFiles;
      		}
				strcpy(fileType, buf);
				printf("File Type: %s\n", fileType);

				if (getResponse(NET_ESMF_FILES, bufSize, buf) <= 0)
      		{
         		ESMC_LogDefault.MsgFoundError(
            		ESMC_RC_FILE_READ,
            		"Error reading get files response - filename from socket.",
            		ESMC_CONTEXT, &localrc);

         		return dataFiles;
      		}
				strcpy(fileName, buf);
				printf("File Name: %s\n", fileName);

				dataFiles.push_back(fileName);
			}
		}

		if (getResponse(NET_ESMF_FILES, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading get data response - status from socket.",
            ESMC_CONTEXT, &localrc);

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
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::dataDesc()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::dataDesc()
//
// !INTERFACE:
ESMCI_WebServDataDesc*  ESMCI_WebServCompSvrClient::dataDesc(
//
// !RETURN VALUE:
//   ESMCI_WebServDataDesc*  pointer to the data descriptor object
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to get the description
//    of the output data, retrieves the description and the component status, 
//    and then disconnects from the server.
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
	ESMCI_WebServDataDesc*	dataDesc = NULL;

	if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return dataDesc;
   }

   //***
   // Send the "Get Data Desc" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_DATA_DESC, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending get data description request to socket.",
         ESMC_CONTEXT, &localrc);

      return dataDesc;
   }

	int		numVarNames  = 0;
	string*	varNames     = NULL;
	int		numLatValues = 0;
	double*	latValues    = NULL;
	int		numLonValues = 0;
	double*	lonValues    = NULL;

	if (bytesSent == 4)
	{
      //***
      // Retrieve the response... which should include the following:
		//   - number of variable names
		//   - the variable names (if any)
		//   - the number of lat values
		//   - the lat values
		//   - the number of lon values
		//   - the lon values
      //***

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
			char	varName[1024];
			int	numVars = ntohl(*((unsigned int*)buf));
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
			double*	lats;
			int		numLats = ntohl(*((unsigned int*)buf));
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
			double*	lons;
			int		numLons = ntohl(*((unsigned int*)buf));
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

		theOutputDataDesc = new ESMCI_WebServDataDesc(numVarNames,
	                                                 varNames,
	                                                 numLatValues,
	                                                 latValues,
	                                                 numLonValues,
	                                                 lonValues);

		if (getResponse(NET_ESMF_DATA_DESC, bufSize, buf) <= 0)
      {
         ESMC_LogDefault.MsgFoundError(
            ESMC_RC_FILE_READ,
            "Error reading get data response - status from socket.",
            ESMC_CONTEXT, &localrc);

         return dataDesc;
      }
		status = ntohl(*((unsigned int*)buf));
		printf("Status: %d\n", status);
	}

   //***
   // Disconnect from the component service
   //***
	disconnect();

	dataDesc = theOutputDataDesc;
	return dataDesc;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServCompSvrClient::outputData()"
//BOPI
// !ROUTINE:  ESMCI_WebServCompSvrClient::outputData()
//
// !INTERFACE:
ESMCI_WebServDataContent*  ESMCI_WebServCompSvrClient::outputData(
//
// !RETURN VALUE:
//   ESMCI_WebServDataContent*  pointer to the data content object
//
// !ARGUMENTS:
//
  double      timestamp, // (in) timestamp for which the output data is returned
  int*        retNumVars,
  string**    retVarNames,
  int*        retNumLats,
  int*        retNumLons
  )
//
// !DESCRIPTION:
//    Connects to the component server, makes a request to get the output data 
//    for a specified timestamp, retrieves the output data and the component 
//    status, and then disconnects from the server.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	int	status = 0;
	int	bufSize = 0;
   char  buf[10240];

   //***
   // Connect to the component service
   //***
	ESMCI_WebServDataContent*	outputData = NULL;

	if (connect() < 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return outputData;
   }

   //***
   // Send the "Get Data" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_DATA, 4, &netClientId)) != 4)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_WRITE,
         "Error sending get data description request to socket.",
         ESMC_CONTEXT, &localrc);

      return outputData;
   }

	//***
	// Send the timestamp value
	//***
	double	netTimestamp = timestamp;
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
printf("timestamp sent\n");

   //***
   // Retrieve the response... which should include an array of double values
	// for each variable name.  Now, in order to know the order of the variable
	// name values as well as the size of the data, the server first sends
	// the number of variables, the variable names, and the number of latitudes
	// and longitudes.
   //***

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

	int	numVars = ntohl(*((unsigned int*)buf));
	printf("Num Variables: %d\n", numVars);

	*retNumVars = numVars;

	//***
	// Read the variable names from the socket
	//***
	string*	varNames = new string[numVars];
	char		varName[1024];

	*retVarNames = new string[numVars];

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
		(*retVarNames)[i] = varName;
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

	int	numLats = ntohl(*((unsigned int*)buf));
	printf("Num Lats: %d\n", numLats);

	*retNumLats = numLats;

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

	int	numLons = ntohl(*((unsigned int*)buf));
	printf("Num Lons: %d\n", numLons);

	*retNumLons = numLons;

	int		dataSize = numLats * numLons;
printf("Data Size: %d\n", dataSize);

	outputData = new ESMCI_WebServDataContent(numLats, numLons);

	for (int i = 0; i < numVars; ++i)
	{
		double*	dataValues = NULL;
	
		if (dataSize > 0)
		{
			bufSize = sizeof(double) * dataSize;
			int	bytesRead = 0;

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
	// Get the Status
	//***
	if (getResponse(NET_ESMF_DATA, bufSize, buf) <= 0)
   {
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_READ,
         "Error reading get data response - status from socket.",
         ESMC_CONTEXT, &localrc);

      return outputData;
   }
	status = ntohl(*((unsigned int*)buf));
	printf("Status: %d\n", status);

   //***
   // Disconnect from the component service
   //***
	disconnect();

	return outputData;
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
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "End Client" request... along with the client identifier
   //***
	unsigned int	netClientId = htonl(theClientId);
	int				bytesSent = 0;

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
      ESMC_LogDefault.MsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         ESMC_CONTEXT, &localrc);

      return ESMF_FAILURE;
   }

   //***
   // Send the "Exit" request... the client id does not need to be sent.
   //***
	int				bytesSent = 0;

	if ((bytesSent = sendRequest(NET_ESMF_EXIT, 0, NULL)) != 0)
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
