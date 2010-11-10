// $Id: ESMCI_WebServ_F.C,v 1.3 2010/11/10 20:16:35 ksaint Exp $
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
#define ESMC_FILENAME "ESMCI_WebServ_F.C"
//==============================================================================
//
// ESMC WebServInterface method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C Interface methods declared
// in the companion file ESMCI_WebServInterface.h.  This code
// provides the interfaces to the methods needed to implement a component
// service with a Fortran app driver.
//
//-----------------------------------------------------------------------------

#include "ESMCI_WebServ.h"

#include <stdio.h>
#include <string.h>
#include "ESMCI_WebServComponentSvr.h"
#include "ESMCI_WebServNetEsmfServer.h"
#include "ESMCI_WebServNetEsmfClient.h"
#include "ESMCI_WebServRegistrarClient.h"
#include "ESMCI_Macros.h"
#include "ESMCI_Comp.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_WebServ_F.C,v 1.3 2010/11/10 20:16:35 ksaint Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_componentsvcloop()"
//BOPI
// !ROUTINE:  c_esmc_componentsvcloop()
//
// !INTERFACE:
void FTN(c_esmc_componentsvcloop)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  ESMCI::GridComp*   comp,				// (in) the grid component
  ESMCI::State*      importState,	// (in) the component import state
  ESMCI::State*      exportState,	// (in) the component export state
  ESMCI::Clock*      clock,			// (in) the component clock
  int*               portNum,			// (in) the service port number
  int*               rc			      // (in) the return code
  )
//
// !DESCRIPTION:
//    Creates a component service on the specified port and calls the
//    loop method to listen for client requests.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("Port Number: %d\n", *portNum);
	int	localrc = 0;

   //***
   // This loop should not return until either an "exit" message has been
   // received or an error has occurred.
   //***
	ESMCI::ESMCI_WebServComponentSvr	server(*portNum);

	if (server.requestLoop(comp, 
                          importState, 
                          exportState, 
                          clock, 
                          0, 
                          ESMF_BLOCKING) != ESMF_SUCCESS)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error during request loop setup.",
         &localrc);

		*rc = localrc;
		return;
	}

   *rc = ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_gridserviceloop()"
//BOPI
// !ROUTINE:  c_esmc_gridserviceloop()
//
// !INTERFACE:
void FTN(c_esmc_gridserviceloop)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  ESMCI::GridComp*   comp,				// (in) the grid component
  ESMCI::State*      importState,	// (in) the component import state
  ESMCI::State*      exportState,	// (in) the component export state
  ESMCI::Clock*      clock,			// (in) the component clock
  int*               portNum,			// (in) the service port number
  int*               rc			      // (in) the return code
  )
//
// !DESCRIPTION:
//    Creates a grid component service on the specified port and calls the
//    loop method to listen for client requests.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("Port Number: %d\n", *portNum);
	int	localrc = 0;

	ESMCI::ESMCI_WebServNetEsmfServer	server(*portNum);

	if (server.requestLoop(comp, 
                          importState, 
                          exportState, 
                          clock, 
                          0, 
                          ESMF_BLOCKING) != ESMF_SUCCESS)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error during request loop setup.",
         &localrc);

		*rc = localrc;
		return;
	}

   *rc = ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_couplerserviceloop()"
//BOPI
// !ROUTINE:  c_esmc_couplerserviceloop()
//
// !INTERFACE:
void FTN(c_esmc_couplerserviceloop)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  ESMCI::CplComp*    comp,				// (in) the coupler component
  ESMCI::State*      importState,	// (in) the component import state
  ESMCI::State*      exportState,	// (in) the component export state
  ESMCI::Clock*      clock,			// (in) the component clock
  int*               portNum,			// (in) the service port number
  int*               rc			      // (in) the return code
  )
//
// !DESCRIPTION:
//    Creates a coupler component service on the specified port and calls the
//    loop method to listen for client requests.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("Port Number: %d\n", *portNum);
	int	localrc = 0;

	ESMCI::ESMCI_WebServNetEsmfServer	server(*portNum);

	if (server.requestLoop(comp, 
                          importState, 
                          exportState, 
                          clock, 
                          0, 
                          ESMF_BLOCKING) != ESMF_SUCCESS)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Error during request loop setup.",
         &localrc);

		*rc = localrc;
		return;
	}

   *rc = ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_registercomponent()"
//BOPI
// !ROUTINE:  c_esmc_registercomponent()
//
// !INTERFACE:
void FTN(c_esmc_registercomponent)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  char*         params, 	// (in) the input parameters tokenized in a string
  int*          rc,        // (in) the return code
  ESMCI_FortranStrLenArg  param_len	// (in) the length of the input parameters string
  )
//
// !DESCRIPTION:
//    Parses the input parameters and uses that information to register the
//    component with the Registrar.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("registerComponent()\n");
	//printf("Params: %s\n", params);
	//printf("Length: %d\n", param_len);

	int	localrc = 0;

	char	paramStr[param_len + 1];
	strncpy(paramStr, params, param_len);
	paramStr[param_len] = '\0';

	char	name[param_len];
	char	desc[param_len];
	char	hostName[param_len];
	char	portNum[param_len];

	strcpy(name, strtok(paramStr, ":"));
	strcpy(desc, strtok(NULL, ":"));
	strcpy(hostName, strtok(NULL, ":"));
	strcpy(portNum, strtok(NULL, ":"));

	//printf("Name: %s\n", name);
	//printf("Desc: %s\n", desc);
	//printf("Host: %s\n", hostName);
	//printf("Port: %s\n", portNum);

	ESMCI::ESMCI_WebServRegistrarClient	client("localhost", REGISTRAR_PORT);

	if (client.connect() < 0)
   {
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		*rc = localrc;
		return;
   }

	char	response[1024];
	if (client.registerComp(name, 
                           desc, 
                           hostName, 
                           portNum, 
                           response) == ESMF_FAILURE)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_UNEXPECTED,
         "Error registering component service.",
         &localrc);

		*rc = localrc;
	}

	printf("Response: %s\n", response);
	client.disconnect();

   *rc = ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_unregistercomponent()"
//BOPI
// !ROUTINE:  c_esmc_unregistercomponent()
//
// !INTERFACE:
void FTN(c_esmc_unregistercomponent)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  char*         params, 	// (in) the input parameters tokenized in a string
  int*          rc,        // (in) the return code
  ESMCI_FortranStrLenArg  param_len	// (in) the length of the input parameters string
  )
//
// !DESCRIPTION:
//    Parses the input parameters and uses that information to register the
//    component with the Registrar.
//
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("unregisterComponent()\n");
	//printf("Params: %s\n", params);
	//printf("Length: %d\n", param_len);

	int	localrc = 0;

	char	paramStr[param_len + 1];
	strncpy(paramStr, params, param_len);
	paramStr[param_len] = '\0';

	char	name[param_len];
	char	hostName[param_len];
	char	portNum[param_len];

	strcpy(name, strtok(paramStr, ":"));
	strcpy(hostName, strtok(NULL, ":"));
	strcpy(portNum, strtok(NULL, ":"));

	//printf("Name: %s\n", name);
	//printf("Host: %s\n", hostName);
	//printf("Port: %s\n", portNum);

	ESMCI::ESMCI_WebServRegistrarClient	client("localhost", REGISTRAR_PORT);

	if (client.connect() < 0)
   {
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_OPEN,
         "Unable to connect to server socket.",
         &localrc);

		*rc = localrc;
		return;
   }

	char	response[1024];
	if (client.unregisterComp(name, 
                             hostName, 
                             portNum, 
                             response) == ESMF_FAILURE)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_UNEXPECTED,
         "Error unregistering component service.",
         &localrc);

		*rc = localrc;
	}


	printf("Response: %s\n", response);
	client.disconnect();

   *rc = ESMF_SUCCESS;
}
