// $Id: ESMCI_WebServ_F.C,v 1.5 2011/01/05 20:05:48 svasquez Exp $
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
static const char *const version = "$Id: ESMCI_WebServ_F.C,v 1.5 2011/01/05 20:05:48 svasquez Exp $";
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
  ESMC_BlockingFlag* blockingFlag,  // (in) the blocking flag
  int*               phase,         // (in) the phase
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
                          *phase, 
                          *blockingFlag) != ESMF_SUCCESS)
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
  char*                   compName,		// (in) the grid component name
  char*                   compDesc,		// (in) the grid component description
  char*                   hostName, 	// (in) the service host name
  int*                    portNum,     // (in) the service port number
  int*                    rc,          // (in) the return code
  ESMCI_FortranStrLenArg  compNameLen,	// (in) the length of the component name
  ESMCI_FortranStrLenArg  compDescLen,	// (in) the length of the comp desc
  ESMCI_FortranStrLenArg  hostNameLen	// (in) the length of the host name
  )
//
// !DESCRIPTION:
//    Parses the input parameters and uses that information to register the
//    component with the Registrar.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;

	char	nameStr[ESMF_MAXSTR];
	char	descStr[ESMF_MAXSTR];
	char	hostStr[ESMF_MAXSTR];
	char	portStr[ESMF_MAXSTR];

	strncpy(nameStr, compName, compNameLen);
	strncpy(descStr, compDesc, compDescLen);
	strncpy(hostStr, hostName, hostNameLen);
	sprintf(portStr, "%d", portNum);

	//printf("Name: %s\n", nameStr);
	//printf("Desc: %s\n", descStr);
	//printf("Host: %s\n", hostStr);
	//printf("Port: %s\n", portStr);

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
	if (client.registerComp(nameStr, 
                           descStr, 
                           hostStr, 
                           portStr, 
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
  char*                   compName,		// (in) the grid component name
  char*                   hostName, 	// (in) the service host name
  int*                    portNum,     // (in) the service port number
  int*                    rc,          // (in) the return code
  ESMCI_FortranStrLenArg  compNameLen,	// (in) the length of the component name
  ESMCI_FortranStrLenArg  hostNameLen	// (in) the length of the host name
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

	int	localrc = 0;
	char	nameStr[ESMF_MAXSTR];
	char	hostStr[ESMF_MAXSTR];
	char	portStr[ESMF_MAXSTR];

	strncpy(nameStr, compName, compNameLen);
	strncpy(hostStr, hostName, hostNameLen);
	sprintf(portStr, "%d", portNum);

	//printf("Name: %s\n", nameStr);
	//printf("Host: %s\n", hostStr);
	//printf("Port: %s\n", portStr);

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
	if (client.unregisterComp(nameStr, 
                             hostStr, 
                             portStr, 
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
