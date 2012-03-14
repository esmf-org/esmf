// $Id: ESMCI_WebServ_F.C,v 1.12 2012/03/14 14:44:06 ksaint Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research,
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

#if !defined (ESMF_OS_MinGW)
#include <unistd.h>
#else
#include <Winsock.h>
#endif

#include "ESMCI_WebServComponentSvr.h"
#include "ESMCI_WebServNetEsmfServer.h"
#include "ESMCI_WebServNetEsmfClient.h"
#include "ESMCI_WebServRegistrarClient.h"
#include "ESMCI_Macros.h"
#include "ESMCI_Comp.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_WebServ_F.C,v 1.12 2012/03/14 14:44:06 ksaint Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------

ESMCI::ESMCI_WebServComponentSvr*	theComponentServer = NULL;
string		theClientId = "";


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_componentsvcloop()"
//BOPI
// !ROUTINE:  c_esmc_componentsvcloop()
//
// !INTERFACE:
void FTN_X(c_esmc_componentsvcloop)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  char*                   clientId, 		// (in) the client identifier
  ESMCI::GridComp*        comp,				// (in) the grid component
  ESMCI::State*           importState,		// (in) the component import state
  ESMCI::State*           exportState,		// (in) the component export state
  ESMCI::Clock*           clock,				// (in) the component clock
  ESMC_BlockingFlag*      blockingFlag,  	// (in) the blocking flag
  int*                    phase,         	// (in) the phase
  int*                    portNum,			// (in) the service port number
  int*                    rc,			      // (in) the return code
  ESMCI_FortranStrLenArg  clientIdLen		// (in) the length of the client id
  )
//
// !DESCRIPTION:
//    Creates a component service on the specified port and calls the
//    loop method to listen for client requests.
//
//EOPI
//-----------------------------------------------------------------------------
{
	printf("Port Number: %d\n", *portNum);
	printf("Client ID: %d\n", atoi(clientId));
	int	localrc = 0;

   //***
   // This loop should not return until either an "exit" message has been
   // received or an error has occurred.
   //***
	theComponentServer = 
		new ESMCI::ESMCI_WebServComponentSvr(*portNum, atoi(clientId));

	if (theComponentServer->requestLoop(comp, 
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
#define ESMC_METHOD "c_esmc_cplcomponentsvcloop()"
//BOPI
// !ROUTINE:  c_esmc_cplcomponentsvcloop()
//
// !INTERFACE:
void FTN_X(c_esmc_cplcomponentsvcloop)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  char*                   clientId, 		// (in) the client identifier
  ESMCI::CplComp*         comp,				// (in) the grid component
  ESMCI::State*           importState,		// (in) the component import state
  ESMCI::State*           exportState,		// (in) the component export state
  ESMCI::Clock*           clock,				// (in) the component clock
  ESMC_BlockingFlag*      blockingFlag,  	// (in) the blocking flag
  int*                    phase,         	// (in) the phase
  int*                    portNum,			// (in) the service port number
  int*                    rc,			      // (in) the return code
  ESMCI_FortranStrLenArg  clientIdLen		// (in) the length of the client id
  )
//
// !DESCRIPTION:
//    Creates a component service on the specified port and calls the
//    loop method to listen for client requests.
//
//EOPI
//-----------------------------------------------------------------------------
{
	printf("Port Number: %d\n", *portNum);
	printf("Client ID: %d\n", atoi(clientId));
	int	localrc = 0;

   //***
   // This loop should not return until either an "exit" message has been
   // received or an error has occurred.
   //***
//	ESMCI::ESMCI_WebServComponentSvr	server(*portNum);
	theComponentServer = new ESMCI::ESMCI_WebServComponentSvr(*portNum, atoi(clientId));

printf("Component Server Request Loop\n");
	if (theComponentServer->cplCompRequestLoop(comp, 
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
#define ESMC_METHOD "c_esmc_registercomponent()"
//BOPI
// !ROUTINE:  c_esmc_registercomponent()
//
// !INTERFACE:
void FTN_X(c_esmc_registercomponent)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  char*                   compName,		// (in) the grid component name
  char*                   compDesc,		// (in) the grid component description
  char*                   clientId, 	// (in) the client identifier
  int*                    portNum,     // (in) the service port number
  int*                    rc,          // (in) the return code
  ESMCI_FortranStrLenArg  compNameLen,	// (in) the length of the component name
  ESMCI_FortranStrLenArg  compDescLen,	// (in) the length of the comp desc
  ESMCI_FortranStrLenArg  clientIdLen	// (in) the length of the client id
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
	char	clientIdStr[ESMF_MAXSTR];
	char	portStr[ESMF_MAXSTR];
	char	hostStr[ESMF_MAXSTR];

	strncpy(descStr, compDesc, compDescLen);
	descStr[compDescLen] = '\0';
	string	descString(descStr);
	size_t	found = descString.find_last_not_of(' ');
	if (found != string::npos)
		descString.erase(found + 1);
	else
		descString.clear();

	strncpy(nameStr, compName, compNameLen);
	nameStr[compNameLen] = '\0';
	string	nameString(nameStr);
	found = nameString.find_last_not_of(' ');
	if (found != string::npos)
		nameString.erase(found + 1);
	else
		nameString.clear();

	strncpy(clientIdStr, clientId, clientIdLen);
	clientIdStr[clientIdLen] = '\0';
	string	clientIdString(clientIdStr);
	found = clientIdString.find_last_not_of(' ');
	if (found != string::npos)
		clientIdString.erase(found + 1);
	else
		clientIdString.clear();

	theClientId = clientIdString;

	sprintf(portStr, "%d", *portNum);
	gethostname(hostStr, ESMF_MAXSTR);

	printf("Name: %s\n", nameString.c_str());
	printf("Desc: %s\n", descString.c_str());
	printf("Port: %s\n", portStr);
	printf("Client ID: %s\n", clientIdString.c_str());
	printf("Host: %s\n", hostStr);

	ESMCI::ESMCI_WebServRegistrarClient	client("localhost", REGISTRAR_PORT);

	if (client.compStarted(clientIdString.c_str(), 
                          nameString.c_str(), 
                          descString.c_str(), 
                          hostStr) == ESMF_FAILURE)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_UNEXPECTED,
         "Error registering component service.",
         &localrc);

		*rc = localrc;
	}
printf("Successfully notified Registrar of component ready.\n");

   *rc = ESMF_SUCCESS;

	return;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_unregistercomponent()"
//BOPI
// !ROUTINE:  c_esmc_unregistercomponent()
//
// !INTERFACE:
void FTN_X(c_esmc_unregistercomponent)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  char*                   clientId,		// (in) the client identifier
  int*                    rc,          // (in) the return code
  ESMCI_FortranStrLenArg  clientIdLen	// (in) the length of the clientId
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
	char	clientIdStr[ESMF_MAXSTR];

	strncpy(clientIdStr, clientId, clientIdLen);
	clientIdStr[clientIdLen] = '\0';
	string	clientIdString(clientIdStr);
	size_t	found = clientIdString.find_last_not_of(' ');
	if (found != string::npos)
		clientIdString.erase(found + 1);
	else
		clientIdString.clear();

	printf("Client ID: %s\n", clientIdStr);

	ESMCI::ESMCI_WebServRegistrarClient	client("localhost", REGISTRAR_PORT);

	char	response[1024];
	if (client.setStatus(clientIdString.c_str(), 
                        client.getStateStr(NET_ESMF_STAT_DONE)) == ESMF_FAILURE)
	{
      ESMC_LogDefault.ESMC_LogMsgFoundError(
         ESMC_RC_FILE_UNEXPECTED,
         "Error unregistering component service.",
         &localrc);

		*rc = localrc;
	}

   *rc = ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_getportnum()"
//BOPI
// !ROUTINE:  c_esmc_getportnum()
//
// !INTERFACE:
void FTN_X(c_esmc_getportnum)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int*               portNum,			// (out) the service port number
  int*               rc			      // (out) the return code
  )
//
// !DESCRIPTION:
//    Determines a suitable, available port number for the service.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;

   *portNum = 27061;

   *rc = ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_addoutputfilename()"
//BOPI
// !ROUTINE:  c_esmc_addoutputfilename()
//
// !INTERFACE:
void FTN_X(c_esmc_addoutputfilename)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  char*                   filename,		// (in) the output filename
  int*                    rc,			   // (out) the return code
  ESMCI_FortranStrLenArg  filenameLen	// (in) the length of the filename
  )
//
// !DESCRIPTION:
//    Adds a filename to the list of output filenames.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	char	filenameStr[ESMF_MAXSTR];

   // TODO: everything
   if (theComponentServer != NULL)
	{
		strncpy(filenameStr, filename, filenameLen);
		theComponentServer->addOutputFilename(filenameStr);
	}

   *rc = ESMF_SUCCESS;
}

/*

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_addoutputdata()"
//BOPI
// !ROUTINE:  c_esmc_addoutputdata()
//
// !INTERFACE:
void FTN_X(c_esmc_addoutputdata)(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  double*                 timestamp,	// (in) 
  char*                   varName,		// (in) 
  double**                dataValues,	// (in) 
  int*                    rc,			   // (out) the return code
  ESMCI_FortranStrLenArg  varNameLen	// (in) the length of the var name
  )
//
// !DESCRIPTION:
//    Adds output data to the current output data structure.
//
//EOPI
//-----------------------------------------------------------------------------
{
	int	localrc = 0;
	char	varNameStr[ESMF_MAXSTR];

	strncpy(varNameStr, varName, varNameLen);
	printf("Var Name: %s\n", varName);
	for (int i = 0; i < 5; ++i)
	{
		printf("Values[%d]: %g\n", i, dataValues[i]);
	}

   // TODO: everything
   if (theComponentServer != NULL)
	{
		//strncpy(filenameStr, filename, filenameLen);
		//theComponentServer->addOutputFilename(filenameStr);
	}

   *rc = ESMF_SUCCESS;
}
*/
