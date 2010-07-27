#include "ESMCI_WebServComponentSvr.h"

#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <iostream>
#include <fstream>

#include "ESMCI_WebServSocketUtils.h"
#include <ESMCI_IO_NetCDF.h>

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

using namespace ESMCI;

/*
*****************************************************************************
**
*****************************************************************************
*/
ComponentSvr::ComponentSvr(int  port)
{
	pthread_mutex_init(&theStatusMutex, NULL);

	setPort(port);

	setStatus(NET_ESMF_STAT_READY);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
ComponentSvr::~ComponentSvr()
{
	theSocket.disconnect();

	pthread_mutex_destroy(&theStatusMutex);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ComponentSvr::setPort(int  port)
{
	thePort = port;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ComponentSvr::requestLoop(ESMCI::GridComp*   comp,
                                ESMCI::State*      importState,
                                ESMCI::State*      exportState,
                                ESMCI::Clock*      clock,
                                int                phase,
                                ESMC_BlockingFlag  blockingFlag)
{
printf("ComponentSvr::grid requestLoop()\n");
	theGridComp = comp;
	theImportState = importState;
	theExportState = exportState;
	theClock	= clock;
	thePhase = phase;
	theBlockingFlag = blockingFlag;
	
comp->print("");
	if (theSocket.connect(thePort) < 0)
	{
		return;
	}

	int	request;

	do
	{
		request = getNextRequest();
		serviceRequest(request);
	} while (request != NET_ESMF_EXIT);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  ComponentSvr::getNextRequest()
{
//printf("ComponentSvr::getNextRequest()\n");
	theSocket.accept();

	int	n;
	char	requestStr[50];

	theSocket.read(n, requestStr);

	//printf("SERVER: request: %s\n", requestStr);

	return getRequestId(requestStr);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  ComponentSvr::serviceRequest(int  request)
{
//printf("ComponentSvr::serviceRequest()\n");
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

	case NET_ESMF_END: 
		processEnd();
		break;

	default:
		break;
	}

	theSocket.close();

	return request;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  ComponentSvr::getRequestId(const char  request[])
{
//printf("ComponentSvr::getRequestId()\n");
	if (strcmp(request, "INIT")  == 0)	return NET_ESMF_INIT;
	if (strcmp(request, "RUN")   == 0)	return NET_ESMF_RUN;
	if (strcmp(request, "FINAL") == 0)	return NET_ESMF_FINAL;
	if (strcmp(request, "STATE") == 0)	return NET_ESMF_STATE;
	if (strcmp(request, "FILES") == 0)	return NET_ESMF_FILES;
	if (strcmp(request, "END")   == 0)	return NET_ESMF_END;

	return NET_ESMF_UNKN;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
char*  ComponentSvr::getRequestFromId(int  id)
{
//printf("ComponentSvr::getRequestFromId()\n");
	switch (id)
	{
	case NET_ESMF_INIT:	return (char*)"INIT";
	case NET_ESMF_RUN:	return (char*)"RUN";
	case NET_ESMF_FINAL:	return (char*)"FINAL";
	case NET_ESMF_STATE:	return (char*)"STATE";
	case NET_ESMF_FILES:	return (char*)"FILES";
	case NET_ESMF_END:	return (char*)"END";
	default:					return (char*)"UNKN";
	}

	return (char*)"UNKN";
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ComponentSvr::setStatus(int  status)
{
	pthread_mutex_lock(&theStatusMutex);
	theCurrentStatus = status;
	pthread_mutex_unlock(&theStatusMutex);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ComponentSvr::processInit()
{
	printf("\n\nSERVER: processing Init\n");

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	theSocket.read(bytesRead, buf);

   theCurrentClientId = ntohl(*((unsigned int*)buf));
printf("Client ID: %d\n", theCurrentClientId);

	//***
	// Get the number of files (should be either 0 or 1)... if there's 1, then
	// get the filename
	//***
	theSocket.read(bytesRead, buf);

   int	numFiles = ntohl(*((unsigned int*)buf));
	char	filename[1024];
printf("Num Files: %d\n", numFiles);

	if (numFiles > 0)
	{
		theSocket.read(bytesRead, buf);
		strcpy(filename, (char*)buf);
printf("Filename: %s\n", filename);

		//***
		// TODO: Add the filename to a list of filenames in the ComponentSvr class
		//***
	}

	//***
	// If a filename was specified, create the import state object
	//***
	if (numFiles > 0)
	{
		//***
		//	Create state object from specified file
		// TODO: Add a flag that the calling program can set to indicate whether
		//       not there is a state to import
		// TODO: Read from the file as part of the netCDF web service 
		//       (instead of as a local file)
		//***
/*
** KDS:
** I'm not worrying about import files at this state of the prototype...
** but in a future version, we'll want to read the import file(s) into the
** import state object
**
		if (1) // change this to if (hasExportFile) when ready
		{
			char		localFilename[256];

			sprintf(localFilename, "/usr/local/share/hyrax/data/nc/%s", 
                             	  filename);
	
			int			rc = 0;
			IO_NetCDF*	netCdfFile = ESMCI_IO_NetCDFCreate(
												strlen(localFilename),
												localFilename,
												ESMC_NULL_POINTER,
												&rc);

printf("Reading file: %s\n", localFilename);
			netCdfFile->read(strlen(localFilename), localFilename);

			//***
			// Ideally, I'd just say theImportState = netCdfFile->getState()...
			// however, I don't think I can override the pointer location for the
			// import state can be changed, so instead, I'm copying over the
			// contents from the local state object to theImportState object.
			//***
			State*			localState = netCdfFile->getState();
			vector<string>	arrayNames = localState->getArrayNames();

			for (int i = 0; i < arrayNames.size(); ++i)
			{
				Array*	thisArray;
				localState->getArray((char*)(arrayNames[i].c_str()), &thisArray);

				theImportState->addArray(thisArray);
			}
			//theImportState->print();

			ESMCI_IO_NetCDFDestroy(&netCdfFile);
		}
*/
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
		pthread_t	thread;
		int			rc = 0;

		rc = pthread_create(&thread, NULL, initThreadStartup, this);
	}

	//***
	// Send the current state back to the client 
	//***
	unsigned int	netStatus = htonl(theCurrentStatus);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ComponentSvr::processRun()
{
	printf("\n\nSERVER: processing Run\n");

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	theSocket.read(bytesRead, buf);

   int	clientId = ntohl(*((unsigned int*)buf));
printf("Client ID: %d\n", clientId);

	if (clientId != theCurrentClientId)
	{
		int				status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);
		return;
	}

printf("Current Status: %d\n", theCurrentStatus);
printf("Init done value: %d\n", NET_ESMF_INIT_DONE);
	if (theCurrentStatus == NET_ESMF_STAT_INIT_DONE)
	{
		//***
		// Call the component run
		//***
printf("Setting Status\n");
		setStatus(NET_ESMF_STAT_RUNNING);

		pthread_t	thread;
		int			rc = 0;
printf("Creating thread\n");
		rc = pthread_create(&thread, NULL, runThreadStartup, this);
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	unsigned int	netStatus = htonl(theCurrentStatus);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ComponentSvr::processFinal()
{
	printf("\n\nSERVER: processing Final\n");

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	theSocket.read(bytesRead, buf);

   int	clientId = ntohl(*((unsigned int*)buf));
printf("Client ID: %d\n", clientId);

	if (clientId != theCurrentClientId)
	{
		int				status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);
		return;
	}

	if ((theCurrentStatus == NET_ESMF_STAT_INIT_DONE)  ||
	    (theCurrentStatus == NET_ESMF_STAT_RUN_DONE))
	{
		//***
		// Set the status to indicate that the service is busy right now
		//***
		setStatus(NET_ESMF_STAT_FINALIZING);

		pthread_t	thread;
		int			rc = 0;
		rc = pthread_create(&thread, NULL, finalThreadStartup, this);
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	unsigned int	netStatus = htonl(theCurrentStatus);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ComponentSvr::processState()
{
	printf("\n\nSERVER: processing State\n");

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	theSocket.read(bytesRead, buf);

   int	clientId = ntohl(*((unsigned int*)buf));
printf("Client ID: %d\n", clientId);

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
/*
	if (clientId != theCurrentClientId)
	{
		int				status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);
		return;
	}
*/

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
printf("The Current Status: %d\n", theCurrentStatus);
	unsigned int	netStatus = htonl(theCurrentStatus);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ComponentSvr::processFiles()
{
	printf("\n\nSERVER: processing Files\n");

	int	numFiles = 0;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	theSocket.read(bytesRead, buf);

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
		theSocket.write(4, &netNumFiles);

		int				status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);

		return;
	}

	//***
	// Write the file information back to the client
	//***
	if (theCurrentStatus == NET_ESMF_STAT_FINAL_DONE)
	{
		//***
		// KDS: All of this needs to change... I'm hardcoding this for now...
		//***
		numFiles = 1;
		char	fileInfoBuf[1024];

		unsigned int  netNumFiles = htonl(numFiles);
		theSocket.write(4, &netNumFiles);

		//strcpy(fileInfoBuf, "import");
		//theSocket.write(strlen(fileInfoBuf) + 1, fileInfoBuf);

		//strcpy(fileInfoBuf, "file1.nc");
		//strcpy(fileInfoBuf, clientInfo->importFilename());
		//theSocket.write(strlen(fileInfoBuf) + 1, fileInfoBuf);

		strcpy(fileInfoBuf, "export");
		theSocket.write(strlen(fileInfoBuf) + 1, fileInfoBuf);

		strcpy(fileInfoBuf, "camrun.cam2.rh0.000-01-02-00000.nc");
		//strcpy(fileInfoBuf, clientInfo->exportFilename());
		theSocket.write(strlen(fileInfoBuf) + 1, fileInfoBuf);
/*
		numFiles = 0;
		unsigned int  netNumFiles = htonl(numFiles);
		theSocket.write(4, &netNumFiles);
*/
	}
	else
	{
		numFiles = 0;
		unsigned int  netNumFiles = htonl(numFiles);
		theSocket.write(4, &netNumFiles);
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	unsigned int	netStatus = htonl(theCurrentStatus);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  ComponentSvr::processEnd()
{
	printf("\n\nSERVER: processing End\n");

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	theSocket.read(bytesRead, buf);

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
		theSocket.write(4, &netStatus);
		return;
	}

	setStatus(NET_ESMF_STAT_DONE);

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	unsigned int	netStatus = htonl(theCurrentStatus);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void*  ComponentSvr::runInit(void)
{
printf("initializing a grid component\n");
// Check current status
		int	rc = 0;
   	FTN(f_esmf_processinit)(theGridComp,
                              theImportState, 
                              theExportState, 
                              theClock, 
                              thePhase, 
                              &rc);
printf("Initialize Status: %d\n", rc);
sleep(5);

		setStatus(NET_ESMF_STAT_INIT_DONE);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void*  ComponentSvr::runRun(void)
{
printf("ComponentSvr::runRun()\n");

		int	rc = 0;
   	FTN(f_esmf_processrun)(theGridComp,
                          theImportState, 
                          theExportState, 
                          theClock, 
                          thePhase, 
                          &rc);
printf("Run Status: %d\n", rc);
sleep(5);

		setStatus(NET_ESMF_STAT_RUN_DONE);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void*  ComponentSvr::runFinal(void)
{

		//***
		// Write the export file out to the local server file location
		// KDS: All of this needs to change...
		// TODO: Add a flag that the calling program can set to indicate whether
		//       not there is a state to export
		// TODO: Write directory to the file as part of the netCDF web service 
		//       (instead of as a local file)
		//***
/*
		if (1) // change this to if (hasExportFile) when ready
		{
			char		localFilename[256];
			char		exportFilename[512];
			char*		webServer = "http://27thstsoftware.com:8080";
			char*		openDapPath = "opendap/data/nc";

			sprintf(localFilename, "/usr/local/share/hyrax/data/nc/afile_%d.nc", 
                             clientInfo->clientId());
			sprintf(exportFilename, "%s/%s/afile_%d.nc", 
                              webServer, openDapPath, clientInfo->clientId());

			int			rc = 0;
			IO_NetCDF*	netCdfFile = ESMCI_IO_NetCDFCreate(
												strlen(localFilename),
												localFilename,
												ESMC_NULL_POINTER,
												&rc);

			netCdfFile->setState(theExportState);
			//theExportState->print();

printf("Writing file: %s\n", localFilename);
			netCdfFile->write(strlen(localFilename), localFilename);

			ESMCI_IO_NetCDFDestroy(&netCdfFile);

			clientInfo->setExportFilename(exportFilename);
		}
*/

		//***
		// Call the component finalize
		//***
		int	rc = 0;
      FTN(f_esmf_processfinal)(theGridComp,
                               theImportState, 
                               theExportState, 
                               theClock, 
                               thePhase, 
                               &rc);
printf("Finalize Status: %d\n", rc);
sleep(5);

		setStatus(NET_ESMF_STAT_FINAL_DONE);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void*  initThreadStartup(void*  tgtObject)
{
	ComponentSvr*	svrObject = (ComponentSvr*)tgtObject;

	svrObject->runInit();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void*  runThreadStartup(void*  tgtObject)
{
printf("runThreadStartup()\n");
	ComponentSvr*	svrObject = (ComponentSvr*)tgtObject;

	svrObject->runRun();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void*  finalThreadStartup(void*  tgtObject)
{
	ComponentSvr*	svrObject = (ComponentSvr*)tgtObject;

	svrObject->runFinal();
}
