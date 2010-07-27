#include "ESMCI_WebServNetEsmfServer.h"

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
#include "ESMCI_IO_NetCDF.h"

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
};

using namespace ESMCI;

/*
*****************************************************************************
**
*****************************************************************************
*/
NetEsmfServer::NetEsmfServer(int  port)
{
	theStatus = (char*)NET_ESMF_STAT_IDLE;
	theNextClientId = 101;

	setPort(port);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
NetEsmfServer::~NetEsmfServer()
{
	theSocket.disconnect();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::setPort(int  port)
{
	thePort = port;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::requestLoop(ESMCI::GridComp*   comp,
                                 ESMCI::State*      importState,
                                 ESMCI::State*      exportState,
                                 ESMCI::Clock*      clock,
                                 int                phase,
                                 ESMC_BlockingFlag  blockingFlag)
{
printf("NetEsmfServer::grid requestLoop()\n");
	theCompType = ESMC_COMPTYPE_GRID;

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
void  NetEsmfServer::requestLoop(ESMCI::CplComp*    comp,
                                 ESMCI::State*      importState,
                                 ESMCI::State*      exportState,
                                 ESMCI::Clock*      clock,
                                 int                phase,
                                 ESMC_BlockingFlag  blockingFlag)
{
printf("NetEsmfServer::coupler requestLoop()\n");
	theCompType = ESMC_COMPTYPE_COUPLER;

	theCouplerComp = comp;
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
int  NetEsmfServer::getNextRequest()
{
//printf("NetEsmfServer::getNextRequest()\n");
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
int  NetEsmfServer::serviceRequest(int  request)
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

//printf("Sending msg: %s\n", theMsg);

	if (request != NET_ESMF_NEW)
	{
		//theSocket.send(theMsg);
	}
	theSocket.close();

	return request;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfServer::getRequestId(const char  request[])
{
//printf("NetEsmfServer::getRequestId()\n");
	if (strcmp(request, "NEW")   == 0)	return NET_ESMF_NEW;
	if (strcmp(request, "EXIT")  == 0)	return NET_ESMF_EXIT;
	if (strcmp(request, "INIT")  == 0)	return NET_ESMF_INIT;
	if (strcmp(request, "RUN")   == 0)	return NET_ESMF_RUN;
	if (strcmp(request, "FINAL") == 0)	return NET_ESMF_FINAL;
	if (strcmp(request, "STATE") == 0)	return NET_ESMF_STATE;
	if (strcmp(request, "FILES") == 0)	return NET_ESMF_FILES;
	if (strcmp(request, "END")   == 0)	return NET_ESMF_END;
	if (strcmp(request, "PING")  == 0)	return NET_ESMF_PING;

	return NET_ESMF_UNKN;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
char*  NetEsmfServer::getRequestFromId(int  id)
{
//printf("NetEsmfServer::getRequestFromId()\n");
	switch (id)
	{
	case NET_ESMF_EXIT:	return (char*)"EXIT";
	case NET_ESMF_NEW:	return (char*)"NEW";
	case NET_ESMF_INIT:	return (char*)"INIT";
	case NET_ESMF_RUN:	return (char*)"RUN";
	case NET_ESMF_FINAL:	return (char*)"FINAL";
	case NET_ESMF_STATE:	return (char*)"STATE";
	case NET_ESMF_FILES:	return (char*)"FILES";
	case NET_ESMF_END:	return (char*)"END";
	case NET_ESMF_PING:	return (char*)"PING";
	default:					return (char*)"UNKN";
	}

	return (char*)"UNKN";
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::processNew()
{
	printf("\n\nSERVER: processing New\n");

	//***
	// Read the client name
	//***
	int	bytesRead = 0;
	char	buf[1024];
	theSocket.read(bytesRead, buf);
printf("Buffer: %s\n", buf);

	//***
	// Generate a new client id and add the new client to the collection 
	// of clients
	//***
	int			clientId = getNextClientId();
	ClientInfo*	newClient = new ClientInfo(clientId);
	theClients[clientId] = newClient;

	newClient->setStatus(NET_ESMF_STAT_READY);

	//***
	// Send back the new client id
	//***
	int	netClientId = htonl(clientId);
printf("Network client id: %d\n", netClientId);
	theSocket.write(4, &netClientId);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::processInit()
{
	printf("\n\nSERVER: processing Init\n");

	int	status = NET_ESMF_STAT_IDLE;

	//***
	// Get the client id 
	//***
	int	bytesRead = 0;
	char	buf[1024];

	theSocket.read(bytesRead, buf);

   int	clientId = ntohl(*((unsigned int*)buf));
printf("Client ID: %d\n", clientId);

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
	}

	//***
	// Now that everything's been read off the socket, lookup the client info
	// based on the client id.  If the client can't be found, then send back
	// an error
	//***
	map<int, ClientInfo*>::iterator	iter;
	ClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);
		return;
	}

	clientInfo = iter->second;
clientInfo->print();

	//***
	// If a filename was specified, create the import state object
	//***
	if (numFiles > 0)
	{
		//clientInfo->setImportFilename(filename);

		//***
		//	Create state object from specified file
		// TODO: Add a flag that the calling program can set to indicate whether
		//       not there is a state to import
		// TODO: Read from the file as part of the netCDF web service 
		//       (instead of as a local file)
		//***
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
	}

	//***
	// Call the component initialize
	//***
	clientInfo->setStatus(NET_ESMF_STAT_BUSY);

	int	rc = 0;
	if (theCompType == ESMC_COMPTYPE_GRID)
	{
printf("initializing a grid component\n");
/*
		theGridComp->initialize(theImportState, 
                              theExportState, 
                              theClock, 
                              thePhase, 
                              &rc);
*/
      FTN(f_esmf_processinit)(theGridComp,
                              theImportState, 
                              theExportState, 
                              theClock, 
                              thePhase, 
                              &rc);
printf("Return code: %d\n", rc);
	}
	else if (theCompType == ESMC_COMPTYPE_COUPLER)
	{
printf("initializing a coupler component\n");
		theCouplerComp->initialize(theImportState, 
                                 theExportState, 
                                 theClock, 
                                 thePhase, 
                                 &rc);
	}
printf("Initialize Status: %d\n", rc);

	clientInfo->setStatus(NET_ESMF_STAT_INIT_DONE);

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
   // TODO: determine status from rc
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);
	theSocket.write(4, &netStatus);
clientInfo->print();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::processRun()
{
	printf("\n\nSERVER: processing Run\n");

	int	status = NET_ESMF_STAT_IDLE;

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
	map<int, ClientInfo*>::iterator	iter;
	ClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);
		return;
	}

	clientInfo = iter->second;
clientInfo->print();

	//***
	// Call the component run
	//***
	clientInfo->setStatus(NET_ESMF_STAT_BUSY);

	int	rc = 0;
	if (theCompType == ESMC_COMPTYPE_GRID)
	{
/*
		theGridComp->run(theImportState, 
                       theExportState, 
                       theClock, 
                       thePhase, 
                       &rc);
*/
      FTN(f_esmf_processrun)(theGridComp,
                             theImportState, 
                             theExportState, 
                             theClock, 
                             thePhase, 
                             &rc);
	}
	else if (theCompType == ESMC_COMPTYPE_COUPLER)
	{
		theCouplerComp->run(theImportState, 
                          theExportState, 
                          theClock, 
                          thePhase, 
                          &rc);
	}

	clientInfo->setStatus(NET_ESMF_STAT_RUN_DONE);

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
   // TODO: determine status from rc
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::processFinal()
{
	printf("\n\nSERVER: processing Final\n");

	int	status = NET_ESMF_STAT_IDLE;

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
	map<int, ClientInfo*>::iterator	iter;
	ClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);
		return;
	}

	clientInfo = iter->second;
clientInfo->print();

	//***
	// Set the status to indicate that the service is busy right now
	//***
	clientInfo->setStatus(NET_ESMF_STAT_BUSY);

	//***
	// Write the export file out to the local server file location
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
	if (theCompType == ESMC_COMPTYPE_GRID)
	{
/*
		theGridComp->finalize(theImportState, 
                            theExportState, 
                            theClock, 
                            thePhase, 
                            &rc);
*/
      FTN(f_esmf_processfinal)(theGridComp,
                               theImportState, 
                               theExportState, 
                               theClock, 
                               thePhase, 
                               &rc);
	}
	else if (theCompType == ESMC_COMPTYPE_COUPLER)
	{
		theCouplerComp->finalize(theImportState, 
                               theExportState, 
                               theClock, 
                               thePhase, 
                               &rc);
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
		char		localFilename[256];
		char		exportFilename[512];
		char*		webServer = "http://27thstsoftware.com:8080";
		char*		openDapPath = "opendap/data/nc";

		sprintf(localFilename, "/usr/local/share/hyrax/data/nc/afile_%d.nc", 
                             clientInfo->clientId());
		sprintf(exportFilename, "%s/%s/afile_%d.nc", 
                              webServer, openDapPath, clientInfo->clientId());

		copyFile("afile.nc", localFilename);
		clientInfo->setExportFilename(exportFilename);
	}
*/

	clientInfo->setStatus(NET_ESMF_STAT_FINAL_DONE);

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
   // TODO: determine status from rc
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::processState()
{
	printf("\n\nSERVER: processing State\n");

	int	status = NET_ESMF_STAT_IDLE;

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
	map<int, ClientInfo*>::iterator	iter;
	ClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);
		return;
	}

	clientInfo = iter->second;
clientInfo->print();

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
   // TODO: determine status from rc
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::processFiles()
{
	printf("\n\nSERVER: processing Files\n");

	int	status = NET_ESMF_STAT_IDLE;
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
	map<int, ClientInfo*>::iterator	iter;
	ClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		numFiles = 0;
		unsigned int  netNumFiles = htonl(numFiles);
		theSocket.write(4, &netNumFiles);

		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);

		return;
	}

	clientInfo = iter->second;
clientInfo->print();

	//***
	// Write the file information back to the client
	//***
	numFiles = 2;
	char	fileInfoBuf[1024];

	unsigned int  netNumFiles = htonl(numFiles);
	theSocket.write(4, &netNumFiles);

	strcpy(fileInfoBuf, "import");
	theSocket.write(strlen(fileInfoBuf) + 1, fileInfoBuf);

	strcpy(fileInfoBuf, "file1.nc");
	//strcpy(fileInfoBuf, clientInfo->importFilename());
	theSocket.write(strlen(fileInfoBuf) + 1, fileInfoBuf);

	strcpy(fileInfoBuf, "export");
	theSocket.write(strlen(fileInfoBuf) + 1, fileInfoBuf);

	strcpy(fileInfoBuf, "file2.nc");
	//strcpy(fileInfoBuf, clientInfo->exportFilename());
	theSocket.write(strlen(fileInfoBuf) + 1, fileInfoBuf);

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::processEnd()
{
	printf("\n\nSERVER: processing End\n");

	int	status = NET_ESMF_STAT_IDLE;

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
	map<int, ClientInfo*>::iterator	iter;
	ClientInfo*								clientInfo = NULL;

	if ((iter = theClients.find(clientId)) == theClients.end())
	{
		status = NET_ESMF_STAT_ERROR;
		unsigned int	netStatus = htonl(status);
		theSocket.write(4, &netStatus);
		return;
	}

	clientInfo = iter->second;
clientInfo->print();

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
	unsigned int	netStatus = htonl(status);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::processPing()
{
	printf("\n\nSERVER: processing Ping\n");
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfServer::getNextClientId()
{
	int	nextClientId = theNextClientId;

	++theNextClientId;

	return nextClientId;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfServer::copyFile(const char*  srcFilename,
                              const char*  destFilename)
{
	fstream	fin(srcFilename, ios::in | ios::binary);
	fstream	fout(destFilename, ios::out | ios::binary);

	if ((fin == NULL)  ||  (fout == NULL))
	{
		cerr << "Error copying file" << endl;
		return;
	}

	char	c;
	while (!fin.eof())
	{
		fin.get(c);
		fout.put(c);
	}
}
