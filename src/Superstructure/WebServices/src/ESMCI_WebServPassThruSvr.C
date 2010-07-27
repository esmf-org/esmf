#include "ESMCI_WebServPassThruSvr.h"

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
#include "ESMCI_WebServCompSvrClient.h"


/*
*****************************************************************************
**
*****************************************************************************
*/
PassThruSvr::PassThruSvr(int     port,
                         string  camDir)
{
	theStatus = (char*)NET_ESMF_STAT_IDLE;
	theNextClientId = 101;

	theCAMDir = camDir;
	theOutputFile = NULL;

	setPort(port);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
PassThruSvr::~PassThruSvr()
{
	theSocket.disconnect();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  PassThruSvr::setPort(int  port)
{
	thePort = port;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  PassThruSvr::requestLoop()
{
printf("PassThruSvr::requestLoop()\n");
	
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
int  PassThruSvr::getNextRequest()
{
//printf("PassThruSvr::getNextRequest()\n");
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
int  PassThruSvr::serviceRequest(int  request)
{
printf("PassThruSvr::serviceRequest()\n");
printf("Request ID: %d\n", request);
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

	case NET_ESMF_DATA: 
		processGetData();
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
int  PassThruSvr::getRequestId(const char  request[])
{
//printf("PassThruSvr::getRequestId()\n");
	if (strcmp(request, "NEW")   == 0)	return NET_ESMF_NEW;
	if (strcmp(request, "EXIT")  == 0)	return NET_ESMF_EXIT;
	if (strcmp(request, "INIT")  == 0)	return NET_ESMF_INIT;
	if (strcmp(request, "RUN")   == 0)	return NET_ESMF_RUN;
	if (strcmp(request, "FINAL") == 0)	return NET_ESMF_FINAL;
	if (strcmp(request, "STATE") == 0)	return NET_ESMF_STATE;
	if (strcmp(request, "FILES") == 0)	return NET_ESMF_FILES;
	if (strcmp(request, "DATA")  == 0)	return NET_ESMF_DATA;
	if (strcmp(request, "END")   == 0)	return NET_ESMF_END;
	if (strcmp(request, "PING")  == 0)	return NET_ESMF_PING;

	return NET_ESMF_UNKN;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
char*  PassThruSvr::getRequestFromId(int  id)
{
//printf("PassThruSvr::getRequestFromId()\n");
	switch (id)
	{
	case NET_ESMF_EXIT:	return (char*)"EXIT";
	case NET_ESMF_NEW:	return (char*)"NEW";
	case NET_ESMF_INIT:	return (char*)"INIT";
	case NET_ESMF_RUN:	return (char*)"RUN";
	case NET_ESMF_FINAL:	return (char*)"FINAL";
	case NET_ESMF_STATE:	return (char*)"STATE";
	case NET_ESMF_FILES:	return (char*)"FILES";
	case NET_ESMF_DATA:	return (char*)"DATA";
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
void  PassThruSvr::processNew()
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

	//***
	// KDS: In the future, I think I want to startup a new component server
	//      for each client.
	//***
	newClient->setServerHost("localhost");
	newClient->setServerPort(27060);

	newClient->setStatus(NET_ESMF_STAT_READY);

	//***
	// Send back the new client id
	//***
	int	netClientId = htonl(clientId);
//printf("Network client id: %d\n", netClientId);
	theSocket.write(4, &netClientId);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  PassThruSvr::processInit()
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
printf("Num Files: %d\n", numFiles);

	char	filename[1024];

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
	status = clientInfo->status();

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
	}

	//***
	// Call the component initialize (must be in the READY state before the
	// initialize can be called)
	//***
printf("Connecting to Component Server\n");
	CompSvrClient	client(clientInfo->serverHost().c_str(), 
                         clientInfo->serverPort(), 
                         clientInfo->clientId());
printf("Connected to Component Server\n");

printf("Getting State\n");
	status = client.state();
printf("State: %d\n", status);
	clientInfo->setStatus(status);

	if (status == NET_ESMF_STAT_READY)
	{
		status = client.init();
		//clientInfo->setStatus(NET_ESMF_STAT_INITIALIZING);
		clientInfo->setStatus(status);
	}
	else
	{
		// Should return an error message
	}

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	status = clientInfo->status();
	unsigned int	netStatus = htonl(status);
	theSocket.write(4, &netStatus);
//clientInfo->print();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  PassThruSvr::processRun()
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
	status = clientInfo->status();

	//***
	// Call the component run (must have completed the initialize phase before
	// the run could be called)
	//***
	CompSvrClient	client(clientInfo->serverHost().c_str(), 
                         clientInfo->serverPort(), 
                         clientInfo->clientId());

	status = client.state();
	clientInfo->setStatus(status);

	if (status == NET_ESMF_STAT_INIT_DONE)
	{
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
void  PassThruSvr::processFinal()
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
	status = clientInfo->status();

	//***
	// Call the component finalize (must have completed at least the initalize
   // phase, and possibly the run phase before the finalize can be called 
   // (KDS: assuming you can call finalize after calling just initialize, but
   //       not sure if that is true.)
	//***
	CompSvrClient	client(clientInfo->serverHost().c_str(), 
                         clientInfo->serverPort(), 
                         clientInfo->clientId());

	status = client.state();
	clientInfo->setStatus(status);

	if ((status == NET_ESMF_STAT_RUN_DONE)  ||
	    (status == NET_ESMF_STAT_INIT_DONE))
	{
		status = client.final();
		//clientInfo->setStatus(NET_ESMF_STAT_FINALIZING);
		clientInfo->setStatus(status);
	}
	else
	{
		// Should return an error message
	}

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
void  PassThruSvr::processState()
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
	status = clientInfo->status();

	CompSvrClient	client(clientInfo->serverHost().c_str(), 
                         clientInfo->serverPort(), 
                         clientInfo->clientId());

	status = client.state();
	clientInfo->setStatus(status);

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
void  PassThruSvr::processFiles()
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
// KDS: Get status from component server
//***
//***
// KDS: Make call to component server to get filenames... set status to whatever
//      status gets returned
//***
	status = clientInfo->status();

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
void  PassThruSvr::processGetData()
{
	printf("\n\nSERVER: processing GetData\n");

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

	char	varName[256];
	theSocket.read(bytesRead, buf);
	strncpy(varName, (char*)buf, 255);
//printf("Var Name: %s\n", varName);

	//***
	// These next values are read as strings and then converted to double values
	//***
	char	tempValue[256];

	// Read time
	theSocket.read(bytesRead, buf);
	strncpy(tempValue, (char*)buf, 255);
//printf("Time: %s\n", tempValue);
	double	timeValue = atof(tempValue);

	// Read lat
	theSocket.read(bytesRead, buf);
	strncpy(tempValue, (char*)buf, 255);
//printf("Lat: %s\n", tempValue);
	double	latValue = atof(tempValue);

	// Read lon
	theSocket.read(bytesRead, buf);
	strncpy(tempValue, (char*)buf, 255);
//printf("Lon: %s\n", tempValue);
	double	lonValue = atof(tempValue);

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
	// If the data files (to be added to ClientInfo) have not been retrieved
	// from the component server, then get them.
	// KDS: Right now, I'm hardcoding the output filename...
	//***
	if (theOutputFile == NULL)
	{
		theOutputFile = new CAMOutputFile(theCAMDir + 
													 "/camrun.cam2.rh0.0000-01-02-00000.nc");

		//***
		// KDS: Make call to component server to get filenames... set status 
		//      to whatever status gets returned
		//***
		status = clientInfo->status();
	}

	//***
	// Read the data from the specified file
	//***
	double	dataValue = theOutputFile->getDataValue(varName, 
                                                    timeValue, 
                                                    latValue, 
                                                    lonValue);
//printf("Data Value: %e\n", dataValue);

	//***
	// Write the data back to the client
	//***
	sprintf(tempValue, "%e", dataValue);
	theSocket.write(strlen(tempValue) + 1, tempValue);

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
void  PassThruSvr::processEnd()
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
	status = clientInfo->status();

	CompSvrClient	client(clientInfo->serverHost().c_str(), 
                         clientInfo->serverPort(), 
                         clientInfo->clientId());

	status = client.end();
	clientInfo->setStatus(status);

   //***
   // Remove the client from the collection of clients
	//***
	theClients.erase(clientId);
	delete clientInfo;

	//***
	// Send the current state back to the client (use the return code from
	// the component initialize call to determine the state)
	//***
	status = NET_ESMF_STAT_DONE;
	unsigned int	netStatus = htonl(status);
	theSocket.write(4, &netStatus);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  PassThruSvr::processPing()
{
	printf("\n\nSERVER: processing Ping\n");
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  PassThruSvr::getNextClientId()
{
	int	nextClientId = theNextClientId;

	++theNextClientId;

	return nextClientId;
}
