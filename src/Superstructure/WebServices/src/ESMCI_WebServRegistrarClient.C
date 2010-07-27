#include "ESMCI_WebServRegistrarClient.h"

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

#include "ESMCI_WebServSocketUtils.h"


/*
*****************************************************************************
**
*****************************************************************************
*/
RegistrarClient::RegistrarClient(const char*  host,
                                 int          port)
{
	theHost = NULL;

	if (host == NULL)
	{
		host = "localhost";
	}

	setHost(host);
	setPort(port);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
RegistrarClient::~RegistrarClient()
{
	theSocket.disconnect();
	delete theHost;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  RegistrarClient::setHost(const char*  host)
{
	if (theHost)
	{
		delete theHost;
	}

	theHost = new char[strlen(host) + 1];
	strcpy(theHost, host);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  RegistrarClient::setPort(int  port)
{
	thePort = port;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  RegistrarClient::sendRequest(int    request,
                                  int    length,
                                  void*  data)
{
//printf("RegistrarClient::sendRequest()\n");
	int	bytesWritten = 0;

	if ((length > 0)  &&  (data != NULL))
	{
		bytesWritten = theSocket.write(length, data);
	}

	return bytesWritten;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  RegistrarClient::getResponse(int    request,
                                  int&   length,
                                  void*  data)
{
//printf("RegistrarClient::getResponse()\n");
	length = 0;
	theSocket.read(length, data);

	return length;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  RegistrarClient::registerComp(char*  name,
                                   char*  desc,
                                   char*  hostName,
                                   char*  portNum,
                                   void*  retValue)
{
//printf("RegistrarClient::registerComp()\n");
	theSocket.write(strlen("register") + 1, (char*)"register");
	theSocket.write(strlen(name) + 1, name);
	theSocket.write(strlen(desc) + 1, desc);
	theSocket.write(strlen(hostName) + 1, hostName);
	theSocket.write(strlen(portNum) + 1, portNum);

	int	length = 0;
	theSocket.read(length, retValue);

	return length;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  RegistrarClient::unregisterComp(char*  name,
                                     char*  hostName,
                                     char*  portNum,
                                     void*  retValue)
{
//printf("RegistrarClient::unregisterComp()\n");
	theSocket.write(strlen("unregister") + 1, (char*)"unregister");
	theSocket.write(strlen(name) + 1, name);
	theSocket.write(strlen(hostName) + 1, hostName);
	theSocket.write(strlen(portNum) + 1, portNum);

	int	length = 0;
	theSocket.read(length, retValue);

	return length;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  RegistrarClient::connect()
{
//printf("RegistrarClient::connect()\n");
	return theSocket.clientConnect(theHost, thePort);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  RegistrarClient::disconnect()
{
//printf("RegistrarClient::disconnect()\n");
	return theSocket.disconnect();
}
