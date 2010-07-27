#include "ESMCI_WebServNetEsmfClient.h"

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
NetEsmfClient::NetEsmfClient(const char*  host,
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
NetEsmfClient::~NetEsmfClient()
{
	theSocket.disconnect();
	delete theHost;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfClient::setHost(const char*  host)
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
void  NetEsmfClient::setPort(int  port)
{
	thePort = port;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfClient::sendRequest(int    request,
                                int    length,
                                void*  data)
{
//printf("NetEsmfClient::sendRequest()\n");
	char*		requestStr = getRequestFromId(request);

	theSocket.send(requestStr);
	//printf("\nCLIENT: req: %s\n", requestStr);

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
int  NetEsmfClient::getResponse(int    request,
                                int&   length,
                                void*  data)
{
//printf("NetEsmfClient::getResponse()\n");
	length = 0;
	theSocket.read(length, data);

	return length;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfClient::connect()
{
//printf("NetEsmfClient::connect()\n");
	return theSocket.connect(theHost, thePort);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  NetEsmfClient::disconnect()
{
//printf("NetEsmfClient::disconnect()\n");
	return theSocket.disconnect();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfClient::getRequestId(const char  request[])
{
//printf("NetEsmfClient::getRequestId()\n");
	if (strcmp(request, "EXIT")  == 0)	return NET_ESMF_EXIT;
	if (strcmp(request, "NEW")   == 0)	return NET_ESMF_NEW;
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
char*  NetEsmfClient::getRequestFromId(int  id)
{
//printf("NetEsmfClient::getRequestFromId()\n");
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
int  NetEsmfClient::newClient(const char*  clientName)
{
	int	clientId = -1;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return clientId;
	}

	strcpy(buf, clientName);
	bufSize = strlen(clientName) + 1;

	int	bytesSent = sendRequest(NET_ESMF_NEW, bufSize, buf);

	if (bytesSent == bufSize)
	{
		getResponse(NET_ESMF_NEW, bufSize, buf);

		if (bufSize == 4)
		{
			clientId = ntohl(*((unsigned int*)buf));
printf("Client ID: %d\n", clientId);
		}
	}

	disconnect();

	return clientId;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfClient::init(int  clientId)
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	//strcpy(buf, clientName);
	//bufSize = strlen(clientName) + 1;

	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = sendRequest(NET_ESMF_INIT, 4, &netClientId);

	if (bytesSent == 4)
	{
		int	numFiles = 0;
		unsigned int	netNumFiles = htonl(numFiles);
		int	bytesWritten = theSocket.write(4, &netNumFiles);

		getResponse(NET_ESMF_INIT, bufSize, buf);

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
//printf("Status: %d\n", status);
		}
	}

	disconnect();

	return status;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfClient::init(int          clientId,
                         const char*  filename)
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = sendRequest(NET_ESMF_INIT, 4, &netClientId);

	if (bytesSent == 4)
	{
		int				numFiles = 1;
		unsigned int	netNumFiles = htonl(numFiles);

		int	bytesWritten = theSocket.write(4, &netNumFiles);
		bytesWritten = theSocket.write(strlen(filename) + 1, (void*)filename);

		getResponse(NET_ESMF_INIT, bufSize, buf);

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
printf("Status: %d\n", status);
		}
	}

	disconnect();

	return status;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfClient::run(int  clientId)
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = sendRequest(NET_ESMF_RUN, 4, &netClientId);

	if (bytesSent == 4)
	{
		getResponse(NET_ESMF_RUN, bufSize, buf);

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
printf("Status: %d\n", status);
		}
	}

	disconnect();

	return status;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfClient::final(int  clientId)
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = sendRequest(NET_ESMF_FINAL, 4, &netClientId);

	if (bytesSent == 4)
	{
		getResponse(NET_ESMF_FINAL, bufSize, buf);

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
printf("Status: %d\n", status);
		}
	}

	disconnect();

	return status;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfClient::state(int  clientId)
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = sendRequest(NET_ESMF_STATE, 4, &netClientId);

	if (bytesSent == 4)
	{
		getResponse(NET_ESMF_STATE, bufSize, buf);

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
printf("Status: %d\n", status);
		}
	}

	disconnect();

	return status;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
vector<string>  NetEsmfClient::files(int  clientId)
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	vector<string>		dataFiles;

	if (connect() < 0)
	{
		return dataFiles;
	}

	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = sendRequest(NET_ESMF_FILES, 4, &netClientId);

	if (bytesSent == 4)
	{
		getResponse(NET_ESMF_FILES, bufSize, buf);

		if (bufSize == 4)
		{
			char	fileType[1024];
			char	fileName[1024];
			int	numFiles = ntohl(*((unsigned int*)buf));
printf("Num Files: %d\n", numFiles);

			for (int i = 0; i < numFiles; ++i)
			{
				getResponse(NET_ESMF_FILES, bufSize, buf);
				strcpy(fileType, buf);
printf("File Type: %s\n", fileType);
				getResponse(NET_ESMF_FILES, bufSize, buf);
				strcpy(fileName, buf);
printf("File Name: %s\n", fileName);

				dataFiles.push_back(fileName);
			}
		}

		getResponse(NET_ESMF_FILES, bufSize, buf);
		status = ntohl(*((unsigned int*)buf));
printf("Status: %d\n", status);
	}

	disconnect();

	return dataFiles;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
string  NetEsmfClient::getData(int     clientId,
                               string  varName,
                               string  time,
                               string  lat,
                               string   lon,
                               string&  dataValue)
{
	int		status = 0;
	int		bufSize = 0;
   char  	buf[1024];

	if (connect() < 0)
	{
		return "";
	}

//printf("NetEsmfClient::getData()\n");
	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = sendRequest(NET_ESMF_DATA, 4, &netClientId);

	if (bytesSent == 4)
	{
		const char*	varNameChar = varName.c_str();
		int	bytesWritten = 
					theSocket.write(strlen(varNameChar) + 1, (void*)varNameChar);

		const char*	timeChar = time.c_str();
		bytesWritten = theSocket.write(strlen(timeChar) + 1, (void*)timeChar);

		const char*	latChar = lat.c_str();
		bytesWritten = theSocket.write(strlen(latChar) + 1, (void*)latChar);

		const char*	lonChar = lon.c_str();
		bytesWritten = theSocket.write(strlen(lonChar) + 1, (void*)lonChar);

		getResponse(NET_ESMF_DATA, bufSize, buf);
		dataValue = buf;
	}

	disconnect();

	return dataValue;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  NetEsmfClient::end(int  clientId)
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(clientId);
	int	bytesSent = sendRequest(NET_ESMF_END, 4, &netClientId);

	if (bytesSent == 4)
	{
		getResponse(NET_ESMF_END, bufSize, buf);

		if (bufSize == 4)
		{
			status = ntohl(*((unsigned int*)buf));
printf("Status: %d\n", status);
		}
	}

	disconnect();

	return status;
}
