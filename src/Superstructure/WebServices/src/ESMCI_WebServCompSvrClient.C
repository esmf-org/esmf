#include "ESMCI_WebServCompSvrClient.h"

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
CompSvrClient::CompSvrClient(const char*  host,
                             int          port,
                             int          clientId)
{
	theHost = NULL;

	if (host == NULL)
	{
		host = "localhost";
	}

	setHost(host);
	setPort(port);
	setClientId(clientId);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
CompSvrClient::~CompSvrClient()
{
	theSocket.disconnect();
	delete theHost;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  CompSvrClient::setHost(const char*  host)
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
void  CompSvrClient::setPort(int  port)
{
	thePort = port;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  CompSvrClient::setClientId(int  clientId)
{
	theClientId = clientId;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  CompSvrClient::sendRequest(int    request,
                                int    length,
                                void*  data)
{
//printf("CompSvrClient::sendRequest()\n");
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
int  CompSvrClient::getResponse(int    request,
                                int&   length,
                                void*  data)
{
//printf("CompSvrClient::getResponse()\n");
	length = 0;
	theSocket.read(length, data);

	return length;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  CompSvrClient::connect()
{
//printf("CompSvrClient::connect()\n");
	return theSocket.connect(theHost, thePort);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  CompSvrClient::disconnect()
{
//printf("CompSvrClient::disconnect()\n");
	return theSocket.disconnect();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  CompSvrClient::getRequestId(const char  request[])
{
//printf("CompSvrClient::getRequestId()\n");
	if (strcmp(request, "EXIT")  == 0)	return NET_ESMF_EXIT;
	if (strcmp(request, "NEW")   == 0)	return NET_ESMF_NEW;
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
char*  CompSvrClient::getRequestFromId(int  id)
{
//printf("CompSvrClient::getRequestFromId()\n");
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
int  CompSvrClient::init()
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

	unsigned int	netClientId = htonl(theClientId);
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
int  CompSvrClient::init(const char*  filename)
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(theClientId);
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
int  CompSvrClient::run()
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(theClientId);
	int	bytesSent = sendRequest(NET_ESMF_RUN, 4, &netClientId);

	if (bytesSent == 4)
	{
		getResponse(NET_ESMF_RUN, bufSize, buf);

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
int  CompSvrClient::final()
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(theClientId);
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
int  CompSvrClient::state()
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(theClientId);
	int	bytesSent = sendRequest(NET_ESMF_STATE, 4, &netClientId);

	if (bytesSent == 4)
	{
		getResponse(NET_ESMF_STATE, bufSize, buf);

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
vector<string>  CompSvrClient::files()
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	vector<string>		dataFiles;

	if (connect() < 0)
	{
		return dataFiles;
	}

	unsigned int	netClientId = htonl(theClientId);
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
int  CompSvrClient::end()
{
	int	status = 0;
	int	bufSize = 0;
   char  buf[1024];

	if (connect() < 0)
	{
		return status;
	}

	unsigned int	netClientId = htonl(theClientId);
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
