#include "ESMCI_WebServLowLevelSocket.h"

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
LowLevelSocket::LowLevelSocket()
{
	theTSock = -1;
	theSock = -1;
	theNonBlock = false;
	thePhSize = sizeof(pHeader);
	thePHead.magic = MAGIC;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
LowLevelSocket::~LowLevelSocket()
{
	disconnect();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  LowLevelSocket::nonblock()
{
//printf("LowLevelSocket::nonblock()\n");
	/*
	** Turn on the non-blocking attribute for the socket
	*/
	theNonBlock = true;

	if (theTSock <= 0)
	{
		return 0;
	}

	int	sock;
	if (theSock > 0)
	{
		sock = theSock;
	}
	else
	{
		sock = theTSock;
	}

	if (fcntl(sock, F_SETFL, fcntl(sock, F_GETFL) | O_NONBLOCK) < 0)
	{
		notify("Unable to set nonblock attribute", ERROR, 
             "LowLevelSocket:nonblock");
		perror("");
		return -1;
	}

	return 0;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  LowLevelSocket::serverConnect(int  port)
{
//printf("LowLevelSocket::serverConnect()\n");
	disconnect();

	theTSock = socket(AF_INET, SOCK_STREAM, 0);
	if (theTSock < 0)
	{
		notify("socket failed", ERROR, "LowLevelSocket::serverConnect");
		perror("");
		return -1;
	}

	struct sockaddr_in	server;
	server.sin_family = AF_INET;
	server.sin_addr.s_addr = INADDR_ANY;
	server.sin_port = htons(port);

	int	status = 0;
	int	t = 0;

	do
	{
		status = bind(theTSock, (struct sockaddr*)&server, sizeof(server));
		++t;

		if ((status < 0)  &&  (t < TWAIT))
		{
			sleep(1);
		}
	} while ((status < 0)  &&  (t < TWAIT));

	if (status < 0)
	{
		notify("bind failed", ERROR, "LowLevelSocket::serverConnect");
		perror("");
		return -1;
	}

	if (theNonBlock)
	{
		nonblock();
	}

	listen(theTSock, 5);

	return theTSock;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  LowLevelSocket::accept()
{
//printf("LowLevelSocket::accept()\n");
	if (theTSock <= 0)
	{
		return -1;
	}

	close();

	if ((theSock = ::accept(theTSock, NULL, NULL)) < 0)
	{
		if (theSock != EAGAIN)
		{
			notify("accept failed", ERROR, "LowLevelSocket::serverConnect");
			perror("");
			disconnect();
		}
		return -1;
	}

	return theSock;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  LowLevelSocket::clientConnect(const char*  host,
                                   int          port)
{
//printf("LowLevelSocket::clientConnect()\n");
	disconnect();

	theSock = socket(AF_INET, SOCK_STREAM, 0);
	if (theSock < 0)
	{
		notify("socket failed", ERROR, "LowLevelSocket::clientConnect");
		perror("");
		return -1;
	}

	struct sockaddr_in	server;
	server.sin_family = AF_INET;

	struct hostent*	hp = gethostbyname(host);
	if (hp == NULL)
	{
		char	serr[1024];
		sprintf(serr, "gethostbyname(%s) failed", host);
		notify(serr, ERROR, "LowLevelSocket::clientConnect");
		perror("");
		return -1;
	}

	memcpy(&server.sin_addr, hp->h_addr, hp->h_length);
	server.sin_port = htons(port);

	if (connect(theSock, (struct sockaddr*)&server, sizeof(server)) < 0)
	{
		notify("connect failed", ERROR, "LowLevelSocket::clientConnect");
		perror("");
		disconnect();
		return -1;
	}

	return theSock;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  LowLevelSocket::close()
{
//printf("LowLevelSocket::close()\n");
	if (theSock > 0)
	{
		::close(theSock);
		theSock = -1;
	}
}


/*
*****************************************************************************
**
*****************************************************************************
*/
void  LowLevelSocket::disconnect()
{
//printf("LowLevelSocket::disconnect()\n");
	close();

	if (theTSock > 0)
	{
		::close(theTSock);
		theTSock = -1;
	}
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  LowLevelSocket::send(int    size,
                          void*  data)
{
//printf("LowLevelSocket::send1()\n");
	return ::send(theSock, size, data);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  LowLevelSocket::recv(int    size,
                          void*  data)
{
//printf("LowLevelSocket::recv()\n");
	return ::recv(theSock, size, data);
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  LowLevelSocket::read(int&   size,
                          void*  data)
{
//printf("LowLevelSocket::read()\n");
	size = 0;

//printf("Reading size: %d\n", thePhSize);
	if (recv(thePhSize, &thePHead) != thePhSize)
	{
		notify("recv failed", WARN, "LowLevelSocket::read");
		return 0;
	}

	thePHead.magic = ntohl(thePHead.magic);
	thePHead.size = ntohl(thePHead.size);

//printf("Magic: %d\n", thePHead.magic);
//printf("Size: %d\n", thePHead.size);

	if (thePHead.magic != MAGIC)
	{
		thePHead.magic = MAGIC;
		notify("recv failed - invalid packet header", WARN, 
             "LowLevelSocket::read");
		return 0;
	}

	size = thePHead.size;

	int	bytesRead = recv(size, data);
//printf("Bytes Read: %d\n", bytesRead);
	if (bytesRead != size)
	{
		notify("recv failed", WARN, "LowLevelSocket::read");
	}
//printf("Data: %s\n", data);

	return bytesRead;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  LowLevelSocket::write(int    size,
                           void*  data)
{
//printf("LowLevelSocket::write()\n");
	thePHead.magic = htonl(MAGIC);
	thePHead.size = htonl(size);

	int	bytesSent = 0;
	if ((bytesSent = send(thePhSize, &thePHead)) != thePhSize)
	{
		notify("send failed", WARN, "LowLevelSocket::write");
		return 0;
	}

	int	bytesWritten = send(size, data);
	if (bytesWritten != size)
	{
		notify("send failed", WARN, "LowLevelSocket::write");
	}

	return bytesWritten;
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  LowLevelSocket::send(const char  msg[])
{
//printf("LowLevelSocket::send2()\n");
	return write(strlen(msg) + 1, (void*)msg);
}
