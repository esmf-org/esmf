#include "ESMCI_WebServClientSocket.h"

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
ClientSocket::ClientSocket()
{
}


/*
*****************************************************************************
**
*****************************************************************************
*/
ClientSocket::~ClientSocket()
{
	disconnect();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  ClientSocket::connect(const char*  host,
                           int          port)
{
//printf("ClientSocket::connect()\n");
	return clientConnect(host, port);
}
