#include "ESMCI_WebServServerSocket.h"

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
ServerSocket::ServerSocket()
{
}


/*
*****************************************************************************
**
*****************************************************************************
*/
ServerSocket::~ServerSocket()
{
	disconnect();
}


/*
*****************************************************************************
**
*****************************************************************************
*/
int  ServerSocket::connect(int  port)
{
printf("ServerSocket::connect()\n");
	return serverConnect(port);
}
