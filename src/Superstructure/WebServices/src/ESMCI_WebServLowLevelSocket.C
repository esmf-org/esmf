// $Id: ESMCI_WebServLowLevelSocket.C,v 1.2 2010/11/02 18:36:04 ksaint Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServLowLevelSocket.C"
//==============================================================================
//
// ESMC WebServLowLevelSocket method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ LowLevelSocket methods declared
// in the companion file ESMCI_WebServLowLevelSocket.h.  This code
// provides some basic, low-level socket functionality to setup and
// create sockets, and to send and receive data across the sockets.
//
//-----------------------------------------------------------------------------


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

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_WebServLowLevelSocket.C,v 1.2 2010/11/02 18:36:04 ksaint Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI
{

//-----------------------------------------------------------------------------
//
// constructor and destructor
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::ESMCI_WebServLowLevelSocket()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::ESMCI_WebServLowLevelSocket()
// 
// !INTERFACE:
ESMCI_WebServLowLevelSocket::ESMCI_WebServLowLevelSocket(
// 
// 
// !ARGUMENTS:
// 
  )
// 
// !DESCRIPTION:
//    Setup the initial default values for the socket interface.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	theTSock = -1;
	theSock = -1;
	theNonBlock = false;
	thePhSize = sizeof(pHeader);
	thePHead.magic = MAGIC;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::~ESMCI_WebServLowLevelSocket()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::~ESMCI_WebServLowLevelSocket()
// 
// !INTERFACE:
ESMCI_WebServLowLevelSocket::~ESMCI_WebServLowLevelSocket(
// 
// 
// !ARGUMENTS:
// 
  )
// 
// !DESCRIPTION:
//    Cleanup the socket interface.  For now, all this involves is making
//    sure the socket is disconnected.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	disconnect();
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::nonblock()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::nonblock()
// 
// !INTERFACE:
int  ESMCI_WebServLowLevelSocket::nonblock(
// 
// !RETURN VALUE:
//   int  return code - 0 for success, -1 for failure
// 
// !ARGUMENTS:
// 
  )
// 
// !DESCRIPTION:
//    Set the socket up to be non blocking.
//    (KDS: Not sure how portable this command is, since it uses the fcntl
//          system call.  However, I don't believe it's being used at all 
//          with the ESMF code... may want to remove later.)
// 
//EOPI
//-----------------------------------------------------------------------------
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
		//notify("Unable to set nonblock attribute", ERROR, 
      //       "LowLevelSocket:nonblock");
		//perror("");
		return -1;
	}

	return 0;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::serverConnect()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::serverConnect()
// 
// !INTERFACE:
int  ESMCI_WebServLowLevelSocket::serverConnect(
// 
// !RETURN VALUE:
//   int  socket file descriptor if successful, -1 otherwise.
// 
// !ARGUMENTS:
// 
  int  port          // (in) port number on which socket service is setup
  )
// 
// !DESCRIPTION:
//    Sets up a socket service on which we listen for requests from clients.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::serverConnect()\n");
	disconnect();

	theTSock = socket(AF_INET, SOCK_STREAM, 0);
	if (theTSock < 0)
	{
		//notify("socket failed", ERROR, "LowLevelSocket::serverConnect");
		//perror("");
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
		//notify("bind failed", ERROR, "LowLevelSocket::serverConnect");
		//perror("");
		return -1;
	}

	if (theNonBlock)
	{
		nonblock();
	}

	listen(theTSock, 5);

	return theTSock;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::accept()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::accept()
// 
// !INTERFACE:
int  ESMCI_WebServLowLevelSocket::accept(
// 
// !RETURN VALUE:
//   int  return code - 0 for success, -1 for failure
// 
// !ARGUMENTS:
// 
  )
// 
// !DESCRIPTION:
//    Sets up a socket service on which we listen for requests from clients.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::accept()\n");

	//***
	// Make sure the server socket has been created
	//***
	if (theTSock <= 0)
	{
		return -1;
	}

	//***
	// If a communication socket exists, make sure it's closed first
	//***
	close();

	//***
	// Wait for a request on the server socket and create a communication 
	// socket when a request comes in
	//***
	if ((theSock = ::accept(theTSock, NULL, NULL)) < 0)
	{
		if (theSock != EAGAIN)
		{
			//notify("accept failed", ERROR, "LowLevelSocket::serverConnect");
			//perror("");
			disconnect();
		}
		return -1;
	}

	return theSock;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::clientConnect()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::clientConnect()
// 
// !INTERFACE:
int  ESMCI_WebServLowLevelSocket::clientConnect(
// 
// !RETURN VALUE:
//   int  socket file descriptor if successful, -1 otherwise.
// 
// !ARGUMENTS:
// 
  const char*	host,	// (in) name of the host to which we are connecting
  int  			port  // (in) port number on the host to which we are connecting
  )
// 
// !DESCRIPTION:
//    Sets up a socket service on which we listen for requests from clients.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::clientConnect()\n");

	//***
	// First, make sure that we're not already connected... if so, disconnect
	//***
	disconnect();

	//***
	// Create the client socket
	//***
	theSock = socket(AF_INET, SOCK_STREAM, 0);
	if (theSock < 0)
	{
		//notify("socket failed", ERROR, "LowLevelSocket::clientConnect");
		//perror("");
		return -1;
	}

	struct sockaddr_in	server;
	server.sin_family = AF_INET;

	struct hostent*	hp = gethostbyname(host);
	if (hp == NULL)
	{
		//char	serr[1024];
		//sprintf(serr, "gethostbyname(%s) failed", host);
		//notify(serr, ERROR, "LowLevelSocket::clientConnect");
		//perror("");
		return -1;
	}

	memcpy(&server.sin_addr, hp->h_addr, hp->h_length);
	server.sin_port = htons(port);

	//***
	// Connect the client socket to the server socket
	//***
	if (connect(theSock, (struct sockaddr*)&server, sizeof(server)) < 0)
	{
		//notify("connect failed", ERROR, "LowLevelSocket::clientConnect");
		//perror("");
		disconnect();
		return -1;
	}

	return theSock;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::close()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::close()
// 
// !INTERFACE:
void  ESMCI_WebServLowLevelSocket::close(
// 
// !RETURN VALUE:
// 
// !ARGUMENTS:
// 
  )
// 
// !DESCRIPTION:
//    Close the communication socket (if it's open).
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::close()\n");

	if (theSock > 0)
	{
		::close(theSock);
		theSock = -1;
	}
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::disconnect()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::disconnect()
// 
// !INTERFACE:
void  ESMCI_WebServLowLevelSocket::disconnect(
// 
// !RETURN VALUE:
// 
// !ARGUMENTS:
// 
  )
// 
// !DESCRIPTION:
//    Close the communication socket and disconnect the server socket.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::disconnect()\n");

	close();

	if (theTSock > 0)
	{
		::close(theTSock);
		theTSock = -1;
	}
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::send()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::send()
// 
// !INTERFACE:
int  ESMCI_WebServLowLevelSocket::send(
// 
// !RETURN VALUE:
//   int  number of characters (bytes) sent if successful, -1 otherwise.
// 
// !ARGUMENTS:
// 
  int  	size,  // (in) the size of the data to send
  void*  data   // (in) the data to send
  )
// 
// !DESCRIPTION:
//    Transmits the specified data across the socket.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::send1()\n");

	return ESMCI::ESMCI_WebServSend(theSock, size, data);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::recv()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::recv()
// 
// !INTERFACE:
int  ESMCI_WebServLowLevelSocket::recv(
// 
// !RETURN VALUE:
//   int  number of characters (bytes) received if successful, 0 if the peer
//        has performed an orderly shutdown, and -1 otherwise.
// 
// !ARGUMENTS:
// 
  int  	size,  // (in) the size of the data to send
  void*  data   // (inout) the buffer where the data is put; must have memory
                // allocated at least the specified size
  )
// 
// !DESCRIPTION:
//    Retrieves the specified amount of data from the communication socket.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::recv()\n");

	return ESMCI::ESMCI_WebServRecv(theSock, size, data);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::read()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::read()
// 
// !INTERFACE:
int  ESMCI_WebServLowLevelSocket::read(
// 
// !RETURN VALUE:
//   int  number of characters (bytes) read (not including the packet header) 
//        if successful, -1 otherwise.
// 
// !ARGUMENTS:
// 
  int&  	size,  // (out) the size of the data according to the packet header
  void*  data   // (out) the buffer to contain the data (enough memory must 
                // be allocated ahed of time)
  )
// 
// !DESCRIPTION:
//    Retrieves data from the communication socket.  First, the packet header
//    is read, which specifies the amount of data to be transmitted, and then
//    the actual data is read.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::read()\n");

	size = 0;

	//***
	// Read the packet header from the socket
	//***
	//printf("Reading size: %d\n", thePhSize);
	if (recv(thePhSize, &thePHead) != thePhSize)
	{
		//notify("recv failed", WARN, "LowLevelSocket::read");
		return 0;
	}

	//***
	// Make sure we deal with endiannes
	//***
	thePHead.magic = ntohl(thePHead.magic);
	thePHead.size = ntohl(thePHead.size);

	//printf("Magic: %d\n", thePHead.magic);
	//printf("Size: %d\n", thePHead.size);

	//***
	// Make sure the magic number matches up so that we can guarantee we're
	// using the correct protocols
	//***
	if (thePHead.magic != MAGIC)
	{
		thePHead.magic = MAGIC;
		//notify("recv failed - invalid packet header", WARN, 
      //       "LowLevelSocket::read");
		return 0;
	}

	//***
	// Get the size of the data to be read from the packet header and read the
	// the data from the socket
	//***
	size = thePHead.size;

	int	bytesRead = recv(size, data);
	//printf("Bytes Read: %d\n", bytesRead);

	if (bytesRead != size)
	{
		//notify("recv failed", WARN, "LowLevelSocket::read");
	}
	//printf("Data: %s\n", data);

	return bytesRead;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::write()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::write()
// 
// !INTERFACE:
int  ESMCI_WebServLowLevelSocket::write(
// 
// !RETURN VALUE:
//   int  number of characters (bytes) written (not including the packet
//        header) if successful, -1 otherwise.
// 
// !ARGUMENTS:
// 
  int  	size,  // (in) the size of the data to send
  void*  data   // (in) the data to send
  )
// 
// !DESCRIPTION:
//    Transmits the specified data across the socket.  First, it creates and
//    sends a packet header that includes the magic number and the size of the
//    data, and then it sends the actual data.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::write()\n");

	//***
	// Make sure we handle endianness
	//***
	thePHead.magic = htonl(MAGIC);
	thePHead.size = htonl(size);

	//***
	// Send the packet header
	//***
	int	bytesSent = 0;
	if ((bytesSent = send(thePhSize, &thePHead)) != thePhSize)
	{
		//notify("send failed", WARN, "LowLevelSocket::write");
		return 0;
	}

	//***
	// Send the data
	//***
	int	bytesWritten = send(size, data);
	if (bytesWritten != size)
	{
		//notify("send failed", WARN, "LowLevelSocket::write");
	}

	return bytesWritten;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServLowLevelSocket::send()"
//BOPI
// !ROUTINE:  ESMCI_WebServLowLevelSocket::send()
// 
// !INTERFACE:
int  ESMCI_WebServLowLevelSocket::send(
// 
// !RETURN VALUE:
//   int  number of characters (bytes) written (not including the packet
//        header) if successful, -1 otherwise.
// 
// !ARGUMENTS:
// 
  const char	msg[]	// (in) the string to send
  )
// 
// !DESCRIPTION:
//    Convenience method to send a string across the communication network.
//    This method determines the data size by getting the string length (plus
//    one) and passes it on, along with the string data, to the write method.
// 
//EOPI
//-----------------------------------------------------------------------------
{
	//printf("LowLevelSocket::send2()\n");

	return write(strlen(msg) + 1, (void*)msg);
}

} // end namespace
