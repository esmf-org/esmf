// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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

#include <string.h>

#ifdef ESMF_OS_MinGW

#include <Windows.h>
#define sleep(secs) Sleep(secs*1000)
#include <Winsock.h>
typedef char* value_ptr_t;

#else

#include <fcntl.h>
#include <unistd.h>
// <sys/types.h> is not strictly required for POSIX 2001, but some
// older systems - like Darwin - may need it.
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
typedef void* value_ptr_t;

#endif

#include "ESMCI_WebServSocketUtils.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
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
//    {\tt ESMF\_SUCCESS} or error code on failure.
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

        int             localrc = ESMC_RC_NOT_IMPL;

        /*
        ** Turn on the non-blocking attribute for the socket
        */
        theNonBlock = true;

        if (theTSock <= 0)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_OBJ_WRONG,
                        "The Server listening socket not valid.",
                        ESMC_CONTEXT, &localrc);

                return localrc;
        }

        int     sock;
        if (theSock > 0)
        {
                sock = theSock;
        }
        else
        {
                sock = theTSock;
        }

#if !defined (ESMF_OS_MinGW)
        if (fcntl(sock, F_SETFL, fcntl(sock, F_GETFL) | O_NONBLOCK) < 0)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_CANNOT_SET,
                        "Unable to set nonblock attribute.",
                        ESMC_CONTEXT, &localrc);

                return localrc;
        }
#endif

        return ESMF_SUCCESS;
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
//   int  socket file descriptor if successful, ESMF_FAILURE otherwise.
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
        int     localrc = 0;

        //printf("LowLevelSocket::serverConnect()\n");
        disconnect();

        theTSock = socket(AF_INET, SOCK_STREAM, 0);
        if (theTSock < 0)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_OPEN,
                        "Unable to open socket connection.",
                        ESMC_CONTEXT, &localrc);

                return ESMF_FAILURE;
        }

        //***
        // Set the SO_REUSEADDR to true so that the server can be restarted
        // quickly without rejecting the bind
        //***
#ifdef SO_REUSEADDR
        // Turns out that the platform that doesn't have this macro defined
        // (frost Bluegene/L) doesn't really support sockets at all!
        int     optVal = 1;
        setsockopt(theTSock, SOL_SOCKET, SO_REUSEADDR, (value_ptr_t)&optVal,
          sizeof(optVal));
#endif

        struct sockaddr_in      server;
        server.sin_family = AF_INET;
        server.sin_addr.s_addr = INADDR_ANY;
        server.sin_port = htons(port);

        int     status = 0;
        int     t = 0;

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
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_OPEN,
                        "Socket bind failed.",
                        ESMC_CONTEXT, &localrc);

                return ESMF_FAILURE;
        }

        if (theNonBlock)
        {
                nonblock();
        }

        if (listen(theTSock, 5) < 0)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_OPEN,
                        "Socket listen failed.",
                        ESMC_CONTEXT, &localrc);

                return ESMF_FAILURE;
        }

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
//   int  return code - the socket id if success; ESMF_FAILURE for failure
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
        int     localrc = 0;

        //***
        // Make sure the server socket has been created
        //***
        if (theTSock <= 0)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_OBJ_WRONG,
                        "The Server listening socket not valid.",
                        ESMC_CONTEXT, &localrc);

                return ESMF_FAILURE;
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
                disconnect();

                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_OPEN,
                        "Socket accept failed.",
                        ESMC_CONTEXT, &localrc);

                return ESMF_FAILURE;
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
//   int  socket file descriptor if successful, ESMF_FAILURE otherwise.
//
// !ARGUMENTS:
//
  const char*   host,   // (in) name of the host to which we are connecting
  int                           port  // (in) port number on the host to which we are connecting
  )
//
// !DESCRIPTION:
//    Sets up a socket service on which we listen for requests from clients.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("LowLevelSocket::clientConnect()\n");
        int     localrc = 0;

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
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_OPEN,
                        "Unable to create client socket.",
                        ESMC_CONTEXT, &localrc);

                return ESMF_FAILURE;
        }

        struct sockaddr_in      server;
        server.sin_family = AF_INET;

        struct hostent*         hp = gethostbyname(host);
        if (hp == NULL)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_OBJ_BAD,
                        "Call to gethostbyname failed.",
                        ESMC_CONTEXT, &localrc);

                return ESMF_FAILURE;
        }

        memcpy(&server.sin_addr, hp->h_addr, hp->h_length);
        server.sin_port = htons(port);

        //***
        // Connect the client socket to the server socket
        //***
        if (connect(theSock, (struct sockaddr*)&server, sizeof(server)) < 0)
        {
                disconnect();

                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_OPEN,
                        "Client socket connect failed.",
                        ESMC_CONTEXT, &localrc);

                return ESMF_FAILURE;
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
#if !defined (ESMF_OS_MinGW)
                ::close(theSock);
#else
                ::closesocket(theSock);
#endif
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
#if !defined (ESMF_OS_MinGW)
                ::close(theTSock);
#else
                ::closesocket(theTSock);
#endif
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
//   int  number of characters (bytes) sent
//
// !ARGUMENTS:
//
  int           size,  // (in) the size of the data to send
  void*  data   // (in) the data to send
  )
//
// !DESCRIPTION:
//    Transmits the specified data across the socket.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("LowLevelSocket::send()\n");

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
//        has performed an orderly shutdown, and ESMF_FAILURE otherwise.
//
// !ARGUMENTS:
//
  int           size,  // (in) the max size of the data to receive
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
//        if successful, ESMF_FAILURE otherwise.
//
// !ARGUMENTS:
//
  int&          size,  // (out) the size of the data according to the packet header
  void*  data   // (out) the buffer to contain the data (enough memory must
                // be allocated ahead of time)
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

        int     localrc = 0;

        size = 0;

        //***
        // Read the packet header from the socket
        //***
        //printf("Reading size: %d\n", thePhSize);
        if (recv(thePhSize, &thePHead) != thePhSize)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_READ,
                        "Socket receive failed.",
                        ESMC_CONTEXT, &localrc);

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

                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_READ,
                        "Socket receive failed: invalid packet header.",
                        ESMC_CONTEXT, &localrc);

                return 0;
        }

        //***
        // Get the size of the data to be read from the packet header and read the
        // the data from the socket
        //***
        size = thePHead.size;

        int     bytesRead = recv(size, data);
        //printf("Bytes Read: %d\n", bytesRead);

        if (bytesRead != size)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_READ,
                        "Socket receive failed: number of bytes read not expected size.",
                        ESMC_CONTEXT, &localrc);
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
//        header) if successful, ESMF_FAILURE otherwise.
//
// !ARGUMENTS:
//
  int           size,  // (in) the size of the data to send
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

        int     localrc = 0;

        //***
        // Make sure we handle endianness
        //***
        thePHead.magic = htonl(MAGIC);
        thePHead.size = htonl(size);

        //***
        // Send the packet header
        //***
        int     bytesSent = 0;
        if ((bytesSent = send(thePhSize, &thePHead)) != thePhSize)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Socket send failed.",
                        ESMC_CONTEXT, &localrc);

                return 0;
        }

        //***
        // Send the data
        //***
        int     bytesWritten = send(size, data);
        if (bytesWritten != size)
        {
                ESMC_LogDefault.MsgFoundError(
                        ESMC_RC_FILE_WRITE,
                        "Socket send failed: number of bytes sent not expected size.",
                        ESMC_CONTEXT, &localrc);
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
//        header) if successful, ESMF_FAILURE otherwise.
//
// !ARGUMENTS:
//
  const char    msg[]   // (in) the string to send
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
