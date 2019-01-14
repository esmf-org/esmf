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
#define ESMC_FILENAME "ESMCI_WebServSecureClientSocket.C"
//==============================================================================
//
// ESMC WebServSecureClientSocket method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ SecureClientSocket methods declared
// in the companion file ESMCI_WebServSecureClientSocket.h.  This code
// provides provides a higher-level interface for creating a socket
// client to send requests to and receive responses from socket services.
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServSecureClientSocket.h"

#include "ESMCI_WebServSocketUtils.h"
#include "ESMCI_Macros.h"

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
#define ESMC_METHOD "ESMCI_WebServSecureClientSocket::ESMCI_WebServSecureClientSocket()"
//BOPI
// !ROUTINE:  ESMCI_WebServSecureClientSocket::ESMCI_WebServSecureClientSocket()
//
// !INTERFACE:
ESMCI_WebServSecureClientSocket::ESMCI_WebServSecureClientSocket(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Setup the initial default values for the client socket interface.
//
//EOPI
//-----------------------------------------------------------------------------
{
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServSecureClientSocket::~ESMCI_WebServSecureClientSocket()"
//BOPI
// !ROUTINE:  ESMCI_WebServSecureClientSocket::~ESMCI_WebServSecureClientSocket()
//
// !INTERFACE:
ESMCI_WebServSecureClientSocket::~ESMCI_WebServSecureClientSocket(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Cleanup the socket client by making sure the socket is closed.
//
//EOPI
//-----------------------------------------------------------------------------
{
        disconnect();
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServSecureClientSocket::connect()"
//BOPI
// !ROUTINE:  ESMCI_WebServSecureClientSocket::connect()
//
// !INTERFACE:
int  ESMCI_WebServSecureClientSocket::connect(
//
// !RETURN VALUE:
//   int  socket file descriptor if successful, ESMF_FAILURE otherwise.
//
// !ARGUMENTS:
//
  const char*  host, // (in) name of the machine which hosts the socket
                     // service and to which we're connecting.
  int  port          // (in) port number of the socket service to which
                     // we're connecting.
  )
//
// !DESCRIPTION:
//    Creates a client socket and connects to a socket service on the
//    specified host listening on the specified port.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //printf("SecureClientSocket::connect()\n");
        return clientConnect(host, port);
}


} // end namespace
