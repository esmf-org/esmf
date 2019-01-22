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
#define ESMC_FILENAME "ESMCI_WebServServerSocket.C"
//==============================================================================
//
// ESMC WebServServerSocket method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ServerSocket methods declared
// in the companion file ESMCI_WebServServerSocket.h.  This code
// provides provides a higher-level interface for creating a socket
// services to listen for, and respond to, client requests.
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServServerSocket.h"

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
#define ESMC_METHOD "ESMCI_WebServServerSocket::ESMCI_WebServServerSocket()"
//BOPI
// !ROUTINE:  ESMCI_WebServServerSocket::ESMCI_WebServServerSocket()
//
// !INTERFACE:
ESMCI_WebServServerSocket::ESMCI_WebServServerSocket(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Setup the initial default values for the server socket interface.
//
//EOPI
//-----------------------------------------------------------------------------
{
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServServerSocket::~ESMCI_WebServServerSocket()"
//BOPI
// !ROUTINE:  ESMCI_WebServServerSocket::~ESMCI_WebServServerSocket()
//
// !INTERFACE:
ESMCI_WebServServerSocket::~ESMCI_WebServServerSocket(
//
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Cleanup the socket service by making sure the socket is closed.
//
//EOPI
//-----------------------------------------------------------------------------
{
        disconnect();
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServServerSocket::connect()"
//BOPI
// !ROUTINE:  ESMCI_WebServServerSocket::connect()
//
// !INTERFACE:
int  ESMCI_WebServServerSocket::connect(
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
        //printf("ESMCI_WebServServerSocket::connect()\n");

        return serverConnect(port);
}


} // end namespace
