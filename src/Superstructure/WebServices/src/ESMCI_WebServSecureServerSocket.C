// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServSecureServerSocket.C"
//==============================================================================
//
// ESMC WebServSecureServerSocket method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ SecureServerSocket methods declared
// in the companion file ESMCI_WebServSecureServerSocket.h.  This code
// provides provides a higher-level interface for creating a socket 
// services to listen for, and respond to, client requests.
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServSecureServerSocket.h"

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
#define ESMC_METHOD "ESMCI_WebServSecureServerSocket::ESMCI_WebServSecureServerSocket()"
//BOPI
// !ROUTINE:  ESMCI_WebServSecureServerSocket::ESMCI_WebServSecureServerSocket()
//
// !INTERFACE:
ESMCI_WebServSecureServerSocket::ESMCI_WebServSecureServerSocket(
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
#define ESMC_METHOD "ESMCI_WebServSecureServerSocket::~ESMCI_WebServSecureServerSocket()"
//BOPI
// !ROUTINE:  ESMCI_WebServSecureServerSocket::~ESMCI_WebServSecureServerSocket()
//
// !INTERFACE:
ESMCI_WebServSecureServerSocket::~ESMCI_WebServSecureServerSocket(
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
#define ESMC_METHOD "ESMCI_WebServSecureServerSocket::connect()"
//BOPI
// !ROUTINE:  ESMCI_WebServSecureServerSocket::connect()
//
// !INTERFACE:
int  ESMCI_WebServSecureServerSocket::connect(
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
	//printf("ESMCI_WebServSecureServerSocket::connect()\n");

	return serverConnect(port);
}


} // end namespace
