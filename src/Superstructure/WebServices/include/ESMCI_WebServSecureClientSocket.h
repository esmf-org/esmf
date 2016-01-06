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
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_WebServSecureClientSocket_H
#define ESMCI_WebServSecureClientSocket_H

#include "ESMCI_WebServSecureSocket.h"

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServSecureClientSocket
//
// !DESCRIPTION:
//
// The code in this file defines the C++ SecureClientSocket members and method
// signatures (prototypes).  The companion file ESMCI\_WebServSecureClientSocket.C
// contains the full code (bodies) for the SecureClientSocket methods.
//
// This class provides the basic functionality for creating a socket client.
// It is derived from the ESMCI_WebServSecureSocket class and provides a
// higher-level interface to that class specifically for socket clients.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServSecureClientSocket : public ESMCI_WebServSecureSocket
  {
  public:

     // constructor and destructor
	  ESMCI_WebServSecureClientSocket();
	  ~ESMCI_WebServSecureClientSocket();

     // setting up a client
	  int  connect(const char*  host,
                  int          port);
  };

} // end namespace


#endif 	// ESMCI_WebServSecureClientSocket_H
