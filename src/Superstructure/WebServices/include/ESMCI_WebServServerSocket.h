// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_WebServServerSocket_H
#define ESMCI_WebServServerSocket_H

#include "ESMCI_WebServLowLevelSocket.h"

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServServerSocket
//
// !DESCRIPTION:
//
// The code in this file defines the C++ ServerSocket members and method
// signatures (prototypes).  The companion file ESMCI\_WebServServerSocket.C
// contains the full code (bodies) for the ServerSocket methods.
//
// This class provides the basic functionality for creating a socket services.
// It is derived from the ESMCI_WebServLowLevelSocket class and provides a
// higher-level interface to that class specifically for socket services.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServServerSocket : public ESMCI_WebServLowLevelSocket
  {
  public:

     // constructor and destructor
          ESMCI_WebServServerSocket();
          ~ESMCI_WebServServerSocket();

     // setting up a server
          int  connect(int  port);
  };

} // end namespace

#endif          // ESMCI_WebServServerSocket_H
