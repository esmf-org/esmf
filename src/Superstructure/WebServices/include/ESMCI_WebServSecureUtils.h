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


#ifndef ESMCI_WebServSecureUtils_H
#define ESMCI_WebServSecureUtils_H

#include "ESMCI_WebServSocketUtils.h"
#include <openssl/ssl.h>

//-------------------------------------------------------------------------
//BOPI
//
// !DESCRIPTION:
//
// The code in this file defines the C++ SecureUtils function
// signatures (prototypes).  The companion file ESMCI\_WebServSecureUtils.C
// contains the full code (bodies) for the SecureUtils functions.
//
// This file contains some utility functions that don't belong in a class,
// but are needed to create and use the server and client sockets.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  // Functions to send and receive data using sockets
  int  ESMCI_WebServSecureSend(SSL*   fd,
                               int    size,
                               void*  data);

  int  ESMCI_WebServSecureRecv(SSL*   fd,
                               int    size,
                               void*  data);

  int  ESMCI_WebServSecureRecv(SSL*         fd,
                               const char*  s);

  SSL_CTX*  ESMCI_WebServInitContext(const char*  keyFile,
                                     const char*  password);

  static int  ESMCI_WebServPasswdCallback(char*  buf,
                                          int    rwflag,
                                          int    num,
                                          void*  userData);

} // end namespace

#endif          // ESMCI_WebServSecureUtils_H
