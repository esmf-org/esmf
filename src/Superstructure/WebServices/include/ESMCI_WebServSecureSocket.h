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


#ifndef ESMCI_WebServSecureSocket_H
#define ESMCI_WebServSecureSocket_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServSecureSocket
//
// !DESCRIPTION:
//
// The code in this file defines the C++ SecureSocket members and method
// signatures (prototypes).  The companion file ESMCI\_WebServSecureSocket.C
// contains the full code (bodies) for the SecureSocket methods.
//
// This class provides the fundamental socket functionality upon which the 
// rest of the network code (client and server) is built.
// (KDS: This class should never be implemented directly... I should probably
//       make it abstract.)
//
//EOPI
//-------------------------------------------------------------------------


#define MAGIC  (0x8765)

//-------------------------------------------------------------------------
#include <openssl/ssl.h>

namespace ESMCI
{

  //***
  // packet header
  //***
  struct pHeader
  {
	  int		magic;
	  int		size;
  };

  //=========================================================================
  class ESMCI_WebServSecureSocket
  {
  public:

     // constructor and destructor
	  ESMCI_WebServSecureSocket();
	  ~ESMCI_WebServSecureSocket();

     // setting up connections
	  int  serverConnect(int  port);
	  int  accept();

	  int  clientConnect(const char*  host,
                        int          port);

     // closing connections
	  void  close();
	  void  disconnect();

     // passing data
	  int  read(int&   size,
               void*  data);
	  int  write(int    size,
                void*  data);

	  int  send(const char  msg[]);

	  int  nonblock();

private:

	  int  send(int    size,
               void*  data);
	  int  recv(int    size,
               void*  data);


	  int			theTSock;		// the server listening socket file descriptor
	  int			theSock;			// the communication socket file descriptor
	  bool		theNonBlock;	// flag indicating whether or not socket is
	                    			// blocking
	  int			thePhSize;     // the packet header size
	  pHeader	thePHead;		// the packet header

     SSL_CTX*	theContext;		
     SSL*		theSecureSocket;
     BIO*		theSocketBuffer;
  };

} // end namespace

#endif 	// ESMCI_WebServSecureSocket_H
