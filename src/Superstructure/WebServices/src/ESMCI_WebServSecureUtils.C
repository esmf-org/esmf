// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_WebServSecureUtils.C"
//==============================================================================
//
// ESMC WebServSecureUtils function implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ SecureUtils functions declared
// in the companion file ESMCI_WebServSecureUtils.h.  This code
// provides the utilities to send and receive data using sockets, as well
// as the functionality to print messages to stderr (though this will
// probably be replaced with ESMF error handling code).
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServSecureUtils.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


namespace ESMCI
{

        static char*    pass;

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServSecureSend()"
//BOPI
// !ROUTINE:  ESMCI_WebServSecureSend()
//
// !INTERFACE:
int  ESMCI_WebServSecureSend(
//
// !RETURN VALUE:
//   int  the total number of bytes written to the socket
//
// !ARGUMENTS:
//
  SSL*   fd,            // (in) the file descriptor for the socket
  int    size,          // (in) the amount of data to send across the socket
  void*  data           // (in) the data to send
  )
//
// !DESCRIPTION:
//    Sends the specified amount of specified data across the specified
//    socket.
//
//EOPI
//-----------------------------------------------------------------------------
{
        if (size < 0)
        {
                return 0;
        }

        unsigned char*          ptr = (unsigned char*)data;
        //printf("Sending: %s\n", ptr);

        int     totalBytesWritten = 0;
        int     t = 0;
        int     localrc = 0;

        //***
        // Loop through the data and send the the number of specified bytes
        // (sockets don't always send all of the data you request, so we have
        // to loop to make sure everything gets sent).
        //***
        while ((totalBytesWritten < size)  &&  (t < TWAIT))
        {
                int     bytesWritten = SSL_write(fd,
                                     ptr + totalBytesWritten,
                                     size - totalBytesWritten);
                //printf("::send - Bytes Written: %d\n", bytesWritten);

                if (bytesWritten > 0)
                {
                        totalBytesWritten += bytesWritten;
                }
                else if (bytesWritten < 0)
                {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_WRITE,
                "Error while writing to socket.",
                &localrc);
                }
        }

        return totalBytesWritten;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServSecureRecv()"
//BOPI
// !ROUTINE:  ESMCI_WebServSecureRecv()
//
// !INTERFACE:
int  ESMCI_WebServSecureRecv(
//
// !RETURN VALUE:
//   int  the total number of bytes read from the socket
//
// !ARGUMENTS:
//
  SSL*   fd,            // (in) the file descriptor for the socket
  int    size,          // (in) the amount of data to read from the socket
  void*  data           // (inout) the data buffer where the read data gets put.
                        // The memory for this buffer should be allocated by the
                        // calling function with enough space to handle the
                        // specified size.
  )
//
// !DESCRIPTION:
//    Reads the specified amount of data from the specified socket and
//    puts the data into the specified data buffer.
//
//EOPI
//-----------------------------------------------------------------------------
{
        if (size < 0)
        {
                return 0;
        }

        //printf("recv Size: %d\n", size);
        unsigned char*  ptr = (unsigned char*)data;

        int     totalBytesRead = 0;
        int     t = 0;
        int     localrc = 0;

        //***
        // Continually read from the socket until the specified amount of data
        // has been read (or the timeout value has been reached)
        //***
        while ((totalBytesRead < size)  &&  (t < TWAIT))
        {
                int     bytesRead = SSL_read(fd,
                                 ptr + totalBytesRead,
                                 size - totalBytesRead);

                if (bytesRead > 0)
                {
                        totalBytesRead += bytesRead;
                }
      else if (bytesRead < 0)
                {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_READ,
                "Error while reading from socket.",
                &localrc);
                }
        }

        return totalBytesRead;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServSecureRecv()"
//BOPI
// !ROUTINE:  ESMCI_WebServSecureRecv()
//
// !INTERFACE:
int  ESMCI_WebServSecureRecv(
//
// !RETURN VALUE:
//   int  the total number of bytes read from the socket
//
// !ARGUMENTS:
//
  SSL*         fd,      // (in) the file descriptor for the socket
  const char*  s        // (inout) the data buffer where the read data gets put.
                                // The memory for this buffer should be allocated by the
                                // calling function with enough space to handle the data.
  )
//
// !DESCRIPTION:
//    Reads all of the available data from the socket and puts the data
//    into the specified data buffer.
//
//EOPI
//-----------------------------------------------------------------------------
{
        char*           cp = const_cast<char*>(s);
        int             totalBytesRead = 0;
        int             bytesRead = 0;

        //***
        // Keep reading from the socket until there's no more data to read, as
        // indicated by the number of bytes read equalling zero.
        //***
        do
        {
                bytesRead = SSL_read(fd, cp, 1);
                //printf("Bytes Read: %d\n", bytesRead);

                if (bytesRead > 0)
                {
                        totalBytesRead += bytesRead;
                        if (*cp == 0)
                        {
                                break;
                        }

                        cp += bytesRead;
                }
        } while (totalBytesRead > 0);

        return totalBytesRead;
}


SSL_CTX*  ESMCI_WebServInitContext(
   const char*  keyFile,
   const char*  password)
{
        SSL_METHOD*             method;
        SSL_CTX*                        context;

        // Global system initialization
        SSL_library_init();
        SSL_load_error_strings();

        // Setup a SIGPIPE handler
        // KDS: Not sure if I want to do this in ESMF... will have to see what it
        //      does first, so I'm keeping it in for now.
        // KDS: Actually, the example doesn't do anything in the signal handler,
        //      so I think I'm going to leave it out now.
        //signal(SIGPIPE, sigpipe_handle);

        // Create the context
        method = SSLv23_method();
        context = SSL_CTX_new(method);

        // Load the keys and certificates
        if (!(SSL_CTX_use_certificate_chain_file(context, keyFile)))
        {
                // Error and return
                fprintf(stderr, "Can't read certificate file\n");
                return NULL;
        }

        pass =(char*) password;
        SSL_CTX_set_default_passwd_cb(context, ESMCI_WebServPasswdCallback);
        if (!(SSL_CTX_use_PrivateKey_file(context, keyFile, SSL_FILETYPE_PEM)))
        {
                // Error and return
                fprintf(stderr, "Can't read key file\n");
                return NULL;
        }

        // Load the CAs we trust
        if (!(SSL_CTX_load_verify_locations(context, "root.pem", 0)))
        {
                // Error and return
                fprintf(stderr, "Can't read CA list\n");
                return NULL;
        }

        return context;
}


static int  ESMCI_WebServPasswdCallback(
        char*  buf,
   int    rwflag,
   int    num,
   void*  userData)
{
        if (num < (strlen(pass) + 1))
        {
                return 0;
        }

        strcpy(buf, pass);

        return(strlen(pass));
}


} // end namespace
