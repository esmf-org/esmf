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
#define ESMC_FILENAME "ESMCI_WebServSocketUtils.C"
//==============================================================================
//
// ESMC WebServSocketUtils function implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ SocketUtils functions declared
// in the companion file ESMCI_WebServSocketUtils.h.  This code
// provides the utilities to send and receive data using sockets, as well
// as the functionality to print messages to stderr (though this will
// probably be replaced with ESMF error handling code).
//
//-----------------------------------------------------------------------------
#include "ESMCI_WebServSocketUtils.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined (ESMF_OS_MinGW)
#include <unistd.h>
#else
#include <Windows.h>
#endif

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_WebServNetEsmf.h"

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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServGetRequestFromId()"
//BOPI
// !ROUTINE:  ESMCI_WebServGetRequestFromId()
//
// !INTERFACE:
char*  ESMCI_WebServGetRequestFromId(
//
// !RETURN VALUE:
//    char*  string value for the specified request id
//
// !ARGUMENTS:
//
  int  id      // request id for which the string value is to be returned
  )
//
// !DESCRIPTION:
//    Looks up a request string value based on a specified request id.
//
//EOPI
//-----------------------------------------------------------------------------
{
   //printf("ESMCI_WebServProcCtrlClient::getRequestFromId()\n");

   switch (id)
   {
   case NET_ESMF_EXIT:       return (char*)"EXIT";
   case NET_ESMF_NEW:        return (char*)"NEW";
   case NET_ESMF_INIT:       return (char*)"INIT";
   case NET_ESMF_RUN:        return (char*)"RUN";
   case NET_ESMF_TIMESTEP:   return (char*)"TIMESTEP";
   case NET_ESMF_FINAL:      return (char*)"FINAL";
   case NET_ESMF_STATE:      return (char*)"STATE";
   case NET_ESMF_DATA_DESC:  return (char*)"DATADESC";
   case NET_ESMF_DATA:       return (char*)"DATA";
   case NET_ESMF_FILES:      return (char*)"FILES";
   case NET_ESMF_END:        return (char*)"END";
   case NET_ESMF_PING:       return (char*)"PING";
   default:                  return (char*)"UNKN";
   }

   return (char*)"UNKN";
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServGetRequestId()"
//BOPI
// !ROUTINE:  ESMCI_WebServGetRequestId()
//
// !INTERFACE:
int  ESMCI_WebServGetRequestId(
//
// !RETURN VALUE:
//    int  id of the request based on the specified string; ESMF_FAILURE
//         if the id cannot be found
//
// !ARGUMENTS:
//
  const char  request[] // request string for which the id is to be returned
  )
//
// !DESCRIPTION:
//    Looks up a request id based on a specified string value.
//
//EOPI
//-----------------------------------------------------------------------------
{
   //printf("ESMCI_WebServGetRequestId()\n");

   if (strcmp(request, "EXIT")     == 0)  return NET_ESMF_EXIT;
   if (strcmp(request, "NEW")      == 0)  return NET_ESMF_NEW;
   if (strcmp(request, "INIT")     == 0)  return NET_ESMF_INIT;
   if (strcmp(request, "RUN")      == 0)  return NET_ESMF_RUN;
   if (strcmp(request, "TIMESTEP") == 0)  return NET_ESMF_TIMESTEP;
   if (strcmp(request, "FINAL")    == 0)  return NET_ESMF_FINAL;
   if (strcmp(request, "STATE")    == 0)  return NET_ESMF_STATE;
   if (strcmp(request, "DATADESC") == 0)  return NET_ESMF_DATA_DESC;
   if (strcmp(request, "DATA")     == 0)  return NET_ESMF_DATA;
   if (strcmp(request, "FILES")    == 0)  return NET_ESMF_FILES;
   if (strcmp(request, "END")      == 0)  return NET_ESMF_END;
   if (strcmp(request, "PING")     == 0)  return NET_ESMF_PING;

   return ESMF_FAILURE;
}

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServNotify()"
//BOPI
// !ROUTINE:  ESMCI_WebServNotify()
//
// !INTERFACE:
void  ESMCI_WebServNotify(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  const char  msg[],                                    // message to print to stderr
  WebServSeverity    severity = WebServPRINT,   // level of severity
  const char  proc[] = NULL             // method/function/procedure name
  )
//
// !DESCRIPTION:
//    Formats and prints the specified message to stderr.  If the severity
//    is set to "FATAL", then this function will also exit the application.
//
//EOPI
//-----------------------------------------------------------------------------
{
        //***
        // Print the severity level
        //***
        switch (severity)
        {
        case WebServPRINT:
                fprintf(stderr, "MSG");
                break;

        case WebServWARN:
                fprintf(stderr, "WARNING");
                break;

        case WebServERROR:
                fprintf(stderr, "ERROR");
                break;

        case WebServFATAL:
                fprintf(stderr, "FATAL");
                break;

        default:
                break;
        }

        //***
        // Print the procedure name (if any)
        //***
        if (proc != NULL)
        {
                fprintf(stderr, "[%s]", proc);
        }

        //***
        // Print the message
        //***
        fprintf(stderr, ": %s\n", msg);

        //***
        // Exit the application if this is a fatal message
        // (KDS: This should probably go away.)
        //***
        if (severity == WebServFATAL)
        {
                exit(0);
        }
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServSend()"
//BOPI
// !ROUTINE:  ESMCI_WebServSend()
//
// !INTERFACE:
int  ESMCI_WebServSend(
//
// !RETURN VALUE:
//   int  the total number of bytes written to the socket
//
// !ARGUMENTS:
//
  int    fd,            // (in) the file descriptor for the socket
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

        char*           ptr = (char*)data;
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
#if !defined (ESMF_OS_MinGW)
                int     bytesWritten = write(fd,
                                 ptr + totalBytesWritten,
                                 size - totalBytesWritten);
#else
                int     bytesWritten = send(fd,
                                 ptr + totalBytesWritten,
                                 size - totalBytesWritten,
                                 0);
#endif
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
                ESMC_CONTEXT, &localrc);
                }
        }

        return totalBytesWritten;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRecv()"
//BOPI
// !ROUTINE:  ESMCI_WebServRecv()
//
// !INTERFACE:
int  ESMCI_WebServRecv(
//
// !RETURN VALUE:
//   int  the total number of bytes read from the socket
//
// !ARGUMENTS:
//
  int    fd,            // (in) the file descriptor for the socket
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
        char*   ptr = (char*)data;

        int     totalBytesRead = 0;
        int     t = 0;
        int     localrc = 0;

        //***
        // Continually read from the socket until the specified amount of data
        // has been read (or the timeout value has been reached)
        //***
        while ((totalBytesRead < size)  &&  (t < TWAIT))
        {
#if !defined (ESMF_OS_MinGW)
                int     bytesRead = read(fd, ptr + totalBytesRead, size - totalBytesRead);
#else
                int     bytesRead = recv(fd, ptr + totalBytesRead, size - totalBytesRead, 0);
#endif

                if (bytesRead > 0)
                {
                        totalBytesRead += bytesRead;
                }
      else if (bytesRead < 0)
                {
        ESMC_LogDefault.MsgFoundError(
                ESMC_RC_FILE_READ,
                "Error while reading from socket.",
                ESMC_CONTEXT, &localrc);
                }
        }

        return totalBytesRead;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_WebServRecv()"
//BOPI
// !ROUTINE:  ESMCI_WebServRecv()
//
// !INTERFACE:
int  ESMCI_WebServRecv(
//
// !RETURN VALUE:
//   int  the total number of bytes read from the socket
//
// !ARGUMENTS:
//
  int    fd,            // (in) the file descriptor for the socket
  const char*  s  // (inout) the data buffer where the read data gets put.
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
#if !defined (ESMF_OS_MinGW)
                bytesRead = read(fd, cp, 1);
#else
                bytesRead = recv(fd, cp, 1, 0);
#endif
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

} // end namespace
