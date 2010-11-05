// $Id: ESMCI_WebServSocketUtils.C,v 1.3 2010/11/05 18:46:57 ksaint Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
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
#include <unistd.h>

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_WebServSocketUtils.C,v 1.3 2010/11/05 18:46:57 ksaint Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI
{

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
  const char  msg[],					// message to print to stderr
  Severity    severity = PRINT,	// level of severity
  const char  proc[] = NULL		// method/function/procedure name
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
	case PRINT:
		fprintf(stderr, "MSG");
		break;

	case WARN:
		fprintf(stderr, "WARNING");
		break;

	case ERROR:
		fprintf(stderr, "ERROR");
		break;

	case FATAL:
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
	if (severity == FATAL)
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
  int    fd,		// (in) the file descriptor for the socket
  int    size,		// (in) the amount of data to send across the socket
  void*  data		// (in) the data to send
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

	unsigned char*		ptr = (unsigned char*)data;
	//printf("Sending: %s\n", ptr);

	int	totalBytesWritten = 0;
	int	t = 0;
	int	localrc = 0;

	//***
	// Loop through the data and send the the number of specified bytes
	// (sockets don't always send all of the data you request, so we have
	// to loop to make sure everything gets sent).
	//***
	while ((totalBytesWritten < size)  &&  (t < TWAIT))
	{
		int	bytesWritten = write(fd, 
                                 ptr + totalBytesWritten, 
                                 size - totalBytesWritten);
		//printf("::send - Bytes Written: %d\n", bytesWritten);

		if (bytesWritten > 0)
		{
			totalBytesWritten += bytesWritten;
		}
		else if (bytesWritten < 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_WRITE,
         	"Error while writing to socket.",
         	&localrc);
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
  int    fd,		// (in) the file descriptor for the socket
  int    size,		// (in) the amount of data to read from the socket
  void*  data		// (inout) the data buffer where the read data gets put.
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
	unsigned char*	ptr = (unsigned char*)data;

	int	totalBytesRead = 0;
	int	t = 0;
	int	localrc = 0;

	//***
	// Continually read from the socket until the specified amount of data
	// has been read (or the timeout value has been reached)
	//***
	while ((totalBytesRead < size)  &&  (t < TWAIT))
	{
		int	bytesRead = read(fd, ptr + totalBytesRead, size - totalBytesRead);

		if (bytesRead > 0)
		{
			totalBytesRead += bytesRead;
		}
      else if (bytesRead < 0)
		{
      	ESMC_LogDefault.ESMC_LogMsgFoundError(
         	ESMC_RC_FILE_READ,
         	"Error while reading from socket.",
         	&localrc);
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
  int    fd,		// (in) the file descriptor for the socket
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
	char*		cp = const_cast<char*>(s);
	int		totalBytesRead = 0;
	int		bytesRead = 0;

	//***
	// Keep reading from the socket until there's no more data to read, as
	// indicated by the number of bytes read equalling zero.
	//***
	do
	{
		bytesRead = read(fd, cp, 1);
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
