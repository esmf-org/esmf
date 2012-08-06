// $Id: ESMCI_IO.C,v 1.16 2012/08/06 01:26:54 gold2718 Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC IO method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO} methods declared
// in the companion file {\tt ESMCI\_IO.h}
//
//-------------------------------------------------------------------------
//
#define ESMC_FILENAME "ESMCI_IO.C"

// include associated header file
#include "ESMCI_IO.h"

// higher level, 3rd party or system includes here
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <vector>
#include <iostream>

// other ESMF include files here.
#include "ESMCI_Macros.h"
#include "ESMCI_Container.h"
#include <ESMCI_LogErr.h>
#include <ESMF_LogMacros.inc>
#include <ESMCI_ArrayBundle.h>

//#define __DEBUG
#ifdef __DEBUG
// Debug version
#define PRINTPOS  std::cout << ESMC_METHOD << " called at " << ESMC_FILENAME  \
                            << ":" << __LINE__ << std::endl
#define PRINTMSG(_msg)  std::cout << ESMC_METHOD << " at " << ESMC_FILENAME   \
                                  << ":" << __LINE__ << _msg << std::endl
#else
// Non-debug version
#define PRINTPOS
#define PRINTMSG(_msg)
#endif

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_IO.C,v 1.16 2012/08/06 01:26:54 gold2718 Exp $";
//-------------------------------------------------------------------------

namespace ESMCI
{
//
//-------------------------------------------------------------------------
//
// constructors and destruct()
//
//-------------------------------------------------------------------------
//

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::destruct()"
//BOPI
// !IROUTINE:  ESMCI::IO::destruct - release IO
//
// !INTERFACE:
void IO::destruct(void) {
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      {\tt ESMCI::IO::construct}, or for the object's use.
//
//EOPI
//-----------------------------------------------------------------------------
  // Delete the I/O Handler (if present)
  PRINTPOS;
  if (ioHandler != (IO_Handler *)NULL) {
    // Allow ioHandler to close and clean up as necessary.
    IO_Handler::destroy(&ioHandler);
  }
  // Delete all object nodes
  while(!objects.empty()) {
    IO_ObjectContainer *obj = objects.back();
    objects.pop_back();
    delete(obj);
  }
} // end IO:destruct
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::create()"
//BOPI
// !IROUTINE:  ESMCI::IO::create
//
// !INTERFACE:
IO *IO::create(
//
// !RETURN VALUE:
//    IO * to newly allocated IO
//
// !ARGUMENTS:
//
  int *rc                                  // (out) return code
  ) {
//
// !DESCRIPTION:
//    Create an empty {\tt IO} object.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  IO *ioclass;

  PRINTPOS;
  // call class constructor
  try{
    ioclass = new IO(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return ESMC_NULL_POINTER;
  } catch(...) {
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }

  // return successfully
  if (rc != NULL) {
    *rc = ESMF_SUCCESS;
  }
  return ioclass;
} // end IO::create
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::destroy()"
//BOPI
// !IROUTINE:  IO::destroy - free an IO created with Create
//
// !INTERFACE:
int IO::destroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  IO **ioclass) {  // (in) - IO to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys an IO object previously allocated
//      via an {\tt ESMCI\_IOCreate} routine.  Define for deep classes only.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  int localrc = ESMC_RC_NOT_IMPL;         // local return code

  PRINTPOS;
  // return with errors for NULL pointer
  if (ioclass == ESMC_NULL_POINTER || *ioclass == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                          "- Not a valid pointer to ioclass",
                                          &rc);
    return rc;
  }

  try {
    // destruct IO object
    (*ioclass)->destruct();
    (*ioclass)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
    localrc = ESMF_SUCCESS;
  } catch(int localrc) {
    // catch standard ESMF return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc);
    return rc;
  } catch(...) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
                                          "- Caught exception", &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
} // end IO::destroy
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::read()"
//BOP
// !IROUTINE:  IO::read - Read the items in an IO object from a file
//
// !INTERFACE:
int IO::read(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:

  char const * const file,        // (in)    - name of file being read
  ESMC_IOFmtFlag *iofmt,          // (in)    - IO format flag
  int   *timeslice                // (in)    - timeslice option
  ) {
// !DESCRIPTION:
//      Reads an {\tt ESMC\_IO} object from file
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  PRINTPOS;
  // Open the file
  IOReadFlag readflag = IO_READ;
  IOWriteFlag writeflag = IO_NO_WRITE;
  localrc = open(file, &readflag, &writeflag, iofmt);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
    return rc;
  }

  localrc = read(timeslice);

  // Close the file
  localrc = close();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
    return rc;
  }
    
  // return successfully
  rc = ESMF_SUCCESS;
  return (rc);
}  // end IO::read
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::read()"
//BOP
// !IROUTINE:  IO::read - Read the items in an IO object from an open file
//
// !INTERFACE:
int IO::read(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:

  int   *timeslice                // (in)    - timeslice option
  ) {
// !DESCRIPTION:
//      Read the items in an {\tt ESMC\_IO} object from an open file or stream
//      controlled by the object's IO_Handler member.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  PRINTPOS;
  // Read each item from the object list
  std::vector<IO_ObjectContainer *>::iterator it;
  for (it = objects.begin(); it < objects.end(); ++it) {
    switch((*it)->type) {
    case IO_NULL:
      localrc = ESMF_STATUS_UNALLOCATED;
      break;
    case IO_ARRAY:
      ioHandler->arrayRead((*it)->getArray(),
                           (*it)->getName(), timeslice, &localrc);
      break;
      // These aren't handled yet
    case IO_ATTRIBUTE:
    case IO_GRID:
    case IO_MESH:
    default:
      localrc = ESMF_STATUS_INVALID;
      break;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
      // Close the file but return original error even if close fails.
      localrc = close();
      return rc;
    }
  }
    
  // return successfully
  rc = ESMF_SUCCESS;
  return (rc);
}  // end IO::read
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::write()"
//BOP
// !IROUTINE:  IO::write - Performs a write on an IO object
//
// !INTERFACE:
int IO::write(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
  char const * const file,        // (in)    - name of file being read
  ESMC_IOFmtFlag *iofmt,          // (in)    - IO format flag
  bool append,                    // (in)    - IO append flag
  int   *timeslice                // (in)    - timeslice option
  ) {
// !DESCRIPTION:
//      Writes an {\tt ESMC\_IO} object to file
//
//EOP
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  IOWriteFlag iowriteflag;                // I/O write/append flag

  PRINTPOS;
  // Open the file
  IOReadFlag readflag = IO_NO_READ;
  if (append) {
    iowriteflag = IO_APPEND;
  } else {
    iowriteflag = IO_TRUNCATE;
  }
  localrc = open(file, &readflag, &iowriteflag, iofmt);
  std::cout << ESMC_METHOD << ": open returned " << localrc << std::endl;
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
    return rc;
  }

  localrc = write(timeslice);
  std::cout << ESMC_METHOD << ": write returned " << localrc << std::endl;

  // Close the file
  localrc = close();
  std::cout << ESMC_METHOD << ": close returned " << localrc << std::endl;
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
    return rc;
  }
    
  // return successfully
  rc = ESMF_SUCCESS;
  return (rc);
}  // end IO::write
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::write()"
//BOP
// !IROUTINE:  IO::write - Performs a write on an IO object
//
// !INTERFACE:
int IO::write(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
  char const * const file,        // (in)    - name of file being read
  ESMC_IOFmtFlag *iofmt,          // (in)    - IO format flag
  IOWriteFlag *iowriteflag,       // (in)    - IO write/append flag
  int   *timeslice                // (in)    - timeslice option
  ) {
// !DESCRIPTION:
//      Writes an {\tt ESMC\_IO} object to file
//
//EOP
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  PRINTPOS;
  // Open the file
  IOReadFlag readflag = IO_NO_READ;
  localrc = open(file, &readflag, iowriteflag, iofmt);
  std::cout << ESMC_METHOD << ": open returned " << localrc << std::endl;
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
    return rc;
  }

  localrc = write(timeslice);
  std::cout << ESMC_METHOD << ": write returned " << localrc << std::endl;

  // Close the file
  localrc = close();
  std::cout << ESMC_METHOD << ": close returned " << localrc << std::endl;
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
    return rc;
  }
    
  // return successfully
  rc = ESMF_SUCCESS;
  return (rc);
}  // end IO::write
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::write()"
//BOP
// !IROUTINE:  IO::write - Write the items in an IO object to an open file
//
// !INTERFACE:
int IO::write(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:

  int   *timeslice                // (in)    - timeslice option
  ) {
// !DESCRIPTION:
//      Write the items in an {\tt ESMC\_IO} object to an open file or stream
//      controlled by the object's IO_Handler member.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Write each item from the object list to the open IO handler
  PRINTPOS;
  std::vector<IO_ObjectContainer *>::iterator it;
  for (it = objects.begin(); it < objects.end(); ++it) {
    switch((*it)->type) {
    case IO_NULL:
      localrc = ESMF_STATUS_UNALLOCATED;
      break;
    case IO_ARRAY:
      ioHandler->arrayWrite((*it)->getArray(),
                            (*it)->getName(), timeslice, &localrc);
      break;
      // These aren't handled yet
    case IO_ATTRIBUTE:
    case IO_GRID:
    case IO_MESH:
    default:
      localrc = ESMF_STATUS_INVALID;
      break;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
      // Close the file but return original error even if close fails.
      localrc = close();
      return rc;
    }
  }
    
  // return successfully
  rc = ESMF_SUCCESS;
  return (rc);
}  // end IO::write
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::open()"
//BOP
// !IROUTINE:  IO::open - Open a file or stream for I/O
//
// !INTERFACE:
int IO::open(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:

  char const * const file,        // (in)    - name of file being read
  IOReadFlag *ioreadflag,         // (in)    - open file for reading?
  IOWriteFlag *iowriteflag,       // (in)    - open file for write/append?
  ESMC_IOFmtFlag *iofmt           // (in)    - IO format flag
  ) {
// !DESCRIPTION:
//      Open a file or stream for I/O. Create a new IO_Handler if necessary
//      It is an error if a handler exists with a different I/O format (iofmt)
//      It is an error if the IO_Handler is already connected to an open stream
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Make sure pointer inputs have something in them
  if ((char const * const)NULL == file) {
    localrc = ESMC_RC_PTR_NULL;
    ESMC_LogDefault.Write("Filename to open cannot be NULL",
                          ESMC_LOG_ERROR, ESMC_CONTEXT);
    return localrc;
  }
  PRINTPOS;
  if ((IOReadFlag *)NULL == ioreadflag) {
    localrc = ESMC_RC_PTR_NULL;
    ESMC_LogDefault.Write("IO Read Flag cannot be NULL",
                          ESMC_LOG_ERROR, ESMC_CONTEXT);
    return localrc;
  }
  if ((IOWriteFlag *)NULL == iowriteflag) {
    localrc = ESMC_RC_PTR_NULL;
    ESMC_LogDefault.Write("IO Write Flag cannot be NULL",
                          ESMC_LOG_ERROR, ESMC_CONTEXT);
    return localrc;
  }
  if ((ESMC_IOFmtFlag *)NULL == iofmt) {
    localrc = ESMC_RC_PTR_NULL;
    ESMC_LogDefault.Write("IO Format cannot be NULL",
                          ESMC_LOG_ERROR, ESMC_CONTEXT);
    return localrc;
  }

  // Ensure that we have an IO_Handler (create if necessary)
  if ((IO_Handler *)NULL == ioHandler) {
    ioHandler = IO_Handler::create(file, iofmt, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
      std::cout << ESMC_METHOD << " at " << ESMC_FILENAME << ":"
                << __LINE__ << ", IO_Handler::create returned "
                << localrc << std::endl;
      ioHandler = (IO_Handler *)NULL;
      return localrc;
    }
  } else if (ioHandler->getFormat() != *iofmt) {
    std::cout << ESMC_METHOD << " at " << ESMC_FILENAME << ":"
              << __LINE__ << ", IO_Handler::create is wrong format, "
              << ioHandler->getFormat() << " instead of, "
              << *iofmt << std::endl;
    localrc = ESMC_RC_FILE_OPEN;
    ESMC_LogDefault.Write("Internal error, ioHandler has wrong format",
                          ESMC_LOG_ERROR, ESMC_CONTEXT);
    return localrc;
  }
  // No else (state looks OK)

  // Check to make sure that a file is not already open
  if (ioHandler->isOpen() != ESMF_FALSE) {
    std::cout << ESMC_METHOD << " called at " << ESMC_FILENAME << ":"
              << __LINE__ << ", IO_Handler is already open " << std::endl;
    localrc = ESMC_RC_FILE_OPEN;
    ESMC_LogDefault.Write("Internal error, ioHandler is already open",
                          ESMC_LOG_ERROR, ESMC_CONTEXT);
    return localrc;
  }

  // Open the file
  ioHandler->open(file, ioreadflag, iowriteflag, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
    std::cout << ESMC_METHOD << " called at " << ESMC_FILENAME << ":"
              << __LINE__ << ", IO_Handler::open returned "
              << localrc << std::endl;
    return localrc;
  }
  std::cout << ESMC_METHOD << " at " << ESMC_FILENAME << ":"
            << __LINE__ << ", ioHandler->open returned "
            << localrc << std::endl;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return (rc);
}  // end IO::open
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::flush()"
//BOP
// !IROUTINE:  IO::flush - Flush data to an open file or stream
//
// !INTERFACE:
int IO::flush(void
//
// !RETURN VALUE:
//     int error return code
//
  ) {
// !DESCRIPTION:
//      Flush data to an open file or stream
//      It is not an error if the file is not open
//      It is an error if no IOHandler exists
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // First, ensure that we have an IO_Handler
  if ((IO_Handler *)NULL == ioHandler) {
    localrc = ESMC_RC_OBJ_NOT_CREATED;
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
      return rc;
    }
  }

  // Check to make sure that a file is already open
  if (ioHandler->isOpen() != ESMF_FALSE) {
    ioHandler->flush(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
      return rc;
    }
  }
    
  // return successfully
  rc = ESMF_SUCCESS;
  return (rc);
}  // end IO::flush
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::close()"
//BOP
// !IROUTINE:  IO::close - Close an open file or stream
//
// !INTERFACE:
int IO::close(void
//
// !RETURN VALUE:
//     int error return code
  ) {
// !DESCRIPTION:
//      Close an open file or stream
//      It is an error if the file is not open
//      It is an error if no IOHandler exists
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  PRINTPOS;
  // First, ensure that we have an IO_Handler
  if ((IO_Handler *)NULL == ioHandler) {
    localrc = ESMC_RC_OBJ_NOT_CREATED;
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
      return rc;
    }
  }

  // Check to make sure that a file is already open
  if (ioHandler->isOpen() != ESMF_FALSE) {
    ioHandler->flush(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
      return rc;
    }
    if (ESMF_SUCCESS == localrc) {
      ioHandler->close();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
        return rc;
      }
    }
  } else {
    localrc = ESMC_RC_FILE_CLOSE;
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
      return rc;
    }
  }
    
  // return successfully
  rc = ESMF_SUCCESS;
  return (rc);
}  // end IO::close
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::addArray()"
//BOP
// !IROUTINE:  IO::addArray - Add an array to the list of I/O objects
//
// !INTERFACE:
int IO::addArray(
//
// !RETURN VALUE:
//     int error return code
// !ARGUMENTS:
  ESMC_Base *arr_p,                // (in)    - The array to add
  const char * const variableName  // (in)    - Name to use for array
  ) {
// !DESCRIPTION:
//      Add an array to the list of objects to read or write. The 
//      {\tt variableName} argument will be used as the field name for
//      NetCDF files (or other formats requiring a name).
//      {\tt arr_p} is required
//      {\tt variableName} is not required (may be NULL), however, this
//         may cause an error when I/O is attempted.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_SUCCESS;             // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  PRINTPOS;
  // Check to make sure that an array is passed
  if ((ESMC_Base *)NULL == arr_p) {
    localrc = ESMC_RC_PTR_NULL;
    ESMC_LogDefault.Write("Array argument cannot be NULL",
                          ESMC_LOG_ERROR, ESMC_CONTEXT);
  } else {
    if (((const char * const)NULL != variableName) &&
        (strlen(variableName) > ESMF_MAXSTR)) {
      ESMC_LogDefault.Write("Array name length exceeds ESMF_MAXSTR, truncated",
                            ESMC_LOG_WARN, ESMC_CONTEXT);
    }
    try {
      IO_ObjectContainer *newObj = new IO_ObjectContainer((Array *)arr_p,
                                                          variableName);
      if ((IO_ObjectContainer *)NULL == newObj) {
        localrc = ESMC_RC_MEM_ALLOCATE;
        ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
        ESMC_LogDefault.Write("Unable to allocate storage for IO object",
                              ESMC_LOG_ERROR, ESMC_CONTEXT);
      } else {
        objects.push_back(newObj);
      }
    } catch(...) {
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
    }
  }
    
  // return successfully
  rc = localrc;
  return (rc);
}  // end IO::addArray
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::clear()"
//BOP
// !IROUTINE:  IO::clear - Clear all objects from this IO's I/O queue
//
// !INTERFACE:
void IO::clear(void) {
// !DESCRIPTION:
//      Clear all objects from this IO's I/O queue (e.g., arrays's, attributes)
//      The IO_Handler object belonging to this is not affected
//
//EOP
//-----------------------------------------------------------------------------

  IO_ObjectContainer *obj;

  PRINTPOS;
  // Clear out the objects list.
  while (!objects.empty()) {
    obj = objects.back();
    objects.pop_back();
    delete obj;
  }
}  // end IO::close
//-------------------------------------------------------------------------

}  // end namespace ESMCI
