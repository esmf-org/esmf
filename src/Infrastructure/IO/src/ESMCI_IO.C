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
#include "ESMC_Interface.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

#include "esmf_io_debug.h"

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
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
    try {
      IO_Handler::destroy(&ioHandler);
      ioHandler = (IO_Handler *)NULL;
    } catch(int localrc) {
      int rc;
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc);
      // Don't return, try to finish anyway
    } catch(...) {
      int rc;
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD,
        "- Caught exception", ESMC_CONTEXT, &rc);
      // Don't return, try to finish anyway
    }
  }
  // Delete all object nodes
  try {
    while(!objects.empty()) {
      IO_ObjectContainer *obj = objects.back();
      objects.pop_back();
      delete(obj);
    }
  } catch(int localrc) {
    int rc;
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return;
  } catch(...) {
    int rc;
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD,
      "- Caught exception", ESMC_CONTEXT, &rc);
    return;
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
  if (rc != NULL) {
    *rc = ESMC_RC_NOT_IMPL;               // final return code
  }

  IO *ioclass;

  PRINTPOS;
  // call class constructor
  try{
    ioclass = new IO(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) {
      ioclass = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  } catch(...) {
    // allocation error
    ioclass = ESMC_NULL_POINTER;
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to ioclass", ESMC_CONTEXT, &rc);
    return rc;
  }

  try {
    // delete the IO object (this will call destruct)
    delete (*ioclass);
    *ioclass = ESMC_NULL_POINTER;
    localrc = ESMF_SUCCESS;
  } catch(int localrc) {
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  } catch(...) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, &rc);
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
  ESMC_IOFmt_Flag iofmt,          // (in)    - IO format flag
  int   *timeslice                // (in)    - timeslice option
  ) {
// !DESCRIPTION:
//      Reads an {\tt ESMC\_IO} object from file
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc1 = ESMC_RC_NOT_IMPL;        // local return code
  int localrc2 = ESMC_RC_NOT_IMPL;        // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  PRINTPOS;
  // Open the file
  bool overwrite = false;
  bool readonly = true;
  localrc1 = open(file, ESMC_FILESTATUS_OLD, iofmt, overwrite, readonly);
  if (ESMC_LogDefault.MsgFoundError(localrc1, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    switch(rc) {
    case ESMF_RC_LIB_NOT_PRESENT:
    case ESMF_RC_ARG_BAD:
      return rc;
    default:
      return ESMF_RC_FILE_READ;
    }
  }

  localrc1 = read(timeslice);
  // Can't quit even if error; Have to close first
  PRINTMSG("read returned " << localrc1);

  // Close the file
  localrc2 = close();
  PRINTMSG("close returned " << localrc2);
  if (ESMC_LogDefault.MsgFoundError(localrc1, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    return ESMF_RC_FILE_READ;
  } else if (ESMC_LogDefault.MsgFoundError(localrc2, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    return ESMF_RC_FILE_READ;
  }
    
  // return
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
  bool need_redist, has_undist;
  for (it = objects.begin(); it < objects.end(); ++it) {
    Array *temp_array_p = (*it)->getArray();  // default to caller-provided Array
    Array *temp_array_undist_p;               // temp when Array has undistributed dimensions
    ESMCI::RouteHandle *rh;
    switch((*it)->type) {
    case IO_NULL:
      localrc = ESMF_STATUS_UNALLOCATED;
      break;
    case IO_ARRAY:
      // Check for undistributed dimensions
      has_undist = undist_check (temp_array_p, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
        return rc;

      if (has_undist) {
        temp_array_undist_p = temp_array_p;
        // Create an aliased Array which treats all dimensions as distributed.
        // std::cout << ESMC_METHOD << ": calling undist_arraycreate_alldist" << std::endl;
        undist_arraycreate_alldist (temp_array_undist_p, &temp_array_p, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;
      }

      // Check for redistribution (when > 1 DE per PET)
      need_redist = redist_check(temp_array_p, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc)) {
      // Close the file but return original error even if close fails.
        localrc = close();
        return rc;
      }
      if (need_redist) {
        // Create a compatible temp Array with 1 DE per PET
        // std::cout << ESMC_METHOD << ": calling redist_arraycreate1de" << std::endl;
        redist_arraycreate1de((*it)->getArray(), &temp_array_p, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &rc)) {
        // Close the file but return original error even if close fails.
          localrc = close();
          return rc;
        }
      }

      // Read data into the Array
      // std::cout << ESMC_METHOD << ": calling arrayRead" << std::endl;
      ioHandler->arrayRead(temp_array_p,
                          (*it)->getName(), timeslice, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
        return rc;

      if (need_redist) {
        // Redistribute into the caller supplied Array
        // std::cout << ESMC_METHOD << ": DE count > 1 - redistStore" << std::endl;
        localrc = ESMCI::Array::redistStore(temp_array_p, (*it)->getArray(), &rh, NULL);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;

        // std::cout << ESMC_METHOD << ": DE count > 1 - redistribute data" << std::endl;
        localrc = ESMCI::Array::redist(temp_array_p, (*it)->getArray(), &rh);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;

        localrc = temp_array_p->redistRelease(rh);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;
        // std::cout << ESMC_METHOD << ": DE count > 1 - redistribute complete!" << std::endl;

        // Cleanups
        // std::cout << ESMC_METHOD << ": cleaning up" << std::endl;
        localrc = ESMCI::Array::destroy(&temp_array_p);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;
      }
      break;
      // These aren't handled yet
    case IO_ATTRIBUTE:
    case IO_GRID:
    case IO_MESH:
    default:
      localrc = ESMF_STATUS_INVALID;
      if (ESMC_LogDefault.MsgFoundError(localrc, "Unhandled read case type", ESMC_CONTEXT, &rc))
          return rc;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
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
  char const * const file,        // (in)    - name of file being written
  ESMC_IOFmt_Flag iofmt,          // (in)    - IO format flag
  bool overwrite,                 // (in)    - overwrite fields if true
  ESMC_FileStatus_Flag status,    // (in)    - file status flag
  int   *timeslice                // (in)    - timeslice option
  ) {
// !DESCRIPTION:
//      Writes an {\tt ESMC\_IO} object to file
//
//EOP
  // initialize return code; assume routine not implemented
  int localrc1 = ESMC_RC_NOT_IMPL;        // local return code
  int localrc2 = ESMC_RC_NOT_IMPL;        // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  PRINTPOS;
  // Open the file
  localrc1 = open(file, status, iofmt, overwrite);
  PRINTMSG("open returned " << localrc1);
  if (ESMC_LogDefault.MsgFoundError(localrc1, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    switch(rc) {
    case ESMF_RC_LIB_NOT_PRESENT:
    case ESMF_RC_ARG_BAD:
      return rc;
    default:
      return ESMF_RC_FILE_WRITE;
    }
  }

  localrc1 = write(timeslice);
  if (ESMC_LogDefault.MsgFoundError(localrc1, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  PRINTMSG("write returned " << localrc1);
  // Can't quit even if error; Have to close first

  // Close the file
  localrc2 = close();
  PRINTMSG("close returned " << localrc2);
  if (ESMC_LogDefault.MsgFoundError(localrc1, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    return ESMF_RC_FILE_WRITE;
  } else if (ESMC_LogDefault.MsgFoundError(localrc2, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, &rc)) {
    return ESMF_RC_FILE_WRITE;
  }
    
  // return
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
  bool need_redist, has_undist;
  for (it = objects.begin(); it < objects.end(); ++it) {
    Array *temp_array_p = (*it)->getArray();  // default to caller-provided Array
    DistGrid *dg = temp_array_p->getDistGrid ();

    int tilecount = dg->getTileCount ();
    if (tilecount != 1) {
      localrc = ESMF_RC_NOT_IMPL;
      if (ESMC_LogDefault.MsgFoundError(localrc, "tilecount != 1 not supported yet", ESMC_CONTEXT,
          &rc)) {
      // Close the file but return original error even if close fails.
        localrc = close();
        return rc;
      }
    }

    Array *temp_array_undist_p;  // temp when Array has undistributed dimensions
    ESMCI::RouteHandle *rh;
    switch((*it)->type) {
    case IO_NULL:
      localrc = ESMF_STATUS_UNALLOCATED;
      break;
    case IO_ARRAY:
      need_redist = redist_check(temp_array_p, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc)) {
      // Close the file but return original error even if close fails.
        localrc = close();
        return rc;
      }
      // std::cout << ESMC_METHOD << ": need_redist = " << (need_redist?"y":"n") << std::endl;
      if (need_redist) {
        // Create a compatible temp Array with 1 DE per PET
        // std::cout << ESMC_METHOD << ": DE count > 1 - redist_arraycreate1de" << std::endl;
        redist_arraycreate1de((*it)->getArray(), &temp_array_p, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
            &rc)) {
        // Close the file but return original error even if close fails.
          localrc = close();
          return rc;
        }

        // Redistribute into the temp Array
        // std::cout << ESMC_METHOD << ": DE count > 1 - redistStore" << std::endl;
        localrc = ESMCI::Array::redistStore((*it)->getArray(), temp_array_p, &rh, NULL);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
        // Close the file but return original error even if close fails.
          localrc = close();
          return rc;
        }

        // std::cout << ESMC_METHOD << ": DE count > 1 - redistribute data" << std::endl;
        localrc = ESMCI::Array::redist((*it)->getArray(), temp_array_p, &rh);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
        // Close the file but return original error even if close fails.
          localrc = close();
          return rc;
        }
        // std::cout << ESMC_METHOD << ": DE count > 1 - redistribute complete!" << std::endl;
      }

      // Check for undistributed dimensions
      has_undist = undist_check (temp_array_p, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
        return rc;

      if (has_undist) {
        temp_array_undist_p = temp_array_p;
        // Create an aliased Array which treats all dimensions as distributed.
        // std::cout << ESMC_METHOD << ": calling undist_arraycreate_alldist()" << std::endl;
        undist_arraycreate_alldist (temp_array_undist_p, &temp_array_p, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;
      }

      // Write the Array
      ioHandler->arrayWrite(temp_array_p,
                            (*it)->getName(), timeslice, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
        return rc;

      // Clean ups //
      if (need_redist) {
        localrc = temp_array_p->redistRelease(rh);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;
        localrc = ESMCI::Array::destroy(&temp_array_p);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
          return rc;
      }
      break;
      // These aren't handled yet
    case IO_ATTRIBUTE:
    case IO_GRID:
    case IO_MESH:
    default:
      localrc = ESMF_STATUS_INVALID;
      if (ESMC_LogDefault.MsgFoundError(localrc, "Unhandled write case type", ESMC_CONTEXT, &rc))
          return rc;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
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

  char const * const file,             // (in)  - name of file being read
  ESMC_FileStatus_Flag filestatusflag, // (in)  - file status flag
  ESMC_IOFmt_Flag iofmt,               // (in)  - IO format flag
  bool overwrite,                      // (in)  - overwrite fields?
  bool readonly                        // (in)  - If false then read/write
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

  PRINTPOS;
  // Make sure pointer inputs have something in them
  if (NULL == file) {
    localrc = ESMC_RC_PTR_NULL;
    ESMC_LogDefault.Write("Filename to open cannot be NULL",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    return localrc;
  }

  // Ensure that we have an IO_Handler (create if necessary)
  if ((IO_Handler *)NULL == ioHandler) {
    ioHandler = IO_Handler::create(file, iofmt, &localrc);
    if (ESMF_SUCCESS != localrc) {
      PRINTMSG("IO_Handler::create returned " << localrc);
      ioHandler = (IO_Handler *)NULL;
      return localrc;
    }
  } else if (ioHandler->getFormat() != iofmt) {
    PRINTMSG("IO_Handler::create is wrong format, " <<
             ioHandler->getFormat() << " instead of, " << iofmt);
    localrc = ESMC_RC_FILE_OPEN;
    ESMC_LogDefault.Write("Internal error, ioHandler has wrong format",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    return localrc;
  }
  // No else (state looks OK)

  // Check to make sure that a file is not already open
  if (ioHandler->isOpen() != ESMF_FALSE) {
    PRINTMSG("IO_Handler is already open ");
    localrc = ESMC_RC_FILE_OPEN;
    ESMC_LogDefault.Write("Internal error, ioHandler is already open",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    return localrc;
  }

  // Open the file
  ioHandler->open(file, filestatusflag, overwrite, readonly, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    PRINTMSG("IO_Handler::open returned " << localrc);
    return localrc;
  }
    
  // return successfully
  rc = localrc;
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
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
      return rc;
    }
  }

  // Check to make sure that a file is already open
  if (ioHandler->isOpen() != ESMF_FALSE) {
    ioHandler->flush(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
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
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
      return rc;
    }
  }

  // Check to make sure that a file is already open
  if (ioHandler->isOpen() != ESMF_FALSE) {
    ioHandler->flush(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
      return rc;
    }
    if (ESMF_SUCCESS == localrc) {
      ioHandler->close();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) {
        return rc;
      }
    }
  } else {
    localrc = ESMC_RC_FILE_CLOSE;
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) {
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
  Array *arr_p,               // (in)    - The array to add
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
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    return localrc;
  }

  if ((NULL != variableName) &&
      (strlen(variableName) > ESMF_MAXSTR)) {
    ESMC_LogDefault.Write("Array name length exceeds ESMF_MAXSTR",
                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    return ESMF_RC_LONG_STR;
  }

// Push Array onto the list
  try {
    IO_ObjectContainer *newObj = new IO_ObjectContainer((Array *)arr_p, variableName);

    if ((IO_ObjectContainer *)NULL == newObj) {
      localrc = ESMC_RC_MEM_ALLOCATE;
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
      ESMC_LogDefault.Write("Unable to allocate storage for IO object",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    } else {
      objects.push_back(newObj);
    }
  } catch(...) {
    PRINTMSG("CATCH: Alloc error!!");
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
  }
    
  // return
  if (ESMC_RC_NOT_IMPL == rc) {
    rc = localrc;
  }
  return (rc);
}  // end IO::addArray
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::redist_check()"
//BOP
// !IROUTINE:  IO::redist_check
//
// !INTERFACE:
bool IO::redist_check(Array *array_p, int *rc) {
// !DESCRIPTION:
//      Check for any PET that has DE count != 1.  Redist will be required.
//
//EOP
//-----------------------------------------------------------------------------

  int localrc;
  VM *currentVM = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
    return false;
  int npets = currentVM->getNpets();
  int localDeCount = array_p->getDELayout()->getLocalDeCount();
  std::vector<int> deCounts_send(npets, localDeCount);
  std::vector<int> deCounts_recv(npets);
  localrc = currentVM->VMK::allgather (&deCounts_send[0], &deCounts_recv[0], sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
    return false;
  }

  bool need_redist  = false;
  for (int i=0; i<npets; i++) {
    if (deCounts_recv[i] != 1) {
      need_redist = true;
      break;
    }
  }

  if (rc) *rc = ESMF_SUCCESS;
  return need_redist;

}  // end IO::redist_check
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::redist_arraycreate1de()"
//BOP
// !IROUTINE:  IO::redist_array
//
// !INTERFACE:
void IO::redist_arraycreate1de(Array *src_array_p, Array **dest_array_p, int *rc) {
// !DESCRIPTION:
//      Create a dest Array with DE count on each PET = 1, based on src Array.
//
//EOP
//-----------------------------------------------------------------------------

  int localrc;

  DistGrid *dg_orig = src_array_p->getDistGrid();
  const int *minIndexTile = dg_orig->getMinIndexPDimPTile();
  const int *maxIndexTile = dg_orig->getMaxIndexPDimPTile();

  int ndims = dg_orig->getDimCount();
  int localTileCount = dg_orig->getTileCount();
  localrc = (localTileCount == 1) ? ESMF_SUCCESS : ESMF_RC_NOT_IMPL;
  if (ESMC_LogDefault.MsgFoundError(localrc, "Tile count != 1 is not supported", ESMC_CONTEXT, rc))
    return;

  ESMCI::InterfaceInt minIndexInterface((int*)minIndexTile, ndims);
#if 0
  std::cout << ESMC_METHOD << "[" << me << "]: setting maxindex to: (";
  for (int i=0; i<ndims; i++)
    std::cout << " " << maxIndexTile[i];
  std::cout << " )" << std::endl;
#endif
  ESMCI::InterfaceInt maxIndexInterface((int*)maxIndexTile, ndims);
  DistGrid *distgrid = DistGrid::create(&minIndexInterface, &maxIndexInterface, NULL,
      NULL, 0, NULL, NULL, NULL, NULL, NULL, (ESMCI::DELayout*)NULL, NULL,
      &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
    return;

  // std::cout << ESMC_METHOD << ": creating temp Array for redistribution" << std::endl;
  ESMCI::ArraySpec arrayspec;
  arrayspec.set (src_array_p->getRank(), src_array_p->getTypekind());

  ESMCI::Array *temp_arr_p = ESMCI::Array::create(&arrayspec, distgrid,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
    return;

  temp_arr_p->setName (src_array_p->getName());

  *dest_array_p = temp_arr_p;
  if (rc) *rc = ESMF_SUCCESS;

}  // end IO::redist_arraycreate1de
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::undist_check()"
//BOP
// !IROUTINE:  IO::undist_check - check for undistributed dimensions
//
// !INTERFACE:
bool IO::undist_check(Array *array_p, int *rc) {
// !DESCRIPTION:
//      Check for presence of undistributed dimensions
//
//EOP
//-------------------------------------------------------------------------

  int tc = array_p->getTensorCount();
  if (rc) *rc = ESMF_SUCCESS;
  return tc != 0;

}  // end IO::undist_check
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::undist_arraycreate_alldist()"
//BOP
// !IROUTINE:  IO::undist_arraycreate_alldist
//
// !INTERFACE:
void IO::undist_arraycreate_alldist(Array *src_array_p, Array **dest_array_p, int *rc) {
// !DESCRIPTION:
//      Create a dest Array with all dimensions considered distributed,
//      even though some are not.  Data elements are aliased to those in
//      the src Array.
//
//EOP
//-----------------------------------------------------------------------------

  int localrc;

  int rank = src_array_p->getRank ();
  DistGrid *dg = src_array_p->getDistGrid ();
  int dimcount = dg->getDimCount ();
  int tilecount = dg->getTileCount ();

  const int *arrayToDistGridMap = src_array_p->getArrayToDistGridMap();
  const int *undistLBound = src_array_p->getUndistLBound();
  const int *undistUBound = src_array_p->getUndistUBound();
  const int *minIndexPTile = dg->getMinIndexPDimPTile ();
  const int *maxIndexPTile = dg->getMaxIndexPDimPTile ();

  // construct the new minIndex and maxIndex
//std::cout << ESMC_METHOD << ": construct the new minIndex and maxIndex." << std::endl;
  std::vector<int> minIndexNew(rank);
  std::vector<int> maxIndexNew(rank);
  int jj = 0;
  for (int i=0; i<rank; i++) {
    int j = arrayToDistGridMap[i];
    if (j > 0) {
      // valid DistGrid dimension
      minIndexNew[i] = minIndexPTile[j-1];
      maxIndexNew[i] = maxIndexPTile[j-1];
    } else {
      // undistributed dimension
      minIndexNew[i] = undistLBound[jj];
      maxIndexNew[i] = undistUBound[jj];
      jj++;
    }
  }

  // general dimensionality of regDecomp
  std::vector<int> regDecomp(rank);
  VM *currentVM = VM::getCurrent(&localrc);
  regDecomp[0] = currentVM->getNpets(); // first element petCount for default distribution
  for (int i=1; i<rank; i++)  // default all other dims to 1
    regDecomp[i] = 1;

  // create the fixed up DistGrid
//std::cout << ESMC_METHOD << ": creating new DistGrid" << std::endl;
  ESMCI::InterfaceInt minIndexInterface(&minIndexNew[0], rank);
  ESMCI::InterfaceInt maxIndexInterface(&maxIndexNew[0], rank);
  ESMCI::InterfaceInt regDecompInterface(&regDecomp[0] , rank);
  DistGrid *dg_temp = DistGrid::create(&minIndexInterface,
                 &maxIndexInterface, &regDecompInterface,
      NULL, 0, NULL, NULL, NULL, NULL, NULL, (ESMCI::DELayout*)NULL, NULL,
      &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
    return;
  }

  // finally, create the fixed up Array using pointer to original data.
  // Assuming only 1 DE/PET since redist step would have been performed previously.
//std::cout << ESMC_METHOD << ": creating alias Array" << std::endl;
  CopyFlag copyflag = DATA_REF;
  *dest_array_p = Array::create (src_array_p->getLocalarrayList(), 1,
      dg_temp, copyflag,
      NULL, NULL, NULL, NULL, NULL,
      NULL, NULL,
      NULL, NULL, NULL,
      &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
    return;
  }

  (*dest_array_p)->setName(src_array_p->getName());

  if (rc) *rc = ESMF_SUCCESS;

}  // end IO::undist_arraycreate_alldist
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
