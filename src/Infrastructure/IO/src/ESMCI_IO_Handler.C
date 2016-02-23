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
// The code in this file implements the C++ {\tt ESMC\_IO_Handler} methods
// declared in the companion file {\tt ESMCI\_IO_Handler.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_IO_Handler.C"

// include associated header file
#include "ESMCI_IO_Handler.h"

// higher level, 3rd party or system includes here
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <vector>
#include <iostream>
#include <fstream>

// other ESMF include files here.
#include "ESMCI_Macros.h"
#include "ESMCI_Container.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_PIO_Handler.h"

#define ROOT_PET (0)

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
#define ESMC_METHOD "ESMCI::IO_Handler::IO_Handler()"
//BOPI
// !IROUTINE:  ESMCI::IO_Handler::IO_Handler    - constructor
//
// !INTERFACE:
IO_Handler::IO_Handler (
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
  ESMC_IOFmt_Flag fmtArg               // (in)  - the desired I/O format
//
  ) {
//
// !DESCRIPTION:
//    Fill the internal information of an ESMCI::IO_Handler object.
//
//EOPI
//-----------------------------------------------------------------------------
  localPet = 0;
  indexflag = ESMC_INDEX_DELOCAL;
  iofmtFlag = fmtArg;
  fileStatusFlag = ESMC_FILESTATUS_UNKNOWN;
  overwrite = false;
  filename[0] = '\0';

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Handler::create()"
//BOPI
// !IROUTINE:  ESMCI::IO_Handler::create
//
// !INTERFACE:
IO_Handler *IO_Handler::create (
//
// !RETURN VALUE:
//    IO_Handler * to newly allocated IO_Handler
//
// !ARGUMENTS:
      ESMC_IOFmt_Flag iofmt,              // (in)  the desired I/O format
//
  int *rc                                 // (out) return code
  ) {
//
// !DESCRIPTION:
//    Create an initialized {\tt IO_Handler} object of the correct type for
//    the specified I/O format (iofmt).
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;            // local return code
  char errmsg[256];                          // Specific error message
  IO_Handler *iohandler = ESMC_NULL_POINTER; // New handler to return
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }


  // call class constructor
  try {
    // Determine if we have the support for the requested I/O format
    switch (iofmt) {
    case ESMF_IOFMT_BIN:
#ifdef ESMF_PIO
      iohandler = new PIO_Handler(iofmt, &localrc);
#else // ESMF_PIO
      localrc = ESMF_RC_LIB_NOT_PRESENT;
      ESMC_LogDefault.Write("PIO library required for I/O operation",
                            ESMC_LOGMSG_WARN, ESMC_CONTEXT);
#endif // ESMF_PIO
      break;
    case ESMF_IOFMT_NETCDF:
      // No break
    case ESMF_IOFMT_NETCDF4P:
      // No break
    case ESMF_IOFMT_NETCDF4C:
#if  defined(ESMF_PIO) && (defined(ESMF_NETCDF) || defined(ESMF_PNETCDF))
      iohandler = new PIO_Handler(iofmt, &localrc);
#else // defined(ESMF_PIO) && (defined(ESMF_NETCDF) || defined(ESMF_PNETCDF))
      sprintf(errmsg, "PIO & (P)NetCDF libraries required for I/O operation");
      localrc = ESMF_RC_LIB_NOT_PRESENT;
#endif // defined(ESMF_PIO) && (defined(ESMF_NETCDF) || defined(ESMF_PNETCDF))
      break;
    default:
      localrc = ESMF_RC_ARG_BAD;
      break;
    }
    // Process the result
    switch (localrc) {
    case ESMF_SUCCESS:
      // No action needed, should have a good PIO Handler.
      break;
    case ESMF_RC_LIB_NOT_PRESENT:
      ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
      break;
    case ESMF_RC_ARG_BAD:
      ESMC_LogDefault.Write("Unknown I/O Format",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      break;
    default:
      ESMC_LogDefault.Write("Unknown I/O Error",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    }
  } catch(...) {
    // allocation error
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);
  }

  // return
  if (rc != NULL) {
    *rc = localrc;
  }
  return iohandler;
} // end IO_Handler::create
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Handler::create()"
//BOPI
// !IROUTINE:  ESMCI::IO_Handler::create
//
// !INTERFACE:
IO_Handler *IO_Handler::create (
//
// !RETURN VALUE:
//    IO_Handler * to newly allocated IO_Handler
//
// !ARGUMENTS:
      const std::string& file,             // (in) A file for Handler
      ESMC_IOFmt_Flag iofmt,               // (in) the desired I/O format
//
  int *rc                                  // (out) return code
  ) {
//
// !DESCRIPTION:
//    Create an initialized {\tt IO_Handler} object of the correct type for
//    the specified I/O format (iofmt).
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;   // final return code
  }

  IO_Handler *iohandler = IO_Handler::create(iofmt, &localrc);

  if (ESMC_NULL_POINTER != iohandler)
    iohandler->filename = file;

  // return successfully
  if (rc != NULL) {
    *rc = localrc;
  }
  return iohandler;;
} // end IO_Handler::create
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Handler::destroy()"
//BOPI
// !IROUTINE:  IO_Handler::destroy - free an IO created with Create
//
// !INTERFACE:
int IO_Handler::destroy (
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
  IO_Handler **ioclass) {  // in - IO_Handler to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys an IO object previously allocated
//      via an {\tt ESMCI\_IOCreate} routine.  Define for deep classes only.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMF_RC_NOT_IMPL;              // final return code
  int localrc = ESMF_RC_NOT_IMPL;         // local return code

  // return with errors for NULL pointer
  if (ioclass == ESMC_NULL_POINTER || *ioclass == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL,
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
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, &rc);
    return rc;
  }
#ifdef ESMF_PIO
    // PIO
    PRINTPOS;
    PIO_Handler::finalize(&localrc);
    PRINTMSG("after finalize, localrc = " << localrc);
    if (ESMF_SUCCESS != localrc) {
      char errmsg[256];
      sprintf(errmsg, "PIO_Handler::finalize error = %d", localrc);
      ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }
#endif // ESMF_PIO

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
} // end IO_Handler::destroy
//-------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Handler::finalize()"
//BOPI
// !IROUTINE:  IO_Handler::finalize - Close connections and free resources
//
// !INTERFACE:
void IO_Handler::finalize (
//
// !RETURN VALUE:
//
// !ARGUMENTS:
  int *rc) {                          // (out) - Status code
//
// !DESCRIPTION:
//      Static function to ensure that all open files and streams are closed.
//      Also, any resources used to store filesystem or I/O information is
//      deleted.
//
//EOPI
//-----------------------------------------------------------------------------
  // Because this routine may not do anything (depends on compilation options),
  // we initialize the return code as success
  int localrc = ESMF_SUCCESS;             // local return code
  if ((int *)NULL != rc) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  try {
    // We don't have any open files or resources, however, classes descended
    // from us might.
    // This is not very OO-like but we have to have some place to store
    // knowledge about other classes which might have static information
    // that needs cleaning up.

#ifdef ESMF_PIO
    // PIO
    PRINTPOS;
    PIO_Handler::finalize(&localrc);
    PRINTMSG("after finalize, localrc = " << localrc);
    if (ESMF_SUCCESS != localrc) {
      char errmsg[256];
      sprintf(errmsg, "PIO_Handler::finalize error = %d", localrc);
      ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }
#endif // ESMF_PIO
    // If we need to call other finalize routines, we need to decide what
    // to do about the final return code since we should call all
    // finalize routines even if one fails.
  } catch(int localrc) {
    // catch standard ESMF return code
    PRINTMSG("caught passthru, localrc = " << localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return;
  } catch(...) {
    PRINTMSG("caught unknown error");
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, rc);
    return;
  }

  PRINTMSG("before return, localrc = " << localrc);
  // return successfully
  if ((int *)NULL != rc) {
    *rc = localrc;
  }
} // end IO_Handler::finalize
//-------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Handler::setFilename()"
//BOPI
// !IROUTINE:  IO_Handler::setFilename - Set a filename for this handler
//
// !INTERFACE:
int IO_Handler::setFilename(
//
// !RETURN VALUE:
//    int error or success return code
//
// !ARGUMENTS:
  const std::string& name             // (in) - The new filename
) {
//
// !DESCRIPTION:
//      Set a new filename for this IO Handler.
//      Return ESMF_SUCCESS if successful
//      It is an error if a file is presently open (ESMF_RC_FILE_ACTIVE)
//      It is an error if the new name is too long (ESMF_RC_LONG_NAME)
//      If name is NULL, clear the current filename
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMF_RC_NOT_IMPL;              // final return code
  int localrc = ESMF_RC_NOT_IMPL;         // local return code

  // It is an error if we already have an open file
  if (isOpen() != ESMF_FALSE) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_FILE_ACTIVE,
      "- Cannot change name, file open", ESMC_CONTEXT, &rc);
    return rc;
  }

  // clear name for NULL pointer
  if (name.empty())
    filename = "";
  else
    filename = name;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
} // end IO_Handler::setFilename
//-------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Handler::fileExists()"
//BOPI
// !IROUTINE:  IO_Handler::fileExists - See if file exists for read/write
//
// !INTERFACE:
bool IO_Handler::fileExists(
//
// !RETURN VALUE:
//    bool true if file exists and meets the input requirements
//
// !ARGUMENTS:
  const std::string& name,            // (in) - filename to test
  bool needWrite                      // (in) - true if file write is required
) {
//
// !DESCRIPTION:
//      Determine if the file exists with the required permissions.
//      If needRead is true, file must exist with read permission
//      If needWrite is true, file must exist with write permission
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code
  bool fileOK = false;
  std::ios_base::openmode iomode = std::ios_base::binary;
  ESMCI::VM *vm;
  int localPet;
  int localrc;

  // BOGUS: On some systems (I'm looking at you IBM), opening a file with
  //        write on but in off automatically truncates the file.
  //        Solution is to always use in.
  iomode |= std::ios_base::in;
  if (needWrite) {
    iomode |= std::ios_base::out;
  }

  vm = ESMCI::VM::getCurrent(&localrc);
  if (ESMF_SUCCESS == localrc) {
    localPet = vm->getLocalPet();
    if (ROOT_PET == localPet) {
      // Get the file status and broadcast to all PETs
      std::fstream filestr (name.c_str(), iomode);
      fileOK = (filestr.good());
      // filestr will automatically close when function exits
      localrc = vm->broadcast(&fileOK, sizeof(bool), ROOT_PET);
    } else {
      // Non-root PETs just participate in the broadcast
      localrc = vm->broadcast(&fileOK, sizeof(bool), ROOT_PET);
    }
    if (ESMF_SUCCESS != localrc) {
      char errmsg[ESMF_MAXSTR + 64];
      sprintf(errmsg, "Error finding file status for \"%s\"", name.c_str());
      ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    }
  } else {
    // We don't seem to have a VM so just do this on all PETs
    // Log a warning anyway
    ESMC_LogDefault.Write("Unable to obtain a VM",
                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    std::fstream filestr (name.c_str(), iomode);
    fileOK = (filestr.good());
    // filestr will automatically close when function exits
  }
  PRINTMSG("File, \"" << name << "\" " <<
           (fileOK ? "exists" : "does not exist"));
  return fileOK;
} // end IO_Handler::fileExists
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Handler::open()"
//BOP
// !IROUTINE:  IO::open - Open a file or stream for I/O
//
// !INTERFACE:
void IO_Handler::open (
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:

  char const * const file,                 // (in)  - name of file being read
  ESMC_FileStatus_Flag filestatusflag_arg, // (in)  - file status
  bool overwrite_arg,                      // (in)  - overwrite fields is true
  bool readonly_arg,                       // (in)  - if false then read/write
  int *rc                                  // (out) - return code
  ) {
// !DESCRIPTION:
//      Open a file or stream for I/O. Create a new IO_Handler if necessary
//      It is an error if a handler exists with a different I/O format (iofmt)
//      It is an error if the IO_Handler is already connected to an open stream
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_SUCCESS;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;   // final return code
  }

  // Make sure pointer inputs have something in them
  if ((char *)NULL == file) {
    localrc = ESMF_RC_PTR_NULL;
    ESMC_LogDefault.MsgFoundError(localrc, "- NULL filename argument pointer",
      ESMC_CONTEXT, rc);
  } else if (isOpen() == ESMF_TRUE) {
    // Check to make sure that a file is not already open
    localrc = ESMF_RC_FILE_OPEN;
    ESMC_LogDefault.MsgFoundError(localrc, "- File already open", ESMC_CONTEXT,
      rc);
  }

  if (ESMF_SUCCESS == localrc) {
    // Set the filename
    localrc = setFilename(file);
  }

  if (ESMF_SUCCESS == localrc) {
    // Store the IO status and overwrite fields
    fileStatusFlag = filestatusflag_arg;
    overwrite = overwrite_arg;
    // Open the file
    open(readonly_arg, &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, "- Error opening file", ESMC_CONTEXT,
      rc);
  }

  // return
  if (rc != (int *)NULL) {
    *rc = localrc;
  }
}  // end IO::open
//-------------------------------------------------------------------------

}  // end namespace ESMCI
