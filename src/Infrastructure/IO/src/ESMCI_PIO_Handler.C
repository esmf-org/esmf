// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research,
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
// The code in this file implements the C++ {\tt ESMC\_PIO_Handler} methods
// declared in the companion file {\tt ESMCI\_PIO_Handler.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_PIO_Handler.C"

// include associated header file
#include "ESMCI_PIO_Handler.h"

// higher level, 3rd party or system includes here
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <vector>
#include <iostream>
#include <fstream>

// other ESMF include files here.
#include "ESMCI_Macros.h"
#include "ESMCI_Container.h"
#include "ESMC_VM.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_ArrayBundle.h"

// Define PIO NetCDF and Parallel NetCDF flags
#ifdef ESMF_PNETCDF
# define _PNETCDF
#include <pnetcdf.h>
# elif ESMF_NETCDF
# define _NETCDF
# include <netcdf.h>
#endif
#include "pio.h"
#include "pio_types.h"

#include "esmf_io_debug.h"

// For error checking
#define CHECKPIOERROR(_err, _str, _rc)                                        \
  PIO_Handler::CheckPIOError((_err), ESMC_CONTEXT, (_str), &(_rc))
#define CHECKPIOWARN(_err, _str, _rc)                                         \
  PIO_Handler::CheckPIOError((_err), ESMC_CONTEXT, (_str), &(_rc), true)

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
// private helper class for managing PIO I/O Descriptors
//
//-------------------------------------------------------------------------
//
  class PIO_IODescHandler {
  private:
    static std::vector<PIO_IODescHandler *> activePioIoDescriptors;
    pio_iosystem_desc_t ios;     // PIO IO system instance for this descriptor
    pio_io_desc_t io_descriptor; // PIO IO descriptor
    int nDims;                   // The number of dimensions for Array IO
    int *dims;                   // The shape of the Array IO
    int basepiotype;             // PIO version of Array data type
    Array *array_p;              // The array matched to this descriptor
    int arrayRank;               // The rank of array_p
    int *arrayShape;             // The shape of array_p
  public:
    PIO_IODescHandler(pio_iosystem_desc_t iosArg, Array *arrayArg) {
      ios = iosArg;
      io_descriptor = (pio_io_desc_t)NULL;
      array_p = arrayArg;
      nDims = 0;
      dims = (int *)NULL;
      arrayRank = 0;
      arrayShape = (int *)NULL;
    }
    // These definitions are at the end of the file
  public:
    ~PIO_IODescHandler();
    static void finalize(void);
    static int constructPioDecomp(pio_iosystem_desc_t iosys, Array *arr_p,
                                  pio_io_desc_t *newDecomp_p);
    static int freePioDecomp(pio_io_desc_t *decomp_p);
    static int getDims(const pio_io_desc_t &iodesc,
                       int * nioDims = (int *)NULL,
                       int ** ioDims = (int **)NULL,
                       int * narrDims = (int *)NULL,
                       int ** arrDims = (int **)NULL);
    static int getIOType(const pio_io_desc_t &iodesc, int *rc = (int *)NULL);
    static pio_io_desc_t getIODesc(pio_iosystem_desc_t iosys,
                                   Array *arrayArg, int *rc = (int *)NULL);
  };

//
//-------------------------------------------------------------------------
//
// class variables
//
//-------------------------------------------------------------------------
//

  std::vector<pio_iosystem_desc_t> PIO_Handler::activePioInstances;
  std::vector<PIO_IODescHandler *> PIO_IODescHandler::activePioIoDescriptors;

//
//-------------------------------------------------------------------------
//
// initialize and finalize
//
//-------------------------------------------------------------------------
//


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::initialize()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::initialize
//
// !INTERFACE:
void PIO_Handler::initialize (
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  int comp_rank,                        // (in)  - local PE rank
  MPI_Comm comp_comm,                   // (in)  - MPI communicator for IO
  int num_iotasks,                      // (in)  - Number of IO tasks
  int num_aggregator,                   // (in)  - MPI aggregator count
  int stride,                           // (in)  - IO task stride
  int rearr,                            // (in)  - rearrangement type
  int *base_p,                          // (in)  - base option (IO task offset)
  int *rc                               // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Create an active, initialized PIO instance.
//    PIO is initialized based on the input arguments. However, if a
//    compatible PIO iosystem is already initialized, then nothing is done.
//    This is a collective call. Input parameters are read on comp_rank=0,
//    values on other tasks are ignored. ALL PEs which will be participating
//    in future I/O calls with this instance must participate in the call.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;      // local return code
  int base;
  bool instanceFound = false;          // try to find a PIO sys to reuse
  pio_iosystem_desc_t instance = PIO_IOSYSTEM_DESC_NULL;
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;            // final return code
  }
  PRINTPOS;
  if (base_p != (int *)NULL) {
    base = *base_p;
  } else {
    base = 1;
  }

  try {  

#ifdef ESMFIO_DEBUG
    pio_cpp_setdebuglevel(3);
#else // ESMFIO_DEBUG
    pio_cpp_setdebuglevel(0);
#endif // ESMFIO_DEBUG
    if (!instanceFound) {
      PRINTMSG("Before pio_cpp_init_intracom, num_iotasks = " << num_iotasks);
      pio_cpp_init_intracom(comp_rank, comp_comm,
                            num_iotasks, num_aggregator,
                            stride, rearr, &instance, base);
      PRINTMSG("After pio_cpp_init_intracom, instance = " << instance);
      // If we get here, hopefully everything is OK.
      if (instance != PIO_IOSYSTEM_DESC_NULL) {
        // Set the error handling to return PIO errors
        // Just return error (error code may be different on different PEs).
//        pio_cpp_seterrorhandlingi(&instance, PIO_RETURN_ERROR);
        // Broadcast the error to all PEs (consistant error handling)
        pio_cpp_seterrorhandlingi(&instance, PIO_BCAST_ERROR);
        PRINTMSG("After pio_cpp_seterrorhandlingi");
        // Add the instance to the global list
        PIO_Handler::activePioInstances.push_back(instance);
        PRINTMSG("push_back");
        localrc = ESMF_SUCCESS;
      } else {
        // Something went wrong (this really shouldn't happen)
        localrc = ESMF_RC_INTNRL_BAD;
        ESMC_LogDefault.Write("Unknown error in pio_cpp_init_intracom",
                              ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      }
    }
  } catch (int catchrc) {
    // catch standard ESMF return code
    PRINTMSG("Exception: " << catchrc);
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return;
  } catch (...) {
    PRINTMSG("Unknown exception");
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, rc);
    return;
  }

  // return successfully
  if (rc != NULL) {
    *rc = localrc;
  }
} // PIO_Handler::initialize()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::initializeVM()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::initializeVM
//
// !INTERFACE:
int PIO_Handler::initializeVM (void
//
// !RETURN VALUE:
//
//    int return code
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Create an active, initialized PIO instance.
//    PIO is initialized based on defaults gleaned from the VM. However, if a
//    compatible PIO iosystem is already initialized, then nothing is done.
//    This is a collective call. Input parameters are read on comp_rank=0,
//    values on other tasks are ignored. ALL PEs which will be participating
//    in future I/O calls with this instance must participate in the call.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMF_RC_NOT_IMPL;              // return code
  bool instanceFound = false;             // try to find a PIO sys to reuse

  try {  
    if (!instanceFound) {
      ESMC_VM currentVM;            // our VM (for PET and MPI comm info)
      int localPet;
      int petCount;
      int peCount;
      currentVM = ESMC_VMGetCurrent(&rc);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, 
        &rc)) {
        return rc;
      }
      rc = ESMC_VMGet(currentVM, &localPet, &petCount, &peCount,
                      &communicator, (int *)NULL, (int *)NULL);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc)) {
        return rc;
      }
      // Figure out the inputs for the initialize call
      my_rank = localPet;
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
      num_iotasks = petCount;
      num_aggregators = 1;
      stride = 1;
      rearr = PIO_rearr_box;
      base = 0;
#else // defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
      num_iotasks = 1;
      num_aggregators = 1;
      stride = 1;
      rearr = PIO_rearr_box;
      base = 0;
#endif // defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)

      // Call the static function
      PIO_Handler::initialize(my_rank, communicator, num_iotasks,
                              num_aggregators, stride, rearr, &base, &rc);
      PRINTMSG("After initialize, rc = " << rc);
      if (ESMF_SUCCESS == rc) {
        PRINTMSG("Looking for active instance, size = " << activePioInstances.size());
        pioSystemDesc = PIO_Handler::activePioInstances.back();
        PRINTMSG("Fetched PIO system descriptor, " << (void *)pioSystemDesc);
      }
    }
  } catch (int catchrc) {
    // catch standard ESMF return code
    PRINTMSG("Exception: " << catchrc);
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  } catch (...) {
    PRINTMSG("Unknown exception");
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, &rc);
    return rc;
  }

  return rc;
} // PIO_Handler::initializeVM()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::finalize()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::finalize
//
// !INTERFACE:
void PIO_Handler::finalize (
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Tear down all active, initialized PIO instances.
//
//EOPI
//-----------------------------------------------------------------------------
  int error;
  int localrc = ESMF_SUCCESS;             // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTMSG("");
  try {
    // Close any open IO descriptors before turning off the instances
    PIO_IODescHandler::finalize();
  } catch(int lrc) {
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(lrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, rc);
    return;
  }
  try {
    // Now, close any open PIO instances
    while(!PIO_Handler::activePioInstances.empty()) {
      pio_iosystem_desc_t instance = PIO_Handler::activePioInstances.back();
      pio_cpp_finalize(&instance, &error);
      // Even if we have an error but log and keep going to try and shut
      // down other PIO instances
      CHECKPIOWARN(error, "Error shutting down PIO instance", localrc);
#ifdef ESMFIO_DEBUG
      static int ionum = 1;
      if (error != PIO_noerr) {
        int lrc;
        ESMC_VM currentVM;
        int localPet;
        int petCount;
        int peCount;
        MPI_Comm foo;
        currentVM = ESMC_VMGetCurrent(&lrc);
        lrc = ESMC_VMGet(currentVM, &localPet, &petCount, &peCount,
                         &foo, (int *)NULL, (int *)NULL);
        PRINTMSG(" (" << localPet << "): ionum = " << ionum << ", localrc = " << localrc);
      }
      ionum++;
#endif // ESMFIO_DEBUG
      PIO_Handler::activePioInstances.pop_back();
    }
  } catch(int lrc) {
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(lrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch(...) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, rc);
    return;
  }

  // return successfully
  if (rc != NULL) {
    *rc = localrc;
  }
} // PIO_Handler::finalize()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::isPioInitialized()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::isPioInitialized
//
// !INTERFACE:
ESMC_Logical PIO_Handler::isPioInitialized (void
//
// !RETURN VALUE:
//
//  ESMC_Logical ESMF_TRUE if PIO is initialized, ESMF_FALSE otherwise
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Indicate whether or not PIO has been initialized.
//    NB: This does not guarantee that the initialization is appropriate
//        for the desired I/O operations.
//
//EOPI
//-----------------------------------------------------------------------------
  if(activePioInstances.empty()) {
    return ESMF_FALSE;
  } else {
    return ESMF_TRUE;
  }
} // PIO_Handler::isPioInitialized()
//-----------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//
// constructors and destruct()
//
//-------------------------------------------------------------------------
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::PIO_Handler()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::PIO_Handler    - constructor
//
// !INTERFACE:
PIO_Handler::PIO_Handler(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  ESMC_IOFmt_Flag fmtArg,                 // (in)  - File format for PIO to use
  int *rc                                 // (out) - Error return code
  ) : IO_Handler(fmtArg) {
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMCI::PIO_Handler
//    object.
//    No error checking wrt consistency of input arguments is needed because
//    the PIO_Handler constructor is only to be called by IO_Handler::create()
//    interfaces which are responsible for providing consistent arguments
//    to this layer.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  try {  

    // fill in the PIO_Handler object
    pioSystemDesc =  PIO_IOSYSTEM_DESC_NULL;
    pioFileDesc = (pio_file_desc_t)NULL;
    pioIODesc = (pio_io_desc_t)NULL;
    user_count = 0;
    localrc = ESMF_SUCCESS;
    new_file = false;
    // Get the rest from initialize
    localrc = initializeVM();
  
  } catch (int lrc) {
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(lrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch (...) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, rc);
    return;
  }
  
  // return successfully
  if (rc != NULL) {
    *rc = localrc;
  }
} // PIO_Handler::PIO_Handler()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::destruct()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::destruct    - tear down PIO handler
//
// !INTERFACE:
void PIO_Handler::destruct (void
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Close any open files and recover resources.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc; // Only for debug!
  PRINTPOS;
  // Make sure the file is closed
  if (isOpen() == ESMF_TRUE) {
    PRINTMSG(" (" << my_rank << "): closing file");
    close((int *)NULL);     // Don't care about an error, continue with cleanup
  }
  // Kill the file descriptor
  if ((pio_file_desc_t)NULL != pioFileDesc) {
    PRINTMSG(" (" << my_rank << "): killing PIO file descriptor");
    free(pioFileDesc);
    pioFileDesc = (pio_file_desc_t)NULL;
  }
  if ((pio_io_desc_t)NULL != pioIODesc) {
    PRINTMSG(" \"killing\" pioIODesc");
    PIO_IODescHandler::freePioDecomp(&pioIODesc);
    pioIODesc = (pio_io_desc_t)NULL;
  }

  // kill the pointer to the PIO_Handler object
  // NB: This does not shutdown the PIO instance, it may be reused.
  pioSystemDesc =  PIO_IOSYSTEM_DESC_NULL;
} // PIO_Handler::destruct()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::arrayRead()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::arrayRead    - Read an array from a file
//
// !INTERFACE:
void PIO_Handler::arrayRead(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  Array *arr_p,                           // (inout) - Destination of read
  const char * const name,                // (in)    - Optional array name
  int *timeslice,                         // (in)    - Optional timeslice
  int *rc                                 // (out)   - Error return code
  ) {
//
// !DESCRIPTION:
//    Read data from field <name> from the open file. If timeslice is not
//    NULL, it should point to an integer representing the timeslice to read
//    from the Array.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  bool statusOK;                          // true if OK to continue
  int * ioDims;                           // Array IO shape
  int nioDims;                            // Array IO rank
  int * arrDims;                          // Array shape
  int narrDims;                           // Array rank
  pio_io_desc_t iodesc;                   // PIO IO descriptor
  pio_var_desc_t vardesc = NULL;          // PIO variable descriptor
  int basepiotype;                        // PIO version of Array data type
  void *baseAddress;                      // The address of the Array IO data
  int localDE;                            // DE to use for IO
  int nVar;                               // Number of variables in file
  int nAtt;                               // Number of attributes in file
  int unlim;                              // Unlimited dimension ID
  const char *varname;                    // Default variable name

  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;
  // File open?
  statusOK = (isOpen() == ESMF_TRUE);
  if (statusOK) {
    iodesc = getIODesc(pioSystemDesc, arr_p, &ioDims, &nioDims,
                       &arrDims, &narrDims, &basepiotype, &localrc);
    statusOK = !ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc);
  } else {
    ESMC_LogDefault.Write("file not open", ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
  }
  if (statusOK) {
    vardesc = (pio_var_desc_t)calloc(PIO_SIZE_VAR_DESC, 1);
    if ((pio_var_desc_t)NULL == vardesc) {
      ESMC_LogDefault.Write(" failed to allocate pio variable desc",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      statusOK = false;
      vardesc = (pio_var_desc_t)NULL;
    }
  }
  if (statusOK) {
    // Get a pointer to the array data
    // Still have the one DE restriction so use localDE = 0
    localDE = 0;
    baseAddress = arr_p->getLocalarrayList()[localDE]->getBaseAddr();
  }
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN)) {
    int nDims;
    if (((char *)NULL != name) && (strlen(name) > 0)) {
      varname = name;
    } else {
      varname = arr_p->getName();
    }
    PRINTMSG(" (" << my_rank << "): varname = " << varname);
    // Make sure the variable is in the file
    localrc = pio_cpp_inquire(pioFileDesc, &nDims,
                              &nVar, &nAtt, &unlim);
      // This should work if it is a NetCDF file.
    statusOK = CHECKPIOERROR(localrc, "File is not in NetCDF format",
                             localrc);
  }
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN)) {
    localrc = pio_cpp_inq_varid_vdesc(pioFileDesc, varname, vardesc);
    // An error here means the variable is not in the file
    statusOK = CHECKPIOERROR(localrc, "variable not found in file",
                             localrc);
  }
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN)) {
    int frame;
    if (((int *)NULL != timeslice) && (*timeslice > 0)) {
      int dimid_time;
      int time_len;
      localrc = pio_cpp_inq_dimid(pioFileDesc, "time", &dimid_time);
      statusOK = CHECKPIOERROR(localrc, "No time dimension found in file",
                               localrc);
      // Check to see if time is the unlimited dimension
      if (statusOK && (dimid_time != unlim)) {
        PRINTMSG(" Time dimension = " << dimid_time <<
                 ", unlimited dim = " << unlim);
         ESMC_LogDefault.Write(" Time is not the file's unlimited dimension",
                              ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
        statusOK = false;
      }
      if (statusOK) {
        // Check to make sure the requested record is in the file
        localrc = pio_cpp_inq_dimlen(pioFileDesc,
                                     dimid_time, &time_len);
        statusOK = CHECKPIOERROR(localrc,
                                 "Error finding time length", localrc);
      }
      if (statusOK && (*timeslice > time_len)) {
        PRINTMSG(" (" << my_rank << "): " <<
                 "Timeframe is greater than that in file" <<
                 getFilename() << ", file time = " << time_len <<
                 ", requested record = " << *timeslice);
        ESMC_LogDefault.Write("Timeframe is greater than max in file",
                              ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
        statusOK = false;
      }
      frame = *timeslice;
    } else {
      frame = 1;
    }
    if (statusOK && (unlim >= 0)) {
      pio_cpp_setframe(vardesc, frame);
    }
  }
#endif // defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
#ifdef ESMFIO_DEBUG
  pio_cpp_setdebuglevel(0);
#endif // ESMFIO_DEBUG

  PRINTMSG("calling read_darray, status = " << statusOK <<
           ", pio type = " << basepiotype << ", address = " << baseAddress);
  if (statusOK) {
    // Read in the array
    switch(basepiotype) {
    case PIO_int:
      pio_cpp_read_darray_int(pioFileDesc, vardesc, iodesc,
                              (int *)baseAddress,
                              arrDims, narrDims, &localrc);
      break;
    case PIO_real:
      pio_cpp_read_darray_real(pioFileDesc, vardesc, iodesc,
                               (float *)baseAddress,
                               arrDims, narrDims, &localrc);
      break;
    case PIO_double:
      pio_cpp_read_darray_double(pioFileDesc, vardesc, iodesc,
                                 (double *)baseAddress,
                                 arrDims, narrDims, &localrc);
      break;
    default:
      localrc = ESMF_RC_INTNRL_BAD;
      ESMC_LogDefault.Write("Bad PIO IO type", ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    }
  }

  // Cleanup
  if ((pio_var_desc_t)NULL != vardesc) {
    free(vardesc);
    vardesc = (pio_var_desc_t)NULL;
  }

  // return
  if (rc != NULL) {
    *rc = localrc;
  }
} // PIO_Handler::arrayRead()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::arrayWrite()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::arrayWrite    - Write an Array to a file
//
// !INTERFACE:
void PIO_Handler::arrayWrite(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
  Array *arr_p,                           // (in) Destination of write
  const char * const name,                // (in) Optional array name
  int *timeslice,                         // (in) Optional timeslice
  int *rc                                 // (out) - Error return code
//
  ) {
//
// !DESCRIPTION:
//    Call the appropriate PIO write_darray_<rank>_<typekind> function
//    It is an error if this handler object does not have an open 
//    PIO file descriptor and a valid PIO IO descriptor (these items should
//    all be in place after a successful call to PIO_Handler::open).
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  bool statusOK;                          // true if OK to continue
  int * ioDims;                           // Array IO shape
  int nioDims;                            // Array IO rank
  int * arrDims;                          // Array shape
  int narrDims;                           // Array rank
  pio_io_desc_t iodesc;                   // PIO IO descriptor
  pio_var_desc_t vardesc = NULL;          // PIO variable descriptor
  int basepiotype;                        // PIO version of Array data type
  void *baseAddress;                      // The address of the Array IO data
  int localDE;                            // DE to use for IO
  int ncDims[8];                          // To hold NetCDF dimensions
  int unlim;                              // Unlimited dimension ID
  int timeFrame = -1;                     // ID of time dimension (>0 if used)
  int timesliceVal = -1;                  // Used time value (from timeslice)
  bool varExists = false;                 // true if varname is defined in file
  const char *varname;                    // Variable name
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;
  if ((int *)NULL != timeslice) {
    timesliceVal = *timeslice;
  }
  // File open?
  statusOK = (isOpen() == ESMF_TRUE);
  if (statusOK) {
    iodesc = getIODesc(pioSystemDesc, arr_p, &ioDims, &nioDims,
                       &arrDims, &narrDims, &basepiotype, &localrc);
    statusOK = !ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc);
  } else {
    ESMC_LogDefault.Write("file not open", ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
  }
  if (statusOK) {
    vardesc = (pio_var_desc_t)calloc(PIO_SIZE_VAR_DESC, 1);
    if ((pio_var_desc_t)NULL == vardesc) {
      ESMC_LogDefault.Write(" failed to allocate pio variable desc",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      statusOK = false;
      vardesc = (pio_var_desc_t)NULL;
    }
  }
  if (statusOK) {
    // Get a pointer to the array data
    // Still have the one DE restriction so use localDE = 0
    localDE = 0;
    baseAddress = arr_p->getLocalarrayList()[localDE]->getBaseAddr();
    PRINTMSG("baseAddress = 0x" << (void *)baseAddress);
  }
#ifdef ESMFIO_DEBUG
  if (!statusOK) {
    PRINTMSG("Bad status, can't write");
  }
#endif // ESMFIO_DEBUG
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN)) {
    // Define the variable name and check the file
    if (((char *)NULL != name) && (strlen(name) > 0)) {
      varname = name;
    } else {
      varname = arr_p->getName();
    }
    PRINTMSG("varname = \"" << varname << "\"");
    if (ESMF_TRUE == isNewFile()) {
      varExists = false;
    } else {
      int nVar;                           // Number of variables in file
      int nAtt;                           // Number of attributes in file
      int nfDims;                         // Number of dimensions in file
      PRINTMSG("Entering NetCDF define mode (redef)");
      localrc = pio_cpp_redef(pioFileDesc);
      // Not all NetCDF errors are really errors here so we need to check
      if ((PIO_noerr != localrc) && (NC_EINDEFINE != localrc)) {
        statusOK = CHECKPIOERROR(localrc,
                                 ((NC_EPERM == localrc) ?
                                  "File is read only" :
                                  "File is not in NetCDF format"),
                                 localrc);
      }
      if (statusOK) {
        // This should work if it is a NetCDF file.
        PRINTMSG("Calling pio_cpp_inquire");
        localrc = pio_cpp_inquire(pioFileDesc, &nfDims,
                                  &nVar, &nAtt, &unlim);
        statusOK = CHECKPIOERROR(localrc, "File is not in NetCDF format",
                                 localrc);
      }
      if (statusOK) {
        // We have a NetCDF file, see if the variable is in there
        PRINTMSG("Looking for variable in file");
        localrc = pio_cpp_inq_varid_vdesc(pioFileDesc, varname, vardesc);
        // This should succeed if the variable exists
        varExists = (PIO_noerr == localrc);
      }
    }
  }
  // Check consistency of time dimension with timeslice
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN)) {
    bool hasTimeDim;
    int dimidTime;
    int timeLen;
    PRINTMSG("Checking time dimension");
    localrc = pio_cpp_inq_dimid(pioFileDesc, "time", &dimidTime);
    // NetCDF does not specify which error code goes with with
    // condition so we will guess that there is no time dimension 
    // on any error condition (This may be an error depending on context).
    hasTimeDim = (PIO_noerr == localrc);
    PRINTMSG("status = " << statusOK << ", hasTimeDim = " << hasTimeDim);
    if (statusOK && hasTimeDim) {
      // Retrieve the max time field
      localrc = pio_cpp_inq_dimlen(pioFileDesc, dimidTime, &timeLen);
      statusOK = CHECKPIOERROR(localrc,
                               "Error retrieving information about time",
                               localrc);
    }
    if (statusOK && hasTimeDim) {
      // Check to make sure that time is the unlimited dimension
      if (dimidTime != unlim) {
        ESMC_LogDefault.Write("Time is not the file's unlimited dimension",
                              ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
        statusOK = false;
      }
    }
    if (statusOK && (timesliceVal > 0)) {
      if (varExists & !hasTimeDim) {
        // It is an error to not have a time dimension if we are trying to
        // write a timeslice and the variable already exists
        // (it won't have time)
        localrc = ESMF_RC_FILE_WRITE;
        statusOK = false;
        ESMC_LogDefault.Write("Field already exists without time dimension",
                              ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      } else {
        timeFrame = timesliceVal;
      }
    } else if (statusOK && (timesliceVal < 0)) {
      // Special case for no timeslice input passed but time dimension exists
      if (varExists && hasTimeDim) {
        timeFrame = timeLen + 1;
        PRINTMSG("timeslice not passed but set to " << timeFrame);
      }
    }
  }
  PRINTMSG("ready to check var compat., status = " <<
           statusOK << ", timeFrame = " << timeFrame);
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN) && varExists) {
    int nDims;
    int nArrdims = nioDims + ((timeFrame > 0) ? 1 : 0);
    int *dimIds = (int *)NULL;
    // Check compatibility between array to write and existing variable.
    localrc = pio_cpp_inq_varndims_vdesc(pioFileDesc, vardesc, &nDims);
    statusOK = CHECKPIOERROR(localrc,
                             "Error retrieving information about variable",
                             localrc);
    if (statusOK && (nDims != nArrdims)) {
      localrc = ESMF_RC_FILE_UNEXPECTED;;
      ESMC_LogDefault.Write("Variable rank in file does not match array",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      PRINTMSG("Variable rank mismatch: nioDims = " << nioDims <<
               ", nArrdims = " << nArrdims << ", nDims = " << nDims);
      statusOK = false;
    }
    if (statusOK) {
      dimIds = (int *)malloc(nDims * sizeof(int));
      if ((int *)NULL == dimIds) {
        localrc = ESMF_RC_MEM_ALLOCATE;
        ESMC_LogDefault.Write("Error allocating memory",
                              ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
        statusOK = false;
      }
    }
    if (statusOK) {
      PRINTMSG("Calling pio_cpp_inq_vardimid_vdesc");
      localrc = pio_cpp_inq_vardimid_vdesc(pioFileDesc, vardesc,
                                           dimIds, nDims);
      statusOK = CHECKPIOERROR(localrc,
                               "Error retrieving information about variable",
                               localrc);
      if (statusOK) {
        int dimLen;
        int ioDimNum = 0;
        for (int i = 0; i < nDims; i++) {
          PRINTMSG("pio_cpp_inq_dimlen for dim = " << i);
          localrc = pio_cpp_inq_dimlen(pioFileDesc, dimIds[i], &dimLen);
          statusOK = CHECKPIOERROR(localrc,
                                   "Error retrieving dimension information",
                                   localrc);
          if (!statusOK) {
            break;
          }
          if (dimIds[i] == unlim) {
            if (timeFrame <= 0) {
              localrc = ESMF_RC_FILE_UNEXPECTED;
              ESMC_LogDefault.Write("File variable requires time dimension",
                                    ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
              statusOK = false;
            } else if ((timeFrame <= dimLen) && !overwriteFields()) {
              // This 'error' might be incorrect in that we can't figure
              // out the max frame of this variable, only for the whole file.
              localrc = ESMF_RC_FILE_UNEXPECTED;
              ESMC_LogDefault.Write("Can't overwrite timeslice",
                                    ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
              statusOK = false;
            }
          } else if (dimLen != ioDims[ioDimNum]) {
            localrc = ESMF_RC_FILE_UNEXPECTED;
            ESMC_LogDefault.Write("Variable dimension in file does "
                                  "not match array",
                                  ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
            statusOK = false;
            break;
          } else {
            ioDimNum++;
          }
        }
      }
    }
    if ((int *)NULL != dimIds) {
      free(dimIds);
      dimIds = (int *)NULL;
    }
  }
  if (statusOK && varExists && !overwriteFields() && (timeFrame <= 0)) {
    // Check to see if we can overwrite an existing field or timeslice
    localrc = ESMF_RC_FILE_WRITE;
    statusOK = false;
    ESMC_LogDefault.Write("Field exists, however, overwrite is .false.",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
  }

  if (statusOK && (getFormat() != ESMF_IOFMT_BIN) && !varExists) {
    // Ensure we are in define mode
    PRINTMSG("Going into NetCDF define mode (redef)");
    localrc = pio_cpp_redef(pioFileDesc);
    // Not all NetCDF errors are really errors here so we need to check
    if ((PIO_noerr != localrc) && (NC_EINDEFINE != localrc)) {
      statusOK = CHECKPIOERROR(localrc,
                               ((NC_EPERM == localrc) ?
                                "File is read only" :
                                "File is not in NetCDF format"),
                               localrc);
    }
  }
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN) && !varExists) {
    // Create the variable
    for (int i = 0; i < nioDims; i++) {
      char axis[128];
      sprintf(axis, "%s_dim%03d", varname, (i + 1));
      PRINTMSG("Defining dimension " << i);
      localrc = pio_cpp_def_dim(pioFileDesc, axis,
                                ioDims[i], &ncDims[i]);
      if (PIO_noerr != localrc) {
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          rc);
        statusOK = false;
        break;
      }
    }
    PRINTMSG("finished defining space dims, status = " <<
             statusOK << ", timeFrame = " << timeFrame);
    if (statusOK && (timeFrame > -1)) {
      PRINTMSG("Defining time dinension");
      localrc = pio_cpp_def_dim(pioFileDesc, "time",
                                PIO_unlimited, &ncDims[nioDims]);
      if (PIO_noerr != localrc) {
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          rc);
        statusOK = false;
      }
      nioDims++;
    }
  }
  PRINTMSG("status = " << statusOK << ", varExists = " << varExists);
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN) && !varExists) {
    localrc = pio_cpp_def_var_md(pioFileDesc, varname, basepiotype,
                                 ncDims, nioDims, vardesc);
    if (PIO_noerr != localrc) {
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      statusOK = false;
    }
  }
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN) && (timeFrame >= 0)) {
#ifdef ESMFIO_DEBUG
    int nvdims;
    pio_cpp_inq_varndims_vdesc(pioFileDesc, vardesc, &nvdims);
    PRINTMSG("calling setframe, ndims = " << nvdims);
#endif // ESMFIO_DEBUG
    pio_cpp_setframe(vardesc, timeFrame);
  }
#ifdef ESMFIO_DEBUG
  else if (getFormat() != ESMF_IOFMT_BIN) {
    PRINTMSG("NOT calling setframe, status = " <<
             statusOK << ", timeFrame = " << timeFrame);
  }
  if (getFormat() != ESMF_IOFMT_BIN) {
    if (varExists) {
      int varid;
      int lrc;
      lrc = pio_cpp_inq_varid_vid(pioFileDesc, varname, &varid);
      PRINTMSG("varid = " << varid);
    }
  }
#endif // ESMFIO_DEBUG
  PRINTMSG("calling enddef, status = " << statusOK);
  if (statusOK && (getFormat() != ESMF_IOFMT_BIN)) {
    localrc = pio_cpp_enddef(pioFileDesc);
    if (PIO_noerr != localrc) {
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      statusOK = false;
    }
  }
#endif // defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
  PRINTMSG("calling write_darray, status = " << statusOK <<
           ", pio type = " << basepiotype << ", address = " << baseAddress);
#ifdef ESMFIO_DEBUG
  pio_cpp_setdebuglevel(0);
#endif // ESMFIO_DEBUG
  if (statusOK) {
    // Write the array
    switch(basepiotype) {
    case PIO_int:
      pio_cpp_write_darray_int(pioFileDesc, vardesc, iodesc,
                               (int *)baseAddress,
                               arrDims, narrDims, &localrc);
      break;
    case PIO_real:
      pio_cpp_write_darray_real(pioFileDesc, vardesc, iodesc,
                                (float *)baseAddress,
                                arrDims, narrDims, &localrc);
      break;
    case PIO_double:
      pio_cpp_write_darray_double(pioFileDesc, vardesc, iodesc,
                                  (double *)baseAddress,
                                  arrDims, narrDims, &localrc);
      break;
    default:
      localrc = ESMF_RC_INTNRL_BAD;
      PRINTMSG(" Attempt to write basepiotype = " << basepiotype);
      ESMC_LogDefault.Write("Bad PIO IO type", ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }
  }
  PRINTMSG("calling cleanup, status = " << statusOK);

  // Cleanup
  if ((pio_var_desc_t)NULL != vardesc) {
    free(vardesc);
    vardesc = (pio_var_desc_t)NULL;
  }

  // return
  if (rc != NULL) {
    *rc = localrc;
  }
} // PIO_Handler::arrayWrite()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::open()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::open    - open a stream with stored filename
//
// !INTERFACE:
void PIO_Handler::open(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  bool readonly,                       // (in)  - if false, then read/write
  int *rc                              // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Open a file for reading and/or writing.
//    PIO must be initialized for this routine to succeed (ESMF_RC_INTNRL_BAD)
//    It is an error if a file is already open (ESMF_RC_FILE_OPEN)
//
//EOPI
//-----------------------------------------------------------------------------
  int iotype;                             // PIO I/O type
  int mode;                               // PIO file open mode
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;
  if (isPioInitialized() != ESMF_TRUE) {
    localrc = ESMF_RC_INTNRL_BAD;
    ESMC_LogDefault.Write("PIO not initialized",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    if (rc != NULL) {
      *rc = localrc;
    }
    return;
  } else if (isOpen() == ESMF_TRUE) {
    localrc = ESMF_RC_FILE_OPEN;
    ESMC_LogDefault.Write("file already open", ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    if (rc != NULL) {
      *rc = localrc;
    }
    return;
  } else if (PIO_IOSYSTEM_DESC_NULL == pioSystemDesc) {
    // Just grab last created PIO instance for now (TBD: need way to choose)
    pioSystemDesc = PIO_Handler::activePioInstances.back();
  }

  // Allocate a file descriptor
  try {
    pioFileDesc = (pio_file_desc_t)calloc(PIO_SIZE_FILE_DESC, 1);
    PRINTMSG(" allocated pio file desc, addr = " << (void *)pioFileDesc);
    if ((pio_file_desc_t)NULL == pioFileDesc) {
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);
      return;
    }
  } catch(...) {
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, rc);
    return;
  }

  // Translate the I/O format from ESMF to PIO
  localrc = ESMF_SUCCESS;
  if (getFormat() == ESMF_IOFMT_BIN) {
    iotype = PIO_iotype_pbinary;
  } else if (getFormat() == ESMF_IOFMT_NETCDF) {
#ifdef ESMF_PNETCDF
    iotype = PIO_iotype_pnetcdf;
#elif ESMF_NETCDF
    iotype = PIO_iotype_netcdf;
#else  // ESMF_NETCDF
    localrc = ESMF_RC_LIB_NOT_PRESENT;
#endif // ESMF_NETCDF
  } else if (getFormat() == ESMF_IOFMT_NETCDF4C) {
#ifdef ESMF_NETCDF
    iotype = PIO_iotype_netcdf4c;
#else  // ESMF_NETCDF
    localrc = ESMF_RC_LIB_NOT_PRESENT;
#endif // ESMF_NETCDF
  } else if (getFormat() == ESMF_IOFMT_NETCDF4P) {
#ifdef ESMF_NETCDF
    iotype = PIO_iotype_netcdf4p;
#else  // ESMF_NETCDF
    localrc = ESMF_RC_LIB_NOT_PRESENT;
#endif // ESMF_NETCDF
  } else {
    localrc = ESMF_RC_ARG_BAD;
  }
  if (ESMF_SUCCESS != localrc) {
    switch(localrc) {
    case ESMF_RC_ARG_BAD:
      ESMC_LogDefault.Write("unknown I/O format",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      break;
    case ESMF_RC_LIB_NOT_PRESENT:
      ESMC_LogDefault.Write("Library for requested I/O format is not present",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      break;
    default:
      ESMC_LogDefault.Write("Unknown I/O error",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      break;
    }
    if (rc != NULL) {
      *rc = localrc;
    }
    free (pioFileDesc);
    pioFileDesc = NULL;
    return;
  }
  
  // Check to see if we are able to open file properly
  bool okToCreate = false;
  int clobberMode = PIO_NOCLOBBER;
  if (readonly) {
    mode = PIO_NOWRITE;
  }  else {
    mode = PIO_WRITE;
  }
  // Figure out if we need to call createfile or openfile
  new_file = false;
  switch(getFileStatusFlag()) {
  case ESMC_FILESTATUS_UNKNOWN:
    if (IO_Handler::fileExists(getFilename(), !readonly)) {
      // Treat like OLD
      okToCreate = false;
    } else {
      // Treat like NEW
      okToCreate = true;
      clobberMode = PIO_NOCLOBBER;
    }
    break;
  case ESMC_FILESTATUS_OLD:
    okToCreate = false;
    break;
  case ESMC_FILESTATUS_NEW:
    okToCreate = true;
    clobberMode = PIO_NOCLOBBER;
    break;
  case ESMC_FILESTATUS_REPLACE:
    okToCreate = true;
    clobberMode = PIO_CLOBBER;
    break;
  default:
    localrc = ESMF_RC_ARG_BAD;
    ESMC_LogDefault.Write("unknown file status argument",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    if (rc != NULL) {
      *rc = localrc;
    }
    free (pioFileDesc);
    pioFileDesc = NULL;
    return;
  }
  if (okToCreate) {
    // Looks like we are ready to try and create the file
#ifdef ESMFIO_DEBUG
    char errmsg[512];
    pio_cpp_setdebuglevel(3);
    sprintf(errmsg, " calling pio_cpp_createfile: file = \"");
    strncpy((errmsg + strlen(errmsg)), getFilename(), 256);
    strcpy((errmsg + strlen(errmsg)), "\"");
    ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
#endif // ESMFIO_DEBUG
    localrc = pio_cpp_createfile(&pioSystemDesc, pioFileDesc,
                                 iotype, getFilename(), clobberMode);
    if (!CHECKPIOWARN(localrc, "Unable to create file", (*rc))) {
      free (pioFileDesc);
      pioFileDesc = NULL;
      return;
    } else {
      new_file = true;
      PRINTMSG("call to pio_cpp_createfile: success for " << getFilename());
    }
#ifdef ESMFIO_DEBUG
    pio_cpp_setdebuglevel(0);
#endif // ESMFIO_DEBUG
  } else {
    PRINTMSG(" calling pio_cpp_openfile with mode = " << mode <<
             ", file = \"" << getFilename() << "\"");
    // Looks like we are ready to go
    localrc = pio_cpp_openfile(&pioSystemDesc, pioFileDesc,
                               iotype, getFilename(), mode);
    PRINTMSG(", called pio_cpp_openfile on " << getFilename());
    if (!CHECKPIOWARN(localrc, "Unable to open file", (*rc))) {
      free (pioFileDesc);
      pioFileDesc = NULL;
      return;
    }
  }

  // return successfully
  if (rc != NULL) {
    *rc = ESMF_SUCCESS;
  }
} // PIO_Handler::open()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::isOpen()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::isOpen    - Determine is a file is open
//
// !INTERFACE:
ESMC_Logical PIO_Handler::isOpen(
//
// !RETURN VALUE:
//
//  ESMC_Logical ESMF_TRUE if a file is open, ESMF_FALSE otherwise
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Determine if a file is open returning ESMF_TRUE if a file is open,
//    ESMF_FALSE otherwise
//
//EOPI
//-----------------------------------------------------------------------------
  PRINTPOS;
  if ((pio_file_desc_t)NULL == pioFileDesc) {
    PRINTMSG("pioFileDesc is NULL");
    return ESMF_FALSE;
  } else if (pio_cpp_file_is_open(pioFileDesc) != 0) {
    PRINTMSG("File is open");
    return ESMF_TRUE;
  } else {
    // This really should not happen, warn and clean up just in case
    char errmsg[32 + ESMF_MAXSTR];
    sprintf(errmsg, "File, %s, closed by PIO", getFilename());
    ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    free(pioFileDesc);
    pioFileDesc = (pio_file_desc_t)NULL;
    return ESMF_FALSE;
  }
} // PIO_Handler::isOpen()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::flush()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::flush    - Flush any pending I/O operations
//
// !INTERFACE:
void PIO_Handler::flush(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    xxx
//
//EOPI
//-----------------------------------------------------------------------------

  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }
  
  PRINTPOS;
  // Not open? No problem, just skip
  if (isOpen() == ESMF_TRUE) {
    pio_cpp_syncfile(pioFileDesc);
  }
  // return successfully
  if (rc != NULL) {
    *rc = ESMF_SUCCESS;
  }
} // PIO_Handler::flush()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::close()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::close    - Close an open file
//
// !INTERFACE:
void PIO_Handler::close(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Close the open file (if any).
//    It is not an error if no file is open
//
//EOPI
//-----------------------------------------------------------------------------

  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }
  
  PRINTPOS;
  // Not open? No problem, just skip
  if (isOpen() == ESMF_TRUE) {
    pio_cpp_closefile(pioFileDesc);
    free(pioFileDesc);
    pioFileDesc = (pio_file_desc_t)NULL;
    new_file = false;
  }

  // return successfully
  if (rc != NULL) {
    *rc = ESMF_SUCCESS;
  }
} // PIO_Handler::close()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::getIODesc()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::getIODesc - Find or create an IO descriptor
//
// !INTERFACE:
  pio_io_desc_t PIO_Handler::getIODesc(
//
// !RETURN VALUE:
//
//    pio_io_desc_t PIO IO descriptor
//
// !ARGUMENTS:
//
  pio_iosystem_desc_t iosys,          // (in)  - PIO system handle to use
  Array *arr_p,                       // (in)  - Array for IO decompomposition
  int ** ioDims,                      // (out) - Array shape for IO
  int *nioDims,                       // (out) - Rank of Array IO
  int ** arrDims,                     // (out) - Array shape for IO
  int *narrDims,                      // (out) - Rank of Array IO
  int *basepiotype,                   // (out) - Data type for IO
  int *rc                             // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Find or create an appropriate PIO I/O Descriptor and return it.
//
//EOPI
//-----------------------------------------------------------------------------
  pio_io_desc_t new_io_desc = (pio_io_desc_t)NULL;
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }
  
  PRINTPOS;
  new_io_desc = PIO_IODescHandler::getIODesc(iosys, arr_p, &localrc);
  if ((pio_io_desc_t)NULL == new_io_desc) {
    PRINTMSG("calling constructPioDecomp");
    localrc = PIO_IODescHandler::constructPioDecomp(iosys,
                                                    arr_p, &new_io_desc);
    PRINTMSG("constructPioDecomp call complete" << ", localrc = " << localrc);
  }
  if ((ioDims != (int **)NULL) || (nioDims != (int *)NULL) ||
      (arrDims != (int **)NULL) || (narrDims != (int *)NULL)) {
    int niodimArg;
    int *iodimsArg;
    int narrdimArg;
    int *arrdimsArg;
    localrc = PIO_IODescHandler::getDims(new_io_desc, &niodimArg, &iodimsArg,
                                         &narrdimArg, &arrdimsArg);
    if (!ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) {
      if (ioDims != (int **)NULL) {
        *ioDims = iodimsArg;
      }
      if (nioDims != (int *)NULL) {
        *nioDims = niodimArg;
      }
      if (arrDims != (int **)NULL) {
        *arrDims = arrdimsArg;
      }
      if (narrDims != (int *)NULL) {
        *narrDims = narrdimArg;
      }
    }
  }
  PRINTMSG("getDims complete, calling getIOType");
  if (basepiotype != (int *)NULL) {
    *basepiotype = PIO_IODescHandler::getIOType(new_io_desc, &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
  }
  if (rc != NULL) {
    *rc = localrc;
  }
  return new_io_desc;
} // PIO_Handler::getIODesc()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_Handler::CheckPIOError()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::CheckPIOError
//
// !INTERFACE:
bool PIO_Handler::CheckPIOError(
//
// !RETURN VALUE:
//
//    bool true if it is OK to continue (false = error condition)
//
// !ARGUMENTS:
//
  int pioRetCode,                        // (in)  - Return code to check
  int line,                              // (in)  - Line containing error
  const char * const file,               // (in)  - File containing error
  const char * const method,             // (in)  - ESMC_METHOD
  const char *const fmtStr,              // (in)  - Optional error string
  int *rc,                               // (out) - Error return code
  bool warn                              // (in)  - warn instead of error
  ) {
//
// !DESCRIPTION:
//    Log an error (if an error condition is indicated by pioRetCode)
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc = ESMF_SUCCESS;;
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  if (pioRetCode != PIO_noerr) {
    char errmsg[512];
    // Log the error
    if ((char *)NULL != fmtStr) {
      sprintf(errmsg, " %s, (PIO error = %d)", fmtStr, pioRetCode);
    } else {
      sprintf(errmsg, " (PIO error = %d)", pioRetCode);
    }
    if (warn) {
      ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_WARN, line, file, method);
    } else {
      ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_ERROR, line, file, method);
    }
    PRINTMSG("PIO ERROR: " << errmsg);
    // Attempt to find a corresponding ESMC error code
    switch(pioRetCode) {
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
    case NC_EEXIST:
      localrc = ESMF_RC_FILE_CREATE;
      break;
    case NC_EPERM:
      localrc = ESMF_RC_FILE_OPEN;
      break;
#endif // defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
    default:
      localrc = ESMF_RC_LIB;
      break;
    }
  }
  // Set the return code
  if (rc != NULL) {
    *rc = localrc;
  }
  return (pioRetCode == PIO_noerr);
} // PIO_Handler::CheckPIOError()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_IODescHandler::~PIO_IODescHandler()"
//BOPI
// !IROUTINE:  ESMCI::PIO_IODescHandler::~PIO_IODescHandler
//
// !INTERFACE:
PIO_IODescHandler::~PIO_IODescHandler (
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Tear down active, initialized PIO IO Descriptor and free memory
//
//EOPI
//-----------------------------------------------------------------------------
  pio_cpp_freedecomp_ios(&ios, io_descriptor);
  free(io_descriptor);
  io_descriptor = (pio_io_desc_t)NULL;
  if (dims != (int *)NULL) {
    delete[] dims;
    dims = (int *)NULL;
  }
  if (arrayShape != (int *)NULL) {
    delete[] arrayShape;
    arrayShape = (int *)NULL;
  }
  array_p = (Array *)NULL;
} // PIO_IODescHandler::~PIO_IODescHandler()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_IODescHandler::finalize()"
//BOPI
// !IROUTINE:  ESMCI::PIO_IODescHandler::finalize
//
// !INTERFACE:
void PIO_IODescHandler::finalize (
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Tear down all active, initialized PIO IO Descriptors.
//
//EOPI
//-----------------------------------------------------------------------------
  PIO_IODescHandler *handle;

  while(!PIO_IODescHandler::activePioIoDescriptors.empty()) {
    handle = PIO_IODescHandler::activePioIoDescriptors.back();
    delete handle; // Shuts down descriptor
    handle = (PIO_IODescHandler *)NULL;
    PIO_IODescHandler::activePioIoDescriptors.pop_back();
  }
} // PIO_IODescHandler::finalize()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_IODescHandler::constructPioDecomp()"
//BOPI
// !IROUTINE:  ESMCI::PIO_IODescHandler::constructPioDecomp - New Decomposition
//
// !INTERFACE:
int PIO_IODescHandler::constructPioDecomp(
//
// !RETURN VALUE:
//
//    int return code
//
// !ARGUMENTS:
//
  pio_iosystem_desc_t iosys,          // (in)  - PIO system handle to use
  Array *arr_p,                       // (in)  - Array for IO decompomposition
  pio_io_desc_t *newDecomp_p          // (out) - New decomposition descriptor
  ) {
//
// !DESCRIPTION:
//    Gather the necessary information the input array and call PIO_initdecomp.
//    The result is a new decomposition descriptor which is used in the
//     PIO read/write calls.
//
//EOPI
//-----------------------------------------------------------------------------

  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;   // local return code
  ESMC_VM currentVM;                // our VM (for PET and MPI comm info)
  MPI_Comm mpiCommunicator;         // MPI communicator to use for I/O
  int localDe;                      // The DE being processed
  int localDeCount;                 // The number of DEs on this PET
  int localPet;                     // Number of this PET
  int petCount;                     // Total number of PETs in VM
  int peCount;                      // Total number of PEs in VM
  int pioDofCount;                  // Number 
  int64_t *pioDofList;              // Local to global array map
  DistGrid *distGrid;               // The Array's associated DistGrid
  PIO_IODescHandler *handle;        // New handler object for this IO desc.

  PRINTPOS;
  // check the inputs
  if ((Array *)NULL == arr_p) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL, "- arr_p cannot be NULL",
      ESMC_CONTEXT, &localrc);
    return ESMF_RC_ARG_BAD;
  }
  if ((pio_io_desc_t *)NULL == newDecomp_p) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL,
      "- newDecomp_p cannot be NULL", ESMC_CONTEXT, &localrc);
    return ESMF_RC_ARG_BAD;
  }

  handle = new PIO_IODescHandler(iosys, arr_p);
  pioDofList = (int64_t *)NULL;
  currentVM = ESMC_VMGetCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &localrc)) {
    return false;
  }
  localrc = ESMC_VMGet(currentVM, &localPet, &petCount, &peCount,
                       &mpiCommunicator, (int *)NULL, (int *)NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &localrc)) {
    return false;
  }
  localDeCount = arr_p->getDELayout()->getLocalDeCount();
  PRINTMSG("localDeCount = " << localDeCount);
  //TODO: Remove this restriction (possibly with multiple IO descriptors)
  if (localDeCount > 1) {
    ESMC_LogDefault.Write("I/O does not support multiple DEs per PET",
                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    return ESMF_RC_NOT_IMPL;
  }

  pioDofCount = 0;
  // We need the total number of elements
  // NB: This loop is redundant for now, I wish I could lift the restriction.
  for (localDe = 0; localDe < localDeCount; ++localDe) {
    pioDofCount += arr_p->getTotalElementCountPLocalDe()[localDe];
  }
  PRINTMSG("(" << localPet << "): pioDofCount = " << pioDofCount);
  try {
    // Allocate space for the DOF list
    pioDofList = (int64_t *)malloc(pioDofCount * sizeof(int64_t));
    if ((int64_t *)NULL == pioDofList) {
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, &localrc);
      return ESMF_RC_MEM_ALLOCATE;
    }
    handle->io_descriptor = (pio_io_desc_t)calloc(PIO_SIZE_IO_DESC, 1);
    if ((pio_io_desc_t)NULL == handle->io_descriptor) {
      // Free the DofList!
      free(pioDofList);
      pioDofList = (int64_t *)NULL;
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, &localrc);
      return ESMF_RC_MEM_ALLOCATE;
    }
  } catch(...) {
    if ((int64_t *)NULL != pioDofList) {
      // Free the DofList!
      free(pioDofList);
      pioDofList = (int64_t *)NULL;
    }
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &localrc);
    return localrc;
  }
  // Fill in the PIO DOF list (local to global map)
  // TODO: This is where we would need to make some magic to include
  // TODO: multiple DEs.
  localDe = 0;
  localrc = arr_p->constructFileMap(pioDofList, pioDofCount, localDe);
  if (ESMF_SUCCESS == localrc) {
#if 0
    std::cout << " pioDofList = [";
    for (int i = 0; i < pioDofCount; i++) {
      std::cout << " " << pioDofList[i]
                << ((i == (pioDofCount - 1)) ? ' ' : ',');
    }
    std::cout << "]" << std::endl;
#endif // 0
    // Get TKR info
    switch(arr_p->getTypekind()) {
    case ESMC_TYPEKIND_I4:
      handle->basepiotype = PIO_int;
      break;
    case ESMC_TYPEKIND_R4:
      handle->basepiotype = PIO_real;
      break;
    case ESMC_TYPEKIND_R8:
      handle->basepiotype = PIO_double;
      break;
    case ESMC_TYPEKIND_I1:
    case ESMC_TYPEKIND_I2:
    case ESMC_TYPEKIND_I8:
    case ESMC_TYPEKIND_CHARACTER:
    case ESMF_C8:
    case ESMF_C16:
    case ESMC_TYPEKIND_LOGICAL:
    default:
      ESMC_LogDefault.Write("Unsupported typekind",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      localrc = ESMF_RC_ARG_BAD;
    }
  } else {
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &localrc);
    // Can't return, need to free pioDofList
  }

  if (ESMF_SUCCESS == localrc) {
    int tile = 0;
    distGrid = arr_p->getDistGrid();
    const int *minIndexPDimPTile = distGrid->getMinIndexPDimPTile();
    const int *maxIndexPDimPTile = distGrid->getMaxIndexPDimPTile();
    const int *totalLBound = arr_p->getTotalLBound();
    const int *totalUBound = arr_p->getTotalUBound();
// NB: Is this part of the restrictions on Array I/O?
//    nDims = arr_p->getRank();
    handle->nDims = distGrid->getDimCount();
    // Make sure dims is not used
    if (handle->dims != (int *)NULL) {
      delete handle->dims;
      handle->dims = (int *)NULL;
    }
    handle->dims = new int[handle->nDims];
    // Step through the distGrid dimensions, getting the size of the
    // dimension.
    for (int i = 0; i < handle->nDims; i++) {
      handle->dims[i] = (maxIndexPDimPTile[(tile * handle->nDims) + i] -
                         minIndexPDimPTile[(tile * handle->nDims) + i] + 1);
    }

    handle->arrayRank = arr_p->getRank();
    if (handle->arrayShape != (int *)NULL) {
      delete handle->arrayShape;
      handle->arrayShape = (int *)NULL;
    }
    handle->arrayShape = new int[handle->arrayRank];
    for (int i = 0; i < handle->arrayRank; ++i) {
      handle->arrayShape[i] = (totalUBound[(tile * handle->arrayRank) + i] -
                               totalLBound[(tile * handle->arrayRank) + i] +
                               1);
    }

#ifdef ESMFIO_DEBUG
    {
      char dimstr[64];
      for (int i = 0; i < handle->arrayRank; i++) {
        sprintf((dimstr + (5 * i)), " %03d%c", handle->arrayShape[i],
                (((handle->arrayRank - 1) == i) ? ' ' : ','));
      }
      PRINTMSG(", IODesc shape = [" << dimstr << "], calling pio_initdecomp");
    }
    pio_cpp_setdebuglevel(3);
#endif // ESMFIO_DEBUG

    // Create the decomposition
    pio_cpp_initdecomp_dof(&iosys, handle->basepiotype, handle->dims,
                           handle->nDims, pioDofList, pioDofCount,
                           handle->io_descriptor);
    PRINTMSG("after call to pio_cpp_initdecomp_dof");
#ifdef ESMFIO_DEBUG
  pio_cpp_setdebuglevel(0);
#endif // ESMFIO_DEBUG

    // Add the handle into the master list
    PIO_IODescHandler::activePioIoDescriptors.push_back(handle);
    // Finally, set the output handle
    *newDecomp_p = handle->io_descriptor;
  }
  
  // Free the DofList!
  free(pioDofList);
  pioDofList = (int64_t *)NULL;

  return localrc;
} // PIO_IODescHandler::constructPioDecomp()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_IODescHandler::freePioDecomp()"
//BOPI
// !IROUTINE:  ESMCI::PIO_IODescHandler::freePioDecomp - Delete Decomposition
//
// !INTERFACE:
int PIO_IODescHandler::freePioDecomp(
//
// !RETURN VALUE:
//
//    int return code
//
// !ARGUMENTS:
//
  pio_io_desc_t *decomp_p             // (inout) - PIO decomp desc to free
  ) {
//
// !DESCRIPTION:
//    Free a PIO I/O decomposition structure and remove it from the
//    active instances.
//
//EOPI
//-----------------------------------------------------------------------------

  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;   // local return code
  PIO_IODescHandler *handle;        // Temp handler for finding descriptor
  std::vector<PIO_IODescHandler *>::iterator it;
  bool foundHandle = false;

  // check the inputs
  if ((pio_io_desc_t *)NULL == *decomp_p) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL,
      "- newDecomp_p cannot be NULL", ESMC_CONTEXT, &localrc);
    return ESMF_RC_ARG_BAD;
  }

  // Look for newDecomp_p in the active handle instances
  for (it = PIO_IODescHandler::activePioIoDescriptors.begin();
       it < PIO_IODescHandler::activePioIoDescriptors.end(); ++it) {
    handle = *it;
    if (*decomp_p == handle->io_descriptor) {
      foundHandle = true;
      delete handle;
      handle = (PIO_IODescHandler *)NULL;
      *decomp_p = (pio_io_desc_t)NULL;
      break;
    }
  }

  // If we didn't find the handle, that is bad
  if (foundHandle) {
    localrc = ESMF_SUCCESS;
  } else {
    ESMC_LogDefault.Write("PIO IO descriptor not found or freed",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    localrc = ESMF_RC_MEM_DEALLOCATE;
  }

  return localrc;
} // PIO_IODescHandler::freePioDecomp()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_IODescHandler::getDims()"
//BOPI
// !IROUTINE:  ESMCI::PIO_IODescHandler::getDims
//
// !INTERFACE:
int PIO_IODescHandler::getDims(
//
// !RETURN VALUE:
//
//    Return code
//
// !ARGUMENTS:
//
  const pio_io_desc_t &iodesc,    // (in)  - The IO desciptor
  int * nioDims,                  // (out) - The number of IO dimensions
  int ** ioDims,                  // (out) - Array of dimensions (shape) for IO
  int * narrDims,                 // (out) - The array's rank
  int ** arrDims                  // (out) - The array's shape
  ) {
//
// !DESCRIPTION:
//    Return a pointer to the array of dimensions (shape) for the IO descriptor
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc = ESMF_RC_NOT_FOUND;        // local return code

  std::vector<PIO_IODescHandler *>::iterator it;
  PRINTPOS;
  for (it = PIO_IODescHandler::activePioIoDescriptors.begin();
       it < PIO_IODescHandler::activePioIoDescriptors.end();
       it++) {
    if (iodesc == (*it)->io_descriptor) {
      PRINTMSG("getDims: found handler, nioDims = " << (*it)->nDims <<
               ", ioDims = " << (*it)->dims << ", narrDims = " <<
               (*it)->arrayRank);
      if (nioDims != (int *)NULL) {
        *nioDims = (*it)->nDims;
      }
      if (ioDims != (int **)NULL) {
        *ioDims = (*it)->dims;
      }
      if (narrDims != (int *)NULL) {
        *narrDims = (*it)->arrayRank;
      }
      if (arrDims != (int **)NULL) {
        *arrDims = (*it)->arrayShape;
      }
      localrc = ESMF_SUCCESS;
      break;
    }
  }

  // return success or not found
  return localrc;

} // PIO_IODescHandler::getDims()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_IODescHandler::getIOType()"
//BOPI
// !IROUTINE:  ESMCI::PIO_IODescHandler::getIOType
//
// !INTERFACE:
int PIO_IODescHandler::getIOType(
//
// !RETURN VALUE:
//
//    int IO type (e.g., PIO_int)
//
// !ARGUMENTS:
//
  const pio_io_desc_t &iodesc,            // (in)  - The IO desciptor
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Return the IO type descriptor (e.g., PIO_int)
//
//EOPI
//-----------------------------------------------------------------------------
  int iotype = -1;
  int localrc = ESMF_RC_NOT_FOUND;        // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }
  std::vector<PIO_IODescHandler *>::iterator it;
  PRINTPOS;
  for (it = PIO_IODescHandler::activePioIoDescriptors.begin();
       it < PIO_IODescHandler::activePioIoDescriptors.end();
       it++) {
    if (iodesc == (*it)->io_descriptor) {
      iotype = (*it)->basepiotype;
      localrc = ESMF_SUCCESS;
    }
  }

  // return successfully
  if (rc != NULL) {
    *rc = localrc;
  }

  return iotype;
} // PIO_IODescHandler::getIOType()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PIO_IODescHandler::getIODesc()"
//BOPI
// !IROUTINE:  ESMCI::PIO_IODescHandler::getIODesc
//
// !INTERFACE:
pio_io_desc_t PIO_IODescHandler::getIODesc(
//
// !RETURN VALUE:
//
//    pio_io_desc_t Pointer to the IODescHandler matching the IO system and
//                  array
//
// !ARGUMENTS:
//
  pio_iosystem_desc_t iosys,              // (in)  - The PIO IO system
  Array *arrayArg,                        // (in)  - The IO desciptor
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Return a pointer to an appropriate PIO IO descriptor
//
//EOPI
//-----------------------------------------------------------------------------
  pio_io_desc_t iodesc = (pio_io_desc_t)NULL; // IO descriptor
  int localrc = ESMF_RC_NOT_FOUND;        // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  std::vector<PIO_IODescHandler *>::iterator it;
  for (it = PIO_IODescHandler::activePioIoDescriptors.begin();
       it < PIO_IODescHandler::activePioIoDescriptors.end();
       ++it) {
    if ((iosys == (*it)->ios) && (arrayArg == (*it)->array_p)) {
      iodesc = (*it)->io_descriptor;
      localrc = ESMF_SUCCESS;
    }
  }

  // return successfully
  if (rc != NULL) {
    *rc = localrc;
  }

  return iodesc;
} // PIO_IODescHandler::getIODesc()
//-----------------------------------------------------------------------------

}  // end namespace ESMCI
