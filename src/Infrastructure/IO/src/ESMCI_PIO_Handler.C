// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
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
#define PIO_DEBUG_LEVEL 0
// include associated header file
#include "ESMCI_PIO_Handler.h"

// higher level, 3rd party or system includes here
#include <vector>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>

#include <errno.h>
#include <unistd.h>

// other ESMF include files here.
#include "ESMCI_Macros.h"
#include "ESMCI_Container.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_ArrayBundle.h"
#include "ESMCI_Info.h"
#include "json.hpp"
#include "ESMCI_TraceMacros.h"

// Define PIO NetCDF and Parallel NetCDF flags
#ifdef ESMF_PNETCDF
# define _PNETCDF
#include <pnetcdf.h>
# elif ESMF_NETCDF
# define _NETCDF
# include <netcdf.h>
#endif
#include <pio.h>

#include "esmf_io_debug.h"

using json = nlohmann::json;  // Convenience rename for JSON namespace.

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
    int ios;                  // PIO IO system instance for this descriptor
    int io_descriptor;        // PIO IO descriptor
    int nDims;                // The number of dimensions for Array IO
    int *dims;                // The shape of the Array IO
    int basepiotype;          // PIO version of Array data type
    Array *array_p;           // The array matched to this descriptor
    int arrayRank;            // The rank of array_p
    int *arrayShape;          // The shape of array_p
  public:
    PIO_IODescHandler(int iosArg, Array *arrayArg) {
      ios = iosArg;
      io_descriptor = (int)NULL;
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
    static int constructPioDecomp(int iosys, Array *arr_p,
                                  int *newDecomp_p);
    static int freePioDecomp(int *decomp_p);
    static int getDims(const int &iodesc,
                       int * nioDims = (int *)NULL,
                       int ** ioDims = (int **)NULL,
                       int * narrDims = (int *)NULL,
                       int ** arrDims = (int **)NULL);
    static int getIOType(const int &iodesc, int *rc = (int *)NULL);
    static int getIODesc(int iosys,
                                   Array *arrayArg, int *rc = (int *)NULL);
  };

//
//-------------------------------------------------------------------------
//
// class variables
//
//-------------------------------------------------------------------------
//

  std::vector<int> PIO_Handler::activePioInstances;
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
  int instance = 0;
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
    PIOc_set_log_level(PIO_DEBUG_LEVEL);
#endif // ESMFIO_DEBUG
    if (!instanceFound) {
      PRINTMSG("Before PIOc_Init_Intracomm, num_iotasks = " << num_iotasks);
      PIOc_Init_Intracomm(comp_comm, num_iotasks,
                          stride, base, rearr, &instance);
      PRINTMSG("After PIOc_Init_Intracomm, instance = " << instance);
      // If we get here, hopefully everything is OK.
      if (instance != 0) {
#ifdef ESMFIO_FILESYSTEM_LUSTRE
          PIOc_set_hint(instance, "romio_ds_read", "disable");
          PIOc_set_hint(instance, "romio_ds_write", "disable");
#endif
#ifdef ESMFIO_FILESYSTEM_GPFS
          PIOc_set_hint(instance, "ibm_largeblock_io", "true");
#endif
//          localrc = PIOc_set_rearr_opts(instance, PIO_REARR_COMM_P2P, PIO_REARR_COMM_FC_2D_ENABLE, 0, 0, 0, 0, 0, 4);

        // Set the error handling to return PIO errors
        // Just return error (error code may be different on different PEs).
        // Broadcast the error to all PEs (consistant error handling)
        PIOc_Set_IOSystem_Error_Handling(instance, PIO_BCAST_ERROR);
        PRINTMSG("After PIOc_Set_IOSystem_Error_Handling");
        // Add the instance to the global list
        PIO_Handler::activePioInstances.push_back(instance);
        PRINTMSG("push_back");
        localrc = ESMF_SUCCESS;
      } else {
        // Something went wrong (this really shouldn't happen)
        ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "Unknown error in PIOc_Init_Intracomm",
            ESMC_CONTEXT, rc);
        return;
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
      int localrc;
      VM *vm = VM::getCurrent(&localrc);
      if (ESMC_LogDefault.MsgFoundError (localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
        return rc;
      communicator = vm->getMpi_c();
      my_rank = vm->getLocalPet();

      // Figure out the inputs for the initialize call
      int numtasks =  vm->getPetCount();
      stride = vm->getSsiMaxPetCount();
      base = 0; // IO tasks start with base and are every stride tasks until num_iotasks.
      if (numtasks > stride){
          num_iotasks = int(numtasks/stride);
#ifdef ESMF_PNETCDF
          rearr = PIO_REARR_SUBSET;
#else
          rearr = PIO_REARR_BOX;
#endif
      }else{
          num_iotasks = numtasks > 32 ? 32:numtasks;
          stride = numtasks/num_iotasks;
          rearr = PIO_REARR_BOX;
      }

      // Call the static function
      PIO_Handler::initialize(my_rank, communicator, num_iotasks,
                              stride, rearr, &base, &rc);
      PRINTMSG("After initialize, rc = " << rc);
      if (ESMF_SUCCESS == rc) {
        PRINTMSG("Looking for active instance, size = " << activePioInstances.size());
        pioSystemDesc = PIO_Handler::activePioInstances.back();
        PRINTMSG("Fetched PIO system descriptor, " << (void *)pioSystemDesc);
      }
//      PIOc_set_blocksize(444444);
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
  int piorc;
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
      int instance = PIO_Handler::activePioInstances.back();
      piorc = PIOc_finalize(instance);
      // Even if we have an error but log and keep going to try and shut
      // down other PIO instances
      CHECKPIOWARN(piorc, "Error shutting down PIO instance",
          ESMF_RC_FILE_UNEXPECTED, localrc);
      /*
#ifdef ESMFIO_DEBUG
      static int ionum = 1;
      if (piorc != PIO_NOERR) {
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
      */

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

    // fill in the PIO_Handler objecte
      pioSystemDesc =  0;
    pioFileDesc = 0;
    pioIODesc = 0;
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
  // kill the pointer to the PIO_Handler object
  // NB: This does not shutdown the PIO instance, it may be reused.
  pioSystemDesc = 0;
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
  int piorc;                              // PIO error value
  int * ioDims;                           // Array IO shape
  int nioDims;                            // Array IO rank
  int * arrDims;                          // Array shape
  int narrDims;                           // Array rank
  int iodesc;                             // PIO IO descriptor
  int vardesc;                            // PIO variable descriptor
  int basepiotype;                        // PIO version of Array data type
  void *baseAddress;                      // The address of the Array IO data
  int localDE;                            // DE to use for IO
  int nVar;                               // Number of variables in file
  int nAtt;                               // Number of attributes in file
  int unlim;                              // Unlimited dimension ID
  std::string varname;                    // Default variable name

  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;
  // File open?
  if (isOpen() != ESMF_TRUE)
    if (ESMC_LogDefault.MsgFoundError (ESMF_RC_FILE_READ, "file not open",
        ESMC_CONTEXT, rc)) return;

  // Get a pointer to the array data
  // Still have the one DE restriction so use localDE = 0
  localDE = 0;
  baseAddress = arr_p->getLocalarrayList()[localDE]->getBaseAddr();
  int arrlen = 1;
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
    int nDims;

    // If frame >= 0 then we need to not use the unlimited dim in the iodesc.
    iodesc = getIODesc(pioSystemDesc, arr_p, &ioDims, &nioDims,
		       &arrDims, &narrDims, &basepiotype, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
				      ESMC_CONTEXT, rc)) return;

    // This should work if it is a NetCDF file.
    piorc = PIOc_inq(pioFileDesc, &nDims,
		     &nVar, &nAtt, &unlim);
    if (!CHECKPIOERROR(piorc, "File is not in NetCDF format", ESMF_RC_FILE_READ, (*rc))) {
      return;
    }

    if (((char *)NULL != name) && (strlen(name) > 0)) {
      varname = name;
    } else {
      varname = arr_p->getName();
    }

    piorc = PIOc_inq_varid(pioFileDesc, varname.c_str(), &vardesc);
    // An error here means the variable is not in the file
    const std::string errmsg = "variable " + varname + " not found in file";
    if (!CHECKPIOERROR(piorc, errmsg, ESMF_RC_FILE_READ, (*rc))) {
      return;
    }

    int frame;
    if (((int *)NULL != timeslice) && (*timeslice > 0) && narrDims < nioDims) {
      //
      // Do not use the unlimited dim in iodesc calculation
      //
      int dimids[narrDims];
      piorc = PIOc_inq_vardimid(pioFileDesc, vardesc, dimids);
      // This should never happen
      const std::string errmsg = "variable " + varname + " inq_dimid failed";
      if (!CHECKPIOERROR(piorc, errmsg, ESMF_RC_FILE_READ, (*rc))) {
	return;
      }
       
      if(unlim == dimids[narrDims-1]){
	narrDims = narrDims - 1;
      }
      for (int i=0; i<narrDims; i++){
	arrlen *= arrDims[i];
      }

      int dimid_time;
      MPI_Offset time_len;
      piorc = PIOc_inq_dimid(pioFileDesc, "time", &dimid_time);
      if (!CHECKPIOERROR(piorc, "No time dimension found in file", ESMF_RC_FILE_READ, (*rc))) {
        return;
      }
      // Check to see if time is the unlimited dimension
      if (dimid_time != unlim) {
        PRINTMSG(" Time dimension = " << dimid_time <<
                 ", unlimited dim = " << unlim);
        if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
            " time is not the file's unlimited dimension",
            ESMC_CONTEXT, rc)) {
          return;
        }
      }
      // Check to make sure the requested record is in the file
      piorc = PIOc_inq_dimlen(pioFileDesc,
          dimid_time, &time_len);
      if (!CHECKPIOERROR(piorc, "Error finding time length", ESMF_RC_FILE_READ, (*rc))) {
        return;
      }
      if (*timeslice > time_len) {
        PRINTMSG(" (" << my_rank << "): " <<
                 "Timeframe is greater than that in file" <<
                 getFilename() << ", file time = " << time_len <<
                 ", requested record = " << *timeslice);
        if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
            "Timeframe is greater than max in file",
            ESMC_CONTEXT, rc)) {
          return;
        }
      }
      frame = (*timeslice);
    } else {
      frame = -1;
      for (int i=0; i<narrDims; i++){
	arrlen *= arrDims[i];
      }

    }
    if (unlim >= 0 && frame > 0) {
        PRINTMSG("calling setframe for read_darray, frame = " << frame);
        PIOc_setframe(pioFileDesc, vardesc, frame-1);
    }

#endif // defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
#ifdef ESMFIO_DEBUG
  PIOc_set_log_level(0);
#endif // ESMFIO_DEBUG

  PRINTMSG("calling read_darray, pio type = " << basepiotype << ", address = " << baseAddress);
  // Read in the array
  piorc = PIOc_read_darray(pioFileDesc, vardesc, iodesc,
                           arrlen, (void *)baseAddress);

  if (!CHECKPIOERROR(piorc, "Error reading array data", ESMF_RC_FILE_READ, (*rc))) {
    return;
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
  const std::vector<std::string> &dimLabels, // (in) Optional dimension labels
  int *timeslice,                         // (in) Optional timeslice
  const ESMCI::Info *varAttPack,            // (in) Optional per-variable Attribute Package
  const ESMCI::Info *gblAttPack,            // (in) Optional global Attribute Package
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
  int piorc;                              // PIO error value
  int * ioDims;                           // Array IO shape
  int nioDims;                            // Array IO rank
  int * arrDims;                          // Array shape
  int narrDims;                           // Array rank
  int iodesc;                   // PIO IO descriptor
  int vardesc = 0;          // PIO variable descriptor
  int basepiotype;                        // PIO version of Array data type
  void *baseAddress;                      // The address of the Array IO data
  int localDE;                            // DE to use for IO
  int ncDims[8];                          // To hold NetCDF dimensions
  int unlim = -1;                         // Unlimited dimension ID
  int timeFrame = -1;                     // ID of time dimension (>0 if used)
  int timesliceVal = -1;                  // Used time value (from timeslice)
  bool varExists = false;                 // true if varname is defined in file
  std::string varname;                    // Variable name
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;
  if ((int *)NULL != timeslice) {
    timesliceVal = *timeslice;
  }
  // File open?
  if (isOpen() != ESMF_TRUE)
    if (ESMC_LogDefault.MsgFoundError (ESMF_RC_FILE_READ, "file not open",
        ESMC_CONTEXT, rc)) return;

  iodesc = getIODesc(pioSystemDesc, arr_p, &ioDims, &nioDims,
      &arrDims, &narrDims, &basepiotype, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) return;
  for (int i=0; i<narrDims; i++) {
    if (arrDims[i] < 0) {
      if (ESMC_LogDefault.MsgFoundError (ESMF_RC_INTNRL_BAD, "array dimension extent < 0",
            ESMC_CONTEXT, rc)) return;
    }
  }

  if (dimLabels.size() > 0 && dimLabels.size() < (unsigned int)nioDims) {
    std::stringstream errmsg;
    errmsg << dimLabels.size() << " user dimension label(s) supplied, " << nioDims << " expected";
    if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_SIZE, errmsg,
            ESMC_CONTEXT, rc)) return;
  }


  // Get a pointer to the array data
  // Still have the one DE restriction so use localDE = 0
  localDE = 0;
  baseAddress = arr_p->getLocalarrayList()[localDE]->getBaseAddr();
  PRINTMSG("baseAddress = 0x" << (void *)baseAddress);
//  int arrlen = arr_p->getLocalarrayList()[localDE]->getByteCount();
  int arrlen = 1;
  const int *counts = arr_p->getLocalarrayList()[localDE]->getCounts();
  for (int i=0; i<narrDims; i++)
      arrlen *= counts[i]; 
  PRINTMSG("arrlen = " << arrlen);

#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
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
      piorc = PIOc_redef(pioFileDesc);
      // Not all NetCDF errors are really errors here so we need to check
      if ((PIO_NOERR != piorc) && (NC_EINDEFINE != piorc)) {
        if (!CHECKPIOERROR(piorc,
            ((NC_EPERM == piorc) ?
            "File is read only" :
            "File is not in NetCDF format"),
            ESMF_RC_FILE_WRITE, (*rc))) {
          return;
        }
      }

      // This should work if it is a NetCDF file.
      PRINTMSG("Calling PIOc_inq");
      piorc = PIOc_inq(pioFileDesc, &nfDims,
                                &nVar, &nAtt, &unlim);
      if (!CHECKPIOERROR(piorc, "File is not in NetCDF format",
          ESMF_RC_FILE_WRITE, (*rc))) {
        return;
      }

      // We have a NetCDF file, see if the variable is in there
      PRINTMSG("Looking for variable in file");
      piorc = PIOc_inq_varid(pioFileDesc, varname.c_str(), &vardesc);
      // This should succeed if the variable exists
      varExists = (PIO_NOERR == piorc);
    }

  // Check consistency of time dimension with timeslice
  bool hasTimeDim;

    int dimidTime;
    PIO_Offset timeLen;
    PRINTMSG("Checking time dimension");
    //piorc = PIOc_inq_dimid(pioFileDesc, "time", &dimidTime);
    piorc = PIOc_inq_unlimdim(pioFileDesc, &dimidTime);
    // NetCDF does not specify which error code goes with with
    // condition so we will guess that there is no time dimension
    // on any error condition (This may be an error depending on context).
    hasTimeDim = (PIO_NOERR == piorc && dimidTime != -1);
    PRINTMSG("inq_dimid  = " << piorc);
    PRINTMSG("hasTimeDim = " << hasTimeDim);
    PRINTMSG("unlim = " << unlim);
    if (hasTimeDim) {
      // Retrieve the max time field
      piorc = PIOc_inq_dimlen(pioFileDesc, dimidTime, &timeLen);
      PRINTMSG("inq_dimlen = " << piorc);
      PRINTMSG("dimidTime = " << dimidTime);
      PRINTMSG("timeLen = " << timeLen);
      if (!CHECKPIOERROR(piorc, "Error retrieving information about time",
          ESMF_RC_FILE_WRITE, (*rc))) {
        return;
      }

      // Check to make sure that time is the unlimited dimension
      if (dimidTime != unlim)
        if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
            "Time is not the file's unlimited dimension",
             ESMC_CONTEXT, rc)) {
          return;
        }
    }
    if (timesliceVal >= 0) {
      if (varExists & !hasTimeDim) {
        // It is an error to not have a time dimension if we are trying to
        // write a timeslice and the variable already exists
        // (it won't have time)
        if (ESMC_LogDefault.MsgFoundError(ESMF_RC_FILE_WRITE,
            "Field already exists without time dimension",
            ESMC_CONTEXT, rc)) {
          return;
        }
      } else {
        timeFrame = timesliceVal;
      }
    } else if (timesliceVal < 0) {
      // Special case for no timeslice input passed but time dimension exists
      if (varExists && hasTimeDim) {
        timeFrame = timeLen + 1;
        PRINTMSG("timeslice not passed but set to " << timeFrame);
      }
    }

  PRINTMSG("ready to check var compat., timeFrame = " << timeFrame);
  if (varExists) {
    int nDims;
    int nArrdims = nioDims + ((timeFrame > 0) ? 1 : 0);

    // Check compatibility between array to write and existing variable.
    piorc = PIOc_inq_varndims(pioFileDesc, vardesc, &nDims);
    if (!CHECKPIOERROR(piorc, "Error retrieving information about variable",
        ESMF_RC_FILE_WRITE, (*rc))) {
      return;
    }

    if (nDims != nArrdims) {
      if (ESMC_LogDefault.MsgFoundError(ESMF_RC_FILE_UNEXPECTED,
          "Variable rank in file does not match array",
          ESMC_CONTEXT, rc)) {
        PRINTMSG("Variable rank mismatch: nioDims = " << nioDims <<
               ", nArrdims = " << nArrdims << ", nDims = " << nDims);
        return;
      }
    }

    PRINTMSG("Calling pio_cpp_inq_vardimid_vdesc");
    std::vector<int> dimIds(nDims);
    piorc = PIOc_inq_vardimid(pioFileDesc, vardesc, &dimIds.front());
    if (!CHECKPIOERROR(piorc, "Error retrieving information about variable",
        ESMF_RC_FILE_WRITE, (*rc))) {
      return;
    }

    MPI_Offset dimLen;
    int ioDimNum = 0;
    for (int i = 0; i < nDims; i++) {
      piorc = PIOc_inq_dimlen(pioFileDesc, dimIds[i], &dimLen);
      if (!CHECKPIOERROR(piorc, "Error retrieving dimension information",
          ESMF_RC_FILE_WRITE, (*rc))) {
        return;
      }
      PRINTMSG("PIOc_inq_dimlen for dim = " << i << " dimLen="<<dimLen);

      if (dimIds[i] == unlim) {
        if (timeFrame <= 0) {
          if (ESMC_LogDefault.MsgFoundError(ESMF_RC_FILE_UNEXPECTED,
              "File variable requires time dimension",
              ESMC_CONTEXT, rc)) {
            return;
          }
        } else if ((timeFrame <= dimLen) && !overwriteFields()) {
          // This 'error' might be incorrect in that we can't figure
          // out the max frame of this variable, only for the whole file.
          if (ESMC_LogDefault.MsgFoundError(ESMF_RC_FILE_UNEXPECTED,
              "Can't overwrite timeslice",
              ESMC_CONTEXT, rc)) {
            return;
          }
        }
      } else if (dimLen != ioDims[nDims - ioDimNum - 1 - int(hasTimeDim)]) {
          PRINTMSG("dimLen = "<<dimLen<<", ioDims["<<nDims-ioDimNum-1-int(hasTimeDim)<<"]="<<ioDims[nDims-ioDimNum-1 - int(hasTimeDim)]);
          if (ESMC_LogDefault.MsgFoundError(ESMF_RC_FILE_UNEXPECTED,
              "Variable dimension in file does not match array",
              ESMC_CONTEXT, rc)) {
            return;
          }
      } else {
        ioDimNum++;
      }
    }
  }

  if (varExists && !overwriteFields() && (timeFrame <= 0)) {
    // Check to see if we can overwrite an existing field or timeslice
    std::string errmsg = "Variable " + varname + " pre-exists, however overwrite flag is .false.";
    if (ESMC_LogDefault.MsgFoundError(ESMF_RC_FILE_WRITE, errmsg,
        ESMC_CONTEXT, rc)) {
      return;
    }
  }

  if (!varExists) {
    // Ensure we are in define mode
    PRINTMSG("Going into NetCDF define mode (redef)");
    piorc = PIOc_redef(pioFileDesc);
    // Not all NetCDF errors are really errors here so we need to check
    if ((PIO_NOERR != piorc) && (NC_EINDEFINE != piorc)) {
      if (!CHECKPIOERROR(piorc,
          ((NC_EPERM == piorc) ?
          "File is read only" :
          "File is not in NetCDF format"),
          ESMF_RC_FILE_WRITE, (*rc))) {
        return;
      }
    }
  }

  if (!varExists) {
    // Create the variable
    for (int i = 0; i < nioDims; i++) {
      std::string axis;
      if (dimLabels.size() > 0)
        axis = dimLabels[i];
      else {
        std::stringstream axis_tmp;
        axis_tmp << varname << "_dim" << std::setfill('0') << std::setw(3) << i+1;
        axis = axis_tmp.str();
      }

      // if dimension already exists, use it.
      int dimid_existing;
      piorc = PIOc_inq_dimid(pioFileDesc, axis.c_str(), &dimid_existing);
      if (PIO_NOERR == piorc) {
        MPI_Offset dim_len;
        piorc = PIOc_inq_dimlen(pioFileDesc, dimid_existing, &dim_len);
        if (!CHECKPIOERROR(piorc, "Error finding existing dimension length", ESMF_RC_FILE_WRITE, (*rc))) {
          return;
        }
        if (ioDims[i] != dim_len) {
          std::stringstream msg;
          msg << "Existing dimension " << axis << " length " << dim_len << " != required " << ioDims[i];
          if (ESMC_LogDefault.MsgFoundError(ESMF_RC_FILE_WRITE, msg,
              ESMC_CONTEXT, rc)) {
            return;
          }
        }
        ncDims[nioDims - i - 1] = dimid_existing;
      } else {
        PRINTMSG("Defining dimension " << i);
        piorc = PIOc_def_dim(pioFileDesc, axis.c_str(),
                                ioDims[i], &ncDims[nioDims - i - 1]);
        if (!CHECKPIOERROR(piorc, std::string("Defining dimension: ") + axis,
            ESMF_RC_FILE_WRITE, (*rc))) {
          return;
        }
      }
    }
    PRINTMSG("finished defining space dims, timeFrame = " << timeFrame);

    if (timeFrame > -1) {
        nioDims++;
        for(int i=nioDims;i>0;i--)
            ncDims[i] = ncDims[i-1];
      if (hasTimeDim) {
        piorc = PIOc_inq_dimid (pioFileDesc, "time", &ncDims[0]);
        if (!CHECKPIOERROR(piorc, "Attempting to obtain 'time' dimension ID",
            ESMF_RC_FILE_WRITE, (*rc))) {
          return;
        }
      } else {
        PRINTMSG("Defining time dimension");
        piorc = PIOc_def_dim(pioFileDesc, "time",
                                PIO_UNLIMITED, &ncDims[0]);
        if (!CHECKPIOERROR(piorc, "Attempting to define 'time' dimension",
            ESMF_RC_FILE_WRITE, (*rc))) {
          return;
        }
      }
    }
  }
  PRINTMSG("varExists = " << varExists);
  if (!varExists) {
    PRINTMSG("niodims = " << nioDims);
    PRINTMSG("basepiotype = " << basepiotype);
     
    piorc = PIOc_def_var(pioFileDesc, varname.c_str(), basepiotype,
                         nioDims, ncDims, &vardesc);
    if (!CHECKPIOERROR(piorc, "Attempting to define PIO vardesc for: " + varname,
        ESMF_RC_FILE_WRITE, (*rc))) {
      return;
    }
  }
  if (timeFrame >= 0) {
#ifdef ESMFIO_DEBUG
    int nvdims;
    PIOc_inq_varndims(pioFileDesc, vardesc, &nvdims);
    PRINTMSG("calling setframe, timeFrame = " << timeFrame);
#endif // ESMFIO_DEBUG
    piorc = PIOc_setframe(pioFileDesc, vardesc, timeFrame-1);
    if (!CHECKPIOERROR(piorc, "Attempting to setframe for: " + varname,
        ESMF_RC_FILE_WRITE, (*rc))) {
      return;
    }
  }
#ifdef ESMFIO_DEBUG
  else {
    PRINTMSG("NOT calling setframe, timeFrame = " << timeFrame);
  }
    if (varExists) {
      int varid;
      int lrc;
      lrc = PIOc_inq_varid(pioFileDesc, varname.c_str(), &varid);
      PRINTMSG("varid = " << varid);
    }
#endif // ESMFIO_DEBUG

    // ESMF Attribute Package -> NetCDF variable and global attributes
    if (varAttPack) {
      attPackPut (vardesc, varAttPack, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) {
        return;
      }
    }
    if (gblAttPack) {
      attPackPut (NC_GLOBAL, gblAttPack, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) {
        return;
      }
    }


  PRINTMSG("calling enddef, status = " << rc);

    piorc = PIOc_enddef(pioFileDesc);
    if (!CHECKPIOERROR(piorc,  "Attempting to end definition of variable: " + varname,
        ESMF_RC_FILE_WRITE, (*rc))) {
      return;
    }

#endif // defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
  PRINTMSG("calling write_darray, pio type = " << basepiotype << ", address = " << baseAddress);
#ifdef ESMFIO_DEBUG
  PIOc_set_log_level(0);
#endif // ESMFIO_DEBUG
  // Write the array
  ESMCI_IOREGION_ENTER("PIOc_write_darray");
  piorc =  PIOc_write_darray(pioFileDesc, vardesc, iodesc, arrlen,
                             (void *)baseAddress, NULL);
  if (!CHECKPIOERROR(piorc, "Attempting to write file",
            ESMF_RC_FILE_WRITE, (*rc))) {
      return;
  }
  new_file = false;
  ESMCI_IOREGION_EXIT("PIOc_write_darray");


  // Cleanup & return
  PRINTMSG("cleanup and return");
  if (rc != NULL) {
    *rc = ESMF_SUCCESS;
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
  int piorc;                              // PIO error value
  VM *vm = VM::getCurrent(&localrc);
  int numtasks =  vm->getPetCount();
  int petspernode = vm->getSsiMaxPetCount();


  struct iofmt_map_t {
    int esmf_iofmt;
    int pio_fmt;
  } iofmt_map[] = {
#if defined (ESMF_PNETCDF)
    { ESMF_IOFMT_NETCDF,   PIO_IOTYPE_PNETCDF }
    ,{ ESMF_IOFMT_NETCDF_64BIT_OFFSET, PIO_IOTYPE_PNETCDF }
    ,{ ESMF_IOFMT_NETCDF_64BIT_DATA, PIO_IOTYPE_PNETCDF }
    ,{ ESMF_IOFMT_NETCDF4,  PIO_IOTYPE_NETCDF }
    ,{ ESMF_IOFMT_NETCDF4C, PIO_IOTYPE_NETCDF4C }
    ,{ ESMF_IOFMT_NETCDF4P, PIO_IOTYPE_NETCDF4P }
#elif defined (ESMF_NETCDF)
    { ESMF_IOFMT_NETCDF,   PIO_IOTYPE_NETCDF }
    ,{ ESMF_IOFMT_NETCDF_64BIT_OFFSET, PIO_IOTYPE_NETCDF }
    ,{ ESMF_IOFMT_NETCDF_64BIT_DATA, PIO_IOTYPE_NETCDF }
    ,{ ESMF_IOFMT_NETCDF4,  PIO_IOTYPE_NETCDF }
    ,{ ESMF_IOFMT_NETCDF4C, PIO_IOTYPE_NETCDF4C }
    ,{ ESMF_IOFMT_NETCDF4P, PIO_IOTYPE_NETCDF4P }
#endif
  };

  int iofmt_map_size = sizeof (iofmt_map)/sizeof (iofmt_map_t);

  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;
  if (isPioInitialized() != ESMF_TRUE) {
    if (ESMC_LogDefault.MsgFoundError (ESMF_RC_INTNRL_BAD,
        "PIO not initialized",
        ESMC_CONTEXT, rc)) return;
  } else if (isOpen() == ESMF_TRUE) {
    if (ESMC_LogDefault.MsgFoundError (ESMF_RC_FILE_OPEN,
        "PIO not initialized",
        ESMC_CONTEXT, rc)) return;
  } else if (pioSystemDesc <= 0 ) {
    // Just grab last created PIO instance for now (TBD: need way to choose)
    pioSystemDesc = PIO_Handler::activePioInstances.back();
  }

  // Translate the I/O format from ESMF to PIO
#if !defined(ESMF_NETCDF) && !defined (ESMF_PNETCDF)
  if (ESMC_LogDefault.MsgFoundError(ESMF_RC_LIB_NOT_PRESENT,
      "Library for requested I/O format is not present", ESMC_CONTEXT, rc))
    return;
#endif

  int i_loop;
  for (i_loop=0; i_loop<iofmt_map_size; i_loop++) {
    if (getFormat() == iofmt_map[i_loop].esmf_iofmt) {
      iotype = iofmt_map[i_loop].pio_fmt;
      break;
    }
  }
  if (i_loop == iofmt_map_size) {
    if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_BAD,
        "unsupported/unknown I/O format", ESMC_CONTEXT, rc))
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
  bool file_exists = IO_Handler::fileExists(getFilename(), !readonly);
  switch(getFileStatusFlag()) {
  case ESMC_FILESTATUS_UNKNOWN:
    if (file_exists) {
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
    if (ESMC_LogDefault.MsgFoundError(localrc, "unknown file status argument", ESMC_CONTEXT, rc))
      return;
  }

  if (okToCreate) {
#ifdef ESMFIO_DEBUG
    std::string errmsg = "Calling PIOc_createfile";
    PIOc_set_log_level(PIO_DEBUG_LEVEL);
    ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
#endif // ESMFIO_DEBUG
    // Looks like we are ready to try and create the file
    mode |= clobberMode;
    switch (getFormat()){
    case ESMF_IOFMT_NETCDF_64BIT_OFFSET: 
    {
        mode |= PIO_64BIT_OFFSET;
        break;
    }
    case ESMF_IOFMT_NETCDF_64BIT_DATA: 
    {
        mode |= PIO_64BIT_DATA;
        break;
    }
    }    
    ESMCI_IOREGION_ENTER("PIOc_createfile");

    piorc = PIOc_createfile(pioSystemDesc, &pioFileDesc,
                                 &iotype, getFilename(), mode);
    ESMCI_IOREGION_EXIT("PIOc_createfile");
    if (!CHECKPIOWARN(piorc, std::string("Unable to create file: ") + getFilename(),
      ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    } else {
      new_file = true;
      PRINTMSG("call to PIOc_createfile: success for " << getFilename() << " iotype= "<< iotype << " Mode "<< mode << " ESMF FMT "<<getFormat() );
    }
#ifdef ESMFIO_DEBUG
    PIOc_set_log_level(0);
#endif // ESMFIO_DEBUG
    piorc = PIOc_set_fill(pioFileDesc, PIO_NOFILL, NULL);
    if (!CHECKPIOWARN(piorc, std::string("Unable to set fill on file: ") + getFilename(),
                      ESMF_RC_FILE_OPEN, (*rc))) {
        return;
    }
  } else {
    PRINTMSG(" calling PIOc_openfile with mode = " << mode <<
             ", file = \"" << getFilename() << "\"");
    // Looks like we are ready to go
    ESMCI_IOREGION_ENTER("PIOc_openfile");
    piorc = PIOc_openfile(pioSystemDesc, &pioFileDesc,
                               &iotype, getFilename(), mode);
    ESMCI_IOREGION_EXIT("PIOc_openfile");
    PRINTMSG(", called PIOc_openfile on " << getFilename());
    if (!CHECKPIOWARN(piorc, std::string("Unable to open existing file: ") + getFilename(),
        ESMF_RC_FILE_OPEN, (*rc))) {
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
#define ESMC_METHOD "ESMCI::PIO_Handler::attPackPut()"
//BOPI
// !IROUTINE:  ESMCI::PIO_Handler::attPackPut
//
// !INTERFACE:
void PIO_Handler::attPackPut (
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int vardesc,      // (in) - variable to write attributes into, NULL for global
  const ESMCI::Info *attPack,  // (in) - AttPack containing name/value(s) pairs
  int *rc                      // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Puts the Attributes and their values into the NetCDF file.  If vardesc is 0, the
//    attribute will be considered a global attribute.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int piorc;

  const json &j = attPack->getStorageRef();
  for (json::const_iterator it=j.cbegin(); it!=j.cend(); it++) {
    if (it.key().rfind("ESMF:", 0) == 0) {
      continue;
    }
    json jcurr;
    if (it.value().is_array()) {
      jcurr = it.value();
    } else {
      json arr = json::array();
      arr.push_back(it.value());
      jcurr = arr;
    }
    if (!(jcurr.is_array())) {
      if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ATTR_WRONGTYPE, "Only JSON arrays supported. Key is: " + it.key(), ESMC_CONTEXT, rc))
        return;
    }
    int size = (int)(jcurr.size());

    // Determine if the target key is 32-bit
    bool is_32bit = false;
    try {
      json::json_pointer jp = attPack->formatKey(it.key());
      is_32bit = ESMCI::retrieve_32bit_flag(attPack->getTypeStorage(), jp, true);
    }
    ESMC_CATCH_ERRPASSTHRU

    ESMC_TypeKind_Flag att_type = ESMCI::json_type_to_esmf_typekind(jcurr, true, is_32bit);
    switch (att_type) {
      case ESMC_TYPEKIND_CHARACTER: {
        if (size > 1) {
          if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ATTR_WRONGTYPE, "JSON arrays with size > 1 not supported for strings. Key is: " + it.key(), ESMC_CONTEXT, rc))
            return;
        }
        const std::string value = jcurr[0];
        piorc = PIOc_put_att_text (pioFileDesc, vardesc,
                                   it.key().c_str(), strlen(value.c_str()), value.c_str());
        if (!CHECKPIOERROR(piorc, "Attempting to set string Attribute: " + it.key(),
                           ESMF_RC_FILE_WRITE, (*rc))) return;
        break;
      }
      case ESMC_TYPEKIND_I8: {
        const std::vector<long> value = jcurr.get<std::vector<long>>();
        piorc = PIOc_put_att_long (pioFileDesc, vardesc,
                                   it.key().c_str(), att_type, size, value.data());
        if (!CHECKPIOERROR(piorc, "Attempting to set I8 Attribute: " + it.key(),
                           ESMF_RC_FILE_WRITE, (*rc))) return;
        break;
      }
      case ESMC_TYPEKIND_R8: {
        const std::vector<double> value = jcurr.get<std::vector<double>>();
        piorc = PIOc_put_att_double (pioFileDesc, vardesc,
                                     it.key().c_str(), att_type, size, value.data());
        if (!CHECKPIOERROR(piorc, "Attempting to set R8 Attribute: " + it.key(),
                           ESMF_RC_FILE_WRITE, (*rc))) return;
        break;
      }
      case ESMC_TYPEKIND_I4: {
        const std::vector<int> value = jcurr.get<std::vector<int>>();
        piorc = PIOc_put_att_int (pioFileDesc, vardesc,
                                  it.key().c_str(), att_type, size, value.data());
        if (!CHECKPIOERROR(piorc, "Attempting to set I4 Attribute: " + it.key(),
                           ESMF_RC_FILE_WRITE, (*rc))) return;
        break;
      }
      case ESMC_TYPEKIND_R4: {
        const std::vector<float> value = jcurr.get<std::vector<float>>();
        piorc = PIOc_put_att_float (pioFileDesc, vardesc,
                                    it.key().c_str(), att_type, size, value.data());
        if (!CHECKPIOERROR(piorc, "Attempting to set R4 Attribute: " + it.key(),
                           ESMF_RC_FILE_WRITE, (*rc))) return;
        break;
      }
      default:
        if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ATTR_NOTSET,
                                          "Attribute " + it.key() + " has unsupported value type",
                                          ESMC_CONTEXT, rc)) return;
    }
  }

  if (rc) {*rc = ESMF_SUCCESS;}

} // PIO_Handler::attPackPut()
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
  if (pioFileDesc == 0) {
    PRINTMSG("pioFileDesc is NULL");
    return ESMF_FALSE;
  } else if (PIOc_File_is_Open(pioFileDesc) != 0) {
    PRINTMSG("File is open");
    return ESMF_TRUE;
  } else {
    // This really should not happen, warn and clean up just in case
    std::string errmsg;
    errmsg = std::string ("File, ") + getFilename() + ", closed by PIO";
    ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
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
    PRINTMSG("calling sync");
    ESMCI_IOREGION_ENTER("PIOc_sync");
    PIOc_sync(pioFileDesc);
    ESMCI_IOREGION_EXIT("PIOc_sync");
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
      ESMCI_IOREGION_ENTER("PIOc_closefile");
    int piorc = PIOc_closefile(pioFileDesc);
      ESMCI_IOREGION_EXIT("PIOc_closefile");
    new_file = false;
    if (rc != NULL) *rc = piorc;
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
  int PIO_Handler::getIODesc(
//
// !RETURN VALUE:
//
//    int PIO IO descriptor
//
// !ARGUMENTS:
//
  int iosys,          // (in)  - PIO system handle to use
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

  int new_io_desc = (int)NULL;
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;
  new_io_desc = PIO_IODescHandler::getIODesc(iosys, arr_p, &localrc);
  if ((int)NULL == new_io_desc) {
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
  const std::string &fmtStr,             // (in)  - Optional error string
  int rc_code,                           // (in)  - Default return code on error
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

  std::stringstream errmsg;
  if (pioRetCode != PIO_NOERR) {
#if defined(ESMF_PNETCDF)
    // Log the error, assuming the error code was passed through PIO from PNetCDF
    if (!fmtStr.empty()) {
      errmsg << " " << fmtStr << ", (PIO/PNetCDF error = " <<  ncmpi_strerror (pioRetCode) << ")";
    } else {
      errmsg << " (PIO/PNetCDF error = " <<  ncmpi_strerror (pioRetCode) << ")";
    }
#elif defined(ESMF_NETCDF)
    // Log the error, assuming the error code was passed through PIO from NetCDF
    if (!fmtStr.empty()) {
      errmsg << " " << fmtStr << ", (PIO/NetCDF error = " <<  nc_strerror (pioRetCode) << ")";
    } else {
      errmsg << " (PIO/NetCDF error = " <<  nc_strerror (pioRetCode) << ")";
    }
#else
    if (!fmtStr.empty()) {
      errmsg << " " << fmtStr << ", (PIO error = " << pioRetCode << ")";
    } else {
      errmsg << " (PIO error = " << pioRetCode << ")";
    }
#endif
    // Attempt to find a corresponding ESMC error code
    switch(pioRetCode) {
#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
    case NC_EEXIST:
      localrc = ESMF_RC_FILE_CREATE;
      break;
    case NC_ENOMEM:
      localrc = ESMF_RC_MEM_ALLOCATE;
      break;
    case NC_EPERM:
      localrc = ESMF_RC_FILE_OPEN;
      break;
    default:
      localrc = ESMF_RC_NETCDF_ERROR;
      break;
#else // defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
    default:
      localrc = ESMF_RC_FILE_UNEXPECTED;
      break;
#endif
    }
  } else
    localrc = ESMF_SUCCESS;

  if ((localrc != ESMF_SUCCESS) && warn) {
    ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_WARN,
        line, file, method);
    // run through MsgFoundError in case Log tracing is enabled
    ESMC_LogDefault.MsgFoundError(ESMF_SUCCESS, errmsg,
        line, file, method, rc);
  } else {
    ESMC_LogDefault.MsgFoundError(localrc, errmsg,
        line, file, method, rc);
  }

  return (pioRetCode == PIO_NOERR);
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
    int localrc;
    PRINTMSG("calling PIOc_freedecomp");
    ESMCI_IOREGION_ENTER("PIOc_freedecomp");
    PIOc_freedecomp(ios, io_descriptor);
    ESMCI_IOREGION_EXIT("PIOc_freedecomp");
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
  int iosys,          // (in)  - PIO system handle to use
  Array *arr_p,                       // (in)  - Array for IO decompomposition
  int *newDecomp_p          // (out) - New decomposition descriptor
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
  int localDe;                      // The DE being processed
  int localDeCount;                 // The number of DEs on this PET
  int pioDofCount;                  // Number
  MPI_Offset *pioDofList;              // Local to global array map
  DistGrid *distGrid;               // The Array's associated DistGrid
  PIO_IODescHandler *handle;        // New handler object for this IO desc.

  PRINTPOS;
  // check the inputs
  if ((Array *)NULL == arr_p) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL, "- arr_p cannot be NULL",
      ESMC_CONTEXT, &localrc);
    return ESMF_RC_ARG_BAD;
  }
  if ((int *)NULL == newDecomp_p) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL,
      "- newDecomp_p cannot be NULL", ESMC_CONTEXT, &localrc);
    return ESMF_RC_ARG_BAD;
  }

  handle = new PIO_IODescHandler(iosys, arr_p);
  pioDofList = (MPI_Offset *)NULL;

  localDeCount = arr_p->getDELayout()->getLocalDeCount();
  PRINTMSG("localDeCount = " << localDeCount);
  //TODO: Remove this restriction (possibly with multiple IO descriptors)
  if (localDeCount > 1) {
    ESMC_LogDefault.Write("I/O does not support multiple DEs per PET",
                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    return ESMF_RC_NOT_IMPL;
  }

  // We need the total number of elements
  pioDofCount = 0;
  int const *localDeToDeMap = arr_p->getDistGrid()->getDELayout()->getLocalDeToDeMap();
  // NB: This loop is redundant for now, I wish I could lift the restriction.
  for (localDe = 0; localDe < localDeCount; ++localDe) {
    // consider the fact that replicated dimensions may lead to local elements in the
    // Array, that are not accounted for by actual exclusive elements in the DistGrid
    if (arr_p->getDistGrid()->getElementCountPDe()[localDeToDeMap[localDe]]>0)
      pioDofCount += arr_p->getTotalElementCountPLocalDe()[localDe];
  }

  //  PRINTMSG("(" << my_rank << "): pioDofCount = " << pioDofCount);
  try {
    // Allocate space for the DOF list
    pioDofList = new MPI_Offset[pioDofCount];

  } catch(...) {
    if ((MPI_Offset *)NULL != pioDofList) {
      // Free the DofList!
      delete[] pioDofList;
      pioDofList = (MPI_Offset *)NULL;
    }
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &localrc);
    return localrc;
  }
  // Fill in the PIO DOF list (local to global map)
  // TODO: This is where we would need to make some magic to include
  // TODO: multiple DEs.
  localDe = 0;
  if (pioDofCount>0){
    // construct the mapping of the local elements
    localrc = arr_p->constructFileMap((int64_t *) pioDofList, pioDofCount, localDe);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &localrc)) {
      delete[] pioDofList;
      pioDofList = (MPI_Offset *)NULL;
      return localrc;
    }
  }

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
    handle->basepiotype = PIO_INT;
    break;
   case ESMC_TYPEKIND_R4:
    handle->basepiotype = PIO_REAL;
    break;
   case ESMC_TYPEKIND_R8:
    handle->basepiotype = PIO_DOUBLE;
    break;
  case ESMC_TYPEKIND_I1:
  case ESMC_TYPEKIND_I2:
  case ESMC_TYPEKIND_I8:
  case ESMC_TYPEKIND_CHARACTER:
  case ESMF_C8:
  case ESMF_C16:
  case ESMC_TYPEKIND_LOGICAL:
  default:
    if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_BAD, "Unsupported typekind", ESMC_CONTEXT,
        &localrc)) {
      delete[] pioDofList;
      pioDofList = (MPI_Offset *)NULL;
      return localrc;
    }
  }

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
  PIOc_set_log_level(PIO_DEBUG_LEVEL);
#endif // ESMFIO_DEBUG
  int ddims[handle->nDims];
  for(int i=0; i<handle->nDims; i++)
      ddims[i] = handle->dims[handle->nDims - i - 1];
  // Create the decomposition
  int *rc;
  ESMCI_IOREGION_ENTER("PIOc_InitDecomp");
  PIOc_InitDecomp(iosys, handle->basepiotype, handle->nDims,
                  ddims, pioDofCount, pioDofList,
                  &(handle->io_descriptor), NULL, NULL, NULL);
  ESMCI_IOREGION_EXIT("PIOc_InitDecomp");

  PRINTMSG("after call to PIOc_initdecomp_dof");
#ifdef ESMFIO_DEBUG
  PIOc_set_log_level(0);
#endif // ESMFIO_DEBUG

  // Add the handle into the master list
  PIO_IODescHandler::activePioIoDescriptors.push_back(handle);
  // Finally, set the output handle
  *newDecomp_p = handle->io_descriptor;

  // Free the DofList!
  delete[] pioDofList;
  pioDofList = (MPI_Offset *)NULL;

  // return successfully
  return ESMF_SUCCESS;
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
  int *decomp_p             // (inout) - PIO decomp desc to free
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
  if (decomp_p == 0) {
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
      *decomp_p = (int)NULL;
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
  const int &iodesc,    // (in)  - The IO descriptor
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
  const int &iodesc,            // (in)  - The IO descriptor
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
int PIO_IODescHandler::getIODesc(
//
// !RETURN VALUE:
//
//    int Pointer to the IODescHandler matching the IO system and
//                  array
//
// !ARGUMENTS:
//
  int iosys,              // (in)  - The PIO IO system
  Array *arrayArg,                        // (in)  - The IO descriptor
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Return a pointer to an appropriate PIO IO descriptor
//
//EOPI
//-----------------------------------------------------------------------------
  int iodesc = (int)NULL; // IO descriptor
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
