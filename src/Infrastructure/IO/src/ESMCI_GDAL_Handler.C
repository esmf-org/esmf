// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_GDAL_Handler.C"
//==============================================================================
//
// ESMC IO method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_GDAL_Handler} methods
// declared in the companion file {\tt ESMCI\_GDAL_Handler.h}
//
// This was adapted from the module ESMCI_PIO_Handler.C -- MSL
//-------------------------------------------------------------------------
#define GDAL_DEBUG_LEVEL 4
// include associated header file
#include "ESMCI_GDAL_Handler.h"

// include generic services header file
#include "ESMCI_IO_GDAL.h"

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
#include "ESMCI_LogErr.h"
#include "ESMCI_Info.h"
#include "json.hpp"
#include "ESMCI_TraceMacros.h"

#include "pio.h"

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
// private helper class for managing GDAL I/O Descriptors
//
//-------------------------------------------------------------------------
//
  class GDAL_IODescHandler {
  private:
    static std::vector<GDAL_IODescHandler *> activeGdalIoDescriptors;
    int ios;                  // GDAL IO system instance for this descriptor
    int io_descriptor;        // GDAL IO descriptor
    int nDims;                // The number of dimensions for Array IO
    int *dims;                // The shape of the Array IO
    int basegdaltype;         // GDAL version of Array data type
    int basepiotype;          // PIO version of Array data type
    Array *array_p;           // The array matched to this descriptor
    int tile;                 // The tile number in the array for this descriptor (1-based indexing)
    int arrayRank;            // The rank of array_p
    int *arrayShape;          // The shape of array_p
  public:
    GDAL_IODescHandler(int iosArg, Array *arrayArg) {
      ios = iosArg;
      io_descriptor = (int)NULL;
      array_p = arrayArg;
      nDims = 0;
      dims = (int *)NULL;
      tile = 0;
      arrayRank = 0;
      arrayShape = (int *)NULL;
    }
    // These definitions are at the end of the file
  public:
    ~GDAL_IODescHandler();
    static void finalize(void);
    static int constructGdalDecomp(int iosys, Array *arr_p, int tile,
                                  int *newDecomp_p);
    static int freeGdalDecomp(int *decomp_p);
    static int getDims(const int &iodesc,
                       int * nioDims = (int *)NULL,
                       int ** ioDims = (int **)NULL,
                       int * narrDims = (int *)NULL,
                       int ** arrDims = (int **)NULL);
    static int getIOType(const int &iodesc, int *rc = (int *)NULL);
    static int getIODesc(int iosys,
                         Array *arrayArg, int tileArg, int *rc = (int *)NULL);
  };

//
//-------------------------------------------------------------------------
//
// class variables
//
//-------------------------------------------------------------------------
//

  std::vector<int> GDAL_Handler::activeGdalInstances;
  std::vector<GDAL_IODescHandler *> GDAL_IODescHandler::activeGdalIoDescriptors;

//
//-------------------------------------------------------------------------
//
// initialize and finalize
//
//-------------------------------------------------------------------------
//


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::initialize()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::initialize
//
// !INTERFACE:
void GDAL_Handler::initialize (
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
//    Create an active, initialized GDAL instance.
//    GDAL is initialized based on the input arguments. However, if a
//    compatible GDAL iosystem is already initialized, then nothing is done.
//    This is a collective call. Input parameters are read on comp_rank=0,
//    values on other tasks are ignored. ALL PEs which will be participating
//    in future I/O calls with this instance must participate in the call.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;      // local return code
  int base;
  bool instanceFound = false;          // try to find a GDAL sys to reuse
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
    PIOc_set_log_level(GDAL_DEBUG_LEVEL);
#endif // ESMFIO_DEBUG
    if (!instanceFound) {
      PRINTMSG("Before GDALc_Init_Intracomm, num_iotasks = " << num_iotasks);
      PIOc_Init_Intracomm(comp_comm, num_iotasks,
                          stride, base, rearr, &instance);
      PRINTMSG("After GDALc_Init_Intracomm, instance = " << instance);
      // If we get here, hopefully everything is OK.
      if (instance != 0) {
#ifdef ESMFIO_FILESYSTEM_LUSTRE
          GDALc_set_hint(instance, "romio_ds_read", "disable");
          GDALc_set_hint(instance, "romio_ds_write", "disable");
#endif
#ifdef ESMFIO_FILESYSTEM_GPFS
          GDALc_set_hint(instance, "ibm_largeblock_io", "true");
#endif

        // Set the error handling to return GDAL errors
        // Just return error (error code may be different on different PEs).
        // Broadcast the error to all PEs (consistant error handling)
//        GDALc_Set_IOSystem_Error_Handling(instance, GDAL_BCAST_ERROR);
//        PRINTMSG("After GDALc_Set_IOSystem_Error_Handling");
        // Add the instance to the global list
        GDAL_Handler::activeGdalInstances.push_back(instance);
        PRINTMSG("push_back");
        localrc = ESMF_SUCCESS;
      } else {
        // Something went wrong (this really shouldn't happen)
        ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "Unknown error in GDALc_Init_Intracomm",
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
} // GDAL_Handler::initialize()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::initializeVM()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::initializeVM
//
// !INTERFACE:
int GDAL_Handler::initializeVM (void
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
//    Create an active, initialized GDAL instance.
//    GDAL is initialized based on defaults gleaned from the VM. However, if a
//    compatible GDAL iosystem is already initialized, then nothing is done.
//    This is a collective call. Input parameters are read on comp_rank=0,
//    values on other tasks are ignored. ALL PEs which will be participating
//    in future I/O calls with this instance must participate in the call.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMF_RC_NOT_IMPL;              // return code
  bool instanceFound = false;             // try to find a GDAL sys to reuse

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
          rearr = GDAL_REARR_BOX;
      }else{
          num_iotasks = numtasks > 32 ? 32:numtasks;
          stride = numtasks/num_iotasks;
          rearr = GDAL_REARR_BOX;
      }

      // Call the static function
      GDAL_Handler::initialize(my_rank, communicator, num_iotasks,
                              stride, rearr, &base, &rc);
      PRINTMSG("After initialize, rc = " << rc);
      if (ESMF_SUCCESS == rc) {
        PRINTMSG("Looking for active instance, size = " << activeGdalInstances.size());
        gdalSystemDesc = GDAL_Handler::activeGdalInstances.back();
        PRINTMSG("Fetched GDAL system descriptor, " << (void *)gdalSystemDesc);
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
} // GDAL_Handler::initializeVM()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::finalize()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::finalize
//
// !INTERFACE:
void GDAL_Handler::finalize (
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
//    Tear down all active, initialized GDAL instances.
//
//EOPI
//-----------------------------------------------------------------------------
  int gdalrc;
  int localrc = ESMF_SUCCESS;             // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTMSG("");
  try {
    // Close any open IO descriptors before turning off the instances
    GDAL_IODescHandler::finalize();
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
    // Now, close any open GDAL instances
    while(!GDAL_Handler::activeGdalInstances.empty()) {
      int instance = GDAL_Handler::activeGdalInstances.back();
//      gdalrc = GDALc_finalize(instance);
      // Even if we have an error but log and keep going to try and shut
      // down other GDAL instances
      CHECKGDALWARN(gdalrc, "Error shutting down GDAL instance",
          ESMF_RC_FILE_UNEXPECTED, localrc);

      GDAL_Handler::activeGdalInstances.pop_back();
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
} // GDAL_Handler::finalize()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::isGdalInitialized()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::isGdalInitialized
//
// !INTERFACE:
ESMC_Logical GDAL_Handler::isGdalInitialized (void
//
// !RETURN VALUE:
//
//  ESMC_Logical ESMF_TRUE if GDAL is initialized, ESMF_FALSE otherwise
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Indicate whether or not GDAL has been initialized.
//    NB: This does not guarantee that the initialization is appropriate
//        for the desired I/O operations.
//
//EOPI
//-----------------------------------------------------------------------------
  if(activeGdalInstances.empty()) {
    return ESMF_FALSE;
  } else {
    return ESMF_TRUE;
  }
} // GDAL_Handler::isGdalInitialized()
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
#define ESMC_METHOD "ESMCI::GDAL_Handler::GDAL_Handler()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::GDAL_Handler    - constructor
//
// !INTERFACE:
GDAL_Handler::GDAL_Handler(
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
//
  ESMC_IOFmt_Flag fmtArg,                 // (in)  - File format for GDAL to use
  int ntilesArg,                          // (in)  - Number of tiles in arrays handled by this object
  int *rc                                 // (out) - Error return code
  ) : IO_Handler(fmtArg, ntilesArg) {
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMCI::GDAL_Handler
//    object.
//    No error checking wrt consistency of input arguments is needed because
//    the GDAL_Handler constructor is only to be called by IO_Handler::create()
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

    // fill in the GDAL_Handler object
    gdalSystemDesc =  0;
    gdalFileDesc = new GDALDatasetH[ntilesArg];
    for (int i = 0; i < ntilesArg; ++i) {
      gdalFileDesc[i] = NULL;
    }
    gdalFileID = new int[ntilesArg];
    for (int i = 0; i < ntilesArg; ++i) {
      gdalFileID[i] = NULL;
    }
    localrc = ESMF_SUCCESS;
    new_file = new bool[ntilesArg];
    for (int i = 0; i < ntilesArg; ++i) {
      new_file[i] = false;
    }
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
} // GDAL_Handler::GDAL_Handler()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::destruct()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::destruct    - tear down GDAL handler
//
// !INTERFACE:
void GDAL_Handler::destruct (void
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
  // Make sure the file is closed (note that it's okay to call this even if the file is already closed)
  PRINTMSG(" (" << my_rank << "): closing file");
  close((int *)NULL);     // Don't care about an error, continue with cleanup
  // kill the pointer to the GDAL_Handler object
  // NB: This does not shutdown the GDAL instance, it may be reused.
  gdalSystemDesc = 0;
  // Deallocate some memory
  delete[] gdalFileDesc;
  delete[] new_file;
} // GDAL_Handler::destruct()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::arrayReadOneTileFile()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::arrayReadOneTileFile    - Read an array from a file, for the given tile
//
// !INTERFACE:
void GDAL_Handler::arrayReadOneTileFile(
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
//
  Array *arr_p,                           // (inout) - Destination of read
  int tile,                               // (in)    - Tile we are reading (1-based indexing)
  const char * const name,                // (in)    - Optional array name
  int *timeslice,                         // (in)    - Optional timeslice
  int *rc                                 // (out)   - Error return code
  ) {
//
// !DESCRIPTION:
//    Read data from field <name> from the open file for the given tile.
//    For typical single-tile arrays, this will just be called once per arrayRead, with tile=1.
//    If timeslice is not NULL, it should point to an integer representing the
//    timeslice to read from the Array.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  int gdalrc;                             // GDAL error value
  int * ioDims;                           // Array IO shape
  int nioDims;                            // Array IO rank
  int * arrDims;                          // Array shape
  int narrDims;                           // Array rank
  int iodesc;                             // GDAL IO descriptor
  GDALDatasetH filedesc;                  // GDAL file descriptor
  int fileID;
  int fielddesc;                          // GDAL field descriptor
  int basegdaltype;                       // GDAL version of Array data type
  int basepiotype;                        // PIO version of Array data type
  void *baseAddress;                      // The address of the Array IO data
  int localDE;                            // DE to use for IO
  int nField;                             // Number of field in file
  int nAtt;                               // Number of attributes in file
  int unlim;                              // Unlimited dimension ID
  std::string fieldname;                  // Default field name

  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;

  // File open?

  // Array compatible with this object?
  localrc = checkArray(arr_p);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
    return;

  filedesc = gdalFileDesc[tile-1]; // note that tile indices are 1-based
  fileID   = gdalFileID[tile-1];   // note that tile indices are 1-based

  if (filedesc == NULL) {
    PRINTMSG("DataSource is NULL X");
//    return;
  } else{
    PRINTMSG("DataSource is GOOD X");
  }

  // Get a pointer to the array data
  // Still have the one DE restriction so use localDE = 0
  localDE = 0;
  int tileOfThisDe = arr_p->getDistGrid()->getTilePLocalDe(localDE, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
    return;
  // arrlen will be the size owned locally (0 if this DE doesn't own the current tile)
  // (though note that this value isn't actually used by GDAL)
  int arrlen;
  if (tileOfThisDe == tile) {
    baseAddress = arr_p->getLocalarrayList()[localDE]->getBaseAddr();
    arrlen = 1;
  } else {
    baseAddress = NULL;
    arrlen = 0;
  }
 
    int nDims;

    iodesc = getIODesc(gdalSystemDesc, arr_p, tile, &ioDims, &nioDims,
		       &arrDims, &narrDims, &basepiotype, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
				      ESMC_CONTEXT, rc)) return;

    PRINTMSG("IODesc: iodims " << *ioDims << " nioDims " << nioDims << " arrDims " << *arrDims); 

    if (((char *)NULL != name) && (strlen(name) > 0)) {
      fieldname = name;
    } else {
      fieldname = arr_p->getName();
    }

    gdalrc = GDALc_inq_fieldid(fileID, fieldname.c_str(), &fielddesc);
    PRINTMSG("Field " << fieldname.c_str() << " FieldDesc " << fielddesc << " gdalrc " << gdalrc);
    // An error here means the variable is not in the file
    if (gdalrc != PIO_NOERR) {
      PRINTMSG("Field " << fieldname.c_str() << " not found in file");
//    if (!CHECKGDALERROR(gdalrc, errmsg, ESMF_RC_FILE_READ, (*rc))) {
      return;
    }

//>>    int frame;
//>>    if (((int *)NULL != timeslice) && (*timeslice > 0) && narrDims < nioDims) {
//>>      //
//>>      // Do not use the unlimited dim in iodesc calculation
//>>      //
//>>      int dimids[narrDims];
//>>      gdalrc = GDALc_inq_vardimid(filedesc, vardesc, dimids);
//>>      // This should never happen
//>>      const std::string errmsg = "variable " + varname + " inq_dimid failed";
//>>      if (!CHECKGDALERROR(gdalrc, errmsg, ESMF_RC_FILE_READ, (*rc))) {
//>>	return;
//>>      }

//>>      if(unlim == dimids[narrDims-1]){
//>>	narrDims = narrDims - 1;
//>>      }
//>>      for (int i=0; i<narrDims; i++){
//>>        // Note that arrDims[i] will be 0 if this DE doesn't own the current tile
//>>	arrlen *= arrDims[i];
//>>      }

//>> TBD! THIS REQUIRES SOME DISCUSSION: HOW TO DEAL WITH FILES W/O DATE/TIME DEFS? -- MSL
      int dimid_time;
      MPI_Offset time_len;
      gdalrc = GDALc_inq_timeid(fileID, &dimid_time);
//>>      if (!CHECKGDALERROR(gdalrc, "No time dimension found in file", ESMF_RC_FILE_READ, (*rc))) {
//>>        return;
//>>      }

      // Check to make sure the requested record is in the file
//>>      gdalrc = GDALc_inq_dimlen(filedesc,
//>>          dimid_time, &time_len);
//>>      if (!CHECKGDALERROR(gdalrc, "Error finding time length", ESMF_RC_FILE_READ, (*rc))) {
//>>        return;
//>>      }
//>>      if (*timeslice > time_len) {
//>>        PRINTMSG(" (" << my_rank << "): " <<
//>>                 "Timeframe is greater than that in file" <<
//>>                 getFilename(tile) << ", file time = " << time_len <<
//>>                 ", requested record = " << *timeslice);
//>>        if (ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_UNEXPECTED,
//>>            "Timeframe is greater than max in file",
//>>            ESMC_CONTEXT, rc)) {
//>>          return;
//>>        }
//>>      }
//>>      frame = (*timeslice);
//>>    } else {
//>>      frame = -1;
//>>      for (int i=0; i<narrDims; i++){
//>>        // Note that arrDims[i] will be 0 if this DE doesn't own the current tile
//>>	arrlen *= arrDims[i];
//>>      }
//>>
//>>    }
//>>    if (unlim >= 0 && frame > 0) {
//>>        PRINTMSG("calling setframe for read_darray, frame = " << frame);
//>>        GDALc_setframe(filedesc, vardesc, frame-1);
//>>    }
      PRINTPOS;
      PRINTMSG("calling read_darray, gdal type = " << basepiotype << ", address = " << baseAddress);
  // Read in the array
      
  gdalrc = PIOc_read_darray(fileID, fielddesc, iodesc,
                           arrlen, (void *)baseAddress);


  if (!CHECKGDALERROR(gdalrc, "Error reading array data", ESMF_RC_FILE_READ, (*rc))) {
    return;
  }

  // return
  if (rc != NULL) {
    *rc = localrc;
  }
} // GDAL_Handler::arrayReadOneTileFile()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::arrayWriteOneTileFile()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::arrayWriteOneTileFile    - Write an Array to a file, for the given tile
//
// !INTERFACE:
void GDAL_Handler::arrayWriteOneTileFile(
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
  Array *arr_p,                           // (in) Destination of write
  int tile,                               // (in) Tile we are writing (1-based indexing)
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
//    Write data to field <name> to the open file, for the given tile.
//    For typical single-tile arrays, this will just be called once per arrayWrite, with tile=1.
//    Calls the appropriate GDAL write_darray_<rank>_<typekind> function.
//    It is an error if this handler object does not have an open
//    GDAL file descriptor and a valid GDAL IO descriptor (these items should
//    all be in place after a successful call to GDAL_Handler::open).
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  int gdalrc;                             // GDAL error value
  int * ioDims;                           // Array IO shape
  int nioDims;                            // Array IO rank
  int * arrDims;                          // Array shape
  int narrDims;                           // Array rank
  int iodesc;                             // GDAL IO descriptor
  GDALDatasetH filedesc;                  // GDAL file descriptor
  int fileID;
  int vardesc = 0;                        // GDAL variable descriptor
  int basegdaltype;                       // GDAL version of Array data type
  void *baseAddress;                      // The address of the Array IO data
  int localDE;                            // DE to use for IO
  int ncDims[8];                          // To hold NetCDF dimensions
  int unlim = -1;                         // Unlimited dimension ID
  int timeFrame = -1;                     // ID of time dimension (>0 if used)
  int timesliceVal = -1;                  // Used time value (from timeslice)
  bool varExists = false;                 // true if varname is defined in file
  std::string varname;                    // Variable name
//>>>  if (rc != NULL) {
//>>>    *rc = ESMF_RC_NOT_IMPL;               // final return code
//>>>  }
//>>>
//>>>  PRINTPOS;
//>>>
//>>>  if ((int *)NULL != timeslice) {
//>>>    timesliceVal = *timeslice;
//>>>  }
//>>>  // File open?
//>>>  if (isOpen(tile) != ESMF_TRUE)
//>>>    if (ESMC_LogDefault.MsgFoundError (ESMF_RC_FILE_READ, "file not open",
//>>>        ESMC_CONTEXT, rc)) return;
//>>>
//>>>  // Array compatible with this object?
//>>>  localrc = checkArray(arr_p);
//>>>  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
//>>>    return;
//>>>
//>>>  filedesc = gdalFileDesc[tile-1]; // note that tile indices are 1-based
//>>>  fileID   = gdalFileID[tile-1];   // note that tile indices are 1-based
//>>>  iodesc = getIODesc(gdalSystemDesc, arr_p, tile, &ioDims, &nioDims,
//>>>      &arrDims, &narrDims, &basegdaltype, &localrc);
//>>>  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
//>>>      ESMC_CONTEXT, rc)) return;
//>>>  for (int i=0; i<narrDims; i++) {
//>>>    if (arrDims[i] < 0) {
//>>>      if (ESMC_LogDefault.MsgFoundError (ESMF_RC_INTNRL_BAD, "array dimension extent < 0",
//>>>            ESMC_CONTEXT, rc)) return;
//>>>    }
//>>>  }
//>>>
//>>>  if (dimLabels.size() > 0 && dimLabels.size() < (unsigned int)nioDims) {
//>>>    std::stringstream errmsg;
//>>>    errmsg << dimLabels.size() << " user dimension label(s) supplied, " << nioDims << " expected";
//>>>    if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_SIZE, errmsg,
//>>>            ESMC_CONTEXT, rc)) return;
//>>>  }
//>>>
//>>>
//>>>  // Get a pointer to the array data
//>>>  // Still have the one DE restriction so use localDE = 0
//>>>  localDE = 0;
//>>>  int tileOfThisDe = arr_p->getDistGrid()->getTilePLocalDe(localDE, &localrc);
//>>>  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
//>>>				    ESMC_CONTEXT, rc)) return;
//>>>  int arrlen;
//>>>  if (tileOfThisDe == tile) {
//>>>    baseAddress = arr_p->getLocalarrayList()[localDE]->getBaseAddr();
//>>>    PRINTMSG("baseAddress = 0x" << (void *)baseAddress);
//>>>    // arrlen = arr_p->getLocalarrayList()[localDE]->getByteCount();
//>>>    arrlen = 1;
//>>>    const int *counts = arr_p->getLocalarrayList()[localDE]->getCounts();
//>>>    for (int i=0; i<narrDims; i++)
//>>>      arrlen *= counts[i];
//>>>  } else {
//>>>    baseAddress = NULL;
//>>>    arrlen = 0;
//>>>  }
//>>>  PRINTMSG("arrlen = " << arrlen);
//>>>
//>>>  // Define the variable name and check the file
//>>>  if (((char *)NULL != name) && (strlen(name) > 0)) {
//>>>    varname = name;
//>>>  } else {
//>>>    varname = arr_p->getName();
//>>>  }
//>>>  PRINTMSG("varname = \"" << varname << "\"");
//>>>  if (ESMF_TRUE == isNewFile(tile)) {
//>>>    varExists = false;
//>>>  } else {
//>>>    int nVar;                           // Number of variables in file
//>>>    int nAtt;                           // Number of attributes in file
//>>>    int nfDims;                         // Number of dimensions in file
//>>>
//>>>    // We have a GDAL file, see if the variable is in there
//>>>    PRINTMSG("Looking for variable in file");
//>>>    gdalrc = GDALc_inq_fieldid(fileID, varname.c_str(), &vardesc);
//>>>    PRINTMSG("Variable in file result: " << gdalrc << vardesc);
//>>>    // This should succeed if the variable exists
//>>>    varExists = ( vardesc >= 0 );
//>>>  }
//>>>
//>>>  // Check consistency of time dimension with timeslice
//>>>  bool hasTimeDim;
//>>>
//>>>  int dimidTime;
//>>>//  GDAL_Offset timeLen;
//>>>  PRINTMSG("Checking time dimension");
//>>>
//>>>  // NetCDF does not specify which error code goes with with
//>>>  // condition so we will guess that there is no time dimension
//>>>  // on any error condition (This may be an error depending on context).
//>>>  hasTimeDim = (PIO_NOERR == gdalrc && dimidTime != -1);
//>>>  PRINTMSG("inq_dimid  = " << gdalrc);
//>>>  PRINTMSG("hasTimeDim = " << hasTimeDim);
//>>>  PRINTMSG("unlim = " << unlim);
//>>>
//>>>// 1) Create the datasource and file. <-- Should be created by openOneFile()
//>>>// 2) Define the layer
//>>>// 3) Define the field
//>>>  if (!varExists) {
//>>>    ESMCI_IOREGION_ENTER("GDALc_write_darray");
//>>>    gdalrc =  GDALc_def_field(fileID, varname.c_str(), basegdaltype, &vardesc);
//>>>    if (!CHECKGDALERROR(gdalrc, "Attempting to define GDAL/PIO vardesc for: " + varname,
//>>>        ESMF_RC_FILE_WRITE, (*rc))) {
//>>>      return;
//>>>    }
//>>>  }
//>>>
//>>>  PRINTMSG("calling write_darray, gdal type = " << basegdaltype << ", address = " << baseAddress);
//>>>  // Write the array
//>>>  ESMCI_IOREGION_ENTER("GDALc_write_darray");
//>>>  gdalrc =  PIOc_write_darray(fileID, vardesc, iodesc, arrlen,
//>>>			       (void *)baseAddress, NULL);
//>>>  if (!CHECKGDALERROR(gdalrc, "Attempting to write file",
//>>>		      ESMF_RC_FILE_WRITE, (*rc))) {
//>>>    return;
//>>>  }
//>>>  new_file[tile-1] = false;
//>>>  ESMCI_IOREGION_EXIT("GDALc_write_darray");
//>>>
//>>>  // Cleanup & return
//>>>  PRINTMSG("cleanup and return");
  if (rc != NULL) {
    *rc = ESMF_SUCCESS;
  }
} // GDAL_Handler::arrayWriteOneTileFile()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::openOneTileFile()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::openOneTileFile    - open a stream with stored filename, for the given tile
//
// !INTERFACE:
void GDAL_Handler::openOneTileFile(
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
//
  int tile,                            // (in)  - tile for which we're opening the file (1-based indexing)
  bool readonly,                       // (in)  - if false, then read/write
  int *rc                              // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Open a file for reading and/or writing for the given tile.
//    For typical single-tile arrays, this will just be called once per open, with tile=1.
//    GDAL must be initialized for this routine to succeed (ESMF_RC_INTNRL_BAD)
//    It is an error if a file is already open (ESMF_RC_FILE_OPEN)
//
//EOPI
//-----------------------------------------------------------------------------
  int iotype=0;                           // GDAL I/O type
  bool mode;                              // GDAL file open mode: write|nowrite
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  int gdalrc;                             // GDAL error value
  VM *vm = VM::getCurrent(&localrc);
  int numtasks =  vm->getPetCount();
  int petspernode = vm->getSsiMaxPetCount();


  struct iofmt_map_t {
    int esmf_iofmt;
    int gdal_fmt;
  } iofmt_map[] = {
    { ESMF_IOFMT_SHP, PIO_IOTYPE_GDAL } // This needs mods for different GIS types soon <<MSL>>
  };

  int iofmt_map_size = sizeof (iofmt_map)/sizeof (iofmt_map_t);

  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;
  if (isGdalInitialized() != ESMF_TRUE) {
    if (ESMC_LogDefault.MsgFoundError (ESMF_RC_INTNRL_BAD,
        "GDAL not initialized",
        ESMC_CONTEXT, rc)) return;
  } else if (isOpen(tile) == ESMF_TRUE) {
    if (ESMC_LogDefault.MsgFoundError (ESMF_RC_FILE_OPEN,
        "File is already open",
        ESMC_CONTEXT, rc)) return;
  } else if (gdalSystemDesc <= 0 ) {
    // Just grab last created GDAL instance for now (TBD: need way to choose)
    gdalSystemDesc = GDAL_Handler::activeGdalInstances.back();
  }

  // Translate the I/O format from ESMF to GDAL
#if !defined(ESMF_GDAL)
  if (ESMC_LogDefault.MsgFoundError(ESMF_RC_LIB_NOT_PRESENT,
      "Library for requested I/O format is not present", ESMC_CONTEXT, rc))
    return;
#endif

  int i_loop;
  for (i_loop=0; i_loop<iofmt_map_size; i_loop++) {
    if (getFormat() == iofmt_map[i_loop].esmf_iofmt) {
      iotype = iofmt_map[i_loop].gdal_fmt;
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
  bool clobberMode = false;
  if (readonly) {
    mode = false;
  }  else {
    mode = true;
  }
  // Figure out if we need to call createfile or openfile
  new_file[tile-1] = false;
  const std::string thisFilename = getFilename(tile, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
    return;
  bool file_exists = IO_Handler::fileExists(thisFilename, !readonly);
  switch(getFileStatusFlag()) {
  case ESMC_FILESTATUS_UNKNOWN:
    if (file_exists) {
      // Treat like OLD
      okToCreate = false;
    } else {
      // Treat like NEW
      okToCreate = true;
      clobberMode = false;
    }
    break;
  case ESMC_FILESTATUS_OLD:
    okToCreate = false;
    break;
  case ESMC_FILESTATUS_NEW:
    okToCreate = true;
    clobberMode = false;
    break;
  case ESMC_FILESTATUS_REPLACE:
    okToCreate = true;
    clobberMode = true;
    break;
  default:
    localrc = ESMF_RC_ARG_BAD;
    if (ESMC_LogDefault.MsgFoundError(localrc, "unknown file status argument", ESMC_CONTEXT, rc))
      return;
  }

  if (okToCreate) {
#ifdef ESMFIO_DEBUG
    std::string errmsg = "Calling GDALc_createfile";
    ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
#endif // ESMFIO_DEBUG
    // Looks like we are ready to try and create the file
    ESMCI_IOREGION_ENTER("GDALc_createfile");

//>>>    gdalrc = GDALc_createfile(gdalSystemDesc, &(gdalFileID[tile-1]),
//>>>                            &iotype, thisFilename.c_str(), mode);
    ESMCI_IOREGION_EXIT("GDALc_createfile");
    if (!CHECKGDALWARN(gdalrc, std::string("Unable to create file: ") + thisFilename,
      ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    } else {
      new_file[tile-1] = true;
      PRINTMSG("call to GDALc_createfile: success for " << thisFilename << " iotype= "<< iotype << " Mode "<< mode << " ESMF FMT "<<getFormat() );
    }
  } else {
    PRINTMSG(" calling GDALc_openfile with mode = " << mode << ", file = \"" << thisFilename << "\"");
    // Looks like we are ready to go
    ESMCI_IOREGION_ENTER("GDALc_openfile");
    gdalrc = GDALc_openfile(gdalSystemDesc, (&gdalFileID[tile-1]), (&gdalFileDesc[tile-1]),
                          &iotype, thisFilename.c_str(), mode);
    ESMCI_IOREGION_EXIT("GDALc_openfile");
//>> DEBUG:    PRINTMSG("NLayers 0: " << OGR_DS_GetLayerCount(gdalFileDesc[tile-1]));
    PRINTMSG(", called GDALc_openfile on " << thisFilename);
    if (!CHECKGDALWARN(gdalrc, std::string("Unable to open existing file: ") + thisFilename,
        ESMF_RC_FILE_OPEN, (*rc))) {
      return;
    }
  }

  // return successfully
  if (rc != NULL) {
    *rc = ESMF_SUCCESS;
  }
} // GDAL_Handler::openOneTileFile()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::attPackPut()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::attPackPut
//
// !INTERFACE:
void GDAL_Handler::attPackPut (
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  int vardesc,      // (in) - variable to write attributes into, NULL for global
  const ESMCI::Info *attPack,  // (in) - AttPack containing name/value(s) pairs
  int tile,                    // (in) - Tile number for which we are putting attributes (1-based indexing)
  int *rc                      // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Puts the Attributes and their values into the GDAL file.  If vardesc is 0, the
//    attribute will be considered a global attribute.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  int gdalrc;
  GDALDatasetH filedesc = gdalFileDesc[tile-1]; // note that tile indices are 1-based

//>>  const json &j = attPack->getStorageRef();
//>>  for (json::const_iterator it=j.cbegin(); it!=j.cend(); it++) {
//>>    if (it.key().rfind("ESMF:", 0) == 0) {
//>>      continue;
//>>    }
//>>    json jcurr;
//>>    if (it.value().is_array()) {
//>>      jcurr = it.value();
//>>    } else {
//>>      json arr = json::array();
//>>      arr.push_back(it.value());
//>>      jcurr = arr;
//>>    }
//>>    if (!(jcurr.is_array())) {
//>>      if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ATTR_WRONGTYPE, "Only JSON arrays supported. Key is: " + it.key(), ESMC_CONTEXT, rc))
//>>        return;
//>>    }
//>>    int size = (int)(jcurr.size());
//>>
//>>    // Determine if the target key is 32-bit
//>>    bool is_32bit = false;
//>>    try {
//>>      json::json_pointer jp = attPack->formatKey(it.key());
//>>      is_32bit = ESMCI::retrieve_32bit_flag(attPack->getTypeStorage(), jp, true);
//>>    }
//>>    ESMC_CATCH_ERRPASSTHRU
//>>
//>>    ESMC_TypeKind_Flag att_type = ESMCI::json_type_to_esmf_typekind(jcurr, true, is_32bit);
//>>    switch (att_type) {
//>>      case ESMC_TYPEKIND_CHARACTER: {
//>>        if (size > 1) {
//>>          if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ATTR_WRONGTYPE, "JSON arrays with size > 1 not supported for strings. Key is: " + it.key(), ESMC_CONTEXT, rc))
//>>            return;
//>>        }
//>>        const std::string value = jcurr[0];
//>>        gdalrc = GDALc_put_att_text (filedesc, vardesc,
//>>                                   it.key().c_str(), strlen(value.c_str()), value.c_str());
//>>        if (!CHECKGDALERROR(gdalrc, "Attempting to set string Attribute: " + it.key(),
//>>                           ESMF_RC_FILE_WRITE, (*rc))) return;
//>>        break;
//>>      }
//>>      case ESMC_TYPEKIND_I8: {
//>>        const std::vector<long> value = jcurr.get<std::vector<long>>();
//>>        gdalrc = GDALc_put_att_long (filedesc, vardesc,
//>>                                   it.key().c_str(), att_type, size, value.data());
//>>        if (!CHECKGDALERROR(gdalrc, "Attempting to set I8 Attribute: " + it.key(),
//>>                           ESMF_RC_FILE_WRITE, (*rc))) return;
//>>        break;
//>>      }
//>>      case ESMC_TYPEKIND_R8: {
//>>        const std::vector<double> value = jcurr.get<std::vector<double>>();
//>>        gdalrc = GDALc_put_att_double (filedesc, vardesc,
//>>                                     it.key().c_str(), att_type, size, value.data());
//>>        if (!CHECKGDALERROR(gdalrc, "Attempting to set R8 Attribute: " + it.key(),
//>>                           ESMF_RC_FILE_WRITE, (*rc))) return;
//>>        break;
//>>      }
//>>      case ESMC_TYPEKIND_I4: {
//>>        const std::vector<int> value = jcurr.get<std::vector<int>>();
//>>        gdalrc = GDALc_put_att_int (filedesc, vardesc,
//>>                                  it.key().c_str(), att_type, size, value.data());
//>>        if (!CHECKGDALERROR(gdalrc, "Attempting to set I4 Attribute: " + it.key(),
//>>                           ESMF_RC_FILE_WRITE, (*rc))) return;
//>>        break;
//>>      }
//>>      case ESMC_TYPEKIND_R4: {
//>>        const std::vector<float> value = jcurr.get<std::vector<float>>();
//>>        gdalrc = GDALc_put_att_float (filedesc, vardesc,
//>>                                    it.key().c_str(), att_type, size, value.data());
//>>        if (!CHECKGDALERROR(gdalrc, "Attempting to set R4 Attribute: " + it.key(),
//>>                           ESMF_RC_FILE_WRITE, (*rc))) return;
//>>        break;
//>>      }
//>>      default:
//>>        if (ESMC_LogDefault.MsgFoundError(ESMF_RC_ATTR_NOTSET,
//>>                                          "Attribute " + it.key() + " has unsupported value type",
//>>                                          ESMC_CONTEXT, rc)) return;
//>>    }
//>>  }

  if (rc) {*rc = ESMF_SUCCESS;}

} // GDAL_Handler::attPackPut()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::isOpen()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::isOpen    - Determine is a file is open
//
// !INTERFACE:
ESMC_Logical GDAL_Handler::isOpen(
//
// !RETURN VALUE:
//
//  ESMC_Logical ESMF_TRUE if a file is open, ESMF_FALSE otherwise
//
// !ARGUMENTS:
//
  int tile // (in) - tile number for which we check if a file is open (relevant for
            // multi-tile arrays with IO to a separate file for each array) (1-based indexing)
  ) {
//
// !DESCRIPTION:
//    Determine if a file is open returning ESMF_TRUE if a file is open,
//    ESMF_FALSE otherwise
//
//EOPI
//-----------------------------------------------------------------------------
  PRINTPOS;
  int fileid = gdalFileID[tile-1]; // note that tile indices are 1-based
  if (fileid == NULL) {
    PRINTMSG("gdalFileDesc is NULL");
    return ESMF_FALSE;
  } else if (fileid != NULL) {
    PRINTMSG("File is open");
    return ESMF_TRUE;
  } else {
    // This really should not happen, warn and clean up just in case
    std::string errmsg;
    const std::string thisFilename = getFilename(tile);
    errmsg = std::string ("File, ") + thisFilename + ", closed by GDAL";
    ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    return ESMF_FALSE;
  }
} // GDAL_Handler::isOpen()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::flushOneTileFile()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::flushOneTileFile    - Flush any pending I/O operations for this tile's file
//
// !INTERFACE:
void GDAL_Handler::flushOneTileFile(
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
//
  int tile,                               // (in)  - Tile for which we want to flush the file (1-based indexing)
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Flush any pending I/O operations for this tile's file.
//    For typical single-tile arrays, this will just be called once per flush, with tile=1.
//    It is safe to call this on a non-open file; in this case, nothing is done.
//
//EOPI
//-----------------------------------------------------------------------------

  int gdalrc;

  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  PRINTPOS;
  // Not open? No problem, just skip
  if (isOpen(tile) == ESMF_TRUE) {
    PRINTMSG("calling sync");
//>>    ESMCI_IOREGION_ENTER("GDALc_sync");
    if (gdalFileDesc[tile-1] != NULL) {
//>>      OGR_DS_Destroy(gdalFileDesc[tile-1]);
      gdalrc = PIOc_sync(gdalFileID[tile-1]);
    }
//>>    GDALc_sync(gdalFileDesc[tile-1]);
//>>    ESMCI_IOREGION_EXIT("GDALc_sync");
  }
  // return successfully
  if (rc != NULL) {
    *rc = ESMF_SUCCESS;
  }
} // GDAL_Handler::flushOneTileFile()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::closeOneTileFile()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::closeOneTileFile    - Close this tile's possibly-open file
//
// !INTERFACE:
void GDAL_Handler::closeOneTileFile(
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
//
  int tile,                               // (in)  - Tile for which we want to close the file (1-based indexing)
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Close the open file associated with this tile (if any).
//    For typical single-tile arrays, this will just be called once per close, with tile=1.
//    It is NOT an error if no file is open
//
//EOPI
//-----------------------------------------------------------------------------

  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

//>>  PRINTPOS;
//>>  // Not open? No problem, just skip
  if (isOpen(tile) == ESMF_TRUE) {
    ESMCI_IOREGION_ENTER("GDALc_closefile");
    int gdalrc = PIOc_closefile(gdalFileID[tile-1]);
    ESMCI_IOREGION_EXIT("GDALc_closefile");
    gdalFileID[tile-1] = 0;
    gdalFileDesc[tile-1] = NULL;
    new_file[tile-1] = false;
    if (rc != NULL) *rc = gdalrc;
  }

  // return successfully
  if (rc != NULL) {
    *rc = ESMF_SUCCESS;
  }
} // GDAL_Handler::closeOneTileFile()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::getIODesc()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::getIODesc - Find or create an IO descriptor
//
// !INTERFACE:
  int GDAL_Handler::getIODesc(
//
// !RETURN VALUE:
//
//    int GDAL IO descriptor
//
// !ARGUMENTS:
//
  int iosys,          // (in)  - GDAL system handle to use
  Array *arr_p,                       // (in)  - Array for IO decompomposition
  int tile,                           // (in)  - Tile number in array (1-based indexing)
  int ** ioDims,                      // (out) - Array shape for IO
  int *nioDims,                       // (out) - Rank of Array IO
  int ** arrDims,                     // (out) - Array shape for IO
  int *narrDims,                      // (out) - Rank of Array IO
  int *basepiotype,                  // (out) - Data type for IO
  int *rc                             // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Find or create an appropriate GDAL I/O Descriptor and return it.
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
  new_io_desc = GDAL_IODescHandler::getIODesc(iosys, arr_p, tile, &localrc);
  if ((int)NULL == new_io_desc) {
    PRINTMSG("calling constructGdalDecomp");
    localrc = GDAL_IODescHandler::constructGdalDecomp(iosys,
                                                    arr_p, tile, &new_io_desc);
    PRINTMSG("constructGdalDecomp call complete" << ", localrc = " << localrc);
  }
  if ((ioDims != (int **)NULL) || (nioDims != (int *)NULL) ||
      (arrDims != (int **)NULL) || (narrDims != (int *)NULL)) {
    int niodimArg;
    int *iodimsArg;
    int narrdimArg;
    int *arrdimsArg;

    localrc = GDAL_IODescHandler::getDims(new_io_desc, &niodimArg, &iodimsArg,
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
    *basepiotype = GDAL_IODescHandler::getIOType(new_io_desc, &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
  }
  if (rc != NULL) {
    *rc = localrc;
  }
  return new_io_desc;
} // GDAL_Handler::getIODesc()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_Handler::CheckGDALError()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_Handler::CheckGDALError
//
// !INTERFACE:
bool GDAL_Handler::CheckGDALError(
//
// !RETURN VALUE:
//
//    bool true if it is OK to continue (false = error condition)
//
// !ARGUMENTS:
//
  int gdalRetCode,                       // (in)  - Return code to check
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
//    Log an error (if an error condition is indicated by gdalRetCode)
//
//EOPI
//-----------------------------------------------------------------------------
//>>  int localrc = ESMF_SUCCESS;;
//>>  if (rc != NULL) {
//>>    *rc = ESMF_RC_NOT_IMPL;               // final return code
//>>  }
//>>
//>>  std::stringstream errmsg;
//>>  if (gdalRetCode != GDAL_NOERR) {
//>>#if defined(ESMF_PNETCDF)
//>>    // Log the error, assuming the error code was passed through GDAL from PNetCDF
//>>    if (!fmtStr.empty()) {
//>>      errmsg << " " << fmtStr << ", (GDAL/PNetCDF error = " <<  ncmpi_strerror (gdalRetCode) << ")";
//>>    } else {
//>>      errmsg << " (GDAL/PNetCDF error = " <<  ncmpi_strerror (gdalRetCode) << ")";
//>>    }
//>>#elif defined(ESMF_NETCDF)
//>>    // Log the error, assuming the error code was passed through GDAL from NetCDF
//>>    if (!fmtStr.empty()) {
//>>      errmsg << " " << fmtStr << ", (GDAL/NetCDF error = " <<  nc_strerror (gdalRetCode) << ")";
//>>    } else {
//>>      errmsg << " (GDAL/NetCDF error = " <<  nc_strerror (gdalRetCode) << ")";
//>>    }
//>>#else
//>>    if (!fmtStr.empty()) {
//>>      errmsg << " " << fmtStr << ", (GDAL error = " << gdalRetCode << ")";
//>>    } else {
//>>      errmsg << " (GDAL error = " << gdalRetCode << ")";
//>>    }
//>>#endif
//>>    // Attempt to find a corresponding ESMC error code
//>>    switch(gdalRetCode) {
//>>#if defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
//>>    case NC_EEXIST:
//>>      localrc = ESMF_RC_FILE_CREATE;
//>>      break;
//>>    case NC_ENOMEM:
//>>      localrc = ESMF_RC_MEM_ALLOCATE;
//>>      break;
//>>    case NC_EPERM:
//>>      localrc = ESMF_RC_FILE_OPEN;
//>>      break;
//>>    default:
//>>      localrc = ESMF_RC_NETCDF_ERROR;
//>>      break;
//>>#else // defined(ESMF_NETCDF) || defined(ESMF_PNETCDF)
//>>    default:
//>>      localrc = ESMF_RC_FILE_UNEXPECTED;
//>>      break;
//>>#endif
//>>    }
//>>  } else
//>>    localrc = ESMF_SUCCESS;
//>>
//>>  if ((localrc != ESMF_SUCCESS) && warn) {
//>>    ESMC_LogDefault.Write(errmsg, ESMC_LOGMSG_WARN,
//>>        line, file, method);
//>>    // run through MsgFoundError in case Log tracing is enabled
//>>    ESMC_LogDefault.MsgFoundError(ESMF_SUCCESS, errmsg,
//>>        line, file, method, rc);
//>>  } else {
//>>    ESMC_LogDefault.MsgFoundError(localrc, errmsg,
//>>        line, file, method, rc);
//>>  }
//>>
  return (gdalRetCode == PIO_NOERR);
} // GDAL_Handler::CheckGDALError()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_IODescHandler::~GDAL_IODescHandler()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_IODescHandler::~GDAL_IODescHandler
//
// !INTERFACE:
GDAL_IODescHandler::~GDAL_IODescHandler (
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Tear down active, initialized GDAL IO Descriptor and free memory
//
//EOPI
//-----------------------------------------------------------------------------
    int localrc;
//>>    PRINTMSG("calling GDALc_freedecomp");
//>>    ESMCI_IOREGION_ENTER("GDALc_freedecomp");
//>>    GDALc_freedecomp(ios, io_descriptor);
//>>    ESMCI_IOREGION_EXIT("GDALc_freedecomp");
//>>    if (dims != (int *)NULL) {
//>>        delete[] dims;
//>>        dims = (int *)NULL;
//>>    }
//>>    if (arrayShape != (int *)NULL) {
//>>        delete[] arrayShape;
//>>        arrayShape = (int *)NULL;
//>>    }
//>>    array_p = (Array *)NULL;
} // GDAL_IODescHandler::~GDAL_IODescHandler()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_IODescHandler::finalize()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_IODescHandler::finalize
//
// !INTERFACE:
void GDAL_IODescHandler::finalize (
//
// !RETURN VALUE:
//
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Tear down all active, initialized GDAL IO Descriptors.
//
//EOPI
//-----------------------------------------------------------------------------
  GDAL_IODescHandler *handle;

  while(!GDAL_IODescHandler::activeGdalIoDescriptors.empty()) {
    handle = GDAL_IODescHandler::activeGdalIoDescriptors.back();
    delete handle; // Shuts down descriptor
    handle = (GDAL_IODescHandler *)NULL;
    GDAL_IODescHandler::activeGdalIoDescriptors.pop_back();
  }
} // GDAL_IODescHandler::finalize()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_IODescHandler::constructGdalDecomp()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_IODescHandler::constructGdalDecomp - New Decomposition
//
// !INTERFACE:
int GDAL_IODescHandler::constructGdalDecomp(
//
// !RETURN VALUE:
//
//    int return code
//
// !ARGUMENTS:
//
  int iosys,                // (in)  - GDAL system handle to use
  Array *arr_p,             // (in)  - Array for IO decompomposition
  int tile,                 // (in)  - Tile number in array (1-based indexing)
  int *newDecomp_p          // (out) - New decomposition descriptor
  ) {
//
// !DESCRIPTION:
//    Gather the necessary information the input array and call GDAL_initdecomp.
//    The result is a new decomposition descriptor which is used in the
//     GDAL read/write calls.
//
//EOPI
//-----------------------------------------------------------------------------

  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;   // local return code
  int rc;
  int localDe;                      // The DE being processed
  int localDeCount;                 // The number of DEs on this PET
  int gdalDofCount;                  // Number
  MPI_Offset *gdalDofList;              // Local to global array map
  DistGrid *distGrid;               // The Array's associated DistGrid
  GDAL_IODescHandler *handle;        // New handler object for this IO desc.

  PRINTPOS;
  // check the inputs
  if ((Array *)NULL == arr_p) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL, "- arr_p cannot be NULL",
      ESMC_CONTEXT, &localrc);
    return ESMF_RC_ARG_BAD;
  }
  if (tile < 1) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_BAD, "- tile must be >= 1",
      ESMC_CONTEXT, &localrc);
    return localrc;
  }
  if ((int *)NULL == newDecomp_p) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL,
      "- newDecomp_p cannot be NULL", ESMC_CONTEXT, &localrc);
    return ESMF_RC_ARG_BAD;
  }

  handle = new GDAL_IODescHandler(iosys, arr_p);
  gdalDofList = (MPI_Offset *)NULL;

  localDeCount = arr_p->getDELayout()->getLocalDeCount();
  PRINTMSG("localDeCount = " << localDeCount);
  //TODO: Remove this restriction (possibly with multiple IO descriptors)
  if (localDeCount > 1) {
    ESMC_LogDefault.Write("I/O does not support multiple DEs per PET",
                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    return ESMF_RC_NOT_IMPL;
  }

  // TODO: To support multiple DEs per PE, we would need to extend this to be an array
  bool thisDeIsThisTile = false;

  // We need the total number of elements
  gdalDofCount = 0;
  int const *localDeToDeMap = arr_p->getDistGrid()->getDELayout()->getLocalDeToDeMap();
  // NB: This loop is redundant for now, I wish I could lift the restriction.
  for (localDe = 0; localDe < localDeCount; ++localDe) {
    // consider the fact that replicated dimensions may lead to local elements in the
    // Array, that are not accounted for by actual exclusive elements in the DistGrid
    int tileOfThisDe = arr_p->getDistGrid()->getTilePLocalDe(localDe, &rc);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc))
      return localrc;
    if (tileOfThisDe == tile) {
      // TODO: As noted above, to support multiple DEs per PE, we would need to extend this to be an array
      thisDeIsThisTile = true;
    }
    if (thisDeIsThisTile && arr_p->getDistGrid()->getElementCountPDe()[localDeToDeMap[localDe]]>0)
      gdalDofCount += arr_p->getTotalElementCountPLocalDe()[localDe];
  }

  PRINTMSG("gdalDofCount = " << gdalDofCount);
  try {
    // Allocate space for the DOF list
    gdalDofList = new MPI_Offset[gdalDofCount];

  } catch(...) {
    if ((MPI_Offset *)NULL != gdalDofList) {
      // Free the DofList!
      delete[] gdalDofList;
      gdalDofList = (MPI_Offset *)NULL;
    }
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &localrc);
    return localrc;
  }
  // Fill in the GDAL DOF list (local to global map)
  // TODO: This is where we would need to make some magic to include multiple DEs.
  // TODO: (Particular care may be needed in the multi-tile case, where some DEs on the
  // TODO: current PE may be part of the current tile, while others are not.
  // TODO: For now, with one DE, we can assume that, if gdalDofCount>0, then this DE
  // TODO: corresponds to the current tile.)
  localDe = 0;
  if (gdalDofCount>0){
    // construct the mapping of the local elements
    localrc = arr_p->constructFileMap((int64_t *) gdalDofList, gdalDofCount, localDe);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &localrc)) {
      delete[] gdalDofList;
      gdalDofList = (MPI_Offset *)NULL;
      return localrc;
    }
  }

#if 0
    std::cout << " gdalDofList = [";
    for (int i = 0; i < gdalDofCount; i++) {
      std::cout << " " << gdalDofList[i]
                << ((i == (gdalDofCount - 1)) ? ' ' : ',');
    }
    std::cout << "]" << std::endl;
#endif // 0
  // Get TKR info
  switch(arr_p->getTypekind()) {
  case ESMC_TYPEKIND_I4:
    handle->basegdaltype = OFTInteger;
    handle->basepiotype  = PIO_INT;
    break;
   case ESMC_TYPEKIND_R4:
//    handle->basegdaltype = OFTReal;
//    break;
   case ESMC_TYPEKIND_R8:
    handle->basegdaltype = OFTReal;
    handle->basepiotype  = PIO_DOUBLE;
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
      delete[] gdalDofList;
      gdalDofList = (MPI_Offset *)NULL;
      return localrc;
    }
  }

  distGrid = arr_p->getDistGrid();
  const int *minIndexPDimPTile = distGrid->getMinIndexPDimPTile();
  const int *maxIndexPDimPTile = distGrid->getMaxIndexPDimPTile();

  handle->tile = tile;
// NB: Is this part of the restrictions on Array I/O?
//    nDims = arr_p->getRank();
  handle->nDims = distGrid->getDimCount();
  // Make sure dims is not used
  if (handle->dims != (int *)NULL) {
    delete handle->dims;
    handle->dims = (int *)NULL;
  }
  handle->dims = new int[handle->nDims];
  // Step through the distGrid dimensions, getting the size of the dimension. (This is the
  // size of the full array across all PEs.)
  for (int i = 0; i < handle->nDims; i++) {
    handle->dims[i] = (maxIndexPDimPTile[((tile - 1) * handle->nDims) + i] -
                       minIndexPDimPTile[((tile - 1) * handle->nDims) + i] + 1);
  }

  handle->arrayRank = arr_p->getRank();
  if (handle->arrayShape != (int *)NULL) {
    delete handle->arrayShape;
    handle->arrayShape = (int *)NULL;
  }
  handle->arrayShape = new int[handle->arrayRank];
  // Get the size of each dimension owned locally.
  if (thisDeIsThisTile) {
    const int *totalLBound = arr_p->getTotalLBound();
    const int *totalUBound = arr_p->getTotalUBound();
    for (int i = 0; i < handle->arrayRank; ++i) {
      // TODO: This is another place that would need to be generalized to handle more than
      // one DE per PE: totalLBound and totalUBound are dimensioned
      // [redDimCount*ssiLocalDeCount], so the below expression (which doesn't include the
      // current DE count) would need to be adjusted to handle multiple DEs.
      handle->arrayShape[i] = (totalUBound[i] - totalLBound[i] + 1);
    }
  } else {
    // This DE is for some other tile, so as far as this tile is concerned, we own 0 elements
    for (int i = 0; i < handle->arrayRank; ++i) {
      handle->arrayShape[i] = 0;
    }
  }

#ifdef ESMFIO_DEBUG
  {
    char shapestr[64];
    for (int i = 0; i < handle->arrayRank; i++) {
      sprintf((shapestr + (5 * i)), " %03d%c", handle->arrayShape[i],
              (((handle->arrayRank - 1) == i) ? ' ' : ','));
    }
    PRINTMSG(", IODesc shape = [" << shapestr << "], calling gdal_initdecomp");

    char dimstr[64];
    for (int i = 0; i < handle->nDims; i++) {
        sprintf((dimstr + (5 * i)), " %03d%c", handle->dims[i],
                (((handle->nDims - 1) == i) ? ' ' : ','));
    }
    PRINTMSG(", IODesc dims = [" << dimstr << "]");
  }
#endif // ESMFIO_DEBUG
  int ddims[handle->nDims];
  for(int i=0; i<handle->nDims; i++)
      ddims[i] = handle->dims[handle->nDims - i - 1];
  // Create the decomposition
  ESMCI_IOREGION_ENTER("GDALc_InitDecomp");
  PIOc_InitDecomp(iosys, handle->basepiotype, handle->nDims,
                  ddims, gdalDofCount, gdalDofList,
                  &(handle->io_descriptor), NULL, NULL, NULL);
  ESMCI_IOREGION_EXIT("GDALc_InitDecomp");

  PRINTMSG("after call to GDALc_initdecomp_dof");

  // Add the handle into the master list
  GDAL_IODescHandler::activeGdalIoDescriptors.push_back(handle);
  // Finally, set the output handle
  *newDecomp_p = handle->io_descriptor;

  // Free the DofList!
  delete[] gdalDofList;
  gdalDofList = (MPI_Offset *)NULL;

  // return successfully
  return ESMF_SUCCESS;
} // GDAL_IODescHandler::constructGdalDecomp()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_IODescHandler::freeGdalDecomp()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_IODescHandler::freeGdalDecomp - Delete Decomposition
//
// !INTERFACE:
int GDAL_IODescHandler::freeGdalDecomp(
//
// !RETURN VALUE:
//
//    int return code
//
// !ARGUMENTS:
//
  int *decomp_p             // (inout) - GDAL decomp desc to free
  ) {
//
// !DESCRIPTION:
//    Free a GDAL I/O decomposition structure and remove it from the
//    active instances.
//
//EOPI
//-----------------------------------------------------------------------------

  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;   // local return code
  GDAL_IODescHandler *handle;        // Temp handler for finding descriptor
  std::vector<GDAL_IODescHandler *>::iterator it;
  bool foundHandle = false;

  // check the inputs
  if (decomp_p == 0) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL,
      "- newDecomp_p cannot be NULL", ESMC_CONTEXT, &localrc);
    return ESMF_RC_ARG_BAD;
  }

  // Look for newDecomp_p in the active handle instances
  for (it = GDAL_IODescHandler::activeGdalIoDescriptors.begin();
       it < GDAL_IODescHandler::activeGdalIoDescriptors.end(); ++it) {
    handle = *it;
    if (*decomp_p == handle->io_descriptor) {
      foundHandle = true;
      delete handle;
      handle = (GDAL_IODescHandler *)NULL;
      *decomp_p = (int)NULL;
      break;
    }
  }

  // If we didn't find the handle, that is bad
  if (foundHandle) {
    localrc = ESMF_SUCCESS;
  } else {
    ESMC_LogDefault.Write("GDAL IO descriptor not found or freed",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    localrc = ESMF_RC_MEM_DEALLOCATE;
  }

  return localrc;
} // GDAL_IODescHandler::freeGdalDecomp()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_IODescHandler::getDims()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_IODescHandler::getDims
//
// !INTERFACE:
int GDAL_IODescHandler::getDims(
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

  std::vector<GDAL_IODescHandler *>::iterator it;
  PRINTPOS;
  PRINTMSG("iodesc " << iodesc );
  for (it = GDAL_IODescHandler::activeGdalIoDescriptors.begin();
       it < GDAL_IODescHandler::activeGdalIoDescriptors.end();
       it++) {

    PRINTMSG("iodesc " << iodesc << ", io_desc " << (*it)->io_descriptor );
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

} // GDAL_IODescHandler::getDims()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_IODescHandler::getIOType()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_IODescHandler::getIOType
//
// !INTERFACE:
int GDAL_IODescHandler::getIOType(
//
// !RETURN VALUE:
//
//    int IO type (e.g., GDAL_int)
//
// !ARGUMENTS:
//
  const int &iodesc,            // (in)  - The IO descriptor
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Return the IO type descriptor (e.g., GDAL_int)
//
//EOPI
//-----------------------------------------------------------------------------
  int iotype = -1;
  int localrc = ESMF_RC_NOT_FOUND;        // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }
  std::vector<GDAL_IODescHandler *>::iterator it;
  PRINTPOS;
  for (it = GDAL_IODescHandler::activeGdalIoDescriptors.begin();
       it < GDAL_IODescHandler::activeGdalIoDescriptors.end();
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
} // GDAL_IODescHandler::getIOType()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GDAL_IODescHandler::getIODesc()"
//BOPI
// !IROUTINE:  ESMCI::GDAL_IODescHandler::getIODesc
//
// !INTERFACE:
int GDAL_IODescHandler::getIODesc(
//
// !RETURN VALUE:
//
//    int Pointer to the IODescHandler matching the IO system and
//                  array
//
// !ARGUMENTS:
//
  int iosys,              // (in)  - The GDAL IO system
  Array *arrayArg,        // (in)  - The IO descriptor
  int tileArg,            // (in)  - Tile number in array (1-based indexing)
  int *rc                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Return a pointer to an appropriate GDAL IO descriptor
//
//EOPI
//-----------------------------------------------------------------------------
  int iodesc = (int)NULL; // IO descriptor
  int localrc = ESMF_RC_NOT_FOUND;        // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  std::vector<GDAL_IODescHandler *>::iterator it;
  for (it = GDAL_IODescHandler::activeGdalIoDescriptors.begin();
       it < GDAL_IODescHandler::activeGdalIoDescriptors.end();
       ++it) {
    if ((iosys == (*it)->ios) && (arrayArg == (*it)->array_p) && (tileArg == (*it)->tile)) {
      iodesc = (*it)->io_descriptor;
      localrc = ESMF_SUCCESS;
    }
  }

  // return successfully
  if (rc != NULL) {
    *rc = localrc;
  }

  return iodesc;
} // GDAL_IODescHandler::getIODesc()
//-----------------------------------------------------------------------------

}  // end namespace ESMCI
