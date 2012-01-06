// $Id: ESMCI_IO.C,v 1.14 2012/01/06 20:17:14 svasquez Exp $
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

#include "ESMCI_Macros.h"
#include "ESMCI_Container.h"
#include <ESMCI_LogErr.h>
#include <ESMF_LogMacros.inc>
#include <ESMCI_ArrayBundle.h>



extern "C" {

// Prototypes of the Fortran interface functions.
void FTN_X(f_esmf_pioread)(ESMCI::Array *array, char *file,
  char *variableName, int *timeslice, ESMC_IOFmtFlag *iofmt,  int *rc);

void FTN_X(f_esmf_piowrite)(ESMCI::Array *array, char *file,
  char *variableName, bool *append, int *timeslice, ESMC_IOFmtFlag *iofmt,
  int *rc);

}; // extern "C"



 using namespace std; 

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_IO.C,v 1.14 2012/01/06 20:17:14 svasquez Exp $";
//-------------------------------------------------------------------------

namespace ESMCI
{
//
//-------------------------------------------------------------------------
//
// construct() and destruct()
//
//-------------------------------------------------------------------------
//
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO:IO()"
//-------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMCI::IO::IO() - fill in allocated IO object
//
// !INTERFACE:
   IO::IO(
//
// !RETURN VALUE:
//     pointer to newly allocated IO
//
// !ARGUMENTS:
      ArrayBundle     **dataList,     // in
      int             dataCount,         // in
      int             *rc                // (out)
      ) { 

// !DESCRIPTION:
//    ESMF routine which fills in the contents
//
//EOPI
// !REQUIREMENTS:

    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

    try 
    {

      dataCreator = false; // Array objects were provided externally

      // invalidate the name for this ArrayBundle object in the Base class
      ESMC_BaseSetName(NULL, "IO");

    }
    catch (...) 
    {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", rc);
      return;
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;

}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::destruct()"
//BOPI
// !IROUTINE:  ESMCI::IO::destruct - release IO
//
// !INTERFACE:
      int IO::destruct(bool followCreator){
//
// !RETURN VALUE:
//    int error return code
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      {\tt ESMCI::IO::construct}, 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // ArrayBundle::destroy(&arrayList);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
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
  ArrayBundle **arrayList,                 // (in)
  int arrayCount,                          // (in)
  int *rc                                  // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt IO} object from list of Arrays.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  IO *ioclass;
  try{

  // call class constructor
  try{
    ioclass = new IO(arrayList, arrayCount, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return ESMC_NULL_POINTER;
  }catch(...){
    // allocation error
    ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::ArrayBundle.", rc);
    return ESMC_NULL_POINTER;
  }

  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", rc);
    return ESMC_NULL_POINTER;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return ioclass;
}
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
      IO **ioclass) {  // in - IO to destroy
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

  // return with errors for NULL pointer
  if (ioclass == ESMC_NULL_POINTER || *ioclass == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to ioclass", &rc);
    return rc;
  }

  // destruct IO object
  localrc = (*ioclass)->destruct();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;

  (*ioclass)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
 } // end IO::destroy
//-------------------------------------------------------------------------


#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO::read()"
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO::read - Performs a read on an IO object
//
// !INTERFACE:
      int IO::read(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
  Array *array,                   // in    - Array
  char  *file,                    // in    - name of file being read
  char  *variableName,            // in    - start of halo region
  int   *timeslice,               // in    - timeslice option
  ESMC_IOFmtFlag *iofmt           // in    - IO format flag
  ){
// !DESCRIPTION:
//      Reads an {\tt ESMC\_IO} object from file
//
//EOP
//-----------------------------------------------------------------------------
    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    // call into Fortran interface
    FTN_X(f_esmf_pioread)(array, file, variableName, timeslice,
      iofmt, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
    return rc;
  }

    // return successfully
    return (rc);

}  // end IO::read

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
  Array *array,                   // in    - Array
  char  *file,                    // in    - name of file being read
  char  *variableName,            // in    - start of halo region
  bool  *append,                  // in    - append as group
  int   *timeslice,               // in    - timeslice option
  ESMC_IOFmtFlag *iofmt           // in    - IO format flag
  ){
// !DESCRIPTION:
//      Writes an {\tt ESMC\_IO} object to file
//
//EOP
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // call into Fortran interface
  // We call it f_esmf_piowrite for now, it will be f_esmf_piowrite
  FTN_X(f_esmf_piowrite)(array, file, variableName, append, timeslice,
      iofmt, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc)) {
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  // end IO::write

//-------------------------------------------------------------------------


}  // end namespace ESMCI
