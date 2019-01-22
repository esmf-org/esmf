//$1.10 2007/04/26 16:13:59 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_LocStream.C"
//==============================================================================
//
// ESMC LocStream method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt LocStream} methods declared
// in the companion file {\tt ESMCI\_LocStream.h}.  These are wrappers for the
// actual code which is implemented in F90.
//
//-----------------------------------------------------------------------------
// associated header file
#include "ESMCI_LocStream.h"

//insert any higher level, 3rd party or system includes here
#include <string>         // strlen()

// ESMF headers
#include "ESMCI_LogErr.h"
#include "ESMCI_Array.h"
#include "ESMCI_Grid.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS: LocStream object
//
// !DESCRIPTION:
//  LocStream class which provides interfaces to the Fortran implementation
//    of LocStreams.
//EOP
//-----------------------------------------------------------------------------

// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// prototypes for the fortran interface routines.
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
extern "C" {
// Prototypes of the Fortran interface functions.
  void FTN_X(f_esmf_locstreamcreate)(ESMCI::LocStream *locstreamp, int *ls_size,
                                     ESMC_IndexFlag *indexflag, ESMC_CoordSys_Flag *coordSys, int *rc);
  void FTN_X(f_esmf_locstreamgetbounds)(ESMCI::LocStream *locstreamp, int *localDe,
                                        int *cLBound, int *cUBound, int *rc);
  void FTN_X(f_esmf_locstreamaddkeyalloc)(ESMCI::LocStream *locstreamp, char *keyName,
                                          ESMC_TypeKind_Flag *typekind,
                                          int *rc, ESMCI_FortranStrLenArg nlen);
  void FTN_X(f_esmf_locstreamgetkeyarray)(ESMCI::LocStream *locstreamp, char *keyName,
                                          void *array_pointer, int *rc,
                                          ESMCI_FortranStrLenArg nlen);
  void FTN_X(f_esmf_locstreamdestroy)(ESMCI::LocStream *locstreamp, int *rc);


}
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the LocStream routines
//
//

namespace ESMCI {
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocStream::create()"
//BOP
// !IROUTINE:  ESMCI::LocStream::create - Create a new LocStream
//
// !INTERFACE:
  LocStream *LocStream::create(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMCI::LocStream object
//
// !ARGUMENTS:
                                   int ls_size,         // size of location stream
                                   ESMC_IndexFlag *indexflag,
                                   ESMC_CoordSys_Flag *coordSys,
                                   int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new LocStream.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMCI\_LocStream.h)
//
//EOP

    // prepare the location stream pointer
    ESMCI::LocStream * locstream = NULL;

    try {
      // Initialize return code. Assume routine not implemented
      int localrc = ESMC_RC_NOT_IMPL;
      if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

      locstream = new LocStream;

      FTN_X(f_esmf_locstreamcreate)(locstream, &ls_size, indexflag, coordSys, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
        return ESMC_NULL_POINTER;
      }

    } catch (std::exception &x) {
      // catch Grid exception return code
      if (x.what()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      x.what(), ESMC_CONTEXT,rc);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "UNKNOWN", ESMC_CONTEXT,rc);
      }
      return ESMC_NULL_POINTER;

    } catch(int localrc) {
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
      return ESMC_NULL_POINTER;

    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "- Caught unknown exception", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }


    if (rc != NULL) *rc = ESMF_SUCCESS;

    return locstream;

 }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocStream::getbounds()"
//BOP
// !IROUTINE:  ESMCI::LocStream::getbounds - get LocStream bounds
//
// !INTERFACE:
  int LocStream::getbounds(
//
// !RETURN VALUE:
//    int error return code

// !ARGUMENTS:
                       LocStream *locstream,
                       int localDe,
                       int *cLBound,
                       int *cUBound){

// !DESCRIPTION:
//      ESMF routine to return bounds from the LocStream.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMCI\_LocStream.h)
//
//EOP
// !REQUIREMENTS:

// Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;

    FTN_X(f_esmf_locstreamgetbounds)(locstream, &localDe, cLBound, cUBound, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                      &rc)) return rc;

    localrc = ESMF_SUCCESS;

    return localrc;

  }



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocStream::addKeyAlloc()"
//BOP
// !IROUTINE:  ESMCI::LocStream::addKeyAlloc - add key array
//
// !INTERFACE:
  int LocStream::addKeyAlloc(
//
// !RETURN VALUE:
//    int error return code

// !ARGUMENTS:
                       LocStream *locstream,
                       const char *keyName,
                       ESMC_TypeKind_Flag *typekind){

// !DESCRIPTION:
//      ESMF routine to add a key to the LocStream.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMCI\_LocStream.h)
//
//EOP
// !REQUIREMENTS:

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    int rc = ESMC_RC_NOT_IMPL;

    try {

      char * fName = NULL;
      int slen = 0;
      if(keyName != NULL){
        slen = strlen(keyName);
        fName = new char[slen];
        localrc = ESMC_CtoF90string(keyName, fName, slen);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
          delete[] fName;
          return rc;
        }
      }

      FTN_X(f_esmf_locstreamaddkeyalloc)(locstream, fName, typekind, &localrc, slen);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
        if (fName) delete[] fName;
        return rc;
      }

    } catch (std::exception &x) {
      // catch Grid exception return code
      if (x.what()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      x.what(), ESMC_CONTEXT,&rc);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "UNKNOWN", ESMC_CONTEXT,&rc);
      }
      return rc;

    } catch(int localrc) {
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,&rc);
      return rc;

    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "- Caught unknown exception", ESMC_CONTEXT, &rc);
      return rc;
    }


    localrc = ESMF_SUCCESS;

    return localrc;

  }


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocStream::getKeyArray()"
//BOP
// !IROUTINE:  ESMCI::LocStream::getKeyArray - get key array
//
// !INTERFACE:
  ESMC_Array LocStream::getKeyArray(
//
// !RETURN VALUE:
//    int error return code

// !ARGUMENTS:
                       const char *keyName,
                       int *rc){

// !DESCRIPTION:
//      ESMF routine to retrieve a key in the LocStream.
//
//      Note: this is a class helper function, not a class method
//      (see declaration in ESMCI\_LocStream.h)
//
//EOP
// !REQUIREMENTS:

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    ESMC_Array array;
    array.ptr = NULL; // initialize
    char * fName = NULL;

    try {

      int slen = 0;
      if(keyName != NULL){
        slen = strlen(keyName);
        fName = new char[slen];
        localrc = ESMC_CtoF90string(keyName, fName, slen);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
          delete[] fName;
          return array;
        }
      }

      FTN_X(f_esmf_locstreamgetkeyarray)(this, fName, (ESMCI::Array **)&(array.ptr),
                                         &localrc, slen);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        rc)) {
        if (fName) delete[] fName;
        return array;
      }

    } catch (std::exception &x) {
      // catch Grid exception return code
      if (x.what()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      x.what(), ESMC_CONTEXT,rc);
      } else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "UNKNOWN", ESMC_CONTEXT,rc);
      }

      if (fName) delete[] fName;
      return array;

    } catch(int localrc) {
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,rc);
      return array;

    } catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "- Caught unknown exception", ESMC_CONTEXT, rc);
      if (fName) delete[] fName;
      return array;
    }

    // return successfully
    if (rc) *rc = ESMF_SUCCESS;
    return array;
  }


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::LocStream::destroy()"
  //BOP
  // !IROUTINE:  ESMCI::LocStream::destroy - free a LocStream created with Create
  //
  // !INTERFACE:
  int LocStream::destroy(
                     //
                     // !RETURN VALUE:
                     //    int error return code

                     // !ARGUMENTS:
                     LocStream *locstream){

    // !DESCRIPTION:
    //      ESMF routine which destroys a LocStream object previously allocated
    //      via an ESMCI\_LocStreamCreate routine.  Define for deep classes only.
    //
    //      Note: this is a class helper function, not a class method
    //      (see declaration in ESMCI\_LocStream.h)
    //
    //EOP
    // !REQUIREMENTS:

    // Initialize return code. Assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;

    FTN_X(f_esmf_locstreamdestroy)(locstream, &localrc);

    delete locstream;
    localrc = ESMF_SUCCESS;

    return localrc;

  }

  //-----------------------------------------------------------------------------



} // namespace ESMCI

