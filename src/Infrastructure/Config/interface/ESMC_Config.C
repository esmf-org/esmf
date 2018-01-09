// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Config.C"
//==============================================================================
//
// ESMC Config method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Config} methods declared
// in the companion file ESMC_Config.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMC_Config.h"

// include higher level, 3rd party or system headers
#include <string.h>

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_Util.h"
#include "ESMCI_Arg.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char* const version = "$Id$";
//-----------------------------------------------------------------------------

// class declaration type -> this should be moved into ESMCI namespace
class ESMCI_Config{
  ESMCI::F90ClassHolder fortranclass;
};

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// prototypes for the fortran interface routines.
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
extern "C" {

  void FTN_X(f_esmf_configcreate)(ESMCI_Config* config, int* rc);

  void FTN_X(f_esmf_configdestroy)(ESMCI_Config* config, int* rc);

  void FTN_X(f_esmf_configloadfile)(ESMCI_Config* config, const char* fname,
    int* unique, int* rc, ESMCI_FortranStrLenArg flen);

  void FTN_X(f_esmf_configfindlabel)(ESMCI_Config* config, const char* label,
    ESMC_Logical *isPresent, int* rc,
    ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_confignextline)(ESMCI_Config* config, ESMC_Logical* tableEnd, int* rc);

//  void FTN_X(f_esmf_configgetchar)(ESMCI_Config* config, char* value, char* label,
//    char* dvalue, int* rc, ESMCI_FortranStrLenArg, ESMCI_FortranStrLenArg llen,
//    ESMCI_FortranStrLenArg);

  void FTN_X(f_esmf_configgetlen)(ESMCI_Config* config, int* wordCount,
    const char* label, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetlennolabel)(ESMCI_Config* config, int* wordCount,
    int* rc );

  void FTN_X(f_esmf_configgetdim)(ESMCI_Config* config, int* lineCount,
    int* columnCount, const char* label, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetdimnolabel)(ESMCI_Config* config, int* lineCount,
    int* columnCount, int* rc);

  void FTN_X(f_esmf_configvalidate)(ESMCI_Config* config, char* options, int* rc,
    ESMCI_FortranStrLenArg olen);

  void FTN_X(f_esmf_configvalidatenooptions)(ESMCI_Config* config, int* rc);

  //
  // Functions for ConfigGetAttribute interface
  //
  /*
  ===========================================================================
  functions commented out until optional arguments (in the f77 "glue" code
  are removed!
  ===========================================================================

  void FTN_X(f_esmf_configgetstring)(ESMCI_Config* config, char* value,
    char* label, char* dvalue, int* rc, ESMCI_FortranStrLenArg vlen,
    ESMCI_FortranStrLenArg llen, ESMCI_FortranStrLenArg dlen);

  void FTN_X(f_esmf_configgetinti4)(ESMCI_Config* config, ESMC_I4* value,
    char* label, ESMC_I4* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetinti8)(ESMCI_Config* config, ESMC_I8* value,
    char* label, ESMC_I8* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetfloatr4)(ESMCI_Config* config, ESMC_R4* value,
    char* label, ESMC_R4* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetfloatr8)(ESMCI_Config* config, ESMC_R8* value,
    char* label, ESMC_R8* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetlogical)(ESMCI_Config* config, int* value,
    char* label, int* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetintsi4)(ESMCI_Config* config, int* count,
    ESMC_I4* value, char* label, ESMC_I4* dvalue, int* rc,
    ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetintsi8)(ESMCI_Config* config, int* count,
    ESMC_I8* value, char* label, ESMC_I8* dvalue, int* rc,
    ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetfloatsr4)(ESMCI_Config* config, int* count,
    ESMC_R4* value, char* label, ESMC_R4* dvalue, int* rc,
    ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetfloatsr8)(ESMCI_Config* config, int* count,
    ESMC_R8* value, char* label, ESMC_R8* dvalue, int* rc,
    ESMCI_FortranStrLenArg llen);

  void FTN_X(f_esmf_configgetlogicals)(ESMCI_Config* config, int* count,
    int* value, char* label, int* dvalue, int* rc, ESMCI_FortranStrLenArg llen);

  ===========================================================================
  End of commented out function prototypes
  ===========================================================================
  */

}; // end prototypes for fortran interface




//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// ESMC_Config API
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
extern "C" {


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigCreate()"
//BOP
// !IROUTINE:  ESMC_ConfigCreate - Create a Config object
//
// !INTERFACE:
ESMC_Config ESMC_ConfigCreate(
//
// !RETURN VALUE:
//  ESMC_Config*  to newly allocated ESMC_Config
//
// !ARGUMENTS:
//
  int* rc                    // out - return code
  ) {
//
// !DESCRIPTION:
//  Creates an {\tt ESMC\_Config} for use in subsequent calls.
//
//   The arguments are:
//   \begin{description}
//   \item [{[rc]}]
//     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // local return code

  // Initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  ESMC_Config config;
  config.ptr = NULL;  // initialize
  
  // allocate the new Config object
  ESMCI_Config* configp;
  try{
    configp = new ESMCI_Config;
  }catch(...){
     // allocation error
     ESMC_LogDefault.MsgAllocError("for new ESMCI_Config.", ESMC_CONTEXT, rc);  
     return config;
  }

  // call into Fortran interface
  FTN_X(f_esmf_configcreate)(configp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) {
    delete configp;
    return config;
  }

  // set return code for this branch
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  
  // final return
  config.ptr = (void*)configp;
  return config;

} // end ESMC_ConfigCreate
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigDestroy()"
//BOP
// !IROUTINE:  ESMC_ConfigDestroy - Destroy a Config object
//
// !INTERFACE:
int ESMC_ConfigDestroy(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS:
  ESMC_Config* config        // in  - ESMC_Config to destroy
  ) {
//
// !DESCRIPTION:
//  Destroys the {\tt config} object.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                     // return code
  int localrc;                // local return code

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER || config->ptr == ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // typecase into ESMCI type
  ESMCI_Config *configp = (ESMCI_Config*)(config->ptr);

  // call into Fortran interface
  FTN_X(f_esmf_configdestroy)(configp, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    return rc;
  }
  delete configp;
  config->ptr = ESMC_NULL_POINTER;

  // set return code for this branch
  rc = ESMF_SUCCESS;

  // final return
  return rc;

} // end ESMC_ConfigDestroy
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigLoadFile()"
//BOP
// !IROUTINE:  ESMC_ConfigLoadFile - Load resource file into memory
//
// !INTERFACE:
int ESMC_ConfigLoadFile(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS: 
  ESMC_Config config,        // in  - ESMC_Config object
  const char* name,          // in  - file name
  ...                        // optional argument list: (unique)
  ) {
//
// !DESCRIPTION:
//  Resource file with {\tt filename} is loaded into memory.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [filename]
//     Configuration file name.
//   \item [{[delayout]}]
//     {\tt ESMC\_DELayout} associated with this {\tt config} object.
//     **NOTE: {\tt ESMC\_DELayout} is not yet enabled
//   \item [{[unique]}]
//     If specified as true, uniqueness of labels are checked and 
//     error code set if duplicates found (optional).
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                     // return code
  int localrc;                // local return code
  const int numArg = 1;       // number of optional arguments
  ESMCI_ArgList argPtr;       // optional argument list pointer
  ESMCI_ArgID argID;          // optional argument list id
  int  unique;                // optional unique argument
  int* uniquep = NULL;        // pointer to unique

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // Initialize unique
  unique = 0;
  uniquep = &unique;

  // return with errors for NULL pointer
  if (config.ptr == ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
    return rc;
  }

  // typecase into ESMCI type
  ESMCI_Config *configp = (ESMCI_Config*)(config.ptr);

  // check the optional argument list:
  ESMCI_ArgStart(argPtr,name);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgUniqueID:
        ESMCI_ArgGetInt(argPtr);
        break;
      default:
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT,
        &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // parse the optional argument list:
  ESMCI_ArgStart(argPtr,name);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgUniqueID:
        unique = ESMCI_ArgGetInt(argPtr);
        break;
      default:
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT,
        &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // call Fortran interface
  FTN_X(f_esmf_configloadfile)(configp, name, uniquep, &localrc,
     strlen (name));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc))
    return rc;

  // set return code for this branch
  rc = localrc;

  // final return
  return rc;

} // end ESMC_ConfigLoadFile
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigFindLabel()"
//BOP
// !IROUTINE:  ESMC_ConfigFindLabel - Find a label
//
// !INTERFACE:
int ESMC_ConfigFindLabel(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//  If the {\tt isPresent} argument is set to {\tt NULL},
//  and the label is not found, an error is returned.
//
// !ARGUMENTS: 
  ESMC_Config config,        // in  - ESMC_Config object
  const char* label,         // in  - label
  int *isPresent             // out - label presence flag
  ) {
//
// !DESCRIPTION:
//  Finds the {\tt label} (key) in the {\tt config} file. 
//
//  Since the search is done by looking for a word in the 
//  whole resource file, it is important to use special 
//  conventions to distinguish labels from other words 
//  in the resource files. The DAO convention is to finish 
//  line labels by : and table labels by ::.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [label]
//     Identifying label. 
//   \item [isPresent]
//     If non-NULL, the address specified is given a value of 1 if the
//     label is found, and 0 when the label is not found.
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                     // return code
  int localrc;                // local return code

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config.ptr == ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
    return rc;
  }

  // typecase into ESMCI type
  ESMCI_Config *configp = (ESMCI_Config*)(config.ptr);

  // call Fortran interface
  ESMC_Logical Fpresent;
  FTN_X(f_esmf_configfindlabel)(configp, label,
      &Fpresent, &localrc, strlen (label));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    if (isPresent != NULL)
      *isPresent = 0;
    return rc;
  }
  if (isPresent != NULL)
    *isPresent = Fpresent == ESMF_TRUE;

  // set return code for this branch
  rc = ESMF_SUCCESS;

  // final return
  return rc;

} // end ESMC_ConfigFindLabel
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigNextLine()"
//BOP
// !IROUTINE:  ESMC_ConfigNextLine - Find next line
//
// !INTERFACE:
int ESMC_ConfigNextLine(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS:
  ESMC_Config config,       // in
  int *tableEnd){           // out
//
// !DESCRIPTION:
//  Selects the next line (for tables).
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [{[tableEnd]}]
//     End of table mark (::) found flag.  Returns 1 when found, and 0 when
//     not found.
//   \end{description} //
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                     // return code
  int localrc;                // local return code
  ESMC_Logical localtableEnd; // local tableEnd flag

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config.ptr == ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
    return rc;
  }

  // typecase into ESMCI type
  ESMCI_Config *configp = (ESMCI_Config*)(config.ptr);

  // call Fortran interface
  FTN_X(f_esmf_confignextline)(configp, &localtableEnd, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) {
    return rc;
  }

  *tableEnd = localtableEnd == ESMF_TRUE;

  // set return code for this branch
  rc = ESMF_SUCCESS;

  // final return
  return rc;

} // end ESMC_ConfigNextLine
//-----------------------------------------------------------------------------


/* More than 1 optional argument.
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigGetChar()"
//BOP
// !IROUTINE:  ESMC_ConfigGetChar - Get a character
//
// !INTERFACE:
//      int ESMC_ConfigGetChar(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS: 
//  ESMC_Config config,        // in  - ESMC_Config object
//  char* value,               // out - value
//  ...                        // optional argument list: (label, dvalue)
//  ) {
//
// !DESCRIPTION:
//  Gets a character {\tt value} from the {\tt config} object.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [value]
//     Returned value. 
//   \item [{[label]}]
//     Identifying label (optional).
//   \item [{[dvalue]}]
//     Default value if {\tt label} is not found in {\tt config} object (optional).
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
//  // local vars
//  int rc;                     // return code
//  int localrc;                // local return code
//  const int numArg = 2;       // number of optional arguments
//  ESMCI_ArgList argPtr;       // optional argument list pointer
//  ESMCI_ArgID argID;          // optional argument list id
//  char* label = NULL;         // optional label argument
//  char  dvalue;               // optional default value argument
//  char* dvaluep = NULL;       // pointer to dvalue
//  char* fLabel = NULL;
//  int llen = 0;

//  // Initialize return code; assume routine not implemented
//  rc = ESMC_RC_NOT_IMPL;
//  localrc = ESMC_RC_NOT_IMPL;


//  // return with errors for NULL pointer
//  if (config.ptr == ESMC_NULL_POINTER) {
//    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
//      "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
//    return rc;
//  }

//  // typecase into ESMCI type
//  ESMCI_Config *configp = (ESMCI_Config*)(config.ptr);

//  // check the optional argument list
//  ESMCI_ArgStart(argPtr,value);
//  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
//    switch ( argID ) {
//      case ESMCI_ConfigArgLabelID:
//        ESMCI_ArgGetString(argPtr);
//        break;
//      case ESMCI_ConfigArgDvalueID:
//        ESMCI_ArgGetChar(argPtr);
//        break;
//      default:
//        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT, &rc);
//        return rc;
//    }
//  }
//  ESMCI_ArgEnd(argPtr);

//  // parse the optional argument list
//  ESMCI_ArgStart(argPtr,value);
//  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
//    switch ( argID ) {
//      case ESMCI_ConfigArgLabelID:
//        label = ESMCI_ArgGetString(argPtr);
//        break;
//      case ESMCI_ConfigArgDvalueID:
//        dvalue  = ESMCI_ArgGetChar(argPtr);
//        dvaluep = &dvalue;
//        break;
//      default:
//        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT, &rc);
//        return rc;
//    }
//  }
//  ESMCI_ArgEnd(argPtr);

//  // convert label to fortran string
//  if (label != NULL) {
//    llen = strlen(label);
//    fLabel = new char[llen];
//    localrc = ESMC_CtoF90string(label, fLabel, llen);
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//      delete[] fLabel;
//      return rc;
//    }
//  }

//  // call Fortran interface
//  FTN_X(f_esmf_configgetchar)(configp, value, fLabel, dvaluep, &localrc, 1, llen,
//    1);
//  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//    if (fLabel != NULL) {delete[] fLabel;}
//    return rc;
//  }
//  
//  // clean up
//  if (fLabel != NULL) {delete[] fLabel;}

//  // set return code for this branch
//  rc = ESMF_SUCCESS;

//  // final return
//  return rc;

//} // end ESMC_ConfigGetChar

*/
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigGetLen()"
//BOP
// !IROUTINE:  ESMC_ConfigGetLen - Get the length of the line in words
//
// !INTERFACE:
      int ESMC_ConfigGetLen(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS: 
  ESMC_Config config,        // in  - ESMC_Config object
  int* wordCount,            // out - length of line in words
  ...                        // optional argument list: (label)
  ) {
//
// !DESCRIPTION:
//  Gets the length of the line in words by counting words
//  disregarding types.  Returns the word count as an integer.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [wordCount]
//     Returned number of words in the line. 
//   \item [{[label]}]
//     Identifying label.  If not specified, use the current line (optional).
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                     // return code
  int localrc;                // local return code
  const int numArg = 1;       // number of optional arguments
  ESMCI_ArgList argPtr;       // optional argument list pointer
  ESMCI_ArgID argID;          // optional argument list id
  char* label = NULL;         // optional label argument

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config.ptr == ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
    return rc;
  }

  // typecase into ESMCI type
  ESMCI_Config *configp = (ESMCI_Config*)(config.ptr);

  // check the optional argument list
  ESMCI_ArgStart(argPtr,wordCount);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgLabelID:
        ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT,
          &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // parse the optional argument list
  ESMCI_ArgStart(argPtr,wordCount);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgLabelID:
        label = ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT,
          &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  if (label != NULL) {
    // call into Fortran interface
    FTN_X(f_esmf_configgetlen)(configp, wordCount, label, &localrc,
        strlen (label));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
      return rc;

  }else{

   // call into Fortran interface without the label optional argument
    FTN_X(f_esmf_configgetlennolabel)(configp, wordCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc)) {
      return rc;
    }

  }
  // set return code for this branch
  rc = ESMF_SUCCESS;

  // final return
  return rc;

} // end ESMC_ConfigGetLen
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigGetDim()"
//BOP
// !IROUTINE:  ESMC_ConfigGetDim - Get table sizes
//
// !INTERFACE:
      int ESMC_ConfigGetDim(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS: 
  ESMC_Config config,        // in  - ESMC_Config object
  int* lineCount,            // out - number of lines in table
  int* columnCount,          // out - number of columns in table
  ...                        // optional argument list: (label)
  ) {
//
// !DESCRIPTION:
//  Returns the number of lines in the table in {\tt lineCount} and 
//  the maximum number of words in a table line in {\tt columnCount}.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [lineCount]
//     Returned number of lines in the table. 
//   \item [columnCount]
//     Returned maximum number of words in a table line. 
//   \item [{[label]}]
//     Identifying label (optional).
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                     // return code
  int localrc;                // local return code
  const int numArg = 1;       // number of optional arguments
  ESMCI_ArgList argPtr;       // optional argument list pointer
  ESMCI_ArgID argID;          // optional argument list id
  char* label = NULL;         // optional label argument

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config.ptr == ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
    return rc;
  }

  // typecase into ESMCI type
  ESMCI_Config *configp = (ESMCI_Config*)(config.ptr);

  // check the optional argument list
  ESMCI_ArgStart(argPtr,columnCount);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgLabelID:
        ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT,
          &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // parse the optional argument list
  ESMCI_ArgStart(argPtr,columnCount);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgLabelID:
        label = ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT, &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // convert label to fortran string
  if (label != NULL) {

    // call Fortran interface
    FTN_X(f_esmf_configgetdim)(configp, lineCount, columnCount, label,
        &localrc, strlen (label));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
      return rc;

  }else{

    FTN_X(f_esmf_configgetdimnolabel)(configp, lineCount, columnCount, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
      return rc;
  }

  // set return code for this branch
  rc = ESMF_SUCCESS;

  // final return
  return rc;

} // end ESMC_ConfigGetDim
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigValidate()"
//BOP
// !IROUTINE:  ESMC_ConfigValidate - Validate a Config object
//
// !INTERFACE:
int ESMC_ConfigValidate(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//  Equals {\tt ESMF\_RC\_ATTR\_UNUSED} if any unused attributes are found
//  with option "unusedAttributes" below.
//
// !ARGUMENTS: 
  ESMC_Config config,        // in  - ESMC_Config object
  ...                        // optional argument list: (options)
  ) {
//
// !DESCRIPTION:
//   Checks whether a {\tt config} object is valid.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item[{[options]}]
//     If none specified:  simply check that the buffer is not full and the
//       pointers are within range (optional).
//     "unusedAttributes" - Report to the default logfile all attributes not
//       retrieved via a call to {\tt ESMC\_ConfigGetAttribute()} or
//       {\tt ESMC\_ConfigGetChar()}.  The attribute name (label) will be
//       logged via {\tt ESMC\_LogErr} with the WARNING log message type.
//       For an array-valued attribute, retrieving at least one value via
//       {\tt ESMC\_ConfigGetAttribute()} or {\tt ESMC\_ConfigGetChar()}
//       constitutes being "used."
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                     // return code
  int localrc;                // local return code
  const int numArg = 1;       // number of optional arguments
  ESMCI_ArgList argPtr;       // optional argument list pointer
  ESMCI_ArgID argID;          // optional argument list id
  char* options = NULL;       // optional options argument

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config.ptr == ESMC_NULL_POINTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
    return rc;
  }

  // typecase into ESMCI type
  ESMCI_Config *configp = (ESMCI_Config*)(config.ptr);

  // check the optional argument list
  ESMCI_ArgStart(argPtr,config);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgOptionsID:
        ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT, &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // parse the optional argument list
  ESMCI_ArgStart(argPtr,config);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgOptionsID:
        options = ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT, &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  if (options != NULL) {

    // call Fortran interface
    FTN_X(f_esmf_configvalidate)(configp, options, &localrc, strlen (options));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
      return rc;

  }else{
        FTN_X(f_esmf_configvalidatenooptions)(configp,  &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
      return rc;

  }

  // set return code for this branch
  rc = ESMF_SUCCESS;

  // final return
  return rc;

} // end ESMC_ConfigValidate
//-----------------------------------------------------------------------------


/*  

    ===========================================================================
    interfaces with more than one optional argument are commented out
    for now. Their implementation _must_not_ involve use of optional arguments
    in the f77 "glue" code.
    ===========================================================================
*/

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigGetAttribute()"
//BOP
// !IROUTINE:  ESMC_ConfigGetAttribute - Get an attribute
//
// !INTERFACE:
// int ESMC_ConfigGetAttribute(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS: 
// ESMC_Config config,        // in  - ESMC_Config object
// void* value,               // out - value
// ESMC_TypeKind_Flag tk,          // in  - tk
// ...                        // optional argument list: (count, label, dvalue)
// ) {
//
// !DESCRIPTION:
//  Gets an attribute {\tt value} from the {\tt config} object.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [value]
//     Returned value(s). 
//   \item [tk]
//     Data type/kind of returned value(s). 
//   \item [count]
//     Number of returned values expected (optional).  If the attribute is a
//     string (i.e., {\tt tk = ESMC_TYPEKIND_CHARACTER}, then {\tt count} is
//     not used and only one value is returned.
//   \item [{[label]}]
//     Identifying label (optional).
//   \item [{[dvalue]}]
//     Default value if {\tt label} is not found in {\tt config} object (optional).
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
// // local vars
// int rc;                     // return code
// int localrc;                // local return code
// const int numArg = 3;       // number of optional arguments
// ESMCI_ArgList argPtr;       // optional argument list pointer
// ESMCI_ArgID argID;          // optional argument list id
// int  count = 1;             // optional count argument
// char* label = NULL;         // optional label argument
// void*   dvaluep = NULL;     // pointer to optional dvalue_x
// ESMC_I4 dvalue_i4;          // optional default integer i4 value argument
// ESMC_I8 dvalue_i8;          // optional default integer i8 value argument
// ESMC_R4 dvalue_r4;          // optional default real r4 value argument
// ESMC_R8 dvalue_r8;          // optional default real r8 value argument
// int     dvalue_l;           // optional default logical value argument
// char*   dvalue_s;           // optional default string value argument
// char* fLabel = NULL;        // fortran label string
// char* fDvalue = NULL;       // fortran default string value
// char  fValue[ESMF_MAXSTR];  // fortran string value
// int llen = 0;               // length of label string
// int vlen = ESMF_MAXSTR;     // length of fortran string value
// int dlen = 0;               // length of default string value
// int i;

// // Initialize return code; assume routine not implemented
// rc = ESMC_RC_NOT_IMPL;
// localrc = ESMC_RC_NOT_IMPL;

// // return with errors for NULL pointer
// if (config.ptr == ESMC_NULL_POINTER) {
//   ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
//     "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
//   return rc;
// }

// // typecase into ESMCI type
// ESMCI_Config *configp = (ESMCI_Config*)(config.ptr);

// // check for supported type/kind:
// switch ( tk ) {
//   case ESMC_TYPEKIND_I4:
//     break;
//   case ESMC_TYPEKIND_I8:
//     break;
//   case ESMC_TYPEKIND_R4:
//     break;
//   case ESMC_TYPEKIND_R8:
//     break;
//   case ESMC_TYPEKIND_LOGICAL:
//     break;
//   case ESMC_TYPEKIND_CHARACTER:
//     break;
//   default:
//     ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
//                     "- unknown data type/kind", ESMC_CONTEXT, &rc);
//     return rc;
// } // end switch (tk)

// // check the optional argument list:
// ESMCI_ArgStart(argPtr,tk);
// while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
//   switch ( argID ) {
//     case ESMCI_ConfigArgCountID:
//       ESMCI_ArgGetInt(argPtr);
//       break;
//     case ESMCI_ConfigArgLabelID:
//       ESMCI_ArgGetString(argPtr);
//       break;
//     case ESMCI_ConfigArgDvalueID:
//       switch ( tk ) {
//         case ESMC_TYPEKIND_I4:
//           ESMCI_ArgGetI4(argPtr);
//           break;
//         case ESMC_TYPEKIND_I8:
//           ESMCI_ArgGetI8(argPtr);
//           break;
//         case ESMC_TYPEKIND_R4:
//           ESMCI_ArgGetR4(argPtr);
//           break;
//         case ESMC_TYPEKIND_R8:
//           ESMCI_ArgGetR8(argPtr);
//           break;
//         case ESMC_TYPEKIND_LOGICAL:
//           ESMCI_ArgGetInt(argPtr);
//           break;
//         case ESMC_TYPEKIND_CHARACTER:
//           ESMCI_ArgGetString(argPtr);
//           break;
//       } // end switch (tk)
//       break;
//     default:
//       ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT, &rc);
//       return rc;
//   } // end switch (argID)
// } // end while (argID)
// ESMCI_ArgEnd(argPtr);

// // parse the optional argument list:
// ESMCI_ArgStart(argPtr,tk);
// while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
//   switch ( argID ) {
//     case ESMCI_ConfigArgCountID:
//       count = ESMCI_ArgGetInt(argPtr);
//       break;
//     case ESMCI_ConfigArgLabelID:
//       label = ESMCI_ArgGetString(argPtr);
//       break;
//     case ESMCI_ConfigArgDvalueID:
//       switch ( tk ) {
//         case ESMC_TYPEKIND_I4:
//           dvalue_i4 = ESMCI_ArgGetI4(argPtr);
//           dvaluep = &dvalue_i4;
//           break;
//         case ESMC_TYPEKIND_I8:
//           dvalue_i8 = ESMCI_ArgGetI8(argPtr);
//           dvaluep = &dvalue_i8;
//           break;
//         case ESMC_TYPEKIND_R4:
//           dvalue_r4 = ESMCI_ArgGetR4(argPtr);
//           dvaluep = &dvalue_r4;
//           break;
//         case ESMC_TYPEKIND_R8:
//           dvalue_r8 = ESMCI_ArgGetR8(argPtr);
//           dvaluep = &dvalue_r8;
//           break;
//         case ESMC_TYPEKIND_LOGICAL:
//           dvalue_l = ESMCI_ArgGetInt(argPtr);
//           dvaluep = &dvalue_l;
//           break;
//         case ESMC_TYPEKIND_CHARACTER:
//           dvalue_s = ESMCI_ArgGetString(argPtr);
//           dvaluep = &dvalue_s;
//           break;
//       } // end switch (tk)
//       break;
//     default:
//       ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT, &rc);
//       return rc;
//   } // end switch (argID)
// } // end while (argID)
// ESMCI_ArgEnd(argPtr);

// // convert label to fortran string
// if (label != NULL) {
//   llen = strlen(label);
//   fLabel = new char[llen];
//   localrc = ESMC_CtoF90string(label, fLabel, llen);
//   if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//     delete[] fLabel;
//     return rc;
//   }
// }

// // branch according to data type/kind
// switch ( tk ) {

//   // ESMC_TYPEKIND_I4
//   case ESMC_TYPEKIND_I4: {

//     // call Fortran interface
//     if (count > 1) {
//       FTN_X(f_esmf_configgetintsi4)(configp, &count, (ESMC_I4*)value,
//                  fLabel, (ESMC_I4*)dvaluep, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configgetinti4)(configp, (ESMC_I4*)value,
//                  fLabel, (ESMC_I4*)dvaluep, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }

//     // set return code for this branch
//     rc = ESMF_SUCCESS;

//     break;
//   } // end ESMC_TYPEKIND_I4

//   // ESMC_TYPEKIND_I8
//   case ESMC_TYPEKIND_I8: {

//     // call Fortran interface
//     if (count > 1) {
//       FTN_X(f_esmf_configgetintsi8)(configp,  &count, (ESMC_I8*)value,
//                  fLabel, (ESMC_I8*)dvaluep, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configgetinti8)(configp, (ESMC_I8*)value,
//                  fLabel, (ESMC_I8*)dvaluep, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }

//     // set return code for this branch
//     rc = ESMF_SUCCESS;

//     break;
//   } // end ESMC_TYPEKIND_I8

//   // ESMC_TYPEKIND_R4
//   case ESMC_TYPEKIND_R4: {

//     // call Fortran interface
//     if (count > 1) {
//       FTN_X(f_esmf_configgetfloatsr4)(configp, &count, (ESMC_R4*)value,
//                  fLabel, (ESMC_R4*)dvaluep, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configgetfloatr4)(configp, (ESMC_R4*)value,
//                  fLabel, (ESMC_R4*)dvaluep, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }

//     // set return code for this branch
//     rc = ESMF_SUCCESS;

//     break;
//   } // end ESMC_TYPEKIND_R4

//   // ESMC_TYPEKIND_R8
//   case ESMC_TYPEKIND_R8: {

//     // call Fortran interface
//     if (count > 1) {
//       FTN_X(f_esmf_configgetfloatsr8)(configp, &count, (ESMC_R8*)value,
//                  fLabel, (ESMC_R8*)dvaluep, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configgetfloatr8)(configp, (ESMC_R8*)value,
//                  fLabel, (ESMC_R8*)dvaluep, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }

//     // set return code for this branch
//     rc = ESMF_SUCCESS;

//     break;
//   } // end ESMC_TYPEKIND_R8

//   // ESMC_TYPEKIND_LOGICAL
//   case ESMC_TYPEKIND_LOGICAL: {

//     // call Fortran interface
//     if (count > 1) {
//       FTN_X(f_esmf_configgetlogicals)(configp, &count, (int*)value,
//                  fLabel, (int*)dvaluep, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configgetlogical)(configp, (int*)value,
//                  fLabel, (int*)dvaluep, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }

//     // set return code for this branch
//     rc = ESMF_SUCCESS;

//     break;
//   } // end ESMC_TYPEKIND_LOGICAL

//   // ESMC_TYPEKIND_CHARACTER
//   case ESMC_TYPEKIND_CHARACTER: {

//     // convert dvalue_s to fortran string
//     if (dvaluep != NULL) {
//       dlen = strlen(dvalue_s);
//       fDvalue = new char[dlen];
//       localrc = ESMC_CtoF90string(dvalue_s, fDvalue, dlen);
//       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//         if (fLabel != NULL) {delete[] fLabel;}
//         delete[] fDvalue;
//         return rc;
//       }
//     }

//     // call Fortran interface
//     FTN_X(f_esmf_configgetstring)(configp, fValue,
//                fLabel, fDvalue, &localrc, vlen, llen, dlen);

//     // handle special case of internal non-zero non-failure return code
//     // that occurs when line is blank and default is provided.
//     if (localrc == -1 && dvaluep != NULL) localrc = ESMF_SUCCESS;

//     // check local return code
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       if (fDvalue != NULL) {delete[] fDvalue;}
//       return rc;
//     }

//     // assign fValue_s to value
//     localrc = ESMC_F90toCstring(fValue, ESMF_MAXSTR, (char*)value, ESMF_MAXSTR);
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       if (fDvalue != NULL) {delete[] fDvalue;}
//       return rc;
//     }

//     // clean up
//     if (fDvalue != NULL) {delete[] fDvalue;}

//     // set return code for this branch
//     rc = ESMF_SUCCESS;

//     break;
//   } // end ESMC_TYPEKIND_CHARACTER

// } // end branch on data type/kind

// // clean up
// if (fLabel != NULL) {delete[] fLabel;}

// // final return
// return rc;

//} // end ESMC_ConfigGetAttribute
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigSetAttribute()"
//BOP
// !IROUTINE:  ESMC_ConfigSetAttribute - Set an attribute
//
// !INTERFACE:
// int ESMC_ConfigSetAttribute(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS: 
// ESMC_Config config,        // in  - ESMC_Config object
// void* value,               // in  - value
// ESMC_TypeKind_Flag tk,          // in  - tk
// ...                        // optional argument list: (count, label)
// ) {
//
// !DESCRIPTION:
//  Sets an attribute {\tt value} in the {\tt config} object.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [value]
//     Attribute value(s) to set. 
//   \item [tk]
//     Data type/kind of attribute value(s). 
//   \item [count]
//     Number of attribute values to set (optional). 
//   \item [{[label]}]
//     Identifying label (optional).
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
// // local vars
// int rc;                     // return code
// int localrc;                // local return code
// const int numArg = 2;       // number of optional arguments
// ESMCI_ArgList argPtr;       // optional argument list pointer
// ESMCI_ArgID argID;          // optional argument list id
// int  count = 1;             // optional count argument
// char* label = NULL;         // optional label argument
// char* fLabel = NULL;        // fortran label string
// char  fValue[ESMF_MAXSTR];  // fortran string value
// int llen = 0;               // length of label string
// int vlen = 0;               // length of input string value
// int i;

// // Initialize return code; assume routine not implemented
// rc = ESMC_RC_NOT_IMPL;
// localrc = ESMC_RC_NOT_IMPL;

// // return with errors for NULL pointer
// if (config.ptr == ESMC_NULL_POINTER) {
//   ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
//     "- Not a valid pointer to Config", ESMC_CONTEXT, &rc);
//   return rc;
// }

// // typecase into ESMCI type
// ESMCI_Config *configp = (ESMCI_Config*)(config.ptr);

// // check for supported type/kind:
// switch ( tk ) {
//   case ESMC_TYPEKIND_I4:
//     break;
//   case ESMC_TYPEKIND_I8:
//     break;
//   case ESMC_TYPEKIND_R4:
//     break;
//   case ESMC_TYPEKIND_R8:
//     break;
//   case ESMC_TYPEKIND_LOGICAL:
//     break;
//   case ESMC_TYPEKIND_CHARACTER:
//     break;
//   default:
//     ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
//                     "- unknown data type/kind", ESMC_CONTEXT, &rc);
//     return rc;
// } // end switch (tk)

// // check the optional argument list:
// ESMCI_ArgStart(argPtr,tk);
// while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
//   switch ( argID ) {
//     case ESMCI_ConfigArgCountID:
//       ESMCI_ArgGetInt(argPtr);
//       break;
//     case ESMCI_ConfigArgLabelID:
//       ESMCI_ArgGetString(argPtr);
//       break;
//     default:
//       ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT, &rc);
//       return rc;
//   } // end switch (argID)
// } // end while (argID)
// ESMCI_ArgEnd(argPtr);

// // parse the optional argument list:
// ESMCI_ArgStart(argPtr,tk);
// while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
//   switch ( argID ) {
//     case ESMCI_ConfigArgCountID:
//       count = ESMCI_ArgGetInt(argPtr);
//       break;
//     case ESMCI_ConfigArgLabelID:
//       label = ESMCI_ArgGetString(argPtr);
//       break;
//     default:
//       ESMC_LogDefault.MsgFoundError(ESMC_RC_OPTARG_BAD, "", ESMC_CONTEXT, &rc);
//       return rc;
//   } // end switch (argID)
// } // end while (argID)
// ESMCI_ArgEnd(argPtr);

// // convert label to fortran string
// if (label != NULL) {
//   llen = strlen(label);
//   fLabel = new char[llen];
//   localrc = ESMC_CtoF90string(label, fLabel, llen);
//   if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//     delete[] fLabel;
//     return rc;
//   }
// }

// // branch according to data type/kind
// switch ( tk ) {

//   // ESMC_TYPEKIND_I4
//   case ESMC_TYPEKIND_I4: {

//     // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
//     if (count > 1) {
//       FTN_X(f_esmf_configsetintsi4)(configp, (ESMC_I4*)value, &count,
//                  fLabel, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configsetinti4)(configp, (ESMC_I4*)value,
//                  fLabel, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }
********************************************/

//     // set return code for this branch
//     rc = ESMC_RC_NOT_IMPL;

//     break;
//   } // end ESMC_TYPEKIND_I4

//   // ESMC_TYPEKIND_I8
//   case ESMC_TYPEKIND_I8: {

//     // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
//     if (count > 1) {
//       FTN_X(f_esmf_configsetintsi8)(configp, (ESMC_I8*)value, &count,
//                  fLabel, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configsetinti8)(configp, (ESMC_I8*)value,
//                  fLabel, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }
********************************************/

//     // set return code for this branch
//     rc = ESMC_RC_NOT_IMPL;

//     break;
//   } // end ESMC_TYPEKIND_I8

//   // ESMC_TYPEKIND_R4
//   case ESMC_TYPEKIND_R4: {

//     // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
//     if (count > 1) {
//       FTN_X(f_esmf_configsetfloatsr4)(configp, (ESMC_R4*)value, &count,
//                  fLabel, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configsetfloatr4)(configp, (ESMC_R4*)value,
//                  fLabel, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }
********************************************/

//     // set return code for this branch
//     rc = ESMC_RC_NOT_IMPL;

//     break;
//   } // end ESMC_TYPEKIND_R4

//   // ESMC_TYPEKIND_R8
//   case ESMC_TYPEKIND_R8: {

//     // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
//     if (count > 1) {
//       FTN_X(f_esmf_configsetfloatsr8)(configp, (ESMC_R8*)value, &count,
//                  fLabel, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configsetfloatr8)(configp, (ESMC_R8*)value,
//                  fLabel, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }
********************************************/

//     // set return code for this branch
//     rc = ESMC_RC_NOT_IMPL;

//     break;
//   } // end ESMC_TYPEKIND_R8

//   // ESMC_TYPEKIND_LOGICAL
//   case ESMC_TYPEKIND_LOGICAL: {

//     // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
//     if (count > 1) {
//       FTN_X(f_esmf_configsetlogicals)(configp, (int*)value, &count,
//                  fLabel, &localrc, llen);
//     } else {
//       FTN_X(f_esmf_configsetlogical)(configp, (int*)value,
//                  fLabel, &localrc, llen);
//     }
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }
********************************************/

//     // set return code for this branch
//     rc = ESMC_RC_NOT_IMPL;

//     break;
//   } // end ESMC_TYPEKIND_LOGICAL

//   // ESMC_TYPEKIND_CHARACTER
//   case ESMC_TYPEKIND_CHARACTER: {

//     // convert value to fortran string
//     if ((char*)value != NULL) {
//       vlen = strlen((char*)value);
//       localrc = ESMC_CtoF90string((char*)value, fValue, vlen);
//       if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//         if (fLabel != NULL) {delete[] fLabel;}
//         return rc;
//       }
//     }

//     // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
//     FTN_X(f_esmf_configsetstring)(configp, fValue,
//                fLabel, &localrc, vlen, llen);

//     // check local return code
//     if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
//       if (fLabel != NULL) {delete[] fLabel;}
//       return rc;
//     }
********************************************/

//     // set return code for this branch
//     rc = ESMC_RC_NOT_IMPL;

//     break;
//   } // end ESMC_TYPEKIND_CHARACTER

// } // end branch on data type/kind

// // clean up
// if (fLabel != NULL) {delete[] fLabel;}

// // final return
// return rc;

//} // end ESMC_ConfigSetAttribute
//-----------------------------------------------------------------------------


}; //end extern "C"

