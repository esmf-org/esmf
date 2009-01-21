// $Id: ESMC_Config.C,v 1.6.2.1 2009/01/21 21:25:20 cdeluca Exp $
//
// Earth System Modeling Framework
// copyright 2002-2009, University Corporation for Atmospheric Research, 
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

// required includes
#include "ESMCI_Config.h"
#include "ESMCI_Arg.h"
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"
#include <string.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char* const version = 
             "$Id: ESMC_Config.C,v 1.6.2.1 2009/01/21 21:25:20 cdeluca Exp $";
//-----------------------------------------------------------------------------


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
ESMC_Config* ESMC_ConfigCreate(
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

  // allocate the new Config object
  ESMC_Config* config;
  try{
    config = new ESMC_Config;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_Config.", rc);  
     return ESMC_NULL_POINTER;
  }

  // call into Fortran interface
  FTN(f_esmf_configcreate)(config, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)) {
    delete config;
    config = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }

  // set return code for this branch
  if (rc!=NULL) *rc = ESMF_SUCCESS;

  // final return
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
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // call into Fortran interface
  FTN(f_esmf_configdestroy)(config, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    return rc;
  }
  delete config;
  config = ESMC_NULL_POINTER;

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
  ESMC_Config* config,       // in  - ESMC_Config object
  char* name,                // in  - file name
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
  int nlen;
  char* fName = NULL;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // check the optional argument list:
  ESMCI_ArgStart(argPtr,name);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgUniqueID:
        ESMCI_ArgGetInt(argPtr);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
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
        uniquep = &unique;
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // convert file name to fortran string
  nlen = strlen(name);
  fName = new char[nlen];
  localrc = ESMC_CtoF90string(name, fName, nlen);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    delete[] fName;
    return rc;
  }

  // call Fortran interface
  FTN(f_esmf_configloadfile)(config, fName, uniquep, &localrc, nlen);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    delete[] fName;
    return rc;
  }

  // clean up
  delete[] fName;

  // set return code for this branch
  rc = ESMF_SUCCESS;

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
//  Equals -1 if buffer could not be loaded, -2 if label not found,
//  and -3 if invalid operation with index.
//
// !ARGUMENTS: 
  ESMC_Config* config,       // in  - ESMC_Config object
  char* label                // in  - label
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
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int rc;                     // return code
  int localrc;                // local return code
  int llen;
  char* fLabel = NULL;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // convert label to fortran string
  llen = strlen(label);
  fLabel = new char[llen];
  localrc = ESMC_CtoF90string(label, fLabel, llen);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    delete[] fLabel;
    return rc;
  }

  // call Fortran interface
  FTN(f_esmf_configfindlabel)(config, fLabel, &localrc, llen);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    delete[] fLabel;
    return rc;
  }

  // clean up
  delete[] fLabel;

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
  ESMC_Config* config,       // in  - ESMC_Config object
  ...                        // optional argument list: (tableEnd)
  ) {
//
// !DESCRIPTION:
//  Selects the next line (for tables).
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [{[tableEnd]}]
//     If specifed as {\tt TRUE}, end of table mark (::) is checked.
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
  int* tableEndp = NULL;      // pointer to tableEnd

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // check the optional argument list:
  ESMCI_ArgStart(argPtr,config);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgTableEndID:
        ESMCI_ArgGet(argPtr,int*);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // parse the optional argument list:
  ESMCI_ArgStart(argPtr,config);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgTableEndID:
        tableEndp = ESMCI_ArgGet(argPtr,int*);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // call Fortran interface
  FTN(f_esmf_confignextline)(config, tableEndp, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    return rc;
  }

  // set return code for this branch
  rc = ESMF_SUCCESS;

  // final return
  return rc;

} // end ESMC_ConfigNextLine
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigGetChar()"
//BOP
// !IROUTINE:  ESMC_ConfigGetChar - Get a character
//
// !INTERFACE:
      int ESMC_ConfigGetChar(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS: 
  ESMC_Config* config,       // in  - ESMC_Config object
  char* value,               // out - value
  ...                        // optional argument list: (label, dvalue)
  ) {
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
  // local vars
  int rc;                     // return code
  int localrc;                // local return code
  const int numArg = 2;       // number of optional arguments
  ESMCI_ArgList argPtr;       // optional argument list pointer
  ESMCI_ArgID argID;          // optional argument list id
  char* label = NULL;         // optional label argument
  char  dvalue;               // optional default value argument
  char* dvaluep = NULL;       // pointer to dvalue
  char* fLabel = NULL;
  int llen = 0;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // check the optional argument list
  ESMCI_ArgStart(argPtr,value);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgLabelID:
        ESMCI_ArgGetString(argPtr);
        break;
      case ESMCI_ConfigArgDvalueID:
        ESMCI_ArgGetChar(argPtr);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // parse the optional argument list
  ESMCI_ArgStart(argPtr,value);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgLabelID:
        label = ESMCI_ArgGetString(argPtr);
        break;
      case ESMCI_ConfigArgDvalueID:
        dvalue  = ESMCI_ArgGetChar(argPtr);
        dvaluep = &dvalue;
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // convert label to fortran string
  if (label != NULL) {
    llen = strlen(label);
    fLabel = new char[llen];
    localrc = ESMC_CtoF90string(label, fLabel, llen);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
      delete[] fLabel;
      return rc;
    }
  }

  // call Fortran interface
  FTN(f_esmf_configgetchar)(config, value, fLabel, dvaluep, &localrc, 1, llen, 1);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    if (fLabel != NULL) {delete[] fLabel;}
    return rc;
  }

  // clean up
  if (fLabel != NULL) {delete[] fLabel;}

  // set return code for this branch
  rc = ESMF_SUCCESS;

  // final return
  return rc;

} // end ESMC_ConfigGetChar
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
  ESMC_Config* config,       // in  - ESMC_Config object
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
  char* fLabel = NULL;
  int llen = 0;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // check the optional argument list
  ESMCI_ArgStart(argPtr,wordCount);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgLabelID:
        ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
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
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // convert label to fortran string
  if (label != NULL) {
    llen = strlen(label);
    fLabel = new char[llen];
    localrc = ESMC_CtoF90string(label, fLabel, llen);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
      delete[] fLabel;
      return rc;
    }
  }

  // call into Fortran interface
  FTN(f_esmf_configgetlen)(config, wordCount, fLabel, &localrc, llen);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    if (fLabel != NULL) {delete[] fLabel;}
    return rc;
  }

  // clean up
  if (fLabel != NULL) {delete[] fLabel;}

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
  ESMC_Config* config,       // in  - ESMC_Config object
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
  char* fLabel = NULL;
  int llen = 0;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // check the optional argument list
  ESMCI_ArgStart(argPtr,columnCount);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgLabelID:
        ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
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
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // convert label to fortran string
  if (label != NULL) {
    llen = strlen(label);
    fLabel = new char[llen];
    localrc = ESMC_CtoF90string(label, fLabel, llen);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
      delete[] fLabel;
      return rc;
    }
  }

  // call Fortran interface
  FTN(f_esmf_configgetdim)(config, lineCount, columnCount, fLabel, &localrc, llen);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    if (fLabel != NULL) {delete[] fLabel;}
    return rc;
  }

  // clean up
  if (fLabel != NULL) {delete[] fLabel;}

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
  ESMC_Config* config,       // in  - ESMC_Config object
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
  char* foptions = NULL;
  int olen = 0;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // check the optional argument list
  ESMCI_ArgStart(argPtr,config);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgOptionsID:
        ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
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
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    }
  }
  ESMCI_ArgEnd(argPtr);

  // convert options to fortran string
  if (options != NULL) {
    olen = strlen(options);
    foptions = new char[olen];
    localrc = ESMC_CtoF90string(options, foptions, olen);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
      delete[] foptions;
      return rc;
    }
  }

  // call Fortran interface
  FTN(f_esmf_configvalidate)(config, foptions, &localrc, olen);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
    if (foptions != NULL) {delete[] foptions;}
    return rc;
  }

  // clean up
  if (foptions != NULL) delete[] foptions;

  // set return code for this branch
  rc = ESMF_SUCCESS;

  // final return
  return rc;

} // end ESMC_ConfigValidate
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigGetAttribute()"
//BOP
// !IROUTINE:  ESMC_ConfigGetAttribute - Get an attribute
//
// !INTERFACE:
int ESMC_ConfigGetAttribute(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS: 
  ESMC_Config* config,       // in  - ESMC_Config object
  void* value,               // out - value
  ESMC_TypeKind tk,          // in  - tk
  ...                        // optional argument list: (count, label, dvalue)
  ) {
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
  // local vars
  int rc;                     // return code
  int localrc;                // local return code
  const int numArg = 3;       // number of optional arguments
  ESMCI_ArgList argPtr;       // optional argument list pointer
  ESMCI_ArgID argID;          // optional argument list id
  int  count = 1;             // optional count argument
  char* label = NULL;         // optional label argument
  void*   dvaluep = NULL;     // pointer to optional dvalue_x
  ESMC_I4 dvalue_i4;          // optional default integer i4 value argument
  ESMC_I8 dvalue_i8;          // optional default integer i8 value argument
  ESMC_R4 dvalue_r4;          // optional default real r4 value argument
  ESMC_R8 dvalue_r8;          // optional default real r8 value argument
  int     dvalue_l;           // optional default logical value argument
  char*   dvalue_s;           // optional default string value argument
  char* fLabel = NULL;        // fortran label string
  char* fDvalue = NULL;       // fortran default string value
  char  fValue[ESMF_MAXSTR];  // fortran string value
  int llen = 0;               // length of label string
  int vlen = ESMF_MAXSTR;     // length of fortran string value
  int dlen = 0;               // length of default string value
  int i;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // check for supported type/kind:
  switch ( tk ) {
    case ESMC_TYPEKIND_I4:
      break;
    case ESMC_TYPEKIND_I8:
      break;
    case ESMC_TYPEKIND_R4:
      break;
    case ESMC_TYPEKIND_R8:
      break;
    case ESMC_TYPEKIND_LOGICAL:
      break;
    case ESMC_TYPEKIND_CHARACTER:
      break;
    default:
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "- unknown data type/kind", &rc);
      return rc;
  } // end switch (tk)

  // check the optional argument list:
  ESMCI_ArgStart(argPtr,tk);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgCountID:
        ESMCI_ArgGetInt(argPtr);
        break;
      case ESMCI_ConfigArgLabelID:
        ESMCI_ArgGetString(argPtr);
        break;
      case ESMCI_ConfigArgDvalueID:
        switch ( tk ) {
          case ESMC_TYPEKIND_I4:
            ESMCI_ArgGetI4(argPtr);
            break;
          case ESMC_TYPEKIND_I8:
            ESMCI_ArgGetI8(argPtr);
            break;
          case ESMC_TYPEKIND_R4:
            ESMCI_ArgGetR4(argPtr);
            break;
          case ESMC_TYPEKIND_R8:
            ESMCI_ArgGetR8(argPtr);
            break;
          case ESMC_TYPEKIND_LOGICAL:
            ESMCI_ArgGetInt(argPtr);
            break;
          case ESMC_TYPEKIND_CHARACTER:
            ESMCI_ArgGetString(argPtr);
            break;
        } // end switch (tk)
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    } // end switch (argID)
  } // end while (argID)
  ESMCI_ArgEnd(argPtr);

  // parse the optional argument list:
  ESMCI_ArgStart(argPtr,tk);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgCountID:
        count = ESMCI_ArgGetInt(argPtr);
        break;
      case ESMCI_ConfigArgLabelID:
        label = ESMCI_ArgGetString(argPtr);
        break;
      case ESMCI_ConfigArgDvalueID:
        switch ( tk ) {
          case ESMC_TYPEKIND_I4:
            dvalue_i4 = ESMCI_ArgGetI4(argPtr);
            dvaluep = &dvalue_i4;
            break;
          case ESMC_TYPEKIND_I8:
            dvalue_i8 = ESMCI_ArgGetI8(argPtr);
            dvaluep = &dvalue_i8;
            break;
          case ESMC_TYPEKIND_R4:
            dvalue_r4 = ESMCI_ArgGetR4(argPtr);
            dvaluep = &dvalue_r4;
            break;
          case ESMC_TYPEKIND_R8:
            dvalue_r8 = ESMCI_ArgGetR8(argPtr);
            dvaluep = &dvalue_r8;
            break;
          case ESMC_TYPEKIND_LOGICAL:
            dvalue_l = ESMCI_ArgGetInt(argPtr);
            dvaluep = &dvalue_l;
            break;
          case ESMC_TYPEKIND_CHARACTER:
            dvalue_s = ESMCI_ArgGetString(argPtr);
            dvaluep = &dvalue_s;
            break;
        } // end switch (tk)
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    } // end switch (argID)
  } // end while (argID)
  ESMCI_ArgEnd(argPtr);

  // convert label to fortran string
  if (label != NULL) {
    llen = strlen(label);
    fLabel = new char[llen];
    localrc = ESMC_CtoF90string(label, fLabel, llen);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
      delete[] fLabel;
      return rc;
    }
  }

  // branch according to data type/kind
  switch ( tk ) {

    // ESMC_TYPEKIND_I4
    case ESMC_TYPEKIND_I4: {

      // call Fortran interface
      if (count > 1) {
        FTN(f_esmf_configgetintsi4)(config, &count, (ESMC_I4*)value,
                   fLabel, (ESMC_I4*)dvaluep, &localrc, llen);
      } else {
        FTN(f_esmf_configgetinti4)( config, (ESMC_I4*)value,
                   fLabel, (ESMC_I4*)dvaluep, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }

      // set return code for this branch
      rc = ESMF_SUCCESS;

      break;
    } // end ESMC_TYPEKIND_I4

    // ESMC_TYPEKIND_I8
    case ESMC_TYPEKIND_I8: {

      // call Fortran interface
      if (count > 1) {
        FTN(f_esmf_configgetintsi8)(config,  &count, (ESMC_I8*)value,
                   fLabel, (ESMC_I8*)dvaluep, &localrc, llen);
      } else {
        FTN(f_esmf_configgetinti8)( config, (ESMC_I8*)value,
                   fLabel, (ESMC_I8*)dvaluep, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }

      // set return code for this branch
      rc = ESMF_SUCCESS;

      break;
    } // end ESMC_TYPEKIND_I8

    // ESMC_TYPEKIND_R4
    case ESMC_TYPEKIND_R4: {

      // call Fortran interface
      if (count > 1) {
        FTN(f_esmf_configgetfloatsr4)(config, &count, (ESMC_R4*)value,
                   fLabel, (ESMC_R4*)dvaluep, &localrc, llen);
      } else {
        FTN(f_esmf_configgetfloatr4)( config, (ESMC_R4*)value,
                   fLabel, (ESMC_R4*)dvaluep, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }

      // set return code for this branch
      rc = ESMF_SUCCESS;

      break;
    } // end ESMC_TYPEKIND_R4

    // ESMC_TYPEKIND_R8
    case ESMC_TYPEKIND_R8: {

      // call Fortran interface
      if (count > 1) {
        FTN(f_esmf_configgetfloatsr8)(config, &count, (ESMC_R8*)value,
                   fLabel, (ESMC_R8*)dvaluep, &localrc, llen);
      } else {
        FTN(f_esmf_configgetfloatr8)( config, (ESMC_R8*)value,
                   fLabel, (ESMC_R8*)dvaluep, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }

      // set return code for this branch
      rc = ESMF_SUCCESS;

      break;
    } // end ESMC_TYPEKIND_R8

    // ESMC_TYPEKIND_LOGICAL
    case ESMC_TYPEKIND_LOGICAL: {

      // call Fortran interface
      if (count > 1) {
        FTN(f_esmf_configgetlogicals)(config, &count, (int*)value,
                   fLabel, (int*)dvaluep, &localrc, llen);
      } else {
        FTN(f_esmf_configgetlogical)( config, (int*)value,
                   fLabel, (int*)dvaluep, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }

      // set return code for this branch
      rc = ESMF_SUCCESS;

      break;
    } // end ESMC_TYPEKIND_LOGICAL

    // ESMC_TYPEKIND_CHARACTER
    case ESMC_TYPEKIND_CHARACTER: {

      // convert dvalue_s to fortran string
      if (dvaluep != NULL) {
        dlen = strlen(dvalue_s);
        fDvalue = new char[dlen];
        localrc = ESMC_CtoF90string(dvalue_s, fDvalue, dlen);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
          if (fLabel != NULL) {delete[] fLabel;}
          delete[] fDvalue;
          return rc;
        }
      }

      // call Fortran interface
      FTN(f_esmf_configgetstring)(config, fValue,
                 fLabel, fDvalue, &localrc, vlen, llen, dlen);

      // handle special case of internal non-zero non-failure return code
      // that occurs when line is blank and default is provided.
      if (localrc == -1 && dvaluep != NULL) localrc = ESMF_SUCCESS;

      // check local return code
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        if (fDvalue != NULL) {delete[] fDvalue;}
        return rc;
      }

      // assign fValue_s to value
      localrc = ESMC_F90toCstring(fValue, ESMF_MAXSTR, (char*)value, ESMF_MAXSTR);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        if (fDvalue != NULL) {delete[] fDvalue;}
        return rc;
      }

      // clean up
      if (fDvalue != NULL) {delete[] fDvalue;}

      // set return code for this branch
      rc = ESMF_SUCCESS;

      break;
    } // end ESMC_TYPEKIND_CHARACTER

  } // end branch on data type/kind

  // clean up
  if (fLabel != NULL) {delete[] fLabel;}

  // final return
  return rc;

} // end ESMC_ConfigGetAttribute
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ConfigSetAttribute()"
//BOP
// !IROUTINE:  ESMC_ConfigSetAttribute - Set an attribute
//
// !INTERFACE:
int ESMC_ConfigSetAttribute(
//
// !RETURN VALUE:
//  int error return code
//  Equals {\tt ESMF\_SUCCESS} if there are no errors.
//
// !ARGUMENTS: 
  ESMC_Config* config,       // in  - ESMC_Config object
  void* value,               // in  - value
  ESMC_TypeKind tk,          // in  - tk
  ...                        // optional argument list: (count, label)
  ) {
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
  // local vars
  int rc;                     // return code
  int localrc;                // local return code
  const int numArg = 2;       // number of optional arguments
  ESMCI_ArgList argPtr;       // optional argument list pointer
  ESMCI_ArgID argID;          // optional argument list id
  int  count = 1;             // optional count argument
  char* label = NULL;         // optional label argument
  char* fLabel = NULL;        // fortran label string
  char  fValue[ESMF_MAXSTR];  // fortran string value
  int llen = 0;               // length of label string
  int vlen = 0;               // length of input string value
  int i;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;
  localrc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (config == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Config", &rc);
    return rc;
  }

  // check for supported type/kind:
  switch ( tk ) {
    case ESMC_TYPEKIND_I4:
      break;
    case ESMC_TYPEKIND_I8:
      break;
    case ESMC_TYPEKIND_R4:
      break;
    case ESMC_TYPEKIND_R8:
      break;
    case ESMC_TYPEKIND_LOGICAL:
      break;
    case ESMC_TYPEKIND_CHARACTER:
      break;
    default:
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "- unknown data type/kind", &rc);
      return rc;
  } // end switch (tk)

  // check the optional argument list:
  ESMCI_ArgStart(argPtr,tk);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgCountID:
        ESMCI_ArgGetInt(argPtr);
        break;
      case ESMCI_ConfigArgLabelID:
        ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    } // end switch (argID)
  } // end while (argID)
  ESMCI_ArgEnd(argPtr);

  // parse the optional argument list:
  ESMCI_ArgStart(argPtr,tk);
  while ( (argID = ESMCI_ArgGetID(argPtr)) != ESMCI_ArgLastID ) {
    switch ( argID ) {
      case ESMCI_ConfigArgCountID:
        count = ESMCI_ArgGetInt(argPtr);
        break;
      case ESMCI_ConfigArgLabelID:
        label = ESMCI_ArgGetString(argPtr);
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OPTARG_BAD, "", &rc);
        return rc;
    } // end switch (argID)
  } // end while (argID)
  ESMCI_ArgEnd(argPtr);

  // convert label to fortran string
  if (label != NULL) {
    llen = strlen(label);
    fLabel = new char[llen];
    localrc = ESMC_CtoF90string(label, fLabel, llen);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
      delete[] fLabel;
      return rc;
    }
  }

  // branch according to data type/kind
  switch ( tk ) {

    // ESMC_TYPEKIND_I4
    case ESMC_TYPEKIND_I4: {

      // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
      if (count > 1) {
        FTN(f_esmf_configsetintsi4)(config, (ESMC_I4*)value, &count,
                   fLabel, &localrc, llen);
      } else {
        FTN(f_esmf_configsetinti4)( config, (ESMC_I4*)value,
                   fLabel, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }
********************************************/

      // set return code for this branch
      rc = ESMC_RC_NOT_IMPL;

      break;
    } // end ESMC_TYPEKIND_I4

    // ESMC_TYPEKIND_I8
    case ESMC_TYPEKIND_I8: {

      // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
      if (count > 1) {
        FTN(f_esmf_configsetintsi8)(config, (ESMC_I8*)value, &count,
                   fLabel, &localrc, llen);
      } else {
        FTN(f_esmf_configsetinti8)( config, (ESMC_I8*)value,
                   fLabel, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }
********************************************/

      // set return code for this branch
      rc = ESMC_RC_NOT_IMPL;

      break;
    } // end ESMC_TYPEKIND_I8

    // ESMC_TYPEKIND_R4
    case ESMC_TYPEKIND_R4: {

      // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
      if (count > 1) {
        FTN(f_esmf_configsetfloatsr4)(config, (ESMC_R4*)value, &count,
                   fLabel, &localrc, llen);
      } else {
        FTN(f_esmf_configsetfloatr4)( config, (ESMC_R4*)value,
                   fLabel, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }
********************************************/

      // set return code for this branch
      rc = ESMC_RC_NOT_IMPL;

      break;
    } // end ESMC_TYPEKIND_R4

    // ESMC_TYPEKIND_R8
    case ESMC_TYPEKIND_R8: {

      // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
      if (count > 1) {
        FTN(f_esmf_configsetfloatsr8)(config, (ESMC_R8*)value, &count,
                   fLabel, &localrc, llen);
      } else {
        FTN(f_esmf_configsetfloatr8)( config, (ESMC_R8*)value,
                   fLabel, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }
********************************************/

      // set return code for this branch
      rc = ESMC_RC_NOT_IMPL;

      break;
    } // end ESMC_TYPEKIND_R8

    // ESMC_TYPEKIND_LOGICAL
    case ESMC_TYPEKIND_LOGICAL: {

      // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
      if (count > 1) {
        FTN(f_esmf_configsetlogicals)(config, (int*)value, &count,
                   fLabel, &localrc, llen);
      } else {
        FTN(f_esmf_configsetlogical)( config, (int*)value,
                   fLabel, &localrc, llen);
      }
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }
********************************************/

      // set return code for this branch
      rc = ESMC_RC_NOT_IMPL;

      break;
    } // end ESMC_TYPEKIND_LOGICAL

    // ESMC_TYPEKIND_CHARACTER
    case ESMC_TYPEKIND_CHARACTER: {

      // convert value to fortran string
      if ((char*)value != NULL) {
        vlen = strlen((char*)value);
        localrc = ESMC_CtoF90string((char*)value, fValue, vlen);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
          if (fLabel != NULL) {delete[] fLabel;}
          return rc;
        }
      }

      // call Fortran interface
/* ***** THIS SECTION IS UNIMPLEMENTED *****
      FTN(f_esmf_configsetstring)(config, fValue,
                 fLabel, &localrc, vlen, llen);

      // check local return code
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)) {
        if (fLabel != NULL) {delete[] fLabel;}
        return rc;
      }
********************************************/

      // set return code for this branch
      rc = ESMC_RC_NOT_IMPL;

      break;
    } // end ESMC_TYPEKIND_CHARACTER

  } // end branch on data type/kind

  // clean up
  if (fLabel != NULL) {delete[] fLabel;}

  // final return
  return rc;

} // end ESMC_ConfigSetAttribute
//-----------------------------------------------------------------------------


}; //end extern "C"

