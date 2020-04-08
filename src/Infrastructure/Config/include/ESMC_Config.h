// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Config_H
#define ESMC_Config_H

//-----------------------------------------------------------------------------
// ESMC_Config - C++ interface to the F90 Config object
//
// The code in this file defines the C++ Config members and declares method 
// signatures (prototypes).  The companion file {\tt ESMC\_Config.C} contains
// the definitions (full code bodies) for the Config methods.
//-----------------------------------------------------------------------------

#include "ESMC_Arg.h"
#include "ESMC_Util.h"

// Optional argument identifier list for the ESMC_Config API.
enum {
  ESMCI_ConfigArgLabelID   = ESMCI_ArgBaseID,  // char*
  ESMCI_ConfigArgDvalueID,                     // type depends on TypeKind
  ESMCI_ConfigArgCountID,                      // int
  ESMCI_ConfigArgUniqueID,                     // int
  ESMCI_ConfigArgTableEndID,                   // int*
  ESMCI_ConfigArgOptionsID                     // char*
};

// Argument expansion macros for the ESMC_Config API.
#define ESMC_ConfigArgLabel(ARG)      ESMCI_Arg(ESMCI_ConfigArgLabelID,ARG)
#define ESMC_ConfigArgDvalue(ARG)     ESMCI_Arg(ESMCI_ConfigArgDvalueID,ARG)
#define ESMC_ConfigArgCount(ARG)      ESMCI_Arg(ESMCI_ConfigArgCountID,ARG)
#define ESMC_ConfigArgUnique(ARG)     ESMCI_Arg(ESMCI_ConfigArgUniqueID,ARG)
#define ESMC_ConfigArgTableEnd(ARG)   ESMCI_Arg(ESMCI_ConfigArgTableEndID,ARG)
#define ESMC_ConfigArgOptions(ARG)    ESMCI_Arg(ESMCI_ConfigArgOptionsID,ARG)


#ifdef __cplusplus
extern "C" {
#endif

// class declaration type
typedef struct {
  void* ptr;
}ESMC_Config ;


// prototypes for the ESMC_Config API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigCreate - Create a Config object
//
// !INTERFACE:
ESMC_Config ESMC_ConfigCreate(
  int* rc                    // out
);
// !RETURN VALUE:
//  ESMC_Config*  to newly allocated ESMC_Config
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

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigDestroy - Destroy a Config object
//
// !INTERFACE:
int ESMC_ConfigDestroy(
  ESMC_Config* config        // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Destroys the {\tt config} object.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object to destroy.
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigCreate - Create a Config object from a section of an existing Config object
//
// !INTERFACE:
ESMC_Config ESMC_ConfigCreateFromSection(
  ESMC_Config config,       // in
  const char* olabel,       // in
  const char* clabel,       // in
  int* rc                   // out
);
// !RETURN VALUE:
//  ESMC_Config*  to newly allocated ESMC_Config
//
// !DESCRIPTION:
//   Instantiates an {\tt ESMC\_Config} object from a section of an existing
//   {\tt ESMC\_Config} object delimited by {\tt openlabel} and {\tt closelabel}.
//   An error is returned if neither of the input labels is found in input
//   {\tt config}.
//
//   Note that a section is intended as the content of a given {\tt ESMC\_Config}
//   object delimited by two distinct labels. Such content, as well as each of the
//   surrounding labels, are still within the scope of the parent {\tt ESMC\_Config}
//   object. Therefore, including in a section labels used outside that
//   section should be done carefully to prevent parsing conflicts.
//
//   The arguments are:
//   \begin{description}
//     \item[config]
//       The input {\tt ESMC\_Config} object.
//     \item[openlabel]
//       Label marking the beginning of a section in {\tt config}.
//     \item[closelabel]
//       Label marking the end of a section in {\tt config}.
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigFindLabel - Find a label
//
// !INTERFACE:
int ESMC_ConfigFindLabel(
  ESMC_Config config,        // in
  const char* label,         // in
  int *isPresent             // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//  If label not found, and the {\tt isPresent} pointer is {\tt NULL},
//  an error will be returned.
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
//   \item [{[isPresent]}]
//     Label presence flag.  (optional).  If non-NULL, the target is
//     set to 1 when the label is found; otherwise set to 0.
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigFindNextLabel - Find a label
//
// !INTERFACE:
int ESMC_ConfigFindNextLabel(
  ESMC_Config config,        // in
  const char* label,         // in
  int *isPresent             // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//  If label not found, and the {\tt isPresent} pointer is {\tt NULL},
//  an error will be returned.
//
// !DESCRIPTION: Finds the {\tt label} (key) string in the {\tt config} object,
//   starting from the current position pointer.
//
//   This method is equivalent to {\tt ESMC\_ConfigFindLabel}, but the search
//   is performed starting from the current position pointer.
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

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigGetDim - Get table sizes
//
// !INTERFACE:
int ESMC_ConfigGetDim(
  ESMC_Config config,        // in
  int* lineCount,            // out
  int* columnCount,          // out
  ...                        // optional argument list
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
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
//  Due to this method accepting optional arguments, the final argument
//  must be {\tt ESMC\_ArgLast}.
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigGetLen - Get the length of the line in words
//
// !INTERFACE:
int ESMC_ConfigGetLen(
  ESMC_Config config,        // in
  int* wordCount,            // out
  ...                        // optional argument list
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
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
//  Due to this method accepting optional arguments, the final argument
//  must be {\tt ESMC\_ArgLast}.
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigLoadFile - Load resource file into memory
//
// !INTERFACE:
int ESMC_ConfigLoadFile(
  ESMC_Config config,        // in
  const char* file,          // in
  ...                        // optional argument list
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Resource file with {\tt filename} is loaded into memory.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \item [file]
//     Configuration file name.
//   \item [{[delayout]}]
//     {\tt ESMC\_DELayout} associated with this {\tt config} object.
//     **NOTE: This argument is not currently supported.
//   \item [{[unique]}]
//     If specified as true, uniqueness of labels are checked and 
//     error code set if duplicates found (optional).
//   \end{description}
//
//  Due to this method accepting optional arguments, the final argument
//  must be {\tt ESMC\_ArgLast}.
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigNextLine - Find next line
//
// !INTERFACE:
int ESMC_ConfigNextLine(
  ESMC_Config config,       // in
  int *tableEnd             // out
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
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
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigPrint - Write content of config object to standard output
//
// !INTERFACE:
int ESMC_ConfigPrint(
  ESMC_Config config        // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//   Write content of a {\tt ESMC\_Config} object to standard output.
//
//   The arguments are:
//   \begin{description}
//   \item [config]
//     Already created {\tt ESMC\_Config} object.
//   \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//int ESMC_ConfigGetChar(ESMC_Config config, char* value, ...);
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ConfigValidate - Validate a Config object
//
// !INTERFACE:
int ESMC_ConfigValidate(
  ESMC_Config config,        // in
  ...                        // optional argument list
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//  Equals ESMF_RC_ATTR_UNUSED if any unused attributes are found
//  with option "unusedAttributes" below.
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
//  Due to this method accepting optional arguments, the final argument
//  must be {\tt ESMC\_ArgLast}.
//
//EOP

// The following are not currently implemented:
int ESMC_ConfigGetAttribute(
  ESMC_Config config,        // in
  void* value,               // out
  enum ESMC_TypeKind_Flag tk,
  ...
);

int ESMC_ConfigSetAttribute(
  ESMC_Config config,        // in
  void* value,               // in
  enum ESMC_TypeKind_Flag tk,
  ...
);

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_Config_H
