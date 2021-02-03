// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC Schema file parsing (and eventually writing?) implementation
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO_Handler} methods
// declared in the companion file {\tt ESMCI\_IO_Handler.h}
//
//-------------------------------------------------------------------------
//
#define ESMC_FILENAME "ESMCI_IO_Schema.C"

// Below is a comment block showing the format of a Schema file
// Hash marks represent comments
#if 0
[Grid]                           # Denotes section reserved for grid info.
coordNames=('lat','lon')         # Names of grid coordinate dimensions in file
latUnits=('degrees_east','degree_east','degrees_E', # Allowed lat unit names
          'degree_E','degreesE','degreeE')          # Note <CR> not significant
lonUnits=('degrees_north','degree_north','degrees_N', # Allowed lon unit names
          'degree_N','degreesN','degreeN')

#endif // 0 (comment region)

// include associated header file
#include "ESMCI_IO_Schema.h"

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
#include "ESMCI_LogErr.h"

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI
{

  // Helper classes to hold Schema information
  class IOParseStream : public std::fstream {
  private:
    static int inbufferSize = 2048;
    char *inbuffer;
    bool linereadMax;
  public:
    IOParseStream(const char * filename, int *rc = (int *)NULL);
    ~IOParseStream();
    const char *const readLine(bool *maxLine = (bool *)NULL);
  }

  class IOSchemaBase {
    // IOSchemaBase is a virtual base class for Schema objects
    virtual ~IOSchemaBase() = 0;
  };  // class IOSchemaBase

  class IOSchemaVariable : public IOSchemaBase {
    // IOSchemaVariable holds schema information for a NetCDF variable
  };

  class IOSchemaGrid : public IOSchemaBase {
    // IOSchemaGrid holds grid schema information
    
  };


//
//-------------------------------------------------------------------------
//
// constructor and destructor
//
//-------------------------------------------------------------------------
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Schema::IO_Schema()"
//BOPI
// !IROUTINE:  ESMCI::IO_Schema::IO_Schema    - constructor
//
// !INTERFACE:
IO_Schema::IO_Schema(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  const char *const file,                 // (in)  - File to parse for Schema
  int *rc                                 // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    Parse the Schema information in <file> and return a new IO_Schema
//    object.

//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }

  try {  

    // fill in member variables
    if ((const char *const)NULL == file) {
      localrc = ESMF_RC_PTR_NULL;
      filename = (char *)NULL;
      ESMC_LogDefault.Write("file argument to IO_Schema cannot be NULL",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    } else {
      filename = new new char[strlen(file) + 1];
      if ((char *)NULL == filename) {
        localrc = ESMF_RC_MEM_ALLOCATE;
      } else {
        strcpy(filename, file);
        localrc = ESMF_SUCCESS;
      }
    }
    // See if we got this far OK (inputs check out)
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    localrc = parseFile(filename);
  } catch (int lrc) {
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(lrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch (...) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, rc);
    return;
  }
  
  // return
  if (rc != NULL) {
    *rc = localrc;
  }
} // IO_Schema::IO_Schema()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Schema::~IO_Schema()"
//BOPI
// !IROUTINE:  ESMCI::IO_Schema::~IO_Schema    - destructor
//
// !INTERFACE:
IO_Schema::~IO_Schema(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  ) {
//
// !DESCRIPTION:
//    Delete the internal memory of this IO_Schema object

//
//EOPI
//-----------------------------------------------------------------------------

  IOSchemaBase *object;

  // delete any memory stored in member variables
  if ((char *)NULL != filename) {
    delete filename;
    filename = (char *)NULL;
  }

  while(!schemaObjects.empty()) {
    object = schemaObjects.back();
    delete object;
    object = (IO_Schema *)NULL;
    schemaObjects.pop_back();
  }

} // IO_Schema::~IO_Schema()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Schema::parseFile()"
//BOPI
// !IROUTINE:  ESMCI::IO_Schema::parseFile    - parse the Schema info in <file>
//
// !INTERFACE:
int IO_Schema::parseFile(
//
// !RETURN VALUE:
//
//    int return code
//
// !ARGUMENTS:
//
  const char *const file                  // (in)  - File to parse for Schema
  ) {
//
// !DESCRIPTION:
//    Parse the Schema information in <file> and return a new IO_Schema
//    object.

//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // temp return code
  int rc = ESMF_RC_NOT_IMPL;              // final return code

  try {  

  } catch (int lrc) {
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(lrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc);
    return;
  } catch (...) {
    ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, "- Caught exception",
      ESMC_CONTEXT, rc);
    return;
  }
  
  // return
  return rc;
} // IO_Schema::parseFile()
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IOParseStream::IOParseStream()"
//BOPI
// !IROUTINE:  ESMCI::IOParseStream::IOParseStream   - constructor
//
// !INTERFACE:
IOParseStream::IOParseStream(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  const char * filename,              // (in)  - File to open for parsing
  int *rc                             // (out) - Error return code
  ) : fstream(filename, fstream::in) {
//
// !DESCRIPTION:
//    Open a stream for parsing <filename>

//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if ((int *)NULL != rc) {
    *rc = ESMF_RC_NOT_IMPL;              // final return code
  }
  // If we get here, we can assume that the fstream was constructed
  inbuffer = (char *)NULL;
  linereadMax = false;
 
  // Check for an error condition
  if (good()) {
    // The file opened successfully so allocate a buffer
    filename = new char[inbufferSize];
    if ((char *)NULL == filename) {
      ESMC_LogDefault.Write("Schema file open error, cannot allocate buffer",
                            ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
      localrc = ESMF_RC_MEM_ALLOCATE;
    } else {
      localrc = ESMF_SUCCESS;
    }
  } else if (eof()) {
    ESMC_LogDefault.Write("Schema file open failed -- empty file?",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    localrc = ESMF_RC_FILE_OPEN;
  } else if (bad()) {
    ESMC_LogDefault.Write("Schema file open failed -- bad stream",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    localrc = ESMF_RC_FILE_OPEN;
  } else if (fail()) {
    // Fail should be after bad since it includes the bad bit
    ESMC_LogDefault.Write("Schema file open failed -- open failed",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    localrc = ESMF_RC_FILE_OPEN;
  } else {
    ESMC_LogDefault.Write("Schema file open failed -- unknown error",
                          ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    localrc = ESMF_RC_FILE_OPEN;
  }

  // return
  if ((int *)NULL != rc) {
    *rc = localrc;
  }
} // IOParseStream::IOParseStream()
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IOParseStream::IOParseStream()"
//BOPI
// !IROUTINE:  ESMCI::IOParseStream::~IOParseStream   - destructor
//
// !INTERFACE:
IOParseStream::~IOParseStream(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  const char * filename                  // (in)  - File to open for parsing
  ) {
//
// !DESCRIPTION:
//    Open a stream for parsing <filename>

//
//EOPI
//-----------------------------------------------------------------------------
  if (is_open()) {
    close();
  }
  if ((char *)NULL != filename) {
    delete filename;
    filename = (char *)NULL;
  }
} // IOParseStream::~IOParseStream()
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IOParseStream::readLine()"
//BOPI
// !IROUTINE:  ESMCI::IOParseStream::readLine   - read a line from stream
//
// !INTERFACE:
const char * const IOParseStream::readLine(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
  bool *maxLine,                      // (in)  - Set to true for linelength
  int *rc                             // (out) - Error return code
  ) {
//
// !DESCRIPTION:
//    

//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMF_RC_NOT_IMPL;         // local return code
  if (rc != NULL) {
    *rc = ESMF_RC_NOT_IMPL;               // final return code
  }
  int charsRead = 0;
  
  // Read a line
  

  // return
  if (rc != NULL) {
    *rc = localrc;
  }
} // IOParseStream::readLine()
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------

}  // end namespace ESMCI
