// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMCI_AttributeWrite.C"

// Attribute method implementation (body) file

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Attribute} methods declared
// in the companion file ESMCI_Attribute.h
//
//-----------------------------------------------------------------------------
// associated class definition file and others
#include "ESMCI_Attribute.h"

#include "ESMCI_Macros.h"
#include "ESMCI_IO_XML.h"
#include "ESMCI_LogErr.h"

#include <cstdlib>
#include <cstring>

using std::string;

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeRead"
//BOPI
// !IROUTINE:  AttributeRead - read Attributes from XML file
//
// !INTERFACE:
      int Attribute::AttributeRead(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      int fileNameLen,              //  in - file name length
      const char* fileName,         //  in - file name
      int schemaFileNameLen,        //  in - schema file name length
      const char* schemaFileName) { //  in - schema file name

//
// !DESCRIPTION:
//    Read the contents of an XML file into an {\tt Attribute} hierarchy.
//    Expected to be called internally.
//
//EOPI

  // Initialize local return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMF_SUCCESS;

  // instantiate IO object; initialize with pointer to this Attribute node, to
  // place file-read attributes into.
  IO_XML *io_xml = ESMCI_IO_XMLCreate("", "", 
                                      (ESMCI::Attribute*)this, &localrc);
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  // read the XML file, placing contents into this Attribute node
  localrc = io_xml->read(string(fileName,fileNameLen), string(schemaFileName, schemaFileNameLen));
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  localrc = ESMCI_IO_XMLDestroy(&io_xml);
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  return rc;

 } // end AttributeRead

    //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWrite"
//BOPI
// !IROUTINE:  AttributeWrite - Write contents of an {\tt Attribute} package
//
// !INTERFACE:
    int Attribute::AttributeWrite(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
            const string &convention,        //  in - convention
            const string &purpose,           //  in - purpose
            const string &object,            //  in - object
            const string &varobj,            //  in - variable object
            const string &basename,          //  in - basename
            const ESMC_AttWriteFlag &attwriteflag) const {
//
// !DESCRIPTION:
//    Write the contents of an {\tt Attribute} to file.  Expected to be
//    called internally.
//
//EOPI

      int localrc;
      char msgbuf[4*ESMF_MAXSTR];

      // Initialize local return code; assume routine not implemented
      localrc = ESMC_RC_NOT_IMPL;

      if (attwriteflag == ESMC_ATTWRITE_TAB) {
        localrc = AttributeWriteTab(convention, purpose, object, varobj, basename);
      } else if (attwriteflag == ESMC_ATTWRITE_XML) {
        localrc = AttributeWriteXML(convention, purpose, object, varobj, basename);
      } else {
        sprintf(msgbuf, "ESMC_AttWriteFlag = %d is not recognized", attwriteflag);
        ESMC_LogDefault.MsgFoundError(localrc, msgbuf, ESMC_CONTEXT, &localrc);
      }

      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

      return localrc;

    } // end AttributeWrite

} // namespace ESMCI

