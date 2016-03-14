// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMCI_AttributeWriteJSON.C"

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
#include <sstream>
#include <vector>

using std::string;
using std::ostringstream;
using std::vector;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "streamJSON"
//BOPI
// !IROUTINE:  streamJSON - stream an Attpack to JSON formatted output
//
// !INTERFACE:
    int Attribute::streamJSON(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
            string &output) const {
//
// !DESCRIPTION:
//    stream an Attpack to JSON formatted output
//
//EOPI

      // helper variables
      ostringstream os;
      Attribute *attr;

      // open the JSON
      // TODO: add attrObject to root Attributes
      os << "\n{\n  \"" << getObject() <<"\" :{\n";

      // stream the JSON, starting from root
      if (attrRoot == ESMF_FALSE) attr = &(attrBase->root);
      attr->streamJSONiter(os, 2);

      // close the JSON
      os << "  }\n}\n";

      // set the return value to the ostringstream
      output = os.str();

      return ESMF_SUCCESS;

    } // end streamJSON

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "streamJSONiter"

    int Attribute::streamJSONiter(ostringstream &os, unsigned int level) const {

      int localrc;

      vector<string> vv;

      bool newlevel;
      newlevel = false;

      string newline = "\n";
      string opencb = "{";
      string closecb = "}";
      string openb = "[";
      string closeb = "]";
      string indent = "";
      string indent_inc = "  ";
      string separator = ": ";
      string comma = ",";
      string quote = "\"";
      string space = " ";

      /*printf("debug:\n  level=%d\n  object=%s\n  name=%s\n",
             level, getObject().c_str(), getName().c_str());*/

      // set the indent
      for (unsigned int l = 0; l < level; ++l) indent += indent_inc;

      // Stream all the Attributes
      for (unsigned int i=0; i<attrList.size(); ++i) {

        // write json to stream
        if (attrList.at(i)->isSet()) attrList.at(i)->get(&vv);
        if (vv.size() == 1) {
          os << indent << quote << attrList.at(i)->attrName
             << quote << separator
             << quote << vv.at(0) << quote << comma << newline;
        }
      }

      // Recurse the Attpack tree, this version compacts all nested attpacks
      // into the same json object block
      for (unsigned int j=0; j<packList.size(); ++j) {
        localrc = packList.at(j)->streamJSONiter(os, level);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &localrc)) return localrc;
      }

      // Recurse the object hierarchy
      for (unsigned int k=0; k<linkList.size(); ++k) {
        // only open arrays once
        if (k == 0) {
          // TODO: add attrObject to root Attributes
          string obj = "";
          if (linkList.at(k)->attrList.size() > 0)
            obj = linkList.at(k)->attrList.at(0)->getObject();
          else if (linkList.at(k)->packList.size() > 0)
            obj = linkList.at(k)->packList.at(0)->getObject();
          // pluralize the object.. nitty gritty here
          obj += "s";

          os << indent << quote << obj
             << quote << separator << openb;
        }

        // open new object
        os << opencb << newline;

        // recurse
        localrc = linkList.at(k)->streamJSONiter(os, level + 1);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_CONTEXT,
                                          &localrc))
          return localrc;

        // only close arrays once
        if (k<linkList.size()-1)
          os << indent << closecb << comma << space;
        else
          os << indent << closecb << closeb << newline;
      }

      return ESMF_SUCCESS;

    } // end streamJSONiter

    } // namespace ESMCI

