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
      os << "{  \"" << getObject() <<"\" :{";

      // stream the JSON, starting from root
      if (attrRoot == ESMF_FALSE) attr = &(attrBase->root);
      attr->streamJSONiter(os, 2);

      // for flat lists, remove comma from last object
      // - better yet would be to return a list of name/value pairs from an
      //   iterator function and then assemble output in a flat function
      // TODO: this is hacky
      // - removed the seekp call in favor of this other sequence of calls
      // to appease the older pgi compilers

      if (attr->linkList.size() == 0) {
        //os.seekp(-1,os.cur);
        string temp = os.str();
        temp.erase(temp.end()-1);
        os.str("");
        os.clear();
        os << temp;
      }

      // close the JSON
      os << "  }}";

      // set the return value to the ostringstream
      output = os.str();

      return ESMF_SUCCESS;

    } // end streamJSON

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "streamJSONiter"

    int Attribute::streamJSONiter(ostringstream &os, unsigned int level) const {

      int localrc;

      vector<string> 		vecs;		// vector of strings
      vector<ESMC_I4>       veci;       // vector of integers
      vector<ESMC_I8>       vecl;       // vector of longs
      vector<ESMC_R4>       vecf;       // vector of floats (real*4)
      vector<ESMC_R8>       vecd;       // vector of doubles (real*8)
      vector<ESMC_Logical>  vecb;       // vector of booleans (logical)

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
        if (attrList.at(i)->isSet()) {

        	string val;
        	int attrCount = -1;

        	localrc = attrList.at(i)->getCount(ESMC_ATTGETCOUNT_ATTRIBUTE, &attrCount);
        	if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        	        				&localrc)) return localrc;

        	if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER) {
        		localrc = attrList.at(i)->get(&vecs);
        		if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        				&localrc)) return localrc;
        		val = attrValuesToString(&vecs);
        	}
        	else if (attrList.at(i)->tk == ESMC_TYPEKIND_I4) {
        		localrc = attrList.at(i)->get(&attrCount, &veci);
				if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
						&localrc)) return localrc;
				val = attrValuesToString(&veci);
        	}
        	else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) {
				localrc = attrList.at(i)->get(&attrCount, &vecl);
				if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
						&localrc)) return localrc;
				val = attrValuesToString(&vecl);
			}
        	else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) {
				localrc = attrList.at(i)->get(&attrCount, &vecf);
				if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
						&localrc)) return localrc;
				val = attrValuesToString(&vecf);
			}
        	else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) {
				localrc = attrList.at(i)->get(&attrCount, &vecd);
				if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
						&localrc)) return localrc;
				val = attrValuesToString(&vecd);
			}
        	else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
				localrc = attrList.at(i)->get(&attrCount, &vecb);
				if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
						&localrc)) return localrc;
				val = attrValuesToString(&vecb);
        	}
        	else {
        		val = "\"UNSUPPORTED-TYPE\"";
        	}

        	os << indent << quote << attrList.at(i)->attrName << quote
        			<< separator << val;

        	//add comma if needed
        	if (i < attrList.size()-1 || packList.size() > 0) {
        		os << comma;
        	}

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
      // RSD - this is not working now, sometimes obj below is
      //       an empty string, not sure why?
      /*
      for (unsigned int k=0; k<linkList.size(); ++k) {
        // only open arrays once
        if (k == 0) {
          // TODO: add attrObject to root Attributes
          string obj = "";
          if (linkList.at(k)->attrList.size() > 0)
            obj = linkList.at(k)->attrList.at(0)->getObject();
          else if (linkList.at(k)->packList.size() > 0)
            obj = linkList.at(k)->packList.at(0)->getObject();

          // TODO: this is hacky
          // pluralize the object.. nitty gritty here
          obj += "s";

          os << indent << quote << obj
             << quote << separator << openb;
        }

        // open new object
        os << opencb;

        // recurse
        localrc = linkList.at(k)->streamJSONiter(os, level + 1);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_CONTEXT,
                                          &localrc))
          return localrc;

        // remove comma from last object
        // - better yet would be to return a list of name/value pairs from an
        //   iterator function and then assemble output in a flat function
        // TODO: this is hacky
        // - removed the seekp call in favor of this other sequence of calls
        // to appease the older pgi compilers
        //os.seekp(-1,os.cur);
        string temp = os.str();
        temp.erase(temp.end()-1);
        os.str("");
        os.clear();
        os << temp;

        // only close arrays once
        if (k<linkList.size()-1)
          os << indent << closecb << comma << space;
        else
          os << indent << closecb << closeb;
      }
	  */

      return ESMF_SUCCESS;

    } // end streamJSONiter


    //-----------------------------------------------------------------------------
    #undef  ESMC_METHOD
    #define ESMC_METHOD "attrValuesToString"

    template<typename T>
    string Attribute::attrValuesToString(const vector<T> *vec) const{
    	ostringstream valstr;
    	if (vec->size() == 1) {
    		valstr << "\"" << vec->at(0) << "\"";
    	}
    	else if (vec->size() > 1) {
    		valstr << "[";
			for (int i = 0; i < vec->size(); i++) {
				valstr << "\"" << vec->at(i) << "\"";
				if (i < vec->size()-1) {
					valstr << ",";
				}
			}
			valstr << "]";
    	}
    	return valstr.str();
    }


	} // namespace ESMCI

