// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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
#include "ESMCI_State.h"

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
                ESMC_Logical flattenPackList,
                ESMC_Logical includeUnset,
                        ESMC_Logical includeLinks,
                string &output
                        ) const {
//
// !DESCRIPTION:
//    stream an Attpack to JSON formatted output
//
//EOPI

      // helper variables
      Attribute *attr;
      int totalStreamed = 0;
      int localrc = ESMF_SUCCESS;

      // stream the JSON, starting from root
      if (attrRoot == ESMF_FALSE) {
          attr = attrBase->ESMC_BaseGetRoot();
          localrc = attr->streamAttributeRootToJSON(flattenPackList, includeUnset, includeLinks, output, &totalStreamed);
                  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                        &localrc)) return localrc;
      }
      else {
          localrc = streamAttributeRootToJSON(flattenPackList, includeUnset, includeLinks, output, &totalStreamed);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                &localrc)) return localrc;
      }

      return ESMF_SUCCESS;

    } // end streamJSON


    //-----------------------------------------------------------------------------
    #undef  ESMC_METHOD
        #define ESMC_METHOD "streamAttributeRootToJSON"

    int Attribute::streamAttributeRootToJSON(ESMC_Logical flattenPackList,
                ESMC_Logical includeUnset, ESMC_Logical includeLinks,
                        string &output, int *totalStreamed) const {

        int localrc;
                ostringstream ostream;
                string stringJSON;
                string objName;

                //For some reason attrObject is not being set on root
                //attributes.  This should be fixed.  In the meantime,
                //we need to traverse a bit to get the object name.
                objName = getObject();
                if (objName.length() == 0) {
                        if (attrList.size() > 0) {
                                objName = attrList.at(0)->getObject();
                        }
                        if (objName.length() == 0) {
                                if (packList.size() > 0) {
                                        objName = packList.at(0)->getObject();
                                }
                        }
                        if (objName.length() == 0) {
                                objName = "Unknown";
                        }
                }

                //char warnMsg[ESMF_MAXSTR];
                //sprintf(warnMsg, "Cannot determine object name for JSON output: %.25s", getName().c_str());
                //ESMC_LogDefault.Write(warnMsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);

                ostream << "{\"" << objName << "\":";
                if (flattenPackList == ESMF_TRUE) {
                        ostream << "{";
                }
                localrc = streamAttributeToJSON(flattenPackList, includeUnset, includeLinks, stringJSON, totalStreamed);
                if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                                                &localrc)) return localrc;

                ostream << stringJSON;
                if (flattenPackList == ESMF_TRUE) {
                        ostream << "}";
                }
                ostream << "}";

                output = ostream.str();

                return ESMF_SUCCESS;
    }


    //-----------------------------------------------------------------------------
    #undef  ESMC_METHOD
    #define ESMC_METHOD "streamAttributeToJSON"

        int Attribute::streamAttributeToJSON(ESMC_Logical flattenPackList,
                        ESMC_Logical includeUnset, ESMC_Logical includeLinks,
                        string &output, int *totalStreamed) const {

                int localrc;
                ostringstream ostream;
                const char *separator = "";
                *totalStreamed = 0;
                int localStreamed = 0;

                string stringJSON;
                if (flattenPackList == ESMF_FALSE) {
                        ostream << "{";
                }

                localrc = streamAttributeListToJSON(attrList,
                                flattenPackList, includeUnset, stringJSON, &localStreamed);
                if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                                &localrc)) return localrc;
                if (localStreamed > 0) {
                        if (flattenPackList == ESMF_FALSE) {
                                ostream << "\"attrList\":";
                        }
                        ostream << stringJSON;
                        separator = ",";
                        *totalStreamed = *totalStreamed + localStreamed;
                }

                localrc = streamAttributePackToJSON(packList,
                                flattenPackList, includeUnset, includeLinks, stringJSON, &localStreamed);
                if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                                &localrc)) return localrc;
                if (localStreamed > 0) {
                        ostream << separator;
                        if (flattenPackList == ESMF_FALSE) {
                                ostream << "\"packList\":";
                        }
                        ostream << stringJSON;
                        separator = ",";
                        *totalStreamed = *totalStreamed + localStreamed;
                }

                if (includeLinks == ESMF_TRUE) {
                        localrc = streamAttributeLinksToJSON(linkList,
                                        flattenPackList, includeUnset, stringJSON, &localStreamed);
                        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                                                &localrc)) return localrc;
                        if (localStreamed > 0) {
                                ostream << separator << "\"linkList\":" << stringJSON;
                                *totalStreamed = *totalStreamed + localStreamed;
                        }
                }

                if (flattenPackList == ESMF_FALSE) {
                        ostream << "}";
                }

                output = ostream.str();

                //printf("Exit streamComposite\n");

                return ESMF_SUCCESS;
        }


    //-----------------------------------------------------------------------------
        #undef  ESMC_METHOD
    #define ESMC_METHOD "streamAttributeListToJSON"

        int Attribute::streamAttributeListToJSON(vector<Attribute *> attrVector,
                        ESMC_Logical flattenPackList, ESMC_Logical includeUnset,
                        string &output, int *totalStreamed) const {

                int localrc;
                ostringstream ostream;
                const char *separator = "";
                *totalStreamed = 0;

                vector<string>                    vecs;                   // vector of strings
                vector<ESMC_I4>       veci;       // vector of integers
                vector<ESMC_I8>       vecl;       // vector of longs
                vector<ESMC_R4>       vecf;       // vector of floats (real*4)
                vector<ESMC_R8>       vecd;       // vector of doubles (real*8)
                vector<ESMC_Logical>  vecb;       // vector of booleans (logical)

                if (flattenPackList == ESMF_FALSE) {
                        ostream << "{";
                }

                for (int i = 0; i < attrVector.size(); i++) {

                        Attribute *cur = attrVector.at(i);

                        if (cur->isSet()) {
                                string val;
                                int attrCount = -1;

                                localrc = cur->getCount(ESMC_ATTGETCOUNT_ATTRIBUTE, &attrCount);
                                if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                                                &localrc)) return localrc;

                                if (cur->tk == ESMC_TYPEKIND_CHARACTER) {
                                        localrc = cur->get(&vecs);
                                        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                        &localrc)) return localrc;
                                        val = attrValuesToString(&vecs);
                                }
                                else if (cur->tk == ESMC_TYPEKIND_I4) {
                                        localrc = cur->get(&attrCount, &veci);
                                        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                        &localrc)) return localrc;
                                        val = attrValuesToString(&veci);
                                }
                                else if (cur->tk == ESMC_TYPEKIND_I8) {
                                        localrc = cur->get(&attrCount, &vecl);
                                        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                        &localrc)) return localrc;
                                        val = attrValuesToString(&vecl);
                                }
                                else if (cur->tk == ESMC_TYPEKIND_R4) {
                                        localrc = cur->get(&attrCount, &vecf);
                                        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                        &localrc)) return localrc;
                                        val = attrValuesToString(&vecf);
                                }
                                else if (cur->tk == ESMC_TYPEKIND_R8) {
                                        localrc = cur->get(&attrCount, &vecd);
                                        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                        &localrc)) return localrc;
                                        val = attrValuesToString(&vecd);
                                }
                                else if (cur->tk == ESMC_TYPEKIND_LOGICAL) {
                                        localrc = cur->get(&attrCount, &vecb);
                                        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                                        &localrc)) return localrc;
                                        val = attrValuesToString(&vecb);
                                }
                                else {
                                        val = "\"UNSUPPORTED-TYPE\"";
                                }

                                ostream << separator << "\"" << cur->attrName << "$" << cur->attrConvention << "$" << cur->attrPurpose << "\":" << val;
                                separator = ",";
                                (*totalStreamed)++;
                        }
                        else if (includeUnset == ESMF_TRUE){
                                //leaf that is unset
                                ostream << separator << "\"" << cur->attrName << "$" << cur->attrConvention << "$" << cur->attrPurpose << "\":\"UNSET\"";
                                separator = ",";
                                (*totalStreamed)++;
                        }
                }

                if (flattenPackList == ESMF_FALSE) {
                        ostream << "}";
                }
                output = ostream.str();

                return ESMF_SUCCESS;
        }

    //-----------------------------------------------------------------------------
        #undef  ESMC_METHOD
    #define ESMC_METHOD "streamAttributePackToJSON"

        int Attribute::streamAttributePackToJSON(vector<Attribute *> attrVector,
                        ESMC_Logical flattenPackList, ESMC_Logical includeUnset,
                        ESMC_Logical includeLinks,
                        string &output, int *totalStreamed) const {

                int localrc;
                ostringstream ostream;
                const char *separator = "";
                *totalStreamed = 0;
                int localStreamed = 0;

                //printf("Enter streamAttributePackToJSON\n");

                if (flattenPackList == ESMF_FALSE) {
                        ostream << "[";
                }

                for (int i = 0; i < attrVector.size(); i++) {
                        string stringJSON;
                        localrc = attrVector.at(i)->streamAttributeToJSON(flattenPackList,
                                        includeUnset, includeLinks, stringJSON, &localStreamed);
                        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &localrc)) return localrc;
                        if (localStreamed > 0) {
                                ostream << separator << stringJSON;
                                separator = ",";
                                *totalStreamed = *totalStreamed + localStreamed;
                        }
                }

                if (flattenPackList == ESMF_FALSE) {
                        ostream << "]";
                }
                output = ostream.str();

                //printf("Exit streamAttributePackToJSON\n");

                return ESMF_SUCCESS;
        }


    //-----------------------------------------------------------------------------
        #undef  ESMC_METHOD
    #define ESMC_METHOD "streamAttributeLinksToJSON"

        int Attribute::streamAttributeLinksToJSON(vector<Attribute *> attrVector,
                        ESMC_Logical flattenPackList, ESMC_Logical includeUnset,
                        string &output, int *totalStreamed) const {

                int localrc;
                ostringstream ostream;
                const char *separator = "";
                *totalStreamed = 0;
                int localStreamed = 0;

                ostream << "[";

                for (int i = 0; i < attrVector.size(); i++) {
                        string stringJSON;
                        localrc = attrVector.at(i)->streamAttributeRootToJSON(flattenPackList,
                                        includeUnset, ESMF_TRUE, stringJSON, &localStreamed);
                        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                          &localrc)) return localrc;
                        if (localStreamed > 0) {
                                ostream << separator << stringJSON;
                                separator = ",";
                                *totalStreamed = *totalStreamed + localStreamed;
                        }
                }

                ostream << "]";

                output = ostream.str();

                return ESMF_SUCCESS;
        }


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

