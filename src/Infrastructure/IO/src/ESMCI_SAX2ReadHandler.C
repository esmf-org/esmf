// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC IO_XML SAX2ReadHandler method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++
// {\tt ESMCI\_SAX2ReadHandler} methods declared
// in the companion file {\tt ESMCI\_SAX2ReadHandler.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_SAX2ReadHandler.C"

// associated class definition file
#include "ESMCI_SAX2ReadHandler.h"

//#include <iostream>
//using std::cout;
//using std::endl;

#include <exception>
using std::exception;

#include "ESMCI_LogErr.h"

using std::string;
using std::vector;

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI{

#ifdef ESMF_XERCES

#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2ReadHandler::SAX2ReadHandler()"

// constructor
SAX2ReadHandler::SAX2ReadHandler(ESMCI::Attribute *attr) : DefaultHandler()
{
  this->attr = attr;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2ReadHandler::startElement()"

// startElement parse-event handler
void SAX2ReadHandler::startElement(const XMLCh* const uri,
                                   const XMLCh* const localname,
                                   const XMLCh* const qname,
                                   const Attributes&  attrs)
{
    int status;
    char* msg1 = XMLString::transcode(uri);
    char* msg2 = XMLString::transcode(localname);
    char* msg3 = XMLString::transcode(qname);
    //cout << "Start of element: "<< msg1 << ", " << msg2 << ", "
    //                            << msg3 << ", " << endl;
    // remember this name to associate with the subsequent value callback
    //   to SAX2ReadHandler::characters()
    // TODO:  qname better than localname?
    string cqname(msg3, strlen(msg3));
    cqname.resize(cqname.find_last_not_of(" ")+1);
    if (cqname == "component" ||
        cqname == "custom_component") {
        //cout << "setting 'comp' object type for " << cqname << endl;
      this->object = "comp";
    } else if (cqname == "variable") {
      this->object = "field";
    } else if (cqname == "GridSpec") {
      this->object = "grid";
    // TODO:  state, grid, others
    } else this->qname = cqname;

    char* Qname=NULL;
    char* URI=NULL;
    char* local=NULL;
    char* type=NULL;
    char* value=NULL;

    for (XMLSize_t i = 0; i < attrs.getLength(); i++) {
      Qname = XMLString::transcode(attrs.getQName(i));
      URI   = XMLString::transcode(attrs.getURI(i));
      local = XMLString::transcode(attrs.getLocalName(i));
      type  = XMLString::transcode(attrs.getType(i));
      value = XMLString::transcode(attrs.getValue(i));
      //cout <<"  Attributes: " << Qname << ", " << URI << ", " << local << ", "
      //                         << type << ", " << value << endl;

      Attribute *attPackAttr;
      string cname(Qname, strlen(Qname));
      string cvalue(value, strlen(value));
      cname.resize(cname.find_last_not_of(" ")+1);
      cvalue.resize(cvalue.find_last_not_of(" ")+1);
      vector<string> valueVector;
      valueVector.push_back(cvalue);

      if (cname.empty()) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "bad attribute name conversion", ESMC_CONTEXT, &status);
          //if (rc) *rc = status;  TODO
          return;
      }
 
      if (cvalue.empty()) {
          ESMC_LogDefault.Write("Attribute has an empty value argument",
                                  ESMC_LOGMSG_INFO, ESMC_CONTEXT);
          cvalue = '\0';
      }

      if (cname == "convention") {
        this->convention = value;
      }
      else if (cname == "purpose") {
        this->purpose = value;
      }
      
      // Set the attribute on the object, if neither convention nor purpose
      if (cname != "convention" && cname != "purpose") {
        if (!this->convention.empty() && !this->purpose.empty()) {
          // string attPackInstanceName;
          attPackAttr = this->attr->AttPackGetAttribute(cname, ESMC_ATTNEST_ON);
          status = attPackAttr->AttrModifyValue(ESMC_TYPEKIND_CHARACTER, 1, &valueVector);
//          status = this->attr->AttPackSet(cname, ESMC_TYPEKIND_CHARACTER, 1,
//                                          &valueVector, this->convention,
//                                                   this->purpose, 
//                                                   this->object,
//                                                   attPackInstanceName); 
          //cout << "setting xml attribute into AttPack" << endl;
        } else {
          // TODO:  handle one or the other (xor) of conv, purp (error) ?
          status = this->attr->AttributeSet(cname, valueVector.size(), &valueVector);
          //cout << "setting xml loose attribute" << endl;
        }
        if (status != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "failed setting attribute value", ESMC_CONTEXT, &status);
        }
      }
    }

    // Create attribute package, if just now specified
    if (cqname == "attribute_package" && 
        !convention.empty() && !purpose.empty()) {
      //cout << "creating attribute package, " << convention << ", "
      //                                      << purpose << endl;
      status = attr->AttPackCreateStandard(convention, purpose, object);
      //cout << "AttPackCreateStandard() status = " << status << endl;
      if (status != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                  "failed creating attribute package", ESMC_CONTEXT, &status);
      }
    }

    if (value != NULL) XMLString::release(&value);
    if (type  != NULL) XMLString::release(&type);
    if (local != NULL) XMLString::release(&local);
    if (URI   != NULL) XMLString::release(&URI);
    if (Qname != NULL) XMLString::release(&Qname);

    XMLString::release(&msg3);
    XMLString::release(&msg2);
    XMLString::release(&msg1);

} // SAX2ReadHandler::startElement()

#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2ReadHandler::characters()"

void SAX2ReadHandler::characters(const XMLCh *const chars,
                                 const XMLSize_t length)
{
    int status;
    char* msg = XMLString::transcode(chars);
    if (msg[0] != '\n' &&
        msg[0] != ' ') {  // TODO: can these callbacks be prevented?
      //cout << "      characters: "<< msg << endl;
      //cout << "      length: "<< length << " characters" << endl;

      Attribute *attPackAttr;
      string cvalue(msg, length);
      cvalue.resize(cvalue.find_last_not_of(" ")+1);
      vector<string> valueVector;
      valueVector.push_back(cvalue);

      if (this->qname.empty()) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
               "no attribute name to associate value to", ESMC_CONTEXT, &status);
          //if (rc) *rc = status;  TODO
          return;
      }
 
      if (cvalue.empty()) {
          ESMC_LogDefault.Write("Attribute has an empty value argument",
                                 ESMC_LOGMSG_INFO, ESMC_CONTEXT);
          cvalue = '\0';
      }

      // Set the attribute on the object
      if (!this->convention.empty() && !this->purpose.empty()) {
        // string attPackInstanceName;
        attPackAttr = this->attr->AttPackGetAttribute(this->qname, ESMC_ATTNEST_ON);
        status = attPackAttr->AttrModifyValue(ESMC_TYPEKIND_CHARACTER, 1, &valueVector);
//        status = this->attr->AttPackSet(this->qname, ESMC_TYPEKIND_CHARACTER, 1,
//                                        &valueVector, this->convention,
//                                                 this->purpose, 
//                                                 this->object,
//                                                 attPackInstanceName); 
        //cout << "set xml element on AttPack" << endl;
      } else {
        // TODO:  handle one or the other (xor) of conv, purp (error) ?
        status = this->attr->AttributeSet(this->qname, valueVector.size(), &valueVector);
        //cout << "set loose xml element, " << this->qname << "=" << cvalue << endl;
      }
      if (status != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
               "failed setting attribute value", ESMC_CONTEXT, &status);
      }
    }
    XMLString::release(&msg);

} // SAX2ReadHandler::characters

#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2ReadHandler::endElement()"

void SAX2ReadHandler::endElement(const XMLCh* const uri,
                                 const XMLCh* const localname,
                                 const XMLCh* const qname)
{
    int status;
    char* msg1 = XMLString::transcode(uri);
    char* msg2 = XMLString::transcode(localname);
    char* msg3 = XMLString::transcode(qname);
    //cout << "End of element: "<< msg1 << ", " << msg2 << ", "
    //                          << msg3 << ", " << endl;
    // TODO:  qname better than localname?
    this->qname.clear();

    // if end of attribute package, clear convention, purpose and object type
    if (strncmp(msg3, "attribute_package", 17) == 0) {
      convention.clear();
      purpose.clear();
      object.clear();
    }

    XMLString::release(&msg3);
    XMLString::release(&msg2);
    XMLString::release(&msg1);

} // SAX2ReadHandler::endElement()


// constructor
SAX2ErrorHandler::SAX2ErrorHandler(void) : ErrorHandler()
{
  // need to do any more?
}

#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2ErrorHandler::warning()"

void SAX2ErrorHandler::warning(const SAXParseException& exc)
{
    // TODO: Fix:  (...) is passed instead of valid reference to exc,
    //       resulting in crash (shown by gdb).  However, a simple C++ test
    //       program doing the same thing works!  Something about ESMF
    //       F90/C/C++ mixed environment? platform specific? 
    //       Occurs on at least gfortran 4.4.0 and intel 11.0.083

#if 0
    const XMLCh*  xid = exc.getPublicId();
    if (xid == 0) xid = exc.getSystemId();

    char* id  = XMLString::transcode(xid);    
    char* msg = XMLString::transcode(exc.getMessage());

    char logMsg[ESMF_MAXSTR];
    sprintf(logMsg, "SAX2 parse warning in %s, line %d, column %d, "
                    "message is: %s\n", id, exc.getLineNumber(),
                                            exc.getColumnNumber(), msg);
    ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);

    XMLString::release(&msg);
    XMLString::release(&id);
#else
    char logMsg[ESMF_MAXSTR];
    sprintf(logMsg, "SAX2 warning\n");
    ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
#endif

} // SAX2ErrorHandler::warning()

#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2ErrorHandler::error()"

void SAX2ErrorHandler::error(const SAXParseException& exc)
{
    // TODO: Fix:  (...) is passed instead of valid reference to exc,
    //       resulting in crash (shown by gdb).  However, a simple C++ test
    //       program doing the same thing works!  Something about ESMF
    //       F90/C/C++ mixed environment? platform specific? 
    //       Occurs on at least gfortran 4.4.0 and intel 11.0.083.

#if 0
    const XMLCh*  xid = exc.getPublicId();
    if (xid == 0) xid = exc.getSystemId();

    char* id  = XMLString::transcode(xid);    
    char* msg = XMLString::transcode(exc.getMessage());

    char logMsg[ESMF_MAXSTR];
    sprintf(logMsg, "SAX2 parse error in %s, line %d, column %d, "
                    "message is: %s\n", id, exc.getLineNumber(),
                                            exc.getColumnNumber(), msg);
    ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_ERROR, ESMC_CONTEXT);

    XMLString::release(&msg);
    XMLString::release(&id);

    throw exc;
#else
    char logMsg[ESMF_MAXSTR];
    sprintf(logMsg, "SAX2 parse error\n");
    ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    throw exception();
#endif

} // SAX2ErrorHandler::error()

#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2ErrorHandler::fatalError()"

void SAX2ErrorHandler::fatalError(const SAXParseException& exc)
{
    // TODO: Fix:  (...) is passed instead of valid reference to exc,
    //       resulting in crash (shown by gdb).  However, a simple C++ test
    //       program doing the same thing works!  Something about ESMF
    //       F90/C/C++ mixed environment? platform specific? 
    //       Occurs on at least gfortran 4.4.0 and intel 11.0.083.

#if 0
    const XMLCh*  xid = exc.getPublicId();
    if (xid == 0) xid = exc.getSystemId();

    char* id  = XMLString::transcode(xid);    
    char* msg = XMLString::transcode(exc.getMessage());

    char logMsg[ESMF_MAXSTR];
    sprintf(logMsg, "SAX2 parse fatal error in %s, line %d, column %d, "
                    "message is: %s\n", id, exc.getLineNumber(),
                                            exc.getColumnNumber(), msg);
    ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_ERROR, ESMC_CONTEXT);

    XMLString::release(&msg);
    XMLString::release(&id);

    throw exc;
#else
    char logMsg[ESMF_MAXSTR];
    sprintf(logMsg, "SAX2 parse fatal error\n");
    ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_ERROR, ESMC_CONTEXT);
    throw exception();
#endif

} // SAX2ErrorHandler::fatalError()

#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2ErrorHandler::resetErrors()"

void SAX2ErrorHandler::resetErrors(void)
{
  // TODO:  can reset a failed flag if needed
}

#endif // ESMF_XERCES

} // end namespace ESMCI
