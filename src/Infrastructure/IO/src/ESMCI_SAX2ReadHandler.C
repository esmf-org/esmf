// $Id: ESMCI_SAX2ReadHandler.C,v 1.1 2010/02/11 06:58:19 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
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
//
#define ESMC_FILENAME "ESMCI_SAX2ReadHandler.C"

#include <ESMCI_LogErr.h>
#include <ESMF_LogMacros.inc>

// associated class definition file
#include <ESMCI_SAX2ReadHandler.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_SAX2ReadHandler.C,v 1.1 2010/02/11 06:58:19 eschwab Exp $";
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
    if (cqname == "model_component") {
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

      string cname(Qname, strlen(Qname));
      string cvalue(value, strlen(value));
      cname.resize(cname.find_last_not_of(" ")+1);
      cvalue.resize(cvalue.find_last_not_of(" ")+1);

      if (cname.empty()) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                             "bad attribute name conversion", &status);
          //if (rc) *rc = status;  TODO
          return;
      }
 
      if (cvalue.empty()) {
          ESMC_LogDefault.Write("Attribute has an empty value argument",
                                  ESMC_LOG_INFO);
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
          status = this->attr->AttPackSet(cname, ESMC_TYPEKIND_CHARACTER, 1,
                                          &cvalue, this->convention,
                                                   this->purpose, 
                                                   this->object);
        } else {
          // TODO:  handle one or the other (xor) of conv, purp (error) ?
          status = this->attr->AttributeSet(cname, &cvalue);
        }
        if (status != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                               "failed setting attribute value", &status);
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
                             "failed creating attribute package", &status);
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

      string cvalue(msg, length);
      cvalue.resize(cvalue.find_last_not_of(" ")+1);

      if (this->qname.empty()) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "no attribute name to associate value to", &status);
          //if (rc) *rc = status;  TODO
          return;
      }
 
      if (cvalue.empty()) {
          ESMC_LogDefault.Write("Attribute has an empty value argument",
                                 ESMC_LOG_INFO);
          cvalue = '\0';
      }

      // Set the attribute on the object
      if (!this->convention.empty() && !this->purpose.empty()) {
        status = this->attr->AttPackSet(this->qname, ESMC_TYPEKIND_CHARACTER, 1,
                                        &cvalue, this->convention,
                                                 this->purpose, 
                                                 this->object);
      } else {
        // TODO:  handle one or the other (xor) of conv, purp (error) ?
        status = this->attr->AttributeSet(this->qname, &cvalue);
      }
      if (status != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                             "failed setting attribute value", &status);
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

#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2ReadHandler::fatalError()"

void SAX2ReadHandler::fatalError(const SAXParseException& exception)
{
    char* message = XMLString::transcode(exception.getMessage());
    //cout << "SAX XML Fatal Error: " << message
    //     << " at line: " << exception.getLineNumber()
    //     << endl;
    // TODO:  proper ESMC_Log* call
    XMLString::release(&message);

} // SAX2ReadHandler::fatalError()

#endif // ESMF_XERCES

} // end namespace ESMCI
