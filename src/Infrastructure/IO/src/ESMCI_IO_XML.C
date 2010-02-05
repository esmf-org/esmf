// $Id: ESMCI_IO_XML.C,v 1.4.2.1 2010/02/05 19:58:03 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC IO_XML method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO\_XML} methods declared
// in the companion file {\tt ESMCI\_IO\_XML.h}
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMCI_IO_XML.C"

 // higher level, 3rd party or system includes here
 #include <stdio.h>
 #include <string.h>
 #include <ctype.h>
 #include <iostream>

#ifdef ESMF_XERCES
 #include <xercesc/sax2/SAX2XMLReader.hpp>
 #include <xercesc/sax2/XMLReaderFactory.hpp>
 #include <xercesc/sax2/DefaultHandler.hpp>
 #include <xercesc/sax2/Attributes.hpp>
 #include <xercesc/util/XMLString.hpp>
#endif

 #include <ESMCI_LogErr.h>
 #include <ESMF_LogMacros.inc>

 // associated class definition file
 #include <ESMCI_IO_XML.h>

 using namespace std; 

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_IO_XML.C,v 1.4.2.1 2010/02/05 19:58:03 svasquez Exp $";
//-------------------------------------------------------------------------

#ifdef ESMF_XERCES

#undef  ESMC_METHOD
#define ESMC_METHOD "MySAX2Handler::MySAX2Handler()"

// constructor
MySAX2Handler::MySAX2Handler(ESMCI::Attribute *attr) : DefaultHandler()
{
  this->attr = attr;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "MySAX2Handler::startElement()"

// startElement parse-event handler
void MySAX2Handler::startElement(const XMLCh* const uri,
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
    //   to MySAX2Handler::characters()
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

} // MySAX2Handler::startElement()

#undef  ESMC_METHOD
#define ESMC_METHOD "MySAX2Handler::characters()"

void MySAX2Handler::characters(const XMLCh *const chars,
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

} // MySAX2Handler::characters

#undef  ESMC_METHOD
#define ESMC_METHOD "MySAX2Handler::endElement()"

void MySAX2Handler::endElement(const XMLCh* const uri,
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

} // MySAX2Handler::endElement()

#undef  ESMC_METHOD
#define ESMC_METHOD "MySAX2Handler::fatalError()"

void MySAX2Handler::fatalError(const SAXParseException& exception)
{
    char* message = XMLString::transcode(exception.getMessage());
    //cout << "SAX XML Fatal Error: " << message
    //     << " at line: " << exception.getLineNumber()
    //     << endl;
    // TODO:  proper ESMC_Log* call
    XMLString::release(&message);

} // MySAX2Handler::fatalError()

#endif

namespace ESMCI{

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the IO_XML routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_IO_XMLCreate - Allocates and Initializes an IO_XML object
//
// !INTERFACE:
      IO_XML *ESMCI_IO_XMLCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated IO_XML
//
// !ARGUMENTS:
      int                nameLen,          // in
      const char        *name,             // in
      Attribute         *attr,             // in
      int               *rc) {             // out - return code

// !DESCRIPTION:
//      Allocates and Initializes a {\tt ESMC\_IO\_XML} with given values
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI_IO_XMLCreate(new)"

    int returnCode;
    IO_XML *io_xml;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // allocate an io object & set defaults via constructor
    try {
      io_xml = new IO_XML;
    }
    catch (...) {
      ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);
      return(ESMC_NULL_POINTER);
    }

    if (name != ESMC_NULL_POINTER) {
      // use given name
      returnCode = io_xml->ESMC_BaseSetF90Name((char*) name, nameLen);
    } else {
      // create default name "IO_XML<ID>"
      returnCode = io_xml->ESMC_BaseSetName((const char*) ESMC_NULL_POINTER,
                                            "IO_XML");
    }
    ESMC_LogDefault.MsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);

    if (attr != ESMC_NULL_POINTER) io_xml->attr = attr;

    // TODO returnCode = io_xml->validate();
    returnCode = ESMF_SUCCESS;
    ESMC_LogDefault.MsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);
    return(io_xml);

 } // end ESMCI_IO_XMLCreate (new)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_IO_XMLDestroy - free an IO_XML created with Create
//
// !INTERFACE:
      int ESMCI_IO_XMLDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      IO_XML **io_xml) {  // in - IO_XML to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys an IO object previously allocated
//      via an {\tt ESMCI\_IO\_XMLCreate} routine. Define for deep classes only.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (io_xml == ESMC_NULL_POINTER || *io_xml == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to io_xml", &rc);
    return rc;
  }

  try{
    // destruct Array object
    (*io_xml)->destruct();
    // mark as invalid object
    (*io_xml)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
 } // end ESMCI_IO_XMLDestroy

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_XML::read - Performs a read on an IO object
//
// !INTERFACE:
      int IO_XML::read(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
      int                fileNameLen,         // in
      const char        *fileName) {          // in

// !DESCRIPTION:
//      Reads an {\tt ESMC\_IO\_XML} object from file
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_XML::read()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    if (fileName != ESMC_NULL_POINTER) {
      // TODO: only use local of fileName this one time;
      //   don't change set IO_XML member fileName
      if (fileNameLen < ESMF_MAXSTR) {
        strncpy(this->fileName, fileName, fileNameLen);
        this->fileName[fileNameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(this->fileName, fileName, ESMF_MAXSTR-1);
        this->fileName[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "io_xml fileName %s, length >= ESMF_MAXSTR; truncated.",
                fileName);
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_WARN,ESMC_CONTEXT);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } else {
      // TODO use existing IO_XML fileName member
    }

#ifdef ESMF_XERCES
    try {
        XMLPlatformUtils::Initialize();
    }
    catch (const XMLException& toCatch) {
        char* message = XMLString::transcode(toCatch.getMessage());
        //cout << "Error during initialization! :\n";
        //cout << "Exception message is: \n"
        //     << message << "\n";
        XMLString::release(&message);
        return ESMF_FAILURE; // TODO: specific ESMF rc
    }

    SAX2XMLReader* parser = XMLReaderFactory::createXMLReader();
    parser->setFeature(XMLUni::fgSAX2CoreValidation, true);
    parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);   // optional

    MySAX2Handler* myHandler;
    myHandler = new MySAX2Handler(attr);
    parser->setContentHandler(myHandler);
    parser->setErrorHandler(myHandler);

    try {
        // Xerces C++ SAX2 API reads the XML file, producing callbacks to
        //   MySAX2Handler::startElement(), MySAX2Handler::characters(), and
        //   MySAX2Handler::endElement() for each XML element
        parser->parse(this->fileName); 
    }
    catch (const XMLException& toCatch) {
        char* message = XMLString::transcode(toCatch.getMessage());
        //cout << "Exception message is: \n"
        //     << message << "\n";
        XMLString::release(&message);
        return ESMF_FAILURE; // TODO: specific ESMF rc
    }
    catch (const SAXParseException& toCatch) {
        char* message = XMLString::transcode(toCatch.getMessage());
        //cout << "Exception message is: \n"
        //     << message << "\n";
        XMLString::release(&message);
        return ESMF_FAILURE; // TODO: specific ESMF rc
    }
    catch (...) {
        //cout << "Unexpected Exception \n" ;
        return ESMF_FAILURE; // TODO: specific ESMF rc
    }

    delete myHandler;
    delete parser;

    XMLPlatformUtils::Terminate();
#else
    // xerces library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;
#endif

    return (rc);

}  // end IO_XML::read

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_XML - native C++ constructor
//
// !INTERFACE:
      IO_XML::IO_XML(void)
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes for either C++ or F90, since {\tt ESMC\_IO\_XML} is a deep,
//      dynamically allocated class.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_XML() native constructor"

 : ESMC_Base(-1) {  // invoke ESMC_Base constructor with id=(-1); prevents
                    // Base id increment for non-distributed,
                    // non-reconcilable objects such as IO.
    attr = ESMC_NULL_POINTER;
    // create default name "IO_XML<ID>"
    ESMC_BaseSetName(ESMC_NULL_POINTER, "IO_XML");
    // copy = false;  // TODO: see notes in constructors and destructor below

 } // end IO_XML

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_XML - native C++ constructor
//
// !INTERFACE:
      IO_XML::IO_XML(Attribute* attribute)
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes for either C++ or F90, since {\tt ESMC\_IO} is a deep,
//      dynamically allocated class.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_XML() native constructor"

 : ESMC_Base(-1) {  // invoke ESMC_Base constructor with id=(-1); prevents
                    // Base id increment for non-distributed,
                    // non-reconcilable objects such as IO.
    attr = attribute;
    attr->ESMC_Print(); // TODO: for debug, comment out
    // create default name "IO_XML<ID>"
    ESMC_BaseSetName(ESMC_NULL_POINTER, "IO_XML");
    // copy = false;  // TODO: see notes in constructors and destructor below

 } // end IO_XML

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_XML - destruct()
//
// !INTERFACE:
void IO_XML::destruct(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Destruct an IO_XML object
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 } // end destruct()

}  // end namespace ESMCI
