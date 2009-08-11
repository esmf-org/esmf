// $Id: ESMCI_IO.C,v 1.9 2009/08/11 05:29:35 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC IO method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO} methods declared
// in the companion file {\tt ESMCI\_IO.h}
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMCI_IO.C"

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
 #include <ESMCI_IO.h>

 using namespace std; 

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_IO.C,v 1.9 2009/08/11 05:29:35 eschwab Exp $";
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
    cout << "Start of element: "<< msg1 << ", " << msg2 << ", "
                                << msg3 << ", " << endl;
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
      cout << "  Attributes: " << Qname << ", " << URI << ", " << local << ", "
                               << type << ", " << value << endl;

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
      cout << "creating attribute package, " << convention << ", "
                                             << purpose << endl;
      status = attr->AttPackCreateStandard(convention, purpose, object);
      cout << "AttPackCreateStandard() status = " << status << endl;
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
      cout << "      characters: "<< msg << endl;
      cout << "      length: "<< length << " characters" << endl;

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
    cout << "End of element: "<< msg1 << ", " << msg2 << ", "
                              << msg3 << ", " << endl;
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
    cout << "SAX XML Fatal Error: " << message
         << " at line: " << exception.getLineNumber()
         << endl;
    // TODO:  proper ESMC_Log* call
    XMLString::release(&message);

} // MySAX2Handler::fatalError()

#endif

namespace ESMCI{

// initialize static io instance counter
// TODO: inherit from ESMC_Base class
int IO::count=0;

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the IO routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_IOCreate - Allocates and Initializes an IO object
//
// !INTERFACE:
      IO *ESMCI_IOCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated IO
//
// !ARGUMENTS:
      int                nameLen,          // in
      const char        *name,             // in
      Attribute         *attr,             // in
      int               *rc) {             // out - return code

// !DESCRIPTION:
//      Allocates and Initializes a {\tt ESMC\_IO} with given values
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI_IOCreate(new)"

    int returnCode;
    IO *io;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // allocate an io object & set defaults via constructor
    try {
      io = new IO;
    }
    catch (...) {
      ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);
      return(ESMC_NULL_POINTER);
    }

    // TODO: use inherited methods from ESMC_Base
    if (name != ESMC_NULL_POINTER) {
      if (nameLen < ESMF_MAXSTR) {
        strncpy(io->name, name, nameLen);
        io->name[nameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(io->name, name, ESMF_MAXSTR-1);
        io->name[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "io name %s, length >= ESMF_MAXSTR; truncated.",
                name);
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_WARN,ESMC_CONTEXT);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } else {
      // create default name "IONNN"
      sprintf(io->name, "IO%3.3d\0", io->id);
    }

    if (attr != ESMC_NULL_POINTER) io->attr = attr;

// TODO 

    // TODO returnCode = io->validate();
    returnCode = ESMF_SUCCESS;
    ESMC_LogDefault.MsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);
    return(io);

 } // end ESMCI_IOCreate (new)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_IODestroy - free an IO created with Create
//
// !INTERFACE:
      int ESMCI_IODestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      IO **io) {  // in - IO to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys an IO object previously allocated
//      via an {\tt ESMCI\_IOCreate} routine.  Define for deep classes only.
//
//EOP

   // TODO: io->destruct(); constructor calls it!  ?
   delete *io; // ok to delete null pointer

   *io = ESMC_NULL_POINTER;
   return(ESMF_SUCCESS);

 } // end ESMCI_IODestroy

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO::read - Performs a read on an IO object
//
// !INTERFACE:
      int IO::read(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
      int                fileNameLen,         // in
      const char        *fileName) {          // in

// !DESCRIPTION:
//      Reads an {\tt ESMC\_IO} object from file
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO::read()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // TODO: use inherited methods from ESMC_Base
    if (fileName != ESMC_NULL_POINTER) {
      // TODO: only use local of fileName this one time;
      //   don't change set IO member fileName
      if (fileNameLen < ESMF_MAXSTR) {
        strncpy(this->fileName, fileName, fileNameLen);
        this->fileName[fileNameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(this->fileName, fileName, ESMF_MAXSTR-1);
        this->fileName[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "io fileName %s, length >= ESMF_MAXSTR; truncated.",
                name);
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_WARN,ESMC_CONTEXT);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } else {
      // TODO use existing IO fileName member
    }

#ifdef ESMF_XERCES
    try {
        XMLPlatformUtils::Initialize();
    }
    catch (const XMLException& toCatch) {
        char* message = XMLString::transcode(toCatch.getMessage());
        cout << "Error during initialization! :\n";
        cout << "Exception message is: \n"
             << message << "\n";
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
        cout << "Exception message is: \n"
             << message << "\n";
        XMLString::release(&message);
        return ESMF_FAILURE; // TODO: specific ESMF rc
    }
    catch (const SAXParseException& toCatch) {
        char* message = XMLString::transcode(toCatch.getMessage());
        cout << "Exception message is: \n"
             << message << "\n";
        XMLString::release(&message);
        return ESMF_FAILURE; // TODO: specific ESMF rc
    }
    catch (...) {
        cout << "Unexpected Exception \n" ;
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

}  // end IO::read

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO - native C++ constructor
//
// !INTERFACE:
      IO::IO(void) {
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
 #define ESMC_METHOD "ESMCI::IO() native constructor"

    attr = ESMC_NULL_POINTER;
    name[0] = '\0';
    id = ++count;  // TODO: inherit from ESMC_Base class
    // copy = false;  // TODO: see notes in constructors and destructor below

 } // end IO

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO - native C++ constructor
//
// !INTERFACE:
      IO::IO(Attribute* attribute) {
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
 #define ESMC_METHOD "ESMCI::IO() native constructor"

    attr = attribute;
    attr->ESMC_Print(); // TODO: for debug, comment out
    // create default name "IONNN"
    id = ++count;  // TODO: inherit from ESMC_Base class
    sprintf(name, "IO%3.3d\0", id);
    // copy = false;  // TODO: see notes in constructors and destructor below

 } // end IO

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~IO - native C++ destructor
//
// !INTERFACE:
      IO::~IO(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  // TODO: Decrement static count for one less object; but don't decrement   //       for copies.  Must create and set a copy flag property to detect.
  //       Also must set copy flag in copy constructor and overloaded
  //       assignment method, and provide interface from F90.
  // if (!copy) count--;

 } // end ~IO

}  // end namespace ESMCI
