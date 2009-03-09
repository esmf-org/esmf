// $Id: ESMCI_IO.C,v 1.1 2009/03/09 05:59:17 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC Clock method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Clock} methods declared
// in the companion file {\tt ESMCI\_Clock.h}
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMCI_IO.C"

 // higher level, 3rd party or system includes here
 #include <stdio.h>
 #include <string.h>
 #include <ctype.h>
 #include <iostream>

 #include <xercesc/sax2/SAX2XMLReader.hpp>
 #include <xercesc/sax2/XMLReaderFactory.hpp>
 #include <xercesc/sax2/DefaultHandler.hpp>
 #include <xercesc/sax2/Attributes.hpp>
 #include <xercesc/util/XMLString.hpp>

 #include <ESMCI_LogErr.h>
 #include <ESMF_LogMacros.inc>

 // associated class definition file
 #include <ESMCI_IO.h>

 using namespace std; 

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_IO.C,v 1.1 2009/03/09 05:59:17 eschwab Exp $";
//-------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "MySAX2Handler::MySAX2Handler()"

MySAX2Handler::MySAX2Handler(ESMC_Base *base) : DefaultHandler()
{
  this->base = base;
}

#undef  ESMC_METHOD
#define ESMC_METHOD "MySAX2Handler::startElement()"

void MySAX2Handler::startElement(const   XMLCh* const    uri,
                            const   XMLCh* const    localname,
                            const   XMLCh* const    qname,
                            const   Attributes&     attrs)
{
    char* msg1 = XMLString::transcode(uri);
    char* msg2 = XMLString::transcode(localname);
    char* msg3 = XMLString::transcode(qname);
    cout << "I saw element: "<< msg1 << ", " << msg2 << ", "
                             << msg3 << ", " << endl;

    // remember this name to associate with the subsequent value callback
    //   to MySAX2Handler::characters()
    // TODO:  qname better than localname?
    // TODO:  check length, use C++ string
    strcpy(this->qname, msg3);

    char* Qname=NULL;
    char* URI=NULL;
    char* local=NULL;
    char* type=NULL;
    char* value=NULL;

    for (XMLSize_t i = 0; i < attrs.getLength(); i++) {
      Qname = XMLString::transcode(attrs.getQName(i));
      URI = XMLString::transcode(attrs.getURI(i));
      local = XMLString::transcode(attrs.getLocalName(i));
      type = XMLString::transcode(attrs.getType(i));
      value = XMLString::transcode(attrs.getValue(i));
      cout << "  Attributes: " << Qname << ", " << URI << ", " << local << ", "
                               << type << ", " << value << endl;
    }

    if (value != NULL) XMLString::release(&value);
    if (type != NULL) XMLString::release(&type);
    if (local != NULL) XMLString::release(&local);
    if (URI != NULL) XMLString::release(&URI);
    if (Qname != NULL) XMLString::release(&Qname);

    XMLString::release(&msg3);
    XMLString::release(&msg2);
    XMLString::release(&msg1);
}

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

    string cname(this->qname, strlen(this->qname));
    string cvalue(msg, length);
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

      // Set the attribute on the object.
      status = this->base->root.AttributeSet(cname, &cvalue);

      // TODO: the following is based on F90->C->C++ glue code call for F90
      //    ESMF_AttributeSet(gridcomp, ...), which appears not to work;
      //    produces garbage chars appended after good chars when retrieved
      //    from F90 via ESMF_AttributeGet().
     // status = this->base->root.AttPackSet(cname, ESMC_TYPEKIND_CHARACTER, 1, 
     //                                      &cvalue, "ESG", "general", "comp");

      if (status != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                             "failed setting attribute value", &status);
      }
    }
    XMLString::release(&msg);
}

#undef  ESMC_METHOD
#define ESMC_METHOD "MySAX2Handler::fatalError()"

void MySAX2Handler::fatalError(const SAXParseException& exception)
{
    char* message = XMLString::transcode(exception.getMessage());
    cout << "Fatal Error: " << message
         << " at line: " << exception.getLineNumber()
         << endl;
    // TODO:  proper ESMC_Log* call
    XMLString::release(&message);
}

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
      ESMC_Base         *base,             // in
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

    if (base != ESMC_NULL_POINTER) io->base = base;

// TODO 

    // TODO returnCode = io->validate();
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

    try {
        XMLPlatformUtils::Initialize();
    }
    catch (const XMLException& toCatch) {
        char* message = XMLString::transcode(toCatch.getMessage());
        cout << "Error during initialization! :\n";
        cout << "Exception message is: \n"
             << message << "\n";
        XMLString::release(&message);
        return 1;
    }

    SAX2XMLReader* parser = XMLReaderFactory::createXMLReader();
    parser->setFeature(XMLUni::fgSAX2CoreValidation, true);
    parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);   // optional

    MySAX2Handler* myHandler = new MySAX2Handler(base);
    parser->setContentHandler(myHandler);
    parser->setErrorHandler(myHandler);

    try {
        // Xerces C++ SAX2 API reads the XML file, producing callbacks to
        //   MySAX2Handler::startElement() and MySAX2Handler::characters()
        //   for each XML element
        parser->parse(this->fileName); 
    }
    catch (const XMLException& toCatch) {
        char* message = XMLString::transcode(toCatch.getMessage());
        cout << "Exception message is: \n"
             << message << "\n";
        XMLString::release(&message);
        return -1;
    }
    catch (const SAXParseException& toCatch) {
        char* message = XMLString::transcode(toCatch.getMessage());
        cout << "Exception message is: \n"
             << message << "\n";
        XMLString::release(&message);
        return -1;
    }
    catch (...) {
        cout << "Unexpected Exception \n" ;
        return -1;
    }

    delete myHandler;
    delete parser;

    XMLPlatformUtils::Terminate();

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

    name[0] = '\0';
    id = ++count;  // TODO: inherit from ESMC_Base class
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
