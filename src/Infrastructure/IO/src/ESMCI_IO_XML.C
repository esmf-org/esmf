// $Id: ESMCI_IO_XML.C,v 1.9 2010/04/13 06:00:49 eschwab Exp $
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
 #include <stdarg.h>
 #include <string>
 #include <memory> // std::auto_ptr

#ifdef ESMF_XERCES
 #include <xercesc/sax2/SAX2XMLReader.hpp>
 #include <xercesc/sax2/XMLReaderFactory.hpp>
 #include <xercesc/internal/VecAttributesImpl.hpp>
 #include <xercesc/util/RefVectorOf.hpp>
 #include <xercesc/validators/common/Grammar.hpp>
 #include <xercesc/framework/XMLGrammarPoolImpl.hpp>
#endif
 
 #include <ESMCI_LogErr.h>
 #include <ESMF_LogMacros.inc>

 // associated class definition files
 #include <ESMCI_IO_XML.h>
 #include <ESMCI_SAX2ReadHandler.h>
 #include <ESMCI_SAX2WriteHandler.h>

 using namespace std; 

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_IO_XML.C,v 1.9 2010/04/13 06:00:49 eschwab Exp $";
//-------------------------------------------------------------------------


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
      int                fileNameLen,      // in
      const char        *fileName,         // in
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

    if (fileName != ESMC_NULL_POINTER) {
      if (fileNameLen < ESMF_MAXSTR) {
        strncpy(io_xml->fileName, fileName, fileNameLen);
        io_xml->fileName[fileNameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(io_xml->fileName, fileName, ESMF_MAXSTR-1);
        io_xml->fileName[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "io_xml fileName %s, length >= ESMF_MAXSTR; truncated.",
                fileName);
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_WARN,ESMC_CONTEXT);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    }

    if (attr != ESMC_NULL_POINTER) io_xml->attr = attr;

    // TODO returnCode = io_xml->validate();
    returnCode = ESMF_SUCCESS;
    ESMC_LogDefault.MsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);

#ifdef ESMF_XERCES
    try {
        XMLPlatformUtils::Initialize();
    }
    catch (const XMLException& toCatch) {
        char logMsg[ESMF_MAXSTR];
        char* message = XMLString::transcode(toCatch.getMessage());
        sprintf(logMsg, "Error during Xerces initialization! :\n"
                        "  Exception message is: %s\n", message);
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_WARN, ESMC_CONTEXT);
        XMLString::release(&message);
        returnCode = ESMC_RC_LIB;
        ESMC_LogDefault.MsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);
    }
#endif

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
    // destruct IO object
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
  
#ifdef ESMF_XERCES
  XMLPlatformUtils::Terminate();
#endif

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
    // TODO:  move parser & readHandler instantiation to Create()/construct(),
    //        if we need to do multiple reads per IO_XML object lifetime. ?

    MemoryManager* mm (XMLPlatformUtils::fgMemoryManager);
    auto_ptr<XMLGrammarPool> gpool (new XMLGrammarPoolImpl (mm));

    auto_ptr<SAX2XMLReader> parser(
         XMLReaderFactory::createXMLReader(XMLPlatformUtils::fgMemoryManager,
                                          gpool.get())); 

    // Commonly useful configuration
    parser->setFeature (XMLUni::fgSAX2CoreNameSpaces, true);
    parser->setFeature (XMLUni::fgSAX2CoreNameSpacePrefixes, true);
    parser->setFeature (XMLUni::fgSAX2CoreValidation, true);

    // Enable validation
    parser->setFeature (XMLUni::fgXercesSchema, true);
    parser->setFeature (XMLUni::fgXercesSchemaFullChecking, true);
    parser->setFeature (XMLUni::fgXercesValidationErrorAsFatal, true);

    // Use the loaded grammar during parsing.
    parser->setFeature (XMLUni::fgXercesUseCachedGrammarInParse, true);

    // Don't load schemas from XML document's xsi:schemaLocation attributes
    parser->setFeature(XMLUni::fgXercesLoadSchema, false);

#if _XERCES_VERSION >= 30100
    // Allows all xsd's loaded into grammar pool to be searched automatically,
    // for matching root element, when parsing an xml file. New in Xerces 3.1.0
    parser->setFeature (XMLUni::fgXercesHandleMultipleImports, true);
#endif

    // TODO: explore for future use
    //parser->setFeature(XMLUni::fgXercesIdentityConstraintChecking, true);
    //parser->setFeature(XMLUni::fgXercesDynamic, false);

    SAX2ReadHandler readHandler(attr);
    parser->setContentHandler(&readHandler);
    SAX2ErrorHandler errorHandler;
    parser->setErrorHandler(&errorHandler);

    // TODO: explore for future use
    //DefaultHandler handler;
    //parser->setEntityResolver(&handler);

    // Read-in the ESMF xsd files, into grammar pool, for xml file validation.
    //   Order irrelevant with new XMLUni::fgXercesHandleMultipleImports 
    //   feature in Xerces 3.1.0. (see above).
    if (!parser->loadGrammar ("esmf_comp.xsd",
                              Grammar::SchemaGrammarType, true))
    {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "Unable to load esmf_comp.xsd file\n");
      ESMC_LogDefault.Write(logMsg, ESMC_LOG_ERROR, ESMC_CONTEXT);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_READ, ESMF_ERR_PASSTHRU,&rc);
      return rc;
    }
    if (!parser->loadGrammar ("esmf_grid.xsd",
                              Grammar::SchemaGrammarType, true))
    {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "Unable to load esmf_grid.xsd file\n");
      ESMC_LogDefault.Write(logMsg, ESMC_LOG_ERROR, ESMC_CONTEXT);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_READ, ESMF_ERR_PASSTHRU,&rc);
      return rc;
    }
    if (!parser->loadGrammar ("esmf_field.xsd",
                              Grammar::SchemaGrammarType, true))
    {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "Unable to load esmf_field.xsd file\n");
      ESMC_LogDefault.Write(logMsg, ESMC_LOG_ERROR, ESMC_CONTEXT);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_READ, ESMF_ERR_PASSTHRU,&rc);
      return rc;
    }

    // Lock the grammar pool. This is necessary if we plan to use the
    // same grammar pool in multiple threads (this way we can reuse the
    // same grammar in multiple parsers). Locking the pool disallows any
    // modifications to the pool, such as an attempt by one of the threads
    // to cache additional schemas.
    gpool->lockPool();

    try {
        // Xerces C++ SAX2 API reads the XML file, producing callbacks to
        //   SAX2ReadHandler::startElement(),
        //   SAX2ReadHandler::characters(), and
        //   SAX2ReadHandler::endElement() for each XML element
        parser->parse(this->fileName); 
    }
    catch (const XMLException& toCatch) {
        char logMsg[ESMF_MAXSTR];
        char* message = XMLString::transcode(toCatch.getMessage());
        sprintf(logMsg, "XML Parse Exception, message is: %s\n", message);
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_ERROR, ESMC_CONTEXT);
        XMLString::release(&message);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_READ, ESMF_ERR_PASSTHRU,&rc);
        return rc;
    }
    catch (const SAXParseException& toCatch) {
        char logMsg[ESMF_MAXSTR];
        char* message = XMLString::transcode(toCatch.getMessage());
        sprintf(logMsg, "SAX Parse Exception, message is: %s\n", message);
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_ERROR, ESMC_CONTEXT);
        XMLString::release(&message);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_READ, ESMF_ERR_PASSTHRU,&rc);
        return rc;
    }
    catch (...) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "Unexpected, unknown exception during SAX parse.\n");
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_ERROR, ESMC_CONTEXT);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_READ, ESMF_ERR_PASSTHRU,&rc);
        return rc;
    }
    // parser deleted by auto_ptr via either normal or exception exit

#else
    // xerces library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;
#endif

    return (rc);

}  // end IO_XML::read

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_XML::writeStartElement - Performs a write on an IO object
//
// !INTERFACE:
      int IO_XML::writeStartElement(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
      const string        &name,
      const int            indentLevel,
      const int            nPairs, ...) {
      // nPairs of (char *attrName, char *attrValue) var args

// !DESCRIPTION:
//      Part of writing an {\tt ESMC\_IO\_XML} object to file.
//      Maps to SAX2 startElement().
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_XML::writeStartElement()"

    va_list ap;
    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    va_start(ap, nPairs);

#ifdef ESMF_XERCES
    // if first write() call, instantiate a writeHandler for this IO_XML.
    // TODO:  move instantiation to Create()/construct(), if we ever
    //      create "r,w,rw" flags in Create(), as well as move
    //      readHandler/parser instantiation to Create()/construct().  ?
    if (writeHandler == ESMC_NULL_POINTER) {
      writeHandler = new SAX2WriteHandler(this->fileName, "LATIN1",
                                          XMLFormatter::UnRep_CharRef, false);
    }

    char  *attrName,  *attrValue;
    XMLCh *attrNameX, *attrValueX;
    XMLAttr *attr = NULL;
    XMLSize_t attrCount=0;
    RefVectorOf<XMLAttr> *attrList = NULL;
    VecAttributesImpl fAttrList;  // empty if no attrs passed in (nPairs = 0)

    // prep passed-in XML element attributes, if any
    if (nPairs > 0) {
      attrList = new RefVectorOf<XMLAttr>(nPairs, true); // adopt elems
      for (int i=0; i < nPairs; i++) {
        attrName  = va_arg(ap, char*);
        attrValue = va_arg(ap, char*);
        if (strlen(attrName) > 0) {
          attrNameX  = XMLString::transcode(attrName);
          attrValueX = XMLString::transcode(attrValue);
          attr = new XMLAttr(0, attrNameX, XMLUni::fgZeroLenString, attrValueX);
          attrList->addElement(attr);
          attrCount++;
          XMLString::release(&attrNameX);
          XMLString::release(&attrValueX);
        }
      }
      fAttrList.setVector(attrList, attrCount, NULL, true); // adopt elems
      // The above containers attrList and fAttrlist are set to
      // adopt their elements, so they will deallocate them upon their demise.
      // This happens when fAttrList goes out of scope at the end of this
      // method.  So attrList and attr do not need to be deleted explicitly
      // here.  Multiple XMLAttr allocations can be done with attr, each of
      // which resides in (is adopted by) attrList.
    }

    // indent if needed
    if (indentLevel > 0) {
      string indentSpaces;
      for(int i=0; i<indentLevel; i++) indentSpaces += "  ";
      XMLCh* indent = XMLString::transcode(indentSpaces.c_str());
      writeHandler->characters(indent, XMLString::stringLen(indent));
      XMLString::release(&indent);
    }
 
    // write out the element's name and its attributes
    //   <name attrName="attrValue" ... >
    XMLCh* qname = XMLString::transcode(name.c_str());
    writeHandler->startElement(XMLUni::fgZeroLenString,
                               XMLUni::fgZeroLenString,
                               qname, fAttrList);
    XMLString::release(&qname);

    // end of line
    XMLCh* newLine = XMLString::transcode("\n");
    writeHandler->characters(newLine, XMLString::stringLen(newLine));
    XMLString::release(&newLine);

#else
    // xerces library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;
#endif

    va_end(ap);
    return (rc);

}  // end IO_XML::writeStartElement

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_XML::writeElement - Performs a write on an IO object
//
// !INTERFACE:
      int IO_XML::writeElement(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
      const string        &name,
      const string        &value,
      const int           indentLevel) {

// !DESCRIPTION:
//      Part of writing an {\tt ESMC\_IO\_XML} object to file.
//      Maps to SAX2 characters(), optionally to startElement() & endElement().
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_XML::writeElement()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

#ifdef ESMF_XERCES
    // if first write() call, instantiate a writeHandler for this IO_XML.
    // TODO:  move instantiation to Create()/construct(), if we ever
    //      create "r,w,rw" flags in Create(), as well as move
    //      readHandler/parser instantiation to Create()/construct().  ?
    if (writeHandler == ESMC_NULL_POINTER) {
      writeHandler = new SAX2WriteHandler(this->fileName, "LATIN1",
                                          XMLFormatter::UnRep_CharRef, false);
    }

    // indent if needed
    if (indentLevel > 0) {
      string indentSpaces;
      for(int i=0; i<indentLevel; i++) indentSpaces += "  ";
      XMLCh* indent = XMLString::transcode(indentSpaces.c_str());
      writeHandler->characters(indent, XMLString::stringLen(indent));
      XMLString::release(&indent);
    }
 
    XMLCh* qname;
    if (!(name.empty())) {
      VecAttributesImpl fAttrList;  // empty attributes list
      qname = XMLString::transcode(name.c_str());
      // write out the start of the XML element <name>
      writeHandler->startElement(XMLUni::fgZeroLenString,
                                 XMLUni::fgZeroLenString,
                                 qname, fAttrList);
    }
         
    // write out element name's value <name>value</name>
    XMLCh* outChars = XMLString::transcode(value.c_str());
    writeHandler->characters(outChars, XMLString::stringLen(outChars));
    XMLString::release(&outChars);
         
    if (!(name.empty())) {
      // write out the end of the XML element </name>
      writeHandler->endElement(XMLUni::fgZeroLenString,
                               XMLUni::fgZeroLenString, qname);
      XMLString::release(&qname);

      // end of line
      XMLCh* newLine = XMLString::transcode("\n");
      writeHandler->characters(newLine, XMLString::stringLen(newLine));
      XMLString::release(&newLine);
    }
#else
    // xerces library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;
#endif

    return (rc);

}  // end IO_XML::writeElement

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_XML::writeEndElement - Performs a write on an IO object
//
// !INTERFACE:
      int IO_XML::writeEndElement(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
      const string        &name,
      const int            indentLevel) {

// !DESCRIPTION:
//      Part of writing an {\tt ESMC\_IO\_XML} object to file.
//      Maps to SAX2 endElement()
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_XML::writeEndElement()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

#ifdef ESMF_XERCES
    // if first write() call, instantiate a writeHandler for this IO_XML.
    // TODO:  move instantiation to Create()/construct(), if we ever
    //      create "r,w,rw" flags in Create(), as well as move
    //      readHandler/parser instantiation to Create()/construct().  ?
    if (writeHandler == ESMC_NULL_POINTER) {
      writeHandler = new SAX2WriteHandler(this->fileName, "LATIN1",
                                          XMLFormatter::UnRep_CharRef, false);
    }

    // indent if needed
    if (indentLevel > 0) {
      string indentSpaces;
      for(int i=0; i<indentLevel; i++) indentSpaces += "  ";
      XMLCh* indent = XMLString::transcode(indentSpaces.c_str());
      writeHandler->characters(indent, XMLString::stringLen(indent));
      XMLString::release(&indent);
    }
 
    // write out the end of the XML element </name>
    XMLCh* qname = XMLString::transcode(name.c_str());
    writeHandler->endElement(XMLUni::fgZeroLenString,
                             XMLUni::fgZeroLenString, qname);
    XMLString::release(&qname);

    // end of line
    XMLCh* newLine = XMLString::transcode("\n");
    writeHandler->characters(newLine, XMLString::stringLen(newLine));
    XMLString::release(&newLine);
#else
    // xerces library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;
#endif

    return (rc);

}  // end IO_XML::writeEndElement

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_XML::write - Performs a write on an IO object
//
// !INTERFACE:
      int IO_XML::write(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
      int                fileNameLen,         // in
      const char        *fileName,
      const char        *outChars,
      int                flag) {          // in

// !DESCRIPTION:
//      Writes an {\tt ESMC\_IO\_XML} object to file
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_XML::write()"

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
    // if first write() call, instantiate a writeHandler for this IO_XML.
    // TODO:  move instantiation to Create()/construct(), if we ever
    //      create "r,w,rw" flags in Create(), as well as move
    //      readHandler/parser instantiation to Create()/construct().  ?
    if (writeHandler == ESMC_NULL_POINTER) {
      writeHandler = new SAX2WriteHandler(this->fileName, "LATIN1",
                                          XMLFormatter::UnRep_CharRef, false);
    }

    switch (flag)
    {
      case 1:
        {
          VecAttributesImpl fAttrList;
          XMLCh* qname = XMLString::transcode(outChars);
          writeHandler->startElement(XMLUni::fgZeroLenString,
                                     XMLUni::fgZeroLenString,
                                     qname, fAttrList);
          XMLString::release(&qname);
        }
        break;
      case 2:
        {
          XMLCh* chars = XMLString::transcode(outChars);
          writeHandler->characters(chars, XMLString::stringLen(chars));
          XMLString::release(&chars);
        }
        break;
      case 3:
        {
          XMLCh* qname = XMLString::transcode(outChars);
          writeHandler->endElement(XMLUni::fgZeroLenString,
                                   XMLUni::fgZeroLenString, qname);
          XMLString::release(&qname);
        }
        break;
      default:
        // todo:  report unknown flag
        break;
    }
#else
    // xerces library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;
#endif

    return (rc);

}  // end IO_XML::write

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

#ifdef ESMF_XERCES
    writeHandler = ESMC_NULL_POINTER;
    // readHandler = ESMC_NULL_POINTER;  // TODO:  ?
    // parser = ESMC_NULL_POINTER;       // TODO:  ?
#endif

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

#ifdef ESMF_XERCES
    writeHandler = ESMC_NULL_POINTER;
    // readHandler = ESMC_NULL_POINTER;  // TODO:  ?
    // parser = ESMC_NULL_POINTER;       // TODO:  ?
#endif

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

#ifdef ESMF_XERCES
  delete writeHandler;  // NULL ok
  writeHandler = ESMC_NULL_POINTER;

  // delete readHandler;  // NULL ok  // TODO:
  // readHandler = ESMC_NULL_POINTER;

  // delete parser;  // NULL ok
  // parser = ESMC_NULL_POINTER;
#endif

 } // end destruct()

}  // end namespace ESMCI
