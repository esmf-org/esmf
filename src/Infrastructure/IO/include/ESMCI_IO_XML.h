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
// ESMF IO XML C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMCI_IO_XML_H
#define ESMCI_IO_XML_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMCI_Macros.h"

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI::IO_XML - Handles low-level XML IO for ESMF internals and
//  ESMF user API.  Translates between ESMF internals/API and specific XML
//  protocols/API such as SAX2 and DOM.  Xerces is used for SAX2 currently,
//  (via ESMCI::SAX2[Write,Read]Handler), although Xerces/DOM could 
//  be added via a separate class such as ESMCI::DOM.
//
// !DESCRIPTION:
//  TODO
//-------------------------------------------------------------------------
//
// !USES:
#include <fstream>
#include <string>
#include <stdarg.h>
#include "ESMCI_Base.h"           // inherited Base class
#include "ESMCI_SAX2WriteHandler.h"
// #include "ESMCI_SAX2ReadHandler.h"  // TODO: ?

namespace ESMCI{

// !PUBLIC TYPES:
 class IO_XML;
 class Attribute;

// !PRIVATE TYPES:

 // class definition type
 class IO_XML : public ESMC_Base { // inherit from ESMC_Base class
  private:   // corresponds to F90 module 'type ESMF_IO_XML' members
    Attribute *attr;    // root node of associated object's attributes
    std::string  fileName;
    std::string  schemaFileName;
#ifdef ESMF_XERCES
    SAX2WriteHandler* writeHandler;   // to file; a future use could be to 
                                      // write to a network protocol rather
                                      // than a file.
    // SAX2ReadHandler* readHandler;  // TODO:  multiple reads per
    // SAX2XMLReader* parser;         //        IO_XML object lifetime ?
#else
    std::ofstream writeFile;
#endif

// !PUBLIC MEMBER FUNCTIONS:

  public:
    // accessor methods

    // Read/Write (via SAX2 API)
    int read(const std::string& fileName,
             const std::string& schemaFileName);

    // maps to SAX2 startElement() & characters(), but not endElement();
    //   use to open a nested tag section
    int writeStartElement(const std::string& name,
                          const std::string& value,
                          const int     indentLevel,
                          const int     nPairs, ...); // nPairs of
                 // (char *attrName, char *attrValue)

    // maps to SAX2 startElement, characters() & endElement();
    //   use to write an entire tag, with xml attrs, and with no nested tags
    int writeElement(const std::string& name,
                     const std::string& value,
                     const int     indentLevel,
                     const int     nPairs, ...); // nPairs of
                 // (char *attrName, char *attrValue)

    // maps to SAX2 endElement(); use to close a nested tag section
    int writeEndElement(const std::string& name,
                        const int     indentLevel);

    // write an XML comment
    int writeComment(const std::string& comment, const int indentLevel=0);

    int write(const std::string& fileName,
              const char* outChars, int flag);

    // internal validation
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0) const;

    // native C++ constructors/destructors
    IO_XML(void);
    IO_XML(Attribute*);
    // IO_XML(const IO_XML &io_xml);  TODO
    ~IO_XML(){destruct();}

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

    // used internally by public methods writeStartElement() & writeElement()
    //   to share the common logic of writing the bulk of the tag
    //   (the difference is in the handling of the end-of-line/end-of-tag)
    int writeElementCore(const std::string& name,
                         const std::string& value,
                         const int     indentLevel,
                         const int     nPairs,
                         va_list       args); // nPairs of
                     // (char *attrName, char *attrValue)
    void destruct();

    // replace special characters to XML entities to prevent malformed XML
    int replaceXMLEntities(std::string& str);

    // friend function to allocate and initialize IO_XML object from heap
    friend IO_XML *ESMCI_IO_XMLCreate(const std::string& name,
                                      const std::string& fileName,
                                      Attribute*, int*);

    // friend function to copy an io_xml  TODO ?
    //friend IO_XML *ESMCI_IO_XML(IO_XML*, int*);

    // friend function to de-allocate IO_XML
    friend int ESMCI_IO_XMLDestroy(IO_XML**);

//
//EOP
//-------------------------------------------------------------------------

};  // end class IO_XML

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++.
    // These also establish defaults to match F90 optional args.  TODO ?

    // friend function to allocate and initialize io from heap
    IO_XML *ESMCI_IO_XMLCreate(const std::string&  name="",
                               const std::string&  fileName="",
                               Attribute* attr=0, int* rc=0);

    // friend function to copy an io_xml  TODO ?
    //IO *ESMCI_IO_XMLCreate(IO_XML *io_xml, int *rc=0);

    // friend function to de-allocate io_xml
    int ESMCI_IO_XMLDestroy(IO_XML **io_xml);

    // friend to restore state  TODO ?
    //IO *ESMCI_IO_XMLReadRestart(const std::string&  name=0,
                                   //int*         rc=0);

}   // namespace ESMCI

#endif // ESMC_IO_XML_H
