// $Id: ESMCI_IO_XML.h,v 1.2.2.1 2010/02/05 19:58:00 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
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
#include "ESMC_Start.h"

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI::IO_XML - Handles low-level XML IO for ESMF internals and user API.
//
// !DESCRIPTION:
//  TODO
//-------------------------------------------------------------------------
//
// !USES:
#include "ESMC_Base.h"           // inherited Base class

#ifdef ESMF_XERCES
 #include <xercesc/sax2/DefaultHandler.hpp>

 using namespace xercesc;

 // define class to handle sax2 parse events
 class MySAX2Handler : public DefaultHandler {
 private:
    ESMCI::Attribute* attr;
    string qname;
    string convention;
    string purpose;
    string object;

 public:
     void startElement(
         const XMLCh* const uri,
         const XMLCh* const localname,
         const XMLCh* const qname,
         const Attributes&  attrs
     );

     void characters(
         const XMLCh* const chars,
         const XMLSize_t    length
     );

     void endElement(
         const XMLCh* const uri,
         const XMLCh* const localname,
         const XMLCh* const qname	 
     ); 

     void fatalError(
         const SAXParseException&
     );

     MySAX2Handler(ESMCI::Attribute *attr);
 };
#endif

 namespace ESMCI{

// !PUBLIC TYPES:
 class IO_XML;

// !PRIVATE TYPES:

 // class definition type
 class IO_XML : ESMC_Base { // inherit from ESMC_Base class
  private:   // corresponds to F90 module 'type ESMF_IO_XML' members
    Attribute *attr;    // root node of associated object's attributes
    char       fileName[ESMF_MAXSTR];

// !PUBLIC MEMBER FUNCTIONS:

  public:
    // accessor methods

    // Read/Write to support the F90 optional arguments interface
    int read(int fileNameLen, const char* fileName);

    // internal validation
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0) const;

    // native C++ constructors/destructors
    IO_XML(void);
    IO_XML(Attribute*);
    // IO_XML(const IO_XML &io_xml);  TODO
    ~IO_XML(){destruct();}
   private:
    void destruct();

    // friend function to allocate and initialize IO_XML object from heap
    friend IO_XML *ESMCI_IO_XMLCreate(int, const char*, Attribute*, int*);

    // friend function to copy an io_xml  TODO ?
    //friend IO_XML *ESMCI_IO_XML(IO_XML*, int*);

    // friend function to de-allocate IO_XML
    friend int ESMCI_IO_XMLDestroy(IO_XML**);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

//
//EOP
//-------------------------------------------------------------------------

};  // end class IO_XML

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++.
    // These also establish defaults to match F90 optional args.  TODO ?

    // friend function to allocate and initialize io from heap
    IO_XML *ESMCI_IO_XMLCreate(int nameLen, const char* name=0,
                               Attribute* attr=0, int* rc=0);

    // friend function to copy an io_xml  TODO ?
    //IO *ESMCI_IO_XMLCreate(IO_XML *io_xml, int *rc=0);

    // friend function to de-allocate io_xml
    int ESMCI_IO_XMLDestroy(IO_XML **io_xml);

    // friend to restore state  TODO ?
    //IO *ESMCI_IO_XMLReadRestart(int nameLen,
                                   //const char*  name=0,
                                   //ESMC_IOSpec* iospec=0,
                                   //int*         rc=0);

}   // namespace ESMCI

#endif // ESMC_IO_XML_H
