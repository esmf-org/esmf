// $Id: ESMCI_IO.h,v 1.5 2009/07/07 05:49:12 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF IO C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMCI_IO_H
#define ESMCI_IO_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMC_Start.h"

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI::IO - Handles low-level IO for ESMF internals and user API.
//
// !DESCRIPTION:
//  TODO
//-------------------------------------------------------------------------
//
// !USES:
#include "ESMC_Base.h"           // inherited Base class TODO ?

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
 class IO;

// !PRIVATE TYPES:
 // class configuration type:  not needed for IO TODO ?

 // class definition type
 class IO { // TODO: inherit public ESMC_Base class when
            // fully aligned with F90 equiv 
  private:   // corresponds to F90 module 'type ESMF_IO' members
    char         name[ESMF_MAXSTR];  // name of io object
    Attribute   *attr;    // associated object's attributes
// TODO: need both base and attr?
    char         fileName[ESMF_MAXSTR];
    int          id;          // unique identifier. used for equality
                              //    checks and to generate unique default
                              //    names.
                              //    TODO: inherit from ESMC_Base class
    static int   count;       // number of io objects created. Thread-safe
                              //   because int is atomic.
                              //    TODO: inherit from ESMC_Base class

// !PUBLIC MEMBER FUNCTIONS:

  public:
    // IO doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required TODO ?

    // accessor methods

    // Read/Write to support the F90 optional arguments interface
    int read(int fileNameLen, const char* fileName);

    // internal validation
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0) const;

    // native C++ constructors/destructors
    IO(void);
    IO(Attribute*);
    // IO(const IO &io);  TODO
    ~IO(void);

    // friend function to allocate and initialize IO object from heap
    friend IO *ESMCI_IOCreate(int, const char*, Attribute*, int*);

    // friend function to copy an io  TODO ?
    //friend IO *ESMCI_IO(IO*, int*);

    // friend function to de-allocate IO
    friend int ESMCI_IODestroy(IO**);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

//
//EOP
//-------------------------------------------------------------------------

};  // end class IO

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++.
    // These also establish defaults to match F90 optional args.  TODO ?

    // friend function to allocate and initialize io from heap
    IO *ESMCI_IOCreate(int nameLen, const char* name=0,
                       Attribute* attr=0, int* rc=0);

    // friend function to copy an io  TODO ?
    //IO *ESMCI_IOCreate(IO *io, int *rc=0);

    // friend function to de-allocate io
    int ESMCI_IODestroy(IO **io);

    // friend to restore state  TODO ?
    //IO *ESMCI_IOReadRestart(int nameLen,
                                   //const char*  name=0,
                                   //ESMC_IOSpec* iospec=0,
                                   //int*         rc=0);

}   // namespace ESMCI

#endif // ESMC_IO_H
