// $Id: ESMCI_SAX2ReadHandler.h,v 1.1 2010/02/11 06:58:19 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF IO XML SAX2 ReadHandler C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMCI_SAX2READHANDLER_H
#define ESMCI_SAX2READHANDLER_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI::SAX2ReadHandler - Handles XML SAX2 Parse events for the IO class
//
// !DESCRIPTION:
//  TODO
//-------------------------------------------------------------------------
//
// !USES:
#ifdef ESMF_XERCES
 #include <xercesc/sax2/DefaultHandler.hpp>
 #include <xercesc/framework/XMLFormatter.hpp>
 #include <xercesc/sax2/Attributes.hpp>
 XERCES_CPP_NAMESPACE_USE
#endif

#include "ESMC_Base.h"

namespace ESMCI{

#ifdef ESMF_XERCES
 // define class to handle sax2 parse events; supports io_xml::read()
 class SAX2ReadHandler : public DefaultHandler {
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
        const SAXParseException& exception
    );

    SAX2ReadHandler(ESMCI::Attribute *attr);
 };
#endif // ESMF_XERCES

} // namespace ESMCI

#endif // ESMCI_SAX2READHANDLER_H
