// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF IO_XML SAX2WriteHandler C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMCI_SAX2WRITEHANDLER_H
#define ESMCI_SAX2WRITEHANDLER_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI::SAX2WriteHandler - Handles XML SAX2 Write events for the
// IO_XML class.
//
// !DESCRIPTION:
//  TODO
//-------------------------------------------------------------------------
//
// !USES:
#ifdef ESMF_XERCES
 #include <xercesc/sax2/DefaultHandler.hpp>
 #include <xercesc/framework/LocalFileFormatTarget.hpp>
 //#include <xercesc/framework/XMLFormatter.hpp>
 #include <xercesc/sax2/Attributes.hpp>
 XERCES_CPP_NAMESPACE_USE
#endif

namespace ESMCI{

#ifdef ESMF_XERCES
 // define class to handle sax2 write calls; supports io_xml::write()
 class SAX2WriteHandler : public DefaultHandler, public LocalFileFormatTarget {
 private:
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fFormatter
    //      This is the formatter object that is used to output the data
    //      to the target. It is set up to format to the standard output
    //      stream.
    // -----------------------------------------------------------------------
    XMLFormatter fFormatter;
    bool         fExpandNS ;

 public:
    // -----------------------------------------------------------------------
    //  Constructor/Destructor
    // -----------------------------------------------------------------------
    SAX2WriteHandler(
         const char* const              fileName,
         const char* const              encodingName,
         const XMLFormatter::UnRepFlags unRepFlags,
         const bool                     expandNamespaces
    );
    ~SAX2WriteHandler();

#if 0
    // -----------------------------------------------------------------------
    //  Implementation of the XMLFormatTarget interface
    // -----------------------------------------------------------------------
    void writeChars(
        const XMLByte* const toWrite
    );

    virtual void writeChars(
        const XMLByte* const toWrite,
        const XMLSize_t      count,
        XMLFormatter* const  formatter
    );
#endif

    void comment(
        const XMLCh* const chars,
        const XMLSize_t    length
    );

    // -----------------------------------------------------------------------
    //  Implementation of the SAX DefaultHandler interface
    // -----------------------------------------------------------------------
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

    void startDocument();
    void endDocument();

    void ignorableWhitespace(
        const XMLCh* const chars,
        const XMLSize_t    length
    );

    void processingInstruction(
        const XMLCh* const target,
        const XMLCh* const data
    );

    // -----------------------------------------------------------------------
    //  Implementations of the SAX ErrorHandler interface
    // -----------------------------------------------------------------------
    void warning(const SAXParseException& exception);
    void error(const SAXParseException& exception);
    void fatalError(const SAXParseException& exception);

    // -----------------------------------------------------------------------
    //  Implementation of the SAX DTDHandler interface
    // -----------------------------------------------------------------------
    void notationDecl(
        const XMLCh* const name,
        const XMLCh* const publicId,
        const XMLCh* const systemId
    );

    void unparsedEntityDecl(
        const XMLCh* const name,
        const XMLCh* const publicId,
        const XMLCh* const systemId,
        const XMLCh* const notationName
    );
};
#endif // ESMF_XERCES

} // namespace ESMCI

#endif // ESMCI_SAX2WRITEHANDLER_H
