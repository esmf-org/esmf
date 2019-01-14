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
// ESMC IO_XML SAX2WriteHandler method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++
// {\tt ESMCI\_SAX2WriteHandler} methods declared
// in the companion file {\tt ESMCI\_SAX2WriteHandler.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_SAX2WriteHandler.C"

// associated class definition file
#include "ESMCI_SAX2WriteHandler.h"

#include<iostream>
using namespace std;

#ifdef ESMF_XERCES
 #include <xercesc/util/XMLUniDefs.hpp>
#endif

#include "ESMCI_LogErr.h"

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI{

#ifdef ESMF_XERCES
// ---------------------------------------------------------------------------
//  Local const data
//
//  Note: This is the 'safe' way to do these strings. If your compiler supports
//        L"" style strings, and portability is not a concern, you can use
//        those types constants directly.
// ---------------------------------------------------------------------------
static const XMLCh  gEndElement[] = { chOpenAngle, chForwardSlash, chNull };
static const XMLCh  gEndPI[] = { chQuestion, chCloseAngle, chNull };
static const XMLCh  gStartPI[] = { chOpenAngle, chQuestion, chNull };
static const XMLCh  gXMLDecl1[] =
{
        chOpenAngle, chQuestion, chLatin_x, chLatin_m, chLatin_l
    ,   chSpace, chLatin_v, chLatin_e, chLatin_r, chLatin_s, chLatin_i
    ,   chLatin_o, chLatin_n, chEqual, chDoubleQuote, chDigit_1, chPeriod
    ,   chDigit_0, chDoubleQuote, chSpace, chLatin_e, chLatin_n, chLatin_c
    ,   chLatin_o, chLatin_d, chLatin_i, chLatin_n, chLatin_g, chEqual
    ,   chDoubleQuote, chNull
};

static const XMLCh  gXMLDecl2[] =
{
        chDoubleQuote, chQuestion, chCloseAngle
    ,   chLF, chNull
};

// ---------------------------------------------------------------------------
//  This is a simple class that lets us do easy (though not terribly efficient)
//  trancoding of XMLCh data to local code page for display.
// ---------------------------------------------------------------------------
class StrX
{
public :
    // -----------------------------------------------------------------------
    //  Constructor and Destructor
    // -----------------------------------------------------------------------
    StrX(const XMLCh* const toTranscode)
    {
        // Call the private transcoding method
        fLocalForm = XMLString::transcode(toTranscode);
    }

    ~StrX()
    {
        XMLString::release(&fLocalForm);
    }

    // -----------------------------------------------------------------------
    //  Getter methods
    // -----------------------------------------------------------------------
    const char* localForm() const
    {
        return fLocalForm;
    }

private :
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fLocalForm
    //      This is the local code page form of the string.
    // -----------------------------------------------------------------------
    char* fLocalForm;
};

inline XERCES_STD_QUALIFIER ostream& operator<<(XERCES_STD_QUALIFIER ostream& target, const StrX& toDump)
{
XERCES_STD_QUALIFIER cerr << "ostream operator<<()" << XERCES_STD_QUALIFIER endl;
    target << toDump.localForm();
    return target;
}

// ---------------------------------------------------------------------------
//  SAX2WriteHandler: Constructor/Destructor
// ---------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "SAX2WriteHandler::SAX2WriteHandler()"

SAX2WriteHandler::SAX2WriteHandler(const char* const  fileName,
                             const char* const        encodingName,
                             const XMLFormatter::UnRepFlags unRepFlags,
                             const bool                     expandNamespaces) :

    LocalFileFormatTarget(fileName),
    fFormatter
    (
        encodingName
        , 0
        , this
        , XMLFormatter::NoEscapes
        , unRepFlags
    ),
    fExpandNS( expandNamespaces )
{
    //
    //  Go ahead and output an XML Decl with our known encoding. This
    //  is not the best answer, but its the best we can do until we
    //  have SAX2 support.
    //
//XERCES_STD_QUALIFIER cerr << "SAX2WriteHandler()" << XERCES_STD_QUALIFIER endl;
    fFormatter << gXMLDecl1 << fFormatter.getEncodingName() << gXMLDecl2;
}

SAX2WriteHandler::~SAX2WriteHandler()
{
//XERCES_STD_QUALIFIER cerr << "~SAX2WriteHandler()" << XERCES_STD_QUALIFIER endl;
}

#if 0
// ---------------------------------------------------------------------------
//  SAX2WriteHandler: Overrides of the output formatter target interface
// ---------------------------------------------------------------------------
void SAX2WriteHandler::writeChars(const XMLByte* const /* toWrite */)
{
//XERCES_STD_QUALIFIER cerr << "writeCharsStub()" << XERCES_STD_QUALIFIER endl;
}
void SAX2WriteHandler::writeChars(const XMLByte* const toWrite,
                                  const XMLSize_t      count,
                                  XMLFormatter* const  /* formatter */)
{
    // For this one, just dump them to the standard output
    // Surprisingly, Solaris was the only platform on which
    // required the char* cast to print out the string correctly.
    // Without the cast, it was printing the pointer value in hex.
    // Quite annoying, considering every other platform printed
    // the string with the explicit cast to char* below.
//XERCES_STD_QUALIFIER cerr << "writeChars()" << XERCES_STD_QUALIFIER endl;
    XERCES_STD_QUALIFIER cout.write((char *) toWrite, (int) count);
    XERCES_STD_QUALIFIER cout.flush();
}
#endif

void SAX2WriteHandler::comment(const XMLCh* const chars,
                               const XMLSize_t    length)
{
//XERCES_STD_QUALIFIER cerr << "comment()" << XERCES_STD_QUALIFIER endl;
    fFormatter << chOpenAngle << chBang << chDash << chDash << chSpace; // <!-- 
    fFormatter.formatBuf(chars, length, XMLFormatter::CharEscapes);
    fFormatter << chSpace<< chDash << chDash << chCloseAngle ; // -->
}

// ---------------------------------------------------------------------------
//  SAX2WriteHandler: Overrides of the SAX DocumentHandler interface
// ---------------------------------------------------------------------------
void SAX2WriteHandler::startElement(const XMLCh* const uri,
                                    const XMLCh* const localname,
                                    const XMLCh* const qname,
                                    const Attributes&  attributes)
{
//XERCES_STD_QUALIFIER cerr << "startElement()" << XERCES_STD_QUALIFIER endl;
    // The name has to be representable without any escapes
    fFormatter  << XMLFormatter::NoEscapes << chOpenAngle ;
    if ( fExpandNS )
    {
        if (XMLString::compareIString(uri,XMLUni::fgZeroLenString) != 0)
            fFormatter << uri << chColon;
        fFormatter<< localname ;
    }
    else
        fFormatter<< qname ;

    XMLSize_t len = attributes.getLength();
    for (XMLSize_t index = 0; index < len; index++)
    {
        //
        //  Again the name has to be completely representable. But the
        //  attribute can have refs and requires the attribute style
        //  escaping.
        //
        fFormatter  << XMLFormatter::NoEscapes << chSpace ;
        if ( fExpandNS )
        {
            if (XMLString::compareIString(attributes.getURI(index),XMLUni::fgZeroLenString) != 0)
                fFormatter << attributes.getURI(index) << chColon;
            fFormatter << attributes.getLocalName(index) ;
        }
        else
            fFormatter << attributes.getQName(index) ;

        fFormatter << chEqual << chDoubleQuote
                   << XMLFormatter::AttrEscapes
                   << attributes.getValue(index)
                   << XMLFormatter::NoEscapes
                   << chDoubleQuote;
    }
    fFormatter << chCloseAngle;
}

void SAX2WriteHandler::characters(const XMLCh* const chars,
                                  const XMLSize_t    length)
{
//XERCES_STD_QUALIFIER cerr << "characters()" << XERCES_STD_QUALIFIER endl;
    fFormatter.formatBuf(chars, length, XMLFormatter::CharEscapes);
}

void SAX2WriteHandler::endElement(const XMLCh* const uri,
                                  const XMLCh* const localname,
                                  const XMLCh* const qname)
{
//XERCES_STD_QUALIFIER cerr << "endElement()" << XERCES_STD_QUALIFIER endl;
    // No escapes are legal here
    fFormatter << XMLFormatter::NoEscapes << gEndElement ;
    if ( fExpandNS )
    {
        if (XMLString::compareIString(uri,XMLUni::fgZeroLenString) != 0)
            fFormatter << uri << chColon;
        fFormatter << localname << chCloseAngle;
    }
    else
        fFormatter << qname << chCloseAngle;
}

void SAX2WriteHandler::startDocument()
{
//XERCES_STD_QUALIFIER cerr << "startDocument()" << XERCES_STD_QUALIFIER endl;
}

void SAX2WriteHandler::endDocument()
{
//XERCES_STD_QUALIFIER cerr << "endDocument()" << XERCES_STD_QUALIFIER endl;
}

void SAX2WriteHandler::ignorableWhitespace( const XMLCh* const chars
                                           ,const XMLSize_t length)
{
//XERCES_STD_QUALIFIER cerr << "ignorableWhitespace()" << XERCES_STD_QUALIFIER endl;
    fFormatter.formatBuf(chars, length, XMLFormatter::NoEscapes);
}


void SAX2WriteHandler::processingInstruction(const XMLCh* const target
                                           , const XMLCh* const data)
{
//XERCES_STD_QUALIFIER cerr << "processingInstruction()" << XERCES_STD_QUALIFIER endl;
    fFormatter << XMLFormatter::NoEscapes << gStartPI  << target;
    if (data)
        fFormatter << chSpace << data;
    fFormatter << XMLFormatter::NoEscapes << gEndPI;
}

// ---------------------------------------------------------------------------
//  SAX2WriteHandler: Overrides of the SAX ErrorHandler interface
// ---------------------------------------------------------------------------
void SAX2WriteHandler::error(const SAXParseException& e)
{
    XERCES_STD_QUALIFIER cerr << "\nError at file " << StrX(e.getSystemId())
       << ", line " << e.getLineNumber()
       << ", char " << e.getColumnNumber()
       << "\n  Message: " << StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
}

void SAX2WriteHandler::fatalError(const SAXParseException& e)
{
    XERCES_STD_QUALIFIER cerr << "\nFatal Error at file "
                              << StrX(e.getSystemId())
       << ", line " << e.getLineNumber()
       << ", char " << e.getColumnNumber()
       << "\n  Message: " << StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
}

void SAX2WriteHandler::warning(const SAXParseException& e)
{
    XERCES_STD_QUALIFIER cerr << "\nWarning at file " << StrX(e.getSystemId())
       << ", line " << e.getLineNumber()
       << ", char " << e.getColumnNumber()
       << "\n  Message: " << StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
}

// ---------------------------------------------------------------------------
//  SAX2WriteHandler: Overrides of the SAX DTDHandler interface
// ---------------------------------------------------------------------------
void SAX2WriteHandler::unparsedEntityDecl(const XMLCh* const /* name */
                                        , const XMLCh* const /* publicId */
                                        , const XMLCh* const /* systemId */
                                        , const XMLCh* const /* notationName */)
{
//XERCES_STD_QUALIFIER cerr << "unparsedEntityDecl()" << XERCES_STD_QUALIFIER endl;
    // Not used at this time
}


void SAX2WriteHandler::notationDecl(const XMLCh* const /* name */
                                  , const XMLCh* const /* publicId */
                                  , const XMLCh* const /* systemId */)
{
//XERCES_STD_QUALIFIER cerr << "notationDecl()" << XERCES_STD_QUALIFIER endl;
    // Not used at this time
}
#endif // ESMF_XERCES

} // end namespace ESMCI
