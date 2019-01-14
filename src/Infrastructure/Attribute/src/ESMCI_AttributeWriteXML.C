// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMCI_AttributeWriteXML.C"

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Attribute} methods declared
// in the companion file ESMCI_Attribute.h for write XML formatted files.
//
//-----------------------------------------------------------------------------
// associated class definition file and others
#include "ESMCI_Attribute.h"

#include "ESMCI_Macros.h"
#include "ESMCI_IO_XML.h"
#include "ESMCI_LogErr.h"

#include <cstdlib>
#include <cstring>
#include <sstream>

using std::string;
using std::ostringstream;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXML"
//BOPI
// !IROUTINE:  AttributeWriteXML - Write contents of an {\tt Attribute} package
//
// !INTERFACE:
int Attribute::AttributeWriteXML(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        const string &convention,        //  in - convention
        const string &purpose,           //  in - purpose
        const string &object,            //  in - object
        const string &varobj,            //  in - variable object
        const string &basename) const {  //  in - basename
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

// TODO:  Rewrite this method (non-recursive) to be just a switcher to other
//        methods (recursive) based on file standard type (separate methods
//        for ESMF, CIM, WaterML, etc).  Each file type requires its own logic
//        for how to traverse the attribute tree (possibly multiple times) and
//        output the file (a form of serialization).

    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // Initialize the GUID generator
    // First call to AttributeWriteCIM() will initiate a new sequence of GUIDs
    // used to fill <documentID>s and <id>s in the XML output.
    localrc = ESMC_InitializeGUID();
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // Instantiate IO object to do the actual writing
    string fileName = basename + ".xml";
    IO_XML *io_xml = ESMCI_IO_XMLCreate("", fileName,
    (ESMCI::Attribute*)this, &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // first break off for the CIM stuff which is in a different branch of functions
    if (object.compare("comp")==0 &&
        (convention.compare(CIM_1_5_CONV)==0 ||
         convention.compare(CIM_1_5_1_CONV)==0 ||
         convention.compare(CIM_1_7_1_CONV)==0) &&
        purpose.compare(MODEL_COMP_PURP)==0)
        localrc = AttributeWriteCIM(io_xml, convention);
    else if (object.compare("comp")==0 &&
             (convention.compare(CIM_1_5_1_CONV)==0 ||
              convention.compare(CIM_1_7_1_CONV)==0) &&
             purpose.compare(GRIDS_PURP)==0) {
        string gridGUID = "";
        ESMC_GenerateGUID(gridGUID);
        int indent = 0;
        bool gridSolo = true;
        localrc = AttributeWriteCIMgrids(io_xml, convention, gridGUID, indent, gridSolo);
    } else
        localrc = AttributeWriteXMLheader(io_xml,convention, purpose, object, varobj);
    if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, "Failed writing the XML file",
                                      ESMC_CONTEXT, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        return ESMF_FAILURE;
    }

    // destroy the io_xml object, which closes the file
    localrc = ESMCI_IO_XMLDestroy(&io_xml);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    return localrc;

} // end AttributeWriteXML

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLheader"
//BOPI
// !IROUTINE:  AttributeWriteXMLheader - Write the XML header
//
// !INTERFACE:
int Attribute::AttributeWriteXMLheader(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                  //  in - io pointer to write
        const string &convention,        //  in - convention
        const string &purpose,           //  in - purpose
        const string &object,            //  in - object
        const string &varobj) const {    //  in - varobject
//
// !DESCRIPTION:
//    Print the XML header.  Expected to be
//    called internally.
//
//EOPI

    char msgbuf[4*ESMF_MAXSTR];
    string modelcompname, fullname, version;
    Attribute *attr, *attpack;
    int localrc, rows, columns, fldcount;
    bool fielddone, griddone, compdone;
    ESMC_Logical presentflag;

    fielddone = false; griddone = false; compdone = false;
    rows = 0; fldcount = 0; columns = 0;
    attr = NULL; attpack = NULL;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;



    // Write ESMF header
    string comment = "Generated with ESMF Version ";
    comment += ESMF_VERSION_STRING;
    comment += ", http://www.earthsystemmodeling.org";
    localrc = io_xml->writeComment(comment);
    if (localrc == ESMF_RC_LIB_NOT_PRESENT) {
        sprintf(msgbuf, "Xerces C++ library (>= v3.1.0) not present");
        ESMC_LogDefault.MsgFoundError(localrc, msgbuf, ESMC_CONTEXT, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        return ESMF_RC_LIB_NOT_PRESENT;
    }

    //
    // determine modelcompname, fullname, version for header
    //
    if (object.compare("comp")==0) {
        // get value of attribute 0 or set to N/A if not present
        string attPackInstanceName;
        attpack = AttPackGet(convention, purpose, object, attPackInstanceName,
                             ESMC_ATTNEST_ON);
        localrc = AttPackIsPresent("ComponentShortName", attpack, ESMC_ATTNEST_ON,
                                   &presentflag);
        if (localrc != ESMF_SUCCESS) {
            sprintf(msgbuf, "failed finding an attribute");
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
            localrc = ESMCI_IO_XMLDestroy(&io_xml);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;
        }
        if (presentflag == ESMF_TRUE) {
            string attPackInstanceName;
            attr = (AttPackGet(convention, purpose, object,attPackInstanceName,
                               ESMC_ATTNEST_ON)->AttPackGetAttribute(
                    "ComponentShortName", ESMC_ATTNEST_ON));
            if (attr != NULL) {
                if (attr->vcpp.empty()) modelcompname = "N/A";
                else modelcompname = attr->vcpp.at(0);
            } else {
                sprintf(msgbuf, "failed getting attribute value");
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
                localrc = ESMCI_IO_XMLDestroy(&io_xml);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;
            }
        }
        else {
            modelcompname="N/A";
        }

        // get value of attribute 1 or set to N/A if not present
        attpack = AttPackGet(convention, purpose, object, attPackInstanceName,
                             ESMC_ATTNEST_ON);
        localrc = AttPackIsPresent("ComponentLongName", attpack, ESMC_ATTNEST_ON,
                                   &presentflag);
        if (localrc != ESMF_SUCCESS) {
            sprintf(msgbuf, "failed finding an attribute");
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
            localrc = ESMCI_IO_XMLDestroy(&io_xml);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;
        }
        if (presentflag == ESMF_TRUE) {
            string attPackInstanceName;
            attr = (AttPackGet(convention,purpose,object,attPackInstanceName,
                               ESMC_ATTNEST_ON)->AttPackGetAttribute(
                    "ComponentLongName", ESMC_ATTNEST_ON));
            if (attr != NULL) {
                if (attr->vcpp.empty()) fullname = "N/A";
                else fullname = attr->vcpp.at(0);
            } else {
                sprintf(msgbuf, "failed getting attribute value");
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
                localrc = ESMCI_IO_XMLDestroy(&io_xml);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;
            }
        }
        else {
            fullname="N/A";
        }

        // get value of attribute 2 or set to N/A if not present
        attpack = AttPackGet(convention, purpose, object, attPackInstanceName,
                             ESMC_ATTNEST_ON);
        localrc = AttPackIsPresent("Version", attpack, ESMC_ATTNEST_ON,
                                   &presentflag);
        if (localrc != ESMF_SUCCESS) {
            sprintf(msgbuf, "failed finding an attribute");
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
            localrc = ESMCI_IO_XMLDestroy(&io_xml);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;
        }
        if (presentflag == ESMF_TRUE) {
            string attPackInstanceName;
            attr = (AttPackGet(convention,purpose,object,attPackInstanceName,
                               ESMC_ATTNEST_ON)->AttPackGetAttribute(
                    "Version", ESMC_ATTNEST_ON));
            if (attr != NULL) {
                if (attr->vcpp.empty()) version = "N/A";
                else version = attr->vcpp.at(0);
            } else {
                sprintf(msgbuf, "failed getting attribute value");
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
                localrc = ESMCI_IO_XMLDestroy(&io_xml);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;
            }
        }
        else {
            version="N/A";
        }

    } else if (object.compare("state")==0 ||
               object.compare("fieldbundle")==0 ||
               object.compare("field")==0 ||
               object.compare("arraybundle")==0 ||
               object.compare("array")==0 ||
               object.compare("grid")==0 ||
               object.compare("locstream")==0 ||
               object.compare("distgrid")==0) {
        modelcompname="N/A";
        fullname="N/A";
        version="N/A";
    }
    else {
        sprintf(msgbuf, "AttributeWrite called from an invalid ESMF object");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
        return ESMF_FAILURE;
    }

    // TODO: replace this prototype for WaterML TimeSeries
    if (convention.compare("WaterML")==0 &&
        purpose.compare("TimeSeries")==0) {

        // Write the WaterML XML file header
        localrc = io_xml->writeStartElement("timeSeriesResponse", "", 0, 6,
                                            "xmlns:gml", "http://www.opengis.net/gml",
                                            "\n  xmlns:xlink", "http://www.w3.org/1999/xlink",
                                            " xmlns:xsd", "http://www.w3.org/2001/XMLSchema",
                                            "\n  xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance",
                                            " xmlns:wtr", "http://www.cuahsi.org/waterML/",
                                            "\n  xmlns", "http://www.cuahsi.org/waterML/1.0/");
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeStartElement("queryInfo", "", 1, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeElement("creationTime",
                                       "2009-01-08T15:52:17.8495Z", 2, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeStartElement("criteria", "", 2, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeElement("locationParam",
                                       "LittleBearRiver:USU-LBR-Paradise", 3, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeElement("variableParam", "LBR:USU39", 3, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeStartElement("timeParam", "", 3, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeElement("beginDateTime", "2008-04-14T13:00:00", 4, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeElement("endDateTime", "2008-04-15T12:00:00", 4, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeEndElement("timeParam", 3);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeEndElement("criteria", 2);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeElement("note", "OD Web Service", 2, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeEndElement("queryInfo", 1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeStartElement("timeSeries", "", 1, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    } else {

        // Write the ESMF XML file header
        localrc = io_xml->writeStartElement("model_component", "", 1, 9,
                                            "name", modelcompname.c_str(),
                                            "full_name", fullname.c_str(),
                                            "version", version.c_str(),
                                            "\n      xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance",
                                            "\n      xmlns:xlink", "http://www.w3.org/1999/xlink",
                                            "\n      xmlns:gco", "http://www.isotc211.org/2005/gco",
                                            "\n      xmlns:gmd", "http://www.isotc211.org/2005/gmd",
                                            "\n      xmlns", "http://www.earthsystemmodeling.org",
                                            "\n      xsi:schemaLocation", "http://www.earthsystemmodeling.org file:/esmf_model_component.xsd");
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
#if 0
        // TODO:  write as separate elements instead of atts ?
    localrc = io_xml->writeStartElement("model_component", "", 1, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    localrc = io_xml->writeElement("name", modelcompname, 2, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    localrc = io_xml->writeElement("full_name", fullname, 2, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    localrc = io_xml->writeElement("version", version, 2, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
#endif

    }

    // determine the number of fields to write
    localrc = AttributeCountTree(convention, purpose, varobj, rows, columns);
    if (localrc != ESMF_SUCCESS) {
        sprintf(msgbuf, "Attribute failed counting fields");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        return ESMF_FAILURE;
    }

    // if not called from component, won't need to print that stuff
    if (object.compare("comp")!=0) {
        compdone = true;
    }

    // recurse the Attribute hierarchy
    // TODO: split out WaterML, ESMF
    //   (AttributeWriteWaterML(), AttributeWriteESMF(),
    //    deprecate AttributeXML()? )
    localrc = AttributeWriteXMLtraverse(io_xml,convention,purpose,columns,
                                        fielddone,griddone,compdone);
    if (localrc != ESMF_SUCCESS) {
        sprintf(msgbuf, "Attribute failed recursing in WriteXML");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        return ESMF_FAILURE;
    }

    // TODO: replace this prototype for WaterML TimeSeries
    if (convention.compare("WaterML")==0 &&
        purpose.compare("TimeSeries")==0) {

        // write the WaterML footer
        localrc = io_xml->writeEndElement("timeSeries", 1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeEndElement("timeSeriesResponse", 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    } else {

        // write the ESMF XML footer
        localrc = io_xml->writeEndElement("model_component", 1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    return localrc;

} // end AttributeWriteXMLheader

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLtraverse"
//BOPI
// !IROUTINE:  AttributeWriteXMLtraverse - {\tt Attribute} hierarchy traversal write
//
// !INTERFACE:
int Attribute::AttributeWriteXMLtraverse(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                  //  in - io pointer to write
        const string &convention,        //  in - convention
        const string &purpose,           //  in - purpose
        const int &columns,              //  in - columns
        bool &fielddone,                 //  in - bool for field
        bool &griddone,                  //  in - bool for grid
        bool &compdone) const{           //  in - bool for comp
//
// !DESCRIPTION:
//    Write the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    char msgbuf[4*ESMF_MAXSTR];
    int localrc;
    int index;
    unsigned int i;
    Attribute *attpack;

    index = 0;
    attpack = NULL;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // do component write
    if (!compdone) {
// TODO: implement attPackInstanceName search, if possible to have multiple
//       instances of attpack parents?
        string attPackInstanceName;
//printf("XMLtraverse(): looking for 1st attpack\n");
        attpack = AttPackGet(convention, purpose, "comp", attPackInstanceName,
                             ESMC_ATTNEST_ON);
        if (attpack != NULL) {
//    while (attpack != NULL) {
//printf("XMLtraverse(): found attPackInstanceName %s match\n",
//       attPackInstanceName);
            localrc = attpack->AttributeWriteXMLbuffer(io_xml);
            if (localrc != ESMF_SUCCESS) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "AttributeWriteXMLtraverse failed AttributeWriteXMLbuffer", ESMC_CONTEXT, &localrc);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;
            }
            // get next occurence of this attpack, if any
//      ordinal++;
//      attpack = AttPackGet(convention, purpose, "comp", &ordinal,
//                           ESMC_ATTNEST_ON);
        }
//printf("XMLtraverse(): here3\n");
        compdone = true;
    }

    // do field write
    if (!fielddone) {
        // TODO: replace this prototype for WaterML TimeSeries
        if (!(convention.compare("WaterML")==0 &&
              purpose.compare("TimeSeries")==0)) {
            // write the field header
            localrc = io_xml->writeStartElement("variable_set", "", 2, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
        // call the field write buffer method
        localrc = AttributeWriteXMLbufferfield(io_xml, convention, purpose, index, columns);
        if (localrc != ESMF_SUCCESS) {
            sprintf(msgbuf, "AttributeWriteXMLtraverse failed AttributeWriteXMLbufferfield");
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;
        }
        if (!(convention.compare("WaterML")==0 &&
              purpose.compare("TimeSeries")==0)) {
            // write the field footer
            localrc = io_xml->writeEndElement("variable_set", 2);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
        // done with field
        fielddone = true;
    }

    // do grid write
    if (!griddone) {
        string attPackInstanceName;
        attpack = AttPackGet(convention, purpose, "grid", attPackInstanceName,
                             ESMC_ATTNEST_ON);
        if (attpack) {
            // write the grid header
            localrc = io_xml->writeStartElement("GridSpec", "", 0, 1, "name", attpack->attrBase->ESMC_BaseGetName());
            localrc = io_xml->writeStartElement("Mosaic", "", 1, 1, "name", attpack->attrBase->ESMC_BaseGetName());
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            localrc = attpack->AttributeWriteXMLbuffergrid(io_xml);
            if (localrc != ESMF_SUCCESS) {
                sprintf(msgbuf, "AttributeWriteXMLtraverse failed AttributeWriteXMLbuffergrid");
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;
            }
            // write the grid footer
            localrc = io_xml->writeEndElement("Mosaic", 1);
            localrc = io_xml->writeEndElement("GridSpec", 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            griddone = true;
            return ESMF_SUCCESS;
        }
    }

    // recurse across all linked ESMF objects (e.g. child components, states,
    // fieldBundles, fields, grids, arrays)
    for(i=0; i<linkList.size(); i++)
        localrc = linkList.at(i)->AttributeWriteXMLtraverse(io_xml,convention,purpose,columns,
                                                            fielddone,griddone,compdone);

    return ESMF_SUCCESS;

} // end AttributeWriteXMLtraverse
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLbuffergrid"
//BOPI
// !IROUTINE:  AttributeWriteXMLbuffergrid - Write contents of an {\tt Attribute} package
//
// !INTERFACE:
int Attribute::AttributeWriteXMLbuffergrid(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml) const {   //  in - io pointer to write
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    // TODO:  consolidate with AttributeWriteXMLbuffer() below (no difference?)

    char msgbuf[4*ESMF_MAXSTR];
    int localrc;
    unsigned int i;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    for (i=0;  i<attrList.size(); ++i) {
        // if this is internal info, retrieve the correct Attribute
        if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER) {
            string value = attrList.at(i)->vcpp.at(0);
            if (strncmp(value.c_str(), "ESMF:", 5) == 0) {
                // this is internal information, call internal routine and continue
                int nest_level = 2;
                AttributeWriteInternalInfoGrid(io_xml, nest_level, attrList.at(i));
                continue;
            }
        }

        if (attrList.at(i)->items == 1) {
            string name = attrList.at(i)->attrName;
            ostringstream outstring;

            switch (attrList.at(i)->tk)
            {
                case ESMC_TYPEKIND_I4:
                    outstring << attrList.at(i)->vip.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 2, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_I8:
                    outstring << attrList.at(i)->vlp.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 2, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_R4:
                    outstring << attrList.at(i)->vfp.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 2, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_R8:
                    outstring << attrList.at(i)->vdp.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 2, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_LOGICAL:
                    if (attrList.at(i)->vbp.at(0) == ESMF_TRUE) {
                        localrc = io_xml->writeElement(name, "true", 2, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    } else if (attrList.at(i)->vbp.at(0) == ESMF_FALSE) {
                        localrc = io_xml->writeElement(name, "false", 2, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    }
                    break;

                case ESMC_TYPEKIND_CHARACTER:
                    if (strncmp((attrList.at(i)->vcpp.at(0)).c_str(), "ESMF:", 5) == 0) break;
                    localrc = io_xml->writeElement(name, attrList.at(i)->vcpp.at(0), 2, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                default:
                    localrc = io_xml->writeElement(name, "N/A", 2, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;
            }
        } else if (attrList.at(i)->items >1) {
            //if (attrList.at(i)->items > 1 && value.substr(5,value.length()) != "farrayPtr") {
            sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
            ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        } else if (attrList.at(i)->items == 0) {
            // do nothing
        } else {
            sprintf(msgbuf,"Items < 1, problem.");
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;
        }

    }

    // recurse through entire attribute tree on this ESMF object
    for(i=0; i<packList.size(); i++)
        localrc = packList.at(i)->AttributeWriteXMLbuffergrid(io_xml);

    return ESMF_SUCCESS;

} // end AttributeWriteXMLbuffergrid
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLbuffer"
//BOPI
// !IROUTINE:  AttributeWriteXMLbuffer - Write contents of an {\tt Attribute} package
//
// !INTERFACE:
int Attribute::AttributeWriteXMLbuffer(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml) const {   //  in - io pointer to write
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    // TODO: consolidate with AttributeWriteXMLbuffergrid() above (no difference?)

    char msgbuf[4*ESMF_MAXSTR];
    int localrc;
    unsigned int i;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    for (i=0; i<attrList.size(); ++i) {
        if (attrList.at(i)->items == 1) {
            string name = attrList.at(i)->attrName;
            //localrc = io_xml->writeStartElement(attrPurpose, "", 2, 0);
            //localrc = io_xml->writeStartElement(name+"_set", "", 2, 0);
            //ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            ostringstream outstring;
            switch (attrList.at(i)->tk)
            {
                case ESMC_TYPEKIND_I4:
                    outstring << attrList.at(i)->vip.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 3, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_I8:
                    outstring << attrList.at(i)->vlp.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 3, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_R4:
                    outstring << attrList.at(i)->vfp.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 3, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_R8:
                    outstring << attrList.at(i)->vdp.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 3, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_LOGICAL:
                    if (attrList.at(i)->vbp.at(0) == ESMF_TRUE) {
                        localrc = io_xml->writeElement(name, "true", 3, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    } else if (attrList.at(i)->vbp.at(0) == ESMF_FALSE) {
                        localrc = io_xml->writeElement(name, "false", 3, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    }
                    break;

                case ESMC_TYPEKIND_CHARACTER:
                    localrc = io_xml->writeElement(name, attrList.at(i)->vcpp.at(0), 3, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                default:
                    localrc = io_xml->writeElement(name, "N/A", 1, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;
            }
            //localrc = io_xml->writeEndElement(attrPurpose, 2);
            //localrc = io_xml->writeEndElement(name+"_set", 2);
            //ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else if (attrList.at(i)->items >1) {
            sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
            ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        } else if (attrList.at(i)->items == 0) {
            //do nothing
        } else {
            sprintf(msgbuf,"Items < 1, problem.");
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;
        }
    }

    // recurse through entire attribute tree on this ESMF object
    for(i=0; i<packList.size(); i++)
        localrc = packList.at(i)->AttributeWriteXMLbuffer(io_xml);

    return ESMF_SUCCESS;

} // end AttributeWriteXMLbuffer
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLbufferfield"
//BOPI
// !IROUTINE:  AttributeWriteXMLbufferfield - Write contents of an {\tt Attribute} package for field
//
// !INTERFACE:
int Attribute::AttributeWriteXMLbufferfield(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                //  in - io pointer to write
        const string &convention,      //  in - convention
        const string &purpose,         //  in - purpose
        int &index,                    //  in - counter
        const int &columns) const{     //  in - columns
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    char msgbuf[4*ESMF_MAXSTR];
    int localrc;
    unsigned int i;
    Attribute *attpack;

    attpack = NULL;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    string attPackInstanceName;
    attpack = AttPackGet(convention, purpose, "field", attPackInstanceName,
                         ESMC_ATTNEST_ON);
    if (attpack) {
        // TODO: replace this prototype for WaterML TimeSeries
        if (convention.compare("WaterML")==0 &&
            purpose.compare("TimeSeries")==0) {
            localrc = attpack->AttributeWriteWaterMLbuffieldT(io_xml, index, columns);
        } else {
            localrc = attpack->AttributeWriteXMLbufferfieldT(io_xml, index, columns);
        }
        if (localrc != ESMF_SUCCESS) {
            sprintf(msgbuf, "AttributeWriteXMLbufferfield failed AttributeWriteXMLbufferfieldT");
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;
        }
    }

    for(i=0; i<linkList.size(); i++) {
        index = 0;
        localrc = linkList.at(i)->AttributeWriteXMLbufferfield(io_xml,
                                                               convention,purpose,index,columns);
    }

    return ESMF_SUCCESS;

} // end AttributeWriteXMLbufferfield
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLbufferfieldT"
//BOPI
// !IROUTINE:  AttributeWriteXMLbufferfieldT - Write contents of an {\tt Attribute} package for field
//
// !INTERFACE:
int Attribute::AttributeWriteXMLbufferfieldT(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                //  in - io pointer to write
        int &index,                    //  in - counter
        const int &columns) const{     //  in - columns
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    char msgbuf[4*ESMF_MAXSTR];
    int localrc;
    unsigned int i;
    Attribute *attpack;

    attpack = NULL;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    for (i=0; i<attrList.size(); i++) {
        if (index == 0) {
            localrc = io_xml->writeStartElement("variable", "", 3, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
        string name = attrList.at(i)->attrName;
        if (attrList.at(i)->items == 0) {
            localrc = io_xml->writeElement(name, "", 4, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else if (attrList.at(i)->items == 1) {
            ostringstream outstring;
            switch (attrList.at(i)->tk)
            {
                case ESMC_TYPEKIND_I4:
                    outstring << attrList.at(i)->vip.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 4, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_I8:
                    outstring << attrList.at(i)->vlp.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 4, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_R4:
                    outstring << attrList.at(i)->vfp.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 4, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_R8:
                    outstring << attrList.at(i)->vdp.at(0);
                    localrc = io_xml->writeElement(name, outstring.str(), 4, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                case ESMC_TYPEKIND_LOGICAL:
                    if (attrList.at(i)->vbp.at(0) == ESMF_TRUE) {
                        localrc = io_xml->writeElement(name, "true", 4, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    } else if (attrList.at(i)->vbp.at(0) == ESMF_FALSE) {
                        localrc = io_xml->writeElement(name, "false", 4, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    }
                    break;

                case ESMC_TYPEKIND_CHARACTER:
                    localrc = io_xml->writeElement(name, attrList.at(i)->vcpp.at(0), 4, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;

                default:
                    localrc = io_xml->writeElement(name, "N/A", 4, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    break;
            }

        } else if (attrList.at(i)->items > 1) {
            sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
            ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO, ESMC_CONTEXT);
        } else {
            sprintf(msgbuf,"Items < 1, problem.");
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;
        }
        ++index;
        if (index == columns) {
            localrc = io_xml->writeEndElement("variable", 3);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
    }

    for(i=0; i<packList.size(); i++)
        localrc = packList.at(i)->AttributeWriteXMLbufferfieldT(io_xml,index,columns);

    return ESMF_SUCCESS;

} // end AttributeWriteXMLbufferfieldT
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteWaterMLbuffieldT"
//BOPI
// !IROUTINE:  AttributeWriteWaterMLbuffieldT - Write contents of an {\tt Attribute} package for field
//
// !INTERFACE:
int Attribute::AttributeWriteWaterMLbuffieldT(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                //  in - io pointer to write
        int &index,                    //  in - counter
        const int &columns) const{     //  in - columns
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    char msgbuf[4*ESMF_MAXSTR];
    int localrc;
    unsigned int i;
    Attribute *attpack;

    // TODO:  replace with actual field->array reference/pointer
    double array3d[]={0.1192, // 1 (name, values match those in
                      0.114,  // 2   WaterML use_test_case)
                      0.1424, // 3
                      0.1814, // 4
                      0.3841, // 5
                      0.9124, // 6
                      0.0655, // 7
                      0.4342, // 8
                      0.9543, // 9
                      0.7308, // 10
                      0.7004, // 11
                      0.6097, // 12
                      0.5544, // 13
                      0.6182, // 14
                      0.5476, // 15
                      0.5728, // 16
                      0.369,  // 17
                      0.3664, // 18
                      0.4767, // 19
                      0.3144, // 20
                      0.4517, // 21
                      0.3838, // 22
                      0.4702, // 23
                      0.389,  // 24
                      0.4949, // 25
                      0.2778, // 26
                      0.3576, // 27
                      0.3618, // 28
                      0.2803};// 29

    attpack = NULL;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // skip TimeSeries attpack (just a wrapper)
    if (attrPurpose.compare("TimeSeries")!=0) {

        // attpack header
        if (attrPurpose.compare("sourceInfo")==0) {
            localrc = io_xml->writeStartElement("sourceInfo", "", 2, 1,
                                                "xsi:type", "SiteInfoType");
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else if (attrPurpose.compare("variable")==0) {
            localrc = io_xml->writeStartElement("variable", "", 2, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }

        // print each attribute in attpack
        string xmlAttName[6], xmlAttVal[6];
        int xmlAttCount=0;
        double val;
        for (i=0; i<attrList.size(); i++) {
            // TODO: check for #items, tk

            if (attrPurpose.compare("sourceInfo")==0) {
                if (attrList.at(i)->attrName.compare("siteCode")==0) {
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 3, 2,
                                                   "network", "LittleBearRiver", "siteID", "2");
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else if (attrList.at(i)->attrName.compare("latitude")==0) {
                    localrc = io_xml->writeStartElement("geoLocation", "", 3, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeStartElement("geogLocation", "", 4, 2,
                                                        "xsi:type", "LatLonPointType", "srs", "EPSG:4269");
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 5, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else if (attrList.at(i)->attrName.compare("longitude")==0) {
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 5, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeEndElement("geogLocation", 4);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else if (attrList.at(i)->attrName.compare("X")==0) {
                    localrc = io_xml->writeStartElement("localSiteXY", "", 4, 1,
                                                        "projectionInformation", " NAD83 / UTM zone 12N");
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 5, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else if (attrList.at(i)->attrName.compare("Y")==0) {
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 5, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeEndElement("localSiteXY", 4);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeEndElement("geoLocation", 3);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else if (attrList.at(i)->attrName.compare("County")==0 ||
                           attrList.at(i)->attrName.compare("State")==0  ||
                           attrList.at(i)->attrName.compare("Site Comments")==0) {
                    localrc = io_xml->writeElement("note", attrList.at(i)->vcpp.at(0), 3, 1,
                                                   "title", attrList.at(i)->attrName.c_str());
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else { // siteName or verticalDatum
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 3, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }
            } else if (attrPurpose.compare("variable")==0) {
                if (attrList.at(i)->attrName.compare("variableCode")==0) {
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 3, 3,
                                                   "vocabulary", "LBR", "default", "true",
                                                   "variableID", "39");
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else if (attrList.at(i)->attrName.compare("units")==0) {
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 3, 2,
                                                   "unitsAbbreviation", "mg/L", "unitsCode", "199");
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else if (attrList.at(i)->attrName.compare("timeSupport")==0) {
                    localrc = io_xml->writeStartElement(attrList.at(i)->attrName, "", 3, 1,
                                                        "isRegular", (attrList.at(i)->vcpp.at(0)).c_str());
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeEndElement(attrList.at(i)->attrName, 3);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else {
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 3, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }
            } else if (attrPurpose.compare("values")==0) {
                // collect attr names, values to output at end of loop
                xmlAttName[xmlAttCount] = attrList.at(i)->attrName;
                xmlAttVal[xmlAttCount]  = attrList.at(i)->vcpp.at(0);
                xmlAttCount++;
            } else if (attrPurpose.compare((attrPurpose.size()-2),2,".1")==0) {
                // <value>
                // collect attr names, values to output at end of loop
                xmlAttName[xmlAttCount] = attrList.at(i)->attrName;
                xmlAttVal[xmlAttCount]  = attrList.at(i)->vcpp.at(0);
                xmlAttCount++;
                if (attrList.at(i)->attrName.compare("sampleID")==0) {
                    // TODO: get value from array here
                    val =
                            array3d[atoi((attrList.at(i)->attrPurpose.substr(5,2)).c_str())-1];
                }
            } else if (attrPurpose.compare("method")==0) {
                if (attrList.at(i)->attrName.compare("methodID")==0) {
                    localrc = io_xml->writeStartElement(attrPurpose, "", 3, 1,
                                                        attrList.at(i)->attrName.c_str(),
                                                        attrList.at(i)->vcpp.at(0).c_str());
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else {
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 4, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeEndElement(attrPurpose, 3);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }
            } else if (attrPurpose.compare("source")==0) {
                if (attrList.at(i)->attrName.compare("sourceID")==0) {
                    localrc = io_xml->writeStartElement(attrPurpose, "", 3, 1,
                                                        attrList.at(i)->attrName.c_str(),
                                                        attrList.at(i)->vcpp.at(0).c_str());
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else {
                    localrc = io_xml->writeElement(attrList.at(i)->attrName,
                                                   attrList.at(i)->vcpp.at(0), 4, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    if (attrList.at(i)->attrName.compare("SourceDescription")==0) {
                        localrc = io_xml->writeStartElement("ContactInformation", "", 4, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                        localrc = io_xml->writeElement("ContactName", "Amber Spackman", 5, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                        localrc = io_xml->writeElement("TypeOfContact", "main", 5, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                        localrc = io_xml->writeElement("Phone", "1-435-797-0045", 5, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                        localrc = io_xml->writeElement("Email", "amber.s@aggiemail.usu.edu", 5, 0);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                        localrc = io_xml->writeElement("Address",
                                                       "8200 Old Main Hill, Logan, Utah 84322-8200", 5, 1,
                                                       "xsi:type", "xsd:string");
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                        localrc = io_xml->writeEndElement("ContactInformation", 4);
                        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    }
                }
            }
        }

        if (attrPurpose.compare("values")==0) {
            localrc = io_xml->writeStartElement(attrPurpose, "", 2, xmlAttCount,
                                                xmlAttName[0].c_str(), xmlAttVal[0].c_str(),
                                                xmlAttName[1].c_str(), xmlAttVal[1].c_str(),
                                                xmlAttName[2].c_str(), xmlAttVal[2].c_str());
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else if (attrPurpose.compare((attrPurpose.size()-2),2,".1")==0) {
            // <value>
            ostringstream outstring;
            outstring << val;
            localrc = io_xml->writeElement("value", outstring.str(), 3, xmlAttCount,
                                           xmlAttName[0].c_str(), xmlAttVal[0].c_str(),
                                           xmlAttName[1].c_str(), xmlAttVal[1].c_str(),
                                           xmlAttName[2].c_str(), xmlAttVal[2].c_str(),
                                           xmlAttName[3].c_str(), xmlAttVal[3].c_str(),
                                           xmlAttName[4].c_str(), xmlAttVal[4].c_str(),
                                           xmlAttName[5].c_str(), xmlAttVal[5].c_str());
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else if (attrPurpose.compare("source")==0) {
            localrc = io_xml->writeEndElement("source", 3);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeEndElement("values", 2);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
            // attpack footer
        else if (attrPurpose.compare("sourceInfo")==0) {
            localrc = io_xml->writeEndElement("sourceInfo", 2);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else if (attrPurpose.compare("variable")==0) {
            localrc = io_xml->writeEndElement("variable", 2);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }

    } // end if not TimeSeries attpack wrapper

    // recurse remaining attpacks
    for(i=0; i<packList.size(); i++)
        localrc = packList.at(i)->AttributeWriteWaterMLbuffieldT(io_xml,index,columns);

    return ESMF_SUCCESS;

} // end AttributeWriteWaterMLbuffieldT

} // namespace ESMCI
