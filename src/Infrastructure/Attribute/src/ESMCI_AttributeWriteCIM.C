// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMCI_AttributeWriteCIM.C"

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Attribute} methods declared
// in the companion file ESMCI_Attribute.h for writing CIM compliant files.
//
//-----------------------------------------------------------------------------
// associated class definition file and others
#include "ESMCI_Attribute.h"

#include "ESMCI_Macros.h"
#include "ESMCI_IO_XML.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Time.h"

#include <cstdlib>
#include <cstring>
#include <vector>

using std::string;
using std::vector;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIM"
//BOPI
// !IROUTINE:  AttributeWriteCIM - Write contents of a CIM {\tt Attribute} package
//
// !INTERFACE:
int Attribute::AttributeWriteCIM(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                   //  in - io pointer to write
        const string &convention) const { // in - convention
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//    The output logic design for the WriteCIM*() methods is to match the CIM
//    release validation requirements, no more, no less.  It imposes the least
//    restrictions on what attributes are required to be set in order to
//    produce a valid CIM XML file.  If a required attribute is not set, a
//    warning message is logged, and output continues with whatever else can be
//    output.  When the user tries to validate such output, it will be invalid,
//    but between the output file, its validation errors, and the ESMF warning
//    messages in the log files, the user should be able to determine what
//    attributes to set to make the output valid.  The ESMF Fortran Reference
//    manual for Attribute packages documents what the required attributes are.
//    Regardless of whether the output is valid, it will probably be
//    harvestable by ESG, since ESG does not first validate the file, as of
//    ESG 1.3.1.
//
//EOPI
//-----------------------------------------------------------------------------
    int localrc;
    string helper1, helper2;

    // save this attribute pointer as the root of the tree to be written out,
    // for later use in multiple nested recursions in producing the xml output
    writeRoot = (ESMCI::Attribute*)this;

    //
    // determine if a grid is present
    //
    bool gridPresent = false;
    string gridGUID = "";
    gridPresent = AttributeWriteCIMgridPresent(convention);
    // CIM1.5 does not support grids
    if (convention.compare(CIM_1_5_CONV)==0) gridPresent = false;
    if (gridPresent) ESMC_GenerateGUID(gridGUID);

    //
    // Write the CIM XML file header
    //
    if (convention.compare(CIM_1_5_CONV)==0) {
        helper1 = "http://www.purl.org/org/esmetadata/cim/1.5/schemas";
        helper2 = "http://www.purl.org/org/esmetadata/cim/1.5/schemas/cim.xsd";
    }
    else if (convention.compare(CIM_1_5_1_CONV)==0) {
        helper1 = "http://www.purl.org/org/esmetadata/cim/1.5.1/schemas";
        helper2 = "http://www.purl.org/org/esmetadata/cim/1.5.1/schemas/cim.xsd";
    }
    else if (convention.compare(CIM_1_7_1_CONV)==0) {
        helper1 = "http://www.purl.org/org/esmetadata/cim/1.7.1/schemas";
        helper2 = "http://www.purl.org/org/esmetadata/cim/1.7.1/schemas/cim.xsd";
    }
    localrc = io_xml->writeStartElement("CIMDocumentSet", "", 0, 7,
                                        "xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance",
                                        "xmlns:xlink", "http://www.w3.org/1999/xlink",
                                        "xmlns:gml", "http://www.opengis.net/gml/3.2",
                                        "xmlns:gco", "http://www.isotc211.org/2005/gco",
                                        "xmlns:gmd", "http://www.isotc211.org/2005/gmd",
                                        "xmlns", helper1.c_str(),
                                        "xsi:schemaLocation", helper2.c_str());
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    //
    // write CIM document node <modelComponent>
    //
    localrc = AttributeWriteCIMmodelComp(io_xml, convention, 1, gridPresent, gridGUID);
    if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                      "Attribute failed recursing in WriteXML", ESMC_CONTEXT, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        return ESMF_FAILURE;
    }

    //
    // write CIM document node <simulationRun>
    //
    localrc = AttributeWriteCIMsimRun(io_xml, convention);
    if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                      "Attribute failed recursing in WriteXML", ESMC_CONTEXT, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        return ESMF_FAILURE;
    }

    //
    // write CIM document node platform>
    //
    localrc = AttributeWriteCIMplatform(io_xml, convention);
    if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                      "Attribute failed recursing in WriteXML", ESMC_CONTEXT, &localrc);
        localrc = ESMCI_IO_XMLDestroy(&io_xml);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        return ESMF_FAILURE;
    }

    //
    // write the Gridspec
    //
    if (gridPresent) {
        localrc = AttributeWriteCIMgrids(io_xml, convention, gridGUID, 1, false);
        if (localrc != ESMF_SUCCESS) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Attribute failed recursing in WriteXML", ESMC_CONTEXT, &localrc);
            localrc = ESMCI_IO_XMLDestroy(&io_xml);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;
        }
    }

    //
    // Write the CIM XML file footer
    //
    localrc = io_xml->writeEndElement("CIMDocumentSet", 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    return ESMF_SUCCESS;

} // end AttributeWriteCIM
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMmodelComp"
//BOPI
// !IROUTINE:  AttributeWriteCIMmodelComp - Write contents of a CIM {\tt Attribute} package modelComponent record
//
// !INTERFACE:
int Attribute::AttributeWriteCIMmodelComp(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                //  in - io pointer to write
        const string &convention,      // in - convention
        int indent,                    //  in - starting indent level
        const bool gridPresent,        // in - bool to tell if there is a grid
        const string gridGUID) const { // in - grid GUID
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    int localrc;
    Attribute *attpack = NULL;
    Attribute *attr = NULL;
    static int callCount=0;
    int callCountBeforeRecursion;
    bool inObjectTree, inThisCompTreeOnly, inNestedAttPacks;

    vector<string> valuevector;
    string value;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // distinguish between 1st call and recursive calls
    callCount++;

    string attPackInstanceName;
    attpack = AttPackGet(convention, MODEL_COMP_PURP, "comp", attPackInstanceName,
                         ESMC_ATTNEST_ON);
    if (attpack == NULL) return ESMF_SUCCESS;  // if package not found, return

    localrc = io_xml->writeStartElement("modelComponent", "", indent++, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    attr = attpack->AttPackGetAttribute("ShortName", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        attr = attpack->AttPackGetAttribute("Version", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            value += valuevector.at(0); // append Version to ShortName
        }
        localrc = io_xml->writeElement("shortName", value, indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'ShortName'+'Version'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        if (indent <= 2) { // top-level (e.g. coupler) component
            localrc = io_xml->writeComment(
                    "   to ESG: 1) Component name, left-side navigator bar.", indent+1);
            localrc = io_xml->writeComment(
                    "   to ESG: 2) \"Simulation Metadata:\", top of display, 1st line; "
                            "'SimulationShortName' appended.", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else { // lower-level (e.g. gridded) component
            localrc = io_xml->writeComment(
                    "   to ESG: Component name, left-side navigator bar.", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
    } else {
        ESMC_LogDefault.Write("Attribute ShortName in standard attribute package "
                                      "(convention='CIM 1.5', purpose='ModelComp')"
                                      " required to be set, to produce valid CIM XML output.",
                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }
    attr = attpack->AttPackGetAttribute("LongName", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("longName", value, indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'LongName'.", indent+1);
        localrc = io_xml->writeComment(
                "   to ESG: \"Full Name:\", top of display, 2nd line; "
                        "'SimulationLongName' appended.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("Description", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("description", value, indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'Description'.", indent+1);
        localrc = io_xml->writeComment(
                "   to ESG: \"Description:\" in top box.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    // <componentProperties><componentProperty> nodes
    bool CPgeneral = AttPackIsSet(convention,
                                  COMP_PROP_PURP, "comp",
                                  inObjectTree=false, // only look at this comp, not children
                                  inThisCompTreeOnly=true,
                                  inNestedAttPacks=false);

    bool CPscientific = AttPackIsSet(convention,
                                     SCI_PROP_PURP, "comp",
                                     inObjectTree=false, // only look at this comp, not children
                                     inThisCompTreeOnly=true,
                                     inNestedAttPacks=false);

    bool CPfield   = AttPackIsSet(ESMF_CONV, GENERAL_PURP, "field",
                                  inObjectTree=true, inThisCompTreeOnly=true,
                                  inNestedAttPacks=true); // only look for CF/Extended
    // atts nested within ESMF/General,
    // nested within CIM/Inputs.
    // TODO: enforce CIM/Inputs as
    // top-level attpack (via pathing
    // mechanism?)
    if (CPgeneral || CPscientific || CPfield) {
        localrc = io_xml->writeStartElement("componentProperties", "", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        if (CPgeneral) {
            localrc = AttributeWriteCIMCP(io_xml, convention,
                                          COMP_PROP_PURP, indent);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
        if (CPscientific) {
            localrc = AttributeWriteCIMCP(io_xml, convention,
                                          SCI_PROP_PURP, indent);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
        if (CPfield) {
            localrc = AttributeWriteCIMCPfield(io_xml, convention, indent);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }

        localrc = io_xml->writeEndElement("componentProperties", indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    // TODO: uncomment and expand when we have better definition from CIM
    //localrc = io_xml->writeElement("numericalProperties", "", indent, 0);
    //ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    //
    //localrc = io_xml->writeElement("scientificProperties", "", indent, 0);
    //ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // <responsibleParty> nodes
    localrc = attpack->AttributeWriteCIMRP(io_xml, indent);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    attr = attpack->AttPackGetAttribute("ReleaseDate", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("releaseDate", value, indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'ReleaseDate'.", indent+1);
        localrc = io_xml->writeComment(
                "   to ESG: \"Release Date\" under tabs \"Properties/Basic\"", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    // <citation> nodes
    localrc = attpack->AttributeWriteCIMcitation(io_xml, indent);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // <onlineResource>
    attr = attpack->AttPackGetAttribute("URL", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("onlineResource", "", indent, 0);
        localrc = io_xml->writeStartElement("gmd:linkage", "", ++indent, 0);
        localrc = io_xml->writeElement("gmd:URL", value, ++indent, 0);
        localrc = io_xml->writeComment("from ESMF: CIM/Main, 'URL'.", indent+1);
        localrc = io_xml->writeComment("   to ESG: Not ingested yet.", indent+1);
        localrc = io_xml->writeEndElement("gmd:linkage", --indent);
        localrc = io_xml->writeEndElement("onlineResource", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    // <grid>
    if (gridPresent) {
        localrc = AttributeWriteCIMgridref(io_xml, indent, gridGUID);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

#if 0
    // <composition><coupling> (all CIM fields within all child components,
  // written only in top-level component (e.g. coupler))
  if (callCount == 1) { // for top-level component only
    if (AttPackIsSet(convention, INPUTS_PURP, "field",
                     inObjectTree=true,
                     inThisCompTreeOnly=false,  // look at all child comps
                     inNestedAttPacks=false)) { // only look at CIM/Inputs atts,
                                                // not nested CF atts
      localrc = io_xml->writeStartElement("composition", "", 2, 0);
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

      localrc = AttributeWriteCIMcomposition(io_xml);

      localrc = io_xml->writeElement("description", "", 3, 0);
      localrc = io_xml->writeEndElement("composition", 2);
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
  }
#endif

    // <childComponent> tree
    callCountBeforeRecursion = callCount;
    for(int i=0; i<linkList.size(); i++) {
        Attribute *ap;
        for(int j=0; j<linkList.at(i)->packList.size(); j++) {
            ap = linkList.at(i)->packList.at(j);
            if (!(ap->attrConvention.compare(convention)==0 &&
                  ap->attrPurpose.compare(MODEL_COMP_PURP)==0 &&
                  ap->attrObject.compare("comp")==0)) {
                continue; // skip non-CIM components
            } else {
                // recurse through child CIM components
                localrc = io_xml->writeStartElement("childComponent", "", indent, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = linkList.at(i)->AttributeWriteCIMmodelComp(io_xml, convention, ++indent, false, gridGUID);

                localrc = io_xml->writeEndElement("childComponent", --indent);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
        }
    }

    attr = attpack->AttPackGetAttribute("SimulationNumberOfProcessingElements", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("deployment", "", indent, 0);
        localrc = io_xml->writeStartElement("parallelisation", "", ++indent, 0);
        localrc = io_xml->writeElement("processes", value, ++indent, 0);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'SimulationNumberOfProcessingElements'.", indent+1);
        localrc = io_xml->writeComment(
                "   to ESG: Not ingested yet.", indent+1);
        localrc = io_xml->writeEndElement("parallelisation", --indent);
        localrc = io_xml->writeEndElement("deployment", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    attr = attpack->AttPackGetAttribute("ModelType", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("type", "", indent, 2,
                                            "open", "true",
                                            "value", value.c_str());
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'ModelType'.", indent+1);
        localrc = io_xml->writeComment(
                "   to ESG: \"Realm:\", expanded under component name, "
                        "left-side navigator bar.", indent+1);
        localrc = io_xml->writeStartElement("controlledVocabulary","", ++indent, 0);
        // TODO:  make new att for ModelTypeCV ? (DRS_CMIP5_componentType, metafor)
        if (callCountBeforeRecursion == 1) { // for top-level component only
            localrc = io_xml->writeElement("name", "metafor", ++indent, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else { // for all child components
            localrc = io_xml->writeElement("name", "DRS_CMIP5_componentType",
                                           ++indent, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
        localrc = io_xml->writeElement("server",
                                       "http://proj.badc.rl.ac.uk/svn/metafor/cmip5q/trunk", indent, 0);
        localrc = io_xml->writeEndElement("controlledVocabulary", --indent);
        localrc = io_xml->writeEndElement("type", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        ESMC_LogDefault.Write("Attribute ModelType in standard attribute package "
                                      "(convention='CIM 1.5', purpose='ModelComp')"
                                      " required to be set, to produce valid CIM XML output.",
                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }

    // generate and save a GUID for this component, then output it
    ESMC_GenerateGUID(attpack->attrGUID);
    localrc = io_xml->writeElement("documentID", attpack->attrGUID, indent, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    attr = attpack->AttPackGetAttribute("MetadataVersion", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("documentVersion", value, indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'MetadataVersion'.", indent+1);
        localrc = io_xml->writeComment(
                "   to ESG: \"Metadata Version\" under tabs \"Properties/Basic\".",
                indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        localrc = io_xml->writeElement("documentVersion", "1.0", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    // stamp the metadata source as ESMF, version x
    string metadataSource = "ESMF Version ";
    metadataSource += ESMF_VERSION_STRING;
    localrc = io_xml->writeStartElement("documentAuthor", "", indent, 0);
    localrc = io_xml->writeStartElement("gmd:individualName", "", ++indent, 0);
    localrc = io_xml->writeElement("gco:CharacterString", metadataSource.c_str(),                                  ++indent, 0);
    localrc = io_xml->writeEndElement("gmd:individualName", --indent);
    localrc = io_xml->writeStartElement("gmd:role", "", indent, 0);
    localrc = io_xml->writeElement("gmd:CI_RoleCode", "", ++indent, 2,
                                   "codeList", "",
                                   "codeListValue",
                                   "documentAuthor");
    localrc = io_xml->writeEndElement("gmd:role", --indent);
    localrc = io_xml->writeEndElement("documentAuthor", --indent);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // use TimeMgr for timestamping
    // TODO: also use timezone when implemented in TimeMgr
    Time dateTime(0, 0, 1, ESMC_NULL_POINTER, ESMC_CALKIND_GREGORIAN, 0);
    char dateTimeString[ESMF_MAXSTR];
    dateTime.syncToRealTime();
    dateTime.getString(dateTimeString);
    localrc = io_xml->writeElement("documentCreationDate",
                                   dateTimeString, indent, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // <documentGenealogy>
    if (attpack->AttPackGetAttribute("PreviousVersionDescription", ESMC_ATTNEST_ON)->isSet() ||
        attpack->AttPackGetAttribute("PreviousVersion", ESMC_ATTNEST_ON)->isSet()) {

        localrc = io_xml->writeStartElement("documentGenealogy", "", indent, 0);
        localrc = io_xml->writeStartElement("relationship", "", ++indent, 0);
        localrc = io_xml->writeStartElement("documentRelationship", "", ++indent, 2,
                                            "direction", "toTarget",
                // direction: {'toTarget', 'fromTarget'}
                                            "type", "previousVersionOf");
        // type: {'similarTo', 'other', 'laterVersionOf',
        //        'previousVersionOf', 'fixedVersionOf'}
        // Both direction & type are set to match Metafor Questionnaire output.
        // Note: ESG 1.3.1 harvester CimHarvest.java looks for "previousVersion";
        //       bug was reported in Jira ticket #2692 8/31/11.

        attr = attpack->AttPackGetAttribute("PreviousVersionDescription", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            value = valuevector.at(0);
            localrc = io_xml->writeElement("description", value, ++indent, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeComment(
                    "from ESMF: CIM/Main, 'PreviousVersionDescription'.", indent+1);
            localrc = io_xml->writeComment(
                    "   to ESG: Not ingested yet.", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }

        attr = attpack->AttPackGetAttribute("PreviousVersion", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            value = valuevector.at(0);
            localrc = io_xml->writeStartElement("target", "", indent, 0);
            localrc = io_xml->writeStartElement("reference", "", ++indent, 0);
            localrc = io_xml->writeElement("name", value, ++indent, 0);
            localrc = io_xml->writeComment(
                    "from ESMF: CIM/Main, 'PreviousVersion'.", indent+1);
            localrc = io_xml->writeComment(
                    "   to ESG: Not ingested yet.", indent+1);
            localrc = io_xml->writeEndElement("reference", --indent);
            localrc = io_xml->writeEndElement("target", --indent);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else {
            ESMC_LogDefault.Write("Attribute PreviousVersion in standard attribute "
                                          "package (convention='CIM 1.5', "
                                          "purpose='ModelComp') "
                                          "required to be set, when attribute PreviousVersionDescription is also "
                                          "set, to produce valid CIM XML output.",
                                  ESMC_LOGMSG_WARN, ESMC_CONTEXT);
        }

        localrc = io_xml->writeEndElement("documentRelationship", --indent);
        localrc = io_xml->writeEndElement("relationship", --indent);
        localrc = io_xml->writeEndElement("documentGenealogy", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    localrc = io_xml->writeEndElement("modelComponent", --indent);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    return ESMF_SUCCESS;

} // end AttributeWriteCIMmodelComp
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMsimRun"
//BOPI
// !IROUTINE:  AttributeWriteCIMsimRun - Write contents of a CIM {\tt Attribute} package simulationRun record
//
// !INTERFACE:
int Attribute::AttributeWriteCIMsimRun(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                  //  in - io pointer to write
        const string &convention) const { //  in - convention
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    int localrc;
    char msgbuf[4*ESMF_MAXSTR];
    Attribute *attpack = NULL;
    Attribute *attr = NULL;
    bool inObjectTree, inThisCompTreeOnly, inNestedAttPacks;

    vector<string> valuevector;
    string value;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    string attPackInstanceName;
    attpack = AttPackGet(convention, MODEL_COMP_PURP, "comp", attPackInstanceName,
                         ESMC_ATTNEST_ON);
    if (attpack == NULL) return ESMF_SUCCESS;

    localrc = io_xml->writeStartElement("simulationRun", "", 1, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    attr = attpack->AttPackGetAttribute("SimulationRationale", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("rationale", value, 2, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'SimulationRationale'.", 3);
        localrc = io_xml->writeComment(
                "   to ESG: Not ingested yet.", 3);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("SimulationProjectName", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("project", "", 2, 2,
                                            "open", "true",
                                            "value", value.c_str());
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'SimulationProjectName'.", 3);
        localrc = io_xml->writeComment(
                "   to ESG: \"Project\" under tabs \"Properties/Basic\".", 3);
        localrc = io_xml->writeStartElement("controlledVocabulary", "", 3, 0);
        localrc = io_xml->writeElement("name", "project", 4, 0);
        localrc = io_xml->writeElement("server",
                                       "http://proj.badc.rl.ac.uk/svn/metafor/cmip5q/trunk", 4, 0);
        localrc = io_xml->writeEndElement("controlledVocabulary", 3);
        localrc = io_xml->writeEndElement("project", 2);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("SimulationShortName", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("shortName", value, 2, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'SimulationShortName'.", 3);
        localrc = io_xml->writeComment(
                "   to ESG: 1) \"Simulation Metadata:\", top of display, 1st line; "
                        "appended to 'ShortName' and 'Version' of top-level component.", 3);
        localrc = io_xml->writeComment(
                "   to ESG: 2) Appears as 2nd part of simulation name when "
                        "Curator website is searched for Simulations/Realm/Earth System.", 3);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        ESMC_LogDefault.Write("Attribute SimulationShortName in standard attribute "
                                      "package (convention='CIM 1.5', "
                                      "purpose='ModelComp') "
                                      "required to be set, to produce valid CIM XML output.",
                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }
    attr = attpack->AttPackGetAttribute("SimulationLongName", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("longName", value, 2, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'SimulationLongName'.", 3);
        localrc = io_xml->writeComment(
                "   to ESG: \"Full Name:\", top of display, 2nd line under title, "
                        "appended to 'LongName'.", 3);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        ESMC_LogDefault.Write("Attribute SimulationLongName in standard attribute "
                                      "package (convention='CIM 1.5', "
                                      "purpose='ModelComp') "
                                      "required to be set, to produce valid CIM XML output.",
                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }

    // TODO: required elements in CIM; need atts defined in package ?
    localrc = io_xml->writeStartElement("supports", "", 2, 0);
    localrc = io_xml->writeElement("reference", "", 3, 0);
    localrc = io_xml->writeEndElement("supports", 2);
    localrc = io_xml->writeStartElement("calendar", "", 2, 0);
    localrc = io_xml->writeElement("realCalendar", "", 3, 0);
    localrc = io_xml->writeEndElement("calendar", 2);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // <input> -- for all CIM fields within all child components,
    // written only here in the one top-level <simulationRun> document)
    if (AttPackIsSet(convention, INPUTS_PURP, "field",
                     inObjectTree=true,
                     inThisCompTreeOnly=false,  // look at all child comps
                     inNestedAttPacks=false)) { // only look at CIM/Inputs atts,
        // not nested CF atts
        localrc = AttributeWriteCIMinput(io_xml, convention);
    }

    localrc = io_xml->writeStartElement("dateRange", "", 2, 0);
    localrc = io_xml->writeStartElement("closedDateRange", "", 3, 0);
    attr = attpack->AttPackGetAttribute("SimulationDuration", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("duration", value, 4, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'SimulationDuration'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Simulation Duration\" under tabs \"Properties/Basic\".",5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("SimulationEndDate", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("endDate", value, 4, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'SimulationEndDate'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Simulation End Date\" under tabs \"Properties/Basic\".", 5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("SimulationStartDate", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("startDate", value, 4, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'SimulationStartDate'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Simulation Start Date\" under tabs \"Properties/Basic\".",5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        ESMC_LogDefault.Write("Attribute SimulationStartDate in standard attribute "
                                      "package (convention='CIM 1.5', "
                                      "purpose='ModelComp') "
                                      "required to be set, to produce valid CIM XML output.",
                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }
    localrc = io_xml->writeEndElement("closedDateRange", 3);
    localrc = io_xml->writeEndElement("dateRange", 2);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // TODO: required elements in CIM; need atts defined in package ?
    localrc = io_xml->writeStartElement("model", "", 2, 0);
    localrc = io_xml->writeElement("reference", "", 3, 0);
    localrc = io_xml->writeEndElement("model", 2);

    // generate a GUID for this simulationRun document, then output it
    string GUID;
    ESMC_GenerateGUID(GUID);
    localrc = io_xml->writeElement("documentID", GUID, 2, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    attr = attpack->AttPackGetAttribute("MetadataVersion", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("documentVersion", value, 2, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'MetadataVersion'.", 3);
        localrc = io_xml->writeComment(
                "   to ESG: \"Metadata Version\" under tabs \"Properties/Basic\".", 3);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        localrc = io_xml->writeElement("documentVersion", "1.0", 2, 0);
    }
    attr = attpack->AttPackGetAttribute("SimulationEnsembleID", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("externalID", "", 2, 2,
                                            "open", "true",
                                            "value", value.c_str());
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'SimulationEnsembleID'.", 3);
        localrc = io_xml->writeComment(
                "   to ESG: \"Ensemble Identification\" under tabs "
                        "\"Properties/Basic\".", 3);
        localrc = io_xml->writeStartElement("standard", "", 3, 0);
        localrc = io_xml->writeElement("name", "DRS_CMIP5_ensembleType", 4, 0);
        localrc = io_xml->writeEndElement("standard", 3);
        localrc = io_xml->writeEndElement("externalID", 2);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    // use TimeMgr for timestamping
    // TODO: also use timezone when implemented in TimeMgr
    Time dateTime(0, 0, 1, ESMC_NULL_POINTER, ESMC_CALKIND_GREGORIAN, 0);
    char dateTimeString[ESMF_MAXSTR];
    dateTime.syncToRealTime();
    dateTime.getString(dateTimeString);
    localrc = io_xml->writeElement("documentCreationDate",
                                   dateTimeString, 2, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    localrc = io_xml->writeEndElement("simulationRun", 1);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    return ESMF_SUCCESS;

} // end AttributeWriteCIMsimRun
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMplatform"
//BOPI
// !IROUTINE:  AttributeWriteCIMplatform - Write contents of a CIM {\tt Attribute} package platform record
//
// !INTERFACE:
int Attribute::AttributeWriteCIMplatform(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                  //  in - io pointer to write
        const string &convention) const { //  in - convention
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    int localrc;
    char msgbuf[4*ESMF_MAXSTR];
    Attribute *attpack = NULL;
    Attribute *attr = NULL;
    Attribute *attpackMain = NULL;

    vector<string> valuevector;
    string value, machineName, compilerName;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    string attPackInstanceName;
    attpack = AttPackGet(convention, PLATFORM_PURP, "comp", attPackInstanceName,
                         ESMC_ATTNEST_ON);
    if (attpack == NULL) return ESMF_SUCCESS;

    localrc = io_xml->writeStartElement("platform", "", 1, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    attr = attpack->AttPackGetAttribute("MachineName", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        machineName = valuevector.at(0);
    } else {
        ESMC_LogDefault.Write("Attribute MachineName in "
                                      "standard attribute package (convention='CIM 1.5', "
                                      "purpose='Platform') "
                                      "required to be set, to produce valid CIM XML output.",
                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }
    attr = attpack->AttPackGetAttribute("CompilerName", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        compilerName = valuevector.at(0);
    }

    if (!machineName.empty()) {
        if (compilerName.empty()) {
            localrc = io_xml->writeElement("shortName", machineName, 2, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeComment(
                    "from ESMF: CIM/Platform, 'MachineName'.", 3);
            localrc = io_xml->writeComment(
                    "   to ESG: Ingested below from "
                            "[platform][unit][machine][machineName].", 3);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else {
            localrc = io_xml->writeElement("shortName", machineName + "_" +
                                                        compilerName, 2, 0);
            localrc = io_xml->writeElement("longName", "Machine " + machineName +
                                                       " and compiler " + compilerName, 2, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeComment(
                    "from ESMF: CIM/Platform, 'MachineName' and 'CompilerName'.", 3);
            localrc = io_xml->writeComment(
                    "   to ESG: Ingested below from [platform][unit][machine][machineName] "
                            "and [platform][unit][compiler][compilerName].", 3);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
    }

    attr = attpack->AttPackGetAttribute("MachineDescription", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("description", value, 2, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Platform, 'MachineDescription'.", 3);
        localrc = io_xml->writeComment(
                "   to ESG: Not ingested yet.", 3);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    localrc = io_xml->writeStartElement("unit", "", 2, 0);
    localrc = io_xml->writeStartElement("machine", "", 3, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    if (!machineName.empty()) {
        localrc = io_xml->writeElement("machineName", machineName, 4, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Platform, 'MachineName'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Machine Name\" under tabs \"Properties/Technical\".", 5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("MachineSystem", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("machineSystem", value, 4, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Platform, 'MachineSystem'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Hardware Type\" under tabs \"Properties/Technical\".", 5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("MachineOperatingSystem", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("machineOperatingSystem", "", 4, 2,
                                       "open", "true",
                                       "value", value.c_str());
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Platform, 'MachineOperatingSystem'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Operating System\" under tabs \"Properties/Technical\".",5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("MachineVendor", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("machineVendor", "", 4, 2,
                                       "open", "true",
                                       "value", value.c_str());
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Platform, 'MachineVendor'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: Ingested, but only used to classify platform.", 5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("MachineInterconnectType", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("machineInterconnect", "", 4, 2,
                                       "open", "true",
                                       "value", value.c_str());
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Platform, 'MachineInterconnectType'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Interconnect Type\" under tabs \"Properties/Technical\".",5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("MachineMaximumProcessors", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("machineMaximumProcessors", value, 4, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Platform, 'MachineMaximumProcessors'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Maximum Processors\" under tabs "
                        "\"Properties/Technical\".", 5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("MachineCoresPerProcessor", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("machineCoresPerProcessor", value, 4, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Platform, 'MachineCoresPerProcessor'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Number of Cores per Processor\" under tabs "
                        "\"Properties/Technical\".", 5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = attpack->AttPackGetAttribute("MachineProcessorType", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("machineProcessorType", "", 4, 2,
                                       "open", "true",
                                       "value", value.c_str());
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Platform, 'MachineProcessorType'.", 5);
        localrc = io_xml->writeComment(
                "   to ESG: \"Processor\" under tabs \"Properties/Technical\".", 5);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    localrc = io_xml->writeEndElement("machine", 3);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    if (attpack->AttPackGetAttribute("CompilerName", ESMC_ATTNEST_ON)->isSet() ||
        attpack->AttPackGetAttribute("CompilerVersion", ESMC_ATTNEST_ON)->isSet()) {

        localrc = io_xml->writeStartElement("compiler", "", 3, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        attr = attpack->AttPackGetAttribute("CompilerName", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            value = valuevector.at(0);
            localrc = io_xml->writeElement("compilerName", value, 4, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeComment(
                    "from ESMF: CIM/Platform, 'CompilerName'.", 5);
            localrc = io_xml->writeComment(
                    "   to ESG: \"Compiler\" under tabs \"Properties/Technical\", "
                            "'CompilerVersion' appended.", 5);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else {
            ESMC_LogDefault.Write("Attribute CompilerName in "
                                          "standard attribute package (convention='CIM 1.5', "
                                          "purpose='Platform') "
                                          "required to be set, when attribute CompilerVersion is also set, "
                                          "to produce valid CIM XML output.",
                                  ESMC_LOGMSG_WARN, ESMC_CONTEXT);
        }
        attr = attpack->AttPackGetAttribute("CompilerVersion", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            value = valuevector.at(0);
            localrc = io_xml->writeElement("compilerVersion", value, 4, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeComment(
                    "from ESMF: CIM/Platform, 'CompilerVersion'.", 5);
            localrc = io_xml->writeComment(
                    "   to ESG: \"Compiler\" under tabs \"Properties/Technical\", "
                            "appended to 'CompilerName'.", 5);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else {
            ESMC_LogDefault.Write("Attribute CompilerVersion in "
                                          "standard attribute package (convention='CIM 1.5', "
                                          "purpose='Platform') "
                                          "required to be set, when attribute CompilerName is also set, "
                                          "to produce valid CIM XML output.",
                                  ESMC_LOGMSG_WARN, ESMC_CONTEXT);
        }
        localrc = io_xml->writeEndElement("compiler", 3);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    localrc = io_xml->writeEndElement("unit", 2);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    //localrc = io_xml->writeElement("contact", "", 2, 0);
    // TODO:  <contact><gmd:CI_ResponsibleParty>
    //localrc = io_xml->writeStartElement("contact", "", 2, 0);
    //localrc = attpack->AttributeWriteCIMRP(io_xml, 3);
    //localrc = io_xml->writeEndElement("contact", 2);
    //ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // generate a GUID for this platform document, then output it
    string GUID;
    ESMC_GenerateGUID(GUID);
    localrc = io_xml->writeElement("documentID", GUID, 2, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    // get CIM/Main package to retrieve MetadataVersion
    attpackMain = AttPackGet(convention, MODEL_COMP_PURP, "comp",
                             attPackInstanceName, ESMC_ATTNEST_ON);
    if (attpackMain == NULL) return ESMF_SUCCESS;  // if package not found, return
    attr = attpackMain->AttPackGetAttribute("MetadataVersion", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("documentVersion", value, 2, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Main, 'MetadataVersion'.", 3);
        localrc = io_xml->writeComment(
                "   to ESG: Ingested above from [modelComponent][documentVersion] "
                        "for top-level component.", 3);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        localrc = io_xml->writeElement("documentVersion", "1.0", 2, 0);
    }

    // use TimeMgr for timestamping
    // TODO: also use timezone when implemented in TimeMgr
    Time dateTime(0, 0, 1, ESMC_NULL_POINTER, ESMC_CALKIND_GREGORIAN, 0);
    char dateTimeString[ESMF_MAXSTR];
    dateTime.syncToRealTime();
    dateTime.getString(dateTimeString);
    localrc = io_xml->writeElement("documentCreationDate",
                                   dateTimeString, 2, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    localrc = io_xml->writeEndElement("platform", 1);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    return ESMF_SUCCESS;

} // end AttributeWriteCIMplatform
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMRP"
//BOPI
// !IROUTINE:  AttributeWriteCIMRP - Write contents of a CIM {\tt Attribute} package ResponsibleParty record
//
// !INTERFACE:
int Attribute::AttributeWriteCIMRP(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,      //  in - io pointer to write
        int indent) const {  //  in - starting indent level
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    int localrc;
    Attribute *attpack = NULL;
    Attribute *attr = NULL;
    bool inNestedAttPacks;

    vector<string> valuevector;
    string value, nameType, role;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // write out all nested RPs
    for(int i=0; i<packList.size(); i++) {
        // RLO: added recursion to fix the false one-level recursion that worked before
        //    because the attpack nesting for standard attpacks was making flat trees
        localrc = packList.at(i)->AttributeWriteCIMRP(io_xml, indent);
    }

    if (!(attrConvention.compare("ISO 19115")==0 &&
          attrPurpose.compare(RESP_PARTY_PURP)==0))
        return ESMF_SUCCESS; // skip non-RPs

    // if no attributes set in this attpack instance, skip it ...
    if (!(AttPackIsSet(inNestedAttPacks=false))) return ESMF_SUCCESS;

    // responsibleParty header
    localrc = io_xml->writeStartElement("responsibleParty", "", indent++, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    if (AttPackGetAttribute("Name", ESMC_ATTNEST_ON)->isSet()) {
        // first, determine name type:  individual, organization, or position.
        //   first choice is the setting of the NameType attribute ...
        nameType = "gmd:individualName";  // default
        attr = AttPackGetAttribute("NameType", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            value = valuevector.at(0);
            transform(value.begin(), value.end(), value.begin(), ::tolower);
            if (value == "individual") {
                nameType = "gmd:individualName";
            } else if (value == "organization") {
                nameType = "gmd:organisationName";
            } else if (value == "position") {
                nameType = "gmd:positionName";
            } else {
                ESMC_LogDefault.Write("Attribute NameType in "
                                              "standard attribute package (convention='ISO 19115', "
                                              "purpose='RespParty' should be one of "
                                              "{Individual, Organization, Position}.",
                                      ESMC_LOGMSG_WARN, ESMC_CONTEXT);
            }
            // ... otherwise guess based on the role ...
        } else if ((attr = AttPackGetAttribute("ResponsiblePartyRole", ESMC_ATTNEST_ON))->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            role = valuevector.at(0);
            if (role != "PI") transform(role.begin(), role.end(),
                                        role.begin(), ::tolower);
            if (role == "center" || role == "funder") {
                nameType = "gmd:organisationName";
            } else if (role == "PI" || role == "author" || role == "contact") {
                nameType = "gmd:individualName";
            }
        }
        // ... finally output the Name using the name type
        localrc = AttPackGetAttribute("Name", ESMC_ATTNEST_ON)->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement(nameType, "", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/ResponsibleParty, 'Name'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        if (role == "PI") {
            localrc = io_xml->writeComment(
                    "   to ESG: \"Principal Investigator\" under tabs "
                            "\"Properties/Basic\".", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else if (role == "contact") {
            localrc = io_xml->writeComment(
                    "   to ESG: \"Contact Name\" under tabs "
                            "\"Properties/Basic\".", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else if (role == "funder") {
            localrc = io_xml->writeComment(
                    "   to ESG: \"Funding Source\" under tabs "
                            "\"Properties/Basic\".", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } else {
            localrc = io_xml->writeComment(
                    "   to ESG: Ingested and stored for role code \"" + role +
                    "\", but not displayed yet.", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
        localrc = io_xml->writeEndElement(nameType, --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    if (AttPackGetAttribute("PhysicalAddress", ESMC_ATTNEST_ON)->isSet() ||
        AttPackGetAttribute("EmailAddress", ESMC_ATTNEST_ON)->isSet() ||
        AttPackGetAttribute("URL", ESMC_ATTNEST_ON)->isSet()) {

        // contactInfo header
        localrc = io_xml->writeStartElement("gmd:contactInfo", "", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeStartElement("gmd:CI_Contact", "", ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        if (AttPackGetAttribute("PhysicalAddress", ESMC_ATTNEST_ON)->isSet() ||
            AttPackGetAttribute("EmailAddress", ESMC_ATTNEST_ON)->isSet()) {

            // address header
            localrc = io_xml->writeStartElement("gmd:address", "", ++indent, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeStartElement("gmd:CI_Address", "", ++indent, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            attr = AttPackGetAttribute("PhysicalAddress", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeStartElement("gmd:deliveryPoint", "", ++indent, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/ResponsibleParty, 'PhysicalAddress'.", indent+1);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: Not ingested yet.", indent+1);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("gmd:deliveryPoint", --indent);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }

            attr = AttPackGetAttribute("EmailAddress", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeStartElement("gmd:electronicMailAddress", "", indent, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/ResponsibleParty, 'EmailAddress'.", indent+1);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                if (role == "contact") {
                    localrc = io_xml->writeComment(
                            "   to ESG: \"Contact Email\" under tabs \"Properties/Basic\".",
                            indent+1);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else if (role == "PI" || role == "author") {
                    localrc = io_xml->writeComment(
                            "   to ESG: Ingested and stored for role code \"" + role +
                            "\", but not displayed yet.", indent+1);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else {
                    localrc = io_xml->writeComment(
                            "   to ESG: Not ingested yet for role code \"" + role +
                            "\".", indent+1);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }
                localrc = io_xml->writeEndElement("gmd:electronicMailAddress", --indent);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }

            // address footer
            localrc = io_xml->writeEndElement("gmd:CI_Address", --indent);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeEndElement("gmd:address", --indent);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        } // end if PhysicalAddress or EmailAddress

        attr = AttPackGetAttribute("URL", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            value = valuevector.at(0);
            localrc = io_xml->writeStartElement("gmd:onlineResource", "", indent, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeStartElement("gmd:CI_OnlineResource", "", ++indent, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeStartElement("gmd:linkage", "", ++indent, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            localrc = io_xml->writeElement("gmd:URL", value, ++indent, 0);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeComment(
                    "from ESMF: CIM/ResponsibleParty, 'URL'.", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeComment(
                    "   to ESG: Not ingested yet.", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeEndElement("gmd:linkage", --indent);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeEndElement("gmd:CI_OnlineResource", --indent);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            localrc = io_xml->writeEndElement("gmd:onlineResource", --indent);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }

        // contact footer
        localrc = io_xml->writeEndElement("gmd:CI_Contact", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:contactInfo", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } // end if PhysicalAddress, EmailAddress or URL

    attr = AttPackGetAttribute("ResponsiblePartyRole", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        if (value != "PI") transform(value.begin(), value.end(),
                                     value.begin(), ::tolower);
        if (value == "center") value = "centre";
        localrc = io_xml->writeStartElement("gmd:role", "", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeElement("gmd:CI_RoleCode", "", ++indent, 2,
                                       "codeList", "",
                                       "codeListValue",
                                       value.c_str());
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/ResponsibleParty, 'ResponsiblePartyRole'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "   to ESG: Ingested, but only used to control display.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:role", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        ESMC_LogDefault.Write("Attribute ResponsiblePartyRole in "
                                      "standard attribute package (convention='ISO 19115', "
                                      "purpose='RespParty') "
                                      "required to be set, when other attributes in this package are set, "
                                      "to produce valid CIM XML output.",
                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }

    // use "Abbreviation" attribute if set ...
    attr = AttPackGetAttribute("Abbreviation", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeElement("abbreviation", value, indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/ResponsibleParty, 'Abbreviation'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "   to ESG: Not ingested yet.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else if ((attr = AttPackGetAttribute("Name", ESMC_ATTNEST_ON))->isSet()) {
        // ... otherwise get initials from "Name"
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        char s[2*ESMF_MAXSTR], abbr[ESMF_MAXSTR], *p;
        int i;
        strcpy(s, value.c_str());
        p = strtok(s, " ");
        for (i=0; p != NULL && i<ESMF_MAXSTR; i++) {
            abbr[i] = p[0];
            p = strtok(NULL, " ");
        }
        abbr[i] = '\0';
        localrc = io_xml->writeElement("abbreviation", abbr, indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/ResponsibleParty, abbreviation (initials) of 'Name'.",
                indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "   to ESG: Not ingested yet.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    // responsibleParty footer
    localrc = io_xml->writeEndElement("responsibleParty", --indent);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    return ESMF_SUCCESS;

} // end AttributeWriteCIMRP
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMCP"
//BOPI
// !IROUTINE:  AttributeWriteCIMCP - Write contents of a CIM {\tt Attribute} Component Properties Description package (General or Scientific) as <componentProperties><componentProperty> records within a component.
//
// !INTERFACE:
int Attribute::AttributeWriteCIMCP(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                //  in - io pointer to write
        const string &convention,      //  in - convention
        const string &purpose,         //  in - purpose (General or Scientific)
        int indent) const {            //  in - starting indent level
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    int localrc;
    Attribute *attpack = NULL;
    Attribute *attr = NULL;

    vector<string> valuevector;
    string value;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // Get the General or Scientific attpack, as specified by arg purpose
    string attPackInstanceName;
    attpack = AttPackGet(convention, purpose, "comp", attPackInstanceName,
                         ESMC_ATTNEST_ON);
    if(!attpack) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
                                      "Cannot find the specified Attribute package\n", ESMC_CONTEXT, &localrc);
        return localrc;
    }

    // output all attributes set in this package
    for(int i=0; i<attpack->attrList.size(); i++) {
        string name = attpack->attrList.at(i)->attrName;
        attr = attpack->AttPackGetAttribute(name, ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);

            // Custom, user-defined general component properties
            if (purpose.compare(COMP_PROP_PURP) == 0) {
                localrc = io_xml->writeStartElement("componentProperty", "", indent+1,
                                                    2, "type", "custom", "represented", "true");
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeElement("shortName", name, indent+2, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                for(int j=0; j<valuevector.size(); j++) {
                    localrc = io_xml->writeElement("value", valuevector.at(j), indent+2, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }

                localrc = io_xml->writeComment(
                        "from ESMF: CIM/GeneralComponentProperties, custom user-defined "
                                "attribute name '" + name + "'.", indent+3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: Not ingested yet.", indent+3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("componentProperty", indent+1);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                // Scientific properties
            } else {  // purpose = SCI_PROP_PURP
                localrc = io_xml->writeStartElement("componentProperty", "", indent+1,
                                                    1, "represented", "true");
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeElement("shortName", name, indent+2, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                for(int j=0; j<valuevector.size(); j++) {
                    localrc = io_xml->writeElement("value", valuevector.at(j), indent+2, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }

                localrc = io_xml->writeComment(
                        "from ESMF: CIM/ScientificProperties, Metafor-defined "
                                "attribute name '" + name + "'.", indent+3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: ESG-name mapped from Metafor-name, under tabs "
                                "\"Properties/Scientific\".", indent+3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("componentProperty", indent+1);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
        }
    }

    return ESMF_SUCCESS;

} // end AttributeWriteCIMCP
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMCPfield"
//BOPI
// !IROUTINE:  AttributeWriteCIMCPfield - Write contents of a CIM {\tt Attribute} Inputs package as <componentProperties><componentProperty> records within a component. (fields from all components in tree)
//
// !INTERFACE:
int Attribute::AttributeWriteCIMCPfield(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,      //  in - io pointer to write
        const string &convention,      //  in - convention
        int indent) const {  //  in - starting indent level
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    int localrc;
    Attribute *attpack = NULL;
    Attribute *attr = NULL;

    vector<string> valuevector;
    string value;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // write out all CIM field properties in this component tree
    for(int i=0; i<linkList.size(); i++) {
        // only consider objects within this component
        if (strcmp(linkList.at(i)->attrBase->ESMC_BaseGetClassName(),
                   "Component")==0) continue;

        // recurse until we reach field objects
        if (strcmp(linkList.at(i)->attrBase->ESMC_BaseGetClassName(),"Field")!=0) {
            localrc = linkList.at(i)->AttributeWriteCIMCPfield(io_xml, convention, indent);
            continue;
        }
        // found field object, now look for CIM/Inputs package
        for(int j=0; j<linkList.at(i)->packList.size(); j++) {
            attpack = linkList.at(i)->packList.at(j);
            if (!(attpack->attrConvention.compare(convention)==0 &&
                  attpack->attrPurpose.compare(INPUTS_PURP)==0 &&
                  attpack->attrObject.compare("field")==0)) {
                continue; // skip non-CIM fields and others
            }

            // skip if no ESMF/General, CF/Extended, or CF/general  attributes set
            if (attpack->AttPackGetAttribute("Intent", ESMC_ATTNEST_ON)->isSet() &&
                attpack->AttPackGetAttribute("ShortName", ESMC_ATTNEST_ON)->isSet() &&
                attpack->AttPackGetAttribute("LongName", ESMC_ATTNEST_ON)->isSet() &&
                attpack->AttPackGetAttribute("Units", ESMC_ATTNEST_ON)->isSet() &&
                attpack->AttPackGetAttribute("StandardName", ESMC_ATTNEST_ON)->isSet()) {
                continue;
            }

            // found CIM/Inputs package, now write its set attributes
            attr = attpack->AttPackGetAttribute("Intent", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                // map ESMF values {Export,Import} to CIM values {out,in}
                transform(value.begin(), value.end(), value.begin(), ::tolower);
                if (value == "export") {
                    value = "out";
                } else if (value == "import") {
                    value = "in";
                } else {
                    ESMC_LogDefault.Write("Attribute Intent in "
                                                  "standard attribute package (convention='CIM 1.5', "
                                                  "purpose='Inputs') must be one of "
                                                  "{Export, Import} to produce valid CIM XML output.",
                                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
                }
                localrc = io_xml->writeStartElement("componentProperty", "", indent+1,
                                                    2, "intent", value.c_str(),
                                                    "represented", "true");
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'Intent' (\"Export\"=\"out\", "
                                "\"Import=\"in\").", indent+2);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: Not ingested yet.", indent+2);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            } else {
                localrc = io_xml->writeStartElement("componentProperty", "", indent+1,
                                                    1, "represented", "true");
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
            attr = attpack->AttPackGetAttribute("ShortName", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeElement("shortName", value, indent+2, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'ShortName'.", indent+3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: Ingested below from "
                                "[simulationRun][input][connection][connectionTarget][dataSource]"
                                "[reference][name].", indent+3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            } else {
                ESMC_LogDefault.Write("Attribute ShortName in attpack "
                                              "CF/General, nested within std attpack (conv='CIM 1.5', "
                                              "purp='Inputs'), required to be set, if other "
                                              "attributes are set in nested packages CF/General, "
                                              "CF/Extended, or ESMF/General, to produce valid CIM XML output.",
                                      ESMC_LOGMSG_WARN, ESMC_CONTEXT);
            }
            attr = attpack->AttPackGetAttribute("LongName", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeElement("longName", value, indent+2, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'LongName'.", indent+3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: Not ingested yet.", indent+3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
            attr = attpack->AttPackGetAttribute("Units", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeStartElement("units", "", indent+2, 2,
                                                    "open", "true",
                                                    "value", value.c_str());
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'Units'.", indent+3);
                localrc = io_xml->writeComment(
                        "   to ESG: Not ingested yet.", indent+3);
                localrc = io_xml->writeStartElement("controlledVocabulary", "",
                                                    indent+3, 0);
                localrc = io_xml->writeElement("name", "units", indent+4, 0);
                localrc = io_xml->writeElement("server",
                                               "http://proj.badc.rl.ac.uk/svn/metafor/cmip5q/trunk",
                                               indent+4, 0);
                localrc = io_xml->writeEndElement("controlledVocabulary", indent+3);
                localrc = io_xml->writeEndElement("units", indent+2);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
            attr = attpack->AttPackGetAttribute("StandardName", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeStartElement("standardName", "", indent+2, 2,
                                                    "open", "true",
                                                    "value", value.c_str());
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'StandardName'.", indent+3);
                localrc = io_xml->writeComment(
                        "   to ESG: Not ingested yet.", indent+3);
                localrc = io_xml->writeStartElement("controlledVocabulary", "",
                                                    indent+3, 0);
                localrc = io_xml->writeElement("name", "cfName", indent+4, 0);
                localrc = io_xml->writeElement("server",
                                               "http://proj.badc.rl.ac.uk/svn/metafor/cmip5q/trunk",
                                               indent+4, 0);
                localrc = io_xml->writeEndElement("controlledVocabulary", indent+3);
                localrc = io_xml->writeEndElement("standardName", indent+2);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
            localrc = io_xml->writeEndElement("componentProperty", indent+1);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
    }

    return ESMF_SUCCESS;

} // end AttributeWriteCIMCPfield
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWritecitation"
//BOPI
// !IROUTINE:  AttributeWriteCIMcitation - Write contents of a CIM {\tt Attribute} package Citation record
//
// !INTERFACE:
int Attribute::AttributeWriteCIMcitation(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,      //  in - io pointer to write
        int indent) const {  //  in - starting indent level
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    int localrc;
    Attribute *attpack = NULL;
    Attribute *attr = NULL;
    bool inNestedAttPacks;

    vector<string> valuevector;
    string value;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // write out all nested citations
    for(int i=0; i<packList.size(); i++) {
        // RLO: added recursion to fix the false one-level recursion that worked before
        //    because the attpack nesting for standard attpacks was making flat trees
        localrc = packList.at(i)->AttributeWriteCIMcitation(io_xml, indent);
    }

    if (!(attrConvention.compare("ISO 19115")==0 &&
          attrPurpose.compare("Citation")==0))
        return ESMF_SUCCESS; // skip non-RPs

    // if no attributes set in this attpack instance, skip it ...
    if (!(AttPackIsSet(inNestedAttPacks=false))) return ESMF_SUCCESS;

    // citation header
    localrc = io_xml->writeStartElement("citation", "", indent, 0);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    attr = AttPackGetAttribute("ShortTitle", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("gmd:title", "", ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Citation, 'ShortTitle'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "   to ESG: Not ingested yet.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:title", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        ESMC_LogDefault.Write("Attribute ShortTitle in "
                                      "standard attribute package (convention='ISO 19115', "
                                      "purpose='Citation') "
                                      "required to be set, when other attributes in this package are set, "
                                      "to produce valid CIM XML output.",
                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }
    attr = AttPackGetAttribute("Date", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("gmd:date", "", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeStartElement("gmd:CI_Date", "", ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeStartElement("gmd:date", "", ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeElement("gco:Date", value, ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Citation, 'Date'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "   to ESG: Not ingested yet.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:date", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeStartElement("gmd:dateType", "", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeElement("gmd:CI_DateTypeCode", "",
                                       ++indent, 2,
                                       "codeList", "",
                                       "codeListValue", "");
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:dateType", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:CI_Date", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:date", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    } else {
        ESMC_LogDefault.Write("Attribute Date in "
                                      "standard attribute package (convention='ISO 19115', "
                                      "purpose='Citation') "
                                      "required to be set, when other attributes in this package are set, "
                                      "to produce valid CIM XML output.",
                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    }
    attr = AttPackGetAttribute("PresentationForm", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("gmd:presentationForm", "", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeElement("gmd:CI_PresentationFormCode", value,
                                       ++indent,
                                       2, "codeList", "",
                                       "codeListValue", "");
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Citation, 'PresentationForm'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "   to ESG: Not ingested yet.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:presentationForm", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    attr = AttPackGetAttribute("DOI", ESMC_ATTNEST_ON);
    if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("gmd:otherCitationDetails", "",
                                            indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Citation, 'DOI'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "   to ESG: \"Reference\" under tab \"References\", "
                        "appended to 'LongTitle'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:otherCitationDetails", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    if (AttPackGetAttribute("LongTitle", ESMC_ATTNEST_ON)->isSet() ||
        AttPackGetAttribute("URL", ESMC_ATTNEST_ON)->isSet()) {
        value.clear();
        attr = AttPackGetAttribute("LongTitle", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            value = valuevector.at(0);
        }
        // TODO:  CIM, as of 1.5/1.7, using ISO CI_Citation_Type, has no
        // definition for a citation URL.  Until CIM extends (subclasses)
        // CI_Citation_Type to add a place for citation URL, append it to
        // LongTitle.
        attr = AttPackGetAttribute("URL", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
            localrc = attr->get(&valuevector);
            if (valuevector.size() > 1) {
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;}
            // append URL to LongTitle for now TODO: separate when place for
            // citation URL is created in CIM
            value += (value.empty() ? "" : " ") + valuevector.at(0);
        }
        localrc = io_xml->writeStartElement("gmd:collectiveTitle", "", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeElement("gco:CharacterString", value, ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "from ESMF: CIM/Citation, 'LongTitle' + 'URL'.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeComment(
                "   to ESG: \"Reference\" under tab \"References\", "
                        "'DOI' appended.", indent+1);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("gmd:collectiveTitle", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }

    // citation footer
    localrc = io_xml->writeEndElement("citation", --indent);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

    return ESMF_SUCCESS;

} // end AttributeWriteCIMcitation
#if 0
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMcomposition"
//BOPI
// !IROUTINE:  AttributeWriteCIMcomposition - Write contents of a CIM {\tt Attribute} package composition node (fields from all components in tree)
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMcomposition(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml) const {      //  in - io pointer to write
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  char msgbuf[4*ESMF_MAXSTR];
  Attribute *attpack = NULL, *ap;
  Attribute *attr = NULL;
  bool inNestedAttPacks;

  vector<string> valuevector, value2vector;
  string value, value2;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // write out all CIM fields in component tree
  for(int i=0; i<linkList.size(); i++) {
    for(int j=0; j<linkList.at(i)->packList.size(); j++) {
      attpack = linkList.at(i)->packList.at(j);
      if (!(attpack->attrConvention.compare(convention)==0 &&
            attpack->attrPurpose.compare(INPUTS_PURP)==0 &&
            attpack->attrObject.compare("field")==0))
        continue; // skip non-CIM fields

      // if no attributes set in this attpack, skip it ...
      if (!(attpack->AttPackIsSet(inNestedAttPacks=false))) continue;

      // otherwise, write it out ...
      attr = attpack->AttPackGetAttribute("CouplingPurpose", ESMC_ATTNEST_ON);
      if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                        "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        // map ESMF values {Ancillary, Boundary, Initial} to CIM enum
        // values {ancillaryFile, boundaryCondition, initialCondition}
        transform(value.begin(), value.end(), value.begin(), ::tolower);
        if (value == "ancillary") {
          value = "ancillaryFile";
        } else if (value == "boundary") {
          value = "boundaryCondition";
        } else if (value == "initial") {
          value = "initialCondition";
        } else {
          ESMC_LogDefault.Write("Attribute CouplingPurpose in "
            "standard attribute package (convention='CIM 1.5', "
            "purpose='Inputs') must be one of "
            "{Ancillary, Boundary, Initial} "
            "to produce valid CIM XML output.",
            ESMC_LOGMSG_WARN, ESMC_CONTEXT);
        }
        localrc = io_xml->writeStartElement("coupling", "", 3, 2,
                     "fullySpecified", "false", "purpose", value.c_str());
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
      } else {
        // Output starting <coupling> element, to match ending element
        // </coupling>, but with a blank purpose="" attr. This will produce an
        // invalid CIM file, yet keep it well-formed XML.  Better than
        // outputting no <coupling></coupling> pair, which would produce far
        // more validation errors, confusing a user as to what the real
        // problem is -- that attribute CouplingPurpose is not set.
        localrc = io_xml->writeStartElement("coupling", "", 3, 2,
                     "fullySpecified", "false", "purpose", "");
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        ESMC_LogDefault.Write("Attribute CouplingPurpose in "
          "standard attribute package (convention='CIM 1.5', "
          "purpose='Inputs') "
          "required to be set, when other attributes in this package are set, "
          "to produce valid CIM XML output.",
          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
      }
      attr = attpack->AttPackGetAttribute("Frequency", ESMC_ATTNEST_ON);
      if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                      "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);

        // parse frequency value and units
        char s[2*ESMF_MAXSTR], empty[]="", *freq, *units;
        strcpy(s, value.c_str());
        freq = strtok(s, " ");
        units = strtok(NULL, " ");
        if (freq == NULL || units == NULL) {
          ESMC_LogDefault.Write("Attribute InputFrequency, in CIM/Inputs "
            "Description standard attribute package, must have both a time "
            "value and a units specification, e.g. '15 Minutes'.",
            ESMC_LOGMSG_WARN, ESMC_CONTEXT);
          // prevent Xerces crash upon null ptr exception throw (with F90 main)
          if (freq == NULL) freq = empty;
          if (units == NULL) units = empty;
        }
        // CIM enum: {seconds, minutes, hours, days, months, years,
        //                decades, centuries}
        value = units;
        transform(value.begin(), value.end(), value.begin(), ::tolower);
        if (value != "seconds" && value != "minutes" && value != "hours" &&
            value != "days" && value != "months" && value != "years" &&
            value != "decades" && value != "centuries") {
          ESMC_LogDefault.Write("Attribute InputFrequency, in CIM/Inputs "
            "Description standard attribute package, must have units as one of "
            "{Seconds, Minutes, Hours, Days, Months, Years, "
            "Decades, Centuries}, to produce valid CIM XML output.",
            ESMC_LOGMSG_WARN, ESMC_CONTEXT);
        }
        localrc = io_xml->writeStartElement("timeProfile", "", 4, 2,
                              "units", value.c_str(), "variableRate", "false");
        localrc = io_xml->writeElement("rate", freq, 5, 0);
        localrc = io_xml->writeEndElement("timeProfile", 4);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
      }
      if (attpack->AttPackGetAttribute("SpatialRegriddingMethod", ESMC_ATTNEST_ON)->isSet() ||
          attpack->AttPackGetAttribute("SpatialRegriddingDimension", ESMC_ATTNEST_ON)->isSet()) {

        attr = attpack->AttPackGetAttribute("Frequency", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
          localrc = attr->get(&valuevector);
          if (valuevector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                        "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
          value = valuevector.at(0);
          // CIM enum: {1D, 2D, 3D}
          transform(value.begin(), value.end(), value.begin(), ::toupper);
          if (value != "1D" && value != "2D" && value != "3D") {
            ESMC_LogDefault.Write("Attribute SpatialRegriddingDimension, in "
              "CIM/Inputs standard attribute package, must "
              "be one of {1D, 2D, 3D} to produce valid CIM XML output.",
              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
          }
          localrc = io_xml->writeStartElement("spatialRegridding", "", 4, 1,
                             "spatialRegriddingDimension", value.c_str());
        } else {
          // Output starting <spatialRegridding> element, to match ending
          // element </spatialRegridding>, but without a
          // spatialRegriddingDimension="" xml attribute.
          localrc = io_xml->writeStartElement("spatialRegridding", "", 4, 0);
        }
        attr = attpack->AttPackGetAttribute("SpatialRegriddingMethod", ESMC_ATTNEST_ON);
        if (attr->isSet()) {
          localrc = attr->get(&value2vector);
          if (value2vector.size() > 1) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                        "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
            return ESMF_FAILURE;}
          value2 = value2vector.at(0);
          // CIM enum: {linear, near-neighbour,
          //                cubic, conservative-first-order,
          //                conservative-second-order,
          //                conservative, non-conservative}
          transform(value2.begin(), value2.end(), value2.begin(), ::tolower);
          if (value2 == "near-neighbor") value2 = "near-neighbour";
          if (value2 != "linear" && value2 != "near-neighbour" &&
              value2 != "cubic" && value2 != "conservative-first-order" &&
              value2 != "conservative-second-order" &&
              value2 != "conservative" && value2 != "non-conservative") {
            ESMC_LogDefault.Write("Attribute SpatialRegriddingMethod, in "
              "CIM/Inputs standard attribute package, must be "
              "one of {Linear, Near-Neighbor, Cubic, "
              "Conservative-First-Order, Conservative-Second-Order, "
              "Conservative, Non-Conservative} to produce valid CIM "
              "XML output.",
              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
          }
          localrc = io_xml->writeElement("spatialRegriddingStandardMethod",
                                       value2.c_str(), 5, 0);
        }
        localrc = io_xml->writeEndElement("spatialRegridding", 4);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
      }
      attr = attpack->AttPackGetAttribute("TimeTransformationType", ESMC_ATTNEST_ON);
      if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                      "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("timeTransformation", "", 4, 0);
        localrc = io_xml->writeElement("mappingType", "", 5, 2,
                                       "open", "true",
                                       "value", value.c_str());
        localrc = io_xml->writeEndElement("timeTransformation", 4);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
      }
      attr = attpack->AttPackGetAttribute("CouplingSource", ESMC_ATTNEST_ON);
      if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                      "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("couplingSource", "", 4, 0);
        localrc = io_xml->writeStartElement("dataSource", "", 5, 0);
        localrc = io_xml->writeStartElement("reference", "", 6, 0);
        localrc = io_xml->writeElement("name", value, 7, 0);
        localrc = io_xml->writeEndElement("reference", 6);
        localrc = io_xml->writeEndElement("dataSource", 5);
        localrc = io_xml->writeEndElement("couplingSource", 4);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
      } else {
        ESMC_LogDefault.Write("Attribute CouplingSource in "
          "standard attribute package (convention='CIM 1.5', "
          "purpose='Inputs') "
          "required to be set, when other attributes in this package are set, "
          "to produce valid CIM XML output.",
          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
      }
      attr = attpack->AttPackGetAttribute("CouplingTarget", ESMC_ATTNEST_ON);
      if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                      "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("couplingTarget", "", 4, 0);
        localrc = io_xml->writeStartElement("dataSource", "", 5, 0);
        localrc = io_xml->writeStartElement("reference", "", 6, 0);
        localrc = io_xml->writeElement("name", value, 7, 0);
        localrc = io_xml->writeEndElement("reference", 6);
        localrc = io_xml->writeEndElement("dataSource", 5);
        localrc = io_xml->writeEndElement("couplingTarget", 4);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
      } else {
        ESMC_LogDefault.Write("Attribute CouplingTarget in "
          "standard attribute package (convention='CIM 1.5', "
          "purpose='Inputs') "
          "required to be set, when other attributes in this package are set, "
          "to produce valid CIM XML output.",
          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
      }
      attr = attpack->AttPackGetAttribute("ShortName", ESMC_ATTNEST_ON);
      if (attr->isSet()) {
        localrc = attr->get(&valuevector);
        if (valuevector.size() > 1) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
          return ESMF_FAILURE;}
        value = valuevector.at(0);
        localrc = io_xml->writeStartElement("connection", "", 4, 0);
        localrc = io_xml->writeStartElement("connectionTarget", "", 5, 0);
        localrc = io_xml->writeStartElement("dataSource", "", 6, 0);
        localrc = io_xml->writeStartElement("reference", "", 7, 0);
        localrc = io_xml->writeElement("name", value, 8, 0);
        localrc = io_xml->writeEndElement("reference", 7);
        localrc = io_xml->writeEndElement("dataSource", 6);
        localrc = io_xml->writeEndElement("connectionTarget", 5);
        localrc = io_xml->writeEndElement("connection", 4);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
      }
      localrc = io_xml->writeEndElement("coupling", 3);
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    }
    // recurse through ESMF objects
    localrc = linkList.at(i)->AttributeWriteCIMcomposition(io_xml);
  }

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMcomposition
#endif
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMinput"
//BOPI
// !IROUTINE:  AttributeWriteCIMinput - Write contents of a CIM {\tt Attribute} package input node (fields from all components in tree)
//
// !INTERFACE:
int Attribute::AttributeWriteCIMinput(
// // !RETURN VALUE: //    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        IO_XML *io_xml,                   //  in - io pointer to write
        const string &convention) const { //  in - convention
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

    int localrc;
    char msgbuf[4*ESMF_MAXSTR];
    Attribute *attpack = NULL, *ap;
    Attribute *attr = NULL;
    bool inNestedAttPacks;

    vector<string> valuevector, value2vector;
    string value, value2, couplingPurpose;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // write out all CIM fields in component tree
    for(int i=0; i<linkList.size(); i++) {
        for(int j=0; j<linkList.at(i)->packList.size(); j++) {
            attpack = linkList.at(i)->packList.at(j);
            if (!(attpack->attrConvention.compare(convention)==0 &&
                  attpack->attrPurpose.compare(INPUTS_PURP)==0 &&
                  attpack->attrObject.compare("field")==0))
                continue; // skip non-CIM fields

            // if no attributes set in this attpack, skip it ...
            if (!(attpack->AttPackIsSet(inNestedAttPacks=false))) continue;

            // otherwise, write it out ...
            attr = attpack->AttPackGetAttribute("CouplingPurpose", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                // map ESMF values {Ancillary, Boundary, Initial} to CIM enum
                // values {ancillaryFile, boundaryCondition, initialCondition}
                transform(value.begin(), value.end(), value.begin(), ::tolower);
                if (value == "ancillary") {
                    couplingPurpose = "ancillaryFile";
                } else if (value == "boundary") {
                    couplingPurpose = "boundaryCondition";
                } else if (value == "initial") {
                    couplingPurpose = "initialCondition";
                } else {
                    ESMC_LogDefault.Write("Attribute CouplingPurpose in "
                                                  "standard attribute package (convention='CIM 1.5', "
                                                  "purpose='Inputs') must be one of "
                                                  "{Ancillary, Boundary, Initial} "
                                                  "to produce valid CIM XML output.",
                                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
                }
                localrc = io_xml->writeStartElement("input", "", 2, 2,
                                                    "fullySpecified", "true", "purpose",
                                                    couplingPurpose.c_str());
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'CouplingPurpose' "
                                "(\"Ancillary\"=\"ancillaryFile\", "
                                "\"Boundary\"=\"boundaryCondition\", "
                                "\"Initial\"=\"initialCondition\")", 3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: Title of expandable bar under tab \"Inputs\".", 3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            } else {
                // Output starting <input> element, to match ending element
                // </input>, but with a blank purpose="" attr. This will produce an
                // invalid CIM file, yet keep it well-formed XML.  Better than
                // outputting no <input></input> pair, which would produce far
                // more validation errors, confusing a user as to what the real
                // problem is -- that attribute CouplingPurpose is not set.
                localrc = io_xml->writeStartElement("input", "", 2, 2,
                                                    "fullySpecified", "true", "purpose", "");
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                ESMC_LogDefault.Write("Attribute CouplingPurpose in "
                                              "standard attribute package (convention='CIM 1.5', "
                                              "purpose='Inputs') "
                                              "required to be set, when other attributes in this package are set, "
                                              "to produce valid CIM XML output.",
                                      ESMC_LOGMSG_WARN, ESMC_CONTEXT);
            }
            attr = attpack->AttPackGetAttribute("Description", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeElement("description", value, 3, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'Description'.", 4);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: Next to field name (after colon) under tab \"Inputs\".", 4);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }

            if (couplingPurpose == "ancillaryFile") {
                localrc = io_xml->writeStartElement("type", "", 3, 2,
                                                    "open", "true", "value", "File");
            } else {
                localrc = io_xml->writeStartElement("type", "", 3, 2,
                                                    "open", "true", "value", "Shared Memory");
            }
            localrc = io_xml->writeStartElement("controlledVocabulary","", 4, 0);
            localrc = io_xml->writeElement("name", "CouplingType", 5, 0);
            localrc = io_xml->writeElement("server",
                                           "http://proj.badc.rl.ac.uk/svn/metafor/cmip5q/trunk", 5, 0);
            localrc = io_xml->writeEndElement("controlledVocabulary", 4);
            localrc = io_xml->writeEndElement("type", 3);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            attr = attpack->AttPackGetAttribute("Frequency", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);

                // parse frequency value and units
                char s[2*ESMF_MAXSTR], empty[]="", *freq, *units;
                strcpy(s, value.c_str());
                freq = strtok(s, " ");
                units = strtok(NULL, " ");
                if (freq == NULL || units == NULL) {
                    ESMC_LogDefault.Write("Attribute InputFrequency, in CIM/Inputs "
                                                  "Description standard attribute package, must have both a time "
                                                  "value and a units specification, e.g. '15 Minutes'.",
                                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
                    // prevent Xerces crash upon null ptr exception throw (with F90 main)
                    if (freq == NULL) freq = empty;
                    if (units == NULL) units = empty;
                }
                // CIM enum: {seconds, minutes, hours, days, months, years,
                //                decades, centuries}
                value = units;
                transform(value.begin(), value.end(), value.begin(), ::tolower);
                if (value != "seconds" && value != "minutes" && value != "hours" &&
                    value != "days" && value != "months" && value != "years" &&
                    value != "decades" && value != "centuries") {
                    ESMC_LogDefault.Write("Attribute InputFrequency, in CIM/Inputs "
                                                  "Description standard attribute package, must have units as one of "
                                                  "{Seconds, Minutes, Hours, Days, Months, Years, "
                                                  "Decades, Centuries}, to produce valid CIM XML output.",
                                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
                }
                localrc = io_xml->writeStartElement("timeProfile", "", 3, 2,
                                                    "units", value.c_str(), "variableRate", "false");
                localrc = io_xml->writeElement("rate", freq, 4, 0);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'Frequency'.", 5);
                localrc = io_xml->writeComment(
                        "   to ESG: \"Input Frequency\" under tab \"Inputs\", "
                                "under field name.", 5);
                localrc = io_xml->writeEndElement("timeProfile", 3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
            if (attpack->AttPackGetAttribute("SpatialRegriddingMethod", ESMC_ATTNEST_ON)->isSet() ||
                attpack->AttPackGetAttribute("SpatialRegriddingDimension", ESMC_ATTNEST_ON)->isSet()) {

                attr = attpack->AttPackGetAttribute("SpatialRegriddingDimension", ESMC_ATTNEST_ON);
                if (attr->isSet()) {
                    localrc = attr->get(&valuevector);
                    if (valuevector.size() > 1) {
                        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                      "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                        return ESMF_FAILURE;}
                    value = valuevector.at(0);
                    // CIM enum: {1D, 2D, 3D}
                    transform(value.begin(), value.end(), value.begin(), ::toupper);
                    if (value != "1D" && value != "2D" && value != "3D") {
                        ESMC_LogDefault.Write("Attribute SpatialRegriddingDimension, in "
                                                      "CIM/Inputs standard attribute package, must "
                                                      "be one of {1D, 2D, 3D} to produce valid CIM XML output.",
                                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
                    }
                    localrc = io_xml->writeStartElement("spatialRegridding", "", 3, 1,
                                                        "spatialRegriddingDimension", value.c_str());
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeComment(
                            "from ESMF: CIM/Inputs, 'SpatialRegriddingDimension'.", 4);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeComment(
                            "   to ESG: Not ingested yet.", 4);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else {
                    // Output starting <spatialRegridding> element, to match ending
                    // element </spatialRegridding>, but without a
                    // spatialRegriddingDimension="" xml attribute.
                    localrc = io_xml->writeStartElement("spatialRegridding", "", 3, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }
                attr = attpack->AttPackGetAttribute("SpatialRegriddingMethod", ESMC_ATTNEST_ON);
                if (attr->isSet()) {
                    localrc = attr->get(&value2vector);
                    if (value2vector.size() > 1) {
                        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                      "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                        return ESMF_FAILURE;}
                    value2 = value2vector.at(0);
                    // CIM enum: {linear, near-neighbour,
                    //                cubic, conservative-first-order,
                    //                conservative-second-order,
                    //                conservative, non-conservative}
                    transform(value2.begin(), value2.end(), value2.begin(), ::tolower);
                    if (value2 == "near-neighbor") value2 = "near-neighbour";
                    if (value2 != "linear" && value2 != "near-neighbour" &&
                        value2 != "cubic" && value2 != "conservative-first-order" &&
                        value2 != "conservative-second-order" &&
                        value2 != "conservative" && value2 != "non-conservative") {
                        ESMC_LogDefault.Write("Attribute SpatialRegriddingMethod, in "
                                                      "CIM/Inputs standard attribute package, must be "
                                                      "one of {Linear, Near-Neighbor, Cubic, "
                                                      "Conservative-First-Order, Conservative-Second-Order, "
                                                      "Conservative, Non-Conservative} to produce valid CIM "
                                                      "XML output.",
                                              ESMC_LOGMSG_WARN, ESMC_CONTEXT);
                    }
                    localrc = io_xml->writeElement("spatialRegriddingStandardMethod",
                                                   value2.c_str(), 4, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeComment(
                            "from ESMF: CIM/Inputs, 'SpatialRegriddingMethod'.", 5);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                    localrc = io_xml->writeComment(
                            "   to ESG: \"Input Spatial Regridding Method\" "
                                    "under tab \"Inputs\", under field name.", 5);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }
                localrc = io_xml->writeEndElement("spatialRegridding", 3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
            attr = attpack->AttPackGetAttribute("TimeTransformationType", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeStartElement("timeTransformation", "", 3, 0);
                localrc = io_xml->writeElement("mappingType", "", 4, 2,
                                               "open", "true",
                                               "value", value.c_str());
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'TimeTransformationType'.", 5);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: \"Input Time Transformation Type\" "
                                "under tab \"Inputs\", under field name.", 5);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("timeTransformation", 3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
            attr = attpack->AttPackGetAttribute("CouplingSource", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);

                localrc = io_xml->writeStartElement("couplingSource", "", 3, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeStartElement("dataSource", "", 4, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeStartElement("reference", "", 5, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                // recursively search from top-level component for a
                //   component attpack that has a ShortName value that matches the
                // CouplingSource value, then output that component's GUID
                ap = writeRoot->AttPackGet(convention,
                                           MODEL_COMP_PURP, "comp",
                                           "ShortName", value);
                if (ap != NULL) {
                    localrc = io_xml->writeElement("id", ap->attrGUID, 6, 0);
                } else {
                    // TODO:  output value of CouplingSource
                    ESMC_LogDefault.Write("The value of attribute CouplingSource in "
                                                  "standard attribute package (convention='CIM 1.5', "
                                                  "purpose='Inputs') "
                                                  "does not correspond to the value of any ShortName "
                                                  "attribute within a component attribute package "
                                                  "(convention='CIM 1.5', "
                                                  "purpose='ModelComp'). "
                                                  "Skipping output of <couplingSource>...<id>.",
                                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
                }

                localrc = io_xml->writeElement("name", value, 6, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'CouplingSource'.", 7);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: \"Input Source Component\" "
                                "under tab \"Inputs\", under field name.", 7);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                if (couplingPurpose == "ancillaryFile") {
                    localrc = io_xml->writeElement("type", "dataObject", 6, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                } else {
                    localrc = io_xml->writeElement("type", "modelComponent", 6, 0);
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }
                localrc = io_xml->writeEndElement("reference", 5);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("dataSource", 4);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("couplingSource", 3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            } else {
                ESMC_LogDefault.Write("Attribute CouplingSource in "
                                              "standard attribute package (convention='CIM 1.5', "
                                              "purpose='Inputs') "
                                              "required to be set, when other attributes in this package are set, "
                                              "to produce valid CIM XML output.",
                                      ESMC_LOGMSG_WARN, ESMC_CONTEXT);
            }
            attr = attpack->AttPackGetAttribute("CouplingTarget", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeStartElement("couplingTarget", "", 3, 0);
                localrc = io_xml->writeStartElement("dataSource", "", 4, 0);
                localrc = io_xml->writeStartElement("reference", "", 5, 0);

                // recursively search from top-level component for a
                //   component attpack that has a ShortName value that matches the
                // CouplingTarget value, then output that component's GUID
                ap = writeRoot->AttPackGet(convention,
                                           MODEL_COMP_PURP, "comp",
                                           "ShortName", value);
                if (ap != NULL) {
                    localrc = io_xml->writeElement("id", ap->attrGUID, 6, 0);
                } else {
                    // TODO:  output value of CouplingTarget
                    ESMC_LogDefault.Write("The value of attribute CouplingTarget in "
                                                  "standard attribute package (convention='CIM 1.5', "
                                                  "purpose='Inputs') "
                                                  "does not correspond to the value of any ShortName "
                                                  "attribute within a component attribute package "
                                                  "(convention='CIM 1.5', "
                                                  "purpose='ModelComp'). "
                                                  "Skipping output of <couplingSource>...<id>.",
                                          ESMC_LOGMSG_WARN, ESMC_CONTEXT);
                }

                localrc = io_xml->writeElement("name", value, 6, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'CouplingTarget'.", 7);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: \"Input Target Component\" "
                                "under tab \"Inputs\", under field name.", 7);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeElement("type", "modelComponent", 6, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("reference", 5);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("dataSource", 4);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("couplingTarget", 3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            } else {
                ESMC_LogDefault.Write("Attribute CouplingTarget in "
                                              "standard attribute package (convention='CIM 1.5', "
                                              "purpose='Inputs') "
                                              "required to be set, when other attributes in this package are set, "
                                              "to produce valid CIM XML output.",
                                      ESMC_LOGMSG_WARN, ESMC_CONTEXT);
            }
            attr = attpack->AttPackGetAttribute("ShortName", ESMC_ATTNEST_ON);
            if (attr->isSet()) {
                localrc = attr->get(&valuevector);
                if (valuevector.size() > 1) {
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                                  "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &localrc);
                    return ESMF_FAILURE;}
                value = valuevector.at(0);
                localrc = io_xml->writeStartElement("connection", "", 3, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeStartElement("connectionTarget", "", 4, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeStartElement("dataSource", "", 5, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeStartElement("reference", "", 6, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeElement("name", value, 7, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "from ESMF: CIM/Inputs, 'ShortName'.", 8);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeComment(
                        "   to ESG: Field name under tab \"Inputs\".", 8);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeElement("type", "componentProperty", 7, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("reference", 6);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("dataSource", 5);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("connectionTarget", 4);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeEndElement("connection", 3);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
            localrc = io_xml->writeEndElement("input", 2);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        }
        // recurse through ESMF objects
        localrc = linkList.at(i)->AttributeWriteCIMinput(io_xml, convention);
    }

    return ESMF_SUCCESS;

} // end AttributeWriteCIMinput
#if 0
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMtraverse"
//BOPI
// !IROUTINE:  AttributeWriteCIMtraverse - Write contents of a CIM {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttributeWriteCIMtraverse(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      IO_XML *io_xml,   //  in - io pointer to write
      ESMC_CIMDocumentType cimDocType) const {
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  int localrc;
  unsigned int i;
  Attribute *attpack = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  int ordinal=1;
  attpack = AttPackGet(convention, MODEL_COMP_PURP, "comp", &ordinal,
                               ESMC_ATTNEST_ON);
  while (attpack != NULL) {
    localrc = attpack->AttributeWriteCIMbuffer(io_xml, cimDocType);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "AttributeWriteCIMtraverse failed AttributeWriteCIMbuffer", ESMC_CONTEXT, &localrc);
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
    return ESMF_FAILURE;
    }

    // get next occurence of this attpack, if any, on this component
    ordinal++;
    attpack = AttPackGet(convention, MODEL_COMP_PURP, "comp", &ordinal,
                                 ESMC_ATTNEST_ON);
  }

  // recurse across all linked ESMF objects (e.g. child components, states,
  // fieldBundles, fields, grids, arrays)
  for(i=0; i<linkList.size(); i++)
    localrc = linkList.at(i)->AttributeWriteCIMtraverse(io_xml, cimDocType);

  return ESMF_SUCCESS;

 } // end AttributeWriteCIMtraverse
#endif

} // namespace ESMCI
