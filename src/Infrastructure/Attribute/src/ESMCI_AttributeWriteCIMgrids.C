// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMCI_AttributeWriteCIMgrids.C"

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Attribute} methods declared
// in the companion file ESMCI_Attribute.h for writing CIM grids compliant files.
//
//-----------------------------------------------------------------------------
// associated class definition file and others
#include "ESMCI_Attribute.h"

#include "ESMCI_Macros.h"
#include "ESMCI_IO_XML.h"
#include "ESMCI_Base.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Time.h"
#include "ESMCI_Grid.h"

#include <cstdlib>
#include <cstring>
#include <sstream>
#include <vector>
#include <algorithm>
#include "time.h"

using std::string;
using std::vector;
using std::ostringstream;
using std::transform;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

extern "C" {
// Prototypes of the Fortran interface functions.
// TODO: These should probably go in the Superstructure/AttributeAPI directory

void FTN_X(f_esmf_gridattgetinfoint)(ESMCI::Grid **grid, const char *name,
                                     int *value, int *rc,
                                     ESMCI_FortranStrLenArg nlen);
void FTN_X(f_esmf_gridattgetinfochar)(ESMCI::Grid **grid, const char *name,
                                      char *value, int *rc,
                                      ESMCI_FortranStrLenArg nlen,
                                      ESMCI_FortranStrLenArg vlen);
void FTN_X(f_esmf_gridattgetinfointlist)(ESMCI::Grid **grid, const char *name,
                                         int *valueList, int *len1,
                                         int *il_present, const char *inputList,
                                         int *lens, int *len2, int *rc,
                                         ESMCI_FortranStrLenArg nlen,
                                         ESMCI_FortranStrLenArg slen);
void FTN_X(f_esmf_gridattgetinfor8list)(ESMCI::Grid **grid, const char *name,
                                        double *valueList, int *len1,
                                        int *il_present, const char *inputList,
                                        int *lens, int *len2, int *rc,
                                        ESMCI_FortranStrLenArg nlen,
                                        ESMCI_FortranStrLenArg slen);
void FTN_X(f_esmf_gridattgetinfologicallist)(ESMCI::Grid **grid, const char *name,
                                             bool *valueList, int *len1,
                                             int *il_present, const char *inputList,
                                             int *len2, int *rc,
                                             ESMCI_FortranStrLenArg nlen);
}

namespace ESMCI {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMgrids"
//BOPI
// !IROUTINE:  AttributeWriteCIMgrids - {\tt Attribute} hierarchy traversal write
//
// !INTERFACE:
    int Attribute::AttributeWriteCIMgrids(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
            IO_XML *io_xml,           //  in - io pointer to write
            const string convention,
            const string gridGUID,          //  in - string for the gridGUID
            const int indent,                     //  in - starting indent
            const bool gridSolo) const{     //  in - bool to tell if this is only a grid
//
// !DESCRIPTION:
//    Write the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

        char msgbuf[4*ESMF_MAXSTR];
        int localrc;
        int index, local_indent;
        unsigned int i;
        Attribute *attpack;
        string helper1, helper2;

        index = 0;
        local_indent = indent;
        attpack = NULL;

        // Initialize local return code; assume routine not implemented
        localrc = ESMC_RC_NOT_IMPL;

        // write out the CIM grid properties in this component tree
        //  NOTE:  this only reaches the FIRST grid with the right attpack
        for(int i=0; i<linkList.size(); i++) {
            // only consider objects within this component
            /*if (strcmp(linkList.at(i)->attrBase->ESMC_BaseGetClassName(),
                "Component")==0) continue;*/

            // recurse until we reach the component grid
            if (strcmp(linkList.at(i)->attrBase->ESMC_BaseGetClassName(),"Grid")!=0) {
                localrc = linkList.at(i)->AttributeWriteCIMgrids(io_xml, convention,
                                                                 gridGUID, indent, gridSolo);
                continue;
            }
            // found grid object, now look for CIM/Inputs package
            for(int j=0; j<linkList.at(i)->packList.size(); j++) {
                attpack = linkList.at(i)->packList.at(j);
                if (!(attpack->attrConvention.compare(convention)==0 &&
                      attpack->attrPurpose.compare(GRIDS_PURP)==0 &&
                      attpack->attrObject.compare("grid")==0)) continue;

                // write the grid header
                if (gridSolo) {
                    local_indent = 0;
                    if (convention.compare(CIM_1_5_1_CONV)==0) {
                        helper1 = "http://www.purl.org/org/esmetadata/cim/1.5.1/schemas";
                        helper2 = "http://www.purl.org/org/esmetadata/cim/1.5.1/schemas/cim.xsd";
                    }
                    else if (convention.compare(CIM_1_7_1_CONV)==0) {
                        helper1 = "http://www.purl.org/org/esmetadata/cim/1.7.1/schemas";
                        helper2 = "http://www.purl.org/org/esmetadata/cim/1.7.1/schemas/cim.xsd";
                    }
                    localrc = io_xml->writeStartElement("gridSpec", "", local_indent, 8,
                                                        "xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance",
                                                        "xmlns:xlink", "http://www.w3.org/1999/xlink",
                                                        "xmlns:gml", "http://www.opengis.net/gml/3.2",
                                                        "xmlns:gco", "http://www.isotc211.org/2005/gco",
                                                        "xmlns:gmd", "http://www.isotc211.org/2005/gmd",
                                                        "xmlns", helper1.c_str(),
                                                        "xsi:schemaLocation", helper2.c_str(),
                                                        "gml:id", "ESMFCIMGrids");
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }
                else {
                    localrc = io_xml->writeStartElement("gridSpec", "", local_indent, 1,
                                                        "gml:id", "ESMFCIMGrids");
                    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                }

                // start the esmModelGrid
                localrc = io_xml->writeStartElement("esmModelGrid", "", ++local_indent, 4,
                                                    "id", attpack->AttributeGetInternalGridString("ESMF:name").c_str(),
                                                    "isLeaf", attpack->AttributeGetInternalGridString("isLeaf").c_str(),
                                                    "gridType", attpack->AttributeGetInternalGridString("gridType").c_str(),
                                                    "numTiles", attpack->AttributeGetInternalGridInt("ESMF:tileCount").c_str());
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                // write the shortname and longname
                localrc = io_xml->writeElement("shortName",
                                               (attpack->AttributeGetInternalGridString("ESMF:name")).c_str(), ++local_indent, 0);
                localrc = io_xml->writeElement("longName",
                                               (attpack->AttributeGetInternalGridString("longName")).c_str(), local_indent, 0);

                // start the gridTile
                localrc = io_xml->writeStartElement("gridTile", "", local_indent, 3,
                        // TODO: This information is retrieved incorrectly right now because
                        // there is no way to get grid tile numbers yet because multi-tile
                        // grids are not yet supported.  This could be done by asking the
                        // DistGrid for the deToTileMap, from which you can get a tile number
                        // but we will wait for proper multi-tile support anyway.
                                                    "id", attpack->AttributeGetInternalGridInt("ESMF:tileCount").c_str(),
                                                    "discretizationType", attpack->AttributeGetInternalGridString("discretizationType").c_str(),
                                                    "geometryType", attpack->AttributeGetInternalGridString("geometryType").c_str());
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                // start the simpleGridGeom
                localrc = io_xml->writeStartElement("simpleGridGeom", "", ++local_indent, 1,
                                                    "numDims", attpack->AttributeGetInternalGridInt("ESMF:dimCount").c_str());
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                // write coords
                // increase indentation outside of the for loop, so it's not done multiple times
                int nest_level = ++local_indent;
                for (int i=0;  i<attpack->attrList.size(); ++i) {
                    string value = attpack->attrList.at(i)->vcpp.at(0);
                    // if this is internal info, retrieve the correct Attribute
                    if (attpack->attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER &&
                        strcmp(value.c_str(), "ESMF:farrayPtr") == 0) {
                        // this is coordinate information, call internal routine and continue
                        AttributeWriteInternalInfoGrid(io_xml, nest_level, attpack->attrList.at(i));
                    }
                }

                // end the simpleGridGeom
                localrc = io_xml->writeEndElement("simpleGridGeom", --local_indent);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                // end the gridTile
                localrc = io_xml->writeEndElement("gridTile", --local_indent);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                // end the esmModelGrid
                localrc = io_xml->writeEndElement("esmModelGrid", --local_indent);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                // write the grid footer
                localrc = io_xml->writeElement("documentID", gridGUID,
                                               local_indent, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                // TODO: do something different here..
                // required by ES-DOC to properly index this fiel in the esmf test project
                localrc = io_xml->writeElement("documentProject", "ESMF test project",
                                               local_indent, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeElement("documentVersion",
                                               "1", local_indent, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
                localrc = io_xml->writeElement("documentCreationDate",
                                               getTime().c_str(), local_indent, 0);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

                localrc = io_xml->writeEndElement("gridSpec", --local_indent);
                ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
            }
        }

        return ESMF_SUCCESS;

    } // end AttributeWriteCIMgrids
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMgridref"
//BOPI
// !IROUTINE:  AttributeWriteCIMgridref -
//
// !INTERFACE:
    int Attribute::AttributeWriteCIMgridref(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
            IO_XML *io_xml,           //  in - io pointer to write
            int indent,               //  in - indentation
            const string gridGUID) const{   //  in - string for the gridGUID
//
// !DESCRIPTION:
//    Write the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

        int localrc;

        // Initialize local return code; assume routine not implemented
        localrc = ESMC_RC_NOT_IMPL;

        localrc = io_xml->writeStartElement("grid", "", indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeStartElement("reference", "", ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeElement("id", gridGUID, ++indent, 0);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        localrc = io_xml->writeEndElement("reference", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
        localrc = io_xml->writeEndElement("grid", --indent);
        ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

        return ESMF_SUCCESS;

    } // end AttributeWriteCIMgridref
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteCIMgridPresent"
//BOPI
// !IROUTINE:  AttributeWriteCIMgridPresent -
//
// !INTERFACE:
    bool Attribute::AttributeWriteCIMgridPresent(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
            const string convention) const{
//
// !DESCRIPTION:
//    Write the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

        int localrc;
        Attribute *attpack;
        bool gridPresent;
        gridPresent = false;

        // Initialize local return code; assume routine not implemented
        localrc = ESMC_RC_NOT_IMPL;

        //  NOTE:  this only reaches the FIRST grid with the right attpack

        // write out the CIM grid properties in this component tree
        for(int i=0; i<linkList.size(); i++) {
            // only consider objects within this component
            /*
            if (strcmp(linkList.at(i)->attrBase->ESMC_BaseGetClassName(),
                "Component")==0) {
              printf("Hit a component!\n");
              continue;
            }*/

            // recurse until we reach the component grid
            if (strcmp(linkList.at(i)->attrBase->ESMC_BaseGetClassName(),"Grid")!=0) {
                gridPresent = linkList.at(i)->AttributeWriteCIMgridPresent(convention);
                if (gridPresent) return gridPresent;
                else continue;
            }
            // found grid object, now look for CIM/Inputs package
            for(int j=0; j<linkList.at(i)->packList.size(); j++) {
                attpack = linkList.at(i)->packList.at(j);
                if (!(attpack->attrConvention.compare(convention)==0 &&
                      attpack->attrPurpose.compare(GRIDS_PURP)==0 &&
                      attpack->attrObject.compare("grid")==0)) continue;
                // found a grid with the correct attpack, return true
                return true;
            }
        }

        return gridPresent;

    } // end AttributeWriteCIMgridPresent
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteInternalInfoGrid"
//BOPI
// !IROUTINE:  AttributeWriteInternalInfoGrid - Write internal information to Grid CIM file
//
// !INTERFACE:
    int Attribute::AttributeWriteInternalInfoGrid(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
            IO_XML *io_xml, int nest_level, Attribute *attr) const {
//
// !DESCRIPTION:
//    Print the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI
//-----------------------------------------------------------------------------
        int localrc;
        char msgbuf[4*ESMF_MAXSTR];

        // initialize
        string name = attr->attrName;
        string value = attr->vcpp.at(0);
        ostringstream outstring;

        // strip the 'ESMF:' off of the value and set as name of Attribute to be retrieved
        string mod_name = value.substr(5,value.length());

        // cast the base back to a Grid ;)
        ESMCI::Grid *grid = reinterpret_cast<ESMCI::Grid *> (attr->attrBase);
        /* debugging
        if (grid) {
          ESMC_GridStatus_Flag gridstatus = grid->getStatus();
          printf("Attribute: %s\ngrid status = %d\n", mod_name.c_str(), gridstatus);
        }*/

        // TODO: hardcoded case statement for now, will have to use a config file in future
        // integer valued info
        if (mod_name == "dimCount" ||
            mod_name == "tileCount" ||
            mod_name == "staggerlocCount" ||
            mod_name == "localDECount" ||
            mod_name == "arbDim" ||
            mod_name == "rank" ||
            mod_name == "arbDimCount" ||
            mod_name == "arbIndexCount") {

            // initialize int return parameters
            int int_value = 0;
            // TODO: input parameters are unimplemented until there is a definite way to specify them

            // call into the glue layer to Fortran Attribute layer
            FTN_X(f_esmf_gridattgetinfoint)(&grid,
                                            mod_name.c_str(), &int_value,
                                            &localrc,
                                            mod_name.size());
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            // write the output value to the output stream and write to XML file
            outstring << int_value;
            localrc = io_xml->writeElement(name, outstring.str(), nest_level, 0);
        }
            // character string valued info
        else if (mod_name == "coordTypeKind" ||
                 mod_name == "indexflag" ||
                 mod_name == "status" ||
                 mod_name == "name") {

            // TODO: get rid of the fixed size buffer!
            // initialize char return parameters
            char char_value[ESMF_MAXSTR];
            int vlen = ESMF_MAXSTR;

            // call into the glue layer to Fortran Attribute layer
            FTN_X(f_esmf_gridattgetinfochar)(&grid,
                                             mod_name.c_str(),
                                             char_value, &localrc,
                                             mod_name.size(), vlen);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            // TODO: related to fixed buffer, convert to string and resize to remove cruft
            string char_string_value(char_value, vlen);
            char_string_value.resize(char_string_value.find_last_not_of(" ")+1);

            // write the output string to the output stream and write to XML file
            outstring << char_string_value;
            localrc = io_xml->writeElement(name, outstring.str(), nest_level, 0);
        }
            // list valued info
        else if (mod_name == "farrayPtr") {

            // strip the 'Input:' off of the input arguments and organize
            int lens_len = attr->items-1; // -1 because we dont' need the 'value'
            if (attr->items <= 0) {
                sprintf(msgbuf,"farrayPtr requires input arguments!");
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
                return ESMF_FAILURE;
            }

            string inputString = "";
            int *lens;
            lens = new int[lens_len];
            int slen = 0;
            int coordDim = 0;
            for (int j=0; j<lens_len; ++j) {
                string temp_string = attr->vcpp.at(j+1);
                if (strncmp(temp_string.c_str(), "Input:", 6) == 0) {
                    string temp_substr = temp_string.substr(6,temp_string.length());
                    inputString.append(temp_substr);
                    // test if substring == coordDim and get the number if so
                    if (strncmp(temp_substr.c_str(),"coordDim=", 9) == 0) {
                        string temp_numberstring = temp_substr.substr(9,temp_substr.length());
                        coordDim = atoi(temp_numberstring.c_str());
                    }
                    lens[j] = temp_substr.length();
                    slen = slen + lens[j];
                }
            }

            // first retrieve the dimCount and typekind of the coordinates
            string dimCount_name = "dimCount";
            int dimCount = 0;
            FTN_X(f_esmf_gridattgetinfoint)(&grid, dimCount_name.c_str(),
                                            &dimCount, &localrc,
                                            dimCount_name.size());
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            string cTK_name = "coordTypeKind";
            // TODO: remove fixed length buffer
            char coordTypeKind[ESMF_MAXSTR];
            int vlen = ESMF_MAXSTR;
            FTN_X(f_esmf_gridattgetinfochar)(&grid, cTK_name.c_str(),
                                             coordTypeKind, &localrc,
                                             cTK_name.size(), vlen);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            // TODO: related to fixed buffer, convert to string and resize to remove cruft
            string cTK_string(coordTypeKind, vlen);
            cTK_string.resize(cTK_string.find_last_not_of(" ")+1);

            // next retrieve the exclusiveCount so that we can allocate space for coordinates
            int il_present = 0;
            string exclusiveCount_name = "exclusiveCount";
            int *exclusiveCount;
            exclusiveCount = new int[dimCount];
            FTN_X(f_esmf_gridattgetinfointlist)(&grid, exclusiveCount_name.c_str(),
                                                exclusiveCount, &dimCount,
                                                &il_present, inputString.c_str(),
                                                lens, &lens_len, &localrc,
                                                exclusiveCount_name.size(), slen);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            // allocate space for the coordinates
            int num_coords = 1;
            for (int i = 0; i < dimCount; ++i)
                num_coords *= exclusiveCount[i];

            if (cTK_string != "ESMF_TYPEKIND_R8") {
                sprintf(msgbuf,"coordinates are only available in ESMF_TYPEKIND_R8 right now..");
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
                delete [] lens;
                delete [] exclusiveCount;
                return ESMF_FAILURE;
            }

            // now retrieve the coordinates
            il_present = 1;
            double *valueList;
            valueList = new double[num_coords];

            FTN_X(f_esmf_gridattgetinfor8list)(&grid,
                                               mod_name.c_str(),
                                               valueList, &num_coords,
                                               &il_present, const_cast<char *> (inputString.c_str()),
                                               lens, &lens_len, &localrc,
                                               mod_name.size(), slen);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            // write the output values to the output stream and write to XML file
            for (int i=0; i<num_coords; ++i)
                outstring << valueList[i] << " ";
            localrc = io_xml->writeElement(name, outstring.str(), nest_level, 0);
            delete [] valueList;
            delete [] lens;
            delete [] exclusiveCount;
        }

        return ESMF_SUCCESS;

    } // end AttributeWriteInternalInfoGrid
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetInternalGridInt"
//BOPI
// !IROUTINE:  AttributeGetInternalGridInt - retrieve internal information for Grid CIM file
//
// !INTERFACE:
    string Attribute::AttributeGetInternalGridInt(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
            string inputString) const {   //  in - io pointer to write
//
// !DESCRIPTION:
//    Return the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI
//-----------------------------------------------------------------------------
        int localrc;
        char msgbuf[4*ESMF_MAXSTR];

        // Initialize local return code; assume routine not implemented
        localrc = ESMC_RC_NOT_IMPL;

        // this is internal info
        if (strncmp(inputString.c_str(), "ESMF:", 5) == 0) {
            // strip the 'ESMF:' off of the value and set as name of Attribute to be retrieved
            string mod_name = inputString.substr(5,inputString.length());

            // cast the base back to a Grid ;)
            ESMCI::Grid *grid = reinterpret_cast<ESMCI::Grid *> (attrList.at(0)->attrBase);

            // initialize int return parameters
            int int_value = 0;
            // call into the glue layer to Fortran Attribute layer
            FTN_X(f_esmf_gridattgetinfoint)(&grid,
                                            mod_name.c_str(), &int_value,
                                            &localrc,
                                            mod_name.size());
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            // return the output value
            char char_value[10]; // larger than length of biggest possible integer
            //itoa(int_value, char_value, 10);
            sprintf(char_value, "%x", int_value);
            string string_value(char_value);
            return string_value;

            // this is not internal info
        } else {
            for (int i=0; i<attrList.size(); ++i) {
                Attribute *attr = attrList.at(i);
                string mod_name = attr->attrName;

                if (strcmp(mod_name.c_str(), inputString.c_str()) == 0) {
                    if (attr->tk == ESMC_TYPEKIND_I4 || attr->tk == ESMC_TYPEKIND_I8) {
                        int int_value = attr->vip.at(0);
                        char char_value[10]; // larger than length of biggest possible integer
                        //std::itoa(int_value, char_value, 10);
                        sprintf(char_value, "%x", int_value);
                        string string_value(char_value);
                        return string_value;
                    }
                }
            }
            return "N/A";
        }
    } // end AttributeGetInternalGridInt
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// character string valued info
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetInternalGridString"
//BOPI
// !IROUTINE:  AttributeGetInternalGridString - retrieve internal information for Grid CIM file
//
// !INTERFACE:
    string  Attribute::AttributeGetInternalGridString(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
            string inputString) const {   //  in - io pointer to write
//
// !DESCRIPTION:
//    Return the contents of a CIM {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI
//-----------------------------------------------------------------------------
        int localrc;
        char msgbuf[4*ESMF_MAXSTR];

        // Initialize local return code; assume routine not implemented
        localrc = ESMC_RC_NOT_IMPL;

        // this is internal info
        if (strncmp(inputString.c_str(), "ESMF:", 5) == 0) {
            // strip the 'ESMF:' off of the value and set as name of Attribute to be retrieved
            string mod_name = inputString.substr(5,inputString.length());

            // cast the base back to a Grid ;)
            ESMCI::Grid *grid = reinterpret_cast<ESMCI::Grid *> (attrList.at(0)->attrBase);

            // TODO: get rid of the fixed size buffer!
            // initialize char return parameters
            char char_value[ESMF_MAXSTR];
            int vlen = ESMF_MAXSTR;
            // call into the glue layer to Fortran Attribute layer
            FTN_X(f_esmf_gridattgetinfochar)(&grid,
                                             mod_name.c_str(),
                                             char_value,
                                             &localrc, mod_name.size(), vlen);
            ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);

            // TODO: related to fixed buffer, convert to string and resize to remove cruft
            string char_string_value(char_value, vlen);
            char_string_value.resize(char_string_value.find_last_not_of(" ")+1);

            // return the output value
            return char_string_value;

            // this is not internal info
        } else {
            for (int i=0; i<attrList.size(); ++i) {
                Attribute *attr = attrList.at(i);
                string mod_name = attr->attrName;

                if (strcmp(mod_name.c_str(), inputString.c_str()) == 0) {
                    string value = attr->vcpp.at(0);
                    return value;
                }
            }
            return "N/A";
        }
    } // end AttributeGetInternalGridString
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "getTime"
//BOPI
// !IROUTINE:  getTime - retrieve time in CF1.6 format
//
// !INTERFACE:
    const string Attribute::getTime(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
    ) const {
//
// !DESCRIPTION:
//
//
//EOPI
//-----------------------------------------------------------------------------

// Convert #1 to #2

// #1: Thu Nov 15 08:19:14 2012
// #2: 2012-09-25T09:54:30
//     012345678901234567890123

        string newstr;

        time_t rawtime;
        time(&rawtime);

        string timestr(ctime (&rawtime));

        char day[3];
        strftime(day, 3, "%d",localtime(&rawtime));
        string sday(day);

        newstr.resize(19);
        newstr.insert(0, timestr.substr(20,4));
        newstr.insert(4, "-");
        newstr.insert(5, month2Num(timestr.substr(4,3)).c_str());
        newstr.insert(7, "-");
        //newstr.insert(8, timestr.substr(8,2));
        newstr.insert(8, sday.substr(0,2));
        newstr.insert(10, "T");
        newstr.insert(11, timestr.substr(11,8));

        return newstr;

    } // end getTime
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "month2Num"
//BOPI
// !IROUTINE:  month2Num - return number associated with month
//
// !INTERFACE:
    string Attribute::month2Num(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
            string month) const {
//
// !DESCRIPTION:
//
//
//EOPI
//-----------------------------------------------------------------------------
        int localrc;
        string monthnum;

        // Initialize local return code; assume routine not implemented
        localrc = ESMC_RC_NOT_IMPL;

#if 0
        // initialize
  monthnum = "00";
#endif

        // set monthnum to valid value
        if (strcmp(month.c_str(), "Jan") == 0)
            monthnum = "01";
        else if (strcmp(month.c_str(), "Feb") == 0)
            monthnum = "02";
        else if (strcmp(month.c_str(), "Mar") == 0)
            monthnum = "03";
        else if (strcmp(month.c_str(), "Apr") == 0)
            monthnum = "04";
        else if (strcmp(month.c_str(), "May") == 0)
            monthnum = "05";
        else if (strcmp(month.c_str(), "Jun") == 0)
            monthnum = "06";
        else if (strcmp(month.c_str(), "Jul") == 0)
            monthnum = "07";
        else if (strcmp(month.c_str(), "Aug") == 0)
            monthnum = "08";
        else if (strcmp(month.c_str(), "Sep") == 0)
            monthnum = "09";
        else if (strcmp(month.c_str(), "Oct") == 0)
            monthnum = "10";
        else if (strcmp(month.c_str(), "Nov") == 0)
            monthnum = "11";
        else if (strcmp(month.c_str(), "Dec") == 0)
            monthnum = "12";
        else {
            monthnum = "00";
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                          "month string does not match a valid value, ", ESMC_CONTEXT, &localrc);
        }

        return monthnum;

    } // end month2Num
//-----------------------------------------------------------------------------

} // namespace ESMCI
