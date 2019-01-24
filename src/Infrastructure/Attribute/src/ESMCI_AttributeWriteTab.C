// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMC_FILENAME "ESMCI_AttributeWriteTab.C"

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Attribute} methods declared
// in the companion file ESMCI_Attribute.h for writing tab-delimited files.
//
//-----------------------------------------------------------------------------
// associated class definition file and others
#include "ESMCI_Attribute.h"

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

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

#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteTab"
//BOPI
// !IROUTINE:  AttributeWriteTab - write Attributes in Tab delimited format
//
// !INTERFACE:
int Attribute::AttributeWriteTab(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        const string &convention,             //  in - convention
        const string &purpose,                //  in - purpose
        const string &object,                 //  in - object
        const string &varobj,                 //  in - variable object
        const string &basename) const{        //  in - basename
//
// !DESCRIPTION:
//    Write the contents on an {\tt Attribute} hierarchy in Tab delimited format.
//    Expected to be called internally.
//
//EOPI

    FILE* tab;
    char msgbuf[4*ESMF_MAXSTR];
    int localrc;
    int rows, columns, index;
    unsigned int i;
    int *attrLens;
    vector<string> attrNames;

    rows = 0; columns = 0; index = 0;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // Open a text file for writing
    sprintf(msgbuf,"%s.stdout",basename.c_str());
    if((tab=fopen(msgbuf,"w"))==NULL) {
        localrc = ESMF_FAILURE;
        sprintf(msgbuf,"Could not open the write file!");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                                      msgbuf, ESMC_CONTEXT, &localrc);
        return ESMF_FAILURE;
    }

    // determine the number of fields to write
    localrc = AttributeCountTree(convention, purpose, varobj, rows, columns);
    if (localrc != ESMF_SUCCESS) {
        sprintf(msgbuf, "AttributeWriteTab failed counting objects");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
        fclose(tab);
        return ESMF_FAILURE;
    }

    // allocate the integer array of length maxobjs
    attrLens = new int[columns];
    if (!attrLens) {
        sprintf(msgbuf, "AttributeWriteTab failed allocating attrLens");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf,  ESMC_CONTEXT, &localrc);
        fclose(tab);
        return ESMF_FAILURE;
    }
    for (i=0; i<columns; i++) attrLens[i] = 0;
    attrNames.reserve(columns);

    // make a function to recurse the tree, find the max lengths, and compare names
    localrc = AttributeCountTreeLens(convention, purpose, varobj, attrLens,
                                     attrNames);
    if (localrc != ESMF_SUCCESS) {
        sprintf(msgbuf, "AttributeWriteTab failed CountTreeLens");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
        delete [] attrLens;
        attrNames.clear();
        fclose(tab);
        return ESMF_FAILURE;
    }

    // write the header
    sprintf(msgbuf, "Name: %s\t  Convention: %s\t  Purpose: %s\t\r\n\n",
            basename.c_str(),convention.c_str(),purpose.c_str());
    fprintf(tab,"%s",msgbuf);
    for (i=0; i<columns; i++) {
        sprintf(msgbuf, "%-*s\t",attrLens[i],attrNames.at(i).c_str());
        fprintf(tab,"%s",msgbuf);
    }
    sprintf(msgbuf, "\r\n");
    fprintf(tab,"%s",msgbuf);

    localrc = AttributeWriteTabTraverse(tab,convention,purpose,index,columns,
                                        attrLens,attrNames);
    if (localrc != ESMF_SUCCESS) {
        sprintf(msgbuf, "Attribute failed recursing in WriteTab");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
        delete [] attrLens;
        attrNames.clear();
        fclose(tab);
        return ESMF_FAILURE;
    }

    // close the file
    delete [] attrLens;
    attrNames.clear();
    fclose(tab);

    return ESMF_SUCCESS;

} // end AttributeWriteTab
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteTabTraverse"
//BOPI
// !IROUTINE:  AttributeWriteTabTraverse - write Attributes in Tab delimited format
//                                             recursive function
//
// !INTERFACE:
int Attribute::AttributeWriteTabTraverse(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        FILE *tab,                                //  in - file to write
        const string &convention,                 //  in - convention
        const string &purpose,                    //  in - purpose
        int &index,                               //  in - counter
        const int &columns,                       //  in - columns
        int *attrLens,                            //  in - column widths
        const vector<string> &attrNames) const{   //  inout - column headings
//
// !DESCRIPTION:
//    Write the contents on an {\tt Attribute} hierarchy in Tab delimited format.
//
//EOPI

    char msgbuf[4*ESMF_MAXSTR];
    int localrc;
    unsigned int i;
    Attribute *attpack;

    attpack = NULL;

    // Initialize local return code
    localrc = ESMC_RC_NOT_IMPL;

    string attPackInstanceName;
    attpack = AttPackGet(convention, purpose, "field", attPackInstanceName,
                         ESMC_ATTNEST_ON);
    if (attpack) {
        localrc = attpack->AttributeWriteTabBuffer(tab,index,columns,attrLens,attrNames);
        if (localrc != ESMF_SUCCESS) {
            sprintf(msgbuf, "AttributeWriteTabTraverse failed AttributeWriteTabBuffer");
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
            fclose(tab);
            return ESMF_FAILURE;
        }
    }

    // Recurse the hierarchy
    for (i=0; i<linkList.size(); i++) {
        index = 0;
        localrc = linkList.at(i)->AttributeWriteTabTraverse(tab, convention,
                                                            purpose, index, columns, attrLens, attrNames);
    }

    return ESMF_SUCCESS;

} // end AttributeWriteTabTraverse
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteTabBuffer"
//BOPI
// !IROUTINE:  AttributeWriteTabBuffer - write Attributes in Tab delimited format
//                                             recursive function
//
// !INTERFACE:
int Attribute::AttributeWriteTabBuffer(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        FILE *tab,                               //  in - file to write
        int &index,                              //  in - index counter
        const int &columns,                      //  in - columns
        int *attrLens,                           //  in - integer array of attribute lengths
        const vector<string> &attrNames) const{  //  in - attribute names
//
// !DESCRIPTION:
//    Write the contents on an {\tt Attribute} hierarchy in Tab delimited format.
//    Expected to be called internally.
//
//EOPI

    char msgbuf[4*ESMF_MAXSTR];
    int localrc;
    int tlen;
    unsigned int i;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    for (i=0; i<attrList.size(); i++) {
        if(attrList.at(i)->attrName.compare(attrNames.at(index)) == 0 &&
           attrList.at(i)->attrObject.compare("field") == 0) {
            if (attrLens[index] < attrNames.at(index).size())
                tlen = attrNames.at(index).size()+2;
            else
                tlen = attrLens[index];
            if (attrList.at(i)->items == 0) {
                sprintf(msgbuf, "%-*s\t",tlen, " ");
                fprintf(tab,"%s",msgbuf);
            } else if (attrList.at(i)->items == 1) {
                if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
                    sprintf(msgbuf, "%-*d\t",tlen,attrList.at(i)->vip.at(0));
                else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8)
                    sprintf(msgbuf, "%-*lld\t",tlen,attrList.at(i)->vlp.at(0));
                else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4)
                    sprintf(msgbuf, "%-*f\t",tlen,attrList.at(i)->vfp.at(0));
                else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8)
                    sprintf(msgbuf, "%-*g\t",tlen,attrList.at(i)->vdp.at(0));
                else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
                    if (attrList.at(i)->vbp.at(0) == ESMF_TRUE)
                        sprintf(msgbuf, "%-*s\t",tlen,"true");
                    else if (attrList.at(i)->vbp.at(0) == ESMF_FALSE)
                        sprintf(msgbuf, "%-*s\t",tlen,"false");
                }
                else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
                    sprintf(msgbuf, "%-*s\t",tlen,attrList.at(i)->vcpp.at(0).c_str());
                else
                    sprintf(msgbuf, "%-*s\t",tlen,"N/A");
                fprintf(tab,"%s",msgbuf);
            }
            else if (attrList.at(i)->items > 1) {
                sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, &localrc);
                sprintf(msgbuf,"ITEMS>1");
                fprintf(tab,"%s",msgbuf);
            }
            ++index;
            if (index == columns) {
                sprintf(msgbuf, "\r\n");
                fprintf(tab,"%s",msgbuf);
            }
        }
    }

    for(i=0; i<packList.size(); ++i)
        localrc = packList.at(i)->AttributeWriteTabBuffer(tab,index,columns,
                                                          attrLens,attrNames);

    return ESMF_SUCCESS;

} // end AttributeWriteTabBuffer

} // namespace ESMCI
