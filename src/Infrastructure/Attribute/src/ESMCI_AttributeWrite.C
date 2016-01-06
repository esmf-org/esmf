// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMCI_AttributeWrite.C"

// Attribute method implementation (body) file

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Attribute} methods declared
// in the companion file ESMCI_Attribute.h
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

#include <sstream>
#include <cstring>
#include <cstdlib>
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
//-----------------------------------------------------------------------------
//
// This section includes all the Attribute routines
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// PUBLIC:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// READ ROUTINES:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeRead"
//BOPI
// !IROUTINE:  AttributeRead - read Attributes from XML file
//
// !INTERFACE:
      int Attribute::AttributeRead(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      int fileNameLen,              //  in - file name length
      const char* fileName,         //  in - file name
      int schemaFileNameLen,        //  in - schema file name length
      const char* schemaFileName) { //  in - schema file name

//
// !DESCRIPTION:
//    Read the contents of an XML file into an {\tt Attribute} hierarchy.
//    Expected to be called internally.
//
//EOPI

  // Initialize local return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMF_SUCCESS;

  // instantiate IO object; initialize with pointer to this Attribute node, to
  // place file-read attributes into.
  IO_XML *io_xml = ESMCI_IO_XMLCreate("", "", 
                                      (ESMCI::Attribute*)this, &localrc);
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  // read the XML file, placing contents into this Attribute node
  localrc = io_xml->read(string(fileName,fileNameLen), string(schemaFileName, schemaFileNameLen));
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  localrc = ESMCI_IO_XMLDestroy(&io_xml);
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc);
  if (localrc != ESMF_SUCCESS) rc = localrc;

  return rc;

 } // end AttributeRead

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// WRITE ROUTINES:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
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

