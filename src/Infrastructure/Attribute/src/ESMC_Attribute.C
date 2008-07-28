// $Id: ESMC_Attribute.C,v 1.16 2008/07/28 15:21:08 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMC_Attribute.C"

// ESMC_Attribute method implementation (body) file

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC_Attribute} methods declared
// in the companion file ESMC_Attribute.h
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include "ESMC_Attribute.h"
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Attribute.C,v 1.16 2008/07/28 15:21:08 rokuingh Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the ESMC_Attribute routines
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PRIVATE:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Attribute"
//BOPI
// !IROUTINE:  ESMC_Attribute - empty private copy constructor
//
// !INTERFACE:
      ESMC_Attribute::ESMC_Attribute(
//
// !ARGUMENTS:
      const ESMC_Attribute&) {
// 
// !RETURN VALUE:
//    {\tt ESMC_Attribute} object.
// 
// !DESCRIPTION:
//    Empty private copy constructor.
//
//EOPI

}  // end ESMC_Attribute

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PUBLIC:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttPackCreate"
//BOPI
// !IROUTINE:  ESMC_AttPackCreate() - create an attpack and add an {\tt ESMC_Attribute}
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttPackCreate(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,             // in - Attribute name
      char *convention,       // in - Attribute convention
      char *purpose,          // in - Attribute purpose
      char *object) {         // in - Attribute object type
// 
// !DESCRIPTION:
//     Setup the name, convention and purpose of an attpack and add
//     an {\tt ESMC_Attribute} with a specified name but no value.
//
//EOPI

  int localrc;
  char attpackname[ESMF_MAXSTR];
  ESMC_Attribute *attr;
  ESMC_Attribute *attpack;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!purpose) || (purpose[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute purpose", &localrc);
       return ESMF_FAILURE;
  }
  
  // simple sanity checks
  if ((!convention) || (convention[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute convention", &localrc);
       return ESMF_FAILURE;
  }
  
  // simple sanity checks
  if ((!object) || (object[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute object", &localrc);
       return ESMF_FAILURE;
  }

  // Search for the attpack, make it if not found
  attpack = ESMC_AttPackGet(convention, purpose, object);
  if(!attpack) {
    sprintf(attpackname,"Attribute Package %s %s %s %d",object,purpose,
      convention,attrCount);
    attpack = new ESMC_Attribute(attpackname, convention, purpose, object);
    if (!attpack) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed initializing an attpack", &localrc);
      return ESMF_FAILURE;
    }
    localrc = ESMC_AttributeSet(attpack);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed adding an attpack to an Attribute", &localrc);
      return ESMF_FAILURE;
    }
  }
  
  // make an Attribute in the attpack
  attr = new ESMC_Attribute(name, convention, purpose, object);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed initialized an attpack Attribute", &localrc);
    return ESMF_FAILURE;
  }
  
  // add an Attribute to the attpack
  localrc = attpack->ESMC_AttributeSet(attr);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed adding an attpack Attribute", &localrc);
    return ESMF_FAILURE;
  }
  
  return ESMF_SUCCESS;

}  // end ESMC_AttPackCreate()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttPackGet"
//BOPI
// !IROUTINE:  ESMC_AttPackGet - get an attpack on an {\tt ESMC_Attribute}
//
// !INTERFACE:
      ESMC_Attribute *ESMC_Attribute::ESMC_AttPackGet(
// 
// !RETURN VALUE:
//    {\tt ESMC_Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      char *convention,             // in - Attribute convention to retrieve
      char *purpose,                // in - Attribute purpose to retrieve
      char *object) const {         // in - Attribute object type to retrieve
// !DESCRIPTION:
//    Get an attpack on an {\tt ESMC_Attribute} given it's convention, 
//    purpose, and object type.
//
//EOPI

  // simple sanity checks
  if ((!purpose) || (purpose[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute purpose", NULL);
       return NULL;
  }
  
  // simple sanity checks
  if ((!convention) || (convention[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute convention", NULL);
       return NULL;
  }
  
  // simple sanity checks
  if ((!object) || (object[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute object", NULL);
       return NULL;
  }

  // look for the attpack on this Attribute
  for (int i=0; i<attrCount; i++) {
      if (strcmp(convention, attrList[i]->attrConvention) == 0 && 
          strcmp(purpose, attrList[i]->attrPurpose) == 0 &&
          strcmp(object, attrList[i]->attrObject) == 0 &&
          attrList[i]->attrPack == ESMF_TRUE) {
          return attrList[i];
          }
  }
 
  // if you got here, you did not find the attpack
  return NULL;

}  // end ESMC_AttPackGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttPackGetAttribute"
//BOPI
// !IROUTINE:  ESMC_AttPackGetAttribute - get an {\tt ESMC_Attribute} from an attpack
//
// !INTERFACE:
      ESMC_Attribute *ESMC_Attribute::ESMC_AttPackGetAttribute(
// 
// !RETURN VALUE:
//    {\tt ESMC_Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      char *name,                   // in - Attribute name to retrieve
      char *convention,             // in - Attribute convention to retrieve
      char *purpose,                // in - Attribute purpose to retrieve
      char *object) const {         // in - Attribute object type to retrieve)
// 
// !DESCRIPTION:
//     Get an {\tt ESMC_Attribute} from an attpack given its name, convention, 
//     purpose, and object type.
//
//EOPI

  int i;
  int attCount;
  ESMC_Attribute *attr;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute name", NULL);
       return NULL;
  }

  // simple sanity checks
  if ((!purpose) || (purpose[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute purpose", NULL);
       return NULL;
  }
  
  // simple sanity checks
  if ((!convention) || (convention[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute convention", NULL);
       return NULL;
  }
  
  // simple sanity checks
  if ((!object) || (object[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute object", NULL);
       return NULL;
  }

  // look for the Attribute on this attpack
  for (i=0; i<attrCount; i++) {
      if (strcmp(name, attrList[i]->attrName) == 0 && 
          strcmp(convention, attrList[i]->attrConvention) == 0 &&
          strcmp(purpose, attrList[i]->attrPurpose) == 0 &&
          strcmp(object, attrList[i]->attrObject) == 0) {

      // if you get here, you found a match. 
      return attrList[i]; 
      }   
  }
  
  // you get here if no matches found
  return NULL;

}  // end ESMC_AttPackGetAttribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttPackSet"
//BOPI
// !IROUTINE:  ESMC_AttPackSet() - set an {\tt ESMC_Attribute} in an attpack
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttPackSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,             // in - Attribute name
      ESMC_TypeKind tk,       // in - typekind
      int count,              // in - item count
      void *value,            // in - Attribute value
      char *convention,       // in - attpack convention
      char *purpose,          // in - attpack purpose
      char *object) {         // in - attpack object type
// 
// !DESCRIPTION:
//     Set the value for an {\tt ESMC_Attribute} belonging to an attpack with  
//     convention, purpose, and object type.
//
//EOPI

  int localrc;
  char msgbuf[ESMF_MAXSTR];
  ESMC_Attribute *attr;
  ESMC_Attribute *attpack;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Find the attpack Attribute
  attpack = ESMC_AttPackGet(convention, purpose, object);
  if(!attpack) {
       sprintf(msgbuf, "Cannot find an Attribute package with:\nconvention = '%s'\npurpose = '%s'\nobject = '%s'\n",
                      convention, purpose, object);
       printf(msgbuf);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
       return localrc;
  }
  
  attr = attpack->ESMC_AttPackGetAttribute(name, convention, purpose, object);
  if (!attr) {
       sprintf(msgbuf, "This Attribute package does have an Attribute named '%s'\n", name);
       printf(msgbuf);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
       return localrc;
  }

  // Set the Attribute
  localrc = attr->ESMC_AttrModifyValue(tk, count, value);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed modifying an attpack Attribute", &localrc);
    return ESMF_FAILURE;
  }
  
  // return
  if (localrc != ESMF_SUCCESS) return ESMF_FAILURE;
  return ESMF_SUCCESS;
  
}  // end ESMC_AttPackSet()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeWriteTab"
//BOPI
// !IROUTINE:  ESMC_AttributeWriteTab - write Attributes in Tab delimited format
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeWriteTab(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *convention,             //  in - convention
      char *purpose,                //  in - purpose
      char *object,                 //  in - object
      char *varobj,                 //  in - variable object
      char *basename,               //  in - basename
      int &count) const{            //  in - count  
//
// !DESCRIPTION:
//    Write the contents on an {\tt ESMC_Attribute} hierarchy in Tab delimited format.  
//    Expected to be called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc;
  int slen,llen,ulen,ilen,elen,tlen;
  
  slen = 8;
  llen = 30;
  ulen = 8;
  ilen = 10;
  elen = 10;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  if (count == 0) {
    sprintf(msgbuf, "Name: %s\t  Convention: %s\t  Purpose: %s\t\r\n\n",
      basename,convention,purpose);
    printf(msgbuf);
    ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    sprintf(msgbuf, "%-*s\t%-*s\t%-*s\t%-*s\t%-*s\t\r\n",slen,"Short Name",llen,"Long Name",ulen,
      "Units",ilen,"Import",elen,"Export");
    printf(msgbuf);
    ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
    ++count;
  }

  if (strcmp(convention,attrConvention) == 0 && 
      strcmp(purpose,attrPurpose) == 0 &&
      strcmp(varobj,attrObject) == 0) {
      if(strcmp("shortname",attrName) == 0)
        tlen = slen;
      else if(strcmp("longname",attrName) == 0)
        tlen = llen;
      else if(strcmp("units",attrName) == 0)
        tlen = ulen;
      else if(strcmp("import",attrName) == 0)
        tlen = ilen;
      else if(strcmp("export",attrName) == 0)
        tlen = elen;
      if (items == 1) {
        if (tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "%-*d\t",tlen,vi);
        else if (tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "%-*ld\t",tlen,vtl); 
        else if (tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "%-*f\t",tlen,vf);  
        else if (tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "%-*g\t",tlen,vd);  
        else if (tk == ESMC_TYPEKIND_LOGICAL) 
          sprintf(msgbuf, "%-*s\t",tlen,ESMC_LogicalString(vb));
        else if (tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "%-*s\t",tlen,vcp);
        else {
          sprintf(msgbuf, "%-*s\t",tlen,"N/A");
        }
        printf(msgbuf);
        ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      }
      else { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      }
        if (strcmp("export",attrName) == 0) {
          sprintf(msgbuf, "\r\n");
          printf(msgbuf);
          ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
        }
  }
  for(int i=0;  i<attrCount; i++) {
      attrList[i]->ESMC_AttributeWriteTab(convention,purpose,object,varobj,basename,count);
  }

/*
  attpack = ESMC_AttPackGet(convention, purpose, object);
  if (!attpack) {
       sprintf(msgbuf, "  Cannot find an Attribute package on this object with:\nconvention = '%s'\npurpose = '%s'\nobject = '%s'\n",
                      convention, purpose, object);
       printf(msgbuf);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
       return localrc;
  }

  for (int i=0; i<attpack->attrCount; i++) {
      sprintf(msgbuf, "   Attr %d:\n", i);
      printf(msgbuf);
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      attpack->attrList[i]->ESMC_Print();
  }
*/
  
  return ESMF_SUCCESS;

 } // end ESMC_AttributeWriteTab
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeWriteXML"
//BOPI
// !IROUTINE:  ESMC_AttributeWriteXML - Write contents of an {\tt ESMC_Attribute} package
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeWriteXML(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *convention,        //  in - convention
      char *purpose,           //  in - purpose
      char *object,            //  in - object
      char *varobj,            //  in - variable object
      char *basename) const{        //  in - basename
//
// !DESCRIPTION:
//    Print the contents of an {\tt ESMC_Attribute}.  Expected to be
//    called internally.
//
//EOPI

  FILE* xml;
  char msgbuf[ESMF_MAXSTR];
  int localrc;
  int stop = 0;
  int fldcount = 0;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Open an XML file for writing
  sprintf(msgbuf,"%s.xml",basename);
  if((xml=fopen(msgbuf,"w"))==NULL) {
    localrc = ESMF_FAILURE;
    sprintf(msgbuf,"Could not open the xml file!");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
    return ESMF_FAILURE;
  } 

  // Write the XML file header
  sprintf(msgbuf,"<model_component name=\"%s\"\n",basename);
  printf(msgbuf);
  fprintf(xml,msgbuf);
  sprintf(msgbuf,"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
  printf(msgbuf);
  fprintf(xml,msgbuf);
  sprintf(msgbuf,"xsi:schemaLocation=\"http://www.esmf.ucar.edu file:/esmf_model_component.xsd\"\n");
  printf(msgbuf);
  fprintf(xml,msgbuf);
  sprintf(msgbuf,"xmlns=\"http://www.esmf.ucar.edu\">\n\n");
  printf(msgbuf);
  fprintf(xml,msgbuf);
  
  // determine the number of fields to write
  localrc = ESMC_AttributeCountTree(convention, purpose, varobj, stop);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "ESMC_Attribute failed counting fields", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }

  // recurse the Attribute hierarchy
  localrc = ESMC_AttributeWriteXMLrecurse(xml,convention,purpose,object,varobj,stop,fldcount);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "ESMC_Attribute failed recursing in WriteTab", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }

  // close the file
  fclose(xml);

  return ESMF_SUCCESS;

 } // end ESMC_AttributeWriteXML
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeWriteXMLrecurse"
//BOPI
// !IROUTINE:  ESMC_AttributeWriteXMLrecurse - {\tt ESMC_Attribute} hierarchy recurse write
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeWriteXMLrecurse(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      FILE *xml,               //  in - file pointer to write
      char *convention,        //  in - convention
      char *purpose,           //  in - purpose
      char *object,            //  in - object
      char *varobj,            //  in - variable object
      int stop,                //  in - stop case            
      int &fldcount) const{    //  in - field count
//
// !DESCRIPTION:
//    Write the contents of an {\tt ESMC_Attribute}.  Expected to be
//    called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // write the component level information
  if (strcmp(convention,attrConvention) == 0 && 
      strcmp(purpose,attrPurpose) == 0 &&
      strcmp(object,attrObject) == 0 &&
      attrPack == ESMF_TRUE) {
    for (unsigned int i=0;  i<attrCount; i++) { 
      sprintf(msgbuf,"<%s_set>\n",attrList[i]->attrName);
      printf(msgbuf);
      fprintf(xml,msgbuf);
      if (attrList[i]->items == 1) {
        if (attrList[i]->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "  <%s name=\"%d\" />\n",attrList[i]->attrName,attrList[i]->vi);
        else if (attrList[i]->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "  <%s name=\"%ld\" />\n",attrList[i]->attrName,attrList[i]->vtl); 
        else if (attrList[i]->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "  <%s name=\"%f\" />\n",attrList[i]->attrName,attrList[i]->vf);  
        else if (attrList[i]->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "  <%s name=\"%g\" />\n",attrList[i]->attrName,attrList[i]->vd);  
        else if (attrList[i]->tk == ESMC_TYPEKIND_LOGICAL) 
          sprintf(msgbuf, "  <%s name=\"%s\" />\n",attrList[i]->attrName,ESMC_LogicalString(attrList[i]->vb));
        else if (attrList[i]->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "  <%s name=\"%s\" />\n",attrList[i]->attrName,attrList[i]->vcp);
        else {
          sprintf(msgbuf, "  <%s name=\"%s\" />\n",attrList[i]->attrName,"N/A");
        }
      }
      else { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      }
      printf(msgbuf);
      fprintf(xml,msgbuf);
      sprintf(msgbuf,"</%s_set>\n\n",attrList[i]->attrName);
      printf(msgbuf);
      fprintf(xml,msgbuf);
    }
  }
  
  // write the field level information
  if (strcmp(convention,attrConvention) == 0 && 
      strcmp(purpose,attrPurpose) == 0 &&
      strcmp(varobj,attrObject) == 0 &&
      attrPack == ESMF_TRUE) {
    if (fldcount == 0) {
      sprintf(msgbuf,"<variable_set>\n");
      printf(msgbuf);
      fprintf(xml,msgbuf);
    }
    for (unsigned int i=0;  i<attrCount; i++) { 
      if (attrList[i]->items == 1) {
        if (i == 0) {
          sprintf(msgbuf,"  <variable ");
          printf(msgbuf);
          fprintf(xml,msgbuf);
        }
        if (attrList[i]->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "%s=\"%d\" ",attrList[i]->attrName,attrList[i]->vi);
        else if (attrList[i]->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "%s=\"%ld\" ",attrList[i]->attrName,attrList[i]->vtl); 
        else if (attrList[i]->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "%s=\"%f\" ",attrList[i]->attrName,attrList[i]->vf);  
        else if (attrList[i]->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "%s=\"%g\" ",attrList[i]->attrName,attrList[i]->vd);  
        else if (attrList[i]->tk == ESMC_TYPEKIND_LOGICAL) 
          sprintf(msgbuf, "%s=\"%s\" ",attrList[i]->attrName,ESMC_LogicalString(attrList[i]->vb));
        else if (attrList[i]->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "%s=\"%s\" ",attrList[i]->attrName,attrList[i]->vcp);
        else {
          sprintf(msgbuf, "%s=\"%s\" ",attrList[i]->attrName,"N/A");
        }
        printf(msgbuf);
        fprintf(xml,msgbuf);
        if ((i+1)%2 == 0 && i != attrCount-1) {
          sprintf(msgbuf,"\n            ");
          printf(msgbuf);
          fprintf(xml,msgbuf);
        }
      }
      else { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      }
    }
    if (attrList != NULL) {
      sprintf(msgbuf," />\n");
      printf(msgbuf);
      fprintf(xml,msgbuf);
    }  
    fldcount++;
    if (fldcount == stop) {
      sprintf(msgbuf,"</variable_set>\n");
      printf(msgbuf);
      fprintf(xml,msgbuf);
      fldcount++;
    }
  }
  
  // write the footer (using the fldcount+1 for now to show all fields written)
  if (fldcount == stop+1) {
    sprintf(msgbuf,"\n</model_component>\n");
    printf(msgbuf);
    fprintf(xml,msgbuf);
    fldcount++;
  }
  
  // recurse through the Attribute hierarchy
  for(int i=0;  i<attrCount; i++) {
      attrList[i]->ESMC_AttributeWriteXMLrecurse(xml,convention,purpose,object,
        varobj,stop,fldcount);
  }

  return ESMF_SUCCESS;

 } // end ESMC_AttributeWriteXMLrecurse
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeAlloc"
//BOPI
// !IROUTINE:  ESMC_AttributeAlloc - insure the attrList is long enough
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeAlloc(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int adding) {             // in - number of Attributes being added
// 
// !DESCRIPTION:
//     Insure there is enough space to add nattr more {\tt ESMC_Attributes}.
//
//EOPI

#define ATTR_CHUNK  4           // allocate and realloc in units of this

  void *saveme;
  int localrc;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;

  if ((attrCount + adding) <= attrAlloc) 
      return ESMF_SUCCESS;

  // FIXME: this should be arrays of *attrs, not whole size, right?
  saveme = (void *)attrList;
  attrList = (ESMC_Attribute **)realloc((void *)attrList, 
                           (attrAlloc + ATTR_CHUNK) * sizeof(ESMC_Attribute *));
  if (attrList == NULL) {
      free(saveme);   // although at this point, the heap is probably boffed
      return ESMF_FAILURE;
  }
  attrAlloc += ATTR_CHUNK;

#undef ATTR_CHUNK

  return ESMF_SUCCESS;

}  // end ESMC_AttributeAlloc
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeCopyAll"
//BOPI
// !IROUTINE:  ESMC_AttributeCopyAll - copy {\tt ESMC_Attributes} between objects 
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeCopyAll(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *source) {  // in - the source object
// 
// !DESCRIPTION:
//     All {\tt ESMC_Attributes} associated with the source object are copied to the
//     destination object (*this).
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  *this = source->root;
  
  return ESMF_SUCCESS;

}  // end ESMC_AttributeCopyAll
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeCountTree"
//BOPI
// !IROUTINE:  ESMC_AttributeCountTree - count objects in {\tt ESMC_Attribute} hierarchy 
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeCountTree(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *convention,              // in - convention
      char *purpose,                 // in - purpose
      char *object,                  // in - object type to look for
      int &ans) const{               // inout - the count
// 
// !DESCRIPTION:
//     Count the number of objects in the {\tt ESMC_Attribute} hierarchy 

//EOPI

  int localrc;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // If this is object matches, count it
  if (strcmp(convention,attrConvention) == 0 && 
      strcmp(purpose,attrPurpose) == 0 &&
      strcmp(object,attrObject) == 0 &&
      attrPack == ESMF_TRUE) {
    ans++;
  }
  
  // Recurse the hierarchy
  for (int i = 0; i < attrCount; i++) {
    localrc = attrList[i]->ESMC_AttributeCountTree(convention, purpose, object, ans);
  }
  
  return ESMF_SUCCESS;

}  // end ESMC_AttributeCountTree
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(int) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      ESMC_I4 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I4} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I4) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind I4", &localrc);
        return ESMF_FAILURE;
    }
  
    // simple sanity checks
    if (attr->items != 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
      return ESMF_FAILURE;
    }

    *value = attr->vi;
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(int)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(int *) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_I4 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I4} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I4) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind I4", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) {
      if (attr->items == 1)
        value[0] = attr->vi;
      else for (i=0; i<attr->items; i++)
        value[i] = attr->vip[i];
    }
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(int *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(ESMC_I8) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      ESMC_I8 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I8} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I8) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind I8", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    *value = attr->vtl;
  }
  
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(ESMC_I8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(ESMC_I8 *) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_I8 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I8} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I8) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind I8", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) {
      if (attr->items == 1)
          value[0] = attr->vtl;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vlp[i];
    }
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(ESMC_I8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(ESMC_R4) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      ESMC_R4 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R4} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R4) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind R4", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return localrc;
    }

    *value = attr->vf;
  }
  
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(ESMC_R4)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(ESMC_R4 *) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_R4 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R4} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R4) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind R4", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) {
      if (attr->items == 1)
          value[0] = attr->vf;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vfp[i];
    }
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(ESMC_R4 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(ESMC_R8) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      ESMC_R8 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R8} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R8) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind R8", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    *value = attr->vd;
  }
  
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(ESMC_R8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(ESMC_R8 *) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_R8 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R8} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R8) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind R8", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) {
      if (attr->items == 1)
          value[0] = attr->vd;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vdp[i];
    }
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(ESMC_R8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(ESMC_Logical) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      ESMC_Logical *value) const {   // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_Logical} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_LOGICAL) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind LOGICAL", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    *value = attr->vb;
  }
  
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(ESMC_Logical)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(ESMC_Logical *) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_Logical *value) const {   // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_Logical} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_LOGICAL) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind LOGICAL", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) {
      if (attr->items == 1)
          value[0] = attr->vb;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vbp[i];
    }
  }

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(ESMC_Logical *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(char) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,            // in - name of Attribute to retrieve
      char *value) const {   // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt char *} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // simple sanity checks
  if (!value) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad value return argument", &localrc);
      return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind CHARACTER", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    strcpy(value, attr->vcp);
  }
  
  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(char)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(name) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                    // in - name of Attribute to retrieve
      ESMC_TypeKind *tk,             // out - typekind
      int *count,                    // out - number of values in list
      void *value) const {           // out - Attribute value(s)
// 
// !DESCRIPTION:
//    Get the {\tt void *} value of an {\tt ESMC_Attribute} by name.
//
//EOPI

  int localrc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "bad Attribute name", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    if (tk) 
      *tk = attr->tk;

    if (count) {
      if (attr->tk == ESMC_TYPEKIND_CHARACTER) {
         *count = attr->slen;
      }
      else {
         *count = attr->items; 
      }
    }

    if (value) {
      if (attr->items == 1) {
              if (attr->tk == ESMC_TYPEKIND_I4)
                  *(ESMC_I4 *)value = attr->vi; 
              else if (attr->tk == ESMC_TYPEKIND_I8)
                  *(ESMC_I8 *)value = attr->vtl; 
              else if (attr->tk == ESMC_TYPEKIND_R4)
                  *(ESMC_R4 *)value = attr->vf; 
              else if (attr->tk == ESMC_TYPEKIND_R8)
                  *(ESMC_R8 *)value = attr->vd; 
              else if (attr->tk == ESMC_TYPEKIND_LOGICAL)
                  *(ESMC_Logical *)value = attr->vb;
              else{
                   ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                                       "unknown typekind",
                                       &localrc);
                   return ESMF_FAILURE;
               }
        }
        else {
              if (attr->tk == ESMC_TYPEKIND_I4) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_I4 *)value)[i] = attr->vip[i];
              } else if (attr->tk == ESMC_TYPEKIND_I8) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_I8 *)value)[i] = attr->vlp[i];
              } else if (attr->tk == ESMC_TYPEKIND_R4) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_R4 *)value)[i] = attr->vfp[i];
              } else if (attr->tk == ESMC_TYPEKIND_R8) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_R8 *)value)[i] = attr->vdp[i];
              } else if (attr->tk == ESMC_TYPEKIND_LOGICAL) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_Logical *)value)[i] = attr->vbp[i];
              } else{
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown typekind", 
                                       &localrc);
              return ESMF_FAILURE;
              }
        }
      }  // value
  }
  // ***FIXME*** there is no support for char* here

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(name)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet(num) - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int num,                       // in - number of Attribute to retrieve
      char *name,                    // out - Attribute name
      ESMC_TypeKind *tk,             // out - typekind
      int *count,                    // out - number of values in list
      void *value) const {           // out - Attribute value(s)
// 
// !DESCRIPTION:
//    Get the {\tt void *} value of an {\tt ESMC_Attribute} by number.
//
//EOPI

  int localrc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!num) || (num < 0)) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "Attribute number must be > 0", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMC_AttributeGet(num);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogWrite("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    if (name)
       strcpy(name, attr->attrName);

    if (tk) 
      *tk = attr->tk;

    if (count) {
     if (attr->tk == ESMC_TYPEKIND_CHARACTER)
         *count = attr->slen;
     else
         *count = attr->items;
    }

    if (value) {
      if (attr->items == 1) {
              if (attr->tk == ESMC_TYPEKIND_I4)
                  *(ESMC_I4 *)value = attr->vi; 
              else if (attr->tk == ESMC_TYPEKIND_I8)
                  *(ESMC_I8 *)value = attr->vtl; 
              else if (attr->tk == ESMC_TYPEKIND_R4)
                  *(ESMC_R4 *)value = attr->vf; 
              else if (attr->tk == ESMC_TYPEKIND_R8)
                  *(ESMC_R8 *)value = attr->vd; 
              else if (attr->tk == ESMC_TYPEKIND_LOGICAL)
                  *(ESMC_Logical *)value = attr->vb;
              else{
                  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown typekind", 
                                       &localrc);
                  return ESMF_FAILURE;
              }
 
      } else {
              if (attr->tk == ESMC_TYPEKIND_I4) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_I4 *)value)[i] = attr->vip[i];
              } else if (attr->tk == ESMC_TYPEKIND_I8) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_I8 *)value)[i] = attr->vlp[i];
              } else if (attr->tk == ESMC_TYPEKIND_R4) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_R4 *)value)[i] = attr->vfp[i];
              } else if (attr->tk == ESMC_TYPEKIND_R8) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_R8 *)value)[i] = attr->vdp[i];
              } else if (attr->tk == ESMC_TYPEKIND_LOGICAL) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_Logical *)value)[i] = attr->vbp[i];
              } else {
                  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown typekind", 
                                       &localrc);
                  return ESMF_FAILURE;
              }
      }
    }  // value
  }
  
  // ***FIXME*** there is no support for char* here

  return ESMF_SUCCESS;

}  // end ESMC_AttributeGet(num)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet - get {\tt ESMC_Attribute} from an ESMF type
//
// !INTERFACE:
      ESMC_Attribute *ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMC_Attribute} pointer or NULL on error exit.
// 
// !ARGUMENTS:
      char *name) const {        // in - Attribute name to retrieve
// 
// !DESCRIPTION:
//    Get the name of an {\tt ESMC_Attribute}.
//
//EOPI

  int i;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad Attribute name", NULL);
       return NULL;
  }

  for (i=0; i<attrCount; i++) {
      if (strcmp(name, attrList[i]->attrName))
          continue;

      // if you get here, you found a match. 
      return attrList[i]; 
  }   

  // you get here if no matches found
  return NULL;

}  // end ESMC_AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGet"
//BOPI
// !IROUTINE:  ESMC_AttributeGet - get an {\tt ESMC_Attribute} by number
//
// !INTERFACE:
      ESMC_Attribute *ESMC_Attribute::ESMC_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int number) const {             // in - Attribute number
// 
// !DESCRIPTION:
//     Allows the caller to get {\tt ESMC_Attributes} by number instead of by name.
//     This can be useful in iterating through all {\tt ESMC_Attributes} in a loop.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];

  // simple sanity check
  if ((number < 0) || (number >= attrCount)) {
      sprintf(msgbuf, "Attribute number must be  0 < N <= %d\n", attrCount-1);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, NULL);
      return NULL;
  }

  return attrList[number];

}  // end ESMC_AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGetCount"
//BOPI
// !IROUTINE:  ESMC_AttributeGetCount - get an the number of {\tt ESMC_Attributes}
// 
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGetCount(
// 
// !RETURN VALUE:
//    number of {\tt ESMC_Attributes} in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of {\tt ESMC_Attributes} present
//
//EOPI

  return attrCount;

} // end ESMC_AttributeGetCount
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Attribute *attr) {   // in - Attribute name, type, value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//     This version of set is used when the caller has already allocated
//     an {\tt ESMC_Attribute} object and filled it, and the {\tt ESMC_Attribute} 
//     is simply added to the list belonging to this object.
//
//EOPI

  int i, localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute object", &localrc);
       return ESMF_FAILURE;
  }

  // first, see if you are replacing an existing Attribute
  for (i=0; i<attrCount; i++) {
      if (strcmp(attr->attrName, attrList[i]->attrName))
          continue;

      // FIXME: we might want an explicit flag saying that this is what
      // is wanted, instead of an error if a previous value not expected.

      // if you get here, you found a match.  replace previous copy.

      // delete old Attribute, including possibly freeing a list
      attrList[i]->~ESMC_Attribute();

      attrList[i] = attr;
      return ESMF_SUCCESS;
  }   

  // new Attribute name, make sure there is space for it.
  localrc = ESMC_AttributeAlloc(1);
  if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeSet failed to allocate space", &localrc);
       return ESMF_FAILURE;
  }  
  attrList[attrCount] = attr;   
  attrCount++;
  
  return ESMF_SUCCESS;

}  // end ESMC_AttributeSet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(int) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      ESMC_I4 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I4} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_I4, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(int)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(int *) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      int count,               // in - number of ints in list
      ESMC_I4 *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I4} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_I4, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(int *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(ESMC_I8) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      ESMC_I8 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I*} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_I8, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(ESMC_I8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(ESMC_I8 *) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      int count,               // in - number of ints in list
      ESMC_I8 *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I8} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_I8, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(ESMC_I8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(ESMC_R4) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      ESMC_R4 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R4} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_R4, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(ESMC_R4)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(ESMC_R4 *) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      int count,               // in - number of ESMC_R4s in list
      ESMC_R4 *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R4} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_R4, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(ESMC_R4 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(ESMC_R8) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      ESMC_R8 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R8} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_R8, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(ESMC_R8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(ESMC_R8 *) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      int count,               // in - number of ESMC_R8s in list
      ESMC_R8 *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R8} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_R8, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(ESMC_R8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(ESMC_Logical) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      ESMC_Logical value) {    // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_Logical} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_LOGICAL, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(ESMC_Logical)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(ESMC_Logical *) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,              // in - Attribute name
      int count,               // in - number of logicals in list
      ESMC_Logical *value) {   // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_Logical} valueList of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_LOGICAL, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(ESMC_Logical *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet(char) - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,       // in - Attribute name
      char *value) {    // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt char *} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_CHARACTER, 1, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet(char)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSet"
//BOPI
// !IROUTINE:  ESMC_AttributeSet - set {\tt ESMC_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,           // in - Attribute name
      ESMC_TypeKind tk,     // in - typekind
      int count,            // in - number of values
      void *value) {        // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt void*} value of an {\tt ESMC_Attribute}.
//
//EOPI

  int localrc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, tk, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMC_AttributeSet(attr);

  return localrc;

}  // end ESMC_AttributeSet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSetLink"
//BOPI
// !IROUTINE:  ESMC_AttributeSetLink - set a link in an {\tt ESMC_Attribute} hierarchy
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSetLink(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *destination) {  // in/out destination Attribute to be linked
// !DESCRIPTION:
//     Set a link in an {\tt ESMC_Attribute} hierarchy.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  if(this->attrAlloc < (this->attrCount + 1)) {
    localrc = this->ESMC_AttributeAlloc(1);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeSetLink failed to allocate space", &localrc);
      return ESMF_FAILURE;
    }
  }
    
  this->attrList[attrCount] = &(destination->root);
  (this->attrCount)++;

  return ESMF_SUCCESS;

}  // end ESMC_AttributeSetLink
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// NOT IMPLEMENTED:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeCopy"
//BOPI
// !IROUTINE:  ESMC_AttributeCopy - copy an {\tt ESMC_Attribute} between two objects
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeCopy(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,                 // in - Attribute to copy
      ESMC_Attribute *destination) {   // in - the destination object
// 
// !DESCRIPTION:
//     The specified {\tt ESMC_Attribute} associated with the source object (this) is
//     copied to the destination object. 

//EOPI

  return ESMC_RC_NOT_IMPL;

}  // end ESMC_AttributeCopy
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGetList"
//BOPI
// !IROUTINE:  ESMC_AttributeGetList - get multiple {\tt ESMC_Attribute}s at once
// 
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGetList(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char **namelist,                   // in - null term list of names
      ESMC_Attribute *valuelist) const { // out - list of Attribute values
// 
// !DESCRIPTION:
//     Get multiple {\tt ESMC_Attributes} from an object in a single call
//
//EOPI

  return ESMC_RC_NOT_IMPL;

}  // end ESMC_AttributeGetList
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGetNameList"
//BOPI
// !IROUTINE:  ESMC_AttributeGetNameList - get the list of {\tt ESMC_Attribute} names
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeGetNameList(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int *count,               // out - number of Attributes
      char **namelist) const {  // out - namelist
// 
// !DESCRIPTION:
//     Return a list of all {\tt ESMC_Attribute} names without returning the values.
//
//EOPI

  return ESMC_RC_NOT_IMPL;

}  // end ESMC_AttributeGetNameList
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSetList"
//BOPI
// !IROUTINE:  ESMC_AttributeSetList - set multiple {\tt ESMC_Attribute}s at once
// 
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttributeSetList(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      int count,                   // in - number of Attributes to set
      ESMC_Attribute *valuelist) { // in - list of Attribute values
// 
// !DESCRIPTION:
//    Set multiple {\tt ESMC_Attribute}s on an object in one call. 
//
//EOPI

  return ESMC_RC_NOT_IMPL;

}  // end ESMC_AttributeSetList
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// Modifiers, Constructors, Destructors, Serializers, Print:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Attribute()"
//BOPI
// !IROUTINE:  ESMC_Attribute - native C++ constructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::ESMC_Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     Create an empty {\tt ESMC_Attribute} structure.
//
//EOPI

  attrCount = 0;
  attrAlloc = 0;
  attrList = ESMC_NULL_POINTER;
  attrName[0] = '\0';
  items = 0;
  slen = 0;
  attrConvention[0] = '\0';
  attrPurpose[0] = '\0';
  attrObject[0] = '\0';
  voidp = NULL;

 } // end ESMC_Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Attribute()"
//BOPI
// !IROUTINE:  ESMC_Attribute - native C++ constructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::ESMC_Attribute(
//
// !RETURN VALUE:
//    {\tt ESMC_Attribute} object
//
// !ARGUMENTS:
        char *name,                // Attribute name
        ESMC_TypeKind typekind,    // typekind
        int numitems,              // single or list
        void *datap) {             // generic pointer to values
//
// !DESCRIPTION:
//   Initialize an {\tt ESMC_Attribute}, and make a copy of the data if items > 1.
//
//EOPI
  int i, len, localrc;
  char msgbuf[ESMF_MAXSTR];

  if (!name)
      attrName[0] = '\0';
  else {
      len = strlen(name)+1;   // strlen doesn't count trailing null
      if (len > ESMF_MAXSTR) {
        sprintf(msgbuf, "attr name %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      }
      memcpy(attrName, name, len);
  }

  tk = typekind;
  items = numitems;
  slen = 0;          // only used for string values
  attrConvention[0] = '\0';
  attrPurpose[0] = '\0';
  attrObject[0] = '\0';

  attrCount = 0;
  attrAlloc = 0;
  attrList = ESMC_NULL_POINTER;
  
  if (items <= 0)
      voidp = NULL;
 
  else if (items == 1) {
      if (!datap) 
          voidp = NULL;
      else  {
            if (tk == ESMC_TYPEKIND_I4)
                vi = *(ESMC_I4 *)datap;  
            else if (tk == ESMC_TYPEKIND_I8)
                vtl = *(ESMC_I8 *)datap;  
            else if (tk == ESMC_TYPEKIND_R4)
                vf = *(ESMC_R4 *)datap;  
            else if (tk == ESMC_TYPEKIND_R8)
                vd = *(ESMC_R8 *)datap;  
            else if (tk == ESMC_TYPEKIND_LOGICAL)
                vb = *(ESMC_Logical *)datap;  
            else if (tk == ESMC_TYPEKIND_CHARACTER){
                slen = strlen((char *)datap) + 1;
                vcp = new char[slen];
                strncpy(vcp, (char *)datap, slen);
           }else
                voidp = NULL;
    }

  } else {
    // items > 1, alloc space for a list and do the copy
        if (tk == ESMC_TYPEKIND_I4) {
            vip = new ESMC_I4[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vip[i] = ((ESMC_I4 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_I8) {
            vlp = new ESMC_I8[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vlp[i] = ((ESMC_I8 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_R4) {
            vfp = new ESMC_R4[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vfp[i] = ((ESMC_R4 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_R8) {
            vdp = new ESMC_R8[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vdp[i] = ((ESMC_R8 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_LOGICAL) {
            vbp = new ESMC_Logical[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vbp[i] = ((ESMC_Logical *)datap)[i];  

        } else
           // error - arrays of char strings not allowed
                voidp = NULL;
  }

 } // end ESMC_Attribute
//----------------------------------------------------------------------------- 
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Attribute()"
//BOPI
// !IROUTINE:  ESMC_Attribute - native C++ constructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::ESMC_Attribute(
//
// !RETURN VALUE:
//    new {\tt ESMC_Attribute} object
//
// !ARGUMENTS:
        char *name,                  // Attribute name
        char *conv,                  // convention
        char *purp,                  // purpose
        char *obj) {                 // object
//
// !DESCRIPTION:
//   Initialize an {\tt ESMC_Attribute} and set the name, convention, and purpose.
//
//EOPI
  int len, localrc;
  char msgbuf[ESMF_MAXSTR];

  attrCount = 0;
  attrAlloc = 0;
  attrList = ESMC_NULL_POINTER;
  items = 0;
  slen = 0;
  voidp = NULL;
  attrPack = ESMF_TRUE;

  if (!name)
      attrName[0] = '\0';
  else {
      len = strlen(name)+1;   // strlen doesn't count trailing null
      if (len > ESMF_MAXSTR) {
        sprintf(msgbuf, "attr name %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      }
      memcpy(attrName, name, len);
  }

  if (!conv)
      attrConvention[0] = '\0';
  else {
      len = strlen(conv)+1;   // strlen doesn't count trailing null
      if (len > ESMF_MAXSTR) {
        sprintf(msgbuf, "attr convention %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      }
      memcpy(attrConvention, conv, len);
  }

  if (!purp)
      attrPurpose[0] = '\0';
  else {
      len = strlen(purp)+1;   // strlen doesn't count trailing null
      if (len > ESMF_MAXSTR) {
        sprintf(msgbuf, "attr purpose %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      }
      memcpy(attrPurpose, purp, len);
  }

  if (!obj)
      attrObject[0] = '\0';
  else {
      len = strlen(obj)+1;   // strlen doesn't count trailing null
      if (len > ESMF_MAXSTR) {
        sprintf(msgbuf, "attr object %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      }
      memcpy(attrObject, obj, len);
  }
} // end ESMC_Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_Attribute()"
//BOPI
// !IROUTINE:  ~ESMC_Attribute - native C++ destructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::~ESMC_Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//    Delete an {\tt ESMC_Attribute} hierarchy.
//
//EOPI

  if (tk == ESMC_TYPEKIND_CHARACTER) delete [] vcp;

  if (items > 1) {
        if (tk == ESMC_TYPEKIND_I4) delete [] vip;
        else if (tk == ESMC_TYPEKIND_I8) delete [] vlp;
        else if (tk == ESMC_TYPEKIND_R4) delete [] vfp;
        else if (tk == ESMC_TYPEKIND_R8) delete [] vdp;  
        else if (tk == ESMC_TYPEKIND_LOGICAL) delete [] vbp;
  }

  // if there are Attributes or attpacks delete, if links disconnect
  for (int i=0; i<attrCount; i++) {
    if(attrList[i]->attrList == ESMC_NULL_POINTER ||
       attrList[i]->attrPack == ESMF_TRUE) attrList[i]->~ESMC_Attribute();
    else attrList[i] = ESMC_NULL_POINTER;
  }

  if(attrList) delete [] attrList;

 } // end ~ESMC_Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttrModifyValue()"
//BOPI
// !IROUTINE:  ESMC_AttrModifyValue - native C++ modifyer for ESMC_Attribute class
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_AttrModifyValue(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        ESMC_TypeKind typekind,    // typekind
        int numitems,              // single or list
        void *datap) {             // generic pointer to values
//
// !DESCRIPTION:
//   Set a value on an existing {\tt ESMC_Attribute} object.
//
//EOPI
  int i, localrc;

  tk = typekind;
  items = numitems;
  slen = 0;          // only used for string values
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  if (items <= 0)
      voidp = NULL;
 
  else if (items == 1) {
      if (!datap) 
          voidp = NULL;
      else  {
            if (tk == ESMC_TYPEKIND_I4)
                vi = *(ESMC_I4 *)datap;  
            else if (tk == ESMC_TYPEKIND_I8)
                vtl = *(ESMC_I8 *)datap;  
            else if (tk == ESMC_TYPEKIND_R4)
                vf = *(ESMC_R4 *)datap;  
            else if (tk == ESMC_TYPEKIND_R8)
                vd = *(ESMC_R8 *)datap;  
            else if (tk == ESMC_TYPEKIND_LOGICAL)
                vb = *(ESMC_Logical *)datap;  
            else if (tk == ESMC_TYPEKIND_CHARACTER){
                slen = strlen((char *)datap) + 1;
                vcp = new char[slen];
                strncpy(vcp, (char *)datap, slen);
           }else
                voidp = NULL;
    }

  } else {
    // items > 1, alloc space for a list and do the copy
        if (tk == ESMC_TYPEKIND_I4) {
            vip = new ESMC_I4[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vip[i] = ((ESMC_I4 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_I8) {
            vlp = new ESMC_I8[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vlp[i] = ((ESMC_I8 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_R4) {
            vfp = new ESMC_R4[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vfp[i] = ((ESMC_R4 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_R8) {
            vdp = new ESMC_R8[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vdp[i] = ((ESMC_R8 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_LOGICAL) {
            vbp = new ESMC_Logical[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vbp[i] = ((ESMC_Logical *)datap)[i];  

        } else
           // error - arrays of char strings not allowed
                voidp = NULL;
  }

  return ESMF_SUCCESS;

 } // end ESMC_AttrModifyValue
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Deserialize"
//BOPI
// !IROUTINE:  ESMC_Deserialize - Turn a byte stream into an object
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_Deserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // in - byte stream to read
      int *offset) {         // inout - original offset, updated to point 
                             //       to first free byte after current obj
//
// !DESCRIPTION:
//    Turn a stream of bytes into an {\tt ESMC_Attribute} hierarchy.
//
//EOPI
    int loffset, nbytes, chars;
    int localrc;
    
    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // Define serialization macros
#define DESERIALIZE_VAR(bufptr,loff,var,t) \
  var=(*((t *)((bufptr)+(loff))));    \
  loff += (sizeof(t));  

#define DESERIALIZE_VARC(bufptr,loff,var,s) \
  memcpy(&(var),(bufptr)+(loff),s);      \
  loff += s;

#define DESERIALIZE_VAR1D(bufptr,loff,varptr,s,t)  \
  varptr = new t[s];           \
  memcpy(varptr,(bufptr)+(loff),((s)*sizeof(t)));      \
  loff += ((s)*sizeof(t));

    // get localoffset
    loffset=*offset;

    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrName,chars);
    DESERIALIZE_VAR(buffer,loffset,tk,ESMC_TypeKind);
      
    DESERIALIZE_VAR(buffer,loffset,items,int);
    DESERIALIZE_VAR(buffer,loffset,slen,int);
    
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrConvention,chars);
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrPurpose,chars);
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrObject,chars);
      
    DESERIALIZE_VAR(buffer,loffset,attrCount,int);
    DESERIALIZE_VAR(buffer,loffset,attrAlloc,int);
    
    localrc = ESMC_AttributeAlloc(attrAlloc);

    if (items == 1) {
      if (tk == ESMC_TYPEKIND_I4) {
        DESERIALIZE_VAR(buffer,loffset,vi,ESMC_I4); }
      else if (tk == ESMC_TYPEKIND_I8) {
        DESERIALIZE_VAR(buffer,loffset,vtl,ESMC_I8); }
      else if (tk == ESMC_TYPEKIND_R4) {
        DESERIALIZE_VAR(buffer,loffset,vf,ESMC_R4); }
      else if (tk == ESMC_TYPEKIND_R8) {
        DESERIALIZE_VAR(buffer,loffset,vd,ESMC_R8); }
      else if (tk == ESMC_TYPEKIND_LOGICAL) {
        DESERIALIZE_VAR(buffer,loffset,vb,ESMC_Logical); }
      else if (tk == ESMC_TYPEKIND_CHARACTER) {
        DESERIALIZE_VAR(buffer,loffset,chars,int);
        DESERIALIZE_VAR1D(buffer,loffset,vcp,chars,char); }
    }
    if (items > 1) { 
      if (tk == ESMC_TYPEKIND_I4) {
        DESERIALIZE_VAR1D(buffer,loffset,vip,items,ESMC_I4); }
      else if (tk == ESMC_TYPEKIND_I8) {
        DESERIALIZE_VAR1D(buffer,loffset,vlp,items,ESMC_I8);} 
      else if (tk == ESMC_TYPEKIND_R4) {
        DESERIALIZE_VAR1D(buffer,loffset,vfp,items,ESMC_R4); }
      else if (tk == ESMC_TYPEKIND_R8) {
        DESERIALIZE_VAR1D(buffer,loffset,vdp,items,ESMC_R8);}
      else if (tk == ESMC_TYPEKIND_LOGICAL) {
        DESERIALIZE_VAR1D(buffer,loffset,vbp,items,ESMC_Logical); }
    }

    // make sure loffset is aligned correctly
    nbytes=loffset%8;
    if (nbytes!=0) loffset += 8-nbytes;  

    // Deserialize the {\tt ESMC_Attribute} hierarchy
    for (int i=0; i<attrCount; i++) {
      attrList[i] = new ESMC_Attribute();
      if (!(attrList[i]))
        return ESMF_FAILURE;
      attrList[i]->ESMC_Deserialize(buffer,&loffset);
    }
      
    // make sure loffset is aligned correctly
    nbytes=loffset%8;
    if (nbytes!=0) loffset += 8-nbytes;
       
    // output localoffset
    *offset=loffset;

    // Undefine serialization macros, so they don't cause troubles elsewhere
#undef DESERIALIZE_VAR
#undef DESERIALIZE_VARC
#undef DESERIALIZE_VAR1D

   return ESMF_SUCCESS;

 } // end ESMC_Deserialize
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Serialize"
//BOPI
// !IROUTINE:  ESMC_Serialize - Turn the object information into a byte stream
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_Serialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *offset) const{    // inout - original offset, updated to point 
                             //  to first free byte after current obj info
// 
// !DESCRIPTION:
//    Turn an {\tt ESMC_Attribute} into a stream of bytes.
//
//EOPI
    int loffset=0;
    bool cc;
    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;
    cc = false;
    localrc = ESMC_SerializeCC(buffer,length,loffset,cc);
    cc = true;
    localrc = ESMC_SerializeCC(buffer,length,*offset,cc);

    // return successfully
    return ESMF_SUCCESS;

 } // end ESMC_Serialize
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_SerializeCC"
//BOPI
// !IROUTINE:  ESMC_SerializeCC - Turn the object information into a byte stream
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_SerializeCC(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int &offset,           // inout - original offset, updated throughout
      bool cc) const {       // in - to tell whether in count or copy mode 
//
// !DESCRIPTION:
//    Turn an {\tt ESMC_Attribute} into a stream of bytes.
//
//EOPI
    int nbytes;
    int localrc;

    // Define serialization macros
#define SERIALIZE_VAR(cc,bufptr,loff,var,t) \
  if (cc) *((t *)((bufptr)+(loff)))=var;    \
  loff += (sizeof(t));   

#define SERIALIZE_VARC(cc,bufptr,loff,var,s) \
  if (cc) memcpy((bufptr)+(loff),&(var),s);      \
  loff += s;

#define SERIALIZE_VAR1D(cc,bufptr,loff,varptr,s,t)    \
  if (cc) memcpy((bufptr)+(loff),varptr,((s)*sizeof(t)));       \
  loff += ((s)*sizeof(t)); 
  
    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

      SERIALIZE_VAR(cc,buffer,offset,(strlen(attrName)+1),int);
      SERIALIZE_VARC(cc,buffer,offset,attrName,(strlen(attrName)+1));
      SERIALIZE_VAR(cc,buffer,offset,tk,ESMC_TypeKind);
      
      SERIALIZE_VAR(cc,buffer,offset,items,int);
      SERIALIZE_VAR(cc,buffer,offset,slen,int);
    
      SERIALIZE_VAR(cc,buffer,offset,(strlen(attrConvention)+1),int);
      SERIALIZE_VARC(cc,buffer,offset,attrConvention,(strlen(attrConvention)+1));
      SERIALIZE_VAR(cc,buffer,offset,(strlen(attrPurpose)+1),int);
      SERIALIZE_VARC(cc,buffer,offset,attrPurpose,(strlen(attrPurpose)+1));
      SERIALIZE_VAR(cc,buffer,offset,(strlen(attrObject)+1),int);
      SERIALIZE_VARC(cc,buffer,offset,attrObject,(strlen(attrObject)+1));
      
      SERIALIZE_VAR(cc,buffer,offset,attrCount,int);
      SERIALIZE_VAR(cc,buffer,offset,attrAlloc,int);

      if (items == 1) {
        if (tk == ESMC_TYPEKIND_I4) {
          SERIALIZE_VAR(cc,buffer,offset,vi,ESMC_I4); }
        else if (tk == ESMC_TYPEKIND_I8) {
          SERIALIZE_VAR(cc,buffer,offset,vtl,ESMC_I8); }
        else if (tk == ESMC_TYPEKIND_R4) {
          SERIALIZE_VAR(cc,buffer,offset,vf,ESMC_R4); }
        else if (tk == ESMC_TYPEKIND_R8) {
          SERIALIZE_VAR(cc,buffer,offset,vd,ESMC_R8); }
        else if (tk == ESMC_TYPEKIND_LOGICAL) {
          SERIALIZE_VAR(cc,buffer,offset,vb,ESMC_Logical); }
        else if (tk == ESMC_TYPEKIND_CHARACTER) {
          SERIALIZE_VAR(cc,buffer,offset,(strlen(vcp)+1),int);
          SERIALIZE_VAR1D(cc,buffer,offset,vcp,strlen(vcp)+1,char); }
      }
      if (items > 1) { 
        if (tk == ESMC_TYPEKIND_I4) {
          SERIALIZE_VAR1D(cc,buffer,offset,vip,items,ESMC_I4); }
        else if (tk == ESMC_TYPEKIND_I8) {
          SERIALIZE_VAR1D(cc,buffer,offset,vlp,items,ESMC_I8); }
        else if (tk == ESMC_TYPEKIND_R4) {
          SERIALIZE_VAR1D(cc,buffer,offset,vfp,items,ESMC_R4); }
        else if (tk == ESMC_TYPEKIND_R8) {
          SERIALIZE_VAR1D(cc,buffer,offset,vdp,items,ESMC_R8); }
        else if (tk == ESMC_TYPEKIND_LOGICAL) {
          SERIALIZE_VAR1D(cc,buffer,offset,vbp,items,ESMC_Logical); }
      }

      // make sure offset is aligned correctly
      nbytes=offset%8;
      if (nbytes!=0) offset += 8-nbytes;
    
      // Serialize the Attribute hierarchy
      for (int i=0; i<attrCount; i++) {
        attrList[i]->ESMC_SerializeCC(buffer,length,offset,cc);
      }
  
      // make sure offset is aligned correctly
      nbytes=offset%8;
      if (nbytes!=0) offset += 8-nbytes;
      
      // check if buffer has enough free memory, expand?
      if (*length < offset){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                      "Buffer too short to add an Attribute hierarchy", &localrc);
        return localrc;
      }
      
    // Undefine serialization macros, so they don't cause troubles elsewhere
#undef SERIALIZE_VAR
#undef SERIALIZE_VARC
#undef SERIALIZE_VAR1D

  // return successfully
  return ESMF_SUCCESS;

 } // end ESMC_SerializeCC
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Print"
//BOPI
// !IROUTINE:  ESMC_Attribute::ESMC_Print - Print the {\tt ESMC_Attribute} contents
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_Print(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      void) const {                    // could add options at some point
// 
// !DESCRIPTION:
//     Print the contents of an {\tt ESMC_Attribute} object
//
//EOPI
  int localrc;
  char msgbuf[ESMF_MAXSTR];

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // print name
  sprintf(msgbuf, "        name: %s\n",  attrName);
  printf(msgbuf);
  ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  
  // print items if there are any
  if (items <= 0) {
      sprintf(msgbuf, "        value: \n");
      printf(msgbuf);
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  }

  if (items == 1) {
      sprintf(msgbuf, "        value: ");
      printf(msgbuf);
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
             if (tk == ESMC_TYPEKIND_I4)
                 sprintf(msgbuf, "%d\n", vi); 
             else if (tk == ESMC_TYPEKIND_I8)
                 sprintf(msgbuf, "%ld\n", vtl); 
             else if (tk == ESMC_TYPEKIND_R4)
                 sprintf(msgbuf, "%f\n", vf); 
             else if (tk == ESMC_TYPEKIND_R8)
                 sprintf(msgbuf, "%g\n", vd); 
             else if (tk == ESMC_TYPEKIND_LOGICAL)
                 sprintf(msgbuf, "%s\n", ESMC_LogicalString(vb)); 
             else if (tk == ESMC_TYPEKIND_CHARACTER)
                 sprintf(msgbuf, "%s\n", vcp);
             else{ 
                 ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "unknown value", &localrc);
                 return localrc;
             }
      printf(msgbuf);
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  }

  if (items > 1) { 
      sprintf(msgbuf, "        %d items, values:\n", items);
      printf(msgbuf);
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      for (int i=0; i<items; i++) {
                if (tk == ESMC_TYPEKIND_I4) {
                    sprintf(msgbuf, "          \t item %d: %d\n", i, vip[i]); 
                } else if (tk == ESMC_TYPEKIND_I8) {
                    sprintf(msgbuf, "          \t item %d: %ld\n", i, vlp[i]); 
                } else if (tk == ESMC_TYPEKIND_R4) {
                    sprintf(msgbuf, "          \t item %d: %f\n", i, vfp[i]); 
                } else if (tk == ESMC_TYPEKIND_R8) {
                    sprintf(msgbuf, "          \t item %d: %g\n", i, vdp[i]); 
                } else if (tk == ESMC_TYPEKIND_LOGICAL) {
                    sprintf(msgbuf, "          \t item %d: %s\n", i,
                      ESMC_LogicalString(vbp[i]));
                } else{
                    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "          \t unknown value", &localrc);
                    return localrc;
                }
      printf(msgbuf);
      }
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  }

  // print convention
  sprintf(msgbuf, "        convention: %s\n",  attrConvention);
  printf(msgbuf);
  ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  
  // print purpose
  sprintf(msgbuf, "        purpose: %s\n",  attrPurpose);
  printf(msgbuf);
  ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  
  // print object
  sprintf(msgbuf, "        object: %s\n",  attrObject);
  printf(msgbuf);
  ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);

  sprintf(msgbuf, "        attrCount: %d\n", attrCount);
  printf(msgbuf);
  ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  for (int i=0; i<attrCount; i++) {
      sprintf(msgbuf, "   Attr %d:\n", i);
      printf(msgbuf);
      ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      attrList[i]->ESMC_Print();
  }

  return ESMF_SUCCESS;

}  // end ESMC_Print
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeCopy(=)"
//BOPI
// !IROUTINE:  ESMC_AttributeCopy(=) - assignment operator for {\tt ESMC_Attribute}
//
// !INTERFACE:
      ESMC_Attribute& ESMC_Attribute::operator=(
//
// !RETURN VALUE:
//    Updated desination {\tt ESMC_Attribute}
//
// !ARGUMENTS:
        const ESMC_Attribute &source) {   // in - ESMC_Attribute
//
// !DESCRIPTION:
//   Copy an {\tt ESMC_Attribute}, including contents, to destination (this).
//
//EOPI
  int i, localrc;
  ESMC_Attribute *attr;

  memcpy(attrName, source.attrName, ESMF_MAXSTR);
  memcpy(attrConvention, source.attrConvention, ESMF_MAXSTR);
  memcpy(attrPurpose, source.attrPurpose, ESMF_MAXSTR);
  memcpy(attrObject, source.attrObject, ESMF_MAXSTR);

  tk = source.tk;
  items = source.items;
  slen = source.slen;
  
  if (items <= 0)
    voidp = NULL;
 
  else if (items == 1) {
        if (tk == ESMC_TYPEKIND_I4)
            vi = source.vi;  
        else if (tk == ESMC_TYPEKIND_I8)
            vtl = source.vtl;  
        else if (tk == ESMC_TYPEKIND_R4)
            vf = source.vf;  
        else if (tk == ESMC_TYPEKIND_R8)
            vd = source.vd;  
        else if (tk == ESMC_TYPEKIND_LOGICAL)
            vb = source.vb;
        else if (tk == ESMC_TYPEKIND_CHARACTER){
            vcp = new char[slen];   // includes trailing null
            memcpy(vcp, (char *)source.vcp, slen);
        }else
            voidp = NULL;
  } else {
    // items > 1, alloc space for a list and do the copy
          if (tk == ESMC_TYPEKIND_I4) {
              vip = new ESMC_I4[items];      
              for (i=0; i<items; i++)
                  vip[i] = source.vip[i];  
          } else if (tk == ESMC_TYPEKIND_I8) {
              vlp = new ESMC_I8[items];      
              for (i=0; i<items; i++)
                  vlp[i] = source.vlp[i];  
          } else if (tk == ESMC_TYPEKIND_R4) {
              vfp = new ESMC_R4[items];      
              for (i=0; i<items; i++)
                  vfp[i] = source.vfp[i];  
          } else if (tk == ESMC_TYPEKIND_R8) {
              vdp = new ESMC_R8[items];      
              for (i=0; i<items; i++)
                  vdp[i] = source.vdp[i];  
          } else if (tk == ESMC_TYPEKIND_LOGICAL){
              vbp = new ESMC_Logical[items];      
              for (i=0; i<items; i++)
                vbp[i] = source.vbp[i];  
          }else
          // error - arrays of char strings not allowed
             voidp = NULL;
  }

  // if Attribute list, copy it.
  for (i=0; i<source.attrCount; i++) {
    if(source.attrList[i]->attrList == ESMC_NULL_POINTER) {
      attr = new ESMC_Attribute();
      *attr = *(source.attrList[i]);
      localrc = ESMC_AttributeSet(attr);
    }
    else {
      if(this->attrAlloc < (this->attrCount + 1)) {
        localrc = this->ESMC_AttributeAlloc(1);
      }
    
      attrList[attrCount] = source.attrList[i];
      (this->attrCount)++;
    }
  }

  return (*this);

 } // end ESMC_Attribute::operator=
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeGetObjectList"
//BOPI
// !IROUTINE:  ESMC_AttributeGetObjectList - get an {\tt ESMC_Attribute} from multiple ESMF objects 
//
// !INTERFACE:
      int ESMC_AttributeGetObjectList(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,            // in - list of ESMC objects
      char *name,                        // in - Attribute name
      ESMC_Attribute *valuelist) {       // out - list of Attribute values
// 
// !DESCRIPTION:
//     Get the same {\tt ESMC_Attribute} name from multiple objects in one call.
//
//EOPI
    int localrc;

    // Initialize local return code ; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    return localrc;

}  // end ESMC_AttributeGetObjectList
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeSetObjectList"
//BOPI
// !IROUTINE:  ESMC_AttributeSetObjectList - set an {\tt ESMC_Attribute} on multiple ESMF objects
//
// !INTERFACE:
      int ESMC_AttributeSetObjectList(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,    // in - list of ESMC objects
      ESMC_Attribute *value) {   // in - Attribute value
// 
// !DESCRIPTION:
//     Set the same {\tt ESMC_Attribute} on multiple objects in one call.
//
//EOPI

    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    return localrc;

}  // end ESMC_AttributeSetObjectList
//-----------------------------------------------------------------------------
