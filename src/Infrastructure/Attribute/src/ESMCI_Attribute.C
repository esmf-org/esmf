// $Id: ESMCI_Attribute.C,v 1.5 2008/10/23 20:58:29 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMCI_Attribute.C"

// ESMCI_Attribute method implementation (body) file

// single blank line to make protex happy.
//BOPI

//EOPI
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMCI_Attribute} methods declared
// in the companion file ESMCI_Attribute.h
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include "ESMC_Start.h"
#include "ESMCI_Attribute.h"
#include "ESMC_Base.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_Attribute.C,v 1.5 2008/10/23 20:58:29 rokuingh Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

// initialize class-wide instance counter
static int globalCount = 0;   //TODO: this should be a counter per VM context

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the ESMCI_Attribute routines
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PRIVATE:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_Attribute"
//BOPI
// !IROUTINE:  ESMCI_Attribute - empty private copy constructor
//
// !INTERFACE:
      ESMCI_Attribute::ESMCI_Attribute(
//
// !ARGUMENTS:
      const ESMCI_Attribute&) {
// 
// !RETURN VALUE:
//    {\tt ESMCI_Attribute} object.
// 
// !DESCRIPTION:
//    Empty private copy constructor.
//
//EOPI

}  // end ESMCI_Attribute

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PUBLIC:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttPackCreate"
//BOPI
// !IROUTINE:  ESMCI_AttPackCreate() - create an attpack and add an {\tt ESMCI_Attribute}
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttPackCreate(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - Attribute name
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object,                  // in - Attribute object type
      const ESMC_AttPackNestFlag &flag) {    // in - flag to nest or not  
// 
// !DESCRIPTION:
//     Setup the name, convention and purpose of an attpack and add
//     an {\tt ESMCI_Attribute} with a specified name but no value.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr, *attpack, *nestedpack;
  bool stop = false;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Search for the attpack, make it if not found
  attpack = ESMCI_AttPackGet(convention, purpose, object);
  if(!attpack) {
    // name the attribute package using convention, purpose, and object
    attpack = new ESMCI_Attribute(convention, purpose, object);
    if (!attpack) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed initializing an attpack", &localrc);
      return ESMF_FAILURE;
    }
    
    if (flag == ESMF_ATTPACKNEST_ON) {
      // look for the lowest down nested attpack to attach this new one to
      nestedpack = ESMCI_AttPackGetNested(stop);
    
      // set attrNested
      attpack->attrNested = ESMF_TRUE;
      // set the attpack on the nestedpack, here or elsewhere
      localrc = nestedpack->ESMCI_AttributeSet(attpack);
    }
    else if (flag == ESMF_ATTPACKNEST_OFF) {
      // set the attpack here
      localrc = ESMCI_AttributeSet(attpack);
    }
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed adding attpack", &localrc);
      return ESMF_FAILURE;
    }
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed adding an attpack to an Attribute", &localrc);
      return ESMF_FAILURE;
    }
  }
  
  // make an Attribute in the new attpack
  attr = new ESMCI_Attribute(name, convention, purpose, object);  
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed initialized an attpack Attribute", &localrc);
    return ESMF_FAILURE;
  }
  
  // add the new Attribute to the new attpack
  localrc = attpack->ESMCI_AttributeSet(attr);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed adding an attpack Attribute", &localrc);
    return ESMF_FAILURE;
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttPackCreate()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttPackGet"
//BOPI
// !IROUTINE:  ESMCI_AttPackGet - get an attpack on an {\tt ESMCI_Attribute}
//
// !INTERFACE:
      ESMCI_Attribute *ESMCI_Attribute::ESMCI_AttPackGet(
// 
// !RETURN VALUE:
//    {\tt ESMCI_Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      const string &convention,             // in - Attribute convention to retrieve
      const string &purpose,                // in - Attribute purpose to retrieve
      const string &object) const {         // in - Attribute object type to retrieve
// !DESCRIPTION:
//    Get an attpack on an {\tt ESMCI_Attribute} given it's convention, 
//    purpose, and object type.
//
//EOPI

  unsigned int i;
  ESMCI_Attribute *attpack;
  
  attpack = NULL;

  // look for the attpack on this Attribute
  for (i=0; i<attrCount; i++) {
    // if this is the Attpack we're looking for
    if (convention.compare(attrList[i]->attrConvention) == 0 && 
        purpose.compare(attrList[i]->attrPurpose) == 0 &&
        object.compare(attrList[i]->attrObject) == 0 &&
        attrList[i]->attrPack == ESMF_TRUE) {
          return attrList[i];
        }
    // else if this is the head of a nested attpack hierarchy
    else if (attrList[i]->attrPackHead == ESMF_TRUE) {
      attpack = attrList[i]->ESMCI_AttPackGet(convention, purpose, object);
    }
  }

  return attpack;

}  // end ESMCI_AttPackGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttPackGetAttribute"
//BOPI
// !IROUTINE:  ESMCI_AttPackGetAttribute - get an {\tt ESMCI_Attribute} from an attpack
//
// !INTERFACE:
      ESMCI_Attribute *ESMCI_Attribute::ESMCI_AttPackGetAttribute(
// 
// !RETURN VALUE:
//    {\tt ESMCI_Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      const string &name,                   // in - Attribute name to retrieve
      const string &convention,             // in - Attribute convention to retrieve
      const string &purpose,                // in - Attribute purpose to retrieve
      const string &object) const {         // in - Attribute object type to retrieve)
// 
// !DESCRIPTION:
//     Get an {\tt ESMCI_Attribute} from an attpack given its name, convention, 
//     purpose, and object type.  This routine is assumed to be called on the 
//     Attribute package that holds the Attribute in question.
//
//EOPI

  unsigned int i;

  // look for the Attribute on this attpack
  for (i=0; i<attrCount; i++) {
    if (name.compare(attrList[i]->attrName) == 0 && 
        convention.compare(attrList[i]->attrConvention) == 0 &&
        purpose.compare(attrList[i]->attrPurpose) == 0 &&
        object.compare(attrList[i]->attrObject) == 0) {

      // if you get here, you found a match. 
      return attrList[i]; 
    }   
  }
  
  // you get here if no matches found
  return NULL;

}  // end ESMCI_AttPackGetAttribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttPackGetIndex"
//BOPI
// !IROUTINE:  ESMCI_AttPackGetIndex - get the index of an attpack on an 
//                                    {\tt ESMCI_Attribute}
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttPackGetIndex(
// 
// !RETURN VALUE:
//    {\tt ESMCI_Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      const string &convention,             // in - Attribute convention to retrieve
      const string &purpose,                // in - Attribute purpose to retrieve
      const string &object) const {         // in - Attribute object type to retrieve
// !DESCRIPTION:
//    Get an attpack on an {\tt ESMCI_Attribute} given it's convention, 
//    purpose, and object type.
//
//EOPI

  unsigned int i;

  // look for the attpack on this Attribute
  for (i=0; i<attrCount; i++) {
    if (convention.compare(attrList[i]->attrConvention) == 0 && 
        purpose.compare(attrList[i]->attrPurpose) == 0 &&
        object.compare(attrList[i]->attrObject) == 0 &&
        attrList[i]->attrPack == ESMF_TRUE) {
          return i;
    }
  }
 
  // if you got here, you did not find the attpack
  return -1;

}  // end ESMCI_AttPackGetIndex
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttPackGetNested"
//BOPI
// !IROUTINE:  ESMCI_AttPackGetNested - get head of lowest nested attpack
//
// !INTERFACE:
      ESMCI_Attribute *ESMCI_Attribute::ESMCI_AttPackGetNested(
// 
// !RETURN VALUE:
//    {\tt ESMCI_Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      bool &done) const {         // in - stop case
// !DESCRIPTION:
//    Recursive call to get the head of the lowest nested attpack. 
//
//EOPI

  unsigned int i;
  ESMCI_Attribute *attr;

  // look for another attpack, re-curse if found, return when done
  for (i=0; i<attrCount; i++) {
    if (attrList[i]->attrPackHead == ESMF_TRUE) {
          attr = attrList[i]->ESMCI_AttPackGetNested(done);
          return attr;
    }
  }
  
  // if not done, return this
  if (done) return attr;
  else {
    done = true;
    // cast away constness, just this once, to return the attr*
    return const_cast<ESMCI_Attribute*> (this);
  }

}  // end ESMCI_AttPackGetNested
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttPackIsPresent"
//BOPI
// !IROUTINE:  ESMCI_AttPackIsPresent - query an {\tt ESMCI_Attribute} for an attpack
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttPackIsPresent(
// 
// !RETURN VALUE:
//    Value of the present flag.
// 
// !ARGUMENTS:
      const string &name,                             // in - Attribute name
      const string &convention,                       // in - Attribute convention
      const string &purpose,                          // in - Attribute purpose
      const string &object,                           // in - Attribute object type
      ESMC_Logical *present ) const {         // in/out - the present flag
// 
// !DESCRIPTION:
//     Query an Attribute package for an {\tt ESMCI_Attribute} given its name, convention, 
//     purpose, and object type.
//
//EOPI

  unsigned int i;
  ESMCI_Attribute *attr, *attpack;

  // get the attpack
  attpack = ESMCI_AttPackGet(convention, purpose, object);
  if (!attpack) {
    *present = ESMF_FALSE;
    return ESMF_SUCCESS;
  }
  // get the attr on the attpack
  attr = attpack->ESMCI_AttPackGetAttribute(name, convention, purpose, object);
  if (!attr) *present = ESMF_FALSE;
  else *present = ESMF_TRUE;
  
  // return
  return ESMF_SUCCESS;

}  // end ESMCI_AttPackIsPresent
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttPackRemove"
//BOPI
// !IROUTINE:  ESMCI_AttPackRemove - Remove an {\tt ESMCI_Attribute} package
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttPackRemove(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - convention
      const string &purpose,                 // in - purpose
      const string &object) {                // in - object type to look for
// 
// !DESCRIPTION:
//     Remove an {\tt ESMCI_Attribute} package

//EOPI

  int localrc;
  char msgbuf[ESMF_MAXSTR];
  unsigned int i;
  ESMCI_Attribute *attpack, *nestedpack;
  bool stop = false;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
    
  // get the attpack
  attpack = ESMCI_AttPackGet(convention, purpose, object);
  if(!attpack) {
       sprintf(msgbuf, "Cannot find an Attribute package with:\nconvention = '%s'\npurpose = '%s'\nobject = '%s'\n",
                      convention.c_str(), purpose.c_str(), object.c_str());
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
      return ESMF_FAILURE;
  }
  
  int end = attpack->attrCount;
  int removed = 0;
  // remove all of the attributes in this package
  for (i=0; i<end; i++) {
    (attpack->attrList[i])->~ESMCI_Attribute();
    attpack->attrList.erase(attpack->attrList.begin() + i - removed);
    (attpack->attrCount)--;
    removed++;
  }
  
  // if the attpack is not empty at this point, we screwed up
  if (!(attpack->attrList.empty())) {
    sprintf(msgbuf, "failed removing entire attribute package, attrCount = %d",
      attpack->attrCount);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  msgbuf, &localrc);
    return ESMF_FAILURE;
  }
  
  // if this is nested, find the attPack holding it
  if (attpack->attrNested) {
    // unset the attrPackHead variable so GetNested finds the Attpack holding this one
    attpack->attrPackHead = ESMF_FALSE;
    // then get the last possible parent attpack 
    nestedpack = ESMCI_AttPackGetNested(stop);
  }
  // else this is it
  else nestedpack = this;
  // then find the index of the attpack we're removing
  int ind = nestedpack->ESMCI_AttPackGetIndex(convention, purpose, object);
  if (ind >= 0) {
    attpack->~ESMCI_Attribute();
    (nestedpack->attrList).erase((nestedpack->attrList).begin() + ind);
    nestedpack->attrCount--;
  }
  // else we screwed up
  else {
    sprintf(msgbuf, "failed removing the head of the attribute package");
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  msgbuf, &localrc);
    return ESMF_FAILURE;
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttPackRemove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttPackRemoveAttribute"
//BOPI
// !IROUTINE:  ESMCI_AttPackRemoveAttribute - Remove an {\tt ESMCI_Attribute} from
//                                            an {\tt ESMCI_Attribute} package
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttPackRemoveAttribute(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name
      const string &convention,              // in - convention
      const string &purpose,                 // in - purpose
      const string &object) {                // in - object type to look for
// 
// !DESCRIPTION:
//     Remove an {\tt ESMCI_Attribute} from an {\tt ESMCI_Attribute} package

//EOPI

  int localrc;
  char msgbuf[ESMF_MAXSTR];
  unsigned int i;
  ESMCI_Attribute *attpack, *nestedpack;
  bool done, stop;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // initialize the booleans
  done = false;
  stop = false;
  
  // get the attpack
  attpack = ESMCI_AttPackGet(convention, purpose, object);
  if(!attpack) {
       sprintf(msgbuf, "Cannot find an Attribute package with:\nconvention = '%s'\npurpose = '%s'\nobject = '%s'\n",
                      convention.c_str(), purpose.c_str(), object.c_str());
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
      return ESMF_FAILURE;
  }
  
  for (i=0; i<attpack->attrCount; i++) {
    if (name.compare(attpack->attrList[i]->attrName) == 0 &&
      convention.compare(attpack->attrList[i]->attrConvention) == 0 && 
      purpose.compare(attpack->attrList[i]->attrPurpose) == 0 &&
      object.compare(attpack->attrList[i]->attrObject) == 0) {
      // found a match, destroy it
      (attpack->attrList[i])->~ESMCI_Attribute();
      attpack->attrList.erase(attpack->attrList.begin() + i);
      (attpack->attrCount)--;
      done = true;
      break;
    }
  }
  
  if (!done) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "could not locate, ", &localrc);
    return ESMF_FAILURE;
  }

  if (attpack->attrCount == 0) {
    // to get rid of the attpack, we first change the name
    attpack->attrName = "\0";
    // then get the last possible parent attpack 
    nestedpack = ESMCI_AttPackGetNested(stop);
    // then find the index of the attpack we're removing
    int ind = ESMCI_AttPackGetIndex(convention, purpose, object);
    if (ind >= 0) {
      attpack->~ESMCI_Attribute();
      (nestedpack->attrList).erase((nestedpack->attrList).begin() + ind);
      nestedpack->attrCount--;
    }
    // else we screwed up
    else {
      sprintf(msgbuf, "failed removing the head of the attribute package");
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  msgbuf, &localrc);
      return ESMF_FAILURE;
    }
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttPackRemoveAttribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttPackSet"
//BOPI
// !IROUTINE:  ESMCI_AttPackSet() - set an {\tt ESMCI_Attribute} in an attpack
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttPackSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,             // in - Attribute name
      const ESMC_TypeKind &tk,       // in - typekind
      int count,              // in - item count
      void *value,            // in - Attribute value
      const string &convention,       // in - attpack convention
      const string &purpose,          // in - attpack purpose
      const string &object) {         // in - attpack object type
// 
// !DESCRIPTION:
//     Set the value for an {\tt ESMCI_Attribute} belonging to an attpack with  
//     convention, purpose, and object type.
//
//EOPI

  int localrc;
  char msgbuf[ESMF_MAXSTR];
  ESMCI_Attribute *attr;
  ESMCI_Attribute *attpack;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Find the attpack Attribute
  attpack = ESMCI_AttPackGet(convention, purpose, object);
  if(!attpack) {
       sprintf(msgbuf, "Cannot find an Attribute package with:\nconvention = '%s'\npurpose = '%s'\nobject = '%s'\n",
                      convention.c_str(), purpose.c_str(), object.c_str());
       printf(msgbuf);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
       return localrc;
  }
  
  attr = attpack->ESMCI_AttPackGetAttribute(name, convention, purpose, object);
  if (!attr) {
       sprintf(msgbuf, "This Attribute package does have an Attribute named '%s'\n", name.c_str());
       printf(msgbuf);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
       return localrc;
  }

  // Set the Attribute
  localrc = attr->ESMCI_AttrModifyValue(tk, count, value);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed modifying an attpack Attribute", &localrc);
    return ESMF_FAILURE;
  }
  
  // return
  if (localrc != ESMF_SUCCESS) return ESMF_FAILURE;
  return ESMF_SUCCESS;
  
}  // end ESMCI_AttPackSet()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeCopyAll"
//BOPI
// !IROUTINE:  ESMCI_AttributeCopyAll - copy {\tt ESMCI_Attributes} between objects 
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeCopyAll(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *source) {  // in - the source object
// 
// !DESCRIPTION:
//     All {\tt ESMCI_Attributes} associated with the source object are copied to the
//     destination object (*this).
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  *this = source->root;
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeCopyAll
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeCountTree"
//BOPI
// !IROUTINE:  ESMCI_AttributeCountTree - count objects in {\tt ESMCI_Attribute} hierarchy 
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeCountTree(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,                 // in - convention
      const string &purpose,                    // in - purpose
      const string &object,                     // in - object type to look for
      int &objCount,                    // inout - count of objects in tree
      int &objmaxattrCount ) const{     // inout - max count of attrs on objects in tree
// 
// !DESCRIPTION:
//     Count the number of objects in the {\tt ESMCI_Attribute} hierarchy 

//EOPI

  int localrc;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // If this is object matches, count it
  if (convention.compare(attrConvention) == 0 && 
      purpose.compare(attrPurpose) == 0 &&
      object.compare(attrObject) == 0 &&
      attrPack == ESMF_TRUE) {
    objCount++;
    if (objmaxattrCount < attrCount) objmaxattrCount = attrCount;
  }
  
  // Recurse the hierarchy
  for (int i = 0; i < attrCount; i++) {
    localrc = attrList[i]->ESMCI_AttributeCountTree(convention, purpose, object, 
      objCount, objmaxattrCount);
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeCountTree
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeCountTreeLens"
//BOPI
// !IROUTINE:  ESMCI_AttributeCountTreeLens - get lengths of {\tt ESMCI_Attribute} values
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeCountTreeLens(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,                 // in - convention
      const string &purpose,                    // in - purpose
      const string &object,                     // in - object type to look for
      int *attrLens,                    // inout - lengths of values
      vector<string> &attrNames ) const{         // inout - names to match values
// 
// !DESCRIPTION:
//     Find the length of {\tt ESMCI_Attribute} values and names to go with them.

//EOPI

  int localrc, len;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // If this is object matches
  if (convention.compare(attrConvention) == 0 && 
      purpose.compare(attrPurpose) == 0 &&
      object.compare(attrObject) == 0 &&
      attrPack == ESMF_TRUE) {
    for(unsigned int i=0; i<attrCount; i++) {
      // add name
      if (attrLens[i] == 0) {
        attrNames.push_back(attrList[i]->attrName);
      }
      // check name
      else if (attrLens[i] > 0) {
        if (attrNames[i].compare(attrList[i]->attrName) != 0) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "Attribute package name out of order", 
                             &localrc);
          return ESMF_FAILURE;
        }
      }
      else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "length < 0 = not good.", 
                             &localrc);
          return ESMF_FAILURE;
      }

      // add length
      if (attrList[i]->items > 1) {
        ESMC_LogDefault.Write("Write items > 1 - Not yet implemented\n",
          ESMC_LOG_INFO);
        attrLens[i] = 0;
      }
      else if (attrList[i]->items == 1) {
        if (attrList[i]->tk == ESMC_TYPEKIND_LOGICAL) {
          attrLens[i] = 8;
        }
        else if (attrList[i]->tk == ESMC_TYPEKIND_CHARACTER) {
          if ((attrList[i]->vcp.size())+3 > attrLens[i])
            attrLens[i] = (attrList[i]->vcp.size())+3;
        }
        else {
          ESMC_LogDefault.Write("working on counting digits",
            ESMC_LOG_INFO);
          //***FIXME*** here we do log10(number) + 4
          attrLens[i] = 10;
        }
      }
      
    }
  }
  
  // Recurse the hierarchy
  for (int i = 0; i < attrCount; i++) {
    localrc = attrList[i]->ESMCI_AttributeCountTreeLens(convention, purpose, object, 
      attrLens, attrNames);
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeCountTreeLens
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(int) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_I4 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I4} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I4) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind I4", &localrc);
        return ESMF_FAILURE;
    }
  
    // simple sanity checks
    if (attr->items != 1) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
      return ESMF_FAILURE;
    }

    *value = attr->vi;
  }

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(int)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(int *) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_I4> *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I4} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc, i;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I4) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind I4", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items <= 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vip;
  }

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(int *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(ESMC_I8) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_I8 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I8} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I8) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind I8", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    *value = attr->vtl;
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(ESMC_I8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(ESMC_I8 *) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_I8> *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I8} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc, i;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I8) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind I8", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items <= 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vlp;
  }

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(ESMC_I8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(ESMC_R4) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_R4 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R4} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R4) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind R4", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return localrc;
    }

    *value = attr->vf;
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(ESMC_R4)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(ESMC_R4 *) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_R4> *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R4} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc, i;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R4) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind R4", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items <= 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vfp;
  }

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(ESMC_R4 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(ESMC_R8) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_R8 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R8} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R8) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind R8", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    *value = attr->vd;
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(ESMC_R8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(ESMC_R8 *) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_R8> *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R8} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc, i;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  
  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R8) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind R8", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items <= 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vdp;
  }

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(ESMC_R8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(ESMC_Logical) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_Logical *value) const {   // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_Logical} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_LOGICAL) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind LOGICAL", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    *value = attr->vb;
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(ESMC_Logical)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(ESMC_Logical *) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      int *count,                    // out - number of values in list
      vector<ESMC_Logical> *value) const {   // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_Logical} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc, i;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_LOGICAL) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind LOGICAL", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items <= 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    if (count) 
      *count = attr->items;

    if (value) 
      *value = attr->vbp;
  }

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(ESMC_Logical *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(char) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,            // in - name of Attribute to retrieve
      string *value) const {   // out - Attribute value
// 
// !DESCRIPTION:
//    Get the value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind CHARACTER", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items != 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    *value = attr->vcp;
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(char)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(charlist) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,            // in - name of Attribute to retrieve
      vector<string> *value) const {   // out - Attribute values
// 
// !DESCRIPTION:
//    Get the value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not typekind CHARACTER", &localrc);
       return ESMF_FAILURE;
    }

    // simple sanity checks
    if (attr->items <= 1) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "Attribute not single value", &localrc);
       return ESMF_FAILURE;
    }

    *value = attr->vcpp;
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(charlist)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(name) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,           // in - name of Attribute to retrieve
      ESMC_TypeKind *tk,            // out - typekind
      int *itemCount) const {           // out - number of values in list
// 
// !DESCRIPTION:
//    Get the {\tt void *} value of an {\tt ESMCI_Attribute} by name.
//
//EOPI

  int localrc, i;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = ESMCI_AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    if (tk) 
      *tk = attr->tk;

    if (itemCount)
      *itemCount = attr->items; 
  }

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(name)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet(num) - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int num,                       // in - number of Attribute to retrieve
      string *name,                  // out - Attribute name
      ESMC_TypeKind *tk,             // out - typekind
      int *itemCount) const {            // out - number of values in list
// 
// !DESCRIPTION:
//    Get the {\tt void *} value of an {\tt ESMCI_Attribute} by number.
//
//EOPI

  int localrc, i;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (num < 0) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "Attribute number must be > 0", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = ESMCI_AttributeGet(num);
  if (!attr) {
    ESMC_LogDefault.Write("Attribute not found, using default value if present",
      ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    if (name) 
      *name = attr->attrName;

    if (tk) 
      *tk = attr->tk;

    if (itemCount)
      *itemCount = attr->items;
  }

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet(num)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      ESMCI_Attribute *ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMCI_Attribute} pointer or NULL on error exit.
// 
// !ARGUMENTS:
      const string &name) const {        // in - Attribute name to retrieve
// 
// !DESCRIPTION:
//    Get the name of an {\tt ESMCI_Attribute}.
//
//EOPI

  int i;

  for (i=0; i<attrCount; i++) {
      if (name.compare(attrList[i]->attrName))
          continue;

      // if you get here, you found a match. 
      return attrList[i]; 
  }   

  // you get here if no matches found
  return NULL;

}  // end ESMCI_AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet - get an {\tt ESMCI_Attribute} by number
//
// !INTERFACE:
      ESMCI_Attribute *ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int number) const {             // in - Attribute number
// 
// !DESCRIPTION:
//     Allows the caller to get {\tt ESMCI_Attributes} by number instead of by name.
//     This can be useful in iterating through all {\tt ESMCI_Attributes} in a loop.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];

  // simple sanity check
  if ((number < 0) || (number >= attrCount)) {
      sprintf(msgbuf, "Number = %d, attribute number must be  1 < N < %d\n", 
        number, attrCount);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, NULL);
      return NULL;
  }

  return attrList[number];

}  // end ESMCI_AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGet"
//BOPI
// !IROUTINE:  ESMCI_AttributeGet - get {\tt ESMCI_Attribute} from an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMCI_Attribute} pointer or NULL on error exit.
// 
// !ARGUMENTS:
      const string &name,               // in - Attribute name to retrieve
      int *lens,                // in - Atttribute char* lengths to retrieve
      int count) const {        // in - number of Attribute lengths to retrieve
// 
// !DESCRIPTION:
//    Get the lengths of the strings in an {\tt ESMCI_Attribute}.
//
//EOPI

  int size;
  unsigned int i;
  ESMCI_Attribute *attr;

  // look for the Attribute
  for (i=0; i<attrCount; i++) {
      if (name.compare(attrList[i]->attrName) == 0)
          break;  // found a match
  }   

  // grab the Attribute
  attr = attrList[i];
  
  // check that it is valid
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                                          "Attribute not found", NULL);
    return ESMF_FAILURE;
  }
  
  // check that this is a char Attribute
  if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                                          "Attribute is not a char", NULL);
    return ESMF_FAILURE;
  }
  
  // find the lengths of the strings on this Attribute
  for (i = 0; i < count; i++) {
    if (attr->items == 1)
      lens[i] = (attr->vcp).size();
    else if (attr->items > 1) {
      lens[i] = (attr->vcpp[i]).size();
    }
  }
    

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGetCount"
//BOPI
// !IROUTINE:  ESMCI_AttributeGetCount - get an the number of {\tt ESMCI_Attributes}
// 
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGetCount(
// 
// !RETURN VALUE:
//    number of {\tt ESMCI_Attributes} in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of {\tt ESMCI_Attributes} present
//
//EOPI

  return attrCount;

} // end ESMCI_AttributeGetCount
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGetItemCount"
//BOPI
// !IROUTINE:  ESMCI_AttributeGetItemCount - get the item count of this {\tt ESMCI_Attribute}
// 
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeGetItemCount(
// 
// !RETURN VALUE:
//    item count of this {\tt ESMCI_Attribute}
// 
// !ARGUMENTS:
      const string &name) const {       // in - name
// 
// !DESCRIPTION:
//      Returns number of items on this {\tt ESMCI_Attribute}
//
//EOPI

  ESMCI_Attribute *attr;

  attr = ESMCI_AttributeGet(name);
  if (!attr) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                                            "Attribute not found", NULL);
      return ESMF_FAILURE;
  }

  return attr->items;

} // end ESMCI_AttributeGetItemCount
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeIsPresent"
//BOPI
// !IROUTINE:  ESMCI_AttributeIsPresent - query for an {\tt ESMCI_Attribute}
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeIsPresent(
// 
// !RETURN VALUE:
//    Value of the present flag.
// 
// !ARGUMENTS:
      const string &name,                             // in - Attribute name
      ESMC_Logical *present) const {         // in/out - the present flag
// 
// !DESCRIPTION:
//     Query for an {\tt ESMCI_Attribute} given its name
//
//EOPI

  ESMCI_Attribute *attr;

  attr = ESMCI_AttributeGet(name);
  if (!attr) *present = ESMF_FALSE;
  else *present = ESMF_TRUE;
  
  // return
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeIsPresent
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeRemove"
//BOPI
// !IROUTINE:  ESMCI_AttributeRemove - Remove the {\tt ESMCI_Attribute}
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeRemove(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name) {                // in - name
// 
// !DESCRIPTION:
//     Remove the {\tt ESMCI_Attribute} 

//EOPI

  int localrc;
  unsigned int i, j;
  bool done=false;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  for (i=0; i<attrCount; i++) {
    if (name.compare(attrList[i]->attrName) == 0) {
      // found a match, destroy it
      attrList[i]->~ESMCI_Attribute();
      attrList.erase(attrList.begin() + i);
      attrCount--;
      done = true;
      break;
    }
  }
  
  if (!done) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "could not locate, ", &localrc);
    return ESMF_FAILURE;
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeRemove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMCI_Attribute *attr) {   // in - Attribute name, type, value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//     This version of set is used when the caller has already allocated
//     an {\tt ESMCI_Attribute} object and filled it, and the {\tt ESMCI_Attribute} 
//     is simply added to the list belonging to this object.
//
//EOPI

  int i, localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (!attr) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad Attribute object", &localrc);
       return ESMF_FAILURE;
  }

  // first, see if you are replacing an existing Attribute
  for (i=0; i<attrCount; i++) {
      if ((attr->attrName).compare(attrList[i]->attrName))
          continue;

      // FIXME: we might want an explicit flag saying that this is what
      // is wanted, instead of an error if a previous value not expected.

      // if you get here, you found a match.  replace previous copy.

      // FIXME: this should use destroy
      // delete old Attribute, including possibly freeing a list
      attrList[i]->~ESMCI_Attribute();

      attrList[i] = attr;
      return ESMF_SUCCESS;
  }   

  attrList.push_back(attr);   
  attrCount++;
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeSet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(int) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_I4 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I4} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_I4, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(int)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(int *) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of ints in list
      vector<ESMC_I4> *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I4} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_I4, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(int *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(ESMC_I8) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_I8 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I*} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_I8, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(ESMC_I8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(ESMC_I8 *) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of ints in list
      vector<ESMC_I8> *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I8} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_I8, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(ESMC_I8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(ESMC_R4) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_R4 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R4} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_R4, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(ESMC_R4)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(ESMC_R4 *) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of ESMC_R4s in list
      vector<ESMC_R4> *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R4} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_R4, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(ESMC_R4 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(ESMC_R8) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_R8 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R8} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_R8, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(ESMC_R8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(ESMC_R8 *) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of ESMC_R8s in list
      vector<ESMC_R8> *value) {        // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R8} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_R8, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(ESMC_R8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(ESMC_Logical) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_Logical value) {    // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_Logical} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_LOGICAL, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(ESMC_Logical)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(ESMC_Logical *) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      int count,               // in - number of logicals in list
      vector<ESMC_Logical> *value) {   // in - Attribute values
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_Logical} valueList of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_LOGICAL, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(ESMC_Logical *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(char) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,       // in - Attribute name
      string *value) {    // in - Attribute value
// 
// !DESCRIPTION:
//    Set the value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_CHARACTER, 1, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(char)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet(charlist) - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,       // in - Attribute name
      int count,               // in - number of strings in vector
      vector<string> *value) {    // in - Attribute value
// 
// !DESCRIPTION:
//    Set the value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, ESMC_TYPEKIND_CHARACTER, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet(charlist)
//-----------------------------------------------------------------------------
/*#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSet"
//BOPI
// !IROUTINE:  ESMCI_AttributeSet - set {\tt ESMCI_Attribute} on an ESMF type
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,           // in - Attribute name
      const ESMC_TypeKind &tk,     // in - typekind
      int count,            // in - number of values
      void *value) {        // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt void*} value of an {\tt ESMCI_Attribute}.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new ESMCI_Attribute(name, tk, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = ESMCI_AttributeSet(attr);

  return localrc;

}  // end ESMCI_AttributeSet*/
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSetLink"
//BOPI
// !IROUTINE:  ESMCI_AttributeSetLink - set a link in an {\tt ESMCI_Attribute} hierarchy
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSetLink(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *destination) {  // in/out destination Attribute to be linked
// !DESCRIPTION:
//     Set a link in an {\tt ESMCI_Attribute} hierarchy.
//
//EOPI

  int localrc;
  ESMCI_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
    
  for (unsigned int i=0; i<attrCount; i++) {
    if (attrList[i]->attrRoot) {
      if (destination->root.attrID == attrList[i]->attrID) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeSetLink tried to double set a link", &localrc);
        return ESMF_FAILURE;
      }
    }
  }
  
  attr = &(destination->root);
    
  attrList.push_back(attr);
  attrCount++;

  return ESMF_SUCCESS;

}  // end ESMCI_AttributeSetLink
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSetObjsInTree"
//BOPI
// !IROUTINE:  ESMCI_AttributeSetObjsInTree - set all objects in {\tt ESMCI_Attribute} hierarchy 
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeSetObjsInTree(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name
      const string &object,                  // in - object
      const ESMC_TypeKind &tk,              // in - typekind
      int count,                     // in - count
      void *value) {                 // in - value
// 
// !DESCRIPTION:
//     Set the objects in the {\tt ESMCI_Attribute} hierarchy 

//EOPI

  int localrc;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // If this is object matches, count it
  if (object.compare(attrObject) == 0 && 
      name.compare(attrName) == 0) {
    localrc = ESMCI_AttrModifyValue(tk, count, value);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeSetObjsInTree failed to set an Attribute", &localrc);
      return ESMF_FAILURE;
    }
  }
  
  // Recurse the hierarchy
  for (int i = 0; i < attrCount; i++) {
    localrc = attrList[i]->ESMCI_AttributeSetObjsInTree(object,name,tk,count,value);
  }
  
  return ESMF_SUCCESS;

}  // end ESMCI_AttributeSetObjsInTree
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// WRITE ROUTINES:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeWriteTab"
//BOPI
// !IROUTINE:  ESMCI_AttributeWriteTab - write Attributes in Tab delimited format
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeWriteTab(
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
//    Write the contents on an {\tt ESMCI_Attribute} hierarchy in Tab delimited format.  
//    Expected to be called internally.
//
//EOPI

  FILE* tab;
  char msgbuf[ESMF_MAXSTR];
  int localrc;
  int na,maxobjs,count;
  int *attrLens;
  vector<string> attrNames;
  
  maxobjs = 0;
  count = 0;
  na = 0;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
    // Open an XML file for writing
  sprintf(msgbuf,"%s.out",basename.c_str());
  if((tab=fopen(msgbuf,"w"))==NULL) {
    localrc = ESMF_FAILURE;
    sprintf(msgbuf,"Could not open the write file!");
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
    return ESMF_FAILURE;
  } 

  // determine the number of fields to write
  localrc = ESMCI_AttributeCountTree(convention, purpose, varobj, na, maxobjs);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "ESMCI_AttributeWriteTab failed counting max objs",
                                &localrc);
    fclose(tab);
    return ESMF_FAILURE;
  }
  
  // allocate the integer array of length maxobjs
  attrLens = new int[maxobjs];
  if (!attrLens) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "ESMCI_AttributeWriteTab failed allocating attrLens",
                                &localrc);
    fclose(tab);
    return ESMF_FAILURE;
  }
  for (unsigned int i=0; i<maxobjs; i++) attrLens[i] = 0;
  attrNames.reserve(maxobjs);
    
  // make a function to recurse the tree, find the max lengths, and compare names
  localrc = ESMCI_AttributeCountTreeLens(convention, purpose, varobj, attrLens,
    attrNames);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "ESMCI_AttributeWriteTab failed CountTreeLens",
                                &localrc);
    delete [] attrLens;
    attrNames.clear();
    fclose(tab);
    return ESMF_FAILURE;
  }

  // write the header
  sprintf(msgbuf, "Name: %s\t  Convention: %s\t  Purpose: %s\t\r\n\n",
    basename.c_str(),convention.c_str(),purpose.c_str());
  fprintf(tab,msgbuf);
  for (unsigned int i=0; i<maxobjs; i++) {
    sprintf(msgbuf, "%-*s\t",attrLens[i],attrNames[i].c_str());
    fprintf(tab,msgbuf);
  }
  sprintf(msgbuf, "\r\n");
  fprintf(tab,msgbuf);
    
 // recurse the Attribute hierarchy
  localrc = ESMCI_AttributeWriteTabrecurse(tab,convention,purpose,varobj,attrLens,
    attrNames,maxobjs,count);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "ESMCI_Attribute failed recursing in WriteTab", &localrc);
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

 } // end ESMCI_AttributeWriteTab
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeWriteTabrecurse"
//BOPI
// !IROUTINE:  ESMCI_AttributeWriteTabrecurse - write Attributes in Tab delimited format
//                                             recursive function
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeWriteTabrecurse(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      FILE *tab,                    //  in - file to write
      const string &convention,             //  in - convention
      const string &purpose,                //  in - purpose
      const string &varobj,                    //  in - variable object
      int *attrLens,                //  in - integer array of attribute lengths
      const vector<string> &attrNames,
      const int &maxattrs,                 //  in - max number of attributes in a package
      int &count) const{            //  inout - current count
//
// !DESCRIPTION:
//    Write the contents on an {\tt ESMCI_Attribute} hierarchy in Tab delimited format.  
//    Expected to be called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc;
  int tlen;
  unsigned int i;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  
  if (convention.compare(attrConvention) == 0 && 
      purpose.compare(attrPurpose) == 0 &&
      varobj.compare(attrObject) == 0) {
      for (i=0; i<maxattrs; i++) {
        if(attrName.compare(attrNames[i]) == 0)
          tlen = attrLens[i];
      }
      if (items == 1) {
        if (tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "%-*d\t",tlen,vi);
        else if (tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "%-*ld\t",tlen,vtl); 
        else if (tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "%-*f\t",tlen,vf);  
        else if (tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "%-*g\t",tlen,vd);  
        else if (tk == ESMC_TYPEKIND_LOGICAL) {
          if (vb == ESMF_TRUE) 
            sprintf(msgbuf, "%-*s\t",tlen,"true");
          else if (vb == ESMF_FALSE)
            sprintf(msgbuf, "%-*s\t",tlen,"false");
        }
        else if (tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "%-*s\t",tlen,vcp.c_str());
        else {
          sprintf(msgbuf, "%-*s\t",tlen,"N/A");
        }
        fprintf(tab,msgbuf);
      }
      else if (items > 1) { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        fprintf(tab,msgbuf);
      }
        if (attrName.compare(attrNames[maxattrs-1]) == 0) {
          sprintf(msgbuf, "\r\n");
          fprintf(tab,msgbuf);
        }
  }
  for(i=0;  i<attrCount; i++) {
      attrList[i]->ESMCI_AttributeWriteTabrecurse(tab,convention,purpose,varobj,
        attrLens,attrNames,maxattrs,count);
  }

  return ESMF_SUCCESS;

 } // end ESMCI_AttributeWriteTabrecurse
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeWriteXML"
//BOPI
// !IROUTINE:  ESMCI_AttributeWriteXML - Write contents of an {\tt ESMCI_Attribute} package
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeWriteXML(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      const string &convention,        //  in - convention
      const string &purpose,           //  in - purpose
      const string &object,            //  in - object
      const string &varobj,            //  in - variable object
      const string &basename) const{        //  in - basename
//
// !DESCRIPTION:
//    Print the contents of an {\tt ESMCI_Attribute}.  Expected to be
//    called internally.
//
//EOPI

  FILE* xml;
  char msgbuf[ESMF_MAXSTR];
  string modelcompname, fullname, version;
  ESMCI_Attribute *attpack, *attr;
  int localrc, stop, fldcount, na;
  ESMC_Logical presentflag;

  stop = 0;
  fldcount = 0;
  na = 0;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Open an XML file for writing
  sprintf(msgbuf,"%s.xml",basename.c_str());
  if((xml=fopen(msgbuf,"w"))==NULL) {
    localrc = ESMF_FAILURE;
    sprintf(msgbuf,"Could not open the xml file!");
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
    return ESMF_FAILURE;
  } 

//  *** THIS IS A HACK, DON'T LIKE IT, TEMPORARY UNTIL WRITE CLASS AVAILABLE ***
  if (object.compare("comp")==0) {
  attpack = ESMCI_AttPackGet(convention, purpose, object);
  if (!attpack) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed getting attpack in WriteXML", &localrc);
    return ESMF_FAILURE;
  }

  // get value of attribute 0 or set to N/A if not present
  localrc = attpack->ESMCI_AttributeIsPresent("name", &presentflag);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed finding an attribute", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
  if (presentflag == ESMF_SUCCESS) {
    attr = attpack->ESMCI_AttributeGet(0);
    modelcompname = attr->vcp;
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed getting attribute value", &localrc);
      fclose(xml);
      return ESMF_FAILURE;
    }
  }
  else {
    modelcompname="N/A";
  }
  
  // get value of attribute 1 or set to N/A if not present
  localrc = attpack->ESMCI_AttributeIsPresent("full_name", &presentflag);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed finding an attribute", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
  if (presentflag == ESMF_SUCCESS) {
    attr = attpack->ESMCI_AttributeGet(1);
    fullname = attr->vcp;
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed getting attribute value", &localrc);
      fclose(xml);
      return ESMF_FAILURE;
    }
  }
  else {
    fullname="N/A";
  }
  
  // get value of attribute 2 or set to N/A if not present
  localrc = attpack->ESMCI_AttributeIsPresent("version", &presentflag);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed finding an attribute", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
  if (presentflag == ESMF_SUCCESS) {
    attr = attpack->ESMCI_AttributeGet(2);
    version = attr->vcp;
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed getting attribute value", &localrc);
      fclose(xml);
      return ESMF_FAILURE;
    }
  }
  else {
    version="N/A";
  }
  
  // Write the XML file header
  sprintf(msgbuf,"<model_component name=\"%s\" full_name=\"%s\" version=\"%s\"\n",
    modelcompname.c_str(),fullname.c_str(),version.c_str());
  //printf(msgbuf);
  fprintf(xml,msgbuf);
  sprintf(msgbuf,"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
  //printf(msgbuf);
  fprintf(xml,msgbuf);
  sprintf(msgbuf,"xsi:schemaLocation=\"http://www.esmf.ucar.edu file:/esmf_model_component.xsd\"\n");
  //printf(msgbuf);
  fprintf(xml,msgbuf);
  sprintf(msgbuf,"xmlns=\"http://www.esmf.ucar.edu\">\n\n");
  //printf(msgbuf);
  fprintf(xml,msgbuf);
  }
// *** HACK ***
   
  // determine the number of fields to write
  localrc = ESMCI_AttributeCountTree(convention, purpose, varobj, stop, na);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "ESMCI_Attribute failed counting fields", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }

  // recurse the Attribute hierarchy
  localrc = ESMCI_AttributeWriteXMLrecurse(xml,convention,purpose,object,varobj,stop,fldcount);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "ESMCI_Attribute failed recursing in WriteTab", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }

  // close the file
  fclose(xml);

  return ESMF_SUCCESS;

 } // end ESMCI_AttributeWriteXML
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeWriteXMLrecurse"
//BOPI
// !IROUTINE:  ESMCI_AttributeWriteXMLrecurse - {\tt ESMCI_Attribute} hierarchy recurse write
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttributeWriteXMLrecurse(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      FILE *xml,               //  in - file pointer to write
      const string &convention,        //  in - convention
      const string &purpose,           //  in - purpose
      const string &object,            //  in - object
      const string &varobj,            //  in - variable object
      const int &stop,                //  in - stop case            
      int &fldcount) const{    //  in - field count
//
// !DESCRIPTION:
//    Write the contents of an {\tt ESMCI_Attribute}.  Expected to be
//    called internally.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // write the component level information
  if (convention.compare(attrConvention) == 0 && 
      purpose.compare(attrPurpose) == 0 &&
      object.compare(attrObject) == 0 &&
      attrPack == ESMF_TRUE) {
    for (unsigned int i=3;  i<attrCount; i++) { 
      sprintf(msgbuf,"<%s_set>\n",(attrList[i]->attrName).c_str());
      //printf(msgbuf);
      fprintf(xml,msgbuf);
      if (attrList[i]->items == 1) {
        if (attrList[i]->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "  <%s name=\"%d\" />\n",(attrList[i]->attrName).c_str(),attrList[i]->vi);
        else if (attrList[i]->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "  <%s name=\"%ld\" />\n",(attrList[i]->attrName).c_str(),attrList[i]->vtl); 
        else if (attrList[i]->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "  <%s name=\"%f\" />\n",(attrList[i]->attrName).c_str(),attrList[i]->vf);  
        else if (attrList[i]->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "  <%s name=\"%g\" />\n",(attrList[i]->attrName).c_str(),attrList[i]->vd);  
        else if (attrList[i]->tk == ESMC_TYPEKIND_LOGICAL) {
          if (attrList[i]->vb == ESMF_TRUE) 
            sprintf(msgbuf, "  <%s name=\"%s\" />\n",(attrList[i]->attrName).c_str(),"true");
          else if (attrList[i]->vb == ESMF_FALSE)
            sprintf(msgbuf, "  <%s name=\"%s\" />\n",(attrList[i]->attrName).c_str(),"false");
        }
        else if (attrList[i]->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "  <%s name=\"%s\" />\n",(attrList[i]->attrName).c_str(),
            (attrList[i]->vcp).c_str());
        else {
          sprintf(msgbuf, "  <%s name=\"%s\" />\n",(attrList[i]->attrName).c_str(),"N/A");
        }
      }
      else { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      }
      //printf(msgbuf);
      fprintf(xml,msgbuf);
      sprintf(msgbuf,"</%s_set>\n\n",(attrList[i]->attrName).c_str());
      //printf(msgbuf);
      fprintf(xml,msgbuf);
    }
  }
  
  // write the field level information
  if (convention.compare(attrConvention) == 0 && 
      purpose.compare(attrPurpose) == 0 &&
      varobj.compare(attrObject) == 0 &&
      attrPack == ESMF_TRUE) {
    if (fldcount == 0) {
      sprintf(msgbuf,"<variable_set>\n");
      //printf(msgbuf);
      fprintf(xml,msgbuf);
    }
    for (unsigned int i=0;  i<attrCount; i++) { 
      if (attrList[i]->items == 1) {
        if (i == 0) {
          sprintf(msgbuf,"  <variable ");
          //printf(msgbuf);
          fprintf(xml,msgbuf);
        }
        if (attrList[i]->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "%s=\"%d\" ",(attrList[i]->attrName).c_str(),attrList[i]->vi);
        else if (attrList[i]->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "%s=\"%ld\" ",(attrList[i]->attrName).c_str(),attrList[i]->vtl); 
        else if (attrList[i]->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "%s=\"%f\" ",(attrList[i]->attrName).c_str(),attrList[i]->vf);  
        else if (attrList[i]->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "%s=\"%g\" ",(attrList[i]->attrName).c_str(),attrList[i]->vd);  
        else if (attrList[i]->tk == ESMC_TYPEKIND_LOGICAL) {
          if (attrList[i]->vb == ESMF_TRUE) 
            sprintf(msgbuf, "%s=\"%s\" ",(attrList[i]->attrName).c_str(),"true");
          else if (attrList[i]->vb == ESMF_FALSE)
            sprintf(msgbuf, "%s=\"%s\" ",(attrList[i]->attrName).c_str(),"false");
        }
        else if (attrList[i]->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "%s=\"%s\" ",(attrList[i]->attrName).c_str(),
            (attrList[i]->vcp).c_str());
        else {
          sprintf(msgbuf, "%s=\"%s\" ",(attrList[i]->attrName).c_str(),"N/A");
        }
        //printf(msgbuf);
        fprintf(xml,msgbuf);
        if (i != attrCount-1) {
          sprintf(msgbuf,"\n            ");
          //printf(msgbuf);
          fprintf(xml,msgbuf);
        }
      }
      else if (attrList[i]->items == 1) { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      }
    }
    if (!attrList.empty()) {
      sprintf(msgbuf," />\n");
      //printf(msgbuf);
      fprintf(xml,msgbuf);
    }  
    fldcount++;
    if (fldcount == stop) {
      sprintf(msgbuf,"</variable_set>\n");
      //printf(msgbuf);
      fprintf(xml,msgbuf);
      fldcount++;
    }
  }
  
  // write the footer (using the fldcount+1 for now to show all fields written)
  if (fldcount == stop+1) {
    sprintf(msgbuf,"\n</model_component>\n");
    //printf(msgbuf);
    fprintf(xml,msgbuf);
    fldcount++;
  }
  
  // recurse through the Attribute hierarchy
  for(int i=0;  i<attrCount; i++) {
      attrList[i]->ESMCI_AttributeWriteXMLrecurse(xml,convention,purpose,object,
        varobj,stop,fldcount);
  }

  return ESMF_SUCCESS;

 } // end ESMCI_AttributeWriteXMLrecurse
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Print"
//BOPI
// !IROUTINE:  ESMCI_Attribute::ESMC_Print - Print the {\tt ESMCI_Attribute} contents
//
// !INTERFACE:
      int ESMCI_Attribute::ESMC_Print(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      void) const {                    // could add options at some point
// 
// !DESCRIPTION:
//     Print the contents of an {\tt ESMCI_Attribute} object
//
//EOPI
  int localrc;
  char msgbuf[ESMF_MAXSTR];

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // print name
  sprintf(msgbuf, "        name: %s\n",  attrName.c_str());
  printf(msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // print items if there are any
  if (items <= 0) {
      sprintf(msgbuf, "        value: \n");
      printf(msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  }

  if (items == 1) {
      sprintf(msgbuf, "        value: ");
      printf(msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
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
                 sprintf(msgbuf, "%s\n", vcp.c_str());
             else{ 
                 ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "unknown value", &localrc);
                 return localrc;
             }
      printf(msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  }

  if (items > 1) { 
      sprintf(msgbuf, "        %d items, values:\n", items);
      printf(msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
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
                } else if (tk == ESMC_TYPEKIND_CHARACTER) {
                    sprintf(msgbuf, "          \t item %d: %s\n", i, vcpp[i].c_str());
                } else{
                    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "          \t unknown value", &localrc);
                    return localrc;
                }
      printf(msgbuf);
      }
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  }

  // print convention
  sprintf(msgbuf, "        convention: %s\n",  attrConvention.c_str());
  printf(msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // print purpose
  sprintf(msgbuf, "        purpose: %s\n",  attrPurpose.c_str());
  printf(msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // print object
  sprintf(msgbuf, "        object: %s\n",  attrObject.c_str());
  printf(msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);

  sprintf(msgbuf, "        attrCount: %d\n", attrCount);
  printf(msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  for (int i=0; i<attrCount; i++) {
      sprintf(msgbuf, "   Attr %d:\n", i);
      printf(msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      attrList[i]->ESMC_Print();
  }

  return ESMF_SUCCESS;

}  // end ESMC_Print
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// Modifiers, Constructors, Destructors, Serializers, Print:
//
//-----------------------------------------------------------------------------
//----------------------------------------------------------------------------- 
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_Attribute()"
//BOPI
// !IROUTINE:  ESMCI_Attribute - native C++ constructor for ESMCI_Attribute class
//
// !INTERFACE:
      ESMCI_Attribute::ESMCI_Attribute(
//
// !RETURN VALUE:
//    new {\tt ESMCI_Attribute} object
//
// !ARGUMENTS:
        const string &conv,                  // convention
        const string &purp,                  // purpose
        const string &obj) {                 // object
//
// !DESCRIPTION:
//   Initialize an {\tt ESMCI_Attribute} and set the name, convention, and purpose.
//
//EOPI
  
  char name[ESMF_MAXSTR];
  
  tk = ESMF_NOKIND;
  items = 0;
  slen = 0;
  attrRoot = ESMF_FALSE;

  attrConvention = conv;
  attrPurpose = purp;
  attrObject = obj;
  attrPack = ESMF_TRUE;
  attrPackHead = ESMF_TRUE;
  attrNested = ESMF_FALSE;

  attrID = globalCount++;
  attrCount = 0;
  attrList.reserve(attrCount);

  // set name out of order so using attrID is thread-safe
  sprintf(name, "Attribute package - %d %s %s %s",attrID, 
  conv.c_str(), purp.c_str(), obj.c_str());
  attrName = name;

  vi = 0;
  vip.reserve(0);
  vtl = 0;
  vlp.reserve(0);
  vf = 0;
  vfp.reserve(0);
  vd = 0;
  vdp.reserve(0);
  vb = ESMF_FALSE;
  vbp.reserve(0);

} // end ESMCI_Attribute
//----------------------------------------------------------------------------- 
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_Attribute()"
//BOPI
// !IROUTINE:  ESMCI_Attribute - native C++ constructor for ESMCI_Attribute class
//
// !INTERFACE:
      ESMCI_Attribute::ESMCI_Attribute(
//
// !RETURN VALUE:
//    new {\tt ESMCI_Attribute} object
//
// !ARGUMENTS:
        const string &name,                  // Attribute name
        const string &conv,                  // convention
        const string &purp,                  // purpose
        const string &obj) {                 // object
//
// !DESCRIPTION:
//   Initialize an {\tt ESMCI_Attribute} and set the name, convention, and purpose.
//
//EOPI

  attrName = name;
  tk = ESMF_NOKIND;
  items = 0;
  slen = 0;
  attrRoot = ESMF_FALSE;

  attrConvention = conv;
  attrPurpose = purp;
  attrObject = obj;
  attrPack = ESMF_TRUE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  attrID = globalCount++;
  attrCount = 0;
  attrList.reserve(attrCount);

  vi = 0;
  vip.reserve(0);
  vtl = 0;
  vlp.reserve(0);
  vf = 0;
  vfp.reserve(0);
  vd = 0;
  vdp.reserve(0);
  vb = ESMF_FALSE;
  vbp.reserve(0);

} // end ESMCI_Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_Attribute()"
//BOPI
// !IROUTINE:  ESMCI_Attribute - native C++ constructor for ESMCI_Attribute class
//
// !INTERFACE:
      ESMCI_Attribute::ESMCI_Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     Create an empty {\tt ESMCI_Attribute} structure.
//
//EOPI

  attrName = '\0';
  tk = ESMF_NOKIND;
  items = 0;
  slen = 0;
  attrRoot = ESMF_TRUE;

  attrConvention = '\0';
  attrPurpose = '\0';
  attrObject = '\0';
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  attrID = globalCount++;
  attrCount = 0;
  attrList.reserve(attrCount);

  vi = 0;
  vip.reserve(0);
  vtl = 0;
  vlp.reserve(0);
  vf = 0;
  vfp.reserve(0);
  vd = 0;
  vdp.reserve(0);
  vb = ESMF_FALSE;
  vbp.reserve(0);
  
 } // end ESMCI_Attribute
//----------------------------------------------------------------------------- 
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_Attribute()"
//BOPI
// !IROUTINE:  ESMCI_Attribute - native C++ constructor for ESMCI_Attribute class
//
// !INTERFACE:
      ESMCI_Attribute::ESMCI_Attribute(
//
// !RETURN VALUE:
//    new {\tt ESMCI_Attribute} object
//
// !ARGUMENTS:
        const ESMC_Logical &attributeRoot) {                 // root value
//
// !DESCRIPTION:
//   Initialize an {\tt ESMCI_Attribute} and set the name, convention, and purpose.
//
//EOPI

  attrName = '\0';
  tk = ESMF_NOKIND;
  items = 0;
  slen = 0;
  attrRoot = attributeRoot;

  attrConvention = '\0';
  attrPurpose = '\0';
  attrObject = '\0';
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  attrID = globalCount++;
  attrCount = 0;
  attrList.reserve(attrCount);

  vi = 0;
  vip.reserve(0);
  vtl = 0;
  vlp.reserve(0);
  vf = 0;
  vfp.reserve(0);
  vd = 0;
  vdp.reserve(0);
  vb = ESMF_FALSE;
  vbp.reserve(0);

} // end ESMCI_Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_Attribute()"
//BOPI
// !IROUTINE:  ESMCI_Attribute - native C++ constructor for ESMCI_Attribute class
//
// !INTERFACE:
      ESMCI_Attribute::ESMCI_Attribute(
//
// !RETURN VALUE:
//    {\tt ESMCI_Attribute} object
//
// !ARGUMENTS:
        const string &name,                // Attribute name
        const ESMC_TypeKind &typekind,    // typekind
        int numitems,              // single or list
        void *datap) {             // generic pointer to values
//
// !DESCRIPTION:
//   Initialize an {\tt ESMCI_Attribute}, and make a copy of the data if items > 1.
//
//EOPI
  unsigned int i;

  attrName = name;
  tk = typekind;
  items = numitems;
  slen = 0;          // only used for string values
  attrRoot = ESMF_FALSE;
   
  attrConvention = '\0';
  attrPurpose = '\0';
  attrObject = '\0';
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  attrID = globalCount++;
  attrCount = 0;
  attrList.reserve(attrCount);
  
  vi = 0;
  vip.reserve(0);
  vtl = 0;
  vlp.reserve(0);
  vf = 0;
  vfp.reserve(0);
  vd = 0;
  vdp.reserve(0);
  vb = ESMF_FALSE;
  vbp.reserve(0);
 
  if (items == 1) {
      if (datap) {
            if (tk == ESMC_TYPEKIND_I4)
                vi = *(static_cast<ESMC_I4*> (datap));  
            else if (tk == ESMC_TYPEKIND_I8)
                vtl = *(static_cast<ESMC_I8*> (datap));  
            else if (tk == ESMC_TYPEKIND_R4)
                vf = *(static_cast<ESMC_R4*> (datap));  
            else if (tk == ESMC_TYPEKIND_R8)
                vd = *(static_cast<ESMC_R8*> (datap));  
            else if (tk == ESMC_TYPEKIND_LOGICAL)
                vb = *(static_cast<ESMC_Logical*> (datap));  
            else if (tk == ESMC_TYPEKIND_CHARACTER)
                vcp = *(static_cast<string*> (datap));
      }

  } else if (items > 1) {
    // items > 1, alloc space for a list and do the copy
        if (tk == ESMC_TYPEKIND_I4) {
            vip.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vip.push_back((*(static_cast<vector<ESMC_I4>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_I8) {
            vlp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vlp.push_back((*(static_cast<vector<ESMC_I8>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_R4) {
            vfp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vfp.push_back((*(static_cast<vector<ESMC_R4>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_R8) {
            vdp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vdp.push_back((*(static_cast<vector<ESMC_R8>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_LOGICAL) {
            vbp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vbp.push_back((*(static_cast<vector<ESMC_Logical>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_CHARACTER) {
            vcpp.reserve(items);
            if (datap) {
              for (i=0; i<items; i++) 
                vcpp.push_back((*(static_cast<vector<string>*> (datap)))[i]);
            }
        }
  }

 } // end ESMCI_Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttrModifyValue()"
//BOPI
// !IROUTINE:  ESMCI_AttrModifyValue - native C++ modifyer for ESMCI_Attribute class
//
// !INTERFACE:
      int ESMCI_Attribute::ESMCI_AttrModifyValue(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        const ESMC_TypeKind &typekind,    // typekind
        int numitems,              // single or list
        void *datap) {             // generic pointer to values
//
// !DESCRIPTION:
//   Set a value on an existing {\tt ESMCI_Attribute} object.
//
//EOPI
  int i, localrc;

  tk = typekind;
  items = numitems;
  slen = 0;          // only used for string values
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  if (items == 1) {
      if (datap) {
            if (tk == ESMC_TYPEKIND_I4)
                vi = *(static_cast<ESMC_I4*> (datap));  
            else if (tk == ESMC_TYPEKIND_I8)
                vtl = *(static_cast<ESMC_I8*> (datap));  
            else if (tk == ESMC_TYPEKIND_R4)
                vf = *(static_cast<ESMC_R4*> (datap));  
            else if (tk == ESMC_TYPEKIND_R8)
                vd = *(static_cast<ESMC_R8*> (datap));  
            else if (tk == ESMC_TYPEKIND_LOGICAL)
                vb = *(static_cast<ESMC_Logical*> (datap));  
            else if (tk == ESMC_TYPEKIND_CHARACTER)
                vcp = *(static_cast<string*> (datap));
      }

  } else if (items > 1) {
        if (tk == ESMC_TYPEKIND_I4) {
            vip.clear();
            vip.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vip.push_back((*(static_cast<vector<ESMC_I4>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_I8) {
            vlp.clear();
            vlp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vlp.push_back((*(static_cast<vector<ESMC_I8>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_R4) {
            vfp.clear();
            vfp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vfp.push_back((*(static_cast<vector<ESMC_R4>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_R8) {
            vdp.clear();
            vdp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vdp.push_back((*(static_cast<vector<ESMC_R8>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_LOGICAL) {
            vbp.clear();
            vbp.reserve(items);      
            if (datap) 
              for (i=0; i<items; i++)
                vbp.push_back((*(static_cast<vector<ESMC_Logical>*> (datap)))[i]);  
        } else if (tk == ESMC_TYPEKIND_CHARACTER) {
            vcpp.clear();
            vcpp.reserve(items);
            if (datap) {
              for (i=0; i<items; i++) 
                vcpp.push_back((*(static_cast<vector<string>*> (datap)))[i]);
            }
        }
  }

  return ESMF_SUCCESS;

 } // end ESMCI_AttrModifyValue
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeCopy(=)"
//BOPI
// !IROUTINE:  ESMCI_AttributeCopy(=) - assignment operator for {\tt ESMCI_Attribute}
//
// !INTERFACE:
      ESMCI_Attribute& ESMCI_Attribute::operator=(
//
// !RETURN VALUE:
//    Updated desination {\tt ESMCI_Attribute}
//
// !ARGUMENTS:
        const ESMCI_Attribute &source) {   // in - ESMCI_Attribute
//
// !DESCRIPTION:
//   Copy an {\tt ESMCI_Attribute}, including contents, to destination (this).
//
//EOPI
  int i, localrc;
  ESMCI_Attribute *attr;

  attrName = source.attrName;
  tk = source.tk;
  items = source.items;
  slen = source.slen;
  attrRoot = source.attrRoot;
  
  attrConvention = source.attrConvention;
  attrPurpose = source.attrPurpose;
  attrObject = source.attrObject;
  attrPack = source.attrPack;
  attrPackHead = source.attrPackHead;
  attrNested = source.attrNested;

  if (items == 1) {
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
        else if (tk == ESMC_TYPEKIND_CHARACTER)
            vcp = source.vcp;
  } else if (items > 1) {
    // items > 1, alloc space for a list and do the copy
          if (tk == ESMC_TYPEKIND_I4) {
              vip.clear();
              vip.reserve(items);
              vip = source.vip;
          } else if (tk == ESMC_TYPEKIND_I8) {
              vlp.clear();
              vlp.reserve(items);
              vlp = source.vlp;
          } else if (tk == ESMC_TYPEKIND_R4) {
              vfp.clear();
              vfp.reserve(items);
              vfp = source.vfp;
          } else if (tk == ESMC_TYPEKIND_R8) {
              vdp.clear();
              vdp.reserve(items);
              vdp = source.vdp;
          } else if (tk == ESMC_TYPEKIND_LOGICAL){
              vbp.clear();
              vbp.reserve(items);
              vbp = source.vbp;
          } else if (tk == ESMC_TYPEKIND_CHARACTER) {
              vcpp.clear();
              vcpp.reserve(items);
              vcpp = source.vcpp;
          }
  }

  // if Attribute list, copy it.
  for (i=0; i<source.attrCount; i++) {
    if(source.attrList[i]->attrRoot == ESMF_FALSE) {
      attr = new ESMCI_Attribute(ESMF_FALSE);
      *attr = *(source.attrList[i]);
      localrc = ESMCI_AttributeSet(attr);
    }
    else {
      attr = source.attrList[i];
      attrList.push_back(attr);
      attrCount++;
    }
  }

  return (*this);

 } // end ESMCI_Attribute::operator=
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMCI_Attribute()"
//BOPI
// !IROUTINE:  ~ESMCI_Attribute - native C++ destructor for ESMCI_Attribute class
//
// !INTERFACE:
      ESMCI_Attribute::~ESMCI_Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//    Delete an {\tt ESMCI_Attribute} hierarchy.
//
//EOPI

  if (items > 1) {
        if (tk == ESMC_TYPEKIND_I4) vip.clear();
        else if (tk == ESMC_TYPEKIND_I8) vlp.clear();
        else if (tk == ESMC_TYPEKIND_R4) vfp.clear();
        else if (tk == ESMC_TYPEKIND_R8) vdp.clear();  
        else if (tk == ESMC_TYPEKIND_LOGICAL) vbp.clear();
        else if (tk == ESMC_TYPEKIND_CHARACTER) vcpp.clear();
  }

  // if there are Attributes or attpacks delete, if links disconnect
  for (int i=0; i<attrCount; i++) {
    if (attrRoot == ESMF_TRUE) attrList[i] = ESMC_NULL_POINTER;
    else delete attrList[i];
  }

 } // end ~ESMCI_Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Deserialize"
//BOPI
// !IROUTINE:  ESMC_Deserialize - Turn a byte stream into an object
//
// !INTERFACE:
      int ESMCI_Attribute::ESMC_Deserialize(
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
//    Turn a stream of bytes into an {\tt ESMCI_Attribute} hierarchy.
//
//EOPI
    int loffset, nbytes, chars;
    int localrc;
    unsigned int i;
    
    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // Define serialization macros
#define DESERIALIZE_VAR(bufptr,loff,var,t) \
  var=(*(reinterpret_cast<t*> ((bufptr)+(loff))));    \
  loff += (sizeof(t));  

#define DESERIALIZE_VARC(bufptr,loff,var,var2,s) \
  string var2((bufptr)+(loff),s); \
  var = var2; \
  loff += s; \

    // get localoffset
    loffset=*offset;
    
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrName,temp,chars);

    DESERIALIZE_VAR(buffer,loffset,tk,ESMC_TypeKind);
    DESERIALIZE_VAR(buffer,loffset,items,int);
    DESERIALIZE_VAR(buffer,loffset,slen,int);
    DESERIALIZE_VAR(buffer,loffset,attrRoot,ESMC_Logical);
    
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrConvention,temp2,chars);
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrPurpose,temp3,chars);
    DESERIALIZE_VAR(buffer,loffset,chars,int);
    DESERIALIZE_VARC(buffer,loffset,attrObject,temp4,chars);
      
    DESERIALIZE_VAR(buffer,loffset,attrPack,ESMC_Logical);
    DESERIALIZE_VAR(buffer,loffset,attrPackHead,ESMC_Logical);
    DESERIALIZE_VAR(buffer,loffset,attrNested,ESMC_Logical);
    
    DESERIALIZE_VAR(buffer,loffset,attrCount,int);
        
    attrList.reserve(attrCount);

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
        DESERIALIZE_VARC(buffer,loffset,vcp,temp5,chars); 
      }
    }
    if (items > 1) { 
      if (tk == ESMC_TYPEKIND_I4) {
        vip.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_I4 vipTemp;
          vipTemp = (*(reinterpret_cast<ESMC_I4*> (buffer+loffset)));
          vip.push_back(vipTemp);
          loffset += sizeof(ESMC_I4);
        }}
      else if (tk == ESMC_TYPEKIND_I8) {
        vlp.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_I8 vlpTemp;
          vlpTemp = (*(reinterpret_cast<ESMC_I8*> (buffer+loffset)));
          vlp.push_back(vlpTemp);
          loffset += sizeof(ESMC_I8);
        }}
      else if (tk == ESMC_TYPEKIND_R4) {
        vfp.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_R4 vfpTemp;
          vfpTemp = (*(reinterpret_cast<ESMC_R4*> (buffer+loffset)));
          vfp.push_back(vfpTemp);
          loffset += sizeof(ESMC_R4);
        }}
      else if (tk == ESMC_TYPEKIND_R8) {
        vdp.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_R8 vdpTemp;
          vdpTemp = (*(reinterpret_cast<ESMC_R8*> (buffer+loffset)));
          vdp.push_back(vdpTemp);
          loffset += sizeof(ESMC_R8);
        }}
      else if (tk == ESMC_TYPEKIND_LOGICAL) {
        vbp.reserve(items);
        for (i=0; i<items; i++) {
          ESMC_Logical vbpTemp;
          vbpTemp = (*(reinterpret_cast<ESMC_Logical*> (buffer+loffset)));
          vbp.push_back(vbpTemp);
          loffset += sizeof(ESMC_Logical);
        }}
      else if (tk == ESMC_TYPEKIND_CHARACTER) {
          vcpp.reserve(items);
          for (i=0; i<items; i++) {
            DESERIALIZE_VAR(buffer,loffset,chars,int);
            string vcppTemp((buffer)+(loffset),chars);
            loffset += chars;
            vcpp.push_back(vcppTemp);
          }
        }
    }

    // make sure loffset is aligned correctly
    nbytes=loffset%8;
    if (nbytes!=0) loffset += 8-nbytes;  

    // Deserialize the {\tt ESMCI_Attribute} hierarchy
    for (int i=0; i<attrCount; i++) {
      attrList[i] = new ESMCI_Attribute(ESMF_FALSE);
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

   return ESMF_SUCCESS;

 } // end ESMC_Deserialize
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Serialize"
//BOPI
// !IROUTINE:  ESMC_Serialize - Turn the object information into a byte stream
//
// !INTERFACE:
      int ESMCI_Attribute::ESMC_Serialize(
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
//    Turn an {\tt ESMCI_Attribute} into a stream of bytes.
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
      int ESMCI_Attribute::ESMC_SerializeCC(
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
//    Turn an {\tt ESMCI_Attribute} into a stream of bytes.
//
//EOPI
    int nbytes;
    int localrc;
    unsigned int i;

    // Define serialization macros
#define SERIALIZE_VAR(cc,bufptr,loff,var,t) \
  if (cc) *(reinterpret_cast<t*> ((bufptr)+(loff)))=var;    \
  loff += (sizeof(t));   

#define SERIALIZE_VARC(cc,bufptr,loff,var,s) \
  if (cc) strncpy((bufptr)+(loff),(var).c_str(),s);      \
  loff += s;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

      SERIALIZE_VAR(cc,buffer,offset,(attrName.size()),int);
      SERIALIZE_VARC(cc,buffer,offset,attrName,(attrName.size()));

      SERIALIZE_VAR(cc,buffer,offset,tk,ESMC_TypeKind);
      
      SERIALIZE_VAR(cc,buffer,offset,items,int);
      SERIALIZE_VAR(cc,buffer,offset,slen,int);
      SERIALIZE_VAR(cc,buffer,offset,attrRoot,ESMC_Logical);
      
      SERIALIZE_VAR(cc,buffer,offset,(attrConvention.size()),int);
      SERIALIZE_VARC(cc,buffer,offset,attrConvention,(attrConvention.size()));
      SERIALIZE_VAR(cc,buffer,offset,(attrPurpose.size()),int);
      SERIALIZE_VARC(cc,buffer,offset,attrPurpose,(attrPurpose.size()));
      SERIALIZE_VAR(cc,buffer,offset,(attrObject.size()),int);
      SERIALIZE_VARC(cc,buffer,offset,attrObject,(attrObject.size()));
      
      SERIALIZE_VAR(cc,buffer,offset,attrPack,ESMC_Logical);
      SERIALIZE_VAR(cc,buffer,offset,attrPackHead,ESMC_Logical);
      SERIALIZE_VAR(cc,buffer,offset,attrNested,ESMC_Logical);
           
      SERIALIZE_VAR(cc,buffer,offset,attrCount,int);

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
          SERIALIZE_VAR(cc,buffer,offset,(vcp.size()),int);
          SERIALIZE_VARC(cc,buffer,offset,vcp,(vcp.size()));
        }
      }
      if (items > 1) { 
        if (tk == ESMC_TYPEKIND_I4) {
          for (i=0; i<items; i++) {
            SERIALIZE_VAR(cc,buffer,offset,vip[i],ESMC_I4);
          }}
        else if (tk == ESMC_TYPEKIND_I8) {
          for (i=0; i<items; i++) {
            SERIALIZE_VAR(cc,buffer,offset,vlp[i],ESMC_I8);
          }}
        else if (tk == ESMC_TYPEKIND_R4) {
          for (i=0; i<items; i++) {
            SERIALIZE_VAR(cc,buffer,offset,vfp[i],ESMC_R4);
          }}
        else if (tk == ESMC_TYPEKIND_R8) {
          for (i=0; i<items; i++) {
            SERIALIZE_VAR(cc,buffer,offset,vdp[i],ESMC_R8);
          }}
        else if (tk == ESMC_TYPEKIND_LOGICAL) {
          for (i=0; i<items; i++) {
            SERIALIZE_VAR(cc,buffer,offset,vbp[i],ESMC_Logical);
          }}
        else if (tk == ESMC_TYPEKIND_CHARACTER) {
          for (i=0; i<items; i++) {
            SERIALIZE_VAR(cc,buffer,offset,(vcpp[i].size()),int);
            SERIALIZE_VARC(cc,buffer,offset,vcpp[i],(vcpp[i].size())); 
          }
        }
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
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                      "Buffer too short to add an Attribute hierarchy", &localrc);
        return localrc;
      }
      
    // Undefine serialization macros, so they don't cause troubles elsewhere
#undef SERIALIZE_VAR
#undef SERIALIZE_VARC

  // return successfully
  return ESMF_SUCCESS;

 } // end ESMC_SerializeCC
//-----------------------------------------------------------------------------
/*#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeGetObjectList"
//BOPI
// !IROUTINE:  ESMCI_AttributeGetObjectList - get an {\tt ESMCI_Attribute} from multiple ESMF objects 
//
// !INTERFACE:
      int ESMCI_AttributeGetObjectList(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,            // in - list of ESMC objects
      string name,                        // in - Attribute name
      ESMCI_Attribute *valuelist) {       // out - list of Attribute values
// 
// !DESCRIPTION:
//     Get the same {\tt ESMCI_Attribute} name from multiple objects in one call.
//
//EOPI
    int localrc;

    // Initialize local return code ; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    return localrc;

}  // end ESMCI_AttributeGetObjectList
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI_AttributeSetObjectList"
//BOPI
// !IROUTINE:  ESMCI_AttributeSetObjectList - set an {\tt ESMCI_Attribute} on multiple ESMF objects
//
// !INTERFACE:
      int ESMCI_AttributeSetObjectList(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,    // in - list of ESMC objects
      ESMCI_Attribute *value) {   // in - Attribute value
// 
// !DESCRIPTION:
//     Set the same {\tt ESMCI_Attribute} on multiple objects in one call.
//
//EOPI

    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    return localrc;

}  // end ESMCI_AttributeSetObjectList
//-----------------------------------------------------------------------------
*/

} // namespace ESMCI