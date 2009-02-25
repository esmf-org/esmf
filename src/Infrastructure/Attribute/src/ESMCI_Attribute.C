// $Id: ESMCI_Attribute.C,v 1.15 2009/02/25 05:28:16 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMCI_Attribute.C"

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
//
 // associated class definition file and others
#include "ESMC_Start.h"
#include "ESMCI_Attribute.h"
#include "ESMC_Base.h"
#include "ESMCI_LogErr.h"
//#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_Attribute.C,v 1.15 2009/02/25 05:28:16 rokuingh Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Attribute routines
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PRIVATE:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute"
//BOPI
// !IROUTINE:  Attribute - empty private copy constructor
//
// !INTERFACE:
      Attribute::Attribute(
//
// !ARGUMENTS:
      const Attribute&) {
// 
// !RETURN VALUE:
//    {\tt Attribute} object.
// 
// !DESCRIPTION:
//    Empty private copy constructor.
//
//EOPI

}  // end Attribute

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PUBLIC:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackAddAttribute"
//BOPI
// !IROUTINE:  AttPackAddAttribute() - add an {\tt Attribute} to an attpack
//
// !INTERFACE:
      int Attribute::AttPackAddAttribute(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - Attribute name
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object) {                // in - Attribute object type 
// 
// !DESCRIPTION:
//     Add an {\tt Attribute} with a specified name but no value.
//
//EOPI

  int localrc;
  char msgbuf[ESMF_MAXSTR];
  Attribute *attr, *attpack;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Search for the attpack, make it if not found
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
       sprintf(msgbuf, "Cannot find an Attribute package with:\nconvention = '%s'\npurpose = '%s'\nobject = '%s'\n",
                      convention.c_str(), purpose.c_str(), object.c_str());
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
      return ESMF_FAILURE;
  }
  
  // make an Attribute in the new attpack
  attr = new Attribute(name, convention, purpose, object);  
  if (!attr) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed initialized an attpack Attribute", &localrc);
    return ESMF_FAILURE;
  }
  
  // add the new Attribute to the new attpack
  localrc = attpack->AttributeSet(attr);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed adding an attpack Attribute", &localrc);
    return ESMF_FAILURE;
  }
  
  return ESMF_SUCCESS;

}  // end AttPackAddAttribute()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackCreate"
//BOPI
// !IROUTINE:  AttPackCreate() - create an attpack
//
// !INTERFACE:
      int Attribute::AttPackCreate(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object,                  // in - Attribute object type
      const ESMC_AttPackNestFlag &flag) {    // in - flag to nest or not  
// 
// !DESCRIPTION:
//     Setup the name, convention and purpose of an attpack.
//
//EOPI

  int localrc;
  Attribute *attpack, *nestedpack;
  bool stop = false;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Search for the attpack, make it if not found
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
    // name the attribute package using convention, purpose, and object
    attpack = new Attribute(convention, purpose, object);
    if (!attpack) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed initializing an attpack", &localrc);
      return ESMF_FAILURE;
    }
    
    if (flag == ESMF_ATTPACKNEST_ON) {
      // look for the lowest down nested attpack to attach this new one to
      nestedpack = AttPackGetNested(stop);
    
      // set attrNested
      attpack->attrNested = ESMF_TRUE;
      // set the attpack on the nestedpack, here or elsewhere
      localrc = nestedpack->AttributeSet(attpack);
    }
    else if (flag == ESMF_ATTPACKNEST_OFF) {
      // set the attpack here
      localrc = AttributeSet(attpack);
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
    
  return ESMF_SUCCESS;

}  // end AttPackCreate()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGet"
//BOPI
// !IROUTINE:  AttPackGet - get an attpack on an {\tt Attribute}
//
// !INTERFACE:
      Attribute *Attribute::AttPackGet(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      const string &convention,             // in - Attribute convention to retrieve
      const string &purpose,                // in - Attribute purpose to retrieve
      const string &object) const {         // in - Attribute object type to retrieve
// !DESCRIPTION:
//    Get an attpack on an {\tt Attribute} given it's convention, 
//    purpose, and object type.
//
//EOPI

  unsigned int i;
  Attribute *attpack;
  
  attpack = NULL;

  // look for the attpack on this Attribute
  for (i=0; i<attrCount; i++) {
    // if this is the Attpack we're looking for
    if (convention.compare(attrList.at(i)->attrConvention) == 0 && 
        purpose.compare(attrList.at(i)->attrPurpose) == 0 &&
        object.compare(attrList.at(i)->attrObject) == 0 &&
        attrList.at(i)->attrPack == ESMF_TRUE) {
          return attrList.at(i);
        }
    // else if this is the head of a nested attpack hierarchy
    else if (attrList.at(i)->attrPackHead == ESMF_TRUE) {
      attpack = attrList.at(i)->AttPackGet(convention, purpose, object);
    }
  }

  return attpack;

}  // end AttPackGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGetAttribute"
//BOPI
// !IROUTINE:  AttPackGetAttribute - get an {\tt Attribute} from an attpack
//
// !INTERFACE:
      Attribute *Attribute::AttPackGetAttribute(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      const string &name,                   // in - Attribute name to retrieve
      const string &convention,             // in - Attribute convention to retrieve
      const string &purpose,                // in - Attribute purpose to retrieve
      const string &object) const {         // in - Attribute object type to retrieve)
// 
// !DESCRIPTION:
//     Get an {\tt Attribute} from an attpack given its name, convention, 
//     purpose, and object type.  This routine is assumed to be called on the 
//     Attribute package that holds the Attribute in question.
//
//EOPI

  unsigned int i;

  // look for the Attribute on this attpack
  for (i=0; i<attrCount; i++) {
    if (name.compare(attrList.at(i)->attrName) == 0 && 
        convention.compare(attrList.at(i)->attrConvention) == 0 &&
        purpose.compare(attrList.at(i)->attrPurpose) == 0 &&
        object.compare(attrList.at(i)->attrObject) == 0) {

      // if you get here, you found a match. 
      return attrList.at(i); 
    }   
  }
  
  // you get here if no matches found
  return NULL;

}  // end AttPackGetAttribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGetIndex"
//BOPI
// !IROUTINE:  AttPackGetIndex - get the index of an attpack on an 
//                                    {\tt Attribute}
//
// !INTERFACE:
      int Attribute::AttPackGetIndex(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      const string &convention,             // in - Attribute convention to retrieve
      const string &purpose,                // in - Attribute purpose to retrieve
      const string &object) const {         // in - Attribute object type to retrieve
// !DESCRIPTION:
//    Get an attpack on an {\tt Attribute} given it's convention, 
//    purpose, and object type.
//
//EOPI

  unsigned int i;

  // look for the attpack on this Attribute
  for (i=0; i<attrCount; i++) {
    if (convention.compare(attrList.at(i)->attrConvention) == 0 && 
        purpose.compare(attrList.at(i)->attrPurpose) == 0 &&
        object.compare(attrList.at(i)->attrObject) == 0 &&
        attrList.at(i)->attrPack == ESMF_TRUE) {
          return i;
    }
  }
 
  // if you got here, you did not find the attpack
  return -1;

}  // end AttPackGetIndex
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackGetNested"
//BOPI
// !IROUTINE:  AttPackGetNested - get head of lowest nested attpack
//
// !INTERFACE:
      Attribute *Attribute::AttPackGetNested(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer to requested object or NULL on early exit.
// 
// !ARGUMENTS:
      bool &done) const {         // in - stop case
// !DESCRIPTION:
//    Recursive call to get the head of the lowest nested attpack. 
//
//EOPI

  unsigned int i;
  Attribute *attr;

  // look for another attpack, re-curse if found, return when done
  for (i=0; i<attrCount; i++) {
    if (attrList.at(i)->attrPackHead == ESMF_TRUE) {
          attr = attrList.at(i)->AttPackGetNested(done);
          return attr;
    }
  }
  
  // if not done, return this
  if (done) return attr;
  else {
    done = true;
    // cast away constness, just this once, to return the attr*
    return const_cast<Attribute*> (this);
  }

}  // end AttPackGetNested
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackIsPresent"
//BOPI
// !IROUTINE:  AttPackIsPresent - query an {\tt Attribute} for an attpack
//
// !INTERFACE:
      int Attribute::AttPackIsPresent(
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
//     Query an Attribute package for an {\tt Attribute} given its name, convention, 
//     purpose, and object type.
//
//EOPI

  unsigned int i;
  Attribute *attr, *attpack;

  // get the attpack
  attpack = AttPackGet(convention, purpose, object);
  if (!attpack) {
    *present = ESMF_FALSE;
    return ESMF_SUCCESS;
  }
  // get the attr on the attpack
  attr = attpack->AttPackGetAttribute(name, convention, purpose, object);
  if (!attr) *present = ESMF_FALSE;
  else *present = ESMF_TRUE;
  
  // return
  return ESMF_SUCCESS;

}  // end AttPackIsPresent
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackRemove"
//BOPI
// !IROUTINE:  AttPackRemove - Remove an {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttPackRemove(
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
//     Remove an {\tt Attribute} package

//EOPI

  int localrc;
  char msgbuf[ESMF_MAXSTR];
  unsigned int i;
  Attribute *attpack, *nestedpack;
  bool stop = false;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
    
  // get the attpack
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
       sprintf(msgbuf, "Cannot find an Attribute package with:\nconvention = '%s'\npurpose = '%s'\nobject = '%s'\n",
                      convention.c_str(), purpose.c_str(), object.c_str());
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
      return ESMF_FAILURE;
  }
  
  // remove all Attributes in this Attribute package
  for (i=0; i<attpack->attrCount; i++) {
    delete (attpack->attrList[i]);
    attpack->attrList[i] = 0;
  }
  (attpack->attrList).erase(remove((attpack->attrList).begin(), (attpack->attrList).end(),
    static_cast<Attribute*>(0)),(attpack->attrList).end());
  attpack->attrCount = 0;
  attpack->structChange = ESMF_TRUE;
  
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
    nestedpack = AttPackGetNested(stop);
  }
  // else this is it
  else nestedpack = this;
  // then find the index of the attpack we're removing
  int ind = nestedpack->AttPackGetIndex(convention, purpose, object);
  if (ind >= 0) {
    delete attpack;
    (nestedpack->attrList).erase((nestedpack->attrList).begin() + ind);
    nestedpack->attrCount--;
    nestedpack->structChange = ESMF_TRUE;
  }
  // else we screwed up
  else {
    sprintf(msgbuf, "failed removing the head of the attribute package");
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  msgbuf, &localrc);
    return ESMF_FAILURE;
  }
  
  return ESMF_SUCCESS;

}  // end AttPackRemove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackRemoveAttribute"
//BOPI
// !IROUTINE:  AttPackRemoveAttribute - Remove an {\tt Attribute} from
//                                            an {\tt Attribute} package
//
// !INTERFACE:
      int Attribute::AttPackRemoveAttribute(
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
//     Remove an {\tt Attribute} from an {\tt Attribute} package

//EOPI

  int localrc;
  char msgbuf[ESMF_MAXSTR];
  unsigned int i;
  Attribute *attpack, *nestedpack;
  bool done, stop;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // initialize the booleans
  done = false;
  stop = false;
  
  // get the attpack
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
       sprintf(msgbuf, "Cannot find an Attribute package with:\nconvention = '%s'\npurpose = '%s'\nobject = '%s'\n",
                      convention.c_str(), purpose.c_str(), object.c_str());
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
      return ESMF_FAILURE;
  }
  
  for (i=0; i<attpack->attrCount; i++) {
    if (name.compare(attpack->attrList.at(i)->attrName) == 0 &&
      convention.compare(attpack->attrList.at(i)->attrConvention) == 0 && 
      purpose.compare(attpack->attrList.at(i)->attrPurpose) == 0 &&
      object.compare(attpack->attrList.at(i)->attrObject) == 0) {
      // found a match, destroy it
      delete (attpack->attrList.at(i));
      attpack->attrList.erase(attpack->attrList.begin() + i);
      (attpack->attrCount)--;
      attpack->structChange = ESMF_TRUE;
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
    nestedpack = AttPackGetNested(stop);
    // then find the index of the attpack we're removing
    int ind = AttPackGetIndex(convention, purpose, object);
    if (ind >= 0) {
      delete attpack;
      (nestedpack->attrList).erase((nestedpack->attrList).begin() + ind);
      nestedpack->attrCount--;
      nestedpack->structChange = ESMF_TRUE;
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

}  // end AttPackRemoveAttribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackSet"
//BOPI
// !IROUTINE:  AttPackSet() - set an {\tt Attribute} in an attpack
//
// !INTERFACE:
      int Attribute::AttPackSet(
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
//     Set the value for an {\tt Attribute} belonging to an attpack with  
//     convention, purpose, and object type.
//
//EOPI

  int localrc;
  char msgbuf[ESMF_MAXSTR];
  Attribute *attr;
  Attribute *attpack;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Find the attpack Attribute
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
       sprintf(msgbuf, "Cannot find an Attribute package with:\nconvention = '%s'\npurpose = '%s'\nobject = '%s'\n",
                      convention.c_str(), purpose.c_str(), object.c_str());
       printf(msgbuf);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
       return localrc;
  }
  
  attr = attpack->AttPackGetAttribute(name, convention, purpose, object);
  if (!attr) {
       sprintf(msgbuf, "This Attribute package does have an Attribute named '%s'\n", name.c_str());
       printf(msgbuf);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
       return localrc;
  }

  // Set the Attribute
  localrc = attr->AttrModifyValue(tk, count, value);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed modifying an attpack Attribute", &localrc);
    return ESMF_FAILURE;
  }
  
  // return
  if (localrc != ESMF_SUCCESS) return ESMF_FAILURE;
  return ESMF_SUCCESS;
  
}  // end AttPackSet()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCopy"
//BOPI
// !IROUTINE:  AttributeCopy - copy all {\tt Attribute} data 
//
// !INTERFACE:
      int Attribute::AttributeCopy(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
        const Attribute &source) {   // in - source
// 
// !DESCRIPTION:
//     All of the {\tt Attribute} data associated with the source object is 
//     copied.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // TODO: should check for self copy!!!

  // first clear destinations value arguments
  vi = 0;
  vip.clear();
  vtl = 0;
  vlp.clear();
  vf = 0;
  vfp.clear();
  vd = 0;
  vdp.clear();
  vb = ESMF_FALSE;
  vbp.clear();
  vcp = "";
  vcpp.clear();
  
  // now reset all Attribute info
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

  valueChange = ESMF_TRUE;

  if (source.items == 1) {
        if (source.tk == ESMC_TYPEKIND_I4)
            vi = source.vi;  
        else if (source.tk == ESMC_TYPEKIND_I8)
            vtl = source.vtl;  
        else if (source.tk == ESMC_TYPEKIND_R4)
            vf = source.vf;  
        else if (source.tk == ESMC_TYPEKIND_R8)
            vd = source.vd;  
        else if (source.tk == ESMC_TYPEKIND_LOGICAL)
            vb = source.vb;
        else if (source.tk == ESMC_TYPEKIND_CHARACTER)
            vcp = source.vcp;
  } else if (source.items > 1) {
    // items > 1, alloc space for a list and do the copy
          if (source.tk == ESMC_TYPEKIND_I4) {
              vip.reserve(source.items);
              vip = source.vip;
          } else if (source.tk == ESMC_TYPEKIND_I8) {
              vlp.reserve(source.items);
              vlp = source.vlp;
          } else if (source.tk == ESMC_TYPEKIND_R4) {
              vfp.reserve(source.items);
              vfp = source.vfp;
          } else if (source.tk == ESMC_TYPEKIND_R8) {
              vdp.reserve(source.items);
              vdp = source.vdp;
          } else if (source.tk == ESMC_TYPEKIND_LOGICAL){
              vbp.reserve(source.items);
              vbp = source.vbp;
          } else if (source.tk == ESMC_TYPEKIND_CHARACTER) {
              vcpp.reserve(source.items);
              vcpp = source.vcpp;
          }
  }

  return ESMF_SUCCESS;

}  // end AttributeCopy
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCopyHybrid"
//BOPI
// !IROUTINE:  AttributeCopyHybrid - copy {\tt Attributes} between ESMF objects
//
// !INTERFACE:
      int Attribute::AttributeCopyHybrid(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        const Attribute &source) {   // in - source
//
// !DESCRIPTION:
//   Copy all {\tt Attribute} data, copy by value all {\tt Attributes} in this
//   base level, and copy by reference all {\tt Attributes} in lower base levels.
//
//EOPI
  int i, localrc;
  Attribute *attr;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // call local copy on this Attribute 
  localrc = AttributeCopy(source);
  if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                          "AttributeCopyHybrid() bailed in AttributeCopy()", 
                          &localrc);
      return ESMF_FAILURE;
  }

  // if this base level copy by value, if not copy by value
  for (i=0; i<source.attrCount; i++) {
    if(source.attrList.at(i)->attrRoot != ESMF_TRUE) {
      attr = new Attribute(ESMF_FALSE);
      // set new attr to point to its intented destination and recurse
      (attr->attrBase) = (this->attrBase); 
      localrc = attr->AttributeCopyValue(*(source.attrList.at(i)));
      if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "AttributeCopyValue() bailed in recursion", 
                             &localrc);
          return ESMF_FAILURE;
      }
      localrc = AttributeSet(attr);
      if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "AttributeCopyHybrid() bailed in AttributeSet()", 
                             &localrc);
          return ESMF_FAILURE;
      }
      // update of count and flags is handled in AttributeSet()
    }
    else {
      attr = source.attrList.at(i);
      attrList.push_back(attr);
      // update count and flags
      attrCount++;
      structChange = ESMF_TRUE;
      linkChange = ESMF_TRUE;
    }
  }

  return ESMF_SUCCESS;

 } // end AttributeCopyHybrid
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCopyValue"
//BOPI
// !IROUTINE:  AttributeCopyValue - copy {\tt Attributes} between ESMF objects
//
// !INTERFACE:
      int Attribute::AttributeCopyValue(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        const Attribute &source) {   // in - source
//
// !DESCRIPTION:
//   Copy all {\tt Attribute} data and copy by value all {\tt Attributes} in 
//   this base level.
//
//EOPI
  int i, localrc;
  Attribute *attr;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // call local copy on source
  localrc = AttributeCopy(source);
  if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                          "AttributeCopyValue() bailed in AttributeCopy()", 
                          &localrc);
      return ESMF_FAILURE;
  }

  // copy base level Attributes by value
  for (i=0; i<source.attrCount; i++) {
    if(source.attrList.at(i)->attrRoot != ESMF_TRUE) {
      attr = new Attribute(ESMF_FALSE);
      // set new attr to point to its intented destination and recurse
      (attr->attrBase) = (this->attrBase); 
      localrc = attr->AttributeCopyValue(*(source.attrList.at(i)));
      if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "AttributeCopyValue() bailed in recursion", 
                             &localrc);
          return ESMF_FAILURE;
      }
      // add newly initialized attr to destination
      localrc = AttributeSet(attr);
      if (localrc != ESMF_SUCCESS) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "AttributeCopyValue() bailed in AttributeSet()", 
                             &localrc);
          return ESMF_FAILURE;
      }
      // update of count and flags is handled in AttributeSet()
    }
  }

  return ESMF_SUCCESS;

 } // end AttributeCopyValue
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCountTree"
//BOPI
// !IROUTINE:  AttributeCountTree - count objects in {\tt Attribute} hierarchy 
//
// !INTERFACE:
      int Attribute::AttributeCountTree(
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
//     Count the number of objects in the {\tt Attribute} hierarchy 

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
    localrc = attrList.at(i)->AttributeCountTree(convention, purpose, object, 
      objCount, objmaxattrCount);
  }
  
  return ESMF_SUCCESS;

}  // end AttributeCountTree
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCountTreeLens"
//BOPI
// !IROUTINE:  AttributeCountTreeLens - get lengths of {\tt Attribute} values
//
// !INTERFACE:
      int Attribute::AttributeCountTreeLens(
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
//     Find the length of {\tt Attribute} values and names to go with them.

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
        attrNames.push_back(attrList.at(i)->attrName);
      }
      // check name
      else if (attrLens[i] > 0) {
        if (attrNames[i].compare(attrList.at(i)->attrName) != 0) {
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
      if (attrList.at(i)->items > 1) {
        ESMC_LogDefault.Write("Write items > 1 - Not yet implemented\n",
          ESMC_LOG_INFO);
        attrLens[i] = 0;
      }
      else if (attrList.at(i)->items == 1) {
        if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
          attrLens[i] = 8;
        }
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER) {
          if ((attrList.at(i)->vcp.size())+3 > attrLens[i])
            attrLens[i] = (attrList.at(i)->vcp.size())+3;
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
    localrc = attrList.at(i)->AttributeCountTreeLens(convention, purpose, object, 
      attrLens, attrNames);
  }
  
  return ESMF_SUCCESS;

}  // end AttributeCountTreeLens
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(int) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_I4 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I4} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(int)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(int *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
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
//    Get the {\tt ESMC_I4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc, i;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(int *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_I8) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_I8 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_I8} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(ESMC_I8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_I8 *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
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
//    Get the {\tt ESMC_I8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc, i;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(ESMC_I8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_R4) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_R4 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R4} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(ESMC_R4)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_R4 *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
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
//    Get the {\tt ESMC_R4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc, i;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(ESMC_R4 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_R8) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_R8 *value) const {        // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_R8} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(ESMC_R8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_R8 *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
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
//    Get the {\tt ESMC_R8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc, i;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  
  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(ESMC_R8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_Logical) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,                    // in - name of Attribute to retrieve
      ESMC_Logical *value) const {   // out - Attribute value
// 
// !DESCRIPTION:
//    Get the {\tt ESMC_Logical} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(ESMC_Logical)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(ESMC_Logical *) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
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
//    Get the {\tt ESMC_Logical} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc, i;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(ESMC_Logical *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(char) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,            // in - name of Attribute to retrieve
      string *value) const {   // out - Attribute value
// 
// !DESCRIPTION:
//    Get the value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(char)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(charlist) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,            // in - name of Attribute to retrieve
      vector<string> *value) const {   // out - Attribute values
// 
// !DESCRIPTION:
//    Get the value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(charlist)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(name) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
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
//    Get the {\tt void *} value of an {\tt Attribute} by name.
//
//EOPI

  int localrc, i;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
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

}  // end AttributeGet(name)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet(num) - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
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
//    Get the {\tt void *} value of an {\tt Attribute} by number.
//
//EOPI

  int localrc, i;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (num < 0) {
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             "Attribute number must be > 0", &localrc);
       return ESMF_FAILURE;
  }

  // Get the attribute
  attr = AttributeGet(num);
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

}  // end AttributeGet(num)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      Attribute *Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer or NULL on error exit.
// 
// !ARGUMENTS:
      const string &name) const {        // in - Attribute name to retrieve
// 
// !DESCRIPTION:
//    Get the name of an {\tt Attribute}.
//
//EOPI

  int i;

  for (i=0; i<attrCount; i++) {
      if (name.compare(attrList.at(i)->attrName))
          continue;

      // if you get here, you found a match. 
      return attrList.at(i); 
  }   

  // you get here if no matches found
  return NULL;

}  // end AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet - get an {\tt Attribute} by number
//
// !INTERFACE:
      Attribute *Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int number) const {             // in - Attribute number
// 
// !DESCRIPTION:
//     Allows the caller to get {\tt Attributes} by number instead of by name.
//     This can be useful in iterating through all {\tt Attributes} in a loop.
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

  return attrList.at(number);

}  // end AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGet"
//BOPI
// !IROUTINE:  AttributeGet - get {\tt Attribute} from an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeGet(
// 
// !RETURN VALUE:
//    {\tt Attribute} pointer or NULL on error exit.
// 
// !ARGUMENTS:
      const string &name,               // in - Attribute name to retrieve
      int *lens,                // in - Atttribute char* lengths to retrieve
      int count) const {        // in - number of Attribute lengths to retrieve
// 
// !DESCRIPTION:
//    Get the lengths of the strings in an {\tt Attribute}.
//
//EOPI

  int size;
  unsigned int i;
  Attribute *attr;

  // look for the Attribute
  for (i=0; i<attrCount; i++) {
      if (name.compare(attrList.at(i)->attrName) == 0)
          break;  // found a match
  }   

  // grab the Attribute
  attr = attrList.at(i);
  
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

}  // end AttributeGet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetCount"
//BOPI
// !IROUTINE:  AttributeGetCount - get an the number of {\tt Attributes}
// 
// !INTERFACE:
      int Attribute::AttributeGetCount(
// 
// !RETURN VALUE:
//    number of {\tt Attributes} in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of {\tt Attributes} present
//
//EOPI

  return attrCount;

} // end AttributeGetCount
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetItemCount"
//BOPI
// !IROUTINE:  AttributeGetItemCount - get the item count of this {\tt Attribute}
// 
// !INTERFACE:
      int Attribute::AttributeGetItemCount(
// 
// !RETURN VALUE:
//    item count of this {\tt Attribute}
// 
// !ARGUMENTS:
      const string &name) const {       // in - name
// 
// !DESCRIPTION:
//      Returns number of items on this {\tt Attribute}
//
//EOPI

  Attribute *attr;

  attr = AttributeGet(name);
  if (!attr) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                                            "Attribute not found", NULL);
      return ESMF_FAILURE;
  }

  return attr->items;

} // end AttributeGetItemCount
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeIsPresent"
//BOPI
// !IROUTINE:  AttributeIsPresent - query for an {\tt Attribute}
//
// !INTERFACE:
      int Attribute::AttributeIsPresent(
// 
// !RETURN VALUE:
//    Value of the present flag.
// 
// !ARGUMENTS:
      const string &name,                             // in - Attribute name
      ESMC_Logical *present) const {         // in/out - the present flag
// 
// !DESCRIPTION:
//     Query for an {\tt Attribute} given its name
//
//EOPI

  Attribute *attr;

  attr = AttributeGet(name);
  if (!attr) *present = ESMF_FALSE;
  else *present = ESMF_TRUE;
  
  // return
  return ESMF_SUCCESS;

}  // end AttributeIsPresent
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeRemove"
//BOPI
// !IROUTINE:  AttributeRemove - Remove the {\tt Attribute}
//
// !INTERFACE:
      int Attribute::AttributeRemove(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name) {                // in - name
// 
// !DESCRIPTION:
//     Remove the {\tt Attribute} 

//EOPI

  int localrc;
  unsigned int i, j;
  bool done=false;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  for (i=0; i<attrCount; i++) {
    if (name.compare(attrList.at(i)->attrName) == 0) {
      // found a match, destroy it
      delete attrList.at(i);
      attrList.erase(attrList.begin() + i);
      attrCount--;
      structChange = ESMF_TRUE;
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

}  // end AttributeRemove
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      Attribute *attr) {   // in - Attribute name, type, value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//     This version of set is used when the caller has already allocated
//     an {\tt Attribute} object and filled it, and the {\tt Attribute} 
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
    if ((attr->attrName).compare(attrList.at(i)->attrName)==0 &&
        (attr->attrConvention).compare(attrList.at(i)->attrConvention)==0 &&
        (attr->attrPurpose).compare(attrList.at(i)->attrPurpose)==0 &&
        (attr->attrObject).compare(attrList.at(i)->attrObject)==0) {

      // FIXME: we might want an explicit flag saying that this is what
      // is wanted, instead of an error if a previous value not expected.

      // if you get here, you found a match.  replace previous copy.

      // FIXME: this should use destroy
      // delete old Attribute, including possibly freeing a list
      delete attrList.at(i);

      // replace the original Attribute with attr
      attrList.at(i) = attr;
      // attr may be of a different value than the original Attribute
      attr->valueChange = ESMF_TRUE;
      // point attr to its new Base
      attr->attrBase = this->attrBase; 
      return ESMF_SUCCESS;
    }
  }   

  attr->structChange = ESMF_TRUE;
  attr->attrBase = this->attrBase;
  attrList.push_back(attr);   
  attrCount++;
  structChange = ESMF_TRUE;
  
  return ESMF_SUCCESS;

}  // end AttributeSet
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(int) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_I4 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I4} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I4, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(int)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(int *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
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
//    Set the {\tt ESMC_I4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I4, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(int *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_I8) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_I8 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_I*} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I8, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(ESMC_I8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_I8 *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
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
//    Set the {\tt ESMC_I8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I8, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(ESMC_I8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_R4) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_R4 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R4} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R4, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(ESMC_R4)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_R4 *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
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
//    Set the {\tt ESMC_R4} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R4, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(ESMC_R4 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_R8) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_R8 value) {         // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_R8} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R8, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(ESMC_R8)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_R8 *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
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
//    Set the {\tt ESMC_R8} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R8, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(ESMC_R8 *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_Logical) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,              // in - Attribute name
      ESMC_Logical value) {    // in - Attribute value
// 
// !DESCRIPTION:
//    Set the {\tt ESMC_Logical} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_LOGICAL, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(ESMC_Logical)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(ESMC_Logical *) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
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
//    Set the {\tt ESMC_Logical} valueList of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_LOGICAL, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(ESMC_Logical *)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(char) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &name,       // in - Attribute name
      string *value) {    // in - Attribute value
// 
// !DESCRIPTION:
//    Set the value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_CHARACTER, 1, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(char)
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet(charlist) - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
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
//    Set the value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_CHARACTER, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet(charlist)
//-----------------------------------------------------------------------------
/*#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSet"
//BOPI
// !IROUTINE:  AttributeSet - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttributeSet(
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
//    Set the {\tt void*} value of an {\tt Attribute}.
//
//EOPI

  int localrc;
  Attribute *attr;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, tk, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  localrc = AttributeSet(attr);

  return localrc;

}  // end AttributeSet*/
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSetLink"
//BOPI
// !IROUTINE:  AttributeSetLink - set a link in an {\tt Attribute} hierarchy
//
// !INTERFACE:
      int Attribute::AttributeSetLink(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *destination) {  // in/out destination Attribute to be linked
// !DESCRIPTION:
//     Set a link in an {\tt Attribute} hierarchy.
//
//EOPI

  int localrc;
  Attribute *attr;
  char msgbuf[ESMF_MAXSTR];

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
    
  for (unsigned int i=0; i<attrCount; i++) {
    if (attrList.at(i)->attrRoot == ESMF_TRUE) {
      if (destination->ESMC_BaseGetID() == attrList.at(i)->attrBase->ESMC_BaseGetID()) {
        sprintf(msgbuf, "AttributeSetLink tried to double set a link SrcBase = %d, DestBase = %d",
                   attrList.at(i)->attrBase->ESMC_BaseGetID(), destination->ESMC_BaseGetID());
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        return ESMF_FAILURE;
      }
    }
  }
  
  attr = &(destination->root);
    
  attrList.push_back(attr);
  attrCount++;
  
  // now set linkChange
  linkChange = ESMF_TRUE;

  return ESMF_SUCCESS;

}  // end AttributeSetLink
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSetObjsInTree"
//BOPI
// !IROUTINE:  AttributeSetObjsInTree - set all objects in {\tt Attribute} hierarchy 
//
// !INTERFACE:
      int Attribute::AttributeSetObjsInTree(
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
//     Set the objects in the {\tt Attribute} hierarchy 

//EOPI

  int localrc;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // If this is object matches, count it
  if (object.compare(attrObject) == 0 && 
      name.compare(attrName) == 0) {
    localrc = AttrModifyValue(tk, count, value);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeSetObjsInTree failed to set an Attribute", &localrc);
      return ESMF_FAILURE;
    }
  }
  
  // Recurse the hierarchy
  for (int i = 0; i < attrCount; i++) {
    localrc = attrList.at(i)->AttributeSetObjsInTree(object,name,tk,count,value);
  }
  
  return ESMF_SUCCESS;

}  // end AttributeSetObjsInTree
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// WRITE ROUTINES:
//
//-----------------------------------------------------------------------------
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
  sprintf(msgbuf,"%s.stdout",basename.c_str());
  if((tab=fopen(msgbuf,"w"))==NULL) {
    localrc = ESMF_FAILURE;
    sprintf(msgbuf,"Could not open the write file!");
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
    return ESMF_FAILURE;
  } 

  // determine the number of fields to write
  localrc = AttributeCountTree(convention, purpose, varobj, na, maxobjs);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "AttributeWriteTab failed counting max objs",
                                &localrc);
    fclose(tab);
    return ESMF_FAILURE;
  }
  
  // allocate the integer array of length maxobjs
  attrLens = new int[maxobjs];
  if (!attrLens) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "AttributeWriteTab failed allocating attrLens",
                                &localrc);
    fclose(tab);
    return ESMF_FAILURE;
  }
  for (unsigned int i=0; i<maxobjs; i++) attrLens[i] = 0;
  attrNames.reserve(maxobjs);
    
  // make a function to recurse the tree, find the max lengths, and compare names
  localrc = AttributeCountTreeLens(convention, purpose, varobj, attrLens,
    attrNames);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "AttributeWriteTab failed CountTreeLens",
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
  localrc = AttributeWriteTabrecurse(tab,convention,purpose,varobj,attrLens,
    attrNames,maxobjs,count);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "Attribute failed recursing in WriteTab", &localrc);
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
#define ESMC_METHOD "AttributeWriteTabrecurse"
//BOPI
// !IROUTINE:  AttributeWriteTabrecurse - write Attributes in Tab delimited format
//                                             recursive function
//
// !INTERFACE:
      int Attribute::AttributeWriteTabrecurse(
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
//    Write the contents on an {\tt Attribute} hierarchy in Tab delimited format.  
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
      attrList.at(i)->AttributeWriteTabrecurse(tab,convention,purpose,varobj,
        attrLens,attrNames,maxattrs,count);
  }

  return ESMF_SUCCESS;

 } // end AttributeWriteTabrecurse
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
      const string &basename) const{        //  in - basename
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  FILE* xml;
  char msgbuf[ESMF_MAXSTR];
  string modelcompname, fullname, version;
  Attribute *attpack, *attr;
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
  attpack = AttPackGet(convention, purpose, object);
  if (!attpack) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed getting attpack in WriteXML", &localrc);
    return ESMF_FAILURE;
  }

  // get value of attribute 0 or set to N/A if not present
  localrc = attpack->AttributeIsPresent("name", &presentflag);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed finding an attribute", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
  if (presentflag == ESMF_SUCCESS) {
    attr = attpack->AttributeGet(0);
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
  localrc = attpack->AttributeIsPresent("full_name", &presentflag);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed finding an attribute", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
  if (presentflag == ESMF_SUCCESS) {
    attr = attpack->AttributeGet(1);
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
  localrc = attpack->AttributeIsPresent("version", &presentflag);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "failed finding an attribute", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
  if (presentflag == ESMF_SUCCESS) {
    attr = attpack->AttributeGet(2);
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
  localrc = AttributeCountTree(convention, purpose, varobj, stop, na);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "Attribute failed counting fields", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }

  // recurse the Attribute hierarchy
  localrc = AttributeWriteXMLrecurse(xml,convention,purpose,object,varobj,stop,fldcount);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                               "Attribute failed recursing in WriteTab", &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }

  // close the file
  fclose(xml);

  return ESMF_SUCCESS;

 } // end AttributeWriteXML
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeWriteXMLrecurse"
//BOPI
// !IROUTINE:  AttributeWriteXMLrecurse - {\tt Attribute} hierarchy recurse write
//
// !INTERFACE:
      int Attribute::AttributeWriteXMLrecurse(
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
//    Write the contents of an {\tt Attribute}.  Expected to be
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
      sprintf(msgbuf,"<%s_set>\n",(attrList.at(i)->attrName).c_str());
      //printf(msgbuf);
      fprintf(xml,msgbuf);
      if (attrList.at(i)->items == 1) {
        if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "  <%s name=\"%d\" />\n",(attrList.at(i)->attrName).c_str(),attrList.at(i)->vi);
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "  <%s name=\"%ld\" />\n",(attrList.at(i)->attrName).c_str(),attrList.at(i)->vtl); 
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "  <%s name=\"%f\" />\n",(attrList.at(i)->attrName).c_str(),attrList.at(i)->vf);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "  <%s name=\"%g\" />\n",(attrList.at(i)->attrName).c_str(),attrList.at(i)->vd);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
          if (attrList.at(i)->vb == ESMF_TRUE) 
            sprintf(msgbuf, "  <%s name=\"%s\" />\n",(attrList.at(i)->attrName).c_str(),"true");
          else if (attrList.at(i)->vb == ESMF_FALSE)
            sprintf(msgbuf, "  <%s name=\"%s\" />\n",(attrList.at(i)->attrName).c_str(),"false");
        }
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "  <%s name=\"%s\" />\n",(attrList.at(i)->attrName).c_str(),
            (attrList.at(i)->vcp).c_str());
        else {
          sprintf(msgbuf, "  <%s name=\"%s\" />\n",(attrList.at(i)->attrName).c_str(),"N/A");
        }
      }
      else { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      }
      //printf(msgbuf);
      fprintf(xml,msgbuf);
      sprintf(msgbuf,"</%s_set>\n\n",(attrList.at(i)->attrName).c_str());
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
      if (attrList.at(i)->items == 1) {
        if (i == 0) {
          sprintf(msgbuf,"  <variable ");
          //printf(msgbuf);
          fprintf(xml,msgbuf);
        }
        if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "%s=\"%d\" ",(attrList.at(i)->attrName).c_str(),attrList.at(i)->vi);
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "%s=\"%ld\" ",(attrList.at(i)->attrName).c_str(),attrList.at(i)->vtl); 
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "%s=\"%f\" ",(attrList.at(i)->attrName).c_str(),attrList.at(i)->vf);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "%s=\"%g\" ",(attrList.at(i)->attrName).c_str(),attrList.at(i)->vd);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
          if (attrList.at(i)->vb == ESMF_TRUE) 
            sprintf(msgbuf, "%s=\"%s\" ",(attrList.at(i)->attrName).c_str(),"true");
          else if (attrList.at(i)->vb == ESMF_FALSE)
            sprintf(msgbuf, "%s=\"%s\" ",(attrList.at(i)->attrName).c_str(),"false");
        }
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "%s=\"%s\" ",(attrList.at(i)->attrName).c_str(),
            (attrList.at(i)->vcp).c_str());
        else {
          sprintf(msgbuf, "%s=\"%s\" ",(attrList.at(i)->attrName).c_str(),"N/A");
        }
        //printf(msgbuf);
        fprintf(xml,msgbuf);
        if (i != attrCount-1) {
          sprintf(msgbuf,"\n            ");
          //printf(msgbuf);
          fprintf(xml,msgbuf);
        }
      }
      else if (attrList.at(i)->items == 1) { 
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
      attrList.at(i)->AttributeWriteXMLrecurse(xml,convention,purpose,object,
        varobj,stop,fldcount);
  }

  return ESMF_SUCCESS;

 } // end AttributeWriteXMLrecurse
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Print"
//BOPI
// !IROUTINE:  Attribute::ESMC_Print - Print the {\tt Attribute} contents
//
// !INTERFACE:
      int Attribute::ESMC_Print(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      void) const {                    // could add options at some point
// 
// !DESCRIPTION:
//     Print the contents of an {\tt Attribute} object
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
      attrList.at(i)->ESMC_Print();
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
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(
//
// !RETURN VALUE:
//    new {\tt Attribute} object
//
// !ARGUMENTS:
        const string &conv,                  // convention
        const string &purp,                  // purpose
        const string &obj) {                 // object
//
// !DESCRIPTION:
//   Initialize an {\tt Attribute} and set the name, convention, and purpose.
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
  
  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;

  attrBase = NULL;
  attrCount = 0;
  attrList.reserve(attrCount);

  sprintf(name, "Attribute package - %s %s %s", 
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

} // end Attribute
//----------------------------------------------------------------------------- 
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(
//
// !RETURN VALUE:
//    new {\tt Attribute} object
//
// !ARGUMENTS:
        const string &name,                  // Attribute name
        const string &conv,                  // convention
        const string &purp,                  // purpose
        const string &obj) {                 // object
//
// !DESCRIPTION:
//   Initialize an {\tt Attribute} and set the name, convention, and purpose.
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

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;

  attrBase = NULL;
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

} // end Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     Create an empty {\tt Attribute} structure.
//
//EOPI

  //attrName = '\0';
  tk = ESMF_NOKIND;
  items = 0;
  slen = 0;
  attrRoot = ESMF_TRUE;

  //attrConvention = '\0';
  //attrPurpose = '\0';
  //attrObject = '\0';
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;

  attrBase = NULL;
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
  
 } // end Attribute
//----------------------------------------------------------------------------- 
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(
//
// !RETURN VALUE:
//    new {\tt Attribute} object
//
// !ARGUMENTS:
        const ESMC_Logical &attributeRoot) {                 // root value
//
// !DESCRIPTION:
//   Initialize an {\tt Attribute} and set the name, convention, and purpose.
//
//EOPI

  //attrName = '\0';
  tk = ESMF_NOKIND;
  items = 0;
  slen = 0;
  attrRoot = attributeRoot;

  //attrConvention = '\0';
  //attrPurpose = '\0';
  //attrObject = '\0';
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;

  attrBase = NULL;
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
  

} // end Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "Attribute()"
//BOPI
// !IROUTINE:  Attribute - native C++ constructor for Attribute class
//
// !INTERFACE:
      Attribute::Attribute(
//
// !RETURN VALUE:
//    {\tt Attribute} object
//
// !ARGUMENTS:
        const string &name,                // Attribute name
        const ESMC_TypeKind &typekind,    // typekind
        int numitems,              // single or list
        void *datap) {             // generic pointer to values
//
// !DESCRIPTION:
//   Initialize an {\tt Attribute}, and make a copy of the data if items > 1.
//
//EOPI
  unsigned int i;

  attrName = name;
  tk = typekind;
  items = numitems;
  slen = 0;          // only used for string values
  attrRoot = ESMF_FALSE;
   
  //attrConvention = '\0';
  //attrPurpose = '\0';
  //attrObject = '\0';
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;

  attrBase = NULL;
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

 } // end Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttrModifyValue()"
//BOPI
// !IROUTINE:  AttrModifyValue - native C++ modifyer for Attribute class
//
// !INTERFACE:
      int Attribute::AttrModifyValue(
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
//   Set a value on an existing {\tt Attribute} object.
//
//EOPI
  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  if (numitems == 1) {
      if (datap) {
            if (typekind == ESMC_TYPEKIND_I4)
                vi = *(static_cast<ESMC_I4*> (datap));  
            else if (typekind == ESMC_TYPEKIND_I8)
                vtl = *(static_cast<ESMC_I8*> (datap));  
            else if (typekind == ESMC_TYPEKIND_R4)
                vf = *(static_cast<ESMC_R4*> (datap));  
            else if (typekind == ESMC_TYPEKIND_R8)
                vd = *(static_cast<ESMC_R8*> (datap));  
            else if (typekind == ESMC_TYPEKIND_LOGICAL)
                vb = *(static_cast<ESMC_Logical*> (datap));  
            else if (typekind == ESMC_TYPEKIND_CHARACTER)
                vcp = *(static_cast<string*> (datap));
      }

  } else if (numitems > 1) {
        if (typekind == ESMC_TYPEKIND_I4) {
            vip.clear();
            vip.reserve(numitems);      
            if (datap) 
              for (i=0; i<numitems; i++)
                vip.push_back((*(static_cast<vector<ESMC_I4>*> (datap)))[i]);  
        } else if (typekind == ESMC_TYPEKIND_I8) {
            vlp.clear();
            vlp.reserve(numitems);      
            if (datap) 
              for (i=0; i<numitems; i++)
                vlp.push_back((*(static_cast<vector<ESMC_I8>*> (datap)))[i]);  
        } else if (typekind == ESMC_TYPEKIND_R4) {
            vfp.clear();
            vfp.reserve(numitems);      
            if (datap) 
              for (i=0; i<numitems; i++)
                vfp.push_back((*(static_cast<vector<ESMC_R4>*> (datap)))[i]);  
        } else if (typekind == ESMC_TYPEKIND_R8) {
            vdp.clear();
            vdp.reserve(numitems);      
            if (datap) 
              for (i=0; i<numitems; i++)
                vdp.push_back((*(static_cast<vector<ESMC_R8>*> (datap)))[i]);  
        } else if (typekind == ESMC_TYPEKIND_LOGICAL) {
            vbp.clear();
            vbp.reserve(numitems);      
            if (datap) 
              for (i=0; i<numitems; i++)
                vbp.push_back((*(static_cast<vector<ESMC_Logical>*> (datap)))[i]);  
        } else if (typekind == ESMC_TYPEKIND_CHARACTER) {
            vcpp.clear();
            vcpp.reserve(numitems);
            if (datap) {
              for (i=0; i<numitems; i++) 
                vcpp.push_back((*(static_cast<vector<string>*> (datap)))[i]);
            }
        }
  }
 
  // if a change was made, note the new values
  if (numitems >= 1) {
    tk = typekind;
    items = numitems;
    slen = 0;
    valueChange = ESMF_TRUE;
  }

  return ESMF_SUCCESS;

 } // end AttrModifyValue
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeOperator="
//BOPI
// !IROUTINE:  Attribute - generic operator=
//
// !INTERFACE:
      Attribute& Attribute::operator=(
//
// !ARGUMENTS:
      const Attribute& source) {
// 
// !RETURN VALUE:
//    {\tt Attribute} object.
// 
// !DESCRIPTION:
//    Generic operator= call AttributeCopyValue.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  localrc = AttributeCopyValue(source);
  if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
                          "AttributeOperator= bailed in AttributeCopyValue()", 
                          &localrc);
  }
  
  return (*this);

}  // end AttributeOperator=
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~Attribute()"
//BOPI
// !IROUTINE:  ~Attribute - native C++ destructor for Attribute class
//
// !INTERFACE:
      Attribute::~Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//    Delete an {\tt Attribute} hierarchy.
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
    if (attrList.at(i)->attrRoot == ESMF_TRUE) attrList.at(i) = ESMC_NULL_POINTER;
    else delete attrList.at(i);
  }

 } // end ~Attribute
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Deserialize"
//BOPI
// !IROUTINE:  ESMC_Deserialize - Turn a byte stream into an object
//
// !INTERFACE:
      int Attribute::ESMC_Deserialize(
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
//    Turn a stream of bytes into an {\tt Attribute} hierarchy.
//
//EOPI
    int loffset, nbytes, chars;
    int localrc;
    unsigned int i;
    Attribute *attr;
    
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

    // Deserialize the {\tt Attribute} hierarchy
    int numattrs = attrCount;
    attrCount = 0;
    for (int i=0; i<numattrs; i++) {
      attr = new Attribute(ESMF_FALSE);
      if (!attr)
        return ESMF_FAILURE;
      attr->setBase(attrBase);
      localrc = attr->ESMC_Deserialize(buffer,&loffset);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeDeserialize failed deserializing next attr", &localrc);
        return ESMF_FAILURE;
      }
      localrc = AttributeSet(attr);
      if (localrc != ESMF_SUCCESS) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
                  "AttributeDeserialize failed adding attribute", &localrc);
        return ESMF_FAILURE;
      }
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
      int Attribute::ESMC_Serialize(
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
//    Turn an {\tt Attribute} into a stream of bytes.
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
      int Attribute::ESMC_SerializeCC(
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
//    Turn an {\tt Attribute} into a stream of bytes.
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
          
      // we don't serialize through links, so we must compute attrCount - linkAttrs
      int realCount = 0;
      for (i=0; i<attrCount; ++i)
        if (attrList.at(i)->attrRoot == ESMF_FALSE) ++realCount;

      SERIALIZE_VAR(cc,buffer,offset,realCount,int);

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
      for (i=0; i<attrCount; i++) {
        if (attrList.at(i)->attrRoot==ESMF_FALSE) 
          attrList.at(i)->ESMC_SerializeCC(buffer,length,offset,cc);
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
#define ESMC_METHOD "AttributeGetObjectList"
//BOPI
// !IROUTINE:  AttributeGetObjectList - get an {\tt Attribute} from multiple ESMF objects 
//
// !INTERFACE:
      int AttributeGetObjectList(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,            // in - list of ESMC objects
      string name,                        // in - Attribute name
      Attribute *valuelist) {       // out - list of Attribute values
// 
// !DESCRIPTION:
//     Get the same {\tt Attribute} name from multiple objects in one call.
//
//EOPI
    int localrc;

    // Initialize local return code ; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    return localrc;

}  // end AttributeGetObjectList
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeSetObjectList"
//BOPI
// !IROUTINE:  AttributeSetObjectList - set an {\tt Attribute} on multiple ESMF objects
//
// !INTERFACE:
      int AttributeSetObjectList(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,    // in - list of ESMC objects
      Attribute *value) {   // in - Attribute value
// 
// !DESCRIPTION:
//     Set the same {\tt Attribute} on multiple objects in one call.
//
//EOPI

    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    return localrc;

}  // end AttributeSetObjectList
//-----------------------------------------------------------------------------
*/

} // namespace ESMCI
