// $Id: ESMCI_Attribute.C,v 1.50.2.2 2010/02/05 19:53:35 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
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
#include "ESMF_LogMacros.inc"
#include "ESMCI_IO_XML.h"
//#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_Attribute.C,v 1.50.2.2 2010/02/05 19:53:35 svasquez Exp $";
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
  Attribute *attr, *attpack;

  attr = NULL; attpack = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Search for the attpack, make it if not found
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Cannot find the specified Attribute package\n", &localrc);
      return localrc;
  }
  
  // make an Attribute in the new attpack
  attr = new Attribute(name, convention, purpose, object);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "failed initialized an attpack Attribute", &localrc);
    return localrc;
  }
  
  // add the new Attribute to the new attpack
  localrc = attpack->AttributeSet(attr);
  if (localrc != ESMF_SUCCESS) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_NOTSET,
      "failed adding an attpack Attribute", &localrc);
    return localrc;
  }
  
  return ESMF_SUCCESS;

}  // end AttPackAddAttribute()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackCreateCustom"
//BOPI
// !IROUTINE:  AttPackCreateCustom() - create an attpack
//
// !INTERFACE:
      int Attribute::AttPackCreateCustom(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object) {                // in - Attribute object type
// 
// !DESCRIPTION:
//     Setup the name, convention and purpose of an attpack.
//
//EOPI

  int localrc;
  Attribute *attpack;
  
  attpack = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Search for the attpack, make it if not found
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
    // name the attribute package using convention, purpose, and object
    attpack = new Attribute(convention, purpose, object);
    if (!attpack) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
        "failed initializing an attpack", &localrc);
      return localrc;
    }

    localrc = AttPackSet(attpack);
    if (localrc != ESMF_SUCCESS) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_NOTSET,
        "failed adding an attpack to an Attribute", &localrc);
      return localrc;
    }
  }
    
  return ESMF_SUCCESS;

}  // end AttPackCreateCustom()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackCreateStandard"
//BOPI
// !IROUTINE:  AttPackCreateStandard() - create an attpack
//
// !INTERFACE:
      int Attribute::AttPackCreateStandard(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object) {                // in - Attribute object type
// 
// !DESCRIPTION:
//     Setup the name, convention and purpose of an attpack.
//
//EOPI

  int localrc;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Grid standard Attribute package
  if (object.compare("grid")==0) {
    localrc = AttPackCreateCustom("GridSpec", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackNest("ESMF", "General", object, "GridSpec", "General");
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    // TODO: CongruentTiles & GridType will be at the mosaic level,
    //       others at the tile level
    localrc = AttPackAddAttribute("CongruentTiles", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("GridType", "GridSpec", "General", object);

    // TODO: Area & Coordinatepoles await further spec from Sylvia Murphy & Co.
    //localrc = AttPackAddAttribute("Area", "GridSpec", "General", object);
    //localrc = AttPackAddAttribute("CoordinatePoles", "GridSpec", "General", object);

    localrc = AttPackAddAttribute("DimOrder", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("DiscretizationType", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("GeometryType", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("IsConformal", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("IsPoleCovered", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("IsRegular", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("IsUniform", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("NorthPoleLocation", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("NumberOfCells", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("NumDims", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("NX", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("NY", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("NZ", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("Resolution", "GridSpec", "General", object);
    localrc = AttPackAddAttribute("RegDecompX", "ESMF", "General", object);
    localrc = AttPackAddAttribute("RegDecompY", "ESMF", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;

  } else if (object.compare("field")==0 || object.compare("array")==0) {
  // Field standard Attribute package
    localrc = AttPackCreateCustom("CF", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackNest("CF", "Extended", object, "CF", "General");
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackNest("ESG", "General", object, "CF", "Extended");
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackNest("ESMF", "General", object, "ESG", "General");
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackAddAttribute("LongName", "CF", "General", object);
    localrc = AttPackAddAttribute("Name", "CF", "General", object);
    localrc = AttPackAddAttribute("Units", "CF", "General", object);
    localrc = AttPackAddAttribute("StandardName", "CF", "Extended", object);
    localrc = AttPackAddAttribute("Export", "ESG", "General", object);
    localrc = AttPackAddAttribute("Import", "ESG", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;

  } else if (object.compare("state")==0) {
  // State standard Attribute package
    localrc = AttPackCreateCustom("ESMF", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackAddAttribute("Export", "ESMF", "General", object);
    localrc = AttPackAddAttribute("Import", "ESMF", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  } else if (object.compare("comp")==0) {
  // Component standard Attribute package
    localrc = AttPackCreateCustom("CF", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackNest("ESG", "General", object, "CF", "General");
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackNest("ESMF", "General", object, "ESG", "General");
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackAddAttribute("References", "CF", "General", object);
    localrc = AttPackAddAttribute("Comment", "CF", "General", object);
    localrc = AttPackAddAttribute("Version", "ESG", "General", object);
    localrc = AttPackAddAttribute("PhysicalDomain", "ESG", "General", object);
    localrc = AttPackAddAttribute("Name", "ESG", "General", object);
    localrc = AttPackAddAttribute("ModelComponentFramework", "ESG", "General", object);
    localrc = AttPackAddAttribute("Institution", "ESG", "General", object);
    localrc = AttPackAddAttribute("FullName", "ESG", "General", object);
    localrc = AttPackAddAttribute("Discipline", "ESG", "General", object);
    localrc = AttPackAddAttribute("CodingLanguage", "ESG", "General", object);
    localrc = AttPackAddAttribute("Author", "ESG", "General", object);
    localrc = AttPackAddAttribute("Agency", "ESG", "General", object);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
    
  return ESMF_SUCCESS;

}  // end AttPackCreateStandard()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackNest"
//BOPI
// !IROUTINE:  AttPackNest() - nest an attpack
//
// !INTERFACE:
      int Attribute::AttPackNest(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const string &convention,              // in - Attribute convention
      const string &purpose,                 // in - Attribute purpose
      const string &object,                  // in - Attribute object type
      const string &nestConvention,          // in - Attribute nestConvention
      const string &nestPurpose) {           // in - Attribute nestPurpose
// 
// !DESCRIPTION:
//     Setup the name, convention and purpose of a nested attpack.
//
//EOPI

  int localrc;
  unsigned int i;
  Attribute *attpack, *nestpack, *localParent;
  bool done = false;

  attpack = NULL; nestpack = NULL; localParent = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Search for the pack to nest this one around
  nestpack = AttPackGet(nestConvention, nestPurpose, object);
  if(!nestpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND,
      "could not find the attpack", &localrc);
    return localrc;
  }
    
  // Make the attpack
  attpack = new Attribute(convention, purpose, object);
  if(!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED,
      "could not make the attpack", &localrc);
    return localrc;
  }
  
  // Put the attpack onto nestPack's parent
  localrc = nestpack->parent->AttPackSet(attpack);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;
  
  localParent = nestpack->parent;

  // Now remove nestpack from it's parent
  for (i=0; i<localParent->packList.size(); i++) {
    if (nestConvention.compare(localParent->packList.at(i)->attrConvention)==0 && 
        nestPurpose.compare(localParent->packList.at(i)->attrPurpose)==0 &&
        object.compare(localParent->packList.at(i)->attrObject)==0) {
      localParent->packList.erase(localParent->packList.begin() + i);
      localParent->structChange = ESMF_TRUE;
      done = true;
      break;
    }
  }
  
  // Put nestpack onto attpack
  localrc = attpack->AttPackSet(nestpack);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;
  
  if (!done) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_DELETED,
      "AttPackNest failed removing the Attribute package", &localrc);
    return localrc;
  }

  return ESMF_SUCCESS;

}  // end AttPackNest()
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
  
  for (i=0; i<packList.size(); i++) {
    // look for the attpack on this Attribute
    if (convention.compare(packList.at(i)->attrConvention) == 0 && 
        purpose.compare(packList.at(i)->attrPurpose) == 0 &&
        object.compare(packList.at(i)->attrObject) ==0)
          return packList.at(i);
    // recurse through the nested Attribute packages
    attpack = packList.at(i)->AttPackGet(convention, purpose, object);
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
      const string &name) const {         // in - Attribute name to retrieve)
// 
// !DESCRIPTION:
//     Get an {\tt Attribute} from an attpack given its name, convention, 
//     purpose, and object type.  This routine is assumed to be called on the 
//     Attribute package that holds the Attribute in question.
//
//EOPI

  Attribute *attr;
  unsigned int i;
  
  attr = NULL;
    
  // recurse through the nested Attribute packages
  for (i=0; i<packList.size(); i++)
      attr = packList.at(i)->AttPackGetAttribute(name);
  
  // look for the Attribute on this attpack
  for (i=0; i<attrList.size(); i++) {
    if (name.compare(attrList.at(i)->attrName) == 0)
      return attrList.at(i);
  }

  return attr;

}  // end AttPackGetAttribute
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
      ESMC_Logical *present) const {         // in/out - the present flag
// 
// !DESCRIPTION:
//     Query an Attribute package for an {\tt Attribute} given its name, convention, 
//     purpose, and object type.
//
//EOPI

  unsigned int i;
  Attribute *attr, *attpack;
  
  attr = NULL; attpack = NULL;

  // get the attpack
  attpack = AttPackGet(convention, purpose, object);
  if (!attpack) {
    *present = ESMF_FALSE;
    return ESMF_SUCCESS;
  }
  // get the attr on the attpack
  attr = attpack->AttPackGetAttribute(name);
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
  unsigned int i;
  Attribute *attpack, *attrparent;
  bool done = false;
  
  attpack = NULL; attrparent = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
    
  // get the attpack
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the Attribute package", &localrc);
    return localrc;
  }
  
  // save the parent, remove attpack from it's parent, then delete attpack
  attrparent = attpack->parent;
  for (i=0; i<attrparent->packList.size(); i++) {
    if (convention.compare(attrparent->packList.at(i)->attrConvention)==0 && 
        purpose.compare(attrparent->packList.at(i)->attrPurpose)==0 &&
        object.compare(attrparent->packList.at(i)->attrObject)==0) {
      delete (attrparent->packList.at(i));
      attrparent->packList.erase(attrparent->packList.begin() + i);
      attrparent->structChange = ESMF_TRUE;
      done = true;
      break;
    }
  }
  
  if (!done) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_DELETED,
      "AttPackRemove could not locate the Attribute package", &localrc);
    return localrc;
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
  unsigned int i;
  Attribute *attr, *attpack, *attrparent;
  bool done = false;

  attr = NULL; attpack = NULL; attrparent = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  // get the attpack
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the specified Attribute package", &localrc);
    return localrc;
  }
  
  attr = attpack->AttPackGetAttribute(name);
  if(!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the Attribute in this Attribute Package", &localrc);
    return localrc;
  }
    
  attrparent = attr->parent;
  localrc = attrparent->AttributeRemove(name);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;
  
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
  Attribute *attr;
  Attribute *attpack;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Find the attpack Attribute
  attpack = AttPackGet(convention, purpose, object);
  if(!attpack) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Cannot find the specified Attribute package", &localrc);
    return localrc;
  }
  
  attr = attpack->AttPackGetAttribute(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "This Attribute package does have the specified Attribute", &localrc);
    return localrc;
  }

  // Set the Attribute
  localrc = attr->AttrModifyValue(tk, count, value);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;
  
  // return
  if (localrc != ESMF_SUCCESS) return ESMF_FAILURE;
  return ESMF_SUCCESS;
  
}  // end AttPackSet()
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttPackSet"
//BOPI
// !IROUTINE:  AttPackSet - set {\tt Attribute} on an ESMF type
//
// !INTERFACE:
      int Attribute::AttPackSet(
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

  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, 
      "bad Attribute object", &localrc);
    return localrc;
  }

  // first, see if you are replacing an existing Attribute
  for (i=0; i<packList.size(); i++) {
    if ((attr->attrName).compare(packList.at(i)->attrName)==0 &&
        (attr->attrConvention).compare(packList.at(i)->attrConvention)==0 &&
        (attr->attrPurpose).compare(packList.at(i)->attrPurpose)==0 &&
        (attr->attrObject).compare(packList.at(i)->attrObject)==0) {

      // if you get here, you found a match.  replace previous copy.

      // delete old Attribute, including possibly freeing a list
      delete packList.at(i);

      // replace the original Attribute with attr
      packList.at(i) = attr;
      // attr may be of a different value than the original Attribute
      attr->valueChange = ESMF_TRUE;
      // point attr to its new Base
      attr->attrBase = this->attrBase; 
      attr->parent = this;
      return ESMF_SUCCESS;
    }
  }   

  // point attr to its new Base
  attr->attrBase = this->attrBase;
  attr->parent = this;
  
  // add the Attribute
  packList.push_back(attr);
  structChange = ESMF_TRUE;
  attrPack = ESMF_TRUE;
  
  return ESMF_SUCCESS;

}  // end AttPackSet
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
  vl = 0;
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
            vl = source.vl;  
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
  int localrc;
  unsigned int i;
  Attribute *attr;
  
  attr = NULL;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // call local copy on this Attribute 
  localrc = AttributeCopy(source);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  // copy Attributes and Attribute packages by reference
  for (i=0; i<source.attrList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Failed allocating an Attribute", &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase); 
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.attrList.at(i)));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttributeSet(attr);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
  // copy Attribute packages by reference
  for (i=0; i<source.packList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Failed allocating an Attribute", &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase);
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.packList.at(i)));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    localrc = AttPackSet(attr);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
  // copy Attribute links by value
  for (i=0; i<source.linkList.size(); i++) {
    attr = source.linkList.at(i);
    localrc = AttributeLink(attr);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
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
  int localrc;
  unsigned int i;
  Attribute *attr;
  
  attr = NULL;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // call local copy on source
  localrc = AttributeCopy(source);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  // copy base level Attributes by value
  for (i=0; i<source.attrList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Failed allocating an Attribute", &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase);
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.attrList.at(i)));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    // add newly initialized attr to destination
    localrc = AttributeSet(attr);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
  // copy base level Attributes by value
  for (i=0; i<source.packList.size(); i++) {
    attr = NULL;
    attr = new Attribute(ESMF_FALSE);
    if (!attr) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
        "Failed allocating an Attribute", &localrc);
      return localrc;
    }
    // set new attr to point to its intented destination and recurse
    (attr->attrBase) = (this->attrBase);
    (attr->parent) = this;
    localrc = attr->AttributeCopyValue(*(source.packList.at(i)));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    // add newly initialized attr to destination
    localrc = AttPackSet(attr);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }

  return ESMF_SUCCESS;

 } // end AttributeCopyValue
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeMove"
//BOPI
// !IROUTINE:  AttributeMove - Move {\tt Attributes} between ESMF objects
//
// !INTERFACE:
      int Attribute::AttributeMove(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
        Attribute *source) {   // in - source
//
// !DESCRIPTION:
//   Copy all {\tt Attribute} data and copy by reference all {\tt Attributes} in 
//   this base level and remove everything copied from source.  
//
//   Note:  this routine is written with the assumption of being called from a Base
//
//EOPI
  int localrc;
  unsigned int i;
  Attribute *attr;
  
  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // call local copy on source
  localrc = AttributeCopy(*source);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  // copy base level Attributes by value
  for (i=0; i<source->attrList.size(); i++) {
    // add each attr to destination
    localrc = AttributeSet(source->attrList.at(0));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    // now remove this Attribute from source, this is a swap
    source->attrList.erase(source->attrList.begin());
  }
  // copy base level Attribute packages by value
  for (i=0; i<source->packList.size(); i++) {
    // add each attr to destination
    localrc = AttPackSet(source->packList.at(0));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    // now remove this Attribute from source, this is a swap
    source->packList.erase(source->packList.begin());
  }
/*
  // copy the Attribute links by value
  for (i=0; i<source->linkList.size(); i++) {
    // set the links
    localrc = AttributeLink(source->linkList.at(0));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    // now remove the link from source, this is a swap
    localrc = source->AttributeLinkRemove(source->linkList.at(0));
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
*/

  return ESMF_SUCCESS;

 } // end AttributeMove
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
      int &objcount,             // inout - object count
      int &numattrs ) const{     // inout - count of attrs on objects in tree
// 
// !DESCRIPTION:
//     Count the number of objects in the {\tt Attribute} hierarchy 

//EOPI

  int localrc;
  unsigned int i;
  Attribute *attpack;

  attpack = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
 
  attpack = AttPackGet(convention, purpose, object);
  if (attpack) {
    numattrs = 0;
    objcount++;
    localrc = attpack->AttributeCountTreeAttpack(objcount, numattrs);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); i++) {
    localrc = linkList.at(i)->AttributeCountTree(convention, purpose, object, 
      objcount, numattrs);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    }

  return ESMF_SUCCESS;

}  // end AttributeCountTree
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCountTreeAttpack"
//BOPI
// !IROUTINE:  AttributeCountTreeAttpack - count objects in {\tt Attribute} hierarchy 
//
// !INTERFACE:
      int Attribute::AttributeCountTreeAttpack(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int &objcount,             // inout - count of objects to write
      int &numattrs ) const{     // inout - count of attrs on objects in tree
// 
// !DESCRIPTION:
//     Count the number of objects in the {\tt Attribute} hierarchy 

//EOPI

  int localrc;
  unsigned int i;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;

  numattrs += attrList.size(); 

  // Recurse the hierarchy
  for (i=0; i<packList.size(); i++) {
    localrc = packList.at(i)->AttributeCountTreeAttpack(objcount,numattrs);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    }

  return ESMF_SUCCESS;

}  // end AttributeCountTreeAttpack
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
      const string &convention,           // in - convention
      const string &purpose,              // in - purpose
      const string &object,               // in - object type to look for
      int *attrLens,                      // inout - lengths of column names
      vector<string> &attrNames ) const{  // inout - names of columns
// 
// !DESCRIPTION:
//     Count the number of objects in the Attribute hierarchy

//EOPI

  int localrc;
  unsigned int i;
  int index;
  Attribute *attpack;
  
  attpack = NULL;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  attpack = AttPackGet(convention, purpose, object);
  if (attpack) {
    index = 0;
    localrc = attpack->AttributeCountTreeLensAttpack(index, attrLens, attrNames);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); ++i) {
    localrc = linkList.at(i)->AttributeCountTreeLens(convention, purpose, object, attrLens, attrNames);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    }

  return ESMF_SUCCESS;

}  // end AttributeCountTreeLens
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeCountTreeLensAttpack"
//BOPI
// !IROUTINE:  AttributeCountTreeLensAttpack - get lengths and names of columns
//
// !INTERFACE:
      int Attribute::AttributeCountTreeLensAttpack(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      int &index,               // in - index counter
      int *attrLens,                      // inout - lengths of column names
      vector<string> &attrNames ) const{  // inout - names of columns
// 
// !DESCRIPTION:
//     Get the lengths and names of the columns in the table

//EOPI

  int localrc;
  unsigned int i;
  Attribute *attr;
  
  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  for (i=0; i<attrList.size(); ++i) {
    if (attrLens[index] == 0)
      attrNames.push_back(attrList.at(i)->attrName);
    else if (attrLens[index] > 0) {
      // this should fail
      if (attrNames[index].compare(attrList.at(i)->attrName)) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_WRONG, 
          "Attribute package name out of order", &localrc);
        return localrc;
      }
    } else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
        "Length < 0 = no good", &localrc);
      return localrc;
    }
    // add length
    if (attrList.at(i)->items > 1) {
      ESMC_LogDefault.Write("Write items >1 not yet implemented", ESMC_LOG_INFO);
      attrLens[index] = 0;
    } else if (attrList.at(i)->items == 1) {
        if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL)
          attrLens[index] = 8;
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER) {
          if ((attrList.at(i)->vcp.size()+3) > attrLens[index])
            attrLens[index] = (attrList.at(i)->vcp.size()+3);
        } else {
            attr = attrList.at(i);
            if (attr->tk == ESMC_TYPEKIND_I4) {
              /*if (attr->vi < -1) attrLens[index] = 3 + ceil(log10(-1*attr->vi));
              else if (attr->vi >= -1 && attr->vi <=1) attrLens[index] = 4;
              else attrLens[index] = 3 + ceil(log10(attr->vi));
              */ attrLens[index] = 10;
            } else if (attr->tk == ESMC_TYPEKIND_I8) {
              /*if (attr->vl < -1) attrLens[index] = 3 + ceil(log10(-1*attr->vl));
              else if (attr->vl >= -1 && attr->vl <=1) attrLens[index] = 4;
              else attrLens[index] = 3 + ceil(log10(attr->vl));
              */ attrLens[index] = 10;
            } else if (attr->tk == ESMC_TYPEKIND_R4) {
              /*if (attr->vf < -1) attrLens[index] = 3 + ceil(log10(-1*attr->vf));
              else if (attr->vf >= -1 && attr->vf <=1) attrLens[index] = 4;
              else attrLens[index] = 3 + ceil(log10(attr->vf));
              */ attrLens[index] = 10;
            } else if (attr->tk == ESMC_TYPEKIND_R8) {
              /*if (attr->vd < -1) attrLens[index] = 3 + ceil(log10(-1*attr->vd));
              else if (attr->vd >= -1 && attr->vd <=1) attrLens[index] = 4;
              else attrLens[index] = 3 + ceil(log10(attr->vd));
              */ attrLens[index] = 10;
            } else {
              ESMC_LogDefault.Write(
               "Couldn't find data type, using generic string length", 
               ESMC_LOG_INFO);
              attrLens[index] = 10;
            }
        }
    }
    ++index;
  }
  
  // Recurse the hierarchy
  for (i=0; i<packList.size(); i++) {
    localrc = packList.at(i)->AttributeCountTreeLensAttpack(index, attrLens, attrNames);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    }

  return ESMF_SUCCESS;

}  // end AttributeCountTreeLensAttpack
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
  
  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I4) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind I4", &localrc);
      return localrc;
    }
  
    // simple sanity checks
    if (attr->items != 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF, 
        "Attribute not single value", &localrc);
      return localrc;
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

  int localrc;
  Attribute *attr;
  
  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return ESMF_FAILURE;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I4) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind I4", &localrc);
      return localrc;
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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I8) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind I8", &localrc);
      return localrc;
    }

    // simple sanity checks
    if (attr->items != 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF, 
        "Attribute not single value", &localrc);
      return localrc;
    }

    *value = attr->vl;
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

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_I8) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind I8", &localrc);
      return localrc;
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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R4) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind R4", &localrc);
      return localrc;
    }

    // simple sanity checks
    if (attr->items != 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF, 
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

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R4) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind R4", &localrc);
      return localrc;
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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R8) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind R8", &localrc);
      return localrc;
    }

    // simple sanity checks
    if (attr->items != 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF, 
        "Attribute not single value", &localrc);
      return localrc;
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

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL; 
  
  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_R8) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind R8", &localrc);
      return localrc;
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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_LOGICAL) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind LOGICAL", &localrc);
      return localrc;
    }

    // simple sanity checks
    if (attr->items != 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF, 
        "Attribute not single value", &localrc);
      return localrc;
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

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_LOGICAL) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind LOGICAL", &localrc);
      return localrc;
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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind CHARACTER", &localrc);
      return localrc;
    }

    // simple sanity checks
    if (attr->items != 1) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF, 
        "Attribute not single value", &localrc);
      return localrc;
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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
    return localrc;
  }
  else {
    // simple sanity checks
    if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
        "Attribute not typekind CHARACTER", &localrc);
      return localrc;
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

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // Get the attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
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

  int localrc;
  Attribute *attr;

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (num < 0) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
      "Attribute index must be >0", &localrc);
    return localrc;
  }

  // Get the attribute
  attr = AttributeGet(num);
  if (!attr) {
    ESMC_LogDefault.Write(
      "Attribute not found, using default value if present", ESMC_LOG_INFO);
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

  unsigned int i;

  for (i=0; i<attrList.size(); i++) {
      if (name.compare(attrList.at(i)->attrName) == 0)
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

  // simple sanity check
  if ((number < 0) || (number >= attrList.size())) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
      "Invalid index for AttributeGet(index)", NULL);
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

  int size, localrc;
  unsigned int i;
  Attribute *attr;

  attr = NULL;

  // look for the Attribute
  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Could not locate the Attribute", &localrc);
    return localrc;
  }
    
  // check that this is a char Attribute
  if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, 
      "Attribute is not typekind CHARACTER", &localrc);
    return localrc;
  }
  
  // check that the count is correct
  if (count < 0 || (count > attr->items)) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_ITEMSOFF, 
      "Count argument is incorrect", &localrc);
    return localrc;
  }
  
  // find the lengths of the strings on this Attribute
  for (i=0; i<count; i++) {
    if (attr->items == 1)
      lens[i] = (attr->vcp).size();
    else if (attr->items > 1)
      lens[i] = (attr->vcpp[i]).size();
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

  return attrList.size();

} // end AttributeGetCount
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetCountPack"
//BOPI
// !IROUTINE:  AttributeGetCountPack - get an the number of {\tt Attribute} packages
// 
// !INTERFACE:
      int Attribute::AttributeGetCountPack(
// 
// !RETURN VALUE:
//    number of {\tt Attribute} packages in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of {\tt Attribute} packages present
//
//EOPI

  return packList.size();

} // end AttributeGetCountPack
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetCountLink"
//BOPI
// !IROUTINE:  AttributeGetCountLink - get the number of {\tt Attribute} links
// 
// !INTERFACE:
      int Attribute::AttributeGetCountLink(
// 
// !RETURN VALUE:
//    number of {\tt Attribute} links in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of {\tt Attribute} links present
//
//EOPI

  return linkList.size();

} // end AttributeGetCountLink
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeGetCountTotal"
//BOPI
// !IROUTINE:  AttributeGetCountTotal - get the total number of {\tt Attributes}
// 
// !INTERFACE:
      int Attribute::AttributeGetCountTotal(
// 
// !RETURN VALUE:
//    total number of {\tt Attributes} in this attrList
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns the total number of {\tt Attributes} present
//
//EOPI

  return attrList.size()+packList.size()+linkList.size();

} // end AttributeGetCountTotal
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

  int localrc;
  Attribute *attr;
  
  attr = NULL;

  attr = AttributeGet(name);
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Attribute not found", &localrc);
    return localrc;
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
  
  attr = NULL;

  attr = AttributeGet(name);
  if (!attr)
    *present = ESMF_FALSE;
  else *present = ESMF_TRUE;
  
  // return
  return ESMF_SUCCESS;

}  // end AttributeIsPresent
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeLink"
//BOPI
// !IROUTINE:  AttributeLink - Link an {\tt Attribute} hierarchy
//
// !INTERFACE:
      int Attribute::AttributeLink(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      Attribute *destination) {  // in/out destination Attribute to be linked
// !DESCRIPTION:
//     Link an {\tt Attribute} hierarchy.
//
//EOPI

  int localrc;
  unsigned int i;
  Attribute *attr;
  
  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
    
  for (i=0; i<linkList.size(); i++) {
    if ((destination->attrBase->ESMC_BaseGetID() == 
      linkList.at(i)->attrBase->ESMC_BaseGetID()) &&
      ESMCI::VMIdCompare(destination->attrBase->ESMC_BaseGetVMId(),
      linkList.at(i)->attrBase->ESMC_BaseGetVMId())) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_LINK, 
        "AttributeLink tried to double set a link", &localrc);
      return localrc;
    }
  }
  
  // actually set the link
  attr = destination;
  // attrBase and parent should already be set
  linkList.push_back(attr);  
  linkChange = ESMF_TRUE;

  return ESMF_SUCCESS;

}  // end AttributeLink
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "AttributeLinkRemove"
//BOPI
// !IROUTINE:  AttributeLinkRemove - Remove a link in an {\tt Attribute} hierarchy
//
// !INTERFACE:
      int Attribute::AttributeLinkRemove(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      Attribute *destination) {  // in/out destination Attribute to be linked
// !DESCRIPTION:
//     Set a link in an {\tt Attribute} hierarchy.
//
//EOPI

  int localrc;
  unsigned int i;
  bool done=false;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
    
  for (i=0; i<linkList.size(); i++) {
    if (destination->attrBase->ESMC_BaseGetID() == 
        linkList.at(i)->attrBase->ESMC_BaseGetID() &&
        ESMCI::VMIdCompare(destination->attrBase->ESMC_BaseGetVMId(),
        linkList.at(i)->attrBase->ESMC_BaseGetVMId())) {
        // don't delete the root, but erase the Attribute pointer
        linkList.erase(linkList.begin() + i);
        structChange = ESMF_TRUE;
        done = true;
        break;
    }
  }
  
  // link wasn't found
  if (!done) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "AttributeLink could not find the link to remove", &localrc);
    return localrc;
  }
  
  return ESMF_SUCCESS;
    
}  // end AttributeLinkRemove
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
  unsigned int i;
  bool done=false;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  for (i=0; i<attrList.size(); i++) {
    if (name.compare(attrList.at(i)->attrName) == 0) {
      // found a match, destroy it
      delete attrList.at(i);
      attrList.erase(attrList.begin() + i);
      structChange = ESMF_TRUE;
      done = true;
      break;
    }
  }
  
  if (!done) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_FOUND, 
      "Could not locate the Attribute to remove", &localrc);
    return localrc;
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

  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, 
      "Bad Attribute object", &localrc);
    return localrc;
  }

  // first, see if you are replacing an existing Attribute
  for (i=0; i<attrList.size(); i++) {
    if ((attr->attrName).compare(attrList.at(i)->attrName)==0 &&
        (attr->attrConvention).compare(attrList.at(i)->attrConvention)==0 &&
        (attr->attrPurpose).compare(attrList.at(i)->attrPurpose)==0 &&
        (attr->attrObject).compare(attrList.at(i)->attrObject)==0) {

      // if you get here, you found a match.  replace previous copy.

      // delete old Attribute, including possibly freeing a list
      delete attrList.at(i);

      // replace the original Attribute with attr
      attrList.at(i) = attr;
      // attr may be of a different value than the original Attribute
      attr->valueChange = ESMF_TRUE;
      // point attr to its new Base
      attr->attrBase = this->attrBase; 
      attr->parent = this;
      return ESMF_SUCCESS;
    }
  }   

  // point attr to its new Base
  attr->attrBase = this->attrBase;
  attr->parent = this;
  
  // add the Attribute
  attrList.push_back(attr);
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
  
  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I4, 1, &value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I4, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I8, 1, &value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_I8, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R4, 1, &value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R4, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R8, 1, &value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_R8, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_LOGICAL, 1, &value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_LOGICAL, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_CHARACTER, 1, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

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

  attr = NULL;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  attr = new Attribute(name, ESMC_TYPEKIND_CHARACTER, count, value);  
  if (!attr) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_NOT_CREATED, 
      "Bad Attribute object", &localrc);
    return localrc;
  }
 
  localrc = AttributeSet(attr);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) return localrc;

  return ESMF_SUCCESS;

}  // end AttributeSet(charlist)
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
      const int &count,                     // in - count
      void *value) {                 // in - value
// 
// !DESCRIPTION:
//     Set the objects in the {\tt Attribute} hierarchy 

//EOPI

  int localrc;
  unsigned int i;

  // Initialize local return code
  localrc = ESMC_RC_NOT_IMPL;
  
  for (i=0; i<attrList.size(); i++) {
  // If this is object matches, count it
  if (object.compare(attrList.at(i)->attrObject) == 0 && 
      name.compare(attrList.at(i)->attrName) == 0) {
    localrc = attrList.at(i)->AttrModifyValue(tk, count, value);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }
  }
  
  // Recurse the hierarchy
  for (i=0; i<packList.size(); i++) {
    localrc = packList.at(i)->AttributeSetObjsInTree(object,name,tk,count,value);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }  

  // Recurse the hierarchy
  for (i=0; i<linkList.size(); i++) {
    localrc = linkList.at(i)->AttributeSetObjsInTree(object,name,tk,count,value);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
  }  

  return ESMF_SUCCESS;

}  // end AttributeSetObjsInTree

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
      int fileNameLen,             //  in - file name length
      const char* fileName) {      //  in - file name

//
// !DESCRIPTION:
//    Read the contents of an XML file into an {\tt Attribute} hierarchy.
//    Expected to be called internally.
//
//EOPI

  int rc;

  // instantiate IO object; initialize with pointer to this Attribute node, to
  // place file-read attributes into.
  IO_XML *io_xml = new IO_XML(this);  // deleted via ESMF's garbage collection

  // read the XML file, placing contents into this Attribute node
  rc = io_xml->read(fileNameLen, fileName);
  ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc);

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
  
    // Open an XML file for writing
  sprintf(msgbuf,"%s.stdout",basename.c_str());
  if((tab=fopen(msgbuf,"w"))==NULL) {
    localrc = ESMF_FAILURE;
    sprintf(msgbuf,"Could not open the write file!");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
    return ESMF_FAILURE;
  } 

  // determine the number of fields to write
  localrc = AttributeCountTree(convention, purpose, varobj, rows, columns);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "AttributeWriteTab failed counting objects");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    fclose(tab);
    return ESMF_FAILURE;
  }
  
  // allocate the integer array of length maxobjs
  attrLens = new int[columns];
  if (!attrLens) {
    sprintf(msgbuf, "AttributeWriteTab failed allocating attrLens");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf,  &localrc);
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
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
  
  attpack = AttPackGet(convention, purpose, "field");
  if (attpack) {
    localrc = attpack->AttributeWriteTabBuffer(tab,index,columns,attrLens,attrNames);
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "AttributeWriteTabTraverse failed AttributeWriteTabBuffer");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
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
        sprintf(msgbuf, "%-*\t",tlen);
        fprintf(tab,"%s",msgbuf);
      } else if (attrList.at(i)->items == 1) {
        if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "%-*d\t",tlen,attrList.at(i)->vi);
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "%-*lld\t",tlen,attrList.at(i)->vl); 
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "%-*f\t",tlen,attrList.at(i)->vf);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "%-*g\t",tlen,attrList.at(i)->vd);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
          if (attrList.at(i)->vb == ESMF_TRUE) 
            sprintf(msgbuf, "%-*s\t",tlen,"true");
          else if (attrList.at(i)->vb == ESMF_FALSE)
            sprintf(msgbuf, "%-*s\t",tlen,"false");
        }
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "%-*s\t",tlen,attrList.at(i)->vcp.c_str());
        else
          sprintf(msgbuf, "%-*s\t",tlen,"N/A");
        fprintf(tab,"%s",msgbuf);
      }
      else if (attrList.at(i)->items > 1) { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
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
      const string &basename) const{        //  in - basename
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  FILE* xml;
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

  // Open an XML file for writing
  sprintf(msgbuf,"%s.xml",basename.c_str());
  if((xml=fopen(msgbuf,"w"))==NULL) {
    localrc = ESMF_FAILURE;
    sprintf(msgbuf,"Could not open the xml file!");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             msgbuf, &localrc);
    return ESMF_FAILURE;
  } 

  if (object.compare("comp")==0) {
  // get value of attribute 0 or set to N/A if not present
  localrc = AttPackIsPresent("Name",convention,purpose,object,&presentflag);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "failed finding an attribute");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
  if (presentflag == ESMF_TRUE) {
    attr = (AttPackGet(convention, purpose, object)->AttPackGetAttribute("Name"));
    modelcompname = attr->vcp;
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "failed getting attribute value");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      fclose(xml);
      return ESMF_FAILURE;
    }
  }
  else {
    modelcompname="N/A";
  }
  
  // get value of attribute 1 or set to N/A if not present
  localrc = AttPackIsPresent("FullName",convention,purpose,object,&presentflag);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "failed finding an attribute");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
  if (presentflag == ESMF_TRUE) {
    attr = (AttPackGet(convention,purpose,object)->AttPackGetAttribute("FullName"));
    fullname = attr->vcp;
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "failed getting attribute value");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      fclose(xml);
      return ESMF_FAILURE;
    }
  }
  else {
    fullname="N/A";
  }
  
  // get value of attribute 2 or set to N/A if not present
  localrc = AttPackIsPresent("Version",convention,purpose,object,&presentflag);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "failed finding an attribute");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
  if (presentflag == ESMF_TRUE) {
    attr = (AttPackGet(convention,purpose,object)->AttPackGetAttribute("Version"));
    version = attr->vcp;
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "failed getting attribute value");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      fclose(xml);
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
             object.compare("array")==0) { 
    modelcompname="N/A";
    fullname="N/A";
    version="N/A";
  }
  else {
    sprintf(msgbuf, "AttributeWrite called from an invalid ESMF object");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    return ESMF_FAILURE;
  }
  
  // TODO: replace this prototype for WaterML TimeSeries
  if (convention.compare("WaterML")==0 & 
      purpose.compare("TimeSeries")==0) {

    // Write the WaterML XML file header
    fprintf(xml,"<timeSeriesResponse xmlns:gml=\"http://www.opengis.net/gml\"\n");
    fprintf(xml,"    xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"\n");
    fprintf(xml,"    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:wtr=\"http://www.cuahsi.org/waterML/\"\n");
    fprintf(xml,"    xmlns=\"http://www.cuahsi.org/waterML/1.0/\">\n");
  
    fprintf(xml,"    <queryInfo>\n");
    fprintf(xml,"        <creationTime>2009-01-08T15:52:17.8495Z</creationTime>\n");
    fprintf(xml,"        <criteria>\n");
    fprintf(xml,"            <locationParam>LittleBearRiver:USU-LBR-Paradise</locationParam>\n");
    fprintf(xml,"            <variableParam>LBR:USU39</variableParam>\n");
    fprintf(xml,"            <timeParam>\n");
    fprintf(xml,"                <beginDateTime>2008-04-14T13:00:00</beginDateTime>\n");
    fprintf(xml,"                <endDateTime>2008-04-15T12:00:00</endDateTime>\n");
    fprintf(xml,"            </timeParam>\n");
    fprintf(xml,"        </criteria>\n");
    fprintf(xml,"        <note>OD Web Service</note>\n");
    fprintf(xml,"    </queryInfo>\n");
    fprintf(xml,"    <timeSeries>\n");

  } else {

    // Write the XML file header
    sprintf(msgbuf,"<model_component name=\"%s\" full_name=\"%s\" version=\"%s\"\n",
      modelcompname.c_str(),fullname.c_str(),version.c_str());
    fprintf(xml,"%s",msgbuf);
    sprintf(msgbuf,"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
    fprintf(xml,"%s",msgbuf);
    sprintf(msgbuf,"xsi:schemaLocation=\"http://www.earthsystemmodeling.org file:/esmf_model_component.xsd\"\n");
    fprintf(xml,"%s",msgbuf);
    sprintf(msgbuf,"xmlns=\"http://www.earthsystemmodeling.org\">\n\n");
    fprintf(xml,"%s",msgbuf);
  }

  // determine the number of fields to write
  localrc = AttributeCountTree(convention, purpose, varobj, rows, columns);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "Attribute failed counting fields");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }
 
  // if not called from component, won't need to print that stuff
  if (object.compare("comp"))
    compdone = true;
 
  // recurse the Attribute hierarchy
  localrc = AttributeWriteXMLtraverse(xml,convention,purpose,columns,
    fielddone,griddone,compdone);
  if (localrc != ESMF_SUCCESS) {
    sprintf(msgbuf, "Attribute failed recursing in WriteTab");
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
    fclose(xml);
    return ESMF_FAILURE;
  }

  // TODO: replace this prototype for WaterML TimeSeries
  if (convention.compare("WaterML")==0 & 
      purpose.compare("TimeSeries")==0) {

    // write the WaterML footer
    fprintf(xml,"    </timeSeries>\n");
    fprintf(xml,"</timeSeriesResponse>\n");
  
  } else {

    // write the XML footer
    sprintf(msgbuf,"\n</model_component>\n");
    fprintf(xml,"%s",msgbuf);
  }
  
  // close the file
  fclose(xml);

  return localrc;

 } // end AttributeWriteXML
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
      FILE *xml,               //  in - file pointer to write
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
  attpack = AttPackGet(convention, purpose, "comp");
  if (attpack) {
    localrc = attpack->AttributeWriteXMLbuffer(xml);
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "AttributeWriteXMLtraverse failed AttributeWriteXMLbuffer");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      fclose(xml);
      return ESMF_FAILURE;
    }
  }
    compdone = true;
  }

  // do field write
  if (!fielddone) {
    // TODO: replace this prototype for WaterML TimeSeries
    if (!(convention.compare("WaterML")==0 & 
          purpose.compare("TimeSeries")==0)) {
      // write the field header
       sprintf(msgbuf,"<variable_set>\n");
       fprintf(xml,"%s",msgbuf);
    }
    // call the field write buffer method
    localrc = AttributeWriteXMLbufferfield(xml, convention, purpose, index, columns);
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "AttributeWriteXMLtraverse failed AttributeWriteXMLbufferfield");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      fclose(xml);
      return ESMF_FAILURE;
    }
    if (!(convention.compare("WaterML")==0 & 
          purpose.compare("TimeSeries")==0)) {
      // write the field footer
      sprintf(msgbuf,"</variable_set>\n\n");
      fprintf(xml,"%s",msgbuf);
    }
    // done with field
    fielddone = true;
  }
  
  // do grid write
  if (!griddone) {
  attpack = AttPackGet(convention, purpose, "grid");
  if (attpack) {
    // write the field header
    sprintf(msgbuf,"<GridSpec name=\"%s\">\n", attpack->attrBase->ESMC_BaseGetName());
    fprintf(xml,"%s",msgbuf);
    sprintf(msgbuf,"  <Mosaic name=\"%s\">\n", attpack->attrBase->ESMC_BaseGetName());
    fprintf(xml,"%s",msgbuf);
    localrc = attpack->AttributeWriteXMLbuffergrid(xml);
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "AttributeWriteXMLtraverse failed AttributeWriteXMLbuffer");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      fclose(xml);
      return ESMF_FAILURE;
    }
    sprintf(msgbuf,"  </Mosaic>\n");
    fprintf(xml,"%s",msgbuf);
    sprintf(msgbuf,"</GridSpec>\n");
    fprintf(xml,"%s",msgbuf);
    griddone = true;
    return ESMF_SUCCESS;
  }
  }
  
  for(i=0; i<linkList.size(); i++)
    localrc = linkList.at(i)->AttributeWriteXMLtraverse(xml,convention,purpose,columns,
      fielddone,griddone,compdone);

  return ESMF_SUCCESS;

 } // end AttributeWriteXMLtraverse
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
      FILE *xml) const{             //  in - file pointer to write
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

    for (i=0;  i<attrList.size(); ++i) { 
      if (attrList.at(i)->items == 1) {
        if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "    <%s>%d</%s>\n",attrList.at(i)->attrName.c_str(),
            attrList.at(i)->vi,attrList.at(i)->attrName.c_str());
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "    <%s>%lld</%s>\n",attrList.at(i)->attrName.c_str(),
            attrList.at(i)->vl,attrList.at(i)->attrName.c_str());
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "    <%s>%f</%s>\n",attrList.at(i)->attrName.c_str(),
            attrList.at(i)->vf,attrList.at(i)->attrName.c_str());
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "    <%s>%g</%s>\n",attrList.at(i)->attrName.c_str(),
            attrList.at(i)->vd,attrList.at(i)->attrName.c_str());
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
          if (attrList.at(i)->vb == ESMF_TRUE) 
            sprintf(msgbuf, "    <%s>%s</%s>\n",attrList.at(i)->attrName.c_str(),
              "true",attrList.at(i)->attrName.c_str());
          else if (attrList.at(i)->vb == ESMF_FALSE)
            sprintf(msgbuf, "    <%s>%s</%s>\n",attrList.at(i)->attrName.c_str(),
              "false",attrList.at(i)->attrName.c_str());
        }
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "    <%s>%s</%s>\n",attrList.at(i)->attrName.c_str(),
            attrList.at(i)->vcp.c_str(),attrList.at(i)->attrName.c_str());
        else
          sprintf(msgbuf, "    <%s>%s</%s>\n",attrList.at(i)->attrName.c_str(),
            "N/A",attrList.at(i)->attrName.c_str());
      fprintf(xml,"%s",msgbuf);
      } else if (attrList.at(i)->items >1) { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      } else if (attrList.at(i)->items == 0) {
        // do nothing
      } else {
        sprintf(msgbuf,"Items < 1, problem.");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        return ESMF_FAILURE;
      }
    }

  for(i=0; i<packList.size(); i++)
    localrc = packList.at(i)->AttributeWriteXMLbuffergrid(xml);

  return ESMF_SUCCESS;

 } // end AttributeWriteXMLbuffergrid
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
      FILE *xml) const{             //  in - file pointer to write
//
// !DESCRIPTION:
//    Print the contents of an {\tt Attribute}.  Expected to be
//    called internally.
//
//EOPI

  char msgbuf[4*ESMF_MAXSTR];
  int localrc;
  unsigned int i;

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

    for (i=0;  i<attrList.size(); ++i) { 
      if (attrList.at(i)->items == 1) {
      sprintf(msgbuf,"<%s_set>\n",attrList.at(i)->attrName.c_str());
      fprintf(xml,"%s",msgbuf);
        if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "  <%s name=\"%d\" />\n",attrList.at(i)->attrName.c_str(),attrList.at(i)->vi);
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "  <%s name=\"%lld\" />\n",attrList.at(i)->attrName.c_str(),attrList.at(i)->vl); 
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "  <%s name=\"%f\" />\n",attrList.at(i)->attrName.c_str(),attrList.at(i)->vf);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "  <%s name=\"%g\" />\n",attrList.at(i)->attrName.c_str(),attrList.at(i)->vd);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
          if (attrList.at(i)->vb == ESMF_TRUE) 
            sprintf(msgbuf, "  <%s name=\"%s\" />\n",attrList.at(i)->attrName.c_str(),"true");
          else if (attrList.at(i)->vb == ESMF_FALSE)
            sprintf(msgbuf, "  <%s name=\"%s\" />\n",attrList.at(i)->attrName.c_str(),"false");
        }
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "  <%s name=\"%s\" />\n",attrList.at(i)->attrName.c_str(),
            attrList.at(i)->vcp.c_str());
        else
          sprintf(msgbuf, "  <%s name=\"%s\" />\n",attrList.at(i)->attrName.c_str(),"N/A");
      fprintf(xml,"%s",msgbuf);
      sprintf(msgbuf,"</%s_set>\n\n",attrList.at(i)->attrName.c_str());
      fprintf(xml,"%s",msgbuf);
      } else if (attrList.at(i)->items >1) { 
        sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
        ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      } else if (attrList.at(i)->items == 0) {
        //do nothing
      } else {
        sprintf(msgbuf,"Items < 1, problem.");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        return ESMF_FAILURE;
      }
    }

  for(i=0; i<packList.size(); i++)
    localrc = packList.at(i)->AttributeWriteXMLbuffer(xml);

  return ESMF_SUCCESS;

 } // end AttributeWriteXMLbuffer
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
      FILE *xml,                     //  in - file pointer to write
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

  attpack = AttPackGet(convention, purpose, "field");
  if (attpack) {
    // TODO: replace this prototype for WaterML TimeSeries
    if (convention.compare("WaterML")==0 & 
        purpose.compare("TimeSeries")==0) {
      localrc = attpack->AttributeWriteWaterMLbuffieldT(xml, index, columns);
    } else {
      localrc = attpack->AttributeWriteXMLbufferfieldT(xml, index, columns);
    }
    if (localrc != ESMF_SUCCESS) {
      sprintf(msgbuf, "AttributeWriteXMLbufferfield failed AttributeWriteXMLbufferfieldT");
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
      fclose(xml);
      return ESMF_FAILURE;
    }
  }

  for(i=0; i<linkList.size(); i++) {
    index = 0;
    localrc = linkList.at(i)->AttributeWriteXMLbufferfield(xml,
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
      FILE *xml,                     //  in - file pointer to write
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
        sprintf(msgbuf, "  <variable ");
      	fprintf(xml,"%s",msgbuf);
      } else {
        sprintf(msgbuf, "            ");
        fprintf(xml,"%s",msgbuf);
      }
      if (attrList.at(i)->items == 0) {
        sprintf(msgbuf, "  %s=\"\" ",attrList.at(i)->attrName.c_str());
        fprintf(xml,"%s",msgbuf);
      } else if (attrList.at(i)->items == 1) {
        if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
          sprintf(msgbuf, "  %s=\"%d\" ",attrList.at(i)->attrName.c_str(),attrList.at(i)->vi);
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) 
          sprintf(msgbuf, "  %s=\"%lld\" ",attrList.at(i)->attrName.c_str(),attrList.at(i)->vl); 
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) 
          sprintf(msgbuf, "  %s=\"%f\" ",attrList.at(i)->attrName.c_str(),attrList.at(i)->vf);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) 
          sprintf(msgbuf, "  %s=\"%g\" ",attrList.at(i)->attrName.c_str(),attrList.at(i)->vd);  
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
          if (attrList.at(i)->vb == ESMF_TRUE)
            sprintf(msgbuf, "  %s=\"%s\" ",attrList.at(i)->attrName.c_str(),"true");
          else if (attrList.at(i)->vb == ESMF_FALSE)
            sprintf(msgbuf, "  %s=\"%s\" ",attrList.at(i)->attrName.c_str(),"false");
        }
        else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
          sprintf(msgbuf, "  %s=\"%s\" ",attrList.at(i)->attrName.c_str(),
            (attrList.at(i)->vcp).c_str());
        else
          sprintf(msgbuf, "  %s=\"%s\" ",attrList.at(i)->attrName.c_str(),"N/A");
        fprintf(xml,"%s",msgbuf);
      } else if (attrList.at(i)->items > 1) { 
          sprintf(msgbuf,"Write items > 1 - Not yet implemented\n");
          ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
          sprintf(msgbuf,"  %s= ITEMS>1", attrList.at(i)->attrName.c_str());
          fprintf(xml,"%s",msgbuf);
      } else {
        sprintf(msgbuf,"Items < 1, problem.");
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
        return ESMF_FAILURE;
      }
    ++index;
      if (index == columns)
        sprintf(msgbuf," />\n");
      else 
        sprintf(msgbuf,"\n");
      fprintf(xml,"%s",msgbuf);
    }  

  for(i=0; i<packList.size(); i++)
    localrc = packList.at(i)->AttributeWriteXMLbufferfieldT(xml,index,columns);

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
      FILE *xml,                     //  in - file pointer to write
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
      fprintf(xml, "        <sourceInfo xsi:type=\"SiteInfoType\">\n");
    } else if (attrPurpose.compare("variable")==0) {
      fprintf(xml, "        <variable>\n");
    } else if (attrPurpose.compare("values")==0) {
      fprintf(xml, "        <values>");
    } else if (attrPurpose.compare((attrPurpose.size()-2),2,".1")==0) {
      // <value>
      fprintf(xml, "            <value>");
    } else if (attrPurpose.compare("method")==0) {
      fprintf(xml, "            <method>");
    } else if (attrPurpose.compare("source")==0) {
      fprintf(xml, "            <source>");
    }

    // print each attribute in attpack
    for (i=0; i<attrList.size(); i++) { 
      // TODO: check for #items, tk

      if (attrPurpose.compare("sourceInfo")==0) {
        if (attrList.at(i)->attrName.compare("siteCode")==0) {
          fprintf(xml, "            <%s network=\"LittleBearRiver\" "
                       "siteID=\"2\">%s</%s>\n",
                        attrList.at(i)->attrName.c_str(),
                       (attrList.at(i)->vcp).c_str(),
                        attrList.at(i)->attrName.c_str());
        } else if (attrList.at(i)->attrName.compare("latitude")==0) {
          fprintf(xml, "            <geoLocation>\n");
          fprintf(xml, "                <geogLocation xsi:type=\"LatLonPointType\" srs=\"EPSG:4269\">\n");
          fprintf(xml, "                    <%s>%s</%s>\n",
                  attrList.at(i)->attrName.c_str(),
                 (attrList.at(i)->vcp).c_str(),
                  attrList.at(i)->attrName.c_str());
        } else if (attrList.at(i)->attrName.compare("longitude")==0) {
          fprintf(xml, "                    <%s>%s</%s>\n",
                  attrList.at(i)->attrName.c_str(),
                 (attrList.at(i)->vcp).c_str(),
                  attrList.at(i)->attrName.c_str());
          fprintf(xml, "                </geogLocation>\n");
        } else if (attrList.at(i)->attrName.compare("X")==0) {
          fprintf(xml, "                <localSiteXY projectionInformation=\" NAD83 / UTM zone 12N\">\n");
          fprintf(xml, "                    <%s>%s</%s>\n",
                  attrList.at(i)->attrName.c_str(),
                 (attrList.at(i)->vcp).c_str(),
                  attrList.at(i)->attrName.c_str());
        } else if (attrList.at(i)->attrName.compare("Y")==0) {
          fprintf(xml, "                    <%s>%s</%s>\n",
                  attrList.at(i)->attrName.c_str(),
                 (attrList.at(i)->vcp).c_str(),
                  attrList.at(i)->attrName.c_str());
          fprintf(xml, "                </localSiteXY>\n");
          fprintf(xml, "            </geoLocation>\n");
        } else if (attrList.at(i)->attrName.compare("County")==0 ||
                   attrList.at(i)->attrName.compare("State")==0  ||
                   attrList.at(i)->attrName.compare("Site Comments")==0) {
          fprintf(xml, "            <note title=\"%s\">%s</note>\n",
                  attrList.at(i)->attrName.c_str(),
                 (attrList.at(i)->vcp).c_str());
        } else { // siteName or verticalDatum
          fprintf(xml, "            <%s>%s</%s>\n",
                  attrList.at(i)->attrName.c_str(),
                 (attrList.at(i)->vcp).c_str(),
                  attrList.at(i)->attrName.c_str());
        }
      } else if (attrPurpose.compare("variable")==0) {
        if (attrList.at(i)->attrName.compare("variableCode")==0) {
          fprintf(xml, "            <%s vocabulary=\"LBR\" "
                       "default=\"true\" variableID=\"39\">%s</%s>\n",
                        attrList.at(i)->attrName.c_str(),
                       (attrList.at(i)->vcp).c_str(),
                        attrList.at(i)->attrName.c_str());
        } else if (attrList.at(i)->attrName.compare("units")==0) {
          fprintf(xml, "            <%s unitsAbbreviation=\"mg/L\" "
                       "unitsCode=\"199\">%s</%s>\n",
                        attrList.at(i)->attrName.c_str(),
                       (attrList.at(i)->vcp).c_str(),
                        attrList.at(i)->attrName.c_str());
        } else if (attrList.at(i)->attrName.compare("timeSupport")==0) {
          fprintf(xml, "            <%s isRegular=\"%s\">\n",
                        attrList.at(i)->attrName.c_str(),
                       (attrList.at(i)->vcp).c_str());
        } else {
          fprintf(xml, "            <%s>%s</%s>\n",
                  attrList.at(i)->attrName.c_str(),
                 (attrList.at(i)->vcp).c_str(),
                  attrList.at(i)->attrName.c_str());
        }
      } else if (attrPurpose.compare("values")==0) {
        fprintf(xml, " %s=\"%s\"",
                attrList.at(i)->attrName.c_str(),
               (attrList.at(i)->vcp).c_str());
      } else if (attrPurpose.compare((attrPurpose.size()-2),2,".1")==0) {
        // <value>
        fprintf(xml, " %s=\"%s\"",
                attrList.at(i)->attrName.c_str(),
               (attrList.at(i)->vcp).c_str());
        if (attrList.at(i)->attrName.compare("dateTime")==0 ||
            attrList.at(i)->attrName.compare("sourceID")==0) {
          fprintf(xml, "\n                ");
        } else if (attrList.at(i)->attrName.compare("sampleID")==0) {
          fprintf(xml, ">");
          // TODO: print value from array here
          fprintf(xml, "%g",
            array3d[atoi((attrList.at(i)->attrPurpose.substr(5,2)).c_str())-1]);
        }
      } else if (attrPurpose.compare("method")==0) {
        if (attrList.at(i)->attrName.compare("methodID")==0) {
          fprintf(xml, " %s=\"%s\">\n",
                        attrList.at(i)->attrName.c_str(),
                       (attrList.at(i)->vcp).c_str());
        } else {
          fprintf(xml, "                <%s>%s</%s>\n",
                  attrList.at(i)->attrName.c_str(),
                 (attrList.at(i)->vcp).c_str(),
                  attrList.at(i)->attrName.c_str());
        }
      } else if (attrPurpose.compare("source")==0) {
        if (attrList.at(i)->attrName.compare("sourceID")==0) {
          fprintf(xml, " %s=\"%s\">\n",
                        attrList.at(i)->attrName.c_str(),
                       (attrList.at(i)->vcp).c_str());
        } else {
          fprintf(xml, "                <%s>%s</%s>\n",
                  attrList.at(i)->attrName.c_str(),
                 (attrList.at(i)->vcp).c_str(),
                  attrList.at(i)->attrName.c_str());
          if (attrList.at(i)->attrName.compare("SourceDescription")==0) {
            fprintf(xml, "                <ContactInformation>\n");
            fprintf(xml, "                    <ContactName>Amber Spackman</ContactName>\n");
            fprintf(xml, "                    <TypeOfContact>main</TypeOfContact>\n");
            fprintf(xml, "                    <Phone>1-435-797-0045</Phone>\n");
            fprintf(xml, "                    <Email>amber.s@aggiemail.usu.edu</Email>\n");
            fprintf(xml, "                    <Address> xsi:type=\"xsd:string\">8200 Old Main Hill ,Logan, Utah 84322-8200</Address>\n");
            fprintf(xml, "                </ContactInformation>\n");
          }
        }
      }
    }

    // attpack footer
    if (attrPurpose.compare("sourceInfo")==0) {
      fprintf(xml, "        </sourceInfo>\n");
    } else if (attrPurpose.compare("variable")==0) {
      fprintf(xml, "        </variable>\n");
    } else if (attrPurpose.compare("values")==0) {
      fprintf(xml, ">\n");
    } else if (attrPurpose.compare((attrPurpose.size()-2),2,".1")==0) {
      // <value>
      fprintf(xml, "</value>\n");
    } else if (attrPurpose.compare("method")==0) {
      fprintf(xml, "            </method>\n");
    } else if (attrPurpose.compare("source")==0) {
      fprintf(xml, "            </source>\n");
      fprintf(xml, "        </values>\n");
    }

  } // end if not TimeSeries attpack wrapper

  // recurse remaining attpacks
  for(i=0; i<packList.size(); i++)
    localrc = packList.at(i)->AttributeWriteWaterMLbuffieldT(xml,index,columns);

  return ESMF_SUCCESS;

 } // end AttributeWriteWaterMLbuffieldT
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
  unsigned int i;
  char msgbuf[4*ESMF_MAXSTR];

  // Initialize local return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  for (i=0; i<attrList.size(); i++) {
    sprintf(msgbuf, "   Attr %d:\n", i);
    printf("%s",msgbuf);
    ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  // print name
  sprintf(msgbuf, "        name: %s\n",  attrList.at(i)->attrName.c_str());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // print items if there are any
  if (attrList.at(i)->items <= 0) {
      sprintf(msgbuf, "        value: \n");
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  }

  if (attrList.at(i)->items == 1) {
      sprintf(msgbuf, "        value: ");
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
             if (attrList.at(i)->tk == ESMC_TYPEKIND_I4)
                 sprintf(msgbuf, "%d\n", attrList.at(i)->vi); 
             else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8)
                 sprintf(msgbuf, "%lld\n", attrList.at(i)->vl); 
             else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4)
                 sprintf(msgbuf, "%f\n", attrList.at(i)->vf); 
             else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8)
                 sprintf(msgbuf, "%g\n", attrList.at(i)->vd); 
             else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL)
                 sprintf(msgbuf, "%s\n", ESMC_LogicalString(attrList.at(i)->vb)); 
             else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER)
                 sprintf(msgbuf, "%s\n", attrList.at(i)->vcp.c_str());
             else{ 
                 sprintf(msgbuf, "unknown value");
                 ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ATTR_WRONGTYPE, msgbuf, &localrc);
                 return localrc;
             }
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  }

  if (attrList.at(i)->items > 1) { 
      sprintf(msgbuf, "        %d items, values:\n", attrList.at(i)->items);
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      for (unsigned int j=0; j<attrList.at(i)->items; j++) {
                if (attrList.at(i)->tk == ESMC_TYPEKIND_I4) {
                    sprintf(msgbuf, "          \t item %d: %d\n", j, attrList.at(i)->vip[j]); 
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_I8) {
                    sprintf(msgbuf, "          \t item %d: %lld\n", j, attrList.at(i)->vlp[j]); 
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_R4) {
                    sprintf(msgbuf, "          \t item %d: %f\n", j, attrList.at(i)->vfp[j]); 
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_R8) {
                    sprintf(msgbuf, "          \t item %d: %g\n", j, attrList.at(i)->vdp[j]); 
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_LOGICAL) {
                    sprintf(msgbuf, "          \t item %d: %s\n", j,
                      ESMC_LogicalString(attrList.at(i)->vbp[j]));
                } else if (attrList.at(i)->tk == ESMC_TYPEKIND_CHARACTER) {
                    sprintf(msgbuf, "          \t item %d: %s\n", j, attrList.at(i)->vcpp[j].c_str());
                } else{
                    sprintf(msgbuf, "          \t unknown value");
                    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &localrc);
                    return localrc;
                }
      printf("%s",msgbuf);
      ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
      }
  }

  // print convention
  sprintf(msgbuf, "        convention: %s\n",  attrList.at(i)->attrConvention.c_str());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // print purpose
  sprintf(msgbuf, "        purpose: %s\n",  attrList.at(i)->attrPurpose.c_str());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // print object
  sprintf(msgbuf, "        object: %s\n",  attrList.at(i)->attrObject.c_str());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);

  sprintf(msgbuf, "        attrCount: %d\n", attrList.at(i)->AttributeGetCountTotal());
  printf("%s",msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  }
  
  for (i=0; i<packList.size(); i++) {
    packList.at(i)->ESMC_Print();
  }
  for (i=0; i<linkList.size(); i++) {
    linkList.at(i)->ESMC_Print();
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

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);

  sprintf(name, "Attribute package - %s %s %s", 
    conv.c_str(), purp.c_str(), obj.c_str());
  attrName = name;

  vi = 0;
  vip.reserve(0);
  vl = 0;
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

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);

  vi = 0;
  vip.reserve(0);
  vl = 0;
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

  attrName = "";
  tk = ESMF_NOKIND;
  items = 0;
  attrRoot = ESMF_TRUE;

  attrConvention = "";
  attrPurpose = "";
  attrObject = "";
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);

  vi = 0;
  vip.reserve(0);
  vl = 0;
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

  attrName ="";
  tk = ESMF_NOKIND;
  items = 0;
  attrRoot = attributeRoot;

  attrConvention = "";
  attrPurpose = "";
  attrObject = "";
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);

  vi = 0;
  vip.reserve(0);
  vl = 0;
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
  attrRoot = ESMF_FALSE;
   
  attrConvention = "";
  attrPurpose = "";
  attrObject = "";
  attrPack = ESMF_FALSE;
  attrPackHead = ESMF_FALSE;
  attrNested = ESMF_FALSE;

  linkChange = ESMF_FALSE;
  structChange = ESMF_TRUE;
  valueChange = ESMF_FALSE;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;
  
  attrList.reserve(0);
  packList.reserve(0);
  linkList.reserve(0);
  
  vi = 0;
  vip.reserve(0);
  vl = 0;
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
                vl = *(static_cast<ESMC_I8*> (datap));  
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
                vl = *(static_cast<ESMC_I8*> (datap));  
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
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &localrc)) delete this;

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

  unsigned int i;

  attrBase = ESMC_NULL_POINTER;
  parent = ESMC_NULL_POINTER;

  if (items > 1) {
        if (tk == ESMC_TYPEKIND_I4) vip.clear();
        else if (tk == ESMC_TYPEKIND_I8) vlp.clear();
        else if (tk == ESMC_TYPEKIND_R4) vfp.clear();
        else if (tk == ESMC_TYPEKIND_R8) vdp.clear();  
        else if (tk == ESMC_TYPEKIND_LOGICAL) vbp.clear();
        else if (tk == ESMC_TYPEKIND_CHARACTER) vcpp.clear();
  }

  for (i=0; i<attrList.size(); i++)
    delete attrList.at(i);
  
  for (i=0; i<packList.size(); i++)
    delete packList.at(i);
  
  for (i=0; i<linkList.size(); i++)
    linkList.at(i) = ESMC_NULL_POINTER;
    
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
    int attrCount, packCount, linkCount;
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
    DESERIALIZE_VAR(buffer,loffset,packCount,int);
///    DESERIALIZE_VAR(buffer,loffset,linkCount,int);
        
    attrList.reserve(attrCount);
    packList.reserve(packCount);
//    linkList.reserve(linkCount);

    if (items == 1) {
      if (tk == ESMC_TYPEKIND_I4) {
        DESERIALIZE_VAR(buffer,loffset,vi,ESMC_I4); }
      else if (tk == ESMC_TYPEKIND_I8) {
        DESERIALIZE_VAR(buffer,loffset,vl,ESMC_I8); }
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
    for (i=0; i<attrCount; i++) {
      attr = NULL;
      attr = new Attribute(ESMF_FALSE);
      if (!attr)
        return ESMF_FAILURE;
      attr->setBase(attrBase);
      attr->parent = this;
      localrc = attr->ESMC_Deserialize(buffer,&loffset);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttributeSet(attr);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &localrc)) return localrc;
    }
      
    for (i=0; i<packCount; i++) {
      attr = NULL;
      attr = new Attribute(ESMF_FALSE);
      if (!attr)
        return ESMF_FAILURE;
      attr->setBase(attrBase);
      attr->parent = this;
      localrc = attr->ESMC_Deserialize(buffer,&loffset);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &localrc)) return localrc;
      localrc = AttPackSet(attr);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &localrc)) return localrc;
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
      int *offset,           // inout - original offset, updated to point 
                             //  to first free byte after current obj info
      ESMC_InquireFlag inquireflag) const { // in - inquire flag
// 
// !DESCRIPTION:
//    Turn an {\tt Attribute} into a stream of bytes.
//
//EOPI
    int loffset=*offset;
    bool cc;
    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;
    cc = false;
    localrc = ESMC_SerializeCC(buffer,length,loffset,cc,inquireflag);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &localrc)) return localrc;
    if (inquireflag != ESMF_INQUIREONLY) {
      cc = true;
      localrc = ESMC_SerializeCC(buffer,length,*offset,cc,inquireflag);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &localrc)) return localrc;
    } else
      *offset = loffset;

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
      bool cc,               // in - to tell whether in count or copy mode 
      ESMC_InquireFlag inquireflag) const { // in - inquire flag
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
          
      SERIALIZE_VAR(cc,buffer,offset,attrList.size(),int);
      SERIALIZE_VAR(cc,buffer,offset,packList.size(),int);
//      SERIALIZE_VAR(cc,buffer,offset,linkList.size(),int);

      if (items == 1) {
        if (tk == ESMC_TYPEKIND_I4) {
          SERIALIZE_VAR(cc,buffer,offset,vi,ESMC_I4); }
        else if (tk == ESMC_TYPEKIND_I8) {
          SERIALIZE_VAR(cc,buffer,offset,vl,ESMC_I8); }
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
      for (i=0; i<attrList.size(); i++)
          attrList.at(i)->ESMC_SerializeCC(buffer,length,offset,cc,inquireflag);
  
      for (i=0; i<packList.size(); i++)
          packList.at(i)->ESMC_SerializeCC(buffer,length,offset,cc,inquireflag);
  
/*      for (i=0; i<linkList.size(); i++)
          linkList.at(i)->ESMC_SerializeCC(buffer,length,offset,cc,inquireflag); */
  
      // make sure offset is aligned correctly
      nbytes=offset%8;
      if (nbytes!=0) offset += 8-nbytes;
      
      // check if buffer has enough free memory, expand?
      if (inquireflag != ESMF_INQUIREONLY) {
        if (*length < offset){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_MEM_ALLOCATE, 
            "Buffer too short to add an Attribute hierarchy", &localrc);
          return localrc;
        }
      }
      
    // Undefine serialization macros, so they don't cause troubles elsewhere
#undef SERIALIZE_VAR
#undef SERIALIZE_VARC

  // return successfully
  return ESMF_SUCCESS;

 } // end ESMC_SerializeCC
//-----------------------------------------------------------------------------
} // namespace ESMCI
