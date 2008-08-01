// $Id: ESMC_Attribute.h,v 1.13 2008/08/01 19:25:01 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Attribute C++ declaration include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMC_ATTRIBUTE_H
#define ESMC_ATTRIBUTE_H

//-----------------------------------------------------------------------------

#include "stdio.h"

#include "ESMCI_Util.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Attribute
//
// !DESCRIPTION:
// The code in this file implements the Attribute defined type
// and methods.
//
//-----------------------------------------------------------------------------
//
// !USES:

// !PUBLIC TYPES:
  class ESMC_Attribute;
  class ESMC_Base;


// Single Attribute, (name, value) pair which can contain:
//  int / Integer*4, single value or list
//  double / Real*8, single value or list
//  char / Character - single character string (lists not allowed)
//  ESMC_Logical - single value or arrays (note: not bool or .TRUE.)
class ESMC_Attribute 
{
 private:
    char attrName[ESMF_MAXSTR]; // inline to reduce memory thrashing
    ESMC_TypeKind tk;           // typekind indicator
    int items;                  // number of items (NOT byte count) for lists
    int slen;                   // for string, length, inc trailing NULL. 

    char attrConvention[ESMF_MAXSTR];             // for att packages
    char attrPurpose[ESMF_MAXSTR];                // for att packages
    char attrObject[ESMF_MAXSTR];                 // for att packages
    ESMC_Logical attrPack;                         // for att packages

    int attrCount;              // number of attributes in use in list
    int attrAlloc;              // number of attributes currently allocated
    ESMC_Attribute **attrList;  // attributes - array of pointers

    // ***FIXME*** members of a union are public by default
    union {                     // overload pointers to conserve space 
      ESMC_I4    vi;       // integer, or
      ESMC_I4  *vip;       // pointer to integer list, or
      ESMC_I8   vtl;       // long, or
                  //  ^  TODO: change back to vl when Cray X1 compiler fixed
      ESMC_I8  *vlp;       // pointer to long list, or
      ESMC_R4    vf;       // float (real*4), or
      ESMC_R4  *vfp;       // pointer to float (real*4) list, or
      ESMC_R8    vd;       // double (real*8), or
      ESMC_R8  *vdp;       // pointer to double (real*8) list, or
      ESMC_Logical    vb;       // boolean (logical), or
      ESMC_Logical  *vbp;       // pointer to boolean (logical) list, or
      char          *vcp;       // pointer to a NULL term character string, or
      char        **vcpp;
      void        *voidp;       // cannot be dereferenced, but generic.
      // ESMC_Array  *ap;       // pointer to an ESMC_Array object (someday?)
    };

    // prevent accidental copying
    //ESMC_Attribute& operator=(const ESMC_Attribute&);
    ESMC_Attribute(const ESMC_Attribute&);

 public:
    // attpack methods
    int ESMC_AttPackCreate(char *name, char *convention, char *purpose, char *object);
    ESMC_Attribute *ESMC_AttPackGet(char *convention, char *purpose, char *object) const;
    ESMC_Attribute *ESMC_AttPackGetAttribute(char *name, 
                                             char *convention, char *purpose, char *object) const;
    int ESMC_AttPackSet(char *name, ESMC_TypeKind tk, int count, void *value, 
                        char *convention, char *purpose, char *object);
    
    // extend pointer list
    int ESMC_AttributeAlloc(int adding);
    
    // copy an attribute hierarchy
    int ESMC_AttributeCopyAll(ESMC_Base *source);
    
    // count the number of objects in an attribute hierarchy
    int ESMC_AttributeCountTree(char *convention, 
      char *purpose, char *object, int &ans) const;
    
    // attribute methods - get
    int ESMC_AttributeGet(char *name, ESMC_I4 *value) const;
    int ESMC_AttributeGet(char *name, int *count, ESMC_I4 *value) const;
    int ESMC_AttributeGet(char *name, ESMC_I8 *value) const;
    int ESMC_AttributeGet(char *name, int *count, ESMC_I8 *value) const;
    int ESMC_AttributeGet(char *name, ESMC_R4 *value) const;
    int ESMC_AttributeGet(char *name, int *count, ESMC_R4 *value) const;
    int ESMC_AttributeGet(char *name, ESMC_R8 *value) const;
    int ESMC_AttributeGet(char *name, int *count, ESMC_R8 *value) const;
    int ESMC_AttributeGet(char *name, ESMC_Logical *value) const;
    int ESMC_AttributeGet(char *name, int *count, ESMC_Logical *value) const;
    int ESMC_AttributeGet(char *name, char *value) const;
    int ESMC_AttributeGet(char *name, ESMC_TypeKind *tk, int *count, 
      void *value) const;
    int ESMC_AttributeGet(int num, char *name, ESMC_TypeKind *tk, int *count,
      void *value) const;

    // getting either by name or number directly return attribute ptr
    ESMC_Attribute *ESMC_AttributeGet(char *name) const; 
    ESMC_Attribute *ESMC_AttributeGet(int num) const;

    // helper to get lengths of strings in a char* list
    int ESMC_AttributeGet(char *name, int *lens, int count) const;

    // count of attributes on an object
    int ESMC_AttributeGetCount(void) const;
    
    // number of items on an attribute
    int ESMC_AttributeGetItemCount(char *name) const;

    // setting when you have an attribute already assembled
    int ESMC_AttributeSet(ESMC_Attribute *attr);

    // attribute methods - set
    int ESMC_AttributeSet(char *name, ESMC_I4 value);
    int ESMC_AttributeSet(char *name, int count, ESMC_I4 *value);
    int ESMC_AttributeSet(char *name, ESMC_I8 value);
    int ESMC_AttributeSet(char *name, int count, ESMC_I8 *value);
    int ESMC_AttributeSet(char *name, ESMC_R4 value);
    int ESMC_AttributeSet(char *name, int count, ESMC_R4 *value);
    int ESMC_AttributeSet(char *name, ESMC_R8 value);
    int ESMC_AttributeSet(char *name, int count, ESMC_R8 *value);
    int ESMC_AttributeSet(char *name, ESMC_Logical value);
    int ESMC_AttributeSet(char *name, int count, ESMC_Logical *value);
    int ESMC_AttributeSet(char *name, char *value);
    int ESMC_AttributeSet(char *name, ESMC_TypeKind tk, int count, void *value);
    
    // attribute set a link in hierarchy
    int ESMC_AttributeSetLink(ESMC_Base *destination);

    // attribute write methods
    int ESMC_AttributeWriteTab(char *convention, char *purpose, char *object, 
                          char *varobj, char *basename, int &count) const;

    int ESMC_AttributeWriteXML(char *convention, char *purpose, char *object, 
                               char *varobj, char *basename) const;
    
    int ESMC_AttributeWriteXMLrecurse(FILE *xml, char *convention, char *purpose, char *object, 
                               char *varobj, int stop, int& fldcount) const;

    // not implemented yet
    int ESMC_AttributeCopy(char *name, ESMC_Attribute *destination);
    int ESMC_AttributeGetList(char **namelist, ESMC_Attribute *valuelist) const;
    int ESMC_AttributeGetNameList(int *count, char **namelist) const;
    int ESMC_AttributeSetList(int count, ESMC_Attribute *valuelist);
    
    // Modifiers, Constructors, Destructors, Serializers, Print
    ESMC_Attribute(void);
    ESMC_Attribute(char *name, ESMC_TypeKind tk, int numitems, void *datap);
    ESMC_Attribute(char *name, char *conv, char *purp, char *obj);
    ~ESMC_Attribute(void);
    int ESMC_AttrModifyValue(ESMC_TypeKind typekind,int numitems,void *datap);
    int ESMC_Deserialize(char *buffer, int *offset);
    int ESMC_Serialize(char *buffer, int *length, int *offset) const;
    int ESMC_SerializeCC(char *buffer, int *length, int &offset, bool cc) const;
    int ESMC_Print(void) const;

    // temporary copy constructor
    ESMC_Attribute& operator=(const ESMC_Attribute& source);
};

// fortran interface functions to attribute objects
extern "C" {
  void FTN(c_esmc_attpackcreate)(ESMC_Base **base, char *name, char *convention, char *purpose, 
                                 char *object, int *rc, int nlen, int clen, int plen, int olen);
  void FTN(c_esmc_attpackgetchar)(ESMC_Base **base, char *name, char *value, 
                                  char *convention, char *purpose, char *object, int *rc, int nlen, 
                                  int vlen, int clen, int plen, int olen);
  void FTN(c_esmc_attpackgetcharlist)(ESMC_Base **base, char *name, ESMC_TypeKind *tk, int *count,
                                  int *lens, char *valueList, char *convention, char *purpose, 
                                  char *object, int *rc, int nlen, int vlen, int clen, int plen, 
                                  int olen);
  void FTN(c_esmc_attpackgetvalue)(ESMC_Base **base, char *name, ESMC_TypeKind *tk, int *count,
                                  void *value, char *convention, char *purpose, char *object, 
                                  int *rc, int nlen, int clen, int plen, int olen);
  void FTN(c_esmc_attpacksetchar)(ESMC_Base **base, char *name, char *value, ESMC_TypeKind *tk, 
                                  char *convention, char *purpose, char *object, int *rc, int nlen, 
                                  int vlen, int clen, int plen, int olen);
  void FTN(c_esmc_attpacksetcharlist)(ESMC_Base **base, char *name, ESMC_TypeKind *tk, int *count,
                                   char *valueList, int *lens, char *convention, char *purpose, 
                                   char *object, int *rc, int nlen, int vlen, int clen, int plen, 
                                   int olen);
  void FTN(c_esmc_attpacksetvalue)(ESMC_Base **base, char *name, ESMC_TypeKind *tk, int *count,
                                  void *value, char *convention, char *purpose, char *object, 
                                  int *rc, int nlen, int clen, int plen, int olen);
  void FTN(c_esmc_attributewritetab)(ESMC_Base **base, char *convention, char *purpose,
                                  char *object, char *targetobj, int *rc, int clen, int plen, 
                                  int olen, int tlen);
  void FTN(c_esmc_attributewritexml)(ESMC_Base **base, char *convention, char *purpose,
                                  char *object, char *targetobj, int *rc, 
                                  int clen, int plen, int olen, int tlen);
  void FTN(c_esmc_attributecopyall)(ESMC_Base **source, ESMC_Base **destination, int *rc);
  void FTN(c_esmc_attributegetcount)(ESMC_Base **base, int *count, int *rc);
  void FTN(c_esmc_attributegetinfoname)(ESMC_Base **base, char *name, 
                                        ESMC_TypeKind *tk,
                                        int *count, int *rc, int nlen);
  void FTN(c_esmc_attributegetinfonum)(ESMC_Base **base, int *num, 
                                      char *name,
                                      ESMC_TypeKind *tk, int *count, 
                                      int *rc, int nlen);
  void FTN(c_esmc_attributegetchar)(ESMC_Base **base, char *name, char *value, 
                                    int *rc, int nlen, int vlen);
  void FTN(c_esmc_attributegetvalue)(ESMC_Base **base, char *name, 
                                    ESMC_TypeKind *tk,
                                    int *count, void *value, int *rc, int nlen);
  void FTN(c_esmc_attributesetchar)(ESMC_Base **base, char *name, char *value, 
                                    int *rc, int nlen, int vlen);
  void FTN(c_esmc_attributesetcharlist)(ESMC_Base **base, char *name, ESMC_TypeKind *tk,
                                        int *count, char *valueList, int *lens, int *rc, int nlen);
  void FTN(c_esmc_attributesetvalue)(ESMC_Base **base, char *name, ESMC_TypeKind *tk,
                                    int *count, void *value, int *rc, int nlen);
  void FTN(c_esmc_attributesetlink)(ESMC_Base **source, ESMC_Base **destination, int *rc);
  }

// class utility functions, not methods, since they operate on
//  multiple objects
int ESMC_AttributeSetObjectList(ESMC_Base *anytypelist, ESMC_Attribute *valuelist);
int ESMC_AttributeGetObjectList(ESMC_Base *anytypelist, ESMC_Attribute *valuelist);

#endif  // ESMC_ATTRIBUTE_H
