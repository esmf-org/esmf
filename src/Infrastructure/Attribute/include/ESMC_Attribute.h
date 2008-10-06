// $Id: ESMC_Attribute.h,v 1.18 2008/10/06 19:15:19 rokuingh Exp $
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
#include <string>
using std::string;
#include <vector>
using std::vector;

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
    string attrName; // inline to reduce memory thrashing
    ESMC_TypeKind tk;           // typekind indicator
    int items;                  // number of items (NOT byte count) for lists
    int slen;                   // for string, length, inc trailing NULL.
    ESMC_Logical attrRoot;
  
    string attrConvention;             // for att packages
    string attrPurpose;                // for att packages
    string attrObject;                 // for att packages
    ESMC_Logical attrPack;                         // for att packages

    int attrCount;              // number of attributes in use in list
    int attrAlloc;              // number of attributes currently allocated
    ESMC_Attribute **attrList;  // attributes - array of pointers

    // Attribute values
    ESMC_I4               vi;       // integer, or
    vector<ESMC_I4>       vip;       // pointer to integer list, or
    ESMC_I8               vtl;       // long, or
                            //  ^  TODO: change back to vl when Cray X1 compiler fixed
    vector<ESMC_I8>       vlp;       // pointer to long list, or
    ESMC_R4               vf;       // float (real*4), or
    vector<ESMC_R4>       vfp;       // pointer to float (real*4) list, or
    ESMC_R8               vd;       // double (real*8), or
    vector<ESMC_R8>       vdp;       // pointer to double (real*8) list, or
    ESMC_Logical          vb;       // boolean (logical), or
    vector<ESMC_Logical>  vbp;       // pointer to boolean (logical) list, or
    string                vcp;       // pointer to a NULL term character string, or
    vector<string>        vcpp;
    //void                  *voidp;       // cannot be dereferenced, but generic.

    // prevent accidental copying
    //ESMC_Attribute& operator=(const ESMC_Attribute&);
    ESMC_Attribute(const ESMC_Attribute&);

//-----------------------------------------------------------------------------
 public:
    // attpack methods
    int ESMC_AttPackCreate(const string &name, const string &convention, 
      const string &purpose, const string &object);
    ESMC_Attribute *ESMC_AttPackGet(const string &convention, 
      const string &purpose, const string &object) const;
    ESMC_Attribute *ESMC_AttPackGetAttribute(const string &name, 
      const string &convention, const string &purpose, 
      const string &object) const;
    int ESMC_AttPackIsPresent(const string &name, const string &convention, 
      const string &purpose, const string &object, ESMC_Logical *present) const;
    int ESMC_AttPackSet(const string &name, const ESMC_TypeKind &tk, 
      int count, void *value, const string &convention, 
      const string &purpose, const string &object);
    
    // extend pointer list
    int ESMC_AttributeAlloc(int adding);
    
    // copy an attribute hierarchy
    int ESMC_AttributeCopyAll(ESMC_Base *source);
    
    // count the number of objects in an attribute hierarchy
    int ESMC_AttributeCountTree(const string &convention, const string &purpose,  
      const string &object, int &objCount, int &objmaxattrCount) const;
    int ESMC_AttributeCountTreeLens(const string &convention, const string &purpose,  
      const string &object, int *attrLens, vector<string> &attrNames) const;
    
    // destroy an attribute or attpack
    int ESMC_AttributeRemove(const string &name, const string &convention, 
      const string &purpose, const string &object);
    int ESMC_AttributeRemove(const string &name);
    
    // attribute methods - get
    int ESMC_AttributeGet(const string &name, ESMC_I4 *value) const;
    int ESMC_AttributeGet(const string &name, int *count, vector<ESMC_I4> *value) const;
    int ESMC_AttributeGet(const string &name, ESMC_I8 *value) const;
    int ESMC_AttributeGet(const string &name, int *count, vector<ESMC_I8> *value) const;
    int ESMC_AttributeGet(const string &name, ESMC_R4 *value) const;
    int ESMC_AttributeGet(const string &name, int *count, vector<ESMC_R4> *value) const;
    int ESMC_AttributeGet(const string &name, ESMC_R8 *value) const;
    int ESMC_AttributeGet(const string &name, int *count, vector<ESMC_R8> *value) const;
    int ESMC_AttributeGet(const string &name, ESMC_Logical *value) const;
    int ESMC_AttributeGet(const string &name, int *count, vector<ESMC_Logical> *value) const;
    int ESMC_AttributeGet(const string &name, string *value) const;
    int ESMC_AttributeGet(const string &name, vector<string> *value) const;
    int ESMC_AttributeGet(const string &name, ESMC_TypeKind *tk, int *count, 
      void *value) const;
    int ESMC_AttributeGet(int num, string &name, ESMC_TypeKind *tk, int *count,
      void *value) const;

    // getting either by name or number directly return attribute ptr
    ESMC_Attribute *ESMC_AttributeGet(const string &name) const; 
    ESMC_Attribute *ESMC_AttributeGet(int num) const;

    // helper to get lengths of strings in a char* list
    int ESMC_AttributeGet(const string &name, int *lens, int count) const;

    // count of attributes on an object
    int ESMC_AttributeGetCount(void) const;
    
    // number of items on an attribute
    int ESMC_AttributeGetItemCount(const string &name) const;

    // query for existence of an attribute
    int ESMC_AttributeIsPresent(const string &name, ESMC_Logical *present) const;

    // setting when you have an attribute already assembled
    int ESMC_AttributeSet(ESMC_Attribute *attr);

    // attribute methods - set
    int ESMC_AttributeSet(const string &name, ESMC_I4 value);
    int ESMC_AttributeSet(const string &name, int count, vector<ESMC_I4> *value);
    int ESMC_AttributeSet(const string &name, ESMC_I8 value);
    int ESMC_AttributeSet(const string &name, int count, vector<ESMC_I8> *value);
    int ESMC_AttributeSet(const string &name, ESMC_R4 value);
    int ESMC_AttributeSet(const string &name, int count, vector<ESMC_R4> *value);
    int ESMC_AttributeSet(const string &name, ESMC_R8 value);
    int ESMC_AttributeSet(const string &name, int count, vector<ESMC_R8> *value);
    int ESMC_AttributeSet(const string &name, ESMC_Logical value);
    int ESMC_AttributeSet(const string &name, int count, vector<ESMC_Logical> *value);
    int ESMC_AttributeSet(const string &name, string *value);
    int ESMC_AttributeSet(const string &name, int count, vector<string> *value);
//    int ESMC_AttributeSet(const string &name, const ESMC_TypeKind &tk, 
//      int count, void *value);
    
    // attribute set a link in hierarchy
    int ESMC_AttributeSetLink(ESMC_Base *destination);

    // attribute set a link in hierarchy
    int ESMC_AttributeSetObjsInTree(const string &name, const string &object, 
      const ESMC_TypeKind &tk, int count, void *value);

    // attribute write methods
    int ESMC_AttributeWriteTab(const string &convention, const string &purpose, 
      const string &object, const string &varobj, const string &basename) const;
    int ESMC_AttributeWriteTabrecurse(FILE *tab, const string &convention, 
      const string &purpose, const string &obj, int *attrLens, 
      const vector<string> &attrNames, const int &maxattrs, int &count) const;
    int ESMC_AttributeWriteXML(const string &convention, const string &purpose, 
      const string &object, const string &varobj, const string &basename) const;
    int ESMC_AttributeWriteXMLrecurse(FILE *xml, const string &convention, 
      const string &purpose, const string &object, const string &varobj, 
      const int &stop, int &fldcount) const;
    int ESMC_Print(void) const;

    // Modifiers, Constructors, Destructors, Serializers, Operators
    ESMC_Attribute(const string &name, const string &conv, const string &purp, 
      const string &obj);
    ESMC_Attribute(void);
    ESMC_Attribute(const ESMC_Logical &attributeRoot);
    ESMC_Attribute(const string &name, const ESMC_TypeKind &typekind, 
      int numitems, void *datap);
    int ESMC_AttrModifyValue(const ESMC_TypeKind &typekind, int numitems, 
      void *datap);
    ESMC_Attribute& operator=(const ESMC_Attribute &source);
    ~ESMC_Attribute(void);
    int ESMC_Deserialize(char *buffer, int *offset);
    int ESMC_Serialize(char *buffer, int *length, int *offset) const;
    int ESMC_SerializeCC(char *buffer, int *length, int &offset, bool cc) const;
};

// fortran interface functions to attribute objects
extern "C" {
  void FTN(c_esmc_attpackcreate)(ESMC_Base **base, char *name, char *convention, char *purpose, 
                                 char *object, int *rc, int nlen, int clen, int plen, int olen);
  void FTN(c_esmc_attpackdestroy)(ESMC_Base **base, char *name, char *convention, char *purpose,
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
  void FTN(c_esmc_attpackispresent)(ESMC_Base **base, char *name, char *convention, char *purpose, 
                                  char *object, ESMC_Logical *present, int *rc, int nlen, 
                                  int clen, int plen, int olen);
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
  void FTN(c_esmc_attributedestroy)(ESMC_Base **base, char *name, int *rc, int nlen);
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
  void FTN(c_esmc_attributeispresent)(ESMC_Base **base, char *name, ESMC_Logical *present, 
                                    int *rc, int nlen);
  void FTN(c_esmc_attributesetchar)(ESMC_Base **base, char *name, char *value, 
                                    int *rc, int nlen, int vlen);
  void FTN(c_esmc_attributesetcharlist)(ESMC_Base **base, char *name, ESMC_TypeKind *tk,
                                        int *count, char *valueList, int *lens, int *rc, int nlen);
  void FTN(c_esmc_attributesetvalue)(ESMC_Base **base, char *name, ESMC_TypeKind *tk,
                                    int *count, void *value, int *rc, int nlen);
  void FTN(c_esmc_attributesetlink)(ESMC_Base **source, ESMC_Base **destination, int *rc);
  void FTN(c_esmc_attributesetobjsintree)(ESMC_Base **base, char *object, char *name, 
                                          ESMC_TypeKind *tk, int *count, void *value, 
                                          int *rc, int olen, int nlen);
  }

// class utility functions, not methods, since they operate on
//  multiple objects
//int ESMC_AttributeSetObjectList(ESMC_Base *anytypelist, ESMC_Attribute *valuelist);
//int ESMC_AttributeGetObjectList(ESMC_Base *anytypelist, ESMC_Attribute *valuelist);

#endif  // ESMC_ATTRIBUTE_H
