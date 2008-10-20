// $Id: ESMCI_Attribute.h,v 1.3 2008/10/20 22:13:28 rokuingh Exp $
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

#ifndef ESMCI_ATTRIBUTE_H
#define ESMCI_ATTRIBUTE_H

//-----------------------------------------------------------------------------

#include <cstring>
#include <string>
#include <vector>
using namespace std;

#include "ESMCI_Util.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMCI_Attribute
//
// !DESCRIPTION:
// The code in this file implements the Attribute defined type
// and methods.
//
//-----------------------------------------------------------------------------
//
// !USES:


// !PUBLIC TYPES:
  class ESMC_Base;

namespace ESMCI {

  class ESMCI_Attribute;

class ESMCI_Attribute 
{
 private:
    string attrName; // inline to reduce memory thrashing
    ESMC_TypeKind tk;           // typekind indicator
    int items;                  // number of items (NOT byte count) for lists
    int slen;                   // for string, length, inc trailing NULL.
    ESMC_Logical attrRoot;
  
    string attrConvention;             // Convention of Attpack
    string attrPurpose;                // Purpose of Attpack
    string attrObject;                 // Object of Attpack
    ESMC_Logical attrPack;             // an Attribute in an Attpack
    ESMC_Logical attrPackHead;         // the head of an Attpack

    int attrID;                 // ID of the attribute
    int attrCount;              // number of attributes in use in list
    vector<ESMCI_Attribute*>  attrList;  // attributes - array of pointers

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

    // prevent accidental copying
    ESMCI_Attribute(const ESMCI_Attribute&);

//-----------------------------------------------------------------------------
 public:
    // attpack methods
    int ESMCI_AttPackCreate(const string &name, const string &convention, 
      const string &purpose, const string &object);
    ESMCI_Attribute *ESMCI_AttPackGet(const string &convention, 
      const string &purpose, const string &object) const;
    ESMCI_Attribute *ESMCI_AttPackGetAttribute(const string &name, 
      const string &convention, const string &purpose, 
      const string &object) const;
    int ESMCI_AttPackGetIndex(const string &convention, 
      const string &purpose, const string &object) const;
    ESMCI_Attribute *ESMCI_AttPackGetNested(bool &done) const;
    int ESMCI_AttPackIsPresent(const string &name, const string &convention, 
      const string &purpose, const string &object, ESMC_Logical *present) const;
    int ESMCI_AttPackRemove(const string &convention, 
      const string &purpose, const string &object);
    int ESMCI_AttPackRemoveAttribute(const string &name, const string &convention, 
      const string &purpose, const string &object);
    int ESMCI_AttPackSet(const string &name, const ESMC_TypeKind &tk, 
      int count, void *value, const string &convention, 
      const string &purpose, const string &object);
    
    // copy an attribute hierarchy
    int ESMCI_AttributeCopyAll(ESMC_Base *source);
    
    // count the number of objects in an attribute hierarchy
    int ESMCI_AttributeCountTree(const string &convention, const string &purpose,  
      const string &object, int &objCount, int &objmaxattrCount) const;
    int ESMCI_AttributeCountTreeLens(const string &convention, const string &purpose,  
      const string &object, int *attrLens, vector<string> &attrNames) const;
    
    // attribute methods - get
    int ESMCI_AttributeGet(const string &name, ESMC_I4 *value) const;
    int ESMCI_AttributeGet(const string &name, int *count, vector<ESMC_I4> *value) const;
    int ESMCI_AttributeGet(const string &name, ESMC_I8 *value) const;
    int ESMCI_AttributeGet(const string &name, int *count, vector<ESMC_I8> *value) const;
    int ESMCI_AttributeGet(const string &name, ESMC_R4 *value) const;
    int ESMCI_AttributeGet(const string &name, int *count, vector<ESMC_R4> *value) const;
    int ESMCI_AttributeGet(const string &name, ESMC_R8 *value) const;
    int ESMCI_AttributeGet(const string &name, int *count, vector<ESMC_R8> *value) const;
    int ESMCI_AttributeGet(const string &name, ESMC_Logical *value) const;
    int ESMCI_AttributeGet(const string &name, int *count, vector<ESMC_Logical> *value) const;
    int ESMCI_AttributeGet(const string &name, string *value) const;
    int ESMCI_AttributeGet(const string &name, vector<string> *value) const;

    // getting either by name or number directly return attribute ptr
    ESMCI_Attribute *ESMCI_AttributeGet(const string &name) const; 
    ESMCI_Attribute *ESMCI_AttributeGet(int num) const;

    // get attribute info
    int ESMCI_AttributeGet(const string &name, int *lens, int count) const;
    int ESMCI_AttributeGet(const string &name, ESMC_TypeKind *tk, int *itemCount) const;
    int ESMCI_AttributeGet(int num, string *name, ESMC_TypeKind *tk, int *itemCount) const;
    int ESMCI_AttributeGetCount(void) const;
    int ESMCI_AttributeGetItemCount(const string &name) const;

    // query for existence of an attribute
    int ESMCI_AttributeIsPresent(const string &name, ESMC_Logical *present) const;

    // destroy an attribute or attpack
    int ESMCI_AttributeRemove(const string &name);
    
    // setting when you have an attribute already assembled
    int ESMCI_AttributeSet(ESMCI_Attribute *attr);

    // attribute methods - set
    int ESMCI_AttributeSet(const string &name, ESMC_I4 value);
    int ESMCI_AttributeSet(const string &name, int count, vector<ESMC_I4> *value);
    int ESMCI_AttributeSet(const string &name, ESMC_I8 value);
    int ESMCI_AttributeSet(const string &name, int count, vector<ESMC_I8> *value);
    int ESMCI_AttributeSet(const string &name, ESMC_R4 value);
    int ESMCI_AttributeSet(const string &name, int count, vector<ESMC_R4> *value);
    int ESMCI_AttributeSet(const string &name, ESMC_R8 value);
    int ESMCI_AttributeSet(const string &name, int count, vector<ESMC_R8> *value);
    int ESMCI_AttributeSet(const string &name, ESMC_Logical value);
    int ESMCI_AttributeSet(const string &name, int count, vector<ESMC_Logical> *value);
    int ESMCI_AttributeSet(const string &name, string *value);
    int ESMCI_AttributeSet(const string &name, int count, vector<string> *value);
//    int ESMCI_AttributeSet(const string &name, const ESMC_TypeKind &tk, 
//      int count, void *value);
    
    // attribute set a link in hierarchy
    int ESMCI_AttributeSetLink(ESMC_Base *destination);

    // recursive call to set all attributes with attrObject = object
    int ESMCI_AttributeSetObjsInTree(const string &name, const string &object, 
      const ESMC_TypeKind &tk, int count, void *value);

    // attribute write methods
    int ESMCI_AttributeWriteTab(const string &convention, const string &purpose, 
      const string &object, const string &varobj, const string &basename) const;
    int ESMCI_AttributeWriteTabrecurse(FILE *tab, const string &convention, 
      const string &purpose, const string &obj, int *attrLens, 
      const vector<string> &attrNames, const int &maxattrs, int &count) const;
    int ESMCI_AttributeWriteXML(const string &convention, const string &purpose, 
      const string &object, const string &varobj, const string &basename) const;
    int ESMCI_AttributeWriteXMLrecurse(FILE *xml, const string &convention, 
      const string &purpose, const string &object, const string &varobj, 
      const int &stop, int &fldcount) const;
    int ESMC_Print(void) const;

    // Modifiers, Constructors, Destructors, Serializers, Operators
    ESMCI_Attribute(const string &conv, const string &purp, const string &obj);
    ESMCI_Attribute(const string &name, const string &conv, const string &purp, 
      const string &obj);
    ESMCI_Attribute(void);
    ESMCI_Attribute(const ESMC_Logical &attributeRoot);
    ESMCI_Attribute(const string &name, const ESMC_TypeKind &typekind, 
      int numitems, void *datap);
    int ESMCI_AttrModifyValue(const ESMC_TypeKind &typekind, int numitems, 
      void *datap);
    ESMCI_Attribute& operator=(const ESMCI_Attribute &source);
    ~ESMCI_Attribute(void);
    int ESMC_Deserialize(char *buffer, int *offset);
    int ESMC_Serialize(char *buffer, int *length, int *offset) const;
    int ESMC_SerializeCC(char *buffer, int *length, int &offset, bool cc) const;
};
} // namespace

// fortran interface functions to attribute objects
extern "C" {
  void FTN(c_esmci_attpackcreate)(ESMC_Base **base, char *name, char *convention, char *purpose, 
                                 char *object, int *rc, int nlen, int clen, int plen, int olen);
  void FTN(c_esmci_attpackdestroy)(ESMC_Base **base, char *name, char *convention, char *purpose,
                                    char *object, int *rc, int nlen, int clen, int plen, int olen);
  void FTN(c_esmci_attpackgetchar)(ESMC_Base **base, char *name, char *value, 
                                  char *convention, char *purpose, char *object, int *rc, int nlen, 
                                  int vlen, int clen, int plen, int olen);
  void FTN(c_esmci_attpackgetcharlist)(ESMC_Base **base, char *name, ESMC_TypeKind *tk, int *count,
                                  int *lens, char *valueList, char *convention, char *purpose, 
                                  char *object, int *rc, int nlen, int vlen, int clen, int plen, 
                                  int olen);
  void FTN(c_esmci_attpackgetvalue)(ESMC_Base **base, char *name, ESMC_TypeKind *tk, int *count,
                                  void *value, char *convention, char *purpose, char *object, 
                                  int *rc, int nlen, int clen, int plen, int olen);
  void FTN(c_esmci_attpackispresent)(ESMC_Base **base, char *name, char *convention, char *purpose, 
                                  char *object, ESMC_Logical *present, int *rc, int nlen, 
                                  int clen, int plen, int olen);
  void FTN(c_esmci_attpacksetchar)(ESMC_Base **base, char *name, char *value, ESMC_TypeKind *tk, 
                                  char *convention, char *purpose, char *object, int *rc, int nlen, 
                                  int vlen, int clen, int plen, int olen);
  void FTN(c_esmci_attpacksetcharlist)(ESMC_Base **base, char *name, ESMC_TypeKind *tk, int *count,
                                   char *valueList, int *lens, char *convention, char *purpose, 
                                   char *object, int *rc, int nlen, int vlen, int clen, int plen, 
                                   int olen);
  void FTN(c_esmci_attpacksetvalue)(ESMC_Base **base, char *name, ESMC_TypeKind *tk, int *count,
                                  void *value, char *convention, char *purpose, char *object, 
                                  int *rc, int nlen, int clen, int plen, int olen);
  void FTN(c_esmci_attributewritetab)(ESMC_Base **base, char *convention, char *purpose,
                                  char *object, char *targetobj, int *rc, int clen, int plen, 
                                  int olen, int tlen);
  void FTN(c_esmci_attributewritexml)(ESMC_Base **base, char *convention, char *purpose,
                                  char *object, char *targetobj, int *rc, 
                                  int clen, int plen, int olen, int tlen);
  void FTN(c_esmci_attributecopyall)(ESMC_Base **source, ESMC_Base **destination, int *rc);
  void FTN(c_esmci_attributedestroy)(ESMC_Base **base, char *name, int *rc, int nlen);
  void FTN(c_esmci_attributegetcount)(ESMC_Base **base, int *count, int *rc);
  void FTN(c_esmci_attributegetinfoname)(ESMC_Base **base, char *name, 
                                        ESMC_TypeKind *tk,
                                        int *count, int *rc, int nlen);
  void FTN(c_esmci_attributegetinfonum)(ESMC_Base **base, int *num, 
                                      char *name,
                                      ESMC_TypeKind *tk, int *count, 
                                      int *rc, int nlen);
  void FTN(c_esmci_attributegetchar)(ESMC_Base **base, char *name, char *value, 
                                    int *rc, int nlen, int vlen);
  void FTN(c_esmci_attributegetvalue)(ESMC_Base **base, char *name, 
                                    ESMC_TypeKind *tk,
                                    int *count, void *value, int *rc, int nlen);
  void FTN(c_esmci_attributeispresent)(ESMC_Base **base, char *name, ESMC_Logical *present, 
                                    int *rc, int nlen);
  void FTN(c_esmci_attributesetchar)(ESMC_Base **base, char *name, char *value, 
                                    int *rc, int nlen, int vlen);
  void FTN(c_esmci_attributesetcharlist)(ESMC_Base **base, char *name, ESMC_TypeKind *tk,
                                        int *count, char *valueList, int *lens, int *rc, int nlen);
  void FTN(c_esmci_attributesetvalue)(ESMC_Base **base, char *name, ESMC_TypeKind *tk,
                                    int *count, void *value, int *rc, int nlen);
  void FTN(c_esmci_attributesetlink)(ESMC_Base **source, ESMC_Base **destination, int *rc);
  void FTN(c_esmci_attributesetobjsintree)(ESMC_Base **base, char *object, char *name, 
                                          ESMC_TypeKind *tk, int *count, void *value, 
                                          int *rc, int olen, int nlen);
  }

// class utility functions, not methods, since they operate on
//  multiple objects
//int ESMCI_AttributeSetObjectList(ESMC_Base *anytypelist, ESMCI_Attribute *valuelist);
//int ESMCI_AttributeGetObjectList(ESMC_Base *anytypelist, ESMCI_Attribute *valuelist);

#endif  // ESMCI_ATTRIBUTE_H
