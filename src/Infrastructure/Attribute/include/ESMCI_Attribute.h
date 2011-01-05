// $Id: ESMCI_Attribute.h,v 1.41 2011/01/05 20:05:41 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
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
#include <algorithm>
#include <cmath>
#include <cstdio>
using namespace std;

#include "ESMCI_Util.h"

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  Attribute
//
// !DESCRIPTION:
// The code in this file implements the Attribute defined type
// and methods.
//
//-----------------------------------------------------------------------------
//
// !USES:

// Eventually move this to ESMCI_Util.h
enum ESMC_AttUpdateRm{ESMC_ATTUPDATERM_ATTRIBUTE=-42,
                      ESMC_ATTUPDATERM_ATTPACKATT,
                      ESMC_ATTUPDATERM_ATTPACK};

// !PUBLIC TYPES:
  class ESMC_Base;

namespace ESMCI {

  class Attribute;
  typedef Attribute* ESMCI_AttributePtr;
  class VM;
  class IO_XML;

class Attribute 
{
 private:
    string attrName; // inline to reduce memory thrashing
    ESMC_TypeKind tk;           // typekind indicator
    int items;                  // number of items (NOT byte count) for lists
    ESMC_Logical attrRoot;
  
    string attrConvention;             // Convention of Attpack
    string attrPurpose;                // Purpose of Attpack
    string attrObject;                 // Object of Attpack
    ESMC_Logical attrPack;             // an Attribute in an Attpack
    ESMC_Logical attrPackHead;         // the head of an Attpack
    ESMC_Logical attrNested;           // a nested Attpack

    ESMC_Logical linkChange;           // flag for link changes
    ESMC_Logical structChange;         // flag for structural changes
    ESMC_Logical valueChange;          // flag for value changes

    ESMC_Base *attrBase;        // pointer to a root attr's Base object
    Attribute *parent;          // pointer to the parent of this Attribute

    vector<Attribute*>  attrList;  // attributes - array of pointers
    vector<Attribute*>  packList;  // attributes - array of pointers
    vector<Attribute*>  linkList;  // attributes - array of pointers

    // Attribute values
    ESMC_I4               vi;       // integer, or
    vector<ESMC_I4>       vip;       // pointer to integer list, or
    ESMC_I8               vl;       // long, or
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
    Attribute(const Attribute&);

//-----------------------------------------------------------------------------
 public:
    // helper to set the Base address in attrBase
    void setBase(ESMC_Base *setBase){ attrBase = setBase; }

    // attpack methods
    int AttPackAddAttribute(const string &name, const string &convention, 
      const string &purpose, const string &object);
    int AttPackCreateCustom(const string &convention, 
      const string &purpose, const string &object);
    int AttPackCreateStandard(const string &convention, 
      const string &purpose, const string &object);
    Attribute *AttPackGet(const string &convention, 
      const string &purpose, const string &object,
      const string &attPackInstanceName) const;
    Attribute *AttPackGetAttribute(const string &name) const;
    int AttPackIsPresent(const string &name, const string &convention, 
      const string &purpose, const string &object,
      const string &attPackInstanceName, ESMC_Logical *present) const;
    int AttPackNest(const string &convention, const string &purpose,
      const string &object, 
      const string &nestConvention, const string &nestPurpose);
    int AttPackNest(const string &convention, const string &purpose,
      const string &object, 
      int nestCount, const vector<string> &nestConvention,
                     const vector<string> &nestPurpose);
    int AttPackRemove(const string &convention, 
      const string &purpose, const string &object, 
      const string &attPackInstanceName);
    int AttPackRemoveAttribute(const string &name, const string &convention, 
      const string &purpose, const string &object, 
      const string &attPackInstanceName);
    int AttPackSet(const string &name, const ESMC_TypeKind &tk, 
      int count, void *value, const string &convention, 
      const string &purpose, const string &object,
      const string &attPackInstanceName);
    int AttPackSet(Attribute *attr);
    
    // copy and swap an attribute hierarchy
    int  AttributeCopy(const Attribute &source);
    int  AttributeCopyHybrid(const Attribute &source);
//    int  AttributeCopyReference(const Attribute &source);
//    int  AttributeCopyReferenceTree(const Attribute &source);
    int  AttributeCopyValue(const Attribute &source);
//    int  AttributeCopyValueTree(const Attribute &source);
    int  AttributeMove(Attribute *source);
    
    // count the number of objects in an attribute hierarchy
    int AttributeCountTree(const string &convention, const string &purpose,  
      const string &object, int &objcount, int &numattrs) const;
    int AttributeCountTreeAttpack(int &objcount, int& numattrs) const;
    int AttributeCountTreeLens(const string &convention, const string &purpose,  
      const string &object, int *attrLens, vector<string> &attrNames) const;
    int AttributeCountTreeLensAttpack(int &index, int *attrLens, 
      vector<string> &attrNames) const;
    
    // attribute methods - get
    int AttributeGet(const string &name, ESMC_I4 *value) const;
    int AttributeGet(const string &name, int *count, vector<ESMC_I4> *value) const;
    int AttributeGet(const string &name, ESMC_I8 *value) const;
    int AttributeGet(const string &name, int *count, vector<ESMC_I8> *value) const;
    int AttributeGet(const string &name, ESMC_R4 *value) const;
    int AttributeGet(const string &name, int *count, vector<ESMC_R4> *value) const;
    int AttributeGet(const string &name, ESMC_R8 *value) const;
    int AttributeGet(const string &name, int *count, vector<ESMC_R8> *value) const;
    int AttributeGet(const string &name, ESMC_Logical *value) const;
    int AttributeGet(const string &name, int *count, vector<ESMC_Logical> *value) const;
    int AttributeGet(const string &name, string *value) const;
    int AttributeGet(const string &name, vector<string> *value) const;

    // getting either by name or number directly return attribute ptr
    Attribute *AttributeGet(const string &name) const; 
    Attribute *AttributeGet(int num) const;

    // get attribute info
    int AttributeGet(const string &name, int *lens, int count) const;
    int AttributeGet(const string &name, ESMC_TypeKind *tk, int *itemCount) const;
    int AttributeGet(int num, string *name, ESMC_TypeKind *tk, int *itemCount) const;
    int AttributeGetCount(void) const;
    int AttributeGetCountPack(void) const;
    int AttributeGetCountLink(void) const;
    int AttributeGetCountTotal(void) const;
    int AttributeGetItemCount(const string &name) const;

    // get attribute parent
    Attribute *AttributeGetParent(void) const; 

    // query for existence of an attribute
    int AttributeIsPresent(const string &name, ESMC_Logical *present) const;

    // query for whether an attribute has been set
    bool AttributeIsSet(const string &name) const;

    // link/unlink an attribute hierarchy
    int AttributeLink(Attribute *destination, ESMC_Logical *linkChangeIn);
    int AttributeLinkRemove(Attribute *destination, ESMC_Logical *linkChangeIn);

    // destroy an attribute or attpack
    int AttributeRemove(const string &name);
    
    // setting when you have an attribute already assembled
    int AttributeSet(Attribute *attr);

    // attribute methods - set
    int AttributeSet(const string &name, ESMC_I4 value);
    int AttributeSet(const string &name, int count, vector<ESMC_I4> *value);
    int AttributeSet(const string &name, ESMC_I8 value);
    int AttributeSet(const string &name, int count, vector<ESMC_I8> *value);
    int AttributeSet(const string &name, ESMC_R4 value);
    int AttributeSet(const string &name, int count, vector<ESMC_R4> *value);
    int AttributeSet(const string &name, ESMC_R8 value);
    int AttributeSet(const string &name, int count, vector<ESMC_R8> *value);
    int AttributeSet(const string &name, ESMC_Logical value);
    int AttributeSet(const string &name, int count, vector<ESMC_Logical> *value);
    int AttributeSet(const string &name, string *value);
    int AttributeSet(const string &name, int count, vector<string> *value);
//    int AttributeSet(const string &name, const ESMC_TypeKind &tk, 
//      int count, void *value);
    
    // recursive call to set all attributes with attrObject = object
    int AttributeSetObjsInTree(const string &name, const string &object, 
      const ESMC_TypeKind &tk, const int &count, void *value);

    // attribute update
    int AttributeUpdate(VM *vm, const vector<ESMC_I4> &rootList);
    int AttributeUpdateBufRecv(char *recvBuf, int localPet, int *offset,
      const int &length);
    int AttributeUpdateBufSend(char *sendBuf, int localPet, int *offset,
      int *length) const;
    int AttributeUpdateComm(VM *vm, int sendBufSize, int *recvBufSize, 
      char *sendBuf, char *recvBuf, const vector<ESMC_I4> &roots, 
      const vector<ESMC_I4> &nonroots) const;
    int AttributeUpdateTreeChanges(int *linkChanges,
      int *structChanges, int *valueChanges, int *numKeys) const;
    bool AttributeUpdateKeyCompare(char *key1, char *key2) const;
    int AttributeUpdateKeyCreate(char *key) const;
    int AttributeUpdateNeeded(VM *vm, int &bufSize,
      const vector<ESMC_I4> &roots, const vector<ESMC_I4> &nonroots) const;
    int AttributeUpdateRemove(int attrNum);
    int AttributeUpdateReset();

    // attribute read methods
    int AttributeRead(int fileNameLen, const char* fileName,
                      int schemaFileNameLen, const char* schemaFileName);

    // attribute write methods
    int AttributeWriteTab(const string &convention, const string &purpose, 
      const string &object, const string &varobj, const string &basename) const;
    int AttributeWriteTabTraverse(FILE *tab, const string &convention,
      const string &purpose, int &index, const int &columns, int *attrLens,
      const vector<string> &attrNames) const;
    int AttributeWriteTabBuffer(FILE *tab, int &index, const int &columns, 
      int *attrLens, const vector<string> &attrNames) const;

    int AttributeWriteXML(const string &convention, const string &purpose, 
      const string &object, const string &varobj, const string &basename) const;

    int AttributeWriteXMLtraverse(IO_XML *io_xml, const string &convention,
      const string &purpose,
      const int &columns,bool &fielddone,bool &griddone,bool &compdone) const;
    int AttributeWriteXMLbuffer(IO_XML *io_xml) const;
    int AttributeWriteXMLbuffergrid(IO_XML *io_xml) const;
    int AttributeWriteXMLbufferfield(IO_XML *io_xml, const string &convention,
      const string &purpose, int &index, const int &columns) const;
    int AttributeWriteXMLbufferfieldT(IO_XML *io_xml, int &index,
       const int &columns) const;

    int AttributeWriteWaterMLbuffieldT(IO_XML *io_xml, int &index,
       const int &columns) const;

    int AttributeWriteCIM(IO_XML *io_xml) const;
    int AttributeWriteCIMmodelComp(IO_XML *io_xml, int indent) const;
    int AttributeWriteCIMsimRun(IO_XML *io_xml) const;
    int AttributeWriteCIMplatform(IO_XML *io_xml) const;
    int AttributeWriteCIMRP(IO_XML *io_xml, int indent) const;
    int AttributeWriteCIMcitation(IO_XML *io_xml, int indent) const;
    int AttributeWriteCIMcomposition(IO_XML *io_xml) const;
    int AttributeWriteCIMbuffer(IO_XML *io_xml) const; 

    // Print
    int ESMC_Print(void) const;

    // Modifiers, Constructors, Destructors, Serializers, Operators
    Attribute(const string &conv, const string &purp, const string &obj);
    Attribute(const string &name, const string &conv, const string &purp, 
      const string &obj);
    Attribute(void);
    Attribute(const ESMC_Logical &attributeRoot);
    Attribute(const string &name, const ESMC_TypeKind &typekind, 
      int numitems, void *datap);
    int AttrModifyValue(const ESMC_TypeKind &typekind, int numitems, 
      void *datap);
    Attribute& operator=(const Attribute& source);
    ~Attribute(void);
    int ESMC_Deserialize(char *buffer, int *offset);
    int ESMC_Serialize(char *buffer, int *length, int *offset,
      ESMC_InquireFlag inquireflag) const;
    int ESMC_SerializeCC(char *buffer, int *length, int &offset,
      bool cc, ESMC_InquireFlag inquireflag) const;

};
} // namespace

// fortran interface functions to attribute objects
extern "C" {
  void FTN(c_esmc_attpackaddattribute)(ESMC_Base **base, char *name,
                                  char *convention, char *purpose, 
                                  char *object, int *rc,
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg clen,
                                  ESMCI_FortranStrLenArg plen,
                                  ESMCI_FortranStrLenArg olen);
  void FTN(c_esmc_attpackcreatecustom)(ESMC_Base **base,
                                  char *convention, char *purpose, 
                                  char *object, int *rc, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen,
                                  ESMCI_FortranStrLenArg olen);
  void FTN(c_esmc_attpackcreatestandard)(ESMC_Base **base,
                                  char *convention, char *purpose, 
                                  char *object, int *rc, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen);
  void FTN(c_esmc_attpacknest)(ESMC_Base **base,
                                  char *convention, char *purpose, 
                                  char *object, int *nestCount,
                                  char *nestConvention, char *nestPurpose, 
                                  int *nestConvLens, int *nestPurpLens,
                                  int *rc, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen, 
                                  ESMCI_FortranStrLenArg nclen, 
                                  ESMCI_FortranStrLenArg nplen);
  void FTN(c_esmc_attpackdestroy)(ESMC_Base **base, char *name,
                                  char *convention, char *purpose,
                                  char *object, int *rc, 
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen);
  void FTN(c_esmc_attpackgetchar)(ESMC_Base **base, char *name, char *value, 
                                  char *convention, char *purpose,
                                  char *object,
                                  char *attPackInstanceName, int *rc, 
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg vlen, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg alen);
  void FTN(c_esmc_attpackgetcharlist)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind *tk, int *count,
                                  int *lens, char *valueList,
                                  char *convention, char *purpose, 
                                  char *object, char *attPackInstanceName,
                                  int *rc, 
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg vlen, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg alen);
  void FTN(c_esmc_attpackgetvalue)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind *tk, int *count,
                                  void *value, char *convention, char *purpose,
                                  char *object, char *attPackInstanceName,
                                  int *rc, 
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg alen);
  void FTN(c_esmc_attpackispresent)(ESMC_Base **base, char *name,
                                  char *convention, char *purpose, 
                                  char *object, char *attPackInstanceName,
                                  ESMC_Logical *present, int *rc, 
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg alen);
// TODO:  intel 11.0.083 compiler on Columbia errors out on ESMCI_Attribute_F.C
//        with the following prototype:
// error: more than one instance of overloaded function "c_esmc_attpackremove_" has "C" linkage
//  void FTN(c_esmc_attpackremove)(ESMC_Base **base,
//                                  char *convention, char *purpose,
//                                  char *object, char *attPackInstanceName,
//                                  int *rc,
//                                  ESMCI_FortranStrLenArg clen,
//                                  ESMCI_FortranStrLenArg plen,
//                                  ESMCI_FortranStrLenArg olen,
//                                  ESMCI_FortranStrLenArg olen,
//                                  ESMCI_FortranStrLenArg alen);
  void FTN(c_esmc_attpackremoveattribute)(ESMC_Base **base, char *name,
                                  char *convention, char *purpose,
                                  char *object, char *attPackInstanceName,
                                  int *rc,
                                  ESMCI_FortranStrLenArg nlen,
                                  ESMCI_FortranStrLenArg clen,
                                  ESMCI_FortranStrLenArg plen,
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg alen);
  void FTN(c_esmc_attpacksetchar)(ESMC_Base **base, char *name, char *value,
                                  ESMC_TypeKind *tk, 
                                  char *convention, char *purpose,
                                  char *object, char *attPackInstanceName,
                                  int *rc,
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg vlen, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg alen);
  void FTN(c_esmc_attpacksetcharlist)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind *tk, int *count,
                                  char *valueList, int *lens,
                                  char *convention, char *purpose, 
                                  char *object, char *attPackInstanceName,
                                  int *rc,
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg vlen, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg alen);
  void FTN(c_esmc_attpacksetvalue)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind *tk, int *count,
                                  void *value, char *convention, char *purpose,
                                  char *object, char *attPackInstanceName,
                                  int *rc,
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg alen);
  void FTN(c_esmc_attributewritetab)(ESMC_Base **base,
                                  char *convention, char *purpose,
                                  char *object, char *targetobj, int *rc, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen, 
                                  ESMCI_FortranStrLenArg tlen);
  void FTN(c_esmc_attributewritexml)(ESMC_Base **base,
                                  char *convention, char *purpose,
                                  char *object, char *targetobj, int *rc, 
                                  ESMCI_FortranStrLenArg clen, 
                                  ESMCI_FortranStrLenArg plen, 
                                  ESMCI_FortranStrLenArg olen, 
                                  ESMCI_FortranStrLenArg tlen);
  void FTN(c_esmc_attributecopy)(ESMC_Base **source, ESMC_Base **destination, 
                                  ESMC_AttCopyFlag *attcopyflag,
                                  ESMC_AttTreeFlag *atttreeflag, int *rc);
  void FTN(c_esmc_attributedestroy)(ESMC_Base **base, char *name, int *rc, 
                                  ESMCI_FortranStrLenArg nlen);
  void FTN(c_esmc_attributegetcount)(ESMC_Base **base, int *count,
                                  ESMC_AttGetCountFlag *flag, int *rc);
  void FTN(c_esmc_attributegetinfoname)(ESMC_Base **base, char *name, 
                                  ESMC_TypeKind *tk,
                                  int *count, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN(c_esmc_attributegetinfonum)(ESMC_Base **base, int *num, 
                                  char *name,
                                  ESMC_TypeKind *tk, int *count, int *rc, 
                                  ESMCI_FortranStrLenArg nlen);
  void FTN(c_esmc_attributegetchar)(ESMC_Base **base, char *name, char *value, 
                                  int *rc, 
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg vlen);
  void FTN(c_esmc_attributegetvalue)(ESMC_Base **base, char *name, 
                                  ESMC_TypeKind *tk,
                                  int *count, void *value, int *rc, 
                                  ESMCI_FortranStrLenArg nlen);
  void FTN(c_esmc_attributeispresent)(ESMC_Base **base, char *name,
                                  ESMC_Logical *present, 
                                  int *rc, 
                                  ESMCI_FortranStrLenArg nlen);
  void FTN(c_esmc_attributelink)(ESMC_Base **source, ESMC_Base **destination,
                                  ESMC_Logical *linkChanges, int *rc);
  void FTN(c_esmc_attributelinkremove)(ESMC_Base **source,
                                  ESMC_Base **destination, ESMC_Logical *linkChange,
                                  int *rc);
  void FTN(c_esmc_attributemove)(ESMC_Base **source,
                                  ESMC_Base **destination, int *rc);
  void FTN(c_esmc_attributesetchar)(ESMC_Base **base, char *name, char *value, 
                                  int *rc, 
                                  ESMCI_FortranStrLenArg nlen, 
                                  ESMCI_FortranStrLenArg vlen);
  void FTN(c_esmc_attributesetcharlist)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind *tk,
                                  int *count, char *valueList, int *lens,
                                  int *rc, 
                                  ESMCI_FortranStrLenArg nlen,
                                  ESMCI_FortranStrLenArg vllen);
  void FTN(c_esmc_attributesetvalue)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind *tk,
                                  int *count, void *value, int *rc, 
                                  ESMCI_FortranStrLenArg nlen);
  void FTN(c_esmc_attributesetobjsintree)(ESMC_Base **base, char *object,
                                  char *name, 
                                  ESMC_TypeKind *tk, int *count, void *value, 
                                  int *rc, 
                                  ESMCI_FortranStrLenArg olen, 
                                  ESMCI_FortranStrLenArg nlen);
  void FTN(c_esmc_attributeupdate)(ESMC_Base **base, ESMCI::VM **vm,
                                  int *rootList, int *count, int *rc);
  void FTN(c_esmc_attributeupdatereset)(ESMC_Base **base, int *rc);
}

// class utility functions, not methods, since they operate on
//  multiple objects
//int AttributeSetObjectList(ESMC_Base *anytypelist, Attribute *valuelist);
//int AttributeGetObjectList(ESMC_Base *anytypelist, Attribute *valuelist);

#endif  // ESMCI_ATTRIBUTE_H
