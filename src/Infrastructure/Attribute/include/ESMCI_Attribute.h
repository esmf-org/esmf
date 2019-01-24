// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
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
#include <fstream>

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
                      ESMC_ATTUPDATERM_ATTPACK,
                      ESMC_ATTUPDATERM_HOOKANDCONTINUE}; // hack for nonordered containers

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
    std::string attrName; // inline to reduce memory thrashing
    ESMC_TypeKind_Flag tk;       // typekind indicator
    unsigned int items;          // number of items (NOT byte count) for lists

    ESMC_Logical attrRoot;
    ESMC_Logical attrUpdateDone; // hack for non-ordered containers

    std::string attrConvention;             // Convention of Attpack
    std::string attrPurpose;                // Purpose of Attpack
    std::string attrObject;                 // Object of Attpack

    ESMC_Logical attrPack;             // an Attribute in an Attpack
      // - only to be set on attPack attributes
    ESMC_Logical attrPackHead;         // the head of an Attpack
    ESMC_Logical attrNested;           // a nested Attpack

    ESMC_Logical deleteChange;         // flag for deletions
    ESMC_Logical linkChange;           // flag for link changes
    ESMC_Logical structChange;         // flag for structural changes
      // - do NOT set on attPack head
    ESMC_Logical valueChange;          // flag for value changes

    ESMC_Base *attrBase;        // pointer to a root attr's Base object
    Attribute *parent;          // pointer to the parent of this Attribute

    std::vector<Attribute*>  attrList;  // attributes - array of pointers
    std::vector<Attribute*>  packList;  // attributes - array of pointers
    std::vector<Attribute*>  linkList;  // attributes - array of pointers

    // Attribute values
    std::vector<ESMC_I4>       vip;       // vector of integers
    std::vector<ESMC_I8>       vlp;       // vector of longs
    std::vector<ESMC_R4>       vfp;       // vector of floats (real*4)
    std::vector<ESMC_R8>       vdp;       // vector of doubles (real*8)
    std::vector<ESMC_Logical>  vbp;       // vector of booleans (logical)
    std::vector<std::string>   vcpp;      // vector of strings

    std::string attrGUID;                 // Globally Unique Identifier for
                                          //  an Attribute or Attpack
                                          //  (used in CIM XML output)

    static Attribute *writeRoot;  // attribute on which AttributeWrite*()
                                  // method was called; used for multiple
                                  // nested recursions of the tree
                                  // (e.g. for CIM output)

    int        id;         // unique identifier. used to generate unique
                                  // default names.
                                  //    TODO: inherit from ESMC_Base class?
                                  //      -- but circular dependency exists
                                  //         with 'root' in ESMC_Base
    static int count;      // number of attributes created in this address
                           //   space. Thread-safe because int is atomic.
                                  //    TODO: inherit from ESMC_Base class?
    // prevent accidental copying
    Attribute(const Attribute&);

//-----------------------------------------------------------------------------
 public:
    // constant strings for conventions and purposes
    static const char CF_CONV[];
    static const char ESG_CONV[];
    static const char ESMF_CONV[];
    static const char CIM_1_5_CONV[];
    static const char CIM_1_5_1_CONV[];
    static const char CIM_1_7_1_CONV[];


    static const char GENERAL_PURP[];
    static const char EXTENDED_PURP[];
    static const char INPUTS_PURP[];
    static const char MODEL_COMP_PURP[];
    static const char PLATFORM_PURP[];
    static const char RESP_PARTY_PURP[];
    static const char CITATION_PURP[];
    static const char SCI_PROP_PURP[];
    static const char COMP_PROP_PURP[];
    static const char GRIDS_PURP[];

    // Print
    void attprint(char *msgbuf, bool tofile, std::ofstream &fp) const \
      {if (tofile) { std::string str(msgbuf); fp << str;} else printf("%s", msgbuf);}
    int ESMC_Print(bool tofile, const char *filename, bool append) const;
    int print_to_file(bool tofile, std::ofstream &fp, unsigned int level) const;
    int ESMC_Print(void) const;

    // Modifiers, Constructors, Destructors, Serializers, Operators
    Attribute(const std::string &conv, const std::string &purp, const std::string &obj);
    Attribute(const std::string &name, const std::string &conv, const std::string &purp,
      const std::string &obj);
    Attribute(void);
    Attribute(const ESMC_Logical &attributeRoot);
    Attribute(const std::string &name, const ESMC_TypeKind_Flag &typekind,
      int numitems, void *datap);
    int AttrModifyValue(const ESMC_TypeKind_Flag &typekind, int numitems,
      void *datap);
    Attribute& operator=(const Attribute& source);
    ~Attribute(void);
    int ESMC_Deserialize(char *buffer, int *offset);
    int ESMC_Serialize(char *buffer, int *length, int *offset,
      ESMC_InquireFlag inquireflag) const;
    int ESMC_SerializeCC(char *buffer, int *length, int &offset,
      bool cc, ESMC_InquireFlag inquireflag) const;
    void clean();

    // accessors for private member variables
    inline const std::string getName() const {return this->attrName;}
    inline       ESMC_TypeKind_Flag getTypeKind() const {return this->tk;}
    inline       int getItemCount() const {return this->items;}
    inline const std::string getConvention() const {return this->attrConvention;}
    inline const std::string getPurpose() const {return this->attrPurpose;}
    inline const std::string getObject() const {return this->attrObject;}
    inline const Attribute *getParent() const {return this->parent;}
    inline const ESMC_Base *getBase() const {return this->attrBase;}

    // accessors for counts
    inline int getCountAttr() const {return attrList.size();};
    inline int getCountPack() const {return packList.size();};
    inline int getCountLink() const {return linkList.size();};
    inline int getCountTotal() const {
      return attrList.size()+packList.size()+linkList.size();};

    // helper to set the Base address in attrBase
    void setBase(ESMC_Base *setBase) {attrBase = setBase;}

    // get the value of an Attribute
    int get(int *count, std::vector<ESMC_I4> *value) const;
    int get(int *count, std::vector<ESMC_I8> *value) const;
    int get(int *count, std::vector<ESMC_R4> *value) const;
    int get(int *count, std::vector<ESMC_R8> *value) const;
    int get(int *count, std::vector<ESMC_Logical> *value) const;
    int get(std::vector<std::string> *value) const;

    // get info from an Attribute
    int get(int *lens, int count) const;
    int getCount(ESMC_AttGetCountFlag gcflag, int *count) const;
    int getCount(ESMC_AttGetCountFlag gcflag,
             ESMC_AttNest_Flag anflag, int *count) const;

    // query whether an Attribute is "present" or "set"
    bool isSet() const;

    int streamJSON(ESMC_Logical flattenPackList, ESMC_Logical includeUnset, ESMC_Logical includeLinks, std::string &output) const;
    int streamAttributeToJSON(ESMC_Logical flattenPackList, ESMC_Logical includeUnset, ESMC_Logical includeLinks, std::string &output,  int *totalStreamed) const;
    int streamAttributeRootToJSON(ESMC_Logical flattenPackList, ESMC_Logical includeUnset, ESMC_Logical includeLinks, std::string &output, int *totalStreamed) const;
    int streamAttributeListToJSON(std::vector<Attribute *> attrVector, ESMC_Logical flattenPackList, ESMC_Logical includeUnset, std::string &output, int *totalStreamed) const;
        int streamAttributePackToJSON(std::vector<Attribute *> attrVector, ESMC_Logical flattenPackList, ESMC_Logical includeUnset, ESMC_Logical includeLinks, std::string &output, int *totalStreamed) const;
        int streamAttributeLinksToJSON(std::vector<Attribute *> attrVector, ESMC_Logical flattenPackList, ESMC_Logical includeUnset, std::string &output, int *totalStreamed) const;

        template<typename T>
    std::string attrValuesToString(const std::vector<T> *vec) const;

    // return an Attribute by name or number
    Attribute *AttributeGet(const std::string &name) const;
    Attribute *AttributeGet(int num) const;

    // query whether an Attribute is "present"
    int AttributeIsPresent(const std::string &name, ESMC_Logical *present) const;
    int AttributeIsPresent(const int &num, ESMC_Logical *present) const;

    // attribute methods - set
    int AttributeSet(const std::string &name, int count, std::vector<ESMC_I4> *value);
    int AttributeSet(const std::string &name, int count, std::vector<ESMC_I8> *value);
    int AttributeSet(const std::string &name, int count, std::vector<ESMC_R4> *value);
    int AttributeSet(const std::string &name, int count, std::vector<ESMC_R8> *value);
    int AttributeSet(const std::string &name, int count, std::vector<ESMC_Logical> *value);
    int AttributeSet(const std::string &name, int count, std::vector<std::string> *value);

     // copy and swap an attribute hierarchy
    int AttributeCopy(const Attribute &source);
    int AttributeCopyHybrid(const Attribute &source);
    int AttributeCopyIgnore(const Attribute &source);
    int AttributeCopyReplace(const Attribute &source);
    int AttributeMove(Attribute *source);

    // count the number of objects in an attribute hierarchy
    int AttributeCountTree(const std::string &convention, const std::string &purpose,
      const std::string &object, int &objcount, int &numattrs) const;
    int AttributeCountTreeAttpack(int &objcount, int& numattrs) const;
    int AttributeCountTreeLens(const std::string &convention, const std::string &purpose,
      const std::string &object, int *attrLens, std::vector<std::string> &attrNames) const;
    int AttributeCountTreeLensAttpack(int &index, int *attrLens,
      std::vector<std::string> &attrNames) const;

    // link/unlink an attribute hierarchy
    int AttributeLink(Attribute *destination, ESMC_Logical *linkChangeIn);
    int AttributeLinkRemove(Attribute *destination, ESMC_Logical *linkChangeIn);

    // destroy an attribute or attpack
    int AttributeRemove(const std::string &name);

    // setting when you have an attribute already assembled
    int AttributeSet(Attribute *attr);

   // recursive call to set all attributes with attrObject = object
    int AttributeSetObjsInTree(const std::string &name, const std::string &object,
      const ESMC_TypeKind_Flag &tk, const int &count, void *value);

    // attpack methods
    int AttPackAddAttribute(const std::string &name, const std::string &convention,
      const std::string &purpose, const std::string &object);
    int AttPackAddAttribute(const std::string &name);

    Attribute *AttPackCreateCustom(const std::string &convention,
      const std::string &purpose, const std::string &object);
    int AttPackCreateStandard(const std::string &convention,
      const std::string &purpose, const std::string &object);
    int AttPackCreateStandard(const std::string &convention, const std::string &purpose,
      const std::string &object,
      const std::vector<std::string> &nestConvention, const std::vector<std::string> &nestPurpose,
      const std::vector<int> &nestAttPackInstanceCountList, int nestCount,
      std::vector<std::string> &nestAttPackInstanceNameList,
      int &nestAttPackInstanceNameCount);

    Attribute *AttPackGet(const std::string &convention,
      const std::string &purpose, const std::string &object,
      const std::string &attPackInstanceName, ESMC_AttNest_Flag anflag) const;
    Attribute *AttPackGet(const std::string &convention,
      const std::string &purpose, const std::string &object,
      const std::string &name, const std::string &value) const;
    int AttPackGet(const std::string &convention,
      const std::string &purpose, const std::string &object,
      std::vector<std::string> &attPackInstanceNameList,
      int &attPackInstanceNameCount, ESMC_AttNest_Flag anflag) const;

    Attribute *AttPackGetAttribute(const std::string &name) const;
    Attribute *AttPackGetAttribute(const int &num) const;
    Attribute *AttPackGetAttribute(const std::string &name,
                                   ESMC_AttNest_Flag anflag) const;
    Attribute *AttPackGetAttribute(const int &num,
                                   ESMC_AttNest_Flag anflag) const;

    // for an Attpack given a pointer
    int AttPackIsPresent(const ESMCI::Attribute *attpack,
                         ESMC_Logical *present);

    // for Attribute in an Attpack
    int AttPackIsPresent(const std::string &name,
                         const ESMCI::Attribute *attpack,
                         ESMC_AttNest_Flag anflag, ESMC_Logical *present) const;
    int AttPackIsPresent(const int &num,
                         const ESMCI::Attribute *attpack,
                         ESMC_AttNest_Flag anflag, ESMC_Logical *present) const;
    bool AttPackIsSet(const std::string &convention, const std::string &purpose,
                      const std::string &object,
                      const bool &inObjectTree,
                      const bool &inThisCompTreeOnly,
                      const bool &inNestedAttPacks) const;
    bool AttPackIsSet(const bool &inNestedAttPacks) const;

    int AttPackNest(const std::string &convention, const std::string &purpose,
                    const std::string &object,
                    const std::string &nestConvention, const std::string &nestPurpose);
    int AttPackNest(const std::string &convention, const std::string &purpose,
                    const std::string &object,
                    int nestCount, const std::vector<std::string> &nestConvention,
                    const std::vector<std::string> &nestPurpose);

    int AttPackRemove(ESMCI::Attribute *attpack);
    int AttPackRemoveAttribute(const std::string &name,
                               ESMCI::Attribute *attpack,
                               ESMC_AttNest_Flag anflag);

    int AttPackSet(Attribute *attr);


    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
    //  AttributeUpdate
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //

    // attribute update
    int AttributeUpdate(VM *vm, const std::vector<ESMC_I4> &rootList,
                        bool reconcile);
    int AttributeUpdateBufRecv(char *recvBuf, int localPet, int *offset,
      const int &length);
    int AttributeUpdateBufSend(char *sendBuf, int localPet, int *offset,
      int *length) const;
    int AttributeUpdateComm(VM *vm, int sendBufSize, int *recvBufSize,
      char *sendBuf, char *recvBuf, const std::vector<ESMC_I4> &roots,
      const std::vector<ESMC_I4> &nonroots) const;
    int AttributeUpdateTreeChanges(int *linkChanges,
      int *structChanges, int *valueChanges, int *deletChange, int *numKeys) const;
    int AttributeUpdateCountPackage(int *packAttCount) const;
    bool AttributeUpdateKeyCompare(char *key1, char *key2) const;
    int AttributeUpdateKeyCreate(char *key) const;
    int AttributeUpdateNeeded(VM *vm, int &bufSize,
      const std::vector<ESMC_I4> &roots, const std::vector<ESMC_I4> &nonroots,
      bool reconcile) const;
    int AttributeUpdateRemove(int attrNum);
    int AttributeUpdateReset();

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
    //  AttributeWrite
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //

    // attribute read/write methods
    int AttributeRead(int fileNameLen, const char* fileName,
                      int schemaFileNameLen, const char* schemaFileName);
    int AttributeWrite(const std::string &convention, const std::string &purpose,
                       const std::string &object, const std::string &varobj,
                       const std::string &basename,
                       const ESMC_AttWriteFlag &attwriteflag) const;

    // attribute write methods
    int AttributeWriteTab(const std::string &convention, const std::string &purpose,
      const std::string &object, const std::string &varobj, const std::string &basename) const;
    int AttributeWriteTabTraverse(FILE *tab, const std::string &convention,
      const std::string &purpose, int &index, const int &columns, int *attrLens,
      const std::vector<std::string> &attrNames) const;
    int AttributeWriteTabBuffer(FILE *tab, int &index, const int &columns,
      int *attrLens, const std::vector<std::string> &attrNames) const;

    int AttributeWriteXML(const std::string &convention, const std::string &purpose,
      const std::string &object, const std::string &varobj, const std::string &basename) const;
    int AttributeWriteXMLheader(IO_XML *io_xml, const std::string &convention,
      const std::string &purpose, const std::string &object,
      const std::string &varobj) const;
    int AttributeWriteXMLtraverse(IO_XML *io_xml, const std::string &convention,
      const std::string &purpose,
      const int &columns,bool &fielddone,bool &griddone,bool &compdone) const;
    int AttributeWriteXMLbuffer(IO_XML *io_xml) const;
    int AttributeWriteXMLbuffergrid(IO_XML *io_xml) const;
    int AttributeWriteXMLbufferfield(IO_XML *io_xml, const std::string &convention,
      const std::string &purpose, int &index, const int &columns) const;
    int AttributeWriteXMLbufferfieldT(IO_XML *io_xml, int &index,
       const int &columns) const;

    int AttributeWriteWaterMLbuffieldT(IO_XML *io_xml, int &index,
       const int &columns) const;

    int AttributeWriteCIM(IO_XML *io_xml, const std::string &convention) const;
    int AttributeWriteCIMmodelComp(IO_XML *io_xml, const std::string &convention, int indent,
                                   const bool gridPresent, const std::string gridGUID) const;
    int AttributeWriteCIMsimRun(IO_XML *io_xml, const std::string &convention) const;
    int AttributeWriteCIMplatform(IO_XML *io_xml, const std::string &convention) const;
    int AttributeWriteCIMCP(IO_XML *io_xml, const std::string &convention,
                            const std::string &purpose, int indent) const;
    int AttributeWriteCIMCPfield(IO_XML *io_xml, const std::string &convention, int indent) const;
    int AttributeWriteCIMRP(IO_XML *io_xml, int indent) const;
    int AttributeWriteCIMcitation(IO_XML *io_xml, int indent) const;
    //int AttributeWriteCIMcomposition(IO_XML *io_xml) const;
    int AttributeWriteCIMinput(IO_XML *io_xml, const std::string &convention) const;
    int AttributeWriteCIMbuffer(IO_XML *io_xml) const;

    int AttributeWriteCIMgrids(IO_XML *io_xml, const std::string convention, const std::string gridGUID, const int indent, const bool gridSolo) const;
    int AttributeWriteCIMgridref(IO_XML *io_xml, int indent, const std::string gridGUID) const;
    bool AttributeWriteCIMgridPresent(const std::string convention) const;
    int AttributeWriteInternalInfoGrid(IO_XML *io_xml, int nest_level, Attribute *attr) const;
    std::string AttributeGetInternalGridInt(std::string inputString) const;
    std::string AttributeGetInternalGridString(std::string inputString) const;
    const std::string getTime() const;
    std::string month2Num(std::string month) const;
};
} // namespace

// fortran interface functions to attribute objects
extern "C" {
  void FTN_X(c_esmc_attpackaddatt)(char *name,
                                  ESMCI::Attribute **attribute, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attpackaddattribute)(ESMC_Base **base, char *name,
                                  int *count, char *specList,
                                  int *lens, int *rc,
                                  ESMCI_FortranStrLenArg nlen,
                                  ESMCI_FortranStrLenArg slen);
  void FTN_X(c_esmc_attpackcreatecustom)(ESMC_Base **base,
                                  int *count, char *specList, int *lens,
                                  ESMCI::Attribute **attpack,
                                  int *rc, ESMCI_FortranStrLenArg slen);
  void FTN_X(c_esmc_attpackcreatestandard)(ESMC_Base **base,
                                  int *count, char *specList,
                                  int *lens, int *rc,
                                  ESMCI_FortranStrLenArg slen);
  void FTN_X(c_esmc_attpacknest)(ESMC_Base **base,
                                  int *count, char *specList,
                                  int *lens,
                                  int *nestCount,
                                  char *nestConvention, char *nestPurpose,
                                  int *nestConvLens, int *nestPurpLens,
                                  int *rc,
                                  ESMCI_FortranStrLenArg slen,
                                  ESMCI_FortranStrLenArg nclen,
                                  ESMCI_FortranStrLenArg nplen);
  void FTN_X(c_esmc_attpackcreatestdnest)(ESMC_Base **base,
                                  char *convention, char *purpose,
                                  char *object,
                                  char *nestConvention, char *nestPurpose,
                                  int *nestConvLens, int *nestPurpLens,
                                  int *nestAttPackInstanceCountList,
                                  int *nestCount,
                                  char *nestAttPackInstanceNameList,
                                  int *nestAttPackInstanceNameLens,
                                  int *nestattPackInstanceNameSize,
                                  int *nestAttPackInstanceNameCount,
                                  int *rc,
                                  ESMCI_FortranStrLenArg clen,
                                  ESMCI_FortranStrLenArg plen,
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg nclen,
                                  ESMCI_FortranStrLenArg nplen,
                                  ESMCI_FortranStrLenArg napinlen);
// TODO:  intel 11.0.083 compiler on Columbia errors out on ESMCI_Attribute_F.C
//        with the following prototype:
// error: more than one instance of overloaded function "c_esmc_attpackremove_" has "C" linkage
  void FTN_X(c_esmc_attpackstreamjson)(ESMCI::Attribute **attpack,
                                  int *flattenPackList,
                                                                  int *includeUnset,
                                                                  int *includeLinks,
                                                                  char *output, int *rc,
                                  ESMCI_FortranStrLenArg olen);
  void FTN_X(c_esmc_attpackstreamjsonstrlen)(ESMCI::Attribute **attpack,
                                                                  int *flattenPackList,
                                                                  int *includeUnset,
                                                                  int *includeLinks,
                                  int *jsonstrlen, int *rc);
  void FTN_X(c_esmc_attpackget)(ESMC_Base **base, ESMCI::Attribute **attpack,
                                  int *count, char *specList, int *lens,
                                  ESMC_AttNest_Flag *anflag,
                                  ESMC_Logical *present, int *rc,
                                  ESMCI_FortranStrLenArg slen);
  void FTN_X(c_esmc_attpackremove)(ESMC_Base **base,
                                  ESMCI::Attribute **attpack,
                                  int *rc);
  void FTN_X(c_esmc_attpackremoveattribute)(ESMC_Base **base, char *name,
                                  ESMCI::Attribute **attpack,
                                  ESMC_AttNest_Flag *anflag,
                                  int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attpackgetcharlist)(ESMC_Base **base, char *name,
                                  ESMCI::Attribute **attpack,
                                  ESMC_TypeKind_Flag *tk, int *count,
                                  ESMC_AttNest_Flag *anflag, int *lens,
                                  char *valueList, int *rc,
                                  ESMCI_FortranStrLenArg nlen,
                                  ESMCI_FortranStrLenArg vlen);
  void FTN_X(c_esmc_attpackgetvalue)(ESMC_Base **base, char *name,
                                  ESMCI::Attribute **attpack,
                                  ESMC_TypeKind_Flag *tk, int *count,
                                  ESMC_AttNest_Flag *anflag,
                                  void *value, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attpackgetapinstnames)(ESMC_Base **base,
                                  ESMCI::Attribute **attpack,
                                  char *attPackInstanceNameList,
                                  int *attPackInstanceNameLens,
                                  int *attPackInstanceNameSize,
                                  int *attPackInstanceNameCount,
                                  ESMC_AttNest_Flag *anflag,
                                  int *rc,
                                  ESMCI_FortranStrLenArg napinlen);
  void FTN_X(c_esmc_attpackispresent)(ESMC_Base **base,
                                  ESMCI::Attribute **attpack,
                                  ESMC_Logical *present, int *rc);
  void FTN_X(c_esmc_attpackispresentatt)(ESMC_Base **base, char *name,
                                  ESMCI::Attribute **attpack,
                                  //ESMC_AttNest_Flag *anflag,
                                  ESMC_Logical *present, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attpackispresentindex)(ESMC_Base **base, int *num,
                                  ESMCI::Attribute **attpack,
                                  //ESMC_AttNest_Flag *anflag,
                                  ESMC_Logical *present, int *rc);
  void FTN_X(c_esmc_attpacksetcharlist)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind_Flag *tk, int *count,
                                  char *valueList, int *lens,
                                  ESMCI::Attribute **attpack,
                                  ESMC_AttNest_Flag *anflag, int *rc,
                                  ESMCI_FortranStrLenArg nlen,
                                  ESMCI_FortranStrLenArg vlen);
  void FTN_X(c_esmc_attpacksetvalue)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind_Flag *tk, int *count,
                                  void *value, ESMCI::Attribute **attpack,
                                  ESMC_AttNest_Flag *anflag, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attributeread)(ESMC_Base **base, int *fileNameLen,
                                  const char *fileName, int *schemaFileNameLen,
                                  const char *schemaFileName, int *status,
                                  ESMCI_FortranStrLenArg fnlen,
                                  ESMCI_FortranStrLenArg sfnlen);
  void FTN_X(c_esmc_attributewrite)(ESMC_Base **base,
                                  char *convention, char *purpose,
                                  char *object, char *targetobj,
                                  ESMC_AttWriteFlag *attwriteflag, int *rc,
                                  ESMCI_FortranStrLenArg clen,
                                  ESMCI_FortranStrLenArg plen,
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg tlen);
  void FTN_X(c_esmc_attributecopy)(ESMC_Base **source, ESMC_Base **destination,
                                  ESMC_AttCopyFlag *attcopyflag, int *rc);
  void FTN_X(c_esmc_attributegetcharlist)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind_Flag *tk, int *count, int *lens,
                                  char *valueList, int *rc,
                                  ESMCI_FortranStrLenArg nlen,
                                  ESMCI_FortranStrLenArg vlen);
  void FTN_X(c_esmc_attributegetvalue)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind_Flag *tk,
                                  int *count, void *value, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attpackgetinfoname)(ESMC_Base **base, char *name,
                                  ESMCI::Attribute **attpack,
                                  ESMC_AttNest_Flag *anflag,
                                  ESMC_TypeKind_Flag *tk, int *count,
                                  int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attributegetinfoname)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind_Flag *tk,
                                  int *count, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attributegetinfonum)(ESMC_Base **base, int *num,
                                  char *name,
                                  ESMC_TypeKind_Flag *tk, int *count, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attributegetcount)(ESMC_Base **base, int *count,
                                  ESMC_AttGetCountFlag *flag, int *rc);
  void FTN_X(c_esmc_attributegetcountattpack)(ESMCI::Attribute **attpack,
                                  int *count, ESMC_AttGetCountFlag *gcflag,
                                  ESMC_AttNest_Flag *anflag, int *rc);
  void FTN_X(c_esmc_attributeispresent)(ESMC_Base **base, char *name,
                                  ESMC_Logical *present,
                                  int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attributelink)(ESMC_Base **source, ESMC_Base **destination,
                                  ESMC_Logical *linkChanges, int *rc);
  void FTN_X(c_esmc_attributelinkremove)(ESMC_Base **source,
                                  ESMC_Base **destination, ESMC_Logical *linkChange,
                                  int *rc);
  void FTN_X(c_esmc_attributeremove)(ESMC_Base **base, char *name, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attributesetcharlist)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind_Flag *tk,
                                  int *count, char *valueList, int *lens,
                                  int *rc,
                                  ESMCI_FortranStrLenArg nlen,
                                  ESMCI_FortranStrLenArg vllen);
  void FTN_X(c_esmc_attributesetvalue)(ESMC_Base **base, char *name,
                                  ESMC_TypeKind_Flag *tk,
                                  int *count, void *value, int *rc,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attributesetobjsintree)(ESMC_Base **base, char *object,
                                  char *name,
                                  ESMC_TypeKind_Flag *tk, int *count, void *value,
                                  int *rc,
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg nlen);
  void FTN_X(c_esmc_attributesetobjchrintree)(ESMC_Base **base, char *object,
                                  char *name, char *value, int *rc,
                                  ESMCI_FortranStrLenArg olen,
                                  ESMCI_FortranStrLenArg nlen,
                                  ESMCI_FortranStrLenArg vlen);
  void FTN_X(c_esmc_attributeupdate)(ESMC_Base **base, ESMCI::VM **vm,
                                  int *rootList, int *count,
                                  ESMC_Logical *reconcile, int *rc);
  void FTN_X(c_esmc_attributeupdatereset)(ESMC_Base **base, int *rc);
}

// class utility functions, not methods, since they operate on
//  multiple objects
//int AttributeSetObjectList(ESMC_Base *anytypelist, Attribute *valuelist);
//int AttributeGetObjectList(ESMC_Base *anytypelist, Attribute *valuelist);

#endif  // ESMCI_ATTRIBUTE_H
