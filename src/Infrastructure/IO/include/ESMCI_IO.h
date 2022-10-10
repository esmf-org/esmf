// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef __ESMCI_IO_H
#define __ESMCI_IO_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::IO - IO
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt IO} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_IO.C}
// contains the full code (bodies) for the {\tt IO} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMCI_Base.h"       // Base is superclass to IO
#include "ESMCI_VM.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"
#include "ESMCI_IO_Handler.h"
#include "ESMCI_Info.h"

#include <cstdio>
#include <vector>
#include <string>
#include <utility>

//-------------------------------------------------------------------------

namespace ESMCI {

  enum IOListObjectType { IO_NULL = 1,
                          IO_ARRAY,
                          IO_ATTRIBUTE,
                          IO_GRID,
                          IO_MESH };

  typedef union {
    Array *arr;
    ESMCI::Info *attr;
  } IO_ObjectType;

  struct IO_ObjectContainer {
    enum IOListObjectType type;
    IO_ObjectType object;           // e.g., Array, Attribute
    std::string name;
    ESMCI::Info *dimAttPack;
    ESMCI::Info *varAttPack;
    ESMCI::Info *gblAttPack;
    ESMC_I8 number;

    IO_ObjectContainer () {
      type = IO_NULL;
      object.arr = (Array *)NULL;
      name[0] = '\0';
      dimAttPack = NULL;
      varAttPack = NULL;
      gblAttPack = NULL;
      number = 0;
    }
    IO_ObjectContainer (Array *arr_p, const std::string &arrName,
                        ESMCI::Info *dimAttPack,
                        ESMCI::Info *varAttPack,
                        ESMCI::Info *gblAttPack) {
      type = IO_ARRAY;
      object.arr = arr_p;
      if (arrName.length() > 0)
        name = arrName;

      this->dimAttPack = dimAttPack;
      this->varAttPack = varAttPack;
      this->gblAttPack = gblAttPack;
      number = 0;
    }
    ~IO_ObjectContainer() {
      name = "";
      object.arr = (Array *)NULL;
      if (dimAttPack) {delete dimAttPack;}
      dimAttPack = NULL;
      if (varAttPack) {delete varAttPack;}
      varAttPack = NULL;
      if (gblAttPack) {delete gblAttPack;}
      gblAttPack = NULL;
      number = 0;
      type = IO_NULL;
    }
    Array *getArray(void) {
      return object.arr;
    }
    ESMCI::Info *getAttribute(void) {
      return object.attr;
    }
    const char *getName(void) {
      return name.c_str();
    }

  };


  // classes and structs

  class IO;

  // class definitions
  
  //===========================================================================
  
  //===========================================================================
  
  //===========================================================================
  class IO {
  
  private:
  // global information
    IO_Handler    *ioHandler;
    std::vector<IO_ObjectContainer *> objects;

    // All of the arrays handled by a given IO_Handler must have the same number
    // of tiles. Since an IO object only contains a single IO_Handler, this
    // implies that a given IO object is associated with a specific number of
    // tiles in the arrays it can read and write. We could relax that
    // restriction in the future by having multiple IO_Handlers, each handling a
    // different number of tiles.
    int             ntiles;         // Number of tiles in arrays handled by this object
                                    // (should only be accessed via getNtiles(), even
                                    // internally)
    bool            ntilesIsLocked; // Once ntiles has been set explicitly or queried for
                                    // any purpose, it cannot be changed further;
                                    // ntilesIsLocked indicates that state.
    
  public:
    // native constructor and destructor
    IO(int *rc = NULL) {
      ioHandler = (IO_Handler *)NULL;
      // No constructor call for objects -- use default Allocator

      // Start by assuming ntiles = 1. Typically we expect this to be set
      // explicitly via a call to setOrCheckNtiles before it is accessed.
      // However, there are some theoretical cases where setOrCheckNtiles may
      // not be called, such as writing a file that doesn't contain any arrays
      // (just attributes / metadata). In these cases, we want to default to the
      // single-tile case. However, we want to ensure that a given IO object
      // always uses the same value of ntiles - so once we have explicitly set
      // ntiles or have done any operations that depend on ntiles (such as
      // opening a file), the ntiles value becomes locked and cannot be changed.
      // The ntilesIsLocked variable tracks this state.
      ntiles = 1;
      ntilesIsLocked = false;
      // return successfully
      if (rc != NULL) {
        *rc = ESMF_SUCCESS;
      }
    }
  public:
    ~IO() { destruct(); }
    // create() and destroy()
    static IO *create(int *rc = NULL);
    static int destroy(IO **io);
  private:
    void destruct(void);
  public:
    // read()
    // An atomic read function which transparently handles open and close
    int read(const std::string &file, ESMC_IOFmt_Flag iofmt,
             int *timeslice = NULL);

    // A non-atomic read which is only successful on an open IO stream
    int read(int *timeslice = NULL);

    // write()
    // An atomic write function which transparently handles open and close
    // This version closely matches the functionality in the ESMF
    // interface. If NULL is passed for any arguments (except for file which is
    // is required), defaults will be used.
    int write(const std::string &file,
              ESMC_IOFmt_Flag iofmt,
              bool overwrite,
              ESMC_FileStatus_Flag status,
              int *timeslice = NULL);

    // A non-atomic write which is only successful on an open IO stream
    int write(int *timeslice = NULL);

    // get() and set()
    const char *getName() const { return "ESMCI::IO"; }

    // match()
    static bool match(IO const * const io1, IO const * const io2,
                      int *rc = NULL) {
      if (rc != (int *)NULL) {
        *rc = ESMF_SUCCESS;
      }
      return (io1 == io2);
    }

    // open() and close()
    int open(const std::string &file,
             ESMC_FileStatus_Flag filestatusflag,
             ESMC_IOFmt_Flag iofmt,
             bool overwrite = false,
             bool readonly = false);
    int flush(void);
    int close(void);

    // add and remove objects
    int addArray(Array *arr_p);
    int addArray(Array *arr_p,
                 const std::string &variableName,
                 ESMCI::Info *dimAttPack,
                 ESMCI::Info *varAttPack,
                 ESMCI::Info *gblAttPack);
// TBI
#if 0
    void addAttributes(ESMC_Base *obj_p,
                       const char *schemaFileName,
                       const char *convention,
                       const char *purpose,
                       int *rc=NULL);
    void addGrid(ESMC_Base *grid_p, char *gridName,
                 int *rc=NULL);
#endif // TBI
    void dimlabel_get (ESMCI::Info *dimAttPack,
        std::string labeltype,
        std::vector<std::string> &dimLabels,
        int *rc);
    void dimlabel_merge (std::vector<std::string> &dimLabels,
        std::vector<std::string> &ugdimLabels,
        Array *array,
        int *rc);
    bool redist_check(Array *array_p, int *rc);
    void redist_arraycreate1de(Array *src_array_p, Array **dst_array_p, int petCount, int *rc);
    bool undist_check(Array *array_p, int *rc);
    void undist_arraycreate_alldist(Array *src_array_p, Array **dst_array_p, int *rc);
    void clear();

// TBI
#if 0
    int print() const;
    int validate() const;

    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset,
                  const ESMC_AttReconcileFlag &attreconflag,
                  const ESMC_InquireFlag &inquireflag) const;
    int deserialize(char *buffer, int *offset,
                    const ESMC_AttReconcileFlag &attreconflag);
#endif // TBI

  private:
    const int getNtiles(void) {
      // Once ntiles has been queried for any reason, it becomes locked and cannot be
      // changed. (See comment in the constructor for more details.)
      ntilesIsLocked = true;
      return ntiles;
    }

    // If ntiles has not yet been set (i.e., this function has not yet been
    // called), then set ntiles to the given value. If ntiles has already been
    // set, then verify that ntiles matches the already-set value; if it
    // doesn't, return an error via the return value. Note that all arrays
    // handled by this IO object must have the same number of tiles; to confirm
    // that this is the case, this function should be called for each array,
    // specifying the number of tiles for that array.
    //
    // If this IO object will be handling multi-tile arrays, then this function
    // must be called before calling open or any other function that depends on
    // the number of tiles.
    int setOrCheckNtiles(int ntiles_arg);

    // Verify that the given value of ntiles matches the already-set ntiles (set
    // via a prior call to setOrCheckNtiles); if it doesn't, return an error via
    // the return value. (Note that all arrays handled by this IO object must
    // have the same number tiles.)
    //
    // It is often better to use setOrCheckNtiles, since that version will set
    // ntiles if it hasn't already been set; this version is meant only for the
    // situation where ntiles should already have been set previously.
    int checkNtiles(int ntiles_arg) const;

// Don't know yet if we will need these
#if 0
    // Attribute writing
    int writeStartElement(const std::string& name,
                          const std::string& value,
                          const int     indentLevel,
                          const int     nPairs, ...); // nPairs of
                 // (char *attrName, char *attrValue)

    int writeElement(const std::string& name,
                     const std::string& value,
                     const int     indentLevel,
                     const int     nPairs, ...); // nPairs of
                 // (char *attrName, char *attrValue)

    int writeEndElement(const std::string& name,
                        const int     indentLevel);

    // write an XML comment
    int writeComment(const std::string& comment, const int indentLevel=0);

    int write(int fileNameLen, const char* fileName,
              const char* outChars, int flag);
#endif
  };  // class IO
  //===========================================================================
  
} // namespace ESMCI

#endif // __ESMCI_IO_H
