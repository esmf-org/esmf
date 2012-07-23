// $Id: ESMCI_IO_Handler.h,v 1.1 2012/07/23 20:20:54 gold2718 Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
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

#ifndef __ESMCI_IO_HANDLER_H
#define __ESMCI_IO_HANDLER_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::IO_Handler - IO
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt IO\_Handler} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_IO\_Handler.C}
// contains the full code (bodies) for the {\tt IO\_Handler} methods.
// Note that {\tt IO\_Handler} is an abstract base class useful to the
// {\tt IO} class.
// Note that the create and destroy methods will create (or destroy) the
// correct derived class based on the I/O format passed to create.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMCI_Base.h"       // Base is superclass to IO_Handler
#include "ESMCI_VM.h"
#include "ESMCI_Array.h"
#include "ESMCI_Util.h"

#include <cstdio>
#include <vector>

//-------------------------------------------------------------------------

namespace ESMCI {

  // IO-specific flags

  enum IOReadFlag { IO_NO_READ = 1,
                    IO_READ };

  enum IOWriteFlag { IO_NO_WRITE = 1,
                     IO_APPEND,
                     IO_NEW,
                     IO_TRUNCATE };

  // classes and structs

  class IO_Handler;

  // class definitions
  
  //===========================================================================
  
  //===========================================================================
  
  //===========================================================================
  class IO_Handler : public ESMC_Base {    // inherits from ESMC_Base class
  
  private:
    // global information
    int            localPet;
    ESMC_IndexFlag indexflag;
    ESMC_IOFmtFlag iofmtFlag;
    char           filename[ESMF_MAXSTR]; // The filename for this object
    
  protected:
    // native constructor and destructor
    IO_Handler(ESMC_IOFmtFlag *fmtArg);
    // prevent baseID counter increment
    IO_Handler(ESMC_IOFmtFlag *fmtArg, int baseID);
  private:
//    IO(ESMC_IOFmtFlag fmtArg, int rank, int *rc);
  protected:
    ~IO_Handler() { destruct(); }
    // create() and destroy()
  public:
    static IO_Handler *create(ESMC_IOFmtFlag *iofmt, int *rc = NULL);
    static IO_Handler *create(char const * const file,
                              ESMC_IOFmtFlag *iofmt, int *rc = NULL);
    static int destroy(IO_Handler **io);
    static void finalize(int *rc = NULL);
  private:
    virtual void destruct(void) { }
  public:

    // read()
    // Non-atomic reads which are only successful on an open IO stream
    virtual void arrayRead(Array *arr_p, const char *name,
                           int *timeslice = NULL, int *rc = NULL) = 0;

    // write()
    // Non-atomic writes which are only successful on an open IO stream
    virtual void arrayWrite(Array *arr_p, const char *name,
                            int *timeslice = NULL, int *rc = NULL) = 0;

    // get() and set()
  public:
    const char *getName(void) const { return ESMC_BaseGetName(); }
    ESMC_IOFmtFlag getFormat(void) { return iofmtFlag; }
    virtual bool formatOk(ESMC_IOFmtFlag *newIofmt) {
      return (((ESMC_IOFmtFlag *)NULL != newIofmt) &&
              (*newIofmt == iofmtFlag));
    }
    const char *getFilename(void) const { return filename; }
  protected:
    virtual void setFormat(ESMC_IOFmtFlag *newIofmt) {
      if ((ESMC_IOFmtFlag *)NULL != newIofmt) {
        iofmtFlag = *newIofmt;
      }
    }
    int setFilename(const char * const name);
    bool fileExists(bool needRead, bool needWrite);
  public:
    virtual int setName(const char *name) {
            return ESMC_BaseSetName(name, "IO_Handler");
    }

    // match()
    static bool match(IO_Handler const * const ioh1,
                      IO_Handler const * const ioh2,
                      int *rc = NULL) {
      if (rc != (int *)NULL) {
        *rc = ESMF_SUCCESS;
      }
      return (ioh1 == ioh2);
    }

    // open() and close()
    virtual void open(IOReadFlag *ioreadflag,
                      IOWriteFlag *iowriteflag,
                      int *rc = NULL) = 0;
    void open(char const * const file,
              IOReadFlag *ioreadflag,
              IOWriteFlag *iowriteflag,
              int *rc = NULL);
    virtual ESMC_Logical isOpen(void) { return ESMF_FALSE; }
    virtual void flush(int *rc = NULL) = 0;
    virtual void close(int *rc = NULL) = 0;

// Don't know yet if we will need these
#if 0
  private:
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
#endif // 0
  };  // class IO_Handler
  //============================================================================
  
} // namespace ESMCI

#endif // __ESMCI_IO_HANDLER_H
