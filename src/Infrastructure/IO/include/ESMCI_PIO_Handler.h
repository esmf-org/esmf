// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
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

#ifndef __ESMCI_PIO_HANDLER_H
#define __ESMCI_PIO_HANDLER_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::PIO_Handler - IO
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt PIO\_Handler} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_PIO\_Handler.C}
// contains the full code (bodies) for the {\tt PIO\_Handler} methods.
// {\tt PIO\_Handler} is derived from the {\tt IO\_Handler}
// base class.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMCI_VM.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"
#include "ESMCI_IO_Handler.h"       // IO_Handler is superclass to PIO_Handler
#include "mpi.h"

#include <vector>

// PIO include files
#include "pio.h"

//-------------------------------------------------------------------------

namespace ESMCI {

  // classes and structs

  class PIO_Handler;       // The main class for implementing PIO functionality
  class PIO_SystemHandler; // For managing PIO system instances
//  class PIO_IODescHandler; // For managing PIO IO descriptors

  // class definitions
  
  //===========================================================================
  
  //===========================================================================
  
  //===========================================================================
  class PIO_Handler : public IO_Handler {    // inherits from IO_Handler class
  
  private:

    // global information
    static std::vector<pio_iosystem_desc_t> activePioInstances;
    pio_iosystem_desc_t pioSystemDesc; // Descriptor for initialized PIO inst.
    pio_file_desc_t pioFileDesc;       // Descriptor for open PIO file
    pio_io_desc_t pioIODesc;           // Descriptor created by initdecomp
    MPI_Comm communicator;
    int my_rank;
    int num_iotasks;
    int num_aggregators;
    int stride;
    int rearr;
    int base;
    int user_count;
    bool new_file;

  public:
    // native constructor and destructor
    PIO_Handler(ESMC_IOFmt_Flag fmtArg, int *rc);
    // Static initialize and finalize routines for PIO
    static void initialize(int comp_rank, MPI_Comm comp_comm,
                           int num_iotasks, int num_aggregator,
                           int stride, int rearr, int *base_p, int *rc = NULL);
// Deferred for when intercom support is desired
//    static PIO_Handler *initialize(int component_count, MPI_Comm peer_comm,
//                                   MPI_Comm *comp_comms, MPI_Comm io_comm,
//                                   int *rc = NULL);
    static void finalize(int *rc = NULL);
    // Be able to see if PIO is initialized
    static ESMC_Logical isPioInitialized(void);
    // Non-static member for default initialization
    int initializeVM(void);
  public:
    ~PIO_Handler() { destruct(); }
  private:
    void destruct(void);
  public:

    // read()
    // Non-atomic reads which are only successful on an open IO stream
    void arrayRead(Array *arr_p, const char * const name,
                   int *timeslice = NULL, int *rc = NULL);

    // write()
    // Non-atomic writes which are only successful on an open IO stream
    void arrayWrite(Array *arr_p, const char * const name,
                    const std::vector<std::string> &dimLabels,
                    int *timeslice = NULL,
                    const Attribute *varAttPack = NULL,
                    const Attribute *gblAttPack = NULL,
                    int *rc = NULL);

    // get() and set()
  public:

    // match()
    static bool match(PIO_Handler const * const ioh1,
                      PIO_Handler const * const ioh2,
                      int *rc = NULL) {
      if (rc != (int *)NULL) {
        *rc = ESMF_SUCCESS;
      }
      return (ioh1 == ioh2);
    }

    // open() and close()
    void open(bool readonly_arg, int *rc = NULL);
    ESMC_Logical isOpen(void);
    ESMC_Logical isNewFile(void) {
      return (new_file ? ESMF_TRUE : ESMF_FALSE);
    }
    void flush(int *rc = NULL);
    void close(int *rc = NULL);

  private:
    pio_io_desc_t getIODesc(pio_iosystem_desc_t iosys, Array *arr_p,
                            int ** iodims = (int **)NULL,
                            int *nioDims = (int *)NULL,
                            int ** arrdims = (int **)NULL,
                            int *narrDims = (int *)NULL,
                            int *basepiotype = (int *)NULL,
                            int *rc = (int *)NULL);
    void attPackPut (pio_var_desc_t vardesc, const Attribute *attPack, int *rc);
    // Error recording routine
    static bool CheckPIOError(int pioRetCode,
                              int line, const char * const file,
                              const char * const method,
                              const std::string &fmtStr,
                              int rc_code,
                              int *rc,
                              bool warn = false);

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
  //===========================================================================
  
} // namespace ESMCI

#endif // __ESMCI_PIO_HANDLER_H
