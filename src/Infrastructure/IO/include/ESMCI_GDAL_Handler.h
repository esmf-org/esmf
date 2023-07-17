// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research, 
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

#ifndef __ESMCI_GDAL_HANDLER_H
#define __ESMCI_GDAL_HANDLER_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::GDAL_Handler - IO
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt GDAL\_Handler} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_GDAL\_Handler.C}
// contains the full code (bodies) for the {\tt GDAL\_Handler} methods.
// {\tt GDAL\_Handler} is derived from the {\tt IO\_Handler}
// base class.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMCI_VM.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"
#include "ESMCI_IO_Handler.h"       // IO_Handler is superclass to GDAL_Handler
#include "mpi.h"

#include <vector>

// GDAL include files
#include <ogr_api.h>
#include <gdal.h>
#include <ogr_srs_api.h>

//-------------------------------------------------------------------------

namespace ESMCI {

  // classes and structs

  class GDAL_Handler;       // The main class for implementing GDAL functionality
  class GDAL_SystemHandler;
  // class definitions
  
  //===========================================================================
  
  //===========================================================================
  
  //===========================================================================
  class GDAL_Handler : public IO_Handler {    // inherits from IO_Handler class
  
  private:

    // global information
    static std::vector<int> activeGdalInstances;
    int gdalSystemDesc; // Descriptor for initialized GDAL inst.
    int *gdalFileDesc;  // Descriptor(s) for open GDAL file (typically just one, but multiple for I/O of multi-tile arrays)
    MPI_Comm communicator;
    int my_rank;
    int num_iotasks;
    int stride;
    int rearr;
    int base;
    bool *new_file; // Typically just one value, but multiple for I/O of multi-tile arrays

  public:
    // native constructor and destructor
    GDAL_Handler(ESMC_IOFmt_Flag fmtArg, int ntilesArg, int *rc);
    // Static initialize and finalize routines for GDAL
    static void initialize(int comp_rank, MPI_Comm comp_comm,
                           int num_iotasks, 
                           int stride, int rearr, int *base_p, int *rc = NULL);
    static void finalize(int *rc = NULL);
    // Be able to see if GDAL is initialized
    static ESMC_Logical isGdalInitialized(void);
    // Non-static member for default initialization
    int initializeVM(void);
  public:
    ~GDAL_Handler() { destruct(); }
  private:
    void destruct(void);
  public:

    // read()
    // Non-atomic reads which are only successful on an open IO stream
    void arrayReadOneTileFile(Array *arr_p, int tile, const char * const name,
                              int *timeslice = NULL, int *rc = NULL);

    // write()
    // Non-atomic writes which are only successful on an open IO stream
    void arrayWriteOneTileFile(Array *arr_p, int tile, const char * const name,
                               const std::vector<std::string> &dimLabels,
                               int *timeslice = NULL,
                               const ESMCI::Info *varAttPack = NULL,
                               const ESMCI::Info *gblAttPack = NULL,
                               int *rc = NULL);

    // get() and set()
  public:

    // match()
    static bool match(GDAL_Handler const * const ioh1,
                      GDAL_Handler const * const ioh2,
                      int *rc = NULL) {
      if (rc != (int *)NULL) {
        *rc = ESMF_SUCCESS;
      }
      return (ioh1 == ioh2);
    }

    // open() and close()
    void openOneTileFile(int tile, bool readonly_arg, int *rc = NULL);
    ESMC_Logical isOpen(int tile);
    ESMC_Logical isNewFile(int tile) {  // note 1-based indexing for tile
      return (new_file[tile-1] ? ESMF_TRUE : ESMF_FALSE);
    }
    void flushOneTileFile(int tile, int *rc = NULL);
    void closeOneTileFile(int tile, int *rc = NULL);

  private:
    int getIODesc(int iosys, Array *arr_p, int tile,
                            int ** iodims = (int **)NULL,
                            int *nioDims = (int *)NULL,
                            int ** arrdims = (int **)NULL,
                            int *narrDims = (int *)NULL,
                            int *basegdaltype = (int *)NULL,
                            int *rc = (int *)NULL);
    void attPackPut (int vardesc, const ESMCI::Info *attPack, int tile, int *rc);

  public:
    // Error recording routine
    static bool CheckGDALError(int gdalRetCode,
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
  
// Macros

// For error checking
#define CHECKGDALERROR(_err, _str, _rc_code, _rc)                                        \
  GDAL_Handler::CheckGDALError((_err), ESMC_CONTEXT, (_str), _rc_code, &(_rc))
#define CHECKGDALWARN(_err, _str, _rc_code, _rc)                                         \
  GDAL_Handler::CheckGDALError((_err), ESMC_CONTEXT, (_str), _rc_code, &(_rc), true)


} // namespace ESMCI

#endif // __ESMCI_GDAL_HANDLER_H
