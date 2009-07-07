// $Id: ESMCI_IO_NetCDF.h,v 1.1 2009/07/07 06:00:17 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF IO C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMCI_IO_NetCDF_H
#define ESMCI_IO_NetCDF_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMC_Start.h"

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI::IO_NetCDF - TODO
//
// !DESCRIPTION:
//  TODO
//-------------------------------------------------------------------------
//
// !USES:
#include "ESMC_Base.h"           // inherited Base class TODO ?
#include <ESMCI_IO.h>
#include <ESMCI_State.h>

#ifdef ESMF_NETCDF
#include <netcdfcpp.h>
#include <ncvalues.h>
#endif

namespace ESMCI
{

// !PUBLIC TYPES:
 class IO_NetCDF;

// !PRIVATE TYPES:
 // class configuration type:  not needed for IO_NetCDF TODO ?

 // class definition type
 class IO_NetCDF : IO { // TODO: inherit public ESMC_Base class when
            // fully aligned with F90 equiv 
  private:   // corresponds to F90 module 'type ESMF_IO_NetCDF' members
    char         name[ESMF_MAXSTR];  // name of io object
    ESMC_Base   *base;    // associated object's base
    char         fileName[ESMF_MAXSTR];
    State*       theState;
    int          id;          // unique identifier. used for equality
                              //    checks and to generate unique default
                              //    names.
                              //    TODO: inherit from ESMC_Base class
    static int   count;       // number of io objects created. Thread-safe
                              //   because int is atomic.
                              //    TODO: inherit from ESMC_Base class

// !PUBLIC MEMBER FUNCTIONS:

  public:
    // IO_NetCDF doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required TODO ?

    // accessor methods

    // Get the State object
    State*  getState()  { return theState; }
    void  setState(State*  newState)  { theState = newState; }

    // Read/Write to support the F90 optional arguments interface
    int read(int fileNameLen, const char* fileName);
    int write(int fileNameLen, const char* fileName);

    // internal validation
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0) const;

    // native C++ constructors/destructors
    IO_NetCDF(void);
    // IO_NetCDF(const IO_NetCDF &io);  TODO
    ~IO_NetCDF(void);

    // friend function to allocate and initialize IO_NetCDF object from heap
    friend IO_NetCDF *ESMCI_IO_NetCDFCreate(int, const char*, ESMC_Base*, int*);

    // friend function to copy an io  TODO ?
    //friend IO_NetCDF *ESMCI_IO(IO_NetCDF*, int*);

    // friend function to de-allocate IO_NetCDF
    friend int ESMCI_IO_NetCDFDestroy(IO_NetCDF**);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

#ifdef ESMF_NETCDF
    ESMC_TypeKind  ncToEsmcType(NcType  ncTypeVal);
    NcType         esmcToNcType(ESMC_TypeKind  esmcTypeVal);

    Array*  readArray(NcFile*  netCdfFile,
                      int      varIndex);
    int     writeArray(NcFile*  netCdfFile,
                       Array*   thisArray,
                       int      numDims,
                       NcDim**  dimensions);
#endif

//
//EOP
//-------------------------------------------------------------------------

};  // end class IO_NetCDF

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++.
    // These also establish defaults to match F90 optional args.  TODO ?

    // friend function to allocate and initialize io from heap
    IO_NetCDF *ESMCI_IO_NetCDFCreate(int nameLen, const char* name=0,
                       ESMC_Base* base=0, int* rc=0);

    // friend function to copy an io  TODO ?
    //IO_NetCDF *ESMCI_IO_NetCDFCreate(IO_NetCDF *io, int *rc=0);

    // friend function to de-allocate clock
    int ESMCI_IO_NetCDFDestroy(IO_NetCDF **io);

    // friend to restore state  TODO ?
    //Clock *ESMCI_IO_NetCDFReadRestart(int nameLen,
                                   //const char*  name=0,
                                   //ESMC_IO_NetCDFSpec* iospec=0,
                                   //int*         rc=0);

}   // namespace ESMCI

#endif // ESMC_IO_NetCDF_H
