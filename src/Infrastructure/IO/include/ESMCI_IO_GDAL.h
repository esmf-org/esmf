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
// ESMF IO_NetCDF C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMCI_IO_GDAL_H
#define ESMCI_IO_GDAL_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMCI_Macros.h"

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI::IO_GDAL - Handles low-level GDAL IO for ESMF internals and user API.
//
// !DESCRIPTION:
//  TODO
//-------------------------------------------------------------------------
//
// !USES:
#include "ESMCI_Base.h"           // inherited Base class
#include "ESMCI_State.h"

#include <string>

#ifdef ESMF_GDAL
#include <ogr_api.h>
#include <gdal.h>
#include <ogr_srs_api.h>
#endif

namespace ESMCI
{

// !PUBLIC TYPES:
 class IO_GDAL;

// !PRIVATE TYPES:

 // class definition type
 class IO_GDAL : public ESMC_Base { // inherit ESMC_Base class
  private:   // corresponds to F90 module 'type ESMF_IO_GDAL' members
    ESMC_Base   *base;    // associated object's base
    std::string  fileName;
    State*       theState;

// !PUBLIC MEMBER FUNCTIONS:

  public:
    // accessor methods

    // Get the State object
    State*  getState()  { return theState; }
    void  setState(State*  newState)  { theState = newState; }

    // Read/Write to support the F90 optional arguments interface
    int read(const std::string& fileName);
    int write(const std::string& fileName);

    // internal validation
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0) const;

    // native C++ constructors/destructors
    IO_GDAL(void);
    // IO_GDAL(const IO_GDAL &io_gdal);  TODO
    ~IO_GDAL(){destruct();}
   private:
    void destruct();
   
   public:
    // friend function to allocate and initialize IO_GDAL object from heap
    friend IO_GDAL *ESMCI_IO_GDALCreate(const std::string&, ESMC_Base*, int*);

    // friend function to copy an io_gdal  TODO ?
    //friend IO_GDAL *ESMCI_IO_GDAL(IO_GDAL*, int*);

    // friend function to de-allocate IO_GDAL
    friend int ESMCI_IO_GDALDestroy(IO_GDAL**);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

#ifdef ESMF_GDAL
//    int ncerrToEsmcRc (int ncerr);
    ESMC_TypeKind_Flag  gdalToEsmcType(OGRFieldType gdalTypeVal);
//    nc_type         esmcToNcType(ESMC_TypeKind_Flag  esmcTypeVal);

//    Array*  readArray(NcFile  netCdfFile,
//                       int    varIndex,
//                       int    *rc);
//    int     writeArray(NcFile netCdfFile,
//                       Array* thisArray,
//                       int    numDims,
//                       int*   dimensions);
#endif

//
//EOP
//-------------------------------------------------------------------------

};  // end class IO_GDAL

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++.
    // These also establish defaults to match F90 optional args.  TODO ?

    // friend function to allocate and initialize io from heap
    IO_GDAL *ESMCI_IO_GDALCreate(const std::string& name=0,
                       ESMC_Base* base=0, int* rc=0);

    // friend function to copy an io_netcdf  TODO ?
    //IO_GDAL *ESMCI_IO_GDALCreate(IO_GDAL *io_netcdf, int *rc=0);

    // friend function to de-allocate clock
    int ESMCI_IO_GDALDestroy(IO_GDAL **io_netcdf);

    // friend to restore state  TODO ?
    //Clock *ESMCI_IO_GDALReadRestart(const std::string& name=0,
                                   //int*         rc=0);

}   // namespace ESMCI

#endif // ESMC_IO_GDAL_H
