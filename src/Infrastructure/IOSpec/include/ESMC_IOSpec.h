// $Id: ESMC_IOSpec.h,v 1.5.2.2 2009/01/21 21:25:21 cdeluca Exp $
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

#ifndef ESMC_IOSPEC_H
#define ESMC_IOSPEC_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.

 // Predefined file formats
 enum ESMC_IOFileFormat {ESMF_IO_FILEFORMAT_UNSPECIFIED = 0,
                         ESMF_IO_FILEFORMAT_NETCDF,
                         ESMF_IO_FILEFORMAT_HDF};

 // What type of I/O - Read only, write only, R/W, append with truncation
 enum ESMC_IORWType {ESMF_IO_RWTYPE_UNSPECIFIED = 0,
                     ESMF_IO_RWTYPE_READONLY,
                     ESMF_IO_RWTYPE_WRITEONLY,
                     ESMF_IO_RWTYPE_READWRITE,
                     ESMF_IO_RWTYPE_APPEND,
                     ESMF_IO_RWTYPE_TRUNCATE};

// !PUBLIC TYPES:
 class ESMC_IOSpec;

// !PRIVATE TYPES:

 // class definition type
class ESMC_IOSpec {
//class ESMC_IOSpec : public ESMC_Base { // TODO: inherit from ESMC_Base class
                                         // when fully aligned with F90 equiv

  private:   // corresponds to F90 module 'type ESMF_IOSpec' members
    ESMC_Status       iostatus;
    ESMC_IOFileFormat iofileformat;
    ESMC_IORWType     iorwtype;
    char              filename[ESMF_MAXSTR];
    bool              asyncIO;  // TODO:  should be class or enum

  public:

    // TODO:  define methods equivalent to F90

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing
    // TODO:  ReadRestart()/WriteRestart() ?

    // internal validation
    int ESMC_IOSpecValidate(const char *options=0) const;

    // for testing/debugging
    int ESMC_IOSpecPrint(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_IOSpec(void);
    ~ESMC_IOSpec(void);

 // < declare the rest of the public interface methods here >

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

//
//EOP
//-------------------------------------------------------------------------

};  // end class ESMC_IOSpec

#endif // ESMC_IOSPEC_H
