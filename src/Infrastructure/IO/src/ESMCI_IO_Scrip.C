// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC IO_Scrip method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO\_Scrip} methods declared
// in the companion file {\tt ESMCI\_IO\_Scrip.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_IO_Scrip.C"

// associated class definition file
#include "ESMCI_IO_Scrip.h"

// higher level, 3rd party or system includes here
#include <stdio.h>

#include "ESMCI_LogErr.h"

//-------------------------------------------------------------------------
// Prototypes of the C->Fortran interface functions.
extern "C" {
  void FTN_X(f_esmf_scrip_inq)(const char *filename, int *grid_dims, 
                               int *grid_rank, int *rc,
                               ESMCI_FortranStrLenArg len_filename);
}
//-------------------------------------------------------------------------

using namespace std; 

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI
{

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the IO_Scrip routines
//
//
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_NetCDF - native C++ constructor
//
// !INTERFACE:
  IO_Scrip::IO_Scrip(void)
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes for either C++ or F90, since {\tt ESMC\_IO\_Scrip} is a deep,
//      dynamically allocated class.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Scrip() native constructor"
    
 : ESMC_Base(-1) {  // invoke ESMC_Base constructor with id=(-1); prevents
                    // Base id increment for non-distributed,
                    // non-reconcilable objects such as IO.
    // create default name "IO_Scrip<ID>"
    ESMC_BaseSetName(ESMC_NULL_POINTER, "IO_Scrip");
  } // end IO_Scrip

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_Scrip - destruct()
//
// !INTERFACE:
  void IO_Scrip::destruct(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Destruct an IO_Scrip object
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n
    
  } // end destruct()
  
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_Scrip::inq -- Reads rank and dimensions from a SCRIP NetCDF file.
//
// !INTERFACE:
  void IO_Scrip::inq(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
  const char *filename, // (in) - The SCRIP filename
  int *grid_dims,       // (out) - dimensions
  int *grid_rank,       // (out) - rank
  int *rc               // (out) - return code
  ) {
//
// !DESCRIPTION:
//      Reads rank and dimensions from a SCRIP NetCDF file.
//
//EOP
  
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_Scrip::inq()"
    
    FTN_X(f_esmf_scrip_inq)(filename, grid_dims, grid_rank, rc, 
                            strlen(filename));
  }
}  // end namespace ESMCI
