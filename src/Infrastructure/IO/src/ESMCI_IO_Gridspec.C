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
// ESMC IO_Gridspec method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO\_Gridspec} methods declared
// in the companion file {\tt ESMCI\_IO\_Gridspec.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_IO_Gridspec.C"

// associated class definition file
#include "ESMCI_IO_Gridspec.h"

// higher level, 3rd party or system includes here
#include <stdio.h>

#include "ESMCI_LogErr.h"

//-------------------------------------------------------------------------
// Prototypes of the C->Fortran interface functions.
extern "C" {
  void FTN_X(f_esmf_gridspec_inq)(const char *filename, int *ndims, int *grid_dims, int *rc,
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
// This section includes all the IO_Gridspec routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_NetCDF - native C++ constructor
//
// !INTERFACE:
  IO_Gridspec::IO_Gridspec(void)
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes for either C++ or F90, since {\tt ESMC\_IO\_Gridspec} is a deep,
//      dynamically allocated class.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::IO_Gridspec() native constructor"

 : ESMC_Base(-1) {  // invoke ESMC_Base constructor with id=(-1); prevents
                    // Base id increment for non-distributed,
                    // non-reconcilable objects such as IO.
    // create default name "IO_Gridspec<ID>"
    ESMC_BaseSetName(ESMC_NULL_POINTER, "IO_Gridspec");
  } // end IO_Gridspec

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_Gridspec - destruct()
//
// !INTERFACE:
  void IO_Gridspec::destruct(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Destruct an IO_Gridspec object
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  } // end destruct()

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_Gridspec::inq -- Reads rank and dimensions from a GRIDSPEC NetCDF file.
//
// !INTERFACE:
  void IO_Gridspec::inq(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
  const char *filename, // (in) - The GRIDSPEC filename
  int *ndims,           // (out) - rank
  int *grid_dims,       // (out) - dimensions
  int *rc               // (out) - return code
  ) {
//
// !DESCRIPTION:
//      Reads rank and dimensions from a GRIDSPEC NetCDF file.
//
//EOP

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI_IO_Gridspec::inq()"

    FTN_X(f_esmf_gridspec_inq)(filename, ndims, grid_dims, rc, strlen(filename));
  }
}  // end namespace ESMCI
