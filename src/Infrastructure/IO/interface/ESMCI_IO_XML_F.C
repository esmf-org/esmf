// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_F90Interface.h"
#include "ESMCI_IO_XML.h"

#include <string>

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMCI\_IO\_XML} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

namespace ESMCI{

// the interface subroutine names MUST be in lower case
extern "C" {

       //--------------------------------------------------------------------
       void FTN_X(c_esmc_io_xmlcreate)(IO_XML **ptr,
                                    const char *name,
                                    const char *fileName,
                                    ESMC_Base **base,
                                    int *status,
                                    ESMCI_FortranStrLenArg name_l,
                                    ESMCI_FortranStrLenArg filename_l) {
          ESMF_CHECK_POINTER(*base, status)
          *ptr = ESMCI_IO_XMLCreate(std::string(name, name_l),
                                    std::string(fileName, filename_l),
                                         &((*base)->root),  // attributes
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       //--------------------------------------------------------------------
       void FTN_X(c_esmc_io_xmldestroy)(IO_XML **ptr, int *status) {
          int rc = ESMCI_IO_XMLDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       //--------------------------------------------------------------------
       void FTN_X(c_esmc_io_xmlread)(IO_XML **ptr,
                                  const char *fileName,
                                  const char *schemaFileName,
                                  int *status,
                                  ESMCI_FortranStrLenArg fileName_l,
                                  ESMCI_FortranStrLenArg schemaFileName_l) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->IO_XML::read(std::string(fileName,fileName_l),
                                        std::string(schemaFileName, schemaFileName_l) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

};

} // namespace ESMCI
