// $Id: ESMCI_IO_XML_F.C,v 1.1.2.1 2010/02/05 19:58:01 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
#include <ESMCI_F90Interface.h>
#include <ESMCI_IO_XML.h>
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
       void FTN(c_esmc_io_xmlcreate)(IO_XML **ptr,
                                    int *nameLen,
                                    const char *name,
                                    ESMC_Base **base,
                                    int *status) {
          ESMF_CHECK_POINTER(*base, status)
          *ptr = ESMCI_IO_XMLCreate(
                                           *nameLen,   // always present 
                                                       //   internal argument.
                    ESMC_NOT_PRESENT_FILTER(name),
                                         &((*base)->root),  // attributes
                    ESMC_NOT_PRESENT_FILTER(status) );
       }

       //--------------------------------------------------------------------
       void FTN(c_esmc_io_xmldestroy)(IO_XML **ptr, int *status) {
          int rc = ESMCI_IO_XMLDestroy(ptr);
          if (ESMC_PRESENT(status)) *status = rc;
       }

       //--------------------------------------------------------------------
       void FTN(c_esmc_io_xmlread)(IO_XML **ptr,
                                  int *fileNameLen,
                                  const char *fileName,
                                  int *status) {
          ESMF_CHECK_POINTER(*ptr, status)
          int rc = (*ptr)->IO_XML::read(
                                           *fileNameLen, // always present 
                                                         // internal argument.
                    ESMC_NOT_PRESENT_FILTER(fileName) );
          if (ESMC_PRESENT(status)) *status = rc;
       }

};

} // namespace ESMCI
