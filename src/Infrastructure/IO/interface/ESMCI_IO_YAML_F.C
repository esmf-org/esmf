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
#include "ESMCI_Util.h"
#include "ESMCI_IO_YAML.h"

#include <string>
#include <cstring>

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMCI\_IO\_YAML} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

namespace ESMCI {

  // the interface subroutine names MUST be in lower case
  extern "C" {

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcreate)(IO_YAML **ptr,
                                     int      *status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcreate()"
      int rc;
      *ptr = IO_YAML::create(&rc);
      if (ESMC_PRESENT(status)) *status = rc;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamldestroy)(IO_YAML **ptr, 
                                      int      *status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamldestroy()"
      int rc = IO_YAML::destroy(ptr);
      if (ESMC_PRESENT(status)) *status = rc;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlread)(IO_YAML   **ptr,
                                   const char *fileName,
                                   int        *status,
                                   ESMCI_FortranStrLenArg fileName_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlread()"
      ESMF_CHECK_POINTER(ptr, status)
      int rc = (*ptr)->read(std::string(fileName,fileName_l));
      if (ESMC_PRESENT(status)) *status = rc;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlwrite)(IO_YAML  **ptr,
                                   const char *fileName,
                                   int        *status,
                                   ESMCI_FortranStrLenArg fileName_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlwrite()"
      ESMF_CHECK_POINTER(ptr, status)
      int rc = (*ptr)->write(std::string(fileName,fileName_l));
      if (ESMC_PRESENT(status)) *status = rc;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlprint)(IO_YAML **ptr,
                                   int       *status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlprint()"
      ESMF_CHECK_POINTER(ptr, status)
      int rc = (*ptr)->write();
      if (ESMC_PRESENT(status)) *status = rc;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlingest)(IO_YAML   **ptr,
                                     const char *content,
                                     int        *status,
                                     ESMCI_FortranStrLenArg content_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlingest()"
      ESMF_CHECK_POINTER(ptr, status)
      int rc = (*ptr)->ingest(std::string(content,content_l));
      if (ESMC_PRESENT(status)) *status = rc;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcinit)(IO_YAML   **ptr,
                           const IO_YAML::ContentType::value *type,
                           int                          *status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcinit()"
      ESMF_CHECK_POINTER(ptr, status)
      IO_YAML::ContentType::value ltype = IO_YAML::ContentType::Unset;
      if (type != NULL) ltype = *type;
      int rc = (*ptr)->cinit(ltype);
      if (ESMC_PRESENT(status)) *status = rc;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcprint)(IO_YAML   **ptr,
                                     int        *status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcprint()"
      ESMF_CHECK_POINTER(ptr, status)
      int rc = (*ptr)->cwrite();
      if (ESMC_PRESENT(status)) *status = rc;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcwrite)(IO_YAML   **ptr,
                                     const char *filename,
                                     int        *status,
                                     ESMCI_FortranStrLenArg filename_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcwrite()"
      ESMF_CHECK_POINTER(ptr, status)
      int rc = (*ptr)->cwrite(std::string(filename,filename_l));
      if (ESMC_PRESENT(status)) *status = rc;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcsize)(IO_YAML   **ptr,
                                   int        *size,
                                   int        *status     ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcsize()"
      ESMF_CHECK_POINTER(ptr, status)
      *size = (*ptr)->csize();
      if (ESMC_PRESENT(status)) *status = ESMF_SUCCESS;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcget)(IO_YAML   **ptr,
                                   char       *data,
                                   int        *status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcget()"
      ESMF_CHECK_POINTER(ptr, status)
      if (data) {
        const std::string buffer = (*ptr)->cget();
        size_t len = buffer.copy(data, std::strlen(data));
        data[len] = '\0';
      }
      if (ESMC_PRESENT(status)) *status = ESMF_SUCCESS;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlparse)(IO_YAML **ptr,
                              const IO_YAML::ParseFormat::value *parseFmt,
                              int                               *status) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlparse()"
      ESMF_CHECK_POINTER(ptr, status)
      IO_YAML::ParseFormat::value formOption = IO_YAML::ParseFormat::Unset;
      if (parseFmt != NULL) formOption = *parseFmt;
      int rc = (*ptr)->parse(formOption);
      if (ESMC_PRESENT(status)) *status = rc;
    }

};

} // namespace ESMCI
