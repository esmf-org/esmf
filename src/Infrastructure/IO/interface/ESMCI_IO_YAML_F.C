// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_IO_YAML_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_F90Interface.h"
#include "ESMCI_IO_YAML.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Util.h"

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
    void FTN_X(c_esmc_io_yamlcreate)(IO_YAML **ptr, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcreate()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // call into C++
      *ptr = IO_YAML::create(&localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamldestroy)(IO_YAML **ptr, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamldestroy()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // call into C++
      localrc = IO_YAML::destroy(ptr);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlread)(IO_YAML **ptr,
      const char *fileName, int *rc, ESMCI_FortranStrLenArg fileName_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlread()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // call into C++
      localrc = (*ptr)->read(std::string(fileName,fileName_l));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlwrite)(IO_YAML **ptr,
      const char *fileName, int *rc, ESMCI_FortranStrLenArg fileName_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlwrite()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // call into C++
      localrc = (*ptr)->write(std::string(fileName,fileName_l));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlprint)(IO_YAML **ptr, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlprint()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // call into C++
      localrc = (*ptr)->write();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlingest)(IO_YAML **ptr,
      const char *content, int *rc, ESMCI_FortranStrLenArg content_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlingest()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // call into C++
      localrc = (*ptr)->ingest(std::string(content,content_l));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcinit)(IO_YAML **ptr,
      const IO_YAML::ContentType::value *type, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcinit()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // set default content type
      IO_YAML::ContentType::value ltype = IO_YAML::ContentType::Unset;
      if (type != NULL) ltype = *type;
      // call into C++
      localrc = (*ptr)->cinit(ltype);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcprint)(IO_YAML **ptr, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcprint()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // call into C++
      localrc = (*ptr)->cwrite();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcwrite)(IO_YAML **ptr,
      const char *filename, int *rc, ESMCI_FortranStrLenArg filename_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcwrite()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // call into C++
      localrc = (*ptr)->cwrite(std::string(filename,filename_l));
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlclinec)(IO_YAML **ptr, int *count, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlclinec()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // call into C++
      *count = (*ptr)->clinec();
      // this call always succeeds
      localrc = ESMF_SUCCESS;
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcsize)(IO_YAML **ptr, int *size, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcsize()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // call into C++
      *size = (*ptr)->csize();
      // this call always succeeds
      localrc = ESMF_SUCCESS;
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlcget)(IO_YAML **ptr, char *data, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlcget()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // call into C++
      if (data) {
        const std::string buffer = (*ptr)->cget();
        if (!buffer.empty()) {
          size_t len = buffer.copy(data, std::strlen(data));
          data[len] = '\0';
        }
      }
      // this call always succeeds
      localrc = ESMF_SUCCESS;
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

    //--------------------------------------------------------------------
    void FTN_X(c_esmc_io_yamlparse)(IO_YAML **ptr,
      const IO_YAML::ParseFormat::value *parseFmt, int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_io_yamlparse()"
      // Initialize return code; assume routine not implemented
      if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
      int localrc = ESMC_RC_NOT_IMPL;
      // check pointer argument
      ESMF_CHECK_POINTER(*ptr, rc)
      // set default parse format
      IO_YAML::ParseFormat::value formOption = IO_YAML::ParseFormat::Unset;
      if (parseFmt != NULL) formOption = *parseFmt;
      // call into C++
      localrc = (*ptr)->parse(formOption);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }

};

} // namespace ESMCI
