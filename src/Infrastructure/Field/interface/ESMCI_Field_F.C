// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC interface routines

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here

#include <cstring>
using namespace std;

#include "ESMCI_Base.h"
#include "ESMCI_Array.h"
#include "ESMCI_Field.h"
#include "ESMCI_Grid.h"
#include "ESMCI_IO.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Info.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version =
             "$Id$";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Field routines
//
//

  void FTN_X(c_esmc_fieldioaddarray)(ESMCI::IO **ptr,
                                ESMC_Base **base,
                                ESMCI::Array **array,
                                ESMCI::Grid  **grid,
                                char *variableName,
                                ESMC_Base **gblbase,    // Optional, may be NULL
                                char *conventionName, char *purposeName,
                                int *rc,
                                ESMCI_FortranStrLenArg varname_l,
                                ESMCI_FortranStrLenArg convention_l,
                                ESMCI_FortranStrLenArg purpose_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_fieldioaddarray()"
    // Initialize return code; assume routine not implemented
    if (rc != NULL) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    int localrc = ESMC_RC_NOT_IMPL;
    // helper variable
    string varName;
    if (variableName)
       varName = string (variableName, ESMC_F90lentrim (variableName, varname_l));

    std::string convention;
    if (conventionName)
      convention = string (conventionName, ESMC_F90lentrim (conventionName, convention_l));
    std::string purpose;
    if (purposeName)
      purpose = string (purposeName, ESMC_F90lentrim (purposeName, purpose_l));

    // Key for the "attribute package"
    const std::string key = "/"+convention+"/"+purpose;
    bool has_convpurp = (convention.length() > 0) && (purpose.length() > 0);

    ESMCI::Grid *grid_p = NULL;
    if (has_convpurp) {
      grid_p = *grid;
      if (grid_p->ESMC_BaseGetInfo()->getCountPack() == 0) {
        localrc = ESMF_RC_ATTR_NOTSET;
        if (ESMC_LogDefault.MsgFoundError(localrc, "No Field or Grid AttPacks found", ESMC_CONTEXT,
            rc)) return;
      }
    }

    // If present, use Attributes at the Grid level for dimension names
    ESMCI::Info *dimAttPack = nullptr;
    if (has_convpurp) {
      try {
        if (grid_p->ESMC_BaseGetInfo()->hasKey(key, true, false)) {
          dimAttPack = new ESMCI::Info();
          grid_p->ESMC_BaseGetInfo()->get(*dimAttPack, key);
        }
      } catch (ESMCI::esmc_error &exc) {
        ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), exc.what(), ESMC_CONTEXT, rc);
        return;
      }
    }

    // If present, use Attributes at the Field level for variable attributes
    ESMC_Base *base_p = *base;
    ESMCI::Info *varAttPack = nullptr;
    if (has_convpurp) {
      try {
        if (base_p->ESMC_BaseGetInfo()->hasKey(key, true, false)) {
          varAttPack = new ESMCI::Info();
          base_p->ESMC_BaseGetInfo()->get(*varAttPack, key);
        }
      } catch (ESMCI::esmc_error &exc) {
        ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), exc.what(), ESMC_CONTEXT, rc);
        return;
      }
    }

    // If present, use Attributes at the FieldBundle level for global attributes
    ESMCI::Info *gblAttPack = nullptr;
    if (gblbase) {
      ESMC_Base *gblbase_p = *gblbase;
      if (has_convpurp) {
        try {
          if (gblbase_p->ESMC_BaseGetInfo()->hasKey(key, true, false)) {
            gblAttPack = new ESMCI::Info();
            gblbase_p->ESMC_BaseGetInfo()->get(*gblAttPack, key);
          }
        } catch (ESMCI::esmc_error &exc) {
          ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), exc.what(), ESMC_CONTEXT, rc);
          return;
        }
      }
    }

    // call into C++
    localrc = (*ptr)->addArray(*array, varName, dimAttPack, varAttPack, gblAttPack);
    ESMC_LogDefault.MsgFoundError(localrc,
                                  ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
    if (rc != NULL) {
      *rc = localrc;
    }
  }

// non-method functions
void FTN_X(c_esmc_fieldserialize)(
                ESMC_Status *status,
                ESMC_Status *iostatus,
                int * dimCount,
                int * gridToFieldMap,
                int * ungriddedLBound,
                int * ungriddedUBound,
                int * totalLWidth,
                int * totalUWidth,
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *rc,
                ESMCI_FortranStrLenArg buf_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_fieldserialize()"
    ESMC_InquireFlag linquireflag = *inquireflag;
    int i;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    // TODO: verify length > 4 status vars, and if not, make room.
    int fixedpart = 4 * sizeof(int *) + sizeof(int) + 5 * ESMF_MAXDIM * sizeof(int);
    if ((*inquireflag != ESMF_INQUIREONLY) && (*length - *offset) < fixedpart) {

         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add a Field object", ESMC_CONTEXT, rc);
         return;

        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    sp = (ESMC_Status *)(buffer + *offset);
    if (linquireflag != ESMF_INQUIREONLY) {
      *sp++ = *status;
      *sp++ = *iostatus;
    } else
      sp += 2;

    // copy the rest of the field parameters
    // we are explicitly assuming Fortran-integer is of size C-int
    // This might be probelmatic on 64bit machines depending how compiler flag is used
    // e.g. we know gridToFieldMap is of type cpu_word *, its element maybe int32 or int64
    // depending on the size of Fortran-integer
    char * ptr = (char *)sp;
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)dimCount, sizeof(int));
    ptr += sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)gridToFieldMap, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)ungriddedLBound, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)ungriddedUBound, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)totalLWidth, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)totalUWidth, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);

    *offset = ptr - buffer;

    if (rc) *rc = ESMF_SUCCESS;

    return;
}


void FTN_X(c_esmc_fielddeserialize)(
                ESMC_Status *status,
                ESMC_Status *iostatus,
                const int * dimCount,
                const int * gridToFieldMap,
                const int * ungriddedLBound,
                const int * ungriddedUBound,
                const int * totalLWidth,
                const int * totalUWidth,
                const char *buffer,
                int *offset,
                int *rc,
                ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_fielddeserialize()"
    int i;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    sp = (ESMC_Status *)(buffer + *offset);
    *status = *sp++;
    *iostatus = *sp++;

    char * ptr = (char *)sp;
    memcpy((void *)dimCount, (const void *)ptr, sizeof(int));
    ptr += sizeof(int);
    memcpy((void *)gridToFieldMap, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ungriddedLBound, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ungriddedUBound, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)totalLWidth, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)totalUWidth, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);

    *offset = ptr - buffer;

    if (rc) *rc = ESMF_SUCCESS;

    return;
}


}
