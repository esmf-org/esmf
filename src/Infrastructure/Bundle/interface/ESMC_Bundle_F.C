// $Id: ESMC_Bundle_F.C,v 1.7 2007/06/23 04:00:04 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
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
// The code in this file implements the C++ {\tt ESMC\_Bundle} methods declared
// in the companion file ESMC_Bundle.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"

 // associated class definition file
#include "ESMC_Bundle.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_Bundle_F.C,v 1.7 2007/06/23 04:00:04 cdeluca Exp $";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Bundle routines
//
//

#if 0
      type ESMF_PackFlag
        integer :: packflag
      end type

      type(ESMF_PackFlag), parameter :: ESMF_PACKED_DATA = ESMF_PackFlag(1), &
                                        ESMF_NO_PACKED_DATA = ESMF_PackFlag(2)

      type ESMF_BundleFieldAccess
         type(ESMF_InterleaveFlag) :: bfa_type
         integer :: bfa_start
         integer :: bfa_end
         integer :: bfa_strides
      end type

      type ESMF_BundleFieldInterleave
        integer :: field_order                      ! index of this field
        type(ESMF_FieldDataMap) :: field_dm         ! copy of this fields dm
        type(ESMF_BundleFieldAccess) :: field_bfa   ! access info if packed
      end type

      type ESMF_LocalBundle
        type(ESMF_Array) :: packed_data               ! local packed array
        type(ESMF_Status) :: igridstatus
        type(ESMF_Status) :: arraystatus
        integer :: accesscount
      end type

      type ESMF_BundleType
        type(ESMF_Base) :: base                   ! base class object
        type(ESMF_Field), dimension(:), pointer :: flist
        type(ESMF_Status) :: bundlestatus
        type(ESMF_Status) :: igridstatus
        integer :: field_count
        type(ESMF_IGrid) :: igrid                  ! associated global igrid
        type(ESMF_LocalBundle) :: localbundle    ! this differs per DE
        type(ESMF_Packflag) :: pack_flag         ! is packed data present?
        type(ESMF_BundleFieldInterleave) :: fil  ! ordering in buffer
        type(ESMF_BundleDataMap) :: mapping      ! map info
        type(ESMF_IOSpec) :: iospec              ! iospec values
        type(ESMF_Status) :: iostatus            ! if unset, inherit from gcomp
      
      end type
#endif


// non-method functions
void FTN(c_esmc_bundleserialize)(ESMC_Status *bundlestatus, 
                            ESMC_Status *igridstatus, 
                            int *field_count, 
                            int *pack_flag, 
                            void *mapping, 
                            ESMC_Status *iostatus, 
                            void *buffer, int *length, int *offset, int *localrc){

    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;
    int *ip;

    // TODO: verify length > need, and if not, make room.
    int fixedpart = 8 * sizeof(int *);
    if ((*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                            "Buffer too short to add a Bundle object", localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    sp = (ESMC_Status *)((char *)(buffer) + *offset);
    *sp++ = *bundlestatus;
    *sp++ = *igridstatus; 
    ip = (int *)sp;
    *ip++ = *field_count; 
    *ip++ = *pack_flag; 
    //*ip++ = *mapping; 
    sp = (ESMC_Status *)ip;
    *sp++ = *iostatus; 

    *offset = (char *)sp - (char *)buffer;

    return;
} 


// non-method functions
void FTN(c_esmc_bundledeserialize)(ESMC_Status *bundlestatus, 
                              ESMC_Status *igridstatus, 
                              int *field_count, 
                              int *pack_flag, 
                              void *mapping, 
                              ESMC_Status *iostatus, 
                              void *buffer, int *offset, int *localrc){

    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;
    int *ip;

    sp = (ESMC_Status *)((char *)(buffer) + *offset);
    *bundlestatus = *sp++;
    *igridstatus = *sp++;
    ip = (int *)sp;
    *field_count = *ip++;
    *pack_flag = *ip++;
    // *mapping = *ip++;
    sp = (ESMC_Status *)ip;
    *iostatus = *sp++;

    *offset = (char *)sp - (char *)buffer;

    return;
} 


}
