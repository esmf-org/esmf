! $Id: ESMF_FieldBundleHalo.F90,v 1.12 2011/02/23 13:18:35 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_FieldBundleHalo.F90"
!
!   ESMF FieldBundle Communications Halo module
module ESMF_FieldBundleHaloMod
!
!==============================================================================
!
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldBundleHaloMod - FieldBundleHalo routines for FieldBundle objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldBundleHalo} subroutine.
!
!EOPI
!------------------------------------------------------------------------------
! !USES:
    use ESMF_UtilTypesMod
    use ESMF_InitMacrosMod
    use ESMF_LogErrMod
    use ESMF_VMMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod
    use ESMF_FieldBundleMod
    use ESMF_RHandleMod
    use ESMF_ArrayMod
    use ESMF_ArrayBundleMod

    implicit none
    private

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!  <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!  <none>
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
    public ESMF_FieldBundleHaloStore
    public ESMF_FieldBundleHalo
    public ESMF_FieldBundleHaloRelease
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id: ESMF_FieldBundleHalo.F90,v 1.12 2011/02/23 13:18:35 feiliu Exp $'

!------------------------------------------------------------------------------
contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHalo()"
!BOP
! !IROUTINE: ESMF_FieldBundleHalo - Execute a FieldBundle halo operation
!
! !INTERFACE:
  subroutine ESMF_FieldBundleHalo(fieldbundle, routehandle, keywordEnforcer, &
    checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(inout)           :: fieldbundle
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        type(ESMF_KeywordEnforcer),           optional  :: keywordEnforcer ! must use keywords below
        logical,                intent(in),   optional  :: checkflag
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed FieldBundle halo operation for the Fields in fieldbundle.
!   See {\tt ESMF\_FieldBundleStore()} on how to compute routehandle.
!   \end{sloppypar}
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} with source data. The data in this 
!       FieldBundle may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input FieldBundle pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code
        
        ! local variables to buffer optional arguments
        logical                 :: l_checkflag! helper variable
        type(ESMF_Field)        :: l_field ! helper variable

        ! local internal variables
        integer                 :: fcount, i

        type(ESMF_ArrayBundle)  :: arrayBundle
        type(ESMF_Array), allocatable :: arrays(:)

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional FieldBundle args
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

        ! Set default flags
        l_checkflag = ESMF_FALSE
        if (present(checkflag)) l_checkflag = checkflag

        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

        fcount = fieldbundle%btypep%field_count

        ! build arrayBundle on-the-fly
        allocate(arrays(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(fieldbundle, i, l_field, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_field, array=arrays(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        arrayBundle = ESMF_ArrayBundleCreate(arrays, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(arrays)

        call ESMF_ArrayBundleHalo(arrayBundle, routehandle, &
            checkflag=l_checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! garbage collection
        call ESMF_ArrayBundleDestroy(arrayBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleHalo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHaloRelease()"
!BOP
! !IROUTINE: ESMF_FieldBundleHaloRelease - Release resources associated with a FieldBundle 
! halo operation
!
! !INTERFACE:
  subroutine ESMF_FieldBundleHaloRelease(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Release resouces associated with a FieldBundle halo operation. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
            
        ! Call into the RouteHandle code
        call ESMF_RouteHandleRelease(routehandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleHaloRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHaloStore()"
!BOP
! !IROUTINE: ESMF_FieldBundleHaloStore - Precompute a FieldBundle halo operation
!
! !INTERFACE:
    subroutine ESMF_FieldBundleHaloStore(fieldbundle, routehandle, &
      keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)           :: fieldbundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),   optional :: rc
!
! !DESCRIPTION:
!   Store a FieldBundle halo operation over the data in {\tt fieldbundle}. By 
!   definition, all elements in the total Field regions that lie
!   outside the exclusive regions will be considered potential destination
!   elements for halo. However, only those elements that have a corresponding
!   halo source element, i.e. an exclusive element on one of the DEs, will be
!   updated under the halo operation. Elements that have no associated source
!   remain unchanged under halo.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleHalo()} on any FieldBundle that is weakly congruent
!   and typekind conform to {\tt fieldbundle}. Congruency for FieldBundles is
!   given by the congruency of its constituents.
!   Congruent Fields possess matching DistGrids, and the shape of the local
!   array tiles matches between the Fields for every DE. For weakly congruent
!   Fields the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Fields that differ in the number of elements in the left most
!   undistributed dimensions.
!  
!   This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} containing data to be haloed. The data in this 
!       FieldBundle may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: l_field
        type(ESMF_ArrayBundle)                        :: arrayBundle
        type(ESMF_Array), allocatable                 :: arrays(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc) 

        ! build arrayBundle on-the-fly
        fcount = fieldbundle%btypep%field_count
        allocate(arrays(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(fieldbundle, i, l_field, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_field, array=arrays(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        arrayBundle = ESMF_ArrayBundleCreate(arrays, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(arrays)

        call ESMF_ArrayBundleHaloStore(arrayBundle, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! garbage collection
        call ESMF_ArrayBundleDestroy(arrayBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleHaloStore
!------------------------------------------------------------------------------ 

end module
