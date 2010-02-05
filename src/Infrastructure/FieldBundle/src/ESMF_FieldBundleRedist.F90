! $Id: ESMF_FieldBundleRedist.F90,v 1.9.2.1 2010/02/05 19:56:23 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_FieldBundleRedist.F90"
!
!   ESMF FieldBundle Communications Redist module
module ESMF_FieldBundleRedistMod
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
! !MODULE: ESMF_FieldBundleRedistMod - FieldBundleRedist routines for FieldBundle objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldBundleRedist} subroutine.
!
!EOPI
!------------------------------------------------------------------------------
! !USES:
    use ESMF_UtilTypesMod
    use ESMF_InitMacrosMod
    use ESMF_LogErrMod
    use ESMF_VMMod
    use ESMF_FieldMod
    use ESMF_FieldRedistMod
    use ESMF_FieldBundleMod
    use ESMF_RHandleMod
    use ESMF_ArrayBundleMod
    implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!  <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!  <none>
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
    public ESMF_FieldBundleRedistStore
    public ESMF_FieldBundleRedist
    public ESMF_FieldBundleRedistRelease
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id: ESMF_FieldBundleRedist.F90,v 1.9.2.1 2010/02/05 19:56:23 svasquez Exp $'

!------------------------------------------------------------------------------
    interface ESMF_FieldBundleRedistStore
        module procedure ESMF_FieldBundleRedistStoreI4
        module procedure ESMF_FieldBundleRedistStoreI8
        module procedure ESMF_FieldBundleRedistStoreR4
        module procedure ESMF_FieldBundleRedistStoreR8
        module procedure ESMF_FieldBundleRedistStoreNF
    end interface
!------------------------------------------------------------------------------
contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRedist()"
!BOP
! !IROUTINE: ESMF_FieldBundleRedist - Execute an FieldBundle redistribution
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRedist(srcFieldBundle, dstFieldBundle, routehandle, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(in),   optional  :: srcFieldBundle
        type(ESMF_FieldBundle), intent(inout),optional  :: dstFieldBundle
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        logical,                intent(in),   optional  :: checkflag
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a precomputed FieldBundle redistribution from {\tt srcFieldBundle} to
!   {\tt dstFieldBundle}. Both {\tt srcFieldBundle} and {\tt dstFieldBundle} must be
!   congruent and typekind conform with the respective FieldBundles used during 
!   {\tt ESMF\_FieldBundleRedistStore()}. Congruent FieldBundles possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the FieldBundles for every DE.
!
!   It is erroneous to specify the identical FieldBundle object for {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} arguments.
!
!   See {\tt ESMF\_FieldBundleRedistStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   For examples and associated documentations using this method see Section  
!   \ref{sec:fieldbundle:usage:redist_1dptr}. 
!
!   \begin{description}
!   \item [{[srcFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with source data.
!   \item [{[dstFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with destination data.
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
        type(ESMF_Field)        :: l_srcField ! helper variable
        type(ESMF_Field)        :: l_dstField ! helper variable

        ! local internal variables
        logical                 :: src_bundle = .true.
        logical                 :: dst_bundle = .true.
        integer                 :: fcount, i
        type(ESMF_ArrayBundle)  :: srcab, dstab
        type(ESMF_Array), allocatable :: srca(:), dsta(:)

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional FieldBundle args
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

        ! Set default flags
        l_checkflag = ESMF_FALSE
        if (present(checkflag)) l_checkflag = checkflag

        if (present(srcFieldBundle)) then
            ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc)

            fcount = srcFieldBundle%btypep%field_count

            allocate(srca(fcount))
            do i = 1, fcount
                call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
                if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
                call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
                if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            enddo
            srcab = ESMF_ArrayBundleCreate(srca, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(srca)
        else
            src_bundle = .false.
        endif

        if (present(dstFieldBundle)) then
            ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc)

            fcount = dstFieldBundle%btypep%field_count

            allocate(dsta(fcount))
            do i = 1, fcount
                call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
                if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
                call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
                if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            enddo
            dstab = ESMF_ArrayBundleCreate(dsta, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(dsta)
        else
            dst_bundle = .false.
        endif
        
        ! perform FieldBundle redistribution
        if(src_bundle .and. dst_bundle) &
            call ESMF_ArrayBundleRedist(srcab, dstab, routehandle, &
                checkflag=l_checkflag, rc=localrc)
        if(src_bundle .and. .not. dst_bundle) &
            call ESMF_ArrayBundleRedist(srcArrayBundle=srcab, routehandle=routehandle, &
                checkflag=l_checkflag, rc=localrc)
        if(.not. src_bundle .and. dst_bundle) &
            call ESMF_ArrayBundleRedist(dstArrayBundle=dstab, routehandle=routehandle, &
                checkflag=l_checkflag, rc=localrc)
        if(.not. src_bundle .and. .not. dst_bundle) &
            call ESMF_ArrayBundleRedist(routehandle=routehandle, &
                checkflag=l_checkflag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! garbage collection
        if (present(srcFieldBundle)) then
          call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if (present(dstFieldBundle)) then
          call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleRedist

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRedistRelease()"
!BOP
! !IROUTINE: ESMF_FieldBundleRedistRelease - Release resources associated with FieldBundle 
! redistribution
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRedistRelease(routehandle, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Release resouces associated with an FieldBundle redistribution. After this call
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
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleRedistRelease

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute FieldBundle redistribution 
! with local factor argument 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldBundleRedistStore() 
! subroutine ESMF_FieldBundleRedistStore<type><kind>(srcFieldBundle, dstFieldBundle, & 
!        routehandle, factor, srcToDstTransposeMap, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_FieldBundle),   intent(in)            :: srcFieldBundle  
!   type(ESMF_FieldBundle),   intent(inout)         :: dstFieldBundle  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!   <type>(ESMF_KIND_<kind>), intent(in)            :: factor
!   integer,                  intent(in),  optional :: srcToDstTransposeMap(:)
!   integer,                  intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! 
! Store an FieldBundle redistribution operation from {\tt srcFieldBundle} to {\tt dstFieldBundle}. 
! PETs that
! specify a {\tt factor} argument must use the <type><kind> overloaded interface. Other 
! PETs call into the interface without {\tt factor} argument. If multiple PETs specify 
! the {\tt factor} argument its type and kind as well as its value must match across 
! all PETs. If none of the PETs specifies a {\tt factor} argument the default will be a  
! factor of 1. 
!  
! Both {\tt srcFieldBundle} and {\tt dstFieldBundle} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! patches within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! Redistribution corresponds to an identity mapping of the source FieldBundle vector to 
! the destination FieldBundle vector. 
!  
! Source and destination FieldBundles may be of different <type><kind>. Further source 
! and destination FieldBundles may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical FieldBundle object for srcFieldBundle 
! and dstFieldBundle arguments. 
!  
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldBundleRedist()} on any pair of FieldBundles that are congruent and typekind 
! conform with the srcFieldBundle, dstFieldBundle pair. Congruent FieldBundles possess matching 
! DistGrids and the shape of the local array tiles matches between the FieldBundles for 
! every DE. 
!
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is collective across the current VM.  
! 
! For examples and associated documentations using this method see Section  
! \ref{sec:fieldbundle:usage:redist_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcFieldBundle]  
!       {\tt ESMF\_FieldBundle} with source data. 
! \item [dstFieldBundle] 
!       {\tt ESMF\_FieldBundle} with destination data. 
! \item [routehandle] 
!       Handle to the precomputed Route. 
! \item [factor]
!       FActor by which to multiply source data. Default is 1.
! \item [{[srcToDstTransposeMap]}] 
!       List with as many entries as there are dimensions in {\tt srcFieldBundle}. Each 
! entry maps the corresponding {\tt srcFieldBundle} dimension 
! against the specified {\tt dstFieldBundle} 
! dimension. Mixing of distributed and undistributed dimensions is supported.  
! \item [{[rc]}]  
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreI4" 
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreI4(srcFieldBundle, dstFieldBundle, & 
        routehandle, factor, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I4),  intent(in)            :: factor
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        if(srcFieldBundle%btypep%field_count .ne. dstFieldBundle%btypep%field_count) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
               "src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rc)
            return
        endif 

        ! Retrieve source and destination fields. 
        ! TODO: change loop end if necessary

        fcount = srcFieldBundle%btypep%field_count
        allocate(srca(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(srca, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        fcount = dstFieldBundle%btypep%field_count
        allocate(dsta(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(dsta, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreI4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreI8" 
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreI8(srcFieldBundle, dstFieldBundle, & 
        routehandle, factor, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I8),  intent(in)            :: factor
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        if(srcFieldBundle%btypep%field_count .ne. dstFieldBundle%btypep%field_count) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
               "src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rc)
            return
        endif 

        fcount = srcFieldBundle%btypep%field_count
        allocate(srca(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(srca, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        fcount = dstFieldBundle%btypep%field_count
        allocate(dsta(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(dsta, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreI8
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreR4"
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreR4(srcFieldBundle, dstFieldBundle, & 
        routehandle, factor, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R4),     intent(in)            :: factor
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        if(srcFieldBundle%btypep%field_count .ne. dstFieldBundle%btypep%field_count) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
               "src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rc)
            return
        endif 

        fcount = srcFieldBundle%btypep%field_count
        allocate(srca(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(srca, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        fcount = dstFieldBundle%btypep%field_count
        allocate(dsta(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(dsta, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreR4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreR8"
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreR8(srcFieldBundle, dstFieldBundle, & 
        routehandle, factor, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R8),     intent(in)            :: factor
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        if(srcFieldBundle%btypep%field_count .ne. dstFieldBundle%btypep%field_count) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
               "src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rc)
            return
        endif 

        fcount = srcFieldBundle%btypep%field_count
        allocate(srca(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(srca, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        fcount = dstFieldBundle%btypep%field_count
        allocate(dsta(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(dsta, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreR8

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute FieldBundle redistribution with local factor argument 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldBundleRedistStore() 
! subroutine ESMF_FieldBundleRedistStoreNF(srcFieldBundle, dstFieldBundle, & 
!        routehandle, factor, srcToDstTransposeMap, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_FieldBundle),   intent(in)            :: srcFieldBundle  
!   type(ESMF_FieldBundle),   intent(inout)         :: dstFieldBundle  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!   integer,                  intent(in) , optional :: srcToDstTransposeMap(:)
!   integer,                  intent(out), optional :: rc 
! 
! !DESCRIPTION: 
!
! Store an FieldBundle redistribution operation from {\tt srcFieldBundle}
! to {\tt dstFieldBundle}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
! 
! Both {\tt srcFieldBundle} and {\tt dstFieldBundle} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! patches within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! Redistribution corresponds to an identity mapping of the source FieldBundle vector to 
! the destination FieldBundle vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical FieldBundle object for srcFieldBundle and dstFieldBundle 
! arguments. 
!  
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldBundleRedist()} on any pair of Fields that are congruent and typekind 
! conform with the srcFieldBundle, dstFieldBundle pair. Congruent Fields possess matching 
! DistGrids and the shape of the local array tiles matches between the Fields for 
! every DE. 
!  
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!
! This call is collective across the current VM.  
! 
! For examples and associated documentations using this method see Section  
! \ref{sec:fieldbundle:usage:redist_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcFieldBundle]  
!       {\tt ESMF\_FieldBundle} with source data. 
! \item [dstFieldBundle] 
!       {\tt ESMF\_FieldBundle} with destination data. 
! \item [routehandle] 
!       Handle to the precomputed Route. 
! \item [{[srcToDstTransposeMap]}] 
!       List with as many entries as there are dimensions in {\tt srcFieldBundle}. Each 
! entry maps the corresponding {\tt srcFieldBundle} dimension 
! against the specified {\tt dstFieldBundle} 
! dimension. Mixing of distributed and undistributed dimensions is supported.  
! \item [{[rc]}]  
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreNF" 
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreNF(srcFieldBundle, dstFieldBundle, & 
        routehandle, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        if(srcFieldBundle%btypep%field_count .ne. dstFieldBundle%btypep%field_count) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
               "src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rc)
            return
        endif 

        ! TODO:
        ! internal grids match
        !if(ESMF_GridMatch(srcFieldBundle%btypep%grid, dstFieldBundle%btypep%grid) then
        !    call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
        !       "src and dst FieldBundle must have matching grid", &
        !        ESMF_CONTEXT, rc)
        !    return
        !endif 

        fcount = srcFieldBundle%btypep%field_count
        allocate(srca(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(srca, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        fcount = dstFieldBundle%btypep%field_count
        allocate(dsta(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(dsta, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreNF
end module
