! $Id: ESMF_FieldBundleRedist.F90,v 1.1 2008/05/22 18:22:43 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
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
      '$Id: ESMF_FieldBundleRedist.F90,v 1.1 2008/05/22 18:22:43 feiliu Exp $'

!------------------------------------------------------------------------------
    interface ESMF_FieldBundleRedistStore
        module procedure ESMF_FieldBundleRedistStoreI4
        module procedure ESMF_FieldBundleRedistStoreI8
        module procedure ESMF_FieldBundleRedistStoreR4
        module procedure ESMF_FieldBundleRedistStoreR8
        module procedure ESMF_FieldBundleRedistStoreNF
    end interface ESMF_FieldBundleRedistStore
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
        type(ESMF_FieldBundle), intent(inout),optional  :: srcFieldBundle
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
!   \ref{sec:fieldbundle:usage:smm_1dptr}. 
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

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional FieldBundle args
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

        if (present(srcFieldBundle)) then
            ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc)
        else
            src_bundle = .false.
        endif

        if (present(dstFieldBundle)) then
            ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc)
        else
            dst_bundle = .false.
        endif
        
        ! Set default flags
        l_checkflag = ESMF_FALSE
        if (present(checkflag)) l_checkflag = checkflag

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

        ! perform FieldBundle redistribution
        do i = 1, fcount
            if(src_bundle) then
                call ESMF_FieldBundleGet(srcFieldBundle, i, field=l_srcField, rc=localrc)
                if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(dst_bundle) then
                call ESMF_FieldBundleGet(dstFieldBundle, i, field=l_dstField, rc=localrc)
                if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(src_bundle .and. dst_bundle) &
                call ESMF_FieldRedist(l_srcField, l_dstField, routehandle, &
                    l_checkflag, localrc)
            if(src_bundle .and. .not. dst_bundle) &
                call ESMF_FieldRedist(srcField=l_srcField, routehandle=routehandle, &
                    checkflag=l_checkflag, rc=localrc)
            if(.not. src_bundle .and. dst_bundle) &
                call ESMF_FieldRedist(dstField=l_dstField, routehandle=routehandle, &
                    checkflag=l_checkflag, rc=localrc)
            if(.not. src_bundle .and. .not. dst_bundle) &
                call ESMF_FieldRedist(routehandle=routehandle, &
                    checkflag=l_checkflag, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

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
!   type(ESMF_FieldBundle),   intent(inout)         :: srcFieldBundle  
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
! \ref{sec:fieldbundle:usage:smm_1dptr}. 
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
        type(ESMF_FieldBundle), intent(inout)         :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I4),  intent(in)            :: factor
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: srcField, dstField   
        !integer                                     :: srcEle, dstEle ! n elements
        !integer                                     :: sfdc, dfdc ! dimcount
        !integer, dimension(:), allocatable          :: sftc, dftc ! total count

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        !! src and dst FieldBundle cannot be same object
        !if(srcFieldBundle%ftypep .eq. dstFieldBundle%ftypep) then
        !    call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
        !       "src and dst FieldBundle cannot be same object", &
        !        ESMF_CONTEXT, rc)
        !    return
        !endif

        !!! src and dst FieldBundle must have same number of data elements
        !call ESMF_FieldBundleGet(srcFieldBundle, dimCount=sfdc, rc=localrc)
        !if (ESMF_LogMsgFoundError(localrc, & 
        !    ESMF_ERR_PASSTHRU, & 
        !    ESMF_CONTEXT, rc)) return 

        !call ESMF_FieldBundleGet(dstFieldBundle, dimCount=dfdc, rc=localrc)
        !if (ESMF_LogMsgFoundError(localrc, & 
        !    ESMF_ERR_PASSTHRU, & 
        !    ESMF_CONTEXT, rc)) return 

        !allocate(sftc(sfdc), dftc(dfdc))
        !
        ! TODO: there needs to be a way to loop through all DEs on all PETs to
        ! get the total number of data elements...
        !call ESMF_FieldBundleGet(srcFieldBundle, localDe=0, totalCount=sftc, rc=localrc)
        !if (ESMF_LogMsgFoundError(localrc, & 
        !    ESMF_ERR_PASSTHRU, & 
        !    ESMF_CONTEXT, rc)) return 

        !call ESMF_FieldBundleGet(dstFieldBundle, localDe=0, totalCount=dftc, rc=localrc)
        !if (ESMF_LogMsgFoundError(localrc, & 
        !    ESMF_ERR_PASSTHRU, & 
        !    ESMF_CONTEXT, rc)) return 

        !srcEle = 1
        !do i = 1, sfdc
        !    srcEle = srcEle * sftc(i)
        !enddo

        !dstEle = 1
        !do i = 1, dfdc
        !    dstEle = dstEle * dftc(i)
        !enddo
    
        !deallocate(sftc, dftc)

        !if(srcEle .ne. dstEle) then
        !    call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
        !       "src and dst FieldBundle must have same number of data elements", &
        !        ESMF_CONTEXT, rc)
        !    return
        !endif

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
        fcount = srcFieldBundle%btypep%field_count
        do i = 1, fcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, field=srcField, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 
            call ESMF_FieldBundleGet(dstFieldBundle, i, field=dstField, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 

            ! perform redistribution through array 
            ! For performance consideration: 
            ! Rely on underlying ArrayRedist to perform sanity checking of the other parameters 
            call ESMF_FieldRedistStore(srcField, dstField, routehandle, factor, & 
                srcToDstTransposeMap, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 
        enddo

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
        type(ESMF_FieldBundle), intent(inout)         :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I8),  intent(in)            :: factor
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: srcField, dstField   

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
        fcount = srcFieldBundle%btypep%field_count
        do i = 1, fcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, field=srcField, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 
            call ESMF_FieldBundleGet(dstFieldBundle, i, field=dstField, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 

            ! perform redistribution through array 
            ! For performance consideration: 
            ! Rely on underlying ArrayRedist to perform sanity checking of the other parameters 
            call ESMF_FieldRedistStore(srcField, dstField, routehandle, factor, & 
                srcToDstTransposeMap, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 
        enddo

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
        type(ESMF_FieldBundle), intent(inout)         :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R4),  intent(in)               :: factor
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: srcField, dstField   

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
        fcount = srcFieldBundle%btypep%field_count
        do i = 1, fcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, field=srcField, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 
            call ESMF_FieldBundleGet(dstFieldBundle, i, field=dstField, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 

            ! perform redistribution through array 
            ! For performance consideration: 
            ! Rely on underlying ArrayRedist to perform sanity checking of the other parameters 
            call ESMF_FieldRedistStore(srcField, dstField, routehandle, factor, & 
                srcToDstTransposeMap, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 
        enddo

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
        type(ESMF_FieldBundle), intent(inout)         :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R8),  intent(in)               :: factor
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: srcField, dstField   

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
        fcount = srcFieldBundle%btypep%field_count
        do i = 1, fcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, field=srcField, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 
            call ESMF_FieldBundleGet(dstFieldBundle, i, field=dstField, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 

            ! perform redistribution through array 
            ! For performance consideration: 
            ! Rely on underlying ArrayRedist to perform sanity checking of the other parameters 
            call ESMF_FieldRedistStore(srcField, dstField, routehandle, factor, & 
                srcToDstTransposeMap, rc=localrc) 
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 
        enddo

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
!   type(ESMF_FieldBundle),   intent(inout)         :: srcFieldBundle  
!   type(ESMF_FieldBundle),   intent(inout)         :: dstFieldBundle  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
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
! \ref{sec:fieldbundle:usage:smm_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcFieldBundle]  
!       {\tt ESMF\_FieldBundle} with source data. 
! \item [dstFieldBundle] 
!       {\tt ESMF\_FieldBundle} with destination data. 
! \item [routehandle] 
!       Handle to the precomputed Route. 
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
        routehandle, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(inout)         :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, fcount, i
        type(ESMF_Field)                              :: srcField, dstField

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

        ! perform redistribution
        ! For performance consideration: 
        ! Rely on underlying ArrayRedist to perform sanity checking of the other parameters 

        !TODO:
        ! decide if routehandle(:) or routehandle should be used
        fcount = srcFieldBundle%btypep%field_count
        do i = 1, 1 !fcount
            srcField = srcFieldBundle%btypep%flist(i)
            dstField = dstFieldBundle%btypep%flist(i)
            call ESMF_FieldRedistStore(srcField, dstField, routehandle, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, & 
                ESMF_ERR_PASSTHRU, & 
                ESMF_CONTEXT, rc)) return 
        enddo

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldBundleRedistStoreNF
end module
