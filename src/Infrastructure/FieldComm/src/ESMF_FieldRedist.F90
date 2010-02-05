! $Id: ESMF_FieldRedist.F90,v 1.14.2.1 2010/02/05 19:56:49 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_FieldRedist.F90"
!
!   ESMF Field Communications Redist module
module ESMF_FieldRedistMod
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
! !MODULE: ESMF_FieldRedistMod - FieldRedist routines for Field objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldRedist} subroutine.
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
    use ESMF_ArrayMod
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
    public ESMF_FieldRedistStore
    public ESMF_FieldRedist
    public ESMF_FieldRedistRelease
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id: ESMF_FieldRedist.F90,v 1.14.2.1 2010/02/05 19:56:49 svasquez Exp $'

!------------------------------------------------------------------------------
    interface ESMF_FieldRedistStore
        module procedure ESMF_FieldRedistStoreI4
        module procedure ESMF_FieldRedistStoreI8
        module procedure ESMF_FieldRedistStoreR4
        module procedure ESMF_FieldRedistStoreR8
        module procedure ESMF_FieldRedistStoreNF
    end interface
!------------------------------------------------------------------------------
contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRedist()"
!BOP
! !IROUTINE: ESMF_FieldRedist - Execute an Field redistribution
!
! !INTERFACE:
  subroutine ESMF_FieldRedist(srcField, dstField, routehandle, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_Field),       intent(inout),optional  :: srcField
        type(ESMF_Field),       intent(inout),optional  :: dstField
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        logical,                intent(in),   optional  :: checkflag
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a precomputed Field redistribution from {\tt srcField} to
!   {\tt dstField}. Both {\tt srcField} and {\tt dstField} must be
!   congruent and typekind conform with the respective Fields used during 
!   {\tt ESMF\_FieldRedistStore()}. Congruent Fields possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the Fields for every DE.
!
!   It is erroneous to specify the identical Field object for {\tt srcField} and
!   {\tt dstField} arguments.
!
!   See {\tt ESMF\_FieldRedistStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   For examples and associated documentations using this method see Section  
!   \ref{sec:field:usage:redist_1dptr}. 
!
!   \begin{description}
!   \item [{[srcField]}]
!     {\tt ESMF\_Field} with source data.
!   \item [{[dstField]}]
!     {\tt ESMF\_Field} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Field pair will be checked for
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
        type(ESMF_Array)        :: l_srcArray ! helper variable
        type(ESMF_Array)        :: l_dstArray ! helper variable

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional Field args
        ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc)
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc)

        if (present(srcField)) then
          call ESMF_FieldGet(srcField, array=l_srcArray, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call ESMF_ArraySetThisNull(l_srcArray, localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (present(dstField)) then
          call ESMF_FieldGet(dstField, array=l_dstArray, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call ESMF_ArraySetThisNull(l_dstArray, localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        
        ! perform Field redist through internal array
        call ESMF_ArrayRedist(l_srcArray, l_dstArray, routehandle, &
          checkflag, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRedist

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRedistRelease()"
!BOP
! !IROUTINE: ESMF_FieldRedistRelease - Release resources associated with Field redistribution
!
! !INTERFACE:
  subroutine ESMF_FieldRedistRelease(routehandle, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Release resouces associated with an Field redistribution. After this call
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
        ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
            
        ! Call into the RouteHandle code
        call ESMF_RouteHandleRelease(routehandle, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldRedistRelease

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldRedistStore - Precompute Field redistribution with local factor argument 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldRedistStore() 
! subroutine ESMF_FieldRedistStore<type><kind>(srcField, dstField, & 
!        routehandle, factor, srcToDstTransposeMap, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_Field),         intent(inout)         :: srcField  
!   type(ESMF_Field),         intent(inout)         :: dstField  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!   <type>(ESMF_KIND_<kind>), intent(in)            :: factor 
!   integer,                  intent(in),  optional :: srcToDstTransposeMap(:) 
!   integer,                  intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! \label{FieldRedistStoreTK}
! {\tt ESMF\_FieldRedistStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_FieldRedistStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_FieldRedistStore()} method, as provided
! through the separate entry points shown in \ref{FieldRedistStoreTK} and
! \ref{FieldRedistStoreNF}, is described in the following paragraphs as a whole.
!
! Store a Field redistribution operation from {\tt srcField} to {\tt dstField}.
! Interface \ref{FieldRedistStoreTK} allows PETs to specify a {\tt factor}
! argument. PETs not specifying a {\tt factor} argument call into interface
! \ref{FieldRedistStoreNF}. If multiple PETs specify the {\tt factor} argument,
! its type and kind, as well as its value must match across all PETs. If none
! of the PETs specify a {\tt factor} argument the default will be a factor of
! 1. The resulting factor is applied to all of the source data during
! redistribution, allowing scaling of the data, e.g. for unit transformation.
!  
! Both {\tt srcField} and {\tt dstField} are interpreted as sequentialized 
! vectors. The sequence is defined by the order of DistGrid dimensions and the
! order of patches within the DistGrid or by user-supplied arbitrary sequence
! indices. See section \ref{Array:SparseMatMul} for details on the definition
! of {\em sequence indices}.
!
! Source Field, destination Field, and the factor may be of different
! <type><kind>. Further, source and destination Fields may differ in shape,
! however, the number of elements must match. 
!  
! If {\tt srcToDstTransposeMap} is not specified the redistribution corresponds
! to an identity mapping of the sequentialized source Field to the
! sequentialized destination Field. If the {\tt srcToDstTransposeMap}
! argument is provided it must be identical on all PETs. The
! {\tt srcToDstTransposeMap} allows source and destination Field dimensions to
! be transposed during the redistribution. The number of source and destination
! Field dimensions must be equal under this condition and the size of mapped
! dimensions must match.
!  
! It is erroneous to specify the identical Field object for {\tt srcField} and
! {\tt dstField} arguments. 
!  
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldRedist()} on any pair of Fields that are congruent and
! typekind conform with the {\tt srcField}, {\tt dstField} pair. Congruent
! Fields possess matching DistGrids and the shape of the local Array tiles
! matches between the Fields for every DE.
!
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is {\em collective} across the current VM.  
! 
! For examples and associated documentations using this method see Section  
! \ref{sec:field:usage:redist_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcField]  
!   {\tt ESMF\_Field} with source data. 
! \item [dstField] 
!   {\tt ESMF\_Field} with destination data. 
! \item [routehandle] 
!   Handle to the precomputed Route. 
! \item [factor]
!   Factor by which to multiply data. Default is 1. See full method
!   description above for details on the interplay with other PETs.
! \item [{[srcToDstTransposeMap]}] 
!   List with as many entries as there are dimensions in {\tt srcField}. Each
!   entry maps the corresponding {\tt srcField} dimension against the specified
!   {\tt dstField} dimension. Mixing of distributed and undistributed
!   dimensions is supported.
! \item [{[rc]}]  
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldRedistStoreI4" 
!BOPI
! !IROUTINE: ESMF_FieldRedistStore - Precompute Field redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldRedistStore()
    subroutine ESMF_FieldRedistStoreI4(srcField, dstField, & 
        routehandle, factor, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I4),  intent(in)            :: factor
        integer,                intent(in), optional  :: srcToDstTransposeMap(:) 
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArrayRedist to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! perform redist through array 
        ! For performance consideration: 
        ! Rely on ArrayRedist to perform sanity checking of the other parameters 
        call ESMF_ArrayRedistStore(srcArray, dstArray, routehandle, factor, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldRedistStoreI4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldRedistStoreI8" 
!BOPI
! !IROUTINE: ESMF_FieldRedistStore - Precompute Field redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldRedistStore()
    subroutine ESMF_FieldRedistStoreI8(srcField, dstField, & 
        routehandle, factor, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I8),  intent(in)            :: factor
        integer,                intent(in), optional  :: srcToDstTransposeMap(:) 
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArrayRedist to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! perform redist through array 
        ! For performance consideration: 
        ! Rely on ArrayRedist to perform sanity checking of the other parameters 
        call ESMF_ArrayRedistStore(srcArray, dstArray, routehandle, factor, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldRedistStoreI8
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldRedistStoreR4"
!BOPI
! !IROUTINE: ESMF_FieldRedistStore - Precompute Field redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldRedistStore()
    subroutine ESMF_FieldRedistStoreR4(srcField, dstField, & 
        routehandle, factor, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R4),     intent(in)            :: factor
        integer,                intent(in), optional  :: srcToDstTransposeMap(:) 
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArrayRedist to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! perform redist through array 
        ! For performance consideration: 
        ! Rely on ArrayRedist to perform sanity checking of the other parameters 
        call ESMF_ArrayRedistStore(srcArray, dstArray, routehandle, factor, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldRedistStoreR4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldRedistStoreR8"
!BOPI
! !IROUTINE: ESMF_FieldRedistStore - Precompute Field redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldRedistStore()
    subroutine ESMF_FieldRedistStoreR8(srcField, dstField, & 
        routehandle, factor, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R8),     intent(in)            :: factor
        integer,                intent(in), optional  :: srcToDstTransposeMap(:) 
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArrayRedist to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! perform redist through array 
        ! For performance consideration: 
        ! Rely on ArrayRedist to perform sanity checking of the other parameters 
        call ESMF_ArrayRedistStore(srcArray, dstArray, routehandle, factor, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldRedistStoreR8

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldRedistStore - Precompute Field redistribution without local factor argument 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldRedistStore() 
! subroutine ESMF_FieldRedistStoreNF(srcField, dstField, & 
!        routehandle, srcToDstTransposeMap, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_Field),         intent(inout)         :: srcField  
!   type(ESMF_Field),         intent(inout)         :: dstField  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!   integer,                  intent(in),  optional :: srcToDstTransposeMap(:) 
!   integer,                  intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! 
! \label{FieldRedistStoreNF}
! {\tt ESMF\_FieldRedistStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_FieldRedistStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_FieldRedistStore()} method, as provided
! through the separate entry points shown in \ref{FieldRedistStoreTK} and
! \ref{FieldRedistStoreNF}, is described in the following paragraphs as a whole.
!
! Store a Field redistribution operation from {\tt srcField} to {\tt dstField}.
! Interface \ref{FieldRedistStoreTK} allows PETs to specify a {\tt factor}
! argument. PETs not specifying a {\tt factor} argument call into interface
! \ref{FieldRedistStoreNF}. If multiple PETs specify the {\tt factor} argument,
! its type and kind, as well as its value must match across all PETs. If none
! of the PETs specify a {\tt factor} argument the default will be a factor of
! 1. The resulting factor is applied to all of the source data during
! redistribution, allowing scaling of the data, e.g. for unit transformation.
!  
! Both {\tt srcField} and {\tt dstField} are interpreted as sequentialized 
! vectors. The sequence is defined by the order of DistGrid dimensions and the
! order of patches within the DistGrid or by user-supplied arbitrary sequence
! indices. See section \ref{Array:SparseMatMul} for details on the definition
! of {\em sequence indices}.
!
! Source Field, destination Field, and the factor may be of different
! <type><kind>. Further, source and destination Fields may differ in shape,
! however, the number of elements must match. 
!  
! If {\tt srcToDstTransposeMap} is not specified the redistribution corresponds
! to an identity mapping of the sequentialized source Field to the
! sequentialized destination Field. If the {\tt srcToDstTransposeMap}
! argument is provided it must be identical on all PETs. The
! {\tt srcToDstTransposeMap} allows source and destination Field dimensions to
! be transposed during the redistribution. The number of source and destination
! Field dimensions must be equal under this condition and the size of mapped
! dimensions must match.
!  
! It is erroneous to specify the identical Field object for {\tt srcField} and
! {\tt dstField} arguments. 
!  
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldRedist()} on any pair of Fields that are congruent and
! typekind conform with the {\tt srcField}, {\tt dstField} pair. Congruent
! Fields possess matching DistGrids and the shape of the local Array tiles
! matches between the Fields for every DE.
! \newline
!  
! This call is {\em collective} across the current VM.  
! 
! For examples and associated documentations using this method see Section  
! \ref{sec:field:usage:redist_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcField]  
!   {\tt ESMF\_Field} with source data. 
! \item [dstField] 
!   {\tt ESMF\_Field} with destination data. 
! \item [routehandle] 
!   Handle to the precomputed Route. 
! \item [{[srcToDstTransposeMap]}] 
!   List with as many entries as there are dimensions in {\tt srcField}. Each
!   entry maps the corresponding {\tt srcField} dimension against the specified
!   {\tt dstField} dimension. Mixing of distributed and undistributed
!   dimensions is supported.
! \item [{[rc]}]  
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldRedistStoreNF" 
!BOPI
! !IROUTINE: ESMF_FieldRedistStore - Precompute Field redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldRedistStore()
    subroutine ESMF_FieldRedistStoreNF(srcField, dstField, & 
        routehandle, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer,                intent(in), optional  :: srcToDstTransposeMap(:) 
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                     :: localrc 
        type(ESMF_Array)                            :: srcArray, dstArray   

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variable: focus on field and farray 
        ! rely on ArrayRedist to check the sanity of other variables 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, srcField, rc) 
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, dstField, rc) 

        ! Retrieve source and destination arrays. 
        call ESMF_FieldGet(srcField, array=srcArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 
        call ESMF_FieldGet(dstField, array=dstArray, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        ! perform redist through array 
        ! For performance consideration: 
        ! Rely on ArrayRedist to perform sanity checking of the other parameters 
        call ESMF_ArrayRedistStore(srcArray, dstArray, routehandle, & 
            srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldRedistStoreNF
end module
