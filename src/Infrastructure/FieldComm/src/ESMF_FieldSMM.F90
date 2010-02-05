! $Id: ESMF_FieldSMM.F90,v 1.7.4.1 2010/02/05 19:56:49 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_FieldSMM.F90"
!
!   ESMF Field Communications SMM module
module ESMF_FieldSMMMod
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
! !MODULE: ESMF_FieldSMMMod - FieldSMM routines for Field objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldSMM} subroutine.
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
    public ESMF_FieldSMMStore
    public ESMF_FieldSMM
    public ESMF_FieldSMMRelease
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id: ESMF_FieldSMM.F90,v 1.7.4.1 2010/02/05 19:56:49 svasquez Exp $'

!------------------------------------------------------------------------------
    interface ESMF_FieldSMMStore
        module procedure ESMF_FieldSMMStoreI4
        module procedure ESMF_FieldSMMStoreI8
        module procedure ESMF_FieldSMMStoreR4
        module procedure ESMF_FieldSMMStoreR8
        module procedure ESMF_FieldSMMStoreNF
    end interface
!------------------------------------------------------------------------------
contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSMM()"
!BOP
! !IROUTINE: ESMF_FieldSMM - Execute an Field sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_FieldSMM(srcField, dstField, routehandle, zeroflag, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_Field),       intent(inout),optional  :: srcField
        type(ESMF_Field),       intent(inout),optional  :: dstField
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        type(ESMF_RegionFlag),  intent(in),   optional  :: zeroflag
        logical,                intent(in),   optional  :: checkflag
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a precomputed Field sparse matrix multiplication from {\tt srcField} to
!   {\tt dstField}. Both {\tt srcField} and {\tt dstField} must be
!   congruent and typekind conform with the respective Fields used during 
!   {\tt ESMF\_FieldSMMStore()}. Congruent Fields possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the Fields for every DE.
!
!   It is erroneous to specify the identical Field object for {\tt srcField} and
!   {\tt dstField} arguments.
!
!   See {\tt ESMF\_FieldSMMStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   For examples and associated documentations using this method see Section  
!   \ref{sec:field:usage:smm_1dptr}. 
!
!   \begin{description}
!   \item [{[srcField]}]
!     {\tt ESMF\_Field} with source data.
!   \item [{[dstField]}]
!     {\tt ESMF\_Field} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[zeroflag]}]
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of
!     all DEs in {\tt dstField} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstField} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroflag} to 
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination Field that will be updated by the sparse matrix
!     multiplication. See section \ref{opt:regionflag} for a complete list of
!     valid settings.
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
        
        ! perform Field sparse matrix multiplication through internal array
        call ESMF_ArraySMM(l_srcArray, l_dstArray, routehandle, &
          zeroflag, checkflag, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldSMM

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldSMMRelease()"
!BOP
! !IROUTINE: ESMF_FieldSMMRelease - Release resources associated with Field 
! sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_FieldSMMRelease(routehandle, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Release resouces associated with an Field sparse matrix multiplication. After this call
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

    end subroutine ESMF_FieldSMMRelease

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication 
! with local factor argument 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldSMMStore() 
! subroutine ESMF_FieldSMMStore<type><kind>(srcField, dstField, & 
!        routehandle, factorList, factorIndexList, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_Field),         intent(inout)         :: srcField  
!   type(ESMF_Field),         intent(inout)         :: dstField  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!   <type>(ESMF_KIND_<kind>), intent(in)            :: factorList(:) 
!   integer,                  intent(in),           :: factorIndexList(:,:) 
!   integer,                  intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! 
! Store an Field sparse matrix multiplication operation from {\tt srcField}
! to {\tt dstField}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
!  
! Both {\tt srcField} and {\tt dstField} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! patches within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! SMM corresponds to an identity mapping of the source Field vector to 
! the destination Field vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical Field object for srcField and dstField 
! arguments. 
!  
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldSMM()} on any pair of Fields that are congruent and typekind 
! conform with the srcField, dstField pair. Congruent Fields possess matching 
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
! \ref{sec:field:usage:smm_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcField]  
!       {\tt ESMF\_Field} with source data. 
! \item [dstField] 
!       {\tt ESMF\_Field} with destination data. 
! \item [routehandle] 
!       Handle to the precomputed Route. 
! \item [factorList]
!       List of non-zero coefficients.
! \item [factorIndexList]
!     Pairs of sequence indices for the factors stored in {\tt factorList}.
!
!     The second dimension of {\tt factorIndexList} steps through the list of
!     pairs, i.e. {\tt size(factorIndexList,2) == size(factorList)}. The first
!     dimension of {\tt factorIndexList} is either of size 2 or size 4.
!
!     In the {\em size 2 format} {\tt factorIndexList(1,:)} specifies the
!     sequence index of the source element in the {\tt srcField} while
!     {\tt factorIndexList(2,:)} specifies the sequence index of the
!     destination element in {\tt dstField}. For this format to be a valid
!     option source and destination Fields must have matching number of
!     tensor elements (the product of the sizes of all Field tensor dimensions).
!     Under this condition an identiy matrix can be applied within the space of
!     tensor elements for each sparse matrix factor.
!
!     The {\em size 4 format} is more general and does not require a matching
!     tensor element count. Here the {\tt factorIndexList(1,:)} specifies the
!     sequence index while {\tt factorIndexList(2,:)} specifies the tensor
!     sequence index of the source element in the {\tt srcField}. Further
!     {\tt factorIndexList(3,:)} specifies the sequence index and
!     {\tt factorIndexList(4,:)} specifies the tensor sequence index of the 
!     destination element in the {\tt dstField}.
!
!     See section \ref{Array:SparseMatMul} for details on the definition of 
!     Field {\em sequence indices} and {\em tensor sequence indices}.
! \item [{[rc]}]  
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreI4" 
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreI4(srcField, dstField, & 
        routehandle, factorList, factorIndexList, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I4),  intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
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
        ! rely on ArraySMM to check the sanity of other variables 
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

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray, dstArray, routehandle, factorList, & 
            factorIndexList, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreI4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreI8" 
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreI8(srcField, dstField, & 
        routehandle, factorList, factorIndexList, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I8),  intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
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
        ! rely on ArraySMM to check the sanity of other variables 
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

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray, dstArray, routehandle, factorList, & 
            factorIndexList, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreI8
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreR4"
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreR4(srcField, dstField, & 
        routehandle, factorList, factorIndexList, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R4),     intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
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
        ! rely on ArraySMM to check the sanity of other variables 
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

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray, dstArray, routehandle, factorList, & 
            factorIndexList, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreR4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreR8"
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreR8(srcField, dstField, & 
        routehandle, factorList, factorIndexList, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R8),     intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
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
        ! rely on ArraySMM to check the sanity of other variables 
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

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray, dstArray, routehandle, factorList, & 
            factorIndexList, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreR8

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication with local factor argument 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldSMMStore() 
! subroutine ESMF_FieldSMMStoreNF(srcField, dstField, & 
!        routehandle, factorList, factorIndexList, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_Field),         intent(inout)         :: srcField  
!   type(ESMF_Field),         intent(inout)         :: dstField  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!   integer,                  intent(out), optional :: rc 
! 
! !DESCRIPTION: 
!
! Store an Field sparse matrix multiplication operation from {\tt srcField}
! to {\tt dstField}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
! 
! Both {\tt srcField} and {\tt dstField} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! patches within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! SMM corresponds to an identity mapping of the source Field vector to 
! the destination Field vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical Field object for srcField and dstField 
! arguments. 
!  
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldSMM()} on any pair of Fields that are congruent and typekind 
! conform with the srcField, dstField pair. Congruent Fields possess matching 
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
! \ref{sec:field:usage:smm_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcField]  
!       {\tt ESMF\_Field} with source data. 
! \item [dstField] 
!       {\tt ESMF\_Field} with destination data. 
! \item [routehandle] 
!       Handle to the precomputed Route. 
! \item [{[rc]}]  
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldSMMStoreNF" 
!BOPI
! !IROUTINE: ESMF_FieldSMMStore - Precompute Field sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldSMMStore()
    subroutine ESMF_FieldSMMStoreNF(srcField, dstField, & 
        routehandle, rc) 

        ! input arguments 
        type(ESMF_Field),       intent(inout)         :: srcField  
        type(ESMF_Field),       intent(inout)         :: dstField  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
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
        ! rely on ArraySMM to check the sanity of other variables 
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

        ! perform sparse matrix multiplication through array 
        ! For performance consideration: 
        ! Rely on ArraySMM to perform sanity checking of the other parameters 
        call ESMF_ArraySMMStore(srcArray, dstArray, routehandle, & 
            rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rc)) return 

        if (present(rc)) rc = ESMF_SUCCESS 
    end subroutine ESMF_FieldSMMStoreNF
end module
