! $Id: ESMF_ArrayPr.F90,v 1.1.2.4 2009/01/21 21:25:19 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_ArrayPr.F90"
!==============================================================================
!
! ESMF Array Module
module ESMF_ArrayPrMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the Array class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_ArrayMod
!

!   Fortran API wrapper of C++ implemenation of Array
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_LocalArrayMod
  use ESMF_ArraySpecMod
  use ESMF_VMMod
  use ESMF_DELayoutMod
  use ESMF_DistGridMod
  use ESMF_RHandleMod
  use ESMF_F90InterfaceMod  ! ESMF Fortran-C++ interface helper
  
  ! class sub modules
  use ESMF_ArrayCreateMod   ! contains the ESMF_Array derived type definition
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ArrayPrint
  public ESMF_ArrayRedist
  public ESMF_ArrayRedistRelease
  public ESMF_ArrayRedistStore

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_ArrayPr.F90,v 1.1.2.4 2009/01/21 21:25:19 cdeluca Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayRedistStore

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayRedistStoreI4
    module procedure ESMF_ArrayRedistStoreI8
    module procedure ESMF_ArrayRedistStoreR4
    module procedure ESMF_ArrayRedistStoreR8
    module procedure ESMF_ArrayRedistStoreNF
!EOPI

  end interface

      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayPrint()"
!BOP
! !IROUTINE: ESMF_ArrayPrint - Print Array internals

! !INTERFACE:
  subroutine ESMF_ArrayPrint(array, options, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)              :: array
    character(len=*), intent(in),   optional  :: options
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!   Print internal information of the specified {\tt ESMF\_Array} object.
!
!   Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!   On some platforms/compilers there is a potential issue with interleaving
!   Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!   the expected order.  If this occurs, it is recommended to use the
!   standard Fortran call {\tt flush(6)} as a workaround until this issue
!   is fixed in a future release. 

!   The arguments are:
!   \begin{description}
!   \item[array] 
!     {\tt ESMF\_Array} object.
!   \item[{[options]}] 
!     Print options are not yet supported.
!   \item[{[rc]}] 
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
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ArrayPrint(array, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ArrayPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedist()"
!BOP
! !IROUTINE: ESMF_ArrayRedist - Execute an Array redistribution
!
! !INTERFACE:
  subroutine ESMF_ArrayRedist(srcArray, dstArray, routehandle, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in),   optional  :: srcArray
    type(ESMF_Array),       intent(inout),optional  :: dstArray
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    type(ESMF_Logical),     intent(in),   optional  :: checkflag
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a precomputed Array redistribution from {\tt srcArray} to
!   {\tt dstArray}. Both {\tt srcArray} and {\tt dstArray} must be
!   congruent and typekind conform with the respective Arrays used during 
!   {\tt ESMF\_ArrayRedistStore()}. Congruent Arrays possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the Arrays for every DE.
!
!   It is erroneous to specify the identical Array object for {\tt srcArray} and
!   {\tt dstArray} arguments.
!
!   See {\tt ESMF\_ArrayRedistStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [{[srcArray]}]
!     {\tt ESMF\_Array} with source data.
!   \item [{[dstArray]}]
!     {\tt ESMF\_Array} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[checkflag]}]
!     If set to {\tt ESMF\_TRUE} the input Array pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt ESMF\_FALSE} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt ESMF\_FALSE} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Logical)      :: opt_checkflag! helper variable
    type(ESMF_Array)        :: opt_srcArray ! helper variable
    type(ESMF_Array)        :: opt_dstArray ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments, deal with optional Array args
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
    if (present(srcArray)) then
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
      opt_srcArray = srcArray
    else
      call ESMF_ArraySetThisNull(opt_srcArray, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(dstArray)) then
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
      opt_dstArray = dstArray
    else
      call ESMF_ArraySetThisNull(opt_dstArray, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Set default flags
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
        
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedist(opt_srcArray, opt_dstArray, routehandle, &
      opt_checkflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedist
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistRelease()"
!BOP
! !IROUTINE: ESMF_ArrayRedistRelease - Release resources associated with Array redistribution
!
! !INTERFACE:
  subroutine ESMF_ArrayRedistRelease(routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Release resouces associated with an Array redistribution. After this call
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

    ! Check init status of arguments, deal with optional Array args
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
        
    ! Call into the RouteHandle code
    call ESMF_RouteHandleRelease(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistRelease
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution with local factor argument
!
! !INTERFACE:
! ! Private name; call using ESMF_ArrayRedistStore()
! subroutine ESMF_ArrayRedistStore<type><kind>(srcArray, dstArray, routehandle, &
!   factor, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
!   type(ESMF_Array),           intent(in)              :: srcArray
!   type(ESMF_Array),           intent(inout)           :: dstArray
!   type(ESMF_RouteHandle),     intent(inout)           :: routehandle
!   <type>(ESMF_KIND_<kind>),   intent(in)              :: factor
!   integer,                    intent(in),   optional  :: srcToDstTransposeMap(:)
!   integer,                    intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Store an Array redistribution operation from {\tt srcArray} to
!   {\tt dstArray}. PETs that specify a {\tt factor} argument must use the
!   <type><kind> overloaded interface. Other PETs call into the interface
!   without {\tt factor} argument. If multiple PETs specify the {\tt factor}
!   argument its type and kind as well as its value must match across all
!   PETs. If none of the PETs specifies a {\tt factor} argument the default
!   will be a factor of 1.
!
!   Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized
!   vectors. The sequence is defined by the order of DistGrid dimensions and 
!   the order of patches within the DistGrid or by user-supplied arbitrary
!   sequence indices. See section \ref{Array:SparseMatMul} for details on the
!   definition of {\em sequence indices}. Redistribution corresponds to an
!   identity mapping of the source Array vector to the destination Array vector.
!
!   Source and destination Arrays may be of different <type><kind>. Further
!   source and destination Arrays may differ in shape, however, the number
!   of elements must match.
!
!   It is erroneous to specify the identical Array object for {\tt srcArray} and
!   {\tt dstArray} arguments.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayRedist()} on any pair of Arrays that are congruent
!   and typekind conform with the {\tt srcArray}, {\tt dstArray} pair. 
!   Congruent Arrays possess matching DistGrids and the shape of the local
!   array tiles matches between the Arrays for every DE.\newline
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [srcArray]
!     {\tt ESMF\_Array} with source data.
!   \item [dstArray]
!     {\tt ESMF\_Array} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[factor]}]
!     Factor by which to multipy source data. Default is 1.
!   \item [{[srcToDstTransposeMap]}]
!     List with as many entries as there are dimensions in {\tt srcArray}. Each
!     entry maps the corresponding {\tt srcArray} dimension against the 
!     specified {\tt dstArray} dimension. Mixing of distributed and
!     undistributed dimensions is supported.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreI4()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreI4(srcArray, dstArray, routehandle, &
    factor, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer(ESMF_KIND_I4),      intent(in)              :: factor
    integer,                    intent(in),   optional  :: srcToDstTransposeMap(:)
    integer,                    intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I4, factor, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreI8()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreI8(srcArray, dstArray, routehandle, &
    factor, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer(ESMF_KIND_I8),      intent(in)              :: factor
    integer,                    intent(in),   optional  :: srcToDstTransposeMap(:)
    integer,                    intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I8, factor, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreI8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreR4()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreR4(srcArray, dstArray, routehandle, &
    factor, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R4),         intent(in)              :: factor
    integer,                    intent(in),   optional  :: srcToDstTransposeMap(:)
    integer,                    intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_R4, factor, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStoreR8()"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreR8(srcArray, dstArray, routehandle, &
    factor, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R8),         intent(in)              :: factor
    integer,                    intent(in),   optional  :: srcToDstTransposeMap(:)
    integer,                    intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStore(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_R8, factor, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStore()"
!BOP
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution without local factor argument
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayRedistStore()
  subroutine ESMF_ArrayRedistStoreNF(srcArray, dstArray, routehandle, &
    srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer,                    intent(in),   optional  :: srcToDstTransposeMap(:)
    integer,                    intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Store an Array redistribution operation from {\tt srcArray} to
!   {\tt dstArray}. PETs that specify a {\tt factor} argument must use the
!   <type><kind> overloaded interface. Other PETs call into the interface
!   without {\tt factor} argument. If multiple PETs specify the {\tt factor}
!   argument its type and kind as well as its value must match across all
!   PETs. If none of the PETs specifies a {\tt factor} argument the default
!   will be a factor of 1.
!
!   Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized
!   vectors. The sequence is defined by the order of DistGrid dimensions and 
!   the order of patches within the DistGrid or by user-supplied arbitrary
!   sequence indices. See section \ref{Array:SparseMatMul} for details on the
!   definition of {\em sequence indices}. Redistribution corresponds to an
!   identity mapping of the source Array vector to the destination Array vector.
!
!   Source and destination Arrays may be of different <type><kind>. Further
!   source and destination Arrays may differ in shape, however, the number
!   of elements must match.
!
!   It is erroneous to specify the identical Array object for {\tt srcArray} and
!   {\tt dstArray} arguments.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayRedist()} on any pair of Arrays that are congruent
!   and typekind conform with the {\tt srcArray}, {\tt dstArray} pair. 
!   Congruent Arrays possess matching DistGrids and the shape of the local
!   array tiles matches between the Arrays for every DE.\newline
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [srcArray]
!     {\tt ESMF\_Array} with source data.
!   \item [dstArray]
!     {\tt ESMF\_Array} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[srcToDstTransposeMap]}]
!     List with as many entries as there are dimensions in {\tt srcArray}. Each
!     entry maps the corresponding {\tt srcArray} dimension against the 
!     specified {\tt dstArray} dimension. Mixing of distributed and
!     undistributed dimensions is supported.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedistStoreNF(srcArray, dstArray, routehandle, &
      srcToDstTransposeMapArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayRedistStoreNF
!------------------------------------------------------------------------------


end module ESMF_ArrayPrMod
