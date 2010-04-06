! $Id: ESMF_ArrayHa.F90,v 1.2 2010/04/06 05:58:32 theurich Exp $
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
#define ESMF_FILENAME "ESMF_ArrayHa.F90"
!==============================================================================
!
! ESMF Array Module
module ESMF_ArrayHaMod
!
!==============================================================================
!
! This file contains the Fortran wrapper code for the C++ implementation of
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
  public ESMF_ArrayHalo
  public ESMF_ArrayHaloRelease
  public ESMF_ArrayHaloStore
  public ESMF_ArrayPrint
  public ESMF_ArrayRedist
  public ESMF_ArrayRedistRelease
  public ESMF_ArrayRedistStore

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_ArrayHa.F90,v 1.2 2010/04/06 05:58:32 theurich Exp $'

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


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHalo()"
!BOP
! !IROUTINE: ESMF_ArrayHalo - Execute an Array halo operation
!
! !INTERFACE:
  subroutine ESMF_ArrayHalo(array, routehandle, commflag, &
    finishedflag, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(inout)           :: array
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    type(ESMF_CommFlag),    intent(in),   optional  :: commflag
    logical,                intent(out),  optional  :: finishedflag
    logical,                intent(in),   optional  :: checkflag
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a precomputed Array halo operation for {\tt array}. The {\tt array}
!   argument must be weakly congruent and typekind conform to the Array used
!   during {\tt ESMF\_ArrayHaloStore()}.
!   Congruent Arrays possess matching DistGrids, and the shape of the local
!   array tiles matches between the Arrays for every DE. For weakly congruent
!   Arrays the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Arrays that differ in the number of elements in the left most
!   undistributed dimensions.
!
!   See {\tt ESMF\_ArrayHaloStore()} on how to precompute {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [array]
!     {\tt ESMF\_Array} containing data to be haloed.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[commflag]}]
!     Indicate communication option. Default is {\tt ESMF\_COMM\_BLOCKING},
!     resulting in a blocking operation.
!     See section \ref{opt:commflag} for a complete list of valid settings.
!   \item [{[finishedflag]}]
!     Used in combination with {\tt commflag = ESMF\_COMM\_NBTESTFINISH}.
!     Returned {\tt finishedflag} equal to {\tt .true.} indicates that all
!     operations have finished. A value of {\tt .false.} indicates that there
!     are still unfinished operations that require additional calls with
!     {\tt commflag = ESMF\_COMM\_NBTESTFINISH}, or a final call with
!     {\tt commflag = ESMF\_COMM\_NBWAITFINISH}. For all other {\tt commflag}
!     settings the returned value in {\tt finishedflag} is always {\tt .true.}.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Array pair will be checked for
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
    type(ESMF_CommFlag)     :: opt_commflag ! helper variable
    type(ESMF_Logical)      :: opt_finishedflag! helper variable
    type(ESMF_Logical)      :: opt_checkflag! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
    
    ! Set default flags
    opt_commflag = ESMF_COMM_BLOCKING
    if (present(commflag)) opt_commflag = commflag
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayHalo(array, routehandle, &
      opt_commflag, opt_finishedflag, opt_checkflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! translate back finishedflag
    if (present(finishedflag)) then
      finishedflag = opt_finishedflag
    endif
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayHalo
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloRelease()"
!BOP
! !IROUTINE: ESMF_ArrayHaloRelease - Release resources associated with Array halo operation
!
! !INTERFACE:
  subroutine ESMF_ArrayHaloRelease(routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Release resouces associated with an Array halo operation. 
!   After this call {\tt routehandle} becomes invalid.
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

  end subroutine ESMF_ArrayHaloRelease
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloStore()"
!BOP
! !IROUTINE: ESMF_ArrayHaloStore - Precompute an Array halo operation
!
! !INTERFACE:
    subroutine ESMF_ArrayHaloStore(array, routehandle, halostartregionflag, &
      haloLDepth, haloUDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(inout)                :: array
    type(ESMF_RouteHandle), intent(inout)                :: routehandle
    type(ESMF_HaloStartRegionFlag), intent(in), optional :: halostartregionflag
    integer,                intent(in),         optional :: haloLDepth(:)
    integer,                intent(in),         optional :: haloUDepth(:)
    integer,                intent(out),        optional :: rc
!
! !DESCRIPTION:
!   Store an Array halo operation over the data in {\tt array}. By default,
!   i.e. without specifying {\tt halostartregionflag}, {\tt haloLDepth} and
!   {\tt haloUDepth}, all elements between the exclusive and total region of 
!   the Array will be considered potential destination elements for halo.
!   However, only those elements that correspond to an actual halo source
!   element, will be updated under the halo operation. Elements that have no
!   associated source remain unchanged under halo.
!
!   Specifying {\tt halostartregionflag} allows to change the shape of the 
!   effective halo region from the inside. Setting this flag to
!   {\tt ESMF\_REGION\_COMPUTATIONAL} means that only elements outside 
!   the computational region of the Array are considered for potential
!   destination elements for halo.
!
!   The {\tt haloLDepth} and {\tt haloUDepth} arguments allow to reduce
!   the extent of the effective halo region. Starting at the region specified
!   by {\tt halostartregionflag}, the {\tt haloLDepth} and {\tt haloUDepth}
!   define a halo depth in each direction. Note that the maximum halo region is
!   limited by the total Array region, independent of the actual
!   {\tt haloLDepth} and {\tt haloUDepth} setting. The total Array region is
!   local DE specific, and {\tt haloLDepth} and {\tt haloUDepth} are interpreted
!   as the maximum desired extent where possible.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayHalo()} on any Array that is weakly congruent
!   and typekind conform to {\tt array}.
!   Congruent Arrays possess matching DistGrids, and the shape of the local
!   array tiles matches between the Arrays for every DE. For weakly congruent
!   Arrays the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Arrays that differ in the number of elements in the left most
!   undistributed dimensions.
!  
!   This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [array]
!     {\tt ESMF\_Array} containing data to be haloed.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[halostartregionflag]}]
!     The start of the effective halo region on every DE. The default
!     setting is {\tt ESMF\_REGION\_EXCLUSIVE}, rendering all non-exclusive
!     elements potential halo destination elments.
!     See section \ref{opt:halostartregionflag} for a complete list of
!     valid settings.
!   \item[{[haloLDepth]}] 
!     This vector specifies the lower corner of the effective halo
!     region with respect to the lower corner of {\tt halostartregionflag}.
!     The size of {\tt haloLDepth} must equal the number of distributed Array
!     dimensions.
!   \item[{[haloUDepth]}] 
!     This vector specifies the upper corner of the effective halo
!     region with respect to the upper corner of {\tt halostartregionflag}.
!     The size of {\tt haloUDepth} must equal the number of distributed Array
!     dimensions.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                         :: localrc        ! local return code
    type(ESMF_HaloStartRegionFlag)  :: opt_halostartregionflag ! helper variable
    type(ESMF_InterfaceInt)         :: haloLDepthArg  ! helper variable
    type(ESMF_InterfaceInt)         :: haloUDepthArg  ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Set default flags
    opt_halostartregionflag = ESMF_REGION_EXCLUSIVE
    if (present(halostartregionflag)) opt_halostartregionflag = halostartregionflag

    ! Deal with (optional) array arguments
    haloLDepthArg = ESMF_InterfaceIntCreate(haloLDepth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    haloUDepthArg = ESMF_InterfaceIntCreate(haloUDepth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayHaloStore(array, routehandle, opt_halostartregionflag, &
      haloLDepthArg, haloUDepthArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayHaloStore
!------------------------------------------------------------------------------


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
!   Print internal information of the specified {\tt ESMF\_Array} object. \\
!
!   Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!   On some platforms/compilers there is a potential issue with interleaving
!   Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!   the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!   may be used on unit 6 to get coherent output.  \\

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
  subroutine ESMF_ArrayRedist(srcArray, dstArray, routehandle, commflag, &
    finishedflag, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in),   optional  :: srcArray
    type(ESMF_Array),       intent(inout),optional  :: dstArray
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    type(ESMF_CommFlag),    intent(in),   optional  :: commflag
    logical,                intent(out),  optional  :: finishedflag
    logical,                intent(in),   optional  :: checkflag
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a precomputed Array redistribution from {\tt srcArray} to
!   {\tt dstArray}. Both {\tt srcArray} and {\tt dstArray} must be
!   weakly congruent and typekind conform with the respective Arrays used 
!   during {\tt ESMF\_ArrayRedistStore()}.
!   Congruent Arrays possess matching DistGrids, and the shape of the local
!   array tiles matches between the Arrays for every DE. For weakly congruent
!   Arrays the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Arrays that differ in the number of elements in the left most
!   undistributed dimensions.
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
!   \item [{[commflag]}]
!     Indicate communication option. Default is {\tt ESMF\_COMM\_BLOCKING},
!     resulting in a blocking operation.
!     See section \ref{opt:commflag} for a complete list of valid settings.
!   \item [{[finishedflag]}]
!     Used in combination with {\tt commflag = ESMF\_COMM\_NBTESTFINISH}.
!     Returned {\tt finishedflag} equal to {\tt .true.} indicates that all
!     operations have finished. A value of {\tt .false.} indicates that there
!     are still unfinished operations that require additional calls with
!     {\tt commflag = ESMF\_COMM\_NBTESTFINISH}, or a final call with
!     {\tt commflag = ESMF\_COMM\_NBWAITFINISH}. For all other {\tt commflag}
!     settings the returned value in {\tt finishedflag} is always {\tt .true.}.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Array pair will be checked for
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
    type(ESMF_Array)        :: opt_srcArray ! helper variable
    type(ESMF_Array)        :: opt_dstArray ! helper variable
    type(ESMF_CommFlag)     :: opt_commflag ! helper variable
    type(ESMF_Logical)      :: opt_finishedflag! helper variable
    type(ESMF_Logical)      :: opt_checkflag! helper variable

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
    opt_commflag = ESMF_COMM_BLOCKING
    if (present(commflag)) opt_commflag = commflag
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
        
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayRedist(opt_srcArray, opt_dstArray, routehandle, &
      opt_commflag, opt_finishedflag, opt_checkflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! translate back finishedflag
    if (present(finishedflag)) then
      finishedflag = opt_finishedflag
    endif
    
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
! \label{ArrayRedistStoreTK}
! {\tt ESMF\_ArrayRedistStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_ArrayRedistStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_ArrayRedistStore()} method, as provided
! through the separate entry points shown in \ref{ArrayRedistStoreTK} and
! \ref{ArrayRedistStoreNF}, is described in the following paragraphs as a whole.
!
! Store an Array redistribution operation from {\tt srcArray} to {\tt dstArray}.
! Interface \ref{ArrayRedistStoreTK} allows PETs to specify a {\tt factor}
! argument. PETs not specifying a {\tt factor} argument call into interface
! \ref{ArrayRedistStoreNF}. If multiple PETs specify the {\tt factor} argument,
! its type and kind, as well as its value must match across all PETs. If none
! of the PETs specify a {\tt factor} argument the default will be a factor of
! 1. The resulting factor is applied to all of the source data during
! redistribution, allowing scaling of the data, e.g. for unit transformation.
!  
! Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized 
! vectors. The sequence is defined by the order of DistGrid dimensions and the
! order of patches within the DistGrid or by user-supplied arbitrary sequence
! indices. See section \ref{Array:SparseMatMul} for details on the definition
! of {\em sequence indices}.
!
! Source Array, destination Array, and the factor may be of different
! <type><kind>. Further, source and destination Arrays may differ in shape,
! however, the number of elements must match. 
!  
! If {\tt srcToDstTransposeMap} is not specified the redistribution corresponds
! to an identity mapping of the sequentialized source Array to the
! sequentialized destination Array. If the {\tt srcToDstTransposeMap}
! argument is provided it must be identical on all PETs. The
! {\tt srcToDstTransposeMap} allows source and destination Array dimensions to
! be transposed during the redistribution. The number of source and destination
! Array dimensions must be equal under this condition and the size of mapped
! dimensions must match.
!  
! It is erroneous to specify the identical Array object for {\tt srcArray} and
! {\tt dstArray} arguments. 
!  
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayRedist()} on any pair of Arrays that are weakly congruent
!   and typekind conform with the {\tt srcArray}, {\tt dstArray} pair. 
!   Congruent Arrays possess matching DistGrids, and the shape of the local
!   array tiles matches between the Arrays for every DE. For weakly congruent
!   Arrays the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Arrays that differ in the number of elements in the left most
!   undistributed dimensions.
!
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [srcArray]
!     {\tt ESMF\_Array} with source data.
!   \item [dstArray]
!     {\tt ESMF\_Array} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [factor]
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
! \label{ArrayRedistStoreNF}
! {\tt ESMF\_ArrayRedistStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_ArrayRedistStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_ArrayRedistStore()} method, as provided
! through the separate entry points shown in \ref{ArrayRedistStoreTK} and
! \ref{ArrayRedistStoreNF}, is described in the following paragraphs as a whole.
!
! Store an Array redistribution operation from {\tt srcArray} to {\tt dstArray}.
! Interface \ref{ArrayRedistStoreTK} allows PETs to specify a {\tt factor}
! argument. PETs not specifying a {\tt factor} argument call into interface
! \ref{ArrayRedistStoreNF}. If multiple PETs specify the {\tt factor} argument,
! its type and kind, as well as its value must match across all PETs. If none
! of the PETs specify a {\tt factor} argument the default will be a factor of
! 1. The resulting factor is applied to all of the source data during
! redistribution, allowing scaling of the data, e.g. for unit transformation.
!  
! Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized 
! vectors. The sequence is defined by the order of DistGrid dimensions and the
! order of patches within the DistGrid or by user-supplied arbitrary sequence
! indices. See section \ref{Array:SparseMatMul} for details on the definition
! of {\em sequence indices}.
!
! Source Array, destination Array, and the factor may be of different
! <type><kind>. Further, source and destination Arrays may differ in shape,
! however, the number of elements must match. 
!  
! If {\tt srcToDstTransposeMap} is not specified the redistribution corresponds
! to an identity mapping of the sequentialized source Array to the
! sequentialized destination Array. If the {\tt srcToDstTransposeMap}
! argument is provided it must be identical on all PETs. The
! {\tt srcToDstTransposeMap} allows source and destination Array dimensions to
! be transposed during the redistribution. The number of source and destination
! Array dimensions must be equal under this condition and the size of mapped
! dimensions must match.
!  
! It is erroneous to specify the identical Array object for {\tt srcArray} and
! {\tt dstArray} arguments. 
!  
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayRedist()} on any pair of Arrays that are weakly congruent
!   and typekind conform with the {\tt srcArray}, {\tt dstArray} pair.
!   Congruent Arrays possess matching DistGrids, and the shape of the local
!   array tiles matches between the Arrays for every DE. For weakly congruent
!   Arrays the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Arrays that differ in the number of elements in the left most
!   undistributed dimensions.
!   \newline
!  
! This call is {\em collective} across the current VM.  
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


end module ESMF_ArrayHaMod
