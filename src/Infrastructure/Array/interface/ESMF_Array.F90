! $Id: ESMF_Array.F90,v 1.104.2.1 2010/02/05 19:52:20 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_Array.F90"
!==============================================================================
!
! ESMF Array Module
module ESMF_ArrayMod
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
  use ESMF_ArrayGatherMod
  use ESMF_ArrayGetMod
  use ESMF_ArrayPrMod
  use ESMF_ArrayScatterMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Array                 ! implemented in ESMF_ArrayCreateMod 
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ArrayCreate           ! implemented in ESMF_ArrayCreateMod 
  public ESMF_ArrayDestroy          ! implemented in ESMF_ArrayCreateMod 
  public ESMF_ArrayGather           ! implemented in ESMF_ArrayGatherMod 
  public ESMF_ArrayGet              ! implemented in ESMF_ArrayGetMod 
  public ESMF_ArrayHalo
  public ESMF_ArrayHaloStore
  public ESMF_ArrayHaloRun
  public ESMF_ArrayPrint            ! implemented in ESMF_ArrayPrMod
  public ESMF_ArrayRedist           ! implemented in ESMF_ArrayPrMod
  public ESMF_ArrayRedistRelease    ! implemented in ESMF_ArrayPrMod
  public ESMF_ArrayRedistStore      ! implemented in ESMF_ArrayPrMod
  public ESMF_ArrayReduce
  public ESMF_ArrayScatter          ! implemented in ESMF_ArrayScatterMod 
  public ESMF_ArraySet
  public ESMF_ArraySMM
  public ESMF_ArraySMMRelease
  public ESMF_ArraySMMStore
#ifdef FIRSTNEWARRAYPROTOTYPE
  public ESMF_ArrayWait
#endif
  public ESMF_ArrayValidate
  
! - ESMF-internal methods:
  public ESMF_ArrayGetInit          ! implemented in ESMF_ArrayCreateMod
  public ESMF_ArraySetInitCreated   ! implemented in ESMF_ArrayCreateMod
  public ESMF_ArrayGetThis          ! implemented in ESMF_ArrayCreateMod
  public ESMF_ArraySetThis          ! implemented in ESMF_ArrayCreateMod
  public ESMF_ArraySetThisNull      ! implemented in ESMF_ArrayCreateMod
  public ESMF_ArrayCopyThis         ! implemented in ESMF_ArrayCreateMod


!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Array.F90,v 1.104.2.1 2010/02/05 19:52:20 svasquez Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArraySet -- Generic interface

! !INTERFACE:
  interface ESMF_ArraySet

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArraySet
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArraySet} functions.   
!EOPI 
  end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayHalo -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayHalo

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayHalo
!EOPI

  end interface

      
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArraySMMStore -- Generic interface

! !INTERFACE:
  interface ESMF_ArraySMMStore

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArraySMMStoreI4
    module procedure ESMF_ArraySMMStoreI8
    module procedure ESMF_ArraySMMStoreR4
    module procedure ESMF_ArraySMMStoreR8
    module procedure ESMF_ArraySMMStoreNF
!EOPI

  end interface

      
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayReduce -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayReduce

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayReduce
    module procedure ESMF_ArrayReduceFarray

#ifdef FIRSTNEWARRAYPROTOTYPE

    module procedure ESMF_ArrayReduceScalarBR8       !1st prototype
    module procedure ESMF_ArrayReduceScalarNBRootR8  !1st prototype
    module procedure ESMF_ArrayReduceScalarNBR8      !1st prototype
#endif
! todo: need to write vector version where the user can specify which
!       dimensions of narray are supposed to be reduced. output is vector
!       good news is that the vector version does not have to be type/kind
!       overloaded because of the result being a LocalArray!

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayReduce} functions.   
!EOPI 
  end interface



#ifdef FIRSTNEWARRAYPROTOTYPE
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayWait -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayWait

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayWaitRoot
    module procedure ESMF_ArrayWaitDE

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayWait} functions.   
!EOPI 
  end interface
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!==================== communication calls ===========================



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHalo()"
!BOPI
! !IROUTINE: ESMF_ArrayHalo - Halo an Array
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayHalo()
    subroutine ESMF_ArrayHalo(array, regionflag, haloLDepth, haloUDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(inout)          :: array
    type(ESMF_RegionFlag),  intent(in),   optional :: regionflag
    integer,                intent(in),   optional :: haloLDepth(:)
    integer,                intent(in),   optional :: haloUDepth(:)
    integer,                intent(out),  optional :: rc
!
! !DESCRIPTION:
!   Perform a halo operation over the data in an {\tt ESMF\_Array} object.
!
!   The optional {\tt haloLDepth} and {\tt haloUDepth} arguments can be 
!   provided to specified the exact shape of the halo region. By default 
!   {\tt haloLDepth} and {\tt haloUDepth} are assumed relative to the 
!   computational region of the Array object. The optional {\tt regionflag}
!   may be used to change to the exclusive region as reference for the halo
!   widths.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [array]
!         {\tt ESMF\_Array} containing data to be haloed.
!   \item [{[regionflag]}]
!         Specifies the reference for halo width arguments: 
!         {\tt ESMF\_REGION\_EXCLUSIVE} or {\tt ESMF\_REGION\_COMPUTATIONAL}
!         (default).
!   \item[{[haloLDepth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the total data
!      region with respect to the lower corner of the computational region
!      or exclusive region (depending on {\tt regionflag}.
!   \item[{[haloUDepth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the total data
!      region with respect to the upper corner of the computational region
!      or exclusive region (depending on {\tt regionflag}.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
  end subroutine ESMF_ArrayHalo
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloStore()"
!BOPI
! !IROUTINE: ESMF_ArrayHaloStore - Store an ArrayHalo operation
!
! !INTERFACE:
    subroutine ESMF_ArrayHaloStore(array, regionflag, haloLDepth, &
      haloUDepth, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(inout)          :: array
    type(ESMF_RegionFlag),  intent(in),   optional :: regionflag
    integer,                intent(in),   optional :: haloLDepth(:)
    integer,                intent(in),   optional :: haloUDepth(:)
    type(ESMF_RouteHandle), intent(inout)          :: routehandle
    integer,                intent(out),  optional :: rc
!
! !DESCRIPTION:
!   Store a halo operation over the data in an {\tt ESMF\_Array}. See the
!   description for {\tt ArrayHalo()} for details. No actual halo operation
!   is performed by this call, use {\tt ArrayHaloRun} to execute a stored
!   halo operation.
!
!   The Route referenced by the returned {\tt ESMF\_RouteHandle} object can 
!   be used with any {\tt ESMF\_Array} object that is {\em DistGrid conform}, 
!   i.e. has been defined on a congruent DistGrid object. In particular it can
!   be used for all Arrays in an ArrayBundle that are DistGrid conform with the
!   Array used to precompute the Route.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [array]
!         {\tt ESMF\_Array} containing data to be haloed.
!   \item [{[regionflag]}]
!         Specifies the reference for halo width arguments: 
!         {\tt ESMF\_REGION\_EXCLUSIVE} or {\tt ESMF\_REGION\_COMPUTATIONAL}
!         (default).
!   \item[{[haloLDepth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the total data
!      region with respect to the lower corner of the computational region
!      or exclusive region (depending on {\tt regionflag}.
!   \item[{[haloUDepth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the total data
!      region with respect to the upper corner of the computational region
!      or exclusive region (depending on {\tt regionflag}.
!   \item [routehandle]
!         Handle to the Route storing the precomputed halo operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

  end subroutine ESMF_ArrayHaloStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloRun()"
!BOPI
! !IROUTINE: ESMF_ArrayHaloRun - Execute an ArrayHalo operation
!
! !INTERFACE:
    subroutine ESMF_ArrayHaloRun(array, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(inout)         :: array
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Execute the halo operation stored in the Route referenced by 
!   {\tt routehandle} over the data in {\tt array}. See the description for 
!   {\tt ArrayHaloStore()} and {\tt ArrayHalo()} for details. 
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [array]
!         {\tt ESMF\_Array} containing data to be haloed.
!   \item [routehandle]
!         Handle to the Route that stores the halo operation to be performed.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

  end subroutine ESMF_ArrayHaloRun
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayReduce()"
!BOPI
! !IROUTINE: ESMF_ArrayReduce

! !INTERFACE:
  subroutine ESMF_ArrayReduce(array, result, reduceflag, rootPET, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(inout)           :: array
    real(ESMF_KIND_R8),         intent(out),  optional  :: result
    type(ESMF_ReduceFlag),      intent(in)              :: reduceflag
    integer,                    intent(in)              :: rootPET
    type(ESMF_VM),              intent(in),   optional  :: vm
    integer,                    intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Fully reduce the entire Array into a single {\tt result} on {\tt rootPET}
!     according to the operation specified in {\tt reduceflag}. Only root must
!     specify a valid result argument.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object across which data will be scattered.
!     \item[{[result ]}]
!        Argument into which to reduce the Array. Only root
!        must provide a valid {\tt result} argument.
!     \item[reduceflag] 
!        Reduction operation. See section \ref{opt:reduceflag} for a list of 
!        valid reduce operations. There will be options that determine the 
!        sequence of operations to ensure bit-wise reproducibility.
!     \item[rootPET]
!          root.
!     \item[{[vm]}]
!        Optional {\tt ESMF\_VM} object of the current context. Providing the
!        VM of the current context will lower the method's overhead.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    result = real(0.,ESMF_KIND_R8)  ! quiet down compiler warnings while not fully implemented
    
  end subroutine ESMF_ArrayReduce
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayReduce()"
!BOPI
! !IROUTINE: ESMF_ArrayReduce

! !INTERFACE:
  ! Private name; call using ESMF_ArrayReduce()
  subroutine ESMF_ArrayReduceFarray(array, farray, reduceflag, rootPET, &
    dimList, patch, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(inout)           :: array
    real(ESMF_KIND_R8), target, intent(out),  optional  :: farray(:,:)
    type(ESMF_ReduceFlag),      intent(in)              :: reduceflag
    integer,                    intent(in)              :: rootPET
    integer,                    intent(in)              :: dimList(:)
    integer,                    intent(in),   optional  :: patch
    type(ESMF_VM),              intent(in),   optional  :: vm
    integer,                    intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Reduce the dimensions specified in {\tt dimList} of the Array object 
!     into {\tt farray} on {\tt rootPET} according to the operation specified 
!     in {\tt reduceflag}. Only root must provide a valid {\tt farray} argument.
!     
!     This partial reduction operation is patch specific, i.e. only a single
!     DistGrid patch of the Array will be reduced. The patch can be selected
!     by the optional {\tt patch} argument. The shape of the provided 
!     {\tt farray} argument must match that of the Array patch reduced by the
!     dimensions specified in {\tt dimList}.
!      
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object across which data will be scattered.
!     \item[{[farray]}]
!        Fortran array into which to reduce the Array. Only root
!        must provide a valid {\tt farray} argument.
!     \item[reduceflag] 
!        Reduction operation. See section \ref{opt:reduceflag} for a list of 
!        valid reduce operations. There will be options that determine the 
!        sequence of operations to ensure bit-wise reproducibility.
!     \item[rootPET]
!          root.
!     \item[dimList]
!        List of Array dimensions to be reduced.
!     \item[{[patch]}]
!        The DistGrid patch in {\tt array} to reduce into {\tt farray}.
!        By default patch 1 of {\tt farray} will be reduced.
!     \item[{[vm]}]
!        Optional {\tt ESMF\_VM} object of the current context. Providing the
!        VM of the current context will lower the method's overhead.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
  end subroutine ESMF_ArrayReduceFarray
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySet()"
!BOP
! !IROUTINE: ESMF_ArraySet - Set Array properties
!
! !INTERFACE:
  subroutine ESMF_ArraySet(array, name, computationalLWidth, &
    computationalUWidth, rc)

!
! !ARGUMENTS:
    type(ESMF_Array),   intent(inout)           :: array
    character(len = *), intent(in),   optional  :: name
    integer,            intent(in),   optional  :: computationalLWidth(:,:)
    integer,            intent(in),   optional  :: computationalUWidth(:,:)
    integer,            intent(out),  optional  :: rc

!
! !DESCRIPTION:
!     Sets adjustable settings in an {\tt ESMF\_Array} object. Arrays with
!     tensor dimensions will set values for {\em all} tensor components.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!       {\tt ESMF\_Array} object for which to set properties.
!     \item [{[name]}]
!       The Array name.
!     \item[{[computationalLWidth]}] 
!       This argument must have of size {\tt (dimCount, localDeCount)}.
!       {\tt computationalLWidth} specifies the lower corner of the
!       computational region with respect to the lower corner of the exclusive
!       region for all local DEs.
!     \item[{[computationalUWidth]}] 
!       This argument must have of size {\tt (dimCount, localDeCount)}.
!       {\tt computationalUWidth} specifies the upper corner of the
!       computational region with respect to the upper corner of the exclusive
!       region for all local DEs.
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt)       :: computationalLWidthArg ! helper variable
    type(ESMF_InterfaceInt)       :: computationalUWidthArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Set the name in Base object
    if (present(name)) then
      call c_ESMC_SetName(array, "Array", name, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Deal with (optional) array arguments
    computationalLWidthArg = &
      ESMF_InterfaceIntCreate(farray2D=computationalLWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUWidthArg = &
      ESMF_InterfaceIntCreate(farray2D=computationalUWidth, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySet(array, computationalLWidthArg, &
      computationalUWidthArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(computationalLWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUWidthArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySMM()"
!BOP
! !IROUTINE: ESMF_ArraySMM - Execute an Array sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_ArraySMM(srcArray, dstArray, routehandle, zeroflag, &
    checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in),   optional  :: srcArray
    type(ESMF_Array),       intent(inout),optional  :: dstArray
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    type(ESMF_RegionFlag),  intent(in),   optional  :: zeroflag
    logical,                intent(in),   optional  :: checkflag
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a precomputed Array sparse matrix multiplication from {\tt srcArray}
!   to {\tt dstArray}. Both {\tt srcArray} and {\tt dstArray} must be
!   congruent and typekind conform with the respective Arrays used during 
!   {\tt ESMF\_ArraySMMStore()}. Congruent Arrays possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the Arrays for every DE.
!
!   It is erroneous to specify the identical Array object for {\tt srcArray} and
!   {\tt dstArray} arguments.
!
!   See {\tt ESMF\_ArraySMMStore()} on how to precompute 
!   {\tt routehandle}. See section \ref{Array:SparseMatMul} for details on the
!   operation {\tt ESMF\_ArraySMM()} performs.
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
!   \item [{[zeroflag]}]
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of
!     all DEs in {\tt dstArray} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstArray} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroflag} to 
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination Array that will be updated by the sparse matrix
!     multiplication. See section \ref{opt:regionflag} for a complete list of
!     valid settings.
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
    type(ESMF_RegionFlag)   :: opt_zeroflag ! helper variable
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
    opt_zeroflag = ESMF_REGION_TOTAL
    if (present(zeroflag)) opt_zeroflag = zeroflag
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
        
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySMM(opt_srcArray, opt_dstArray, routehandle, &
      opt_zeroflag, opt_checkflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySMM
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySMMRelease()"
!BOP
! !IROUTINE: ESMF_ArraySMMRelease - Release resources associated with Array sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_ArraySMMRelease(routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Release resouces associated with an Array sparse matrix multiplication. 
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

  end subroutine ESMF_ArraySMMRelease
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ArraySMMStore - Precompute Array sparse matrix multiplication with local factors
!
! !INTERFACE:
! ! Private name; call using ESMF_ArraySMMStore()
! subroutine ESMF_ArraySMMStore<type><kind>(srcArray, dstArray, &
!   routehandle, factorList, factorIndexList, rc)
!
! !ARGUMENTS:
!   type(ESMF_Array),           intent(in)              :: srcArray
!   type(ESMF_Array),           intent(inout)           :: dstArray
!   type(ESMF_RouteHandle),     intent(inout)           :: routehandle
!   <type>(ESMF_KIND_<kind>), target, intent(in)        :: factorList(:)
!   integer,                    intent(in)              :: factorIndexList(:,:)
!   integer,                    intent(out),  optional  :: rc
!
! !DESCRIPTION:
! \label{ArraySMMStoreTK}
! {\tt ESMF\_ArraySMMStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_ArraySMMStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_ArraySMMStore()} method, as provided
! through the separate entry points shown in \ref{ArraySMMStoreTK} and
! \ref{ArraySMMStoreNF}, is described in the following paragraphs as a whole.
!
!   Store an Array sparse matrix multiplication operation from {\tt srcArray}
!   to {\tt dstArray}. PETs that specify non-zero matrix coefficients must use
!   the <type><kind> overloaded interface and provide the {\tt factorList} and
!   {\tt factorIndexList} arguments. Providing {\tt factorList} and
!   {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
!   {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
!   PET does not provide matrix elements. Alternatively, PETs that do not 
!   provide matrix elements may also call into the overloaded interface
!   {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
!
!   Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized
!   vectors. The sequence is defined by the order of DistGrid dimensions and 
!   the order of patches within the DistGrid or by user-supplied arbitrary
!   sequence indices. See section \ref{Array:SparseMatMul} for details on the
!   definition of {\em sequence indices}.
!
!   Source and destination Arrays, as well as the supplied {\tt factorList}
!   argument, may be of different <type><kind>. Further source and
!   destination Arrays may differ in shape and number of elements.
!
!   It is erroneous to specify the identical Array object for {\tt srcArray} and
!   {\tt dstArray} arguments.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArraySMM()} on any pair of Arrays that are congruent
!   and typekind conform with the {\tt srcArray}, {\tt dstArray} pair. 
!   Congruent Arrays possess matching DistGrids and the shape of the local
!   array tiles matches between the Arrays for every DE.
!
!   This method is overloaded for:\newline
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!   \newline
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
!   \item [factorList]
!     List of non-zero coefficients.
!   \item [factorIndexList]
!     Pairs of sequence indices for the factors stored in {\tt factorList}.
!
!     The second dimension of {\tt factorIndexList} steps through the list of
!     pairs, i.e. {\tt size(factorIndexList,2) == size(factorList)}. The first
!     dimension of {\tt factorIndexList} is either of size 2 or size 4.
!
!     In the {\em size 2 format} {\tt factorIndexList(1,:)} specifies the
!     sequence index of the source element in the {\tt srcArray} while
!     {\tt factorIndexList(2,:)} specifies the sequence index of the
!     destination element in {\tt dstArray}. For this format to be a valid
!     option source and destination Arrays must have matching number of
!     tensor elements (the product of the sizes of all Array tensor dimensions).
!     Under this condition an identiy matrix can be applied within the space of
!     tensor elements for each sparse matrix factor.
!
!     The {\em size 4 format} is more general and does not require a matching
!     tensor element count. Here the {\tt factorIndexList(1,:)} specifies the
!     sequence index while {\tt factorIndexList(2,:)} specifies the tensor
!     sequence index of the source element in the {\tt srcArray}. Further
!     {\tt factorIndexList(3,:)} specifies the sequence index and
!     {\tt factorIndexList(4,:)} specifies the tensor sequence index of the 
!     destination element in the {\tt dstArray}.
!
!     See section \ref{Array:SparseMatMul} for details on the definition of 
!     Array {\em sequence indices} and {\em tensor sequence indices}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySMMStoreI4()"
!BOPI
! !IROUTINE: ESMF_ArraySMMStore - Precompute and store an Array sparse matrix multiplication 
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySMMStore()
  subroutine ESMF_ArraySMMStoreI4(srcArray, dstArray, routehandle, factorList, &
    factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer(ESMF_KIND_I4), target, intent(in)           :: factorList(:)
    integer,                    intent(in)              :: factorIndexList(:,:)
    integer,                    intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                         :: localrc            ! local return code
    integer(ESMF_KIND_I4), pointer  :: opt_factorList(:)  ! helper variable
    integer                         :: len_factorList     ! helper variable
    type(ESMF_InterfaceInt)         :: factorIndexListArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Wrap factor arguments
    len_factorList = size(factorList)
    opt_factorList => factorList
    factorIndexListArg = &
      ESMF_InterfaceIntCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySMMStore(srcArray, dstArray, routehandle, &
      ESMF_TYPEKIND_I4, opt_factorList, len_factorList, factorIndexListArg, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySMMStoreI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySMMStoreI8()"
!BOPI
! !IROUTINE: ESMF_ArraySMMStore - Precompute and store an Array sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySMMStore()
  subroutine ESMF_ArraySMMStoreI8(srcArray, dstArray, routehandle, factorList, &
    factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer(ESMF_KIND_I8), target, intent(in)           :: factorList(:)
    integer,                    intent(in)              :: factorIndexList(:,:)
    integer,                    intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                         :: localrc            ! local return code
    integer(ESMF_KIND_I8), pointer  :: opt_factorList(:)  ! helper variable
    integer                         :: len_factorList     ! helper variable
    type(ESMF_InterfaceInt)         :: factorIndexListArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Wrap factor arguments
    len_factorList = size(factorList)
    opt_factorList => factorList
    factorIndexListArg = &
      ESMF_InterfaceIntCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySMMStore(srcArray, dstArray, routehandle, &
      ESMF_TYPEKIND_I8, opt_factorList, len_factorList, factorIndexListArg, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySMMStoreI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySMMStoreR4()"
!BOPI
! !IROUTINE: ESMF_ArraySMMStore - Precompute and store an Array sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySMMStore()
  subroutine ESMF_ArraySMMStoreR4(srcArray, dstArray, routehandle, factorList, &
    factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R4), target, intent(in)              :: factorList(:)
    integer,                    intent(in)              :: factorIndexList(:,:)
    integer,                    intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                         :: localrc            ! local return code
    real(ESMF_KIND_R4), pointer     :: opt_factorList(:)  ! helper variable
    integer                         :: len_factorList     ! helper variable
    type(ESMF_InterfaceInt)         :: factorIndexListArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Wrap factor arguments
    len_factorList = size(factorList)
    opt_factorList => factorList
    factorIndexListArg = &
      ESMF_InterfaceIntCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySMMStore(srcArray, dstArray, routehandle, &
      ESMF_TYPEKIND_R4, opt_factorList, len_factorList, factorIndexListArg, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySMMStoreR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySMMStoreR8()"
!BOPI
! !IROUTINE: ESMF_ArraySMMStore - Precompute and store an Array sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySMMStore()
  subroutine ESMF_ArraySMMStoreR8(srcArray, dstArray, routehandle, factorList, &
    factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R8), target, intent(in)              :: factorList(:)
    integer,                    intent(in)              :: factorIndexList(:,:)
    integer,                    intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                         :: localrc            ! local return code
    real(ESMF_KIND_R8), pointer     :: opt_factorList(:)  ! helper variable
    integer                         :: len_factorList     ! helper variable
    type(ESMF_InterfaceInt)         :: factorIndexListArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Wrap factor arguments
    len_factorList = size(factorList)
    opt_factorList => factorList
    factorIndexListArg = &
      ESMF_InterfaceIntCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySMMStore(srcArray, dstArray, routehandle, &
      ESMF_TYPEKIND_R8, opt_factorList, len_factorList, factorIndexListArg, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySMMStoreR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySMMStoreNF()"
!BOP
! !IROUTINE: ESMF_ArraySMMStore - Precompute Array sparse matrix multiplication without local factors
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySMMStore()
  subroutine ESMF_ArraySMMStoreNF(srcArray, dstArray, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer,                    intent(out),  optional  :: rc
!
! !DESCRIPTION:
! \label{ArraySMMStoreNF}
! {\tt ESMF\_ArraySMMStore()} is a collective method across all PETs of the
! current Component. The interface of the method is overloaded, allowing 
! -- in principle -- each PET to call into {\tt ESMF\_ArraySMMStore()}
! through a different entry point. Restrictions apply as to which combinations
! are sensible. All other combinations result in ESMF run time errors. The
! complete semantics of the {\tt ESMF\_ArraySMMStore()} method, as provided
! through the separate entry points shown in \ref{ArraySMMStoreTK} and
! \ref{ArraySMMStoreNF}, is described in the following paragraphs as a whole.
!
!   Store an Array sparse matrix multiplication operation from {\tt srcArray}
!   to {\tt dstArray}. PETs that specify non-zero matrix coefficients must use
!   the <type><kind> overloaded interface and provide the {\tt factorList} and
!   {\tt factorIndexList} arguments. Providing {\tt factorList} and
!   {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
!   {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
!   PET does not provide matrix elements. Alternatively, PETs that do not 
!   provide matrix elements may also call into the overloaded interface
!   {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
!
!   Both {\tt srcArray} and {\tt dstArray} are interpreted as sequentialized
!   vectors. The sequence is defined by the order of DistGrid dimensions and 
!   the order of patches within the DistGrid or by user-supplied arbitrary
!   sequence indices. See section \ref{Array:SparseMatMul} for details on the
!   definition of {\em sequence indices}.
!
!   Source and destination Arrays, as well as the supplied {\tt factorList}
!   argument, may be of different <type><kind>. Further source and
!   destination Arrays may differ in shape and number of elements.
!
!   It is erroneous to specify the identical Array object for {\tt srcArray} and
!   {\tt dstArray} arguments.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArraySMM()} on any pair of Arrays that are congruent
!   and typekind conform with the {\tt srcArray}, {\tt dstArray} pair. 
!   Congruent Arrays possess matching DistGrids and the shape of the local
!   array tiles matches between the Arrays for every DE.
!   \newline
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
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySMMStoreNF(srcArray, dstArray, routehandle, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySMMStoreNF
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayValidate()"
!BOP
! !IROUTINE: ESMF_ArrayValidate - Validate Array internals

! !INTERFACE:
  subroutine ESMF_ArrayValidate(array, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)              :: array
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt Array} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          Specified {\tt ESMF\_Array} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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
    call c_ESMC_ArrayValidate(array, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArrayValidate
!------------------------------------------------------------------------------



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!! old-style newArray calls of 1st prototype calls !!!!!!!!!!!!!!!!!


#ifdef FIRSTNEWARRAYPROTOTYPE


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayReduceScalarBR8()"
!BOPI
! !IROUTINE: ESMF_ArrayReduce - Reduce a Array to a single R8 scalar

! !INTERFACE:
  ! Private name; call using ESMF_ArrayReduce()
  subroutine ESMF_ArrayReduceScalarBR8(array, result, reduceflag, &
    reduceflagDummy, rootPET, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),    intent(in)              :: array
    real(ESMF_KIND_R8),     intent(out)             :: result
    type(ESMF_ReduceFlag),  intent(in)              :: reduceflag
    type(ESMF_ReduceFlag),  intent(in)              :: reduceflagDummy !prevent conflict
    integer,                intent(in)              :: rootPET
    type(ESMF_VM),          intent(in),   optional  :: vm
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Reduce {\tt array} to a singe R8 scalar {\tt result}. The reduction
!     operation is specified in {\tt reduceFlag}.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object that will be reduced.
!     \item[result]
!        Upon return this will hold the result of the reduction operation. Only
!        {\tt rootPET} must provide a valid {\tt result} argument.
!        {\tt result} arguments on other PETs will be used to check the
!        data type and kind but are otherwise ignored.
!   \item[reduceflag] 
!        Reduction operation. See section \ref{opt:reduceflag} for a list of 
!        valid reduce operations.
!     \item[rootPET]
!        PET on which result will be returned.
!     \item[{[vm]}]
!        Optional {\tt ESMF\_VM} object of the current context. Providing the
!        VM of the current context will lower the method's overhead.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayReduceScalarB(array, result, ESMF_TYPEKIND_R8, reduceflag, &
!      rootPET, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayReduceScalarBR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayReduceScalarNBRootR8()"
!BOPI
! !IROUTINE: ESMF_ArrayReduce - Reduce a Array to a single R8 scalar

! !INTERFACE:
  ! Private name; call using ESMF_ArrayReduce()
  subroutine ESMF_ArrayReduceScalarNBRootR8(array, result, reduceflag, &
    rootPET, commhandle, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),    intent(in)              :: array
    real(ESMF_KIND_R8),     intent(out)             :: result
    type(ESMF_ReduceFlag),  intent(in)              :: reduceflag
    integer,                intent(in)              :: rootPET
    type(ESMF_CommHandle),  intent(inout)           :: commhandle
    type(ESMF_VM),          intent(in),   optional  :: vm
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Reduce {\tt array} to a singe R8 scalar {\tt result}. The reduction
!     operation is specified in {\tt reduceFlag}.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object that will be reduced.
!     \item[result]
!        Upon return (after wait returned!!!) this will hold the result of the reduction operation. Only
!        {\tt rootPET} must provide a valid {\tt result} argument.
!        {\tt result} arguments on other PETs will be used to check the
!        data type and kind but are otherwise ignored.
!     \item[reduceflag] 
!        Reduction operation. See section \ref{opt:reduceflag} for a list of 
!        valid reduce operations.
!     \item[rootPET]
!        PET on which result will be returned.
!     \item[commhandle]
!          Upon return {\tt commhandle} on {\tt rootPET} holds the 
!          {\tt ESMF\_CommHandle} associated with the non-blocking scatter 
!          operation.
!     \item[{[vm]}]
!        Optional {\tt ESMF\_VM} object of the current context. Providing the
!        VM of the current context will lower the method's overhead.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayReduceScalarNBRoot(array, result, ESMF_TYPEKIND_R8, reduceflag, &
!      rootPET, commhandle, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayReduceScalarNBRootR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayReduceScalarNBR8()"
!BOPI
! !IROUTINE: ESMF_ArrayReduce - Reduce a Array to a single R8 scalar

! !INTERFACE:
  ! Private name; call using ESMF_ArrayReduce()
  subroutine ESMF_ArrayReduceScalarNBR8(array, result, reduceflag, rootPET, &
    de, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),    intent(in)              :: array
    real(ESMF_KIND_R8),     intent(out)             :: result
    type(ESMF_ReduceFlag),  intent(in)              :: reduceflag
    integer,                intent(in)              :: rootPET
    integer,                intent(in)              :: de
    type(ESMF_VM),          intent(in),   optional  :: vm
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Reduce {\tt array} to a singe R8 scalar {\tt result}. The reduction
!     operation is specified in {\tt reduceFlag}.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object that will be reduced.
!     \item[result]
!        Upon return this will hold the result of the reduction operation. Only
!        {\tt rootPET} must provide a valid {\tt result} argument.
!        {\tt result} arguments on other PETs will be used to check the
!        data type and kind but are otherwise ignored.
!     \item[reduceflag] 
!        Reduction operation. See section \ref{opt:reduceflag} for a list of 
!        valid reduce operations.
!     \item[rootPET]
!        PET on which result will be returned.
!     \item[de]
!          DE for which this call is issued.
!     \item[{[vm]}]
!        Optional {\tt ESMF\_VM} object of the current context. Providing the
!        VM of the current context will lower the method's overhead.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayReduceScalarNB(array, result, ESMF_TYPEKIND_R8, reduceflag, &
!      rootPET, de, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayReduceScalarNBR8
!------------------------------------------------------------------------------








! ---- Wait methods ---------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayWaitRoot()"
!BOPI
! !IROUTINE: ESMF_ArrayWait - Wait for non-blocking Array communication

! !INTERFACE:
  ! Private name; call using ESMF_ArrayWait()
  subroutine ESMF_ArrayWaitRoot(array, rootPET, commhandle, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(inout)           :: array
    integer,                intent(in)              :: rootPET
    type(ESMF_CommHandle),  intent(inout)           :: commhandle
    type(ESMF_VM),          intent(in),   optional  :: vm
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Wait for non-blocking communication associated with {\tt commhandle}
!     to finish making all data objects valid and accessible. This call is
!     only to be issued from the PET which was {\tt rootPET} for the respecitve
!     communication call.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object across which data will be scattered.
!     \item[rootPET]
!          PET that was the root during the respective communication call.
!     \item[commhandle]
!          Wait for the data object on rootPET associated with commhandle to
!          become available.
!     \item[{[vm]}]
!        Optional {\tt ESMF\_VM} object of the current context. Providing the
!        VM of the current context will lower the method's overhead.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayWaitRoot(array, rootPET, commhandle, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayWaitRoot
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayWaitDE()"
!BOPI
! !IROUTINE: ESMF_ArrayWait - Wait for non-blocking Array communication

! !INTERFACE:
  ! Private name; call using ESMF_ArrayWait()
  subroutine ESMF_ArrayWaitDE(array, de, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(inout)           :: array
    integer,                intent(in)              :: de
    type(ESMF_VM),          intent(in),   optional  :: vm
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Wait for non-blocking communication to finish for the specific DE,
!     making available the associated data in {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object across which data will be scattered.
!     \item[de]
!          DE for which this call is issued. 
!     \item[{[vm]}]
!        Optional {\tt ESMF\_VM} object of the current context. Providing the
!        VM of the current context will lower the method's overhead.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayWaitDE(array, de, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayWaitDE
!------------------------------------------------------------------------------


#endif

end module ESMF_ArrayMod
