! $Id: ESMF_Array.F90,v 1.49 2007/03/31 05:50:47 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
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

!   F90 API wrapper of C++ implemenation of Array
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
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  
  ! class sub modules
  use ESMF_ArrayCreateMod   ! contains the ESMF_Array derived type definition
  use ESMF_ArrayGetMod
  use ESMF_ArrayGatherMod
  use ESMF_ArrayScatterMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_ArrayBundle
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_ArrayBundle
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Array                 ! implemented in ESMF_ArrayCreateMod 
  public ESMF_ArrayBundle
  public ESMF_RegionFlag, ESMF_REGION_EXCLUSIVE, ESMF_REGION_COMPUTATIONAL
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ArrayCreate           ! implemented in ESMF_ArrayCreateMod 
  public ESMF_ArrayDestroy          ! implemented in ESMF_ArrayCreateMod 
  public ESMF_ArrayGet              ! implemented in ESMF_ArrayGetMod 
  public ESMF_ArraySet
  public ESMF_ArrayGather           ! implemented in ESMF_ArrayGatherMod 
  public ESMF_ArrayReduce
  public ESMF_ArrayScatter          ! implemented in ESMF_ArrayScatterMod 
  public ESMF_ArrayHalo
  public ESMF_ArrayHaloStore
  public ESMF_ArrayHaloRun
  public ESMF_ArrayRedist
  public ESMF_ArrayRedistStore
  public ESMF_ArrayRedistRun
  public ESMF_ArraySparseMatMulStore
  public ESMF_ArraySparseMatMul
  public ESMF_ArrayWait
  public ESMF_ArrayPrint
  public ESMF_ArrayValidate
  
  public ESMF_ArrayBundleCreate
  public ESMF_ArrayBundleDestroy
  public ESMF_ArrayBundleGet
  public ESMF_ArrayBundleHalo
  public ESMF_ArrayBundleHaloStore
  public ESMF_ArrayBundleHaloRun
  public ESMF_ArrayBundleRedist
  public ESMF_ArrayBundleRedistStore
  public ESMF_ArrayBundleRedistRun
  public ESMF_ArrayBundleSparseMatMulStr
  public ESMF_ArrayBundleSparseMatMul
  public ESMF_ArrayBundleValidate

! - ESMF-private methods:
  public ESMF_ArrayGetInit            ! implemented in ESMF_ArrayCreateMod 
  public ESMF_ArraySetInitCreated     ! implemented in ESMF_ArrayCreateMod 
  public ESMF_ArrayBundleGetInit


!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Array.F90,v 1.49 2007/03/31 05:50:47 cdeluca Exp $'

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
    module procedure ESMF_ArraySetAllDecompAllDe
    module procedure ESMF_ArraySetTensorAllDe
      
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
! !IROUTINE: ESMF_ArrayReduce -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayReduce

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayReduce
    module procedure ESMF_ArrayReduceFarray

    module procedure ESMF_ArrayReduceScalarBR8       !1st prototype
    module procedure ESMF_ArrayReduceScalarNBRootR8  !1st prototype
    module procedure ESMF_ArrayReduceScalarNBR8      !1st prototype
! todo: need to write vector version where the user can specify which
!       dimensions of narray are supposed to be reduced. output is vector
!       good news is that the vector version does not have to be type/kind
!       overloaded because of the result being a LocalArray!

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayReduce} functions.   
!EOPI 
  end interface
      

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


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySetAllDecompAllDe()"
!BOPI
! !IROUTINE: ESMF_ArraySetAllDecompAllDe - Set Array internals

! !INTERFACE:
  ! Private name; call using ESMF_ArraySet()
  subroutine ESMF_ArraySetAllDecompAllDe(array, staggerLoc, vectorDim, &
    computationalLWidth, computationalUWidth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)              :: array
    integer,          intent(in),   optional  :: staggerLoc
    integer,          intent(in),   optional  :: vectorDim
    integer,          intent(in),   optional  :: computationalLWidth(:)
    integer,          intent(in),   optional  :: computationalUWidth(:)
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Set internal information.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        {\tt ESMF\_Array} object for which to set properties.
!     \item[staggerLoc]
!       Stagger location of this Array.
!     \item[vectorDim]
!       Dimension along this vector component is aligned.
!     \item[{[computationalLWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
!     \item[{[computationalUWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the computational
!      region with respect to the upper corner of the exclusive region.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Use LogErr to handle return code
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArraySetAllDecompAllDe
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySetTensorAllDe()"
!BOPI
! !IROUTINE: ESMF_ArraySetTensorAllDe - Set Array internals

! !INTERFACE:
  ! Private name; call using ESMF_ArraySet()
  subroutine ESMF_ArraySetTensorAllDe(array, tensorIndex, staggerLoc, &
    vectorDim, computationalLWidth, computationalUWidth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)              :: array
    integer,          intent(in)              :: tensorIndex(:)
    integer,          intent(in),   optional  :: staggerLoc
    integer,          intent(in),   optional  :: vectorDim
    integer,          intent(in),   optional  :: computationalLWidth(:)
    integer,          intent(in),   optional  :: computationalUWidth(:)
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Set internal information.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        {\tt ESMF\_Array} object for which to set properties.
!     \item[{[tensorIndex]}]
!       Specifies the element within the not distributed array dimensions for which
!       properties are to be set. Default is to set it for all elements.
!     \item[staggerLoc]
!       Stagger location of this tensor element.
!     \item[vectorDim]
!       Dimension along this vector component of this tensor element is aligned.
!     \item[{[computationalLWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
!     \item[{[computationalUWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the computational
!      region with respect to the upper corner of the exclusive region.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Use LogErr to handle return code
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArraySetTensorAllDe
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
!   Prints internal information about the specified {\tt ESMF\_DELayout} 
!   object to {\tt stdout}.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          Specified {\tt ESMF\_Array} object.
!     \item[{[options]}] 
!          Print options are not yet supported.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status       ! local error status

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ArrayPrint(array, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ArrayPrint
!------------------------------------------------------------------------------



!==================== communication calls ===========================



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHalo"
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
  end subroutine ESMF_ArrayHalo
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloStore"
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
  end subroutine ESMF_ArrayHaloStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloRun"
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
  end subroutine ESMF_ArrayHaloRun
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySparseMatMulStore"
!BOP
! !IROUTINE: ESMF_ArraySparseMatMulStore - Store an Array sparse matrix multiplication operation
!
! !INTERFACE:
    subroutine ESMF_ArraySparseMatMulStore(srcArray, dstArray, &
      factorList, factorIndexList, rootPET, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    real(ESMF_KIND_R8), target, intent(in),   optional  :: factorList(:)
    integer,                    intent(in),   optional  :: factorIndexList(:,:)
    integer,                    intent(in)              :: rootPet
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer,                    intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Store an Array sparse matrix multiplication operation from {\tt srcArray} 
!   to {\tt dstArray} with the non-zero matrix coefficients stored in {\tt
!   factorList}. Both Arrays are interpreted as sequentialized vectors. The 
!   sequence is defined by the order of DistGrid dimensions and the order of 
!   patches within the DistGrid. Source and destination Arrys may have different
!   shape and different number of cells.
!
!   The sparse matrix is constructed from the {\tt factorList} and 
!   {\tt factorIndexList} arguments which must be provided on rootPET.
!
!   A {\tt routehandle} is returned and can be used on any pairs of 
!   Arrays that are DistGrid-conform with the Array pair for which the Route
!   was precomputed. 
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArray]
!         {\tt ESMF\_Array} containing source data.
!   \item [dstArray]
!         {\tt ESMF\_Array} holding destination data.
!   \item [{[factorList]}]
!         List of non-zero coefficients. Only rootPET must provide a valid
!         {\tt factorList}.
!   \item [{[factorIndexList]}]
!         List of indices for the factors stored in {\tt factorList}. The 
!         second dimensiom steps through the list elements which are defined by
!         the first dimension. Each list element contains two integers: {\tt 
!         factorIndexList(1,:)} indicates the index in the source Array and
!         {\tt factorIndexList(2,:)} indicates the index in the destination
!         Array. Only rootPET must provide a valid {\tt factorIndexList}.
!     \item[rootPet]
!          PET on which weights are provided.
!   \item [routehandle]
!         Handle to the Route that stores the precomputed operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: status         ! local error status
    real(ESMF_KIND_R8), target    :: dummy(0)       ! helper variable
    real(ESMF_KIND_R8), pointer   :: opt_factorList(:) ! helper variable
    integer                       :: len_factorList    ! helper variable
    type(ESMF_InterfaceInt):: factorIndexListArg    ! helper variable

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    
    ! Deal with (optional) array arguments
    if (present(factorList)) then
      len_factorList = size(factorList)
      opt_factorList => factorList
    else
      len_factorList = 0
      opt_factorList => dummy
    endif
    factorIndexListArg = &
      ESMF_InterfaceIntCreate(farray2D=factorIndexList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySparseMatMulStore(srcArray, dstArray, opt_factorList, &
      len_factorList, factorIndexListArg, rootPet, routehandle, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySparseMatMulStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySparseMatMul"
!BOP
! !IROUTINE: ESMF_ArraySparseMatMul - Execute an Array sparse matrix multiplication operation
!
! !INTERFACE:
    subroutine ESMF_ArraySparseMatMul(srcArray, dstArray, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in)              :: srcArray
    type(ESMF_Array),       intent(inout)           :: dstArray
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute an Array sparse matrix operation from {\tt srcArray} to
!   {\tt dstArray}. See {\tt ArrayInterpolateStore()} for details.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArray]
!         {\tt ESMF\_Array} containing source data.
!   \item [dstArray]
!         {\tt ESMF\_Array} holding destination data.
!   \item [routehandle]
!         Handle to the Route that stores the operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: status         ! local error status

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, dstArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySparseMatMul(srcArray, dstArray, routehandle, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySparseMatMul
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedist"
!BOPI
! !IROUTINE: ESMF_ArrayRedist - Redistribute data from srcArray to dstArray
!
! !INTERFACE:
    subroutine ESMF_ArrayRedist(srcArray, dstArray, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)            :: srcArray
    type(ESMF_Array), intent(out)           :: dstArray
    integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
!   Redistribute data from {\tt srcArray} to {\tt dstArray}. Redist requires
!   that the rank of {\tt srcArray} and {\tt dstArray} be the same. Furthermore
!   the number of distributed dimensions in the associated DistGrids must match
!   and the {\tt indexflag} must be the same.
!
!   There are two variants of the {\tt ArrayRedist()} operation. In the first
!   case the associated DistGrids are defined with indexflag {\tt ESMF\_GLOBAL}
!   and data is mapped from {\tt srcArray} to {\tt dstArray} according to the
!   global index space. In the second case, for indexflag {\tt ESMF\_DELOCAL},
!   the data mapping is done in patch-local index space patch for patch.
!   
!   In either case ArrayRedist() does not require that source and destination
!   Arrays have the same number of cells. Only destination cells that also
!   appear in the source Array will be overwritten.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArray]
!         {\tt ESMF\_Array} containing source data.
!   \item [dstArray]
!         {\tt ESMF\_Array} holding destination data.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayRedist
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStore"
!BOPI
! !IROUTINE: ESMF_ArrayRedistStore - Store an ArrayRedist() operation
!
! !INTERFACE:
    subroutine ESMF_ArrayRedistStore(srcArray, dstArray, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in)              :: srcArray
    type(ESMF_Array),       intent(out)             :: dstArray
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Store an ArrayRedist() operation for {\tt srcArray} and {\tt dstArray}. See
!   ArrayRedist() for details.
!
!   The returned {\tt routehandle} can be used with {\tt srcArray} and
!   {\tt dstArray} arguments that are DistGrid-conform to those for which 
!   the operation was precomputed and stored. 
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArray]
!         {\tt ESMF\_Array} containing source data.
!   \item [dstArray]
!         {\tt ESMF\_Array} holding destination data.
!   \item [routehandle]
!         Handle to the Route that stores the operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayRedistStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistRun"
!BOPI
! !IROUTINE: ESMF_ArrayRedistRun - Execute a stored ArrayRedist() operation
!
! !INTERFACE:
    subroutine ESMF_ArrayRedistRun(srcArray, dstArray, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in)              :: srcArray
    type(ESMF_Array),       intent(out)             :: dstArray
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a stored ArrayRedist() operation for {\tt srcArray} and 
!   {\tt dstArray}.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArray]
!         {\tt ESMF\_Array} containing source data.
!   \item [dstArray]
!         {\tt ESMF\_Array} holding destination data.
!   \item [routehandle]
!         Handle to the Route that stores the operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayRedistRun
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayScatterB(array, larray, rootPET, vm, localrc)

    ! Use LogErr to handle return code
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

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
!        Fortran90 array into which to reduce the Array. Only root
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayScatterB(array, larray, rootPET, vm, localrc)

    ! Use LogErr to handle return code
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayReduceFarray
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySet"
!BOP
! !IROUTINE: ESMF_ArraySet - Set Array properties
!
! !INTERFACE:
  subroutine ESMF_ArraySet(array, name, rc)

!
! !ARGUMENTS:
  type(ESMF_Array),    intent(inout)         :: array
  character (len = *), intent(in)            :: name
  integer,             intent(out), optional :: rc

!
! !DESCRIPTION:
!     Sets the name of the {\tt ESMF\_Array} object.
!     Note: Unlike most other ESMF objects there are very few items which can 
!     be changed once an {\tt ESMF\_Array} object has been created.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array}.
!     \item [name]
!           The Array name.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                       :: status         ! local error status

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Set the name in Base object
    call c_ESMC_SetName(array, "Array", name, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySet
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    !todo: call c_ESMC_ArrayValidate(array, localrc)
    localrc = ESMF_SUCCESS  ! remove when todo is done.
    
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Return success
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayWaitDE(array, de, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayWaitDE
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleCreate"
!BOPI
! !IROUTINE: ESMF_ArrayBundleCreate - Create an ArrayBundle from a list of Arrays
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayBundleCreate()
    function ESMF_ArrayBundleCreate(arrayList, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)             :: arrayList(:)
    integer,          intent(out),  optional :: rc
!         
! !RETURN VALUE:
    type(ESMF_ArrayBundle) :: ESMF_ArrayBundleCreate
!
! !DESCRIPTION:
!   Create an {\tt ESMF\_ArrayBundle} object from a list of Arrays. All the
!   Arrays listed in {\tt arrayList} must be defined on congruent DistGrids, i.e
!   the covered index space, the decomposition and distribution must be 
!   identical. The Arrays may, however, have different memory layouts.
!
!   The creation of an ArrayBundle leaves the bundled Arrays unchanged, they
!   remain valid individual objects. An ArrayBundle is a light weight container
!   of Array references. The actual data remains in place, there are no
!   data movements of duplications associated with the creation of an 
!   ArrayBundle.
!
!   \begin{description}
!   \item [arrayList]
!         List of {\tt ESMF\_Array} objects to be bundled.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: i

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Check init status of arguments
    do i=1, size(arrayList)
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, arrayList(i), rc)
    enddo
    
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_ArrayBundleCreate)
 
    ! Return successfully
    !todo: if (present(rc)) rc = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
  end function ESMF_ArrayBundleCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleDestroy()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleDestroy - Destroy ArrayBundle object

! !INTERFACE:
  subroutine ESMF_ArrayBundleDestroy(arraybundle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)           :: arraybundle
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Destroy an {\tt ESMF\_ArrayBundle} object. The member Arrays are not
!     touched by this operation and remain valid objects that need to be 
!     destroyed individually if necessary.
!
!     The arguments are:
!     \begin{description}
!     \item[arraybundle] 
!          {\tt ESMF\_ArrayBundle} object to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayBundleDestroy(array, localrc)
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this ArrayBundle as invalid
    arraybundle%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(arraybundle)
 
    ! Return successfully
    !todo: if (present(rc)) rc = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
  end subroutine ESMF_ArrayBundleDestroy
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleGet"
!BOPI
! !IROUTINE: ESMF_ArrayBundleGet - Get list of Arrays out of an ArrayBundle
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayBundleGet()
    subroutine ESMF_ArrayBundleGet(arraybundle, arrayCount, arrayList, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)                :: arraybundle
    integer,                intent(out),    optional  :: arrayCount
    type(ESMF_Array),       intent(inout),  optional  :: arrayList(:)
    integer,                intent(out),    optional  :: rc
!
! !DESCRIPTION:
!   Get the list of Arrays bundled in an ArrayBundle.
!
!   \begin{description}
!   \item [arraybundle]
!         {\tt ESMF\_ArrayBundle} to be queried.
!   \item [{[arrayCount]}]
!         Upon return holds the number of Arrays bundled in the ArrayBundle.
!   \item [{[arrayList]}]
!         Upon return holds a List of Arrays bundled in ArrayBundle. The
!         argument must be allocated to be at least of size {\tt arrayCount}.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayBundleGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleHalo"
!BOPI
! !IROUTINE: ESMF_ArrayBundleHalo - Halo an ArrayBundle
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayBundleHalo()
    subroutine ESMF_ArrayBundleHalo(arraybundle, arrayIndex, regionflag, &
      haloLDepth, haloUDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)           :: arraybundle
    integer,                intent(in),   optional  :: arrayIndex
    type(ESMF_RegionFlag),  intent(in),   optional  :: regionflag
    integer,                intent(in),   optional  :: haloLDepth(:)
    integer,                intent(in),   optional  :: haloUDepth(:)
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Perform a halo operation over the data in an {\tt ESMF\_ArrayBundle} object.
!
!   The optional {\tt haloLDepth} and {\tt haloUDepth} arguments can be 
!   provided to specified the exact shape of the halo region. By default 
!   {\tt haloLDepth} and {\tt haloUDepth} are assumed relative to the 
!   computational region of the Array objects in ArrayBundle. The optional {\tt regionflag}
!   may be used to change to the exclusive region as reference for the halo
!   widths.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [arraybundle]
!         {\tt ESMF\_ArrayBundle} containing data to be haloed.
!   \item [{[arrayIndex]}]
!         Index to indicate which Array in the ArrayBundle is to be haloed.
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
  end subroutine ESMF_ArrayBundleHalo
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleHaloStore"
!BOPI
! !IROUTINE: ESMF_ArrayBundleHaloStore - Store an ArrayBundleHalo operation
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleHaloStore(arraybundle, arrayIndex, &
      regionflag, haloLDepth, haloUDepth, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)           :: arraybundle
    integer,                intent(in),   optional  :: arrayIndex
    type(ESMF_RegionFlag),  intent(in),   optional  :: regionflag
    integer,                intent(in),   optional  :: haloLDepth(:)
    integer,                intent(in),   optional  :: haloUDepth(:)
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Store a halo operation over the data in an {\tt ESMF\_ArrayBundle}. See the
!   description for {\tt ArrayBundleHalo()} for details. No actual halo operation
!   is performed by this call, use {\tt ArrayBundleHaloRun} to execute a stored
!   halo operation.
!
!   The Route referenced by the returned {\tt ESMF\_RouteHandle} object can 
!   be used with any {\tt ESMF\_ArrayBundle} object that holds {\em DistGrid conform}, 
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
!   \item [ArrayBundle]
!         {\tt ESMF\_ArrayBundle} containing data to be haloed.
!   \item [{[arrayIndex]}]
!         Index to indicate which Array in the ArrayBundle is to be haloed.
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
  end subroutine ESMF_ArrayBundleHaloStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleHaloRun"
!BOPI
! !IROUTINE: ESMF_ArrayBundleHaloRun - Execute an ArrayBundleHalo operation
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleHaloRun(arraybundle, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)          :: arraybundle
    type(ESMF_RouteHandle), intent(inout)       :: routehandle
    integer, intent(out), optional              :: rc
!
! !DESCRIPTION:
!   Execute the halo operation stored in the Route referenced by 
!   {\tt routehandle} over the data in {\tt ArrayBundle}. See the description for 
!   {\tt ArrayBundleHaloStore()} and {\tt ArrayBundleHalo()} for details. 
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [ArrayBundle]
!         {\tt ESMF\_ArrayBundle} containing data to be haloed.
!   \item [routehandle]
!         Handle to the Route that stores the halo operation to be performed.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayBundleHaloRun
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedist"
!BOPI
! !IROUTINE: ESMF_ArrayBundleRedist - Redistribute data from srcArrayBundle to dstArrayBundle
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleRedist(srcArrayBundle, dstArrayBundle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)       :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(out)      :: dstArrayBundle
    integer, intent(out), optional              :: rc
!
! !DESCRIPTION:
!   Pairwise redistribute data from the Arrays in {\tt srcArrayBundle} to the
!   Arrays in {\tt dstArrayBundle}. Redist requires that the rank of 
!   source Arrays and destination Arrays be the same. Furthermore
!   the number of distributed dimensions in the associated DistGrids must match
!   and the {\tt indexflag} must be the same.
!
!   There are two variants of the Redist operation. In the first
!   case, when the associated DistGrids are defined with indexflag 
!   {\tt ESMF\_GLOBAL},
!   data is mapped from source Array to destination Array according to the
!   global index space. In the second case, for indexflag {\tt ESMF\_DELOCAL},
!   the data mapping is done in patch-local index space patch for patch.
!   
!   In either case Redist does not require that source and destination
!   Arrays have the same number of cells. Only destination cells that also
!   appear in the source Array will be overwritten.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArrayBundle]
!         {\tt ESMF\_ArrayBundle} containing source Arrays.
!   \item [srcArrayBundle]
!         {\tt ESMF\_ArrayBundle} holding destination Arrays.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayBundleRedist
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistStore"
!BOPI
! !IROUTINE: ESMF_ArrayBundleRedistStore - Store an ArrayBundleRedist() operation
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)           :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(out)          :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   Store an ArrayRedistBundle() operation for {\tt srcArrayBundle} and 
!   {\tt dstArrayBundle}. See ArrayBundleRedist() for details.
!
!   The returned {\tt routehandle} can be used with {\tt srcArrayBundle} and
!   {\tt dstArrayBundle} arguments that are DistGrid-conform to those for which 
!   the operation was precomputed and stored. 
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArrayBundle]
!         {\tt ESMF\_ArrayBundle} containing source Arrays.
!   \item [dstArrayBundle]
!         {\tt ESMF\_ArrayBundle} holding destination Arrays.
!   \item [routehandle]
!         Handle to the Route that stores the operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayBundleRedistStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistRun"
!BOPI
! !IROUTINE: ESMF_ArrayBundleRedistRun - Execute a stored ArrayBundleRedist() operation
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleRedistRun(srcArrayBundle, dstArrayBundle, &
      routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)           :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(out)          :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   Execute a stored ArrayBundleRedist() operation for {\tt srcArrayBundle} and 
!   {\tt dstArrayBundle}.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArrayBundle]
!         {\tt ESMF\_ArrayBundle} containing source Arrays.
!   \item [dstArrayBundle]
!         {\tt ESMF\_ArrayBundle} holding destination Arrays.
!   \item [routehandle]
!         Handle to the Route that stores the operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayBundleRedistRun
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSparseMatMulStr"
!BOPI
! !IROUTINE: ESMF_ArrayBundleSparseMatMulStr - Store an ArrayBundle sparse matrix multiplication operation
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleSparseMatMulStr(srcArrayBundle, dstArrayBundle, &
      factorList, factorIndexList, rootPET, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)           :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)        :: dstArrayBundle
    real(ESMF_KIND_R8),  intent(in), optional       :: factorList(:)
    integer,             intent(in), optional       :: factorIndexList(:,:)
    integer,             intent(in)                 :: rootPET
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   Store an Array sparse matrix multiplication operation from the Arrays in
!   {\tt srcArrayBundle} to the Arrays in {\tt dstArrayBundle} with the 
!   non-zero matrix coefficients stored in {\tt factorList}. Both Arrays are 
!   interpreted as sequentialized vectors. The 
!   sequence is defined by the order of DistGrid dimensions and the order of 
!   patches within the DistGrid. Source and destination Arrys may have different
!   shape and different number of cells.
!
!   The sparse matrix is constructed from the {\tt factorList} and 
!   {\tt factorIndexList} arguments which must be provided on rootPET.
!
!   A {\tt routehandle} is returned and can be used on any pairs of 
!   Arrays that are DistGrid-conform with the Array pair for which the Route
!   was precomputed. 
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArrayBundle]
!         {\tt ESMF\_ArrayBundle} containing source Arrays.
!   \item [dstArrayBundle]
!         {\tt ESMF\_Array} holding destination Arrays.
!   \item [{[factorList]}]
!         List of non-zero coefficients. Only rootPET must provide a valid
!         {\tt factorList}.
!   \item [{[factorIndexList]}]
!         List of indices for the factors stored in {\tt factorList}. The 
!         second dimensiom steps through the list elements which are defined by
!         the first dimension. Each list element contains two integers: {\tt 
!         factorIndexList(1,:)} indicates the index in the source Array and
!         {\tt factorIndexList(2,:)} indicates the index in the destination
!         Array. Only rootPET must provide a valid {\tt factorIndexList}.
!     \item[rootPET]
!          PET on which weights are provided.
!   \item [routehandle]
!         Handle to the Route that stores the precomputed operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayBundleSparseMatMulStr
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSparseMatMul"
!BOPI
! !IROUTINE: ESMF_ArrayBundleSparseMatMul - Execute an ArrayBundle sparse matrix multiplication operation
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleSparseMatMul(srcArrayBundle, dstArrayBundle, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)           :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)        :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   Execute an Array sparse matrix operation from the Arrays in {\tt srcArray} 
!   to the Arrays in {\tt dstArray}. See {\tt ArrayInterpolateStore()} for 
!   details.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!   \begin{description}
!   \item [srcArrayBundle]
!         {\tt ESMF\_ArrayBundle} containing source data.
!   \item [dstArrayBundle]
!         {\tt ESMF\_ArrayBundle} holding destination data.
!   \item [routehandle]
!         Handle to the Route that stores the operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  end subroutine ESMF_ArrayBundleSparseMatMul
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleValidate()"
!BOP
! !IROUTINE: ESMF_ArrayBundleValidate - Validate ArrayBundle internals

! !INTERFACE:
  subroutine ESMF_ArrayBundleValidate(arraybundle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)              :: arraybundle
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt ArrayBundle} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[arraybundle] 
!          Specified {\tt ESMF\_ArrayBundle} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    !todo: call c_ESMC_ArrayBundleValidate(arraybundle, localrc)
    localrc = ESMF_SUCCESS  ! remove when todo is done.
    
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArrayBundleValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleGetInit"
!BOPI
! !IROUTINE: ESMF_ArrayBundleGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_ArrayBundleGetInit(arraybundle) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_ArrayBundleGetInit   
!
! !ARGUMENTS:
      type(ESMF_ArrayBundle), intent(in), optional :: arraybundle
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [arraybundle]
!           ArrayBundle object.
!     \end{description}
!
!EOPI

    if (present(arraybundle)) then
      ESMF_ArrayBundleGetInit = ESMF_INIT_GET(arraybundle)
    else
      ESMF_ArrayBundleGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_ArrayBundleGetInit
!------------------------------------------------------------------------------


end module ESMF_ArrayMod
