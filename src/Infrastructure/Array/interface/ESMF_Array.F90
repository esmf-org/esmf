! $Id: ESMF_Array.F90,v 1.83 2008/02/27 15:16:18 rokuingh Exp $
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
  public ESMF_ArrayRedistStore
  public ESMF_ArrayRedist
  public ESMF_ArraySparseMatMulStore
  public ESMF_ArraySparseMatMul
#ifdef FIRSTNEWARRAYPROTOTYPE
  public ESMF_ArrayWait
#endif
  public ESMF_ArrayPrint
  public ESMF_ArrayValidate
  
  public ESMF_ArrayBundleCreate
  public ESMF_ArrayBundleDestroy
  public ESMF_ArrayBundleGet
  public ESMF_ArrayBundleHalo
  public ESMF_ArrayBundleHaloStore
  public ESMF_ArrayBundleHaloRun
  public ESMF_ArrayBundleRedistStore
  public ESMF_ArrayBundleRedist
  public ESMF_ArrayBundleSparseMatMulStr
  public ESMF_ArrayBundleSparseMatMul
  public ESMF_ArrayBundleValidate

  public ESMF_ArrayAttPackCreate      ! Attribute packages
  public ESMF_ArrayAttPackSet         ! Attribute packages
  public ESMF_ArrayAttPackWrite       ! Attribute packages

  public ESMF_ArrayAttributeSet       ! Set and Get attributes
  public ESMF_ArrayAttributeGet       !  

  public ESMF_ArrayAttributeGetCount  ! number of attribs
  public ESMF_ArrayAttributeGetInfo   ! get type, length by name or number

! - ESMF-internal methods:
  public ESMF_ArrayGetInit            ! implemented in ESMF_ArrayCreateMod 
  public ESMF_ArraySetInitCreated     ! implemented in ESMF_ArrayCreateMod 
  public ESMF_ArrayBundleGetInit


!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Array.F90,v 1.83 2008/02/27 15:16:18 rokuingh Exp $'

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
    module procedure ESMF_ArraySetTensor
      
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

      
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArraySparseMatMulStore -- Generic interface

! !INTERFACE:
  interface ESMF_ArraySparseMatMulStore

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArraySparseMatMulStoreI4
    module procedure ESMF_ArraySparseMatMulStoreI8
    module procedure ESMF_ArraySparseMatMulStoreR4
    module procedure ESMF_ArraySparseMatMulStoreR8
    module procedure ESMF_ArraySparseMatMulStoreNF
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
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet  - Set Array attributes
!
! !INTERFACE:
      interface ESMF_ArrayAttributeSet 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttrSetInt4
        module procedure ESMF_ArrayAttrSetInt4List
        module procedure ESMF_ArrayAttrSetInt8
        module procedure ESMF_ArrayAttrSetInt8List
        module procedure ESMF_ArrayAttrSetReal4
        module procedure ESMF_ArrayAttrSetReal4List
        module procedure ESMF_ArrayAttrSetReal8
        module procedure ESMF_ArrayAttrSetReal8List
        module procedure ESMF_ArrayAttrSetLogical
        module procedure ESMF_ArrayAttrSetLogicalList
        module procedure ESMF_ArrayAttrSetChar

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Array}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet  - Get Array attributes
!
! !INTERFACE:
      interface ESMF_ArrayAttributeGet 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttrGetInt4
        module procedure ESMF_ArrayAttrGetInt4List
        module procedure ESMF_ArrayAttrGetInt8
        module procedure ESMF_ArrayAttrGetInt8List
        module procedure ESMF_ArrayAttrGetReal4
        module procedure ESMF_ArrayAttrGetReal4List
        module procedure ESMF_ArrayAttrGetReal8
        module procedure ESMF_ArrayAttrGetReal8List
        module procedure ESMF_ArrayAttrGetLogical
        module procedure ESMF_ArrayAttrGetLogicalList
        module procedure ESMF_ArrayAttrGetChar

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_Array}.
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayAttributeGetInfo - Get type, count from a Array attribute
!
! !INTERFACE:
      interface ESMF_ArrayAttributeGetInfo
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayAttrGetInfoByName
        module procedure ESMF_ArrayAttrGetInfoByNum

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about attributes from an {\tt ESMF\_Array}.
 
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


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
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
! !IROUTINE: ESMF_ArrayRedistStore - Precompute Array redistribution
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
  subroutine ESMF_ArraySet(array, name, staggerLoc, vectorDim, &
    computationalLWidth, computationalUWidth, rc)

!
! !ARGUMENTS:
    type(ESMF_Array),   intent(inout)           :: array
    character(len = *), intent(in),   optional  :: name
    integer,            intent(in),   optional  :: staggerLoc
    integer,            intent(in),   optional  :: vectorDim
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
!     \item[{[staggerLoc]}]
!       User-defined stagger location.
!     \item[{[vectorDim]}]
!       User-defined vector dimension.
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
    call c_ESMC_ArraySet(array, staggerLoc, vectorDim, computationalLWidthArg, &
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
#define ESMF_METHOD "ESMF_ArraySetTensor()"
!BOP
! !IROUTINE: ESMF_ArraySet - Set Array internals for specific tensor component

! !INTERFACE:
  ! Private name; call using ESMF_ArraySet()
  subroutine ESMF_ArraySetTensor(array, tensorIndex, staggerLoc, vectorDim, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)              :: array
    integer,          intent(in)              :: tensorIndex(:)
    integer,          intent(in),   optional  :: staggerLoc
    integer,          intent(in),   optional  :: vectorDim
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Sets adjustable settings in an {\tt ESMF\_Array} object for a specific
!     tensor component.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!       {\tt ESMF\_Array} object for which to set properties.
!     \item[tensorIndex]
!       Specifies the tensor component within the not distributed array
!       dimensions for which properties are to be set.
!     \item[{[staggerLoc]}]
!       Stagger location of this tensor element.
!     \item[{[vectorDim]}]
!       Dimension along this vector component of this tensor element is aligned.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: tensorIndexArg  ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Deal with (optional) array arguments
    tensorIndexArg = ESMF_InterfaceIntCreate(tensorIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySetTensor(array, tensorIndexArg, staggerLoc, vectorDim, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(tensorIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySetTensor
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute Array sparse matrix multiplication with local factors
!
! !INTERFACE:
! ! Private name; call using ESMF_ArraySparseMatMulStore()
! subroutine ESMF_ArraySparseMatMulStore<type><kind>(srcArray, dstArray, &
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
!   {\tt ESMF\_ArraySparseMatMul()} on any pair of Arrays that are congruent
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
#define ESMF_METHOD "ESMF_ArraySparseMatMulStoreI4()"
!BOPI
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute and store an Array sparse matrix multiplication 
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySparseMatMulStore()
  subroutine ESMF_ArraySparseMatMulStoreI4(srcArray, dstArray, routehandle, &
    factorList, factorIndexList, rc)
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
    call c_ESMC_ArraySparseMatMulStore(srcArray, dstArray, routehandle, &
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

  end subroutine ESMF_ArraySparseMatMulStoreI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySparseMatMulStoreI8()"
!BOPI
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute and store an Array sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySparseMatMulStore()
  subroutine ESMF_ArraySparseMatMulStoreI8(srcArray, dstArray, routehandle, &
    factorList, factorIndexList, rc)
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
    call c_ESMC_ArraySparseMatMulStore(srcArray, dstArray, routehandle, &
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

  end subroutine ESMF_ArraySparseMatMulStoreI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySparseMatMulStoreR4()"
!BOPI
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute and store an Array sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySparseMatMulStore()
  subroutine ESMF_ArraySparseMatMulStoreR4(srcArray, dstArray, routehandle, &
    factorList, factorIndexList, rc)
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
    call c_ESMC_ArraySparseMatMulStore(srcArray, dstArray, routehandle, &
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

  end subroutine ESMF_ArraySparseMatMulStoreR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySparseMatMulStoreR8()"
!BOPI
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute and store an Array sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySparseMatMulStore()
  subroutine ESMF_ArraySparseMatMulStoreR8(srcArray, dstArray, routehandle, &
    factorList, factorIndexList, rc)
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
    call c_ESMC_ArraySparseMatMulStore(srcArray, dstArray, routehandle, &
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

  end subroutine ESMF_ArraySparseMatMulStoreR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySparseMatMulStoreNF()"
!BOP
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute Array sparse matrix multiplication without local factors
!
! !INTERFACE:
  ! Private name; call using ESMF_ArraySparseMatMulStore()
  subroutine ESMF_ArraySparseMatMulStoreNF(srcArray, dstArray, routehandle, &
    rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)              :: srcArray
    type(ESMF_Array),           intent(inout)           :: dstArray
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer,                    intent(out),  optional  :: rc
!
! !DESCRIPTION:
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
!   {\tt ESMF\_ArraySparseMatMul()} on any pair of Arrays that are congruent
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
    call c_ESMC_ArraySparseMatMulStoreNF(srcArray, dstArray, routehandle, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySparseMatMulStoreNF
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySparseMatMul()"
!BOP
! !IROUTINE: ESMF_ArraySparseMatMul - Execute an Array sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_ArraySparseMatMul(srcArray, dstArray, routehandle, &
    zeroflag, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in),   optional  :: srcArray
    type(ESMF_Array),       intent(inout),optional  :: dstArray
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    type(ESMF_Logical),     intent(in),   optional  :: zeroflag
    type(ESMF_Logical),     intent(in),   optional  :: checkflag
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a precomputed Array sparse matrix multiplication from {\tt srcArray}
!   to {\tt dstArray}. Both {\tt srcArray} and {\tt dstArray} must be
!   congruent and typekind conform with the respective Arrays used during 
!   {\tt ESMF\_ArraySparseMatMulStore()}. Congruent Arrays possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the Arrays for every DE.
!
!   It is erroneous to specify the identical Array object for {\tt srcArray} and
!   {\tt dstArray} arguments.
!
!   See {\tt ESMF\_ArraySparseMatMulStore()} on how to precompute 
!   {\tt routehandle}. See section \ref{Array:SparseMatMul} for details on the
!   operation {\tt ESMF\_ArraySparseMatMul()} performs.
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
!     If set to {\tt ESMF\_TRUE} {\em (default)} the total regions of all 
!     DEs in {\tt dstArray} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_FALSE} the elements in {\tt dstArray} will not be modified
!     prior to the sparse matrix multiplication and results will be added
!     to the incoming element values.
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
    type(ESMF_Logical)      :: opt_zeroflag ! helper variable
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
    opt_zeroflag = ESMF_TRUE
    if (present(zeroflag)) opt_zeroflag = zeroflag
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
        
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArraySparseMatMul(opt_srcArray, opt_dstArray, routehandle, &
      opt_zeroflag, opt_checkflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySparseMatMul
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


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleCreate()"
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

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    do i=1, size(arrayList)
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, arrayList(i), rc)
    enddo
    
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_ArrayBundleCreate)
 
    ! return successfully
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
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

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
 
    ! return successfully
    !todo: if (present(rc)) rc = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
  end subroutine ESMF_ArrayBundleDestroy
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleGet()"
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
  ! Initialize return code
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_ArrayBundleGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleHalo()"
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
  ! Initialize return code
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_ArrayBundleHalo
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleHaloStore()"
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
  ! Initialize return code
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_ArrayBundleHaloStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleHaloRun()"
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
  ! Initialize return code
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_ArrayBundleHaloRun
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedist()"
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
!   Arrays have the same number of elements. Only destination elements that also
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
  ! Initialize return code
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_ArrayBundleRedist
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistStore()"
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
  ! Initialize return code
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_ArrayBundleRedistStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistRun()"
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
  ! Initialize return code
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_ArrayBundleRedistRun
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSparseMatMulStr()"
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
!   shape and different number of elements.
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
  ! Initialize return code
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_ArrayBundleSparseMatMulStr
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSparseMatMul()"
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
!         {\tt ESMF\_ArrayBundle} with source data.
!   \item [dstArrayBundle]
!         {\tt ESMF\_ArrayBundle} with destination data.
!   \item [routehandle]
!         Handle to the Route that stores the operation.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
  ! Initialize return code
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_ArrayBundleSparseMatMul
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleValidate()"
!BOPI
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
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    !todo: call c_ESMC_ArrayBundleValidate(arraybundle, localrc)
    localrc = ESMF_SUCCESS  ! remove when todo is done.
    
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArrayBundleValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleGetInit()"
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

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttPackCreate"
!BOP
! !IROUTINE: ESMF_ArrayAttPackCreate - Create the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttPackCreate()
      subroutine ESMF_ArrayAttPackCreate(array, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets up the attribute package for the {\tt array}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: name1, name2, name3, name4
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! check input variables
      !ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      !call ESMF_ArrayValidate(array, rc=localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                            ESMF_ERR_PASSTHRU, &
      !                            ESMF_CONTEXT, rc)) return

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif

      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif

      fobject = 'array'

      name1 = 'longname'
      name2 = 'shortname'
      name3 = 'units'
      name4 = 'coordinates'

      call c_ESMC_ArrayAttPackCreate(array, name1, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_ArrayAttPackCreate(array, name2, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_ArrayAttPackCreate(array, name3, fconvention, &
        fpurpose, fobject, localrc)
      call c_ESMC_ArrayAttPackCreate(array, name4, fconvention, &
        fpurpose, fobject, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttPackCreate
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttPackSet"
!BOP
! !IROUTINE: ESMF_ArrayAttPackSet - Set an attribute in the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttPackSet()
      subroutine ESMF_ArrayAttPackSet(array, name, value, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character(ESMF_MAXSTR), intent(in) :: name
      character(ESMF_MAXSTR), intent(in) :: value
      character(ESMF_MAXSTR), intent(in), optional :: convention
      character(ESMF_MAXSTR), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Sets an attribute the attribute package for the {\tt array}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [name]
!      The name of the attribute to be set.
!     \item [value]
!      The value of the attribute to be set.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! check input variables
      !ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      !call ESMF_ArrayValidate(array, rc=localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                            ESMF_ERR_PASSTHRU, &
      !                            ESMF_CONTEXT, rc)) return

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_ArrayAttPackSet(array, name, value, fconvention, &
        fpurpose, fobject, localrc)
        
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttPackSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttPackWrite"
!BOP
! !IROUTINE: ESMF_ArrayAttPackWrite - Print the attribute package
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttPackWrite()
      subroutine ESMF_ArrayAttPackWrite(array, convention, purpose, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in), optional :: convention
      character (len = *), intent(in), optional :: purpose
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Print the attribute package for the {\tt array}.
!     The attribute package defines the convention, purpose, and object type of the three 
!     associated attributes {\tt name}, {\tt organization}, and {\tt discipline}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!      An {\tt ESMF\_Array} object.
!     \item [convention]
!      The convention of the attribute package.
!     \item [purpose]
!      The purpose of the attribute package.
!     \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc                           ! Error status
      character(ESMF_MAXSTR) :: fconvention, fpurpose, fobject

      ! Initialize return code; assume failure until success is certain
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
      ! check input variables
      !ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)
       
      !call ESMF_ArrayValidate(array, rc=localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                            ESMF_ERR_PASSTHRU, &
      !                            ESMF_CONTEXT, rc)) return

      if (present(convention))  then
        fconvention = convention
      else 
        fconvention = 'N/A'
      endif
      
      if (present(purpose)) then
        fpurpose = purpose
      else 
        fpurpose = 'N/A'
      endif
      
      fobject = 'array'

      call c_ESMC_ArrayAttPackWrite(array, fconvention, &
        fpurpose, fobject, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttPackWrite
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayAttributeGet  - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_ArrayAttributeGet(array, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Array), intent(inout) :: array  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an attribute from the {\tt array}.
!     Supported values for <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(out) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(out) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(out) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(out) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(out) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(out) :: valueList
!     \item type(ESMF\_Logical), intent(out) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(out) :: valueList
!     \item character (len = *), intent(out), value
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInt4"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetInt4(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an integer attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                       

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInt4List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetInt4List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte integer list attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc      
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInt8"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetInt8(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc       

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInt8List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetInt8List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte integer list attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                 
      integer :: limit

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetReal4"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetReal4(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc           

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetReal4List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetReal4List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from an {\tt ESMF\_Array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetReal8"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetReal8(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from the {\tt array}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc            

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetReal8List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetReal8List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from an {\tt ESMF\_Array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc     
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetLogical"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetLogical(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetLogicalList"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetLogicalList(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc                
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                    "count longer than valueList", &
                                     ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeGetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetChar"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeGet - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGet()
      subroutine ESMF_ArrayAttrGetChar(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character attribute from the {\tt array}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeGetChar(array, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetChar


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttributeGetCount"

!BOP
! !IROUTINE: ESMF_ArrayAttributeGetCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_ArrayAttributeGetCount(array, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns the number of attributes associated with the given {\tt array} 
!     in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc 

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeGetCount(array, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttributeGetCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInfoByName"

!BOP
! !IROUTINE: ESMF_ArrayAttributeGetInfo - Query Array attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGetInfo()
      subroutine ESMF_ArrayAttrGetInfoByName(array, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns information associated with the named attribute, 
!     including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc             
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttrGetInfoName(array, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrGetInfoByNum"

!BOP
! !IROUTINE: ESMF_ArrayAttributeGetInfo - Query Array attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeGetInfo()
      subroutine ESMF_ArrayAttrGetInfoByNum(array, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out), optional :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute, 
!      including {\tt name}, {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: localrc 
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize 
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttrGetInfoNum(array, attributeIndex, &
        localName, localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrGetInfoByNum
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayAttributeSet - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_ArrayAttributeSet(array, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Array), intent(inout) :: array  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt array}.
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList}.
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(in) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(in) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(in) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(in) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(in) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(in) :: valueList
!     \item type(ESMF\_Logical), intent(in) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(in) :: valueList
!     \item character (len = *), intent(in), value
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetInt4"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetInt4(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer attribute to the {\tt array}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc 

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetInt4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetInt4List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetInt4List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetInt4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetInt8"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetInt8(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer attribute to the {\tt array}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetInt8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetInt8List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetInt8List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte integer list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetInt8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetReal4"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetReal4(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real attribute to the {\tt array}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetReal4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetReal4List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetReal4List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetReal4List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetReal8"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetReal8(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real attribute to the {\tt array}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetReal8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetReal8List"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetReal8List(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetReal8List

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetLogical"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetLogical(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetLogicalList"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetLogicalList(array, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: limit

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_ArrayAttributeSetValue(array, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetLogicalList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayAttrSetChar"

!BOPI
! !IROUTINE: ESMF_ArrayAttributeSet - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAttributeSet()
      subroutine ESMF_ArrayAttrSetChar(array, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character attribute to the {\tt array}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           An {\tt ESMF\_Array} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,array,rc)

      call c_ESMC_ArrayAttributeSetChar(array, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayAttrSetChar
!------------------------------------------------------------------------------

end module ESMF_ArrayMod
