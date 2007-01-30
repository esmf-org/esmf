! $Id: ESMF_Array.F90,v 1.42 2007/01/30 17:51:57 theurich Exp $
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
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_Array
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_Array
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

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
!
! ESMF_RegionFlag

  type ESMF_RegionFlag
  sequence
  private
    integer :: value
  end type

  type(ESMF_RegionFlag), parameter :: &
    ESMF_REGION_EXCLUSIVE = ESMF_RegionFlag(1), &
    ESMF_REGION_COMPUTATIONAL = ESMF_RegionFlag(2)

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Array
  public ESMF_ArrayBundle
  public ESMF_RegionFlag, ESMF_REGION_EXCLUSIVE, ESMF_REGION_COMPUTATIONAL
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ArrayCreate
  public ESMF_ArrayDestroy
  public ESMF_ArrayGet
  public ESMF_ArraySet
  public ESMF_ArrayGather
  public ESMF_ArrayReduce
  public ESMF_ArrayScatter
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
  public ESMF_ArrayGetInit
  public ESMF_ArraySetInitCreated
  public ESMF_ArrayBundleGetInit


!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Array.F90,v 1.42 2007/01/30 17:51:57 theurich Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayCreate -- Generic interface

! !INTERFACE:
      interface ESMF_ArrayCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_ArrayCreateFromLA
      module procedure ESMF_ArrayCreateAssumedShape
      module procedure ESMF_ArrayCreateLocalArray
      module procedure ESMF_ArrayCreateAllocate
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayCreate} functions.   
!EOPI 
      end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayGet -- Generic interface

! !INTERFACE:
      interface ESMF_ArrayGet

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_ArrayGet
      module procedure ESMF_ArrayGetHalo
      module procedure ESMF_ArrayGetFarray2R8
      module procedure ESMF_ArrayGetFarray3R8
      module procedure ESMF_ArrayGetLarray
      module procedure ESMF_ArrayGetTotalCellMask1D
      module procedure ESMF_ArrayGetTotalCellMask2D
      module procedure ESMF_ArrayGetTotalCellMask3D
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayGet} functions.   
!EOPI 
      end interface


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

      end interface

      
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayGather -- Generic interface

! !INTERFACE:
      interface ESMF_ArrayGather

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_ArrayGather

      end interface

      
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayScatter -- Generic interface

! !INTERFACE:
      interface ESMF_ArrayScatter

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_ArrayScatter2R8
      module procedure ESMF_ArrayScatter3R8

      module procedure ESMF_ArrayScatterB        !1st prototype
      module procedure ESMF_ArrayScatterNBRoot   !1st prototype
      module procedure ESMF_ArrayScatterNB       !1st prototype

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayScatter} functions.   
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




! --- Old-style newArray!
! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayCreateFromLA()"
!BOPI
! !IROUTINE: ESMF_ArrayCreate - Create from LocalArray

! !INTERFACE:
  ! Private name; call using ESMF_ArrayCreate()
  function ESMF_ArrayCreateFromLA(larray, haloWidth, deCount, rootPET, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray),  intent(in),   optional  :: larray
    integer, target,        intent(in),   optional  :: haloWidth(:)
    integer,                intent(in),   optional  :: deCount
    integer,                intent(in)              :: rootPET
    integer,                intent(out),  optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_Array) :: ESMF_ArrayCreateFromLA
!
! !DESCRIPTION:
!     Create a new {\tt ESMF\_Array} from an {\tt ESMF\_LocalArray} on 
!     {\tt rootPET}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[larray]}] 
!          The {\tt ESMF\_LocalArray} object that will be used to determine
!          the index space of the {\tt ESMF\_Array}. The data in 
!          {\tt rootPET}'s {\tt larray} will be scattered across the newly
!          created {\tt ESMF\_Array} object before returning. Only 
!         {\tt rootPET} must provide a (valid) {\tt larray} argument.
!     \item[{[haloWidth]}]
!          Array holding the halo width for each array dimension. Default is
!          a halo width of 0 in all dimensions. A negative halo width indicates
!          that the respective array dimension is not to be decomposed. Only
!          {\tt rootPET} must provide a (valid) {\tt haloWidth} argument.
!     \item[{[deCount]}]
!          Number of DEs into which this Array is to be decomposed. By default
!          deCount will equal petCount of the current VM context. Only 
!          {\tt rootPET} must provide a (valid) {\tt deCount} argument.
!     \item[rootPET] 
!          PET that holds the valid data in {\tt larray}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: laRank
    integer, pointer :: opt_haloWidth(:)
    integer :: len_haloWidth
    integer, target  :: dummy(0)     ! used to satisfy the C interface...
    type(ESMF_VM):: vm
    integer:: localPET

    type(ESMF_Array):: array  ! opaque pointer to new C++ Array object

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, larray, rc)
    
    ! Initialize the pointer to NULL
    array%this = ESMF_NULL_POINTER
    
    if (present(haloWidth)) then
      opt_haloWidth => haloWidth
      len_haloWidth = size(haloWidth)
      ! For rootPET check if size(haloWidth) matches the rank of the LocalArray
      call ESMF_VMGetCurrent(vm, rc=localrc)
      call ESMF_VMGet(vm, localPET=localPET, rc=localrc)
      if (localPET==rootPET) then
        call ESMF_LocalArrayGet(larray, rank=laRank, rc=localrc)
        if (len_haloWidth/=laRank) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
            "size of haloWidth array does not match rank of LocalArray", &
            ESMF_CONTEXT, rc)) return
        endif
      endif
    else
      opt_haloWidth => dummy
      len_haloWidth = 0
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayCreate(array, larray, opt_haloWidth, len_haloWidth, &
!      deCount, rootPET, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_ArrayCreateFromLA = array
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_ArrayCreateFromLA)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_ArrayCreateFromLA
!------------------------------------------------------------------------------


!===============================================================================
! ArrayCreate() interfaces
!===============================================================================


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayCreateAssumedShape()"
!BOP
! !IROUTINE: ESMF_ArrayCreateAssumedShape - Create Array from Fortran90 array

! !INTERFACE:
  ! Private name; call using ESMF_ArrayCreate()
  function ESMF_ArrayCreateAssumedShape(farray, distgrid, dimmap, &
    computationalLWidth, computationalUWidth, totalLWidth, totalUWidth, &
    indexflag, staggerLoc, vectorDim, lbounds, ubounds, rc)
!
! !ARGUMENTS:
       real(ESMF_KIND_R8),    intent(in), target      :: farray(:,:)
       type(ESMF_DistGrid),   intent(in)              :: distgrid
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: computationalLWidth(:)
       integer,               intent(in),   optional  :: computationalUWidth(:)
       integer,               intent(in),   optional  :: totalLWidth(:)
       integer,               intent(in),   optional  :: totalUWidth(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: staggerLoc
       integer,               intent(in),   optional  :: vectorDim
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(out),  optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_Array) :: ESMF_ArrayCreateAssumedShape
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Array} object from an existing local native Fortran90
! array according to distgrid. Besides {\tt farray} each PET must issue this 
! call with identical arguments in order to create a consistent Array object.
! The local arrays provided must be dimensioned according to the DE-local
! total region. Bounds of the exclusive regions are set as specified in the
! distgrid argument. Bounds for array dimensions that are not distributed can 
! be chosen freely.
!
! This interface requires a 1 DE per PET decomposition. The Array object will 
! not be created and an error will be returned if this condition is not met.
!
! The not distributed array dimensions form a tensor of rank = array.rank - 
! distgrid.dimCount. By default all tensor elements are associated with 
! stagger location 0. The widths of the computational region are set to 
! the provided value, or zero by default, for all tensor elements. Use 
! {\tt ESMF\_ArraySetTensor()} to change these default settings after the 
! Array object has been created.
!
! The return value is the new {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item[farray] 
!      Valid native Fortran90 array, i.e. memory must be associated with the
!      actual argument. The type/kind/rank information of {\tt farray} will be
!      used to set {\tt Array}'s properties accordingly. The shape of 
!      {\tt farray} will be checked against the information contained in the 
!      {\tt distgrid}.
! \item[distgrid]
!      {\tt ESMF\_DistGrid} object that describes how the array is decomposed and
!      distributed over DEs. The dimCount of distgrid must be smaller or equal
!      to the rank of farray.
! \item[{[dimmap]}]
!      List that contains as many elements as is indicated by distgrids's 
!      dimCount. The elements map each dimension of distgrid to a dimension in
!      {\tt farray}. The default is to map all of distgrid's dimensions against the
!      lower dimension of {\tt farray} in sequence. Unmapped {\tt farray}
!      dimensions are considered to be not distributed dimensions and form a 
!      tensor of rank = farray.rank - distgrid.dimCount or farray.rank -
!      distgrid.dimCount - 1 if {\tt farray} contains an additional DE 
!      dimension.
! \item[{[computationalLWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
!      The default is a zero vector.
! \item[{[computationalUWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the computational
!      region with respect to the upper corner of the exclusive region.
!      The default is a zero vector.
! \item[{[totalLWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the total memory
!      region with respect to the lower corner of the exclusive region.
!      The default is a zero vector.
! \item[{[totalUWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the total memory
!      region with respect to the upper corner of the exclusive region.
!      The default is a vector that contains the remaining number of cells in
!      each direction as to fit the computational region into the memory
!      region provided by the {\tt farray} argument.
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[staggerLoc]}]
!      Stagger location is an arbitrary integer index.
! \item[{[vectorDim]}]
!      If the data stored in this Array object is a component of a vector field
!      then the {\tt vectorDim} argument may be used to identify the dimension
!      along which the vector component is aligned. This information is used to
!      correctly apply the {\tt signChangeVector} defined in the connection
!      transformations of the corresponding DistGrid.
! \item[{[lbounds]}] 
!      Lower bounds for the array dimensions that are not distributed.
! \item[{[ubounds]}] 
!      Upper bounds for the array dimensions that are not distributed.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                           :: status           ! local error status
    type(ESMF_Array)                  :: array ! opaque pointer to new C++ Array
    type(ESMF_LocalArray), allocatable  :: larrayList(:)  ! helper variable
    real(ESMF_KIND_R8), pointer       :: fptr(:,:)        ! helper variable

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Mark this DistGrid as invalid
    array%this = ESMF_NULL_POINTER

    ! prepare the LocalArray list to be used in the ArrayCreate() call
    allocate(larrayList(1))
    fptr => farray
    larrayList(1) = ESMF_LocalArrayCreate(fptr, ESMF_DATA_REF, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! create the Array object
    array = ESMF_ArrayCreate(larrayList, distgrid, dimmap, &
    computationalLWidth, computationalUWidth, totalLWidth, totalUWidth, &
    indexflag, staggerLoc, vectorDim, lbounds, ubounds, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_LocalArrayDestroy(larrayList(1), rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate(larrayList)
 
    ! Set return value
    ESMF_ArrayCreateAssumedShape = array
    
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_ArrayCreateAssumedShape)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_ArrayCreateAssumedShape
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayCreateLocalArray()"
!BOP
! !IROUTINE: ESMF_ArrayCreateLocalArray - Create from a list of LocalArray elements

! !INTERFACE:
  ! Private name; call using ESMF_ArrayCreate()
  function ESMF_ArrayCreateLocalArray(larrayList, distgrid, dimmap, &
    computationalLWidth, computationalUWidth, totalLWidth, totalUWidth, &
    indexflag, staggerLoc, vectorDim, lbounds, ubounds, rc)
!
! !ARGUMENTS:
       type(ESMF_LocalArray), intent(in)              :: larrayList(:)
       type(ESMF_DistGrid),   intent(in)              :: distgrid
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: computationalLWidth(:)
       integer,               intent(in),   optional  :: computationalUWidth(:)
       integer,               intent(in),   optional  :: totalLWidth(:)
       integer,               intent(in),   optional  :: totalUWidth(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: staggerLoc
       integer,               intent(in),   optional  :: vectorDim
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(out),  optional  :: rc
!     
! !RETURN VALUE:
    type(ESMF_Array) :: ESMF_ArrayCreateLocalArray
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Array} object from existing {\tt ESMF\_LocalArray}
! objects according to distgrid. Besides {\tt larrayList} each PET must issue
! this call with identical arguments in order to create a consistent Array 
! object. The local arrays provided must be dimensioned according to the 
! DE-local total region. Bounds of the exclusive regions are set as specified 
! in the distgrid argument. Bounds for array dimensions that are not distributed
! can be chosen freely.
!
! This interface is able to handle multiple DEs per PET.
!
! The not distributed array dimensions form a tensor of rank = array.rank - 
! distgrid.dimCount. By default all tensor elements are associated with 
! stagger location 0. The widths of the computational region are set to 
! the provided value, or zero by default, for all tensor elements. Use 
! {\tt ESMF\_ArraySetTensor()} to change these default settings after the 
! Array object has been created.
!
! The return value is the new {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item[larrayList] 
!      List of valid {\tt ESMF\_LocalArray} objects, i.e. memory must be 
!      associated with the actual arguments. The type/kind/rank information of 
!      all {\tt larrayList} elements must be identical and will
!      be used to set {\tt Array}'s properties accordingly. The shape of each
!      {\tt larrayList} element will be checked against the information 
!      contained in the {\tt distgrid}.
! \item[distgrid]
!      {\tt ESMF\_DistGrid} object that describes how the array is decomposed and
!      distributed over DEs. The dimCount of distgrid must be smaller or equal
!      to the rank specified in arrayspec, otherwise a runtime ESMF error will be
!      raised.
! \item[{[dimmap]}]
!      List that has as many elements as is indicated by distgrids's dimCount
!      value. The elements map each dimension of distgrid to a dimension in
!      arrayspec. The default is to map all of distgrid's dimensions against the
!      lower dimension of arrayspec in sequence.
! \item[{[computationalLWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[computationalUWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the computational
!      region with respect to the upper corner of the exclusive region.
! \item[{[totalMemoryLWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the total memory
!      region with respect to the lower corner of the exclusive region.
! \item[{[totalMemoryUWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the total memory
!      region with respect to the upper corner of the exclusive region.
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[staggerLoc]}]
!      Stagger location is an arbitrary integer index.
! \item[{[vectorDim]}]
!      If the data stored in this Array object is a component of a vector field
!      then the {\tt vectorDim} argument may be used to identify the dimension
!      along which the vector component is aligned. This information is used to
!      correctly apply the {\tt signChangeVector} defined in the connection
!      transformations of the corresponding DistGrid.
! \item[{[lbounds]}] 
!      Lower bounds for tensor array dimensions.
! \item[{[ubounds]}] 
!      Upper bounds for tensor array dimensions.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status     ! local error status
    type(ESMF_Array)        :: array      ! opaque pointer to new C++ Array
    integer                 :: larrayCount, i         ! helper variable
    type(ESMF_InterfaceInt) :: dimmapArg              ! helper variable
    type(ESMF_InterfaceInt) :: computationalLWidthArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUWidthArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLWidthArg         ! helper variable
    type(ESMF_InterfaceInt) :: totalUWidthArg         ! helper variable
    type(ESMF_InterfaceInt) :: lboundsArg             ! helper variable
    type(ESMF_InterfaceInt) :: uboundsArg             ! helper variable
    type(ESMF_Pointer), allocatable :: larrayPointerList(:) ! helper variable

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Determine the number of LocalArray elements in list
    larrayCount = size(larrayList)
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    do i=1, larrayCount
      ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, larrayList(i), rc)
    enddo
    
    ! Copy C++ pointers of deep objects into a simple ESMF_Pointer array
    ! This is necessary in order to strip off the F90 init check members
    ! when passing into C++
    allocate(larrayPointerList(larrayCount))
    do i=1, larrayCount
      call ESMF_LocalArrayGetThis(larrayList(i), larrayPointerList(i), status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    
    ! Deal with (optional) array arguments
    dimmapArg = ESMF_InterfaceIntCreate(dimmap, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLWidthArg = ESMF_InterfaceIntCreate(computationalLWidth, &
      rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUWidthArg = ESMF_InterfaceIntCreate(computationalUWidth, &
      rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLWidthArg = ESMF_InterfaceIntCreate(totalLWidth, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUWidthArg = ESMF_InterfaceIntCreate(totalUWidth, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    lboundsArg = ESMF_InterfaceIntCreate(lbounds, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    uboundsArg = ESMF_InterfaceIntCreate(ubounds, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    array%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayCreateLocalArray(array, larrayPointerList, larrayCount, &
      distgrid, dimmapArg, computationalLWidthArg, computationalUWidthArg, &
      totalLWidthArg, totalUWidthArg, indexflag, staggerLoc, vectorDim, &
      lboundsArg, uboundsArg, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Garbage collection
    deallocate(larrayPointerList)
    call ESMF_InterfaceIntDestroy(dimmapArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(lboundsArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(uboundsArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_ArrayCreateLocalArray = array 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_ArrayCreateLocalArray)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_ArrayCreateLocalArray
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayCreateAllocate()"
!BOP
! !IROUTINE: ESMF_ArrayCreateAllocate - Create Array and allocate memory

! !INTERFACE:
  ! Private name; call using ESMF_ArrayCreate()
  function ESMF_ArrayCreateAllocate(arrayspec, distgrid, dimmap, &
    computationalLWidth, computationalUWidth, totalLWidth, totalUWidth, &
    indexflag, staggerLoc, vectorDim, lbounds, ubounds, rc)
!
! !ARGUMENTS:
       type(ESMF_ArraySpec),  intent(inout)              :: arrayspec
       type(ESMF_DistGrid),   intent(in)              :: distgrid
       integer,               intent(in),   optional  :: dimmap(:)
       integer,               intent(in),   optional  :: computationalLWidth(:)
       integer,               intent(in),   optional  :: computationalUWidth(:)
       integer,               intent(in),   optional  :: totalLWidth(:)
       integer,               intent(in),   optional  :: totalUWidth(:)
       type(ESMF_IndexFlag),  intent(in),   optional  :: indexflag
       integer,               intent(in),   optional  :: staggerLoc
       integer,               intent(in),   optional  :: vectorDim
       integer,               intent(in),   optional  :: lbounds(:)
       integer,               intent(in),   optional  :: ubounds(:)
       integer,               intent(out),  optional  :: rc
!     
! !RETURN VALUE:
    type(ESMF_Array) :: ESMF_ArrayCreateAllocate
!
! !DESCRIPTION:
! Create an {\tt ESMF\_Array} object and allocate uninitialized data space 
! according to arrayspec and distgrid. Each PET must issue
! this call with identical arguments in order to create a consistent Array 
! object. DE-local allocations are made according to the total region defined
! by the arguments to this call: {\tt distgrid} and the optional {\tt Width} 
! arguments.
!
! The return value is the new {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item[arrayspec] 
!     {\tt ESMF\_ArraySpec} object containing the type/kind/rank information.
! \item[distgrid]
!      {\tt ESMF\_DistGrid} object that describes how the array is decomposed and
!      distributed over DEs. The dimCount of distgrid must be smaller or equal
!      to the rank specified in arrayspec, otherwise a runtime ESMF error will be
!      raised.
! \item[{[dimmap]}]
!      List that has as many elements as is indicated by distgrids's dimCount
!      value. The elements map each dimension of distgrid to a dimension in
!      arrayspec. The default is to map all of distgrid's dimensions against the
!      lower dimension of arrayspec in sequence.
! \item[{[computationalLWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the computational
!      region with respect to the lower corner of the exclusive region.
! \item[{[computationalUWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the computational
!      region with respect to the upper corner of the exclusive region.
! \item[{[totalMemoryLWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the lower corner of the total memory
!      region with respect to the lower corner of the exclusive region.
! \item[{[totalMemoryUWidth]}] 
!      This vector argument must have dimCount elements, where dimCount is
!      specified in distgrid. It specifies the upper corner of the total memory
!      region with respect to the upper corner of the exclusive region.
! \item[{[indexflag]}]
!      Flag that indicates how the DE-local indices are to be defined.
! \item[{[staggerLoc]}]
!      Stagger location is an arbitrary integer index.
! \item[{[vectorDim]}]
!      If the data stored in this Array object is a component of a vector field
!      then the {\tt vectorDim} argument may be used to identify the dimension
!      along which the vector component is aligned. This information is used to
!      correctly apply the {\tt signChangeVector} defined in the connection
!      transformations of the corresponding DistGrid.
! \item[{[lbounds]}] 
!      Lower bounds for tensor array dimensions.
! \item[{[ubounds]}] 
!      Upper bounds for tensor array dimensions.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status     ! local error status
    type(ESMF_Array)        :: array      ! opaque pointer to new C++ Array
    type(ESMF_InterfaceInt) :: dimmapArg              ! helper variable
    type(ESMF_InterfaceInt) :: computationalLWidthArg ! helper variable
    type(ESMF_InterfaceInt) :: computationalUWidthArg ! helper variable
    type(ESMF_InterfaceInt) :: totalLWidthArg         ! helper variable
    type(ESMF_InterfaceInt) :: totalUWidthArg         ! helper variable
    type(ESMF_InterfaceInt) :: lboundsArg             ! helper variable
    type(ESMF_InterfaceInt) :: uboundsArg             ! helper variable

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit,arrayspec)
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)

    ! Deal with (optional) array arguments
    dimmapArg = ESMF_InterfaceIntCreate(dimmap, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLWidthArg = ESMF_InterfaceIntCreate(computationalLWidth, &
      rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUWidthArg = ESMF_InterfaceIntCreate(computationalUWidth, &
      rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLWidthArg = ESMF_InterfaceIntCreate(totalLWidth, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUWidthArg = ESMF_InterfaceIntCreate(totalUWidth, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    lboundsArg = ESMF_InterfaceIntCreate(lbounds, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    uboundsArg = ESMF_InterfaceIntCreate(ubounds, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    array%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayCreateAllocate(array, arrayspec, distgrid, dimmapArg, &
      computationalLWidthArg, computationalUWidthArg, totalLWidthArg, &
      totalUWidthArg, indexflag, staggerLoc, vectorDim, lboundsArg, &
      uboundsArg, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(dimmapArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(lboundsArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(uboundsArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_ArrayCreateAllocate = array 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_ArrayCreateAllocate)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_ArrayCreateAllocate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayDestroy()"
!BOP
! !IROUTINE: ESMF_ArrayDestroy - Destroy Array object

! !INTERFACE:
  subroutine ESMF_ArrayDestroy(array, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(inout)           :: array
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Destroy an {\tt ESMF\_Array} object.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          {\tt ESMF\_Array} object to be destroyed.
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
    call c_ESMC_ArrayDestroy(array, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Mark this Array as invalid
    array%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(array)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ArrayDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGet()"
!BOP
! !IROUTINE: ESMF_ArrayGet - Get Array internals

! !INTERFACE:
  subroutine ESMF_ArrayGet(array, type, kind, rank, larrayList, distgrid, &
    delayout, indexflag, dimmap, inverseDimmap, exclusiveLBound, exclusiveUBound,&
    computationalLBound, computationalUBound, totalLBound, totalUBound, &
    computationalLWidth, computationalUWidth, totalLWidth, totalUWidth, &
    name, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),              intent(in)            :: array
    type(ESMF_DataType),           intent(out), optional :: type
    type(ESMF_DataKind),           intent(out), optional :: kind
    integer,                       intent(out), optional :: rank
    type(ESMF_LocalArray), target, intent(out), optional :: larrayList(:)
    type(ESMF_DistGrid),           intent(out), optional :: distgrid
    type(ESMF_DELayout),           intent(out), optional :: delayout
    type(ESMF_IndexFlag),          intent(out), optional :: indexflag
    integer,                       intent(out), optional :: dimmap(:)
    integer,                       intent(out), optional :: inverseDimmap(:)
    integer,                       intent(out), optional :: exclusiveLBound(:,:)
    integer,                       intent(out), optional :: exclusiveUBound(:,:)
    integer,                       intent(out), optional :: computationalLBound(:,:)
    integer,                       intent(out), optional :: computationalUBound(:,:)
    integer,                       intent(out), optional :: totalLBound(:,:)
    integer,                       intent(out), optional :: totalUBound(:,:)
    integer,                       intent(out), optional :: computationalLWidth(:,:)
    integer,                       intent(out), optional :: computationalUWidth(:,:)
    integer,                       intent(out), optional :: totalLWidth(:,:)
    integer,                       intent(out), optional :: totalUWidth(:,:)
    character(len=ESMF_MAXSTR),    intent(out), optional :: name
    integer,                       intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Get internal information.
!
! This interface works for any number of DEs per PET.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        Queried {\tt ESMF\_Array} object.
!     \item[{[type]}]
!        Type of the Array object.
!     \item[{[kind]}]
!        Kind of the Array object.
!     \item[{[rank]}]
!        Rank of the Array object.
!     \item[{[larrayList]}]
!        Upon return this holds a list of the associated {\tt ESMC\_LocalArray}
!        objects. {\tt larrayList} must be allocated to be at least of size
!        {\tt localDeCount}, i.e. the number of DEs associated with the calling
!        PET.`
!     \item[{[distgrid]}]
!        Upon return this holds the associated {\tt ESMF\_DistGrid} object.
!     \item[{[delayout]}]
!        Upon return this holds the associated {\tt ESMF\_DELayout} object.
!     \item[{[indexflag]}]
!       Upon return this flag indicates how the DE-local indices are defined.
!     \item[{[exclusiveLBound]}]
!        Upon return this holds the lower bounds of the exclusive regions for
!        all PET-local DEs. {\tt exclusiveLBound} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item[{[exclusiveUBound]}]
!        Upon return this holds the upper bounds of the exclusive regions for
!        all PET-local DEs. {\tt exclusiveUBound} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item[{[computationalLBound]}]
!        Upon return this holds the lower bounds of the computational regions for
!        all PET-local DEs. {\tt computationalLBound} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item[{[computationalUBound]}]
!        Upon return this holds the upper bounds of the computational regions for
!        all PET-local DEs. {\tt computationalUBound} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item[{[totalLBound]}]
!        Upon return this holds the lower bounds of the total regions for
!        all PET-local DEs. {\tt totalLBound} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item[{[totalUBound]}]
!        Upon return this holds the upper bounds of the total regions for
!        all PET-local DEs. {\tt totalUBound} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item[{[computationalLWidth]}]
!        Upon return this holds the lower width of the computational regions for
!        all PET-local DEs. {\tt computationalLWidth} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item[{[computationalUWidth]}]
!        Upon return this holds the upper width of the computational regions for
!        all PET-local DEs. {\tt computationalUWidth} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item[{[totalLWidth]}]
!        Upon return this holds the lower width of the total memory regions for
!        all PET-local DEs. {\tt computationalUWidth} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item[{[totalUWidth]}]
!        Upon return this holds the upper width of the total memory regions for
!        all PET-local DEs. {\tt totalUWidth} must be allocated to be
!        of size (dimCount, localDeCount).
!     \item [{[name]}]
!        Name of the Array object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                       :: status         ! local error status
    type(ESMF_Pointer), pointer   :: opt_larrayPtrList(:)   ! helper variable
    integer                       :: len_larrayPtrList, i   ! helper variable
    type(ESMF_InterfaceInt)       :: dimmapArg              ! helper variable
    type(ESMF_InterfaceInt)       :: inverseDimmapArg       ! helper variable
    type(ESMF_InterfaceInt)       :: exclusiveLBoundArg     ! helper variable
    type(ESMF_InterfaceInt)       :: exclusiveUBoundArg     ! helper variable
    type(ESMF_InterfaceInt)       :: computationalLBoundArg ! helper variable
    type(ESMF_InterfaceInt)       :: computationalUBoundArg ! helper variable
    type(ESMF_InterfaceInt)       :: totalLBoundArg         ! helper variable
    type(ESMF_InterfaceInt)       :: totalUBoundArg         ! helper variable
    type(ESMF_InterfaceInt)       :: computationalLWidthArg ! helper variable
    type(ESMF_InterfaceInt)       :: computationalUWidthArg ! helper variable
    type(ESMF_InterfaceInt)       :: totalLWidthArg         ! helper variable
    type(ESMF_InterfaceInt)       :: totalUWidthArg         ! helper variable

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Deal with (optional) array arguments
    if (present(larrayList)) then
      len_larrayPtrList = size(larrayList)
      allocate(opt_larrayPtrList(len_larrayPtrList))
    else
      len_larrayPtrList = 0
      allocate(opt_larrayPtrList(1))
    endif
    dimmapArg = ESMF_InterfaceIntCreate(dimmap, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    inverseDimmapArg = ESMF_InterfaceIntCreate(inverseDimmap, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveLBoundArg = ESMF_InterfaceIntCreate(farray2D=exclusiveLBound, &
      rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    exclusiveUBoundArg = ESMF_InterfaceIntCreate(farray2D=exclusiveUBound, &
      rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLBoundArg = &
      ESMF_InterfaceIntCreate(farray2D=computationalLBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUBoundArg = &
      ESMF_InterfaceIntCreate(farray2D=computationalUBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLBoundArg = ESMF_InterfaceIntCreate(farray2D=totalLBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUBoundArg = ESMF_InterfaceIntCreate(farray2D=totalUBound, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalLWidthArg = &
      ESMF_InterfaceIntCreate(farray2D=computationalLWidth, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    computationalUWidthArg = &
      ESMF_InterfaceIntCreate(farray2D=computationalUWidth, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalLWidthArg = ESMF_InterfaceIntCreate(farray2D=totalLWidth, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    totalUWidthArg = ESMF_InterfaceIntCreate(farray2D=totalUWidth, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayGet(array, type, kind, rank, opt_larrayPtrList, &
      len_larrayPtrList, distgrid, delayout, indexflag, dimmapArg, &
      inverseDimmapArg, exclusiveLBoundArg, exclusiveUBoundArg, &
      computationalLBoundArg, computationalUBoundArg, &
      totalLBoundArg, totalUBoundArg, &
      computationalLWidthArg, computationalUWidthArg, &
      totalLWidthArg, totalUWidthArg, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Set init code for deep C++ objects
    if (present(delayout)) then
      call ESMF_DELayoutSetInitCreated(delayout, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(distgrid)) then
      call ESMF_DistGridSetInitCreated(distgrid, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(larrayList)) then
      do i=1, len_larrayPtrList
        call ESMF_LocalArraySetThis(larrayList(i), opt_larrayPtrList(i), &
          rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_LocalArraySetInitCreated(larrayList(i), rc=status)
        if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif
    
    ! Garbage collection
    deallocate(opt_larrayPtrList)
    call ESMF_InterfaceIntDestroy(dimmapArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(inverseDimmapArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveLBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(exclusiveUBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUBoundArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalLWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(computationalUWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalLWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(totalUWidthArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! special call to get name out of Base class
    if (present(name)) then
      call c_ESMC_GetName(array, name, status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGetHalo()"
!BOPI
! !IROUTINE: ESMF_ArrayGetHalo - Get information about a stored halo operation

! !INTERFACE:
  ! Private name; call using ESMF_Get()
  subroutine ESMF_ArrayGetHalo(array, routehandle, regionflag, &
    haloLDepth, haloUDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in)             :: array
    type(ESMF_RouteHandle), intent(in)             :: routehandle
    type(ESMF_RegionFlag),  intent(out),  optional :: regionflag
    integer,                intent(out),  optional :: haloLDepth(:)
    integer,                intent(out),  optional :: haloUDepth(:)
    integer,                intent(out),  optional :: rc  
!         
!
! !DESCRIPTION:
!     Get Fortran90 pointer to DE-local memory regions in Array object.
!
! This interface requires that exactly 1 DE is associated with the calling PET.
! An error will be returned if this condition is not met.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        Queried {\tt ESMF\_Array} object.
!   \item [routehandle]
!         Handle to the stored Route
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
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
  end subroutine ESMF_ArrayGetHalo
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGetFarray2R8()"
!BOP
! !IROUTINE: ESMF_ArrayGetFarray - Get Fortran90 pointer to memory region

! !INTERFACE:
  ! Private name; call using ESMF_Get()
  subroutine ESMF_ArrayGetFarray2R8(array, farrayPtr, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)            :: array
    real(ESMF_KIND_R8), pointer                       :: farrayPtr(:,:)
    integer,                    intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Get Fortran90 pointer to DE-local memory regions in Array object.
!
! This interface requires that exactly 1 DE is associated with the calling PET.
! An error will be returned if this condition is not met.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        Queried {\tt ESMF\_Array} object.
!     \item[{[farrayPtr]}] 
!        Upon return {\tt farrayPtr} points to the DE-local data allocation of
!        {\tt array}.
!     \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                             :: status         ! local error status
    type(ESMF_DELayout)                 :: delayout
    integer                             :: localDeCount
    type(ESMF_LocalArray), allocatable  :: larrayList(:)
    
    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! use general Get() method to obtain information
    call ESMF_ArrayGet(array, delayout=delayout, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! check that there is exactly one DE associated with this PET
    if (localDeCount /= 1) then
      call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, &
        "- Multiple DEs per PET prohibits request", &
        ESMF_CONTEXT, rc)
      return
    endif
    allocate(larrayList(localDeCount))
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=rc)
    call ESMF_LocalArrayGetData(larrayList(1), farrayPtr, ESMF_DATA_REF, rc=rc)
    deallocate(larrayList)
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArrayGetFarray2R8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGetFarray3R8()"
!BOP
! !IROUTINE: ESMF_ArrayGetFarray - Get Fortran90 pointer to memory region

! !INTERFACE:
  ! Private name; call using ESMF_Get()
  subroutine ESMF_ArrayGetFarray3R8(array, farrayPtr, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(in)            :: array
    real(ESMF_KIND_R8), pointer                       :: farrayPtr(:,:,:)
    integer,                    intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Get Fortran90 pointer to DE-local memory regions in Array object.
!
! This interface requires that exactly 1 DE is associated with the calling PET.
! An error will be returned if this condition is not met.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        Queried {\tt ESMF\_Array} object.
!     \item[{[farrayPtr]}] 
!        Upon return {\tt farrayPtr} points to the DE-local data allocation of
!        {\tt array}.
!     \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                             :: status         ! local error status
    type(ESMF_DELayout)                 :: delayout
    integer                             :: localDeCount
    type(ESMF_LocalArray), allocatable  :: larrayList(:)
    
    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! use general Get() method to obtain information
    call ESMF_ArrayGet(array, delayout=delayout, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! check that there is exactly one DE associated with this PET
    if (localDeCount /= 1) then
      call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, &
        "- Multiple DEs per PET prohibits request", &
        ESMF_CONTEXT, rc)
      return
    endif
    allocate(larrayList(localDeCount))
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=rc)
    call ESMF_LocalArrayGetData(larrayList(1), farrayPtr, ESMF_DATA_REF, rc=rc)
    deallocate(larrayList)
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArrayGetFarray3R8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGetLarray()"
!BOP
! !IROUTINE: ESMF_ArrayGetLarray - Get Array internals

! !INTERFACE:
  ! Private name; call using ESMF_Get()
  subroutine ESMF_ArrayGetLarray(array, larray, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in)            :: array
    type(ESMF_LocalArray),  intent(inout)         :: larray
    integer,                intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Get internal information.
!
! This interface requires that exactly 1 DE is associated with the calling PET.
! An error will be returned if this condition is not met.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        Queried {\tt ESMF\_Array} object.
!     \item[larray] 
!        Upon return {\tt larray} refers to the DE-local data allocation of
!        {\tt array}.
!     \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                             :: status         ! local error status
    type(ESMF_DELayout)                 :: delayout
    integer                             :: localDeCount
    type(ESMF_LocalArray), allocatable  :: larrayList(:)
    
    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! use general Get() method to obtain information
    call ESMF_ArrayGet(array, delayout=delayout, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! check that there is exactly one DE associated with this PET
    if (localDeCount /= 1) then
      call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, &
        "Uninitialized Grid argument", &
        ESMF_CONTEXT, rc)
      return
    endif
    allocate(larrayList(localDeCount))
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=rc)
    ! todo: this overrides anything that is currently in the larray argument!!!
    larray = larrayList(1)  ! copy the contents, i.e. the C pointer
    deallocate(larrayList)
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArrayGetLarray
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGetTotalCellMask1D()"
!BOPI
! !IROUTINE: ESMF_ArrayGetTotalCellMask1D - Get Array internals for local DE

! !INTERFACE:
  ! Private name; call using ESMF_Get()
  subroutine ESMF_ArrayGetTotalCellMask1D(array, routehandlelist, localDe, &
    totalCellMask, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in)            :: array
    type(ESMF_RouteHandle), intent(in), optional  :: routehandlelist(:)
    integer,                intent(in)            :: localDe
    integer,                intent(out)           :: totalCellMask(:)
    integer,                intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Get internal information.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        Queried {\tt ESMF\_Array} object.
!     \end{description}
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
  end subroutine ESMF_ArrayGetTotalCellMask1D
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGetTotalCellMask2D()"
!BOPI
! !IROUTINE: ESMF_ArrayGetTotalCellMask2D - Get Array internals for local DE

! !INTERFACE:
  ! Private name; call using ESMF_Get()
  subroutine ESMF_ArrayGetTotalCellMask2D(array, routehandlelist, localDe, &
    totalCellMask, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in)            :: array
    type(ESMF_RouteHandle), intent(in), optional  :: routehandlelist(:)
    integer,                intent(in)            :: localDe
    integer,                intent(out)           :: totalCellMask(:,:)
    integer,                intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Get internal information.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        Queried {\tt ESMF\_Array} object.
!     \end{description}
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
  end subroutine ESMF_ArrayGetTotalCellMask2D
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGetTotalCellMask3D()"
!BOPI
! !IROUTINE: ESMF_ArrayGetTotalCellMask3D - Get Array internals for local DE

! !INTERFACE:
  ! Private name; call using ESMF_Get()
  subroutine ESMF_ArrayGetTotalCellMask3D(array, routehandlelist, localDe, &
    totalCellMask, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),       intent(in)            :: array
    type(ESMF_RouteHandle), intent(in), optional  :: routehandlelist(:)
    integer,                intent(in)            :: localDe
    integer,                intent(out)           :: totalCellMask(:,:,:)
    integer,                intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Get internal information.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        Queried {\tt ESMF\_Array} object.
!     \end{description}
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
  end subroutine ESMF_ArrayGetTotalCellMask3D
!------------------------------------------------------------------------------



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



! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGather()"
!BOPI
! !IROUTINE: ESMF_ArrayGather - Gather a Array into a Fortran90 array

! !INTERFACE:
  subroutine ESMF_ArrayGather(array, farray, patch, rootPET, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(inout)           :: array
    real(ESMF_KIND_R8), target, intent(out),  optional  :: farray(:,:)
    integer,                    intent(in),   optional  :: patch
    integer,                    intent(in)              :: rootPET
    type(ESMF_VM),              intent(in),   optional  :: vm
    integer,                    intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Gather the data of {\tt array} into {\tt farray} located on 
!     {\tt rootPET}. A single DistGrid patch in Array must be gathered into 
!     {\tt farray}. The optional {\tt patch}
!     argument allows selection of the patch. For Arrays defined on a single 
!     patch DistGrid the default selection (patch 1) will be correct. The 
!     shape of {\tt farray} must match the shape of the patch in Array.
!
!     This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The
!     call will block until all PET-local data objects are accessible.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object from which data will be gathered.
!     \item[{[farray]}]
!        The Fortran90 array into which to gather data. Only root
!        must provide a valid {\tt farray}.
!     \item[{[patch]}]
!        The DistGrid patch in {\tt array} from which to gather {\tt farray}.
!        By default {\tt farray} will be gathered from patch 1.
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

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_ArrayScatterB(array, larray, rootPET, vm, localrc)

    ! Use LogErr to handle return code
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayGather
!------------------------------------------------------------------------------


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
#define ESMF_METHOD "ESMF_ArrayScatter2R8()"
!BOP
! !IROUTINE: ESMF_ArrayScatter - Scatter a Fortran90 array across Array

! !INTERFACE:
  subroutine ESMF_ArrayScatter2R8(array, farray, patch, rootPET, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(inout)           :: array
    real(ESMF_KIND_R8), target, intent(in)              :: farray(:,:)
    integer,                    intent(in),   optional  :: patch
    integer,                    intent(in)              :: rootPET
    type(ESMF_VM),              intent(in),   optional  :: vm
    integer,                    intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Scatter the data of {\tt farray} located on {\tt rootPET} 
!     across an {ESMF\_Array} object. A single {\tt farray} must be
!     scattered across a single DistGrid patch in Array. The optional {\tt patch}
!     argument allows selection of the patch. For Arrays defined on a single 
!     patch DistGrid the default selection (patch 1) will be correct. The 
!     shape of {\tt farray} must match the shape of the patch in Array.
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
!        The Fortran90 array that is to be scattered. Only root
!        must provide a valid {\tt farray}.
!     \item[{[patch]}]
!        The DistGrid patch in {\tt array} into which to scatter {\tt farray}.
!        By default {\tt farray} will be scattered into patch 1.
!     \item[rootPET]
!          PET that holds the valid data in {\tt farray}.
!     \item[{[vm]}]
!        Optional {\tt ESMF\_VM} object of the current context. Providing the
!        VM of the current context will lower the method's overhead.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                       :: status         ! local error status
    integer                       :: counts(2)      ! counts vector
    integer                       :: i

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! prepare counts vector
    do i=1, 2
      counts(i) = size(farray, i)
    enddo
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayScatter(array, farray(1,1), ESMF_DATA_REAL, ESMF_R8, &
      2, counts, patch, rootPet, vm, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayScatter2R8
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayScatter3R8()"
!BOP
! !IROUTINE: ESMF_ArrayScatter - Scatter a Fortran90 array across Array

! !INTERFACE:
  subroutine ESMF_ArrayScatter3R8(array, farray, patch, rootPet, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),           intent(inout)           :: array
    real(ESMF_KIND_R8), target, intent(in)              :: farray(:,:,:)
    integer,                    intent(in),   optional  :: patch
    integer,                    intent(in)              :: rootPet
    type(ESMF_VM),              intent(in),   optional  :: vm
    integer,                    intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Scatter the data of {\tt farray} located on {\tt rootPET} 
!     across an {ESMF\_Array} object. A single {\tt farray} must be
!     scattered across a single DistGrid patch in Array. The optional {\tt patch}
!     argument allows selection of the patch. For Arrays defined on a single 
!     patch DistGrid the default selection (patch 1) will be correct. The 
!     shape of {\tt farray} must match the shape of the patch in Array.
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
!        The Fortran90 array that is to be scattered. Only root
!        must provide a valid {\tt farray}.
!     \item[{[patch]}]
!        The DistGrid patch in {\tt array} into which to scatter {\tt farray}.
!        By default {\tt farray} will be scattered into patch 1.
!     \item[rootPet]
!          PET that holds the valid data in {\tt farray}.
!     \item[{[vm]}]
!        Optional {\tt ESMF\_VM} object of the current context. Providing the
!        VM of the current context will lower the method's overhead.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                       :: status         ! local error status
    integer                       :: counts(3)      ! counts vector
    integer                       :: i

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! prepare counts vector
    do i=1, 3
      counts(i) = size(farray, i)
    enddo
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayScatter(array, farray(1,1,1), ESMF_DATA_REAL, ESMF_R8, &
      3, counts, patch, rootPet, vm, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayScatter3R8
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


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGetInit"
!BOPI
! !IROUTINE: ESMF_ArrayGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_ArrayGetInit(array) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_ArrayGetInit   
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in), optional :: array
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           Array object.
!     \end{description}
!
!EOPI

    if (present(array)) then
      ESMF_ArrayGetInit = ESMF_INIT_GET(array)
    else
      ESMF_ArrayGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_ArrayGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySetInitCreated()"
!BOPI
! !IROUTINE: ESMF_ArraySetInitCreated - Set Array init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_ArraySetInitCreated(array, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(inout)           :: array
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in Array object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          Specified {\tt ESMF\_Array} object.
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
    
    ! Set init code
    ESMF_INIT_SET_CREATED(array)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArraySetInitCreated
!------------------------------------------------------------------------------




!!!!!!!!!!!!!! 1st prototype calls !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayScatterB()"
!BOPI
! !IROUTINE: ESMF_ArrayScatter - Scatter a LocalArray across Array

! !INTERFACE:
  ! Private name; call using ESMF_ArrayScatter()
  subroutine ESMF_ArrayScatterB(array, larray, rootPET, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),    intent(inout)           :: array
    type(ESMF_LocalArray),  intent(in)  :: larray
    integer,                intent(in)              :: rootPET
    type(ESMF_VM),          intent(in),   optional  :: vm
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Scatter the data of an {\tt ESMF\_LocalArray} located on {\tt rootPET} 
!     across an {ESMF\_Array} object. This version of the interface 
!     implements the PET-based blocking paradigm: Each PET of the VM must issue
!     this call exactly once for {\em all} of its DEs. The call is 
!     PET-collective, meaning that all PETs must issue the call regardless 
!     whether {\tt array}'s DELayout associates DEs with a PET or not. The
!     call will block until all PET-local data objects are accessible.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object across which data will be scattered.
!     \item[{[larray]}]
!        The {\tt ESMF\_LocalArray} object that is to be scattered. Only root
!        must provide a valid larray.
!     \item[rootPET]
!          PET that holds the valid data in {\tt larray}.
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
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayScatterB
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayScatterNBRoot()"
!BOPI
! !IROUTINE: ESMF_ArrayScatter - Scatter a LocalArray across Array

! !INTERFACE:
  ! Private name; call using ESMF_ArrayScatter()
  subroutine ESMF_ArrayScatterNBRoot(array, larray, rootPET, commhandle, &
    vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),    intent(inout)           :: array
    type(ESMF_LocalArray),  intent(in),   optional  :: larray
    integer,                intent(in)              :: rootPET
    type(ESMF_CommHandle),  intent(inout)           :: commhandle
    type(ESMF_VM),          intent(in),   optional  :: vm
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Scatter the data of an {\tt ESMF\_LocalArray} located on {\tt rootPET} 
!     across an {ESMF\_Array} object. This version of the interface 
!     implements the DE-based non-blocking paradigm.
!     Although only {\tt rootPET} {\em must} issue this call, it is no error
!     for other PETs to also call this routine. However, only {\tt rootPET}
!     will receive a valid {\tt commhandle} which can be used in
!     {\tt ESMF\_ArrayWait} to ensure that access to {\tt larray} is
!     safe again.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object across which data will be scattered.
!     \item[{[larray]}]
!        The {\tt ESMF\_LocalArray} object that is to be scattered. Only root
!        must provide a valid larray.
!     \item[rootPET]
!          PET that holds the valid data in {\tt larray}.
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
!    call c_ESMC_ArrayScatterNBRoot(array, larray, rootPET, commhandle, vm,&
!      localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayScatterNBRoot
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayScatterNB()"
!BOPI
! !IROUTINE: ESMF_ArrayScatter - Scatter a LocalArray across Array

! !INTERFACE:
  ! Private name; call using ESMF_ArrayScatter()
  subroutine ESMF_ArrayScatterNB(array, larray, rootPET, de, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),    intent(inout)           :: array
    type(ESMF_LocalArray),  intent(in)  :: larray
    integer,                intent(in)              :: rootPET
    integer,                intent(in)              :: de
    type(ESMF_VM),          intent(in),   optional  :: vm
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Scatter the data of an {\tt ESMF\_LocalArray} located on {\tt rootPET} 
!     across an {ESMF\_Array} object. This version of the interface 
!     implements the DE-based non-blocking paradigm.
!     Each PET must issue this call once for {\em each} of its DEs. The call
!     is non-blocking and will return immediatly. Use {\tt ESMF\_ArrayWait}
!     to wait for the completion on a specific DE. No {\tt ESMF\_CommHandle} is
!     necessary for the per DE-synchronization because the {\tt array} variable
!     holds all required information and it is not allowed to have more than 
!     one pending communication call per DE per Array.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!        The {\tt ESMF\_Array} object across which data will be scattered.
!     \item[{[larray]}]
!        The {\tt ESMF\_LocalArray} object that is to be scattered. Only root
!        must provide a valid larray.
!     \item[rootPET]
!          PET that holds the valid data in {\tt larray}.
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
!    call c_ESMC_ArrayScatterNB(array, larray, rootPET, de, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_ArrayScatterNB
!------------------------------------------------------------------------------


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
!    call c_ESMC_ArrayReduceScalarB(array, result, ESMF_R8, reduceflag, &
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
!    call c_ESMC_ArrayReduceScalarNBRoot(array, result, ESMF_R8, reduceflag, &
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
!    call c_ESMC_ArrayReduceScalarNB(array, result, ESMF_R8, reduceflag, &
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
