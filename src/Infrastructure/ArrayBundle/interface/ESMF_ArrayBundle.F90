! $Id: ESMF_ArrayBundle.F90,v 1.1.2.6 2008/04/25 23:39:01 oehmke Exp $
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
#define ESMF_FILENAME "ESMF_ArrayBundle.F90"
!==============================================================================
!
! ESMF Array Module
module ESMF_ArrayBundleMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the ArrayBundle class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_ArrayBundleMod
!

!   F90 API wrapper of C++ implemenation of ArrayBundle
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  use ESMF_RHandleMod
  use ESMF_ArrayMod
  
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
  public ESMF_ArrayBundle
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ArrayBundleCreate
  public ESMF_ArrayBundleDestroy
  public ESMF_ArrayBundleGet
  public ESMF_ArrayBundlePrint
  public ESMF_ArrayBundleSparseMatMul
  public ESMF_ArrayBundleSparseMatMulRel
  public ESMF_ArrayBundleSparseMatMulStr

  public ESMF_ArrayBundleHalo
  public ESMF_ArrayBundleHaloStore
  public ESMF_ArrayBundleHaloRun
  public ESMF_ArrayBundleRedistStore
  public ESMF_ArrayBundleRedist
  public ESMF_ArrayBundleValidate

! - ESMF-internal methods:
  public ESMF_ArrayBundleGetInit
  public ESMF_ArrayBundleSetInitCreated
  public ESMF_ArrayBundleSetThisNull


!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_ArrayBundle.F90,v 1.1.2.6 2008/04/25 23:39:01 oehmke Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayBundleSparseMatMulStr -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayBundleSparseMatMulStr

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayBundleSparseMMSI4
    module procedure ESMF_ArrayBundleSparseMMSI8
    module procedure ESMF_ArrayBundleSparseMMSR4
    module procedure ESMF_ArrayBundleSparseMMSR8
    module procedure ESMF_ArrayBundleSparseMMSNF
!EOPI

  end interface


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleCreate()"
!BOP
! !IROUTINE: ESMF_ArrayBundleCreate - Create an ArrayBundle from a list of Arrays
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleCreate()
  function ESMF_ArrayBundleCreate(arrayList, arrayCount, name, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in)            :: arrayList(:)
    integer,          intent(in),  optional :: arrayCount
    character (len=*),intent(in),  optional :: name
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
    type(ESMF_ArrayBundle) :: ESMF_ArrayBundleCreate
!
! !DESCRIPTION:
! Create an {\tt ESMF\_ArrayBundle} object from a list of Arrays.
!
! The creation of an ArrayBundle leaves the bundled Arrays unchanged, they
! remain valid individual objects. An ArrayBundle is a light weight container
! of Array references. The actual data remains in place, there are no
! data movements or duplications associated with the creation of an 
! ArrayBundle.
!
! \begin{description}
! \item [arrayList]
!       List of {\tt ESMF\_Array} objects to be bundled.
! \item [{[arrayCount]}]
!       If provided specifies that only first {\tt arrayCount} Arrays in the
!       {\tt arrayList} argument are to be included in the ArrayBundle. By
!       default {\tt arrayCount} is equal to {\tt size(arrayList)}.
! \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc    ! local return code
    type(ESMF_ArrayBundle)  :: arraybundle! opaque pointer to ESMCI class
    integer :: arrayCount_opt, i
    type(ESMF_Pointer), allocatable :: arrayPointerList(:)
    integer :: len_name

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Determine the number of ArrayList elements
    arrayCount_opt = size(arrayList)
    if (present(arrayCount)) then
      if (arrayCount < 0) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
          "- arrayCount must be positive", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      if (arrayCount > arrayCount_opt) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
          "- arrayCount cannot be larger than size of arrayList", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      arrayCount_opt = arrayCount
    endif

    ! Check init status of arguments
    do i=1, arrayCount_opt
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, arrayList(i), rc)
    enddo
    
    ! Copy C++ pointers of deep objects into a simple ESMF_Pointer array
    ! This is necessary in order to strip off the F90 init check members
    ! when passing into C++
    allocate(arrayPointerList(arrayCount_opt))
    do i=1, arrayCount_opt
      call ESMF_ArrayGetThis(arrayList(i), arrayPointerList(i), localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    
    ! Mark this ArrayBundle object as invalid
    arraybundle%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments
    ! Optional name argument requires separate calls into C++
    if (present(name)) then
      len_name = len(name)
      call c_ESMC_ArrayBundleCreate(arraybundle, arrayPointerList, &
        arrayCount_opt, name, len_name, localrc)
    else
      len_name = 0
      call c_ESMC_ArrayBundleCreate(arraybundle, arrayPointerList, &
        arrayCount_opt, "", len_name, localrc)
    endif    
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Garbage collection
    deallocate(arrayPointerList)

    ! Set return value
    ESMF_ArrayBundleCreate = arraybundle

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_ArrayBundleCreate)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_ArrayBundleCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleDestroy()"
!BOP
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
! Destroy an {\tt ESMF\_ArrayBundle} object. The member Arrays are not
! touched by this operation and remain valid objects that need to be 
! destroyed individually if necessary.
!
! The arguments are:
! \begin{description}
! \item[arraybundle] 
!      {\tt ESMF\_ArrayBundle} object to be destroyed.
! \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ArrayBundleDestroy(arraybundle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this ArrayBundle as invalid
    arraybundle%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(arraybundle)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ArrayBundleDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleGet()"
!BOP
! !IROUTINE: ESMF_ArrayBundleGet - Get list of Arrays out of an ArrayBundle
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayBundleGet()
    subroutine ESMF_ArrayBundleGet(arraybundle, arrayCount, arrayList, &
      name, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)               :: arraybundle
    integer,                intent(out),    optional :: arrayCount
    type(ESMF_Array),       intent(inout),  optional :: arrayList(:)
    character(len=*),       intent(out),    optional :: name
    integer,                intent(out),    optional :: rc
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
!   \item [{[name]}]
!         Name of the ArrayBundle object.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Pointer), pointer   :: opt_arrayPtrList(:)   ! helper variable
    integer                       :: len_arrayPtrList, i   ! helper variable
    integer                       :: opt_arrayCount        ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Deal with (optional) array arguments
    if (present(arrayList)) then
      len_arrayPtrList = size(arrayList)
      allocate(opt_arrayPtrList(len_arrayPtrList))
    else
      len_arrayPtrList = 0
      allocate(opt_arrayPtrList(1))
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ArrayBundleGet(arraybundle, opt_arrayCount, opt_arrayPtrList, &
      len_arrayPtrList, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code for deep C++ objects
    if (present(arrayList)) then
      do i=1, min(len_arrayPtrList, opt_arrayCount)
        call ESMF_ArraySetThis(arrayList(i), opt_arrayPtrList(i), &
          rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArraySetInitCreated(arrayList(i), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif
    
    ! fill in arrayCount output variable
    if (present(arrayCount)) then
      arrayCount = opt_arrayCount
    endif

    ! Garbage collection
    deallocate(opt_arrayPtrList)

    ! Special call to get name out of Base class
    if (present(name)) then
      call c_ESMC_GetName(arraybundle, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_ArrayBundleGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundlePrint()"
!BOP
! !IROUTINE: ESMF_ArrayBundlePrint - Print ArrayBundle internals

! !INTERFACE:
  subroutine ESMF_ArrayBundlePrint(arraybundle, options, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)        :: arraybundle
    character(len=*), intent(in),   optional  :: options
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!   Print internal information of the specified {\tt ESMF\_ArrayBundle} object.
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle] 
!     {\tt ESMF\_ArrayBundle} object.
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
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ArrayBundlePrint(arraybundle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ArrayBundlePrint
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
!   Store a Redist() operation for {\tt srcArrayBundle} and 
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
#define ESMF_METHOD "ESMF_ArrayBundleSparseMatMul()"
!BOP
! !IROUTINE: ESMF_ArrayBundleSparseMatMul - Execute an ArrayBundle sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_ArrayBundleSparseMatMul(srcArrayBundle, dstArrayBundle, &
    routehandle, zeroflag, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in),   optional  :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout),optional  :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    type(ESMF_Logical),     intent(in),   optional  :: zeroflag
    type(ESMF_Logical),     intent(in),   optional  :: checkflag
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Execute a precomputed Array sparse matrix multiplication from the Arrays in
!   {\tt srcArrayBundle} to the Arrays in {\tt dstArrayBundle}.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [{[srcArrayBundle]}]
!     {\tt ESMF\_ArrayBundle} with source data.
!   \item [{[dstArrayBundle]}]
!     {\tt ESMF\_ArrayBundle} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[zeroflag]}]
!     If set to {\tt ESMF\_TRUE} {\em (default)} the total regions of all 
!     DEs in all Arrays in {\tt dstArrayBundle} will be initialized to zero 
!     before updating the elements with the results of the sparse matrix 
!     multiplication. If set to {\tt ESMF\_FALSE} the elements in the Arrays
!     in {\tt dstArrayBundle} will not be modified prior to the sparse matrix
!     multiplication and results will be added to the incoming element values.
!   \item [{[checkflag]}]
!     If set to {\tt ESMF\_TRUE} the input Array pairs will be checked for
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
    type(ESMF_ArrayBundle)  :: opt_srcArrayBundle ! helper variable
    type(ESMF_ArrayBundle)  :: opt_dstArrayBundle ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments, deal with optional ArrayBundle args
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
    if (present(srcArrayBundle)) then
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
      opt_srcArrayBundle = srcArrayBundle
    else
      call ESMF_ArrayBundleSetThisNull(opt_srcArrayBundle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(dstArrayBundle)) then
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
      opt_dstArrayBundle = dstArrayBundle
    else
      call ESMF_ArrayBundleSetThisNull(opt_dstArrayBundle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Set default flags
    opt_zeroflag = ESMF_TRUE
    if (present(zeroflag)) opt_zeroflag = zeroflag
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
        
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSparseMatMul(opt_srcArrayBundle, opt_dstArrayBundle,&
      routehandle, opt_zeroflag, opt_checkflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSparseMatMul
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSparseMatMulRel()"
!BOP
! !IROUTINE: ESMF_ArrayBundleSparseMatMulRel - Release resources associated with ArrayBundle sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_ArrayBundleSparseMatMulRel(routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Release resouces associated with an ArrayBundle sparse matrix multiplication. 
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
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
        
    ! Call into the RouteHandle code
    call ESMF_RouteHandleRelease(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSparseMatMulRel
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ArrayBundleSparseMatMulStr - Precompute ArrayBundle sparse matrix multiplication with local factors
!
! !INTERFACE:
! ! Private name; call using ESMF_ArrayBundleSparseMatMulStr()
! subroutine ESMF_ArrayBundleSparseMatMulStr<type><kind>(srcArrayBundle, &
!   dstArrayBundle, routehandle, factorList, factorIndexList, rc)
!
! !ARGUMENTS:
!   type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
!   type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
!   type(ESMF_RouteHandle),     intent(inout)           :: routehandle
!   <type>(ESMF_KIND_<kind>), target, intent(in)        :: factorList(:)
!   integer,                    intent(in)              :: factorIndexList(:,:)
!   integer,                    intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Store an ArrayBundle sparse matrix multiplication operation from
!   {\tt srcArrayBundle} to {\tt dstArrayBundle}. The sparse matrix
!   multiplication between ArrayBundles is defined as the sequence of
!   individual Array sparse matrix multiplications over all source and
!   destination Array pairs in sequence. The method requires that
!   {\tt srcArrayBundle} and {\tt dstArrayBundle} reference an identical
!   number of {\tt ESMF\_Array} objects.
!
!   The effect of this method on ArrayBundles that contain aliased members is
!   undefined.
!
!   See the description of method {\tt ESMF\_ArraySparseMatMulStore()} for
!   the definition of the Array based operation.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayBundleSparseMatMul()} on any pair of ArrayBundles that 
!   are congruent and typekind conform with the Arrays contained in
!   {\tt srcArrayBundle} and {\tt dstArrayBundle}. 
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
!   \item [srcArrayBundle]
!     {\tt ESMF\_ArrayBundle} with source data.
!   \item [dstArrayBundle]
!     {\tt ESMF\_ArrayBundle} with destination data.
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
!     sequence index of the source element in the source Array while
!     {\tt factorIndexList(2,:)} specifies the sequence index of the
!     destination element in the destination Array. For this format to be a
!     valid option source and destination Arrays must have matching number of
!     tensor elements (the product of the sizes of all Array tensor dimensions).
!     Under this condition an identiy matrix can be applied within the space of
!     tensor elements for each sparse matrix factor.
!
!     The {\em size 4 format} is more general and does not require a matching
!     tensor element count. Here the {\tt factorIndexList(1,:)} specifies the
!     sequence index while {\tt factorIndexList(2,:)} specifies the tensor
!     sequence index of the source element in the source Array. Further
!     {\tt factorIndexList(3,:)} specifies the sequence index and
!     {\tt factorIndexList(4,:)} specifies the tensor sequence index of the 
!     destination element in the destination Array.
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
#define ESMF_METHOD "ESMF_ArrayBundleSparseMMSI4()"
!BOPI
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute and store an Array sparse matrix multiplication 
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSparseMatMulStr()
  subroutine ESMF_ArrayBundleSparseMMSI4(srcArrayBundle, dstArrayBundle, &
    routehandle, factorList, factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
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
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Wrap factor arguments
    len_factorList = size(factorList)
    opt_factorList => factorList
    factorIndexListArg = &
      ESMF_InterfaceIntCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSparseMMStr(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_I4, opt_factorList, len_factorList, &
      factorIndexListArg, localrc)
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

  end subroutine ESMF_ArrayBundleSparseMMSI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSparseMMSI8()"
!BOPI
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute and store an Array sparse matrix multiplication 
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSparseMatMulStr()
  subroutine ESMF_ArrayBundleSparseMMSI8(srcArrayBundle, dstArrayBundle, &
    routehandle, factorList, factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
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
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Wrap factor arguments
    len_factorList = size(factorList)
    opt_factorList => factorList
    factorIndexListArg = &
      ESMF_InterfaceIntCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSparseMMStr(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_I8, opt_factorList, len_factorList, &
      factorIndexListArg, localrc)
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

  end subroutine ESMF_ArrayBundleSparseMMSI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSparseMMSR4()"
!BOPI
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute and store an Array sparse matrix multiplication 
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSparseMatMulStr()
  subroutine ESMF_ArrayBundleSparseMMSR4(srcArrayBundle, dstArrayBundle, &
    routehandle, factorList, factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
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
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Wrap factor arguments
    len_factorList = size(factorList)
    opt_factorList => factorList
    factorIndexListArg = &
      ESMF_InterfaceIntCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSparseMMStr(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_R4, opt_factorList, len_factorList, &
      factorIndexListArg, localrc)
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

  end subroutine ESMF_ArrayBundleSparseMMSR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSparseMMSR8()"
!BOPI
! !IROUTINE: ESMF_ArraySparseMatMulStore - Precompute and store an Array sparse matrix multiplication 
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSparseMatMulStr()
  subroutine ESMF_ArrayBundleSparseMMSR8(srcArrayBundle, dstArrayBundle, &
    routehandle, factorList, factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
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
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Wrap factor arguments
    len_factorList = size(factorList)
    opt_factorList => factorList
    factorIndexListArg = &
      ESMF_InterfaceIntCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSparseMMStr(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_R8, opt_factorList, len_factorList, &
      factorIndexListArg, localrc)
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

  end subroutine ESMF_ArrayBundleSparseMMSR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSparseMMSNF()"
!BOP
! !IROUTINE: ESMF_ArrayBundleSparseMatMulStr - Precompute ArrayBundle sparse matrix multiplication without local factors
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSparseMatMulStr()
  subroutine ESMF_ArrayBundleSparseMMSNF(srcArrayBundle, dstArrayBundle, &
    routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer,                    intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Store an ArrayBundle sparse matrix multiplication operation from
!   {\tt srcArrayBundle} to {\tt dstArrayBundle}. The sparse matrix
!   multiplication between ArrayBundles is defined as the sequence of
!   individual Array sparse matrix multiplications over all source and
!   destination Array pairs in sequence. The method requires that
!   {\tt srcArrayBundle} and {\tt dstArrayBundle} reference an identical
!   number of {\tt ESMF\_Array} objects.
!
!   The effect of this method on ArrayBundles that contain aliased members is
!   undefined.
!
!   See the description of method {\tt ESMF\_ArraySparseMatMulStore()} for
!   the definition of the Array based operation.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayBundleSparseMatMul()} on any pair of ArrayBundles that 
!   are congruent and typekind conform with the Arrays contained in
!   {\tt srcArrayBundle} and {\tt dstArrayBundle}. 
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
!   \item [srcArrayBundle]
!     {\tt ESMF\_ArrayBundle} with source data.
!   \item [dstArrayBundle]
!     {\tt ESMF\_ArrayBundle} with destination data.
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
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSparseMMStrNF(srcArrayBundle, dstArrayBundle, &
      routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSparseMMSNF
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
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
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


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleSetInitCreated - Set ArrayBundle init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_ArrayBundleSetInitCreated(arraybundle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)           :: arraybundle
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in ArrayBundle object to "CREATED".
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

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(arraybundle)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArrayBundleSetInitCreated
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSetThisNull()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleSetThisNull - Set ArrayBundle this member to ESMF_NULL_POINTER

! !INTERFACE:
  subroutine ESMF_ArrayBundleSetThisNull(arraybundle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)           :: arraybundle
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set Array this member to ESMF_NULL_POINTER.
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

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    arraybundle%this = ESMF_NULL_POINTER

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArrayBundleSetThisNull
!------------------------------------------------------------------------------


end module ESMF_ArrayBundleMod
