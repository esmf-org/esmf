! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
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
  use iso_c_binding

  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  use ESMF_IOUtilMod
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
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
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
  public operator(==)
  public operator(/=)

  public ESMF_ArrayBundleAdd
  public ESMF_ArrayBundleAddReplace
  public ESMF_ArrayBundleCreate
  public ESMF_ArrayBundleDestroy
  public ESMF_ArrayBundleGet
  public ESMF_ArrayBundleGetThis
  public ESMF_ArrayBundleHalo
  public ESMF_ArrayBundleHaloRelease
  public ESMF_ArrayBundleHaloStore
  public ESMF_ArrayBundleIsCreated
  public ESMF_ArrayBundlePrint
  public ESMF_ArrayBundleRead
  public ESMF_ArrayBundleRedist
  public ESMF_ArrayBundleRedistStore
  public ESMF_ArrayBundleRedistRelease
  public ESMF_ArrayBundleRemove
  public ESMF_ArrayBundleReplace
  public ESMF_ArrayBundleSMM
  public ESMF_ArrayBundleSMMRelease
  public ESMF_ArrayBundleSMMStore
  public ESMF_ArrayBundleValidate
  public ESMF_ArrayBundleWrite

! - ESMF-internal methods:
  public ESMF_ArrayBundleGetInit
  public ESMF_ArrayBundleSetInitCreated
  public ESMF_ArrayBundleSetThisNull


!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayBundleGet -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayBundleGet

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayBundleGetListAll
    module procedure ESMF_ArrayBundleGetItem
    module procedure ESMF_ArrayBundleGetList
!EOPI

  end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayBundleRedistStore -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayBundleRedistStore

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayBundleRedistStoreI4
    module procedure ESMF_ArrayBundleRedistStoreI8
    module procedure ESMF_ArrayBundleRedistStoreR4
    module procedure ESMF_ArrayBundleRedistStoreR8
    module procedure ESMF_ArrayBundleRedistStoreNF
!EOPI

  end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayBundleSMMStore -- Generic interface

! !INTERFACE:
  interface ESMF_ArrayBundleSMMStore

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayBundleSMMStoreI4
    module procedure ESMF_ArrayBundleSMMStoreI8
    module procedure ESMF_ArrayBundleSMMStoreR4
    module procedure ESMF_ArrayBundleSMMStoreR8
    module procedure ESMF_ArrayBundleSMMStoreNF
!EOPI

  end interface


!===============================================================================
! ArrayBundleOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ArrayBundleAssignment(=) - ArrayBundle assignment
!
! !INTERFACE:
!   interface assignment(=)
!   arraybundle1 = arraybundle2
!
! !ARGUMENTS:
!   type(ESMF_ArrayBundle) :: arraybundle1
!   type(ESMF_ArrayBundle) :: arraybundle2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Assign arraybundle1 as an alias to the same ESMF ArrayBundle object in memory
!   as arraybundle2. If arraybundle2 is invalid, then arraybundle1 will be equally invalid after
!   the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle1]
!     The {\tt ESMF\_ArrayBundle} object on the left hand side of the assignment.
!   \item[arraybundle2]
!     The {\tt ESMF\_ArrayBundle} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ArrayBundleOperator(==) - ArrayBundle equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (arraybundle1 == arraybundle2) then ... endif
!             OR
!   result = (arraybundle1 == arraybundle2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_ArrayBundle), intent(in) :: arraybundle1
!   type(ESMF_ArrayBundle), intent(in) :: arraybundle2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Test whether arraybundle1 and arraybundle2 are valid aliases to the same ESMF
!   ArrayBundle object in memory. For a more general comparison of two ESMF ArrayBundles,
!   going beyond the simple alias test, the ESMF\_ArrayBundleMatch() function (not yet
!   implemented) must be used.
!   \end{sloppypar}
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle1]
!     The {\tt ESMF\_ArrayBundle} object on the left hand side of the equality
!     operation.
!   \item[arraybundle2]
!     The {\tt ESMF\_ArrayBundle} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_ArrayBundleEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ArrayBundleOperator(/=) - ArrayBundle not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (arraybundle1 /= arraybundle2) then ... endif
!             OR
!   result = (arraybundle1 /= arraybundle2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_ArrayBundle), intent(in) :: arraybundle1
!   type(ESMF_ArrayBundle), intent(in) :: arraybundle2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Test whether arraybundle1 and arraybundle2 are {\it not} valid aliases to the
!   same ESMF ArrayBundle object in memory. For a more general comparison of two ESMF
!   ArrayBundles, going beyond the simple alias test, the ESMF\_ArrayBundleMatch() function
!   (not yet implemented) must be used.
!   \end{sloppypar}
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle1]
!     The {\tt ESMF\_ArrayBundle} object on the left hand side of the non-equality
!     operation.
!   \item[arraybundle2]
!     The {\tt ESMF\_ArrayBundle} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_ArrayBundleNE

  end interface
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! ! Interoperability interfaces

#ifndef ESMF_NO_F2018ASSUMEDTYPE

  interface

    subroutine c_ESMC_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, ignoreUnmatchedFlag, len_ignoreUnmatchedFlag, &
      srcToDstTransposeMap, typekind, factor, rc)
      import                :: ESMF_ArrayBundle, ESMF_RouteHandle
      import                :: ESMF_InterArray, ESMF_TypeKind_Flag, ESMF_Logical
      type(ESMF_ArrayBundle):: srcArrayBundle, dstArrayBundle
      type(ESMF_RouteHandle):: routehandle
      type(ESMF_Logical)    :: ignoreUnmatchedFlag
      integer               :: len_ignoreUnmatchedFlag
      type(ESMF_InterArray) :: srcToDstTransposeMap
      type(ESMF_TypeKind_Flag):: typekind
      type(*)               :: factor
      integer               :: rc
    end subroutine

    subroutine c_ESMC_ArrayBundleSMMStore(srcArrayBundle, dstArrayBundle, &
      routehandle, typekindFactors, factorList, factorListCount, &
      factorIndexList, ignoreUnmatchedFlag, len_ignoreUnmatchedFlag, &
      srcTermProcessing, rc)
      import                :: ESMF_ArrayBundle, ESMF_RouteHandle
      import                :: ESMF_InterArray, ESMF_TypeKind_Flag, ESMF_Logical
      type(ESMF_ArrayBundle):: srcArrayBundle, dstArrayBundle
      type(ESMF_RouteHandle):: routehandle
      type(ESMF_TypeKind_Flag):: typekindFactors
      type(*)               :: factorList(*)
      integer               :: factorListCount
      type(ESMF_InterArray) :: factorIndexList
      type(ESMF_Logical)    :: ignoreUnmatchedFlag
      integer               :: len_ignoreUnmatchedFlag
      type(ESMF_InterArray) :: srcTermProcessing
      integer               :: rc
    end subroutine

  end interface

#endif

!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleEQ()"
!BOPI
! !IROUTINE:  ESMF_ArrayBundleEQ - Compare two ArrayBundles for equality
!
! !INTERFACE:
  function ESMF_ArrayBundleEQ(arraybundle1, arraybundle2)
! 
! !RETURN VALUE:
    logical :: ESMF_ArrayBundleEQ

! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in) :: arraybundle1
    type(ESMF_ArrayBundle), intent(in) :: arraybundle2

! !DESCRIPTION:
!   Test if both {\tt arraybundle1} and {\tt arraybundle2} alias the same ESMF ArrayBundle 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE abinit1, abinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    abinit1 = ESMF_ArrayBundleGetInit(arraybundle1)
    abinit2 = ESMF_ArrayBundleGetInit(arraybundle2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (abinit1 .eq. ESMF_INIT_CREATED .and. &
      abinit2 .eq. ESMF_INIT_CREATED) then
      ESMF_ArrayBundleEQ = arraybundle1%this .eq. arraybundle2%this
    else
      ESMF_ArrayBundleEQ = ESMF_FALSE
    endif

  end function ESMF_ArrayBundleEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleNE()"
!BOPI
! !IROUTINE:  ESMF_ArrayBundleNE - Compare two ArrayBundles for non-equality
!
! !INTERFACE:
  function ESMF_ArrayBundleNE(arraybundle1, arraybundle2)
! 
! !RETURN VALUE:
    logical :: ESMF_ArrayBundleNE

! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in) :: arraybundle1
    type(ESMF_ArrayBundle), intent(in) :: arraybundle2

! !DESCRIPTION:
!   Test if both {\tt arraybundle1} and {\tt arraybundle2} alias the same ESMF ArrayBundle 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE abinit1, abinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ESMF_ArrayBundleNE = .not.ESMF_ArrayBundleEQ(arraybundle1, arraybundle2)

  end function ESMF_ArrayBundleNE
!-------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleAdd()"
!BOP
! !IROUTINE: ESMF_ArrayBundleAdd - Add Arrays to an ArrayBundle
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleAdd(arraybundle, arrayList, keywordEnforcer, &
      multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)         :: arraybundle
    type(ESMF_Array),       intent(in)            :: arrayList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: multiflag
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Add Array(s) to an ArrayBundle. It is an error if {\tt arrayList} contains
!   Arrays that match by name Arrays already contained in {\tt arraybundle}.
!
!   \begin{description}
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} to be added to.
!   \item [arrayList]
!     List of {\tt ESMF\_Array} objects to be added.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be added to {\tt arraybundle}. For {\tt .false.} added items must
!     have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     under {\tt multiflag=.false.} mode, where it is {\em not} an error if 
!     {\tt arrayList} contains items with names that are also found in 
!     {\tt arraybundle}. The {\tt arraybundle} is left unchanged for these items.
!     For {\tt .false.} this is treated as an error condition. 
!     The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical)            :: multiflagArg
    type(ESMF_Logical)            :: relaxedflagArg
    integer :: arrayCount, i
    type(ESMF_Pointer), allocatable :: arrayPointerList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Determine the number of ArrayList elements
    arrayCount = size(arrayList)

    ! Check init status of array arguments
    do i=1, arrayCount
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, arrayList(i), rc)
    enddo
    
    if (present(multiflag)) then
      multiflagArg = multiflag
    else
      multiflagArg = ESMF_FALSE
    endif
    
    if (present(relaxedflag)) then
      relaxedflagArg = relaxedflag
    else
      relaxedflagArg = ESMF_FALSE
    endif
    
    ! Copy C++ pointers of deep objects into a simple ESMF_Pointer array
    ! This is necessary in order to strip off the F90 init check members
    ! when passing into C++
    allocate(arrayPointerList(arrayCount))
    do i=1, arrayCount
      call ESMF_ArrayGetThis(arrayList(i), arrayPointerList(i), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! Call into the C++ interface layer
    call c_ESMC_ArrayBundleAdd(arraybundle, arrayPointerList, arrayCount, &
      multiflagArg, relaxedflagArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Garbage collection
    deallocate(arrayPointerList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_ArrayBundleAdd
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleAddReplace()"
!BOP
! !IROUTINE: ESMF_ArrayBundleAddReplace - Conditionally add or replace Arrays in an ArrayBundle
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleAddReplace(arraybundle, arrayList, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)         :: arraybundle
    type(ESMF_Array),       intent(in)            :: arrayList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Arrays in {\tt arrayList} that do not match any Arrays by name in 
!   {\tt arraybundle} are added to the ArrayBundle. Arrays in {\tt arraybundle}
!   that match by name Arrays in {\tt arrayList} are replaced by those Arrays.
!
!   \begin{description}
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} to be manipulated.
!   \item [arrayList]
!     List of {\tt ESMF\_Array} objects to be added or used as replacement.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer :: arrayCount, i
    type(ESMF_Pointer), allocatable :: arrayPointerList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Determine the number of ArrayList elements
    arrayCount = size(arrayList)

    ! Check init status of array arguments
    do i=1, arrayCount
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, arrayList(i), rc)
    enddo
    
    ! Copy C++ pointers of deep objects into a simple ESMF_Pointer array
    ! This is necessary in order to strip off the F90 init check members
    ! when passing into C++
    allocate(arrayPointerList(arrayCount))
    do i=1, arrayCount
      call ESMF_ArrayGetThis(arrayList(i), arrayPointerList(i), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! Call into the C++ interface layer
    call c_ESMC_ArrayBundleAddReplace(arraybundle, arrayPointerList, &
      arrayCount, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Garbage collection
    deallocate(arrayPointerList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_ArrayBundleAddReplace
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleCreate()"
!BOP
! !IROUTINE: ESMF_ArrayBundleCreate - Create an ArrayBundle from a list of Arrays
!
! !INTERFACE:
  function ESMF_ArrayBundleCreate(keywordEnforcer, arrayList, multiflag, &
    relaxedflag, name, rc)
!         
! !RETURN VALUE:
    type(ESMF_ArrayBundle) :: ESMF_ArrayBundleCreate
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Array), intent(in),  optional :: arrayList(:)
    logical,          intent(in),  optional :: multiflag
    logical,          intent(in),  optional :: relaxedflag
    character(len=*), intent(in),  optional :: name
    integer,          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Create an {\tt ESMF\_ArrayBundle} object from a list of existing Arrays.
!
!   The creation of an ArrayBundle leaves the bundled Arrays unchanged, they
!   remain valid individual objects. An ArrayBundle is a light weight container
!   of Array references. The actual data remains in place, there are no
!   data movements or duplications associated with the creation of an 
!   ArrayBundle.
!
!   \begin{description}
!   \item [{[arrayList]}]
!     List of {\tt ESMF\_Array} objects to be bundled.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be added to {\tt arraybundle}. For {\tt .false.} added items must
!     have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     under {\tt multiflag=.false.} mode, where it is {\em not} an error if 
!     {\tt arrayList} contains items with names that are also found in 
!     {\tt arraybundle}. The {\tt arraybundle} is left unchanged for these items.
!     For {\tt .false.} this is treated as an error condition. 
!     The default setting is {\tt .false.}.
!   \item [{[name]}]
!     Name of the created {\tt ESMF\_ArrayBundle}. A default name is generated
!     if not specified.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc    ! local return code
    type(ESMF_ArrayBundle)  :: arraybundle! opaque pointer to ESMCI class
    integer                 :: arrayCount, i
    type(ESMF_Pointer), allocatable :: arrayPointerList(:)
    integer                       :: len_name
    type(ESMF_Logical)            :: multiflagArg
    type(ESMF_Logical)            :: relaxedflagArg

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! invalidate return value
    arraybundle%this = ESMF_NULL_POINTER
    ESMF_ArrayBundleCreate = arraybundle

    ! Determine the number of ArrayList elements
    if (present(arrayList)) then
      arrayCount = size(arrayList)
    else
      arrayCount = 0
    endif

    ! Check init status of arguments
    do i=1, arrayCount
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, arrayList(i), rc)
    enddo
    
    if (present(multiflag)) then
      multiflagArg = multiflag
    else
      multiflagArg = ESMF_FALSE
    endif
    
    if (present(relaxedflag)) then
      relaxedflagArg = relaxedflag
    else
      relaxedflagArg = ESMF_FALSE
    endif

    ! Copy C++ pointers of deep objects into a simple ESMF_Pointer array
    ! This is necessary in order to strip off the F90 init check members
    ! when passing into C++
    allocate(arrayPointerList(arrayCount))
    do i=1, arrayCount
      call ESMF_ArrayGetThis(arrayList(i), arrayPointerList(i), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    
    ! Call into the C++ interface, which will sort out optional arguments
    ! Optional name argument requires separate calls into C++
    if (present(name)) then
      len_name = len(name)
      call c_ESMC_ArrayBundleCreate(arraybundle, arrayPointerList, &
        arrayCount, multiflagArg, relaxedflagArg, name, len_name, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      len_name = 0
      call c_ESMC_ArrayBundleCreate(arraybundle, arrayPointerList, &
        arrayCount, multiflagArg, relaxedflagArg, "", len_name, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif    

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
! !IROUTINE: ESMF_ArrayBundleDestroy - Release resources associated with an ArrayBundle

! !INTERFACE:
  subroutine ESMF_ArrayBundleDestroy(arraybundle, keywordEnforcer, noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)           :: arraybundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional  :: noGarbage
    integer,                intent(out),  optional  :: rc
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Destroys an {\tt ESMF\_ArrayBundle} object. The member Arrays are not
!   touched by this operation and remain valid objects that need to be 
!   destroyed individually if necessary. 
!
!   By default a small remnant of the object is kept in memory in order to 
!   prevent problems with dangling aliases. The default garbage collection
!   mechanism can be overridden with the {\tt noGarbage} argument.
!
! The arguments are:
! \begin{description}
! \item[arraybundle]
!      {\tt ESMF\_ArrayBundle} object to be destroyed.
! \item[{[noGarbage]}]
!      If set to {\tt .TRUE.} the object will be fully destroyed and removed
!      from the ESMF garbage collection system. Note however that under this 
!      condition ESMF cannot protect against accessing the destroyed object 
!      through dangling aliases -- a situation which may lead to hard to debug 
!      application crashes.
! 
!      It is generally recommended to leave the {\tt noGarbage} argument
!      set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!      garbage collection system which will prevent problems with dangling
!      aliases or incorrect sequences of destroy calls. However this level of
!      support requires that a small remnant of the object is kept in memory
!      past the destroy call. This can lead to an unexpected increase in memory
!      consumption over the course of execution in applications that use 
!      temporary ESMF objects. For situations where the repeated creation and 
!      destruction of temporary objects leads to memory issues, it is 
!      recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!      removing the entire temporary object from memory.
! \item[{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc        ! local return code
    type(ESMF_Logical)      :: opt_noGarbage  ! helper variable

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Set default flags
    opt_noGarbage = ESMF_FALSE
    if (present(noGarbage)) opt_noGarbage = noGarbage

    ! Call into the C++ interface layer
    call c_ESMC_ArrayBundleDestroy(arraybundle, opt_noGarbage, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
#define ESMF_METHOD "ESMF_ArrayBundleGetListAll()"
!BOP
! !IROUTINE: ESMF_ArrayBundleGet - Get object-wide information from an ArrayBundle
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayBundleGet()   
    subroutine ESMF_ArrayBundleGetListAll(arraybundle, keywordEnforcer, &
      itemorderflag, arrayCount, arrayList, arrayNameList, name, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),    intent(in)            :: arraybundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_ItemOrder_Flag), intent(in),  optional :: itemorderflag
    integer,                   intent(out), optional :: arrayCount
    type(ESMF_Array),          intent(out), optional :: arrayList(:)
    character(len=*),          intent(out), optional :: arrayNameList(:)
    character(len=*),          intent(out), optional :: name
    integer,                   intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added argument {\tt itemorderflag}.
!              The new argument gives the user control over the order in which
!              the items are returned.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Get general, i.e. not Array name specific information from the ArrayBundle.
!
!   \begin{description}
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} to be queried.
!   \item[{[itemorderflag]}]
!     Specifies the order of the returned items in the {\tt arrayList} and
!     {\tt arrayNameList}.
!     The default is {\tt ESMF\_ITEMORDER\_ABC}.
!     See \ref{const:itemorderflag} for a full list of options.
!   \item [{[arrayCount]}]
!     Upon return holds the number of Arrays bundled in the ArrayBundle.
!   \item [{[arrayList]}]
!     Upon return holds a list of Arrays bundled in {\tt arraybundle}. The
!     argument must be allocated to be at least of size {\tt arrayCount}.
!   \item [{[arrayNameList]}]
!     Upon return holds a list of the names of the Arrays bundled in 
!     {\tt arraybundle}. The argument must be allocated to be at least of
!     size {\tt arrayCount}.
!   \item [{[name]}]
!     Name of the ArrayBundle object.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: opt_arrayCount         ! helper variable
    type(ESMF_Pointer), pointer   :: opt_arrayPtrList(:)    ! helper variable
    integer                       :: len_arrayPtrList       ! helper variable
    integer                       :: i                      ! helper variable
    type(ESMF_ItemOrder_Flag)     :: itemorderflagArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Deal with optional itemorderflag argument
    itemorderflagArg = ESMF_ITEMORDER_ABC ! default
    if (present(itemorderflag)) &
      itemorderflagArg = itemorderflag
    
    ! Deal with (optional) array arguments
    len_arrayPtrList = 0
    if (present(arrayList)) then
      len_arrayPtrList = size(arrayList)
    endif
    if (present(arrayNameList)) then
      len_arrayPtrList = max(len_arrayPtrList, size(arrayNameList))
    endif
    if (present(arrayList).or.present(arrayNameList)) then
      allocate(opt_arrayPtrList(len_arrayPtrList))
    else
      allocate(opt_arrayPtrList(1))
    endif

    ! Call into the C++ interface layer
    call c_ESMC_ArrayBundleGetListAll(arraybundle, opt_arrayCount, &
      opt_arrayPtrList, len_arrayPtrList, itemorderflagArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! fill in arrayCount output variable
    if (present(arrayCount)) then
      arrayCount = opt_arrayCount
    endif

    ! Set init code for deep C++ objects
    if (present(arrayList)) then
      do i=1, min(size(arrayList), opt_arrayCount)
        call ESMF_ArraySetThis(arrayList(i), opt_arrayPtrList(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArraySetInitCreated(arrayList(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif
    
    ! Fill arrayNameList
    if (present(arrayNameList)) then
      do i=1, min(size(arrayNameList), opt_arrayCount)
        call c_ESMC_GetName(opt_arrayPtrList(i), arrayNameList(i), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif
    
    ! Garbage collection
    deallocate(opt_arrayPtrList)

    ! Special call to get name out of Base class
    if (present(name)) then
      call c_ESMC_GetName(arraybundle, name, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_ArrayBundleGetListAll
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleGetItem()"
!BOP
! !IROUTINE: ESMF_ArrayBundleGet - Get information about an Array by name and optionally return an Array
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayBundleGet()   
    subroutine ESMF_ArrayBundleGetItem(arraybundle, arrayName, &
      keywordEnforcer, array, arrayCount, isPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: arraybundle
    character(len=*),       intent(in)            :: arrayName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Array),       intent(out), optional :: array
    integer,                intent(out), optional :: arrayCount
    logical,                intent(out), optional :: isPresent
    integer,                intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Get information about items that match {\tt arrayName} in ArrayBundle.
!
!   \begin{description}
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} to be queried.
!   \item [arrayName]
!     Specified name.
!   \item [{[array]}]
!     Upon return holds the requested Array item. It is an error if this
!     argument was specified and there is not exactly one Array item in 
!     {\tt arraybundle} that matches {\tt arrayName}.
!   \item [{[arrayCount]}]
!     Number of Arrays with {\tt arrayName} in {\tt arraybundle}.
!   \item [{[isPresent]}]
!     Upon return indicates whether Array(s) with {\tt arrayName} exist
!     in {\tt arraybundle}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical)            :: dummyIsPresent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    if (present(array)) then
      ! Call into the C++ interface
      call c_ESMC_ArrayBundleGetItem(arraybundle, trim(arrayName), array, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code for deep C++ object
      call ESMF_ArraySetInitCreated(array, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    if (present(arrayCount)) then
      ! Call into the C++ interface
      call c_ESMC_ArrayBundleGetCount(arraybundle, trim(arrayName), &
        arrayCount, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(isPresent)) then
      ! Call into the C++ interface
      call c_ESMC_ArrayBundleGetIsPresent(arraybundle, trim(arrayName), &
        dummyIsPresent, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      isPresent = dummyIsPresent
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_ArrayBundleGetItem
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleGetList()"
!BOP
! !IROUTINE: ESMF_ArrayBundleGet - Get a list of Arrays by name
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayBundleGet()   
    subroutine ESMF_ArrayBundleGetList(arraybundle, arrayName, arrayList, &
      keywordEnforcer, itemorderflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),    intent(in)            :: arraybundle
    character(len=*),          intent(in)            :: arrayName
    type(ESMF_Array),          intent(out)           :: arrayList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_ItemOrder_Flag), intent(in),  optional :: itemorderflag
    integer,                   intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added argument {\tt itemorderflag}.
!              The new argument gives the user control over the order in which
!              the items are returned.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Get the list of Arrays from ArrayBundle that match {\tt arrayName}.
!
!   \begin{description}
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} to be queried.
!   \item [arrayName]
!     Specified name.
!   \item [arrayList]
!     List of Arrays in {\tt arraybundle} that match {\tt arrayName}. The
!     argument must be allocated to be at least of size {\tt arrayCount}
!     returned for this {\tt arrayName}.
!   \item[{[itemorderflag]}]
!     Specifies the order of the returned items in the {\tt arrayList}.
!     The default is {\tt ESMF\_ITEMORDER\_ABC}.
!     See \ref{const:itemorderflag} for a full list of options.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: opt_arrayCount         ! helper variable
    type(ESMF_Pointer), pointer   :: opt_arrayPtrList(:)    ! helper variable
    integer                       :: len_arrayPtrList       ! helper variable
    integer                       :: i                      ! helper variable
    type(ESMF_ItemOrder_Flag)     :: itemorderflagArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Deal with optional itemorderflag argument
    itemorderflagArg = ESMF_ITEMORDER_ABC ! default
    if (present(itemorderflag)) &
      itemorderflagArg = itemorderflag
    
    ! Prepare local variables
    len_arrayPtrList = size(arrayList)
    allocate(opt_arrayPtrList(len_arrayPtrList))

    ! Call into the C++ interface layer
    call c_ESMC_ArrayBundleGetList(arraybundle, trim(arrayName), &
      opt_arrayCount, opt_arrayPtrList, len_arrayPtrList, itemorderflagArg, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code for deep C++ objects
    do i=1, min(size(arrayList), opt_arrayCount)
      call ESMF_ArraySetThis(arrayList(i), opt_arrayPtrList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_ArraySetInitCreated(arrayList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    
    ! Garbage collection
    deallocate(opt_arrayPtrList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_ArrayBundleGetList
!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleGetThis()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleGetThis - Internal access routine for C++ pointer

! !INTERFACE:
subroutine ESMF_ArrayBundleGetThis(arrayBundle, this, rc)
! !ARGUMENTS:
  type(ESMF_ArrayBundle), intent(in) :: arrayBundle
  type(ESMF_Pointer), intent(inout) :: this
  integer, intent(inout), optional :: rc
!
! !DESCRIPTION:
!     Internal access routine for C++ pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[arrayBundle]
!          Specified {\tt ESMF\_ArrayBundle} object.
!     \item[this]
!          C++ pointer.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  this = arrayBundle%this
  if (present(rc)) rc = ESMF_SUCCESS
end subroutine ESMF_ArrayBundleGetThis

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleHalo()"
!BOP
! !IROUTINE: ESMF_ArrayBundleHalo - Execute an ArrayBundle halo operation
!
! !INTERFACE:
  subroutine ESMF_ArrayBundleHalo(arraybundle, routehandle, keywordEnforcer, &
    checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)          :: arraybundle
    type(ESMF_RouteHandle), intent(inout)          :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional :: checkflag
    integer,                intent(out),  optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Execute a precomputed ArrayBundle halo operation for the Arrays in
!   {\tt arrayBundle}.
!
!   See {\tt ESMF\_ArrayBundleHaloStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} containing data to be haloed.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Array pairs will be checked for
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
    type(ESMF_Logical)      :: opt_checkflag! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments, deal with optional ArrayBundle args
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Set default flags
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleHalo(arraybundle, routehandle, opt_checkflag, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleHalo
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleHaloRelease()"
!BOP
! !IROUTINE: ESMF_ArrayBundleHaloRelease - Release resources associated with an ArrayBundle halo operation
!
! !INTERFACE:
  subroutine ESMF_ArrayBundleHaloRelease(routehandle, keywordEnforcer, &
    noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)          :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional :: noGarbage
    integer,                intent(out),  optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Release resources associated with an ArrayBundle halo operation.
!   After this call {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
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
    call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleHaloRelease
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleHaloStore()"
!BOP
! !IROUTINE: ESMF_ArrayBundleHaloStore - Precompute an ArrayBundle halo operation
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleHaloStore(arraybundle, routehandle, &
      keywordEnforcer, startregion, haloLDepth, haloUDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(inout)         :: arraybundle
    type(ESMF_RouteHandle),     intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_StartRegion_Flag),intent(in),  optional :: startregion
    integer,                    intent(in),  optional :: haloLDepth(:)
    integer,                    intent(in),  optional :: haloUDepth(:)
    integer,                    intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Store an ArrayBundle halo operation over the data in {\tt arraybundle}. By 
!   default, i.e. without specifying {\tt startregion}, {\tt haloLDepth}
!   and {\tt haloUDepth}, all elements in the total Array regions that lie
!   outside the exclusive regions will be considered potential destination
!   elements for the halo operation. However, only those elements that have a corresponding
!   halo source element, i.e. an exclusive element on one of the DEs, will be
!   updated under the halo operation. Elements that have no associated source
!   remain unchanged under halo.
!   \end{sloppypar}
!
!   Specifying {\tt startregion} allows to change the shape of the 
!   effective halo region from the inside. Setting this flag to
!   {\tt ESMF\_STARTREGION\_COMPUTATIONAL} means that only elements outside 
!   the computational region for each Array are considered for potential
!   destination elements for the halo operation. The default is
!   {\tt ESMF\_STARTREGION\_EXCLUSIVE}.
!
!   The {\tt haloLDepth} and {\tt haloUDepth} arguments allow to reduce
!   the extent of the effective halo region. Starting at the region specified
!   by {\tt startregion}, the {\tt haloLDepth} and {\tt haloUDepth}
!   define a halo depth in each direction. Note that the maximum halo region is
!   limited by the total region for each Array, independent of the actual
!   {\tt haloLDepth} and {\tt haloUDepth} setting. The total Array regions are
!   local DE specific. The {\tt haloLDepth} and {\tt haloUDepth} are interpreted
!   as the maximum desired extent, reducing the potentially larger region
!   available for the halo operation.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayBundleHalo()} on any pair of ArrayBundles that matches 
!   {\tt srcArrayBundle} and {\tt dstArrayBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em distributed} dimensions. However, the size, 
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!  
!   This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} containing data to be haloed. The data in the halo
!     regions may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[startregion]}]
!     \begin{sloppypar}
!     The start of the effective halo region on every DE. The default
!     setting is {\tt ESMF\_STARTREGION\_EXCLUSIVE}, rendering all non-exclusive
!     elements potential halo destination elements.
!     See section \ref{const:startregion} for a complete list of
!     valid settings.
!     \end{sloppypar}
!   \item[{[haloLDepth]}] 
!     This vector specifies the lower corner of the effective halo
!     region with respect to the lower corner of {\tt startregion}.
!     The size of {\tt haloLDepth} must equal the number of distributed Array
!     dimensions.
!   \item[{[haloUDepth]}] 
!     This vector specifies the upper corner of the effective halo
!     region with respect to the upper corner of {\tt startregion}.
!     The size of {\tt haloUDepth} must equal the number of distributed Array
!     dimensions.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                         :: localrc          ! local return code
    type(ESMF_StartRegion_Flag)     :: opt_startregion  ! helper variable
    type(ESMF_InterArray)           :: haloLDepthArg    ! helper variable
    type(ESMF_InterArray)           :: haloUDepthArg    ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Set default flags
    opt_startregion = ESMF_STARTREGION_EXCLUSIVE
    if (present(startregion)) opt_startregion = startregion

    ! Deal with (optional) array arguments
    haloLDepthArg = ESMF_InterArrayCreate(haloLDepth, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    haloUDepthArg = ESMF_InterArrayCreate(haloUDepth, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleHaloStore(arraybundle, routehandle, &
      opt_startregion, haloLDepthArg, haloUDepthArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(haloLDepthArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(haloUDepthArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleHaloStore
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleIsCreated()"
!BOP
! !IROUTINE: ESMF_ArrayBundleIsCreated - Check whether an ArrayBundle object has been created

! !INTERFACE:
  function ESMF_ArrayBundleIsCreated(arraybundle, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_ArrayBundleIsCreated
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: arraybundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt arraybundle} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[arraybundle]
!     {\tt ESMF\_ArrayBundle} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_ArrayBundleIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_ArrayBundleGetInit(arraybundle)==ESMF_INIT_CREATED) &
      ESMF_ArrayBundleIsCreated = .true.
  end function
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundlePrint()"
!BOP
! !IROUTINE: ESMF_ArrayBundlePrint - Print ArrayBundle information

! !INTERFACE:
  subroutine ESMF_ArrayBundlePrint(arraybundle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: arraybundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc  
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Print internal information of the specified {\tt ESMF\_ArrayBundle}
!   object to {\tt stdout}. \\
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle] 
!     {\tt ESMF\_ArrayBundle} object.
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
    
    ! Flush before crossing language interface to ensure correct output order
    call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface.
    call c_ESMC_ArrayBundlePrint(arraybundle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ArrayBundlePrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRead()"
!BOP
! !IROUTINE: ESMF_ArrayBundleRead - Read Arrays to an ArrayBundle from file(s)
! \label{api:ArrayBundleRead}

! !INTERFACE:
  subroutine ESMF_ArrayBundleRead(arraybundle, fileName, keywordEnforcer, &
    singleFile, timeslice, iofmt, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)          :: arraybundle
    character(*),           intent(in)             :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional  :: singleFile
    integer,                intent(in),  optional  :: timeslice
    type(ESMF_IOFmt_Flag),  intent(in),  optional  :: iofmt
    integer,                intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Read Array data to an ArrayBundle object from file(s).
!   For this API to be functional, the environment variable {\tt ESMF\_PIO} 
!   should be set to "internal" when the ESMF library is built.
!   Please see the section on Data I/O,~\ref{io:dataio}.
!
!   Limitations:
!   \begin{itemize}
!     \item Only single tile Arrays are supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle] 
!     An {\tt ESMF\_ArrayBundle} object.
!   \item[fileName]
!     The name of the file from which ArrayBundle data is read.
!   \item[{[singleFile]}]
!     A logical flag, the default is .true., i.e., all Arrays in the bundle 
!     are stored in one single file. If .false., each Array is stored 
!     in separate files; these files are numbered with the name based on the
!     argument "file". That is, a set of files are named: [file\_name]001,
!     [file\_name]002, [file\_name]003,...
!   \item[{[timeslice]}]
!    The time-slice number of the variable read from file.
!   \item[{[iofmt]}]
!     \begin{sloppypar}
!    The I/O format.  Please see Section~\ref{opt:iofmtflag} for the list
!    of options. If not present, file names with a {\tt .bin} extension will
!    use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc} extension
!    will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc              ! local return code
    type(ESMF_Logical)      :: opt_singlefileflag   ! helper variable
    type(ESMF_IOFmt_Flag)   :: opt_iofmt            ! helper variable
    integer                 :: file_ext_p

#ifdef ESMF_PIO
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)

    ! Set default flags
    opt_singlefileflag = ESMF_TRUE
    if (present(singleFile)) then
      if (.not. singleFile) then
        opt_singlefileflag = ESMF_FALSE
      end if
    endif

    ! Set iofmt based on file name extension (if present)
    if (present (iofmt)) then
      opt_iofmt = iofmt
    else
      if (index (fileName, '.') > 0) then
        file_ext_p = index (fileName, '.', back=.true.)
        select case (fileName(file_ext_p:))
        case ('.nc')
          opt_iofmt = ESMF_IOFMT_NETCDF
        case ('.bin')
          opt_iofmt = ESMF_IOFMT_BIN
        case default
          opt_iofmt = ESMF_IOFMT_NETCDF
        end select
      else
        opt_iofmt = ESMF_IOFMT_NETCDF
      end if
    end if

    ! Call into the C++ interface, which will call IO object
    call c_esmc_arraybundleread(arraybundle, fileName,       &
        opt_singlefileflag, timeslice, opt_iofmt, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,       &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

#else
    ! Return indicating PIO not present
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT,                 &
        msg="ESMF must be compiled with PIO support to support I/O methods", &
        ESMF_CONTEXT, rcToReturn=rc)
#endif

  end subroutine ESMF_ArrayBundleRead
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedist()"
!BOP
! !IROUTINE: ESMF_ArrayBundleRedist - Execute an ArrayBundle redistribution
! !INTERFACE:
  subroutine ESMF_ArrayBundleRedist(srcArrayBundle, dstArrayBundle, &
    routehandle, keywordEnforcer, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in),    optional :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout), optional :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),    optional :: checkflag
    integer,                intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed ArrayBundle redistribution from the Arrays in
!   {\tt srcArrayBundle} to the Arrays in {\tt dstArrayBundle}.
!   \end{sloppypar}
!
!   The {\tt srcArrayBundle} and {\tt dstArrayBundle} arguments are optional in
!   support of the situation where {\tt srcArrayBundle} and/or
!   {\tt dstArrayBundle} are not defined on all PETs. The {\tt srcArrayBundle}
!   and {\tt dstArrayBundle} must be specified on those PETs that hold source
!   or destination DEs, respectively, but may be omitted on all other PETs.
!   PETs that hold neither source nor destination DEs may omit both arguments.
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
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Array pairs will be checked for
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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(dstArrayBundle)) then
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
      opt_dstArrayBundle = dstArrayBundle
    else
      call ESMF_ArrayBundleSetThisNull(opt_dstArrayBundle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Set default flags
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
        
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedist(opt_srcArrayBundle, opt_dstArrayBundle,&
      routehandle, opt_checkflag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleRedist
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistRelease()"
!BOP
! !IROUTINE: ESMF_ArrayBundleRedistRelease - Release resources associated with ArrayBundle redistribution
!
! !INTERFACE:
  subroutine ESMF_ArrayBundleRedistRelease(routehandle, keywordEnforcer, &
    noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)          :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional :: noGarbage
    integer,                intent(out),  optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Release resources associated with an ArrayBundle redistribution.
!   After this call {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
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
    call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleRedistRelease
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ArrayBundleRedistStore - Precompute an ArrayBundle redistribution with local factor argument
!
! !INTERFACE:
! ! Private name; call using ESMF_ArrayBundleRedistStore()
! subroutine ESMF_ArrayBundleRedistStore<type><kind>(srcArrayBundle, &
!   dstArrayBundle, routehandle, factor, keywordEnforcer, ignoreUnmatchedIndicesFlag, &
!   srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
!   type(ESMF_ArrayBundle),  intent(in)            :: srcArrayBundle
!   type(ESMF_ArrayBundle),  intent(inout)         :: dstArrayBundle
!   type(ESMF_RouteHandle),  intent(inout)         :: routehandle
!   <type>(ESMF_KIND_<kind>),intent(in)            :: factor
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   logical,                 intent(in),  optional :: ignoreUnmatchedIndicesFlag(:)
!   integer,                 intent(in),  optional :: srcToDstTransposeMap(:)
!   integer,                 intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.1.0] Added argument {\tt ignoreUnmatchedIndicesFlag} to support cases
!    where source and destination side do not cover the exact same index space.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Store an ArrayBundle redistribution operation from
!   {\tt srcArrayBundle} to {\tt dstArrayBundle}. The redistribution
!   between ArrayBundles is defined as the sequence of
!   individual Array redistributions over all source and
!   destination Array pairs in sequence. The method requires that
!   {\tt srcArrayBundle} and {\tt dstArrayBundle} reference an identical
!   number of {\tt ESMF\_Array} objects.
!
!   The effect of this method on ArrayBundles that contain aliased members is
!   undefined.
!
!   PETs that specify a {\tt factor} argument must use the
!   <type><kind> overloaded interface. Other PETs call into the interface
!   without {\tt factor} argument. If multiple PETs specify the {\tt factor}
!   argument its type and kind as well as its value must match across all
!   PETs. If none of the PETs specifies a {\tt factor} argument the default
!   will be a factor of 1.
!
!   See the description of method {\tt ESMF\_ArrayRedistStore()} for
!   the definition of the Array based operation.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayBundleRedist()} on any pair of ArrayBundles that matches 
!   {\tt srcArrayBundle} and {\tt dstArrayBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em distributed} dimensions. However, the size, 
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
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
!     {\tt ESMF\_ArrayBundle} with destination data. The data in these Arrays
!     may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [factor]
!     Factor by which to multiply source data.
!   \item [{[ignoreUnmatchedIndicesFlag]}] 
!     If set to {.false.}, the {\em default}, source and destination side must
!     cover the identical index space, using precisely matching sequence
!     indices. If set to {.true.}, mismatching sequence indices between source
!     and destination side are silently ignored.
!     The size of this array argument must either be 1 or equal the number of
!     Arrays in the {\tt srcArrayBundle} and {\tt dstArrayBundle} arguments. In
!     the latter case, the handling of unmatched indices is specified for each
!     Array pair separately. If only one element is specified, it is
!     used for {\em all} Array pairs.
!   \item [{[srcToDstTransposeMap]}]
!     List with as many entries as there are dimensions in the Arrays in
!     {\tt srcArrayBundle}. Each
!     entry maps the corresponding source Array dimension against the 
!     specified destination Array dimension. Mixing of distributed and
!     undistributed dimensions is supported.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistStoreI4()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleRedistStore - Precompute an ArrayBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleRedistStore()
  subroutine ESMF_ArrayBundleRedistStoreI4(srcArrayBundle, dstArrayBundle, &
    routehandle, factor, keywordEnforcer, ignoreUnmatchedIndicesFlag, &
    srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)         :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
    integer(ESMF_KIND_I4),  intent(in)            :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                intent(in),  optional :: srcToDstTransposeMap(:)
    integer,                intent(out), optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical), pointer   :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target    :: def_ignoreUnmatched(1)
    integer                       :: len_ignoreUnmatched
    type(ESMF_InterArray)         :: srcToDstTransposeMapArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Deal with srcToDstTransposeMap
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I4, factor, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleRedistStoreI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistStoreI8()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleRedistStore - Precompute an ArrayBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleRedistStore()
  subroutine ESMF_ArrayBundleRedistStoreI8(srcArrayBundle, dstArrayBundle, &
    routehandle, factor, keywordEnforcer, ignoreUnmatchedIndicesFlag, &
    srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)         :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
    integer(ESMF_KIND_I8),  intent(in)            :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                intent(in),  optional :: srcToDstTransposeMap(:)
    integer,                intent(out), optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical), pointer   :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target    :: def_ignoreUnmatched(1)
    integer                       :: len_ignoreUnmatched
    type(ESMF_InterArray)         :: srcToDstTransposeMapArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Deal with srcToDstTransposeMap
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_I8, factor, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleRedistStoreI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistStoreR4()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleRedistStore - Precompute an ArrayBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleRedistStore()
  subroutine ESMF_ArrayBundleRedistStoreR4(srcArrayBundle, dstArrayBundle, &
    routehandle, factor, keywordEnforcer, ignoreUnmatchedIndicesFlag, &
    srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)         :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
    real(ESMF_KIND_R4),     intent(in)            :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                intent(in),  optional :: srcToDstTransposeMap(:)
    integer,                intent(out), optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical), pointer   :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target    :: def_ignoreUnmatched(1)
    integer                       :: len_ignoreUnmatched
    type(ESMF_InterArray)         :: srcToDstTransposeMapArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Deal with srcToDstTransposeMap
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_R4, factor, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleRedistStoreR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistStoreR8()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleRedistStore - Precompute an ArrayBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleRedistStore()
  subroutine ESMF_ArrayBundleRedistStoreR8(srcArrayBundle, dstArrayBundle, &
    routehandle, factor, keywordEnforcer, ignoreUnmatchedIndicesFlag, &
    srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)         :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
    real(ESMF_KIND_R8),     intent(in)            :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                intent(in),  optional :: srcToDstTransposeMap(:)
    integer,                intent(out), optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical), pointer   :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target    :: def_ignoreUnmatched(1)
    integer                       :: len_ignoreUnmatched
    type(ESMF_InterArray)         :: srcToDstTransposeMapArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Deal with srcToDstTransposeMap
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcToDstTransposeMapArg, ESMF_TYPEKIND_R8, factor, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleRedistStoreR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistStoreNF()"
!BOP
! !IROUTINE: ESMF_ArrayBundleRedistStore - Precompute an ArrayBundle redistribution without local factor argument
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleRedistStore()
  subroutine ESMF_ArrayBundleRedistStoreNF(srcArrayBundle, dstArrayBundle, &
    routehandle, keywordEnforcer, ignoreUnmatchedIndicesFlag, &
    srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)         :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                intent(in),  optional :: srcToDstTransposeMap(:)
    integer,                intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.1.0] Added argument {\tt ignoreUnmatchedIndicesFlag} to support cases
!    where source and destination side do not cover the exact same index space.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Store an ArrayBundle redistribution operation from
!   {\tt srcArrayBundle} to {\tt dstArrayBundle}. The redistribution
!   between ArrayBundles is defined as the sequence of
!   individual Array redistributions over all source and
!   destination Array pairs in sequence. The method requires that
!   {\tt srcArrayBundle} and {\tt dstArrayBundle} reference an identical
!   number of {\tt ESMF\_Array} objects.
!
!   The effect of this method on ArrayBundles that contain aliased members is
!   undefined.
!
!   PETs that specify a {\tt factor} argument must use the
!   <type><kind> overloaded interface. Other PETs call into the interface
!   without {\tt factor} argument. If multiple PETs specify the {\tt factor}
!   argument its type and kind as well as its value must match across all
!   PETs. If none of the PETs specifies a {\tt factor} argument the default
!   will be a factor of 1.
!
!   See the description of method {\tt ESMF\_ArrayRedistStore()} for
!   the definition of the Array based operation.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayBundleRedist()} on any pair of ArrayBundles that matches 
!   {\tt srcArrayBundle} and {\tt dstArrayBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em distributed} dimensions. However, the size, 
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!  
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [srcArrayBundle]
!     {\tt ESMF\_ArrayBundle} with source data.
!   \item [dstArrayBundle]
!     {\tt ESMF\_ArrayBundle} with destination data. The data in these Arrays
!     may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[ignoreUnmatchedIndicesFlag]}] 
!     If set to {.false.}, the {\em default}, source and destination side must
!     cover the identical index space, using precisely matching sequence
!     indices. If set to {.true.}, mismatching sequence indices between source
!     and destination side are silently ignored.
!     The size of this array argument must either be 1 or equal the number of
!     Arrays in the {\tt srcArrayBundle} and {\tt dstArrayBundle} arguments. In
!     the latter case, the handling of unmatched indices is specified for each
!     Array pair separately. If only one element is specified, it is
!     used for {\em all} Array pairs.
!   \item [{[srcToDstTransposeMap]}]
!     List with as many entries as there are dimensions in the Arrays in
!     {\tt srcArrayBundle}. Each
!     entry maps the corresponding source Array dimension against the 
!     specified destination Array dimension. Mixing of distributed and
!     undistributed dimensions is supported.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical), pointer   :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target    :: def_ignoreUnmatched(1)
    integer                       :: len_ignoreUnmatched
    type(ESMF_InterArray)         :: srcToDstTransposeMapArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Deal with srcToDstTransposeMap
    srcToDstTransposeMapArg = ESMF_InterArrayCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStoreNF(srcArrayBundle, dstArrayBundle, &
      routehandle, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcToDstTransposeMapArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterArrayDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleRedistStoreNF
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRemove()"
!BOP
! !IROUTINE: ESMF_ArrayBundleRemove - Remove Arrays from ArrayBundle
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleRemove(arraybundle, arrayNameList, &
      keywordEnforcer, multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)         :: arraybundle
    character(len=*),       intent(in)            :: arrayNameList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: multiflag
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Remove Array(s) by name from ArrayBundle. In the relaxed setting it is 
!   {\em not} an error if {\tt arrayNameList} contains names that are not 
!   found in {\tt arraybundle}.
!
!   \begin{description}
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} from which to remove items.
!   \item [arrayNameList]
!     List of items to remove.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple Arrays with the same name
!     to be removed from {\tt arraybundle}. For {\tt .false.}, items to be
!     removed must have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "remove"
!     where it is {\em not} an error if {\tt arrayNameList} contains item
!     names that are not found in {\tt arraybundle}. For {\tt .false.} this is 
!     treated as an error condition. 
!     Further, in {\tt multiflag=.false.} mode, the relaxed definition of
!     "remove" also covers the case where there are multiple items in
!     {\tt arraybundle} that match a single entry in {\tt arrayNameList}.
!     For {\tt relaxedflag=.false.} this is treated as an error condition.
!     The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical)            :: multiflagArg
    type(ESMF_Logical)            :: relaxedflagArg
    integer                       :: itemCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    itemCount = size(arrayNameList)

    if (present(multiflag)) then
      multiflagArg = multiflag
    else
      multiflagArg = ESMF_FALSE
    endif
    if (present(relaxedflag)) then
      relaxedflagArg = relaxedflag
    else
      relaxedflagArg = ESMF_FALSE
    endif
    
    ! Call into the C++ interface layer
    call c_ESMC_ArrayBundleRemove(arraybundle, arrayNameList, itemCount, &
      multiflagArg, relaxedflagArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_ArrayBundleRemove
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleReplace()"
!BOP
! !IROUTINE: ESMF_ArrayBundleReplace - Replace Arrays in ArrayBundle
!
! !INTERFACE:
    subroutine ESMF_ArrayBundleReplace(arraybundle, arrayList, &
      keywordEnforcer, multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)         :: arraybundle
    type(ESMF_Array),       intent(in)            :: arrayList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: multiflag
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Replace Array(s) by name in ArrayBundle. In the relaxed setting it is not
!   an error if {\tt arrayList} contains Arrays that do not match by name any
!   item in {\tt arraybundle}. These Arrays are simply ignored in this case.
!
!   \begin{description}
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} in which to replace items.
!   \item [arrayList]
!     List of items to replace.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be replaced in {\tt arraybundle}. For {\tt .false.}, items to be
!     replaced must have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "replace"
!     where it is {\em not} an error if {\tt arrayList} contains items with
!     names that are not found in {\tt arraybundle}. These items in 
!     {\tt arrayList} are ignored in the relaxed mode. For {\tt .false.} this
!     is treated as an error condition.
!     Further, in {\tt multiflag=.false.} mode, the relaxed definition of
!     "replace" also covers the case where there are multiple items in
!     {\tt arraybundle} that match a single entry by name in {\tt arrayList}.
!     For {\tt relaxedflag=.false.} this is treated as an error condition.
!     The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical)            :: multiflagArg
    type(ESMF_Logical)            :: relaxedflagArg
    integer :: arrayCount, i
    type(ESMF_Pointer), allocatable :: arrayPointerList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Determine the number of ArrayList elements
    arrayCount = size(arrayList)

    ! Check init status of array arguments
    do i=1, arrayCount
      ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit, arrayList(i), rc)
    enddo
    
    ! Copy C++ pointers of deep objects into a simple ESMF_Pointer array
    ! This is necessary in order to strip off the F90 init check members
    ! when passing into C++
    allocate(arrayPointerList(arrayCount))
    do i=1, arrayCount
      call ESMF_ArrayGetThis(arrayList(i), arrayPointerList(i), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    if (present(multiflag)) then
      multiflagArg = multiflag
    else
      multiflagArg = ESMF_FALSE
    endif
    if (present(relaxedflag)) then
      relaxedflagArg = relaxedflag
    else
      relaxedflagArg = ESMF_FALSE
    endif
    
    ! Call into the C++ interface layer
    call c_ESMC_ArrayBundleReplace(arraybundle, arrayPointerList, arrayCount, &
      multiflagArg, relaxedflagArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Garbage collection
    deallocate(arrayPointerList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_ArrayBundleReplace
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSMM()"
!BOP
! !IROUTINE: ESMF_ArrayBundleSMM - Execute an ArrayBundle sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_ArrayBundleSMM(srcArrayBundle, dstArrayBundle, &
    routehandle, keywordEnforcer, &
    zeroregion, & ! DEPRECATED ARGUMENT
    zeroregionflag, termorderflag, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),    intent(in),         optional :: srcArrayBundle
    type(ESMF_ArrayBundle),    intent(inout),      optional :: dstArrayBundle
    type(ESMF_RouteHandle),    intent(inout)                :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Region_Flag),    intent(in), optional :: zeroregion ! DEPRECATED ARGUMENT
    type(ESMF_Region_Flag),    intent(in), target, optional :: zeroregionflag(:)
    type(ESMF_TermOrder_Flag), intent(in), target, optional :: termorderflag(:)
    logical,                   intent(in),         optional :: checkflag
    integer,                   intent(out),        optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt termorderflag}.
!              The new argument gives the user control over the order in which
!              the src terms are summed up.
! \item[8.1.0] Added argument {\tt zeroregionflag}, and deprecated
!              {\tt zeroregion}. The new argument allows greater flexibility
!              in setting the zero region for individual ArrayBundle members.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Execute a precomputed ArrayBundle sparse matrix multiplication from the
!   Arrays in {\tt srcArrayBundle} to the Arrays in {\tt dstArrayBundle}.
!
!   The {\tt srcArrayBundle} and {\tt dstArrayBundle} arguments are optional in
!   support of the situation where {\tt srcArrayBundle} and/or
!   {\tt dstArrayBundle} are not defined on all PETs. The {\tt srcArrayBundle}
!   and {\tt dstArrayBundle} must be specified on those PETs that hold source
!   or destination DEs, respectively, but may be omitted on all other PETs.
!   PETs that hold neither source nor destination DEs may omit both arguments.
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
!   \item [{[zeroregion]}] 
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of 
!     all DEs in all Arrays in {\tt dstArrayBundle} will be initialized to zero 
!     before updating the elements with the results of the sparse matrix 
!     multiplication. If set to {\tt ESMF\_REGION\_EMPTY} the elements in the
!     Arrays in {\tt dstArrayBundle} will not be modified prior to the sparse
!     matrix multiplication and results will be added to the incoming element
!     values. Setting {\tt zeroregion} to {\tt ESMF\_REGION\_SELECT} will only
!     zero out those elements in the destination Arrays that will be updated
!     by the sparse matrix multiplication. See section \ref{const:region}
!     for a complete list of valid settings.
!   \item [{[zeroregionflag]}] 
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of 
!     all DEs in the destination Array will be initialized to zero 
!     before updating the elements with the results of the sparse matrix 
!     multiplication. If set to {\tt ESMF\_REGION\_EMPTY} the elements in the
!     destination Array will not be modified prior to the sparse
!     matrix multiplication and results will be added to the incoming element
!     values. A setting of {\tt ESMF\_REGION\_SELECT} will only
!     zero out those elements in the destination Array that will be updated
!     by the sparse matrix multiplication. See section \ref{const:region}
!     for a complete list of valid settings.
!     The size of this array argument must either be 1 or equal the number of
!     Arrays in the {\tt srcArrayBundle} and {\tt dstArrayBundle} arguments. In
!     the latter case, the zero region for each Array SMM operation is
!     indicated separately. If only one zero region element is specified, it is
!     used for {\em all} Array pairs.
!   \item [{[termorderflag]}]
!     Specifies the order of the source side terms in all of the destination
!     sums. The {\tt termorderflag} only affects the order of terms during 
!     the execution of the RouteHandle. See the \ref{RH:bfb} section for an
!     in-depth discussion of {\em all} bit-for-bit reproducibility
!     aspects related to route-based communication methods.
!     See \ref{const:termorderflag} for a full list of options.
!     The size of this array argument must either be 1 or equal the number of
!     Arrays in the {\tt srcArrayBundle} and {\tt dstArrayBundle} arguments. In
!     the latter case, the term order for each Array SMM operation is
!     indicated separately. If only one term order element is specified, it is
!     used for {\em all} Array pairs.
!     The default is {\tt (/ESMF\_TERMORDER\_FREE/)}, allowing maximum 
!     flexibility in the order of terms for optimum performance.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input Array pairs will be checked for
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
    integer                   :: localrc      ! local return code
    type(ESMF_ArrayBundle)    :: opt_srcArrayBundle ! helper variable
    type(ESMF_ArrayBundle)    :: opt_dstArrayBundle ! helper variable
    type(ESMF_Region_Flag), pointer    :: opt_zeroregion(:) ! helper variable
    type(ESMF_Region_Flag), target     :: def_zeroregion(1) ! helper variable
    type(ESMF_TermOrder_Flag), pointer :: opt_termorder(:)  ! helper variable
    type(ESMF_TermOrder_Flag), target  :: def_termorder(1)  ! helper variable
    type(ESMF_Logical)        :: opt_checkflag      ! helper variable
    integer                   :: zeroRegionFlag_len ! helper variable
    integer                   :: termOrderFlag_len  ! helper variable
    
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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(dstArrayBundle)) then
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
      opt_dstArrayBundle = dstArrayBundle
    else
      call ESMF_ArrayBundleSetThisNull(opt_dstArrayBundle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Deal with optional arguments and defaults
    ! - zeroregionflag
    def_zeroregion = ESMF_REGION_TOTAL
    if (present(zeroregionflag)) then
      if (size(zeroregionflag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'zeroregionflag' argument must not be zero.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      opt_zeroregion => zeroregionflag
    else
      opt_zeroregion => def_zeroregion
    endif
    zeroRegionFlag_len = size(opt_zeroregion)
    ! - termorderflag
    def_termorder = ESMF_TERMORDER_FREE
    if (present(termorderflag)) then
      if (size(termorderflag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'termorderflag' argument must not be zero.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      opt_termorder => termorderflag
    else
      opt_termorder => def_termorder
    endif
    termOrderFlag_len = size(opt_termorder)
    ! - checkflag
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
    
    !TODO: remove this block once 'zeroregion' has been removed from API
    ! Handle deprecated 'zeroregion' argument while still supported
    if (present(zeroregion)) then
      if (present(zeroregionflag)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
          msg="The 'zeroregion' and 'zeroregionflag' arguments must not both"//&
          " be specified.", ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      def_zeroregion = zeroregion
    endif
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMM(opt_srcArrayBundle, opt_dstArrayBundle,&
      routehandle, opt_zeroregion(1), zeroRegionFlag_len, opt_termorder(1), &
      termOrderFlag_len, opt_checkflag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSMM
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSMMRelease()"
!BOP
! !IROUTINE: ESMF_ArrayBundleSMMRelease - Release resources associated with ArrayBundle sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_ArrayBundleSMMRelease(routehandle, keywordEnforcer, &
    noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)          :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional :: noGarbage
    integer,                intent(out),  optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Release resources associated with an ArrayBundle sparse matrix multiplication. 
!   After this call {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
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
    call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSMMRelease
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ArrayBundleSMMStore - Precompute an ArrayBundle sparse matrix multiplication with local factors
!
! !INTERFACE:
! ! Private name; call using ESMF_ArrayBundleSMMStore()
! subroutine ESMF_ArrayBundleSMMStore<type><kind>(srcArrayBundle, &
!   dstArrayBundle, routehandle, factorList, factorIndexList, keywordEnforcer, &
!   ignoreUnmatchedIndicesFlag, srcTermProcessing, rc)
!
! !ARGUMENTS:
!   type(ESMF_ArrayBundle),           intent(in)    :: srcArrayBundle
!   type(ESMF_ArrayBundle),           intent(inout) :: dstArrayBundle
!   type(ESMF_RouteHandle),           intent(inout) :: routehandle
!   <type>(ESMF_KIND_<kind>), target, intent(in)    :: factorList(:)
!   integer,                          intent(in)    :: factorIndexList(:,:)
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   logical,                intent(in),    optional :: ignoreUnmatchedIndicesFlag(:)
!   integer,                intent(inout), optional :: srcTermProcessing(:)
!   integer,                intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.1.0r] Added argument {\tt srcTermProcessing}.
!              The new argument gives the user access to the tuning parameter
!              affecting the sparse matrix execution and bit-wise 
!              reproducibility.
! \item[8.1.0] Added argument {\tt ignoreUnmatchedIndicesFlag} to support cases
!    where the sparse matrix includes terms with source or destination sequence
!    indices not present in the source or destination array.
! \end{description}
! \end{itemize}
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
!   PETs that specify non-zero matrix coefficients must use
!   the <type><kind> overloaded interface and provide the {\tt factorList} and
!   {\tt factorIndexList} arguments. Providing {\tt factorList} and
!   {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
!   {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
!   PET does not provide matrix elements. Alternatively, PETs that do not 
!   provide matrix elements may also call into the overloaded interface
!   {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
!   
!   See the description of method {\tt ESMF\_ArraySMMStore()} for
!   the definition of the Array based operation.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayBundleSMM()} on any pair of ArrayBundles that matches 
!   {\tt srcArrayBundle} and {\tt dstArrayBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em distributed} dimensions. However, the size, 
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
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
!     {\tt ESMF\_ArrayBundle} with destination data. The data in these Arrays
!     may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [factorList]
!     List of non-zero coefficients.
!   \item [factorIndexList]
!     Pairs of sequence indices for the factors stored in {\tt factorList}.
!
!     \begin{sloppypar}
!     The second dimension of {\tt factorIndexList} steps through the list of
!     pairs, i.e. {\tt size(factorIndexList,2) == size(factorList)}. The first
!     dimension of {\tt factorIndexList} is either of size 2 or size 4.
!     \end{sloppypar}
!
!     In the {\em size 2 format} {\tt factorIndexList(1,:)} specifies the
!     sequence index of the source element in the source Array while
!     {\tt factorIndexList(2,:)} specifies the sequence index of the
!     destination element in the destination Array. For this format to be a
!     valid option source and destination Arrays must have matching number of
!     tensor elements (the product of the sizes of all Array tensor dimensions).
!     Under this condition an identity matrix can be applied within the space of
!     tensor elements for each sparse matrix factor.
!
!     \begin{sloppypar}
!     The {\em size 4 format} is more general and does not require a matching
!     tensor element count. Here the {\tt factorIndexList(1,:)} specifies the
!     sequence index while {\tt factorIndexList(2,:)} specifies the tensor
!     sequence index of the source element in the source Array. Further
!     {\tt factorIndexList(3,:)} specifies the sequence index and
!     {\tt factorIndexList(4,:)} specifies the tensor sequence index of the 
!     destination element in the destination Array.
!     \end{sloppypar}
!
!     See section \ref{Array:SparseMatMul} for details on the definition of 
!     Array {\em sequence indices} and {\em tensor sequence indices}.
!
!   \item [{[ignoreUnmatchedIndicesFlag]}] 
!     If set to {.false.}, the {\em default}, source and destination side must
!     cover all of the squence indices defined in the sparse matrix. An error
!     will be returned if a sequence index in the sparse matrix does not match
!     on either the source or destination side.
!     If set to {.true.}, mismatching sequence indices are silently ignored.
!     The size of this array argument must either be 1 or equal the number of
!     Arrays in the {\tt srcArrayBundle} and {\tt dstArrayBundle} arguments. In
!     the latter case, the handling of unmatched indices is specified for each
!     Array pair separately. If only one element is specified, it is
!     used for {\em all} Array pairs.
!
!   \item [{[srcTermProcessing]}]
!       Source term summing options for route handle creation. See
!       {\tt ESMF\_ArraySMMStore} documentation for a full parameter description.
!       Two forms may be provided. If a single element list is provided, this
!       integer value is applied across all bundle members. Otherwise, the list must
!       contain as many elements as there are bundle members. For the special case
!       of accessing the auto-tuned parameter (providing a negative integer value),
!       the list length must equal the bundle member count.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSMMStoreI4()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleSMMStore - Precompute an ArrayBundle sparse matrix multiplication with local factors
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSMMStore()
  subroutine ESMF_ArrayBundleSMMStoreI4(srcArrayBundle, dstArrayBundle, &
    routehandle, factorList, factorIndexList, keywordEnforcer, &
    ignoreUnmatchedIndicesFlag, srcTermProcessing, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),        intent(in)           :: srcArrayBundle
    type(ESMF_ArrayBundle),        intent(inout)        :: dstArrayBundle
    type(ESMF_RouteHandle),        intent(inout)        :: routehandle
    integer(ESMF_KIND_I4), target, intent(in)           :: factorList(:)
    integer,                       intent(in)           :: factorIndexList(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                    intent(in),    optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                    intent(inout), optional :: srcTermProcessing(:)
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                         :: localrc              ! local return code
    integer(ESMF_KIND_I4), pointer  :: opt_factorList(:)    ! helper variable
    integer                         :: len_factorList       ! helper variable
    type(ESMF_InterArray)           :: factorIndexListArg   ! helper variable
    type(ESMF_Logical), pointer     :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target      :: def_ignoreUnmatched(1)
    integer                         :: len_ignoreUnmatched
    type(ESMF_InterArray)           :: srcTermProcessingArg ! helper variable

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
      ESMF_InterArrayCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Wrap srcTermProcessing argument
    srcTermProcessingArg = &
      ESMF_InterArrayCreate(srcTermProcessing, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMMStore(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_I4, opt_factorList, len_factorList, &
      factorIndexListArg, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcTermProcessingArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterArrayDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSMMStoreI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSMMStoreI8()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleSMMStore - Precompute an ArrayBundle sparse matrix multiplication with local factors
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSMMStore()
  subroutine ESMF_ArrayBundleSMMStoreI8(srcArrayBundle, dstArrayBundle, &
    routehandle, factorList, factorIndexList, keywordEnforcer, &
    ignoreUnmatchedIndicesFlag, srcTermProcessing, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),        intent(in)           :: srcArrayBundle
    type(ESMF_ArrayBundle),        intent(inout)        :: dstArrayBundle
    type(ESMF_RouteHandle),        intent(inout)        :: routehandle
    integer(ESMF_KIND_I8), target, intent(in)           :: factorList(:)
    integer,                       intent(in)           :: factorIndexList(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                    intent(in),    optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                    intent(inout), optional :: srcTermProcessing(:)
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                         :: localrc              ! local return code
    integer(ESMF_KIND_I8), pointer  :: opt_factorList(:)    ! helper variable
    integer                         :: len_factorList       ! helper variable
    type(ESMF_InterArray)           :: factorIndexListArg   ! helper variable
    type(ESMF_Logical), pointer     :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target      :: def_ignoreUnmatched(1)
    integer                         :: len_ignoreUnmatched
    type(ESMF_InterArray)           :: srcTermProcessingArg ! helper variable

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
      ESMF_InterArrayCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Wrap srcTermProcessing argument
    srcTermProcessingArg = &
      ESMF_InterArrayCreate(srcTermProcessing, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMMStore(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_I8, opt_factorList, len_factorList, &
      factorIndexListArg, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcTermProcessingArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterArrayDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSMMStoreI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSMMStoreR4()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleSMMStore - Precompute an ArrayBundle sparse matrix multiplication with local factors
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSMMStore()
  subroutine ESMF_ArrayBundleSMMStoreR4(srcArrayBundle, dstArrayBundle, &
    routehandle, factorList, factorIndexList, keywordEnforcer, &
    ignoreUnmatchedIndicesFlag, srcTermProcessing, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)             :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)          :: dstArrayBundle
    type(ESMF_RouteHandle),     intent(inout)          :: routehandle
    real(ESMF_KIND_R4), target, intent(in)             :: factorList(:)
    integer,                    intent(in)             :: factorIndexList(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                    intent(in),    optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                    intent(inout), optional :: srcTermProcessing(:)
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                         :: localrc              ! local return code
    real(ESMF_KIND_R4), pointer     :: opt_factorList(:)    ! helper variable
    integer                         :: len_factorList       ! helper variable
    type(ESMF_InterArray)           :: factorIndexListArg   ! helper variable
    type(ESMF_Logical), pointer     :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target      :: def_ignoreUnmatched(1)
    integer                         :: len_ignoreUnmatched
    type(ESMF_InterArray)           :: srcTermProcessingArg ! helper variable

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
      ESMF_InterArrayCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Wrap srcTermProcessing argument
    srcTermProcessingArg = &
      ESMF_InterArrayCreate(srcTermProcessing, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMMStore(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_R4, opt_factorList, len_factorList, &
      factorIndexListArg, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcTermProcessingArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterArrayDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSMMStoreR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSMMStoreR8()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleSMMStore - Precompute an ArrayBundle sparse matrix multiplication with local factors
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSMMStore()
  subroutine ESMF_ArrayBundleSMMStoreR8(srcArrayBundle, dstArrayBundle, &
    routehandle, factorList, factorIndexList, keywordEnforcer, &
    ignoreUnmatchedIndicesFlag, srcTermProcessing, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R8), target, intent(in)              :: factorList(:)
    integer,                    intent(in)              :: factorIndexList(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                    intent(in),    optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                    intent(inout), optional :: srcTermProcessing(:)
    integer,                    intent(out),   optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                         :: localrc              ! local return code
    real(ESMF_KIND_R8), pointer     :: opt_factorList(:)    ! helper variable
    integer                         :: len_factorList       ! helper variable
    type(ESMF_InterArray)           :: factorIndexListArg   ! helper variable
    type(ESMF_Logical), pointer     :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target      :: def_ignoreUnmatched(1)
    integer                         :: len_ignoreUnmatched
    type(ESMF_InterArray)           :: srcTermProcessingArg ! helper variable

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
      ESMF_InterArrayCreate(farray2D=factorIndexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Wrap srcTermProcessing argument
    srcTermProcessingArg = &
      ESMF_InterArrayCreate(srcTermProcessing, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMMStore(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_R8, opt_factorList, len_factorList, &
      factorIndexListArg, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcTermProcessingArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterArrayDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSMMStoreR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSMMStoreNF()"
!BOP
! !IROUTINE: ESMF_ArrayBundleSMMStore - Precompute an ArrayBundle sparse matrix multiplication without local factors
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleSMMStore()
  subroutine ESMF_ArrayBundleSMMStoreNF(srcArrayBundle, dstArrayBundle, &
    routehandle, keywordEnforcer, ignoreUnmatchedIndicesFlag, srcTermProcessing, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),  intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),  intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle),  intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                 intent(in),    optional :: ignoreUnmatchedIndicesFlag(:)
    integer,                 intent(inout), optional :: srcTermProcessing(:)
    integer,                 intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.1.0r] Added argument {\tt srcTermProcessing}.
!              The new argument gives the user access to the tuning parameter
!              affecting the sparse matrix execution and bit-wise 
!              reproducibility.
! \item[8.1.0] Added argument {\tt ignoreUnmatchedIndicesFlag} to support cases
!    where the sparse matrix includes terms with source or destination sequence
!    indices not present in the source or destination array.
! \end{description}
! \end{itemize}
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
!   PETs that specify non-zero matrix coefficients must use
!   the <type><kind> overloaded interface and provide the {\tt factorList} and
!   {\tt factorIndexList} arguments. Providing {\tt factorList} and
!   {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
!   {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
!   PET does not provide matrix elements. Alternatively, PETs that do not 
!   provide matrix elements may also call into the overloaded interface
!   {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
!   
!   See the description of method {\tt ESMF\_ArraySMMStore()} for
!   the definition of the Array based operation.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayBundleSMM()} on any pair of ArrayBundles that matches 
!   {\tt srcArrayBundle} and {\tt dstArrayBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em distributed} dimensions. However, the size, 
!   number, and index order of {\em undistributed} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!  
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [srcArrayBundle]
!     {\tt ESMF\_ArrayBundle} with source data.
!   \item [dstArrayBundle]
!     {\tt ESMF\_ArrayBundle} with destination data. The data in these Arrays
!     may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!
!   \item [{[ignoreUnmatchedIndicesFlag]}] 
!     If set to {.false.}, the {\em default}, source and destination side must
!     cover all of the squence indices defined in the sparse matrix. An error
!     will be returned if a sequence index in the sparse matrix does not match
!     on either the source or destination side.
!     If set to {.true.}, mismatching sequence indices are silently ignored.
!     The size of this array argument must either be 1 or equal the number of
!     Arrays in the {\tt srcArrayBundle} and {\tt dstArrayBundle} arguments. In
!     the latter case, the handling of unmatched indices is specified for each
!     Array pair separately. If only one element is specified, it is
!     used for {\em all} Array pairs.
!
!   \item [{[srcTermProcessing]}]
!       Source term summing options for route handle creation. See
!       {\tt ESMF\_ArraySMMStore} documentation for a full parameter description.
!       Two forms may be provided. If a single element list is provided, this
!       integer value is applied across all bundle members. Otherwise, the list must
!       contain as many elements as there are bundle members. For the special case
!       of accessing the auto-tuned parameter (providing a negative integer value),
!       the list length must equal the bundle member count.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                         :: localrc              ! local return code
    type(ESMF_Logical), pointer     :: opt_ignoreUnmatched(:)
    type(ESMF_Logical), target      :: def_ignoreUnmatched(1)
    integer                         :: len_ignoreUnmatched
    type(ESMF_InterArray)           :: srcTermProcessingArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with ignoreUnmatchedIndicesFlag
    def_ignoreUnmatched(1) = .false.
    if (present(ignoreUnmatchedIndicesFlag)) then
      if (size(ignoreUnmatchedIndicesFlag)==0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="Size of 'ignoreUnmatchedIndicesFlag' argument must not be zero.",&
          ESMF_CONTEXT, rcToReturn=rc)
        return ! bail out
      endif
      allocate(opt_ignoreUnmatched(size(ignoreUnmatchedIndicesFlag)))
      opt_ignoreUnmatched(:) = ignoreUnmatchedIndicesFlag(:)
    else
      opt_ignoreUnmatched => def_ignoreUnmatched
    endif
    len_ignoreUnmatched = size(opt_ignoreUnmatched)

    ! Wrap srcTermProcessing argument
    srcTermProcessingArg = &
      ESMF_InterArrayCreate(srcTermProcessing, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMMStoreNF(srcArrayBundle, dstArrayBundle, &
      routehandle, opt_ignoreUnmatched(1), len_ignoreUnmatched, &
      srcTermProcessingArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    if (present(ignoreUnmatchedIndicesFlag)) then
      deallocate(opt_ignoreUnmatched)
    endif

    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleSMMStoreNF
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleValidate()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleValidate - Validate ArrayBundle internals

! !INTERFACE:
  subroutine ESMF_ArrayBundleValidate(arraybundle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)              :: arraybundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional  :: rc  
!
! !DESCRIPTION:
!   Validates that the {\tt ArrayBundle} is internally consistent.
!   The method returns an error code if problems are found.  
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle] 
!     Specified {\tt ESMF\_ArrayBundle} object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Call into the C++ interface layer
    !todo: call c_ESMC_ArrayBundleValidate(arraybundle, localrc)
    localrc = ESMF_SUCCESS  ! remove when todo is done.
    
    ! Use LogErr to handle return code
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArrayBundleValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleWrite()"
!BOP
! !IROUTINE: ESMF_ArrayBundleWrite - Write the Arrays into a file
! \label{api:ArrayBundleWrite}

! !INTERFACE:
  subroutine ESMF_ArrayBundleWrite(arraybundle, fileName, keywordEnforcer, &
    convention, purpose, singleFile, overwrite, status, timeslice, iofmt, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: arraybundle
    character(*),               intent(in)              :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*),               intent(in),  optional  :: convention
    character(*),               intent(in),  optional  :: purpose
    logical,                    intent(in),  optional  :: singleFile
    logical,                    intent(in),  optional  :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional  :: status
    integer,                    intent(in),  optional  :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional  :: iofmt
    integer,                    intent(out), optional  :: rc  
!
! !DESCRIPTION:
!   Write the Arrays into a file. For this API to be functional,
!   the environment variable {\tt ESMF\_PIO} should be set to "internal"
!   when the ESMF library is built. Please see the section on 
!   Data I/O,~\ref{io:dataio}.
!
!   When {\tt convention} and {\tt purpose} arguments are specified, NetCDF dimension
!   labels and variable attributes are written from each Array in the ArrayBundle
!   from the corresponding Attribute package. Additionally, Attributes may be
!   set on the ArrayBundle level under the same Attribute package.  This allows
!   the specification of global attributes within the file.
!   As with individual Arrays, the value associated with each name may be either
!   a scalar character string, or a scalar or array of type integer, real, or
!   double precision.
!
!   Limitations:
!   \begin{itemize}
!     \item Only single tile Arrays are supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle] 
!     An {\tt ESMF\_ArrayBundle} object.
!   \item[fileName]
!     The name of the output file to which array bundle data is written.
!   \item[{[convention]}]
!     Specifies an Attribute package associated with the ArrayBundle, and the
!     contained Arrays, used to create NetCDF dimension labels and attributes
!     in the file.  When this argument is present, the {\tt purpose} 
!     argument must also be present.  Use this argument only with a NetCDF
!     I/O format. If binary format is used, ESMF will return an error code.
!   \item[{[purpose]}]
!     Specifies an Attribute package associated with the ArrayBundle, and the
!     contained Arrays, used to create NetCDF dimension labels and attributes
!     in the file.  When this argument is present, the {\tt convention} 
!     argument must also be present.  Use this argument only with a NetCDF
!     I/O format. If binary format is used, ESMF will return an error code.
!   \item[{[singleFile]}]
!     A logical flag, the default is .true., i.e., all arrays in the bundle 
!     are written in one single file. If .false., each array will be written
!     in separate files; these files are numbered with the name based on the
!     argument "file". That is, a set of files are named: [file\_name]001,
!     [file\_name]002, [file\_name]003,...
!   \item[{[overwrite]}]
!    \begin{sloppypar}
!      A logical flag, the default is .false., i.e., existing Array data may
!      {\em not} be overwritten. If .true., the overwrite behavior depends
!      on the value of {\tt iofmt} as shown below:
!    \begin{description}
!    \item[{\tt iofmt} = {\tt ESMF\_IOFMT\_BIN}:]\ All data in the file will
!      be overwritten with each Arrays's data.
!    \item[{\tt iofmt} = {\tt ESMF\_IOFMT\_NETCDF, ESMF\_IOFMT\_NETCDF\_64BIT\_OFFSET}:]\ Only the
!      data corresponding to each Array's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!    \end{description}
!    \end{sloppypar}
!   \item[{[status]}]
!    \begin{sloppypar}
!    The file status. Please see Section~\ref{const:filestatusflag} for
!    the list of options. If not present, defaults to
!    {\tt ESMF\_FILESTATUS\_UNKNOWN}.
!    \end{sloppypar}
!   \item[{[timeslice]}]
!    \begin{sloppypar}
!    Some I/O formats (e.g. NetCDF) support the output of data in form of
!    time slices. The {\tt timeslice} argument provides access to this
!    capability. {\tt timeslice} must be positive. The behavior of this
!    option may depend on the setting of the {\tt overwrite} flag:
!    \begin{description}
!    \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!    less than the maximum time already in the file, the write will fail.
!    \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!    \end{description}
!    By default, i.e. by omitting the {\tt timeslice} argument, no
!    provisions for time slicing are made in the output file,
!    however, if the file already contains a time axis for the variable,
!    a timeslice one greater than the maximum will be written.
!    \end{sloppypar}
!   \item[{[iofmt]}]
!     \begin{sloppypar}
!    The I/O format.  Please see Section~\ref{opt:iofmtflag} for the list
!    of options. If not present, file names with a {\tt .bin} extension will
!    use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc} extension
!    will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                    :: localrc             ! local return code
    type(ESMF_Logical)         :: opt_singlefileflag  ! helper variable
    integer                    :: len_varName         ! helper variable
    type(ESMF_Logical)         :: opt_overwriteflag   ! helper variable
    type(ESMF_FileStatus_Flag) :: opt_status          ! helper variable
    type(ESMF_IOFmt_Flag)      :: opt_iofmt           ! helper variable
    integer                    :: file_ext_p

#ifdef ESMF_PIO
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)

    ! Attributes
    if (present (convention) .neqv. present (purpose)) then
      if (ESMF_LogFoundError (ESMF_RC_ARG_WRONG,  &
          msg='Both convention and purpose must be specified',  &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    ! Set default flags
    opt_singlefileflag = ESMF_TRUE
    if (present(singleFile)) opt_singlefileflag = singlefile

    opt_overwriteflag = ESMF_FALSE
    if (present(overwrite)) opt_overwriteflag = overwrite

    opt_status = ESMF_FILESTATUS_UNKNOWN
    if (present(status)) opt_status = status

    ! Set iofmt based on file name extension (if present)
    if (present (iofmt)) then
      opt_iofmt = iofmt
    else
      if (index (fileName, '.') > 0) then
        file_ext_p = index (fileName, '.', back=.true.)
        select case (fileName(file_ext_p:))
        case ('.nc')
          opt_iofmt = ESMF_IOFMT_NETCDF
        case ('.bin')
          opt_iofmt = ESMF_IOFMT_BIN
        case default
          opt_iofmt = ESMF_IOFMT_NETCDF
        end select
      else
        opt_iofmt = ESMF_IOFMT_NETCDF
      end if
    end if

    ! Call into the C++ interface, which will call IO object
    call c_esmc_arraybundlewrite(arraybundle, fileName,        &
        convention, purpose,                                   &
        opt_singlefileflag, opt_overwriteflag, opt_status,     &
        timeslice, opt_iofmt, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,         &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

#else
    ! Return indicating PIO not present
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT,                 &
        msg="ESMF must be compiled with PIO support to support I/O methods", &
        ESMF_CONTEXT, rcToReturn=rc)
#endif
 
  end subroutine ESMF_ArrayBundleWrite
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
!   Access deep object init code.
!
!   The arguments are:
!   \begin{description}
!   \item [arraybundle]
!     ArrayBundle object.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(arraybundle)) then
      ESMF_ArrayBundleGetInit = ESMF_INIT_GET(arraybundle)
    else
      ESMF_ArrayBundleGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_ArrayBundleGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_ArrayBundleSetInitCreated - Set ArrayBundle init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_ArrayBundleSetInitCreated(arraybundle, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)          :: arraybundle
    integer,                intent(out),  optional :: rc  
!
! !DESCRIPTION:
!   Set init code in ArrayBundle object to "CREATED".
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle] 
!     Specified {\tt ESMF\_ArrayBundle} object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
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
    type(ESMF_ArrayBundle), intent(inout)          :: arraybundle
    integer,                intent(out),  optional :: rc  
!
! !DESCRIPTION:
!   Set Array this member to ESMF_NULL_POINTER.
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle] 
!     Specified {\tt ESMF\_ArrayBundle} object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
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
