! $Id: ESMF_ArrayBundle.F90,v 1.59 2011/05/05 17:49:42 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
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
  public operator(==)
  public operator(/=)

  public ESMF_ArrayBundleAdd
  public ESMF_ArrayBundleAddReplace
  public ESMF_ArrayBundleCreate
  public ESMF_ArrayBundleDestroy
  public ESMF_ArrayBundleGet
  public ESMF_ArrayBundleHalo
  public ESMF_ArrayBundleHaloRelease
  public ESMF_ArrayBundleHaloStore
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
    '$Id: ESMF_ArrayBundle.F90,v 1.59 2011/05/05 17:49:42 theurich Exp $'

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
    module procedure ESMF_ArrayBundleGetList
    module procedure ESMF_ArrayBundleGetItem
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
! \apiStatusCompatible
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
! \apiStatusCompatible
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
! \apiStatusCompatible
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
      relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)         :: arraybundle
    type(ESMF_Array),       intent(in)            :: arrayList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
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
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     where it is {\em not} an error if {\tt arrayList} contains Arrays with
!     names that are also found in {\tt arraybundle}. The {\tt arraybundle} 
!     is left unchanged for these Arrays. For {\tt .false.} this is treated
!     as an error condition. The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
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

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ArrayBundleAdd(arraybundle, arrayPointerList, arrayCount, &
      relaxedflagArg, localrc)
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
!
! !STATUS:
! \apiStatusCompatible
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

    ! Call into the C++ interface, which will sort out optional arguments.
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
  function ESMF_ArrayBundleCreate(keywordEnforcer, arrayList, name, rc)
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Array), intent(in),  optional :: arrayList(:)
    character (len=*),intent(in),  optional :: name
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
    type(ESMF_ArrayBundle) :: ESMF_ArrayBundleCreate
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
    integer :: arrayCount, i
    type(ESMF_Pointer), allocatable :: arrayPointerList(:)
    integer :: len_name
    type(ESMF_Logical) :: linkChange

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
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
    
    ! Copy C++ pointers of deep objects into a simple ESMF_Pointer array
    ! This is necessary in order to strip off the F90 init check members
    ! when passing into C++
    allocate(arrayPointerList(arrayCount))
    do i=1, arrayCount
      call ESMF_ArrayGetThis(arrayList(i), arrayPointerList(i), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    
    ! Mark this ArrayBundle object as invalid
    arraybundle%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments
    ! Optional name argument requires separate calls into C++
    if (present(name)) then
      len_name = len(name)
      call c_ESMC_ArrayBundleCreate(arraybundle, arrayPointerList, &
        arrayCount, name, len_name, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      len_name = 0
      call c_ESMC_ArrayBundleCreate(arraybundle, arrayPointerList, &
        arrayCount, "", len_name, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif    

    ! Garbage collection
    deallocate(arrayPointerList)

    ! link the Attribute hierarchies
    linkChange = ESMF_TRUE;
    do i=1,arrayCount
      call c_ESMC_AttributeLink(arraybundle, arrayList(i), linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc))  return
    enddo

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
! !IROUTINE: ESMF_ArrayBundleDestroy - Destroy an ArrayBundle

! !INTERFACE:
  subroutine ESMF_ArrayBundleDestroy(arraybundle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)           :: arraybundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional  :: rc  
!         
! !STATUS:
! \apiStatusCompatible
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
#define ESMF_METHOD "ESMF_ArrayBundleGetList()"
!BOP
! !IROUTINE: ESMF_ArrayBundleGet - Get info from ArrayBundle in form of lists
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayBundleGet()   
    subroutine ESMF_ArrayBundleGetList(arraybundle, keywordEnforcer, arrayCount, &
      arrayList, arrayNameList, name, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)             :: arraybundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional :: arrayCount
    type(ESMF_Array),       intent(out),  optional :: arrayList(:)
    character(len=*),       intent(out),  optional :: arrayNameList(:)
    character(len=*),       intent(out),  optional :: name
    integer,                intent(out),  optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Get the list of Arrays and Array names bundled in an ArrayBundle.
!
!   \begin{description}
!   \item [arraybundle]
!         {\tt ESMF\_ArrayBundle} to be queried.
!   \item [{[arrayCount]}]
!         Upon return holds the number of Arrays bundled in the ArrayBundle.
!   \item [{[arrayList]}]
!         Upon return holds a list of Arrays bundled in {\tt arraybundle}. The
!         argument must be allocated to be at least of size {\tt arrayCount}.
!   \item [{[arrayNameList]}]
!         Upon return holds a list of the names of the Array bundled in 
!         {\tt arraybundle}. The argument must be allocated to be at least of
!         size {\tt arrayCount}.
!   \item [{[name]}]
!         Name of the ArrayBundle object.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: opt_arrayCount         ! helper variable
    type(ESMF_Pointer), pointer   :: opt_arrayPtrList(:)    ! helper variable
    integer                       :: len_arrayPtrList       ! helper variable
    integer                       :: i                      ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
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

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ArrayBundleGetList(arraybundle, opt_arrayCount, &
      opt_arrayPtrList, len_arrayPtrList, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! fill in arrayCount output variable
    if (present(arrayCount)) then
      arrayCount = opt_arrayCount
    endif

    ! Set init code for deep C++ objects
    if (present(arrayList)) then
      do i=1, min(size(arrayList), opt_arrayCount)
        call ESMF_ArraySetThis(arrayList(i), opt_arrayPtrList(i), &
          rc=localrc)
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
  
  end subroutine ESMF_ArrayBundleGetList
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleGetItem()"
!BOP
! !IROUTINE: ESMF_ArrayBundleGet - Get info from ArrayBundle for single item
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayBundleGet()   
    subroutine ESMF_ArrayBundleGetItem(arraybundle, arrayName, &
      keywordEnforcer, array, isPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: arraybundle
    character(len=*),       intent(in)            :: arrayName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Array),       intent(out), optional :: array
    logical,                intent(out), optional :: isPresent
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Get an Array by name from ArrayBundle. It is an error if no Array called
!   {\tt arrayName} exists in {\tt arraybundle}.
!
!   \begin{description}
!   \item [arraybundle]
!         {\tt ESMF\_ArrayBundle} to be queried.
!   \item [arrayName]
!         Specific item by name.
!   \item [{[array]}]
!         Upon return holds the requested Array item.
!   \item [{[isPresent]}]
!         Upon return indicates whether Array item with {\tt arrayName} is
!         contained in {\tt arraybundle}.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
      call c_ESMC_ArrayBundleGetItem(arraybundle, arrayName, array, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code for deep C++ object
      call ESMF_ArraySetInitCreated(array, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    if (present(isPresent)) then
      ! Call into the C++ interface
      call c_ESMC_ArrayBundleGetIsPresent(arraybundle, arrayName, &
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
#define ESMF_METHOD "ESMF_ArrayBundleHalo()"
!BOP
! !IROUTINE: ESMF_ArrayBundleHalo - Execute an ArrayBundle halo operation
!
! !INTERFACE:
  subroutine ESMF_ArrayBundleHalo(arraybundle, routehandle, keywordEnforcer, &
    checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)           :: arraybundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional  :: checkflag
    integer,                intent(out),  optional  :: rc
!
! !STATUS:
! \apiStatusCompatible
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
  subroutine ESMF_ArrayBundleHaloRelease(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional  :: rc
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Release resouces associated with an ArrayBundle halo operation.
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
      keywordEnforcer, halostartregionflag, haloLDepth, haloUDepth, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)                :: arraybundle
    type(ESMF_RouteHandle), intent(inout)                :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HaloStartRegionFlag), intent(in), optional :: halostartregionflag
    integer,                intent(in),         optional :: haloLDepth(:)
    integer,                intent(in),         optional :: haloUDepth(:)
    integer,                intent(out),        optional :: rc
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Store an ArrayBundle halo operation over the data in {\tt arraybundle}. By 
!   default, i.e. without specifying {\tt halostartregionflag}, {\tt haloLDepth}
!   and {\tt haloUDepth}, all elements in the total Array regions that lie
!   outside the exclusive regions will be considered potential destination
!   elements for halo. However, only those elements that have a corresponding
!   halo source element, i.e. an exclusive element on one of the DEs, will be
!   updated under the halo operation. Elements that have no associated source
!   remain unchanged under halo.
!   \end{sloppypar}
!
!   Specifying {\tt halostartregionflag} allows to change the shape of the 
!   effective halo region from the inside. Setting this flag to
!   {\tt ESMF\_REGION\_COMPUTATIONAL} means that only elements outside 
!   the computational region for each Array are considered for potential
!   destination elements for halo. The default is {\tt ESMF\_REGION\_EXCLUSIVE}.
!
!   The {\tt haloLDepth} and {\tt haloUDepth} arguments allow to reduce
!   the extent of the effective halo region. Starting at the region specified
!   by {\tt halostartregionflag}, the {\tt haloLDepth} and {\tt haloUDepth}
!   define a halo depth in each direction. Note that the maximum halo region is
!   limited by the total region for each Array, independent of the actual
!   {\tt haloLDepth} and {\tt haloUDepth} setting. The total Array regions are
!   local DE specific. The {\tt haloLDepth} and {\tt haloUDepth} are interpreted
!   as the maximum desired extent, reducing the potentially larger region
!   available for halo.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_ArrayBundleHalo()} on any ArrayBundle that is weakly congruent
!   and typekind conform to {\tt arraybundle}. Congruency for ArrayBundles is
!   given by the congruency of its constituents.
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
!   \item [arraybundle]
!     {\tt ESMF\_ArrayBundle} containing data to be haloed. The data in the halo
!     regions may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[halostartregionflag]}]
!     \begin{sloppypar}
!     The start of the effective halo region on every DE. The default
!     setting is {\tt ESMF\_REGION\_EXCLUSIVE}, rendering all non-exclusive
!     elements potential halo destination elments.
!     See section \ref{opt:halostartregionflag} for a complete list of
!     valid settings.
!     \end{sloppypar}
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
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    ! Set default flags
    opt_halostartregionflag = ESMF_REGION_EXCLUSIVE
    if (present(halostartregionflag)) opt_halostartregionflag = halostartregionflag

    ! Deal with (optional) array arguments
    haloLDepthArg = ESMF_InterfaceIntCreate(haloLDepth, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    haloUDepthArg = ESMF_InterfaceIntCreate(haloUDepth, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleHaloStore(arraybundle, routehandle, &
      opt_halostartregionflag, haloLDepthArg, haloUDepthArg, localrc)
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
#define ESMF_METHOD "ESMF_ArrayBundlePrint()"
!BOP
! !IROUTINE: ESMF_ArrayBundlePrint - Print ArrayBundle internals

! !INTERFACE:
  subroutine ESMF_ArrayBundlePrint(arraybundle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)              :: arraybundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional  :: rc  
!         
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Print internal information of the specified {\tt ESMF\_ArrayBundle} object. \\
!
!   Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!   On some platforms/compilers there is a potential issue with interleaving
!   Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!   the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!   may be used on unit 6 to get coherent output.  \\
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
    
    ! Call into the C++ interface, which will sort out optional arguments.
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
  subroutine ESMF_ArrayBundleRead(arraybundle, file, keywordEnforcer, &
    singleFile, iofmt, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)             :: arraybundle
    character(*),           intent(in)             :: file
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional  :: singleFile
    type(ESMF_IOFmtFlag),   intent(in),  optional  :: iofmt
    integer,                intent(out), optional  :: rc
!         
!
!
! !DESCRIPTION:
!   Read Array data to an ArrayBundle object from file(s).
!   For this API to be functional, the environment variable {\tt ESMF\_PIO} 
!   should be set to "internal" when the ESMF library is built.
!   Please see the section on Data I/O,~\ref{io:dataio}.
!
!   Limitations:
!   \begin{itemize}
!     \item Only 1 DE per PET supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle] 
!     An {\tt ESMF\_ArrayBundle} object.
!   \item[file]
!     The name of the file from which ArrayBundle data is read.
!   \item[{[singleFile]}]
!     A logical flag, the default is .true., i.e., all Arrays in the bundle 
!     are stored in one single file. If .false., each Array is stored 
!     in separate files; these files are numbered with the name based on the
!     argument "file". That is, a set of files are named: [file\_name]001,
!     [file\_name]002, [file\_name]003,...
!   \item[{[iofmt]}]
!     \begin{sloppypar}
!     The IO format. Please see Section~\ref{opt:iofmtflag} for the list
!     of options.  If not present, defaults to {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    character(len=80), allocatable :: Aname(:)
    integer :: arrayCount,i
    type(ESMF_Array), allocatable :: arrayList(:)
    logical                       :: singlef
    character(len=80)             :: filename
    character(len=3)              :: cnum
    type(ESMF_IOFmtFlag)          :: iofmtd

#ifdef ESMF_PIO
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)

    ! Check options
    singlef = .true.
    if (present(singleFile)) singlef = singleFile
    iofmtd = ESMF_IOFMT_NETCDF   ! default format
    if(present(iofmt)) iofmtd = iofmt

    call ESMF_ArrayBundleGet(arraybundle, arrayCount=arrayCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    allocate (Aname(arrayCount))
    allocate (arrayList(arrayCount))
    call ESMF_ArrayBundleGet(arraybundle, arrayList=arrayList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    if (singlef) then
      ! Get and read the arrays in the Bundle
      do i=1,arrayCount
       call ESMF_ArrayGet(arrayList(i), name=Aname(i), rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_ArrayRead(arrayList(i), file=file, variableName=Aname(i), &
          iofmt=iofmtd, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    else
      do i=1,arrayCount
        write(cnum,"(i3.3)") i
        filename = file // cnum
        call ESMF_ArrayGet(arrayList(i), name=Aname(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayRead(arrayList(i), file=filename,  &
               variableName=Aname(i), iofmt=iofmtd, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

#else
    ! Return indicating PIO not present
    if (present(rc)) rc = ESMF_RC_LIB_NOT_PRESENT
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
    type(ESMF_ArrayBundle), intent(in),     optional  :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout),  optional  :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)             :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),     optional  :: checkflag
    integer,                intent(out),    optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed ArrayBundle redistribution from the Arrays in
!   {\tt srcArrayBundle} to the Arrays in {\tt dstArrayBundle}.
!   \end{sloppypar}
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
  subroutine ESMF_ArrayBundleRedistRelease(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Release resouces associated with an ArrayBundle redistribution.
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
!   dstArrayBundle, routehandle, factor, keywordEnforcer, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
!   type(ESMF_ArrayBundle),   intent(in)            :: srcArrayBundle
!   type(ESMF_ArrayBundle),   intent(inout)         :: dstArrayBundle
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!   <type>(ESMF_KIND_<kind>), intent(in)            :: factor
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                  intent(in),  optional :: srcToDstTransposeMap(:)
!   integer,                  intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
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
!   {\tt ESMF\_ArrayBundleRedist()} on any pair of ArrayBundles that 
!   are weakly congruent and typekind conform with the Arrays contained in
!   {\tt srcArrayBundle} and {\tt dstArrayBundle}. 
!   Congruent Arrays possess matching DistGrids, and the shape of the local
!   array tiles matches between the Arrays for every DE. For weakly congruent
!   Arrays the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Arrays that differ in the number of elements in the left most
!   undistributed dimensions.
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
!     Factor by which to multipy source data.
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
    routehandle, factor, keywordEnforcer, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer(ESMF_KIND_I4),  intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),   optional  :: srcToDstTransposeMap(:)
    integer,                intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, srcToDstTransposeMapArg, ESMF_TYPEKIND_I4, factor, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    routehandle, factor, keywordEnforcer, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    integer(ESMF_KIND_I8),  intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),   optional  :: srcToDstTransposeMap(:)
    integer,                intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, srcToDstTransposeMapArg, ESMF_TYPEKIND_I8, factor, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    routehandle, factor, keywordEnforcer, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    real(ESMF_KIND_R4),     intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),   optional  :: srcToDstTransposeMap(:)
    integer,                intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, srcToDstTransposeMapArg, ESMF_TYPEKIND_R4, factor, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    routehandle, factor, keywordEnforcer, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
    real(ESMF_KIND_R8),     intent(in)              :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),   optional  :: srcToDstTransposeMap(:)
    integer,                intent(out),  optional  :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStore(srcArrayBundle, dstArrayBundle, &
      routehandle, srcToDstTransposeMapArg, ESMF_TYPEKIND_R8, factor, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArrayBundleRedistStoreR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayBundleRedistStore()"
!BOP
! !IROUTINE: ESMF_ArrayBundleRedistStore - Precompute an ArrayBundle redistribution without local factor argument
!
! !INTERFACE:
  ! Private name; call using ESMF_ArrayBundleRedistStore()
  subroutine ESMF_ArrayBundleRedistStoreNF(srcArrayBundle, dstArrayBundle, &
    routehandle, keywordEnforcer, srcToDstTransposeMap, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)            :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout)         :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: srcToDstTransposeMap(:)
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
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
!   {\tt ESMF\_ArrayBundleRedist()} on any pair of ArrayBundles that 
!   are weakly congruent and typekind conform with the Arrays contained in
!   {\tt srcArrayBundle} and {\tt dstArrayBundle}. 
!   Congruent Arrays possess matching DistGrids, and the shape of the local
!   array tiles matches between the Arrays for every DE. For weakly congruent
!   Arrays the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Arrays that differ in the number of elements in the left most
!   undistributed dimensions.
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
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: srcToDstTransposeMapArg   ! index helper

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, srcArrayBundle, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, dstArrayBundle, rc)
    
    ! Deal with (optional) array arguments
    srcToDstTransposeMapArg = ESMF_InterfaceIntCreate(srcToDstTransposeMap, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleRedistStoreNF(srcArrayBundle, dstArrayBundle, &
      routehandle, srcToDstTransposeMapArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark routehandle object as being created
    call ESMF_RouteHandleSetInitCreated(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(srcToDstTransposeMapArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
      keywordEnforcer, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)         :: arraybundle
    character(len=*),       intent(in)            :: arrayNameList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
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
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "remove"
!     where it is {\em not} an error if {\tt arrayNameList} contains names
!     that are not found in {\tt arraybundle}. For {\tt .false.} this is treated
!     as an error condition. The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical)            :: relaxedflagArg
    integer                       :: itemCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)
    
    itemCount = size(arrayNameList)

    if (present(relaxedflag)) then
      relaxedflagArg = relaxedflag
    else
      relaxedflagArg = ESMF_FALSE
    endif
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ArrayBundleRemove(arraybundle, arrayNameList, itemCount, &
      relaxedflagArg, localrc)
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
      keywordEnforcer, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(inout)         :: arraybundle
    type(ESMF_Array),       intent(in)            :: arrayList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
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
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "replace"
!     where it is {\em not} an error if {\tt arrayList} contains items that
!     do not match by name any item in {\tt arraybundle}. For {\tt .false.}
!     this is an error condition. The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
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

    if (present(relaxedflag)) then
      relaxedflagArg = relaxedflag
    else
      relaxedflagArg = ESMF_FALSE
    endif
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_ArrayBundleReplace(arraybundle, arrayPointerList, arrayCount, &
      relaxedflagArg, localrc)
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
    routehandle, keywordEnforcer, zeroflag, checkflag, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in),    optional  :: srcArrayBundle
    type(ESMF_ArrayBundle), intent(inout), optional  :: dstArrayBundle
    type(ESMF_RouteHandle), intent(inout)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_RegionFlag),  intent(in),    optional  :: zeroflag
    logical,                intent(in),    optional  :: checkflag
    integer,                intent(out),   optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Execute a precomputed ArrayBundle sparse matrix multiplication from the
!   Arrays in {\tt srcArrayBundle} to the Arrays in {\tt dstArrayBundle}.
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
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of 
!     all DEs in all Arrays in {\tt dstArrayBundle} will be initialized to zero 
!     before updating the elements with the results of the sparse matrix 
!     multiplication. If set to {\tt ESMF\_REGION\_EMPTY} the elements in the
!     Arrays in {\tt dstArrayBundle} will not be modified prior to the sparse
!     matrix multiplication and results will be added to the incoming element
!     values. Setting {\tt zeroflag} to {\tt ESMF\_REGION\_SELECT} will only
!     zero out those elements in the destination Arrays that will be updated
!     by the sparse matrix multiplication. See section \ref{opt:regionflag}
!     for a complete list of valid settings.
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
    type(ESMF_RegionFlag)   :: opt_zeroflag ! helper variable
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
    opt_zeroflag = ESMF_REGION_TOTAL
    if (present(zeroflag)) opt_zeroflag = zeroflag
    opt_checkflag = ESMF_FALSE
    if (present(checkflag)) opt_checkflag = checkflag
        
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMM(opt_srcArrayBundle, opt_dstArrayBundle,&
      routehandle, opt_zeroflag, opt_checkflag, localrc)
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
  subroutine ESMF_ArrayBundleSMMRelease(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
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
!   dstArrayBundle, routehandle, factorList, factorIndexList, keywordEnforcer, rc)
!
! !ARGUMENTS:
!   type(ESMF_ArrayBundle),           intent(in)            :: srcArrayBundle
!   type(ESMF_ArrayBundle),           intent(inout)         :: dstArrayBundle
!   type(ESMF_RouteHandle),           intent(inout)         :: routehandle
!   <type>(ESMF_KIND_<kind>), target, intent(in)            :: factorList(:)
!   integer,                          intent(in)            :: factorIndexList(:,:)
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                          intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
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
!   {\tt ESMF\_ArrayBundleSMM()} on any pair of ArrayBundles that 
!   are weakly congruent and typekind conform with the Arrays contained in
!   {\tt srcArrayBundle} and {\tt dstArrayBundle}. 
!   Congruent Arrays possess matching DistGrids, and the shape of the local
!   array tiles matches between the Arrays for every DE. For weakly congruent
!   Arrays the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Arrays that differ in the number of elements in the left most
!   undistributed dimensions.
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
!     Under this condition an identiy matrix can be applied within the space of
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
    routehandle, factorList, factorIndexList, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer(ESMF_KIND_I4), target, intent(in)           :: factorList(:)
    integer,                    intent(in)              :: factorIndexList(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMMStore(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_I4, opt_factorList, len_factorList, &
      factorIndexListArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    routehandle, factorList, factorIndexList, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    integer(ESMF_KIND_I8), target, intent(in)           :: factorList(:)
    integer,                    intent(in)              :: factorIndexList(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMMStore(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_I8, opt_factorList, len_factorList, &
      factorIndexListArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    routehandle, factorList, factorIndexList, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R4), target, intent(in)              :: factorList(:)
    integer,                    intent(in)              :: factorIndexList(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMMStore(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_R4, opt_factorList, len_factorList, &
      factorIndexListArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    routehandle, factorList, factorIndexList, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
    real(ESMF_KIND_R8), target, intent(in)              :: factorList(:)
    integer,                    intent(in)              :: factorIndexList(:,:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_ArrayBundleSMMStore(srcArrayBundle, dstArrayBundle, &
      routehandle, ESMF_TYPEKIND_R8, opt_factorList, len_factorList, &
      factorIndexListArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(factorIndexListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),     intent(in)              :: srcArrayBundle
    type(ESMF_ArrayBundle),     intent(inout)           :: dstArrayBundle
    type(ESMF_RouteHandle),     intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(out),  optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
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
!   {\tt ESMF\_ArrayBundleSMM()} on any pair of ArrayBundles that 
!   are weakly congruent and typekind conform with the Arrays contained in
!   {\tt srcArrayBundle} and {\tt dstArrayBundle}. 
!   Congruent Arrays possess matching DistGrids, and the shape of the local
!   array tiles matches between the Arrays for every DE. For weakly congruent
!   Arrays the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Arrays that differ in the number of elements in the left most
!   undistributed dimensions.
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
    call c_ESMC_ArrayBundleSMMStoreNF(srcArrayBundle, dstArrayBundle, &
      routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
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
  subroutine ESMF_ArrayBundleWrite(arraybundle, file, keywordEnforcer, &
    singleFile, timeslice, iofmt, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)              :: arraybundle
    character(*),           intent(in)              :: file
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional  :: singleFile
    integer,                intent(in),   optional  :: timeslice
    type(ESMF_IOFmtFlag),   intent(in),   optional  :: iofmt
    integer,                intent(out),  optional  :: rc  
!         
!
!
! !DESCRIPTION:
!   Write the Arrays into a file. For this API to be functional,
!   the environment variable {\tt ESMF\_PIO} should be set to "internal"
!   when the ESMF library is built. Please see the section on 
!   Data I/O,~\ref{io:dataio}.
!
!   Limitations:
!   \begin{itemize}
!     \item Only 1 DE per PET supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[arraybundle] 
!     An {\tt ESMF\_ArrayBundle} object.
!   \item[file]
!     The name of the output file to which array bundle data is written.
!   \item[{[singleFile]}]
!     A logical flag, the default is .true., i.e., all arrays in the bundle 
!     are written in one single file. If .false., each array will be written
!     in separate files; these files are numbered with the name based on the
!     argument "file". That is, a set of files are named: [file\_name]001,
!     [file\_name]002, [file\_name]003,...
!   \item[{[timeslice]}]
!     Some IO formats (e.g. NetCDF) support the output of data in form of
!     time slices. The {\tt timeslice} argument provides access to this
!     capability. Usage of this feature requires that the first slice is
!     written with a positive {\tt timeslice} value, and that subsequent slices
!     are written with a {\tt timeslice} argument that increments by one each
!     time. By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file.
!   \item[{[iofmt]}]
!     \begin{sloppypar}
!     The IO format. Please see Section~\ref{opt:iofmtflag} for the list
!     of options.  If not present, defaults to {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    character(len=80), allocatable :: Aname(:)
    integer :: arrayCount,i,time
    type(ESMF_Array), allocatable :: arrayList(:)
    logical :: singlef
    character(len=80) :: filename
    character(len=3) :: cnum
    type(ESMF_IOFmtFlag)        :: iofmtd

#ifdef ESMF_PIO
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ArrayBundleGetInit, arraybundle, rc)

    ! Check options
    singlef = .true.
    if (present(singleFile)) singlef = singleFile
    iofmtd = ESMF_IOFMT_NETCDF   ! default format
    if(present(iofmt)) iofmtd = iofmt
    time = -1   ! default, no time dimension
    if (present(timeslice)) time = timeslice
    
    call ESMF_ArrayBundleGet(arraybundle, arrayCount=arrayCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    allocate (Aname(arrayCount))
    allocate (arrayList(arrayCount))
    call ESMF_ArrayBundleGet(arraybundle, arrayList=arrayList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    if (singlef) then
      ! Get and write the first array in the Bundle
      call ESMF_ArrayGet(arrayList(1), name=Aname(1), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_ArrayWrite(arrayList(1), file=file, timeslice=time, iofmt=iofmtd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      ! Get and write the rest of the arrays in the Bundle
      do i=2,arrayCount
       call ESMF_ArrayGet(arrayList(i), name=Aname(i), rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_ArrayWrite(arrayList(i), file=file, timeslice=time, &
         append=.true., iofmt=iofmtd, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    else
      do i=1,arrayCount
        write(cnum,"(i3.3)") i
        filename = file // cnum
        ! Get and write the first array in the Bundle
        call ESMF_ArrayGet(arrayList(i), name=Aname(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayWrite(arrayList(i), file=trim(filename),  &
           timeslice=time, iofmt=iofmtd, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

#else
    ! Return indicating PIO not present
    if (present(rc)) rc = ESMF_RC_LIB_NOT_PRESENT
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
