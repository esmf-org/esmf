! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Container.F90"
!==============================================================================
!
! ESMF Container Module
module ESMF_ContainerMod
!
!==============================================================================
!
! This file contains the Fortran wrapper code for the C++ implementation of
!  the Container class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_ContainerMod
!

!   Fortran API wrapper of C++ implemenation of Container
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_IOUtilMod

  use ESMF_FieldMod         ! ESMF Fortran-C++ interface helper
  use ESMF_FieldGetMod      ! ESMF FieldGet interfaces
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!     ! ESMF_Container
!
!------------------------------------------------------------------------------

  ! Fortran class type to hold pointer to C++ object
  type ESMF_Container
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Container
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-internal methods:
  public ESMF_ContainerAdd
  public ESMF_ContainerAddReplace
  public ESMF_ContainerClear
  public ESMF_ContainerCreate
  public ESMF_ContainerDestroy
  public ESMF_ContainerGet
  public ESMF_ContainerRemove
  public ESMF_ContainerReplace
  public ESMF_ContainerPrint

  public ESMF_ContainerGetInit

  public ESMF_ContainerGarbageOn
  public ESMF_ContainerGarbageOff
  public ESMF_ContainerGarbageClear
  public ESMF_ContainerGarbageGet
  
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


! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_ContainerAdd -- Generic interface

! !INTERFACE:
  interface ESMF_ContainerAdd

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ContainerAddFieldList

! !DESCRIPTION: 
!   Add item to Container.
!EOPI 
  end interface

! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_ContainerAddReplace -- Generic interface

! !INTERFACE:
  interface ESMF_ContainerAddReplace

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ContainerAddReplaceFL

! !DESCRIPTION: 
!   AddReplace item to/in Container.
!EOPI 
  end interface

! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_ContainerGet -- Generic interface

! !INTERFACE:
  interface ESMF_ContainerGet

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ContainerGetField
    module procedure ESMF_ContainerGetFieldList
    module procedure ESMF_ContainerGetFieldListAll

! !DESCRIPTION: 
!   Query Container.
!EOPI 
  end interface

! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_ContainerReplace -- Generic interface

! !INTERFACE:
  interface ESMF_ContainerReplace

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ContainerReplaceFieldList

! !DESCRIPTION: 
!   Replace item in Container.
!EOPI 
  end interface

! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_ContainerGarbageGet -- Generic interface

! !INTERFACE:
  interface ESMF_ContainerGarbageGet

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ContainerGarbageGetFL

! !DESCRIPTION: 
!   Query Container for garbage.
!EOPI 
  end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerAddFieldList()"
!BOPI
! !IROUTINE: ESMF_ContainerAdd - Add Fields to Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerAdd()
  subroutine ESMF_ContainerAddFieldList(container, itemList, keywordEnforcer, &
    multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)         :: container
    type(ESMF_Field),     intent(in)            :: itemList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,              intent(in),  optional :: multiflag
    logical,              intent(in),  optional :: relaxedflag
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Add elements to an {\tt ESMF\_Container} object.
!
!   This method defines garbage as those elements in {\tt itemList} that
!   cannot be added to the container because an element with the same name
!   already exists in the container. Garbage can only be generated in relaxed
!   mode.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be added to.
!   \item[itemList]
!     Items to be added.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be added to {\tt container}. For {\tt .false.}, added items must
!     have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     under {\tt multiflag=.false.} mode, where it is {\em not} an error if 
!     {\tt itemList} contains items with names that are also found in 
!     {\tt container}. The {\tt container} is left unchanged for these items. 
!     For {\tt .false.} this is treated as an error condition. 
!     The default setting is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                     :: localrc      ! local return code
    type(ESMF_Logical)          :: multiflagArg
    type(ESMF_Logical)          :: relaxedflagArg
    integer                     :: i, stat
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Pointer)          :: vector
    type(ESMF_Field)            :: field

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
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
    
    do i=1, size(itemList)
      ! Check init status of arguments
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldGetInit, itemList(i), rc)
      
      ! Get the name of the Field
      call ESMF_FieldGet(itemList(i), name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface layer
      field = itemList(i) ! makes object passing robust
      call c_ESMC_ContainerAdd(container, trim(name), field, &
        multiflagArg, relaxedflagArg, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerAddFieldList
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerAddReplaceFL()"
!BOPI
! !IROUTINE: ESMF_ContainerAddReplace - Conditionally add or replace Fields in Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerAddReplace()
  subroutine ESMF_ContainerAddReplaceFL(container, itemList, keywordEnforcer, &
    rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)         :: container
    type(ESMF_Field),     intent(in)            :: itemList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Elements in {\tt itemList} that do not match any items by name in 
!   {\tt container} are added to the Container. Elements in {\tt itemList}
!   that match by name items in {\tt container} replaced those items.
!
!   This method defines garbage as those elements in {\tt container} that
!   were replaced as a consequence of this operation.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be added to.
!   \item[itemList]
!     Elements to be added or used to replace items with.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                     :: localrc      ! local return code
    integer                     :: i, stat
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Pointer)          :: vector
    type(ESMF_Field)            :: field

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    do i=1, size(itemList)
      ! Check init status of arguments
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldGetInit, itemList(i), rc)
      
      ! Get the name of the Field
      call ESMF_FieldGet(itemList(i), name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! Call into the C++ interface layer
      field = itemList(i) ! makes object passing robust
      call c_ESMC_ContainerAddReplace(container, trim(name), field, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerAddReplaceFL
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerClear()"
!BOPI
! !IROUTINE: ESMF_ContainerClear - Clear Container object

! !INTERFACE:
  subroutine ESMF_ContainerClear(container, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out),  optional  :: rc
!         
! !DESCRIPTION:
!   Clear an {\tt ESMF\_Container} object, i.e. remove all items.
!
!   This method defines garbage as all the elements in the {\tt container}.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be cleared.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface layer
    call c_ESMC_ContainerClear(container, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerClear
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerCreate()"
!BOPI
! !IROUTINE: ESMF_ContainerCreate

! !INTERFACE:
  function ESMF_ContainerCreate(rc)
!
! !ARGUMENTS:
    integer,                    intent(out),  optional  :: rc
!     
! !RETURN VALUE:
    type(ESMF_Container) :: ESMF_ContainerCreate
!         
!
! !DESCRIPTION:
!   Create empty ESMF Container.
!
!   The arguments are:
!   \begin{description}
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Container)    :: container    ! new Container

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Mark this Container object as invalid
    container%this = ESMF_NULL_POINTER
    
    call c_ESMC_ContainerCreate(container, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_ContainerCreate = container 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_ContainerCreate)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_ContainerCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerDestroy()"
!BOPI
! !IROUTINE: ESMF_ContainerDestroy - Destroy Container object

! !INTERFACE:
  subroutine ESMF_ContainerDestroy(container, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out),  optional  :: rc
!         
! !DESCRIPTION:
!   Destroy an {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be destroyed.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface layer
    call c_ESMC_ContainerDestroy(container, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Mark this Container object as invalid
    container%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(container)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGetField()"
!BOPI
! !IROUTINE: ESMF_ContainerGet - Query scalar information about a specific itemName

! !INTERFACE:
  ! Private name; call using ESMF_ContainerGet()
  subroutine ESMF_ContainerGetField(container, itemName, keywordEnforcer, &
    item, itemCount, isPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
    character(len=*),     intent(in)            :: itemName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Field),     intent(out), optional :: item
    integer,              intent(out), optional :: itemCount
    logical,              intent(out), optional :: isPresent
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Get items from a {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be queried.
!   \item[itemName]
!     The name of the specified item.
!   \item[{[item]}]
!     Returned item.
!   \item [{[itemCount]}]
!     Number of items with {\tt itemName} in {\tt container}.
!   \item [{[isPresent]}]
!     Upon return indicates whether item with {\tt itemName} is contained in 
!     {\tt container}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    type(ESMF_Logical)            :: isPres

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    if (present(item)) then
      ! Call into the C++ interface
      call c_ESMC_ContainerGetField(container, trim(itemName), item, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    if (present(itemCount)) then
      ! Call into the C++ interface
      call c_ESMC_ContainerGetCount(container, trim(itemName), itemCount, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(isPresent)) then
      ! Call into the C++ interface
      call c_ESMC_ContainerGetIsPresent(container, trim(itemName), &
        isPres, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      isPresent = isPres
    endif
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGetField
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGetFieldList()"
!BOPI
! !IROUTINE: ESMF_ContainerGet - Access a list of items matching itemName

! !INTERFACE:
  ! Private name; call using ESMF_ContainerGet()
  subroutine ESMF_ContainerGetFieldList(container, itemName, itemList, &
    keywordEnforcer, itemorderflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Container),      intent(in)            :: container
    character(len=*),          intent(in)            :: itemName
    type(ESMF_Field),          pointer               :: itemList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_ItemOrder_Flag), intent(in),  optional :: itemorderflag
    integer,                   intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Get items from a {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be queried.
!   \item[itemName]
!     The name of the specified item.
!   \item[{[itemList]}]
!     List of items in {\tt container} that match {\tt itemName}. 
!     This argument has the pointer attribute.
!     If the argument comes into this call associated the memory 
!     allocation is not changed. Instead the size of the memory allocation is
!     checked against the total number of elements in the container, and if
!     sufficiently sized the container elements are returned in the provided
!     memory allocation. If the argument comes into this call unassociated,
!     memory will be allocated internally and filled with the container
!     elements. In the latter case the size of the returned {\tt itemList}
!     will be identical to the number of items in the container that matches
!     {\tt itemName} - even if that number is zero.
!     In both cases the returned {\tt itemList} will be associated. It is the
!     responsibility of the caller to deallocate the memory.
!   \item[{[itemorderflag]}]
!     Specifies the order of the returned container items in the {\tt itemList}.
!     The default is {\tt ESMF\_ITEMORDER\_ABC}.
!     See \ref{const:itemorderflag} for a full list of options.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                   :: localrc      ! local return code
    integer                   :: stat
    integer                   :: i, itemC
    type(ESMF_Pointer)        :: vector
    type(ESMF_Field)          :: field
    type(ESMF_ItemOrder_Flag) :: itemorderflagArg
    
    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Deal with optional itemorderflag argument
    itemorderflagArg = ESMF_ITEMORDER_ABC ! default
    if (present(itemorderflag)) &
      itemorderflagArg = itemorderflag
    
    ! Call into the C++ interface
    call c_ESMC_ContainerGetCount(container, trim(itemName), itemC, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    if (associated(itemList)) then
      if (size(itemList) < itemC) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="itemList is too small", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif
    else
      allocate(itemList(itemC), stat=stat)
      if (ESMF_LogFoundAllocError(stat, msg= "allocating itemList", &
        ESMF_CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    
    ! Call into the C++ interface to set up the vector on the C++ side
    call c_ESMC_ContainerGetVector(container, trim(itemName), vector, &
      itemorderflagArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    do i=0, itemC-1 ! C-style indexing, zero-based
      
      ! Call into the C++ interface to get item from vector
      call c_ESMC_ContainerGetVField(container, vector, i, field, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      itemList(i+1) = field ! makes object passing robust

    enddo
    
    ! release vector here
    ! Call into the C++ interface to release the vector on the C++ side
    call c_ESMC_ContainerReleaseVector(container, vector, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGetFieldList
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGetFieldListAll()"
!BOPI
! !IROUTINE: ESMF_ContainerGet - Query Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerGet()
  subroutine ESMF_ContainerGetFieldListAll(container, keywordEnforcer, &
    itemorderflag, itemList, itemCount, rc)
!
! !ARGUMENTS:
    type(ESMF_Container),      intent(in)            :: container
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_ItemOrder_Flag), intent(in),  optional :: itemorderflag
    type(ESMF_Field),          pointer,     optional :: itemList(:)
    integer,                   intent(out), optional :: itemCount
    integer,                   intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Get items from a {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be queried.
!   \item[{[itemorderflag]}]
!     Specifies the order of the returned container items in the {\tt itemList}.
!     The default is {\tt ESMF\_ITEMORDER\_ABC}.
!     See \ref{const:itemorderflag} for a full list of options.
!   \item[{[itemList]}]
!     List of items in {\tt container}. This argument has the pointer
!     attribute. If the argument comes into this call associated the memory 
!     allocation is not changed. Instead the size of the memory allocation is
!     checked against the total number of elements in the container, and if
!     sufficiently sized the container elements are returned in the provided
!     memory allocation. If the argument comes into this call unassociated,
!     memory will be allocated internally and filled with the container
!     elements. In the latter case the size of the returned {\tt itemList}
!     will be identical to the number of items in the container - even if that
!     number is zero.
!     In both cases the returned {\tt itemList} will be associated. It is the
!     responsibility of the caller to deallocate the memory.
!   \item[{[itemCount]}]
!     Number of items {\tt container}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                   :: localrc      ! local return code
    integer                   :: stat
    integer                   :: i, itemC
    type(ESMF_Pointer)        :: vector
    type(ESMF_Field)          :: field
    type(ESMF_ItemOrder_Flag) :: itemorderflagArg

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Deal with optional itemorderflag argument
    itemorderflagArg = ESMF_ITEMORDER_ABC ! default
    if (present(itemorderflag)) &
      itemorderflagArg = itemorderflag
    
    ! Call into the C++ interface
    call c_ESMC_ContainerGetCountAll(container, itemC, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    if (present(itemList)) then
      if (associated(itemList)) then
        if (size(itemList) < itemC) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg="itemList is too small", &
            ESMF_CONTEXT, rcToReturn=rc)
          return  ! bail out
        endif
      else
        allocate(itemList(itemC), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg= "allocating itemList", &
          ESMF_CONTEXT, rcToReturn=rc)) return ! bail out
      endif
      
      ! Call into the C++ interface to set up the vector on the C++ side
      call c_ESMC_ContainerGetVectorAll(container, vector, itemorderflagArg, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      do i=0, itemC-1 ! C-style indexing, zero-based
        
        ! Call into the C++ interface to get item from vector
        call c_ESMC_ContainerGetVField(container, vector, i, field, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        itemList(i+1) = field ! makes object passing robust

      enddo
      
      ! release vector here
      ! Call into the C++ interface to release the vector on the C++ side
      call c_ESMC_ContainerReleaseVector(container, vector, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
    endif
    
    if (present(itemCount)) then
      itemCount = itemC
    endif
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGetFieldListAll
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerRemove()"
!BOPI
! !IROUTINE: ESMF_ContainerRemove - Remove object from Container

! !INTERFACE:
  subroutine ESMF_ContainerRemove(container, itemNameList, keywordEnforcer, &
    multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
    character(len=*),     intent(in)            :: itemNameList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,              intent(in),  optional :: multiflag
    logical,              intent(in),  optional :: relaxedflag
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Remove items from a {\tt ESMF\_Container} object.
!
!   This method defines garbage as those elements that were removed from the
!   container as a result of this operation. Garbage can be generated in both
!   relaxed and strict mode.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be queried.
!   \item[itemNameList]
!     The names of the items to remove
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be removed from {\tt container}. For {\tt .false.}, items to be removed
!     must have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "remove"
!     where it is {\em not} an error if {\tt itemNameList} contains item
!     names that are not found in {\tt container}. For {\tt .false.} this is 
!     treated as an error condition. 
!     Further, in {\tt multiflag=.false.} mode, the relaxed definition of
!     "remove" also covers the case where there are multiple items in
!     {\tt container} that match a single entry in {\tt itemNameList}.
!     For {\tt relaxedflag=.false.} this is treated as an error condition.
!     The default setting is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                     :: localrc      ! local return code
    type(ESMF_Logical)          :: multiflagArg
    type(ESMF_Logical)          :: relaxedflagArg
    integer                     :: i
    character(len=ESMF_MAXSTR)  :: name

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
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
    
    do i=1, size(itemNameList)
      ! Call into the C++ interface layer
      call c_ESMC_ContainerRemove(container, trim(itemNameList(i)), &
        multiflagArg, relaxedflagArg, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerRemove
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerReplaceFieldList()"
!BOPI
! !IROUTINE: ESMF_ContainerReplace - Replace Fields in Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerReplace()
  subroutine ESMF_ContainerReplaceFieldList(container, itemList, &
    keywordEnforcer, multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)         :: container
    type(ESMF_Field),     intent(in)            :: itemList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,              intent(in),  optional :: multiflag
    logical,              intent(in),  optional :: relaxedflag
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Replace items in an {\tt ESMF\_Container} object.
!
!   This method defines garbage as those elements in {\tt container} that
!   were replaced as a consequence of this operation {\em and} elements in
!   {\tt itemList} that were not used for replacement (in relaxed mode).
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be added to.
!   \item[itemList]
!     Elements used to replace container items.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be replaced in {\tt container}. For {\tt .false.}, items to be replaced
!     must have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "replace"
!     where it is {\em not} an error if {\tt itemList} contains items with
!     names that are not found in {\tt container}. These items in 
!     {\tt itemList} are ignored in the relaxed mode. For {\tt .false.} this
!     is treated as an error condition.
!     Further, in {\tt multiflag=.false.} mode, the relaxed definition of
!     "replace" also covers the case where there are multiple items in
!     {\tt container} that match a single entry by name in {\tt itemList}.
!     For {\tt relaxedflag=.false.} this is treated as an error condition.
!     The default setting is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                     :: localrc      ! local return code
    type(ESMF_Logical)          :: multiflagArg
    type(ESMF_Logical)          :: relaxedflagArg
    integer                     :: i, stat
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Pointer)          :: vector
    type(ESMF_Field)            :: field

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
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
    
    do i=1, size(itemList)
      ! Check init status of arguments
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldGetInit, itemList(i), rc)
      
      ! Get the name of the Field
      call ESMF_FieldGet(itemList(i), name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! Call into the C++ interface layer
      field = itemList(i) ! makes object passing robust
      call c_ESMC_ContainerReplace(container, trim(name), field, &
        multiflagArg, relaxedflagArg, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerReplaceFieldList
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerPrint()"
!BOPI
! !IROUTINE: ESMF_ContainerPrint - Print Container object

! !INTERFACE:
  subroutine ESMF_ContainerPrint(container, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out),  optional  :: rc
!         
! !DESCRIPTION:
!   Print an {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be printed.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface layer
    call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_ContainerPrint(container, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGetInit"
!BOPI
! !IROUTINE: ESMF_ContainerGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_ContainerGetInit(container) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_ContainerGetInit   
!
! !ARGUMENTS:
      type(ESMF_Container), intent(in), optional :: container
!
! !DESCRIPTION:
!   Access deep object init code.
!
!   The arguments are:
!   \begin{description}
!   \item [{[container]}]
!     Container object.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(container)) then
      ESMF_ContainerGetInit = ESMF_INIT_GET(container)
    else
      ESMF_ContainerGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_ContainerGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGarbageOn()"
!BOPI
! !IROUTINE: ESMF_ContainerGarbageOn - Turn on garbage feature in container

! !INTERFACE:
  subroutine ESMF_ContainerGarbageOn(container, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out),  optional  :: rc
!         
! !DESCRIPTION:
!   Turn on garbage feature in an {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to turn on garbage feature.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface layer
    call c_ESMC_ContainerGarbageOn(container, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGarbageOn
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGarbageOff()"
!BOPI
! !IROUTINE: ESMF_ContainerGarbageOff - Turn off garbage feature in container

! !INTERFACE:
  subroutine ESMF_ContainerGarbageOff(container, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out),  optional  :: rc
!         
! !DESCRIPTION:
!   Turn off garbage feature in an {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to turn off garbage feature.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface layer
    call c_ESMC_ContainerGarbageOff(container, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGarbageOff
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGarbageClear()"
!BOPI
! !IROUTINE: ESMF_ContainerGarbageClear - Clear garbage in container

! !INTERFACE:
  subroutine ESMF_ContainerGarbageClear(container, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out),  optional  :: rc
!         
! !DESCRIPTION:
!   Clear garbage in an {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to clear garbage for.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface layer
    call c_ESMC_ContainerGarbageClear(container, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGarbageClear
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGarbageGetFL()"
!BOPI
! !IROUTINE: ESMF_ContainerGarbageGet - Query Container object about Field garbage

! !INTERFACE:
  ! Private name; call using ESMF_ContainerGarbageGet()
  subroutine ESMF_ContainerGarbageGetFL(container, keywordEnforcer, &
    garbageList, garbageCount, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Field),     pointer,     optional :: garbageList(:)
    integer,              intent(out), optional :: garbageCount
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Get items from a {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be queried.
!   \item[{[garbageCount]}]
!     Number of objects in {\tt container} garbage.
!   \item[{[garbageList]}]
!     List of objects in {\tt container} garbage. This argument has the pointer
!     attribute. If the argument comes into this call associated the memory 
!     allocation is not changed. Instead the size of the memory allocation is
!     checked against the total number of elements in the container gargbage,
!     and if sufficiently sized the container garbage elements are returned in
!     the provided memory allocation. If the argument comes into this call
!     unassociated, memory will be allocated internally and filled with the
!     container garbage elements. In the latter case the size of the returned
!     {\tt garbageList} will be identical to the number of items in the
!     container garbage - even if that number is zero.
!     In both cases the returned {\tt garbageList} will be associated. It is the
!     responsibility of the caller to deallocate the memory.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: stat
    integer                       :: i, garbageC
    type(ESMF_Pointer)            :: vector
    type(ESMF_Field)              :: field

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface
    call c_ESMC_ContainerGarbageCount(container, garbageC, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    if (present(garbageList)) then
      if (associated(garbageList)) then
        if (size(garbageList) < garbageC) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg="garbageList is too small", &
            ESMF_CONTEXT, rcToReturn=rc)
          return  ! bail out
        endif
      else
        allocate(garbageList(garbageC), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg= "allocating garbageList", &
          ESMF_CONTEXT, rcToReturn=rc)) return ! bail out
      endif
      

      ! Call into the C++ interface to set up the vector on the C++ side
      call c_ESMC_ContainerGarbageGet(container, vector, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      do i=0, garbageC-1 ! C-style indexing, zero-based
        
        ! Call into the C++ interface to get item from vector
        call c_ESMC_ContainerGetVField(container, vector, i, field, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        garbageList(i+1) = field ! makes object passing robust

      enddo
      
      ! release vector here
      ! Call into the C++ interface to release the vector on the C++ side
      call c_ESMC_ContainerReleaseVector(container, vector, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
    endif
    
    if (present(garbageCount)) then
      garbageCount = garbageC
    endif
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGarbageGetFL
!------------------------------------------------------------------------------


end module ESMF_ContainerMod
