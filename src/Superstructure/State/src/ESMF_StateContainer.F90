! $Id: ESMF_StateContainer.F90,v 1.1 2011/05/11 16:40:41 theurich Exp $
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
#define ESMF_FILENAME "ESMF_StateContainer.F90"
!==============================================================================
!
! ESMF Container Module
module ESMF_StateContainerMod
!
!==============================================================================
!
! This file contains the StateItem specific overloads to the ESMF_Container API
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_StateContainerMod
!

!   Fortran API wrapper of C++ implemenation of Container
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling

  use ESMF_ContainerMod     ! ESMF Container
  use ESMF_StateItemMod     ! ESMF State types
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-internal methods:
  public ESMF_ContainerAdd
  public ESMF_ContainerAddReplace
  public ESMF_ContainerGet
  public ESMF_ContainerReplace

  public ESMF_ContainerGarbageGet  ! TODO first need to add generic interf. 

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_StateContainer.F90,v 1.1 2011/05/11 16:40:41 theurich Exp $'

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
    module procedure ESMF_ContainerAddSIL

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
    module procedure ESMF_ContainerAddReplaceSIL

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
    module procedure ESMF_ContainerGetSI
    module procedure ESMF_ContainerGetSIL

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
    module procedure ESMF_ContainerReplaceSIL

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
    module procedure ESMF_ContainerGarbageGetSIL

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
#define ESMF_METHOD "ESMF_ContainerAddSIL()"
!BOPI
! !IROUTINE: ESMF_ContainerAdd - Add StateItems to Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerAdd()
  subroutine ESMF_ContainerAddSIL(container, itemList, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)         :: container
    type(ESMF_StateItem), intent(in)            :: itemList(:)
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
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     where it is {\em not} an error if {\tt itemList} contains items
!     with names that are also found in {\tt container}. The {\tt container} 
!     is left unchanged for these items. For {\tt .false.} this is treated
!     as an error condition. The default setting is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                     :: localrc      ! local return code
    type(ESMF_Logical)          :: relaxedflagArg
    integer                     :: i, stat
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Pointer)          :: vector

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    if (present(relaxedflag)) then
      relaxedflagArg = relaxedflag
    else
      relaxedflagArg = ESMF_FALSE
    endif
    
    do i=1, size(itemList)
    
      ! Get the name of the item
      call ESMF_StateItemGet(itemList(i), name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! Call into the C++ interface layer
      call c_ESMC_ContainerAdd(container, trim(name), itemList(i), &
        relaxedflagArg, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerAddSIL
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerAddReplaceSIL()"
!BOPI
! !IROUTINE: ESMF_ContainerAddReplace - Conditionally add or replace StateItems in Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerAddReplace()
  subroutine ESMF_ContainerAddReplaceSIL(container, itemList, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)         :: container
    type(ESMF_StateItem), intent(in)            :: itemList(:)
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

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    do i=1, size(itemList)
    
      ! Get the name of the StateItems
      call ESMF_StateItemGet(itemList(i), name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! Call into the C++ interface layer
      call c_ESMC_ContainerAddReplace(container, trim(name), itemList(i), &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerAddReplaceSIL
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGetSI()"
!BOPI
! !IROUTINE: ESMF_ContainerGet - Query Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerGet()
  subroutine ESMF_ContainerGetSI(container, itemName, item, isPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
    character(len=*),     intent(in)            :: itemName
    type(ESMF_StateItem), intent(out)           :: item
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
!   \item[item]
!     Returned item.
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
    type(ESMF_Logical)            :: dummyIsPresent

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface
    call c_ESMC_ContainerGetSI(container, itemName, item, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    if (present(isPresent)) then
      ! Call into the C++ interface
      call c_ESMC_ContainerGetIsPresent(container, itemName, &
        dummyIsPresent, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      isPresent = dummyIsPresent
    endif
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGetSI
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGetSIL()"
!BOPI
! !IROUTINE: ESMF_ContainerGet - Query Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerGet()
  subroutine ESMF_ContainerGetSIL(container, itemCount, itemList, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
    integer,              intent(out), optional :: itemCount
    type(ESMF_StateItem), pointer               :: itemList(:)
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Get items from a {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be queried.
!   \item[{[itemCount]}]
!     Number of items {\tt container}.
!   \item[itemList]
!     List of items in {\tt container}. This argument has the pointer
!     attribute. If the argument comes into this call associated the memory 
!     allocation is not changed. Instead the size of the memory allocation is
!     checked against the total number of elements in the container, and if
!     sufficiently sized the container elements are returned in the provided
!     memory allocation. If the argument comes into this call unassociated,
!     memory will be allocated internally and filled with the container
!     elements. In both cases it is the caller responsibility to deallocate
!     the memory.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: stat
    integer                       :: i, itemC
    type(ESMF_Pointer)            :: vector

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface
    call c_ESMC_ContainerGetCount(container, itemC, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    if (associated(itemList)) then
      if (size(itemList) < itemC) then
        call ESMF_LogSetError(ESMF_RC_ARG_SIZE, &
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
    call c_ESMC_ContainerGetVector(container, vector, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    do i=0, itemC-1 ! C-style indexing, zero-based
        
      ! Call into the C++ interface to set up the vector on the C++ side
      call c_ESMC_ContainerGetVectorItem(container, vector, i, &
        itemList(i+1), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    enddo
      
    ! release vector here
    ! Call into the C++ interface to release the vector on the C++ side
    call c_ESMC_ContainerReleaseVector(container, vector, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    if (present(itemCount)) then
      itemCount = itemC
    endif
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGetSIL
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerReplaceSIL()"
!BOPI
! !IROUTINE: ESMF_ContainerReplace - Replace StateItems in Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerReplace()
  subroutine ESMF_ContainerReplaceSIL(container, itemList, relaxedflag, &
    rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)         :: container
    type(ESMF_StateItem), intent(in)            :: itemList(:)
    logical,              intent(in),  optional :: relaxedflag
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Replace items in an {\tt ESMF\_Container} object.
!
!   This method defines garbage as those elements in {\tt container} that
!   were replaced as a consequence of this call.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be added to.
!   \item[itemList]
!     Elements used to replace container items.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "replace"
!     where it is {\em not} an error if {\tt itemList} contains items with
!     names that are not found in {\tt container}. These items in 
!     {\tt itemList} are ignored in the relaxed mode. For {\tt .false.} this
!     is treated as an error condition. The default setting is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                     :: localrc      ! local return code
    type(ESMF_Logical)          :: relaxedflagArg
    integer                     :: i, stat
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Pointer)          :: vector

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    if (present(relaxedflag)) then
      relaxedflagArg = relaxedflag
    else
      relaxedflagArg = ESMF_FALSE
    endif
    
    do i=1, size(itemList)

      ! Get the name of the StateItem
      call ESMF_StateItemGet(itemList(i), name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! Call into the C++ interface layer
      call c_ESMC_ContainerReplace(container, trim(name), itemList(i), &
        relaxedflagArg, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerReplaceSIL
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGarbageGetSIL()"
!BOPI
! !IROUTINE: ESMF_ContainerGarbageGet - Query Container object about StateItem garbage

! !INTERFACE:
  ! Private name; call using ESMF_ContainerGarbageGet()
  subroutine ESMF_ContainerGarbageGetSIL(container, garbageCount, &
    garbageList, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
    integer,              intent(out), optional :: garbageCount
    type(ESMF_StateItem), pointer               :: garbageList(:)
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
!   \item[garbageList]
!     List of objects in {\tt container} garbage. This argument has the pointer
!     attribute. If the argument comes into this call associated the memory 
!     allocation is not changed. Instead the size of the memory allocation is
!     checked against the total number of elements in the container gargbage,
!     and if sufficiently sized the container garbage elements are returned in
!     the provided memory allocation. If the argument comes into this call
!     unassociated, memory will be allocated internally and filled with the
!     container garbage elements. In both cases it is the caller responsibility
!     to deallocate the memory.
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

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface
    call c_ESMC_ContainerGarbageCount(container, garbageC, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    if (associated(garbageList)) then
      if (size(garbageList) < garbageC) then
        call ESMF_LogSetError(ESMF_RC_ARG_SIZE, &
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
      
      ! Call into the C++ interface to set up the vector on the C++ side
      call c_ESMC_ContainerGetVectorItem(container, vector, i, &
        garbageList(i+1), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    enddo
    
    ! release vector here
    ! Call into the C++ interface to release the vector on the C++ side
    call c_ESMC_ContainerReleaseVector(container, vector, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    if (present(garbageCount)) then
      garbageCount = garbageC
    endif
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGarbageGetSIL
!------------------------------------------------------------------------------


end module ESMF_StateContainerMod
