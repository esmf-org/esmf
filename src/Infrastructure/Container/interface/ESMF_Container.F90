! $Id: ESMF_Container.F90,v 1.10 2011/05/10 00:42:56 theurich Exp $
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
  sequence
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
  public ESMF_ContainerCreate
  public ESMF_ContainerDestroy
  public ESMF_ContainerGet
  public ESMF_ContainerRemove
  public ESMF_ContainerPrint

  public ESMF_ContainerGarbageOn
  public ESMF_ContainerGarbageOff
  public ESMF_ContainerGarbageClear
  public ESMF_ContainerGarbageGet

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Container.F90,v 1.10 2011/05/10 00:42:56 theurich Exp $'

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
! !IROUTINE: ESMF_ContainerGet -- Generic interface

! !INTERFACE:
  interface ESMF_ContainerGet

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ContainerGetField
    module procedure ESMF_ContainerGetFieldList

! !DESCRIPTION: 
!   Query Container.
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
  subroutine ESMF_ContainerAddFieldList(container, fieldList, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)         :: container
    type(ESMF_Field),     intent(in)            :: fieldList(:)
    logical,              intent(in),  optional :: relaxedflag
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Add Fields to an {\tt ESMF\_Container} object.
!
!   This method defines garbage as those elements in {\tt fieldList} that
!   cannot be added to the container because a Field with the same name already
!   exists in the container. Garbage can only be generated in relaxed mode.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be added to.
!   \item[fieldList]
!     Field objects to be added.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     where it is {\em not} an error if {\tt fieldList} contains Fields with
!     names that are also found in {\tt container}. The {\tt container} 
!     is left unchanged for these Fields. For {\tt .false.} this is treated
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
    
    do i=1, size(fieldList)
      ! Check init status of arguments
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldGetInit, fieldList(i), rc)
      
      ! Get the name of the Field
      call ESMF_FieldGet(fieldList(i), name=name, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! Call into the C++ interface layer
      call c_ESMC_ContainerAdd(container, trim(name), fieldList(i), &
        relaxedflagArg, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerAddFieldList
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
  subroutine ESMF_ContainerDestroy(container, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
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
! !IROUTINE: ESMF_ContainerGet - Query Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerGet()
  subroutine ESMF_ContainerGetField(container, fieldName, field, isPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
    character(len=*),     intent(in)            :: fieldName
    type(ESMF_Field),     intent(out), optional :: field
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
!   \item[fieldName]
!     The name of the specified Field object.
!   \item[{[field]}]
!     Returned Field object.
!   \item [{[isPresent]}]
!     Upon return indicates whether Field item with {\tt fieldName} is
!     contained in {\tt container}.
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
    
    if (present(field)) then
      ! Call into the C++ interface
      call c_ESMC_ContainerGetField(container, fieldName, field, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    if (present(isPresent)) then
      ! Call into the C++ interface
      call c_ESMC_ContainerGetIsPresent(container, fieldName, &
        dummyIsPresent, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      isPresent = dummyIsPresent
    endif
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGetField
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerGetFieldList()"
!BOPI
! !IROUTINE: ESMF_ContainerGet - Query Container object

! !INTERFACE:
  ! Private name; call using ESMF_ContainerGet()
  subroutine ESMF_ContainerGetFieldList(container, fieldCount, fieldList, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
    integer,              intent(out), optional :: fieldCount
    type(ESMF_Field),     pointer,     optional :: fieldList(:)
    integer,              intent(out), optional :: rc
!         
! !DESCRIPTION:
!   Get items from a {\tt ESMF\_Container} object.
!
!   The arguments are:
!   \begin{description}
!   \item[container]
!     {\tt ESMF\_Container} object to be queried.
!   \item[{[fieldCount]}]
!     Number of Field objects in {\tt container}.
!   \item[{[fieldList]}]
!     List of Field objects in {\tt container}. This argument has the pointer
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
    integer                       :: i, dummyFieldCount
    type(ESMF_Pointer)            :: vector

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_ContainerGetInit, container, rc)
    
    ! Call into the C++ interface
    call c_ESMC_ContainerGetCount(container, dummyFieldCount, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    if (present(fieldList)) then
      if (associated(fieldList)) then
        if (size(fieldList) < dummyFieldCount) then
          call ESMF_LogSetError(ESMF_RC_ARG_SIZE, &
            msg="fieldList is too small", &
            ESMF_CONTEXT, rcToReturn=rc)
          return  ! bail out
        endif
      else
        allocate(fieldList(dummyFieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg= "allocating fieldList", &
          ESMF_CONTEXT, rcToReturn=rc)) return ! bail out
      endif
      
      ! Call into the C++ interface to set up the vector on the C++ side
      call c_ESMC_ContainerGetVector(container, vector, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      do i=0, dummyFieldCount-1 ! C-style indexing, zero-based
        
        ! Call into the C++ interface to set up the vector on the C++ side
        call c_ESMC_ContainerGetVectorItem(container, vector, i, &
          fieldList(i+1), localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      enddo
      
      ! release vector here
      ! Call into the C++ interface to release the vector on the C++ side
      call c_ESMC_ContainerReleaseVector(container, vector, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
    endif
    
    if (present(fieldCount)) then
      fieldCount = dummyFieldCount
    endif
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGetFieldList
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerRemove()"
!BOPI
! !IROUTINE: ESMF_ContainerRemove - Remove object from Container

! !INTERFACE:
  subroutine ESMF_ContainerRemove(container, itemNameList, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
    character(len=*),     intent(in)            :: itemNameList(:)
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
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "remove"
!     where it is {\em not} an error if {\tt fieldNameList} contains Field
!     names that are not found in {\tt container}. For {\tt .false.} this is 
!     treated as an error condition. The default setting is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                     :: localrc      ! local return code
    type(ESMF_Logical)          :: relaxedflagArg
    integer                     :: i
    character(len=ESMF_MAXSTR)  :: name

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
    
    do i=1, size(itemNameList)
      ! Call into the C++ interface layer
      call c_ESMC_ContainerRemove(container, trim(itemNameList(i)), &
        relaxedflagArg, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerRemove
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ContainerPrint()"
!BOPI
! !IROUTINE: ESMF_ContainerPrint - Print Container object

! !INTERFACE:
  subroutine ESMF_ContainerPrint(container, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
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
  subroutine ESMF_ContainerGarbageOn(container, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
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
  subroutine ESMF_ContainerGarbageOff(container, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
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
  subroutine ESMF_ContainerGarbageClear(container, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)           :: container
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
#define ESMF_METHOD "ESMF_ContainerGarbageGet()"
!BOPI
! !IROUTINE: ESMF_ContainerGarbageGet - Query Container object about garbage

! !INTERFACE:
  subroutine ESMF_ContainerGarbageGet(container, garbageCount, garbageList, rc)
!
! !ARGUMENTS:
    type(ESMF_Container), intent(in)            :: container
    integer,              intent(out), optional :: garbageCount
    type(ESMF_Field),     pointer,     optional :: garbageList(:)
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
      
    if (present(garbageList)) then
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
      
    endif
    
    if (present(garbageCount)) then
      garbageCount = garbageC
    endif
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_ContainerGarbageGet
!------------------------------------------------------------------------------


end module ESMF_ContainerMod
