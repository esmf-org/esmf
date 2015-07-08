! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2015, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_FreeFormatDef.F90"
!==============================================================================

module NUOPC_FreeFormatDef

  use ESMF

  implicit none
  
  private
  
  ! type
  public NUOPC_FreeFormat
  
  ! parameter
  public NUOPC_FreeFormatLen

  ! methods  
  public NUOPC_FreeFormatCreate
  public NUOPC_FreeFormatDestroy
  public NUOPC_FreeFormatGet
  public NUOPC_FreeFormatGetLine
  public NUOPC_FreeFormatPrint
  
  
!==============================================================================
! 
! Constants
!
!==============================================================================
  
  integer, parameter              :: NUOPC_FreeFormatLen = 160

!==============================================================================
! 
! DERIVED TYPES
!
!==============================================================================
  
  type NUOPC_FreeFormat
    character(len=NUOPC_FreeFormatLen), pointer   :: stringList(:)
    integer                                       :: count
  end type

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

#if 0
  interface NUOPC_FreeFormatBla
    module procedure NUOPC_FreeFormatBla1
    module procedure NUOPC_FreeFormatBla2
  end interface
#endif

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatCreate - Create a FreeFormat object
! !INTERFACE:
  function NUOPC_FreeFormatCreate(stringList, capacity, rc)
! !RETURN VALUE:
    type(NUOPC_FreeFormat) :: NUOPC_FreeFormatCreate
! !ARGUMENTS:
    character(len=NUOPC_FreeFormatLen), optional, intent(in)  :: stringList(:)
    integer,                            optional, intent(in)  :: capacity
    integer,                            optional, intent(out) :: rc
! !DESCRIPTION:
!   Create a new FreeFormat object. If {\tt stringList} is provided, then the
!   newly created object will hold the provided strings and the count is that 
!   of {\tt size(stringList)}. If {\tt capacity} is provided, it is used to set
!   the capacity of the newly created FreeFormat object. Providing a
!   {\tt capacity} that is smaller than {\tt size(stringList)} triggers an
!   error.
!EOP
  !-----------------------------------------------------------------------------
    integer   :: stat, i
    integer   :: stringCount, capacityOpt
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! initialize members
    NUOPC_FreeFormatCreate%stringList => NULL()
    NUOPC_FreeFormatCreate%count      =  0;
    
    ! determine count
    if (present(stringList)) then
      stringCount = size(stringList)
    else
      stringCount = 0
    endif
    
    ! determine capacity
    if (present(capacity)) then
      capacityOpt = capacity
      if (stringCount>capacity) then
        ! error condition
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Capacity if provided must at least be as large as stringCount", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
    else
      capacityOpt = stringCount
    endif
    
    ! allocate
    allocate(NUOPC_FreeFormatCreate%stringList(capacityOpt), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of NUOPC_FreeFormat%stringList.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! conditionally fill initial strings
    NUOPC_FreeFormatCreate%count = stringCount
    if (present(stringList)) then
      do i=1, stringCount
        NUOPC_FreeFormatCreate%stringList(i) = stringList(i)
      enddo
    endif

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatDestroy - Destroy a FreeFormat object
! !INTERFACE:
  subroutine NUOPC_FreeFormatDestroy(freeFormat, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat),           intent(inout) :: freeFormat
    integer,                optional, intent(out)   :: rc
! !DESCRIPTION:
!   Destroy a FreeFormat object. All internal memory is deallocated.
!EOP
  !-----------------------------------------------------------------------------
    integer   :: stat
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! conditionally deallocate members 
    if (associated(freeFormat%stringList)) then
      deallocate(freeFormat%stringList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of NUOPC_FreeFormat%stringList.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatGet - Get information from a FreeFormat object
! !INTERFACE:
  subroutine NUOPC_FreeFormatGet(freeFormat, count, capacity, stringList, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat),                       intent(in)  :: freeFormat
    integer,                            optional, intent(out) :: count
    integer,                            optional, intent(out) :: capacity
    character(len=NUOPC_FreeFormatLen), optional, pointer     :: stringList(:)
    integer,                            optional, intent(out) :: rc
! !DESCRIPTION:
!   Get information from a FreeFormat object.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(count)) then
      count = freeFormat%count
    endif

    if (present(capacity)) then
      capacity = size(freeFormat%stringList)
    endif

    if (present(stringList)) then
      stringList => freeFormat%stringList
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatGetLine - Get line info from a FreeFormat object
! !INTERFACE:
  subroutine NUOPC_FreeFormatGetLine(freeFormat, line, lineString, tokenCount, &
    tokenList, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat),                       intent(in)  :: freeFormat
    integer,                                      intent(in)  :: line
    character(len=NUOPC_FreeFormatLen), optional, intent(out) :: lineString
    integer,                            optional, intent(out) :: tokenCount
    character(len=NUOPC_FreeFormatLen), optional, intent(out) :: tokenList(:)
    integer,                            optional, intent(out) :: rc
! !DESCRIPTION:
!   Get information about a specific line in a FreeFormat object.
!EOP
  !-----------------------------------------------------------------------------
    integer                               :: i, count, last
    character(len=NUOPC_FreeFormatLen)    :: string
    logical                               :: spaceFlag
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! error checking
    if (line>freeFormat%count) then
      ! error condition
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="The line index cannot be larger than the string count", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! access the line string
    string = trim(freeFormat%stringList(line))

    ! ouput: lineString
    if (present(lineString)) then
      lineString = string
    endif
    
    if (present(tokenCount) .or. present(tokenList)) then
      ! count tokens
      count = 0 ! reset
      spaceFlag = (string(1:1) == ' ')
      do i=2, len(string)
        if ((string(i:i)==' ') .and. .not.spaceFlag) then
          count = count+1
        endif
        spaceFlag = (string(i:i) == ' ')
      enddo
    
      ! output: tokenCount
      if (present(tokenCount)) then
        tokenCount = count
      endif
    endif
    
    if (present(tokenList)) then
      if (size(tokenList) /= count) then
        ! error condition
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="tokenList must have exactly as many elements as there are tokens", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      
      count = 0 ! reset
      spaceFlag = (string(1:1) == ' ')
      last = 1  ! reset
      do i=2, len(string)
        if ((string(i:i)==' ') .and. .not.spaceFlag) then
          count = count+1
          tokenList(count) = adjustl(string(last:i))
          last = i+1
        endif
        spaceFlag = (string(i:i) == ' ')
      enddo
      
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatPrint - Print a FreeFormat object
! !INTERFACE:
  subroutine NUOPC_FreeFormatPrint(freeFormat, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat),           intent(in)    :: freeFormat
    integer,                optional, intent(out)   :: rc
! !DESCRIPTION:
!   Print a FreeFormat object.
!EOP
  !-----------------------------------------------------------------------------
    integer   :: i
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! conditionally deallocate members 
    if (associated(freeFormat%stringList)) then
      do i=1, freeFormat%count
        print *, freeFormat%stringList(i)
      enddo
    endif

  end subroutine
  !-----------------------------------------------------------------------------

end module NUOPC_FreeFormatDef
