! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
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
  public NUOPC_FreeFormatAdd
  public NUOPC_FreeFormatCreate
  public NUOPC_FreeFormatDestroy
  public NUOPC_FreeFormatGet
  public NUOPC_FreeFormatGetLine
  public NUOPC_FreeFormatLog
  public NUOPC_FreeFormatPrint
  
  
!==============================================================================
! 
! Constants
!
!==============================================================================
  
  integer, parameter              :: NUOPC_FreeFormatLen = 800

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

  interface NUOPC_FreeFormatCreate
    module procedure NUOPC_FreeFormatCreateDefault
    module procedure NUOPC_FreeFormatCreateRead
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatAdd - Add lines to a FreeFormat object
! !INTERFACE:
  subroutine NUOPC_FreeFormatAdd(freeFormat, stringList, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat),           intent(inout) :: freeFormat
    character(len=*),                 intent(in)    :: stringList(:)
    integer,                optional, intent(out)   :: rc
! !DESCRIPTION:
!   Add lines to a FreeFormat object. The capacity of {\tt freeFormat} may 
!   increase during this operation. The elements in {\tt stringList} are 
!   added to the end of {\tt freeFormat}.
!EOP
  !-----------------------------------------------------------------------------
    integer             :: stat, i, j
    integer             :: stringCount, availableCount, newCapacity
    integer, parameter  :: extraCount = 10
    character(len=NUOPC_FreeFormatLen), pointer   :: newStringList(:)

    if (present(rc)) rc = ESMF_SUCCESS
    
    stringCount = size(stringList)
    availableCount = size(freeFormat%stringList)-freeFormat%count
    
    if (stringCount >= availableCount) then
      newCapacity = freeFormat%count + stringCount + extraCount
      allocate(newStringList(newCapacity), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of new stringList.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      do i=1, freeFormat%count
        newStringList(i) = freeFormat%stringList(i)  ! copy the existing entries
      enddo
      deallocate(freeFormat%stringList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of stringList.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      freeFormat%stringList => newStringList ! point to the new stringList
    endif
    
    ! fill in the new strings
    i = freeFormat%count + 1
    do j=1, stringCount
      if (len_trim(stringList(j)) > NUOPC_FreeFormatLen) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="String length is above implementation limit", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      freeFormat%stringList(i) =  stringList(j)
      i=i+1
    enddo

    freeFormat%count = freeFormat%count + stringCount
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatCreate - Create a FreeFormat object
! !INTERFACE:
  ! call using generic interface: NUOPC_FreeFormatCreate
  function NUOPC_FreeFormatCreateDefault(stringList, capacity, rc)
! !RETURN VALUE:
    type(NUOPC_FreeFormat) :: NUOPC_FreeFormatCreateDefault
! !ARGUMENTS:
    character(len=*), optional, intent(in)  :: stringList(:)
    integer,          optional, intent(in)  :: capacity
    integer,          optional, intent(out) :: rc
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
    NUOPC_FreeFormatCreateDefault%stringList => NULL()
    NUOPC_FreeFormatCreateDefault%count      =  0;
    
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
    allocate(NUOPC_FreeFormatCreateDefault%stringList(capacityOpt), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of NUOPC_FreeFormat%stringList.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! conditionally fill initial strings
    if (present(stringList)) then
      call NUOPC_FreeFormatAdd(NUOPC_FreeFormatCreateDefault, stringList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatCreate - Create a FreeFormat object from Config
! !INTERFACE:
  ! call using generic interface: NUOPC_FreeFormatCreate
  function NUOPC_FreeFormatCreateRead(config, label, relaxedflag, rc)
! !RETURN VALUE:
    type(NUOPC_FreeFormat) :: NUOPC_FreeFormatCreateRead
! !ARGUMENTS:
    type(ESMF_Config)                            :: config
    character(len=*),      intent(in)            :: label
    logical,               intent(in),  optional :: relaxedflag
    integer,               intent(out), optional :: rc 
! !DESCRIPTION:
!   Create a new FreeFormat object from ESMF\_Config object. The {\tt config}
!   object must exist, and {\tt label} must reference a table attribute 
!   within {\tt config}.
!
! By default an error is returned if {\tt label} is not found in {\tt config}.
! This error can be suppressed by setting {\tt relaxedflag=.true.}, and an 
! empty FreeFormat object will be returned.

!EOP
  !-----------------------------------------------------------------------------
    logical   :: isPresent
    integer   :: stat, i, j
    integer   :: lineCount, columnCount
    integer, allocatable  :: count(:)
    character(len=NUOPC_FreeFormatLen), allocatable  :: stringList(:)
    character(len=NUOPC_FreeFormatLen), allocatable  :: line(:)
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ConfigFindLabel(config, label=label, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    if (.not.isPresent) then
      if (present(relaxedflag)) then
        if (relaxedflag) then
          ! successful relaxed return with empty FreeFormat object
          NUOPC_FreeFormatCreateRead = NUOPC_FreeFormatCreate(rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) return  ! bail out
          return ! early return
        endif
      endif
      ! error condition -> must bail
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Label must be present.", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    call ESMF_ConfigGetDim(config, lineCount, columnCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    allocate(stringList(lineCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="stringList.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    allocate(count(lineCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="count.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    call ESMF_ConfigFindLabel(config, label=label, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    do i=1, lineCount
      call ESMF_ConfigNextLine(config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      count(i) = ESMF_ConfigGetLen(config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    enddo

    call ESMF_ConfigFindLabel(config, label=label, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    do i=1, lineCount
      call ESMF_ConfigNextLine(config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      allocate(line(count(i)))
      call ESMF_ConfigGetAttribute(config, line, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      stringList(i) = ""
      do j=1, count(i)
        stringList(i)=trim(stringList(i))//" "//trim(adjustl(line(j)))
      enddo
      deallocate(line)
    enddo
    
    NUOPC_FreeFormatCreateRead = NUOPC_FreeFormatCreate(stringList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    deallocate(stringList, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="stringList.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    deallocate(count, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="count.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

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
      nullify(freeFormat%stringList)
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatGet - Get information from a FreeFormat object
! !INTERFACE:
  subroutine NUOPC_FreeFormatGet(freeFormat, lineCount, capacity, stringList, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat),                       intent(in)  :: freeFormat
    integer,                            optional, intent(out) :: lineCount
    integer,                            optional, intent(out) :: capacity
    character(len=NUOPC_FreeFormatLen), optional, pointer     :: stringList(:)
    integer,                            optional, intent(out) :: rc
! !DESCRIPTION:
!   Get information from a FreeFormat object.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(lineCount)) then
      lineCount = freeFormat%count
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
! !IROUTINE: NUOPC_FreeFormatLog - Write a FreeFormat object to the default Log
! !INTERFACE:
  subroutine NUOPC_FreeFormatLog(freeFormat, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat),           intent(in)    :: freeFormat
    integer,                optional, intent(out)   :: rc
! !DESCRIPTION:
!   Write a FreeFormat object to the default Log.
!EOP
  !-----------------------------------------------------------------------------
    integer   :: i
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! loop over lines
    if (associated(freeFormat%stringList)) then
      do i=1, freeFormat%count
        call ESMF_LogWrite(freeFormat%stringList(i), ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
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
    
    ! loop over lines
    if (associated(freeFormat%stringList)) then
      do i=1, freeFormat%count
        print *, freeFormat%stringList(i)
      enddo
    endif

  end subroutine
  !-----------------------------------------------------------------------------

end module NUOPC_FreeFormatDef
