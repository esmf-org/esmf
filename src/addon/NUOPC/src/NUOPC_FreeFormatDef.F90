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
    module procedure NUOPC_FreeFormatCreateReadFile
    module procedure NUOPC_FreeFormatCreateReadYAML
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatAdd - Add lines to a FreeFormat object
! !INTERFACE:
  subroutine NUOPC_FreeFormatAdd(freeFormat, stringList, line, rc)
! !ARGUMENTS:
    type(NUOPC_FreeFormat),           intent(inout) :: freeFormat
    character(len=*),                 intent(in)    :: stringList(:)
    integer,                optional, intent(in)    :: line
    integer,                optional, intent(out)   :: rc
! !DESCRIPTION:
!   Add lines to a FreeFormat object. The capacity of {\tt freeFormat} may 
!   increase during this operation. The new lines provided in {\tt stringList}
!   are added starting at position {\tt line}. If {\tt line} is greater than the
!   current {\tt lineCount} of {\tt freeFormat}, blank lines are inserted to
!   fill the gap. By default, i.e. without specifying the {\tt line} argument,
!   the elements in {\tt stringList} are added to the {\em end} of the
!   {\tt freeFormat} object.
!EOP
  !-----------------------------------------------------------------------------
    integer             :: stat, i, j, lineOpt
    integer             :: stringCount, availableCount, newCapacity
    integer, parameter  :: extraCount = 10 ! resize with additional capacity
    integer             :: neededCount, gapCount
    character(len=NUOPC_FreeFormatLen), pointer   :: newStringList(:)

    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(line)) then
      ! sanity check
      if (line<1) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="The 'line' argument must be a positive integer starting at 1.", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
      lineOpt = line
    else
      lineOpt = freeFormat%count + 1
    endif
    
    stringCount = size(stringList)
    availableCount = size(freeFormat%stringList)-freeFormat%count
    
    gapCount = lineOpt - (freeFormat%count + 1)
    neededCount = stringCount ! initialize
    if (gapCount > 0) neededCount = neededCount + gapCount
    
    ! deal with capacity and moving old strings
    if (neededCount > availableCount) then
      ! must allocate a new stringList
      newCapacity = freeFormat%count + neededCount + extraCount
      allocate(newStringList(newCapacity), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of new stringList.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      ! copy lines from old to new allocation
      do i=1, lineOpt-1
        newStringList(i) = freeFormat%stringList(i)
      enddo
      do i=lineOpt, lineOpt+stringCount-1
        newStringList(i) = ""
      enddo
      do i=lineOpt, freeFormat%count
        newStringList(i+stringCount) = freeFormat%stringList(i)
      enddo
      ! replace old with new allocation
      deallocate(freeFormat%stringList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of stringList.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      freeFormat%stringList => newStringList ! point to the new stringList
    else
      ! the existing stringList has enough capacity -> deal with gap not zero
      if (gapCount > 0) then
        ! blank out gap lines
        do i=freeFormat%count+1, freeFormat%count+gapCount
          freeFormat%stringList(i) = ""
        enddo
      elseif (gapCount < 0) then
        ! move the existing lines to create space to insert new strings
        do i=freeFormat%count, lineOpt, -1
          freeFormat%stringList(i+stringCount) = freeFormat%stringList(i)
        enddo
      endif
    endif
    
    ! fill in the new strings
    i = lineOpt
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

    ! finally adjust the new count
    freeFormat%count = freeFormat%count + neededCount
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatCreate - Create a FreeFormat object
! !INTERFACE:
  ! Private name; call using NUOPC_FreeFormatCreate()
  function NUOPC_FreeFormatCreateDefault(freeFormat, stringList, capacity, rc)
! !RETURN VALUE:
    type(NUOPC_FreeFormat) :: NUOPC_FreeFormatCreateDefault
! !ARGUMENTS:
    type(NUOPC_FreeFormat), optional, intent(in)  :: freeFormat
    character(len=*),       optional, intent(in)  :: stringList(:)
    integer,                optional, intent(in)  :: capacity
    integer,                optional, intent(out) :: rc
! !DESCRIPTION:
!   Create a new FreeFormat object, which by default is empty. 
!   If {\tt freeFormat} is provided, then the newly created object starts as
!   a copy of {\tt freeFormat}. If {\tt stringList} is provided, then it is
!   added to the end of the newly created object. If {\tt capacity} is provided,
!   it is used for the {\em initial} creation of the newly created FreeFormat 
!   object. However, if the {\tt freeFormat} or {\tt stringList} arguments are
!   present, the final capacity may be larger than specified by {\tt capacity}.
!EOP
  !-----------------------------------------------------------------------------
    integer                                     :: localrc
    integer                                     :: stat, i
    integer                                     :: lineCount, capacityOpt
    character(len=NUOPC_FreeFormatLen), pointer :: stringListOpt(:)    

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! initialize return members
    NUOPC_FreeFormatCreateDefault%stringList => NULL()
    NUOPC_FreeFormatCreateDefault%count      =  0;
    
    ! determine initial capacity
    capacityOpt = 10  ! default
    if (present(capacity)) capacityOpt = capacity
    
    ! create initial allocation
    allocate(NUOPC_FreeFormatCreateDefault%stringList(capacityOpt), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of NUOPC_FreeFormat%stringList.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    ! conditionally copy the incoming freeFormat contents
    if (present(freeFormat)) then
      call NUOPC_FreeFormatGet(freeFormat, lineCount=lineCount, &
        stringList=stringListOpt, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      call NUOPC_FreeFormatAdd(NUOPC_FreeFormatCreateDefault, &
        stringListOpt(1:lineCount), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif
    
    ! conditionally add the stringList to the end
    if (present(stringList)) then
      call NUOPC_FreeFormatAdd(NUOPC_FreeFormatCreateDefault, stringList, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FreeFormatCreate - Create a FreeFormat object from Config
! !INTERFACE:
  ! Private name; call using NUOPC_FreeFormatCreate()
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
    integer   :: localrc
    integer   :: stat, i, j
    integer   :: lineCount, columnCount
    integer, allocatable  :: count(:)
    character(len=NUOPC_FreeFormatLen), allocatable  :: stringList(:)
    character(len=NUOPC_FreeFormatLen), allocatable  :: line(:)
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! initialize return members
    NUOPC_FreeFormatCreateRead%stringList => NULL()
    NUOPC_FreeFormatCreateRead%count      =  0;

    call ESMF_ConfigFindLabel(config, label=label, isPresent=isPresent, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    
    if (.not.isPresent) then
      if (present(relaxedflag)) then
        if (relaxedflag) then
          ! successful relaxed return with empty FreeFormat object
          NUOPC_FreeFormatCreateRead = NUOPC_FreeFormatCreate(rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
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
    
    call ESMF_ConfigGetDim(config, lineCount, columnCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    allocate(stringList(lineCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="stringList.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    allocate(count(lineCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="count.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    call ESMF_ConfigFindLabel(config, label=label, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    do i=1, lineCount
      call ESMF_ConfigNextLine(config, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      count(i) = ESMF_ConfigGetLen(config, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    enddo

    call ESMF_ConfigFindLabel(config, label=label, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    do i=1, lineCount
      call ESMF_ConfigNextLine(config, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      allocate(line(count(i)))
      call ESMF_ConfigGetAttribute(config, line, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      stringList(i) = ""
      do j=1, count(i)
        stringList(i)=trim(stringList(i))//" "//trim(adjustl(line(j)))
      enddo
      deallocate(line)
    enddo
    
    NUOPC_FreeFormatCreateRead = NUOPC_FreeFormatCreate(stringList=stringList, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

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
!BOPI
! !IROUTINE: NUOPC_FreeFormatCreate - Create a FreeFormat object from YAML
! !INTERFACE:
  ! Private name; call using NUOPC_FreeFormatCreate()
  function NUOPC_FreeFormatCreateReadYAML(ioyaml, rc)
! !RETURN VALUE:
    type(NUOPC_FreeFormat) :: NUOPC_FreeFormatCreateReadYAML
! !ARGUMENTS:
    type(ESMF_IO_YAML)                           :: ioyaml
    integer,               intent(out), optional :: rc
! !DESCRIPTION:
!   Create a new FreeFormat object from ESMF\_IO\_YAML object. The object
!   must exist or an error is returned.
!

!EOPI
  !-----------------------------------------------------------------------------
    integer :: localrc, stat
    integer :: lineCount
    character(len=NUOPC_FreeFormatLen), allocatable  :: stringList(:)

    if (present(rc)) rc = ESMF_SUCCESS

    ! initialize return members
    NUOPC_FreeFormatCreateReadYAML%stringList => NULL()
    NUOPC_FreeFormatCreateReadYAML%count      =  0;

    ! generate content for FreeFormat object
    call ESMF_IO_YAMLContentInit(ioyaml, cflag=ESMF_IOYAML_CONTENT_FREEFORM, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! get capacity (# lines) of generated FreeFormat object
    call ESMF_IO_YAMLContentGet(ioyaml, lineCount=lineCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! allocate array containing FreeFormat lines
    allocate(stringList(lineCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="stringList.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! initialize array
    ! -- this is required or the following API won't be able to check its size
    stringList = ""

    ! retrieve content of FreeFormat object
    call ESMF_IO_YAMLContentGet(ioyaml, content=stringList, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! create new FreeFormat object using content from YAML parser
    NUOPC_FreeFormatCreateReadYAML = &
      NUOPC_FreeFormatCreateDefault(stringList=stringList, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! free up memory
    deallocate(stringList, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="stringList.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FreeFormatCreate - Create a FreeFormat object from file
! !INTERFACE:
  ! Private name; call using NUOPC_FreeFormatCreate()
  function NUOPC_FreeFormatCreateReadFile(fileName, iofmt, label, relaxedflag, rc)
! !RETURN VALUE:
    type(NUOPC_FreeFormat) :: NUOPC_FreeFormatCreateReadFile
! !ARGUMENTS:
    character(len=*),      intent(in)            :: fileName
    type(ESMF_IOFmt_flag), intent(in),  optional :: iofmt
    character(len=*),      intent(in),  optional :: label
    logical,               intent(in),  optional :: relaxedflag
    integer,               intent(out), optional :: rc
! !DESCRIPTION:
!   Create a new FreeFormat object from a file with given {\tt iofmt}.
!   Supported values for {\tt iofmt} are: {\tt ESMF\_IOFMT\_CONFIG} 
!   (ESMF Config file) and {\tt ESMF\_IOFMT\_YAML} (YAML file).
!   If {\tt ESMF\_IOFMT\_CONFIG} is used, the optional argument {\tt label}
!   must be provided, and {\tt relaxedflag} may be used.
!   If I/O format is {\tt ESMF\_IOFMT\_YAML}, both {\tt label} and 
!   {\tt relaxedflag} are ignored.
!   If {\tt iofmt} is not provided, it defaults to {\tt ESMF\_IOFMT\_CONFIG}.
!
!EOPI
  !-----------------------------------------------------------------------------
    integer               :: localrc
    type(ESMF_IOFmt_flag) :: iofmtOpt
    type(ESMF_Config)     :: config
    type(ESMF_IO_YAML)    :: ioyaml

    if (present(rc)) rc = ESMF_SUCCESS

    ! initialize return members
    NUOPC_FreeFormatCreateReadFile%stringList => NULL()
    NUOPC_FreeFormatCreateReadFile%count      =  0;

    ! set defaults
    iofmtOpt = ESMF_IOFMT_CONFIG
    if (present(iofmt)) iofmtOpt = iofmt

    if (iofmtOpt == ESMF_IOFMT_CONFIG) then

      ! create config object
      config = ESMF_ConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

      ! read config from file
      call ESMF_ConfigLoadFile(config, fileName, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

      ! create FreeFormat object by parsing config
      NUOPC_FreeFormatCreateReadFile = &
        NUOPC_FreeFormatCreateRead(config, label, &
        relaxedflag=relaxedflag, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

      ! free up memory
      call ESMF_ConfigDestroy(config, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    else if (iofmtOpt == ESMF_IOFMT_YAML) then

      ! create IO_YAML object
      ioyaml = ESMF_IO_YAMLCreate(rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

      ! read YAML into IO_YAML object
      call ESMF_IO_YAMLRead(ioyaml, fileName, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

      ! parse YAML doc as NUOPC Field Dictionary
      call ESMF_IO_YAMLParse(ioyaml, parseflag=ESMF_IOYAML_PARSE_NUOPCFD, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

      ! create FreeFormat object from parsed content
      NUOPC_FreeFormatCreateReadFile = &
        NUOPC_FreeFormatCreateReadYAML(ioyaml, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

      ! free up memory
      call ESMF_IO_YAMLDestroy(ioyaml, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    else

      ! I/O format not implemented
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="unsupported iofmt", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out

    end if

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
    integer   :: localrc
    integer   :: i
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! loop over lines
    if (associated(freeFormat%stringList)) then
      do i=1, freeFormat%count
        call ESMF_LogWrite(freeFormat%stringList(i), ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
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
        print *, trim(freeFormat%stringList(i))
      enddo
    endif

  end subroutine
  !-----------------------------------------------------------------------------

end module NUOPC_FreeFormatDef
