! $Id: NUOPC.F90,v 1.2 2011/04/13 23:44:47 theurich Exp $
module NUOPC

  !-----------------------------------------------------------------------------
  ! Generic code collection
  !-----------------------------------------------------------------------------

  use ESMF_Mod

  implicit none
  
  private
  
  public NUOPC_FieldAttributeGet
  public NUOPC_FieldAttributeAdd
  public NUOPC_CplCompAreServicesSet
  public NUOPC_CplCompAttributeGet
  public NUOPC_CplCompAttributeAdd
  public NUOPC_TimePrint
  public NUOPC_ClockPrintCurrTime
  public NUOPC_ClockPrintStartTime
  public NUOPC_ClockPrintStopTime
  public NUOPC_ClockInitialize
  public NUOPC_GridCompAreServicesSet  
  public NUOPC_GridCompSetClock
  public NUOPC_GridCompCheckSetClock
  public NUOPC_StateBuildStdList
  public NUOPC_StateIsAllConnected
  public NUOPC_StateSetTimestamp
  public NUOPC_StateAddPotentialField
  public NUOPC_StateReplaceWRealField
  public NUOPC_StateIsCurrentTimestamp
  public NUOPC_FieldBundleUpdateTime
  public NUOPC_GridCreateSimpleXY
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldAttributeGet - Get a NUOPC Field Attribute
! !INTERFACE:
  subroutine NUOPC_FieldAttributeGet(field, name, value, rc)
! !ARGUMENTS:
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: name
    character(*), intent(out)             :: value
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Access the Attribute {\tt name} inside of {\tt field} using the
!   convention {\tt NUOPC} and purpose {\tt General}. Return with error if
!   the Attribute is not present or not set.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: defaultvalue
    
    if (present(rc)) rc = ESMF_SUCCESS

    defaultvalue = "CheckThisDefaultValue"

    call ESMF_AttributeGet(field, name=name, value=value, &
      defaultvalue=defaultvalue, convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (trim(value) == trim(defaultvalue)) then
      ! attribute not present
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not present",&
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    else if (len_trim(value) == 0) then
      ! attribute present but not set
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not set",&
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldAttributeAdd - Add a NUOPC Field Attribute
! !INTERFACE:
  subroutine NUOPC_FieldAttributeAdd(field, StandardName, Units, Connected, rc)
! !ARGUMENTS:
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: StandardName
    character(*), intent(in)              :: Units
    character(*), intent(in)              :: Connected
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: attrList(2)

    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be added to the Fields
    attrList(1) = "Connected"  ! values: "true" or "false"
    attrList(2) = "TimeStamp"  ! values: list of 9 integers: yy,mm,dd,h,m,s,ms,us,ns
    
    ! add Attribute packages
    call ESMF_AttributeAdd(field, convention="ESG", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeAdd(field, convention="NUOPC", purpose="General",   &
      attrList=attrList, nestConvention="ESG", nestPurpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! set Attributes
    call ESMF_AttributeSet(field, &
      name="StandardName", value=StandardName, &
      convention="ESG", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeSet(field, &
      name="Units", value=Units, &
      convention="ESG", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeSet(field, &
      name="Connected", value=Connected, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeSet(field, &
      name="TimeStamp", valueList=(/0,0,0,0,0,0,0,0,0/), &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CplCompAreServicesSet - Check if SetServices was called
! !INTERFACE:
  function NUOPC_CplCompAreServicesSet(comp, rc)
! !ARGUMENTS:
    logical :: NUOPC_CplCompAreServicesSet
    type(ESMF_CplComp), intent(in)            :: comp
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if SetServices was called. Otherwise {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Pointer)      :: vm_info
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! make a copy of the external externalClock
    call ESMF_CompGet(comp%compp, vm_info=vm_info, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    if (vm_info == ESMF_NULL_POINTER) then
      NUOPC_CplCompAreServicesSet = .false.
    else
      NUOPC_CplCompAreServicesSet = .true.
    endif
      
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CplCompAttributeGet - Get a NUOPC CplComp Attribute
! !INTERFACE:
  subroutine NUOPC_CplCompAttributeGet(comp, cplList, cplListSize, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                    :: comp
    character(*), intent(inout)           :: cplList(:)
    integer,      intent(inout)           :: cplListSize
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: defaultvalue
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeGet(comp, name="CplList", valueList=cplList, &
      itemCount=cplListSize, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CplCompAttributeAdd - Add a NUOPC CplComp Attribute
! !INTERFACE:
  subroutine NUOPC_CplCompAttributeAdd(comp, importState, exportState, rc)
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)         :: comp
    type(ESMF_State),   intent(in)            :: importState
    type(ESMF_State),   intent(in)            :: exportState
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: attrList(2)
    integer, parameter      :: maxCount=10
    character(ESMF_MAXSTR)  :: cplListValues(maxCount)
    integer                 :: count

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! Set up a customized list of Attributes to be added to the CplComp
    attrList(1) = "LongName"
    attrList(2) = "CplList"
    
    ! add Attribute packages
    call ESMF_AttributeAdd(comp, convention="NUOPC", purpose="General",   &
      attrList=attrList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! find cplListValues
    call NUOPC_FillCplList(importState, exportState, cplList=cplListValues, &
      count=count, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set Attributes
    call ESMF_AttributeSet(comp, &
      name="LongName", value="NUOPC Generic Connector Component", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (count>0) then
      call ESMF_AttributeSet(comp, &
        name="CplList", valueList=cplListValues(1:count), &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
      
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FillCplList - Fill the cplList according to matching Fields
! !INTERFACE:
  subroutine NUOPC_FillCplList(importState, exportState, cplList, count, rc)
! !ARGUMENTS:
    type(ESMF_State),       intent(in)            :: importState
    type(ESMF_State),       intent(in)            :: exportState
    character(ESMF_MAXSTR), intent(out)           :: cplList(:)
    integer,                intent(out)           :: count
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: maxCount, i, j
    character(ESMF_MAXSTR), pointer :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer :: exportStandardNameList(:)
    
    if (present(rc)) rc = ESMF_SUCCESS

    maxCount = size(cplList)
    count = 0 ! initialize
    
    ! build list of standard names of all Fields inside of importState
    call NUOPC_StateBuildStdList(importState, importStandardNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateBuildStdList(exportState, exportStandardNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (associated(importStandardNameList) .and. &
      associated(exportStandardNameList)) then
      
      ! simple linear search of items that match between both lists
      do i=1, size(importStandardNameList)
        do j=1, size(exportStandardNameList)
          if (importStandardNameList(i) == exportStandardNameList(j)) then
            count = count+1
            if (count > maxCount) then
              call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                msg="Not enough space in cplList",&
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)
                return  ! bail out
            endif
            cplList(count) = importStandardNameList(i)
            exit
          endif
        enddo
      enddo
      
    endif
      
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_TimePrint - Formatted print ot time information
! !INTERFACE:
  subroutine NUOPC_TimePrint(time, string, rc)
! !ARGUMENTS:
    type(ESMF_Time)                               :: time
    character(*),           intent(in),  optional :: string
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                 :: yy, mm, dd, h, m, s, ms
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  
    if (present(string)) then
      write (*, "(A, I4, I3, I3, I3, I3, I3, I4)") string, &
        yy, mm, dd, h, m, s, ms
    else
      write (*, "(I4, I3, I3, I3, I3, I3, I4)") &
        yy, mm, dd, h, m, s, ms
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ClockPrintCurrTime - Formatted print ot current time
! !INTERFACE:
  subroutine NUOPC_ClockPrintCurrTime(clock, string, rc)
! !ARGUMENTS:
    type(ESMF_Clock)                              :: clock
    character(*),           intent(in),  optional :: string
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Formatted print of the current time in clock.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Time)         :: currTime
    if (present(rc)) rc = ESMF_SUCCESS
  
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_TimePrint(currTime, string, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ClockPrintStartTime - Formatted print ot start time
! !INTERFACE:
  subroutine NUOPC_ClockPrintStartTime(clock, string, rc)
! !ARGUMENTS:
    type(ESMF_Clock)                              :: clock
    character(*),           intent(in),  optional :: string
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Formatted print of the start time in clock.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Time)         :: startTime
    if (present(rc)) rc = ESMF_SUCCESS
  
    call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_TimePrint(startTime, string, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ClockPrintStopTime - Formatted print ot stop time
! !INTERFACE:
  subroutine NUOPC_ClockPrintStopTime(clock, string, rc)
! !ARGUMENTS:
    type(ESMF_Clock)                              :: clock
    character(*),           intent(in),  optional :: string
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Formatted print of the stop time in clock.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Time)         :: stopTime
    if (present(rc)) rc = ESMF_SUCCESS
  
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_TimePrint(stopTime, string, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ClockInitialize - Initialize a clock from clock with stabilityTimeStep
! !INTERFACE:
  function NUOPC_ClockInitialize(externalClock, stabilityTimeStep, rc)
! !ARGUMENTS:
    type(ESMF_Clock) :: NUOPC_ClockInitialize
    type(ESMF_Clock)                               :: externalClock
    type(ESMF_TimeInterval), intent(in),  optional :: stabilityTimeStep
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Clock)        :: internalClock
    type(ESMF_TimeInterval) :: externalTimeStep
    type(ESMF_TimeInterval) :: actualTimeStep
    integer                 :: internalStepCount
    
    if (present(rc)) rc = ESMF_SUCCESS
    
      ! make a copy of the external externalClock
    internalClock = ESMF_ClockCreate(externalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    if (present(stabilityTimeStep)) then
    
      ! determine the internal timeStep
      ! The external (parent) timeStep must be a multiple of the internal
      ! timeStep. At the same time there is typically a physical/stability limit
      ! for the internal timeStep. The following procedure finds an internal
      ! timeStep that is as close as possible to the provided stability limit, 
      ! while <= that limit. At the same time the external timeStep is a multiple
      ! of the internal timeStep.
      call ESMF_ClockGet(externalClock, timeStep=externalTimeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
      internalStepCount = ceiling(externalTimeStep / stabilityTimeStep)
      actualTimeStep = externalTimeStep / internalStepCount
    
      call ESMF_ClockSet(internalClock, timeStep=actualTimeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
      
    NUOPC_ClockInitialize = internalClock
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCompAreServicesSet - Check if SetServices was called
! !INTERFACE:
  function NUOPC_GridCompAreServicesSet(comp, rc)
! !ARGUMENTS:
    logical :: NUOPC_GridCompAreServicesSet
    type(ESMF_GridComp), intent(in)            :: comp
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if SetServices was called. Otherwise {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Pointer)      :: vm_info
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! make a copy of the external externalClock
    call ESMF_CompGet(comp%compp, vm_info=vm_info, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    if (vm_info == ESMF_NULL_POINTER) then
      NUOPC_GridCompAreServicesSet = .false.
    else
      NUOPC_GridCompAreServicesSet = .true.
    endif
      
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCompSetClock - Set initialized a clock in GridComp
! !INTERFACE:
  subroutine NUOPC_GridCompSetClock(comp, externalClock, stabilityTimeStep, rc)
! !ARGUMENTS:
    type(ESMF_GridComp),     intent(inout)         :: comp
    type(ESMF_Clock),        intent(in)            :: externalClock
    type(ESMF_TimeInterval), intent(in),  optional :: stabilityTimeStep
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Set the Component internal Clock as a copy of the externalClock, but
!   with a timeStep that is less than or equal to the stabilityTimeStep.
!   At the same time ensure that the timeStep of the externalClock is
!   a multiple of the internal Clock's timeStep. If the stabilityTimeStep
!   argument is not provided then the internal Clock will simply be set
!   as a copy of the externalClock.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Clock)        :: internalClock

    if (present(rc)) rc = ESMF_SUCCESS
    
    internalClock = NUOPC_ClockInitialize(externalClock, stabilityTimeStep, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompSet(comp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCompCheckSetClock - Check clock compatibility
! !INTERFACE:
  subroutine NUOPC_GridCompCheckSetClock(comp, externalClock, rc)
! !ARGUMENTS:
    type(ESMF_GridComp),     intent(inout)         :: comp
    type(ESMF_Clock),        intent(in)            :: externalClock
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Compare the externalClock to the Component internal Clock to make sure
!   they match in their current Time. Further ensure that the externalClock's
!   timeStep is a multiple of the internal Clock's timeStep. If both
!   these condition are satisfied then the stopTime of the internal Clock is
!   set to be reachable in one timeStep of the external Clock, taking into
!   account the direction of the Clock.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables    
    type(ESMF_Clock)        :: internalClock
    type(ESMF_Time)         :: externalCurrTime, currTime, stopTime
    type(ESMF_TimeInterval) :: externalTimeStep, timeStep
    type(ESMF_Direction)    :: direction

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! compare external and internal Clocks for consistency
    call ESMF_ClockGet(externalClock, currTime=externalCurrTime, &
      timeStep=externalTimeStep, direction=direction, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_GridCompGet(comp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(internalClock, currTime=currTime, timeStep=timeStep, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! ensure the current times match between external and internal Clock
    if (currTime /= externalCurrTime) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="internal and external Clocks do not match in current time!", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! ensure that the external timestep is still a multiple of the internal one
    if (ceiling(externalTimeStep/timeStep) /= floor(externalTimeStep/timeStep))&
      then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="external timestep is not multiple of internal timestep!", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! set the new stopTime of the internalClock
    if (direction==ESMF_MODE_FORWARD) then
      stopTime = currTime + externalTimeStep
    else
      stopTime = currTime - externalTimeStep
    endif
    call ESMF_ClockSet(internalClock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateBuildStdList - Build a Field list from State according to standardName attribute
! !INTERFACE:
  subroutine NUOPC_StateBuildStdList(state, stdAttrNameList, stdItemNameList, &
    stdConnectedList, rc)
! !ARGUMENTS:
    type(ESMF_State),       intent(in)            :: state
    character(ESMF_MAXSTR), pointer               :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer, optional     :: stdItemNameList(:)
    character(ESMF_MAXSTR), pointer, optional     :: stdConnectedList(:)
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer           :: item, itemCount, fieldCount, stat
    type(ESMF_Field)  :: field
    character(ESMF_MAXSTR), allocatable   :: itemNameList(:)
    type(ESMF_StateItemType), allocatable :: stateitemtypeList(:)
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
          
    if (itemCount > 0) then
      allocate(itemNameList(itemCount))
      allocate(stateitemtypeList(itemCount))
      call ESMF_StateGet(state, itemNameList=itemNameList, &
        itemtypeList=stateitemtypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      fieldCount = 0  ! reset
      do item=1, itemCount
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) &
          fieldCount = fieldCount + 1
      enddo
      
      allocate(stdAttrNameList(fieldCount), stat=stat)
      if (ESMF_LogFoundAllocError(stat, msg= "allocating stdAttrNameList", &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (present(stdItemNameList)) then
        allocate(stdItemNameList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg= "allocating stdItemNameList", &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif

      if (present(stdConnectedList)) then
        allocate(stdConnectedList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg= "allocating stdConnectedList", &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif

      fieldCount = 1  ! reset

      do item=1, itemCount
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, itemName=itemNameList(item), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_FieldAttributeGet(field, name="StandardName", &
            value=stdAttrNameList(fieldCount), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (present(stdItemNameList)) then
            stdItemNameList(fieldCount)=itemNameList(item)
          endif
          if (present(stdConnectedList)) then
            call NUOPC_FieldAttributeGet(field, name="Connected", &
              value=stdConnectedList(fieldCount), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          endif
          fieldCount = fieldCount + 1
        endif
      enddo
        
      deallocate(itemNameList)
      deallocate(stateitemtypeList)
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateBuildStdList - Build a Field list from State according to standardName attribute
! !INTERFACE:
  function NUOPC_StateIsAllConnected(state, rc)
! !ARGUMENTS:
    logical :: NUOPC_StateIsAllConnected
    type(ESMF_State)                          :: state
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer           :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer           :: stdConnectedList(:)
    logical                                   :: allConnected
    integer                                   :: i

    if (present(rc)) rc = ESMF_SUCCESS
    
    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdConnectedList=stdConnectedList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    allConnected = .true.  ! initialize
    do i=1, size(stdConnectedList)
      if (stdConnectedList(i) /= "true") then
        allConnected = .false.
        exit
      endif
    enddo

    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdConnectedList)) deallocate(stdConnectedList)

    NUOPC_StateIsAllConnected = allConnected

  end function
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateSetTimestamp - Set timestamp on all fields in a State
! !INTERFACE:
  subroutine NUOPC_StateSetTimestamp(state, clock, rc)
! !ARGUMENTS:
    type(ESMF_State),        intent(inout)         :: state
    type(ESMF_Clock),        intent(in)            :: clock
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    type(ESMF_Field)                      :: field
    type(ESMF_Time)         :: time
    integer                 :: yy, mm, dd, h, m, s, ms, us, ns
    integer                 :: i
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, us=us, &
      ns=ns, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  
    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdItemNameList=stdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    do i=1, size(stdItemNameList)
      call ESMF_StateGet(state, field=field, itemName=stdItemNameList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(field, &
        name="TimeStamp", valueList=(/yy,mm,dd,h,m,s,ms,us,ns/), &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo
    
    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdItemNameList)) deallocate(stdItemNameList)
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateAddPotentialField - Set timestamp on all fields in a State
! !INTERFACE:
  subroutine NUOPC_StateAddPotentialField(state, name, StandardName, Units, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(*),     intent(in)            :: name
    character(*),     intent(in)            :: StandardName
    character(*),     intent(in)            :: Units
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)        :: field
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    field = ESMF_FieldCreateEmpty(name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldAttributeAdd(field, StandardName=StandardName, &
      Units=Units, Connected="false", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateAdd(state, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateReplaceWRealField - Replace potential with actual fields
! !INTERFACE:
  subroutine NUOPC_StateReplaceWRealField(state, field, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    type(ESMF_Field), intent(in)            :: field
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)        :: potentialField
    character(ESMF_MAXSTR)  :: name
    character(ESMF_MAXSTR)  :: StandardName
    character(ESMF_MAXSTR)  :: Units
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_FieldGet(field, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateGet(state, field=potentialField, itemName=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldAttributeGet(potentialField, name="StandardName", &
      value=StandardName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldAttributeGet(potentialField, name="Units", &
      value=Units, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldAttributeAdd(field, StandardName=StandardName,&
      Units=Units, Connected="false", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateReplace(state, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateIsCurrentTimestamp - Check if fields in state are at current time
! !INTERFACE:
  function NUOPC_StateIsCurrentTimestamp(state, clock, rc)
! !ARGUMENTS:
    logical :: NUOPC_StateIsCurrentTimestamp
    type(ESMF_State),        intent(inout)         :: state
    type(ESMF_Clock),        intent(in)            :: clock
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    type(ESMF_Field)                      :: field
    type(ESMF_Time)         :: time, fieldTime
    integer                 :: i, valueList(9)
    logical                 :: isCurrent
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdItemNameList=stdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    isCurrent = .true. ! initialize
    
    do i=1, size(stdItemNameList)
      call ESMF_StateGet(state, field=field, itemName=stdItemNameList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeGet(field, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_TimeSet(fieldTime, &
        yy=valueList(1), mm=ValueList(2), dd=ValueList(3), &
         h=valueList(4),  m=ValueList(5),  s=ValueList(6), &
        ms=valueList(7), us=ValueList(8), ns=ValueList(9), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (fieldTime /= time) then
        isCurrent = .false.
        exit
      endif
    enddo
    
    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdItemNameList)) deallocate(stdItemNameList)
    
    NUOPC_StateIsCurrentTimestamp = isCurrent
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldBundleUpdateTime - Update the time stamp on all fiels in fieldbundle
! !INTERFACE:
  subroutine NUOPC_FieldBundleUpdateTime(srcFields, dstFields, rc)
! !ARGUMENTS:
    type(ESMF_FieldBundle),  intent(inout)         :: srcFields
    type(ESMF_FieldBundle),  intent(inout)         :: dstFields
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    type(ESMF_Field)        :: srcField, dstField
    integer                 :: i, valueList(9), srcCount, dstCount
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (srcCount /= dstCount) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="count mismatch",&
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    do i=1, srcCount    
      call ESMF_FieldBundleGet(srcFields, fieldIndex=i, field=srcField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_FieldBundleGet(dstFields, fieldIndex=i, field=dstField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeGet(srcField, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(dstField, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCreateSimpleXY - Create a simple XY cartesian grid
! !INTERFACE:
  function NUOPC_GridCreateSimpleXY(x_min, y_min, x_max, y_max, &
    i_count, j_count, rc)
! !ARGUMENTS:
    type(ESMF_Grid):: NUOPC_GridCreateSimpleXY
    real(ESMF_KIND_R8), intent(in)            :: x_min, x_max, y_min, y_max
    integer,            intent(in)            :: i_count, j_count
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer :: i, j, imin_t, imax_t, jmin_t, jmax_t
    real(ESMF_KIND_R8), pointer :: CoordX(:), CoordY(:)
    real(ESMF_KIND_R8):: dx, dy
    type(ESMF_Grid):: grid
    
    if (present(rc)) rc = ESMF_SUCCESS

    dx = (x_max-x_min)/i_count
    dy = (y_max-y_min)/j_count

    grid = ESMF_GridCreateShapeTile(maxIndex=(/i_count,j_count/), &
      coordDep1=(/1/), coordDep2=(/2/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      indexflag=ESMF_INDEX_GLOBAL, name="SimpleXY", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! add center stagger
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, farrayPtr=coordX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, farrayPtr=coordY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compute center stagger coordinate values
    imin_t = lbound(CoordX,1)
    imax_t = ubound(CoordX,1)
    jmin_t = lbound(CoordY,1)
    jmax_t = ubound(CoordY,1)
      
    coordX(imin_t) = (imin_t-1)*dx + 0.5*dx
    do i = imin_t+1, imax_t
      coordX(i) = coordX(i-1) + dx
    enddo
    coordY(jmin_t) = (jmin_t-1)*dy + 0.5*dy
    do j = jmin_t+1, jmax_t
      coordY(j) = coordY(j-1) + dy
    enddo
    
    NUOPC_GridCreateSimpleXY = grid
    
  end function
  !-----------------------------------------------------------------------------

end module
