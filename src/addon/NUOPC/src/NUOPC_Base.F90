! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2014, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Base.F90"
!==============================================================================

!TODO: make this macros available through ESMF as parameter or find other way
#define ESMF_INIT_CREATED 82949521

module NUOPC_Base

  !-----------------------------------------------------------------------------
  ! Generic code collection
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC_FieldDictionaryDef

  implicit none
  
  private
  
  ! private module variables
  logical, save :: NUOPC_FieldDictionaryIsSetup = .false.
  
  ! public module variables
  type(ESMF_Container), save  :: NUOPC_FieldDictionary
  public NUOPC_FieldDictionary
  integer, parameter          :: NUOPC_PhaseMapStringLength = 30
  public NUOPC_PhaseMapStringLength

  ! public module interfaces
  public NUOPC_ClockCheckSetClock
  public NUOPC_ClockInitialize
  public NUOPC_ClockPrintCurrTime
  public NUOPC_ClockPrintStartTime
  public NUOPC_ClockPrintStopTime
  public NUOPC_CplCompAreServicesSet
  public NUOPC_CplCompAttributeAdd
  public NUOPC_CplCompAttributeGet
  public NUOPC_CplCompAttributeSet
  public NUOPC_FieldAttributeAdd
  public NUOPC_FieldAttributeGet
  public NUOPC_FieldAttributeSet
  public NUOPC_FieldBundleUpdateTime
  public NUOPC_FieldDictionaryAddEntry  
  public NUOPC_FieldDictionaryGetEntry  
  public NUOPC_FieldDictionaryHasEntry  
  public NUOPC_FieldDictionarySetup
  public NUOPC_FieldIsAtTime
  public NUOPC_FieldWrite
  public NUOPC_FillCplList
  public NUOPC_GridCompAreServicesSet  
  public NUOPC_GridCompAttributeAdd
  public NUOPC_GridCompCheckSetClock
  public NUOPC_GridCompSetClock
  public NUOPC_GridCompSetServices
  public NUOPC_GridCreateSimpleSph
  public NUOPC_GridCreateSimpleXY
  public NUOPC_IsCreated
  public NUOPC_Nop
  public NUOPC_StateAdvertiseField
  public NUOPC_StateAdvertiseFields
  public NUOPC_StateBuildStdList
  public NUOPC_StateIsAllConnected
  public NUOPC_StateIsAtTime
  public NUOPC_StateIsFieldConnected
  public NUOPC_StateIsUpdated
  public NUOPC_StateRealizeField
  public NUOPC_StateSetTimestamp
  public NUOPC_StateUpdateTimestamp
  public NUOPC_StateWrite
  public NUOPC_TimePrint
  

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  interface NUOPC_IsCreated
    module procedure NUOPC_ClockIsCreated
    module procedure NUOPC_FieldBundleIsCreated
    module procedure NUOPC_FieldIsCreated
    module procedure NUOPC_GridIsCreated
  end interface
  
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ClockCheckSetClock - Check a Clock for compatibility
! !INTERFACE:
  subroutine NUOPC_ClockCheckSetClock(setClock, checkClock, &
    setStartTimeToCurrent, rc)
! !ARGUMENTS:
    type(ESMF_Clock),        intent(inout)         :: setClock
    type(ESMF_Clock),        intent(in)            :: checkClock
    logical,                 intent(in),  optional :: setStartTimeToCurrent
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Compares {\tt setClock} to {\tt checkClock} to make sure they match in
!   their current Time. Further ensures that {\tt checkClock}'s timeStep is a
!   multiple of {\tt setClock}'s timeStep. If both these condition are satisfied
!   then the stopTime of the {\tt setClock} is set one {\tt checkClock}'s
!   timeStep ahead of the current Time, taking into account the direction of 
!   the Clock.
!
!   By default the startTime of the {\tt setClock} is not modified. However, if
!   {\tt setStartTimeToCurrent == .true.} the startTime of {\tt setClock} is set
!   to the currentTime of {\tt checkClock}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables    
    type(ESMF_Time)           :: checkCurrTime, currTime, stopTime, startTime
    type(ESMF_TimeInterval)   :: checkTimeStep, timeStep
    type(ESMF_Direction_Flag) :: direction
    type(ESMF_Time)           :: setTime

    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ClockGet(checkClock, currTime=checkCurrTime, &
      timeStep=checkTimeStep, direction=direction, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call ESMF_ClockGet(setClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! ensure the current times match between checkClock and setClock
    if (currTime /= checkCurrTime) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="setClock and checkClock do not match in current time!", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! ensure that the check timestep is a multiple of the internal one
    if (ceiling(checkTimeStep/timeStep) /= floor(checkTimeStep/timeStep))&
      then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="checkClock timestep is not multiple of setClock timestep!", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! set the new stopTime of the setClock
    if (direction==ESMF_DIRECTION_FORWARD) then
      stopTime = currTime + checkTimeStep
    else
      stopTime = currTime - checkTimeStep
    endif
    call ESMF_ClockSet(setClock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
   ! conditionally set startTime of the setClock
   if (present(setStartTimeToCurrent)) then
      if (setStartTimeToCurrent) then
        call ESMF_ClockGet(checkClock, currTime=setTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        call ESMF_ClockSet(setClock, startTime=setTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ClockInitialize - Initialize a new Clock from Clock and stabilityTimeStep
! !INTERFACE:
  function NUOPC_ClockInitialize(externalClock, stabilityTimeStep, rc)
! !RETURN VALUE:
    type(ESMF_Clock) :: NUOPC_ClockInitialize
! !ARGUMENTS:
    type(ESMF_Clock)                               :: externalClock
    type(ESMF_TimeInterval), intent(in),  optional :: stabilityTimeStep
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Returns a new Clock instance that is a copy of the incoming Clock, but
!   potentially with a smaller timestep. The timestep is chosen so that the
!   timestep of the incoming Clock ({\tt externalClock}) is a multiple of the
!   new Clock's timestep, and at the same time the new timestep is <= 
!   the {\tt stabilityTimeStep}.
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
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
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
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    
      internalStepCount = ceiling(externalTimeStep / stabilityTimeStep)
      actualTimeStep = externalTimeStep / internalStepCount
    
      call ESMF_ClockSet(internalClock, timeStep=actualTimeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
      
    NUOPC_ClockInitialize = internalClock
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ClockPrintCurrTime - Formatted print ot current time
! !INTERFACE:
  subroutine NUOPC_ClockPrintCurrTime(clock, string, unit, rc)
! !ARGUMENTS:
    type(ESMF_Clock), intent(in)            :: clock
    character(*),     intent(in),  optional :: string
    character(*),     intent(out), optional :: unit
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Writes the formatted current time of {\tt clock} to {\tt unit}. Prepends 
!   {\tt string} if provided. If {\tt unit} is present it must be an internal
!   unit, i.e. a string variable. If {\tt unit} is not present then the output
!   is written to the default external unit (typically that would be stdout).
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Time)         :: currTime
    if (present(rc)) rc = ESMF_SUCCESS
  
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call NUOPC_TimePrint(currTime, string, unit, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  end subroutine

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ClockPrintStartTime - Formatted print ot start time
! !INTERFACE:
  subroutine NUOPC_ClockPrintStartTime(clock, string, unit, rc)
! !ARGUMENTS:
    type(ESMF_Clock), intent(in)            :: clock
    character(*),     intent(in),  optional :: string
    character(*),     intent(out), optional :: unit
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Writes the formatted start time of {\tt clock} to {\tt unit}. Prepends 
!   {\tt string} if provided. If {\tt unit} is present it must be an internal
!   unit, i.e. a string variable. If {\tt unit} is not present then the output
!   is written to the default external unit (typically that would be stdout).
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Time)         :: startTime
    if (present(rc)) rc = ESMF_SUCCESS
  
    call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call NUOPC_TimePrint(startTime, string, unit, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ClockPrintStopTime - Formatted print ot stop time
! !INTERFACE:
  subroutine NUOPC_ClockPrintStopTime(clock, string, unit, rc)
! !ARGUMENTS:
    type(ESMF_Clock), intent(in)            :: clock
    character(*),     intent(in),  optional :: string
    character(*),     intent(out), optional :: unit
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Writes the formatted stop time of {\tt clock} to {\tt unit}. Prepends 
!   {\tt string} if provided. If {\tt unit} is present it must be an internal
!   unit, i.e. a string variable. If {\tt unit} is not present then the output
!   is written to the default external unit (typically that would be stdout).
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Time)         :: stopTime
    if (present(rc)) rc = ESMF_SUCCESS
  
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call NUOPC_TimePrint(stopTime, string, unit, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CplCompAreServicesSet - Check if SetServices was called
! !INTERFACE:
  function NUOPC_CplCompAreServicesSet(comp, rc)
! !RETURN VALUE:
    logical :: NUOPC_CplCompAreServicesSet
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: comp
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if SetServices has been called for {\tt comp}.
!   Otherwise returns {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Pointer)      :: vm_info
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! make a copy of the external externalClock
    call ESMF_CompGet(comp%compp, vm_info=vm_info, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
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
! !IROUTINE: NUOPC_CplCompAttributeAdd - Add the NUOPC CplComp Attributes
! !INTERFACE:
  subroutine NUOPC_CplCompAttributeAdd(comp, rc)
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)         :: comp
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Adds standard NUOPC Attributes to a Coupler Component. Checks the provided
!   importState and exportState arguments for matching Fields and adds the list
!   as "CplList" Attribute.
!
!   This adds the standard NUOPC Coupler Attribute package: convention="NUOPC", 
!   purpose="General" to the Field. The NUOPC Coupler Attribute package extends
!   the ESG Component Attribute package: convention="ESG", purpose="General".
!
!   The arguments are:
!   \begin{description}
!   \item[comp]
!     The {\tt ESMF\_CplComp} object to which the Attributes are added.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: attrList(3)

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! Set up a customized list of Attributes to be added to the CplComp
    attrList(1) = "Verbosity"           ! control verbosity
    attrList(2) = "InitializePhaseMap"  ! list of strings to map str to phase #
    attrList(3) = "CplList"
    
    ! add Attribute packages
    call ESMF_AttributeAdd(comp, convention="ESG", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeAdd(comp, convention="NUOPC", purpose="General",   &
      attrList=attrList, nestConvention="ESG", nestPurpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
          
    ! set Attributes to defaults
    call ESMF_AttributeSet(comp, &
      name="Verbosity", value="low", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CplCompAttributeGet - Get a NUOPC CplComp Attribute
! !INTERFACE:
  subroutine NUOPC_CplCompAttributeGet(comp, cplList, cplListSize, rc)
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: comp
    character(*),       intent(out), optional :: cplList(:)
    integer,            intent(out), optional :: cplListSize
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Accesses the "CplList" Attribute inside of {\tt comp} using the
!   convention {\tt NUOPC} and purpose {\tt General}. Returns with error if
!   the Attribute is not present or not set.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    if (present(cplList)) then
      call ESMF_AttributeGet(comp, name="CplList", valueList=cplList, &
        itemCount=cplListSize, convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      call ESMF_AttributeGet(comp, name="CplList", &
        itemCount=cplListSize, convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CplCompAttributeSet - Set the NUOPC CplComp Attributes
! !INTERFACE:
  subroutine NUOPC_CplCompAttributeSet(comp, importState, exportState, rc)
! !ARGUMENTS:
    type(ESMF_CplComp), intent(inout)         :: comp
    type(ESMF_State),   intent(in)            :: importState
    type(ESMF_State),   intent(in)            :: exportState
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Checks the provided importState and exportState arguments for matching Fields
!   and sets the coupling list as "CplList" Attribute in {\tt comp}.
!
!   The arguments are:
!   \begin{description}
!   \item[comp]
!     The {\tt ESMF\_CplComp} object to which the Attributes are set.
!   \item[importState]
!     Import State.
!   \item[exportState]
!     Export State.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer :: cplList(:)
    integer                         :: count

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! find cplList
    nullify(cplList)
    call NUOPC_FillCplList(importState, exportState, cplList=cplList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set Attributes
    call ESMF_AttributeSet(comp, &
      name="ComponentLongName", value="NUOPC Generic Connector Component", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    if (associated(cplList)) then
      count = size(cplList)
      if (count>0) then
        call ESMF_AttributeSet(comp, &
          name="CplList", valueList=cplList(1:count), &
          convention="NUOPC", purpose="General", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif
      deallocate(cplList)
    endif
      
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldAttributeAdd - Add the NUOPC Field Attributes
! !INTERFACE:
  subroutine NUOPC_FieldAttributeAdd(field, StandardName, Units, LongName, &
    ShortName, Connected, rc)
! !ARGUMENTS:
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: StandardName
    character(*), intent(in),  optional   :: Units
    character(*), intent(in),  optional   :: LongName
    character(*), intent(in),  optional   :: ShortName
    character(*), intent(in),  optional   :: Connected
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Adds standard NUOPC Attributes to a Field object. Checks the provided
!   arguments against the NUOPC Field Dictionary. Omitted optional
!   information is filled in using defaults out of the NUOPC Field Dictionary.
!
!   This adds the standard NUOPC Field Attribute package: convention="NUOPC", 
!   purpose="General" to the Field. The NUOPC Field Attribute package extends
!   the ESG Field Attribute package: convention="ESG", purpose="General". 
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object to which the Attributes are added.
!   \item[StandardName]
!     The StandardName of the Field. Must be a StandardName found in
!     the  NUOPC Field Dictionary.
!   \item[{[Units]}]
!     The Units of the Field. Must be convertible to the canonical
!     units specified in the NUOPC Field Dictionary for the specified
!     StandardName.
!     If omitted, the default is to use the canonical units associated with
!     the StandardName in the NUOPC Field Dictionary.
!   \item[{[LongName]}]
!     The LongName of the Field. NUOPC does not restrict the value
!     of this variable.
!     If omitted, the default is to use the LongName associated with 
!     the StandardName in the NUOPC Field Dictionary.
!   \item[{[ShortName]}]
!     The ShortName of the Field. NUOPC does not restrict the value
!     of this variable.
!     If omitted, the default is to use the ShortName associated with 
!     the StandardName in the NUOPC Field Dictionary.
!   \item[{[Connected]}]
!     The connection status of the Field. Must be one of the NUOPC supported
!     values: {\tt false} or {\tt true}.
!     If omitted, the default is a connected status of {\tt false}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)            :: attrList(7)
    character(ESMF_MAXSTR)            :: tempString
    logical                           :: accepted
    integer                           :: i
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be added to the Fields
    attrList(1) = "Connected"  ! values: "true" or "false"
    attrList(2) = "TimeStamp"  ! values: list of 9 integers: yy,mm,dd,h,m,s,ms,us,ns
    attrList(3) = "ProducerConnection"! values: "open", "targeted", "connected"
    attrList(4) = "ConsumerConnection"! values: "open", "targeted", "connected"
    attrList(5) = "Updated" ! values: "true" or "false"
    attrList(6) = "TransferOfferGeomObject" ! values: "cannot provide",
                                            !   "can provide", "will provide"
    attrList(7) = "TransferActionGeomObject" ! values: "provide", "accept"
    
    ! add Attribute packages
    call ESMF_AttributeAdd(field, convention="ESG", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeAdd(field, convention="NUOPC", purpose="General",   &
      attrList=attrList, nestConvention="ESG", nestPurpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! Attributes don't offer controlled vocabulary checking (yet) -> do it here!
    ! first ensure that NUOPC_FieldDictionary is set up
    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! check that StandardName has an entry in the NUOPC_FieldDictionary
    call ESMF_ContainerGet(NUOPC_FieldDictionary, itemName=StandardName, &
      isPresent=accepted, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (.not.accepted) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=StandardName//" is not a StandardName in the NUOPC_FieldDictionary!",&
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif
    call ESMF_ContainerGetUDT(NUOPC_FieldDictionary, trim(StandardName), &
      fdEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! set StandardName
    call ESMF_AttributeSet(field, &
      name="StandardName", value=trim(StandardName), &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! set Units
    if (present(Units)) then
      if ((trim(Units))/=trim(fdEntry%wrap%canonicalUnits)) then
        ! not the same as canoncial units
        accepted = .false. ! reset
        ! TODO: implement access to UDUNITS-2 to figure if Units can be 
        ! TODO: converted to the canonicalUnits, if so then o.k.
        if (.not.accepted) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg=Units//" cannot be converted to the canonical units in "// &
              " NUOPC_FieldDictionary for StandardName: "//StandardName,&
              line=__LINE__, file=FILENAME, rcToReturn=rc)
          return  ! bail out
        endif
      endif
      tempString = Units
    else
      tempString = fdEntry%wrap%canonicalUnits  ! default
    endif
    call ESMF_AttributeSet(field, &
      name="Units", value=trim(tempString), &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set LongName
    if (present(LongName)) then
      tempString = LongName
    else
      tempString = fdEntry%wrap%defaultLongName   ! default
    endif
    call ESMF_AttributeSet(field, &
      name="LongName", value=trim(tempString), &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set ShortName
    if (present(ShortName)) then
      tempString = ShortName
    else
      tempString = fdEntry%wrap%defaultShortName  ! default
    endif
    call ESMF_AttributeSet(field, &
      name="ShortName", value=trim(tempString), &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set Connected
    if (present(Connected)) then
      accepted = .false. ! reset
      do i=1, size(fdEntry%wrap%connectedOptions)
        if ((trim(Connected))==trim(fdEntry%wrap%connectedOptions(i))) then
          accepted = .true.
          exit
        endif
      enddo
      if (.not.accepted) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=Connected//" is not a supported Connected value in the "// &
            " NUOPC_FieldDictionary for StandardName: "//StandardName,&
            line=__LINE__, file=FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      tempString = Connected
    else
      tempString = fdEntry%wrap%connectedOptions(1)  ! default
    endif
    call ESMF_AttributeSet(field, &
      name="Connected", value=trim(tempString), &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set TimeStamp
    call ESMF_AttributeSet(field, &
      name="TimeStamp", valueList=(/0,0,0,0,0,0,0,0,0/), &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set ProducerConnection
    call ESMF_AttributeSet(field, &
      name="ProducerConnection", value="open", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set ConsumerConnection
    call ESMF_AttributeSet(field, &
      name="ConsumerConnection", value="open", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set Updated
    call ESMF_AttributeSet(field, &
      name="Updated", value="false", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set TransferOfferGeomObject
    call ESMF_AttributeSet(field, &
      name="TransferOfferGeomObject", value="will provide", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set TransferActionGeomObject
    call ESMF_AttributeSet(field, &
      name="TransferActionGeomObject", value="provide", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldAttributeGet - Get a NUOPC Field Attribute
! !INTERFACE:
  subroutine NUOPC_FieldAttributeGet(field, name, value, rc)
! !ARGUMENTS:
    type(ESMF_Field), intent(in)            :: field
    character(*),     intent(in)            :: name
    character(*),     intent(out)           :: value
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Accesses the Attribute {\tt name} inside of {\tt field} using the
!   convention {\tt NUOPC} and purpose {\tt General}. Returns with error if
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
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (trim(value) == trim(defaultvalue)) then
      ! attribute not present
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not present",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    else if (len_trim(value) == 0) then
      ! attribute present but not set
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not set",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldAttributeSet - Set a NUOPC Field Attribute
! !INTERFACE:
  subroutine NUOPC_FieldAttributeSet(field, name, value, rc)
! !ARGUMENTS:
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: name
    character(*), intent(in)              :: value
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the Attribute {\tt name} inside of {\tt field} using the
!   convention {\tt NUOPC} and purpose {\tt General}.
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(field, name=name, value=value, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldBundleUpdateTime - Update the time stamp on all Fields in a FieldBundle
! !INTERFACE:
  subroutine NUOPC_FieldBundleUpdateTime(srcFields, dstFields, rc)
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)            :: srcFields
    type(ESMF_FieldBundle), intent(inout)         :: dstFields
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Updates the time stamp on all Fields in the {\tt dstFields} FieldBundle to
!   be the same as in the {\tt dstFields} FieldBundle.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)              :: srcField, dstField
    type(ESMF_Field), allocatable :: srcFieldList(:)
    type(ESMF_Field), allocatable :: dstFieldList(:)
    integer                       :: i, valueList(9), srcCount, dstCount
    
!gjtdebug    character(ESMF_MAXSTR)  :: tempString1, tempString2
!gjtdebug    character(5*ESMF_MAXSTR):: msgString
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (srcCount /= dstCount) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="count mismatch",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    allocate(srcFieldList(srcCount))
    call ESMF_FieldBundleGet(srcFields, fieldList=srcFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    allocate(dstFieldList(dstCount))
    call ESMF_FieldBundleGet(dstFields, fieldList=dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    do i=1, srcCount    
      srcField = srcFieldList(i)
      dstField = dstFieldList(i)
      call ESMF_AttributeGet(srcField, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
!gjtdebug call ESMF_FieldGet(srcField, name=tempString1)        
!gjtdebug call ESMF_FieldGet(dstField, name=tempString2)        
!gjtdebug write (msgString, *) "updating TimeStamp:", trim(tempString1), " -> ", trim(tempString2), srcField%ftypep%base
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
!gjtdebug write (msgString, *) valueList
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        
      call ESMF_AttributeSet(dstField, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    enddo
    deallocate(srcFieldList, dstFieldList)

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionaryAddEntry - Add an entry to the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryAddEntry(standardName, canonicalUnits, &
    defaultLongName, defaultShortName, rc)
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName
    character(*),                 intent(in)            :: canonicalUnits
    character(*),                 intent(in),  optional :: defaultLongName
    character(*),                 intent(in),  optional :: defaultShortName
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Adds an entry to the NUOPC Field dictionary. If necessary the dictionary is
!   first set up.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call NUOPC_FieldDictionaryAddEntryI(NUOPC_FieldDictionary, &
      standardName = standardName, canonicalUnits = canonicalUnits, &
      defaultLongName = defaultLongName, defaultShortName = defaultShortName, &
      rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionaryGetEntry - Get information about a NUOPC Field dictionary entry
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryGetEntry(standardName, canonicalUnits, &
    defaultLongName, defaultShortName, rc)
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName
    character(*),                 intent(out), optional :: canonicalUnits
    character(*),                 intent(out), optional :: defaultLongName
    character(*),                 intent(out), optional :: defaultShortName
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Returns the canonical units, the default LongName and the default ShortName
!   that the NUOPC Field dictionary associates with a StandardName.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call NUOPC_FieldDictionaryGetEntryI(NUOPC_FieldDictionary, &
      standardName = standardName, canonicalUnits = canonicalUnits, &
      defaultLongName = defaultLongName, defaultShortName = defaultShortName, &
      rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionaryHasEntry - Check whether the NUOPC Field dictionary has a specific entry
! !INTERFACE:
  function NUOPC_FieldDictionaryHasEntry(standardName, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldDictionaryHasEntry
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if the NUOPC Field dictionary has an entry with the
!   specified StandardName, {\tt .false.} otherwise.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    NUOPC_FieldDictionaryHasEntry = &
      NUOPC_FieldDictionaryHasEntryI(NUOPC_FieldDictionary, &
      standardName = standardName, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionarySetup - Setup the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionarySetup(rc)
! !ARGUMENTS:
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Setup the NUOPC Field dictionary.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    if (.not.NUOPC_FieldDictionaryIsSetup) then
    
      NUOPC_FieldDictionary = ESMF_ContainerCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    
      call ESMF_ContainerGarbageOn(NUOPC_FieldDictionary, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out

      call NUOPC_FieldDictionaryDefinition(NUOPC_FieldDictionary, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      
      NUOPC_FieldDictionaryIsSetup = .true.
      
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldIsAtTime - Check if the Field is at the given Time
! !INTERFACE:
  function NUOPC_FieldIsAtTime(field, time, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldIsAtTime
! !ARGUMENTS:
    type(ESMF_Field), intent(in)            :: field
    type(ESMF_Time),  intent(in)            :: time
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if the Field has a timestamp 
!   that matches {\tt time}. Otherwise returns {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Time)         :: fieldTime
    integer                 :: i, valueList(9)
    type(ESMF_CalKind_Flag) :: calkindflag
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    NUOPC_FieldIsAtTime = .true. ! initialize
    
    call ESMF_TimeGet(time, calkindflag=calkindflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_AttributeGet(field, &
      name="TimeStamp", valueList=valueList, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (ValueList(2)==0) then
      ! month value of 0 is indicative of an uninitialized time stamp
      NUOPC_FieldIsAtTime = .false.
      return
    else
      call ESMF_TimeSet(fieldTime, &
        yy=valueList(1), mm=ValueList(2), dd=ValueList(3), &
         h=valueList(4),  m=ValueList(5),  s=ValueList(6), &
        ms=valueList(7), us=ValueList(8), ns=ValueList(9), &
        calkindflag=calkindflag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (fieldTime /= time) then
        NUOPC_FieldIsAtTime = .false.
        return
      endif
    endif

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldWrite - Write Field data into a file under the StandardName Attribute
! !INTERFACE:
  subroutine NUOPC_FieldWrite(field, file, overwrite, status, timeslice, &
    iofmt, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_Field),           intent(in)            :: field 
    character(*),               intent(in)            :: file 
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional :: iofmt
    logical,                    intent(in),  optional :: relaxedflag
    integer,                    intent(out), optional :: rc
! !DESCRIPTION:
!   Write Field data into a file under the StandardName Attribute. If the
!   {\tt relaxedflag} is provided, set to {\tt .true.}, then no error is
!   returned even if the call cannot write the file due to library limitations.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: standardName
    logical                 :: ioCapable
    logical                 :: doItFlag

    if (present(rc)) rc = ESMF_SUCCESS
    
    ioCapable = (ESMF_IO_PIO_PRESENT .and. &
      (ESMF_IO_NETCDF_PRESENT .or. ESMF_IO_PNETCDF_PRESENT))
    
    doItFlag = .true. ! default
    if (present(relaxedFlag)) then
      doItFlag = .not.relaxedflag .or. (relaxedflag.and.ioCapable)
    endif
    
    if (doItFlag) then
      
      call NUOPC_FieldAttributeGet(field, name="StandardName", &
        value=standardName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    
      call ESMF_FieldWrite(field, file=file, variableName=standardName, &
        overwrite=overwrite, status=status, timeslice=timeslice, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FillCplList - Fill the cplList according to matching Fields
! !INTERFACE:
  subroutine NUOPC_FillCplList(importState, exportState, cplList, rc)
! !ARGUMENTS:
    type(ESMF_State),       intent(in)            :: importState
    type(ESMF_State),       intent(in)            :: exportState
    character(ESMF_MAXSTR), pointer               :: cplList(:)
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Constructs a list of matching StandardNames of Fields in the 
!   {\tt importState} and {\tt exportState}. Returns this list in {\tt cplList}.
!
!   The pointer argument {\tt cplList} must enter this method unassociated. On
!   return, the deallocation of the potentially associated pointer becomes the
!   caller's responsibility.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: maxCount, count, i, j
    character(ESMF_MAXSTR), pointer :: l_cplList(:)
    character(ESMF_MAXSTR), pointer :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer :: exportStandardNameList(:)
    type(ESMF_Field),       pointer :: importFieldList(:)
    type(ESMF_Field),       pointer :: exportFieldList(:)
    type(ESMF_Field)                :: field
    character(ESMF_MAXSTR)          :: consumerConnection
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! ensure cplList argument enters unassociated
    if (associated(cplList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="cplList must enter unassociated", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif

    ! nullify pointers to prepare for the following calls
    nullify(importStandardNameList)
    nullify(exportStandardNameList)
    nullify(importFieldList)
    nullify(exportFieldList)
    
    ! build list of standard names of all Fields inside of importState
    call NUOPC_StateBuildStdList(importState, importStandardNameList, &
      stdFieldList=importFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! build list of standard names of all Fields inside of exportState
    call NUOPC_StateBuildStdList(exportState, exportStandardNameList, &
      stdFieldList=exportFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! associated pointers means that there are name lists
    if (associated(importStandardNameList) .and. &
      associated(exportStandardNameList)) then
      
      ! the maximum number of matches is limited by the larger list, because
      ! the same producer can be matched to multiple consumers
      maxCount = max(size(importStandardNameList), size(exportStandardNameList))
      allocate(l_cplList(maxCount)) ! temporary list

      count = 0
      ! simple linear search of items that match between both lists
      do i=1, size(importStandardNameList)  ! producer side
        do j=1, size(exportStandardNameList)  ! consumer side
          if (importStandardNameList(i) == exportStandardNameList(j)) then
            ! found matching standard name pair
            ! -> now see if the consumer side is already satisfied
            field = exportFieldList(j)
            call NUOPC_FieldAttributeGet(field, name="ConsumerConnection", &
              value=consumerConnection, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            if (trim(consumerConnection)/="satisfied") then
              ! the consumer side was still open
              call NUOPC_FieldAttributeSet(field, name="ConsumerConnection", &
                value="satisfied", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
              count = count+1
              if (count > maxCount) then
                call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                  msg="Bad internal error - should never get here!",&
                  line=__LINE__, file=FILENAME, rcToReturn=rc)
                return  ! bail out
              endif
              l_cplList(count) = importStandardNameList(i)
              exit
            endif
          endif
        enddo
      enddo
      
      ! transfer info: l_cplList -> cplList
      allocate(cplList(count))
      do i=1, count
        cplList(i) = l_cplList(i)
      enddo
      deallocate(l_cplList)
      
    endif
    
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    if (associated(importFieldList)) deallocate(importFieldList)
    if (associated(exportFieldList)) deallocate(exportFieldList)
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCompAreServicesSet - Check if SetServices was called
! !INTERFACE:
  function NUOPC_GridCompAreServicesSet(comp, rc)
! !RETURN VALUE:
    logical :: NUOPC_GridCompAreServicesSet
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: comp
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if SetServices has been called for {\tt comp}. 
!   Otherwise returns {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Pointer)      :: vm_info
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! make a copy of the external externalClock
    call ESMF_CompGet(comp%compp, vm_info=vm_info, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
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
! !IROUTINE: NUOPC_GridCompAttributeAdd - Add the NUOPC GridComp Attributes
! !INTERFACE:
  subroutine NUOPC_GridCompAttributeAdd(comp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                   :: comp
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Adds standard NUOPC Attributes to a Gridded Component.
!
!   This adds the standard NUOPC GridComp Attribute package: convention="NUOPC",
!   purpose="General" to the Gridded Component. The NUOPC GridComp Attribute
!   package extends the CIM Component Attribute package: convention="CIM 1.5",
!   purpose="ModelComp".
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)            :: attrList(7)
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be added to the Fields
    attrList(1) = "Verbosity"           ! control verbosity
    attrList(2) = "InitializePhaseMap"  ! list of strings to map str to phase #
    attrList(3) = "InternalInitializePhaseMap"  ! list of strings to map str to phase #
    attrList(4) = "NestingGeneration" ! values: integer starting 0 for parent
    attrList(5) = "Nestling"  ! values: integer starting 0 for first nestling
    attrList(6) = "InitializeDataComplete"  ! values: strings "false"/"true"
    attrList(7) = "InitializeDataProgress"  ! values: strings "false"/"true"
    
    ! add Attribute packages
if (ESMF_VERSION_MAJOR >= 6) then
    call ESMF_AttributeAdd(comp, convention="CIM 1.5", &
      purpose="ModelComp", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeAdd(comp, convention="NUOPC", purpose="General",   &
      attrList=attrList, nestConvention="CIM 1.5", &
      nestPurpose="ModelComp", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
else
! gjt: keep this branch for now because I have an old sandbox where most of
! the ESMF is back in the MAJOR 5 because of I/O issues in MAJOR 6.
! TODO: remove this else branch once the I/O issues in MAJOR 6 are fixed.
    call ESMF_AttributeAdd(comp, convention="CIM", &
      purpose="Model Component Simulation Description", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeAdd(comp, convention="NUOPC", purpose="General",   &
      attrList=attrList, nestConvention="CIM", &
      nestPurpose="Model Component Simulation Description", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
endif

    ! set Attributes to defaults
    call ESMF_AttributeSet(comp, &
      name="Verbosity", value="low", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeSet(comp, &
      name="NestingGeneration", value=0, &        ! default to parent level
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeSet(comp, &
      name="Nestling", value=0, &                 ! default to first nestling
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeSet(comp, &
      name="InitializeDataComplete", value="false", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_AttributeSet(comp, &
      name="InitializeDataProgress", value="false", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCompCheckSetClock - Check Clock compatibility and set stopTime
! !INTERFACE:
  subroutine NUOPC_GridCompCheckSetClock(comp, externalClock, rc)
! !ARGUMENTS:
    type(ESMF_GridComp),     intent(inout)         :: comp
    type(ESMF_Clock),        intent(in)            :: externalClock
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Compares {\tt externalClock} to the Component internal Clock to make sure
!   they match in their current Time. Further ensures that the external Clock's
!   timeStep is a multiple of the internal Clock's timeStep. If both
!   these condition are satisfied then the stopTime of the internal Clock is
!   set to be reachable in one timeStep of the external Clock, taking into
!   account the direction of the Clock.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables    
    type(ESMF_Clock)        :: internalClock

    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(comp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    call NUOPC_ClockCheckSetClock(setClock=internalClock, &
      checkClock=externalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCompSetClock - Initialize and set the internal Clock of a GridComp
! !INTERFACE:
  subroutine NUOPC_GridCompSetClock(comp, externalClock, stabilityTimeStep, &
                                     rc)
! !ARGUMENTS:
    type(ESMF_GridComp),     intent(inout)         :: comp
    type(ESMF_Clock),        intent(in)            :: externalClock
    type(ESMF_TimeInterval), intent(in),  optional :: stabilityTimeStep
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Sets the Component internal Clock as a copy of {\tt externalClock}, but
!   with a timeStep that is less than or equal to the stabilityTimeStep.
!   At the same time ensures that the timeStep of the external Clock is
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
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_GridCompSet(comp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCompSetServices - Try to find and call SetServices in a shared object
! !INTERFACE:
  recursive subroutine NUOPC_GridCompSetServices(comp, sharedObj, userRc, rc)
! !ARGUMENTS:
    type(ESMF_GridComp),     intent(inout)         :: comp
    character(len=*),        intent(in),  optional :: sharedObj
    integer,                 intent(out), optional :: userRc
    integer,                 intent(out), optional :: rc
! !DESCRIPTION:
!   Try to find a routine called "SetServices" in the sharedObj and execute it
!   to set the component's services. An attempt is made to find a routine that
!   is close in name to "SetServices", allowing compiler name mangeling, i.e.
!   upper and lower case, as well as trailing underscores.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    logical           :: userRoutineFound

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! attempt to find something called SetServices, allowing variations
    ! caused by compiler name mangeling
    
    call ESMF_GridCompSetServices(comp, userRoutine="setservices", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="setservices_", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="setservices__", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SETSERVICES", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SETSERVICES_", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SETSERVICES__", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SetServices", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully
      
    call ESMF_GridCompSetServices(comp, userRoutine="SetServices_", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully

    call ESMF_GridCompSetServices(comp, userRoutine="SetServices__", &
      sharedObj=sharedObj, userRoutineFound=userRoutineFound, &
      userRc=userRc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (userRoutineFound) return ! bail out successfully

    ! getting down to here means that none of the attempts were successful
    if (present(sharedObj)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Could not find a matching SetServices routine in "//trim(sharedObj),&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Could not find a matching SetServices routine in the executable.", &
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
    endif
      
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCreateSimpleSph - Create a simple Spherical Grid
! !INTERFACE:
  function NUOPC_GridCreateSimpleSph(x_min, y_min, x_max, y_max, &
    i_count, j_count, area_adj, tag, scheme, rc)
! !RETURN VALUE:
    type(ESMF_Grid):: NUOPC_GridCreateSimpleSph
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(in)            :: x_min, x_max, y_min, y_max
    integer,            intent(in)            :: i_count, j_count
    real(ESMF_KIND_R4), intent(in),  optional :: area_adj
    character(len=*),   intent(in),  optional :: tag
    integer,            intent(in) , optional :: scheme
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Creates and returns a simple spherical Grid.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                   :: nx, ny
    real(ESMF_KIND_R8)                        :: dx, dy, sx, sy
    integer                                   :: i, j
    real(ESMF_KIND_R8), pointer               :: coordX(:,:), coordY(:,:)
    real(ESMF_KIND_R8), pointer               :: f_area(:,:), f_area_m(:)
    real(ESMF_KIND_R8), pointer               :: o_area(:,:)
    real(ESMF_KIND_R8)                        :: startx, starty
    integer                                   :: l_scheme
    type(ESMF_Mesh)                           :: mesh
    type(ESMF_Field)                          :: field
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! convert to input variables to the internal variables
    sx = x_min
    sy = y_min
    nx = i_count
    ny = j_count
    dx = (x_max - x_min) / nx
    dy = (y_max - y_min) / ny
    
    ! scheme
    l_scheme = ESMF_REGRID_SCHEME_REGION3D
    if(present(scheme)) l_scheme = scheme

    if(l_scheme == ESMF_REGRID_SCHEME_FULL3D) then
      NUOPC_GridCreateSimpleSph = ESMF_GridCreate1PeriDim(maxIndex=(/nx, ny/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
        !regDecomp=(/npet, 1/), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      NUOPC_GridCreateSimpleSph = ESMF_GridCreateNoPeriDim(maxIndex=(/nx, ny/),&
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/1,1/), &
        !regDecomp=(/npet, 1/), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif 

    call ESMF_GridAddCoord(NUOPC_GridCreateSimpleSph, &
      staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridAddCoord(NUOPC_GridCreateSimpleSph, &
      staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! global indexing
    ! atm grid is not decomposed in the y direction
    !startx = lpet*nx/npet*dx
    startx = sx
    starty = sy
    ! compute coord
    ! X center
    call ESMF_GridGetCoord(NUOPC_GridCreateSimpleSph, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, farrayPtr=coordX, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! Y center
    call ESMF_GridGetCoord(NUOPC_GridCreateSimpleSph, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, farrayPtr=coordY, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = startx + dx/2. + (i-1)*dx
        coordY(i,j) = starty + dy/2. + (j-1)*dy
      enddo
    enddo
    !print *, 'startx: ', startx, lbound(coordX, 1), ubound(coordX, 1), 'coordX: ', coordX(:,1)
    ! X corner
    call ESMF_GridGetCoord(NUOPC_GridCreateSimpleSph, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, farrayPtr=coordX, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! Y corner
    call ESMF_GridGetCoord(NUOPC_GridCreateSimpleSph, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, farrayPtr=coordY, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = startx + (i-1)*dx
        coordY(i,j) = starty + (j-1)*dy
      enddo
    enddo

    if(present(area_adj)) then
      ! retrieve area

      !mesh = ESMF_GridToMesh(NUOPC_GridCreateSimpleSph, &
      !  ESMF_STAGGERLOC_CORNER, 0, &
      !  regridConserve=ESMF_REGRID_CONSERVE_ON, rc=rc)
      !if (ESMF_LogFoundError(rc, &
      !    ESMF_ERR_PASSTHRU, &
      !    ESMF_CONTEXT, rcToReturn=rc)) return

      !allocate(f_area_m(mesh%NumOwnedElements))
      !call ESMF_MeshGetElemArea(mesh,  arealist=f_area_m, rc=rc)
      !if (ESMF_LogFoundError(rc, &
      !    ESMF_ERR_PASSTHRU, &
      !    ESMF_CONTEXT, rcToReturn=rc)) return
      !deallocate(f_area_m)

      ! find out original Grid cell area
      field = ESMF_FieldCreate(NUOPC_GridCreateSimpleSph, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call ESMF_FieldRegridGetArea(field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call ESMF_FieldGet(field, farrayPtr=o_area, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      ! add area to Grid
      call ESMF_GridAddItem(NUOPC_GridCreateSimpleSph, ESMF_GRIDITEM_AREA, &
        staggerloc=ESMF_STAGGERLOC_CENTER,  rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      call ESMF_GridGetItem(NUOPC_GridCreateSimpleSph, ESMF_GRIDITEM_AREA, &
        staggerloc=ESMF_STAGGERLOC_CENTER, farrayptr=f_area, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      ! adjust Grid area
      f_area = area_adj*o_area

    endif

    if(present(rc)) rc = ESMF_SUCCESS

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_GridCreateSimpleXY - Create a simple XY cartesian Grid
! !INTERFACE:
  function NUOPC_GridCreateSimpleXY(x_min, y_min, x_max, y_max, &
    i_count, j_count, rc)
! !RETURN VALUE:
    type(ESMF_Grid):: NUOPC_GridCreateSimpleXY
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(in)            :: x_min, x_max, y_min, y_max
    integer,            intent(in)            :: i_count, j_count
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Creates and returns a very simple XY cartesian Grid.
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

    grid = ESMF_GridCreateNoPeriDim(maxIndex=(/i_count,j_count/), &
      coordDep1=(/1/), coordDep2=(/2/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      indexflag=ESMF_INDEX_GLOBAL, name="SimpleXY", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! add center stagger
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, farrayPtr=coordX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, farrayPtr=coordY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compute center stagger coordinate values
    imin_t = lbound(CoordX,1)
    imax_t = ubound(CoordX,1)
    jmin_t = lbound(CoordY,1)
    jmax_t = ubound(CoordY,1)
      
    coordX(imin_t) = x_min + (imin_t-1)*dx + 0.5*dx
    do i = imin_t+1, imax_t
      coordX(i) = coordX(i-1) + dx
    enddo
    coordY(jmin_t) = y_min + (jmin_t-1)*dy + 0.5*dy
    do j = jmin_t+1, jmax_t
      coordY(j) = coordY(j-1) + dy
    enddo
    
    NUOPC_GridCreateSimpleXY = grid
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsCreated - Check whether a Clock object has been created
! !INTERFACE:
  ! call using generic interface: NUOPC_IsCreated
  function NUOPC_ClockIsCreated(clock, rc)
! !RETURN VALUE:
    logical :: NUOPC_ClockIsCreated
! !ARGUMENTS:
    type(ESMF_Clock)               :: clock
    integer, intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if the {\tt clock} is in the
!   created state, {\tt .false.} otherwise.
!EOP
  !-----------------------------------------------------------------------------    
    NUOPC_ClockIsCreated = .false.  ! default assumption
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_ClockGetInit(clock)==ESMF_INIT_CREATED) &
      NUOPC_ClockIsCreated = .true.
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsCreated - Check whether a FieldBundle object has been created
! !INTERFACE:
  ! call using generic interface: NUOPC_IsCreated
  function NUOPC_FieldBundleIsCreated(fieldbundle, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldBundleIsCreated
! !ARGUMENTS:
    type(ESMF_FieldBundle)         :: fieldbundle
    integer, intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if the {\tt fieldbundle} is in the
!   created state, {\tt .false.} otherwise.
!EOP
  !-----------------------------------------------------------------------------    
    NUOPC_FieldBundleIsCreated = .false.  ! default assumption
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_FieldBundleGetInit(fieldbundle)==ESMF_INIT_CREATED) &
      NUOPC_fieldbundleIsCreated = .true.
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsCreated - Check whether a Field object has been created
! !INTERFACE:
  ! call using generic interface: NUOPC_IsCreated
  function NUOPC_FieldIsCreated(field, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldIsCreated
! !ARGUMENTS:
    type(ESMF_Field)               :: field
    integer, intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if the {\tt field} is in the
!   created state, {\tt .false.} otherwise.
!EOP
  !-----------------------------------------------------------------------------    
    NUOPC_FieldIsCreated = .false.  ! default assumption
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_FieldGetInit(field)==ESMF_INIT_CREATED) &
      NUOPC_fieldIsCreated = .true.
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_IsCreated - Check whether a Grid object has been created
! !INTERFACE:
  ! call using generic interface: NUOPC_IsCreated
  function NUOPC_GridIsCreated(grid, rc)
! !RETURN VALUE:
    logical :: NUOPC_GridIsCreated
! !ARGUMENTS:
    type(ESMF_Grid)                :: grid
    integer, intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if the {\tt grid} is in the
!   created state, {\tt .false.} otherwise.
!EOP
  !-----------------------------------------------------------------------------    
    NUOPC_GridIsCreated = .false.  ! default assumption
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_GridGetInit(grid)==ESMF_INIT_CREATED) &
      NUOPC_gridIsCreated = .true.
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Nop - No-Operation attachable method for GridComp
! !INTERFACE:
  subroutine NUOPC_Nop(gcomp, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
! !DESCRIPTION:
!   Dummy method implementing a No-Operation with an interface that matches the
!   requirements for a attachable method for ESMF\_GridComp objects.
!
!   The arguments are:
!   \begin{description}
!   \item[gcomp]
!     The {\tt ESMF\_GridComp} object to which this method is attached.
!   \item[rc]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    rc = ESMF_SUCCESS
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateAdvertiseField - Advertise a Field in a State
! !INTERFACE:
  subroutine NUOPC_StateAdvertiseField(state, StandardName, Units, &
    LongName, ShortName, name, TransferOfferGeomObject, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(*),     intent(in)            :: StandardName
    character(*),     intent(in),  optional :: Units
    character(*),     intent(in),  optional :: LongName
    character(*),     intent(in),  optional :: ShortName
    character(*),     intent(in),  optional :: name
    character(*),     intent(in),  optional :: TransferOfferGeomObject
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Advertises a Field in a State. This call checks the provided
!   information against the NUOPC Field Dictionary. Omitted optional
!   information is filled in using defaults out of the NUOPC Field Dictionary.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object through which the Field is advertised.
!   \item[StandardName]
!     The StandardName of the advertised Field. Must be a StandardName found in
!     the  NUOPC Field Dictionary.
!   \item[{[Units]}]
!     The Units of the advertised Field. Must be convertible to the canonical
!     units specified in the NUOPC Field Dictionary for the specified
!     StandardName.
!     If omitted, the default is to use the canonical units associated with
!     the StandardName in the NUOPC Field Dictionary.
!   \item[{[LongName]}]
!     The LongName of the advertised Field. NUOPC does not restrict the value
!     of this variable.
!     If omitted, the default is to use the LongName associated with 
!     the StandardName in the NUOPC Field Dictionary.
!   \item[{[ShortName]}]
!     The ShortName of the advertised Field. NUOPC does not restrict the value
!     of this variable.
!     If omitted, the default is to use the ShortName associated with 
!     the StandardName in the NUOPC Field Dictionary.
!   \item[{[name]}]
!     The actual name of the advertised Field by which it is accessed in the
!     State object. NUOPC does not restrict the value of this variable.
!     If omitted, the default is to use the value of the ShortName.
!   \item[{[TransferOfferGeomObject]}]
!     The transfer offer for the geom object (Grid, Mesh, LocStream, 
!     XGrid) associated with the advertised Field. NUOPC controls the vocabulary
!     of this attribute: "will provide", "can provide", "cannot provide".
!     If omitted, the default is "will provide".
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)        :: field
    character(ESMF_MAXSTR)  :: tempString
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    field = ESMF_FieldEmptyCreate(name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call NUOPC_FieldAttributeAdd(field, StandardName=StandardName, &
      Units=Units, LongName=LongName, ShortName=ShortName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (.not.present(name)) then
      ! name was not provided -> default to using ShortName
      call NUOPC_FieldAttributeGet(field, name="ShortName", value=tempString, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      !TODO: simplify the following once ESMF supports changing name of Fields
      call ESMF_FieldDestroy(field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      field = ESMF_FieldEmptyCreate(name=trim(tempString), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call NUOPC_FieldAttributeAdd(field, StandardName=StandardName, &
        Units=Units, LongName=LongName, ShortName=ShortName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    if (present(TransferOfferGeomObject)) then
      if (trim(TransferOfferGeomObject)=="will provide") then
        call NUOPC_FieldAttributeSet(field, name="TransferOfferGeomObject", &
          value="will provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      elseif (trim(TransferOfferGeomObject)=="can provide") then
        call NUOPC_FieldAttributeSet(field, name="TransferOfferGeomObject", &
          value="can provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      elseif (trim(TransferOfferGeomObject)=="cannot provide") then
        call NUOPC_FieldAttributeSet(field, name="TransferOfferGeomObject", &
          value="cannot provide", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="must provoide a valid string for TransferOfferGeomObject", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      endif
    endif
    call ESMF_StateAdd(state, fieldList=(/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateAdvertiseFields - Advertise Fields in a State
! !INTERFACE:
  subroutine NUOPC_StateAdvertiseFields(state, StandardNames, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(*),     intent(in)            :: StandardNames(:)
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Advertises Fields in a State. Defaults are set according to the 
!   NUOPC Field Dictionary.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object through which the Field is advertised.
!   \item[StandardNames]
!     A list of StandardNames of the advertised Fields. Must be StandardNames 
!     found in the  NUOPC Field Dictionary.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                 :: i
    
    if (present(rc)) rc = ESMF_SUCCESS

    do i=1, size(StandardNames)
      call NUOPC_StateAdvertiseField(state, StandardName=StandardNames(i), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateBuildStdList - Build lists of Field information from a State
! !INTERFACE:
  recursive subroutine NUOPC_StateBuildStdList(state, stdAttrNameList, &
    stdItemNameList, stdConnectedList, stdFieldList, rc)
! !ARGUMENTS:
    type(ESMF_State),       intent(in)            :: state
    character(ESMF_MAXSTR), pointer               :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer, optional     :: stdItemNameList(:)
    character(ESMF_MAXSTR), pointer, optional     :: stdConnectedList(:)
    type(ESMF_Field),       pointer, optional     :: stdFieldList(:)
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Constructs lists containing the StandardName, Field name, and connected 
!   status of the Fields in the {\tt state}. Returns this information in the
!   list arguments. Recursively parses through nested States.
!
!   All pointer arguments present must enter this method unassociated. On 
!   return, the deallocation of an associated pointer becomes the user
!   responsibility.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer           :: item, itemCount, fieldCount, stat, i
    type(ESMF_Field)  :: field
    character(ESMF_MAXSTR), allocatable     :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: stateitemtypeList(:)
    type(ESMF_State)                        :: nestedState
    character(ESMF_MAXSTR), pointer         :: l_stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer         :: l_stdItemNameList(:)
    character(ESMF_MAXSTR), pointer         :: l_stdConnectedList(:)
    type(ESMF_Field),       pointer         :: l_stdFieldList(:)
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
          
    if (itemCount > 0) then
      allocate(itemNameList(itemCount))
      allocate(stateitemtypeList(itemCount))
      call ESMF_StateGet(state, itemNameList=itemNameList, &
        itemtypeList=stateitemtypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
        
      fieldCount = 0  ! reset
      do item=1, itemCount
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
          fieldCount = fieldCount + 1
        else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
          ! recursively parse the nested state
          nullify(l_stdAttrNameList)
          call ESMF_StateGet(state, itemName=itemNameList(item), &
            nestedState=nestedState, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          call NUOPC_StateBuildStdList(nestedState, l_stdAttrNameList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          if (associated(l_stdAttrNameList)) then
            fieldCount = fieldCount + size(l_stdAttrNameList)
            deallocate(l_stdAttrNameList)
          endif
        endif
      enddo
      
      if (associated(stdAttrNameList)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="stdAttrNameList must enter unassociated", &
          line=__LINE__, &
          file=FILENAME, &
          rcToReturn=rc)
        return  ! bail out
      else
        allocate(stdAttrNameList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg="allocating stdAttrNameList", &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif

      if (present(stdItemNameList)) then
        if (associated(stdItemNameList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="stdItemNameList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(stdItemNameList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating stdItemNameList", &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      endif

      if (present(stdConnectedList)) then
        if (associated(stdConnectedList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="stdConnectedList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(stdConnectedList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating stdConnectedList", &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      endif

      if (present(stdFieldList)) then
        if (associated(stdFieldList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="stdFieldList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(stdFieldList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating stdFieldList", &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      endif

      fieldCount = 1  ! reset

      do item=1, itemCount
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, itemName=itemNameList(item), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          call NUOPC_FieldAttributeGet(field, name="StandardName", &
            value=stdAttrNameList(fieldCount), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          if (present(stdItemNameList)) then
            stdItemNameList(fieldCount)=itemNameList(item)
          endif
          if (present(stdConnectedList)) then
            call NUOPC_FieldAttributeGet(field, name="Connected", &
              value=stdConnectedList(fieldCount), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          endif
          if (present(stdFieldList)) then
            stdFieldList(fieldCount)=field
          endif
          fieldCount = fieldCount + 1
        else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
          ! recursively parse the nested state
          nullify(l_stdAttrNameList)
          nullify(l_stdItemNameList)
          nullify(l_stdConnectedList)
          nullify(l_stdFieldList)
          call ESMF_StateGet(state, itemName=itemNameList(item), &
            nestedState=nestedState, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          call NUOPC_StateBuildStdList(nestedState, l_stdAttrNameList, &
            l_stdItemNameList, l_stdConnectedList, l_stdFieldList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          if (associated(l_stdAttrNameList)) then
            do i=1, size(l_stdAttrNameList)
              stdAttrNameList(fieldCount) = l_stdAttrNameList(i)
              if (present(stdItemNameList)) then
                stdItemNameList(fieldCount) = l_stdItemNameList(i)
              endif
              if (present(stdConnectedList)) then
                stdConnectedList(fieldCount) = l_stdConnectedList(i)
              endif
              if (present(stdFieldList)) then
                stdFieldList(fieldCount) = l_stdFieldList(i)
              endif
              fieldCount = fieldCount + 1
            enddo
            deallocate(l_stdAttrNameList)
            deallocate(l_stdItemNameList)
            deallocate(l_stdConnectedList)
            deallocate(l_stdFieldList)
          endif
        endif
      enddo
        
      deallocate(itemNameList)
      deallocate(stateitemtypeList)
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateIsAllConnected - Check if all the Fields in a State are connected
! !INTERFACE:
  function NUOPC_StateIsAllConnected(state, rc)
! !RETURN VALUE:
    logical :: NUOPC_StateIsAllConnected
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if all the Fields in {\tt state} are connected.
!   Otherwise returns {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer           :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer           :: stdConnectedList(:)
    logical                                   :: allConnected
    integer                                   :: i

    if (present(rc)) rc = ESMF_SUCCESS
    
    nullify(stdAttrNameList)
    nullify(stdConnectedList)

    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdConnectedList=stdConnectedList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    allConnected = .true.  ! initialize
    if (associated(stdConnectedList)) then
      do i=1, size(stdConnectedList)
        if (stdConnectedList(i) /= "true") then
          allConnected = .false.
          exit
        endif
      enddo
    endif

    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdConnectedList)) deallocate(stdConnectedList)

    NUOPC_StateIsAllConnected = allConnected

  end function
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateIsAtTime - Check if all the Fields in a State are at the given Time
! !INTERFACE:
  function NUOPC_StateIsAtTime(state, time, rc)
! !RETURN VALUE:
    logical :: NUOPC_StateIsAtTime
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    type(ESMF_Time),  intent(in)            :: time
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if all the Fields in {\tt state} have a timestamp 
!   that matches {\tt time}. Otherwise returns {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    type(ESMF_Field),       pointer       :: stdFieldList(:)
    type(ESMF_Field)                      :: field
    integer                 :: i
    character(ESMF_MAXSTR)  :: iString, msgString
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    NUOPC_StateIsAtTime = .true.  ! initialize
    
    nullify(stdAttrNameList)
    nullify(stdItemNameList)
    nullify(stdFieldList)

    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdItemNameList=stdItemNameList, stdFieldList=stdFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    if (associated(stdItemNameList)) then
      do i=1, size(stdItemNameList)
        write (iString, *) i
        write (msgString, *) "Failure in NUOPC_StateIsAtTime() for item "// &
          trim(adjustl(iString))//": "//trim(stdItemNameList(i))
        field=stdFieldList(i)
        NUOPC_StateIsAtTime = NUOPC_FieldIsAtTime(field, time, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=msgString, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        if (.not.NUOPC_StateIsAtTime) exit
      enddo
    endif
    
    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdItemNameList)) deallocate(stdItemNameList)
    if (associated(stdFieldList)) deallocate(stdFieldList)
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateIsFieldConnected - Test if Field in a State is connected
! !INTERFACE:
  function NUOPC_StateIsFieldConnected(state, fieldName, rc)
! !RETURN VALUE:
    logical :: NUOPC_StateIsFieldConnected
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    character(*),     intent(in)            :: fieldName
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if Fields with name {\tt fieldName} contained in 
!   {\tt state} is connected. Otherwise returns {\tt .false.}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)        :: field
    character(ESMF_MAXSTR)  :: connectedValue

    if (present(rc)) rc = ESMF_SUCCESS
    
    NUOPC_StateIsFieldConnected = .false. ! initialize

    call ESMF_StateGet(state, itemName=fieldName, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call NUOPC_FieldAttributeGet(field, name="Connected", &
      value=connectedValue, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    if (connectedValue=="true") then
      NUOPC_StateIsFieldConnected = .true.
    endif

  end function
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateIsUpdated - Check if all the Fields in a State are marked as updated
! !INTERFACE:
  function NUOPC_StateIsUpdated(state, count, rc)
! !RETURN VALUE:
    logical :: NUOPC_StateIsUpdated
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    integer,          intent(out), optional :: count
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if all the Fields in {\tt state} have their "Updated"
!   Attribute set to "true". Otherwise returns {\tt .false.}. The {\tt count}
!   argument returns how many of the FIelds have the Updated" Attribtue set to
!   "true".
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    type(ESMF_Field),       pointer       :: stdFieldList(:)
    type(ESMF_Field)                      :: field
    character(ESMF_MAXSTR)                :: value
    integer                 :: i
    character(ESMF_MAXSTR)  :: iString, msgString
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    nullify(stdAttrNameList)
    nullify(stdItemNameList)
    nullify(stdFieldList)
    
    if (present(count)) count = 0 ! reset
    
    NUOPC_StateIsUpdated = .true. ! initialize 

    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdItemNameList=stdItemNameList, stdFieldList=stdFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    if (associated(stdItemNameList)) then
      do i=1, size(stdItemNameList)
        write (iString, *) i
        write (msgString, *) "Failure in NUOPC_StateIsUpdated() for item "// &
          trim(adjustl(iString))//": "//trim(stdItemNameList(i))
        field=stdFieldList(i)
        call ESMF_AttributeGet(field, name="Updated", value=value, &
          convention="NUOPC", purpose="General", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        if (present(count) .and. trim(value)=="true") then
          count = count + 1
        else if (trim(value)=="false") then
          NUOPC_StateIsUpdated = .false. ! toggle
          if (.not.present(count)) exit ! no need to continue looking
        endif
      enddo
    endif
    
    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdItemNameList)) deallocate(stdItemNameList)
    if (associated(stdFieldList)) deallocate(stdFieldList)
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateRealizeField - Realize a previously advertised Field in a State
! !INTERFACE:
  subroutine NUOPC_StateRealizeField(state, field, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    type(ESMF_Field), intent(in)            :: field
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Realizes a previously advertised Field in {\tt state}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)        :: advertisedField
    character(ESMF_MAXSTR)  :: name
    character(ESMF_MAXSTR)  :: StandardName
    character(ESMF_MAXSTR)  :: Units
    character(ESMF_MAXSTR)  :: LongName
    character(ESMF_MAXSTR)  :: ShortName
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_FieldGet(field, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call ESMF_StateGet(state, itemName=name, field=advertisedField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_FieldAttributeGet(advertisedField, name="StandardName", &
      value=StandardName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_FieldAttributeGet(advertisedField, name="Units", &
      value=Units, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_FieldAttributeGet(advertisedField, name="LongName", &
      value=LongName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_FieldAttributeGet(advertisedField, name="ShortName", &
      value=ShortName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_FieldAttributeAdd(field, StandardName=StandardName,&
      Units=Units, LongName=LongName, ShortName=ShortName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call ESMF_StateReplace(state, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateSetTimestamp - Set a time stamp on all Fields in a State
! !INTERFACE:
  subroutine NUOPC_StateSetTimestamp(state, clock, selective, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    type(ESMF_Clock), intent(in)            :: clock
    logical,          intent(in),  optional :: selective
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Sets the TimeStamp Attribute according to {\tt clock} on all the Fields in 
!   {\tt state}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    character(ESMF_MAXSTR)                :: value
    type(ESMF_Field)                      :: field
    type(ESMF_Time)         :: time
    integer                 :: yy, mm, dd, h, m, s, ms, us, ns
    integer                 :: i
    logical                 :: selected
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, us=us, &
      ns=ns, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  
    nullify(stdAttrNameList)
    nullify(stdItemNameList)
  
    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdItemNameList=stdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    if (associated(stdItemNameList)) then
      do i=1, size(stdItemNameList)
        call ESMF_StateGet(state, field=field, itemName=stdItemNameList(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        if (present(selective)) then
          if (selective) then
            call ESMF_AttributeGet(field, &
              name="Updated", value=value, &
              convention="NUOPC", purpose="General", &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
            if (trim(value)=="true") then
              selected=.true.
            else
              selected = .false.
            endif
          else
            selected=.true.
          endif
        else
          selected=.true.
        endif
        if (selected) then
          call ESMF_AttributeSet(field, &
            name="TimeStamp", valueList=(/yy,mm,dd,h,m,s,ms,us,ns/), &
            convention="NUOPC", purpose="General", &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        endif
      enddo
    endif
    
    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdItemNameList)) deallocate(stdItemNameList)
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateUpdateTimestamp - Update the timestamp on all the Fields in a State
! !INTERFACE:
  subroutine NUOPC_StateUpdateTimestamp(state, rootPet, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    integer,          intent(in)            :: rootPet
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Updates the TimeStamp Attribute for all the Fields on all the PETs in the
!   current VM to the TimeStamp Attribute held by the Field instance on the 
!   {\tt rootPet}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    type(ESMF_Field)                      :: field
    integer                 :: i, localPet, valueList(9)
    type(ESMF_VM)           :: vm
    
!gjtdebug    character(ESMF_MAXSTR)  :: tempString1, msgString

    if (present(rc)) rc = ESMF_SUCCESS
    
    nullify(stdAttrNameList)
    nullify(stdItemNameList)

    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdItemNameList=stdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    if (associated(stdItemNameList)) then
      do i=1, size(stdItemNameList)

        call ESMF_StateGet(state, field=field, itemName=stdItemNameList(i), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
          
        call ESMF_AttributeGet(field, &
          name="TimeStamp", valueList=valueList, &
          convention="NUOPC", purpose="General", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out

!print *, "NUOPC_StateUpdateTimestamp BEFORE: ", valueList

        call ESMF_VMBroadcast(vm, bcstData=valueList, count=size(valueList), &
          rootPet=rootPet, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        
!print *, "NUOPC_StateUpdateTimestamp AFTER:  ", valueList
        
        if (localPet /= rootPet) then
        
          call ESMF_AttributeSet(field, &
            name="TimeStamp", valueList=valueList, &
            convention="NUOPC", purpose="General", &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

!gjtdebug call ESMF_FieldGet(field, name=tempString1)        
!gjtdebug write (msgString, *) "updating to broadcasted TimeStamp:", trim(tempString1), field%ftypep%base
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
!gjtdebug write (msgString, *) valueList
!gjtdebug call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

        endif
        
      enddo
    endif
    
    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdItemNameList)) deallocate(stdItemNameList)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateWrite - Write the Fields within a State to NetCDF files
! !INTERFACE:
  subroutine NUOPC_StateWrite(state, fieldNameList, filePrefix, overwrite, &
    status, timeslice, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_State),           intent(in)            :: state
    character(len=*),           intent(in),  optional :: fieldNameList(:)
    character(len=*),           intent(in),  optional :: filePrefix
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    logical,                    intent(in),  optional :: relaxedflag
    integer,                    intent(out), optional :: rc
! !DESCRIPTION:
!   Write the data of the Fields within a State to NetCDF files. Each Field is
!   written to an individual file using the StandardName Attribute as NetCDF
!   attribute.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: i, itemCount
    type(ESMF_Field)                :: field
    type(ESMF_StateItem_Flag)       :: itemType
    character(len=80)               :: fileName
    character(len=80), allocatable  :: fieldNameList_loc(:)

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(fieldNameList)) then
      allocate(fieldNameList_loc(size(fieldNameList)))
      do i=1, size(fieldNameList)
        fieldNameList_loc(i) = trim(fieldNameList(i))
      enddo
    else
      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(fieldNameList_loc(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList_loc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    do i=1, size(fieldNameList_loc)
      call ESMF_StateGet(state, itemName=fieldNameList_loc(i), &
        itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (itemType == ESMF_STATEITEM_FIELD) then
        ! field is available in the state
        call ESMF_StateGet(state, itemName=fieldNameList_loc(i), field=field, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        ! -> output to file
        if (present(filePrefix)) then
          write (fileName,"(A)") filePrefix//trim(fieldNameList_loc(i))//".nc"
        else
          write (fileName,"(A)") trim(fieldNameList_loc(i))//".nc"
        endif
        call NUOPC_FieldWrite(field, file=trim(fileName), overwrite=overwrite, &
          status=status, timeslice=timeslice, relaxedflag=relaxedflag, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
    enddo
    
    deallocate(fieldNameList_loc)

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_TimePrint - Formatted print ot time information
! !INTERFACE:
  subroutine NUOPC_TimePrint(time, string, unit, rc)
! !ARGUMENTS:
    type(ESMF_Time), intent(in)            :: time
    character(*),    intent(in),  optional :: string
    character(*),    intent(out), optional :: unit
    integer,         intent(out), optional :: rc
! !DESCRIPTION:
!   Write a formatted time with or without {\tt string}
!   to {\tt unit}. If {\tt unit} is present it must be an internal unit, i.e. a 
!   string variable. If {\tt unit} is not present then the output is written to
!   the default external unit (typically that would be stdout).
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                 :: yy, mm, dd, h, m, s, ms
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
  
    if (present(unit)) then
      if (present(string)) then
        write (unit, "(A, I4, I3, I3, I3, I3, I3, I4)") string, &
          yy, mm, dd, h, m, s, ms
      else
        write (unit, "(I4, I3, I3, I3, I3, I3, I4)") &
          yy, mm, dd, h, m, s, ms
      endif
    else
      if (present(string)) then
        write (*, "(A, I4, I3, I3, I3, I3, I3, I4)") string, &
          yy, mm, dd, h, m, s, ms
      else
        write (*, "(I4, I3, I3, I3, I3, I3, I4)") &
          yy, mm, dd, h, m, s, ms
      endif
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
