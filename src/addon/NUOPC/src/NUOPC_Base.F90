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
#define FILENAME "src/addon/NUOPC/src/NUOPC_Base.F90"
!==============================================================================

!TODO: make this macros available through ESMF as parameter or find other way
#define ESMF_INIT_CREATED 82949521

#define PROFILE_off

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
  logical, save :: NUOPC_FieldDictionaryAutoAdd = .false.  
  
  ! public module variables
  type(ESMF_Container), save  :: NUOPC_FieldDictionary
  public NUOPC_FieldDictionary
  integer, parameter          :: NUOPC_PhaseMapStringLength = 160
  public NUOPC_PhaseMapStringLength

  ! public module interfaces
  public NUOPC_ClockCheckSetClock
  public NUOPC_ClockInitialize
  public NUOPC_ClockPrintCurrTime
  public NUOPC_ClockPrintStartTime
  public NUOPC_ClockPrintStopTime
  public NUOPC_FieldAttributeAdd
  public NUOPC_FieldAttributeGet
  public NUOPC_FieldAttributeSet
  public NUOPC_FieldBundleUpdateTime
  public NUOPC_FieldDictionaryAddEntry
  public NUOPC_FieldDictionaryGetEntry
  public NUOPC_FieldDictionaryHasEntry
  public NUOPC_FieldDictionaryMatchSyno  
  public NUOPC_FieldDictionarySetSyno  
  public NUOPC_FieldDictionarySetup
  public NUOPC_FieldDictionarySetAutoAdd
  public NUOPC_FieldIsAtTime
  public NUOPC_FieldWrite             ! mark deprecated, use NUOPC_Write
  public NUOPC_GridCreateSimpleSph
  public NUOPC_GridCreateSimpleXY
  public NUOPC_IsCreated
  public NUOPC_Nop
  public NUOPC_StateAdvertiseField    ! mark deprecated, use NUOPC_Advertise
  public NUOPC_StateAdvertiseFields   ! mark deprecated, use NUOPC_Advertise
  public NUOPC_StateAttributeAdd
  public NUOPC_StateAttributeGet
  public NUOPC_StateAttributeSet
  public NUOPC_StateBuildStdList
  public NUOPC_StateIsAllConnected
  public NUOPC_StateIsAtTime
  public NUOPC_StateIsFieldConnected
  public NUOPC_StateIsUpdated
  public NUOPC_StateNamespaceAdd
  public NUOPC_StateRealizeField      ! mark deprecated, use NUOPC_Realize
  public NUOPC_StateReconcile
  public NUOPC_StateSetTimestamp
  public NUOPC_StateUpdateTimestamp
  public NUOPC_StateWrite             ! mark deprecated, use NUOPC_Write
  public NUOPC_TimePrint
  ! -- utility methods following the new v7 scheme
  public NUOPC_Advertise
  public NUOPC_FillData
  public NUOPC_Realize
  public NUOPC_UpdateTimestamp
  public NUOPC_Write

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  interface NUOPC_Advertise
    module procedure NUOPC_StateAdvertiseField
    module procedure NUOPC_StateAdvertiseFields
  end interface
  
  interface NUOPC_IsCreated
    module procedure NUOPC_ClockIsCreated
    module procedure NUOPC_FieldBundleIsCreated
    module procedure NUOPC_FieldIsCreated
    module procedure NUOPC_GridIsCreated
  end interface
  
  interface NUOPC_Realize
    module procedure NUOPC_RealizeComplete
    module procedure NUOPC_StateRealizeField
  end interface
  
  interface NUOPC_UpdateTimestamp
    module procedure NUOPC_UpdateFieldList
    module procedure NUOPC_UpdateAcrossFieldLists
    module procedure NUOPC_FieldBundleUpdateTime
    module procedure NUOPC_StateUpdateTimestamp
  end interface
  
  interface NUOPC_Write
    module procedure NUOPC_WriteWeights
    module procedure NUOPC_FieldWrite
    module procedure NUOPC_StateWrite
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
!   information is filled in using defaults.
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
!     If omitted, the default is to use the StandardName.
!   \item[{[ShortName]}]
!     The ShortName of the Field. NUOPC does not restrict the value
!     of this variable.
!     If omitted, the default is to use the StandardName.
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
      if (NUOPC_FieldDictionaryAutoAdd) then
        call NUOPC_FieldDictionaryAddEntry(standardName=trim(StandardName), &
          canonicalUnits="unknown - Autogenerated Entry", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=StandardName//" is not a StandardName in the NUOPC_FieldDictionary!",&
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
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
      tempString = trim(LongName)
    else
      tempString = trim(StandardName)   ! default
    endif
    call ESMF_AttributeSet(field, &
      name="LongName", value=trim(tempString), &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! set ShortName
    if (present(ShortName)) then
      tempString = trim(ShortName)
    else
      tempString = trim(StandardName)   ! default
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
!   be the same as in the {\tt srcFields} FieldBundle.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field), pointer     :: srcFieldList(:)
    type(ESMF_Field), pointer     :: dstFieldList(:)
    integer                       :: srcCount, dstCount
    
!gjtdebug character(ESMF_MAXSTR)  :: tempString1, tempString2
!gjtdebug character(5*ESMF_MAXSTR):: msgString
    
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
    
    call NUOPC_UpdateTimestamp(srcFieldList, dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    deallocate(srcFieldList, dstFieldList)

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionaryAddEntry - Add an entry to the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryAddEntry(standardName, canonicalUnits, rc)
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName
    character(*),                 intent(in)            :: canonicalUnits
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Add an entry to the NUOPC Field dictionary. If necessary the dictionary is
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
  subroutine NUOPC_FieldDictionaryGetEntry(standardName, canonicalUnits, rc)
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName
    character(*),                 intent(out), optional :: canonicalUnits
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Return the canonical units that the NUOPC Field dictionary associates with
!   the {\tt standardName}.
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
!   Return {\tt .true.} if the NUOPC Field dictionary has an entry with the
!   specified {\tt standardName}, {\tt .false.} otherwise.
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
! !IROUTINE: NUOPC_FieldDictionaryMatchSyno - Check whether the NUOPC Field dictionary considers the standard names synonyms
! !INTERFACE:
  function NUOPC_FieldDictionaryMatchSyno(standardName1, standardName2, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldDictionaryMatchSyno
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardName1
    character(*),                 intent(in)            :: standardName2
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the NUOPC Field dictionary considers
!   {\tt standardName1} and {\tt standardName2} synonyms, {\tt .false.} 
!   otherwise. An entry with standard name of {\tt standardName1} must
!   exist in the field dictionary, or else an error will be returned. 
!   However, {\tt standardName2} need not correspond to an existing entry.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    NUOPC_FieldDictionaryMatchSyno = &
      NUOPC_FieldDictionaryMatchSynoI(NUOPC_FieldDictionary, &
      standardName1 = standardName1, standardName2 = standardName2, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FieldDictionarySetSyno - Set synonyms in the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionarySetSyno(standardNames, rc)
! !ARGUMENTS:
    character(*),                 intent(in)            :: standardNames(:)
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Set all of the elements of the {\tt standardNames} argument to be considered
!   synonyms by the field dictionary. Every element in {\tt standardNames} must
!   correspond to the standard name of already existing entries in the field 
!   dictionary, or else an error will be returned.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_FieldDictionarySetup(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call NUOPC_FieldDictionarySetSynoI(NUOPC_FieldDictionary, &
      standardNames = standardNames, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
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
!BOPI
! !IROUTINE: NUOPC_FieldDictionarySetAutoAdd - Turn on/off AutoAdd
! !INTERFACE:
  subroutine NUOPC_FieldDictionarySetAutoAdd(setting, rc)
! !ARGUMENTS:
    logical,      intent(in)              :: setting
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Turn on/off AutoAdd in the NUOPC Field dictionary.
!EOPI
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    NUOPC_FieldDictionaryAutoAdd = setting

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
! !IROUTINE: NUOPC_GridCreateSimpleSph - Create a simple Spherical Grid
! !INTERFACE:
  function NUOPC_GridCreateSimpleSph(x_min, y_min, x_max, y_max, &
    i_count, j_count, half_polar_cell, area_adj, tag, scheme, rc)
! !RETURN VALUE:
    type(ESMF_Grid):: NUOPC_GridCreateSimpleSph
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(in)            :: x_min, x_max, y_min, y_max
    integer,            intent(in)            :: i_count, j_count
    logical,            intent(in),  optional :: half_polar_cell
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
    real(ESMF_KIND_R8)                        :: dx, dy, sx, sy, halfdy
    integer                                   :: i, j
    real(ESMF_KIND_R8), pointer               :: coordX(:,:), coordY(:,:)
    real(ESMF_KIND_R8), pointer               :: f_area(:,:), f_area_m(:)
    real(ESMF_KIND_R8), pointer               :: o_area(:,:)
    real(ESMF_KIND_R8)                        :: startx, starty
    integer                                   :: l_scheme
    type(ESMF_Mesh)                           :: mesh
    type(ESMF_Field)                          :: field
    logical                                   :: l_half_polar_cell
    
    if (present(rc)) rc = ESMF_SUCCESS
    l_half_polar_cell = .false.
    if(present(half_polar_cell)) l_half_polar_cell = half_polar_cell

    ! convert to input variables to the internal variables
    sx = x_min
    sy = y_min
    nx = i_count
    ny = j_count
    dx = (x_max - x_min) / nx
    if(l_half_polar_cell) then
      dy = (y_max - y_min) / (ny - 1)
      halfdy = dy/2.
    else
      dy = (y_max - y_min) / ny
    endif
    
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
    if(l_half_polar_cell) then
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordX, 2), ubound(coordX, 2)
          coordX(i,j) = startx + dx/2. + (i-1)*dx
          coordY(i,j) = starty + halfdy/2. + (j-1)*dy
        enddo
      enddo
    else
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordX, 2), ubound(coordX, 2)
          coordX(i,j) = startx + dx/2. + (i-1)*dx
          coordY(i,j) = starty + dy/2. + (j-1)*dy
        enddo
      enddo
    endif
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
    if(l_half_polar_cell) then
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordX, 2), ubound(coordX, 2)
          coordX(i,j) = startx + (i-1)*dx
          if(j == 1) then
            coordY(i,j) = starty
          else 
            coordY(i,j) = starty + halfdy + (j-2)*dy
          endif
        enddo
      enddo
    else
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordX, 2), ubound(coordX, 2)
          coordX(i,j) = startx + (i-1)*dx
          coordY(i,j) = starty + (j-1)*dy
        enddo
      enddo
    endif

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
!     If omitted, the default is to use the StandardName.
!   \item[{[ShortName]}]
!     The ShortName of the advertised Field. NUOPC does not restrict the value
!     of this variable.
!     If omitted, the default is to use the StandardName.
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
  subroutine NUOPC_StateAdvertiseFields(state, StandardNames, &
    TransferOfferGeomObject, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(*),     intent(in)            :: StandardNames(:)
    character(*),     intent(in),  optional :: TransferOfferGeomObject
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
!   \item[{[TransferOfferGeomObject]}]
!     The transfer offer for the geom object (Grid, Mesh, LocStream, 
!     XGrid) associated with the advertised Fields. This setting applies to all
!     the Fields advertised in this call. NUOPC controls the vocabulary
!     of this attribute: "will provide", "can provide", "cannot provide".
!     If omitted, the default is "will provide".
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
        TransferOfferGeomObject=TransferOfferGeomObject, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateAttributeAdd - Add the NUOPC State Attributes
! !INTERFACE:
  subroutine NUOPC_StateAttributeAdd(state, rc)
! !ARGUMENTS:
    type(ESMF_state)                      :: state
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Adds standard NUOPC Attributes to a State.
!
!   This adds the standard NUOPC State Attribute package: convention="NUOPC",
!   purpose="General" to the State.
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)            :: attrList(1)
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be added to the Fields
    attrList(1) = "Namespace"           ! namespace of this State
    
    ! add Attribute packages
    call ESMF_AttributeAdd(state, convention="NUOPC", purpose="General", &
      attrList=attrList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set Attributes to defaults
    ! <no defaults currently>
    
  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateAttributeGet - Get a NUOPC State Attribute
! !INTERFACE:
  subroutine NUOPC_StateAttributeGet(state, name, value, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    character(*),     intent(in)            :: name
    character(*),     intent(out)           :: value
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Accesses the Attribute {\tt name} inside of {\tt state} using the
!   convention {\tt NUOPC} and purpose {\tt General}. Returns with error if
!   the Attribute is not present or not set.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: defaultvalue
    
    if (present(rc)) rc = ESMF_SUCCESS

    defaultvalue = "CheckThisDefaultValue"

    call ESMF_AttributeGet(state, name=name, value=value, &
      defaultvalue=defaultvalue, &
      convention="NUOPC", purpose="General", &
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
! !IROUTINE: NUOPC_StateAttributeSet - Set a NUOPC State Attribute
! !INTERFACE:
  subroutine NUOPC_StateAttributeSet(state, name, value, rc)
! !ARGUMENTS:
    type(ESMF_State)                      :: state
    character(*), intent(in)              :: name
    character(*), intent(in)              :: value
    integer,      intent(out), optional   :: rc
! !DESCRIPTION:
!   Set the Attribute {\tt name} inside of {\tt state} using the
!   convention {\tt NUOPC} and purpose {\tt General}.
!EOP
  !-----------------------------------------------------------------------------
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeSet(state, name=name, value=value, &
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
! !IROUTINE: NUOPC_StateBuildStdList - Build lists of Field information from a State
! !INTERFACE:
  recursive subroutine NUOPC_StateBuildStdList(state, stdAttrNameList, &
    stdItemNameList, stdConnectedList, namespaceList, stdFieldList, rc)
! !ARGUMENTS:
    type(ESMF_State),       intent(in)            :: state
    character(ESMF_MAXSTR), pointer, optional     :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer, optional     :: stdItemNameList(:)
    character(ESMF_MAXSTR), pointer, optional     :: stdConnectedList(:)
    character(ESMF_MAXSTR), pointer, optional     :: namespaceList(:)
    type(ESMF_Field),       pointer, optional     :: stdFieldList(:)
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Constructs lists containing the StandardName, Field name, and connected 
!   status of the Fields in {\tt state}. Returns this information in the
!   list arguments. Recursively parses through nested States.
!
!   All pointer arguments present must enter this method unassociated. On 
!   return, the deallocation of an associated pointer becomes the responsibility
!   of the caller.
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
    character(ESMF_MAXSTR), pointer         :: l_namespaceList(:)
    type(ESMF_Field),       pointer         :: l_stdFieldList(:)
    character(ESMF_MAXSTR)                  :: namespace
    
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
      
      if (present(stdAttrNameList)) then
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

      if (present(namespaceList)) then
        if (associated(namespaceList)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="namespaceList must enter unassociated", &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)
          return  ! bail out
        else
          allocate(namespaceList(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(stat, msg="allocating namespaceList", &
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
        call NUOPC_StateAttributeGet(state, name="Namespace", value=namespace, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, itemName=itemNameList(item), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          if (present(stdAttrNameList)) then
            call NUOPC_FieldAttributeGet(field, name="StandardName", &
              value=stdAttrNameList(fieldCount), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=FILENAME)) &
              return  ! bail out
          endif
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
          if (present(namespaceList)) then
            NamespaceList(fieldCount)=trim(namespace)
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
          nullify(l_namespaceList)
          nullify(l_stdFieldList)
          call ESMF_StateGet(state, itemName=itemNameList(item), &
            nestedState=nestedState, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          call NUOPC_StateBuildStdList(nestedState, l_stdAttrNameList, &
            l_stdItemNameList, l_stdConnectedList, l_namespaceList, &
            l_stdFieldList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
          if (associated(l_stdAttrNameList)) then
            do i=1, size(l_stdAttrNameList)
              if (present(stdAttrNameList)) then
                stdAttrNameList(fieldCount) = l_stdAttrNameList(i)
              endif
              if (present(stdItemNameList)) then
                stdItemNameList(fieldCount) = l_stdItemNameList(i)
              endif
              if (present(stdConnectedList)) then
                stdConnectedList(fieldCount) = l_stdConnectedList(i)
              endif
              if (present(namespaceList)) then
                namespaceList(fieldCount) = trim(namespace)//":"// &
                  trim(l_namespaceList(i))
              endif
              if (present(stdFieldList)) then
                stdFieldList(fieldCount) = l_stdFieldList(i)
              endif
              fieldCount = fieldCount + 1
            enddo
            deallocate(l_stdAttrNameList)
            deallocate(l_stdItemNameList)
            deallocate(l_stdConnectedList)
            deallocate(l_namespaceList)
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
        if (.not.NUOPC_StateIsAtTime) then
          write (msgString, *) "Field not at expected time for item "// &
            trim(adjustl(iString))//": "//trim(stdItemNameList(i))
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
          exit
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
! !IROUTINE: NUOPC_StateNamespaceAdd - Add a namespace to a State
! !INTERFACE:
  subroutine NUOPC_StateNamespaceAdd(state, namespace, nestedStateName, &
    nestedState, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(len=*), intent(in)            :: namespace
    character(len=*), intent(in),  optional :: nestedStateName
    type(ESMF_State), intent(out), optional :: nestedState
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Add a namespace to {\tt state}. This creates a nested State inside of 
!   {\tt state}. The nested State is returned as {\tt nestedState}.
!   If provided, {\tt nestedStateName} will be used to name the newly created
!   nested State, otherwise it will default to be identical to {\tt namespace}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_State)        :: nestedS
    character(len=80)       :: nestedSName
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (present(nestedStateName)) then
      nestedSName = trim(nestedStateName)
    else
      nestedSName = trim(namespace)
    endif
    
    nestedS = ESMF_StateCreate(name=nestedSName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call NUOPC_StateAttributeAdd(nestedS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_StateAttributeSet(nestedS, name="Namespace", &
      value=trim(namespace), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    call ESMF_StateAdd(state, (/nestedS/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (present(nestedState)) &
      nestedState = nestedS
    
  end subroutine
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
!   Realizes a previously advertised Field in {\tt state} by replacing the
!   advertised Field with {\tt field}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)        :: advertisedField
    character(ESMF_MAXSTR)  :: name
    character(ESMF_MAXSTR)  :: StandardName
    character(ESMF_MAXSTR)  :: Units
    character(ESMF_MAXSTR)  :: LongName
    character(ESMF_MAXSTR)  :: ShortName
    integer                 :: i
    integer, parameter      :: attrCount=6
    character(ESMF_MAXSTR)  :: attrList(attrCount)
    character(ESMF_MAXSTR)  :: tempString
    
    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be copied
    attrList(1) = "Connected"
    attrList(2) = "ProducerConnection"
    attrList(3) = "ConsumerConnection"
    attrList(4) = "Updated"
    attrList(5) = "TransferOfferGeomObject"
    attrList(6) = "TransferActionGeomObject"
    
    ! Obtain the advertised Field
    
    call ESMF_FieldGet(field, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    call ESMF_StateGet(state, itemName=name, field=advertisedField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! Obtain basic attributes from the advertised Field
      
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
    
    ! Add the Field attributes to the realizing Field and set basic values
    
    call NUOPC_FieldAttributeAdd(field, StandardName=StandardName,&
      Units=Units, LongName=LongName, ShortName=ShortName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! Loop over the list of Attributes and transfer between Fields
    
    do i=1, attrCount
      
      call NUOPC_FieldAttributeGet(advertisedField, name=trim(attrList(i)), &
        value=tempString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out

      call NUOPC_FieldAttributeSet(field, name=trim(attrList(i)), &
        value=trim(tempString), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out

    enddo
    
    ! Finally replace the advertised Field with the realizing Field
      
    call ESMF_StateReplace(state, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_StateReconcile - Reconcile a State
! !INTERFACE:
  subroutine NUOPC_StateReconcile(state, rc)
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Reconcile the {\tt state} as required by NUOPC Layer.
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateReconcile(state, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
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
    type(ESMF_Field),       pointer       :: stdFieldList(:)
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
    nullify(stdFieldList)
  
    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdItemNameList=stdItemNameList, stdFieldList=stdFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    if (associated(stdItemNameList)) then
      do i=1, size(stdItemNameList)
        field=stdFieldList(i)
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
    if (associated(stdFieldList)) deallocate(stdFieldList)
    
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
    type(ESMF_Field),       pointer       :: stdFieldList(:)
    type(ESMF_Field)                      :: field
    integer                 :: i, localPet, valueList(9)
    type(ESMF_VM)           :: vm
    
    real(ESMF_KIND_R8)        :: timeBase, time0, time
    character(ESMF_MAXSTR)    :: msgString

!gjtdebug character(ESMF_MAXSTR)  :: tempString1, msgString

    if (present(rc)) rc = ESMF_SUCCESS
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(timeBase)
    time0=timeBase
#endif

    nullify(stdFieldList)

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 01 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    call NUOPC_StateBuildStdList(state, stdFieldList=stdFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 02 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

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

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 03 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    if (associated(stdFieldList)) then
      do i=1, size(stdFieldList)
        field=stdFieldList(i)
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
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 04 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    if (associated(stdFieldList)) deallocate(stdFieldList)
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "StateUpdateTimestamp Profile 05 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_UpdateTimestamp - Update the timestamp on all the Fields
! !INTERFACE:
  ! call using generic interface: NUOPC_UpdateTimestamp
  subroutine NUOPC_UpdateFieldList(fieldList, rootPet, rc)
! !ARGUMENTS:
    type(ESMF_Field), pointer               :: fieldList(:)
    integer,          intent(in)            :: rootPet
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Updates the TimeStamp Attribute for all the Fields on all the PETs in the
!   current VM to the TimeStamp Attribute held by the Field instance on the 
!   {\tt rootPet}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)                      :: field
    integer                 :: i, localPet, valueList(9)
    type(ESMF_VM)           :: vm
    
    real(ESMF_KIND_R8)        :: timeBase, time0, time
    character(ESMF_MAXSTR)    :: msgString
    type(ESMF_AttPack)        :: attPack
    
!gjtdebug character(ESMF_MAXSTR)  :: tempString1, msgString

    if (present(rc)) rc = ESMF_SUCCESS
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(timeBase)
    time0=timeBase
#endif

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 01 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 02 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

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

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 03 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

    if (associated(fieldList)) then
      do i=1, size(fieldList)
        field=fieldList(i)
#ifdef PROFILE_DETAILS_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04a time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
        call ESMF_AttributeGet(field, &
          name="TimeStamp", valueList=valueList, &
          convention="NUOPC", purpose="General", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out

#ifdef PROFILE_DETAILS_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04b time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
    
!print *, "NUOPC_UpdateFieldList BEFORE: ", valueList

        call ESMF_VMBroadcast(vm, bcstData=valueList, count=size(valueList), &
          rootPet=rootPet, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        
#ifdef PROFILE_DETAILS_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04c time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
    
!print *, "NUOPC_UpdateFieldList AFTER:  ", valueList
        
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
        
#ifdef PROFILE_DETAILS_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04d time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
    
      enddo
    endif
    
#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 04 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

#ifdef PROFILE_on
    ! PROFILE
    call ESMF_VMWtime(time)
    write (msgString, *) "UpdateFieldList Profile 05 time=   ", &
      time-time0, time-timeBase
      time0=time
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_UpdateTimestamp - Propagate the time stamp from src to dst Fields
! !INTERFACE:
  ! call using generic interface: NUOPC_UpdateTimestamp
  subroutine NUOPC_UpdateAcrossFieldLists(srcFieldList, dstFieldList, rc)
! !ARGUMENTS:
    type(ESMF_Field), pointer               :: srcFieldList(:)
    type(ESMF_Field), pointer               :: dstFieldList(:)
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Updates the time stamp on all Fields in the {\tt dstFieldList} to
!   equal the time stamps of {\tt dstFieldList}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)              :: srcField, dstField
    integer                       :: i, valueList(9), srcCount, dstCount
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    if (.not.associated(srcFieldList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="must be associated",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    srcCount=size(srcFieldList)
    if (.not.associated(dstFieldList)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="must be associated",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
    dstCount=size(dstFieldList)
    if (srcCount /= dstCount) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="count mismatch",&
        line=__LINE__, &
        file=FILENAME, &
        rcToReturn=rc)
      return  ! bail out
    endif
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
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed writing file: "// &
          trim(fileName), &
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

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FillData - Fill data into a Field
! !INTERFACE:
  subroutine NUOPC_FillData(field, dataFillScheme, member, step, rc)
! !ARGUMENTS:
    type(ESMF_Field), intent(inout) :: field
    character(len=*), intent(in)    :: dataFillScheme
    integer, intent(in)             :: member
    integer, intent(in)             :: step
    integer, intent(out), optional  :: rc
! !DESCRIPTION:
!   Fill data into {\tt field} according to {\tt dataFillScheme}. Depending
!   on the chosen fill scheme, the {\tt member} and {\tt step} arguments are
!   used as to provide differing fill data patterns.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Grid)                 :: grid
    real(ESMF_KIND_R8), pointer     :: dataPtr(:,:), lonPtr(:,:), latPtr(:,:)
    integer                         :: i, j

    if (trim(dataFillScheme)=="sincos") then
      ! 2D sin*cos pattern
      ! TODO: support Meshes
      ! TODO: support nD, not just 2D
      call ESMF_FieldGet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=lonPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=latPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do j=lbound(dataPtr,2),ubound(dataPtr,2)
      do i=lbound(dataPtr,1),ubound(dataPtr,1)
        dataPtr(i,j) = sin(real(member)*3.1416*(lonPtr(i,j)+real(step))/180.) &
                     * cos(real(member)*3.1416*(latPtr(i,j)+real(step))/180.)
      enddo
      enddo
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Unknown dataFillScheme requested.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Realize - Realize Fields inside a State on a single Grid with selection
! !INTERFACE:
  ! call using generic interface: NUOPC_Realize
  subroutine NUOPC_RealizeComplete(state, grid, selection, dataFillScheme, rc)
! !ARGUMENTS:
    type(ESMF_State)                :: state
    type(ESMF_Grid)                 :: grid
    character(len=*), optional      :: selection
    character(len=*), optional      :: dataFillScheme    
    integer, intent(out), optional  :: rc
! !DESCRIPTION:
!   Realize the Fields inside of {\tt state}. All of the Fields are created
!   on the same {\tt grid} object. Fields are realized and/or removed from
!   {\tt state} according to {\tt selection} argument. The options are:
!   \begin{itemize}
!   \item {\tt "realize\_all"} (default)
!   \item {\tt "realize\_connected\_remove\_others"}
!   \end{itemize}
!
!   Realized Fields are filled with data if {\tt dataFillScheme} is present.
!   See {\tt NUOOC\_FillData()} for details about the available data fill 
!   schemes.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(len=80), allocatable  :: fieldNameList(:)
    integer                         :: i, itemCount, k
    type(ESMF_Field)                :: field
    character(len=80)               :: selectionOpt

    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(fieldNameList(itemCount))
    call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! optional selection argument
    if (present(selection)) then
      selectionOpt=trim(selection)
    else
      selectionOpt="realize_all"
    endif

    k=1 ! initialize
    do i=1, itemCount
      if (trim(selectionOpt)=="realize_all") then
        ! create a Field
        field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, &
          name=fieldNameList(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! realize the connected Field using the just created Field
        call NUOPC_StateRealizeField(state, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (present(dataFillScheme)) then
          ! a data fill scheme was provided -> use it to initialize
          call NUOPC_FillData(field, dataFillScheme, member=k, step=0, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          k=k+1 ! increment the member counter
        endif
      else if (trim(selectionOpt)=="realize_connected_remove_others") then
        if (NUOPC_StateIsFieldConnected(state, fieldName=fieldNameList(i))) then
          ! create a Field
          field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, &
            name=fieldNameList(i), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! realize the connected Field using the just created Field
          call NUOPC_StateRealizeField(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (present(dataFillScheme)) then
            ! a data fill scheme was provided -> use it to initialize
            call NUOPC_FillData(field, dataFillScheme, member=k, step=0, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            k=k+1 ! increment the member counter
          endif
        else
          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldNameList(i)/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Unknown selection requested.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      endif
    enddo
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Write - Write distributed weights into file
! !INTERFACE:
  ! call using generic interface: NUOPC_Write
  subroutine NUOPC_WriteWeights(factorList, fileName, rc)
! !ARGUMENTS:
    real(ESMF_KIND_R8), pointer               :: factorList(:)
    character(*),       intent(in)            :: fileName
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Each PET calls with its local list of factors. The call then writes the
!   distributed factors into a single file. The order of the factors in the file
!   is first by PET, and within each PET the PET-local order is preserved. 
!   Changing the number of PETs for the same regrid operation will likely change
!   the order of factors across PETs, and therefore files written will differ.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer, allocatable            :: deBlockList(:,:,:), weightsPerPet(:)
    type(ESMF_VM)                   :: vm
    type(ESMF_DistGrid)             :: dg
    type(ESMF_Array)                :: array
    integer                         :: localPet, petCount
    integer                         :: j
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    allocate(weightsPerPet(petCount))
    call ESMF_VMAllGather(vm, (/size(factorList)/), weightsPerPet, &
      count=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    allocate(deBlockList(1,2,petCount))
    do j=1, petCount
      if (j==1) then
        deBlockList(1,1,j) = 1
        deBlockList(1,2,j) = weightsPerPet(1)
      else
        deBlockList(1,1,j) = deBlockList(1,2,j-1) + 1
        deBlockList(1,2,j) = deBlockList(1,1,j) + weightsPerPet(j) - 1
      endif
    enddo
    dg = ESMF_DistGridCreate(minIndex=(/1/), &
      maxIndex=(/deBlockList(1,2,petCount)/), &
      deBlockList=deBlockList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    array = ESMF_ArrayCreate(dg, factorList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_ArrayWrite(array, fileName, &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_ArrayDestroy(array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_DistGridDestroy(dg, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    deallocate(weightsPerPet, deBlockList)
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
