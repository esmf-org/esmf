!==============================================================================
! ESMX (Earth System Model eXecutable)
!   This file contains the main ESMX Application program.
!==============================================================================
#define FILENAME "src/addon/ESMX/ESMX_App.F90"
!==============================================================================

!==============================================================================
! ESMX Application
!==============================================================================

program ESMX_App

  use ESMF
  use NUOPC
  use ESMX_Driver, only: driverSS => SetServices, HConfigCreateFoundNode

  implicit none

  integer                   :: rc, urc
  type(ESMF_GridComp)       :: driver
  type(ESMF_HConfig)        :: hconfig, hconfigNode
  character(:), allocatable :: configKey(:)
  character(:), allocatable :: valueString
  logical                   :: isFlag, logFlush
  type(ESMF_Time)           :: startTime, stopTime
  type(ESMF_TimeInterval)   :: timeStep
  type(ESMF_Clock)          :: clock

  ! Initialize ESMF
  configKey = ["ESMX", "App "]
  call ESMF_Initialize(configFilenameFromArgNum=1, & ! arg 1 to spec alt. config
    configFileName="esmxRun.yaml", configKey=configKey, &
    hconfig=hconfig, defaultDefaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Find hconfigNode that holds app level settings according to configKey
  hconfigNode = HConfigCreateFoundNode(hconfig, configKey=configKey, &
    foundFlag=isFlag, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (.not.isFlag) then
    call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
      msg="Must provide settings for: "//configKey(1)//":"//configKey(2), &
      line=__LINE__, file=FILENAME, rcToReturn=rc)
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Validate hconfigNode against ESMX/App controlled key vocabulary
  isFlag = ESMF_HConfigValidateMapKeys(hconfigNode, &
    vocabulary=["defaultLogFilename          ", & ! ESMF_Initialize option
                "logAppendFlag               ", & ! ESMF_Initialize option
                "logKindFlag                 ", & ! ESMF_Initialize option
                "defaultCalKind              ", & ! ESMF_Initialize option
                "globalResourceControl       ", & ! ESMF_Initialize option
                "ESMF_RUNTIME_COMPLIANCECHECK", & ! ESMF_Initialize option
                "ESMF_RUNTIME_GARBAGE        ", & ! ESMF_Initialize option
                "ESMF_RUNTIME_PROFILE        ", & ! ESMF_Initialize option
                "ESMF_RUNTIME_PROFILE_OUTPUT ", & ! ESMF_Initialize option
                "ESMF_RUNTIME_PROFILE_PETLIST", & ! ESMF_Initialize option
                "ESMF_RUNTIME_PROFILE_REGRID ", & ! ESMF_Initialize option
                "ESMF_RUNTIME_TRACE          ", & ! ESMF_Initialize option
                "ESMF_RUNTIME_TRACE_CLOCK    ", & ! ESMF_Initialize option
                "ESMF_RUNTIME_TRACE_PETLIST  ", & ! ESMF_Initialize option
                "ESMF_RUNTIME_TRACE_COMPONENT", & ! ESMF_Initialize option
                "ESMF_RUNTIME_TRACE_FLUSH    ", & ! ESMF_Initialize option
                "startTime                   ", & ! ESMX_App option
                "stopTime                    ", & ! ESMX_App option
                "logFlush                    ", & ! ESMX_App option
                "fieldDictionary             "  & ! ESMX_App option
                ], badKey=valueString, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (.not.isFlag) then
    call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
      msg="An invalid key was found in config under ESMX/App (maybe a typo?): "//valueString, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Set logFlush
  isFlag = ESMF_HConfigIsDefined(hconfigNode, keyString="logFlush", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (isFlag) then
    logFlush = ESMF_HConfigAsLogical(hconfigNode, keyString="logFlush", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_LogSet(flush=logFlush, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Log top banner
  call ESMF_LogWrite("=============================================", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("ESMX (Earth System Model eXecutable) STARTING", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("=============================================", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set field dictionary
  isFlag = ESMF_HConfigIsDefined(hconfigNode, keyString="fieldDictionary", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (isFlag) then
    valueString = ESMF_HConfigAsString(hconfigNode, &
      keyString="fieldDictionary", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call NUOPC_FieldDictionarySetup(fileName=valueString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Unable to read Field Dictionary file: "//valueString, &
      line=__LINE__, file=FILENAME)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Create esmx driver
  driver = ESMF_GridCompCreate(name="ESMX_Driver", hconfig=hconfig, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! SetServices esmx driver
  call ESMF_GridCompSetServices(driver, driverSS, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set run clock
  valueString = ESMF_HConfigAsString(hconfigNode, keyString="startTime", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_TimeSet(startTime, timeString=valueString, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  valueString = ESMF_HConfigAsString(hconfigNode, keyString="stopTime", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_TimeSet(stopTime, timeString=valueString, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! set the driver clock default timeStep = stopTime - startTime
  ! use with runSequence @*,
  ! or overwritten with explicit timeStep in runSequence
  timeStep = stopTime - startTime
  clock = ESMF_ClockCreate(name="ESMX Application Clock", &
    timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the hconfigNode
  call ESMF_HConfigDestroy(hconfigNode, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Find hconfigNode that holds driver level attributes, conditionally ingest
  configKey = ["ESMX      ", "Driver    ", "attributes"]
  hconfigNode = HConfigCreateFoundNode(hconfig, configKey=configKey, &
    foundFlag=isFlag, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (isFlag) then
    call NUOPC_CompAttributeIngest(driver, hconfigNode, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Destroy the hconfigNode
  call ESMF_HConfigDestroy(hconfigNode, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Initialize esmx driver
  call ESMF_GridCompInitialize(driver, clock=clock, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Run esmx driver
  call ESMF_GridCompRun(driver, clock=clock, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize esmx driver
  call ESMF_GridCompFinalize(driver, clock=clock, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the esmx driver
  call ESMF_GridCompDestroy(driver, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the clock
  call ESMF_ClockDestroy(clock, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Log bottom banner
  call ESMF_LogWrite("=============================================", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("ESMX (Earth System Model eXecutable) FINISHED", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("=============================================", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize(rc=rc)
  if (rc /= ESMF_SUCCESS) then
    write (ESMF_UtilIOStderr,*) "Error Finalizing ESMF"
  endif

end program ESMX_App
