!==============================================================================
! ESMX (Earth System Model eXecutable)
!   This file contains the main ESMX Application program.
!==============================================================================
#define FILENAME "src/addon/ESMX/ESMF_App.F90"
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
  type(ESMF_Config)         :: config
  type(ESMF_HConfig)        :: hconfig, hconfigNode
  character(:), allocatable :: configKey(:)
  character(:), allocatable :: valueString
  logical                   :: isPresent, logFlush
  type(ESMF_Time)           :: startTime, stopTime
  type(ESMF_TimeInterval)   :: timeStep
  type(ESMF_Clock)          :: clock

  ! Initialize ESMF
  configKey = ["ESMX", "App "]
  call ESMF_Initialize(configFilenameFromArgNum=1, & ! arg 1 to spec alt. config
    configFileName="esmxRun.yaml", configKey=configKey, &
    config=config, defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Access hconfig
  call ESMF_ConfigGet(config, hconfig=hconfig, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Find hconfigNode that holds app level settings according to configKey
  hconfigNode = HConfigCreateFoundNode(hconfig, configKey=configKey, &
    foundFlag=isPresent, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (.not.isPresent) then
    call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
      msg="Must provide settings for: "//configKey(1)//":"//configKey(2), &
      line=__LINE__, file=FILENAME, rcToReturn=rc)
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Set logFlush
  isPresent = ESMF_HConfigIsDefined(hconfigNode, keyString="logFlush", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (isPresent) then
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
  isPresent = ESMF_HConfigIsDefined(hconfigNode, keyString="fieldDictionary", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (isPresent) then
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
  driver = ESMF_GridCompCreate(name="ESMX_Driver", config=config, rc=rc)
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
    foundFlag=isPresent, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (isPresent) then
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
