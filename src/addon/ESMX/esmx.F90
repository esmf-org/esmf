!==============================================================================
! ESMX (Earth System Model eXecutable)
!   This file contains the main ESMX application program.
!==============================================================================
#define FILENAME "src/addon/ESMX/esmx.F90"
!==============================================================================

!==============================================================================
! ESMX application
!==============================================================================

program ESMX

  use ESMF
  use NUOPC
  use ESMX_Driver, only: driverSS => SetServices

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: driver
  type(ESMF_Config)       :: config
  character(len=240)      :: fieldDictionary
  logical                 :: logFlush
  type(NUOPC_FreeFormat)  :: ff

  ! Initialize ESMF
  call ESMF_Initialize(configFileName="esmxRun.config", config=config, &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ConfigGetAttribute(config, logFlush, &
    label="ESMX_log_flush:", default=.false., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogSet(flush=logFlush, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("=============================================", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("ESMX (Earth System Model eXecutable) STARTING", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("=============================================", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ConfigGetAttribute(config, fieldDictionary, &
    label="ESMX_field_dictionary:", default="<no-set>", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (trim(fieldDictionary)/="<no-set>") then
    ! Read custom dictionary from YAML file
    call NUOPC_FieldDictionarySetup(fileName=trim(fieldDictionary), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Unable to read Field Dictionary file: "//trim(fieldDictionary), &
      line=__LINE__, &
      file=FILENAME)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Create the esmx driver
  driver = ESMF_GridCompCreate(name="ESMX_Driver", config=config, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! SetServices for the esmx driver
  call ESMF_GridCompSetServices(driver, driverSS, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! read and ingest free format driver attributes
  ff = NUOPC_FreeFormatCreate(config, label="ESMX_attributes::", &
    relaxedflag=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call NUOPC_CompAttributeIngest(driver, ff, addFlag=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call NUOPC_FreeFormatDestroy(ff, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ff = NUOPC_FreeFormatCreate(config, label="ALLCOMP_attributes::", &
    relaxedflag=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call NUOPC_CompAttributeIngest(driver, ff, addFlag=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call NUOPC_FreeFormatDestroy(ff, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call Initialize for the esmx driver
  call ESMF_GridCompInitialize(driver, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call Run for the esmx driver
  call ESMF_GridCompRun(driver, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call Finalize for the esmx driver
  call ESMF_GridCompFinalize(driver, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the esmx driver
  call ESMF_GridCompDestroy(driver, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("=============================================", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("ESMX (Earth System Model eXecutable) FINISHED", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("=============================================", &
    ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=FILENAME)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize(rc=rc)

end program ESMX
