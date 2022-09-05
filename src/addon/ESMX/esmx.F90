!==============================================================================
! ESMX (Earth System Model eXecutable)
!   This file contains the main application program and the top level driver.
!==============================================================================
#define FILENAME "src/addon/ESMX/esmx.F90"
!==============================================================================

module esmxDriver

  use ESMF
  use NUOPC
  use NUOPC_Driver,             driverSS    => SetServices

  include "compUse.inc"

  implicit none

  private

  public SetServices, SetVM

  type type_CompDef
    procedure(SetServices), pointer, nopass :: ssPtr => null()
    procedure(SetVM),       pointer, nopass :: svPtr => null()
    character(ESMF_MAXSTR)                  :: name = "__uninitialized__"
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! Derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, driverSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! Specialize Driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call NUOPC_CompSpecialize(driver, specLabel=label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(len=32)               :: startTimeString, stopTimeString
    type(ESMF_Time)                 :: startTime, stopTime
    type(ESMF_TimeInterval)         :: timeStep
    type(ESMF_Clock)                :: internalClock
    integer                         :: i, j, componentCount, ompNumThreads
    integer, allocatable            :: petList(:)
    type(ESMF_GridComp)             :: comp
    type(ESMF_Config)               :: config
    type(NUOPC_FreeFormat)          :: ff
    character(len=32), allocatable  :: compLabels(:)
    character(len=32)               :: prefix
    character(len=240)              :: model
    type(ESMF_Info)                 :: info
    type(type_CompDef), allocatable :: CompDef(:)
    logical                         :: inCompDef

    ! get the config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! determine the generic component labels
    componentCount = ESMF_ConfigGetLen(config, label="ESMX_component_list:", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    allocate(compLabels(componentCount))
    call ESMF_ConfigGetAttribute(config, valueList=compLabels, &
      label="ESMX_component_list:", count=componentCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! setup CompDef structure
    allocate(CompDef(componentCount))
    include "compDef.inc"

    ! determine information for each component and add to the driver
    do i=1, componentCount
      ! construct component prefix
      prefix=trim(compLabels(i))

      ! set up petList
      ff = NUOPC_FreeFormatCreate(config, label=trim(prefix)//"_petlist:", &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call NUOPC_IngestPetList(petList, ff, rc=rc)
      call ESMF_ConfigGetAttribute(config, model, &
        label=trim(prefix)//"_model:", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      ! set up NUOPC hint for OpenMP
      call ESMF_ConfigGetAttribute(config, ompNumThreads, &
        label=trim(prefix)//"_omp_num_threads:", default=-1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      info = ESMF_InfoCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (ompNumThreads /= -1) then
        call ESMF_InfoSet(info, key="/NUOPC/Hint/PePerPet/MaxCount", &
          value=ompNumThreads, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif

      ! see whether there is an entry for this component inside CompDef
      inCompDef = .false.
      do j=1, componentCount
        if (trim(CompDef(j)%name)=="__uninitialized__") exit
        if (trim(CompDef(j)%name)==trim(model)) then
          inCompDef = .true.
          exit
        endif
      enddo

      if (inCompDef) then
        ! add child component with SetVM and SetServices in CompDef
        call NUOPC_DriverAddComp(driver, trim(prefix), config=config, &
          compSetServicesRoutine=CompDef(j)%ssPtr, compSetVMRoutine=CompDef(j)%svPtr, &
          info=info, petList=petList, comp=comp, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg="Unable to add component '"//trim(prefix)// &
            "' to driver via Fortran module.", &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      else
        ! add child component with SetVM and SetServices in shared object
        call NUOPC_DriverAddComp(driver, trim(prefix), config=config, &
          sharedObj=trim(model), info=info, petList=petList, comp=comp, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg="Unable to add component '"//trim(prefix)// &
            "' to driver via shared object: "//trim(model), &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif

      ! read and ingest free format component attributes
      ff = NUOPC_FreeFormatCreate(config, &
        label=trim(prefix)//"_attributes::", relaxedflag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out        
      call NUOPC_CompAttributeIngest(comp, ff, addFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call NUOPC_FreeFormatDestroy(ff, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      ff = NUOPC_FreeFormatCreate(config, &
        label="ALLCOMP_attributes::", relaxedflag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out        
      call NUOPC_CompAttributeIngest(comp, ff, addFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call NUOPC_FreeFormatDestroy(ff, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      ! clean-up
      deallocate(petList)
      call ESMF_InfoDestroy(info, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    enddo

    deallocate(compLabels)

    ! read startTimeString and stopTimeString from config
    call ESMF_ConfigGetAttribute(config, startTimeString, label="startTime:", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_ConfigGetAttribute(config, stopTimeString, label="stopTime:", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! set the driver clock startTime/stopTime
    call ESMF_TimeSet(startTime, timeString=startTimeString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_TimeSet(stopTime,  timeString=stopTimeString,  rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! set the driver clock default timeStep = stopTime - startTime
    ! use with runSequence @*,
    ! or overwritten with explicit timeStep in runSequence
    timeStep = stopTime - startTime

    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    rc = ESMF_SUCCESS

  end subroutine SetModelServices

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)              :: name
    type(ESMF_Config)                   :: config
    type(NUOPC_FreeFormat)              :: runSeqFF

    rc = ESMF_SUCCESS

    ! query the driver for its name and config
    call ESMF_GridCompGet(driver, name=name, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! read free format run sequence from config
    runSeqFF = NUOPC_FreeFormatCreate(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, &
      autoAddConnectors=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! clean-up
    call NUOPC_FreeFormatDestroy(runSeqFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine SetRunSequence

  !-----------------------------------------------------------------------------

end module esmxDriver

!==============================================================================
! ESMX application
!==============================================================================

program ESMX

  use ESMF
  use NUOPC
  use esmxDriver, only: driverSS => SetServices

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
  driver = ESMF_GridCompCreate(name="esmxDriver", config=config, rc=rc)
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
