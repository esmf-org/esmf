!==============================================================================
! ESMX_Driver (Earth System Model eXecutable Driver)
!   This file contains the ESMX top level driver.
!==============================================================================
#define FILENAME "src/addon/ESMX/ESMX_Driver.F90"
!==============================================================================

module ESMX_Driver

  use ESMF
  use NUOPC
  use NUOPC_Driver,             driverSS    => SetServices

  include "compUse.inc"

  implicit none

  private

  public SetServices, SetVM, HConfigCreateFoundNode

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
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! Specialize SetModelServices
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! Specialize SetRunSequence
    call NUOPC_CompSpecialize(driver, specLabel=label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    integer                         :: i, j, componentCount, ompNumThreads
    integer, allocatable            :: petList(:)
    type(ESMF_GridComp)             :: comp
    type(ESMF_Config)               :: config
    type(ESMF_HConfig)              :: hconfig, hconfigNode, hconfigNode2
    character(:), allocatable       :: configKey(:)
    character(:), allocatable       :: componentList(:)
    character(:), allocatable       :: compLabel
    character(:), allocatable       :: model
    character(:), allocatable       :: string1, string2
    type(ESMF_Info)                 :: info
    type(type_CompDef), allocatable :: CompDef(:)
    logical                         :: inCompDef, isPresent

    rc = ESMF_SUCCESS

    ! Look for config in the component
    call ESMF_GridCompGet(driver, configIsPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (isPresent) then
      ! Get config from component
      call ESMF_GridCompGet(driver, config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      ! Access hconfig
      call ESMF_ConfigGet(config, hconfig=hconfig, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    else
      ! Attempt to open hconfig from default file "esmxRun.yaml"
      config = ESMF_ConfigCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_ConfigLoadFile(config, filename="esmxRun.yaml", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      ! Set config on the component
      call ESMF_GridCompSet(driver, config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      ! Access hconfig
      call ESMF_ConfigGet(config, hconfig=hconfig, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      ! Find hconfig node that holds driver level attributes, conditionally ingest
      configKey = ["ESMX      ", "Driver    ", "attributes"]
      hconfigNode2 = HConfigCreateFoundNode(hconfig, configKey=configKey, &
        foundFlag=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      if (isPresent) then
        call NUOPC_CompAttributeIngest(driver, hconfigNode2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif
      call ESMF_HConfigDestroy(hconfigNode2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif

    ! Find hconfigNode that holds driver level settings according to configKey
    configKey = ["ESMX  ", "Driver"]
    hconfigNode = HConfigCreateFoundNode(hconfig, configKey=configKey, &
      foundFlag=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (.not.isPresent) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Must provide settings for: "//configKey(1)//":"//configKey(2), &
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    ! Ingest the generic component label list
    componentList = ESMF_HConfigAsStringSeq(hconfigNode, stringLen=32, &
      keyString="componentList", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_HConfigDestroy(hconfigNode, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! Determine componentCount
    componentCount = size(componentList)

    ! Setup CompDef structure
    allocate(CompDef(componentCount))
    include "compDef.inc"

    ! Determine information for each component and add to the driver
    do i=1, componentCount
      ! compLabel
      compLabel=trim(componentList(i))

      ! Find hconfigNode that holds component level settings
      hconfigNode = HConfigCreateFoundNode(hconfig, configKey=[compLabel], &
        foundFlag=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      if (.not.isPresent) then
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must provide settings for component: "//compLabel, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return  ! bail out
      endif

      ! Set model
      isPresent = ESMF_HConfigIsDefined(hconfigNode, keyString="model", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      if (.not.isPresent) then
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must provide `model` setting for component: "//compLabel, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return  ! bail out
      endif
      model = ESMF_HConfigAsString(hconfigNode, keyString="model", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out

      ! set up petList
      isPresent = ESMF_HConfigIsDefined(hconfigNode, keyString="petList", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      if (isPresent) then
        hconfigNode2 = ESMF_HConfigCreateAt(hconfigNode, keyString="petList", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        call NUOPC_IngestPetList(petList, hconfigNode2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        call ESMF_HConfigDestroy(hconfigNode2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      else
        allocate(petList(0))
      endif

      ! Set NUOPC hint for OpenMP
      info = ESMF_InfoCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      isPresent = ESMF_HConfigIsDefined(hconfigNode, &
        keyString="ompNumThreads", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      if (isPresent) then
        ompNumThreads = ESMF_HConfigAsI4(hconfigNode, &
          keyString="ompNumThreads", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        call ESMF_InfoSet(info, key="/NUOPC/Hint/PePerPet/MaxCount", &
          value=ompNumThreads, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif

      ! Search for an entry for this component model inside CompDef
      inCompDef = .false.
      do j=1, componentCount
        if (trim(CompDef(j)%name)=="__uninitialized__") exit
        ! case insensitive string comparison
        string1 = ESMF_UtilStringLowerCase(trim(CompDef(j)%name), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        string2 = ESMF_UtilStringLowerCase(trim(model), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        if (string1==string2) then
          inCompDef = .true.
          exit
        endif
      enddo

      if (inCompDef) then
        ! add child component with SetVM and SetServices in CompDef
        call NUOPC_DriverAddComp(driver, trim(compLabel), config=config, &
          compSetServicesRoutine=CompDef(j)%ssPtr, compSetVMRoutine=CompDef(j)%svPtr, &
          info=info, petList=petList, comp=comp, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg="Unable to add component '"//trim(compLabel)// &
            "' to driver via Fortran module.", &
          line=__LINE__, file=FILENAME)) return  ! bail out
      else
        ! add child component with SetVM and SetServices in shared object
        call NUOPC_DriverAddComp(driver, trim(compLabel), config=config, &
          sharedObj=trim(model), info=info, petList=petList, comp=comp, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, &
          msg="Unable to add component '"//trim(compLabel)// &
            "' to driver via shared object: "//trim(model), &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif

      ! Find hconfig node that holds model level attributes, conditionally ingest
      isPresent = ESMF_HConfigIsDefined(hconfigNode, keyString="attributes", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      if (isPresent) then
        hconfigNode2 = ESMF_HConfigCreateAt(hconfigNode, &
          keyString="attributes", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        call NUOPC_CompAttributeIngest(comp, hconfigNode2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        call ESMF_HConfigDestroy(hconfigNode2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif

      ! clean-up
      deallocate(petList)
      call ESMF_InfoDestroy(info, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_HConfigDestroy(hconfigNode, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    enddo

    ! Ensure clock is set (by parent)
    call ESMF_GridCompGet(driver, clockIsPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (.not.isPresent) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Parent must set the driver clock!", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

  end subroutine SetModelServices

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Config)               :: config
    type(ESMF_HConfig)              :: hconfig, hconfigNode
    character(:), allocatable       :: configKey(:)
    logical                         :: isPresent

    rc = ESMF_SUCCESS

    ! Query the driver for config -> hconfig
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_ConfigGet(config, hconfig=hconfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! Find hconfigNode that holds driver level settings according to configKey
    configKey = ["ESMX  ", "Driver"]
    hconfigNode = HConfigCreateFoundNode(hconfig, configKey=configKey, &
      foundFlag=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (.not.isPresent) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Must provide settings for: "//configKey(1)//":"//configKey(2), &
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return  ! bail out
    endif

    isPresent = ESMF_HConfigIsDefined(hconfigNode, keyString="runSequence", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (isPresent) then
      hconfig = ESMF_HConfigCreateAt(hconfigNode, keyString="runSequence", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call NUOPC_DriverIngestRunSequence(driver, hconfig, &
        autoAddConnectors=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_HConfigDestroy(hconfig, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif

    ! clean-up
    call ESMF_HConfigDestroy(hconfigNode, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine SetRunSequence

  !-----------------------------------------------------------------------------

  function HConfigCreateFoundNode(hconfig, configKey, foundFlag, rc)
    type(ESMF_HConfig)    :: HConfigCreateFoundNode
    type(ESMF_HConfig)    :: hconfig
    character(*)          :: configKey(:)
    logical, intent(out)  :: foundFlag
    integer, intent(out)  :: rc

    ! local variables
    type(ESMF_HConfig)    :: hconfigNodePrev
    integer               :: i
    logical               :: isFlag

    rc = ESMF_SUCCESS

    HConfigCreateFoundNode = ESMF_HConfigCreate(hconfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    foundFlag = .true.
    do i=1, size(configKey)
      isFlag = ESMF_HConfigIsMap(HConfigCreateFoundNode, &
        keyString=configKey(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (.not.isFlag) then
        ! configKey must be a map
        foundFlag = .false.
        exit  ! break out of loop
      endif
      hconfigNodePrev = HConfigCreateFoundNode
      HConfigCreateFoundNode = ESMF_HConfigCreateAt(hconfigNodePrev, &
        keyString=configKey(i),rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_HConfigDestroy(hconfigNodePrev, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

  end function

  !-----------------------------------------------------------------------------

end module ESMX_Driver
