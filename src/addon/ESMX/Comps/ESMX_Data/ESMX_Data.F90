module ESMX_Data

  !-----------------------------------------------------------------------------
  ! ESMX Data Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_ModelBase, &
    modelBaseSS    => SetServices

  use dataProcess

  implicit none

  private

  public SetServices, SetVM

  type GeomItem
    character(len=:), allocatable :: name
    type(ESMF_Geom)               :: geom
  end type

  type Validate
    real(ESMF_KIND_R8)            :: min, max, mask
    logical                       :: minGuard, maxGuard, maskGuard
    logical                       :: diagnose
    character(len=:), allocatable :: action
  end type

  type ImportItem
    type(ESMF_Field)              :: field
    type(Validate)                :: dataValidate
  end type

  type ExportItem
    type(ESMF_Field)              :: field
    type(Validate)                :: dataValidate
    character(len=:), allocatable :: dataInit
    character(len=:), allocatable :: dataAdvance
  end type

  type InternalStateStruct
    character(len=:),      allocatable :: timeKeeping    ! "Model" or "Mediator"
    type(GeomItem),        allocatable :: geomItems(:)
    type(ImportItem),      allocatable :: importItems(:)
    type(ExportItem),      allocatable :: exportItems(:)
    integer                            :: stepCounter
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------

  contains

  !-----------------------------------------------------------------------------

  subroutine SetServices(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    character(ESMF_MAXSTR)     :: name
    integer                    :: stat
    type(InternalState)        :: is
    character(len=64)          :: value
    type(ESMF_HConfig)         :: hconfig, hconfigNode
    character(:), allocatable  :: badKey
    logical                    :: isFlag

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(xdata, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! derive generic model phases
    call NUOPC_CompDerive(xdata, modelBaseSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of the internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//__FILE__, &
      rcToReturn=rc)) return  ! bail out
    call ESMF_InternalStateAdd(xdata, internalState=is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! initialize the stepCounter inside the internal state
    is%wrap%stepCounter = 0

    ! specialize model
    call NUOPC_CompSpecialize(xdata, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(xdata, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(xdata, specLabel=label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(xdata, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(xdata, specLabel=label_TimestampExport, &
      specRoutine=TimestampExport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(xdata, specLabel=label_Finalize, &
      specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! validate config
    call ESMF_GridCompGet(xdata, hconfigIsPresent=isFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    if (isFlag) then
      ! Config present, assert it is in the ESMX YAML format
      call ESMF_GridCompGet(xdata, hconfig=hconfig, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      hconfigNode = ESMF_HConfigCreateAt(hconfig, keyString=trim(name), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

      ! component responsibility to validate ESMX handled options here,
      ! and potentially locally handled options
      isFlag = ESMF_HConfigValidateMapKeys(hconfigNode, &
        vocabulary=["model        ", &  ! ESMX_Driver handled option
                    "petList      ", &  ! ESMX_Driver handled option
                    "devList      ", &  ! ESMX_Driver handled option
                    "ompNumThreads", &  ! ESMX_Driver handled option
                    "stdout       ", &  ! ESMX_Driver handled option
                    "stderr       ", &  ! ESMX_Driver handled option
                    "attributes   ", &  ! ESMX_Driver handled option
                    "timeKeeping  ", &  ! ESMX_Data handled option
                    "geometries   ", &  ! ESMX_Data handled option
                    "importFields ", &  ! ESMX_Data handled option
                    "exportFields "  &  ! ESMX_Data handled option
                   ], badKey=badKey, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      if (.not.isFlag) then
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
          msg="An invalid key was found for component '"//trim(name)// &
            "' (maybe a typo?): "//badKey, &
          line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)
        return  ! bail out
      endif

      ! ingest hconfig
      call IngestFromHConfig(hconfigNode, is%wrap%timeKeeping, &
        is%wrap%geomItems, is%wrap%importItems, is%wrap%exportItems, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg="Problem ingesting hconfig.", &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    endif

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine IngestFromHConfig(hconfig, timeKeeping, geoms, imports, exports, &
    rc)
    type(ESMF_HConfig),                 intent(in)  :: hconfig
    character(len=:),      allocatable, intent(out) :: timeKeeping
    type(GeomItem),        allocatable, intent(out) :: geoms(:)
    type(ImportItem),      allocatable, intent(out) :: imports(:)
    type(ExportItem),      allocatable, intent(out) :: exports(:)
    integer,                            intent(out) :: rc

    ! local variables
    character(len=:), allocatable :: tempString
    logical                       :: isFlag
    type(ESMF_HConfig)            :: hconfigNode
    type(ESMF_HConfigIter)        :: hconfigIt, hconfigItBegin, hconfigItEnd
    integer                       :: itemCount, item

    rc=ESMF_SUCCESS

    ! handle timeKeeping
    tempString = ESMF_HConfigAsString(hconfig, keyString="timeKeeping", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg="Must specify 'timeKeeping'!", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    timeKeeping = ESMF_UtilStringUpperCase(tempString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg="Must specify 'timeKeeping'!", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (timeKeeping /= "MODEL" .and. timeKeeping /= "MEDIATOR") then
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="An invalid setting was found for 'timeKeeping' "// &
          "(maybe a typo?): "//tempString, &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    ! handle geometries
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="geometries", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (isFlag) then
      ! ingest geometries
      hconfigNode = ESMF_HConfigCreateAt(hconfig, keyString="geometries", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      itemCount = ESMF_HConfigGetSize(hconfigNode, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      if (itemCount>0) then
        allocate(geoms(itemCount))

        hconfigItBegin = ESMF_HConfigIterBegin(hconfigNode, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        hconfigItEnd = ESMF_HConfigIterEnd(hconfigNode, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        item = 0
        hconfigIt = hconfigItBegin
        do while (ESMF_HConfigIterLoop(hconfigIt, hconfigItBegin, &
          hconfigItEnd, rc=rc))
          ! error check ESMF_HConfigIterLoop()
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          item = item+1

          geoms(item)%geom = GeomCreateFromHConfig(hconfigIt, &
            name=geoms(item)%name, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

        enddo
        ! error check ESMF_HConfigIterLoop()
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

      endif

      call ESMF_HConfigDestroy(hconfigNode, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    endif

    ! handle importFields
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="importFields", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (isFlag) then
      ! ingest importFields
      hconfigNode = ESMF_HConfigCreateAt(hconfig, keyString="importFields", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      itemCount = ESMF_HConfigGetSize(hconfigNode, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      if (itemCount>0) then
        allocate(imports(itemCount))

        hconfigItBegin = ESMF_HConfigIterBegin(hconfigNode, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        hconfigItEnd = ESMF_HConfigIterEnd(hconfigNode, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        item = 0
        hconfigIt = hconfigItBegin
        do while (ESMF_HConfigIterLoop(hconfigIt, hconfigItBegin, &
          hconfigItEnd, rc=rc))
          ! error check ESMF_HConfigIterLoop()
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          item = item+1

          imports(item)%field = FieldCreateFromHConfig(hconfigIt, geoms=geoms, &
            dataValidate=imports(item)%dataValidate, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg="Problem creating import field.", &
            line=__LINE__, file=__FILE__)) return  ! bail out

        enddo
        ! error check ESMF_HConfigIterLoop()
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

      endif

      call ESMF_HConfigDestroy(hconfigNode, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    endif

    ! handle exportFields
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="exportFields", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (isFlag) then
      ! ingest exportFields
      hconfigNode = ESMF_HConfigCreateAt(hconfig, keyString="exportFields", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      itemCount = ESMF_HConfigGetSize(hconfigNode, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      if (itemCount>0) then
        allocate(exports(itemCount))

        hconfigItBegin = ESMF_HConfigIterBegin(hconfigNode, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        hconfigItEnd = ESMF_HConfigIterEnd(hconfigNode, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        item = 0
        hconfigIt = hconfigItBegin
        do while (ESMF_HConfigIterLoop(hconfigIt, hconfigItBegin, &
          hconfigItEnd, rc=rc))
          ! error check ESMF_HConfigIterLoop()
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          item = item+1

          exports(item)%field = FieldCreateFromHConfig(hconfigIt, geoms=geoms, &
            dataValidate=exports(item)%dataValidate, &
            dataInit=exports(item)%dataInit, &
            dataAdvance=exports(item)%dataAdvance, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg="Problem creating export field.", &
            line=__LINE__, file=__FILE__)) return  ! bail out

        enddo
        ! error check ESMF_HConfigIterLoop()
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

      endif

      call ESMF_HConfigDestroy(hconfigNode, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    endif

  end subroutine IngestFromHConfig

  !-----------------------------------------------------------------------------

  function FieldCreateFromHConfig(hconfig, geoms, dataValidate, &
    dataInit, dataAdvance, rc)
    type(ESMF_Field)                           :: FieldCreateFromHConfig
    type(ESMF_HConfigIter),        intent(in)  :: hconfig
    type(GeomItem), allocatable,   intent(in)  :: geoms(:)
    type(Validate),                intent(out) :: dataValidate
    character(len=:), allocatable, intent(out), optional :: dataInit
    character(len=:), allocatable, intent(out), optional :: dataAdvance
    integer,                       intent(out) :: rc

    ! local variables
    logical                       :: isFlag
    type(ESMF_HConfig)            :: hconfigMap, hconfigMap2
    character(:),    allocatable  :: geometry, name, badkey, string
    type(ESMF_Grid)               :: grid
    integer                       :: item
    type(ESMF_TypeKind_Flag)      :: typekind
    integer,         allocatable  :: gridToFieldMap(:)
    integer,         allocatable  :: ungriddedLBound(:)
    integer,         allocatable  :: ungriddedUBound(:)
    integer(ESMF_KIND_I4)         :: valueI4
    integer(ESMF_KIND_I8)         :: valueI8
    real(ESMF_KIND_R4)            :: valueR4
    real(ESMF_KIND_R8)            :: valueR8
    character(len=20), allocatable:: vocabulary(:)
    logical                       :: geomFound

    rc=ESMF_SUCCESS

    name = ESMF_HConfigAsStringMapKey(hconfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! assert this to be a map element
    hconfigMap = ESMF_HConfigCreateAtMapVal(hconfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    isFlag = ESMF_HConfigIsMap(hconfigMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (isFlag) then
      ! validate keys in map
      if (present(dataAdvance)) then
        vocabulary=["geometry       ", &
                    "gridToFieldMap ", &
                    "ungriddedLBound", &
                    "ungriddedUBound", &
                    "typekind       ", &
                    "dataValidate   ", &
                    "dataInit       ", &
                    "dataAdvance    "  ]
      else
        vocabulary=["geometry       ", &
                    "gridToFieldMap ", &
                    "ungriddedLBound", &
                    "ungriddedUBound", &
                    "typekind       ", &
                    "dataValidate   ", &
                    "dataInit       "  ]
      end if
      isFlag = ESMF_HConfigValidateMapKeys(hconfigMap, vocabulary=vocabulary, &
        badKey=badKey, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      deallocate(vocabulary)
      if (.not.isFlag) then
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
          msg="An invalid key was found for field '"//trim(name)//"' "// &
            "(maybe a typo?): "//badKey, &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif

      ! handle geometry (required)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="geometry", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! ingest
        geometry = ESMF_HConfigAsString(hconfigMap, keyString="geometry", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        ! error
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
          msg="The 'geometry' key is required, but missing for field '"//&
            name//"'!", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif

      ! search for geometry match
      geomFound = .false.
      if (allocated(geoms)) then
        do item=1, size(geoms)
          if (geoms(item)%name == geometry) then
            geomFound = .true.
            exit
          endif
        enddo
      endif

      if (.not.geomFound) then
        !TODO: trigger geom transfer for this field, but for now:
        ! error condition
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
          msg="Unknown geometry for field '"//trim(name)//"': "//geometry, &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif

      ! handle typekind (required)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="typekind", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! ingest
        string = ESMF_HConfigAsString(hconfigMap, keyString="typekind", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        string = ESMF_UtilStringUpperCase(string, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        select case (string)
          case ("I4")
            typekind = ESMF_TYPEKIND_I4
          case ("I8")
            typekind = ESMF_TYPEKIND_I8
          case ("R4")
            typekind = ESMF_TYPEKIND_R4
          case ("R8")
            typekind = ESMF_TYPEKIND_R8
          case default
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
              msg="Invalid value for typekind: "//string, &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return  ! bail out
        end select
      else
        ! error
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
          msg="The 'typekind' key is required, but missing for field '"//&
            name//"'!", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif

      ! handle gridToFieldMap (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="gridToFieldMap", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! ingest
        gridToFieldMap = ESMF_HConfigAsI4Seq(hconfigMap, &
          keyString="gridToFieldMap", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif

      ! handle ungriddedLBound (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="ungriddedLBound", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! ingest
        ungriddedLBound = ESMF_HConfigAsI4Seq(hconfigMap, &
          keyString="ungriddedLBound", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif

      ! handle ungriddedUBound (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="ungriddedUBound", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! ingest
        ungriddedUBound = ESMF_HConfigAsI4Seq(hconfigMap, &
          keyString="ungriddedUBound", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif

      ! create the field
      FieldCreateFromHConfig = ESMF_FieldCreate(geoms(item)%geom, &
        typekind=typekind, gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! handle dataValidate (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="dataValidate", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! assert this to be a map element
        hconfigMap2 = ESMF_HConfigCreateAt(hconfigMap, &
          keyString="dataValidate", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        isFlag = ESMF_HConfigIsMap(hconfigMap2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        if (isFlag) then
          ! dataValidate key provided -> ingest
          vocabulary=["min      ", &
                      "max      ", &
                      "mask     ", &
                      "diagnose ", &
                      "action   "  ]
          isFlag = ESMF_HConfigValidateMapKeys(hconfigMap2, &
            vocabulary=vocabulary, badKey=badKey, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          deallocate(vocabulary)
          if (.not.isFlag) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
              msg="An invalid key was found in 'dataValidate' for field "// &
                "'"//trim(name)//"' "// "(maybe a typo?): "//badKey, &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return  ! bail out
          endif
        else
          ! not a map -> error condition
          call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
            msg="The value associated with key 'dataValidate' for field "// &
            "'"//trim(name)//"' must be a map!", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
        endif

        ! handle min (optional)
        isFlag = ESMF_HConfigIsDefined(hconfigMap2, keyString="min", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isFlag) then
          ! ingest and set guard variable
          dataValidate%min = ESMF_HConfigAsR8(hconfigMap2, &
            keyString="min", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          dataValidate%minGuard = .true.
        else
          ! default
          dataValidate%minGuard = .false.
        endif

        ! handle max (optional)
        isFlag = ESMF_HConfigIsDefined(hconfigMap2, keyString="max", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isFlag) then
          ! ingest and set guard variable
          dataValidate%max = ESMF_HConfigAsR8(hconfigMap2, &
            keyString="max", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          dataValidate%maxGuard = .true.
        else
          ! default
          dataValidate%maxGuard = .false.
        endif

        ! handle mask (optional)
        isFlag = ESMF_HConfigIsDefined(hconfigMap2, keyString="mask", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isFlag) then
          ! ingest and set guard variable
          dataValidate%mask = ESMF_HConfigAsR8(hconfigMap2, &
            keyString="mask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          dataValidate%maskGuard = .true.
        else
          ! default
          dataValidate%maskGuard = .false.
        endif

        ! handle diagnose (optional)
        isFlag = ESMF_HConfigIsDefined(hconfigMap2, keyString="diagnose", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isFlag) then
          ! ingest and set guard variable
          dataValidate%diagnose = ESMF_HConfigAsLogical(hconfigMap2, &
            keyString="diagnose", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          ! default
          dataValidate%diagnose = .false.
        endif

        ! handle action (optional)
        isFlag = ESMF_HConfigIsDefined(hconfigMap2, keyString="action", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isFlag) then
          ! ingest and set guard variable
          dataValidate%action = ESMF_HConfigAsString(hconfigMap2, &
            keyString="action", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          ! default
          dataValidate%action = "none"
        endif

      else
        ! dataValidate key not provided, default all members
        dataValidate%minGuard   = .false.
        dataValidate%maxGuard   = .false.
        dataValidate%maskGuard  = .false.
        dataValidate%diagnose   = .false.
        dataValidate%action     = "none"
      endif

      ! upper case to be case insensitive
      dataValidate%action = ESMF_UtilStringUpperCase(dataValidate%action, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! handle dataInit (optional)
      if (present(dataInit)) then
        isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="dataInit", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isFlag) then
          ! dataInit key provided -> read value string
          dataInit = ESMF_HConfigAsString(hconfigMap, &
            keyString="dataInit", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          ! dataInit key not provided, default
          dataInit = ""  ! NOOP
        endif
      endif

      ! handle dataAdvance (optional)
      if (present(dataAdvance)) then
        isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="dataAdvance", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isFlag) then
          ! dataAdvance key provided -> read value string
          dataAdvance = ESMF_HConfigAsString(hconfigMap, &
            keyString="dataAdvance", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          ! dataAdvance key not provided, default
          dataAdvance = ""  ! NOOP
        endif
      endif

    else
      ! not a map -> error condition
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="The value associated with key '"//trim(name)//"' "// &
        "under 'importFields' or 'exportFields' must be a map!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    call ESMF_HConfigDestroy(hconfigMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end function

  !-----------------------------------------------------------------------------

  function GeomCreateFromHConfig(hconfig, name, rc)
    type(ESMF_Geom)                        :: GeomCreateFromHConfig
    type(ESMF_HConfigIter),    intent(in)  :: hconfig
    character(:), allocatable, intent(out) :: name
    integer,                   intent(out) :: rc

    ! local variables
    logical                       :: isFlag
    type(ESMF_HConfig)            :: hconfigMap
    character(:),    allocatable  :: geom
    type(ESMF_Grid)               :: grid
    type(ESMF_StaggerLoc)         :: staggerLoc

    rc=ESMF_SUCCESS

    name = ESMF_HConfigAsStringMapKey(hconfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! assert this to be a map element
    hconfigMap = ESMF_HConfigCreateAtMapVal(hconfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    isFlag = ESMF_HConfigIsMap(hconfigMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (isFlag) then
      ! look for the geom key to determine the kind of geometry

      geom = ESMF_HConfigAsString(hconfigMap, keyString="geom", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      if (geom == "grid1PeriDimUfrm") then
        grid = Grid1PeriDimUfrmFromHConfig(hconfigMap, name=name, &
          staggerLoc=staggerLoc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        GeomCreateFromHConfig = ESMF_GeomCreate(grid, staggerLoc=staggerLoc, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else if (geom == "gridNoPeriDimUfrm") then
        grid = GridNoPeriDimUfrmFromHConfig(hconfigMap, name=name, &
          staggerLoc=staggerLoc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        GeomCreateFromHConfig = ESMF_GeomCreate(grid, staggerLoc=staggerLoc, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
!      else if (geom == "mesh") then
      else
        ! error condition
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
          msg="Unknown geom for '"//trim(name)//"': "//geom, &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif

    else
      ! not a map -> error condition
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="The value associated with key '"//trim(name)//"' "// &
        "under 'geometries' must be a map!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    call ESMF_HConfigDestroy(hconfigMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end function

  !-----------------------------------------------------------------------------

  function Grid1PeriDimUfrmFromHConfig(hconfig, name, staggerLoc, rc)
    type(ESMF_Grid)                     :: Grid1PeriDimUfrmFromHConfig
    type(ESMF_HConfig),     intent(in)  :: hconfig
    character(*),           intent(in)  :: name
    type(ESMF_StaggerLoc),  intent(out) :: staggerLoc
    integer,                intent(out) :: rc

    ! local variables
    logical                         :: isFlag
    character(:),       allocatable :: badKey, string
    integer,            allocatable :: minIndex(:), maxIndex(:)
    real(ESMF_KIND_R8), allocatable :: minCornerCoord(:), maxCornerCoord(:)
    integer                         :: rank
    type(ESMF_CoordSys_Flag), allocatable :: coordSys
    logical,            allocatable :: ignoreNonPeriCoord

    ! validate keys in map
    isFlag = ESMF_HConfigValidateMapKeys(hconfig, &
      vocabulary=["geom               ", &
                  "minIndex           ", &
                  "maxIndex           ", &
                  "minCornerCoord     ", &
                  "maxCornerCoord     ", &
                  "coordSys           ", &
                  "staggerLoc         ", &
                  "ignoreNonPeriCoord "  &
                 ], badKey=badKey, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (.not.isFlag) then
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="An invalid key was found for grid '"//trim(name)//"' "// &
          "(maybe a typo?): "//badKey, &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    ! handle maxIndex (required)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="maxIndex", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      maxIndex = ESMF_HConfigAsI4Seq(hconfig, keyString="maxIndex", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      rank = size(maxIndex)
    else
      ! error
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="The 'maxIndex' key is required for '"//trim(name)//"'!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    ! handle minIndex (optional)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="minIndex", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      minIndex = ESMF_HConfigAsI4Seq(hconfig, keyString="minIndex", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! handle minCornerCoord (required)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="minCornerCoord", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      minCornerCoord = ESMF_HConfigAsR8Seq(hconfig, &
        keyString="minCornerCoord", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else
      ! error
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="The 'minCornerCoord' key is required for '"//trim(name)//"'!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    ! handle maxCornerCoord (required)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="maxCornerCoord", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      maxCornerCoord = ESMF_HConfigAsR8Seq(hconfig, &
        keyString="maxCornerCoord", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else
      ! error
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="The 'maxCornerCoord' key is required for '"//trim(name)//"'!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    ! handle coordSys (optional)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="coordSys", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      string = ESMF_HConfigAsString(hconfig, keyString="coordSys", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      string = ESMF_UtilStringUpperCase(string, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(coordSys)
      select case (string)
        case ("CART")
          coordSys = ESMF_COORDSYS_CART
        case ("SPH_DEG")
          coordSys = ESMF_COORDSYS_SPH_DEG
        case ("SPH_RAD")
          coordSys = ESMF_COORDSYS_SPH_RAD
        case default
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            msg="Invalid value for coordSys: "//string, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
      end select
    endif

    ! handle staggerLoc (optional)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="staggerLoc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      string = ESMF_HConfigAsString(hconfig, keyString="staggerLoc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      string = ESMF_UtilStringUpperCase(string, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      select case (string)
        case ("CENTER")
          staggerLoc = ESMF_STAGGERLOC_CENTER
        case ("CORNER")
          staggerLoc = ESMF_STAGGERLOC_CORNER
        case ("EDGE1")
          staggerLoc = ESMF_STAGGERLOC_EDGE1
        case ("EDGE2")
          staggerLoc = ESMF_STAGGERLOC_EDGE2
        case default
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            msg="Invalid value for staggerLoc: "//string, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
      end select
    else
      ! default
      staggerLoc = ESMF_STAGGERLOC_CENTER
    endif

    ! handle ignoreNonPeriCoord (optional)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="ignoreNonPeriCoord", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      allocate(ignoreNonPeriCoord)
      ignoreNonPeriCoord = ESMF_HConfigAsLogical(hconfig, &
        keyString="ignoreNonPeriCoord", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! create the grid
    Grid1PeriDimUfrmFromHConfig = ESMF_GridCreate1PeriDimUfrm(name=name, &
      minIndex=minIndex, maxIndex=maxIndex, &
      minCornerCoord=minCornerCoord, maxCornerCoord=maxCornerCoord, &
      coordSys=coordSys, staggerLocList=[staggerLoc], &
      ignoreNonPeriCoord=ignoreNonPeriCoord, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end function

  !-----------------------------------------------------------------------------

  function GridNoPeriDimUfrmFromHConfig(hconfig, name, staggerLoc, rc)
    type(ESMF_Grid)                     :: GridNoPeriDimUfrmFromHConfig
    type(ESMF_HConfig),     intent(in)  :: hconfig
    character(*),           intent(in)  :: name
    type(ESMF_StaggerLoc),  intent(out) :: staggerLoc
    integer,                intent(out) :: rc

    ! local variables
    logical                         :: isFlag
    character(:),       allocatable :: badKey, string
    integer,            allocatable :: minIndex(:), maxIndex(:)
    real(ESMF_KIND_R8), allocatable :: minCornerCoord(:), maxCornerCoord(:)
    integer                         :: rank
    type(ESMF_CoordSys_Flag), allocatable :: coordSys

    ! validate keys in map
    isFlag = ESMF_HConfigValidateMapKeys(hconfig, &
      vocabulary=["geom               ", &
                  "minIndex           ", &
                  "maxIndex           ", &
                  "minCornerCoord     ", &
                  "maxCornerCoord     ", &
                  "coordSys           ", &
                  "staggerLoc         "  &
                 ], badKey=badKey, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (.not.isFlag) then
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="An invalid key was found for grid '"//trim(name)//"' "// &
          "(maybe a typo?): "//badKey, &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    ! handle maxIndex (required)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="maxIndex", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      maxIndex = ESMF_HConfigAsI4Seq(hconfig, keyString="maxIndex", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      rank = size(maxIndex)
    else
      ! error
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="The 'maxIndex' key is required for '"//trim(name)//"'!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    ! handle minIndex (optional)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="minIndex", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      minIndex = ESMF_HConfigAsI4Seq(hconfig, keyString="minIndex", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! handle minCornerCoord (required)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="minCornerCoord", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      minCornerCoord = ESMF_HConfigAsR8Seq(hconfig, &
        keyString="minCornerCoord", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else
      ! error
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="The 'minCornerCoord' key is required for '"//trim(name)//"'!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    ! handle maxCornerCoord (required)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="maxCornerCoord", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      maxCornerCoord = ESMF_HConfigAsR8Seq(hconfig, &
        keyString="maxCornerCoord", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else
      ! error
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="The 'maxCornerCoord' key is required for '"//trim(name)//"'!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    ! handle coordSys (optional)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="coordSys", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      string = ESMF_HConfigAsString(hconfig, keyString="coordSys", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      string = ESMF_UtilStringUpperCase(string, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(coordSys)
      select case (string)
        case ("CART")
          coordSys = ESMF_COORDSYS_CART
        case ("SPH_DEG")
          coordSys = ESMF_COORDSYS_SPH_DEG
        case ("SPH_RAD")
          coordSys = ESMF_COORDSYS_SPH_RAD
        case default
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            msg="Invalid value for coordSys: "//string, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
      end select
    endif

    ! handle staggerLoc (optional)
    isFlag = ESMF_HConfigIsDefined(hconfig, keyString="staggerLoc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! ingest
      string = ESMF_HConfigAsString(hconfig, keyString="staggerLoc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      string = ESMF_UtilStringUpperCase(string, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      select case (string)
        case ("CENTER")
          staggerLoc = ESMF_STAGGERLOC_CENTER
        case ("CORNER")
          staggerLoc = ESMF_STAGGERLOC_CORNER
        case ("EDGE1")
          staggerLoc = ESMF_STAGGERLOC_EDGE1
        case ("EDGE2")
          staggerLoc = ESMF_STAGGERLOC_EDGE2
        case default
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            msg="Invalid value for staggerLoc: "//string, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
      end select
    else
      ! default
      staggerLoc = ESMF_STAGGERLOC_CENTER
    endif

    ! create the grid
    GridNoPeriDimUfrmFromHConfig = ESMF_GridCreateNoPeriDimUfrm(name=name, &
      minIndex=minIndex, maxIndex=maxIndex, &
      minCornerCoord=minCornerCoord, maxCornerCoord=maxCornerCoord, &
      coordSys=coordSys, staggerLocList=[staggerLoc], rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end function

  !-----------------------------------------------------------------------------

  subroutine Advertise(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    character(ESMF_MAXSTR)     :: name, fieldName
    integer                    :: stat, i
    type(ESMF_State)           :: importState, exportState
    type(InternalState)        :: is

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(xdata, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query component for internal state
    nullify(is%wrap)
    call ESMF_InternalStateGet(xdata, internalState=is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query for importState and exportState
    call NUOPC_ModelBaseGet(xdata, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! advertise import fields
    if (allocated(is%wrap%importItems)) then
      do i=1, size(is%wrap%importItems)
        call ESMF_FieldGet(is%wrap%importItems(i)%field, name=fieldName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        call NUOPC_Advertise(importState, StandardName=fieldName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      enddo
    endif

    ! advertise export fields
    if (allocated(is%wrap%exportItems)) then
      do i=1, size(is%wrap%exportItems)
        call ESMF_FieldGet(is%wrap%exportItems(i)%field, name=fieldName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        call NUOPC_Advertise(exportState, StandardName=fieldName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      enddo
    endif

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine Realize(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    character(ESMF_MAXSTR)     :: name
    integer                    :: stat, i
    type(ESMF_State)           :: importState, exportState
    type(InternalState)        :: is

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(xdata, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query component for internal state
    nullify(is%wrap)
    call ESMF_InternalStateGet(xdata, internalState=is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query for importState and exportState
    call NUOPC_ModelBaseGet(xdata, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! realize import fields
    if (allocated(is%wrap%importItems)) then
      do i=1, size(is%wrap%importItems)
        call NUOPC_Realize(importState, is%wrap%importItems(i)%field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      enddo
    endif

    ! realize export fields
    if (allocated(is%wrap%exportItems)) then
      do i=1, size(is%wrap%exportItems)
        call NUOPC_Realize(exportState, is%wrap%exportItems(i)%field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      enddo
    endif

  end subroutine Realize

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    character(ESMF_MAXSTR)     :: name
    integer                    :: diagnostic
    integer                    :: stat, i
    type(ESMF_Time)            :: time
    type(ESMF_Clock)           :: clock
    type(ESMF_State)           :: importState
    type(ESMF_State)           :: exportState
    type(InternalState)        :: is
    logical                    :: neededCurrent

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(xdata, name=name, diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query component for internal state
    nullify(is%wrap)
    call ESMF_InternalStateGet(xdata, internalState=is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query component for clock, import, and export states
    call NUOPC_ModelBaseGet(xdata, clock=clock, &
      importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! see if all the fields in the importState are at current time
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    neededCurrent = NUOPC_IsAtTime(importState, time=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    if (neededCurrent) then
      ! indicate that data initialization is complete (breaking out of init-loop)
      call NUOPC_CompAttributeSet(xdata, &
        name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    endif

    ! set all of the fields in the exportState as Updated
    if (allocated(is%wrap%exportItems)) then
      do i=1, size(is%wrap%exportItems)
        call NUOPC_SetAttribute(is%wrap%exportItems(i)%field, &
          name="Updated", value="true", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      enddo
    endif

    ! Initialize the data in export fields
    call DataHandling(importState, is%wrap%exportItems, is%wrap%stepCounter, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    if (btest(diagnostic,17)) then
      ! write fields of the importState
      call NUOPC_Write(importState, &
        fileNamePrefix="field_"//trim(name)//"_import_data_initialize_", &
        status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      ! write fields of the exportState
      call NUOPC_Write(exportState, &
        fileNamePrefix="field_"//trim(name)//"_export_data_initialize_", &
        status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    endif

  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine Advance(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    character(ESMF_MAXSTR)     :: name, fieldName
    integer                    :: diagnostic
    type(ESMF_Clock)           :: clock
    type(ESMF_State)           :: importState
    type(ESMF_State)           :: exportState
    character(len=160)         :: clockString
    integer                    :: i, localPet
    type(ESMF_FileStatus_Flag) :: filestatus
    type(InternalState)        :: is
    integer                    :: statsCount, warnCount, errCount
    real(ESMF_KIND_R8)         :: statsMean, statsMin, statsMax
    logical                    :: statsOkay, headerPrinted

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(xdata, name=name, diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    call ESMF_GridCompGet(xdata, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query component for internal state
    nullify(is%wrap)
    call ESMF_InternalStateGet(xdata, internalState=is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    associate(stepCounter => is%wrap%stepCounter)
    stepCounter=stepCounter+1

    ! query component for import and export states
    call NUOPC_ModelBaseGet(xdata, clock=clock, &
      importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    if (btest(diagnostic,17)) then
      ! write fields of the importState
      filestatus=ESMF_FILESTATUS_OLD
      if (stepCounter==1) filestatus=ESMF_FILESTATUS_REPLACE
      call NUOPC_Write(importState, &
        fileNamePrefix="field_"//trim(name)//"_import_advance_", &
        timeslice=stepCounter, status=filestatus, relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    endif

    ! reset counters
    warnCount = 0
    errCount = 0

    ! diagnose and check import fields
    if (allocated(is%wrap%importItems)) then
      headerPrinted = .false.
      do i=1, size(is%wrap%importItems)
        associate(dataValidate => is%wrap%importItems(i)%dataValidate)
        if (.not.dataValidate%diagnose .and. &
          dataValidate%action /= "WARNING" .and. &
          dataValidate%action /= "ERROR") cycle
        call FieldStats(is%wrap%importItems(i)%field, dataValidate, &
          statsCount=statsCount, statsMean=statsMean, statsMin=statsMin, &
          statsMax=statsMax, statsOkay=statsOkay, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        if (dataValidate%diagnose .or. .not.statsOkay) then
          if (localPet == 0) then
            if (.not.headerPrinted) then
              headerPrinted = .true.
              call ESMF_ClockPrint(clock, options="currTime", &
                unit=clockString, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail
              write(*,'(A,1X,A)') trim(name)//": Model Advance at: ", &
                trim(clockString)
              write(*,'(A)') trim(name)//": Import Fields"
              write(*,'(A,1X,A25,1X,A9,3(1X,A9),1X,A4)') &
                trim(name)//":", "FIELD", "COUNT", "MEAN", "MIN", "MAX", "OKAY"
            endif
            call ESMF_FieldGet(is%wrap%importItems(i)%field, name=fieldName, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
            write(*,'(A,1X,A25,1X,I9,3(1X,E9.2),1X,L4)') &
              trim(name)//":", trim(fieldName), &
              statsCount, statsMean, statsMin, statsMax, statsOkay
          endif
        endif
        if (.not.statsOkay) then
          if (dataValidate%action == "WARNING") warnCount = warnCount + 1
          if (dataValidate%action == "ERROR") errCount = errCount + 1
        endif
        end associate
      enddo
    endif

    ! Advance the data in export fields
    call DataHandling(importState, is%wrap%exportItems, is%wrap%stepCounter, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! diagnose and check export fields
    if (allocated(is%wrap%exportItems)) then
      headerPrinted = .false.
      do i=1, size(is%wrap%exportItems)
        associate(dataValidate => is%wrap%exportItems(i)%dataValidate)
        if (.not.dataValidate%diagnose .and. &
          dataValidate%action /= "WARNING" .and. &
          dataValidate%action /= "ERROR") cycle
        call FieldStats(is%wrap%exportItems(i)%field, dataValidate, &
          statsCount=statsCount, statsMean=statsMean, statsMin=statsMin, &
          statsMax=statsMax, statsOkay=statsOkay, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        if (dataValidate%diagnose .or. .not.statsOkay) then
          if (localPet == 0) then
            if (.not.headerPrinted) then
              headerPrinted = .true.
              call ESMF_ClockPrint(clock, options="currTime", &
                unit=clockString, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail
              write(*,'(A,1X,A)') trim(name)//": Model Advance at: ", &
                trim(clockString)
              write(*,'(A)') trim(name)//": Export Fields"
              write(*,'(A,1X,A25,1X,A9,3(1X,A9),1X,A4)') &
                trim(name)//":", "FIELD", "COUNT", "MEAN", "MIN", "MAX", "OKAY"
            endif
            call ESMF_FieldGet(is%wrap%exportItems(i)%field, name=fieldName, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
            write(*,'(A,1X,A25,1X,I9,3(1X,E9.2),1X,L4)') &
              trim(name)//":", trim(fieldName), &
              statsCount, statsMean, statsMin, statsMax, statsOkay
          endif
        endif
        if (.not.statsOkay) then
          if (dataValidate%action == "WARNING") warnCount = warnCount + 1
          if (dataValidate%action == "ERROR") errCount = errCount + 1
        endif
        end associate
      enddo
    endif

    if (btest(diagnostic,17)) then
      ! write fields of the exportState
      filestatus=ESMF_FILESTATUS_OLD
      if (stepCounter==1) filestatus=ESMF_FILESTATUS_REPLACE
      call NUOPC_Write(exportState, &
        fileNamePrefix="field_"//trim(name)//"_export_advance_", &
        timeslice=stepCounter, status=filestatus, relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    endif

    ! handle warnCount
    if (warnCount > 0) then
      call ESMF_LogWrite( &
        msg="Found fields with value outside valid [min,max] range!", &
        logmsgFlag=ESMF_LOGMSG_WARNING, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    endif

    ! handle errCount
    if (errCount > 0) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
        msg="Found fields with value outside valid [min,max] range!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    end associate

  end subroutine Advance

  !-----------------------------------------------------------------------------

  subroutine FieldStats(field, dataValidate, statsCount, statsMean, statsMin, &
    statsMax, statsOkay, rc)
    ! arguments
    type(ESMF_Field)                :: field
    type(Validate)                  :: dataValidate
    integer,            intent(out) :: statsCount
    real(ESMF_KIND_R8), intent(out) :: statsMean, statsMin, statsMax
    logical,            intent(out) :: statsOkay
    integer,            intent(out) :: rc
    ! local variables
    type(ESMF_VM)                   :: vm
    type(ESMF_TypeKind_Flag)        :: typekind
    integer                         :: rank
    integer                         :: lcount(1), gcount(1)
    real(ESMF_KIND_R8)              :: lsum(1), lmin(1), lmax(1)
    real(ESMF_KIND_R8)              :: gsum(1), gmin(1), gmax(1)
    real(ESMF_KIND_R8)              :: dataMin, dataMax

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_FieldGet(field, typekind=typekind, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 2) then
      if (typekind == ESMF_TYPEKIND_I4) then
        block
          integer(ESMF_KIND_I4), pointer  :: fptr(:,:)
          integer(ESMF_KIND_I4)           :: dataMask
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataValidate%maskGuard) then
            dataMask = dataValidate%mask
            lcount(1) = count(fptr/=dataMask)
            lsum(1)   = sum(fptr, fptr/=dataMask)
            lmin(1)   = minval(fptr, fptr/=dataMask)
            lmax(1)   = maxval(fptr, fptr/=dataMask)
          else
            lcount(1) = size(fptr)
            lsum(1)   = sum(fptr)
            lmin(1)   = minval(fptr)
            lmax(1)   = maxval(fptr)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_I8) then
        block
          integer(ESMF_KIND_I8), pointer  :: fptr(:,:)
          integer(ESMF_KIND_I8)           :: dataMask
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataValidate%maskGuard) then
            dataMask = dataValidate%mask
            lcount(1) = count(fptr/=dataMask)
            lsum(1)   = sum(fptr, fptr/=dataMask)
            lmin(1)   = minval(fptr, fptr/=dataMask)
            lmax(1)   = maxval(fptr, fptr/=dataMask)
          else
            lcount(1) = size(fptr)
            lsum(1)   = sum(fptr)
            lmin(1)   = minval(fptr)
            lmax(1)   = maxval(fptr)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_R4) then
        block
          real(ESMF_KIND_R4), pointer  :: fptr(:,:)
          real(ESMF_KIND_R4)           :: dataMask
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataValidate%maskGuard) then
            dataMask = dataValidate%mask
            lcount(1) = count(fptr/=dataMask)
            lsum(1)   = sum(fptr, fptr/=dataMask)
            lmin(1)   = minval(fptr, fptr/=dataMask)
            lmax(1)   = maxval(fptr, fptr/=dataMask)
          else
            lcount(1) = size(fptr)
            lsum(1)   = sum(fptr)
            lmin(1)   = minval(fptr)
            lmax(1)   = maxval(fptr)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_R8) then
        block
          real(ESMF_KIND_R8), pointer  :: fptr(:,:)
          real(ESMF_KIND_R8)           :: dataMask
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataValidate%maskGuard) then
            dataMask = dataValidate%mask
            lcount(1) = count(fptr/=dataMask)
            lsum(1)   = sum(fptr, fptr/=dataMask)
            lmin(1)   = minval(fptr, fptr/=dataMask)
            lmax(1)   = maxval(fptr, fptr/=dataMask)
          else
            lcount(1) = size(fptr)
            lsum(1)   = sum(fptr)
            lmin(1)   = minval(fptr)
            lmax(1)   = maxval(fptr)
          endif
        end block
      else
        ! error condition: unsupported typekind
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="Unsupported typekind!", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif
    else if (rank == 3) then
      if (typekind == ESMF_TYPEKIND_I4) then
        block
          integer(ESMF_KIND_I4), pointer  :: fptr(:,:,:)
          integer(ESMF_KIND_I4)           :: dataMask
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataValidate%maskGuard) then
            dataMask = dataValidate%mask
            lcount(1) = count(fptr/=dataMask)
            lsum(1)   = sum(fptr, fptr/=dataMask)
            lmin(1)   = minval(fptr, fptr/=dataMask)
            lmax(1)   = maxval(fptr, fptr/=dataMask)
          else
            lcount(1) = size(fptr)
            lsum(1)   = sum(fptr)
            lmin(1)   = minval(fptr)
            lmax(1)   = maxval(fptr)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_I8) then
        block
          integer(ESMF_KIND_I8), pointer  :: fptr(:,:,:)
          integer(ESMF_KIND_I8)           :: dataMask
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataValidate%maskGuard) then
            dataMask = dataValidate%mask
            lcount(1) = count(fptr/=dataMask)
            lsum(1)   = sum(fptr, fptr/=dataMask)
            lmin(1)   = minval(fptr, fptr/=dataMask)
            lmax(1)   = maxval(fptr, fptr/=dataMask)
          else
            lcount(1) = size(fptr)
            lsum(1)   = sum(fptr)
            lmin(1)   = minval(fptr)
            lmax(1)   = maxval(fptr)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_R4) then
        block
          real(ESMF_KIND_R4), pointer  :: fptr(:,:,:)
          real(ESMF_KIND_R4)           :: dataMask
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataValidate%maskGuard) then
            dataMask = dataValidate%mask
            lcount(1) = count(fptr/=dataMask)
            lsum(1)   = sum(fptr, fptr/=dataMask)
            lmin(1)   = minval(fptr, fptr/=dataMask)
            lmax(1)   = maxval(fptr, fptr/=dataMask)
          else
            lcount(1) = size(fptr)
            lsum(1)   = sum(fptr)
            lmin(1)   = minval(fptr)
            lmax(1)   = maxval(fptr)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_R8) then
        block
          real(ESMF_KIND_R8), pointer  :: fptr(:,:,:)
          real(ESMF_KIND_R8)           :: dataMask
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataValidate%maskGuard) then
            dataMask = dataValidate%mask
            lcount(1) = count(fptr/=dataMask)
            lsum(1)   = sum(fptr, fptr/=dataMask)
            lmin(1)   = minval(fptr, fptr/=dataMask)
            lmax(1)   = maxval(fptr, fptr/=dataMask)
          else
            lcount(1) = size(fptr)
            lsum(1)   = sum(fptr)
            lmin(1)   = minval(fptr)
            lmax(1)   = maxval(fptr)
          endif
        end block
      else
        ! error condition: unsupported typekind
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="Unsupported typekind!", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif
    else
      ! error condition: unsupported rank
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Unsupported rank!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    call ESMF_VMAllReduce(vm, sendData=lcount, &
      recvData=gcount, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    statsCount = gcount(1)

    call ESMF_VMAllReduce(vm, sendData=lsum, &
      recvData=gsum, count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (statsCount > 0) then
      statsMean = gsum(1)/statsCount
    else
      statsMean = 0._ESMF_KIND_R8
    endif

    call ESMF_VMAllReduce(vm, sendData=lmin, &
      recvData=gmin, count=1, reduceflag=ESMF_REDUCE_MIN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    statsMin = gmin(1)

    call ESMF_VMAllReduce(vm, sendData=lmax, &
      recvData=gmax, count=1, reduceflag=ESMF_REDUCE_MAX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    statsMax = gmax(1)

    statsOkay = .true. ! initialize to .true. then see if not so

    if (dataValidate%minGuard) then
      if (statsMin < dataValidate%min) statsOkay = .false.  ! values below min
    endif

    if (dataValidate%maxGuard) then
      if (statsMax > dataValidate%max) statsOkay = .false.  ! values above max
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataHandling(importState, exportItems, step, rc)
    type(ESMF_State)              :: importState
    type(ExportItem), allocatable :: exportItems(:)
    integer,          intent(in)  :: step
    integer,          intent(out) :: rc

    integer                       :: i
    character(len=:), allocatable :: expression

    rc = ESMF_SUCCESS

    ! Early return if there is nothing to be done
    if (.not.allocated(exportItems)) return

    do i=1, size(exportItems)

      if (step==0) then
        expression = exportItems(i)%dataInit
      else
        expression = exportItems(i)%dataAdvance
      endif

      if (expression == "") cycle  ! NOOP

      call process(importState, expression, exportItems(i)%field, step, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    end do

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine TimestampExport(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    character(ESMF_MAXSTR)     :: name
    type(ESMF_Clock)           :: clock
    type(type_InternalState)   :: modelBaseIs
    type(InternalState)        :: is

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(xdata, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query component for modelBase internal state
    nullify(modelBaseIs%wrap)
#ifdef ESMF_NO_F2018ASSUMEDTYPE
    call ESMF_UserCompGetInternalState(xdata, label_InternalState, &
      modelBaseIs, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
    call ESMF_UserCompGetInternalState(xdata, label_InternalState, &
      modelBaseIs, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#endif

    ! query component for internal state
    nullify(is%wrap)
    call ESMF_InternalStateGet(xdata, internalState=is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! use correct timeKeeping
    if (is%wrap%timeKeeping == "MODEL") then
      ! Model style timeKeeping -> timestamp exports with post-Advance time

      ! query component for clock
      call NUOPC_ModelBaseGet(xdata, clock=clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

      ! update timestamp on export Fields
      if (associated(modelBaseIs%wrap%cachedExportFieldList)) then
        call NUOPC_SetTimestamp(modelBaseIs%wrap%cachedExportFieldList, &
          clock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      endif

    else
      ! Mediator style timeKeeping -> timestamp exports with pre-Advance time

      ! update timestamp on export Fields
      if (associated(modelBaseIs%wrap%cachedExportFieldList)) then
        call NUOPC_SetTimestamp(modelBaseIs%wrap%cachedExportFieldList, &
          modelBaseIs%wrap%preAdvanceCurrTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      endif

    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    character(ESMF_MAXSTR)     :: name
    integer                    :: i, stat
    type(InternalState)        :: is

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(xdata, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query component for internal state
    nullify(is%wrap)
    call ESMF_InternalStateGet(xdata, internalState=is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! destroy importItems
    if (allocated(is%wrap%importItems)) then
      do i=1, size(is%wrap%importItems)
        call ESMF_FieldDestroy(is%wrap%importItems(i)%field, noGarbage=.true., &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      enddo
      deallocate(is%wrap%importItems)
    endif

    ! destroy exportItems
    if (allocated(is%wrap%exportItems)) then
      do i=1, size(is%wrap%exportItems)
        call ESMF_FieldDestroy(is%wrap%exportItems(i)%field, noGarbage=.true., &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        deallocate(is%wrap%exportItems(i)%dataInit)
        deallocate(is%wrap%exportItems(i)%dataAdvance)
      enddo
      deallocate(is%wrap%exportItems)
    endif

    ! destroy geomItems
    if (allocated(is%wrap%geomItems)) then
      do i=1, size(is%wrap%geomItems)
        call ESMF_GeomDestroy(is%wrap%geomItems(i)%geom, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      enddo
      deallocate(is%wrap%geomItems)
    endif

    ! deallocate the internal state
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Internal State memory deallocation failed.", &
      line=__LINE__, file=trim(name)//":"//__FILE__, &
      rcToReturn=rc)) return  ! bail out

  end subroutine Finalize

  !-----------------------------------------------------------------------------

end module ESMX_Data
