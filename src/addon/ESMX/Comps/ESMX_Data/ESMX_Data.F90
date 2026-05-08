module ESMX_Data

  !-----------------------------------------------------------------------------
  ! ESMX Data Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_ModelBase, &
    modelBaseSS    => SetServices

  implicit none

  private

  public SetServices, SetVM

  type GeomItem
    character(len=:), allocatable :: name
    type(ESMF_Geom)               :: geom
  end type

  type ImportItem
    type(ESMF_Field)              :: field
    logical                       :: dataDiagnose
    character(len=:), allocatable :: dataValidate
  end type

  type ExportItem
    type(ESMF_Field)              :: field
    logical                       :: dataDiagnose
    character(len=:), allocatable :: dataValidate
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
            dataDiagnose=imports(item)%dataDiagnose, &
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
            dataDiagnose=exports(item)%dataDiagnose, &
            dataValidate=exports(item)%dataValidate, &
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

  function FieldCreateFromHConfig(hconfig, geoms, dataDiagnose, dataValidate, &
    dataAdvance, rc)
    type(ESMF_Field)                           :: FieldCreateFromHConfig
    type(ESMF_HConfigIter),        intent(in)  :: hconfig
    type(GeomItem),                intent(in)  :: geoms(:)
    logical,                       intent(out) :: dataDiagnose
    character(len=:), allocatable, intent(out) :: dataValidate
    character(len=:), allocatable, intent(out), optional :: dataAdvance
    integer,                       intent(out) :: rc

    ! local variables
    logical                       :: isFlag
    type(ESMF_HConfig)            :: hconfigMap
    character(:),    allocatable  :: geometry, name, badkey, string
    type(ESMF_Grid)               :: grid
    integer                       :: item
    type(ESMF_TypeKind_Flag)      :: typekind
    integer,         allocatable  :: gridToFieldMap(:)
    integer,         allocatable  :: ungriddedLBound(:)
    integer,         allocatable  :: ungriddedUBound(:)
    type(ESMF_Info)               :: info
    integer(ESMF_KIND_I4)         :: valueI4
    integer(ESMF_KIND_I8)         :: valueI8
    real(ESMF_KIND_R4)            :: valueR4
    real(ESMF_KIND_R8)            :: valueR8
    character(len=20), allocatable:: vocabulary(:)

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
                    "dataInit       ", &
                    "dataMask       ", &
                    "dataMin        ", &
                    "dataMax        ", &
                    "typekind       ", &
                    "dataDiagnose   ", &
                    "dataValidate   ", &
                    "dataAdvance    "  ]
      else
        vocabulary=["geometry       ", &
                    "gridToFieldMap ", &
                    "ungriddedLBound", &
                    "ungriddedUBound", &
                    "dataInit       ", &
                    "dataMask       ", &
                    "dataMin        ", &
                    "dataMax        ", &
                    "typekind       ", &
                    "dataDiagnose   ", &
                    "dataValidate   "  ]
      end if
      isFlag = ESMF_HConfigValidateMapKeys(hconfigMap, vocabulary=vocabulary, &
        badKey=badKey, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
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

      ! search for match in geoms list
      do item=1, size(geoms)
        if (geoms(item)%name == geometry) exit
      enddo

      if (item == size(geoms)+1) then
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

      ! access the info object
      call ESMF_InfoGetFromHost(FieldCreateFromHConfig, info=info, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! handle dataInit (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="dataInit", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! ingest key and set as field info metadata
        call InfoIngestFromHConfig(info, hconfigMap, key="dataInit", &
          typekind=typekind, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        ! for now call FieldFill() right here... this may move into realize
        ! always use valueR8, because that is what FieldFill() takes for const
        if (typekind == ESMF_TYPEKIND_I4) then
          call ESMF_InfoGet(info, key="dataInit", value=valueI4, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          valueR8 = real(valueI4,ESMF_KIND_R8)
        else if (typekind == ESMF_TYPEKIND_I8) then
          call ESMF_InfoGet(info, key="dataInit", value=valueI8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          valueR8 = real(valueI8,ESMF_KIND_R8)
        else if (typekind == ESMF_TYPEKIND_R4) then
          call ESMF_InfoGet(info, key="dataInit", value=valueR4, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          valueR8 = real(valueR4,ESMF_KIND_R8)
        else if (typekind == ESMF_TYPEKIND_R8) then
          call ESMF_InfoGet(info, key="dataInit", value=valueR8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        call ESMF_FieldFill(FieldCreateFromHConfig, dataFillScheme="const", &
          const1=valueR8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif

      ! handle dataMask (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="dataMask", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! ingest key and set as field info metadata
        call InfoIngestFromHConfig(info, hconfigMap, key="dataMask", &
          typekind=typekind, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif

      ! handle dataMin (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="dataMin", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! ingest key and set as field info metadata
        call InfoIngestFromHConfig(info, hconfigMap, key="dataMin", &
          typekind=typekind, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif

      ! handle dataMax (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="dataMax", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! ingest key and set as field info metadata
        call InfoIngestFromHConfig(info, hconfigMap, key="dataMax", &
          typekind=typekind, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif

      ! handle dataDiagnose (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="dataDiagnose", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! dataDiagnose key provided -> read value
        dataDiagnose = ESMF_HConfigAsLogical(hconfigMap, &
          keyString="dataDiagnose", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        ! dataDiagnose key not provided, default
        dataDiagnose = .false.
      endif

      ! handle dataValidate (optional)
      isFlag = ESMF_HConfigIsDefined(hconfigMap, keyString="dataValidate", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (isFlag) then
        ! dataValidate key provided -> read value string
        dataValidate = ESMF_HConfigAsString(hconfigMap, &
          keyString="dataValidate", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        dataValidate = ESMF_UtilStringUpperCase(dataValidate, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        ! dataValidate key not provided, default
        dataValidate = "NO"
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
        "under 'geometries' must be a map!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    call ESMF_HConfigDestroy(hconfigMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end function

  !-----------------------------------------------------------------------------

  subroutine InfoIngestFromHConfig(info, hconfig, key, typekind, rc)
    type(ESMF_Info),          intent(inout) :: info
    type(ESMF_HConfig),       intent(in)    :: hconfig
    character(*),             intent(in)    :: key
    type(ESMF_TypeKind_Flag), intent(in)    :: typekind
    integer,                  intent(out)   :: rc

    ! local variables
    integer(ESMF_KIND_I4)         :: valueI4
    integer(ESMF_KIND_I8)         :: valueI8
    real(ESMF_KIND_R4)            :: valueR4
    real(ESMF_KIND_R8)            :: valueR8

    rc=ESMF_SUCCESS

    if (typekind == ESMF_TYPEKIND_I4) then
      valueI4 = ESMF_HConfigAsI4(hconfig, keyString=key, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_InfoSet(info, key=key, value=valueI4, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (typekind == ESMF_TYPEKIND_I8) then
      valueI8 = ESMF_HConfigAsI8(hconfig, keyString=key, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_InfoSet(info, key=key, value=valueI8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (typekind == ESMF_TYPEKIND_R4) then
      valueR4 = ESMF_HConfigAsR4(hconfig, keyString=key, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_InfoSet(info, key=key, value=valueR4, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (typekind == ESMF_TYPEKIND_R8) then
      valueR8 = ESMF_HConfigAsR8(hconfig, keyString=key, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_InfoSet(info, key=key, value=valueR8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else
      call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
        msg="Unsupported typekind setting!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

  end subroutine

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

      if (geom == "grid1PeriDim") then
        grid = Grid1PeriDimFromHConfig(hconfigMap, name=name, &
          staggerLoc=staggerLoc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        GeomCreateFromHConfig = ESMF_GeomCreate(grid, staggerLoc=staggerLoc, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else if (geom == "gridNoPeriDim") then
        grid = GridNoPeriDimFromHConfig(hconfigMap, name=name, &
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

  function Grid1PeriDimFromHConfig(hconfig, name, staggerLoc, rc)
    type(ESMF_Grid)                     :: Grid1PeriDimFromHConfig
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
    Grid1PeriDimFromHConfig = ESMF_GridCreate1PeriDimUfrm(name=name, &
      minIndex=minIndex, maxIndex=maxIndex, &
      minCornerCoord=minCornerCoord, maxCornerCoord=maxCornerCoord, &
      coordSys=coordSys, staggerLocList=[staggerLoc], &
      ignoreNonPeriCoord=ignoreNonPeriCoord, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end function

  !-----------------------------------------------------------------------------

  function GridNoPeriDimFromHConfig(hconfig, name, staggerLoc, rc)
    type(ESMF_Grid)                     :: GridNoPeriDimFromHConfig
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
    GridNoPeriDimFromHConfig = ESMF_GridCreateNoPeriDimUfrm(name=name, &
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

    ! Advance the data in export fields
    call DataAdvance(importState, is%wrap%exportItems, rc=rc)
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
    call ESMF_ClockPrint(clock, options="currTime", &
      unit=clockString, rc=rc)
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

    ! write to standard out
    if (localPet == 0) then
      write(*,'(A,1X,A)') trim(name)//": Model Advance at: ",trim(clockString)
    endif

    ! reset counters
    warnCount = 0
    errCount = 0

    ! diagnose and check import fields
    if (allocated(is%wrap%importItems)) then
      headerPrinted = .false.
      do i=1, size(is%wrap%importItems)
        if (.not.is%wrap%importItems(i)%dataDiagnose .and. &
          is%wrap%importItems(i)%dataValidate /= "WARN" .and. &
          is%wrap%importItems(i)%dataValidate /= "ERR") cycle
        call FieldStats(is%wrap%importItems(i)%field, statsCount=statsCount, &
          statsMean=statsMean, statsMin=statsMin, statsMax=statsMax, &
          statsOkay=statsOkay, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        if (is%wrap%importItems(i)%dataDiagnose .or. .not.statsOkay) then
          if (localPet == 0) then
            if (.not.headerPrinted) then
              headerPrinted = .true.
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
          if (is%wrap%importItems(i)%dataValidate == "WARN") &
            warnCount = warnCount + 1
          if (is%wrap%importItems(i)%dataValidate == "ERR") &
            errCount = errCount + 1
        endif
      enddo
    endif

    ! Advance the data in export fields
    call DataAdvance(importState, is%wrap%exportItems, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! diagnose and check export fields
    if (allocated(is%wrap%exportItems)) then
      headerPrinted = .false.
      do i=1, size(is%wrap%exportItems)
        if (.not.is%wrap%exportItems(i)%dataDiagnose .and. &
          is%wrap%exportItems(i)%dataValidate /= "WARN" .and. &
          is%wrap%exportItems(i)%dataValidate /= "ERR") cycle
        call FieldStats(is%wrap%exportItems(i)%field, statsCount=statsCount, &
          statsMean=statsMean, statsMin=statsMin, statsMax=statsMax, &
          statsOkay=statsOkay, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        if (is%wrap%importItems(i)%dataDiagnose .or. .not.statsOkay) then
          if (localPet == 0) then
            if (.not.headerPrinted) then
              headerPrinted = .true.
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
          if (is%wrap%exportItems(i)%dataValidate == "WARN") &
            warnCount = warnCount + 1
          if (is%wrap%exportItems(i)%dataValidate == "ERR") &
            errCount = errCount + 1
        endif
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

  subroutine FieldStats(field, statsCount, statsMean, statsMin, statsMax, &
    statsOkay, rc)
    ! arguments
    type(ESMF_Field)                :: field
    integer,            intent(out) :: statsCount
    real(ESMF_KIND_R8), intent(out) :: statsMean, statsMin, statsMax
    logical,            intent(out) :: statsOkay
    integer,            intent(out) :: rc
    ! local variables
    logical                         :: isFlag
    type(ESMF_VM)                   :: vm
    type(ESMF_TypeKind_Flag)        :: typekind
    integer                         :: rank
    integer                         :: lcount(1), gcount(1)
    real(ESMF_KIND_R8)              :: lsum(1), lmin(1), lmax(1)
    real(ESMF_KIND_R8)              :: gsum(1), gmin(1), gmax(1)
    real(ESMF_KIND_R8)              :: dataMin, dataMax
    logical                         :: dataMinSet, dataMaxSet
    type(ESMF_Info)                 :: info

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_FieldGet(field, typekind=typekind, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_InfoGetFromHost(field, info=info, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 2) then
      if (typekind == ESMF_TYPEKIND_I4) then
        block
          integer(ESMF_KIND_I4), pointer  :: fptr(:,:)
          integer(ESMF_KIND_I4)           :: dataMask, value
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          isFlag = ESMF_InfoIsPresent(info, key="dataMask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (isFlag) then
            call ESMF_InfoGet(info, key="dataMask", value=dataMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
          dataMinSet = ESMF_InfoIsPresent(info, key="dataMin", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMinSet) then
            call ESMF_InfoGet(info, key="dataMin", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMin = real(value, ESMF_KIND_R8)
          endif
          dataMaxSet = ESMF_InfoIsPresent(info, key="dataMax", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMaxSet) then
            call ESMF_InfoGet(info, key="dataMax", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMax = real(value, ESMF_KIND_R8)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_I8) then
        block
          integer(ESMF_KIND_I8), pointer  :: fptr(:,:)
          integer(ESMF_KIND_I8)           :: dataMask, value
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          isFlag = ESMF_InfoIsPresent(info, key="dataMask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (isFlag) then
            call ESMF_InfoGet(info, key="dataMask", value=dataMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
          dataMinSet = ESMF_InfoIsPresent(info, key="dataMin", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMinSet) then
            call ESMF_InfoGet(info, key="dataMin", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMin = real(value, ESMF_KIND_R8)
          endif
          dataMaxSet = ESMF_InfoIsPresent(info, key="dataMax", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMaxSet) then
            call ESMF_InfoGet(info, key="dataMax", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMax = real(value, ESMF_KIND_R8)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_R4) then
        block
          real(ESMF_KIND_R4), pointer  :: fptr(:,:)
          real(ESMF_KIND_R4)           :: dataMask, value
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          isFlag = ESMF_InfoIsPresent(info, key="dataMask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (isFlag) then
            call ESMF_InfoGet(info, key="dataMask", value=dataMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
          dataMinSet = ESMF_InfoIsPresent(info, key="dataMin", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMinSet) then
            call ESMF_InfoGet(info, key="dataMin", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMin = real(value, ESMF_KIND_R8)
          endif
          dataMaxSet = ESMF_InfoIsPresent(info, key="dataMax", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMaxSet) then
            call ESMF_InfoGet(info, key="dataMax", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMax = real(value, ESMF_KIND_R8)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_R8) then
        block
          real(ESMF_KIND_R8), pointer  :: fptr(:,:)
          real(ESMF_KIND_R8)           :: dataMask, value
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          isFlag = ESMF_InfoIsPresent(info, key="dataMask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (isFlag) then
            call ESMF_InfoGet(info, key="dataMask", value=dataMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
          dataMinSet = ESMF_InfoIsPresent(info, key="dataMin", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMinSet) then
            call ESMF_InfoGet(info, key="dataMin", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMin = real(value, ESMF_KIND_R8)
          endif
          dataMaxSet = ESMF_InfoIsPresent(info, key="dataMax", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMaxSet) then
            call ESMF_InfoGet(info, key="dataMax", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMax = real(value, ESMF_KIND_R8)
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
          integer(ESMF_KIND_I4)           :: dataMask, value
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          isFlag = ESMF_InfoIsPresent(info, key="dataMask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (isFlag) then
            call ESMF_InfoGet(info, key="dataMask", value=dataMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
          dataMinSet = ESMF_InfoIsPresent(info, key="dataMin", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMinSet) then
            call ESMF_InfoGet(info, key="dataMin", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMin = real(value, ESMF_KIND_R8)
          endif
          dataMaxSet = ESMF_InfoIsPresent(info, key="dataMax", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMaxSet) then
            call ESMF_InfoGet(info, key="dataMax", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMax = real(value, ESMF_KIND_R8)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_I8) then
        block
          integer(ESMF_KIND_I8), pointer  :: fptr(:,:,:)
          integer(ESMF_KIND_I8)           :: dataMask, value
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          isFlag = ESMF_InfoIsPresent(info, key="dataMask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (isFlag) then
            call ESMF_InfoGet(info, key="dataMask", value=dataMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
          dataMinSet = ESMF_InfoIsPresent(info, key="dataMin", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMinSet) then
            call ESMF_InfoGet(info, key="dataMin", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMin = real(value, ESMF_KIND_R8)
          endif
          dataMaxSet = ESMF_InfoIsPresent(info, key="dataMax", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMaxSet) then
            call ESMF_InfoGet(info, key="dataMax", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMax = real(value, ESMF_KIND_R8)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_R4) then
        block
          real(ESMF_KIND_R4), pointer  :: fptr(:,:,:)
          real(ESMF_KIND_R4)           :: dataMask, value
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          isFlag = ESMF_InfoIsPresent(info, key="dataMask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (isFlag) then
            call ESMF_InfoGet(info, key="dataMask", value=dataMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
          dataMinSet = ESMF_InfoIsPresent(info, key="dataMin", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMinSet) then
            call ESMF_InfoGet(info, key="dataMin", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMin = real(value, ESMF_KIND_R8)
          endif
          dataMaxSet = ESMF_InfoIsPresent(info, key="dataMax", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMaxSet) then
            call ESMF_InfoGet(info, key="dataMax", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMax = real(value, ESMF_KIND_R8)
          endif
        end block
      else if (typekind == ESMF_TYPEKIND_R8) then
        block
          real(ESMF_KIND_R8), pointer  :: fptr(:,:,:)
          real(ESMF_KIND_R8)           :: dataMask, value
          call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          isFlag = ESMF_InfoIsPresent(info, key="dataMask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (isFlag) then
            call ESMF_InfoGet(info, key="dataMask", value=dataMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
          dataMinSet = ESMF_InfoIsPresent(info, key="dataMin", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMinSet) then
            call ESMF_InfoGet(info, key="dataMin", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMin = real(value, ESMF_KIND_R8)
          endif
          dataMaxSet = ESMF_InfoIsPresent(info, key="dataMax", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dataMaxSet) then
            call ESMF_InfoGet(info, key="dataMax", value=value, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            dataMax = real(value, ESMF_KIND_R8)
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

    if (dataMinSet) then
      if (statsMin < dataMin) statsOkay = .false.  ! found values below min
    endif

    if (dataMaxSet) then
      if (statsMax > dataMax) statsOkay = .false.  ! found values above max
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataAdvance(importState, exportItems, rc)
    type(ESMF_State)              :: importState
    type(ExportItem), allocatable :: exportItems(:)
    integer, intent(out)          :: rc

    type(ESMF_Field)              :: importField
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtrImportI4(:), fPtrExportI4(:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtrImportI8(:), fPtrExportI8(:)
    real(ESMF_KIND_R4), pointer, contiguous    :: fPtrImportR4(:), fPtrExportR4(:)
    real(ESMF_KIND_R8), pointer, contiguous    :: fPtrImportR8(:), fPtrExportR8(:)
    real(ESMF_KIND_R8)            :: value
    character(len=:), allocatable :: infix_expression, rpn_expression, token
    integer                       :: i, count, cur, top, depth
    type(ESMF_TYPEKIND_Flag)      :: tkImport, tkExport

    ! R8 workspace stack, last dimension is stack level
    real(ESMF_KIND_R8), allocatable :: stack(:,:)

    rc = ESMF_SUCCESS

    ! Early return if there is nothing to be done
    if (.not.allocated(exportItems)) return

    do i=1, size(exportItems)
      if (exportItems(i)%dataAdvance == "") cycle  ! NOOP

      ! Normalize the incoming infix string with single white space deliminators
      call normalize_infix(exportItems(i)%dataAdvance, infix_expression)

      ! Convert standard infix notation to reverse polish notation
      call infix_to_rpn(infix_expression, rpn_expression)

      ! Determine the required stack depth for RPN processing
      depth = compute_rpn_depth(rpn_expression)

      ! Setup export pointer
      call ESMF_FieldGet(exportItems(i)%field, typekind=tkExport, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (tkExport == ESMF_TYPEKIND_I4) then
        call access_data_i4(exportItems(i)%field, fPtrExportI4, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        count = size(fPtrExportI4)
      else if (tkExport == ESMF_TYPEKIND_I8) then
        call access_data_i8(exportItems(i)%field, fPtrExportI8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        count = size(fPtrExportI8)
      else if (tkExport == ESMF_TYPEKIND_R4) then
        call access_data_r4(exportItems(i)%field, fPtrExportR4, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        count = size(fPtrExportR4)
      else if (tkExport == ESMF_TYPEKIND_R8) then
        call access_data_r8(exportItems(i)%field, fPtrExportR8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        count = size(fPtrExportR8)
      else
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
          msg="DataAdvance() only supports I4, I8, R4, and R8.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      end if

      ! Setup workspace stack
      allocate(stack(count, depth))

      ! Evaluate RPN
      top = 0
      cur = 1
      do while (cur <= len_trim(rpn_expression))
        call get_next_token(rpn_expression, cur, token)
        if (token == "") exit

        select case (token)
        case ("+")
          stack(:,top-1) = stack(:,top-1) + stack(:,top)
          top = top - 1
        case ("-")
          stack(:,top-1) = stack(:,top-1) - stack(:,top)
          top = top - 1
        case ("*")
          stack(:,top-1) = stack(:,top-1) * stack(:,top)
          top = top - 1
        case ("/")
          stack(:,top-1) = stack(:,top-1) / stack(:,top)
          top = top - 1
        case default
          top = top + 1
          if (try_parse(token, value)) then
            ! Numerical value
            stack(:,top) = value
          else
            ! Variable Name: Pull from importState
            call ESMF_StateGet(importState, itemName=token, field=importField, &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldGet(importField, typekind=tkImport, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (tkImport == ESMF_TYPEKIND_I4) then
              call access_data_i4(importField, fPtrImportI4, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              stack(:,top) = fPtrImportI4
            else if (tkImport == ESMF_TYPEKIND_I8) then
              call access_data_i8(importField, fPtrImportI8, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              stack(:,top) = fPtrImportI8
            else if (tkImport == ESMF_TYPEKIND_R4) then
              call access_data_r4(importField, fPtrImportR4, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              stack(:,top) = fPtrImportR4
            else if (tkImport == ESMF_TYPEKIND_R8) then
              call access_data_r8(importField, fPtrImportR8, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              stack(:,top) = fPtrImportR8
            else
              call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
                msg="DataAdvance() only supports I4, I8, R4, and R8.", &
                line=__LINE__, file=__FILE__, rcToReturn=rc)
              return  ! bail out
            end if
          end if
        end select
      end do

      ! Stored final result in the export field
      if (tkExport == ESMF_TYPEKIND_I4) then
        fPtrExportI4 = stack(:,1)
      else if (tkExport == ESMF_TYPEKIND_I8) then
        fPtrExportI8 = stack(:,1)
      else if (tkExport == ESMF_TYPEKIND_R4) then
        fPtrExportR4 = stack(:,1)
      else if (tkExport == ESMF_TYPEKIND_R8) then
        fPtrExportR8 = stack(:,1)
      end if

      ! clean-up workspace stack
      deallocate(stack)

    end do

  end subroutine

  subroutine access_data_i4(field, fPtr, rc)
    ! Access field data as 1D contigous data array
    type(ESMF_Field),                           intent(in)   :: field
    integer(ESMF_KIND_I4), pointer, contiguous, intent(out)  :: fPtr(:)
    integer,                                    intent(out)  :: rc

    integer                                    :: rank
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr2D(:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr3D(:,:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr4D(:,:,:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr5D(:,:,:,:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr6D(:,:,:,:,:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr7D(:,:,:,:,:,:,:)

    call ESMF_FieldGet(field, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 1) then
      call ESMF_FieldGet(field, farrayPtr=fPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (rank == 2) then
      call ESMF_FieldGet(field, farrayPtr=fPtr2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr2D)) => fPtr2D(:, :)
    else if (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=fPtr3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr3D)) => fPtr3D(:, :,:)
    else if (rank == 4) then
      call ESMF_FieldGet(field, farrayPtr=fPtr4D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr4D)) => fPtr4D(:, :,:,:)
    else if (rank == 5) then
      call ESMF_FieldGet(field, farrayPtr=fPtr5D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr5D)) => fPtr5D(:, :,:,:,:)
    else if (rank == 6) then
      call ESMF_FieldGet(field, farrayPtr=fPtr6D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr6D)) => fPtr6D(:, :,:,:,:,:)
    else if (rank == 7) then
      call ESMF_FieldGet(field, farrayPtr=fPtr7D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr7D)) => fPtr7D(:, :,:,:,:,:,:)
    end if
  end subroutine

  subroutine access_data_i8(field, fPtr, rc)
    ! Access field data as 1D contigous data array
    type(ESMF_Field),                           intent(in)   :: field
    integer(ESMF_KIND_I8), pointer, contiguous, intent(out)  :: fPtr(:)
    integer,                                    intent(out)  :: rc

    integer                                    :: rank
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr2D(:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr3D(:,:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr4D(:,:,:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr5D(:,:,:,:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr6D(:,:,:,:,:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr7D(:,:,:,:,:,:,:)

    call ESMF_FieldGet(field, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 1) then
      call ESMF_FieldGet(field, farrayPtr=fPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (rank == 2) then
      call ESMF_FieldGet(field, farrayPtr=fPtr2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr2D)) => fPtr2D(:, :)
    else if (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=fPtr3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr3D)) => fPtr3D(:, :,:)
    else if (rank == 4) then
      call ESMF_FieldGet(field, farrayPtr=fPtr4D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr4D)) => fPtr4D(:, :,:,:)
    else if (rank == 5) then
      call ESMF_FieldGet(field, farrayPtr=fPtr5D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr5D)) => fPtr5D(:, :,:,:,:)
    else if (rank == 6) then
      call ESMF_FieldGet(field, farrayPtr=fPtr6D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr6D)) => fPtr6D(:, :,:,:,:,:)
    else if (rank == 7) then
      call ESMF_FieldGet(field, farrayPtr=fPtr7D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr7D)) => fPtr7D(:, :,:,:,:,:,:)
    end if
  end subroutine

  subroutine access_data_r4(field, fPtr, rc)
    ! Access field data as 1D contigous data array
    type(ESMF_Field),                        intent(in)   :: field
    real(ESMF_KIND_R4), pointer, contiguous, intent(out)  :: fPtr(:)
    integer,                                 intent(out)  :: rc

    integer                                 :: rank
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr2D(:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr3D(:,:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr4D(:,:,:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr5D(:,:,:,:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr6D(:,:,:,:,:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr7D(:,:,:,:,:,:,:)

    call ESMF_FieldGet(field, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 1) then
      call ESMF_FieldGet(field, farrayPtr=fPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (rank == 2) then
      call ESMF_FieldGet(field, farrayPtr=fPtr2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr2D)) => fPtr2D(:, :)
    else if (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=fPtr3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr3D)) => fPtr3D(:, :,:)
    else if (rank == 4) then
      call ESMF_FieldGet(field, farrayPtr=fPtr4D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr4D)) => fPtr4D(:, :,:,:)
    else if (rank == 5) then
      call ESMF_FieldGet(field, farrayPtr=fPtr5D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr5D)) => fPtr5D(:, :,:,:,:)
    else if (rank == 6) then
      call ESMF_FieldGet(field, farrayPtr=fPtr6D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr6D)) => fPtr6D(:, :,:,:,:,:)
    else if (rank == 7) then
      call ESMF_FieldGet(field, farrayPtr=fPtr7D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr7D)) => fPtr7D(:, :,:,:,:,:,:)
    end if
  end subroutine

  subroutine access_data_r8(field, fPtr, rc)
    ! Access field data as 1D contigous data array
    type(ESMF_Field),                        intent(in)   :: field
    real(ESMF_KIND_R8), pointer, contiguous, intent(out)  :: fPtr(:)
    integer,                                 intent(out)  :: rc

    integer                                 :: rank
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr2D(:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr3D(:,:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr4D(:,:,:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr5D(:,:,:,:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr6D(:,:,:,:,:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr7D(:,:,:,:,:,:,:)

    call ESMF_FieldGet(field, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 1) then
      call ESMF_FieldGet(field, farrayPtr=fPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (rank == 2) then
      call ESMF_FieldGet(field, farrayPtr=fPtr2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr2D)) => fPtr2D(:, :)
    else if (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=fPtr3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr3D)) => fPtr3D(:, :,:)
    else if (rank == 4) then
      call ESMF_FieldGet(field, farrayPtr=fPtr4D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr4D)) => fPtr4D(:, :,:,:)
    else if (rank == 5) then
      call ESMF_FieldGet(field, farrayPtr=fPtr5D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr5D)) => fPtr5D(:, :,:,:,:)
    else if (rank == 6) then
      call ESMF_FieldGet(field, farrayPtr=fPtr6D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr6D)) => fPtr6D(:, :,:,:,:,:)
    else if (rank == 7) then
      call ESMF_FieldGet(field, farrayPtr=fPtr7D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr7D)) => fPtr7D(:, :,:,:,:,:,:)
    end if
  end subroutine

  subroutine normalize_infix(input, output)
    ! Normalize the incoming infix string with single white space deliminators
    character(len=*),               intent(in)  :: input
    character(len=:), allocatable,  intent(out) :: output
    character                       :: c
    integer                         :: i
    logical                         :: needs_leading_zero, in_operand

    output = ""
    needs_leading_zero = .true.
    in_operand = .false.

    do i = 1, len_trim(input)
      c = input(i:i)  ! current character

      if (c == " ") then
        if (in_operand) then
          ! finish operand by adding trailing space
          output = output // " "
          in_operand = .false.
        end if
        cycle
      end if

      if (is_boundary(c)) then
        if (in_operand) then
          ! finish operand by adding trailing space
          output = output // " "
          in_operand = .false.
        end if
        ! Handle Unary Plus and Minus
        if ((c == "-" .or. c == "+") .and. needs_leading_zero) then
          ! insert the leading zero with space
          output = output // "0 "
        end if
        ! Add operator and trailing space
        output = output // c // " "
        ! Update the flag for next iteration according to operator
        needs_leading_zero = (c /= ")")
      else
        ! Inside operand
        in_operand = .true.
        output = output // c
        needs_leading_zero = .false.
      end if
    end do

    ! Remove trailing space
    output = trim(output)

  end subroutine

  logical function is_boundary(ch)
    ! Look for boundary character
    character, intent(in) :: ch
    select case (ch)
      case ("+", "-", "*", "/", "(", ")")
        is_boundary = .true.
      case default
        is_boundary = .false.
    end select
  end function

  subroutine infix_to_rpn(infix, rpn)
    ! Convert standard infix notation to reverse polish notation
    character(len=*), intent(in)               :: infix
    character(len=:), allocatable, intent(out) :: rpn
    character(len=128):: op_stack(20) ! Stack for operators
    integer           :: stack_ptr
    character(len=:), allocatable :: token
    integer           :: cur, i

    rpn = ""
    stack_ptr = 0
    cur = 1

    do while (cur <= len_trim(infix))
      ! Extract next space-separated token
      call get_next_token(infix, cur, token)
      if (len(token) == 0) exit

      if (is_operator(token)) then
        ! Handle Operators
        do while (stack_ptr > 0)
          if (op_stack(stack_ptr) /= "(" .and. &
              precedence(op_stack(stack_ptr)) >= precedence(token)) then
            rpn = rpn // trim(op_stack(stack_ptr)) // " "
            stack_ptr = stack_ptr - 1
          else
            exit
          end if
        end do
        stack_ptr = stack_ptr + 1
        op_stack(stack_ptr) = token

      else if (token == "(") then
        ! Handle Left Parenthesis
        stack_ptr = stack_ptr + 1
        op_stack(stack_ptr) = "("

      else if (token == ")") then
        ! Handle Right Parenthesis
        do while (stack_ptr > 0 .and. op_stack(stack_ptr) /= "(")
          rpn = rpn // trim(op_stack(stack_ptr)) // " "
          stack_ptr = stack_ptr - 1
        end do
        if (stack_ptr > 0) stack_ptr = stack_ptr - 1 ! Pop the "("

      else
        ! Number or field name - send straight to output
        rpn = rpn // token // " "
      end if
    end do

    ! Pop remaining operators from stack
    do i = stack_ptr, 1, -1
      rpn = rpn // trim(op_stack(i)) // " "
    end do

    ! Remove trailing space
    rpn = trim(rpn)

  end subroutine

  integer function compute_rpn_depth(rpn)
    ! Find high-water mark for a dryrun RPN execution
    character(len=*), intent(in)  :: rpn
    character(len=:), allocatable :: token
    integer                       :: cur, current_depth, max_depth

    max_depth = 0
    current_depth = 0
    cur = 1

    do while (cur <= len_trim(rpn))
      ! Extract next token from the RPN string
      call get_next_token(rpn, cur, token)
      if (token == "") exit

      if (is_operator(token)) then
        ! Binary operators (+, -, *, /) pop two operands and push one result.
        ! This results in a net change of -1 to the stack height.
        current_depth = current_depth - 1
      else
        ! Field names or numeric constants are pushed onto the stack.
        ! This results in a net change of +1.
        current_depth = current_depth + 1
      end if

      ! Update the "high-water mark"
      if (current_depth > max_depth) max_depth = current_depth
    end do

    compute_rpn_depth = max_depth

  end function

  subroutine get_next_token(str, cur, token)
    ! Look for the next token in a white space separated string
    character(len=*), intent(in)               :: str
    integer, intent(inout)                     :: cur
    character(len=:), allocatable, intent(out) :: token

    integer :: next_s

    if (cur > len(str)) then
      token = ""; return
    end if

    next_s = index(str(cur:), " ")

    if (next_s == 0) then
      token = str(cur:)
      cur = len(str) + 1
    else
      token = str(cur : cur + next_s - 2)
      cur = cur + next_s
    end if

  end subroutine

  logical function try_parse(token, value)
    ! Try to parse the token as a numerical constant
    character(len=*), intent(in)    :: token
    real(ESMF_KIND_R8), intent(out) :: value
    character(len=128)              :: buffer
    integer                         :: ios

    buffer = adjustl(token) ! use fixed size buffer for read
    read(buffer, *, iostat=ios) value
    try_parse = (ios == 0)
  end function try_parse

  integer function precedence(op)
    ! Operator precendece
    character(len=*), intent(in) :: op
    select case (trim(op))
      case ("+", "-") ; precedence = 2
      case ("*", "/") ; precedence = 3
      case default    ; precedence = 0
    end select
  end function

  logical function is_operator(token)
    ! Identify token as operator
    character(len=*), intent(in) :: token
    select case (trim(token))
      case ("+", "-", "*", "/") ; is_operator = .true.
      case default              ; is_operator = .false.
    end select
  end function

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
        deallocate(is%wrap%importItems(i)%dataValidate)
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
        deallocate(is%wrap%exportItems(i)%dataValidate)
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
