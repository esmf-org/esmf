module esmx_data

  !-----------------------------------------------------------------------------
  ! X Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices

  implicit none

  private

  public SetServices, SetVM

  ! parameters
  real(ESMF_KIND_R8), parameter :: filv = -1.0E34_ESMF_KIND_R8

  ! derived types
  type xdata_field
    character(len=64)           :: stdn        = "dummy"
    integer                     :: fdim        = 2
    real(ESMF_KIND_R8)          :: dflt        = filv
    logical                     :: rlze        = .false.
    real(ESMF_KIND_R8)          :: minv        = filv
    real(ESMF_KIND_R8)          :: maxv        = filv
    logical                     :: okay        = .true.
    real(ESMF_KIND_R8)          :: lmin(1)     = filv
    real(ESMF_KIND_R8)          :: gmin(1)     = filv
    real(ESMF_KIND_R8)          :: lmax(1)     = filv
    real(ESMF_KIND_R8)          :: gmax(1)     = filv
    real(ESMF_KIND_R8)          :: lsum(2)     = filv
    real(ESMF_KIND_R8)          :: gsum(2)     = filv
    real(ESMF_KIND_R8)          :: gavg        = filv
    type(ESMF_Field), pointer   :: efld        => null()
    real(ESMF_KIND_R8), pointer :: ptr2(:,:)   => null()
    real(ESMF_KIND_R8), pointer :: ptr3(:,:,:) => null()
    type(xdata_field), pointer  :: nfld        => null()
  endtype xdata_field

  type xdata_state
    ! component information
    character(32) :: cname       = "XDATA"
    integer       :: verbosity   =  0
    integer       :: diagnostic  =  0
    logical       :: write_final = .true.
    integer       :: myid        = -1
    integer       :: outid       =  0
    type(ESMF_VM) :: vm
    ! grid information
    integer                  :: nx = 64
    integer                  :: ny = 32
    integer                  :: nz = 4
    real(ESMF_KIND_R8)       :: minx = -126.000_ESMF_KIND_R8
    real(ESMF_KIND_R8)       :: maxx =  -64.000_ESMF_KIND_R8
    real(ESMF_KIND_R8)       :: miny =   22.000_ESMF_KIND_R8
    real(ESMF_KIND_R8)       :: maxy =   50.000_ESMF_KIND_R8
    type(ESMF_CoordSys_Flag) :: coordSys = ESMF_COORDSYS_SPH_DEG
    type(ESMF_Grid)          :: grid
    ! field information
    type(xdata_field), pointer :: imp_flds_head => null()
    type(xdata_field), pointer :: exp_flds_head => null()
    type(xdata_field), pointer :: imp_flds_tail => null()
    type(xdata_field), pointer :: exp_flds_tail => null()
  endtype xdata_state

  type xstate_wrap
    type(xdata_state), pointer :: ptr
  endtype xstate_wrap

  contains

  !-----------------------------------------------------------------------------
  ! X Component Specialization
  !-----------------------------------------------------------------------------

  subroutine SetServices(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xdata_state), pointer :: xstate
    character(len=64)          :: value
    type(ESMF_Config)          :: config
    type(ESMF_HConfig)         :: hconfig, hconfigNode
    character(80)              :: compLabel
    character(:), allocatable  :: badKey
    logical                    :: isFlag

    rc = ESMF_SUCCESS

    ! derive generic model phases
    call NUOPC_CompDerive(xdata, modelSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! allocate memory for this internal state and set it in the component
    allocate(is%ptr, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='XDATA: Memory allocation failed.', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return
    call ESMF_GridCompSetInternalState(xdata, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xdata, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! specialize model
    call NUOPC_CompSpecialize(xdata, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xdata, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xdata, specLabel=label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xdata, specLabel=label_CheckImport, &
       specRoutine=CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xdata, specLabel=label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xdata, specLabel=label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for vm and local pet
    call ESMF_GridCompGet(xdata, vm=xstate%vm, &
      localPet=xstate%myid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! validate config
    call ESMF_GridCompGet(xdata, name=compLabel, configIsPresent=isFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isFlag) then
      ! Config present, assert it is in the ESMX YAML format
      call ESMF_GridCompGet(xdata, config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,    file=__FILE__)) return  ! bail out
      call ESMF_ConfigGet(config, hconfig=hconfig, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      hconfigNode = ESMF_HConfigCreateAt(hconfig, keyString=compLabel, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! component responsibility to validate ESMX handled options here, and
      ! potentially locally handled options
      isFlag = ESMF_HConfigValidateMapKeys(hconfigNode, &
        vocabulary=["model        ", &  ! ESMX_Driver handled option
                    "petList      ", &  ! ESMX_Driver handled option
                    "ompNumThreads", &  ! ESMX_Driver handled option
                    "attributes   ", &  ! ESMX_Driver handled option
                    "output       ", &  ! ESMX_Data handled option
                    "geom         ", &  ! ESMX_Data handled option
                    "importFields ", &  ! ESMX_Data handled option
                    "exportFields "  &  ! ESMX_Data handled option
                   ], badKey=badKey, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (.not.isFlag) then
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
          msg="An invalid key was found in config under "//trim(compLabel)// &
            " (maybe a typo?): "//badKey, &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif

  endsubroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine Advertise(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(ESMF_State)           :: importState, exportState
    type(xdata_state), pointer :: xstate
    type(xdata_field), pointer :: xfield => null()

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xdata, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! get configuration information
    call x_comp_get_config(xdata, xstate, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for information
    call NUOPC_CompGet(xdata, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query for importState and exportState
    call NUOPC_ModelGet(xdata, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! advertise import fields
    xfield => xstate%imp_flds_head
    do while (associated(xfield))
      call NUOPC_Advertise(importState, xfield%stdn, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xfield => xfield%nfld
    enddo

    ! advertise export fields
    xfield => xstate%exp_flds_head
    do while (associated(xfield))
      call NUOPC_Advertise(exportState, xfield%stdn, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xfield => xfield%nfld
    enddo
  endsubroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine Realize(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(ESMF_State)           :: importState, exportState
    type(xdata_state), pointer :: xstate
    type(xdata_field), pointer :: xfield

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xdata, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xdata, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query for importState and exportState
    call NUOPC_ModelGet(xdata, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! create grid
    xstate%grid = ESMF_GridCreateNoPeriDimUfrm( &
      name=trim(xstate%cname)//"_grid", &
      minIndex=(/1, 1/), maxIndex=(/xstate%nx, xstate%ny/), &
      minCornerCoord=(/xstate%minx,xstate%miny/), &
      maxCornerCoord=(/xstate%maxx,xstate%maxy/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
      coordSys=xstate%coordSys, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! write grid to NetCDF file
    if (btest(xstate%diagnostic,16)) then
      call x_comp_grid_diag(xstate, trim(xstate%cname)//"_grid.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif

    ! realize import fields
    xfield => xstate%imp_flds_head
    do while (associated(xfield))
      call x_comp_realize_field(xstate, xfield, importState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xfield => xfield%nfld
    enddo

    ! realize export fields
    xfield => xstate%exp_flds_head
    do while (associated(xfield))
      call x_comp_realize_field(xstate, xfield, exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xfield => xfield%nfld
    enddo
  endsubroutine Realize

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xdata_state), pointer :: xstate
    type(xdata_field), pointer :: xfield
    type(ESMF_State)           :: importState
    type(ESMF_State)           :: exportState

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xdata, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xdata, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for import and export states
    call NUOPC_ModelGet(xdata, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! reset import fields
    xfield => xstate%imp_flds_head
    do while (associated(xfield))
      if (xfield%rlze) then
        call ESMF_FieldFill(xfield%efld, dataFillScheme="const", &
          const1=xfield%dflt, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif
      xfield => xfield%nfld
    enddo

    ! reset export fields
    xfield => xstate%exp_flds_head
    do while (associated(xfield))
      if (xfield%rlze) then
        call ESMF_FieldFill(xfield%efld, dataFillScheme="const", &
          const1=xfield%dflt, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call NUOPC_SetAttribute(xfield%efld, &
          name="Updated", value="true", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif
      xfield => xfield%nfld
    enddo

    call NUOPC_CompAttributeSet(xdata, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

  endsubroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine CheckImport(xdata, rc)
    ! arguments
    type(ESMF_GridComp) :: xdata
    integer,intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xdata_state), pointer :: xstate
    type(ESMF_Clock)           :: modelClock
    type(ESMF_Time)            :: modelCurrTime
    type(ESMF_State)           :: importState
    logical                    :: allCurrTime

    rc = ESMF_SUCCESS

    ! query component for internal State
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xdata, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xdata, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query the component for its clock and import state
    call NUOPC_ModelGet(xdata, modelClock=modelClock, &
      importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! get the stop time out of the clock
    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    allCurrTime = NUOPC_IsAtTime(importState, modelCurrTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (.NOT.allCurrTime) then
      call ESMF_LogWrite(trim(xstate%cname)//": "// &
        "NUOPC INCOMPATIBILITY DETECTED: Import Fields not at current time", &
        ESMF_LOGMSG_WARNING)
    endif
  endsubroutine CheckImport

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xdata_state), pointer :: xstate
    type(xdata_field), pointer :: xfield
    type(ESMF_Clock)           :: modelClock
    type(ESMF_State)           :: importState
    type(ESMF_State)           :: exportState
    character(len=160)         :: clockString
    integer                    :: errCount

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xdata, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xdata, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for import and export states
    call NUOPC_ModelGet(xdata, modelClock=modelClock, &
      importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_ClockPrint(modelClock, options="currTime", &
      unit=clockString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! write to standard out
    if (xstate%myid .eq. xstate%outid) then
      write(*,'(A,X,A)') trim(xstate%cname)//": Model Advance",trim(clockString)
    endif

    ! sum import data from all PETs
    xfield => xstate%imp_flds_head
    errCount = 0
    if (xstate%myid .eq. xstate%outid) then
      write(*,'(A)') trim(xstate%cname)//": Import Fields"
      write(*,'(A,X,A25,X,A9,3(X,A9),X,A4)') &
        trim(xstate%cname)//":", "FIELD", &
        "COUNT", "MEAN", &
        "MIN", "MAX", &
        "OKAY"
    endif
    do while (associated(xfield))
      call x_comp_check_field(xstate, xfield, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (xstate%myid .eq. xstate%outid) then
        write(*,'(A,X,A25,X,I9,3(X,E9.2),X,L4)') &
          trim(xstate%cname)//":", trim(xfield%stdn), &
          int(xfield%gsum(2)), xfield%gavg, &
          xfield%gmin(1), xfield%gmax(1), &
          xfield%okay
        if (.not. xfield%okay) errCount = errCount + 1
      endif
      xfield => xfield%nfld
    enddo

    ! sum export data from all PETs
    xfield => xstate%exp_flds_head
    if (xstate%myid .eq. xstate%outid) then
      write(*,'(A)') trim(xstate%cname)//": Export Fields"
      write(*,'(A,X,A25,X,A9,3(X,A9))') &
        trim(xstate%cname)//":", "FIELD", &
        "COUNT", "MEAN", &
        "MIN", "MAX"
    endif
    do while (associated(xfield))
      call x_comp_check_field(xstate, xfield, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (xstate%myid .eq. xstate%outid) then
        write(*,'(A,X,A25,X,I9,3(X,E9.2))') &
          trim(xstate%cname)//":", trim(xfield%stdn), &
          int(xfield%gsum(2)), xfield%gavg, &
          xfield%gmin(1), xfield%gmax(1)
      endif
      xfield => xfield%nfld
    enddo

    ! check for errors
    if (errCount .gt. 0) then
      write(*,'(A)') trim(xstate%cname)//": ERROR - check import fields"
      call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, &
        msg=trim(xstate%cname)//": import field error, check output", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

  endsubroutine ModelAdvance

  !-----------------------------------------------------------------------------

  subroutine ModelFinalize(xdata, rc)
    ! arguments
    type(ESMF_GridComp)  :: xdata
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xdata_state), pointer :: xstate
    type(xdata_field), pointer :: xfield
    type(ESMF_State)           :: importState
    type(ESMF_State)           :: exportState
    integer                    :: fc
    type(ESMF_Field), pointer  :: fl(:)
    type(ESMF_FieldBundle)     :: fb
    character(ESMF_MAXSTR)     :: fieldName

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xdata, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xdata, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! access import- and exportState
    call NUOPC_ModelGet(xdata, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! write final import and export states
    if (xstate%write_final) then
      call NUOPC_GetStateMemberCount(importState, fieldCount=fc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (fc .gt. 0) then
        nullify(fl)
        call NUOPC_GetStateMemberLists(importState, fieldList=fl, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        fb = ESMF_FieldBundleCreate(fieldList=fl, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_FieldBundleWrite(fb, &
          fileName=trim(xstate%cname)//"_final_import.nc", &
          overwrite=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_FieldBundleDestroy(fb, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        deallocate(fl)
      endif
      call NUOPC_GetStateMemberCount(exportState, fieldCount=fc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (fc .gt. 0) then
        nullify(fl)
        call NUOPC_GetStateMemberLists(exportState, fieldList=fl, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        fb = ESMF_FieldBundleCreate(fieldList=fl, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_FieldBundleWrite(fb, &
          fileName=trim(xstate%cname)//"_final_export.nc", &
          overwrite=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_FieldBundleDestroy(fb, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        deallocate(fl)
      endif
    endif

    ! remove import fields from importState and destroy
    do while (associated(xstate%imp_flds_head))
      xfield => xstate%imp_flds_head
      xstate%imp_flds_head => xfield%nfld
      call ESMF_FieldGet(xfield%efld, name=fieldName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_StateRemove(importState, (/fieldName/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_FieldDestroy(xfield%efld, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      deallocate(xfield, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory deallocation failed.', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      nullify(xfield)
    enddo
    xstate%imp_flds_tail => null()

    ! remove export fields from exportState and destroy
    do while (associated(xstate%exp_flds_head))
      xfield => xstate%exp_flds_head
      xstate%exp_flds_head => xfield%nfld
      call ESMF_FieldGet(xfield%efld, name=fieldName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_StateRemove(exportState, (/fieldName/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_FieldDestroy(xfield%efld, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      deallocate(xfield, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory deallocation failed.', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      nullify(xfield)
    enddo
    xstate%exp_flds_tail => null()

    ! destroy grid
    call ESMF_GridDestroy(xstate%grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    deallocate(is%ptr, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='XDATA: Memory deallocation failed.', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return
  endsubroutine ModelFinalize

  !-----------------------------------------------------------------------------
  ! X Comp Internal Subroutines
  !-----------------------------------------------------------------------------

  subroutine x_comp_get_config(xdata, xstate, rc)
    ! arguments
    type(ESMF_GridComp)                       :: xdata
    type(xdata_state), pointer, intent(inout) :: xstate
    integer, intent(out)                      :: rc
    ! local variables
    logical            :: isPresent
    integer            :: stat
    logical            :: check
    type(ESMF_Config)  :: config
    type(ESMF_HConfig) :: hconfig
    type(ESMF_HConfig) :: xdatacfg

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    call ESMF_GridCompGet(xdata, configIsPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      ! get config from component
      call ESMF_GridCompGet(xdata, config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      ! access hconfig
      call ESMF_ConfigGet(config, hconfig=hconfig, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      isPresent = ESMF_HConfigIsDefined(hconfig, &
        keyString=xstate%cname, rc=rc)
      if (isPresent) then
        ! access xdatacfg
        xdatacfg = ESMF_HConfigCreateAt(hconfig, &
          keyString=xstate%cname, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call x_comp_read_output(xdatacfg, xstate, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call x_comp_read_geom(xdatacfg, xstate, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call x_comp_read_fields(xdatacfg, xstate, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_HConfigDestroy(xdatacfg, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif ! xdatacfg
    endif ! config

  endsubroutine x_comp_get_config

  !-----------------------------------------------------------------------------

  subroutine x_comp_read_output(xdatacfg, xstate, rc)
    ! arguments
    type(ESMF_HConfig)                        :: xdatacfg
    type(xdata_state), pointer, intent(inout) :: xstate
    integer, intent(out)                      :: rc
    ! local variables
    logical            :: isPresent
    integer            :: stat
    logical            :: check
    type(ESMF_HConfig) :: outcfg
    character(:), allocatable  :: cfgval

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! read output configuration
    isPresent = ESMF_HConfigIsDefined(xdatacfg, &
      keyString="output", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      ! access output
      outcfg = ESMF_HConfigCreateAt(xdatacfg, &
        keyString="output", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      check = x_comp_hconfig_check(outcfg, &
        (/"write_final"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (.not. check) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg=trim(xstate%cname)//": valid options for output " //&
          "(write_final)", &
        line=__LINE__,file=__FILE__, rcToReturn=rc)
        return
      endif
      ! options
      xstate%write_final = x_comp_hconfig_logical(outcfg, "write_final", &
        defaultValue=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_HConfigDestroy(outcfg, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif ! outcfg

  endsubroutine x_comp_read_output

  !-----------------------------------------------------------------------------

  subroutine x_comp_read_geom(xdatacfg, xstate, rc)
    ! arguments
    type(ESMF_HConfig)                        :: xdatacfg
    type(xdata_state), pointer, intent(inout) :: xstate
    integer, intent(out)                      :: rc
    ! local variables
    logical            :: isPresent
    integer            :: stat
    logical            :: check
    type(ESMF_HConfig) :: geomcfg
    character(len=64)  :: cfgval

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! read geom configuration
    isPresent = ESMF_HConfigIsDefined(xdatacfg, &
      keyString="geom", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      ! access geom
      geomcfg = ESMF_HConfigCreateAt(xdatacfg, &
        keyString="geom", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      check = x_comp_hconfig_check(geomcfg, &
        (/"nx      ", &
          "ny      ", &
          "nz      ", &
          "coordSys", &
          "minx    ", &
          "maxx    ", &
          "miny    ", &
          "maxy    "/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (.not. check) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg=trim(xstate%cname)//": valid options for geom " //&
          "(nx, ny, nz, coordSys, minx, maxx, miny, maxy)", &
        line=__LINE__,file=__FILE__, rcToReturn=rc)
        return
      endif
      ! dimensions
      xstate%nx = x_comp_hconfig_i4(geomcfg, "nx", &
        defaultValue=64, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xstate%ny = x_comp_hconfig_i4(geomcfg, "ny", &
        defaultValue=32, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xstate%nz = x_comp_hconfig_i4(geomcfg, "nz", &
        defaultValue=4, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      ! coordinate system
      cfgval = x_comp_hconfig_str(geomcfg, "coordSys", &
        defaultValue="ESMF_COORDSYS_SPH_DEG", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      cfgval = ESMF_UtilStringUpperCase(cfgval, rc=rc)
      select case (cfgval)
        case ("ESMF_COORDSYS_CART")
          xstate%coordSys = ESMF_COORDSYS_CART
        case ("ESMF_COORDSYS_SPH_DEG")
          xstate%coordSys = ESMF_COORDSYS_SPH_DEG
        case ("ESMF_COORDSYS_SPH_RAD")
          xstate%coordSys = ESMF_COORDSYS_SPH_RAD
        case default
          call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
            msg=trim(xstate%cname)//': invalid value - coordSys', &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
      endselect
      ! coordinates
      xstate%minx = x_comp_hconfig_r8(geomcfg, "minx", &
        defaultValue=-126.0_ESMF_KIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xstate%maxx = x_comp_hconfig_r8(geomcfg, "maxx", &
        defaultValue=-64.0_ESMF_KIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xstate%miny = x_comp_hconfig_r8(geomcfg, "miny", &
        defaultValue=22.0_ESMF_KIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xstate%maxy = x_comp_hconfig_r8(geomcfg, "maxy", &
        defaultValue=50.0_ESMF_KIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_HConfigDestroy(geomcfg, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif ! geomcfg

  endsubroutine x_comp_read_geom

  !-----------------------------------------------------------------------------

  subroutine x_comp_read_fields(xdatacfg, xstate, rc)
    ! arguments
    type(ESMF_HConfig)                        :: xdatacfg
    type(xdata_state), pointer, intent(inout) :: xstate
    integer, intent(out)                      :: rc
    ! local variables
    logical                    :: isPresent
    integer                    :: stat
    logical                    :: check
    type(ESMF_HConfig)         :: flistcfg
    type(ESMF_HConfig)         :: fieldcfg
    type(ESMF_HConfigIter)     :: flistcur
    type(ESMF_HConfigIter)     :: flistbeg
    type(ESMF_HConfigIter)     :: flistend
    character(:), allocatable  :: fname
    type(xdata_field), pointer :: xfield

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! read import field configuration
    isPresent = ESMF_HConfigIsDefined(xdatacfg, &
      keyString="importFields", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      ! access flistcfg(import)
      flistcfg = ESMF_HConfigCreateAt(xdatacfg, &
        keyString="importFields", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      flistbeg = ESMF_HConfigIterBegin(flistcfg, rc=rc)
      flistend = ESMF_HConfigIterEnd(flistcfg, rc=rc)
      flistcur = flistbeg
      do while (ESMF_HConfigIterLoop(flistcur, flistbeg, flistend, rc=rc))
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        fname = ESMF_HConfigAsStringMapKey(flistcur, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        ! access fieldcfg
        fieldcfg = ESMF_HConfigCreateAt(flistcfg, keyString=fname, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        check = x_comp_hconfig_check(fieldcfg, &
          (/"dim", "min", "max"/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        if (.not. check) then
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=trim(xstate%cname)//": ("//fname//")" //&
            " valid options for importFields (dim, min, max)", &
          line=__LINE__,file=__FILE__, rcToReturn=rc)
          return
        endif
        nullify(xfield)
        allocate(xfield, stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg=trim(xstate%cname)//': Memory allocation failed.', &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return
        xfield%stdn = fname
        xfield%fdim = x_comp_hconfig_i4(fieldcfg, "dim", &
          defaultValue=2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        xfield%okay = .false.
        xfield%lsum = (/filv, 0.0_ESMF_KIND_R8/)
        xfield%lmin = filv
        xfield%lmax = filv
        xfield%gsum = (/filv, 0.0_ESMF_KIND_R8/)
        xfield%gmin = filv
        xfield%gmax = filv
        xfield%gavg = filv
        xfield%minv = x_comp_hconfig_r8(fieldcfg, "min", &
          defaultValue=0.0_ESMF_KIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        xfield%maxv = x_comp_hconfig_r8(fieldcfg, "max", &
          defaultValue=0.0_ESMF_KIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        xfield%dflt = filv
        xfield%nfld => null()
        if (.not. associated(xstate%imp_flds_head)) then
          xstate%imp_flds_head => xfield
          xstate%imp_flds_tail => xfield
        else
          xstate%imp_flds_tail%nfld => xfield
          xstate%imp_flds_tail => xfield
        endif
        call ESMF_HConfigDestroy(fieldcfg, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      enddo ! fieldcfg
      call ESMF_HConfigDestroy(flistcfg, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif ! flistcfg

    ! read export field configuration
    isPresent = ESMF_HConfigIsDefined(xdatacfg, &
      keyString="exportFields", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      ! access flistcfg(export)
      flistcfg = ESMF_HConfigCreateAt(xdatacfg, &
        keyString="exportFields", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      flistbeg = ESMF_HConfigIterBegin(flistcfg, rc=rc)
      flistend = ESMF_HConfigIterEnd(flistcfg, rc=rc)
      flistcur = flistbeg
      do while (ESMF_HConfigIterLoop(flistcur, flistbeg, flistend, rc=rc))
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        fname = ESMF_HConfigAsStringMapKey(flistcur, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        ! access fieldcfg
        fieldcfg = ESMF_HConfigCreateAt(flistcfg, keyString=fname, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        check = x_comp_hconfig_check(fieldcfg, &
          (/"dim", "val"/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        if (.not. check) then
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg=trim(xstate%cname)//": ("//fname//")" //&
            " valid options for exportFields (dim, val)", &
          line=__LINE__,file=__FILE__, rcToReturn=rc)
          return
        endif
        nullify(xfield)
        allocate(xfield, stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg=trim(xstate%cname)//': Memory allocation failed.', &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return
        xfield%stdn = fname
        xfield%fdim = x_comp_hconfig_i4(fieldcfg, "dim", &
          defaultValue=2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        xfield%okay = .false.
        xfield%lsum = (/filv, 0.0_ESMF_KIND_R8/)
        xfield%lmin = filv
        xfield%lmax = filv
        xfield%gsum = (/filv, 0.0_ESMF_KIND_R8/)
        xfield%gmin = filv
        xfield%gmax = filv
        xfield%gavg = filv
        xfield%minv = 0.0_ESMF_KIND_R8
        xfield%maxv = 0.0_ESMF_KIND_R8
        xfield%dflt = x_comp_hconfig_r8(fieldcfg, "val", &
          defaultValue=0.0_ESMF_KIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        xfield%nfld => null()
        if (.not. associated(xstate%exp_flds_head)) then
          xstate%exp_flds_head => xfield
          xstate%exp_flds_tail => xfield
        else
          xstate%exp_flds_tail%nfld => xfield
          xstate%exp_flds_tail => xfield
        endif
        call ESMF_HConfigDestroy(fieldcfg, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      enddo ! fieldcfg
      call ESMF_HConfigDestroy(flistcfg, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif ! flistcfg

  endsubroutine x_comp_read_fields

  !-----------------------------------------------------------------------------

  subroutine x_comp_grid_diag(xstate, fileName, overwrite, status, timeslice, &
  iofmt, relaxedflag, rc)
    ! arguments
    type(xdata_state), pointer, intent(in)           :: xstate
    character(len=*), intent(in), optional           :: fileName
    logical, intent(in), optional                    :: overwrite
    type(ESMF_FileStatus_Flag), intent(in), optional :: status
    integer, intent(in), optional                    :: timeslice
    type(ESMF_IOFmt_Flag), intent(in), optional      :: iofmt
    logical, intent(in), optional                    :: relaxedflag
    integer, intent(out)                             :: rc
    ! local variables
    logical                 :: ioCapable
    logical                 :: doItFlag
    character(len=64)       :: lfileName
    character(len=64)       :: gridName
    type(ESMF_Array)        :: array
    type(ESMF_ArrayBundle)  :: arraybundle
    logical                 :: isPresent
    integer                 :: dimCount
    integer                 :: dimIndex
    integer,allocatable     :: coordDimCount(:)
    integer                 :: coordDimMax
    integer                 :: stat
    logical                 :: hasCorners

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ioCapable = (ESMF_IO_PIO_PRESENT .and. &
      (ESMF_IO_NETCDF_PRESENT .or. ESMF_IO_PNETCDF_PRESENT))

    doItFlag = .true. ! default
    if (present(relaxedFlag)) then
      doItFlag = .not.relaxedflag .or. (relaxedflag.and.ioCapable)
    endif

    if (doItFlag) then

      if (present(fileName)) then
        lfileName = trim(fileName)
      else
        call ESMF_GridGet(xstate%grid, name=gridName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        lfileName = trim(gridName)//".nc"
      endif

      arraybundle = ESMF_ArrayBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      ! -- centers --

      call ESMF_GridGetCoord(xstate%grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        isPresent=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (isPresent) then
        call ESMF_GridGetCoord(xstate%grid, coordDim=1, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArraySet(array, name="lon_center", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_GridGetCoord(xstate%grid, coordDim=2, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArraySet(array, name="lat_center", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif

      ! -- corners --

      call ESMF_GridGetCoord(xstate%grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        isPresent=hasCorners, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (hasCorners) then
        call ESMF_GridGetCoord(xstate%grid, coordDim=1, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        if (.not. ESMF_LogFoundError(rcToCheck=rc)) then
          call ESMF_ArraySet(array, name="lon_corner", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
          call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
        endif
        call ESMF_GridGetCoord(xstate%grid, coordDim=2, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        if (.not. ESMF_LogFoundError(rcToCheck=rc)) then
          call ESMF_ArraySet(array, name="lat_corner", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
          call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
        endif
      endif

      ! -- mask --

      call ESMF_GridGetItem(xstate%grid, itemflag=ESMF_GRIDITEM_MASK, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (isPresent) then
        call ESMF_GridGetItem(xstate%grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArraySet(array, name="mask", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif

      ! -- area --

      call ESMF_GridGetItem(xstate%grid, itemflag=ESMF_GRIDITEM_AREA, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (isPresent) then
        call ESMF_GridGetItem(xstate%grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArraySet(array, name="area", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif

      call ESMF_ArrayBundleWrite(arraybundle, &
        fileName=trim(lfileName),rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      call ESMF_ArrayBundleDestroy(arraybundle,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif
  endsubroutine x_comp_grid_diag

  !-----------------------------------------------------------------------------

  subroutine x_comp_realize_field(xstate, xfield, state, rc)
    ! arguments
    type(xdata_state), pointer, intent(inout) :: xstate
    type(xdata_field), pointer, intent(inout) :: xfield
    type(ESMF_State), intent(inout)  :: state
    integer, intent(out)             :: rc
    ! local variables
    integer :: stat

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    if (.not. associated(xfield)) then
      call ESMF_LogSetError(ESMF_RC_MEM_ALLOCATE, &
        msg=trim(xstate%cname)//": xfield error", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return
    endif

    if (associated(xfield%efld)) then
      call ESMF_LogSetError(ESMF_RC_MEM_ALLOCATE, &
        msg=trim(xstate%cname)//": ESMF_Field error - "//trim(xfield%stdn), &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return
    endif
    allocate(xfield%efld, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=trim(xstate%cname)//': Memory allocation failed.', &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    if (xfield%fdim .eq. 3) then
      xfield%efld = ESMF_FieldCreate(name=trim(xfield%stdn), grid=xstate%grid, &
        typekind=ESMF_TYPEKIND_R8, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/xstate%nz/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_FieldGet(xfield%efld, farrayPtr=xfield%ptr3, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    elseif (xfield%fdim .eq. 2) then
      xfield%efld = ESMF_FieldCreate(name=trim(xfield%stdn), grid=xstate%grid, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_FieldGet(xfield%efld, farrayPtr=xfield%ptr2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg=trim(xstate%cname)//": field dimension - "//trim(xfield%stdn), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    call NUOPC_Realize(state, field=xfield%efld, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(xfield%efld, dataFillScheme="const", &
      const1=0.0_ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xfield%rlze = .true.
  endsubroutine x_comp_realize_field

  !-----------------------------------------------------------------------------

  subroutine x_comp_check_field(xstate, xfield, rc)
    ! arguments
    type(xdata_state), pointer, intent(in)    :: xstate
    type(xdata_field), pointer, intent(inout) :: xfield
    integer, intent(out)                      :: rc
    ! local variables

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XDATA: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    if (xfield%rlze) then
      if(xfield%fdim .eq. 3) then
        xfield%lsum(1)=sum(xfield%ptr3,xfield%ptr3.ne.filv)
        xfield%lsum(2)=count(xfield%ptr3.ne.filv)
        xfield%lmin(1)=minval(xfield%ptr3,xfield%ptr3.ne.filv)
        xfield%lmax(1)=maxval(xfield%ptr3,xfield%ptr3.ne.filv)
      elseif(xfield%fdim .eq. 2) then
        xfield%lsum(1)=sum(xfield%ptr2,xfield%ptr2.ne.filv)
        xfield%lsum(2)=count(xfield%ptr2.ne.filv)
        xfield%lmin(1)=minval(xfield%ptr2,xfield%ptr2.ne.filv)
        xfield%lmax(1)=maxval(xfield%ptr2,xfield%ptr2.ne.filv)
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg=trim(xstate%cname)//": field dimension - "//trim(xfield%stdn), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
      call ESMF_VMReduce(vm=xstate%vm, sendData=xfield%lsum, &
        recvData=xfield%gsum, count=2, &
        reduceflag=ESMF_REDUCE_SUM, rootPet=xstate%outid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_VMReduce(vm=xstate%vm, sendData=xfield%lmin, &
        recvData=xfield%gmin, count=1, &
        reduceflag=ESMF_REDUCE_MIN, rootPet=xstate%outid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_VMReduce(vm=xstate%vm, sendData=xfield%lmax, &
        recvData=xfield%gmax, count=1, &
        reduceflag=ESMF_REDUCE_MAX, rootPet=xstate%outid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (xstate%myid .eq. xstate%outid) then
        ! calculate average
        if(xfield%gsum(2) .lt. 1) then
          xfield%gavg = 0.0_ESMF_KIND_R8
        else
          xfield%gavg = xfield%gsum(1) / xfield%gsum(2)
        endif
        ! check
        if((xfield%gmin(1) .lt. xfield%minv) .or. &
           (xfield%gmax(1) .gt. xfield%maxv)) then
          xfield%okay = .false.
        else
          xfield%okay = .true.
        endif
      endif
    else
      xfield%gsum = (/filv, 0.0_ESMF_KIND_R8/)
      xfield%gmin = filv
      xfield%gmax = filv
      xfield%gavg = 0.0_ESMF_KIND_R8
      xfield%okay = .false.
    endif
  endsubroutine x_comp_check_field

  !-----------------------------------------------------------------------------

  function x_comp_hconfig_check(hconfig, options, caseinsensitive, rc)
    ! return value
    logical :: x_comp_hconfig_check
    ! arguments
    type(ESMF_HConfig), intent(in) :: hconfig
    character(*), intent(in)       :: options(:)
    logical, intent(in), optional  :: caseinsensitive
    integer, intent(out)           :: rc
    ! local variables
    integer                             :: i
    integer                             :: stat
    logical                             :: local_ci
    type(ESMF_HConfigIter)              :: listcur, listbeg, listend
    character(ESMF_MAXSTR)              :: key
    character(ESMF_MAXSTR), allocatable :: options_ci(:)

    rc = ESMF_SUCCESS

    x_comp_hconfig_check = ESMF_HConfigIsDefined(hconfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (.not. x_comp_hconfig_check) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="XDATA: HConfig is not Map", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    if (present(caseinsensitive)) then
      local_ci = caseinsensitive
    else
      local_ci = .false.
    endif

    listbeg = ESMF_HConfigIterBegin(hconfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    listend = ESMF_HConfigIterEnd(hconfig, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    listcur = listbeg
    if (local_ci) then
      allocate(options_ci(size(options)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg='XDATA: Memory allocation failed.', &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      do i=1, size(options)
        options_ci(i) = ESMF_UtilStringLowerCase(options(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      enddo
      do while (ESMF_HConfigIterLoop(listcur, listbeg, listend, rc=rc))
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        key = ESMF_HConfigAsStringMapKey(listcur, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        key = ESMF_UtilStringLowerCase(key, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        if (.not. any(key .eq. options_ci)) then
          x_comp_hconfig_check = .false.
          return
        endif
      enddo
      deallocate(options_ci)
    else
      do while (ESMF_HConfigIterLoop(listcur, listbeg, listend, rc=rc))
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        key = ESMF_HConfigAsStringMapKey(listcur, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        if (.not. any(key .eq. options)) then
          x_comp_hconfig_check = .false.
          return
        endif
      enddo
      x_comp_hconfig_check = .true.
    endif

  endfunction x_comp_hconfig_check

  !-----------------------------------------------------------------------------

  function x_comp_hconfig_i4(hconfig, key, defaultValue, rc)
    ! return value
    integer(ESMF_KIND_I4) :: x_comp_hconfig_i4
    ! arguments
    type(ESMF_HConfig), intent(in)              :: hconfig
    character(*), intent(in)                    :: key
    integer(ESMF_KIND_I4), intent(in), optional :: defaultValue
    integer, intent(out)                        :: rc
    ! local variables
    logical :: isPresent
    logical :: check

    rc = ESMF_SUCCESS
    x_comp_hconfig_i4 = 0

    isPresent = ESMF_HConfigIsDefined(hconfig, keyString=key, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    if (isPresent) then
      x_comp_hconfig_i4 = ESMF_HConfigAsI4(hconfig, keyString=key, &
        asOkay=check, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (.not.check) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="XDATA: Value cannot be converted to I4 - "//trim(key), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    elseif (present(defaultValue)) then
      x_comp_hconfig_i4 = defaultValue
    else
      call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
        msg="XDATA: Key not found - "//trim(key), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
  endfunction x_comp_hconfig_i4

  !-----------------------------------------------------------------------------

  function x_comp_hconfig_r8(hconfig, key, defaultValue, rc)
    ! return value
    real(ESMF_KIND_R8) :: x_comp_hconfig_r8
    ! arguments
    type(ESMF_HConfig), intent(in)           :: hconfig
    character(*), intent(in)                 :: key
    real(ESMF_KIND_R8), intent(in), optional :: defaultValue
    integer, intent(out)                     :: rc
    ! local variables
    logical :: isPresent
    logical :: check

    rc = ESMF_SUCCESS
    x_comp_hconfig_r8 = 0.0_ESMF_KIND_R8

    isPresent = ESMF_HConfigIsDefined(hconfig, keyString=key, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    if (isPresent) then
      x_comp_hconfig_r8 = ESMF_HConfigAsR8(hconfig, keyString=key, &
        asOkay=check, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (.not.check) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="XDATA: Value cannot be converted to R8 - "//trim(key), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    elseif (present(defaultValue)) then
      x_comp_hconfig_r8 = defaultValue
    else
      call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
        msg="XDATA: Key not found - "//trim(key), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
  endfunction x_comp_hconfig_r8

  !-----------------------------------------------------------------------------

  function x_comp_hconfig_str(hconfig, key, defaultValue, rc)
    ! return value
    character(:), allocatable :: x_comp_hconfig_str
    ! arguments
    type(ESMF_HConfig), intent(in)     :: hconfig
    character(*), intent(in)           :: key
    character(*), intent(in), optional :: defaultValue
    integer, intent(out)               :: rc
    ! local variables
    logical :: isPresent
    logical :: check

    rc = ESMF_SUCCESS
    x_comp_hconfig_str = ' '

    isPresent = ESMF_HConfigIsDefined(hconfig, keyString=key, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    if (isPresent) then
      x_comp_hconfig_str = ESMF_HConfigAsString(hconfig, keyString=key, &
        asOkay=check, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (.not.check) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="XDATA: Value cannot be converted to String - "//trim(key), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    elseif (present(defaultValue)) then
      x_comp_hconfig_str = defaultValue
    else
      call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
        msg="XDATA: Key not found - "//trim(key), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
  endfunction x_comp_hconfig_str

  !-----------------------------------------------------------------------------

  function x_comp_hconfig_logical(hconfig, key, defaultValue, rc)
    ! return value
    logical :: x_comp_hconfig_logical
    ! arguments
    type(ESMF_HConfig), intent(in) :: hconfig
    character(*), intent(in)       :: key
    logical, intent(in), optional  :: defaultValue
    integer, intent(out)           :: rc
    ! local variables
    logical :: isPresent
    logical :: check

    rc = ESMF_SUCCESS
    x_comp_hconfig_logical = .false.

    isPresent = ESMF_HConfigIsDefined(hconfig, keyString=key, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    if (isPresent) then
      x_comp_hconfig_logical = ESMF_HConfigAsLogical(hconfig, keyString=key, &
        asOkay=check, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (.not.check) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="XDATA: Value cannot be converted to Logical - "//trim(key), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    elseif (present(defaultValue)) then
      x_comp_hconfig_logical = defaultValue
    else
      call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
        msg="XDATA: Key not found - "//trim(key), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
  endfunction x_comp_hconfig_logical

  !-----------------------------------------------------------------------------


endmodule esmx_data
