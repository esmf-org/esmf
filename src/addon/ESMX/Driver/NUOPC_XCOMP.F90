module nuopc_xcomp

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
  type xcomp_field
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
    type(xcomp_field), pointer  :: nfld        => null()
  endtype xcomp_field

  type xcomp_state
    ! component information
    character(32) :: cname      = "XCOMP"
    integer       :: verbosity  =  0
    integer       :: diagnostic =  0
    integer       :: myid       = -1
    integer       :: outid      =  0
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
    character(len=ESMF_MAXSTR) :: field_file    = "esmxRun.config"
    type(xcomp_field), pointer :: imp_flds_head => null()
    type(xcomp_field), pointer :: exp_flds_head => null()
    type(xcomp_field), pointer :: imp_flds_tail => null()
    type(xcomp_field), pointer :: exp_flds_tail => null()
  endtype xcomp_state

  type xstate_wrap
    type(xcomp_state), pointer :: ptr
  endtype xstate_wrap

  contains

  !-----------------------------------------------------------------------------
  ! X Component Specialization
  !-----------------------------------------------------------------------------

  subroutine SetServices(xcomp, rc)
    ! arguments
    type(ESMF_GridComp)  :: xcomp
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xcomp_state), pointer :: xstate
    character(len=64)          :: value

    rc = ESMF_SUCCESS

    ! derive generic model phases
    call NUOPC_CompDerive(xcomp, modelSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! allocate memory for this internal state and set it in the component
    allocate(is%ptr, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='XCOMP: Memory allocation failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call ESMF_GridCompSetInternalState(xcomp, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xcomp, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! specialize model
    call NUOPC_CompSpecialize(xcomp, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xcomp, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xcomp, specLabel=label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xcomp, specLabel=label_CheckImport, &
       specRoutine=CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xcomp, specLabel=label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(xcomp, specLabel=label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for vm and local pet
    call ESMF_GridCompGet(xcomp, vm=xstate%vm, &
      localPet=xstate%myid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
  endsubroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine Advertise(xcomp, rc)
    ! arguments
    type(ESMF_GridComp)  :: xcomp
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(ESMF_State)           :: importState, exportState
    type(xcomp_state), pointer :: xstate
    type(xcomp_field), pointer :: xfield => null()

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xcomp, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! get configuration information
    call x_comp_get_attributes(xcomp, xstate, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for information
    call NUOPC_CompGet(xcomp, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query for importState and exportState
    call NUOPC_ModelGet(xcomp, importState=importState, &
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

  subroutine Realize(xcomp, rc)
    ! arguments
    type(ESMF_GridComp)  :: xcomp
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(ESMF_State)           :: importState, exportState
    type(xcomp_state), pointer :: xstate
    type(xcomp_field), pointer :: xfield

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xcomp, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xcomp, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query for importState and exportState
    call NUOPC_ModelGet(xcomp, importState=importState, &
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

  subroutine DataInitialize(xcomp, rc)
    ! arguments
    type(ESMF_GridComp)  :: xcomp
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xcomp_state), pointer :: xstate
    type(xcomp_field), pointer :: xfield
    type(ESMF_State)           :: importState
    type(ESMF_State)           :: exportState

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xcomp, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xcomp, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for import and export states
    call NUOPC_ModelGet(xcomp, importState=importState, &
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

    call NUOPC_CompAttributeSet(xcomp, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

  endsubroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine CheckImport(xcomp, rc)
    ! arguments
    type(ESMF_GridComp) :: xcomp
    integer,intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xcomp_state), pointer :: xstate
    type(ESMF_Clock)           :: modelClock
    type(ESMF_Time)            :: modelCurrTime
    type(ESMF_State)           :: importState
    logical                    :: allCurrTime

    rc = ESMF_SUCCESS

    ! query component for internal State
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xcomp, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xcomp, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query the component for its clock and import state
    call NUOPC_ModelGet(xcomp, modelClock=modelClock, &
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

  subroutine ModelAdvance(xcomp, rc)
    ! arguments
    type(ESMF_GridComp)  :: xcomp
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xcomp_state), pointer :: xstate
    type(xcomp_field), pointer :: xfield
    type(ESMF_Clock)           :: modelClock
    type(ESMF_State)           :: importState
    type(ESMF_State)           :: exportState
    character(len=160)         :: clockString
    integer                    :: errCount

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xcomp, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xcomp, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for import and export states
    call NUOPC_ModelGet(xcomp, modelClock=modelClock, &
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
      endif
      if (.not. xfield%okay) errCount = errCount + 1
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

  subroutine ModelFinalize(xcomp, rc)
    ! arguments
    type(ESMF_GridComp)  :: xcomp
    integer, intent(out) :: rc
    ! local variables
    integer                    :: stat
    type(xstate_wrap)          :: is
    type(xcomp_state), pointer :: xstate
    type(xcomp_field), pointer :: xfield

    rc = ESMF_SUCCESS

    ! query component for internal state
    nullify(is%ptr)
    call ESMF_GridCompGetInternalState(xcomp, is, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate => is%ptr
    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! query component for information
    call NUOPC_CompGet(xcomp, name=xstate%cname, &
      verbosity=xstate%verbosity, diagnostic=xstate%diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! destroy import fields
    do while (associated(xstate%imp_flds_head))
      xfield => xstate%imp_flds_head
      xstate%imp_flds_head => xfield%nfld
      call ESMF_FieldDestroy(xfield%efld, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      deallocate(xfield, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory deallocation failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      nullify(xfield)
    enddo
    xstate%imp_flds_tail => null()

    ! destroy export fields
    do while (associated(xstate%exp_flds_head))
      xfield => xstate%exp_flds_head
      xstate%exp_flds_head => xfield%nfld
      call ESMF_FieldDestroy(xfield%efld, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      deallocate(xfield, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory deallocation failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      nullify(xfield)
    enddo
    xstate%exp_flds_tail => null()

    ! destroy grid
    call ESMF_GridDestroy(xstate%grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    deallocate(is%ptr, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='XCOMP: Memory deallocation failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
  endsubroutine ModelFinalize

  !-----------------------------------------------------------------------------
  ! X Comp Internal Subroutines
  !-----------------------------------------------------------------------------

  subroutine x_comp_get_attributes(xcomp, xstate, rc)
    ! arguments
    type(ESMF_GridComp)                       :: xcomp
    type(xcomp_state), pointer, intent(inout) :: xstate
    integer, intent(out)                      :: rc
    ! local variables
    character(len=64) :: attval
    real              :: tmpReal

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! dimensions
    call ESMF_AttributeGet(xcomp, name="nx", value=attval, &
      defaultValue="64", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate%nx = ESMF_UtilString2Int(attval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_AttributeGet(xcomp, name="ny", value=attval, &
      defaultValue="32", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate%ny = ESMF_UtilString2Int(attval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_AttributeGet(xcomp, name="nz", value=attval, &
      defaultValue="4", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate%nz = ESMF_UtilString2Int(attval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! coordinate system
    call ESMF_AttributeGet(xcomp, name="coordSys", value=attval, &
      defaultValue="ESMF_COORDSYS_SPH_DEG", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    attval = ESMF_UtilStringUpperCase(attval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    select case (attval)
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
    call ESMF_AttributeGet(xcomp, name="minx", value=attval, &
      defaultValue="-126.0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    tmpReal = ESMF_UtilString2Real(attval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate%minx = real(tmpReal, ESMF_KIND_R8)
    call ESMF_AttributeGet(xcomp, name="maxx", value=attval, &
      defaultValue="-64.0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    tmpReal = ESMF_UtilString2Real(attval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate%maxx = real(tmpReal, ESMF_KIND_R8)
    call ESMF_AttributeGet(xcomp, name="miny", value=attval, &
      defaultValue="22", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    tmpReal = ESMF_UtilString2Real(attval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate%miny = real(tmpReal, ESMF_KIND_R8)
    call ESMF_AttributeGet(xcomp, name="maxy", value=attval, &
      defaultValue="50", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    tmpReal = ESMF_UtilString2Real(attval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate%maxy = real(tmpReal, ESMF_KIND_R8)

    ! field information filename
    call ESMF_AttributeGet(xcomp, name="field_file", value=attval, &
      defaultValue="esmxRun.config", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    xstate%field_file = attval

    call x_comp_read_fields(xstate, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

  endsubroutine x_comp_get_attributes

  !-----------------------------------------------------------------------------

  subroutine x_comp_read_fields(xstate, rc)
    ! arguments
    type(xcomp_state), pointer, intent(inout) :: xstate
    integer, intent(out)                      :: rc
    ! local variables
    integer                                     :: stat
    integer                                     :: i
    type(ESMF_Config)                           :: fieldsConfig
    type(NUOPC_FreeFormat)                      :: attrFF
    integer                                     :: lineCount
    integer                                     :: tokenCount
    character(NUOPC_FreeFormatLen), allocatable :: tokenList(:)
    type(xcomp_field), pointer                  :: xfield
    real                                        :: tmpReal

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! load field_file into fieldsConfig
    fieldsConfig = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_ConfigLoadFile(fieldsConfig, xstate%field_file, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! read import fields from config
    attrFF = NUOPC_FreeFormatCreate(fieldsConfig, &
      label=trim(xstate%cname)//"_import_fields::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_FreeFormatGet(attrFF, lineCount=lineCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    do i=1, lineCount
      nullify(xfield)
      call NUOPC_FreeFormatGetLine(attrFF, line=i, &
        tokenCount=tokenCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (tokenCount.ne.4) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg=trim(xstate%cname)//": Malformed import field "// &
            " 'STANDARD_NAME' 'DIMENSIONS' 'MIN_RANGE' 'MAX_RANGE'"// &
            " in file: "//trim(xstate%field_file), &
          line=__LINE__,file=__FILE__, rcToReturn=rc)
        return
      endif
      allocate(tokenList(tokenCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory allocation failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      call NUOPC_FreeFormatGetLine(attrFF, line=i, tokenList=tokenList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      allocate(xfield, stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory allocation failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      xfield%stdn = tokenList(1)
      xfield%fdim = ESMF_UtilString2Int(tokenList(2), rc=rc)
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
      tmpReal = ESMF_UtilString2Real(tokenList(3), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xfield%minv = real(tmpReal, ESMF_KIND_R8)
      tmpReal = ESMF_UtilString2Real(tokenList(4), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xfield%maxv = real(tmpReal, ESMF_KIND_R8)
      xfield%dflt = filv
      xfield%nfld => null()
      deallocate(tokenList)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory deallocation failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      if (.not. associated(xstate%imp_flds_head)) then
        xstate%imp_flds_head => xfield
        xstate%imp_flds_tail => xfield
      else
        xstate%imp_flds_tail%nfld => xfield
        xstate%imp_flds_tail => xfield
      endif
    enddo
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

    ! read export fields from config
    attrFF = NUOPC_FreeFormatCreate(fieldsConfig, &
      label=trim(xstate%cname)//"_export_fields::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_FreeFormatGet(attrFF, lineCount=lineCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    do i=1, lineCount
      nullify(xfield)
      call NUOPC_FreeFormatGetLine(attrFF, line=i, &
        tokenCount=tokenCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (tokenCount.ne.3) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg=trim(xstate%cname)//": Malformed export field "// &
            " 'STANDARD_NAME' 'DIMENSIONS' 'FILL_VALUE'"// &
            " in file: "//trim(xstate%field_file), &
          line=__LINE__,file=__FILE__, rcToReturn=rc)
        return
      endif
      allocate(tokenList(tokenCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory allocation failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      call NUOPC_FreeFormatGetLine(attrFF, line=i, tokenList=tokenList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      allocate(xfield, stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory allocation failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      xfield%stdn = tokenList(1)
      xfield%fdim = ESMF_UtilString2Int(tokenList(2), rc=rc)
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
      tmpReal = ESMF_UtilString2Real(tokenList(3), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      xfield%dflt = real(tmpReal, ESMF_KIND_R8)
      xfield%nfld => null()
      deallocate(tokenList)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(xstate%cname)//': Memory deallocation failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      if (.not. associated(xstate%exp_flds_head)) then
        xstate%exp_flds_head => xfield
        xstate%exp_flds_tail => xfield
      else
        xstate%exp_flds_tail%nfld => xfield
        xstate%exp_flds_tail => xfield
      endif
    enddo
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

    ! clean up
    call ESMF_ConfigDestroy(fieldsConfig, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

  endsubroutine x_comp_read_fields

  !-----------------------------------------------------------------------------

  subroutine x_comp_grid_diag(xstate, fileName, overwrite, status, timeslice, &
  iofmt, relaxedflag, rc)
    ! arguments
    type(xcomp_state), pointer, intent(in)           :: xstate
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
        msg='XCOMP: xstate has not been associated', &
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
    type(xcomp_state), pointer, intent(inout) :: xstate
    type(xcomp_field), pointer, intent(inout) :: xfield
    type(ESMF_State), intent(inout)  :: state
    integer, intent(out)             :: rc
    ! local variables

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    if (.not. associated(xfield)) then
      call ESMF_LogSetError(ESMF_RC_MEM_ALLOCATE, &
        msg=trim(xstate%cname)//": xfield error", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    if (associated(xfield%efld)) then
      call ESMF_LogSetError(ESMF_RC_MEM_ALLOCATE, &
        msg=trim(xstate%cname)//": ESMF_Field error - "//trim(xfield%stdn), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
    allocate(xfield%efld)

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
    type(xcomp_state), pointer, intent(in)    :: xstate
    type(xcomp_field), pointer, intent(inout) :: xfield
    integer, intent(out)                      :: rc
    ! local variables

    rc = ESMF_SUCCESS

    if (.not. associated(xstate)) then
      call ESMF_LogSetError(ESMF_RC_PTR_NOTALLOC, &
        msg='XCOMP: xstate has not been associated', &
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

endmodule nuopc_xcomp
