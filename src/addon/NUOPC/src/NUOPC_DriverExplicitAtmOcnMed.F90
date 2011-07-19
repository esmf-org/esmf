! $Id: NUOPC_DriverExplicitAtmOcnMed.F90,v 1.4 2011/07/19 22:16:53 theurich Exp $

#define FILENAME "src/addon/NUOPC/NUOPC_DriverExplicitAtmOcnMed.F90"

module NUOPC_DriverExplicitAtmOcnMed

  !-----------------------------------------------------------------------------
  ! Generic Driver Component for ATM, OCN, MED with explicit time stepping
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_DriverExplicit, only: &
    DrivEx_routine_SS             => routine_SetServices, &
    DrivEx_routine_DRS            => routine_DeallocateRunSequence, &
    DrivEx_routine_ARE            => routine_AddRunElement, &
    DrivEx_type_IS                => type_InternalState, &
    DrivEx_label_IS               => label_InternalState, &
    DrivEx_label_SetModelServices => label_SetModelServices, &
    DrivEx_label_SetModelCount    => label_SetModelCount, &
    DrivEx_label_Finalize         => label_Finalize

  implicit none
  
  private
  
  public routine_SetServices
  public type_InternalState, type_InternalStateStruct
  public label_InternalState, label_SetModelServices, label_Finalize
  
  character(*), parameter :: &
    label_InternalState = "DriverExplicitAtmOcnMed_InternalState"
  character(*), parameter :: &
    label_SetModelServices = "DriverExplicitAtmOcnMed_SetModelServices"
  character(*), parameter :: &
    label_Finalize = "DriverExplicitAtmOcnMed_Finalize"
  
  type type_InternalStateStruct
    type(ESMF_GridComp) :: atm
    type(ESMF_State)    :: atmIS, atmES
    type(ESMF_GridComp) :: ocn
    type(ESMF_State)    :: ocnIS, ocnES
    type(ESMF_GridComp) :: med
    type(ESMF_State)    :: medIS, medES
    type(ESMF_CplComp)  :: atm2med, ocn2med
    type(ESMF_CplComp)  :: med2atm, med2ocn
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! NUOPC_DriverExplicit registers the generic methods
    call DrivEx_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=DrivEx_label_SetModelCount, &
      userRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label=DrivEx_label_SetModelServices, &
      userRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label=DrivEx_label_Finalize, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(DrivEx_type_IS)  :: superIS

    rc = ESMF_SUCCESS
    
    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, DrivEx_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! set the modelCount for ATM-OCN-MED coupling
    superIS%wrap%modelCount = 3
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(DrivEx_type_IS)      :: superIS
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS
    
    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, DrivEx_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! map components and states for ATM-OCN pair coupling
    is%wrap%atm = superIS%wrap%modelComp(1)
    is%wrap%atmIS = superIS%wrap%modelIS(1)
    is%wrap%atmES = superIS%wrap%modelES(1)
    is%wrap%ocn = superIS%wrap%modelComp(2)
    is%wrap%ocnIS = superIS%wrap%modelIS(2)
    is%wrap%ocnES = superIS%wrap%modelES(2)
    is%wrap%med = superIS%wrap%modelComp(3)
    is%wrap%medIS = superIS%wrap%modelIS(3)
    is%wrap%medES = superIS%wrap%modelES(3)
    is%wrap%atm2med = superIS%wrap%connectorComp(1,3)
    is%wrap%ocn2med = superIS%wrap%connectorComp(2,3)
    is%wrap%med2atm = superIS%wrap%connectorComp(3,1)
    is%wrap%med2ocn = superIS%wrap%connectorComp(3,2)
    
    ! maybe too much? but maybe nice to have the component names specified?
    call ESMF_GridCompSet(is%wrap%atm, name="ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridCompSet(is%wrap%ocn, name="OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridCompSet(is%wrap%med, name="MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%atm2med, name="ATM2MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%ocn2med, name="OCN2MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%med2atm, name="MED2ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%med2ocn, name="MED2OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! The default RunSequence defined by the DriverExplicit is not suitable
    ! for the ATM-OCN-MED case. The default RunSequence must be overridden here.
    call DrivEx_routine_DRS(gcomp, rc=rc) ! deallocate default RunSequence
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! atm2med
    call DrivEx_routine_ARE(gcomp, i=1, j=3, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! ocn2med
    call DrivEx_routine_ARE(gcomp, i=2, j=3, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! med
    call DrivEx_routine_ARE(gcomp, i=3, j=0, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! med2atm
    call DrivEx_routine_ARE(gcomp, i=3, j=1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! med2ocn
    call DrivEx_routine_ARE(gcomp, i=3, j=2, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! atm
    call DrivEx_routine_ARE(gcomp, i=1, j=0, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! ocn
    call DrivEx_routine_ARE(gcomp, i=2, j=0, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! SPECIALIZE by calling into attached method to SetServices for modelComps
    call ESMF_MethodExecute(gcomp, label=label_SetModelServices, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine
    
  !-----------------------------------------------------------------------------
  
  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    logical                   :: existflag

    rc = ESMF_SUCCESS
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_Finalize, existflag=existflag, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine
      
end module
