! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2014, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_DriverAtmOcnMed.F90"
!==============================================================================

module NUOPC_DriverAtmOcnMed

  !-----------------------------------------------------------------------------
  ! Generic Driver Component for ATM, OCN, MED with default explicit time stepping
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, only: &
    Driver_routine_SS             => routine_SetServices, &
    Driver_type_IS                => type_InternalState, &
    Driver_label_IS               => label_InternalState, &
    Driver_label_SetModelCount    => label_SetModelCount, &
    Driver_label_SetModelPetLists => label_SetModelPetLists, &
    Driver_label_SetModelServices => label_SetModelServices, &
    Driver_label_Finalize         => label_Finalize

  implicit none
  
  private
  
  public routine_SetServices
  public type_InternalState, type_InternalStateStruct
  public label_InternalState, label_SetModelPetLists
  public label_SetModelServices, label_Finalize
  
  character(*), parameter :: &
    label_InternalState = "DriverAtmOcnMed_InternalState"
  character(*), parameter :: &
    label_SetModelPetLists = "DriverAtmOcnMed_SetModelPetLists"
  character(*), parameter :: &
    label_SetModelServices = "DriverAtmOcnMed_SetModelServices"
  character(*), parameter :: &
    label_Finalize = "DriverAtmOcnMed_Finalize"
  
  type type_InternalStateStruct
    integer, pointer    :: atmPetList(:)
    integer, pointer    :: ocnPetList(:)
    integer, pointer    :: medPetList(:)
    type(ESMF_GridComp) :: atm
    type(ESMF_GridComp) :: ocn
    type(ESMF_GridComp) :: med
    type(ESMF_State)    :: atmIS, atmES
    type(ESMF_State)    :: ocnIS, ocnES
    type(ESMF_State)    :: medIS, medES
    integer, pointer    :: atm2medPetList(:)
    integer, pointer    :: ocn2medPetList(:)
    integer, pointer    :: med2atmPetList(:)
    integer, pointer    :: med2ocnPetList(:)
    type(ESMF_CplComp)  :: atm2med, ocn2med
    type(ESMF_CplComp)  :: med2atm, med2ocn
    type(NUOPC_RunSequence), pointer  :: runSeq(:)
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
    
    ! local variables
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! NUOPC_Driver registers the generic methods
    call Driver_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelCount, &
      userRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelPetLists, &
      userRoutine=SetModelPetLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelServices, &
      userRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_Finalize, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(Driver_type_IS)  :: superIS
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! set the modelCount for ATM-OCN-MED coupling
    superIS%wrap%modelCount = 3
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelPetLists(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    type(Driver_type_IS)      :: superIS
    logical                   :: existflag
    character(ESMF_MAXSTR)    :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! nullify the petLists
    nullify(is%wrap%atmPetList)
    nullify(is%wrap%ocnPetList)
    nullify(is%wrap%medPetList)
    nullify(is%wrap%atm2medPetList)
    nullify(is%wrap%ocn2medPetList)
    nullify(is%wrap%med2atmPetList)
    nullify(is%wrap%med2ocnPetList)
    
    ! SPECIALIZE by calling into optional attached method to set modelPetLists
    call ESMF_MethodExecute(gcomp, label=label_SetModelPetLists, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    if (existflag) then
      ! query Component for super internal State
      nullify(superIS%wrap)
      call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
      ! set the petLists
      superIS%wrap%modelPetLists(1)%petList => is%wrap%atmPetList
      superIS%wrap%modelPetLists(2)%petList => is%wrap%ocnPetList
      superIS%wrap%modelPetLists(3)%petList => is%wrap%medPetList
      superIS%wrap%connectorPetLists(1,3)%petList => is%wrap%atm2medPetList
      superIS%wrap%connectorPetLists(2,3)%petList => is%wrap%ocn2medPetList
      superIS%wrap%connectorPetLists(3,1)%petList => is%wrap%med2atmPetList
      superIS%wrap%connectorPetLists(3,2)%petList => is%wrap%med2ocnPetList
    endif
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(Driver_type_IS)      :: superIS
    type(type_InternalState)  :: is
    character(ESMF_MAXSTR)    :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
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
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_GridCompSet(is%wrap%ocn, name="OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_GridCompSet(is%wrap%med, name="MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_CplCompSet(is%wrap%atm2med, name="ATM2MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_CplCompSet(is%wrap%ocn2med, name="OCN2MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_CplCompSet(is%wrap%med2atm, name="MED2ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    call ESMF_CplCompSet(is%wrap%med2ocn, name="MED2OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! The default run sequence defined by the generic Driver Component is not 
    ! suitable for the ATM-OCN-MED case. The default RunSeq must be overwritten.
    call NUOPC_RunSequenceDeallocate(superIS%wrap%runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! add a single run sequence elements
    call NUOPC_RunSequenceAdd(superIS%wrap%runSeq, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out    
    ! atm2med in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=1, j=3, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! ocn2med in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=2, j=3, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! med in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=3, j=-1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! med2atm in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=3, j=1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! med2ocn in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=3, j=2, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! atm in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=1, j=-1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    ! ocn in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=2, j=-1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! nullify the runSeq
    nullify(is%wrap%runSeq)
    
    ! SPECIALIZE by calling into attached method to SetModelServices
    call ESMF_MethodExecute(gcomp, label=label_SetModelServices, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
    ! optionally overwrite the default run sequence
    if (associated(is%wrap%runSeq)) then
      call NUOPC_RunSequenceDeallocate(superIS%wrap%runSeq, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      superIS%wrap%runSeq => is%wrap%runSeq
    endif
      
  end subroutine
    
  !-----------------------------------------------------------------------------
  
  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    logical                   :: existflag
    character(ESMF_MAXSTR)    :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_Finalize, existflag=existflag, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      
    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine
      
end module
