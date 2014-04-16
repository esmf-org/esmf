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
#define FILENAME "src/addon/NUOPC/src/NUOPC_Comp.F90"
!==============================================================================

module NUOPC_Comp

  use ESMF
  use NUOPC_Base

  implicit none
  
  private
  
  ! public module interfaces
  public NUOPC_CompDerive
  public NUOPC_CompSetEntryPoint
  public NUOPC_CompSpecialize
  
  ! interface blocks
  interface NUOPC_CompDerive
    module procedure NUOPC_GridCompDerive
    module procedure NUOPC_CplCompDerive
  end interface
  !---------------------------------------------
  interface NUOPC_CompSetEntryPoint
    module procedure NUOPC_GridCompSetEntryPoint
!    module procedure NUOPC_CplCompSetEntryPoint
  end interface
  !---------------------------------------------
  interface NUOPC_CompSpecialize
    module procedure NUOPC_GridCompSpecialize
    module procedure NUOPC_CplCompSpecialize
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompDerive - Derive a GridComp from a generic component
! !INTERFACE:
  ! Private name; call using NUOPC_CompDerive() 
  subroutine NUOPC_GridCompDerive(comp, genericSetServicesRoutine, rc)
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: comp
    interface
      subroutine genericSetServicesRoutine(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Derive a GridComp (i.e. Model, Mediator, or Driver) from a generic component.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                   :: localrc

    if (present(rc)) rc = ESMF_SUCCESS
    
    call genericSetServicesRoutine(comp, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompDerive - Derive a CplComp from a generic component
! !INTERFACE:
  ! Private name; call using NUOPC_CompDerive() 
  subroutine NUOPC_CplCompDerive(comp, genericSetServicesRoutine, rc)
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(in)            :: comp
    interface
      subroutine genericSetServicesRoutine(cplcomp, rc)
        use ESMF
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,             intent(out), optional :: rc
! !DESCRIPTION:
!   Derive an CplComp (i.e. Connector) from a generic component.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                   :: localrc

    if (present(rc)) rc = ESMF_SUCCESS
    
    call genericSetServicesRoutine(comp, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSetEntryPoint - Set entry point for a GridComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSetEntryPoint()
  subroutine NUOPC_GridCompSetEntryPoint(comp, methodflag, phaseLabelList, &
    userRoutine, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: comp
    type(ESMF_Method_Flag), intent(in)      :: methodflag
    character(len=*),       intent(in)      :: phaseLabelList(:)
    interface
      subroutine userRoutine(gridcomp, importState, exportState, clock, rc)
        use ESMF_CompMod
        use ESMF_StateMod
        use ESMF_ClockMod
        implicit none
        type(ESMF_GridComp)         :: gridcomp     ! must not be optional
        type(ESMF_State)            :: importState  ! must not be optional
        type(ESMF_State)            :: exportState  ! must not be optional
        type(ESMF_Clock)            :: clock        ! must not be optional
        integer, intent(out)        :: rc           ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Set entry point for a GridComp (i.e. Model, Mediator, or Driver).
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                  :: i, phase, itemCount, phaseLabelCount, stat
    character(len=8)         :: phaseString
    character(len=40)        :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)

    if (present(rc)) rc = ESMF_SUCCESS

    ! determine next available phase index    
    call ESMF_GridCompGetEPPhaseCount(comp, ESMF_METHOD_INITIALIZE, &
      phaseCount=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    phase = phase + 1

    ! set the entry point with this phase index
    call ESMF_GridCompSetEntryPoint(comp, methodflag, userRoutine=userRoutine, &
      phase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
print *, "gjt: phaseLabelList:", phaseLabelList, "     phase:", phase

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "FinalizePhaseMap"
    endif
    
    ! determine how many phaseLabels are contained in the incoming list
    phaseLabelCount = size(phaseLabelList)
    
    ! query the already existing phaseMap enties
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    allocate(phases(itemCount+phaseLabelCount), stat=stat) ! space to add more
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (itemCount > 0) then
      call ESMF_AttributeGet(comp, name=trim(attributeName), &
        valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif
    
    ! add the new entries to the phaseMap
    write(phaseString, "(I6)") phase
    do i=1, phaseLabelCount
      phases(itemCount+i) = trim(phaseLabelList(i))//"="//&
        trim(adjustl(phaseString))
    enddo
    
    ! set the new phaseMap in the Attribute
    call ESMF_AttributeSet(comp, name=trim(attributeName), &
      valueList=phases, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    ! clean-up
    deallocate(phases)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSpecialize - Specialize a derived GridComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSpecialize()
  subroutine NUOPC_GridCompSpecialize(comp, specLabel, specIndex, &
    specRoutine, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: comp
    character(len=*), intent(in)            :: specLabel
    integer,          intent(in),  optional :: specIndex
    interface
      subroutine specRoutine(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Specialize a derived GridComp (i.e. Model, Mediator, or Driver).
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_MethodAdd(comp, label=specLabel, index=specIndex, &
      userRoutine=specRoutine, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSpecialize - Specialize a derived CplComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSpecialize()
  subroutine NUOPC_CplCompSpecialize(comp, specLabel, specIndex, &
    specRoutine, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: comp
    character(len=*), intent(in)            :: specLabel
    integer,          intent(in),  optional :: specIndex
    interface
      subroutine specRoutine(cplcomp, rc)
        use ESMF
        implicit none
        type(ESMF_CplComp)         :: cplcomp  ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Specialize a derived CplComp (i.e. Model, Mediator, or Driver).
!EOP
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_MethodAdd(comp, label=specLabel, index=specIndex, &
      userRoutine=specRoutine, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
