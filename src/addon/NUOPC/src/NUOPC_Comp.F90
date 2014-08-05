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
  public NUOPC_CompFilterPhaseMap
  public NUOPC_CompSearchPhaseMap
  public NUOPC_CompSetEntryPoint
  public NUOPC_CompSetInternalEntryPoint
  public NUOPC_CompSpecialize
  
  ! interface blocks
  interface NUOPC_CompDerive
    module procedure NUOPC_GridCompDerive
    module procedure NUOPC_CplCompDerive
  end interface
  !---------------------------------------------
  interface NUOPC_CompFilterPhaseMap
    module procedure NUOPC_GridCompFilterPhaseMap
    module procedure NUOPC_CplCompFilterPhaseMap
  end interface
  !---------------------------------------------
  interface NUOPC_CompSearchPhaseMap
    module procedure NUOPC_GridCompSearchPhaseMap
    module procedure NUOPC_CplCompSearchPhaseMap
  end interface
  !---------------------------------------------
  interface NUOPC_CompSetEntryPoint
    module procedure NUOPC_GridCompSetEntryPoint
    module procedure NUOPC_CplCompSetEntryPoint
  end interface
  !---------------------------------------------
  interface NUOPC_CompSetInternalEntryPoint
    module procedure NUOPC_GridCompSetIntEntryPoint
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
!   Derive a CplComp (i.e. Connector) from a generic component.
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
! !IROUTINE: NUOPC_CompFilterPhaseMap - Filter the Phase Map of a GridComp
! !INTERFACE:
  ! Private name; call using NUOPC_CompFilterPhaseMap()
  subroutine NUOPC_GridCompFilterPhaseMap(comp, methodflag, acceptStringList, &
    rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                           :: comp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
    character(len=*),       intent(in)            :: acceptStringList(:)
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Filter all PhaseMap entries in a GridComp (i.e. Model, Mediator, or Driver)
! that do {\em not} match any entry in the {\tt acceptStringList}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i, ii, iii
    integer                   :: itemCount, stat
    integer                   :: acceptStringCount
    character(ESMF_MAXSTR)    :: name
    character(len=40)         :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)
    character(len=NUOPC_PhaseMapStringLength), pointer :: newPhases(:)

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    ! determine how many phaseLabels are contained in the incoming list
    acceptStringCount = size(acceptStringList)
    
    ! query the already existing phaseMap enties
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(phases(itemCount), newPhases(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, &
      file=trim(name)//":"//FILENAME)) return  ! bail out
    if (itemCount > 0) then
      call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! filter all entries that do not match entries in acceptStringList
    iii=0 ! reset
    do i=1, itemCount
      ! see if there is a match with an entry in the acceptStringList
      do ii=1, acceptStringCount
        if (index(phases(i),trim(acceptStringList(ii))) > 0 ) exit
      enddo
      if (ii <= acceptStringCount) then
        ! found a match -> preserve the phaseMap entry
        iii = iii+1
        newPhases(iii) = trim(phases(i))
      endif
    enddo
    
    ! insert a dummy entry in case no entries are left
    if (iii==0) then
      iii=1
      newPhases(1) = "IPDvDummy" ! something obvious
    endif
    
    ! set the filtered phase map as the Attribute
    call ESMF_AttributeSet(comp, name=trim(attributeName), &
      valueList=newPhases(1:iii), &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! clean-up
    deallocate(phases, newPhases)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompFilterPhaseMap - Filter the Phase Map of a CplComp
! !INTERFACE:
  ! Private name; call using NUOPC_CompFilterPhaseMap()
  subroutine NUOPC_CplCompFilterPhaseMap(comp, methodflag, acceptStringList, &
    rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: comp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
    character(len=*),       intent(in)            :: acceptStringList(:)
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Filter all PhaseMap entries in a CplComp (i.e. Connector)
! that do {\em not} match any entry in the {\tt acceptStringList}.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i, ii, iii
    integer                   :: itemCount, stat
    integer                   :: acceptStringCount
    character(ESMF_MAXSTR)    :: name
    character(len=40)         :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)
    character(len=NUOPC_PhaseMapStringLength), pointer :: newPhases(:)

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    ! determine how many phaseLabels are contained in the incoming list
    acceptStringCount = size(acceptStringList)
    
    ! query the already existing phaseMap enties
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    allocate(phases(itemCount), newPhases(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of temporary data structure.", &
      line=__LINE__, &
      file=trim(name)//":"//FILENAME)) return  ! bail out
    if (itemCount > 0) then
      call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    endif

    ! filter all entries that do not match entries in acceptStringList
    iii=0 ! reset
    do i=1, itemCount
      ! see if there is a match with an entry in the acceptStringList
      do ii=1, acceptStringCount
        if (index(phases(i),trim(acceptStringList(ii))) > 0 ) exit
      enddo
      if (ii <= acceptStringCount) then
        ! found a match -> preserve the phaseMap entry
        iii = iii+1
        newPhases(iii) = trim(phases(i))
      endif
    enddo
    
    ! insert a dummy entry in case no entries are left
    if (iii==0) then
      iii=1
      newPhases(1) = "IPDvDummy" ! something obvious
    endif
    
    ! set the filtered phase map as the Attribute
    call ESMF_AttributeSet(comp, name=trim(attributeName), &
      valueList=newPhases(1:iii), &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! clean-up
    deallocate(phases, newPhases)
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSearchPhaseMap - Search the Phase Map of a GridComp
! !INTERFACE:
  ! Private name; call using NUOPC_CompSearchPhaseMap()
  subroutine NUOPC_GridCompSearchPhaseMap(comp, methodflag, phaseLabel, &
    phaseIndex, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                           :: comp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
    character(len=*),       intent(in),  optional :: phaseLabel
    integer,                intent(out)           :: phaseIndex
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Search all PhaseMap entries in a GridComp (i.e. Model, Mediator, or Driver)
! to see if {\tt phaseLabel} is found. Return the associated ESMF
! {\tt phaseIndex}, or {\tt -1} if not found. If {\tt phaseLabel} is not
! specified, set {\tt phaseIndex} to the first entry in the PhaseMap, or 
! {\tt -1} if there are no entries.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i
    integer                   :: itemCount, stat, ind, max
    character(ESMF_MAXSTR)    :: name
    character(len=40)         :: attributeName
    logical                   :: phaseFlag
    character(len=NUOPC_PhaseMapStringLength), pointer  :: phases(:)
    character(len=NUOPC_PhaseMapStringLength)           :: tempString

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    phaseIndex = -1             ! initialize to invalid
    phaseFlag  = .false.        ! initialize
    
    ! access phaseMap info
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! search the phaseMap
    if (itemCount > 0) then
      allocate(phases(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of temporary data structure.", &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (present(phaseLabel)) then
        do i=1, itemCount
          if (index(phases(i),trim(phaseLabel//"=")) > 0) exit
        enddo
        if (i <= itemCount) then
          ! phaseLabel was found
          phaseFlag = .true.
          tempString = trim(phases(i))
        endif
      else
        phaseFlag = .true.
        tempString = trim(phases(1))  ! by default select the first map entry
      endif
      if (phaseFlag) then
        ind = index(tempString, "=")
        max = len(tempString)
        read (tempString(ind+1:max), "(i4)") phaseIndex ! obtain phase index
      endif
      ! clean-up
      deallocate(phases)
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CompSearchPhaseMap - Search the Phase Map of a CplComp
! !INTERFACE:
  ! Private name; call using NUOPC_CompSearchPhaseMap()
  subroutine NUOPC_CplCompSearchPhaseMap(comp, methodflag, phaseLabel, &
    phaseIndex, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                            :: comp
    type(ESMF_Method_Flag), intent(in)            :: methodflag
    character(len=*),       intent(in),  optional :: phaseLabel
    integer,                intent(out)           :: phaseIndex
    integer,                intent(out), optional :: rc 
!
! !DESCRIPTION:
! Search all PhaseMap entries in a CplComp (i.e. Connector)
! to see if {\tt phaseLabel} is found. Return the associated ESMF
! {\tt phaseIndex}, or {\tt -1} if not found. If {\tt phaseLabel} is not
! specified, set {\tt phaseIndex} to the first entry in the PhaseMap, or 
! {\tt -1} if there are no entries.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                   :: i
    integer                   :: itemCount, stat, ind, max
    character(ESMF_MAXSTR)    :: name
    character(len=40)         :: attributeName
    logical                   :: phaseFlag
    character(len=NUOPC_PhaseMapStringLength), pointer  :: phases(:)
    character(len=NUOPC_PhaseMapStringLength)           :: tempString

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_CplCompGet(comp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "FinalizePhaseMap"
    endif
    
    phaseIndex = -1             ! initialize to invalid
    phaseFlag  = .false.        ! initialize
    
    ! access phaseMap info
    call ESMF_AttributeGet(comp, name=trim(attributeName), &
      itemCount=itemCount, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
    
    ! search the phaseMap
    if (itemCount > 0) then
      allocate(phases(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of temporary data structure.", &
        line=__LINE__, &
        file=trim(name)//":"//FILENAME)) return  ! bail out
      call ESMF_AttributeGet(comp, name=trim(attributeName), valueList=phases, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out
      if (present(phaseLabel)) then
        do i=1, itemCount
          if (index(phases(i),trim(phaseLabel//"=")) > 0) exit
        enddo
        if (i <= itemCount) then
          ! phaseLabel was found
          phaseFlag = .true.
          tempString = trim(phases(i))
        endif
      else
        phaseFlag = .true.
        tempString = trim(phases(1))  ! by default select the first map entry
      endif
      if (phaseFlag) then
        ind = index(tempString, "=")
        max = len(tempString)
        read (tempString(ind+1:max), "(i4)") phaseIndex ! obtain phase index
      endif
      ! clean-up
      deallocate(phases)
    endif
    
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
! Set an entry point for a GridComp (i.e. Model, Mediator, or Driver). Publish
! the new entry point in the correct {\tt PhaseMap} component attribute.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                  :: i, ii, iii
    integer                  :: phase, itemCount, phaseLabelCount, stat
    character(len=8)         :: phaseString
    character(len=40)        :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! determine next available phase index    
    call ESMF_GridCompGetEPPhaseCount(comp, methodflag, &
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
    
!print *, "NUOPC_GridCompSetEntryPoint: phaseLabelList:", &
!phaseLabelList, "     phase:", phase

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
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
    iii=0 ! initialize
    do i=1, phaseLabelCount
      ! see if this same phaseLabel has already been used before
      do ii=1, itemCount
        if (index(phases(ii),trim(phaseLabelList(i))) > 0 ) exit
      enddo
      if (ii <= itemCount) then
        ! overwrite an existing entry with the same phaseLabel
        phases(ii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      else
        ! add a new entry for the phaseLabel at the end of the list
        iii = iii+1
        phases(itemCount+iii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      endif
    enddo
    
    ! set the new phaseMap in the Attribute
    call ESMF_AttributeSet(comp, name=trim(attributeName), &
      valueList=phases(1:itemCount+iii), &
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
! !IROUTINE: NUOPC_CompSetEntryPoint - Set entry point for a CplComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSetEntryPoint()
  subroutine NUOPC_CplCompSetEntryPoint(comp, methodflag, phaseLabelList, &
    userRoutine, rc)
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: comp
    type(ESMF_Method_Flag), intent(in)      :: methodflag
    character(len=*),       intent(in)      :: phaseLabelList(:)
    interface
      subroutine userRoutine(cplcomp, importState, exportState, clock, rc)
        use ESMF_CompMod
        use ESMF_StateMod
        use ESMF_ClockMod
        implicit none
        type(ESMF_CplComp)          :: cplcomp      ! must not be optional
        type(ESMF_State)            :: importState  ! must not be optional
        type(ESMF_State)            :: exportState  ! must not be optional
        type(ESMF_Clock)            :: clock        ! must not be optional
        integer, intent(out)        :: rc           ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Set an entry point for a CplComp (i.e. Connector). Publish
! the new entry point in the correct {\tt PhaseMap} component attribute.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                  :: i, ii, iii
    integer                  :: phase, itemCount, phaseLabelCount, stat
    character(len=8)         :: phaseString
    character(len=40)        :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)

    if (present(rc)) rc = ESMF_SUCCESS

    ! determine next available phase index    
    call ESMF_CplCompGetEPPhaseCount(comp, methodflag, &
      phaseCount=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
    phase = phase + 1

    ! set the entry point with this phase index
    call ESMF_CplCompSetEntryPoint(comp, methodflag, userRoutine=userRoutine, &
      phase=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    
!print *, "NUOPC_CplCompSetEntryPoint: phaseLabelList:", &
!phaseLabelList, "     phase:", phase

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "RunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
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
    iii=0 ! initialize
    do i=1, phaseLabelCount
      ! see if this same phaseLabel has already been used before
      do ii=1, itemCount
        if (index(phases(ii),trim(phaseLabelList(i))) > 0 ) exit
      enddo
      if (ii <= itemCount) then
        ! overwrite an existing entry with the same phaseLabel
        phases(ii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      else
        ! add a new entry for the phaseLabel at the end of the list
        iii = iii+1
        phases(itemCount+iii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      endif
    enddo
    
    ! set the new phaseMap in the Attribute
    call ESMF_AttributeSet(comp, name=trim(attributeName), &
      valueList=phases(1:itemCount+iii), &
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
! !IROUTINE: NUOPC_CompSetInternalEntryPoint - Set internal entry point for a GridComp
!
! !INTERFACE:
  ! Private name; call using NUOPC_CompSetInternalEntryPoint()
  subroutine NUOPC_GridCompSetIntEntryPoint(comp, methodflag, phaseLabelList, &
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
! Set the internal entry point for a GridComp (i.e. Driver). Only Drivers 
! utilize internal entry points.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                  :: i, ii, iii
    integer                  :: phase, itemCount, phaseLabelCount, stat
    character(len=8)         :: phaseString
    character(len=40)        :: attributeName
    character(len=NUOPC_PhaseMapStringLength), pointer :: phases(:)

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! determine next available phase index    
    call ESMF_GridCompGetEPPhaseCount(comp, methodflag, &
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
    
!print *, "NUOPC_GridCompSetEntryPoint: phaseLabelList:", &
!phaseLabelList, "     phase:", phase

    ! determine which phaseMap to deal with
    attributeName = "UnknownPhaseMap" ! initialize to something obvious
    if (methodflag == ESMF_METHOD_INITIALIZE) then
      attributeName = "InternalInitializePhaseMap"
    elseif (methodflag == ESMF_METHOD_RUN) then
      attributeName = "InternalRunPhaseMap"
    elseif (methodflag == ESMF_METHOD_FINALIZE) then
      attributeName = "InternalFinalizePhaseMap"
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
    iii=0 ! initialize
    do i=1, phaseLabelCount
      ! see if this same phaseLabel has already been used before
      do ii=1, itemCount
        if (index(phases(ii),trim(phaseLabelList(i))) > 0 ) exit
      enddo
      if (ii <= itemCount) then
        ! overwrite an existing entry with the same phaseLabel
        phases(ii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      else
        ! add a new entry for the phaseLabel at the end of the list
        iii = iii+1
        phases(itemCount+iii) = trim(phaseLabelList(i))//"="//&
          trim(adjustl(phaseString))
      endif
    enddo
    
    ! set the new phaseMap in the Attribute
    call ESMF_AttributeSet(comp, name=trim(attributeName), &
      valueList=phases(1:itemCount+iii), &
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
! Specialize a derived CplComp (i.e. Connector).
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
