! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Compliance_Model.F90"
!==============================================================================

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Compliance Interface Component for NUOPC_Model components.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
! !DESCRIPTION:
!  Interface Component
!-------------------------------------------------------------------------

module NUOPC_Compliance_Model

    use ESMF
    use NUOPC_Base, only: NUOPC_PhaseMapStringLength  ! change this?
    use NUOPC_Compliance_Base

    implicit none
  
    private
  
    ! these map NUOPC events to the phase when it should occur
    ! therefore, we can check after the phase to verify
    character(*), parameter :: &
        event_AdvertiseFields = "IPDv00p1|IPDv01p1|IPDv02p1|IPDv03p1|IPDv04p1"
    character(*), parameter :: &
        event_RealizeFields = "IPDv00p2"  ! TODO: deal with other versions of realize
    character(*), parameter :: &
        event_InternalClockSet = "IPDv00p3|IPDv01p4|IPDv02p4|IPDv03p6|IPDv04p6"

    public setvmIC, registerIC
        
contains

    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !   !  The setvm routine is used by the child component to set VM properties
    !   !TODO:  currently the setvmIC() is _not_ hooked into the ESMF callback

    recursive subroutine setvmIC(comp, rc)
        type(ESMF_GridComp)   :: comp
        integer, intent(out)  :: rc
    
        ! Initialize user return code
        rc = ESMF_SUCCESS
    
        print *, ">START setvmIC"

        ! This code is being executed _after_ the actual Component SetVM call
    
        !TODO: currently the setvmIC() is _not_ hooked into the ESMF callback

        print *, ">STOP setvmIC"

    end subroutine

    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !   !  The Register routine sets the subroutines to be called
    !   !   as the init, run, and finalize routines.  Note that these are
    !   !   private to the module.
 
    recursive subroutine registerIC(comp, rc)
        type(ESMF_GridComp)   :: comp
        integer, intent(out)  :: rc

        character(ESMF_MAXSTR)  :: prefix
        character(ESMF_MAXSTR)  :: output
        integer                 :: phaseCount, phase
        logical                 :: phaseZeroFlag
        logical                 :: regAdvertise, regRealize
        character(NUOPC_PhaseMapStringLength) :: phaseLabel
    
        ! Initialize user return code
        rc = ESMF_SUCCESS
    
        ! methods user must provide
        regAdvertise = .false.
        regRealize = .false.

        ! IMPORTANT: As an InterfaceComponent the code must ensure:
        ! 1) That the return code from the actual child method is returned to the
        !    parent (note that this is not currently possible for the register)

        ! This code is being executed _after_ the actual Component Register call

        call prefixString(comp, prefix=prefix, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        if (.not. complianceInit) then
            call NUOPC_ComplianceInit(rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        endif

        !---------------------------------------------------------------------------
        ! Start Compliance Checking and IC method Registration

        if (ccfDepth <= maxDepth .or. maxDepth < 0) then

            call NUOPC_ComplianceLogWrite(trim(prefix)//">START register compliance check.", &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
    
            ! check Initialize registration
            call ESMF_GridCompGetEPPhaseCount(comp, ESMF_METHOD_INITIALIZE, phaseCount, &
                phaseZeroFlag, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            if (phaseZeroFlag) then
                call NUOPC_ComplianceLogWrite(trim(prefix)//" phase ZERO for Initialize registered.",&
                    rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
                call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZEIC, &
                    userRoutine=ic_init, phase=0, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            else
                call NUOPC_ComplianceLogWrite(trim(prefix)//" ==> NUOPC requires Initialize phase ZERO!", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif
            if (phaseCount == 0) then
                call NUOPC_ComplianceLogWrite(trim(prefix)//" ==> No Initialize method registered!", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            else
                if (phaseZeroFlag) then
                    write(output,*) " ",phaseCount," phase(s) of Initialize registered"// &
                        " (not counting ZERO phase)."
                else
                    write(output,*) " ",phaseCount," phase(s) of Initialize registered."
                endif
                call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
                do phase=1, phaseCount
                    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZEIC, &
                        userRoutine=ic_init, phase=phase, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out

                    call NUOPC_CompSearchPhaseMapByIndex(comp, ESMF_METHOD_INITIALIZE, &
                        phase, phaseLabel, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out

                    if (index(event_AdvertiseFields, trim(phaseLabel)) > 0) then
                        !print *, "Found advertise: "//phaseLabel, "phase=", phase
                        regAdvertise = .true.
                    endif
                    if (index(event_RealizeFields, trim(phaseLabel)) > 0) then
                        regRealize = .true.
                    endif

                enddo
            endif
    
            ! report missing phases
            ! this does NOT appear to work because it looks like these
            ! phases are defaulted in the InitializePhaseMap
            if (.not. regAdvertise) then
                call NUOPC_ComplianceLogWrite(trim(prefix)//" missing advertise phase.", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif
            if (.not. regRealize) then
                call NUOPC_ComplianceLogWrite(trim(prefix)//" missing realize phase.", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif

            ! check Run registration
            call ESMF_GridCompGetEPPhaseCount(comp, ESMF_METHOD_RUN, phaseCount, &
                phaseZeroFlag, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            if (phaseZeroFlag) then
                call NUOPC_ComplianceLogWrite(trim(prefix)//" phase ZERO for Run registered.",&
                    rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
                call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUNIC, &
                    userRoutine=ic_run, phase=0, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif
            if (phaseCount == 0) then
                call NUOPC_ComplianceLogWrite(trim(prefix)//" ==> No Run method registered!", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            else
                if (phaseZeroFlag) then
                    write(output,*) " ",phaseCount," phase(s) of Run registered"// &
                        " (not counting ZERO phase)."
                else
                    write(output,*) " ",phaseCount," phase(s) of Run registered."
                endif
                call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
                do phase=1, phaseCount
                    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUNIC, &
                        userRoutine=ic_run, phase=phase, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                enddo
            endif

            ! check Finalize registration
            call ESMF_GridCompGetEPPhaseCount(comp, ESMF_METHOD_FINALIZE, phaseCount, &
                phaseZeroFlag, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
            if (phaseZeroFlag) then
                call NUOPC_ComplianceLogWrite(trim(prefix)//" phase ZERO for Finalize registered.",&
                    rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
                call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZEIC, &
                    userRoutine=ic_final, phase=0, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            endif
            if (phaseCount == 0) then
                call NUOPC_ComplianceLogWrite(trim(prefix)//" ==> No Finalize method registered!", &
                    ESMF_LOGMSG_WARNING, rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
            else
                if (phaseZeroFlag) then
                    write(output,*) " ",phaseCount," phase(s) of Finalize registered"// &
                        " (not counting ZERO phase)."
                else
                    write(output,*) " ",phaseCount," phase(s) of Finalize registered."
                endif
                call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), rc=rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, &
                    file=FILENAME)) &
                    return  ! bail out
                do phase=1, phaseCount
                    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZEIC, &
                        userRoutine=ic_final, phase=phase, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                        line=__LINE__, &
                        file=FILENAME)) &
                        return  ! bail out
                enddo
            endif

            call NUOPC_ComplianceLogWrite(trim(prefix)//">STOP register compliance check.", &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            if (outputTrace) then
              call NUOPC_TraceComponentInfo(comp, rc=rc)
              if (ESMF_LogFoundError(rc, &
                line=__LINE__, file=FILENAME)) return                              
            endif
            
        endif
      ! Stop Compliance Checking
      !---------------------------------------------------------------------------

    end subroutine


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !   !   Initialization routine.
    
    recursive subroutine ic_init(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp)   :: comp
        type(ESMF_State)      :: importState, exportState
        type(ESMF_Clock)      :: clock
        integer, intent(out)  :: rc

        ! Local variables
        integer                 :: userrc
        character(ESMF_MAXSTR)  :: prefix
        character(ESMF_MAXSTR)  :: output, pString
        type(ESMF_Clock)        :: clockCopy
        integer                 :: phase
        character(NUOPC_PhaseMapStringLength) :: phaseLabel
        character(ESMF_MAXSTR)  :: compName
        logical                 :: clockIsCreated
        !type(ESMF_Clock)        :: clockInternal
        !logical                 :: clockIsPresent
    
        ! Initialize user return code
        rc = ESMF_SUCCESS

        call prefixString(comp, prefix=prefix, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        call ESMF_GridCompGet(comp, currentPhase=phase, name=compName, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        ! format phase
        write(pString,*) phase
        call NUOPC_CompSearchPhaseMapByIndex(comp, ESMF_METHOD_INITIALIZE, &
          phaseIndex=phase, phaseLabel=phaseLabel, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out

        !---------------------------------------------------------------------------
        ! Start Compliance Checking: InitializePrologue
        if (ccfDepth <= maxDepth .or. maxDepth < 0) then

            if (outputJSON) then
                call JSON_LogCtrlFlow("start_prologue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

            write(output,*) ">START InitializePrologue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check importState
            call NUOPC_CheckState(prefix, referenceName="importState", state=importState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check exportState
            call NUOPC_CheckState(prefix, referenceName="exportState", state=exportState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check clock usage
            call NUOPC_CheckClockUsageIncoming(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! check Component statistics
            call NUOPC_CheckComponentStatistics(prefix, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check Component metadata
            call NUOPC_CheckComponentMetadata(prefix, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! phase specific checks
            call dispatchPhaseChecks(prefix, comp, ESMF_METHOD_INITIALIZE, &
                phase, importState, exportState, clock, .true., rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write(output,*) ">STOP InitializePrologue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            if (outputJSON) then
                call JSON_LogCtrlFlow("stop_prologue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
                call JSON_LogCtrlFlow("start_phase", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

            if (outputTrace) then
               call ESMF_TracePhasePrologueEnter(comp, rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return               
               call ESMF_TraceMemInfo(rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return                              
               clockIsCreated = ESMF_ClockIsCreated(clock, rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return
               if (clockIsCreated) then
                  call ESMF_TraceClock(clock, rc=rc)
                  if (ESMF_LogFoundError(rc, &
                       line=__LINE__, file=FILENAME)) return                              
               endif
               !call NUOPC_TraceComponentInfo(comp, rc=rc)
               !if (ESMF_LogFoundError(rc, &
               !     line=__LINE__, file=FILENAME)) return                              
               call ESMF_TracePhaseEnter(comp, rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return                              
            endif
            
        endif
        ! Stop Compliance Checking: InitializePrologue
        !---------------------------------------------------------------------------
        ccfDepth = ccfDepth + 1
        
        ! Call the actual Initialize routine        
        call ESMF_GridCompInitializeAct(comp, importState, exportState, clock, &
            phase=phase, userRc=userrc, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
   
        ccfDepth = ccfDepth - 1
        !---------------------------------------------------------------------------
        ! Start Compliance Checking: InitializeEpilogue
        if (ccfDepth <= maxDepth .or. maxDepth < 0) then
           
           if (outputTrace) then
              call ESMF_TracePhaseExit(comp, rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return                              
              clockIsCreated = ESMF_ClockIsCreated(clock, rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return
              if (clockIsCreated) then
                 call ESMF_TraceClock(clock, rc=rc)
                 if (ESMF_LogFoundError(rc, &
                       line=__LINE__, file=FILENAME)) return                              
              endif
              call ESMF_TraceMemInfo(rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return
              call ESMF_TracePhaseEpilogueExit(comp, rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return                              
           endif

            if (outputJSON) then
                call JSON_LogCtrlFlow("stop_phase", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
                call JSON_LogCtrlFlow("start_epilogue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

            call prefixString(comp, prefix=prefix, forward=.false., rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write(output,*) ">START InitializeEpilogue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! check Component statistics
            call NUOPC_CheckComponentStatistics(prefix, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check Component metadata
            call NUOPC_CheckComponentMetadata(prefix, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check importState
            call NUOPC_CheckState(prefix, referenceName="importState", state=importState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
    
            ! compliance check exportState
            call NUOPC_CheckState(prefix, referenceName="exportState", state=exportState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check clock usage
            call NUOPC_CheckClockUsageOutgoing(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check internal Clock
            ! REMOVED - moved to phase specific check (checkEpi_InternalClockSet)
            !call NUOPC_CheckInternalClock(prefix, comp=comp, clock=clock, &
            !    mustMatchCurr=.false., mustReachStop=.false., rc=rc)
            !if (ESMF_LogFoundError(rc, &
            !    line=__LINE__, &
            !    file=FILENAME)) &
            !    return  ! bail out

            ! Component Attributes should be set up -> ready to output
            !    call ESMF_AttributeWrite(comp, convention='CIM 1.5', &
            !      purpose='ModelComp', &
            !      attwriteflag=ESMF_ATTWRITE_XML, rc=rc)
            !    if (ESMF_LogFoundError(rc, &
            !      line=__LINE__, &
            !      file=FILENAME)) &
            !      return  ! bail out

            call dispatchPhaseChecks(prefix, comp, ESMF_METHOD_INITIALIZE, &
                phase, importState, exportState, clock, .false., rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write(output,*) ">STOP InitializeEpilogue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            if (outputJSON) then
                call JSON_LogCtrlFlow("stop_epilogue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

        endif
        ! Stop Compliance Checking: InitializeEpilogue
        !---------------------------------------------------------------------------

        ! if not bailed for other reasons then pass back the actual userrc
        rc = userrc

    end subroutine ic_init



    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !   !  Run routine
 
    recursive subroutine ic_run(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp)   :: comp
        type(ESMF_State)      :: importState, exportState
        type(ESMF_Clock)      :: clock
        integer, intent(out)  :: rc

        ! Local variables
        integer                 :: userrc
        character(ESMF_MAXSTR)  :: prefix
        character(ESMF_MAXSTR)  :: output, pString
        type(ESMF_Clock)        :: clockCopy
        integer                 :: phase
        character(NUOPC_PhaseMapStringLength) :: phaseLabel
        character(ESMF_MAXSTR)  :: compName
        logical                 :: clockIsCreated
        !type(ESMF_Clock)        :: clockInternal
        !logical                 :: clockIsPresent
    
        ! Initialize user return code
        rc = ESMF_SUCCESS

        call prefixString(comp, prefix=prefix, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        call ESMF_GridCompGet(comp, currentPhase=phase, name=compName, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        ! format phase
        write(pString,*) phase
        call NUOPC_CompSearchPhaseMapByIndex(comp, ESMF_METHOD_RUN, &
          phaseIndex=phase, phaseLabel=phaseLabel, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out

        !---------------------------------------------------------------------------
        ! Start Compliance Checking: RunPrologue
        if (ccfDepth <= maxDepth .or. maxDepth < 0) then

            if (outputJSON) then
                call JSON_LogCtrlFlow("start_prologue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

            write(output,*) ">START RunPrologue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check importState
            call NUOPC_CheckState(prefix, referenceName="importState", state=importState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check exportState
            call NUOPC_CheckState(prefix, referenceName="exportState", state=exportState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check clock usage
            call NUOPC_CheckClockUsageIncoming(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check internal Clock
            call NUOPC_CheckInternalClock(prefix, comp=comp, clock=clock, &
                mustMatchCurr=.true., mustReachStop=.false., rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! check Component statistics
            call NUOPC_CheckComponentStatistics(prefix, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write(output,*) ">STOP RunPrologue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            if (outputJSON) then
                call JSON_LogCtrlFlow("stop_prologue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
                call JSON_LogCtrlFlow("start_phase", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

            if (outputTrace) then
               call ESMF_TracePhasePrologueEnter(comp, rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return               
               call ESMF_TraceMemInfo(rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return                              
               clockIsCreated = ESMF_ClockIsCreated(clock, rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return
               if (clockIsCreated) then
                  call ESMF_TraceClock(clock, rc=rc)
                  if (ESMF_LogFoundError(rc, &
                       line=__LINE__, file=FILENAME)) return                              
               endif
               call ESMF_TracePhaseEnter(comp, rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return                              
            endif

         endif
        ! Stop Compliance Checking: RunPrologue
        !---------------------------------------------------------------------------
        ccfDepth = ccfDepth + 1
        
        ! Call the actual Run routine
        call ESMF_GridCompRunAct(comp, importState, exportState, clock, &
            phase=phase, userRc=userrc, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out
        
        ccfDepth = ccfDepth - 1
        !---------------------------------------------------------------------------
        ! Start Compliance Checking: RunEpilogue
        if (ccfDepth <= maxDepth .or. maxDepth < 0) then

           if (outputTrace) then
              call ESMF_TracePhaseExit(comp, rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return                              
              clockIsCreated = ESMF_ClockIsCreated(clock, rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return
              if (clockIsCreated) then
                 call ESMF_TraceClock(clock, rc=rc)
                 if (ESMF_LogFoundError(rc, &
                       line=__LINE__, file=FILENAME)) return                              
              endif
              call ESMF_TraceMemInfo(rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return
              call ESMF_TracePhaseEpilogueExit(comp, rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return                              
           endif
           
           if (outputJSON) then
                call JSON_LogCtrlFlow("stop_phase", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
                call JSON_LogCtrlFlow("start_epilogue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

            call prefixString(comp, prefix=prefix, forward=.false., rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write(output,*) ">START RunEpilogue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! check Component statistics
            call NUOPC_CheckComponentStatistics(prefix, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check importState
            call NUOPC_CheckState(prefix, referenceName="importState", state=importState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check exportState
            call NUOPC_CheckState(prefix, referenceName="exportState", state=exportState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check clock usage
            call NUOPC_CheckClockUsageOutgoing(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check internal Clock
            call NUOPC_CheckInternalClock(prefix, comp=comp, clock=clock, &
                mustMatchCurr=.false., mustReachStop=.true., rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write(output,*) ">STOP RunEpilogue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            if (outputJSON) then
                call JSON_LogCtrlFlow("stop_epilogue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

        endif
        ! Stop Compliance Checking: RunEpilogue
        !---------------------------------------------------------------------------

        ! if not bailed for other reasons then pass back the actual userrc
        rc = userrc

    end subroutine ic_run


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !   !  Finalize routine
 
    recursive subroutine ic_final(comp, importState, exportState, clock, rc)
        type(ESMF_GridComp)   :: comp
        type(ESMF_State)      :: importState, exportState
        type(ESMF_Clock)      :: clock
        integer, intent(out)  :: rc

        ! Local variables
        integer                 :: userrc
        character(ESMF_MAXSTR)  :: prefix
        character(ESMF_MAXSTR)  :: output, pString
        type(ESMF_Clock)        :: clockCopy
        integer                 :: phase
        character(NUOPC_PhaseMapStringLength) :: phaseLabel
        character(ESMF_MAXSTR)  :: compName
        logical                 :: clockIsCreated
        !type(ESMF_Clock)        :: clockInternal
        !logical                 :: clockIsPresent
    
        ! Initialize user return code
        rc = ESMF_SUCCESS

        call prefixString(comp, prefix=prefix, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        call ESMF_GridCompGet(comp, currentPhase=phase, name=compName, &
            rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        ! format phase
        write(pString,*) phase
        call NUOPC_CompSearchPhaseMapByIndex(comp, ESMF_METHOD_FINALIZE, &
          phaseIndex=phase, phaseLabel=phaseLabel, rc=rc)
        if (ESMF_LogFoundError(rc, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out

        !---------------------------------------------------------------------------
        ! Start Compliance Checking: FinalizePrologue
        if (ccfDepth <= maxDepth .or. maxDepth < 0) then

            if (outputJSON) then
                call JSON_LogCtrlFlow("start_prologue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

            write(output,*) ">START FinalizePrologue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check importState
            call NUOPC_CheckState(prefix, referenceName="importState", state=importState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check exportState
            call NUOPC_CheckState(prefix, referenceName="exportState", state=exportState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check clock usage
            call NUOPC_CheckClockUsageIncoming(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! check Component statistics
            call NUOPC_CheckComponentStatistics(prefix, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write(output,*) ">STOP FinalizePrologue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            if (outputJSON) then
                call JSON_LogCtrlFlow("stop_prologue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
                call JSON_LogCtrlFlow("start_phase", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif
            
            if (outputTrace) then
               call ESMF_TracePhasePrologueEnter(comp, rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return               
               call ESMF_TraceMemInfo(rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return                              
               clockIsCreated = ESMF_ClockIsCreated(clock, rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return
               if (clockIsCreated) then
                  call ESMF_TraceClock(clock, rc=rc)
                  if (ESMF_LogFoundError(rc, &
                       line=__LINE__, file=FILENAME)) return                              
               endif
               call ESMF_TracePhaseEnter(comp, rc=rc)
               if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return                              
            endif
                    
        endif
        ! Stop Compliance Checking: FinalizePrologue
        !---------------------------------------------------------------------------
        ccfDepth = ccfDepth + 1

        ! Call the actual Finalize routine
        call ESMF_GridCompFinalizeAct(comp, importState, exportState, clock, &
            phase=phase, userRc=userrc, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        ccfDepth = ccfDepth - 1
        !---------------------------------------------------------------------------
        ! Start Compliance Checking: FinalizeEpilogue
        if (ccfDepth <= maxDepth .or. maxDepth < 0) then

           if (outputTrace) then
              call ESMF_TracePhaseExit(comp, rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return                              
              clockIsCreated = ESMF_ClockIsCreated(clock, rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return
              if (clockIsCreated) then
                 call ESMF_TraceClock(clock, rc=rc)
                 if (ESMF_LogFoundError(rc, &
                      line=__LINE__, file=FILENAME)) return                              
              endif
              call ESMF_TraceMemInfo(rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return
              call ESMF_TracePhaseEpilogueExit(comp, rc=rc)
              if (ESMF_LogFoundError(rc, &
                   line=__LINE__, file=FILENAME)) return                              
           endif
           
           if (outputJSON) then
                call JSON_LogCtrlFlow("stop_phase", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
                call JSON_LogCtrlFlow("start_epilogue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

            call prefixString(comp, prefix=prefix, forward=.false., rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write(output,*) ">START FinalizeEpilogue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! check Component statistics
            call NUOPC_CheckComponentStatistics(prefix, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check importState
            call NUOPC_CheckState(prefix, referenceName="importState", state=importState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check exportState
            call NUOPC_CheckState(prefix, referenceName="exportState", state=exportState, &
                rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            ! compliance check clock usage
            call NUOPC_CheckClockUsageOutgoing(prefix, clock=clock, clockCopy=clockCopy, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            write(output,*) ">STOP FinalizeEpilogue for phase:", &
              trim(adjustl(pString)), ": ", trim(phaseLabel)
            call NUOPC_ComplianceLogWrite(trim(prefix)//trim(output), &
                ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out

            if (outputJSON) then
                call JSON_LogCtrlFlow("stop_epilogue", comp, rc)
                if (ESMF_LogFoundError(rc, &
                    line=__LINE__, file=FILENAME)) return  ! bail out
            endif

        endif
        ! Stop Compliance Checking: FinalizeEpilogue
        !---------------------------------------------------------------------------

        ! if not bailed for other reasons then pass back the actual userrc
        rc = userrc

    end subroutine ic_final


    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    ! IC HELPER ROUTINES:
    !-------------------------------------------------------------------------

    recursive subroutine prefixString(comp, prefix, forward, rc)
        type(ESMF_GridComp)                       :: comp
        character(*),       intent(inout)         :: prefix
        logical,            intent(in),  optional :: forward
        integer,            intent(out), optional :: rc

        integer                 :: localrc
        character(ESMF_MAXSTR)  :: compName
        character(len=3)        :: arrow
        character(len=3)        :: forwardArrow

        if (present(rc)) rc = ESMF_SUCCESS

        forwardArrow = "|->"
        arrow = "|->" ! default direction
        if (present(forward)) then
            if (.not.forward) arrow = "|<-"
        endif

        call ESMF_GridCompGet(comp, name=compName, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        prefix = "COMPLIANCECHECKER:"//repeat(forwardArrow, ccfDepth-1)//&
            arrow//":"//trim(compName)//":"

    end subroutine



    recursive subroutine dispatchPhaseChecks(prefix, comp, methodflag, &
        phaseIndex, importState, exportState, clock, prologue, rc)

        character(*), intent(in)           :: prefix
        type(ESMF_GridComp)                :: comp
        type(ESMF_Method_Flag), intent(in) :: methodflag
        integer,                intent(in) :: phaseIndex
        type(ESMF_State)                   :: importState, exportState
        type(ESMF_Clock)                   :: clock
        logical                            :: prologue
        integer, intent(out)               :: rc

        ! local variables
        character(NUOPC_PhaseMapStringLength) :: phaseLabel

        rc = ESMF_SUCCESS

        call NUOPC_CompSearchPhaseMapByIndex(comp, &
            methodflag, phaseIndex, phaseLabel, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        if (prologue) then
            ! nothing yet
        else ! epilogue
            if (methodflag==ESMF_METHOD_INITIALIZE) then
                if (index(event_AdvertiseFields, trim(phaseLabel)) > 0) then
                    call checkEpi_Advertise(prefix, comp, importState, &
                        exportState, clock, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                      line=__LINE__, &
                      file=FILENAME)) &
                      return  ! bail out
                elseif (index(event_InternalClockSet, trim(phaseLabel)) > 0) then
                    call checkEpi_InternalClockSet(prefix, comp, importState, &
                        exportState, clock, rc=rc)
                    if (ESMF_LogFoundError(rc, &
                      line=__LINE__, &
                      file=FILENAME)) &
                      return  ! bail out
                endif
            endif
        endif

    end subroutine dispatchPhaseChecks

    !
    ! Checks related to field advertise initialize
    !
    recursive subroutine checkEpi_Advertise(prefix, comp, &
        importState, exportState, clock, rc)

        character(*), intent(in)           :: prefix
        type(ESMF_GridComp)                :: comp
        type(ESMF_State)                   :: importState, exportState
        type(ESMF_Clock)                   :: clock
        integer, intent(out)               :: rc

        ! local variables
        integer  :: importFieldCount, exportFieldCount

        rc = ESMF_SUCCESS

        !print *, "Inside checkEpi_Advertise()"

        importFieldCount = 0
        exportFieldCount = 0

        call checkStateAfterAdvertise(prefix=prefix, &
            state=importState, totalFields=importFieldCount, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        call checkStateAfterAdvertise(prefix=prefix, &
            state=exportState, totalFields=exportFieldCount, rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

        if (importFieldCount + exportFieldCount == 0) then
            call NUOPC_ComplianceLogWrite(trim(prefix)// &
                " does not advertise any fields (import or export).", &
                ESMF_LOGMSG_WARNING, rc=rc)
            if (ESMF_LogFoundError(rc, &
                line=__LINE__, &
                file=FILENAME)) &
                return  ! bail out
        endif


    end subroutine

    !
    ! Checks after internal clock should be set
    !
    recursive subroutine checkEpi_InternalClockSet(prefix, comp, &
        importState, exportState, clock, rc)

        character(*), intent(in)           :: prefix
        type(ESMF_GridComp)                :: comp
        type(ESMF_State)                   :: importState, exportState
        type(ESMF_Clock)                   :: clock
        integer, intent(out)               :: rc

        rc = ESMF_SUCCESS

        call NUOPC_CheckInternalClock(prefix, comp=comp, clock=clock, &
                mustMatchCurr=.false., mustReachStop=.false., rc=rc)
        if (ESMF_LogFoundError(rc, &
            line=__LINE__, &
            file=FILENAME)) &
            return  ! bail out

    end subroutine



    recursive subroutine checkStateAfterAdvertise(prefix, &
        state, totalFields, rc)

        character(*), intent(in)              :: prefix
        type(ESMF_State)                      :: state
        integer,      intent(out), optional   :: totalFields
        integer,      intent(out), optional   :: rc

        ! local variables
        integer :: localrc
        integer :: item, itemCount
        character(ESMF_MAXSTR), allocatable    :: itemNameList(:)
        type(ESMF_StateItem_Flag), allocatable :: stateitemtypeList(:)
        type(ESMF_Field)                       :: field
        type(ESMF_Field), allocatable          :: fields(:)
        type(ESMF_FieldBundle)                 :: fieldbundle
        integer                                :: fieldCount, fitem

        if (present(rc)) rc = ESMF_SUCCESS

        totalFields = 0

        call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        if (itemCount > 0) then
            allocate(itemNameList(itemCount))
            allocate(stateitemtypeList(itemCount))
            call ESMF_StateGet(state, itemNameList=itemNameList, &
                itemtypeList=stateitemtypeList, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                line=__LINE__, &
                file=FILENAME, &
                rcToReturn=rc)) &
                return  ! bail out

            do item=1, itemCount

                if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
                    totalFields = totalFields + 1
                    call ESMF_StateGet(state, itemName=itemNameList(item), &
                        field=field, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                        line=__LINE__, &
                        file=FILENAME, &
                        rcToReturn=rc)) &
                        return  ! bail out
                    call checkFieldMetaAfterAdvertise(prefix, field=field, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                        line=__LINE__, &
                        file=FILENAME, &
                        rcToReturn=rc)) &
                        return  ! bail out
                else if (stateitemtypeList(item) == ESMF_STATEITEM_FIELDBUNDLE) then
                    call ESMF_StateGet(state, itemName=itemNameList(item), &
                        fieldbundle=fieldbundle, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                        line=__LINE__, &
                        file=FILENAME, &
                        rcToReturn=rc)) &
                        return  ! bail out
                    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                        line=__LINE__, &
                        file=FILENAME, &
                        rcToReturn=rc)) &
                        return  ! bail out
                    allocate(fields(fieldCount))
                    call ESMF_FieldBundleGet(fieldbundle, fieldList=fields, rc=localrc)
                    if (ESMF_LogFoundError(localrc, &
                        line=__LINE__, &
                        file=FILENAME, &
                        rcToReturn=rc)) &
                        return  ! bail out
                    do fitem=1, fieldCount
                        totalFields = totalFields + 1
                        field = fields(fitem)
                        call checkFieldMetaAfterAdvertise(prefix, field=field, rc=localrc)
                        if (ESMF_LogFoundError(localrc, &
                            line=__LINE__, &
                            file=FILENAME, &
                            rcToReturn=rc)) &
                            return  ! bail out
                    enddo
                    deallocate(fields)
                endif

            enddo

            deallocate(stateitemtypeList)
            deallocate(itemNameList)
        endif

    end subroutine


    !-------------------------------------------------------------------------

    !  recursive subroutine checkFieldMetadata(prefix, field, rc)
    !    character(*), intent(in)              :: prefix
    !    type(ESMF_Field)                      :: field
    !    integer,      intent(out), optional   :: rc
    !
    !    character(ESMF_MAXSTR)                :: attributeName
    !    character(ESMF_MAXSTR)                :: convention
    !    character(ESMF_MAXSTR)                :: purpose
    !
    !    if (present(rc)) rc = ESMF_SUCCESS
    !
    !    ! set NUOPC convention and purpose specifiers
    !    convention = "NUOPC"
    !    purpose = "General"
    !
    !    call ESMF_LogWrite(trim(prefix)//" Field level attribute check: "// &
    !      "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
    !      ESMF_LOGMSG_INFO, rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "StandardName"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "Units"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "LongName"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "ShortName"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "Intent"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "Connected"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "TimeStamp"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "ProducerConnection"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "ConsumerConnection"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "Updated"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "TransferOfferGeomObject"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !    attributeName = "TransferActionGeomObject"
    !    call checkFieldAttribute(prefix, field=field, &
    !      attributeName=attributeName, convention=convention, purpose=purpose, &
    !      rc=rc)
    !    if (ESMF_LogFoundError(rc, &
    !      line=__LINE__, &
    !      file=FILENAME)) &
    !      return  ! bail out
    !
    !  end subroutine

    recursive subroutine checkFieldMetaAfterAdvertise(prefix, field, rc)
        character(*), intent(in)              :: prefix
        type(ESMF_Field)                      :: field
        integer,      intent(out), optional   :: rc

        integer                               :: localrc
        character(ESMF_MAXSTR)                :: attributeName
        character(ESMF_MAXSTR)                :: convention
        character(ESMF_MAXSTR)                :: purpose

        if (present(rc)) rc = ESMF_SUCCESS

        ! set NUOPC convention and purpose specifiers
        convention = "NUOPC"
        purpose = "Instance"

        !    call ESMF_LogWrite(trim(prefix)//" Field level attribute check: "// &
        !      "convention: '"//trim(convention)//"', purpose: '"//trim(purpose)//"'.", &
        !      ESMF_LOGMSG_INFO, rc=localrc)
        !    if (ESMF_LogFoundError(localrc, &
        !      line=__LINE__, &
        !      file=FILENAME)) &
        !      return  ! bail out

        attributeName = "StandardName"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "Units"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "LongName"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "ShortName"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        !      How is this used by NUOPC?

        !      attributeName = "Intent"
        !      call NUOPC_CheckFieldAttribute(prefix, field=field, &
        !          attributeName=attributeName, convention=convention, purpose=purpose, &
        !          rc=localrc)
        !      if (ESMF_LogFoundError(localrc, &
        !          line=__LINE__, &
        !          file=FILENAME)) &
        !          return  ! bail out

        attributeName = "Connected"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "TimeStamp"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "ProducerConnection"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "ConsumerConnection"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "Updated"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "TransferOfferGeomObject"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "TransferActionGeomObject"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "UngriddedLBound"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            warnIfMissing=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

        attributeName = "UngriddedUBound"
        call NUOPC_CheckFieldAttribute(prefix, field=field, &
            attributeName=attributeName, convention=convention, purpose=purpose, &
            warnIfMissing=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            line=__LINE__, &
            file=FILENAME, &
            rcToReturn=rc)) &
            return  ! bail out

    end subroutine
    
    recursive subroutine NUOPC_TraceComponentInfo(comp, rc)
      
      type(ESMF_GridComp), intent(in) :: comp
      integer, intent(out)  :: rc

      character(len=5)  :: attrConv(4)
      character(len=8)  :: attrPurp(4)
      character(len=20) :: attrName(4)
      character(len=10) :: attrKey(4)
      
      rc = ESMF_SUCCESS      
      
      attrConv = "NUOPC"
      attrPurp = "Instance"

      attrName(1) = "Kind"
      attrKey(1) = "Kind"
      
      attrName(2) = "InitializePhaseMap"
      attrKey(2) = "IPM"
      
      attrName(3) = "RunPhaseMap"
      attrKey(3) = "RPM"

      attrName(4) = "FinalizePhaseMap"
      attrKey(4) = "FPM"
      
      call ESMF_TraceComponentInfo(comp, attrConv, &
           attrPurp, attrName, attrKey, rc=rc)
      if (ESMF_LogFoundError(rc, &
           line=__LINE__, &
           file=FILENAME)) &
           return  
      
    end subroutine NUOPC_TraceComponentInfo

    
    
!-------------------------------------------------------------------------

!  recursive function zeroTerminatedString(string)
!    logical :: zeroTerminatedString
!    character(len=*), intent(in)        :: string
!    integer(kind=selected_int_kind(2))  :: vbyte, mbyte
!
!    zeroTerminatedString = .false. ! initialize
!    vbyte = transfer(string, mbyte)
!
!    print *, "In function zeroTerminatedString", vbyte
!
!    if (vbyte==0) zeroTerminatedString = .true.
!
!  end function
 
!-------------------------------------------------------------------------


end module NUOPC_Compliance_Model


!-------------------------------------------------------------------------
! The register routine of internal ICs must be available as an external routine

recursive subroutine NUOPC_Model_ComplianceICR(comp, rc)
  use ESMF
  use NUOPC_Compliance_Model
  implicit none
  type(ESMF_GridComp)   :: comp
  integer               :: rc
  
  call registerIC(comp, rc)   ! simply call the internal IC module's register
  if (ESMF_LogFoundError(rc, &
    line=__LINE__, &
    file=FILENAME)) &
    return  ! bail out
  
end subroutine
