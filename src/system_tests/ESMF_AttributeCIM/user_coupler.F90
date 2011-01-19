!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Coupler
!
!
!\begin{verbatim}

module user_coupler

  ! ESMF Framework module
  use ESMF_Mod
    
  implicit none
  
  private
   
  public usercpl_setvm, usercpl_register
  
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine usercpl_setvm(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc
#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for
    ! your own code development you probably don't want to include the
    ! following call unless you are interested in exploring ESMF's
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc
    ! Initialize return code
    rc = ESMF_SUCCESS
    
    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)          :: vm
    character(ESMF_MAXSTR) :: convCIM, purpComp, purpPlatform
    character(ESMF_MAXSTR) :: convISO, purpRP

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Need to reconcile import and export states across the VM, including
    ! Attributes set on the Gridded Components (especially the links between
    ! the States, Field Bundles, and Fields).
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(importState, vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
!    call ESMF_StateReconcile(exportState, vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
!    if (rc/=ESMF_SUCCESS) return ! bail out
                                  
    ! Create the CIM Attribute package on the Coupler Component and set its
    ! values.  The standard Attribute package currently supplied by ESMF for a
    ! CIM Component contains several Attributes, grouped into sub-packages.
    ! These Attributes conform to the CIM convention as defined by Metafor and
    ! their values are set individually.

    !
    ! Top-level model component attributes, set on coupler
    !
    convCIM = 'CIM 1.0'
    purpComp = 'Model Component Simulation Description'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpComp, rc=rc)

    call ESMF_AttributeSet(comp, 'ShortName', &
      'HiGEM', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'LongName', &
      'UK High Resolution Global Environment Model', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'Description', &
      'HiGEM brings together expertise from NERC, the UK academic ' // &
      'community and the Met Office in a concerted UK effort to ' // &
      'develop coupled climate models with increased horizontal ' // &
      'resolutions. Increasing the horizontal resolution of coupled ' // &
      'climate models will allow us to capture climate processes and ' // &
      'weather systems in much greater detail.', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2009-01-01T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ModelType', &
      'AerosolEmissionAndConc', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Simulation run attributes
    call ESMF_AttributeSet(comp, 'SimulationShortName', &
      'ESMF_ESM1', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationLongName', &
      'Earth System Modeling Framework Earth System Model 1.0', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationRationale', &
      'ESMF ESM1 simulation run in repsect to CMIP5 core experiment 1.1 (Decadal)',&
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationStartDate', &
      '1960-1-1T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationDuration', &
      '10.0 Years', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Document genealogy
    call ESMF_AttributeSet(comp, 'PreviousVersion', &
     'HadGEM1 Atmosphere', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'PreviousVersionDescription', &
      'Horizontal resolution increased to 1.25 x 0.83 degrees;&#13; ' // &
      'Timestep reduced from 30 minutes to 20 minutes;&#13; ' // &
      'Magnitude of polar filtering in the advection scheme reduced;&#13; ' // &
      'Vertical velocity threshold at which targeted moisture diffusion ' // &
      'is triggered was increased from 0.1m/s to 0.4m/s;&#13; ' // &
      'Snow-free sea-ice albedo reduced from 0.61 to 0.57;&#13; ' // &
      'Total ocean current included in the calculation of surface ' // &
      'fluxes of heat, moisture, and momentum.', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Platform description attributes
    purpPlatform = 'Platform Description'
    call ESMF_AttributeSet(comp, 'CompilerName', &
      'Pathscale', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'CompilerVersion', &
      '3.0', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'MachineName', &
      'HECToR', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'MachineDescription', &
      'HECToR (Phase 2a) is currently an integrated system known ' // &
      'as Rainier, which includes a scalar MPP XT4 system, a vector ' // &
      'system known as BlackWidow, and storage systems.', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'MachineSystem', &
      'Parallel', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'MachineOperatingSystem', &
      'Unicos', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'MachineVendor', &
      'Cray Inc', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'MachineInterconnectType', &
      'Cray Interconnect', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'MachineMaximumProcessors', &
      '22656', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'MachineCoresPerProcessor', &
      '4', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    call ESMF_AttributeSet(comp, 'MachineProcessorType', &
      'AMD X86_64', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Responsible party attributes (for Principal Investigator)
    convISO = 'ISO 19115'
    purpRP = 'Responsible Party Description'
    call ESMF_AttributeSet(comp, 'Name', &
      'Gerard Devine', &
        convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
      'Department of Meteorology University of Reading Earley Gate, Reading Devine', &
        convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'EmailAddress', &
      'g.m.devine@reading.ac.uk', &
        convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
      'Author', &
        convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS
    
  end subroutine user_final

end module user_coupler
    
!\end{verbatim}
