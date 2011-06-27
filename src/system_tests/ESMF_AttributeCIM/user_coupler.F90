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
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    if (pthreadsEnabled) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif

  end subroutine

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc
    ! Initialize return code
    rc = ESMF_SUCCESS
    
    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
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
    character(ESMF_MAXSTR) :: convCIM, purpComp, purpProp, purpPlatform
    character(ESMF_MAXSTR) :: convISO, purpRP, purpCitation
    character(ESMF_MAXSTR), dimension(2)  :: nestConv, nestPurp
    character(ESMF_MAXSTR), dimension(5)  :: nestAttPackName
    character(ESMF_MAXSTR), dimension(3)  :: compPropAtt
    integer :: nameCount

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Need to reconcile import and export states across the VM, including
    ! Attributes set on the Gridded Components (especially the links between
    ! the States, Field Bundles, and Fields).
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(importState, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
!    call ESMF_StateReconcile(exportState, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
!    if (rc/=ESMF_SUCCESS) return ! bail out
                                  
    ! Create the CIM Attribute package on the Coupler Component and set its
    ! values.  The standard Attribute package currently supplied by ESMF for a
    ! CIM Component contains several Attributes, grouped into sub-packages.
    ! These Attributes conform to the CIM convention as defined by Metafor and
    ! their values are set individually.

    !
    ! Top-level model component attributes, set on coupler
    !
    convCIM = 'CIM 1.5'
    purpComp = 'Model Component Simulation Description'
    purpProp = 'General Component Properties Description'

    convISO = 'ISO 19115'
    purpRP = 'Responsible Party Description'
    purpCitation = 'Citation Description'
    purpPlatform = 'Platform Description'

    nestConv(1) = convISO
    nestPurp(1) = purpRP
    nestConv(2) = convISO
    nestPurp(2) = purpCitation

    ! Add CIM Attribute package to top-level coupler component, 
    !  containing a variable number of Responsible Party and 
    !  Citation sub-packages
    !   convention = 'CIM 1.5'
    !   purpose    = 'Model Component Simulation Description'
    !   nestConvention(1) = 'ISO 19115'
    !   nestPurpose(1)    = 'Responsible Party Description'
    !   nestConvention(2) = 'ISO 19115'
    !   nestPurpose(2)    = 'Citation Description'

    ! Specify the top-level Coupler Component to have 2 Responsible Party
    !   sub-packages and 1 Citation sub-package
    nameCount = 0
    call ESMF_AttributeAdd(comp, convention=convCIM, &
      purpose=purpComp, nestConvention=nestConv, nestPurpose=nestPurp, &
      nestAttPackInstanceCountList=(/2,1/), &
      nestAttPackInstanceNameList=nestAttPackName, &
      nestCount=2, nestAttPackInstanceNameCount=nameCount, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Specify the top-level Coupler Component to have a Component Properties
    ! package with some custom attributes
    compPropAtt(1) = 'SimulationType'
    compPropAtt(2) = 'SimulationURL'
    compPropAtt(3) = 'Visualization'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpProp, &
      attrList=compPropAtt, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeSet(comp, 'ShortName', 'HiGEM', &
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

    call ESMF_AttributeSet(comp, 'Version', &
      '2.0', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2009-01-01T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ModelType', &
      'AerosolEmissionAndConc', convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'URL', &
      'www.nerc.ac.uk', convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Simulation run attributes
    call ESMF_AttributeSet(comp, 'SimulationShortName', &
      'SMS.f09_g16.X.bluefire', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationLongName', &
      'HiGEM - Earth System Modeling Framework Earth System Model 1.0', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationRationale', &
'HiGEM-ESMF simulation run in repsect to CMIP5 core experiment 1.1 ()', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationStartDate', &
     '1960-01-01T00:00:00Z', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationDuration', &
     'P10Y', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationNumberOfProcessingElements', &
     '16', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationMetadataVersion', &
     '1.0', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Document genealogy
    call ESMF_AttributeSet(comp, 'PreviousVersion', &
     'HadGEM1 Atmosphere', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'PreviousVersionDescription', &
     'Horizontal resolution increased to 1.25 x 0.83 degrees; ' // &
     'Timestep reduced from 30 minutes to 20 minutes; ' // &
     'Magnitude of polar filtering in the advection scheme reduced; ' // &
     'Vertical velocity threshold at which targeted moisture diffusion ' // &
     'is triggered was increased from 0.1m/s to 0.4m/s; ' // &
     'Snow-free sea-ice albedo reduced from 0.61 to 0.57; ' // &
     'Total ocean current included in the calculation of surface ' // &
     'fluxes of heat, moisture, and momentum.', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Platform description attributes
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
    if (rc .ne. ESMF_SUCCESS) return

    ! Component Properties: custom attributes
    call ESMF_AttributeSet(comp, 'SimulationType', &
     'branch', &
      convention=convCIM, purpose=purpProp, rc=rc)
    call ESMF_AttributeSet(comp, 'SimulationURL', &
     'http://higem.nerc.ac.uk', &
      convention=convCIM, purpose=purpProp, rc=rc)
    call ESMF_AttributeSet(comp, 'Visualization', &
     'true', &
      convention=convCIM, purpose=purpProp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Set the values of the 2 Responsible Party sub-packages, created above
    ! for the Coupler Component in the ESMF\_AttributeAdd(comp, ...) call.

    ! Responsible party attributes (for Principal Investigator)
    call ESMF_AttributeSet(comp, 'Name', &
     'Gerard Devine', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'Abbreviation', &
     'GMD', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Meteorology University of Reading Earley Gate, ' // &
     'Reading Devine', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'g.m.devine@reading.ac.uk', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'PI', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'URL', &
     'www.epcc.ed.ac.uk', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Responsible party attributes (for Author)
    call ESMF_AttributeSet(comp, 'Name', &
     'CESM Atmosphere Model Working Group', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'Abbreviation', &
     'AMWG', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'NameType', &
     'Organization', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Climate and Global Dynamics Division, National Center for ' // &
     'Atmospheric Research, Boulder, Colorado', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'hannay@ucar.edu', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'Author', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    call ESMF_AttributeSet(comp, 'URL', &
     'http://www.cesm.ucar.edu/working_groups/Atmosphere', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2),rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Set the values of the 1 Citation sub-package, created above
    ! for the Coupler Component in the ESMF\_AttributeAdd(comp, ...) call.

    ! Citation attributes
    call ESMF_AttributeSet(comp, 'ShortTitle', &
     'Shaffrey_2009', &
      convention=convISO, purpose=purpCitation, rc=rc)
    call ESMF_AttributeSet(comp, 'LongTitle', &
     'Shaffrey, L.C.; Norton, W.A.; Vidale, P.L.; Demory, M.E.; ' // &
     'Donners, J.; Cole, J.W.; Wilson, S.S.; Slingo, J.M.; ' // &
     'Steenman-Clark, L.; Stevens, I.; Stevens, D.P.; Roberts, M.J.; ' // &
     'Clayton, A.; Johns, T.C.; Martin, G.M.; Harle, J.D.; New, A.L.; ' // &
     'Jrrar, A.; Connolley, W.M.; King, J.C.; Woodage, J.; Slingo, A.; ' // &
     'Clark, D.B.; Davies, T.M.; Iwi, A.M.. 2009 UK-HiGEM: ' // &
     'The New U.K. High Resolution Global Environment Model - Model ' // &
     'description and basic evaluation. Journal of Climate, 22 (8). ' // &
     '1861-1896.', &
      convention=convISO, purpose=purpCitation, rc=rc)
    call ESMF_AttributeSet(comp, 'Date', &
     '2009-03-05', &
      convention=convISO, purpose=purpCitation, rc=rc)
    call ESMF_AttributeSet(comp, 'PresentationForm', &
     'Online Refereed', &
      convention=convISO, purpose=purpCitation, rc=rc)
    call ESMF_AttributeSet(comp, 'DOI', &
     'doi:10.1175/2008JCLI2508.1', &
      convention=convISO, purpose=purpCitation, rc=rc)
    call ESMF_AttributeSet(comp, 'URL', &
     'http://www.ecmwf.int', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

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
