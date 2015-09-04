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
  use ESMF
    
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
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
      userRoutine=user_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
      userRoutine=user_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
      userRoutine=user_final, rc=rc)
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
	  type(ESMF_AttPack)        :: attpack
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
    convCIM = 'CIM 1.7.1'
    purpComp = 'ModelComp'
    purpProp = 'CompProp'

    convISO = 'ISO 19115'
    purpRP = 'RespParty'
    purpCitation = 'Citation'
    purpPlatform = 'Platform'

    nestConv(1) = convISO
    nestPurp(1) = purpRP
    nestConv(2) = convISO
    nestPurp(2) = purpCitation

    ! Add CIM Attribute package to top-level coupler component, 
    !  containing a variable number of Responsible Party and 
    !  Citation sub-packages
    !   convention = 'CIM 1.7.1'
    !   purpose    = 'ModelComp'
    !   nestConvention(1) = 'ISO 19115'
    !   nestPurpose(1)    = 'RespParty'
    !   nestConvention(2) = 'ISO 19115'
    !   nestPurpose(2)    = 'Citation'

    ! Specify the top-level Coupler Component to have 3 Responsible Party
    !   sub-packages and 2 Citation sub-packages
    nameCount = 0
    call ESMF_AttributeAdd(comp, convention=convCIM, &
      purpose=purpComp, nestConvention=nestConv, nestPurpose=nestPurp, &
      nestAttPackInstanceCountList=(/3,2/), &
      nestAttPackInstanceNameList=nestAttPackName, &
      nestCount=2, nestAttPackInstanceNameCount=nameCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Specify the top-level Coupler Component to have a Component Properties
    ! package with some custom attributes
    compPropAtt(1) = 'SimulationType'
    compPropAtt(2) = 'SimulationURL'
    compPropAtt(3) = 'Visualization'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpProp, &
      attrList=compPropAtt, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'ShortName', 'EarthSys', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  1) Name of component in navigator bar on the left; 
    !                  attribute 'Version' appended, if set.
    !               2) Also "Simulation Metadata:", for top-level component, 
    !                  first part of display, at top, 1st line, prepended to
    !                  top-level component's attributes 'Version' (if set) and
    !                  'SimulationShortName'.

    call ESMF_AttributeSet(comp, 'LongName', &
                           'Earth System High Resolution Global Model', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Full Name:"  first part of display, at top, 2nd line 
    !               under title, prepended to attribute 'SimulationLongName'.

    call ESMF_AttributeSet(comp, 'Description', &
      'EarthSys brings together expertise from the global ' // &
      'community in a concerted effort to develop coupled ' // &
      'climate models with increased horizontal resolutions.  ' // &
      'Increasing the horizontal resolution of coupled climate ' // &
      'models will allow us to capture climate processes and ' // &
      'weather systems in much greater detail.', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Description:" in top box.

    call ESMF_AttributeSet(comp, 'Version', &
      '2.0', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Appended to attribute 'ShortName', then displayed as name 
    !               of component in navigator bar on the left.

    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2009-01-01T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Release Date" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'ModelType', &
      'model', convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Maps to "Realm:", expanded under component name, in 
    !               navigator bar on the left.

    call ESMF_AttributeSet(comp, 'URL', &
      'www.earthsys.org', convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'MetadataVersion', &
      '1.2', convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Metadata Version" under tabs "Properties->Basic".


    ! Simulation run attributes
    call ESMF_AttributeSet(comp, 'SimulationShortName', &
      'SMS.f09_g16.X.hector', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Simulation Metadata:"  1st part of display, at top, 
    !               1st line, appended to top-level component's attributes 
    !               'ShortName' and 'Version'.  Similarly, shows up as the 
    !               2nd part of the simulation name when searching the
    !               ESG website for Simulations->Realm->Earth System.

    call ESMF_AttributeSet(comp, 'SimulationLongName', &
      'EarthSys - Earth System Modeling Framework Earth System Model 1.0', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Full Name:"  1st part of display, at top, 2nd line under 
    !               title, appended to attribute 'LongName'.

    call ESMF_AttributeSet(comp, 'SimulationProjectName', &
      'CMIP5', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Project" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'SimulationEnsembleID', &
      'a1b1c1', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Ensemble Identification" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'SimulationRationale', &
'EarthSys-ESMF simulation run in repsect to CMIP5 core experiment 1.1 ()', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'SimulationStartDate', &
     '1960-01-01T00:00:00Z', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Simulation Start Date" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'SimulationDuration', &
     'P10Y', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Simulation Duration" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'SimulationEndDate', &
     '1970-01-01T00:00:00Z', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Simulation End Date" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'SimulationNumberOfProcessingElements', &
     '16', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! Document genealogy
    call ESMF_AttributeSet(comp, 'PreviousVersion', &
     'HadGEM1 Atmosphere', &
      convention=convCIM, purpose=purpComp, rc=rc)
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'PreviousVersionDescription', &
     'Horizontal resolution increased to 1.20 x 0.80 degrees; ' // &
     'Timestep reduced from 30 minutes to 15 minutes.', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! Platform description attributes
    call ESMF_AttributeGetAttPack(comp, convCIM, purpPlatform, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'CompilerName', &
     'Pathscale', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Compiler", concatenated with attribute 'CompilerVersion',
    !               under tabs "Properties->Technical".

    call ESMF_AttributeSet(comp, 'CompilerVersion', &
     '3.0', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Concatenated to attribute 'CompilerName' and displayed as 
    !               "Compiler" under tabs "Properties->Technical".

    call ESMF_AttributeSet(comp, 'MachineName', &
     'HECToR', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Machine Name" under tabs "Properties->Technical".

    call ESMF_AttributeSet(comp, 'MachineDescription', &
     'HECToR (Phase 2a) is currently an integrated system known ' // &
     'as Rainier, which includes a scalar MPP XT4 system, a vector ' // &
     'system known as BlackWidow, and storage systems.', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'MachineSystem', &
     'Parallel', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Hardware Type" under tabs "Properties->Technical".

    call ESMF_AttributeSet(comp, 'MachineOperatingSystem', &
     'Unicos', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Operating System" under tabs "Properties->Technical".

    call ESMF_AttributeSet(comp, 'MachineVendor', &
     'Cray Inc', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    ! ESG Display:  Ingested, but not displayed, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'MachineInterconnectType', &
     'Cray Interconnect', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Interconnect Type" under tabs "Properties->Technical".

    call ESMF_AttributeSet(comp, 'MachineMaximumProcessors', &
     '22656', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Maximum Processors" under tabs "Properties->Technical".

    call ESMF_AttributeSet(comp, 'MachineCoresPerProcessor', &
     '4', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Number of Cores per Processor" under tabs 
    !               "Properties->Technical".

    call ESMF_AttributeSet(comp, 'MachineProcessorType', &
     'AMD X86_64', &
      convention=convCIM, purpose=purpPlatform, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Processor" under tabs "Properties->Technical".


    ! Component Properties: custom attributes
    call ESMF_AttributeGetAttPack(comp, convCIM, purpProp, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'SimulationType', &
     'branch', &
      convention=convCIM, purpose=purpProp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'SimulationURL', &
     'http://earthsys.org/simulations', &
      convention=convCIM, purpose=purpProp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'Visualization', &
     'true', &
      convention=convCIM, purpose=purpProp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! Set the values of the 3 Responsible Party sub-packages, created above
    ! for the Coupler Component in the ESMF\_AttributeAdd(comp, ...) call.

    ! Responsible party attributes (for Principal Investigator)
    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'Name', &
     'John Doe', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Principal Investigator" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'Abbreviation', &
     'JD', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Meteorology, University of ABC', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'john.doe@earthsys.org', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Ingested, but not displayed, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'PI', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Ingested, but only used to control display.

    call ESMF_AttributeSet(comp, 'URL', &
     'www.earthsys.org', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! Responsible party attributes (for Contact)
    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, &
      attPackInstanceName=nestAttPackName(2), attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'Name', &
     'Samuel Doe', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Contact Name" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'Abbreviation', &
     'SD', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Meteorology, University of ABC', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'samuel.doe@earthsys.org', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Contact Email" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'Contact', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Ingested, but only used to control display.

    call ESMF_AttributeSet(comp, 'URL', &
     'www.earthsys.org', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(2), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! Responsible party attributes (for Funder)
    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, &
      attPackInstanceName=nestAttPackName(3), attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'Name', &
     'EarthSys Funding Office', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(3), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Funding Source" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Oceanography, University of GHI', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(3), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'sally.doe@earthsys.org', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(3), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'Funder', &
      convention=convISO, purpose=purpRP, &
      attPackInstanceName=nestAttPackName(3), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Ingested, but only used to control display.


    ! Set the values of the 2 Citation sub-packages, created above
    ! for the Coupler Component in the ESMF\_AttributeAdd(comp, ...) call.

    ! Citation attributes (1st Citation attribute package)
    call ESMF_AttributeGetAttPack(comp, convISO, purpCitation, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'ShortTitle', &
     'Doe_2009', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'LongTitle', &
     'Doe, J.A.; Doe, S.B.; ' // &
     'Doe, J.C.; 2009 EarthSys: ' // &
     'The Earth System High Resolution Global Model - ' // &
     'Coupled Atmosphere-Ocean model description and basic evaluation. ' // &
     'Journal of Earth Modeling, 15 (2). 1261-1296.', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Reference", concatenated with attribute 'DOI', under 
    !               tab "References".

    call ESMF_AttributeSet(comp, 'Date', &
     '2009-03-05', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'PresentationForm', &
     'Online Refereed', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'DOI', &
     'doi:17.1035/2009JCLI4508.1', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Concatenated to attribute 'LongTitle' and displayed as 
    !               "Reference" under tab "References".

    call ESMF_AttributeSet(comp, 'URL', &
     'http://www.earthsys.org/publications', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not output to CIM, as of v1.5/1.7 (no definition for it). 


    ! Citation attributes (2nd Citation attribute package)
    !  note:  nestAttPackName(5) refers to the 2nd
    !         nested Citation attribute package, after
    !         the 3 Responsible Party packages and 
    !         the 1st Citation package (nestAttPackName(4)).
    !         nestAttPackName(x) is not needed (optional)
    !         when referring to the 1st nested attribute package
    !         of either a Responsible Party or a Citation.
    call ESMF_AttributeGetAttPack(comp, convISO, purpCitation, &
      attPackInstanceName=nestAttPackName(5), attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'ShortTitle', &
     'Doe_2006', &
      convention=convISO, purpose=purpCitation, &
      attPackInstanceName=nestAttPackName(5), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'LongTitle', &
     'Doe, J.A.; Doe, S.B.; ' // &
     '2006 EarthSys: ' // &
     'The Earth System High Resolution Global Model - ' // &
     'Improvements in Atmosphere and Ocean modeling. ' // &
     'Journal of Earth Modeling, 11 (3). 1021-1036.', &
      convention=convISO, purpose=purpCitation, &
      attPackInstanceName=nestAttPackName(5), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Reference", concatenated with attribute 'DOI', under 
    !               tab "References".

    call ESMF_AttributeSet(comp, 'Date', &
     '2006-10-21', &
      convention=convISO, purpose=purpCitation, &
      attPackInstanceName=nestAttPackName(5), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'PresentationForm', &
     'Online Refereed', &
      convention=convISO, purpose=purpCitation, &
      attPackInstanceName=nestAttPackName(5), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'DOI', &
     'doi:11.1234/2006JCLI1357.1', &
      convention=convISO, purpose=purpCitation, &
      attPackInstanceName=nestAttPackName(5), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Concatenated to attribute 'LongTitle' and displayed as 
    !               "Reference" under tab "References".

    call ESMF_AttributeSet(comp, 'URL', &
     'http://www.earthsys.org/publications', &
      convention=convISO, purpose=purpCitation, &
      attPackInstanceName=nestAttPackName(5), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not output to CIM, as of v1.5/1.7 (no definition for it). 


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
