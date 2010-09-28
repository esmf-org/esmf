! $Id: ESMF_AttributeCIMEx.F90,v 1.7 2010/09/28 05:55:58 eschwab Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_AttributeCIMEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: Advanced Attribute usage: CIM Attribute Packages}
! \label{sec:attribute:usage:cimAttPack}
!
! This example illustrates the use of the Metafor CIM Attribute packages,
! supplied by ESMF, to create an Attribute hierarchy on an ESMF object tree.
! A coupler Component and three gridded Components are used together with
! three States, two FieldBundles, and six realistic Fields to create an ESMF
! object tree.  CIM Attributes packages are created on the Components and
! Fields, and then the individual Attributes within the packages are populated
! with values.  Finally, all the Attributes are written to a CIM-formatted
! XML file.
!EOE
!
!  !PROGRAM: ESMF\_AttributeCIMEx - Example of Attribute Package usage.
!
!  !DESCRIPTION: 
!
!  This program shows an example of CIM Attribute usage.

!BOC
      ! Use ESMF framework module
      use ESMF_Mod
      implicit none

      ! Local variables  
      integer                 :: rc, finalrc, petCount, localPet
      type(ESMF_VM)           :: vm
      type(ESMF_Field)        :: DMS_emi, UM, OH, Orog, Ozone, SST
      type(ESMF_FieldBundle)  :: fbundle1, fbundle2
      type(ESMF_State)        :: exportState1, exportState2, exportState3
      type(ESMF_CplComp)      :: cplcomp
      type(ESMF_GridComp)     :: gridcomp1, gridcomp2, gridcomp3
      character(ESMF_MAXSTR)  :: convCIM, purpComp, purpField, purpPlatform
      
      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, defaultlogfilename="AttributeCIMEx.Log", &
                    defaultlogtype=ESMF_LOG_MULTI, rc=rc)
      
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
      if (rc/=ESMF_SUCCESS) goto 10
!EOC
      
      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_AttributeCIMEx Example"
        print *, "--------------------------------------- "
      endif

!BOE
!    Create the ESMF objects that will hold the CIM Attributes.
!    These objects include a coupler Component, three gridded Components,
!    three States (one State per gridded Component) two FieldBundles,
!    and six Fields.  In this example we are constructing empty Fields
!    without an underlying Grid.
!EOE

!BOC
      ! Create Components
      cplcomp = ESMF_CplCompCreate(name="coupler_component", &
          petList=(/0/), rc=rc)
      gridcomp1 = ESMF_GridCompCreate(name="gridded_component1", &
          petList=(/0/), rc=rc)
      gridcomp2 = ESMF_GridCompCreate(name="gridded_component2", &
          petList=(/0/), rc=rc)
      gridcomp3 = ESMF_GridCompCreate(name="gridded_component3", &
          petList=(/0/), rc=rc)

      ! Create States
      exportState1 = ESMF_StateCreate("exportState1", ESMF_STATE_EXPORT, rc=rc)
      exportState2 = ESMF_StateCreate("exportState2", ESMF_STATE_EXPORT, rc=rc)
      exportState3 = ESMF_StateCreate("exportState3", ESMF_STATE_EXPORT, rc=rc)
        
      ! Create Field Bundles
      fbundle1 = ESMF_FieldBundleCreate(name="fbundle1", rc=rc)
      fbundle2 = ESMF_FieldBundleCreate(name="fbundle2", rc=rc)

      ! Create Fields
      DMS_emi = ESMF_FieldCreateEmpty(name='DMS_emi', rc=rc)
      UM = ESMF_FieldCreateEmpty(name='UM', rc=rc)
      OH = ESMF_FieldCreateEmpty(name='OH', rc=rc)
      Orog = ESMF_FieldCreateEmpty(name='Orog', rc=rc)
      Ozone = ESMF_FieldCreateEmpty(name='Ozone', rc=rc)
      SST = ESMF_FieldCreateEmpty(name='SST', rc=rc)
!EOC

!BOE
!    Now add CIM Attribute packages to all of the Components and Fields.
!EOE

!BOC 
      convCIM = 'CIM 1.0'
      purpComp = 'Model Component Simulation Description'
      purpField = 'Inputs Description'
      purpPlatform = 'Platform Description'

      ! Add CIM Attribute package to Components
      !   convention = 'CIM 1.0'
      !   purpose    = 'Model Component Simulation Description'
      call ESMF_AttributeAdd(cplcomp, convention=convCIM, &
        purpose=purpComp, rc=rc)
      call ESMF_AttributeAdd(gridcomp1, convention=convCIM, &
        purpose=purpComp, rc=rc)
      call ESMF_AttributeAdd(gridcomp2, convention=convCIM, &
        purpose=purpComp, rc=rc)
      call ESMF_AttributeAdd(gridcomp3, convention=convCIM, &
        purpose=purpComp, rc=rc)

      ! Add CIM Attribute package to Fields
      !   convention = 'CIM 1.0'
      !   purpose    = 'Inputs Description'
      call ESMF_AttributeAdd(DMS_emi, convention=convCIM, purpose=purpField, &
           rc=rc)
      call ESMF_AttributeAdd(UM, convention=convCIM, purpose=purpField,rc=rc)
      call ESMF_AttributeAdd(OH, convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeAdd(Orog, convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeAdd(Ozone, convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeAdd(SST, convention=convCIM, purpose=purpField, rc=rc)
!EOC  

!BOE
!     The standard Attribute package currently supplied by ESMF for a
!     CIM Component contains several Attributes, grouped into sub-packages.
!     These Attributes conform to the CIM convention as defined by Metafor and
!     their values are set individually.
!EOE

!BOC
      !
      ! Top-level model component attributes, set on coupler
      !
      call ESMF_AttributeSet(cplcomp, 'ComponentShortName', 'HiGEM', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ComponentLongName', &
                             'UK High Resolution Global Environment Model', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ComponentDescription', &
        'HiGEM brings together expertise from NERC, the UK academic ' // &
        'community and the Met Office in a concerted UK effort to ' // &
        'develop coupled climate models with increased horizontal ' // &
        'resolutions. Increasing the horizontal resolution of coupled ' // &
        'climate models will allow us to capture climate processes and ' // &
        'weather systems in much greater detail.', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ReleaseDate', &
        '2009', &
          convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ModelType', &
        'AerosolEmissionAndConc', convention=convCIM, purpose=purpComp, rc=rc)

      ! Simulation run attributes
      call ESMF_AttributeSet(cplcomp, 'SimulationShortName', &
        '1.1_HiGEM_Sim', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationLongName', &
        'HiGEM Simulation for Experiment 1.1', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationRationale', &
       'HiGEM simulation run in repsect to CMIP5 core experiment 1.1 (Decadal)', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationStartDate', &
       '1960-1-1T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationDuration', &
       '10.0 Years', &
        convention=convCIM, purpose=purpComp, rc=rc)

      ! Document genealogy
      call ESMF_AttributeSet(cplcomp, 'PreviousVersion', &
       'HadGEM1 Atmosphere', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'PreviousVersionDescription', &
        'Horizontal resolution increased to 1.25 x 0.83 degrees;&#13; ' // &
        'Timestep reduced from 30 minutes to 20 minutes;&#13; ' // &
        'Magnitude of polar filtering in the advection scheme reduced;&#13; ' // &
        'Vertical velocity threshold at which targeted moisture diffusion ' // &
        'is triggered was increased from 0.1m/s to 0.4m/s;&#13; ' // &
        'Snow-free sea-ice albedo reduced from 0.61 to 0.57;&#13; ' // &
        'Total ocean current included in the calculation of surface ' // &
        'fluxes of heat, moisture, and momentum.', &
        convention=convCIM, purpose=purpComp, rc=rc)

      ! Platform description attributes
      call ESMF_AttributeSet(cplcomp, 'MachineName', &
       'HECToR', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineDescription', &
        'HECToR (Phase 2a) is currently an integrated system known ' // &
        'as Rainier, which includes a scalar MPP XT4 system, a vector ' // &
        'system known as BlackWidow, and storage systems.', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineHardwareType', &
       'Parallel', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineOperatingSystem', &
       'Unicos', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineVendor', &
       'Cray Inc', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineInterconnectType', &
       'Cray Interconnect', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineMaximumProcessors', &
       '22656', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineCoresPerProcessor', &
       '4', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineProcessor', &
       'AMD X86_64', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineCompiler', &
       'Pathscale', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineCompilerVersion', &
       '3.0', &
        convention=convCIM, purpose=purpPlatform, rc=rc)

      ! Responsible party attributes (for Principal Investigator)
      call ESMF_AttributeSet(cplcomp, 'IndividualName', &
       'Gerard Devine', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'PhysicalAddress', &
       'Department of Meteorology University of Reading Earley Gate, Reading Devine', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'EmailAddress', &
       'g.m.devine@reading.ac.uk', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ResponsiblePartyRole', &
       'author', &
        convention=convCIM, purpose=purpComp, rc=rc)

      !
      !  Child component attributes, set on gridcomp1, child of cplcomp
      !
      call ESMF_AttributeSet(gridcomp1, 'ComponentShortName', 'HiGEM_Atmos', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'ComponentLongName', &
                             'Atmosphere component of the HiGEM model', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'ReleaseDate', &
        '2009', &
          convention=convCIM, purpose=purpComp, rc=rc)

      ! Responsible party attributes (for Principal Investigator)
      call ESMF_AttributeSet(gridcomp1, 'IndividualName', &
       'Gerard Devine', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'PhysicalAddress', &
       'Department of Meteorology University of Reading Earley Gate, Reading UK', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'EmailAddress', &
       'g.m.devine@reading.ac.uk', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'ResponsiblePartyRole', &
       'author', &
        convention=convCIM, purpose=purpComp, rc=rc)

      !
      !  Child component attributes, set on gridcomp3, child of gridcomp1
      !
      call ESMF_AttributeSet(gridcomp3, 'ComponentShortName', &
                             'HiGEM AtmosDynCore', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'ComponentLongName', &
                             'Dynamical core of HiGEM_Atmos', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ModelType', &
        'AtmosDynamicalCore', convention=convCIM, purpose=purpComp, rc=rc)

      ! Responsible party attributes (for Principal Investigator)
      call ESMF_AttributeSet(gridcomp3, 'IndividualName', &
       'Gerard Devine', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'PhysicalAddress', &
       'Department of Meteorology University of Reading Earley Gate, Reading UK', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'EmailAddress', &
       'g.m.devine@reading.ac.uk', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'ResponsiblePartyRole', &
       'author', &
        convention=convCIM, purpose=purpComp, rc=rc)

      !
      !  Child component attributes, set on gridcomp2, child of cplcomp
      !
      call ESMF_AttributeSet(gridcomp2, 'ComponentShortName', 'HiGEM_AtmosChem', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'ComponentLongName', &
                             'Atmospheric chemistry component of HiGEM', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ModelType', &
        'AtmosphericChemistry', convention=convCIM, purpose=purpComp, rc=rc)

      ! Responsible party attributes (for Principal Investigator)
      call ESMF_AttributeSet(gridcomp2, 'IndividualName', &
       'Gerard Devine', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'PhysicalAddress', &
       'Department of Meteorology University of Reading Earley Gate, Reading UK', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'EmailAddress', &
       'g.m.devine@reading.ac.uk', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'ResponsiblePartyRole', &
       'author', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC

!BOE
!     The standard Attribute package currently supplied by ESMF for 
!     CIM Fields contains a standard CF-Extended package nested within it.
!EOE

!BOC
      ! DMS_emi CF-Extended Attributes
      call ESMF_AttributeSet(DMS_emi, 'VariableShortName', 'DMS_emi', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'VariableStandardName', 'DMS_emissions', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'VariableLongName', 'DMS emissions', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'VariableUnits', 'unknown', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! DMS_emi CIM Attributes
      call ESMF_AttributeSet(DMS_emi, 'InputType', 'boundaryCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'InputTargetComponent', &
                                      'HiGEM_AtmosChem', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'InputSpatialRegriddingMethod', &
                                      'conservativeSpatialRegridding', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'InputSpatialRegriddingType', 'TBD', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'InputFrequency', '15 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'InputTimeTransformationType', &
                                      'TimeAverage', &
           convention=convCIM, purpose=purpField, rc=rc)

      ! UM CF-Extended Attributes
      call ESMF_AttributeSet(UM, 'VariableShortName', 'UM_Initial_1960', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! UM CIM Attributes
      call ESMF_AttributeSet(UM, 'InputType', 'initialCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(UM, 'InputTargetComponent', &
                                      'HiGEM_AtmosChem', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(UM, 'InputTimeTransformationType', 'Exact', &
           convention=convCIM, purpose=purpField, rc=rc)
    
      ! OH CF-Extended Attributes
      call ESMF_AttributeSet(OH, 'VariableShortName', 'OH_Conc_1900', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'VariableStandardName', &
                                 'OH_Concentrations', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'VariableLongName', &
                                 'seasonal_oxidant_conc', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'VariableUnits', 'unknown', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! OH CIM Attributes
      call ESMF_AttributeSet(OH, 'InputType', 'boundaryCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'InputTargetComponent', &
                                 'HiGEM_AtmosChem', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'InputSpatialRegriddingMethod', &
                                 'conservativeSpatialRegridding', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'InputSpatialRegriddingType', 'TBD', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'InputFrequency', '15 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'InputTimeTransformationType', &
                                 'TimeInterpolation', &
           convention=convCIM, purpose=purpField, rc=rc)
    
      ! Orog CF-Extended Attributes
      call ESMF_AttributeSet(Orog, 'VariableShortName', 'UM_Orog_n320', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'VariableStandardName', 'Height', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'VariableLongName', 'Orography', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'VariableUnits', 'unknown', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! Orog CIM Attributes
      call ESMF_AttributeSet(Orog, 'InputType', 'initialCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'InputTargetComponent', &
                                   'HiGEM_Atmos', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'InputTimeTransformationType', 'Exact', &
           convention=convCIM, purpose=purpField, rc=rc)
    
      ! Ozone CF-Extended Attributes
      call ESMF_AttributeSet(Ozone, 'VariableShortName', 'Global_O3_mon', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'VariableStandardName', 'Ozone', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'VariableLongName', 'Ozone', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'VariableUnits', 'unknown', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! Ozone CIM Attributes
      call ESMF_AttributeSet(Ozone, 'InputType', 'boundaryCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'InputTargetComponent', &
                                    'HiGEM_Atmos', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'InputSpatialRegriddingMethod', &
                                    'conservativeSpatialRegridding', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'InputSpatialRegriddingType', 'TBD', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'InputFrequency', '15 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'InputTimeTransformationType', &
                                    'TimeInterpolation', &
           convention=convCIM, purpose=purpField, rc=rc)
    
      ! SST CF-Extended Attributes
      call ESMF_AttributeSet(SST, 'VariableShortName', 'SST', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! SST CIM Attributes
      call ESMF_AttributeSet(SST, 'InputType', 'initialCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'InputTargetComponent', &
                                  'HiGEM_Atmos', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'InputSpatialRegriddingMethod', &
                                  'conservativeSpatialRegridding', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'InputSpatialRegriddingType', 'TBD', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'InputFrequency', '15 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'InputTimeTransformationType', &
                                  'TimeAverage', &
           convention=convCIM, purpose=purpField, rc=rc)
!EOC  

!BOE
!     Adding the Fields to the FieldBundles will automatically link the 
!     Attribute hierarchies.  The same type of link will be generated
!     when adding a FieldBundle to a State.
!EOE

!BOC
      ! Add two Fields to the first FieldBundle,
      !  which in turn is added to the first State
      call ESMF_FieldBundleAdd(fbundle1, DMS_emi, rc=rc)
      call ESMF_FieldBundleAdd(fbundle1, UM, rc=rc)
      call ESMF_StateAdd(exportState1, fieldbundle=fbundle1, rc=rc)

      ! Add two Fields to the second FieldBundle,
      !  which in turn is added to the second State
      call ESMF_FieldBundleAdd(fbundle2, OH, rc=rc)
      call ESMF_FieldBundleAdd(fbundle2, Orog, rc=rc)
      call ESMF_StateAdd(exportState2, fieldbundle=fbundle2, rc=rc)

      ! Add the remaining two Fields directly to the third State,
      !  without a FieldBundle
      call ESMF_StateAdd(exportState3, field=Ozone, rc=rc)
      call ESMF_StateAdd(exportState3, field=SST, rc=rc)
!EOC

!BOE
!     The Attribute link between a State and the Component, and between
!     Components, must be set manually.
!EOE

!BOC
      ! Link States to the gridded Components
      call ESMF_AttributeLink(gridcomp1, exportState1, rc=rc)
      call ESMF_AttributeLink(gridcomp2, exportState2, rc=rc)
      call ESMF_AttributeLink(gridcomp3, exportState3, rc=rc)

      ! Gridded Component 1 and gridded Component 2 are children of the coupler
      call ESMF_AttributeLink(cplcomp, gridcomp1, rc=rc)
      call ESMF_AttributeLink(cplcomp, gridcomp2, rc=rc)
      ! Gridded Component 3 is a child of gridded Component 1 (grandchild of 
      ! the coupler)
      call ESMF_AttributeLink(gridcomp1, gridcomp3, rc=rc)
!EOC

!BOE
!     Write the entire CIM Attribute hierarchy, beginning at the coupler
!     Component (the top), to an XML file formatted to conform to CIM
!     specifications.  The CIM output tree structure differs from the
!     internal Attribute hierarchy in that it has all the attributes of
!     the fields within its top-level <modelComponent> record.  The filename
!     used, coupler\_component.xml, is derived from the name of the coupler
!     Component, given as an input argument in the ESMF\_CplCompCreate()
!     call above.  The file is written to the examples execution directory.
!EOE

      if (localPet==0) then
!BOC
      call ESMF_AttributeWrite(cplcomp, convCIM, purpComp, &
        attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
!EOC
        if (rc/=ESMF_SUCCESS) goto 10
      endif

      ! Clean-up
      call ESMF_FieldDestroy(field=SST, rc=rc)
      call ESMF_FieldDestroy(field=Ozone, rc=rc)
      call ESMF_FieldDestroy(field=Orog, rc=rc)
      call ESMF_FieldDestroy(field=OH, rc=rc)
      call ESMF_FieldDestroy(field=UM, rc=rc)
      call ESMF_FieldDestroy(field=DMS_emi, rc=rc)
      call ESMF_FieldBundleDestroy(fbundle2, rc=rc)
      call ESMF_FieldBundleDestroy(fbundle1, rc=rc)
      call ESMF_StateDestroy(exportState3, rc=rc)
      call ESMF_StateDestroy(exportState2, rc=rc)
      call ESMF_StateDestroy(exportState1, rc=rc)
      call ESMF_GridCompDestroy(gridcomp3, rc=rc)
      call ESMF_GridCompDestroy(gridcomp2, rc=rc)
      call ESMF_GridCompDestroy(gridcomp1, rc=rc)
      call ESMF_CplCompDestroy(cplcomp, rc=rc)

      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "End of ESMF_AttributeCIMEx Example"
        print *, "--------------------------------------- "
      endif

      call ESMF_Finalize(rc=rc)

10    continue
      if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
      call ESMF_Finalize(rc=rc)
  
      if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
      if (finalrc==ESMF_SUCCESS) then
        print *, "PASS: ESMF_AttributeCIMEx.F90"
      else
        print *, "FAIL: ESMF_AttributeCIMEx.F90"
      endif
  
end program
