! $Id: ESMF_AttributeCIMEx.F90,v 1.27 2011/05/11 05:57:30 eschwab Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
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
! \subsubsection{CIM Attribute packages}
! \label{sec:attribute:usage:cimAttPack}
!
! This example illustrates the use of the Metafor CIM Attribute packages,
! supplied by ESMF, to create an Attribute hierarchy on an ESMF object tree.
! A coupler Component and four gridded Components are used together with
! four States, two FieldBundles, and eight realistic Fields to create an ESMF
! object tree.  CIM Attributes packages are created on the Components and
! Fields, and then the individual Attributes within the packages are populated
! with values.  Finally, all the Attributes are written to a CIM-formatted
! XML file.
!EOE

#include "ESMF.h"

!-----------------------------------------------------------------------------
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
      integer                 :: rc, finalrc, petCount, localPet, nameCount
      type(ESMF_VM)           :: vm
      type(ESMF_Field)        :: DMS_emi, UM, OH, Orog, Ozone, SST, SO2, NOx
      type(ESMF_FieldBundle)  :: fbundle1, fbundle2
      type(ESMF_State)        :: exportState1, exportState2, exportState3
      type(ESMF_State)        :: exportState4
      type(ESMF_CplComp)      :: cplcomp
      type(ESMF_GridComp)     :: gridcomp1, gridcomp2, gridcomp3, gridcomp4
      character(ESMF_MAXSTR)  :: convCIM, purpComp, purpProp
      character(ESMF_MAXSTR)  :: purpField, purpPlatform
      character(ESMF_MAXSTR)  :: convISO, purpRP, purpCitation
      character(ESMF_MAXSTR), dimension(2)  :: nestConv, nestPurp
      character(ESMF_MAXSTR), dimension(5)  :: nestAttPackName
      character(ESMF_MAXSTR), dimension(8)  :: compPropAtt
      
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
!    These objects include a coupler Component, four gridded Components,
!    four States (one State per gridded Component) two FieldBundles,
!    and eight Fields.  In this example we are constructing empty Fields
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
      gridcomp4 = ESMF_GridCompCreate(name="gridded_component4", &
          petList=(/0/), rc=rc)

      ! Create States
      exportState1 = ESMF_StateCreate(name="exportState1",  &
                                      stateType=ESMF_STATE_EXPORT, &
                                      rc=rc)
      exportState2 = ESMF_StateCreate(name="exportState2",  &
                                      stateType=ESMF_STATE_EXPORT, &
                                      rc=rc)
      exportState3 = ESMF_StateCreate(name="exportState3",  &
                                      stateType=ESMF_STATE_EXPORT, &
                                      rc=rc)
      exportState4 = ESMF_StateCreate(name="exportState4",  &
                                      stateType=ESMF_STATE_EXPORT, &
                                      rc=rc)
        
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
      SO2 = ESMF_FieldCreateEmpty(name='SO2', rc=rc)
      NOx = ESMF_FieldCreateEmpty(name='NOx', rc=rc)
!EOC

!BOE
!    Now add CIM Attribute packages to all of the Components and Fields.  For 
!    the top-level Coupler Component, add a CIM package with 2 Responsible 
!    Party sub-packages and 1 Citation sub-package.  Also, add a CIM Component
!    Properties package, containing custom attributes.  For the Gridded 
!    Components, add a CIM package for each, with the default of 1 Responsible
!    Party sub-package and 1 Citation sub-package.
!EOE

!BOC 
      convCIM = 'CIM 1.0'
      purpComp = 'Model Component Simulation Description'
      purpProp = 'General Component Properties Description'

      purpField = 'Inputs Description'
      purpPlatform = 'Platform Description'

      convISO = 'ISO 19115'
      purpRP = 'Responsible Party Description'
      purpCitation = 'Citation Description'

      nestConv(1) = convISO
      nestPurp(1) = purpRP
      nestConv(2) = convISO
      nestPurp(2) = purpCitation

      ! Add CIM Attribute package to Components, containing a variable number
      !   of Responsible Party and Citation sub-packages
      !   convention = 'CIM 1.0'
      !   purpose    = 'Model Component Simulation Description'
      !   nestConvention(1) = 'ISO 19115'
      !   nestPurpose(1)    = 'Responsible Party Description'
      !   nestConvention(2) = 'ISO 19115'
      !   nestPurpose(2)    = 'Citation Description'

      ! Specify the top-level Coupler Component to have 2 Responsible Party
      !   sub-packages and 1 Citation sub-package
      nameCount = 0
      call ESMF_AttributeAdd(cplcomp, convention=convCIM, &
        purpose=purpComp, nestConvention=nestConv, nestPurpose=nestPurp, &
        nestAttPackInstanceCountList=(/2,1/), &
        nestAttPackInstanceNameList=nestAttPackName, &
        nestCount=2, nestAttPackInstanceNameCount=nameCount, rc=rc)

      ! Specify the top-level Coupler Component to have a Component Properties
      ! package with some custom attributes
      compPropAtt(1) = 'Tag'
      compPropAtt(2) = 'CaseName'
      compPropAtt(3) = 'SimulationURL'
      compPropAtt(4) = 'WorkingGroup'
      compPropAtt(5) = 'NumPEs'
      compPropAtt(6) = 'MetadataVersion'
      compPropAtt(7) = 'SimulationType'
      compPropAtt(8) = 'Diagnostic'
      call ESMF_AttributeAdd(cplcomp, convention=convCIM, purpose=purpProp, &
        attrList=compPropAtt, rc=rc)
      
      ! Specify the Gridded Components to have the default of 1 Responsible
      !   Party sub-package and 1 Citation sub-package
      call ESMF_AttributeAdd(gridcomp1, convention=convCIM, &
        purpose=purpComp, rc=rc)
      call ESMF_AttributeAdd(gridcomp2, convention=convCIM, &
        purpose=purpComp, rc=rc)
      call ESMF_AttributeAdd(gridcomp3, convention=convCIM, &
        purpose=purpComp, rc=rc)
      call ESMF_AttributeAdd(gridcomp4, convention=convCIM, &
        purpose=purpComp, rc=rc)

      ! Add CIM Attribute package to Fields
      !   convention = 'CIM 1.0'
      !   purpose    = 'Inputs Description'
      call ESMF_AttributeAdd(DMS_emi, convention=convCIM, &
           purpose=purpField, rc=rc)
      call ESMF_AttributeAdd(UM, convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeAdd(OH, convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeAdd(Orog, convention=convCIM, purpose=purpField, &
            rc=rc)
      call ESMF_AttributeAdd(Ozone, convention=convCIM, purpose=purpField, &
            rc=rc)
      call ESMF_AttributeAdd(SST, convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeAdd(SO2, convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeAdd(NOx, convention=convCIM, purpose=purpField, rc=rc)
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
      call ESMF_AttributeSet(cplcomp, 'ShortName', 'HiGEM', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'LongName', &
                             'UK High Resolution Global Environment Model', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'Description', &
        'HiGEM brings together expertise from NERC, the UK academic ' // &
        'community and the Met Office in a concerted UK effort to ' // &
        'develop coupled climate models with increased horizontal ' // &
        'resolutions. Increasing the horizontal resolution of coupled ' // &
        'climate models will allow us to capture climate processes and ' // &
        'weather systems in much greater detail.', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ReleaseDate', &
        '2009-01-01T00:00:00Z', &
          convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ModelType', &
        'AerosolEmissionAndConc', convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'URL', &
        'www.nerc.ac.uk', convention=convCIM, purpose=purpComp, rc=rc)

      ! Simulation run attributes
      call ESMF_AttributeSet(cplcomp, 'SimulationShortName', &
        'ESMF_ESM1', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationLongName', &
        'Earth System Modeling Framework Earth System Model 1.0', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationRationale', &
'ESMF ESM1 simulation run in repsect to CMIP5 core experiment 1.1 ()', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationStartDate', &
       '1960-01-01T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationDuration', &
       'P10Y', &
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
      call ESMF_AttributeSet(cplcomp, 'CompilerName', &
       'Pathscale', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'CompilerVersion', &
       '3.0', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineName', &
       'HECToR', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineDescription', &
'HECToR (Phase 2a) is currently an integrated system known ' // &
'as Rainier, which includes a scalar MPP XT4 system, a vector ' // &
'system known as BlackWidow, and storage systems.', &
convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MachineSystem', &
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
      call ESMF_AttributeSet(cplcomp, 'MachineProcessorType', &
       'AMD X86_64', &
        convention=convCIM, purpose=purpPlatform, rc=rc)

      ! Component Properties: custom attributes
      call ESMF_AttributeSet(cplcomp, 'Tag', &
       'esmf_cesm1_beta16_04', &
        convention=convCIM, purpose=purpProp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'CaseName', &
       'SMS.f09_g16.X.bluefire', &
        convention=convCIM, purpose=purpProp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationURL', &
       'www.cesm.ucar.edu', &
        convention=convCIM, purpose=purpProp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'WorkingGroup', &
       'Atmosphere Model', &
        convention=convCIM, purpose=purpProp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'NumPEs', &
       '16', &
        convention=convCIM, purpose=purpProp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'MetadataVersion', &
       '1', &
        convention=convCIM, purpose=purpProp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'SimulationType', &
       'branch', &
        convention=convCIM, purpose=purpProp, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'Diagnostic', &
       'true', &
        convention=convCIM, purpose=purpProp, rc=rc)
!EOC

!BOE
!    Set the values of the 2 Responsible Party sub-packages, created above
!    for the Coupler Component in the ESMF\_AttributeAdd(cplcomp, ...) call.
!EOE

!BOC 
      ! Responsible party attributes (for Principal Investigator)
      call ESMF_AttributeSet(cplcomp, 'Name', &
       'Gerard Devine', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'PhysicalAddress', &
'Department of Meteorology University of Reading Earley Gate, Reading Devine', &
convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'EmailAddress', &
       'g.m.devine@reading.ac.uk', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ResponsiblePartyRole', &
       'PI', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'URL', &
       'www.epcc.ed.ac.uk', &
        convention=convISO, purpose=purpRP, rc=rc)

      ! Responsible party attributes (for Center)
      call ESMF_AttributeSet(cplcomp, 'Name', &
       'Department of Meteorology University of Reading', &
        convention=convISO, purpose=purpRP, &
        attPackInstanceName=nestAttPackName(2),rc=rc)
      call ESMF_AttributeSet(cplcomp, 'PhysicalAddress', &
       'Reading, Berkshire, United Kingdom', &
        convention=convISO, purpose=purpRP, &
        attPackInstanceName=nestAttPackName(2),rc=rc)
      call ESMF_AttributeSet(cplcomp, 'EmailAddress', &
       'info@reading.ac.uk', &
        convention=convISO, purpose=purpRP, &
        attPackInstanceName=nestAttPackName(2),rc=rc)
      call ESMF_AttributeSet(cplcomp, 'ResponsiblePartyRole', &
       'Center', &
        convention=convISO, purpose=purpRP, &
        attPackInstanceName=nestAttPackName(2),rc=rc)
      call ESMF_AttributeSet(cplcomp, 'URL', &
       'www.epcc.ed.ac.uk', &
        convention=convISO, purpose=purpRP, &
        attPackInstanceName=nestAttPackName(2),rc=rc)
!EOC

!BOE
!    Set the values of the 1 Citation sub-package, created above
!    for the Coupler Component in the ESMF\_AttributeAdd(cplcomp, ...) call.
!EOE

!BOC 
      ! Citation attributes
      call ESMF_AttributeSet(cplcomp, 'ShortTitle', &
       'Shaffrey_2009', &
        convention=convISO, purpose=purpCitation, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'LongTitle', &
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
call ESMF_AttributeSet(cplcomp, 'Date', &
'2009-03-05', &
convention=convISO, purpose=purpCitation, rc=rc)
call ESMF_AttributeSet(cplcomp, 'PresentationForm', &
'Online Refereed', &
convention=convISO, purpose=purpCitation, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'DOI', &
       'doi:10.1175/2008JCLI2508.1', &
        convention=convISO, purpose=purpCitation, rc=rc)
      call ESMF_AttributeSet(cplcomp, 'URL', &
       'http://www.ecmwf.int/', &
        convention=convISO, purpose=purpCitation, rc=rc)

      !
      !  Child component attributes, set on gridcomp1, child of cplcomp
      !
      call ESMF_AttributeSet(gridcomp1, 'ShortName', 'HiGEM_Atmos', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'LongName', &
                             'Atmosphere component of the HiGEM model', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'ReleaseDate', &
        '2009-12-31T23:59:59Z', &
          convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'ModelType', &
        'CloudSimulator', convention=convCIM, purpose=purpComp, rc=rc)

      ! Responsible party attributes (for Author)
      call ESMF_AttributeSet(gridcomp1, 'Name', &
       'John Doe', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'PhysicalAddress', &
       'Department of Meteorology University of ABC', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'EmailAddress', &
       'john.doe@uabc.edu', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp1, 'ResponsiblePartyRole', &
       'Author', &
        convention=convISO, purpose=purpRP, rc=rc)

      !
      !  Child component attributes, set on gridcomp3, child of gridcomp1
      !
      call ESMF_AttributeSet(gridcomp3, 'ShortName', &
                             'HiGEM AtmosDynCore', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'LongName', &
                             'Dynamical core of HiGEM_Atmos', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'ReleaseDate', &
        '2009-10-31T23:59:59Z', &
          convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'ModelType', &
        'AtmosDynamicalCore', convention=convCIM, purpose=purpComp, rc=rc)

      ! Responsible party attributes (for Contact)
      call ESMF_AttributeSet(gridcomp3, 'Name', &
       'Jane Doe', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'PhysicalAddress', &
       'Department of Meteorology University of DEF', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'EmailAddress', &
       'jane.doe@udef.edu', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp3, 'ResponsiblePartyRole', &
       'Contact', &
        convention=convISO, purpose=purpRP, rc=rc)

      !
      !  Child component attributes, set on gridcomp2, child of cplcomp
      !
      call ESMF_AttributeSet(gridcomp2, 'ShortName', 'HiGEM_AtmosChem', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'LongName', &
                             'Atmospheric chemistry component of HiGEM', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'ReleaseDate', &
        '2009-05-31T23:59:59Z', &
          convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'ModelType', &
        'AtmosphericChemistry', convention=convCIM, purpose=purpComp, rc=rc)

      ! Responsible party attributes (for Center)
      call ESMF_AttributeSet(gridcomp2, 'Name', &
       'GHI', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'PhysicalAddress', &
       'Department of Meteorology University of GHI', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'EmailAddress', &
       'info@ughi.edu', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp2, 'ResponsiblePartyRole', &
       'Center', &
        convention=convISO, purpose=purpRP, rc=rc)

      !
      !  Child component attributes, set on gridcomp4, child of gridcomp2
      !
      call ESMF_AttributeSet(gridcomp4, 'ShortName', &
                             'POP2', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp4, 'LongName', &
                             'Parallel Ocean Program', &
        convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp4, 'ReleaseDate', &
        '2010-06-10T00:00:00Z', &
          convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(gridcomp4, 'ModelType', &
        'Ocean', convention=convCIM, purpose=purpComp, rc=rc)

      ! Responsible party attributes (for Funder)
      call ESMF_AttributeSet(gridcomp4, 'Name', &
       'Sally Doe', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp4, 'PhysicalAddress', &
       'Department of Oceanography University of DEF', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp4, 'EmailAddress', &
       'sally.doe@udef.edu', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp4, 'ResponsiblePartyRole', &
       'Funder', &
        convention=convISO, purpose=purpRP, rc=rc)

!EOC

!BOE
!     The standard Attribute package currently supplied by ESMF for 
!     CIM Fields contains a standard CF-Extended package nested within it.
!EOE

!BOC
      ! DMS_emi CF-Extended Attributes
      call ESMF_AttributeSet(DMS_emi, 'ShortName', 'DMS_emi', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'StandardName', 'DMS_emissions', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'LongName', 'DMS emissions', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'Units', 'unknown', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! DMS_emi CIM Attributes
      call ESMF_AttributeSet(DMS_emi, 'CouplingPurpose', &
                                      'boundaryCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'CouplingSource', &
                                      'DMS_emi', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'CouplingTarget', &
                                      'HiGEM_AtmosChem', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'SpatialRegriddingMethod', &
                                      'Conservative-First-Order', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'SpatialRegriddingDimension', &
                                      '1D', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'Frequency', '15 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(DMS_emi, 'TimeTransformationType', &
                                      'TimeAverage', &
           convention=convCIM, purpose=purpField, rc=rc)

      ! UM CF-Extended Attributes
      call ESMF_AttributeSet(UM, 'ShortName', 'UM_Initial_1960', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! UM CIM Attributes
      call ESMF_AttributeSet(UM, 'CouplingPurpose', &
                                 'initialCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(UM, 'CouplingSource', &
                                 'Ocean Biogeo Chemistry', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(UM, 'CouplingTarget', &
                                 'HiGEM_AtmosChem', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(UM, 'TimeTransformationType', 'Exact', &
           convention=convCIM, purpose=purpField, rc=rc)
    
      ! OH CF-Extended Attributes
      call ESMF_AttributeSet(OH, 'ShortName', 'OH_Conc_1900', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'StandardName', &
                                 'OH_Concentrations', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'LongName', &
                                 'seasonal_oxidant_conc', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'Units', 'unknown', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! OH CIM Attributes
      call ESMF_AttributeSet(OH, 'CouplingPurpose', 'boundaryCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'CouplingSource', &
                                 'Land_Emissions', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'CouplingTarget', &
                                 'HiGEM_AtmosChem', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'SpatialRegriddingMethod', &
                                 'Conservative-First-Order', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'SpatialRegriddingDimension', &
                                 '2D', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'Frequency', '15 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(OH, 'TimeTransformationType', &
                                 'TimeInterpolation', &
           convention=convCIM, purpose=purpField, rc=rc)
    
      ! Orog CF-Extended Attributes
      call ESMF_AttributeSet(Orog, 'ShortName', 'UM_Orog_n320', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'StandardName', 'Height', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'LongName', 'Orography', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'Units', 'unknown', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! Orog CIM Attributes
      call ESMF_AttributeSet(Orog, 'CouplingPurpose', 'initialCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'CouplingSource', &
                                   'Land_Emissions', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'CouplingTarget', &
                                   'HiGEM_Atmos', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Orog, 'TimeTransformationType', 'Exact', &
           convention=convCIM, purpose=purpField, rc=rc)
    
      ! Ozone CF-Extended Attributes
      call ESMF_AttributeSet(Ozone, 'ShortName', 'Global_O3_mon', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'StandardName', 'Ozone', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'LongName', 'Ozone', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'Units', 'unknown', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! Ozone CIM Attributes
      call ESMF_AttributeSet(Ozone, 'CouplingPurpose', 'boundaryCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'CouplingSource', &
                                    'Global_O3_mon', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'CouplingTarget', &
                                    'HiGEM_Atmos', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'SpatialRegriddingMethod', &
                                    'Conservative-First-Order', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'SpatialRegriddingDimension', &
                                    '3D', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'Frequency', '15 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(Ozone, 'TimeTransformationType', &
                                    'TimeInterpolation', &
           convention=convCIM, purpose=purpField, rc=rc)
    
      ! SST CF-Extended Attributes
      call ESMF_AttributeSet(SST, 'ShortName', 'SST', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! SST CIM Attributes
      call ESMF_AttributeSet(SST, 'CouplingPurpose', 'initialCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'CouplingSource', &
                                  'seasonal_oxidant_conc', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'CouplingTarget', &
                                  'HiGEM_Atmos', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'SpatialRegriddingMethod', &
                                  'Conservative-First-Order', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'SpatialRegriddingDimension', &
                                  '2D', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'Frequency', '15 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SST, 'TimeTransformationType', &
                                  'TimeAverage', &
           convention=convCIM, purpose=purpField, rc=rc)

      ! SO2 CF-Extended Attributes
      call ESMF_AttributeSet(SO2, 'ShortName', 'SO2', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! SO2 CIM Attributes
      call ESMF_AttributeSet(SO2, 'CouplingPurpose', 'boundaryCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SO2, 'CouplingSource', &
                                  'POP2 Ocean', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SO2, 'CouplingTarget', &
                                  'HiGEM_Atmos', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SO2, 'SpatialRegriddingMethod', &
                                  'Cubic', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SO2, 'SpatialRegriddingDimension', &
                                  '3D', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SO2, 'Frequency', '10 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(SO2, 'TimeTransformationType', &
                                  'Exact', &
           convention=convCIM, purpose=purpField, rc=rc)

      ! NOx CF-Extended Attributes
      call ESMF_AttributeSet(NOx, 'ShortName', 'NOx', &
           convention=convCIM, purpose=purpField, rc=rc)
      ! NOx CIM Attributes
      call ESMF_AttributeSet(NOx, 'CouplingPurpose', 'initialCondition', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(NOx, 'CouplingSource', &
                                  'POP2 Ocean', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(NOx, 'CouplingTarget', &
                                  'HiGEM_Atmos', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(NOx, 'SpatialRegriddingMethod', &
                                  'Linear', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(NOx, 'SpatialRegriddingDimension', &
                                  '1D', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(NOx, 'Frequency', '5 minutes', &
           convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(NOx, 'TimeTransformationType', &
                                  'TimeAccumulation', &
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

      ! Add two Fields directly to the third State,
      !  without a FieldBundle
      call ESMF_StateAdd(exportState3, field=Ozone, rc=rc)
      call ESMF_StateAdd(exportState3, field=SST, rc=rc)

      ! Add the remaining two Fields directly to the fourth State,
      !  without a FieldBundle
      call ESMF_StateAdd(exportState4, field=SO2, rc=rc)
      call ESMF_StateAdd(exportState4, field=NOx, rc=rc)
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
      call ESMF_AttributeLink(gridcomp4, exportState4, rc=rc)

      ! Gridded Component 1 and gridded Component 2 are children 
      ! of the coupler
      call ESMF_AttributeLink(cplcomp, gridcomp1, rc=rc)
      call ESMF_AttributeLink(cplcomp, gridcomp2, rc=rc)
      ! Gridded Component 3 is a child of gridded Component 1 (grandchild of 
      ! the coupler), and Gridded Component 4 is a child of gridded
      ! Component 2 (grandchild of the coupler)
      call ESMF_AttributeLink(gridcomp1, gridcomp3, rc=rc)
      call ESMF_AttributeLink(gridcomp2, gridcomp4, rc=rc)
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
        if (rc/=ESMF_SUCCESS .and. rc/=ESMF_RC_LIB_NOT_PRESENT) goto 10
      endif

      ! Clean-up
      call ESMF_FieldDestroy(field=NOx, rc=rc)
      call ESMF_FieldDestroy(field=SO2, rc=rc)
      call ESMF_FieldDestroy(field=SST, rc=rc)
      call ESMF_FieldDestroy(field=Ozone, rc=rc)
      call ESMF_FieldDestroy(field=Orog, rc=rc)
      call ESMF_FieldDestroy(field=OH, rc=rc)
      call ESMF_FieldDestroy(field=UM, rc=rc)
      call ESMF_FieldDestroy(field=DMS_emi, rc=rc)
      call ESMF_FieldBundleDestroy(fbundle2, rc=rc)
      call ESMF_FieldBundleDestroy(fbundle1, rc=rc)
      call ESMF_StateDestroy(exportState4, rc=rc)
      call ESMF_StateDestroy(exportState3, rc=rc)
      call ESMF_StateDestroy(exportState2, rc=rc)
      call ESMF_StateDestroy(exportState1, rc=rc)
      call ESMF_GridCompDestroy(gridcomp4, rc=rc)
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
