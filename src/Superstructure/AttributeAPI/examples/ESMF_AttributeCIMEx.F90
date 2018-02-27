! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
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
!\begin{sloppypar}
! This example illustrates the use of the Metafor CIM Attribute packages,
! supplied by ESMF, to create an Attribute hierarchy on an ESMF object tree.
! Gridded, coupler and science Components are used together with a State
! and a realistic Field
! to create a simple ESMF object tree.  CIM Attributes packages are created
! on the Components and Field, and then the individual Attributes within the
! packages are populated with values.  Finally, all the Attributes are written
! to a CIM-formatted XML file.  For a more comprehensive example, see the
! ESMF\_AttributeCIM system test.
!\end{sloppypar}
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
      use ESMF
      use ESMF_TestMod
      implicit none

      ! Local variables
      integer                 :: rc, finalrc, petCount, localPet, result
          type(ESMF_AttPack)      :: attpack
      type(ESMF_VM)           :: vm
      type(ESMF_Field)        :: ozone
      type(ESMF_State)        :: exportState
      type(ESMF_CplComp)      :: cplcomp
      type(ESMF_GridComp)     :: gridcomp
      type(ESMF_SciComp)      :: scicomp
      character(ESMF_MAXSTR)  :: convCIM, purpComp, purpProp, purpSci
      character(ESMF_MAXSTR)  :: purpField, purpPlatform
      character(ESMF_MAXSTR)  :: convISO, purpRP, purpCitation
      character(ESMF_MAXSTR), dimension(2)  :: compPropAtt
      character(ESMF_MAXSTR), dimension(2)  :: rad_sciPropAtt
      character(ESMF_MAXSTR)  :: testname
      character(ESMF_MAXSTR)  :: failMsg
!EOC

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_AttributeCIMEx"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------



!BOC


      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, defaultlogfilename="AttributeCIMEx.Log", &
        logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_AttributeCIMEx Example"
        print *, "--------------------------------------- "
      endif

!BOE
!\begin{sloppypar}
!    Create the ESMF objects that will hold the CIM Attributes.
!    These objects include all three Component types (coupler, gridded,
!    and science Components) as well as a State, and a Field.
!    In this example we are constructing empty Fields without an
!    underlying Grid.
!\end{sloppypar}
!EOE

!BOC
      ! Create top-level Coupler Component
      cplcomp = ESMF_CplCompCreate(name="coupler_component", &
        petList=(/0/), rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      ! Create Gridded Component as a child of the Coupler Component
      gridcomp = ESMF_GridCompCreate(name="gridded_component", &
        petList=(/0/), rc=rc)

      call ESMF_AttributeLink(cplcomp, gridcomp, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      ! Create Science Component as a child of the Gridded Component
      scicomp = ESMF_SciCompCreate(name="science_component", rc=rc)

      call ESMF_AttributeLink(gridcomp, scicomp, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      ! Create State
      exportState = ESMF_StateCreate(name="exportState",  &
        stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


      ! Create Field
      ozone = ESMF_FieldEmptyCreate(name='ozone', rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


!BOC
      convCIM = 'CIM 1.5'
      purpComp = 'ModelComp'
      purpProp = 'CompProp'
      purpSci = 'SciProp'
      purpField = 'Inputs'
      purpPlatform = 'Platform'

      convISO = 'ISO 19115'
      purpRP = 'RespParty'
      purpCitation = 'Citation'
!EOC

!BOE
!\begin{sloppypar}
!    Add CIM Component package and Attributes to the Coupler Component.
!\end{sloppypar}
!EOE

!BOC
      call ESMF_AttributeAdd(cplcomp,  &
                             convention=convCIM, purpose=purpComp, rc=rc)

      call ESMF_AttributeSet(cplcomp, "ShortName", "Driver", &
                             convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, "LongName", &
                             "Model Driver", &
                             convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(cplcomp, "ModelType", &
                             "climate", &
                             convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


      ! Simulation run attributes
      call ESMF_AttributeSet(cplcomp, 'SimulationShortName', &
                                      'SMS.f09_g16.X.hector', &
        convention=convCIM, purpose=purpComp, rc=rc)

      call ESMF_AttributeSet(cplcomp, 'SimulationLongName', &
        'EarthSys - Earth System Modeling Framework Earth System Model 1.0', &
        convention=convCIM, purpose=purpComp, rc=rc)

      call ESMF_AttributeSet(cplcomp, 'SimulationRationale', &
  'EarthSys-ESMF simulation run in respect to CMIP5 core experiment 1.1 ()', &
        convention=convCIM, purpose=purpComp, rc=rc)

      call ESMF_AttributeSet(cplcomp, 'SimulationStartDate', &
                                       '1960-01-01T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)

      call ESMF_AttributeSet(cplcomp, 'SimulationDuration', 'P10Y', &
        convention=convCIM, purpose=purpComp, rc=rc)

      call ESMF_AttributeSet(cplcomp, &
         'SimulationNumberOfProcessingElements', '16', &
          convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeGetAttPack(cplcomp, convCIM, purpPlatform, &
        attpack=attpack, rc=rc)

      call ESMF_AttributeSet(cplcomp, 'MachineName', 'HECToR', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!    Now add CIM Attribute packages and Attributes to the Gridded Component
!    and Field.  Also, add a CIM Component Properties package, to contain
!    two custom attributes.
!\end{sloppypar}
!EOE

!BOC
      ! Add CIM Attribute package to the gridded Component
      call ESMF_AttributeAdd(gridcomp, convention=convCIM, &
        purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


      ! Specify the gridded Component to have a Component Properties
      ! package with two custom attributes, with user-specified names
      compPropAtt(1) = 'SimulationType'
      compPropAtt(2) = 'SimulationURL'
      call ESMF_AttributeAdd(gridcomp, convention=convCIM, purpose=purpProp, &
        attrList=compPropAtt, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


      ! Add CIM Attribute package to the Field
      call ESMF_AttributeAdd(ozone, convention=convCIM, purpose=purpField, &
        rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


!BOE
!\begin{sloppypar}
!     The standard Attribute package supplied by ESMF for a CIM Component
!     contains several Attributes, grouped into sub-packages.  These
!     Attributes conform to the CIM convention as defined by Metafor and
!     their values are set individually.
!\end{sloppypar}
!EOE

!BOC
      !
      ! Top-level model component attributes, set on gridded component
      !
      call ESMF_AttributeSet(gridcomp, 'ShortName', 'EarthSys_Atmos', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'LongName', &
        'Earth System High Resolution Global Atmosphere Model', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'Description', &
        'EarthSys brings together expertise from the global ' // &
        'community in a concerted effort to develop coupled ' // &
        'climate models with increased horizontal resolutions.  ' // &
        'Increasing the horizontal resolution of coupled climate ' // &
        'models will allow us to capture climate processes and ' // &
        'weather systems in much greater detail.', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'Version', '2.0', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'ReleaseDate', '2009-01-01T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'ModelType', 'aerosol', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'URL', &
        'www.earthsys.org', convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'MetadataVersion', '1.1', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


      ! Document genealogy
      call ESMF_AttributeSet(gridcomp, 'PreviousVersion', &
                                       'EarthSys1 Atmosphere', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'PreviousVersionDescription', &
       'Horizontal resolution increased to 1.20 x 0.80 degrees; ' // &
       'Timestep reduced from 30 minutes to 15 minutes.', &
        convention=convCIM, purpose=purpComp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeGetAttPack(gridcomp, convCIM, purpPlatform, &
        attpack=attpack, rc=rc)

      ! Platform description attributes
      call ESMF_AttributeSet(gridcomp, 'CompilerName', 'Pathscale', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'CompilerVersion', '3.0', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'MachineName', 'HECToR', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'MachineDescription', &
        'HECToR (Phase 2a) is currently an integrated system known ' // &
        'as Rainier, which includes a scalar MPP XT4 system, a vector ' // &
        'system known as BlackWidow, and storage systems.', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'MachineSystem', 'Parallel', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(gridcomp, 'MachineOperatingSystem', 'Unicos', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(gridcomp, 'MachineVendor', 'Cray Inc', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
      call ESMF_AttributeSet(gridcomp, 'MachineInterconnectType', &
                                       'Cray Interconnect', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'MachineMaximumProcessors', '22656', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'MachineCoresPerProcessor', '4', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'MachineProcessorType', 'AMD X86_64', &
        convention=convCIM, purpose=purpPlatform, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeGetAttPack(gridcomp, convCIM, purpProp, &
         attpack=attpack, rc=rc)

      ! Component Properties: custom attributes
      call ESMF_AttributeSet(gridcomp, 'SimulationType', 'branch', &
        convention=convCIM, purpose=purpProp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'SimulationURL', &
                                       'http://earthsys.org/simulations', &
        convention=convCIM, purpose=purpProp, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!    Set the attribute values of the Responsible Party sub-package, created
!    above for the gridded Component in the ESMF\_AttributeAdd(gridcomp, ...)
!    call.
!\end{sloppypar}
!EOE

!BOC
      call ESMF_AttributeGetAttPack(gridcomp, convISO, purpRP, &
        attpack=attpack, rc=rc)

      ! Responsible party attributes (for Principal Investigator)
      call ESMF_AttributeSet(gridcomp, 'Name', 'John Doe', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp, 'Abbreviation', 'JD', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp, 'PhysicalAddress', &
          'Department of Meteorology, University of ABC', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp, 'EmailAddress', &
                                       'john.doe@earthsys.org', &
        convention=convISO, purpose=purpRP, rc=rc)
      call ESMF_AttributeSet(gridcomp, 'ResponsiblePartyRole', 'PI', &
        convention=convISO, purpose=purpRP, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'URL', 'www.earthsys.org', &
        convention=convISO, purpose=purpRP, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!    Set the attribute values of the Citation sub-package, created above
!    for the gridded Component in the ESMF\_AttributeAdd(gridcomp, ...) call.
!\end{sloppypar}
!EOE

!BOC
      call ESMF_AttributeGetAttPack(gridcomp, convISO, purpCitation, &
        attpack=attpack, rc=rc)

      ! Citation attributes
      call ESMF_AttributeSet(gridcomp, 'ShortTitle', 'Doe_2009', &
        convention=convISO, purpose=purpCitation, rc=rc)
      call ESMF_AttributeSet(gridcomp, 'LongTitle', &
       'Doe, J.A.; Norton, A.B.; ' // &
       'Clark, G.H.; Davies, I.J.. 2009 EarthSys: ' // &
       'The Earth System High Resolution Global Atmosphere Model - Model ' // &
       'description and basic evaluation. Journal of Climate, 15 (2). ' // &
       '1261-1296.', &
        convention=convISO, purpose=purpCitation, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'Date', '2010-03-15', &
        convention=convISO, purpose=purpCitation, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'PresentationForm', 'Online Refereed', &
        convention=convISO, purpose=purpCitation, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'DOI', 'doi:17.1035/2009JCLI4508.1', &
        convention=convISO, purpose=purpCitation, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, 'URL', &
                             'http://www.earthsys.org/publications', &
        convention=convISO, purpose=purpCitation, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!     Add Component attributes to the Science Component and then add
!     scientific properties to it.
!\end{sloppypar}
!EOE

!BOC
      call ESMF_AttributeAdd(scicomp,  &
                             convention=convCIM, purpose=purpComp, rc=rc)

      call ESMF_AttributeSet(scicomp, "ShortName", "AtmosRadiation", &
                             convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(scicomp, "LongName", &
                             "Atmosphere Radiation", &
                             convention=convCIM, purpose=purpComp, rc=rc)
      call ESMF_AttributeSet(scicomp, "ModelType", &
                             "radiation", &
                             convention=convCIM, purpose=purpComp, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      rad_sciPropAtt(1) = 'LongwaveSchemeType'
      rad_sciPropAtt(2) = 'LongwaveSchemeMethod'

      call ESMF_AttributeAdd(scicomp,  &
                             convention=convCIM, purpose=purpSci, &
                             attrList=rad_sciPropAtt, rc=rc)

      call ESMF_AttributeSet(scicomp, &
                             'LongwaveSchemeType', &
                             'wide-band model', &
                             convention=convCIM, purpose=purpSci, rc=rc)
      call ESMF_AttributeSet(scicomp, &
                             'LongwaveSchemeMethod', &
                             'two-stream', &
                             convention=convCIM, purpose=purpSci, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


!BOE
!\begin{sloppypar}
!     The standard Attribute package currently supplied by ESMF for
!     CIM Fields contains a standard CF-Extended package nested within it.
!\end{sloppypar}
!EOE

!BOC
      call ESMF_AttributeGetAttPack(ozone, convCIM, purpField, &
        attpack=attpack, rc=rc)

      ! ozone CF-Extended Attributes
      call ESMF_AttributeSet(ozone, 'ShortName', 'Global_O3_mon', &
       convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(ozone, 'StandardName', 'ozone', &
       convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(ozone, 'LongName', 'ozone', &
       convention=convCIM, purpose=purpField, rc=rc)
      call ESMF_AttributeSet(ozone, 'Units', 'unknown', &
       convention=convCIM, purpose=purpField, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC


      ! ozone CIM Attributes
      call ESMF_AttributeSet(ozone, 'CouplingPurpose', 'Boundary', &
       convention=convCIM, purpose=purpField, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(ozone, 'CouplingSource', 'EarthSys_Atmos', &
       convention=convCIM, purpose=purpField, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(ozone, 'CouplingTarget', &
       'EarthSys_AtmosDynCore', convention=convCIM, &
        purpose=purpField, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(ozone, 'Description', &
                                    'Global Ozone concentration ' // &
                                    'monitoring in the atmosphere.', &
       convention=convCIM, purpose=purpField, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(ozone, 'SpatialRegriddingMethod', &
                                    'Conservative-First-Order', &
       convention=convCIM, purpose=purpField, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(ozone, 'SpatialRegriddingDimension', '3D', &
       convention=convCIM, purpose=purpField, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(ozone, 'Frequency', '15 Minutes', &
       convention=convCIM, purpose=purpField, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(ozone, 'TimeTransformationType', &
                                    'TimeInterpolation', &
       convention=convCIM, purpose=purpField, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!     Adding the Field to the State will automatically link the
!     Attribute hierarchies from the State to the Field
!\end{sloppypar}
!EOE

!BOC
      ! Add the Field directly to the State
      call ESMF_StateAdd(exportState, fieldList=(/ozone/), rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!     The Attribute link between a Component and a State must be set manually.
!\end{sloppypar}
!EOE

!BOC
      ! Link the State to the gridded Component
      call ESMF_AttributeLink(gridcomp, exportState, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!     Write the entire CIM Attribute hierarchy, beginning at the gridded
!     Component (the top), to an XML file formatted to conform to CIM
!     specifications.  The CIM output tree structure differs from the
!     internal Attribute hierarchy in that it has all the attributes of
!     the fields within its top-level <modelComponent> record.  The filename
!     used, gridded\_component.xml, is derived from the name of the gridded
!     Component, given as an input argument in the ESMF\_GridCompCreate()
!     call above.  The file is written to the examples execution directory.
!\end{sloppypar}
!EOE

!BOC
      call ESMF_AttributeWrite(cplcomp, convCIM, purpComp, &
        attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
!EOC
        if (rc/=ESMF_SUCCESS .and. rc/=ESMF_RC_LIB_NOT_PRESENT) goto 10

      ! Clean-up
      call ESMF_FieldDestroy(field=ozone, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_StateDestroy(exportState, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_SciCompDestroy(scicomp, rc=rc)
      call ESMF_GridCompDestroy(gridcomp, rc=rc)
      call ESMF_CplCompDestroy(cplcomp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "End of ESMF_AttributeCIMEx Example"
        print *, "--------------------------------------- "
      endif


      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
      ! file that the scripts grep for.
      call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


!BOC

      call ESMF_Finalize(rc=rc)
!EOC

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
