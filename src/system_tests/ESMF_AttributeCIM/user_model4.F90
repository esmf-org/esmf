! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component, most recent interface revision.
!
!
!\begin{verbatim}

module user_model4

  ! ESMF Framework module
  use ESMF

  implicit none

  private

  public userm4_setvm, userm4_register, user_init

  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.

  subroutine userm4_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
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
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
    endif
#endif

  end subroutine

  subroutine userm4_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
      userRoutine=user_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
      userRoutine=user_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
      userRoutine=user_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.


  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
          type(ESMF_AttPack)        :: attpack
    character(ESMF_MAXSTR)      :: convCIM, purpComp, purpSci, purpField
    character(ESMF_MAXSTR)      :: convISO, purpRP, purpCitation
    character(ESMF_MAXSTR)      :: sciPropAtt(5)
    type(ESMF_Field)            :: SOA, POM
    type(ESMF_FieldBundle)      :: fieldbundle

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Create the CIM Attribute package on the Gridded Component and set its
    ! values.  The standard Attribute package currently supplied by ESMF for a
    ! CIM Component contains several Attributes, grouped into sub-packages.
    ! These Attributes conform to the CIM convention as defined by Metafor and
    ! their values are set individually.

    !
    !  CIM child component attributes, set on this comp, child of the coupler
    !
    convCIM = 'CIM 1.7.1'
    purpComp = 'ModelComp'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'ShortName', &
                           'EarthSys_OceanBioGeoChem', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  1) Name of component in navigator bar on the left;
    !                  attribute 'Version' appended, if set.
    !               2) Also "Simulation Metadata:", for top-level component,
    !                  first part of display, at top, 1st line, prepended to
    !                  top-level component's attributes 'Version' (if set) and
    !                  'SimulationShortName'.

    call ESMF_AttributeSet(comp, 'LongName', &
                           'Ocean biogeochemistry component of EarthSys', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Full Name:"  first part of display, at top, 2nd line
    !               under title, prepended to attribute 'SimulationLongName'.

    call ESMF_AttributeSet(comp, 'Description', &
      'The biogeochemistry component of the EarthSys ocean model.', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Description:" in top box.

    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2010-06-10T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Release Date" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'ModelType', &
      'ocnBgchem', convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Maps to "Realm:", expanded under component name, in
    !               navigator bar on the left.


    ! Responsible party attributes (for Author)
    convISO = 'ISO 19115'
    purpRP = 'RespParty'
    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'Name', &
     'EarthSys Atmosphere Model Working Group', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display: Ingested and stored, but not yet displayed, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'Abbreviation', &
     'EAMWG', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'NameType', &
     'Organization', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not in CIM output; used only by ESMF to override
    !               default output of Name's XML element.

    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Climate Division, International Center for ' // &
     'Atmospheric Research', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'info@earthsys.org', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display: Ingested and stored, but not yet displayed, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'Author', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Ingested, but only used to control display.

    call ESMF_AttributeSet(comp, 'URL', &
     'http://www.earthsys.org/working_groups/Atmosphere', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! Citation attributes
    convISO = 'ISO 19115'
    purpCitation = 'Citation'
    call ESMF_AttributeGetAttPack(comp, convISO, purpCitation, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'ShortTitle', &
     'Doe_2005', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'LongTitle', &
     'Doe, J.A.; Doe, S.B.; ' // &
     'Doe, J.C.; 2005 EarthSys: ' // &
     'The Earth System High Resolution Global Model - ' // &
     'Ocean Biogeochemistry model description . Journal of ' // &
     'Earth Modeling, 11 (5). 1661-1696.', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Reference", concatenated with attribute 'DOI', under
    !               tab "References".

    call ESMF_AttributeSet(comp, 'Date', &
     '2005-07-09', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'PresentationForm', &
     'Online Refereed', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'DOI', &
     'doi:13.1031/2005JCLI4504.1', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Concatenated to attribute 'LongTitle' and displayed as
    !               "Reference" under tab "References".

    call ESMF_AttributeSet(comp, 'URL', &
     'http://www.earthsys.org/publications', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not output to CIM, as of v1.5/1.7 (no definition for it).


    !
    !  CIM child component scientific property attributes
    !
    convCIM = 'CIM 1.7.1'
    purpSci = 'SciProp'
    ! Define some user-specified scientific properties
    sciPropAtt(1) = 'OceanBiogeoChemistryOceanBioKeyPropertiesTransportMethod'
    sciPropAtt(2) = 'OceanBiogeoChemistryOceanBioBoundaryForcingAtmosphericDeposition'
    sciPropAtt(3) = 'OceanBiogeoChemistryOceanBioChemistryCarbonChemistrypH-scale'
    sciPropAtt(4) = 'OceanBiogeoChemistryOceanBioTracersNutrientsListOfSpecies'
    sciPropAtt(5) = 'OceanBiogeoChemistryOceanBioTracersOceanBioTracersEcosystemZooplanctonType'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpSci, &
      attrList=sciPropAtt, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Scientific Properties:  attributes per Metafor standard

    call ESMF_AttributeSet(comp, &
     'OceanBiogeoChemistryOceanBioKeyPropertiesTransportMethod', &
       'different from Ocean Tracers', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs
    !               "Properties->Scientific"

    call ESMF_AttributeSet(comp, &
     'OceanBiogeoChemistryOceanBioBoundaryForcingAtmosphericDeposition', &
       'other', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs
    !               "Properties->Scientific"

    call ESMF_AttributeSet(comp, &
     'OceanBiogeoChemistryOceanBioChemistryCarbonChemistrypH-scale', &
       'sea water', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs
    !               "Properties->Scientific"

    call ESMF_AttributeSet(comp, &
     'OceanBiogeoChemistryOceanBioTracersNutrientsListOfSpecies', &
       'Iron (Fe)', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs
    !               "Properties->Scientific"

    call ESMF_AttributeSet(comp, &
     'OceanBiogeoChemistryOceanBioTracersOceanBioTracersEcosystemZooplanctonType', &
       'generic', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs
    !               "Properties->Scientific"


    ! Create two Fields, and add CIM Attribute packages.
    ! The standard Attribute package currently supplied by ESMF for
    ! CIM Fields contains a standard CF-Extended package nested within it.
    convCIM = 'CIM 1.7.1'
    purpField = 'Inputs'

    ! SOA Field
    SOA = ESMF_FieldEmptyCreate(name='SOA', rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_AttributeAdd(SOA, convention=convCIM, purpose=purpField,rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    ! SOA CF-Extended Attributes
    call ESMF_AttributeSet(SOA, 'ShortName', 'SOA', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  As field name under tab "Inputs".

    call ESMF_AttributeSet(SOA, 'LongName', 'Secondary organic aerosols', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(SOA, 'Units', 'kg/m3', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! SOA CIM Attributes
    call ESMF_AttributeSet(SOA, 'CouplingPurpose', 'Boundary', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Title of expandable bar under tab "Inputs",
    !               "Boundary Conditions".

    call ESMF_AttributeSet(SOA, 'CouplingSource', &
                                'EarthSys_OceanBioGeoChem', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Source Component" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(SOA, 'CouplingTarget', &
                                'EarthSys_Ocean', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Target Component" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(SOA, 'Description', &
                                'Secondary organic aerosols in the ' // &
                                'ocean.', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Next to field name (after colon) under tab "Inputs".

    call ESMF_AttributeSet(SOA, 'SpatialRegriddingMethod', &
                                'Cubic', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Spatial Regridding Method" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(SOA, 'SpatialRegriddingDimension', &
                                '3D', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(SOA, 'Frequency', '2 Years', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Frequency" under tab "Inputs", under field name.

    call ESMF_AttributeSet(SOA, 'TimeTransformationType', &
                                'Exact', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Time Transformation Type" under tab "Inputs",
    !               under field name.


    ! POM Field
    POM = ESMF_FieldEmptyCreate(name='POM', rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_AttributeAdd(POM, convention=convCIM, purpose=purpField,rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! POM CF-Extended Attributes
    call ESMF_AttributeSet(POM, 'ShortName', 'POM', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  As field name under tab "Inputs".

    call ESMF_AttributeSet(POM, 'LongName', 'Particulate organic matter', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(POM, 'Units', 'mol/m3', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! POM CIM Attributes
    call ESMF_AttributeSet(POM, 'CouplingPurpose', 'Initial', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Title of expandable bar under tab "Inputs",
    !               "Boundary Conditions".

    call ESMF_AttributeSet(POM, 'CouplingSource', &
                                'EarthSys_OceanBioGeoChem', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Source Component" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(POM, 'CouplingTarget', &
                                'EarthSys_Ocean', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Target Component" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(POM, 'Description', &
                                'Particulate organic matter in ' // &
                                'the ocean.', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Next to field name (after colon) under tab "Inputs".

    call ESMF_AttributeSet(POM, 'SpatialRegriddingMethod', &
                                'Conservative', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Spatial Regridding Method" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(POM, 'SpatialRegriddingDimension', &
                                '1D', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(POM, 'Frequency', '45 Seconds', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Frequency" under tab "Inputs", under field name.

    call ESMF_AttributeSet(POM, 'TimeTransformationType', &
                                'TimeAccumulation', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Time Transformation Type" under tab "Inputs",
    !               under field name.


    ! Create a FieldBundle for the two Fields
    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle4", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Add the Fields to the FieldBundle (this will connect the Attribute
    ! hierarchies of the FieldBundle and Fields)
    call ESMF_FieldBundleAdd(fieldbundle, (/SOA/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleAdd(fieldbundle, (/POM/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Link the Attributes from the FieldBundle to the export State
    call ESMF_StateAdd(exportState, fieldbundleList=(/fieldbundle/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine user_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !

  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Nothing happens in this run cycle for this simple example

  end subroutine user_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !

  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fieldbundle
    integer                     :: k

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_StateGet(exportState, "fieldbundle4", fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    do k = 1, 2
        call ESMF_FieldBundleGet(fieldbundle, fieldIndex=k, field=field, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out

        call ESMF_FieldDestroy(field, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
    enddo

    call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
  end subroutine user_final

end module user_model4

!\end{verbatim}
