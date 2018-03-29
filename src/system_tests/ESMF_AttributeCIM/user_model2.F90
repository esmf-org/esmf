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

module user_model2

  ! ESMF Framework module
  use ESMF

  implicit none

  private

  public userm2_setvm, userm2_register, user_init

  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.

  subroutine userm2_setvm(comp, rc)
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

  subroutine userm2_register(comp, rc)
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
    character(ESMF_MAXSTR)      :: sciPropAtt(3)
    type(ESMF_Field)            :: DMS_emi, SST
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
    ! Specify the Gridded Component to have the default of 1 Responsible
    !   Party sub-package and 1 Citation sub-package
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'ShortName', 'EarthSys_Ocean', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  1) Name of component in navigator bar on the left;
    !                  attribute 'Version' appended, if set.
    !               2) Also "Simulation Metadata:", for top-level component,
    !                  first part of display, at top, 1st line, prepended to
    !                  top-level component's attributes 'Version' (if set) and
    !                  'SimulationShortName'.

    call ESMF_AttributeSet(comp, 'LongName', &
                           'Ocean component of EarthSys', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Full Name:"  first part of display, at top, 2nd line
    !               under title, prepended to attribute 'SimulationLongName'.

    call ESMF_AttributeSet(comp, 'Description', &
      'The EarthSys ocean component uses a latitude-longitude grid ' // &
      'with a zonal resolution of 2 degrees, and a meridional ' // &
      'resolution of 2 degrees.  It has 30 evenly spaced levels in ' // &
      'the vertical.  The timestep period is 1 hour.', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Description:" in top box.

    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2009-05-31T23:59:59Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Release Date" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'ModelType', &
      'ocean', convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Maps to "Realm:", expanded under component name, in
    !               navigator bar on the left.


    ! Responsible party attributes (for Center)
    convISO = 'ISO 19115'
    purpRP = 'RespParty'
    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'Name', &
     'GHI', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Ingested and stored, but not yet displayed, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Meteorology, University of ABC', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'info@earthsys.org', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'Center', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Ingested, but only used to control display.


    ! Citation attributes
    convISO = 'ISO 19115'
    purpCitation = 'Citation'
    call ESMF_AttributeGetAttPack(comp, convISO, purpCitation, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'ShortTitle', &
     'Doe_2007', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'LongTitle', &
     'Doe, J.A.; Doe, S.B.; ' // &
     'Doe, J.C.; 2007 EarthSys: ' // &
     'The Earth System High Resolution Global Model - ' // &
     'Ocean model description . Journal of Earth Modeling, 13 (4). ' // &
     '1461-1496.', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Reference", concatenated with attribute 'DOI', under
    !               tab "References".

    call ESMF_AttributeSet(comp, 'Date', &
     '2007-05-07', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'PresentationForm', &
     'Online Refereed', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'DOI', &
     'doi:15.1033/2007JCLI4506.1', &
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
    sciPropAtt(1) = 'OceanOceanKeyPropertiesModelFamily'
    sciPropAtt(2) = 'OceanOceanKeyPropertiesBasicApproximations'
    sciPropAtt(3) = 'OceanOceanKeyPropertiesListOfPrognosticVariables'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpSci, &
      attrList=sciPropAtt, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Scientific Properties:  attributes per Metafor standard
    call ESMF_AttributeSet(comp, &
     'OceanOceanKeyPropertiesModelFamily', &
       'OGCM', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs
    !               "Properties->Scientific"

    call ESMF_AttributeSet(comp, &
     'OceanOceanKeyPropertiesBasicApproximations', &
       'non-hydrostatic', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs
    !               "Properties->Scientific"

    call ESMF_AttributeSet(comp, &
     'OceanOceanKeyPropertiesListOfPrognosticVariables', &
       'salinity', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs
    !               "Properties->Scientific"


    ! Create two Fields, and add CIM Attribute packages.
    ! The standard Attribute package currently supplied by ESMF for
    ! CIM Fields contains a standard CF-Extended package nested within it.
    convCIM = 'CIM 1.7.1'
    purpField = 'Inputs'

    ! DMS_emi Field
    DMS_emi = ESMF_FieldEmptyCreate(name='DMS_emi', rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_AttributeAdd(DMS_emi, convention=convCIM, purpose=purpField,rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    ! DMS_emi ESMF-General Attribute
    call ESMF_AttributeSet(DMS_emi, 'Intent', 'Export', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! DMS_emi CF-Extended Attributes
    call ESMF_AttributeSet(DMS_emi, 'ShortName', 'DMS_emi', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  As field name under tab "Inputs".

    call ESMF_AttributeSet(DMS_emi, 'StandardName', 'DMS_emissions', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(DMS_emi, 'LongName', 'DMS emissions', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(DMS_emi, 'Units', 'kg/m2/s', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! DMS_emi CIM Attributes
    call ESMF_AttributeSet(DMS_emi, 'CouplingPurpose', &
                                    'Ancillary', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Title of expandable bar under tab "Inputs",
    !               "Boundary Conditions".

    call ESMF_AttributeSet(DMS_emi, 'CouplingSource', &
                                    'EarthSys_Ocean', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Source Component" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(DMS_emi, 'CouplingTarget', &
                                    'EarthSys_OceanBioGeoChem', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Target Component" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(DMS_emi, 'Description', &
                                    'Dimethyl Sulfide emissions in the ' // &
                                    'atmosphere.', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Next to field name (after colon) under tab "Inputs".

    call ESMF_AttributeSet(DMS_emi, 'SpatialRegriddingMethod', &
                                    'Conservative-First-Order', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Spatial Regridding Method" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(DMS_emi, 'Frequency', '15 Minutes', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Frequency" under tab "Inputs", under field name.

    call ESMF_AttributeSet(DMS_emi, 'TimeTransformationType', &
                                    'TimeAverage', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Time Transformation Type" under tab "Inputs",
    !               under field name.


    ! SST Field
    SST = ESMF_FieldEmptyCreate(name='SST', rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_AttributeAdd(SST, convention=convCIM, purpose=purpField,rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! SST CF-Extended Attributes
    call ESMF_AttributeSet(SST, 'ShortName', 'SST', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  As field name under tab "Inputs".

    call ESMF_AttributeSet(SST, 'Units', 'K', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! SST CIM Attributes
    call ESMF_AttributeSet(SST, 'CouplingPurpose', 'Initial', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Title of expandable bar under tab "Inputs",
    !               "Boundary Conditions".

    call ESMF_AttributeSet(SST, 'CouplingSource', &
                                'EarthSys_Ocean', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Source Component" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(SST, 'CouplingTarget', &
                                'EarthSys_OceanBioGeoChem', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Target Component" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(SST, 'Description', &
                                'Sea surface temperature.', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Next to field name (after colon) under tab "Inputs".

    call ESMF_AttributeSet(SST, 'SpatialRegriddingMethod', &
                                'Non-Conservative', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Spatial Regridding Method" under tab "Inputs",
    !               under field name.

    call ESMF_AttributeSet(SST, 'SpatialRegriddingDimension', &
                                '2D', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(SST, 'Frequency', '5 Months', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Frequency" under tab "Inputs", under field name.

    call ESMF_AttributeSet(SST, 'TimeTransformationType', &
                                'TimeAverage', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Time Transformation Type" under tab "Inputs",
    !               under field name.


    ! Create a FieldBundle for the two Fields
    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle2", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Add the Fields to the FieldBundle (this will connect the Attribute
    ! hierarchies of the FieldBundle and Fields)
    call ESMF_FieldBundleAdd(fieldbundle, (/DMS_emi/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleAdd(fieldbundle, (/SST/), rc=rc)
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

    call ESMF_StateGet(exportState, "fieldbundle2", fieldbundle, rc=rc)
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

end module user_model2

!\end{verbatim}
