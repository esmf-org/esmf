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

module user_model1

  ! ESMF Framework module
  use ESMF

  implicit none
  
  private
    
  public userm1_setvm, userm1_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm1_setvm(comp, rc)
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

  subroutine userm1_register(comp, rc)
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
    type(ESMF_Field)            :: OH, Orog
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
    ! Specify the Gridded Components to have the default of 1 Responsible
    !   Party sub-package and 1 Citation sub-package
    call ESMF_AttributeAdd(comp, convention=convCIM, &
      purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'ShortName', 'EarthSys_Atmos', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  1) Name of component in navigator bar on the left; 
    !                  attribute 'Version' appended, if set.
    !               2) Also "Simulation Metadata:", for top-level component, 
    !                  first part of display, at top, 1st line, prepended to
    !                  top-level component's attributes 'Version' (if set) and
    !                  'SimulationShortName'.

    call ESMF_AttributeSet(comp, 'LongName', &
                           'Atmosphere component of the EarthSys model', &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Full Name:"  first part of display, at top, 2nd line 
    !               under title, prepended to attribute 'SimulationLongName'.

    call ESMF_AttributeSet(comp, 'Description', &
      'The EarthSys atmosphere model has a horizontal resolution of 1.125 ' // &
      'degrees of latitude by 1.75 degrees of longitude with 36 layers ' // &
      'in the vertical. The atmospheric timestep period is 30 minutes.', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Description:" in top box.

    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2009-12-31T23:59:59Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Release Date" under tabs "Properties->Basic".

    call ESMF_AttributeSet(comp, 'ModelType', &
      'Atmosphere', convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Maps to "Realm:", expanded under component name, in 
    !               navigator bar on the left.


    ! Responsible party attributes (for Principal Investigator)
    convISO = 'ISO 19115'
    purpRP = 'RespParty'
    call ESMF_AttributeGetAttPack(comp, convISO, purpRP, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_AttributeSet(comp, 'Name', &
     'John Doe', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Principal Investigator" under tabs "Properties->Basic".

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
     'Author', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Ingested, but only used to control display.


    ! Citation attributes
    convISO = 'ISO 19115'
    purpCitation = 'Citation'
    call ESMF_AttributeGetAttPack(comp, convISO, purpCitation, attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    call ESMF_AttributeSet(comp, 'ShortTitle', &
     'Doe_2008', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.
  
    call ESMF_AttributeSet(comp, 'LongTitle', &
     'Doe, J.A.; Doe, S.B.; ' // &
     'Doe, J.C.; 2008 EarthSys: ' // &
     'The Earth System High Resolution Global Model - ' // &
     'Atmosphere model description . Journal of Earth Modeling, 14 (3). ' // &
     '1361-1396.', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Reference", concatenated with attribute 'DOI', under 
    !               tab "References".

    call ESMF_AttributeSet(comp, 'Date', &
     '2008-04-06', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'PresentationForm', &
     'Online Refereed', &
      convention=convISO, purpose=purpCitation, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(comp, 'DOI', &
     'doi:16.1034/2008JCLI4507.1', &
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
    sciPropAtt(1) = 'AtmosphereAtmosConvectTurbulCloudMicrophysicsProcesses'
    sciPropAtt(2) = 'AtmosphereAtmosConvectTurbulCloudAtmosCloudSchemeCloudSchemeAttributesSeparatedCloudTreatment'
    sciPropAtt(3) = 'AtmosphereAtmosConvectTurbulCloudCloudSimulatorInputsRadarRadarType'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpSci, &
      attrList=sciPropAtt, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Scientific Properties:  attributes per Metafor standard
    call ESMF_AttributeSet(comp, &
      'AtmosphereAtmosConvectTurbulCloudMicrophysicsProcesses', &
        'effect of snow', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs 
    !               "Properties->Scientific"

    call ESMF_AttributeSet(comp, &
      'AtmosphereAtmosConvectTurbulCloudAtmosCloudSchemeCloudSchemeAttributesSeparatedCloudTreatment', &
        'yes', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs 
    !               "Properties->Scientific"

    call ESMF_AttributeSet(comp, &
      'AtmosphereAtmosConvectTurbulCloudCloudSimulatorInputsRadarRadarType', &
        'spaceborne', &
      convention=convCIM, purpose=purpSci, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  ESG-name mapped from Metafor-name, under tabs 
    !               "Properties->Scientific"


    ! Create two Fields, and add CIM Attribute packages.
    ! The standard Attribute package currently supplied by ESMF for 
    ! CIM Fields contains a standard CF-Extended package nested within it.
    convCIM = 'CIM 1.7.1'
    purpField = 'Inputs'


    ! OH Field
    OH = ESMF_FieldEmptyCreate(name='OH', rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_AttributeAdd(OH, convention=convCIM, purpose=purpField,rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    ! OH CF-Extended Attributes
    call ESMF_AttributeSet(OH, 'ShortName', 'OH_Conc_1900', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  As field name under tab "Inputs".

    call ESMF_AttributeSet(OH, 'StandardName', &
                               'OH_Concentrations', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(OH, 'LongName', &
                               'seasonal_oxidant_conc', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(OH, 'Units', 'kg/m3', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! OH CIM Attributes
    call ESMF_AttributeSet(OH, 'CouplingPurpose', 'Boundary', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Title of expandable bar under tab "Inputs", 
    !               "Boundary Conditions".

    call ESMF_AttributeSet(OH, 'CouplingSource', &
                               'EarthSys_Atmos', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Source Component" under tab "Inputs", 
    !               under field name. 

    call ESMF_AttributeSet(OH, 'CouplingTarget', &
                               'EarthSys_AtmosDynCore', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Target Component" under tab "Inputs", 
    !               under field name.

    call ESMF_AttributeSet(OH, 'Description', &
                               'Seasonal oxidant concentration in ' // &
                               'the atmosphere.', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Next to field name (after colon) under tab "Inputs".

    call ESMF_AttributeSet(OH, 'SpatialRegriddingMethod', &
                               'Near-Neighbor', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Spatial Regridding Method" under tab "Inputs",
    !               under field name. 

    call ESMF_AttributeSet(OH, 'SpatialRegriddingDimension', &
                               '2D', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(OH, 'Frequency', '10 Hours', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Frequency" under tab "Inputs", under field name.

    call ESMF_AttributeSet(OH, 'TimeTransformationType', &
                               'TimeInterpolation', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Time Transformation Type" under tab "Inputs", 
    !               under field name. 

    
    ! Orog Field
    Orog = ESMF_FieldEmptyCreate(name='Orog', rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_AttributeAdd(Orog, convention=convCIM, purpose=purpField,rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out


    ! Orog CF-Extended Attributes
    call ESMF_AttributeSet(Orog, 'ShortName', 'UM_Orog_n320', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  As field name under tab "Inputs".

    call ESMF_AttributeSet(Orog, 'StandardName', 'Height', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(Orog, 'LongName', 'Orography', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(Orog, 'Units', 'm', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.


    ! Orog CIM Attributes
    call ESMF_AttributeSet(Orog, 'CouplingPurpose', 'Initial', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Title of expandable bar under tab "Inputs", 
    !               "Boundary Conditions".

    call ESMF_AttributeSet(Orog, 'CouplingSource', &
                                 'EarthSys_Atmos', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Source Component" under tab "Inputs", 
    !               under field name. 

    call ESMF_AttributeSet(Orog, 'CouplingTarget', &
                                 'EarthSys_AtmosDynCore', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Target Component" under tab "Inputs", 
    !               under field name.

    call ESMF_AttributeSet(Orog, 'Description', &
                                 'Orography/height data in meters at ' // &
                                 'n320 resolution.', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Next to field name (after colon) under tab "Inputs".

    call ESMF_AttributeSet(Orog, 'SpatialRegriddingMethod', &
                                 'Conservative', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Spatial Regridding Method" under tab "Inputs",
    !               under field name. 

    call ESMF_AttributeSet(Orog, 'SpatialRegriddingDimension', &
                                 '2D', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  Not ingested, as of ESG 1.3.1.

    call ESMF_AttributeSet(Orog, 'TimeTransformationType', 'Exact', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    ! ESG Display:  "Input Time Transformation Type" under tab "Inputs", 
    !               under field name. 


    ! Create a FieldBundle for the two Fields
    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle1", rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
      
    ! Add the Fields to the FieldBundle (this will link the Attribute
    ! hierarchies of the FieldBundle and Fields)
    call ESMF_FieldBundleAdd(fieldbundle, (/OH/), rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleAdd(fieldbundle, (/Orog/), rc=rc)
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

    character(ESMF_MAXSTR)      :: convCIM, purpComp, attrVal

    convCIM = 'CIM 1.7.1'
    purpComp = 'ModelComp'

    ! Initialize return code
    rc = ESMF_SUCCESS

#if 0
    ! for testing ESMF_AttributeUpdate()
    call ESMF_AttributeRemove(comp, name="ReleaseDate", &
      purpose=purpComp, convention=convCIM ,rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    attrVal = "Test change"
    call ESMF_AttributeSet(comp, name="Name", &
      value=attrVal, &
      convention=convCIM, purpose=purpComp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
#endif
                                                             
  end subroutine user_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fieldbundle
    integer                     :: k

    ! Initialize return code
    rc = ESMF_SUCCESS
    
    call ESMF_StateGet(exportState, "fieldbundle1", fieldbundle, rc=rc)
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

end module user_model1
    
!\end{verbatim}
