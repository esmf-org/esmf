! $Id: user_model3.F90,v 1.4 2011/06/27 22:31:16 rokuingh Exp $
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

module user_model3

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
  
  private
    
  public userm3_setvm, userm3_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm3_setvm(comp, rc)
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

  subroutine userm3_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
      rc=rc)
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
    character(ESMF_MAXSTR)      :: convCIM, purpComp, purpField
    character(ESMF_MAXSTR)      :: convISO, purpRP
    type(ESMF_Field)            :: Ozone, SST
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
    convCIM = 'CIM 1.5'
    purpComp = 'Model Component Simulation Description'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeSet(comp, 'ShortName', &
                           'HiGEM AtmosDynCore', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'LongName', &
                           'Dynamical core of HiGEM_Atmos', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2009-10-31T23:59:59Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ModelType', &
      'AtmosDynamicalCore', convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Responsible party attributes (for Contact)
    convISO = 'ISO 19115'
    purpRP = 'Responsible Party Description'
    call ESMF_AttributeSet(comp, 'Name', &
     'Jane Doe', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Meteorology University of DEF', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'jane.doe@udef.edu', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'Contact', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Create two Fields, and add CIM Attribute packages.
    ! The standard Attribute package currently supplied by ESMF for 
    ! CIM Fields contains a standard CF-Extended package nested within it.
    convCIM = 'CIM 1.5'
    purpField = 'Inputs Description'

    ! Ozone Field
    Ozone = ESMF_FieldEmptyCreate(name='Ozone', rc=rc)
    call ESMF_AttributeAdd(Ozone, convention=convCIM, purpose=purpField,rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Ozone CF-Extended Attributes
    call ESMF_AttributeSet(Ozone, 'ShortName', 'Global_O3_mon', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Ozone, 'StandardName', 'Ozone', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Ozone, 'LongName', 'Ozone', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Ozone, 'Units', 'unknown', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Ozone CIM Attributes
    call ESMF_AttributeSet(Ozone, 'CouplingPurpose', 'Boundary', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Ozone, 'CouplingSource', &
                                  'Global_O3_mon', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Ozone, 'CouplingTarget', &
                                  'HiGEM_Atmos', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Ozone, 'SpatialRegriddingMethod', &
                                  'Conservative-Second-Order', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Ozone, 'SpatialRegriddingDimension', &
                                  '3D', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Ozone, 'Frequency', '20 Days', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Ozone, 'TimeTransformationType', &
                                  'TimeInterpolation', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! SST Field
    SST = ESMF_FieldEmptyCreate(name='SST', rc=rc)
    call ESMF_AttributeAdd(SST, convention=convCIM, purpose=purpField,rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! SST CF-Extended Attributes
    call ESMF_AttributeSet(SST, 'ShortName', 'SST', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! SST CIM Attributes
    call ESMF_AttributeSet(SST, 'CouplingPurpose', 'Initial', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(SST, 'CouplingSource', &
                                'seasonal_oxidant_conc', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(SST, 'CouplingTarget', &
                                'HiGEM_Atmos', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(SST, 'SpatialRegriddingMethod', &
                                'Non-Conservative', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(SST, 'SpatialRegriddingDimension', &
                                '2D', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(SST, 'Frequency', '5 Months', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(SST, 'TimeTransformationType', &
                                'TimeAverage', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Create a FieldBundle for the two Fields
    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle3", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
      
    ! Add the Fields to the FieldBundle (this will link the Attribute
    ! hierarchies of the FieldBundle and Fields)
    call ESMF_FieldBundleAdd(fieldbundle, (/Ozone/), rc=rc)
    call ESMF_FieldBundleAdd(fieldbundle, (/SST/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Link the Attributes from the FieldBundle to the export State
    call ESMF_StateAdd(exportState, fieldbundle=fieldbundle, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    
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

    convCIM = 'CIM 1.5'
    purpComp = 'Model Component Simulation Description'

    ! Initialize return code
    rc = ESMF_SUCCESS

#if 0
    ! for testing ESMF_AttributeUpdate()
    call ESMF_AttributeRemove(comp, name="ReleaseDate", &
      purpose=purpComp, convention=convCIM ,rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    attrVal = "Test change"
    call ESMF_AttributeSet(comp, name="Name", &
      value=attrVal, &
      convention=convCIM, purpose=purpComp, rc=rc)
#endif

    if (rc .ne. ESMF_SUCCESS) return
                                                             
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
    
    call ESMF_StateGet(exportState, "fieldbundle3", fieldbundle, rc=rc)
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

end module user_model3
    
!\end{verbatim}
