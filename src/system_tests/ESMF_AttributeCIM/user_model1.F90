! $Id: user_model1.F90,v 1.5 2010/10/09 03:01:06 eschwab Exp $
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
  use ESMF_Mod

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
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
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
    type(ESMF_Field)            :: DMS_emi, UM
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
    convCIM = 'CIM 1.0'
    purpComp = 'Model Component Simulation Description'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeSet(comp, 'ShortName', &
      'HiGEM_Atmos', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'LongName', &
      'Atmosphere component of the HiGEM model', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2009', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Responsible party attributes (for Principal Investigator)
    call ESMF_AttributeSet(comp, 'Name', &
      'Gerard Devine', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Meteorology University of Reading Earley Gate, Reading UK',&
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'EmailAddress', &
      'g.m.devine@reading.ac.uk', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
      'Author', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Create two Fields, and add CIM Attribute packages.
    ! The standard Attribute package currently supplied by ESMF for 
    ! CIM Fields contains a standard CF-Extended package nested within it.
    convCIM = 'CIM 1.0'
    purpField = 'Inputs Description'

    ! DMS_emi Field
    DMS_emi = ESMF_FieldCreateEmpty(name='DMS_emi', rc=rc)
    call ESMF_AttributeAdd(DMS_emi, convention=convCIM, purpose=purpField,rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! DMS_emi CF-Extended Attributes
    call ESMF_AttributeSet(DMS_emi, 'ShortName', 'DMS_emi', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(DMS_emi, 'StandardName', 'DMS_emissions', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(DMS_emi, 'LongName', 'DMS emissions', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(DMS_emi, 'Units', 'unknown', &
        convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! DMS_emi CIM Attributes
    call ESMF_AttributeSet(DMS_emi, 'CouplingPurpose', 'boundaryCondition', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(DMS_emi, 'CouplingTarget', &
                                    'HiGEM_AtmosChem', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(DMS_emi, 'SpatialRegriddingMethod', &
                                    'conservativeSpatialRegridding', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(DMS_emi, 'SpatialRegriddingType', 'TBD', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(DMS_emi, 'Frequency', '15 minutes', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(DMS_emi, 'TimeTransformationType', &
                                    'TimeAverage', &
        convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! UM Field
    UM = ESMF_FieldCreateEmpty(name='UM', rc=rc)
    call ESMF_AttributeAdd(UM, convention=convCIM, purpose=purpField,rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! UM CF-Extended Attributes
    call ESMF_AttributeSet(UM, 'ShortName', 'UM_Initial_1960', &
        convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! UM CIM Attributes
    call ESMF_AttributeSet(UM, 'CouplingPurpose', 'initialCondition', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(UM, 'CouplingTarget', 'HiGEM_AtmosChem', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(UM, 'TimeTransformationType', 'Exact', &
        convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Create a FieldBundle for the two Fields
    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle1", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
      
    ! Add the Fields to the FieldBundle (this will link the Attribute
    ! hierarchies of the FieldBundle and Fields)
    call ESMF_FieldBundleAdd(fieldbundle, DMS_emi, rc=rc)
    call ESMF_FieldBundleAdd(fieldbundle, UM, rc=rc)
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

    convCIM = 'CIM 1.0'
    purpComp = 'Model Component Simulation Description'

    ! Initialize return code
    rc = ESMF_SUCCESS

#if 0
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
