! $Id: user_model2.F90,v 1.10 2010/11/03 04:58:56 theurich Exp $
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
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

  subroutine userm2_register(comp, rc)
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
    character(ESMF_MAXSTR)      :: convISO, purpRP
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
    convCIM = 'CIM 1.0'
    purpComp = 'Model Component Simulation Description'
    call ESMF_AttributeAdd(comp, convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_AttributeSet(comp, 'ShortName', &
      'HiGEM_AtmosChem', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'LongName', &
      'Atmospheric chemistry component of HiGEM', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ModelType', &
      'AtmosphericChemistry', &
        convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Responsible party attributes (for Principal Investigator)
    convISO = 'ISO 19115'
    purpRP = 'Responsible Party Description'
    call ESMF_AttributeSet(comp, 'Name', &
      'Gerard Devine', &
        convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Meteorology University of Reading Earley Gate, Reading UK',&
        convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'EmailAddress', &
      'g.m.devine@reading.ac.uk', &
        convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
      'PI', &
        convention=convISO, purpose=purpRP, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Create two Fields, and add CIM Attribute packages.
    ! The standard Attribute package currently supplied by ESMF for 
    ! CIM Fields contains a standard CF-Extended package nested within it.
    convCIM = 'CIM 1.0'
    purpField = 'Inputs Description'

    ! OH Field
    OH = ESMF_FieldCreateEmpty(name='OH', rc=rc)
    call ESMF_AttributeAdd(OH, convention=convCIM, purpose=purpField,rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! OH CF-Extended Attributes
    call ESMF_AttributeSet(OH, 'ShortName', 'OH_Conc_1900', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(OH, 'StandardName', 'OH_Concentrations', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(OH, 'LongName', 'seasonal_oxidant_conc', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(OH, 'Units', 'unknown', &
        convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! OH CIM Attributes
    call ESMF_AttributeSet(OH, 'CouplingPurpose', 'boundaryCondition', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(OH, 'CouplingTarget', 'HiGEM_AtmosChem', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(OH, 'SpatialRegriddingMethod', &
                               'conservative-first-order', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(OH, 'Frequency', '15 minutes', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(OH, 'TimeTransformationType', &
                               'TimeInterpolation', &
        convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    
    ! Orog Field
    Orog = ESMF_FieldCreateEmpty(name='Orog', rc=rc)
    call ESMF_AttributeAdd(Orog, convention=convCIM, purpose=purpField,rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Orog CF-Extended Attributes
    call ESMF_AttributeSet(Orog, 'ShortName', 'UM_Orog_n320', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Orog, 'StandardName', 'Height', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Orog, 'LongName', 'Orography', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Orog, 'Units', 'unknown', &
        convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Orog CIM Attributes
    call ESMF_AttributeSet(Orog, 'CouplingPurpose', 'initialCondition', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Orog, 'CouplingTarget', 'HiGEM_Atmos', &
        convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(Orog, 'TimeTransformationType', 'Exact', &
        convention=convCIM, purpose=purpField, rc=rc)
   
    ! Create a FieldBundle for the two Fields
    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle2", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
      
    ! Add the Fields to the FieldBundle (this will connect the Attribute
    ! hierarchies of the FieldBundle and Fields)
    call ESMF_FieldBundleAdd(fieldbundle, OH, rc=rc)
    call ESMF_FieldBundleAdd(fieldbundle, Orog, rc=rc)
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
