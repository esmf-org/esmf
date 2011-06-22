! $Id: user_model4.F90,v 1.3 2011/06/22 17:08:30 feiliu Exp $
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
  use ESMF_Mod

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
    type(ESMF_Field)            :: SO2, NOx
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
                           'POP2', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'LongName', &
                           'Parallel Ocean Program', &
      convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ReleaseDate', &
      '2010-06-10T00:00:00Z', &
        convention=convCIM, purpose=purpComp, rc=rc)
    call ESMF_AttributeSet(comp, 'ModelType', &
      'Ocean', convention=convCIM, purpose=purpComp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Responsible party attributes (for Funder)
    convISO = 'ISO 19115'
    purpRP = 'Responsible Party Description'
    call ESMF_AttributeSet(comp, 'Name', &
     'Sally Doe', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'PhysicalAddress', &
     'Department of Oceanography University of DEF', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'EmailAddress', &
     'sally.doe@udef.edu', &
      convention=convISO, purpose=purpRP, rc=rc)
    call ESMF_AttributeSet(comp, 'ResponsiblePartyRole', &
     'Funder', &
      convention=convISO, purpose=purpRP, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! Create two Fields, and add CIM Attribute packages.
    ! The standard Attribute package currently supplied by ESMF for 
    ! CIM Fields contains a standard CF-Extended package nested within it.
    convCIM = 'CIM 1.5'
    purpField = 'Inputs Description'

    ! SO2 Field
    SO2 = ESMF_FieldEmptyCreate(name='SO2', rc=rc)
    call ESMF_AttributeAdd(SO2, convention=convCIM, purpose=purpField,rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! SO2 CF-Extended Attributes
    call ESMF_AttributeSet(SO2, 'ShortName', 'SO2', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! SO2 CIM Attributes
    call ESMF_AttributeSet(SO2, 'CouplingPurpose', 'Boundary', &
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
    call ESMF_AttributeSet(SO2, 'Frequency', '2 Years', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(SO2, 'TimeTransformationType', &
                                'Exact', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    
    ! NOx Field
    NOx = ESMF_FieldEmptyCreate(name='NOx', rc=rc)
    call ESMF_AttributeAdd(NOx, convention=convCIM, purpose=purpField,rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! NOx CF-Extended Attributes
    call ESMF_AttributeSet(NOx, 'ShortName', 'NOx', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    ! NOx CIM Attributes
    call ESMF_AttributeSet(NOx, 'CouplingPurpose', 'Initial', &
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
    call ESMF_AttributeSet(NOx, 'Frequency', '45 Seconds', &
         convention=convCIM, purpose=purpField, rc=rc)
    call ESMF_AttributeSet(NOx, 'TimeTransformationType', &
                                'TimeAccumulation', &
         convention=convCIM, purpose=purpField, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
   
    ! Create a FieldBundle for the two Fields
    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle4", rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
      
    ! Add the Fields to the FieldBundle (this will connect the Attribute
    ! hierarchies of the FieldBundle and Fields)
    call ESMF_FieldBundleAdd(fieldbundle, (/SO2/), rc=rc)
    call ESMF_FieldBundleAdd(fieldbundle, (/NOx/), rc=rc)
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
