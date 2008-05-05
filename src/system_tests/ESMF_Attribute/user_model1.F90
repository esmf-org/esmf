! $Id: user_model1.F90,v 1.21 2008/05/05 07:45:10 rokuingh Exp $
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
    
  public userm1_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
#endif
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)         :: vm
    integer               :: petCount
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    rc = ESMF_SUCCESS
    return
    
    ! get here only on error exit
10  continue
    print *, 'FAILURE in Comp 1 Init!!!!'
    rc = ESMF_FAILURE

  end subroutine user_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)               :: vm
    type(ESMF_DistGrid)         :: distgrid
    type(ESMF_ArraySpec)        :: arrayspec
    type(ESMF_Array)            :: array
    type(ESMF_Grid)             :: grid
    type(ESMF_Field)            :: evap,h250,omega,salt,sh,sno,so4,t2m,ua,wet1
    type(ESMF_FieldBundle)      :: fbundle
    integer                     :: petCount, status, myPet
    character(len=ESMF_MAXSTR)  :: name1,name2,name3,name4,value1,value2,value3,value4
    character(len=ESMF_MAXSTR)  :: conv, purp, fconv, fpurp, outvalue

    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Get the export state
    call ESMF_StateGet(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! initialize variables
    fconv = 'netCDF'
    fpurp = 'basic'
    name1 = 'shortname'
    name2 = 'longname'
    name3 = 'units'
    name4 = 'dimensions'
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'evap'
    value2 = 'evaporation from turbulence'
    value3 = 'm-2 s-1'
    value4 = 'xy'
      
    evap = ESMF_FieldCreate("evap", rc=status)
    call ESMF_AttributeAdd(evap, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(evap, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(evap, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(evap, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(evap, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'h250'
    value2 = 'height at 250 Pa'
    value3 = 'm'
    value4 = 'xy'
      
    h250 = ESMF_FieldCreate("h250", rc=status)
    call ESMF_AttributeAdd(h250, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(h250, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(h250, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(h250, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(h250, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'omega'
    value2 = 'verticle pressure velocity'
    value3 = 'Pa s-1'
    value4 = 'xyz'
      
    omega = ESMF_FieldCreate("omega", rc=status)
    call ESMF_AttributeAdd(omega, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(omega, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(omega, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(omega, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(omega, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'salt'
    value2 = 'sea salt mixing ratio'
    value3 = 'unitless'
    value4 = 'xyz'
      
    salt = ESMF_FieldCreate("salt", rc=status)
    call ESMF_AttributeAdd(salt, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(salt, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(salt, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(salt, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(salt, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'sh'
    value2 = 'sensible heat flux from turbulence'
    value3 = 'W m-2'
    value4 = 'xy'
      
    sh = ESMF_FieldCreate("sh", rc=status)
    call ESMF_AttributeAdd(sh, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(sh, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(sh, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(sh, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(sh, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'sno'
    value2 = 'snowfall'
    value3 = 'kg m-2 s-1'
    value4 = 'xy'
      
    sno = ESMF_FieldCreate("sno", rc=status)
    call ESMF_AttributeAdd(sno, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(sno, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(sno, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(sno, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(sno, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'so4'
    value2 = 'sulfate aerosol mixing ratio'
    value3 = 'unitless'
    value4 = 'xyz'
      
    so4 = ESMF_FieldCreate("so4", rc=status)
    call ESMF_AttributeAdd(so4, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(so4, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(so4, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(so4, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(so4, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 't2m'
    value2 = '2-meter air temperature'
    value3 = 'K'
    value4 = 'xyz'
      
    t2m = ESMF_FieldCreate("t2m", rc=status)
    call ESMF_AttributeAdd(t2m, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(t2m, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(t2m, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(t2m, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(t2m, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'ua'
    value2 = 'surface eastward wind'
    value3 = 'm s-1'
    value4 = 'xy'
      
    ua = ESMF_FieldCreate("ua", rc=status)
    call ESMF_AttributeAdd(ua, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(ua, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(ua, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(ua, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(ua, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'wet1'
    value2 = 'surface soil wetness'
    value3 = 'unitless'
    value4 = 'xyz'
      
    wet1 = ESMF_FieldCreate("wet1", rc=status)
    call ESMF_AttributeAdd(wet1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(wet1, name1, value1, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(wet1, name2, value2, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(wet1, name3, value3, convention=fconv, purpose=fpurp, rc=status)
    call ESMF_AttributeSet(wet1, name4, value4, convention=fconv, purpose=fpurp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
      
    ! create a field bundle for the first five fields from above
    fbundle = ESMF_FieldBundleCreate(name="fbundle", rc=status)
    if (status .ne. ESMF_SUCCESS) return
      
    ! connect the attrs from the first five fields above to the field bundle
    call ESMF_AttributeSet(fbundle, evap, rc=status)
    call ESMF_AttributeSet(fbundle, h250, rc=status)
    call ESMF_AttributeSet(fbundle, omega, rc=status)
    call ESMF_AttributeSet(fbundle, salt, rc=status)
    call ESMF_AttributeSet(fbundle, sh, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! connect the attrs from the remaining five fields directly to the export state
    call ESMF_AttributeSet(exportState, sno, rc=status)
    call ESMF_AttributeSet(exportState, so4, rc=status)
    call ESMF_AttributeSet(exportState, t2m, rc=status)
    call ESMF_AttributeSet(exportState, ua, rc=status)
    call ESMF_AttributeSet(exportState, wet1, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! connect the attrs from the field bundle to the export state
    call ESMF_AttributeSet(exportState, fbundle, rc=status)
    if (status .ne. ESMF_SUCCESS) return
  
    ! Don't delete the fields so we can copy attrs (shallow) and reuse

    rc = ESMF_SUCCESS
    return
                                                             
    ! get here only on error exit
20  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in COMP1 Run!!!"
    return

  end subroutine user_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS
    return

    ! get here only on error exit
30  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in COMP1 Finalize!!!"
    return

  end subroutine user_final


end module user_model1
    
!\end{verbatim}
