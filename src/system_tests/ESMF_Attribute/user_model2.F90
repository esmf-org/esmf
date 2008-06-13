! $Id: user_model2.F90,v 1.8 2008/06/13 00:29:34 theurich Exp $
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
  use ESMF_Mod

  implicit none
    
  public userm2_register
  
  type(ESMF_Field)            :: aero,delp,dewl,hi,ox,ple,s,speed,taux,zle
  type(ESMF_FieldBundle)      :: fbundle

  
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm2_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
#endif

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

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, supportPthreadsFlag=supportPthreads, rc=rc)
    if (supportPthreads) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
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
    type(ESMF_VM)          :: vm
    integer                :: petCount, status, myPet
    character(ESMF_MAXSTR) :: name1,name2,name3,name4,value1,value2,value3,value4,conv,purp
    
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
    conv = 'netCDF'
    purp = 'basic'
    name1 = 'shortname'
    name2 = 'longname'
    name3 = 'units'
    name4 = 'dimensions'
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'aero'
    value2 = 'aerosols'
    value3 = 'unitless'
    value4 = 'xy'
      
    aero = ESMF_FieldCreateEmpty("aero", rc=status)
    call ESMF_AttributeAdd(aero, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(aero, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(aero, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(aero, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(aero, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'delp'
    value2 = 'pressure thickness'
    value3 = 'Pa'
    value4 = 'xyz'

    delp = ESMF_FieldCreateEmpty("delp", rc=status)
    call ESMF_AttributeAdd(delp, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(delp, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(delp, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(delp, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(delp, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'dewl'
    value2 = 'dewfall'
    value3 = 'kg m-2 s-1'
    value4 = 'xy'
      
    dewl = ESMF_FieldCreateEmpty("dewl", rc=status)
    call ESMF_AttributeAdd(dewl, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(dewl, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(dewl, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(dewl, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(dewl, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'hi'
    value2 = 'sea ice skin layer depth'
    value3 = 'm'
    value4 = 'xy'
      
    hi = ESMF_FieldCreateEmpty("hi", rc=status)
    call ESMF_AttributeAdd(hi, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(hi, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(hi, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(hi, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(hi, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'ox'
    value2 = 'odd oxygen mixing ratio'
    value3 = 'pppv'
    value4 = 'xy'
      
    ox = ESMF_FieldCreateEmpty("ox", rc=status)
    call ESMF_AttributeAdd(ox, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(ox, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(ox, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(ox, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(ox, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'ple'
    value2 = 'air pressure'
    value3 = 'Pa'
    value4 = 'xy'
      
    ple = ESMF_FieldCreateEmpty("ple", rc=status)
    call ESMF_AttributeAdd(ple, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(ple, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(ple, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(ple, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(ple, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 's'
    value2 = 'dry static energy'
    value3 = 'm+2 s-2'
    value4 = 'xy'
      
    s = ESMF_FieldCreateEmpty("s", rc=status)
    call ESMF_AttributeAdd(s, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(s, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(s, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(s, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(s, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'speed'
    value2 = 'surface wind speed'
    value3 = 'm s-1'
    value4 = 'xy'
      
    speed = ESMF_FieldCreateEmpty("speed", rc=status)
    call ESMF_AttributeAdd(speed, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(speed, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(speed, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(speed, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(speed, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'taux'
    value2 = 'eastward surface stress'
    value3 = 'N m-2'
    value4 = 'xy'
      
    taux = ESMF_FieldCreateEmpty("taux", rc=status)
    call ESMF_AttributeAdd(taux, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(taux, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(taux, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(taux, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(taux, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! create a field, make an attribute package, and set attributes in the package
    value1 = 'zle'
    value2 = 'geopotential height'
    value3 = 'm'
    value4 = 'xyz'
      
    zle = ESMF_FieldCreateEmpty("zle", rc=status)
    call ESMF_AttributeAdd(zle, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(zle, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(zle, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(zle, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(zle, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
      
    ! create a field bundle for the first five fields from above
    fbundle = ESMF_FieldBundleCreate(name="fbundle", rc=status)
    if (status .ne. ESMF_SUCCESS) return
      
    ! connect the attrs from the first five fields above to the field bundle
    call ESMF_AttributeSet(fbundle, aero, rc=status)
    call ESMF_AttributeSet(fbundle, delp, rc=status)
    call ESMF_AttributeSet(fbundle, dewl, rc=status)
    call ESMF_AttributeSet(fbundle, hi, rc=status)
    call ESMF_AttributeSet(fbundle, ox, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! connect the attrs from the remaining five fields directly to the export state
    call ESMF_AttributeSet(exportState, ple, rc=status)
    call ESMF_AttributeSet(exportState, s, rc=status)
    call ESMF_AttributeSet(exportState, speed, rc=status)
    call ESMF_AttributeSet(exportState, taux, rc=status)
    call ESMF_AttributeSet(exportState, zle, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! connect the attrs from the field bundle to the export state
    call ESMF_AttributeSet(exportState, fbundle, rc=status)
    if (status .ne. ESMF_SUCCESS) return
  
    ! Don't delete the fields so we can copy attrs (shallow) and reuse
    
    rc = ESMF_SUCCESS
    return
    
    ! get here only on error exit
10  continue
    print *, 'FAILURE in Comp 2 Init!!!!'
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
    integer                     :: petCount, status, myPet
    character(ESMF_MAXSTR)      :: conv,purp
    
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
    conv = 'netCDF'
    purp = 'basic'

    if (myPet .eq. 0) then
      call ESMF_AttributeWrite(importState,conv,purp,rc=rc)
      call ESMF_AttributeWrite(exportState,conv,purp,rc=rc)
      if (rc .ne. ESMF_SUCCESS) return
    endif

    rc = ESMF_SUCCESS
    return
                                                             
    ! get here only on error exit
20  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in COMP2 Run!!!"
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
    
    call ESMF_FieldDestroy(aero,rc=rc)
    call ESMF_FieldDestroy(delp,rc=rc)
    call ESMF_FieldDestroy(dewl,rc=rc)
    call ESMF_FieldDestroy(hi,rc=rc)
    call ESMF_FieldDestroy(ox,rc=rc)
    call ESMF_FieldDestroy(ple,rc=rc)
    call ESMF_FieldDestroy(s,rc=rc)
    call ESMF_FieldDestroy(speed,rc=rc)
    call ESMF_FieldDestroy(taux,rc=rc)
    call ESMF_FieldDestroy(zle,rc=rc)
    call ESMF_FieldBundleDestroy(fbundle,rc=rc)

    ! get here only on error exit
30  continue
    rc = ESMF_FAILURE
    print *, "FAILURE in COMP2 Finalize!!!"
    return

  end subroutine user_final


end module user_model2
    
!\end{verbatim}
