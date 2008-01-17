! $Id: user_model1.F90,v 1.7 2008/01/17 00:19:06 rokuingh Exp $
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
    type(ESMF_Field)            :: field
    type(ESMF_Grid)             :: grid
    type(ESMF_Array)            :: array
    type(ESMF_DistGrid)         :: distgrid
    type(ESMF_ArraySpec)        :: arrayspec
    integer                     :: petCount, backward_run, status, myPet
    character(len=ESMF_MAXSTR)  :: name, value, conv, purp, fconv, fpurp   
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) goto 20

    ! Get the direction of the run
    call ESMF_StateGetAttributeInfo(importState, "backward_run", rc=backward_run)
    if (status .ne. ESMF_SUCCESS) goto 20

    ! if this is the forward interpolation
    if (backward_run .ne. ESMF_SUCCESS) then
      conv = 'ESG-CDP'  
      purp = 'general'
      name = 'name'
      value = 'State attribute package'
      call ESMF_StateSetAttPack(importState, name, value, convention=conv, purpose=purp, rc=rc)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      if (myPet .eq. 0) then
        print *, 'Write the Attpack from the first run of component 1.'
    
        call ESMF_StateWriteAttPack(importState, convention=conv, purpose=purp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20
      endif
    endif

    ! if this is the backward interpolation check error
    if (backward_run .eq. ESMF_SUCCESS) then
      ! state stuff
      conv = 'ESG-CDP'  
      purp = 'general'
      name = 'discipline'
      value = 'Sir, Yes! Sir.'
      call ESMF_StateSetAttPack(importState, name, value, convention=conv, purpose=purp, rc=rc)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      ! field stuff
      fconv = 'netCDF'
      fpurp = 'basic'
      name = 'longname'
      value = 'precipitation'
      
      field = ESMF_FieldCreateEmpty("field1", rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      call ESMF_FieldCreateAttPack(field, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      call ESMF_FieldSetAttPack(field, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      ! grid stuff
      grid = ESMF_GridCreateEmpty(rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      call ESMF_GridSetAttribute(grid, name, value, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      if (myPet .eq. 0) then
        print *, 'Set an attribute on the grid with:'
        print *, 'name: ', name
        print *, 'value: ', value
      endif
      
      call ESMF_GridGetAttribute(grid, name, value, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      if (myPet .eq. 0) then
        print *, 'Get an attribute on the grid with:'
        print *, 'name: ', name
        print *, 'value: ', value
      endif

      call ESMF_GridCreateAttPack(grid, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      call ESMF_GridSetAttPack(grid, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      ! array stuff
      call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
      distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
        regDecomp=(/2,3/), rc=rc)
      array = ESMF_ArrayCreate(arrayspec, distgrid, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      call ESMF_ArraySetAttribute(array, name, value, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      if (myPet .eq. 0) then
        print *, 'Set an attribute on the array with:'
        print *, 'name: ', name
        print *, 'value: ', value
      endif
      
      call ESMF_ArrayGetAttribute(array, name, value, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      if (myPet .eq. 0) then
        print *, 'Get an attribute on the array with:'
        print *, 'name: ', name
        print *, 'value: ', value
      endif

      call ESMF_ArrayCreateAttPack(array, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20
      
      call ESMF_ArraySetAttPack(array, name, value, convention=fconv, purpose=fpurp, rc=status)
      if (status .ne. ESMF_SUCCESS) goto 20

      if (myPet .eq. 0) then
        conv = 'ESG-CDP'
        purp = 'general'
      
        print *, 'Write the State Attpack from the second run of component 1.'
        call ESMF_StateWriteAttPack(importState, convention=conv, purpose=purp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20
        
        print *, 'Write the Field Attpack from the second run of component 1.'
        call ESMF_FieldWriteAttPack(field, convention=fconv, purpose=fpurp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20
        
        print *, 'Write the Grid Attpack from the second run of component 1.'
        call ESMF_GridWriteAttPack(grid, convention=fconv, purpose=fpurp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20
        
        print *, 'Write the Array Attpack from the second run of component 1.'
        call ESMF_ArrayWriteAttPack(array, convention=fconv, purpose=fpurp, rc=rc)
        if (status .ne. ESMF_SUCCESS) goto 20
      endif
    endif

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
