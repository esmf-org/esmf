! $Id: ESMF_FieldHaloSTest.F90,v 1.56 2009/10/28 02:05:34 theurich Exp $
!
! System test FieldHalo
!  Description on Sourceforge under System Test #70385

!-------------------------------------------------------------------------
!ESMF_SYSTEM_removeTEST        String used by test script to count system tests.
!=========================================================================


!BOP
!
! !DESCRIPTION:
! System test FieldHalo.
!
!
!\begin{verbatim}

    program FieldHalo

#include "ESMF_Macros.inc"

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    implicit none
    
    ! Subroutine to set entry points.
    external setserv

    ! Module Local variables
    type(ESMF_GridComp) :: comp1
    type(ESMF_VM) :: vm
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_State) :: import
    integer :: npets, my_id, rc

    ! width of halo region - moved to "shared" module
    !integer, parameter :: halo_width = 2

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "----------------------"
    print *, "System Test FieldHalo:"
    print *, "----------------------"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    call ESMF_Initialize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    call ESMF_VMGet(vm, petCount=npets, localPet=my_id, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    if (npets .ne. 4) then
        print *, "This system test needs to run 4-way, current np = ", npets
        goto 10
    endif

    cname = "System Test FieldHalo"
    comp1 = ESMF_GridCompCreate(name=cname, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    print *, "Comp Create finished, name = ", trim(cname)

    call ESMF_GridCompSetServices(comp1, setserv, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

!
!-------------------------------------------------------------------------
!  Init section
!
    import = ESMF_StateCreate("igridded comp import", ESMF_STATE_IMPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_GridCompInitialize(comp1, importState=import, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    print *, "Comp Init returned"

!
!-------------------------------------------------------------------------
!     Run section
!

    call ESMF_GridCompRun(comp1, importState=import, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    print *, "Comp Run returned"

!
!-------------------------------------------------------------------------
!     Finalize section

    call ESMF_GridCompFinalize(comp1, importState=import, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    print *, "Comp Finalize returned without error"

!
!-------------------------------------------------------------------------
!     Destroy section
! 

    call ESMF_GridCompDestroy(comp1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_StateDestroy(import, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10  print *, "System Test FieldHalo complete"

    write(failMsg, *) "System Test failure"
    write(testname, *) "System Test FieldHalo: Field Halo Test"

    if ((my_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
      ! Separate message to console, for quick confirmation of success/failure
      if (rc .eq. ESMF_SUCCESS) then
        write(finalMsg, *) "SUCCESS.  Halo values are as expected."
      else
        write(finalMsg, *) "System Test did not succeed. ", &
        "Halo values do not match expected, or error code set ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif
    
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)
  
    call ESMF_Finalize(rc=rc)  
    
    end program FieldHalo
    

! module global data

    module shared
      use ESMF_Mod

      ! width of halo region
      integer, parameter, public :: halo_width = 2

      ! route handle
      type(ESMF_RouteHandle), save, public :: routehandle
    end module shared
        
! end global data
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Set services
!-------------------------------------------------------------------------
!
    subroutine setserv(comp, rc)
      use ESMF_Mod

      type(ESMF_GridComp) :: comp
      integer :: rc

      external myinit, myrun, myfinal
       
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, myinit, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, myrun, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, myfinal, rc=rc)
      if (rc .ne. ESMF_SUCCESS) return
  
      rc = ESMF_SUCCESS

    end subroutine setserv

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!
    subroutine myinit(comp, importState, exportState, clock, rc)
      use ESMF_Mod
      use shared

      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      integer :: i, j
      type(ESMF_VM) :: vm
      type(ESMF_DELayout) :: delayout1 
      type(ESMF_IGrid) :: igrid1
      type(ESMF_Field) :: field1
      type(ESMF_ArraySpec) :: arrayspec
      integer(ESMF_KIND_I4), dimension(:,:), pointer :: ldata
      integer :: lowerindex(2), upperindex(2)
      integer :: de_id
      integer, dimension(ESMF_MAXIGRIDDIM) :: counts
      type(ESMF_IGridHorzStagger) :: horz_stagger
      real(ESMF_KIND_R8) :: min(2), max(2)
      character(len=ESMF_MAXSTR) :: gname, fname

      print *, "Entering Initialization routine"

      ! Query the component for how many pets we have, and make a layout 
      ! based on that.
      call ESMF_GridCompGet(comp, vm=vm, rc=rc)

      ! Make sure we were given enough pets for what we expected.
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
   
      if (npets .ne. 4) then
         print *, "This component needs to run 4-way"
         rc = ESMF_FAILURE
         goto 30
      endif

      ! Create a DELayout for the Component
      delayout1 = ESMF_DELayoutCreate(vm, (/ 2, 2 /), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! The user creates a simple horizontal IGrid internally by passing all
      ! necessary information through the CreateInternal argument list.

      counts(1) = 30
      counts(2) = 36
      min(1) = 0.0
      max(1) = 15.0
      min(2) = 0.0
      max(2) = 12.0
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A
      gname = "test igrid 1"

      igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                       minGlobalCoordPerDim=min, &
                                       maxGlobalCoordPerDim=max, &
                                       horzStagger=horz_stagger, &
                                       name=gname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_IGridDistribute(igrid1, delayout=delayout1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      print *, "IGrid Create returned"

      ! Figure out our local processor id to use as data in the Field.
      call ESMF_DELayoutGetDeprecated(delayout1, localDE=de_id, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Create an arrayspec for a 2-D array 
      call ESMF_ArraySpecSet(arrayspec, rank=2, &
                             typekind=ESMF_TYPEKIND_I4)

      ! Create a Field using the IGrid and ArraySpec created above
      fname = "DE id"
      field1 = ESMF_FieldCreate(igrid1, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo_width, name=fname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_FieldPrint(field1, rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_FieldValidate(field1, rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      print *, "Field Create returned"

      ! Add the field to the import state.
      call ESMF_StateAddField(importState, field1, rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_StatePrint(importState, "", rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Get pointer to the actual data
      call ESMF_FieldGetDataPointer(field1, ldata, ESMF_DATA_REF, rc=rc)

      ! Set initial data values over whole field to -1
      lowerindex = lbound(ldata)
      upperindex = ubound(ldata)
      do j=lowerindex(2),upperindex(2)
        do i=lowerindex(1),upperindex(1)
          ldata(i,j) = -1
        enddo
      enddo

      ! Set initial data values over computational domain to the de identifier
      do j=lowerindex(2)+halo_width,upperindex(2)-halo_width
        do i=lowerindex(1)+halo_width,upperindex(1)-halo_width
           ldata(i,j) =de_id
         enddo
      enddo

      ! and have the framework precompute the halo communication patterns
      call ESMF_FieldHaloStore(field1, routehandle, rc=rc)

      print *, "Exiting Initialization routine"

30    continue
      ! you come directly here on errors.  you also fall into this code
      ! if all is well.  rc already has the error code (if any) so you
      ! have nothing else to do but return at this point.

    end subroutine myinit

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!

    subroutine myrun(comp, importState, exportState, clock, rc)
      use ESMF_Mod
      use shared

      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      type(ESMF_Field) :: field1

      print *, "Entering Run routine"

      call ESMF_StatePrint(importState, "", rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Get the field from the import state
      print *, "About to get field from import state"
      call ESMF_StateGetField(importState, "DE id", field1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      print *, "Returned from getting field from import state"
      call ESMF_FieldPrint(field1, "", rc)
      if (rc .ne. ESMF_SUCCESS) goto 30


      ! Call Field method to halo data.  This updates the data in place.
      print *, "about to call Field Halo"
      !call ESMF_FieldHalo(field1, rc=rc)   ! deprecated
      call ESMF_FieldHalo(field1, routehandle, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      print *, "returned from Field Halo call"

      print *, "Exiting Run routine"

30    continue
      ! you come directly here on errors.  you also fall into this code
      ! if all is well.  rc already contains the return code, so there's
      ! nothing to do here but return.

      end subroutine myrun

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------


    subroutine myfinal(comp, importState, exportState, clock, rc)
      use ESMF_Mod
      use shared

      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      integer :: i, j, ni, nj, xpos, ypos, nx, ny
      integer :: de_id, mismatch, target
      integer :: pattern(3,3), nDE(2), myDE(2)
      integer(ESMF_KIND_I4), dimension(:,:), pointer :: ldata
      integer :: lowerindex(2), upperindex(2)
      type(ESMF_DELayout) :: delayout
      type(ESMF_Field) :: field1
      type(ESMF_IGrid) :: igrid1

      print *, "Entering Finalize routine"

      ! release the saved information about the communications
      call ESMF_FieldHaloRelease(routehandle, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Get Field from import state
      call ESMF_StateGetField(importState, "DE id", field1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Get a pointer to the IGrid in the Field, and then to the DELayout
      ! associated with that IGrid
      call ESMF_FieldGet(field1, igrid=igrid1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_IGridGet(igrid1, delayout=delayout, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! get our local de number from the layout
      call ESMF_DELayoutGetDeprecated(delayout, localDE=de_id, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Get a pointer to the data in the Field
      call ESMF_FieldGetDataPointer(field1, ldata, ESMF_DATA_REF, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Get size of local array
      lowerindex = lbound(ldata)
      upperindex = ubound(ldata)

      ni = upperindex(1) - lowerindex(1) + 1
      nj = upperindex(2) - lowerindex(2) + 1

      ! Validity check for results - return error if results do not match

      ! get info about total number of DEs in each dim and which one
      ! we are.  then use them to compute the values in the halo.
      call ESMF_DELayoutGetDeprecated(delayout, deCountPerDim=nDE, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      nx = nDE(1)
      ny = nDE(2)
      write(*,*) 'nx, ny = ', nx, ny
      call ESMF_DELayoutGetDELocalInfo(delayout, de_id, coord=myDE, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      xpos = myDE(1)
      ypos = myDE(2)
      write(*,*) 'xpos, ypos = ', xpos, ypos
   
      mismatch = 0

      ! check interior points
      target = de_id
      pattern(2,2) = target
      do j=1+halo_width,nj-halo_width
        do i=1+halo_width,ni-halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! now edges minus corners

      ! bottom middle
      if (ypos .eq. 1) then
        target = -1
      else
        target = de_id - nx 
      endif
      pattern(2, 1) = target
      do j=1,halo_width
        do i=1+halo_width,nx-halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo
      ! top middle
      if (ypos .eq. ny) then
        target = -1
      else
        target = de_id + nx 
      endif
      pattern(2, 3) = target
      do j=nj-halo_width+1,nj
        do i=1+halo_width, ni-halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo
      ! left side middle
      if (xpos .eq. 1) then
        target = -1
      else
        target = de_id - 1
      endif
      pattern(1, 2) = target
      do j=1+halo_width,nj-halo_width
        do i=1, halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo
      ! right side middle
      if (xpos .eq. nx) then
        target = -1
      else
        target = de_id + 1
      endif
      pattern(3, 2) = target
      do j=1+halo_width,nj-halo_width
        do i=ni-halo_width+1,ni
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! and finally corners
      ! lower left
      if ((xpos .eq. 1) .or. (ypos .eq. 1)) then
        target = -1
      else
        target = de_id - nx - 1
      endif
      pattern(1, 1) = target
      do j=1,halo_width
        do i=1,halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo
      ! lower right
      if ((xpos .eq. nx) .or. (ypos .eq. 1)) then
        target = -1
      else
        target = de_id - nx + 1
      endif
      pattern(3, 1) = target
      do j=1,halo_width
        do i=ni-halo_width+1, ni
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo
      ! upper left
      if ((xpos .eq. 1) .or. (ypos .eq. ny)) then
        target = -1
      else
        target = de_id + nx - 1
      endif
      pattern(1, 3) = target
      do j=nj-halo_width+1,nj
        do i=1, halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo
      ! upper right
      if ((xpos .eq. nx) .or. (ypos .eq. ny)) then
        target = -1
      else
        target = de_id + nx + 1
      endif
      pattern(3, 3) = target
      do j=nj-halo_width+1,nj
        do i=ni-halo_width+1,ni
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! Print what the results should look like
      print *, "------------------------------------------------------"
      print *, "pattern should look like:"
      print *, pattern(1, 3), pattern(2, 3), pattern(3, 3)
      print *, pattern(1, 2), pattern(2, 2), pattern(3, 2)
      print *, pattern(1, 1), pattern(2, 1), pattern(3, 1)
      print *, "------------------------------------------------------"
      print *, ""
      ! Print results so we can see either way what was computed.
      print *, "------------------------------------------------------"
      write(*,*) 'actual results from de_id = ',de_id
      do j = nj,1,-1
        write(*,10) (ldata(i,j), i=1,ni)
 10     format(20(1x,i2))
      enddo
      print *, "------------------------------------------------------"
    
      ! if any numbers are not what we expected, error out
      if (mismatch .gt. 0) then
        print *, "FAILURE: found ", mismatch, "mismatching values, returning error code"
        rc = ESMF_FAILURE
        return
      endif

30    continue
      ! you come directly here on errors.  you also fall into this code
      ! if all is well.  rc already contains the return code, so there's
      ! nothing to do here but return.

      print *, "Exiting Finalize routine"

    end subroutine myfinal

!\end{verbatim}
    
