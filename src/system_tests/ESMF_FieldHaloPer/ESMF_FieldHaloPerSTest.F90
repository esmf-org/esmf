! $Id: ESMF_FieldHaloPerSTest.F90,v 1.52 2009/10/28 02:05:35 theurich Exp $
!
! System test FieldHaloPeriodic
!  Field Halo with periodic boundary conditions.
!  Description on Sourceforge under System Test #82899

!-------------------------------------------------------------------------
!ESMF_SYSTEM_removeTEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test FieldHaloPeriodic.
!
!
!\begin{verbatim}

    module global_data

      use ESMF_Mod

      ! set to true to get more output
      logical :: verbose = .false.
   
      ! which type of periodic boundaries
      type(ESMF_Logical) :: periodic(2)

      ! halo width on each edge
      integer :: halo_width = 3

      ! route handle for stored halo communication patterns
      type(ESMF_RouteHandle), save :: routehandle(4)

      public :: verbose, periodic, halo_width, routehandle

    end module


    program FieldHaloPeriodic

#include "ESMF_Macros.inc"

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    use global_data
    
    implicit none
    
    ! Subroutine to set entry points.
    external setserv

    ! Global variables
    type(ESMF_GridComp) :: comp1
    type(ESMF_VM) :: vm
    integer :: pe_id
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_State) :: import
    integer :: rc

        
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "------------------------------"
    print *, "System Test FieldHaloPeriodic:"
    print *, "------------------------------"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize the framework and get a copy of the global VM
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cname = "System Test FieldHaloPeriodic"
    comp1 = ESMF_GridCompCreate(name=cname, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    if (verbose) print *, "Comp Create finished, name = ", trim(cname)

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

    if (verbose) print *, "Comp Init returned"

!
!-------------------------------------------------------------------------
!     Run section
!

    call ESMF_GridCompRun(comp1, importState=import, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    call ESMF_GridCompRun(comp1, importState=import, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    call ESMF_GridCompRun(comp1, importState=import, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    if (verbose) print *, "Comp Run returned"

!
!-------------------------------------------------------------------------
!     Finalize section

    call ESMF_GridCompFinalize(comp1, importState=import, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    if (verbose) print *, "Comp Finalize returned without error"

!
!-------------------------------------------------------------------------
!     Destroy section
! 

    call ESMF_GridCompDestroy(comp1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    call ESMF_StateDestroy(import, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    if (verbose) print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10  print *, "System Test FieldHaloPeriodic complete."

    call ESMF_VMGet(vm, localPet=pe_id, rc=rc)
    
    write(failMsg, *) "System Test failure"
    write(testname, *) "System Test FieldHaloPeriodic: Field Halo Test"

    if ((pe_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
      ! Separate message to console, for quick confirmation of success/failure
      if (rc .eq. ESMF_SUCCESS) then
        write(finalMsg, *) "SUCCESS.  Periodic Halo values are as expected."
      else
        write(finalMsg, *) "System Test did not succeed. ", &
        "Periodic Halo values do not match expected, or error code set ", rc
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

    end program FieldHaloPeriodic
    

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Set services
!-------------------------------------------------------------------------
!
    subroutine setserv(comp, rc)
      use ESMF_Mod
      use global_data

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
      use global_data

      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      integer :: i, j
      type(ESMF_Field) :: field(4)
      type(ESMF_IGrid) :: igrid(4), thisigrid
      type(ESMF_ArraySpec) :: arrayspec
      integer(ESMF_KIND_I4), dimension(:,:), pointer :: ldata
      integer :: lowerindex(2), upperindex(2)
      integer :: pe_id
      type(ESMF_VM) :: vm
      type(ESMF_DELayout) :: layout1
      integer, dimension(2) :: shape
      integer, dimension(ESMF_MAXIGRIDDIM) :: counts
      type(ESMF_IGridHorzStagger) :: horz_stagger
      real(ESMF_KIND_R8) :: min(2), max(2)
      character(len=ESMF_MAXSTR) :: gname, fname

      if (verbose) print *, "Entering Initialization routine"

      ! Query component for layout
      call ESMF_GridCompGet(comp, vm=vm, rc=rc)
      if (verbose) print *, "myinit: getting vm, rc = ", rc
      if (rc .ne. ESMF_SUCCESS) goto 30

      call ESMF_VMGet(vm, petCount=npets, localPet=pe_id, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
  
      ! Create a DELayout for the Component, basing the shape on
      ! the number of PETs we are given.
      select case (npets)
         case (4)
             shape(1) = 2
             shape(2) = 2
         case (6)
             shape(1) = 3
             shape(2) = 2
         case (8)
             shape(1) = 2
             shape(2) = 4
         case (12)
             shape(1) = 3
             shape(2) = 4
         case (16)
             shape(1) = 4
             shape(2) = 4
         case (24)
             shape(1) = 4
             shape(2) = 6
         case (32)
             shape(1) = 8
             shape(2) = 4
         case default
             shape(1) = npets
             shape(2) = 1
      end select

      layout1 = ESMF_DELayoutCreate(vm, shape, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
  
      ! print out shape of DELayout
      print *, ""
      print *, "DE ID numbers:"
      do j = shape(2)-1,0,-1
        write(*,5) (j*shape(1) + i, i=0,shape(1)-1)
   5   format(25(1x,i2))
      enddo
      print *, ""
  
      ! The user creates a simple horizontal IGrid internally by passing all
      ! necessary information through the CreateInternal argument list.

      counts(1) = 30
      counts(2) = 36
      min(1) = 0.0
      max(1) = 15.0
      min(2) = 0.0
      max(2) = 12.0
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

      print *, "IGrid 1 is Periodic along the 1st dimension"
      periodic(1) = ESMF_TRUE
      periodic(2) = ESMF_FALSE

      gname = "test igrid 1"

      igrid(1) = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                         minGlobalCoordPerDim=min, &
                                         maxGlobalCoordPerDim=max, &
                                         horzStagger=horz_stagger, &
                                         periodic=periodic, &
                                         name=gname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_IGridDistribute(igrid(1), delayout=layout1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      if (verbose) print *, "IGrid Create returned"

      print *, "IGrid 2 is Periodic along the 2st dimension"
      periodic(1) = ESMF_FALSE
      periodic(2) = ESMF_TRUE

      gname = "test igrid 2"

      igrid(2) = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                         minGlobalCoordPerDim=min, &
                                         maxGlobalCoordPerDim=max, &
                                         horzStagger=horz_stagger, &
                                         periodic=periodic, &
                                         name=gname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_IGridDistribute(igrid(2), delayout=layout1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      if (verbose) print *, "IGrid Create returned"

      print *, "IGrid 3 is Periodic along both dimensions"
      periodic(1) = ESMF_TRUE
      periodic(2) = ESMF_TRUE

      gname = "test igrid 3"

      igrid(3) = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                         minGlobalCoordPerDim=min, &
                                         maxGlobalCoordPerDim=max, &
                                         horzStagger=horz_stagger, &
                                         periodic=periodic, &
                                         name=gname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_IGridDistribute(igrid(3), delayout=layout1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      if (verbose) print *, "IGrid Create returned"

      print *, "IGrid 4 is not Periodic along either dimension"
      periodic(1) = ESMF_FALSE
      periodic(2) = ESMF_FALSE

      gname = "test igrid 4"

      igrid(4) = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                         minGlobalCoordPerDim=min, &
                                         maxGlobalCoordPerDim=max, &
                                         horzStagger=horz_stagger, &
                                         periodic=periodic, &
                                         name=gname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_IGridDistribute(igrid(4), delayout=layout1, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      if (verbose) print *, "IGrid Create returned"

      ! Figure out our local processor id to use as data in the Field.
      call ESMF_DELayoutGetDeprecated(layout1, localDe=pe_id, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Create an arrayspec for a 2-D array 
      call ESMF_ArraySpecSet(arrayspec, rank=2, &
                             typekind=ESMF_TYPEKIND_I4)

      ! Create 4 Fields using the IGrids and ArraySpec created above
      fname = "Periodic in X"
      thisigrid = igrid(1)
      field(1) = ESMF_FieldCreate(thisigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo_width, name=fname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      call ESMF_FieldHaloStore(field(1), routehandle(1), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      fname = "Periodic in Y"
      thisigrid = igrid(2)
      field(2) = ESMF_FieldCreate(thisigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo_width, name=fname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      call ESMF_FieldHaloStore(field(2), routehandle(2), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      fname = "Periodic in both X and Y"
      thisigrid = igrid(3)
      field(3) = ESMF_FieldCreate(thisigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo_width, name=fname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      call ESMF_FieldHaloStore(field(3), routehandle(3), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      fname = "Not Periodic"
      thisigrid = igrid(4)
      field(4) = ESMF_FieldCreate(thisigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                haloWidth=halo_width, name=fname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      call ESMF_FieldHaloStore(field(4), routehandle(4), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Add the fields to the import state.
      call ESMF_StateAddField(importState, field(1), rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_StateAddField(importState, field(2), rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_StateAddField(importState, field(3), rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      call ESMF_StateAddField(importState, field(4), rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      if (verbose) call ESMF_StatePrint(importState, "", rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      do k=1, 4

          ! Get pointer to the actual data
          call ESMF_FieldGetDataPointer(field(k), ldata, ESMF_DATA_REF, rc=rc)
    
          ! Set initial data values over whole field to -1
          lowerindex = lbound(ldata)
          upperindex = ubound(ldata)
          do j=lowerindex(2),upperindex(2)
            do i=lowerindex(1),upperindex(1)
              ldata(i,j) = -1
            enddo
          enddo
    
          ! Set initial data values over computational domain to the pet num
          do j=lowerindex(2)+halo_width,upperindex(2)-halo_width
            do i=lowerindex(1)+halo_width,upperindex(1)-halo_width
               ldata(i,j) =pe_id
             enddo
          enddo

      enddo
      if (verbose) print *, "Exiting Initialization routine"

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
      use global_data

      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      type(ESMF_Field) :: field1

      if (verbose) print *, "Entering Run routine"

      if (verbose) call ESMF_StatePrint(importState, "", rc)
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Get the field from the import state
      if (verbose) print *, "About to get field from import state"
      call ESMF_StateGetField(importState, "Periodic in X", field1, rc=rc);
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Call Field method to halo data.  This updates the data in place.
      if (verbose) print *, "about to call Field Halo"
      call ESMF_FieldHalo(field1, routehandle(1), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      if (verbose) print *, "returned from Field Halo call"


      ! Get the field from the import state
      if (verbose) print *, "About to get field from import state"
      call ESMF_StateGetField(importState, "Periodic in Y", field1, rc=rc);
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Call Field method to halo data.  This updates the data in place.
      if (verbose) print *, "about to call Field Halo"
      call ESMF_FieldHalo(field1, routehandle(2), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      if (verbose) print *, "returned from Field Halo call"


      ! Get the field from the import state
      if (verbose) print *, "About to get field from import state"
      call ESMF_StateGetField(importState, "Periodic in both X and Y", field1, rc=rc);
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Call Field method to halo data.  This updates the data in place.
      if (verbose) print *, "about to call Field Halo"
      call ESMF_FieldHalo(field1, routehandle(3), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      if (verbose) print *, "returned from Field Halo call"


      ! Get the field from the import state
      if (verbose) print *, "About to get field from import state"
      call ESMF_StateGetField(importState, "Not Periodic", field1, rc=rc);
      if (rc .ne. ESMF_SUCCESS) goto 30

      ! Call Field method to halo data.  This updates the data in place.
      if (verbose) print *, "about to call Field Halo"
      call ESMF_FieldHalo(field1, routehandle(4), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 30
      if (verbose) print *, "returned from Field Halo call"


      if (verbose) print *, "Exiting Run routine"

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
      use global_data

      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer :: rc

      ! Local variables
      type(ESMF_Field) :: field1
      integer :: localrc, finalrc, i
  

      if (verbose) print *, "Entering Finalize routine"

      ! set this up to run the validate code on all fields
      ! so we can see and compare the output.  but if any of
      ! the verify routines return error, return error at the end.
      finalrc = ESMF_SUCCESS


      ! Get Fields from import state
      call ESMF_StateGetField(importState, "Periodic in X", field1, rc=rc);
      if (rc .ne. ESMF_SUCCESS) then 
        finalrc = ESMF_FAILURE
        goto 30
      endif
      call verifyhalo(field1, localrc)
      if (localrc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    

      call ESMF_StateGetField(importState, "Periodic in Y", field1, rc=rc);
      if (rc .ne. ESMF_SUCCESS) then 
        finalrc = ESMF_FAILURE
        goto 30
      endif
      call verifyhalo(field1, localrc)
      if (localrc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    

      call ESMF_StateGetField(importState, "Periodic in both X and Y", field1, rc=rc);
      if (rc .ne. ESMF_SUCCESS) then 
        finalrc = ESMF_FAILURE
        goto 30
      endif
      call verifyhalo(field1, localrc)
      if (localrc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    

      call ESMF_StateGetField(importState, "Not Periodic", field1, rc=rc);
      if (rc .ne. ESMF_SUCCESS) then 
        finalrc = ESMF_FAILURE
        goto 30
      endif
      call verifyhalo(field1, localrc)
      if (localrc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

      do i=1, 4
        call ESMF_FieldHaloRelease(routehandle(i), rc)
        if (rc .ne. ESMF_SUCCESS) goto 30
      enddo

30 continue
      ! come straight here if you cannot get the data from the state.
      ! otherwise error codes are accumulated but ignored until the
      ! end so we can see the output from all the cases to help track
      ! down errors.

      if (verbose) print *, "Exiting finalize routine"
      rc = finalrc

    end subroutine myfinal


    subroutine verifyhalo(thisfield, rc)
      use ESMF_Mod
      use global_data

      type(ESMF_Field), intent(inout) :: thisfield
      integer, intent(out) :: rc


      ! Local variables
      integer :: i, j, ni, nj, xpos, ypos, pos(2), nx, ny, ncount(2)
      integer :: pe_id, target
      integer :: mismatch
      integer :: pattern(3,3)
      type(ESMF_Logical) :: pflags(2)
      integer(ESMF_KIND_I4), dimension(:,:), pointer :: ldata
      integer :: lowerindex(2), upperindex(2)
      type(ESMF_DELayout) :: layout
      type(ESMF_IGrid) :: igrid1
      character(len=ESMF_MAXSTR) :: fname

      if (verbose) print *, "Entering halo verification routine"

      call ESMF_FieldGet(thisfield, igrid=igrid1, rc=rc)
      if (verbose) print *, "igrid back from field"
      call ESMF_IGridGetDELayout(igrid1, delayout=layout, rc=rc)
      call ESMF_IGridGet(igrid1, periodic=pflags, rc=rc)
      if (verbose) print *, "layout, periodic flags back from igrid"

      call ESMF_FieldGet(thisfield, name=fname, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 40
      if (verbose) print *, "name back from field"

      ! Get our pe_id from layout
      call ESMF_DELayoutGetDeprecated(layout, localDe=pe_id, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 40

      ! Get a pointer to the start of the data
      call ESMF_FieldGetDataPointer(thisfield, ldata, ESMF_DATA_REF, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 40
      if (verbose) print *, "data back from array"

      ! Get size of local array
      lowerindex = lbound(ldata)
      upperindex = ubound(ldata)
      ni = upperindex(1) - lowerindex(1) + 1
      nj = upperindex(2) - lowerindex(2) + 1

      ! get info about total number of DEs in each dim and which one
      ! we are.  then use them to compute the values in the halo.
      call ESMF_DELayoutGetDeprecated(layout, deCountPerDim=ncount, rc=rc)
      nx = ncount(1)
      ny = ncount(2)
      if (rc .ne. ESMF_SUCCESS) goto 40
      call ESMF_DELayoutGetDELocalInfo(layout, pe_id, coord=pos, rc=rc)
      xpos = pos(1)
      ypos = pos(2)
      if (rc .ne. ESMF_SUCCESS) goto 40
     
      ! for the calculations below we need the first xpos=0, not 1
      xpos = xpos - 1
      ypos = ypos - 1


      !!
      ! construct the pattern for side, corners, and interior points
      !!

      ! start by filling in the pattern for an interior de and then
      !  check for boundary de's and overwrite those.
      pattern(1,1) = pe_id - nx - 1
      pattern(2,1) = pe_id - nx
      pattern(3,1) = pe_id - nx + 1

      pattern(1,2) = pe_id - 1
      pattern(2,2) = pe_id
      pattern(3,2) = pe_id + 1

      pattern(1,3) = pe_id + nx - 1
      pattern(2,3) = pe_id + nx
      pattern(3,3) = pe_id + nx + 1


      ! now edges minus corners

      ! bottom middle
      if (ypos .eq. 0) then
        if (pflags(2) .eq. ESMF_TRUE) then
          target = (ny-1)*nx + xpos 
        else
          target = -1
        endif
        pattern(2, 1) = target
      endif

      ! top middle
      if (ypos .eq. ny-1) then
        if (pflags(2) .eq. ESMF_TRUE) then
          target = xpos 
        else
          target = -1
        endif
        pattern(2, 3) = target
      endif
      
      ! left side middle
      if (xpos .eq. 0) then
        if (pflags(1) .eq. ESMF_TRUE) then
          target = pe_id + (nx-1)
        else
          target = -1
        endif
        pattern(1, 2) = target
      endif
      
      ! right side middle
      if (xpos .eq. nx-1) then
        if (pflags(1) .eq. ESMF_TRUE) then
          target = pe_id - (nx-1)
        else
          target = -1
        endif
        pattern(3, 2) = target
      endif

      ! and finally corners

      ! lower left
      if ((xpos .eq. 0) .and. (ypos .eq. 0)) then
        if ((pflags(1) .eq. ESMF_TRUE) .and. &
            (pflags(2) .eq. ESMF_TRUE)) then
          target = -1 ! ambiguous
          !target = pe_id + nx - 1  ! ambiguous
          !target = pe_id + (ny-1)*nx - 1 ! ambiguous
        else
          target = -1
        endif
        pattern(1, 1) = target
      else if (xpos .eq. 0) then
        if (pflags(1) .eq. ESMF_TRUE) then
          target = pe_id - 1
        else
          target = -1
        endif
        pattern(1, 1) = target
      else if (ypos .eq. 0) then
        if (pflags(2) .eq. ESMF_TRUE) then
          target = pe_id + (ny-1)*nx - 1
        else
          target = -1
        endif
        pattern(1, 1) = target
      endif
      
      ! lower right
      if ((xpos .eq. nx-1) .and. (ypos .eq. 0)) then
        if ((pflags(1) .eq. ESMF_TRUE) .and. &
            (pflags(2) .eq. ESMF_TRUE)) then
          target = -1  ! ambiguous
          !target = pe_id - nx + 1  ! ambiguous
          !target = pe_id + (ny-1)*nx + 1  ! ambiguous
        else
          target = -1
        endif
        pattern(3, 1) = target
      else if (xpos .eq. nx-1) then
        if (pflags(1) .eq. ESMF_TRUE) then
          target = pe_id - 2*nx + 1
        else
          target = -1
        endif
        pattern(3, 1) = target
      else if (ypos .eq. 0) then
        if (pflags(2) .eq. ESMF_TRUE) then
          target = pe_id + (ny-1)*nx + 1
        else
          target = -1
        endif
        pattern(3, 1) = target
      endif
      
      ! upper left
      if ((xpos .eq. 0) .and. (ypos .eq. ny-1)) then
        if ((pflags(1) .eq. ESMF_TRUE) .and. &
            (pflags(2) .eq. ESMF_TRUE)) then
          target = -1 ! ambiguous
          !target = pe_id + nx - 1  ! ambiguous
          !target = xpos + nx - 1 ! ambiguous
        else
          target = -1
        endif
        pattern(1, 3) = target
      else if (xpos .eq. 0) then
        if (pflags(1) .eq. ESMF_TRUE) then
          target = pe_id + 2*nx - 1
        else
          target = -1
        endif
        pattern(1, 3) = target
      else if (ypos .eq. ny-1) then
        if (pflags(2) .eq. ESMF_TRUE) then
          target = xpos - 1
        else
          target = -1
        endif
        pattern(1, 3) = target
      endif
      
      ! upper right
      if ((xpos .eq. nx-1) .and. (ypos .eq. ny-1)) then
        if ((pflags(1) .eq. ESMF_TRUE) .and. &
            (pflags(2) .eq. ESMF_TRUE)) then
          target = -1  ! ambiguous
          !target = pe_id - nx + 1  ! ambiguous
          !target = pe_id + 1  ! ambiguous
        else
          target = -1
        endif
        pattern(3, 3) = target
      else if (xpos .eq. nx-1) then
        if (pflags(1) .eq. ESMF_TRUE) then
          target = pe_id + 1
        else
          target = -1
        endif
        pattern(3, 3) = target
      else if (ypos .eq. ny-1) then
        if (pflags(2) .eq. ESMF_TRUE) then
          target = xpos + 1
        else
          target = -1
        endif
        pattern(3, 3) = target
      endif



      !!
      ! compare pattern to actual values and count mismatches
      !!

      mismatch = 0

      ! check interior points
      target = pattern(2,2)
      do j=1+halo_width,nj-halo_width
        do i=1+halo_width,ni-halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! now edges minus corners

      ! bottom middle
      target = pattern(2, 1)
      do j=1,halo_width
        do i=1+halo_width,ni-halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! top middle
      target = pattern(2, 3)
      do j=nj-halo_width+1,nj
        do i=1+halo_width, ni-halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! left side middle
      target = pattern(1, 2)
      do j=1+halo_width,nj-halo_width
        do i=1, halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! right side middle
      target = pattern(3, 2)
      do j=1+halo_width,nj-halo_width
        do i=ni-halo_width+1,ni
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! and finally corners

      ! lower left
      target = pattern(1, 1)
      do j=1,halo_width
        do i=1,halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! lower right
      target = pattern(3, 1)
      do j=1,halo_width
        do i=ni-halo_width+1, ni
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! upper left
      target = pattern(1, 3) 
      do j=nj-halo_width+1,nj
        do i=1, halo_width
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      ! upper right
      target = pattern(3, 3) 
      do j=nj-halo_width+1,nj
        do i=ni-halo_width+1,ni
          if (ldata(i,j) .ne. target) then
             mismatch = mismatch + 1
          endif
        enddo
      enddo

      !!
      ! Print what the results should look like
      !!
      print *, '------------------------------------------------------'
      print *, 'DE ID = ', pe_id, trim(fname)
      print *, 'Pattern should look like:'
      print *, pattern(1, 3), pattern(2, 3), pattern(3, 3)
      print *, pattern(1, 2), pattern(2, 2), pattern(3, 2)
      print *, pattern(1, 1), pattern(2, 1), pattern(3, 1)
      print *, ''
      ! Print results so we can see either way what was computed.
      write(*,*) 'Actual values are:'
      do j = nj,1,-1
        write(*,10) (ldata(i,j), i=1,ni)
 10     format(25(1x,i2))
      enddo
      print *, '------------------------------------------------------'
    
      ! if any numbers are not what we expected, error out
      if (mismatch .gt. 0) then
        print *, "FAILURE: found ", mismatch, "mismatching values, returning error code"
        rc = ESMF_FAILURE
        return
      endif

40    continue
      ! you come directly here on errors.  you also fall into this code
      ! if all is well.  rc already contains the return code, so there's
      ! nothing to do here but return.

      if (verbose) print *, "Exiting verify halo routine"

    end subroutine verifyhalo

!\end{verbatim}
    
