!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

    program ESMF_RegridTimeUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOC
! !PROGRAM: ESMF_RegridTimeUTest - Using the Regridding methods
!
! !DESCRIPTION:
!
! This program runs Unit tests for F90 Field Regrid routines.
! Tests that Regriding does not take too long to complete.

!-----------------------------------------------------------------------------

    ! USES:Framework module
    use ESMF_TestMod  ! test methods
    use ESMF_Mod      ! Framework module
    use ESMF_UtilTypesMod     ! ESMF utility types
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_RegridTimeUTest.F90,v 1.10.2.4 2009/01/21 21:25:23 cdeluca Exp $'
!------------------------------------------------------------------------------

    integer :: lrc,iFunction
    integer ::  npets, localPet
    type(ESMF_VM) :: vm
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result=0.
    integer :: sub_rc    !Subroutine return call
    integer :: iDistr, nXY(2,2)
    integer :: TwoOrOne, rc
    integer :: i, numloops, idummy
    real(ESMF_KIND_R8) :: startTime, endTime, testTime
    real(ESMF_KIND_R8) :: calibrationTime, dummy
    logical :: notdone
    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------

    call ESMF_TestStart(ESMF_SRCLINE, rc=lrc)
    call ESMF_VMGetGlobal(vm, rc=lrc)
    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPET=localPet,  rc=lrc)


    nXY(1,:)=(/ npets, 1 /)

    TwoOrOne= 1 + mod(npets+1,2)
    nXY(2,:)=(/ npets/TwoOrOne, TwoOrOne /)

    sub_rc=ESMF_SUCCESS

   !--------------------------------
   !NEX_removeUTest
   !Test for function, f=x, decomp = (npets,1) regrid_method = ESMF_REGRID_METHOD_BILINEAR
    iFunction = 1
    iDistr = 1
    sub_rc=ESMF_SUCCESS
    write(failMsg, *) "Error in regrid"
    write(name, *) "Regrid f=x, decomp=(npets,1) and Bilinear"
    call RegridUTest(FieldChoice=iFunction,npetsXY=nXY(iDistr,:),regridmethod=ESMF_REGRID_METHOD_BILINEAR)
    call ESMF_Test((sub_rc.eq.ESMF_SUCCESS),name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
   !--------------------------------
   !EX_removeUTest
   ! Verify that ESMF_VMWtime works (we are going to use it to verify that
   ! the conservative regrid algorithm does not run too slowly.)
    call ESMF_VMWtime(startTime,rc=rc)
    write(failMsg, *) "ESMF_VMWtime returns zero"
    write(name, *) " Verify ESMF_VMWtime returns non-zero"
    call ESMF_Test((startTime.ne.0),name, failMsg, result, ESMF_SRCLINE)

   !--------------------------------
   ! do a fixed calculation to try to calibrate what the basic speed of
   ! this machine is.  the regridding should be within some percentage of
   ! this calibration.  (e.g. regrid on slow machines shouldn't be penalized 
   ! for not running as fast as it does on fast machines). 
   notdone = .true.
   numloops = 100000
   idummy = 0
   print *, "Starting calibration"

   call ESMF_VMWtime(startTime,rc=rc)
   ! do something which takes computational time
   do i=1, numloops
       dummy = modulo(i * 3.14159, 2.718)
       if (dummy .ge. 0.5) idummy = idummy + 1
   enddo
   call ESMF_VMWtime(endTime,rc=rc)
   calibrationTime = endTime - startTime
   print *, "Calibration factor = ", calibrationTime

   !--------------------------------
   !EX_removeUTest
   !Test for function, f=2+ (sin(2*theta))**16 * cos(16*phi), decomp = (npets/2,2), regrid_method = ESMF_REGRID_METHOD_CONSERV1
    iFunction = 4
    iDistr = 2
    sub_rc=ESMF_SUCCESS
    write(failMsg, *) "Error in regrid"
    write(name, *) "Regrid f=2+ (sin(2*theta))**16 * cos(16*phi), decomp=(npets/2,2), and Conserv1"
    !Get startTime
    call ESMF_VMWtime(startTime,rc=rc)
    call RegridUTest(FieldChoice=iFunction,npetsXY=nXY(iDistr,:),regridmethod=ESMF_REGRID_METHOD_CONSERV1)
    !Get endTime
    call ESMF_VMWTime(endTime,rc=rc)
    testTime = endTime - startTime
    call ESMF_Test((sub_rc.eq.ESMF_SUCCESS),name, failMsg, result, ESMF_SRCLINE)

   !--------------------------------
   !EX_removeUTest
   ! Verify that the regrid does not take too long
    write(failMsg, *) "Regrid took too long"
    write(name, *) " Verify Conserv Regrid takes no longer than 60 seconds"
    print *, "Test Duration Time:", testTime
    print *, "Calibrated Test Duration Time:", testTime/calibrationTime
    print *, "(Raw start, end time:", startTime, endTime, ")"
    call ESMF_Test((testTime.lt.60),name, failMsg, result, ESMF_SRCLINE)

#endif


    call ESMF_TestEnd(result, ESMF_SRCLINE)
    print *, "Regrid Unit test  returned"

    contains

    subroutine RegridUTest(FieldChoice, npetsXY, regridmethod)

    implicit none

      ! Choice of test function for the field to be regridded
      ! 1 -->   f=x
      ! 2 -->   f=2+cos(pi*r/L)
      ! 3 -->   f=2+(cos(theta))**2 * cos(2*phi)
      ! 4-->    f=2+ (sin(2*theta))**16 * cos(16*phi)

      integer :: FieldChoice
      type(ESMF_RegridMethod) :: regridmethod

    ! Local variables
    type(ESMF_Field) :: field1, field2
    type(ESMF_IGrid) :: srcigrid, dstigrid
    type(ESMF_RouteHandle) :: regrid_rh
    type(ESMF_DELayout) :: layout1, layout2, layout3
    integer :: rc

    integer :: i, j, lb(2), ub(2), halo
    integer :: TwoOrOne
    integer :: npetsXY(2)
    type(ESMF_ArraySpec) :: arrayspec
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1, f90ptr2
    real (ESMF_KIND_R8), dimension(:,:), pointer :: x_coords,y_coords
    real (ESMF_KIND_R8), dimension(:,:), pointer :: x_coords2,y_coords2
    real (ESMF_KIND_R8), dimension(:,:), allocatable :: SolnOnTarget
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords
    real (ESMF_KIND_R8) :: epsil, length_scale, pi,radius, RelativeError
    real (ESMF_KIND_R8) :: max_error, avg_error



!-------------------------------------------------------------------------
!   ! Setup:
!   !
!   !  Create a source and destination igrid with data on it, to use
!   !  in the Regrid calls below.
 
    TwoOrOne= 1 + mod(npets+1,2)
    layout1 = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=rc)
    layout2 = ESMF_DELayoutCreate(vm, npetsXY, rc=rc)
    layout3 = ESMF_DELayoutCreate(vm, (/  npets /), rc=rc)
  ! layout2 = ESMF_DELayoutCreate(vm, (/ npets/TwoOrOne, TwoOrOne /), rc=rc)
  ! layout2 = ESMF_DELayoutCreate(vm, (/ npets, 1 /), rc=rc)

    mincoords = (/  0.0,  0.0 /)
    maxcoords = (/ 20.0,  30.0 /)
   !epsil=epsilon(maxcoords(1))*maxcoords(1)*1.e3
    epsil=.05

    !Create thesource igrid
   !===========================
    srcigrid = ESMF_IGridCreateHorzXYUni((/100,150 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_B_NE, &
                   name="srcigrid", rc=rc)

   !Distribute the source igrid
   !===========================
    call ESMF_IGridDistribute(srcigrid, delayout=layout1, rc=rc)

   !Create the destination igrid
   !===========================

    dstigrid = ESMF_IGridCreateHorzXYUni((/100,150 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="srcigrid", rc=rc)

   !Distribute destination igrid
   !===========================
    call ESMF_IGridDistribute(dstigrid, delayout=layout2, rc=rc)
 


   !Specify settings for the field array
   !====================================
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   !Create the field (with halo width of 3)
   !=======================================
    halo = 3
    field1 = ESMF_FieldCreate(srcigrid, arrayspec, &
                              horzRelloc=ESMF_CELL_NECORNER, &
                              haloWidth=halo, name="src pressure", rc=rc)

   !Create a pointer to the source field data space
   !===============================================
    call ESMF_FieldGetDataPointer(field1, f90ptr1, ESMF_DATA_REF, rc=rc)


   !Get the actual values of the x and y coordinate arrays
   !======================================================

    call ESMF_IGridGetCoord(srcigrid, dim=1, horzRelLoc=ESMF_CELL_NECORNER,  &
           centercoord=x_coords, rc=rc)

    call ESMF_IGridGetCoord(srcigrid, dim=2, horzRelLoc=ESMF_CELL_NECORNER,  &
           centercoord=y_coords, rc=rc)


    !Assign values to the source field data (4 case choices) via pointer
    lb(:) = lbound(f90ptr1)
    ub(:) = ubound(f90ptr1)
    pi = 3.1416
    select case(FieldChoice)
    case(1) !** f=x
      !Set the values of the NE corner igrid points equal to their x-coordinate
      f90ptr1(:,:) = 0.0
      do j=lb(2)+halo, ub(2)-halo
        do i=lb(1)+halo, ub(1)-halo
        f90ptr1(i, j) = x_coords(i-halo,j-halo)
        enddo
      enddo
    case(2) !**f=2+cos(pi*r/L)
      f90ptr1(:,:) = 0.0
      length_scale=sqrt( maxcoords(1)**2 + maxcoords(2)**2 )
      do j=lb(2)+halo, ub(2)-halo
        do i=lb(1)+halo, ub(1)-halo
          radius=sqrt( x_coords(i-halo,j-halo)**2 +   &
                       y_coords(i-halo,j-halo)**2 )
          f90ptr1(i, j) = 2. + cos( pi * radius / length_scale )
        end do
      end do
    case(3) !**f=2+cos((pi/2)*y/ymax)*cos(4*pi*x/xmax)
      do j=lb(2)+halo, ub(2)-halo
        do i=lb(1)+halo, ub(1)-halo
          f90ptr1(i, j) = 2. + &
                        cos( pi*y_coords(i-halo,j-halo)/(2.*maxcoords(2)) ) &
                         *cos( 4.*pi*x_coords(i-halo,j-halo)/maxcoords(1)  )
        end do
      end do
    case(4) !**f=2+sin(pi*y/ymax)**16 * cos(16*pi*x/xmax)
      do j=lb(2)+halo, ub(2)-halo
        do i=lb(1)+halo, ub(1)-halo
          f90ptr1(i, j) = 2. + &
                 sin( pi*y_coords(i-halo,j-halo)/maxcoords(2) )**16 &
                *cos( 16.*pi*x_coords(i-halo,j-halo)/maxcoords(1) )
        end do
      end do
    end select


    !Create the destination field
   !=============================
    field2 = ESMF_FieldCreate(dstigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                                                   name="dst pressure", rc=rc)

    ! fields all ready to go

!\subsubsection{Precomputing and Executing a Regrid}
      
!  The user has already created an {\tt ESMF\_IGrid}, an
!  {\tt ESMF\_Array} with data, and put them together in an {\tt ESMF\_Field}.
!  An {\tt ESMF\_RouteHandle} is created and the data movement needed to
!  execute the regrid is stored with that handle by the store method. 
!  To actually execute the operation, the source and destination data
!  objects must be supplied, along with the same {\tt ESMF\_RouteHandle}.
      


   !Do all the calculations in preparation for the actual re-igridding
   !=================================================================
    call ESMF_FieldRegridStore(field1, field2, vm, &
                               routehandle=regrid_rh, &
                               regridmethod=regridmethod, rc=rc)
                               !regridmethod=ESMF_REGRID_METHOD_CONSERV1, rc=rc)
                               !regridmethod=ESMF_REGRID_METHOD_BILINEAR, rc=rc)

   !Regrid
   !======
    call ESMF_FieldRegrid(field1, field2, regrid_rh, rc=rc)

   !Get a pointer to the data in the destination field
   !==================================================
    call ESMF_FieldGetDataPointer(field2, f90ptr2, ESMF_DATA_REF, rc=rc)

    !Array bounds in the destination igrid
    !------------------------------------
    lb(:) = lbound(f90ptr2)
    ub(:) = ubound(f90ptr2)
    
    print *, localPet,'In igrid 2 lb=',lb,'  ub=',ub

    !Compute the "exact" array values in the destination igrid (for verification)
    !---------------------------------------------------------------------------
    allocate( SolnOnTarget( lb(1):ub(1) , lb(2):ub(2) ) )

    call ESMF_IGridGetCoord(dstigrid, dim=1, horzRelLoc=ESMF_CELL_CENTER,  &
           centercoord=x_coords2, rc=rc)
    call ESMF_IGridGetCoord(dstigrid, dim=2, horzRelLoc=ESMF_CELL_CENTER,  &
           centercoord=y_coords2, rc=rc)


    !Solution values at the target igrid -- 4 cases
    select case (FieldChoice)
    case(1) !** f=x
      SolnOnTarget(:,:)=x_coords2(:,:)
    case(2) !** f=2+cos(pi*r/L)
      do j=lb(2),ub(2)
        do i=lb(1),ub(1)
          radius=sqrt( x_coords2(i,j)**2 + y_coords2(i,j)**2 )
          SolnOnTarget(i, j) = 2. + cos( pi * radius / length_scale )
        end do
      end do
    case(3) !**f=2+cos((pi/2)*y/ymax)*cos(4*pi*x/xmax)
      do j=lb(2),ub(2)
        do i=lb(1),ub(1)
          SolnOnTarget(i, j) = 2. + &
                        cos( pi*y_coords2(i,j)/(2.*maxcoords(2)) ) &
                         *cos( 4.*pi*x_coords2(i,j)/maxcoords(1)  )
        end do
      end do
    case(4) !**f=2+sin(pi*y/ymax)**16 * cos(16*pi*x/xmax)
      do j=lb(2), ub(2)
        do i=lb(1), ub(1)
          SolnOnTarget(i, j) = 2. + &
                 sin( pi*y_coords2(i,j)/maxcoords(2) )**16 &
                *cos( 16.*pi*x_coords2(i,j)/maxcoords(1) )
        end do
      end do


    end select
 
   !Verify success in regridding. Compute maximum and average normalized error
   !--------------------------------------------------------------------------
   max_error=0.
   avg_error=0.

   do j=lb(2)+1,ub(2)
     do i=lb(1)+1,ub(1)
       RelativeError=abs( (SolnOnTarget(i,j)-f90ptr2(i,j)) / SolnOnTarget(i,j) )
       if (RelativeError .gt. epsil ) then
         sub_rc=ESMF_FAILURE
         print*,localPet,'ERROR: FieldChoice=',FieldChoice,' npetsXY=', &
                 npetsXY,' i,j=',i,j, &
                ' SolnOnTarget=',SolnOnTarget(i,j), f90ptr2(i,j), RelativeError
       end if
       avg_error=avg_error+RelativeError
       if(RelativeError > max_error) max_error=RelativeError
     end do
   end do

   avg_error=avg_error/( (ub(1)-lb(1)+1) * (ub(2)-lb(2)+1) )
   if (localPet .eq. 0) &
   !write(*,*)'i=1,j=3 FieldChoice=',FieldChoice, &
   !&         'SolnOnTarget, f90potr2 =',SolnOnTarget(1,3), f90ptr2(1,3)           
   print*, 'localPet=',localPet, 'FieldChoice=',FieldChoice, &
           '  maximum normalized error=',max_error, &
           '  average normalized error=',avg_error 


    call ESMF_FieldRegridRelease(regrid_rh, rc=rc)



!-------------------------------------------------------------------------
!    ! Cleanup

    call ESMF_FieldDestroy(field1, rc=rc)

    call ESMF_FieldDestroy(field2, rc=rc)

    call ESMF_IGridDestroy(srcigrid, rc=rc)

    call ESMF_IGridDestroy(dstigrid, rc=rc)

    end subroutine RegridUTest

    end program ESMF_RegridTimeUTest
