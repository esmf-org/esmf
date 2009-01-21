! $Id: ESMF_RegridBandingUTest.F90,v 1.10.2.3 2009/01/21 21:25:23 cdeluca Exp $
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
!
    program ESMF_FieldRegridBandingUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOC
! !PROGRAM: ESMF_FieldRegridBandingUTest - Using the Regridding methods
!
! !DESCRIPTION:
!
! This program runs Unit tests for F90 Field Regrid routines. 
! It transfers data between IGrids with row decomposition, where we have earlier
! seen banding problems.
!-----------------------------------------------------------------------------

    ! USES:Framework module
    use ESMF_TestMod  ! test methods
    use ESMF_Mod      ! Framework module

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_RegridBandingUTest.F90,v 1.10.2.3 2009/01/21 21:25:23 cdeluca Exp $'
!------------------------------------------------------------------------------
      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0


      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
    
    ! Local variables
    type(ESMF_Field) :: field1, field2
    type(ESMF_IGrid) :: srcigrid, dstigrid
    type(ESMF_RouteHandle) :: regrid_rh
    type(ESMF_DELayout) :: layout1
    integer :: rc, loop_rc
!EOC

    integer :: finalrc, npets, localPet
    integer :: i, j, lb(2), ub(2), halo
    type(ESMF_ArraySpec) :: arrayspec
    !type(ESMF_FieldDataMap) :: datamap
    type(ESMF_VM) :: vm
    real (ESMF_KIND_R8) :: pi
    real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1, f90ptr2, test
    real (ESMF_KIND_R8), dimension(:,:), pointer :: x_coords, y_coords
    real (ESMF_KIND_R8), dimension(:,:), pointer :: x_coords2, y_coords2
    real (ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

    finalrc = ESMF_SUCCESS
        
!-------------------------------------------------------------------------
!   ! Setup:
!   !
!   !  Create a source and destination igrid with data on it, to use
!   !  in the Regrid calls below.
 
    call ESMF_VMGetGlobal(vm, rc=rc)
    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPET=localPet,  rc=rc)

    layout1 = ESMF_DELayoutCreate(vm, (/ 1, npets /), rc=rc)

    pi = 3.14159
    mincoords = (/  0.0,  0.0 /)
    maxcoords = (/ 60.0, 50.0 /)
   !===========================
    !NEX_removeUTest
    !Test source igrid creation
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating source igrid"
    srcigrid = ESMF_IGridCreateHorzXYUni((/ 60, 40 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_A, &
                   name="srcigrid", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !===========================
    !NEX_removeUTest
    !Test source igrid creation
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Distributing source igrid"
    call ESMF_IGridDistribute(srcigrid, delayout=layout1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !===========================
    !NEX_removeUTest
    ! Create with similar igrid coordinates, but different layout
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating destination igrid"
    dstigrid = ESMF_IGridCreateHorzXYUni((/ 200, 100 /), &
                   mincoords, maxcoords, &
                   horzStagger=ESMF_IGRID_HORZ_STAGGER_D_NE, &
                   name="srcigrid", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !===========================
    !NEX_removeUTest
    !Test source igrid creation
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Distributing destination igrid"
    call ESMF_IGridDistribute(dstigrid, delayout=layout1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !===========================
    !NEX_removeUTest
    !Test specifications setting for the field array
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Setting array specifications"
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !===========================
    !NEX_removeUTest
    ! allow for a halo width of 3, let field create data space
    halo = 3
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Field creation"
    field1 = ESMF_FieldCreate(srcigrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                              haloWidth=3, name="src pressure", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                                
   !===========================
    !NEX_removeUTest
    ! get a fortran pointer to the data spacd
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting a pointer to the source field"
    call ESMF_FieldGetDataPointer(field1, f90ptr1, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
   !===========================
    !NEX_removeUTest
    !get the cell-centeredcoordinates of the source igrid
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting the c-c coordinate ESMF array of the source igrid"
    call ESMF_IGridGetCoord(srcigrid, dim=1, horzRelLoc=ESMF_CELL_CENTER,  &
                           centercoord=x_coords, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   !===========================
    !NEX_removeUTest
    !get the cell-centeredcoordinates of the source igrid
    call ESMF_IGridGetCoord(srcigrid, dim=2, horzRelLoc=ESMF_CELL_CENTER,  &
                           centercoord=y_coords, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    lb(:) = lbound(f90ptr1)
    ub(:) = ubound(f90ptr1)
    
    !Set the values of the c-c igrid points equal to their x-coordinate
    f90ptr1(:,:) = 0.0
    do j=lb(2)+halo, ub(2)-halo
      do i=lb(1)+halo, ub(1)-halo
        f90ptr1(i, j) =  10.0 + 5.0*sin(x_coords(i-halo,j-halo)/60.0*pi) &
                              + 2.0*sin(y_coords(i-halo,j-halo)/50.0*pi)
      enddo
    enddo

   !===========================
    !NEX_removeUTest
    !create the destination field
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Creating the destination field"
    field2 = ESMF_FieldCreate(dstigrid, arrayspec, horzRelloc=ESMF_CELL_NFACE, &
                              name="dst pressure", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

 
    ! fields all ready to go


!BOE
!\subsubsection{Precomputing and Executing a Regrid}
      
!  The user has already created an {\tt ESMF\_IGrid}, an
!  {\tt ESMF\_Array} with data, and put them together in an {\tt ESMF\_Field}.
!  An {\tt ESMF\_RouteHandle} is created and the data movement needed to
!  execute the regrid is stored with that handle by the store method. 
!  To actually execute the operation, the source and destination data
!  objects must be supplied, along with the same {\tt ESMF\_RouteHandle}.
!EOE
      

!BOC
   !===========================
    !NEX_removeUTest
    !Do all the calculations in preparation for the actual re-igridding
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Do and Store all the Regrid calcuations"
    call ESMF_FieldRegridStore(field1, field2, vm, &
                               routehandle=regrid_rh, &
                               regridmethod=ESMF_REGRID_METHOD_BILINEAR, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!EOC

!BOC
   !===========================
    !NEX_removeUTest
    !Regrid
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Regrid"
    call ESMF_FieldRegrid(field1, field2, regrid_rh, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!EOC

   !===========================
    !NEX_removeUTest
    !Get a pointer to the data in the destination field
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting a pointer to the destination field"
    call ESMF_FieldGetDataPointer(field2, f90ptr2, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    lb(:) = lbound(f90ptr2)
    ub(:) = ubound(f90ptr2)
    allocate(test(lb(1):ub(1), lb(2):ub(2)))
    
    print *, localPet,'In igrid 2 lb=',lb,'  ub=',ub
  !  print *, localPet,'Array after re-igridding is ',f90ptr2

    call ESMF_IGridGetCoord(dstigrid, dim=1, horzRelLoc=ESMF_CELL_CENTER,  &
           centercoord=x_coords2, rc=rc)
    call ESMF_IGridGetCoord(dstigrid, dim=2, horzRelLoc=ESMF_CELL_CENTER,  &
           centercoord=y_coords2, rc=rc)

   loop_rc=ESMF_SUCCESS
   do j=lb(2),ub(2)
     do i=lb(1),ub(1)
        test(i,j) =  10.0 + 5.0*sin(x_coords2(i,j)/60.0*pi) &
                          + 2.0*sin(y_coords2(i,j)/50.0*pi)
        if ((abs(test(i,j) - f90ptr2(i,j))/test(i,j)) .gt. 0.030) &
            loop_rc=ESMF_FAILURE
     end do
   end do
   write(*,*) 'got out'
   !---------------------------------------------------------------
   !NEX_removeUTest
   !Test Regrid for simple re-distribution operation
    write(failMsg, *) "Error in regrid -- row decomposition test"
    write(name, *) "Regrid test"
    call ESMF_Test( (loop_rc .eq. ESMF_SUCCESS), &
                    & name, failMsg, result, ESMF_SRCLINE)
    print *, localPet,'regridded value is more than three percent different ', &
             'than the calculated value.'

!BOC
    call ESMF_FieldRegridRelease(regrid_rh, rc=rc)
!EOC


    print *, "Regrid Unit test  returned"

!-------------------------------------------------------------------------
!    ! Cleanup

    deallocate(test)

    call ESMF_FieldDestroy(field1, rc=rc)

    call ESMF_FieldDestroy(field2, rc=rc)

    call ESMF_IGridDestroy(srcigrid, rc=rc)

    call ESMF_IGridDestroy(dstigrid, rc=rc)

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_FieldRegridBandingUTest
