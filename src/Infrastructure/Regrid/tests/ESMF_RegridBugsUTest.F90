! $Id: ESMF_RegridBugsUTest.F90,v 1.1.2.2 2006/11/23 18:47:20 donstark Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_RegridBugTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_RegridBugTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Regrid unit tests designed to confirm
! that known bugs have been fixed.
! This version works for V2.2.2 and above of the framework
! Regrid methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod    ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_RegridBugsUTest.F90,v 1.1.2.2 2006/11/23 18:47:20 donstark Exp $'
!------------------------------------------------------------------------------
      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! Grid and Distribution Handles
      Type (ESMF_VM)   :: vm
      Type (ESMF_Grid) :: Sgrid, Dgrid
      Type (ESMF_Field) :: Sfield, Dfield
      Type (ESMF_RouteHandle) :: routehandle
      Type (ESMF_DELayout) :: delayout
      Type (ESMF_ArraySpec) :: arrayspec
      Type(ESMF_RelLoc) :: srcRelLoc, dstRelLoc

      ! Local variables
      Integer :: i, j, ixS, iyS, ixD, iyD
      Integer :: cellCounts(2), decount(2)
      Integer :: npets, myDE
      real(ESMF_KIND_R8) :: error, calc, maxError, maxPerError
      real(ESMF_KIND_R8) :: minCValue, maxCValue

      ! Data Arrays
      real(ESMF_KIND_R8), dimension(:,:), pointer :: Sfptr,Dfptr

      ! Parameters
      real(ESMF_KIND_R8), parameter :: pi = 3.1415927d0
      real(ESMF_KIND_R8), parameter :: d2r = pi/180.0d0,r2d = 180.0d0/pi,pi2 = 360.0d0
!
      real(ESMF_KIND_R8), pointer, dimension(:,:) :: localX,localY,DlocalX,DlocalY
      Type (ESMF_Array), dimension(2) :: dstlocalcoord, srclocalcoord
      ! Source Grid
      integer, parameter :: slons = 12, slats = 19
      real(ESMF_KIND_R8), parameter :: SvLat(slats) = (/-90.,-80.,-70.,-60.,-50.,     &
                -40.,-30.,-20.,-10.,0.,10.,20.,30.,40.,50.,60.,70.,80.,90./)
      real(ESMF_KIND_R8), parameter :: SvLon(slons) =  (/0.,30.,60.,90.,120.,150.,    &
                180.,210.,240.,270.,300.,330./)
      ! Destination Grid
      integer, parameter :: dlons = 11, dlats = 13
      real(ESMF_KIND_R8), parameter ::  DvLat(dlats) = (/-60.,-55.,-50.,-45.,-40.,    &
                -35.,-30.,-25.,-20.,-15.,-10.,-5.,0. /)
      real(ESMF_KIND_R8), parameter :: DvLon(dlons) = (/-110.,-100.,-90.,-80.,-70.,   &
                -60.,-50.,-40.,-30.,-20.,-10./)


      !------------------------------------------------------------------------
      !  Start program
      !------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      if (.not. ESMF_TestMinPETs(8, ESMF_SRCLINE)) goto 10


      ! Query for Global VM and create a layout with the right breakdown
      
      !------------------------------------------------------------------------
      call ESMF_VMGetGlobal(vm, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      if (npets .eq. 1) then
        decount(:) = (/ 1, 1 /)
      else
        decount(:) = (/ 2, npets/2 /)
      endif
      delayout = ESMF_DELayoutCreate(vm, decount, rc=rc)
      write(failMsg, *) "Failed creating a delayout rc =", rc
      write(name, *) "Creating a DELayout"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !-----------------------
      ! construct spherical test grids
      !-----------------------
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      Sgrid = ESMF_GridCreateHorzLatLon( SvLon, SvLat,                      &
                                    horzstagger=ESMF_GRID_HORZ_STAGGER_A,   &
                                    dimNames=(/"longitude", "latitude "/),  &
                                    dimUnits=(/"degrees",   "degrees"  /),  &
                                    coordorder=ESMF_COORD_ORDER_XYZ,        &
                                    periodic=(/ESMF_TRUE, ESMF_FALSE/),     &
                                    name="Source grid", rc=rc)
      write(failMsg, *) "Failed creating spherical SRC grid rc =", rc
      write(name, *) "Creating Spherical SRC grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      Dgrid = ESMF_GridCreateHorzLatLon( DvLon, DvLat,                      &
                                    horzstagger=ESMF_GRID_HORZ_STAGGER_A,   &
                                    dimNames=(/"longitude", "latitude "/),  &
                                    dimUnits=(/"degrees",   "degrees"  /),  &
                                    coordorder=ESMF_COORD_ORDER_XYZ,        &
                                    periodic=(/ESMF_FALSE, ESMF_FALSE/),    &
                                    name="Destination grid", rc=rc)
      write(failMsg, *) "Failed creating spherical DST grid rc =", rc
      write(name, *) "Creating Spherical DST grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !-----------------------
      ! distribute grid and validate
      !-----------------------
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      call ESMF_GridDistribute( Sgrid, delayout=delayout,rc=rc)
      write(failMsg, *) "Failed Distributing spherical SRC grid"
      write(name, *) "Distributing spherical SRC grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      call ESMF_GridDistribute( Dgrid, delayout=delayout,rc=rc)
      write(failMsg, *) "Failed Distributing spherical DST grid"
      write(name, *) "Distributing spherical DST grid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !-----------------------
      !  Create Source Field
      !-----------------------
      ! Get local Grid coordinates and dimensions
      !-----------------------
      call ESMF_GridGetDELocalInfo(Sgrid, ESMF_CELL_CENTER,             &
                 localCellCountPerDim=cellCounts, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      ixS = cellCounts(1)
      iyS = cellCounts(2)
!
      srcRelLoc = ESMF_CELL_CENTER
      call ESMF_GridGetCoord( Sgrid, horzRelloc=srcRelLoc,    &
                           centerCoord=srclocalcoord, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_ArrayGetData(srclocalcoord(1), localX, ESMF_DATA_COPY, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_ArrayGetData(srclocalcoord(2), localY, ESMF_DATA_COPY, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !---------------------------------
      ! Set up and initialize a 2D fortran array for source field
      !---------------------------------
      allocate( Sfptr(ixS,iyS) )
      do j = 1,iyS
        do i = 1,ixS
          Sfptr(i,j) = 10.0 + 5.0*sin(d2r*localX(i,j))               &
                              + 2.0*sin(d2r*localY(i,j)*2.0)
        enddo
      enddo
      ! Create the field from the fortran array
      Sfield = ESMF_FieldCreate(Sgrid, Sfptr, ESMF_DATA_REF,         &
               horzRelloc=ESMF_CELL_CENTER,                          &
               name="Source Field", rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      !-----------------------
      !  Create Destination Field
      !-----------------------
      call ESMF_ArraySpecSet(arrayspec, rank=2, type=ESMF_DATA_REAL, &
                          kind=ESMF_R8,rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      Dfield = ESMF_FieldCreate(Dgrid, arrayspec=arrayspec,          &
                            horzRelloc=ESMF_CELL_CENTER,             &
                            name="Destination field", rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      ! Get the allocated array back as an F90 array pointer
      call ESMF_FieldGetDataPointer(Dfield, Dfptr, ESMF_DATA_REF, rc=rc)
      Dfptr(:,:) = -9999.0
      !-----------------------
      ! Get local Grid coordinates and dimensions
      !-----------------------
      call ESMF_GridGetDELocalInfo(Dgrid, ESMF_CELL_CENTER,          &
                 localCellCountPerDim=cellCounts, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      ixD = cellCounts(1)
      iyD = cellCounts(2)
      !-----------------------
      ! Regrid
      !-----------------------
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      call ESMF_FieldRegridStore(Sfield,Dfield, vm, routehandle,     &
                  regridmethod=ESMF_REGRID_METHOD_CONSERV1, rc=rc)
      write(failMsg, *) "Field Regrid Store Failed"  
      write(name, *) "Field Regrid Store completed"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      call ESMF_FieldRegrid(Sfield, Dfield, routehandle, rc=rc)
      write(failMsg, *) "Field Regrid Run Failed"
      write(name, *) "Field Regrid Run completed"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !-----------------------
      ! Verify Results
      !-----------------------
      dstRelLoc = ESMF_CELL_CENTER
      call ESMF_GridGetCoord(Dgrid, horzRelloc=dstRelLoc,             &
                         centerCoord=dstlocalcoord, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_ArrayGetData(dstlocalcoord(1), DlocalX, &
                               ESMF_DATA_COPY, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_ArrayGetData(dstlocalcoord(2), DlocalY, &
                               ESMF_DATA_COPY, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      !---------------------------------
      ! Set up and initialize a 2D fortran array for source field
      !---------------------------------
      ! calculate data error from computed results
      maxError    = 0.0
      maxPerError = 0.0
      maxCValue   = 0.0
      minCValue   = 1000.0
      do j = 1,iyD
        do i = 1,ixD
          calc = 10.0 + 5.0*sin(d2r*DlocalX(i,j))          &
                        + 2.0*sin(d2r*DlocalY(i,j)*2.0)
          error = abs( Dfptr(i,j) - calc )
          minCValue   = min(minCValue, abs(calc))
          maxCValue   = max(maxCValue, abs(calc))
          maxError    = max(maxError, abs(error))
          maxPerError = max(maxPerError, 100.*abs(error)/abs(calc))
        enddo
      enddo
      write(*,*) " "
      write(*,*) "Detailed results for DE #", myDE, ":"
      write(*,*) "   minimum computed value  = ", minCValue
      write(*,*) "   maximum computed value  = ", maxCValue
      write(*,*) "   maximum error           = ", maxError
      write(*,*) "   maximum percent error   = ", maxPerError

      !------------------------------------------------------------------------
      ! validate the results
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Percentage error in data result is too large"
      write(name, *) "Regrid percentage error less than 10%"
      call ESMF_Test((maxError .lt. 10.0), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE
      ! add more tests here.
#endif
      !-----------------------
      ! finish up
      !-----------------------
      deallocate( Sfptr,Dfptr )
      call ESMF_FieldDestroy(Dfield, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_FieldDestroy(Sfield, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridDestroy( Sgrid, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridDestroy( Dgrid, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_DELayoutDestroy(delayout, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
!-------------------------------------------------------------------------
!   ! Release resources stored for the Regridding.
!-------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      call ESMF_FieldRegridRelease(routehandle, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Releasing the routehandle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 10

10    continue

      ! return number of failures to environment; 0 = success (all pass)

      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      end program ESMF_RegridBugTest
