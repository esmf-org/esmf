! $Id: ESMF_RegridUTest.F90,v 1.24.2.4 2009/01/21 21:25:23 cdeluca Exp $
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
      program ESMF_RegridTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_RegridTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Regrid unit tests.
! The companion file ESMF\_Regrid.F90 contains the definitions for the
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
      '$Id: ESMF_RegridUTest.F90,v 1.24.2.4 2009/01/21 21:25:23 cdeluca Exp $'
!------------------------------------------------------------------------------
      type(ESMF_VM):: vm

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      !character(ESMF_MAXSTR) :: validate_options
      !character(ESMF_MAXSTR) :: print_options

      ! Local variables
      type(ESMF_Field) :: humidity1, humidity2
      type(ESMF_DELayout) :: delayout
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_IGrid) :: igrid1, igrid2
      real(ESMF_KIND_R8) :: min(2), max(2)
      integer :: counts(ESMF_MAXIGRIDDIM)
      integer :: npets, countsPerDE1(4), countsPerDE2(2)
      real(ESMF_KIND_R8) :: delta1(40), delta2(50)
      type(ESMF_IGridHorzStagger) :: horz_stagger
      type(ESMF_RouteHandle) :: routehandle


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      if (.not. ESMF_TestMinPETs(8, ESMF_SRCLINE)) goto 10


      ! Query for Global VM and create a layout with the right breakdown
      
      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      delayout = ESMF_DELayoutCreate(vm, (/ npets/2, 2 /), rc=rc)
      write(failMsg, *) "creating a delayout rc =", rc
      write(name, *) "Creating a DELayout"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! Set up a 2D real arrayspec
      call ESMF_ArraySpecSet(arrayspec, rank=2, &
                             typekind=ESMF_TYPEKIND_R8)
      write(failMsg, *) "setting an arrayspec rc =", rc
      write(name, *) "Initializing an ArraySpec"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! Add a "humidity" field to the export state.
      countsPerDE1 = (/ 15, 15, 15, 15 /)
      countsPerDE2 = (/ 40, 0 /)

      counts(1) = 60
      counts(2) = 40
      min(1) = 0.0
      max(1) = 60.0
      min(2) = 0.0
      max(2) = 50.0
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A

      igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                  minGlobalCoordPerDim=min, &
                                  maxGlobalCoordPerDim=max, &
                                  horzStagger=horz_stagger, &
                                  name="source igrid", rc=rc)
      write(failMsg, *) "igrid create rc =", rc 
      write(name, *) "Creating an XY Uniform IGrid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      call ESMF_IGridDistribute(igrid1, delayout=delayout, &
                                 countsPerDEDim1=countsPerDE1, &
                                 countsPerDEDim2=countsPerDE2, &
                                 rc=rc)
      write(failMsg, *) "igrid distribute rc =", rc
      write(name, *) "Distributing the IGrid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! Create the field and have it create the array internally
      humidity1 = ESMF_FieldCreate(igrid1, arrayspec, &
                                   horzRelloc=ESMF_CELL_CENTER, &
                                   haloWidth=0, name="humidity1", rc=rc)
      write(failMsg, *) "field create rc =", rc
      write(name, *) "field create rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! Add a "humidity" field to the import state.
      countsPerDE1 = (/ 10, 6, 12, 12 /)
      countsPerDE2 = (/ 0, 50 /)
      min(1) = 0.0
      delta1 = (/ 1.0, 1.0, 1.0, 1.1, 1.1, 1.1, 1.2, 1.2, 1.3, 1.4, &
                    1.4, 1.5, 1.6, 1.6, 1.6, 1.8, 1.8, 1.7, 1.7, 1.6, &
                    1.6, 1.6, 1.8, 1.8, 2.0, 2.0, 2.2, 2.2, 2.2, 2.2, &
                    2.0, 1.7, 1.5, 1.3, 1.2, 1.1, 1.0, 1.0, 1.0, 0.9 /)
      min(2) = 0.0
      delta2 = (/ 0.8, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.6, 0.7, 0.8, &
                    0.9, 0.9, 0.9, 0.9, 1.0, 1.0, 1.0, 1.0, 0.9, 1.0, &
                    1.0, 1.0, 1.0, 1.1, 1.2, 1.3, 1.3, 1.3, 1.4, 1.4, &
                    1.4, 1.4, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.2, 1.2, &
                    1.1, 1.0, 1.0, 0.9, 0.8, 0.7, 0.6, 0.6, 0.5, 0.5 /)
      min(1) = 0.0
      min(2) = 0.0
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_D_NE

      igrid2 = ESMF_IGridCreateHorzXY(minGlobalCoordPerDim=min, &
                                      delta1=delta1, delta2=delta2, &
                                      horzStagger=horz_stagger, &
                                      name="source igrid", rc=rc)
      write(failMsg, *) "igrid create rc =", rc
      write(name, *) "igrid create rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      call ESMF_IGridDistribute(igrid2, delayout=delayout, &
                                 countsPerDEDim1=countsPerDE1, &
                                 countsPerDEDim2=countsPerDE2, &
                                 rc=rc)
      write(failMsg, *) "igrid distribute rc =", rc
      write(name, *) "igrid distribute rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! Create the field and have it create the array internally
      humidity2 = ESMF_FieldCreate(igrid2, arrayspec, &
                                     horzRelloc=ESMF_CELL_NFACE, &
                                     haloWidth=0, name="humidity2", rc=rc)
      write(failMsg, *) "field create rc =", rc
      write(name, *) "field create rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  

      !call ESMF_FieldPrint(humidity1, rc=rc)
      !call ESMF_FieldPrint(humidity2, rc=rc)
      !------------------------------------------------------------------------
      ! Up to here the tests are all part of the set up.  From here on,
      ! the tests are really regrid tests.
      !------------------------------------------------------------------------
      
      !------------------------------------------------------------------------
      ! These are fields on different IGrids - call RegridStore to set
      ! up the Regrid structure

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      print *, "ready to call regrid store"
      call ESMF_FieldRegridStore(humidity1, humidity2, vm, &
                                 routehandle, &
                                 regridmethod=ESMF_REGRID_METHOD_BILINEAR, &
                                 rc=rc)
      print *, "back from regrid store"
      write(failMsg, *) "regrid store rc =", rc
      write(name, *) "regrid store rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      print *, "ready to call regrid run"
      call ESMF_FieldRegrid(humidity1, humidity2, routehandle, rc=rc)
      print *, "back from regrid run"
      write(failMsg, *) "regrid run rc =", rc
      write(name, *) "regrid run rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !call ESMF_FieldPrint(humidity1, rc=rc)
      !call ESMF_FieldPrint(humidity2, rc=rc)
      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      print *, "ready to call regrid release"
      call ESMF_FieldRegridRelease(routehandle, rc=rc)
      print *, "back from regrid release"
      write(failMsg, *) "regrid release rc =", rc
      write(name, *) "regrid release rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
#ifdef ESMF_TESTEXHAUSTIVE
      ! add more tests here.

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      call ESMF_FieldRegridStore(humidity1, humidity2, vm, &
                                 routehandle, &
                                 regridmethod=ESMF_REGRID_METHOD_NEAR_NBR, &
                                 rc=rc)
      write(failMsg, *) "regrid store, bad method rc =", rc
      write(name, *) "regrid store, bad method rc =", rc
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

10    continue

      ! TODO: we would like to set the program return value for use by
      ! the makefile, but is there no way to do this in F90?
      ! return number of failures to environment; 0 = success (all pass)

      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      end program ESMF_RegridTest
