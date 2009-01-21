! $Id: ESMF_FRoute4UTest.F90,v 1.24.2.4 2009/01/21 21:25:20 cdeluca Exp $
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
      program ESMF_FRouteUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_FRouteUTest - Unit test for Field Route function.
!
! !DESCRIPTION:
!
! The code in this file drives the F90 Field Route tests.  The Field
!   Route function is complex enough to require a separate test file.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FRoute4UTest.F90,v 1.24.2.4 2009/01/21 21:25:20 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      type(ESMF_IGrid) :: igrid1, igrid2, igrid3
      type(ESMF_ArraySpec) :: arrayspec
      integer, dimension(ESMF_MAXDIM) :: g1_cells, g2_cells
      type(ESMF_FieldDataMap) :: dm
      !type(ESMF_RelLoc) :: rl
      type(ESMF_DELayout) :: layout1, layout2
      type(ESMF_VM) :: vm
      type(ESMF_RouteHandle) :: rh
      type(ESMF_IOSpec) :: ios
      character (len = 20) :: gname
      type(ESMF_Field) :: f1, f2
      integer :: half, quart
      real (ESMF_KIND_R8):: min(2), max(2)
      integer :: counts(ESMF_MAXIGRIDDIM)
      type(ESMF_IGridHorzStagger) :: horz_stagger
      integer :: status, myde, npets

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      ! this test *must* run at least 4-way
      if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) goto 10


      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, petCount=npets, rc=rc)

      half = npets / 2
      quart = npets / 4

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! Make a Nx4 and Nx2 layout
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Creating a DELayout Test"
      layout1 = ESMF_DELayoutCreate(vm, (/ quart, 4 /), rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .eq. ESMF_FAILURE) then
        print *, "cannot create Nx4 layout"
        goto 10
      endif

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      print *, "Layout 1:"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Printing a DELayout Test"
      call ESMF_DELayoutPrint(layout1, "", rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Creating a DELayout Test"
      layout2 = ESMF_DELayoutCreate(vm, (/ half, 2 /), rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .eq. ESMF_FAILURE) then
        print *, "cannot create Nx2 layout"
        goto 10
      endif

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      print *, "Layout 1:"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Printing a DELayout Test"
      print *, "Layout 2:"
      call ESMF_DELayoutPrint(layout2, "", rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "DELayout Get Test"
      call ESMF_DELayoutGetDeprecated(layout1, localDE=myde, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      counts(1) = 48
      counts(2) = 24
      min(1) = 0.0
      max(1) = 20.0
      min(2) = 0.0
      max(2) = 5.0
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A
      gname = "test igrid 1"

      igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=gname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "Creating a source Test IGrid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid distribute Test "
      call ESMF_IGridDistribute(igrid1, delayout=layout1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! Destroy IGrid Test
      write(failMsg, *) ""
      write(name, *) "Destroy IGrid Test"
      call ESMF_IGridDestroy(igrid1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
     
      
      !EX_removeUTest_Multi_Proc_Only
      igrid1 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=gname, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a source Test IGrid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCESS"
      write(name, *) "IGrid distribute Test "
      call ESMF_IGridDistribute(igrid1, delayout=layout1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! Verifing that an uninitialized IGrid can be printed
      call ESMF_IGridPrint(igrid3, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an uninitialized IGrid Test"
!     call 
      ! Second igrid
      gname = "test igrid 2"
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_D_NE
      igrid2 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=gname, rc=status)
      call ESMF_IGridDistribute(igrid2, delayout=layout2, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a destination Test IGrid"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! Verifing that an Array can be created
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get DE Local Info Test"
      call ESMF_IGridGetDELocalInfo(igrid1, localCellCountPerDim=g1_cells, &
                          horzRelloc=ESMF_CELL_CENTER, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! second array
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get DE Local Info Test"
      call ESMF_IGridGetDELocalInfo(igrid2, localCellCountPerDim=g2_cells, &
                          horzRelloc=ESMF_CELL_CENTER)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! Test requirement FLD1.1.1
      ! Fields may be created by specifying attributes, a igrid, data array
      ! dimensions and descriptors, optional masks (e.g. for active cells),
      ! and an optional I/O specification. In this case a field will
      ! allocate its own data. The igrid passed into the argument list
      ! is referenced and not copied.
      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc=rc)
      write(name, *) "Creating an ArraySpec Test "
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! Verifing that a Field can be created with a IGrid and Array
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field DataMap Set Test"
      call ESMF_FieldDataMapSetDefault(dm, ESMF_INDEX_IJ, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a IGrid and Array Test"
      f1 = ESMF_FieldCreate(igrid1, arrayspec, allocflag=ESMF_ALLOC, &
                            horzRelloc=ESMF_CELL_CENTER, vertRelloc=ESMF_CELL_CELL, &
                            datamap=dm, haloWidth=1, name="Field 0", iospec=ios, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! second field
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a Field with a IGrid and Array Test"
      f2 = ESMF_FieldCreate(igrid2, arrayspec, allocflag=ESMF_ALLOC, &
                            horzRelloc=ESMF_CELL_CENTER, vertRelloc=ESMF_CELL_CELL, &
                            datamap=dm, haloWidth=1, name="Field 1", iospec=ios, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! route test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Redist Store Test"
      call ESMF_FieldRedistStore(f1, f2, vm, routehandle=rh, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Redist Test"
      call ESMF_FieldRedist(f1, f2, rh, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! -- add missing EX label here when this test is included --
      !write(failMsg, *) "Did not return ESMF_SUCCESS"
      !write(name, *) "Field Redist Test with different halo widths"
      ! this needs a separate test created where f1 and f2 have different
      ! halo widths -- once the code has been added to support it.
      ! (the current version of the framework does not yet contain code that
      ! can execute the redist function if the halo widths are not equal.)
      !call ESMF_FieldRedist(f1, f2, rh, rc=rc)
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Redist Release Test"
      call ESMF_FieldRedistRelease(rh, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! results
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Print Test"
      call ESMF_FieldPrint(f2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Destroy Test"
      call ESMF_FieldDestroy(f1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Destroy Test"
      call ESMF_FieldDestroy(f2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid Destroy Test"
      call ESMF_IGridDestroy(igrid1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid Destroy Test"
      call ESMF_IGridDestroy(igrid2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


#endif

10    print *, "end of Field Route test"

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_FRouteUTest
