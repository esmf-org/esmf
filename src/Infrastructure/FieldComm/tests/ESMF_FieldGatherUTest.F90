! $Id: ESMF_FieldGatherUTest.F90,v 1.20.2.4 2009/01/21 21:25:21 cdeluca Exp $
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
      program ESMF_FieldGatherUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_Macros.inc"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldGatherUTest - This test verifies FieldGather functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of FiledGather.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
    
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldGatherUTest.F90,v 1.20.2.4 2009/01/21 21:25:21 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      integer :: i, j, ifld, jfld
      integer :: npets, myDE
      integer :: hWidth
      integer, dimension(2) :: counts, localCounts
      logical :: ok
      real(ESMF_KIND_R8) :: pi, minGather, maxGather
      real(ESMF_KIND_R8), dimension(2) :: min, max
      real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY
      real(ESMF_KIND_R8), dimension(:,:), pointer :: srcData, gatheredData
      type(ESMF_IGridHorzStagger) :: horzStagger
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_InternArray) :: array2
      type(ESMF_IGrid)  ::  igrid
      type(ESMF_Field) :: field
      type(ESMF_VM):: vm
      type(ESMF_DELayout) :: delayout

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      if (.not. ESMF_TestMinPETs(6, ESMF_SRCLINE)) goto 20

      ! Get the PET count
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20

#if ESMF_TESTEXHAUSTIVE
!-----------------------------------------------------------------------------
      ! Create a 2D layout to be used by the Field
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a DELayout Test"
      delayout = ESMF_DELayoutCreate(vm, (/ 2, npets/2 /), rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20

!-----------------------------------------------------------------------------
      ! Create a igrid and corresponding Field.  Note that the counts are
      ! prime numbers to ensure the igrid can not be evenly distributed
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Array Spec Set Test"
      pi              = 3.14159
      hWidth          = 2
      counts(1)       = 61
      counts(2)       = 53
      min(1)          = 0.0
      max(1)          = 61.0
      min(2)          = 0.0
      max(2)          = 53.0
      horzStagger     = ESMF_IGRID_HORZ_STAGGER_A
      call ESMF_ArraySpecSet(arrayspec, rank=2, &
                             typekind=ESMF_TYPEKIND_R8, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-----------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid Create Horz XYUni Test"
      igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                                      minGlobalCoordPerDim=min, &
                                      maxGlobalCoordPerDim=max, &
                                      horzStagger=horzStagger, &
                                      name="source igrid", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20


!-----------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid Distribute Test"
      call ESMF_IGridDistribute(igrid, delayout=delayout, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20

!-----------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Create Test"
      field = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                               haloWidth=hWidth, name="field", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20

!-----------------------------------------------------------------------------
      ! Get coordinate arrays available for setting the source data array
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid Get Corrd Test"
      call ESMF_IGridGetCoord(igrid, dim=1, horzRelloc=ESMF_CELL_CENTER, &
                             centerCoord=coordX, localCounts=localCounts, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20


!-----------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid Get Corrd Test"
      call ESMF_IGridGetCoord(igrid, dim=2, horzRelloc=ESMF_CELL_CENTER, &
                             centerCoord=coordY, localCounts=localCounts, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20

!-----------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Get Data Test"
      call ESMF_FieldGetDataPointer(field, srcData, copyflag=ESMF_DATA_REF, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20

      ! initialize data arrays
      srcData = 0.0

      ! set data array to a function of coordinates (in the computational part
      ! of the array only, not the halo region)
      do j   = 1,localCounts(2)
        do i = 1,localCounts(1)
          srcData(i,j) = 10.0 + 5.0*sin(coordX(i,j)/61.0*pi) &
                              + 2.0*sin(coordY(i,j)/53.0*pi) 
        enddo
      enddo

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
      ! Call gather method here, output ends up in array2 on DE0
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Gather Test"
      call ESMF_FieldGather(field, 0, array2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
      ! check results, at least if the values are in the global computational
      ! range
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout Get Test"
      call ESMF_DELayoutGetDeprecated(delayout, localDE=myDE, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-----------------------------------------------------------------------------
      ok = .true.
      if (myDE.eq.0) then
        call ESMF_InternArrayGetData(array2, gatheredData, ESMF_DATA_REF, rc)
        if (rc .ne. ESMF_SUCCESS) goto 20
        minGather =  9999999.
        maxGather = -9999999.
        do j   = 1,counts(2)-2*hwidth
          jfld = j+hWidth
          do i = 1,counts(1)-2*hwidth
            ifld = i+hWidth
            if (minGather.gt.gatheredData(ifld,jfld)) &
                minGather =  gatheredData(ifld,jfld)
            if (maxGather.lt.gatheredData(ifld,jfld)) &
                maxGather =  gatheredData(ifld,jfld)
          enddo
        enddo
        ok = (minGather.ge.10.0 .AND. maxGather.le.17.0)
      endif
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not calculate correct results"
      write(name, *) "Field Gather Test"
      call ESMF_Test(ok, name, failMsg, result, ESMF_SRCLINE)

      ! Clean up

!-----------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Field Destroy Test"
      call ESMF_FieldDestroy(field, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20

!-----------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "IGrid Destroy Test"
      call ESMF_IGridDestroy(igrid, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20

!-----------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout Destroy Test"
      call ESMF_DELayoutDestroy(delayout, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (rc .ne. ESMF_SUCCESS) goto 20
#endif

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
   20 continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_FieldGatherUTest
