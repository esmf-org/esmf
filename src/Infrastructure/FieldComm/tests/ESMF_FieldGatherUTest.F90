! $Id: ESMF_FieldGatherUTest.F90,v 1.1 2004/09/23 20:45:13 jwolfe Exp $
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
      program ESMF_FieldGatherUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Macros.inc>
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
      '$Id: ESMF_FieldGatherUTest.F90,v 1.1 2004/09/23 20:45:13 jwolfe Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name, gName, Rgname

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      integer :: i, j, ifld, jfld
      integer :: npets, myDE
      integer :: hWidth
      integer, dimension(2) :: counts, localCounts
      real(ESMF_KIND_R8) :: pi, minGather, maxGather
      real(ESMF_KIND_R8), dimension(2) :: min, max
      real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY
      real(ESMF_KIND_R8), dimension(:,:), pointer :: srcData, gatheredData
      type(ESMF_GridHorzStagger) :: horzStagger
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Array) :: array1, array2
      type(ESMF_Array), dimension(:), pointer :: coordArray
      type(ESMF_Grid)  ::  grid
      type(ESMF_Field) :: field
      type(ESMF_VM):: vm
      type(ESMF_DELayout) :: delayout

!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------

      !NEX_UTest
      call ESMF_Initialize(vm=vm, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20

      ! Get the PET count
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20

      if (npets .eq. 1) then
        print *, "This test must run with > 1 processor"
        goto 20
      endif

      ! Create a 2D layout to be used by the Field
      delayout = ESMF_DELayoutCreate(vm, (/ 2, npets/2 /), rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20

      ! Create a grid and corresponding Field.  Note that the counts are
      ! prime numbers to ensure the grid can not be evenly distributed
      pi              = 3.14159
      hWidth          = 2
      counts(1)       = 61
      counts(2)       = 53
      min(1)          = 0.0
      max(1)          = 61.0
      min(2)          = 0.0
      max(2)          = 53.0
      horzStagger     = ESMF_GRID_HORZ_STAGGER_A
      call ESMF_ArraySpecSet(arrayspec, rank=2, type=ESMF_DATA_REAL, &
                             kind=ESMF_R8)

      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                                      minGlobalCoordPerDim=min, &
                                      maxGlobalCoordPerDim=max, &
                                      horzStagger=horzStagger, &
                                      name="source grid", rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20
      call ESMF_GridDistribute(grid, delayout=delayout, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20
      field = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                               haloWidth=hWidth, name="field", rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20

      ! Get coordinate arrays available for setting the source data array
      allocate(coordArray(2))
      call ESMF_GridGetCoord(grid, horzRelloc=ESMF_CELL_CENTER, &
                             centerCoord=coordArray, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20
      call ESMF_ArrayGetData(coordArray(1), coordX, ESMF_DATA_REF, rc)
      if (rc .ne. ESMF_SUCCESS) goto 20
      call ESMF_ArrayGetData(coordArray(2), coordY, ESMF_DATA_REF, rc)
      if (rc .ne. ESMF_SUCCESS) goto 20
      call ESMF_ArrayGet(coordArray(1), counts=localCounts, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20

      ! Get pointers to the data and set it up
      call ESMF_FieldGetArray(field, array1, rc)
      if (rc .ne. ESMF_SUCCESS) goto 20
      call ESMF_ArrayGetData(array1, srcData, ESMF_DATA_REF, rc)
      if (rc .ne. ESMF_SUCCESS) goto 20

      ! initialize data arrays
      srcData = 0.0

      ! set data array to a function of coordinates (in the computational part
      ! of the array only, not the halo region)
      do j   = 1,localCounts(2)
        do i = 1,localCounts(1)
          srcData(i+hWidth,j+hWidth) = 10.0 + 5.0*sin(coordX(i,j)/61.0*pi) &
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
      call ESMF_FieldGather(field, 0, array2, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 20

      ! check results, at least if the values are in the global computational
      ! range
      call ESMF_DELayoutGet(delayout, localDE=myDE, rc=rc)
      if (myDE.eq.0) then
        call ESMF_ArrayGetData(array2, gatheredData, ESMF_DATA_REF, rc)
        if (rc .ne. ESMF_SUCCESS) goto 20
        minGather =  9999999.
        maxGather = -9999999.
        do j   = 1,counts(2)-2*hwidth
          jfld = j+hWidth
          do i = 1,counts(1)-2*hwidth
            ifld = i+hWidth
            if (minGather.lt.gatheredData(ifld,jfld)) &
                minGather =  gatheredData(ifld,jfld)
            if (maxGather.gt.gatheredData(ifld,jfld)) &
                maxGather =  gatheredData(ifld,jfld)
          enddo
        enddo
        write(failMsg, *) "Did not calculate correct results"
        write(name, *) "Field Gather Test"
        call ESMF_Test((minGather.ge.10.0 .AND. maxGather.le.32.0), &
                        name, failMsg, result, ESMF_SRCLINE)
      endif

      ! Clean up
      call ESMF_FieldDestroy(field, rc)
      if (rc .ne. ESMF_SUCCESS) goto 20
      call ESMF_GridDestroy(grid, rc)
      if (rc .ne. ESMF_SUCCESS) goto 20
      call ESMF_DELayoutDestroy(delayout, rc)
      if (rc .ne. ESMF_SUCCESS) goto 20

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
   20 call ESMF_Finalize(rc)

      end program ESMF_FieldGatherUTest
