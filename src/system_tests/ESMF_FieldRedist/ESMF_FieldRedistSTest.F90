! $Id: ESMF_FieldRedistSTest.F90,v 1.14 2004/04/12 22:12:00 jwolfe Exp $
!
! System test FieldRedist
!  Description on Sourceforge under System Test #XXXXX

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test FieldRedist.
!
! This system test checks the functionality of the FieldRedist routine by
! redistributing data from one Field to another and then back again.  The
! original data should exactly match the final data, which serves as the
! test for SUCCESS.  This program creates two identical Grids on different
! layouts.  The first Grid has two Fields created from it, the first as the
! source for the test and the second for the final results.  The second Grid
! has a single Field that serves as an intermediate result between the
! two redistributions.
!
!\begin{verbatim}

    program FieldRedist

#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    implicit none
    
    ! Local variables
    integer :: i, j, rc
    integer :: ndes, deId
    integer :: miscount, hWidth
    integer :: status
    integer, dimension(2) :: decompids1, decompids2, counts, localCounts
    logical :: match
    real(ESMF_KIND_R8) :: pi
    real(ESMF_KIND_R8), dimension(2) :: min, max
    real(ESMF_KIND_R8), dimension(:,:), pointer :: coordX, coordY
    real(ESMF_KIND_R8), dimension(:,:), pointer :: srcdata, resdata
    type(ESMF_GridType)    :: horzGridType
    type(ESMF_GridStagger) :: horzStagger
    type(ESMF_CoordSystem) :: horzCoordSystem
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array1, array2, array3
    type(ESMF_Array), dimension(:), pointer :: coordArray
    type(ESMF_DELayout) :: layout0, layout1       ! these could go once the
                                                  ! newDELayout takes over
    type(ESMF_Grid)  ::  grid1,  grid2
    type(ESMF_Field) :: field1, field2, field3
    type(ESMF_RouteHandle) :: rh12, rh23

#ifdef ESMF_ENABLE_VM
    type(ESMF_VM):: vm
    type(ESMF_newDELayout) :: delayout0, delayout1
#endif
    integer, dimension(:), allocatable :: delist
    integer :: nde(2)

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test FieldRedist:"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    call ESMF_Initialize(rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

#ifdef ESMF_ENABLE_VM
    call ESMF_VMGetGlobal(vm, rc)

    ! Create a default 1-dim DELayout with N DE's, where N is number of PETs in VM
    delayout0 = ESMF_newDELayoutCreate(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) stop
    call ESMF_newDELayoutGet(delayout0, ndes, rc=rc)
    if (rc /= ESMF_SUCCESS) stop
    call ESMF_newDELayoutPrint(delayout0)
#endif
    ! Create a default 1xN DELayout
    layout0 = ESMF_DELayoutCreate(rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_DELayoutGetNumDES(layout0, ndes, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    if (ndes .eq. 1) then
       print *, "This test must run with > 1 processor"
       goto 20
    endif

    ! And then create a 2D layout to be used by the Fields
#ifdef ESMF_ENABLE_VM
    delayout1 = ESMF_newDELayoutCreate(vm, (/ 2, ndes/2 /), rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_newDELayoutPrint(delayout1)
#endif
    nde(1) = 2
    nde(2) = ndes/2
    allocate(delist(ndes))
    delist = (/ (i, i=0, ndes-1) /)
    layout1 = ESMF_DELayoutCreate(layout0, 2, (/ nde(1), nde(2) /), (/ 0, 0 /), &
                                  de_indices=delist, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "DELayout Create finished, rc =", rc

    print *, "Create section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    !  Create the grids and corresponding Fields
    !  note that the Grids are the same but decomposed differently
    pi              = 3.14159
    hWidth          = 2
    counts(1)       = 60
    counts(2)       = 50
    min(1)          = 0.0
    max(1)          = 60.0
    min(2)          = 0.0
    max(2)          = 50.0
    horzGridType    = ESMF_GridType_XY
    horzStagger     = ESMF_GridStagger_A
    horzCoordSystem = ESMF_CoordSystem_Cartesian
    call ESMF_ArraySpecSet(arrayspec, rank=2, type=ESMF_DATA_REAL, &
                            kind=ESMF_R8)

    decompids1(1) = 1
    decompids1(2) = 2
    grid1 = ESMF_GridCreateLogRectUniform(2, counts=counts, &
                                          minGlobalCoordPerDim=min, &
                                          maxGlobalCoordPerDim=max, &
                                          layout=layout1, &
                                          decompIds=decompids1, &
                                          horzGridType=horzGridType, &
                                          horzStagger=horzStagger, &
                                          horzCoordSystem=horzCoordSystem, &
                                          name="source grid", rc=status &
#ifdef ESMF_ENABLE_VM
                                           , delayout=delayout1 &
#endif
                                          )
    field1 = ESMF_FieldCreate(grid1, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                              haloWidth=hWidth, name="field1", rc=rc)
    field3 = ESMF_FieldCreate(grid1, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                              haloWidth=hWidth, name="field3", rc=rc)

    decompids2(1) = 2
    decompids2(2) = 1
    grid2 = ESMF_GridCreateLogRectUniform(2, counts=counts, &
                                          minGlobalCoordPerDim=min, &
                                          maxGlobalCoordPerDim=max, &
                                          layout=layout1, &
                                          decompIds=decompids2, &
                                          horzGridType=horzGridType, &
                                          horzStagger=horzStagger, &
                                          horzCoordSystem=horzCoordSystem, &
                                          name="destination grid", rc=status &
#ifdef ESMF_ENABLE_VM
                                           , delayout=delayout1 &
#endif
                                          )
    field2 = ESMF_FieldCreate(grid2, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                              haloWidth=hWidth, name="field2", rc=rc)

    ! precompute communication patterns
    call ESMF_FieldRedistStore(field1, field2, layout1, rh12, rc=status &
#ifdef ESMF_ENABLE_VM
                               , parentDelayout=delayout1 &
#endif
                               )
    call ESMF_FieldRedistStore(field2, field3, layout1, rh23, rc=status &
#ifdef ESMF_ENABLE_VM
                               , parentDelayout=delayout1 &
#endif
                               )

    ! get coordinate arrays available for setting the source data array
    allocate(coordArray(2))
    call ESMF_GridGetCoord(grid1, horzRelloc=ESMF_CELL_CENTER, &
                           centerCoord=coordArray, rc=status)
    call ESMF_ArrayGetData(coordArray(1), coordX, ESMF_DATA_REF, status)
    call ESMF_ArrayGetData(coordArray(2), coordY, ESMF_DATA_REF, status)
    call ESMF_ArrayGet(coordArray(1), counts=localCounts, rc=status)

    ! Get pointers to the data and set it up
    call ESMF_FieldGetArray(field1, array1, rc)
    call ESMF_ArrayGetData(array1, srcdata, ESMF_DATA_REF, rc)
    call ESMF_FieldGetArray(field3, array3, rc)
    call ESMF_ArrayGetData(array3, resdata, ESMF_DATA_REF, rc)

    ! initialize data arrays
    srcdata = 0.0
    resdata = 0.0

    ! set data array to a function of coordinates (just in the computational part
    ! of the array)
    do j    = 1,localCounts(2)
       do i = 1,localCounts(1)
           srcdata(i+hWidth,j+hWidth) = 10.0 + 5.0*sin(coordX(i,j)/60.0*pi) &
                                             + 2.0*sin(coordY(i,j)/50.0*pi) 
      enddo
    enddo

    print *, "Initial data, before Transpose:"
    call ESMF_ArrayPrint(array1, "foo", rc);
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! No deallocate() is needed for array data, it will be freed when the
    ! Array is destroyed. 

    print *, "Init section finished"
!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    !! Call transpose method here, output ends up in field2
    call ESMF_FieldRedist(field1, field2, rh12, rc=status)
    call ESMF_FieldGetArray(field2, array2, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after Transpose:"
    call ESMF_ArrayPrint(array2, "", rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    !! Transpose back so we can compare contents
    !! Call transpose method again here, output ends up in field3
    call ESMF_FieldRedist(field2, field3, rh23, rc=status)
    call ESMF_FieldGetArray(field3, array3, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after second Transpose, should match original:"
    call ESMF_ArrayPrint(array3, "", rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "Run section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Print result

#ifdef ESMF_ENABLE_VM
    call ESMF_newDELayoutGet(delayout1, localDe=deId, rc=rc)
#else
    call ESMF_DELayoutGetDEID(layout1, deId, rc)
#endif
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"
    print *, "Result from deId ", deId
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    match    = .true.
    miscount = 0
    do i     = 1,size(resdata,1)
      do j   = 1,size(resdata,2)
        if (srcdata(i,j) .ne. resdata(i,j)) then
          print *, "array contents do not match at: (", i,j, ") on DE ", &
                   deId, ".  src=", srcdata(i,j), "dst=", resdata(i,j)
          match = .false.
          miscount = miscount + 1
          if (miscount .gt. 40) then
            print *, "more than 40 mismatches, skipping rest of loop"
            goto 10
          endif
        endif
      enddo
    enddo
    if (match) print *, "Array contents matched correctly!! deId = ", deId
10  continue

    print *, "Finalize section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Clean up

    call ESMF_FieldRedistRelease(rh12, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldRedistRelease(rh23, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(field1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(field2, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_FieldDestroy(field3, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_GridDestroy(grid1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_GridDestroy(grid2, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_DELayoutDestroy(layout1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
20    print *, "System Test FieldRedist complete!"

    if ((deId .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
      write(failMsg, *)  "Transposed transpose not same as original"
      write(testname, *) "System Test FieldRedist: Field Transpose/Redistribute"

      call ESMF_Test((miscount.eq.0) .and. (rc.eq.ESMF_SUCCESS), &
                     testname, failMsg, testresult, ESMF_SRCLINE)

      ! Separate message to console, for quick confirmation of success/failure
      if ((miscount.eq.0) .and. (rc .eq. ESMF_SUCCESS)) then
        write(finalMsg, *) "SUCCESS!! Data transposed twice same as original"
      else
        write(finalMsg, *) "System Test did not succeed. ", &
        "Data transpose does not match expected values, or error code set ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif
    
    call ESMF_Finalize(rc)

    end program FieldRedist
    
!\end{verbatim}
    
