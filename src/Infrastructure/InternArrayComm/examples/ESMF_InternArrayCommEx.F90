! $Id: ESMF_InternArrayCommEx.F90,v 1.6.2.1 2007/12/14 20:25:32 svasquez Exp $
!
! Example code which shows how to use Array Communication routines

!-------------------------------------------------------------------------------
!ESMF_notworking_EXAMPLE        String used by test script to count examples.
!-------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to use the
!  Communications routines at the Array level.
!  Also see the Programming Model section of this document.
!
!EOP
!BOC

!   ! Example program showing various ways to use the Array communication 
!   !  level routines
    program ESMF_ArrayCommEx
    
!   ! The ESMF module
    use ESMF_Mod
    implicit none

!   ! Local variables
    integer :: rc
    integer :: half, quart
    integer :: bytwo, byfour
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array1, array2
    type(ESMF_IGrid) :: igrid1, igrid2
    type(ESMF_FieldDataMap) :: datamap
    type(ESMF_VM) :: vm
    type(ESMF_DELayout) :: delayout1, delayout2
    integer :: counts(ESMF_MAXIGRIDDIM), nPETs
    integer :: g1_cells(ESMF_MAXIGRIDDIM), g2_cells(ESMF_MAXIGRIDDIM)
    real (ESMF_KIND_R8) :: min(2), max(2)
    real (ESMF_KIND_R8), pointer :: f90ptr1(:,:), f90ptr2(:,:)
    type(ESMF_IGridHorzStagger) :: horz_stagger
    character(len=32) :: gname
!EOC

    integer :: finalrc 
    finalrc = ESMF_SUCCESS

!BOC
    ! Initialize the framework and get back the default Virtual Machine info
    call ESMF_Initialize(vm=vm, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!-------------------------------------------------------------------------------
!   Create the igrid, datamap, and array objects here
!   which will be needed for the examples below.

    call ESMF_VMGet(vm, petcount=nPETs, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    bytwo = 2
    half = nPETs / bytwo
    if (half .le. 0) then
       half = 1
       bytwo = 1
    endif
    byfour = 4
    quart = nPETs / byfour 
    if (quart .le. 0) then
       quart = 1
       byfour = 1
    endif

    ! Make a Nx4 and Nx2 layout
    delayout1 = ESMF_DELayoutCreate(vm, (/ quart, byfour /), rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    delayout2 = ESMF_DELayoutCreate(vm, (/ half, bytwo /), rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    !------------------------------------------------------------------------
    ! First igrid
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
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_IGridDistribute(igrid1, delayout=delayout1, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
   

    !------------------------------------------------------------------------
    ! Second igrid
    gname = "test igrid 2"
    horz_stagger = ESMF_IGRID_HORZ_STAGGER_D_NE
    igrid2 = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=gname, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_IGridDistribute(igrid2, delayout=delayout2, rc=rc)
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    !------------------------------------------------------------------------
    ! Array spec
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc) 
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! Array data
    call ESMF_IGridGetDELocalInfo(igrid1, localCellCountPerDim=g1_cells, &
                          horzRelloc=ESMF_CELL_CENTER)
    allocate(f90ptr1(g1_cells(1)+4, g1_cells(2)+4))
    f90ptr1 = 10
    array1 = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, haloWidth=2, rc=rc)

    ! Second array
    call ESMF_IGridGetDELocalInfo(igrid2, localCellCountPerDim=g2_cells, &
                          horzRelloc=ESMF_CELL_CENTER)
    allocate(f90ptr2(g2_cells(1)+4, g2_cells(2)+4))
    f90ptr2 = -1
    array2 = ESMF_ArrayCreate(f90ptr2, ESMF_DATA_REF, haloWidth=2, rc=rc)

    !------------------------------------------------------------------------
    ! FieldDataMap

    call ESMF_FieldDataMapSetDefault(datamap, ESMF_INDEX_IJ)

    !f1 = ESMF_FieldCreate(igrid1, arr1, ESMF_DATA_REF, ESMF_CELL_CENTER, &
    !                        ESMF_CELL_CELL, 1, dm, "Field 0", ios, rc)

!-------------------------------------------------------------------------------

#if 0
!BOC
!-------------------------------------------------------------------------------
!   ! Example 1:
!   !  Array Halo
 
!   ! Allocate and set initial data values
    ni = 15 
    allocate(intptr(ni))
    do i=1,ni
       intptr(i) = i
    enddo
    print *, "intptr data = ", intptr

    array1 = ESMF_ArrayCreate(intptr, ESMF_DATA_REF, rc=rc)

    print *, "array 1 create returned"
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
    call ESMF_ArrayPrint(array1, "foo", rc)

    print *, "array 1 print returned"
!EOC

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOC
    call ESMF_ArrayGetData(array1, intptr2, ESMF_DATA_REF, rc)

    print *, "array 1 getdata returned"

    print *, "intptr2 data = ", intptr2
!EOC

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOC
    call ESMF_ArrayDestroy(array1, rc)

    print *, "array 1 destroy returned"
!EOC

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOC
!-------------------------------------------------------------------------------
!   ! Example 2:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 2D.
 
!   ! Allocate and set initial data values
    ni = 5 
    nj = 3 
    allocate(realptr(ni,nj))
    do i=1,ni
     do j=1,nj
       realptr(i,j) = i + ((j-1)*ni) + 0.1
     enddo
    enddo
    print *, "realptr data = ", realptr

    array2 = ESMF_ArrayCreate(realptr, ESMF_DATA_REF, rc=rc)

    print *, "array 2 create returned"
!EOC

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOC
    call ESMF_ArrayPrint(array2, "foo", rc)

    print *, "array 2 print returned"
!EOC
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print *, "FAILED"
    end if

!BOC
    call ESMF_ArrayGetData(array2, realptr2, ESMF_DATA_REF, rc)

    print *, "array 2 getdata returned"

    print *, "realptr2 data = ", realptr2
!EOC

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOC
    call ESMF_ArrayDestroy(array2, rc)

    print *, "array 2 destroy returned"
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------------
!   ! Example 3:
!   !  Create based on an array specification.

    arank = 2
!   !   arrayspec = ESMF_ArraySpecCreate(arank, ESMF_TYPEKIND_R4, &
    !                                         nx, ny, rc)

!   !   array2 = ESMF_ArrayCreate(arrayspec, ESMF_NO_DATA, rc)
#endif

!BOC
    call ESMF_Finalize(rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_ArrayCommEx.F90"
    else
        print *, "FAIL: ESMF_ArrayCommEx.F90"
    end if
!BOC
    end program ESMF_ArrayCommEx
!EOC
    
