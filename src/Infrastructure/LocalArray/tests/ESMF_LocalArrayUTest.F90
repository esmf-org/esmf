! $Id: ESMF_LocalArrayUTest.F90,v 1.3 2003/10/20 20:13:56 cdeluca Exp $
!
! Example/test code which creates new arrays.

!-------------------------------------------------------------------------------
#include <ESMF_Macros.inc>
!-------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Arrays.  
!  Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing various ways to create a Array object
    program ESMF_LocalArrayTest
    
!   ! Other ESMF modules which are needed by Arrays
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_LocalArrayMod
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
    implicit none

!   ! Local variables
    integer :: nx, ny, arank, brank, rc 
    integer :: i, j, k, l, m, ni, nj, nk, nl, nm
    type(ESMF_ArraySpec) :: arrayspec, spec
    type(ESMF_DataType) :: atype
    type(ESMF_DataKind) :: akind
    integer :: counts(ESMF_MAXDIM)
    type(ESMF_LocalArray) :: array1, array2, array3, array4
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: real3dptr, real3d2ptr
    real(ESMF_KIND_R8), dimension(:,:), pointer :: realptr, realptr2
    integer(ESMF_KIND_I4), dimension(:), pointer :: intptr, intptr2
    integer(ESMF_KIND_I4), dimension(:,:), pointer :: int2Dptr
    character(ESMF_MAXSTR) :: filename


    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0

    
    call ESMF_Initialize()

!-------------------------------------------------------------------------------
!   ! Test 1:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Integer, 1D.
    print *, ">>> Test 1:"
 
!   ! Allocate and set initial data values
    ni = 15 
    allocate(intptr(ni))
    do i=1,ni
       intptr(i) = i
    enddo
    print *, "intptr data = ", intptr

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Local Array with Integer 1D Data Test"
    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 create returned"

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a Local Array with Integer 1D Data Test"
    call ESMF_LocalArrayPrint(array1, "foo", rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 print returned"

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array with Integer 1D Data Test"
    call ESMF_LocalArrayGetData(array1, intptr2, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    ni = 15 
    do i=1,ni
        if (intptr(i).eq.intptr2(i)) then
            result = 0
        else
            result = 1
            exit
        end if
    enddo
    write(failMsg, *) "Array data did not compare."
    write(name, *) "Compare Local Array Integer 1D Data Test"
    call ESMF_Test((result.eq.0), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 getdata returned"
    print *, "intptr2 data = ", intptr2

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    write(name, *) "Local Array Destroy Test"
    call ESMF_LocalArrayDestroy(array1)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 destroy returned"


!-------------------------------------------------------------------------------
!   ! Test 2:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Integer, 1D.
    print *, ">>> Test 2:"
 
 
!   ! Allocate and set initial data values
    ni = 35 
    allocate(intptr(ni))
    do i=1,ni
       intptr(i) = i
    enddo

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array Based on existing pointer Test"
    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 create returned"

    call ESMF_LocalArrayPrint(array1, "foo", rc)
    print *, "array 1 print returned"

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array Data Test"
    call ESMF_LocalArrayGetData(array1, intptr2, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    ni = 35
    do i=1,ni
        if (intptr(i).eq.intptr2(i)) then
                result = 0
        else
                result = 1
                exit
        end if
    enddo
    write(failMsg, *) "Array data did not compare."
    write(name, *) "Compare Local Array Data Integer 1D Test"
    call ESMF_Test((result.eq.0), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    write(name, *) "Local Array Destroy Test"
    call ESMF_LocalArrayDestroy(array1)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    write(name, *) "Local Array Destroy a destroyed Array Test"
    call ESMF_LocalArrayDestroy(array1)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 destroy returned"


!-------------------------------------------------------------------------------
!   ! Test 2a:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Integer, 1D.
    print *, ">>> Test 2a:"
 
 
!   ! Allocate and set initial data values
    ni = 22 
    allocate(intptr(ni))
    do i=1,ni
       intptr(i) = i
    enddo

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array Based on allocated pointer Test"
    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 create returned"

    call ESMF_LocalArrayPrint(array1, "foo", rc)
    print *, "array 1 print returned"

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array with Integer 1D Data Test"
    call ESMF_LocalArrayGetData(array1, intptr2, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    ni = 22
    do i=1,ni
        if (intptr(i).eq.intptr2(i)) then
                result = 0
        else
                result = 1
                exit
        end if
    enddo
    write(failMsg, *) "Array data did not compare."
    write(name, *) "Compare Local Array Data Integer 1D Test"
    call ESMF_Test((result.eq.0), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_LocalArrayDestroy(array1)
    print *, "array 1 destroy returned"


!-------------------------------------------------------------------------------
!   ! Test 3:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 2D.
    print *, ">>> Test 3:"
 
 
!   ! Allocate and set initial data values
    ni = 5 
    nj = 3 
    allocate(realptr(ni,nj))
    do i=1,ni
     do j=1,nj
       realptr(i,j) = i + ((j-1)*ni) + 0.1
     enddo
    enddo
    print *, "partial print of realptr data = ", realptr(1:3,1:3)

   !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array with 2D Real Data Test"
    array2 = ESMF_LocalArrayCreate(realptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 2 create returned"

    call ESMF_LocalArrayPrint(array2, "foo", rc)
    print *, "array 2 print returned"

    do i=1,ni
     do j=1,nj
       realptr(i,j) = (i*2) + ((j-1)*ni) 
     enddo
    enddo
    print *, "realptr data changed after nocopy set, now = ", realptr

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 2D Real Data Test"
    call ESMF_LocalArrayGetData(array2, realptr2, ESMF_DATA_REF, rc)
    print *, "array 2 getdata returned"
    print *, "partial print of realptr2 data = ", realptr2(1:3,1:3)

   !NEX_UTest
    do i=1,ni
     do j=1,nj
        if (realptr(i,j).eq.realptr2(i,j)) then
                result = 0
        else
                result = 1
                exit
        end if
     enddo
    enddo
    write(failMsg, *) "Array data did not compare."
    write(name, *) "Compare Local Array 2D Real Data Test"
    call ESMF_Test((result.eq.0), name, failMsg, result, ESMF_SRCLINE)



    call ESMF_LocalArrayDestroy(array2)
    print *, "array 2 destroy returned"


!-------------------------------------------------------------------------------
!   ! Test 4:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 2D.
    print *, ">>> Test 4:"
 
 
!   ! Allocate and set initial data values
    ni = 15 
    nj = 13 
    allocate(realptr(ni,nj))
    do i=1,ni
     do j=1,nj
       realptr(i,j) = i + ((j-1)*ni) + 0.1
     enddo
    enddo

   !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array with 2D Real Data based on an existing F90 pointer Test"
    array2 = ESMF_LocalArrayCreate(realptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 2 create returned"

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 2D Real Data Test"
    call ESMF_LocalArrayGetData(array2, realptr2, ESMF_DATA_REF, rc)
    print *, "array 2 getdata returned"
    print *, "partial print of realptr2 data = ", realptr2(1:3,1:3)

   !NEX_UTest
    do i=1,ni
     do j=1,nj
        if (realptr(i,j).eq.realptr2(i,j)) then
                result = 0
        else
                result = 1
                exit
        end if
     enddo
    enddo
    write(failMsg, *) "Array data did not compare."
    write(name, *) "Compare Local Array 2D Real Data Test"
    call ESMF_Test((result.eq.0), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_LocalArrayPrint(array2, "foo", rc)
    print *, "array 2 print returned"

    call ESMF_LocalArrayDestroy(array2)
    print *, "array 2 destroy returned"


!-------------------------------------------------------------------------------
!   ! Test 5:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 2D.  DATA_COPY set
    print *, ">>> Test 5:"
 
 
!   ! Allocate and set initial data values
    ni = 15 
    nj = 13 
    allocate(realptr(ni,nj))
    do i=1,ni
     do j=1,nj
       realptr(i,j) = i + ((j-1)*ni) + 0.1
     enddo
    enddo

   !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array with 2D Real Data based on an existing F90 pointer Test"
    array2 = ESMF_LocalArrayCreate(realptr, ESMF_DATA_COPY, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 2 create returned"

    call ESMF_LocalArrayPrint(array2, "foo", rc)
    print *, "array 2 print returned"

    do i=1,ni
     do j=1,nj
       realptr(i,j) = (i*2) + ((j-1)*ni) 
     enddo
    enddo
    print *, "realptr data changed after docopy set, now = ", realptr(1:3,1:3)

    call ESMF_LocalArrayGetData(array2, realptr2, ESMF_DATA_REF, rc)
    print *, "array 2 getdata returned"
    print *, "realptr2 data = ", realptr2(1:3,1:3)

   !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 2D Real Data Test"
    call ESMF_LocalArrayGetData(array2, realptr2, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 2 getdata returned"
    print *, "realptr2 data = ", realptr2(1:3,1:3)

   !NEX_UTest
    do i=1,ni
     do j=1,nj
        if (realptr(i,j).eq.realptr2(i,j)) then
                result = 0
        else
                result = 1
                exit
        end if
     enddo
    enddo
    write(failMsg, *) "Array data should not be the same."
    write(name, *) "Compare Local Array 2D Real Data Test"
    call ESMF_Test((result.eq.1), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_LocalArrayDestroy(array2)
    print *, "array 2 destroy returned"


!-------------------------------------------------------------------------------
!   ! Test 6:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 3D.  DATA_COPY set
    print *, ">>> Test 6:"
 
 
!   ! Allocate and set initial data values
    ni = 15 
    nj = 13 
    nk = 9
    allocate(real3dptr(ni,nj,nk))
    do i=1,ni
     do j=1,nj
       do k=1,nk
        real3dptr(i,j,k) = i + ((j-1)*ni) + ((k-1)*ni*nj) + 0.1
       enddo
     enddo
    enddo

   !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Local Array 3D DATA_COPY Real Data Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_COPY, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    print *, "array 4 create returned"

#ifdef ESMF_EXHAUSTIVE
    ! The following test is commented out because it crashes.
    ! Bug report 790766 has been filed. When the bug report is
    ! closed this code will be uncommented.
    !write(failMsg, *) "Did not return ESMF_SUCCESS"
    !write(name, *) "Getting Local Array 3D Real Data with wrong dimension array Test"
    !call ESMF_LocalArrayGetData(array4, realptr2, ESMF_DATA_COPY, rc)
    !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

   !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 3D Real Data without allocating array size Test"
    call ESMF_LocalArrayGetData(array4, real3d2ptr, ESMF_DATA_COPY, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 4 getdata returned"

    ni = 15
    nj = 13
    nk = 9
    do i=1,ni
     do j=1,nj
       do k=1,nk
        if (real3dptr(i,j,k).eq.real3d2ptr(i,j,k)) then
                result = 0
        else
                result = 1
                exit
        end if
       enddo
     enddo
    enddo

   !EX_UTest
    write(failMsg, *) "Array data should be the same."
    write(name, *) "Compare Local Array 3D Real Data without allocating array size Test"
    call ESMF_Test((result.eq.0), name, failMsg, result, ESMF_SRCLINE)

#endif

   !NEX_UTest
    allocate(real3d2ptr(ni,nj,nk))
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 3D Real Data Test"
    call ESMF_LocalArrayGetData(array4, real3d2ptr, ESMF_DATA_COPY, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 4 getdata returned"

    ni = 15
    nj = 13
    nk = 9
    do i=1,ni
     do j=1,nj
       do k=1,nk
        if (real3dptr(i,j,k).eq.real3d2ptr(i,j,k)) then
                result = 0
        else
                result = 1
                exit
        end if
       enddo
     enddo
    enddo

   !NEX_UTest
    write(failMsg, *) "Array data should be the same."
    write(name, *) "Compare Local Array 3D Real Data Test"
    call ESMF_Test((result.eq.0), name, failMsg, result, ESMF_SRCLINE)

    ! with do copy, the original can go now
    deallocate(real3dptr)

    call ESMF_LocalArrayPrint(array4, "foo", rc)
    print *, "array 4 print returned"

    call ESMF_LocalArrayDestroy(array4)
    print *, "array 4 destroy returned"


 
 
!   ! Allocate and free different sizes testing end of array printing code
    ni = 3 
    nj = 7 
    nk = 60
    allocate(real3dptr(ni,nj,nk))

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Local Array 3D DATA_COPY Real Data Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    print *, "array 4 create returned"

    call ESMF_LocalArrayPrint(array4, "foo", rc)
    print *, "array 4 print returned"

    ! this deletes the space
    call ESMF_LocalArrayDestroy(array4)
    print *, "array 4 destroy returned"

!   ! Allocate and free different sizes testing end of array printing code
    ni = 10 
    nj = 3 
    nk = 40

#ifdef ESMF_EXHAUSTIVE

    deallocate(real3dptr)

    !EX_UTest
    write(failMsg, *) "Did not return ESMF_FAILURE"
    write(name, *) "Creating a Local Array 3D DATA_COPY Real Data with deallocated array Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)

    print *, "array 4 create returned"

    !This print of array4 is commented out becauses it crashes
    !Bug report 791282 has been filed.
    !This print statement will be uncommented when the bug is fixed
    ! this deletes the space
    !call ESMF_LocalArrayPrint(array4, "foo", rc)
    print *, "array 4 print returned"

    !This test is commented out becauses it crashes
    !Bug report 791282 has been filed.
    !This test will be uncommented when the bug is fixed
    ! this deletes the space
    !EX_UTest
    !write(failMsg, *) "Did not return ESMF_SUCCESS"
    !write(name, *) "Destroying a Local Array 3D DATA_COPY Real Data with deallocated array Test"
    !call ESMF_LocalArrayDestroy(array4)
    !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !print *, "array 4 destroy returned"

#endif

    !NEX_UTest
    allocate(real3dptr(ni,nj,nk))
    write(failMsg, *) "Did not return ESMF_FAILURE"
    write(name, *) "Creating a Local Array 3D DATA_COPY Real Data with an allocated array Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    print *, "array 4 create returned"

    call ESMF_LocalArrayPrint(array4, "foo", rc)
    print *, "array 4 print returned"

    ! this deletes the space
    call ESMF_LocalArrayDestroy(array4)
    print *, "array 4 destroy returned"

!   ! Allocate and free different sizes testing end of array printing code
    ni = 11 
    nj = 3 
    nk = 40
    allocate(real3dptr(ni,nj,nk))

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Creating a Local Array 3D DATA_COPY Real Data with an allocated array Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 4 create returned"

    call ESMF_LocalArrayPrint(array4, "foo", rc)
    print *, "array 4 print returned"

    ! this deletes the space
    call ESMF_LocalArrayDestroy(array4)
    print *, "array 4 destroy returned"


!-------------------------------------------------------------------------------
!   ! Test 7:
!   !  Create based on an array specification.
    ! print *, ">>> Test 7:"
 

    !NEX_UTest
    arank = 2
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Initializing an Array Spec of rank 2 Test"
    call ESMF_LocalArraySpecInit(arrayspec, arank, ESMF_DATA_REAL, ESMF_R4, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! set all counts to 1 first, then alter the ones you want to change
    counts = 1
    counts(1) = 10
    counts(2) = 20

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Creating an Array Spec Test"
    array2 = ESMF_LocalArrayCreate(arrayspec, counts(1:2), rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !NEX_UTest
    write(failMsg, *) "rank not correct" 
    write(name, *) "Get Spec rank and verify Test"
    call ESMF_LocalArraySpecGet(arrayspec, rank=brank, rc=rc)
    call ESMF_Test((arank.eq.brank), name, failMsg, result, ESMF_SRCLINE)


    !NEX_UTest
    write(failMsg, *) "type not correct" 
    write(name, *) "Get Spec type and verify Test"
    call ESMF_LocalArraySpecGet(arrayspec, type=atype, rc=rc)
    call ESMF_Test((atype.eq.ESMF_DATA_REAL), name, failMsg, result, ESMF_SRCLINE)

    !NEX_UTest
    write(failMsg, *) "kind not correct" 
    write(name, *) "Get Spec kind and verify Test"
    call ESMF_LocalArraySpecGet(arrayspec, kind=akind, rc=rc)
    call ESMF_Test((akind.eq.ESMF_R4), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

    !EX_UTest
    arank = 10
    write(failMsg, *) "Did not return ESMF_FAILURE"
    write(name, *) "Initializing an Array Spec of rank 10 Test"
    call ESMF_LocalArraySpecInit(arrayspec, arank, ESMF_DATA_REAL, ESMF_R4, rc)
    call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)



    !EX_UTest
    write(failMsg, *) "Did not return ESMF_FAILURE"
    write(name, *) "Creating an Array from a Spec with rank of 10 Test"
    array2 = ESMF_LocalArrayCreate(arrayspec, counts, rc)
    call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    arank = 5
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Initializing an Array Spec of rank 5 Test"
    call ESMF_LocalArraySpecInit(arrayspec, arank, ESMF_DATA_REAL, ESMF_R4, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating an Array from a Spec with rank of 5 Test"
    array2 = ESMF_LocalArrayCreate(arrayspec, counts, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

    !NEX_UTest
    arank = 4
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Initializing an Array Spec of rank 4 Test"
    call ESMF_LocalArraySpecInit(arrayspec, arank, ESMF_DATA_REAL, ESMF_R4, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)



    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating an Array from a Spec with rank of 4 Test"
    array2 = ESMF_LocalArrayCreate(arrayspec, counts, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------------
!   ! Test 8:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Integer, 1D.
    print *, ">>> Test 8:"
 
 
!   ! Allocate and set initial data values
    ni = 35 
    nj = 40
    allocate(int2Dptr(ni,nj))
    do i=1,ni
      do j=1,nj
       int2Dptr(i, j) = i + j
      enddo
    enddo

    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Initializing an Array with data type integer Test"
    array1 = ESMF_LocalArrayCreate(int2Dptr, ESMF_DATA_REF, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 create returned"

    call ESMF_LocalArrayWrite(array1, filename="./foo", rc=rc)
    print *, "array 1 write returned"

    call ESMF_LocalArrayDestroy(array1)
    print *, "array 1 destroy returned"

!-------------------------------------------------------------------------------

    call ESMF_Finalize()


    end program ESMF_LocalArrayTest
    
!\end{verbatim}
    
