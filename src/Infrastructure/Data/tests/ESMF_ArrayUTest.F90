! $Id: ESMF_ArrayUTest.F90,v 1.4 2003/07/17 20:13:16 nscollins Exp $
!
! Example/test code which creates new arrays.

!-------------------------------------------------------------------------------
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
    implicit none

!   ! Local variables
    integer :: nx, ny, arank, rc 
    integer :: i, j, k, l, m, ni, nj, nk, nl, nm
    type(ESMF_ArraySpec) :: arrayspec
    integer :: counts(ESMF_MAXDIM)
    type(ESMF_LocalArray) :: array1, array2, array3, array4
    real(ESMF_IKIND_R8), dimension(:,:,:), pointer :: real3dptr
    real(ESMF_IKIND_R8), dimension(:,:), pointer :: realptr, realptr2
    integer(ESMF_IKIND_I4), dimension(:), pointer :: intptr, intptr2
    integer(ESMF_IKIND_I4), dimension(:,:), pointer :: int2Dptr
    character(ESMF_MAXSTR) :: filename

    
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

    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc)
    print *, "array 1 create returned"

    call ESMF_LocalArrayPrint(array1, "foo", rc)
    print *, "array 1 print returned"

    call ESMF_LocalArrayGetData(array1, intptr2, ESMF_DATA_REF, rc)
    print *, "array 1 getdata returned"
    print *, "intptr2 data = ", intptr2

    call ESMF_LocalArrayDestroy(array1)
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

    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc)
    print *, "array 1 create returned"

    call ESMF_LocalArrayPrint(array1, "foo", rc)
    print *, "array 1 print returned"

    call ESMF_LocalArrayDestroy(array1)
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

    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc)
    print *, "array 1 create returned"

    call ESMF_LocalArrayPrint(array1, "foo", rc)
    print *, "array 1 print returned"

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

    array2 = ESMF_LocalArrayCreate(realptr, ESMF_DATA_REF, rc)
    print *, "array 2 create returned"

    call ESMF_LocalArrayPrint(array2, "foo", rc)
    print *, "array 2 print returned"

    do i=1,ni
     do j=1,nj
       realptr(i,j) = (i*2) + ((j-1)*ni) 
     enddo
    enddo
    print *, "realptr data changed after nocopy set, now = ", realptr

    call ESMF_LocalArrayGetData(array2, realptr2, ESMF_DATA_REF, rc)
    print *, "array 2 getdata returned"
    print *, "partial print of realptr2 data = ", realptr2(1:3,1:3)

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

    array2 = ESMF_LocalArrayCreate(realptr, ESMF_DATA_REF, rc)
    print *, "array 2 create returned"

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

    array2 = ESMF_LocalArrayCreate(realptr, ESMF_DATA_COPY, rc)
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

    call ESMF_LocalArrayDestroy(array2)
    print *, "array 2 destroy returned"


!-------------------------------------------------------------------------------
!   ! Test 6:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 2D.  DATA_COPY set
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

    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_COPY, rc)
    print *, "array 4 create returned"

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

    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc)
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
    allocate(real3dptr(ni,nj,nk))

    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc)
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

    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc)
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
 

    arank = 2
    call ESMF_LocalArraySpecInit(arrayspec, arank, ESMF_DATA_REAL, ESMF_KIND_R4, rc)

    counts(1) = 10
    counts(2) = 20

    array2 = ESMF_LocalArrayCreate(arrayspec, counts(1:2), rc)


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

    array1 = ESMF_LocalArrayCreate(int2Dptr, ESMF_DATA_REF, rc)
    print *, "array 1 create returned"

    call ESMF_LocalArrayWrite(array1, filename="./foo", rc=rc)
    print *, "array 1 write returned"

    call ESMF_LocalArrayDestroy(array1)
    print *, "array 1 destroy returned"

!-------------------------------------------------------------------------------

    end program ESMF_LocalArrayTest
    
!\end{verbatim}
    
