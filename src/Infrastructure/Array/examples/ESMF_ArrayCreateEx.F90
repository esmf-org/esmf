! $Id: ESMF_ArrayCreateEx.F90,v 1.3 2003/12/03 21:44:58 svasquez Exp $
!
! Example/test code which creates a new field.

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
    program ESMF_ArrayCreateEx
    
!   ! Other ESMF modules which are needed by Arrays
    use ESMF_IOMod
    use ESMF_Mod
    implicit none

!   ! Local variables
    integer :: nx, ny, arank, rc, finalrc 
    integer :: i, j, ni, nj
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array1, array2, array3
    real(selected_real_kind(6,45)), dimension(:,:), pointer :: realptr, realptr2
    integer(selected_int_kind(5)), dimension(:), pointer :: intptr, intptr2
    finalrc = ESMF_SUCCESS

    
!-------------------------------------------------------------------------------
!   ! Example 1:
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Integer, 1D.
 
!   ! Allocate and set initial data values
    ni = 15 
    allocate(intptr(ni))
    do i=1,ni
       intptr(i) = i
    enddo
    print *, "intptr data = ", intptr

    array1 = ESMF_ArrayCreate(intptr, ESMF_DATA_REF, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "array 1 create returned"

    call ESMF_ArrayPrint(array1, "foo", rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "array 1 print returned"

    call ESMF_ArrayGetData(array1, intptr2, ESMF_DATA_REF, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "array 1 getdata returned"
    print *, "intptr2 data = ", intptr2

    call ESMF_ArrayDestroy(array1)
    print *, "array 1 destroy returned"


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

    array2 = ESMF_ArrayCreate(realptr, ESMF_DATA_REF, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "array 2 create returned"

    call ESMF_ArrayPrint(array2, "foo", rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "array 2 print returned"

    call ESMF_ArrayGetData(array2, realptr2, ESMF_DATA_REF, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "array 2 getdata returned"
    print *, "realptr2 data = ", realptr2

    call ESMF_ArrayDestroy(array2)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    print *, "array 2 destroy returned"


!-------------------------------------------------------------------------------
!   ! Example 3:
!   !  Create based on an array specification.

    arank = 2
!   !   arrayspec = ESMF_ArraySpecCreate(arank, ESMF_DATA_REAL, ESMF_R4, &
    !                                         nx, ny, rc)

!   !   array2 = ESMF_ArrayCreate(arrayspec, ESMF_NO_DATA, rc)

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_ArrayCreateEx.F90"
    else
        print *, "FAIL: ESMF_ArrayCreateEx.F90"
    end if



     end program ESMF_ArrayCreateEx
    
!\end{verbatim}
    
