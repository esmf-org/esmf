! $Id: ESMF_ArrayCreateEx.F90,v 1.8 2004/01/09 23:55:10 svasquez Exp $
!
! Example/test code which creates a new field.

!-------------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
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
    integer :: nx, ny, arank, rc
    integer :: i, j, ni, nj
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array1, array2, array3
    real(selected_real_kind(6,45)), dimension(:,:), pointer :: realptr, realptr2
    integer(selected_int_kind(5)), dimension(:), pointer :: intptr, intptr2
!\end{verbatim}
!EOP

    integer :: finalrc 
    finalrc = ESMF_SUCCESS

!BOP
!\begin{verbatim}
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

    print *, "array 1 create returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_ArrayPrint(array1, "foo", rc)

    print *, "array 1 print returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_ArrayGetData(array1, intptr2, ESMF_DATA_REF, rc)

    print *, "array 1 getdata returned"

    print *, "intptr2 data = ", intptr2
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_ArrayDestroy(array1, rc)

    print *, "array 1 destroy returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
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

    print *, "array 2 create returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_ArrayPrint(array2, "foo", rc)

    print *, "array 2 print returned"
!\end{verbatim}
!EOP
    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print *, "FAILED"
    end if

!BOP
!\begin{verbatim}
    call ESMF_ArrayGetData(array2, realptr2, ESMF_DATA_REF, rc)

    print *, "array 2 getdata returned"

    print *, "realptr2 data = ", realptr2
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
    call ESMF_ArrayDestroy(array2, rc)

    print *, "array 2 destroy returned"
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

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
!BOP
!\begin{verbatim}
    end program ESMF_ArrayCreateEx
!\end{verbatim}
!EOP
    
