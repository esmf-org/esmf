! $Id: ESMF_InternArrayGetEx.F90,v 1.7.2.3 2009/01/21 21:25:21 cdeluca Exp $
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

    program ESMF_ArrayGetEx

!-------------------------------------------------------------------------------
!ESMF_notworking_EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_ArrayGetEx - Examples of Array Usage
!
! !DESCRIPTION:
!
! This program shows examples of Array creation


    ! ESMF Framework module
    use ESMF_Mod
    implicit none

    ! Local variables
    integer :: arank, rc
    integer :: i, j, ni, nj
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_TypeKind) :: akind
    type(ESMF_Array) :: array1, array2
    real(selected_real_kind(6,45)), dimension(:,:), pointer :: realptr, realptr2
    integer(selected_int_kind(5)), dimension(:), pointer :: intptr, intptr2
!EOC

    integer :: finalrc 
    finalrc = ESMF_SUCCESS

    call ESMF_Initialize(rc=rc)

    ! Allocate and set initial data values
    ni = 15 
    allocate(intptr(ni))
    do i=1,ni
       intptr(i) = i
    enddo

    array1 = ESMF_ArrayCreate(intptr, ESMF_DATA_REF, rc=rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOE
!\subsubsection{Print Array Contents}
    
!  Print the data contents of an {\tt ESMF\_Array}.
!EOE

!BOC
    call ESMF_ArrayPrint(array1, rc=rc)
!EOC

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOE
!\subsubsection{Get a Pointer to the Array Contents}
    
!  Associate a Fortran pointer with the data from an {\tt ESMF\_Array}.
!  Point directly at the data contents; do not make a separate copy.
!EOE

!BOC
    call ESMF_ArrayGetData(array1, intptr2, ESMF_DATA_REF, rc)
!EOC

    print *, "array 1 getdata returned"

    print *, "intptr2 data = ", intptr2

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOE
!\subsubsection{Destroy an Array}
    
!  When finished with an {\tt ESMF\_Array}, remove the object
!  and release any resources associated with it.
!EOE

!BOC
    call ESMF_ArrayDestroy(array1, rc)
!EOC

    print *, "array 1 destroy returned"

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    ! Allocate and set initial data values
    ni = 5 
    nj = 3 
    allocate(realptr(ni,nj))
    do i=1,ni
     do j=1,nj
       realptr(i,j) = i + ((j-1)*ni) + 0.1
     enddo
    enddo
    print *, "realptr data = ", realptr

    array2 = ESMF_ArrayCreate(realptr, ESMF_DATA_COPY, rc=rc)

    print *, "array 2 create returned"

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_ArrayPrint(array2, rc=rc)

    print *, "array 2 print returned"

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
        print *, "FAILED"
    end if

!BOE
!\subsubsection{Get a Pointer to a Copy of the Array Contents}
    
!  Associate a Fortran pointer with the data from an {\tt ESMF\_Array}.
!  Allocate and copy the existing data into a separate buffer and return
!  that space.  It can be manipulated independently from the 
!  {\tt ESMF\_Array} contents.  It must be deallocated by the user when
!  no longer needed.
!EOE

!BOC
    call ESMF_ArrayGetData(array2, realptr2, ESMF_DATA_COPY, rc)
!EOC

    print *, "array 2 getdata returned"

    print *, "realptr2 data = ", realptr2

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_ArrayDestroy(array2, rc)

    print *, "array 2 destroy returned"

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    arank = 2
    akind = ESMF_TYPEKIND_R8

    call ESMF_ArraySpecSet(arrayspec, arank, akind)
    array2 = ESMF_ArrayCreate(arrayspec, (/10, 20 /), rc=rc)

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_ArrayGetEx.F90"
    else
        print *, "FAIL: ESMF_ArrayGetEx.F90"
    end if
!BOC
    call ESMF_Finalize(rc=rc)

    end program ESMF_ArrayGetEx
!EOC
    
