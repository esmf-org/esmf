! $Id: ESMF_LocalArrayUTest.F90,v 1.46.2.7 2008/11/07 23:52:08 theurich Exp $
!
! Example/test code which creates new arrays.

!-------------------------------------------------------------------------------
#include "ESMF.h"
#include "ESMF_Macros.inc"
!-------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Arrays.  
!  Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

    ! Example program showing various ways to create a Array object
    program ESMF_LocalArrayTest
    
    ! Other ESMF modules which are needed by Arrays
    use ESMF_BaseMod
    use ESMF_IOSpecMod
    use ESMF_ArraySpecMod
    use ESMF_LocalArrayMod
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
    implicit none

    ! Local variables
    integer :: arank, brank, rc 
    integer :: i, j, k, ni, nj, nk
    type(ESMF_ArraySpec) :: arrayspec, arrayspec1
    type(ESMF_TypeKind) :: akind
    integer :: counts(ESMF_MAXDIM), lb(1), ub(1), rlb(1), rub(1)
    type(ESMF_LocalArray) :: array1, array2, array4, arrayCpy
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: real3dptr, real3d2ptr
    real(ESMF_KIND_R8), dimension(:,:), pointer :: realptr, realptr2
    integer(ESMF_KIND_I4), dimension(:), pointer :: intptr, intptr2
    integer(ESMF_KIND_I4), dimension(:,:), pointer :: int2Dptr

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0

    
    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

!------------------------------------------------------------------------------
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Integer, 1D.
 
    !--------------------------------------------------------------------------
    !NEX_UTest
    ! Allocate and set initial data values, using a lower bound != 1
    ni = 515 
    allocate(intptr(5:ni+5))
    do i=5,ni+5
       intptr(i) = 11*i
    enddo
    print *, "first 10 intptr data = ", intptr(5:15)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Local Array with Integer 1D Data Test"
    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    write(name, *) "Local Array Destroy Test"
    call ESMF_LocalArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------------
 
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray from TKR without counts, lbounds, ubounds"
    write(failMsg, *) "Did return ESMF_SUCCESS"
    array1 = ESMF_LocalArrayCreate(rank=1, typekind=ESMF_TYPEKIND_I4, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray from TKR with counts and without lbounds, ubounds"
    write(failMsg, *) "Did return ESMF_SUCCESS"
    counts(1) = 10
    array1 = ESMF_LocalArrayCreate(rank=1, typekind=ESMF_TYPEKIND_I4, &
      counts=counts, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Local Array Destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    call ESMF_LocalArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray from TKR with counts and lbounds and without ubounds"
    write(failMsg, *) "Did return ESMF_SUCCESS"
    counts(1) = 10
    lb(1) = -5
    array1 = ESMF_LocalArrayCreate(rank=1, typekind=ESMF_TYPEKIND_I4, &
      counts=counts, lbounds=lb, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Local Array Destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    call ESMF_LocalArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray from TKR with counts and ubounds and without lbounds"
    write(failMsg, *) "Did return ESMF_SUCCESS"
    counts(1) = 10
    ub(1) = 20
    array1 = ESMF_LocalArrayCreate(rank=1, typekind=ESMF_TYPEKIND_I4, &
      counts=counts, ubounds=ub, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Local Array Destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    call ESMF_LocalArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray from TKR with matching counts, lbounds and ubounds"
    write(failMsg, *) "Did return ESMF_SUCCESS"
    counts(1) = 10
    lb(1) = -5
    ub(1) = 4
    array1 = ESMF_LocalArrayCreate(rank=1, typekind=ESMF_TYPEKIND_I4, &
      counts=counts, lbounds=lb, ubounds=ub, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Local Array Destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    call ESMF_LocalArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray from TKR with mis-matching counts, lbounds and ubounds"
    write(failMsg, *) "Did return ESMF_SUCCESS"
    counts(1) = 9
    lb(1) = -5
    ub(1) = 4
    array1 = ESMF_LocalArrayCreate(rank=1, typekind=ESMF_TYPEKIND_I4, &
      counts=counts, lbounds=lb, ubounds=ub, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray from TKR with rank that does not match bounds"
    write(failMsg, *) "Did return ESMF_SUCCESS"
    counts(1) = 10
    lb(1) = -5
    ub(1) = 4
    array1 = ESMF_LocalArrayCreate(rank=2, typekind=ESMF_TYPEKIND_I4, &
      counts=counts, lbounds=lb, ubounds=ub, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray from TKR without counts"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    lb(1) = -5
    ub(1) = 4
    array1 = ESMF_LocalArrayCreate(rank=1, typekind=ESMF_TYPEKIND_I4, &
      lbounds=lb, ubounds=ub, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray as a Copy of existing LocalArray"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    arrayCpy = ESMF_LocalArrayCreate(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Local Array Destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    call ESMF_LocalArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Printing LocalArray Copy after original LocalArray destroy"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_LocalArrayPrint(arrayCpy, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Local Array Copy Destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    call ESMF_LocalArrayDestroy(arrayCpy, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
    !--------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Creating a LocalArray from unassociated Fortran pointer with docpy argument"
    write(failMsg, *) "Did return ESMF_SUCCESS"
    nullify(real3dptr) ! make sure real3dptr is unassociated
    array1 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
   
#ifdef ESMF_TESTEXHAUSTIVE

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Returned ESMF_SUCCESS for uninitialized ArraySpec"
    write(name, *) "Validate a non-set ArraySpec"
    call ESMF_ArraySpecValidate(arrayspec1, rc)
    call ESMF_Test((rc .ne. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !EX_UTest
    arank = 3
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Initializing an Array Spec of rank 3 Test"
    call ESMF_ArraySpecSet(arrayspec1, arank, ESMF_TYPEKIND_R4, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Validate an ArraySpec"
    call ESMF_ArraySpecValidate(arrayspec1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Local Array with Integer 1D Data Test"
    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1b create returned"

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a Local Array with Integer 1D Data Test"
    call ESMF_LocalArrayPrint(array1, "", rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1b print returned"

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array with Integer 1D Data Test"
    call ESMF_LocalArrayGet(array1, intptr2, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1b get returned"

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting lower and upper index bounds"
    call ESMF_LocalArrayGet(array1, intptr2, ESMF_DATA_REF, rc=rc)
    call ESMF_LocalArrayGet(array1, lbounds=lb, ubounds=ub, rc=rc)  
    rlb = lbound(intptr2)
    rub = ubound(intptr2)
    print *, "real lb, ub = ", rlb(1), rub(1), "  lib return lb, ub = ", lb(1), ub(1)
    call ESMF_Test((rlb(1).eq.lb(1)).and.(rub(1).eq.ub(1)), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1b getbounds returned"

    !--------------------------------------------------------------------------
    !EX_UTest
    do i=5,ni+5
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
    print *, "array 1b get returned"
    print *, "first 10 intptr2 data = ", intptr2(5:15)

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    write(name, *) "Local Array Destroy Test"
    call ESMF_LocalArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1b destroy returned"
    deallocate(intptr)


!-------------------------------------------------------------------------------
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Integer, 1D.
 
    ! Allocate and set initial data values
    ni = 835 
    allocate(intptr(ni))
    do i=1,ni
       intptr(i) = i
    enddo

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array Based on existing pointer Test"
    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1c create returned"

    call ESMF_LocalArrayPrint(array1, "", rc=rc)
    print *, "array 1c print returned"

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array Data Test"
    call ESMF_LocalArrayGet(array1, intptr2, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
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

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS."
    write(name, *) "Local Array Destroy Test"
    call ESMF_LocalArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1c destroy returned"

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Returned ESMF_SUCCESS incorrectly."
    write(name, *) "Local Array Destroy a destroyed Array Test"
    call ESMF_LocalArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1c re-destroy returned"

    deallocate(intptr)

!-------------------------------------------------------------------------------
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Integer, 1D.
 
    ! Allocate and set initial data values
    ni = 1022 
    allocate(intptr(ni))
    do i=1,ni
       intptr(i) = i
    enddo

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array Based on allocated pointer Test"
    array1 = ESMF_LocalArrayCreate(intptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1d create returned"

    call ESMF_LocalArrayPrint(array1, "", rc=rc)
    print *, "array 1d print returned"

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array with Integer 1D Data Test"
    call ESMF_LocalArrayGet(array1, intptr2, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !EX_UTest
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

    call ESMF_LocalArrayDestroy(array1, rc=rc)
    print *, "array 1d destroy returned"


!-------------------------------------------------------------------------------
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 2D.
 
    ! Allocate and set initial data values
    ni = 50
    nj = 30
    allocate(realptr(3:ni+3,7:nj+7))
    do i=3,ni+3
     do j=7,nj+7
       realptr(i,j) = i + ((j-1)*ni) + 0.1
     enddo
    enddo
    print *, "partial print of realptr data = ", realptr(3:6,7:9)

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array with 2D Real Data Test"
    print *, "Creating array2"
    array2 = ESMF_LocalArrayCreate(realptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 2a create returned"
    !print *, "array 2a create BYPASSED"

    call ESMF_LocalArrayPrint(array2, "", rc=rc)
    print *, "array 2a print returned"

    do i=3,ni+3
     do j=7,nj+7
       realptr(i,j) = (i*2) + ((j-1)*ni) 
     enddo
    enddo
    print *, "realptr data changed after nocopy set, now = ", realptr(3:6,7:9)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 2D Real Data Test"
    call ESMF_LocalArrayGet(array2, realptr2, ESMF_DATA_REF, rc=rc)
    print *, "array 2a get returned"
    print *, "bounds: ", lbound(realptr2), ubound(realptr2)
    print *, "partial print of realptr2 data = ", realptr2(3:7,7:9)

    !--------------------------------------------------------------------------
    !EX_UTest
    do i=3,ni+3
     do j=7,nj+7
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

    call ESMF_LocalArrayDestroy(array2, rc=rc)
    print *, "array 2a destroy returned"
    deallocate(realptr)

!-------------------------------------------------------------------------------
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 2D.
 
    ! Allocate and set initial data values
    ni = 15 
    nj = 13 
    allocate(realptr(ni,nj))
    do i=1,ni
     do j=1,nj
       realptr(i,j) = i + ((j-1)*ni) + 0.1
     enddo
    enddo

    !--------------------------------------------------------------------------
   !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array with 2D Real Data based on an existing F90 pointer Test"
    array2 = ESMF_LocalArrayCreate(realptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 2b create returned"

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 2D Real Data Test"
    call ESMF_LocalArrayGet(array2, realptr2, ESMF_DATA_REF, rc=rc)
    print *, "array 2b get returned"
    print *, "partial print of realptr2 data = ", realptr2(1:3,1:3)

    !--------------------------------------------------------------------------
    !EX_UTest
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

    call ESMF_LocalArrayPrint(array2, "", rc=rc)
    print *, "array 2b print returned"

    call ESMF_LocalArrayDestroy(array2, rc=rc)
    print *, "array 2b destroy returned"
    deallocate(realptr)


!-------------------------------------------------------------------------------
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 2D.  DATA_COPY set
 
    ! Allocate and set initial data values
    ni = 4015 
    nj = 13 
    allocate(realptr(ni,nj))
    do i=1,ni
     do j=1,nj
       realptr(i,j) = i + ((j-1)*ni) + 0.1
     enddo
    enddo

    !--------------------------------------------------------------------------
   !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating Local Array with 2D Real Data based on an existing F90 pointer Test"
    array2 = ESMF_LocalArrayCreate(realptr, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 2c create returned"

    call ESMF_LocalArrayPrint(array2, "", rc=rc)
    print *, "array 2c print returned"

    do i=1,ni
     do j=1,nj
       realptr(i,j) = (i*2) + ((j-1)*ni) 
     enddo
    enddo
    print *, "realptr data changed after docopy set, now = ", realptr(1:3,1:3)

    call ESMF_LocalArrayGet(array2, realptr2, ESMF_DATA_REF, rc=rc)
    print *, "array 2c get returned"
    print *, "realptr2 data = ", realptr2(1:3,1:3)

    !--------------------------------------------------------------------------
   !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 2D Real Data Test"
    call ESMF_LocalArrayGet(array2, realptr2, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 2c get returned"
    print *, "realptr2 data = ", realptr2(1:3,1:3)

    !--------------------------------------------------------------------------
   !EX_UTest
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

    call ESMF_LocalArrayDestroy(array2, rc=rc)
    print *, "array 2c destroy returned"


!-------------------------------------------------------------------------------
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Real, 3D.  DATA_COPY set
 
    ! Allocate and set initial data values
    ni = 5 
    nj = 43 
    nk = 8
    allocate(real3dptr(ni,nj,nk))
    do i=1,ni
     do j=1,nj
       do k=1,nk
        real3dptr(i,j,k) = i + ((j-1)*ni) + ((k-1)*ni*nj) + 0.1
       enddo
     enddo
    enddo

    !--------------------------------------------------------------------------
   !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Local Array 3D DATA_COPY Real Data Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    print *, "array 4a create returned"

    !--------------------------------------------------------------------------
   !EX_UTest
    write(failMsg, *) "Did returned ESMF_SUCCESS incorrectly"
    write(name, *) "Getting Local Array 3D Real Data with wrong dimension array Test"
    call ESMF_LocalArrayGet(array4, realptr2, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
   !EX_UTest
    nullify(real3d2ptr)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 3D Real Data without allocating array size Test"
    call ESMF_LocalArrayGet(array4, real3d2ptr, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 4a get returned"

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
    deallocate (real3d2ptr)

    !--------------------------------------------------------------------------
   !EX_UTest
    write(failMsg, *) "Array data should be the same."
    write(name, *) "Compare Local Array 3D Real Data without allocating array size Test"
    call ESMF_Test((result.eq.0), name, failMsg, result, ESMF_SRCLINE)


    !--------------------------------------------------------------------------
   !EX_UTest
    allocate(real3d2ptr(ni,nj,nk))
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting Local Array 3D Real Data Test"
    call ESMF_LocalArrayGet(array4, real3d2ptr, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 4a get returned"

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
    deallocate (real3d2ptr)

    !--------------------------------------------------------------------------
   !EX_UTest
    write(failMsg, *) "Array data should be the same."
    write(name, *) "Compare Local Array 3D Real Data Test"
    call ESMF_Test((result.eq.0), name, failMsg, result, ESMF_SRCLINE)

    ! with do copy, the original can go now
    deallocate(real3dptr)

    !call ESMF_LocalArrayPrint(array4, "", rc=rc)
    print *, "array 4a print returned"

    call ESMF_LocalArrayDestroy(array4, rc=rc)
    print *, "array 4a destroy returned"


 
 
    ! Allocate and free different sizes testing end of array printing code
    ni = 33 
    nj = 8 
    nk = 160
    allocate(real3dptr(ni,nj,nk))

    !--------------------------------------------------------------------------
   !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Local Array 3D DATA_COPY Real Data Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    print *, "array 4b create returned"

    call ESMF_LocalArrayPrint(array4, "", rc=rc)
    print *, "array 4b print returned"

    ! this deletes the space
    call ESMF_LocalArrayDestroy(array4, rc=rc)
    print *, "array 4b destroy returned"

    ! Allocate and free different sizes testing end of array printing code
    ni = 10 
    nj = 3 
    nk = 40


    deallocate(real3dptr)
    nullify(real3dptr)

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_FAILURE"
    write(name, *) "Creating a Local Array 3D DATA_REF Real Data with deallocated array Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    print *, "array 4c failed create returned"

    !This print of array4 is commented out becauses it crashes
    !Bug report 972679 has been filed.
    !This print statement will be uncommented when the bug is fixed
    !call ESMF_LocalArrayPrint(array4, "", rc=rc)
    !print *, "array 4c print of bad array returned"

    !This test is commented out becauses it crashes
    !Bug report 791282 has been filed.
    !This test will be uncommented when the bug is fixed
    !write(failMsg, *) "Did not return ESMF_FAILURE"
    !write(name, *) "Destroying a Local array Test"
    !call ESMF_LocalArrayDestroy(array4, rc=rc)
    !call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !print *, "array 4c re-destroy returned"


    !--------------------------------------------------------------------------
    !EX_UTest
    allocate(real3dptr(ni,nj,nk))
    write(failMsg, *) "Did not return ESMF_FAILURE"
    write(name, *) "Creating a Local Array 3D DATA_REF Real Data with an allocated array Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    print *, "array 4d create returned"

    call ESMF_LocalArrayPrint(array4, "", rc=rc)
    print *, "array 4d print returned"

    ! this does not delete the space, we have to do it ourselves
    call ESMF_LocalArrayDestroy(array4, rc=rc)
    !print *, "array 4d destroy returned"
    deallocate(real3dptr)

    ! Allocate and free different sizes testing end of array printing code
    ni = 11 
    nj = 3 
    nk = 40
    allocate(real3dptr(ni,nj,nk))

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Creating a Local Array 3D DATA_REF Real Data with an allocated array Test"
    array4 = ESMF_LocalArrayCreate(real3dptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 4e create returned"

    call ESMF_LocalArrayPrint(array4, "", rc=rc)
    print *, "array 4e print returned"

    ! this does not delete the space, we have to do it ourselves
    call ESMF_LocalArrayDestroy(array4, rc=rc)
    print *, "array 4e destroy returned"
    deallocate(real3dptr)


!-------------------------------------------------------------------------------
!   !  Create based on an array specification.

    !--------------------------------------------------------------------------
    !EX_UTest
    arank = 2
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Initializing an Array Spec of rank 2 Test"
    call ESMF_ArraySpecSet(arrayspec, arank, ESMF_TYPEKIND_R4, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! set all counts to 1 first, then alter the ones you want to change
    counts = 1
    counts(1) = 10
    counts(2) = 20

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Creating an Array Spec Test"
    array2 = ESMF_LocalArrayCreate(arrayspec, counts(1:2), rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "rank not correct" 
    write(name, *) "Get Spec rank and verify Test"
    call ESMF_ArraySpecGet(arrayspec, rank=brank, rc=rc)
    call ESMF_Test((arank.eq.brank), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "kind not correct" 
    write(name, *) "Get Spec kind and verify Test"
    call ESMF_ArraySpecGet(arrayspec, typekind=akind, rc=rc)
    call ESMF_Test((akind.eq.ESMF_TYPEKIND_R4), name, failMsg, result, ESMF_SRCLINE)

    ! Finish for arank = 2
    call ESMF_LocalArrayDestroy(array2, rc=rc)

    !--------------------------------------------------------------------------
    !EX_UTest
    arank = 10
    write(failMsg, *) "Did not return ESMF_FAILURE"
    write(name, *) "Initializing an Array Spec of rank 10 Test"
    call ESMF_ArraySpecSet(arrayspec, arank, ESMF_TYPEKIND_R4, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_FAILURE"
    write(name, *) "Creating an Array from a Spec with rank of 10 Test"
    array2 = ESMF_LocalArrayCreate(arrayspec, counts, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !EX_UTest
    arank = 5
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Initializing an Array Spec of rank 5 Test"
    call ESMF_ArraySpecSet(arrayspec, arank, ESMF_TYPEKIND_R4, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating an Array from a Spec with rank of 5 Test"
    array2 = ESMF_LocalArrayCreate(arrayspec, counts, rc=rc)
#ifdef ESMF_NO_GREATER_THAN_4D
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

    !--------------------------------------------------------------------------
    !EX_UTest
    arank = 4
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Initializing an Array Spec of rank 4 Test"
    call ESMF_ArraySpecSet(arrayspec, arank, ESMF_TYPEKIND_R4, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating an Array from a Spec with rank of 4 Test"
    array2 = ESMF_LocalArrayCreate(arrayspec, counts, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------------
!   !  Create based on an existing, allocated F90 pointer. 
!   !  Data is type Integer, 1D.
 
    ! Allocate and set initial data values
    ni = 35 
    nj = 40
    allocate(int2Dptr(ni,nj))
    do i=1,ni
      do j=1,nj
       int2Dptr(i, j) = i + j
      enddo
    enddo

    !--------------------------------------------------------------------------
    !EX_UTest
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Initializing an Array with data type integer Test"
    array1 = ESMF_LocalArrayCreate(int2Dptr, ESMF_DATA_REF, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1f create returned"

    call ESMF_LocalArrayWrite(array1, filename="./TestArrayData", rc=rc)
    print *, "array 1f write returned"

    call ESMF_LocalArrayDestroy(array1, rc=rc)
    print *, "array 1f destroy returned"
    deallocate(int2Dptr)

!-------------------------------------------------------------------------------

#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_LocalArrayTest
    
!\end{verbatim}
    
