! $Id: ESMF_ArrayUTest.F90,v 1.1 2004/04/08 19:20:22 svasquez Exp $
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
      program ESMF_ArrayUTest

!------------------------------------------------------------------------------
!

#include <ESMF.h>

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayTest - Test Array functionalities
!
! !DESCRIPTION:
!
! The code in this file drives F90 Array unit tests.
!
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod      ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ArrayUTest.F90,v 1.1 2004/04/08 19:20:22 svasquez Exp $'
!------------------------------------------------------------------------------

     ! cumulative result: count failures; no failures equals "all pass"
     integer :: result = 0


!   ! Local variables
    integer :: nx, ny, arank, brank, rc 
    integer :: i, j, k, l, m, ni, nj, nk, nl, nm
    type(ESMF_ArraySpec) :: arrayspec, spec
    type(ESMF_DataType) :: atype
    type(ESMF_DataKind) :: akind
    integer :: counts(ESMF_MAXDIM), lb(1), ub(1), rlb(1), rub(1)
    type(ESMF_Array) :: array1, array2, array3, array4
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: real3dptr, real3d2ptr
    real(ESMF_KIND_R8), dimension(:,:), pointer :: realptr, realptr2
    integer(ESMF_KIND_I4), dimension(:), pointer :: intptr, intptr2
    integer(ESMF_KIND_I4), dimension(:,:), pointer :: int2Dptr
    character(ESMF_MAXSTR) :: filename
    real, dimension(:,:), pointer :: f90ptr1


    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0

    
    call ESMF_Initialize()

!-------------------------------------------------------------------------------
!   !  Create an Array Test
 
    !NEX_UTest
    allocate(f90ptr1(10,20))
    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Create Array Test"
    array1 =  ESMF_ArrayCreate( f90ptr1, ESMF_DATA_COPY, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 create returned"

!-------------------------------------------------------------------------------
!   !  Destroy an Array Test

    write(failMsg, *) "Did not return ESMF_SUCCESS" 
    write(name, *) "Destroy Array Test"
    call ESMF_ArrayDestroy(array1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    print *, "array 1 destroy returned"

!-------------------------------------------------------------------------------

    call ESMF_Finalize()


    end program ESMF_ArrayUTest
    
!\end{verbatim}
    
