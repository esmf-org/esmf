! $Id: ESMF_DELayout_F1Ex.F90,v 1.7 2004/01/07 21:09:51 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.

! ESMC CommMem application example program
!____________________________________________________________________________
!EXAMPLE        String used by test script to count examples.
!-----------------------------------------------------------------------------
!
! !DESCRIPTION:
!
! Exercises the DELayout F90 to C++ interface.
! pre-test for System Test #62501: Uses data from Test 62501: row 5
!
! on halem, run with
! bsub -P "hp606" -q general -n 4 prun -n 2 -N 1 ./ESMF_DELayout_FEx
! to run 2 DEs (MPI processes) on one node
!-----------------------------------------------------------------------------
!BOP

!See the following code fragments for examples of how to create DELayouts and use them in the communications routines.
!Also see the Programming Model section of this document.  
!\begin{verbatim}

program ESMF_DELayout_F1Ex

#include <ESMF_Macros.inc>

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  implicit none

  type(ESMF_DELayout) :: layout
  integer, dimension(2) :: delist
  integer :: nx, ny, x, y, id, rc
  integer, dimension(20) :: array1, array2
  integer :: i, result, len
!\end{verbatim}
!EOP

  integer :: finalrc
  finalrc = ESMF_SUCCESS

!BOP
!\begin{verbatim}
  ! initialize framework
  call ESMF_Initialize(rc)

  ! 2 DEs: DE 0 and DE 1
  delist = (/ 0, 1 /)
  
  ! number of data elements per DE
  len = 20

  ! create 2x1 layout of DEs in X-direction
  layout = ESMF_DELayoutCreate(delist, 2, (/ 2, 1 /), (/ 0, 0 /), rc)

  ! verify size of layout
  call ESMF_DELayoutGetSize(layout, nx, ny, rc)
!\end{verbatim}
!EOP

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

!BOP
!\begin{verbatim}
  ! initialize framework
  print *, "ESMF_DELayoutGetSize(nx, ny) = ", nx, ny

  ! get our DE's position within the layout
  call ESMF_DELayoutGetDEPosition(layout, x, y, rc)
!\end{verbatim}
!EOP

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

!BOP
!\begin{verbatim}
  print *, "ESMF_DELayoutGetDEPosition(x, y) = ", x, y

  ! get our DE id
  call ESMF_DELayoutGetDEid(layout, id, rc)

  print *, "ESMF_DELayoutGetDEid(id) = ", id
!\end{verbatim}
!EOP

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

!BOP
!\begin{verbatim}

  ! populate DE 0 array with first half of row 5
  do i=1,len
    array1(i) = 80 + i
  end do

  ! populate DE 1 array with second half of row 5
  do i=1,len
    array2(i) = 480 + i
  end do

  ! verify
  print *, "array1() = ", array1
  print *, "array2() = ", array2

  ! add 'em up!
  ! perform allreduce with our DE's array
  if (id .eq. 0) then
    call ESMF_DELayoutAllReduce(layout, array1, result, len, ESMF_SUM, rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
  else
    call ESMF_DELayoutAllReduce(layout, array2, result, len, ESMF_SUM, rc)
!\end{verbatim}
!EOP

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOP
!\begin{verbatim}
  endif

  ! ... and the answer is ...
  print *, "ESMF_DELayoutAllReduce(sum) = ", result
!\end{verbatim}
!EOP

  if (result.NE.11620) then
     finalrc = ESMF_FAILURE
  end if

!BOP
!\begin{verbatim}
  call ESMF_DELayoutDestroy(layout, rc)
!\end{verbatim}
!EOP

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

!BOP
!\begin{verbatim}
  call ESMF_Finalize(rc)
!\end{verbatim}
!EOP

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

  if (finalrc.EQ.ESMF_SUCCESS) then
     print *, "PASS: ESMF_DELayout_F1Ex.F90"
  else
     print *, "FAIL: ESMF_DELayout_F1Ex.F90"
  end if

!BOP
!\begin{verbatim}
end program ESMF_DELayout_F1Ex
!\end{verbatim}
!EOP
