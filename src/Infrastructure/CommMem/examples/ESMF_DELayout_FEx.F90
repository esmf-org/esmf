! $Id: ESMF_DELayout_FEx.F90,v 1.1 2003/03/10 03:46:57 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.

! ESMC CommMem application example program

!-----------------------------------------------------------------------------
!
! !DESCRIPTION:
!
! Excercises the Layout F90 to C++ interface.
! pre-test for System Test #62501: Uses data from Test 62501: row 5
!
! on halem, run with
! bsub -P "hp606" -q general -n 4 prun -n 2 -N 1 ./ESMF_Layout_FEx
! to run 2 DEs (MPI processes) on one node
!-----------------------------------------------------------------------------

program ESMF_Layout_FEx

  use ESMF_LayoutMod

  type(ESMF_Layout) :: layout
  integer, dimension(2) :: delist
  integer :: nx, ny, x, y, id, rc
  integer, dimension(20) :: array1, array2
  integer :: i, result, len

  ! 2 DEs: DE 0 and DE 1
  delist = (/ 0, 1 /)
  
  ! number of data elements per DE
  len = 20

  ! create 2x1 layout of DEs in X-direction
  layout = ESMF_LayoutCreate(2, 1, delist, ESMF_XFAST, rc)

  ! verify size of layout
  call ESMF_LayoutGetSize(layout, nx, ny, rc)
  print *, "ESMF_LayoutGetSize(nx, ny) = ", nx, ny

  ! get our DE's position within the layout
  call ESMF_LayoutGetDEPosition(layout, x, y, rc)
  print *, "ESMF_LayoutGetDEPosition(x, y) = ", x, y

  ! get our DE id
  call ESMF_LayoutGetDEid(layout, id, rc)
  print *, "ESMF_LayoutGetDEid(id) = ", id

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
    call ESMF_LayoutAllReduce(layout, array1, result, len, ESMF_SUM, rc)
  else
    call ESMF_LayoutAllReduce(layout, array2, result, len, ESMF_SUM, rc)
  endif

  ! ... and the answer is ...
  print *, "ESMF_LayoutAllReduce(sum) = ", result

  call ESMF_LayoutDestroy(layout, rc)

end program ESMF_Layout_FEx
