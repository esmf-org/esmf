! on halem, run with
! bsub -P "hp606" -q general -n 4 prun -n 2 -N 1 ./ESMF_Layout_FTest
! to run 2 DEs (MPI processes) on one node

program ESMF_Layout_FTest

  use ESMF_LayoutMod

  type(ESMF_Layout) :: layout
  integer, dimension(2) :: delist
  integer :: nx, ny, x, y, id, rc
  integer, dimension(5) :: array
  integer :: result, len

  delist = (/ 0, 1 /)
  len = 5

  layout = ESMF_LayoutCreate(2, 1, delist, ESMF_XFAST, rc)

  call ESMF_LayoutGetSize(layout, nx, ny, rc)
  print *, "ESMF_LayoutGetSize(nx, ny) = ", nx, ny

  call ESMF_LayoutGetDEPosition(layout, x, y, rc)
  print *, "ESMF_LayoutGetDEPosition(x, y) = ", x, y

  call ESMF_LayoutGetDEid(layout, id, rc)
  print *, "ESMF_LayoutGetDEid(id) = ", id

  do i=1,len
    array(i) = (id+1) * i
  end do
  print *, "array() = ", array

  call ESMF_LayoutAllReduce(layout, array, result, len, ESMF_SUM, rc)
  print *, "ESMF_LayoutAllReduce(sum) = ", result

  call ESMF_LayoutDestroy(layout, rc)

end program ESMF_Layout_FTest
