! $Id: ESMF_DELayout_FEx2.F90,v 1.2 2003/03/10 04:16:22 cdeluca Exp $
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
! Excercises the DELayout F90 to C++ interface.
! pre-test for System Test #70384 
!
! on halem, run with
! bsub -P "hp606" -q general -n 8 prun -m cyclic -n 6 -N 2 ./ESMF_DELayout_FEx2
! to run 6 DEs (MPI processes) on two nodes
!-----------------------------------------------------------------------------

program ESMF_DELayout_FEx2

  use ESMF_DELayoutMod

  type(ESMF_DELayout) :: layout
  integer, dimension(6) :: delist
  integer :: nx, ny, x, y, id, rc
  integer, dimension(20) :: sArray1, sArray2, sArray3, sArray4, sArray5, sArray6
  integer, dimension(180) :: rArray
  integer, dimension(6) :: rlen, rdispls
  integer :: i, slen

  ! 6 DEs: DE 0 - DE 5
  delist = (/ 0, 1, 2, 3, 4, 5 /)
  
  ! number of data elements per DE
  slen = 20

  ! fill-in rlen and rdispl arrays
  do i=1,6
    rlen(i) = slen
    rdispls(i) = (i-1) * (slen + 10)
  end do

  ! create 2x3 layout of DEs in X-direction
  layout = ESMF_DELayoutCreate(2, 3, delist, ESMF_XFAST, rc)

  ! verify size of layout
  call ESMF_DELayoutGetSize(layout, nx, ny, rc)
  print *, "ESMF_DELayoutGetSize(nx, ny) = ", nx, ny

  ! get our DE's position within the layout
  call ESMF_DELayoutGetDEPosition(layout, x, y, rc)
  print *, "ESMF_DELayoutGetDEPosition(x, y) = ", x, y

  ! get our DE id
  call ESMF_DELayoutGetDEid(layout, id, rc)
  print *, "ESMF_DELayoutGetDEid(id) = ", id

  ! each DE populates its send array and sends it 
  if (id .eq. 0) then
    do i=1,slen
      sArray1(i) = i
    end do

    ! verify
    print *, "sArray1() = ", sArray1

    call ESMF_DELayoutAllGatherVI(layout, sArray1, slen, &
                                        rArray,  rlen, rdispls, rc)
  else if (id .eq. 1) then
    do i=1,slen
      sArray2(i) = i * 2
    end do

    ! verify
    print *, "sArray2() = ", sArray2

    call ESMF_DELayoutAllGatherVI(layout, sArray2, slen, &
                                        rArray,  rlen, rdispls, rc)
  else if (id .eq. 2) then
    do i=1,slen
      sArray3(i) = i * 3
    end do

    ! verify
    print *, "sArray3() = ", sArray3

    call ESMF_DELayoutAllGatherVI(layout, sArray3, slen, &
                                        rArray,  rlen, rdispls, rc)
  else if (id .eq. 3) then
    do i=1,slen
      sArray4(i) = i * 4
    end do

    ! verify
    print *, "sArray4() = ", sArray4

    call ESMF_DELayoutAllGatherVI(layout, sArray4, slen, &
                                        rArray,  rlen, rdispls, rc)
  else if (id .eq. 4) then
    do i=1,slen
      sArray5(i) = i * 5
    end do

    ! verify
    print *, "sArray5() = ", sArray5

    call ESMF_DELayoutAllGatherVI(layout, sArray5, slen, &
                                        rArray,  rlen, rdispls, rc)
  else if (id .eq. 5) then
    do i=1,slen
      sArray6(i) = i * 6
    end do

    ! verify
    print *, "sArray6() = ", sArray6

    call ESMF_DELayoutAllGatherVI(layout, sArray6, slen, &
                                        rArray,  rlen, rdispls, rc)
  endif

  ! ... and the result is ...
  print *, "DE ", id, " rArray() = ", rArray

  call ESMF_DELayoutDestroy(layout, rc)

end program ESMF_DELayout_FEx2
