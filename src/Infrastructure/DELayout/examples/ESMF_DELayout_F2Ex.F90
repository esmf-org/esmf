! $Id: ESMF_DELayout_F2Ex.F90,v 1.6 2004/04/09 19:53:57 eschwab Exp $
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
! Excercises the DELayout F90 to C++ interface.
! pre-test for System Test #70384 
!
! on halem, run with
! bsub -P "hp606" -q general -n 8 prun -m cyclic -n 6 -N 2 ./ESMF_DELayout_FEx2
! to run 6 DEs (MPI processes) on two nodes
!-----------------------------------------------------------------------------

program ESMF_DELayout_FEx2

  use ESMF_Mod

  type(ESMF_DELayout) :: layout
  integer, dimension(6) :: delist
  integer, dimension(2) :: layoutDims, layoutCommTypes
  integer :: nx, ny, x, y, id, rc, finalrc
  integer(ESMF_KIND_I4), dimension(20) :: sArray1I4, sArray2I4, sArray3I4, &
                                           sArray4I4, sArray5I4, sArray6I4
  integer(ESMF_KIND_I4), dimension(180) :: rArrayI4

  real(ESMF_KIND_R4), dimension(20) :: sArray1R4, sArray2R4, sArray3R4, &
                                        sArray4R4, sArray5R4, sArray6R4
  real(ESMF_KIND_R4), dimension(180) :: rArrayR4

  real(ESMF_KIND_R8), dimension(20) :: sArray1R8, sArray2R8, sArray3R8, &
                                        sArray4R8, sArray5R8, sArray6R8
  real(ESMF_KIND_R8), dimension(180) :: rArrayR8

  integer, dimension(6) :: rlen, rdispls
  integer :: i, slen
  finalrc = ESMF_SUCCESS

  ! 6 DEs: DE 0 - DE 5
  delist = (/ 0, 1, 2, 3, 4, 5 /)
  
  ! 2x3 DE layout
  layoutDims = (/ 2, 3 /)

  ! layout comm types
  layoutCommTypes = (/ ESMF_COMMTYPE_SHR, ESMF_COMMTYPE_SHR /)

  ! number of data elements per DE
  slen = 20

  ! initialize rArray to zero
  do i=1,180
    rArrayI4(i) = 0
    rArrayR4(i) = 0
    rArrayR8(i) = 0
  end do

  ! fill-in rlen and rdispl arrays
  do i=1,6
    rlen(i) = slen
    rdispls(i) = (i-1) * (slen + 10)
  end do

  ! Initialize ESMF
  call ESMF_Initialize(rc=rc)

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

  ! create 2x3 layout of DEs in X-direction
  layout = ESMF_DELayoutCreate(delist, 2, layoutDims, layoutCommTypes, rc)

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if


  ! verify size of layout
  call ESMF_DELayoutGetSize(layout, nx, ny, rc)

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

  print *, "ESMF_DELayoutGetSize(nx, ny) = ", nx, ny

  ! get our DE's position within the layout
  call ESMF_DELayoutGetDEPosition(layout, x, y, rc)

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

  print *, "ESMF_DELayoutGetDEPosition(x, y) = ", x, y

  ! get our DE id
  call ESMF_DELayoutGetDEid(layout, id, rc)

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

  print *, "ESMF_DELayoutGetDEid(id) = ", id

  ! each DE populates its send array and sends it 
  if (id .eq. 0) then
    do i=1,slen
      sArray1I4(i) = i
      sArray1R4(i) = i
      sArray1R8(i) = i
    end do

    ! verify
    print *, "sArray1I4() = ", sArray1I4
    print *, "sArray1R4() = ", sArray1R4
    print *, "sArray1R8() = ", sArray1R8

    call ESMF_DELayoutAllGatherV(layout, sArray1I4, slen, &
                                         rArrayI4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray1R4, slen, &
                                         rArrayR4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray1R8, slen, &
                                         rArrayR8,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

  else if (id .eq. 1) then
    do i=1,slen
      sArray2I4(i) = i * 2
      sArray2R4(i) = i * 2
      sArray2R8(i) = i * 2
    end do

    ! verify
    print *, "sArray2I4() = ", sArray2I4
    print *, "sArray2R4() = ", sArray2R4
    print *, "sArray2R8() = ", sArray2R8

    call ESMF_DELayoutAllGatherV(layout, sArray2I4, slen, &
                                         rArrayI4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray2R4, slen, &
                                         rArrayR4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray2R8, slen, &
                                         rArrayR8,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

  else if (id .eq. 2) then
    do i=1,slen
      sArray3I4(i) = i * 3
      sArray3R4(i) = i * 3
      sArray3R8(i) = i * 3
    end do

    ! verify
    print *, "sArray3I4() = ", sArray3I4
    print *, "sArray3R4() = ", sArray3R4
    print *, "sArray3R8() = ", sArray3R8

    call ESMF_DELayoutAllGatherV(layout, sArray3I4, slen, &
                                         rArrayI4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray3R4, slen, &
                                         rArrayR4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray3R8, slen, &
                                         rArrayR8,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

  else if (id .eq. 3) then
    do i=1,slen
      sArray4I4(i) = i * 4
      sArray4R4(i) = i * 4
      sArray4R8(i) = i * 4
    end do

    ! verify
    print *, "sArray4I4() = ", sArray4I4
    print *, "sArray4R4() = ", sArray4R4
    print *, "sArray4R8() = ", sArray4R8

    call ESMF_DELayoutAllGatherV(layout, sArray4I4, slen, &
                                         rArrayI4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray4R4, slen, &
                                         rArrayR4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray4R8, slen, &
                                         rArrayR8,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

  else if (id .eq. 4) then
    do i=1,slen
      sArray5I4(i) = i * 5
      sArray5R4(i) = i * 5
      sArray5R8(i) = i * 5
    end do

    ! verify
    print *, "sArray5I4() = ", sArray5I4
    print *, "sArray5R4() = ", sArray5R4
    print *, "sArray5R8() = ", sArray5R8

    call ESMF_DELayoutAllGatherV(layout, sArray5I4, slen, &
                                         rArrayI4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray5R4, slen, &
                                         rArrayR4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray5R8, slen, &
                                         rArrayR8,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

  else if (id .eq. 5) then
    do i=1,slen
      sArray6I4(i) = i * 6
      sArray6R4(i) = i * 6
      sArray6R8(i) = i * 6
    end do

    ! verify
    print *, "sArray6I4() = ", sArray6I4
    print *, "sArray6R4() = ", sArray6R4
    print *, "sArray6R8() = ", sArray6R8

    call ESMF_DELayoutAllGatherV(layout, sArray6I4, slen, &
                                         rArrayI4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray6R4, slen, &
                                         rArrayR4,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    call ESMF_DELayoutAllGatherV(layout, sArray6R8, slen, &
                                         rArrayR8,  rlen, rdispls, rc)

    if (rc.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

  endif

  ! ... and the result is ...
  print *, "DE ", id, " rArrayI4() = ", rArrayI4
  print *, "DE ", id, " rArrayR4() = ", rArrayR4
  print *, "DE ", id, " rArrayR8() = ", rArrayR8

  call ESMF_DELayoutDestroy(layout, rc)


  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

  ! Finalize ESMF
  call ESMF_Finalize(rc)

  if (rc.NE.ESMF_SUCCESS) then
      finalrc = ESMF_FAILURE
  end if

  if (finalrc.EQ.ESMF_SUCCESS) then
     print *, "PASS: ESMF_DELayout_FEx2.F90"
  else
     print *, "FAIL: ESMF_DELayout_FEx2.F90"
  end if



end program ESMF_DELayout_FEx2
