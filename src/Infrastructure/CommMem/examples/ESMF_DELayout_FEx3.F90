! $Id: ESMF_DELayout_FEx3.F90,v 1.4 2003/09/04 18:57:55 cdeluca Exp $
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
! Excercises the DELayoutScatter F90 to C++ interface.
! pre-test for System Test #79497
!
! on halem, run with
! bsub -P "hp606" -q general -n 8 prun -m cyclic -n 6 -N 2 ./ESMF_DELayout_FEx3
! to run 6 DEs (MPI processes) on two nodes
!-----------------------------------------------------------------------------

program ESMF_DELayout_FEx3

  use ESMF_Mod

  type(ESMF_DELayout) :: layout
  integer, dimension(6) :: delist
  integer, dimension(2) :: layoutDims, layoutCommTypes
  integer :: nx, ny, x, y, id, rc
  integer :: i, len, srcDEid

  real(ESMF_KIND_R8), dimension(:), pointer :: sArrayR8
  real(ESMF_KIND_R8), dimension(:), pointer :: rArrayR8
  real(ESMF_KIND_R4), dimension(:), pointer :: sArrayR4
  real(ESMF_KIND_R4), dimension(:), pointer :: rArrayR4
  integer(ESMF_KIND_I4), dimension(:), pointer :: sArrayI4
  integer(ESMF_KIND_I4), dimension(:), pointer :: rArrayI4

  type(ESMF_LocalArray) :: sndArrayR8, rcvArrayR8, &
                           sndArrayR4, rcvArrayR4, &
                           sndArrayI4, rcvArrayI4

  ! allocate data arrays
  allocate(sArrayR8(120))
  allocate(rArrayR8(20))
  allocate(sArrayR4(120))
  allocate(rArrayR4(20))
  allocate(sArrayI4(120))
  allocate(rArrayI4(20))

  ! 2x3 DE layout
  layoutDims = (/ 2, 3 /)

  ! layout comm types
  layoutCommTypes = (/ ESMF_COMMTYPE_SHR, ESMF_COMMTYPE_SHR /)

  ! 6 DEs: DE 0 - DE 5
  delist = (/ 0, 1, 2, 3, 4, 5 /)
  
  ! number of data elements per DE
  len = 20

  ! initialize rArray to zero
  do i=1,20
    rArrayR8(i) = 0.0
    rArrayR4(i) = 0.0
    rArrayI4(i) = 0
  end do

  call ESMF_FrameworkInitialize(rc)

  ! create 2x3 layout of DEs in X-direction
  layout = ESMF_DELayoutCreate(delist, 2, layoutDims, layoutCommTypes, rc)

  ! verify size of layout
  call ESMF_DELayoutGetSize(layout, nx, ny, rc)
  print *, "ESMF_DELayoutGetSize(nx, ny) = ", nx, ny

  ! get our DE's position within the layout
  call ESMF_DELayoutGetDEPosition(layout, x, y, rc)
  print *, "ESMF_DELayoutGetDEPosition(x, y) = ", x, y

  ! get our DE id
  call ESMF_DELayoutGetDEid(layout, id, rc)
  print *, "ESMF_DELayoutGetDEid(id) = ", id

  ! create ESMF_LocalArray containers for the data arrays
  sndArrayR8 = ESMF_LocalArrayCreate(sArrayR8, rc=rc)
  rcvArrayR8 = ESMF_LocalArrayCreate(rArrayR8, rc=rc)
  sndArrayR4 = ESMF_LocalArrayCreate(sArrayR4, rc=rc)
  rcvArrayR4 = ESMF_LocalArrayCreate(rArrayR4, rc=rc)
  sndArrayI4 = ESMF_LocalArrayCreate(sArrayI4, rc=rc)
  rcvArrayI4 = ESMF_LocalArrayCreate(rArrayI4, rc=rc)

  ! root DE populates send array and sends it
  srcDEid = 2
  if (id .eq. srcDEid) then
    do i=1,120
      sArrayR8(i) = i
      sArrayR4(i) = i
      sArrayI4(i) = i
    end do

    ! verify
    print *, "sArrayR8 = ", sArrayR8
    print *, "sArrayR4 = ", sArrayR4
    print *, "sArrayI4 = ", sArrayI4
  endif

  ! scatter naked F90 arrays

  call ESMF_DELayoutScatter(layout, sArrayR8, rArrayR8, len, srcDEid, rc)
  print *, "DE ", id, " rArrayR8() = ", rArrayR8

  call ESMF_DELayoutScatter(layout, sArrayR4, rArrayR4, len, srcDEid, rc)
  print *, "DE ", id, " rArrayR4() = ", rArrayR4

  call ESMF_DELayoutScatter(layout, sArrayI4, rArrayI4, len, srcDEid, rc)
  print *, "DE ", id, " rArrayI4() = ", rArrayI4

  ! scatter ESMF local arrays

  call ESMF_DELayoutScatter(layout, sndArrayR8, rcvArrayR8, len, srcDEid, rc)
  print *, "DE ", id, " rArrayR8() = ", rArrayR8

  call ESMF_DELayoutScatter(layout, sndArrayR4, rcvArrayR4, len, srcDEid, rc)
  print *, "DE ", id, " rArrayR4() = ", rArrayR4

  call ESMF_DELayoutScatter(layout, sndArrayI4, rcvArrayI4, len, srcDEid, rc)
  print *, "DE ", id, " rArrayI4() = ", rArrayI4

  ! clean up
  call ESMF_LocalArrayDestroy(sndArrayR8, rc)
  call ESMF_LocalArrayDestroy(rcvArrayR8, rc)
  call ESMF_LocalArrayDestroy(sndArrayR4, rc)
  call ESMF_LocalArrayDestroy(rcvArrayR4, rc)
  call ESMF_LocalArrayDestroy(sndArrayI4, rc)
  call ESMF_LocalArrayDestroy(rcvArrayI4, rc)
  call ESMF_DELayoutDestroy(layout, rc)

  call ESMF_FrameworkFinalize(rc)

end program ESMF_DELayout_FEx3
