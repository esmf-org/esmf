! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_LAPACKUTest

! Sanity test calling an LAPACK routine from the ESMF framework

#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  implicit none

  character(ESMF_MAXSTR) :: name, failMsg
  integer :: rc, result

#if defined (ESMF_LAPACK)
#if defined (ESMF_LAPACK_INTERNAL)
#include "ESMF_LapackBlas.inc"
#endif

  integer, parameter :: dp_k = kind (1.0d0)

  integer, parameter :: n = 123, nrhs = 12
  real(dp_k) :: a(n, n), b(n, nrhs)
  integer :: pivs(n)

  real(dp_k) :: cond
  integer :: rank, info

  real(dp_k) :: work1(1)
  real(dp_k), allocatable :: work(:)
  integer :: worklen

  interface
    subroutine DGELSY (m, n, nrhs, a, lda, b, ldb, jpvt,  &
        rcond, rank, work, lwork, info)
      implicit none
      integer, parameter :: dp_k = kind (1.0d0)
      integer, intent(in) :: m, n, nrhs
      real(dp_k), intent(inout) :: a(*)
      integer, intent(in) :: lda
      real(dp_k), intent(inout) :: b(*)
      integer, intent(in) :: ldb
      integer, intent(out) :: jpvt(*)
      real(dp_k), intent(in) :: rcond
      integer, intent(out) :: rank
      real(dp_k), intent(out) :: work(*)
      integer, intent(in) :: lwork
      integer, intent(inout) :: info
    end subroutine
  end interface

#endif

! Basic test of calling an LAPACK routine

  call ESMF_TestStart (ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  result = 0

#if defined (ESMF_LAPACK)

  call random_number (a)
  call random_number (b)


  !------------------------------------------------------------------------
  !NEX_UTest
  name = "DGELSY workspace size inquiry test"
  info = 0
  pivs = 0
  call DGELSY (n, n, nrhs, a, n, b, n, pivs, cond, rank, work1, -1, info)
  worklen = int (work1(1))
  print *, '  suggested workspace length =', worklen

  write (failMsg, *) trim (name) // ': info =', info
  call ESMF_Test (info == 0, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  name = "DGELSY computation test"
  allocate (work(worklen))
  cond = 1.234e-5
  pivs = 0
  call DGELSY (n, n, nrhs, a, n, b, n, pivs, cond, rank, work, worklen, info)

  write (failMsg, *) trim (name) // ': info =', info
  call ESMF_Test (info == 0, name, failMsg, result, ESMF_SRCLINE)
  deallocate (work)
#else
  ! Add two dummy passes so test won't show up as crashed
  name = "dummy test without LAPACK"
  failMsg = "dummy failure"
  call ESMF_Test((.TRUE.), name, failMsg, result, ESMF_SRCLINE)
  call ESMF_Test((.TRUE.), name, failMsg, result, ESMF_SRCLINE)

#endif

  call ESMF_TestEnd (ESMF_SRCLINE)

end program
