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
!
program ESMF_LogErrPerfUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_LogErrPerfUTest - This unit test file tests LogErr performance
!  
! !DESCRIPTION:
!
! The code in this file drives Fortran ESMF_Log API.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc
  
  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  
  ! other variables
  real(ESMF_KIND_R8)     :: dt, dtTest

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Performance of ESMF_LogFoundError() 1000x Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call perfLogFoundError(n=1000, dt=dt)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Performance of ESMF_LogFoundError() 10000x Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call perfLogFoundError(n=10000, dt=dt)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Performance of ESMF_LogFoundError() 100000x Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call perfLogFoundError(n=100000, dt=dt)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Performance of ESMF_LogFoundError() 1000000x Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call perfLogFoundError(n=1000000, dt=dt)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Threshold check for ESMF_LogFoundError() 1000000x Test"
#ifdef ESMF_BOPT_g
  dtTest = 2.d-7  ! 200ns is expected to pass in debug mode
#else
  dtTest = 6.d-8  ! 60ns is expected to pass in optimized mode
#endif
  write(failMsg, *) "ESMF_LogFoundError() performance problem! ", &
    dt, ">", dtTest
  call ESMF_Test((dt<dtTest), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

 contains !---------------------------------------------------------------------

  !--------------------------------------------------------------------------
  subroutine perfLogFoundError(n, dt)
    integer             :: n
    real(ESMF_KIND_R8)  :: dt
    ! local vars
    integer             :: i
    real(ESMF_KIND_R8)  :: t0, t1
    character(160)      :: msgString
    logical             :: testFlag
    !
    call ESMF_VMWtime(t0, rc=rc)
    do i=1, n
      testFlag = (ESMF_LogFoundError(rcToCheck=ESMF_SUCCESS, &
        msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))
    enddo
    call ESMF_VMWtime(t1, rc=rc)
    dt = (t1-t0)/real(n)
    write(msgString,*) "perfLogFoundError: ", n, " iterations took", t1-t0, &
      " seconds. => ", dt, " per iteration."
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return
    ! indicate success
    rc=ESMF_SUCCESS
  end subroutine
  !--------------------------------------------------------------------------

end program ESMF_LogErrPerfUTest
