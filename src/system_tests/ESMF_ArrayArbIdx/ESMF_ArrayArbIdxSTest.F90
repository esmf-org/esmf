! $Id: ESMF_ArrayArbIdxSTest.F90,v 1.1 2007/06/12 21:28:58 dneckels Exp $
!
!-------------------------------------------------------------------------
!SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test ArrayArbIdx
!     Test the use of 1D arrays with 'user supplied' arbitrary indexing for
!  the array entries.
!
!\begin{verbatim}

program ArrayArbIdx

#include <ESMF_Macros.inc>

! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: testresult = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg


  type(ESMF_VM) :: vm
  type(ESMF_DISTGRID) :: distgrid
  integer :: rc, myPet, nPets
  integer :: indices(20)

  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_VMGet(vm, localPet=myPet, petCount=nPets, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  do i=1,20
    indices(i) = myPet*20 + i
  enddo 

  distgrid = ESMF_DistGridCreate(indices, vm, rc)

  call ESMF_DistGridPrint(distgrid)


10 continue
  print *, "System Test ArrayArbIdx complete."

  ! Normal ESMF Test output
  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ArrayArbIdx"

  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  if ((localPet .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
    ! Separate message to console, for quick confirmation of success/failure
    if (rc .eq. ESMF_SUCCESS) then
      write(finalMsg, *) "SUCCESS: ArrayArbIdx test finished correctly."
    else
      write(finalMsg, *) "System Test did not succeed.  Error code ", rc
    endif
    write(0, *) ""
    write(0, *) trim(testname)
    write(0, *) trim(finalMsg)
    write(0, *) ""

  endif

  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"
  print *, "Test finished, localPet = ", localPet
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"





  call ESMF_Finalize()


end program ArrayArbIdx
    
!\end{verbatim}
    
