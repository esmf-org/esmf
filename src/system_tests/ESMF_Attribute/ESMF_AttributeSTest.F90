!-------------------------------------------------------------------------
!SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test Attribute.  
!     
!\begin{verbatim}

program ESMF_AttributeSTest
#define ESMF_METHOD "program ESMF_AttributeSTest"

#include "ESMF.h"
#include <ESMF_Macros.inc>

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod  

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, rc
  type(ESMF_VM):: vm
  type(ESMF_State) :: state1
  character(len=ESMF_MAXSTR) :: state1name, name, value
  character(len=ESMF_MAXSTR) :: conv, purp, obj

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: testresult = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ESMF_AttributeSTest"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of ", trim(testname)
  print *, "--------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
  ! Initialize framework and get back default global VM
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

  ! Get number of PETs we are running with
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, '-------------------------------Initialize----------------------------------'

  state1name = 'State 1'
  name = 'name'
  value = 'NCAR'
  conv = 'ESG-CDP'
  purp = 'general'
  obj = 'state'

  state1 = ESMF_StateCreate('comp1_import', ESMF_STATE_IMPORT, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_StateCreateAttPack(state1, convention=conv, purpose=purp, object=obj, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10
  
  call ESMF_StateSetAttPack(state1, name, value, convention=conv, purpose=purp, object=obj, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10
  
  call ESMF_StatePrint(state1, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, '---------------------------------Destroy------------------------------------'

  ! Destroy the states
  call ESMF_StateDestroy(state1, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10
  
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue
  
  ! Normal ESMF Test output
  print *, testname, " complete."

  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  if ((localPet .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
    ! Separate message to console, for quick confirmation of success/failure
    write(finalMsg, *) "SUCCESS: ",trim(testname)," finished correctly."
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

end program ESMF_AttributeSTest
    
!\end{verbatim}
    
