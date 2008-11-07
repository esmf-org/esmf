! $Id: ESMF_DistDirSTest.F90,v 1.6.2.3 2008/11/07 22:58:18 theurich Exp $
!
!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test DistDir
! A distributed directory object is created, both in a component and
! in the full application.  The object is tested for consistency in
! both settings.
!
!\begin{verbatim}

program DistDir

#include "ESMF_Macros.inc"

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  use user_model1, only : userm1_register

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, rc
  character(len=ESMF_MAXSTR) :: cname1
  type(ESMF_VM):: vm
  type(ESMF_State) :: c1exp
  type(ESMF_GridComp) :: comp1

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: testresult = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of System Test DistDir:           "
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
! Test in the full address space
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

  call cdistdir_test(vm, rc)
  if (rc .ne. ESMF_SUCCESS) goto 10

  ! Create the model component
  cname1 = "user model 1"
  ! use petList to define comp1 on PET 0,1,2,3
  comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1,2,3/), rc=rc)
  print *, "Created component ", trim(cname1), "rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  !  call ESMF_GridCompPrint(comp1, "", rc)

  print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetServices(comp1, userm1_register, rc)
  print *, "Comp SetServices finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
  c1exp = ESMF_StateCreate("comp1 export", ESMF_STATE_EXPORT, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompInitialize(comp1, exportState=c1exp, rc=rc)
  print *, "Comp 1 Initialize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompRun(comp1, exportState=c1exp, rc=rc)
  print *, "Comp 1 Run returned, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompFinalize(comp1, exportState=c1exp, rc=rc)
  print *, "Comp 1 Finalize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_StateDestroy(c1exp, rc)

  call ESMF_GridCompDestroy(comp1, rc)

  print *, "All Destroy routines finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue
  print *, "System Test DistDir complete."

  ! Normal ESMF Test output
  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test DistDir"

  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  if ((localPet .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
    ! Separate message to console, for quick confirmation of success/failure
    if (rc .eq. ESMF_SUCCESS) then
      write(finalMsg, *) "SUCCESS: DistDir test finished correctly."
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

  call ESMF_Finalize(rc=rc) 

end program DistDir
    
!\end{verbatim}
    
