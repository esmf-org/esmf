! $Id: ESMF_ArraySparseMatMulSTest.F90,v 1.9 2007/06/25 06:02:05 theurich Exp $
!
!-------------------------------------------------------------------------
!SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test ArraySparseMatMul.  
!    Two gridded components and one coupler component, one-way coupling.
!
!    First gridded component runs on 4 PETs and defines a 2D source Array 
!    100x150. Second gridded component defines a destination Array also 
!    100x150 but runs on only 2 PETs. Both gridded components use DELayouts 
!    with 1 DE per PET. The decomposition of the source Array is defined as 
!    (petCount x 1) = (4 x 1) while the destination Array is decomposed as 
!    (1 x petCount) = (1 x 2).
!
!    The first component initializes the source Array to a geometric function:
!
!       10.0 + 5.0*sin((I/Imax)*pi) + 2.0*sin((J/Jmax)*pi)
!
!    The coupler component runs on all 6 PETs and reconciles import and export
!    states which contain source and destination Array, respectively. The 
!    coupler component then calls ArraySparseMatMul() using the identity matrix.
!    This amounts to a redistribution of the source Array data onto the 
!    destination Array.
!    
!    Finally the second gridded component compares the data stored in the
!    destination Array to the exact solution of the above function as a measure
!    of the accuracy of the ArraySparseMat() method.
!
!\begin{verbatim}

program ArraySparseMatMul

#include <ESMF_Macros.inc>

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  use user_model1, only : userm1_register
  use user_model2, only : userm2_register
  use user_coupler, only : usercpl_register

  implicit none
    
  ! Local variables
  integer :: localPet, petCount, rc
  character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
  type(ESMF_VM):: vm
  type(ESMF_State) :: c1exp, c2imp
  type(ESMF_GridComp) :: comp1, comp2
  type(ESMF_CplComp) :: cpl

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: testresult = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  print *, "--------------------------------------- "
  print *, "Start of System Test ArraySparseMatMul: "
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

  if (petCount .lt. 6) then
    print *, "This system test needs to run at least 6-way, petCount = ", &
      petCount
    goto 10
  endif

  ! Create the 2 model components and coupler
  cname1 = "user model 1"
  ! use petList to define comp1 on PET 0,1,2,3
  comp1 = ESMF_GridCompCreate(name=cname1, petList=(/0,1,2,3/), rc=rc)
  print *, "Created component ", trim(cname1), "rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  !  call ESMF_GridCompPrint(comp1, "", rc)

  cname2 = "user model 2"
  ! use petList to define comp2 on PET 4,5
  comp2 = ESMF_GridCompCreate(name=cname2, petList=(/4,5/), rc=rc)
  print *, "Created component ", trim(cname2), "rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  !  call ESMF_GridCompPrint(comp2, "", rc)

  cplname = "user one-way coupler"
  ! no petList means that coupler component runs on all PETs
  cpl = ESMF_CplCompCreate(name=cplname, rc=rc)
  print *, "Created component ", trim(cplname), ", rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
  !  call ESMF_CplCompPrint(cpl, "", rc)

  print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompSetServices(comp1, userm1_register, rc)
  print *, "Comp SetServices finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_GridCompSetServices(comp2, userm2_register, rc)
  print *, "Comp SetServices finished, rc= ", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_CplCompSetServices(cpl, usercpl_register, rc)
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
 
  c2imp = ESMF_StateCreate("comp2 import", ESMF_STATE_IMPORT, rc=rc)
  if (rc .ne. ESMF_SUCCESS) goto 10
  call ESMF_GridCompInitialize(comp2, importState=c2imp, rc=rc)
  print *, "Comp 2 Initialize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
 
  ! note that the coupler's import is comp1's export state
  ! and coupler's export is comp2's import state
  call ESMF_CplCompInitialize(cpl, c1exp, c2imp, rc=rc)
  print *, "Coupler Initialize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompRun(comp1, exportState=c1exp, rc=rc)
  print *, "Comp 1 Run returned, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_CplCompRun(cpl, c1exp, c2imp, rc=rc)
  print *, "Coupler Run returned, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_GridCompRun(comp2, importState=c2imp, rc=rc)
  print *, "Comp 2 Run returned, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_GridCompFinalize(comp1, exportState=c1exp, rc=rc)
  print *, "Comp 1 Finalize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_GridCompFinalize(comp2, importState=c2imp, rc=rc)
  print *, "Comp 2 Finalize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

  call ESMF_CplCompFinalize(cpl, c1exp, c2imp, rc=rc)
  print *, "Coupler Finalize finished, rc =", rc
  if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_StateDestroy(c1exp, rc)
  call ESMF_StateDestroy(c2imp, rc)

  call ESMF_GridCompDestroy(comp1, rc)
  call ESMF_GridCompDestroy(comp2, rc)
  call ESMF_CplCompDestroy(cpl, rc)

  print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

10 continue
  print *, "System Test ArraySparseMatMul complete."

  ! Normal ESMF Test output
  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ArraySparseMatMul"

  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  if ((localPet .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
    ! Separate message to console, for quick confirmation of success/failure
    if (rc .eq. ESMF_SUCCESS) then
      write(finalMsg, *) "SUCCESS: ArraySparseMatMul test finished correctly."
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

end program ArraySparseMatMul
    
!\end{verbatim}
    
