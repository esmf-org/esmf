! $Id: ESMF_DELayoutWorkQueueUTest.F90,v 1.5 2007/03/31 05:51:00 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!

module ESMF_DELayoutWQUTest_mod

  use ESMF_Mod

  implicit none
  
  public mygcomp_register_withoutthreads
  public mygcomp_register_withthreads
    
  contains !--------------------------------------------------------------------

  subroutine mygcomp_register_withoutthreads(gcomp, rc)
    type(ESMF_GridComp), intent(inout):: gcomp
    integer, intent(out):: rc
    
    print *, "*** hi from mygcomp_register ***"
    
    ! Run this VM default mode: single-threaded
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, mygcomp_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    
    rc = ESMF_SUCCESS      
  end subroutine !--------------------------------------------------------------
  
  subroutine mygcomp_register_withthreads(gcomp, rc)
    type(ESMF_GridComp), intent(inout):: gcomp
    integer, intent(out):: rc
    
    print *, "*** hi from mygcomp_register ***"
    
    ! Run this VM as multi-threaded as resources will allow
    call ESMF_GridCompSetVMMaxThreads(gcomp, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, mygcomp_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    
    rc = ESMF_SUCCESS      
  end subroutine !--------------------------------------------------------------
  
  recursive subroutine mygcomp_run(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp), intent(inout):: gcomp
    type(ESMF_State), intent(in):: istate, estate
    type(ESMF_Clock), intent(in):: clock
    integer, intent(out):: rc
    
    type(ESMF_VM):: vm
    type(ESMF_DELayout):: delayout
    integer:: petCount, localPet, localDeCount, i, workDe, k, deCount
    integer:: localrc ! absoft refuses to use the dummy variable rc in functions
    integer, allocatable:: localDeList(:)
    type(ESMF_DELayoutServiceReply):: reply
    real:: x
    

    print *, "*** hi from mygcomp_run ***"
    
    call ESMF_GridCompGet(gcomp, vm=vm)
!    call ESMF_VMPrint(vm, rc)

    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)

    deCount = 10*petCount
    delayout = ESMF_DELayoutCreate(deCount=deCount, &
      dePinFlag=ESMF_DE_PIN_VAS, rc=localrc)
    rc = localrc
    
!    call ESMF_DELayoutPrint(delayout, rc=rc)

    call ESMF_DELayoutGet(delayout, vasLocalDeCount=localDeCount, rc=rc)
    allocate(localDeList(localDeCount))
    call ESMF_DELayoutGet(delayout, vasLocalDeList=localDeList, rc=rc)
    
  do k=1, 4
  
    do i=1, localDeCount
      workDe = localDeList(i)
!      print *, "I am PET", localPET, " and I am offering service for DE ", workDe
      reply = ESMF_DELayoutServiceOffer(delayout, de=workDe, rc=localrc)
      rc = localrc
      if (reply == ESMF_DELAYOUT_SERVICE_ACCEPT) then
!        print *, "I am PET", localPET, ", service offer for DE ", workDe, &
!          " was accepted."
        call work(x, workDe, petCount)  ! work for workDe
!        print *, "x = ", x
        call ESMF_DELayoutServiceComplete(delayout, de=workDe, rc=rc)
!        print *, "I am PET", localPET, ", service for DE ", workDe, &
!          " completed."
      endif
    enddo
    
  enddo    
    
    deallocate(localDeList)
    
    call ESMF_DELayoutDestroy(delayout, rc=rc)

    rc = ESMF_SUCCESS
  end subroutine !--------------------------------------------------------------


  recursive subroutine work(x, de, petCount)
    real:: x, zend
    integer:: de, petCount
    
    real:: z, de_ratio, random
    
    zend = 10000.
    de_ratio = 2.* 3.1415 * de/petCount
    
    call random_number(random)
    zend = zend * (1. + 10. * sin(de_ratio) * random)
    
    x=0.    
    do z=0., zend, 0.01
      x = x + sin(z) * de
    enddo
    
  end subroutine


end module



program ESMF_DELayoutWQUTest

  !---------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

  !============================================================================
  !BOP
  ! !PROGRAM: ESMF_DELayoutTest - This unit test file verifies DELayout methods.
  !
  ! !DESCRIPTION:
  !
  ! The code in this file drives F90 DELayout work queue unit test.
  !
  !---------------------------------------------------------------------------
  ! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  use ESMF_DELayoutWQUTest_mod

  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_GridComp):: gcomp
  type(ESMF_Clock):: dummyclock
  type(ESMF_State):: dummystate
  real(ESMF_KIND_R8):: timeStart, timeEnd
  type(ESMF_VM):: vm
  integer:: localPet
  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_VMGetGlobal(vm, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !----------------- test without threads ----------------------------


  gcomp = ESMF_GridCompCreate(name="gridded component", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(gcomp, mygcomp_register_withoutthreads, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !------------------------------------------------------------------------
  !NEX___UTest Disabled see bug 1489171
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Run work queue without threads Test"
  call ESMF_VMWtime(timeStart)
  call ESMF_GridCompRun(gcomp, dummystate, dummystate, dummyclock, rc=rc)
  call ESMF_VMWtime(timeEnd)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (rc.ne.ESMF_SUCCESS) goto 10
  
  print *, "<without threads> PET ", localPet, " time: ", timeEnd-timeStart
  
  call ESMF_GridCompDestroy(gcomp, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !----------------- test with threads -------------------------------


  gcomp = ESMF_GridCompCreate(name="gridded component", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridCompSetServices(gcomp, mygcomp_register_withthreads, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
  !------------------------------------------------------------------------
  !NEX___UTest Disabled see bug 1489171
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Run work queue with threads Test"
  call ESMF_VMWtime(timeStart)
  call ESMF_GridCompRun(gcomp, dummystate, dummystate, dummyclock, rc=rc)
  call ESMF_VMWtime(timeEnd)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (rc.ne.ESMF_SUCCESS) goto 10

  print *, "<with threads> PET ", localPet, " time: ", timeEnd-timeStart

  call ESMF_GridCompDestroy(gcomp, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
 
10 continue
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  
end program
