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

module ESMF_DELayoutWQUTest_mod

  use ESMF

  implicit none
  
  public mygcomp_register
  public mygcomp_setvm_withthreads
    
  contains !--------------------------------------------------------------------

  recursive subroutine mygcomp_register(gcomp, rc)
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    rc = ESMF_SUCCESS

    !IO may not be thread-safe print *, "*** hi from mygcomp_register ***"
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      userRoutine=mygcomp_run, rc=rc)
      
    if (rc /= ESMF_SUCCESS) return  ! bail out
    
  end subroutine !--------------------------------------------------------------
  
  subroutine mygcomp_setvm_withthreads(gcomp, rc)
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    rc = ESMF_SUCCESS

    print *, "*** hi from mygcomp_setvm_withthreads ***"
    
    ! Run this VM as multi-threaded as resources will allow
    call ESMF_GridCompSetVMMaxThreads(gcomp, rc=rc)
    if (rc /= ESMF_SUCCESS) return  ! bail out
    
  end subroutine !--------------------------------------------------------------
  
  recursive subroutine mygcomp_run(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    type(ESMF_VM):: vm
    type(ESMF_DELayout):: delayout
    integer:: petCount, localPet, localDeCount, i, workDe, k, deCount
    integer, allocatable:: localDeToDeMap(:), petMap(:)
    type(ESMF_ServiceReply_Flag):: reply
    
    integer, parameter  :: workLoad = 10
    
    rc = ESMF_SUCCESS

!    print *, "*** hi from mygcomp_run ***"
    
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    deCount = workLoad * petCount
    allocate(petMap(deCount))
    do i=0, petCount-1
      petMap(i*workLoad+1:(i+1)*workLoad) = i
    enddo
    delayout = ESMF_DELayoutCreate(petMap=petMap, pinflag=ESMF_PIN_DE_TO_VAS, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
!    call ESMF_DELayoutPrint(delayout, rc=rc)
!    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_DELayoutGet(delayout, vasLocalDeCount=localDeCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    allocate(localDeToDeMap(localDeCount))
    call ESMF_DELayoutGet(delayout, vasLocalDeToDeMap=localDeToDeMap, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    do i=1, localDeCount
      workDe = localDeToDeMap(i)
!      print *, "I am PET", localPET, " and I am offering service for DE ", workDe
      reply = ESMF_DELayoutServiceOffer(delayout, de=workDe, rc=rc)
      if (rc/=ESMF_SUCCESS) return ! bail out
      if (reply == ESMF_SERVICEREPLY_ACCEPT) then
!        print *, "I am PET", localPET, ", service offer for DE ", workDe, &
!          " was accepted."
        call work(workDe, deCount)  ! work for workDe
        call ESMF_DELayoutServiceComplete(delayout, de=workDe, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
!        print *, "I am PET", localPET, ", service for DE ", workDe, &
!          " completed."
      endif
    enddo
    
    deallocate(localDeToDeMap, petMap)
    
    call ESMF_DELayoutDestroy(delayout, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------

  recursive subroutine work(de, deCount)
    integer             :: de, deCount
    real(ESMF_KIND_R8)  :: dt
    
    ! very unbalanced work weight
    dt = 2.d0 * exp(-((de-deCount/2)**2)/8.)

!print *, "de=", de, "dt=", dt
    
    call ESMF_VMWtimeDelay(dt)    
  end subroutine

end module

!==============================================================================

program ESMF_DELayoutWQUTest

  !---------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

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
  use ESMF

  use ESMF_DELayoutWQUTest_mod

  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_GridComp):: gcomp
  real(ESMF_KIND_R8):: timeStart, timeEnd
  type(ESMF_VM):: vm
  logical:: pthreadsEnabled
  integer:: localPet
  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_VMGet(vm, localPet=localPet, pthreadsEnabledFlag=pthreadsEnabled, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  !----------------- test without threads ----------------------------

  !NEX_UTest
  write(name, *) "GridCompCreate() - round 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gcomp = ESMF_GridCompCreate(name="myGridComp1", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "GridCompSetServices() - round 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompSetServices(gcomp, userRoutine=mygcomp_register, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Run work queue - round 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_VMWtime(timeStart)
  call ESMF_GridCompRun(gcomp, rc=rc)
  call ESMF_VMWtime(timeEnd)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  print *, "<round 1> PET ", localPet, " time: ", timeEnd-timeStart
  
  !NEX_UTest
  write(name, *) "GridCompDestroy() - round 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompDestroy(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------- test with threads -------------------------------

  !NEX_UTest
  write(name, *) "GridCompCreate() - round 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gcomp = ESMF_GridCompCreate(name="myGridComp2", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "GridCompSetVM() - round 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  if (pthreadsEnabled) then
    call ESMF_GridCompSetVM(gcomp, userRoutine=mygcomp_setvm_withthreads, rc=rc)
  else
    rc=ESMF_SUCCESS
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
  !NEX_UTest
  write(name, *) "GridCompSetServices() - round 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompSetServices(gcomp, userRoutine=mygcomp_register, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Run work queue - round 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_VMWtime(timeStart)
  call ESMF_GridCompRun(gcomp, rc=rc)
  call ESMF_VMWtime(timeEnd)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "<round 2> PET ", localPet, " time: ", timeEnd-timeStart

  !NEX_UTest
  write(name, *) "GridCompDestroy() - round 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompDestroy(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
#if 0
  call ESMF_VMLogCurrentGarbageInfo("Before Garbage Collection", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  !---------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  
end program
