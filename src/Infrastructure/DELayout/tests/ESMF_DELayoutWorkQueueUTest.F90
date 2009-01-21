! $Id: ESMF_DELayoutWorkQueueUTest.F90,v 1.9.2.9 2009/01/21 21:25:20 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    rc = ESMF_SUCCESS

    print *, "*** hi from mygcomp_register ***"
    
    ! Run this VM default mode: mpi-only, no threads
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, mygcomp_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc /= ESMF_SUCCESS) return  ! bail out
    
  end subroutine !--------------------------------------------------------------
  
  subroutine mygcomp_register_withthreads(gcomp, rc)
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    rc = ESMF_SUCCESS

    print *, "*** hi from mygcomp_register ***"
    
    ! Run this VM as multi-threaded as resources will allow
    call ESMF_GridCompSetVMMaxThreads(gcomp, rc=rc)
    if (rc /= ESMF_SUCCESS) return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, mygcomp_run, &
      ESMF_SINGLEPHASE, rc)
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
    integer, allocatable:: localDeList(:)
    type(ESMF_DELayoutServiceReply):: reply
    real:: x
    
    rc = ESMF_SUCCESS

!    print *, "*** hi from mygcomp_run ***"
    
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    deCount = 10*petCount
    delayout = ESMF_DELayoutCreate(deCount=deCount, &
      dePinFlag=ESMF_DE_PIN_VAS, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
!    call ESMF_DELayoutPrint(delayout, rc=rc)
!    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_DELayoutGet(delayout, vasLocalDeCount=localDeCount, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    allocate(localDeList(localDeCount))
    call ESMF_DELayoutGet(delayout, vasLocalDeList=localDeList, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    do k=1, 4
      do i=1, localDeCount
        workDe = localDeList(i)
!        print *, "I am PET", localPET, " and I am offering service for DE ", workDe
        reply = ESMF_DELayoutServiceOffer(delayout, de=workDe, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
        if (reply == ESMF_DELAYOUT_SERVICE_ACCEPT) then
!          print *, "I am PET", localPET, ", service offer for DE ", workDe, &
!            " was accepted."
          call work(x, workDe, petCount)  ! work for workDe
!          print *, "x = ", x
          call ESMF_DELayoutServiceComplete(delayout, de=workDe, rc=rc)
          if (rc/=ESMF_SUCCESS) return ! bail out
!          print *, "I am PET", localPET, ", service for DE ", workDe, &
!            " completed."
        endif
      enddo
    enddo    
    
    deallocate(localDeList)
    
    call ESMF_DELayoutDestroy(delayout, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine !--------------------------------------------------------------


  recursive subroutine work(x, de, petCount)
    real:: x, zend
    integer:: de, petCount, i, iend
    
    real:: z, de_ratio, random
    
    zend = 10000.
    de_ratio = 2.* 3.1415 * de/petCount
    
    call random_number(random)
    zend = zend * (1. + 10. * sin(de_ratio) * random)
    
    x=0.
    z=0.
    iend = zend/0.01
    do i=0, iend
      x = x + sin(z) * de
      z = z + 0.01
    enddo
    
  end subroutine


end module



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
  use ESMF_Mod

  use ESMF_DELayoutWQUTest_mod

  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_GridComp):: gcomp
  real(ESMF_KIND_R8):: timeStart, timeEnd
  type(ESMF_VM):: vm
  type(ESMF_Logical):: supportPthreads
  integer:: localPet
  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_VMGet(vm, localPet=localPet, supportPthreadsFlag=supportPthreads, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !----------------- test without threads ----------------------------

  !NEX_UTest
  write(name, *) "GridCompCreate() - round 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gcomp = ESMF_GridCompCreate(name="gridded component", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "GridCompSetServices() - round 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompSetServices(gcomp, mygcomp_register_withoutthreads, rc)
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
  write(name, *) "Run work queue - round 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompDestroy(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !----------------- test with threads -------------------------------

  !NEX_UTest
  write(name, *) "GridCompCreate() - round 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gcomp = ESMF_GridCompCreate(name="gridded component", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "GridCompSetServices() - round 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  if (supportPthreads==ESMF_TRUE) then
    call ESMF_GridCompSetServices(gcomp, mygcomp_register_withthreads, rc)
  else
    call ESMF_GridCompSetServices(gcomp, mygcomp_register_withoutthreads, rc)
  endif
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
  write(name, *) "Run work queue - round 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridCompDestroy(gcomp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
 
  !---------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  
end program
