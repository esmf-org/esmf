! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

module producerMod
  use ESMF
  implicit none
  private
  
  public producerReg
  
  contains
  
  subroutine producerReg(gcomp, rc)
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, init, rc=rc)
  end subroutine
  
!-------------------------------------------------------------------------
!BOE
!\subsubsection{Producer Component attaches user defined method}
!      
!  The producer Component attaches a user defined method to {\tt exportState}
!  during the Component's initialize method. The user defined method is
!  attached with label {\tt finalCalculation} by which it will become
!  accessible to the consumer Component.
!EOE
!BOC
  subroutine init(gcomp, importState, exportState, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: importState, exportState
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    call ESMF_MethodAdd(exportState, label="finalCalculation", &
      userRoutine=finalCalc, rc=rc)

    rc = 0
  end subroutine !--------------------------------------------------------------
!EOC

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Producer Component implements user defined method}
!      
!  The producer Component implements the attached, user defined method
!  {\tt finalCalc}. Strict interface rules apply for the user defined
!  method.
!EOE
!BOC
  subroutine finalCalc(state, rc)
    ! arguments
    type(ESMF_State):: state
    integer, intent(out):: rc

    ! access data objects in state and perform calculation
    
    print *, "dummy output from attached method "

    rc = 0
  end subroutine !--------------------------------------------------------------
!EOC

end module



module consumerMod
  use ESMF
  implicit none
  private
  
  public consumerReg
  
  contains
  
  subroutine consumerReg(gcomp, rc)
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, init, rc=rc)
  end subroutine
  
!-------------------------------------------------------------------------
!BOE
!\subsubsection{Consumer Component executes user defined method}
!      
!  The consumer Component executes the user defined method on the
!  {\tt importState}.
!
!EOE
!BOC
  subroutine init(gcomp, importState, exportState, clock, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: importState, exportState
    type(ESMF_Clock):: clock
    integer, intent(out):: rc
    
    integer:: userRc
    
    call ESMF_MethodExecute(importState, label="finalCalculation", &
      userRc=userRc, rc=rc)

    rc = 0
  end subroutine !--------------------------------------------------------------
!EOC


end module

program ESMF_AttachMethodsEx

!==============================================================================
! !PROGRAM: ESMF_AttachMethodsEx - Demonstrate Attachable Methods API
!
! !DESCRIPTION:
!
! This program shows examples of Attachable Methods.
!-----------------------------------------------------------------------------
#include "ESMF.h"

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod
  use producerMod
  use consumerMod
  implicit none

  ! Local variables
  integer :: rc
  
  type(ESMF_GridComp):: producer, consumer
  type(ESMF_State):: state

  
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_AttachMethodsEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS


  call ESMF_Initialize(defaultlogfilename="AttachMethodsEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  producer = ESMF_GridCompCreate(name="producer", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  consumer = ESMF_GridCompCreate(name="consumer", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(producer, userRoutine=producerReg, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(consumer, userRoutine=consumerReg, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  state = ESMF_StateCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(producer, exportState=state, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(consumer, importState=state, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(producer, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(consumer, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(state, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_AttachMethodsEx.F90"
  else
    print *, "FAIL: ESMF_AttachMethodsEx.F90"
  endif

end program ESMF_AttachMethodsEx
    
