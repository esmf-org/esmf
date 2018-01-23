! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_MAPL_GenericSetServicesUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_MAPL_GenericSetServicesUTest - Tests MAPL's generic SS
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use MAPL_Mod, only: MAPL_GenericSetServices
  
implicit none

!------------------------------------------------------------------------------
! The following line turns a repository identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! Local variables
!  type(ESMF_VM)         :: vm
  integer               :: rc, urc
  type(ESMF_GridComp)   :: gridComp
  type(ESMF_State)      :: importState, exportState
  type(ESMF_Clock)      :: clock
  type(ESMF_Time)       :: startTime, stopTime
  type(ESMF_TimeInterval) :: timeStep

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! get global VM
!  call ESMF_VMGetGlobal(vm, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  

!------------------------------------------------------------------------

  call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_TimeSet(startTime, yy=2016, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_TimeSet(stopTime, yy=2017, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_TimeIntervalSet(timeStep, s=3600*24, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  clock = ESMF_ClockCreate(name='MyClock', timeStep=timeStep, &
       startTime=startTime, stopTime=stopTime, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  gridComp = ESMF_GridCompCreate(name='MAPL Component', &
       configfile="MAPL.rc", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(gridComp, MAPL_GenericSetServices, &
       userrc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  importState = ESMF_StateCreate(name="MAPLimport", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  exportState = ESMF_StateCreate(name="MAPLexport", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(gridComp, importState=importState, &
       exportState=exportState, clock=clock, userrc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do while (.not. ESMF_ClockIsDone(clock))
     call ESMF_GridCompRun(gridComp, importState=importState, &
       exportState=exportState, clock=clock, userrc=urc, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     call ESMF_ClockAdvance(clock, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo

  call ESMF_GridCompFinalize(gridComp, importState=importState, &
       exportState=exportState, clock=clock, userrc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  write(name, *) "MAPL_GenericSetServices test"
  write(failMsg, *) "Failed"
  ! if we get here before abort, test passes
  call ESMF_Test(.true., name, failMsg, result, ESMF_SRCLINE)
 
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


end program ESMF_MAPL_GenericSetServicesUTest
