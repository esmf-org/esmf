! $Id: ESMF_LogErrEx.F90,v 1.15 2004/11/29 18:13:59 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!

    program ESMF_LogErrEx

!------------------------------------------------------------------------------
!EXAMPLE	Writing messages to log with different methods.
!==============================================================================
!BOC
! !PROGRAM: ESMF_LogErrEx - Log Error examples
!
! !DESCRIPTION:
!
! This program shows examples of Log Error writing
!-----------------------------------------------------------------------------

! Macros for cpp usage
#include "ESMF_LogMacros.inc"
! ESMF Framework module
    use ESMF_Mod
    implicit none
    
    ! return variables
    integer :: rc1,rc2,rcToTest,allocRcToTest
    ! a log object that is not the default log
    type(ESMF_LOG):: alog
    type(ESMF_Time) :: time
    integer, pointer :: intptr(:)
!EOC
    integer :: finalrc
    finalrc = ESMF_SUCCESS
!BOE
!\subsubsection{Default Log}

! This example shows how to use the default log.  This example does not use cpp
! macros.
!EOE
!BOC
    ! Initialize ESMF to initialize the default log
    call ESMF_Initialize(rc=rc1)
!EOC
    if (rc1.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    ! LogWrite 
    call ESMF_LogWrite("Log Write 2",ESMF_LOG_INFO,rc=rc2)
!EOC
    if (rc2.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    ! LogMsgSetError
    call ESMF_LogMsgSetError(ESMF_FAILURE,"Convergence failure",rcToReturn=rc2)

    ! LogMsgFoundError
    call ESMF_TimeSyncToRealTime(time, rcToTest)
    if (ESMF_LogMsgFoundError(rcToTest, "getting wall clock time", &
                              rcToReturn=rc2)) then
        ! Error getting time. The previous call will have printed the error
        ! already into the log file.  Add any additional error handling here.
        ! (This call is expected to provoke an error from the Time Manager.)
    endif

    ! LogMsgFoundAllocError
    allocate(intptr(10), stat=allocRcToTest)
    if (ESMF_LogMsgFoundAllocError(allocRcToTest,"integer array", rcToReturn=rc2)) then
        ! Error during allocation.  The previous call will have logged already
        ! an error message into the log.
    endif

    deallocate(intptr)
!EOC
!BOE
!\subsubsection{User Created Log}

! This example shows how to use a user created log.  This example uses cpp macros.
!EOE
!BOC
! File define
! Method define
#define ESMF_FILE "ESMF_LogErrEx File"
! Method define
#define ESMF_METHOD "ESMF_LogErrEx Method"
    ! Open a log named "Testlog.txt" associated with alog.
    call ESMF_LogOpen(alog, "TestLog.txt",rc=rc1)
!EOC
    if (rc1.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    ! LogWrite
    call ESMF_LogWrite("Log Write 2", ESMF_LOG_INFO, ESMF_CONTEXT,log=alog,rc=rc2)
!EOC
    if (rc2.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC
    ! LogMsgSetError
    call ESMF_LogMsgSetError(ESMF_FAILURE,"Interpolation Failure", ESMF_CONTEXT, &
          rcToReturn=rc2,log=alog)

    ! Close the log.
    call ESMF_LogClose(alog,rc2)
!EOC
    if (rc2.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if
!BOC

    ! Finalize ESMF to close the default log
    call ESMF_Finalize(rc=rc1)
!EOC
    if (rc1.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_LogErrEx.F90"
    else
        print *, "FAIL: ESMF_LogErrEx.F90"
    end if
!BOC

end program
!EOC
