! $Id: ESMF_LogErrEx.F90,v 1.24.2.1 2005/02/09 18:49:30 nscollins Exp $
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
    integer :: rc1,rc2,rc3,rcToTest,allocRcToTest
    ! a log object that is not the default log
    type(ESMF_LOG):: alog
    type(ESMF_LogType) :: defaultLogtype
    type(ESMF_Time) :: time
    integer, pointer :: intptr(:)
!EOC
    integer :: finalrc
    finalrc = ESMF_SUCCESS
!BOE
!\subsubsection{Default Log}

! This example shows how to use the default Log.  This example does not use cpp
! macros but does use multi Logs.  A separate Log will be created for each PET.
!EOE
!BOC
    ! Initialize ESMF to initialize the default Log
    call ESMF_Initialize(rc=rc1,defaultlogtype=ESMF_LOG_MULTI)
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
! This example shows how to use a user created Log.  This example uses cpp macros.
! For this example, a single Log is used so all PETs write to the same Log.
!EOE
!BOC
! File define
! Method define
#define ESMF_FILE "ESMF_LogErrEx File"
! Method define
#define ESMF_METHOD "ESMF_LogErrEx Method"
    ! Open a Log named "Testlog.txt" associated with alog.
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

!BOE
!\subsubsection{Get and Set}
! This example shows how to use Get and Set routines, on both the default Log as
! as the user created Log from the previous example.
!EOE

!BOC
    ! This is an example showing a query of the default Log.  Please note that no
    ! Log is passed in the argument list, so the default Log will be used.
    call ESMF_LogGet(logtype=defaultLogtype, rc=rc3)
!EOC
    if (rc3.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

!BOC
    ! This is an example setting a property of a Log that is not the default.
    ! It was opened in the previous example, and the handle for it must be
    ! passed in argument list.
    call ESMF_LogSet(log=alog, halt=ESMF_LOG_HALTERROR, rc=rc3)
!EOC
    if (rc3.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    ! Finalize ESMF to close the default log
    call ESMF_Finalize(rc=rc1)
    if (rc1.NE.ESMF_SUCCESS) then
        finalrc = ESMF_FAILURE
    end if

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_LogErrEx.F90"
    else
        print *, "FAIL: ESMF_LogErrEx.F90"
    end if

end program
