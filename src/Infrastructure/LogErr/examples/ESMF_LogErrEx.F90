! $Id: ESMF_LogErrEx.F90,v 1.7 2004/06/18 10:44:16 nscollins Exp $
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
!	Writing messages to log with different methods.
!	Add example string when put being built
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
    integer :: rc1,rc2
    ! function return variables
    logical :: ret
    ! a log object that is not the default log
    type(ESMF_LOG):: alog
!EOC
!BOE
!\subsubsection{Default Log}

! This example shows how to use the default log.  This example does not use cpp
! macros.
!EOE
!BOC
    ! Initialize ESMF to initialize the default log
    call ESMF_Initialize(rc=rc1)

    ! LogWrite 
    ret= ESMF_LogWrite("Log Write 2",ESMF_LOG_INFO)
    ! LogMsgFoundError
    ret = ESMF_LogMsgFoundError(ESMF_FAILURE,"hello",rcToReturn=rc2)
    ! LogMsgFoundAllocError
    ret = ESMF_LogFoundAllocError(ESMF_FAILURE,rcToReturn=rc2)
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
    ! LogWrite
    ret= ESMF_LogWrite("Log Write 2", ESMF_LOG_INFO, ESMF_CONTEXT,log=alog)
    ! LogMsgFoundError
    ret = ESMF_LogMsgFoundError(ESMF_FAILURE,"hello", ESMF_CONTEXT, &
          rcToReturn=rc2,log=alog)
    ! LogMsgFoundAllocError
    ret = ESMF_LogFoundAllocError(ESMF_FAILURE, ESMF_CONTEXT, rc2,alog)
    ! Close the log.
    call ESMF_LogClose(alog)

    ! Finalize ESMF to close the default log
    call ESMF_Finalize(rc=rc1)
end program
!EOC
