! $Id: ESMF_LogErrEx.F90,v 1.1 2004/06/09 05:19:34 cpboulder Exp $
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

#include "ESMF_LogMacros.inc"
! Method define
#define ESMF_METHOD "ESMF_LogErrEx"
! ESMF Framework module
    use ESMF_Mod
    !use ESMF_LogErrMod
    implicit none

    !type(ESMF_Log) :: aLog1
    real :: real_num
    integer :: rc1,rc2
    logical :: ret

    call ESMF_LogInitialize("aLog1.txt",rc=rc1)
    call ESMF_LogWrite("Log Write 2",ESMF_LOG_INFO)
    ret = ESMF_LogMsgFoundError(ESMF_FAILURE,"hello",rcToReturn=rc2)
    ret = ESMF_LogFoundAllocError(ESMF_FAILURE,rc2)
end program
