! $Id: ESMF_LogErrUTest.F90,v 1.10 2005/03/09 16:17:36 nscollins Exp $
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
      program ESMF_LogErrUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>
#include <ESMF.h>

!==============================================================================
!BOP
! !PROGRAM: ESMF_LogErrTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 LogErr unit tests.
! The companion file ESMF\_LogErr.F90 contains the definitions for the
! LogErr methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_LogErrUTest.F90,v 1.10 2005/03/09 16:17:36 nscollins Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      integer :: rc2
      logical :: is_error
      type(ESMF_Log) :: log1

!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------
      print *, "Starting LogErr Tests"

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Open
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogOpen(log1, "Log Test File", rc=rc)
      write(name, *) "Open Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogWrite(log=log1, msg="Log Write 2",msgtype=ESMF_LOG_INFO)
      write(name, *) "Use of separate log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Close
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogClose(log1, rc)
      write(name, *) "Close Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc


#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogWrite(msg="Log Write 2",msgtype=ESMF_LOG_INFO)
      ! TODO:  this should be the format, with an rc argument
      !call ESMF_LogWrite(msg="Log Write 2",msgtype=ESMF_LOG_INFO,rc=rc)
      write(name, *) "Use of default log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Error
      write(failMsg, *) "Did not return FALSE"
      is_error=ESMF_LogMsgFoundError(ESMF_FAILURE,"hello",rcToReturn=rc2)
      write(name, *) "Error Msg Found Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Error
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      is_error=ESMF_LogMsgFoundError(ESMF_SUCCESS,"hello",rcToReturn=rc2)
      write(name, *) "Error Msg Found Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Alloc Error
      write(failMsg, *) "Did not return ESMF_FAILURE"
      is_error=ESMF_LogFoundAllocError(ESMF_FAILURE,rcToReturn=rc2)
      write(name, *) "Log Found Alloc Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_RC_MEM"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_RC_MEM), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Alloc Error
      write(failMsg, *) "Did not return ESMF_FAILURE"
      is_error=ESMF_LogMsgFoundAllocError(ESMF_FAILURE,"hello",rcToReturn=rc2)
      write(name, *) "Error Msg Found Alloc Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_RC_MEM), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Error
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      is_error=ESMF_LogMsgFoundAllocError(ESMF_SUCCESS,"hello",rcToReturn=rc2)
      write(name, *) "Error Msg Found Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Alloc Error
      write(failMsg, *) "Did not return ESMF_FAILURE"
      is_error=ESMF_LogFoundAllocError(ESMF_FAILURE,rcToReturn=rc2)
      write(name, *) "Log Found Alloc Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_RC_MEM"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_RC_MEM), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Alloc Error
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      is_error=ESMF_LogFoundAllocError(ESMF_SUCCESS,rcToReturn=rc2)
      write(name, *) "Log Found Alloc Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Error
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      is_error=ESMF_LogFoundError(ESMF_SUCCESS,rcToReturn=rc2)
      write(name, *) "Log Found Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc
      !------------------------------------------------------------------------


      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Error
      write(failMsg, *) "Did not return ESMF_FAILURE"
      is_error=ESMF_LogFoundError(ESMF_FAILURE,rcToReturn=rc2)
      write(name, *) "Log Found Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)


      end program ESMF_LogErrUTest
