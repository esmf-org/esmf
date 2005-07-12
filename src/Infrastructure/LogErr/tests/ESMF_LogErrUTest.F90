! $Id: ESMF_LogErrUTest.F90,v 1.24.2.1 2005/07/12 20:25:08 svasquez Exp $
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
      '$Id: ESMF_LogErrUTest.F90,v 1.24.2.1 2005/07/12 20:25:08 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name, msg_type, Pet_num
      character(10) :: log_time, my_time
      character(1) :: pet_char
      character(4) :: my_pet_char
      character(8) :: my_todays_date, todays_date
      integer :: rndseed(1), v(8)
      

      character :: random_char
      character (9) :: random_string, msg_string
      character (5) :: random_chars

!     !LOCAL VARIABLES:
      integer :: rc2, ran_num, i, j, input_status, my_pet
      real :: r1
      logical :: is_error
      type(ESMF_Log) :: log1, log2
      type(ESMF_VM):: vm


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

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test LogFlush of new unopened log
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogFlush of unopened log Test"
      call ESMF_LogFlush(log2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !This test crashes, 1170027 bug open, commented out
      ! Test Log Write without opening log file
      !write(failMsg, *) "Did not return ESMF_SUCCESS"
      !call ESMF_LogWrite(log=log2, msg="Log Write One",msgtype=ESMF_LOG_INFO)
      !write(name, *) "Write without opening log file Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Open
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogOpen(log2, "Log Test File 2", rc=rc)
      write(name, *) "Open Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log reopenOpen 
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogOpen(log2, "Log_Test_File_3", rc=rc)
      write(name, *) "Open Already Open Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      ! Get the local PET number
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=my_pet, rc=rc)
      ! Loop according to Pet Number so each PET has a different time
      do i=1, (((my_pet+1)*7) * 116931)
         ! This call put here to waste time
     	 call date_and_time(date=my_todays_date, time=my_time)
      end do
      ! Generate a random string using clock as seed and write it to log file
      call date_and_time(values=v(:))
      rndseed(1)=v(8)*v(7)+1
      print *, "rndseed= " , rndseed(1)
      call random_seed(put=rndseed)
      do i=1, 5
      	call random_number(r1)
      	ran_num = int(26.0*r1) + 65
      	random_char  = achar(ran_num)
	random_chars(i:i) = random_char
      end do
	print *, "Random string is ", random_chars

      ! Convert PET to character
      pet_char  = achar(my_pet + 48)
      ! Append to "PET"
      my_pet_char = "PET" // pet_char
      random_string = my_pet_char // random_chars

      !EX_UTest
      ! Test Log Write 
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      ! Get date and time to compare later in the test
      call date_and_time(date=my_todays_date, time=my_time)
      call ESMF_LogWrite(log=log2, msg=random_string,msgtype=ESMF_LOG_INFO,rc=rc)
      write(name, *) "Write to log file Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test LogFlush of log
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogFlush Test"
      call ESMF_LogFlush(log2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Verify that the correct string was written to the file
      !EX_UTest
      rc = ESMF_FAILURE
      input_status = 0
      open (unit=1, file = "Log_Test_File_3", action = "read", form = "formatted")
      do
      	read (1, *) todays_date, log_time, msg_type, Pet_num, msg_string
        if (input_status < 0) then
		exit
        else 
	    if (msg_string.eq.random_string) then
	    	rc = ESMF_SUCCESS
		exit
	    endif
	endif
      end do
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " Verify LogFlush Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc
      print *, "msg_string is ", msg_string


      !------------------------------------------------------------------------
      !EX_UTest
      ! Verify correct date was written to log file
      write(failMsg, *) "Date in file is wrong"
      write(name, *) "Verify date in Log File Test"
      call ESMF_Test((my_todays_date.eq.todays_date), name, failMsg, result, ESMF_SRCLINE)
      print *, " my_todays_date is ", my_todays_date
      print *, " todays_date is ", todays_date


      !------------------------------------------------------------------------
      !Commented out because it is not a reliable test
      !TODO - Find a way to compare an error value
      ! Verify correct time was written to log file
      !write(failMsg, *) "time in file is wrong"
      !write(name, *) "Verify time in Log File Test"
      !call ESMF_Test((my_time.eq.log_time), name, failMsg, result, ESMF_SRCLINE)
      print *, " my_time = ", my_time
      print *, " log_time = ", log_time
      

      !------------------------------------------------------------------------
      !EX_UTest
      ! Verify correct message type was written to log file
      write(failMsg, *) "Message type in file is wrong"
      write(name, *) "Verify Message Type in Log File Test"
      call ESMF_Test((msg_type.eq."INFO"), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Verify correct PET was written into log file
      write(failMsg, *) "PET number in file is wrong"
      write(name, *) "Verify PET number in Log File Test"
      call ESMF_Test((my_pet_char.eq.Pet_num), name, failMsg, result, ESMF_SRCLINE)
      print *, " My PET char is ", my_pet_char
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Close
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogClose(log2, rc)
      write(name, *) "Close Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test LogFlush of new unopened log
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogFlush of unopened log Test"
      call ESMF_LogFlush(log2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)


      end program ESMF_LogErrUTest
