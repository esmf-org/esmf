! $Id: ESMF_LogErrUTest.F90,v 1.45.2.5 2009/01/21 21:25:22 cdeluca Exp $
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
      program ESMF_LogErrUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

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
      '$Id: ESMF_LogErrUTest.F90,v 1.45.2.5 2009/01/21 21:25:22 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name, msg_type, pet_num
      type(ESMF_Time) :: log_time, my_time
      type(ESMF_TimeInterval) :: time_diff, zero, one_sec
      character(1) :: pet_char
      character(4) :: my_pet_char
      character(8) :: todays_date
      character(10) :: todays_time
      character(80) :: filename
      integer :: my_v(8), log_v(8), k, init_sec
      integer, pointer :: rndseed(:)
      

      character :: random_char
      character (9) :: random_string, msg_string
      character (5) :: random_chars

      !LOCAL VARIABLES:
      integer :: rc2, ran_num, i, input_status, my_pet
      real :: r1
      logical :: is_error
      type(ESMF_Log) :: log1, log2, log3, log4, log5, log6, log7
      type(ESMF_VM):: vm
      type(ESMF_LogType) :: logtype


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
      call ESMF_CalendarSetDefault(ESMF_CAL_GREGORIAN, rc)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Open
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogOpen(log1, "Log_Test_File", rc=rc)
      write(name, *) "Open Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Open
      logtype = ESMF_LOG_SINGLE
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogOpen(log5, "Single_Log_File", logtype=logtype,  rc=rc)
      write(name, *) "Open ESMF_LOG_SINGLE Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc


      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogWrite(log=log5, msg="Log Single Msg",msgtype=ESMF_LOG_INFO, &
                         rc=rc)
      write(name, *) "Write to Single Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Open
      logtype = ESMF_LOG_NONE
      write(failMsg, *) "Did not return ESMF_FAILURE"
      call ESMF_LogOpen(log5, "None_Log_File", logtype=logtype,  rc=rc)
      write(name, *) "Open ESMF_LOG_NONE Log of opened file Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Open
      logtype = ESMF_LOG_NONE
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogOpen(log7, "None_Log_File", logtype=logtype,  rc=rc)
      write(name, *) "Open ESMF_LOG_NONE Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc


      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogWrite(log=log1, msg="Log Write 2",msgtype=ESMF_LOG_INFO, &
                         rc=rc)
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


#ifdef ESMF_TESTEXHAUSTIVE


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Close of never pened file
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogClose(log4, rc)
      write(name, *) "Close Log File of never opened file Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test LogFlush of an unopened log
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " LogFlush of unopened log Test"
      call ESMF_LogFlush(log6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogWrite(msg="Log Write 2",msgtype=ESMF_LOG_INFO,rc=rc)
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

!      !------------------------------------------------------------------------
      !EX_UTest
!      ! Test Log Found Alloc Error
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
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " LogFlush of unopened log Test"
      call ESMF_LogFlush(log2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Write without opening log file
      write(failMsg, *) "Returned ESMF_SUCCESS"
      call ESMF_LogWrite(log=log2, msg="Log Write One",msgtype=ESMF_LOG_INFO,rc=rc)
      write(name, *) "Write without opening log file Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Open
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogOpen(log2, "Log_Test_File_2", rc=rc)
      write(name, *) "Open Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log reopenOpen 
      write(failMsg, *) "Did not return ESMF_FAILURE"
      call ESMF_LogOpen(log2, "Log_Test_File_3", rc=rc)
      write(name, *) "Open Already Open Log Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------

      print *, "Starting a no-op loop to wait before testing time and date"
      ! Get the local PET number
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=my_pet, rc=rc)
      ! Loop according to Pet Number so each PET has a different time
      call date_and_time(values=my_v)
      init_sec = my_v(7)
      do i=1, (((my_pet+1)*7) * 116931)
         ! This call makes "todays_time" has a second difference between 
         ! pets.
         call date_and_time(values=my_v)
         if( (my_v(7)-init_sec) .ge. (my_pet+1) ) then
            call date_and_time(date=todays_date, time=todays_time)
            !write(*,*)"my_pet, diff ", my_pet, (my_v(7)-init_sec)
            exit
         endif
      end do
      print *, "Ending the no-op loop"

      ! Generate a random string using clock as seed and write it to log file
      call random_seed()
      call random_seed(size=k)
      print *, "size of random seed = ", k
      allocate(rndseed(k))
      call date_and_time(values=my_v)
      do i=1,k
        rndseed(i)=i*(my_v(6)+my_v(7))
      end do
      print *, "generated a random seed based on current time = " , rndseed
      call random_seed(put=rndseed(1:k))
      deallocate(rndseed)
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
      call date_and_time(values=my_v)
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
      !EX_UTest
      ! Test Log Close
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogClose(log2, rc)
      write(name, *) "Close Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      ! Verify that the file can be opened with Fortran IO
      !EX_UTest
      write(name, *) "Open Log file with Fortran IO Test"
      write(failMsg, *) "open() returned failure"
      filename = my_pet_char // ".Log_Test_File_2"
      open (unit=1, file = filename, action = "read", &
            form = "formatted", iostat = input_status)
      call ESMF_Test((input_status.eq.0), name, failMsg, result, ESMF_SRCLINE)
      print *, " filename = ", filename
      print *, " input_status = ", input_status
      if (input_status.ne.0) goto 100 ! cannot continue test without open file

      !------------------------------------------------------------------------
      ! Verify that the correct string was written to the file
      !EX_UTest
      rc = ESMF_FAILURE
      do
          read (1, *, iostat = input_status) todays_date, todays_time, &
                                             msg_type, Pet_num, msg_string
          if (input_status < 0) then
              exit
          endif 
          if (msg_string.eq.random_string) then
              rc = ESMF_SUCCESS
              exit
          endif
      end do
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Verify random string Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc
      print *, "msg_string is ", msg_string

      !------------------------------------------------------------------------
      !EX_UTest
      ! Verify correct date and time was written to log file (within 1 second)
      write(failMsg, *) "Date and/or Time in file is wrong"
      write(name, *) "Verify Date and Time in Log File Test"
      read(todays_date, 10) log_v(1), log_v(2), log_v(3)
   10 format(i4,2i2)
      read(todays_time, 20) log_v(5), log_v(6), log_v(7), log_v(8)
   20 format(3i2,1x,i3)
      call ESMF_TimeSet(my_time, yy=my_v(1), mm=my_v(2), dd=my_v(3), &
                        h=my_v(5), m=my_v(6), s=my_v(7), ms=my_v(8), rc=rc)
      call ESMF_TimeSet(log_time, yy=log_v(1), mm=log_v(2), dd=log_v(3), &
                        h=log_v(5), m=log_v(6), s=log_v(7), ms=log_v(8), rc=rc)
      time_diff = log_time - my_time
      call ESMF_TimeIntervalSet(zero, s=0, rc=rc)
      call ESMF_TimeIntervalSet(one_sec, s=1, rc=rc)
      call ESMF_Test((time_diff.ge.zero .and. time_diff.le.one_sec), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, " my_time is "
      call ESMF_TimePrint(my_time, "string", rc)
      print *, " log_time is "
      call ESMF_TimePrint(log_time, "string", rc)

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
      call ESMF_Test((lge(my_pet_char,pet_num).and.lle(my_pet_char,pet_num)), &
        name, failMsg, result, ESMF_SRCLINE)
      print *, " My PET char is ", my_pet_char, "and I read: ", pet_num
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test LogFlush of new unopened log
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " LogFlush of unopened log Test"
      call ESMF_LogFlush(log2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)

#endif
100   continue
      call ESMF_TestEnd(result, ESMF_SRCLINE)


      end program ESMF_LogErrUTest
