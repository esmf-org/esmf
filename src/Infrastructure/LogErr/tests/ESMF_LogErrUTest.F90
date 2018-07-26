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
      program ESMF_LogErrUTest

!------------------------------------------------------------------------------

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
      use ESMF

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! test return codes
      integer :: rc, rcToReturn, desiredRc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name


      !LOCAL VARIABLES:
      type(ESMF_Log) :: log1, log5, log7, log_moe
      type(ESMF_LogKind_Flag) :: logkindflag
      character(4) :: my_pet_char
      integer :: my_pet, num_pets
      character(1) :: pet_char
      type(ESMF_VM):: vm
      logical :: ele, hasNc

#ifdef ESMF_TESTEXHAUSTIVE
      character(ESMF_MAXSTR) :: pet_num
      real :: r1
      logical :: is_error
      character(ESMF_MAXSTR) :: msg_type
      character(ESMF_MAXPATHLEN) :: filename, pet_filename
      integer :: ran_num, rc2, k, i
      integer :: ioerr
      integer :: datetime_commbuf(8)
      integer, allocatable :: rndseed(:)  ! cannot be pointer b/c absoft bug
      type(ESMF_Log) :: log2, log4, log6, log8
      type(ESMF_Log) :: log9, log9_alias
      character (5) :: random_chars
      character (9) :: msg_string, random_string
      character :: random_char
      integer :: my_v(8), log_v(8)
      character(8) :: todays_date
      character(10) :: todays_time
      type(ESMF_TimeInterval) :: one_sec, zero, time_diff
      type(ESMF_Time) :: my_time, log_time
      integer :: log8unit, moe_unit
      logical :: was_found
      logical :: flush_flag
      logical :: highRes_flag
      logical :: trace_flag
      type(ESMF_LogMsg_Flag), pointer :: logabort_flags(:)
      character(2) :: tooshortstr
      character(128), parameter :: json_string = &
          '{&
          &   "comp" :{&
          &     "event": "start_phase",&
          &     "phaseLabel": "IPDv01p2",&
          &     "phase": "0",&
          &   }&
          &}'

#endif

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
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, rc=rc)

      ! Get the local PET number
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=my_pet, petCount=num_pets, rc=rc)
      ! Convert PET to character
      pet_char  = achar(my_pet + 48)
      ! Append to "PET"
      my_pet_char = "PET" // pet_char

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
      logkindflag = ESMF_LOGKIND_SINGLE
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogOpen(log5, "Single_Log_File", logkindflag=logkindflag,  rc=rc)
      write(name, *) "Open ESMF_LOGKIND_SINGLE Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc


      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogWrite(log=log5, msg="Log Single Msg",logmsgFlag=ESMF_LOGMSG_INFO, &
                         rc=rc)
      write(name, *) "Write to Single Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Open
      logkindflag = ESMF_LOGKIND_NONE
      write(failMsg, *) "Did not return ESMF_RC_FILE_OPEN"
      call ESMF_LogOpen(log5, "None_Log_File", logkindflag=logkindflag,  rc=rc)
      write(name, *) "Open ESMF_LOGKIND_NONE Log of opened file Test"
      call ESMF_Test((rc.eq.ESMF_RC_FILE_OPEN), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Open
      logkindflag = ESMF_LOGKIND_NONE
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogOpen(log7, "None_Log_File", logkindflag=logkindflag,  rc=rc)
      write(name, *) "Open ESMF_LOGKIND_NONE Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogWrite(log=log1, msg="Log Write 2",logmsgFlag=ESMF_LOGMSG_INFO, &
                         rc=rc)
      write(name, *) "Use of separate log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Close
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogClose(log1, rc=rc)
      write(name, *) "Close Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

       ! -----------------------------------------------------------------------
       ! ESMF_LogFoundNetCDFError Testing

#if (defined ESMF_NETCDF || ESMF_PNETCDF)
        hasNc = .true.
#else
        hasNc = .false.
#endif

        ! ----------------------------------------------------------------------------
        !NEX_UTest
        rc = ESMF_RC_NOT_IMPL
        write(name, *) "ESMF_LogFoundNetCDFError Unit Test - No NetCDF error"
        write(failMsg, *) "NetCDF error code is a success code. Error checker should pass through."

        ele = ESMF_LogFoundNetCDFError(0)
        if (ele .and. hasNc) then
          rc = ESMF_FAILURE
        else
          rc = ESMF_SUCCESS
        endif
        call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, __FILE__, __LINE__)

        ! ----------------------------------------------------------------------------
        !NEX_UTest
        rc = ESMF_RC_NOT_IMPL
        write(name, *) "ESMF_LogFoundNetCDFError Unit Test - Found NetCDF error"
        write(failMsg, *) "NetCDF error not caught by error logging"

        rcToReturn = ESMF_SUCCESS
        ele = ESMF_LogFoundNetCDFError(1, line=2, file='what', &
                                       msg='message from a user', &
                                       method='my_method', rcToReturn=rcToReturn)
        if (ele) then
          rc = ESMF_SUCCESS
        else
          rc = ESMF_FAILURE
        endif

        if (hasNc) then
          desiredRc = ESMF_RC_NETCDF_ERROR
        else
          desiredRc = ESMF_RC_LIB_NOT_PRESENT
        endif
        if (rcToReturn .eq. desiredRc) then
          rc = ESMF_SUCCESS
        else
          rc = ESMF_FAILURE
        endif

        call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, __FILE__,&
                       __LINE__)

        ! ----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Close of never opened file
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogClose(log4, rc=rc)
      write(name, *) "Close Log File of never opened file Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test LogFlush of an unopened log
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " LogFlush of unopened log Test"
      call ESMF_LogFlush(log6, rc=rc)
      call ESMF_Test(rc /= ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogWrite(msg="Log Write 2",logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
      write(name, *) "Use of default log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test writing a JSON string
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "JSON output Test"
      call ESMF_LogWrite (msg=json_string, logmsgFlag=ESMF_LOGMSG_JSON, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Error
      write(failMsg, *) "Did not return .FALSE."
      is_error=ESMF_LogFoundError(ESMF_FAILURE, msg="hello",rcToReturn=rc2)
      write(name, *) "Error Msg Found Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test default value of tracing flag
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      write (name, *) "LogGet trace flag default flag"
      call ESMF_LogGet (trace=trace_flag, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS .and. .not. trace_flag,  &
          name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test turning on tracing for a while
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      write (name, *) "LogSet trace flag set to .true."
      call ESMF_LogSet (trace=.true., rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test to check that tracing was turned on
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      write (name, *) "LogGet trace flag set to .true."
      call ESMF_LogGet (trace=trace_flag, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS .and. trace_flag,  &
          name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Error
      write(failMsg, *) "Did not return .FALSE."
      is_error=ESMF_LogFoundError(ESMF_SUCCESS, msg="hello",  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Error Msg Found Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! AllocError and DeallocError tests
      !
      ! Note that the StatusToCheck arguments are Fortran status values, not
      ! ESMF rc values.  By the Fortran Standards, allocate and deallocate
      ! stat values are zero upon success, and non-zero on error.
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Alloc Error
      write(failMsg, *) "Did not return .TRUE."
      is_error=ESMF_LogFoundAllocError(statusToCheck=1,  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Log Found Alloc Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_RC_MEM_ALLOCATE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_RC_MEM_ALLOCATE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Alloc Error
      write(failMsg, *) "Did not return .TRUE."
      is_error=ESMF_LogFoundAllocError(statusToCheck=1, msg="hello",  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Error Msg Found Alloc Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_RC_MEM_ALLOCATE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_RC_MEM_ALLOCATE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Error
      write(failMsg, *) "Did not return .FALSE."
      is_error=ESMF_LogFoundAllocError(statusToCheck=0, msg="hello",  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Error Msg Found Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Alloc Error
      write(failMsg, *) "Did not return ESMF_FAILURE"
      is_error=ESMF_LogFoundAllocError(statusToCheck=1,  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Log Found Alloc Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_RC_MEM_ALLOCATE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_RC_MEM_ALLOCATE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Alloc Error
      write(failMsg, *) "Did not return .FALSE."
      rc2 = ESMF_FAILURE
      is_error=ESMF_LogFoundAllocError(statusToCheck=0,  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Log Found Alloc Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not leave return code unchanged"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Dealloc Error
      write(failMsg, *) "Did not return .TRUE."
      is_error=ESMF_LogFoundDeallocError(statusToCheck=1,  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Log Found Dealloc Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_RC_MEM_DEALLOCATE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_RC_MEM_DEALLOCATE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Dealloc Error
      write(failMsg, *) "Did not return .TRUE."
      is_error=ESMF_LogFoundDeallocError(statusToCheck=1, msg="hello",  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Error Msg Found Dealloc Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_RC_MEM_DEALLOCATE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_RC_MEM_DEALLOCATE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Error Msg Found Error
      write(failMsg, *) "Did not return .FALSE."
      is_error=ESMF_LogFoundDeallocError(statusToCheck=0, msg="hello",  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Error Msg Found Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Dealloc Error
      write(failMsg, *) "Did not return ESMF_FAILURE"
      is_error=ESMF_LogFoundDeallocError(statusToCheck=1,  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Log Found Dealloc Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_RC_MEM_DEALLOCATE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_RC_MEM_DEALLOCATE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Dealloc Error
      write(failMsg, *) "Did not return .FALSE."
      rc2 = ESMF_FAILURE
      is_error=ESMF_LogFoundDeallocError(statusToCheck=0,  &
          file=ESMF_FILENAME, line=__LINE__, rcToReturn=rc2)
      write(name, *) "Log Found Dealloc Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not leave return code unchanged"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      ! Additional Log methods
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test turning off tracing
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      write (name, *) "LogSet trace flag set to .true."
      call ESMF_LogSet (trace=.false., rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test turning off tracing
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      write (name, *) "LogGet trace flag set to .false."
      call ESMF_LogGet (trace=trace_flag, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS .and. .not. trace_flag,  &
          name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test accessing flush immediate flag
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      write (name, *) "LogGet flush immediate flag get"
      call ESMF_LogGet (flush=trace_flag, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test turning on flush immediate
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      write (name, *) "LogSet flush immediate flag set to .true."
      call ESMF_LogSet (flush=.true., rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test restoring flush immediate flag
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      write (name, *) "LogSet flush immediate flag restore"
      call ESMF_LogSet (flush=trace_flag, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !-----------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Error
      write(failMsg, *) "Did not return .FALSE."
      rc2 = ESMF_FAILURE
      is_error=ESMF_LogFoundError(ESMF_SUCCESS,rcToReturn=rc2)
      write(name, *) "Log Found Error Test"
      call ESMF_Test((.NOT.is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not leave return code unchanged"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Found Error
      write(failMsg, *) "Did not return ESMF_FAILURE"
      is_error=ESMF_LogFoundError(ESMF_FAILURE,rcToReturn=rc2)
      write(name, *) "Log Found Error Test"
      call ESMF_Test((is_error), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Value of rcToReturn
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " Verify rcToReturn Value Test"
      call ESMF_Test((rc2.eq.ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc2 = ", rc2

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test getting default Log file name
      write (name, *) "LogGet get default Log file name"
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogGet (fileName=filename, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      print *, 'default Log file name = ', trim (fileName)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test getting default Log file name with too short a string
      write (name, *) "LogGet get default Log file name with short string"
      write(failMsg, *) "Incorrectly returned ESMF_SUCCESS"
      call ESMF_LogGet (fileName=tooshortstr, rc=rc)
      call ESMF_Test(rc /= ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test LogFlush of new unopened log
      write(failMsg, *) "Did not return ESMF_FAILURE"
      write(name, *) " LogFlush of unopened log Test"
      call ESMF_LogFlush(log2, rc=rc)
      call ESMF_Test(rc /= ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Write without opening log file
      write(failMsg, *) "Returned ESMF_SUCCESS"
      call ESMF_LogWrite(log=log2, msg="Log Write One",logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
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
      write(failMsg, *) "Did not return ESMF_RC_FILE_OPEN"
      call ESMF_LogOpen(log2, "Log_Test_File_3", rc=rc)
      write(name, *) "Open Already Open Log Test"
      call ESMF_Test((rc.eq.ESMF_RC_FILE_OPEN), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test getting Log file name
      write (name, *) "LogGet get Log file name"
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogGet (log2, fileName=filename, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      print *, 'Log file name = ', trim (fileName)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Check returned Log file name
      write (name, *) "Check returned Log file name"
      write (failMsg, *) "Did not return ESMF_SUCCESS"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, filename == my_pet_char // ".Log_Test_File_2")
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      print *, "Starting a no-op loop to wait before testing time and date"

      call date_and_time(date=todays_date, time=todays_time, values=my_v)

      ! Each PET needs to have a time which is at least 1 second later
      ! than its lower-numbered neighbor.

      do, i=1, num_pets-1
        if (my_pet == i-1) then
          datetime_commbuf = my_v
          call ESMF_VMSend (vm,  &
            datetime_commbuf, count=size (datetime_commbuf), dstPet=i)
        else if (my_pet == i) then
          call ESMF_VMRecv (vm,  &
            datetime_commbuf, count=size (datetime_commbuf), srcPet=i-1)
          do
            call date_and_time (values=my_v, date=todays_date, time=todays_time)
            if (datetime_commbuf(7) /= my_v(7)) exit
          end do
          datetime_commbuf = my_v
        end if
        call ESMF_VMBarrier (vm)
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

      random_string = my_pet_char // random_chars

      !EX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      ! Get date and time to compare later in the test
      call date_and_time(values=my_v)
      call ESMF_LogWrite(log=log2, msg=random_string,logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
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
      call ESMF_LogClose(log2, rc=rc)
      write(name, *) "Close Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Log Close
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogClose(log2, rc=rc)
      write(name, *) "Close a closed Log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      ! Verify that the file can be opened with Fortran IO
      !EX_UTest
      write(name, *) "Open Log file with Fortran IO Test"
      write(failMsg, *) "open() returned failure"
      filename = my_pet_char // ".Log_Test_File_2"
      open (unit=1, file = filename, action = "read", &
            form = "formatted", status='old', iostat = ioerr)
      call ESMF_Test((ioerr == 0), name, failMsg, result, ESMF_SRCLINE)
      print *, " filename = ", trim (filename)
      print *, " iostat = ", ioerr
      if (ioerr /= 0) goto 100 ! cannot continue test without open file

      !------------------------------------------------------------------------
      ! Verify that the correct string was written to the file
      !EX_UTest
      rc = ESMF_FAILURE
      do
          read (1, *, iostat = ioerr) todays_date, todays_time, &
                                      msg_type, Pet_num, msg_string
          if (ioerr < 0) then
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
      call ESMF_TimePrint(my_time, options="string", rc=rc)
      print *, " log_time is "
      call ESMF_TimePrint(log_time, options="string", rc=rc)
if (time_diff < zero) stop 1

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
      print *, " My PET char is ", my_pet_char, ", and I read: ", pet_num

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test LogFlush of new unopened log
      write(failMsg, *) "Did not return ESMF_RC_FILE_OPEN"
      write(name, *) " LogFlush of unopened log Test"
      call ESMF_LogFlush(log2, rc=rc)
      call ESMF_Test((rc == ESMF_RC_FILE_OPEN), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test LogFlush of default log
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogFlush of default log Test"
      call ESMF_LogFlush( rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogSet with logmsgList set to info only Test"
      call ESMF_LogSet (  &
          logmsgList=(/ ESMF_LOGMSG_INFO /),  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Write a log file with message filtering
      write (failMsg, *) 'Could not write a log file with filtering'
      write (name, *) ' Creating a log file with message filtering'
      ESMF_BLOCK(msg_filter_test)
        ! Make sure we start with a clean log
        call ESMF_UtilIOUnitGet (unit=log8unit, rc=rc)
        if (rc /= ESMF_SUCCESS) exit msg_filter_test

        open (log8unit, file=trim (my_pet_char) // '.logAllow',  &
            status='unknown', iostat=ioerr)
        if (ioerr /= 0) then
            rc = ESMF_FAILURE
            exit msg_filter_test
        end if

        close (log8unit, status='delete', iostat=ioerr)
        if (ioerr /= 0) then
            rc = ESMF_FAILURE
            exit msg_filter_test
        end if

        ! Write some messages
        call ESMF_LogOpen (log8, filename='logAllow', rc=rc)
        if (rc /= ESMF_SUCCESS) exit msg_filter_test

        call ESMF_LogSet (log=log8,  &
            logmsgList=(/ ESMF_LOGMSG_INFO /),  &
            rc=rc)
        if (rc /= ESMF_SUCCESS) exit msg_filter_test

        call ESMF_LogWrite (log=log8,  &
            logmsgFlag=ESMF_LOGMSG_INFO, msg='should be in log', rc=rc)
        if (rc /= ESMF_SUCCESS) exit msg_filter_test

        call ESMF_LogWrite (log=log8,  &
            logmsgFlag=ESMF_LOGMSG_WARNING, msg='should NOT be in log', rc=rc)
        if (rc /= ESMF_SUCCESS) exit msg_filter_test

        call ESMF_LogWrite (log=log8,  &
            logmsgFlag=ESMF_LOGMSG_ERROR, msg='should NOT be in log', rc=rc)
        if (rc /= ESMF_SUCCESS) exit msg_filter_test

        call ESMF_LogClose (log8, rc=rc)
        if (rc /= ESMF_SUCCESS) exit msg_filter_test

      ESMF_ENDBLOCK(msg_filter_test)
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test log for INFO messages
      write(failMsg, *) "INFO log message expected, but not found"
      write(name, *) " Search log for INFO messages"

      ! Check for messages we do want
      call search_file (filename=trim (my_pet_char) // '.logAllow',  &
          text='INFO', found=was_found, rc=rc)
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, was_found .and. rc==ESMF_SUCCESS)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test log for WARNING messages
      write(failMsg, *) "WARNING log message found, but not expected"
      write(name, *) " Search log for WARNING messages"

      ! Check for messages we didn't want
      call search_file (filename=trim (my_pet_char) // '.logAllow',  &
          text='WARNING', found=was_found, rc=rc)
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, .not. was_found .and. rc==ESMF_SUCCESS)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test log for ERROR messages
      write(failMsg, *) "ERROR log message found, but not expected"
      write(name, *) " Search log for ERROR messages"

      ! Check for messages we didn't want
      call search_file (filename=trim (my_pet_char) // '.logAllow',  &
          text='ERROR', found=was_found, rc=rc)
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, .not. was_found .and. rc==ESMF_SUCCESS)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogSet with logmsgList set for warnings and info Test"
      call ESMF_LogSet (  &
          logmsgList=(/ ESMF_LOGMSG_WARNING, ESMF_LOGMSG_INFO /),  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogSet with logmsgList set for errors and info Test"
      call ESMF_LogSet (  &
          logmsgList=(/ ESMF_LOGMSG_ERROR, ESMF_LOGMSG_INFO /),  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogSet with logmsgList set for ALL types Test"
      call ESMF_LogSet (  &
          logmsgList = ESMF_LOGMSG_ALL,  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter when ALL set
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogWrite ERROR when ALL set Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_ERROR,  &
          msg="ALL set, should be in log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter when ALL set
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogWrite WARNING when ALL set Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_WARNING,  &
          msg="ALL set, should be in log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter when ALL set
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogWrite INFO when ALL set Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_INFO,  &
          msg="ALL set, should be in log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter when ALL set
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogWrite TRACE when ALL set Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_TRACE,  &
          msg="ALL set, should be in log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter with NO messages
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogSet with logmsgList set for NONE Test"
      call ESMF_LogSet (  &
          logmsgList = ESMF_LOGMSG_NONE,  &
          rc=rc)

      ! Have to turn the log back on so the test output will get logged.  :)
      call ESMF_LogSet (  &
          logmsgList = ESMF_LOGMSG_ALL,  &
          rc=rc2)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

     !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter with NOTRACE
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogSet with logmsgList set for NOTRACE Test"
      call ESMF_LogSet (  &
          logmsgList = ESMF_LOGMSG_NOTRACE,  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter when NOTRACE set
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogWrite ERROR when NOTRACE set Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_ERROR,  &
          msg="NOTRACE set, should be in log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter when NOTRACE set
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogWrite WARNING when NOTRACE set Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_WARNING,  &
          msg="NOTRACE set, should be in log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter when NOTRACE set
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogWrite INFO when NOTRACE set Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_INFO,  &
          msg="NOTRACE set, should be in log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test logmsgList filter when NOTRACE set
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogWrite when NOTRACE set Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_TRACE,  &
          msg="NOTRACE set, should NOT be in log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogSet logmsgAbort setting
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogSet with logmsgAbort Test"
      call ESMF_LogSet (logmsgAbort=(/ESMF_LOGMSG_ERROR/),  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogGet logmsgAbort getting
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogGet with logmsgAbort Test"
      logabort_flags => null ()
      call ESMF_LogGet (logmsgAbort=logabort_flags, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogGet logmsgAbort return association
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogGet with logmsgAbort set association Test"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, associated (logabort_flags) )
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogGet logmsgAbort return size
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogGet with logmsgAbort Test"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, size (logabort_flags) == 1)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogGet logmsgAbort return values
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogGet with logmsgAbort Test"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE,  &
           logabort_flags(1) == ESMF_LOGMSG_ERROR )
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogSet logmsgAbort clear
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogSet with logmsgAbort Test"
      call ESMF_LogSet (logmsgAbort=ESMF_LOGMSG_NONE, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogGet logmsgAbort getting
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogGet with logmsgAbort cleared Test"
      if (associated (logabort_flags)) deallocate (logabort_flags)
      logabort_flags => null ()
      call ESMF_LogGet (logmsgAbort=logabort_flags, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogGet logmsgAbort return association
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogGet with logmsgAbort cleared association Test"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, associated (logabort_flags) )
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogGet logmsgAbort return size
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) " LogGet with logmsgAbort cleared size Test"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, size (logabort_flags) == 0)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      if (associated (logabort_flags)) deallocate (logabort_flags)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test opening a log for assignment
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Opening Log for assignment Test"
      call ESMF_LogOpen (log9, "Log_assignment_log9", rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogAssignment(=)(log,log)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Log assignment (and write via alias) Test"
      log9_alias = log9
      call ESMF_LogWrite ("test message via alias",  &
          logmsgFlag=ESMF_LOGMSG_INFO, log=log9_alias, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogOperator(==)(log,log)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Log equality with same log Test"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, log9 == log9_alias)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogOperator(==)(log,log)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Log equality with different log Test"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, .not. (log8 == log9))
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogOperator(/=)(log,log)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Log inequality with same log Test"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, .not. (log9 /= log9_alias))
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test ESMF_LogOperator(/=)(log,log)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Log inequality with different log Test"
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, log8 /= log9)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test closing a log via an alias
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Closing log via alias Test"
      call ESMF_LogClose (log=log9_alias, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test opening the default log under a different name when it is already open
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Opening unclosed default log Test"
      call ESMF_LogOpen ('new_log', rc=rc)
      call ESMF_Test((rc /= ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test setting the MPI_Wtime flag
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set highResTimestampFlag test"
      call ESMF_LogSet (highResTimestampFlag=.true., rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test the MPI_Wtime flag
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Test highResTimestampFlag set test"
      call ESMF_LogGet (highResTimestampFlag=highRes_flag, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS .and. highRes_flag), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test writing with highResTimestampFlag set
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing with high res timestamp set Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_INFO,  &
          msg=" High res timestamps set, and should be in the log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test clearing the MPI_Wtime flag
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Clear highResTimestampFlag test"
      call ESMF_LogSet (highResTimestampFlag=.false., rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test the MPI_Wtime flag
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Test highResTimestampFlag clear test"
      call ESMF_LogGet (highResTimestampFlag=highRes_flag, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS .and. .not. highRes_flag), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test writing with highResTimestampFlag cleared
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Writing with high res timestamp cleared Test"
      call ESMF_LogWrite (logmsgFlag=ESMF_LOGMSG_INFO,  &
          msg=" High res timestamps cleared, and should be NOT be in the log",  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


! Test ESMF_LOGKIND_MULTI_ON_ERROR feature

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test opening a Log with ESMF_LOGKIND_MULTI_ON_ERROR
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Open Log with ESMF_LOGKIND_MULTI_ON_ERROR test"

      write (filename,'(a,i4.4,a)') 'on_error_log', my_pet,'.Log'
      pet_filename = my_pet_char // '.' // filename(:len_trim (filename))

      ESMF_BLOCK(open_moe_log1)

        call ESMF_UtilIOUnitGet (unit=moe_unit, rc=rc)
        if (rc /= ESMF_SUCCESS) exit open_moe_log1

        ! If there is an existing Log file, delete it.
        open (moe_unit, file=pet_filename, status='old', iostat=ioerr)
        if (ioerr == 0) then
          close (moe_unit, status='delete')
        end if

        call ESMF_LogOpen(log_moe, filename, logkindflag=ESMF_LOGKIND_MULTI_ON_ERROR, rc=rc)

      ESMF_ENDBLOCK(open_moe_log1)
        
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Write some non-ERROR messages
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Write non-ERROR messages with ESMF_LOGKIND_MULTI_ON_ERROR test"

      call ESMF_LogWrite(log=log_moe, msg="INFO test Msg",logmsgFlag=ESMF_LOGMSG_INFO, &
                         rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Close log
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Close (non-existant) ESMF_LOGKIND_MULTI_ON_ERROR log test"

      call ESMF_LogClose (log=log_moe, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test for log - which shouldn't exist
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Test for non-existant ESMF_LOGKIND_MULTI_ON_ERROR log test"

      ESMF_BLOCK(open_moe_log2)

        call ESMF_UtilIOUnitGet (unit=moe_unit, rc=rc)
        if (rc /= ESMF_SUCCESS) exit open_moe_log2

        ! Open should fail
        open (moe_unit, file=pet_filename, status='old', iostat=ioerr)
        rc = merge (ESMF_SUCCESS, ESMF_FAILURE, ioerr /= 0)

      ESMF_ENDBLOCK(open_moe_log2)

      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test opening a Log with ESMF_LOGKIND_MULTI_ON_ERROR
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Open Log with ESMF_LOGKIND_MULTI_ON_ERROR test"

      ESMF_BLOCK(open_moe_log3)

        call ESMF_UtilIOUnitGet (unit=moe_unit, rc=rc)
        if (rc /= ESMF_SUCCESS) exit open_moe_log3

        ! If there is an existing Log file, delete it.
        open (moe_unit, file=pet_filename, status='old', iostat=ioerr)
        if (ioerr == 0) then
          close (moe_unit, status='delete')
        end if

        call ESMF_LogOpen(log_moe, filename, logkindflag=ESMF_LOGKIND_MULTI_ON_ERROR, rc=rc)

      ESMF_ENDBLOCK(open_moe_log3)
        
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Write non-ERROR message
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Write non-ERROR message with ESMF_LOGKIND_MULTI_ON_ERROR test"

      call ESMF_LogWrite(log=log_moe, msg="INFO test Msg",logmsgFlag=ESMF_LOGMSG_INFO, &
                         rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Write ERROR message
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Write ERROR message with ESMF_LOGKIND_MULTI_ON_ERROR test"

      call ESMF_LogWrite(log=log_moe, msg="ERROR test Msg",logmsgFlag=ESMF_LOGMSG_ERROR, &
                         rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Write non-ERROR message
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Write non-ERROR message with ESMF_LOGKIND_MULTI_ON_ERROR test"

      call ESMF_LogWrite(log=log_moe, msg="INFO test Msg",logmsgFlag=ESMF_LOGMSG_INFO, &
                         rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Close log
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Close ESMF_LOGKIND_MULTI_ON_ERROR log test"

      call ESMF_LogClose (log=log_moe, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test for log - which should exist
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Test for ESMF_LOGKIND_MULTI_ON_ERROR log test"

      ESMF_BLOCK(open_moe_log4)

        call ESMF_UtilIOUnitGet (unit=moe_unit, rc=rc)
        if (rc /= ESMF_SUCCESS) exit open_moe_log4

        ! If there is an existing Log file, delete it.
        open (moe_unit, file=pet_filename, status='old', iostat=ioerr)
        rc = merge (ESMF_SUCCESS, ESMF_FAILURE, ioerr == 0)
        if (ioerr == 0) then
          ! close (moe_unit, status='delete')
        end if

      ESMF_ENDBLOCK(open_moe_log4)

      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
100   continue
      call ESMF_TestEnd(ESMF_SRCLINE)

contains

  subroutine search_file (filename, text, found, rc)
    character(*), intent(in)  :: filename
    character(*), intent(in)  :: text
    logical,      intent(out) :: found
    integer,      intent(out) :: rc

    character(ESMF_MAXSTR)    :: record
    integer :: ioerr
    integer :: unitno

    rc    = ESMF_FAILURE
    found = .false.

    call ESMF_UtilIOUnitGet (unitno)
    open (unit=unitno, file=filename, status='old',  &
        action='read', position='rewind', iostat=ioerr)
    if (ioerr /= 0) then
      print *, 'Could not open file: ', trim (filename), ', iostat =', ioerr
      return
    end if

    do
      read (unitno, '(a)', iostat=ioerr) record
      if (ioerr /= 0) exit
      found = index (record, text) > 0
      if (found) exit
    end do

    close (unitno)

    rc = ESMF_SUCCESS

  end subroutine search_file

      end program ESMF_LogErrUTest
