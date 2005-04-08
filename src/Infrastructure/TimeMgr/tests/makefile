# $Id: makefile,v 1.24 2005/04/08 17:35:49 svasquez Exp $

ALL:  build_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/TimeMgr/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_AlarmUTest \
		$(ESMC_TESTDIR)/ESMF_ClockUTest \
		$(ESMC_TESTDIR)/ESMF_CalendarUTest \
		$(ESMC_TESTDIR)/ESMF_CalRangeUTest \
		$(ESMC_TESTDIR)/ESMF_TimeIntervalUTest \
		$(ESMC_TESTDIR)/ESMF_TimeUTest 
#	        $(ESMC_TESTDIR)/ESMC_ClockUTest 
# remove temporarily

TESTS_RUN     = RUN_ESMF_AlarmUTest \
		RUN_ESMF_CalendarUTest \
		RUN_ESMF_ClockUTest \
		RUN_ESMF_CalRangeUTest \
		RUN_ESMF_TimeIntervalUTest \
		RUN_ESMF_TimeUTest
#		RUN_ESMC_ClockUTest 

TESTS_RUN_UNI = RUN_ESMF_AlarmUTestUNI \
		RUN_ESMF_CalendarUTestUNI \
		RUN_ESMF_ClockUTestUNI \
		RUN_ESMF_CalRangeUTestUNI \
		RUN_ESMF_TimeIntervalUTestUNI \
		RUN_ESMF_TimeUTestUNI
#		RUN_ESMC_ClockUTestUNI 


include ${ESMF_DIR}/makefile

DIRS	  =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) 
CLOBBERDIRS =



#
#
#

RUN_ESMF_AlarmUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_AlarmUTest > $(ESMC_TESTDIR)/ESMF_AlarmUTest.stdout
	-cat *AlarmUTest.Log > $(ESMC_TESTDIR)/ESMF_AlarmUTest.Log
	-rm *AlarmUTest.Log

RUN_ESMF_AlarmUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_AlarmUTest > $(ESMC_TESTDIR)/ESMF_AlarmUTest.stdout
	-cat *AlarmUTest.Log > $(ESMC_TESTDIR)/ESMF_AlarmUTest.Log
	-rm *AlarmUTest.Log



#
#
#

RUN_ESMF_ClockUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout
	-cat *ClockUTest.Log > $(ESMC_TESTDIR)/ESMF_ClockUTest.Log
	-rm *ClockUTest.Log

RUN_ESMF_ClockUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout
	-cat *ClockUTest.Log > $(ESMC_TESTDIR)/ESMF_ClockUTest.Log
	-rm *ClockUTest.Log



#
#
#

RUN_ESMF_CalendarUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CalendarUTest > $(ESMC_TESTDIR)/ESMF_CalendarUTest.stdout
	-cat *CalendarUTest.Log > $(ESMC_TESTDIR)/ESMF_CalendarUTest.Log
	-rm *CalendarUTest.Log

RUN_ESMF_CalendarUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CalendarUTest > $(ESMC_TESTDIR)/ESMF_CalendarUTest.stdout
	-cat *CalendarUTest.Log > $(ESMC_TESTDIR)/ESMF_CalendarUTest.Log
	-rm *CalendarUTest.Log



#
#
#

#RUN_ESMC_ClockUTest:
#	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout
#	-cat *ClockUTest.Log > $(ESMC_TESTDIR)/ESMC_ClockUTest.Log
#	-rm *ClockUTest.Log

#RUN_ESMC_ClockUTestUNI:
#	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout
#	-cat *ClockUTest.Log > $(ESMC_TESTDIR)/ESMC_ClockUTest.Log
#	-rm *ClockUTest.Log


#
#
#

RUN_ESMF_CalRangeUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CalRangeUTest > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.stdout
	-cat *CalRangeUTest.Log > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.Log
	-rm *CalRangeUTest.Log

RUN_ESMF_CalRangeUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CalRangeUTest > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.stdout
	-cat *CalRangeUTest.Log > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.Log
	-rm *CalRangeUTest.Log


#
#
#

RUN_ESMF_TimeIntervalUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.stdout
	-cat *TimeIntervalUTest.Log > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.Log
	-rm *TimeIntervalUTest.Log

RUN_ESMF_TimeIntervalUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.stdout
	-cat *TimeIntervalUTest.Log > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.Log
	-rm *TimeIntervalUTest.Log



#
#
#

RUN_ESMF_TimeUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_TimeUTest > $(ESMC_TESTDIR)/ESMF_TimeUTest.stdout
	-cat *TimeUTest.Log > $(ESMC_TESTDIR)/ESMF_TimeUTest.Log
	-rm *TimeUTest.Log

RUN_ESMF_TimeUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_TimeUTest > $(ESMC_TESTDIR)/ESMF_TimeUTest.stdout
	-cat *TimeUTest.Log > $(ESMC_TESTDIR)/ESMF_TimeUTest.Log
	-rm *TimeUTest.Log

