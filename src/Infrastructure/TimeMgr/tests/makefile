# $Id: makefile,v 1.26 2005/06/28 20:56:14 svasquez Exp $

ALL:  build_unit_tests

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
	-$(RM) *AlarmUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_AlarmUTest > $(ESMC_TESTDIR)/ESMF_AlarmUTest.stdout
	-cat *AlarmUTest.Log > $(ESMC_TESTDIR)/ESMF_AlarmUTest.Log
	-$(RM) *AlarmUTest.Log

RUN_ESMF_AlarmUTestUNI:
	-$(RM) *AlarmUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_AlarmUTest > $(ESMC_TESTDIR)/ESMF_AlarmUTest.stdout
	-cat *AlarmUTest.Log > $(ESMC_TESTDIR)/ESMF_AlarmUTest.Log
	-$(RM) *AlarmUTest.Log



#
#
#

RUN_ESMF_ClockUTest:
	-$(RM) *ClockUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout
	-cat *ClockUTest.Log > $(ESMC_TESTDIR)/ESMF_ClockUTest.Log
	-$(RM) *ClockUTest.Log

RUN_ESMF_ClockUTestUNI:
	-$(RM) *ClockUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout
	-cat *ClockUTest.Log > $(ESMC_TESTDIR)/ESMF_ClockUTest.Log
	-$(RM) *ClockUTest.Log



#
#
#

RUN_ESMF_CalendarUTest:
	-$(RM) *CalendarUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CalendarUTest > $(ESMC_TESTDIR)/ESMF_CalendarUTest.stdout
	-cat *CalendarUTest.Log > $(ESMC_TESTDIR)/ESMF_CalendarUTest.Log
	-$(RM) *CalendarUTest.Log

RUN_ESMF_CalendarUTestUNI:
	-$(RM) *CalendarUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CalendarUTest > $(ESMC_TESTDIR)/ESMF_CalendarUTest.stdout
	-cat *CalendarUTest.Log > $(ESMC_TESTDIR)/ESMF_CalendarUTest.Log
	-$(RM) *CalendarUTest.Log



#
#
#

#RUN_ESMC_ClockUTest:
#	-$(RM) *ClockUTest.Log
#	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout
#	-cat *ClockUTest.Log > $(ESMC_TESTDIR)/ESMC_ClockUTest.Log
#	-$(RM) *ClockUTest.Log

#RUN_ESMC_ClockUTestUNI:
#	-$(RM) *ClockUTest.Log
#	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout
#	-cat *ClockUTest.Log > $(ESMC_TESTDIR)/ESMC_ClockUTest.Log
#	-$(RM) *ClockUTest.Log


#
#
#

RUN_ESMF_CalRangeUTest:
	-$(RM) *CalRangeUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CalRangeUTest > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.stdout
	-cat *CalRangeUTest.Log > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.Log
	-$(RM) *CalRangeUTest.Log

RUN_ESMF_CalRangeUTestUNI:
	-$(RM) *CalRangeUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CalRangeUTest > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.stdout
	-cat *CalRangeUTest.Log > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.Log
	-$(RM) *CalRangeUTest.Log


#
#
#

RUN_ESMF_TimeIntervalUTest:
	-$(RM) *TimeIntervalUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.stdout
	-cat *TimeIntervalUTest.Log > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.Log
	-$(RM) *TimeIntervalUTest.Log

RUN_ESMF_TimeIntervalUTestUNI:
	-$(RM) *TimeIntervalUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.stdout
	-cat *TimeIntervalUTest.Log > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.Log
	-$(RM) *TimeIntervalUTest.Log



#
#
#

RUN_ESMF_TimeUTest:
	-$(RM) *TimeUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_TimeUTest > $(ESMC_TESTDIR)/ESMF_TimeUTest.stdout
	-cat *TimeUTest.Log > $(ESMC_TESTDIR)/ESMF_TimeUTest.Log
	-$(RM) *TimeUTest.Log

RUN_ESMF_TimeUTestUNI:
	-$(RM) *TimeUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_TimeUTest > $(ESMC_TESTDIR)/ESMF_TimeUTest.stdout
	-cat *TimeUTest.Log > $(ESMC_TESTDIR)/ESMF_TimeUTest.Log
	-$(RM) *TimeUTest.Log

