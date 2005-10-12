# $Id: makefile,v 1.28 2005/10/12 19:06:21 nscollins Exp $

ALL:  build_unit_tests
run:  run_unit_tests

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

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_AlarmUTest \
		$(ESMF_TESTDIR)/ESMF_ClockUTest \
		$(ESMF_TESTDIR)/ESMF_CalendarUTest \
		$(ESMF_TESTDIR)/ESMF_CalRangeUTest \
		$(ESMF_TESTDIR)/ESMF_TimeIntervalUTest \
		$(ESMF_TESTDIR)/ESMF_TimeUTest 
#	        $(ESMF_TESTDIR)/ESMC_ClockUTest 
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




RUN_ESMF_AlarmUTest:
	$(MAKE) TNAME=Alarm NP=4 ftest

RUN_ESMF_AlarmUTestUNI:
	$(MAKE) TNAME=Alarm NP=1 ftest



RUN_ESMF_ClockUTest:
	$(MAKE) TNAME=Clock NP=4 ftest

RUN_ESMF_ClockUTestUNI:
	$(MAKE) TNAME=Clock NP=1 ftest



RUN_ESMF_CalendarUTest:
	$(MAKE) TNAME=Calendar NP=4 ftest

RUN_ESMF_CalendarUTestUNI:
	$(MAKE) TNAME=Calendar NP=1 ftest



RUN_ESMC_ClockUTest:
	$(MAKE) TNAME=Clock NP=4 ctest

RUN_ESMC_ClockUTestUNI:
	$(MAKE) TNAME=Clock NP=1 ctest



RUN_ESMF_CalRangeUTest:
	$(MAKE) TNAME=CalRange NP=4 ftest

RUN_ESMF_CalRangeUTestUNI:
	$(MAKE) TNAME=CalRange NP=1 ftest



RUN_ESMF_TimeIntervalUTest:
	$(MAKE) TNAME=TimeInterval NP=4 ftest

RUN_ESMF_TimeIntervalUTestUNI:
	$(MAKE) TNAME=TimeInterval NP=1 ftest



RUN_ESMF_TimeUTest:
	$(MAKE) TNAME=Time NP=4 ftest

RUN_ESMF_TimeUTestUNI:
	$(MAKE) TNAME=Time NP=1 ftest


