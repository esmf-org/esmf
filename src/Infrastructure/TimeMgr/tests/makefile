# $Id: makefile,v 1.23 2005/03/01 21:05:02 nscollins Exp $

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

RUN_ESMF_AlarmUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_AlarmUTest > $(ESMC_TESTDIR)/ESMF_AlarmUTest.stdout



#
#
#

RUN_ESMF_ClockUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout

RUN_ESMF_ClockUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout



#
#
#

RUN_ESMF_CalendarUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CalendarUTest > $(ESMC_TESTDIR)/ESMF_CalendarUTest.stdout

RUN_ESMF_CalendarUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CalendarUTest > $(ESMC_TESTDIR)/ESMF_CalendarUTest.stdout



#
#
#

#RUN_ESMC_ClockUTest:
#	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout

#RUN_ESMC_ClockUTestUNI:
#	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout


#
#
#

RUN_ESMF_CalRangeUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CalRangeUTest > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.stdout

RUN_ESMF_CalRangeUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CalRangeUTest > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.stdout


#
#
#

RUN_ESMF_TimeIntervalUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.stdout

RUN_ESMF_TimeIntervalUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.stdout



#
#
#

RUN_ESMF_TimeUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_TimeUTest > $(ESMC_TESTDIR)/ESMF_TimeUTest.stdout

RUN_ESMF_TimeUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_TimeUTest > $(ESMC_TESTDIR)/ESMF_TimeUTest.stdout

