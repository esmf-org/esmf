# $Id: makefile,v 1.22 2004/12/15 22:50:17 nscollins Exp $

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

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt stdoutfiles NULL core ESMF_LogFile $(TESTS_BUILD) 
CLOBBERDIRS =



#
#
#

RUN_ESMF_AlarmUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_AlarmUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_AlarmUTest > $(ESMC_TESTDIR)/ESMF_AlarmUTest.stdout

RUN_ESMF_AlarmUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_AlarmUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_AlarmUTest > $(ESMC_TESTDIR)/ESMF_AlarmUTest.stdout



#
#
#

RUN_ESMF_ClockUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ClockUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout

RUN_ESMF_ClockUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ClockUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout



#
#
#

RUN_ESMF_CalendarUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_CalendarUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CalendarUTest > $(ESMC_TESTDIR)/ESMF_CalendarUTest.stdout

RUN_ESMF_CalendarUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CalendarUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CalendarUTest > $(ESMC_TESTDIR)/ESMF_CalendarUTest.stdout



#
#
#

#RUN_ESMC_ClockUTest:
#	-@rm -f testlog stdlog.* logfile.* ESMC_ClockUTest.tmps
#	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout

#RUN_ESMC_ClockUTestUNI:
#	-@rm -f testlog stdlog.* logfile.* ESMC_ClockUTest.tmps
#	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout


#
#
#

RUN_ESMF_CalRangeUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_CalRangeUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CalRangeUTest > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.stdout

RUN_ESMF_CalRangeUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CalRangeUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CalRangeUTest > $(ESMC_TESTDIR)/ESMF_CalRangeUTest.stdout


#
#
#

RUN_ESMF_TimeIntervalUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_TimeIntervalUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.stdout

RUN_ESMF_TimeIntervalUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TimeIntervalUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest > $(ESMC_TESTDIR)/ESMF_TimeIntervalUTest.stdout



#
#
#

RUN_ESMF_TimeUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_TimeUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_TimeUTest > $(ESMC_TESTDIR)/ESMF_TimeUTest.stdout

RUN_ESMF_TimeUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TimeUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_TimeUTest > $(ESMC_TESTDIR)/ESMF_TimeUTest.stdout

