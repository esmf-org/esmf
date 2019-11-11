# $Id$ 

ALL:     build_unit_tests
run:     run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Infrastructure/LogErr/tests

# TODO:FIELDINTEGRATION Restore LogErrHaltUTest

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMC_LogErrUTest \
		$(ESMF_TESTDIR)/ESMC_LogErrPerfUTest \
		$(ESMF_TESTDIR)/ESMF_LogErrUTest \
		$(ESMF_TESTDIR)/ESMF_LogErrPerfUTest
#		$(ESMF_TESTDIR)/ESMF_LogErrHaltUTest

TESTS_RUN     = RUN_ESMC_LogErrUTest \
		RUN_ESMC_LogErrPerfUTest \
		RUN_ESMF_LogErrUTest \
		RUN_ESMF_LogErrPerfUTest
#                RUN_ESMF_LogErrHaltUTest

TESTS_RUN_UNI = RUN_ESMC_LogErrUTestUNI \
		RUN_ESMC_LogErrPerfUTestUNI \
		RUN_ESMF_LogErrUTestUNI \
		RUN_ESMF_LogErrPerfUTestUNI
#                RUN_ESMF_LogErrHaltUTestUNI



include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) Log*
CLOBBERDIRS =

DIRS        = 

#
# ESMC_LogErr
#

RUN_ESMC_LogErrUTest:
	$(MAKE) TNAME=LogErr NP=4 ctest

RUN_ESMC_LogErrUTestUNI:
	$(MAKE) TNAME=LogErr NP=1 ctest

#
# ESMC_LogErrPerf
#

RUN_ESMC_LogErrPerfUTest:
	$(MAKE) TNAME=LogErrPerf NP=4 ctest

RUN_ESMC_LogErrPerfUTestUNI:
	$(MAKE) TNAME=LogErrPerf NP=1 ctest

#
# ESMF_LogErr
#

ESMF_UTEST_LogErr_OBJS = ESMCI_TestError.o
ESMF_LogErrUTest.o : ESMCI_TestError.o

RUN_ESMF_LogErrUTest:
	# remove log files of previous test runs
	rm -f $(ESMF_TESTDIR)/*Log_Test_File* $(ESMF_TESTDIR)/Single_Log_File
	$(MAKE) TNAME=LogErr NP=4 ftest

RUN_ESMF_LogErrUTestUNI:
	# remove log files of previous test runs
	rm -f $(ESMF_TESTDIR)/*Log_Test_File* $(ESMF_TESTDIR)/Single_Log_File
	$(MAKE) TNAME=LogErr NP=1 ftest

#
# ESMC_LogErrPerf
#

RUN_ESMF_LogErrPerfUTest:
	$(MAKE) TNAME=LogErrPerf NP=4 ftest

RUN_ESMF_LogErrPerfUTestUNI:
	$(MAKE) TNAME=LogErrPerf NP=1 ftest

#
# LogErrHalt - special for the lam implementation of MPI.  halting a
#  task in the middle of execution leaves lam in a bad state.  try to
#  bring lam down and restart it so that subsequent tests can succeed.
#

RUN_ESMF_LogErrHaltUTest:
	$(MAKE) TNAME=LogErrHalt NP=4 ftest
ifeq ($(ESMF_COMM),lam)
	$(MAKE) lamrestart
endif

RUN_ESMF_LogErrHaltUTestUNI:
	$(MAKE) TNAME=LogErrHalt NP=1 ftest
ifeq ($(ESMF_COMM),lam)
	$(MAKE) lamrestart
endif


# try to restart lam in a controlled way
lamrestart:
	-lamhalt
	sleep 5
	-lamboot $(ESMF_LAMARGS)
	
