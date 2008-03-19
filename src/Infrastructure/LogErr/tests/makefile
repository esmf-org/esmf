# $Id: makefile,v 1.16.2.1 2008/03/19 20:40:21 svasquez Exp $ 

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/LogErr/tests

# TODO:FIELDINTEGRATION Restore LogErrHaltUTest

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_LogErrUTest
#		$(ESMF_TESTDIR)/ESMF_LogErrHaltUTest

TESTS_RUN     = RUN_ESMF_LogErrUTest
#                RUN_ESMF_LogErrHaltUTest

TESTS_RUN_UNI = RUN_ESMF_LogErrUTestUNI
#                RUN_ESMF_LogErrHaltUTestUNI



include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) Log*
CLOBBERDIRS =


#
# LogErr
#
RUN_ESMF_LogErrUTest:
	# remove log files of previous test runs
	rm -f $(ESMF_TESTDIR)/*Log_Test_File* $(ESMF_TESTDIR)/Single_Log_File
	$(MAKE) TNAME=LogErr NP=4 ftest

RUN_ESMF_LogErrUTestUNI:
	# remove log files of previous test runs
	rm -f $(ESMF_TESTDIR)/*Log_Test_File* $(ESMF_TESTDIR)/Single_Log_File
	$(MAKE) TNAME=LogErr NP=1 ftest


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
	
