# $Id: makefile,v 1.9 2005/07/15 03:41:47 nscollins Exp $ 

ALL:  build_unit_tests
run:  tree_run_unit_tests

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

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_LogErrUTest \
		$(ESMF_TESTDIR)/ESMF_LogErrHaltUTest

TESTS_RUN     = RUN_ESMF_LogErrUTest \
                RUN_ESMF_LogErrHaltUTest

TESTS_RUN_UNI = RUN_ESMF_LogErrUTestUNI \
                RUN_ESMF_LogErrHaltUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) Log*
CLOBBERDIRS =


#
# LogErr
#
RUN_ESMF_LogErrUTest:
	$(MAKE) TNAME=LogErr NP=4 ftest

RUN_ESMF_LogErrUTestUNI:
	$(MAKE) TNAME=LogErr NP=1 ftest


#
# LogErrHalt
#

RUN_ESMF_LogErrHaltUTest:
	$(MAKE) TNAME=LogErrHalt NP=4 ftest

RUN_ESMF_LogErrHaltUTestUNI:
	$(MAKE) TNAME=LogErrHalt NP=1 ftest


