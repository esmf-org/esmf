# $Id: makefile,v 1.12.2.1 2008/04/09 22:39:13 theurich Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

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
LOCDIR	  = src/Infrastructure/DELayout/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_DELayoutUTest \
                $(ESMF_TESTDIR)/ESMF_DELayoutWorkQueueUTest

TESTS_RUN     = RUN_ESMF_DELayoutUTest \
                RUN_ESMF_DELayoutWorkQueueUTest

TESTS_RUN_UNI = RUN_ESMF_DELayoutUTestUNI \
                RUN_ESMF_DELayoutWorkQueueUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# DELayout unit test
#
RUN_ESMF_DELayoutUTest:
	$(MAKE) TNAME=DELayout NP=4 ftest

RUN_ESMF_DELayoutUTestUNI:
	$(MAKE) TNAME=DELayout NP=1 ftest

#
# DELayout work queue unit test
#
RUN_ESMF_DELayoutWorkQueueUTest:
	$(MAKE) TNAME=DELayoutWorkQueue NP=4 ftest

RUN_ESMF_DELayoutWorkQueueUTestUNI:
	$(MAKE) TNAME=DELayoutWorkQueue NP=1 ftest

