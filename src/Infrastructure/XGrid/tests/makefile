# $Id: makefile,v 1.4 2010/10/13 22:19:18 feiliu Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/XGrid/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_XGridUTest

TESTS_RUN     = RUN_ESMF_XGridUTest

TESTS_RUN_UNI = RUN_ESMF_XGridUTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =

DIRS        = 


#
# XGrid
#

RUN_ESMF_XGridUTest:
	$(MAKE) TNAME=XGrid NP=1 ftest

RUN_ESMF_XGridUTestUNI:
	$(MAKE) TNAME=XGrid NP=1 ftest

