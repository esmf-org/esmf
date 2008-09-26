# $Id: makefile,v 1.21 2008/09/26 04:16:43 cdeluca Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests

LOCDIR	  = src/Infrastructure/Base/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_BaseUTest

TESTS_RUN     = RUN_ESMF_BaseUTest

TESTS_RUN_UNI = RUN_ESMF_BaseUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Base class
#

RUN_ESMF_BaseUTest:
	$(MAKE) TNAME=Base NP=4 ftest

RUN_ESMF_BaseUTestUNI:
	$(MAKE) TNAME=Base NP=1 ftest


