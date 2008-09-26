# $Id: makefile,v 1.8 2008/09/26 04:16:54 cdeluca Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests

LOCDIR	  = src/Infrastructure/IOSpec/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_IOSpecUTest

TESTS_RUN     = RUN_ESMF_IOSpecUTest

TESTS_RUN_UNI = RUN_ESMF_IOSpecUTestUNI 


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# IOSpec
#
RUN_ESMF_IOSpecUTest:
	$(MAKE) TNAME=IOSpec NP=4 ftest

RUN_ESMF_IOSpecUTestUNI:
	$(MAKE) TNAME=IOSpec NP=1 ftest


