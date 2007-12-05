# $Id: makefile,v 1.12 2007/12/05 06:03:33 theurich Exp $

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  =  src/Infrastructure/Config/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ConfigUTest \
	        $(ESMF_TESTDIR)/ESMC_ConfigUTest 

TESTS_RUN     = RUN_ESMF_ConfigUTest \
		RUN_ESMC_ConfigUTest 

TESTS_RUN_UNI = RUN_ESMF_ConfigUTestUNI \
		RUN_ESMC_ConfigUTestUNI 


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# ESMF_ConfigUTest
#
RUN_ESMF_ConfigUTest:
	cp -f ESMF_Resource_File_Sample.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Config NP=4 ftest

RUN_ESMF_ConfigUTestUNI:
	cp -f ESMF_Resource_File_Sample.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Config NP=1 ftest


#
# ESMC_ConfigUTest
#
RUN_ESMC_ConfigUTest:
	cp -f ESMF_Resource_File_Sample.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Config NP=4 ctest

RUN_ESMC_ConfigUTestUNI:
	cp -f ESMF_Resource_File_Sample.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Config NP=1 ctest
