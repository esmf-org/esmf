# $Id: makefile,v 1.41 2007/05/16 00:29:10 donstark Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =
CPFILES   = ESMF_FRoute8UTest.F90

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/Field/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldUTest \
		$(ESMF_TESTDIR)/ESMF_FRoute4UTest \
		$(ESMF_TESTDIR)/ESMF_FRoute8UTest 

TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FRouteUTest \
                RUN_ESMF_FieldHarness2UTest \
		RUN_ESMF_FieldHarness4UTest

TESTS_RUN_UNI = RUN_ESMF_FieldUTestUNI \
                RUN_ESMF_FRoute4UTestUNI \
                RUN_ESMF_FRoute8UTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =

ESMF_FRoute8UTest.F90:  ESMF_FRoute4UTest.F90
	cp -f ESMF_FRoute4UTest.F90 ESMF_FRoute8UTest.F90

#
# Field
#
RUN_ESMF_FieldUTest:
	$(MAKE) TNAME=Field NP=4 ftest

RUN_ESMF_FieldUTestUNI:
	$(MAKE) TNAME=Field NP=1 ftest


#
# Field Route function
#
RUN_ESMF_FRouteUTest:
	$(MAKE) TNAME=FRoute4 NP=4 ftest
	$(MAKE) TNAME=FRoute8 NP=8 ftest

	# for now comment out the larger tests.  as far as i know, these
	# run, but can cause problems for the automatic overnight builds.
	#$(MAKE) TNAME=FRoute12 NP=12 ftest
	#$(MAKE) TNAME=FRoute16 NP=16 ftest
	#$(MAKE) TNAME=FRoute20 NP=20 ftest
	#$(MAKE) TNAME=FRoute24 NP=24 ftest
	#$(MAKE) TNAME=FRoute32 NP=32 ftest

RUN_ESMF_FRoute4UTest:
	$(MAKE) TNAME=FRoute4 NP=4 ftest

RUN_ESMF_FRoute8UTest:
	$(MAKE) TNAME=FRoute8 NP=8 ftest


RUN_ESMF_FRoute4UTestUNI:
	echo "PASS: ESMF_FRoute4UTest not set up to run on one processor." > $(ESMC_TESTDIR)/ESMF_FRoute4UTest.stdout

RUN_ESMF_FRoute8UTestUNI:
	echo "PASS: ESMF_FRoute8UTest not set up to run on one processor." > $(ESMC_TESTDIR)/ESMF_FRoute8UTest.stdout
        
        
RUN_ESMF_FieldHarness2UTest:
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=FieldHarness2 NP=2 htest

RUN_ESMF_FieldHarness4UTest:
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=FieldHarness4 NP=4 htest
