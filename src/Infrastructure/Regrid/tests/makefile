# $Id: makefile,v 1.31 2005/10/21 17:26:27 svasquez Exp $

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
LOCDIR	  = src/Infrastructure/Regrid/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_RegridUTest \
		$(ESMF_TESTDIR)/ESMF_RegridOptionsUTest \
		$(ESMF_TESTDIR)/ESMF_RegridBandingUTest \
		$(ESMF_TESTDIR)/ESMF_RegridFunctionUTest \
		$(ESMF_TESTDIR)/ESMF_RegridSampleUTest

TESTS_RUN     = RUN_ESMF_RegridUTest \
		RUN_ESMF_RegridOptionsUTest \
		RUN_ESMF_RegridBandingUTest \
		RUN_ESMF_RegridFunctionUTest \
		RUN_ESMF_RegridSampleUTest

TESTS_RUN_UNI = RUN_ESMF_RegridUTestUNI \
	        RUN_ESMF_RegridOptionsUTestUNI \
	        RUN_ESMF_RegridBandingUTestUNI \
		RUN_ESMF_RegridFunctionUTestUNI \
		RUN_ESMF_RegridSampleUTestUNI


include $(ESMF_DIR)/makefile


DIRS =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# Regrid Unit Test
#
RUN_ESMF_RegridUTest:
	$(MAKE) TNAME=Regrid NP=8 ftest

RUN_ESMF_RegridUTestUNI:
	$(MAKE) TNAME=Regrid NP=1 ftest


#
# RegridOptions Unit Test
#
RUN_ESMF_RegridOptionsUTest:
	$(MAKE) TNAME=RegridOptions NP=8 ftest

RUN_ESMF_RegridOptionsUTestUNI:
	$(MAKE) TNAME=RegridOptions NP=1 ftest



#
# Regrid Banding Unit Test
#
RUN_ESMF_RegridBandingUTest:
	$(MAKE) TNAME=RegridBanding NP=8 ftest

RUN_ESMF_RegridBandingUTestUNI:
	$(MAKE) TNAME=RegridBanding NP=1 ftest


#
# RegridFunction Unit Test
#
RUN_ESMF_RegridFunctionUTest:
	$(MAKE) TNAME=RegridFunction NP=8 ftest

RUN_ESMF_RegridFunctionUTestUNI:
	$(MAKE) TNAME=RegridFunction NP=1 ftest


#
# RegridSample Unit Test
#


UTEST_RegridSample_OBJS = ESMF_OptionsTable.o \
			  ESMF_Unit_Tests.o \
                          ESMF_RegridArgs.o \
			  ESMF_RegridSubroutines.o

ESMF_RegridSampleUTest.o: ESMF_OptionsTable.o \
                          ESMF_Unit_Tests.o \
                          ESMF_RegridArgs.o \
                          ESMF_RegridSubroutines.o

RUN_ESMF_RegridSampleUTest:
	$(MAKE) TNAME=RegridSample NP=8 ftest

RUN_ESMF_RegridSampleUTestUNI:
	$(MAKE) TNAME=RegridSample NP=1 ftest


