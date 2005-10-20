# $Id: makefile,v 1.30 2005/10/20 22:36:44 svasquez Exp $

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
		$(ESMF_TESTDIR)/ESMF_Regrid1UTest \
		$(ESMF_TESTDIR)/ESMF_RegridBandingUTest \
		$(ESMF_TESTDIR)/ESMF_Regrid_bUTest \
		$(ESMF_TESTDIR)/ESMF_RegridSampleUTest

TESTS_RUN     = RUN_ESMF_RegridUTest \
		RUN_ESMF_RegridOptionsUTest \
		RUN_ESMF_Regrid1UTest \
		RUN_ESMF_RegridBandingUTest \
		RUN_ESMF_Regrid_bUTest \
		RUN_ESMF_RegridSampleUTest

TESTS_RUN_UNI = RUN_ESMF_RegridUTestUNI \
	        RUN_ESMF_RegridOptionsUTestUNI \
	        RUN_ESMF_Regrid1UTestUNI \
	        RUN_ESMF_RegridBandingUTestUNI \
		RUN_ESMF_Regrid_bUTestUNI \
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
# Regrid 1 Unit Test
#
RUN_ESMF_Regrid1UTest:
	$(MAKE) TNAME=Regrid1 NP=8 ftest

RUN_ESMF_Regrid1UTestUNI:
	$(MAKE) TNAME=Regrid1 NP=1 ftest


#
# Regrid 2 Unit Test
#
RUN_ESMF_RegridBandingUTest:
	$(MAKE) TNAME=RegridBanding NP=8 ftest

RUN_ESMF_Regrid2UTestUNI:
	$(MAKE) TNAME=RegridBanding NP=1 ftest


#
# Regrid_b Unit Test
#
RUN_ESMF_Regrid_bUTest:
	$(MAKE) TNAME=Regrid_b NP=8 ftest

RUN_ESMF_Regrid_bUTestUNI:
	$(MAKE) TNAME=Regrid_b NP=1 ftest


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


