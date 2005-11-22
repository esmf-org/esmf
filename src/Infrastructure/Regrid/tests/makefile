# $Id: makefile,v 1.33 2005/11/22 18:28:09 svasquez Exp $

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
		$(ESMF_TESTDIR)/ESMF_RegridToolUTest

TESTS_RUN     = RUN_ESMF_RegridUTest \
		RUN_ESMF_RegridOptionsUTest \
		RUN_ESMF_RegridBandingUTest \
		RUN_ESMF_RegridToolUTest

TESTS_RUN_UNI = RUN_ESMF_RegridUTestUNI \
	        RUN_ESMF_RegridOptionsUTestUNI \
	        RUN_ESMF_RegridBandingUTestUNI \
		RUN_ESMF_RegridToolUTestUNI


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
# RegridTool Unit Test
#


UTEST_RegridTool_OBJS = ESMF_OptionsTable.o \
			  ESMF_Unit_Tests.o \
                          ESMF_RegridArgs.o \
			  ESMF_RegridSubroutines.o

ESMF_RegridToolUTest.o: ESMF_OptionsTable.o \
                          ESMF_Unit_Tests.o \
                          ESMF_RegridArgs.o \
                          ESMF_RegridSubroutines.o

RUN_ESMF_RegridToolUTest:
	$(MAKE) TNAME=RegridTool NP=8 ftest

RUN_ESMF_RegridToolUTestUNI:
	$(MAKE) TNAME=RegridTool NP=1 ftest


