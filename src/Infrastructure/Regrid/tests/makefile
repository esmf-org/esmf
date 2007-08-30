# $Id: makefile,v 1.41 2007/08/30 05:06:39 cdeluca Exp $

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


TESTS_BUILD   = 
#		$(ESMF_TESTDIR)/ESMF_RegridUTest \
#		$(ESMF_TESTDIR)/ESMF_RegridOptionsUTest \
#		$(ESMF_TESTDIR)/ESMF_RegridBandingUTest \
#		$(ESMF_TESTDIR)/ESMF_RegridTimeUTest \
#		$(ESMF_TESTDIR)/ESMF_RegridToolUTest

TESTS_RUN     = 
#		RUN_ESMF_RegridToolUTest \
#		RUN_ESMF_RegridTimeUTest \
#		RUN_ESMF_RegridUTest \
#		RUN_ESMF_RegridOptionsUTest \
#		RUN_ESMF_RegridBandingUTest \
#		RUN_ESMF_RegridTimeUTest

TESTS_RUN_UNI =	
#		RUN_ESMF_RegridToolUTestUNI \
#		RUN_ESMF_RegridTimeUTestUNI \
#		RUN_ESMF_RegridUTestUNI \
#		RUN_ESMF_RegridOptionsUTestUNI \
#		RUN_ESMF_RegridBandingUTestUNI


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
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=RegridOptions NP=8 ftest

RUN_ESMF_RegridOptionsUTestUNI:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=RegridOptions NP=1 ftest



#
# Regrid Banding Unit Test
#
RUN_ESMF_RegridBandingUTest:
	$(MAKE) TNAME=RegridBanding NP=8 ftest

RUN_ESMF_RegridBandingUTestUNI:
	$(MAKE) TNAME=RegridBanding NP=1 ftest


#
# RegridTime Unit Test
#
RUN_ESMF_RegridTimeUTest:
	$(MAKE) TNAME=RegridTime NP=8 ftest

RUN_ESMF_RegridTimeUTestUNI:
	$(MAKE) TNAME=RegridTime NP=1 ftest


#
# RegridTool Unit Test
#


UTEST_RegridTool_OBJS = ESMF_RegridSubroutines.o

ESMF_RegridToolUTest.o: ESMF_RegridSubroutines.o


RUN_ESMF_RegridToolUTest:
	cp -f ESMF_RegridTestList.rc $(ESMF_TESTDIR)
	cp -f ESMF_RegridFunction.rc $(ESMF_TESTDIR)
	cp -f ESMF_RegridMethod.rc $(ESMF_TESTDIR)
	cp -f ESMF_IGridHorzStagger.rc $(ESMF_TESTDIR)
	cp -f ESMF_RegridDELayout.rc $(ESMF_TESTDIR)
	cp -f ESMF_RegridHalo.rc $(ESMF_TESTDIR)
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=RegridTool NP=8 ftest

RUN_ESMF_RegridToolUTestUNI:
	cp -f ESMF_RegridTestList.rc $(ESMF_TESTDIR)
	cp -f ESMF_RegridFunction.rc $(ESMF_TESTDIR)
	cp -f ESMF_RegridMethod.rc $(ESMF_TESTDIR)
	cp -f ESMF_IGridHorzStagger.rc $(ESMF_TESTDIR)
	cp -f ESMF_RegridDELayout.rc $(ESMF_TESTDIR)
	cp -f ESMF_RegridHalo.rc $(ESMF_TESTDIR)
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=RegridTool NP=1 ftest


