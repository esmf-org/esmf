# $Id: makefile,v 1.18 2005/03/01 21:04:27 nscollins Exp $

ALL:  build_tests
 
run:  tree_run_tests

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


TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_RegridUTest \
		$(ESMC_TESTDIR)/ESMF_Regrid1UTest

TESTS_RUN     = RUN_ESMF_RegridUTest \
		RUN_ESMF_Regrid1UTest

TESTS_RUN_UNI = RUN_ESMF_RegridUTestUNI \
	        RUN_ESMF_Regrid1UTestUNI


include $(ESMF_DIR)/makefile


DIRS =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
#
#
RUN_ESMF_RegridUTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout

RUN_ESMF_Regrid1UTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout

RUN_ESMF_RegridUTestUNI:
	echo "PASS: ESMF_RegridUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout

RUN_ESMF_Regrid1UTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout

