# $Id: makefile,v 1.21 2005/03/22 21:49:05 rfaincht Exp $

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


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_RegridUTest \
		$(ESMF_TESTDIR)/ESMF_RegridOptionsUTest \
		$(ESMF_TESTDIR)/ESMF_Regrid1UTest \
		$(ESMF_TESTDIR)/ESMF_Regrid_bUTest

TESTS_RUN     = RUN_ESMF_RegridUTest \
		RUN_ESMF_RegridOptionsUTest \
		RUN_ESMF_Regrid1UTest \
		RUN_ESMF_Regrid_bUTest

TESTS_RUN_UNI = RUN_ESMF_RegridUTestUNI \
	        RUN_ESMF_RegridOptionsUTestUNI \
	        RUN_ESMF_Regrid1UTestUNI \
		RUN_ESMF_Regrid_bUTestUNI


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

RUN_ESMF_RegridUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout

#
#
#
RUN_ESMF_RegridOptionsUTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest > $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest.stdout

RUN_ESMF_RegridOptionsUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest > $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest.stdout


#
#
#
RUN_ESMF_Regrid1UTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout

RUN_ESMF_Regrid1UTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout

#
#
#
RUN_ESMF_Regrid_bUTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid_bUTest > $(ESMF_TESTDIR)/ESMF_Regrid_bUTest.stdout

RUN_ESMF_Regrid_bUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid_bUTest > $(ESMF_TESTDIR)/ESMF_Regrid_bUTest.stdout
