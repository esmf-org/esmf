# $Id: makefile,v 1.14.2.2 2005/05/18 20:29:10 jwolfe Exp $

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
		$(ESMF_TESTDIR)/ESMF_Regrid1UTest \
		$(ESMF_TESTDIR)/ESMF_Regrid2UTest

TESTS_RUN     = RUN_ESMF_RegridUTest \
		RUN_ESMF_Regrid1UTest \
		RUN_ESMF_Regrid2UTest

# TESTS_RUN_UNI = RUN_ESMF_RegridUTestUNI   removed because the test for number of PEs errors
TESTS_RUN_UNI = RUN_ESMF_Regrid1UTestUNI

TESTS_RM      = 

include $(ESMF_DIR)/makefile


DIRS =

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL core ESMF_LogFile $(TESTS_BUILD)
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
RUN_ESMF_Regrid1UTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout

RUN_ESMF_Regrid1UTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout

#
#
#
RUN_ESMF_Regrid2UTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid2UTest > $(ESMF_TESTDIR)/ESMF_Regrid2UTest.stdout

RUN_ESMF_Regrid2UTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid2UTest > $(ESMF_TESTDIR)/ESMF_Regrid2UTest.stdout

