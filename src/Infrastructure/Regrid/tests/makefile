# $Id: makefile,v 1.14 2005/01/07 20:32:47 jwolfe Exp $

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


TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_RegridUTest 

TESTS_RUN     = RUN_ESMF_RegridUTest 

# TESTS_RUN_UNI = RUN_ESMF_RegridUTestUNI   removed because the test for number of PEs errors
TESTS_RUN_UNI =

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

