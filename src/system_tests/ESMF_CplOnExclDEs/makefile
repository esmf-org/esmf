# $Id: makefile,v 1.6 2005/03/01 21:07:38 nscollins Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

CFLAGS    = 
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/ESMF_CplOnExclDEs


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_CouplingOnExclDEs  

SYSTEM_TESTS_RUN_UNI  = RUN_CouplingOnExclDEsUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

ESMF_CplOnExclDEsSTest.o:  user_model1.o user_model2.o user_coupler.o

RUN_CouplingOnExclDEs:
	-${MPIRUN} -np 8 $(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest >$(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest.stdout

RUN_CouplingOnExclDEsUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest >$(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest.stdout




