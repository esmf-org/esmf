# $Id: makefile,v 1.6 2005/03/01 21:09:06 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_SimpleCoupling


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_SimpleCoupling  

SYSTEM_TESTS_RUN_UNI  = RUN_SimpleCouplingUNI


ESMF_SimpleCouplingSTest.o: $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_SimpleCoupling:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest >$(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest.stdout

RUN_SimpleCouplingUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest >$(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest.stdout




