# $Id: makefile,v 1.6 2005/03/01 21:07:34 nscollins Exp $ 


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
LOCDIR	  = src/system_tests/ESMF_CompCreate


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CompCreateSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_model.o

SYSTEM_TESTS_RUN     = RUN_CompCreate

SYSTEM_TESTS_RUN_UNI = RUN_CompCreateUNI


ESMF_CompCreateSTest.o : user_model.o


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompCreate:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompCreateSTest > $(ESMC_TESTDIR)/ESMF_CompCreateSTest.stdout

RUN_CompCreateUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompCreateSTest > $(ESMC_TESTDIR)/ESMF_CompCreateSTest.stdout




