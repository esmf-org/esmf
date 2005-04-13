# $Id: makefile,v 1.7 2005/04/13 23:03:06 nscollins Exp $ 


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

# The Darwin (Mac OS X), xlf 8.1 compiler cannot compile user_model.F90
# with the -g flag (produces an internal compiler error).  removing the -g
# allows the file to build.
ifeq ($(ESMF_ARCH),Darwin)
ifeq ($(ESMF_COMPILER),xlf)
ifeq ($(ESMF_BOPT),g)
G_FFLAGS =
endif
endif
endif

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompCreate:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompCreateSTest > $(ESMC_TESTDIR)/ESMF_CompCreateSTest.stdout

RUN_CompCreateUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompCreateSTest > $(ESMC_TESTDIR)/ESMF_CompCreateSTest.stdout




