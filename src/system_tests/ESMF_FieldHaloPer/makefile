# $Id: makefile,v 1.5 2005/03/01 21:08:05 nscollins Exp $ 

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
DIRS	  = 
LOCDIR	  = src/system_tests/ESMF_FieldHaloPer


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest

# Object files other than ESMF_%STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldHaloPeriodic  

SYSTEM_TESTS_RUN_UNI = RUN_FieldHaloPeriodicUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldHaloPeriodic:
	-${MPIRUN} -np 12 $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest > $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest.stdout

RUN_FieldHaloPeriodicUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest > $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest.stdout



