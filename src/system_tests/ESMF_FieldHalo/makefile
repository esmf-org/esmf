# $Id: makefile,v 1.5 2005/03/01 21:07:58 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_FieldHalo


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldHaloSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldHalo  

SYSTEM_TESTS_RUN_UNI = RUN_FieldHaloUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldHalo:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldHaloSTest > $(ESMC_TESTDIR)/ESMF_FieldHaloSTest.stdout

RUN_FieldHaloUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldHaloSTest > $(ESMC_TESTDIR)/ESMF_FieldHaloSTest.stdout

FieldHalo.rm:
	echo "FieldHalo.rm target called here."


