# $Id: makefile,v 1.4 2005/03/01 21:08:46 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_FieldRegridOrder


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldRegridOrder

SYSTEM_TESTS_RUN_UNI  = RUN_FieldRegridOrderUNI


ESMF_FieldRegridOrderSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldRegridOrder:
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest >$(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest.stdout

RUN_FieldRegridOrderUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest >$(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest.stdout



