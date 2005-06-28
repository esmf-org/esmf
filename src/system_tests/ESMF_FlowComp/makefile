# $Id: makefile,v 1.7 2005/06/28 19:56:50 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_FlowComp


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FlowCompSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = ArraysGlobalMod.o FlowMod.o

SYSTEM_TESTS_RUN      = RUN_FlowComp  

SYSTEM_TESTS_RUN_UNI  = RUN_FlowCompUNI


ESMF_FlowCompSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD) SIE.60 U_velocity.60 V_velocity.60

CLOBBERDIRS =


RUN_FlowComp:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FlowCompSTest >$(ESMC_TESTDIR)/ESMF_FlowCompSTest.stdout

RUN_FlowCompUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FlowCompSTest >$(ESMC_TESTDIR)/ESMF_FlowCompSTest.stdout



