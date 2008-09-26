# $Id: makefile,v 1.9 2008/09/26 04:05:34 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

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
	$(MAKE) TNAME=FlowComp NP=4 stest

RUN_FlowCompUNI:
	$(MAKE) TNAME=FlowComp NP=1 stest



