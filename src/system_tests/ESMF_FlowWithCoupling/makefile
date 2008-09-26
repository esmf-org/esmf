# $Id: makefile,v 1.11 2008/09/26 04:05:35 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FlowWithCoupling


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = FlowArraysMod.o InjectArraysMod.o \
                        FlowSolverMod.o InjectorMod.o CouplerMod.o 

SYSTEM_TESTS_RUN      = RUN_FlowWithCoupling  

SYSTEM_TESTS_RUN_UNI  = RUN_FlowWithCouplingUNI


ESMF_FlowWithCouplingSTest.o: $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD) SIE* V_velocity.* U_velocity.* FLAG.*
CLOBBERDIRS =

RUN_FlowWithCoupling:
	$(MAKE) TNAME=FlowWithCoupling NP=4 stest

RUN_FlowWithCouplingUNI:
	$(MAKE) TNAME=FlowWithCoupling NP=1 stest


