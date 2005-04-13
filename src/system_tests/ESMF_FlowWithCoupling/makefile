# $Id: makefile,v 1.8 2005/04/13 23:03:13 nscollins Exp $ 

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

# The Darwin (Mac OS X), xlf 8.1 compiler cannot compile FlowSolverMod.F90
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
CLEANFILES  = $(SYSTEM_TESTS_BUILD) SIE* V_velocity.* U_velocity.* FLAG.*
CLOBBERDIRS =

RUN_FlowWithCoupling:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest >$(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest.stdout

RUN_FlowWithCouplingUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest >$(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest.stdout


