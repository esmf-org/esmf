# $Id: makefile,v 1.6 2004/12/18 16:34:08 nscollins Exp $ 

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

SYSTEM_TESTS_RM       = FlowWithCoupling.rm

ESMF_FlowWithCouplingSTest.o: $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core ESMF_LogFile $(SYSTEM_TESTS_BUILD) \
              SIE* V_velocity.* U_velocity.* FLAG.*
CLOBBERDIRS =

RUN_FlowWithCoupling:
	-@rm -f testlog stdlog.* logfile.* FlowWithCoupling.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest >$(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest.stdout

RUN_FlowWithCouplingUNI:
	-@rm -f testlog stdlog.* logfile.* FlowWithCoupling.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest >$(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest.stdout

FlowWithCoupling.rm:
	echo "FlowWithCoupling.rm target called here."



