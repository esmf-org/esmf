# $Id: makefile,v 1.1 2003/10/09 20:56:13 cdeluca Exp $ 

ALL: system_tests

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

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_FlowWithCoupling:
	-@rm -f testlog stdlog.* logfile.* FlowWithCoupling.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest >$(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest.stdout

RUN_FlowWithCouplingUNI:
	-@rm -f testlog stdlog.* logfile.* FlowWithCoupling.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest >$(ESMC_TESTDIR)/ESMF_FlowWithCouplingSTest.stdout

FlowWithCoupling.rm:
	echo "FlowWithCoupling.rm target called here."



