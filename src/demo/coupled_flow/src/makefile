# $Id: makefile,v 1.7 2004/12/09 17:22:55 nscollins Exp $

ALL: tree_build_demo

run:  tree_run_demo

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/demo/coupled_flow/src


DEMO_BUILD    = $(ESMC_TESTDIR)/CoupledFlowApp

# Object files other than main that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
DEMO_OBJ      = FlowArraysMod.o InjectArraysMod.o \
                FlowSolverMod.o InjectorMod.o CouplerMod.o \
                CoupledFlowApp.o

DEMO_RUN      = RUN_CoupledFlowApp  

DEMO_RUN_UNI  = RUN_CoupledFlowAppUNI

DEMO_RM       = ESMF_CoupledFlowApp.rm

FlowSolverMod.o:: FlowArraysMod.o

InjectorMod.o:: InjectArraysMod.o

CoupledFlowDemo.o:: FlowSolverMod.o InjectorMod.o CouplerMod.o

CoupledFlowApp.o:: CoupledFlowDemo.o

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =

RUN_CoupledFlowApp:
	-@rm -f testlog stdlog.* logfile.* 
	@echo "Ready to run the ESMF Coupled Flow Demo Application.  This may take several minutes."
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/CoupledFlowApp >$(ESMC_TESTDIR)/CoupledFlowApp.stdout
	@echo "ESMF Coupled Flow Demo Application complete.  Please look in " $(ESMC_TESTDIR)/CoupledFlowApp.stdout " for status results."
	@echo "Look in " `pwd` " for output data files."


RUN_CoupledFlowAppUNI:
	-@rm -f testlog stdlog.* logfile.* 
	@echo "Ready to run the ESMF Coupled Flow Demo Application.  This may take several minutes."
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/CoupledFlowApp >$(ESMC_TESTDIR)/CoupledFlowApp.stdout
	@echo "ESMF Coupled Flow Demo Application complete.  Please look in " $(ESMC_TESTDIR)/CoupledFlowApp.stdout " for status results."
	@echo "Look in " `pwd` " for output data files."


ESMF_CoupledFlowApp.rm:
	echo "ESMF_CoupledFlowApp.rm target called here."



