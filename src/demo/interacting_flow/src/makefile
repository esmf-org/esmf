# $Id: makefile,v 1.1 2004/10/14 17:00:55 nscollins Exp $

ALL: demo

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/demo/interacting_flow/src


DEMO_BUILD    = $(ESMC_TESTDIR)/InteractingFlowApp

# Object files other than main that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
DEMO_OBJ      = FlowArraysMod.o InjectArraysMod.o \
                FlowSolverMod.o InjectorMod.o CouplerMod.o \
                InteractingFlowApp.o

DEMO_RUN      = RUN_InteractingFlowApp  

DEMO_RUN_UNI  = RUN_InteractingFlowAppUNI

DEMO_RM       = ESMF_InteractingFlowApp.rm

FlowSolverMod.o:: FlowArraysMod.o

InjectorMod.o:: InjectArraysMod.o

InteractingFlowDemo.o:: FlowSolverMod.o InjectorMod.o CouplerMod.o

InteractingFlowApp.o:: InteractingFlowDemo.o

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =

RUN_InteractingFlowApp:
	-@rm -f testlog stdlog.* logfile.* 
	@echo "Ready to run the ESMF Interacting Flow Demo Application.  This may take several minutes."
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/InteractingFlowApp >$(ESMC_TESTDIR)/InteractingFlowApp.stdout
	@echo "ESMF Interacting Flow Demo Application complete.  Please look in " $(ESMC_TESTDIR)/InteractingFlowApp.stdout " for status results."
	@echo "Look in " `pwd` " for output data files."


RUN_InteractingFlowAppUNI:
	-@rm -f testlog stdlog.* logfile.* 
	@echo "Ready to run the ESMF Interacting Flow Demo Application.  This may take several minutes."
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/InteractingFlowApp >$(ESMC_TESTDIR)/InteractingFlowApp.stdout
	@echo "ESMF Interacting Flow Demo Application complete.  Please look in " $(ESMC_TESTDIR)/InteractingFlowApp.stdout " for status results."
	@echo "Look in " `pwd` " for output data files."


ESMF_InteractingFlowApp.rm:
	echo "ESMF_InteractingFlowApp.rm target called here."



