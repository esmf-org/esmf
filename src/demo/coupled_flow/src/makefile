# $Id: makefile,v 1.8.2.1 2005/02/09 17:01:49 nscollins Exp $

ALL: tree_build_demos

run:  tree_run_demos

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


DEMOS_BUILD    = $(ESMC_TESTDIR)/CoupledFlowApp

# Object files other than main that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
DEMOS_OBJ      = FlowArraysMod.o InjectArraysMod.o \
                 FlowSolverMod.o InjectorMod.o CouplerMod.o \
                 CoupledFlowApp.o

DEMOS_RUN      = RUN_CoupledFlowApp  

DEMOS_RUN_UNI  = RUN_CoupledFlowAppUNI

DEMOS_RM       = ESMF_CoupledFlowApp.rm

FlowSolverMod.o:: FlowArraysMod.o

InjectorMod.o:: InjectArraysMod.o

CoupledFlowDemo.o:: FlowSolverMod.o InjectorMod.o CouplerMod.o

CoupledFlowApp.o:: CoupledFlowDemo.o

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod SIE.* U_velocity.* V_velocity.* FLAG.* DE.*
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



