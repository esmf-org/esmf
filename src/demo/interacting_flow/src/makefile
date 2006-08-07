# $Id: makefile,v 1.7.2.3 2006/08/07 17:10:42 theurich Exp $

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
LOCDIR	  = src/demo/interacting_flow/src


DEMOS_BUILD    = $(ESMF_TESTDIR)/InteractingFlowApp

# Object files other than main that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
DEMOS_OBJ      = FlowArraysMod.o InjectArraysMod.o \
                 FlowSolverMod.o InjectorMod.o CouplerMod.o \
                 InteractingFlowApp.o

DEMOS_RUN      = RUN_InteractingFlowApp  

DEMOS_RUN_UNI  = RUN_InteractingFlowAppUNI

DEMOS_RM       = ESMF_InteractingFlowApp.rm

FlowSolverMod.o:: FlowArraysMod.o

InjectorMod.o:: InjectArraysMod.o

InteractingFlowDemo.o:: FlowSolverMod.o InjectorMod.o CouplerMod.o

InteractingFlowApp.o:: InteractingFlowDemo.o

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = SIE.* U_velocity.* V_velocity.* FLAG.* DE.* \
              $(DEMOS_BUILD).stdout
CLOBBERDIRS =

RUN_InteractingFlowApp:
	-@rm -f testlog stdlog.* logfile.* 
	@echo "Ready to run the ESMF Interacting Flow Demo Application.  This may take several minutes."
	-@if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo ${ESMF_MPIRUN} -np 4 $(ESMF_TESTDIR)/InteractingFlowApp ; \
	  ${ESMF_MPIRUN} -np 4 $(ESMF_TESTDIR)/InteractingFlowApp ; \
	else \
	  echo ${ESMF_MPIRUN} -np 4 $(ESMF_TESTDIR)/InteractingFlowApp \>$(ESMF_TESTDIR)/InteractingFlowApp.stdout ; \
	  ${ESMF_MPIRUN} -np 4 $(ESMF_TESTDIR)/InteractingFlowApp >$(ESMF_TESTDIR)/InteractingFlowApp.stdout ; \
	fi ; 
	@echo "ESMF Interacting Flow Demo Application complete.  Please look in " $(ESMF_TESTDIR)/InteractingFlowApp.stdout " for status results."
	@echo "Look in " `pwd` " for output data files."


RUN_InteractingFlowAppUNI:
	-@rm -f testlog stdlog.* logfile.* 
	@echo "Ready to run the ESMF Interacting Flow Demo Application.  This may take several minutes."
	-@if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo ${ESMF_MPIRUN} -np 1 $(ESMF_TESTDIR)/InteractingFlowApp ; \
	  ${ESMF_MPIRUN} -np 1 $(ESMF_TESTDIR)/InteractingFlowApp ; \
	else \
	  echo ${ESMF_MPIRUN} -np 1 $(ESMF_TESTDIR)/InteractingFlowApp >$(ESMF_TESTDIR)/InteractingFlowApp.stdout ; \
	  ${ESMF_MPIRUN} -np 1 $(ESMF_TESTDIR)/InteractingFlowApp >$(ESMF_TESTDIR)/InteractingFlowApp.stdout ; \
	fi ; 
	@echo "ESMF Interacting Flow Demo Application complete.  Please look in " $(ESMF_TESTDIR)/InteractingFlowApp.stdout " for status results."
	@echo "Look in " `pwd` " for output data files."


ESMF_InteractingFlowApp.rm:
	echo "ESMF_InteractingFlowApp.rm target called here."



