# $Id: makefile,v 1.2 2008/09/26 21:11:39 cdeluca Exp $

ALL: tree_build_demos

run:  tree_run_demos

LOCDIR	  = src/demos/coupled_flow/src


DEMOS_BUILD    = $(ESMF_TESTDIR)/CoupledFlowApp

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
CLEANFILES  = SIE.* U_velocity.* V_velocity.* FLAG.* DE.* \
              $(DEMOS_BUILD).stdout
CLOBBERDIRS =

RUN_CoupledFlowApp:
	-@rm -f testlog stdlog.* logfile.* 
	@echo "Ready to run the ESMF Coupled Flow Demo Application.  This may take several minutes."
	-@if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo ${ESMF_MPIRUN} -np 4 $(ESMF_TESTDIR)/CoupledFlowApp ; \
	  ${ESMF_MPIRUN} -np 4 $(ESMF_TESTDIR)/CoupledFlowApp ; \
	else \
	  echo ${ESMF_MPIRUN} -np 4 $(ESMF_TESTDIR)/CoupledFlowApp \>$(ESMF_TESTDIR)/CoupledFlowApp.stdout ; \
	  ${ESMF_MPIRUN} -np 4 $(ESMF_TESTDIR)/CoupledFlowApp >$(ESMF_TESTDIR)/CoupledFlowApp.stdout ; \
	fi ; 
	@echo "ESMF Coupled Flow Demo Application complete.  Please look in " $(ESMF_TESTDIR)/CoupledFlowApp.stdout " for status results."
	@echo "Look in " `pwd` " for output data files."


RUN_CoupledFlowAppUNI:
	-@rm -f testlog stdlog.* logfile.* 
	@echo "Ready to run the ESMF Coupled Flow Demo Application.  This may take several minutes."
	-@if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo ${ESMF_MPIRUN} -np 1 $(ESMF_TESTDIR)/CoupledFlowApp ; \
	  ${ESMF_MPIRUN} -np 1 $(ESMF_TESTDIR)/CoupledFlowApp ; \
	else \
	  echo ${ESMF_MPIRUN} -np 1 $(ESMF_TESTDIR)/CoupledFlowApp >$(ESMF_TESTDIR)/CoupledFlowApp.stdout ; \
	  ${ESMF_MPIRUN} -np 1 $(ESMF_TESTDIR)/CoupledFlowApp >$(ESMF_TESTDIR)/CoupledFlowApp.stdout ; \
	fi ; 
	@echo "ESMF Coupled Flow Demo Application complete.  Please look in " $(ESMF_TESTDIR)/CoupledFlowApp.stdout " for status results."
	@echo "Look in " `pwd` " for output data files."


ESMF_CoupledFlowApp.rm:
	echo "ESMF_CoupledFlowApp.rm target called here."



