# $Id: makefile,v 1.3 2003/05/08 20:25:50 nscollins Exp $

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
LOCDIR	  = src/Demo/coupled_flow/src


DEMO_BUILD    = $(ESMC_TESTDIR)/CoupledFlowApp

# Object files other than main that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
DEMO_OBJ      = FlowArraysMod.o InjectArraysMod.o \
                FlowSolverMod.o InjectorMod.o CouplerMod.o \
                CoupledFlowDemo.o CoupledFlowApp.o

DEMO_RUN      = RUN_CoupledFlowApp  

DEMO_RM       = ESMF_CoupledFlowApp.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =

RUN_CoupledFlowApp:
	-@rm -f testlog stdlog.* logfile.* 
	@echo "Ready to run the ESMF Demo Application.  This may take several minutes."
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/CoupledFlowApp >$(ESMC_TESTDIR)/CoupledFlowApp.stdout
	@echo "ESMF Demo Application complete.  Please look in " $(ESMC_TESTDIR)/CoupledFlowApp.stdout " for status results."
	@echo "Look in " `pwd` " for output data files."


ESMF_CoupledFlowApp.rm:
	echo "ESMF_CoupledFlowApp.rm target called here."



