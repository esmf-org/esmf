# $Id: makefile,v 1.3 2003/05/02 20:54:43 nscollins Exp $

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
LOCDIR	  = src/Demo/coupled_flow


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
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/CoupledFlowApp >$(ESMC_TESTDIR)/CoupledFlowApp.stdout

ESMF_CoupledFlowApp.rm:
	echo "ESMF_CoupledFlowApp.rm target called here."



