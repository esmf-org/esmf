# $Id: makefile,v 1.1 2003/05/02 19:24:24 nscollins Exp $

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
                CoupledFlowDemo.o 

DEMO_RUN      = RUN_ESMF_CoupledFlowApp  

DEMO_RUN_UNI  = RUN_ESMF_CoupledFlowAppUNI

DEMO_RM       = ESMF_CoupledFlowApp.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =

RUN_ESMF_CoupledFlowApp:
	-@rm -f testlog stdlog.* logfile.* ESMF_CoupledFlowApp.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CoupledFlowApp >$(ESMC_TESTDIR)/ESMF_CoupledFlowApp.stdout

RUN_ESMF_CoupledFlowAppUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CoupledFlowApp.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CoupledFlowApp >$(ESMC_TESTDIR)/ESMF_CoupledFlowApp.stdout

ESMF_CoupledFlowApp.rm:
	echo "ESMF_CoupledFlowApp.rm target called here."



