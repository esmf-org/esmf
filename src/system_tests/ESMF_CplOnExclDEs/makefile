# $Id: makefile,v 1.3 2004/06/04 08:19:03 nscollins Exp $ 

ALL: build_system_tests

CFLAGS    = 
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/ESMF_CplOnExclDEs


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_CouplingOnExclDEs  

SYSTEM_TESTS_RUN_UNI  = RUN_CouplingOnExclDEsUNI

SYSTEM_TESTS_RM       = CouplingOnExclDEs.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =

ESMF_CplOnExclDEsSTest.o:  user_model1.o user_model2.o user_coupler.o

RUN_CouplingOnExclDEs:
	-@rm -f testlog stdlog.* logfile.* ESMF_CplOnExclDEsSTest.tmps
	-${MPIRUN} -np 8 $(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest >$(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest.stdout

RUN_CouplingOnExclDEsUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CplOnExclDEsSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest >$(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest.stdout

CouplingOnExclDEs.rm:
	echo "CouplingOnExclDEs.rm target called here."



