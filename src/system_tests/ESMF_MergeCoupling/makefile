# $Id: makefile,v 1.3 2004/12/09 17:25:37 nscollins Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/ESMF_MergeCoupling


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_MergeCouplingSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_model3.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_MergeCoupling  

SYSTEM_TESTS_RUN_UNI  = RUN_MergeCouplingUNI

SYSTEM_TESTS_RM       = MergeCoupling.rm

ESMF_MergeCouplingSTest.o: $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core ESMF_LogFile $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_MergeCoupling:
	-@rm -f testlog stdlog.* logfile.* MergeCoupling.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_MergeCouplingSTest >$(ESMC_TESTDIR)/ESMF_MergeCouplingSTest.stdout

RUN_MergeCouplingUNI:
	-@rm -f testlog stdlog.* logfile.* MergeCoupling.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_MergeCouplingSTest >$(ESMC_TESTDIR)/ESMF_MergeCouplingSTest.stdout

MergeCoupling.rm:
	echo "MergeCoupling.rm target called here."



