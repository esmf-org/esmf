# $Id: makefile,v 1.7 2004/12/09 17:25:19 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_ArrayRedist

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ArrayRedistSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_ArrayRedist  

SYSTEM_TESTS_RUN_UNI = RUN_ArrayRedistUNI

SYSTEM_TESTS_RM      = ArrayRedist.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core ESMF_LogFile $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_ArrayRedist:
	-@rm -f testlog stdlog.* logfile.* ArrayRedist.tmps
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_ArrayRedistSTest > $(ESMC_TESTDIR)/ESMF_ArrayRedistSTest.stdout

RUN_ArrayRedistUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayRedistSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ArrayRedistSTest > $(ESMC_TESTDIR)/ESMF_ArrayRedistSTest.stdout

ArrayRedist.rm:
	echo "ArrayRedist.rm target called here."



