# $Id: makefile,v 1.3 2004/12/09 17:25:35 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_FieldRedist

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldRedistSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldRedist  

SYSTEM_TESTS_RUN_UNI = RUN_FieldRedistUNI

SYSTEM_TESTS_RM      = FieldRedist.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core ESMF_LogFile $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_FieldRedist:
	-@rm -f testlog stdlog.* logfile.* FieldRedist.tmps
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldRedistSTest > $(ESMC_TESTDIR)/ESMF_FieldRedistSTest.stdout

RUN_FieldRedistUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldRedistSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldRedistSTest > $(ESMC_TESTDIR)/ESMF_FieldRedistSTest.stdout

FieldRedist.rm:
	echo "FieldRedist.rm target called here."



