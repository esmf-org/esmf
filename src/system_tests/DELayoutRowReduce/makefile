# $Id: makefile,v 1.3 2003/09/24 22:36:19 nscollins Exp $ 

ALL: system_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/DELayoutRowReduce

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_RowReduceSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_RowReduce  

SYSTEM_TESTS_RUN_UNI = RUN_RowReduceUNI

SYSTEM_TESTS_RM      = RowReduce.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_RowReduce:
	-@rm -f testlog stdlog.* logfile.* RowReduce.tmps
	-${MPIRUN} -np 2 $(ESMC_TESTDIR)/ESMF_RowReduceSTest > $(ESMC_TESTDIR)/ESMF_RowReduceSTest.stdout

RUN_RowReduceUNI:
	-@rm -f testlog stdlog.* logfile.* RowReduce.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_RowReduceSTest > $(ESMC_TESTDIR)/ESMF_RowReduceSTest.stdout

RowReduce.rm:
	echo "RowReduce.rm target called here."



