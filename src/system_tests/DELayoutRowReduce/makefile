# $Id: makefile,v 1.2 2003/09/18 22:26:08 cdeluca Exp $ 

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

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/DELayoutRowReduce

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_DELayoutRowReduce  

SYSTEM_TESTS_RUN_UNI = RUN_DELayoutRowReduceUNI

SYSTEM_TESTS_RM      = DELayoutRowReduce.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_DELayoutRowReduce:
	-@rm -f testlog stdlog.* logfile.* DELayoutRowReduce.tmps
	-${MPIRUN} -np 2 $(ESMC_TESTDIR)/DELayoutRowReduce > $(ESMC_TESTDIR)/DELayoutRowReduce.stdout

RUN_DELayoutRowReduceUNI:
	-@rm -f testlog stdlog.* logfile.* DELayoutRowReduce.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/DELayoutRowReduce > $(ESMC_TESTDIR)/DELayoutRowReduce.stdout

DELayoutRowReduce.rm:
	echo "DELayoutRowReduce.rm target called here."



