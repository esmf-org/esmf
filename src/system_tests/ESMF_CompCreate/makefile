# $Id: makefile,v 1.4 2004/08/03 19:33:53 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_CompCreate


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CompCreateSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_model.o

SYSTEM_TESTS_RUN     = RUN_CompCreate

SYSTEM_TESTS_RUN_UNI = RUN_CompCreateUNI

SYSTEM_TESTS_RM      = CompCreate.rm

ESMF_CompCreateSTest.o : user_model.o


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core ESMF_LogFile $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompCreate:
	-@rm -f testlog stdlog.* logfile.* CompCreate.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompCreateSTest > $(ESMC_TESTDIR)/ESMF_CompCreateSTest.stdout

RUN_CompCreateUNI:
	-@rm -f testlog stdlog.* logfile.* CompCreate.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompCreateSTest > $(ESMC_TESTDIR)/ESMF_CompCreateSTest.stdout

CompCreate.rm:
	echo "CompCreate.rm target called here."



