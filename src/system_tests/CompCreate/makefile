# $Id: makefile,v 1.2 2003/09/18 22:16:00 cdeluca Exp $ 

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
LOCDIR	  = src/SystemTests/CompCreate


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/CompCreate

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_model.o

SYSTEM_TESTS_RUN     = RUN_CompCreate

SYSTEM_TESTS_RUN_UNI = RUN_CompCreateUNI

SYSTEM_TESTS_RM      = CompCreate.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompCreate:
	-@rm -f testlog stdlog.* logfile.* CompCreate.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/CompCreate > $(ESMC_TESTDIR)/CompCreate.stdout

RUN_CompCreateUNI:
	-@rm -f testlog stdlog.* logfile.* CompCreate.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/CompCreate > $(ESMC_TESTDIR)/CompCreate.stdout

CompCreate.rm:
	echo "CompCreate.rm target called here."



