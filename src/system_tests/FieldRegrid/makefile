# $Id: makefile,v 1.2 2003/09/18 22:38:22 cdeluca Exp $ 

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
LOCDIR	  = src/system_tests/FieldRegrid


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/FieldRegrid

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldRegrid  

SYSTEM_TESTS_RUN_UNI  = RUN_FieldRegridUNI

SYSTEM_TESTS_RM       = FieldRegrid.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


RUN_FieldRegrid:
	-@rm -f testlog stdlog.* logfile.* FieldRegrid.tmps
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/FieldRegrid >$(ESMC_TESTDIR)/FieldRegrid.stdout

RUN_FieldRegridUNI:
	-@rm -f testlog stdlog.* logfile.* FieldRegrid.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/FieldRegrid >$(ESMC_TESTDIR)/FieldRegrid.stdout

FieldRegrid.rm:
	echo "FieldRegrid.rm target called here."



