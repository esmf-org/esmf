# $Id: makefile,v 1.2 2004/10/05 23:04:46 jwolfe Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_FieldExcl


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldExclSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldExcl  

SYSTEM_TESTS_RUN_UNI  = RUN_FieldExclUNI

ESMF_FieldExclSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod core ESMF_LogFile 
CLOBBERDIRS =


RUN_FieldExcl:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldExclSTest.tmps
	-${MPIRUN} -np 8 $(ESMC_TESTDIR)/ESMF_FieldExclSTest >$(ESMC_TESTDIR)/ESMF_FieldExclSTest.stdout

RUN_FieldExclUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldExclSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclSTest >$(ESMC_TESTDIR)/ESMF_FieldExclSTest.stdout



