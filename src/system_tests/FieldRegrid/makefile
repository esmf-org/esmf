# $Id: makefile,v 1.3 2003/09/19 21:46:07 jwolfe Exp $ 

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


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldRegridSTest

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
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldRegridSTest.tmps
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldRegridSTest >$(ESMC_TESTDIR)/ESMF_FieldRegridSTest.stdout

RUN_FieldRegridUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldRegridSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldRegridSTest >$(ESMC_TESTDIR)/ESMF_FieldRegridSTest.stdout

FieldRegrid.rm:
	echo "FieldRegrid.rm target called here."



