# $Id: makefile,v 1.1 2004/06/02 20:41:35 jwolfe Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_FieldRegridConserv


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldRegridConsrvSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldRegridConserv

SYSTEM_TESTS_RUN_UNI  = RUN_FieldRegridConservUNI

SYSTEM_TESTS_RM       = FieldRegridConserv.rm

ESMF_FieldRegridConsrvSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


RUN_FieldRegridConserv:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldRegridConsrvSTest.tmps
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldRegridConsrvSTest >$(ESMC_TESTDIR)/ESMF_FieldRegridConsrvSTest.stdout

RUN_FieldRegridConservUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldRegridConsrvSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldRegridConsrvSTest >$(ESMC_TESTDIR)/ESMF_FieldRegridConsrvSTest.stdout

FieldRegridConserv.rm:
	echo "FieldRegridConserv.rm target called here."



