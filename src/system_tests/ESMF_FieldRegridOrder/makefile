# $Id: makefile,v 1.1 2004/03/22 19:46:44 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_FieldRegridOrder


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldRegridOrder

SYSTEM_TESTS_RUN_UNI  = RUN_FieldRegridOrderUNI

SYSTEM_TESTS_RM       = FieldRegridOrder.rm

ESMF_FieldRegridOrderSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


RUN_FieldRegridOrder:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldRegridOrderSTest.tmps
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest >$(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest.stdout

RUN_FieldRegridOrderUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldRegridOrderSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest >$(ESMC_TESTDIR)/ESMF_FieldRegridOrderSTest.stdout

FieldRegridOrder.rm:
	echo "FieldRegridOrder.rm target called here."



