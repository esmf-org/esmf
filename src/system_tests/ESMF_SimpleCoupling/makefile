# $Id: makefile,v 1.1 2003/10/09 20:56:14 cdeluca Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_SimpleCoupling


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_SimpleCoupling  

SYSTEM_TESTS_RUN_UNI  = RUN_SimpleCouplingUNI

SYSTEM_TESTS_RM       = SimpleCoupling.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_SimpleCoupling:
	-@rm -f testlog stdlog.* logfile.* SimpleCoupling.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest >$(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest.stdout

RUN_SimpleCouplingUNI:
	-@rm -f testlog stdlog.* logfile.* SimpleCoupling.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest >$(ESMC_TESTDIR)/ESMF_SimpleCouplingSTest.stdout

SimpleCoupling.rm:
	echo "SimpleCoupling.rm target called here."



