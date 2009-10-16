# $Id: makefile,v 1.7 2009/10/16 01:38:28 theurich Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_ArrayRedistMPMD


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ArrayRedistSTestA $(ESMC_TESTDIR)/ESMF_ArrayRedistSTestB

# Object files other than STest%.o that the system tests executable will depend
# on. List objects files in the order that they are to be compiled/created.
SYSTEM_TESTS_OBJ_A    = user_model1.o user_coupler.o
SYSTEM_TESTS_OBJ_B    = user_model2.o user_coupler.o

ifeq ($(ESMF_TESTMPMD),ON)
SYSTEM_TESTS_RUN      = RUN_ArrayRedistMPMD
else
SYSTEM_TESTS_RUN      =
endif

SYSTEM_TESTS_RUN_UNI  =

ESMF_ArrayRedistSTestA.o : $(SYSTEM_TESTS_OBJ_A)
ESMF_ArrayRedistSTestB.o : $(SYSTEM_TESTS_OBJ_B)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ArrayRedistMPMD:
	-@cd $(ESMF_TESTDIR) ; \
	${ESMF_MPIMPMDRUN} -n 4 ./ESMF_ArrayRedistSTestA : -n 2 ./ESMF_ArrayRedistSTestB > ./ESMF_ArrayRedistSTestA.stdout ; \
	cat ./PET*ArrayRedistSTest*.Log> ./ESMF_ArrayRedistSTestA.Log ; \
	$(ESMF_RM) ./PET*ArrayRedistSTest*.Log
