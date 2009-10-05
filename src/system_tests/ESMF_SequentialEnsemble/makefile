# $Id: makefile,v 1.1 2009/10/05 18:25:34 svasquez Exp $

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_SequentialEnsemble


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SequentialEnsembleSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_modelA.o user_modelB.o user_modelC.o user_modelD.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ESMF_SequentialEnsemble

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SequentialEnsembleUNI


ESMF_SeqEnsemExSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ESMF_SequentialEnsemble:
	$(MAKE) TNAME=SequentialEnsemble NP=6 stest

RUN_ESMF_SequentialEnsembleUNI:
	$(MAKE) TNAME=SequentialEnsemble NP=1 stest
