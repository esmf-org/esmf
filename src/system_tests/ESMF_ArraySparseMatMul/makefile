# $Id: makefile,v 

ALL: tree_build_system_tests

run: tree_run_system_tests

CFLAGS    = 
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/ESMF_SeqEnsemEx


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SeqEnsemExSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_modelA.o user_modelB.o user_modelC.o user_modelD.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ESMF_SeqEnsemEx

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SeqEnsemExUNI


ESMF_SeqEnsemExSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ESMF_SeqEnsemEx:
	$(MAKE) TNAME=SeqEnsemEx NP=6 stest

RUN_ESMF_SeqEnsemExUNI:
	$(MAKE) TNAME=SeqEnsemEx NP=1 stest
