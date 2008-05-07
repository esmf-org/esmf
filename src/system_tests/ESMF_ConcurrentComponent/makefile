# $Id: makefile,v 1.1.2.2 2008/05/07 23:23:50 theurich Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_ConcurrentComponent


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ConcurrentCompSTest

# Object files other than *STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ConcurrentComp

SYSTEM_TESTS_RUN_UNI  =

ESMF_ConcurrentCompSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ConcurrentComp:
	$(MAKE) TNAME=ConcurrentComp NP=8 stest

