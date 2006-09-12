# $Id: makefile,v 1.1.2.2 2006/09/12 22:21:44 theurich Exp $ 

ALL:      tree_build_system_tests
run:      tree_run_system_tests
run_uni:  tree_run_system_tests_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/ESMF_FieldRedistArb2Arb/

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldRedistArb2ArbSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldRedistArb2Arb

SYSTEM_TESTS_RUN_UNI = RUN_FieldRedistArb2ArbUNI


include $(ESMF_DIR)/makefile

# The Darwin (Mac OS X), xlf 8.1 compiler cannot compile user_model1.F90
# with the -g flag (produces an internal compiler error).  removing the -g
# allows the file to build.
ifeq ($(ESMF_ARCH),Darwin)
ifeq ($(ESMF_COMPILER),xlf)
ifeq ($(ESMF_BOPT),g)
G_FFLAGS =
endif
endif
endif

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_FieldRedistArb2Arb:
	$(MAKE) TNAME=FieldRedistArb2Arb NP=6 stest

RUN_FieldRedistArb2ArbUNI:
	$(MAKE) TNAME=FieldRedistArb2Arb NP=1 stest



