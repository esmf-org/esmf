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
LOCDIR	  = src/system_tests/ESMF_BundleRedistBlk2Arb

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BundleRedistBlk2ArbSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_BundleRedistBlk2Arb

SYSTEM_TESTS_RUN_UNI = RUN_BundleRedistBlk2ArbUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_BundleRedistBlk2Arb:
	$(MAKE) TNAME=BundleRedistBlk2Arb NP=6 stest

RUN_BundleRedistBlk2ArbUNI:
	$(MAKE) TNAME=BundleRedistBlk2Arb NP=1 stest



