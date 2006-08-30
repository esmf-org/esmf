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
LOCDIR	  = src/system_tests/ESMF_BundleRedistBlk2Blk

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BundleRedistBlk2BlkSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_BundleRedistBlk2Blk

SYSTEM_TESTS_RUN_UNI = RUN_BundleRedistBlk2BlkUNI


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

RUN_BundleRedistBlk2Blk:
	$(MAKE) TNAME=BundleRedistBlk2Blk NP=6 stest

RUN_BundleRedistBlk2BlkUNI:
	$(MAKE) TNAME=BundleRedistBlk2Blk NP=1 stest



