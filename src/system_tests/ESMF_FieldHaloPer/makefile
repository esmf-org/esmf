# $Id: makefile,v 1.8 2008/09/26 04:05:33 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FieldHaloPer


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest

# Object files other than ESMF_%STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldHaloPeriodic  

SYSTEM_TESTS_RUN_UNI = RUN_FieldHaloPeriodicUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


# note: this test is set up to run with a variety of different processor 
#  counts (1, 2, and any even number up to 32).  we used to run it with 
#  -np 12, but on some of the smaller machines that used to hang in the
#  mpi code (running out of resources).  so we dropped it down to 6, but
#  on the larger machines we could run this with larger processor counts.
RUN_FieldHaloPeriodic:
	$(MAKE) TNAME=FieldHaloPer NP=6 stest
	#$(MAKE) TNAME=FieldHaloPer NP=12 stest

RUN_FieldHaloPeriodicUNI:
	$(MAKE) TNAME=FieldHaloPer NP=1 stest



