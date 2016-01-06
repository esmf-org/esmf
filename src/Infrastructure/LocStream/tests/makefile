# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/LocStream/tests

.NOTPARALLEL:
TESTS_BUILD   = \
	$(ESMF_TESTDIR)/ESMF_LocStreamUTest \
	$(ESMF_TESTDIR)/ESMC_LocStreamUTest


TESTS_RUN     = \
	RUN_ESMF_LocStreamUTest \
	RUN_ESMC_LocStreamUTest 

TESTS_RUN_UNI = \
	RUN_ESMF_LocStreamUTestUNI \
	RUN_ESMC_LocStreamUTestUNI 

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

# --- LocStream Creation

RUN_ESMF_LocStreamUTest:
	$(MAKE) TNAME=LocStream NP=4 ftest

RUN_ESMF_LocStreamUTestUNI:
	$(MAKE) TNAME=LocStream NP=1 ftest

RUN_ESMC_LocStreamUTest:
	$(MAKE) TNAME=LocStream NP=4 ctest

RUN_ESMC_LocStreamUTestUNI:
	$(MAKE) TNAME=LocStream NP=1 ctest
