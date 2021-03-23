# $Id$

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

.PRECIOUS: %.F90

LOCDIR = src/Superstructure/InfoAPI/tests

.NOTPARALLEL:
#TESTS_BUILD = $(ESMF_TESTDIR)/ESMF_AttPackFieldUTest
TESTS_BUILD = \
          $(ESMF_TESTDIR)/ESMF_InfoGetInterfaceArrayUTest \
          $(ESMF_TESTDIR)/ESMF_InfoSyncUTest \
          $(ESMF_TESTDIR)/ESMF_InfoDescribeUTest \
          $(ESMF_TESTDIR)/ESMF_InfoCacheUTest

TESTS_RUN = \
          RUN_ESMF_InfoGetInterfaceArrayUTest \
          RUN_ESMF_InfoSyncUTest \
          RUN_ESMF_InfoDescribeUTest \
          RUN_ESMF_InfoCacheUTest

TESTS_RUN_UNI = \
          RUN_ESMF_InfoGetInterfaceArrayUTestUNI \
          RUN_ESMF_InfoSyncUTestUNI \
          RUN_ESMF_InfoDescribeUTestUNI \
          RUN_ESMF_InfoCacheUTestUNI

include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD) $(AUTOGEN)
CLOBBERDIRS =


# ==============================================================================

# ---  Info object-level get interfaces


RUN_ESMF_InfoGetInterfaceArrayUTest:
	$(MAKE) TNAME=InfoGetInterfaceArray NP=4 ftest

RUN_ESMF_InfoGetInterfaceArrayUTestUNI:
	$(MAKE) TNAME=InfoGetInterfaceArray NP=1 ftest

# --- Info Synchronization Testing

RUN_ESMF_InfoSyncUTest:
	$(MAKE) TNAME=InfoSync NP=4 ftest

RUN_ESMF_InfoSyncUTestUNI:
	$(MAKE) TNAME=InfoSync NP=1 ftest

# --- InfoDescribe Testing

RUN_ESMF_InfoDescribeUTest:
	$(MAKE) TNAME=InfoDescribe NP=4 ftest

RUN_ESMF_InfoDescribeUTestUNI:
	$(MAKE) TNAME=InfoDescribe NP=1 ftest

# --- InfoCache Testing

RUN_ESMF_InfoCacheUTest:
	$(MAKE) TNAME=InfoCache NP=4 ftest

RUN_ESMF_InfoCacheUTestUNI:
	$(MAKE) TNAME=InfoCache NP=1 ftest