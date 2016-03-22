# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/addon/MAPL/tests

.NOTPARALLEL:
TESTS_BUILD   = \
		$(ESMF_TESTDIR)/ESMF_MAPL_GenericSetServicesUTest

TESTS_RUN     = \
		RUN_ESMF_MAPL_GenericSetServicesUTest

#TESTS_RUN_UNI = \
#		RUN_ESMF_MAPL_GenericSetServicesUTestUNI

#
# check ESMF_TESTHARNESS_FIELD for default, 
# otherwise append test harness suite target
#
#ifeq ($(ESMF_TESTHARNESS_FIELD), default)
#  TESTS_RUN := $(TESTS_RUN) RUN_ESMF_TestHarnessField_default
#  TESTS_RUN_UNI := $(TESTS_RUN_UNI) RUN_ESMF_TestHarnessFieldUNI_default
#else
#  TESTS_RUN := $(TESTS_RUN) $(ESMF_TESTHARNESS_FIELD)
#  TESTS_RUN_UNI := $(TESTS_RUN_UNI) $(ESMF_TESTHARNESS_FIELD)
#endif

include ${ESMF_DIR}/makefile

# add MAPL includes and libs
MAPL_INSTALL_ROOT = $(ESMF_DIR)/src/addon/MAPL/ESMA/Linux
MAPL_INCLUDEPATHS = -I$(MAPL_INSTALL_ROOT)/include/MAPL_Base
MAPL_LINKPATHS    = -L$(MAPL_INSTALL_ROOT)/lib
MAPL_LINKLIBS     = -lGMAO_mpeu -lGMAO_eu -lMAPL_cfio -lMAPL_Base 

ESMF_F90COMPILEPATHS      += $(MAPL_INCLUDEPATHS)
ESMF_F90LINKPATHS         += $(MAPL_LINKPATHS)
ESMF_F90LINKLIBS          += $(MAPL_LINKLIBS)

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES) *.nc
CLOBBERDIRS =

DIRS        = 

#
# MAPL
#

RUN_ESMF_MAPL_GenericSetServicesUTest:
	$(MAKE) TNAME=MAPL_GenericSetServices NP=4 ftest

#
# TestHarness tests
#
#RUN_ESMF_TestHarnessField_default:
#	$(MAKE) TESTHARNESSCASE=field_default NP=4 run_test_harness

#
#RUN_ESMF_TestHarnessField_1:
#	$(MAKE) TESTHARNESSCASE=field_1 NP=4 run_test_harness

# UNI test harness
#RUN_ESMF_TestHarnessFieldUNI_default:
#	$(MAKE) TESTHARNESSCASE=field_default NP=1 run_test_harness

#
#RUN_ESMF_TestHarnessFieldUNI_1:
#	$(MAKE) TESTHARNESSCASE=field_1 NP=1 run_test_harness

##
