# $Id: makefile,v 1.1 2008/10/09 17:01:02 rokuingh Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/AttributeAPI/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_AttributeArrayUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeCplCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeFieldUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeFBundleUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeGridCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeGridUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeStateUTest

TESTS_RUN     = RUN_ESMF_AttributeArrayUTest \
		RUN_ESMF_AttributeCplCompUTest \
		RUN_ESMF_AttributeFieldUTest \
		RUN_ESMF_AttributeFBundleUTest \
		RUN_ESMF_AttributeGridCompUTest \
		RUN_ESMF_AttributeGridUTest \
		RUN_ESMF_AttributeStateUTest

TESTS_RUN_UNI = RUN_ESMF_AttributeArrayUTestUNI \
		RUN_ESMF_AttributeCplCompUTestUNI \
		RUN_ESMF_AttributeFieldUTestUNI \
		RUN_ESMF_AttributeFBundleUTestUNI \
		RUN_ESMF_AttributeGridCompUTestUNI \
		RUN_ESMF_AttributeGridUTestUNI \
		RUN_ESMF_AttributeStateUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Attribute class
#

# --- Array

RUN_ESMF_AttributeArrayUTest:
	$(MAKE) TNAME=AttributeArray NP=4 ftest

RUN_ESMF_AttributeArrayUTestUNI:
	$(MAKE) TNAME=AttributeArray NP=1 ftest

# --- CplComp

RUN_ESMF_AttributeCplCompUTest:
	$(MAKE) TNAME=AttributeCplComp NP=4 ftest

RUN_ESMF_AttributeCplCompUTestUNI:
	$(MAKE) TNAME=AttributeCplComp NP=1 ftest

# --- Field

RUN_ESMF_AttributeFieldUTest:
	$(MAKE) TNAME=AttributeField NP=4 ftest

RUN_ESMF_AttributeFieldUTestUNI:
	$(MAKE) TNAME=AttributeField NP=1 ftest

# --- FBundle

RUN_ESMF_AttributeFBundleUTest:
	$(MAKE) TNAME=AttributeFBundle NP=4 ftest

RUN_ESMF_AttributeFBundleUTestUNI:
	$(MAKE) TNAME=AttributeFBundle NP=1 ftest

# --- GridComp

RUN_ESMF_AttributeGridCompUTest:
	$(MAKE) TNAME=AttributeGridComp NP=4 ftest

RUN_ESMF_AttributeGridCompUTestUNI:
	$(MAKE) TNAME=AttributeGridComp NP=1 ftest

# --- Grid

RUN_ESMF_AttributeGridUTest:
	$(MAKE) TNAME=AttributeGrid NP=4 ftest

RUN_ESMF_AttributeGridUTestUNI:
	$(MAKE) TNAME=AttributeGrid NP=1 ftest

# --- State

RUN_ESMF_AttributeStateUTest:
	$(MAKE) TNAME=AttributeState NP=4 ftest

RUN_ESMF_AttributeStateUTestUNI:
	$(MAKE) TNAME=AttributeState NP=1 ftest


