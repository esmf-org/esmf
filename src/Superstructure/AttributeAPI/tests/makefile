# $Id: makefile,v 1.6 2009/04/22 05:50:37 eschwab Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/AttributeAPI/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_AttributeArrayUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeABundleUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeCplCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeDistGridUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeFieldUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeFBundleUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeGridCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeGridUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeStateUTest \
		$(ESMF_TESTDIR)/ESMF_AttReadCplCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttReadGridCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttReadFieldUTest

TESTS_RUN     = RUN_ESMF_AttributeArrayUTest \
		RUN_ESMF_AttributeABundleUTest \
		RUN_ESMF_AttributeCplCompUTest \
		RUN_ESMF_AttributeDistGridUTest \
		RUN_ESMF_AttributeFieldUTest \
		RUN_ESMF_AttributeFBundleUTest \
		RUN_ESMF_AttributeGridCompUTest \
		RUN_ESMF_AttributeGridUTest \
		RUN_ESMF_AttributeStateUTest \
		RUN_ESMF_AttReadCplCompUTest \
		RUN_ESMF_AttReadGridCompUTest \
		RUN_ESMF_AttReadFieldUTest

TESTS_RUN_UNI = RUN_ESMF_AttributeArrayUTestUNI \
		RUN_ESMF_AttributeABundleUTestUNI \
		RUN_ESMF_AttributeCplCompUTestUNI \
		RUN_ESMF_AttributeDistGridUTestUNI \
		RUN_ESMF_AttributeFieldUTestUNI \
		RUN_ESMF_AttributeFBundleUTestUNI \
		RUN_ESMF_AttributeGridCompUTestUNI \
		RUN_ESMF_AttributeGridUTestUNI \
		RUN_ESMF_AttributeStateUTestUNI \
		RUN_ESMF_AttReadCplCompUTestUNI \
		RUN_ESMF_AttReadGridCompUTestUNI \
		RUN_ESMF_AttReadFieldUTestUNI


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

# --- ArrayBundle

RUN_ESMF_AttributeABundleUTest:
	$(MAKE) TNAME=AttributeABundle NP=4 ftest

RUN_ESMF_AttributeABundleUTestUNI:
	$(MAKE) TNAME=AttributeABundle NP=1 ftest

# --- CplComp

RUN_ESMF_AttributeCplCompUTest:
	$(MAKE) TNAME=AttributeCplComp NP=4 ftest

RUN_ESMF_AttributeCplCompUTestUNI:
	$(MAKE) TNAME=AttributeCplComp NP=1 ftest

RUN_ESMF_AttReadCplCompUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_cplcomp.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadCplComp NP=4 ftest

RUN_ESMF_AttReadCplCompUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_cplcomp.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadCplComp NP=1 ftest

# --- DistGrid

RUN_ESMF_AttributeDistGridUTest:
	$(MAKE) TNAME=AttributeDistGrid NP=4 ftest

RUN_ESMF_AttributeDistGridUTestUNI:
	$(MAKE) TNAME=AttributeDistGrid NP=1 ftest

# --- Field

RUN_ESMF_AttributeFieldUTest:
	$(MAKE) TNAME=AttributeField NP=4 ftest

RUN_ESMF_AttributeFieldUTestUNI:
	$(MAKE) TNAME=AttributeField NP=1 ftest

RUN_ESMF_AttReadFieldUTest:
	cp -f $(ESMF_DIR)/src/Infrastructure/Field/etc/esmf_field.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadField NP=4 ftest

RUN_ESMF_AttReadFieldUTestUNI:
	cp -f $(ESMF_DIR)/src/Infrastructure/Field/etc/esmf_field.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadField NP=1 ftest


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

RUN_ESMF_AttReadGridCompUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadGridComp NP=4 ftest

RUN_ESMF_AttReadGridCompUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadGridComp NP=1 ftest

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


