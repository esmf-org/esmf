# $Id: makefile,v 1.21 2012/10/31 21:11:02 rokuingh Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/AttributeAPI/tests


TESTS_BUILD = $(ESMF_TESTDIR)/ESMF_AttributeArrayUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeABundleUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeCplCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeDistGridUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeFBundleUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeFieldUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeGridCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeGridGetUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeGridUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeSciCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeStateUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeUpdateUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeUpdate2UTest \
		$(ESMF_TESTDIR)/ESMF_AttributeWriteInternalUTest \
		$(ESMF_TESTDIR)/ESMF_AttributeXMLUTest \
		$(ESMF_TESTDIR)/ESMF_AttReadCplCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttReadGridCompUTest \
		$(ESMF_TESTDIR)/ESMF_AttReadFieldUTest

TESTS_RUN = RUN_ESMF_AttributeArrayUTest \
		RUN_ESMF_AttributeABundleUTest \
		RUN_ESMF_AttributeCplCompUTest \
		RUN_ESMF_AttributeDistGridUTest \
		RUN_ESMF_AttributeFieldUTest \
		RUN_ESMF_AttributeFBundleUTest \
		RUN_ESMF_AttributeGridCompUTest \
		RUN_ESMF_AttributeGridGetUTest \
		RUN_ESMF_AttributeGridUTest \
		RUN_ESMF_AttributeSciCompUTest \
		RUN_ESMF_AttributeStateUTest \
		RUN_ESMF_AttributeUpdateUTest \
		RUN_ESMF_AttributeUpdate2UTest \
		RUN_ESMF_AttributeWriteInternalUTest \
		RUN_ESMF_AttributeXMLUTest \
		RUN_ESMF_AttReadCplCompUTest \
		RUN_ESMF_AttReadGridCompUTest \
		RUN_ESMF_AttReadFieldUTest

TESTS_RUN_UNI = RUN_ESMF_AttributeGridGetUTestUNI \
		RUN_ESMF_AttributeArrayUTestUNI \
		RUN_ESMF_AttributeABundleUTestUNI \
		RUN_ESMF_AttributeCplCompUTestUNI \
		RUN_ESMF_AttributeDistGridUTestUNI \
		RUN_ESMF_AttributeFieldUTestUNI \
		RUN_ESMF_AttributeFBundleUTestUNI \
		RUN_ESMF_AttributeGridCompUTestUNI \
		RUN_ESMF_AttributeGridGetUTestUNI \
		RUN_ESMF_AttributeGridUTestUNI \
		RUN_ESMF_AttributeSciCompUTestUNI \
		RUN_ESMF_AttributeStateUTestUNI \
		RUN_ESMF_AttributeWriteInternalUTestUNI \
		RUN_ESMF_AttributeXMLUTestUNI \
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
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_Array002.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_Array002.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeArray NP=4 ftest

RUN_ESMF_AttributeArrayUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_Array002.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_Array002.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeArray NP=1 ftest

# --- ArrayBundle

RUN_ESMF_AttributeABundleUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_MyArrayBundle.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_MyArrayBundle.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeABundle NP=4 ftest

RUN_ESMF_AttributeABundleUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_MyArrayBundle.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_MyArrayBundle.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeABundle NP=1 ftest

# --- CplComp

RUN_ESMF_AttributeCplCompUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_cplcomp.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_cplcomp.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeCplComp NP=4 ftest

RUN_ESMF_AttributeCplCompUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_cplcomp.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_cplcomp.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeCplComp NP=1 ftest

RUN_ESMF_AttReadCplCompUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_cplcomp.xml $(ESMF_TESTDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadCplComp NP=4 ftest

RUN_ESMF_AttReadCplCompUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_cplcomp.xml $(ESMF_TESTDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadCplComp NP=1 ftest

# --- DistGrid

RUN_ESMF_AttributeDistGridUTest:
	$(MAKE) TNAME=AttributeDistGrid NP=4 ftest

RUN_ESMF_AttributeDistGridUTestUNI:
	$(MAKE) TNAME=AttributeDistGrid NP=1 ftest

# --- FBundle

RUN_ESMF_AttributeFBundleUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_fieldbundle.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_fieldbundle.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeFBundle NP=4 ftest

RUN_ESMF_AttributeFBundleUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_fieldbundle.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_fieldbundle.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeFBundle NP=1 ftest

# --- Field

RUN_ESMF_AttributeFieldUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_field.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_field.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeField NP=4 ftest

RUN_ESMF_AttributeFieldUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_field.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_field.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeField NP=1 ftest

RUN_ESMF_AttReadFieldUTest:
	cp -f $(ESMF_DIR)/src/Infrastructure/Field/etc/esmf_field.xml $(ESMF_TESTDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadField NP=4 ftest

RUN_ESMF_AttReadFieldUTestUNI:
	cp -f $(ESMF_DIR)/src/Infrastructure/Field/etc/esmf_field.xml $(ESMF_TESTDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadField NP=1 ftest

# --- GridComp

RUN_ESMF_AttributeGridCompUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridcomp.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridcomp.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeGridComp NP=4 ftest

RUN_ESMF_AttributeGridCompUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridcomp.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridcomp.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeGridComp NP=1 ftest

RUN_ESMF_AttReadGridCompUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_TESTDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadGridComp NP=4 ftest

RUN_ESMF_AttReadGridCompUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_TESTDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadGridComp NP=1 ftest

# --- Grid

RUN_ESMF_AttributeGridUTest:
	$(MAKE) TNAME=AttributeGrid NP=4 ftest

RUN_ESMF_AttributeGridUTestUNI:
	$(MAKE) TNAME=AttributeGrid NP=1 ftest

# --- SciComp

RUN_ESMF_AttributeSciCompUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_scicomp.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_scicomp.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeSciComp NP=4 ftest

RUN_ESMF_AttributeSciCompUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_scicomp.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_scicomp.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeSciComp NP=1 ftest

RUN_ESMF_AttReadSciCompUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_scicomp.xml $(ESMF_TESTDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadSciComp NP=4 ftest

RUN_ESMF_AttReadSciCompUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_scicomp.xml $(ESMF_TESTDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttReadSciComp NP=1 ftest

# --- State

RUN_ESMF_AttributeStateUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_state.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_state.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeState NP=4 ftest

RUN_ESMF_AttributeStateUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_state.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_state.stdout $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeState NP=1 ftest

# --- Update

RUN_ESMF_AttributeUpdateUTest:
	$(MAKE) TNAME=AttributeUpdate NP=4 ftest

RUN_ESMF_AttributeUpdateRemoveOnlyUTest:
	$(MAKE) TNAME=AttributeUpdateRemoveOnly NP=4 ftest

RUN_ESMF_AttributeUpdate2UTest:
	$(MAKE) TNAME=AttributeUpdate2 NP=6 ftest

# --- WriteInternal

RUN_ESMF_AttributeWriteInternalUTest:
	$(MAKE) TNAME=AttributeWriteInternal NP=1 ftest

RUN_ESMF_AttributeWriteInternalUTestUNI:
	$(MAKE) TNAME=AttributeWriteInternal NP=1 ftest

RUN_ESMF_AttributeGridGetUTest:
	$(MAKE) TNAME=AttributeGridGet NP=4 ftest

RUN_ESMF_AttributeGridGetUTestUNI:
	$(MAKE) TNAME=AttributeGridGet NP=1 ftest

# --- XML

RUN_ESMF_AttributeXMLUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridded_comp_cim.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridded_comp_cim_rp.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridded_comp_cust_rp.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeXML NP=4 ftest

RUN_ESMF_AttributeXMLUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridded_comp_cim.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridded_comp_cim_rp.xml $(ESMF_TESTDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_gridded_comp_cust_rp.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=AttributeXML NP=1 ftest

