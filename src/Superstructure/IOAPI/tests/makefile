# $Id: makefile,v 1.2 2009/05/01 04:49:50 eschwab Exp $

ALL:     build_unit_tests
run:     run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Superstructure/IOAPI/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_IOGridCompUTest

TESTS_RUN     = RUN_ESMF_IOGridCompUTest

TESTS_RUN_UNI = RUN_ESMF_IOGridCompUTestUNI


include ${ESMF_DIR}/makefile

DIRS	  =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) 
CLOBBERDIRS =

# --- GridComp

RUN_ESMF_IOGridCompUTest:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=IOGridComp NP=4 ftest

RUN_ESMF_IOGridCompUTestUNI:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_TESTDIR)
	$(MAKE) TNAME=IOGridComp NP=1 ftest
