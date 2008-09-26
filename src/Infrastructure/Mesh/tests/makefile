# $Id: makefile,v 1.2 2008/09/26 21:11:38 cdeluca Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/Mesh/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_MeshUTest

TESTS_OBJ = 

TESTS_RUN     = RUN_ESMF_MeshUTest

TESTS_RUN_UNI = RUN_ESMF_MeshUTestUNI



include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 


#
# unit test targets
#

# --- Mesh test

RUN_ESMF_MeshUTest:
	$(MAKE) TNAME=Mesh NP=4 ftest

RUN_ESMF_MeshUTestUNI:
	$(MAKE) TNAME=Mesh NP=1 ftest

