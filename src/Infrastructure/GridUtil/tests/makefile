# $Id: makefile,v 1.6 2008/08/26 20:48:18 theurich Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/GridUtil/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_GridToMeshUTest

TESTS_OBJ = gridtomesh.o

TESTS_RUN     = RUN_ESMF_GridToMeshUTest

TESTS_RUN_UNI = RUN_ESMF_GridToMeshUTestUNI


#ESMF_UTEST_GridToMesh_OBJS = $(TESTS_OBJ)
ESMF_GridToMeshUTest.o: $(TESTS_OBJ)


include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =



#
# unit test targets
#

# --- GridToMesh test

RUN_ESMF_GridToMeshUTest:
	$(MAKE) TNAME=GridToMesh NP=4 ftest

RUN_ESMF_GridToMeshUTestUNI:
	$(MAKE) TNAME=GridToMesh NP=1 ftest

