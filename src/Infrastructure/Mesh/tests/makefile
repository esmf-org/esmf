# $Id: makefile,v 1.5 2008/11/12 21:46:34 rosalind Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/Mesh/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_MeshUTest \
                $(ESMF_TESTDIR)/ESMC_MeshUTest

TESTS_OBJ = 

TESTS_RUN     = RUN_ESMF_MeshUTest \
                RUN_ESMC_MeshUTest

TESTS_RUN_UNI = RUN_ESMF_MeshUTestUNI \
                RUN_ESMC_MeshUTestUNI



include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =



#
# unit test targets
#

# --- Mesh tests

RUN_ESMF_MeshUTest:
	$(MAKE) TNAME=Mesh NP=4 ftest

RUN_ESMF_MeshUTestUNI:
	$(MAKE) TNAME=Mesh NP=1 ftest

RUN_ESMC_MeshUTest:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=Mesh NP=6 ctest

RUN_ESMC_MeshUTestUNI:
	echo "PASS: ESMC_MeshUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMC_MeshUTest.stdout
	# $(MAKE) TNAME=Mesh NP=1 ctest

